"""Shared building blocks for the 256t video series (Manim Community, no LaTeX).

Every episode file imports * from here. This module provides:

- the palette and fonts from the series style guide (video-series/README.md)
- `NarratedScene`, a Scene that runs exactly as long as its narration clip and
  can line animations up with spoken phrases via `self.cue("phrase")`
- the recurring visual elements (ByteRibbon, CIDText, RecipeMachine, ...)

Only Pango `Text` is used for type, so no LaTeX installation is required.
"""

from __future__ import annotations

import base64
import hashlib
import json
import math
import re
import subprocess
import sys
from pathlib import Path

from manim import *  # noqa: F401,F403

SERIES_DIR = Path(__file__).resolve().parents[1]
REPO_DIR = SERIES_DIR.parent
NARRATION_DIR = SERIES_DIR / "narration"
AUDIO_DIR = SERIES_DIR / "audio"

sys.path.insert(0, str(REPO_DIR / "implementations" / "python"))
sys.path.insert(0, str(SERIES_DIR))
from cid import compute_cid  # noqa: E402
from extract_narration import WORDS_PER_MINUTE, speakable  # noqa: E402

# ---------------------------------------------------------------------------
# Palette and type
# ---------------------------------------------------------------------------

LENGTH_COLOR = BLUE
HASH_COLOR = GOLD
INLINE_COLOR = GREEN
OK_COLOR = GREEN
BAD_COLOR = RED
ADVERSARY_COLOR = PURPLE
PAD_COLOR = GREY
DIM_COLOR = GREY_B

MONO = "Menlo"
SANS = "Avenir Next"

FRAME_W = config.frame_width
FRAME_H = config.frame_height


def label(text: str, size: float = 28, color=WHITE, **kwargs) -> Text:
    return Text(text, font=SANS, font_size=size, color=color, **kwargs)


def mono(text: str, size: float = 28, color=WHITE, **kwargs) -> Text:
    return Text(text, font=MONO, font_size=size, color=color, **kwargs)


def fit_width(mob: Mobject, max_width: float = FRAME_W - 1.0) -> Mobject:
    if mob.width > max_width:
        mob.scale_to_fit_width(max_width)
    return mob


def caption(text: str, size: float = 30, color=WHITE) -> Text:
    """A caption pinned near the bottom of the frame."""
    return fit_width(label(text, size, color)).to_edge(DOWN, buff=0.5)


def title_card(text: str, subtitle: str | None = None) -> VGroup:
    group = VGroup(label(text, 64))
    if subtitle:
        group.add(label(subtitle, 30, DIM_COLOR))
    return group.arrange(DOWN, buff=0.35)


# ---------------------------------------------------------------------------
# CID helpers
# ---------------------------------------------------------------------------


def b64url(data: bytes) -> str:
    return base64.urlsafe_b64encode(data).decode().rstrip("=")


def decode_length(cid: str) -> int:
    # Eight base64url characters are exactly six bytes, so no padding is needed.
    return int.from_bytes(base64.urlsafe_b64decode(cid[:8]), "big")


def sha512_b64(data: bytes) -> str:
    return b64url(hashlib.sha512(data).digest())


def length_prefix(n: int) -> str:
    return b64url(n.to_bytes(6, "big"))


def bits_of(data: bytes) -> str:
    return "".join(f"{byte:08b}" for byte in data)


B64_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

GETTYSBURG = (REPO_DIR / "examples" / "gettysburg_address.txt").read_bytes()
GETTYSBURG_CID = compute_cid(GETTYSBURG)


class CIDText(VGroup):
    """A CID in monospace, colored by part: BLUE length, GOLD hash or GREEN inline.

    `self.prefix` and `self.suffix` are the two parts (each a Text, possibly
    empty), so scenes can brace, highlight, or move them independently.
    """

    def __init__(self, cid: str, size: float = 30, max_width: float | None = None,
                 colored: bool = True, **kwargs):
        super().__init__(**kwargs)
        self.cid = cid
        text = mono(cid, size)
        self.text = text
        inline = decode_length(cid) <= 64
        self.suffix_color = INLINE_COLOR if inline else HASH_COLOR
        self.prefix = text[:8]
        self.suffix = text[8:]
        if colored:
            self.prefix.set_color(LENGTH_COLOR)
            self.suffix.set_color(self.suffix_color)
        self.add(text)
        if max_width is not None:
            fit_width(self, max_width)

    def chars(self, start: int, stop: int | None = None) -> VGroup:
        return self.text[start:stop]


class Check(VMobject):
    def __init__(self, size: float = 0.6, color=OK_COLOR, **kwargs):
        super().__init__(color=color, stroke_width=10, **kwargs)
        self.set_points_as_corners([LEFT * 0.5 + UP * 0.05, DOWN * 0.45 + LEFT * 0.1, UP * 0.55 + RIGHT * 0.55])
        self.scale_to_fit_width(size)


class XMark(VGroup):
    def __init__(self, size: float = 0.6, color=BAD_COLOR, **kwargs):
        a = Line(UL, DR, color=color, stroke_width=10)
        b = Line(UR, DL, color=color, stroke_width=10)
        super().__init__(a, b, **kwargs)
        self.scale_to_fit_width(size)


# ---------------------------------------------------------------------------
# Narration timing
# ---------------------------------------------------------------------------


def _normalize(text: str) -> tuple[str, list[int]]:
    """Lowercase, collapse whitespace, drop punctuation; keep an index map."""
    out, index = [], []
    last_space = True
    for i, ch in enumerate(text):
        if ch.isalnum():
            out.append(ch.lower())
            index.append(i)
            last_space = False
        elif not last_space:
            out.append(" ")
            index.append(i)
            last_space = True
    return "".join(out), index


class Narration:
    """The narration for one scene: its text, duration, and phrase timings.

    Duration comes from the audio clip when one exists (audio/epNN/sMM.mp3 or
    .wav), otherwise from the word count at WORDS_PER_MINUTE. Phrase times come
    from an ElevenLabs character alignment (audio/epNN/sMM.json) when present,
    otherwise they are interpolated by character position.
    """

    def __init__(self, episode: int, number: int):
        self.episode, self.number = episode, number
        stem = f"ep{episode:02d}/s{number:02d}"
        text_path = NARRATION_DIR / f"{stem}.txt"
        if not text_path.exists():
            raise FileNotFoundError(f"{text_path} missing; run extract_narration.py")
        self.text = text_path.read_text().strip()
        self.audio = next((p for p in (AUDIO_DIR / f"{stem}.mp3", AUDIO_DIR / f"{stem}.wav") if p.exists()), None)
        self.alignment = None
        align_path = AUDIO_DIR / f"{stem}.json"
        if align_path.exists():
            data = json.loads(align_path.read_text())
            self.alignment = data.get("alignment", data)
        if self.audio:
            self.duration = _probe_duration(self.audio)
        else:
            self.duration = len(self.text.split()) / WORDS_PER_MINUTE * 60
        self._norm, self._index = _normalize(self.text)

    def time_of(self, phrase: str) -> float:
        needle, _ = _normalize(speakable(phrase))
        needle = needle.strip()
        pos = self._norm.find(needle)
        if pos < 0:
            raise ValueError(
                f"cue {phrase!r} not found in narration ep{self.episode:02d}/s{self.number:02d}")
        char = self._index[pos]
        if self.alignment:
            # The alignment covers the text that was sent; locate the phrase in it.
            starts = self.alignment["character_start_times_seconds"]
            aligned, aligned_index = _normalize("".join(self.alignment["characters"]))
            apos = aligned.find(needle)
            if apos >= 0:
                return starts[aligned_index[apos]]
        return self.duration * char / max(len(self.text), 1)


def _probe_duration(path: Path) -> float:
    result = subprocess.run(
        ["ffprobe", "-v", "error", "-show_entries", "format=duration",
         "-of", "default=noprint_wrappers=1:nokey=1", str(path)],
        capture_output=True, text=True, check=True)
    return float(result.stdout.strip())


class NarratedScene(Scene):
    """A Scene paced by its narration clip.

    Subclasses set `episode` and `number`. Use `self.cue("phrase")` to wait
    until the narrator reaches a phrase (written as it appears in the script),
    and `self.play_at("phrase", *animations)` as a shortcut. When construct()
    returns, the scene holds until the narration ends, plus `tail` seconds.
    """

    episode: int = 0
    number: int = 0
    tail: float = 0.75

    def setup(self):
        self.narration = Narration(self.episode, self.number)
        self.late_cues: list[tuple[str, float]] = []
        if self.narration.audio:
            self.add_sound(str(self.narration.audio))

    @property
    def now(self) -> float:
        return self.renderer.time

    def cue(self, phrase: str, offset: float = 0.0):
        target = self.narration.time_of(phrase) + offset
        delay = target - self.now
        if delay > 1 / config.frame_rate:
            self.wait(delay)
        elif delay < -0.5:
            self.late_cues.append((phrase, -delay))

    def play_at(self, phrase: str, *animations, offset: float = 0.0, **kwargs):
        self.cue(phrase, offset)
        self.play(*animations, **kwargs)

    def clear_scene(self, run_time: float = 0.6):
        if self.mobjects:
            self.play(*[FadeOut(m) for m in self.mobjects], run_time=run_time)

    def tear_down(self):
        remaining = self.narration.duration + self.tail - self.now
        if remaining > 0:
            self.wait(remaining)
        for phrase, late in self.late_cues:
            print(f"[timing] {type(self).__name__}: animations ran {late:.1f}s past cue {phrase!r}")
        if self.now > self.narration.duration + self.tail + 0.5:
            print(f"[timing] {type(self).__name__}: scene runs {self.now:.1f}s, "
                  f"narration is {self.narration.duration:.1f}s")


# ---------------------------------------------------------------------------
# Recurring visuals
# ---------------------------------------------------------------------------


class ByteRibbon(VGroup):
    """A horizontal strip of byte cells.

    `cells` are the strings shown in each cell (characters or hex). Long
    content is shown as the first `max_cells` cells plus an ellipsis cell.
    """

    def __init__(self, cells: list[str], cell_size: float = 0.42, color=WHITE,
                 fill=GREY_E, max_cells: int | None = None, font_size: float | None = None,
                 **kwargs):
        super().__init__(**kwargs)
        truncated = max_cells is not None and len(cells) > max_cells
        shown = cells[:max_cells] if truncated else cells
        font_size = font_size or cell_size * 48
        self.cells = VGroup()
        for content in shown:
            box = Square(cell_size, stroke_color=color, stroke_width=2,
                         fill_color=fill, fill_opacity=1)
            glyph = mono(content if content.strip() else "·", font_size, color)
            if glyph.width > cell_size * 0.85:
                glyph.scale_to_fit_width(cell_size * 0.85)
            glyph.move_to(box)
            self.cells.add(VGroup(box, glyph))
        self.cells.arrange(RIGHT, buff=0)
        self.add(self.cells)
        if truncated:
            dots = mono("…", font_size, color).next_to(self.cells, RIGHT, buff=0.12)
            self.add(dots)

    @classmethod
    def text(cls, data: bytes, **kwargs) -> "ByteRibbon":
        return cls([chr(b) if 32 <= b < 127 else "·" for b in data], **kwargs)

    @classmethod
    def hex(cls, data: bytes, **kwargs) -> "ByteRibbon":
        kwargs.setdefault("cell_size", 0.55)
        return cls([f"{b:02X}" for b in data], **kwargs)

    @classmethod
    def blank(cls, n: int, **kwargs) -> "ByteRibbon":
        return cls([" "] * n, **kwargs)

    def tint(self, indices, color) -> "ByteRibbon":
        for i in indices:
            self.cells[i][0].set_fill(color, opacity=0.55)
        return self


class BitStrip(VGroup):
    """A row of 0/1 cells, optionally with groups marked by color."""

    def __init__(self, bits: str, cell: float = 0.3, color=WHITE, **kwargs):
        super().__init__(**kwargs)
        self.bits = bits
        for b in bits:
            box = Square(cell, stroke_width=1.5, stroke_color=GREY_B,
                         fill_color=GREY_E, fill_opacity=1)
            glyph = mono(b, cell * 55, color).move_to(box)
            self.add(VGroup(box, glyph))
        self.arrange(RIGHT, buff=0)

    def group_boxes(self, size: int = 6, colors=(TEAL, YELLOW)) -> VGroup:
        """Rounded outlines around consecutive groups of `size` bits."""
        outlines = VGroup()
        for k, start in enumerate(range(0, len(self), size)):
            chunk = self[start:start + size]
            outline = SurroundingRectangle(chunk, buff=0.03, corner_radius=0.06,
                                           color=colors[k % len(colors)], stroke_width=3)
            outlines.add(outline)
        return outlines


class RecipeMachine(VGroup):
    """A rounded box with a crank: ribbons in on the left, tags out on the right."""

    def __init__(self, name: str = "public recipe", width: float = 3.2, height: float = 1.8,
                 color=WHITE, icon: Mobject | None = None, **kwargs):
        super().__init__(**kwargs)
        self.body = RoundedRectangle(corner_radius=0.3, width=width, height=height,
                                     stroke_color=color, fill_color=GREY_E, fill_opacity=1)
        self.name = label(name, 26, color).move_to(self.body)
        hub = Dot(self.body.get_top() + UP * 0.35, color=color)
        arm = Line(hub.get_center(), hub.get_center() + RIGHT * 0.45 + UP * 0.25, color=color, stroke_width=5)
        knob = Dot(arm.get_end(), radius=0.09, color=color)
        post = Line(self.body.get_top(), hub.get_center(), color=color, stroke_width=5)
        self.crank = VGroup(arm, knob)
        self.hub = hub
        self.add(self.body, post, hub, self.crank, self.name)
        if icon is not None:
            icon.scale_to_fit_height(height * 0.45).move_to(self.body)
            self.name.next_to(icon, DOWN, buff=0.1).scale(0.85)
            self.add(icon)

    @property
    def intake(self) -> np.ndarray:
        return self.body.get_left()

    @property
    def outlet(self) -> np.ndarray:
        return self.body.get_right()

    def turn(self, turns: float = 2, run_time: float = 1.2) -> Animation:
        return Rotate(self.crank, angle=-TAU * turns, about_point=self.hub.get_center(),
                      run_time=run_time, rate_func=smooth)


def feed_through(machine: RecipeMachine, ribbon: Mobject, product: Mobject,
                 run_time: float = 2.4) -> Succession:
    """Ribbon slides into the machine, crank turns, product emerges on the right."""
    product.next_to(machine.body, RIGHT, buff=0.4)
    return Succession(
        ribbon.animate(run_time=run_time * 0.35).scale(0.3).move_to(machine.intake),
        AnimationGroup(FadeOut(ribbon, run_time=0.2), machine.turn(run_time=run_time * 0.4)),
        FadeIn(product, shift=RIGHT * 0.4, run_time=run_time * 0.25),
    )


class ServerRack(VGroup):
    def __init__(self, color=GREY_B, face: str | None = None, height: float = 1.4, **kwargs):
        super().__init__(**kwargs)
        units = VGroup()
        for _ in range(3):
            unit = RoundedRectangle(corner_radius=0.06, width=1.0, height=0.36,
                                    stroke_color=color, fill_color=GREY_E, fill_opacity=1)
            light = Dot(unit.get_right() + LEFT * 0.16, radius=0.05, color=GREEN_C)
            slot = Line(unit.get_left() + RIGHT * 0.15, unit.get_center() + RIGHT * 0.1,
                        color=color, stroke_width=2)
            units.add(VGroup(unit, slot, light))
        units.arrange(DOWN, buff=0.06)
        self.units = units
        self.add(units)
        if face:
            badge = Circle(0.24, color=color, fill_color=BLACK, fill_opacity=1).move_to(units[1])
            self.add(badge, label(face, 26, color).move_to(badge))
        self.scale_to_fit_height(height)

    def lights(self) -> VGroup:
        return VGroup(*[u[2] for u in self.units])


class CacheBox(VGroup):
    def __init__(self, color=TEAL, size: float = 1.4, **kwargs):
        super().__init__(**kwargs)
        box = RoundedRectangle(corner_radius=0.15, width=size, height=size * 0.8,
                               stroke_color=color, fill_color=GREY_E, fill_opacity=1)
        inf = label("∞", size * 40, color).move_to(box)
        name = label("cache", 22, color).next_to(box, DOWN, buff=0.12)
        self.add(box, inf, name)


class FingerprintIcon(VGroup):
    def __init__(self, color=HASH_COLOR, size: float = 1.0, **kwargs):
        super().__init__(**kwargs)
        for i in range(6):
            r = 0.15 + i * 0.13
            self.add(Arc(radius=r, start_angle=-0.3 + 0.1 * i, angle=PI + 0.6 - 0.15 * i,
                         color=color, stroke_width=4))
        self.add(Line(DOWN * 0.1, DOWN * 0.75, color=color, stroke_width=4).shift(LEFT * 0.12))
        self.scale_to_fit_height(size)


class VerifierLoop(VGroup):
    """request → receive → compute → compare, ending in a check or a cross."""

    STEPS = ("request CID", "receive bytes", "compute CID", "compare strings")

    def __init__(self, width: float = 9.0, color=WHITE, steps=STEPS, station_width: float = 2.0,
                 font_size: float = 20, verdict_side=RIGHT, **kwargs):
        super().__init__(**kwargs)
        self.stations = VGroup()
        for name in steps:
            pill = RoundedRectangle(corner_radius=0.25, width=station_width, height=0.8,
                                    stroke_color=color, fill_color=GREY_E, fill_opacity=1)
            words = fit_width(label(name, font_size, color), station_width - 0.2)
            self.stations.add(VGroup(pill, words.move_to(pill)))
        gap = (width - station_width * len(steps)) / (len(steps) - 1)
        self.stations.arrange(RIGHT, buff=max(gap, 0.25))
        self.arrows = VGroup(*[
            Arrow(a.get_right(), b.get_left(), buff=0.08, stroke_width=3, color=GREY_B)
            for a, b in zip(self.stations, self.stations[1:])
        ])
        back = CurvedArrow(self.stations[-1].get_bottom() + DOWN * 0.05,
                           self.stations[0].get_bottom() + DOWN * 0.05,
                           angle=-PI / 4, color=GREY_D, stroke_width=2)
        self.loop_back = back
        self.verdict_side = verdict_side
        self.verdict_anchor = self.stations[-1].get_critical_point(verdict_side) + verdict_side * 0.6
        self.add(self.stations, self.arrows, back)

    def run(self, ok: bool = True, step_time: float = 0.35) -> Succession:
        anims = []
        for station in self.stations:
            anims.append(Indicate(station, color=YELLOW, scale_factor=1.08, run_time=step_time))
        anchor = self.stations[-1].get_critical_point(self.verdict_side) + self.verdict_side * 0.6
        verdict = (Check() if ok else XMark()).move_to(anchor)
        self.last_verdict = verdict
        anims.append(GrowFromCenter(verdict, run_time=0.3))
        return Succession(*anims)


class LengthOdometer(VGroup):
    """Eight base-64 wheels showing a length prefix."""

    def __init__(self, n: int = 0, size: float = 0.7, **kwargs):
        super().__init__(**kwargs)
        self.size = size
        self.frames = VGroup(*[
            RoundedRectangle(corner_radius=0.08, width=size, height=size * 1.3,
                             stroke_color=LENGTH_COLOR, fill_color=GREY_E, fill_opacity=1)
            for _ in range(8)]).arrange(RIGHT, buff=0.08)
        self.digits = self._digits(n)
        self.value = length_prefix(n)
        self.add(self.frames, self.digits)

    def _digits(self, n: int) -> VGroup:
        prefix = length_prefix(n)
        return VGroup(*[mono(ch, self.size * 50, LENGTH_COLOR).move_to(frame)
                        for ch, frame in zip(prefix, self.frames)])

    def roll_to(self, n: int, run_time: float = 0.8) -> AnimationGroup:
        new = self._digits(n)
        target = length_prefix(n)
        anims = [Transform(old, fresh, path_arc=-PI / 3)
                 for old, fresh, a, b in zip(self.digits, new, self.value, target) if a != b]
        self.value = target
        return AnimationGroup(*anims, run_time=run_time) if anims else Wait(run_time)


class PairWeb(VGroup):
    """`n` dots around a circle, joined by every pairwise line."""

    def __init__(self, n: int, radius: float = 2.5, seed: int = 1, color=YELLOW, **kwargs):
        super().__init__(**kwargs)
        rng = np.random.default_rng(seed)
        angles = np.sort(rng.uniform(0, TAU, n))
        self.dots = VGroup(*[Dot(radius * np.array([math.cos(a), math.sin(a), 0]), color=color)
                             for a in angles])
        self.lines = VGroup(*[
            Line(self.dots[i].get_center(), self.dots[j].get_center(), stroke_width=1,
                 stroke_opacity=0.5, color=BLUE_B)
            for i in range(n) for j in range(i)])
        self.add(self.lines, self.dots)


class DiffusionGrid(VGroup):
    """512 cells (16 × 32): which output bits changed."""

    def __init__(self, cell: float = 0.2, rows: int = 16, cols: int = 32, **kwargs):
        super().__init__(**kwargs)
        for _ in range(rows * cols):
            self.add(Square(cell, stroke_width=0.5, stroke_color=GREY_D,
                            fill_color=GREY_E, fill_opacity=1))
        self.arrange_in_grid(rows, cols, buff=0)

    def show_mask(self, mask: str, color=YELLOW, run_time: float = 0.8) -> AnimationGroup:
        anims = []
        for cell, bit in zip(self, mask):
            target = color if bit == "1" else GREY_E
            anims.append(cell.animate.set_fill(target, opacity=1))
        return AnimationGroup(*anims, run_time=run_time)


def diff_mask(a: bytes, b: bytes) -> str:
    return "".join("1" if x != y else "0" for x, y in zip(bits_of(a), bits_of(b)))


class StreamGauge(VGroup):
    """A progress bar for a download of `length` bytes, with a hard stop at `length`."""

    def __init__(self, length: int, width: float = 9.0, overflow: float = 0.25, **kwargs):
        super().__init__(**kwargs)
        self.length = length
        self.unit = width / length
        total = width * (1 + overflow)
        self.track = Rectangle(width=total, height=0.5, stroke_color=GREY_B)
        self.track.move_to(ORIGIN)
        self.origin = self.track.get_left()
        self.stop_x = self.origin + RIGHT * width
        self.stop = DashedLine(self.stop_x + UP * 0.55, self.stop_x + DOWN * 0.55,
                               color=LENGTH_COLOR, stroke_width=4)
        self.stop_label = label(f"L = {length:,} bytes", 22, LENGTH_COLOR).next_to(self.stop, UP, buff=0.1)
        self.fill = Rectangle(width=0.001, height=0.46, stroke_width=0, fill_color=TEAL,
                              fill_opacity=0.8).move_to(self.origin, aligned_edge=LEFT)
        self.add(self.track, self.fill, self.stop, self.stop_label)

    def fill_to(self, nbytes: float, color=TEAL, run_time: float = 1.5) -> Animation:
        width = max(nbytes * self.unit, 0.001)
        target = Rectangle(width=width, height=0.46, stroke_width=0, fill_color=color,
                           fill_opacity=0.8).move_to(self.origin, aligned_edge=LEFT)
        return Transform(self.fill, target, run_time=run_time, rate_func=linear)


class MerkleTree(VGroup):
    def __init__(self, leaves: int = 8, width: float = 9.0, level_gap: float = 1.0, **kwargs):
        super().__init__(**kwargs)
        self.levels: list[VGroup] = []
        count = leaves
        y = 0.0
        while count >= 1:
            level = VGroup(*[Square(0.4, stroke_color=HASH_COLOR, fill_color=GREY_E, fill_opacity=1)
                             for _ in range(count)])
            level.arrange(RIGHT, buff=(width - 0.4 * count) / max(count - 1, 1) if count > 1 else 0)
            level.move_to(UP * y)
            self.levels.append(level)
            if count == 1:
                break
            count //= 2
            y += level_gap
        self.edges = VGroup()
        for lower, upper in zip(self.levels, self.levels[1:]):
            for k, parent in enumerate(upper):
                for child in lower[2 * k:2 * k + 2]:
                    self.edges.add(Line(child.get_top(), parent.get_bottom(), stroke_width=2, color=GREY_B))
        self.add(self.edges, *self.levels)

    @property
    def leaves(self) -> VGroup:
        return self.levels[0]

    @property
    def root(self) -> Mobject:
        return self.levels[-1][0]


class MaskedFigure(VGroup):
    """A simple adversary figure: head with a mask band, body, and a prop."""

    def __init__(self, prop: str = "liar", color=ADVERSARY_COLOR, **kwargs):
        super().__init__(**kwargs)
        head = Circle(0.38, color=color, fill_color=color, fill_opacity=0.9)
        mask = Rectangle(width=0.8, height=0.16, stroke_width=0, fill_color=BLACK,
                         fill_opacity=1).move_to(head).shift(UP * 0.04)
        eyes = VGroup(Dot(LEFT * 0.14, radius=0.04), Dot(RIGHT * 0.14, radius=0.04)).move_to(mask)
        body = RoundedRectangle(corner_radius=0.3, width=1.0, height=1.2, color=color,
                                fill_color=color, fill_opacity=0.7).next_to(head, DOWN, buff=0.05)
        self.add(body, head, mask, eyes)
        if prop == "liar":
            can = VGroup(Rectangle(width=0.4, height=0.5, color=BAD_COLOR, fill_color=BAD_COLOR,
                                   fill_opacity=0.8),
                         Arc(radius=0.2, start_angle=0, angle=PI, color=GREY_B).shift(UP * 0.25))
            can.next_to(body, RIGHT, buff=0.05).shift(DOWN * 0.2)
            self.add(can)
        else:
            plan = VGroup(Rectangle(width=0.7, height=0.5, color=BLUE_B, fill_color=BLUE_E, fill_opacity=0.9),
                          *[Line(LEFT * 0.25, RIGHT * 0.25, color=BLUE_B, stroke_width=1).shift(UP * dy)
                            for dy in (-0.12, 0, 0.12)])
            plan.next_to(body, RIGHT, buff=0.05).shift(DOWN * 0.1)
            self.add(plan)


def tag_box(text: str, color=WHITE, size: float = 24) -> VGroup:
    """A small labeled rounded box, used for cards, tags and badges."""
    words = label(text, size, color)
    box = RoundedRectangle(corner_radius=0.15, width=words.width + 0.5, height=words.height + 0.4,
                           stroke_color=color, fill_color=BLACK, fill_opacity=0.85)
    return VGroup(box, words.move_to(box))


def card(title: str, body: str | None = None, color=WHITE, width: float = 4.2,
         height: float = 2.2, icon: Mobject | None = None) -> VGroup:
    frame = RoundedRectangle(corner_radius=0.2, width=width, height=height,
                             stroke_color=color, fill_color=GREY_E, fill_opacity=0.9)
    parts = VGroup(label(title, 30, color))
    if body:
        parts.add(fit_width(label(body, 20, GREY_A), width - 0.4))
    parts.arrange(DOWN, buff=0.18)
    if icon is not None:
        icon.scale_to_fit_height(min(0.7, height * 0.3))
        parts = VGroup(icon, parts).arrange(DOWN, buff=0.15)
    parts.move_to(frame)
    if parts.height > height - 0.2:
        parts.scale_to_fit_height(height - 0.3)
    return VGroup(frame, parts)


def quote_card(quote: str, source: str, width: float = 11.0) -> VGroup:
    text = label(quote, 34, WHITE, slant=ITALIC)
    fit_width(text, width - 0.8)
    src = label(f"— {source}", 24, GREY_B).next_to(text, DOWN, buff=0.3).align_to(text, RIGHT)
    frame = SurroundingRectangle(VGroup(text, src), buff=0.4, corner_radius=0.2,
                                 stroke_color=GREY_B, fill_color=GREY_E, fill_opacity=0.9)
    return VGroup(frame, text, src)


def pow2(exp: str | int, base: str = "2", size: float = 36, color=WHITE) -> VGroup:
    """Typeset b^e with a raised exponent using Text only (no LaTeX)."""
    b = label(base, size, color)
    e = label(str(exp), size * 0.6, color).next_to(b, UR, buff=0.02).shift(DOWN * size * 0.004)
    return VGroup(b, e)


def episode_card(number: int, title: str, icon: Mobject | None = None) -> VGroup:
    frame = RoundedRectangle(corner_radius=0.15, width=1.75, height=2.3,
                             stroke_color=GREY_B, fill_color=GREY_E, fill_opacity=1)
    num = label(str(number), 40, YELLOW).move_to(frame.get_top() + DOWN * 0.45)
    name = fit_width(label(title.replace(" ", "\n", 1) if len(title) > 12 else title, 22), 1.6)
    name.move_to(frame.get_bottom() + UP * 0.55)
    group = VGroup(frame, num, name)
    if icon is not None:
        icon.scale_to_fit_height(0.6).move_to(frame)
        group.add(icon)
    return group


EPISODE_TITLES = [
    "Where vs. What",
    "Anatomy of a CID",
    "What a Hash Promises",
    "Accidental Collisions",
    "Adversarial Collisions",
    "Verify, Cache, Trust",
    "Limits and Neighbors",
]


def series_row() -> VGroup:
    cards = VGroup(*[episode_card(i + 1, t) for i, t in enumerate(EPISODE_TITLES)])
    return cards.arrange(RIGHT, buff=0.15)
