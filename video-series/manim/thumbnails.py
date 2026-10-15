"""YouTube thumbnails for the seven episodes, one still Scene per episode.

Same palette, type and visuals as the episodes. Render them all (1280×720 PNGs
in video-series/youtube/epNN/thumbnail.png) with:
    python3 video-series/youtube/build.py --thumbnails
"""

import hashlib

from common import *

HEADLINE = 76
ACCENT = YELLOW


def headline(text: str, size: float = HEADLINE, color=WHITE) -> Text:
    return Text(text, font=SANS, weight=BOLD, font_size=size, color=color, line_spacing=0.8)


class Thumbnail(Scene):
    """Series badge in the top-left corner; subclasses draw the rest."""

    episode = 0

    def badge(self) -> VGroup:
        series = Text("256t", font=MONO, weight=BOLD, font_size=34, color=WHITE)
        number = Text(f"EP {self.episode}", font=SANS, weight=BOLD, font_size=34, color=BLACK)
        pill = RoundedRectangle(corner_radius=0.18, width=number.width + 0.45, height=number.height + 0.3,
                                stroke_width=0, fill_color=ACCENT, fill_opacity=1)
        number.move_to(pill)
        group = VGroup(series, VGroup(pill, number)).arrange(RIGHT, buff=0.3)
        return group.to_corner(UL, buff=0.45)

    def construct(self):
        self.add(self.badge())
        self.draw()

    def draw(self):
        raise NotImplementedError


def cid_tag(cid: str, size: float = 34, shown: int | None = None) -> VGroup:
    # CIDText colors by the length in the first 8 characters, so a shortened CID keeps its colors.
    words = CIDText(cid if shown is None or len(cid) <= shown else cid[:shown] + "…", size=size)
    box = SurroundingRectangle(words, buff=0.28, corner_radius=0.15, stroke_color=GREY_B,
                               fill_color=GREY_E, fill_opacity=1)
    return VGroup(box, words)


class T01WhereVsWhat(Thumbnail):
    episode = 1

    def draw(self):
        words = headline("Names that\ncan't break").to_edge(LEFT, buff=0.7).shift(DOWN * 0.6)
        self.add(words)

        broken = VGroup(
            mono("example.com/report.pdf", 24, GREY_B),
            label("404 Not Found", 40, GREY_A),
        ).arrange(DOWN, buff=0.25)
        broken_box = SurroundingRectangle(broken, buff=0.35, corner_radius=0.15, stroke_color=GREY_C,
                                          fill_color=GREY_E, fill_opacity=1)
        where = VGroup(broken_box, broken)
        cross = XMark(0.7).next_to(broken_box, RIGHT, buff=0.25)
        tag = cid_tag(GETTYSBURG_CID, 34, shown=16)
        arrow = Arrow(UP * 0.5, DOWN * 0.5, color=GREY_B, stroke_width=6)
        check = Check(0.7).next_to(tag, RIGHT, buff=0.25)
        right = VGroup(VGroup(where, cross), arrow, VGroup(tag, check)).arrange(DOWN, buff=0.35)
        right.to_edge(RIGHT, buff=0.6).shift(DOWN * 0.3)
        arrow.align_to(where, LEFT).shift(RIGHT * (where.width / 2 - arrow.width / 2))
        tags = VGroup(label("where", 32, GREY_B).next_to(where, UP, buff=0.15).align_to(where, LEFT),
                      label("what", 32, GREEN).next_to(tag, UP, buff=0.15).align_to(tag, LEFT))
        self.add(right, tags)


class T02AnatomyOfACID(Thumbnail):
    episode = 2

    def draw(self):
        words = headline("Read a CID\nby hand").to_edge(LEFT, buff=0.7).shift(UP * 0.6)
        self.add(words)
        cid = CIDText(GETTYSBURG_CID, size=34, max_width=FRAME_W - 1.0).to_edge(DOWN, buff=1.5)
        prefix = Brace(cid.prefix, DOWN, color=LENGTH_COLOR)
        suffix = Brace(cid.suffix, DOWN, color=HASH_COLOR)
        notes = VGroup(
            label("8 · length", 34, LENGTH_COLOR).next_to(prefix, DOWN, buff=0.15),
            label("86 · fingerprint", 34, HASH_COLOR).next_to(suffix, DOWN, buff=0.15),
        )
        total = headline("8 + 86 = 94", 72, ACCENT).to_edge(RIGHT, buff=0.7).shift(UP * 1.4)
        odometer = LengthOdometer(len(GETTYSBURG), size=0.62).next_to(total, DOWN, buff=0.5).align_to(total, RIGHT)
        self.add(cid, prefix, suffix, notes, total, odometer)


class T03WhatAHashPromises(Thumbnail):
    episode = 3

    def draw(self):
        pair = VGroup(mono("Hello", 72, WHITE), mono("hello", 72, WHITE)).arrange(DOWN, buff=0.35, aligned_edge=LEFT)
        pair[0][0].set_color(ACCENT)
        pair[1][0].set_color(ACCENT)
        words = headline("1 letter\nchanged").next_to(pair, UP, buff=0.6, aligned_edge=LEFT)
        left = VGroup(words, pair).scale(0.92).to_edge(LEFT, buff=0.7).to_edge(DOWN, buff=0.7)
        grid = DiffusionGrid(cell=0.21)
        mask = diff_mask(hashlib.sha512(b"Hello").digest(), hashlib.sha512(b"hello").digest())
        for cell, bit in zip(grid, mask):
            if bit == "1":
                cell.set_fill(HASH_COLOR, opacity=1)
        grid.to_edge(RIGHT, buff=0.6).shift(DOWN * 0.1)
        count = mask.count("1")
        note = label(f"{count} of 512 bits flip", 38, HASH_COLOR).next_to(grid, DOWN, buff=0.3)
        title = label("SHA-512", 34, GREY_B).next_to(grid, UP, buff=0.25)
        self.add(left, grid, note, title)


class T04AccidentalCollisions(Thumbnail):
    episode = 4

    def draw(self):
        web = PairWeb(23, radius=3.1, seed=4).set_opacity(0.35).to_edge(RIGHT, buff=0.4)
        self.add(web)
        base = headline("2", 230, HASH_COLOR)
        exponent = headline("−271", 120, HASH_COLOR).next_to(base, RIGHT, buff=0.1).align_to(base, UP).shift(UP * 0.35)
        odds = VGroup(base, exponent).move_to(RIGHT * 2.6 + DOWN * 0.1)
        words = headline("Odds of an\naccidental\ncollision", 58).to_edge(LEFT, buff=0.7).shift(DOWN * 0.3)
        note = label("a billion files per person per second, since the Big Bang", 26, GREY_A)
        note.to_edge(DOWN, buff=0.45)
        self.add(odds, words, note)


class T05AdversarialCollisions(Thumbnail):
    episode = 5

    def draw(self):
        words = headline("It's easier\nto lie").to_edge(LEFT, buff=0.7).shift(DOWN * 0.4)
        liar = MaskedFigure("liar").scale(1.6)
        forger = MaskedFigure("forger").scale(1.6)
        figures = VGroup(liar, forger).arrange(RIGHT, buff=1.2).to_edge(RIGHT, buff=0.8).shift(UP * 0.4)
        names = VGroup(label("liar", 34, PURPLE_A).next_to(liar, DOWN, buff=0.3),
                       label("forger", 34, PURPLE_A).next_to(forger, DOWN, buff=0.3))
        caught = label("caught by\none hash", 30, OK_COLOR).next_to(names[0], DOWN, buff=0.3)
        cost = label("needs\n2²⁵⁶ tries", 30, HASH_COLOR).next_to(names[1], DOWN, buff=0.3)
        self.add(words, figures, names, caught, cost)


class T06VerifyCacheTrust(Thumbnail):
    episode = 6

    def draw(self):
        words = headline("Verify once.\nCache forever.", 70).to_edge(LEFT, buff=0.7).shift(UP * 0.9)
        self.add(words)
        loop = VerifierLoop(width=11.4, station_width=2.5, font_size=26).to_edge(DOWN, buff=1.2).to_edge(LEFT, buff=0.6)
        verdict = Check(0.9).next_to(loop.stations[-1], RIGHT, buff=0.3)
        cache = CacheBox(size=2.2).to_edge(RIGHT, buff=0.9).shift(UP * 1.2)
        self.add(loop, verdict, cache)


class T07LimitsAndNeighbors(Thumbnail):
    episode = 7

    def draw(self):
        words = headline("256t vs.\nits neighbors", 66).to_edge(LEFT, buff=0.7).shift(DOWN * 0.3)
        center = tag_box("256t", ACCENT, 46).move_to(RIGHT * 3.2 + DOWN * 0.2)
        names = ["Git", "IPFS", "BitTorrent", "SRI", "data: URIs", "ni: URIs"]
        ring = VGroup()
        for index, name in enumerate(names):
            angle = PI / 2 + index * TAU / len(names)
            position = center.get_center() + np.array([math.cos(angle) * 2.9, math.sin(angle) * 2.5, 0])
            node = tag_box(name, GREY_A, 30).move_to(position)
            line = Line(center.get_center(), node.get_center(), color=GREY_D, stroke_width=3)
            ring.add(VGroup(line, node))
        for item in ring:
            item[0].put_start_and_end_on(
                center.get_boundary_point(normalize(item[1].get_center() - center.get_center())),
                item[1].get_boundary_point(normalize(center.get_center() - item[1].get_center())))
        self.add(words, ring, center)
