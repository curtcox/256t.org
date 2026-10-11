"""Episode 2 — Anatomy of a CID.

Scenes follow video-series/ep02-anatomy-of-a-cid.md. Render with:
    python3 video-series/manim/render.py 2
"""

from common import *

OPENING_CIDS = [compute_cid(b"A"), compute_cid(b"Hello"), GETTYSBURG_CID]
IMPLEMENTATIONS = sorted(p.name for p in (REPO_DIR / "implementations").iterdir() if p.is_dir())


def opening_stack(size: float = 30) -> VGroup:
    rows = VGroup(*[CIDText(c, size=size, colored=False) for c in OPENING_CIDS])
    rows.arrange(DOWN, aligned_edge=LEFT, buff=0.45)
    fit_width(rows, FRAME_W - 1.0)
    return rows


def alphabet_table() -> VGroup:
    bands = [(0, 26, BLUE_D), (26, 52, TEAL_D), (52, 62, GREEN_D), (62, 64, GOLD_D)]
    cells = VGroup()
    for i, ch in enumerate(B64_ALPHABET):
        color = next(c for lo, hi, c in bands if lo <= i < hi)
        box = Rectangle(width=0.72, height=0.72, stroke_width=1, stroke_color=GREY_B,
                        fill_color=color, fill_opacity=0.45)
        glyph = mono(ch, 26).move_to(box).shift(UP * 0.08)
        index = label(str(i), 12, GREY_A).move_to(box.get_bottom() + UP * 0.12)
        cells.add(VGroup(box, glyph, index))
    cells.arrange_in_grid(4, 16, buff=0)
    return cells


class S01ThreeNames(NarratedScene):
    episode, number = 2, 1

    def construct(self):
        rows = opening_stack()
        self.play(LaggedStart(*[FadeIn(r, shift=RIGHT * 0.3) for r in rows], lag_ratio=0.3))
        covers = VGroup()
        for r in rows:
            cover = SurroundingRectangle(r.suffix, buff=0.06, stroke_width=0, fill_color=GREY_D, fill_opacity=1)
            q = fit_width(label("?", 22, GREY_A), cover.width * 0.9).move_to(cover)
            covers.add(VGroup(cover, q))
        self.play_at("Right now they probably look like noise", FadeIn(covers))
        self.play_at("read the first one by hand", Indicate(rows[0], color=YELLOW))
        self.play_at("read the length out of all three",
                     *[r.prefix.animate.set_color(LENGTH_COLOR) for r in rows])
        self.play_at("And you might already have noticed",
                     *[r.prefix.animate.set_color(WHITE) for r in rows])
        a_runs = VGroup()
        for r in rows:
            count = len(r.cid) - len(r.cid.lstrip("A"))
            a_runs.add(r.chars(0, count))
        self.play_at("so many capital A's",
                     LaggedStart(*[Indicate(a, color=YELLOW, scale_factor=1.15) for a in a_runs], lag_ratio=0.2))
        self.play(a_runs.animate.set_color(YELLOW))


class S02SixBitCharacters(NarratedScene):
    episode, number = 2, 2

    def construct(self):
        byte = BitStrip("01000001", cell=0.6)
        eq = label('= 65 = "A"', 36).next_to(byte, RIGHT, buff=0.4)
        group = VGroup(byte, eq).move_to(UP * 0.5)
        self.play(FadeIn(byte, lag_ratio=0.1))
        self.play_at("which is the number sixty-five", FadeIn(eq))

        self.play_at("But eight-bit bytes are awkward", FadeOut(group))
        table = alphabet_table().move_to(UP * 0.6)
        name = label("base64url (RFC 4648 §5)", 32).to_edge(UP, buff=0.3)
        self.play_at("So 256t uses an encoding called base64url", FadeIn(name),
                     LaggedStart(*[FadeIn(c) for c in table], lag_ratio=0.01, run_time=1.5))
        bands = [("A–Z → 0–25", 0, 26), ("a–z → 26–51", 26, 52), ("0–9 → 52–61", 52, 62), ("- _ → 62, 63", 62, 64)]
        for (text, lo, hi), phrase in zip(bands, ["the capital letters", "the lowercase letters",
                                                  "the ten digits", "a dash and an underscore"]):
            self.play_at(phrase, Indicate(table[lo:hi], color=YELLOW, scale_factor=1.05), run_time=0.6)
        bits = label("64 symbols = 2⁶  →  each character carries exactly 6 bits", 30, YELLOW)
        bits.next_to(table, DOWN, buff=0.5)
        self.play_at("And sixty-four is two to the sixth", Write(bits))

        # The "url" aside.
        self.play_at('Quick aside on that "url" part', FadeOut(VGroup(table, bits)))
        plus, slash = mono("+", 60), mono("/", 60)
        dash, under = mono("-", 60, GREEN), mono("_", 60, GREEN)
        pair = VGroup(plus, slash).arrange(RIGHT, buff=1.2).move_to(LEFT * 2)
        circles = VGroup(*[Circle(0.5, color=RED).move_to(m) for m in pair])
        std = label("standard base64", 26, GREY_B).next_to(pair, DOWN, buff=0.6)
        self.play(FadeIn(pair), FadeIn(std))
        self.play_at("those both mean something special", Create(circles))
        safe = VGroup(dash, under).arrange(RIGHT, buff=1.2).move_to(pair)
        self.play_at("The URL-safe variant swaps them",
                     FadeOut(circles), ReplacementTransform(plus, dash), ReplacementTransform(slash, under),
                     Transform(std, label("base64url", 26, GREEN).move_to(std)))
        pad = mono("==", 60, GREY_B).move_to(RIGHT * 3 + UP * 0.2)
        bin_ = VGroup(Rectangle(width=1.4, height=1.0, color=GREY_B),
                      label("padding: dropped", 20, GREY_B)).arrange(DOWN, buff=0.15).move_to(RIGHT * 3 + DOWN * 1.8)
        self.play_at("It also drops the equals signs", FadeIn(pad), FadeIn(bin_))
        self.play(pad.animate.move_to(bin_[0]).scale(0.6), run_time=0.8)
        self.play(FadeOut(pad))

        # The gear: 3 bytes = 24 bits = 4 characters.
        self.play_at("Now here is the little gear", FadeOut(VGroup(safe, std, bin_)))
        three = BitStrip(bits_of(b"Man"), cell=0.42).move_to(UP * 0.8)
        byte_marks = VGroup(*[SurroundingRectangle(three[i:i + 8], buff=0.02, color=WHITE, stroke_width=2)
                              for i in (0, 8, 16)])
        byte_names = VGroup(*[label(f"byte {k + 1}", 20, GREY_B).next_to(m, UP, buff=0.1)
                              for k, m in enumerate(byte_marks)])
        self.play(FadeIn(three), Create(byte_marks), FadeIn(byte_names))
        groups = three.group_boxes(6)
        self.play_at("But three bytes is twenty-four bits", FadeOut(byte_marks), FadeOut(byte_names),
                     Create(groups))
        chars = VGroup(*[mono(c, 40, YELLOW).next_to(g, DOWN, buff=0.35) for c, g in zip(b64url(b"Man"), groups)])
        self.play_at("exactly four characters", LaggedStart(*[FadeIn(c, shift=DOWN * 0.2) for c in chars],
                                                              lag_ratio=0.2))
        gear = label("3 bytes = 24 bits = 4 characters", 32).to_edge(DOWN, buff=0.8)
        self.play_at("every three bytes in, four characters out", Write(gear))

        # Worked example: "A" -> QQ.
        self.play_at("Let's do one by hand", FadeOut(VGroup(three, groups, chars, gear)))
        strip = BitStrip("01000001" + "0000", cell=0.6).move_to(UP * 0.8)
        for cell in strip[8:]:
            cell[0].set_fill(GREY_D)
            cell[1].set_color(GREY_B)
        self.play(FadeIn(strip[:8]))
        g1 = SurroundingRectangle(strip[:6], buff=0.03, color=TEAL, stroke_width=3)
        v1 = VGroup(label("16", 32, TEAL), mono("Q", 48, YELLOW)).arrange(DOWN, buff=0.2).next_to(g1, DOWN, buff=0.3)
        self.play_at("Chop off the first six bits", Create(g1))
        self.play_at("That's sixteen", FadeIn(v1[0]))
        self.play_at("the letter Q", FadeIn(v1[1]))
        left = SurroundingRectangle(strip[6:8], buff=0.03, color=YELLOW, stroke_width=3)
        self.play_at("We have two bits left over", Create(left))
        g2 = SurroundingRectangle(strip[6:12], buff=0.03, color=YELLOW, stroke_width=3)
        self.play_at("Pad them out with zeros", FadeIn(strip[8:]), Transform(left, g2))
        v2 = VGroup(label("16", 32, YELLOW), mono("Q", 48, YELLOW)).arrange(DOWN, buff=0.2).next_to(g2, DOWN, buff=0.3)
        self.play_at("that's sixteen again", FadeIn(v2))
        result = VGroup(mono('"A"', 44), label("→", 44), mono("QQ", 44, INLINE_COLOR)).arrange(RIGHT, buff=0.4)
        result.to_edge(DOWN, buff=0.7)
        self.play_at('So the single byte "A"', Write(result))
        first = CIDText(OPENING_CIDS[0], size=44).to_edge(UP, buff=0.5)
        self.play_at("Look back at the first CID", FadeIn(first))
        self.play(Indicate(first.suffix, color=YELLOW, scale_factor=1.3))


class S03LengthPrefix(NarratedScene):
    episode, number = 2, 3

    def construct(self):
        first = CIDText(OPENING_CIDS[0], size=48).to_edge(UP, buff=0.6)
        brace = Brace(first.prefix, DOWN, color=LENGTH_COLOR)
        name = label("length", 28, LENGTH_COLOR)
        brace.put_at_tip(name)
        self.play(FadeIn(first), GrowFromCenter(brace), FadeIn(name))

        strip = BitStrip(format(1, "048b"), cell=0.24).move_to(DOWN * 0.2)
        self.play_at("Eight characters at six bits each", FadeIn(strip, lag_ratio=0.02))
        groups = strip.group_boxes(6)
        self.play(Create(groups))
        bytes_ = VGroup(*[SurroundingRectangle(strip[i:i + 8], buff=0.06, color=WHITE, stroke_width=1.5)
                          for i in range(0, 48, 8)])
        fits = label("6 bytes → exactly 8 characters, no leftovers", 30, YELLOW).next_to(strip, DOWN, buff=0.6)
        self.play_at("Forty-eight bits is exactly six bytes", Create(bytes_))
        self.play_at("with nothing left over", Write(fits))
        boundary = DashedLine(first.prefix.get_right() + RIGHT * 0.04 + UP * 0.45,
                              first.prefix.get_right() + RIGHT * 0.04 + DOWN * 0.45, color=YELLOW)
        self.play_at("starts on a clean character boundary", Create(boundary))

        # The odometer.
        self.play_at("You can read the length like an odometer",
                     FadeOut(VGroup(first, brace, name, strip, groups, bytes_, fits, boundary)))
        odo = LengthOdometer(0, size=0.9).move_to(UP * 1.2)
        readout = label("0 bytes · empty file", 32).next_to(odo, DOWN, buff=0.5)
        self.play(FadeIn(odo), FadeIn(readout))

        def roll(n, text, phrase=None):
            new = label(text, 32).move_to(readout)
            anims = [odo.roll_to(n), Transform(readout, new)]
            if phrase:
                self.play_at(phrase, *anims)
            else:
                self.play(*anims)

        roll(1, "1 byte", "A one-byte file is seven A's and a B")
        roll(1511, "1,511 bytes · Gettysburg Address", "Let's decode the Gettysburg Address")
        work = VGroup(
            label("A = 0  (six times)", 28, GREY_B),
            label("X = 23,   n = 39", 28),
            label("23 × 64 + 39 = 1,511", 34, YELLOW),
        ).arrange(DOWN, buff=0.25).next_to(readout, DOWN, buff=0.5)
        self.play_at("The first six wheels are A", FadeIn(work[0]))
        self.play_at("Then X, which is twenty-three", FadeIn(work[1]), Indicate(odo.digits[6:], color=YELLOW))
        self.play_at("Twenty-three times sixty-four", Write(work[2]))

        # 2^48 = 256 TiB.
        self.play_at("So how big can the content be", FadeOut(work), FadeOut(readout), odo.animate.to_edge(UP))
        chain = VGroup(
            pow2(48, size=48),
            label("=", 48),
            pow2(8, size=48), label("·", 48), pow2(10, size=48), label("·", 48), pow2(10, size=48),
            label("·", 48), pow2(10, size=48), label("·", 48), pow2(10, size=48),
        ).arrange(RIGHT, buff=0.2).move_to(UP * 0.4)
        self.play_at("Forty-eight bits can count up", FadeIn(chain[0]))
        self.play_at("Split that up", FadeIn(chain[1:]))
        units = ["256", "Ki", "Mi", "Gi", "Ti"]
        unit_labels = VGroup(*[label(u, 36, YELLOW).next_to(chain[2 + 2 * k], DOWN, buff=0.45)
                               for k, u in enumerate(units)])
        self.play_at("the \"kilo\" step", LaggedStart(*[FadeIn(u, shift=UP * 0.2) for u in unit_labels],
                                                         lag_ratio=0.4))
        total = label("2⁴⁸ bytes = 256 TiB", 44, YELLOW).move_to(DOWN * 2)
        self.play_at("two hundred and fifty-six terabytes", Write(total))
        logo = label("256t", 96, LENGTH_COLOR).move_to(DOWN * 2)
        self.play_at("Two hundred fifty-six T", ReplacementTransform(total, logo))

        # Why all the A's.
        self.play_at("And now the A's make sense", FadeOut(VGroup(chain, unit_labels, logo)))
        readout = label("", 32).next_to(odo, DOWN, buff=0.5)
        self.add(readout)
        for n, text, phrase in [(1_048_576, "1 MiB", "A one-megabyte file"),
                                (1_073_741_824, "1 GiB", "A gigabyte only gets you"),
                                (2 ** 40, "1 TiB", "You need a full terabyte")]:
            roll(n, text, phrase)
        rows = opening_stack(26).move_to(DOWN * 1.6)
        self.play(FadeIn(rows))
        for r in rows:
            count = len(r.cid) - len(r.cid.lstrip("A"))
            r.chars(0, count).set_color(YELLOW)
        note = caption("A = 0. Real files are small compared to 256 TiB.", 28, YELLOW)
        self.play(FadeIn(note))


class S04Fingerprint(NarratedScene):
    episode, number = 2, 4

    def construct(self):
        ribbon = ByteRibbon.text(GETTYSBURG[:40], cell_size=0.34, max_cells=14).move_to(LEFT * 4.2 + UP * 2)
        machine = RecipeMachine("SHA-512", width=2.6, height=1.4).move_to(UP * 2)
        digest = hashlib.sha512(GETTYSBURG).digest()
        block = label("64 bytes = 512 bits", 28, HASH_COLOR).next_to(machine, RIGHT, buff=0.5)
        self.play(FadeIn(ribbon), FadeIn(machine))
        self.play_at("the next part is the SHA-512 hash", feed_through(machine, ribbon, block))
        tiny = ByteRibbon.text(b"Hi", cell_size=0.34).move_to(LEFT * 4.2 + UP * 2)
        movie = ByteRibbon.text(b"A movie, many gigabytes long", cell_size=0.34, max_cells=12)
        movie.move_to(LEFT * 4.2 + UP * 2)
        self.play_at("no matter whether you feed it a sentence", FadeIn(tiny), run_time=0.4)
        self.play(tiny.animate.scale(0.3).move_to(machine.intake), machine.turn(run_time=0.6), run_time=0.6)
        self.play(FadeOut(tiny), FadeIn(movie), Indicate(block), run_time=0.5)
        self.play(movie.animate.scale(0.3).move_to(machine.intake), machine.turn(run_time=0.6), run_time=0.6)
        self.play(FadeOut(movie), Indicate(block), run_time=0.5)

        bits = bits_of(digest)
        grid = VGroup(*[Square(0.17, stroke_width=0.5, stroke_color=GREY_D, fill_color=GREY_E, fill_opacity=1)
                        for _ in bits]).arrange_in_grid(8, 64, buff=0).move_to(DOWN * 0.6)
        self.play_at("How many base64 characters", FadeIn(grid, lag_ratio=0.002, run_time=1.2))
        tints = [TEAL_E, YELLOW_E]
        self.play_at("Five hundred twelve divided by six",
                     LaggedStart(*[grid[i].animate.set_fill(tints[(i // 6) % 2]) for i in range(510)],
                                 lag_ratio=0.004, run_time=2.5))
        last = grid[510:512]
        spare = VGroup(*[Square(0.17, stroke_width=0.5, fill_color=GREY_D, fill_opacity=1) for _ in range(4)])
        spare.arrange(RIGHT, buff=0).next_to(grid[511], RIGHT, buff=0)
        math_ = label("512 / 6 = 85.33…  →  86 characters", 34, YELLOW).to_edge(DOWN, buff=1.0)
        counter = label("85 full characters + 2 bits left over", 26).next_to(grid, UP, buff=0.25)
        self.play(FadeIn(counter), last.animate.set_fill(RED_E))
        self.play_at("we need eighty-six", FadeIn(spare), Write(math_))

        self.play_at("Eight characters of length",
                     FadeOut(VGroup(grid, spare, counter, math_, machine, block)))
        cid = CIDText(GETTYSBURG_CID, size=30, max_width=FRAME_W - 0.8).move_to(UP * 0.5)
        ruler = VGroup()
        for k in (1, 8, 94):
            ch = cid.chars(k - 1, k)
            tick = Line(UP * 0.12, DOWN * 0.12, color=GREY_B).next_to(ch, DOWN, buff=0.15)
            ruler.add(VGroup(tick, label(str(k), 18, GREY_B).next_to(tick, DOWN, buff=0.08)))
        self.play(FadeIn(cid.prefix, shift=RIGHT), FadeIn(cid.suffix, shift=LEFT))
        self.play(FadeIn(ruler))
        total = label("8 + 86 = 94", 44, YELLOW).move_to(DOWN * 1.8)
        self.play_at("Ninety-four", Write(total))


class S05SmallThings(NarratedScene):
    episode, number = 2, 5

    def construct(self):
        axes = Axes(x_range=[0, 80, 10], y_range=[0, 110, 10], x_length=7.4, y_length=4.8, tips=False,
                    axis_config={"color": GREY_B}).move_to(LEFT * 2.4 + DOWN * 0.4)
        xl = label("content length (bytes)", 22, GREY_B).next_to(axes.x_axis, DOWN, buff=0.45)
        yl = label("characters after the prefix", 22, GREY_B).rotate(PI / 2).next_to(axes.y_axis, LEFT, buff=0.45)
        xt = VGroup(*[label(str(v), 18, GREY_B).next_to(axes.c2p(v, 0), DOWN, buff=0.12) for v in (0, 20, 40, 64, 80)])
        yt = VGroup(*[label(str(v), 18, GREY_B).next_to(axes.c2p(0, v), LEFT, buff=0.12) for v in (0, 43, 86)])

        note = VGroup(Square(1.4, fill_color=YELLOW_D, fill_opacity=1, stroke_width=0),
                      fit_width(label("tiny things:\nuse the\ncontent", 20, BLACK), 1.2))
        note[1].move_to(note[0])
        note.rotate(0.08).to_corner(UR, buff=0.4)
        self.play(FadeIn(note, shift=DOWN))
        self.play_at("Let's plot it", Create(axes), FadeIn(xl), FadeIn(yl), FadeIn(xt), FadeIn(yt))

        flat = axes.plot(lambda x: 86, x_range=[0, 80], color=HASH_COLOR)
        flat_label = label("hash: always 86", 22, HASH_COLOR).next_to(axes.c2p(80, 86), UP, buff=0.12).shift(LEFT * 0.8)
        self.play_at("The hash is a flat line", Create(flat), FadeIn(flat_label))

        points = []
        for n in range(0, 66):
            y = math.ceil(4 * n / 3)
            points += [axes.c2p(n, y), axes.c2p(n + 1, y)]
        stairs = VMobject(color=INLINE_COLOR, stroke_width=4).set_points_as_corners(points[:-2])
        stair_label = label("inline: ⌈4n/3⌉", 22, INLINE_COLOR).next_to(axes.c2p(30, 40), UP + LEFT, buff=0.1)
        self.play_at("Writing the content out directly", Create(stairs, run_time=2.5), FadeIn(stair_label))

        meet = Dot(axes.c2p(64, 86), color=YELLOW, radius=0.09)
        meet_label = label("64 bytes → 86 characters", 22, YELLOW).next_to(meet, DOWN + LEFT, buff=0.15)
        self.play_at("The staircase meets the flat line", FadeIn(meet, scale=2), FadeIn(meet_label))
        over = DashedLine(axes.c2p(65, 86), axes.c2p(65, 87), color=RED)
        over_dot = Dot(axes.c2p(65, 87), color=RED, radius=0.07)
        over_label = label("65 → 87 ✗", 20, RED).next_to(over_dot, RIGHT, buff=0.1)
        self.play_at("One byte more and it would need eighty-seven", FadeIn(over), FadeIn(over_dot), FadeIn(over_label))

        green = Polygon(axes.c2p(0, 0), axes.c2p(64, 0), axes.c2p(64, 110), axes.c2p(0, 110),
                        stroke_width=0, fill_color=INLINE_COLOR, fill_opacity=0.12)
        gold = Polygon(axes.c2p(64, 0), axes.c2p(80, 0), axes.c2p(80, 110), axes.c2p(64, 110),
                       stroke_width=0, fill_color=HASH_COLOR, fill_opacity=0.12)
        self.play_at("At or below it", FadeIn(green))
        self.play_at("Above it, the hash", FadeIn(gold))

        examples = [b"A", b"Hello", b"Hello, world!", b"x" * 64, b"x" * 65]
        names = ['"A"', '"Hello"', '"Hello, world!"', "64 × x  (94 chars)", "65 × x  (94 chars)"]
        rows = VGroup()
        for data, name in zip(examples, names):
            cid = compute_cid(data)
            shown = cid if len(cid) <= 26 else cid[:22] + "…"
            text = mono(shown, 18)
            text[:8].set_color(LENGTH_COLOR)
            text[8:].set_color(INLINE_COLOR if len(data) <= 64 else HASH_COLOR)
            rows.add(VGroup(label(name, 18, GREY_B), text).arrange(DOWN, aligned_edge=LEFT, buff=0.06))
        rows.arrange(DOWN, aligned_edge=LEFT, buff=0.22).to_edge(RIGHT, buff=0.3).shift(DOWN * 0.6)
        self.play_at("every CID for content of sixty-four bytes or more",
                     FadeOut(note), LaggedStart(*[FadeIn(r) for r in rows], lag_ratio=0.25))

        hello = CIDText(compute_cid(b"Hello"), size=60)
        hello_card = VGroup(hello, label('is the word "Hello"', 30, INLINE_COLOR).next_to(hello, DOWN, buff=0.3))
        self.play_at("Inline names have a lovely property",
                     FadeOut(VGroup(axes, xl, yl, xt, yt, flat, flat_label, stairs, stair_label, meet, meet_label,
                                    over, over_dot, over_label, green, gold, rows)))
        self.play(FadeIn(hello_card[0]))
        self.play_at("It is the word", FadeIn(hello_card[1]))

        self.play_at("Here's a pleasing coincidence", hello_card.animate.scale(0.6).to_edge(UP))
        left = VGroup(label("contents of 0–64 bytes", 28, INLINE_COLOR),
                      label("(256⁶⁵ − 1)/255 ≈ 1.004 × 2⁵¹²", 30)).arrange(DOWN, buff=0.25)
        right = VGroup(label("SHA-512 outputs", 28, HASH_COLOR), label("2⁵¹²", 30)).arrange(DOWN, buff=0.25)
        both = VGroup(left, right).arrange(RIGHT, buff=1.5).move_to(DOWN * 0.3)
        self.play(FadeIn(left))
        self.play_at("That's the same as the number", FadeIn(right))
        fill = caption("both fill exactly 86 characters", 32, YELLOW)
        self.play_at("filling up the same eighty-six characters", FadeIn(fill))


class S06ReadingAnyCID(NarratedScene):
    episode, number = 2, 6

    def construct(self):
        step1 = tag_box("first 8 characters → length L", LENGTH_COLOR, 26).move_to(UP * 2.4)
        diamond = VGroup(Square(1.5, color=WHITE).rotate(PI / 4), label("L ≤ 64?", 28)).move_to(UP * 0.4)
        yes = tag_box("the rest IS the content\ndecode it — no network", INLINE_COLOR, 24)
        no = tag_box("the rest is a SHA-512 fingerprint\ngo find bytes that match", HASH_COLOR, 24)
        yes.move_to(LEFT * 3.6 + DOWN * 1.6)
        no.move_to(RIGHT * 3.6 + DOWN * 1.6)
        a1 = Arrow(step1.get_bottom(), diamond.get_top(), buff=0.1, color=GREY_B)
        a2 = Arrow(diamond.get_left(), yes.get_top(), buff=0.1, color=INLINE_COLOR)
        a3 = Arrow(diamond.get_right(), no.get_top(), buff=0.1, color=HASH_COLOR)
        y_lbl = label("yes", 22, INLINE_COLOR).next_to(a2, UP, buff=0.05)
        n_lbl = label("no", 22, HASH_COLOR).next_to(a3, UP, buff=0.05)
        self.play_at("Decode the first eight characters", FadeIn(step1))
        self.play_at("If it's sixty-four or less", GrowArrow(a1), FadeIn(diamond))
        self.play_at("everything after that is the content", GrowArrow(a2), FadeIn(y_lbl), FadeIn(yes))
        self.play_at("If it's more than sixty-four", GrowArrow(a3), FadeIn(n_lbl), FadeIn(no))

        answers = ['the letter "A"', '"Hello"', "Gettysburg Address (1,511 bytes)"]
        branches = [yes, yes, no]
        cues = ["The first is one byte", "The second is five bytes", "The third is fifteen hundred"]
        for cid, answer, branch, phrase in zip(OPENING_CIDS, answers, branches, cues):
            token = CIDText(cid, size=24, max_width=FRAME_W - 1).next_to(step1, UP, buff=0.15)
            self.play_at(phrase, FadeIn(token, shift=DOWN * 0.2), run_time=0.4)
            self.play(token.animate.scale(0.8).move_to(diamond), run_time=0.5)
            result = label(answer, 24, branch[0].get_stroke_color()).next_to(branch, DOWN, buff=0.25)
            self.play(token.animate.move_to(branch).set_opacity(0), FadeIn(result), run_time=0.6)
            self.remove(token)
            self.wait(0.3)
            self.play(FadeOut(result), run_time=0.3)

        empty = CIDText("AAAAAAAA", size=36).next_to(step1, UP, buff=0.15)
        self.play_at("And even the empty file has a name", FadeIn(empty))
        self.play(empty.animate.move_to(yes.get_bottom() + DOWN * 0.5))
        note = caption("L = 0: nothing after the prefix. The empty file has a name, too.", 26, INLINE_COLOR)
        self.play(FadeIn(note))


class S07WholeRecipe(NarratedScene):
    episode, number = 2, 7

    def construct(self):
        source = (
            "def compute_cid(content: bytes) -> str:\n"
            "    prefix = b64url(len(content).to_bytes(6, \"big\"))\n"
            "    if len(content) <= 64:\n"
            "        suffix = b64url(content)\n"
            "    else:\n"
            "        suffix = b64url(sha512(content))\n"
            "    return prefix + suffix\n"
        )
        code = Code(code_string=source, language="python", formatter_style="monokai",
                    add_line_numbers=False, background="rectangle",
                    paragraph_config={"font": MONO, "font_size": 26})
        code.move_to(UP * 0.3)
        self.play(FadeIn(code, shift=UP * 0.2))
        lines = code.code_lines
        highlights = [("Encode the length as six bytes", 1, LENGTH_COLOR),
                      ("If the content is sixty-four bytes or less", 2, INLINE_COLOR),
                      ("encode the content", 3, INLINE_COLOR),
                      ("Otherwise, encode its SHA-512 hash", 5, HASH_COLOR),
                      ("Stick them together", 6, YELLOW)]
        box = None
        for phrase, index, color in highlights:
            new = SurroundingRectangle(lines[index], buff=0.06, color=color, stroke_width=3)
            if box is None:
                box = new
                self.play_at(phrase, Create(box))
            else:
                self.play_at(phrase, Transform(box, new))

        tiles = VGroup()
        for name in IMPLEMENTATIONS:
            frame = RoundedRectangle(corner_radius=0.08, width=1.25, height=0.5, stroke_color=GREY_B,
                                     fill_color=GREY_E, fill_opacity=1)
            tiles.add(VGroup(frame, fit_width(label(name, 16), 1.1).move_to(frame)))
        tiles.arrange_in_grid(cols=9, buff=0.08).move_to(DOWN * 0.2)
        count = label(f"{len(IMPLEMENTATIONS)} implementations, one answer", 30, YELLOW).to_edge(UP, buff=0.4)
        self.play_at("it's been implemented in more than forty",
                     FadeOut(box), ReplacementTransform(code, tiles[IMPLEMENTATIONS.index("python")]),
                     LaggedStart(*[FadeIn(t) for t in tiles], lag_ratio=0.02, run_time=2))
        self.play(FadeIn(count))
        target = CIDText(GETTYSBURG_CID, size=24, max_width=FRAME_W - 1).to_edge(DOWN, buff=0.4)
        self.play_at("they all agree, byte for byte", FadeIn(target))
        outlines = VGroup(*[t[0].copy().set_fill(opacity=0).set_stroke(OK_COLOR, width=3) for t in tiles])
        self.play(LaggedStart(*[Create(o) for o in outlines], lag_ratio=0.02, run_time=1.5))


class S08Recap(NarratedScene):
    episode, number = 2, 8

    def construct(self):
        rows = VGroup(
            VGroup(tag_box("8 chars", LENGTH_COLOR, 28),
                   label("length · 48 bits → up to 2⁴⁸ bytes = 256 TiB", 28)),
            VGroup(VGroup(tag_box("≤ 86 chars", INLINE_COLOR, 28), tag_box("86 chars", HASH_COLOR, 28))
                   .arrange(DOWN, buff=0.1),
                   VGroup(label("the content itself (≤ 64 bytes)", 28, INLINE_COLOR),
                          label("SHA-512 of the content (> 64 bytes)", 28, HASH_COLOR)).arrange(DOWN, buff=0.25)),
            VGroup(tag_box("≤ 94 total", WHITE, 28), label("base64url, no padding", 28)),
        )
        for r in rows:
            r.arrange(RIGHT, buff=0.5)
        rows.arrange(DOWN, aligned_edge=LEFT, buff=0.6).move_to(UP * 0.3)
        self.play_at("eight characters of length", FadeIn(rows[0], shift=RIGHT))
        self.play_at("Then up to eighty-six characters", FadeIn(rows[1], shift=RIGHT))
        self.play_at("Ninety-four characters at most", FadeIn(rows[2], shift=RIGHT))
        fp = FingerprintIcon(size=1.6).move_to(DOWN * 2.6)
        self.play_at("The only part of this we've treated as a black box",
                     Indicate(rows[1][0][1], color=HASH_COLOR), FadeIn(fp, shift=UP))
        self.play_at("Next time, we'll open it up", fp.animate.scale(1.6).move_to(ORIGIN),
                     FadeOut(rows))
