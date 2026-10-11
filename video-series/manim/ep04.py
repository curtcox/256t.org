"""Episode 4 — Accidental Collisions.

Scenes follow video-series/ep04-accidental-collisions.md. Render with:
    python3 video-series/manim/render.py 4
"""

from common import *

PEOPLE = 8e9
FILES_PER_SECOND = 1e9
SECONDS = 13.8e9 * 365.25 * 24 * 3600
TOTAL_FILES = PEOPLE * FILES_PER_SECOND * SECONDS          # ≈ 3.5e36
LOG2_FILES = math.log2(TOTAL_FILES)                          # ≈ 121.4
LOG2_ATOMS = math.log2(1e80)                                 # ≈ 265.8


def birthdays_with_a_match(n: int = 23) -> list[int]:
    """A reproducible set of n birthdays that contains exactly one shared day."""
    seed = 0
    while True:
        days = list(np.random.default_rng(seed).integers(0, 365, n))
        if len(set(days)) == n - 1:
            return days
        seed += 1


def log_bar(exponent: float, width_per_unit: float, color) -> Rectangle:
    return Rectangle(width=max(exponent * width_per_unit, 0.02), height=0.5, stroke_width=0,
                     fill_color=color, fill_opacity=0.85)


class S01FourKinds(NarratedScene):
    episode, number = 4, 1

    def construct(self):
        word = label('"collision"', 64, YELLOW)
        recap = label("collisions in SHA-512: guaranteed to exist", 34).move_to(UP * 0.5)
        worry = label("…but should you ever worry about them?", 34, YELLOW).next_to(recap, DOWN, buff=0.4)
        self.play(FadeIn(recap))
        self.play_at("whether you should ever worry", FadeIn(worry, shift=UP * 0.1))
        self.play_at("notice that the word", FadeOut(VGroup(recap, worry)), FadeIn(word))
        self.play_at("is doing too much work", word.animate.scale(1.25).set_color(RED), rate_func=there_and_back)
        grid = TaxonomyGrid().scale(0.85)
        self.play_at("The 256t spec splits it up", FadeOut(word))
        for name, phrase in [("accidental", "A collision can be accidental"), ("adversarial", "or adversarial"),
                             ("existing", "a collision can be merely existing"), ("problem", "or it can be a problem")]:
            self.play_at(phrase, FadeIn(grid.cards[name], scale=0.9), run_time=0.6)
            if name == "existing":
                self.play_at("have actually produced the same", Indicate(grid.cards[name][1][0], color=YELLOW, scale_factor=1.4))
            if name == "problem":
                self.play_at("got the wrong content back", Indicate(grid.cards[name][1][0], color=RED, scale_factor=1.4))
        self.play_at("Those are really two different questions", GrowArrow(grid.h_axis), FadeIn(grid.h_label))
        self.play_at("And did it actually hurt anyone", GrowArrow(grid.v_axis), FadeIn(grid.v_label))
        focus = SurroundingRectangle(grid.cards["accidental"], color=YELLOW, buff=0.1)
        tag = label("this episode", 24, YELLOW).next_to(focus, DOWN, buff=0.15)
        self.play_at("This episode is about the accidental kind",
                     Create(focus), FadeIn(tag), grid.cards["adversarial"].animate.set_opacity(0.3))
        dice = grid.cards["accidental"][1][0]
        self.play_at("if the whole world keeps making files", Rotate(dice, angle=2 * TAU, run_time=1.5))
        self.play_at("land on the same name by chance", Indicate(grid.cards["existing"], color=YELLOW))


class S02ShrinkTheBattlefield(NarratedScene):
    episode, number = 4, 2

    def construct(self):
        labels = ["0", "1", "2", "…", "64", "65", "…", "1,000", "1,001", "…", "1,511", "…", "2⁴⁸−1"]
        bins = VGroup()
        for text in labels:
            if text == "…":
                bins.add(label("…", 30, GREY_B))
                continue
            n = {"2⁴⁸−1": 2 ** 48 - 1}.get(text, None)
            n = n if n is not None else int(text.replace(",", ""))
            color = INLINE_COLOR if n <= 64 else HASH_COLOR
            box = Rectangle(width=0.9, height=1.6, color=color, fill_color=color, fill_opacity=0.15)
            bins.add(VGroup(box, label(text, 18, color).next_to(box, DOWN, buff=0.1)))
        bins.arrange(RIGHT, buff=0.12).move_to(UP * 0.4)
        fit_width(bins, FRAME_W - 0.6)
        heading = label("one bin per content length", 28, GREY_B).to_edge(UP, buff=0.5)
        self.play_at("let's shrink the battlefield", FadeIn(heading), LaggedStart(*[FadeIn(b) for b in bins],
                                                                                    lag_ratio=0.05))
        inline_bins = VGroup(*bins[:5])
        free = label("inline: no collisions possible", 24, INLINE_COLOR).next_to(inline_bins, UP, buff=0.3)
        self.play_at("anything sixty-four bytes or smaller", FadeIn(free), Indicate(inline_bins, color=INLINE_COLOR))
        smalls = VGroup()
        for word in (b"Hi", b"Ho"):
            smalls.add(VGroup(mono(f'"{word.decode()}"', 24, GREY_A), mono(compute_cid(word), 24, INLINE_COLOR)).arrange(RIGHT, buff=0.3))
        smalls.arrange(DOWN, buff=0.2, aligned_edge=LEFT).move_to(LEFT * 3.4 + DOWN * 2.0)
        differ = label("≠", 40, INLINE_COLOR).next_to(smalls, RIGHT, buff=0.3)
        self.play_at("two different small files can never share", FadeIn(smalls, lag_ratio=0.3), FadeIn(differ))
        self.play_at("That whole region is collision-free",
                     *[b[0].animate.set_fill(INLINE_COLOR, opacity=0.4) for b in inline_bins if len(b) == 2])
        self.play_at("the length is part of the name", FadeOut(VGroup(smalls, differ)),
                     LaggedStart(*[Indicate(b[1], color=LENGTH_COLOR, scale_factor=1.3) for b in bins if len(b) == 2],
                                 lag_ratio=0.05))

        b1000, b1001 = bins[7][0], bins[8][0]
        wall = Line(b1000.get_corner(UR) + RIGHT * 0.06, b1000.get_corner(DR) + RIGHT * 0.06,
                    color=LENGTH_COLOR, stroke_width=8)
        spark = Star(5, outer_radius=0.18, color=RED, fill_opacity=1).move_to(b1000)
        self.play_at("two files of different lengths can never collide", FadeIn(spark), Create(wall))
        self.play(spark.animate.move_to(wall.get_center() + LEFT * 0.1), run_time=0.5)
        self.play(spark.animate.move_to(b1000.get_center()), run_time=0.5, rate_func=rush_from)
        wall_label = label("different length prefix", 20, LENGTH_COLOR).next_to(wall, UP, buff=0.9)
        self.play(FadeIn(wall_label), FadeOut(spark))
        rule = caption("a collision needs the same length AND the same SHA-512", 30, YELLOW)
        self.play_at("A collision only counts if", FadeIn(rule))
        self.play_at("exactly the same length", Indicate(wall_label, color=LENGTH_COLOR))
        self.play_at("exactly the same SHA-512 hash", Indicate(rule, color=YELLOW, scale_factor=1.05))
        hashed = [b for b in bins[5:] if len(b) == 2]
        self.play_at("the real question lives inside one of these bins",
                     LaggedStart(*[Indicate(b, color=HASH_COLOR) for b in hashed], lag_ratio=0.2))

        target = bins[10][0]
        self.play_at("Pick a length", Indicate(bins[10], color=YELLOW))
        rng = np.random.default_rng(5)
        stars = VGroup(*[Dot(np.array([rng.uniform(-6.5, 6.5), rng.uniform(-3.2, 3.2), 0]),
                             radius=rng.uniform(0.01, 0.035), color=GREY_A) for _ in range(500)])
        space = VGroup(label("inside one bin:", 28, GREY_B), pow2(512, size=60, color=HASH_COLOR),
                       label("possible fingerprints", 28, GREY_B)).arrange(RIGHT, buff=0.3)
        self.play_at("Inside that bin", FadeOut(VGroup(bins, heading, free, wall, wall_label, rule)),
                     FadeIn(stars, lag_ratio=0.002), FadeIn(space))
        self.play_at("If we drop files in at random", space.animate.to_edge(UP, buff=0.4))
        drops = VGroup(*[Dot(np.array([rng.uniform(-5, 5), rng.uniform(-2.5, 2), 0]), radius=0.08, color=YELLOW)
                         for _ in range(12)])
        self.play(LaggedStart(*[FadeIn(d, scale=3) for d in drops], lag_ratio=0.2, run_time=2))


class S03BirthdayParadox(NarratedScene):
    episode, number = 4, 3

    def construct(self):
        center = LEFT * 3.2 + DOWN * 0.2
        radius = 2.6
        ring = Circle(radius, color=GREY_D).move_to(center)
        ticks = VGroup(*[Line(ORIGIN, RIGHT * (0.12 if d % 30 else 0.25), color=GREY_C, stroke_width=1)
                         .rotate(TAU * d / 365).shift(center + radius * np.array([math.cos(TAU * d / 365),
                                                                                    math.sin(TAU * d / 365), 0]))
                         for d in range(0, 365, 5)])
        title = label("365 days", 24, GREY_B).move_to(center)
        self.play_at("This is the birthday problem", Create(ring), FadeIn(ticks), FadeIn(title))
        days = birthdays_with_a_match()

        def spot(day: int) -> np.ndarray:
            a = TAU * day / 365
            return center + (radius - 0.25) * np.array([math.cos(a), math.sin(a), 0])

        dots = VGroup(*[Dot(spot(d), radius=0.08, color=YELLOW) for d in days])
        people = label("people: 0", 30).move_to(RIGHT * 3.4 + UP * 2.6)
        pairs = label("pairs: 0", 30).next_to(people, DOWN, buff=0.3, aligned_edge=LEFT)
        self.play_at("How many people do you need", FadeIn(people), FadeIn(pairs))
        lines = VGroup()
        for k, dot in enumerate(dots):
            new_lines = VGroup(*[Line(dot.get_center(), dots[j].get_center(), stroke_width=1, stroke_opacity=0.4,
                                      color=BLUE_B) for j in range(k)])
            lines.add(*new_lines)
            anims = [FadeIn(dot, scale=2),
                     Transform(people, label(f"people: {k + 1}", 30).move_to(people, aligned_edge=LEFT)),
                     Transform(pairs, label(f"pairs: {(k + 1) * k // 2}", 30).move_to(pairs, aligned_edge=LEFT))]
            if len(new_lines):
                anims.append(Create(new_lines))
            self.play(*anims, run_time=0.18)
        twins = [i for i, d in enumerate(days) if days.count(d) > 1]
        self.play_at("The intuition that fails is thinking about you",
                     Indicate(dots[0], color=WHITE, scale_factor=2))
        yours = VGroup(*[Line(dots[0].get_center(), d.get_center(), stroke_width=2.5, color=WHITE) for d in dots[1:]])
        your_count = label("pairs with you: 22", 26, WHITE).next_to(pairs, DOWN, buff=0.3, aligned_edge=LEFT)
        self.play_at("someone shares your birthday", Create(yours, lag_ratio=0.05), FadeIn(your_count))
        self.play_at("doesn't have to involve you", FadeOut(yours), FadeOut(your_count))
        self.play_at("It can be any pair of people", *[dots[i].animate.set_color(RED).scale(1.8) for i in twins])

        work = VGroup(
            label("23 × 22 / 2 = 253 pairs", 30),
            label("each pair matches with probability 1/365", 26, GREY_A),
            label("expected matches ≈ 253 / 365 ≈ 0.69", 30),
            label("P(at least one) ≈ 1 − e^(−253/365) ≈ 50%", 30, YELLOW),
            label("(exactly: 50.7%)", 22, GREY_B),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25).move_to(RIGHT * 3.3 + DOWN * 0.6)
        fit_width(work, 6.3)
        for line, phrase in zip(work, ["With twenty-three people", "Each pair has a one-in",
                                       "So you expect about", "the chance of at least one match", None]):
            if phrase:
                self.cue(phrase)
            self.play(FadeIn(line, shift=LEFT * 0.2), run_time=0.5)
            if line is work[2]:
                self.play_at("around zero point seven", Indicate(line, color=YELLOW))

        self.play_at("Now let's make that general",
                     FadeOut(VGroup(ring, ticks, title, dots, lines, people, pairs, work)))
        general = VGroup(
            label("N possible values,  n items", 34),
            label("pairs ≈ n² / 2", 34),
            label("each pair matches with probability 1/N", 30, GREY_A),
        ).arrange(DOWN, buff=0.35).move_to(UP * 1.4)
        formula = label("P(collision) ≈ n² / (2N)", 48, YELLOW)
        box = SurroundingRectangle(formula, color=YELLOW, buff=0.25)
        result = VGroup(formula, box).move_to(DOWN * 1.0)
        self.play_at("say there are N possible values", FadeIn(general[0]))
        self.play_at("there are about n squared over two pairs", FadeIn(general[1]))
        self.play_at("Each pair matches with probability", FadeIn(general[2]))
        self.play_at("the chance of any collision at all", Write(formula), Create(box))
        sq = label("likely once n ≈ √N        (√365 ≈ 19, vs. 23)", 30).next_to(result, DOWN, buff=0.5)
        self.play_at("The key thing to notice is that n is squared", Indicate(formula, color=YELLOW))
        self.play_at("about the square root of N", FadeIn(sq))
        link = label("√(2⁵¹²) = 2²⁵⁶  — the collision cost from Episode 3", 28, HASH_COLOR).to_edge(DOWN, buff=0.4)
        self.play_at("That square root is exactly why", FadeIn(link))
        drop = VGroup(pow2(512, size=48, color=GREY_A), Arrow(LEFT * 0.8, RIGHT * 0.8, color=YELLOW), pow2(256, size=48, color=HASH_COLOR))
        drop.arrange(RIGHT, buff=0.4).move_to(general)
        self.play_at("dropped from two to the five hundred twelve", FadeOut(general), FadeIn(drop[0]))
        self.play_at("to two to the two hundred fifty-six", GrowArrow(drop[1]), FadeIn(drop[2], shift=LEFT * 0.2))
        self.play_at("the same birthday effect", Indicate(formula, color=YELLOW), Circumscribe(box, color=YELLOW))


class S04ScalingUp(NarratedScene):
    episode, number = 4, 4

    def construct(self):
        formula = label("P ≈ n² / (2N)", 48, YELLOW).to_edge(UP, buff=0.4)
        self.play_at("Now plug in SHA-512", Write(formula))
        n_is = label("N = 2⁵¹²", 40, HASH_COLOR).next_to(formula, DOWN, buff=0.4)
        self.play_at("N is two to the five hundred twelve", FadeIn(n_is, shift=UP * 0.2))
        self.play_at("any accidental collision among n files", Circumscribe(formula, color=HASH_COLOR))
        self.play_at("over two to the five hundred thirteen",
                     Transform(formula, label("P ≈ n² / 2⁵¹³", 48, YELLOW).move_to(formula)), FadeOut(n_is))

        # The exponent ladder: probability exponents from -520 to 0.
        ladder = NumberLine(x_range=[-520, 0, 40], length=6.0, rotation=PI / 2, color=GREY_B,
                            include_numbers=False).move_to(RIGHT * 5 + DOWN * 0.6)
        ladder_labels = VGroup(*[label(power_text(v) if v else "1", 18, GREY_B).next_to(ladder.n2p(v), RIGHT, buff=0.1)
                                 for v in (-520, -400, -280, -160, -40, 0)])
        ladder_title = label("probability", 20, GREY_B).next_to(ladder, UP, buff=0.15)
        self.play_at("arithmetic becomes counting", Create(ladder), FadeIn(ladder_labels), FadeIn(ladder_title))
        rule = label("double n → P × 4 → two rungs up", 26, GREY_A).next_to(formula, DOWN, buff=0.3)
        self.play_at("Every time you double the number of files", FadeIn(rule))

        rows = [(40, "a trillion"), (64, "18 quintillion"), (100, "10³⁰"), (128, "3 × 10³⁸"), (256, "10⁷⁷")]
        header = VGroup(label("files (n)", 24, GREY_B), label("roughly", 24, GREY_B),
                        label("P(any accidental collision)", 24, GREY_B))
        table = VGroup(header)
        for exp, words in rows:
            p = 2 * exp - 513
            ptext = power_text(p) if p < -1 else "~ 1/2"
            table.add(VGroup(label(power_text(exp), 30), label(words, 26, GREY_A),
                             label(ptext, 30, RED if p >= -1 else YELLOW)))
        for k, row in enumerate(table):
            y = 1.2 - 0.62 * k
            for cell, x in zip(row, (-6.0, -3.6, -0.6)):
                cell.move_to(np.array([x, y, 0]), aligned_edge=LEFT)
        self.play(FadeIn(header))
        markers = VGroup()
        cues = ["A trillion files", "Eighteen quintillion files", None, "Go all the way up to two to the one hundred twenty-eight",
                "You don't get to a coin flip"]
        aside = VGroup()
        for row, (exp, _), phrase in zip(table[1:], rows, cues):
            if phrase:
                self.cue(phrase)
            p = 2 * exp - 513
            mark = Dot(ladder.n2p(p), color=RED if p >= -1 else YELLOW, radius=0.09)
            markers.add(mark)
            self.play(FadeIn(row, shift=UP * 0.15), FadeIn(mark, scale=2), *([FadeOut(aside)] if len(aside) else []),
                      run_time=0.6)
            aside = VGroup()
            if exp == 40:
                aside = VGroup(label("(2⁴⁰)² = 2⁸⁰", 30), label("2⁸⁰ / 2⁵¹³ = 2⁻⁴³³", 30, YELLOW)).arrange(RIGHT, buff=0.8)
                aside.move_to(LEFT * 2.6 + DOWN * 3.0)
                self.play_at("Square it", FadeIn(aside[0], shift=UP * 0.1))
                self.play_at("Divide by two to the five hundred thirteen", FadeIn(aside[1], shift=UP * 0.1))
                self.play_at("one in two to the four hundred thirty-three", Indicate(row[2], color=YELLOW), Flash(mark, color=YELLOW))
            if exp == 128:
                aside = VGroup(label("2¹²⁸ =", 24, GREY_A), mono(f"{2 ** 128:,}", 22, GREY_A)).arrange(RIGHT, buff=0.2)
                fit_width(aside, 8.0).move_to(LEFT * 2.4 + DOWN * 3.0)
                self.play_at("a number with thirty-nine digits", FadeIn(aside, shift=UP * 0.1))
                self.play_at("still one in two to the two hundred fifty-seven", Indicate(row[2], color=YELLOW),
                             Flash(mark, color=YELLOW))
        note = label("this many files is not a thing that happens", 22, RED).next_to(table[-1], DOWN, buff=0.2)
        self.play(FadeIn(note))
        self.play_at("These exponents are so lopsided", LaggedStart(*[Indicate(m, scale_factor=1.8) for m in markers], lag_ratio=0.2),
                     Indicate(ladder_labels, color=YELLOW))
        self.play_at("So let's try to make one concrete", Indicate(table[1:4], color=YELLOW, scale_factor=1.02))


class S05GenerousCivilization(NarratedScene):
    episode, number = 4, 5

    def construct(self):
        globe = VGroup(Circle(1.3, color=BLUE_D, fill_color=BLUE_E, fill_opacity=0.6),
                       *[Ellipse(width=2.6 * math.cos(a), height=2.6, color=BLUE_B, stroke_width=1)
                         for a in (0.3, 0.9, 1.3)],
                       *[Line(LEFT * 1.3 * math.cos(b), RIGHT * 1.3 * math.cos(b), color=BLUE_B, stroke_width=1)
                         .shift(UP * 1.3 * math.sin(b)) for b in (-0.6, 0, 0.6)])
        globe.move_to(LEFT * 4 + UP * 0.8)
        people = label("8 billion people", 26).next_to(globe, DOWN, buff=0.3)
        rng = np.random.default_rng(2)
        swarm = VGroup(*[Dot(globe.get_center() + 1.6 * np.array([math.cos(a), math.sin(a), 0]) * rng.uniform(1, 1.4),
                             radius=0.04, color=TEAL) for a in rng.uniform(0, TAU, 60)])
        self.play_at("Imagine every one of the eight billion", FadeIn(globe), FadeIn(people))
        rate = label("× a billion files per second, each", 26, TEAL).next_to(people, DOWN, buff=0.2)
        self.play_at("producing a billion new files every second", FadeIn(swarm, lag_ratio=0.02), FadeIn(rate))
        self.play(*[d.animate.shift(2 * (d.get_center() - globe.get_center()) / 1.6).set_opacity(0) for d in swarm],
                  run_time=1.2)

        timeline = NumberLine(x_range=[0, 13.8, 1], length=6.5, color=GREY_B, include_numbers=False)
        timeline.move_to(RIGHT * 2.7 + UP * 1.8)
        bang = label("Big Bang", 20, YELLOW).next_to(timeline.n2p(0), DOWN, buff=0.15)
        now = label("today", 20).next_to(timeline.n2p(13.8), DOWN, buff=0.15)
        span = label("13.8 billion years", 24, GREY_A).next_to(timeline, UP, buff=0.15)
        self.play_at("since the Big Bang", Create(timeline), FadeIn(bang), FadeIn(now), FadeIn(span))

        math_lines = VGroup(
            label("8 × 10⁹ × 10⁹ / s × 4.35 × 10¹⁷ s", 30),
            label(f"≈ 3.5 × 10³⁶ ≈ 2¹²¹ files", 32, YELLOW),
            label("(2¹²¹)² = 2²⁴²", 30),
            label("P ≈ 2²⁴² / 2⁵¹³", 30),
            label("= 2⁻²⁷¹", 44, YELLOW),
        ).arrange(DOWN, buff=0.25, aligned_edge=LEFT).move_to(RIGHT * 2.7 + DOWN * 1.0)
        pair_a = Dot(globe.get_center() + LEFT * 0.6 + UP * 0.4, color=YELLOW, radius=0.08)
        pair_b = Dot(globe.get_center() + RIGHT * 0.5 + DOWN * 0.5, color=YELLOW, radius=0.08)
        pair_line = DashedLine(pair_a.get_center(), pair_b.get_center(), color=YELLOW)
        pair_q = label("same hash?", 20, YELLOW).next_to(globe, UP, buff=0.15)
        for line, phrase in zip(math_lines, ["That's eight billion, times a billion", "Roughly three and a half",
                                             "Square that", "Divide by two to the five hundred thirteen",
                                             "is about one in two to the two hundred seventy-one"]):
            if line is math_lines[4]:
                self.play_at("any two of those files", FadeIn(pair_a, scale=2), FadeIn(pair_b, scale=2))
                self.play_at("accidentally share a SHA-512 hash", Create(pair_line), FadeIn(pair_q))
            self.play_at(phrase, FadeIn(line, shift=UP * 0.15), run_time=0.6)

        # Pick one atom out of the observable universe.
        self.play_at("For comparison", *[FadeOut(m) for m in self.mobjects])
        web = VGroup(*[Dot(np.array([rng.uniform(-6.8, 6.8), rng.uniform(-3.6, 3.6), 0]), radius=0.02,
                           color=GREY_B) for _ in range(900)])
        atoms = label("atoms in the observable universe ≈ 10⁸⁰ ≈ 2²⁶⁶", 30).to_edge(UP, buff=0.3)
        bg = BackgroundRectangle(atoms, fill_opacity=0.8, buff=0.15)
        self.play(FadeIn(web, lag_ratio=0.001), FadeIn(bg), FadeIn(atoms))
        self.play_at("about two to the two hundred sixty-six", Indicate(atoms, color=YELLOW, scale_factor=1.05))
        chosen = web[417]
        ring = Circle(0.2, color=YELLOW).move_to(chosen)
        self.play_at("picture one particular atom", chosen.animate.set_color(YELLOW).scale(3), Create(ring))
        pointer = Arrow(DOWN * 3.5 + LEFT * 6, web[88].get_center(), color=WHITE, buff=0.05)
        self.play_at("Now pick an atom at random", GrowArrow(pointer))
        compare = VGroup(
            VGroup(label("P(that exact atom)", 28), label("≈ 2⁻²⁶⁶", 32, YELLOW)).arrange(RIGHT, buff=0.3),
            VGroup(label("P(any accidental collision, ever)", 28), label("≈ 2⁻²⁷¹", 32, RED)).arrange(RIGHT, buff=0.3),
        ).arrange(DOWN, buff=0.25, aligned_edge=RIGHT)
        compare_bg = BackgroundRectangle(compare, fill_opacity=0.9, buff=0.3)
        VGroup(compare_bg, compare).to_edge(DOWN, buff=0.4)
        self.play_at("Your chance of picking that exact one", FadeIn(compare_bg), FadeIn(compare[0]))
        factor = label("about 30× less likely", 26, RED).next_to(compare_bg, UP, buff=0.15)
        factor.add_background_rectangle(opacity=0.9, buff=0.1)
        self.play_at("by a factor of about thirty", FadeIn(factor))
        self.play_at("than the chance of a single accidental collision", FadeIn(compare[1], shift=UP * 0.1))
        foot = label("…and that lumps every file into one bin; equal lengths only make it smaller", 20, GREY_B)
        foot_bg = BackgroundRectangle(foot, fill_opacity=0.9)
        VGroup(foot_bg, foot).next_to(atoms, DOWN, buff=0.2)
        self.play_at("And that's with an unfair simplification", FadeIn(foot_bg), FadeIn(foot))
        one_bin = VGroup(Rectangle(width=4.0, height=1.4, color=GREY_B, fill_color=BLACK, fill_opacity=0.92),
                         label("every file, one bin", 26, GREY_A))
        one_bin[1].move_to(one_bin[0])
        one_bin.move_to(UP * 0.6)
        self.play_at("We lumped every file together in one bin", FadeIn(one_bin))
        bins = VGroup(*[VGroup(Rectangle(width=1.1, height=1.4, color=HASH_COLOR, fill_color=BLACK, fill_opacity=0.92),
                               label(n, 18, HASH_COLOR)) for n in ("65 B", "66 B", "1,511 B", "1 MB", "…")])
        for b in bins:
            b[1].move_to(b[0])
        bins.arrange(RIGHT, buff=0.15).move_to(one_bin)
        self.play_at("exactly the same length", ReplacementTransform(one_bin, bins))


class S06WhatActuallyGoesWrong(NarratedScene):
    episode, number = 4, 6

    def construct(self):
        title = label("things that can make a CID check give the wrong answer", 30).to_edge(UP, buff=0.4)
        rows = ["software bugs", "disk and network corruption", "memory bit flips (e.g. cosmic rays)",
                "accidental SHA-512 collision"]
        heights = [5.2, 4.4, 3.6, 0.01]
        names = VGroup(*[label(r, 24) for r in rows]).arrange(DOWN, aligned_edge=RIGHT, buff=0.7)
        names.move_to(LEFT * 3.3 + DOWN * 0.3)
        bars = VGroup(*[Rectangle(width=h, height=0.45, stroke_width=0, fill_color=c, fill_opacity=0.85)
                        .next_to(n, RIGHT, buff=0.3) for n, h, c in zip(names, heights, [TEAL, TEAL, TEAL, RED])])
        for bar, name in zip(bars, names):
            bar.align_to(names, LEFT).shift(RIGHT * (names.width + 0.3))
        axis = label("how often (qualitative, log scale)", 20, GREY_B).next_to(bars, DOWN, buff=0.4)
        self.play_at("This is the right way to think", FadeIn(title))
        zero = label('"is the risk zero?"', 36, GREY_A).move_to(UP * 1.0)
        nope = Line(zero.get_left(), zero.get_right(), color=RED, stroke_width=4)
        better = label('"how does this risk compare?"', 36, YELLOW).move_to(DOWN * 0.4)
        self.play_at("is the risk zero", FadeIn(zero))
        self.play_at("It isn't", Create(nope))
        self.play_at("how does this risk compare", FadeIn(better, shift=UP * 0.1))
        self.play_at("Every real system has failure modes", FadeOut(VGroup(zero, nope, better)), FadeIn(axis))
        for name, bar, phrase in zip(names, bars, ["Software has bugs", "Disks and networks", "a stray cosmic ray",
                                                   "enormously more likely than an accidental"]):
            self.play_at(phrase, FadeIn(name), GrowFromEdge(bar, LEFT), run_time=0.6)
        pointer = Arrow(bars[3].get_right() + RIGHT * 2.2, bars[3].get_right() + RIGHT * 0.1,
                        color=RED, buff=0.05)
        where = label("it's here (too small to draw)", 22, RED).next_to(pointer, RIGHT, buff=0.15)
        self.play(GrowArrow(pointer), FadeIn(where))
        self.play_at("give you the wrong answer", Indicate(title, color=YELLOW, scale_factor=1.03))
        verdict = caption("spend your worry where the risk is", 32, YELLOW)
        self.play_at("because the machine broke, not because the math did", Indicate(bars[:3], color=YELLOW))
        self.play_at("So any engineering effort", FadeIn(verdict))
        self.play_at("defending against accidental collisions", Indicate(where, color=RED))
        self.play_at("risks that actually happen", Indicate(bars[:3], color=TEAL))


class S07ExistingVersusProblem(NarratedScene):
    episode, number = 4, 7

    def construct(self):
        outer = Circle(2.6, color=WHITE, fill_color=GREY_E, fill_opacity=0.6).move_to(LEFT * 3.5)
        inner = Circle(0.8, color=RED, fill_color=RED, fill_opacity=0.25).move_to(outer.get_center() + DOWN * 1.0)
        outer_label = label("existing collisions", 26).next_to(outer.get_top(), DOWN, buff=0.4)
        inner_label = label("problem\ncollisions", 22, RED).move_to(inner)
        self.play_at("where the spec's distinction", Create(outer), FadeIn(outer_label))
        self.play(Create(inner), FadeIn(inner_label))

        steps = ["two different files share a CID", "both end up where you fetch from",
                 "you ask for one, and get the other"]
        gates = VGroup()
        widths = [3.6, 2.0, 0.8]
        for k, (text, w) in enumerate(zip(steps, widths)):
            gate = VGroup(label(f"{k + 1}. {text}", 24),
                          Rectangle(width=w, height=0.3, stroke_width=0, fill_color=RED, fill_opacity=0.8))
            gate.arrange(DOWN, aligned_edge=LEFT, buff=0.15)
            gates.add(gate)
        gates.arrange(DOWN, aligned_edge=LEFT, buff=0.55).move_to(RIGHT * 3.2)
        first_note = label("(already ~2⁻²⁷¹ by accident)", 20, GREY_B).next_to(gates[0][0], DOWN, buff=0.05)
        gates[0][1].next_to(first_note, DOWN, buff=0.1, aligned_edge=LEFT)
        self.play_at("Suppose, against all those odds", FadeIn(gates[0][0]), FadeIn(first_note),
                     GrowFromEdge(gates[0][1], LEFT))
        twins = VGroup(Dot(color=YELLOW), Dot(color=YELLOW)).arrange(RIGHT, buff=0.7).move_to(outer.get_center() + UP * 0.6)
        tie = Line(twins[0].get_center(), twins[1].get_center(), color=YELLOW)
        shared = label("same CID", 20, YELLOW).next_to(tie, UP, buff=0.12)
        self.play_at("Two files somewhere really do share", FadeIn(twins, scale=2), Create(tie), FadeIn(shared))
        self.play_at("For that to actually hurt anyone", Indicate(inner, color=RED))
        self.play_at("both files would have to end up somewhere", FadeIn(gates[1][0]), GrowFromEdge(gates[1][1], LEFT))
        self.play_at("you would have to ask for one", FadeIn(gates[2][0]), GrowFromEdge(gates[2][1], LEFT))
        self.play_at("Each of those steps makes", Indicate(inner, color=RED), inner.animate.scale(0.5))


class S08TheVerdict(NarratedScene):
    episode, number = 4, 8

    def construct(self):
        assumption = card("assumption", "for inputs nobody designed to break it,\nSHA-512 behaves like the ideal ledger",
                          width=9.5, height=2.4, color=YELLOW, body_size=30).move_to(UP * 2.0)
        self.play_at("All of this rests on one assumption", FadeIn(assumption))
        coin = VGroup(Circle(0.35, color=YELLOW, fill_color=YELLOW_E, fill_opacity=1), label("1/0", 16, BLACK))
        coin.next_to(assumption, RIGHT, buff=0.2)
        self.play_at("the ideal coin-flipping ledger", FadeIn(coin), run_time=0.3)
        self.play(Rotate(coin, angle=4 * PI, axis=UP), run_time=1.0)
        mild = tag_box("a mild assumption", GREEN, 24).move_to(UP * 0.45)
        self.play_at("a mild assumption", FadeIn(mild, shift=UP * 0.1))
        a = ByteRibbon.text(b"Report v1", cell_size=0.32)
        b = ByteRibbon.text(b"Report v2", cell_size=0.32).tint([8], YELLOW)
        ha = mono(sha512_b64(b"Report v1")[:22] + "…", 22, HASH_COLOR)
        hb = mono(sha512_b64(b"Report v2")[:22] + "…", 22, HASH_COLOR)
        pair = VGroup(VGroup(a, ha).arrange(RIGHT, buff=0.5), VGroup(b, hb).arrange(RIGHT, buff=0.5))
        pair.arrange(DOWN, buff=0.3).move_to(DOWN * 0.3)
        self.play_at("Similar files don't produce similar fingerprints", FadeOut(mild), FadeIn(pair))
        self.play_at("like lots of files that start the same way",
                     Indicate(VGroup(a.cells[:8], b.cells[:8]), color=YELLOW, scale_factor=1.05))
        self.play_at("doesn't help collisions along", Indicate(VGroup(ha, hb), color=HASH_COLOR, scale_factor=1.05))
        never = label("collisions of any kind ever found for SHA-512: 0", 28, GREEN).next_to(pair, DOWN, buff=0.4)
        self.play_at("nobody has ever found a collision", FadeIn(never))

        quote = quote_card("I'm comfortable just ignoring accidental problem collisions.", "256t.org")
        self.play_at("So the spec's position is", FadeOut(VGroup(assumption, coin, pair, never)), FadeIn(quote))
        sums = label("the arithmetic: ≈ 2⁻²⁷¹, ever", 28, GREY_A).next_to(quote, UP, buff=0.5)
        self.play_at("Having done the arithmetic", FadeIn(sums, shift=DOWN * 0.1))
        conclusion = label("not a shrug — a conclusion", 32, YELLOW).next_to(quote, DOWN, buff=0.5)
        self.play_at("I think that's not a shrug", FadeIn(conclusion))

        grid = TaxonomyGrid().scale(0.7)
        self.play_at("But we've been assuming the universe", FadeOut(VGroup(quote, conclusion, sums)), FadeIn(grid))
        self.play_at("What if someone is actively trying",
                     grid.cards["accidental"].animate.set_opacity(0.25),
                     grid.cards["adversarial"].animate.scale(1.2),
                     grid.cards["adversarial"][0].animate.set_stroke(ADVERSARY_COLOR, width=6))
        self.play_at("Adversaries don't roll dice", Indicate(grid.cards["adversarial"], color=ADVERSARY_COLOR))
