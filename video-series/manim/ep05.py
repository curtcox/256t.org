"""Episode 5 — Adversarial Collisions: It's Easier to Lie.

Scenes follow video-series/ep05-adversarial-collisions.md. Render with:
    python3 video-series/manim/render.py 5
"""

from common import *

SECONDS_PER_YEAR = 365.25 * 24 * 3600
ATTACK_YEARS = 2.0 ** 186 / SECONDS_PER_YEAR          # ≈ 3.1e48
UNIVERSE_AGES = ATTACK_YEARS / 13.8e9                  # ≈ 2.2e38


def doc_icon(title: str, color=WHITE, lines: int = 4) -> VGroup:
    page = Rectangle(width=1.1, height=1.4, color=color, fill_color=GREY_E, fill_opacity=1)
    rows = VGroup(*[Line(LEFT * 0.35, RIGHT * 0.35, stroke_width=2, color=GREY_B) for _ in range(lines)])
    rows.arrange(DOWN, buff=0.15).move_to(page).shift(DOWN * 0.15)
    head = label(title, 16, color).move_to(page.get_top() + DOWN * 0.2)
    return VGroup(page, rows, head)


class TwinRibbons(VGroup):
    """Two different documents wired to one shared CID tag."""

    def __init__(self, cid_text: str = "AAAAABkQ7r…", **kwargs):
        super().__init__(**kwargs)
        self.benign = doc_icon("contract", GREEN)
        self.malicious = doc_icon("contract", RED)
        VGroup(self.benign, self.malicious).arrange(DOWN, buff=0.6)
        self.tag = tag_box(cid_text, HASH_COLOR, 24).next_to(VGroup(self.benign, self.malicious), RIGHT, buff=1.4)
        self.wires = VGroup(*[Line(d.get_right(), self.tag.get_left(), color=HASH_COLOR, stroke_width=3)
                              for d in (self.benign, self.malicious)])
        self.add(self.benign, self.malicious, self.wires, self.tag)


class S01CheapestAttack(NarratedScene):
    episode, number = 5, 1

    def construct(self):
        grid = TaxonomyGrid().scale(0.7)
        grid.cards["accidental"].set_opacity(0.25)
        self.play(FadeIn(grid))
        self.play_at("An adversary gets to choose their inputs",
                     Indicate(grid.cards["adversarial"], color=ADVERSARY_COLOR))
        self.play_at("They can study the algorithm",
                     grid.cards["adversarial"][0].animate.set_stroke(ADVERSARY_COLOR, width=6))
        quote = quote_card("It is easier to just lie than engineer a collision.", "256t.org")
        self.play_at("has a line about this", FadeOut(grid), FadeIn(quote))
        self.play(quote.animate.to_edge(UP, buff=0.4))
        liar = MaskedFigure("liar").move_to(LEFT * 3.5 + DOWN * 1.2)
        forger = MaskedFigure("forger").move_to(RIGHT * 3.5 + DOWN * 1.2)
        self.play_at("That one sentence contains the whole structure", Indicate(quote, color=YELLOW, scale_factor=1.03))
        slots = VGroup(*[VGroup(DashedVMobject(Circle(0.9, color=GREY_B), num_dashes=30), label("?", 48, GREY_B))
                         for _ in range(2)])
        for slot, fig in zip(slots, (liar, forger)):
            slot.move_to(fig)
        self.play_at("two kinds of adversary", LaggedStart(*[FadeIn(sl, scale=0.8) for sl in slots], lag_ratio=0.4))
        liar_name = label("the Liar", 30, ADVERSARY_COLOR).next_to(liar, UP, buff=0.25)
        forger_name = label("the Forger", 30, ADVERSARY_COLOR).next_to(forger, UP, buff=0.25)
        self.play_at("There's the liar", FadeOut(slots[0]), FadeIn(liar, shift=RIGHT), FadeIn(liar_name))
        liar_does = label("hands you the wrong bytes", 22, GREY_A).next_to(liar, DOWN, buff=0.25)
        self.play(FadeIn(liar_does))
        self.play_at("And there's the forger", FadeOut(slots[1]), FadeIn(forger, shift=LEFT), FadeIn(forger_name))
        forger_does = label("makes wrong bytes with the right name", 22, GREY_A).next_to(forger, DOWN, buff=0.25)
        self.play(FadeIn(forger_does))
        effort = Arrow(LEFT * 1.6, RIGHT * 1.6, color=YELLOW, buff=0).move_to(DOWN * 1.2)
        effort_label = label("more effort", 24, YELLOW).next_to(effort, UP, buff=0.15)
        self.play_at("in order of how much effort", GrowArrow(effort), FadeIn(effort_label))


class S02TheLiar(NarratedScene):
    episode, number = 5, 2

    def construct(self):
        client = VGroup(RoundedRectangle(corner_radius=0.1, width=1.6, height=1.0, color=WHITE),
                        label("you", 24)).move_to(LEFT * 5.5 + UP * 1.6)
        client[1].move_to(client[0])
        rack = ServerRack(color=ADVERSARY_COLOR, height=1.6).move_to(RIGHT * 4.8 + UP * 1.6)
        liar = MaskedFigure("liar").scale(0.6).next_to(rack, DOWN, buff=0.2)
        req = Arrow(client.get_right(), rack.get_left(), buff=0.2, color=GREY_B).shift(UP * 0.25)
        req_label = mono("CID A", 24, HASH_COLOR).next_to(req, UP, buff=0.08)
        self.play(FadeIn(client), FadeIn(rack), FadeIn(liar))
        self.play_at("serve you something other than", GrowArrow(req), FadeIn(req_label))
        fake = ByteRibbon.text(b"malware.exe", cell_size=0.3).tint(range(11), RED)
        fake.next_to(rack, LEFT, buff=0.3).shift(DOWN * 0.3)
        self.play_at("Maybe it's malware instead of the installer", FadeIn(fake))
        self.play(fake.animate.next_to(client, RIGHT, buff=0.3).shift(DOWN * 0.5), run_time=1.2)
        edited = ByteRibbon.text(b"edited.pdf", cell_size=0.3).tint(range(10), RED).move_to(fake, aligned_edge=LEFT)
        self.play_at("an edited version of a document", Transform(fake, edited))
        free = tag_box("cost to the liar: $0", RED, 22).next_to(liar, LEFT, buff=0.4)
        self.play_at("It costs them nothing", FadeIn(free, shift=LEFT * 0.2))
        self.play_at("it works every time", Indicate(fake, color=RED, scale_factor=1.1))

        loop = VerifierLoop(width=10).move_to(DOWN * 1.6)
        self.play_at("But we know how to check", FadeOut(free), FadeIn(loop))
        computed = mono("computed: CID B", 22, RED).next_to(loop.stations[2], UP, buff=0.25)
        self.play_at("you compute their CID yourself", loop.run(ok=False))
        self.play(FadeIn(computed))
        bin_ = label("discard", 22, RED).next_to(fake, DOWN, buff=0.15)
        self.play_at("you throw it away", fake.animate.set_opacity(0.15), FadeIn(bin_))

        self.play_at("This defense is absolute", FadeOut(VGroup(computed, bin_, fake, loop.last_verdict)))
        fakes = VGroup(*[ByteRibbon.text(t, cell_size=0.24).tint(range(len(t)), RED)
                         for t in (b"edited.pdf", b"virus.bin", b"fake.txt", b"swap.doc")])
        fakes.arrange(DOWN, buff=0.15).next_to(client, DOWN, buff=0.3).align_to(client, LEFT)
        self.play(LaggedStart(*[FadeIn(f) for f in fakes], lag_ratio=0.2))
        crosses = VGroup(*[XMark(0.25).next_to(f, RIGHT, buff=0.15) for f in fakes])
        self.play_at("how many computers they have", LaggedStart(*[GrowFromCenter(x) for x in crosses], lag_ratio=0.2))
        forger = MaskedFigure("forger").scale(0.5).move_to(LEFT * 1.6 + UP * 0.4)
        forger_note = label("unless it's a genuine collision:\nthat's the Forger's job", 22, ADVERSARY_COLOR)
        forger_note.next_to(forger, RIGHT, buff=0.3)
        self.play_at("unless the liar has found a genuine collision", FadeIn(forger, shift=LEFT), FadeIn(forger_note))
        self.play_at("And that's the forger's job", Indicate(forger, color=ADVERSARY_COLOR))
        stat = caption("cost to defend: one hash computation · success rate of lying: 0%", 28, YELLOW)
        self.play_at("turns it into the most expensive one", FadeIn(stat))


class S03TwoJobs(NarratedScene):
    episode, number = 5, 3

    def construct(self):
        forger = MaskedFigure("forger").scale(0.7).to_edge(UP, buff=0.3)
        question = label("who made the original?", 36, YELLOW).next_to(forger, DOWN, buff=0.3)
        self.play(FadeIn(forger))
        games = VGroup(*[tag_box(t, GREY_B, 26) for t in ("1. preimage", "2. second preimage", "3. collision")])
        games.arrange(RIGHT, buff=0.4).move_to(UP * 0.3)
        self.play_at("Remember the three games", LaggedStart(*[FadeIn(g) for g in games], lag_ratio=0.3))
        self.play_at("one of the last two", games[0].animate.set_opacity(0.2),
                     *[g[0].animate.set_stroke(YELLOW) for g in games[1:]])
        self.play_at("Who made the original", Write(question))

        left_title = label("someone else made it", 26).move_to(LEFT * 3.6 + UP * 1.0)
        right_title = label("the forger made it", 26).move_to(RIGHT * 3.6 + UP * 1.0)
        yours = doc_icon("your file")
        target_tag = tag_box("CID", HASH_COLOR, 22)
        blank = doc_icon("???", RED)
        left = VGroup(VGroup(yours, target_tag).arrange(RIGHT, buff=0.4), blank).arrange(DOWN, buff=0.3).scale(0.8)
        left.next_to(left_title, DOWN, buff=0.3)
        left_cost = VGroup(label("second preimage", 24, GREY_A), label("~2⁵¹²", 40, YELLOW)).arrange(DOWN, buff=0.1)
        left_cost.next_to(left, DOWN, buff=0.25)
        self.play_at("Suppose the original was made by someone else", FadeOut(games), FadeIn(left_title), FadeIn(left[0]))
        self.play_at("the forger has to find a second preimage", FadeIn(blank))
        wire = DashedLine(blank.get_right(), target_tag.get_bottom(), color=HASH_COLOR)
        self.play_at("with the same CID as content", Create(wire))
        self.play_at("they had no part in creating", Indicate(yours, color=GREEN))
        self.play_at("two to the five hundred twelve tries", FadeIn(left_cost))
        nope = label("not happening", 24, GREY_B).next_to(left_cost, RIGHT, buff=0.3)
        self.play_at("That's not happening", FadeIn(nope))

        twins = TwinRibbons().scale(0.75).next_to(right_title, DOWN, buff=0.3)
        right_cost = VGroup(label("collision (birthday)", 24, GREY_A), label("~2²⁵⁶", 40, YELLOW)).arrange(DOWN, buff=0.1)
        right_cost.next_to(twins, DOWN, buff=0.25)
        self.play_at("But now suppose the forger made the original", FadeIn(right_title))
        self.play_at("any two documents of their own design", FadeIn(twins))
        self.play_at("thanks to the birthday effect", FadeIn(right_cost))

        # The story.
        self.play_at("Picture it as a story", *[FadeOut(m) for m in self.mobjects])
        you = VGroup(Circle(0.3, color=TEAL, fill_opacity=1), label("you", 22)).arrange(DOWN, buff=0.1)
        you.move_to(LEFT * 5 + UP * 0.5)
        forger.move_to(RIGHT * 5 + UP * 0.5)
        good = doc_icon("contract", GREEN).move_to(RIGHT * 2.5 + UP * 0.5)
        self.play(FadeIn(you), FadeIn(forger))
        self.play_at("The forger sends you a contract", FadeIn(good, shift=LEFT), run_time=0.5)
        self.play(good.animate.move_to(LEFT * 2.8 + UP * 0.5), run_time=0.6)
        approved = label("approved ✓", 24, GREEN).next_to(good, UP, buff=0.15)
        record = tag_box("CID: AAAAABkQ7r…", HASH_COLOR, 22).next_to(you, DOWN, buff=0.4)
        self.play_at("you approve it", FadeIn(approved))
        self.play_at("you record its CID", FadeIn(record, shift=DOWN * 0.2))
        self.play_at("Later, you fetch that CID", FadeOut(good), FadeOut(approved))
        bad = doc_icon("contract", RED).move_to(RIGHT * 2.5 + DOWN * 1.8)
        self.play_at("you get the other twin", FadeIn(bad), run_time=0.4)
        self.play(bad.animate.move_to(LEFT * 2.8 + DOWN * 1.8), run_time=0.6)
        loop = VerifierLoop(width=8, steps=("compute CID", "compare")).scale(0.8).move_to(RIGHT * 1.7 + DOWN * 1.8)
        self.play_at("You verify it", FadeIn(loop), run_time=0.4)
        self.play_at("And the check passes", loop.run(ok=True, step_time=0.2))
        uneasy = label("…and that's the problem.", 30, RED).to_edge(DOWN, buff=0.3)
        self.play_at("both twins genuinely have that name", FadeIn(uneasy))
        crack = caption("verification defeats the liar, not a true collision", 30, YELLOW).to_edge(UP, buff=0.4)
        self.play_at("So verification defeats the liar completely", FadeIn(crack))
        quote = label('"a CID uniquely determines its content"', 28).move_to(UP * 2.4)
        fracture = VMobject(color=RED, stroke_width=4).set_points_as_corners(
            [quote.get_left() + RIGHT * 2 + UP * 0.3, quote.get_center() + LEFT * 0.4,
             quote.get_center() + RIGHT * 0.3 + UP * 0.1, quote.get_right() + LEFT * 2 + DOWN * 0.3])
        self.play_at("That's the honest crack", FadeOut(crack), FadeIn(quote), Create(fracture))
        wide = label("how wide?", 30, YELLOW).next_to(quote, DOWN, buff=0.15)
        self.play_at("how wide the crack is", FadeIn(wide, shift=UP * 0.1))


class S04HowBig(NarratedScene):
    episode, number = 5, 4

    def construct(self):
        rigs = VGroup(*[VGroup(*[Rectangle(width=0.5, height=0.3, color=GREY_B, fill_color=GREY_D, fill_opacity=1)
                                 for _ in range(6)]).arrange(RIGHT, buff=0.05) for _ in range(4)])
        rigs.arrange(DOWN, buff=0.12)
        warehouses = VGroup(*[rigs.copy() for _ in range(3)]).arrange(RIGHT, buff=0.4).scale(0.8).to_edge(UP, buff=0.5)
        rate = label("Bitcoin network ≈ 10²¹ hashes per second ≈ 2⁷⁰ / s", 32).next_to(warehouses, DOWN, buff=0.3)
        foot = label("(SHA-256, not SHA-512 — used here only as a scale; late-2025 figure)", 20, GREY_B)
        foot.next_to(rate, DOWN, buff=0.12)
        ask = VGroup(pow2(256, size=80), label("a lot?", 48)).arrange(RIGHT, buff=0.4)
        self.play(FadeIn(ask))
        self.play_at("the biggest pile of hashing hardware", ask.animate.scale(0.4).to_corner(DR, buff=0.4),
                     FadeIn(warehouses, lag_ratio=0.05))
        self.play_at("the Bitcoin network", LaggedStart(*[Indicate(w, color=YELLOW, scale_factor=1.05) for w in warehouses],
                                                        lag_ratio=0.2))
        self.play_at("around a zettahash per second", FadeIn(rate))
        self.play_at("It computes a different hash", FadeIn(foot))
        scale = label('a stand-in for "every specialized hashing machine on Earth"', 24, YELLOW).next_to(foot, DOWN, buff=0.35)
        self.play_at("a sense of scale", FadeIn(scale, shift=UP * 0.1))

        math_ = VGroup(
            VGroup(label("2²⁵⁶", 34), label("/ 2⁷⁰ per second", 34), label("= 2¹⁸⁶ seconds", 34)).arrange(RIGHT, buff=0.2),
            label(f"≈ {ATTACK_YEARS / 1e48:.0f} × 10⁴⁸ years", 34, YELLOW),
        ).arrange(DOWN, buff=0.25).move_to(DOWN * 0.8)
        target = tag_box("target: one SHA-512 collision", HASH_COLOR, 24).move_to(UP * 0.55)
        self.play_at("Point all of that at a SHA-512 collision", FadeOut(scale), FadeIn(target, shift=DOWN * 0.2))
        for part, phrase in zip(math_[0], ["Two to the two hundred fifty-six, divided", "divided by two to the seventy",
                                           "is two to the one hundred eighty-six seconds"]):
            self.play_at(phrase, FadeIn(part, shift=UP * 0.1), run_time=0.5)
        self.play_at("three times ten to the forty-eight years", FadeIn(math_[1]))

        bar = Line(LEFT * 6.5, RIGHT * 7.5, color=RED, stroke_width=10).to_edge(DOWN, buff=0.9)
        tick = Line(UP * 0.25, DOWN * 0.25, color=WHITE).move_to(bar.get_start() + RIGHT * 0.05)
        tick_label = label("age of the universe (to scale: thinner than this line)", 20).next_to(tick, UP, buff=0.1)
        tick_label.align_to(tick, LEFT)
        ages = label(f"≈ {UNIVERSE_AGES / 1e38:.0f} × 10³⁸ universe-lifetimes", 26, RED).next_to(bar, DOWN, buff=0.15)
        self.play_at("The universe is about fourteen billion years old", Create(tick), FadeIn(tick_label))
        self.play_at("You'd need it to happen again", Create(bar), FadeIn(ages))

        self.play_at("And quantum computers", *[FadeOut(m) for m in self.mobjects])
        chip = VGroup(Square(1.6, color=TEAL, fill_color=GREY_E, fill_opacity=1),
                      *[Line(LEFT * 0.8, LEFT * 1.1, color=TEAL).shift(UP * y) for y in (-0.5, 0, 0.5)],
                      *[Line(RIGHT * 0.8, RIGHT * 1.1, color=TEAL).shift(UP * y) for y in (-0.5, 0, 0.5)],
                      Circle(0.35, color=TEAL)).move_to(UP * 1.5)
        q = label("best known quantum algorithms lower the exponent…", 30).next_to(chip, DOWN, buff=0.4)
        big = fit_width(mono(f"{2 ** 170:,}", 26, YELLOW), FRAME_W - 1).next_to(q, DOWN, buff=0.4)
        tag = label("…but leave numbers like 2¹⁷⁰ (52 digits)", 30, YELLOW).next_to(big, DOWN, buff=0.3)
        self.play(FadeIn(chip))
        self.play_at("do lower the exponents", FadeIn(q))
        future = VGroup(*[chip.copy().scale(0.45) for _ in range(4)])
        for f, d in zip(future, (LEFT * 2.4 + UP * 0.5, LEFT * 2.4 + DOWN * 0.5, RIGHT * 2.4 + UP * 0.5, RIGHT * 2.4 + DOWN * 0.5)):
            f.move_to(chip.get_center() + d)
        self.play_at("optimistic assumptions about future hardware", LaggedStart(*[FadeIn(f, scale=0.5) for f in future],
                                                                                 lag_ratio=0.2))
        self.play_at("the work for a SHA-512 collision", Indicate(q, color=YELLOW, scale_factor=1.03))
        self.play_at("numbers like two to the hundred and seventy", AddTextLetterByLetter(big), run_time=1.5)
        self.play(FadeIn(tag))
        verdict = caption("brute force is not how anyone will forge a 256t name", 30, GREEN)
        self.play_at("Brute force is simply not how", FadeIn(verdict))


class S05HowHashesDie(NarratedScene):
    episode, number = 5, 5

    def construct(self):
        axis = NumberLine(x_range=[1990, 2026, 5], length=12, color=GREY_B, include_numbers=False).move_to(UP * 0.3)
        years = VGroup(*[label(str(y), 18, GREY_B).next_to(axis.n2p(y), DOWN, buff=0.12) for y in range(1990, 2026, 5)])
        self.play(Create(axis), FadeIn(years))
        md5_y, sha1_y = 2.0, -1.6
        md5_name = label("MD5 · 128-bit · birthday bound 2⁶⁴", 24, ORANGE).move_to(LEFT * 3.4 + UP * 3.45)
        sha1_name = label("SHA-1 · 160-bit · birthday bound 2⁸⁰", 24, TEAL).move_to(LEFT * 3.4 + DOWN * 3.4)

        def event(year: float, y: float, text: str, color, above=True) -> VGroup:
            dot = Dot(axis.n2p(year) + UP * y, color=color, radius=0.09)
            stem = DashedLine(axis.n2p(year), dot.get_center(), color=color, stroke_width=1.5)
            note = fit_width(label(text, 18, color), 2.6)
            note.next_to(dot, UP if above else DOWN, buff=0.1)
            return VGroup(stem, dot, note)

        md5_heights = [2.0, 1.2, 2.3, 1.2]
        sha1_heights = [-1.1, -1.8, -1.1, -2.3]
        md5 = [(1992, "published", "M D five was published in 1992"),
               (2004, "collisions found\n(Wang et al.)", "In 2004, a team led by Xiaoyun Wang"),
               (2008, "rogue CA certificate", "Within a few years"),
               (2012, "Flame forges a Microsoft\ncode-signing certificate", "in 2012, a piece of espionage malware")]
        sha1 = [(1995, "published", "SHA-1, with a hundred-and-sixty bit output"),
                (2005, "theoretical shortcut", "A theoretical shortcut was published in 2005"),
                (2017, "SHAttered: two PDFs,\n~2⁶³ work, ~100 GPU-years", "In 2017, researchers from Google"),
                (2020, "chosen-prefix collision,\n~tens of thousands of $", "And by 2020")]
        self.play(FadeIn(md5_name))
        events = {}
        shortcut = VGroup(tag_box("more inputs (brute force)", GREY_B, 20), tag_box("structure shortcut", ORANGE, 20))
        shortcut.arrange(DOWN, buff=0.25).move_to(RIGHT * 4.9 + UP * 2.2)
        brute_strike = Line(shortcut[0].get_left(), shortcut[0].get_right(), color=RED, stroke_width=4)
        for k, (year, text, phrase) in enumerate(md5):
            ev = event(year, md5_heights[k], text, ORANGE)
            events[year] = ev
            if year == 2008:
                self.play_at(phrase, FadeIn(ev, shift=DOWN * 0.1), FadeOut(VGroup(shortcut, brute_strike)), run_time=0.6)
            else:
                self.play_at(phrase, FadeIn(ev, shift=DOWN * 0.1), run_time=0.6)
            if year == 2004:
                self.play_at("not by trying more inputs", FadeIn(shortcut[0]))
                self.play(Create(brute_strike))
                self.play_at("exploiting structure", FadeIn(shortcut[1], shift=UP * 0.1))
                self.play_at("mixes its bits", Indicate(shortcut[1], color=ORANGE))
            if year == 2012:
                self.play_at("called Flame", Indicate(ev[2], color=RED))
        flame = label("the twins scenario, in the wild", 22, RED).move_to(RIGHT * 4.4 + UP * 3.4)
        cert = VGroup(RoundedRectangle(corner_radius=0.08, width=2.2, height=1.2, color=RED, fill_color=GREY_E, fill_opacity=1),
                      label("Microsoft\ncode-signing cert", 18), label("forged via MD5", 16, RED))
        cert[1].move_to(cert[0]).shift(UP * 0.12)
        cert[2].next_to(cert[1], DOWN, buff=0.06)
        update = tag_box("update ✓ (really Flame)", RED, 18)
        flame_art = VGroup(cert, update).arrange(DOWN, buff=0.2).move_to(RIGHT * 5.2 + UP * 1.75)
        self.play_at("to forge a Microsoft code-signing certificate", FadeIn(cert, shift=LEFT * 0.2))
        self.play_at("disguised as a legitimate software update", FadeIn(update, shift=UP * 0.2))
        self.play_at("That's the twins scenario", FadeIn(flame))
        self.play(FadeIn(sha1_name))
        pdfs = VGroup(doc_icon("PDF 1", WHITE), doc_icon("PDF 2", BLUE_B)).arrange(RIGHT, buff=0.2).scale(0.5)
        same = label("same SHA-1", 18, RED).next_to(pdfs, DOWN, buff=0.08)
        VGroup(pdfs, same).move_to(RIGHT * 5.6 + DOWN * 3.3)
        for k, (year, text, phrase) in enumerate(sha1):
            ev = event(year, sha1_heights[k], text, TEAL, above=False)
            self.play_at(phrase, FadeIn(ev, shift=UP * 0.1), run_time=0.6)
            if year == 2017:
                self.play_at("produced two different PDF files", FadeIn(pdfs), FadeIn(same))
                self.play_at("two to the sixty-three computations", Indicate(ev[2], color=YELLOW))
                gpus = VGroup(*[Rectangle(width=0.16, height=0.1, stroke_width=0, fill_color=TEAL, fill_opacity=0.8)
                                for _ in range(100)]).arrange_in_grid(5, 20, buff=0.03)
                gpus.move_to(RIGHT * 2.9 + DOWN * 3.35)
                gpu_note = label("~100 GPU-years", 16, TEAL).next_to(gpus, UP, buff=0.08)
                self.play_at("a hundred GPU-years", FadeIn(gpus, lag_ratio=0.01), FadeIn(gpu_note))
            if year == 2005:
                self.play(FadeOut(flame_art), run_time=0.4)
            if year == 2020:
                self.play(FadeOut(VGroup(gpus, gpu_note)), run_time=0.3)
            if year == 2020:
                docs = VGroup(*[VGroup(doc_icon(t, c), Rectangle(width=1.1, height=0.3, stroke_width=0, fill_color=GREY_B,
                                                                     fill_opacity=0.6))
                                .arrange(DOWN, buff=0.04) for t, c in (("cert: Alice", WHITE), ("cert: Mallory", RED))])
                docs.arrange(RIGHT, buff=0.3).scale(0.75).move_to(RIGHT * 5.3 + UP * 2.2)
                gib = label("+ gibberish → same SHA-1", 18, RED).next_to(docs, DOWN, buff=0.1)
                self.play_at("where both documents can say whatever", FadeIn(docs), FadeIn(gib))
                cost = tag_box("~$45k (est., 2020)", YELLOW, 20).next_to(gib, DOWN, buff=0.15)
                self.play_at("tens of thousands of dollars", FadeIn(cost))

        lessons = VGroup(*[tag_box(t, YELLOW, 24) for t in (
            "1. broken by shortcuts, not brute force",
            "2. years of warning",
            "3. small outputs: 2⁶⁴ and 2⁸⁰ birthday bounds")]).arrange(DOWN, buff=0.25, aligned_edge=LEFT)
        bg = BackgroundRectangle(lessons, fill_opacity=0.92, buff=0.3)
        VGroup(bg, lessons).move_to(ORIGIN)
        backdrop = [m for m in self.mobjects]
        self.play_at("Three lessons", FadeIn(bg), *[m.animate.set_opacity(0.15) for m in backdrop])
        for item, phrase in zip(lessons, ["First, these functions died from shortcuts", "Second, there were years of warning",
                                          "And third, their outputs were small"]):
            self.play_at(phrase, FadeIn(item, shift=UP * 0.15), run_time=0.5)
        unit = 9.0 / 256
        margins = VGroup()
        for name, bits, color in (("MD5", 64, ORANGE), ("SHA-1", 80, TEAL), ("SHA-512", 256, HASH_COLOR)):
            bar = Rectangle(width=bits * unit, height=0.3, stroke_width=0, fill_color=color, fill_opacity=0.85)
            tag = label(f"{name}: 2{sup(bits)}", 20, color)
            margins.add(VGroup(tag, bar).arrange(RIGHT, buff=0.2))
        margins.arrange(DOWN, aligned_edge=LEFT, buff=0.12)
        for m in margins:
            m[1].align_to(margins, LEFT).shift(RIGHT * 2.0)
        margins_bg = BackgroundRectangle(margins, fill_opacity=0.92, buff=0.2)
        VGroup(margins_bg, margins).next_to(bg, DOWN, buff=0.15)
        self.play_at("birthday bound was only two to the sixty-four", FadeIn(margins_bg), FadeIn(margins[0][0]),
                     GrowFromEdge(margins[0][1], LEFT))
        self.play_at("two to the eighty", FadeIn(margins[1][0]), GrowFromEdge(margins[1][1], LEFT))
        self.play_at("much margin to lose", FadeIn(margins[2][0]), GrowFromEdge(margins[2][1], LEFT))


class S06SHA512Today(NarratedScene):
    episode, number = 5, 6

    def construct(self):
        title = label("SHA-512 · 512-bit · birthday bound 2²⁵⁶", 32, HASH_COLOR).to_edge(UP, buff=0.5)
        track = NumberLine(x_range=[2000, 2026, 5], length=11, color=GREY_B, include_numbers=False).next_to(title, DOWN, buff=0.7)
        years = VGroup(*[label(str(y), 18, GREY_B).next_to(track.n2p(y), DOWN, buff=0.12) for y in range(2000, 2026, 5)])
        start = Dot(track.n2p(2001.5), color=HASH_COLOR)
        empty = label("no collision ever found", 26, GREEN).next_to(track, UP, buff=0.15).shift(RIGHT * 1.5)
        self.play_at("So where does SHA-512 stand", FadeIn(title), Create(track), FadeIn(years), FadeIn(start))
        lifetime = Line(track.n2p(2001.5), track.n2p(2025.8), color=HASH_COLOR, stroke_width=6)
        self.play_at("over two decades", Create(lifetime, run_time=1.2))
        studied = label("one of the most studied functions in cryptography", 22, GREY_A).next_to(years, DOWN, buff=0.25)
        self.play_at("most studied functions", FadeIn(studied))
        self.play_at("no collision for it has ever been found", FadeIn(empty))

        bar = Rectangle(width=11, height=0.7, color=GREY_B).move_to(DOWN * 1.0)
        unit = 11 / 80
        steps_label = label("80 steps", 24, GREY_B).next_to(bar, RIGHT, buff=0.2)
        p27 = Rectangle(width=27 * unit, height=0.7, stroke_width=0, fill_color=ORANGE, fill_opacity=0.7)
        p27.align_to(bar, LEFT).align_to(bar, UP)
        p39 = Rectangle(width=39 * unit, height=0.35, stroke_width=0, fill_color=PURPLE, fill_opacity=0.7)
        p39.next_to(bar, DOWN, buff=0.1).align_to(bar, LEFT)
        l27 = label("27 / 80: best practical collision (reduced rounds)", 22, ORANGE).next_to(bar, UP, buff=0.15).align_to(bar, LEFT)
        l39 = label("39 / 80: semi-free-start (attacker also picks the starting state)", 22, PURPLE)
        l39.next_to(p39, DOWN, buff=0.15).align_to(bar, LEFT)
        full = label("the full function: untouched", 26, GREEN).next_to(l39, DOWN, buff=0.4)
        self.play_at("They attack weakened versions", Create(bar), FadeIn(steps_label))
        self.play_at("reach twenty-seven of its eighty steps", GrowFromEdge(p27, LEFT), FadeIn(l27))
        regs = VGroup(*[Square(0.4, color=PURPLE, fill_color=PURPLE, fill_opacity=0.3) for _ in range(8)]).arrange(RIGHT, buff=0.06)
        cant = VGroup(XMark(0.3), label("real attacks start from the fixed, published state", 20, RED)).arrange(RIGHT, buff=0.15)
        cant.next_to(regs, RIGHT, buff=0.3)
        fit_width(VGroup(regs, cant), FRAME_W - 1).move_to(UP * 0.35)
        regs_label = label("starting state, chosen by the attacker", 22, PURPLE).next_to(regs, UP, buff=0.12)
        start_state = VGroup(regs, regs_label)
        self.play_at("choose the internal starting state", FadeIn(start_state, shift=DOWN * 0.1))
        self.play_at("which a real attacker can't", FadeIn(cant))
        self.play_at("they reach thirty-nine steps", FadeOut(VGroup(start_state, cant)), GrowFromEdge(p39, LEFT), FadeIn(l39))
        self.play_at("That's a meaningful distance", FadeIn(full))
        bound = label("generic collision cost: 2²⁵⁶", 30, HASH_COLOR).next_to(full, DOWN, buff=0.3)
        self.play_at("And its birthday bound starts at", FadeIn(bound, shift=UP * 0.2))
        chunk = label("e.g. a 2⁸⁰-fold shortcut would still leave 2¹⁷⁶", 30, YELLOW).move_to(bound)
        self.play_at("knocked off a huge chunk", Transform(bound, chunk))
        self.play_at("an astronomical number behind", Indicate(bound, color=YELLOW))
        maybe = caption("could that change someday?", 28, GREY_A)
        self.play_at("Could that change someday", FadeOut(bound), FadeIn(maybe))
        self.play_at("In principle", Indicate(maybe, color=GREY_A))
        self.play_at("if history is a guide", Indicate(empty, color=GREEN))
        warn = caption("if history is a guide: years of warning", 28, YELLOW)
        self.play_at("we'd see the cracks coming years ahead", ReplacementTransform(maybe, warn))


class S07WhatStructureBuys(NarratedScene):
    episode, number = 5, 7

    def construct(self):
        cards = VGroup(
            card("inline CIDs", "≤ 64 bytes: no forging possible, ever", INLINE_COLOR, width=4.2, height=2.6,
                 icon=Check(0.6, INLINE_COLOR)),
            card("length prefix", "forger just builds twins of\nequal length — little help here", LENGTH_COLOR,
                 width=4.2, height=2.6, icon=label("?", 50, LENGTH_COLOR)),
            card("everything else", "exactly as strong as SHA-512's\ncollision resistance", HASH_COLOR,
                 width=4.2, height=2.6, icon=FingerprintIcon()),
        ).arrange(RIGHT, buff=0.3)
        question = label("does 256t's structure add anything?", 34).to_edge(UP, buff=0.6)
        self.play(FadeIn(question))
        partly = label("partly.", 34, YELLOW).next_to(question, RIGHT, buff=0.3)
        self.play_at("Partly", FadeIn(partly, shift=LEFT * 0.1))
        self.play_at("Inline CIDs can't be forged", FadeIn(cards[0], shift=UP * 0.2))
        self.play_at("since the content is the name", Indicate(cards[0], color=INLINE_COLOR, scale_factor=1.05))
        self.play_at("The length prefix, honestly", FadeIn(cards[1], shift=UP * 0.2))
        hint = label("(it shines against the Liar — next episode)", 22, LENGTH_COLOR).next_to(cards[1], DOWN, buff=0.3)
        self.play_at("It helps a lot against a different kind", FadeIn(hint))
        self.play_at("For everything else", FadeIn(cards[2], shift=UP * 0.2))
        strength = label("generic collision cost: 2²⁵⁶", 22, HASH_COLOR).next_to(cards[2], DOWN, buff=0.3)
        self.play_at("exactly as strong as", Indicate(cards[2], color=HASH_COLOR, scale_factor=1.05), FadeIn(strength))


class S08TwoDefenses(NarratedScene):
    episode, number = 5, 8

    def construct(self):
        gates = [("1. finds a real SHA-512 collision", 0.25, "no one ever has"),
                 ("2. authored content you rely on", 2.0, "plants one twin"),
                 ("3. hands you the other twin", 2.0, "controls a source you use")]
        rows = VGroup()
        for text, width, note in gates:
            bar = Rectangle(width=4, height=0.45, color=GREY_B)
            opening = Rectangle(width=width, height=0.45, stroke_width=0, fill_color=RED, fill_opacity=0.8)
            opening.move_to(bar)
            rows.add(VGroup(label(text, 26), VGroup(bar, opening), label(note, 20, GREY_B)).arrange(RIGHT, buff=0.4))
        rows.arrange(DOWN, buff=0.45, aligned_edge=LEFT).move_to(UP * 0.8)
        for row in rows:
            row[1].align_to(rows, LEFT).shift(RIGHT * 5.6)
            row[2].next_to(row[1], RIGHT, buff=0.3)
        heading = label("for an adversary to cause a problem collision:", 28, YELLOW).to_edge(UP, buff=0.4)
        self.play_at("three things have to line up", FadeIn(heading))
        for row, phrase in zip(rows, ["They have to find a genuine", "They have to be the author",
                                      "they have to be in a position"]):
            self.play_at(phrase, FadeIn(row, shift=RIGHT * 0.2), run_time=0.6)
        liar = MaskedFigure("liar").scale(0.5).move_to(LEFT * 4.5 + DOWN * 2.2)
        loop = VerifierLoop(width=7, steps=("compute", "compare")).scale(0.7).next_to(liar, RIGHT, buff=0.6)
        self.play_at("Without the first, they're just a liar", FadeIn(liar), FadeIn(loop))
        self.play(loop.run(ok=False))

        self.play_at("comes down to two habits", *[FadeOut(m) for m in self.mobjects])
        habits = VGroup(card("Always verify CID content.", "one hash computation turns every\ncheap attack into the expensive one",
                             GREEN, width=6.0, height=2.4),
                        card("Reduce adversaries.", "be thoughtful about who authored\nwhat you trust, and where you fetch",
                             YELLOW, width=6.0, height=2.4)).arrange(RIGHT, buff=0.4)
        self.play_at("First: always verify", FadeIn(habits[0], shift=UP * 0.2))
        self.play_at("Second: reduce adversaries", FadeIn(habits[1], shift=UP * 0.2))
        who = VGroup(mask_icon(ADVERSARY_COLOR, 0.5), label("who authored it — would they gain from a swap?", 22)).arrange(RIGHT, buff=0.25)
        where = VGroup(ServerRack(height=0.6), label("where are you fetching from?", 22)).arrange(RIGHT, buff=0.25)
        questions = VGroup(who, where).arrange(DOWN, aligned_edge=LEFT, buff=0.3).next_to(habits, DOWN, buff=0.5)
        self.play_at("whose author would benefit", FadeIn(who, shift=UP * 0.1))
        self.play_at("about where you fetch from", FadeIn(where, shift=UP * 0.1))
        dice = dice_icon(GREY_B, 0.8).next_to(habits, DOWN, buff=0.6)
        back = label("back to accidents only", 24, GREY_B).next_to(dice, RIGHT, buff=0.3)
        self.play_at("you're back to accidents only", FadeOut(questions), FadeIn(dice), FadeIn(back))
        nxt = caption("next: what should your code do when bytes arrive?", 28, YELLOW)
        self.play_at("Next time", FadeOut(VGroup(dice, back)), FadeIn(nxt))
        self.play_at("the first habit", Indicate(habits[0], color=GREEN, scale_factor=1.05), habits[1].animate.set_opacity(0.3))
        stream = ByteRibbon.text(b"bytes arriving", cell_size=0.3).move_to(LEFT * 3 + DOWN * 1.8)
        cache = CacheBox(size=1.0).move_to(RIGHT * 3.5 + DOWN * 1.8)
        to_cache = Arrow(stream.get_right(), cache.get_left(), buff=0.3, color=GREY_B)
        self.play_at("when bytes arrive", FadeIn(stream, shift=RIGHT * 0.5))
        self.play_at("cache them forever", GrowArrow(to_cache), FadeIn(cache, scale=0.6))
