"""Episode 3 — What a Hash Promises.

Scenes follow video-series/ep03-what-a-hash-promises.md. Render with:
    python3 video-series/manim/render.py 3
"""

from common import *
from sha512_rounds import changed_bits, round_states
from ep01 import person

HELLO_UPPER, HELLO_LOWER = b"Hello", b"hello"
BALLOT = b"Board ballot #2026-117. Member vote recorded as follows. Final answer: "


def game_card(number: int, title: str, given: str, goal: str, cost: str, color=WHITE) -> VGroup:
    frame = RoundedRectangle(corner_radius=0.2, width=4.1, height=4.4, stroke_color=color,
                             fill_color=GREY_E, fill_opacity=0.9)
    head = label(f"{number}. {title}", 30, color)
    lines = VGroup(
        label("given:", 20, GREY_B), fit_width(label(given, 22), 3.7),
        label("goal:", 20, GREY_B), fit_width(label(goal, 22), 3.7),
    ).arrange(DOWN, aligned_edge=LEFT, buff=0.12)
    meter = VGroup(label("cost", 20, GREY_B), label(cost, 34, YELLOW)).arrange(RIGHT, buff=0.3)
    body = VGroup(head, lines, meter).arrange(DOWN, buff=0.35).move_to(frame)
    return VGroup(frame, body)


class S01OneLetterTwoWorlds(NarratedScene):
    episode, number = 3, 1

    def construct(self):
        upper = CIDText(compute_cid(HELLO_UPPER), size=44)
        lower = CIDText(compute_cid(HELLO_LOWER), size=44)
        words = VGroup(label("Hello", 36), label("hello", 36))
        inline = VGroup(VGroup(words[0], upper).arrange(RIGHT, buff=0.6),
                        VGroup(words[1], lower).arrange(RIGHT, buff=0.6)).arrange(DOWN, buff=0.4)
        inline.move_to(UP * 0.5)
        self.play_at("Here are two words", FadeIn(words))
        self.play_at("with a capital H", Indicate(words[0][0], color=YELLOW, scale_factor=1.6))
        self.play_at("with a lowercase one", Indicate(words[1][0], color=YELLOW, scale_factor=1.6))
        self.play_at("Their inline CIDs", FadeIn(upper), FadeIn(lower))
        diff = VGroup(SurroundingRectangle(upper.chars(8, 9), color=YELLOW, buff=0.05),
                      SurroundingRectangle(lower.chars(8, 9), color=YELLOW, buff=0.05))
        self.play_at("differ in a single character", Create(diff))

        h1 = fit_width(mono(sha512_b64(HELLO_UPPER), 24, HASH_COLOR), FRAME_W - 0.8)
        h2 = fit_width(mono(sha512_b64(HELLO_LOWER), 24, HASH_COLOR), FRAME_W - 0.8)
        hashes = VGroup(h1, h2).arrange(DOWN, buff=0.35).move_to(UP * 2.2)
        self.play_at("Now here are their SHA-512 fingerprints",
                     FadeOut(diff), inline.animate.scale(0.6).to_edge(DOWN, buff=0.4),
                     FadeIn(hashes, shift=DOWN * 0.2))
        self.play_at("Not one family resemblance", Indicate(hashes, color=YELLOW, scale_factor=1.03))

        grid = DiffusionGrid(cell=0.2).move_to(DOWN * 0.6)
        mask = diff_mask(hashlib.sha512(HELLO_UPPER).digest(), hashlib.sha512(HELLO_LOWER).digest())
        count = label(f"{mask.count('1')} of 512 bits differ ≈ {mask.count('1') / 512:.0%}", 32, YELLOW)
        count.next_to(grid, DOWN, buff=0.25)
        self.play_at("Line up the bits and count", FadeOut(inline), FadeIn(grid))
        self.play(grid.show_mask(mask, run_time=1.2))
        self.play_at("two hundred and thirty-nine", Write(count))
        self.play_at("completely unrelated random numbers", Indicate(grid, color=YELLOW, scale_factor=1.02))
        box = RecipeMachine("SHA-512", icon=FingerprintIcon(), width=3.0, height=2.0)
        self.play_at("Last time we treated this fingerprint", FadeOut(VGroup(grid, count, hashes)),
                     FadeIn(box))
        self.play_at("Today we open the box", Indicate(box, color=HASH_COLOR, scale_factor=1.08))
        q = label("what exactly is it promising?", 36, YELLOW).next_to(box, DOWN, buff=0.5)
        self.play_at("what exactly is it promising", Write(q))


class S02WhatAHashIs(NarratedScene):
    episode, number = 3, 2

    def construct(self):
        machine = RecipeMachine("SHA-512", width=3.0, height=1.8).move_to(UP * 0.8)
        out = Rectangle(width=2.6, height=0.6, color=HASH_COLOR, fill_color=HASH_COLOR, fill_opacity=0.3)
        out_label = label("512 bits", 24, HASH_COLOR)
        self.play(FadeIn(machine))
        inputs = [ByteRibbon.text(b"", cell_size=0.35), ByteRibbon.text(b"A", cell_size=0.35),
                  ByteRibbon.text(b"a paragraph of text", cell_size=0.3, max_cells=10),
                  ByteRibbon.text(b"an enormous movie file", cell_size=0.3, max_cells=16)]
        cues = ["from nothing at all", None, "to an enormous file", None]
        for rib, phrase in zip(inputs, cues):
            rib.move_to(LEFT * 4.6 + UP * 0.8)
            block = VGroup(out.copy(), out_label.copy())
            block[1].move_to(block[0])
            block.next_to(machine.body, RIGHT, buff=0.4)
            if phrase:
                self.cue(phrase)
            self.play(feed_through(machine, rib, block, run_time=0.8))
            self.play(FadeOut(block), run_time=0.15)

        tags = VGroup(*[tag_box(t, WHITE, 24) for t in ("any size in", "fixed size out", "deterministic", "fast")])
        tags.arrange(RIGHT, buff=0.3).next_to(machine, DOWN, buff=0.8)
        for tag, phrase in zip(tags, [None, "always five hundred and twelve bits",
                                      "It's deterministic", "And it's fast"]):
            if phrase:
                self.cue(phrase)
            self.play(FadeIn(tag, shift=UP * 0.2), run_time=0.5)

        fp = FingerprintIcon(size=1.8).move_to(RIGHT * 4.6 + UP * 0.8)
        cap = label("a fingerprint for data", 32).next_to(tags, DOWN, buff=0.6)
        self.play_at("The usual metaphor is a fingerprint", FadeIn(fp, scale=0.5), Write(cap))
        prints = VGroup(FingerprintIcon(size=0.9), label("=?", 30, GREY_A), FingerprintIcon(size=0.9)).arrange(RIGHT, buff=0.25)
        prints.move_to(LEFT * 4.8 + UP * 0.8)
        easy = label("easy", 24, GREEN).next_to(prints, UP, buff=0.2)
        self.play_at("compare two fingerprints", FadeIn(prints), FadeIn(easy))
        people = VGroup(person(TEAL).scale(1.4), label("=?", 30, GREY_A), person(PINK).scale(1.4)).arrange(RIGHT, buff=0.25)
        people.move_to(prints)
        self.play_at("two whole people", Transform(prints, people), Transform(easy, label("hard", 24, RED).move_to(easy)))
        star = label("*", 44, RED).next_to(cap, RIGHT, buff=0.05).shift(UP * 0.1)
        self.play_at("But metaphors leak", FadeIn(star, scale=2))
        self.play_at("So let's find the leak", Indicate(star, color=RED, scale_factor=1.6))


class S03CollisionsMustExist(NarratedScene):
    episode, number = 3, 3

    def construct(self):
        left = VGroup(Rectangle(width=2.0, height=5.0, color=GREY_B, fill_color=GREY_D, fill_opacity=0.6),
                      label("all 65-byte files", 24))
        left[1].next_to(left[0], UP, buff=0.15)
        lcount = pow2(520, size=40).next_to(left[0], DOWN, buff=0.2)
        right = VGroup(Rectangle(width=2.0, height=3.0, color=HASH_COLOR, fill_color=HASH_COLOR, fill_opacity=0.25),
                       label("all SHA-512 outputs", 24, HASH_COLOR))
        right[1].next_to(right[0], UP, buff=0.15)
        rcount = pow2(512, size=40, color=HASH_COLOR).next_to(right[0], DOWN, buff=0.2)
        VGroup(left, lcount).move_to(LEFT * 4 + UP * 0.2)
        VGroup(right, rcount).move_to(RIGHT * 4 + UP * 0.2)
        rcount.align_to(lcount, DOWN)
        self.play_at("Consider just the files", FadeIn(left))
        steps = VGroup(label("65 bytes: one too many\nto be stored inline", 26, GREY_A),
                       label("256 = 2⁸ choices per byte", 28),
                       label("256⁶⁵ = (2⁸)⁶⁵ = 2⁵²⁰", 32, YELLOW)).arrange(DOWN, buff=0.45).move_to(UP * 0.4)
        self.play_at("one byte too long to be stored inline", FadeIn(steps[0], shift=UP * 0.2))
        self.play_at("Each byte has two hundred fifty-six possibilities", FadeIn(steps[1], shift=UP * 0.2))
        self.play_at("so there are two to the five hundred and twenty", FadeIn(steps[2], shift=UP * 0.2), FadeIn(lcount))
        self.play_at("And how many possible SHA-512 outputs", FadeOut(steps), FadeIn(right))
        self.play_at("Two to the five hundred and twelve", FadeIn(rcount))

        # Pigeons into holes.
        holes = VGroup(*[Square(0.45, color=HASH_COLOR) for _ in range(8)]).arrange_in_grid(4, 2, buff=0.12)
        holes.move_to(right[0])
        rng = np.random.default_rng(3)
        pigeons = VGroup(*[Dot(radius=0.07, color=GREY_A).move_to(
            left[0].get_center() + np.array([rng.uniform(-0.8, 0.8), rng.uniform(-2.3, 2.3), 0]))
            for _ in range(24)])
        self.play_at("More files than fingerprints", FadeIn(holes), FadeIn(pigeons))
        moves, counts = [], [0] * 8
        for k, p in enumerate(pigeons):
            slot = k % 8
            offset = np.array([-0.12 + 0.12 * (counts[slot] % 3), 0.1 - 0.12 * (counts[slot] // 3), 0])
            counts[slot] += 1
            moves.append(p.animate.move_to(holes[slot].get_center() + offset))
        self.play_at("the pigeonhole principle", LaggedStart(*moves, lag_ratio=0.08, run_time=2.4))
        self.play(*[h.animate.set_fill(RED, opacity=0.35).set_stroke(RED) for h in holes])
        avg = label("2⁵²⁰ / 2⁵¹² = 2⁸ = 256\nper fingerprint, on average", 30, YELLOW).move_to(UP * 1.4)
        self.play_at("on average, every possible SHA-512 output", Write(avg))
        self.play_at("shared by two hundred and fifty-six", *[Indicate(h, color=RED) for h in holes])
        more = VGroup(*[left[0].copy().set_opacity(0.25) for _ in range(3)])
        for k, m in enumerate(more):
            m.shift((RIGHT * 0.15 + UP * 0.15) * (k + 1))
        more_label = label("+ 66-byte, 67-byte, … files", 22, GREY_A).next_to(lcount, DOWN, buff=0.12)
        self.play_at("that's just the sixty-five byte files", FadeIn(more, lag_ratio=0.3), FadeIn(more_label))
        stamp = label("guaranteed to exist", 34, RED).rotate(0.12).move_to(right[0])
        self.play_at("Collisions don't merely exist", FadeIn(stamp, scale=1.6))

        resistant = label('"collision resistant"', 26).move_to(DOWN * 0.9)
        self.play_at('So when people say SHA-512 is "collision resistant"', FadeOut(VGroup(more, more_label)), FadeIn(resistant))
        not_none = VGroup(resistant.copy(), label("≠  no collisions", 26, RED)).arrange(DOWN, buff=0.15).move_to(DOWN * 1.1)
        self.play_at("they cannot mean there are no collisions", Transform(resistant, not_none))
        self.play_at("although collisions are everywhere", Indicate(stamp, color=RED, scale_factor=1.15))
        q = caption("the question is whether anyone can FIND one", 32, YELLOW).to_edge(DOWN, buff=0.25)
        self.play_at("nobody knows how to find even one", FadeIn(q, shift=UP))

        inline = tag_box("≤ 64 bytes:\nno hashing,\nso no pigeonholes", INLINE_COLOR, 26).move_to(DOWN * 0.6)
        self.play_at("Notice, by the way", FadeOut(resistant), FadeIn(inline))
        hi = VGroup(mono('"Hi"', 28), Arrow(LEFT * 0.4, RIGHT * 0.4, color=GREY_B), mono(compute_cid(b"Hi"), 28, INLINE_COLOR))
        hi.arrange(RIGHT, buff=0.25).next_to(inline, DOWN, buff=0.4)
        self.play_at("For content of sixty-four bytes or less", FadeIn(hi, shift=UP * 0.1))
        self.play_at("no hashing, and so no pigeonholes", Indicate(hi[2], color=INLINE_COLOR, scale_factor=1.2))
        self.play_at("collision free by construction", Indicate(inline, color=INLINE_COLOR))


class S04ThreeGames(NarratedScene):
    episode, number = 3, 4

    def construct(self):
        cards = VGroup(
            game_card(1, "Preimage", "a fingerprint alone", "anyone with that print", "~2⁵¹²"),
            game_card(2, "Second preimage", "Alice", "someone else matching Alice", "~2⁵¹²"),
            game_card(3, "Collision", "nothing", "any two people who match", "~2²⁵⁶", YELLOW),
        ).arrange(RIGHT, buff=0.25).move_to(UP * 0.4)
        data = [
            "given a hash, find any input that produces it",
            "given a file, find a different file with the same hash",
            "find any two different inputs with the same hash",
        ]
        data_lines = VGroup(*[fit_width(label(t, 22, GREY_A), 4.1).next_to(c, DOWN, buff=0.15)
                              for t, c in zip(data, cards)])
        self.play_at("Security people split", FadeIn(label("three games", 36).to_edge(UP, buff=0.2)))
        slots = VGroup(*[VGroup(DashedVMobject(c[0].copy().set_fill(opacity=0), num_dashes=40),
                                label(str(k + 1), 60, GREY_B).move_to(c)) for k, c in enumerate(cards)])
        self.play_at("into three separate games", LaggedStart(*[FadeIn(sl) for sl in slots], lag_ratio=0.3))
        self.play_at("very different difficulties", *[Indicate(sl[1], color=YELLOW) for sl in slots])
        analogy = VGroup(person(GREY_A), FingerprintIcon(size=0.5)).arrange(RIGHT, buff=0.15).to_corner(UR, buff=0.3)
        self.play_at("fingerprints of people", FadeIn(analogy, shift=LEFT * 0.2))
        costs = [c[1][2][1] for c in cards]
        given = [c[1][1][1] for c in cards]
        goal = [c[1][1][3] for c in cards]
        pre = [[("I hand you a fingerprint", lambda: Indicate(given[0], color=YELLOW)),
                ("find any person in the world", lambda: Indicate(goal[0], color=YELLOW))],
               [("call her Alice", lambda: Indicate(given[1], color=YELLOW)),
                ("find somebody else", lambda: Indicate(goal[1], color=YELLOW))],
               [("I give you nothing at all", lambda: Indicate(given[2], color=YELLOW))]]
        beats = [
            ("Game one is called finding a preimage", "given an output, find any input",
             [("the best you can do is guess inputs at random", lambda: Circumscribe(cards[0], color=YELLOW)),
              ("that takes on the order of two to the five hundred and twelve tries",
               lambda: Indicate(costs[0], color=YELLOW, scale_factor=1.4))]),
            ("Game two is the second preimage", "here's a file, now find a different file",
             [("This is also about two to the five hundred and twelve tries",
               lambda: Indicate(costs[1], color=YELLOW, scale_factor=1.4))]),
            ("Game three is the collision", "find any two people",
             [("This turns out to be dramatically easier",
               lambda: Indicate(costs[2], color=YELLOW, scale_factor=1.4))]),
        ]
        for card_, slot, line, before, (phrase, data_phrase, extras) in zip(cards, slots, data_lines, pre, beats):
            self.play_at(phrase, FadeOut(slot), FadeIn(card_, shift=UP * 0.2))
            for pre_phrase, make in before:
                self.play_at(pre_phrase, make())
            self.play_at(data_phrase, FadeIn(line))
            for extra_phrase, make in extras:
                self.play_at(extra_phrase, make())
        web = PairWeb(7, radius=0.55).move_to(RIGHT * 4.3 + DOWN * 3.05)
        web_note = label("pairs grow faster than guesses", 20, BLUE_B).next_to(web, LEFT, buff=0.3)
        self.play_at("the number of pairs grows much faster", FadeIn(web.dots), Create(web.lines), FadeIn(web_note))
        root = label("√(2⁵¹²) = 2²⁵⁶", 28, YELLOW).next_to(web_note, LEFT, buff=0.6)
        self.play_at("The cost drops to about the square root", FadeIn(root))
        cake = label("birthday effect — Episode 4", 22, YELLOW).next_to(cards[2], UP, buff=0.15)
        self.play_at("We'll derive exactly why", Indicate(root, color=YELLOW))
        self.play_at("It's the famous birthday paradox", FadeIn(cake))

        # Scale: 2^256 is halfway to 2^512 in exponent, and near the atom count.
        self.play_at('"Dramatically easier" is relative', FadeOut(VGroup(cards, data_lines, cake, web, web_note, root, analogy)))
        bar = NumberLine(x_range=[0, 512, 64], length=11, include_numbers=False, color=GREY_B).move_to(UP * 1.2)
        ticks = VGroup(*[label(power_text(v) if v else "1", 18, GREY_B).next_to(bar.n2p(v), DOWN, buff=0.15)
                         for v in (0, 128, 256, 384, 512)])
        m256 = VGroup(Triangle(color=YELLOW, fill_opacity=1).scale(0.12).rotate(PI).next_to(bar.n2p(256), UP, buff=0.05),
                      label("collision ~2²⁵⁶", 22, YELLOW))
        m256[1].next_to(m256[0], UP, buff=0.08)
        m512 = VGroup(Triangle(color=WHITE, fill_opacity=1).scale(0.12).rotate(PI).next_to(bar.n2p(512), UP, buff=0.05),
                      label("(second) preimage ~2⁵¹²", 22))
        m512[1].next_to(m512[0], UP, buff=0.08).shift(LEFT * 0.8)
        axis_name = label("number of tries, log scale", 20, GREY_B).next_to(bar, DOWN, buff=0.6)
        self.play(Create(bar), FadeIn(ticks), FadeIn(axis_name), FadeIn(m512), FadeIn(m256))
        compare = VGroup(
            VGroup(label("2²⁵⁶ ≈", 34), label("1.2 × 10⁷⁷", 34, YELLOW)).arrange(RIGHT, buff=0.2),
            VGroup(label("atoms in the observable universe ≈", 30, GREY_A), label("10⁸⁰", 34)).arrange(RIGHT, buff=0.2),
        ).arrange(DOWN, buff=0.35, aligned_edge=LEFT).move_to(DOWN * 1.8)
        self.play_at("a number with seventy-eight digits", FadeIn(compare[0]))
        self.play_at("the number of atoms", FadeIn(compare[1]))


class S05InsideTheBlender(NarratedScene):
    episode, number = 3, 5

    def construct(self):
        # Padding and blocks.
        msg = ByteRibbon.text(b"Hello", cell_size=0.5)
        one = VGroup(Square(0.5, color=YELLOW, fill_color=GREY_E, fill_opacity=1), mono("1", 22, YELLOW))
        one[1].move_to(one[0])
        zeros = VGroup(Rectangle(width=3.5, height=0.5, color=GREY_B, fill_color=GREY_E, fill_opacity=1),
                       label("0 0 0 … 0", 20, GREY_B))
        zeros[1].move_to(zeros[0])
        length = VGroup(Rectangle(width=2.6, height=0.5, color=LENGTH_COLOR, fill_color=GREY_E, fill_opacity=1),
                        label("message length (128 bits)", 18, LENGTH_COLOR))
        length[1].move_to(length[0])
        padded = VGroup(msg, one, zeros, length).arrange(RIGHT, buff=0).move_to(UP * 2.2)
        box = RecipeMachine("SHA-512", width=3.6, height=2.2).move_to(UP * 0.3)
        how = label("how does it scramble so thoroughly?", 32, YELLOW).next_to(box, DOWN, buff=0.5)
        self.play(FadeIn(box), FadeIn(how))
        rng = np.random.default_rng(9)
        body = box.body
        wires = VGroup(*[Line(body.get_left() + RIGHT * 0.2 + UP * rng.uniform(-0.8, 0.8),
                              body.get_right() + LEFT * 0.2 + UP * rng.uniform(-0.8, 0.8),
                              color=interpolate_color(TEAL, PURPLE, rng.uniform()), stroke_width=1.5)
                         for _ in range(18)])
        self.play_at("I won't go through every wire", Create(wires, lag_ratio=0.1, run_time=1.5))
        self.play_at("the shape of it is worth seeing", wires.animate.set_opacity(0.25), box.turn())
        self.play_at("First, the input gets padded", FadeOut(VGroup(box, wires, how)), FadeIn(msg))
        self.play(FadeIn(one), FadeIn(zeros))
        self.play_at("writes the length of the message", FadeIn(length, shift=LEFT * 0.3))
        block_brace = Brace(padded, DOWN, color=GREY_B)
        block_text = label("1,024-bit block", 22, GREY_B)
        block_brace.put_at_tip(block_text)
        self.play_at("chopped into blocks", GrowFromCenter(block_brace), FadeIn(block_text))

        # Chaining.
        boxes = VGroup(*[RoundedRectangle(corner_radius=0.15, width=2.0, height=1.0, color=WHITE,
                                          fill_color=GREY_E, fill_opacity=1) for _ in range(3)])
        boxes.arrange(RIGHT, buff=1.4).move_to(DOWN * 0.8)
        names = VGroup(*[label(f"block {k + 1}", 18, GREY_B).next_to(b, UP, buff=0.15) for k, b in enumerate(boxes)])
        compress_labels = VGroup(*[label("compress", 20).move_to(b) for b in boxes])
        state_in = label("start state", 18, GREY_B).next_to(boxes[0], LEFT, buff=0.6)
        arrows = VGroup(Arrow(state_in.get_right(), boxes[0].get_left(), buff=0.1, color=TEAL),
                        *[Arrow(a.get_right(), b.get_left(), buff=0.1, color=TEAL) for a, b in zip(boxes, boxes[1:])])
        final = label("hash", 26, HASH_COLOR).next_to(boxes[-1], RIGHT, buff=0.6)
        arrows.add(Arrow(boxes[-1].get_right(), final.get_left(), buff=0.1, color=HASH_COLOR))
        self.play_at("Each block gets mixed into a running internal state",
                     FadeIn(boxes), FadeIn(compress_labels), FadeIn(names), FadeIn(state_in))
        self.play(LaggedStart(*[GrowArrow(a) for a in arrows[:-1]], lag_ratio=0.4))
        self.play_at("the final state is the hash", GrowArrow(arrows[-1]), FadeIn(final))

        # One round over eight registers.
        self.play_at("The mixing itself is made of humble operations", *[FadeOut(m) for m in self.mobjects])
        regs = VGroup()
        for name in "abcdefgh":
            box = Rectangle(width=1.3, height=0.7, color=WHITE, fill_color=GREY_E, fill_opacity=1)
            regs.add(VGroup(box, mono(name, 30).move_to(box)))
        regs.arrange(RIGHT, buff=0.15).move_to(UP * 1.6)
        bits64 = label("8 registers × 64 bits", 24, GREY_B).next_to(regs, UP, buff=0.25)
        self.play_at("Eight registers of sixty-four bits", FadeIn(regs, lag_ratio=0.1), FadeIn(bits64))
        ops = VGroup(*[tag_box(t, c, 24) for t, c in (
            ("rotate", TEAL), ("XOR", BLUE), ("choose", PURPLE), ("majority", PINK), ("add mod 2⁶⁴", ORANGE))])
        ops.arrange(RIGHT, buff=0.3).move_to(DOWN * 0.2)
        for op, phrase in zip(ops, ["Rotate some bits around", "Combine some with exclusive-or",
                                    "choose bits from two others", "take a majority vote",
                                    "add things together"]):
            self.play_at(phrase, FadeIn(op, shift=UP * 0.2), run_time=0.4)
        feeds = VGroup(Arrow(ops.get_top(), regs[0].get_bottom(), color=ORANGE, buff=0.1),
                       Arrow(ops.get_top(), regs[4].get_bottom(), color=ORANGE, buff=0.1))
        self.play(GrowArrow(feeds[0]), GrowArrow(feeds[1]))
        self.play_at("Then shift everything over by one",
                     *[regs[i][1].animate.move_to(regs[i + 1][0]) for i in range(7)],
                     regs[7][1].animate.set_opacity(0))
        times = label("× 80 rounds per block", 32, YELLOW).to_edge(DOWN, buff=0.8)
        self.play_at("Eighty times per block", Write(times), run_time=0.6)

        # Real diffusion: "Hello" vs "hello", round by round.
        self.play_at("Watch what happens", FadeOut(VGroup(regs, bits64, ops, feeds, times)))
        a, b = round_states(HELLO_UPPER), round_states(HELLO_LOWER)
        grid = DiffusionGrid(cell=0.2).move_to(DOWN * 0.3)
        title = label("state bits that differ: Hello vs hello", 28).next_to(grid, UP, buff=0.3)
        self.play(FadeIn(grid), FadeIn(title))
        readout = label("", 28)
        stops = [(1, "After one round"), (2, None), (4, "A few rounds later"), (8, None), (16, None),
                 (80, "Well before round eighty")]
        for rnd, phrase in stops:
            mask = changed_bits(a[rnd - 1], b[rnd - 1])
            new = label(f"after round {rnd}: {mask.count('1')} of 512 bits differ", 28, YELLOW)
            new.next_to(grid, DOWN, buff=0.3)
            if phrase:
                self.cue(phrase)
                self.play(grid.show_mask(mask, run_time=0.7), Transform(readout, new))
                self.wait(0.3)
            else:
                self.play(grid.show_mask(mask, run_time=0.5), Transform(readout, new), run_time=0.5)
        self.play_at("about half of them flip", Indicate(readout, color=YELLOW))
        self.play_at("the avalanche we saw at the start", Indicate(grid, color=YELLOW, scale_factor=1.02))

        self.play_at("There is no secret anywhere in this", FadeOut(VGroup(grid, title, readout)))
        no_key = VGroup(label("no secret key", 48), label("everything is public", 34, GREY_B)).arrange(DOWN, buff=0.3)
        self.play(FadeIn(no_key))
        public = tag_box("FIPS 180-4 · public since the early 2000s", GREY_B, 24).to_edge(DOWN, buff=0.7)
        self.play_at("The algorithm is published", FadeIn(public, shift=UP * 0.2))
        crowd = VGroup(*[Dot(radius=0.07, color=TEAL) for _ in range(40)]).arrange_in_grid(4, 10, buff=0.15)
        crowd_label = label("decades of cryptographers", 20, TEAL).next_to(crowd, DOWN, buff=0.12)
        VGroup(crowd, crowd_label).to_corner(UR, buff=0.5)
        self.play_at("Its entire security rests", Indicate(no_key, color=YELLOW, scale_factor=1.05))
        self.play_at("decades of the world's cryptographers", FadeIn(crowd, lag_ratio=0.03), FadeIn(crowd_label))
        backwards = label("security = nobody knows how to run it backwards, or steer it", 28, YELLOW)
        backwards.next_to(no_key, DOWN, buff=0.6)
        self.play_at("nobody knows how to run it backwards", Write(backwards))


class S06IdealLedger(NarratedScene):
    episode, number = 3, 6

    def construct(self):
        header = VGroup(label("input", 24, GREY_B), label("512 coin flips", 24, GREY_B)).arrange(RIGHT, buff=3.0)
        title = label("the ideal hash: a gigantic ledger", 34).to_edge(UP, buff=0.4)
        header.next_to(title, DOWN, buff=0.4)
        rng = np.random.default_rng(11)
        inputs = ["Hello", "hello", "Four score and…", "Call me Ishmael…"]
        rows = VGroup()
        for word in inputs:
            bits = "".join(rng.choice(["0", "1"], 28))
            row = VGroup(mono(word, 24), mono(bits + "…", 22, HASH_COLOR))
            row[0].move_to(header[0], aligned_edge=LEFT)
            row[1].move_to(header[1], aligned_edge=LEFT)
            row.shift(DOWN * 0.6 * (len(rows) + 1))
            rows.add(row)
        self.play(FadeIn(title), FadeIn(header))
        self.play_at("Whenever a new input shows up", FadeIn(rows[0][0]))
        coin = Circle(0.35, color=YELLOW, fill_color=YELLOW_E, fill_opacity=1).move_to(rows[0][1].get_left() + RIGHT * 0.5)
        self.play(FadeIn(coin), run_time=0.3)
        self.play(Rotate(coin, angle=4 * PI, axis=UP), run_time=1.0)
        self.play(FadeIn(rows[0][1], lag_ratio=0.1), FadeOut(coin))
        self.play(LaggedStart(*[FadeIn(r) for r in rows[1:]], lag_ratio=0.3))
        repeat = mono("Hello", 24, YELLOW).next_to(rows, DOWN, buff=0.6).align_to(rows[0][0], LEFT)
        self.play_at("If the same input ever comes back", FadeIn(repeat))
        self.play(Indicate(rows[0], color=YELLOW))
        cap = label("perfectly unpredictable, perfectly consistent: the yardstick", 26, GREY_A).to_edge(DOWN, buff=0.5)
        self.play_at("Perfectly unpredictable", FadeIn(cap))

        ledger = VGroup(title, header, rows, repeat)
        self.play_at("No real function can be that ledger", FadeOut(cap), ledger.animate.scale(0.55).to_edge(LEFT, buff=0.3))
        sha = RecipeMachine("SHA-512", width=2.6, height=1.5).to_edge(RIGHT, buff=0.8)
        eq = DashedLine(ledger.get_right() + RIGHT * 0.2, sha.get_left() + LEFT * 0.2, color=GREY_B)
        eq_label = label("indistinguishable\n(as far as anyone knows)", 22, GREY_B).next_to(eq, DOWN, buff=0.2)
        self.play_at("indistinguishable from it", FadeIn(sha), Create(eq), FadeIn(eq_label))
        quoted = VGroup(*[tag_box(t, YELLOW, 22) for t in ("preimage ~2⁵¹²", "second preimage ~2⁵¹²", "collision ~2²⁵⁶")])
        quoted.arrange(RIGHT, buff=0.3).move_to(DOWN * 1.7)
        self.play_at("The costs I quoted for the three games", LaggedStart(*[FadeIn(t, shift=UP * 0.1) for t in quoted],
                                                                           lag_ratio=0.25))
        self.play_at("what the ideal ledger would give you", Indicate(ledger, color=YELLOW, scale_factor=1.03))
        self.play_at("When real hash functions have failed", FadeOut(quoted))

        olds = VGroup(*[RecipeMachine(n, width=2.0, height=1.1) for n in ("MD5", "SHA-1")]).arrange(RIGHT, buff=0.6)
        olds.to_edge(DOWN, buff=0.6)
        cracks = VGroup()
        for m in olds:
            c = VMobject(color=RED, stroke_width=4).set_points_as_corners(
                [m.body.get_top() + LEFT * 0.2, m.body.get_center() + RIGHT * 0.1,
                 m.body.get_center() + DOWN * 0.2 + LEFT * 0.15, m.body.get_bottom() + RIGHT * 0.2])
            cracks.add(c)
        ep5 = label("Episode 5", 22, RED).next_to(olds, RIGHT, buff=0.4)
        self.play_at("someone found a way to tell them apart", FadeIn(olds))
        self.play_at("That's what happened to the older functions", Create(cracks), FadeIn(ep5))
        self.play_at("MD5 and SHA-1", LaggedStart(*[Indicate(m, color=RED) for m in olds], lag_ratio=0.4))
        masked = mask_icon(ADVERSARY_COLOR, 0.5).next_to(ep5, RIGHT, buff=0.2)
        self.play_at("the episode on adversaries", FadeIn(masked, scale=0.5), Indicate(ep5, color=RED))


class S07WhatTwoFiftySixNeeds(NarratedScene):
    episode, number = 3, 7

    def construct(self):
        cards = VGroup(
            game_card(1, "Preimage", "a fingerprint alone", "anyone with that print", "~2⁵¹²"),
            game_card(2, "Second preimage", "Alice", "someone else matching Alice", "~2⁵¹²"),
            game_card(3, "Collision", "nothing", "any two people who match", "~2²⁵⁶"),
        ).arrange(RIGHT, buff=0.25).scale(0.8).move_to(UP * 0.6)
        self.play(FadeIn(cards))
        not_needed = label("not needed: CIDs aren't secret", 22, GREY_B).next_to(cards[0], DOWN, buff=0.2)
        self.play_at("Not the first one", cards[0].animate.set_opacity(0.3), FadeIn(not_needed))
        hello = CIDText(compute_cid(b"Hello"), size=34)
        recover = VGroup(hello, Arrow(LEFT * 0.5, RIGHT * 0.5, color=GREY_B), mono("Hello", 34, INLINE_COLOR))
        recover.arrange(RIGHT, buff=0.3).move_to(DOWN * 2.5)
        self.play_at("the CID literally contains the content", FadeIn(recover[0]))
        fine = label("trivial to recover, and that's fine", 22, GREY_B).next_to(recover, DOWN, buff=0.15)
        self.play_at("recovering the input is trivial", GrowArrow(recover[1]), FadeIn(recover[2]), FadeIn(fine))

        # Warning: a name, not a lock.
        warn_frame = RoundedRectangle(corner_radius=0.2, width=12.5, height=5.6, stroke_color=RED,
                                      fill_color=BLACK, fill_opacity=0.95)
        warn_title = label("a CID does not hide its content", 34, RED).move_to(warn_frame.get_top() + DOWN * 0.5)
        self.play_at("a CID is a name, not a lock", FadeOut(VGroup(recover, fine)), FadeIn(warn_frame), FadeIn(warn_title))
        secret = CIDText(compute_cid(b"hunter2"), size=40).move_to(UP * 1.2)
        decoded = VGroup(label("decodes to", 24, GREY_B), mono("hunter2", 34, INLINE_COLOR)).arrange(RIGHT, buff=0.3)
        decoded.next_to(secret, DOWN, buff=0.2)
        self.play_at("If the secret is short", FadeIn(secret))
        self.play(FadeIn(decoded))

        target = CIDText(compute_cid(BALLOT + b"REJECT"), size=22, max_width=8)
        target_row = VGroup(label("target:", 22, GREY_B), target).arrange(RIGHT, buff=0.3).move_to(DOWN * 0.6)
        template = label('"Board ballot #2026-117. … Final answer: ______"', 20, GREY_A).next_to(target_row, DOWN, buff=0.2)
        self.play_at("anyone who can guess the possibilities", FadeIn(target_row), FadeIn(template))
        guesses = VGroup()
        for word in (b"APPROVE", b"REJECT", b"ABSTAIN"):
            cid = compute_cid(BALLOT + word)
            ok = cid == target.cid
            row = VGroup(mono(word.decode(), 22, WHITE), mono(cid[:20] + "…", 20, HASH_COLOR),
                         (Check(0.3) if ok else XMark(0.25))).arrange(RIGHT, buff=0.3)
            guesses.add(row)
        guesses.arrange(DOWN, aligned_edge=LEFT, buff=0.15).next_to(template, DOWN, buff=0.25)
        self.play_at("compute the CID of each guess", LaggedStart(*[FadeIn(g) for g in guesses], lag_ratio=0.5))

        self.play_at("What 256t does rely on", FadeOut(VGroup(warn_frame, warn_title, secret, decoded,
                                                                  target_row, template, guesses)))
        needed = VGroup(*[label("needed ✓", 24, HASH_COLOR).next_to(c, DOWN, buff=0.2) for c in cards[1:]])
        self.play(FadeIn(needed[0]))
        self.play_at("the second and third games", *[Indicate(c, color=HASH_COLOR, scale_factor=1.03) for c in cards[1:]])
        self.play_at("take content you already have", Indicate(cards[1], color=YELLOW, scale_factor=1.05))
        self.play_at("And it needs that nobody can produce two", FadeIn(needed[1]))
        self.play_at("produce two different pieces of content", Indicate(cards[2], color=YELLOW, scale_factor=1.05))
        true_enough = caption('these make "this CID means these bytes" true enough', 28, YELLOW)
        self.play_at("Those are the properties", FadeIn(true_enough))
        self.play_at("true enough", Indicate(true_enough, color=YELLOW, scale_factor=1.05))

        self.play_at("comes down to one sentence", FadeOut(VGroup(cards, needed, not_needed, true_enough)))
        left = VGroup(label("identity function", 34, INLINE_COLOR), label("≤ 64 bytes", 26, GREY_B),
                      label("perfect where possible", 28)).arrange(DOWN, buff=0.25)
        right = VGroup(label("SHA-512", 34, HASH_COLOR), label("> 64 bytes", 26, GREY_B),
                       label("practically perfect where necessary", 28)).arrange(DOWN, buff=0.25)
        VGroup(left, right).arrange(RIGHT, buff=1.4)
        self.play_at("Where it can be perfect", FadeIn(left))
        self.play_at("And where it can't be perfect", FadeIn(right))
        facts = VGroup(label("collisions: everywhere", 24, GREY_A), label("ever found: none", 24, GREEN)).arrange(DOWN, buff=0.15)
        facts.next_to(right, DOWN, buff=0.35)
        self.play_at("whose collisions are everywhere", FadeIn(facts[0], shift=UP * 0.1))
        self.play_at("no one has ever been able to find", FadeIn(facts[1], shift=UP * 0.1))
        die = dice_icon(YELLOW, 0.7).to_edge(DOWN, buff=1.5)
        self.play_at("Which raises the next question", FadeIn(die), run_time=0.3)
        self.play(Rotate(die, angle=TAU), run_time=1.0)
        dice = label("next: what are the odds of an accident?", 30, YELLOW).to_edge(DOWN, buff=0.6)
        self.play_at("what are the odds", FadeIn(dice))
