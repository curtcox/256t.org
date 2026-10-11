"""Episode 1 — Where vs. What: Names That Can't Break.

Scenes follow video-series/ep01-where-vs-what.md. Render with:
    python3 video-series/manim/render.py 1
"""

from common import *


PAGE_ROWS = [
        "The committee relied on several published reports",
        "and a widely shared dataset, available online —",
        None,
        "which documents the figures in question in detail.",
]


def page_lines(link_text: str = "see the original source") -> tuple[VGroup, Text]:
    """A paragraph of placeholder body text with one hyperlink in it."""
    lines = VGroup()
    link = None
    for row in PAGE_ROWS:
        if row is None:
            before = label("for details,", 30, GREY_B)
            link = label(link_text, 30, BLUE)
            underline = Underline(link, color=BLUE, buff=0.05)
            line = VGroup(before, VGroup(link, underline)).arrange(RIGHT, buff=0.18)
        else:
            line = label(row, 30, GREY_B)
        lines.add(line)
    lines.arrange(DOWN, aligned_edge=LEFT, buff=0.3)
    return lines, link


def cursor() -> VMobject:
    arrow = Polygon(ORIGIN, DOWN * 0.5, DOWN * 0.38 + RIGHT * 0.12, DOWN * 0.55 + RIGHT * 0.25,
                    DOWN * 0.5 + RIGHT * 0.3, DOWN * 0.33 + RIGHT * 0.18, DOWN * 0.33 + RIGHT * 0.35,
                    color=WHITE, fill_color=WHITE, fill_opacity=1, stroke_width=1)
    return arrow


def not_found_card() -> VGroup:
    frame = RoundedRectangle(corner_radius=0.2, width=7, height=3.2, stroke_color=GREY_B,
                             fill_color=GREY_E, fill_opacity=1)
    big = label("404", 96, GREY_A)
    small = label("Not Found", 40, GREY_B)
    return VGroup(frame, VGroup(big, small).arrange(DOWN, buff=0.1).move_to(frame))


def house(color=TEAL, width: float = 1.4) -> VGroup:
    walls = Square(width, color=color, fill_color=color, fill_opacity=0.35)
    roof = Polygon(walls.get_corner(UL) + LEFT * 0.15, walls.get_corner(UR) + RIGHT * 0.15,
                   walls.get_top() + UP * width * 0.55, color=color, fill_color=color, fill_opacity=0.6)
    door = Rectangle(width=width * 0.22, height=width * 0.4, color=color, fill_color=BLACK,
                     fill_opacity=1).align_to(walls, DOWN)
    return VGroup(walls, roof, door)


def map_pin(color=RED) -> VGroup:
    head = Circle(0.3, color=color, fill_color=color, fill_opacity=1)
    tip = Polygon(head.get_left() + DOWN * 0.08, head.get_right() + DOWN * 0.08, head.get_bottom() + DOWN * 0.5,
                  color=color, fill_color=color, fill_opacity=1, stroke_width=0)
    hole = Dot(head.get_center(), radius=0.1, color=BLACK)
    return VGroup(tip, head, hole)


def street_grid() -> VGroup:
    lines = VGroup()
    for x in np.arange(-6, 6.5, 1.5):
        lines.add(Line(UP * 3.5 + RIGHT * x, DOWN * 3.5 + RIGHT * x, stroke_width=1, color=GREY_D))
    for y in np.arange(-3, 3.5, 1.5):
        lines.add(Line(LEFT * 7 + UP * y, RIGHT * 7 + UP * y, stroke_width=1, color=GREY_D))
    return lines


def person(color) -> VGroup:
    head = Circle(0.18, color=color, fill_color=color, fill_opacity=1)
    body = Arc(radius=0.32, start_angle=0, angle=PI, color=color, fill_color=color,
               fill_opacity=1).next_to(head, DOWN, buff=0.05)
    return VGroup(head, body)


class S01ColdOpen(NarratedScene):
    episode, number = 1, 1

    def construct(self):
        lines, link = page_lines()
        lines.move_to(UP * 0.3)
        pointer = cursor().move_to(DOWN * 3 + RIGHT * 4)
        self.play(FadeIn(lines, lag_ratio=0.1), FadeIn(pointer))
        self.play(pointer.animate.move_to(link.get_center() + DOWN * 0.25 + RIGHT * 0.1), run_time=1.2)
        self.play(Indicate(link, color=BLUE_A), pointer.animate.scale(0.85), run_time=0.4)
        card404 = not_found_card()
        self.play_at("Page not found", FadeOut(lines), FadeOut(pointer), FadeIn(card404), run_time=0.4)

        stat1 = card("~38% gone", "pages from 2013, unreachable by 2023", width=5.4, height=1.9)
        src1 = label("Pew Research Center, 2024", 18, GREY_B)
        stat2 = card("~50% broken", "links in U.S. Supreme Court opinions", width=5.4, height=1.9)
        src2 = label("Zittrain, Albert & Lessig, Harv. L. Rev. Forum, 2014", 18, GREY_B)
        group1 = VGroup(stat1, src1).arrange(DOWN, buff=0.12)
        group2 = VGroup(stat2, src2).arrange(DOWN, buff=0.12)
        stats = VGroup(group1, group2).arrange(DOWN, buff=0.5).to_edge(LEFT, buff=0.8)
        self.play_at("In 2024, Pew Research", card404.animate.scale(0.6).to_edge(RIGHT, buff=1.0),
                     FadeIn(group1, shift=RIGHT))
        self.play_at("A decade earlier", FadeIn(group2, shift=RIGHT))

        # Rewind: the link works again, but the page's words quietly change.
        self.play_at("And there is a quieter failure", FadeOut(stats), FadeOut(card404))
        lines, link = page_lines()
        lines.move_to(UP * 0.3)
        self.play(FadeIn(lines), run_time=0.6)
        swaps = [(0, "several published reports", "two internal summaries"),
                 (1, "a widely shared dataset", "a revised dataset")]
        self.cue("The page was edited")
        for row, old, new in swaps:
            old_line = lines[row]
            new_line = label(PAGE_ROWS[row].replace(old, new), 30, YELLOW).move_to(old_line, aligned_edge=LEFT)
            self.play(Transform(old_line, new_line), run_time=0.8)
        last = lines[3]
        self.play(Transform(last, label("which supports a rather different conclusion.", 30, YELLOW)
                            .move_to(last, aligned_edge=LEFT)), run_time=0.8)
        tag = caption("same address, different content", 34, YELLOW)
        self.play_at("Same address, different content", FadeIn(tag, shift=UP))

        self.play_at("This series is about", FadeOut(lines), FadeOut(tag))
        title = title_card("Where vs. What", "Names that can't break")
        site = mono("256t.org", 30, LENGTH_COLOR).next_to(title, DOWN, buff=0.6)
        self.play(Write(title[0]), FadeIn(title[1], shift=UP * 0.2))
        self.play_at("It's written up at a site called 256t.org", FadeIn(site, shift=UP * 0.2))


class S02WhatAURLSays(NarratedScene):
    episode, number = 1, 2

    def construct(self):
        parts = ["https://", "example.com", "/reports/2013/", "annual.pdf"]
        url = VGroup(*[mono(p, 40) for p in parts]).arrange(RIGHT, buff=0.02, aligned_edge=DOWN)
        fit_width(url, FRAME_W - 1.5).move_to(UP * 1)
        self.play(Write(url), run_time=1.5)

        meanings = ["which machine", "which folder", "which name in that folder"]
        orders = ["go here", "then here", "then ask for this"]
        braces, notes = VGroup(), VGroup()
        for piece, meaning in zip(url[1:], meanings):
            brace = Brace(piece, DOWN, color=GREY_B)
            note = label(meaning, 22, YELLOW)
            brace.put_at_tip(note)
            braces.add(brace)
            notes.add(note)
        cues = ["This part says which computer", "This part says which folder", "And this last part"]
        for brace, note, phrase in zip(braces, notes, cues):
            self.play_at(phrase, GrowFromCenter(brace), FadeIn(note, shift=DOWN * 0.2))

        self.cue("Go here")
        for note, order in zip(notes, orders):
            new = label(order, 24, ORANGE).move_to(note)
            self.play(Transform(note, new), run_time=0.5)

        # The URL becomes a map pin over a house.
        grid = street_grid()
        pin = map_pin().move_to(UP * 1.4)
        home = house().move_to(DOWN * 0.6)
        self.play_at("That's a street address",
                     FadeOut(braces), FadeOut(notes),
                     ReplacementTransform(url, pin), FadeIn(grid))
        self.play(FadeIn(home, shift=UP * 0.3))
        self.play_at("It's a terrible way to identify one", Indicate(home, color=YELLOW))

        lot = DashedVMobject(Rectangle(width=1.8, height=0.6, color=GREY_B), num_dashes=24)
        lot.move_to(home.get_bottom() + UP * 0.3)
        rot = label("link rot", 34, RED).next_to(lot, DOWN, buff=0.4)
        self.play_at("If the house is torn down",
                     home.animate.scale(0.01).set_opacity(0).move_to(home.get_bottom()), run_time=0.9)
        self.play(Create(lot), FadeIn(rot))
        new_home = house(color=ORANGE, width=1.6).move_to(home.get_center() + UP * 0.1)
        drift = label("content drift", 34, YELLOW).next_to(new_home, DOWN, buff=0.3)
        self.play_at("And if a different house",
                     FadeOut(lot), GrowFromEdge(new_home, DOWN), Transform(rot, drift))
        self.play_at("The address never promised", Indicate(pin, scale_factor=1.3))


class S03NamingTheThing(NarratedScene):
    episode, number = 1, 3

    def construct(self):
        divider = Line(UP * 3.5, DOWN * 3.5, color=GREY_D)
        where = label("Where?", 44).move_to(LEFT * 3.5 + UP * 2.8)
        what = label("What?", 44).move_to(RIGHT * 3.5 + UP * 2.8)
        rack = ServerRack().move_to(LEFT * 4.6 + UP * 0.2)
        tree = VGroup(*[label(t, 22, GREY_B) for t in ("/reports", "  /2013", "    annual.pdf")])
        tree.arrange(DOWN, aligned_edge=LEFT, buff=0.15).next_to(rack, RIGHT, buff=0.4)
        ribbon = ByteRibbon.text(b"Four score", cell_size=0.38).move_to(RIGHT * 3.5)
        self.play(FadeIn(divider), FadeIn(where), FadeIn(what), FadeIn(rack), FadeIn(tree),
                  FadeIn(ribbon), run_time=0.8)

        self.play_at("What if, instead of naming",
                     FadeOut(VGroup(divider, where, rack, tree)),
                     what.animate.move_to(UP * 2.8),
                     ribbon.animate.move_to(LEFT * 4.3 + UP * 0.3))
        words = label('"annual report, 2013"', 30, GREY_B).move_to(DOWN * 2)
        self.play_at("I don't mean a description in words", FadeIn(words))
        strike = Line(words.get_left(), words.get_right(), color=RED, stroke_width=5)
        self.play_at("I mean a name computed directly", Create(strike))
        machine = RecipeMachine().move_to(RIGHT * 0.6 + UP * 0.3)
        tag = mono(GETTYSBURG_CID[:12] + "…", 28, HASH_COLOR)
        self.play_at("by a fixed, public recipe", FadeIn(machine), FadeOut(words), FadeOut(strike))
        self.play_at("Feed the bytes in", feed_through(machine, ribbon, tag))
        equation = label("name = f(content)", 40, YELLOW).to_edge(DOWN, buff=0.8)
        self.play(Write(equation))

        # Three people, three machines, one name.
        self.play_at("The crucial property", FadeOut(VGroup(machine, tag, what)), equation.animate.to_edge(UP))
        colors = [TEAL, PINK, ORANGE]
        rows = VGroup()
        for i, color in enumerate(colors):
            who = person(color)
            rib = ByteRibbon.text(b"Four score", cell_size=0.28)
            mini = RecipeMachine("recipe", width=1.6, height=0.9)
            out = mono(GETTYSBURG_CID[:12] + "…", 22, HASH_COLOR)
            row = VGroup(who, rib, mini, out).arrange(RIGHT, buff=0.45)
            rows.add(row)
        rows.arrange(DOWN, buff=0.6).move_to(DOWN * 0.3)
        for row in rows:
            row[3].set_opacity(0)
        self.play_at("So anyone, anywhere", LaggedStart(*[FadeIn(r[:3]) for r in rows], lag_ratio=0.3))
        self.play(*[r[2].turn(run_time=1.0) for r in rows])
        self.play(*[r[3].animate.set_opacity(1) for r in rows])
        merged = mono(GETTYSBURG_CID[:12] + "…", 30, HASH_COLOR).move_to(RIGHT * 4.5 + DOWN * 0.3)
        self.play_at("without ever talking to each other",
                     *[Transform(r[3], merged.copy()) for r in rows], run_time=1.2)

        self.play_at("This idea is called content addressing",
                     FadeOut(rows), FadeOut(equation),
                     FadeIn(label("content addressing", 60, YELLOW)))
        nots = VGroup(*[label(t, 28, GREY_B) for t in ("no machine", "no folders", "no directions")])
        nots.arrange(RIGHT, buff=1.0).move_to(DOWN * 1.5)
        for phrase, item in zip(("It says nothing about which machine", "Nothing about folders",
                                 "It's not directions"), nots):
            self.play_at(phrase, FadeIn(item, shift=UP * 0.2), run_time=0.5)
        fp = FingerprintIcon(size=1.2).move_to(DOWN * 1.5)
        self.play_at("It's more like a precise description", FadeOut(nots), FadeIn(fp, scale=0.5))


class S04WhatYouGetForFree(NarratedScene):
    episode, number = 1, 4

    def panel(self, n: int, title: str) -> VGroup:
        frame = RoundedRectangle(corner_radius=0.2, width=6.3, height=2.75, stroke_color=GREY_B,
                                 fill_color=GREY_E, fill_opacity=0.6)
        badge = VGroup(Circle(0.22, color=YELLOW, fill_color=BLACK, fill_opacity=1), label(str(n), 22, YELLOW))
        head = VGroup(badge, label(title, 28)).arrange(RIGHT, buff=0.2)
        head.move_to(frame.get_top() + DOWN * 0.38)
        return VGroup(frame, head)

    def construct(self):
        panels = VGroup(*[self.panel(i + 1, t) for i, t in enumerate(
            ["Any copy will do", "You can check", "Caching is easy", "No silent edits"])])
        panels.arrange_in_grid(2, 2, buff=(0.3, 0.25)).move_to(UP * 0.15)
        heading = label("For free:", 32, GREY_B).to_edge(UP, buff=0.2)
        self.play(FadeIn(heading))

        # 1. Any copy will do.
        p = panels[0]
        tag = mono("AAAAAAXnpY8K…", 20, HASH_COLOR).move_to(p[0].get_center() + UP * 0.35)
        holders = VGroup(ServerRack(height=0.7), ServerRack(height=0.7),
                         label("laptop", 18), label("USB stick", 18), label("cache", 18))
        holders.arrange(RIGHT, buff=0.35).move_to(p[0].get_center() + DOWN * 0.85)
        arrows = VGroup(*[Arrow(tag.get_bottom(), h.get_top(), buff=0.08, stroke_width=2,
                                max_tip_length_to_length_ratio=0.15, color=GREY_B) for h in holders])
        self.play_at("First: any copy will do", FadeIn(p), FadeIn(tag))
        self.play(LaggedStart(*[GrowArrow(a) for a in arrows], lag_ratio=0.15),
                  LaggedStart(*[FadeIn(h) for h in holders], lag_ratio=0.15))
        self.play_at("because it is the original", Indicate(holders, color=GREEN))

        # 2. You can check.
        p = panels[1]
        mini = VerifierLoop(width=4.6, steps=("ask", "receive", "compute", "compare"),
                            station_width=1.0, font_size=18, verdict_side=DOWN)
        mini.move_to(p[0].get_center() + DOWN * 0.05)
        self.play_at("Second, and this is the one", FadeIn(p), FadeIn(mini))
        self.play_at("you run the recipe yourself", mini.run(ok=True))
        self.play_at("even if the bytes came from a stranger", Indicate(mini.last_verdict))
        trust = label("trust: messenger → math", 22, YELLOW).next_to(mini.stations, DOWN, buff=0.35).shift(LEFT * 0.8)
        self.play_at("Trust moves away from the messenger", FadeIn(trust))

        # 3. Caching is easy.
        p = panels[2]
        cache = CacheBox(size=1.2).move_to(p[0].get_center() + LEFT * 1.3 + DOWN * 0.3)
        clock_face = Circle(0.55, color=GREY_B).move_to(p[0].get_center() + RIGHT * 1.4 + DOWN * 0.3)
        hand = Line(clock_face.get_center(), clock_face.get_center() + UP * 0.45, color=WHITE)
        stale = label("stale?", 24, RED).next_to(cache, RIGHT, buff=0.2)
        self.play_at("Third, caching gets easy", FadeIn(p), FadeIn(cache), FadeIn(clock_face), FadeIn(hand))
        self.play_at("is this still current", FadeIn(stale))
        self.play_at("that question disappears",
                     Rotate(hand, angle=-6 * TAU, about_point=clock_face.get_center(), run_time=2),
                     FadeOut(stale))

        # 4. No silent edits.
        p = panels[3]
        v1 = VGroup(label("v1", 22), ByteRibbon.text(b"report", cell_size=0.3),
                    mono("AAAAAABk3x…", 18, HASH_COLOR)).arrange(RIGHT, buff=0.25)
        v1.move_to(p[0].get_center() + UP * 0.2)
        self.play_at("And fourth, nothing can change", FadeIn(p), FadeIn(v1))
        v2 = VGroup(label("v2", 22), ByteRibbon.text(b"rePort", cell_size=0.3).tint([2], YELLOW),
                    mono("AAAAAABkQ7…", 18, HASH_COLOR)).arrange(RIGHT, buff=0.25)
        v2.next_to(v1, DOWN, buff=0.3)
        self.play_at("They've made a new file", TransformFromCopy(v1, v2))
        self.play_at("Updates don't overwrite history", Indicate(VGroup(v1, v2), color=YELLOW))

        # The flip side: "latest" is a pointer, a different problem.
        latest = tag_box("the latest version?", YELLOW).next_to(panels, DOWN, buff=0.1)
        self.play_at("Now, that last point has a flip side",
                     VGroup(panels[:3], cache, clock_face, hand).animate.set_opacity(0.15))
        latest.move_to(panels[2].get_center())
        self.play(FadeIn(latest))
        pointer_arrow = Arrow(latest.get_right(), v2.get_left(), color=YELLOW, buff=0.1)
        note = label("pointer (different problem)", 22, YELLOW).next_to(latest, UP, buff=0.15)
        self.play_at("you need a separate kind of pointer", GrowArrow(pointer_arrow), FadeIn(note))
        bottom = caption("this series: the bottom layer — a name for some bytes that can't break", 26, GREEN)
        bottom.to_edge(DOWN, buff=0.15)
        self.play_at("The scheme we're looking at", FadeIn(bottom, shift=UP))


class S05DesigningTheRecipe(NarratedScene):
    episode, number = 1, 5

    def construct(self):
        wants = ["short, fixed size", "same bytes → same name", "different bytes → different names",
                 "hard to fake"]
        checklist = VGroup(*[label("☐  " + w, 24) for w in wants]).arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        heading = label("What we want from f", 30, YELLOW)
        board = VGroup(heading, checklist).arrange(DOWN, aligned_edge=LEFT, buff=0.35).to_edge(RIGHT, buff=0.4)
        self.play_at("Let's write down what we want", FadeIn(heading))
        for phrase, item in zip(["The name should be short", "The same bytes should always",
                                 "Different bytes should give", "And it should be hard"], checklist):
            self.play_at(phrase, FadeIn(item, shift=LEFT * 0.2), run_time=0.5)

        stage = LEFT * 2.6
        attempt = label("f = the content itself", 30).move_to(stage + UP * 2.6)
        hello = ByteRibbon.text(b"Hello").move_to(stage + UP * 0.8)
        out = mono("Hello", 36, INLINE_COLOR).move_to(stage + DOWN * 0.5)
        self.play_at("The laziest possible recipe", FadeIn(attempt), FadeIn(hello))
        self.play(TransformFromCopy(hello, out))
        ok = Check(0.5).next_to(out, RIGHT)
        self.play_at("Same bytes, same name", GrowFromCenter(ok))
        movie = ByteRibbon.text(b"A long time ago, in a galaxy far, far away....", cell_size=0.35)
        movie.next_to(out, DOWN, buff=0.6).align_to(out, LEFT)
        self.play_at("but a movie makes a name", FadeIn(movie))
        self.play(movie.animate.shift(LEFT * (movie.width - 2)), run_time=1.5, rate_func=linear)
        note = VGroup(Square(1.9, fill_color=YELLOW_D, fill_opacity=1, stroke_width=0),
                      fit_width(label("perfect… for\ntiny things.\nRemember this.", 22, BLACK), 1.6))
        note[1].move_to(note[0])
        note.rotate(0.08).to_corner(DL, buff=0.4)
        self.play_at("But keep it in the back of your mind",
                     FadeOut(VGroup(hello, out, ok, movie)), FadeIn(note, shift=UP, scale=1.2))

        def retitle(title, phrase):
            self.play_at(phrase, Transform(attempt, label(title, 30).move_to(attempt)))

        # Length.
        retitle("f = length", "What about the length")
        a = ByteRibbon.text(b"Four score and seven", cell_size=0.28, max_cells=10)
        b = ByteRibbon.text(b"Call me Ishmael. Some", cell_size=0.28, max_cells=10)
        pair = VGroup(a, b).arrange(DOWN, buff=0.3).move_to(stage + UP * 0.9)
        names = VGroup(*[mono("1511", 30, YELLOW).next_to(r, RIGHT, buff=0.5) for r in pair])
        self.play(FadeIn(pair), FadeIn(names))
        x1 = XMark(0.5).next_to(pair, DOWN, buff=0.3)
        self.play_at("lots of different files share a length", GrowFromCenter(x1))

        # First bytes.
        retitle("f = first 8 bytes", "What about the first few bytes")
        sig = bytes.fromhex("89504E470D0A1A0A")
        pngs = VGroup(*[ByteRibbon.hex(sig, cell_size=0.42) for _ in range(3)]).arrange(DOWN, buff=0.2)
        pngs.move_to(stage + UP * 0.6)
        self.play(FadeOut(VGroup(pair, names, x1)), FadeIn(pngs))
        png_note = label("every PNG file starts this way", 22, GREY_B).next_to(pngs, DOWN, buff=0.2)
        self.play_at("Every PNG image in the world", FadeIn(png_note), *[Indicate(r, color=RED) for r in pngs])

        # Checksum.
        retitle("f = sum of bytes mod 256", "What about a simple checksum")
        data = b"secret"
        rib = ByteRibbon.text(data, cell_size=0.6).move_to(stage + UP * 0.8)
        total = mono(f"sum = {sum(data) % 256}", 30, YELLOW).next_to(rib, DOWN, buff=0.4)
        self.play(FadeOut(VGroup(pngs, png_note)), FadeIn(rib), FadeIn(total))
        c1, c2 = rib.cells[1], rib.cells[4]
        self.play_at("Swap two bytes", c1.animate.move_to(c2.get_center()), c2.animate.move_to(c1.get_center()),
                     run_time=1.0)
        x2 = XMark(0.5).next_to(total, RIGHT, buff=0.4)
        self.play(Indicate(total), GrowFromCenter(x2))

        # A real hash.
        retitle("f = a cryptographic hash", "What we actually want")
        machine = RecipeMachine("hash", icon=FingerprintIcon(), width=2.6, height=1.6).move_to(stage + UP * 0.6)
        h1 = mono(sha512_b64(b"Hello")[:20] + "…", 24, HASH_COLOR).next_to(machine, DOWN, buff=0.4)
        self.play(FadeOut(VGroup(rib, total, x2)), FadeIn(machine))
        self.play(machine.turn(), FadeIn(h1, shift=DOWN * 0.2))
        flip = label("flip one bit of the input", 22, GREY_B).next_to(machine, LEFT, buff=0.3)
        h2 = mono(sha512_b64(b"hello")[:20] + "…", 24, HASH_COLOR).move_to(h1)
        self.play_at("changing a single bit", FadeIn(flip), machine.turn(run_time=0.8))
        self.play(Transform(h1, h2), run_time=0.8)
        self.play_at("That's a cryptographic hash function",
                     *[Transform(item, label("☑  " + w, 24, GREEN).move_to(item, aligned_edge=LEFT))
                       for item, w in zip(checklist, wants)])
        star = label("*", 36, YELLOW).next_to(checklist[2], RIGHT, buff=0.1)
        self.play(FadeIn(star))
        self.play_at("think of it as a fingerprint for data", Indicate(machine))
        self.play_at("And that asterisk", star.animate.scale(2.2).set_color(RED))


class S06Meet256t(NarratedScene):
    episode, number = 1, 6

    def construct(self):
        title = label("256t", 80, LENGTH_COLOR)
        self.play(Write(title))
        self.play_at("The name it produces is called a CID", title.animate.scale(0.5).to_edge(UP))
        name = label("CID — content identifier", 40).move_to(UP * 1.5)
        rule = label("at most 94 characters · A–Z a–z 0–9 - _", 28, GREY_B).next_to(name, DOWN, buff=0.3)
        self.play(FadeIn(name))
        self.play_at("at most ninety-four characters", FadeIn(rule))
        places = VGroup(*[tag_box(t, GREY_B, 22) for t in ("end of a URL", "a filename", "a database column")])
        places.arrange(RIGHT, buff=0.5).move_to(DOWN * 0.5)
        self.play_at("So it can sit at the end of a URL", LaggedStart(*[FadeIn(p) for p in places], lag_ratio=0.4))

        self.play_at("Here's a real one", FadeOut(VGroup(name, rule, places)))
        host = mono("https://256t.org/", 30, GREY_B).move_to(UP * 1.4).to_edge(LEFT, buff=0.5)
        cid = CIDText(GETTYSBURG_CID, size=30, colored=False, max_width=FRAME_W - 0.8)
        cid.next_to(host, DOWN, buff=0.25).to_edge(LEFT, buff=0.4)
        inset = VGroup(label("Gettysburg Address", 26), label("1,511 bytes", 22, GREY_B)).arrange(DOWN)
        inset.move_to(DOWN * 2.2)
        self.play(AddTextLetterByLetter(cid.text), run_time=1.2)
        self.play_at("fifteen hundred and eleven bytes", FadeIn(inset))
        self.play_at("Put it after 256t.org", FadeIn(host, shift=RIGHT))

        brace_all = Brace(cid, DOWN, color=GREY_B)
        self.play_at("A CID has two parts", FadeOut(inset), GrowFromCenter(brace_all))
        self.play(FadeOut(brace_all))
        b1 = Brace(cid.prefix, DOWN, color=LENGTH_COLOR)
        t1 = label("length", 26, LENGTH_COLOR)
        b1.put_at_tip(t1)
        self.play_at("The first eight characters", cid.prefix.animate.set_color(LENGTH_COLOR),
                     GrowFromCenter(b1), FadeIn(t1))
        b2 = Brace(cid.suffix, DOWN, color=HASH_COLOR)
        t2 = label("SHA-512 fingerprint", 26, HASH_COLOR)
        b2.put_at_tip(t2)
        self.play_at("The rest is a SHA-512 fingerprint", cid.suffix.animate.set_color(HASH_COLOR),
                     GrowFromCenter(b2), FadeIn(t2))

        note = VGroup(Square(1.6, fill_color=YELLOW_D, fill_opacity=1, stroke_width=0),
                      label("Remember\nthis.", 22, BLACK))
        note[1].move_to(note[0])
        note.rotate(0.08).to_corner(DL, buff=0.4)
        self.play_at("here's that sticky note coming back", FadeIn(note, shift=UP))
        hello = CIDText(compute_cid(b"Hello"), size=44).move_to(DOWN * 2.2 + RIGHT * 1.2)
        hb = Brace(hello.suffix, DOWN, color=INLINE_COLOR)
        ht = label("the content itself", 24, INLINE_COLOR)
        hb.put_at_tip(ht)
        self.play(note.animate.scale(0.6).next_to(hello, LEFT, buff=0.6))
        self.play_at("Here's the CID for the word", Write(hello))
        self.play(GrowFromCenter(hb), FadeIn(ht))

        self.play_at("And the rule for a server", *[FadeOut(m) for m in self.mobjects if m is not title])
        client = label("you", 30).move_to(LEFT * 5)
        rack = ServerRack(height=1.8)
        req = Arrow(client.get_right(), rack.get_left(), buff=0.3, color=GREY_B)
        req_label = mono(GETTYSBURG_CID[:10] + "…", 22, HASH_COLOR).next_to(req, UP, buff=0.1)
        good = VGroup(ByteRibbon.text(b"Four score", cell_size=0.3), Check(0.4)).arrange(RIGHT, buff=0.2)
        bad = VGroup(not_found_card().scale(0.25))
        outcomes = VGroup(good, label("or", 24, GREY_B), bad).arrange(DOWN, buff=0.3).move_to(RIGHT * 4.5)
        self.play(FadeIn(client), FadeIn(rack), GrowArrow(req), FadeIn(req_label))
        self.play_at("you either get back bytes", FadeIn(good, shift=RIGHT))
        self.play_at("or you get", FadeIn(outcomes[1]), FadeIn(bad, shift=RIGHT))
        self.play_at("That's the whole contract",
                     FadeIn(caption('matching bytes, or "not found." Nothing else.', 30)))


class S07HonestAsterisk(NarratedScene):
    episode, number = 1, 7

    def construct(self):
        rng = np.random.default_rng(7)
        files = VGroup(*[Dot(radius=0.04, color=GREY_A).move_to(
            np.array([rng.uniform(-6.5, -1.5), rng.uniform(-2.6, 2.4), 0])) for _ in range(260)])
        tags = VGroup(*[Square(0.22, stroke_width=1, color=HASH_COLOR) for _ in range(48)])
        tags.arrange_in_grid(8, 6, buff=0.15).move_to(RIGHT * 4 + DOWN * 0.1)
        left = label("all possible files: infinitely many", 24).next_to(files, UP, buff=0.3)
        right = label("all possible CIDs: finitely many", 24).next_to(tags, UP, buff=0.3)
        self.play(FadeIn(files, lag_ratio=0.01), FadeIn(left))
        self.play_at("only finitely many names", FadeIn(tags, lag_ratio=0.02), FadeIn(right))

        arrows = VGroup()
        hit = set()
        collided = set()
        for k in range(70):
            src = files[k * 3]
            idx = int(rng.integers(0, len(tags)))
            if idx in hit:
                collided.add(idx)
            hit.add(idx)
            arrows.add(Line(src.get_center(), tags[idx].get_center(), stroke_width=1, stroke_opacity=0.45,
                            color=GREY_B))
        self.play_at("So some different files must share a name",
                     LaggedStart(*[Create(a) for a in arrows], lag_ratio=0.03, run_time=2.5))
        self.play(*[tags[i].animate.set_fill(RED, opacity=0.9).set_stroke(RED) for i in collided])
        self.play_at("It's just counting", Indicate(right))

        quote = quote_card("The following can be treated as true enough despite being false",
                           "256t.org", width=12)
        bullets = VGroup(*[label("•  " + t, 26) for t in (
            "A 94 character path uniquely determines content.",
            "The content is immutable.",
            "Content can be safely cached indefinitely.")]).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        board = VGroup(quote, bullets).arrange(DOWN, buff=0.5)
        self.play_at("The 256t spec is refreshingly upfront",
                     FadeOut(VGroup(files, tags, arrows, left, right)), FadeIn(quote))
        for phrase, b in zip(("That a CID uniquely determines", "That the content is immutable",
                              "That it can be cached forever"), bullets):
            self.play_at(phrase, FadeIn(b, shift=RIGHT * 0.2), run_time=0.5)
        crack = VMobject(color=WHITE, stroke_width=3).set_points_as_corners(
            [quote.get_corner(UR) + LEFT * 0.2, quote.get_corner(UR) + LEFT * 0.7 + DOWN * 0.4,
             quote.get_corner(UR) + LEFT * 0.55 + DOWN * 0.75, quote.get_corner(UR) + LEFT * 1.1 + DOWN * 1.1])
        self.play_at("They aren't", Create(crack))
        self.play_at("how true is true enough", Indicate(bullets, color=YELLOW))


class S08SeriesMap(NarratedScene):
    episode, number = 1, 8

    def construct(self):
        row = fit_width(series_row(), FRAME_W - 0.6)
        self.play(LaggedStart(*[FadeIn(c, shift=UP * 0.3) for c in row], lag_ratio=0.1), run_time=1.0)
        self.play(row[0].animate.set_opacity(0.4), run_time=0.4)
        cues = [None, "Next time, we'll take a CID apart", "Then we'll look at what a hash function",
                "the accidental kind", "the adversarial kind", "After that, how verification",
                "And finally, what 256t deliberately"]
        numbers = VGroup(*[label(n, 34, YELLOW) for n in ("8", "64", "86", "94", "256")])
        numbers.arrange(RIGHT, buff=0.6).next_to(row, DOWN, buff=0.6)
        for card_, phrase in zip(row, cues):
            if phrase:
                self.play_at(phrase, Indicate(card_, color=YELLOW, scale_factor=1.12))
            if phrase == cues[1]:
                self.play_at("the eight, the sixty-four",
                             LaggedStart(*[FadeIn(n) for n in numbers], lag_ratio=0.4))
                self.play(FadeOut(numbers))
        self.play_at("See you in the next one", row.animate.set_opacity(1))
