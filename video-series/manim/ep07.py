"""Episode 7 — Limits and Neighbors.

Scenes follow video-series/ep07-limits-and-neighbors.md. Render with:
    python3 video-series/manim/render.py 7
"""

import json

from common import *
from ep01 import house, map_pin

GIT_BLOB = hashlib.sha1(b"blob %d\0" % len(GETTYSBURG) + GETTYSBURG).hexdigest()
NI_URI = "ni:///sha-256;" + b64url(hashlib.sha256(GETTYSBURG).digest())
METADATA = json.dumps({"title": "The Gettysburg Address", "type": "text/plain", "file": GETTYSBURG_CID},
                      indent=2).encode()
METADATA_CID = compute_cid(METADATA)


class S01DefinedByItsEdges(NarratedScene):
    episode, number = 7, 1

    def construct(self):
        tag = CIDText(GETTYSBURG_CID[:14] + "…", size=40)
        self.play(FadeIn(tag, scale=0.8))
        does = VGroup(label("bytes → name", 30, GREEN), label("name → check", 30, GREEN)).arrange(DOWN, buff=0.2)
        does.next_to(tag, DOWN, buff=0.5)
        self.play_at("Turn bytes into a name", FadeIn(does[0]))
        self.play_at("Turn a name back into a check", FadeIn(does[1]))
        regions = [("Finding", "where is it published?", UL), ("Publishing", "where and how do I publish?", UR),
                   ("Metadata", "what should accompany it?", DL), ("Permissions", "who may serve it, to whom?", DR)]
        cards = VGroup()
        for name, question, corner in regions:
            c = card(name, question, GREY_B, width=4.2, height=2.0)
            c[0].set_stroke(opacity=0.6)
            c.to_corner(corner, buff=0.5)
            cards.add(c)
        self.play_at("the much longer list of things", LaggedStart(*[FadeIn(c) for c in cards], lag_ratio=0.25))
        stamps = VGroup(*[label("beyond the scope of this text", 16, RED).rotate(0.08).next_to(c[0].get_bottom(), UP, buff=0.12)
                          for c in cards])
        self.play_at("beyond the scope of this text", LaggedStart(*[FadeIn(s, scale=1.4) for s in stamps], lag_ratio=0.15))
        self.play_at("each trade-off buys something", Indicate(tag, color=YELLOW))


class S02FindingAndPublishing(NarratedScene):
    episode, number = 7, 2

    def construct(self):
        tag = CIDText(GETTYSBURG_CID[:14] + "…", size=36).move_to(UP * 2.6)
        q = label("where?", 30, YELLOW).next_to(tag, RIGHT, buff=0.4)
        self.play_at("A CID tells you what you want", FadeIn(tag), FadeIn(q))
        options = VGroup(*[tag_box(t, GREY_B, 22) for t in ("a known base URL", "a list of mirrors", "a search index",
                                                            "a friend's message")]).arrange(RIGHT, buff=0.3)
        options.move_to(UP * 1.3)
        self.play_at("says nothing about which servers", LaggedStart(*[FadeIn(o) for o in options], lag_ratio=0.2))
        bases = ["https://256t.org/", "https://mirror.example/", "file:///archive/"]
        urls = VGroup(*[VGroup(mono(b, 24, GREY_B), mono(GETTYSBURG_CID[:12] + "…", 24, HASH_COLOR)).arrange(RIGHT, buff=0.05)
                        for b in bases]).arrange(DOWN, aligned_edge=LEFT, buff=0.3).move_to(LEFT * 2 + DOWN * 0.6)
        checks = VGroup(*[Check(0.35).next_to(u, RIGHT, buff=0.4) for u in urls])
        self.play_at("The same CID can go on the end of any server's address",
                     LaggedStart(*[FadeIn(u, shift=RIGHT * 0.2) for u in urls], lag_ratio=0.3))
        self.play_at("whatever comes back gets checked the same way", LaggedStart(*[GrowFromCenter(c) for c in checks],
                                                                                  lag_ratio=0.2))

        self.play_at("There's even a draft proposal", FadeOut(VGroup(urls, checks, options)))
        t256 = VGroup(mono("t256:", 40, YELLOW), mono(GETTYSBURG_CID[:12] + "…", 40, HASH_COLOR)).arrange(RIGHT, buff=0.05)
        t256.move_to(UP * 0.3)
        self.play_at("the letters t two fifty-six, a colon", FadeIn(t256[0]))
        self.play(FadeIn(t256[1]))
        no_host = label("draft URI scheme: no host at all", 26, GREY_A).next_to(t256, DOWN, buff=0.3)
        self.play_at("names the content without choosing anyone", FadeIn(no_host))
        bad = VGroup(mono("256t:", 32, RED), label("✗  URI schemes must start with a letter", 24, RED)).arrange(RIGHT, buff=0.3)
        good = VGroup(mono("t256:", 32, GREEN), label("✓", 26, GREEN)).arrange(RIGHT, buff=0.3)
        spelling = VGroup(bad, good).arrange(DOWN, aligned_edge=LEFT, buff=0.2).next_to(no_host, DOWN, buff=0.5)
        self.play_at("the t moved to the front", FadeIn(spelling))

        self.play_at("That might feel like a gap", FadeOut(VGroup(spelling, no_host, t256, q)))
        pin = map_pin().move_to(UP * 0.9)
        home = house().next_to(pin, DOWN, buff=0.2)
        self.play_at("you're back to street addresses", FadeIn(pin), FadeIn(home))
        self.play_at("everything that breaks when the house moves",
                     home.animate.shift(RIGHT * 3).set_opacity(0.3), run_time=1.0)
        self.play(FadeOut(pin), FadeOut(home))
        layers = VGroup(*[tag_box(t, GREY_B, 24) for t in ("finding system A", "finding system B", "finding system C")])
        layers.arrange(RIGHT, buff=0.4).move_to(UP * 0.6)
        base = tag_box("CID: one permanent name", HASH_COLOR, 28).move_to(DOWN * 1.2)
        links = VGroup(*[Line(l.get_bottom(), base.get_top(), color=GREY_D) for l in layers])
        self.play_at("lets any number of finding systems", FadeIn(base), LaggedStart(*[FadeIn(l) for l in layers], lag_ratio=0.3),
                     Create(links))
        self.play(layers[1].animate.set_opacity(0.2), links[1].animate.set_opacity(0.2))
        replacement = tag_box("finding system D", GREY_B, 24).move_to(layers[1])
        self.play_at("and replaced, without a single name ever changing", FadeIn(replacement, shift=DOWN * 0.2),
                     Indicate(base, color=GREEN))


class S03NoMetadata(NarratedScene):
    episode, number = 7, 3

    def construct(self):
        names = ["photo.png", "IMG_2041.PNG", "cat.png"]
        uploads = VGroup(*[VGroup(label(n, 24), ByteRibbon.hex(bytes.fromhex("89504E470D0A1A0A"), cell_size=0.32))
                           .arrange(DOWN, buff=0.12) for n in names]).arrange(DOWN, buff=0.35).move_to(LEFT * 3.5)
        tag = tag_box("one CID", HASH_COLOR, 28).move_to(RIGHT * 3)
        self.play_at("A CID knows nothing about a file's name", FadeIn(uploads, lag_ratio=0.2))
        arrows = VGroup(*[Arrow(u.get_right(), tag.get_left(), buff=0.15, color=GREY_B) for u in uploads])
        self.play_at("Upload the same photo three times", LaggedStart(*[GrowArrow(a) for a in arrows], lag_ratio=0.2),
                     FadeIn(tag))
        dedup = label("duplicates collapse on their own", 26, GREEN).next_to(tag, DOWN, buff=0.3)
        self.play_at("That makes duplicates collapse", FadeIn(dedup))

        self.play_at("The draft URI scheme does allow one small hint", FadeOut(VGroup(uploads, arrows, tag, dedup)))
        uri = mono("t256:AAAAAAANSGVsbG8sIFdvcmxkIQ.txt", 34).move_to(UP * 1.5)
        self.play(FadeIn(uri))
        which = Brace(uri[5:31], DOWN, color=GREEN)
        which_note = label("CID → which bytes (verified)", 22, GREEN)
        which.put_at_tip(which_note)
        how = Brace(uri[31:], UP, color=YELLOW)
        how_note = label("how to read them (a hint)", 22, YELLOW)
        how.put_at_tip(how_note)
        self.play_at("It's after the CID, not inside it", GrowFromCenter(which), FadeIn(which_note))
        self.play_at("It says how to read the bytes", GrowFromCenter(how), FadeIn(how_note))

        self.play_at("For anything more than a hint", FadeOut(VGroup(uri, which, which_note, how, how_note)))
        code = Code(code_string=METADATA.decode().replace(GETTYSBURG_CID, GETTYSBURG_CID[:16] + "…"),
                    language="json", formatter_style="monokai", add_line_numbers=False,
                    paragraph_config={"font": MONO, "font_size": 24}).move_to(LEFT * 2.8 + UP * 0.6)
        self.play(FadeIn(code))
        gettys = VGroup(label("Gettysburg Address", 22), ByteRibbon.text(b"Four score", cell_size=0.3)).arrange(DOWN, buff=0.1)
        gettys.move_to(RIGHT * 4 + UP * 0.6)
        pointer = Arrow(code.get_right(), gettys.get_left(), color=HASH_COLOR, buff=0.2)
        self.play_at("refers to the text by its CID", GrowArrow(pointer), FadeIn(gettys))
        own = CIDText(METADATA_CID, size=20, max_width=6.0).next_to(code, DOWN, buff=0.3)
        own_label = label("this document's own CID", 20, GREY_B).next_to(own, DOWN, buff=0.1)
        self.play_at("That little document has its own CID", FadeIn(own), FadeIn(own_label))

        # A collection of documents, named by its own CID.
        self.play_at("A document that lists other documents", *[FadeOut(m) for m in self.mobjects])
        root = tag_box("collection CID", YELLOW, 26).move_to(UP * 2.4)
        mids = VGroup(*[tag_box(t, HASH_COLOR, 22) for t in ("metadata doc", "metadata doc", "metadata doc")])
        mids.arrange(RIGHT, buff=0.9).move_to(UP * 0.6)
        leaves = VGroup(*[ByteRibbon.text(t, cell_size=0.28) for t in (b"Gettysburg", b"Sonnet 18", b"The Raven")])
        for leaf, mid in zip(leaves, mids):
            leaf.next_to(mid, DOWN, buff=0.9)
        edges = VGroup(*[Line(root.get_bottom(), m.get_top(), color=GREY_B) for m in mids],
                       *[Line(m.get_bottom(), l.get_top(), color=GREY_B) for m, l in zip(mids, leaves)])
        self.play_at("named by its own CID", FadeIn(root), FadeIn(mids), FadeIn(leaves), Create(edges))
        changed = ByteRibbon.text(b"The Rav3n", cell_size=0.28).tint([7], RED).move_to(leaves[2])
        self.play_at("Change any file inside it", Transform(leaves[2], changed))
        self.play(*[Indicate(m, color=RED) for m in (mids[2], root)])
        new_root = tag_box("a different collection CID", RED, 26).move_to(root)
        self.play_at("the collection's name changes", Transform(root, new_root))
        cap = caption("names inside content → structures made of names", 30, YELLOW)
        self.play_at("Names inside content let you build", FadeIn(cap))


class S04Permissions(NarratedScene):
    episode, number = 7, 4

    def construct(self):
        tag = CIDText(GETTYSBURG_CID[:14] + "…", size=36).move_to(LEFT * 3.5)
        scales = VGroup(Line(UP * 0.8, DOWN * 0.8, color=GREY_A), Line(LEFT * 1.0, RIGHT * 1.0, color=GREY_A).shift(UP * 0.6),
                        Arc(radius=0.35, start_angle=PI, angle=PI, color=GREY_A).shift(LEFT * 1.0 + UP * 0.25),
                        Arc(radius=0.35, start_angle=PI, angle=PI, color=GREY_A).shift(RIGHT * 1.0 + UP * 0.25))
        lock = VGroup(RoundedRectangle(corner_radius=0.08, width=0.8, height=0.6, color=GREY_A),
                      Arc(radius=0.25, start_angle=0, angle=PI, color=GREY_A).shift(UP * 0.3))
        globe = VGroup(Circle(0.5, color=GREY_A), Line(UP * 0.5, DOWN * 0.5, color=GREY_A),
                       Line(LEFT * 0.5, RIGHT * 0.5, color=GREY_A))
        human = VGroup(scales, lock, globe).arrange(RIGHT, buff=0.6).move_to(RIGHT * 3.5)
        border = DashedLine(UP * 3, DOWN * 3, color=GREY_B)
        out = label("out of scope", 24, GREY_B).next_to(border, UP, buff=0.1).shift(RIGHT * 1.0)
        self.play(FadeIn(tag))
        self.play_at("Some content is private", FadeIn(lock))
        self.play_at("disputed ownership", FadeIn(scales))
        self.play_at("legal to distribute in one country", FadeIn(globe))
        self.play_at("declines to decide it", Create(border), FadeIn(out))
        cap = caption("a name doesn't decide who may share", 30, YELLOW)
        self.play_at("they're answered by people and laws", FadeIn(cap))


class S05OneHashForTheWholeFile(NarratedScene):
    episode, number = 7, 5

    def construct(self):
        title = label("256t: one hash for the whole file", 30, HASH_COLOR).to_edge(UP, buff=0.4)
        gauge = StreamGauge(1000, width=9, overflow=0.05).move_to(UP * 1.6)
        gauge.stop_label.become(label("last byte", 20, LENGTH_COLOR).next_to(gauge.stop, UP, buff=0.1))
        self.play_at("hashes the whole file as one piece", FadeIn(title), FadeIn(gauge))
        self.play_at("you can't fully accept anything", gauge.fill_to(1000, run_time=1.6))
        ok = Check(0.4).next_to(gauge.stop, RIGHT, buff=0.2)
        self.play(GrowFromCenter(ok))
        self.play_at("if a long download turns out to be wrong", FadeOut(ok), gauge.fill_to(900, color=RED, run_time=0.6))
        which = label("which part was bad? unknown", 24, RED).next_to(gauge, DOWN, buff=0.25)
        self.play(FadeIn(which))

        self.play_at("a structure called a Merkle tree", FadeOut(VGroup(gauge, which)),
                     Transform(title, label("a Merkle tree: hash the chunks, then the hashes", 30, TEAL).to_edge(UP, buff=0.4)))
        tree = MerkleTree(leaves=8, width=9, level_gap=0.9).move_to(DOWN * 0.3)
        chunks = VGroup(*[Rectangle(width=0.95, height=0.4, color=GREY_B, fill_color=GREY_D, fill_opacity=1)
                          .next_to(leaf, DOWN, buff=0.4) for leaf in tree.leaves])
        self.play_at("Cut the file into chunks", FadeIn(chunks, lag_ratio=0.1))
        self.play_at("Hash each chunk", FadeIn(tree.levels[0], lag_ratio=0.1))
        self.play_at("hash the hashes together in pairs", Create(tree.edges), *[FadeIn(level) for level in tree.levels[1:]])
        root_label = label("name = root", 22, YELLOW).next_to(tree.root, UP, buff=0.15)
        self.play_at("name the file by that root", FadeIn(root_label), Indicate(tree.root, color=YELLOW))
        marks = VGroup(*[Check(0.25).next_to(c, DOWN, buff=0.1) for c in chunks])
        bad = XMark(0.25).next_to(chunks[5], DOWN, buff=0.1)
        self.play_at("every chunk can be checked on its own", LaggedStart(*[GrowFromCenter(m) for k, m in enumerate(marks) if k != 5],
                                                                          lag_ratio=0.15),
                     chunks[5].animate.set_fill(RED_E), GrowFromCenter(bad))
        self.play_at("re-fetch only the piece that went wrong", chunks[5].animate.set_fill(GREY_D),
                     Transform(bad, Check(0.25).move_to(bad)))

        self.play_at("So why doesn't 256t do that", *[FadeOut(m) for m in self.mobjects])
        rows = [("", "whole-file hash (256t)", "chunked tree"),
                ("verify while downloading", "reject early only", "every chunk"),
                ("pieces from many servers", "harder", "natural"),
                ("names per file", "exactly one", "depends on settings"),
                ("to implement", "~10 lines", "a lot more")]
        table = VGroup()
        for k, (a, b, c) in enumerate(rows):
            color = GREY_B if k == 0 else WHITE
            row = VGroup(label(a, 24, GREY_A), label(b, 24, HASH_COLOR if k == 0 else color),
                         label(c, 24, TEAL if k == 0 else color))
            for cell, x in zip(row, (-6.2, -1.6, 2.6)):
                cell.move_to(np.array([x, 2.2 - 0.75 * k, 0]), aligned_edge=LEFT)
            table.add(row)
        self.play(FadeIn(table[0]))
        for row, phrase in zip(table[1:], [None, None, "Different choices give the same file different names",
                                           "an implementation that fits on a slide"]):
            if phrase:
                self.cue(phrase)
            self.play(FadeIn(row, shift=UP * 0.15), run_time=0.5)
        ceiling = caption("ceiling: 2⁴⁸ − 1 bytes ≈ 256 TiB per file", 28, LENGTH_COLOR)
        self.play_at("the length prefix tops out", FadeIn(ceiling))


class S06NoAlgorithmAgility(NarratedScene):
    episode, number = 7, 6

    def construct(self):
        fixed = VGroup(CIDText(GETTYSBURG_CID[:14] + "…", size=34), label("SHA-512, always", 22, HASH_COLOR))
        fixed.arrange(DOWN, buff=0.15).move_to(UP * 2 + LEFT * 3.5)
        agile = VGroup(VGroup(tag_box("sha2-512", TEAL, 22), mono("digest…", 30, HASH_COLOR)).arrange(RIGHT, buff=0.15),
                       label("self-describing: names its algorithm", 22, TEAL)).arrange(DOWN, buff=0.15)
        agile.move_to(UP * 2 + RIGHT * 3.5)
        self.play_at("the hash function itself is fixed", FadeIn(fixed))
        self.play_at("put a small label at the front", FadeIn(agile))

        beam = Line(LEFT * 4, RIGHT * 4, color=GREY_B, stroke_width=6).move_to(DOWN * 0.6)
        pivot = Triangle(color=GREY_B, fill_opacity=1).scale(0.3).next_to(beam, DOWN, buff=0)
        left_pan = VGroup(*[label(t, 22) for t in ("one name per content", "nothing to negotiate",
                                                   'no "which hash?" bugs')]).arrange(DOWN, buff=0.15)
        right_pan = VGroup(*[label(t, 22) for t in ("can switch algorithms later",
                                                    "same content, new name when it does", "more for every implementation")])
        right_pan.arrange(DOWN, buff=0.15)
        left_pan.next_to(beam.get_left(), DOWN, buff=0.4).shift(RIGHT * 1.5)
        right_pan.next_to(beam.get_right(), DOWN, buff=0.4).shift(LEFT * 1.5)
        self.play(Create(beam), FadeIn(pivot))
        self.play_at("That flexibility is real", FadeIn(right_pan[0]))
        self.play_at("but it isn't free", FadeIn(right_pan[1]))
        self.play_at("every implementation has to handle every option", FadeIn(right_pan[2]))
        self.play_at("takes the other side of that bet", FadeIn(left_pan, lag_ratio=0.3))
        warn = caption("if SHA-512 ever cracks: a new scheme, with years of warning", 28, YELLOW)
        self.play_at("the remedy would be a new scheme", FadeIn(warn))


class S07TheNeighbors(NarratedScene):
    episode, number = 7, 7

    def construct(self):
        h = Arrow(LEFT * 6.5, RIGHT * 6.5, color=GREY_D, buff=0).shift(DOWN * 0.2)
        v = Arrow(DOWN * 3.6, UP * 3.4, color=GREY_D, buff=0)
        hl = VGroup(label("content in the link", 18, GREY_B).next_to(h.get_start(), DOWN, buff=0.1).shift(RIGHT * 0.9),
                    label("fingerprint in the link", 18, GREY_B).next_to(h.get_end(), DOWN, buff=0.1).shift(LEFT * 1.0))
        vl = VGroup(label("just a check", 18, GREY_B).next_to(v.get_start(), RIGHT, buff=0.1),
                    label("a name you can fetch", 18, GREY_B).next_to(v.get_end(), RIGHT, buff=0.1).shift(DOWN * 0.1))
        self.play_at("let's meet the neighbors", Create(h), Create(v), FadeIn(hl), FadeIn(vl))

        def neighbor(name, example, note, color, pos):
            ex = fit_width(mono(example, 18, color), 3.2)
            group = VGroup(label(name, 24, color), ex, fit_width(label(note, 16, GREY_A), 3.2)).arrange(DOWN, buff=0.06)
            bg = BackgroundRectangle(group, fill_opacity=0.9, buff=0.1)
            return VGroup(bg, group).move_to(pos)

        cards = [
            ("data: URI", "data:text/plain;base64,SGVsbG8=", "the content IS the link", GREEN, LEFT * 4.6 + UP * 1.8,
             "At one extreme are data URIs"),
            ("Subresource Integrity", 'integrity="sha384-…"', "location + fingerprint check", BLUE_B, RIGHT * 3.6 + DOWN * 2.3,
             "Subresource Integrity is the web's way"),
            ("Git", f"blob {len(GETTYSBURG)}\\0… → {GIT_BLOB[:12]}…", "length + fingerprint, like 256t!", ORANGE, RIGHT * 4.9 + UP * 2.9,
             "Git names every file's contents"),
            ("ni: (RFC 6920)", NI_URI[:30] + "…", "IETF hash names; labels the algorithm", TEAL, RIGHT * 4.9 + UP * 1.75,
             "There's even an internet standard"),
            ("BitTorrent magnet", "magnet:?xt=urn:btih:…", "v2 checks each piece (Merkle trees)", PINK, RIGHT * 4.9 + UP * 0.6,
             "Magnet links in BitTorrent"),
            ("IPFS", "bafy…", "self-describing, chunked, a whole network", PURPLE, RIGHT * 1.3 + UP * 2.55,
             "And IPFS goes furthest"),
        ]
        shown = VGroup()
        for name, example, note, color, pos, phrase in cards:
            n = neighbor(name, example, note, color, pos)
            self.play_at(phrase, FadeIn(n, scale=0.9), run_time=0.6)
            shown.add(n)
            if name == "Git":
                self.play_at("Length plus fingerprint", Indicate(n[1][2], color=ORANGE, scale_factor=1.3))
        us = VGroup(label("256t", 34, LENGTH_COLOR), label("a single path segment", 18, GREY_A)).arrange(DOWN, buff=0.05)
        us_bg = SurroundingRectangle(us, color=LENGTH_COLOR, buff=0.15, corner_radius=0.1, fill_color=BLACK, fill_opacity=0.9)
        badge = VGroup(us_bg, us).move_to(LEFT * 1.6 + UP * 0.9)
        inline_link = DashedLine(badge.get_left(), shown[0].get_right(), color=GREEN)
        hash_link = DashedLine(badge.get_right(), shown[2].get_left(), color=ORANGE)
        self.play_at("sits in a deliberately small spot", FadeIn(badge, scale=1.3))
        self.play(Create(inline_link), Create(hash_link))
        self.play_at("at most ninety-four characters", Indicate(badge, color=LENGTH_COLOR))


class S08TheWholePicture(NarratedScene):
    episode, number = 7, 8

    def construct(self):
        cid = CIDText(GETTYSBURG_CID, size=28, max_width=FRAME_W - 1).move_to(UP * 2.6)
        self.play_at("put the whole thing back together", FadeIn(cid))
        where = VGroup(label("where", 30, GREY_B), Arrow(LEFT * 0.6, RIGHT * 0.6, color=GREY_B), label("what", 30, YELLOW))
        where.arrange(RIGHT, buff=0.3).next_to(cid, DOWN, buff=0.4)
        self.play_at("A web address usually tells you where", FadeIn(where))
        b1 = Brace(cid.prefix, DOWN, color=LENGTH_COLOR)
        t1 = label("length → 256 TiB → the name", 20, LENGTH_COLOR)
        b1.put_at_tip(t1)
        t1.align_to(cid, LEFT)
        b2 = Brace(cid.suffix, DOWN, color=HASH_COLOR)
        t2 = label("the content itself, or its SHA-512 fingerprint", 20, HASH_COLOR)
        b2.put_at_tip(t2)
        self.play_at("Eight characters of length", FadeOut(where), GrowFromCenter(b1), FadeIn(t1))
        self.play_at("Then up to eighty-six characters", GrowFromCenter(b2), FadeIn(t2))

        recap = VGroup(
            VGroup(dice_icon(GREY_A, 0.5), label("accidental collision, ever: ~2⁻²⁷¹", 24)).arrange(RIGHT, buff=0.3),
            VGroup(mask_icon(ADVERSARY_COLOR, 0.5), label("lying: defeated by one hash · forging: never done", 24)).arrange(RIGHT, buff=0.3),
            VGroup(CacheBox(size=0.6), label("verify, then fetch from anyone and cache forever", 24)).arrange(RIGHT, buff=0.3),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.35).move_to(DOWN * 1.0)
        self.play_at("Collisions exist, by simple counting", FadeIn(recap[0], shift=RIGHT * 0.2))
        self.play_at("Adversaries have it harder", FadeIn(recap[1], shift=RIGHT * 0.2))
        self.play_at("And once you verify", FadeIn(recap[2], shift=RIGHT * 0.2))

        self.play_at("It's a small idea", *[FadeOut(m) for m in self.mobjects])
        logo_path = REPO_DIR / "examples" / "256t_logo.png"
        logo = ImageMobject(str(logo_path)).scale_to_fit_height(2.6).move_to(UP * 1.2)
        site = mono("256t.org", 40, LENGTH_COLOR).next_to(logo, DOWN, buff=0.4)
        line = label("a name for bytes", 34).next_to(site, DOWN, buff=0.3)
        self.play(FadeIn(logo), FadeIn(site))
        self.play_at("small enough to implement in an afternoon", FadeIn(line))
        row = fit_width(series_row(), FRAME_W - 1).scale(0.7).to_edge(DOWN, buff=0.3)
        self.play_at("pick a file you care about", FadeIn(row, lag_ratio=0.1))
