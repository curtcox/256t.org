"""Episode 6 — Verify, Cache, Trust.

Scenes follow video-series/ep06-verify-cache-trust.md. Render with:
    python3 video-series/manim/render.py 6
"""

from common import *

L = len(GETTYSBURG)
CID_A = compute_cid(b"A")
LYING_LENGTH = "AAAAAAAF" + b64url(b"Hi")      # claims 5 bytes, holds 2


def lenient_decode(text: str) -> bytes:
    """Decode base64url the way many lenient decoders do: ignore stray padding bits."""
    return base64.urlsafe_b64decode(text + "=" * (-len(text) % 4))


ALIASES_OF_A = [c for c in B64_ALPHABET if lenient_decode("Q" + c) == b"A"]
assert len(ALIASES_OF_A) == 16 and ALIASES_OF_A[0] == "Q"


class S01BytesFromAStranger(NarratedScene):
    episode, number = 6, 1

    def construct(self):
        laptop = VGroup(RoundedRectangle(corner_radius=0.1, width=2.0, height=1.3, color=WHITE),
                        Line(LEFT * 1.2, RIGHT * 1.2, color=WHITE)).arrange(DOWN, buff=0.05).move_to(LEFT * 5)
        mirror = ServerRack(face="?", height=1.8).move_to(RIGHT * 5)
        mirror_label = label("unknown mirror", 22, GREY_B).next_to(mirror, DOWN, buff=0.2)
        self.play(FadeIn(laptop), FadeIn(mirror), FadeIn(mirror_label))
        req = Arrow(laptop.get_right(), mirror.get_left(), buff=0.3, color=GREY_B).shift(UP * 0.3)
        get = mono("GET /" + GETTYSBURG_CID[:12] + "…", 22, HASH_COLOR).next_to(req, UP, buff=0.1)
        self.play_at("that claims to have it", GrowArrow(req), FadeIn(get))
        ribbon = ByteRibbon.text(GETTYSBURG[:24], cell_size=0.3).next_to(mirror, LEFT, buff=0.3).shift(DOWN * 0.5)
        self.play_at("bytes start flowing back", FadeIn(ribbon))
        self.play(ribbon.animate.next_to(laptop, RIGHT, buff=0.3).shift(DOWN * 0.5), run_time=2, rate_func=linear)
        q = caption("what should your code do now?", 34, YELLOW)
        self.play_at("the advice was", FadeIn(q))
        airtight = VGroup(label("airtight", 28, GREEN), label("vs.", 24, GREY_B), label("looks airtight", 28, RED))
        airtight.arrange(RIGHT, buff=0.3).move_to(UP * 2.6)
        self.play_at("separate a check that's airtight", FadeIn(airtight))
        cache = CacheBox(size=1.2).move_to(UP * 2.6 + RIGHT * 4.5)
        self.play_at("permission to cache forever", FadeIn(cache, scale=0.5))


class S02VerificationLoop(NarratedScene):
    episode, number = 6, 2

    def construct(self):
        loop = VerifierLoop(width=11).move_to(UP * 0.8)
        self.play(FadeIn(loop))
        for station, phrase in zip(loop.stations, ["You requested a CID", "You receive some bytes",
                                                    "You compute the CID of those bytes", "And you compare the two CIDs"]):
            self.play_at(phrase, Indicate(station, color=YELLOW), run_time=0.6)
        ok = Check().next_to(loop.stations[-1], RIGHT, buff=0.4)
        accept = label("accept", 24, GREEN).next_to(ok, DOWN, buff=0.1)
        self.play_at("If they're identical", GrowFromCenter(ok), FadeIn(accept))
        retry = VGroup(ServerRack(height=0.9), ServerRack(height=0.9)).arrange(RIGHT, buff=0.3)
        retry.next_to(loop, DOWN, buff=0.9)
        retry_label = label("try somewhere else", 22, GREY_B).next_to(retry, DOWN, buff=0.15)
        self.play_at("try somewhere else", FadeIn(retry), FadeIn(retry_label))

        self.play_at("Notice what's not in this loop", FadeOut(VGroup(retry, retry_label, ok, accept)))
        wall = DashedLine(UP * 3.6, DOWN * 3.6, color=GREY_B).move_to(LEFT * 1.0)
        untrusted = label("untrusted side", 24, RED).move_to(LEFT * 4.5 + DOWN * 1.6)
        trusted = label("trusted side", 24, GREEN).move_to(RIGHT * 3.5 + DOWN * 1.6)
        self.play(loop.animate.scale(0.75).move_to(RIGHT * 2.7 + UP * 1.6), Create(wall),
                  FadeIn(untrusted), FadeIn(trusted))
        claims = VGroup(*[label(t, 24, GREY_B) for t in ('"I\'m a trusted server"', "reputation",
                                                         "headers", "labels")]).arrange(DOWN, buff=0.25)
        claims.move_to(LEFT * 4.5 + DOWN * 0.1)
        self.play_at("Nothing the server says about itself", FadeIn(claims, lag_ratio=0.2))
        blocked = VGroup(*[Line(c.get_left(), c.get_right(), color=RED, stroke_width=3) for c in claims])
        self.play(Create(blocked))
        bytes_ = ByteRibbon.text(b"bytes", cell_size=0.4).move_to(LEFT * 4.5 + UP * 2.2)
        self.play_at("The only thing that crosses", FadeIn(bytes_))
        self.play(bytes_.animate.next_to(loop.stations[1], DOWN, buff=0.3), run_time=1.2)
        self.play_at("That's why you can fetch from a stranger", Indicate(bytes_, color=GREEN))


class S03LengthPrefixEarnsItsKeep(NarratedScene):
    episode, number = 6, 3

    def construct(self):
        cid = CIDText(GETTYSBURG_CID, size=26, max_width=FRAME_W - 1).to_edge(UP, buff=0.4)
        brace = Brace(cid.prefix, DOWN, color=LENGTH_COLOR)
        brace_label = label(f"L = {L:,} bytes", 26, LENGTH_COLOR)
        brace.put_at_tip(brace_label)
        brace_label.align_to(cid, LEFT)
        self.play(FadeIn(cid))
        self.play_at("that eight-character length prefix", GrowFromCenter(brace), FadeIn(brace_label))

        gauge = StreamGauge(L, width=9.5).move_to(UP * 0.4)
        gear = VGroup(Circle(0.45, color=HASH_COLOR), fit_width(label("SHA-512", 16, HASH_COLOR), 0.7))
        gear.next_to(gauge, DOWN, buff=0.3)
        gear[1].move_to(gear[0])
        gear_note = label("hashing as bytes arrive", 20, HASH_COLOR).next_to(gear, RIGHT, buff=0.2)
        self.play_at("Before a single byte arrives", FadeIn(gauge))
        self.play_at("you can hash the bytes as they stream in", FadeIn(gear), FadeIn(gear_note))

        cases = VGroup(label("A: honest", 26, GREEN), label("B: too short", 26, RED), label("C: too long", 26, RED))
        cases.arrange(RIGHT, buff=1.2).move_to(DOWN * 1.9)
        # Case A: exactly L bytes.
        self.play_at("If the stream ends at exactly", FadeIn(cases[0]),
                     gauge.fill_to(L, run_time=1.6), Rotate(gear[0], -4 * PI, run_time=1.6))
        ok = Check(0.45).next_to(cases[0], DOWN, buff=0.15)
        self.play(GrowFromCenter(ok))
        # Case B: ends early.
        self.play_at("If the stream ends early", FadeIn(cases[1]), gauge.fill_to(900, color=RED, run_time=1.0))
        bad_b = XMark(0.4).next_to(cases[1], DOWN, buff=0.15)
        self.play(GrowFromCenter(bad_b))
        # Case C: keeps going past L.
        self.play_at("If a server keeps sending past", FadeIn(cases[2]), gauge.fill_to(0.001, run_time=0.3))
        self.play(gauge.fill_to(L, color=ORANGE, run_time=1.0))
        self.play(gauge.fill_to(L * 1.12, color=RED, run_time=0.4))
        scissors = label("✂", 60, RED).move_to(gauge.stop_x + UP * 0.0)
        bad_c = XMark(0.4).next_to(cases[2], DOWN, buff=0.15)
        self.play_at("You can hang up on it right then", FadeIn(scissors, scale=2), GrowFromCenter(bad_c))
        endless = label("server was trying to send ∞", 22, GREY_B).next_to(gauge, UP, buff=0.15).align_to(gauge, RIGHT)
        self.play_at("an endless stream", FadeIn(endless))
        worst = caption("the most a liar can waste: the length you asked for", 28, YELLOW)
        self.play_at("The most it can ever waste", FadeIn(worst))

        self.play_at("Knowing the length up front helps honest transfers",
                     FadeOut(VGroup(cases, ok, bad_b, bad_c, scissors, endless, worst)),
                     gauge.fill_to(L * 0.62, run_time=0.6))
        extras = VGroup(tag_box(f"allocate exactly {L:,} bytes", GREY_A, 22),
                        tag_box("honest progress bar: 62%", GREY_A, 22)).arrange(RIGHT, buff=0.5).move_to(DOWN * 1.9)
        self.play_at("set aside exactly the right amount of memory", FadeIn(extras[0]))
        self.play_at("show an honest progress bar", FadeIn(extras[1]))

        self.play_at("There's one limit worth being upfront about", FadeOut(extras))
        limit = VGroup(label("reject early", 28, RED), label("·", 28, GREY_B), label("accept only at the last byte", 28, GREEN))
        limit.arrange(RIGHT, buff=0.3).move_to(DOWN * 1.9)
        self.play(gauge.fill_to(L * 0.98, run_time=1.2), FadeIn(limit))
        pending = label("?", 40, YELLOW).next_to(gauge.stop, RIGHT, buff=0.2)
        self.play_at("you can only accept at the end", FadeIn(pending))
        self.play(gauge.fill_to(L, run_time=0.4), Transform(pending, Check(0.4).move_to(pending)))
        ahead = label("→ chunked trees do this differently (Episode 7)", 22, HASH_COLOR).to_edge(DOWN, buff=0.4)
        self.play_at("Some other systems make a different trade-off", FadeIn(ahead))


class S04InlineNothingToFetch(NarratedScene):
    episode, number = 6, 4

    def construct(self):
        hello = CIDText(compute_cid(b"Hello"), size=56).move_to(UP * 1.5)
        self.play(FadeIn(hello))
        rule = label("L = 5 ≤ 64  →  decode", 32, INLINE_COLOR).next_to(hello, DOWN, buff=0.4)
        word = mono("Hello", 60, INLINE_COLOR).next_to(rule, DOWN, buff=0.4)
        self.play_at("If the length is sixty-four bytes or less", FadeIn(rule))
        self.play(TransformFromCopy(hello.suffix, word))
        rack = ServerRack(height=1.2).move_to(RIGHT * 5 + DOWN * 2)
        cable = Line(word.get_right() + RIGHT * 0.2, rack.get_left(), color=GREY_B)
        self.add(rack, cable)
        self.play_at("You don't need a server", cable.animate.shift(DOWN * 4).set_opacity(0),
                     rack.animate.shift(RIGHT * 4).set_opacity(0), run_time=1.2)
        nothing = caption("the name is the content", 32, INLINE_COLOR)
        self.play_at("there is nothing to trust", FadeIn(nothing))
        trap = label("…almost.", 40, YELLOW).move_to(DOWN * 1.2)
        self.play_at("Well, almost nothing", FadeOut(nothing), FadeIn(trap))


class S05DecodeIsNotVerify(NarratedScene):
    episode, number = 6, 5

    def construct(self):
        strip = BitStrip("01000001" + "0000", cell=0.55).move_to(UP * 2.2)
        for cell in strip[8:]:
            cell[0].set_fill(GREY_D)
            cell[1].set_color(GREY_B)
        groups = strip.group_boxes(6)
        chars = VGroup(mono("Q", 44, YELLOW).next_to(groups[0], DOWN, buff=0.25),
                       mono("Q", 44, YELLOW).next_to(groups[1], DOWN, buff=0.25))
        self.play_at("Remember how the single byte A", FadeIn(strip), Create(groups), FadeIn(chars))
        real = SurroundingRectangle(strip[6:8], color=GREEN, buff=0.04)
        pad = SurroundingRectangle(strip[8:], color=GREY_B, buff=0.04)
        self.play_at("The second carries the last two real bits", Create(real))
        self.play_at("plus four padding zeros", Create(pad))
        ignore = label("many decoders ignore these", 22, GREY_B).next_to(pad, UP, buff=0.15)
        self.play_at("many decoders simply ignore the padding", FadeIn(ignore))

        new_pad = BitStrip("0001", cell=0.55).move_to(strip[8:])
        for cell in new_pad:
            cell[0].set_fill(RED_E)
        r_char = mono("R", 44, RED).move_to(chars[1])
        self.play_at("if someone hands you Q R", Transform(strip[8:], new_pad), Transform(chars[1], r_char))
        out = VGroup(label("lenient decode →", 26), mono('"A"', 36, GREEN)).arrange(RIGHT, buff=0.2)
        out.next_to(chars, DOWN, buff=0.35)
        self.play_at("still give you the letter A", FadeIn(out))

        fan = VGroup(*[mono("Q" + c, 26, GREEN if c == "Q" else RED) for c in ALIASES_OF_A]).arrange_in_grid(2, 8, buff=(0.45, 0.2))
        fan.next_to(out, DOWN, buff=0.4)
        self.play_at("any of sixteen different second characters", LaggedStart(*[FadeIn(f) for f in fan], lag_ratio=0.06))
        canon = SurroundingRectangle(fan[0], color=GREEN, buff=0.08)
        canon_label = label(f"only {CID_A} is the CID of A", 24, GREEN).next_to(fan, DOWN, buff=0.25)
        self.play_at("But only one of those strings", Create(canon), FadeIn(canon_label))

        # The lying length prefix.
        self.play_at("There's a second version of this trap", *[FadeOut(m) for m in self.mobjects])
        liar = CIDText(LYING_LENGTH, size=52).move_to(UP * 2)
        claim = label("prefix says: 5 bytes", 28, LENGTH_COLOR).next_to(liar, DOWN, buff=0.35).shift(LEFT * 2)
        holds = label("payload holds: 2 bytes (\"Hi\")", 28, INLINE_COLOR).next_to(liar, DOWN, buff=0.35).shift(RIGHT * 2.5)
        self.play(FadeIn(liar))
        self.play_at("might claim five bytes", FadeIn(claim))
        self.play_at("only decode to two", FadeIn(holds))
        sloppy = label('sloppy decoder returns "Hi" and never notices', 26, RED).move_to(DOWN * 0.2)
        self.play_at("A sloppy decoder might just return", FadeIn(sloppy))

        keys = VGroup(*[tag_box(t, GREY_B, 22) for t in ("cache keys", "database index", "approved-document list")])
        keys.arrange(RIGHT, buff=0.4).move_to(DOWN * 1.4)
        self.play_at("can break anything that uses CIDs as keys", LaggedStart(*[FadeIn(k) for k in keys], lag_ratio=0.3))

        # The rule.
        self.play_at("The fix is the same rule", *[FadeOut(m) for m in self.mobjects])
        wrong = VGroup(label("✗  decode the CID → trust the result", 32, RED))
        right = VGroup(label("✓  compute the CID of the bytes → compare strings", 32, GREEN))
        rules = VGroup(wrong, right).arrange(DOWN, buff=0.4, aligned_edge=LEFT).move_to(UP * 1.8)
        self.play_at("Never verify by decoding", FadeIn(wrong))
        self.play_at("Always go the other way", FadeIn(right))
        checks = VGroup()
        for given, data in ((CID_A[:-1] + "R", b"A"), (LYING_LENGTH, b"Hi")):
            computed = compute_cid(data)
            row = VGroup(mono(given, 26, RED), label("vs computed", 20, GREY_B), mono(computed, 26, GREEN),
                         XMark(0.3)).arrange(RIGHT, buff=0.3)
            checks.add(row)
        checks.arrange(DOWN, buff=0.35).move_to(DOWN * 1.0)
        self.play_at("The recipe only ever produces one canonical name", LaggedStart(*[FadeIn(c) for c in checks],
                                                                                         lag_ratio=0.5))


class S06CachingForever(NarratedScene):
    episode, number = 6, 6

    def construct(self):
        quote = quote_card("There are only two hard things in Computer Science:\ncache invalidation and naming things.",
                           "Phil Karlton")
        self.play_at("There's an old joke", FadeIn(quote))
        tag = CIDText(GETTYSBURG_CID[:16] + "…", size=34)
        self.play_at("Content addressing is a nice punchline", ReplacementTransform(quote, tag))

        self.play_at("Think about what makes caching hard", tag.animate.to_edge(UP, buff=0.4))
        stale = VGroup(CacheBox(size=1.3), label("is my copy out of date?", 26, RED)).arrange(RIGHT, buff=0.4)
        self.play(FadeIn(stale))
        never = label("a CID can't go out of date", 26, GREEN).move_to(stale[1], aligned_edge=LEFT)
        self.play_at("But a CID can't go out of date", Transform(stale[1], never))

        self.play_at("So a server can tell every cache", FadeOut(stale))
        headers = VGroup(mono("Cache-Control: public, max-age=31536000, immutable", 26),
                         mono('ETag: "' + GETTYSBURG_CID[:16] + '…"', 26)).arrange(DOWN, aligned_edge=LEFT, buff=0.2)
        headers.move_to(UP * 1.3)
        frame = SurroundingRectangle(headers, buff=0.3, color=GREY_B, corner_radius=0.1)
        self.play_at("suggests a header like this one", FadeIn(frame), AddTextLetterByLetter(headers[0]), run_time=1.5)
        notes = [("public", "any cache may keep it", "Public, meaning"),
                 ("max-age=31536000", "one year: the conventional maximum", "A max age of one year"),
                 ("immutable", "don't even bother re-checking", "And immutable")]
        line = headers[0]
        note_group = VGroup()
        text = line.text
        for k, (word, meaning, phrase) in enumerate(notes):
            start = text.replace(" ", "").find(word.replace(" ", ""))
            part = line[start:start + len(word.replace(" ", ""))]
            note = label(f"{word} → {meaning}", 22, YELLOW).move_to(DOWN * (0.3 + 0.45 * k))
            self.play_at(phrase, Indicate(part, color=YELLOW), FadeIn(note))
            note_group.add(note)
        self.play_at("The CID also makes a perfect version tag", FadeIn(headers[1]))

        # Chain of caches.
        self.play_at("Two small cautions", FadeOut(note_group), VGroup(frame, headers).animate.scale(0.7).to_edge(UP, buff=1.2))
        hops = VGroup(label("browser", 22), label("proxy", 22), label("CDN edge", 22), label("origin", 22))
        boxes = VGroup(*[VGroup(RoundedRectangle(corner_radius=0.12, width=1.9, height=0.9, color=GREY_B), h)
                         for h in hops])
        for b in boxes:
            b[1].move_to(b[0])
        boxes.arrange(RIGHT, buff=0.6).move_to(DOWN * 0.3)
        arrows = VGroup(*[Arrow(a.get_right(), b.get_left(), buff=0.08, color=GREY_B) for a, b in zip(boxes, boxes[1:])])
        badges = VGroup(*[label("∞", 26, TEAL).next_to(b, UP, buff=0.08) for b in boxes[:3]])
        self.play(FadeIn(boxes), FadeIn(arrows), LaggedStart(*[FadeIn(x) for x in badges], lag_ratio=0.2))

        not_found = VGroup(tag_box("404 Not Found", GREY_B, 24),
                           label("a fact about one server, now — cache briefly", 22, RED)).arrange(RIGHT, buff=0.3)
        not_found.move_to(DOWN * 1.7)
        self.play_at('A "not found" response is different', FadeIn(not_found))
        types = VGroup(tag_box("Content-Type: image/png", GREY_B, 20), tag_box("Content-Type: text/plain", GREY_B, 20),
                       label("same bytes, same CID: a hint, not identity", 22, YELLOW)).arrange(RIGHT, buff=0.3)
        types.move_to(DOWN * 2.6)
        fit_width(types, FRAME_W - 0.6)
        self.play_at("headers like the content type", FadeIn(types))

        suffix_old = mono("t256:AAAAAAANSGVsbG8sIFdvcmxkIQ.txt", 30)
        suffix_new = mono("t256:AAAAAAANSGVsbG8sIFdvcmxkIQ.html", 30)
        self.play_at("The same goes for the file-type suffix",
                     FadeOut(VGroup(boxes, arrows, badges, not_found, types, frame, headers, tag)))
        suffix_old.move_to(UP * 1.4)
        self.play(FadeIn(suffix_old))
        cid_part = suffix_old[5:31]
        cid_brace = Brace(cid_part, DOWN, color=GREEN)
        cid_note = label("CID: verified ✓", 22, GREEN)
        cid_brace.put_at_tip(cid_note)
        sfx_note = label("suffix: not verified", 22, RED).next_to(suffix_old[31:], UP, buff=0.2)
        self.play_at("Verification covers the bytes, not the suffix", GrowFromCenter(cid_brace), FadeIn(cid_note),
                     FadeIn(sfx_note))
        suffix_new.move_to(suffix_old, aligned_edge=LEFT)
        self.play_at("anyone who can edit a link", Transform(suffix_old, suffix_new))
        warn = VGroup(Triangle(color=YELLOW, fill_opacity=0).scale(0.4), label("!", 30, YELLOW)).move_to(DOWN * 1.4 + LEFT * 2.5)
        warn[1].move_to(warn[0]).shift(DOWN * 0.08)
        warn_text = label("text treated as HTML can run scripts", 28, YELLOW).next_to(warn, RIGHT, buff=0.3)
        self.play_at("a careless browser might then run code", FadeIn(warn), FadeIn(warn_text))


class S07TrueEnoughRevisited(NarratedScene):
    episode, number = 6, 7

    def construct(self):
        statements = [("A CID uniquely determines content", "I only accept content that has this CID"),
                      ("The content is immutable", "Anything else under this name gets rejected"),
                      ("Content can be cached forever", "A verified copy never needs re-checking")]
        quote = label("true enough despite being false", 36, GREY_A, slant=ITALIC).to_edge(UP, buff=0.5)
        rows = VGroup(*[label("•  " + a, 30) for a, _ in statements]).arrange(DOWN, aligned_edge=LEFT, buff=0.5)
        rows.move_to(UP * 0.4)
        self.play_at("Let's go back to that honest list", FadeIn(quote), FadeIn(rows, lag_ratio=0.2))
        cues = ["stops being a claim about the universe", '"The content is immutable" becomes',
                '"cache it forever" becomes']
        for row, (_, rule), phrase in zip(rows, statements, cues):
            new = label("✓  " + rule, 30, GREEN).move_to(row, aligned_edge=LEFT)
            self.play_at(phrase, Transform(row, new))
        self.play(FadeOut(quote))
        analogies = VGroup(*[tag_box(t, GREY_B, 22) for t in (
            "floating-point ≈ real numbers", "TCP ≈ reliable", "the Earth ≈ flat (for a house)")]).arrange(RIGHT, buff=0.3)
        analogies.move_to(DOWN * 2.0)
        for box, phrase in zip(analogies, ["floating-point numbers", "a stream of unreliable packets", "A carpenter treats"]):
            self.play_at(phrase, FadeIn(box, shift=UP * 0.2), run_time=0.5)
        cap = caption("useful abstractions with known cracks", 28, YELLOW)
        self.play_at("as long as you know exactly where the crack is", FadeIn(cap))


class S08TrustMovesToTheMath(NarratedScene):
    episode, number = 6, 8

    def construct(self):
        tag = CIDText(GETTYSBURG_CID[:16] + "…", size=40).to_edge(UP, buff=0.5)
        self.play(FadeIn(tag))
        sources = VGroup(ServerRack(height=1.2), label("library", 24), label("friend's phone", 24),
                         label("USB stick", 24), ServerRack(face="?", height=1.2))
        names = ["data center", "", "", "", "stranger's mirror"]
        sources.arrange(RIGHT, buff=0.8).move_to(DOWN * 1.8)
        name_labels = VGroup(*[label(n, 20, GREY_B).next_to(s, DOWN, buff=0.15) for n, s in zip(names, sources) if n])
        self.play_at("it stops mattering who hands you the bytes", LaggedStart(*[FadeIn(s) for s in sources], lag_ratio=0.2),
                     FadeIn(name_labels))
        loop_checks = VGroup(*[Check(0.35).next_to(s, UP, buff=0.3) for s in sources])
        lines = VGroup(*[DashedLine(s.get_top(), tag.get_bottom(), color=GREY_D) for s in sources])
        self.play_at("A big data center, a library", Create(lines), LaggedStart(*[GrowFromCenter(c) for c in loop_checks],
                                                                                 lag_ratio=0.25))
        cap = caption("trust the name, not the messenger", 34, YELLOW)
        self.play_at("The trust lives in the name", FadeIn(cap))
