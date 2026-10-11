# Episode 6 — Verify, Cache, Trust

**Target runtime:** ~10 minutes (≈8.0 min of narration at 150 wpm, plus animation holds) · **Narration:** 1,203 words

**Episode goal:** Turn "always verify" into a precise procedure, show how the length prefix makes that
procedure robust against misbehaving servers, expose the decode-vs-recompute pitfall, and explain why
content-addressed responses can be cached forever. Close by revisiting "true enough despite being false"
as a deliberate engineering stance.

**Facts used (verified with the reference implementation and Python's lenient base64 decoder):**
- `QQ` and `QR` (and any second character from `Q` through `f`) all decode to the single byte `A` in a
  lenient base64 decoder. Only `QQ` is canonical, and only `AAAAAAABQQ` is the CID of `A`.
- The canonical caching header suggested in the 256t FAQ: `Cache-Control: public, max-age=31536000, immutable`.

**New recurring visuals:** `StreamGauge` (a progress bar with a hard stop marked at L bytes).

---

## Scene 1 — Bytes from a stranger
*Target: 0:00–1:00*

**Visuals (Manim)**
- A client laptop at left. On the right, a `ServerRack` with a question-mark face: `unknown mirror`.
- The client sends `GET /AAAAAAXnpY8K…` (Gettysburg CID). A ribbon starts flowing back.
- Freeze. Caption: `what should your code do now?`

**Narration**
> Say you've got a CID, and you've found some server you've never heard of that claims to have it. You ask, and bytes start flowing back.
>
> In the last episode, the advice was "always verify." That sounds simple, and it mostly is. But it's worth getting precise about, because there are a few small decisions that separate a check that's airtight from one that only looks airtight. And once verification is solid, it unlocks something very nice: permission to cache forever.

---

## Scene 2 — The verification loop
*Target: 1:00–2:15*

**Visuals (Manim)**
- `VerifierLoop`, drawn as a circuit with four stations that light up in sequence:
  1. `request CID`
  2. `receive bytes`
  3. `compute CID of those bytes`
  4. `compare the two strings, character for character`
- Output: GREEN `accept` or RED `reject`. A parallel path shows the rejected bytes going in a bin and the client
  moving on to a second `ServerRack` to try again.
- Caption: `the server's claims never enter the loop — only the bytes do`.

**Narration**
> Here's the whole procedure. You requested a CID. You receive some bytes. You compute the CID of those bytes, using exactly the same recipe as everyone else. And you compare the two CIDs as strings. If they're identical, you accept the bytes. If not, you throw them away, and try somewhere else.
>
> Notice what's not in this loop. Nothing the server says about itself. Not its name, not its reputation, not any headers or labels it attaches. The only thing that crosses from the untrusted side to the trusted side is the bytes, and the bytes are checked against the name you already had. That's why you can fetch from a stranger.

---

## Scene 3 — The length prefix earns its keep
*Target: 2:15–4:30*

**Visuals (Manim)**
- Zoom in on the BLUE prefix of the requested CID: `AAAAAAXn → L = 1,511 bytes`.
- `StreamGauge`: a horizontal bar with a hard stop drawn at `1,511`. As bytes arrive, the bar fills and a
  hashing gear (`SHA-512, streaming`) turns alongside it, consuming bytes as they arrive.
- **Case A — honest:** stream stops exactly at 1,511, hash finalizes, compare → GREEN.
- **Case B — too long:** a `Liar` server keeps streaming. The bar hits the stop line at 1,511 and the client
  cuts the connection (scissors). Caption: `more than L bytes? Stop. It's wrong.` A counter in the background
  shows the server was trying to send `∞`.
- **Case C — too short:** the stream ends at 900. Caption: `fewer than L bytes? It's wrong.`
- Small extras that appear as tags on the bar: `pre-allocate exactly L bytes`, `honest progress bar`.
- Caveat card (GOLD): `for hashed content, the final verdict needs the final byte` → small arrow pointing
  ahead to Episode 7.

**Narration**
> This is where that eight-character length prefix pulls its weight.
>
> Before a single byte arrives, you already know exactly how many bytes the right answer contains. For the Gettysburg Address, it's fifteen hundred and eleven. So you can hash the bytes as they stream in, while counting them.
>
> If the stream ends at exactly fifteen hundred and eleven, finish the hash and compare. If the stream ends early, it's wrong, and you know immediately. And here's the important case. If a server keeps sending past fifteen hundred and eleven bytes, it's wrong too. You can hang up on it right then. A malicious server can't make you download a gigabyte of junk, or an endless stream, in order to find out that it's lying. The most it can ever waste is the length you asked for.
>
> Knowing the length up front helps honest transfers, too. You can set aside exactly the right amount of memory, and show an honest progress bar.
>
> There's one limit worth being upfront about. For hashed content, the fingerprint covers the whole file, so you can't be certain the bytes are right until you've seen the last one. You can reject early, but you can only accept at the end. Some other systems make a different trade-off there, and we'll look at that in the final episode.

---

## Scene 4 — Inline CIDs: nothing to fetch
*Target: 4:30–5:05*

**Visuals (Manim)**
- `AAAAAAAFSGVsbG8` arrives at the client. The flowchart from Episode 2 runs: `L = 5 ≤ 64 → decode`.
  The word `Hello` appears. The network cable to the server unplugs itself and floats away.
- Caption: `the name is the content`.

**Narration**
> And for small content, the story is even simpler. If the length is sixty-four bytes or less, there's nothing to fetch. The content is in the name. You don't need a server, you don't need a network, and there is nothing to trust.
>
> Well, almost nothing. There's one subtle trap here, and it's a good one.

---

## Scene 5 — Decode is not verify
*Target: 5:05–7:00*

**Visuals (Manim)**
- Recall `"A" → AAAAAAABQQ`. Show the decoding of `QQ` from Episode 2: `010000 | 01 0000` — the last four bits
  (grey) are just padding and get thrown away by the decoder.
- Now feed `QR` into the same decoder: `010000 | 01 0001`. The final four bits (`0001`) are highlighted RED,
  then discarded. Output: `A`.
- A fan of sixteen strings, `QQ, QR, QS, … Qe, Qf`, all feeding into a lenient decoder; all sixteen produce `A`.
- Second trap, the lying length: `AAAAAAAF` (claims 5 bytes) + `SGk` (decodes to `Hi`, 2 bytes). A naive decoder
  happily returns `Hi`.
- The rule, as a big card with two columns:
  - RED column: `decode the CID → trust the result`
  - GREEN column: `compute the CID of the bytes → compare strings`
- Run the GREEN procedure on `AAAAAAABQR`: compute CID of `A` → `AAAAAAABQQ` → strings differ → reject.
  Run on the lying-length CID → computed `AAAAAAACSGk` → differs → reject.

**Narration**
> Remember how the single byte A became Q Q? The first Q carries six real bits. The second carries the last two real bits, plus four padding zeros. A decoder only needs those two real bits, so many decoders simply ignore the padding.
>
> Which means if someone hands you Q R instead of Q Q, a lenient decoder will throw away those extra bits and still give you the letter A. In fact, any of sixteen different second characters will decode to the same byte. But only one of those strings, with zeros in the padding, is the CID of A.
>
> There's a second version of this trap. A CID's length prefix might claim five bytes, while the characters after it only decode to two. A sloppy decoder might just return the two bytes and never notice the disagreement.
>
> Now, in both cases you do get the right bytes for whatever the string decodes to. The danger is in what you conclude. Two different-looking names for the same content can break anything that uses CIDs as keys, like a cache, a database index, or a list of approved documents. And a name that disagrees with itself should never be accepted as a name.
>
> The fix is the same rule as before, applied consistently. Never verify by decoding a CID and trusting what comes out. Always go the other way: take the bytes, compute their CID with the standard recipe, and compare the strings. The recipe only ever produces one canonical name, so anything else fails the comparison.

---

## Scene 6 — Caching forever
*Target: 7:00–8:45*

**Visuals (Manim)**
- A quote card, attributed: `"There are only two hard things in Computer Science: cache invalidation and naming things."` — Phil Karlton.
  The words `cache invalidation` and `naming things` glow, then merge into a single CID tag.
- An HTTP response slides in. Its headers type out:

  ```
  Cache-Control: public, max-age=31536000, immutable
  ETag: "AAAAAAXnpY8K…"
  ```
  Annotations pop next to each part: `public → any cache may keep it`, `31536000 seconds = one year (the conventional maximum)`,
  `immutable → don't even bother re-checking`, `ETag → the CID itself makes a perfect version tag`.
- A chain of caches: browser → corporate proxy → CDN edge → origin. A request passes through, the copy is stored at every hop
  with a `∞` badge, and later requests are answered at the nearest hop with no trip back to the origin.
- Two caveat tags:
  - `404 Not Found` card with a clock icon: `"not found" describes the server, not the content — don't cache it forever`.
  - `Content-Type: image/png` vs `Content-Type: text/plain` on two copies of the same ribbon with the same CID:
    `headers are hints, not identity`.

**Narration**
> There's an old joke in programming that there are only two hard things: cache invalidation, and naming things. Content addressing is a nice punchline to that joke, because by solving naming in this particular way, it makes cache invalidation disappear.
>
> Think about what makes caching hard. A cache keeps a copy of something so it can answer quickly next time. The hard question is always, when is my copy out of date? But a CID can't go out of date. If it named these bytes yesterday, it names exactly these bytes today, and forever.
>
> So a server can tell every cache along the way to keep it. The 256t FAQ suggests a header like this one. Public, meaning any cache may store it. A max age of one year, which is the conventional way of saying "as long as you like." And immutable, which tells browsers not to even bother checking back. The CID also makes a perfect version tag, since it already changes exactly when the content changes.
>
> Two small cautions. A "not found" response is different. It's a statement about one server at one moment, not about the content, which might be published there tomorrow. So "not found" deserves a short cache lifetime, not a permanent one. And headers like the content type aren't part of the name. The same bytes have the same CID whether a server calls them an image or plain text. Treat that kind of information as a hint.

---

## Scene 7 — True enough, revisited
*Target: 8:45–10:00*

**Visuals (Manim)**
- The cracked "true enough despite being false" card from Episode 1 returns.
- One at a time, each statement gets rewritten from a claim about the world into a rule about what you accept,
  with the crack sealed by a GREEN `verify` stamp:
  - `A CID uniquely determines content` → `I only accept content that has this CID`
  - `The content is immutable` → `Anything else under this name gets rejected`
  - `Content can be cached forever` → `A verified copy never needs re-checking`
- A small row of analogies along the bottom, as icons: `floating-point ≈ real numbers`, `TCP ≈ reliable`,
  `the Earth ≈ flat (for building a house)`. Caption: `useful abstractions with known cracks`.

**Narration**
> Let's go back to that honest list from the first episode. The three things the spec says are "true enough despite being false."
>
> Verification changes what kind of statements those are. "A CID uniquely determines content" stops being a claim about the universe, and becomes a rule about you: you only accept content that has this CID. "The content is immutable" becomes: anything else served under this name gets rejected. And "cache it forever" becomes: once a copy has been verified, it never needs checking again.
>
> Engineering is full of statements like this. Computers treat floating-point numbers as if they were real numbers. Networks treat a stream of unreliable packets as a reliable connection. A carpenter treats the Earth as flat. None of these are true. All of them are true enough, as long as you know exactly where the crack is, and you've decided it's somewhere you can live with. Over the last two episodes, we've measured exactly where 256t's crack is.

---

## Scene 8 — Trust moves to the math
*Target: 10:00–10:35*

**Visuals (Manim)**
- Wide shot: one CID tag in the sky. Below it, an eclectic crowd of sources each holding a copy — a big data
  center, a public library, a phone, a USB stick, a stranger's mirror. Each copy passes through the
  client's `VerifierLoop` and lights GREEN.
- Caption: `trust the name, not the messenger`.

**Narration**
> So here's the payoff. Once your code verifies every CID it receives, it stops mattering who hands you the bytes. A big data center, a library, a friend's phone, a stranger's mirror. The trust lives in the name you started with, and the math that checks it.
>
> Next time, in the last episode, we'll look at what 256t deliberately doesn't do, and how it compares to its neighbors.
