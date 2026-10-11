# Episode 5 — Adversarial Collisions: It's Easier to Lie

**Target runtime:** ~11 minutes (≈9.2 min of narration at 150 wpm, plus animation holds) · **Narration:** 1,383 words

**Episode goal:** Separate the two kinds of adversary (the liar and the forger), show that verification
completely defeats the liar, and give an honest, historically grounded account of what a forger would
need against SHA-512, including the scenario where verification *doesn't* help. End on the spec's two
mitigations: always verify, and reduce adversaries.

**Facts used (sources in `README.md` fact-check table):**
- Bitcoin network ≈ 1 zettahash/second (≈ 10^21 ≈ 2^70 hashes/s), first crossed in late 2025. Note on
  screen that Bitcoin computes SHA-256, and we use it only as a scale for "all the hashing hardware on Earth."
- 2^256 / 2^70 = 2^186 seconds ≈ 3 × 10^48 years ≈ 2 × 10^38 × the age of the universe.
- MD5: published 1992; practical collisions 2004 (Wang et al.); rogue CA certificate 2008; Flame malware
  forged a Microsoft code-signing certificate using a novel chosen-prefix MD5 collision (discovered 2012).
- SHA-1: published 1995; theoretical attack 2005; SHAttered 2017 (two different PDFs, ~2^63 SHA-1
  computations, on the order of 100 GPU-years); "SHA-1 is a Shambles" 2020 (first practical chosen-prefix
  collision, estimated ~US$45k of rented GPU time; used to forge a PGP key certification).
- SHA-512: no collision for the full 80-step function has ever been published. Best practical published
  results: collisions for 27 of 80 steps, and "semi-free-start" collisions (attacker also chooses the
  starting state) for 39 of 80 steps.

**New recurring visuals:** `Liar` and `Forger` characters (two simple masked figures, one holding a
paint-can labeled `wrong bytes`, one holding blueprints), `TwinRibbons` (two different ribbons wired to
one CID tag), `Timeline`.

---

## Scene 1 — The cheapest attack
*Target: 0:00–1:15*

**Visuals (Manim)**
- Quote card: `"It is easier to just lie than engineer a collision."` — `256t.org`
- The `TaxonomyGrid` from Episode 4, with the `adversarial` column now lit.
- Two masked figures walk on stage from opposite sides: the `Liar` (paint can) and the `Forger` (blueprints).
  Name tags appear above them.

**Narration**
> Last time we found that accidental collisions are so unlikely you can stop thinking about them. But an adversary is not an accident. An adversary gets to choose their inputs. They can study the algorithm, rent warehouses of computers, and wait.
>
> The 256t spec has a line about this that I really like. "It is easier to just lie than engineer a collision." That one sentence contains the whole structure of this episode, because there are really two kinds of adversary. There's the liar, who simply hands you the wrong bytes. And there's the forger, who tries to make wrong bytes that genuinely have the right name. Let's take them in order of how much effort they're willing to spend.

---

## Scene 2 — The liar, and the one-line defense
*Target: 1:15–2:45*

**Visuals (Manim)**
- A client requests `CID A` from a `ServerRack` controlled by the `Liar`. The Liar paints over the outgoing
  ribbon (it turns RED) and sends it.
- Client-side `VerifierLoop`: received ribbon → `RecipeMachine` → computed `CID B` → compared against `CID A`.
  Mismatch → large RED X; the ribbon is dropped into a bin.
- Repeat at triple speed with a dozen different wrong ribbons; every one bounces.
- Caption: `cost to defend: one hash computation. Success rate of lying: 0%.`
- Small inset of the actual Python reference code doing this:

  ```python
  content = response.read()
  computed = compute_cid(content)
  return content, computed, computed == cid
  ```

**Narration**
> The liar's strategy is to serve you something other than what you asked for. Maybe it's malware instead of the installer. Maybe it's an edited version of a document. It costs them nothing, and if you don't check, it works every time.
>
> But we know how to check. The name is computed from the content, so when the bytes arrive, you compute their CID yourself and compare. If the liar sent anything other than the real content, the CID won't match, and you throw it away.
>
> This defense is absolute in a way that's unusual in security. It doesn't depend on how clever the liar is, or how many computers they have. Any bytes other than the right ones produce a different CID, unless the liar has found a genuine collision. And that's the forger's job, not the liar's. So verification takes the cheapest attack there is, and turns it into the most expensive one.

---

## Scene 3 — Two jobs for the forger
*Target: 2:45–5:15*

**Visuals (Manim)**
- The `Forger` unrolls two blueprints, side by side.
- **Blueprint 1: second preimage.** A ribbon labeled `your file (not made by the forger)` and its CID. The
  Forger must build a *different* ribbon with that same CID. Cost meter: `~2^512`.
- **Blueprint 2: collision (the twins).** The Forger builds *both* ribbons from scratch: `benign` (GREEN tint)
  and `malicious` (RED tint), wired to one shared CID tag (`TwinRibbons`). Cost meter: `~2^256` (birthday).
- **The twins scenario, animated as a short story:**
  1. The Forger hands you a contract, `benign`. You read it, approve it, and write down its CID.
  2. Later, you fetch "that CID" from a server the Forger influences. You receive `malicious`.
  3. Your `VerifierLoop` runs: the computed CID matches. GREEN check. (Hold on this; let it feel wrong.)
- A big question card: `who made the original?`

**Narration**
> The forger wants something harder: different bytes that really do have the right name. Remember the three games from episode three? The forger is playing one of the last two, and which one depends on a question that's easy to skip past. Who made the original?
>
> Suppose the original was made by someone else. It's your file, or a document published by somebody honest. Then the forger has to find a second preimage, different content with the same CID as content they had no part in creating. With SHA-512, the generic cost of that is around two to the five hundred twelve tries. That's not happening.
>
> But now suppose the forger made the original. Then they get to play the easier game. They don't need to hit your target. They just need any two documents of their own design with matching names. One harmless, one harmful. That's a collision, and thanks to the birthday effect, the generic cost drops to about two to the two hundred fifty-six.
>
> Here's why that matters. Picture it as a story. The forger sends you a contract. You read it carefully, you approve it, and you record its CID. Later, you fetch that CID, from somewhere the forger has influence, and you get the other twin. You verify it. And the check passes. Because, as far as 256t is concerned, both twins genuinely have that name.
>
> So verification defeats the liar completely, but it can't defeat a true collision. That's the honest crack in "a CID uniquely determines its content." The question is how wide the crack is.

---

## Scene 4 — How big is two to the two hundred fifty-six, really?
*Target: 5:15–6:45*

**Visuals (Manim)**
- An aerial shot of stylized warehouses of mining rigs. Label: `Bitcoin network ≈ 10^21 hashes per second ≈ 2^70/s`.
  Small footnote: `(SHA-256, not SHA-512 — used here only as a scale)`.
- Division on screen: `2^256 / 2^70 = 2^186 seconds`.
- Convert: `≈ 3 × 10^48 years` → a timeline where `age of the universe` is a single tick, and the bar for the
  attack extends off to the right forever. Caption: `≈ 2 × 10^38 universe-lifetimes`.
- Quantum card: a stylized quantum chip. `best known quantum algorithms lower the exponent — but leave numbers
  like 2^170` (shown as a 52-digit number scrolling past).

**Narration**
> So, is two to the two hundred fifty-six a lot? Let's use the biggest pile of hashing hardware humanity has ever built: the Bitcoin network. As of late 2025 it was doing around a zettahash per second, about ten to the twenty-one hashes every second, which is roughly two to the seventy. It computes a different hash, SHA-256, but it gives us a sense of scale for "every specialized hashing machine on Earth."
>
> Point all of that at a SHA-512 collision by brute force. Two to the two hundred fifty-six, divided by two to the seventy per second, is two to the one hundred eighty-six seconds. That's around three times ten to the forty-eight years. The universe is about fourteen billion years old. You'd need it to happen again roughly ten to the thirty-eight times over.
>
> And quantum computers? The best known quantum algorithms do lower the exponents for these problems. But even under optimistic assumptions about future hardware, the work for a SHA-512 collision stays at numbers like two to the hundred and seventy, which has more than fifty digits. Brute force is simply not how anyone will forge a 256t name.

---

## Scene 5 — How hash functions actually die
*Target: 6:45–9:00*

**Visuals (Manim)**
- `Timeline` from 1990 to today, with two tracks.
- **MD5 track (128-bit output, birthday bound 2^64):**
  - `1992` published.
  - `2004` collisions found by Wang et al. — two RED ribbons wired to one tag.
  - `2008` researchers use a collision to create a rogue certificate authority certificate.
  - `2012` the Flame malware is found using a new chosen-prefix MD5 collision to forge a Microsoft
    code-signing certificate. Icon: an operating system update arrow with a mask on it.
- **SHA-1 track (160-bit output, birthday bound 2^80):**
  - `1995` published.
  - `2005` theoretical shortcut published.
  - `2017` SHAttered: two different PDFs with the same SHA-1. `~2^63 computations, ~100 GPU-years`.
    Show two PDF icons, visibly different, with one shared hash.
  - `2020` "SHA-1 is a Shambles": first practical chosen-prefix collision, estimated around `$45k` of rented GPUs.
- Three takeaway cards slide up from the timeline:
  1. `They broke through shortcuts, not brute force.`
  2. `There were years of warning.`
  3. `Their outputs were small: 2^64 and 2^80 birthday bounds.`
- Brief explainer box for "chosen-prefix": two *meaningful* different documents (e.g. two certificates with
  different names), each with a block of gibberish appended, that hash the same.

**Narration**
> Here's the thing, though. Real hash functions have been broken. Just never by brute force. Let's look at how it actually happened.
>
> MD5 was published in 1992, with a hundred-and-twenty-eight bit output. In 2004, a team led by Xiaoyun Wang showed how to find MD5 collisions quickly, not by trying more inputs, but by exploiting structure in how MD5 mixes its bits. Within a few years, researchers used MD5 collisions to forge a certificate authority certificate. And in 2012, a piece of espionage malware called Flame was found using a previously unknown MD5 collision technique to forge a Microsoft code-signing certificate, so that it could spread disguised as a legitimate software update. That's the twins scenario, exactly, in the wild.
>
> SHA-1, with a hundred-and-sixty bit output, followed a similar arc, just more slowly. A theoretical shortcut was published in 2005. In 2017, researchers from Google and CWI Amsterdam produced two different PDF files with the same SHA-1 hash, at a cost of around two to the sixty-three computations, on the order of a hundred GPU-years. And by 2020, a stronger form called a chosen-prefix collision, where both documents can say whatever the attacker wants, plus a bit of gibberish, was estimated to cost in the tens of thousands of dollars.
>
> Three lessons. First, these functions died from shortcuts, cleverness about their internal structure, not raw computing power. Second, there were years of warning between the first cracks and practical attacks. And third, their outputs were small. MD5's birthday bound was only two to the sixty-four, and SHA-1's two to the eighty. They didn't have much margin to lose.

---

## Scene 6 — SHA-512 today
*Target: 9:00–10:00*

**Visuals (Manim)**
- A third track on the timeline: `SHA-512`, `512-bit output`, `birthday bound 2^256`. Published early 2000s.
  Then a long, empty stretch to today, labeled `no collision ever found`.
- A progress-bar style figure: `80 steps` total. Two shaded regions:
  - `27 / 80` — `best practical collision on a reduced version`
  - `39 / 80` — `best practical "semi-free-start" collision (attacker also picks the starting state)`
  The rest of the bar is untouched. Caption: `the full function: untouched`.

**Narration**
> So where does SHA-512 stand? It's been around for over two decades, it's one of the most studied functions in cryptography, and no collision for it has ever been found.
>
> Researchers do probe it, the way they probed MD5 and SHA-1. They attack weakened versions with fewer rounds. The best practical published collisions reach twenty-seven of its eighty steps. In an even more favorable setting, where the attacker also gets to choose the internal starting state, which a real attacker can't, they reach thirty-nine steps. That's a meaningful distance from the full eighty. And its birthday bound starts at two to the two hundred fifty-six, so even a shortcut that knocked off a huge chunk of that would leave an astronomical number behind.
>
> Could that change someday? In principle. But if history is a guide, we'd see the cracks coming years ahead.

---

## Scene 7 — What 256t's structure does and doesn't buy
*Target: 10:00–10:45*

**Visuals (Manim)**
- Three short cards:
  - GREEN padlock: `inline CIDs (≤ 64 bytes): no forging possible, ever`.
  - BLUE: `length prefix: forger just builds twins of equal length — little help here` (a shrug icon).
  - GOLD: `everything else: as strong as SHA-512 collision resistance`.

**Narration**
> Does 256t's structure add anything here? Partly. Inline CIDs can't be forged at all, since the content is the name. The length prefix, honestly, doesn't help much against a forger: someone building twins just makes them the same length. It helps a lot against a different kind of misbehavior, which we'll see next episode. For everything else, a 256t name is exactly as strong as SHA-512's collision resistance.

---

## Scene 8 — The two defenses
*Target: 10:45–12:00*

**Visuals (Manim)**
- A gate chain for a *problem* collision caused by an adversary, each gate a filter that shrinks the flow:
  1. `adversary finds a real SHA-512 collision` (narrowest gate by far — nearly closed)
  2. `adversary authored the content you rely on`
  3. `you fetch from a source the adversary controls`
  Without gate 1, the attacker is just a `Liar` → the `VerifierLoop` catches it.
- Two large cards to close, matching the spec's wording:
  - `Always verify CID content.`
  - `Reduce adversaries.`
- Fade the cards into the `VerifierLoop` glowing GREEN, with a few `ServerRack`s behind it.

**Narration**
> Let's put it together. For an adversary to cause a real problem collision, three things have to line up. They have to find a genuine SHA-512 collision, which no one has ever done. They have to be the author of content you're relying on, so they can plant one twin. And they have to be in a position to hand you the other. Without the first, they're just a liar, and verification catches them every time.
>
> And so the spec's advice comes down to two habits. First: always verify. It's one hash computation, and it reduces every cheap attack to the expensive one. Second: reduce adversaries. Be thoughtful about trusting content whose author would benefit from swapping it, and about where you fetch from. If nobody is putting malicious content where you might accept it, you're back to accidents only, and we know how that story ends.
>
> Next time, we'll get concrete about the first habit. What exactly should your code do when bytes arrive? And how does that let you cache them forever?
