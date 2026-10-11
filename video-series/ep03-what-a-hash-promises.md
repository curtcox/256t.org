# Episode 3 — What a Hash Promises

**Target runtime:** ~11 minutes (≈9.2 min of narration at 150 wpm, plus animation holds) · **Narration:** 1,379 words

**Episode goal:** Give the viewer a working intuition for SHA-512: what it is, why collisions must exist,
the three distinct security games (preimage, second preimage, collision), and which of them 256t actually
relies on. This episode is the foundation for Episodes 4 and 5.

**Facts used (verified with the reference implementation):** SHA-512 of `Hello` and of `hello` differ in
239 of 512 bits (~47%).

**New recurring visuals:** `HashBlender` (an 8-register round-function diagram), `DiffusionGrid`
(a 512-cell heat map of which output bits changed), `FingerprintCard`.

---

## Scene 1 — One letter, two worlds
*Target: 0:00–1:15*

**Visuals (Manim)**
- Left column, GREEN: inline CIDs, aligned so the one differing character lines up:
  - `AAAAAAAFSGVsbG8` ← `Hello`
  - `AAAAAAAFaGVsbG8` ← `hello`
  The single differing character (`S` vs `a`) highlights; everything else is identical.
- Right column, GOLD: the base64url SHA-512 outputs:
  - `NhX4DJ0pPtdAJof5SyLVjlKbjMeRb4-sf933-9WvTPd309eVp6AKFr9-fz-5Vh7puq5IDan-ehh2nnGIawPzFQ`
  - `m3HSJL1i83hdltRq0-o9czGb-8KJDKra4t_3JRlnPKcjI8PZm6XBHXx6zG4UuMXaDEZjR1wuXDre9G9zvN7AQw`
- Both GOLD strings dissolve into 512-cell `BitStrip`s stacked vertically; cells that differ light up
  (`DiffusionGrid`). Counter: `239 of 512 bits differ ≈ 47%`.

**Narration**
> Here are two words that differ in a single letter: Hello with a capital H, and hello with a lowercase one. Their inline CIDs differ in a single character, which makes sense. The CID is just the word, re-encoded.
>
> Now here are their SHA-512 fingerprints. Not one family resemblance. Line up the bits and count, and two hundred and thirty-nine of the five hundred and twelve bits are different. Very close to half. Which is exactly what you'd expect if the two outputs were completely unrelated random numbers.
>
> That's not a quirk of this example. It's the whole design goal. Last time we treated this fingerprint as a black box. Today we open the box, at least a little, and ask a precise question: what exactly is it promising?

---

## Scene 2 — What a hash function is
*Target: 1:15–2:30*

**Visuals (Manim)**
- A `RecipeMachine` relabeled `SHA-512`. Input ribbons of wildly different sizes (one byte, a paragraph,
  a movie) go in one after another; each time the output is the same fixed-width GOLD block, `512 bits`.
- Four property tags pin onto the machine as they're named:
  `any size in` · `fixed size out` · `deterministic` · `fast`.
- A thumbprint image (`FingerprintCard`) appears beside the machine; then a caption with a small RED
  footnote marker: `a fingerprint for data*`.

**Narration**
> A hash function is a recipe that takes any amount of data, from nothing at all to an enormous file, and produces an output of a fixed size. For SHA-512, that's always five hundred and twelve bits. It's deterministic: the same input always gives exactly the same output, on any computer, forever. And it's fast. Your laptop can hash hundreds of megabytes a second.
>
> The usual metaphor is a fingerprint. It's a small thing that stands in for a big thing, and you can compare two fingerprints much more easily than you can compare two whole people.
>
> But metaphors leak, and this one leaks in an important place. So let's find the leak.

---

## Scene 3 — Collisions must exist
*Target: 2:30–4:00*

**Visuals (Manim)**
- Left: a tall stack labeled `all 65-byte files`, with `2^520` beneath it.
- Right: a shorter stack labeled `all SHA-512 outputs`, with `2^512` beneath it.
- Arrows stream from left to right. The right-hand stack fills; pigeonhole icons appear; the overflow
  arrows have nowhere new to go and pile up on existing outputs.
- Compute on screen: `2^520 / 2^512 = 2^8 = 256`. Caption: `on average, 256 different 65-byte files per fingerprint`.
- Inline region inset (GREEN): `≤ 64 bytes: no hashing, so no pigeonholes`.
- The caption `guaranteed to exist` is stamped across the right stack, then a second caption below it:
  `the question is whether anyone can FIND one`.

**Narration**
> Here's the leak. Consider just the files that are exactly sixty-five bytes long, one byte too long to be stored inline. How many are there? Each byte has two hundred fifty-six possibilities, which is eight bits' worth, so there are two to the five hundred and twenty of them.
>
> And how many possible SHA-512 outputs are there? Two to the five hundred and twelve.
>
> More files than fingerprints. So by pure counting, the pigeonhole principle, lots of different files must share a fingerprint. In fact, on average, every possible SHA-512 output is shared by two hundred and fifty-six different sixty-five byte files. And that's just the sixty-five byte files. Collisions don't merely exist. They're everywhere.
>
> So when people say SHA-512 is "collision resistant," they cannot mean there are no collisions. What they mean is something much more interesting. They mean that, although collisions are everywhere, nobody knows how to find even one.
>
> Notice, by the way, that this argument says nothing about inline CIDs. For content of sixty-four bytes or less, there's no hashing, and so no pigeonholes. That part of the name space is collision free by construction.

---

## Scene 4 — Three different games
*Target: 4:00–6:30*

**Visuals (Manim)**
- Three game cards slide in, one per section, each with a fingerprint-and-person illustration and a cost
  meter on the right. Final layout: three cards in a row.
  1. **Preimage.** Given: a fingerprint alone. Goal: find *anyone* with that fingerprint.
     Data version: given a hash, find any input that produces it. Meter: `~2^512 tries`.
  2. **Second preimage.** Given: a specific person, Alice. Goal: find *someone else* whose fingerprint
     matches Alice's. Data version: given a file, find a different file with the same hash.
     Meter: `~2^512 tries`.
  3. **Collision.** Given: nothing. Goal: find *any two people* whose fingerprints match.
     Data version: find any two different inputs with the same hash. Meter: `~2^256 tries`.
- The third meter is visibly lower; a tiny label with a birthday cake icon: `birthday effect — Episode 4`.
- Emphasize that `2^256` is "lower" only in comparison: on a log-scale bar, `2^256` sits exactly halfway
  to `2^512`. Then a second comparison: `2^256 ≈ 1.2 × 10^77` beside `atoms in the observable universe ≈ 10^80`,
  so the viewer sees they're in the same ballpark.

**Narration**
> Security people split "can you find a collision?" into three separate games, and they have very different difficulties. Let's use fingerprints of people, because the analogy works well here.
>
> Game one is called finding a preimage. I hand you a fingerprint, just the print, and you have to find any person in the world who has it. For a hash, that's: given an output, find any input that produces it. If the hash behaves like it's supposed to, the best you can do is guess inputs at random until one hits, and with five hundred and twelve bits of output, that takes on the order of two to the five hundred and twelve tries.
>
> Game two is the second preimage. Now I hand you a specific person, call her Alice, and you have to find somebody else whose fingerprint matches hers. For data: here's a file, now find a different file with the same hash. This is also about two to the five hundred and twelve tries, because you're still trying to hit one particular target.
>
> Game three is the collision. I give you nothing at all. You just have to find any two people, anywhere, whose fingerprints match each other. This turns out to be dramatically easier. You don't need to hit a target, you just need any two of your guesses to hit each other, and the number of pairs grows much faster than the number of guesses. The cost drops to about the square root: two to the two hundred and fifty-six. We'll derive exactly why in the next episode. It's the famous birthday paradox.
>
> "Dramatically easier" is relative, though. Two to the two hundred and fifty-six is a number with seventy-eight digits. That's in the same general neighborhood as the number of atoms in the observable universe.

---

## Scene 5 — A peek inside the blender
*Target: 6:30–8:15*

**Visuals (Manim)**
- **Padding and blocks.** A ribbon gets a `1` bit appended, then zeros, then a 128-bit field labeled
  `message length`. It's then chopped into 1,024-bit blocks.
- **Chaining.** Blocks feed one at a time into a box labeled `compression function`, whose output loops
  back as the starting state for the next block. The final state is the hash.
- **One round (`HashBlender`).** Eight 64-bit registers labeled `a`–`h` in a row. Animate, schematically,
  the operations of one round: rotations (bits sliding around the register and wrapping), XOR, the `Ch`
  and `Maj` bit-selection functions, and additions modulo 2^64 feeding into `a` and `e`. Then everything
  shifts one register to the right. Caption: `× 80 rounds per block`.
  (Schematic is fine; the point is "lots of simple, cheap operations stirred many times," not the exact
  wiring.)
- **Diffusion.** A `DiffusionGrid` (512 cells) showing how a single flipped input bit spreads: after 1
  round only a few cells are lit; after 2, 4, 8, 16 rounds, the lit region grows until it's an even
  half-lit speckle. Freeze on the final speckle.
- Caption at end: `no secret key. Everything is public.`

**Narration**
> So how does SHA-512 actually scramble things this thoroughly? I won't go through every wire, but the shape of it is worth seeing.
>
> First, the input gets padded, and as part of that padding, SHA-512 writes the length of the message into the very end. Then it's chopped into blocks of a thousand and twenty-four bits. Each block gets mixed into a running internal state, and that state is carried forward to the next block. When the last block is done, the final state is the hash.
>
> The mixing itself is made of humble operations. Eight registers of sixty-four bits each. Rotate some bits around. Combine some with exclusive-or. Use one register to choose bits from two others, take a majority vote between three, add things together and let them overflow. Each of these is trivial on its own. Then shift everything over by one, and do it again. Eighty times per block.
>
> Watch what happens to a single flipped input bit. After one round, it's touched almost nothing. A few rounds later, its influence has spread across a chunk of the state. Well before round eighty, every output bit depends on it in a tangled way, and about half of them flip. That's the avalanche we saw at the start.
>
> And here's what I find striking. There is no secret anywhere in this. No key, no hidden ingredient. The algorithm is published, and has been since the early two thousands. Its entire security rests on the fact that, after decades of the world's cryptographers trying, nobody knows how to run it backwards, or to steer its output.

---

## Scene 6 — The ideal, and how real hashes fall short
*Target: 8:15–9:15*

**Visuals (Manim)**
- An enormous ledger (rows scroll endlessly) titled `the ideal hash`. A new input arrives, a coin-flip
  animation generates 512 random bits, and they're written into the ledger next to it. A repeat input
  looks up its existing row.
- Caption: `a perfectly random function: the yardstick`.
- Beside it, SHA-512 as a compact machine. A dotted equals sign between them, labeled
  `indistinguishable (as far as anyone knows)`.
- Small inset: two older machines, `MD5` and `SHA-1`, with cracks running through their equals signs.
  Caption: `Episode 5`.

**Narration**
> Cryptographers have a yardstick for this. Imagine an ideal hash: a gigantic ledger. Whenever a new input shows up, you flip five hundred and twelve fair coins and write the result down next to it. If the same input ever comes back, you look up what you wrote before. Perfectly unpredictable, and perfectly consistent.
>
> No real function can be that ledger. But a good hash is supposed to be indistinguishable from it, for every practical purpose. The costs I quoted for the three games are what the ideal ledger would give you. When real hash functions have failed, it's because someone found a way to tell them apart from the ledger, a shortcut that made one of those games cheaper. That's what happened to the older functions MD5 and SHA-1, and we'll look at those stories in the episode on adversaries.

---

## Scene 7 — What 256t actually needs
*Target: 9:15–10:20*

**Visuals (Manim)**
- The three game cards from Scene 4 return. 256t's needs are checked off:
  - Preimage: dims, with a note `not needed: CIDs aren't secret`.
  - Second preimage: GOLD check, `needed`.
  - Collision: GOLD check, `needed`.
- Warning card (RED border): `a CID does not hide its content`.
  - Inline example: `AAAAAAAH` + `aHVudGVyMg` (BLUE + GREEN) decodes straight back to `hunter2`
    (a joke placeholder password, 7 bytes). Caption: `short content is IN the name`.
  - Hashed example: a target CID, `AAAAAABNODP1lzPl2M2K…`, for a 77-byte form message
    `Board ballot #2026-117. Member vote recorded as follows. Final answer: ______`. Three guesses fill the
    blank in turn (`APPROVE`, `REJECT`, `ABSTAIN`), each is hashed, and the `REJECT` guess's CID matches
    (GREEN). Caption: `guessable content can be confirmed by guessing`.
    (Exact CIDs: APPROVE → `AAAAAABO5jrMwN14…`, REJECT → `AAAAAABNODP1lzPl…`, ABSTAIN → `AAAAAABO8yIArxnn…`.
    Note the length prefix alone already gives `REJECT` away, since it's one byte shorter. Optional bonus
    beat if time allows.)
- Final frame: the identity function (for ≤ 64 bytes) and SHA-512 (for > 64 bytes) side by side, labeled
  `perfect where possible` / `practically perfect where necessary`.

**Narration**
> So which of these three games does 256t actually care about?
>
> Not the first one, it turns out. A CID isn't trying to keep anything secret. For short content, the CID literally contains the content, so recovering the input is trivial, and that's fine. Which leads to a practical warning: a CID is a name, not a lock. Don't treat the CID of something secret as if it hides that secret. If the secret is short, it's sitting right there in the name. And even if it's long, anyone who can guess the possibilities can just compute the CID of each guess and see which one matches.
>
> What 256t does rely on is the second and third games. It needs that nobody can take content you already have and produce different content with the same name. And it needs that nobody can produce two different pieces of content that share a name. Those are the properties that make "this CID means these bytes" true enough.
>
> So 256t's design really comes down to one sentence. Where it can be perfect, it is: the identity function for small content, with no collisions at all. And where it can't be perfect, it uses a function whose collisions are everywhere, but which no one has ever been able to find.
>
> Which raises the next question. If collisions exist, what are the odds you'll ever run into one by accident? That's next time.
