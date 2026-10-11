# Episode 4 — Accidental Collisions

**Target runtime:** ~10 minutes (≈8.6 min of narration at 150 wpm, plus animation holds) · **Narration:** 1,294 words

**Episode goal:** Introduce the spec's collision taxonomy, derive the birthday bound from scratch, and
then put an honest, generous number on the chance that two different files ever share a CID by accident.
Land the spec's position, "I'm comfortable just ignoring accidental problem collisions," as a
conclusion the viewer reaches themselves.

**Numbers used (all computed; see `README.md` fact-check table):**
- Birthday: 23 people → 253 pairs → P(shared birthday) ≈ 50.7%.
- Approximation: P ≈ n² / (2N). With N = 2^512: P ≈ n² / 2^513.
- Generous civilization: 8 × 10^9 people × 10^9 files/second × 4.35 × 10^17 seconds (13.8 billion
  years) ≈ 3.5 × 10^36 ≈ 2^121 files → P ≈ 2^(242 − 513) ≈ 2^−271.
- Atoms in the observable universe ≈ 10^80 ≈ 2^266.

**New recurring visuals:** `TaxonomyGrid` (4 cards), `PairWeb` (dots connected by all pairwise lines),
`ExponentLadder`.

---

## Scene 1 — Four kinds of collision
*Target: 0:00–1:30*

**Visuals (Manim)**
- `TaxonomyGrid`: four cards arrive, each with an icon. Text on each card is the spec's own definition:
  - `accidental` (dice) — `purely by chance`
  - `adversarial` (mask) — `someone tried to cause it`
  - `existing` (two ribbons merging into one tag) — `a CID has been produced from different content`
  - `problem` (request arrow → wrong ribbon returned, RED) — `usage of the CID to get content returned the wrong content`
- Draw the cards as two axes rather than four boxes: horizontal axis `how did it happen? accidental ↔ adversarial`,
  vertical axis `how far did it get? existing → problem`.
- The `accidental` column highlights. Caption: `this episode`.

**Narration**
> We've established that collisions in SHA-512 must exist. Now let's ask whether you should ever worry about them. And the first step is to notice that the word "collision" is doing too much work.
>
> The 256t spec splits it up. A collision can be accidental, purely by chance, or adversarial, meaning someone tried to cause it. And separately, a collision can be merely existing, meaning two different pieces of content have actually produced the same CID somewhere, or it can be a problem, meaning someone used a CID to fetch content, and got the wrong content back.
>
> Those are really two different questions. How did it happen? And did it actually hurt anyone? This episode is about the accidental kind, where nobody is trying to fool anyone. We just want to know: if the whole world keeps making files, will two of them ever land on the same name by chance?

---

## Scene 2 — Where collisions can't happen
*Target: 1:30–2:30*

**Visuals (Manim)**
- The space of all CIDs drawn as a long row of bins, one per content length: `0`, `1`, `2`, … `64`, `65`, … `2^48 − 1`.
- Bins `0` through `64` are GREEN and labeled `inline: no collisions possible`. They lock with a padlock icon.
- A RED "collision spark" tries to jump between two different-length bins (say `1,000` and `1,001`) and
  bounces off a BLUE wall labeled `different length prefix`.
- The remaining arena: the GOLD bins from `65` up. Zoom into one bin (`1,511 bytes`), which becomes a huge
  starfield: `2^512 possible fingerprints`.

**Narration**
> Before we compute anything, let's shrink the battlefield.
>
> First, anything sixty-four bytes or smaller stores itself inline, so two different small files can never share a CID. That whole region is collision-free.
>
> Second, the length is part of the name. So two files of different lengths can never collide either, no matter what their fingerprints are. A collision only counts if two files have exactly the same length, and exactly the same SHA-512 hash.
>
> So the real question lives inside one of these bins. Pick a length, say fifteen hundred and eleven bytes. Inside that bin are two to the five hundred and twelve possible fingerprints. If we drop files in at random, how many can we drop before two of them are likely to land on the same one?

---

## Scene 3 — The birthday paradox
*Target: 2:30–5:00*

**Visuals (Manim)**
- A circular "calendar" of 365 tick marks. People (dots) drop in one at a time, each landing on a random day.
- Counter: `people: n`. A second counter: `pairs: n(n−1)/2`.
- `PairWeb`: as each new dot appears, lines draw from it to every existing dot, so the number of lines visibly
  explodes. At `n = 23`, freeze: `253 pairs`.
- Side computation, written out:
  - `each pair matches with probability 1/365`
  - `expected matching pairs ≈ 253 / 365 ≈ 0.69`
  - `P(at least one match) ≈ 1 − e^(−253/365) ≈ 50%` (exact: `50.7%`)
- Generalize: `365` morphs into `N` (number of possible values), and `n(n−1)/2` simplifies to `≈ n²/2`.
  Boxed result: `P(collision) ≈ n² / (2N)` (valid while this is small).
- Final beat of the scene: the threshold where P becomes about one half: `n ≈ √N`. Show `√365 ≈ 19` next to `23`
  ("same ballpark").

**Narration**
> This is the birthday problem, and it's one of those results that feels wrong the first time you see it. How many people do you need in a room before there's a fifty-fifty chance that two of them share a birthday? The answer is just twenty-three.
>
> The intuition that fails is thinking about you. The chance that someone shares your birthday is small. But a shared birthday doesn't have to involve you. It can be any pair of people in the room. And the number of pairs grows much faster than the number of people.
>
> With twenty-three people there are twenty-three times twenty-two over two pairs. Two hundred and fifty-three of them. Each pair has a one-in-three-sixty-five chance of matching. So you expect about two hundred fifty-three over three hundred sixty-five matches, around zero point seven, and that makes the chance of at least one match roughly a half.
>
> Now let's make that general. Instead of three hundred sixty-five days, say there are N possible values. With n items, there are about n squared over two pairs. Each pair matches with probability one over N. So as long as the answer is small, the chance of any collision at all is about n squared, over two N.
>
> The key thing to notice is that n is squared. Collisions start becoming likely once n gets to about the square root of N. That square root is exactly why, last episode, the cost of finding a collision dropped from two to the five hundred twelve to two to the two hundred fifty-six. It's the same birthday effect.

---

## Scene 4 — Scaling up to SHA-512
*Target: 5:00–6:45*

**Visuals (Manim)**
- Substitute: `N = 2^512` → `P ≈ n² / 2^513`.
- `ExponentLadder`: a vertical log scale with powers of two as rungs. A slider for `n` on the left and the
  resulting `P` on the right. Rule shown: `each doubling of n moves P up by 2 rungs`.
- Table builds row by row:

  | files (n) | roughly | P(any accidental collision) |
  |---|---|---|
  | 2^40 | a trillion | 2^−433 |
  | 2^64 | 18 quintillion | 2^−385 |
  | 2^100 | 10^30 | 2^−313 |
  | 2^128 | 3 × 10^38 | 2^−257 |
  | 2^256 | 10^77 | ~ 1/2 |

- The last row glows RED, the others stay calm. Caption next to it: `this many files is not a thing that happens`.

**Narration**
> Now plug in SHA-512. N is two to the five hundred twelve, so the chance of any accidental collision among n files is about n squared, over two to the five hundred thirteen.
>
> The nice thing about writing everything in powers of two is that the arithmetic becomes counting. Every time you double the number of files, the probability goes up by a factor of four, which is two rungs on this ladder.
>
> A trillion files is about two to the forty. Square it: two to the eighty. Divide by two to the five hundred thirteen. So the chance is about one in two to the four hundred thirty-three.
>
> Eighteen quintillion files, two to the sixty-four: one in two to the three hundred eighty-five. Go all the way up to two to the one hundred twenty-eight files, a number with thirty-nine digits: still one in two to the two hundred fifty-seven. You don't get to a coin flip until somewhere around two to the two hundred fifty-six files.
>
> These exponents are so lopsided that it's easy to stop feeling them. So let's try to make one concrete.

---

## Scene 5 — A generous civilization
*Target: 6:45–8:30*

**Visuals (Manim)**
- A globe surrounded by a swarm of tiny people icons (`8 billion`). Each icon spits out a rapid stream of
  tiny ribbons (`a billion files per second, each`).
- A timeline from the Big Bang to today zooms out (`13.8 billion years`), and the whole swarm is drawn as
  having started at the Big Bang.
- Multiply on screen:
  `8 × 10^9 people × 10^9 files/s × 4.35 × 10^17 s ≈ 3.5 × 10^36 ≈ 2^121 files`
- Plug in: `P ≈ (2^121)² / 2^513 = 2^242 / 2^513 = 2^−271`.
- Comparison card: `atoms in the observable universe ≈ 10^80 ≈ 2^266`. Animation: one particular atom
  is highlighted somewhere in a cosmic-web image, and a blindfolded hand picks one at random. Caption:
  `P(picking that exact atom) ≈ 2^−266`. Then a scale bar shows `2^−271` is about 32 times smaller still.
- Footnote in small type: `and this lumps every file into a single bin; requiring equal lengths only makes it smaller`.

**Narration**
> Let's be absurdly generous. Imagine every one of the eight billion people alive today producing a billion new files every second. And imagine they've been doing it since the Big Bang, thirteen point eight billion years ago.
>
> That's eight billion, times a billion, times about four times ten to the seventeen seconds. Roughly three and a half times ten to the thirty-six files, which is about two to the one hundred twenty-one.
>
> Square that, and you get two to the two hundred forty-two. Divide by two to the five hundred thirteen. The chance that any two of those files, anywhere, ever, accidentally share a SHA-512 hash, is about one in two to the two hundred seventy-one.
>
> For comparison, there are roughly ten to the eighty atoms in the observable universe, which is about two to the two hundred sixty-six. So picture one particular atom, somewhere out there in all of it. Now pick an atom at random from the entire universe. Your chance of picking that exact one is better, by a factor of about thirty, than the chance of a single accidental collision in our imaginary civilization's entire history.
>
> And that's with an unfair simplification in favor of collisions. We lumped every file together in one bin, when, remember, a collision only counts between files of exactly the same length.

---

## Scene 6 — What actually goes wrong
*Target: 8:30–9:30*

**Visuals (Manim)**
- A bar chart on a log scale titled `things that can make a CID check give the wrong answer`. Bars, qualitatively
  ordered (no fabricated numbers; label them by kind, not by rate):
  - `software bugs`
  - `disk and network corruption`
  - `memory bit flips (e.g. cosmic rays)`
  - `accidental SHA-512 collision` — a bar so short it renders as a single pixel, then the axis zooms out and
    it disappears entirely; an arrow points to where it would be.
- Caption: `spend your worry where the risk is`.

**Narration**
> This is the right way to think about numbers like these. Not "is the risk zero?" It isn't. But "how does this risk compare to the other things that can go wrong?"
>
> Every real system has failure modes. Software has bugs. Disks and networks corrupt data. Every so often a stray cosmic ray flips a bit in a computer's memory. Every one of those is enormously more likely than an accidental SHA-512 collision. If the machine checking a CID is going to give you the wrong answer, it will be because the machine broke, not because the math did.
>
> So any engineering effort spent defending against accidental collisions is effort that could have gone toward risks that actually happen.

---

## Scene 7 — Existing is not the same as problem
*Target: 9:30–10:20*

**Visuals (Manim)**
- Return to the `TaxonomyGrid`'s vertical axis. Draw nested regions: a large circle `existing collisions`, and
  inside it a much smaller circle `problem collisions`.
- A three-step chain lights up for a problem collision, each step a gate:
  1. `two different files share a CID` (already ~2^−271 by accident)
  2. `both end up in a place you fetch from`
  3. `you ask for one and are handed the other`
- Each gate shrinks the probability further (bars get shorter).

**Narration**
> There's one more layer of reassurance, and it's where the spec's distinction between existing and problem collisions earns its keep.
>
> Suppose, against all those odds, an accidental collision did exist. Two files somewhere really do share a CID. For that to actually hurt anyone, both files would have to end up somewhere you fetch from, and you would have to ask for one and be handed the other. An existing collision only becomes a problem collision if all of that also happens. Each of those steps makes an already unimaginably unlikely event less likely still.

---

## Scene 8 — The assumption, and the verdict
*Target: 10:20–11:00*

**Visuals (Manim)**
- A small card: `assumption: for inputs nobody designed to break it, SHA-512 behaves like the ideal ledger`.
  Beside it: two nearly identical files (one byte different) map to unrelated fingerprints, recalling Episode 3.
- Quote card from the spec: `"I'm comfortable just ignoring accidental problem collisions."` — `256t.org`
- The dice icon on the taxonomy grid fades to grey. The mask icon (`adversarial`) brightens and turns toward
  the camera.

**Narration**
> All of this rests on one assumption. For inputs that nobody designed to break it, SHA-512 behaves like the ideal coin-flipping ledger. And that's a mild assumption. Similar files don't produce similar fingerprints, so the ordinary structure in real data, like lots of files that start the same way, doesn't help collisions along. And in the more than two decades SHA-512 has been in use, nobody has ever found a collision of any kind, accidental or otherwise.
>
> So the spec's position is: "I'm comfortable just ignoring accidental problem collisions." Having done the arithmetic, I think that's not a shrug. It's a conclusion.
>
> But we've been assuming the universe is merely indifferent. What if someone is actively trying to fool you? Adversaries don't roll dice. That's next.
