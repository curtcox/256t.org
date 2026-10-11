# Episode 2 — Anatomy of a CID

**Target runtime:** ~11 minutes (≈9.5 min of narration at 150 wpm, plus animation holds) · **Narration:** 1,431 words

**Episode goal:** By the end, the viewer can decode a small CID by hand, read the length out of any CID,
and explain where 8, 64, 86, 94, and "256" come from. Every number in the format should feel derived,
not memorized.

**All CIDs and encodings shown in this episode were computed with the reference implementation
(`implementations/python/cid.py`) and are exact.**

**New recurring visuals:** `BitStrip` (a row of 0/1 cells), `Base64Gear` (a 3-byte ↔ 4-character
regrouping animation), `LengthOdometer`.

---

## Scene 1 — Three names
*Target: 0:00–0:50*

**Visuals (Manim)**
- Three CIDs stacked vertically, left-aligned, in monospace:
  - `AAAAAAABQQ`
  - `AAAAAAAFSGVsbG8`
  - `AAAAAAXnpY8KkcoLHubRMUTViZU671vd9x4IuH0gmZhLvp9TTl-K-J3UzKZCbOUP41fDBlxdcJHSTd7bVYAZvwvibsdFjA`
- Their right-hand sides are covered by grey "?" boxes labeled `what is this?`.
- Lots of leading `A`s get a subtle shimmer.

**Narration**
> Here are three CIDs. Right now they probably look like noise. By the end of this video, you'll be able to read the first one by hand, read the length out of all three, and explain where every number in the format comes from. There's surprisingly little in here that's arbitrary.
>
> And you might already have noticed something odd. Why do they all start with so many capital A's? Keep that question in mind. It has a nice answer.

---

## Scene 2 — Bits, bytes, and six-bit characters
*Target: 0:50–3:00*

**Visuals (Manim)**
- A single byte appears as a `BitStrip` of 8 cells: `0 1 0 0 0 0 0 1`, with `= 65 = "A"` beside it.
- The base64url alphabet fans out as a 64-cell table, colored in four bands:
  `A–Z → 0–25`, `a–z → 26–51`, `0–9 → 52–61`, `- → 62`, `_ → 63`.
- Under the table: `64 symbols = 2^6 → each character carries exactly 6 bits`.
- A small inset shows standard base64's `+` and `/` circled in RED, morphing into `-` and `_`.
  A trailing `==` falls into a bin labeled `padding: dropped`.
- **The gear ratio** (`Base64Gear`): three bytes (24 bits) laid end to end. A cutter slides along and
  re-slices them into four 6-bit groups. Caption: `3 bytes = 24 bits = 4 characters`.
- **Worked example: one byte.** The byte for `A` (`01000001`). Slice into `010000 | 01····`.
  The missing four bits fill in with `0000` (shown in grey). The groups become `16` and `16`, which look up as
  `Q` and `Q`. Result: `QQ`.

**On-screen text**
- `base64url (RFC 4648 §5)`
- `"A" → 01000001 → 010000 | 010000 → Q Q`

**Narration**
> Everything in a CID is made of bytes, and every byte is eight bits. The letter capital A, for instance, is stored as the byte zero one zero zero zero zero zero one, which is the number sixty-five.
>
> But eight-bit bytes are awkward to put in a web address, because most byte values aren't printable characters. So 256t uses an encoding called base64url. It has an alphabet of exactly sixty-four characters: the capital letters, the lowercase letters, the ten digits, and then a dash and an underscore. And sixty-four is two to the sixth, so each character carries exactly six bits of information. No more, no less.
>
> Quick aside on that "url" part. Ordinary base64 uses a plus sign and a slash for its last two characters, and those both mean something special inside a web address. The URL-safe variant swaps them for a dash and an underscore. It also drops the equals signs that ordinary base64 tacks on as padding at the end. So everything here can sit in a URL or a filename untouched.
>
> Now here is the little gear that makes the whole format click. Eight and six don't line up evenly. But three bytes is twenty-four bits, and twenty-four bits is exactly four characters of six bits. So base64 works like a gear: every three bytes in, four characters out.
>
> Let's do one by hand. Take the single byte for capital A. Chop off the first six bits: zero one zero zero zero zero. That's sixteen, and position sixteen in the alphabet is the letter Q. We have two bits left over, zero one. Pad them out with zeros to make a full six-bit group, and that's sixteen again. Another Q. So the single byte "A" becomes the two characters "QQ."
>
> Look back at the first CID. It ends in Q Q.

---

## Scene 3 — The length prefix
*Target: 3:00–5:15*

**Visuals (Manim)**
- The first CID `AAAAAAABQQ` again. A brace under the first 8 characters, colored BLUE: `length`.
- Those 8 characters expand downward into a 48-cell `BitStrip`: forty-seven 0s and a final 1.
  Readout: `= 1 byte`.
- **Why 48 bits?** Show 6 bytes → 48 bits → `Base64Gear` turns exactly twice → 8 characters, with no
  grey padding cells anywhere. Caption: `6 bytes → exactly 8 characters, no leftovers`.
- A vertical line marks the boundary after character 8. Caption: `the next part starts on a clean character boundary`.
- **`LengthOdometer`**: an 8-character odometer, each wheel a base-64 digit. It rolls through these
  values, with a byte-count readout and a familiar-size label:

  | length | prefix | label |
  |---|---|---|
  | 0 | `AAAAAAAA` | empty file |
  | 1 | `AAAAAAAB` | one byte |
  | 64 | `AAAAAABA` | |
  | 1,000 | `AAAAAAPo` | |
  | 1,511 | `AAAAAAXn` | Gettysburg Address |
  | 1,048,576 | `AAAAEAAA` | 1 MiB |
  | 6,059,375 | `AAAAXHVv` | the 256t logo PNG |
  | 1,073,741,824 | `AABAAAAA` | 1 GiB |
  | 1,099,511,627,776 | `AQAAAAAA` | 1 TiB |
  | 281,474,976,710,655 | `________` | the maximum |

- **Decode by hand:** `AAAAAAXn`. Six A's are zero. `X` = 23, `n` = 39. Show `23 × 64 + 39 = 1,511`.
- **The name:** `2^48 = 2^8 · 2^10 · 2^10 · 2^10 · 2^10 = 256 · Ki · Mi · Gi · Ti` → `256 TiB` →
  the "256t" logo assembles out of the expression.
- Back to the opening question: the leading `A`s light up across all three CIDs from Scene 1.
  Caption: `A = 0. Real files are small compared to 256 TiB.`

**Narration**
> The first eight characters of every CID are the length of the content, in bytes.
>
> Eight characters at six bits each is forty-eight bits. And notice how well that fits our gear. Forty-eight bits is exactly six bytes, which turns the gear exactly twice and produces exactly eight characters, with nothing left over and no padding. So the length always takes up precisely eight characters, and whatever comes after it starts on a clean character boundary. I don't think that's an accident.
>
> You can read the length like an odometer, where each wheel counts in base sixty-four instead of base ten. The empty file has length zero, so its prefix is eight A's, because A is the zero digit. A one-byte file is seven A's and a B. Let's decode the Gettysburg Address. The first six wheels are A, so they're zero. Then X, which is twenty-three, and lowercase n, which is thirty-nine. Twenty-three times sixty-four, plus thirty-nine, is fifteen hundred and eleven. That's exactly how many bytes are in the file.
>
> So how big can the content be? Forty-eight bits can count up to two to the forty-eighth. Split that up as two to the eighth, times two to the tenth, four times. Two to the tenth is roughly a thousand, the "kilo" step, and each further factor is the next step up: kilo, mega, giga, tera. So two to the forty-eight bytes is two hundred and fifty-six terabytes, in the power-of-two sense. Two hundred fifty-six T. That's where the name comes from.
>
> And now the A's make sense. A means zero. Two hundred fifty-six terabytes is enormous compared to almost any real file, so the high wheels of the odometer almost never move. A one-megabyte file is four A's, an E, and three more A's. A gigabyte only gets you two A's, a B, and five more A's. You need a full terabyte before that second character even budges.

---

## Scene 4 — The fingerprint
*Target: 5:15–6:30*

**Visuals (Manim)**
- A long ribbon (the Gettysburg text) enters a `RecipeMachine` labeled `SHA-512`. Out comes a block of
  64 bytes, which unrolls into a 512-cell `BitStrip`.
- The bit strip is sliced into 6-bit groups. A counter ticks: `85 full characters`, then
  `2 bits left over` (highlight), padded with four grey zeros → `86th character`.
- On-screen math: `512 / 6 = 85.33… → 86 characters`.
- The 86 GOLD characters snap onto the 8 BLUE ones. Ruler underneath, numbered `1 … 94`.
  Caption: `8 + 86 = 94`.

**Narration**
> Now the rest of the name. For content longer than sixty-four bytes, the next part is the SHA-512 hash of the content. We'll look at what that function does in the next episode. For today, all that matters is that it always produces exactly five hundred and twelve bits, which is sixty-four bytes, no matter whether you feed it a sentence or a movie.
>
> How many base64 characters does it take to hold five hundred and twelve bits? Five hundred twelve divided by six is eighty-five and a third. So eighty-five characters isn't quite enough, and we need eighty-six, with a few padding zeros in the last one.
>
> Eight characters of length, plus eighty-six of fingerprint. Ninety-four. That's the maximum length of a CID, and it's the length of nearly every CID you'll ever see.

---

## Scene 5 — Small things carry themselves
*Target: 6:30–8:45*

**Visuals (Manim)**
- Axes: x-axis `content length (bytes)` from 0 to 80; y-axis `characters after the prefix` from 0 to 110.
- A flat GOLD line at `y = 86`, labeled `hash: always 86`.
- A GREEN staircase rising from the origin, labeled `inline: ⌈4n/3⌉`. Stepping every 3 bytes by 4 characters
  (with partial steps of 2 and 3 characters for 1 and 2 leftover bytes).
- The staircase climbs and touches the flat line exactly at `n = 64` (dot + label `64 bytes → 86 characters`).
  At `n = 65` the staircase would be `87` (dashed, above the line, RED).
- Shade the region left of 64 GREEN (`inline`) and right of 64 GOLD (`hashed`).
- Insets with real examples:
  - `"A"` → `AAAAAAAB` + `QQ`
  - `"Hello"` → `AAAAAAAF` + `SGVsbG8`
  - `"Hello, world!"` (13 bytes) → `AAAAAAAN` + `SGVsbG8sIHdvcmxkIQ`
  - 64 × `x` → `AAAAAABA` + `eHh4…eA` (86 chars, total 94)
  - 65 × `x` → `AAAAAABB` + `Rg2doReeYM-O5F0s…` (86 chars, total 94)
- Side note card (optional, can be cut for time): `distinct contents of ≤ 64 bytes ≈ 2^512`, next to
  `possible SHA-512 outputs = 2^512`. Caption: `both fill exactly 86 characters`.

**Narration**
> Remember the sticky note from last episode? Using the content itself as its own name is perfect, except that big content makes a big name. But a fingerprint costs eighty-six characters no matter what. So for tiny content, the fingerprint is actually longer than the thing it's fingerprinting.
>
> Let's plot it. Along the bottom, the length of the content in bytes. Up the side, how many characters we'd need after the length prefix. The hash is a flat line at eighty-six. Writing the content out directly is this staircase, climbing four characters for every three bytes.
>
> The staircase meets the flat line at exactly sixty-four bytes. Sixty-four bytes is five hundred and twelve bits, the same as a SHA-512 hash, so it takes the same eighty-six characters. One byte more and it would need eighty-seven, which no longer fits.
>
> So the sixty-four byte threshold isn't a design whim. It's the largest size where the content fits in the space a hash would have taken anyway. At or below it, 256t puts the content itself in the name. Above it, the hash. And as a result, every CID for content of sixty-four bytes or more is exactly ninety-four characters long, whichever branch it took.
>
> Inline names have a lovely property. They're not just unlikely to collide. They can't. Two different short contents give two different names, because the names literally contain them. And you don't need a server to look one up. The CID for "Hello" doesn't point to the word "Hello." It is the word "Hello."
>
> Here's a pleasing coincidence that isn't really a coincidence. If you count every possible piece of content from zero up to sixty-four bytes, there are almost exactly two to the five hundred and twelve of them. That's the same as the number of possible SHA-512 outputs. Both kinds of suffix are filling up the same eighty-six characters, just in different ways.

---

## Scene 6 — Reading any CID
*Target: 8:45–9:45*

**Visuals (Manim)**
- Flowchart, built step by step:
  1. `Take the first 8 characters` → `decode → length L`
  2. Diamond: `L ≤ 64?`
     - yes (GREEN) → `the rest is the content. Decode it. Done — no network needed.`
     - no (GOLD) → `the rest is a SHA-512 fingerprint. Go find bytes that match.`
- Run the three opening CIDs through the flowchart one at a time; each lands in its branch. The `?` boxes
  from Scene 1 are removed: `A`, `Hello`, `Gettysburg Address (1,511 bytes)`.
- Edge case: `AAAAAAAA` enters, `L = 0`, inline branch, and the output is an empty ribbon. Caption:
  `the empty file has a name, too`.

**Narration**
> So here's how to read any CID. Decode the first eight characters to get the length. If it's sixty-four or less, everything after that is the content itself, so just decode it, and you're done, without touching a network. If it's more than sixty-four, the rest is a fingerprint, and now you need to go find bytes that match it.
>
> Let's run our three mystery names through. The first is one byte, inline: it's the letter A. The second is five bytes, inline: "Hello." The third is fifteen hundred and eleven bytes, so it's a fingerprint, and it happens to be the Gettysburg Address. And even the empty file has a name: eight A's, and nothing after them.

---

## Scene 7 — The whole recipe fits on one slide
*Target: 9:45–10:40*

**Visuals (Manim)**
- A code block types itself out (Python, from the reference implementation, lightly abridged):

  ```python
  def compute_cid(content: bytes) -> str:
      prefix = b64url(len(content).to_bytes(6, "big"))
      if len(content) <= 64:
          suffix = b64url(content)
      else:
          suffix = b64url(sha512(content))
      return prefix + suffix
  ```
- Each line highlights in sync with the narration (prefix BLUE, inline branch GREEN, hash branch GOLD).
- The block shrinks into one tile in a large grid of language logos/names (Bash, C, Go, Haskell, Java,
  Python, Rust, Zig, … more than forty). Each tile receives the same example file and emits the same CID;
  a single GREEN check sweeps across the grid.

**Narration**
> That's the whole format. Here it is as code. Encode the length as six bytes, big end first, then base64url. If the content is sixty-four bytes or less, encode the content. Otherwise, encode its SHA-512 hash. Stick them together.
>
> Because it's this small, it's been implemented in more than forty programming languages in the 256t repository, from Bash to Zig. And they're all checked against the same set of example files, so they all agree, byte for byte, on every name.

---

## Scene 8 — Recap
*Target: 10:40–11:10*

**Visuals (Manim)**
- Summary card, built from pieces seen in the episode:
  - `8 chars` BLUE — `length, 48 bits → up to 2^48 bytes = 256 TiB`
  - `≤ 86 chars` — GREEN `content itself (≤ 64 bytes)` / GOLD `SHA-512 (> 64 bytes)`
  - `≤ 94 total` — `base64url, no padding`
- The GOLD region pulses; the fingerprint icon floats up out of it as a transition to Episode 3.

**Narration**
> So: eight characters of length, which gives the two fifty-six in the name. Then up to eighty-six characters that are either the content itself or its fingerprint, split at exactly the point where the two cost the same. Ninety-four characters at most.
>
> The only part of this we've treated as a black box is that fingerprint. Next time, we'll open it up, and ask exactly what a hash function promises, and what it doesn't.
