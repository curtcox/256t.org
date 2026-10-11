# 256t — Video Series Scripts

A seven-part explainer series on the 256t content identifier, in the style of 3Blue1Brown. The
narration is meant for ElevenLabs voice synthesis and the graphics for Manim.

**Audience:** curious generalists who are comfortable with exponents and a little algebra. No prior
knowledge of hashing or base64 is assumed, but every technical detail is stated precisely.

| # | Episode | Script | Big idea |
|---|---------|--------|----------|
| 1 | Where vs. What: Names That Can't Break | [ep01](ep01-where-vs-what.md) | Link rot and content drift come from naming locations. Name the content instead. |
| 2 | Anatomy of a CID | [ep02](ep02-anatomy-of-a-cid.md) | 8, 64, 86, 94 and "256" all fall out of base64 arithmetic. Decode a CID by hand. |
| 3 | What a Hash Promises | [ep03](ep03-what-a-hash-promises.md) | Collisions must exist (pigeonhole). The promise is that nobody can *find* them. There are three games, and 256t needs two of them. |
| 4 | Accidental Collisions | [ep04](ep04-accidental-collisions.md) | The birthday bound, derived. A billion files per person per second since the Big Bang gives about 2^−271. |
| 5 | Adversarial Collisions: It's Easier to Lie | [ep05](ep05-adversarial-collisions.md) | Liar vs. forger. Verification defeats lying completely. The "twins" scenario. How MD5 and SHA-1 actually died. |
| 6 | Verify, Cache, Trust | [ep06](ep06-verify-cache-trust.md) | The verification loop, the length prefix as a hard stop, decoding vs. verifying, and caching forever. |
| 7 | Limits and Neighbors | [ep07](ep07-limits-and-neighbors.md) | What 256t deliberately leaves out, the draft `t256:` URI scheme, and how 256t compares to data: URIs, SRI, Git, `ni:` URIs, BitTorrent, and IPFS. |

Total narration is about 9,750 words, roughly 65 minutes at 150 words per minute. With animation holds,
each episode should run about 10–12 minutes. `extract_narration.py` prints current per-episode counts and
keeps the runtime line at the top of each script up to date.

## Files

```
video-series/
  README.md                 this file: overview, style guide, production notes, fact-check table
  ep01-…md … ep07-…md       full scripts: scenes with visuals, on-screen text, and narration
  extract_narration.py      regenerates narration/ from the scripts
  generate_audio.py         voices narration/ with ElevenLabs into audio/ (clips + timestamps)
  narration/
    ep01.txt … ep07.txt     whole-episode narration, ready to paste into ElevenLabs
    ep01/s01.txt …          one file per scene, for per-scene audio clips
  audio/                    (created by generate_audio.py) epNN/sMM.mp3 + sMM.json alignment
  manim/
    common.py               palette, fonts, NarratedScene, and the recurring visuals
    ep01.py … ep07.py       one Scene class per script scene (S01…, S02…, …)
    sha512_rounds.py        readable SHA-512 exposing its rounds (drives Episode 3's diffusion)
    render.py               renders an episode scene by scene and stitches it
    output/, media/         render output (git-ignored)
```

**The scripts are the source of truth.** If you edit narration, edit the `> ` blockquote under
**Narration** in the episode script, then run:

```bash
python3 video-series/extract_narration.py
```

This regenerates every file under `narration/`, applies the pronunciation map, and prints word counts
and estimated durations.

> Keep this directory out of `examples/`. Every file in `examples/` gets a CID generated for it by the
> implementations' `generate` scripts.

## Producing the videos

Requirements: [Manim Community](https://www.manim.community/) 0.21 or later, plus ffmpeg. No LaTeX
is needed, because every piece of text, including math, is typeset with Pango `Text`.

```bash
python3 video-series/extract_narration.py
```

```bash
python3 video-series/generate_audio.py --dry-run
```

```bash
python3 video-series/generate_audio.py
```

```bash
python3 video-series/manim/render.py 1 2 3 4 5 6 7 -q h
```

1. **Narration.** `extract_narration.py` turns the scripts into `narration/`.
2. **Voice.** `generate_audio.py` sends each scene's narration to ElevenLabs' text-to-speech
   "with timestamps" endpoint and saves `audio/epNN/sMM.mp3` plus the character alignment. It reads
   the API key from `ELEVENLABS_API_KEY`, or else from the macOS keychain (generic password, service
   `elevenlabs`). It uses the voice named "Curt Cox 2", looked up by name; override this with
   `--voice-name` or `ELEVENLABS_VOICE_ID`. The model is `eleven_multilingual_v2` unless
   `ELEVENLABS_MODEL_ID` is set. It passes the neighboring scenes as context
   so intonation carries across clips, and it skips scenes whose text hasn't changed. `--dry-run`
   sends nothing and prints the character count (about 56,000 for the whole series), which is what
   ElevenLabs bills on. The voice reads at roughly 160 words per minute.
3. **Render.** `manim/render.py` renders each scene class in order. It builds one continuous audio
   track, with each scene's narration padded to exactly that scene's length (silence where there's no
   narration yet). It encodes the episode once, putting the index at the front of the file
   (`+faststart`) so playback starts smoothly, and writes `manim/output/epNN.mp4`. Use `-q l` for fast previews and `-q h` for 1080p60. `--sheet` also writes a
   4×3 contact sheet per scene, which is a quick way to review a whole episode at a glance.
   `--scene S05SmallThings` renders a single scene.

You can render before any audio exists. Each scene then runs for its estimated narration length
(150 words per minute), so the timing is close but not exact. Once audio exists, scenes snap to the
real clips automatically.

### How scenes sync to the voice

Every scene subclasses `NarratedScene` and calls `self.cue("phrase")` or
`self.play_at("phrase", *animations)` with a phrase copied from the script's narration. The phrase is
matched against the narration text (case and punctuation don't matter, and the pronunciation map is
applied, so you write "CID" rather than "C-I-D"). The time comes from:

- the ElevenLabs character alignment, if `audio/epNN/sMM.json` exists, so it's exact;
- otherwise, the phrase's position in the text, scaled to the clip's length (or to the estimate).

Two rules keep the picture from going stale while the narrator talks. An empty (all-black) screen
never waits for a cue, so the next visual comes in immediately. And animations are slowed down to fill
the pause before the next one, instead of finishing quickly and leaving a still picture. The camera
never zooms.

The slowing works in two passes, both automatic. A planning pass runs the scene with animations
skipped, which takes a second or two, and records when every animation starts. The real render then
stretches each drawing, fading, or moving animation so it ends about 0.3 seconds before the next one
begins, up to 4 times its written run time or 4 seconds, whichever is longer. Some cases differ:

- Brief emphasis (Indicate, Circumscribe, Flash, Wiggle, ApplyWave) slows less, to 2.5 times or 2.5
  seconds, because a very slow pulse reads as sluggish.
- A fade-in that would take more than 1.5 seconds is drawn on with Write instead (in place, without any
  slide), because a long fade looks finished well before it ends.
- Fading the whole screen out is never slowed, so the picture doesn't linger half-faded.
- Anything slowed while coming in from black arrives quickly and then settles slowly, so scenes don't
  open on a long dim fade.

Later waits shrink to absorb the extra time, so every cue still lands on its word. Plans are cached in
`manim/media/plans/` and redone whenever the episode file, `common.py`, the narration, or the alignment
changes. Set `SERIES_NO_STRETCH=1` to render at the written run times.

Slowing only goes so far. When narration runs over a still picture for long, add illustration: a new
beat cued to the words being spoken (a label, a highlight, a small diagram, a step of the argument
appearing). To find those places, run the hold finder after rendering. It prints each static or black
stretch along with the words being spoken during it:

```bash
python3 video-series/manim/render.py 1 2 3 4 5 6 7
```

```bash
python3 video-series/manim/find_holds.py 1 2 3 4 5 6 7 --min 3
```

As of the last pass, no static stretch runs longer than about 4.5 seconds, and the camera never moves.

When `construct()` finishes, the scene holds until the narration ends, plus a short tail. If animations
fall behind the voice, the render prints a `[timing]` line naming the cue and how late it was. If a cue
phrase no longer appears in the narration (say, after a script edit), the render fails with a clear
error rather than drifting silently.

`audio/` is committed, because the clips cost money to regenerate.

## Script format

Each episode file has a header (runtime, goal, facts used, new recurring visuals), followed by numbered
scenes:

- `## Scene N — Title` with a rough target timestamp. Timestamps are for pacing only; the real timing
  comes from the generated audio.
- **Visuals (Manim)**: what the animation shows, in order. Visual beats are listed in the same order as
  the narration sentences they accompany.
- **On-screen text** (optional): exact strings to typeset.
- **Narration**: the spoken text, as a markdown blockquote.

One scene maps to one Manim `Scene` class (`manim/epNN.py`, class `SMM…`) and one audio clip
(`narration/epNN/sMM.txt` → `audio/epNN/sMM.mp3`).

## ElevenLabs notes

- **One narrator, throughout.** In the 3b1b style, the narrator is warm, curious, unhurried, and talks
  *to* the viewer.
- Narration is written to be read aloud. Large numbers and exponents are spelled out ("two to the five
  hundred twelve"), and their notation (`2^512`) appears only on screen. CIDs are never read character
  by character. The narration describes them, and the screen shows them.
- `extract_narration.py` applies a small pronunciation map. **Audition these first and adjust the map
  (or use an ElevenLabs pronunciation dictionary):**

  | Written | Spoken (default) |
  |---|---|
  | 256t / 256t.org | "two fifty-six tee" / "… dot org" |
  | t256 (URI scheme) | "tee two fifty-six" |
  | SHA-512, SHA-256, SHA-1 | "Shaw five-twelve", "Shaw two-fifty-six", "Shaw one" |
  | MD5 | "M D five" |
  | base64 / base64url | "base sixty-four" / "base sixty-four U R L" |
  | CID / CIDs | "C-I-D" / "C-I-Ds" |
  | IPFS, PNG, GPU, CWI, URI(s) | spelled out letter by letter |

- Pauses: scene boundaries are natural pause points. If one clip per episode sounds rushed, generate one
  clip per scene and put the gaps in during editing.

## Manim style guide

Consistency across episodes matters more than any single scene. These conventions are referenced by
name in the scripts.

**Palette** (Manim's built-in constants on the default dark background):

| Meaning | Color | Used for |
|---|---|---|
| Length prefix | `BLUE` | First 8 characters of every CID, length values, the length "wall" |
| Hash / fingerprint | `GOLD` | 86-character SHA-512 suffix, hash machines, hashed region |
| Inline content | `GREEN` | Inline suffix (≤ 64 bytes), "collision-free" regions |
| Verified / accept | `GREEN` | Check marks, verification loop success |
| Wrong / collision / reject | `RED` | Mismatches, colliding tags, RED X |
| Adversary | `PURPLE` | Liar and Forger figures, attacker-controlled servers |
| Neutral / padding bits | `GREY` | Padding zeros in base64, inactive elements |

**Type:** Menlo (`MONO` in `common.py`) for CIDs, code, bytes, and bits, and Avenir Next (`SANS`) for
everything else. Exponents use Unicode superscripts (`power_text(-271)` gives `2⁻²⁷¹`), so no LaTeX is
needed. A full 94-character CID is wide, so `CIDText(..., max_width=FRAME_W - 1)` fits it to the frame
on a single line. It stays readable at 1080p. Never wrap a CID.

**Recurring visuals.** These are implemented in `manim/common.py` unless noted. Some names differ
slightly from the scripts' visual directions, which were written before the code:

| Name | Introduced | Description |
|---|---|---|
| `ByteRibbon` | Ep 1 | Horizontal strip of cells, each a byte (hex or char). Length scales with content. Can tint cells. |
| `RecipeMachine` | Ep 1 | Rounded box with a crank. Ribbon in, tag out (`feed_through`). Label is swappable (`public recipe`, `SHA-512`). |
| `CIDText` | Ep 1 | Monospace CID with colored segments (BLUE prefix + GOLD or GREEN suffix), exposing `.prefix` and `.suffix`. |
| `ServerRack` | Ep 1 | Simple server icon. A face variant (`?`) is used for unknown mirrors, and a PURPLE variant for adversaries. |
| `VerifierLoop` | Ep 1 | Circuit: request → receive → compute → compare → GREEN check or RED X. |
| `CacheBox` | Ep 1 | Box with an `∞` badge. |
| `BitStrip` | Ep 2 | Row of 0/1 cells, which can be sliced into 6-bit groups. |
| `BitStrip.group_boxes` | Ep 2 | The "base64 gear": 3 bytes (24 bits) regrouped into four 6-bit characters. |
| `LengthOdometer` | Ep 2 | 8 base-64 wheels showing a length prefix. |
| (in `ep03.py`) | Ep 3 | Schematic SHA-512 round over 8 registers `a`–`h`. |
| `DiffusionGrid` | Ep 3 | 512-cell heat map of changed bits, driven by real SHA-512 states (`sha512_rounds.py`). |
| `FingerprintIcon` | Ep 1 | Thumbprint icon. |
| `TaxonomyGrid` | Ep 4 | Accidental/adversarial × existing/problem cards. |
| `PairWeb` | Ep 4 | Dots joined by every pairwise line (birthday problem). |
| (in `ep04.py`) | Ep 4 | Vertical log₂ "exponent ladder" for probabilities. |
| `MaskedFigure("liar" / "forger")` | Ep 5 | Two simple PURPLE masked figures (paint can, blueprints). |
| `TwinRibbons` (in `ep05.py`) | Ep 5 | Two different documents wired to one shared CID tag. |
| (in `ep05.py`) | Ep 5 | Multi-track timeline (MD5 / SHA-1). |
| `StreamGauge` | Ep 6 | Progress bar with a hard stop at L bytes. |
| `MerkleTree` | Ep 7 | Chunks → leaf hashes → root. |
| (in `ep07.py`) | Ep 7 | 2-D map of related systems. |
| `NarratedScene` | all | Base scene: paces itself to the narration and provides `cue` / `play_at`. |

**Don't** use 3Blue1Brown's own trademark assets (the Pi creatures, the logo). Copying the look and pacing
of the style is fine. Copying its characters is not.

**Real data only.** Every CID, encoding, and bit count in the scripts was computed with the reference
implementation (`implementations/python/cid.py`). If a scene needs a new example, compute it rather than
making one up:

```bash
python3 -c "import sys; sys.path.insert(0, 'implementations/python'); from cid import compute_cid; print(compute_cid(b'Hello'))"
```

## Reference values used on screen

| Content | Bytes | CID |
|---|---|---|
| (empty) | 0 | `AAAAAAAA` |
| `A` | 1 | `AAAAAAABQQ` |
| `Hi` | 2 | `AAAAAAACSGk` |
| `Hello` | 5 | `AAAAAAAFSGVsbG8` |
| `hello` | 5 | `AAAAAAAFaGVsbG8` |
| `hunter2` | 7 | `AAAAAAAHaHVudGVyMg` |
| `Hello, world!` | 13 | `AAAAAAANSGVsbG8sIHdvcmxkIQ` |
| 64 × `x` | 64 | `AAAAAABAeHh4…eA` (94 chars, inline) |
| 65 × `x` | 65 | `AAAAAABBRg2doReeYM-O5F0s…` (94 chars, hashed) |
| `examples/gettysburg_address.txt` | 1,511 | `AAAAAAXnpY8KkcoLHubRMUTViZU671vd9x4IuH0gmZhLvp9TTl-K-J3UzKZCbOUP41fDBlxdcJHSTd7bVYAZvwvibsdFjA` |
| `examples/256t_logo.png` | 6,059,375 | `AAAAXHVvyV4cESpv…` |

Length prefixes: 1,000 → `AAAAAAPo` · 1 MiB → `AAAAEAAA` · 1 GiB → `AABAAAAA` · 1 TiB → `AQAAAAAA` ·
2^48 − 1 → `________`.

SHA-512 of `Hello` vs `hello`: 239 of 512 bits differ.

## Fact-check table

These are claims that rely on outside sources rather than on arithmetic or the reference implementation.
Re-check anything time-sensitive (marked ⏱) right before recording.

| Claim | Episode | Source |
|---|---|---|
| ~38% of pages that existed in 2013 were no longer accessible in 2023 | 1 | [Pew Research Center, "When Online Content Disappears" (May 2024)](https://www.pewresearch.org/data-labs/2024/05/17/when-online-content-disappears/) |
| ~50% of URLs in U.S. Supreme Court opinions no longer led to the originally cited material | 1 | [Zittrain, Albert & Lessig, "Perma: Scoping and Addressing the Problem of Link and Reference Rot in Legal Citations," 127 Harv. L. Rev. F. 176 (2014)](https://hls.harvard.edu/bibliography/perma-scoping-and-addressing-the-problem-of-link-and-reference-rot-in-legal-citations) |
| Every PNG file starts with the 8 bytes `89 50 4E 47 0D 0A 1A 0A` | 1 | PNG specification (signature) |
| SHA-512: 1024-bit blocks, 128-bit length field in padding, 8 × 64-bit registers, 80 rounds | 3 | FIPS 180-4 |
| Bitcoin network ≈ 1 zettahash/s, first crossed in late 2025 ⏱ | 5 | [Spark glossary: hashrate](https://www.spark.money/glossary/hashrate) and [KuCoin, "Bitcoin Hashrate in 2026"](https://www.kucoin.com/blog/ae-bitcoin-hashrate-in-2026-latest-trends-mining-difficulty-network-security-analysis). Check a live tracker before recording. |
| MD5 collisions (Wang et al., 2004). Rogue CA certificate (2008). Flame used a novel chosen-prefix MD5 collision to forge a Microsoft code-signing certificate (found 2012) | 5 | [Matthew Green, "Flame, certificates, collisions. Oh my."](https://blog.cryptographyengineering.com/2012/06/05/flame-certificates-collisions-oh-my/) and [CWI announcement](https://projects.cwi.nl/crypto/risc.php?s=20130320) |
| SHAttered (2017): ~2^63.1 SHA-1 computations, ~100–110 GPU-years | 5 | [EFF coverage](https://www.eff.org/deeplinks/2017/02/cryptographers-demonstrate-collision-popular-sha-1-algorithm) and the SHAttered paper |
| "SHA-1 is a Shambles" (2020): first practical chosen-prefix collision, estimated ~US$45k (the authors actually spent ~US$75k) | 5 | [Leurent & Peyrin, USENIX Security 2020](https://www.usenix.org/conference/usenixsecurity20/presentation/leurent) |
| Best practical SHA-512 attacks: collision for 27 of 80 steps, semi-free-start collision for 39 of 80 steps. No full SHA-512 collision is known ⏱ | 5 | [Dobraunig, Eichlseder & Mendel, ASIACRYPT 2015](https://www.iacr.org/archive/asiacrypt2015/94520180/94520180.pdf). Check the IACR ePrint archive for newer records before recording. |
| Quantum collision search for a 512-bit hash stays around 2^170 even under optimistic assumptions | 5 | Brassard–Høyer–Tapp (2^(n/3)). The narration hedges this as "numbers like." |
| Git hashes `blob <size>\0<content>`, historically with SHA-1, with SHA-256 as a newer object format | 7 | Git documentation (`git hash-object`, hash-function-transition) |
| `t256:` URI scheme. The optional media-type suffix is outside the CID and not covered by verification. `256t:` is invalid because RFC 3986 schemes must begin with a letter | 6, 7 | [rfc/draft-cox-t256-uri-scheme-00.md](../rfc/draft-cox-t256-uri-scheme-00.md) (this repo) |
| RFC 6920 `ni:` "Naming Things with Hashes" labels the hash algorithm and can't express the length prefix or inline content | 7 | RFC 6920. The draft `t256` RFC, §6 |
| IPFS default chunk size 256 KiB. CIDs carry a multihash and codec | 7 | IPFS/Kubo docs. The script only says "chunks" in the narration. |
| "There are only two hard things in Computer Science…" | 6 | Phil Karlton (widely attributed) |

**Arithmetic the narration relies on**

- 2^48 bytes = 256 TiB. 6 bytes → exactly 8 base64 characters.
- 512 / 6 = 85.33 → 86 characters. 8 + 86 = 94.
- Inline length ⌈4n/3⌉ ≤ 86 ⇔ n ≤ 64 (65 bytes would need 87 characters).
- Contents of 0–64 bytes: (256^65 − 1)/255 ≈ 1.004 × 2^512.
- 65-byte files: 2^520, so 2^520 / 2^512 = 256 files per fingerprint on average.
- Birthday: 23 people → 253 pairs → 50.7%. P ≈ n²/(2N).
- 8×10^9 × 10^9/s × 4.35×10^17 s ≈ 3.5×10^36 ≈ 2^121 files → P ≈ 2^−271. Atoms ≈ 10^80 ≈ 2^266.
- 2^256 / 2^70 per second = 2^186 s ≈ 3×10^48 years ≈ 2×10^38 universe ages.
- In a lenient decoder, `QQ` through `Qf` (16 strings) all decode to the byte `A`.
