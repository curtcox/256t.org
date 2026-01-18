# Full Script (Graphics Only)

## 0:00 — A URL that answers “what”, not “where”
- Show a familiar URL: `https://example.com/files/report.pdf`
- Visually label it “location-like”.
- Morph the last path element into a long token and relabel: “description-like”.
- Split-screen:
  - Left: “Where is it?” (server icon, folder tree)
  - Right: “What is it?” (byte-strip icon, fingerprint tag)
- Show request → server returns bytes; alternate branch: server returns “404”.

## 0:55 — Introduce the CID (Content Identifier)
- Bring in a single path segment token with a bracket under it: “CID”.
- Subtitle: “94 characters or less”.
- Show it as base64url characters (A–Z, a–z, 0–9, `-`, `_`).
- Quick montage of where it fits:
  - URL path segment
  - filename
  - database field cell

## 1:40 — The 94-character structure
- Lay the CID on a ruler marked 1–94.
- Highlight first 8 characters in one color: “length”.
- Highlight remaining up to 86 characters in another color: “hash or literal content”.
- Branch diagram:
  - If length > 64 bytes → “SHA‑512(content) → base64url → 86 chars”
  - If length ≤ 64 bytes → “base64url(content) → ≤86 chars”
- Edge case callout badge:
  - “Exactly 64 bytes: may be inline or hashed (inline preferred)”.

## 2:45 — Why base64url?
- Show the standard base64 alphabet with `+` and `/` circled in red.
- Replace `+` → `-` and `/` → `_`.
- Drop the trailing `=` signs into a trash can labeled “padding omitted”.
- Put the CID into a URL path; show no escaping, no breaking.
- Optional on-screen caption: “base64url = URL/filename-safe alphabet”.

## 3:35 — Why 86 characters?
- Animate “SHA‑512” as a machine that outputs a 64-byte block.
- Convert 64 bytes into a 512-bit ribbon.
- Show a base64 packer:
  - Each character = 6 bits
- Do the division visually:
  - `512 / 6 = 85.333…` → round up to `86`
- Stamp: “SHA‑512 digest → 86 base64url chars”.

## 4:15 — Why 8 characters?
- Show 8 base64 characters in a box.
- Expand them into a 48-bit ruler: 8 × 6 = 48 bits.
- Animate “2^48 bytes” into a size ladder:
  - 2^10 = KiB, 2^20 = MiB, 2^30 = GiB, 2^40 = TiB
- Land on: “2^48 = 256 TiB (≈256 TB)”.
- Label: “This is the ‘256t’ size ceiling”.

## 5:05 — Inline content for ≤ 64 bytes
- Show a tiny byte-string (e.g., `"hello"`), with a byte counter ticking under 64.
- Two competing pipelines appear:
  - Hash pipeline outputs fixed 86-char digest.
  - Inline pipeline outputs a shorter base64url string.
- Cross out the longer pipeline for small content; keep the shorter.
- Visual “uniqueness by definition”:
  - CID contains the content bytes (decoded).
  - Two different contents produce visibly different suffix strings.

## 5:55 — The “this matters” moment (URL + QR)
- Put the CID into a URL path and zoom out until it looks “reasonable length”.
- Collapse the URL into a QR code.
- Phone camera scans QR code; URL reappears.
- Montage: photo / document / audio / video / dataset → each becomes a byte-ribbon → enters CID machine → exits with a CID tag.
- Show a huge “index wall” of CID tags snapping into place without duplicates.

## 6:35 — Nuance: identity vs discovery
- Draw two separate boxes:
  - Box A: “CID defines content identity”
  - Box B: “Discovery / who hosts what”
- Put a big checkmark over Box A and a dotted outline over Box B labeled “out of scope”.
- Show multiple servers each capable of serving the same CID (same content identity, different locations).
- Footnote-style asterisk near “permanent pointer”:
  - “Pointer is permanent; retrieval depends on publication”.

## 7:10 — “True enough despite being false”
- On-screen heading: “True enough despite being false”
- Three bullet cards appear, each with a small crack animation at the corner:
  1) “94-char path uniquely determines content”
     - Add a “100%” badge for ≤64 bytes case; “practically” badge for >64 bytes
  2) “Content is immutable”
     - Show a hypothetical “replacement” attempt bouncing off verification
  3) “Safe to cache indefinitely”
     - Show CDN cache with an “infinite” symbol
- Transition zoom into the crack → it becomes a “collision” warning icon.

## 8:05 — Collision taxonomy
- Create a clean 2×2 grid on screen:
  - accidental vs adversarial
  - existing vs problem
- Animate each definition into its quadrant as a small icon:
  - accidental: dice roll
  - adversarial: deliberate wrench/attacker icon
  - existing: two different byte-ribbons merging into one CID tag
  - problem: request CID A → wrong bytes returned
- Highlight “problem” with a red outline.

## 8:50 — Why accidental problem collisions are tiny
- Visualize the SHA‑512 output space as an enormous starfield.
- Each new object becomes a point in the starfield.
- As points accumulate, show the probability bar rising *imperceptibly*.
- “Birthday effect” visualization:
  - Show `2^512` as the space size.
  - Show “collision-likely around sqrt(space) = 2^256” as a small annotation.
- Fade in the spec sentiment: “accidental problem collisions can be ignored” (as a subtle on-screen quote card).

## 9:40 — Adversaries: “collision” vs “lying”
- Two attacker strategies appear side-by-side:
  - Left: “Engineer SHA‑512 collision” → huge compute meter barely moves.
  - Right: “Just lie (serve wrong bytes)” → instant swap attempt.
- Then show verification loop:
  - bytes received → compute CID → compare
  - Lie path fails immediately (red X).

## 10:40 — Cache forever + verify always
- Show response headers overlay:
  - `Cache-Control: public, max-age=31536000, immutable`
- CDN cache stamps “immutable”.
- Overlay a second stamp: “Verify on receipt” (smaller, but persistent).
- Show the verification loop as a simple circuit that lights green when CID matches.

## 11:25 — Wrap-up
- Reassemble the CID one final time:
  - 8-char length prefix
  - up to 86-char hash or inline content
- Show the two-branch rule again (≤64 inline, >64 hashed).
- Final tableau:
  - CID tag floating above bytes
  - Multiple servers beneath it
  - Cache icon to one side
  - Verification loop glowing green
- End card: “A URL-friendly, content-defined identifier for bytes.”
