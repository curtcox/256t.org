# Episode 7 — Limits and Neighbors

**Target runtime:** ~10 minutes (≈8.3 min of narration at 150 wpm, plus animation holds) · **Narration:** 1,244 words

**Episode goal:** Show that 256t is defined as much by what it leaves out as by what it includes. Walk
through each deliberate omission (discovery, metadata, permissions, chunking, algorithm agility) as a
trade-off with a real benefit, place 256t among its neighbors (data: URIs, Subresource Integrity, Git,
BitTorrent magnet links, IPFS), and close the series.

**Facts used (sources in `README.md` fact-check table):**
- Git names a file's contents ("blob") by hashing the header `blob <size>\0` followed by the content, with
  SHA-1 historically and SHA-256 as a newer option.
- Subresource Integrity: an HTML `integrity` attribute such as `sha384-<base64 digest>`; the browser
  refuses a script or stylesheet whose hash doesn't match.
- data: URIs (RFC 2397) put the content itself inside the link.
- BitTorrent magnet links identify a torrent by the hash of its info dictionary; BitTorrent v2 uses
  per-file SHA-256 Merkle trees.
- IPFS CIDs include a version, a content-type codec, and a "multihash" naming the hash algorithm; large
  files are split into chunks (256 KiB by default in the reference implementation) arranged in a tree.

**New recurring visuals:** `MerkleTree`, `NeighborMap` (a 2-D map of related systems).

---

## Scene 1 — Defined by its edges
*Target: 0:00–1:00*

**Visuals (Manim)**
- A small, bright CID tag in the center of a large dark canvas. Around it, faint dotted regions fade in,
  each labeled with a question from the spec's "Beyond the Scope" section:
  - `Finding — where is it published?`
  - `Publishing — where and how do I publish?`
  - `Metadata — what headers or info should accompany it?`
  - `Permissions — who may serve it, to whom?`
- Each region gets a small stamp: `beyond the scope of this text`.

**Narration**
> Over six episodes, we've seen everything 256t does. It's a short list. Turn bytes into a name. Turn a name back into a check.
>
> What's striking is the much longer list of things it deliberately doesn't do. The spec even has a section for them, called "beyond the scope of this text." In this final episode, I want to go through those omissions, because each one is a trade-off, and each trade-off buys something. Then we'll meet the neighbors: other systems that made different choices.

---

## Scene 2 — Finding and publishing
*Target: 1:00–2:15*

**Visuals (Manim)**
- A CID tag with a question mark beside it. Around it, several possible discovery mechanisms appear as
  separate small icons, none of them connected to the tag by the spec itself: `a known base URL`,
  `a list of mirrors`, `a search index`, `a friend's message`.
- Show the same CID appended to three different base URLs:
  `https://256t.org/AAAAAAXn…`, `https://mirror.example/AAAAAAXn…`, `file:///archive/AAAAAAXn…`.
  The three resulting bytes all pass the same `VerifierLoop`.

**Narration**
> First, finding. A CID tells you what you want, but not where it is. 256t says nothing about which servers host which content, or how you'd find them. The same CID can go on the end of any server's address, or be a filename in a folder on your disk, and whatever comes back gets checked the same way.
>
> That might feel like a gap. But consider the alternative. The moment a name includes a location, you're back to street addresses, and everything that breaks when the house moves. By keeping finding out of the name, 256t lets any number of finding systems be built on top of it, and replaced, without a single name ever changing.

---

## Scene 3 — No metadata, on purpose
*Target: 2:15–3:45*

**Visuals (Manim)**
- A ribbon with three different "outfits": filename `photo.png`, filename `IMG_2041.PNG`, and filename `cat.png`,
  uploaded by three different people on three different dates. All three map to a single CID tag.
  Caption: `same bytes → one name, everywhere`.
- Deduplication beat: three identical copies stored in a bin collapse into one.
- Then: metadata as content. A small JSON document types out:

  ```json
  {
    "title": "The Gettysburg Address",
    "type": "text/plain",
    "file": "AAAAAAXnpY8KkcoL…"
  }
  ```
  It goes through the `RecipeMachine` and gets its *own* CID. An arrow from the JSON's `file` field points to the
  Gettysburg ribbon.
- Extend into a small tree: a "collection" document lists several such documents by CID; its own CID names the
  whole collection. Caption: `names inside content → structures made of names`.

**Narration**
> Second, metadata. A CID knows nothing about a file's name, its type, its author, or its date. Just the bytes.
>
> And that's a feature. Upload the same photo three times under three different filenames, and it gets one name. That makes duplicates collapse on their own, and it means two people who've never met will agree on what to call the same thing.
>
> But what if you do want metadata? Then write it down as content. A small document that says "this is the Gettysburg Address, it's plain text," and then refers to the text by its CID. That little document has its own CID, too. And now something interesting happens. A document that lists other documents by CID, named by its own CID, pins down that entire collection, exactly. Change any file inside it, and the collection's name changes. Names inside content let you build whole structures out of names, and the structures inherit all the same guarantees.

---

## Scene 4 — Permissions
*Target: 3:45–4:30*

**Visuals (Manim)**
- A CID tag beside a set of scales (justice), a padlock, and a globe with border lines drawn on it.
  Caption: `a name doesn't decide who may share`.
- The scales and lock stay outside a boundary line labeled `out of scope`.

**Narration**
> Third, permissions. Some content is private. Some has disputed ownership, or is legal to distribute in one country and not another. The spec acknowledges all of that, and then, explicitly, declines to decide it. A CID is a precise name for some bytes. Who's allowed to serve those bytes, and to whom, are human questions, and they're answered by people and laws, not by an encoding.

---

## Scene 5 — One hash for the whole file
*Target: 4:30–6:45*

**Visuals (Manim)**
- **256t's approach:** a long ribbon, one SHA-512 at the very end. During a download, the `StreamGauge`
  fills; the GREEN check only lights at 100%. Caption: `can reject early, can only accept at the end`.
- A download fails at 90%. Caption: `which part was bad? unknown`.
- **The alternative:** a `MerkleTree`. The ribbon is cut into chunks; each chunk is hashed; pairs of hashes are
  hashed together, up to a single root. During a download, each chunk lights GREEN as it individually checks
  out against the tree. A bad chunk lights RED alone and is re-fetched from another server.
- Trade-off table appears:

  | | whole-file hash (256t) | chunked tree |
  |---|---|---|
  | verify while downloading | reject early only | every chunk |
  | fetch pieces from many servers | harder | natural |
  | names per file | exactly one | depends on chunk size and tree shape |
  | to implement | ~10 lines | a lot more |
- The ceiling: `max size 2^48 − 1 bytes ≈ 256 TiB`.

**Narration**
> Fourth, a design choice that's easy to miss: 256t hashes the whole file as one piece.
>
> That has a cost. Like we saw last episode, you can reject bad bytes early, but you can't fully accept anything until the very last byte arrives. And if a long download turns out to be wrong, you don't know which part was bad.
>
> Many systems avoid that with a structure called a Merkle tree. Cut the file into chunks. Hash each chunk. Then hash the hashes together in pairs, and pairs of those, all the way up to a single root, and name the file by that root. Now every chunk can be checked on its own the moment it arrives, so you can pull different pieces from different servers, and re-fetch only the piece that went wrong.
>
> So why doesn't 256t do that? Because a tree has parameters. How big are the chunks? How wide is the tree? Different choices give the same file different names. In some popular systems, the same file really can end up with several different identifiers depending on settings. 256t gives every sequence of bytes exactly one name, and an implementation that fits on a slide. It's choosing simplicity and uniqueness over piecewise verification.
>
> One more limit falls out of the format: the length prefix tops out at just under two hundred fifty-six terabytes. For a single file, that's comfortably more than almost anyone needs.

---

## Scene 6 — No algorithm agility
*Target: 6:45–7:45*

**Visuals (Manim)**
- A CID tag with `SHA-512` engraved into it permanently (no label field). Beside it, an alternative format
  with a small leading tag: `[algorithm: sha2-512][digest…]`, the "self-describing" style.
- Two-sided balance:
  - Left pan (256t): `one name per content`, `nothing to negotiate`, `no "which hash?" bugs`.
  - Right pan (self-describing): `can switch algorithms later`, `but the same content gets a new name when it does`.
- Callback to the Episode 5 timeline: `years of warning` ribbon above SHA-512's empty track.

**Narration**
> Fifth, the hash function itself is fixed. A CID doesn't say "this was made with SHA-512." It just is SHA-512.
>
> Some systems instead put a small label at the front of every name saying which algorithm was used, so they can switch to a new one later. That flexibility is real, but it isn't free. The same content ends up with different names under different algorithms, and every implementation has to handle every option.
>
> 256t takes the other side of that bet: one algorithm, one name per content, and nothing to negotiate. If SHA-512 ever did start to crack, the remedy would be a new scheme, and the history we looked at suggests there would be years of warning to make that move.

---

## Scene 7 — The neighbors
*Target: 7:45–10:00*

**Visuals (Manim)**
- `NeighborMap`: a 2-D layout with a horizontal axis `content in the link ← → fingerprint in the link` and a
  vertical axis `just a check ↓ ↑ a name you can fetch`. Each neighbor drops in as a card with a short real
  example:
  - **data: URI** (far left) — `data:text/plain;base64,SGVsbG8=`. Label: `the content IS the link`.
  - **Subresource Integrity** (bottom right) —
    `<script src="https://cdn.example/lib.js" integrity="sha384-…">`. Label: `location + fingerprint check`.
  - **Git** (upper right) — `hash("blob 1511\0" + content)`. Label: `also mixes in the length!`
  - **Magnet links / BitTorrent** (upper right) — `magnet:?xt=urn:btih:…`. Label: `fingerprint of a torrent's description; v2 uses Merkle trees`.
  - **IPFS** (upper right, furthest from center) — `bafy…`. Label: `self-describing hash, chunked tree, a whole network`.
- 256t drops in last, straddling the horizontal axis: its inline region overlaps the data: URI card, and its
  hashed region sits in the upper right next to Git. Label: `a single path segment`.

**Narration**
> Now let's meet the neighbors, because seeing where 256t sits among them is the best way to understand it.
>
> At one extreme are data URIs. They put the entire content directly inside the link. That's exactly 256t's inline case, except that data URIs do it for content of any size, so they can get huge.
>
> Subresource Integrity is the web's way of saying "load this script from this location, but refuse it unless its hash matches this." It's verification bolted onto an ordinary address. It solves the content-drift problem, but if the location disappears, you're stuck, because the fingerprint isn't used to find anything.
>
> Git names every file's contents by a hash. And in a nice bit of convergent design, the thing Git hashes starts with the word "blob" and the file's length, before the content. Length plus fingerprint. Git has used SHA-1, which is part of why the SHA-1 collisions we saw caused real concern, and it has been moving toward SHA-256.
>
> Magnet links in BitTorrent name a torrent by the hash of its description, and the newer version of BitTorrent uses Merkle trees, so each piece can be checked as it arrives. And IPFS goes furthest: self-describing names that label their hash algorithm and content type, chunked trees for large files, and a whole peer-to-peer network for finding things.
>
> 256t sits in a deliberately small spot on this map. Smaller than IPFS, more findable than Subresource Integrity, and more compact than a data URI. It's just a name, at most ninety-four characters, that fits in a single segment of a URL, and that anyone can compute.

---

## Scene 8 — The whole picture
*Target: 10:00–11:00*

**Visuals (Manim)**
- Final reassembly, pulling one element from each episode into a single tableau:
  - Ep 1: the map pin morphing into a fingerprint.
  - Ep 2: the CID with BLUE prefix and GOLD/GREEN suffix on its 1–94 ruler.
  - Ep 3: the three game cards.
  - Ep 4: the dice and the `2^−271` card.
  - Ep 5: the Liar bouncing off the `VerifierLoop`; the Forger's blueprint, unfinished.
  - Ep 6: the `∞` cache badge.
  - Ep 7: the `NeighborMap`, with 256t glowing.
- Everything collapses into a single CID tag floating above a ribbon, with several `ServerRack`s below it.
- End card: the 256t logo, `256t.org`, and the line `a name for bytes`.

**Narration**
> So let's put the whole thing back together.
>
> A web address usually tells you where something is. A 256t CID tells you what it is. Eight characters of length, which is where the two hundred fifty-six terabytes and the name come from. Then up to eighty-six characters that are either the content itself, when it's small enough to carry, or its SHA-512 fingerprint.
>
> Collisions exist, by simple counting. But accidental ones are so unlikely that a civilization producing a billion files per person per second since the Big Bang would almost certainly never see one. Adversaries have it harder than you might think, because the cheap attack, lying, is defeated completely by one hash computation, and the expensive attack, forging, has never been done against SHA-512.
>
> And once you verify, you can fetch from anyone, and cache forever.
>
> It's a small idea. That's sort of the point. It's small enough to implement in an afternoon, in whatever language you like, and get exactly the same answers as everyone else. If you want to try it, pick a file you care about, and compute its name. Thanks for watching.
