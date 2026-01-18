# Full Script (Words Only)

## 0:00 — A URL that answers “what”, not “where”
Think about what a URL usually means. It feels like a location: a particular server, a particular path, a particular file.

But the 256t.org specification proposes something slightly different:
the last element of the URL path can be a compact *description of content*—a tag that says what bytes you meant to ask for.

If the server has those bytes, it returns them.
If it doesn’t, it returns a 404.

So the URL stops being a promise about where a file lives, and becomes a statement about what the file *is*.

## 0:55 — The CID: a content identifier that fits in a path segment
That last path element is a CID: a “content identifier”.

It’s a base64url string, 94 characters or less.

And it’s built from exactly two ideas:
1) the **length** of the content  
2) either the **hash** of the content, or—when the content is tiny—the **content itself**

So a CID is not metadata like “type” or “author”.
It’s more fundamental: it’s an identity claim about the bytes.

## 1:40 — The 94-character structure
Here’s the whole format.

A CID is two parts stuck together:

- **First 8 characters:** an encoded content length  
- **Next up to 86 characters:**  
  - for content **longer than 64 bytes**: the SHA‑512 hash of the content, encoded  
  - for content **64 bytes or smaller**: the content itself, encoded

That’s why the CID is “94 characters or less”.
For typical content—anything larger than 64 bytes—it’s the full 94 characters.
For small content, it gets shorter.

And there’s a neat edge case:
content of *exactly* 64 bytes can be stored inline or hashed; inline is preferred for efficiency.

## 2:45 — Why base64url?
All of this is designed to live comfortably in URL path segments, filenames, and database fields.

That’s why it uses base64url:
it’s base64 with a URL- and filename-safe alphabet, and it omits the “=” padding characters.

If you’re wondering, “Isn’t base64 weirdly specific?”—yes, and that’s the point.
Base64 is a packing trick.
Each character carries 6 bits of information, which makes the math of “how many characters do we need?” very clean.

## 3:35 — Why 86 characters for the suffix?
SHA‑512 produces 512 bits, which is 64 bytes.

Base64 stores 6 bits per character.
So 512 divided by 6 is 85 and a third.

Meaning: you need **86** base64url characters to store a SHA‑512 digest.

That’s the “86”.

## 4:15 — Why 8 characters for the prefix?
The length prefix is 8 base64url characters.
That’s 8 times 6 bits, which is 48 bits of length.

48 bits of length means the CID can describe content sizes up to 2^48 bytes.
That’s about 256 terabytes in the power-of-two sense.

So the name “256t” isn’t marketing. It’s the size ceiling implied by the prefix.

## 5:05 — Why embed content when it’s small?
Now for the cleverest part.

If the content is 64 bytes or smaller, the content itself can be base64url-encoded into 86 characters or fewer.

So for small content, it’s wasteful to hash it and store a full 86-character digest.
Instead, the CID stores the content itself.

In that case, the CID is not merely collision-resistant.
It’s unique by definition, because it literally contains the bytes.

So in the small-content case, the collision story is trivial:
two different byte strings cannot share a CID, because the CID includes the byte string.

## 5:55 — The moment where this stops being a cute encoding trick
Now you may be thinking, you know, that's very interesting, but it doesn't really get us anywhere, does it? But consider it is small enough to fit in a URL or a QR code and yet big enough to uniquely identify practically every sequence of bytes produced by people. It is a permanent pointer to anything.

## 6:35 — A careful nuance: a permanent pointer, not a guaranteed retrieval method
There’s an important subtlety hiding inside that phrase “pointer”.

A CID identifies content.
It doesn’t, by itself, tell you where to find it.
Any server can choose to expose a base URL that serves content by CID, but discovery—who hosts what, and how you find them—is intentionally outside the scope of the spec.

So the permanence is about identity, integrity, and caching.
Availability still depends on publication and hosting.

## 7:10 — “True enough despite being false”
The spec makes an unusually honest move.
It names three statements that you can treat as “true enough despite being false”:

- A 94-character path uniquely determines content  
  (and for content ≤ 64 bytes, this is completely true)
- The content is immutable  
  (in theory, different content might meet the same description)
- Content can be safely cached indefinitely

This is not a contradiction. It’s engineering.
It’s the spec telling you: “Here is the abstraction you should use, and here is the tiny crack in the abstraction.”

And that crack is exactly where collision risk lives.

## 8:05 — What collisions actually mean here
The word “collision” is overloaded, so the spec distinguishes four types:

- **accidental**: purely by chance  
- **adversarial**: someone tried to cause it  
- **existing**: two different contents have produced the same CID  
- **problem**: using the CID caused the wrong content to be returned

“Problem collision” is the one that matters operationally.
It’s the collision that shows up in real usage.

## 8:50 — Why accidental problem collisions are ignorable in practice
For content larger than 64 bytes, the CID depends on SHA‑512.

If we model SHA‑512 outputs as uniformly distributed 512-bit labels, then collisions are governed by the birthday effect:
you need on the rough order of the square root of 2^512—so, about 2^256—items before collisions become likely.

That number is so large that, for most systems, accidental problem collisions are not a meaningful engineering risk.
And the spec explicitly states comfort with ignoring accidental problem collisions.

## 9:40 — Adversaries change the story, but not in the way people first assume
If someone is actively trying to harm you, they have two broad strategies:

Strategy A: do real cryptanalysis—engineer a SHA‑512 collision.
Strategy B: just lie—serve you bytes that don’t match the CID and hope you don’t check.

The spec’s point is practical:
it is easier to just lie than to engineer a collision.

Which leads directly to the single most important mitigation:

Always verify CID content when integrity matters.

Verification means:
take the bytes you received, compute their CID, compare it to what you requested.

And the second mitigation is operational:
reduce adversaries.
If nobody can place malicious content where you might accept it, then your threat model collapses toward “accidents only”.

## 10:40 — Caching forever (and why verification complements caching)
Once you embrace the “true enough” model, the caching story becomes extremely clean.

Because the CID describes content, not a mutable name, you can ship caching headers that treat the response as immutable—effectively forever.

The FAQ even calls out a canonical form:
`Cache-Control: public, max-age=31536000, immutable`

But caching is not a substitute for verification.
Caching is about performance.
Verification is about trust.

## 11:25 — Wrap-up: what to remember
A 256t CID is short enough to be a URL path segment and rigid enough to be a universal content tag:

- 8 characters of length
- plus up to 86 characters of hash or literal content
- base64url, padding omitted
- inline content for ≤ 64 bytes, hash for > 64 bytes

Collision risk exists as a theoretical crack in the abstraction, but it is managed by two simple practices:
verify content, and reduce adversarial placement.

And once you do that, you get an unusually powerful idea:
a permanent, portable, URL-friendly identifier for bytes—independent of where they live.
