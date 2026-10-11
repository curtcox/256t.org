# Episode 1 — Where vs. What: Names That Can't Break

**Target runtime:** ~12 minutes (≈10.5 min of narration at 150 wpm, plus animation holds) · **Narration:** 1,575 words

**Episode goal:** Make the viewer feel the problem (link rot and content drift), then build the
idea of content addressing from scratch so that 256t's design feels inevitable rather than arbitrary.

**New recurring visuals introduced** (see `README.md` style guide):
`ByteRibbon`, `RecipeMachine`, `CIDTag`, `ServerRack`, `VerifierLoop`, `CacheBox`.

---

## Scene 1 — Cold open: the dead link
*Target: 0:00–1:15*

**Visuals (Manim)**
- Dark background. A paragraph of generic body text (lorem-style, not a real document) fades in, with
  one phrase underlined in BLUE as a hyperlink.
- A cursor arrow glides to the link and clicks. Hard cut: the whole frame becomes a plain grey
  `404 Not Found` card.
- Rewind effect (frames scrub backwards). Click again. This time a page loads, but individual words in
  it morph into different words one at a time. A small label slides in: `same address, different content`.
- Two stat cards stack on the left side as they're mentioned:
  - `~38% of pages from 2013 unreachable by 2023` (small source line: `Pew Research Center, 2024`)
  - `~50% of links in U.S. Supreme Court opinions no longer led to the cited material`
    (source line: `Zittrain, Albert & Lessig, Harvard Law Review Forum, 2014`)
- Title card: **Where vs. What** with the 256t logo mark small in a corner.

**Narration**
> Here is a sentence you have seen a hundred times. Page not found.
>
> In 2024, Pew Research went back to a large sample of web pages that existed in 2013, and tried to load them again. About thirty-eight percent were simply gone. A decade earlier, a team at Harvard Law School looked at the web links cited in United States Supreme Court opinions, documents that are supposed to last for centuries, and found that about half of them no longer led to the material the Justices had cited.
>
> And there is a quieter failure that is arguably worse. Sometimes the link still works. It just doesn't show you what it used to. The page was edited. The file was replaced. The dataset was quietly updated. Nothing warns you. Same address, different content.
>
> This series is about a small, almost stubbornly simple idea for fixing both problems at once. It's written up at a site called 256t.org. But before we get to its specifics, I want to spend this first episode on the idea underneath it, because once you see that idea clearly, most of the design choices we'll look at later will feel less like choices and more like consequences.

---

## Scene 2 — What a URL actually says
*Target: 1:15–2:25*

**Visuals (Manim)**
- The URL `https://example.com/reports/2013/annual.pdf` writes itself across the screen in monospace.
- Braces drop beneath each piece with labels:
  - `example.com` → `which machine`
  - `/reports/2013/` → `which folder`
  - `annual.pdf` → `which name in that folder`
- The three labels morph into imperative instructions: `go here` → `then here` → `then ask for this`.
- Transition: the URL shrinks into a map pin on a stylized street grid. A small house sits under the pin.
- The house crumbles into an empty lot (label `link rot`). Rewind. A *different* house rises in the
  same spot (label `content drift`). The pin never moves.

**Narration**
> Let's look closely at what an ordinary web address actually says.
>
> Every part of it is about location. This part says which computer to contact. This part says which folder on that computer. And this last part says which name to ask for inside that folder. Read it out loud and it's really a set of directions. Go here. Then go here. Then ask for this.
>
> That's a street address. And a street address is a great way to find a house. It's a terrible way to identify one.
>
> If the house is torn down, the address leads you to an empty lot. That's link rot. And if a different house gets built on the same spot, the address still leads you somewhere, quite confidently, just to the wrong thing. That's content drift. The address never promised anything about the house. It only ever promised a place.

---

## Scene 3 — Naming the thing instead of the place
*Target: 2:25–3:40*

**Visuals (Manim)**
- Split screen. Left half titled `Where?`: a server rack with a folder tree hanging off it.
  Right half titled `What?`: a horizontal `ByteRibbon` (a strip of small cells, each showing a hex byte).
- The right half expands to fill the screen. The ribbon slides into a `RecipeMachine` (a rounded box
  with a crank on the side and the label `public recipe`). The crank turns; a short `CIDTag` pops out the
  far side.
- On-screen equation fades in beneath: `name = f(content)`.
- Three different people icons (simple circles with different colors) at three different locations
  each feed an identical ribbon into their own copy of the machine. All three produce identical tags,
  which fly together and overlap perfectly.

**Narration**
> So here's the alternative. What if, instead of naming where some content lives, the name described what the content is?
>
> And I don't mean a description in words, like "annual report, 2013." I mean a name computed directly from the bytes themselves, by a fixed, public recipe. Feed the bytes in, turn the crank, and a name comes out.
>
> The crucial property is that the recipe doesn't depend on anything except the bytes. Not which computer you're on, not the file's name, not the date. So anyone, anywhere, running the same recipe on the same bytes, gets exactly the same name, without ever talking to each other.
>
> This idea is called content addressing. And it's worth pausing on what that name leaves out. It says nothing about which machine to contact. Nothing about folders. It's not directions to the house. It's more like a precise description of the house, so precise that only one house in the world could match it.

---

## Scene 4 — What you get for free
*Target: 3:40–5:40*

**Visuals (Manim)** — four panels arrive one at a time in a 2×2 grid, each with a number badge.
1. **Any copy will do.** One `CIDTag` at top; arrows fan out to five `ServerRack`s, a laptop, and a USB stick,
   all holding the same ribbon. Each lights up when "asked."
2. **You can check.** The `VerifierLoop`: incoming ribbon → `RecipeMachine` → computed tag compared to
   requested tag. First pass: tags match, GREEN check. Second pass: one byte of the ribbon is RED, the
   computed tag differs, RED X.
3. **Caching is easy.** A `CacheBox` with an ∞ symbol. A clock spins fast beside it; the cached copy
   never gets a "stale?" question mark.
4. **No silent edits.** A ribbon labeled `v1` with its tag. A hand edits one cell; the result is a new ribbon
   `v2` with a completely different tag. Both ribbons and both tags remain side by side.
- After all four: a fifth, smaller panel slides in at the bottom with a question mark: `the latest version?`
  An arrow labeled `pointer (different problem)` points from it to the `v2` tag.

**Narration**
> Once names are computed from content, a whole list of nice properties falls out, almost for free.
>
> First: any copy will do. If the name means "these exact bytes," then it stops mattering where the bytes come from. A big server, a library's archive, a friend's laptop, a cache on the other side of the planet. If someone has a copy, they can hand it to you, and it's just as good as the original, because it is the original.
>
> Second, and this is the one I find genuinely remarkable: you can check. When the bytes arrive, you run the recipe yourself. If the name you compute matches the name you asked for, you got the right thing. That's true even if the bytes came from a stranger you have no reason to trust. Trust moves away from the messenger and onto the math.
>
> Third, caching gets easy. The hardest question about any cached copy is, is this still current? With content names, that question disappears. A copy can't be stale, because the name means exactly these bytes, forever.
>
> And fourth, nothing can change underneath its name. If someone edits the file, they haven't changed the file. They've made a new file, with a new name. The old name still means the old bytes. Updates don't overwrite history. They sit beside it.
>
> Now, that last point has a flip side. A content name can never mean "the latest version of the annual report," because the latest version keeps changing, and the name can't. If you want that, you need a separate kind of pointer that you update over time. That's a real and interesting problem. But it's a different problem, built on top of this one. The scheme we're looking at only tries to solve the bottom layer: a name for some bytes that can't break.

---

## Scene 5 — Designing the recipe
*Target: 5:40–7:45*

**Visuals (Manim)**
- A checklist titled `What we want from f` appears on the right and fills in as the narration goes:
  - `short, fixed size`
  - `same bytes → same name`
  - `different bytes → different names`
  - `hard to fake`
- **Attempt 1: f = the content itself.** A short ribbon (`Hello`) passes through an empty machine
  unchanged; GREEN check. Then a movie-length ribbon enters and the output runs off the right edge of the
  screen; the camera pans to chase it and gives up. A yellow sticky note slaps onto the side of the screen:
  `perfect... for tiny things. Remember this.` (Keep this sticky note in the corner until Scene 6.)
- **Attempt 2: f = length.** Two different ribbons, both labeled `1,511 bytes`, produce the identical tag
  `1511`. RED X.
- **Attempt 3: f = first 8 bytes.** Three PNG file icons; each ribbon's first 8 cells highlight and read
  `89 50 4E 47 0D 0A 1A 0A`. All three tags are identical. RED X. Caption: `every PNG file starts this way`.
- **Attempt 4: f = sum of bytes, mod 256.** A ribbon; two cells swap places; the sum readout doesn't change.
  RED X. Caption: `easy to fake`.
- **Attempt 5: a cryptographic hash.** A ribbon enters a machine with a fingerprint icon. Out comes a
  long scrambled tag. One input cell flips one bit; the output tag re-scrambles so that roughly half of
  its characters change (animate the changed characters flashing). All four checklist items get checks,
  the third one with a small asterisk.

**Narration**
> So what should the recipe be? Let's write down what we want from it. The name should be short, and the same size no matter how big the content is, so it fits in a web address. The same bytes should always give the same name. Different bytes should give different names. And it should be hard for anyone to fake: hard to craft different bytes that produce a name you were expecting.
>
> The laziest possible recipe is to use the content itself as its name. And notice, that's perfect in every way but one. Same bytes, same name. Different bytes, different names, guaranteed. Impossible to fake. The only problem is size. A short word makes a lovely short name, but a movie makes a name that's as long as the movie. So that fails in general. But keep it in the back of your mind, because for very small things, it's hard to beat.
>
> What about the length of the content? Short, fixed size, but obviously lots of different files share a length. What about the first few bytes? Even worse than you'd think. Every PNG image in the world starts with the same eight bytes. What about a simple checksum, like adding up all the bytes? Better spread out, but trivial to fool. Swap two bytes and the sum doesn't even notice.
>
> What we actually want is something that takes any amount of data and scrambles it down to a short, fixed-size output, so thoroughly that changing a single bit anywhere in the input changes about half the bits of the output, in a way no one knows how to predict or steer. That's a cryptographic hash function. We'll spend an entire episode on what that phrase promises, and what it doesn't. For now, think of it as a fingerprint for data.
>
> And that asterisk on "different bytes give different names" is going to be our main character for a couple of episodes. Hold onto it.

---

## Scene 6 — Meet 256t
*Target: 7:45–9:00*

**Visuals (Manim)**
- The URL `https://256t.org/` writes on screen, and then the Gettysburg Address CID types itself out after
  the slash, character by character:
  `AAAAAAXnpY8KkcoLHubRMUTViZU671vd9x4IuH0gmZhLvp9TTl-K-J3UzKZCbOUP41fDBlxdcJHSTd7bVYAZvwvibsdFjA`
  (scale the whole URL to fit the frame width).
- Brace under the whole CID: `CID — content identifier — at most 94 characters`.
- The first 8 characters turn BLUE (`length`), the remaining 86 turn GOLD (`SHA-512 fingerprint`).
- A small inset shows the beginning of the Gettysburg text and `1,511 bytes`.
- The sticky note from Scene 5 peels off the corner and flies to the center. Beneath it appears the CID
  for `Hello`: `AAAAAAAFSGVsbG8` — first 8 BLUE, last 7 GREEN (`the content itself`).
- Final beat: a request arrow into a `ServerRack` with two possible return arrows: a ribbon with a GREEN
  check, or a `404` card. Caption: `matching bytes, or "not found." Nothing else.`

**Narration**
> 256t is one specific, deliberately minimal choice of recipe.
>
> The name it produces is called a CID, short for content identifier. A CID is at most ninety-four characters long, and it's built only from characters that are safe to use in web addresses and filenames. So it can sit at the end of a URL, or be the name of a file, or live in a database column, without any special escaping.
>
> Here's a real one. This is the CID of the Gettysburg Address, fifteen hundred and eleven bytes of text. Put it after 256t.org and you have a link to those exact bytes.
>
> A CID has two parts. The first eight characters encode the length of the content. The rest is a SHA-512 fingerprint of the content. Or, and here's that sticky note coming back, if the content is sixty-four bytes or less, the rest is just the content itself. Here's the CID for the word "Hello." It's short enough that its name simply contains it.
>
> And the rule for a server is just as minimal. Ask for a CID, and you either get back bytes that match it, or you get "not found." That's the whole contract.

---

## Scene 7 — The honest asterisk
*Target: 9:00–9:50*

**Visuals (Manim)**
- Pigeonhole animation: a large, dense cloud of ribbons on the left (`all possible files: infinitely many`),
  a smaller (but still large) grid of tags on the right (`all possible CIDs: finitely many`). Arrows from
  ribbons to tags; inevitably several arrows land on the same tag, and those tags pulse RED.
- A quote card fades in, styled as text from the spec:
  `"The following can be treated as true enough despite being false"` (source: `256t.org`)
  followed by three bullets:
  - `A 94 character path uniquely determines content.`
  - `The content is immutable.`
  - `Content can be safely cached indefinitely.`
- A hairline crack spreads across the card.

**Narration**
> Now, if you've been paying attention, something should be bothering you. There are infinitely many possible files, and only finitely many names of ninety-four characters or less. So some different files must share a name. That's not a weakness of the particular recipe. It's just counting.
>
> The 256t spec is refreshingly upfront about this. It lists a few statements it says you can treat as "true enough despite being false." That a CID uniquely determines its content. That the content is immutable. That it can be cached forever.
>
> So the real question isn't whether these statements are perfectly true. They aren't. The question is how true is true enough, and for whom. That's what most of this series is about.

---

## Scene 8 — Series map
*Target: 9:50–10:30*

**Visuals (Manim)**
- Seven episode cards arrange in a row, each with a small icon. The current card (1) dims slightly as the
  others highlight in turn:
  1. `Where vs. What` (map pin → fingerprint)
  2. `Anatomy of a CID` (ruler under a CID)
  3. `What a Hash Promises` (fingerprint)
  4. `Accidental Collisions` (dice)
  5. `Adversarial Collisions` (wrench / mask)
  6. `Verify, Cache, Trust` (check loop + ∞)
  7. `Limits and Neighbors` (map of related systems)

**Narration**
> Here's where we're going. Next time, we'll take a CID apart character by character, and you'll see that every number in the format, the eight, the sixty-four, the eighty-six, the ninety-four, and even the "two fifty-six" in the name, falls out of a little arithmetic. Then we'll look at what a hash function actually promises. Then two episodes on collisions: the accidental kind, where we'll find out just how true "true enough" is, and the adversarial kind, where someone is actively trying to fool you. After that, how verification and caching work in practice. And finally, what 256t deliberately leaves out, and how it compares to its neighbors.
>
> See you in the next one.
