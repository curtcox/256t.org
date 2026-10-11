#!/usr/bin/env python3
"""Extract ElevenLabs-ready narration from the episode scripts.

The episode markdown files are the single source of truth. For every scene, the
blockquote lines under "**Narration**" are collected, markdown is stripped, the
pronunciation map below is applied, and the result is written to:

    narration/epNN/sMM.txt   one file per scene (one audio clip per Manim scene)
    narration/epNN.txt       the whole episode, scenes separated by blank lines

It also rewrites the runtime/word-count line at the top of each script.

Run from anywhere:  python3 video-series/extract_narration.py
"""

import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT = HERE / "narration"
WORDS_PER_MINUTE = 150

# Applied in order, as whole-word regex replacements. Edit to taste after
# auditioning the voice; ElevenLabs pronunciation dictionaries work too.
PRONUNCIATIONS = [
    (r"t256", "tee two fifty-six"),
    (r"256t\.org", "two fifty-six tee dot org"),
    (r"256t", "two fifty-six tee"),
    (r"SHA-512", "SHA five-twelve"),
    (r"SHA-256", "SHA two-fifty-six"),
    (r"SHA-1", "SHA one"),
    (r"MD5", "M D five"),
    (r"base64url", "base sixty-four U R L"),
    (r"base64", "base sixty-four"),
    (r"CIDs", "C-I-Ds"),
    (r"CID", "C-I-D"),
    (r"IPFS", "I P F S"),
    (r"URIs", "U-R-Is"),
    (r"PNG", "P N G"),
    (r"GPU", "G P U"),
    (r"CWI", "C W I"),
]

SCENE_RE = re.compile(r"^## Scene (\d+)\s*[—-]\s*(.+)$")


def speakable(text: str) -> str:
    text = re.sub(r"[*_`]", "", text)
    for pattern, spoken in PRONUNCIATIONS:
        text = re.sub(rf"(?<![\w-]){pattern}(?![\w])", spoken, text)
    # A replacement can land at the start of a sentence; keep it capitalized.
    for _, spoken in PRONUNCIATIONS:
        text = re.sub(rf"(^|[.!?]\s+){re.escape(spoken)}",
                      lambda m: m.group(1) + spoken[0].upper() + spoken[1:], text)
    return text


def update_header(script: Path, words: int) -> None:
    """Keep the runtime and word-count line in the script header in sync."""
    minutes = words / WORDS_PER_MINUTE
    header = (f"**Target runtime:** ~{round(minutes + 1.5)} minutes "
              f"(≈{minutes:.1f} min of narration at {WORDS_PER_MINUTE} wpm, "
              f"plus animation holds) · **Narration:** {words:,} words")
    text = script.read_text()
    updated = re.sub(r"^\*\*Target runtime:\*\*.*$", header, text,
                     count=1, flags=re.MULTILINE)
    if updated != text:
        script.write_text(updated)


def scenes(markdown: str):
    """Yield (number, title, [paragraphs]) for each scene."""
    current = None
    in_narration = False
    paragraph: list[str] = []

    def flush():
        if paragraph and current is not None:
            current[2].append(" ".join(paragraph))
        paragraph.clear()

    for line in markdown.splitlines():
        match = SCENE_RE.match(line)
        if match:
            flush()
            if current:
                yield current
            current = (int(match.group(1)), match.group(2).strip(), [])
            in_narration = False
        elif line.strip() == "**Narration**":
            in_narration = True
        elif in_narration and line.startswith(">"):
            body = line[1:].strip()
            if body:
                paragraph.append(body)
            else:
                flush()
        elif in_narration and line.strip() and not line.startswith(">"):
            flush()
            in_narration = False
    flush()
    if current:
        yield current


def main() -> int:
    total_words = 0
    for script in sorted(HERE.glob("ep[0-9][0-9]-*.md")):
        episode = script.name[:4]
        scene_dir = OUT / episode
        scene_dir.mkdir(parents=True, exist_ok=True)
        for stale in scene_dir.glob("s*.txt"):
            stale.unlink()

        episode_parts = []
        episode_words = 0
        for number, title, paragraphs in scenes(script.read_text()):
            text = "\n\n".join(speakable(p) for p in paragraphs)
            (scene_dir / f"s{number:02d}.txt").write_text(text + "\n")
            episode_parts.append(text)
            episode_words += len(" ".join(paragraphs).split())

        (OUT / f"{episode}.txt").write_text("\n\n".join(episode_parts) + "\n")
        update_header(script, episode_words)
        minutes = episode_words / WORDS_PER_MINUTE
        print(f"{script.name}: {len(episode_parts)} scenes, "
              f"{episode_words} words, ~{minutes:.1f} min narration")
        total_words += episode_words

    print(f"total: {total_words} words, ~{total_words / WORDS_PER_MINUTE:.0f} min")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
