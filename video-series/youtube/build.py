#!/usr/bin/env python3
"""Write the YouTube upload files for every episode.

    python3 video-series/youtube/build.py               # titles, descriptions, tags
    python3 video-series/youtube/build.py --thumbnails  # also re-render thumbnails
    python3 video-series/youtube/build.py --check       # exit 1 if anything is stale

Edit metadata.json, not the generated files. For each episode this writes
epNN/title.txt, epNN/description.txt and epNN/tags.txt, plus playlist.txt.
Chapter timestamps come from the rendered 1080p scenes, so rebuild after
re-rendering with `python3 video-series/manim/render.py N -q h`. Thumbnails are
the scenes in manim/thumbnails.py, rendered at 1280×720 to epNN/thumbnail.png.

Once episodes are uploaded, add their URLs ("url" per episode, "playlist_url"
for the series) to metadata.json and rebuild: the descriptions then link to the
previous and next episodes and the playlist. Paste the new descriptions in.
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
SERIES = HERE.parent
MANIM = SERIES / "manim"
REPO_URL = "https://github.com/curtcox/256t.org/blob/main/video-series/"

sys.path.insert(0, str(MANIM))
from render import duration, scene_names  # noqa: E402

# YouTube's limits for titles, descriptions and tags.
TITLE_LIMIT, DESCRIPTION_LIMIT, TAGS_LIMIT = 100, 5000, 500
MIN_CHAPTER_SECONDS = 10

problems = []


def timestamp(seconds: float) -> str:
    seconds = int(seconds)
    hours, rest = divmod(seconds, 3600)
    minutes, secs = divmod(rest, 60)
    return f"{hours}:{minutes:02d}:{secs:02d}" if hours else f"{minutes}:{secs:02d}"


def chapter_starts(number: int) -> list[float] | None:
    """Start time of each scene in the stitched episode, from the 1080p renders."""
    starts, elapsed = [], 0.0
    for scene in scene_names(number):
        video = MANIM / "media" / "videos" / f"ep{number:02d}" / "1080p60" / f"{scene}.mp4"
        if not video.exists():
            return None
        starts.append(elapsed)
        elapsed += duration(video)
    return starts


def chapters(episode: dict) -> list[str]:
    number, titles = episode["number"], episode["chapters"]
    scenes = scene_names(number)
    if len(titles) != len(scenes):
        problems.append(f"episode {number}: {len(titles)} chapter titles for {len(scenes)} scenes")
        return []
    starts = chapter_starts(number)
    if starts is None:
        problems.append(f"episode {number}: render 1080p scenes first (render.py {number} -q h)")
        return []
    for (start, title), following in zip(zip(starts, titles), starts[1:] + [None]):
        if following is not None and following - start < MIN_CHAPTER_SECONDS:
            problems.append(f"episode {number}: chapter {title!r} is shorter than {MIN_CHAPTER_SECONDS}s")
    return [f"{timestamp(start)} {title}" for start, title in zip(starts, titles)]


def neighbor(episodes: list[dict], index: int, label: str) -> str | None:
    if not 0 <= index < len(episodes):
        return None
    other = episodes[index]
    name = other["title"].split(" | ")[0]
    line = f"{label}: Ep. {other['number']}, {name}"
    return f"{line}: {other['url']}" if other.get("url") else line


def description(series: dict, episodes: list[dict], index: int) -> str:
    episode = episodes[index]
    parts = [episode["hook"], *episode["summary"]]
    marks = chapters(episode)
    if marks:
        parts.append("Chapters\n" + "\n".join(marks))
    around = [f"Episode {episode['number']} of {len(episodes)} in the series \"{series['playlist_title']}\"."]
    around += [line for line in (neighbor(episodes, index - 1, "Previous"), neighbor(episodes, index + 1, "Next")) if line]
    if series.get("playlist_url"):
        around.append(f"Full series: {series['playlist_url']}")
    parts.append("\n".join(around))
    if episode.get("sources"):
        parts.append("Sources\n" + "\n".join(f"- {name}: {url}" for name, url in episode["sources"]))
    links = [f"- {name}: {url}" for name, url in series["links"]]
    links.append(f"- This episode's script: {REPO_URL}{episode['script']}")
    parts.append("Links\n" + "\n".join(links))
    parts.append(series["credits"])
    parts.append(" ".join(f"#{tag}" for tag in episode["hashtags"]))
    return "\n\n".join(parts) + "\n"


def check_text(number: int, kind: str, text: str, limit: int) -> None:
    if len(text) > limit:
        problems.append(f"episode {number}: {kind} is {len(text)} characters (limit {limit})")
    if "<" in text or ">" in text:
        problems.append(f"episode {number}: {kind} contains < or >, which YouTube rejects")


def render_thumbnails(episodes: list[dict]) -> None:
    source = (MANIM / "thumbnails.py").read_text()
    for episode in episodes:
        number = episode["number"]
        scene = next(line.split("(")[0].removeprefix("class ").strip()
                     for line in source.splitlines() if line.startswith(f"class T{number:02d}"))
        subprocess.run(["manim", "-s", "-r", "1280,720", "--media_dir", str(MANIM / "media"),
                        str(MANIM / "thumbnails.py"), scene], cwd=MANIM, check=True,
                       stdout=subprocess.DEVNULL)
        rendered = next((MANIM / "media" / "images" / "thumbnails").glob(f"{scene}_*.png"))
        target = HERE / f"ep{number:02d}" / "thumbnail.png"
        target.parent.mkdir(exist_ok=True)
        shutil.copyfile(rendered, target)
        if target.stat().st_size > 2 * 1024 * 1024:
            problems.append(f"episode {number}: thumbnail is over YouTube's 2 MB limit")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--thumbnails", action="store_true", help="re-render the thumbnails too")
    parser.add_argument("--check", action="store_true", help="report stale files instead of writing them")
    args = parser.parse_args()

    metadata = json.loads((HERE / "metadata.json").read_text(encoding="utf-8"))
    series, episodes = metadata["series"], metadata["episodes"]
    outputs = {HERE / "playlist.txt": f"{series['playlist_title']}\n\n{series['playlist_description']}\n"}
    for index, episode in enumerate(episodes):
        number = episode["number"]
        folder = HERE / f"ep{number:02d}"
        text = description(series, episodes, index)
        tags = list(dict.fromkeys(series["tags"] + episode["tags"]))
        tag_line = ", ".join(tags)
        check_text(number, "title", episode["title"], TITLE_LIMIT)
        check_text(number, "description", text, DESCRIPTION_LIMIT)
        check_text(number, "tags", tag_line, TAGS_LIMIT)
        outputs[folder / "title.txt"] = episode["title"] + "\n"
        outputs[folder / "description.txt"] = text
        outputs[folder / "tags.txt"] = tag_line + "\n"

    stale = [path for path, text in outputs.items()
             if not path.exists() or path.read_text(encoding="utf-8") != text]
    if not args.check:
        for path in stale:
            path.parent.mkdir(exist_ok=True)
            path.write_text(outputs[path], encoding="utf-8")
        if args.thumbnails:
            render_thumbnails(episodes)
    for path in stale:
        print(("stale: " if args.check else "wrote: ") + str(path.relative_to(SERIES)))
    for problem in problems:
        print("! " + problem, file=sys.stderr)
    return 1 if problems or (args.check and stale) else 0


if __name__ == "__main__":
    sys.exit(main())
