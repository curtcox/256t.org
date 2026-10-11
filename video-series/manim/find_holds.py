#!/usr/bin/env python3
"""Find long static or black stretches in rendered scenes.

    python3 video-series/manim/find_holds.py 1 2      # episodes 1 and 2, holds ≥ 6 s
    python3 video-series/manim/find_holds.py 4 --min 4 --quality h

For every scene video under media/, reports each stretch where the picture
doesn't change for at least --min seconds, or is entirely black for at least
--black seconds, together with the narration spoken during it (from the
ElevenLabs alignment when present). Render the episodes first with render.py:

    python3 video-series/manim/render.py 1 2 3 4 5 6 7
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
from pathlib import Path

from render import MEDIA, QUALITY_DIRS, scene_names

SERIES = Path(__file__).resolve().parents[1]


def detect(video: Path, freeze_min: float, black_min: float) -> list[tuple[str, float, float]]:
    filters = (f"fps=10,scale=320:-1,freezedetect=n=0.0002:d={freeze_min},"
               f"blackdetect=d={black_min}:pix_th=0.06:pic_th=0.9995")
    log = subprocess.run(["ffmpeg", "-i", str(video), "-vf", filters, "-an", "-f", "null", "-"],
                         capture_output=True, text=True).stderr
    spans = []
    starts = [float(x) for x in re.findall(r"freeze_start: ([\d.]+)", log)]
    ends = [float(x) for x in re.findall(r"freeze_end: ([\d.]+)", log)]
    if len(ends) < len(starts):  # frozen through the end of the clip
        ends.append(float(subprocess.run(
            ["ffprobe", "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", str(video)],
            capture_output=True, text=True).stdout))
    spans += [("static", s, e) for s, e in zip(starts, ends)]
    spans += [("black", float(s), float(e))
              for s, e in re.findall(r"black_start:([\d.]+) black_end:([\d.]+)", log)]
    return sorted(spans, key=lambda span: span[1])


def spoken(episode: int, number: int, start: float, end: float) -> str:
    path = SERIES / "audio" / f"ep{episode:02d}" / f"s{number:02d}.json"
    if not path.exists():
        return ""
    alignment = json.loads(path.read_text())["alignment"]
    chars = [c for c, t in zip(alignment["characters"], alignment["character_start_times_seconds"])
             if start <= t < end]
    return "".join(chars).strip()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("episodes", nargs="+", type=int)
    parser.add_argument("--min", type=float, default=6.0, help="shortest static stretch to report (s)")
    parser.add_argument("--black", type=float, default=0.6, help="shortest black stretch to report (s)")
    parser.add_argument("-q", "--quality", default="l", choices=QUALITY_DIRS)
    args = parser.parse_args()
    for episode in args.episodes:
        for scene in scene_names(episode):
            video = MEDIA / "videos" / f"ep{episode:02d}" / QUALITY_DIRS[args.quality] / f"{scene}.mp4"
            if not video.exists():
                print(f"ep{episode:02d} {scene}: not rendered")
                continue
            for kind, start, end in detect(video, args.min, args.black):
                text = spoken(episode, int(scene[1:3]), start, end)
                print(f"ep{episode:02d} {scene} {kind:6s} {start:6.1f}–{end:6.1f}s ({end - start:4.1f}s)  {text[:160]!r}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
