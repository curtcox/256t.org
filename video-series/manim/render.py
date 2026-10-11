#!/usr/bin/env python3
"""Render episodes scene by scene and stitch each into one video.

    python3 video-series/manim/render.py 1            # episode 1, low quality preview
    python3 video-series/manim/render.py 1 3 -q h     # episodes 1 and 3 at 1080p
    python3 video-series/manim/render.py 2 --scene S05SmallThings
    python3 video-series/manim/render.py 4 --sheet    # also write a contact sheet per scene

Scenes are the classes in epNN.py whose names start with S<two digits>, in
order. Output lands in video-series/manim/output/: epNN.mp4 for the episode
and, with --sheet, epNN-SMM.png contact sheets for quick visual review.

Uses the `manim` command on PATH (Manim Community), plus ffmpeg/ffprobe.
"""

import argparse
import ast
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUTPUT = HERE / "output"
MEDIA = HERE / "media"
QUALITY_DIRS = {"l": "480p15", "m": "720p30", "h": "1080p60", "k": "2160p60"}


def scene_names(episode: int) -> list[str]:
    source = (HERE / f"ep{episode:02d}.py").read_text()
    names = [node.name for node in ast.parse(source).body
             if isinstance(node, ast.ClassDef) and node.name[:1] == "S" and node.name[1:3].isdigit()]
    return sorted(names)


def render_scene(episode: int, scene: str, quality: str) -> Path:
    module = HERE / f"ep{episode:02d}.py"
    subprocess.run(["manim", f"-q{quality}", "--media_dir", str(MEDIA), str(module), scene],
                   cwd=HERE, check=True)
    return MEDIA / "videos" / module.stem / QUALITY_DIRS[quality] / f"{scene}.mp4"


def contact_sheet(video: Path, out: Path, frames: int = 12) -> None:
    duration = float(subprocess.run(
        ["ffprobe", "-v", "error", "-show_entries", "format=duration",
         "-of", "default=noprint_wrappers=1:nokey=1", str(video)],
        capture_output=True, text=True, check=True).stdout)
    fps = frames / max(duration, 0.1)
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-i", str(video),
                    "-vf", f"fps={fps:.5f},scale=480:-1,tile=4x3:padding=4:color=gray",
                    "-frames:v", "1", str(out)], check=True)


def has_audio(video: Path) -> bool:
    streams = subprocess.run(["ffprobe", "-v", "error", "-select_streams", "a", "-show_entries",
                              "stream=index", "-of", "csv=p=0", str(video)],
                             capture_output=True, text=True, check=True).stdout
    return bool(streams.strip())


def with_uniform_audio(video: Path, out: Path) -> Path:
    """Copy the video and give it a 48 kHz stereo AAC track (silence if it has none),
    so scenes with and without narration can be concatenated losslessly."""
    if has_audio(video):
        cmd = ["ffmpeg", "-y", "-loglevel", "error", "-i", str(video), "-c:v", "copy",
               "-c:a", "aac", "-ar", "48000", "-ac", "2", str(out)]
    else:
        cmd = ["ffmpeg", "-y", "-loglevel", "error", "-i", str(video),
               "-f", "lavfi", "-i", "anullsrc=r=48000:cl=stereo", "-shortest",
               "-c:v", "copy", "-c:a", "aac", str(out)]
    subprocess.run(cmd, check=True)
    return out


def stitch(videos: list[Path], out: Path) -> None:
    work = out.parent / f".{out.stem}-parts"
    work.mkdir(exist_ok=True)
    parts = [with_uniform_audio(v, work / v.name) for v in videos]
    listing = work / "list.txt"
    listing.write_text("".join(f"file '{p}'\n" for p in parts))
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "concat", "-safe", "0",
                    "-i", str(listing), "-c", "copy", str(out)], check=True)
    shutil.rmtree(work)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("episodes", nargs="+", type=int)
    parser.add_argument("-q", "--quality", default="l", choices=QUALITY_DIRS)
    parser.add_argument("--scene", help="render only this scene class")
    parser.add_argument("--sheet", action="store_true", help="write a contact sheet per scene")
    args = parser.parse_args()

    if not shutil.which("manim"):
        print("manim is not on PATH (install Manim Community)", file=sys.stderr)
        return 1
    OUTPUT.mkdir(exist_ok=True)
    for episode in args.episodes:
        scenes = [args.scene] if args.scene else scene_names(episode)
        videos = []
        for scene in scenes:
            video = render_scene(episode, scene, args.quality)
            videos.append(video)
            if args.sheet:
                contact_sheet(video, OUTPUT / f"ep{episode:02d}-{scene[:3]}.png")
        if not args.scene:
            stitch(videos, OUTPUT / f"ep{episode:02d}.mp4")
            print(f"wrote {OUTPUT / f'ep{episode:02d}.mp4'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
