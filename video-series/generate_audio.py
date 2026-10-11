#!/usr/bin/env python3
"""Generate narration audio with ElevenLabs, one clip per scene, with timestamps.

For each narration/epNN/sMM.txt this writes:

    audio/epNN/sMM.mp3    the spoken clip
    audio/epNN/sMM.json   the character-level alignment ElevenLabs returns, plus
                          a hash of the text it was made from

The Manim scenes pick these up automatically: a scene with audio runs exactly
as long as its clip, and `self.cue("phrase")` lands on the moment the phrase
is spoken. Clips whose text hasn't changed since they were generated are
skipped, so editing one scene only re-voices that scene.

    export ELEVENLABS_API_KEY=...        # never commit this
    export ELEVENLABS_VOICE_ID=...       # the narrator voice
    python3 video-series/generate_audio.py --dry-run      # what would be sent, and how many characters
    python3 video-series/generate_audio.py 1              # voice episode 1
    python3 video-series/generate_audio.py 1 --scene 3    # just episode 1, scene 3
    python3 video-series/generate_audio.py --force        # re-voice everything

Run extract_narration.py first so narration/ is current. Uses only the
standard library. ElevenLabs bills by character; --dry-run prints the count.
"""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import sys
import urllib.error
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
NARRATION = HERE / "narration"
AUDIO = HERE / "audio"
API = "https://api.elevenlabs.io/v1/text-to-speech/{voice}/with-timestamps?output_format={fmt}"
DEFAULT_MODEL = "eleven_multilingual_v2"
DEFAULT_FORMAT = "mp3_44100_128"


def scenes(episodes: list[int], only_scene: int | None):
    for ep_dir in sorted(NARRATION.glob("ep[0-9][0-9]")):
        episode = int(ep_dir.name[2:])
        if episodes and episode not in episodes:
            continue
        files = sorted(ep_dir.glob("s[0-9][0-9].txt"))
        for index, path in enumerate(files):
            number = int(path.stem[1:])
            if only_scene is not None and number != only_scene:
                continue
            previous = files[index - 1].read_text().strip() if index > 0 else None
            following = files[index + 1].read_text().strip() if index + 1 < len(files) else None
            yield episode, number, path.read_text().strip(), previous, following


def text_hash(text: str, voice: str, model: str) -> str:
    return hashlib.sha256(f"{voice}\n{model}\n{text}".encode()).hexdigest()


def up_to_date(json_path: Path, digest: str) -> bool:
    if not json_path.exists() or not json_path.with_suffix(".mp3").exists():
        return False
    try:
        return json.loads(json_path.read_text()).get("text_sha256") == digest
    except json.JSONDecodeError:
        return False


def synthesize(text: str, previous: str | None, following: str | None, voice: str, model: str,
               key: str, fmt: str, settings: dict | None) -> dict:
    body = {"text": text, "model_id": model}
    # Neighboring scenes keep intonation continuous across separately generated clips.
    if previous:
        body["previous_text"] = previous
    if following:
        body["next_text"] = following
    if settings:
        body["voice_settings"] = settings
    request = urllib.request.Request(
        API.format(voice=voice, fmt=fmt), data=json.dumps(body).encode(), method="POST",
        headers={"xi-api-key": key, "Content-Type": "application/json", "Accept": "application/json"})
    with urllib.request.urlopen(request, timeout=300) as response:
        return json.loads(response.read())


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("episodes", nargs="*", type=int, help="episode numbers (default: all)")
    parser.add_argument("--scene", type=int, help="only this scene number")
    parser.add_argument("--force", action="store_true", help="regenerate even if the text is unchanged")
    parser.add_argument("--dry-run", action="store_true", help="list what would be generated; send nothing")
    parser.add_argument("--model", default=os.environ.get("ELEVENLABS_MODEL_ID", DEFAULT_MODEL))
    parser.add_argument("--format", default=DEFAULT_FORMAT, help="ElevenLabs output_format (mp3 formats only)")
    parser.add_argument("--settings", help='voice_settings as JSON, e.g. \'{"stability": 0.5, "similarity_boost": 0.75}\'')
    args = parser.parse_args()

    voice = os.environ.get("ELEVENLABS_VOICE_ID", "")
    key = os.environ.get("ELEVENLABS_API_KEY", "")
    if not args.dry_run and (not voice or not key):
        print("Set ELEVENLABS_API_KEY and ELEVENLABS_VOICE_ID (or use --dry-run).", file=sys.stderr)
        return 1
    settings = json.loads(args.settings) if args.settings else None

    pending, characters = 0, 0
    for episode, number, text, previous, following in scenes(args.episodes, args.scene):
        stem = AUDIO / f"ep{episode:02d}" / f"s{number:02d}"
        digest = text_hash(text, voice, args.model)
        if not args.force and up_to_date(stem.with_suffix(".json"), digest):
            continue
        pending += 1
        characters += len(text)
        name = f"ep{episode:02d}/s{number:02d}"
        if args.dry_run:
            print(f"{name}: {len(text):5d} characters  {text[:60]!r}…")
            continue
        print(f"{name}: voicing {len(text)} characters…", flush=True)
        try:
            result = synthesize(text, previous, following, voice, args.model, key, args.format, settings)
        except urllib.error.HTTPError as error:
            print(f"{name}: ElevenLabs returned {error.code}: {error.read().decode(errors='replace')[:500]}",
                  file=sys.stderr)
            return 1
        stem.parent.mkdir(parents=True, exist_ok=True)
        stem.with_suffix(".mp3").write_bytes(base64.b64decode(result["audio_base64"]))
        stem.with_suffix(".json").write_text(json.dumps({
            "text": text, "text_sha256": digest, "voice_id": voice, "model_id": args.model,
            "alignment": result.get("alignment"),
        }, indent=1))
    verb = "would voice" if args.dry_run else "voiced"
    print(f"{verb} {pending} scene(s), {characters:,} characters")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
