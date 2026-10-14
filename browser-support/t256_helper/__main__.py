"""Run with python3 browser-support/run.py, or python3 -m t256_helper."""

import argparse
import json
import os
from pathlib import Path
import sys
import subprocess

from .resolver import Resolver


def default_directory():
    if os.environ.get("T256_HOME"):
        return Path(os.environ["T256_HOME"]).expanduser()
    if sys.platform == "darwin":
        return Path.home() / "Library/Application Support/t256"
    if sys.platform == "win32":
        return Path(os.environ.get("LOCALAPPDATA", str(Path.home()))) / "t256"
    return Path(os.environ.get("XDG_CONFIG_HOME", str(Path.home() / ".config"))) / "t256"


def main():
    parser = argparse.ArgumentParser(description="Resolve verified t256 content using your selected providers")
    parser.add_argument("--state", type=Path, default=default_directory(), help="Settings and cache directory")
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("init", help="Create a private settings directory with Hashbin as a provider")
    commands.add_parser("config", help="Show provider configuration (credentials remain in environment variables)")
    add = commands.add_parser("add", help="Add a content provider")
    add.add_argument("id")
    add.add_argument("type", choices=["folder", "http", "github", "git"])
    add.add_argument("location", help="Folder, base URL, owner/repo, or local Git clone")
    add.add_argument("--ref", default="HEAD")
    add.add_argument("--directory", default="cids")
    add.add_argument("--token-env", help="Name of a credential environment variable; never its value")
    add.add_argument("--use-gh", action="store_true", help="Use GitHub CLI login for a GitHub provider")
    add.add_argument("--api-url", default="https://api.github.com", help="GitHub API base (supports Enterprise)")
    remove = commands.add_parser("remove"); remove.add_argument("id")
    for name in ("enable", "disable", "index"):
        command = commands.add_parser(name); command.add_argument("id")
    import_index = commands.add_parser("import-index", help="Import a JSON object mapping CIDs to relative provider paths")
    import_index.add_argument("id"); import_index.add_argument("file", type=Path)
    allow = commands.add_parser("allow", help="Grant a website origin read access to selected provider content")
    allow.add_argument("origin")
    fetch = commands.add_parser("resolve", help="Resolve and verify content")
    fetch.add_argument("uri")
    fetch.add_argument("--output", type=Path, help="Write verified bytes to this file")
    serve = commands.add_parser("serve", help="Start the local resolver and settings page")
    serve.add_argument("--port", type=int, default=25600)
    serve.add_argument("--open", action="store_true", help="Open the settings page")
    open_command = commands.add_parser("open", help="Open a t256 URI in the local browser gateway")
    open_command.add_argument("uri")
    native = commands.add_parser("native", help="Browser native messaging host")
    native.add_argument("browser_args", nargs=argparse.REMAINDER, help=argparse.SUPPRESS)
    commands.add_parser("settings", help="Open settings, starting the helper if necessary")
    install = commands.add_parser("install", help="Install per-user t256 scheme registration")
    install.add_argument("--browser", choices=["chrome", "edge", "brave", "opera", "firefox"])
    install.add_argument("--extension-id", help="Also register a browser native messaging host")
    commands.add_parser("uninstall", help="Remove registrations created by this helper")
    args = parser.parse_args()
    try:
        resolver = Resolver(args.state)
        config = json.loads(json.dumps(resolver.config))
        if args.command in ("init", "config"):
            print(json.dumps(config, indent=2))
        elif args.command == "add":
            key = "path" if args.type in ("folder", "git") else "repo" if args.type == "github" else "url"
            provider = {"id": args.id, "type": args.type, key: args.location, "enabled": True}
            if args.type in ("git", "github"):
                provider.update(ref=args.ref, directory=args.directory)
            if args.type == "github":
                provider.update(use_gh=args.use_gh, api_url=args.api_url)
            if args.token_env:
                provider["token_env"] = args.token_env
            config["providers"].append(provider)
            resolver.configure(config)
            print("Added provider " + args.id)
        elif args.command == "remove":
            config["providers"] = [p for p in config["providers"] if p["id"] != args.id]
            resolver.configure(config)
        elif args.command in ("enable", "disable"):
            provider = next(p for p in config["providers"] if p["id"] == args.id)
            provider["enabled"] = args.command == "enable"
            resolver.configure(config)
        elif args.command == "index":
            print("Indexed " + str(resolver.index(args.id)) + " CIDs")
        elif args.command == "import-index":
            print("Imported " + str(resolver.import_index(args.id, json.loads(args.file.read_text(encoding="utf-8")))) + " CIDs")
        elif args.command == "allow":
            if args.origin not in config["allowed_origins"]:
                config["allowed_origins"].append(args.origin)
            resolver.configure(config)
            print("Granted provider read access to " + args.origin)
        elif args.command == "resolve":
            uri, data, provenance = resolver.resolve(args.uri)
            if args.output:
                args.output.write_bytes(data)
            print(json.dumps({"uri": uri.text, "length": uri.length, **provenance}, indent=2))
        elif args.command == "serve":
            from .server import serve
            serve(resolver, args.port, args.open)
        elif args.command in ("open", "native", "settings"):
            from .desktop import open_uri, native_host, ensure_gateway
            if args.command == "open": open_uri(resolver, args.uri)
            elif args.command == "native": native_host(resolver)
            else:
                import webbrowser
                webbrowser.open(ensure_gateway(resolver)["settings_url"])
        elif args.command in ("install", "uninstall"):
            from .install import install, uninstall
            install(resolver, args.browser, args.extension_id) if args.command == "install" else uninstall(resolver)
    except (OSError, ValueError, StopIteration, RuntimeError, subprocess.SubprocessError) as error:
        print("t256: " + (str(error) or "Provider not found"), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
