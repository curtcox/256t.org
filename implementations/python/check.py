from cid import CIDS_DIR, compute_cid


def main() -> int:
    mismatches = []
    count = 0
    for path in sorted(CIDS_DIR.iterdir()):
        if path.is_dir():
            continue
        count += 1
        actual = path.name
        expected = compute_cid(path.read_bytes())
        if actual != expected:
            mismatches.append((path, expected))
    if mismatches:
        print("Found CID mismatches:")
        for path, expected in mismatches:
            print(f"- {path.name} should be {expected}")
        return 1
    print(f"All {count} CID files match their contents.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
