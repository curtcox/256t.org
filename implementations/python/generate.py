from cid import CIDS_DIR, EXAMPLES_DIR, compute_cid


def main() -> int:
    CIDS_DIR.mkdir(exist_ok=True)
    for example in sorted(EXAMPLES_DIR.iterdir()):
        if example.is_dir():
            continue
        content = example.read_bytes()
        cid = compute_cid(content)
        destination = CIDS_DIR / cid
        destination.write_bytes(content)
        print(f"Wrote {destination.name} from {example.name}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
