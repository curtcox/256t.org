import sys
from cid import CIDS_DIR, compute_cid, download_cid


def main() -> int:
    mismatches = []
    download_failures = []
    count = 0
    base_url = "https://256t.org"
    
    for path in sorted(CIDS_DIR.iterdir()):
        if path.is_dir():
            continue
        count += 1
        cid = path.name
        local_content = path.read_bytes()
        expected = compute_cid(local_content)
        
        # Check local CID file
        if cid != expected:
            mismatches.append((path, expected))
        
        # Check downloaded content
        try:
            downloaded_content, computed_cid, is_valid = download_cid(base_url, cid)
            if not is_valid:
                download_failures.append((cid, computed_cid))
            elif downloaded_content != local_content:
                download_failures.append((cid, "content mismatch with local file"))
        except Exception as e:
            download_failures.append((cid, str(e)))
    
    has_errors = False
    
    if mismatches:
        print("Found CID mismatches:")
        for path, expected in mismatches:
            print(f"- {path.name} should be {expected}")
        has_errors = True
    
    if download_failures:
        print("Found download validation failures:", file=sys.stderr)
        for cid, error in download_failures:
            print(f"- {cid}: {error}", file=sys.stderr)
        has_errors = True
    
    if has_errors:
        return 1
        
    print(f"All {count} CID files match their contents.")
    print(f"All {count} downloaded CIDs are valid.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
