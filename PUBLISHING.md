# Publishing and Storage

For deploying content-addressed storage using CIDs, this repository includes tools for uploading to Cloudflare R2 (S3-compatible storage).

## R2 Upload Script

The [`.github/scripts/r2_upload.py`](.github/scripts/r2_upload.py) script uploads files to R2 with metadata-based CID verification:

- Stores the CID in object metadata (`Metadata={'cid': cid}`)
- Verifies objects using `head_object()` to check metadata (fast, no download)
- Falls back to downloading and computing CID if metadata is missing
- Does not rely on ETag (which cannot be set and is unreliable for multipart uploads)

See [`.github/scripts/README.md`](.github/scripts/README.md) for detailed usage, examples, and a Cloudflare Worker snippet showing how to override the ETag header for clients.

---

[← Back to Home](index.html)
