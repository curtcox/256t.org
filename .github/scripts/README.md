# 256t.org R2 Upload Scripts

This directory contains scripts for uploading and verifying content-addressed files to Cloudflare R2 storage.

## Overview

The upload script (`r2_upload.py`) implements content-addressable storage using CIDs (Content Identifiers) as object keys in R2. It stores the CID in object metadata for reliable verification.

## Why Metadata Instead of ETag?

**Important:** R2 (like AWS S3) generates the ETag header server-side, and it **cannot be set** during PUT operations. This makes ETag unreliable for CID verification:

- The ETag is controlled by the storage service, not the client
- For multipart uploads, ETag is not a simple hash but a composite value
- There's no way to override or set ETag to match the CID

**Solution:** We store the canonical CID in object metadata (`Metadata={'cid': cid}`), which:
- Provides a reliable, queryable place to validate content-addressed object identity
- Can be retrieved quickly via `head_object()` without downloading the entire object
- Falls back to downloading and computing the CID if metadata is missing

## Installation

The script requires `boto3`:

```bash
pip install boto3
```

## Usage

### Upload a file

```bash
python .github/scripts/r2_upload.py examples/hello.txt --bucket 256t-org
```

### Verify an existing object

```bash
python .github/scripts/r2_upload.py examples/hello.txt --bucket 256t-org --verify-only
```

### With custom endpoint URL

```bash
python .github/scripts/r2_upload.py examples/hello.txt \
  --bucket 256t-org \
  --endpoint-url https://your-account-id.r2.cloudflarestorage.com
```

## Configuration

The script uses standard AWS credential resolution:
- Environment variables (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`)
- AWS credentials file (`~/.aws/credentials`)
- IAM roles (when running on AWS/EC2)

For Cloudflare R2, you'll need:
- R2 Access Key ID
- R2 Secret Access Key
- Optionally, the R2 endpoint URL

## Upload Behavior

When uploading a file, the script:

1. **Computes the CID** from the file content
2. **Checks if the object exists** using `head_object()`
3. **Verifies via metadata** if present:
   - If `Metadata['cid']` matches: Skip upload (already valid)
   - If `Metadata['cid']` doesn't match: Error (integrity violation)
4. **Falls back to download verification** if metadata is missing
5. **Uploads with metadata** if object doesn't exist:

```python
s3.put_object(
    Bucket=bucket,
    Key=cid,
    Body=data,
    CacheControl='public, max-age=31536000, immutable',
    ContentType='application/octet-stream',
    Metadata={'cid': cid}
)
```

## Verification Process

The script verifies objects in two ways:

### Fast Path: Metadata Verification

```python
head = s3.head_object(Bucket=bucket, Key=cid)
remote_meta_cid = head.get('Metadata', {}).get('cid')
if remote_meta_cid == cid:
    # Valid! Content-addressing satisfied without download
    pass
```

### Fallback: Download and Compute

```python
# If metadata is missing, download and verify
obj = s3.get_object(Bucket=bucket, Key=cid)
remote_bytes = obj['Body'].read()
remote_cid = compute_cid(remote_bytes)
if remote_cid == cid:
    # Valid! But consider re-uploading to add metadata
    pass
```

## Cache Settings

All uploaded objects use the following cache settings:

```python
CacheControl='public, max-age=31536000, immutable'
```

This instructs clients to cache the content indefinitely since CID-addressed content is immutable.

## Setting ETag for Clients

While R2 cannot store a custom ETag, you can override the ETag header in responses to clients using a Cloudflare Worker. This allows clients to see the CID in the ETag header:

### Cloudflare Worker Example

```javascript
// Example Worker snippet for serving R2 objects with CID as ETag
addEventListener('fetch', event => {
  event.respondWith(handle(event.request))
})

async function handle(request) {
  const url = new URL(request.url)
  const key = url.pathname.slice(1) // strip leading '/'
  
  // Fetch object from R2
  const object = await R2_BUCKET.get(key)
  if (!object) return new Response('Not Found', { status: 404 })
  
  // Get CID from metadata, or fall back to using the key itself
  const cid = object.metadata?.cid || key
  
  // Read the object body
  const body = await object.arrayBuffer()
  
  // Return response with CID as ETag
  return new Response(body, {
    headers: {
      'Content-Type': object.httpMetadata?.contentType || 'application/octet-stream',
      'Cache-Control': 'public, max-age=31536000, immutable',
      'ETag': `"${cid}"`  // Override ETag to show CID to clients
    }
  })
}
```

**Note:** This Worker example demonstrates overriding the ETag header returned to clients to match the CID. It does not change the underlying R2 ETag, which remains controlled by the storage service.

### Accessing Custom Metadata in Workers

In Cloudflare Workers, R2 object metadata is accessed via the `metadata` property:

```javascript
const object = await R2_BUCKET.get(key)
const cid = object.metadata?.cid  // Custom metadata we set during upload
```

## CI/CD Integration

To use this script in GitHub Actions:

```yaml
- name: Upload to R2
  env:
    AWS_ACCESS_KEY_ID: ${{ secrets.R2_ACCESS_KEY_ID }}
    AWS_SECRET_ACCESS_KEY: ${{ secrets.R2_SECRET_ACCESS_KEY }}
  run: |
    pip install boto3
    python .github/scripts/r2_upload.py examples/hello.txt \
      --bucket 256t-org \
      --endpoint-url https://${{ secrets.R2_ACCOUNT_ID }}.r2.cloudflarestorage.com
```

## Validation Guidelines

When validating CID objects:

1. **Prefer metadata verification** (fast, no download required)
2. **Use head_object()** to check `Metadata['cid']`
3. **Fall back to download** only if metadata is missing
4. **Never rely on ETag** for CID validation

## Error Handling

The script handles several error conditions:

- **Metadata mismatch**: Object exists but stored CID doesn't match computed CID (error)
- **Content mismatch**: Downloaded content's CID doesn't match expected CID (error)
- **Missing metadata**: Falls back to download verification (warning)
- **Object not found**: Proceeds with upload (normal)

## Troubleshooting

### "Metadata CID mismatch"

This indicates a serious integrity issue where an object exists with a CID key but the metadata doesn't match. This should never happen in normal operation.

### "Object is valid but missing metadata"

This indicates the object was uploaded without CID metadata (perhaps by an older version of the script). Consider re-uploading to add metadata for faster future verifications.

### Authentication Errors

Ensure your R2 credentials are properly configured:
- Check `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` environment variables
- Verify the credentials have read/write permissions for the R2 bucket

## Development

### Testing Locally

You can test uploads to a local S3-compatible server (like MinIO):

```bash
# Start MinIO locally
docker run -p 9000:9000 -p 9001:9001 \
  -e MINIO_ROOT_USER=minioadmin \
  -e MINIO_ROOT_PASSWORD=minioadmin \
  minio/minio server /data --console-address ":9001"

# Upload to MinIO
export AWS_ACCESS_KEY_ID=minioadmin
export AWS_SECRET_ACCESS_KEY=minioadmin
python .github/scripts/r2_upload.py examples/hello.txt \
  --bucket test-bucket \
  --endpoint-url http://localhost:9000
```

## References

- [256t.org Specification](../../README.md)
- [Cloudflare R2 Documentation](https://developers.cloudflare.com/r2/)
- [boto3 S3 Client Documentation](https://boto3.amazonaws.com/v1/documentation/api/latest/reference/services/s3.html)
- [RFC 4648 - Base64url Encoding](https://datatracker.ietf.org/doc/html/rfc4648#section-5)
