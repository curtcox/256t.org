# R2 Upload Script

## Overview

The `r2_upload.py` script manages the upload and verification of CID files to CloudFlare R2 storage. It ensures that all CID files in the repository are properly stored in R2 with appropriate immutable cache settings.

## Features

- **Automated Upload**: Checks all files in the `/cids` directory and uploads missing ones to R2
- **Content Verification**: Verifies that existing CIDs on R2 match their local content
- **CID Validation**: Computes and verifies CIDs for both local and remote content
- **Cache Header Validation**: Ensures uploaded files have proper immutable cache headers
- **Error Reporting**: Fails the job if any discrepancies are found

## GitHub Actions Workflow

The workflow is defined in `.github/workflows/r2-upload.yml` and runs:
- **Daily**: Automatically at 2 AM UTC
- **On-Demand**: Via manual workflow dispatch

## Required Secrets

Configure the following secrets in your GitHub repository (Settings → Secrets and variables → Actions):

### `CLOUDFLARE_ACCOUNT_ID`
Your CloudFlare Account ID. Find this in:
- CloudFlare Dashboard URL: `https://dash.cloudflare.com/<ACCOUNT_ID>`
- Or in Account Settings → Account ID

### `R2_ACCESS_KEY_ID`
The access key ID for R2 API access. Create this in:
- CloudFlare Dashboard → R2 → Manage R2 API Tokens
- Click "Create API Token"
- Set permissions for the specific bucket

### `R2_SECRET_ACCESS_KEY`
The secret access key corresponding to the R2 access key ID (obtained when creating the API token).

### `R2_BUCKET_NAME` (optional)
The name of the R2 bucket to use. Defaults to `256t-cids` if not specified.

## How It Works

1. **Connect to R2**: Establishes a connection to CloudFlare R2 using the S3-compatible API
2. **Process Each CID**:
   - Reads the file from `/cids` directory
   - Computes the CID to verify the filename matches the content
   - Checks if the CID exists on R2
   - If exists:
     - Downloads the content from R2
     - Verifies content matches local file
     - Verifies computed CID matches filename
     - Checks cache headers (public, max-age=31536000, immutable)
   - If missing:
     - Uploads the file with immutable cache settings
     - Downloads and verifies the uploaded content
     - Verifies computed CID matches filename
     - Checks cache headers
3. **Report Results**: 
   - Prints summary of verified, uploaded, and any errors
   - Exits with code 1 if any errors found, 0 if all successful

## Cache Settings

All CIDs are uploaded with the following cache settings:
- `Cache-Control: public, max-age=31536000, immutable`
- `Content-Type: application/octet-stream`

These settings reflect the immutable nature of content-addressable storage - content identified by a CID should never change.

## Local Testing

To test the script locally:

```bash
# Install dependencies
pip install boto3

# Set environment variables
export CLOUDFLARE_ACCOUNT_ID="your-account-id"
export R2_ACCESS_KEY_ID="your-access-key-id"
export R2_SECRET_ACCESS_KEY="your-secret-access-key"
export R2_BUCKET_NAME="256t-cids"  # optional, defaults to this

# Run the script
python .github/scripts/r2_upload.py
```

## Error Handling

The script will fail if:
- Any CID filename doesn't match its computed CID
- Content on R2 doesn't match local content
- Downloaded content's CID doesn't match the filename
- Cache headers are missing or incorrect
- Upload verification fails
- Any network or API errors occur

## Dependencies

- Python 3.12+
- boto3 (AWS SDK for Python, compatible with S3-compatible APIs like R2)
