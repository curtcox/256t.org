# Deployment

This repository contains GitHub Actions workflows to automatically deploy the 256t.org site to GitHub Pages and Cloudflare R2.

## GitHub Pages Deployment

The site is automatically deployed to GitHub Pages when changes are pushed to the `main` branch. The workflow is defined in `.github/workflows/deploy-pages.yml`.

The site is available at: https://curtcox.github.io/256t.org/

## Cloudflare R2 Deployment (256t.org)

The site is automatically deployed to Cloudflare R2 when changes are pushed to the `main` branch. The workflow is defined in `.github/workflows/deploy-cloudflare.yml`.

The 256t.org domain is backed by a Cloudflare R2 bucket (`256t-cids`) that hosts both:
- **CID files**: Content-addressable storage files uploaded by the R2 upload workflow
- **Static site files**: Generated website files (HTML, CSS, JS, etc.)

Both types of files coexist in the same bucket without conflicts, as CIDs are 94-character base64url strings that don't overlap with static file names.

### Required Secrets

To enable R2 deployment, the following GitHub repository secrets must be configured:

1. **`CLOUDFLARE_API_TOKEN`**: A Cloudflare API token with R2 read/write permissions
   - Go to [Cloudflare Dashboard](https://dash.cloudflare.com/) → Profile → API Tokens
   - Create a new token with R2 read/write permissions

2. **`CLOUDFLARE_ACCOUNT_ID`**: Your Cloudflare Account ID
   - Found in the Cloudflare Dashboard URL: `https://dash.cloudflare.com/<ACCOUNT_ID>`
   - Or in Account Settings → Account ID

### Deployment Configuration

The R2 deployment:
- **Bucket Name**: `256t-cids`
- **Build Output Directory**: `dist/`
- **Cache Headers**: 
  - HTML/JSON files: `max-age=300` (5 minutes)
  - Other static files: `max-age=3600` (1 hour)
  - CID files: `max-age=31536000, immutable` (1 year, immutable)

### Manual Deployment

Both workflows can be triggered manually using the "workflow_dispatch" event from the Actions tab in GitHub.

## Build Process

Both deployments use the same build process:

1. Install Python dependencies (pip, markdown)
2. Convert `README.md` to HTML using Python's markdown library
3. Copy `hash.html` to the output directory
4. Copy resource directories to the output directory
5. Output is generated in the `dist/` directory

The generated site consists of:
- `index.html` - The main page (converted from README.md)
- `hash.html` - The hash calculator tool
- `implementations/` - All implementation code in various languages
- `examples/` - Example text files for testing
- `cids/` - Content Identifier files

## CloudFlare R2 CID Upload

CID files are uploaded to CloudFlare R2 for content-addressable storage. The workflow is defined in `.github/workflows/r2-upload.yml`.

This workflow uploads CID files to the same R2 bucket (`256t-cids`) that backs the 256t.org domain, where they coexist with the static site files.

### Schedule

The R2 upload workflow runs:
- **Daily** at 2:00 AM UTC
- **On-demand** via manual workflow dispatch

### Required Secrets

The R2 CID upload workflow uses the same secrets as the R2 site deployment:

1. **`CLOUDFLARE_ACCOUNT_ID`**: Your Cloudflare Account ID
2. **`CLOUDFLARE_API_TOKEN`**: A Cloudflare API token with R2 read/write permissions

### How It Works

The R2 upload script (`.github/scripts/r2_upload.py`):
1. Checks all files in the `/cids` directory
2. For each CID file:
   - Verifies the CID matches the content
   - Checks if it exists on R2
   - If exists: verifies content and cache headers match
   - If missing: uploads with immutable cache settings and verifies
3. Fails the job if any discrepancies are found

All CIDs are stored with cache headers indicating they are immutable:
- `Cache-Control: public, max-age=31536000, immutable`

See [.github/scripts/README.md](.github/scripts/README.md) for more details.
