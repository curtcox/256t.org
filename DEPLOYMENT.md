# Deployment

This repository contains GitHub Actions workflows to automatically deploy the 256t.org site to both GitHub Pages and Cloudflare Pages.

## GitHub Pages Deployment

The site is automatically deployed to GitHub Pages when changes are pushed to the `main` branch. The workflow is defined in `.github/workflows/deploy-pages.yml`.

The site is available at: https://curtcox.github.io/256t.org/

## Cloudflare Pages Deployment

The site is also automatically deployed to Cloudflare Pages when changes are pushed to the `main` branch. The workflow is defined in `.github/workflows/deploy-cloudflare.yml`.

### Required Secrets

To enable Cloudflare Pages deployment, the following GitHub repository secrets must be configured:

1. **`CLOUDFLARE_API_TOKEN`**: A Cloudflare API token with permissions to edit Cloudflare Pages
   - Go to [Cloudflare Dashboard](https://dash.cloudflare.com/) → Profile → API Tokens
   - Create a new token with the "Cloudflare Pages" template or with "Account.Cloudflare Pages" permissions

2. **`CLOUDFLARE_ACCOUNT_ID`**: Your Cloudflare Account ID
   - Found in the Cloudflare Dashboard URL: `https://dash.cloudflare.com/<ACCOUNT_ID>`
   - Or in Account Settings → Account ID

### Project Configuration

The Cloudflare Pages project is configured with:
- **Project Name**: `256t-org`
- **Build Output Directory**: `dist/`
- **Custom Domain**: `256t.org` (must be configured in Cloudflare Pages dashboard)

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

## CloudFlare R2 Storage

CID files are also uploaded to CloudFlare R2 for content-addressable storage. The workflow is defined in `.github/workflows/r2-upload.yml`.

### Schedule

The R2 upload workflow runs:
- **Daily** at 2:00 AM UTC
- **On-demand** via manual workflow dispatch

### Required Secrets

To enable R2 uploads, configure these GitHub repository secrets:

1. **`CLOUDFLARE_ACCOUNT_ID`**: Your Cloudflare Account ID (same as above)

2. **`R2_ACCESS_KEY_ID`**: R2 API access key ID
   - Go to [Cloudflare Dashboard](https://dash.cloudflare.com/) → R2 → Manage R2 API Tokens
   - Create a new API token with read/write permissions for your bucket

3. **`R2_SECRET_ACCESS_KEY`**: R2 API secret access key
   - Obtained when creating the R2 API token (only shown once)

4. **`R2_BUCKET_NAME`**: (Optional) R2 bucket name
   - Defaults to `256t-cids` if not specified

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
