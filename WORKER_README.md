# Cloudflare Worker for 256t.org

This directory contains the Cloudflare Worker that handles routing for 256t.org.

## Purpose

The worker solves the issue where accessing `https://256t.org/` returns a 404 error. It sits in front of the R2 bucket and:
- Serves `index.html` when accessing the root path (`/`)
- Serves `index.html` for directory paths (e.g., `/dir/`)
- Proxies all other requests directly to the R2 bucket

## Files

- **`worker.js`**: The worker code that handles routing logic
- **`wrangler.toml`**: Configuration file for Wrangler CLI

## Deployment

The worker is automatically deployed via GitHub Actions when changes are pushed to the `main` branch. See `.github/workflows/deploy-worker.yml`.

### Manual Deployment

To deploy manually:

```bash
# Install Wrangler CLI
npm install -g wrangler

# Authenticate with Cloudflare
wrangler login

# Set your account ID
export CLOUDFLARE_ACCOUNT_ID=your-account-id

# Deploy the worker
wrangler deploy
```

### Route Configuration

After deploying the worker, you need to configure it to handle traffic for the 256t.org domain:

1. Go to [Cloudflare Dashboard](https://dash.cloudflare.com/)
2. Navigate to Workers & Pages → Overview
3. Find the `256t-org-site` worker
4. Click "Add Route"
5. Add the route: `256t.org/*` (or `*256t.org/*` to include www subdomain)
6. Select the zone: `256t.org`

Alternatively, use Wrangler CLI:

```bash
# Add route via CLI (requires zone ID)
wrangler route add "256t.org/*" --zone-id <ZONE_ID>
```

## Testing

To test the worker locally:

```bash
# Start local development server
wrangler dev

# In another terminal, test the root path
curl http://localhost:8787/

# Should return the content of index.html
```

## How It Works

1. When a request comes in, the worker checks the path
2. If the path is `/` or empty, it rewrites to `/index.html`
3. If the path ends with `/`, it appends `index.html`
4. The worker then fetches the object from the R2 bucket
5. If found, it returns the object with appropriate headers
6. If not found, it returns a 404 error

## Environment Bindings

The worker requires the following bindings (configured in `wrangler.toml`):

- **`BUCKET`**: R2 bucket binding to the `256t-cids` bucket

## Security

- The worker adds CORS headers to allow cross-origin requests
- All R2 objects inherit their cache headers as set during upload
- The worker preserves ETags for proper caching behavior
