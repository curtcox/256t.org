# Cloudflare API Token Setup

This guide provides complete instructions for creating a Cloudflare API token with the permissions required for the 256t.org deployment workflows.

## Overview

The `CLOUDFLARE_API_TOKEN` GitHub secret is used by three workflows:
- **deploy-cloudflare.yml**: Uploads static site files to R2
- **deploy-worker.yml**: Deploys the Cloudflare Worker
- **r2-upload.yml**: Uploads CID files to R2

## Step-by-Step Instructions

### Step 1: Access the Cloudflare Dashboard

1. Go to [https://dash.cloudflare.com/](https://dash.cloudflare.com/)
2. Log in with your Cloudflare account credentials

### Step 2: Navigate to API Tokens

1. Click on your **profile icon** in the top-right corner
2. Select **My Profile** from the dropdown menu
3. In the left sidebar, click on **API Tokens**
4. Click the **Create Token** button

### Step 3: Create a Custom Token

1. Scroll down to the **Custom token** section
2. Click **Get started** next to "Create Custom Token"

### Step 4: Configure Token Settings

#### Token Name
Enter a descriptive name for the token:
```
256t.org Deployment Token
```

#### Permissions

Add the following permissions by clicking **+ Add more** after each one:

| Permission Category | Permission | Access Level |
|---------------------|------------|--------------|
| **Account** → Cloudflare Pages | Edit | Required for Workers deployment |
| **Account** → Workers R2 Storage | Edit | Required for R2 read/write operations |
| **Account** → Workers Scripts | Edit | Required for deploying Workers |
| **Zone** → Zone | Read | Required for zone configuration |
| **Zone** → Workers Routes | Edit | Required for configuring Worker routes |

##### Detailed Permission Setup:

1. **First Permission Row:**
   - Select **Account** from the first dropdown
   - Select **Cloudflare Pages** from the second dropdown
   - Select **Edit** from the third dropdown

2. **Second Permission Row (click + Add more):**
   - Select **Account** from the first dropdown
   - Select **Workers R2 Storage** from the second dropdown
   - Select **Edit** from the third dropdown

3. **Third Permission Row (click + Add more):**
   - Select **Account** from the first dropdown
   - Select **Workers Scripts** from the second dropdown
   - Select **Edit** from the third dropdown

4. **Fourth Permission Row (click + Add more):**
   - Select **Zone** from the first dropdown
   - Select **Zone** from the second dropdown
   - Select **Read** from the third dropdown

5. **Fifth Permission Row (click + Add more):**
   - Select **Zone** from the first dropdown
   - Select **Workers Routes** from the second dropdown
   - Select **Edit** from the third dropdown

#### Account Resources

Configure which accounts the token can access:

1. Select **Include** from the first dropdown
2. Select your account name (or **All accounts** if you only have one)

#### Zone Resources

Configure which zones (domains) the token can access:

1. Select **Include** from the first dropdown
2. Select **Specific zone** from the second dropdown
3. Select **256t.org** from the third dropdown

Alternatively, you can select **All zones** if you prefer broader access.

#### Client IP Address Filtering (Optional)

You can optionally restrict the token to specific IP addresses for additional security. For GitHub Actions, you would need to allow GitHub's IP ranges, which change frequently, so this is typically left empty.

#### TTL (Optional)

You can optionally set an expiration date for the token. If left empty, the token will not expire automatically.

### Step 5: Create and Copy the Token

1. Click **Continue to summary**
2. Review the permissions summary to ensure all required permissions are listed
3. Click **Create Token**
4. **Important**: Copy the token value immediately. This is the only time you will see it.

The token will look something like:
```
Wv1234567890abcdefghijklmnopqrstuvwxyzABCDE
```

### Step 6: Add the Token to GitHub Secrets

1. Go to your GitHub repository: [https://github.com/curtcox/256t.org](https://github.com/curtcox/256t.org)
2. Click **Settings** (requires repository admin access)
3. In the left sidebar, expand **Secrets and variables**
4. Click **Actions**
5. Click **New repository secret** (or **Update secret** if updating an existing token)
6. Enter the following:
   - **Name**: `CLOUDFLARE_API_TOKEN`
   - **Secret**: Paste the token you copied from Cloudflare
7. Click **Add secret**

## Verifying the Token

To verify the token is working correctly:

1. Go to the **Actions** tab in your GitHub repository
2. Select the **Deploy Worker** workflow
3. Click **Run workflow** to trigger a manual deployment
4. Check the workflow logs for successful completion

If the token is missing permissions, you'll see error messages like:
- `Authentication error` - Token is invalid or expired
- `You do not have permission to access this resource` - Missing required permissions

## Token Expiration and Renewal

When a token expires:

1. Create a new token following the steps above
2. Update the `CLOUDFLARE_API_TOKEN` secret in GitHub
3. The new token will be used for all subsequent workflow runs

## Troubleshooting

### Common Issues

| Error | Cause | Solution |
|-------|-------|----------|
| `Authentication error` | Token is invalid or expired | Create a new token |
| `Missing R2 permissions` | Token lacks R2 Storage access | Add "Workers R2 Storage: Edit" permission |
| `Failed to deploy worker` | Token lacks Workers access | Add "Workers Scripts: Edit" permission |
| `Route configuration failed` | Token lacks zone access | Add "Zone: Read" and "Workers Routes: Edit" permissions |

### Checking Token Permissions

To review an existing token's permissions:

1. Go to [Cloudflare Dashboard](https://dash.cloudflare.com/) → Profile → API Tokens
2. Find your token in the list
3. Click the **...** menu and select **Edit**
4. Review the permissions listed

Note: You cannot view the token value again, but you can see and modify its permissions.

## Security Best Practices

1. **Use minimal permissions**: Only grant the permissions listed above
2. **Use zone-specific access**: Restrict to the 256t.org zone rather than all zones
3. **Rotate tokens periodically**: Create new tokens and revoke old ones regularly
4. **Never commit tokens**: Always use GitHub Secrets, never hardcode tokens in code
5. **Set expiration dates**: Consider setting TTL for automatic expiration

## Related Documentation

- [DEPLOYMENT.md](DEPLOYMENT.md) - Overall deployment documentation
- [WORKER_README.md](WORKER_README.md) - Cloudflare Worker setup details
- [Cloudflare API Token Documentation](https://developers.cloudflare.com/fundamentals/api/get-started/create-token/)
