# GitHub Copilot Instructions for 256t.org

## Project Overview

This repository contains the 256t.org specification and implementations - a public specification for content addressable storage using 94-character content identifiers (CIDs).

### What is 256t.org?

256t.org is a content addressable storage scheme where:
- Content is identified by a 94-character (or less) base64url-encoded string called a CID
- The CID consists of:
  - 8 characters: length of content (base64url encoded)
  - 86 characters: SHA-512 hash (for content > 64 bytes) OR the content itself (for content ≤ 64 bytes)
- Content can be safely cached indefinitely as CIDs are immutable

## Repository Structure

- **Root directory**: Contains HTML tools (hash.html, check.html, examples.html) and documentation
- **`implementations/`**: Contains CID implementations in 40+ programming languages
- **`examples/`**: Example files for testing CID generation
- **`cids/`**: Directory containing actual content identifier files
- **`.github/workflows/`**: CI/CD pipelines for deployment to GitHub Pages and Cloudflare Pages

## Coding Guidelines

### When Adding New Language Implementations

Each language implementation should follow this structure:
- **`cid.*`**: Core library with CID computation logic
- **`generate.*`**: Script to generate CIDs for example files
- **`check.*`**: Script to verify CIDs match their content

### CID Computation Requirements

All implementations must:
1. Use filename and URL safe Base64 encoding (base64url, RFC 4648 section 5)
2. Encode length as 8-character base64url string (6 bytes)
3. For content ≤ 64 bytes: use the content itself (base64url encoded)
4. For content > 64 bytes: use SHA-512 hash (base64url encoded)
5. Strip padding from base64url output

### Testing

- Each implementation should be able to process files from the `examples/` directory
- Generated CIDs should match those in `cids.json`
- Use the workflow in `.github/workflows/cid-checks.yml` as reference for CI testing

### HTML/JavaScript Tools

When modifying HTML tools:
- Keep them self-contained (minimal external dependencies)
- Use ECMAScript implementation from `implementations/ecmascript/` for client-side CID computation
- Ensure tools work offline where possible

## Deployment

- Changes to main branch automatically deploy to:
  - GitHub Pages: https://curtcox.github.io/256t.org/
  - Cloudflare Pages: 256t.org (requires configured secrets)
- Build process converts README.md to index.html
- See `DEPLOYMENT.md` for detailed deployment information

## Code Style

- Follow the existing code style and conventions for each language
- Keep implementations simple and readable
- Include comments explaining the CID computation logic
- Maintain consistency with other implementations in the repository

## Documentation

- Keep README.md as the primary specification document
- Update DEPLOYMENT.md if changing CI/CD workflows
- Document any new tools or utilities in README.md

## Best Practices

- Test new implementations against existing CIDs in `cids.json`
- Ensure base64url encoding is correctly implemented (no padding, URL-safe characters)
- Verify SHA-512 hash computation matches other implementations
- Keep implementations independent and self-contained
- Add language badges to README.md when adding new implementations
