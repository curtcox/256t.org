# Unison Implementation

This directory contains the Unison implementation for computing 256t.org Content Identifiers (CIDs).

## Files

- `cid.u` - Core CID computation library
- `check.u` - Verification script to check CID files
- `README.md` - This file

## About Unison

Unison is a modern, statically-typed functional programming language with a unique approach to code storage and distribution. Learn more at [unison-lang.org](https://www.unison-lang.org/).

## Usage

### Prerequisites

Install Unison CLI Manager (UCM):
```bash
# See https://www.unison-lang.org/docs/usage-topics/install/
```

### Running the CID Check

```bash
# From the repository root
cd implementations/unison
ucm run check.u
```

## Implementation Details

The Unison implementation follows the 256t.org specification:

1. **Length Encoding**: The content length is encoded as an 8-character base64url string (6 bytes, big-endian)
2. **Content Handling**:
   - Content ≤ 64 bytes: The content itself is base64url-encoded
   - Content > 64 bytes: SHA-512 hash of the content is base64url-encoded
3. **Base64URL**: Uses URL-safe base64 encoding (RFC 4648 section 5) without padding

## Notes

- Unison uses a content-addressed codebase, making it particularly well-suited for content-addressed storage systems like 256t.org
- The implementation leverages Unison's type system and functional programming paradigms
- File I/O and cryptographic operations use Unison's standard library functions
