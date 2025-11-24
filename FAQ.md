# Frequently Asked Questions

## General Questions

### What is 256t.org?

256t.org is a public specification for content-addressable storage using 94-character content identifiers (CIDs). It defines a simple, standardized way to identify and retrieve content based on its cryptographic hash and length.

### Why use 256t.org instead of other content-addressable storage systems?

256t.org provides a simple, predictable format with fixed-length identifiers (94 characters or less) that are easy to work with in URLs, filenames, and databases. The specification is intentionally minimal and focused, making it straightforward to implement in any programming language.

### How is 256t.org different from IPFS or other content-addressable systems?

While IPFS and similar systems are complete distributed storage networks, 256t.org is just a specification for content identifiers. It defines how to compute and verify CIDs, but leaves storage, retrieval, and discovery mechanisms as implementation details. This makes it simpler and more flexible for different use cases.

## CID Format

### What is a CID?

A CID (Content Identifier) is a 94-character or less base64url-encoded string that uniquely identifies content. It consists of:
- 8 characters: length of content (base64url encoded)
- Up to 86 characters: SHA-512 hash (for content > 64 bytes) OR the content itself (for content ≤ 64 bytes)

### Why 94 characters?

The length was chosen to fit comfortably within URL path segments and common database field sizes:
- 8 characters for length (can represent up to 256 terabytes)
- 86 characters for SHA-512 hash or inline content
- Total: 94 characters

### What is base64url encoding?

Base64url is a variant of Base64 encoding that uses URL and filename safe characters (defined in [RFC 4648 section 5](https://datatracker.ietf.org/doc/html/rfc4648#section-5)). It replaces `+` with `-` and `/` with `_`, and omits padding characters (`=`).

### Why is small content stored inline instead of hashed?

For content 64 bytes or smaller, the base64url-encoded content itself is shorter than or equal to the SHA-512 hash. Storing it inline is more efficient and avoids unnecessary hashing.

### How do I compute a CID?

1. Determine the content length
2. If length ≤ 64 bytes:
   - Base64url encode the content
   - Prepend with base64url-encoded length (8 characters)
3. If length > 64 bytes:
   - Compute SHA-512 hash of the content
   - Base64url encode the hash (86 characters)
   - Prepend with base64url-encoded length (8 characters)

See the [implementations](https://github.com/curtcox/256t.org/tree/main/implementations) directory for code examples in 40+ languages.

## Usage

### Can I use 256t.org for my project?

Yes! The specification is open and free to use. You can implement CID generation and verification in your application, or use one of the existing implementations.

### Where can I store CIDs?

You can store CIDs anywhere:
- In a traditional database
- On a filesystem (as filenames or in a dedicated directory)
- In cloud storage (S3, R2, etc.)
- On a CDN
- In a distributed system

The specification doesn't dictate storage mechanisms.

### How do I retrieve content from a CID?

That depends on your storage implementation. The specification defines the CID format but not the retrieval mechanism. Common approaches include:
- HTTP servers with CID-based routing
- Direct file system access
- Cloud storage APIs
- Custom protocols

### Are there public servers hosting CIDs?

The main 256t.org domain hosts a small demonstration set of CIDs, but it's not intended as a general-purpose public storage service. The focus is on the specification itself.

## Security and Integrity

### Can CIDs collide?

SHA-512 collisions are cryptographically infeasible with current technology. For content > 64 bytes, CIDs are as collision-resistant as SHA-512 itself. For content ≤ 64 bytes, CIDs are unique by definition since they contain the actual content.

### What if someone creates a hash collision?

While theoretically possible, engineering a SHA-512 collision is far beyond current computational capabilities. It's easier for an attacker to simply lie about content than to create a collision. Always verify CID content if security is critical.

### How do I verify content matches a CID?

Use any of the [implementations](https://github.com/curtcox/256t.org/tree/main/implementations) to compute the CID of your content and compare it to the expected CID. All implementations provide verification functions.

### Are CIDs immutable?

Yes, in practice. Since a CID is derived from the content itself, the same CID will always represent the same content. If content changes, its CID changes too.

### Can CIDs be cached indefinitely?

Yes. Since CIDs are content-addressed and immutable, they can be cached forever with `Cache-Control: public, max-age=31536000, immutable` headers.

## Implementation

### Which implementation should I use?

Choose the implementation that matches your project's primary language. All implementations follow the same specification and produce identical CIDs for the same content.

### How do I add a new implementation?

Each implementation should include:
- A core library for CID computation (`cid.*`)
- A script to generate CIDs for example files (`generate.*`)
- A script to verify CIDs match their content (`check.*`)

See existing implementations in the [implementations](https://github.com/curtcox/256t.org/tree/main/implementations) directory for examples.

### Are the implementations tested?

Yes. The repository includes example files in the `examples/` directory and expected CIDs in `cids.json`. Implementations are tested by generating CIDs for these files and verifying they match the expected values.

## Technical Details

### Why SHA-512 instead of SHA-256?

SHA-512 provides a higher collision resistance and the additional length (64 bytes vs 32 bytes) doesn't significantly impact the total CID size thanks to inline storage for small content.

### What happens with content exactly 64 bytes long?

Content of exactly 64 bytes can be stored inline (base64url encodes to ≤ 86 characters) or hashed. The specification allows both, though inline storage is preferred for efficiency.

### Why is the length prefix 8 characters?

8 characters of base64url encoding provides 48 bits of length information, supporting content up to 256 terabytes. This is sufficient for virtually all use cases while keeping the prefix compact.

### Can I use shorter CIDs for small content?

Yes! For content ≤ 64 bytes, the CID is shorter because it only includes the length prefix plus the base64url-encoded content itself (which is ≤ 86 characters).

## Publishing and Storage

### How do I publish CIDs?

See [Publishing and Storage](publishing.html) for detailed information about deploying content-addressed storage using CIDs, including examples of uploading to Cloudflare R2.

### What metadata should accompany CID content?

The specification focuses on content identifiers, not metadata. However, best practices include:
- `Cache-Control: public, max-age=31536000, immutable` for maximum caching
- `Content-Type` headers appropriate to the content
- Optional: Store the CID in response headers or metadata for verification

### Can I host CIDs on GitHub Pages?

Yes, but GitHub Pages has file size limitations and isn't optimized for content-addressable storage. For production use, consider dedicated storage services like S3, R2, or similar.

## Contributing

### How can I contribute to 256t.org?

Contributions are welcome! You can:
- Add implementations in new programming languages
- Improve existing implementations
- Report issues or suggest improvements to the specification
- Improve documentation

See the [GitHub repository](https://github.com/curtcox/256t.org) to get started.

### Where can I ask questions?

Open an issue on the [GitHub repository](https://github.com/curtcox/256t.org/issues) or start a discussion in the GitHub Discussions section.

---

[← Back to Home](index.html)
