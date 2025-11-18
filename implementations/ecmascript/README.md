# ECMAScript Implementation

This is a browser-native ECMAScript implementation of the 256t.org CID specification.

## Features

- **Browser-compatible**: Uses Web Crypto API for SHA-512 hashing
- **No dependencies**: Pure ECMAScript with no external libraries
- **Dual environment**: Core `cid.js` works in both browsers and Node.js

## Files

- `cid.js` - Core CID computation functions (browser-compatible)
- `check.js` - Verification script for CI (uses Node.js APIs)
- `check.html` - Browser-based verification page
- `generate.js` - Generation script for CI (uses Node.js APIs)

## Usage

### In the Browser

```html
<script src="implementations/ecmascript/cid.js"></script>
<script>
  // Compute CID for a string
  computeCid("Hello, World!").then(cid => {
    console.log(cid); // AAAAAAANSGVsbG8sIFdvcmxkIQ
  });
  
  // Compute CID for binary data
  const data = new Uint8Array([1, 2, 3, 4]);
  computeCid(data).then(cid => {
    console.log(cid);
  });
  
  // Use utility functions
  const encoded = toBase64Url(new Uint8Array([1, 2, 3]));
  const lengthPrefix = encodeLength(1024);
</script>
```

### In Node.js

```javascript
// Set up Web Crypto API polyfill for Node.js
const { webcrypto } = require('crypto');
global.crypto = webcrypto;
global.TextEncoder = TextEncoder;

const { computeCid } = require('./cid.js');

// Use async/await
async function main() {
  const cid = await computeCid("Hello, World!");
  console.log(cid);
}
main();
```

## Running Tests

### In the Browser

Open `check.html` in a web browser to run the verification tests.

### In Node.js

```bash
node implementations/ecmascript/check.js
```

## API

### `computeCid(content)`

Computes the CID for the given content.

- **Parameters**: 
  - `content` (Uint8Array|string): The content to hash
- **Returns**: Promise<string> - The CID (94 characters or less)

### `toBase64Url(bytes)`

Converts a Uint8Array to Base64URL encoding.

- **Parameters**: 
  - `bytes` (Uint8Array): The bytes to encode
- **Returns**: string - Base64URL encoded string

### `encodeLength(length)`

Encodes a length value as an 8-character Base64URL string.

- **Parameters**: 
  - `length` (number): The length to encode
- **Returns**: string - 8-character Base64URL string
