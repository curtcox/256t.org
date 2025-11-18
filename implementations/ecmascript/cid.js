// ECMAScript implementation for browser environments
// Uses Web Crypto API for SHA-512 hashing

const CHUNK_SIZE = 0x8000;

/**
 * Convert a Uint8Array to Base64URL encoding
 * @param {Uint8Array} bytes - The bytes to encode
 * @returns {string} Base64URL encoded string
 */
function toBase64Url(bytes) {
  if (!bytes.length) {
    return '';
  }
  let binary = '';
  for (let i = 0; i < bytes.length; i += CHUNK_SIZE) {
    const chunk = bytes.subarray(i, i + CHUNK_SIZE);
    binary += String.fromCharCode(...chunk);
  }
  return btoa(binary)
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/, '');
}

/**
 * Encode a length value as an 8-character Base64URL string
 * @param {number} length - The length to encode
 * @returns {string} 8-character Base64URL string
 */
function encodeLength(length) {
  let remaining = Math.max(0, Math.floor(length));
  const bytes = new Uint8Array(6);
  for (let i = 5; i >= 0; i -= 1) {
    bytes[i] = remaining % 256;
    remaining = Math.floor(remaining / 256);
  }
  return toBase64Url(bytes);
}

/**
 * Compute the CID for given content
 * @param {Uint8Array|string} content - The content to hash
 * @returns {Promise<string>} The CID (94 characters or less)
 */
async function computeCid(content) {
  // Convert string to Uint8Array if needed
  const data = typeof content === 'string' 
    ? new TextEncoder().encode(content)
    : content;
  
  const prefix = encodeLength(data.length);
  
  let suffix;
  if (data.length <= 64) {
    suffix = toBase64Url(data);
  } else {
    const hashBuffer = await crypto.subtle.digest('SHA-512', data);
    const hashBytes = new Uint8Array(hashBuffer);
    suffix = toBase64Url(hashBytes);
  }
  
  return `${prefix}${suffix}`;
}

// Export for ES modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { toBase64Url, encodeLength, computeCid };
}
