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

/**
 * Download content from a URL and validate its CID
 * @param {string} baseUrl - Base URL to download from
 * @param {string} cid - Expected content identifier
 * @returns {Promise<{content: Uint8Array, computed: string, isValid: boolean}>}
 */
async function downloadCid(baseUrl, cid) {
  const url = `${baseUrl.replace(/\/$/, '')}/${cid}`;
  
  // Get fetch function - check global first (Node.js), then globalThis, then global scope
  const fetchFn = (typeof global !== 'undefined' && global.fetch) 
    ? global.fetch.bind(global)
    : (typeof globalThis !== 'undefined' && globalThis.fetch)
    ? globalThis.fetch.bind(globalThis)
    : (typeof fetch !== 'undefined')
    ? fetch
    : null;
    
  if (!fetchFn) {
    throw new Error('fetch is not available');
  }
  
  const response = await fetchFn(url);
  
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
  }
  
  const arrayBuffer = await response.arrayBuffer();
  const content = new Uint8Array(arrayBuffer);
  const computed = await computeCid(content);
  const isValid = computed === cid;
  
  return { content, computed, isValid };
}

/**
 * Convert a Base64URL string to Uint8Array
 * @param {string} base64url - The Base64URL encoded string
 * @returns {Uint8Array} Decoded bytes
 */
function fromBase64Url(base64url) {
  // Add padding if necessary
  let base64 = base64url.replace(/-/g, '+').replace(/_/g, '/');
  while (base64.length % 4) {
    base64 += '=';
  }
  const binary = atob(base64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}

/**
 * Decode the length from a CID's 8-character prefix
 * @param {string} cid - The full CID string
 * @returns {number} The decoded length
 */
function decodeLength(cid) {
  if (!cid || cid.length < 8) {
    return 0;
  }
  const prefix = cid.substring(0, 8);
  const bytes = fromBase64Url(prefix);
  let length = 0;
  for (let i = 0; i < bytes.length; i++) {
    length = length * 256 + bytes[i];
  }
  return length;
}

/**
 * Check if a CID contains a literal value (content ≤ 64 bytes)
 * @param {string} cid - The CID to check
 * @returns {boolean} True if the CID contains literal content
 */
function isLiteralCid(cid) {
  const length = decodeLength(cid);
  return length <= 64;
}

/**
 * Extract the literal content from a CID (only valid for literal CIDs)
 * @param {string} cid - The CID containing literal content
 * @returns {Uint8Array|null} The literal content bytes, or null if not a literal CID
 */
function extractLiteralContent(cid) {
  if (!isLiteralCid(cid)) {
    return null;
  }
  const suffix = cid.substring(8);
  if (!suffix) {
    return new Uint8Array(0);
  }
  return fromBase64Url(suffix);
}

/**
 * Convert bytes to a hex dump string
 * @param {Uint8Array} bytes - The bytes to convert
 * @returns {string} Hex dump string with offset, hex values, and ASCII representation
 */
function toHexDump(bytes) {
  if (!bytes || !bytes.length) {
    return '';
  }
  
  const lines = [];
  const bytesPerLine = 16;
  
  for (let offset = 0; offset < bytes.length; offset += bytesPerLine) {
    const chunk = bytes.slice(offset, offset + bytesPerLine);
    
    // Offset
    const offsetStr = offset.toString(16).padStart(8, '0');
    
    // Hex values
    const hexParts = [];
    for (let i = 0; i < bytesPerLine; i++) {
      if (i < chunk.length) {
        hexParts.push(chunk[i].toString(16).padStart(2, '0'));
      } else {
        hexParts.push('  ');
      }
    }
    const hexStr = hexParts.slice(0, 8).join(' ') + '  ' + hexParts.slice(8).join(' ');
    
    // ASCII representation
    let asciiStr = '';
    for (let i = 0; i < chunk.length; i++) {
      const byte = chunk[i];
      if (byte >= 32 && byte <= 126) {
        asciiStr += String.fromCharCode(byte);
      } else {
        asciiStr += '.';
      }
    }
    
    lines.push(`${offsetStr}  ${hexStr}  |${asciiStr}|`);
  }
  
  return lines.join('\n');
}

// Export for ES modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { toBase64Url, fromBase64Url, encodeLength, decodeLength, computeCid, downloadCid, isLiteralCid, extractLiteralContent, toHexDump };
}
