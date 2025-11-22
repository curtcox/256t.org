// CID computation for Elm implementation
// This JavaScript module provides the CID computation logic for use with Elm
// It follows the 256t.org specification

import { createHash } from 'node:crypto';
import { readFileSync, readdirSync } from 'node:fs';
import { resolve, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export const BASE_DIR = resolve(__dirname, '..', '..');
export const EXAMPLES_DIR = join(BASE_DIR, 'examples');
export const CIDS_DIR = join(BASE_DIR, 'cids');

/**
 * Convert buffer to base64url encoding (URL-safe, no padding)
 */
const toBase64Url = (buffer) =>
  buffer
    .toString('base64')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/g, '');

/**
 * Encode the content length as an 8-character base64url string
 */
export const encodeLength = (length) => {
  const bytes = Buffer.alloc(6);
  bytes.writeUIntBE(length, 0, 6);
  return toBase64Url(bytes);
};

/**
 * Compute CID from content
 * For content <= 64 bytes, uses the content itself (base64url encoded)
 * For content > 64 bytes, uses SHA-512 hash (base64url encoded)
 */
export const computeCid = (content) => {
  const prefix = encodeLength(content.length);
  const suffix =
    content.length <= 64
      ? toBase64Url(content)
      : toBase64Url(createHash('sha512').update(content).digest());
  return `${prefix}${suffix}`;
};

/**
 * Get all CID files from the cids directory
 */
export const getCidFiles = () => {
  return readdirSync(CIDS_DIR).filter((file) => {
    const fullPath = join(CIDS_DIR, file);
    try {
      const stats = readFileSync(fullPath);
      return true;
    } catch {
      return false;
    }
  });
};

/**
 * Read content from a CID file
 */
export const readCidFile = (cid) => {
  return readFileSync(join(CIDS_DIR, cid));
};
