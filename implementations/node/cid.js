import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync, readdirSync, mkdirSync } from 'node:fs';
import { resolve, join } from 'node:path';

export const BASE_DIR = resolve(new URL('.', import.meta.url).pathname, '..', '..');
export const EXAMPLES_DIR = join(BASE_DIR, 'examples');
export const CIDS_DIR = join(BASE_DIR, 'cids');

const toBase64Url = (buffer) =>
  buffer
    .toString('base64')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/g, '');

export const encodeLength = (length) => {
  const bytes = Buffer.alloc(6);
  bytes.writeUIntBE(length, 0, 6);
  return toBase64Url(bytes);
};

export const computeCid = (content) => {
  const prefix = encodeLength(content.length);
  const suffix =
    content.length <= 64
      ? toBase64Url(content)
      : toBase64Url(createHash('sha512').update(content).digest());
  return `${prefix}${suffix}`;
};
