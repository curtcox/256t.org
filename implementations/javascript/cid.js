const crypto = require('crypto');
const path = require('path');

const BASE_DIR = path.resolve(__dirname, '..', '..');
const EXAMPLES_DIR = path.join(BASE_DIR, 'examples');
const CIDS_DIR = path.join(BASE_DIR, 'cids');

const toBase64Url = (buffer) =>
  buffer
    .toString('base64')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/g, '');

const encodeLength = (length) => {
  const bytes = Buffer.alloc(6);
  bytes.writeUIntBE(length, 0, 6);
  return toBase64Url(bytes);
};

const computeCid = (content) => {
  const prefix = encodeLength(content.length);
  const suffix =
    content.length <= 64
      ? toBase64Url(content)
      : toBase64Url(crypto.createHash('sha512').update(content).digest());
  return `${prefix}${suffix}`;
};

module.exports = {
  BASE_DIR,
  EXAMPLES_DIR,
  CIDS_DIR,
  computeCid,
};
