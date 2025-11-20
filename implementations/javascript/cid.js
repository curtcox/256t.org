const crypto = require('crypto');
const https = require('https');
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

const downloadCid = (baseUrl, cid) => {
  return new Promise((resolve, reject) => {
    const url = `${baseUrl.replace(/\/$/, '')}/${cid}`;
    https.get(url, (res) => {
      if (res.statusCode !== 200) {
        reject(new Error(`HTTP ${res.statusCode}: ${res.statusMessage}`));
        return;
      }
      const chunks = [];
      res.on('data', (chunk) => chunks.push(chunk));
      res.on('end', () => {
        const content = Buffer.concat(chunks);
        const computed = computeCid(content);
        const isValid = computed === cid;
        resolve({ content, computed, isValid });
      });
    }).on('error', reject);
  });
};

module.exports = {
  BASE_DIR,
  EXAMPLES_DIR,
  CIDS_DIR,
  computeCid,
  downloadCid,
};
