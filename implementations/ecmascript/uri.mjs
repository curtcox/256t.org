import * as namespace from './cid.js';

const cidLibrary = namespace.default ?? globalThis.t256Cid;
export const computeCid = cidLibrary.computeCid;

export function parseUri(value) {
  const match = typeof value === 'string' && /^(t256):([A-Za-z0-9_-]{8,94})((?:\.[A-Za-z0-9]+)*)$/i.exec(value);
  if (!match) throw new TypeError('Expected t256:<CID>[.suffix], without //, escapes, query, or fragment');
  const [, , cid, rawSuffix] = match;
  const decode = (text) => {
    const data = Uint8Array.from(atob(text.replace(/-/g, '+').replace(/_/g, '/') + '='.repeat((4 - text.length % 4) % 4)), c => c.charCodeAt(0));
    if (cidLibrary.toBase64Url(data) !== text) throw new TypeError('Noncanonical base64url encoding');
    return data;
  };
  const length = decode(cid.slice(0, 8)).reduce((total, byte) => total * 256 + byte, 0);
  const payload = decode(cid.slice(8));
  if (payload.length !== (length <= 64 ? length : 64)) throw new TypeError('Invalid CID payload length');
  const suffix = rawSuffix.toLowerCase();
  return { cid, suffix, length, inline: length <= 64 ? payload : null, uri: `t256:${cid}${suffix}` };
}

export async function verifyUri(uri, bytes) {
  const parsed = typeof uri === 'string' ? parseUri(uri) : uri;
  if (await computeCid(bytes) !== parsed.cid) throw new Error('Content does not match its CID');
  return parsed;
}

export async function readVerified(response, uri, signal) {
  const parsed = typeof uri === 'string' ? parseUri(uri) : uri;
  const bytes = new Uint8Array(parsed.length);
  const reader = response.body.getReader();
  let offset = 0;
  try {
    while (true) {
      const { value, done } = await reader.read();
      if (done) break;
      if (signal?.aborted) throw signal.reason ?? new Error('Aborted');
      if (offset + value.length > bytes.length) throw new Error('Content exceeds CID length');
      bytes.set(value, offset); offset += value.length;
    }
    if (offset !== bytes.length) throw new Error('Content is shorter than its CID length');
    await verifyUri(parsed, bytes);
    return bytes;
  } finally { await reader.cancel(); }
}

export function mediaType(uri) {
  const parsed = typeof uri === 'string' ? parseUri(uri) : uri;
  const types = { txt: 'text/plain', html: 'text/html', htm: 'text/html', css: 'text/css', js: 'text/javascript', mjs: 'text/javascript', json: 'application/json', svg: 'image/svg+xml', png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', gif: 'image/gif', webp: 'image/webp', avif: 'image/avif', pdf: 'application/pdf', gz: 'application/gzip', zip: 'application/zip' };
  return types[parsed.suffix.split('.').at(-1)] ?? 'application/octet-stream';
}
