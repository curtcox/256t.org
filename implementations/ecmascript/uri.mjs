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
