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
  signal?.throwIfAborted();
  const parsed = typeof uri === 'string' ? parseUri(uri) : uri;
  const bytes = new Uint8Array(parsed.length);
  const reader = response.body.getReader();
  // Cancelling the reader also wakes a stalled read, even for streams that do
  // not themselves listen to fetch's signal. Cleanup must not mask CID errors.
  const cancel = () => { reader.cancel(signal?.reason).catch(() => {}); };
  signal?.addEventListener('abort', cancel, { once: true });
  let offset = 0;
  try {
    while (true) {
      const { value, done } = await reader.read();
      signal?.throwIfAborted();
      if (done) break;
      if (offset + value.length > bytes.length) throw new Error('Content exceeds CID length');
      bytes.set(value, offset); offset += value.length;
    }
    if (offset !== bytes.length) throw new Error('Content is shorter than its CID length');
    await verifyUri(parsed, bytes);
    signal?.throwIfAborted();
    return bytes;
  } finally {
    signal?.removeEventListener('abort', cancel);
    cancel(); reader.releaseLock();
  }
}

export function mediaType(uri) {
  const parsed = typeof uri === 'string' ? parseUri(uri) : uri;
  const types = { txt: 'text/plain', html: 'text/html', htm: 'text/html', css: 'text/css', js: 'text/javascript', mjs: 'text/javascript', json: 'application/json', svg: 'image/svg+xml', png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', gif: 'image/gif', webp: 'image/webp', avif: 'image/avif', ico: 'image/x-icon', pdf: 'application/pdf', gz: 'application/gzip', zip: 'application/zip', mp3: 'audio/mpeg', wav: 'audio/wav', mp4: 'video/mp4', webm: 'video/webm', vtt: 'text/vtt', woff: 'font/woff', woff2: 'font/woff2', wasm: 'application/wasm' };
  return types[parsed.suffix.split('.').at(-1)] ?? 'application/octet-stream';
}
