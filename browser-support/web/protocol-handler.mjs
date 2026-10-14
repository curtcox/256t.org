import { parseUri } from '../../implementations/ecmascript/uri.mjs';

// Browser compatibility alias only; the RFC parser remains t256-only.
export const browserScheme = 'web+tcontent';

export function parseBrowserUri(value) {
  if (typeof value !== 'string') throw new TypeError('Expected a t256 URI or browser-compatible link');
  return parseUri(value.replace(/^web\+tcontent:/i, 't256:'));
}

export function parseHandlerFragment(hash) {
  if (!hash || hash === '#') return null;
  const values = new URLSearchParams(hash.replace(/^#/, '')).getAll('uri');
  if (values.length !== 1) throw new TypeError('Expected one URI in the resolver link');
  // URLSearchParams decodes the handler transport once. URI escapes are invalid.
  return parseBrowserUri(values[0]);
}

export function handlerTemplate(pageUrl) {
  const url = new URL(pageUrl);
  const loopback = ['localhost', '127.0.0.1', '[::1]'].includes(url.hostname);
  if (url.username || url.password || !(url.protocol === 'https:' || url.protocol === 'http:' && loopback)) {
    throw new TypeError('Browser links need HTTPS or a local desktop helper page');
  }
  url.search = ''; url.hash = 'uri=%s';
  return url.href;
}
