import { parseUri, readVerified, mediaType } from '../../implementations/ecmascript/uri.mjs';

/** No-install fallback for inline content and public HTTP(S) providers with CORS. */
export class T256WebClient {
  constructor(providers = ['https://256t.us'], { maxBytes = 64 * 1024 * 1024, timeout = 20000 } = {}) {
    this.providers = providers.map(value => {
      const url = new URL(value);
      if (!['http:', 'https:'].includes(url.protocol) || url.username || url.password || url.search || url.hash) throw new TypeError('Use HTTP(S) base URLs without credentials, queries, or fragments');
      return url.href.replace(/\/$/, '');
    });
    this.maxBytes = maxBytes; this.timeout = timeout;
  }

  async read(value, { signal } = {}) {
    const parsed = parseUri(value);
    if (parsed.length > this.maxBytes) throw new RangeError('Content exceeds the page size limit');
    const result = { parsed, type: mediaType(parsed), failures: [] };
    if (parsed.inline !== null) return { ...result, bytes: parsed.inline, provider: 'inline' };
    for (const provider of this.providers) {
      if (signal?.aborted) throw signal.reason ?? new Error('Aborted');
      const controller = new AbortController();
      const abort = () => controller.abort(signal?.reason);
      signal?.addEventListener('abort', abort, { once: true });
      const timer = setTimeout(() => controller.abort(new Error('Provider timed out')), this.timeout);
      try {
        const response = await fetch(`${provider}/${parsed.cid}`, { signal: controller.signal, credentials: 'omit', referrerPolicy: 'no-referrer', redirect: 'error' });
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return { ...result, bytes: await readVerified(response, parsed, signal), provider };
      } catch (error) {
        if (signal?.aborted) throw signal.reason ?? error;
        result.failures.push({ provider, reason: error.message });
      } finally {
        clearTimeout(timer); signal?.removeEventListener('abort', abort);
      }
    }
    throw new Error('No provider returned matching content. Check availability and CORS. ' + result.failures.map(value => value.reason).join('; '));
  }
}
