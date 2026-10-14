import { parseUri, verifyUri } from '../../implementations/ecmascript/uri.mjs';

/** A page opts in to provider access using the connection URL copied from the helper. */
export class T256Client {
  constructor(base, { timeout = 20000, maxBytes = 64 * 1024 * 1024 } = {}) {
    const url = new URL(base);
    if (url.protocol !== 'http:' || url.hostname !== '127.0.0.1' || !/^\/s\/[A-Za-z0-9_-]+$/.test(url.pathname) || url.search || url.hash) {
      throw new TypeError('Use the loopback connection URL supplied by the desktop helper');
    }
    this.base = url.href.replace(/\/$/, '');
    this.timeout = timeout;
    this.maxBytes = maxBytes;
    this.objectUrls = new Set();
    this.hydrated = new WeakMap();
  }

  async request(url, signal, consume) {
    const controller = new AbortController();
    const abort = () => controller.abort(signal?.reason);
    if (signal?.aborted) abort();
    signal?.addEventListener('abort', abort, { once: true });
    const timer = setTimeout(() => controller.abort(new Error('t256 retrieval timed out')), this.timeout);
    try {
      const response = await fetch(url, { signal: controller.signal, credentials: 'omit', referrerPolicy: 'no-referrer' });
      if (!response.ok) throw new Error(`t256 resolver returned HTTP ${response.status}`);
      return await consume(response);
    } finally {
      clearTimeout(timer);
      signal?.removeEventListener('abort', abort);
    }
  }

  async resolve(value, { signal } = {}) {
    const uri = parseUri(value);
    if (uri.length > this.maxBytes) throw new RangeError('Content exceeds the page size limit');
    const result = await this.request(`${this.base}/resolve?uri=${encodeURIComponent(uri.uri)}`, signal, response => response.json());
    if (result.uri !== uri.uri || result.length !== uri.length) throw new Error('Resolver returned a different identifier');
    // Construct resource URLs ourselves; a helper response cannot redirect credentials or scripts.
    return { ...result, url: `${this.base}/resource/${uri.cid}${uri.suffix}`, parsed: uri };
  }

  async bytes(value, options = {}) {
    const uri = parseUri(value);
    if (uri.length > this.maxBytes) throw new RangeError('Content exceeds the page size limit');
    if (uri.inline !== null) return uri.inline;
    return this.request(`${this.base}/raw/${uri.cid}${uri.suffix}`, options.signal, async response => {
      const reader = response.body.getReader();
      const bytes = new Uint8Array(uri.length);
      let offset = 0;
      try {
        while (true) {
          const { value: chunk, done } = await reader.read();
          if (done) break;
          if (options.signal?.aborted) throw options.signal.reason ?? new Error('Aborted');
          if (offset + chunk.length > bytes.length) throw new Error('Content exceeds CID length');
          bytes.set(chunk, offset); offset += chunk.length;
        }
        if (offset !== bytes.length) throw new Error('Content is shorter than its CID length');
        await verifyUri(uri, bytes);
        return bytes;
      } finally {
        await reader.cancel();
      }
    });
  }

  async blobUrl(value, type, options = {}) {
    const bytes = await this.bytes(value, options);
    const url = URL.createObjectURL(new Blob([bytes], { type }));
    this.objectUrls.add(url);
    return url;
  }

  async image(element, value, options = {}) {
    const result = await this.resolve(value, options);
    if (!result.type.startsWith('image/')) throw new TypeError('An image URI needs a recognized image suffix');
    const url = await this.blobUrl(value, result.type, options);
    await new Promise((resolve, reject) => {
      element.addEventListener('load', resolve, { once: true });
      element.addEventListener('error', () => reject(new Error('Image could not be decoded')), { once: true });
      element.src = url;
    });
    return element;
  }

  async script(value, { module = false, signal, nonce } = {}) {
    const result = await this.resolve(value, { signal });
    if (result.type !== 'text/javascript') throw new TypeError('A script URI needs a .js or .mjs suffix');
    const script = document.createElement('script');
    if (nonce) script.nonce = nonce;
    if (module) {
      script.type = 'module';
      script.crossOrigin = 'anonymous';
      script.src = result.url;
    } else {
      script.async = false;
      script.src = await this.blobUrl(value, result.type, { signal });
    }
    await new Promise((resolve, reject) => {
      script.addEventListener('load', resolve, { once: true });
      script.addEventListener('error', () => reject(new Error('Script failed to load; check page CSP and module mappings')), { once: true });
      document.head.append(script);
    });
    return script;
  }

  async worker(value, { signal, name } = {}) {
    const result = await this.resolve(value, { signal });
    if (result.type !== 'text/javascript') throw new TypeError('A worker URI needs a JavaScript suffix');
    return new Worker(await this.blobUrl(value, result.type, { signal }), { type: 'classic', name });
  }

  /** Call before loading any modules. Supply the full graph, including cycles. */
  async imports(mapping, { nonce, signal } = {}) {
    const entries = await Promise.all(Object.entries(mapping).map(async ([specifier, uri]) => [specifier, (await this.resolve(uri, { signal })).url]));
    const element = document.createElement('script');
    element.type = 'importmap';
    if (nonce) element.nonce = nonce;
    element.textContent = JSON.stringify({ imports: Object.fromEntries(entries) });
    document.head.append(element);
    return element;
  }

  /** Inert data attributes avoid races with the browser's HTML parser. */
  async hydrate(root = document, { nonce, signal } = {}) {
    const select = selector => [...(root.matches?.(selector) ? [root] : []), ...root.querySelectorAll(selector)]
      .filter(element => this.hydrated.get(element) !== (element.dataset.t256Src ?? element.dataset.t256Href));
    await Promise.all(select('img[data-t256-src]').map(async image => {
      const uri = image.dataset.t256Src;
      await this.image(image, uri, { signal }); this.hydrated.set(image, uri);
    }));
    for (const link of select('link[data-t256-href]')) {
      if (link.rel !== 'stylesheet') throw new TypeError('Only stylesheet links are supported');
      link.crossOrigin = 'anonymous';
      const uri = link.dataset.t256Href;
      link.href = (await this.resolve(uri, { signal })).url;
      this.hydrated.set(link, uri);
    }
    for (const placeholder of select('script[type="application/t256"][data-t256-src]')) {
      const uri = placeholder.dataset.t256Src;
      await this.script(uri, { module: placeholder.dataset.t256Module === 'true', nonce, signal });
      this.hydrated.set(placeholder, uri);
    }
  }

  /** Opt-in dynamic declarations; arbitrary native t256 attributes are not intercepted. */
  observe(root = document, { onError = error => console.error(error), ...options } = {}) {
    let queue = Promise.resolve();
    const observer = new MutationObserver(() => {
      queue = queue.then(() => this.hydrate(root, options)).catch(onError);
    });
    observer.observe(root, { subtree: true, childList: true, attributes: true, attributeFilter: ['data-t256-src', 'data-t256-href'] });
    options.signal?.addEventListener('abort', () => observer.disconnect(), { once: true });
    return observer;
  }

  dispose() {
    for (const url of this.objectUrls) URL.revokeObjectURL(url);
    this.objectUrls.clear();
  }
}
