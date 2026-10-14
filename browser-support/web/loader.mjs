import { parseUri, readVerified } from '../../implementations/ecmascript/uri.mjs';

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
    this.hydrationQueue = Promise.resolve();
    this.observers = new Set();
    this.lifetime = new AbortController();
    this.imageUrls = new WeakMap();
  }

  check(signal) {
    this.lifetime.signal.throwIfAborted();
    signal?.throwIfAborted();
  }

  async request(url, signal, consume) {
    this.check(signal);
    const controller = new AbortController();
    const abort = () => controller.abort(signal?.reason);
    const disposed = () => controller.abort(this.lifetime.signal.reason);
    signal?.addEventListener('abort', abort, { once: true });
    this.lifetime.signal.addEventListener('abort', disposed, { once: true });
    const timer = setTimeout(() => controller.abort(new Error('t256 retrieval timed out')), this.timeout);
    try {
      const response = await fetch(url, { signal: controller.signal, credentials: 'omit', referrerPolicy: 'no-referrer' });
      if (!response.ok) throw new Error(`t256 resolver returned HTTP ${response.status}`);
      return await consume(response, controller.signal);
    } finally {
      clearTimeout(timer);
      signal?.removeEventListener('abort', abort);
      this.lifetime.signal.removeEventListener('abort', disposed);
    }
  }

  /** Include decoding/execution in the timeout and always remove paired listeners. */
  async loaded(element, start, signal, message) {
    this.check(signal);
    return new Promise((resolve, reject) => {
      const cleanup = () => {
        clearTimeout(timer);
        element.removeEventListener('load', success);
        element.removeEventListener('error', failure);
        signal?.removeEventListener('abort', aborted);
        this.lifetime.signal.removeEventListener('abort', disposed);
      };
      const success = () => { cleanup(); resolve(element); };
      const fail = error => { cleanup(); reject(error); };
      const failure = () => fail(new Error(message));
      const aborted = () => fail(signal.reason);
      const disposed = () => fail(this.lifetime.signal.reason);
      const timer = setTimeout(() => fail(new Error('t256 resource loading timed out')), this.timeout);
      element.addEventListener('load', success, { once: true });
      element.addEventListener('error', failure, { once: true });
      signal?.addEventListener('abort', aborted, { once: true });
      this.lifetime.signal.addEventListener('abort', disposed, { once: true });
      try { start(); } catch (error) { fail(error); }
    });
  }

  release(url) {
    if (this.objectUrls.delete(url)) URL.revokeObjectURL(url);
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
    this.check(options.signal);
    const uri = parseUri(value);
    if (uri.length > this.maxBytes) throw new RangeError('Content exceeds the page size limit');
    if (uri.inline !== null) return uri.inline;
    return this.request(`${this.base}/raw/${uri.cid}${uri.suffix}`, options.signal, (response, signal) => readVerified(response, uri, signal));
  }

  async blobUrl(value, type, options = {}) {
    const bytes = await this.bytes(value, options);
    this.check(options.signal);
    const url = URL.createObjectURL(new Blob([bytes], { type }));
    this.objectUrls.add(url);
    return url;
  }

  async image(element, value, options = {}) {
    const result = await this.resolve(value, options);
    if (!result.type.startsWith('image/')) throw new TypeError('An image URI needs a recognized image suffix');
    const url = await this.blobUrl(value, result.type, options);
    try {
      await this.loaded(element, () => { element.src = url; }, options.signal, 'Image could not be decoded');
    } catch (error) {
      if (element.src === url) element.removeAttribute('src');
      this.release(url); throw error;
    }
    this.release(this.imageUrls.get(element));
    this.imageUrls.set(element, url);
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
    try {
      await this.loaded(script, () => document.head.append(script), signal, 'Script failed to load; check page CSP and module mappings');
    } catch (error) { script.remove(); throw error; }
    finally { if (!module) this.release(script.src); }
    return script;
  }

  async worker(value, { signal, name } = {}) {
    const result = await this.resolve(value, { signal });
    if (result.type !== 'text/javascript') throw new TypeError('A worker URI needs a JavaScript suffix');
    const url = await this.blobUrl(value, result.type, { signal });
    this.check(signal);
    return new Worker(url, { type: 'classic', name });
  }

  /** Call before loading any modules. Supply the full graph, including cycles. */
  async imports(mapping, { nonce, signal } = {}) {
    const entries = await Promise.all(Object.entries(mapping).map(async ([specifier, uri]) => [specifier, (await this.resolve(uri, { signal })).url]));
    this.check(signal);
    const element = document.createElement('script');
    element.type = 'importmap';
    if (nonce) element.nonce = nonce;
    element.textContent = JSON.stringify({ imports: Object.fromEntries(entries) });
    document.head.append(element);
    return element;
  }

  /** Inert data attributes avoid races with the browser's HTML parser. */
  hydrate(root = document, options = {}) {
    // One client can receive observer callbacks and explicit calls concurrently.
    // Serialize scans so an unchanged declaration cannot execute twice.
    const work = this.hydrationQueue.then(() => this.hydrateNow(root, options));
    this.hydrationQueue = work.catch(() => {});
    return work;
  }

  async hydrateNow(root, { nonce, signal } = {}) {
    this.check(signal);
    const select = selector => [...(root.matches?.(selector) ? [root] : []), ...root.querySelectorAll(selector)]
      .filter(element => this.hydrated.get(element) !== (element.dataset.t256Src ?? element.dataset.t256Href));
    await Promise.all(select('img[data-t256-src]').map(async image => {
      const uri = image.dataset.t256Src;
      await this.image(image, uri, { signal }); this.hydrated.set(image, uri);
    }));
    for (const link of select('link[data-t256-href]')) {
      if (link.rel !== 'stylesheet') throw new TypeError('Only stylesheet links are supported');
      link.crossOrigin = 'anonymous';
      if (nonce) link.nonce = nonce;
      const uri = link.dataset.t256Href;
      const url = (await this.resolve(uri, { signal })).url;
      await this.loaded(link, () => { link.href = url; }, signal, 'Stylesheet failed to load; check page CSP');
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
    this.check(options.signal);
    let queue = Promise.resolve();
    let active = true;
    const observer = new MutationObserver(() => {
      queue = queue.then(() => { if (active) return this.hydrate(root, options); })
        .catch(error => { if (active) onError(error); });
    });
    observer.observe(root, { subtree: true, childList: true, attributes: true, attributeFilter: ['data-t256-src', 'data-t256-href'] });
    const disconnect = observer.disconnect.bind(observer);
    const stop = () => { active = false; disconnect(); this.observers.delete(observer); options.signal?.removeEventListener('abort', stop); };
    observer.disconnect = stop;
    this.observers.add(observer);
    options.signal?.addEventListener('abort', stop, { once: true });
    return observer;
  }

  dispose() {
    this.lifetime.abort(new DOMException('t256 client was disposed', 'AbortError'));
    for (const observer of this.observers) observer.disconnect();
    for (const url of this.objectUrls) URL.revokeObjectURL(url);
    this.objectUrls.clear();
  }
}
