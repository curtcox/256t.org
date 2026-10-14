// Runs only after the user selects the page adaptation action (activeTab).
globalThis.t256AdaptPage = async base => {
  const connection = new URL(base);
  if (connection.protocol !== 'http:' || connection.hostname !== '127.0.0.1' || connection.username || connection.password || connection.search || connection.hash || !/^\/s\/[A-Za-z0-9_-]+$/.test(connection.pathname)) throw new Error('Invalid helper connection URL');
  base = connection.href;
  const failures = new Set();
  const requests = new Map();
  const media = new Set();
  let adapted = 0;
  const retrieve = async value => {
    const response = await fetch(`${base}/resolve?uri=${encodeURIComponent(value)}`, { credentials: 'omit', referrerPolicy: 'no-referrer', signal: AbortSignal.timeout(20000) });
    if (!response.ok) {
      const details = await response.json().catch(() => ({}));
      const summary = typeof details?.error === 'string' ? details.error : `Provider access or retrieval failed (HTTP ${response.status})`;
      throw new Error(summary + (Array.isArray(details?.failures) ? details.failures.map(failure => `\n${failure.provider}: ${failure.reason}`).join('') : ''));
    }
    const result = await response.json();
    if (!/^t256:[A-Za-z0-9_-]+(?:\.[A-Za-z0-9]+)*$/i.test(result.uri)) throw new Error('Invalid resolved URI');
    const expected = value.replace(/^t256:/i, 't256:').replace(/\..*$/, suffix => suffix.toLowerCase());
    if (result.uri !== expected) throw new Error('Resolver returned a different identifier');
    return result;
  };
  const resolve = value => {
    if (!requests.has(value)) requests.set(value, retrieve(value));
    return requests.get(value);
  };
  const resource = async value => `${base}/resource/${(await resolve(value)).uri.slice(5)}`;
  const rewriteSrcset = async value => {
    let output = '', position = 0;
    const whitespace = ' \t\n\r\f';
    // HTML candidate boundaries allow commas inside URLs; only rewrite complete
    // URL tokens, never matching a CID inside a data URL or a descriptor.
    while (position < value.length) {
      let start = position;
      while (position < value.length && (whitespace + ',').includes(value[position])) position++;
      output += value.slice(start, position); start = position;
      while (position < value.length && !whitespace.includes(value[position])) position++;
      let end = position;
      while (end > start && value[end - 1] === ',') end--;
      const candidate = value.slice(start, end);
      try { output += /^t256:/i.test(candidate) ? await resource(candidate) : candidate; }
      catch (error) { failures.add(error.message); output += candidate; }
      output += value.slice(end, position);
      if (end !== position) continue;
      start = position; let depth = 0;
      while (position < value.length) {
        const char = value[position++];
        if (char === '(') depth++;
        else if (char === ')') depth = Math.max(0, depth - 1);
        else if (char === ',' && depth === 0) break;
      }
      output += value.slice(start, position);
    }
    return output;
  };
  for (const element of document.querySelectorAll('img[src],img[srcset],source[src],source[srcset],audio[src],video[src],video[poster],track[src],a[href],link[rel="stylesheet"][href]')) {
    const attributes = { A: ['href'], LINK: ['href'], VIDEO: ['src', 'poster'], IMG: ['src', 'srcset'], SOURCE: ['src', 'srcset'] }[element.tagName] ?? ['src'];
    for (const attribute of attributes) {
      const original = element.getAttribute(attribute);
      if (original === null) continue;
      const value = original.trim();
      if (attribute !== 'srcset' && !/^t256:/i.test(value)) continue;
      try {
        const replacement = attribute === 'srcset' ? await rewriteSrcset(original) : element.tagName === 'A' ? `${base}/view/${(await resolve(value)).uri.slice(5)}` : await resource(value);
        if (replacement === original || element.getAttribute(attribute) !== original) continue;
        if (element.tagName === 'A') element.rel = 'noopener noreferrer';
        else element.crossOrigin = 'anonymous';
        if (['SOURCE', 'TRACK'].includes(element.tagName)) {
          const parent = element.closest('audio,video'); if (parent) parent.crossOrigin = 'anonymous';
        }
        element.setAttribute(attribute, replacement); adapted++;
        if (attribute === 'src' && ['AUDIO', 'VIDEO', 'SOURCE'].includes(element.tagName)) {
          const parent = element.closest('audio,video'); if (parent) media.add(parent);
        }
      } catch (error) { failures.add(error.message); }
    }
  }
  for (const element of media) element.load();
  return failures.size ? { adapted, error: [...failures].join('\n') } : { adapted };
};
