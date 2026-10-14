// Runs only after the user selects "Adapt images and links" (activeTab).
globalThis.t256AdaptPage = async base => {
  const failures = [];
  let adapted = 0;
  const resolve = async value => {
    const response = await fetch(`${base}/resolve?uri=${encodeURIComponent(value)}`, { credentials: 'omit', referrerPolicy: 'no-referrer', signal: AbortSignal.timeout(20000) });
    if (!response.ok) throw new Error(`Provider access or retrieval failed (HTTP ${response.status})`);
    const result = await response.json();
    if (!/^t256:[A-Za-z0-9_-]+(?:\.[A-Za-z0-9]+)*$/i.test(result.uri)) throw new Error('Invalid resolved URI');
    const expected = value.replace(/^t256:/i, 't256:').replace(/\..*$/, suffix => suffix.toLowerCase());
    if (result.uri !== expected) throw new Error('Resolver returned a different identifier');
    return result;
  };
  for (const element of document.querySelectorAll('img[src],a[href],link[rel="stylesheet"][href],video[poster]')) {
    const attribute = element.tagName === 'A' || element.tagName === 'LINK' ? 'href' : element.tagName === 'VIDEO' ? 'poster' : 'src';
    const value = element.getAttribute(attribute);
    if (!/^t256:/i.test(value)) continue;
    try {
      const result = await resolve(value);
      if (element.tagName === 'A') {
        element.setAttribute(attribute, `${base}/view/${result.uri.slice(5)}`);
        element.rel = 'noopener noreferrer';
      } else {
        element.crossOrigin = 'anonymous';
        element.setAttribute(attribute, `${base}/resource/${result.uri.slice(5)}`);
      }
      adapted++;
    } catch (error) { failures.push(error.message); }
  }
  return failures.length ? { adapted, error: failures.join('\n') } : { adapted };
};
