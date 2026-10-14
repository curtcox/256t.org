// Opt-in, current-document link adaptation. Nothing is fetched or persisted.
globalThis.t256StopLinks = () => {
  const state = globalThis.t256LinkWatch;
  if (!state) return false;
  state.stop(); globalThis.t256LinkWatch = undefined;
  return true;
};
globalThis.t256WatchLinks = destination => {
  globalThis.t256ViewerUrl(destination, 't256:AAAAAAAA');
  globalThis.t256StopLinks();
  const roots = new Set();
  let cursor, timer, stopped = false, adapted = 0, invalid = 0, lastError;
  const summary = () => ({ watching: !stopped, viewer: destination, adapted, invalid, ...(lastError ? { error: lastError } : {}) });
  const schedule = () => { if (!stopped && timer === undefined) timer = setTimeout(process, 0); };
  const enqueue = node => {
    if (!node.isConnected || ![1, 9].includes(node.nodeType) || roots.has(document)) return;
    roots.add(node);
    // Collapse a burst of additions to one scan instead of retaining many roots.
    if (roots.size > 256) { roots.clear(); roots.add(document); }
    schedule();
  };
  function process() {
    timer = undefined;
    // Yield between bounded batches, including on pages with large new subtrees.
    for (let budget = 250; budget > 0 && !stopped; budget--) {
      if (!cursor) {
        const root = roots.values().next().value;
        if (!root) break;
        roots.delete(root);
        if (!root.isConnected) continue;
        cursor = { walker: document.createTreeWalker(root, NodeFilter.SHOW_ELEMENT), node: root };
      }
      if (!cursor.walker.root.isConnected) { cursor = undefined; continue; }
      const node = cursor.node;
      if (node.isConnected && node.tagName === 'A') {
        try { if (globalThis.t256AdaptAnchor(node, destination)) adapted++; }
        catch (error) { invalid++; lastError = error.message; }
      }
      cursor.node = cursor.walker.nextNode();
      if (!cursor.node) cursor = undefined;
    }
    if (!stopped && (cursor || roots.size)) schedule();
  }
  const observer = new MutationObserver(records => {
    if (records.length > 256) { enqueue(document); return; }
    for (const record of records) {
      if (record.type === 'attributes') {
        // Ignore our own rewritten links and later ordinary HTTP(S) page edits.
        if (/^t256:/i.test(record.target.getAttribute('href')?.trim() ?? '')) enqueue(record.target);
      } else if (record.addedNodes.length > 256) enqueue(record.target);
      else for (const node of record.addedNodes) {
        enqueue(node);
        if (roots.has(document)) return;
      }
    }
  });
  const stop = () => {
    stopped = true; observer.disconnect(); clearTimeout(timer);
    roots.clear(); cursor = undefined;
    window.removeEventListener('pagehide', leave);
  };
  const leave = () => globalThis.t256StopLinks();
  const state = globalThis.t256LinkWatch = { summary, stop };
  observer.observe(document, { subtree: true, childList: true, attributes: true, attributeFilter: ['href'] });
  window.addEventListener('pagehide', leave);
  enqueue(document);
  return state.summary();
};
