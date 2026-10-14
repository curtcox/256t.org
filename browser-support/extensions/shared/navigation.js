// Shared by the popup and its isolated content script; no retrieval happens here.
globalThis.t256ViewerUrl = (destination, value) => {
  const url = new URL(destination);
  const loopback = ['localhost', '127.0.0.1', '[::1]'].includes(url.hostname);
  if (url.username || url.password || url.search || url.hash || !(url.protocol === 'https:' || url.protocol === 'http:' && loopback)) {
    throw new Error('Use an HTTPS viewer URL or a local resolver page, without credentials, query or fragment');
  }
  const uri = globalThis.t256URI.parseUri(value).uri;
  url.hash = `uri=${encodeURIComponent(uri)}`;
  return url.href;
};
