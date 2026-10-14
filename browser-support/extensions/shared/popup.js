const api = globalThis.browser ?? chrome;
const status = document.querySelector('#status');
const message = async value => {
  const result = await api.runtime.sendMessage(value);
  if (result?.error) throw new Error(result.error);
  return result;
};
async function action(work, needsHelper = true) {
  status.textContent = 'Working…';
  try { const result = await work(); status.textContent = typeof result === 'string' ? result : 'Ready.'; }
  catch (error) { status.textContent = error.message + (needsHelper ? '\nInstall the native host or copy a website connection URL from the running helper.' : ''); }
}
document.querySelector('#open').addEventListener('click', () => action(async () => {
  const uri = globalThis.t256URI.parseUri(document.querySelector('#uri').value.trim()).uri;
  const base = document.querySelector('#base').value.trim();
  if (base) {
    const url = new URL(base);
    if (url.protocol !== 'http:' || url.hostname !== '127.0.0.1' || url.username || url.password || !/^\/s\/[A-Za-z0-9_-]+$/.test(url.pathname) || url.search || url.hash) throw new Error('Invalid helper connection URL');
    await api.tabs.create({ url: `${base}/view/${uri.slice(5)}` });
  } else await message({ action: 'open', uri });
}));
document.querySelector('#settings').addEventListener('click', () => action(() => message({ action: 'settings' })));
document.querySelector('#browser-open').addEventListener('click', () => action(async () => {
  const uri = document.querySelector('#uri').value.trim();
  const viewer = document.querySelector('#viewer').value.trim();
  await api.tabs.create({ url: globalThis.t256ViewerUrl(viewer, uri) });
}, false));
document.querySelector('#adapt-links').addEventListener('click', () => action(async () => {
  const [tab] = await api.tabs.query({ active: true, currentWindow: true });
  if (!tab?.url || !/^https?:/.test(tab.url)) throw new Error('Use this on an HTTP(S) page');
  const viewer = document.querySelector('#viewer').value.trim();
  globalThis.t256ViewerUrl(viewer, 't256:AAAAAAAA');
  await api.scripting.executeScript({ target: { tabId: tab.id }, files: ['uri.js', 'navigation.js', 'content.js'] });
  const results = await api.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256AdaptLinks(value), args: [viewer] });
  const result = results[0]?.result;
  if (result?.error) throw new Error(`Adapted ${result.adapted} links.\n${result.error}`);
  return `Adapted ${result?.adapted ?? 0} links. No provider access needed.`;
}, false));
document.querySelector('#adapt').addEventListener('click', () => action(async () => {
  const [tab] = await api.tabs.query({ active: true, currentWindow: true });
  if (!tab?.url || !/^https?:/.test(tab.url)) throw new Error('Use this on an HTTP(S) page');
  const origin = new URL(tab.url).origin;
  let base = document.querySelector('#base').value.trim();
  if (!base) base = (await message({ action: 'connection', origin })).base;
  const url = new URL(base);
  if (url.protocol !== 'http:' || url.hostname !== '127.0.0.1' || !/^\/s\/[A-Za-z0-9_-]+$/.test(url.pathname)) throw new Error('Invalid helper connection URL');
  await api.scripting.executeScript({ target: { tabId: tab.id }, files: ['content.js'] });
  const results = await api.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256AdaptPage(value), args: [base] });
  const result = results[0]?.result;
  if (result?.error) throw new Error(`Adapted ${result.adapted} resource references.\n${result.error}`);
  return `Adapted ${result?.adapted ?? 0} resource references.`;
}));
if (!api.runtime.getManifest().permissions.includes('nativeMessaging')) {
  document.querySelector('#settings').disabled = true;
  status.textContent = 'Use the browser viewer for public/inline content, or copy a website connection URL for local/private content. Native launch requires containing app integration in Safari.';
}
