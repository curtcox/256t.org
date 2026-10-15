const api = globalThis.browser ?? chrome;
const status = document.querySelector('#status');
// Extension text comes from _locales; messages relayed from the helper and
// page scripts are English and use the shared catalog in locales/.
const msg = (key, english, ...values) => api.i18n?.getMessage?.(key, values.map(String)) || english;
document.documentElement.lang = msg('languageTag', 'en');
for (const element of document.querySelectorAll('[data-i18n]')) element.textContent = msg(element.dataset.i18n, element.textContent);
for (const element of document.querySelectorAll('[data-i18n-placeholder]')) element.placeholder = msg(element.dataset.i18nPlaceholder, element.placeholder);
const english = { message: value => value };
const helperMessages = import('./i18n.mjs').then(module => module.translator(document.documentElement.lang)).catch(() => english);
const message = async value => {
  const result = await api.runtime.sendMessage(value);
  if (result?.error) throw new Error(result.error);
  return result;
};
async function action(work, needsHelper = true) {
  status.textContent = msg('working', 'Working…');
  try { const result = await work(); status.textContent = typeof result === 'string' ? result : msg('ready', 'Ready.'); }
  catch (error) {
    const t = await helperMessages;
    status.textContent = t.message(error.message) + (needsHelper ? '\n' + msg('installHint', 'Install the native host or copy a website connection URL from the running helper.') : '');
  }
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
  if (result?.error) throw new Error(`${msg('adaptedLinks', `Adapted ${result.adapted} links.`, result.adapted)}\n${result.error}`);
  return msg('adaptedLinksNoAccess', `Adapted ${result?.adapted ?? 0} links. No provider access needed.`, result?.adapted ?? 0);
}, false));
async function watchTab() {
  const [tab] = await api.tabs.query({ active: true, currentWindow: true });
  if (!tab?.url || !/^https?:/.test(tab.url)) throw new Error('Use this on an HTTP(S) page');
  return tab;
}
async function refreshWatch() {
  try {
    const tab = await watchTab();
    const results = await api.scripting.executeScript({ target: { tabId: tab.id }, func: () => globalThis.t256LinkWatch?.summary() ?? { watching: false } });
    const result = results[0]?.result;
    document.querySelector('#stop-links').disabled = !result?.watching;
    if (result?.watching) {
      document.querySelector('#viewer').value = result.viewer;
      const t = await helperMessages;
      status.textContent = msg('watchingSummary', `Adapting new links on this page. Adapted ${result.adapted} links.`, result.adapted) + (result.error ? `\n${t.message(result.error)}` : '');
    }
  } catch { document.querySelector('#stop-links').disabled = true; }
}
document.querySelector('#watch-links').addEventListener('click', () => action(async () => {
  const tab = await watchTab();
  const viewer = document.querySelector('#viewer').value.trim();
  globalThis.t256ViewerUrl(viewer, 't256:AAAAAAAA');
  await api.scripting.executeScript({ target: { tabId: tab.id }, files: ['uri.js', 'navigation.js', 'content.js', 'watch-links.js'] });
  await api.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256WatchLinks(value), args: [viewer] });
  document.querySelector('#stop-links').disabled = false;
  return msg('watchStarted', 'Adapting new and changed links on this page. No provider access needed.');
}, false));
document.querySelector('#stop-links').addEventListener('click', () => action(async () => {
  const tab = await watchTab();
  await api.scripting.executeScript({ target: { tabId: tab.id }, func: () => globalThis.t256StopLinks?.() ?? false });
  document.querySelector('#stop-links').disabled = true;
  return msg('watchStopped', 'Stopped adapting new links. Existing adapted links stay in place.');
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
  if (result?.error) throw new Error(`${msg('adaptedResources', `Adapted ${result.adapted} resource references.`, result.adapted)}\n${result.error}`);
  return msg('adaptedResources', `Adapted ${result?.adapted ?? 0} resource references.`, result?.adapted ?? 0);
}));
if (!api.runtime.getManifest().permissions.includes('nativeMessaging')) {
  document.querySelector('#settings').disabled = true;
  status.textContent = msg('safariHint', 'Use the browser viewer for public/inline content, or copy a website connection URL for local/private content. Native launch requires containing app integration in Safari.');
}
refreshWatch();
