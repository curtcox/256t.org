const api = globalThis.browser ?? chrome;
const status = document.querySelector('#status');
const message = async value => {
  const result = await api.runtime.sendMessage(value);
  if (result?.error) throw new Error(result.error);
  return result;
};
async function action(work) {
  status.textContent = 'Working…';
  try { await work(); status.textContent = 'Ready.'; }
  catch (error) { status.textContent = error.message + '\nInstall the native host or copy a website connection URL from the running helper.'; }
}
document.querySelector('#open').addEventListener('click', () => action(async () => {
  const uri = document.querySelector('#uri').value.trim();
  const base = document.querySelector('#base').value.trim();
  if (base) {
    const url = new URL(base);
    if (url.protocol !== 'http:' || url.hostname !== '127.0.0.1' || !/^\/s\/[A-Za-z0-9_-]+$/.test(url.pathname) || url.search || url.hash) throw new Error('Invalid helper connection URL');
    if (!/^t256:[A-Za-z0-9_-]+(?:\.[A-Za-z0-9]+)*$/i.test(uri)) throw new Error('Invalid t256 URI');
    await api.tabs.create({ url: `${base}/view/${uri.slice(5)}` });
  } else await message({ action: 'open', uri });
}));
document.querySelector('#settings').addEventListener('click', () => action(() => message({ action: 'settings' })));
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
  if (results[0]?.result?.error) throw new Error(results[0].result.error);
}));
if (!api.runtime.getManifest().permissions.includes('nativeMessaging')) {
  document.querySelector('#settings').disabled = true;
  status.textContent = 'Copy a website connection URL from the helper. Native launch buttons require a containing app integration in Safari.';
}
