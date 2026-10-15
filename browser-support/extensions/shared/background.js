if (typeof importScripts === 'function') importScripts('uri.js');
const api = globalThis.browser ?? chrome;
const msg = (key, english) => api.i18n?.getMessage?.(key) || english;
const native = async message => {
  const result = await api.runtime.sendNativeMessage('org.t256.helper', message);
  if (result?.error) throw new Error(result.error);
  return result;
};
async function openNative(message) {
  const result = await native(message);
  const url = new URL(result.url);
  if (url.protocol !== 'http:' || url.hostname !== '127.0.0.1') throw new Error('Invalid helper address');
  await api.tabs.create({ url: result.url });
}
api.runtime.onInstalled.addListener(() => {
  if (!api.runtime.getManifest().permissions.includes('nativeMessaging')) return;
  api.contextMenus.create({ id: 'open-t256', title: msg('contextMenuOpen', 'Open with t256'), contexts: ['link', 'selection'] });
});
api.contextMenus.onClicked.addListener(info => {
  const uri = (info.linkUrl ?? info.selectionText ?? '').trim();
  if (!/^t256:[A-Za-z0-9_-]+(?:\.[A-Za-z0-9]+)*$/i.test(uri)) return;
  openNative({ action: 'open', uri }).catch(error => console.warn(error.message));
});
if (api.omnibox) {
  const inputUri = text => {
    const value = text.trim();
    return globalThis.t256URI.parseUri(/^t256:/i.test(value) ? value : `t256:${value}`).uri;
  };
  const hint = description => api.omnibox.setDefaultSuggestion({ description });
  api.omnibox.onInputStarted.addListener(() => hint(msg('omniboxStart', 'Enter a CID or a full t256 URI to open verified content')));
  api.omnibox.onInputChanged.addListener((text, suggest) => {
    try { inputUri(text); hint(msg('omniboxValid', 'Open verified content with your desktop helper')); }
    catch { hint(msg('omniboxInvalid', 'Enter a valid CID or t256 URI, optionally followed by a file suffix')); }
    suggest([]);
  });
  api.omnibox.onInputEntered.addListener(text => {
    let uri;
    try { uri = inputUri(text); }
    catch { hint(msg('omniboxInvalid', 'Enter a valid CID or t256 URI, optionally followed by a file suffix')); return; }
    openNative({ action: 'open', uri }).catch(error => console.warn(error.message));
  });
}
api.runtime.onMessage.addListener((message, sender, reply) => {
  // Only extension UI can launch the helper or ask for website connections.
  if (sender.id !== api.runtime.id || sender.tab) return false;
  const action = message?.action;
  if (!['open', 'settings', 'connection'].includes(action)) return false;
  const work = action === 'connection' ? native({ action, origin: message.origin }) : openNative({ action, uri: message.uri });
  work.then(value => reply(value ?? { ok: true }), error => reply({ error: error.message }));
  return true;
});
