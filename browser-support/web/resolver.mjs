import { T256Client } from './loader.mjs';
import { T256WebClient } from './web-client.mjs';
import { mediaType } from '../../implementations/ecmascript/uri.mjs';
import { browserScheme, handlerTemplate, parseBrowserUri, parseHandlerFragment } from './protocol-handler.mjs';
import { loadConnection, saveConnection, forgetConnection } from './connection-preference.mjs';
import { translator } from './i18n.mjs';

// Translates messages relayed from the desktop helper on translated pages.
const t = await translator(document.documentElement.lang);

const element = id => document.getElementById(id);
const objectUrls = new Set();
let active;
function applyFragment() {
  active?.abort(); clearPreview();
  element('uri').value = '';
  try {
    const initial = parseHandlerFragment(location.hash);
    if (initial) element('uri').value = initial.uri;
    element('status').textContent = 'Ready. Choose providers and resolve the content.';
  } catch (error) { element('status').textContent = t.message(error.message); }
}
applyFragment();
window.addEventListener('hashchange', applyFragment);
try {
  const saved = loadConnection(localStorage);
  if (saved) {
    element('connection').value = saved;
    element('forget-connection').hidden = false;
    element('connection-status').textContent = 'Using your remembered connection. Replace it after the helper restarts, or forget it here.';
  }
} catch {
  // Invalid/stale configuration must not prevent manual retrieval or registration.
  try { forgetConnection(localStorage); } catch { /* Storage may be unavailable. */ }
  element('connection-status').textContent = 'No saved connection is available. You can still paste one for this visit.';
}
element('save-connection').addEventListener('click', () => {
  try {
    element('connection').value = saveConnection(localStorage, element('connection').value.trim());
    element('forget-connection').hidden = false;
    element('connection-status').textContent = 'Connection remembered for this site in this browser. Content still waits for Resolve and verify.';
  } catch {
    element('connection-status').textContent = 'Could not remember this connection. Use a valid helper connection URL; if browser storage is blocked, paste it for each visit.';
  }
});
element('forget-connection').addEventListener('click', () => {
  try {
    forgetConnection(localStorage); element('connection').value = '';
    element('forget-connection').hidden = true;
    element('connection-status').textContent = 'Remembered connection removed. To revoke this site’s provider access, remove its grant in helper settings.';
  } catch { element('connection-status').textContent = 'Could not remove the saved connection. Clear this site’s browser storage, or revoke its grant in helper settings.'; }
});
let template;
try { if (window.isSecureContext) template = handlerTemplate(location.href); }
catch { /* Manual resolution remains available on other origins. */ }
const canRegister = template && typeof navigator.registerProtocolHandler === 'function';
element('register-handler').disabled = !canRegister;
element('remove-handler').hidden = !(canRegister && typeof navigator.unregisterProtocolHandler === 'function');
element('handler-status').textContent = canRegister
  ? 'Enable browser links, then approve the request in your browser. You can change the handler in browser settings.'
  : 'This browser or page cannot enable browser links. Paste a URI here, or use the desktop helper or extension.';
element('register-handler').addEventListener('click', () => {
  try {
    navigator.registerProtocolHandler(browserScheme, template);
    element('handler-status').textContent = 'Request sent. Approve it in your browser to open compatible links here; a dismissed request leaves your current handler unchanged.';
  } catch {
    element('handler-status').textContent = 'Your browser did not allow the request. Paste links here or use the desktop helper or extension.';
  }
});
element('remove-handler').addEventListener('click', () => {
  try {
    navigator.unregisterProtocolHandler(browserScheme, template);
    element('handler-status').textContent = 'Removal requested. Check browser settings to confirm your preferred handler.';
  } catch { element('handler-status').textContent = 'Remove this handler in browser settings. Manual resolution is still available.'; }
});
function clearPreview() {
  element('preview').textContent = ''; element('image').hidden = true;
  element('image').removeAttribute('src');
  for (const id of ['download', 'share', 'gateway']) {
    element(id).hidden = true; element(id).removeAttribute('href');
  }
  for (const url of objectUrls) URL.revokeObjectURL(url);
  objectUrls.clear();
}
element('cancel').addEventListener('click', () => active?.abort(new DOMException('Retrieval cancelled', 'AbortError')));
window.addEventListener('pagehide', () => { active?.abort(); clearPreview(); });
element('resolve-form').addEventListener('submit', async event => {
  event.preventDefault(); if (active) return;
  const controller = active = new AbortController();
  const options = { signal: controller.signal };
  let client;
  element('submit').disabled = true; element('cancel').hidden = false;
  element('status').textContent = 'Retrieving and verifying…';
  clearPreview();
  try {
    const uri = parseBrowserUri(element('uri').value.trim());
    element('uri').value = uri.uri;
    const connection = element('connection').value.trim();
    let bytes, provider, type, failures = [];
    if (connection) {
      client = new T256Client(connection);
      ({ provider, failures = [] } = await client.resolve(uri.uri, options));
      bytes = await client.bytes(uri.uri, options);
      type = mediaType(uri);
      element('gateway').href = `${client.base}/view/${uri.cid}${uri.suffix}`;
      element('gateway').hidden = false;
    } else {
      client = new T256WebClient(element('providers').value.split(/\r?\n/).map(value => value.trim()).filter(Boolean));
      ({ bytes, provider, type, failures } = await client.read(uri.uri, options));
    }
    element('status').textContent = `Verified ${bytes.length} bytes · ${provider}` + failures.map(failure => `\n${failure.provider}: ${t.message(failure.reason)}`).join('');
    const objectUrl = URL.createObjectURL(new Blob([bytes], { type: 'application/octet-stream' }));
    objectUrls.add(objectUrl);
    if (type.startsWith('image/') && type !== 'image/svg+xml') {
      const preview = URL.createObjectURL(new Blob([bytes], { type })); objectUrls.add(preview);
      element('image').src = preview; element('image').hidden = false;
    } else if (type.startsWith('text/') || type === 'application/json' || type === 'image/svg+xml') {
      element('preview').textContent = new TextDecoder().decode(bytes.slice(0, 100000));
    }
    element('download').href = objectUrl; element('download').download = uri.cid + uri.suffix; element('download').hidden = false;
    const share = new URL(location.pathname, location.origin); share.hash = `uri=${encodeURIComponent(uri.uri)}`;
    element('share').href = share.href; element('share').hidden = false;
  } catch (error) {
    element('status').textContent = controller.signal.aborted ? 'Retrieval cancelled.' : t.message(error.message + (error.failures ?? []).map(failure => `\n${failure.provider}: ${failure.reason}`).join(''));
  }
  finally {
    client?.dispose?.(); active = undefined;
    element('submit').disabled = false; element('cancel').hidden = true;
  }
});
