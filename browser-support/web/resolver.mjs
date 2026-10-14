import { T256Client } from './loader.mjs';
import { T256WebClient } from './web-client.mjs';
import { mediaType, parseUri } from '../../implementations/ecmascript/uri.mjs';

const element = id => document.getElementById(id);
const initial = new URLSearchParams(location.hash.slice(1)).get('uri');
if (initial) element('uri').value = initial;
let objectUrl;
let active;
function clearPreview() {
  element('preview').textContent = ''; element('image').hidden = true;
  element('image').removeAttribute('src');
  for (const id of ['download', 'share', 'gateway']) {
    element(id).hidden = true; element(id).removeAttribute('href');
  }
  if (objectUrl) URL.revokeObjectURL(objectUrl);
  objectUrl = undefined;
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
    const uri = parseUri(element('uri').value.trim());
    const connection = element('connection').value.trim();
    let bytes, provider, type;
    if (connection) {
      client = new T256Client(connection);
      provider = (await client.resolve(uri.uri, options)).provider;
      bytes = await client.bytes(uri.uri, options);
      type = mediaType(uri);
      element('gateway').href = `${client.base}/view/${uri.cid}${uri.suffix}`;
      element('gateway').hidden = false;
    } else {
      client = new T256WebClient(element('providers').value.split(/\r?\n/).map(value => value.trim()).filter(Boolean));
      ({ bytes, provider, type } = await client.read(uri.uri, options));
    }
    element('status').textContent = `Verified ${bytes.length} bytes · ${provider}`;
    objectUrl = URL.createObjectURL(new Blob([bytes], { type }));
    if (type.startsWith('image/')) { element('image').src = objectUrl; element('image').hidden = false; }
    else if (type.startsWith('text/') || type === 'application/json') element('preview').textContent = new TextDecoder().decode(bytes.slice(0, 100000));
    element('download').href = objectUrl; element('download').download = uri.cid + uri.suffix; element('download').hidden = false;
    const share = new URL(location.pathname, location.origin); share.hash = `uri=${encodeURIComponent(uri.uri)}`;
    element('share').href = share.href; element('share').hidden = false;
  } catch (error) { element('status').textContent = controller.signal.aborted ? 'Retrieval cancelled.' : error.message; }
  finally {
    client?.dispose?.(); active = undefined;
    element('submit').disabled = false; element('cancel').hidden = true;
  }
});
