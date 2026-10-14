const key = location.hash.slice(1);
history.replaceState(null, '', '/');
const status = document.querySelector('#status');
let config;
let busy = false;
async function request(path, body) {
  const response = await fetch(path, { method: body ? 'POST' : 'GET', headers: { Authorization: `Bearer ${key}`, 'Content-Type': 'application/json' }, body: body ? JSON.stringify(body) : undefined });
  const result = await response.json();
  if (!response.ok) throw new Error(result.error);
  return result;
}
async function action(work) {
  if (busy) return;
  busy = true;
  for (const element of document.querySelectorAll('button, input[type="file"]')) element.disabled = true;
  status.textContent = 'Working…';
  try { const message = await work(); status.textContent = message ?? 'Ready. Content is verified before use.'; }
  catch (error) { status.textContent = error.message; }
  finally {
    busy = false;
    for (const element of document.querySelectorAll('button, input[type="file"]')) element.disabled = false;
  }
}
function button(label, work) {
  const element = document.createElement('button'); element.type = 'button'; element.textContent = label;
  element.addEventListener('click', () => action(work)); return element;
}
function render() {
  const rows = config.providers.map((provider, index) => {
    const row = document.createElement('tr');
    for (const text of [`${provider.id} · ${provider.type}${provider.enabled === false ? ' (disabled)' : ''}`, provider.path ?? provider.url ?? provider.repo]) {
      const cell = document.createElement('td'); cell.textContent = text; row.append(cell);
    }
    const controls = document.createElement('td');
    controls.append(button(provider.enabled === false ? 'Enable' : 'Disable', () => edit(next => { next.providers[index].enabled = provider.enabled === false; })));
    if (index) controls.append(button('Move up', () => edit(next => { [next.providers[index - 1], next.providers[index]] = [next.providers[index], next.providers[index - 1]]; })));
    if (['folder', 'git'].includes(provider.type)) controls.append(button('Index files', async () => { const result = await request('/api/index', { id: provider.id }); return `Indexed ${result.indexed} content identifiers`; }));
    if (['folder', 'git', 'github'].includes(provider.type)) {
      const upload = document.createElement('input'); upload.type = 'file'; upload.accept = '.json,application/json';
      upload.setAttribute('aria-label', `Import index for ${provider.id}`);
      const label = document.createElement('label'); label.className = 'import-index'; label.textContent = 'Import index'; label.append(upload);
      upload.addEventListener('change', () => action(async () => {
        const file = upload.files[0]; if (!file) return;
        try {
          if (file.size > 1000 * 1024) throw new Error('This index is too large for settings. Import it using the helper command instead.');
          const result = await request('/api/import-index', { id: provider.id, mapping: JSON.parse(await file.text()) });
          return `Imported ${result.indexed} content identifiers for ${provider.id}`;
        } finally { upload.value = ''; }
      }));
      controls.append(label);
    }
    controls.append(button('Remove', () => edit(next => { next.providers.splice(index, 1); })));
    row.append(controls); return row;
  });
  document.querySelector('#providers').replaceChildren(...rows);
  const origins = config.allowed_origins.map(origin => {
    const item = document.createElement('li'); item.textContent = origin + ' ';
    item.append(button('Revoke', async () => { await edit(next => { next.allowed_origins = next.allowed_origins.filter(value => value !== origin); }); document.querySelector('#connection').textContent = ''; })); return item;
  });
  document.querySelector('#origins').replaceChildren(...origins);
  for (const name of ['max_bytes', 'cache_bytes']) document.querySelector(`[name="${name}"]`).value = config[name] / 1048576;
}
async function refreshCache() {
  const cache = await request('/api/cache');
  document.querySelector('#cache-status').textContent = `${cache.entries} cached objects · ${(cache.bytes / 1048576).toFixed(2)} MiB of ${(cache.budget / 1048576).toFixed(2)} MiB`;
}
async function edit(change) {
  const next = structuredClone(config); change(next);
  await request('/api/config', next); config = next; render();
  await refreshCache();
}
document.querySelector('#add-provider').addEventListener('submit', event => {
  event.preventDefault(); action(async () => {
    const form = new FormData(event.target); const type = form.get('type');
    const provider = { id: form.get('id'), type, enabled: true };
    provider[type === 'http' ? 'url' : type === 'github' ? 'repo' : 'path'] = form.get('location');
    if (['git', 'github'].includes(type)) Object.assign(provider, { ref: form.get('ref'), directory: form.get('directory') });
    if (form.get('token_env')) provider.token_env = form.get('token_env');
    if (type === 'github') Object.assign(provider, { use_gh: form.has('use_gh'), api_url: form.get('api_url') });
    await edit(next => { next.providers.push(provider); }); event.target.reset();
  });
});
document.querySelector('#open-content').addEventListener('submit', event => {
  event.preventDefault(); action(async () => {
    const result = await request('/api/open', { uri: new FormData(event.target).get('uri').trim() });
    const link = document.createElement('a'); link.href = result.url; link.target = '_blank'; link.rel = 'noopener'; link.textContent = 'Open verified content';
    document.querySelector('#connection').replaceChildren(link); link.click();
  });
});
document.querySelector('#grant').addEventListener('submit', event => {
  event.preventDefault(); action(async () => {
    const origin = new URL(new FormData(event.target).get('origin')).origin;
    if (!config.allowed_origins.includes(origin)) await edit(next => { next.allowed_origins.push(origin); });
    const result = await request('/api/grant', { origin });
    document.querySelector('#connection').textContent = result.base;
    try { await navigator.clipboard.writeText(result.base); } catch { /* The visible URL can also be copied manually. */ }
  });
});
document.querySelector('#limits').addEventListener('submit', event => {
  event.preventDefault(); action(async () => {
    const form = new FormData(event.target);
    await edit(next => {
      for (const name of ['max_bytes', 'cache_bytes']) next[name] = Math.floor(Number(form.get(name)) * 1048576);
    });
  });
});
document.querySelector('#refresh-cache').addEventListener('click', () => action(refreshCache));
document.querySelector('#clear-cache').addEventListener('click', () => action(async () => {
  const result = await request('/api/cache/clear', {}); await refreshCache();
  return `Removed ${result.removed} cached objects. Providers and indexes were retained.`;
}));
action(async () => {
  if (!key) throw new Error('Open settings using the helper’s serve --open command or browser extension.');
  config = await request('/api/config'); render(); await refreshCache();
});
