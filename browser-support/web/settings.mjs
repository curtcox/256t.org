const key = location.hash.slice(1);
history.replaceState(null, '', '/');
const status = document.querySelector('#status');
let config;
async function request(path, body) {
  const response = await fetch(path, { method: body ? 'POST' : 'GET', headers: { Authorization: `Bearer ${key}`, 'Content-Type': 'application/json' }, body: body ? JSON.stringify(body) : undefined });
  const result = await response.json();
  if (!response.ok) throw new Error(result.error);
  return result;
}
async function action(work) {
  status.textContent = 'Working…';
  try { await work(); status.textContent = 'Ready. Content is verified before use.'; }
  catch (error) { status.textContent = error.message; }
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
    controls.append(button(provider.enabled === false ? 'Enable' : 'Disable', async () => { provider.enabled = provider.enabled === false; await save(); }));
    if (index) controls.append(button('Move up', async () => { [config.providers[index - 1], config.providers[index]] = [provider, config.providers[index - 1]]; await save(); }));
    if (['folder', 'git'].includes(provider.type)) controls.append(button('Index files', async () => { const result = await request('/api/index', { id: provider.id }); status.textContent = `Indexed ${result.indexed} content identifiers`; }));
    controls.append(button('Remove', async () => { config.providers.splice(index, 1); await save(); }));
    row.append(controls); return row;
  });
  document.querySelector('#providers').replaceChildren(...rows);
  const origins = config.allowed_origins.map(origin => {
    const item = document.createElement('li'); item.textContent = origin + ' ';
    item.append(button('Revoke', async () => { config.allowed_origins = config.allowed_origins.filter(value => value !== origin); await save(); document.querySelector('#connection').textContent = ''; })); return item;
  });
  document.querySelector('#origins').replaceChildren(...origins);
  for (const name of ['max_bytes', 'cache_bytes']) document.querySelector(`[name="${name}"]`).value = config[name] / 1048576;
}
async function save() { await request('/api/config', config); render(); }
document.querySelector('#add-provider').addEventListener('submit', event => {
  event.preventDefault(); action(async () => {
    const form = new FormData(event.target); const type = form.get('type');
    const provider = { id: form.get('id'), type, enabled: true };
    provider[type === 'http' ? 'url' : type === 'github' ? 'repo' : 'path'] = form.get('location');
    if (['git', 'github'].includes(type)) Object.assign(provider, { ref: form.get('ref'), directory: form.get('directory') });
    if (form.get('token_env')) provider.token_env = form.get('token_env');
    if (type === 'github') Object.assign(provider, { use_gh: form.has('use_gh'), api_url: form.get('api_url') });
    const previous = structuredClone(config); config.providers.push(provider);
    try { await save(); event.target.reset(); } catch (error) { config = previous; throw error; }
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
    if (!config.allowed_origins.includes(origin)) { config.allowed_origins.push(origin); await save(); }
    const result = await request('/api/grant', { origin });
    document.querySelector('#connection').textContent = result.base;
    try { await navigator.clipboard.writeText(result.base); } catch { /* The visible URL can also be copied manually. */ }
  });
});
document.querySelector('#limits').addEventListener('submit', event => {
  event.preventDefault(); action(async () => {
    const form = new FormData(event.target);
    for (const name of ['max_bytes', 'cache_bytes']) config[name] = Math.floor(Number(form.get(name)) * 1048576);
    await save();
  });
});
action(async () => {
  if (!key) throw new Error('Open settings using the helper’s serve --open command or browser extension.');
  config = await request('/api/config'); render();
});
