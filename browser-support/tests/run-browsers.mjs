import { chromium, firefox, webkit } from 'playwright';
import { spawn, execFileSync } from 'node:child_process';
import { mkdtemp, readFile, realpath, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import smoke from './browser-smoke.cjs';
import { extensionSmoke } from './extension-smoke.mjs';
import { protocolHandlerSmoke } from './protocol-handler-smoke.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));
const state = await realpath(await mkdtemp(path.join(tmpdir(), 't256-browser-test-')));
const python = process.env.T256_PYTHON ?? 'python3';
const helper = spawn(python, [path.join(root, 'run.py'), '--state', state, 'serve', '--port', '0'], { stdio: ['ignore', 'ignore', 'pipe'] });
let diagnostics = '';
helper.stderr.on('data', bytes => { diagnostics += bytes; });
try {
  let runtime;
  for (let attempt = 0; attempt < 100; attempt++) {
    if (helper.exitCode !== null) throw new Error(`Helper exited: ${diagnostics}`);
    try { runtime = JSON.parse(await readFile(path.join(state, 'runtime.json'), 'utf8')); break; }
    catch { await new Promise(resolve => setTimeout(resolve, 100)); }
  }
  if (!runtime) throw new Error('Helper did not start');
  execFileSync(python, [path.join(root, 'tests/browser_fixture.py'), state], { env: { ...process.env, PYTHONPATH: root }, stdio: 'pipe' });
  const fixture = JSON.parse(await readFile(path.join(state, 'fixture.json'), 'utf8'));
  const engines = { chromium, firefox, webkit };
  const selected = process.argv.slice(2).length ? process.argv.slice(2) : Object.keys(engines);
  for (const name of selected) {
    const browser = await engines[name].launchPersistentContext(path.join(state, `profile-${name}`));
    try {
      const page = await browser.newPage(); page.setDefaultTimeout(15000);
      const result = await smoke(page, runtime, fixture);
      await protocolHandlerSmoke(page, runtime, fixture);
      console.log(`${name} ${browser.browser().version()}: images, script order, CSS imports, modules, dynamic declarations, workers, public resolver, and browser-link handling passed`);
    } finally { await browser.close(); }
    if (name === 'chromium') {
      await extensionSmoke(chromium, root, state, runtime, fixture);
      console.log('Chromium extension: images, responsive candidates, media, captions, page changes, provider diagnostics, gateway navigation, and helper-free viewer links passed');
    }
  }
} finally {
  const exited = new Promise(resolve => helper.once('exit', resolve));
  helper.kill('SIGTERM');
  await Promise.race([exited, new Promise(resolve => setTimeout(resolve, 3000))]);
  if (helper.exitCode === null) helper.kill('SIGKILL');
  await rm(state, { recursive: true, force: true });
}
