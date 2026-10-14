import assert from 'node:assert/strict';
import { cp, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import { createServer } from 'node:http';
import { execFileSync } from 'node:child_process';

export async function extensionSmoke(chromium, root, state, runtime, fixture) {
  const site = createServer((request, response) => { response.writeHead(200, { 'Content-Type': 'text/html' }); response.end('<!doctype html><body></body>'); });
  await new Promise(resolve => site.listen(0, '127.0.0.1', resolve));
  const origin = `http://127.0.0.1:${site.address().port}`;
  const headers = { Authorization: `Bearer ${runtime.admin}`, 'Content-Type': 'application/json' };
  let previousConfig, context;
  try {
    previousConfig = await (await fetch(runtime.origin + '/api/config', { headers })).json();
    const configured = await fetch(runtime.origin + '/api/config', { method: 'POST', headers, body: JSON.stringify({ ...previousConfig, allowed_origins: [...previousConfig.allowed_origins, origin] }) });
    assert.equal(configured.status, 200);
    const grant = await (await fetch(runtime.origin + '/api/grant', { method: 'POST', headers, body: JSON.stringify({ origin }) })).json();
    const extension = path.join(state, 'extension');
    execFileSync(process.env.T256_PYTHON ?? 'python3', [path.join(root, 'build_extensions.py'), '--output', path.join(state, 'packages')]);
    await cp(path.join(state, 'packages/chromium'), extension, { recursive: true });
    const manifest = JSON.parse(await readFile(path.join(extension, 'manifest.json'), 'utf8'));
    // Test-only permission replaces the toolbar click that grants activeTab in a real browser.
    manifest.host_permissions = [runtime.origin + '/*', origin + '/*'];
    await writeFile(path.join(extension, 'manifest.json'), JSON.stringify(manifest));
    context = await chromium.launchPersistentContext(path.join(state, 'extension-profile'), {
      channel: 'chromium', args: [`--disable-extensions-except=${extension}`, `--load-extension=${extension}`],
    });
    const worker = context.serviceWorkers()[0] ?? await context.waitForEvent('serviceworker');
    const page = await context.newPage();
    await page.goto(origin + '/resolve.html');
    await page.evaluate(({ image, root, audio, captions, stylesheet }) => {
      document.body.innerHTML = `<picture><source srcset="${image} 1x"><img id="adapted" src="${image}" srcset="${image} 2x"></picture><a id="link" href="${root}">Content</a><audio id="audio" src="${audio}" preload="metadata"></audio><audio id="source-audio" preload="metadata"><source src="${audio}" type="audio/wav"></audio><video id="poster" poster="${image}"></video><video id="caption-video" src="${audio}" preload="metadata"><track id="captions" src="${captions}" kind="captions" default></video><link rel="stylesheet" href="${stylesheet}"><p id="css">CSS</p>`;
    }, fixture);
    const result = await worker.evaluate(async ({ origin, base }) => {
      const tabs = await chrome.tabs.query({});
      const tab = tabs.find(item => item.url === origin + '/resolve.html');
      await chrome.scripting.executeScript({ target: { tabId: tab.id }, files: ['content.js'] });
      const results = await chrome.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256AdaptPage(value), args: [base] });
      return results[0].result;
    }, { origin, base: grant.base });
    assert.equal(result.error, undefined);
    assert.equal(result.adapted, 10);
    await page.waitForFunction(() => document.querySelector('#adapted').naturalWidth === 48);
    await page.waitForFunction(() => document.querySelector('#audio').readyState >= 1 && document.querySelector('#source-audio').readyState >= 1);
    await page.waitForFunction(() => document.querySelector('#captions').readyState === 2);
    assert.match(await page.locator('#adapted').evaluate(image => image.currentSrc), /\/s\/[^/]+\/resource\//);
    assert.equal(await page.locator('#source-audio').evaluate(media => media.crossOrigin), 'anonymous');
    assert.equal(await page.locator('#css').evaluate(element => getComputedStyle(element).color), 'rgb(20, 80, 160)');
    await page.evaluate(({ image, first }) => {
      const literal = document.createElement('img'); literal.id = 'literal'; literal.srcset = `data:image/svg+xml,${image} 1x`;
      const invalid = document.createElement('img'); invalid.id = 'invalid'; invalid.srcset = image + '-invalid 1x';
      const script = document.createElement('script'); script.id = 'unmodified-script'; script.type = 'application/t256'; script.src = first;
      document.body.append(literal, invalid, script);
    }, fixture);
    const untouched = await worker.evaluate(async ({ origin, base }) => {
      const tab = (await chrome.tabs.query({})).find(item => item.url === origin + '/resolve.html');
      return (await chrome.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256AdaptPage(value), args: [base] }))[0].result;
    }, { origin, base: grant.base });
    assert.equal(untouched.adapted, 0);
    assert.equal(await page.locator('#literal').getAttribute('srcset'), `data:image/svg+xml,${fixture.image} 1x`);
    assert.equal(await page.locator('#invalid').getAttribute('srcset'), fixture.image + '-invalid 1x');
    assert.equal(await page.locator('#unmodified-script').getAttribute('src'), fixture.first);
    await page.evaluate(() => { for (const id of ['literal', 'invalid', 'unmodified-script']) document.getElementById(id).remove(); });
    await page.evaluate(({ missing }) => {
      const image = document.createElement('img'); image.id = 'missing'; image.src = missing; document.body.append(image);
    }, fixture);
    const failed = await worker.evaluate(async ({ origin, base }) => {
      const tab = (await chrome.tabs.query({})).find(item => item.url === origin + '/resolve.html');
      return (await chrome.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256AdaptPage(value), args: [base] }))[0].result;
    }, { origin, base: grant.base });
    assert.match(failed.error, /fixtures: Content or provider location was not found/);
    assert.equal(failed.adapted, 0);
    await page.evaluate(({ image }) => {
      document.querySelector('#missing').remove();
      const element = document.createElement('img'); element.id = 'changing'; element.src = image; document.body.append(element);
    }, fixture);
    await page.route('**/resolve?*', async route => {
      const response = await route.fetch();
      await page.locator('#changing').evaluate(element => { element.src = 'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="3" height="3"/>'; });
      await route.fulfill({ response });
    });
    const changed = await worker.evaluate(async ({ origin, base }) => {
      const tab = (await chrome.tabs.query({})).find(item => item.url === origin + '/resolve.html');
      return (await chrome.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256AdaptPage(value), args: [base] }))[0].result;
    }, { origin, base: grant.base });
    await page.unroute('**/resolve?*');
    assert.equal(changed.adapted, 0);
    assert.match(await page.locator('#changing').getAttribute('src'), /^data:/);
    await page.locator('#link').click();
    await page.waitForFunction(() => document.querySelector('#module')?.textContent === '42');
    return { images: true, media: true, captions: true, navigation: true };
  } finally {
    try {
      await context?.close();
      if (previousConfig) await fetch(runtime.origin + '/api/config', { method: 'POST', headers, body: JSON.stringify(previousConfig) });
    } finally { await new Promise(resolve => site.close(resolve)); }
  }
}
