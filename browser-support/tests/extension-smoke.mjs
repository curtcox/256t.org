import assert from 'node:assert/strict';
import { cp, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';

export async function extensionSmoke(chromium, root, state, runtime, fixture) {
  const extension = path.join(state, 'extension');
  await cp(path.join(root, 'extensions/shared'), extension, { recursive: true });
  const manifest = JSON.parse(await readFile(path.join(extension, 'manifest.json'), 'utf8'));
  // Test-only permission replaces the toolbar click that grants activeTab in a real browser.
  manifest.host_permissions = [runtime.origin + '/*'];
  await writeFile(path.join(extension, 'manifest.json'), JSON.stringify(manifest));
  const context = await chromium.launchPersistentContext(path.join(state, 'extension-profile'), {
    channel: 'chromium', args: [`--disable-extensions-except=${extension}`, `--load-extension=${extension}`],
  });
  try {
    const worker = context.serviceWorkers()[0] ?? await context.waitForEvent('serviceworker');
    const page = await context.newPage();
    await page.goto(runtime.origin + '/resolve.html');
    await page.evaluate(({ image, root }) => {
      document.body.innerHTML = `<img id="adapted" src="${image}"><a id="link" href="${root}">Content</a>`;
    }, fixture);
    const result = await worker.evaluate(async ({ origin, base }) => {
      const tabs = await chrome.tabs.query({});
      const tab = tabs.find(item => item.url === origin + '/resolve.html');
      await chrome.scripting.executeScript({ target: { tabId: tab.id }, files: ['content.js'] });
      const results = await chrome.scripting.executeScript({ target: { tabId: tab.id }, func: value => globalThis.t256AdaptPage(value), args: [base] });
      return results[0].result;
    }, { origin: runtime.origin, base: `${runtime.origin}/s/${runtime.view}` });
    assert.equal(result.error, undefined);
    assert.equal(result.adapted, 2);
    await page.waitForFunction(() => document.querySelector('#adapted').naturalWidth === 48);
    await page.locator('#link').click();
    await page.waitForFunction(() => document.querySelector('#module')?.textContent === '42');
    return { images: true, navigation: true };
  } finally { await context.close(); }
}
