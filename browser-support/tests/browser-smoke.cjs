const assert = require('node:assert/strict');
const { readFile } = require('node:fs/promises');

module.exports = async function browserSmoke(page, runtime, fixture) {
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(runtime.settings_url);
  await page.getByRole('status').filter({ hasText: 'Ready' }).waitFor();
  assert.equal(await page.locator('#providers tr').count(), 1);
  await page.getByLabel('Import index for fixtures').setInputFiles({
    name: 'index.json', mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify({ [fixture.image.slice(5, -4)]: fixture.image.slice(5, -4) })),
  });
  await page.getByRole('status').filter({ hasText: 'Imported 1' }).waitFor();
  // Each engine shares the helper, so reset copies left by the preceding engine.
  await page.getByRole('button', { name: 'Clear cached content' }).click();
  await page.getByRole('status').filter({ hasText: 'Removed' }).waitFor();
  await page.evaluate(async ({ runtime, fixture }) => {
    const response = await fetch(`${runtime.origin}/s/${runtime.view}/raw/${fixture.image.slice(5)}`, { headers: { Range: 'bytes=2-5' } });
    if (response.status !== 206 || (await response.arrayBuffer()).byteLength !== 4) throw new Error('Browser range request failed');
  }, { runtime, fixture });
  await page.getByRole('button', { name: 'Refresh cache usage' }).click();
  await page.locator('#cache-status').filter({ hasText: '1 cached objects' }).waitFor();
  await page.getByRole('button', { name: 'Clear cached content' }).click();
  await page.locator('#cache-status').filter({ hasText: '0 cached objects' }).waitFor();
  await page.locator('#add-provider [name="id"]').fill('fixtures');
  await page.locator('#add-provider [name="location"]').fill('unused');
  await page.getByRole('button', { name: 'Add provider' }).click();
  await page.getByRole('status').filter({ hasText: 'Provider IDs must be unique' }).waitFor();
  await page.getByRole('button', { name: 'Disable', exact: true }).click();
  await page.getByRole('button', { name: 'Enable', exact: true }).waitFor();
  assert.equal(await page.locator('#providers tr').count(), 1);
  const originalLocation = await page.locator('#providers tr td').nth(1).textContent();
  await page.getByRole('button', { name: 'Edit', exact: true }).click();
  assert.equal(await page.locator('#add-provider [name="id"]').evaluate(element => element.readOnly), true);
  assert.equal(await page.locator('#add-provider [name="type"]').isDisabled(), true);
  await page.locator('#add-provider [name="location"]').fill('discard-this-draft');
  await page.getByRole('button', { name: 'Cancel edit' }).click();
  assert.equal(await page.locator('#providers tr td').nth(1).textContent(), originalLocation);
  await page.getByRole('button', { name: 'Edit', exact: true }).click();
  await page.locator('#add-provider [name="timeout"]').fill('2.5');
  await page.getByRole('button', { name: 'Save provider' }).click();
  await page.getByRole('status').filter({ hasText: 'Ready' }).waitFor();
  assert.equal(await page.locator('#providers tr').count(), 1);
  assert.match(await page.locator('#providers').textContent(), /disabled/);
  await page.getByRole('button', { name: 'Edit', exact: true }).click();
  assert.equal(await page.locator('#add-provider [name="timeout"]').inputValue(), '2.5');
  await page.getByRole('button', { name: 'Cancel edit' }).click();
  const downloadReady = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Export index' }).click();
  const download = await downloadReady;
  assert.equal(download.suggestedFilename(), 'fixtures-cids.json');
  assert.deepEqual(JSON.parse(await readFile(await download.path(), 'utf8')), { [fixture.image.slice(5, -4)]: fixture.image.slice(5, -4) });
  await page.getByRole('button', { name: 'Enable', exact: true }).click();
  await page.getByRole('button', { name: 'Disable', exact: true }).waitFor();
  const content = `${runtime.origin}/s/${runtime.view}/view/${fixture.root.slice(5)}`;
  await page.goto(content);
  await page.waitForFunction(() => document.querySelector('#order').textContent === 'first,second');
  await page.waitForFunction(() => document.querySelector('#module').textContent === '42');
  const gateway = await page.evaluate(() => ({
    image: document.querySelector('#image').naturalWidth,
    css: getComputedStyle(document.querySelector('#css')).color,
    opaque: window.origin,
  }));
  assert.equal(gateway.image, 48);
  assert.equal(gateway.css, 'rgb(20, 80, 160)');
  assert.equal(gateway.opaque, 'null');

  // The publisher loader also runs as an ordinary same-origin cooperating page.
  await page.goto(runtime.origin + '/demo.html');
  const loader = await page.evaluate(async ({ runtime, fixture }) => {
    const client = new T256Client(`${runtime.origin}/s/${runtime.view}`);
    document.body.innerHTML = '<img id="loaded"><p id="order"></p><p id="module"></p>';
    await client.image(document.querySelector('#loaded'), fixture.image);
    await client.script(fixture.first);
    await client.script(fixture.second);
    await client.imports({ [fixture.dependency]: fixture.dependency });
    await client.script(fixture.module, { module: true });
    const result = { image: document.querySelector('#loaded').naturalWidth, order: document.querySelector('#order').textContent, module: document.querySelector('#module').textContent };
    client.dispose();
    return result;
  }, { runtime, fixture });
  assert.deepEqual(loader, { image: 48, order: 'first,second', module: '42' });
  const extras = await page.evaluate(async ({ runtime, fixture }) => {
    const { computeCid } = t256URI;
    const client = new T256Client(`${runtime.origin}/s/${runtime.view}`);
    const declaration = document.createElement('script');
    declaration.type = 'application/t256';
    declaration.dataset.t256Src = `t256:${await computeCid('window.hydratedCount=(window.hydratedCount||0)+1')}.js`;
    document.body.append(declaration);
    await Promise.all([client.hydrate(document), client.hydrate(document), client.hydrate(document)]);
    await client.hydrate(document);
    const observer = client.observe(document);
    const ready = new Promise(resolve => document.addEventListener('t256-dynamic-ready', resolve, { once: true }));
    const dynamic = document.createElement('script'); dynamic.type = 'application/t256';
    dynamic.dataset.t256Src = `t256:${await computeCid('document.dispatchEvent(new Event("t256-dynamic-ready"))')}.js`;
    document.body.append(dynamic); await ready; observer.disconnect();
    const worker = await client.worker(`t256:${await computeCid('postMessage("worker-ready")')}.js`);
    const workerResult = await new Promise((resolve, reject) => { worker.onmessage = event => resolve(event.data); worker.onerror = reject; });
    worker.terminate(); client.dispose();
    return { hydrated: window.hydratedCount, dynamic: true, worker: workerResult };
  }, { runtime, fixture });
  assert.deepEqual(extras, { hydrated: 1, dynamic: true, worker: 'worker-ready' });
  const lifecycle = await page.evaluate(async ({ runtime }) => {
    const client = new T256Client(`${runtime.origin}/s/${runtime.view}`);
    const css = document.createElement('link'); css.rel = 'stylesheet';
    css.dataset.t256Href = `t256:${await t256URI.computeCid('#cssMarker { color: rgb(12, 34, 56) }')}.css`;
    const marker = document.createElement('p'); marker.id = 'cssMarker';
    const script = document.createElement('script'); script.type = 'application/t256';
    script.dataset.t256Src = `t256:${await t256URI.computeCid('window.cssBeforeScript=getComputedStyle(cssMarker).color')}.js`;
    document.body.append(css, marker, script);
    await Promise.all([client.hydrate(document), client.hydrate(document)]);
    const ready = window.cssBeforeScript;
    const retry = document.createElement('script'); retry.type = 'application/t256';
    const retryCid = await t256URI.computeCid('window.retryCount=(window.retryCount||0)+1');
    retry.dataset.t256Src = `t256:${retryCid}.txt`; document.body.append(retry);
    let failed = false;
    try { await client.hydrate(document); } catch { failed = true; }
    retry.dataset.t256Src = `t256:${retryCid}.js`;
    await Promise.all([client.hydrate(document), client.hydrate(document)]);
    // Replacing an image releases its prior object URL instead of accumulating blobs.
    const image = document.createElement('img'); document.body.append(image);
    const picture = `t256:${await t256URI.computeCid('<svg xmlns="http://www.w3.org/2000/svg" width="5" height="5"/>')}.svg`;
    await client.image(image, picture); await client.image(image, picture);
    const retained = client.objectUrls.size;
    client.observe(document); client.dispose();
    const observers = client.observers.size;
    return { ready, failed, retried: window.retryCount, retained, observers, released: client.objectUrls.size };
  }, { runtime });
  assert.deepEqual(lifecycle, { ready: 'rgb(12, 34, 56)', failed: true, retried: 1, retained: 1, observers: 0, released: 0 });
  await page.goto(runtime.origin + '/resolve.html');
  await page.locator('#uri').fill('t256:AAAAAAAFaGVsbG8.txt');
  await page.getByRole('button', { name: 'Resolve and verify' }).click();
  await page.waitForFunction(() => document.querySelector('#status').textContent.startsWith('Verified 5 bytes'));
  assert.equal(await page.locator('#preview').textContent(), 'hello');
  assert.deepEqual(errors, []);
  return { gateway, loader, extras, errors };
};
