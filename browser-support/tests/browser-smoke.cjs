const assert = require('node:assert/strict');

module.exports = async function browserSmoke(page, runtime, fixture) {
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(runtime.settings_url);
  await page.getByRole('status').filter({ hasText: 'Ready' }).waitFor();
  assert.equal(await page.locator('#providers tr').count(), 1);
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
