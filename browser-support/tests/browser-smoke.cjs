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
    await client.hydrate(document); await client.hydrate(document);
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
  await page.goto(runtime.origin + '/resolve.html');
  await page.locator('#uri').fill('t256:AAAAAAAFaGVsbG8.txt');
  await page.getByRole('button', { name: 'Resolve and verify' }).click();
  await page.waitForFunction(() => document.querySelector('#status').textContent.startsWith('Verified 5 bytes'));
  assert.equal(await page.locator('#preview').textContent(), 'hello');
  assert.deepEqual(errors, []);
  return { gateway, loader, extras, errors };
};
