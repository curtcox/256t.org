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
    const { T256Client } = await import('/loader.mjs');
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
  assert.deepEqual(errors, []);
  return { gateway, loader, errors };
};
