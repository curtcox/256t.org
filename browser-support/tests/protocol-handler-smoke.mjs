import assert from 'node:assert/strict';

export async function protocolHandlerSmoke(page, runtime, fixture) {
  const address = runtime.origin + '/resolve.html';
  await page.goto(address);
  await page.locator('#status').filter({ hasText: 'Ready.' }).waitFor();
  // Invalid scheme checks cannot install a handler or bypass a consent prompt.
  const rejected = await page.evaluate(() => {
    if (typeof navigator.registerProtocolHandler !== 'function') return 'unsupported';
    try { navigator.registerProtocolHandler('web+t256', location.href + '#uri=%s'); return 'accepted'; }
    catch (error) { return error.name; }
  });
  assert.ok(['unsupported', 'SecurityError'].includes(rejected), rejected);
  let retrievals = 0;
  const requests = request => { if (request.url().includes('/resolve?') || request.url().includes('/resource/')) retrievals++; };
  page.on('request', requests);
  try {
    await page.goto(address + '#uri=' + encodeURIComponent(fixture.image.replace(/^t256:/, 'web+tcontent:')));
    await page.waitForFunction(expected => document.querySelector('#uri').value === expected, fixture.image);
    assert.equal(retrievals, 0);
    await page.getByText('Content providers', { exact: true }).click();
    await page.locator('#connection').fill(`${runtime.origin}/s/${runtime.view}`);
    assert.equal(await page.evaluate(() => localStorage.getItem('t256.resolver.connection.v1')), null);
    await page.getByRole('button', { name: 'Remember connection in this browser' }).click();
    await page.locator('#connection-status').filter({ hasText: 'Connection remembered' }).waitFor();
    await page.reload();
    await page.waitForFunction(expected => document.querySelector('#connection').value === expected, `${runtime.origin}/s/${runtime.view}`);
    assert.equal(retrievals, 0);
    await page.getByRole('button', { name: 'Resolve and verify' }).click();
    await page.locator('#status').filter({ hasText: 'Verified' }).waitFor();
    assert.ok(retrievals > 0);
    assert.ok(!(await page.locator('#share').getAttribute('href')).includes(runtime.view));
    await page.locator('#alias-example').click();
    await page.waitForFunction(() => document.querySelector('#uri').value === 't256:AAAAAAAFaGVsbG8.txt');
    assert.equal(await page.locator('#download').isHidden(), true);
    await page.getByText('Content providers', { exact: true }).click();
    await page.getByRole('button', { name: 'Forget remembered connection' }).click();
    assert.equal(await page.locator('#connection').inputValue(), '');
    assert.equal(await page.evaluate(() => localStorage.getItem('t256.resolver.connection.v1')), null);
    await page.getByRole('button', { name: 'Resolve and verify' }).click();
    await page.locator('#status').filter({ hasText: 'Verified 5 bytes' }).waitFor();
    assert.equal(await page.locator('#preview').textContent(), 'hello');
    assert.match(await page.locator('#share').getAttribute('href'), /#uri=t256%3A/);
    await page.goto(address + '#uri=' + encodeURIComponent('web+t256:AAAAAAAFaGVsbG8.txt'));
    await page.locator('#status').filter({ hasText: 'Expected t256:' }).waitFor();
    assert.equal(await page.locator('#uri').inputValue(), '');
  } finally { page.off('request', requests); }

  // Simulate browser UI acceptance/rejection without registering real handlers.
  await page.addInitScript(() => {
    window.handlerCalls = [];
    Object.defineProperty(navigator, 'registerProtocolHandler', { configurable: true, value: (scheme, template) => {
      window.handlerCalls.push({ scheme, template });
      if (window.rejectHandler) throw new DOMException('Rejected', 'SecurityError');
    } });
    Object.defineProperty(navigator, 'unregisterProtocolHandler', { configurable: true, value: (scheme, template) => window.handlerCalls.push({ remove: true, scheme, template }) });
  });
  await page.goto(address + '?credential=do-not-forward#uri=' + encodeURIComponent('t256:AAAAAAAFaGVsbG8.txt'));
  await page.locator('#register-handler').waitFor({ state: 'visible' });
  await page.waitForFunction(() => !document.querySelector('#register-handler').disabled);
  assert.deepEqual(await page.evaluate(() => window.handlerCalls), []);
  await page.getByRole('button', { name: 'Enable browser links' }).click();
  assert.deepEqual(await page.evaluate(() => window.handlerCalls), [{ scheme: 'web+tcontent', template: address + '#uri=%s' }]);
  await page.locator('#handler-status').filter({ hasText: 'Request sent.' }).waitFor();
  await page.getByRole('button', { name: 'Remove browser link handler' }).click();
  assert.equal((await page.evaluate(() => window.handlerCalls)).at(-1).remove, true);
  await page.evaluate(() => { window.rejectHandler = true; });
  await page.getByRole('button', { name: 'Enable browser links' }).click();
  await page.locator('#handler-status').filter({ hasText: 'did not allow' }).waitFor();
  await page.getByRole('button', { name: 'Resolve and verify' }).click();
  await page.locator('#status').filter({ hasText: 'Verified 5 bytes' }).waitFor();
  await page.addInitScript(() => {
    Storage.prototype.getItem = Storage.prototype.setItem = () => { throw new DOMException('Storage unavailable', 'SecurityError'); };
  });
  await page.goto(address + '#uri=' + encodeURIComponent('t256:AAAAAAAFaGVsbG8.txt'));
  await page.locator('#connection-status').filter({ hasText: 'No saved connection' }).waitFor({ state: 'attached' });
  await page.getByRole('button', { name: 'Resolve and verify' }).click();
  await page.locator('#status').filter({ hasText: 'Verified 5 bytes' }).waitFor();
  await page.getByText('Content providers', { exact: true }).click();
  await page.locator('#connection').fill(`${runtime.origin}/s/${runtime.view}`);
  await page.getByRole('button', { name: 'Remember connection in this browser' }).click();
  await page.locator('#connection-status').filter({ hasText: 'Could not remember' }).waitFor();
}
