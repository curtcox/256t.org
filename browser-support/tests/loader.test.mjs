import assert from 'node:assert/strict';
import { test } from 'node:test';
import { readFile } from 'node:fs/promises';
import { computeCid, parseUri, readVerified, verifyUri } from '../../implementations/ecmascript/uri.mjs';
import { T256Client } from '../web/loader.mjs';
import { T256WebClient } from '../web/web-client.mjs';

test('strict URI parsing agrees with all existing corpus identifiers', async () => {
  const { readdir } = await import('node:fs/promises');
  for (const name of await readdir(new URL('../../cids/', import.meta.url))) {
    const data = new Uint8Array(await readFile(new URL(`../../cids/${name}`, import.meta.url)));
    assert.equal(await computeCid(data), name);
    await verifyUri(`t256:${name}`, data);
  }
  for (const value of ['t256://AAAAAAABeA', 't256:AAAAAAABeB', 't256:AAAAAAABeA=', 't256:AAAAAAABeA#x', 't256:AAAAAAAB']) {
    assert.throws(() => parseUri(value));
  }
});

test('inline content does not contact providers', async () => {
  const client = new T256Client('http://127.0.0.1:25600/s/test-key');
  const data = new TextEncoder().encode('inline');
  assert.deepEqual(await client.bytes(`t256:${await computeCid(data)}.txt`), data);
});

test('disposed clients and already aborted calls reject even inline content', async () => {
  const client = new T256Client('http://127.0.0.1:25600/s/test-key');
  const uri = `t256:${await computeCid('inline')}.txt`;
  const controller = new AbortController(); controller.abort();
  await assert.rejects(client.bytes(uri, { signal: controller.signal }), { name: 'AbortError' });
  client.dispose();
  await assert.rejects(client.bytes(uri), { name: 'AbortError' });
  await assert.rejects(client.hydrate({}), { name: 'AbortError' });
  assert.throws(() => client.observe({}), { name: 'AbortError' });
});

test('disposal cancels a request while consuming its body', async () => {
  const uri = `t256:${await computeCid(new Uint8Array(100))}.txt`;
  const original = globalThis.fetch;
  let reading;
  const started = new Promise(resolve => { reading = resolve; });
  try {
    globalThis.fetch = async (_, { signal }) => new Response(new ReadableStream({
      start(controller) {
        signal.addEventListener('abort', () => controller.error(signal.reason));
        reading();
      },
    }));
    const client = new T256Client('http://127.0.0.1:25600/s/test-key');
    const pending = client.bytes(uri);
    await started; client.dispose();
    await assert.rejects(pending, { name: 'AbortError' });
  } finally { globalThis.fetch = original; }
});

test('loader rejects mismatched and oversized provider bytes', async () => {
  const data = new TextEncoder().encode('a'.repeat(100));
  const uri = `t256:${await computeCid(data)}.js`;
  const original = globalThis.fetch;
  try {
    globalThis.fetch = async () => new Response('b'.repeat(100));
    const client = new T256Client('http://127.0.0.1:25600/s/test-key');
    await assert.rejects(client.bytes(uri), /does not match/);
    globalThis.fetch = async () => new Response('a'.repeat(101));
    await assert.rejects(client.bytes(uri), /exceeds CID length/);
    await assert.rejects(new T256Client(client.base, { maxBytes: 10 }).bytes(uri), /size limit/);
  } finally { globalThis.fetch = original; }
});

test('abort applies while reading a response body, not just its headers', async () => {
  const uri = `t256:${await computeCid(new Uint8Array(100))}.txt`;
  const original = globalThis.fetch;
  try {
    globalThis.fetch = async (_, { signal }) => new Response(new ReadableStream({
      start(controller) { signal.addEventListener('abort', () => controller.error(new Error('aborted'))); },
    }));
    const client = new T256Client('http://127.0.0.1:25600/s/test-key', { timeout: 20 });
    await assert.rejects(client.bytes(uri), /aborted/);
  } finally { globalThis.fetch = original; }
});

test('public web resolver verifies provider fallback and handles compound suffixes', async () => {
  const bytes = new TextEncoder().encode('public content'.repeat(12));
  const uri = `t256:${await computeCid(bytes)}.tar.gz`;
  const original = globalThis.fetch;
  try {
    globalThis.fetch = async url => new Response(url.startsWith('https://bad.example') ? new Uint8Array(bytes.length) : bytes);
    const client = new T256WebClient(['https://bad.example', 'https://good.example']);
    const result = await client.read(uri);
    assert.deepEqual(result.bytes, bytes);
    assert.equal(result.provider, 'https://good.example');
    assert.equal(result.type, 'application/gzip');
    assert.equal(result.failures.length, 1);
  } finally { globalThis.fetch = original; }
});

test('a stalled public response times out during reading and falls back', async () => {
  const bytes = new Uint8Array(100); const uri = `t256:${await computeCid(bytes)}`;
  const original = globalThis.fetch;
  let stalled, cancelled = 0;
  try {
    globalThis.fetch = async url => {
      if (url.startsWith('https://ready.example')) return new Response(bytes);
      stalled = new ReadableStream({ start(controller) { controller.enqueue(bytes.slice(0, 1)); }, cancel() { cancelled++; } });
      return new Response(stalled);
    };
    const client = new T256WebClient(['https://stalled.example', 'https://ready.example'], { timeout: 20 });
    const result = await client.read(uri);
    assert.deepEqual(result.bytes, bytes);
    assert.equal(result.provider, 'https://ready.example');
    assert.match(result.failures[0].reason, /timed out/);
    assert.equal(cancelled, 1);
    assert.equal(stalled.locked, false);
  } finally { globalThis.fetch = original; }
});

test('public cancellation stops a stalled body without trying another provider', async () => {
  const uri = `t256:${await computeCid(new Uint8Array(100))}`;
  const original = globalThis.fetch; const controller = new AbortController();
  let calls = 0, stalled, reading;
  const started = new Promise(resolve => { reading = resolve; });
  try {
    globalThis.fetch = async () => {
      calls++;
      stalled = new ReadableStream({ pull() { reading(); } });
      return new Response(stalled);
    };
    const client = new T256WebClient(['https://one.example', 'https://two.example']);
    const result = client.read(uri, { signal: controller.signal });
    await started; controller.abort(new DOMException('Cancelled by caller', 'AbortError'));
    await assert.rejects(result, { name: 'AbortError' });
    assert.equal(calls, 1);
    assert.equal(stalled.locked, false);
    await assert.rejects(client.read('t256:AAAAAAAFaGVsbG8.txt', { signal: controller.signal }), { name: 'AbortError' });
  } finally { globalThis.fetch = original; }
});

test('cleanup failures do not replace a verification error', async () => {
  const uri = `t256:${await computeCid(new Uint8Array(100))}`;
  const stream = new ReadableStream({ start(controller) { controller.enqueue(new Uint8Array(101)); }, cancel() { throw new Error('Cleanup failed'); } });
  await assert.rejects(readVerified(new Response(stream), uri), /exceeds CID length/);
  assert.equal(stream.locked, false);
});
