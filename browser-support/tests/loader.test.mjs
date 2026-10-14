import assert from 'node:assert/strict';
import { test } from 'node:test';
import { readFile } from 'node:fs/promises';
import { computeCid, parseUri, verifyUri } from '../../implementations/ecmascript/uri.mjs';
import { T256Client } from '../web/loader.mjs';

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
