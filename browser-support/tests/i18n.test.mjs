import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { languages, negotiate, translator } from '../web/i18n.mjs';

const realFetch = globalThis.fetch;
test.before(() => {
  globalThis.fetch = async url => {
    try { return new Response(await readFile(new URL(url)), { headers: { 'Content-Type': 'application/json' } }); }
    catch { return new Response('missing', { status: 404 }); }
  };
});
test.after(() => { globalThis.fetch = realFetch; });

test('negotiate picks a supported language or English', () => {
  assert.deepEqual(languages, ['zh-Hans', 'pt-BR']);
  assert.equal(negotiate(['zh-CN', 'en']), 'zh-Hans');
  assert.equal(negotiate(['pt-PT']), 'pt-BR');
  assert.equal(negotiate(['fr', 'pt-BR']), 'pt-BR');
  assert.equal(negotiate(['zh-TW', 'fr']), 'en');
  assert.equal(negotiate(['en-GB', 'zh-CN']), 'en');
  assert.equal(negotiate([]), 'en');
});

test('English and unknown languages leave text unchanged but fill placeholders', async () => {
  for (const language of ['en', 'fr', undefined]) {
    const t = await translator(language);
    assert.equal(t('Edit {provider}', { provider: 'docs' }), 'Edit docs');
    assert.equal(t.message('Provider not found'), 'Provider not found');
  }
});

test('translations fill placeholders and translate relayed messages line by line', async () => {
  const t = await translator('pt-BR');
  assert.equal(t.language, 'pt-BR');
  assert.equal(t('Edit {provider}', { provider: 'docs' }), 'Editar docs');
  assert.equal(t('Not in the catalog {x}', { x: 1 }), 'Not in the catalog 1');
  assert.equal(t.message('Provider requires path'), 'O provedor exige path');
  assert.equal(t.message('max_bytes must be a positive byte count'), 'max_bytes deve ser uma quantidade positiva de bytes');
  assert.equal(t.message('No selected provider returned matching content\ndocs: Provider retrieval timed out\nweb: HTTP 404: Content was not found or repository access was not granted\nodd: something else'),
    'Nenhum provedor selecionado retornou conteúdo correspondente\ndocs: A obtenção pelo provedor excedeu o tempo limite\nweb: HTTP 404: O conteúdo não foi encontrado ou o acesso ao repositório não foi concedido\nodd: something else');
  const zh = await translator('zh-Hans');
  assert.equal(zh.message('t256 resolver returned HTTP 502'), 't256 解析器返回了 HTTP 502');
});

test('a missing catalog falls back to English', async () => {
  const t = await translator('zh-Hans', new URL('./does-not-exist/', import.meta.url));
  assert.equal(t.language, 'en');
  assert.equal(t.message('Provider not found'), 'Provider not found');
});
