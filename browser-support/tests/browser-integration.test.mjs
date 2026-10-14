import assert from 'node:assert/strict';
import { test } from 'node:test';
import { execFileSync } from 'node:child_process';
import { mkdtemp, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import vm from 'node:vm';
import { computeCid } from '../../implementations/ecmascript/uri.mjs';
import { browserScheme, handlerTemplate, parseBrowserUri, parseHandlerFragment } from '../web/protocol-handler.mjs';

test('browser alias preserves canonical identity and handler transport decodes exactly once', async () => {
  const cid = await computeCid('mixed case payload'.repeat(10));
  const canonical = `t256:${cid}.tar.gz`;
  const alias = `WEB+TCONTENT:${cid}.TAR.GZ`;
  assert.equal(browserScheme, 'web+tcontent');
  assert.equal(parseBrowserUri(alias).uri, canonical);
  assert.equal(parseBrowserUri(canonical).uri, canonical);
  assert.equal(parseHandlerFragment('#uri=' + encodeURIComponent(alias)).uri, canonical);
  assert.equal(parseHandlerFragment(''), null);
  for (const uri of ['web+t256:AAAAAAAFaGVsbG8', 'web+other:AAAAAAAFaGVsbG8', 'web+tcontent://AAAAAAAFaGVsbG8', 'web+tcontent:AAAAAAABeB', alias + '?q=1', alias + '#private', alias.replace(cid, '%41' + cid.slice(1))]) {
    assert.throws(() => parseBrowserUri(uri));
  }
  for (const fragment of ['#uri=' + encodeURIComponent(encodeURIComponent(alias)), '#uri=' + encodeURIComponent(alias) + '&uri=' + encodeURIComponent(canonical), '#other=value']) {
    assert.throws(() => parseHandlerFragment(fragment));
  }
});

test('registration templates retain the hosting path without credentials, queries or capabilities', () => {
  assert.equal(handlerTemplate('https://example.org/project/resolve.html?secret=credential#connection=capability'), 'https://example.org/project/resolve.html#uri=%s');
  assert.equal(handlerTemplate('http://127.0.0.1:25600/resolve.html#uri=old'), 'http://127.0.0.1:25600/resolve.html#uri=%s');
  assert.equal(handlerTemplate('http://[::1]/resolve.html'), 'http://[::1]/resolve.html#uri=%s');
  for (const url of ['http://example.org/resolve.html', 'http://localhost.evil/resolve.html', 'https://user:credential@example.org/resolve.html', 'file:///resolve.html']) {
    assert.throws(() => handlerTemplate(url));
  }
});

test('built extension keyword accepts CIDs/full URIs and rejects invalid input before native launch', async () => {
  const destination = await mkdtemp(path.join(tmpdir(), 't256-extension-unit-'));
  try {
    execFileSync(process.env.T256_PYTHON ?? 'python3', [fileURLToPath(new URL('../build_extensions.py', import.meta.url)), '--output', destination]);
    const background = await readFile(new URL('../extensions/shared/background.js', import.meta.url), 'utf8');
    const uriScript = await readFile(path.join(destination, 'chromium/uri.js'), 'utf8');
    const navigation = await readFile(path.join(destination, 'chromium/navigation.js'), 'utf8');
    const cid = await computeCid('hash content'.repeat(20));
    for (const browser of ['chromium', 'firefox']) {
      const events = {}, messages = [], opened = [], hints = [];
      const event = name => ({ addListener: listener => { events[name] = listener; } });
      const api = {
        runtime: { onInstalled: event('installed'), onMessage: event('message'), sendNativeMessage: async (host, message) => {
          assert.equal(host, 'org.t256.helper'); messages.push(message);
          return { url: `http://127.0.0.1:25600/view/${message.uri.slice(5)}` };
        } },
        contextMenus: { onClicked: event('menu') },
        tabs: { create: async value => { opened.push(value); } },
        omnibox: {
          onInputStarted: event('started'), onInputChanged: event('changed'), onInputEntered: event('entered'),
          setDefaultSuggestion: value => { hints.push(value.description); },
        },
      };
      const context = vm.createContext({ console, URL, atob, btoa, Uint8Array, [browser === 'firefox' ? 'browser' : 'chrome']: api });
      if (browser === 'chromium') context.importScripts = name => {
        assert.equal(name, 'uri.js'); vm.runInContext(uriScript, context);
      };
      else {
        const manifest = JSON.parse(await readFile(path.join(destination, 'firefox/manifest.json'), 'utf8'));
        assert.deepEqual(manifest.background.scripts, ['uri.js', 'background.js']);
        vm.runInContext(uriScript, context);
      }
      vm.runInContext(background, context);
      vm.runInContext(navigation, context);
      assert.equal(context.t256ViewerUrl('https://viewer.example/project/resolve.html', `T256:${cid}.TXT`), `https://viewer.example/project/resolve.html#uri=${encodeURIComponent(`t256:${cid}.txt`)}`);
      for (const viewer of ['javascript:alert(1)', 'http://remote.example/resolve.html', 'https://user:secret@viewer.example/', 'https://viewer.example/?capability=secret', 'https://viewer.example/#secret']) {
        assert.throws(() => context.t256ViewerUrl(viewer, 't256:AAAAAAAFaGVsbG8'));
      }
      assert.throws(() => context.t256ViewerUrl('https://viewer.example/', 't256:AAAAAAABeB'));
      events.started(); assert.match(hints.at(-1), /CID or a full t256 URI/);
      for (const value of [' AAA AAA ', 't256://AAAAAAAFaGVsbG8', 'AAAAAAABeB', 'web+t256:AAAAAAAFaGVsbG8', 'AAAAAAAFaGVsbG8#fragment', '']) {
        events.changed(value, suggestions => assert.equal(suggestions.length, 0));
        assert.match(hints.at(-1), /valid CID/);
        events.entered(value);
      }
      assert.equal(messages.length, 0);
      for (const [input, expected] of [
        [' AAAAAAAFaGVsbG8.TXT ', 't256:AAAAAAAFaGVsbG8.txt'],
        [`T256:${cid}.TAR.GZ`, `t256:${cid}.tar.gz`],
      ]) {
        events.changed(input, () => {}); assert.match(hints.at(-1), /Open verified/);
        events.entered(input);
        await new Promise(resolve => setImmediate(resolve));
        assert.equal(messages.at(-1).uri, expected);
        assert.equal(opened.at(-1).url, `http://127.0.0.1:25600/view/${expected.slice(5)}`);
      }
    }
  } finally { await rm(destination, { recursive: true, force: true }); }
});
