package main

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const (
	helloCID = "AAAAAAANSGVsbG8sIFdvcmxkIQ"
	bigCID   = "AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg"
)

var big = bytes.Repeat([]byte("a"), 65)

type env struct {
	t      *testing.T
	dir    string
	config string
	vars   map[string]string
}

func newEnv(t *testing.T, cfg map[string]any) *env {
	t.Helper()
	e := &env{t: t, dir: t.TempDir(), vars: map[string]string{}}
	e.config = filepath.Join(e.dir, "config.json")
	if cfg == nil {
		cfg = map[string]any{}
	}
	if _, ok := cfg["providers"]; !ok {
		cfg["providers"] = map[string]any{}
	}
	provs := cfg["providers"].(map[string]any)
	if _, ok := provs["local"]; !ok {
		provs["local"] = map[string]any{"type": "dir", "path": filepath.Join(e.dir, "local"), "create": true}
	}
	if _, ok := cfg["sources"]; !ok {
		cfg["sources"] = []string{"local"}
	}
	data, _ := json.Marshal(cfg)
	if err := os.WriteFile(e.config, data, 0o644); err != nil {
		t.Fatal(err)
	}
	return e
}

func (e *env) run(stdin string, args ...string) (int, string, string) {
	var out, errb bytes.Buffer
	a := &app{
		stdin: strings.NewReader(stdin), stdout: &out, stderr: &errb,
		getenv: func(k string) string {
			if k == "T256_CONFIG" {
				return e.config
			}
			return e.vars[k]
		},
	}
	code := a.run(args)
	return code, out.String(), errb.String()
}

func (e *env) write(name string, data []byte) string {
	p := filepath.Join(e.dir, name)
	if err := os.WriteFile(p, data, 0o644); err != nil {
		e.t.Fatal(err)
	}
	return p
}

func expect(t *testing.T, code int, want int, out, errs string) {
	t.Helper()
	if code != want {
		t.Fatalf("exit %d, want %d\nstdout: %s\nstderr: %s", code, want, out, errs)
	}
}

func TestCIDCommand(t *testing.T) {
	e := newEnv(t, nil)
	code, out, errs := e.run("Hello, World!", "cid")
	expect(t, code, 0, out, errs)
	if out != helloCID+"\n" {
		t.Errorf("got %q", out)
	}
	f := e.write("a.txt", big)
	code, out, _ = e.run("", "cid", f, "-f", "t256", "--ext", "auto")
	if code != 0 || out != "t256:"+bigCID+".txt\n" {
		t.Errorf("got %d %q", code, out)
	}
	code, out, _ = e.run("Hello, World!", "cid", "--json")
	var rec record
	if code != 0 || json.Unmarshal([]byte(out), &rec) != nil || rec.CID != helloCID || !rec.Inline || rec.Length != 13 {
		t.Errorf("json: %d %q", code, out)
	}
	code, out, _ = e.run("", "cid", f, f)
	if code != 0 || strings.Count(out, bigCID+"  "+f) != 2 {
		t.Errorf("multi: %q", out)
	}
}

func TestSaveAndLoadDir(t *testing.T) {
	e := newEnv(t, nil)
	f := e.write("big.txt", big)
	code, out, errs := e.run("", "save", f)
	expect(t, code, 0, out, errs)
	if out != bigCID+"\n" {
		t.Fatalf("save printed %q", out)
	}
	if data, err := os.ReadFile(filepath.Join(e.dir, "local", bigCID)); err != nil || !bytes.Equal(data, big) {
		t.Fatalf("stored file: %v", err)
	}
	code, out, errs = e.run("", "load", "t256:"+bigCID+".txt")
	expect(t, code, 0, out, errs)
	if out != string(big) {
		t.Errorf("load printed %q", out)
	}
	dst := filepath.Join(e.dir, "out.bin")
	code, out, errs = e.run("", "load", bigCID, "-o", dst)
	expect(t, code, 0, out, errs)
	if data, _ := os.ReadFile(dst); !bytes.Equal(data, big) {
		t.Error("-o content differs")
	}
	// overwrite an existing output
	code, out, errs = e.run("", "load", "-o", dst, bigCID)
	expect(t, code, 0, out, errs)

	code, out, errs = e.run("", "save", "-", "-f", "url")
	expect(t, code, 0, out, errs) // empty stdin: inline, nothing stored
	if out != "t256:AAAAAAAA\n" {
		t.Errorf("empty save printed %q", out)
	}
}

func TestSaveStdinAndFormats(t *testing.T) {
	e := newEnv(t, nil)
	code, out, errs := e.run(string(big), "save", "-f", "url")
	expect(t, code, 0, out, errs)
	if out != filepath.Join(e.dir, "local", bigCID)+"\n" {
		t.Errorf("url format: %q", out)
	}
	code, out, _ = e.run(string(big), "save", "--json", "--ext", "md")
	var rec record
	if code != 0 || json.Unmarshal([]byte(out), &rec) != nil || rec.Stored == nil || !*rec.Stored ||
		rec.MediaType != "text/markdown" || rec.URI != "t256:"+bigCID+".md" {
		t.Errorf("json: %d %q", code, out)
	}
	code, out, errs = e.run("Hello, World!", "save", "-store-inline")
	expect(t, code, 0, out, errs)
	if _, err := os.Stat(filepath.Join(e.dir, "local", helloCID)); err != nil {
		t.Error("-store-inline did not store")
	}
}

func TestSaveDryRunAndDestinations(t *testing.T) {
	e := newEnv(t, nil)
	other := filepath.Join(e.dir, "nas")
	os.Mkdir(other, 0o755)
	code, out, errs := e.run(string(big), "save", "-n", "--to", other)
	expect(t, code, 0, out, errs)
	if _, err := os.Stat(filepath.Join(other, bigCID)); err == nil {
		t.Error("dry run stored content")
	}
	code, out, errs = e.run(string(big), "save", "--to", other)
	expect(t, code, 0, out, errs)
	if _, err := os.Stat(filepath.Join(other, bigCID)); err != nil {
		t.Error("ad-hoc directory destination not used")
	}
	code, _, errs = e.run(string(big), "save", "--to", filepath.Join(e.dir, "missing"))
	if code != exitError || !strings.Contains(errs, "not available") {
		t.Errorf("missing ad-hoc directory: %d %s", code, errs)
	}
	code, _, errs = e.run(string(big), "save", "--to", "256t.org")
	if code != exitUsage || !strings.Contains(errs, "read-only") {
		t.Errorf("read-only destination: %d %s", code, errs)
	}
	code, _, errs = e.run(string(big), "save", "--to", "nonsense")
	if code != exitUsage || !strings.Contains(errs, "unknown provider") {
		t.Errorf("unknown destination: %d %s", code, errs)
	}
}

func TestLoadInlineNeedsNoSource(t *testing.T) {
	e := newEnv(t, map[string]any{"sources": []string{"https://invalid.invalid"}})
	code, out, errs := e.run("", "load", "256t:"+helloCID)
	expect(t, code, 0, out, errs)
	if out != "Hello, World!" {
		t.Errorf("got %q", out)
	}
}

func TestLoadFallbackAndVerification(t *testing.T) {
	var hits []string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		hits = append(hits, r.URL.Path)
		switch {
		case strings.HasPrefix(r.URL.Path, "/bad/"):
			w.Write(bytes.Repeat([]byte("b"), 65)) // wrong content, right length
		case strings.HasPrefix(r.URL.Path, "/huge/"):
			w.Write(bytes.Repeat([]byte("a"), 1<<20)) // too long
		case strings.HasPrefix(r.URL.Path, "/good/"):
			w.Write(big)
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()
	e := newEnv(t, map[string]any{"sources": []string{"local", srv.URL + "/none", srv.URL + "/bad", srv.URL + "/huge", srv.URL + "/good"}})

	code, out, errs := e.run("", "load", bigCID, "-v")
	expect(t, code, 0, out, errs)
	if out != string(big) || !strings.Contains(errs, "does not match") {
		t.Errorf("out %q errs %s", out, errs)
	}

	code, _, _ = e.run("", "load", bigCID, "--from", srv.URL+"/bad", "-q")
	if code != exitMismatch {
		t.Errorf("only bad source: exit %d", code)
	}
	code, _, errs = e.run("", "load", bigCID, "--from", srv.URL+"/none")
	if code != exitNotFound || !strings.Contains(errs, "not found") {
		t.Errorf("missing: exit %d %s", code, errs)
	}
	// A reference given as a URL is tried first.
	hits = nil
	code, out, _ = e.run("", "load", srv.URL+"/good/"+bigCID, "--from", srv.URL+"/none")
	if code != 0 || out != string(big) || hits[0] != "/good/"+bigCID {
		t.Errorf("url ref: %d %v", code, hits)
	}
	// --cache stores verified content in the destination.
	code, _, errs = e.run("", "load", bigCID, "--from", srv.URL+"/good", "--cache", "-o", filepath.Join(e.dir, "x"))
	expect(t, code, 0, "", errs)
	if _, err := os.Stat(filepath.Join(e.dir, "local", bigCID)); err != nil {
		t.Error("--cache did not store")
	}
	// now local has it
	hits = nil
	code, _, _ = e.run("", "load", bigCID, "-o", filepath.Join(e.dir, "y"))
	if code != 0 || len(hits) != 0 {
		t.Errorf("local source not used first: %v", hits)
	}
}

func TestLoadOutputDir(t *testing.T) {
	e := newEnv(t, nil)
	e.run(string(big), "save")
	outDir := filepath.Join(e.dir, "out")
	os.Mkdir(outDir, 0o755)
	code, out, errs := e.run("", "load", "-O", outDir, bigCID+".txt", "t256:"+helloCID+".txt")
	expect(t, code, 0, out, errs)
	want := filepath.Join(outDir, bigCID+".txt") + "\n" + filepath.Join(outDir, helloCID+".txt") + "\n"
	if out != want {
		t.Errorf("printed %q", out)
	}
	if data, _ := os.ReadFile(filepath.Join(outDir, helloCID+".txt")); string(data) != "Hello, World!" {
		t.Error("inline content not written")
	}
	code, _, _ = e.run("", "load", bigCID, helloCID)
	if code != exitUsage {
		t.Errorf("several refs without -O: %d", code)
	}
	// -o pointing at a directory behaves like -O
	code, _, _ = e.run("", "load", bigCID, "-o", outDir)
	if code != 0 {
		t.Errorf("-o dir: %d", code)
	}
	code, out, _ = e.run("", "load", "-O", outDir, bigCID, "AAAAAABB"+strings.Repeat("A", 86), "--json")
	if code != exitNotFound || strings.Count(out, "\n") != 2 || !strings.Contains(out, `"error"`) {
		t.Errorf("partial failure: %d %q", code, out)
	}
}

func TestVerify(t *testing.T) {
	e := newEnv(t, nil)
	code, out, _ := e.run("Hello, World!", "verify", "t256:"+helloCID+".txt")
	if code != 0 || out != "OK\n" {
		t.Errorf("match: %d %q", code, out)
	}
	code, out, _ = e.run("Hello", "verify", helloCID)
	if code != exitMismatch || !strings.HasPrefix(out, "MISMATCH") {
		t.Errorf("mismatch: %d %q", code, out)
	}
	f := e.write("big", big)
	code, _, _ = e.run("", "verify", "-q", bigCID, f)
	if code != 0 {
		t.Errorf("file: %d", code)
	}
	code, _, _ = e.run("", "verify", "nonsense!")
	if code != exitUsage {
		t.Errorf("bad ref: %d", code)
	}
}

func TestHas(t *testing.T) {
	e := newEnv(t, map[string]any{"sources": []string{"local", "other"},
		"providers": map[string]any{"other": map[string]any{"type": "dir", "path": "${OTHER_DIR}"}}})
	e.vars["OTHER_DIR"] = filepath.Join(e.dir, "other")
	os.Mkdir(e.vars["OTHER_DIR"], 0o755)
	os.WriteFile(filepath.Join(e.vars["OTHER_DIR"], bigCID), big, 0o644)

	code, out, errs := e.run("", "has", bigCID)
	expect(t, code, 0, out, errs)
	if out != filepath.Join(e.dir, "other", bigCID)+"\n" {
		t.Errorf("has printed %q", out)
	}
	code, out, _ = e.run("", "has", "--all", bigCID)
	if code != 0 || !strings.Contains(out, "\tlocal\tno\t") || !strings.Contains(out, "\tother\tyes\t") {
		t.Errorf("--all: %q", out)
	}
	code, _, _ = e.run("", "has", "-q", "AAAAAABB"+strings.Repeat("A", 86))
	if code != exitNotFound {
		t.Errorf("missing: %d", code)
	}
	code, out, _ = e.run("", "has", helloCID)
	if code != 0 || out != "t256:"+helloCID+"\n" {
		t.Errorf("inline: %d %q", code, out)
	}
}

func TestHashbinViaConfig(t *testing.T) {
	var auth, query string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method == http.MethodPost && r.URL.Path == "/api/content" {
			auth, query = r.Header.Get("Authorization"), r.URL.RawQuery
			io.Copy(io.Discard, r.Body)
			json.NewEncoder(w).Encode(map[string]any{"cid": bigCID, "cost_cents": 1, "url": "http://" + r.Host + "/c/" + bigCID})
			return
		}
		if r.URL.Path == "/c/"+bigCID {
			w.Write(big)
			return
		}
		http.NotFound(w, r)
	}))
	defer srv.Close()
	e := newEnv(t, map[string]any{
		"sources":     []string{"hashbin"},
		"destination": "hashbin",
		"providers": map[string]any{"hashbin": map[string]any{
			"type": "hashbin", "content_url": srv.URL + "/c", "api_url": srv.URL + "/api", "retention_months": 3,
		}},
	})
	code, _, errs := e.run(string(big), "save")
	if code != exitError || !strings.Contains(errs, "T256_HASHBIN_API_KEY") {
		t.Errorf("no key: %d %s", code, errs)
	}
	e.vars["T256_HASHBIN_API_KEY"] = "hbk_test"
	code, out, errs := e.run(string(big), "save", "-f", "url", "--max-cost-cents", "2")
	expect(t, code, 0, out, errs)
	if auth != "ApiKey hbk_test" || !strings.Contains(query, "retention_months=3") || !strings.Contains(query, "max_cost_cents=2") {
		t.Errorf("auth %q query %q", auth, query)
	}
	if out != srv.URL+"/c/"+bigCID+"\n" {
		t.Errorf("printed %q", out)
	}
	code, out, _ = e.run("", "load", bigCID)
	if code != 0 || out != string(big) {
		t.Errorf("load via hashbin: %d", code)
	}
	code, out, _ = e.run(string(big), "save", "--skip-existing", "--json")
	if code != 0 || !strings.Contains(out, `"stored":false`) {
		t.Errorf("skip-existing: %d %q", code, out)
	}
}

func TestEnvOverridesAndProviders(t *testing.T) {
	e := newEnv(t, nil)
	e.vars["T256_SOURCES"] = "local, 256t.org"
	e.vars["T256_DESTINATION"] = "hashbin.org"
	code, out, errs := e.run("", "providers")
	expect(t, code, 0, out, errs)
	for _, want := range []string{"source 1", "source 2", "destination", "hashbin"} {
		if !strings.Contains(out, want) {
			t.Errorf("providers output lacks %q:\n%s", want, out)
		}
	}
	code, out, _ = e.run("", "config", "show")
	if code != 0 || !strings.Contains(out, `"destination": "hashbin.org"`) {
		t.Errorf("config show: %q", out)
	}
}

func TestConfigInit(t *testing.T) {
	e := newEnv(t, nil)
	e.config = filepath.Join(e.dir, "new", "config.json")
	code, out, _ := e.run("", "config", "init")
	if code != 0 || out != e.config+"\n" {
		t.Fatalf("init: %d %q", code, out)
	}
	code, _, _ = e.run("", "config", "init")
	if code != exitError {
		t.Errorf("second init should refuse: %d", code)
	}
	code, out, errs := e.run("", "providers")
	expect(t, code, 0, out, errs)
	if !strings.Contains(out, "team") {
		t.Errorf("sample config not loaded: %s", out)
	}
}

func TestUsageAndInfo(t *testing.T) {
	e := newEnv(t, nil)
	if code, _, _ := e.run(""); code != exitUsage {
		t.Errorf("no args: %d", code)
	}
	if code, _, _ := e.run("", "bogus"); code != exitUsage {
		t.Errorf("unknown cmd: %d", code)
	}
	if code, out, _ := e.run("", "version"); code != 0 || !strings.HasPrefix(out, "t256 ") {
		t.Errorf("version: %q", out)
	}
	if code, _, _ := e.run("", "help", "save"); code != 0 {
		t.Errorf("help save: %d", code)
	}
	code, out, _ := e.run("", "info", "t256:"+bigCID+".tar.gz", "--json")
	if code != 0 || !strings.Contains(out, `"media_type":"application/gzip"`) || !strings.Contains(out, `"length":65`) {
		t.Errorf("info: %q", out)
	}
	if code, _, _ := e.run("", "cid", "-f", "bogus"); code != exitUsage {
		t.Errorf("bad format: %d", code)
	}
}
