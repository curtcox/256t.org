package provider

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const bigCID = "AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg"

var big = bytes.Repeat([]byte("a"), 65)

var ctx = context.Background()

func readAll(t *testing.T, rc io.ReadCloser, err error) []byte {
	t.Helper()
	if err != nil {
		t.Fatal(err)
	}
	defer rc.Close()
	data, err := io.ReadAll(rc)
	if err != nil {
		t.Fatal(err)
	}
	return data
}

func TestDir(t *testing.T) {
	root := filepath.Join(t.TempDir(), "store")
	d := &Dir{ProviderName: "d", Path: root}

	if _, err := d.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{}); err == nil {
		t.Fatal("save into a missing directory without Create should fail")
	}
	d.Create = true
	if _, err := d.Load(ctx, bigCID); !errors.Is(err, ErrNotFound) {
		t.Fatalf("Load missing: %v", err)
	}
	if has, err := d.Has(ctx, bigCID); has || err != nil {
		t.Fatalf("Has missing: %v %v", has, err)
	}
	res, err := d.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{})
	if err != nil {
		t.Fatal(err)
	}
	if res.Location != filepath.Join(root, bigCID) {
		t.Errorf("location %s", res.Location)
	}
	if has, _ := d.Has(ctx, bigCID); !has {
		t.Error("Has after save")
	}
	rc, err := d.Load(ctx, bigCID)
	if got := readAll(t, rc, err); !bytes.Equal(got, big) {
		t.Error("content differs")
	}
	res, err = d.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{})
	if err != nil || res.Details["already_present"] != true {
		t.Errorf("second save: %v %v", res.Details, err)
	}
	entries, _ := os.ReadDir(root)
	if len(entries) != 1 {
		t.Errorf("leftover temp files: %v", entries)
	}
	if _, err := d.Save(ctx, bigCID, bytes.NewReader(big[:10]), 65, SaveOptions{}); err != nil {
		// already present with the right size, so no write happens
		t.Errorf("save when present: %v", err)
	}
	ro := &Dir{ProviderName: "ro", Path: root, ReadOnly: true}
	if _, err := ro.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{}); !errors.Is(err, ErrReadOnly) {
		t.Errorf("read-only save: %v", err)
	}
}

func TestDirShortWrite(t *testing.T) {
	d := &Dir{ProviderName: "d", Path: t.TempDir()}
	if _, err := d.Save(ctx, bigCID, bytes.NewReader(big[:10]), 65, SaveOptions{}); err == nil {
		t.Fatal("short content should fail")
	}
	if has, _ := d.Has(ctx, bigCID); has {
		t.Error("short write left a file behind")
	}
}

func TestHTTP(t *testing.T) {
	store := map[string][]byte{}
	var gotAuth, gotType string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		key := strings.TrimPrefix(r.URL.Path, "/base/")
		gotAuth = r.Header.Get("Authorization")
		switch r.Method {
		case http.MethodGet, http.MethodHead:
			data, ok := store[key]
			if !ok {
				http.NotFound(w, r)
				return
			}
			w.Write(data)
		case http.MethodPut:
			gotType = r.Header.Get("Content-Type")
			data, _ := io.ReadAll(r.Body)
			store[key] = data
			w.WriteHeader(http.StatusCreated)
		default:
			w.WriteHeader(http.StatusMethodNotAllowed)
		}
	}))
	defer srv.Close()

	h := &HTTP{ProviderName: "h", URL: srv.URL + "/base/", Headers: map[string]string{"Authorization": "Bearer x"}}
	if h.CanSave() {
		t.Error("no save method means read-only")
	}
	if _, err := h.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{}); !errors.Is(err, ErrReadOnly) {
		t.Errorf("read-only save: %v", err)
	}
	if _, err := h.Load(ctx, bigCID); !errors.Is(err, ErrNotFound) {
		t.Errorf("load missing: %v", err)
	}
	h.SaveMethod = "put"
	if _, err := h.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{ContentType: "text/plain"}); err != nil {
		t.Fatal(err)
	}
	if gotAuth != "Bearer x" || gotType != "text/plain" {
		t.Errorf("headers: %q %q", gotAuth, gotType)
	}
	if has, err := h.Has(ctx, bigCID); !has || err != nil {
		t.Errorf("has: %v %v", has, err)
	}
	rc, err := h.Load(ctx, bigCID)
	if got := readAll(t, rc, err); !bytes.Equal(got, big) {
		t.Error("content differs")
	}
	tmpl := &HTTP{URL: srv.URL + "/base/{cid}"}
	if tmpl.Location("X") != srv.URL+"/base/X" {
		t.Errorf("template location %s", tmpl.Location("X"))
	}
}

func TestHTTPServerError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
	}))
	defer srv.Close()
	h := &HTTP{URL: srv.URL}
	if _, err := h.Load(ctx, bigCID); err == nil || errors.Is(err, ErrNotFound) {
		t.Errorf("want a server error, got %v", err)
	}
}

func TestHashbinSave(t *testing.T) {
	var req *http.Request
	var body []byte
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		req = r
		body, _ = io.ReadAll(r.Body)
		if r.Header.Get("Authorization") != "ApiKey hbk_test" {
			w.WriteHeader(http.StatusUnauthorized)
			json.NewEncoder(w).Encode(map[string]any{"error": "bad key"})
			return
		}
		json.NewEncoder(w).Encode(map[string]any{
			"cid": bigCID, "size_bytes": 65, "cost_cents": 1, "url": "https://256t.us/" + bigCID,
		})
	}))
	defer srv.Close()

	b := &Hashbin{ProviderName: "hashbin", ContentURL: "https://256t.us", APIURL: srv.URL + "/api"}
	if _, err := b.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{MaxCostCents: -1}); err == nil ||
		!strings.Contains(err.Error(), "T256_HASHBIN_API_KEY") {
		t.Errorf("missing key: %v", err)
	}
	b.APIKey = "hbk_wrong"
	if _, err := b.Save(ctx, bigCID, bytes.NewReader(big), 65, SaveOptions{MaxCostCents: -1}); err == nil ||
		!strings.Contains(err.Error(), "bad key") {
		t.Errorf("wrong key: %v", err)
	}
	b.APIKey = "hbk_test"
	res, err := b.Save(ctx, bigCID, bytes.NewReader(big), 65,
		SaveOptions{ContentType: "text/plain", RetentionMonths: 12, MaxCostCents: 5})
	if err != nil {
		t.Fatal(err)
	}
	if req.URL.Path != "/api/content" || req.URL.Query().Get("retention_months") != "12" ||
		req.URL.Query().Get("max_cost_cents") != "5" || req.Header.Get("Content-Type") != "text/plain" {
		t.Errorf("request: %s %v %v", req.URL, req.URL.Query(), req.Header)
	}
	if !bytes.Equal(body, big) {
		t.Error("uploaded body differs")
	}
	if res.Location != "https://256t.us/"+bigCID || res.Details["cost_cents"] != float64(1) {
		t.Errorf("result %+v", res)
	}
	if _, err := b.Save(ctx, "AAAAAAAA", bytes.NewReader(nil), 0, SaveOptions{MaxCostCents: -1}); err == nil {
		t.Error("CID mismatch in response should fail")
	}
}
