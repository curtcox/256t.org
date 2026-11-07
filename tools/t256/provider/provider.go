// Package provider loads and saves 256t content from filesystem directories,
// generic HTTP servers, and hashbin.org.
package provider

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

var (
	// ErrNotFound means the provider does not hold the content.
	ErrNotFound = errors.New("not found")
	// ErrReadOnly means the provider cannot save content.
	ErrReadOnly = errors.New("provider is read-only")
)

// UserAgent is sent with every HTTP request.
var UserAgent = "t256 (+https://256t.org)"

// SaveOptions carries per-save settings. Providers ignore what they do not use.
type SaveOptions struct {
	ContentType     string
	RetentionMonths int   // hashbin: whole months of retention (0 = provider default)
	MaxCostCents    int64 // hashbin: refuse the upload if it would cost more (-1 = no ceiling)
}

// SaveResult describes a completed save.
type SaveResult struct {
	Location string         // URL or path where the content can now be loaded
	Details  map[string]any // provider-specific response fields
}

// Provider is a place 256t content can be loaded from and, optionally, saved to.
type Provider interface {
	Name() string
	Kind() string
	// Describe returns a short human-readable description of where content lives.
	Describe() string
	// Location returns the URL or path the content would be found at.
	Location(cid string) string
	// Load opens the content. It returns ErrNotFound when it is absent.
	// The caller must verify what it reads.
	Load(ctx context.Context, cid string) (io.ReadCloser, error)
	// Has reports whether the provider holds the content (without verifying it).
	Has(ctx context.Context, cid string) (bool, error)
	// CanSave reports whether Save can work.
	CanSave() bool
	// Save stores size bytes read from content under cid.
	Save(ctx context.Context, cid string, content io.Reader, size int64, opts SaveOptions) (SaveResult, error)
}

// ---------------------------------------------------------------- directory

// Dir stores content as files named by CID in one directory. It works for
// local disks and mounted network drives (SMB/NFS/UNC paths) alike, and the
// same directory can be served over HTTP as a 256t server.
type Dir struct {
	ProviderName string
	Path         string
	Create       bool // create the directory on first save
	ReadOnly     bool
}

func (d *Dir) Name() string               { return d.ProviderName }
func (d *Dir) Kind() string               { return "dir" }
func (d *Dir) Describe() string           { return d.Path }
func (d *Dir) Location(cid string) string { return filepath.Join(d.Path, cid) }
func (d *Dir) CanSave() bool              { return !d.ReadOnly }

func (d *Dir) Load(_ context.Context, cid string) (io.ReadCloser, error) {
	f, err := os.Open(d.Location(cid))
	if errors.Is(err, os.ErrNotExist) {
		return nil, ErrNotFound
	}
	return f, err
}

func (d *Dir) Has(_ context.Context, cid string) (bool, error) {
	_, err := os.Stat(d.Location(cid))
	if errors.Is(err, os.ErrNotExist) {
		return false, nil
	}
	return err == nil, err
}

func (d *Dir) Save(_ context.Context, cid string, content io.Reader, size int64, _ SaveOptions) (SaveResult, error) {
	res := SaveResult{Location: d.Location(cid)}
	if d.ReadOnly {
		return res, ErrReadOnly
	}
	if info, err := os.Stat(d.Path); err != nil {
		if !errors.Is(err, os.ErrNotExist) || !d.Create {
			return res, fmt.Errorf("directory %s is not available: %w", d.Path, err)
		}
		if err := os.MkdirAll(d.Path, 0o755); err != nil {
			return res, err
		}
	} else if !info.IsDir() {
		return res, fmt.Errorf("%s is not a directory", d.Path)
	}
	if info, err := os.Stat(res.Location); err == nil && info.Size() == size {
		res.Details = map[string]any{"already_present": true}
		return res, nil
	}
	tmp, err := os.CreateTemp(d.Path, ".t256-*.tmp")
	if err != nil {
		return res, err
	}
	defer os.Remove(tmp.Name())
	n, err := io.Copy(tmp, content)
	if err == nil && n != size {
		err = fmt.Errorf("wrote %d bytes, expected %d", n, size)
	}
	if err == nil {
		err = tmp.Sync()
	}
	if cerr := tmp.Close(); err == nil {
		err = cerr
	}
	if err != nil {
		return res, err
	}
	if err := os.Rename(tmp.Name(), res.Location); err != nil {
		// Another writer may have stored the same content concurrently.
		if info, serr := os.Stat(res.Location); serr == nil && info.Size() == size {
			return res, nil
		}
		return res, err
	}
	return res, nil
}

// --------------------------------------------------------------------- HTTP

// HTTP loads content with GET <URL>/<cid> from any 256t-compatible server.
// If URL contains "{cid}" the CID is substituted there instead of appended.
// Saving is optional: SaveMethod "put" sends PUT to SaveURL (default URL),
// "post" sends POST to SaveURL with the CID left out unless it has "{cid}".
type HTTP struct {
	ProviderName string
	URL          string
	SaveMethod   string            // "", "put" or "post"
	SaveURL      string            // defaults to URL
	Headers      map[string]string // sent with every request (already env-expanded)
	Client       *http.Client
}

func (h *HTTP) Name() string               { return h.ProviderName }
func (h *HTTP) Kind() string               { return "http" }
func (h *HTTP) Describe() string           { return h.URL }
func (h *HTTP) Location(cid string) string { return join(h.URL, cid) }
func (h *HTTP) CanSave() bool              { return h.SaveMethod != "" }

func join(base, cid string) string {
	if strings.Contains(base, "{cid}") {
		return strings.ReplaceAll(base, "{cid}", cid)
	}
	return strings.TrimRight(base, "/") + "/" + cid
}

func (h *HTTP) client() *http.Client {
	if h.Client != nil {
		return h.Client
	}
	return http.DefaultClient
}

func (h *HTTP) request(ctx context.Context, method, u string, body io.Reader) (*http.Request, error) {
	req, err := http.NewRequestWithContext(ctx, method, u, body)
	if err != nil {
		return nil, err
	}
	req.Header.Set("User-Agent", UserAgent)
	for k, v := range h.Headers {
		req.Header.Set(k, v)
	}
	return req, nil
}

func (h *HTTP) Load(ctx context.Context, cid string) (io.ReadCloser, error) {
	return httpGet(ctx, h, h.Location(cid))
}

func httpGet(ctx context.Context, h *HTTP, u string) (io.ReadCloser, error) {
	req, err := h.request(ctx, http.MethodGet, u, nil)
	if err != nil {
		return nil, err
	}
	resp, err := h.client().Do(req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode == http.StatusOK {
		return resp.Body, nil
	}
	resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound || resp.StatusCode == http.StatusGone {
		return nil, ErrNotFound
	}
	return nil, fmt.Errorf("GET %s: %s", u, resp.Status)
}

// LoadURL fetches an arbitrary URL, for references given as URLs.
func LoadURL(ctx context.Context, client *http.Client, u string) (io.ReadCloser, error) {
	return httpGet(ctx, &HTTP{Client: client}, u)
}

func (h *HTTP) Has(ctx context.Context, cid string) (bool, error) {
	u := h.Location(cid)
	req, err := h.request(ctx, http.MethodHead, u, nil)
	if err != nil {
		return false, err
	}
	resp, err := h.client().Do(req)
	if err != nil {
		return false, err
	}
	resp.Body.Close()
	switch {
	case resp.StatusCode == http.StatusOK:
		return true, nil
	case resp.StatusCode == http.StatusNotFound || resp.StatusCode == http.StatusGone:
		return false, nil
	case resp.StatusCode == http.StatusMethodNotAllowed || resp.StatusCode == http.StatusNotImplemented:
		body, err := h.Load(ctx, cid)
		if errors.Is(err, ErrNotFound) {
			return false, nil
		}
		if err != nil {
			return false, err
		}
		body.Close()
		return true, nil
	}
	return false, fmt.Errorf("HEAD %s: %s", u, resp.Status)
}

func (h *HTTP) Save(ctx context.Context, cid string, content io.Reader, size int64, opts SaveOptions) (SaveResult, error) {
	var res SaveResult
	base := h.SaveURL
	if base == "" {
		base = h.URL
	}
	var method, u string
	switch strings.ToLower(h.SaveMethod) {
	case "put":
		method, u = http.MethodPut, join(base, cid)
	case "post":
		method, u = http.MethodPost, strings.ReplaceAll(base, "{cid}", cid)
	case "":
		return res, ErrReadOnly
	default:
		return res, fmt.Errorf("unknown save method %q (use put or post)", h.SaveMethod)
	}
	req, err := h.request(ctx, method, u, content)
	if err != nil {
		return res, err
	}
	req.ContentLength = size
	req.Header.Set("Content-Type", contentType(opts))
	resp, err := h.client().Do(req)
	if err != nil {
		return res, err
	}
	defer resp.Body.Close()
	if resp.StatusCode/100 != 2 {
		return res, fmt.Errorf("%s %s: %s%s", method, u, resp.Status, errorDetail(resp.Body))
	}
	res.Location = h.Location(cid)
	if loc := resp.Header.Get("Location"); loc != "" {
		if r, err := resp.Request.URL.Parse(loc); err == nil {
			res.Location = r.String()
		}
	}
	return res, nil
}

func contentType(opts SaveOptions) string {
	if opts.ContentType != "" {
		return opts.ContentType
	}
	return "application/octet-stream"
}

func errorDetail(body io.Reader) string {
	data, _ := io.ReadAll(io.LimitReader(body, 4096))
	var e struct {
		Error   any            `json:"error"`
		Message string         `json:"message"`
		Details map[string]any `json:"details"`
	}
	if json.Unmarshal(data, &e) == nil && (e.Error != nil || e.Message != "") {
		msg := e.Message
		if e.Error != nil {
			msg = fmt.Sprint(e.Error)
		}
		if len(e.Details) > 0 {
			d, _ := json.Marshal(e.Details)
			msg += " " + string(d)
		}
		return ": " + msg
	}
	if s := strings.TrimSpace(string(data)); s != "" && len(s) < 300 && !strings.HasPrefix(s, "<") {
		return ": " + s
	}
	return ""
}

// ------------------------------------------------------------------ hashbin

// Hashbin loads content from hashbin's content domain and saves it with the
// hashbin.org content API using a scoped API key (content:write).
type Hashbin struct {
	ProviderName string
	ContentURL   string // default https://256t.us
	APIURL       string // default https://hashbin.org/api
	APIKey       string
	APIKeyEnv    string // where the key was expected, for error messages
	Client       *http.Client
}

func (b *Hashbin) Name() string     { return b.ProviderName }
func (b *Hashbin) Kind() string     { return "hashbin" }
func (b *Hashbin) Describe() string { return b.ContentURL + " (save via " + b.APIURL + ")" }
func (b *Hashbin) CanSave() bool    { return true }

func (b *Hashbin) reader() *HTTP {
	return &HTTP{ProviderName: b.ProviderName, URL: b.ContentURL, Client: b.Client}
}

func (b *Hashbin) Location(cid string) string { return b.reader().Location(cid) }

func (b *Hashbin) Load(ctx context.Context, cid string) (io.ReadCloser, error) {
	return b.reader().Load(ctx, cid)
}

func (b *Hashbin) Has(ctx context.Context, cid string) (bool, error) {
	return b.reader().Has(ctx, cid)
}

func (b *Hashbin) Save(ctx context.Context, cid string, content io.Reader, size int64, opts SaveOptions) (SaveResult, error) {
	res := SaveResult{Location: b.Location(cid)}
	if b.APIKey == "" {
		env := b.APIKeyEnv
		if env == "" {
			env = "T256_HASHBIN_API_KEY"
		}
		return res, fmt.Errorf("saving to %s needs a hashbin API key with the content:write scope: "+
			"create one at https://hashbin.org/api-keys.html and set %s (or api_key in the config file)", b.ProviderName, env)
	}
	q := url.Values{}
	if opts.RetentionMonths > 0 {
		q.Set("retention_months", strconv.Itoa(opts.RetentionMonths))
	}
	if opts.MaxCostCents >= 0 {
		q.Set("max_cost_cents", strconv.FormatInt(opts.MaxCostCents, 10))
	}
	u := strings.TrimRight(b.APIURL, "/") + "/content"
	if len(q) > 0 {
		u += "?" + q.Encode()
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, u, content)
	if err != nil {
		return res, err
	}
	req.ContentLength = size
	req.Header.Set("User-Agent", UserAgent)
	req.Header.Set("Authorization", "ApiKey "+b.APIKey)
	req.Header.Set("Content-Type", contentType(opts))
	req.Header.Set("Accept", "application/json")
	client := b.Client
	if client == nil {
		client = http.DefaultClient
	}
	resp, err := client.Do(req)
	if err != nil {
		return res, err
	}
	defer resp.Body.Close()
	if resp.StatusCode/100 != 2 {
		hint := ""
		switch resp.StatusCode {
		case http.StatusUnauthorized:
			hint = " (check the API key)"
		case http.StatusPaymentRequired:
			hint = " (add funds to your hashbin wallet)"
		case http.StatusForbidden:
			hint = " (the API key lacks content:write or a key limit was hit)"
		case http.StatusRequestEntityTooLarge:
			hint = " (hashbin accepts at most 90 MB per upload)"
		}
		return res, fmt.Errorf("hashbin upload failed: %s%s%s", resp.Status, errorDetail(resp.Body), hint)
	}
	var body map[string]any
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		return res, fmt.Errorf("hashbin upload: unreadable response: %v", err)
	}
	res.Details = body
	if got, _ := body["cid"].(string); got != "" && got != cid {
		return res, fmt.Errorf("hashbin stored CID %s, expected %s", got, cid)
	}
	if u, _ := body["url"].(string); u != "" {
		res.Location = u
	}
	return res, nil
}

// NewClient returns an HTTP client whose connection setup and wait for
// response headers are bounded by timeout (0 = no limit). Transfers of large
// bodies are not cut off once they have started.
func NewClient(timeout time.Duration) *http.Client {
	t := http.DefaultTransport.(*http.Transport).Clone()
	if timeout > 0 {
		t.TLSHandshakeTimeout = timeout
		t.ResponseHeaderTimeout = timeout
		t.DialContext = (&net.Dialer{Timeout: timeout, KeepAlive: 30 * time.Second}).DialContext
	}
	return &http.Client{Transport: t}
}
