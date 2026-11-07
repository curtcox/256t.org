package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"time"

	"github.com/curtcox/256t.org/tools/t256/provider"
)

// ProviderConfig is one entry under "providers" in the config file.
type ProviderConfig struct {
	Type string `json:"type"` // "dir", "http" or "hashbin"

	// dir
	Path     string `json:"path,omitempty"`
	Create   *bool  `json:"create,omitempty"`
	ReadOnly bool   `json:"read_only,omitempty"`

	// http
	URL     string            `json:"url,omitempty"`
	Save    string            `json:"save,omitempty"` // "put" or "post"
	SaveURL string            `json:"save_url,omitempty"`
	Headers map[string]string `json:"headers,omitempty"`

	// hashbin
	ContentURL      string `json:"content_url,omitempty"`
	APIURL          string `json:"api_url,omitempty"`
	APIKey          string `json:"api_key,omitempty"`
	APIKeyEnv       string `json:"api_key_env,omitempty"`
	RetentionMonths int    `json:"retention_months,omitempty"`
	MaxCostCents    *int64 `json:"max_cost_cents,omitempty"`
}

// Config is the t256 config file.
type Config struct {
	Sources     []string                  `json:"sources,omitempty"`
	Destination string                    `json:"destination,omitempty"`
	Timeout     string                    `json:"timeout,omitempty"`
	Providers   map[string]ProviderConfig `json:"providers,omitempty"`

	path    string // file it was read from, "" if none
	timeout time.Duration
}

var (
	defaultSources     = []string{"local", "256t.org", "hashbin"}
	defaultDestination = "local"
	defaultTimeout     = 60 * time.Second
)

func (a *app) localStorePath() string {
	home, _ := os.UserHomeDir()
	switch runtime.GOOS {
	case "windows":
		if d := a.getenv("LOCALAPPDATA"); d != "" {
			return filepath.Join(d, "t256", "store")
		}
		return filepath.Join(home, "AppData", "Local", "t256", "store")
	case "darwin":
		return filepath.Join(home, "Library", "Application Support", "t256", "store")
	default:
		if d := a.getenv("XDG_DATA_HOME"); d != "" {
			return filepath.Join(d, "t256", "store")
		}
		return filepath.Join(home, ".local", "share", "t256", "store")
	}
}

func (a *app) builtinProviders() map[string]ProviderConfig {
	yes := true
	return map[string]ProviderConfig{
		"local":    {Type: "dir", Path: a.localStorePath(), Create: &yes},
		"256t.org": {Type: "http", URL: "https://256t.org"},
		"hashbin":  {Type: "hashbin"},
	}
}

var aliases = map[string]string{"hashbin.org": "hashbin", "256t": "256t.org"}

func (a *app) defaultConfigPath() string {
	dir, err := os.UserConfigDir()
	if err != nil {
		return ""
	}
	return filepath.Join(dir, "t256", "config.json")
}

// loadConfig reads the config file named by --config, T256_CONFIG, or the
// default location. A missing default file is not an error.
func (a *app) loadConfig(flagPath string) (*Config, error) {
	path, explicit := flagPath, flagPath != ""
	if path == "" {
		path = a.getenv("T256_CONFIG")
		explicit = path != ""
	}
	if path == "" {
		path = a.defaultConfigPath()
	}
	cfg := &Config{}
	if path != "" {
		data, err := os.ReadFile(path)
		switch {
		case err == nil:
			if err := json.Unmarshal(data, cfg); err != nil {
				return nil, fmt.Errorf("config %s: %v", path, err)
			}
			cfg.path = path
		case errors.Is(err, os.ErrNotExist) && !explicit:
		default:
			return nil, fmt.Errorf("config: %v", err)
		}
	}
	merged := a.builtinProviders()
	for name, p := range cfg.Providers {
		merged[name] = p
	}
	cfg.Providers = merged
	if len(cfg.Sources) == 0 {
		cfg.Sources = defaultSources
	}
	if v := a.getenv("T256_SOURCES"); v != "" {
		cfg.Sources = splitList(v)
	}
	if cfg.Destination == "" {
		cfg.Destination = defaultDestination
	}
	if v := a.getenv("T256_DESTINATION"); v != "" {
		cfg.Destination = v
	}
	cfg.timeout = defaultTimeout
	if v := firstNonEmpty(a.getenv("T256_TIMEOUT"), cfg.Timeout); v != "" {
		d, err := time.ParseDuration(v)
		if err != nil {
			return nil, fmt.Errorf("invalid timeout %q: %v", v, err)
		}
		cfg.timeout = d
	}
	return cfg, nil
}

func firstNonEmpty(vals ...string) string {
	for _, v := range vals {
		if v != "" {
			return v
		}
	}
	return ""
}

func splitList(s string) []string {
	var out []string
	for _, part := range strings.Split(s, ",") {
		if part = strings.TrimSpace(part); part != "" {
			out = append(out, part)
		}
	}
	return out
}

func (a *app) expand(s string) string {
	s = os.Expand(s, a.getenv)
	if s == "~" || strings.HasPrefix(s, "~/") || strings.HasPrefix(s, `~\`) {
		if home, err := os.UserHomeDir(); err == nil {
			s = filepath.Join(home, s[1:])
		}
	}
	return s
}

// resolve turns a provider spec into a Provider. A spec is a configured
// provider name, an http(s) base URL, or a directory path (optionally
// prefixed with "dir:"). forSave makes ad-hoc URLs use PUT.
func (a *app) resolve(cfg *Config, spec string, forSave bool) (provider.Provider, provider.SaveOptions, error) {
	opts := provider.SaveOptions{MaxCostCents: -1}
	name := spec
	if alias, ok := aliases[name]; ok {
		if _, configured := cfg.Providers[name]; !configured {
			name = alias
		}
	}
	if pc, ok := cfg.Providers[name]; ok {
		p, err := a.build(name, pc)
		if pc.Type == "hashbin" {
			opts.RetentionMonths = pc.RetentionMonths
			if pc.MaxCostCents != nil {
				opts.MaxCostCents = *pc.MaxCostCents
			}
		}
		return p, opts, err
	}
	lower := strings.ToLower(spec)
	if strings.HasPrefix(lower, "http://") || strings.HasPrefix(lower, "https://") {
		h := &provider.HTTP{ProviderName: spec, URL: spec, Client: a.client}
		if forSave {
			h.SaveMethod = "put"
		}
		return h, opts, nil
	}
	if strings.HasPrefix(lower, "dir:") || looksLikePath(spec) {
		path := strings.TrimPrefix(strings.TrimPrefix(spec, "dir:"), "DIR:")
		return &provider.Dir{ProviderName: spec, Path: a.expand(path)}, opts, nil
	}
	return nil, opts, fmt.Errorf("unknown provider %q (known: %s; or give a URL or directory path)",
		spec, strings.Join(providerNames(cfg), ", "))
}

func looksLikePath(s string) bool {
	if strings.ContainsAny(s, `/\`) || strings.HasPrefix(s, ".") || strings.HasPrefix(s, "~") {
		return true
	}
	if len(s) >= 2 && s[1] == ':' { // Windows drive letter
		return true
	}
	info, err := os.Stat(s)
	return err == nil && info.IsDir()
}

func providerNames(cfg *Config) []string {
	names := make([]string, 0, len(cfg.Providers))
	for n := range cfg.Providers {
		names = append(names, n)
	}
	sort.Strings(names)
	return names
}

func (a *app) build(name string, pc ProviderConfig) (provider.Provider, error) {
	switch pc.Type {
	case "dir", "directory", "file", "filesystem":
		if pc.Path == "" {
			return nil, fmt.Errorf("provider %q: dir providers need a path", name)
		}
		return &provider.Dir{
			ProviderName: name,
			Path:         a.expand(pc.Path),
			Create:       pc.Create != nil && *pc.Create,
			ReadOnly:     pc.ReadOnly,
		}, nil
	case "http", "https":
		if pc.URL == "" {
			return nil, fmt.Errorf("provider %q: http providers need a url", name)
		}
		headers := map[string]string{}
		for k, v := range pc.Headers {
			headers[k] = a.expand(v)
		}
		return &provider.HTTP{
			ProviderName: name,
			URL:          a.expand(pc.URL),
			SaveMethod:   pc.Save,
			SaveURL:      a.expand(pc.SaveURL),
			Headers:      headers,
			Client:       a.client,
		}, nil
	case "hashbin":
		keyEnv := firstNonEmpty(pc.APIKeyEnv, "T256_HASHBIN_API_KEY")
		return &provider.Hashbin{
			ProviderName: name,
			ContentURL:   firstNonEmpty(a.expand(pc.ContentURL), "https://256t.us"),
			APIURL:       firstNonEmpty(a.expand(pc.APIURL), "https://hashbin.org/api"),
			APIKey:       firstNonEmpty(a.expand(pc.APIKey), a.getenv(keyEnv)),
			APIKeyEnv:    keyEnv,
			Client:       a.client,
		}, nil
	case "":
		return nil, fmt.Errorf("provider %q has no type (use dir, http or hashbin)", name)
	}
	return nil, fmt.Errorf("provider %q: unknown type %q (use dir, http or hashbin)", name, pc.Type)
}

const sampleConfig = `{
  "sources": ["local", "256t.org", "hashbin"],
  "destination": "local",
  "timeout": "60s",
  "providers": {
    "nas": {
      "type": "dir",
      "path": "/Volumes/shared/256t"
    },
    "team": {
      "type": "http",
      "url": "https://cas.example.com/256t",
      "save": "put",
      "headers": { "Authorization": "Bearer ${TEAM_CAS_TOKEN}" }
    },
    "hashbin": {
      "type": "hashbin",
      "api_key_env": "T256_HASHBIN_API_KEY",
      "retention_months": 12,
      "max_cost_cents": 100
    }
  }
}
`
