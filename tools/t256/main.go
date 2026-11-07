// Command t256 loads and saves 256t content-addressed data using local
// directories, network drives, 256t.org, hashbin.org, or any other
// 256t-compatible server.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/curtcox/256t.org/tools/t256/cid"
	"github.com/curtcox/256t.org/tools/t256/provider"
)

var version = "dev"

// Exit codes.
const (
	exitOK       = 0
	exitError    = 1
	exitUsage    = 2
	exitNotFound = 3
	exitMismatch = 4
)

type app struct {
	stdin  io.Reader
	stdout io.Writer
	stderr io.Writer
	getenv func(string) string
	client *http.Client

	quiet   bool
	verbose bool
}

func main() {
	a := &app{stdin: os.Stdin, stdout: os.Stdout, stderr: os.Stderr, getenv: os.Getenv}
	os.Exit(a.run(os.Args[1:]))
}

// exitErr carries an exit code with an error.
type exitErr struct {
	code int
	err  error
}

func (e *exitErr) Error() string { return e.err.Error() }

func fail(code int, format string, args ...any) error {
	return &exitErr{code, fmt.Errorf(format, args...)}
}

func (a *app) run(args []string) int {
	if len(args) == 0 {
		fmt.Fprint(a.stderr, usage)
		return exitUsage
	}
	cmd, rest := args[0], args[1:]
	commands := map[string]func([]string) error{
		"cid":       a.cmdCID,
		"save":      a.cmdSave,
		"put":       a.cmdSave,
		"load":      a.cmdLoad,
		"get":       a.cmdLoad,
		"verify":    a.cmdVerify,
		"has":       a.cmdHas,
		"info":      a.cmdInfo,
		"providers": a.cmdProviders,
		"config":    a.cmdConfig,
	}
	switch cmd {
	case "help", "-h", "-help", "--help":
		if len(rest) > 0 && commands[rest[0]] != nil {
			return a.run([]string{rest[0], "--help"})
		}
		fmt.Fprint(a.stdout, usage)
		return exitOK
	case "version", "-version", "--version":
		fmt.Fprintf(a.stdout, "t256 %s\n", version)
		return exitOK
	}
	f := commands[cmd]
	if f == nil {
		fmt.Fprintf(a.stderr, "t256: unknown command %q\n\n%s", cmd, usage)
		return exitUsage
	}
	err := f(rest)
	if err == nil {
		return exitOK
	}
	if errors.Is(err, flag.ErrHelp) {
		return exitOK
	}
	var ee *exitErr
	if errors.As(err, &ee) {
		if ee.code == exitUsage {
			fmt.Fprintf(a.stderr, "t256 %s: %v\n", cmd, ee.err)
		} else if !a.quiet || ee.code == exitError {
			fmt.Fprintf(a.stderr, "t256: %v\n", ee.err)
		}
		return ee.code
	}
	fmt.Fprintf(a.stderr, "t256: %v\n", err)
	return exitError
}

const usage = `t256 — load and save 256t content-addressed data

Usage:
  t256 save   [FILE...]        store content at the destination, print its reference
  t256 load   REF... [-o FILE] fetch content from the sources, verify it, write it
  t256 cid    [FILE...]        compute references without storing anything
  t256 verify REF [FILE]       check that content matches a reference
  t256 has    REF...           check which sources hold content
  t256 info   REF...           describe a reference (length, inline, media type)
  t256 providers               list providers, sources and the destination
  t256 config [path|show|init] manage the config file
  t256 version

FILE "-" or no FILE means standard input. A REF may be a bare CID, a t256:
URI, a 256t:CID, or an http(s) URL ending in a CID, with an optional media
type suffix such as ".png".

Run "t256 help COMMAND" for a command's options.
Docs: https://github.com/curtcox/256t.org/tree/main/tools/t256
`

// ------------------------------------------------------------------ flags

type listFlag []string

func (l *listFlag) String() string { return strings.Join(*l, ",") }
func (l *listFlag) Set(v string) error {
	*l = append(*l, splitList(v)...)
	return nil
}

type common struct {
	config  string
	timeout string
	json    bool
	format  string
	ext     string
	from    listFlag
	to      string
}

func (a *app) flags(name, synopsis string, c *common, want string) *flag.FlagSet {
	fs := flag.NewFlagSet(name, flag.ContinueOnError)
	fs.SetOutput(a.stderr)
	fs.Usage = func() {
		fmt.Fprintf(a.stderr, "Usage: t256 %s\n\nOptions:\n", synopsis)
		fs.PrintDefaults()
	}
	fs.StringVar(&c.config, "config", "", "config file (default: $T256_CONFIG or the per-user config file)")
	fs.BoolVar(&a.quiet, "q", false, "quiet: no notices on stderr")
	fs.BoolVar(&a.quiet, "quiet", false, "same as -q")
	fs.BoolVar(&a.verbose, "v", false, "verbose: log each provider attempt on stderr")
	fs.BoolVar(&a.verbose, "verbose", false, "same as -v")
	if strings.Contains(want, "n") {
		fs.StringVar(&c.timeout, "timeout", "", "network timeout for connecting and awaiting a response, e.g. 30s (0 = none; default 60s)")
	}
	if strings.Contains(want, "j") {
		fs.BoolVar(&c.json, "json", false, "print one JSON object per item")
	}
	if strings.Contains(want, "f") {
		fs.StringVar(&c.format, "format", "cid", "output form: cid (alias 256t), t256, url, or json")
		fs.StringVar(&c.format, "f", "cid", "same as -format")
		fs.StringVar(&c.ext, "ext", "", `media type suffix to add, e.g. png or tar.gz; "auto" uses the file's extension`)
	}
	if strings.Contains(want, "s") {
		fs.Var(&c.from, "from", "source to use instead of the configured ones; repeatable or comma-separated (name, URL or directory)")
	}
	if strings.Contains(want, "d") {
		fs.StringVar(&c.to, "to", "", "destination instead of the configured one (name, URL or directory)")
	}
	return fs
}

// parse allows flags before, between and after positional arguments.
func parse(fs *flag.FlagSet, args []string) ([]string, error) {
	var pos, tail []string
	for i, arg := range args {
		if arg == "--" {
			args, tail = args[:i], args[i+1:]
			break
		}
	}
	for {
		if err := fs.Parse(args); err != nil {
			if errors.Is(err, flag.ErrHelp) {
				return nil, err
			}
			return nil, &exitErr{exitUsage, err}
		}
		args = fs.Args()
		if len(args) == 0 {
			return append(pos, tail...), nil
		}
		pos = append(pos, args[0])
		args = args[1:]
	}
}

func (a *app) setup(c *common) (*Config, error) {
	cfg, err := a.loadConfig(c.config)
	if err != nil {
		return nil, err
	}
	if c.timeout != "" {
		d, err := time.ParseDuration(c.timeout)
		if c.timeout == "0" {
			d, err = 0, nil
		}
		if err != nil {
			return nil, fail(exitUsage, "invalid --timeout %q", c.timeout)
		}
		cfg.timeout = d
	}
	a.client = provider.NewClient(cfg.timeout)
	if c.format == "json" {
		c.json = true
	}
	switch c.format {
	case "", "cid", "256t", "t256", "url", "json":
	default:
		return nil, fail(exitUsage, "unknown --format %q (use cid, 256t, t256, url or json)", c.format)
	}
	return cfg, nil
}

func (a *app) logf(format string, args ...any) {
	if a.verbose {
		fmt.Fprintf(a.stderr, "t256: "+format+"\n", args...)
	}
}

func (a *app) notef(format string, args ...any) {
	if !a.quiet {
		fmt.Fprintf(a.stderr, "t256: "+format+"\n", args...)
	}
}

func (a *app) sources(cfg *Config, c *common) ([]provider.Provider, error) {
	specs := []string(c.from)
	if len(specs) == 0 {
		specs = cfg.Sources
	}
	var out []provider.Provider
	for _, s := range specs {
		p, _, err := a.resolve(cfg, s, false)
		if err != nil {
			return nil, &exitErr{exitUsage, err}
		}
		out = append(out, p)
	}
	return out, nil
}

func (a *app) destination(cfg *Config, c *common) (provider.Provider, provider.SaveOptions, error) {
	spec := firstNonEmpty(c.to, cfg.Destination)
	p, opts, err := a.resolve(cfg, spec, true)
	if err != nil {
		return nil, opts, &exitErr{exitUsage, err}
	}
	return p, opts, nil
}

// ----------------------------------------------------------------- output

type record struct {
	CID       string `json:"cid"`
	URI       string `json:"uri"`
	Length    int64  `json:"length"`
	Inline    bool   `json:"inline"`
	Suffix    string `json:"suffix,omitempty"`
	MediaType string `json:"media_type,omitempty"`
	File      string `json:"file,omitempty"`
	Provider  string `json:"provider,omitempty"`
	Location  string `json:"location,omitempty"`
	Stored    *bool  `json:"stored,omitempty"`
	Found     *bool  `json:"found,omitempty"`
	Verified  *bool  `json:"verified,omitempty"`
	Computed  string `json:"computed,omitempty"`
	Error     string `json:"error,omitempty"`

	Details map[string]any `json:"details,omitempty"`
}

func newRecord(ref cid.Ref) record {
	return record{
		CID: ref.CID, URI: ref.URI(), Length: ref.Length(), Inline: ref.IsInline(),
		Suffix: ref.Suffix, MediaType: ref.MediaType(),
	}
}

func boolp(b bool) *bool { return &b }

func (a *app) emit(c *common, r record, label string) {
	if c.json {
		data, _ := json.Marshal(r)
		fmt.Fprintln(a.stdout, string(data))
		return
	}
	var s string
	switch c.format {
	case "t256":
		s = r.URI
	case "url":
		s = r.Location
		if s == "" {
			s = r.URI
		}
	default:
		s = r.CID + r.Suffix
	}
	if label != "" {
		s += "  " + label
	}
	fmt.Fprintln(a.stdout, s)
}

func suffixFor(ext, file string) (string, error) {
	switch {
	case ext == "":
		return "", nil
	case ext == "auto":
		if file == "" || file == "-" {
			return "", nil
		}
		return filepath.Ext(file), nil
	}
	s := "." + strings.TrimPrefix(ext, ".")
	if _, err := cid.ParseRef(cid.EncodeLength(0) + s); err != nil {
		return "", fail(exitUsage, "invalid --ext %q", ext)
	}
	return s, nil
}

func parseRef(s string) (cid.Ref, error) {
	ref, err := cid.ParseRef(s)
	if err != nil {
		return ref, fail(exitUsage, "invalid reference %q: %v", s, err)
	}
	return ref, nil
}

// ------------------------------------------------------------------ input

// openInput returns a seekable reader over a file or standard input
// (spooled to a temporary file), and a cleanup function.
func (a *app) openInput(name string) (*os.File, func(), error) {
	if name != "-" {
		f, err := os.Open(name)
		if err != nil {
			return nil, nil, err
		}
		if info, err := f.Stat(); err == nil && info.Mode().IsRegular() {
			return f, func() { f.Close() }, nil
		}
		return a.spoolInput(f, func() { f.Close() })
	}
	return a.spoolInput(a.stdin, func() {})
}

func (a *app) spoolInput(r io.Reader, closeSrc func()) (*os.File, func(), error) {
	defer closeSrc()
	tmp, err := os.CreateTemp("", "t256-in-*")
	if err != nil {
		return nil, nil, err
	}
	cleanup := func() { tmp.Close(); os.Remove(tmp.Name()) }
	if _, err := io.Copy(tmp, r); err != nil {
		cleanup()
		return nil, nil, err
	}
	if _, err := tmp.Seek(0, io.SeekStart); err != nil {
		cleanup()
		return nil, nil, err
	}
	return tmp, cleanup, nil
}

func inputs(args []string) []string {
	if len(args) == 0 {
		return []string{"-"}
	}
	return args
}

// --------------------------------------------------------------- commands

func (a *app) cmdCID(args []string) error {
	var c common
	fs := a.flags("cid", "cid [options] [FILE...]", &c, "jfd")
	args, err := parse(fs, args)
	if err != nil {
		return err
	}
	cfg, err := a.setup(&c)
	if err != nil {
		return err
	}
	var dest provider.Provider
	if c.format == "url" {
		if dest, _, err = a.destination(cfg, &c); err != nil {
			return err
		}
	}
	files := inputs(args)
	for _, name := range files {
		var r io.Reader = a.stdin
		if name != "-" {
			f, err := os.Open(name)
			if err != nil {
				return err
			}
			defer f.Close()
			r = f
		}
		id, _, err := cid.ComputeReader(r)
		if err != nil {
			return fmt.Errorf("%s: %v", name, err)
		}
		suffix, err := suffixFor(c.ext, name)
		if err != nil {
			return err
		}
		rec := newRecord(cid.Ref{CID: id, Suffix: suffix})
		if name != "-" {
			rec.File = name
		}
		if dest != nil && !rec.Inline {
			rec.Provider, rec.Location = dest.Name(), dest.Location(id)
		}
		label := ""
		if len(files) > 1 {
			label = name
		}
		a.emit(&c, rec, label)
	}
	return nil
}

func (a *app) cmdSave(args []string) error {
	var c common
	var ctype string
	var retention int
	var maxCost int64
	var skipExisting, storeInline, dryRun bool
	fs := a.flags("save", "save [options] [FILE...]", &c, "njfd")
	fs.StringVar(&ctype, "type", "", "media type to store with the content (default: from the file name or -ext)")
	fs.IntVar(&retention, "retention-months", 0, "hashbin: whole months to keep the content (default: provider setting)")
	fs.Int64Var(&maxCost, "max-cost-cents", -1, "hashbin: refuse an upload that would cost more than this many cents")
	fs.BoolVar(&skipExisting, "skip-existing", false, "do not upload content the destination already reports having")
	fs.BoolVar(&storeInline, "store-inline", false, "store content of 64 bytes or less too (its CID already contains it)")
	fs.BoolVar(&dryRun, "dry-run", false, "compute references and show the destination without storing anything")
	fs.BoolVar(&dryRun, "n", false, "same as -dry-run")
	args, err := parse(fs, args)
	if err != nil {
		return err
	}
	cfg, err := a.setup(&c)
	if err != nil {
		return err
	}
	dest, opts, err := a.destination(cfg, &c)
	if err != nil {
		return err
	}
	if retention > 0 {
		opts.RetentionMonths = retention
	}
	if maxCost >= 0 {
		opts.MaxCostCents = maxCost
	}
	if !dest.CanSave() && !dryRun {
		return fail(exitUsage, "destination %q is read-only; choose another with --to or in the config file", dest.Name())
	}
	files := inputs(args)
	failed := 0
	for _, name := range files {
		rec, err := a.saveOne(&c, dest, opts, name, ctype, skipExisting, storeInline, dryRun)
		label := ""
		if len(files) > 1 {
			label = name
		}
		if err != nil {
			if len(files) == 1 {
				return err
			}
			failed++
			fmt.Fprintf(a.stderr, "t256: %s: %v\n", name, err)
			if c.json {
				rec.Error = err.Error()
				a.emit(&c, rec, label)
			}
			continue
		}
		a.emit(&c, rec, label)
	}
	if failed > 0 {
		return &exitErr{exitError, fmt.Errorf("%d of %d saves failed", failed, len(files))}
	}
	return nil
}

func (a *app) saveOne(c *common, dest provider.Provider, opts provider.SaveOptions, name, ctype string,
	skipExisting, storeInline, dryRun bool) (record, error) {
	rec := record{File: name, Provider: dest.Name()}
	if name == "-" {
		rec.File = ""
	}
	f, cleanup, err := a.openInput(name)
	if err != nil {
		return rec, err
	}
	defer cleanup()
	id, size, err := cid.ComputeReader(f)
	if err != nil {
		return rec, err
	}
	if size > cid.MaxContentLen {
		return rec, fmt.Errorf("content is too large for a 256t CID")
	}
	suffix, err := suffixFor(c.ext, name)
	if err != nil {
		return rec, err
	}
	ref := cid.Ref{CID: id, Suffix: suffix}
	base := newRecord(ref)
	base.File, base.Provider = rec.File, rec.Provider
	rec = base
	rec.Location = dest.Location(id)
	opts.ContentType = firstNonEmpty(ctype, cid.MediaTypeForSuffix(filepath.Base(name)), ref.MediaType())
	if name == "-" {
		opts.ContentType = firstNonEmpty(ctype, ref.MediaType())
	}
	if ref.IsInline() && !storeInline {
		rec.Location, rec.Stored = "", boolp(false)
		a.notef("%d bytes: the CID contains the content, so nothing was stored (use --store-inline to store it anyway)", size)
		return rec, nil
	}
	if dryRun {
		rec.Stored = boolp(false)
		a.notef("dry run: would save %d bytes to %s", size, rec.Location)
		return rec, nil
	}
	ctx := context.Background()
	if skipExisting {
		has, err := dest.Has(ctx, id)
		if err != nil {
			a.logf("%s: existence check failed: %v", dest.Name(), err)
		}
		if has {
			rec.Stored = boolp(false)
			a.logf("%s already has %s", dest.Name(), id)
			return rec, nil
		}
	}
	if _, err := f.Seek(0, io.SeekStart); err != nil {
		return rec, err
	}
	a.logf("saving %d bytes to %s", size, rec.Location)
	res, err := dest.Save(ctx, id, f, size, opts)
	if err != nil {
		return rec, fmt.Errorf("%s: %w", dest.Name(), err)
	}
	if res.Location != "" {
		rec.Location = res.Location
	}
	rec.Stored, rec.Details = boolp(true), res.Details
	return rec, nil
}

func (a *app) cmdLoad(args []string) error {
	var c common
	var output, outDir string
	var cache bool
	fs := a.flags("load", "load [options] REF... [-o FILE | -O DIR]", &c, "njsd")
	fs.StringVar(&output, "o", "", "write content to FILE instead of standard output (a directory means DIR/CID.suffix)")
	fs.StringVar(&output, "output", "", "same as -o")
	fs.StringVar(&outDir, "O", "", "write each REF to DIR/CID.suffix and print the paths (allows several REFs)")
	fs.StringVar(&outDir, "output-dir", "", "same as -O")
	fs.BoolVar(&cache, "cache", false, "also save verified content to the destination (e.g. the local store)")
	args, err := parse(fs, args)
	if err != nil {
		return err
	}
	if len(args) == 0 {
		return fail(exitUsage, "need a REF to load")
	}
	if output != "" && output != "-" {
		if info, err := os.Stat(output); err == nil && info.IsDir() {
			outDir, output = output, ""
		}
	}
	if len(args) > 1 && outDir == "" {
		return fail(exitUsage, "several REFs need -O DIR")
	}
	if outDir != "" && output != "" {
		return fail(exitUsage, "use either -o or -O, not both")
	}
	toStdout := outDir == "" && (output == "" || output == "-")
	if toStdout && c.json {
		return fail(exitUsage, "--json needs -o or -O, since the content goes to standard output")
	}
	cfg, err := a.setup(&c)
	if err != nil {
		return err
	}
	srcs, err := a.sources(cfg, &c)
	if err != nil {
		return err
	}
	var dest provider.Provider
	var destOpts provider.SaveOptions
	if cache {
		if dest, destOpts, err = a.destination(cfg, &c); err != nil {
			return err
		}
	}
	worst := exitOK
	for _, arg := range args {
		ref, err := parseRef(arg)
		if err != nil {
			return err
		}
		target := output
		if outDir != "" {
			target = filepath.Join(outDir, ref.CID+ref.Suffix)
		}
		rec, err := a.loadOne(ref, srcs, target, toStdout, dest, destOpts)
		if err != nil {
			if len(args) == 1 {
				return err
			}
			code := exitError
			var ee *exitErr
			if errors.As(err, &ee) {
				code = ee.code
			}
			if code > worst || worst == exitOK {
				worst = code
			}
			fmt.Fprintf(a.stderr, "t256: %s: %v\n", arg, err)
			if c.json {
				rec.Error = err.Error()
				a.emit(&c, rec, "")
			}
			continue
		}
		if c.json {
			a.emit(&c, rec, "")
		} else if outDir != "" {
			fmt.Fprintln(a.stdout, target)
		}
	}
	if worst != exitOK {
		return &exitErr{worst, errors.New("some references could not be loaded")}
	}
	return nil
}

func (a *app) loadOne(ref cid.Ref, srcs []provider.Provider, target string, toStdout bool,
	dest provider.Provider, destOpts provider.SaveOptions) (record, error) {
	rec := newRecord(ref)
	rec.File = target
	tmpDir := ""
	if !toStdout {
		tmpDir = filepath.Dir(target)
	}
	var tmp *os.File
	if ref.IsInline() {
		data, _ := ref.Inline()
		f, err := os.CreateTemp(tmpDir, ".t256-*.tmp")
		if err != nil {
			return rec, err
		}
		tmp = f
		if _, err := f.Write(data); err != nil {
			f.Close()
			os.Remove(f.Name())
			return rec, err
		}
		rec.Provider, rec.Verified = "inline", boolp(true)
	} else {
		f, p, loc, err := a.fetch(ref, srcs, tmpDir)
		if err != nil {
			return rec, err
		}
		tmp = f
		rec.Provider, rec.Location, rec.Verified = p, loc, boolp(true)
	}
	defer func() { tmp.Close(); os.Remove(tmp.Name()) }()

	if dest != nil && !ref.IsInline() && dest.Name() != rec.Provider {
		if has, _ := dest.Has(context.Background(), ref.CID); !has {
			if _, err := tmp.Seek(0, io.SeekStart); err != nil {
				return rec, err
			}
			destOpts.ContentType = ref.MediaType()
			if _, err := dest.Save(context.Background(), ref.CID, tmp, ref.Length(), destOpts); err != nil {
				a.notef("could not cache in %s: %v", dest.Name(), err)
			} else {
				a.logf("cached in %s", dest.Name())
			}
		}
	}
	if toStdout {
		if _, err := tmp.Seek(0, io.SeekStart); err != nil {
			return rec, err
		}
		_, err := io.Copy(a.stdout, tmp)
		return rec, err
	}
	if err := tmp.Close(); err != nil {
		return rec, err
	}
	if err := os.Chmod(tmp.Name(), 0o644); err != nil {
		return rec, err
	}
	if err := os.Rename(tmp.Name(), target); err != nil {
		// Windows cannot rename over an existing file.
		if rmErr := os.Remove(target); rmErr == nil {
			err = os.Rename(tmp.Name(), target)
		}
		if err != nil {
			return rec, err
		}
	}
	return rec, nil
}

// fetch tries the reference's own URL and then each source, returning a
// temporary file holding verified content.
func (a *app) fetch(ref cid.Ref, srcs []provider.Provider, tmpDir string) (*os.File, string, string, error) {
	type attempt struct {
		name, loc string
		open      func(context.Context) (io.ReadCloser, error)
	}
	var attempts []attempt
	if ref.URL != "" {
		attempts = append(attempts, attempt{ref.URL, ref.URL, func(ctx context.Context) (io.ReadCloser, error) {
			return provider.LoadURL(ctx, a.client, ref.URL)
		}})
	}
	for _, p := range srcs {
		p := p
		attempts = append(attempts, attempt{p.Name(), p.Location(ref.CID), func(ctx context.Context) (io.ReadCloser, error) {
			return p.Load(ctx, ref.CID)
		}})
	}
	want := ref.Length()
	var problems []string
	mismatches, errs := 0, 0
	for _, at := range attempts {
		rc, err := at.open(context.Background())
		if errors.Is(err, provider.ErrNotFound) {
			a.logf("%s: not found", at.name)
			continue
		}
		if err != nil {
			a.logf("%s: %v", at.name, err)
			problems = append(problems, fmt.Sprintf("%s: %v", at.name, err))
			errs++
			continue
		}
		tmp, err := os.CreateTemp(tmpDir, ".t256-*.tmp")
		if err != nil {
			rc.Close()
			return nil, "", "", err
		}
		h := cid.NewHasher()
		_, err = io.Copy(io.MultiWriter(tmp, h), io.LimitReader(rc, want+1))
		rc.Close()
		if err == nil && h.CID() == ref.CID {
			if _, err = tmp.Seek(0, io.SeekStart); err == nil {
				a.logf("%s: verified %d bytes", at.name, want)
				return tmp, at.name, at.loc, nil
			}
		}
		tmp.Close()
		os.Remove(tmp.Name())
		if err != nil {
			a.logf("%s: %v", at.name, err)
			problems = append(problems, fmt.Sprintf("%s: %v", at.name, err))
			errs++
			continue
		}
		a.notef("%s returned content that does not match the CID; ignoring it", at.name)
		problems = append(problems, at.name+": content did not match the CID")
		mismatches++
	}
	names := make([]string, len(attempts))
	for i, at := range attempts {
		names[i] = at.name
	}
	switch {
	case len(attempts) == 0:
		return nil, "", "", fail(exitUsage, "no sources configured")
	case mismatches > 0 && errs == 0:
		return nil, "", "", fail(exitMismatch, "no source returned matching content (%s)", strings.Join(problems, "; "))
	case errs > 0:
		return nil, "", "", fail(exitError, "%s not loaded (%s)", ref.CID, strings.Join(problems, "; "))
	}
	return nil, "", "", fail(exitNotFound, "%s not found in %s", ref.CID, strings.Join(names, ", "))
}

func (a *app) cmdVerify(args []string) error {
	var c common
	fs := a.flags("verify", "verify [options] REF [FILE]", &c, "j")
	args, err := parse(fs, args)
	if err != nil {
		return err
	}
	if len(args) < 1 || len(args) > 2 {
		return fail(exitUsage, "need REF and at most one FILE")
	}
	ref, err := parseRef(args[0])
	if err != nil {
		return err
	}
	name := "-"
	if len(args) == 2 {
		name = args[1]
	}
	var r io.Reader = a.stdin
	if name != "-" {
		f, err := os.Open(name)
		if err != nil {
			return err
		}
		defer f.Close()
		r = f
	}
	got, _, err := cid.ComputeReader(r)
	if err != nil {
		return err
	}
	rec := newRecord(ref)
	if name != "-" {
		rec.File = name
	}
	rec.Computed, rec.Verified = got, boolp(got == ref.CID)
	if c.json {
		a.emit(&c, rec, "")
	} else if got == ref.CID && !a.quiet {
		fmt.Fprintln(a.stdout, "OK")
	} else if got != ref.CID && !a.quiet {
		fmt.Fprintf(a.stdout, "MISMATCH: content has CID %s\n", got)
	}
	if got != ref.CID {
		return &exitErr{exitMismatch, errors.New("content does not match " + ref.CID)}
	}
	return nil
}

func (a *app) cmdHas(args []string) error {
	var c common
	var all bool
	fs := a.flags("has", "has [options] REF...", &c, "njfs")
	fs.BoolVar(&all, "all", false, "check every source and print one line per source")
	args, err := parse(fs, args)
	if err != nil {
		return err
	}
	if len(args) == 0 {
		return fail(exitUsage, "need a REF")
	}
	cfg, err := a.setup(&c)
	if err != nil {
		return err
	}
	srcs, err := a.sources(cfg, &c)
	if err != nil {
		return err
	}
	missing := 0
	ctx := context.Background()
	for _, arg := range args {
		ref, err := parseRef(arg)
		if err != nil {
			return err
		}
		found := false
		if ref.IsInline() && !all {
			rec := newRecord(ref)
			rec.Provider, rec.Found = "inline", boolp(true)
			a.emitHas(&c, rec, len(args) > 1, all)
			continue
		}
		for _, p := range srcs {
			rec := newRecord(ref)
			rec.Provider, rec.Location = p.Name(), p.Location(ref.CID)
			has, err := p.Has(ctx, ref.CID)
			rec.Found = boolp(has)
			if err != nil {
				rec.Error = err.Error()
				a.logf("%s: %v", p.Name(), err)
			}
			if all {
				a.emitHas(&c, rec, true, true)
			} else if has {
				a.emitHas(&c, rec, len(args) > 1, false)
			}
			if has {
				found = true
				if !all {
					break
				}
			}
		}
		if !found && !(ref.IsInline()) {
			missing++
			if !all {
				a.notef("%s not found", ref.CID)
			}
		}
	}
	if missing > 0 {
		return &exitErr{exitNotFound, fmt.Errorf("%d of %d not found", missing, len(args))}
	}
	return nil
}

func (a *app) emitHas(c *common, rec record, label, all bool) {
	if c.json {
		a.emit(c, rec, "")
		return
	}
	if all {
		status := "no"
		if rec.Found != nil && *rec.Found {
			status = "yes"
		}
		if rec.Error != "" {
			status = "error: " + rec.Error
		}
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\t%s\n", rec.CID+rec.Suffix, rec.Provider, status, rec.Location)
		return
	}
	l := ""
	if label {
		l = rec.CID + rec.Suffix
	}
	if c.format == "url" || c.format == "" || c.format == "cid" {
		cc := *c
		cc.format = "url"
		a.emit(&cc, rec, l)
		return
	}
	a.emit(c, rec, l)
}

func (a *app) cmdInfo(args []string) error {
	var c common
	fs := a.flags("info", "info [options] REF...", &c, "j")
	args, err := parse(fs, args)
	if err != nil {
		return err
	}
	if len(args) == 0 {
		return fail(exitUsage, "need a REF")
	}
	for i, arg := range args {
		ref, err := parseRef(arg)
		if err != nil {
			return err
		}
		rec := newRecord(ref)
		if c.json {
			a.emit(&c, rec, "")
			continue
		}
		if i > 0 {
			fmt.Fprintln(a.stdout)
		}
		fmt.Fprintf(a.stdout, "cid:        %s\nuri:        %s\nlength:     %d\ninline:     %t\n",
			rec.CID, rec.URI, rec.Length, rec.Inline)
		if rec.Suffix != "" {
			fmt.Fprintf(a.stdout, "suffix:     %s\nmedia type: %s\n", rec.Suffix, firstNonEmpty(rec.MediaType, "(unrecognized)"))
		}
		if ref.URL != "" {
			fmt.Fprintf(a.stdout, "url:        %s\n", ref.URL)
		}
	}
	return nil
}

func (a *app) cmdProviders(args []string) error {
	var c common
	fs := a.flags("providers", "providers [options]", &c, "j")
	if _, err := parse(fs, args); err != nil {
		return err
	}
	cfg, err := a.setup(&c)
	if err != nil {
		return err
	}
	type row struct {
		Name        string `json:"name"`
		Kind        string `json:"kind"`
		Where       string `json:"where"`
		CanSave     bool   `json:"can_save"`
		Source      int    `json:"source,omitempty"` // 1-based order, 0 = not a source
		Destination bool   `json:"destination"`
		Error       string `json:"error,omitempty"`
	}
	order := map[string]int{}
	for i, s := range cfg.Sources {
		order[s] = i + 1
	}
	var rows []row
	seen := map[string]bool{}
	addRow := func(spec string) {
		if seen[spec] {
			return
		}
		seen[spec] = true
		r := row{Name: spec, Source: order[spec], Destination: spec == cfg.Destination}
		p, _, err := a.resolve(cfg, spec, spec == cfg.Destination)
		if err != nil {
			r.Error = err.Error()
		} else {
			r.Kind, r.Where, r.CanSave = p.Kind(), p.Describe(), p.CanSave()
		}
		rows = append(rows, r)
	}
	for _, s := range cfg.Sources {
		addRow(s)
	}
	addRow(cfg.Destination)
	for _, n := range providerNames(cfg) {
		addRow(n)
	}
	for _, r := range rows {
		if c.json {
			data, _ := json.Marshal(r)
			fmt.Fprintln(a.stdout, string(data))
			continue
		}
		var roles []string
		if r.Source > 0 {
			roles = append(roles, fmt.Sprintf("source %d", r.Source))
		}
		if r.Destination {
			roles = append(roles, "destination")
		}
		mode := "read-only"
		if r.CanSave {
			mode = "read-write"
		}
		where := r.Where
		if r.Error != "" {
			mode, where = "error", r.Error
		}
		fmt.Fprintf(a.stdout, "%-12s %-8s %-10s %-22s %s\n", r.Name, r.Kind, mode, strings.Join(roles, ", "), where)
	}
	return nil
}

func (a *app) cmdConfig(args []string) error {
	var c common
	var force bool
	fs := a.flags("config", "config [path|show|init] [options]", &c, "")
	fs.BoolVar(&force, "force", false, "init: overwrite an existing config file")
	args, err := parse(fs, args)
	if err != nil {
		return err
	}
	sub := "path"
	if len(args) > 0 {
		sub = args[0]
	}
	path := firstNonEmpty(c.config, a.getenv("T256_CONFIG"), a.defaultConfigPath())
	switch sub {
	case "path":
		fmt.Fprintln(a.stdout, path)
		return nil
	case "show":
		cfg, err := a.setup(&c)
		if err != nil {
			return err
		}
		out := map[string]any{
			"file": firstNonEmpty(cfg.path, "(none; using defaults)"), "sources": cfg.Sources,
			"destination": cfg.Destination, "timeout": cfg.timeout.String(), "providers": cfg.Providers,
		}
		data, _ := json.MarshalIndent(out, "", "  ")
		fmt.Fprintln(a.stdout, string(data))
		return nil
	case "init":
		if _, err := os.Stat(path); err == nil && !force {
			return fail(exitError, "%s already exists (use --force to overwrite)", path)
		}
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(path, []byte(sampleConfig), 0o600); err != nil {
			return err
		}
		fmt.Fprintln(a.stdout, path)
		return nil
	}
	return fail(exitUsage, "unknown config subcommand %q (use path, show or init)", sub)
}
