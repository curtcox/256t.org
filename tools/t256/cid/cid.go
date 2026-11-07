// Package cid computes, parses and verifies 256t content identifiers (CIDs)
// and t256: URIs as defined at https://256t.org and in
// draft-cox-t256-uri-scheme.
package cid

import (
	"bytes"
	"crypto/sha512"
	"encoding/base64"
	"errors"
	"fmt"
	"hash"
	"io"
	"mime"
	"net/url"
	"strings"
)

const (
	// MaxInline is the largest content length carried inside the CID itself.
	MaxInline = 64
	// PrefixLen is the length of the base64url length prefix.
	PrefixLen = 8
	// HashLen is the length of a base64url SHA-512 hash.
	HashLen = 86
	// MaxLen is the longest possible CID.
	MaxLen = PrefixLen + HashLen
	// MaxContentLen is the largest length a 6-octet prefix can express.
	MaxContentLen = 1<<48 - 1
)

var b64 = base64.RawURLEncoding.Strict()

// EncodeLength returns the 8-character length prefix for n bytes.
func EncodeLength(n int64) string {
	var buf [6]byte
	for i := 5; i >= 0; i-- {
		buf[i] = byte(n)
		n >>= 8
	}
	return b64.EncodeToString(buf[:])
}

// Compute returns the CID of content.
func Compute(content []byte) string {
	if len(content) <= MaxInline {
		return EncodeLength(int64(len(content))) + b64.EncodeToString(content)
	}
	sum := sha512.Sum512(content)
	return EncodeLength(int64(len(content))) + b64.EncodeToString(sum[:])
}

// Hasher computes a CID incrementally. It implements io.Writer.
type Hasher struct {
	h    hash.Hash
	head bytes.Buffer
	n    int64
}

// NewHasher returns a Hasher ready for writing.
func NewHasher() *Hasher { return &Hasher{h: sha512.New()} }

func (c *Hasher) Write(p []byte) (int, error) {
	if room := MaxInline + 1 - c.head.Len(); room > 0 {
		if room > len(p) {
			room = len(p)
		}
		c.head.Write(p[:room])
	}
	c.n += int64(len(p))
	return c.h.Write(p)
}

// Len returns the number of bytes written so far.
func (c *Hasher) Len() int64 { return c.n }

// CID returns the CID of everything written so far.
func (c *Hasher) CID() string {
	if c.n <= MaxInline {
		return EncodeLength(c.n) + b64.EncodeToString(c.head.Bytes())
	}
	return EncodeLength(c.n) + b64.EncodeToString(c.h.Sum(nil))
}

// ComputeReader returns the CID of everything read from r and its length.
func ComputeReader(r io.Reader) (string, int64, error) {
	h := NewHasher()
	if _, err := io.Copy(h, r); err != nil {
		return "", 0, err
	}
	return h.CID(), h.Len(), nil
}

// Validate reports whether s is a well-formed CID.
func Validate(s string) error {
	if len(s) < PrefixLen {
		return fmt.Errorf("CID %q is too short: need at least %d characters", s, PrefixLen)
	}
	if len(s) > MaxLen {
		return fmt.Errorf("CID is too long: %d characters, at most %d allowed", len(s), MaxLen)
	}
	for i := 0; i < len(s); i++ {
		if !isB64(s[i]) {
			return fmt.Errorf("CID contains %q, which is not a base64url character", s[i])
		}
	}
	n, err := decodeLength(s[:PrefixLen])
	if err != nil {
		return err
	}
	payload := s[PrefixLen:]
	if n <= MaxInline {
		data, err := b64.DecodeString(payload)
		if err != nil {
			return fmt.Errorf("CID payload is not valid base64url: %v", err)
		}
		if int64(len(data)) != n {
			return fmt.Errorf("CID says %d bytes but its inline payload holds %d", n, len(data))
		}
		return nil
	}
	if len(payload) != HashLen {
		return fmt.Errorf("CID for %d bytes must have an %d-character hash, found %d", n, HashLen, len(payload))
	}
	if _, err := b64.DecodeString(payload); err != nil {
		return fmt.Errorf("CID hash is not valid base64url: %v", err)
	}
	return nil
}

func decodeLength(prefix string) (int64, error) {
	buf, err := b64.DecodeString(prefix)
	if err != nil || len(buf) != 6 {
		return 0, fmt.Errorf("CID length prefix %q is not valid", prefix)
	}
	var n int64
	for _, b := range buf {
		n = n<<8 | int64(b)
	}
	return n, nil
}

func isB64(c byte) bool {
	return c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '-' || c == '_'
}

func isAlnum(c byte) bool {
	return c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'
}

// Ref is a parsed reference to 256t content: a bare CID, a t256: URI, or a
// URL whose final path segment is a CID.
type Ref struct {
	CID    string // the content identifier
	Suffix string // media type suffix including the leading ".", or ""
	URL    string // original http(s) URL, when the reference was one
}

// ParseRef accepts any of:
//
//	AAAAAAANSGVsbG8sIFdvcmxkIQ            bare CID (the "256t" form)
//	AAAAAAANSGVsbG8sIFdvcmxkIQ.txt        bare CID with a suffix
//	t256:AAAAAAANSGVsbG8sIFdvcmxkIQ.txt   t256 URI
//	256t:AAAAAAANSGVsbG8sIFdvcmxkIQ       accepted leniently
//	https://256t.org/AAAAAAANSGVsbG8sIFdvcmxkIQ
func ParseRef(s string) (Ref, error) {
	s = strings.TrimSpace(s)
	var ref Ref
	lower := strings.ToLower(s)
	switch {
	case strings.HasPrefix(lower, "t256:"), strings.HasPrefix(lower, "256t:"):
		s = s[5:]
		if strings.HasPrefix(s, "//") {
			return ref, errors.New("t256 URIs have no authority: write t256:<cid>, not t256://<cid>")
		}
	case strings.HasPrefix(lower, "http://"), strings.HasPrefix(lower, "https://"):
		u, err := url.Parse(s)
		if err != nil {
			return ref, fmt.Errorf("invalid URL: %v", err)
		}
		ref.URL = s
		p := strings.TrimRight(u.Path, "/")
		s = p[strings.LastIndex(p, "/")+1:]
		if s == "" {
			return ref, fmt.Errorf("URL %q does not end in a CID", ref.URL)
		}
	}
	if strings.Contains(s, "%") {
		return ref, errors.New("CIDs never contain percent-encoding")
	}
	if i := strings.IndexByte(s, '.'); i >= 0 {
		ref.CID, ref.Suffix = s[:i], s[i:]
		for _, label := range strings.Split(ref.Suffix[1:], ".") {
			if label == "" {
				return ref, fmt.Errorf("invalid suffix %q", ref.Suffix)
			}
			for j := 0; j < len(label); j++ {
				if !isAlnum(label[j]) {
					return ref, fmt.Errorf("invalid suffix %q: labels may hold only letters and digits", ref.Suffix)
				}
			}
		}
	} else {
		ref.CID = s
	}
	if err := Validate(ref.CID); err != nil {
		return ref, err
	}
	return ref, nil
}

// Length returns the content length the CID declares.
func (r Ref) Length() int64 {
	n, _ := decodeLength(r.CID[:PrefixLen])
	return n
}

// IsInline reports whether the CID carries the content itself.
func (r Ref) IsInline() bool { return r.Length() <= MaxInline }

// Inline returns the content carried in an inline CID.
func (r Ref) Inline() ([]byte, error) {
	if !r.IsInline() {
		return nil, errors.New("CID does not carry its content inline")
	}
	return b64.DecodeString(r.CID[PrefixLen:])
}

// URI returns the t256: URI for the reference, including any suffix.
func (r Ref) URI() string { return "t256:" + r.CID + r.Suffix }

// MediaType returns the media type implied by the suffix's final label,
// or "" when there is no suffix or it is not recognized.
func (r Ref) MediaType() string { return MediaTypeForSuffix(r.Suffix) }

// Verify reports whether content matches the CID.
func (r Ref) Verify(content []byte) bool { return Compute(content) == r.CID }

// common extensions, so results do not depend on the host's MIME tables.
var mediaTypes = map[string]string{
	"bin": "application/octet-stream", "css": "text/css", "csv": "text/csv",
	"gif": "image/gif", "gz": "application/gzip", "htm": "text/html",
	"html": "text/html", "ico": "image/vnd.microsoft.icon", "jpeg": "image/jpeg",
	"jpg": "image/jpeg", "js": "text/javascript", "json": "application/json",
	"m4a": "audio/mp4", "md": "text/markdown", "mjs": "text/javascript",
	"mp3": "audio/mpeg", "mp4": "video/mp4", "pdf": "application/pdf",
	"png": "image/png", "svg": "image/svg+xml", "tar": "application/x-tar",
	"txt": "text/plain", "wasm": "application/wasm", "webm": "video/webm",
	"webp": "image/webp", "xml": "application/xml", "yaml": "application/yaml",
	"yml": "application/yaml", "zip": "application/zip",
}

// MediaTypeForSuffix maps a suffix such as ".tar.gz" or a file name such as
// "a.png" to a media type using its final label.
func MediaTypeForSuffix(s string) string {
	i := strings.LastIndexByte(s, '.')
	if i < 0 || i == len(s)-1 {
		return ""
	}
	ext := strings.ToLower(s[i+1:])
	if t, ok := mediaTypes[ext]; ok {
		return t
	}
	if t := mime.TypeByExtension("." + ext); t != "" {
		if mt, _, err := mime.ParseMediaType(t); err == nil {
			return mt
		}
	}
	return ""
}
