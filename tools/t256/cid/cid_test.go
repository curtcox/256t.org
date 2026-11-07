package cid

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const (
	helloCID = "AAAAAAANSGVsbG8sIFdvcmxkIQ"
	a65CID   = "AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg"
)

func TestRFCExamples(t *testing.T) {
	cases := map[string][]byte{
		"AAAAAAAA": {},
		helloCID:   []byte("Hello, World!"),
		a65CID:     bytes.Repeat([]byte("a"), 65),
	}
	for want, content := range cases {
		if got := Compute(content); got != want {
			t.Errorf("Compute(%q) = %s, want %s", content, got, want)
		}
		got, n, err := ComputeReader(bytes.NewReader(content))
		if err != nil || got != want || n != int64(len(content)) {
			t.Errorf("ComputeReader(%q) = %s, %d, %v", content, got, n, err)
		}
	}
}

// Every file in the repository's cids/ directory is named by its CID.
func TestRepositoryCIDs(t *testing.T) {
	dir := filepath.Join("..", "..", "..", "cids")
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Skipf("cids directory not available: %v", err)
	}
	for _, e := range entries {
		data, err := os.ReadFile(filepath.Join(dir, e.Name()))
		if err != nil {
			t.Fatal(err)
		}
		if got := Compute(data); got != e.Name() {
			t.Errorf("%s: computed %s", e.Name(), got)
		}
		if err := Validate(e.Name()); err != nil {
			t.Errorf("Validate(%s): %v", e.Name(), err)
		}
	}
}

func TestHasherMatchesCompute(t *testing.T) {
	for _, n := range []int{0, 1, 63, 64, 65, 66, 1000, 100000} {
		content := bytes.Repeat([]byte{byte(n)}, n)
		h := NewHasher()
		// write in uneven chunks
		for i := 0; i < len(content); i += 7 {
			end := i + 7
			if end > len(content) {
				end = len(content)
			}
			h.Write(content[i:end])
		}
		if h.CID() != Compute(content) {
			t.Errorf("n=%d: hasher %s != compute %s", n, h.CID(), Compute(content))
		}
	}
}

func TestParseRefForms(t *testing.T) {
	cases := []struct {
		in, cid, suffix, url string
	}{
		{helloCID, helloCID, "", ""},
		{"  " + helloCID + "\n", helloCID, "", ""},
		{helloCID + ".txt", helloCID, ".txt", ""},
		{"t256:" + helloCID, helloCID, "", ""},
		{"T256:" + helloCID + ".tar.gz", helloCID, ".tar.gz", ""},
		{"256t:" + helloCID, helloCID, "", ""},
		{"https://256t.org/" + a65CID, a65CID, "", "https://256t.org/" + a65CID},
		{"https://example.com/cids/" + a65CID + ".png?x=1#frag", a65CID, ".png", "https://example.com/cids/" + a65CID + ".png?x=1#frag"},
	}
	for _, c := range cases {
		ref, err := ParseRef(c.in)
		if err != nil {
			t.Errorf("ParseRef(%q): %v", c.in, err)
			continue
		}
		if ref.CID != c.cid || ref.Suffix != c.suffix || ref.URL != c.url {
			t.Errorf("ParseRef(%q) = %+v", c.in, ref)
		}
	}
}

func TestParseRefRejects(t *testing.T) {
	bad := []string{
		"",
		"AAAAAAA",                            // too short
		"t256://" + helloCID,                 // authority form
		helloCID + "x",                       // payload length wrong for declared length
		"AAAAAAANSGVsbG8sIFdvcmxkIQ%2E",      // percent-encoding
		helloCID + ".",                       // empty suffix label
		helloCID + ".t-x",                    // bad suffix character
		"AAAAAABB" + strings.Repeat("A", 85), // hash too short
		a65CID + "A",                         // too long
		"AAAAAAA!",                           // bad character
		"https://256t.org/",                  // no CID in URL
		"AAAAAAABQR",                         // non-canonical trailing bits ("A" is "QQ")
	}
	for _, s := range bad {
		if _, err := ParseRef(s); err == nil {
			t.Errorf("ParseRef(%q) succeeded, want error", s)
		}
	}
}

func TestRefAccessors(t *testing.T) {
	ref, _ := ParseRef("t256:" + helloCID + ".txt")
	if ref.Length() != 13 || !ref.IsInline() {
		t.Errorf("length %d inline %v", ref.Length(), ref.IsInline())
	}
	data, err := ref.Inline()
	if err != nil || string(data) != "Hello, World!" {
		t.Errorf("Inline() = %q, %v", data, err)
	}
	if ref.URI() != "t256:"+helloCID+".txt" || ref.MediaType() != "text/plain" {
		t.Errorf("URI %s media %s", ref.URI(), ref.MediaType())
	}
	if !ref.Verify([]byte("Hello, World!")) || ref.Verify([]byte("Hello")) {
		t.Error("Verify wrong")
	}
	big, _ := ParseRef(a65CID + ".tar.gz")
	if big.Length() != 65 || big.IsInline() || big.MediaType() != "application/gzip" {
		t.Errorf("big ref: %d %v %s", big.Length(), big.IsInline(), big.MediaType())
	}
	if _, err := big.Inline(); err == nil {
		t.Error("Inline on hash CID should fail")
	}
	if MediaTypeForSuffix(".PNG") != "image/png" || MediaTypeForSuffix("") != "" || MediaTypeForSuffix(".zzzunknown") != "" {
		t.Error("MediaTypeForSuffix")
	}
}
