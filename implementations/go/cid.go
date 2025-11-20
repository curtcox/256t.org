package main

import (
	"crypto/sha512"
	"encoding/base64"
	"encoding/binary"
	"fmt"
	"io"
	"net/http"
	"path/filepath"
	"runtime"
	"strings"
)

var (
	BaseDir     string
	ExamplesDir string
	CidsDir     string
)

func init() {
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		panic("unable to determine caller information")
	}
	BaseDir = filepath.Clean(filepath.Join(filepath.Dir(filename), "..", ".."))
	ExamplesDir = filepath.Join(BaseDir, "examples")
	CidsDir = filepath.Join(BaseDir, "cids")
}

func toBase64URL(data []byte) string {
	return base64.RawURLEncoding.EncodeToString(data)
}

func encodeLength(length int) string {
	var buffer [8]byte
	binary.BigEndian.PutUint64(buffer[:], uint64(length))
	return toBase64URL(buffer[2:])
}

func computeCID(content []byte) string {
	prefix := encodeLength(len(content))

	var suffix string
	if len(content) <= 64 {
		suffix = toBase64URL(content)
	} else {
		hash := sha512.Sum512(content)
		suffix = toBase64URL(hash[:])
	}

	return prefix + suffix
}

type DownloadResult struct {
	Content  []byte
	Computed string
	IsValid  bool
}

func downloadCID(baseURL, cid string) (*DownloadResult, error) {
	url := strings.TrimSuffix(baseURL, "/") + "/" + cid
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("HTTP %d: %s", resp.StatusCode, resp.Status)
	}

	content, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	computed := computeCID(content)
	return &DownloadResult{
		Content:  content,
		Computed: computed,
		IsValid:  computed == cid,
	}, nil
}
