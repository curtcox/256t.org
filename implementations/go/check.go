package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
)

func main() {
	entries, err := os.ReadDir(CidsDir)
	if err != nil {
		fmt.Printf("Failed to read cids directory: %v\n", err)
		os.Exit(1)
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Name() < entries[j].Name()
	})

	type mismatch struct {
		name     string
		expected string
	}
	type downloadFailure struct {
		cid   string
		error string
	}

	var mismatches []mismatch
	var downloadFailures []downloadFailure
	count := 0
	baseURL := "https://256t.org"

	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		count++

		cid := entry.Name()
		path := filepath.Join(CidsDir, cid)
		localContent, err := os.ReadFile(path)
		if err != nil {
			fmt.Printf("Failed to read %s: %v\n", cid, err)
			mismatches = append(mismatches, mismatch{cid, "read error"})
			continue
		}

		expected := computeCID(localContent)
		if expected != cid {
			mismatches = append(mismatches, mismatch{cid, expected})
		}

		// Check downloaded content
		result, err := downloadCID(baseURL, cid)
		if err != nil {
			downloadFailures = append(downloadFailures, downloadFailure{cid, err.Error()})
		} else if !result.IsValid {
			downloadFailures = append(downloadFailures, downloadFailure{cid, result.Computed})
		} else if !bytes.Equal(result.Content, localContent) {
			downloadFailures = append(downloadFailures, downloadFailure{cid, "content mismatch with local file"})
		}
	}

	hasErrors := false

	if len(mismatches) > 0 {
		fmt.Println("Found CID mismatches:")
		for _, m := range mismatches {
			fmt.Printf("- %s should be %s\n", m.name, m.expected)
		}
		hasErrors = true
	}

	if len(downloadFailures) > 0 {
		fmt.Fprintln(os.Stderr, "Found download validation failures:")
		for _, f := range downloadFailures {
			fmt.Fprintf(os.Stderr, "- %s: %s\n", f.cid, f.error)
		}
		hasErrors = true
	}

	if hasErrors {
		os.Exit(1)
	}

	fmt.Printf("All %d CID files match their contents.\n", count)
	fmt.Printf("All %d downloaded CIDs are valid.\n", count)
}
