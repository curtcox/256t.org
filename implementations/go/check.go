package main

import (
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

	mismatches := 0
	count := 0

	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		count++

		path := filepath.Join(CidsDir, entry.Name())
		content, err := os.ReadFile(path)
		if err != nil {
			fmt.Printf("Failed to read %s: %v\n", entry.Name(), err)
			mismatches++
			continue
		}

		expected := computeCID(content)
		if expected != entry.Name() {
			fmt.Printf("%s should be %s\n", entry.Name(), expected)
			mismatches++
		}
	}

	if mismatches > 0 {
		fmt.Printf("Found %d mismatched CID file(s).\n", mismatches)
		os.Exit(1)
	}

	fmt.Printf("All %d CID files match their contents.\n", count)
}
