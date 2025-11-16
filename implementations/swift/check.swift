import Foundation

@main
struct Check {
    static func main() {
        let fileManager = FileManager.default
        var mismatches: [(String, String)] = []
        var count = 0

        do {
            let entries = try fileManager.contentsOfDirectory(at: cidsDir, includingPropertiesForKeys: [.isDirectoryKey])
                .sorted { $0.lastPathComponent < $1.lastPathComponent }
            for entry in entries {
                let resourceValues = try entry.resourceValues(forKeys: [.isDirectoryKey])
                if resourceValues.isDirectory == true {
                    continue
                }
                count += 1
                let actual = entry.lastPathComponent
                let content = try Data(contentsOf: entry)
                let expected = try computeCID(content)
                if actual != expected {
                    mismatches.append((actual, expected))
                }
            }
        } catch {
            fputs("Error: \(error)\n", stderr)
            exit(1)
        }

        if mismatches.isEmpty {
            print("All \(count) CID files match their contents.")
            return
        }

        print("Found CID mismatches:")
        for (actual, expected) in mismatches {
            print("- \(actual) should be \(expected)")
        }
        exit(1)
    }
}
