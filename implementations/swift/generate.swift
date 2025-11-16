import Foundation

@main
struct Generate {
    static func main() {
        let fileManager = FileManager.default
        do {
            try fileManager.createDirectory(at: cidsDir, withIntermediateDirectories: true)
            let examples = try fileManager.contentsOfDirectory(at: examplesDir, includingPropertiesForKeys: [.isDirectoryKey])
                .sorted { $0.lastPathComponent < $1.lastPathComponent }
            for example in examples {
                let resourceValues = try example.resourceValues(forKeys: [.isDirectoryKey])
                if resourceValues.isDirectory == true {
                    continue
                }
                let content = try Data(contentsOf: example)
                let cid = try computeCID(content)
                let destination = cidsDir.appendingPathComponent(cid)
                try content.write(to: destination)
                print("Wrote \(destination.lastPathComponent) from \(example.lastPathComponent)")
            }
        } catch {
            fputs("Error: \(error)\n", stderr)
            exit(1)
        }
    }
}
