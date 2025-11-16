import Foundation

let baseDir = URL(fileURLWithPath: #filePath)
    .deletingLastPathComponent()
    .deletingLastPathComponent()
    .deletingLastPathComponent()

let examplesDir = baseDir.appendingPathComponent("examples")
let cidsDir = baseDir.appendingPathComponent("cids")

enum CIDError: Error {
    case hashingFailed(status: Int32)
}

func toBase64URL(_ data: Data) -> String {
    let encoded = data.base64EncodedString()
    let trimmed = encoded.replacingOccurrences(of: "=", with: "")
    return trimmed
        .replacingOccurrences(of: "+", with: "-")
        .replacingOccurrences(of: "/", with: "_")
}

func encodeLength(_ length: Int) -> String {
    var value = UInt64(length).bigEndian
    let bytes = Data(bytes: &value, count: MemoryLayout<UInt64>.size)
    let sixBytes = bytes.suffix(6)
    return toBase64URL(sixBytes)
}

func sha512(_ content: Data) throws -> Data {
    let process = Process()
    process.executableURL = URL(fileURLWithPath: "/usr/bin/openssl")
    process.arguments = ["dgst", "-binary", "-sha512"]

    let input = Pipe()
    let output = Pipe()
    process.standardInput = input
    process.standardOutput = output

    try process.run()
    input.fileHandleForWriting.write(content)
    try input.fileHandleForWriting.close()
    process.waitUntilExit()

    guard process.terminationStatus == 0 else {
        throw CIDError.hashingFailed(status: process.terminationStatus)
    }

    return output.fileHandleForReading.readDataToEndOfFile()
}

func computeCID(_ content: Data) throws -> String {
    let prefix = encodeLength(content.count)
    let suffix: String
    if content.count <= 64 {
        suffix = toBase64URL(content)
    } else {
        let digest = try sha512(content)
        suffix = toBase64URL(digest)
    }
    return prefix + suffix
}
