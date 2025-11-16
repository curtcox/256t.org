const std = @import("std");
const crypto = std.crypto;

/// Convert bytes to base64url encoding (RFC 4648 Section 5)
/// Returns an allocated string that must be freed by the caller
pub fn toBase64url(allocator: std.mem.Allocator, data: []const u8) ![]u8 {
    const encoder = std.base64.url_safe_no_pad;
    const encoded_len = encoder.Encoder.calcSize(data.len);
    const buffer = try allocator.alloc(u8, encoded_len);
    _ = encoder.Encoder.encode(buffer, data);
    return buffer;
}

/// Encode length as an 8-character base64url string
pub fn encodeLength(allocator: std.mem.Allocator, length: usize) ![]u8 {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, @intCast(length), .big);
    // Use only the last 6 bytes (48 bits)
    return toBase64url(allocator, bytes[2..]);
}

/// Compute CID for given content
/// Returns an allocated string that must be freed by the caller
pub fn computeCid(allocator: std.mem.Allocator, content: []const u8) ![]u8 {
    const prefix = try encodeLength(allocator, content.len);
    defer allocator.free(prefix);

    const suffix = if (content.len <= 64)
        try toBase64url(allocator, content)
    else blk: {
        var hash: [64]u8 = undefined;
        crypto.hash.sha2.Sha512.hash(content, &hash, .{});
        break :blk try toBase64url(allocator, &hash);
    };
    defer allocator.free(suffix);

    // Combine prefix and suffix
    const cid = try allocator.alloc(u8, prefix.len + suffix.len);
    @memcpy(cid[0..prefix.len], prefix);
    @memcpy(cid[prefix.len..], suffix);

    return cid;
}
