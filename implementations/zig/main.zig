const std = @import("std");
const crypto = std.crypto;
const fs = std.fs;
const mem = std.mem;
const process = std.process;

const allocator = std.heap.page_allocator;

fn toBase64Url(bytes: []const u8) ![]u8 {
    const encoded_len = std.base64.UrlSafeAlphabet.encodedLen(bytes.len);
    var encoded_buf = try allocator.alloc(u8, encoded_len);
    std.base64.UrlSafeAlphabet.encode(encoded_buf, bytes);

    var i: usize = encoded_buf.len - 1;
    while (i >= 0) : (i -= 1) {
        if (encoded_buf[i] == '=') {
            continue;
        }
        return encoded_buf[0 .. i + 1];
    }
    return &[];
}

fn encodeLength(len: u64) ![]u8 {
    var len_bytes: [6]u8 = undefined;
    @memcpy(&len_bytes, &@as([8]u8, @bitCast(len))[2..]);
    return toBase64Url(&len_bytes);
}

fn computeCid(content: []const u8) ![]u8 {
    const prefix = try encodeLength(@intCast(content.len));
    defer allocator.free(prefix);

    var suffix: []u8 = undefined;
    if (content.len <= 64) {
        suffix = try toBase64Url(content);
    } else {
        var hash: [64]u8 = undefined;
        crypto.hash.sha2.Sha512.hash(content, &hash, .{});
        suffix = try toBase64Url(&hash);
    }
    defer allocator.free(suffix);

    var cid = std.ArrayList(u8).init(allocator);
    try cid.appendSlice(prefix);
    try cid.appendSlice(suffix);
    return cid.toOwnedSlice();
}

fn generate() !void {
    _ = fs.cwd().makeDir("cids") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => |e| return e,
    };
    var examples_dir = try fs.cwd().openIterableDir("examples", .{});
    defer examples_dir.close();

    var walker = examples_dir.iterate();

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        const file_path = try fs.path.join(allocator, &[_][]const u8{"examples", entry.name});
        defer allocator.free(file_path);
        const content = try fs.cwd().readFileAlloc(allocator, file_path, 1_000_000);
        defer allocator.free(content);
        const cid = try computeCid(content);
        defer allocator.free(cid);

        const dest_path = try fs.path.join(allocator, &[_][]const u8{"cids", cid});
        defer allocator.free(dest_path);
        var dest_file = try fs.cwd().createFile(dest_path, .{});
        defer dest_file.close();
        try dest_file.writeAll(content);
        std.debug.print("Wrote {s} from {s}\n", .{cid, entry.name});
    }
}

fn check() !void {
    var cids_dir = try fs.cwd().openIterableDir("cids", .{});
    defer cids_dir.close();

    var mismatches = std.ArrayList([]const u8).init(allocator);
    defer mismatches.deinit();
    var count: usize = 0;

    var walker = cids_dir.iterate();

    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        count += 1;
        const file_path = try fs.path.join(allocator, &[_][]const u8{"cids", entry.name});
        defer allocator.free(file_path);
        const content = try fs.cwd().readFileAlloc(allocator, file_path, 1_000_000);
        defer allocator.free(content);
        const expected_cid = try computeCid(content);
        defer allocator.free(expected_cid);
        if (!mem.eql(u8, entry.name, expected_cid)) {
            const mismatched_name = try allocator.dupe(u8, entry.name);
            const mismatched_expected = try allocator.dupe(u8, expected_cid);
            try mismatches.append(mismatched_name);
            try mismatches.append(mismatched_expected);
        }
    }

    if (mismatches.items.len > 0) {
        std.debug.print("Found CID mismatches:\n", .{});
        var i: usize = 0;
        while (i < mismatches.items.len) : (i += 2) {
            std.debug.print("- {s} should be {s}\n", .{mismatches.items[i], mismatches.items[i+1]});
        }
        process.exit(1);
    } else {
        std.debug.print("All {} CID files match their contents.\n", .{count});
    }
}

pub fn main() !void {
    var args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} [generate|check]\n", .{args[0]});
        return;
    }

    if (mem.eql(u8, args[1], "generate")) {
        try generate();
    } else if (mem.eql(u8, args[1], "check")) {
        try check();
    } else {
        std.debug.print("Unknown command: {s}\n", .{args[1]});
    }
}
