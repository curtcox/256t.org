const std = @import("std");
const cid = @import("cid.zig");

const Mismatch = struct {
    actual: []const u8,
    expected: []const u8,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Get the path to the cids directory (../../cids relative to implementations/zig)
    const cwd = std.fs.cwd();
    var base_dir = try cwd.openDir("../..", .{});
    defer base_dir.close();

    var cids_dir = try base_dir.openDir("cids", .{ .iterate = true });
    defer cids_dir.close();

    var mismatches = std.ArrayList(Mismatch).init(allocator);
    defer {
        for (mismatches.items) |mismatch| {
            allocator.free(mismatch.actual);
            allocator.free(mismatch.expected);
        }
        mismatches.deinit();
    }

    // Collect all entries
    var entries = std.ArrayList([]const u8).init(allocator);
    defer {
        for (entries.items) |entry| {
            allocator.free(entry);
        }
        entries.deinit();
    }

    var iter = cids_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        const name = try allocator.dupe(u8, entry.name);
        try entries.append(name);
    }

    // Sort entries
    std.mem.sort([]const u8, entries.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);

    var count: usize = 0;

    for (entries.items) |entry_name| {
        count += 1;

        // Read file content
        const content = try cids_dir.readFileAlloc(allocator, entry_name, 1024 * 1024);
        defer allocator.free(content);

        // Compute expected CID
        const expected = try cid.computeCid(allocator, content);

        // Compare
        if (!std.mem.eql(u8, entry_name, expected)) {
            try mismatches.append(.{
                .actual = try allocator.dupe(u8, entry_name),
                .expected = expected,
            });
        } else {
            allocator.free(expected);
        }
    }

    const stdout = std.io.getStdOut().writer();

    if (mismatches.items.len > 0) {
        try stdout.print("Found CID mismatches:\n", .{});
        for (mismatches.items) |mismatch| {
            try stdout.print("- {s} should be {s}\n", .{ mismatch.actual, mismatch.expected });
        }
        std.process.exit(1);
    }

    try stdout.print("All {d} CID files match their contents.\n", .{count});
}
