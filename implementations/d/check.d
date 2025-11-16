module check;

import std.stdio : writeln, writefln;
import std.path : dirName, buildPath, baseName;
import std.file : dirEntries, SpanMode, thisExePath, read;
import std.algorithm : sort, filter;
import std.array : array;

import cid : computeCID;

string repoRoot() {
    auto exeDir = dirName(thisExePath());
    auto implementationsDir = dirName(exeDir);
    return dirName(implementationsDir);
}

struct Mismatch {
    string fileName;
    string expected;
}

int main() {
    const baseDir = repoRoot();
    const cidsDir = buildPath(baseDir, "cids");

    auto entries = dirEntries(cidsDir, SpanMode.shallow)
        .filter!(e => e.isFile)
        .array;
    entries.sort!((a, b) => a.name < b.name);

    Mismatch[] mismatches;
    size_t count = 0;

    foreach (entry; entries) {
        ++count;
        auto path = entry.name;
        auto content = read(path);
        auto actual = baseName(path);
        auto expected = computeCID(cast(const(ubyte)[])content);
        if (actual != expected) {
            mismatches ~= Mismatch(actual, expected);
        }
    }

    if (mismatches.length > 0) {
        writeln("Found CID mismatches:");
        foreach (mismatch; mismatches) {
            writefln("- %s should be %s", mismatch.fileName, mismatch.expected);
        }
        return 1;
    }

    writefln("All %s CID files match their contents.", count);
    return 0;
}
