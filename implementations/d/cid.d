module cid;

import std.base64 : Base64URLNoPadding;
import std.digest.sha : sha512Of;
import std.array : array;

string toBase64Url(const(ubyte)[] data) {
    return Base64URLNoPadding.encode(data).idup;
}

string encodeLength(size_t length) {
    ubyte[6] bytes;
    foreach (i; 0 .. 6) {
        bytes[5 - i] = cast(ubyte)((length >> (8 * i)) & 0xFF);
    }
    return toBase64Url(bytes);
}

string computeCID(const(ubyte)[] content) {
    auto prefix = encodeLength(content.length);
    string suffix;
    if (content.length <= 64) {
        suffix = toBase64Url(content);
    } else {
        auto digest = sha512Of(content).array;
        suffix = toBase64Url(digest);
    }
    return prefix ~ suffix;
}

version (unittest) {
    import std.encoding : Base64Exception;

    unittest {
        // Verify encoding for a known length.
        assert(encodeLength(1) == "AAAAAAAAAQ");
        assert(encodeLength(64) == "AAAAAAAAQA");
    }
}
