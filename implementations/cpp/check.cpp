#include <algorithm>
#include <array>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <openssl/sha.h>

namespace {

std::string base64_url_encode(const std::vector<uint8_t>& data) {
    static constexpr char kAlphabet[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    std::string encoded;
    encoded.reserve(((data.size() + 2) / 3) * 4);

    size_t i = 0;
    while (i + 2 < data.size()) {
        uint32_t triple = (static_cast<uint32_t>(data[i]) << 16) |
                          (static_cast<uint32_t>(data[i + 1]) << 8) |
                          static_cast<uint32_t>(data[i + 2]);
        encoded.push_back(kAlphabet[(triple >> 18) & 0x3F]);
        encoded.push_back(kAlphabet[(triple >> 12) & 0x3F]);
        encoded.push_back(kAlphabet[(triple >> 6) & 0x3F]);
        encoded.push_back(kAlphabet[triple & 0x3F]);
        i += 3;
    }

    const auto remaining = data.size() - i;
    if (remaining == 1) {
        uint32_t triple = static_cast<uint32_t>(data[i]) << 16;
        encoded.push_back(kAlphabet[(triple >> 18) & 0x3F]);
        encoded.push_back(kAlphabet[(triple >> 12) & 0x3F]);
        encoded.push_back('=');
        encoded.push_back('=');
    } else if (remaining == 2) {
        uint32_t triple = (static_cast<uint32_t>(data[i]) << 16) |
                          (static_cast<uint32_t>(data[i + 1]) << 8);
        encoded.push_back(kAlphabet[(triple >> 18) & 0x3F]);
        encoded.push_back(kAlphabet[(triple >> 12) & 0x3F]);
        encoded.push_back(kAlphabet[(triple >> 6) & 0x3F]);
        encoded.push_back('=');
    }

    for (char& c : encoded) {
        if (c == '+') c = '-';
        if (c == '/') c = '_';
    }
    while (!encoded.empty() && encoded.back() == '=') {
        encoded.pop_back();
    }

    return encoded;
}

std::string encode_length(std::size_t length) {
    std::array<uint8_t, 6> bytes{};
    for (int i = 0; i < 6; ++i) {
        bytes[5 - i] = static_cast<uint8_t>((length >> (i * 8)) & 0xFF);
    }
    return base64_url_encode(std::vector<uint8_t>(bytes.begin(), bytes.end()));
}

std::string compute_cid(const std::vector<uint8_t>& content) {
    const auto prefix = encode_length(content.size());

    if (content.size() <= 64) {
        return prefix + base64_url_encode(content);
    }

    std::array<unsigned char, SHA512_DIGEST_LENGTH> digest{};
    SHA512(content.data(), content.size(), digest.data());
    const std::vector<uint8_t> hash_bytes(digest.begin(), digest.end());
    return prefix + base64_url_encode(hash_bytes);
}

std::vector<uint8_t> read_file(const std::filesystem::path& path) {
    std::ifstream file(path, std::ios::binary);
    return std::vector<uint8_t>(std::istreambuf_iterator<char>(file), {});
}

}  // namespace

int main() {
    namespace fs = std::filesystem;

    const fs::path cids_dir = fs::current_path() / "cids";
    if (!fs::exists(cids_dir) || !fs::is_directory(cids_dir)) {
        std::cerr << "Missing cids directory at " << cids_dir << "\n";
        return 1;
    }

    std::vector<fs::path> files;
    for (const auto& entry : fs::directory_iterator(cids_dir)) {
        if (entry.is_regular_file()) {
            files.push_back(entry.path());
        }
    }
    std::sort(files.begin(), files.end());

    std::vector<std::pair<fs::path, std::string>> mismatches;
    std::size_t count = 0;

    for (const auto& path : files) {
        ++count;
        const auto content = read_file(path);
        const auto expected = compute_cid(content);
        const auto actual = path.filename().string();
        if (actual != expected) {
            mismatches.emplace_back(path, expected);
        }
    }

    if (!mismatches.empty()) {
        std::cout << "Found CID mismatches:" << '\n';
        for (const auto& mismatch : mismatches) {
            std::cout << "- " << mismatch.first.filename().string()
                      << " should be " << mismatch.second << '\n';
        }
        return 1;
    }

    std::cout << "All " << count << " CID files match their contents." << '\n';
    return 0;
}

