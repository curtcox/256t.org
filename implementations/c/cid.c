#include "cid.h"

#include <openssl/sha.h>
#include <stdlib.h>
#include <string.h>

static const char kAlphabet[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

char *base64url_encode(const unsigned char *data, size_t length) {
    const size_t full_chunks = length / 3;
    const size_t remaining = length % 3;
    size_t encoded_length = (full_chunks + (remaining ? 1 : 0)) * 4;

    char *encoded = malloc(encoded_length + 1);
    if (!encoded) {
        return NULL;
    }

    size_t out = 0;
    size_t i = 0;
    while (i + 2 < length) {
        const unsigned int triple = (data[i] << 16) | (data[i + 1] << 8) | data[i + 2];
        encoded[out++] = kAlphabet[(triple >> 18) & 0x3F];
        encoded[out++] = kAlphabet[(triple >> 12) & 0x3F];
        encoded[out++] = kAlphabet[(triple >> 6) & 0x3F];
        encoded[out++] = kAlphabet[triple & 0x3F];
        i += 3;
    }

    if (remaining == 1) {
        const unsigned int triple = data[i] << 16;
        encoded[out++] = kAlphabet[(triple >> 18) & 0x3F];
        encoded[out++] = kAlphabet[(triple >> 12) & 0x3F];
        encoded[out++] = '=';
        encoded[out++] = '=';
    } else if (remaining == 2) {
        const unsigned int triple = (data[i] << 16) | (data[i + 1] << 8);
        encoded[out++] = kAlphabet[(triple >> 18) & 0x3F];
        encoded[out++] = kAlphabet[(triple >> 12) & 0x3F];
        encoded[out++] = kAlphabet[(triple >> 6) & 0x3F];
        encoded[out++] = '=';
    }

    for (size_t j = 0; j < encoded_length; ++j) {
        if (encoded[j] == '+') encoded[j] = '-';
        if (encoded[j] == '/') encoded[j] = '_';
    }

    while (encoded_length > 0 && encoded[encoded_length - 1] == '=') {
        --encoded_length;
    }
    encoded[encoded_length] = '\0';

    return encoded;
}

char *encode_length(size_t length) {
    unsigned char bytes[6] = {0};
    for (int i = 0; i < 6; ++i) {
        bytes[5 - i] = (unsigned char)((length >> (i * 8)) & 0xFF);
    }
    return base64url_encode(bytes, sizeof(bytes));
}

char *compute_cid(const unsigned char *content, size_t length) {
    char *prefix = encode_length(length);
    if (!prefix) {
        return NULL;
    }

    char *suffix = NULL;
    if (length <= 64) {
        suffix = base64url_encode(content, length);
    } else {
        unsigned char digest[SHA512_DIGEST_LENGTH];
        SHA512(content, length, digest);
        suffix = base64url_encode(digest, SHA512_DIGEST_LENGTH);
    }

    if (!suffix) {
        free(prefix);
        return NULL;
    }

    const size_t prefix_len = strlen(prefix);
    const size_t suffix_len = strlen(suffix);
    char *cid = malloc(prefix_len + suffix_len + 1);
    if (!cid) {
        free(prefix);
        free(suffix);
        return NULL;
    }

    memcpy(cid, prefix, prefix_len);
    memcpy(cid + prefix_len, suffix, suffix_len);
    cid[prefix_len + suffix_len] = '\0';

    free(prefix);
    free(suffix);
    return cid;
}
