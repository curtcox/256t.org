#ifndef CID_H
#define CID_H

#include <stddef.h>

char *base64url_encode(const unsigned char *data, size_t length);
char *encode_length(size_t length);
char *compute_cid(const unsigned char *content, size_t length);

#endif  // CID_H
