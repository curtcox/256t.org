# 256t.org

## What?
It is a domain dedicated to be a public utility for content addressable storage.
Each 94 character URL path defines the content at that URL.

## Why?
Why [256t.org](https://256t.org)?
A simple standard for a generic content addressable store seems generally useful to me.

## How?

Every 94 character path can be used to retrieve content that matches the length and hash specified.
If no content is available a 404 is returned instead.

A SHA-512 hash is 512 bits or 64 bytes long.
An 8 character base64 store 48 bits. 
```
2^48 = 2^40 * 2^8
  = 2^10 * 2^10 * 2^10 * 2^10 * 2^8
  = 2^8  * 2^10 * 2^10 * 2^10 * 2^10
  = 256    K      M      G      T
```

The following can be treated as true enough despite being false:
- A 94 character path uniquely determines content. (However, this is completely true for content less than 64 bytes.)
- The content is immutable. (It could be replaced by different content that still meets the description.)
- Content can be safely cached indefinitely.

Thus all HTTP meta information such as headers and eTags will indicated that the content can be cached indefinitely.

The 94 character content tag consists of an 8 character length prefix followed by a 86 character hash.

|         | length of the content | hash of the content  | the content iteself   | 
|---------|-----------------------|----------------------|-----------------------|
| when    | always                | length(content) > 64 | length(content) <= 64 |
| start   | 1                     | 9                    | 9                     |
| end     | 8                     | 94                   | 94                    |
| length  | 8                     | 86                   | 0 to 86               |
| format  | Base64                | Base64               | Base64                |
| info    | length(content)       | sha-512(content)     | content               |

### Base64
More specifically, [filename and URL safe](https://datatracker.ietf.org/doc/html/rfc4648#section-5) [Base64](https://en.wikipedia.org/wiki/Base64) aka base64url.

## When

## Where
