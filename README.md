# 256t.org

## What?
It is a domain dedicated to be a public utility for content addressable storage.
Each 94 character URL path defines

## Why?
Why [256t.org](https://256t.org)?
A simple standard for a generic content addressable store seems generally useful to me.

## How?

Every 94 character path can be used to retrieve content that matches the length and hash specified.
If no content is available a 404 is returned instead.

The following can be treated as true enough despite being false:
- A 94 character path uniquely determines content. (However, this is completely true for content less than 64 bytes.)
- The conent is immutable. (It could be replaced by different content that still meets the description.)
- Content can be safely cached indefinitely.

Thus all HTTP meta information such as headers and eTags will indicated that the content can be cached indefinitely.

|         | length | hash                                   | 
|---------|--------|----------------------------------------|
| start   | 1      | 9                                      |
| end     | 8      | 94                                     |
| length  | 8      | 86                                     |
| format  | Base64 | Base64 encoding of content length      |
| content | Base64 | Base64 encoding of 64 bytes of SHA-512 |

### Base64
More specifically, [filename and URL safe](https://datatracker.ietf.org/doc/html/rfc4648#section-5) [Base64](https://en.wikipedia.org/wiki/Base64) aka base64url.

## When

## Where
