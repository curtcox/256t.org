import std/base64
import nimcrypto/sha2

proc toBase64Url[T: byte|char](data: openArray[T]): string =
  result = encode(data, safe = true)
  # Remove trailing '=' padding
  var i = result.len - 1
  while i >= 0 and result[i] == '=':
    i -= 1
  result = result[0..i]

proc encodeLength(length: int): string =
  var bytes: array[6, byte]
  var n = length
  for i in countdown(5, 0):
    bytes[i] = (n and 0xFF).byte
    n = n shr 8
  return toBase64Url(bytes)

proc computeCid*(content: string): string =
  let prefix = encodeLength(content.len)
  var suffix: string
  if content.len <= 64:
    suffix = toBase64Url(content)
  else:
    let hash = sha512.digest(content)
    suffix = toBase64Url(hash.data)
  return prefix & suffix
