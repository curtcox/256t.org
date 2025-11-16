import os
import strutils
import checksums/sha2
import base64

proc toBase64Url(data: string): string =
  result = encode(data, safe = true)
  while result.endsWith("="):
    result = result[0 .. ^2]

proc encodeLength(length: int): string =
  var lenBytes = newString(6)
  var n = length
  var i = 5
  while i >= 0:
    lenBytes[i] = (n and 0xFF).char
    n = n shr 8
    i -= 1
  return toBase64Url(lenBytes)

proc computeCid(content: string): string =
  let prefix = encodeLength(content.len)
  var suffix: string
  if content.len <= 64:
    suffix = toBase64Url(content)
  else:
    let hash = secureHash(Sha_512, content)
    var hashStr = ""
    for b in hash:
      hashStr.add(b.char)
    suffix = toBase64Url(hashStr)
  return (prefix & suffix).toLower

proc generate() =
  createDir("cids")
  for file in walkDir("examples"):
    if file.kind == pcFile:
      let content = readFile(file.path)
      let cid = computeCid(content)
      let destPath = "cids" / cid
      writeFile(destPath, content)
      echo "Wrote " & cid & " from " & file.path

proc check() =
  var mismatches: seq[tuple[path: string, expected: string]] = @[]
  var count = 0
  for file in walkDir("cids"):
    if file.kind == pcFile:
      count += 1
      let content = readFile(file.path)
      let expectedCid = computeCid(content)
      let actualCid = lastPathPart(file.path)
      if actualCid != expectedCid:
        mismatches.add((file.path, expectedCid))

  if mismatches.len > 0:
    echo "Found CID mismatches:"
    for mismatch in mismatches:
      echo "- " & mismatch.path & " should be " & mismatch.expected
    quit(1)
  else:
    echo "All " & $count & " CID files match their contents."

when isMainModule:
  if paramCount() < 1:
    echo "Usage: nim c -r main.nim [generate|check]"
    quit(1)

  let command = paramStr(1)
  case command
  of "generate":
    generate()
  of "check":
    check()
  else:
    echo "Unknown command: " & command
    quit(1)
