import std/os
import std/strformat
import cid

const
  baseDir = parentDir(parentDir(parentDir(currentSourcePath())))
  examplesDir = baseDir / "examples"
  cidsDir = baseDir / "cids"

proc generate() =
  if not dirExists(cidsDir):
    createDir(cidsDir)

  for file in walkDir(examplesDir):
    if file.kind == pcDir:
      continue
    let content = readFile(file.path)
    let cid = computeCid(content)
    let destination = cidsDir / cid
    writeFile(destination, content)
    echo &"Wrote {destination} from {file.path}"

proc check(): int =
  var mismatches: seq[tuple[path: string, expected: string]] = @[]
  var count = 0
  for file in walkDir(cidsDir):
    if file.kind == pcDir:
      continue
    count += 1
    let actual = lastPathPart(file.path)
    let content = readFile(file.path)
    let expected = computeCid(content)
    if actual != expected:
      mismatches.add((file.path, expected))

  if mismatches.len > 0:
    echo "Found CID mismatches:"
    for mismatch in mismatches:
      echo &"- {mismatch.path} should be {mismatch.expected}"
    return 1
  else:
    echo &"All {count} CID files match their contents."
    return 0

when isMainModule:
  let args = commandLineParams()
  if args.len == 0:
    echo "Usage: main [check|generate]"
    quit(1)

  case args[0]
  of "check":
    quit(check())
  of "generate":
    generate()
  else:
    echo "Unknown command: ", args[0]
    quit(1)
