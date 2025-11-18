# [256t.org](https://curtcox.github.io/256t.org/)

## What?

256t.org is a domain dedicated to be a public specification for a specific type of content addressable storage.
In this scheme the last element of up to 94 characters in a URL path defines the content at that URL.
At some point in the future, it may evolve to also be a public utility for publishing content using the scheme.
However, that is currently beyond the scope if this site.

## Why?

Why [256t.org](https://256t.org)?
A simple standard for a generic content addressable store seems generally useful to me.

## How?

Every 94 character path can be used to retrieve content that matches the length and hash specified.
If no content is available a 404 is returned instead.

A SHA-512 hash is 512 bits or 64 bytes long.
64 bytes can be stored in a 86 character base64 string.
```
(64 * 8) / 6 = 85.333... ~= 86
```

Content of 64 bytes or less can be stored directly in equal or lesser space.
In such cases, the content itself should be base64 encoded and used with a minimum of padding rather than using its hash.

An 8 character base64 string can store 48 bits. 
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

## CID

This 94 character or less base64 string which identifies content will be referred to as a content identifier or CID.

## When

## Where

Any server could expose a base URL with contents that adhere to this spec.
Alas, what servers host what content and how to find them is beyond the scope of this text.
This server hosts a small set of CIDs [here](cids/).

## Beyond the Scope of This Text

- Some content could be restricted to certain people or requesting entities.
- Some content could have disputed ownership, differing ownership in different jurisdictions, disputed distribution rights.
- In order to serve or not serve content, these issues need to be decided.
- How are such things decided?

These things are beyond the scope of this text.

## Tools

- [Hash calculator](hash.html) — type text and instantly see its SHA-512 hash encoded in Base64URL.
- [CID Examples](examples.html) — select example files and verify their CIDs using the ECMAScript implementation.
- [File CID Check](check.html) — drag and drop files to analyze their content and calculate their CID.

## What about Collisions?

There are a few different types of collisions that are important to distinquish between:
- accidental -- purely by chance
- adversarial -- someone tried to cause it 
- existing -- a CID has been produced from different content
- problem -- usage of the CID to get content returned the wrong content

The odds of a problem collision are quite low. There are two ways to minimize them:
- Always verify CID content. There are many implementations to do so. It is easier to just lie than engineer a collision.
- Reduce adversaries. If nobody is putting problem content where you might accept it, you are left with just accidents.

I'm comfortable just ignoring accidental problem collisions.

## Implementations

The 256t.org specification has been implemented in multiple programming languages. Each implementation provides utilities to generate and verify content identifiers (CIDs).

| Language | Badge | Code |
|----------|-------|------|
| Bash | ![Bash](https://img.shields.io/badge/Bash-4EAA25?style=flat&logo=gnubash&logoColor=white) | [bash](https://github.com/curtcox/256t.org/tree/main/implementations/bash) |
| C | ![C](https://img.shields.io/badge/C-A8B9CC?style=flat&logo=c&logoColor=white) | [c](https://github.com/curtcox/256t.org/tree/main/implementations/c) |
| C++ | ![C++](https://img.shields.io/badge/C++-00599C?style=flat&logo=cplusplus&logoColor=white) | [cpp](https://github.com/curtcox/256t.org/tree/main/implementations/cpp) |
| C# | ![C#](https://img.shields.io/badge/C%23-239120?style=flat&logo=csharp&logoColor=white) | [csharp](https://github.com/curtcox/256t.org/tree/main/implementations/csharp) |
| Clojure | ![Clojure](https://img.shields.io/badge/Clojure-5881D8?style=flat&logo=clojure&logoColor=white) | [clojure](https://github.com/curtcox/256t.org/tree/main/implementations/clojure) |
| CMake | ![CMake](https://img.shields.io/badge/CMake-064F8C?style=flat&logo=cmake&logoColor=white) | [cmake](https://github.com/curtcox/256t.org/tree/main/implementations/cmake) |
| Crystal | ![Crystal](https://img.shields.io/badge/Crystal-000000?style=flat&logo=crystal&logoColor=white) | [crystal](https://github.com/curtcox/256t.org/tree/main/implementations/crystal) |
| D | ![D](https://img.shields.io/badge/D-B03931?style=flat&logo=d&logoColor=white) | [d](https://github.com/curtcox/256t.org/tree/main/implementations/d) |
| Dart | ![Dart](https://img.shields.io/badge/Dart-0175C2?style=flat&logo=dart&logoColor=white) | [dart](https://github.com/curtcox/256t.org/tree/main/implementations/dart) |
| Deno | ![Deno](https://img.shields.io/badge/Deno-000000?style=flat&logo=deno&logoColor=white) | [deno](https://github.com/curtcox/256t.org/tree/main/implementations/deno) |
| ECMAScript | ![ECMAScript](https://img.shields.io/badge/ECMAScript-F7DF1E?style=flat&logo=javascript&logoColor=black) | [ecmascript](https://github.com/curtcox/256t.org/tree/main/implementations/ecmascript) |
| Elixir | ![Elixir](https://img.shields.io/badge/Elixir-4B275F?style=flat&logo=elixir&logoColor=white) | [elixir](https://github.com/curtcox/256t.org/tree/main/implementations/elixir) |
| Emacs Lisp | ![Emacs Lisp](https://img.shields.io/badge/Emacs%20Lisp-7F5AB6?style=flat&logo=gnuemacs&logoColor=white) | [emacs-lisp](https://github.com/curtcox/256t.org/tree/main/implementations/emacs-lisp) |
| Erlang | ![Erlang](https://img.shields.io/badge/Erlang-A90533?style=flat&logo=erlang&logoColor=white) | [erlang](https://github.com/curtcox/256t.org/tree/main/implementations/erlang) |
| F# | ![F#](https://img.shields.io/badge/F%23-378BBA?style=flat&logo=fsharp&logoColor=white) | [fsharp](https://github.com/curtcox/256t.org/tree/main/implementations/fsharp) |
| Fortran | ![Fortran](https://img.shields.io/badge/Fortran-734F96?style=flat&logo=fortran&logoColor=white) | [fortran](https://github.com/curtcox/256t.org/tree/main/implementations/fortran) |
| Go | ![Go](https://img.shields.io/badge/Go-00ADD8?style=flat&logo=go&logoColor=white) | [go](https://github.com/curtcox/256t.org/tree/main/implementations/go) |
| Groovy | ![Groovy](https://img.shields.io/badge/Groovy-4298B8?style=flat&logo=apachegroovy&logoColor=white) | [groovy](https://github.com/curtcox/256t.org/tree/main/implementations/groovy) |
| Haskell | ![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=flat&logo=haskell&logoColor=white) | [haskell](https://github.com/curtcox/256t.org/tree/main/implementations/haskell) |
| Java | ![Java](https://img.shields.io/badge/Java-ED8B00?style=flat&logo=openjdk&logoColor=white) | [java](https://github.com/curtcox/256t.org/tree/main/implementations/java) |
| JavaScript | ![JavaScript](https://img.shields.io/badge/JavaScript-F7DF1E?style=flat&logo=javascript&logoColor=black) | [javascript](https://github.com/curtcox/256t.org/tree/main/implementations/javascript) |
| Julia | ![Julia](https://img.shields.io/badge/Julia-9558B2?style=flat&logo=julia&logoColor=white) | [julia](https://github.com/curtcox/256t.org/tree/main/implementations/julia) |
| Kotlin | ![Kotlin](https://img.shields.io/badge/Kotlin-7F52FF?style=flat&logo=kotlin&logoColor=white) | [kotlin](https://github.com/curtcox/256t.org/tree/main/implementations/kotlin) |
| Lua | ![Lua](https://img.shields.io/badge/Lua-2C2D72?style=flat&logo=lua&logoColor=white) | [lua](https://github.com/curtcox/256t.org/tree/main/implementations/lua) |
| Nim | ![Nim](https://img.shields.io/badge/Nim-FFE953?style=flat&logo=nim&logoColor=black) | [nim](https://github.com/curtcox/256t.org/tree/main/implementations/nim) |
| Node.js | ![Node.js](https://img.shields.io/badge/Node.js-339933?style=flat&logo=nodedotjs&logoColor=white) | [node](https://github.com/curtcox/256t.org/tree/main/implementations/node) |
| OCaml | ![OCaml](https://img.shields.io/badge/OCaml-EC6813?style=flat&logo=ocaml&logoColor=white) | [ocaml](https://github.com/curtcox/256t.org/tree/main/implementations/ocaml) |
| Perl | ![Perl](https://img.shields.io/badge/Perl-39457E?style=flat&logo=perl&logoColor=white) | [perl](https://github.com/curtcox/256t.org/tree/main/implementations/perl) |
| PHP | ![PHP](https://img.shields.io/badge/PHP-777BB4?style=flat&logo=php&logoColor=white) | [php](https://github.com/curtcox/256t.org/tree/main/implementations/php) |
| PowerShell | ![PowerShell](https://img.shields.io/badge/PowerShell-5391FE?style=flat&logo=powershell&logoColor=white) | [powershell](https://github.com/curtcox/256t.org/tree/main/implementations/powershell) |
| Prolog | ![Prolog](https://img.shields.io/badge/Prolog-E61B23?style=flat) | [prolog](https://github.com/curtcox/256t.org/tree/main/implementations/prolog) |
| Python | ![Python](https://img.shields.io/badge/Python-3776AB?style=flat&logo=python&logoColor=white) | [python](https://github.com/curtcox/256t.org/tree/main/implementations/python) |
| R | ![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white) | [r](https://github.com/curtcox/256t.org/tree/main/implementations/r) |
| Racket | ![Racket](https://img.shields.io/badge/Racket-9F1D20?style=flat&logo=racket&logoColor=white) | [racket](https://github.com/curtcox/256t.org/tree/main/implementations/racket) |
| Ruby | ![Ruby](https://img.shields.io/badge/Ruby-CC342D?style=flat&logo=ruby&logoColor=white) | [ruby](https://github.com/curtcox/256t.org/tree/main/implementations/ruby) |
| Rust | ![Rust](https://img.shields.io/badge/Rust-000000?style=flat&logo=rust&logoColor=white) | [rust](https://github.com/curtcox/256t.org/tree/main/implementations/rust) |
| Scala | ![Scala](https://img.shields.io/badge/Scala-DC322F?style=flat&logo=scala&logoColor=white) | [scala](https://github.com/curtcox/256t.org/tree/main/implementations/scala) |
| Swift | ![Swift](https://img.shields.io/badge/Swift-FA7343?style=flat&logo=swift&logoColor=white) | [swift](https://github.com/curtcox/256t.org/tree/main/implementations/swift) |
| Tcl | ![Tcl](https://img.shields.io/badge/Tcl-000000?style=flat) | [tcl](https://github.com/curtcox/256t.org/tree/main/implementations/tcl) |
| TypeScript | ![TypeScript](https://img.shields.io/badge/TypeScript-3178C6?style=flat&logo=typescript&logoColor=white) | [typescript](https://github.com/curtcox/256t.org/tree/main/implementations/typescript) |
| Zig | ![Zig](https://img.shields.io/badge/Zig-F7A41D?style=flat&logo=zig&logoColor=white) | [zig](https://github.com/curtcox/256t.org/tree/main/implementations/zig) |

