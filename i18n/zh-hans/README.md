# [256t.org](https://curtcox.github.io/256t.org/)

<p align="center">
  <img src="examples/256t_logo.png" alt="256t.org 标志" width="200">
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="examples/256t_qr_code.png" alt="256t.org 二维码" width="200">
</p>

<p align="center">
  <img src="examples/256t.png" alt="256t.org 内容寻址存储概念图" width="800">
</p>

## 是什么？

256t.org 是一个专门用于发布某种特定类型内容寻址存储公开规范的域名。
在这一方案中，URL 路径中最后一个最多 94 个字符的元素决定了该 URL 处的内容。
将来，它也许还会发展成为一个使用该方案发布内容的公共设施。
不过，这目前超出了本站的范围。

## 为什么？

为什么要有 [256t.org](https://256t.org)？
在我看来，一个用于通用内容寻址存储的简单标准总体上是有用的。

## 怎么做？

每一个 94 字符的路径都可以用来获取与其指定的长度和哈希相匹配的内容。
如果没有可用的内容，则返回 404。

SHA-512 哈希的长度为 512 位，即 64 字节。
64 字节可以存放在一个 86 字符的 base64 字符串中。
```
(64 * 8) / 6 = 85.333... ~= 86
```

64 字节或更短的内容可以直接存放在相同或更小的空间里。
在这种情况下，应当对内容本身进行 base64 编码并尽量少用填充，而不是使用它的哈希。

一个 8 字符的 base64 字符串可以存储 48 位。
```
2^48 = 2^40 * 2^8
  = 2^10 * 2^10 * 2^10 * 2^10 * 2^8
  = 2^8  * 2^10 * 2^10 * 2^10 * 2^10
  = 256    K      M      G      T
```

以下说法虽然并不严格成立，但可以视为足够正确：
- 一个 94 字符的路径唯一确定内容。（不过，对于小于 64 字节的内容，这完全成立。）
- 内容是不可变的。（它可能被仍然符合描述的另一份内容所替换。）
- 内容可以安全地无限期缓存。

因此，所有 HTTP 元信息（例如响应头和 ETag）都会表明该内容可以无限期缓存。

94 字符的内容标签由 8 字符的长度前缀和其后 86 字符的哈希组成。

|         | 内容的长度 | 内容的哈希 | 内容本身 |
|---------|-----------------------|----------------------|-----------------------|
| 何时    | 始终                  | length(content) > 64 | length(content) <= 64 |
| 起始    | 1                     | 9                    | 9                     |
| 结束    | 8                     | 94                   | 94                    |
| 长度    | 8                     | 86                   | 0 到 86               |
| 格式    | Base64                | Base64               | Base64                |
| 信息    | length(content)       | sha-512(content)     | content               |

### Base64

更具体地说，是[文件名和 URL 安全](https://datatracker.ietf.org/doc/html/rfc4648#section-5)的 [Base64](https://zh.wikipedia.org/wiki/Base64)，又称 base64url。

## CID

这种用于标识内容、不超过 94 个字符的 base64 字符串，被称为内容标识符（content identifier），简称 CID。

## 何时

## 何处

任何服务器都可以公开一个基础 URL，其内容遵循本规范。
可惜的是，哪些服务器托管哪些内容、以及如何找到它们，超出了本文的范围。
本服务器在[这里](cids/)托管了一小部分 CID。

## 超出本文范围的内容

- 查找
  - 我如何找到发布 CID 的地方？
- 发布
  - 我应该在哪里、以什么方式发布 CID？
- 元数据
  - CID 内容应附带哪些元数据（例如 HTTP 响应头）？
- 权限
  - 某些内容可能仅限特定的人或请求方访问。
  - 某些内容的所有权可能存在争议，在不同司法管辖区的归属可能不同，分发权也可能存在争议。
  - 要决定是否提供某项内容，就必须先解决这些问题。
  - 这些事情该如何决定？

这些事情都超出了本文的范围。

## 工具

- [哈希计算器](hash.html) —— 输入文本，即可立即看到其以 Base64URL 编码的 SHA-512 哈希。
- [CID 示例](examples.html) —— 选择示例文件，并使用 ECMAScript 实现验证它们的 CID。
- [CID 示例（Elm）](examples-elm.html) —— 选择示例文件，并使用 Elm 实现验证它们的 CID。
- [文件 CID 检查](check.html) —— 拖放文件以分析其内容并计算其 CID。
- [CID 下载](download.html) —— 使用 CID 从兼容 256t.org 的服务器下载并验证内容。
- [t256 URI 解析器](resolve.html) —— 使用内联内容、公共提供方或你的桌面助手打开 t256 链接或浏览器兼容链接；还可以选择启用浏览器链接处理。
- [桌面与浏览器支持](browser-support/README.md)（英文）—— 可选的文件夹、网络驱动器、需认证的 GitHub 仓库、经过验证的页面加载器、网关以及可选的浏览器扩展。
- [当前浏览器支持计划](browser-support/README.md#active-implementation-plan)（英文）—— 扩展的链接改写与关键字、Web 处理程序兼容别名，以及在各桌面浏览器上的原生操作系统协议注册。

## 示例文件

[examples/](examples/) 目录包含用于测试 CID 生成和验证的示例文件。其中包括一些 256t 品牌资源，以多种媒体类型演示本规范：

| 文件 | 说明 |
|------|-------------|
| [256t.mp4](examples/256t.mp4) | 讲解 256t.org 内容寻址存储规范的视频（也可在 [YouTube](https://www.youtube.com/watch?v=GrzonG2up5c) 上观看）（英文） |
| [256t.png](examples/256t.png) | 256t.org 概念示意图 |
| [256t_Content_Addressable_Storage_Specification.pdf](examples/256t_Content_Addressable_Storage_Specification.pdf) | 完整规范的 PDF 文档（英文） |
| [256t_and_the_End_of_Broken_Links.m4a](examples/256t_and_the_End_of_Broken_Links.m4a) | 关于 256t.org 如何解决链接失效问题的音频讨论（英文） |
| [256t_logo.jpg](examples/256t_logo.jpg) | 256t.org 标志（JPEG 格式） |
| [256t_logo.png](examples/256t_logo.png) | 256t.org 标志（PNG 格式） |
| [256t_qr_code.png](examples/256t_qr_code.png) | 指向 256t.org 的二维码 |

这些文件可以配合 [CID 示例](examples.html)工具或[文件 CID 检查](check.html)工具使用，以验证它们的内容标识符。

## URI 协议

一份提议 `t256:` URI 协议的 RFC 草案（英文）可在 [rfc/draft-cox-t256-uri-scheme-00.md](rfc/draft-cox-t256-uri-scheme-00.md) 查看。`t256:` URL 只包含一个 CID，没有主机或路径。（`256t:` 这种写法不是有效的 URI 协议名，因为 RFC 3986 要求协议名以字母开头。）

## 发布

关于部署和发布使用 CID 的内容寻址存储的信息，请参阅[发布与存储](publishing.html)。

## 常见问题

对 256t.org 有疑问？请查看[常见问题](faq.html)页面，那里解答了关于 CID、实现、安全性等方面的常见问题。

## 碰撞怎么办？

有几种不同类型的碰撞需要加以区分：
- 意外碰撞 —— 纯属偶然
- 对抗性碰撞 —— 有人蓄意制造
- 既有碰撞 —— 已经有不同的内容产生了同一个 CID
- 问题碰撞 —— 使用 CID 获取内容时返回了错误的内容

发生问题碰撞的几率相当低。有两种方法可以将其降到最低：
- 始终验证 CID 内容。已有许多实现可以做到这一点。直接撒谎要比设计出一次碰撞容易得多。
- 减少对手。如果没有人把有问题的内容放到你可能接受的地方，剩下的就只有意外了。

对于意外造成的问题碰撞，我可以坦然忽略。

## 实现

256t.org 规范已经用多种编程语言实现。每个实现都提供生成和验证内容标识符（CID）的工具。

| 语言 | 徽章 | 代码 |
|----------|-------|------|
| Bash | ![Bash](https://img.shields.io/badge/Bash-4EAA25?style=flat&logo=gnubash&logoColor=white) | [bash](https://github.com/curtcox/256t.org/tree/main/implementations/bash) |
| C | ![C](https://img.shields.io/badge/C-A8B9CC?style=flat&logo=c&logoColor=white) | [c](https://github.com/curtcox/256t.org/tree/main/implementations/c) |
| C++ | ![C++](https://img.shields.io/badge/C++-00599C?style=flat&logo=cplusplus&logoColor=white) | [cpp](https://github.com/curtcox/256t.org/tree/main/implementations/cpp) |
| C# | ![C#](https://img.shields.io/badge/C%23-239120?style=flat&logo=csharp&logoColor=white) | [csharp](https://github.com/curtcox/256t.org/tree/main/implementations/csharp) |
| Clojure | ![Clojure](https://img.shields.io/badge/Clojure-5881D8?style=flat&logo=clojure&logoColor=white) | [clojure](https://github.com/curtcox/256t.org/tree/main/implementations/clojure) |
| ClojureScript | ![ClojureScript](https://img.shields.io/badge/ClojureScript-5881D8?style=flat&logo=clojure&logoColor=white) | [clojurescript](https://github.com/curtcox/256t.org/tree/main/implementations/clojurescript) |
| CMake | ![CMake](https://img.shields.io/badge/CMake-064F8C?style=flat&logo=cmake&logoColor=white) | [cmake](https://github.com/curtcox/256t.org/tree/main/implementations/cmake) |
| Crystal | ![Crystal](https://img.shields.io/badge/Crystal-000000?style=flat&logo=crystal&logoColor=white) | [crystal](https://github.com/curtcox/256t.org/tree/main/implementations/crystal) |
| D | ![D](https://img.shields.io/badge/D-B03931?style=flat&logo=d&logoColor=white) | [d](https://github.com/curtcox/256t.org/tree/main/implementations/d) |
| Dart | ![Dart](https://img.shields.io/badge/Dart-0175C2?style=flat&logo=dart&logoColor=white) | [dart](https://github.com/curtcox/256t.org/tree/main/implementations/dart) |
| Deno | ![Deno](https://img.shields.io/badge/Deno-000000?style=flat&logo=deno&logoColor=white) | [deno](https://github.com/curtcox/256t.org/tree/main/implementations/deno) |
| ECMAScript | ![ECMAScript](https://img.shields.io/badge/ECMAScript-F7DF1E?style=flat&logo=javascript&logoColor=black) | [ecmascript](https://github.com/curtcox/256t.org/tree/main/implementations/ecmascript) |
| Elixir | ![Elixir](https://img.shields.io/badge/Elixir-4B275F?style=flat&logo=elixir&logoColor=white) | [elixir](https://github.com/curtcox/256t.org/tree/main/implementations/elixir) |
| Elm | ![Elm](https://img.shields.io/badge/Elm-60B5CC?style=flat&logo=elm&logoColor=white) | [elm](https://github.com/curtcox/256t.org/tree/main/implementations/elm) |
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
| Unison | ![Unison](https://img.shields.io/badge/Unison-4B32C3?style=flat) | [unison](https://github.com/curtcox/256t.org/tree/main/implementations/unison) |
| Zig | ![Zig](https://img.shields.io/badge/Zig-F7A41D?style=flat&logo=zig&logoColor=white) | [zig](https://github.com/curtcox/256t.org/tree/main/implementations/zig) |
