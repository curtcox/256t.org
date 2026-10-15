<!-- i18n:source sha256:c715377f5cccfb71 -->
# 用于 256t 内容标识符的 't256' URI 协议

```
Internet-Draft                                                    C. Cox
Intended status: Informational                                  256t.org
Expires: 31 January 2027                                    30 July 2026

           The 't256' URI Scheme for 256t Content Identifiers
                      draft-cox-t256-uri-scheme-00
```

## 摘要

本文档定义了 't256' 统一资源标识符（URI）协议。一个 't256' URI 恰好包含一个 256t 内容标识符（CID），其后可以跟一个媒体类型后缀，这种后缀与 HTTP URL 末尾常见的后缀属于同一类（".html"、".png"、".tar.gz"）。CID 通过内容的长度，以及内容的 SHA-512 哈希（或者对于 64 字节及以下的内容，内容本身）来命名内容。因此，'t256' URI 对内容的标识独立于任何位置、协议或服务器，任何持有该内容的一方都可以验证内容与 URI 是否匹配。如果存在后缀，它表示应以何种媒体类型来解释该内容。

## 本备忘录的状态

本 Internet-Draft 的提交完全符合 BCP 78 和 BCP 79 的规定。

Internet-Draft 是互联网工程任务组（IETF）的工作文档。请注意，其他团体也可能以 Internet-Draft 的形式分发工作文档。当前 Internet-Draft 的列表见 https://datatracker.ietf.org/drafts/current/。

Internet-Draft 是有效期最长为六个月的草案文档，随时可能被其他文档更新、替换或废弃。将 Internet-Draft 用作参考资料，或以“进行中的工作”以外的方式引用它们，都是不恰当的。

本 Internet-Draft 将于 2027 年 1 月 31 日到期。

## 版权声明

Copyright (c) 2026 IETF Trust 以及被列为文档作者的人员。保留所有权利。

## 1. 引言

256t 规范 [256T] 定义了一种简单的内容寻址存储形式。内容由内容标识符（CID）命名：CID 是一个最多 94 个字符的字符串，由 8 个字符的长度前缀，后接内容的 SHA-512 哈希的 base64url 编码组成；当内容不超过 64 字节时，后接的是内容本身的 base64url 编码。

CID 与位置无关。无论副本保存在哪台服务器、哪个文件系统或哪种介质上，同一个 CID 都命名同一份内容。现有做法把 CID 作为 'https' URI 的最后一个路径段嵌入，这会把标识符绑定到某个特定主机上。一个只携带 CID 的 URI 协议，使得引用内容时完全不必指明任何主机，解析工作留给使用方完成。HTTP URL 也常以文件扩展名（".html"、".png"）结尾，服务器和客户端据此推断媒体类型；'t256' URI 可以（MAY）携带同类后缀，以表明应如何解释它所命名的内容（第 3.1 节）。

最自然的协议名应当是 "256t"，与规范名称一致。但这种拼写不可用：URI 语法
[RFC3986] 要求协议名以字母开头。因此本文档定义了协议名 "t256"，这是保留该名称的最短有效拼写。第 6 节讨论了这一点以及其他曾被考虑但未被采纳的方案。

## 2. 约定与术语

本文档中的关键词 "MUST"、"MUST NOT"、"REQUIRED"、"SHALL"、"SHALL NOT"、"SHOULD"、"SHOULD NOT"、"RECOMMENDED"、"NOT RECOMMENDED"、"MAY" 和"OPTIONAL"，当且仅当它们如此处所示以全大写形式出现时，应按照 BCP 14 [RFC2119]
[RFC8174] 的描述进行解释。本译文以“必须（MUST）”“不得（MUST NOT）” “应当（SHOULD）”“可以（MAY）”等形式标注这些关键词。

"base64url" 指 [RFC4648] 第 5 节中对 URL 和文件名安全的 base 64 字母表，使用时*不带*填充字符（"="）。

"CID" 指第 3 节定义的 256t 内容标识符。

“内容”（Content）指 CID 所命名的八位字节序列。

“媒体类型”（Media type）指 HTTP Content-Type 头字段 [RFC9110] 中使用的互联网媒体类型（MIME 类型）。

## 3. 协议语法

一个 't256' URI 由协议名、冒号、单个 CID 以及可选的媒体类型后缀组成。该协议是不透明的：它没有授权（authority）部分、没有路径层次、没有查询部分，也没有定义片段语义。

用 [RFC5234] 的扩展巴科斯-诺尔范式（ABNF）表示：

```
t256-URI       = "t256:" cid [ suffix ]

cid            = length-prefix payload

length-prefix  = 8base64url-char

payload        = 0*86base64url-char

base64url-char = ALPHA / DIGIT / "-" / "_"

suffix         = 1*( "." label )

label          = 1*( ALPHA / DIGIT )
```

除 ABNF 之外，还适用以下约束：

1.  长度前缀是内容八位字节长度的无填充 base64url 编码，该长度表示为一个 6 个八位字节的大端无符号整数。6 个八位字节恰好编码为 8 个 base64url 字符，没有填充。设 N 为解码后的长度。

2.  如果 N 小于或等于 64，载荷就是内容本身的无填充 base64url 编码。其解码值必须（MUST）恰好为 N 个八位字节长。

3.  如果 N 大于 64，载荷就是内容的 SHA-512 哈希 [FIPS180-4] 的无填充 base64url 编码。它的长度始终恰好为 86 个字符。

4.  由于八位字节串的无填充 base64url 编码，其长度永远不会模 4 余 1，因此长度模 4 余 1 的载荷是无效的。

由于 "." 不在 base64url 字母表中，第一个 "." 字符（如果有）会明确无误地结束 CID 并开始后缀。因此，CID 的长度在 8 个字符（空内容）到 94 个字符之间，不带后缀的't256' URI 长度在 13 到 99 个字符之间。

按照 [RFC3986] 的要求，协议名不区分大小写，并且应当（SHOULD）以小写书写。CID 区分大小写：base64url 区分大写和小写，仅大小写不同的两个 CID 命名的是不同的内容。规范化处理不得（MUST NOT）改变 CID 的大小写。

由于 CID 和后缀的每个字符都属于 [RFC3986] 的 "unreserved"（非保留）字符集，'t256' URI 永远不需要百分号编码，生成 't256' URI 的一方不得（MUST NOT）对其任何部分进行百分号编码。使用方如果在 't256' URI 中遇到百分号编码的三元组，必须（MUST）将该 URI 视为无效，而不是对其进行解码。

### 3.1. 媒体类型后缀

可选的后缀携带一个隐含的媒体类型，方式与 HTTP URL 路径末尾的文件扩展名通常所起的作用相同。它由一个或多个以点分隔的标签组成；媒体类型由最后一个标签，按照 HTTP 服务器普遍使用的扩展名到媒体类型的映射来确定（例如，"html" 表示 text/html，"png" 表示 image/png，"json" 表示 application/json）。更前面的标签（如"t256:<cid>.tar.gz" 中的标签）不参与该映射，但可以（MAY）像在 HTTP 实践中那样提示额外的处理方式。

本文档不定义后缀注册表。生成方应当（SHOULD）使用那些到 IANA “Media Types”注册表 [RFC6838] 中已注册媒体类型的映射被广泛使用的后缀。

生成方应当（SHOULD）以小写书写后缀。使用方应当（SHOULD）以不区分大小写的方式把后缀映射为媒体类型，这与常见 HTTP 服务器的行为一致。不认识最后一个标签的使用方必须（MUST）按照没有后缀的情况来处理该 URI。

后缀是关于解释方式的声明，而不是关于身份的声明：它对 URI 命名哪份内容没有任何影响，第 8 节说明了它不受验证保护。

## 4. 协议语义

't256' URI 命名的是内容，而不是位置。它命名哪份内容完全由 CID 决定；后缀（如果存在）在验证、提取或获取中都不起作用。可对 't256' URI 执行的操作有：

验证：给定候选内容，使用方按照第 3 节计算该内容的 CID，并将其作为区分大小写的字符串与 URI 中的 CID（不含任何后缀）进行比较。当且仅当两个字符串相等时，内容与 URI 匹配。

直接提取：如果解码后的长度前缀不超过 64，URI 本身就包含了内容，使用方可以（MAY）通过对载荷进行 base64url 解码直接获得内容，无需任何获取步骤。

获取：使用方可以（MAY）尝试通过任何可用机制（HTTP 或其他方式）获取内容。其中一种机制是兼容 256t 的服务器：给定一个基础 URL，获取就是对该基础 URL 发起 HTTP GET 请求，并把 CID 作为最后一个路径段附加在后面 [256T]。使用方如何发现合适的服务器不在本文档的范围之内。以任何方式获取内容的使用方，在把内容视为 URI 所命名的内容之前，都必须（MUST）按上述方法对其进行验证。

解释：处理通过带后缀的 URI 获得的内容时，使用方应当（SHOULD）像对待 Content-Type 为第 3.1 节所述隐含媒体类型的 HTTP 响应载荷一样对待该内容——据此进行渲染、分派给处理程序并应用安全策略。当没有后缀（或后缀无法识别）时，URI 不隐含任何媒体类型，使用方需通过其他方式确定如何解释内容。

当且仅当两个 't256' URI 的 CID 逐码点完全相同时，它们命名同一份内容。当且仅当此外它们的后缀在对协议名和后缀做大小写规范化之后也相同时，这两个 't256' URI 才是等价的。命名同一内容但携带不同后缀的 URI 并不等价，原因与以不同 HTTP Content-Type 头字段提供的相同八位字节不可互换相同。

## 5. 编码注意事项

协议特定部分只能使用 base64url 字母表中的 64 个字符，以及可选后缀中的 "."，这些字符在 [RFC3986] 中都是非保留字符，且都可以用 7 位 US-ASCII 表示。因此不涉及百分号编码、国际化或字符集方面的问题。't256' URI 的 URI 形式与 IRI 形式完全相同。

## 6. 未采纳的替代方案

曾考虑过以下替代设计：

"256t:"：与规范名称完全一致，但违反了 [RFC3986] 的协议名语法，该语法要求第一个字符必须是字母。标准 URI 解析器会拒绝它，IANA 也无法注册它。

"t256://<cid>"：这种带授权部分的形式把 CID 放在主机位置。主机名不区分大小写，通用 URI 处理会把它转为小写，从而破坏 base64url CID。不透明形式完全避免了这一类损坏。

"cid:<cid>"：'cid' 协议已被注册用于 MIME Content-ID 引用 [RFC2392]，尽管术语上恰好相同，它也不可用。

"ni:" [RFC6920]：命名信息（Named Information）协议同样通过哈希命名内容，但它有自己不兼容的结构，没有容纳 256t 长度前缀或小内容内联规则的位置。256t CID 无法无损地表示为 'ni' URI。

"urn:256t:<cid>"：[RFC8141] 允许 URN 命名空间标识符以数字开头，因此这种形式在语法上是有效的，可以在需要 URN 的场合另行采用。之所以没有把它作为主要形式，是因为它过于冗长，而且在已部署的软件中没有任何解析方案。

## 7. 互操作性注意事项

't256' 协议是不透明的，因此执行通用层次化处理（相对引用解析、路径合并、点段移除）的软件没有可操作的对象；'t256' URI 不能用作相对引用的基础 URI。

显示 CID 的应用应当（SHOULD）原样呈现它们。只有在完整 URI 仍可复制的情况下，才可以为了显示而截断，因为截断后的 CID 不命名任何东西。

## 8. 安全注意事项

抗碰撞性：对于超过 64 个八位字节的内容，CID 与内容之间的绑定依赖于 SHA-512 的抗碰撞性。严格来说，CID 并不能唯一确定内容；只是以目前已知的技术，在计算上不可能构造出两份具有相同 CID 的内容。对于 64 个八位字节及以下的内容，这种绑定是精确的，因为 URI 本身就包含了内容。

强制验证：对恶意或出错的服务器来说，返回错误的内容远比设计出一次哈希碰撞容易。因此第 4 节的验证要求是 MUST 级别的：跳过验证的使用方会失去该协议提供的全部完整性保证，把 't256' URI 降格为一个未经认证的文件名。

小内容的机密性：对于 64 个八位字节及以下的内容，URI *就是*内容，只是经过了 base64url 编码。在记录、缓存、显示或分享 URI 的任何场合，这类 URI 都必须（MUST）与内容本身同等敏感地对待。Base64url 是一种编码，而不是加密。

未经认证的媒体类型：后缀不是 CID 的输入，因此不受验证保护。任何能够在传输中或存储时修改 URI 的人，都可以更改或删除其后缀而不会使 URI 失效，从而改变使用方对（未改变的）内容的解释方式——例如把 text/plain 提升为 text/html，进而使脚本得以执行。使用方必须（MUST）采取与对待不可信的 HTTP Content-Type 头字段相同的防御措施，例如限制内容嗅探，并在渲染活动内容类型之前应用与上下文相适应的安全策略。

长度泄露：每个 CID 都会泄露内容的确切长度，而长度本身就可能透露信息（例如，用于在一小组候选文档之间做出区分）。

未涉及可用性：CID 不携带任何关于内容可在何处获取、或是否能够获取的信息。持有有效的 't256' URI 并不能保证有任何服务器会提供该内容。

可缓存性：经过验证的内容可以（MAY）以其 CID 为键无限期缓存。缓存必须（MUST）以完整 CID 为键，并且必须（MUST）在插入内容之前对其进行验证。

## 9. IANA 注意事项

请求 IANA 按照 [RFC7595] 的指南，在“统一资源标识符（URI）协议”注册表中注册以下 URI 协议。

```
Scheme name:                 t256
Status:                      Provisional
Applications/protocols:      256t content addressable storage
                             clients and servers; any application
                             referencing content by 256t CID.
Contact:                     Curt Cox <curtcox@gmail.com>
Change controller:           Curt Cox
References:                  This document; https://256t.org
```

## 10. 参考文献

### 10.1. 规范性参考文献

- [RFC2119]  Bradner, S., "Key words for use in RFCs to Indicate Requirement Levels", BCP 14, RFC 2119, March 1997.
- [RFC3986]  Berners-Lee, T., Fielding, R., and L. Masinter, "Uniform Resource Identifier (URI): Generic Syntax", STD 66, RFC 3986, January 2005.
- [RFC4648]  Josefsson, S., "The Base16, Base32, and Base64 Data Encodings", RFC 4648, October 2006.
- [RFC5234]  Crocker, D. and P. Overell, "Augmented BNF for Syntax Specifications: ABNF", STD 68, RFC 5234, January 2008.
- [RFC8174]  Leiba, B., "Ambiguity of Uppercase vs Lowercase in RFC 2119 Key Words", BCP 14, RFC 8174, May 2017.
- [FIPS180-4]  National Institute of Standards and Technology, "Secure Hash Standard (SHS)", FIPS PUB 180-4, August 2015.
- [256T]  "256t.org Content Addressable Storage Specification", https://256t.org.

### 10.2. 资料性参考文献

- [RFC2392]  Levinson, E., "Content-ID and Message-ID Uniform Resource Locators", RFC 2392, August 1998.
- [RFC6838]  Freed, N., Klensin, J., and T. Hansen, "Media Type Specifications and Registration Procedures", BCP 13, RFC 6838, January 2013.
- [RFC6920]  Farrell, S., Kutscher, D., Dannewitz, C., Ohlman, B., Keranen, A., and P. Hallam-Baker, "Naming Things with Hashes", RFC 6920, April 2013.
- [RFC7595]  Thaler, D., Hansen, T., and T. Hardie, "Guidelines and Registration Procedures for URI Schemes", BCP 35, RFC 7595, June 2015.
- [RFC8141]  Saint-Andre, P. and J. Klensin, "Uniform Resource Names (URNs)", RFC 8141, April 2017.
- [RFC9110]  Fielding, R., Nottingham, M., and J. Reschke, "HTTP Semantics", STD 97, RFC 9110, June 2022.

## 附录 A. 示例

所有示例均使用 [256T] 中的参考实现生成。

空内容（0 个八位字节；载荷为空）：

```
t256:AAAAAAAA
```

13 个八位字节的字符串 "Hello, World!"（内容以内联方式携带；载荷"SGVsbG8sIFdvcmxkIQ" 经 base64url 解码后即为该内容）：

```
t256:AAAAAAANSGVsbG8sIFdvcmxkIQ
```

65 个八位字节，每个都是字母 "a"（超过了 64 个八位字节的阈值，因此载荷是 86 个字符的 base64url SHA-512 哈希，URI 达到其最大长度 99 个字符）：

```
t256:AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg
```

再次使用 "Hello, World!" 内容，带一个隐含媒体类型 text/plain 的后缀。CID 不变：两个 URI 命名的是相同的 13 个八位字节，而这个 URI 还额外说明了应如何解释它们：

```
t256:AAAAAAANSGVsbG8sIFdvcmxkIQ.txt
```

一个多标签后缀，这里仅为说明语法而用在上一个示例的 CID 上。隐含的媒体类型来自最后一个标签（"gz"）；与 HTTP 实践一样，更前面的标签表示解压后的内容将是一个 tar 归档。后缀声明的是一种解释方式；它不会改变 CID 所命名的内容，也不对其做任何证明：

```
t256:AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg.tar.gz
```

## 作者地址

```
Curt Cox
Email: curtcox@gmail.com
URI:   https://256t.org
```
