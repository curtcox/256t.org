<!-- i18n:source sha256:6826a083676fcaea -->
# [256t.org](https://curtcox.github.io/256t.org/)

<p align="center">
  <img src="examples/256t_logo.png" alt="Logotipo do 256t.org" width="200">
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="examples/256t_qr_code.png" alt="QR code do 256t.org" width="200">
</p>

<p align="center">
  <img src="examples/256t.png" alt="Conceito de armazenamento endereçável por conteúdo do 256t.org" width="800">
</p>

## O quê?

O 256t.org é um domínio dedicado a ser uma especificação pública para um tipo específico de armazenamento endereçável por conteúdo.
Nesse esquema, o último elemento de até 94 caracteres no caminho de uma URL define o conteúdo daquela URL.
Em algum momento no futuro, ele poderá evoluir para ser também um serviço público para publicar conteúdo usando o esquema.
No entanto, isso está atualmente fora do escopo deste site.

## Por quê?

Por que o [256t.org](https://256t.org)?
Um padrão simples para um armazenamento genérico endereçável por conteúdo me parece útil de modo geral.

## Como?

Todo caminho de 94 caracteres pode ser usado para obter o conteúdo que corresponde ao tamanho e ao hash especificados.
Se nenhum conteúdo estiver disponível, um 404 é retornado.

Um hash SHA-512 tem 512 bits, ou 64 bytes.
64 bytes podem ser armazenados em uma string base64 de 86 caracteres.
```
(64 * 8) / 6 = 85.333... ~= 86
```

Conteúdo de 64 bytes ou menos pode ser armazenado diretamente em espaço igual ou menor.
Nesses casos, o próprio conteúdo deve ser codificado em base64 e usado com o mínimo de preenchimento, em vez de usar seu hash.

Uma string base64 de 8 caracteres pode armazenar 48 bits.
```
2^48 = 2^40 * 2^8
  = 2^10 * 2^10 * 2^10 * 2^10 * 2^8
  = 2^8  * 2^10 * 2^10 * 2^10 * 2^10
  = 256    K      M      G      T
```

O que segue pode ser tratado como verdadeiro o bastante, apesar de ser falso:
- Um caminho de 94 caracteres determina o conteúdo de forma única. (No entanto, isso é totalmente verdadeiro para conteúdo com menos de 64 bytes.)
- O conteúdo é imutável. (Ele poderia ser substituído por um conteúdo diferente que ainda corresponda à descrição.)
- O conteúdo pode ser armazenado em cache com segurança indefinidamente.

Assim, todas as metainformações HTTP, como cabeçalhos e ETags, indicarão que o conteúdo pode ser armazenado em cache indefinidamente.

A etiqueta de conteúdo de 94 caracteres consiste em um prefixo de tamanho de 8 caracteres seguido de um hash de 86 caracteres.

|         | tamanho do conteúdo | hash do conteúdo | o próprio conteúdo |
|---------|-----------------------|----------------------|-----------------------|
| quando  | sempre                | length(content) > 64 | length(content) <= 64 |
| início  | 1                     | 9                    | 9                     |
| fim     | 8                     | 94                   | 94                    |
| tamanho | 8                     | 86                   | 0 a 86                |
| formato | Base64                | Base64               | Base64                |
| info    | length(content)       | sha-512(content)     | content               |

### Base64

Mais especificamente, [Base64](https://pt.wikipedia.org/wiki/Base64) [segura para nomes de arquivo e URLs](https://datatracker.ietf.org/doc/html/rfc4648#section-5), também conhecida como base64url.

## CID

Essa string base64 de 94 caracteres ou menos, que identifica um conteúdo, será chamada de identificador de conteúdo, ou CID (do inglês *content identifier*).

## Quando

## Onde

Qualquer servidor pode expor uma URL base com conteúdos que sigam esta especificação.
Infelizmente, quais servidores hospedam quais conteúdos e como encontrá-los está fora do escopo deste texto.
Este servidor hospeda um pequeno conjunto de CIDs [aqui](cids/).

## Fora do escopo deste texto

- Encontrar
  - Como encontro algum lugar que publique CIDs?
- Publicar
  - Onde e como publico CIDs?
- Metadados
  - Quais metadados (cabeçalhos HTTP, por exemplo) devem acompanhar o conteúdo de um CID?
- Permissões
  - Alguns conteúdos podem ser restritos a determinadas pessoas ou entidades solicitantes.
  - Alguns conteúdos podem ter propriedade contestada, propriedade diferente em jurisdições diferentes ou direitos de distribuição contestados.
  - Para servir ou não um conteúdo, essas questões precisam ser decididas.
  - Como essas coisas são decididas?

Essas coisas estão fora do escopo deste texto.

## Ferramentas

- [Calculadora de hash](hash.html) — digite um texto e veja instantaneamente seu hash SHA-512 codificado em Base64URL.
- [Exemplos de CID](examples.html) — selecione arquivos de exemplo e verifique seus CIDs usando a implementação em ECMAScript.
- [Exemplos de CID (Elm)](examples-elm.html) — selecione arquivos de exemplo e verifique seus CIDs usando a implementação em Elm.
- [Verificação de CID de arquivo](check.html) — arraste e solte arquivos para analisar seu conteúdo e calcular seu CID.
- [Download por CID](download.html) — baixe e verifique conteúdo de um servidor compatível com o 256t.org usando um CID.
- [Resolvedor de URI t256](resolve.html) — abra links t256 ou compatíveis com navegadores usando conteúdo embutido, provedores públicos ou seu auxiliar de desktop; opcionalmente, ative o tratamento de links no navegador.
- [Suporte para desktop e navegadores](browser-support/README.md) — pastas selecionáveis, unidades de rede, repositórios autenticados do GitHub, um carregador de páginas verificado, gateway e extensões opcionais.
- [Plano ativo de suporte a navegadores](browser-support/README.md#active-implementation-plan) — reescrita de links e palavras-chave na extensão, aliases de compatibilidade com manipuladores web e registro nativo de esquemas no sistema operacional em navegadores de desktop.

## Arquivos de exemplo

O diretório [examples/](examples/) contém arquivos de exemplo para testar a geração e a verificação de CIDs. Entre eles há recursos com a marca 256t que demonstram a especificação com vários tipos de mídia:

| Arquivo | Descrição |
|------|-------------|
| [256t.mp4](examples/256t.mp4) | Vídeo explicando a especificação de armazenamento endereçável por conteúdo do 256t.org (ou no [YouTube](https://www.youtube.com/watch?v=GrzonG2up5c)) (em inglês) |
| [256t.png](examples/256t.png) | Representação visual do conceito do 256t.org |
| [256t_Content_Addressable_Storage_Specification.pdf](examples/256t_Content_Addressable_Storage_Specification.pdf) | Documentação em PDF da especificação completa (em inglês) |
| [256t_and_the_End_of_Broken_Links.m4a](examples/256t_and_the_End_of_Broken_Links.m4a) | Discussão em áudio sobre como o 256t.org lida com links quebrados (em inglês) |
| [256t_logo.jpg](examples/256t_logo.jpg) | Logotipo do 256t.org (formato JPEG) |
| [256t_logo.png](examples/256t_logo.png) | Logotipo do 256t.org (formato PNG) |
| [256t_qr_code.png](examples/256t_qr_code.png) | QR code com link para o 256t.org |

Esses arquivos podem ser usados com a ferramenta [Exemplos de CID](examples.html) ou com a ferramenta [Verificação de CID de arquivo](check.html) para verificar seus identificadores de conteúdo.

## Esquema de URI

Um rascunho de RFC que propõe o esquema de URI `t256:` — uma URL que contém apenas um CID, sem host nem caminho — está disponível em [rfc/draft-cox-t256-uri-scheme-00.md](rfc/draft-cox-t256-uri-scheme-00.md). (A grafia `256t:` não é um esquema de URI válido porque a RFC 3986 exige que os esquemas comecem com uma letra.)

## Publicação

Para informações sobre como implantar e publicar armazenamento endereçável por conteúdo usando CIDs, consulte [Publicação e armazenamento](publishing.html).

## Perguntas frequentes

Tem dúvidas sobre o 256t.org? Confira a página de [Perguntas frequentes](faq.html) para ver respostas a perguntas comuns sobre CIDs, implementação, segurança e mais.

## E as colisões?

Há alguns tipos diferentes de colisão que é importante distinguir:
- acidental — puramente por acaso
- adversarial — alguém tentou provocá-la
- existente — um CID foi produzido a partir de conteúdos diferentes
- problemática — usar o CID para obter um conteúdo retornou o conteúdo errado

As chances de uma colisão problemática são bem baixas. Há duas maneiras de minimizá-las:
- Sempre verifique o conteúdo do CID. Há muitas implementações para fazer isso. É mais fácil simplesmente mentir do que engendrar uma colisão.
- Reduza os adversários. Se ninguém estiver colocando conteúdo problemático onde você possa aceitá-lo, sobram apenas os acidentes.

Para mim, tudo bem simplesmente ignorar colisões problemáticas acidentais.

## Implementações

A especificação do 256t.org foi implementada em várias linguagens de programação. Cada implementação fornece utilitários para gerar e verificar identificadores de conteúdo (CIDs).

| Linguagem | Selo | Código |
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
