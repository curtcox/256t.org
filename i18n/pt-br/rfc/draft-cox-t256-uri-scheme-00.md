<!-- i18n:source sha256:c715377f5cccfb71 -->
# O esquema de URI 't256' para identificadores de conteúdo 256t

```
Internet-Draft                                                    C. Cox
Intended status: Informational                                  256t.org
Expires: 31 January 2027                                    30 July 2026

           The 't256' URI Scheme for 256t Content Identifiers
                      draft-cox-t256-uri-scheme-00
```

## Resumo

Este documento define o esquema de Identificador Uniforme de Recurso (URI) 't256'. Uma URI 't256' contém exatamente um identificador de conteúdo (CID) 256t, opcionalmente seguido de um sufixo de tipo de mídia do tipo comumente usado no final de URLs HTTP (".html", ".png", ".tar.gz"). Um CID nomeia um conteúdo pelo seu tamanho e pelo seu hash SHA-512 ou, para conteúdo de 64 bytes ou menos, pelo próprio conteúdo. Uma URI 't256' identifica, portanto, o conteúdo independentemente de qualquer local, protocolo ou servidor, e qualquer parte que tenha o conteúdo pode verificar se ele corresponde à URI. O sufixo, quando presente, indica o tipo de mídia com o qual o conteúdo deve ser interpretado.

## Status deste memorando

Este Internet-Draft é submetido em total conformidade com as disposições do BCP 78 e do BCP 79.

Internet-Drafts são documentos de trabalho da Internet Engineering Task Force (IETF). Observe que outros grupos também podem distribuir documentos de trabalho como Internet-Drafts. A lista atual de Internet-Drafts está em https://datatracker.ietf.org/drafts/current/.

Internet-Drafts são rascunhos válidos por no máximo seis meses e podem ser atualizados, substituídos ou tornados obsoletos por outros documentos a qualquer momento. Não é apropriado usar Internet-Drafts como material de referência nem citá-los de outra forma que não como "trabalho em andamento".

Este Internet-Draft expira em 31 de janeiro de 2027.

## Aviso de direitos autorais

Copyright (c) 2026 IETF Trust e as pessoas identificadas como autoras do documento. Todos os direitos reservados.

## 1. Introdução

A especificação 256t [256T] define uma forma simples de armazenamento endereçável por conteúdo. O conteúdo é nomeado por um identificador de conteúdo (CID): uma string de no máximo 94 caracteres, composta por um prefixo de tamanho de 8 caracteres seguido da codificação base64url do hash SHA-512 do conteúdo ou, quando o conteúdo tem 64 bytes ou menos, da codificação base64url do próprio conteúdo.

CIDs são independentes de localização. O mesmo CID nomeia o mesmo conteúdo, não importa qual servidor, sistema de arquivos ou mídia guarde uma cópia. A prática atual incorpora CIDs como o segmento final do caminho de uma URI 'https', o que amarra o identificador a um host específico. Um esquema de URI que carrega apenas o CID permite referenciar conteúdo sem nomear host algum, deixando a resolução a cargo do consumidor. URLs HTTP também costumam terminar com uma extensão de nome de arquivo (".html", ".png"), a partir da qual servidores e clientes inferem um tipo de mídia; uma URI 't256' PODE (MAY) carregar o mesmo tipo de sufixo para indicar como o conteúdo que ela nomeia deve ser interpretado (Seção 3.1).

O nome natural do esquema seria "256t", igual ao nome da especificação. Essa grafia não está disponível: a sintaxe de URI [RFC3986] exige que um esquema comece com uma letra. Por isso, este documento define o esquema "t256", a grafia válida mais curta que preserva o nome. A Seção 6 discute isso e outras alternativas que foram consideradas e rejeitadas.

## 2. Convenções e terminologia

As palavras-chave "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED", "MAY" e "OPTIONAL" neste documento devem ser interpretadas conforme descrito no BCP 14 [RFC2119] [RFC8174] quando, e somente quando, aparecem em letras maiúsculas, como mostrado aqui. Nesta tradução, elas aparecem como DEVE (MUST), NÃO DEVE (MUST NOT), DEVERIA (SHOULD), PODE (MAY) etc.

"base64url" significa o alfabeto base 64 seguro para URLs e nomes de arquivo da Seção 5 da [RFC4648], usado *sem* caracteres de preenchimento ("=").

"CID" significa um identificador de conteúdo 256t, conforme definido na Seção 3.

"Conteúdo" significa a sequência de octetos que um CID nomeia.

"Tipo de mídia" significa um tipo de mídia da Internet (tipo MIME), como usado no campo de cabeçalho HTTP Content-Type [RFC9110].

## 3. Sintaxe do esquema

Uma URI 't256' é composta pelo nome do esquema, dois-pontos, um único CID e um sufixo opcional de tipo de mídia. O esquema é opaco: não tem componente de autoridade, nem hierarquia de caminho, nem componente de consulta, nem semântica de fragmento definida.

Na Forma Aumentada de Backus-Naur (ABNF) da [RFC5234]:

```
t256-URI       = "t256:" cid [ suffix ]

cid            = length-prefix payload

length-prefix  = 8base64url-char

payload        = 0*86base64url-char

base64url-char = ALPHA / DIGIT / "-" / "_"

suffix         = 1*( "." label )

label          = 1*( ALPHA / DIGIT )
```

Além da ABNF, aplicam-se as seguintes restrições:

1.  O prefixo de tamanho é a codificação base64url sem preenchimento do tamanho do conteúdo em octetos, representado como um inteiro sem sinal big-endian de 6 octetos. Seis octetos são codificados em exatamente 8 caracteres base64url, sem preenchimento. Seja N o tamanho decodificado.

2.  Se N for menor ou igual a 64, a carga é a codificação base64url sem preenchimento do próprio conteúdo. Seu valor decodificado DEVE (MUST) ter exatamente N octetos.

3.  Se N for maior que 64, a carga é a codificação base64url sem preenchimento do hash SHA-512 [FIPS180-4] do conteúdo. Ela sempre tem exatamente 86 caracteres.

4.  Como as codificações base64url sem preenchimento de sequências de octetos nunca têm tamanho congruente a 1 módulo 4, uma carga de tamanho congruente a 1 módulo 4 é inválida.

Como "." não faz parte do alfabeto base64url, o primeiro caractere ".", se houver, encerra o CID e inicia o sufixo sem ambiguidade. Um CID tem, portanto, entre 8 caracteres (conteúdo vazio) e 94 caracteres, e uma URI 't256' sem sufixo tem entre 13 e 99 caracteres.

O nome do esquema não diferencia maiúsculas de minúsculas, como exige a [RFC3986], e DEVERIA (SHOULD) ser escrito em minúsculas. O CID diferencia maiúsculas de minúsculas: o base64url distingue letras maiúsculas de minúsculas, e dois CIDs que diferem apenas nisso nomeiam conteúdos diferentes. Processos de normalização NÃO DEVEM (MUST NOT) alterar a caixa do CID.

Como todo caractere de um CID e de um sufixo pertence ao conjunto "unreserved" (não reservado) da [RFC3986], uma URI 't256' nunca exige codificação percentual, e quem produz uma URI 't256' NÃO DEVE (MUST NOT) aplicar codificação percentual a nenhuma parte dela. Um consumidor que encontre um tripleto com codificação percentual em uma URI 't256' DEVE (MUST) tratar a URI como inválida, em vez de decodificá-la.

### 3.1. Sufixo de tipo de mídia

O sufixo opcional carrega um tipo de mídia implícito, da mesma forma que uma extensão de nome de arquivo no final do caminho de uma URL HTTP costuma fazer. Ele consiste em um ou mais rótulos separados por pontos; o tipo de mídia é determinado a partir do último rótulo, usando os mapeamentos de extensão para tipo de mídia de uso comum em servidores HTTP (por exemplo, "html" implica text/html, "png" implica image/png, "json" implica application/json). Rótulos anteriores, como em "t256:<cid>.tar.gz", não participam do mapeamento, mas PODEM (MAY) orientar um tratamento adicional, como ocorre na prática HTTP.

Este documento não define um registro de sufixos. Os produtores DEVERIAM (SHOULD) usar sufixos cujo mapeamento para um tipo de mídia registrado no registro "Media Types" da IANA [RFC6838] seja amplamente utilizado.

Os produtores DEVERIAM (SHOULD) escrever os sufixos em minúsculas. Os consumidores DEVERIAM (SHOULD) mapear sufixos para tipos de mídia sem diferenciar maiúsculas de minúsculas, como fazem os servidores HTTP comuns. Um consumidor que não reconheça o último rótulo DEVE (MUST) processar a URI como se não houvesse sufixo.

O sufixo é uma declaração sobre interpretação, não sobre identidade: ele não tem efeito sobre qual conteúdo a URI nomeia, e a Seção 8 explica que ele não é protegido pela verificação.

## 4. Semântica do esquema

Uma URI 't256' nomeia um conteúdo; ela não nomeia um local. Qual conteúdo ela nomeia é determinado inteiramente pelo CID; o sufixo, se presente, não tem papel algum na verificação, na extração ou na obtenção. As operações disponíveis sobre uma URI 't256' são:

Verificação:  Dado um conteúdo candidato, o consumidor calcula o CID desse conteúdo conforme a Seção 3 e o compara, como uma string que diferencia maiúsculas de minúsculas, com o CID da URI (excluindo qualquer sufixo). O conteúdo corresponde à URI se, e somente se, as strings forem iguais.

Extração direta:  Se o prefixo de tamanho decodificado for 64 ou menos, a própria URI contém o conteúdo, e o consumidor PODE (MAY) obtê-lo decodificando a carga em base64url, sem nenhuma etapa de obtenção.

Obtenção:  O consumidor PODE (MAY) tentar obter o conteúdo por qualquer mecanismo disponível, HTTP ou outro. Um desses mecanismos é um servidor compatível com o 256t: dada uma URL base, a obtenção é um GET HTTP da URL base com o CID acrescentado como segmento final do caminho [256T]. Como o consumidor descobre servidores adequados está fora do escopo deste documento. Um consumidor que obtenha o conteúdo por qualquer meio DEVE (MUST) verificá-lo como descrito acima antes de tratá-lo como o conteúdo nomeado pela URI.

Interpretação:  Um consumidor que processe conteúdo obtido por meio de uma URI com sufixo DEVERIA (SHOULD) tratar o conteúdo como trataria a carga de uma resposta HTTP cujo Content-Type fosse o tipo de mídia implícito da Seção 3.1 — renderizando, despachando para manipuladores e aplicando a política de segurança de acordo. Quando não há sufixo (ou o sufixo não é reconhecido), a URI não implica tipo de mídia algum, e o consumidor determina como interpretar o conteúdo por outros meios.

Duas URIs 't256' nomeiam o mesmo conteúdo se, e somente se, seus CIDs forem idênticos ponto de código a ponto de código. Duas URIs 't256' são equivalentes se, e somente se, além disso, seus sufixos forem idênticos após normalizar a caixa do nome do esquema e do sufixo. URIs que nomeiam o mesmo conteúdo, mas carregam sufixos diferentes, não são equivalentes, pelo mesmo motivo que octetos idênticos servidos com campos de cabeçalho HTTP Content-Type diferentes não são intercambiáveis.

## 5. Considerações de codificação

A parte específica do esquema se restringe aos 64 caracteres do alfabeto base64url mais o "." do sufixo opcional, todos não reservados na [RFC3986] e representáveis em US-ASCII de 7 bits. Não surgem questões de codificação percentual, internacionalização ou conjunto de caracteres. Uma URI 't256' é idêntica em sua forma de URI e em sua forma de IRI.

## 6. Alternativas rejeitadas

As seguintes alternativas de design foram consideradas:

"256t:":  Corresponde exatamente ao nome da especificação, mas viola a gramática de esquemas da [RFC3986], que exige que o primeiro caractere seja uma letra. Analisadores de URI padrão a rejeitam, e a IANA não poderia registrá-la.

"t256://<cid>":  A forma com autoridade coloca o CID na posição do host. Nomes de host não diferenciam maiúsculas de minúsculas e são convertidos para minúsculas pelo processamento genérico de URIs, o que destrói um CID base64url. A forma opaca evita toda essa classe de corrupção.

"cid:<cid>":  O esquema 'cid' já está registrado para referências MIME Content-ID [RFC2392] e não está disponível, apesar da coincidência terminológica.

"ni:" [RFC6920]:  O esquema Named Information também nomeia conteúdo por hash, mas tem uma estrutura própria e incompatível, sem lugar para o prefixo de tamanho do 256t nem para a regra de conteúdo embutido para conteúdos pequenos. Um CID 256t não pode ser expresso sem perdas como uma URI 'ni'.

"urn:256t:<cid>":  A [RFC8141] permite que um identificador de namespace de URN comece com um dígito, então essa forma é sintaticamente válida e pode ser adotada separadamente em contextos que exijam URNs. Ela foi rejeitada como forma principal por sua prolixidade e pela falta de qualquer mecanismo de resolução em software implantado.

## 7. Considerações de interoperabilidade

O esquema 't256' é opaco, então softwares que aplicam processamento hierárquico genérico (resolução de referências relativas, mesclagem de caminhos, remoção de segmentos de ponto) não têm sobre o que operar; uma URI 't256' não pode ser usada como URI base para referências relativas.

Aplicações que exibem CIDs DEVERIAM (SHOULD) apresentá-los literalmente. Truncar para exibição só é aceitável quando a URI completa continua disponível para cópia, já que um CID truncado não nomeia nada.

## 8. Considerações de segurança

Resistência a colisões:  O vínculo entre um CID e um conteúdo de mais de 64 octetos se apoia na resistência a colisões do SHA-512. A rigor, um CID não determina o conteúdo de forma única; é computacionalmente inviável, com as técnicas conhecidas atualmente, produzir dois conteúdos com o mesmo CID. Para conteúdo de 64 octetos ou menos, o vínculo é exato, porque a URI contém o próprio conteúdo.

Verificação obrigatória:  É muito mais fácil um servidor malicioso ou defeituoso retornar o conteúdo errado do que engendrar uma colisão de hash. O requisito de verificação da Seção 4 é, portanto, de nível MUST: consumidores que o ignoram perdem todas as propriedades de integridade que o esquema oferece e reduzem uma URI 't256' a um nome de arquivo não autenticado.

Confidencialidade de conteúdo pequeno:  Para conteúdo de 64 octetos ou menos, a URI *é* o conteúdo, apenas codificado em base64url. Essas URIs DEVEM (MUST) ser tratadas com a mesma sensibilidade do próprio conteúdo em qualquer lugar onde URIs sejam registradas, armazenadas em cache, exibidas ou compartilhadas. Base64url é uma codificação, não uma criptografia.

Tipo de mídia não autenticado:  O sufixo não é uma entrada do CID e, portanto, não é protegido pela verificação. Qualquer pessoa capaz de alterar uma URI em trânsito ou em repouso pode mudar ou remover seu sufixo sem invalidar a URI, mudando a forma como os consumidores interpretam o conteúdo (inalterado) — por exemplo, promovendo text/plain a text/html e, com isso, permitindo a execução de scripts. Os consumidores DEVEM (MUST) aplicar as mesmas defesas que aplicariam a um campo de cabeçalho HTTP Content-Type não confiável, como restringir a detecção de conteúdo (sniffing) e aplicar uma política de segurança adequada ao contexto antes de renderizar tipos de conteúdo ativos.

Divulgação do tamanho:  Todo CID divulga o tamanho exato do conteúdo, o que por si só pode ser revelador (por exemplo, para distinguir entre um pequeno conjunto de documentos candidatos).

A disponibilidade não é tratada:  Um CID não carrega nenhuma informação sobre onde, ou se, o conteúdo pode ser obtido. Ter uma URI 't256' válida não garante que algum servidor vá fornecer o conteúdo.

Cacheabilidade:  Conteúdo verificado PODE (MAY) ser armazenado em cache indefinidamente sob seu CID. Os caches DEVEM (MUST) usar o CID completo como chave e DEVEM (MUST) verificar o conteúdo antes de inseri-lo.

## 9. Considerações da IANA

Solicita-se à IANA que registre o seguinte esquema de URI no registro "Uniform Resource Identifier (URI) Schemes", segundo as diretrizes da [RFC7595].

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

## 10. Referências

### 10.1. Referências normativas

- [RFC2119]  Bradner, S., "Key words for use in RFCs to Indicate
  Requirement Levels", BCP 14, RFC 2119, March 1997.
- [RFC3986]  Berners-Lee, T., Fielding, R., and L. Masinter,
  "Uniform Resource Identifier (URI): Generic Syntax", STD 66,
  RFC 3986, January 2005.
- [RFC4648]  Josefsson, S., "The Base16, Base32, and Base64 Data
  Encodings", RFC 4648, October 2006.
- [RFC5234]  Crocker, D. and P. Overell, "Augmented BNF for Syntax
  Specifications: ABNF", STD 68, RFC 5234, January 2008.
- [RFC8174]  Leiba, B., "Ambiguity of Uppercase vs Lowercase in
  RFC 2119 Key Words", BCP 14, RFC 8174, May 2017.
- [FIPS180-4]  National Institute of Standards and Technology,
  "Secure Hash Standard (SHS)", FIPS PUB 180-4, August 2015.
- [256T]  "256t.org Content Addressable Storage Specification",
  https://256t.org.

### 10.2. Referências informativas

- [RFC2392]  Levinson, E., "Content-ID and Message-ID Uniform
  Resource Locators", RFC 2392, August 1998.
- [RFC6838]  Freed, N., Klensin, J., and T. Hansen, "Media Type
  Specifications and Registration Procedures", BCP 13, RFC 6838,
  January 2013.
- [RFC6920]  Farrell, S., Kutscher, D., Dannewitz, C., Ohlman, B.,
  Keranen, A., and P. Hallam-Baker, "Naming Things with Hashes",
  RFC 6920, April 2013.
- [RFC7595]  Thaler, D., Hansen, T., and T. Hardie, "Guidelines and
  Registration Procedures for URI Schemes", BCP 35, RFC 7595,
  June 2015.
- [RFC8141]  Saint-Andre, P. and J. Klensin, "Uniform Resource
  Names (URNs)", RFC 8141, April 2017.
- [RFC9110]  Fielding, R., Nottingham, M., and J. Reschke, "HTTP
  Semantics", STD 97, RFC 9110, June 2022.

## Apêndice A. Exemplos

Todos os exemplos foram produzidos com as implementações de referência em [256T].

Conteúdo vazio (0 octetos; carga vazia):

```
t256:AAAAAAAA
```

A string de 13 octetos "Hello, World!" (conteúdo embutido; a carga "SGVsbG8sIFdvcmxkIQ" decodificada em base64url é o próprio conteúdo):

```
t256:AAAAAAANSGVsbG8sIFdvcmxkIQ
```

65 octetos, cada um a letra "a" (acima do limite de 64 octetos, então a carga é o hash SHA-512 em base64url de 86 caracteres, e a URI tem seu tamanho máximo de 99 caracteres):

```
t256:AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg
```

O conteúdo "Hello, World!" novamente, com um sufixo que implica o tipo de mídia text/plain. O CID não muda: as duas URIs nomeiam os mesmos 13 octetos, e esta diz, além disso, como interpretá-los:

```
t256:AAAAAAANSGVsbG8sIFdvcmxkIQ.txt
```

Um sufixo com vários rótulos, aplicado ao CID do exemplo anterior apenas para ilustrar a sintaxe. O tipo de mídia implícito vem do último rótulo ("gz"); o rótulo anterior indica, como na prática HTTP, que o conteúdo descompactado seria um arquivo tar. O sufixo afirma uma interpretação; ele não muda, nem certifica nada sobre, o conteúdo que o CID nomeia:

```
t256:AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg.tar.gz
```

## Endereço do autor

```
Curt Cox
Email: curtcox@gmail.com
URI:   https://256t.org
```
