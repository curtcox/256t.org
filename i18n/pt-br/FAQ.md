# Perguntas frequentes

## Perguntas gerais

### O que é o 256t.org?

O 256t.org é uma especificação pública para armazenamento endereçável por conteúdo usando identificadores de conteúdo (CIDs) de 94 caracteres. Ele define uma forma simples e padronizada de identificar e obter conteúdo com base em seu hash criptográfico e em seu tamanho.

### Por que usar o 256t.org em vez de outros sistemas de armazenamento endereçável por conteúdo?

O 256t.org oferece um formato simples e previsível, com identificadores de tamanho fixo (94 caracteres ou menos) fáceis de usar em URLs, nomes de arquivo e bancos de dados. A especificação é intencionalmente mínima e focada, o que torna sua implementação simples em qualquer linguagem de programação.

### Qual a diferença entre o 256t.org e o IPFS ou outros sistemas endereçáveis por conteúdo?

Enquanto o IPFS e sistemas semelhantes são redes completas de armazenamento distribuído, o 256t.org é apenas uma especificação de identificadores de conteúdo. Ele define como calcular e verificar CIDs, mas deixa os mecanismos de armazenamento, obtenção e descoberta como detalhes de implementação. Isso o torna mais simples e flexível para diferentes casos de uso.

## Formato do CID

### O que é um CID?

Um CID (identificador de conteúdo) é uma string codificada em base64url de 94 caracteres ou menos que identifica um conteúdo de forma única. Ele consiste em:
- 8 caracteres: tamanho do conteúdo (codificado em base64url)
- Até 86 caracteres: hash SHA-512 (para conteúdo > 64 bytes) OU o próprio conteúdo (para conteúdo ≤ 64 bytes)

### Por que 94 caracteres?

O tamanho foi escolhido para caber com folga em segmentos de caminho de URLs e em tamanhos comuns de campos de bancos de dados:
- 8 caracteres para o tamanho (podem representar até 256 terabytes)
- 86 caracteres para o hash SHA-512 ou o conteúdo embutido
- Total: 94 caracteres

### O que é a codificação base64url?

Base64url é uma variante da codificação Base64 que usa caracteres seguros para URLs e nomes de arquivo (definida na [RFC 4648, seção 5](https://datatracker.ietf.org/doc/html/rfc4648#section-5)). Ela substitui `+` por `-` e `/` por `_`, e omite os caracteres de preenchimento (`=`).

### Por que conteúdos pequenos são armazenados embutidos em vez de passarem por hash?

Para conteúdo de 64 bytes ou menos, o próprio conteúdo codificado em base64url é menor ou igual ao hash SHA-512. Armazená-lo embutido é mais eficiente e evita um hash desnecessário.

### Como calculo um CID?

1. Determine o tamanho do conteúdo
2. Se o tamanho for ≤ 64 bytes:
   - Codifique o conteúdo em base64url
   - Coloque antes o tamanho codificado em base64url (8 caracteres)
3. Se o tamanho for > 64 bytes:
   - Calcule o hash SHA-512 do conteúdo
   - Codifique o hash em base64url (86 caracteres)
   - Coloque antes o tamanho codificado em base64url (8 caracteres)

Veja o diretório [implementations](https://github.com/curtcox/256t.org/tree/main/implementations) para exemplos de código em mais de 40 linguagens.

## Uso

### Posso usar o 256t.org no meu projeto?

Sim! A especificação é aberta e de uso livre. Você pode implementar a geração e a verificação de CIDs na sua aplicação ou usar uma das implementações existentes.

### Onde posso armazenar CIDs?

Você pode armazenar CIDs em qualquer lugar:
- Em um banco de dados tradicional
- Em um sistema de arquivos (como nomes de arquivo ou em um diretório dedicado)
- Em armazenamento em nuvem (S3, R2 etc.)
- Em uma CDN
- Em um sistema distribuído

A especificação não determina mecanismos de armazenamento.

### Como obtenho conteúdo a partir de um CID?

Isso depende da sua implementação de armazenamento. A especificação define o formato do CID, mas não o mecanismo de obtenção. Abordagens comuns incluem:
- Servidores HTTP com roteamento baseado em CID
- Acesso direto ao sistema de arquivos
- APIs de armazenamento em nuvem
- Protocolos personalizados

### Existem servidores públicos que hospedam CIDs?

O domínio principal 256t.org hospeda um pequeno conjunto de CIDs de demonstração, mas não pretende ser um serviço público de armazenamento de uso geral. O foco é a própria especificação.

## Segurança e integridade

### CIDs podem colidir?

Colisões de SHA-512 são criptograficamente inviáveis com a tecnologia atual. Para conteúdo > 64 bytes, os CIDs são tão resistentes a colisões quanto o próprio SHA-512. Para conteúdo ≤ 64 bytes, os CIDs são únicos por definição, pois contêm o próprio conteúdo.

### E se alguém criar uma colisão de hash?

Embora teoricamente possível, engendrar uma colisão de SHA-512 está muito além das capacidades computacionais atuais. É mais fácil para um atacante simplesmente mentir sobre o conteúdo do que criar uma colisão. Sempre verifique o conteúdo do CID se a segurança for crítica.

### Como verifico se um conteúdo corresponde a um CID?

Use qualquer uma das [implementações](https://github.com/curtcox/256t.org/tree/main/implementations) para calcular o CID do seu conteúdo e compará-lo com o CID esperado. Todas as implementações oferecem funções de verificação.

### Os CIDs são imutáveis?

Sim, na prática. Como um CID é derivado do próprio conteúdo, o mesmo CID sempre representará o mesmo conteúdo. Se o conteúdo mudar, seu CID também muda.

### CIDs podem ser armazenados em cache indefinidamente?

Sim. Como os CIDs são endereçados por conteúdo e imutáveis, eles podem ficar em cache para sempre com os cabeçalhos `Cache-Control: public, max-age=31536000, immutable`.

## Implementação

### Qual implementação devo usar?

Escolha a implementação que corresponde à linguagem principal do seu projeto. Todas as implementações seguem a mesma especificação e produzem CIDs idênticos para o mesmo conteúdo.

### Como adiciono uma nova implementação?

Cada implementação deve incluir:
- Uma biblioteca principal para o cálculo do CID (`cid.*`)
- Um script para gerar CIDs para os arquivos de exemplo (`generate.*`)
- Um script para verificar se os CIDs correspondem ao seu conteúdo (`check.*`)

Veja as implementações existentes no diretório [implementations](https://github.com/curtcox/256t.org/tree/main/implementations) como exemplo.

### As implementações são testadas?

Sim. O repositório inclui arquivos de exemplo no diretório `examples/` e os CIDs esperados em `cids.json`. As implementações são testadas gerando CIDs para esses arquivos e verificando se correspondem aos valores esperados.

## Detalhes técnicos

### Por que SHA-512 em vez de SHA-256?

O SHA-512 oferece maior resistência a colisões, e o tamanho adicional (64 bytes contra 32 bytes) não afeta significativamente o tamanho total do CID, graças ao armazenamento embutido para conteúdo pequeno.

### O que acontece com conteúdo de exatamente 64 bytes?

Ele é sempre armazenado embutido. 64 bytes codificados em base64url resultam em exatamente 86 caracteres, o mesmo tamanho de um hash SHA-512, então o conteúdo cabe no espaço que um hash ocuparia. A regra é: 64 bytes ou menos, embutido; mais de 64 bytes, hash. Assim, todo conteúdo tem exatamente um CID. Um CID de 64 bytes que contenha um hash em vez do conteúdo não é válido.

### Por que o prefixo de tamanho tem 8 caracteres?

8 caracteres de codificação base64url fornecem 48 bits de informação de tamanho, suportando conteúdo de até 256 terabytes. Isso é suficiente para praticamente todos os casos de uso, mantendo o prefixo compacto.

### Posso usar CIDs mais curtos para conteúdo pequeno?

Sim! Para conteúdo ≤ 64 bytes, o CID é mais curto porque inclui apenas o prefixo de tamanho mais o próprio conteúdo codificado em base64url (que tem ≤ 86 caracteres).

## Publicação e armazenamento

### Como publico CIDs?

Consulte [Publicação e armazenamento](publishing.html) para informações detalhadas sobre como implantar armazenamento endereçável por conteúdo usando CIDs, incluindo exemplos de envio para o Cloudflare R2.

### Quais metadados devem acompanhar o conteúdo de um CID?

A especificação foca em identificadores de conteúdo, não em metadados. No entanto, as boas práticas incluem:
- `Cache-Control: public, max-age=31536000, immutable` para o máximo de cache
- Cabeçalhos `Content-Type` adequados ao conteúdo
- Opcional: armazenar o CID em cabeçalhos de resposta ou metadados para verificação

### Posso hospedar CIDs no GitHub Pages?

Sim, mas o GitHub Pages tem limites de tamanho de arquivo e não é otimizado para armazenamento endereçável por conteúdo. Para uso em produção, considere serviços de armazenamento dedicados, como S3, R2 ou similares.

## Contribuindo

### Como posso contribuir com o 256t.org?

Contribuições são bem-vindas! Você pode:
- Adicionar implementações em novas linguagens de programação
- Melhorar as implementações existentes
- Relatar problemas ou sugerir melhorias para a especificação
- Melhorar a documentação

Veja o [repositório no GitHub](https://github.com/curtcox/256t.org) para começar.

### Onde posso fazer perguntas?

Abra uma issue no [repositório no GitHub](https://github.com/curtcox/256t.org/issues) ou inicie uma discussão na seção GitHub Discussions.

---

[← Voltar ao início](index.html)
