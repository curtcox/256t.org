<!-- i18n:source sha256:8fa34a8ffffd587a -->
# Suporte do t256 para desktop e navegadores

O auxiliar resolve `t256:<CID>[.suffix]` usando provedores de conteúdo escolhidos pelo usuário. Ele verifica os bytes originais antes de servi-los e mantém as configurações separadas do acesso ao conteúdo. É necessário Python 3.9 ou posterior; o auxiliar não tem dependências de terceiros em tempo de execução. Node.js e Playwright só são necessários para os testes de navegador.


## Primeiros passos

Baixe `browser-support/dist/t256-desktop.zip` no resolvedor de URIs publicado e extraia o arquivo, ou clone este repositório. Preserve a estrutura de diretórios.

A partir da raiz do repositório:

```sh
python3 browser-support/run.py serve --open
```

A página de configurações de provedores permite adicionar, editar, reordenar, desativar, remover e indexar provedores, importar/exportar índices de arquivos e ver ou limpar o conteúdo em cache. A origem pública de conteúdo do Hashbin, `https://256t.us`, vem selecionada inicialmente. **Verificar provedores** verifica uma URI colada sem abri-la e lista as falhas de provedores encontradas antes de uma correspondência, ou todas as falhas se nenhum corresponder. Os diagnósticos usam descrições e códigos de status fixos; caminhos de provedores, valores de credenciais, erros de rede brutos e a saída da GitHub CLI nunca são incluídos. O carregador de páginas expõe os mesmos diagnósticos estruturados em `error.failures`, e o resolvedor público mostra as falhas do auxiliar e os detalhes dos fallbacks bem-sucedidos. No Windows, use `python` no lugar de `python3` se esse for o seu comando do Python. Mantenha esta cópia do repositório em um local estável se for instalar o registro no desktop.

Para abrir uma URI, iniciando o auxiliar automaticamente:

```sh
python3 browser-support/run.py open t256:AAAAAAAFaGVsbG8.txt
python3 browser-support/run.py settings
python3 browser-support/run.py stop
```

As configurações ficam em `~/Library/Application Support/t256` no macOS, em `%LOCALAPPDATA%/t256` no Windows ou em `$XDG_CONFIG_HOME/t256` (normalmente `~/.config/t256`) no Linux. Substitua esse local com `--state DIRECTORY` **antes** do subcomando, ou com `T256_HOME`. Reinicie depois de editar `config.json` manualmente.

## Provedores de conteúdo

```sh
# Pasta ou unidade de rede montada; arquivos nomeados pelo CID não precisam de índice.
python3 browser-support/run.py add documents folder /path/to/documents
python3 browser-support/run.py index documents

# URL base HTTP(S): a obtenção acrescenta /<CID>, sem sufixo.
python3 browser-support/run.py add public http https://example.org/cids

# GitHub: arquivos nomeados pelo CID em um diretório, usando o login existente da GitHub CLI.
python3 browser-support/run.py add github github owner/repository --directory cids --ref main --use-gh

# Ou especifique o NOME de uma variável de ambiente com o token, não o seu valor.
python3 browser-support/run.py add private github owner/private-repository --token-env GH_TOKEN

# Um clone usa o conteúdo commitado na revisão selecionada, incluindo nomes de arquivo comuns.
# Clone/atualize usando suas credenciais Git/SSH existentes e depois aponte o auxiliar para ele.
python3 browser-support/run.py add clone git /path/to/clone --directory '' --ref HEAD
python3 browser-support/run.py index clone

python3 browser-support/run.py disable public
python3 browser-support/run.py enable public
python3 browser-support/run.py resolve t256:AAAAAAAFaGVsbG8.txt --output hello.txt
```

Repositórios públicos, privados e de organizações usam o acesso concedido às credenciais do usuário. `--api-url https://github.example/api/v3` oferece suporte ao GitHub Enterprise. Uma variável de credencial ou um login da GitHub CLI selecionados explicitamente precisam estar disponíveis; credenciais ausentes fazem aquele provedor falhar e permitem que o próximo provedor selecionado seja executado. Deixar as duas opções sem definir permite acesso público, ou o uso do `GH_TOKEN` padrão, se estiver definido. Provedores de API usam `cids/<CID>` por padrão. Para usar nomes de arquivo comuns remotamente, importe um objeto JSON que mapeie CIDs completos para caminhos relativos:

```sh
python3 browser-support/run.py import-index github cid-paths.json
```

Gere esse mapeamento a partir de um clone local ou de uma pasta, sem ler manualmente os arquivos de estado do auxiliar, e depois importe-o em um provedor GitHub com a mesma estrutura de arquivos:

```sh
python3 browser-support/run.py export-index clone --output cid-paths.json
```

Use **Importar índice**, ao lado de um provedor de pasta, clone local ou GitHub nas configurações, para selecionar o mesmo arquivo JSON. As chaves são CIDs puros; os caminhos são relativos à raiz da pasta ou do repositório (inclua os diretórios do repositório nos caminhos importados). O envio pelas configurações é limitado a 1000 KiB; use o comando acima para manifestos maiores. **Exportar índice** baixa o mapeamento atual. Índices de pasta usam barras normais em todos os sistemas operacionais, para que os manifestos exportados possam ser reutilizados em outras plataformas de desktop. Índices antigos do Windows devem ser reconstruídos antes da exportação.

**Editar** preserva o nome, o tipo, a ordem, o estado de ativação, o índice e quaisquer opções não exibidas de um provedor, enquanto atualiza seu local, revisão, opções de autenticação ou tempo limite. Reindexe depois de mudanças que afetem nomes de arquivo ou revisões; use **Cancelar edição** para descartar um rascunho. Os tempos limite de HTTP atualmente se aplicam a operações individuais de socket, e não a um prazo geral de download. Os tempos limite devem ser positivos e de no máximo 3600 segundos. Nomes de variáveis de credencial usam letras, dígitos e sublinhados, começando com uma letra ou sublinhado. Provedores com autenticação selecionada explicitamente exigem HTTPS. Configurações inválidas são rejeitadas antes de substituírem a configuração salva.

Índices nunca estabelecem confiança: os bytes retornados ainda precisam corresponder ao CID. Reindexe depois de alterar arquivos ou revisões do repositório. Clones locais não são atualizados automaticamente. Compartilhamentos montados usam as credenciais de montagem existentes do usuário. Links simbólicos que escapam das raízes selecionadas são recusados. A indexação Git exclui links simbólicos e submódulos. Redirecionamentos HTTP permanecem na origem configurada; redirecionamentos para outras origens são recusados, inclusive para provedores autenticados.

O limite padrão por conteúdo é de 64 MiB; o orçamento do cache em disco é de 256 MiB. Esses limites podem ser editados nas configurações. Os objetos originais atualmente são mantidos em memória; esta versão foi pensada para recursos limitados de desktop, e não para mídias de vários gigabytes. O cache é verificado a cada leitura e esvaziado pelo uso mais recente. Requisições simultâneas para o mesmo CID compartilham uma única obtenção e verificação no provedor, inclusive quando o objeto excede o orçamento do cache em disco. Requisições para CIDs diferentes podem ser resolvidas em paralelo. Mudanças em provedores ou índices de arquivos iniciam uma nova geração de obtenções, para que novas requisições não se juntem a trabalhos iniciados sob configurações antigas. O gateway oferece suporte a intervalos de bytes HTTP únicos para recursos originais, permitindo avançar em áudios/vídeos e downloads parciais. O objeto inteiro é obtido e verificado antes que qualquer intervalo seja enviado. HTML/CSS transformados, intervalos malformados ou multipartes e intervalos condicionais sem um validador correspondente recebem respostas completas. Desativar um provedor mantém o conteúdo em cache verificado anteriormente. Limpe o cache usando **Limpar conteúdo em cache** nas configurações se quiser remover essas cópias. O uso do cache também pode ser visto lá; reduzir o orçamento remove imediatamente as cópias mais antigas. A configuração de provedores e os índices de arquivos sobrevivem à limpeza do cache.

## Opções de integração com navegadores

| Navegador | Carregador de páginas / gateway | Extensão opcional | Navegação pelo desktop |
|---|---|---|---|
| Chrome / Edge | Implementado; verificações no motor Chromium | Pacote Chromium; mensagens nativas | Manipulador do SO |
| Brave / Opera | Implementado; validação nos navegadores de marca pendente | Pacote Chromium; validação pendente | Manipulador do SO |
| Firefox | Implementado; verificações no motor Firefox na CI | Pacote Firefox; mensagens nativas | Manipulador do SO |
| Safari no macOS | Implementado; verificações no motor WebKit | Código-fonte de Safari Web Extension; URL de conexão manual | Manipulador do macOS |

Testes de motor não comprovam os avisos de protocolo dos navegadores instalados, o comportamento da barra de endereços, a aprovação em lojas de extensões nem a assinatura do Safari. Isso exige as verificações manuais descritas abaixo. Uma extensão de navegador não registra um esquema de rede `t256:` geral. O registro por site/PWA não consegue registrar `t256:` puro nem `web+t256:`; o plano ativo de manipulador web abaixo usa um alias opcional composto só de letras.

### Plano de implementação ativo

Atualizado em 2026-10-14. Os três mecanismos são frentes de trabalho ativas. Itens marcados registram implementações existentes ou investigações concluídas; itens desmarcados são trabalho restante. Um mecanismo só está pronto para lançamento depois que suas verificações de aceitação passarem em navegadores instalados. Testes de motor e pacotes de extensão gerados, por si só, não comprovam esse resultado.

| Mecanismo | Status e trabalho existente | Pontos fortes | Limitações | Próxima entrega |
|---|---|---|---|---|
| Script de conteúdo reescreve links `t256:` para um visualizador/gateway; palavra-chave opcional na barra de endereços | **Em andamento: implementação e validação nos navegadores.** Já existem: adaptação de recursos após clique explícito, navegação para o visualizador sem auxiliar, adaptação dinâmica de links opcional, pacotes Chromium/Firefox, código-fonte de conversão para Safari, menus de contexto nativos e entrada validada de CID/URI por palavra-chave. | Preserva a URI da RFC; pode adaptar links existentes e recursos selecionados da página; pode usar os provedores privados/locais do auxiliar. | Exige permissão para acessar a página; a observação dinâmica atualmente cobre links do documento principal. Faltam lojas de extensões/assinatura e verificações no Firefox/Safari instalados. A palavra-chave é um gesto separado na barra de endereços. | Adicionar cobertura dinâmica de recursos/frames e validar cada pacote de extensão instalado. |
| Manipulador de protocolo web, incluindo a grafia solicitada `web+t256:` | **Em andamento: implementação e validação nos navegadores.** Já existem: conversão estrita do alias e controles explícitos de registro/remoção no site; verificações de motor cobrem transporte e rejeição. | Um alias válido pode abrir um visualizador no navegador sem extensão; registro pelo site, pela extensão do Firefox e por PWA Chromium instalado são opções separadas. | `web+t256:` não cumpre as regras de esquema do padrão; um alias difere da RFC. O registro não habilita sub-recursos com esquema personalizado. O Safari precisa de outra rota. | Validar consentimento/despacho em navegadores instalados; adicionar opções de auxiliar salvo, tratamento do manifesto do Firefox e suporte opcional a PWA. |
| Registro de `t256:` no SO com um auxiliar de desktop | **Em andamento: implementação e validação da distribuição.** Já existem: wrapper de app no macOS, registro por usuário no registro do Windows, entrada de desktop no Linux, inicialização, mensagens nativas e desinstalação. | Preserva o esquema exato; abre conteúdo com pastas escolhidas pelo usuário, compartilhamentos montados, bases HTTP e repositórios autenticados; oferece ao Safari uma rota de navegação externa. | Exige instalação e aprovação do navegador quando solicitado. O despacho de URIs digitadas/coladas na barra de endereços varia. O runtime atual precisa do Python em um caminho estável. Não transforma `t256:` em um protocolo de recursos do navegador. | Validar o despacho real nos navegadores e a instalação/remoção limpa nos três SOs; criar pacotes práticos para cada plataforma. |

As [regras de manipuladores personalizados do HTML](https://html.spec.whatwg.org/multipage/system-state.html#custom-handlers) permitem `web+` seguido de letras ASCII minúsculas, então nem `t256` puro nem `web+t256` podem ser registrados pela API padrão de sites. Mantenha `t256:` como o esquema canônico da RFC. Use `web+tcontent:` como o alias de compatibilidade em uso; documente a convenção antes de publicar links com o alias. O suporte exato a `web+t256:` exige uma exceção específica de navegador ou uma mudança upstream e é um item de investigação, não uma capacidade prometida.

O Firefox e o Chromium para desktop oferecem suporte a [`navigator.registerProtocolHandler()`](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/registerProtocolHandler) para esquemas elegíveis; não descreva a API de sites do Chromium como, em geral, dependente de flags. O Chrome também documenta [manipuladores de protocolo para PWAs instalados](https://developer.chrome.com/docs/web-platform/best-practices/url-protocol-handler) como disponíveis a partir do Chrome 96. Teste o registro pelo site, o registro pelo manifesto da extensão e a instalação de PWA de forma independente, com as configurações padrão do navegador.

O Safari também pode adaptar links de páginas por meio de uma [Safari Web Extension](https://developer.apple.com/documentation/safariservices/safari-web-extensions) ou usar o carregador/gateway. O auxiliar do SO é a principal rota externa planejada para o esquema exato no Safari; não é a única integração prática com o Safari.

#### 1. Links pela extensão, visualizador e palavra-chave

- [x] Criar as extensões Chromium e Firefox e o código-fonte de conversão para Safari a partir de código compartilhado (`extensions/shared`, `build_extensions.py`).
- [x] Reescrever os links de navegação existentes da página para o gateway verificado do auxiliar após uma ação explícita do usuário; adaptar também imagens, folhas de estilo e mídia.
- [x] Oferecer menus de contexto do host nativo e tratamento de eventos de palavra-chave nos navegadores com essas APIs; exercitar a adaptação de páginas no smoke test da extensão Chromium.
- [x] Fazer `t256 <CID>[.suffix]` funcionar no modo de palavra-chave. Aceitar CIDs puros e URIs completas, preservar a caixa do CID, validar com o analisador compartilhado da RFC antes do lançamento nativo e mostrar orientações de entrada. Testes unitários exercitam os scripts de segundo plano gerados para Chromium e Firefox; restam as verificações no Firefox instalado, junto com a [API de palavra-chave](https://developer.chrome.com/docs/extensions/reference/api/omnibox) do Chrome.
- [x] Adicionar um destino de visualizador configurável e as ações **Abrir no visualizador do navegador** / **Adaptar links sem o auxiliar**. Usar por padrão o resolvedor HTTPS público para conteúdo público/embutido; conteúdo privado/local mantém a rota do auxiliar. Validar estritamente URIs e locais do visualizador, preservar as relações de link existentes e deixar links/recursos inválidos inalterados. Testes no Chromium cobrem a abertura do popup, a navegação por links, a ausência de requisições a provedores durante a adaptação e os bytes verificados.
- [x] Adicionar **Continuar adaptando links nesta página** e **Parar de adaptar novos links**. Observar âncoras HTML novas/alteradas no documento principal, varrer em lotes limitados, agrupar grandes rajadas de mutações, ignorar elementos removidos e preservar edições comuns da página. A reinjeção substitui o observador anterior; parar ou sair da página limpa o trabalho enfileirado. Testes no Chromium cobrem 700 links adicionados, hrefs alterados, controles do popup, reinício/parada/desmontagem e zero requisições a provedores.
- [ ] **Próximo: estender a adaptação contínua opcional aos recursos.** Preservar edições da página durante a obtenção, liberar recursos na desmontagem e evitar carregamentos repetidos de mídia. Adicionar persistência opcional por site somente com permissões explícitas de site e um controle para desativar; a observação atual de links se limita a este documento.
- [ ] Definir a cobertura de frames e shadow roots abertas; informar frames inacessíveis, páginas protegidas do navegador, CSP restritiva e falhas de permissão de rede local. A adaptação só de links já funciona sem conceder acesso aos provedores locais.
- [ ] Validar a instalação/assinatura no Firefox, a compatibilidade com Edge/Brave/Opera e uma extensão Safari compilada. Para o Safari, manter a opção de conexão manual e planejar a troca de mensagens com o app contêiner; não presumir que o registro de host nativo em Python ou uma API de omnibox funcione lá.
- [ ] Conectar as melhorias de recursos de página ao carregador e ao gateway existentes: scripts precisam de carregamento ordenado, mapas de dependências de módulos e tratamento de CSP, além da reescrita de links. Acompanhar workers de módulo, imports calculados e dependências de SVG como trabalhos de capacidade separados, em vez de alegar suporte transparente a scripts.

Aceitação: âncoras `t256:` não modificadas navegam após a ativação; o modo de palavra-chave aceita CIDs puros e URIs completas onde estiver disponível; a adaptação dinâmica para quando desativada; links copiados/compartilhados não contêm nenhuma capacidade do auxiliar; conteúdo indisponível tem um estado de falha útil. Validar CIDs embutidos e com hash, CIDs com caixa mista, sufixos compostos, entradas inválidas, auxiliar ausente, autorizações revogadas e janelas privadas. Os testes de recursos devem verificar os bytes antes do uso e a ordem de execução dos scripts.

#### 2. Manipulador web e alias de compatibilidade

- [x] Estabelecer a restrição do padrão sobre `web+t256:` e identificar a rota `web+tcontent:`, composta só de letras. Nenhuma mudança na RFC canônica é necessária.
- [x] Implementar o adaptador de alias compartilhado e o ponto de entrada do manipulador em `web/protocol-handler.mjs` e no resolvedor público. Converter apenas o esquema do alias reconhecido, preservar a caixa do CID, usar o analisador estrito da RFC e decodificar o transporte uma única vez. Testes unitários cobrem IDs embutidos/com hash, sufixos compostos, codificações malformadas, aliases desconhecidos e parâmetros de URI duplicados.
- [x] Adicionar botões explícitos de registro no site e de remoção suportada. Usar um modelo de fragmento `%s` na mesma origem, sem parâmetros de consulta existentes nem capacidades do auxiliar; oferecer suporte a HTTPS e a desenvolvimento seguro em loopback. A detecção de recursos, o retorno sobre rejeições e a resolução manual continuam disponíveis. Testes de motor verificam o encaminhamento do modelo e a invocação explícita com APIs de navegador simuladas.
- [x] Reutilizar a resolução verificada pública/embutida e a do auxiliar colado. Explicar que esta página pode ler o identificador, inclusive bytes embutidos, e aguardar a escolha de provedores pelo usuário antes de buscar. Mudanças no hash limpam prévias antigas; downloads e links compartilhados mantêm identificadores `t256:` canônicos.
- [ ] **Próximo: validar o consentimento e o despacho reais** no Firefox/Chrome/Edge instalados, incluindo avisos recusados, janelas privadas e remoção pelas configurações do navegador.
- [x] Adicionar os controles explícitos **Lembrar a conexão neste navegador** e **Esquecer a conexão lembrada**. Restaurar uma conexão de loopback validada nas visitas seguintes sem obter nada automaticamente nem incluí-la em modelos de registro/links compartilhados. Verificações no navegador cobrem salvar/recarregar, remoção e armazenamento bloqueado; URLs inválidas com credenciais são rejeitadas.
- [ ] Testar a rota opcional do [manifesto de extensão `protocol_handlers`](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/manifest.json/protocol_handlers) do Firefox para um visualizador da extensão. Manter campos de manifesto específicos de navegador separados dos pacotes Chromium/Safari e evitar registros concorrentes por padrão.
- [ ] Adicionar um PWA Chromium instalável opcional com `protocol_handlers`, URLs de manipulador com escopo e instruções claras de instalação/remoção. Testar a integração com o SO separadamente do registro pelo site; manter o resolvedor básico utilizável sem instalar o PWA. Sondar Edge, Brave e Opera em vez de herdar a declaração de suporte do Chrome.
- [x] Adicionar verificações negativas de motor para o registro exato de `web+t256:` (rejeição pelo padrão ou API indisponível). As verificações no Chromium e no WebKit passam localmente; as do Firefox rodam na CI. O consentimento de registros válidos não é contornado.
- [ ] Investigar qualquer exceção documentada específica de navegador ou flag necessária em perfis isolados. Explorar uma mudança upstream nas regras de esquema ou em uma lista de permissões apenas como rota de longo prazo; nunca ativar flags de navegador silenciosamente.
- [x] Publicar exemplos canônicos/de alias/HTTPS e a convenção de conversão abaixo. Oferecer controles de remoção onde disponíveis e orientações de configuração nos demais casos; manter alternativas por extensão/SO/manuais para navegadores sem suporte.

Aceitação: um alias válido clicado chega como o identificador canônico exato; aliases inválidos nunca obtêm conteúdo; o registro exige ação explícita; recusar ou remover o registro mantém o fallback utilizável. Testar o fluxo do site no Firefox/Chrome/Edge instalados e os fluxos de PWA/extensão apenas onde houver suporte. Registrar os resultados da barra de endereços separadamente da ativação de links. Não contar o sucesso do manipulador de navegação como suporte de protocolo a `fetch`, imagens ou scripts.

#### 3. Registro no SO, auxiliar nativo e distribuição

- [x] Implementar o registro de esquema por usuário para macOS, Windows e Linux em `t256_helper/install.py`, além da inicialização automática do auxiliar e da análise estrita de URIs.
- [x] Abrir uma página de gateway verificada; oferecer registro de host nativo para a família Chrome/Firefox restrito ao ID de extensão especificado.
- [x] Implementar remoção ciente de propriedade, restauração do manipulador anterior no Linux, configuração de provedores, verificação e acesso a conteúdo privado/local.
- [x] Executar os testes unitários do auxiliar e do registro nos três SOs na CI. Eles não exercitam todos os serviços de registro de desktop/navegador instalados.
- [ ] **Próximo: realizar verificações de instalação e despacho em perfis novos.** Exercitar links clicados, URIs digitadas e coladas, aceitação/recusa de avisos, navegador fechado, auxiliar parado, inicializações repetidas e janelas privadas. Registrar a versão do navegador, a versão do SO e as configurações padrão; classificar entradas na barra de endereços que viram busca como limitação e oferecer alternativas de palavra-chave/colar no visualizador.
- [ ] Empacotar um runtime embutido ou um auxiliar independente para cada plataforma, com interface de instalação, atualização e remoção. Testar caminhos com espaços/caracteres não ASCII, instalações movidas, propriedade de manipuladores existentes e desinstalação após a mudança do manipulador padrão. Manter utilizável o pacote atual com o código-fonte em Python.
- [ ] No macOS, validar a entrega pelo LaunchServices e criar um app distribuível assinado/notarizado; incluir o caminho de conexão da extensão Safari/app contêiner. Tratar o wrapper ad hoc gerado atualmente como uma instalação de desenvolvimento.
- [ ] No Windows, validar a entrega pelo registro por usuário, o encaminhamento seguro de argumentos, a descoberta de navegadores/hosts nativos e um instalador assinado. Evitar exigir privilégios de administrador e evitar substituir outro manipulador sem uma escolha deliberada.
- [ ] No Linux, validar o comportamento da entrada de desktop e do `xdg-mime` no GNOME/KDE, em portais de desktop e em navegadores empacotados como Flatpak/Snap. Documentar os limites de dependências e de acesso do sandbox para compartilhamentos montados, lançadores e hosts nativos.
- [ ] Melhorar o retorno de inicialização e a seleção de destino: distinguir URI inválida, provedor/autenticação ausente, inicialização do navegador bloqueada e conexão do auxiliar expirada. Oferecer abertura na extensão solicitante quando disponível, com o navegador/gateway padrão como fallback do manipulador do SO.

Aceitação: a instalação encaminha a URI inalterada para um único auxiliar em execução e um visualizador verificado, sem laço de inicialização; recusar um aviso do navegador não abre o conteúdo; a desinstalação mantém as configurações do usuário e respeita mudanças posteriores de manipulador. Verificar conteúdo embutido/com hash e os provedores de sistema de arquivos, compartilhamento montado, HTTP, Hashbin e GitHub autorizado selecionados. Manter os testes de navegação pelo SO separados dos testes de recursos de página: o carregamento de recursos usa o carregador ou o gateway transformado.

#### Cobertura de navegadores e ordem de entrega

| Alvo | Plano da extensão | Plano do manipulador web | Critério de lançamento do manipulador do SO |
|---|---|---|---|
| Chrome / Edge no macOS, Windows, Linux | Pacote compartilhado, host nativo, palavra-chave; testar em cada navegador instalado | Alias HTTPS no site; PWA opcional, testado separadamente | Despacho real por clique/digitação/colagem e instalação/remoção em cada SO |
| Firefox no macOS, Windows, Linux | Pacote assinado, host nativo, palavra-chave; testes reais da extensão | Alias HTTPS no site; manipulador opcional pelo manifesto da extensão | Despacho real, avisos e descoberta do host nativo em cada SO |
| Brave / Opera no macOS, Windows, Linux | Código-fonte Chromium compartilhado; testes de permissão/loja nos navegadores de marca | Sondar suporte de site/PWA com as configurações padrão | Despacho real e descoberta do host nativo específica do navegador |
| Safari no macOS | App contêiner compilado, adaptação de links e opções de conexão manual/nativa; sem rota de palavra-chave no momento | Detecção de recursos e fallback HTTPS; sem promessa de suporte à API de sites | LaunchServices, avisos do Safari, assinatura/notarização e testes reais no Safari |

Ordem de entrega: a entrada por palavra-chave, o registro explícito do alias e o fallback do visualizador da extensão estão implementados; em seguida, validar combinações de navegador/SO instalados e empacotar o auxiliar/app do Safari. A adaptação dinâmica, o tratamento do manifesto do Firefox e a instalação de PWA podem vir como incrementos separados. Atualize esta lista a cada incremento e vincule a evidência do teste ou da verificação manual. Toda combinação suportada precisa de uma aprovação registrada ou de uma limitação documentada antes de ser anunciada como pronta para lançamento.

### Páginas cooperantes

Autorize a **origem exata** da página nas configurações do auxiliar e depois copie a URL de conexão dela. Por exemplo, um site de desenvolvimento local pode usar `http://127.0.0.1:8080`. As conexões mudam a cada reinício do auxiliar; revogue uma origem nas configurações para rejeitar requisições seguintes. Autorizar a mesma origem novamente cria uma nova URL de conexão; URLs revogadas anteriormente continuam inválidas. Edições não relacionadas em provedores ou armazenamento preservam as conexões das origens que continuaram autorizadas. Uma conexão é uma capacidade do portador: qualquer pessoa que a tenha pode solicitar CIDs conhecidos aos seus provedores selecionados. Não incorpore URLs de conexão privadas em código-fonte distribuído publicamente nem em links compartilhados.

```html
<img data-t256-src="t256:YOUR_IMAGE_CID.png" alt="Content image">
<link rel="stylesheet" data-t256-href="t256:YOUR_STYLESHEET_CID.css">
<script type="application/t256" data-t256-src="t256:YOUR_FIRST_SCRIPT_CID.js"></script>
<script type="application/t256" data-t256-src="t256:YOUR_SECOND_SCRIPT_CID.js"></script>
<script type="module">
  import { T256Client } from './browser-support/web/loader.mjs';
  const client = new T256Client(connectionUrlSuppliedByTheUser);
  await client.hydrate(document);
  const observer = client.observe(document, { onError: console.error });
  // Na desmontagem: observer.disconnect(); client.dispose();
</script>
```

Declarações inertes evitam condições de corrida com o analisador do navegador. Scripts clássicos rodam em sequência, na ordem do documento; eles são verificados novamente no navegador e carregados como blobs, sem `eval`. Hidratações repetidas ou sobrepostas não executam novamente declarações inalteradas. As folhas de estilo terminam de carregar antes que os scripts declarados sejam executados. Declarações dinâmicas opcionais podem ser tratadas com `observe()`. Os eventos de carregamento são limitados pelo tempo limite do cliente, e `dispose()` cancela obtenções/carregamentos pendentes, desconecta observadores e libera as URLs de blob que possui. A execução de um script já iniciada não pode ser desfeita; workers devolvidos à página devem ser encerrados por quem os chamou.

Para módulos, forneça todo o grafo de dependências antes de carregar qualquer módulo:

Use `<script src="./browser-support/web/loader.js"></script>` para esse caminho e depois acesse `window.T256Client`. O Firefox exige o mapa de importação antes do primeiro carregamento de módulo, incluindo o próprio carregador. A entrada como módulo ES continua adequada para páginas com mapa de importação estático, scripts clássicos, imagens ou módulos autocontidos.

```js
await client.imports({
  't256:DEPENDENCY_CID.mjs': 't256:DEPENDENCY_CID.mjs',
  'named-dependency': 't256:ANOTHER_DEPENDENCY_CID.mjs',
});
await client.script('t256:ENTRY_CID.mjs', { module: true });
const bytes = await client.bytes('t256:DATA_CID.json', { signal });
const worker = await client.worker('t256:WORKER_CID.js');
```

Os módulos são servidos como código-fonte original pelo gateway verificado. Os mapas de importação são explícitos; o código-fonte JavaScript e os imports calculados não são reescritos. Workers clássicos e autocontidos são suportados; grafos de dependências de workers de módulo não são. A sintaxe opaca do t256 não tem semântica de referência relativa, então as dependências precisam usar identificadores completos ou nomes explícitos no mapa de importação.

A CSP normal da página continua valendo: permita a origem do carregador, `blob:` para scripts clássicos/imagens/workers e a origem do auxiliar para requisições de módulos e folhas de estilo. Um nonce de CSP pode ser fornecido aos métodos do carregador. A permissão de rede local do navegador e as verificações de CORS são preservadas. O `/demo.html` do auxiliar é um exemplo executável.

### Gateway para páginas existentes

Abra uma URI `.html` com o auxiliar ou com a página de configurações. O gateway verifica o HTML original e depois transforma atributos que contêm URLs, CSS embutido e valores de mapas de importação antes que o navegador o analise. Candidatos de imagens responsivas em `srcset` e `imagesrcset` são transformados como URLs completas, preservando data URLs e descritores. Referências externas de CSS `url()` e `@import` entre aspas também são transformadas, incluindo escapes de caracteres CSS, palavras-chave escapadas e continuações de linha entre aspas. Comentários e strings CSS comuns permanecem inalterados. Isso segue a [sintaxe de escape do CSS](https://www.w3.org/TR/css-syntax-3/#escaping) padrão. Frames HTML aninhados e links de navegação t256 usam o gateway. O JavaScript original não é alterado.

HTML e CSS transformados precisam estar em UTF-8 e são servidos explicitamente como UTF-8.

Uma página existente pode usar mapas de importação padrão com destinos t256:

```html
<script type="importmap">
{"imports":{"named-dependency":"t256:DEPENDENCY_CID.mjs"}}
</script>
<script type="module" src="t256:ENTRY_CID.mjs"></script>
```

Documentos de conteúdo HTML e SVG têm uma origem opaca e isolada em sandbox, inclusive na navegação direta para representações originais/brutas. Eles podem executar os scripts que selecionarem, mas não podem ler as configurações de gerenciamento do auxiliar nem se autenticar nos provedores. A CSP deles limita os destinos de recursos e conexões ao gateway. Os bytes de origem verificados permanecem no cache de CIDs; HTML/CSS gerados são marcados com `X-T256-Representation: transformed` e `X-T256-Source-CID` e não alegam ter o hash daquele CID de origem.

### Extensões

```sh
python3 browser-support/build_extensions.py
```

Os pacotes são criados em `browser-support/dist/extensions`:

- Chrome, Edge, Brave, Opera: carregue o diretório `chromium` como extensão descompactada.
- Firefox: carregue `firefox/manifest.json` temporariamente para desenvolvimento. Instalações permanentes exigem assinatura da Mozilla ou uma implantação gerenciada adequada.
- Safari: converta o diretório `safari` com `xcrun safari-web-extension-converter` e depois compile/assine o app contêiner no Xcode. Este pacote usa URLs de conexão de site copiadas, e não mensagens nativas em Python. O carregador de páginas/gateway também funciona sem uma extensão do Safari.

A extensão usa `activeTab`; ela não lê todos os sites automaticamente. **Abrir no visualizador do navegador** envia uma URI validada ao visualizador selecionado (por padrão, o resolvedor HTTPS público) sem invocar um host nativo. **Adaptar links sem o auxiliar** reescreve apenas as âncoras `t256:` atuais para esse visualizador. Ela não obtém conteúdo, não reescreve recursos da página e não exige conexão com o auxiliar nem autorização de provedor. URIs inválidas permanecem inalteradas e informam um erro; as relações de link existentes são mantidas. Só são aceitas URLs de visualizador HTTPS ou HTTP em loopback, sem credenciais, consultas ou fragmentos. A URI é levada no fragmento do visualizador, e o visualizador aguarda uma ação de resolução. O visualizador selecionado pode ler o identificador e os bytes embutidos após a navegação. Essas ações estão incluídas no código-fonte de conversão para o Safari; a instalação assinada no Safari continua sendo uma verificação de lançamento.

**Continuar adaptando links nesta página** também trata novas âncoras HTML e valores `href` alterados depois que o popup é fechado. Ela varre em lotes e não busca nada. Reabra o popup para ver o estado ou selecione **Parar de adaptar novos links**. Parar mantém os links já adaptados, cancela o trabalho enfileirado e deixa em paz as edições posteriores da página. Sair da página interrompe a observação, inclusive na navegação pelo cache de voltar/avançar do navegador; ao voltar, é preciso ativá-la novamente. Iniciá-la de novo substitui o observador anterior. Um visualizador alterado vale para os links `t256:` processados em seguida; links de visualizador já reescritos mantêm o destino existente. Esta opção cobre as âncoras HTML do documento principal. Frames, shadow roots e imagens/scripts/mídia dinâmicos continuam como itens separados do plano.

“Adaptar imagens, mídia e links nesta página” trata as imagens atuais (incluindo `srcset` e fontes de picture), fontes de áudio/vídeo, pôsteres, faixas de legenda, folhas de estilo e referências de navegação após um clique explícito. Mudanças de fonte recarregam o elemento de mídia afetado uma vez, para que os recursos possam ser selecionados novamente. URIs idênticas compartilham uma única requisição de metadados por adaptação. Atributos alterados pela página durante a obtenção são deixados como estão, e conteúdo indisponível informa os diagnósticos de provedores do auxiliar. Ela deixa deliberadamente as tags script existentes em paz; use o carregador de páginas ou o gateway para ordem de execução e suporte a módulos.

A integração de inicialização nativa do Chrome/Firefox usa este registro por usuário:

```sh
python3 browser-support/run.py install --browser chrome --extension-id YOUR_32_CHARACTER_EXTENSION_ID
python3 browser-support/run.py install --browser firefox --extension-id t256@256t.org
```

Use `edge`, `brave` ou `opera` para outros locais de host nativo. Os manifestos nativos instalados permitem apenas a extensão especificada. Os menus de contexto e a palavra-chave `t256` na barra de endereços abrem o conteúdo pelo gateway. A palavra-chave exige o gesto de ativação de palavra-chave do navegador; ela não faz com que URIs puras na barra de endereços sejam reconhecidas universalmente. Digite um CID (por exemplo, `t256 AAAAAAAFaGVsbG8.txt`) ou uma URI completa depois de ativar a palavra-chave. Identificadores inválidos mostram orientações de entrada e não iniciam o host nativo.

### Registro no SO

```sh
python3 browser-support/run.py install
python3 browser-support/run.py uninstall
```

O registro usa um `.app` AppleScript no macOS, chaves de registro de protocolo de URL por usuário no Windows e uma entrada de desktop com `xdg-mime` no Linux. Ele não exige acesso de administrador. O Python e esta cópia do repositório precisam permanecer nos caminhos em que foram instalados. A desinstalação mantém as configurações de provedores e os bytes em cache.

O wrapper do macOS é assinado localmente de forma ad hoc. Ele é gerado na instalação e não exige uma identidade de assinatura de desenvolvedor. A distribuição da extensão do Safari é um processo separado, com app contêiner assinado.

## Fallback HTTPS público

O [resolvedor de URIs](../resolve.html) do site extrai conteúdo embutido e verifica bytes de provedores públicos com CORS habilitado, sem instalação. Seu campo opcional de conexão usa o resolvedor de desktop para os provedores privados/locais selecionados. O resolvedor também aceita o alias de compatibilidade `web+tcontent:<CID>[.suffix]`. Ele substitui esse esquema por `t256:` antes de analisar ou obter o conteúdo; a caixa do CID é preservada e os sufixos seguem a normalização do analisador canônico. Por exemplo, `web+tcontent:AAAAAAAFaGVsbG8.txt` corresponde a `t256:AAAAAAAFaGVsbG8.txt`. Nenhum outro alias é aceito, e o analisador da RFC/o auxiliar continuam exigindo `t256:`.

Selecione **Ativar links do navegador** no resolvedor HTTPS (ou em uma página local segura do auxiliar) para solicitar o registro do site e depois aprove no navegador. O modelo usa `resolve.html#uri=%s` na mesma origem, sem consulta, conexão ou credenciais existentes. Uma chamada de API bem-sucedida informa que uma solicitação foi enviada, em vez de alegar aprovação; o navegador controla a confirmação e os padrões. Os links recebidos preenchem o campo de URI canônica e aguardam **Resolver e verificar** antes de acessar os provedores. Provedores existentes e uma conexão colada do auxiliar podem ser selecionados para aquela visita. **Lembrar a conexão neste navegador** salva explicitamente a conexão validada no armazenamento do navegador deste site e a restaura nas visitas seguintes, inclusive na navegação por alias. As conexões não são salvas automaticamente, e a restauração não busca conteúdo. Substitua uma URL lembrada depois de reiniciar o auxiliar; autorizações expiradas/revogadas continuam falhando na obtenção. **Esquecer a conexão lembrada** remove a URL salva e limpa o campo; revogar o acesso aos provedores exige remover a autorização do site nas configurações do auxiliar. Se o armazenamento do navegador estiver bloqueado, a entrada manual da conexão e a resolução continuam funcionando.

Use **Remover o manipulador de links do navegador** onde o navegador oferecer remoção, ou mude o manipulador nas configurações do navegador. Um registro sem suporte/bloqueado mantém a resolução manual utilizável. O registro por PWA e pelo manifesto de extensão do Firefox são itens pendentes separados do plano. Os testes de motor simulam os resultados de interface de registros válidos e exercitam a rejeição real de esquemas inválidos sem registrar um manipulador no navegador do usuário; o consentimento em navegadores instalados e o despacho externo continuam sendo verificações de lançamento.

Prévias de HTML, JavaScript e SVG são texto. Os downloads usam um tipo de mídia binário, preservam os bytes originais verificados e não podem criar um documento HTML/SVG sob a origem da página do resolvedor. Prévias de bitmap usam URLs temporárias separadas. Links compartilhados contêm apenas a URI no fragmento, nunca uma capacidade do auxiliar ou uma credencial de provedor. Use **Cancelar obtenção** para interromper uma requisição pendente a um provedor ou ao auxiliar e tentar novamente. O cancelamento limpa prévias/links de download desatualizados, e sair da página libera seus objetos de prévia. Provedores públicos que excedem o tempo limite durante o streaming do conteúdo passam para o próximo provedor selecionado; o cancelamento por quem chamou interrompe a busca por completo. O leitor cancela streams parados e libera suas travas. Uma requisição do navegador ao auxiliar que foi cancelada pode deixar em execução uma obtenção de provedor já iniciada, para outras requisições que a compartilham.

## Integração com o Hashbin

A documentação local do Hashbin especifica `https://256t.us/<CID>[.<ext>]` para bytes públicos e sem cookies; `https://hashbin.org/api/content/<CID>` retorna metadados. O auxiliar usa `https://256t.us/<CID>` e aplica o sufixo localmente, incluindo sufixos compostos. Falhas HTTP, incluindo 404/451/429, participam do fallback entre provedores. As APIs de publicação/autenticação estão fora do escopo deste resolvedor somente leitura.

## Validação

Estes comandos se aplicam à cópia do código-fonte; o pequeno pacote de runtime para download omite dependências de desenvolvimento e fixtures de teste.

```sh
PYTHONPATH=browser-support python3 -m unittest discover -s browser-support/tests -v
npm --prefix browser-support test
npm --prefix browser-support ci
browser-support/node_modules/.bin/playwright install chromium firefox webkit
npm --prefix browser-support run test:browsers
```

O workflow dedicado testa o enquadramento de resolvedor/gateway/mensagens nativas no macOS, Windows e Linux e os recursos de navegador no Chromium, Firefox e WebKit. Os testes usam provedores locais, e não repositórios privados reais nem credenciais.

Verificações manuais de lançamento: URIs clicadas e coladas; avisos de inicialização do navegador; janelas privadas; autorizações/revogações de provedores; compartilhamentos desconectados; login/SSO do GitHub e renovação de credenciais; extensão Safari assinada; Brave/Opera de marca; instalação e remoção em cada SO. `fetch('t256:…')` nativo e transparente, imports dinâmicos arbitrários, recursos internos de SVG, intervalos de mídia multipartes e a implementação nos motores dos navegadores upstream continuam sendo trabalho futuro.
