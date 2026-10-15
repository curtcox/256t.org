<!-- i18n:source sha256:24fcb1bca3ab318e -->
# Publicação e armazenamento

Para implantar armazenamento endereçável por conteúdo usando CIDs, este repositório inclui ferramentas para enviar arquivos ao Cloudflare R2 (armazenamento compatível com S3).

## Script de envio para o R2

O script [`.github/scripts/r2_upload.py`](https://github.com/curtcox/256t.org/blob/main/.github/scripts/r2_upload.py) envia arquivos para o R2 com verificação de CID baseada em metadados:

- Armazena o CID nos metadados do objeto (`Metadata={'cid': cid}`)
- Verifica os objetos usando `head_object()` para checar os metadados (rápido, sem download)
- Se os metadados estiverem ausentes, recorre a baixar o objeto e calcular o CID
- Não depende do ETag (que não pode ser definido e não é confiável em envios multipartes)

Consulte [`.github/scripts/R2.md`](.github/scripts/R2.md) para uso detalhado, exemplos e um trecho de Cloudflare Worker que mostra como substituir o cabeçalho ETag para os clientes.

---

[← Voltar ao início](index.html)
