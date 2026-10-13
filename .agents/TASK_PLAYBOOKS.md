# TASK_PLAYBOOKS (PascalRAL-dev)

Playbooks curtos para tarefas comuns. Cada playbook aponta exatamente **onde ler** antes de responder/codar.

---

## Playbook A — “Quero uma rota ping (Server)”

1) Leia:
- `src/core/RALRoutes.pas` (como rotas são registradas/descobertas)
- `src/core/RALServer.pas` (como o ciclo executa handlers)

2) Depois escolha engine:
- `src/engine/indy/RALIndyServer.pas` (ou fpHTTP/synopse/cgi)

3) Confirme a assinatura do handler no repo.

4) No handler, preencha resposta usando o padrão do projeto:
- procure exemplos de `AResponse.Answer(...)` em `src/core/RALResponse.pas` e handlers existentes.

---

## Playbook B — “Quero consumir a rota via Client”

1) Leia:
- `src/core/RALClient.pas` (orquestra)
- `src/core/RALRequest.pas` / `src/core/RALResponse.pas`

2) Escolha engine do transporte:
- `src/engine/indy/RALIndyClient.pas`
- `src/engine/fpHTTP/RALfpHTTPClient.pas`
- `src/engine/netHTTP/RALnetHTTPClient.pas`
- `src/engine/synopse/RALSynopseClient.pas`
- `src/engine/unigui/RALUniGUIReg.pas` (se aplicável)

3) Confirme:
- como setar BaseURL/route
- como passa body/headers
- como ler status/response text/json

---

## Playbook C — “Enviar JSON e ler JSON”

1) Leia:
- `src/common/RALJson.pas`
- `src/core/RALParams.pas` (conversion helpers)
- `src/core/RALResponse.pas`

2) Confirme onde fica o encoder/decoder do repo.

3) Em handlers:
- use `AResponse.Answer(HTTP_OK, <payload>, <content-type/rct...>)` (procure os tipos constantes no repo)

---

## Playbook D — “Multipart/form-data”

1) Leia:
- `src/common/RALMultipartCoder.pas`
- `src/core/RALParams.pas` (quando faz decode do body)

2) Confirme:
- como adicionar File vs Text no `TRALParams`
- como ler de volta do `TRALParams` no server handler

---

## Playbook E — “Compressão e Criptografia (pipeline de bytes)”

0) Desde o 1.3 o corpo é único e não se copia: leia antes a seção "One body per
request, one per response" do `CLAUDE.md` e o bloco "Estado" de
`.agents/PLANO_STREAM_UNICO.md` (posse, faixas de tamanho, API de engine).

1) Leia:
- `src/common/RALStream.pas` (vistas, janelas, blocos, disco, `TRALBodyStream`)
- `src/compress/RALCompress*.pas` (`CompressTo`/`DecompressTo`, `RALIsCompressedType`)
- `src/crypto/RALCripto*.pas` (`EncryptTo`/`EncryptInPlace`/`DecryptInPlace`)

2) Leia as dependências no pipeline:
- `src/core/RALCustomObjects.pas` (`SetWireBody`, `TakeWireStream`/`TakeWireString`)
- `src/core/RALRequest.pas` / `src/core/RALResponse.pas`
- `src/core/RALParams.pas` (`DecodeBody` com posse, `PrepareBody`,
  `EffectiveCompress`, `WriteTransformed`, `TakeWireStream`)

3) Só então ajuste engine:
- a entrada vai por `SetWireBody` (com a posse certa) e a saída por
  `TakeWire*`; `ContentType`/`ContentEncoding` se leem DEPOIS do `TakeWire*`.
- procure onde `Content-Encoding`, `Accept-Encoding`, flags de compress/cripto são checados.

4) Prove: os casos `str:` da matriz e a suíte `memoria` do orquestrador.

---

## Playbook F — “DBWare/DBModule (ExecSQL/Open/ApplyUpdates)”

1) Leia:
- `src/database/RALDBModule.pas` (mapeia requests → ações)
- `src/database/RALDBBase.pas` (base comum)

2) Conectores:
- `src/database/FireDAC/*`
- `src/database/sqldb/*`
- `src/database/Zeos/*`

3) Persistência/serialização do resultado:
- `src/storage/RALStorage*.pas`

---

## Playbook G — “Swagger e Postman”

1) Leia:
- `src/modules/swagger/RALSwaggerExporter.pas` + `src/modules/swagger/RALSwaggerModule.pas`
- `src/modules/swagger/RALPostmanExporter.pas`

2) Se precisar customizar:
- procure no repo onde `SwaggerModule` recebe URLs/paths/titles.

---

## Playbook H — “Security/Auth (Allowed/Skip + plugin)”

1) Leia:
- `src/plugins/auth/RALAuthentication.pas`
- `src/core/RALRoutes.pas` (allowed/skip e validações)

2) Leia o fluxo:
- `src/core/RALServer.pas` (quando valida e quando responde)

---

## Regra geral (evitar gastar tokens)

- Comece por **um** arquivo (o mais central do playbook).
- Só depois abra os 1-2 dependentes listados.
- Não use `compiled/` como fonte.

