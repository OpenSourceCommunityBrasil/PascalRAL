# TASK_PLAYBOOKS (PascalRAL-dev)

Playbooks curtos para tarefas comuns. Cada playbook aponta exatamente **onde ler** antes de responder/codar.

---

## Playbook A — “Quero uma rota ping (Server)”

1) Leia:
- `src/base/RALRoutes.pas` (como rotas são registradas/descobertas)
- `src/base/RALServer.pas` (como o ciclo executa handlers)

2) Depois escolha engine:
- `src/engine/indy/RALIndyServer.pas` (ou fpHTTP/synopse/cgi)

3) Confirme a assinatura do handler no repo.

4) No handler, preencha resposta usando o padrão do projeto:
- procure exemplos de `AResponse.Answer(...)` em `src/base/RALResponse.pas` e handlers existentes.

---

## Playbook B — “Quero consumir a rota via Client”

1) Leia:
- `src/base/RALClient.pas` (orquestra)
- `src/base/RALRequest.pas` / `src/base/RALResponse.pas`

2) Escolha engine do transporte:
- `src/engine/indy/RALIndyClient.pas`
- `src/engine/fpHTTP/RALfpHTTPClient.pas`
- `src/engine/netHTTP/RALnetHTTPClient.pas`
- `src/engine/synopse/RALSynopseClient.pas`
- `src/engine/unigui/RALUniGUIRegister.pas` (se aplicável)

3) Confirme:
- como setar BaseURL/route
- como passa body/headers
- como ler status/response text/json

---

## Playbook C — “Enviar JSON e ler JSON”

1) Leia:
- `src/utils/RALJson.pas`
- `src/base/RALParams.pas` (conversion helpers)
- `src/base/RALResponse.pas`

2) Confirme onde fica o encoder/decoder do repo.

3) Em handlers:
- use `AResponse.Answer(HTTP_OK, <payload>, <content-type/rct...>)` (procure os tipos constantes no repo)

---

## Playbook D — “Multipart/form-data”

1) Leia:
- `src/utils/RALMultipartCoder.pas`
- `src/base/RALParams.pas` (quando faz decode do body)

2) Confirme:
- como adicionar File vs Text no `TRALParams`
- como ler de volta do `TRALParams` no server handler

---

## Playbook E — “Compressão e Criptografia (pipeline de bytes)”

1) Leia:
- `src/utils/RALStream.pas` (bytes/stream helpers)
- `src/utils/RALCompress*.pas`
- `src/utils/RALCripto*.pas`

2) Leia as dependências no pipeline:
- `src/base/RALRequest.pas` / `src/base/RALResponse.pas`
- `src/base/RALParams.pas` (EncodeBody/DecodeBody)

3) Só então ajuste engine:
- procure onde `Content-Encoding`, `Accept-Encoding`, flags de compress/cripto são checados.

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
- `src/utils/RALStorage*.pas`

---

## Playbook G — “Swagger e Postman”

1) Leia:
- `src/base/modules/RALSwaggerExporter.pas` + `src/base/modules/RALSwaggerModule.pas`
- `src/base/modules/RALPostmanExporter.pas`

2) Se precisar customizar:
- procure no repo onde `SwaggerModule` recebe URLs/paths/titles.

---

## Playbook H — “Security/Auth (Allowed/Skip + plugin)”

1) Leia:
- `src/base/plugins/RALAuthentication.pas`
- `src/base/RALRoutes.pas` (allowed/skip e validações)

2) Leia o fluxo:
- `src/base/RALServer.pas` (quando valida e quando responde)

---

## Regra geral (evitar gastar tokens)

- Comece por **um** arquivo (o mais central do playbook).
- Só depois abra os 1-2 dependentes listados.
- Não use `compiled/` como fonte.

