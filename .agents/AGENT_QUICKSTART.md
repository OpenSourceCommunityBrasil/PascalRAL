# AGENT_QUICKSTART (PascalRAL-dev)

Guia mínimo para agentes de IA navegarem o projeto com **baixo consumo de contexto**.

> Regra: quando você terminar uma etapa, volte aqui e escolha o próximo arquivo-alvo. Não “passeie” pelo repo.

---

## 0) Identifique seu objetivo (escolha 1)

1. **Criar/entender rotas e execução no Server** → `src/core/RALRoutes.pas` + `src/core/RALServer.pas`
2. **Criar/entender Client (como faz requests e decodifica respostas)** → `src/core/RALClient.pas` + `src/engine/*`
3. **Entender Request/Response/Params (Body, Headers, Query, Answer)** → `src/core/RALParams.pas` + `src/core/RALRequest.pas` + `src/core/RALResponse.pas`
4. **Multipart (upload/download)** → `src/common/RALMultipartCoder.pas`
5. **JSON / Storage (serialização)** → `src/common/RALJson.pas` + `src/storage/RALStorage*.pas`
6. **Compress/Cripto/Stream (pipeline de bytes)** → `src/common/RALStream.pas` + `src/compress/RALCompress*.pas` + `src/crypto/RALCripto*.pas`
7. **DBWare/DBModule (integrar com banco)** → `src/database/RALDBModule.pas` + `src/database/*` + `src/storage/RALStorage*.pas`
8. **Swagger/Postman (export de rotas/documentação)** → `src/modules/swagger/RALSwaggerExporter.pas` + `src/modules/swagger/RALSwaggerModule.pas` + `src/modules/swagger/RALPostmanExporter.pas`
9. **Autenticação/Security** → `src/plugins/auth/RALAuthentication.pas` + `src/core/RALRoutes.pas` (checagens/allowed methods)
10. **XML (leitor, DOM, storage de dataset)** → `src/modules/xml/RALXMLReader.pas` + `src/modules/xml/RALXML.pas` + `src/modules/xml/RALStorageXML.pas` (plano: `.agents/PLANO_SOAP.md`)
11. **SOAP (servidor, WSDL, cliente)** → `src/modules/soap/RALSOAPModule.pas` + `src/modules/soap/RALWSDLExporter.pas` + `src/modules/soap/RALSOAPClient.pas` (tipos e faults em `RALSOAPTypes.pas`; IDE em `RALModuleSOAPReg.pas`)

---

## 1) Fluxos mentais (para não perder contexto)

### Server
- `RALServer` (loop/config)
- `RALRoutes` (resolução de rota)
- Engine de transporte (CGI/fpHTTP/Indy/etc.) traduz bytes ⇄ `TRALRequest/TRALResponse`
- Handler de rota preenche `AResponse` (ex.: `AResponse.Answer(...)`)

### Client
- `RALClient` orquestra
- Engine do transporte envia request
- `TRALRequest` é serializado (params/headers/body + compress/cripto quando configurado)
- Resposta vira `TRALResponse` e é decodificada

---

## 2) Padrão de handler (exemplo “não inventar”)

Ao criar um handler em mensagens/chat:
1. confirme a **assinatura exata** no repo em `RALServer`/`RALRoutes` (ou um exemplo de engine)
2. use `AResponse.Answer(...)` com constantes quando existirem no repo

---

## 3) Próximo arquivo recomendado (sempre)

Comece por:
- `src/core/RALRoutes.pas`

Se você estiver preso, volte e escolha o objetivo do item (0).

---

## 4) Recursos “para agente” (cache de navegação)

- Mapa do repo (com detalhes): `.agents/PROJECT_MAP.md`
- Guidelines anti-custo: `.agents/SKILLS.md`
- Playbooks por tarefa: `.agents/TASK_PLAYBOOKS.md`

