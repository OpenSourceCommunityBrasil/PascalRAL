# PROJECT_MAP.md (PascalRAL-dev)

Este arquivo foi criado para ajudar agentes de IA a entenderem rapidamente a estrutura e os componentes do repositório **PascalRAL-dev**.

> Observação: a listagem abaixo foi baseada na estrutura atual do diretório `src/` e nos arquivos top-level visíveis no workspace.

---

## 0) TL;DR (leia primeiro)

- Se você está em dúvida: comece por `.agents/AGENT_QUICKSTART.md`.


- Se você precisa “onde começar”, use `.agents/AGENT_QUICKSTART.md`.
- Para entender o fluxo geral, use a visão conceitual (seções 4.1/4.2/4.3).
- Para navegação por tarefa: use `.agents/TASK_PLAYBOOKS.md`.

---

## 1) Estrutura raiz


- `.gitignore`
- `.gitmodules`
- `CHANGELOG.md` — histórico de mudanças, releases e notas de correções.
- `LICENSE`
- `README.md` — visão geral, links e referências.
- `READMEES.md` — versão em espanhol.
- `READMEPT.md` — versão em português.
- `SECURITY.md` — política de segurança.
- `pasdoc.pds` — configuração do pasdoc.

- `Assets/` — imagens/ícones (PNG/SVG) e recursos visuais.
- `Assets/components/` — assets de componentes por categoria.
- `Assets/src/` — recursos/arquivos .rc / .fig / assets auxiliares.
- `Assets/svg_icons/` — conjunto de SVGs usados em docs/componentes.

- `src/` — **código-fonte principal**.

> `compiled/` não é fonte de lógica do projeto; é um diretório temporário/derivado.

---

## 2) Código-fonte (`src/`) — visão geral

A pasta `src/` está organizada em:

- `src/base/` — tipos base, core do servidor/cliente, rotas, requisições/respostas e registro.
- `src/database/` — camada de DBWare/DBModule e conectores (FireDAC/SQLDB/Zeos).
- `src/engine/` — engines de transporte (CGI, fpHTTP, Indy, netHTTP, Sagui, Synopse, UniGUI).
- `src/languages/` — arquivos de constantes/strings por idioma.
- `src/others/` — integrações e implementações externas (ex.: kxBSON, brotli, ZSTD).
- `src/utils/` — utilitários (JSON, compressões, hash, stream, storage, criptografia, multipart etc.).
- `src/wizard/` — tooling de wizard (Delphi e Lazarus).

---

## 3) Mapa detalhado por módulo (unidades principais)

### 3.1) `src/base/`

**Arquivos principais:**
- `PascalRAL.inc`
- `RALClient.pas`
- `RALConsts.pas`
- `RALDBTypes.pas`
- `RALParams.pas`
- `RALRegister.pas`
- `RALRequest.pas`
- `RALResponse.pas`
- `RALRoutes.pas`
- `RALServer.pas`
- `RALTypes.pas`

**Subpastas:**
- `src/base/modules/`
  - `RALExternalsLibraries.pas`
  - `RALPostmanExporter.pas`
  - `RALSwaggerExporter.pas`
  - `RALSwaggerModule.pas`
  - `RALWebModule.pas`
- `src/base/plugins/`
  - `RALAuthentication.pas`

---

### 3.2) `src/database/` (DBWare / DBModule)

**Arquivos principais:**
- `RALDBBase.pas`
- `RALDBConnection.pas`
- `RALDBConnectionPooler.pas`
- `RALDBModule.pas`
- `raldbregister.pas`
- `RALDBSQLCache.pas`

**Conectores:**
- `src/database/FireDAC/`
  - `RALDBFireDAC.pas`
  - `RALDBFiredacDAO.pas`
  - `RALDBFireDACLinkReg.pas`
  - `RALDBFiredacMemTable.pas`
- `src/database/sqldb/`
  - `RALDBBufDataset.pas`
  - `RALDBSQLDB.pas`
  - `RALDBSQLDBLinkReg.pas`
- `src/database/Zeos/`
  - `RALDBZeos.pas`
  - `RALDBZeosLinkReg.pas`
  - `RALDBZeosMemTable.pas`

---

### 3.3) `src/engine/` (transport engines)

- `engine/cgi/`: `RALCGIRegister.pas`, `RALCGIServer*.inc`, `RALCGIServer.pas`
- `engine/fpHTTP/`: `RALfpHTTPClient.pas`, `RALfpHTTPRegister.pas`, `RALfpHTTPServer.pas`
- `engine/indy/`: `RALIndyClient.pas`, `RALIndyRegister.pas`, `RALIndyServer.pas`
- `engine/netHTTP/`: `RALnetHTTPClient.pas`, `RALNetHTTPRegister.pas`
- `engine/sagui/`: `RALSaguiRegister.pas`, `RALSaguiServer.pas`
- `engine/synopse/`: `RALSynopseClient.pas`, `RALSynopseRegister.pas`, `RALSynopseServer.pas`
- `engine/unigui/`: `RALUniGUIRegister.pas`, `RALUniGUIServer.pas`

---

### 3.4) `src/languages/`

- `ralconsts_enus.inc`
- `ralconsts_eses.inc`
- `ralconsts_ptbr.inc`
- `rallang.es_ES`
- `rallang.pt_BR`

---

### 3.5) `src/others/` (dependências externas)

- `libsagui.pas`
- `RALBSONReg.pas`
- `RALCompressBrotli.pas`
- `RALCompressZStd.pas`

**kxBSON**
- `others/kxBSON/Source/kxBSON.pas`

**pascal_brotli**
- `others/pascal_brotli/brotlilib.pas`
- `others/pascal_brotli/brotlistream.pas`

**ZSTD**
- `others/ZSTD/LZ4.pas`
- `others/ZSTD/LZ4Lib.pas`
- `others/ZSTD/ZSTD.pas`
- `others/ZSTD/ZSTDLib.pas`

---

### 3.6) `src/utils/` (utilitários)

- `RALBase64.pas`
- `RALCompress.pas`
- `RALCompressZLib.pas`
- `RALCRC32.pas`
- `RALCripto.pas`
- `RALCriptoAES.pas`
- `RALCriptoOpenSSL.pas`
- `ralcrud.pas`
- `RALCustomObjects.pas`
- `RALHashBase.pas`
- `RALHashes.pas`
- `RALHexadecimal.pas`
- `RALJson.pas`
- `RALMD5.pas`
- `RALSHA1.pas`
- `RALSHA2_32.pas`
- `RALSHA2_64.pas`
- `RALMIMETypes.pas`
- `RALMultipartCoder.pas`
- `RALOpenSSL.pas`
- `RALResponsePages.pas`
- `RALStorage.pas`
- `RALStorageBIN.pas`
- `RALStorageBSON.pas`
- `RALStorageCSV.pas`
- `RALStorageJSON.pas`
- `RALStream.pas`
- `RALThreadSafe.pas`
- `RALToken.pas`
- `RALTools.pas`
- `RALTranslate.pas`
- `RALUrlCoder.pas`

**Includes de JSON (multi-compatibilidade):**
- `RALJSON_Delphi.inc`
- `RALJSON_FPC.inc`
- `RALJSON_lkJSON.inc`
- `RALJSON_uJSON.inc`

---

### 3.7) `src/wizard/`

Delphi
- `wizard/delphi/RALWizard.pas`
- `wizard/delphi/RALWizardForm.dfm`
- `wizard/delphi/RALWizardForm.pas`
- `wizard/delphi/RALWizardObjects.pas`
- `wizard/delphi/RALWizardReg.pas`
- `wizard/delphi/RALWizardTools.pas`
- `wizard/delphi/cgi/RALWizardProjCGI.pas`
- `wizard/delphi/console/RALWizardProjConsole.pas`
- `wizard/delphi/standalone/RALWizardFormStandAlone.pas`
- `wizard/delphi/standalone/RALWizardProjStandAlone.pas`

Lazarus
- `wizard/lazarus/ralwizard.pas`
- `wizard/lazarus/ralwizardform.lfm`
- `wizard/lazarus/ralwizardform.pas`

---

## 4) Fluxo conceitual (para IA)

### 4.1) Servidor

1. `RALServer` mantém o ciclo e a configuração geral.
2. `RALRoutes` define e resolve rotas.
3. Engines em `src/engine/*` recebem requisições e convertem para `RALRequest`.
4. Execução da rota (em `RALRoutes` / `RALServer`) produz uma `RALResponse`.
5. Engines transformam `RALResponse` em resposta HTTP.

### 4.2) Cliente

1. `RALClient` orquestra chamadas.
2. Cada engine (Indy/fpHTTP/netHTTP/...) implementa o transporte.
3. `RALRequest` é codificado (params/headers/body; compress/cripto quando configurado).
4. A resposta é decodificada para `RALResponse`.

### 4.3) DBWare

1. `RALDBModule` mapeia requests para queries/execs.
2. Conectores (FireDAC/SQLDB/Zeos) implementam `Open/Exec/ApplyUpdates`.
3. `RALDBStorage*` e utilitários em `src/utils/` lidam com serialização (JSON/BIN/BSON/CSV).

---

## 5) Como usar este mapa

- Para entender rotas e execução: `src/base/RALRoutes.pas` e `src/base/RALServer.pas`.
- Para params/request/response: `src/base/RALParams.pas`, `src/base/RALRequest.pas`, `src/base/RALResponse.pas`.
- Para multipart: `src/utils/RALMultipartCoder.pas`.
- Para compressão/cripto/stream: `src/utils/RALCompress*.pas`, `src/utils/RALCripto*.pas`, `src/utils/RALStream.pas`.

---

## 6) Próximos aprimoramentos do PROJECT_MAP

Este arquivo pode ser enriquecido com:
- tabelas “arquivo → responsabilidade → principais classes/funções”
- mapa de dependências (`uses`/unidades)
- seções específicas de performance e multi-thread.

---

