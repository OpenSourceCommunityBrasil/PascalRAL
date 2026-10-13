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

Desde a 2.0, **uma pasta pertence a um pacote só**; a exceção é `src/others`, que
guarda todos os submódulos e nenhuma unit do RAL. As units de design ficam numa
subpasta `design/` própria: o `lazbuild -B` compila de novo, dentro do pacote de
design, toda unit de runtime que achar na mesma pasta. Nomes de pacote:
`RAL<Categoria><Item>Package` / `RAL<Categoria><Item>Dsgn`; motores `<Motor>RAL`;
`PascalRAL` e `PascalRALDsgn` são os obrigatórios (raiz de `pkg/<IDE>`). O plano e as
decisões estão em `.agents/PLANO_REFORMA_2_0.md`.

| pasta | pacote(s) | o que tem |
|---|---|---|
| `src/core/` | `PascalRAL` | servidor, cliente, request/response, rotas, params, plugins (núcleo), base da autenticação |
| `src/bindings/` | `MsQuicRAL`, `SaguiRAL` | bindings de bibliotecas C (`MsQuic.pas`, `libsagui.pas`), sem unit do RAL |
| `src/common/` | `PascalRAL` | utilitários do núcleo: JSON, stream, MIME, URL, multipart, rede, base64, quadro QUIC |
| `src/compress/` | `PascalRAL` | compressão do núcleo (zlib/gzip/deflate) e o registro de compressores |
| `src/compress/brotli/` | `RALCompressBrotliPackage` | Brotli (o submódulo fica em `src/others/pascal_brotli`) |
| `src/compress/zstd/` | `RALCompressZStdPackage` | ZStd (o submódulo fica em `src/others/ZSTD`) |
| `src/crypto/` | `PascalRAL` | hashes, AES, OpenSSL, BigInt/ASN.1/RSA/X.509, JWS, JWT (`RALToken`) |
| `src/database/` | `RALDBPackage` | DBWare: módulo, conexão, pool, cache de SQL, base dos drivers |
| `src/database/FireDAC/` | `RALDBFireDACDAOPackage`, `RALDBFireDACPackage` | driver e memtable FireDAC, DAO |
| `src/database/FireDAC/design/` | `RALDBFireDACDsgn` | design do FireDAC |
| `src/database/Zeos/` | `RALDBZeosPackage` | driver e memtable Zeos |
| `src/database/Zeos/design/` | `RALDBZeosDsgn` | design do Zeos |
| `src/database/design/` | `RALDBDsgn` | design do DBWare |
| `src/database/sqldb/` | `raldbsqldbpackage.lpk` | driver e dataset sqldb (Lazarus) |
| `src/database/sqldb/design/` | `raldbsqldbdsgn.lpk` | design do sqldb |
| `src/design/` | `PascalRALDsgn` | design do núcleo (`RALRegister`, editores de propriedade) |
| `src/modules/soap/` | `RALModuleSOAPPackage` | SOAP: módulo, cliente, WSDL |
| `src/modules/soap/design/` | `RALModuleSOAPDsgn` | design do SOAP (verbos de IDE) |
| `src/modules/swagger/` | `RALModuleSwaggerPackage` | módulo Swagger e exportadores Swagger/Postman |
| `src/modules/web/` | `RALModuleWebPackage` | módulo web (arquivos estáticos, sessões) |
| `src/modules/xml/` | `RALModuleXMLPackage` | XML (leitor, escritor, DOM) e o storage XML |
| `src/plugins/` | `PascalRAL` | plugins do núcleo: segurança, CORS, conteúdo (limites, compressão, cripto, JSON), concorrência |
| `src/plugins/auth/` | `RALPluginAuthPackage` | autenticação: Basic e JWT (`RALAuthentication`), Digest, OAuth2 |
| `src/plugins/tls/` | `RALPluginTLSPackage` | certificado autoassinado (`RALSelfSigned`) |
| `src/storage/` | `RALStoragePackage` | storage de dataset: base, tipos (`RALStorageTypes`; `RALDBTypes` é a unit de transição), BIN, JSON, CSV |
| `src/storage/bson/` | `RALStorageBSONPackage` | storage BSON (o submódulo kxBSON fica em `src/others/kxBSON`) |
| `src/wizard/delphi/` | `RALWizardDsgn` | assistentes de projeto (Delphi) |
| `src/wizard/delphi/cgi/` | `RALWizardDsgn` |  |
| `src/wizard/delphi/console/` | `RALWizardDsgn` |  |
| `src/wizard/delphi/standalone/` | `RALWizardDsgn` |  |
| `src/wizard/lazarus/` | `RALWizardDsgn` | assistentes de projeto (Lazarus) |
| `src/engine/<motor>/` | `<Motor>RAL` (`indyral.lpk`...) | um motor por pasta |
| `src/languages/` | (incluído pelo `RALConsts`) | textos por idioma |
| `src/others/` | (o pacote que usa cada um) | só submódulos: ZSTD, pascal_brotli, kxBSON |

## 3) Mapa detalhado por pasta (unidades)

### 3.1) `src/core/`

- `RALAuthBase.pas` — PascalRAL
- `RALClient.pas` — PascalRAL
- `RALConsts.pas` — PascalRAL
- `RALCustomObjects.pas` — PascalRAL
- `RALParams.pas` — PascalRAL
- `RALPlugin.pas` — PascalRAL
- `RALRequest.pas` — PascalRAL
- `RALResponse.pas` — PascalRAL
- `RALRoutes.pas` — PascalRAL
- `RALServer.pas` — PascalRAL
- `RALTypes.pas` — PascalRAL

### 3.2) `src/bindings/`

- `libsagui.pas` — SaguiRAL
- `MsQuic.pas` — MsQuicRAL

### 3.3) `src/common/`

- `RALBase64.pas` — PascalRAL
- `RALExternalsLibraries.pas` — PascalRAL
- `RALHexadecimal.pas` — PascalRAL
- `RALJson.pas` — PascalRAL
- `RALMIMETypes.pas` — PascalRAL
- `RALMultipartCoder.pas` — PascalRAL
- `RALNetwork.pas` — PascalRAL
- `RALQuicFrame.pas` — PascalRAL
- `RALResponsePages.pas` — PascalRAL
- `RALStream.pas` — PascalRAL
- `RALThreadSafe.pas` — PascalRAL
- `RALTools.pas` — PascalRAL
- `RALTranslate.pas` — (fora de pacote)
- `RALUrlCoder.pas` — PascalRAL

### 3.4) `src/compress/`

- `RALCompress.pas` — PascalRAL
- `RALCompressZLib.pas` — PascalRAL

### 3.5) `src/compress/brotli/`

- `RALCompressBrotli.pas` — RALCompressBrotliPackage

### 3.6) `src/compress/zstd/`

- `RALCompressZStd.pas` — RALCompressZStdPackage

### 3.7) `src/crypto/`

- `RALASN1.pas` — PascalRAL
- `RALBigInt.pas` — PascalRAL
- `RALCRC32.pas` — PascalRAL
- `RALCripto.pas` — PascalRAL
- `RALCriptoAES.pas` — PascalRAL
- `RALCriptoOpenSSL.pas` — PascalRAL
- `RALHashBase.pas` — PascalRAL
- `RALHashes.pas` — PascalRAL
- `RALJWS.pas` — PascalRAL
- `RALMD5.pas` — PascalRAL
- `RALOpenSSL.pas` — PascalRAL
- `RALRSA.pas` — PascalRAL
- `RALSHA1.pas` — PascalRAL
- `RALSHA2_32.pas` — PascalRAL
- `RALSHA2_64.pas` — PascalRAL
- `RALToken.pas` — PascalRAL
- `RALX509.pas` — PascalRAL

### 3.8) `src/database/`

- `RALDBBase.pas` — RALDBPackage
- `RALDBConnection.pas` — RALDBPackage
- `RALDBModule.pas` — RALDBPackage
- `RALDBPool.pas` — RALDBPackage
- `RALDBSQLCache.pas` — RALDBPackage

### 3.9) `src/database/FireDAC/`

- `RALDBFireDAC.pas` — RALDBFireDACPackage
- `RALDBFiredacDAO.pas` — RALDBFireDACDAOPackage
- `RALDBFiredacMemTable.pas` — RALDBFireDACPackage

### 3.10) `src/database/FireDAC/design/`

- `RALDBFireDACReg.pas` — RALDBFireDACDsgn

### 3.11) `src/database/Zeos/`

- `RALDBZeos.pas` — RALDBZeosPackage
- `RALDBZeosMemTable.pas` — RALDBZeosPackage

### 3.12) `src/database/Zeos/design/`

- `RALDBZeosReg.pas` — RALDBZeosDsgn

### 3.13) `src/database/design/`

- `RALDBReg.pas` — RALDBDsgn

### 3.14) `src/database/sqldb/`

- `RALDBBufDataset.pas` — raldbsqldbpackage.lpk
- `RALDBSQLDB.pas` — raldbsqldbpackage.lpk

### 3.15) `src/database/sqldb/design/`

- `RALDBSQLDBReg.pas` — raldbsqldbdsgn.lpk

### 3.16) `src/design/`

- `RALRegister.pas` — PascalRALDsgn

### 3.17) `src/modules/soap/`

- `RALSOAPActions.pas` — RALModuleSOAPPackage
- `RALSOAPClient.pas` — RALModuleSOAPPackage
- `RALSOAPModule.pas` — RALModuleSOAPPackage
- `RALSOAPTypes.pas` — RALModuleSOAPPackage
- `RALWSDLExporter.pas` — RALModuleSOAPPackage

### 3.18) `src/modules/soap/design/`

- `RALModuleSOAPReg.pas` — RALModuleSOAPDsgn

### 3.19) `src/modules/swagger/`

- `RALModuleSwaggerReg.pas` — RALModuleSwaggerPackage
- `RALPostmanExporter.pas` — RALModuleSwaggerPackage
- `RALSwaggerExporter.pas` — RALModuleSwaggerPackage
- `RALSwaggerModule.pas` — RALModuleSwaggerPackage

### 3.20) `src/modules/web/`

- `RALModuleWebReg.pas` — RALModuleWebPackage
- `RALWebModule.pas` — RALModuleWebPackage

### 3.21) `src/modules/xml/`

- `RALModuleXMLReg.pas` — RALModuleXMLPackage
- `RALStorageXML.pas` — RALModuleXMLPackage
- `RALXML.pas` — RALModuleXMLPackage
- `RALXMLReader.pas` — RALModuleXMLPackage
- `RALXMLWriter.pas` — RALModuleXMLPackage

### 3.22) `src/plugins/`

- `RALConcurrency.pas` — PascalRAL
- `RALContent.pas` — PascalRAL
- `RALCORS.pas` — PascalRAL
- `RALSecurity.pas` — PascalRAL

### 3.23) `src/plugins/auth/`

- `RALAuthentication.pas` — RALPluginAuthPackage
- `RALDigest.pas` — RALPluginAuthPackage
- `RALOAuth2.pas` — RALPluginAuthPackage
- `RALPluginAuthReg.pas` — RALPluginAuthPackage

### 3.24) `src/plugins/tls/`

- `RALPluginTLSReg.pas` — RALPluginTLSPackage
- `RALSelfSigned.pas` — RALPluginTLSPackage

### 3.25) `src/storage/`

- `RALDBTypes.pas` — RALStoragePackage
- `RALStorage.pas` — RALStoragePackage
- `RALStorageBIN.pas` — RALStoragePackage
- `RALStorageCSV.pas` — RALStoragePackage
- `RALStorageJSON.pas` — RALStoragePackage
- `RALStorageReg.pas` — RALStoragePackage
- `RALStorageTypes.pas` — RALStoragePackage

### 3.26) `src/storage/bson/`

- `RALStorageBSON.pas` — RALStorageBSONPackage
- `RALStorageBSONReg.pas` — RALStorageBSONPackage

### 3.27) `src/engine/` (transport engines)

- `engine/cgi/`: `RALCGIReg.pas`, `RALCGIServer*.inc`, `RALCGIServer.pas`
- `engine/fpHTTP/`: `RALfpHTTPClient.pas`, `RALfpHTTPReg.pas`, `RALfpHTTPServer.pas`
- `engine/indy/`: `RALIndyClient.pas`, `RALIndyReg.pas`, `RALIndyServer.pas`
- `engine/msquic/`: `MsQuic.pas` (binding da API C, carga dinamica), `RALMsQuicClient.pas`, `RALMsQuicReg.pas`, `RALMsQuicServer.pas` — QUIC (RFC 9000) por MsQuic; **nao e HTTP/3**: o que trafega e um frame binario proprio, entao as DUAS pontas tem de ser RAL. Precisa da `msquic.dll`/`libmsquic.so.2` (build OpenSSL) em runtime — no Android e `libmsquic.so`, deployada em `library\lib\arm64-v8a\` e `library\lib\armeabi-v7a\`, porque la nao ha soname versionado; a lib so e carregada no `Active := True`. Servidor E cliente rodam no Android 9+ (API 28) sem port, verificados no aparelho nos dois ABIs em 02/10/2026; os binarios (Windows x64/x86, Android arm64/armv7) ficam zipados na branch `external`, em `msquic/`. `README.md` e o guia de uso (deploy por plataforma, certificados); fora do Windows a validacao de certificado e a do OpenSSL (o check de plataforma do msquic e um stub no POSIX), e no Android a loja do sistema e juntada num PEM no cache — `TRALMsQuicClientHTTP.DefaultCaFile` troca a loja
- `engine/kwik/`: `RALKwikClient.pas`, `RALKwikReg.pas`, `java/` (ponte JNI + 5 jars), `README.md` — cliente QUIC de **Android**, so Delphi; fala o MESMO frame do MsQuic (os dois usam `RALQuicFrame`), entao conversa com o `TRALMsQuicServer`; nasceu quando o MsQuic nao tinha `.so` para Android, e hoje fica para o Android 8 (API 26-27, abaixo do piso do msquic) e para app que nao quer biblioteca nativa. Kwik e LGPL v3
- `engine/netHTTP/`: `RALnetHTTPClient.pas`, `RALNetHTTPReg.pas`
- `engine/okhttp/`: `RALOkHttpClient.pas`, `RALOkHttpReg.pas`, `java/` (ponte JNI + jars), `README.md` — cliente, só Android, só Delphi; é o único caminho para HTTP/2 lá
- `engine/sagui/`: `RALSaguiReg.pas`, `RALSaguiServer.pas`
- `engine/synopse/`: `RALSynopseClient.pas`, `RALSynopseReg.pas`, `RALSynopseServer.pas`
- `engine/unigui/`: `RALUniGUIReg.pas`, `RALUniGUIServer.pas`

**O que é igual entre os motores, e onde não dá para ser (2026-09-15)**

- `MaxConnections` — teto de conexões abertas ao mesmo tempo, mesmo nome e mesma semântica (`0` = sem teto, e quem passa do teto tem a conexão NOVA recusada) no Indy, no fpHTTP, no Sagui e no Synopse. No Sagui chamava-se `ConnectionLimit` até esta data; o nome velho continua compilando (propriedade pública, não publicada) e um `.dfm`/`.lfm` antigo ainda carrega, por `DefineProperties`. **UniGUI e CGI ficam de fora por natureza**: um não tem escuta própria, o outro não tem servidor.
- `MaxKeepAliveConnections` (Synopse) **não** é a mesma coisa: não recusa ninguém, só para de conceder keep-alive, e só o `smThreads` tem esse teto.
- `Protocol`/`ProtocolVersion` — **todos** os seis servidores e **todos** os cinco clientes preenchem, mesmo os que só falam HTTP/1.1.
- `ShareConnection` — netHTTP, OkHttp e MsQuic, e `SupportsSharedConnection` diz quais; **ligado por padrão desde 20/09/2026**. Nos outros o transporte é um objeto por conexão: honrar a propriedade **serializaria** chamada concorrente, então eles a ignoram. `PoolConnection.Enabled` (reuso do objeto engine, e da conexão dele) também nasce ligado.
- `KeepAliveInterval` — netHTTP e OkHttp, os dois com frame PING de HTTP/2 de verdade: o OkHttp por `pingInterval`, o netHTTP por `WINHTTP_OPTION_HTTP2_KEEPALIVE` (164) no handle de sessão, que **só existe do Windows 11 em diante** e onde não existe é ignorado em silêncio. O piso é do motor (`MinKeepAliveInterval`: 5000 no netHTTP, nenhum no OkHttp) e é aplicado **na atribuição**, para o valor que se lê ser o que vale.

**Propriedade que não se aplica some do Object Inspector, e nunca levanta.** Quem decide é o próprio componente, em `TRALComponent.IsPropertyRelevant` (`RALCustomObjects`), e a cola de IDE fica num lugar só, `TRALSelectionEditor` em `RALRegister` — assim nenhum motor precisa depender da IDE. Esconder é conforto: o valor escondido pode ser sobra de outra configuração, então é **ignorado**. O que levanta é escolha explícita impossível (`smHttpSys` fora do Windows, `rhv2` num motor sem h2), e essa nunca fica escondida.

Isso vale **um nível abaixo também**, desde 2026-09-16: as quatro propriedades de arquivo do `SSL` somem do servidor Synopse em `smHttpSys`, onde o certificado vem da loja da máquina pelo `netsh` e nenhum `.pem` é lido. O componente é perguntado pelo nome **composto** (`IsPropertyRelevant('SSL.CertificateFile')`), então um método só responde pelos dois níveis, e a cola de IDE é o `TRALNestedProperty`, registrado pelo tipo base — o que faz valer para o `SSL` de qualquer motor sem uma linha de design-time em cada um.

---

### 3.28) `src/modules/xml` e `src/modules/soap` (o que cada unit faz)

XML (`pkg/Delphi/Modules/RALModuleXMLPackage`, `pkg/Lazarus/Modules/ralmodulexmlpackage.lpk`)
- `modules/xml/RALXMLReader.pas` - leitor pull (XML 1.0, namespaces, entidades com limites, codificacoes)
- `modules/xml/RALXMLWriter.pas` - escritor em stream, escapes, nomes codificados
- `modules/xml/RALXML.pas` - DOM com API fluente, caminhos (subconjunto do XPath e forma com pontos), valores tipados
- `modules/xml/RALStorageXML.pas` - `TRALStorageXMLLink`: DBWare, RAW, DataPacket (MIDAS/TBufDataset), ADO; helpers `ToXML`/`LoadFromXML`
- `modules/xml/RALModuleXMLReg.pas` - paleta `RAL - Storage`

SOAP (`pkg/Delphi/Modules/RALModuleSOAPPackage`, `pkg/Lazarus/Modules/ralmodulesoappackage.lpk`; requer o XML)
- `modules/soap/RALSOAPTypes.pas` - versoes, namespaces, `ERALSOAPFault`, montar e ler faults
- `modules/soap/RALSOAPModule.pas` - `TRALSOAPModule` (paleta `RAL - Modules`), operacoes, `TRALSOAPRequest`/`TRALSOAPResponse`
- `modules/soap/RALWSDLExporter.pas` - WSDL 1.1 com bindings SOAP 1.1 e 1.2
- `modules/soap/RALSOAPClient.pas` - `TRALSOAPClient` (descende de `TRALClient`), `ImportWSDL`, operacoes, chamada em thread
- `modules/soap/RALSOAPActions.pas` - acao `TRALSOAPExecute`
- `modules/soap/RALModuleSOAPReg.pas` - registro, verbos "Importar WSDL"/"Exportar WSDL"

---

### 3.29) `src/wizard/`

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
5. `TRALClient.ExecuteThread` decide **em qual thread** a chamada roda: `ebSingleThread`
   (**padrão desde 20/09/2026**; antes era `ebMultiThread`) roda na própria thread chamadora
   e chama o callback **antes** de retornar, então o resultado pode ser lido na linha
   seguinte — e um `Get` num botão trava a tela enquanto dura; `ebMultiThread` dispara uma
   `TRALThreadClient` e volta na hora — o callback chega depois, pela main thread, via
   `OnTerminate`. O callback sempre recebe uma `TRALResponse` válida; o erro vem em
   `AException`.
6. Qual certificado o cliente aceita é decidido no `TRALClient`, nunca no engine:
   `SSL.Pins` (lista: `fp`, `host=fp` ou `host:porta=fp` — resolvida por conexão,
   então pin de um servidor não afeta os outros), `SSL.Required` (recusa http puro),
   `SSL.Verify` (o que o próprio engine faz: `svEngine` mantém o de hoje, que não é
   igual em todos, `svAlways` liga onde está desligado, `svNever` aceita qualquer um)
   e `OnValidateServerCert`. Recusa de certificado chega como
   `TransportError = rteCertificate` em qualquer engine. Cada engine só traduz o callback nativo dele para
   `TRALCertInfo` e pergunta ao `TRALClientHTTP.AcceptServerCert`, onde mora a regra
   única — evento, senão pin, senão o veredito do próprio engine. Sem pin e sem evento
   nada muda, inclusive o fato de Indy e fpHTTP não validarem certificado por padrão.
   Ver `CLAUDE.md`, "Which server certificate a client accepts".
7. `OnBeforeExecute` e `OnAfterExecute` também moram no `TRALClient` e também rodam
   dentro do `BeforeSendUrl`, então valem para os seis engines em Delphi e FPC sem
   tocar em unit de engine nenhuma. Eles dão à aplicação a palavra sobre **cada
   tentativa**: o primeiro roda depois da URL e da política de TLS dela decididas e
   antes de qualquer trabalho de rede (inclusive a busca de token), pode acrescentar
   header ou param no `ARequest` e pode recusar a tentativa com `ACancel` +
   `ACancelReason`, que falha com `rteCancelled`; o segundo roda quando a tentativa
   termina, seja como for que ela termine, e sempre pareado com o primeiro.
   `TRALExecInfo` leva URL, método, tentativa (1-based), engine, `Elapsed` em ms,
   `StatusCode`, `TransportError` e `ErrorMessage`. Tentativa não é chamada: o failover
   e o reenvio do 401 disparam o par de novo, com `Attempt` maior. Ver `CLAUDE.md`,
   "The application's own say over each attempt".
8. O autenticador do cliente e' COMPARTILHADO de proposito (Authentication usa
   FreeNotification, nao posse), entao o que ele guarda entre requisicoes e' tocado
   por todas as threads dos clientes que o dividem. A trava disso e'
   `TRALAuthClient.Lock`/`Unlock` - no autenticador, nao no cliente, porque o
   `LockSession` so protegia um cliente contra ele mesmo. O `BeforeSendUrl` a segura
   durante todo o `SetAuthToken`, o que transforma N clientes sem token em UM
   `/gettoken` em vez de N. Ver `CLAUDE.md`, "One authenticator, many clients".

### 4.3) DBWare

1. `RALDBModule` mapeia requests para queries/execs.
2. Conectores (FireDAC/SQLDB/Zeos) implementam `Open/Exec/ApplyUpdates`.
3. `RALStorage*` em `src/storage/` (e `src/storage/bson`, `src/modules/xml`) lidam com serialização (JSON/BIN/BSON/CSV).

---

## 5) Como usar este mapa

- Para entender rotas e execução: `src/core/RALRoutes.pas` e `src/core/RALServer.pas`.
- Para params/request/response: `src/core/RALParams.pas`, `src/core/RALRequest.pas`, `src/core/RALResponse.pas`.
- **Parâmetros tipados** (`TRALParamType` em `RALTypes.pas`, content types `rctRAL*`
  em `RALMIMETypes.pas`): um param de body pode viajar como valor binário
  little-endian em vez de texto, para não depender do locale das duas pontas.
  `Params.AddParam(nome, valor, rpkBODY, rptDouble)` na ida; do outro lado
  qualquer acessor (`AsInt64`, `AsDouble`, `AsString`…) converte a partir do
  marcador, e `IsTyped` diz se veio tipado. Param de texto continua exatamente
  como sempre foi: só grava marcador quem chama `SetTyped*`, e qualquer escrita
  de conteúdo (`AsString`, `AsStream`, `OpenFile`) derruba o marcador.
  `MediaType` compara o content type ignorando parâmetros como
  `; charset=utf-8` — que é o que o header HTTP acrescenta no caminho de param
  único, onde o marcador viaja como content type da resposta inteira.
- **Separador de header por engine**: cada engine entrega a lista de headers num
  formato diferente — Indy e Synopse passam linhas `Nome: Valor`, o fpHTTP passa
  `TRequest.CustomHeaders`, que é `nome=valor`. `FindHeaderNameSeparator` decide
  pelo dado (vence o que vier primeiro entre `': '` e `'='`), não pelo nome do
  engine. Mexer nisso quebra headers de um engine ou de outro em silêncio.
- Para multipart: `src/common/RALMultipartCoder.pas`.
- Para compressão/cripto/stream: `src/compress/RALCompress*.pas`, `src/crypto/RALCripto*.pas`, `src/common/RALStream.pas`.

---

## 6) Próximos aprimoramentos do PROJECT_MAP

Este arquivo pode ser enriquecido com:
- tabelas “arquivo → responsabilidade → principais classes/funções”
- mapa de dependências (`uses`/unidades)
- seções específicas de performance e multi-thread.

---

