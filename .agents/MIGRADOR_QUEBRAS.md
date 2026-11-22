# Quebras de versão do RAL: conhecimento humano (agente m0-wiki)

Fontes lidas: wiki `atualizacao.md`, `migrar-para-2.0.md`, `plugins.md`; gabarito `677864d` (Samples-2.0) e `37a6638` (Samples); código em `PascalRAL-dev` (tags `v1.0`, `1.1`, branch `dev`) e `PascalRAL-2.0` (HEAD `c319cc7`). Tudo conferido no código, não só na wiki.

Âncoras da wiki: `atualizacao#10--11` (1.0 → 1.1), `atualizacao#11--12-` (1.1 → 1.2), `atualizacao#12--20-` (1.2 → 2.0), `migrar-para-2.0#<seção>`, `plugins#o-que-mudou-da-12-para-a-20`.

Chaves de diff: padrões com `*`. Onde o diff de API provavelmente NÃO enxerga a quebra (unit com o mesmo nome, só mudou de pasta), está dito.

Fatos que valem para todo o arquivo:
- `dev` = 1.2 em desenvolvimento. A 2.0 vem do branch `1.3-plugin` (mesclado com o `dev` em `52f3fd2`); por isso há itens da wiki que citam coisas que nunca existiram em 1.0/1.1/dev (ver Divergências).
- Em 1.0, 1.1 e dev **todos os pacotes de runtime Delphi exigem `PascalRALDsgn`** (pacote de design) e o `PascalRAL.dpk` contém autenticação, módulos Web/Swagger, storages e `RALDBTypes`. É por isso que na 2.0 aparecem pacotes "novos" a exigir.
- Vocabulário do plano: falta uma ação para **acrescentar unit ao `uses`** (`uses-adicionar`). Várias regras abaixo precisam dela (criptotype, Digest/OAuth2, plugins). Hoje só `propriedade-para-componente` a embute. Proponho acrescentar `uses-adicionar` (condição: classe X usada no `.pas`/`.dfm` e unit Y ausente do `uses`).

---------------------------------------------------------------------
# PASSO v1.0 > 1.1
---------------------------------------------------------------------

## 1.1 / Regras

Gabarito: o `37a6638` (Samples, "Correção de exemplos Delphi para versão 1.1 do RAL") **é mesmo 1.0 → 1.1**, mas só prova UMA regra (criptotype). Dos 5 arquivos, 3 mudanças são ruído (ExplicitTop no .dfm, `OldCreateOrder`, SQL do Firebird no `udm.pas`). Ver Divergências.

| id | ação | de → para (exato) | chave de diff provável | wiki |
|---|---|---|---|---|
| `1.1/caminho-plugins-modules` | `caminho-trocar` (acrescentar) | `DCC_UnitSearchPath` do `.dproj` / `OtherUnitFiles` do `.lpi`: onde houver `$(PascalRAL)\base` (ou `...\src\base`), acrescentar `$(PascalRAL)\base\plugins` e `$(PascalRAL)\base\modules`. Quem lista `...\src\utils` e usa Swagger/Postman/ExternalsLibraries também precisa de `base\modules` (as 3 units saíram de `utils`) | **o diff de API não vê** (nomes iguais). Possíveis: `unit-pasta-mudou:RALAuthentication`, `unit-pasta-mudou:RALSwaggerModule`, `unit-pasta-mudou:RALWebModule`, `unit-pasta-mudou:RALSwaggerExporter`, `unit-pasta-mudou:RALPostmanExporter`, `unit-pasta-mudou:RALExternalsLibraries` | atualizacao#10--11 (linha "novas pastas") |
| `1.1/criptotype-tipo` | `identificador-renomear` | `TCriptoType` → `TRALCriptoType` (declaração de variável/parâmetro/campo em `.pas`) | `classe-sumiu:TCriptoType`; `metodo-sumiu:TRALHashes.Encrypt` e `.Decrypt` com parâmetro `TCriptoType` | sem linha na wiki (ver Divergências); commit `4056f7d` |
| `1.1/criptotype-valores` | `identificador-renomear` | `ctAES128` → `crAES128`; `ctAES192` → `crAES192`; `ctAES256` → `crAES256`. **Só** quando o contexto é `TRALHashes.Encrypt/Decrypt` ou variável `TCriptoType`. **Nunca** tocar `ctNone`/`ctGZip`/`ctDeflate`/`ctZLib` (são de `TRALCompressType`) | `enum-valor-sumiu:TCriptoType.ctAES128`, `.ctAES192`, `.ctAES256` | idem; gabarito `37a6638` (FMX e VCL `Cripto Sample/Unit1.pas`, 6 trocas em cada) |
| `1.1/criptotype-uses` | `uses-adicionar` (nova) | acrescentar `RALTypes` ao `uses` do `.pas` que citava `ctAES*` (onde `TRALCriptoType` está declarado: `RALTypes`) | derivada de criptotype-valores | gabarito `37a6638` (`RALHashes, RALTypes`) |
| `1.1/jwt-eventos-of-object` | `padrao-avisar` + `propriedade-renomear` condicional (código) | `TRALServerJWTAuth.OnGetToken` e `.OnValidate` passaram a `TRALOnTokenJWT ... of object` (antes procedimento solto). Atribuição em **código** `X.OnGetToken := ProcSolto` (a RHS é `procedure` de nível de unit, não método de classe) → `X.OnGetTokenGen := ProcSolto` / `X.OnValidate := ProcSolto` → `X.OnValidateGen := ProcSolto`. Se a RHS não é provadamente solta: item manual "mova o procedimento para dentro do formulário/datamodule ou use `...Gen`". Em `.dfm/.lfm` (`OnGetToken = jwtGetToken`) nada muda se já era método do formulário | `evento-mudou:TRALOnTokenJWT`; `published-novo:TRALServerJWTAuth.OnGetTokenGen` (as `*Gen` são `public`, não `published`) | atualizacao#10--11 (linha 2); commit `0a9b80f` "Breaking: JWT callbacks take a method" |
| `1.1/addcookie-obsoleto` | `padrao-avisar` | `TRALResponse.AddCookie(Nome, Valor)` ficou `deprecated 'use AddCookie(ACookie: TRALCookie)'`. Só aviso | `metodo-obsoleto:TRALResponse.AddCookie` | sem wiki; código `RALResponse.pas` 1.1 |
| `1.1/dbbase-descendente` | `padrao-avisar` | quem escreveu descendente próprio de `TRALDBBase`: sumiram os `class function DatabaseName`/`PackageDependency` (eram `virtual; abstract`) em FireDAC/sqldb/Synopse; novos virtuais **não abstratos** `Connect/Disconnect/IsConnected/ResetSession/TestConnection/ValidationSQL`. Raríssimo | `metodo-sumiu:TRALDB*.DatabaseName`, `metodo-sumiu:TRALDB*.PackageDependency` | sem wiki; justificativa: interno |

Sem regra, com justificativa (não quebra projeto de usuário):
- `default-mudou:TRALClient.ConnectTimeout` / `.RequestTimeout` (directive `default 5000/30000` → `30000/10000`): o construtor já usava 30000/10000, então forma salva e comportamento não mudam (bug de streaming corrigido). **Não materializar.**
- `unit-pasta-mudou` das `ralconsts_*.inc` (`base` → `languages`): resolvido pelo `{$I ..\languages\...}` do próprio `RALConsts.pas`.
- `classe-sumiu:TRALThreadCriptoAES` (RALCriptoAES), `metodo-sumiu:TRALSynopseClient.EngineName/EngineVersion/PackageDependency`, `metodo-sumiu:TRALSaguiServer.DecodeAuth` (agora é o `TRALServer.DecodeAuth`, público): internos.
- `published->public:TRALDBBase.DriverType`: era só leitura, nunca foi gravado em formulário.
- `RALDBPool` (unit nova, `RALDBPackage`), `PoolOptions`, `OnValidateSQL`, `LibLocation`, `MaxRedirects`, `SSL.Pin/Pins/Required/Verify`, `OnBeforeExecute/OnAfterExecute`, `TRALParamType`/`SetTyped*`, `AsDateTime`: só adições.

Pacotes: **nenhum nome de pacote mudou** entre 1.0 e 1.1 (Delphi: 15 `.dpk`; Lazarus: 14 `.lpk`; só ganharam units). Nenhuma unit sumiu ou foi renomeada (só `RALDBPool` nova). Conferido por `ls-tree` das duas tags.

## 1.1 / Comportamento

| id | o que é | texto para o usuário (pt-BR) | wiki |
|---|---|---|---|
| `1.1/aes-cbc-incompativel` | Criptografia AES virou CBC real (IV aleatório de 16 bytes no início do corpo, PKCS#7, HMAC-SHA256 no fim). 1.0 e 1.1 **não se entendem**, nem em `TRALHashes`/`TRALCriptoAES` usados fora de rede | "Atualize cliente e servidor juntos para a 1.1: um lado 1.0 e outro 1.1 não conseguem conversar com criptografia ligada. Dados que você cifrou com a 1.0 e guardou (arquivo, banco) **não abrem** na 1.1; decifre antes de atualizar e cifre de novo depois." | atualizacao#10--11 (linha 3). O aviso de dado guardado não está na wiki |
| `1.1/hashes-string-base64` | `TRALHashes.Encrypt(string)` devolve **base64** do cifrado e `Decrypt(string)` espera base64. Antes o texto era o byte cru, estragado pela conversão UTF-8; e `Decrypt(string, TRALCriptoType)` chamava `Encrypt` (bug) | "O texto cifrado por `TRALHashes.Encrypt` agora é base64. Se você gravava ou comparava o texto cifrado, regrave. As sobrecargas com `TStream` continuam binárias." | não está (commit `4056f7d`) |
| `1.1/jwt-sem-ongettoken` | A rota de token do `TRALServerJWTAuth` sem `OnGetToken` responde 401 (antes assinava o JSON que o cliente mandasse). Renovação usa os claims originais; o corpo não reescreve mais o payload | "Atribua `OnGetToken` e confira usuário e senha ali; sem ele o servidor não emite token." | atualizacao#10--11 (linha 4) |
| `1.1/jwt-claims-utc` | `exp`/`iat`/`nbf` saíam em hora local tratada como UTC (token emitido em UTC-3 expirava 3 h antes). Agora saem em UTC correto; tokens de outras bibliotecas passam a ser lidos certo | "Tokens JWT emitidos pela 1.0 podem ter expiração errada: peça novo login depois de atualizar." | não está (commit `eb1f542`) |
| `1.1/jwt-cookie-raltoken` | Com `UseCookie`, o servidor só lê o cookie chamado exatamente `raltoken` (antes pegava o primeiro cookie do cabeçalho, qualquer nome). Idem em Sagui/Indy/fpHTTP, que antes nem chegavam a esse código | "Se você mandava o token em outro cookie, renomeie para `raltoken`." | não está (commit `e5d1932`) |
| `1.1/compress-servidor-vence` | `CompressType` ≠ `ctNone` no servidor é decisão fixa: o cliente não consegue recusar. Só com `ctNone` o servidor segue o `Accept-Encoding` do cliente. O cliente passou a mandar `Accept-Encoding` sempre (antes saía vazio) | "Servidor com `CompressType` fixo comprime sempre. Quer que o cliente decida? Deixe `ctNone`." (na 2.0 vira o plugin de compressão, ver `2.0/plugin-compress`) | não está (commit `a9bfb89`) |
| `1.1/deflate-bruto-delphi` | `ctDeflate` no Delphi passou a ser deflate bruto (como no FPC); antes enquadrava como gzip com cabeçalho "deflate". Interoperabilidade 1.0 Delphi ↔ outros só quebrava com `ctDeflate` explícito | "Se você usa `ctDeflate`, atualize cliente e servidor Delphi juntos." | não está (commit `52eaabc`) |
| `1.1/excecao-handler-500` | Exceção em rota sem `OnServerError` nem `RaiseError` agora responde **500 com a mensagem**; antes respondia 200 vazio | "Se algum cliente seu tratava 200 vazio como 'deu certo', ele agora recebe 500. Trate a exceção na rota ou use `OnServerError`." | não está (commit `b34cb57`) |
| `1.1/verbo-fora-do-permitido-405` | Método fora de `AllowedMethods` respondia **403** e contava como tentativa de força bruta (3 verbos errados bloqueavam o IP); agora responde **405** sem contar | "Testes que esperavam 403 para verbo errado: esperam 405. O IP deixou de ser bloqueado por isso." | wiki diz 405 só na 1.2 e "era 404" (errado, ver Divergências); commit `67f83a2` |
| `1.1/forca-bruta-maxtry` | Antes, **uma** senha errada bloqueava o IP até `ExpirationTime`, ignorando `MaxTry`; agora bloqueia a partir de `MaxTry` falhas. Lista de flood parou de crescer sem fim | "O bloqueio por força bruta agora respeita `MaxTry` (padrão 3). Se confiava no bloqueio na 1ª falha, ponha `MaxTry = 1`." | não está (commit `6f3288e`) |
| `1.1/uploads-nome-arquivo` | `TRALParam.SaveToFile(pasta, nome)` guarda só o último componente do nome (`..\x`/`C:\x` não escapam mais da pasta) | "Se montava subpastas pelo nome do arquivo enviado, passe o caminho completo você mesmo." | não está (commit `b34cb57`) |
| `1.1/limites-padrao-zero` | Novos tetos (`MaxRequestSize`, descompressão máxima) vêm **desligados** (0 = como antes) | nada a fazer | não precisa de item (informativo) |
| `1.1/multipart-cliente-sem-compressao` | Pedido multipart do cliente sai sempre sem compressão (servidor com multipart nativo, ex. Sagui, perdia o corpo) | nada a fazer; fio mudou só nesse caso | não está (commit `febb3ad`/`133ee5d`) |
| `1.1/charral-delphi-xe3-101` | Em Delphi XE3 a 10.0 `CharRAL` passou a `Char` (UTF-16); antes era `UTF8Char` | "Código seu que declara `CharRAL`/`PCharRAL` nessas versões do Delphi precisa de revisão." | não está (`RALTypes.pas` 1.1) |
| `1.1/db-modulo` | `TRALDBModule.GetFields` recusa nome de tabela que não seja identificador; `OnValidateSQL` novo (desligado se não atribuído); pool de conexões desligado por padrão | nada a fazer; quem passava nome de tabela com espaço/ponto-e-vírgula precisa de identificador válido | não está |

## 1.1 / Divergências

1. **`ctAES*`/`TCriptoType` (única regra de código provada pelo gabarito) não está em NENHUMA página da wiki.** A tabela 1.0 → 1.1 tem 4 linhas e nenhuma é essa. Quem usa `TRALHashes.Encrypt(.., ctAES128)` na 1.0 não compila na 1.1 (`E2003`/`undeclared identifier`). Sugestão: acrescentar à wiki.
2. A wiki põe o **405** e o `ebSingleThread` na tabela 1.1 → 1.2 e diz "405 (era 404)". No código: o 405 por verbo fora de `AllowedMethods` já está na **1.1** (e na 1.0 era **403** contando força bruta). O que o `dev` mudou depois (`c7ed026`) é outro caso: rota que existe mas não aceita o verbo, de 404 para 405 com `Allow`. A linha da 1.2 está certa para esse caso; falta a da 1.1.
3. `37a6638` tem 3 mudanças que não são migração: `ExplicitTop` removido do .dfm, `OldCreateOrder` removido, `udm.pas` com SQL do Firebird no lugar de `TabelaExiste` e `FireDAC.Phys.SQLiteWrapper.Stat` no `uses`. **Não gerar regra** a partir delas.
4. O gabarito 1.0 → 1.1 **não toca em caminho de biblioteca, pacote, nem JWT**. As regras `1.1/caminho-plugins-modules` e `1.1/jwt-eventos-of-object` só têm a wiki e o código como fonte. (O `server_bridge` já usava `jwtGetToken`/`jwtValidate` como métodos do formulário, por isso nada precisou mudar.)
5. A wiki não diz que **dados cifrados e guardados** com a 1.0 quebram (só cliente/servidor).
6. A wiki não diz nada sobre `deflate`, `excecao-handler-500`, `jwt-claims-utc`, `jwt-cookie-raltoken`, `compress-servidor-vence`, `forca-bruta-maxtry`: todos de `v1.0..1.1`, 154 commits, vários "Fix" que mudam resposta observável.

---------------------------------------------------------------------
# PASSO 1.1 > dev (1.2)
---------------------------------------------------------------------

Diferença de pacotes/units 1.1 → dev: **nenhuma renomeação, nenhuma unit sumida**. Só adições (pacotes `KwikRAL`, `MsQuicRAL`, `OkHttpRAL`; units `RALNetwork`, `RALQuicFrame`, `MsQuic`, `RALMsQuic*`, `RALKwik*`, `RALOkHttp*`). Não há regra de pacote neste passo.

## dev / Regras

| id | ação | de → para (exato) | chave de diff provável | wiki |
|---|---|---|---|---|
| `1.2/sagui-maxconnections` | `propriedade-renomear` | `TRALSaguiServer.ConnectionLimit` → `MaxConnections` em `.dfm/.lfm` (linha `ConnectionLimit = N`) e em código (`X.ConnectionLimit :=`). O formulário antigo **ainda abre** (`DefineProperties`) e o código antigo ainda compila (propriedade pública, não publicada); a troca é cosmética | `published-sumiu:TRALSaguiServer.ConnectionLimit` (tipo `Int64RAL` → `IntegerRAL`) | atualizacao#11--12- (linha Sagui) |
| `1.2/cors-materializar-asterisco` | `valor-padrao-materializar` | em `.dfm/.lfm` de servidor **sem** a linha `CORSOptions.AllowOrigin` (inclusive servidor sem nenhuma linha `CORSOptions.*`): acrescentar `CORSOptions.AllowOrigin = '*'`. Na 1.1 o construtor punha `'*'` e uma string vazia nunca era gravada, então forma sem a linha rodava com CORS aberto; na 1.2 o construtor põe `''` (fechado) | `default-mudou:TRALCORSOptions.AllowOrigin` (`'*'` → `''`) | atualizacao#11--12- (linha CORS: "Formulários salvos com `*` continuam"). A wiki só cobre quem tem a linha; ver Divergências |
| `1.2/cors-codigo-aviso` | `padrao-avisar` | servidor criado em **código** sem `CORSOptions.AllowOrigin := ...` → item manual: "acrescente `CORSOptions.AllowOrigin := '*'` (ou a lista de sites) logo após criar o servidor". Também `TRALCORSOptions.Create` direto | idem | idem |
| `1.2/cliente-chamada-sincrona` | `padrao-avisar` | chamadas `Get/Post/Put/Patch/Delete(.., callback)` sem 3º parâmetro de comportamento, e `TRALFDQuery.QueryBehavior` sem valor, em código chamado de botão/evento de tela: aviso "agora espera a resposta (`ebSingleThread`); passe `ebMultiThread` onde a tela não pode travar". Em formulário: `QueryBehavior` ausente → **materializar `ebMultiThread`** (valor que o projeto usava) | `default-mudou:TRALClient.Get..AExecBehavior` (`ebMultiThread` → `ebSingleThread`); `default-mudou:TRALFDQuery.QueryBehavior` | atualizacao#11--12- (linha 1) |
| `1.2/cliente-conexao-compartilhada` | `valor-padrao-materializar` (opcional) | `ShareConnection` e `PoolConnection.Enabled` passam a `True`. Para "quase ninguém é afetado" a wiki manda não fazer nada; a regra só materializa `False` se o dono quiser exclusividade. Padrão: **não materializar**, só relatar | `default-mudou:TRALClient.ShareConnection`, `default-mudou:TRALClient.PoolConnection.Enabled` | atualizacao#11--12- (linha 2) |
| `1.2/webmodule-docroot` | `valor-padrao-materializar` (condicional) | `TRALWebModule` com `DocumentRoot` vazio servia a pasta do .exe; agora serve nada. Se o `.dfm` não tem `DocumentRoot`: relatar e perguntar; se o projeto servia a pasta do exe, materializar `UseApplicationPathAsRoot = True` (**abre a pasta do .exe: decisão do dono, não automática**) | `default-mudou:TRALWebModule.DocumentRoot`; `published-novo:TRALWebModule.UseApplicationPathAsRoot` | atualizacao#11--12- (linha DocumentRoot); commit `0c22f5f` |
| `1.2/webmodule-domain` | `padrao-avisar` | WebModule com `Domain` ≠ `/` agora responde só abaixo do `Domain` | `comportamento` (sem diff de API) | atualizacao#11--12- |
| `1.2/params-por-origem` | `padrao-avisar` | no código: `GetKind[..]`, `AssignParamsUrl(rpkFIELD)`, `Params.GetKind` em servidores Indy/UniGUI/fpHTTP/CGI → "revise: formulário = `rpkFIELD`, URL = `rpkQUERY`, cabeçalhos = `rpkHEADER`" | `comportamento` (propriedade `Get`/`GetKind` existem nas duas) | atualizacao#11--12- (linha params) |
| `1.2/params-body-obsoleto` | `padrao-avisar` | `Params.Body` e `TRALParam.AsStream` → `Content`, `SaveToStream`, `Count(rpkBODY)` (no FPC é `deprecated`; no Delphi só comentário) | `metodo-obsoleto:TRALParams.Body`, `metodo-obsoleto:TRALParam.AsStream` | atualizacao#11--12- (linha Body) |
| `1.2/memtable-excecoes` | `padrao-avisar` | em código: `ApplyUpdates`/`ExecSQL` de memtable (`TRALDBFDMemTable`, `TRALDBZMemTable`, `TRALDBBufDataset`) fora de `try/except`; `UpdateSQL` que mexe em vários registros → materializar `CountUpdatedRecords = False` no formulário (propriedade nova publicada em Zeos/BufDataset; `UpdateOptions.CountUpdatedRecords` no FireDAC) quando houver `UpdateSQL` com procedimento | `published-novo:TRALDBZMemTable.CountUpdatedRecords`, `published-novo:TRALDBBufDataset.CountUpdatedRecords` | atualizacao#11--12- (linha ApplyUpdates) |
| `1.2/dbware-legacywire` | `padrao-avisar` | com o outro lado em RAL < 1.2: `RALLegacyWire := True` no lado novo (variável global, só em código) | `unidade-nova-simbolo:RALLegacyWire` | atualizacao#11--12- (linha NUMERIC) |
| `1.2/cookie-samesite-zero` | `padrao-avisar` | `TRALCookieSiteScope` ganhou `cssDefault` **na primeira posição** (`cssLax, cssNone, cssStrict` → `cssDefault, cssLax, cssNone, cssStrict`): `TRALCookie` zerado (ex. `FillChar`) deixa de escrever `SameSite=Lax`. Código que compara ordinal: revisar | `enum-valor-novo:TRALCookieSiteScope.cssDefault`; ordinais mudaram | não está (commit `5151833`) |
| `1.2/synopse-mode` | (sem regra) | `TRALSynopseServer.Mode`: padrão final `smThreads`, igual ao salvo | `published-novo:TRALSynopseServer.Mode` | atualizacao#11--12- (última linha); o `d08a4b6` o pôs `smAsync` e o `95f0282` o devolveu: líquido zero |

Quebras de API do dev que o diff mostrará e **não** são de usuário: `ConnectionLimit` (acima); `TRALRequest.Protocol` (foi para `TRALHTTPHeaderInfo`); `amUNKNOWN` acrescentado ao fim de `TRALMethod` (ordinais antigos intactos); `sftBCD` acrescentado ao fim de `TRALFieldType`; `rteCancelled` ao fim de `TRALTransportError`; `Params.Get/GetKind` ganharam `const` no parâmetro.

## dev / Comportamento

| id | o que é | texto para o usuário (pt-BR) | wiki |
|---|---|---|---|
| `1.2/cors-fechado` | `AllowOrigin` começa vazio: nenhuma página de outro site chama o servidor. Também novos: `AllowCredentials` (só com origem nomeada, nunca com `*`) e lista de origens separadas por espaço/vírgula | "O servidor deixou de aceitar chamadas de outros sites pelo navegador. Servidor criado em código: `CORSOptions.AllowOrigin := '*'` ou a lista de sites. Formulário que tinha `*` salvo continua com `*`." | atualizacao#11--12- |
| `1.2/cliente-espera-resposta` | ver regra `1.2/cliente-chamada-sincrona` | "Chamadas com evento esperam a resposta. Onde a tela não pode travar, passe `ebMultiThread`." | atualizacao#11--12- |
| `1.2/webmodule-sem-docroot` | idem regra `1.2/webmodule-docroot`; também: `BlockedExtensions` (vazio), sessões que funcionam (cookie HttpOnly, 30 min), `FollowLinks` | "Seu site deixou de ser servido? Aponte `DocumentRoot` (relativo ao .exe) ou ligue `UseApplicationPathAsRoot`." | atualizacao#11--12- |
| `1.2/verbo-rota-405` | rota existente que não aceita o verbo: 404 → **405 com `Allow`** (`c7ed026`); método que o servidor não implementa: roda o GET → **501** (`248e7fd`) | "Testes que esperavam 404 para verbo errado esperam 405; método desconhecido responde 501." | atualizacao#11--12- cobre só o 405; o 501 não está |
| `1.2/dbware-decimal-milissegundos` | NUMERIC/DECIMAL viajam inteiros (`sftBCD`) e datas com milissegundos. **1.1 e 1.2 não se leem** nessas colunas/datas | "Atualize cliente e servidor DBWare juntos ou use `RALLegacyWire := True` no lado novo." | atualizacao#11--12- |
| `1.2/memtable-exceptions` | `UPDATE/DELETE` que não afeta exatamente 1 registro é erro (`emDBRowsAffected`); toda falha do `ApplyUpdates`/`ExecSQL` da memtable é levantada além de ir ao `OnError` | "Envolva em `try/except`. Para `UpdateSQL` que mexe em vários registros, `CountUpdatedRecords := False`." | atualizacao#11--12- |
| `1.2/dao-validateapplyupdates` | no DAO FireDAC, com `OnValidateSQL` atribuído e `OnValidateApplyUpdates` não, o `ApplyUpdates` é **recusado** | "Quem validava SQL no DAO: atribua também `OnValidateApplyUpdates`." | não está (commit `72a9e46`) |
| `1.2/forca-bruta-conta-segredo` | força bruta passa a contar só segredo checado e recusado (Bearer falho, 403 da aplicação, flood não contam); rota de token limitada; pedido com corpo na rota de token é **login**, mesmo com Bearer/cookie | "Se dependia de qualquer 401 contar como tentativa, revise `MaxTry`. Login com corpo ganha do token do cookie." | não está (commit `a9792ab`) |
| `1.2/flood-primeira-requisicao` | com `rsoFloodProtection`, a 1ª requisição de cada IP era recusada com 403; corrigido | nada a fazer | não está (`7fb6802`) |
| `1.2/multipart-sem-parte-400` | corpo `multipart/form-data` sem nenhuma parte responde 400 antes da rota | "Cliente que mandava multipart vazio sem boundary recebe 400." | não está (`7fb6802`) |
| `1.2/redirect-https` | com `SSL.Required` (ou pino) o 3xx é a resposta e **não é seguido** para http | "Redirecionamento https → http não é mais seguido quando TLS é exigido." | não está (`72a9e46`) |
| `1.2/params-por-origem` | ver regra | "`ParamByName` continua igual; revise leitura por tipo." | atualizacao#11--12- |
| `1.2/seguranca-opt-in` | `HideErrorDetails`, `SecurityHeaders`, `SessionTimeout`/`FollowLinks` do WebModule: todos **desligados** por padrão | nada a fazer | não precisa de item |
| `1.2/sagui-maxconnections` | ver regra | nada | atualizacao#11--12- |

## dev / Divergências

1. **CORS sem a linha `AllowOrigin` no formulário.** A wiki e o commit `f6452f9` dizem "um formulário mantém o `*` com que foi salvo, porque o streaming sempre o gravou". Não vale para formulário que **não tem a linha**: no Samples, o `Delphi/VCL/Basic Server/Sagui/Principal.dfm` tem `CORSOptions.AllowHeaders`/`MaxAge` e nenhum `AllowOrigin`. Na 1.1 ele rodava com `*` (construtor); na 1.2 fecha. O gabarito 2.0 **não criou o plugin CORS nesse servidor** (só nos 17 que tinham `AllowOrigin`), ou seja, o próprio porte fechou o CORS sem dizer. Pergunta ao dono: materializar `'*'` (preserva o comportamento) ou fechar? Recomendo materializar e avisar.
2. A tabela 1.1 → 1.2 da wiki diz que `ShareConnection`/`PoolConnection` ligados são "quase ninguém"; ok. Mas **`TRALFDQuery.QueryBehavior`** também mudou de padrão e só o texto do `95f0282` diz; a wiki só fala "chamadas do cliente".
3. A wiki não cita: `501` para método desconhecido, `400` para multipart vazio, `OnValidateApplyUpdates`, força bruta/rota de token, `cssDefault`, redirecionamento em TLS exigido.
4. A linha "`ConnectionLimit` → `MaxConnections` / nada: o nome antigo ainda compila" está certa para código e para `.dfm` (`DefineProperties`), mas **o diff mostrará `published-sumiu`**; a regra deve classificar como "compatível, renomear por higiene".

---------------------------------------------------------------------
# PASSO dev > 2.0
---------------------------------------------------------------------

Contexto: `dev` → `2.0` mistura (a) a reforma do `1.3-plugin` (plugins, módulos, auth) e (b) a reforma de pastas/pacotes (commits `breaking:` `e484a4e`, `7e3db14`, `5fa096b`). O 2.0 não tem os ~100 commits do `dev` posteriores a `52f3fd2` que a wiki ainda lista como 1.2; um diff `dev` × `2.0` pode apontar "sumiu" para coisas que só estão atrasadas no 2.0.

## 2.0 / Regras: plugins (propriedade do servidor → componente)

Todas agem em `.dfm/.lfm` do servidor (qualquer `TRAL*Server`) + `.pas` do formulário (campo na classe + unit no `uses`) ou, para servidor criado em código, `X := TRAL...Plugin.Create(Self); X.Server := Servidor`. Nome do componente: `RAL<Plugin>1`, `2`... (`RALCompressPlugin1`, `RALCORSPlugin1`, `RALCriptoPlugin1`, como no gabarito). Posição visual `Left/Top` no gabarito: na coluna do servidor, passo de 56 px (`Top = 64, 120, 176...`), sem importância.

| id | ação | de → para | chave de diff | wiki |
|---|---|---|---|---|
| `2.0/plugin-compress` | `propriedade-para-componente` | `<Servidor>.CompressType = <v>` (qualquer valor, **inclusive `ctNone`** e inclusive linha ausente) → `object RALCompressPlugin1: TRALCompressPlugin`, `Server = <servidor>`, `CompressType = <v>` (linha omitida se `ctNone`, que é o padrão do plugin). Unit `RALContent`. Servidor sem a linha: criar o plugin com padrão mesmo assim (na 1.x todo servidor comprimia conforme o cliente; sem o plugin deixa de comprimir) | `published-sumiu:TRAL*Server.CompressType` (ou `TRALServer.CompressType`) | plugins#o-que-mudou-da-12-para-a-20 (1ª linha); atualizacao#12--20- |
| `2.0/plugin-cripto` | `propriedade-para-componente` | `CriptoOptions.Key = '<k>'` → `object RALCriptoPlugin1: TRALCriptoPlugin`, `Key = '<k>'`. Criar **só** se `CriptoOptions.CriptType <> crNone` **ou** `Key` não vazio. `CriptoOptions.CriptType` é descartada (o tamanho da cifra vem do `Accept-Encription` do cliente). Gabarito: mORMot2 com `crAES256` + `Key = 'oiuy'` gerou o plugin com a chave; os Indy/Sagui com `crNone` e sem chave não geraram nada. Unit `RALContent` | `published-sumiu:TRAL*Server.CriptoOptions` | plugins (linha `CriptoOptions.Key`) |
| `2.0/plugin-limits` | `propriedade-para-componente` | `MaxRequestSize = N` (N ≠ 0) → `TRALLimitsPlugin` com `MaxRequestSize = N`. `RALContent` | `published-sumiu:TRALServer.MaxRequestSize` | plugins |
| `2.0/plugin-jsonbody` | `propriedade-para-componente` | `JSONBodyToParams = True` → `TRALJSONBodyPlugin` (sem propriedades). `RALContent` | `published-sumiu:TRALServer.JSONBodyToParams` | plugins |
| `2.0/plugin-cors` | `propriedade-para-componente` | `CORSOptions.AllowOrigin` → `RALCORSPlugin1.Options.AllowOrigin`; `CORSOptions.AllowHeaders.Strings` → `Options.AllowHeaders.Strings`; `CORSOptions.MaxAge` → `Options.MaxAge`; `CORSOptions.AllowCredentials` → `Options.AllowCredentials`. Criar **só** se `AllowOrigin` não vazio (no passo anterior materializado). Unit `RALCORS`. Gabarito: 17 plugins, todos com `AllowOrigin = '*'` | `published-sumiu:TRAL*Server.CORSOptions` (e `published-sumiu:TRALCORSOptions.*`) | plugins; atualizacao#12--20- |
| `2.0/plugin-bruteforce` | `propriedade-para-componente` | `Security.Options` contém `rsoBruteForceProtection` → `TRALBruteForcePlugin` (`RALSecurity`), `MaxTry = Security.BruteForce.MaxTry`, `ExpirationTime = Security.BruteForce.ExpirationTime` (ms; padrão 3 e 1 800 000). Sem a opção: descartar `Security.BruteForce.*` | `published-sumiu:TRALSecurity.BruteForce`, `published-sumiu:TRALBruteForceProtection.MaxTry/ExpirationTime` | plugins |
| `2.0/plugin-flood` | `propriedade-para-componente` | `rsoFloodProtection` → `TRALFloodPlugin`, `Interval = Security.FloodTimeInterval` (ms, padrão 30). `RALSecurity` | `published-sumiu:TRALSecurity.FloodTimeInterval`, `...Options` | plugins |
| `2.0/plugin-pathtraversal` | `propriedade-para-componente` | `rsoPathTransvBlackList` → `TRALPathTraversalPlugin` (sem propriedades) | `published-sumiu:TRALSecurity.Options` | plugins |
| `2.0/plugin-whitelist` | `propriedade-para-componente` | `Security.WhiteIPList` (não vazia) → `TRALWhiteListPlugin`, `IPList = <as mesmas linhas>` | `published-sumiu:TRALSecurity.WhiteIPList` | plugins |
| `2.0/plugin-blacklist` | `propriedade-para-componente` | `Security.BlackIPList` (não vazia) → `TRALBlackListPlugin`, `IPList = <as mesmas linhas>` | `published-sumiu:TRALSecurity.BlackIPList` | plugins |
| `2.0/plugin-securityheaders` | `propriedade-para-componente` | `SecurityHeaders = [..]` (não vazio) → `TRALSecurityHeadersPlugin`, `Headers = [..]` (mesmos `rsh*`; `TRALSecurityHeader` continua em `RALTypes`) | `published-sumiu:TRALServer.SecurityHeaders` | plugins |
| `2.0/seguranca-descartar-vazio` | `propriedade-remover` | `Security.Options = []`, `Security.BruteForce.*`, `Security.FloodTimeInterval` sem a opção correspondente ligada, `CriptoOptions.CriptType = crNone` sem chave, `MaxRequestSize = 0`, `JSONBodyToParams = False`, `SecurityHeaders = []`: apenas apagar as linhas (é o que o gabarito fez em todo servidor) | todas as `published-sumiu:TRALServer.*` e `TRALSecurity.*` acima, quando o valor é o padrão | atualizacao#12--20- (aviso do `.dfm` que não abre) |
| `2.0/oauth1-removido` | `propriedade-remover` + `padrao-avisar` | `object X: TRALClientOAuth` / `TRALServerOAuth` no formulário e referências no código → item **manual**: "OAuth 1.0a foi removido (nunca funcionou); use `TRALClientOAuth2`/`TRALServerOAuth2` (unit `RALOAuth2`)". Idem tipos `TRALOAuth`, `TRALOAuthAlgorithm`, `TRALOnGetTokenSecret` (eventos `OnGetTokenSecret`) | `classe-sumiu:TRALClientOAuth`, `classe-sumiu:TRALServerOAuth`, `classe-sumiu:TRALOAuth`, `classe-sumiu:TRALOAuthAlgorithm` (RALToken), `classe-sumiu:TRALOnGetTokenSecret` | atualizacao#12--20- (linha OAuth) |

`HideErrorDetails` fica no servidor. `Authentication`, `CookieLife`, `IPConfig`, `Port`, `Routes`, `ResponsePages`, `ServerStatus`, `ShowServerStatus`, `SessionTimeout`, `RaiseError` continuam (conferido nas duas listas de `published` do `TRALServer`).

Detalhe de aplicação mostrado pelo gabarito (45 exemplos, 71 arquivos): 18 `RALCompressPlugin`, 17 `RALCORSPlugin`, 7 `RALCriptoPlugin`; `.pas`: campo `RALCompressPlugin1: TRALCompressPlugin;` etc. logo depois do campo do servidor e `RALCORS, RALContent` no fim do `uses`; servidores criados em código (Http2/QuicBenchmark `UServidor.pas`) ganharam campos `FCompress: TRALCompressPlugin`/`FCripto: TRALCriptoPlugin` criados com `.Server := FServer` e as atribuições `FServer.CompressType := ..` viraram `FCompress.CompressType := ..`.

## 2.0 / Regras: pacotes Delphi (`pacote-renomear`: `.dproj` `DCC_UsePackage`, `.dpk` `requires`)

Conferido em `677864d`: tokens trocados nos `.dproj` (contagem de ocorrências): `RALDBFireDACLink` 86, `RALDBZeosLink` 86, `RALZStdCompress` 79, `RALBrotliCompress` 79, `RALDBFireDACObjects` 54, `RALWizard` 55, `RALBSONStorage` 40. Nenhuma outra linha dos `.dproj` mudou (nem caminho de busca).

| id | de → para | observação | chave de diff | wiki |
|---|---|---|---|---|
| `2.0/pacote-dbfiredac` | `RALDBFireDACLink` → `RALDBFireDACPackage` | design em `RALDBFireDACDsgn` (só IDE; não pôr no projeto) | `pacote-sumiu:delphi/RALDBFireDACLink` | migrar-para-2.0#pacotes-renomeados |
| `2.0/pacote-dbfiredacdao` | `RALDBFireDACObjects` → `RALDBFireDACDAOPackage` | contém `RALDBFiredacDAO` | `pacote-sumiu:delphi/RALDBFireDACObjects` | idem |
| `2.0/pacote-dbzeos` | `RALDBZeosLink` → `RALDBZeosPackage` | design em `RALDBZeosDsgn` | `pacote-sumiu:delphi/RALDBZeosLink` | idem |
| `2.0/pacote-bson` | `RALBSONStorage` → `RALStorageBSONPackage` | contém `kxBSON`, `RALStorageBSON`; exige `RALStoragePackage` | `pacote-sumiu:delphi/RALBSONStorage` | idem |
| `2.0/pacote-zstd` | `RALZStdCompress` → `RALCompressZStdPackage` | | `pacote-sumiu:delphi/RALZStdCompress` | idem |
| `2.0/pacote-brotli` | `RALBrotliCompress` → `RALCompressBrotliPackage` | | `pacote-sumiu:delphi/RALBrotliCompress` | idem |
| `2.0/pacote-wizard` | `RALWizard` → `RALWizardDsgn` | só IDE | `pacote-sumiu:delphi/RALWizard` | idem |
| `2.0/pacote-db` | `RALDBPackage` fica; acrescenta-se `RALDBDsgn` (só IDE) | `RALDBPackage` agora exige `RALStoragePackage` | `pacote-novo:delphi/RALDBDsgn` | idem |
| `2.0/pacote-design-runtime` | em `.dpk`/`.dproj` de **runtime**: `PascalRALDsgn` → `PascalRAL` (ou apenas remover se `PascalRAL` já consta) | em dev os pacotes exigiam `PascalRALDsgn`; na 2.0 ele é `{$DESIGNONLY}` e exige `designide` (compilador não avisa; falha em runtime). Pacote que registra componentes na IDE mantém. **Não é troca cega**: o gabarito deixou `PascalRALDsgn` nos `DCC_UsePackage` dos samples | `pacote-mudou-exige:delphi/PascalRALDsgn` | migrar-para-2.0#seus-pacotes-e-projetos |

Inalterados em Delphi (mesmo nome): `PascalRAL`, `PascalRALDsgn`, `RALDBPackage`, `IndyRAL`, `NetHttpRAL`, `SaguiRAL`, `SynopseRAL`, `UniGUIRAL`, `KwikRAL`, `MsQuicRAL`, `OkHttpRAL`.

`pacote-exigir-se-unit` para Delphi (**não feito pelo gabarito**, necessário só em projeto com pacotes de runtime ligados): usa `RALAuthentication|RALDigest|RALOAuth2` → `RALPluginAuthPackage`; `RALSelfSigned` → `RALPluginTLSPackage`; `RALStorageBIN|RALStorageJSON|RALStorageCSV|RALStorage|RALStorageTypes|RALDBTypes` → `RALStoragePackage`; `RALWebModule` → `RALModuleWebPackage`; `RALSwaggerModule|RALSwaggerExporter|RALPostmanExporter` → `RALModuleSwaggerPackage`. Chave de diff: `unit-mudou-pacote:RALAuthentication` etc.

## 2.0 / Regras: pacotes Lazarus (`pacote-renomear`: `.lpi`/`.lpk` `RequiredPackages`; `.lpr` unit do pacote no `uses`)

Gabarito: `raldbzeoslink` → `raldbzeospackage` em 6 `.lpi` + 4 `.lpr` (`uses`); `raldbsqldblink` → `raldbsqldbpackage` em 1 `.lpi` + 1 `.lpr`. Pacotes **acrescentados**: `ralpluginauthpackage` (5 `.lpi`), `ralstoragepackage` (5), `ralmoduleswaggerpackage` (4), `ralmodulewebpackage` (1) e `pascalral` (1, DBPool, que não o exigia explicitamente).

| id | de → para | observação | chave de diff | wiki |
|---|---|---|---|---|
| `2.0/pacote-lcl-dbzeos` | `raldbzeoslink` → `raldbzeospackage` | no `.lpr`, a unit com o mesmo nome do pacote também troca; design em `raldbzeosdsgn` (só IDE) | `pacote-sumiu:lazarus/raldbzeoslink` | migrar-para-2.0 |
| `2.0/pacote-lcl-dbsqldb` | `raldbsqldblink` → `raldbsqldbpackage` | design em `raldbsqldbdsgn` | `pacote-sumiu:lazarus/raldbsqldblink` | idem |
| `2.0/pacote-lcl-bson` | `raldbbson` → `ralstoragebsonpackage` | exige `ralstoragepackage` | `pacote-sumiu:lazarus/raldbbson` | idem |
| `2.0/pacote-lcl-zstd` | `ralzstdcompress` → `ralcompresszstdpackage` | | `pacote-sumiu:lazarus/ralzstdcompress` | idem |
| `2.0/pacote-lcl-brotli` | `ralbrotlicompress` → `ralcompressbrotlipackage` | | `pacote-sumiu:lazarus/ralbrotlicompress` | idem |
| `2.0/pacote-lcl-wizard` | `ralwizardpack` → `ralwizarddsgn` | só IDE | `pacote-sumiu:lazarus/ralwizardpack` | idem |
| `2.0/pacote-lcl-db` | `raldbpackage` fica; novo `raldbdsgn` (só IDE); `raldbpackage` agora exige `ralstoragepackage` | | `pacote-novo:lazarus/raldbdsgn` | idem |
| `2.0/pacote-lcl-auth` | usa `RALAuthentication`, `RALDigest` ou `RALOAuth2` → exigir `ralpluginauthpackage` | `pacote-exigir-se-unit` | `unit-mudou-pacote:RALAuthentication` (idem Digest, OAuth2) | migrar-para-2.0#seus-pacotes-e-projetos; gabarito |
| `2.0/pacote-lcl-tls` | usa `RALSelfSigned` → `ralplugintlspackage` | | `unit-mudou-pacote:RALSelfSigned` | migrar-para-2.0#o-que-saiu-do-núcleo |
| `2.0/pacote-lcl-storage` | usa `RALStorageBIN`, `RALStorageJSON`, `RALStorageCSV`, `RALStorage`, `RALStorageTypes`, `RALDBTypes` → `ralstoragepackage` | em dev `RALDBTypes` ficava no núcleo | `unit-mudou-pacote:RALStorageBIN`, `...RALStorage`, `...RALDBTypes` | idem; gabarito |
| `2.0/pacote-lcl-web` | usa `RALWebModule` → `ralmodulewebpackage` | | `unit-mudou-pacote:RALWebModule` | idem; gabarito |
| `2.0/pacote-lcl-swagger` | usa `RALSwaggerModule`/`RALSwaggerExporter`/`RALPostmanExporter` → `ralmoduleswaggerpackage` | exige `ralpluginauthpackage` | `unit-mudou-pacote:RALSwaggerModule` | idem; gabarito |
| `2.0/pacote-lcl-core` | usa units do `pascalral` e não o exige → acrescentar `pascalral` | gabarito DBPool | — | gabarito (sem wiki) |

Inalterados em Lazarus: `pascalral`, `pascalraldsgn` (nome interno `PascalRALDsgn`, só caixa), `cgiral`, `fphttpral`, `indyral`, `msquicral`, `saguiral` (`SaguiRAL`), `synopseral`.

## 2.0 / Regras: units (`unit-renomear`, `uses` de `.pas/.dpr/.lpr`) e classes que mudam de unit

Unit de registro: ninguém usa no código do usuário, **a não ser** no `uses` de `.lpr` gerado pelo assistente do Lazarus (`... , raldbzeoslink, ...` é o pacote, não a unit). Renomear só se aparecer.

| id | de → para | chave de diff |
|---|---|---|
| `2.0/unit-dbtypes` | `RALDBTypes` → `RALStorageTypes` (opcional: `RALDBTypes` continua, `deprecated`, com apelidos de tipo/constante) | `unit-sumiu:RALDBTypes` não aparece (a unit existe como apelido); `classe-mudou-unit:TRALFieldType`, `TRALDB`, `TRALDBInfo*`, `TRALDBUpdateSQL` |
| `2.0/unit-reg-motores` | `RALIndyRegister`→`RALIndyReg`; `RALSynopseRegister`→`RALSynopseReg`; `RALSaguiRegister`→`RALSaguiReg`; `RALNetHTTPRegister`→`RALNetHTTPReg`; `RALUniGUIRegister`→`RALUniGUIReg`; `RALfpHTTPRegister`→`RALfpHTTPReg`; `RALCGIRegister`→`RALCGIReg`; `RALMsQuicRegister`→`RALMsQuicReg`; `RALKwikRegister`→`RALKwikReg`; `RALOkHttpRegister`→`RALOkHttpReg` | `unit-sumiu:RAL*Register` |
| `2.0/unit-reg-db` | `raldbregister`→`RALDBReg`; `RALDBFireDACLinkReg`→`RALDBFireDACReg`; `RALDBZeosLinkReg`→`RALDBZeosReg`; `RALDBSQLDBLinkReg`→`RALDBSQLDBReg` | `unit-sumiu:raldbregister`, `unit-sumiu:RALDB*LinkReg` |
| `2.0/unit-reg-bson` | `RALBSONReg`→`RALStorageBSONReg` | `unit-sumiu:RALBSONReg` |
| `2.0/classe-digest` | `TRALServerDigest`, `TRALClientDigest`: unit `RALAuthentication` → `RALDigest`. **Sem apelido**: quem declara o campo no `.pas` precisa de `RALDigest` no `uses` (`uses-adicionar`) | `classe-mudou-unit:TRALServerDigest`, `classe-mudou-unit:TRALClientDigest` |
| `2.0/classe-oauth2` | `TRALServerOAuth2`, `TRALClientOAuth2`: `RALAuthentication` → `RALOAuth2` (idem) | `classe-mudou-unit:TRALServerOAuth2`, `classe-mudou-unit:TRALClientOAuth2` |
| `2.0/classe-authbase` | `TRALAuthentication`, `TRALAuthClient`, `TRALAuthServer`, `TRALAuthTransport`, `TRALOnBeforeGetToken`: declaradas em `RALAuthBase` mas **reexportadas por apelido em `RALAuthentication`**. Não quebra | `classe-mudou-unit:TRALAuthClient` etc.: justificar "apelido" |
| `2.0/classe-cors` | `TRALCORSOptions`, `TRALTextVersion`: `RALServer` → `RALCORS` (sem apelido; só código que a usava direto) | `classe-mudou-unit:TRALCORSOptions` |
| `2.0/classe-security` | `TRALClientList`, `TRALClientBlockList`: `RALServer` → `RALSecurity`; `TRALSecurity`, `TRALBruteForceProtection`: **sumiram** (sem equivalente de tipo; ver plugins) | `classe-mudou-unit:TRALClientList`; `classe-sumiu:TRALSecurity`; `classe-sumiu:TRALBruteForceProtection` |
| `2.0/classe-authattempt` | `TRALAuthAttempt`: `RALAuthentication` → `RALPlugin` | `classe-mudou-unit:TRALAuthAttempt` |
| `2.0/classe-auth-server-base` | `TRALAuthServer` agora descende de `TRALPlugin` (era `TRALAuthentication`): `TRALServerBasicAuth/JWT/Digest/OAuth2` são plugins; código que fazia cast para `TRALAuthentication` no servidor revisar | `ancestral-mudou:TRALAuthServer` |
| `2.0/classe-dbdesign` | `TRALDBBufDatasetEditor/Tables`, `TRALDBFDMemTableEditor/Tables`, `TRALDBZMemTableEditor/Tables`, `TRALDBDatabases`, `TRALDBModuleSelectionEditor`: só IDE (units de registro). Sem regra | `classe-mudou-unit:TRALDB*Editor` etc.: justificar "design" |

Units com o mesmo nome que só mudaram de pasta (dev → 2.0), para `caminho-trocar` e para o diff ver `unit-pasta-mudou`:
`RALTypes RALConsts RALCustomObjects RALParams RALRequest RALResponse RALRoutes RALServer RALClient` → `core`; `RALTools RALStream RALThreadSafe RALJson RALMIMETypes RALUrlCoder RALMultipartCoder RALNetwork RALBase64 RALHexadecimal RALResponsePages RALExternalsLibraries RALQuicFrame` → `common`; `RALCompress RALCompressZLib` → `compress`; `RALCompressZStd` → `compress\zstd`; `RALCompressBrotli` → `compress\brotli`; `RALHashBase RALHashes RALMD5 RALSHA1 RALSHA2_32 RALSHA2_64 RALCRC32 RALCripto RALCriptoAES RALCriptoOpenSSL RALOpenSSL RALToken` → `crypto`; `RALAuthentication` → `plugins\auth`; `RALStorage RALStorageBIN RALStorageJSON RALStorageCSV` → `storage`; `RALStorageBSON` → `storage\bson`; `RALWebModule` → `modules\web`; `RALSwaggerModule RALSwaggerExporter RALPostmanExporter` → `modules\swagger`; `MsQuic libsagui` → `bindings`; `RALRegister` → `design`.

## 2.0 / Regras: caminhos (`caminho-trocar`: `DCC_UnitSearchPath` do `.dproj`, `OtherUnitFiles` do `.lpi`)

Substituição (tabela da wiki, conferida no movimento de units acima; o gabarito **não** mexeu em caminho, os samples dependem do library path da IDE):

| id | 1.x | 2.0 |
|---|---|---|
| `2.0/caminho-base` | `...\base` | `...\core;...\storage` (+ `...\common` se usar utilitários) |
| `2.0/caminho-base-plugins` | `...\base\plugins` | `...\plugins`; `...\plugins\auth` se usa `RALAuthentication/RALDigest/RALOAuth2`; `...\plugins\tls` se usa `RALSelfSigned` |
| `2.0/caminho-base-modules` | `...\base\modules` | `...\common` (`RALExternalsLibraries`); `...\modules\web` se `RALWebModule`; `...\modules\swagger` se Swagger/Postman |
| `2.0/caminho-utils` | `...\utils` | `...\common;...\compress;...\crypto;...\storage` (os que o `uses` pedir) |
| `2.0/caminho-others` | `...\others` | `...\others` (só submódulos) + `...\bindings`, `...\compress\zstd`, `...\compress\brotli`, `...\storage\bson` conforme o uso; MsQuic e Sagui também `...\bindings` |
| `2.0/caminho-inalterados` | `database`, `database\FireDAC`, `database\Zeos`, `database\sqldb`, `engine\<motor>` | iguais |

Chave de diff: `unit-pasta-mudou:*`; o diff de API não vê pasta, então a regra deve ser semeada pela tabela.

## 2.0 / Comportamento

| id | o que é | texto para o usuário (pt-BR) | wiki |
|---|---|---|---|
| `2.0/sem-plugin-sem-recurso` | sem plugin, o servidor não comprime, não cifra, não manda CORS, não limita pedido, não protege contra força bruta/flood/travessia, não manda cabeçalhos de segurança, e só responde rotas | "Depois da migração, confira os plugins ligados a cada servidor (paleta **RAL - Plugins**). O migrador criou os que correspondiam às propriedades que você tinha." | plugins (aviso do alto) |
| `2.0/dfm-1x-nao-abre` | formulário com as propriedades antigas **não abre** (`Property CompressType does not exist`) | "O migrador já tirou as linhas do `.dfm/.lfm`. Se abrir um formulário ainda não migrado: *View as Text*, apague as linhas, ou **Ignore** em cada propriedade." | atualizacao#12--20-; plugins#erros-comuns |
| `2.0/compress-ignora-comprimidos` | imagem, áudio, vídeo, zip, gzip, 7z, rar, pdf e fontes não são mais comprimidos (`SkipCompressedTypes = True`) | "Quer comprimir tudo como antes? `SkipCompressedTypes := False` no plugin de compressão." O migrador **não** materializa isso (gabarito também não) | atualizacao#12--20- |
| `2.0/corpo-somente-leitura` | `Body.Content` pode não ser `TMemoryStream` (> 8 MB, em disco); corpo recebido é somente leitura | "Código com `TMemoryStream(Param.Content).Memory` deve ler como `TStream`; para escrever use `AsStream` (uma cópia)." (`padrao-avisar` em código: o padrão `TMemoryStream(<x>.Content)` e `Content :=`/`.Memory`) | atualizacao#12--20-; corpo-unico |
| `2.0/responsetext-servidor` | `ResponseText` no `OnResponse` é o texto respondido, não o gzip | nada a fazer na maioria dos casos | atualizacao#12--20- |
| `2.0/precisa-de-motor` | sem ao menos um motor (Indy, mORMot2, Sagui, fpHTTP, netHTTP, ...) o RAL não funciona; o instalador recusa | "Instale um motor." | atualizacao#12--20-; migrar-para-2.0#erros-comuns |
| `2.0/classe-nao-encontrada` | `Class TRALServerJWTAuth not found` ao abrir o formulário: faltou instalar `RALPluginAuthPackage` (idem storage, Web e Swagger) | "Instale o pacote novo que guarda essa classe." | migrar-para-2.0#erros-comuns |
| `2.0/f2613` | `F2613 Unit 'RALServer' not found` com os fontes: library path da 1.x | "Troque pelas pastas da 2.0." | migrar-para-2.0#as-pastas-library-path |
| `2.0/cors-multi-origem` | `AllowOrigin` aceita lista (espaço/vírgula) e `AllowCredentials` não funciona com `*` | informativo | plugins |
| `2.0/sagui-e-cgi-params` | herdado da 1.2: ver `1.2/params-por-origem` | | |

## 2.0 / Divergências

1. **A wiki lista pacotes que nunca existiram na 1.0, 1.1 nem no dev:** `RALXMLPackage`/`ralxmlpackage` → `RALModuleXMLPackage`, `RALSOAPPackage`/`ralsoappackage` → `RALModuleSOAPPackage`, e `TRALSelfSigned` → `TRALSelfSignedPlugin` (esta última aparece em atualizacao.md). `ls-tree` de `v1.0`, `1.1` e `dev` não tem XML, SOAP nem SelfSigned; eles são do `1.3-plugin`. **Para `v1.0`/`1.1`/`dev` essas três linhas não geram regra** (nada a migrar). Só importam a quem usou o branch `1.3-plugin`; se o dono quiser cobri-los, são regras de um passo à parte (`1.3-plugin>2.0`).
2. **Pacote de Lazarus de design da 1.x contido no `...Link`:** a wiki mostra uma linha `RALDBZeosLink | raldbzeoslink | RALDBZeosPackage + RALDBZeosDsgn`. Para **projeto** só entra o `Package`; o `Dsgn` é instalação na IDE. A wiki diz isso para `RALDBPackage`, mas não explica nas outras linhas; a regra não deve acrescentar `...Dsgn` a `.dproj/.lpi`.
3. **`RALDBFireDACObjects` e `RALDBFireDACLink`:** a wiki põe `RALDBFireDACObjects → RALDBFireDACDAOPackage` mas não diz que os dois são pacotes independentes (um tem o DAO, outro tem a memtable); quem tinha só um mantém só o seu.
4. **O gabarito não fez** (e a wiki manda em prosa): `PascalRALDsgn` → `PascalRAL` nos pacotes de runtime (os `DCC_UsePackage` dos samples ainda têm `PascalRALDsgn`); `RALPluginAuthPackage`/`RALStoragePackage`/`RALModule*Package` no `DCC_UsePackage` do Delphi (compila com library path; com `UsePackages=true` faltariam); `AllowCredentials`; plugins de segurança (nenhum sample usava). Além disso o gabarito **fechou o CORS do `Basic Server/Sagui`** sem comentar (divergência 1 do passo 1.1 > dev).
5. **O gabarito fez e a wiki não diz:** (a) `TRALCriptoPlugin` só leva `Key`; `CriptoOptions.CriptType` some sem equivalente; (b) todo servidor ganha `TRALCompressPlugin` mesmo com `CompressType = ctNone` ("segue o Accept-Encoding do cliente como o servidor da 1.x fazia"); (c) em `.lpi`, `pascalral` entra como exigência quando o projeto não a tinha (DBPool); (d) no `.lpr` o `uses` troca o nome do pacote (`raldbzeoslink` → `raldbzeospackage`) além do `.lpi`; (e) `.dfm` quebrado antes do passo (Lazarus `mORMot2_Zeos/uprincipal.lfm` tinha objeto sem `end`): o migrador deve **recusar com erro claro** formulário que não analisa, não "consertar".
6. **Wiki × código, `Security.FloodTimeInterval`:** a wiki do plugin diz "`Interval` (ms entre pedidos)"; o código dev e 2.0 usam milissegundos nos dois (padrão 30). Sem divergência real, só confirma que o valor passa direto.
7. **Aliases que a wiki chama de "units não mudaram de nome":** é verdade para as de runtime, **com as exceções** `RALDigest` e `RALOAuth2` (units novas que passaram a guardar `TRALServerDigest/ClientDigest` e `TRALServerOAuth2/ClientOAuth2`, antes dentro de `RALAuthentication`) e `RALCORS`/`RALSecurity`/`RALContent`/`RALConcurrency`/`RALPlugin` (plugins). Quem declara um desses tipos num `.pas` precisa do `uses` novo. A wiki não menciona.
8. **Fonte de verdade do 2.0:** `pkg\Delphi\Wizard`, `pkg\Delphi\Compression` etc. já são as subpastas; os `.dpk` do dev ficavam em `pkg\Delphi\Database`, `compression\brotli`, `compression\zstd`. Quem referencia o `.dpk` por caminho (grupo de projetos próprio) precisa de `caminho-trocar` também para `.dpk` (fora do vocabulário atual, que só cita `.dproj/.lpi`).

---------------------------------------------------------------------
# Contagens
---------------------------------------------------------------------

| passo | regras | comportamento | divergências |
|---|---|---|---|
| v1.0 > 1.1 | 7 regras com id (+ 6 "sem regra" justificadas) | 14 | 6 |
| 1.1 > dev (1.2) | 13 regras com id (1 sem regra) | 14 | 4 |
| dev > 2.0 | 13 de plugin/descarte + 9 pacotes Delphi (+ 1 só-exigir) + 13 pacotes Lazarus + 12 units/classes + 6 caminhos = 53 | 9 | 8 |

Lacunas de vocabulário a decidir: `uses-adicionar` (nova); `caminho-trocar` também para `.dpk`; ação para "objeto do formulário sem equivalente" (`TRALClientOAuth`/`TRALServerOAuth`), hoje só `propriedade-remover`/`padrao-avisar`.
