# Inventario das quebras do RAL

Gerado por `apidiff inventario` a partir de `migrador/api/api-*.json` e `migrador/regras/cobertura.json`. Nao edite a mao: mude a cobertura e gere de novo.

## v1.0 -> 1.1 (fechado)

20 diferencas: 20 cobertas, **0 pendentes**, 0 itens de cobertura sem correspondencia.

### Cobertas (20)

| chave | detalhe | regra ou justificativa |
| --- | --- | --- |
| `assinatura-mudou:TRALHashes.Decrypt` | class function(StringRAL; StringRAL; TCriptoType): StringRAL \| class function(StringRAL; StringRAL; TRALCriptoType): StringRAL \| class function(TStream; StringRAL; TCriptoType): TStream \| class function(TStream; StringRAL; TRALCriptoType): TStream -> class function(StringRAL; StringRAL; TRALCriptoType): StringRAL \| class function(TStream; StringRAL; TRALCriptoType): TStream | regra `1.1/criptotype-tipo` |
| `assinatura-mudou:TRALHashes.Encrypt` | class function(StringRAL; StringRAL; TCriptoType): StringRAL \| class function(StringRAL; StringRAL; TRALCriptoType): StringRAL \| class function(TStream; StringRAL; TCriptoType): TStream \| class function(TStream; StringRAL; TRALCriptoType): TStream -> class function(StringRAL; StringRAL; TRALCriptoType): StringRAL \| class function(TStream; StringRAL; TRALCriptoType): TStream | regra `1.1/criptotype-tipo` |
| `assinatura-mudou:TRALParams.EncodeBody` | function(var StringRAL; var StringRAL): TStream -> function(var StringRAL; var StringRAL; Boolean): TStream | parametro novo ACompressMultipart com valor padrao (True): a chamada antiga compila igual |
| `assinatura-mudou:TRALStringListSafe.AddObject` | procedure(const StringRAL; TObject) -> function(const StringRAL; TObject): Boolean | procedure virou function (Boolean): a chamada antiga compila igual |
| `classe-sumiu:TRALThreadCriptoAES` | unit RALCriptoAES | thread interna do RALCriptoAES, nunca usada fora do RAL |
| `default-mudou:TRALClient.ConnectTimeout` | default 5000 -> 30000; stored (nenhum) -> (nenhum) | o construtor ja usava 30000; so a diretiva default estava errada (o valor sempre foi gravado no formulario). Nem a forma salva nem o comportamento mudam |
| `default-mudou:TRALClient.RequestTimeout` | default 30000 -> 10000; stored (nenhum) -> (nenhum) | o construtor ja usava 10000; so a diretiva default estava errada. Nem a forma salva nem o comportamento mudam |
| `publico-sumiu:TRALMIMEType.ReleaseInstance` | metodo class procedure() | singleton interno, chamado so pelo finalization da propria unit |
| `published-sumiu:TRALDBBase.DriverType` | agora public, nao published | propriedade so de leitura: nunca foi gravada em formulario, e continua public |
| `published-sumiu:TRALDBFireDAC.DriverType` | agora public, nao published | propriedade so de leitura: nunca foi gravada em formulario, e continua public |
| `published-sumiu:TRALDBSQL.DriverType` | agora public, nao published | propriedade so de leitura: nunca foi gravada em formulario, e continua public |
| `published-sumiu:TRALDBSQLDB.DriverType` | agora public, nao published | propriedade so de leitura: nunca foi gravada em formulario, e continua public |
| `published-sumiu:TRALDBZeos.DriverType` | agora public, nao published | propriedade so de leitura: nunca foi gravada em formulario, e continua public |
| `tipo-sumiu:TCriptoType` | enum da unit RALHashes | regra `1.1/criptotype-tipo` |
| `unit-pasta-mudou:RALAuthentication` | src/base -> src/base/plugins | regra `1.1/caminho-plugins-modules` |
| `unit-pasta-mudou:RALExternalsLibraries` | src/utils -> src/base/modules | regra `1.1/caminho-plugins-modules` |
| `unit-pasta-mudou:RALPostmanExporter` | src/utils -> src/base/modules | regra `1.1/caminho-plugins-modules` |
| `unit-pasta-mudou:RALSwaggerExporter` | src/utils -> src/base/modules | regra `1.1/caminho-plugins-modules` |
| `unit-pasta-mudou:RALSwaggerModule` | src/base -> src/base/modules | regra `1.1/caminho-plugins-modules` |
| `unit-pasta-mudou:RALWebModule` | src/base -> src/base/modules | regra `1.1/caminho-plugins-modules` |

## 1.1 -> dev (aberto)

12 diferencas: 12 cobertas, **0 pendentes**, 0 itens de cobertura sem correspondencia.

### Cobertas (12)

| chave | detalhe | regra ou justificativa |
| --- | --- | --- |
| `ancestral-mudou:TRALClientSelectionEditor` | TSelectionEditor -> TRALSelectionEditor | editor de selecao da IDE (design-time), nao aparece em projeto de usuario |
| `ancestral-mudou:TRALServerSelectionEditor` | TSelectionEditor -> TRALSelectionEditor | editor de selecao da IDE (design-time), nao aparece em projeto de usuario |
| `ancestral-mudou:TRALWebSession` | (nenhum) -> TRALThreadSafe | ganhou ancestral (TRALThreadSafe) sem perder membro: o codigo antigo compila igual |
| `assinatura-mudou:TRALJSON.ParseJSON` | class function(const StringRAL): TRALJSONValue \| class function(StringRAL): TRALJSONValue \| class function(TStream): TRALJSONValue -> class function(const StringRAL): TRALJSONValue \| class function(TStream): TRALJSONValue | sumiu so a sobrecarga sem const; a com const StringRAL aceita as mesmas chamadas |
| `assinatura-mudou:TRALJSONObject.Count` | function(): Integer \| function(): IntegerRAL -> function(): Integer | IntegerRAL e apelido de Integer: a diferenca e so de grafia entre as passadas Delphi e FPC |
| `assinatura-mudou:TRALJSONObject.Get` | function(const StringRAL): TRALJSONValue \| function(IntegerRAL): TRALJSONValue \| function(StringRAL): TRALJSONValue -> function(const StringRAL): TRALJSONValue \| function(IntegerRAL): TRALJSONValue | sumiu so a sobrecarga sem const; a com const StringRAL aceita as mesmas chamadas |
| `default-mudou:TRALClientRequest.Protocol` | default (nenhum) -> (nenhum); stored (nenhum) -> False | continua published; so deixou de ser gravada (stored False). Formulario antigo com a linha abre igual |
| `default-mudou:TRALFDQuery.QueryBehavior` | default ebMultiThread -> ebSingleThread; stored (nenhum) -> (nenhum) | regra `1.2/fdquery-behavior-materializar, 1.2/cliente-chamada-sincrona` |
| `default-mudou:TRALRequest.Protocol` | default (nenhum) -> (nenhum); stored (nenhum) -> False | continua published; so deixou de ser gravada (stored False). Formulario antigo com a linha abre igual |
| `default-mudou:TRALServerRequest.Protocol` | default (nenhum) -> (nenhum); stored (nenhum) -> False | continua published; so deixou de ser gravada (stored False). Formulario antigo com a linha abre igual |
| `publico-sumiu:TRALCriptoAES.Destroy` | metodo destructor() | o destrutor herdado continua; Free e Destroy compilam igual |
| `published-sumiu:TRALSaguiServer.ConnectionLimit` | agora public, nao published | regra `1.2/sagui-maxconnections` |

## dev -> 2.0 (aberto)

213 diferencas: 213 cobertas, **0 pendentes**, 0 itens de cobertura sem correspondencia.

### Cobertas (213)

| chave | detalhe | regra ou justificativa |
| --- | --- | --- |
| `ancestral-mudou:TRALAuthServer` | TRALAuthentication -> TRALPlugin | regra `2.0/classe-auth-server-base` |
| `ancestral-mudou:TRALServer` | TRALComponent -> TRALPluginHost | ganhou um ancestral intermediario (TRALPluginHost = class(TRALComponent)): continua descendente de TRALComponent |
| `ancestral-mudou:TRALStringStream` | TMemoryStream -> TRALMemoryStream | TRALMemoryStream = class(TMemoryStream): continua descendente de TMemoryStream |
| `assinatura-mudou:TRALAuthServer.AttemptOf` | function(TRALRequest; TRALResponse; Boolean): TRALAuthAttempt -> function(TRALRequest; TRALResponse; TRALAuthResult; Boolean): TRALAuthAttempt | regra `2.0/auth-attemptof` |
| `assinatura-mudou:TRALDigest.Load` | procedure(const StringRAL) -> function(const StringRAL): Boolean | procedure virou function (Boolean): a chamada antiga compila igual |
| `assinatura-mudou:TRALServerJWTAuth.AttemptOf` | function(TRALRequest; TRALResponse; Boolean): TRALAuthAttempt -> function(TRALRequest; TRALResponse; TRALAuthResult; Boolean): TRALAuthAttempt | regra `2.0/auth-attemptof` |
| `classe-mudou-unit:TRALAuthClient` | RALAuthentication -> RALAuthBase | apelido em RALAuthentication (TRALAuthClient = RALAuthBase.TRALAuthClient): o uses antigo continua valendo |
| `classe-mudou-unit:TRALAuthentication` | RALAuthentication -> RALAuthBase | apelido em RALAuthentication: o uses antigo continua valendo |
| `classe-mudou-unit:TRALAuthServer` | RALAuthentication -> RALAuthBase | apelido em RALAuthentication: o uses antigo continua valendo (a troca de ancestral tem regra propria) |
| `classe-mudou-unit:TRALClientBlockList` | RALServer -> RALSecurity | regra `2.0/classe-security` |
| `classe-mudou-unit:TRALClientDigest` | RALAuthentication -> RALDigest | regra `2.0/classe-digest` |
| `classe-mudou-unit:TRALClientList` | RALServer -> RALSecurity | regra `2.0/classe-security` |
| `classe-mudou-unit:TRALClientOAuth2` | RALAuthentication -> RALOAuth2 | regra `2.0/classe-oauth2` |
| `classe-mudou-unit:TRALCORSOptions` | RALServer -> RALCORS | regra `2.0/classe-cors` |
| `classe-mudou-unit:TRALDB` | RALDBTypes -> RALStorageTypes | regra `2.0/unit-dbtypes` |
| `classe-mudou-unit:TRALDBBufDatasetEditor` | RALDBSQLDBLinkReg -> RALDBSQLDBReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALDBBufDatasetTables` | RALDBSQLDBLinkReg -> RALDBSQLDBReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALDBDatabases` | RALDBRegister -> RALDBReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALDBFDMemTableEditor` | RALDBFireDACLinkReg -> RALDBFireDACReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALDBFDMemTableTables` | RALDBFireDACLinkReg -> RALDBFireDACReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALDBInfoField` | RALDBTypes -> RALStorageTypes | regra `2.0/unit-dbtypes` |
| `classe-mudou-unit:TRALDBInfoFields` | RALDBTypes -> RALStorageTypes | regra `2.0/unit-dbtypes` |
| `classe-mudou-unit:TRALDBInfoTable` | RALDBTypes -> RALStorageTypes | regra `2.0/unit-dbtypes` |
| `classe-mudou-unit:TRALDBInfoTables` | RALDBTypes -> RALStorageTypes | regra `2.0/unit-dbtypes` |
| `classe-mudou-unit:TRALDBModuleSelectionEditor` | RALDBRegister -> RALDBReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALDBUpdateSQL` | RALDBTypes -> RALStorageTypes | regra `2.0/unit-dbtypes` |
| `classe-mudou-unit:TRALDBZMemTableEditor` | RALDBZeosLinkReg -> RALDBZeosReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALDBZMemTableTables` | RALDBZeosLinkReg -> RALDBZeosReg | editor da IDE (design-time), mora na unit de registro |
| `classe-mudou-unit:TRALServerDigest` | RALAuthentication -> RALDigest | regra `2.0/classe-digest` |
| `classe-mudou-unit:TRALServerOAuth2` | RALAuthentication -> RALOAuth2 | regra `2.0/classe-oauth2` |
| `classe-sumiu:TRALBruteForceProtection` | unit RALServer | regra `2.0/classe-security-sumiu` |
| `classe-sumiu:TRALClientOAuth` | unit RALAuthentication | regra `2.0/oauth1-removido` |
| `classe-sumiu:TRALfpHttpConnectionThread` | unit RALfpHTTPServer | thread interna do motor fpHTTP |
| `classe-sumiu:TRALOAuth` | unit RALToken | regra `2.0/oauth1-removido` |
| `classe-sumiu:TRALSecurity` | unit RALServer | regra `2.0/classe-security-sumiu` |
| `classe-sumiu:TRALServerOAuth` | unit RALAuthentication | regra `2.0/oauth1-removido` |
| `enum-valor-sumiu:TRALAuthTypes.ratOAuth` | valor removido | regra `2.0/oauth1-removido` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALAuthentication` | agora em RALPluginAuthPackage | regra `2.0/pacote-exigir-auth` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALDBTypes` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALPostmanExporter` | agora em RALModuleSwaggerPackage | regra `2.0/pacote-exigir-swagger` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALStorage` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALStorageBIN` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALStorageCSV` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALStorageJSON` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALSwaggerExporter` | agora em RALModuleSwaggerPackage | regra `2.0/pacote-exigir-swagger` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALSwaggerModule` | agora em RALModuleSwaggerPackage | regra `2.0/pacote-exigir-swagger` |
| `pacote-perdeu-unit:delphi/PascalRAL/RALWebModule` | agora em RALModuleWebPackage | regra `2.0/pacote-exigir-web` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALAuthentication` | agora em RALPluginAuthPackage | regra `2.0/pacote-exigir-auth` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALDBTypes` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALPostmanExporter` | agora em RALModuleSwaggerPackage | regra `2.0/pacote-exigir-swagger` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALStorage` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALStorageBIN` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALStorageCSV` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALStorageJSON` | agora em RALStoragePackage | regra `2.0/pacote-exigir-storage` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALSwaggerExporter` | agora em RALModuleSwaggerPackage | regra `2.0/pacote-exigir-swagger` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALSwaggerModule` | agora em RALModuleSwaggerPackage | regra `2.0/pacote-exigir-swagger` |
| `pacote-perdeu-unit:lazarus/PascalRAL/RALWebModule` | agora em RALModuleWebPackage | regra `2.0/pacote-exigir-web` |
| `pacote-sumiu:delphi/RALBrotliCompress` | unidades agora em: RALCompressBrotliPackage | regra `2.0/pacote-brotli` |
| `pacote-sumiu:delphi/RALBSONStorage` | unidades agora em: RALStorageBSONPackage | regra `2.0/pacote-bson` |
| `pacote-sumiu:delphi/RALDBFireDACLink` | unidades agora em: RALDBFireDACPackage | regra `2.0/pacote-dbfiredac` |
| `pacote-sumiu:delphi/RALDBFireDACObjects` | unidades agora em: RALDBFireDACDAOPackage | regra `2.0/pacote-dbfiredacdao` |
| `pacote-sumiu:delphi/RALDBZeosLink` | unidades agora em: RALDBZeosPackage | regra `2.0/pacote-dbzeos` |
| `pacote-sumiu:delphi/RALWizard` | unidades agora em: RALWizardDsgn | regra `2.0/pacote-wizard` |
| `pacote-sumiu:delphi/RALZStdCompress` | unidades agora em: RALCompressZStdPackage | regra `2.0/pacote-zstd` |
| `pacote-sumiu:lazarus/ralbrotlicompress` | unidades agora em: RALCompressBrotliPackage | regra `2.0/pacote-brotli` |
| `pacote-sumiu:lazarus/raldbbson` | unidades agora em: RALStorageBSONPackage | regra `2.0/pacote-bson` |
| `pacote-sumiu:lazarus/raldbsqldblink` | unidades agora em: raldbsqldbpackage | regra `2.0/pacote-dbsqldb` |
| `pacote-sumiu:lazarus/raldbzeoslink` | unidades agora em: RALDBZeosPackage | regra `2.0/pacote-dbzeos` |
| `pacote-sumiu:lazarus/ralwizardpack` | unidades agora em: RALWizardDsgn | regra `2.0/pacote-wizard` |
| `pacote-sumiu:lazarus/ralzstdcompress` | unidades agora em: RALCompressZStdPackage | regra `2.0/pacote-zstd` |
| `publico-sumiu:TRALDigest.Header` | prop TStringList | TRALDigest e o leitor interno do cabecalho Digest, usado so pelo plugin |
| `published-sumiu:TRALCGIServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALCGIServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALCGIServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALCGIServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALCGIServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALCGIServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALCGIServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-sumiu:TRALfpHttpServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALfpHttpServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALfpHttpServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALfpHttpServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALfpHttpServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALfpHttpServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALfpHttpServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-sumiu:TRALIndyServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALIndyServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALIndyServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALIndyServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALIndyServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALIndyServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALIndyServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-sumiu:TRALMsQuicServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALMsQuicServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALMsQuicServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALMsQuicServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALMsQuicServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALMsQuicServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALMsQuicServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-sumiu:TRALSaguiServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALSaguiServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALSaguiServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALSaguiServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALSaguiServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALSaguiServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALSaguiServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-sumiu:TRALServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-sumiu:TRALSynopseServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALSynopseServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALSynopseServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALSynopseServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALSynopseServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALSynopseServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALSynopseServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-sumiu:TRALUniGUIServer.CompressType` | tipo TRALCompressType | regra `2.0/plugin-compress` |
| `published-sumiu:TRALUniGUIServer.CORSOptions` | tipo TRALCORSOptions | regra `2.0/plugin-cors` |
| `published-sumiu:TRALUniGUIServer.CriptoOptions` | tipo TRALCriptoOptions | regra `2.0/plugin-cripto` |
| `published-sumiu:TRALUniGUIServer.JSONBodyToParams` | tipo Boolean | regra `2.0/plugin-jsonbody` |
| `published-sumiu:TRALUniGUIServer.MaxRequestSize` | tipo Int64RAL | regra `2.0/plugin-limits` |
| `published-sumiu:TRALUniGUIServer.Security` | tipo TRALSecurity | regra `2.0/plugin-bruteforce, 2.0/plugin-flood, 2.0/plugin-pathtraversal, 2.0/plugin-whitelist, 2.0/plugin-blacklist, 2.0/seguranca-descartar-vazio` |
| `published-sumiu:TRALUniGUIServer.SecurityHeaders` | tipo TRALSecurityHeaders | regra `2.0/plugin-securityheaders` |
| `published-tipo:TRALDigestParams.Stale` | StringRAL -> Boolean | regra `2.0/digest-stale` |
| `rotina-sumiu:RALBSONReg.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALCGIRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALDBFireDACLinkReg.register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALDBRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALDBSQLDBLinkReg.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALDBZeosLinkReg.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALfpHTTPRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALIndyRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALKwikRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALMsQuicRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALNetHTTPRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALOkHttpRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALSaguiRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALSynopseRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `rotina-sumiu:RALUniGUIRegister.Register` | mesmo nome em 26 outras units | Register so e chamado pela IDE ao carregar o pacote; a unit inteira ja tem regra (unit-reg-*) |
| `tipo-sumiu:TRALOAuthAlgorithm` | enum da unit RALToken | regra `2.0/oauth1-removido` |
| `tipo-sumiu:TRALOnGetTokenSecret` | proc da unit RALAuthentication | regra `2.0/oauth1-removido` |
| `unit-pasta-mudou:libsagui` | src/others -> src/bindings | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:MsQuic` | src/engine/msquic -> src/bindings | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALAuthentication` | src/base/plugins -> src/plugins/auth | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALBase64` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALClient` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCompress` | src/utils -> src/compress | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCompressBrotli` | src/others -> src/compress/brotli | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCompressZLib` | src/utils -> src/compress | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCompressZStd` | src/others -> src/compress/zstd | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALConsts` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCRC32` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCripto` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCriptoAES` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCriptoOpenSSL` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALCustomObjects` | src/utils -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALDBTypes` | src/base -> src/storage | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALExternalsLibraries` | src/base/modules -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALHashBase` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALHashes` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALHexadecimal` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALJson` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALMD5` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALMIMETypes` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALMultipartCoder` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALNetwork` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALOpenSSL` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALParams` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALPostmanExporter` | src/base/modules -> src/modules/swagger | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALQuicFrame` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALRegister` | src/base -> src/design | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALRequest` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALResponse` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALResponsePages` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALRoutes` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALServer` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALSHA1` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALSHA2_32` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALSHA2_64` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALStorage` | src/utils -> src/storage | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALStorageBIN` | src/utils -> src/storage | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALStorageBSON` | src/utils -> src/storage/bson | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALStorageCSV` | src/utils -> src/storage | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALStorageJSON` | src/utils -> src/storage | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALStream` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALSwaggerExporter` | src/base/modules -> src/modules/swagger | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALSwaggerModule` | src/base/modules -> src/modules/swagger | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALThreadSafe` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALToken` | src/utils -> src/crypto | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALTools` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALTranslate` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALTypes` | src/base -> src/core | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALUrlCoder` | src/utils -> src/common | regra `2.0/caminho-reforma` |
| `unit-pasta-mudou:RALWebModule` | src/base/modules -> src/modules/web | regra `2.0/caminho-reforma` |
| `unit-sumiu:RALBSONReg` | sem equivalente | regra `2.0/unit-reg-bson` |
| `unit-sumiu:RALCGIRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALDBFireDACLinkReg` | classes agora em RALDBFireDACReg (por exemplo TRALDBFDMemTableEditor) | regra `2.0/unit-reg-db` |
| `unit-sumiu:RALDBRegister` | classes agora em RALDBReg (por exemplo TRALDBDatabases) | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALDBSQLDBLinkReg` | classes agora em RALDBSQLDBReg (por exemplo TRALDBBufDatasetEditor) | regra `2.0/unit-reg-db` |
| `unit-sumiu:RALDBZeosLinkReg` | classes agora em RALDBZeosReg (por exemplo TRALDBZMemTableEditor) | regra `2.0/unit-reg-db` |
| `unit-sumiu:RALfpHTTPRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALIndyRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALKwikRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALMsQuicRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALNetHTTPRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALOkHttpRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALSaguiRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALSynopseRegister` | sem equivalente | regra `2.0/unit-reg-motores` |
| `unit-sumiu:RALUniGUIRegister` | sem equivalente | regra `2.0/unit-reg-motores` |

## Quebras de comportamento

Nao aparecem na API; vem da wiki e viram itens do relatorio do migrador.

| passo | id | o que muda | o que fazer | wiki |
| --- | --- | --- | --- | --- |
| v1.0>1.1 | `1.1/aes-cbc-incompativel` | AES virou CBC de verdade (IV aleatorio, PKCS#7, HMAC-SHA256): 1.0 e 1.1 nao se entendem com criptografia ligada | Atualize cliente e servidor juntos. Dado cifrado com a 1.0 e guardado (arquivo, banco) nao abre na 1.1: decifre antes de atualizar e cifre de novo depois. | atualizacao.md#10--11 |
| v1.0>1.1 | `1.1/hashes-string-base64` | TRALHashes.Encrypt(string) devolve base64 e Decrypt(string) espera base64 | Se voce gravava ou comparava o texto cifrado, regrave. As sobrecargas com TStream continuam binarias. |  |
| v1.0>1.1 | `1.1/jwt-eventos-of-object` | OnGetToken e OnValidate do TRALServerJWTAuth passaram a ser of object | Procedimento solto atribuido em codigo: use OnGetTokenGen/OnValidateGen, ou mova o procedimento para o formulario. | atualizacao.md#10--11 |
| v1.0>1.1 | `1.1/jwt-sem-ongettoken` | rota de token do TRALServerJWTAuth sem OnGetToken responde 401 | Atribua OnGetToken e confira usuario e senha ali. | atualizacao.md#10--11 |
| v1.0>1.1 | `1.1/jwt-claims-utc` | exp/iat/nbf passaram a sair em UTC correto | Tokens emitidos pela 1.0 podem ter expiracao errada: peca novo login depois de atualizar. |  |
| v1.0>1.1 | `1.1/jwt-cookie-raltoken` | com UseCookie, o servidor so le o cookie chamado raltoken | Se mandava o token em outro cookie, renomeie para raltoken. |  |
| v1.0>1.1 | `1.1/compress-servidor-vence` | CompressType diferente de ctNone no servidor comprime sempre; so ctNone segue o Accept-Encoding do cliente | Quer que o cliente decida? Deixe ctNone. |  |
| v1.0>1.1 | `1.1/deflate-bruto-delphi` | ctDeflate no Delphi passou a ser deflate bruto, como no FPC | Se usa ctDeflate, atualize cliente e servidor Delphi juntos. |  |
| v1.0>1.1 | `1.1/excecao-handler-500` | excecao em rota sem OnServerError nem RaiseError responde 500 com a mensagem (antes 200 vazio) | Trate a excecao na rota ou use OnServerError. |  |
| v1.0>1.1 | `1.1/verbo-fora-do-permitido-405` | metodo fora de AllowedMethods responde 405 sem contar forca bruta (antes 403, contando) | Testes que esperavam 403 para verbo errado esperam 405. |  |
| v1.0>1.1 | `1.1/forca-bruta-maxtry` | o bloqueio por forca bruta respeita MaxTry (antes bloqueava na primeira falha) | Se dependia do bloqueio na primeira falha, ponha MaxTry = 1. |  |
| v1.0>1.1 | `1.1/uploads-nome-arquivo` | TRALParam.SaveToFile guarda so o ultimo componente do nome do arquivo | Se montava subpastas pelo nome do arquivo enviado, passe o caminho completo voce mesmo. |  |
| v1.0>1.1 | `1.1/charral-delphi-xe3-101` | do Delphi XE3 ao 10.0, CharRAL passou a Char (UTF-16) | Codigo que declara CharRAL/PCharRAL nessas versoes precisa de revisao. |  |
| 1.1>dev | `1.2/cors-fechado` | AllowOrigin comeca vazio: o CORS vem fechado. Formulario sem a linha AllowOrigin rodava aberto na 1.1 | O migrador escreve CORSOptions.AllowOrigin = '*' no formulario que nao tinha a linha (o projeto continua aberto). Servidor criado em codigo: CORSOptions.AllowOrigin := '*' ou a lista de sites. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/cliente-espera-resposta` | chamadas do cliente com evento esperam a resposta (ebSingleThread) | Onde a tela nao pode travar, passe ebMultiThread. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/cliente-conexao-compartilhada` | ShareConnection e PoolConnection.Enabled ligados por padrao | Quase ninguem e afetado; para conexao exclusiva, desligue os dois. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/webmodule-sem-docroot` | TRALWebModule com DocumentRoot vazio deixou de servir a pasta do executavel | Aponte DocumentRoot (relativo ao .exe) ou ligue UseApplicationPathAsRoot. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/webmodule-domain` | WebModule com Domain diferente de / responde so abaixo do Domain | Confira os links do site. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/verbo-rota-405` | rota existente que nao aceita o verbo: 404 virou 405 com Allow; metodo desconhecido responde 501 | Testes que esperavam 404 para verbo errado esperam 405. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/params-por-origem` | parametros separados por origem (rpkFIELD formulario, rpkQUERY URL, rpkHEADER cabecalho) nos motores Indy, UniGUI, fpHTTP e CGI | ParamByName continua igual; revise leitura por tipo. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/params-body-obsoleto` | Params.Body e TRALParam.AsStream obsoletos | Use Content, SaveToStream e Count(rpkBODY). | atualizacao.md#11--12- |
| 1.1>dev | `1.2/dbware-decimal-milissegundos` | NUMERIC/DECIMAL viajam inteiros e datas com milissegundos: 1.1 e 1.2 nao se leem nessas colunas | Atualize cliente e servidor DBWare juntos ou use RALLegacyWire := True no lado novo. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/memtable-excecoes` | UPDATE/DELETE que nao afeta exatamente 1 registro e erro; falha de ApplyUpdates/ExecSQL e levantada | Envolva em try/except; para UpdateSQL que mexe em varios registros, CountUpdatedRecords := False. | atualizacao.md#11--12- |
| 1.1>dev | `1.2/dao-validateapplyupdates` | no DAO FireDAC com OnValidateSQL e sem OnValidateApplyUpdates, o ApplyUpdates e recusado | Atribua tambem OnValidateApplyUpdates. |  |
| 1.1>dev | `1.2/forca-bruta-conta-segredo` | forca bruta conta so segredo checado e recusado; pedido com corpo na rota de token e login | Se dependia de qualquer 401 contar como tentativa, revise MaxTry. |  |
| 1.1>dev | `1.2/multipart-sem-parte-400` | multipart/form-data sem nenhuma parte responde 400 | Cliente que mandava multipart vazio recebe 400. |  |
| 1.1>dev | `1.2/redirect-https` | com SSL.Required (ou pino) o redirecionamento https -> http nao e seguido | Nada, se o servidor nao redireciona para http. |  |
| 1.1>dev | `1.2/cookie-samesite-zero` | TRALCookieSiteScope ganhou cssDefault na primeira posicao: os ordinais mudaram | Codigo que compara ordinal ou zera TRALCookie com FillChar: revise. |  |
| dev>2.0 | `2.0/sem-plugin-sem-recurso` | sem plugin, o servidor nao comprime, nao cifra, nao manda CORS, nao limita pedido, nao protege e nao manda cabecalhos de seguranca | Confira os plugins ligados a cada servidor (paleta RAL - Plugins). O migrador cria os que correspondiam as propriedades que voce tinha. | plugins.md |
| dev>2.0 | `2.0/compress-ignora-comprimidos` | imagem, audio, video, zip, pdf e fontes nao sao mais comprimidos (SkipCompressedTypes = True) | Quer comprimir tudo como antes? SkipCompressedTypes := False no plugin de compressao. | atualizacao.md#12--20- |
| dev>2.0 | `2.0/corpo-somente-leitura` | Body.Content pode nao ser TMemoryStream (acima de 8 MB vai para disco); corpo recebido e somente leitura | Leia TMemoryStream(Param.Content).Memory como TStream; para escrever use AsStream (uma copia). | atualizacao.md#12--20- |
| dev>2.0 | `2.0/responsetext-servidor` | ResponseText no OnResponse e o texto respondido, nao o gzip | Nada na maioria dos casos. | atualizacao.md#12--20- |
| dev>2.0 | `2.0/classe-nao-encontrada` | auth, storage, Web e Swagger sairam do nucleo: 'Class ... not found' ao abrir o formulario sem o pacote novo | Instale o pacote novo que guarda a classe (o instalador ja faz). | migrar-para-2.0.md#erros-comuns |
| dev>2.0 | `2.0/cors-multi-origem` | AllowOrigin aceita lista (espaco ou virgula) e AllowCredentials nao funciona com * | Informativo. | plugins.md |

