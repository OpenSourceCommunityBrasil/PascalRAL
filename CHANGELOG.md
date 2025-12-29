# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **fix:  - ajuste de identação de algumas units.  - Ajuste em MIMETypes para usar a função interna do RAL com todos os tipos definidos no IANA pra corrigir erro no tráfego de arquivos web em alguns sistemas operacionais que não preenchiam corretamente todos os MIMETypes.** (2025-12-29 – Mobius One)
  feat:
  - Criação de função StreamToByteString para evitar if no StreamToString, otimizando o processo.
  chore:
  - Atualização de submodulo pascal_brotli.

- **- implementado classe de conversao para hexadecimal - correção do output de criptografias para string (formato binario)** (2025-12-14 – Fernando Banhos)

- **- implementado controle de input e output nos formato base64 para criptografia - faltando implmentacao input e output no formato hex para criptografia** (2025-12-12 – Fernando Castelano Banhos)

- **- Correção de Bug na Cripto AES (Nativa do RAL) - Implementação de Cripto usando OpenSSL** (2025-12-12 – Fernando Castelano Banhos)

- **feat:  - Capacidade de fazer For .. in no TRALRoutes  - Possibilidade de criar rotas em runtime apenas com métodos não pertencentes a uma classe com o OnReplyGen  - Possibilidade de atribuir métodos diferentes por verbo nas rotas  - Ajuste no Swagger para que ele consiga tratar corretamente o novo modelo de rotas, sem prejudicar o funcionamento de sistemas já existentes** (2025-12-08 – Mobius One)

- **feat:  - Capacidade de executar laços for..in com TRALParams** (2025-11-26 – Mobius One)

- **fix:  - Corrigido AV não mostrando no cliente #117  - Correção preventiva de erros nos clientes na função tratarExcecao  - Correção preventiva de erro em casos de Param Stream vazio** (2025-11-17 – Mobius One)
  feat:
  - Atualização de arquivo de fontes de identidade visual
  Fixed #117


### Changed
- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-12-14 – Fernando Banhos)

- **- correção de typo (erro na digitacao da classe)** (2025-12-14 – Fernando Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-12-12 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-12-12 – Fernando Castelano Banhos)

- **- correção de saida da cripto para string** (2025-12-12 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-12-12 – Fernando Castelano Banhos)

- **- correção de merge do git** (2025-12-12 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-12-08 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-11-29 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-11-26 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-11-23 – Mobius One)


### Fixed
- **Fix:  - Revert de arquivos incorretos no commit anterior.** (2025-12-29 – Mobius One)

- **fix:  - Correção em RALStream para corrigir erro em compressão** (2025-12-14 – Mobius One)

- **Refactor issue labeler workflow** (2025-11-29 – Mobius One)

- **Refactor issue labeler workflow** (2025-11-29 – Mobius One)

- **fix:  - Correção de compilação para Delphi 10.2 Tokyo, testes pendentes** (2025-11-26 – Mobius One)

- **fix:  - Correção de memoryleak na conversão de StreamToString. #118  - Remoção de redundância do fix anterior.** (2025-11-23 – Mobius One)
  Fixed #118

- **chore: Update Lazarus version options in bug report template EN** (2025-11-14 – Mobius One)

- **Fix:  - README.md banner link** (2025-11-02 – Mobius One)

- **fix: Ajuste de bot de changelog pra branch dev** (2025-10-27 – Mobius One)


### Removed
- **chore: Update dropdown options in bug report template PT** (2025-11-14 – Mobius One)

- **chore: Update dropdown options in bug report template EN** (2025-11-14 – Mobius One)


## [0.12.2] - 2025-10-25

### Security
- **- Alteração de variável de versionamento pra ser uma string ao invés de concatenação de int  - Remoção de warnings sobre variáveis não utilizadas em diversos métodos no pacote base  + Ajuste de pacote PascalRAL incluindo classes faltantes que estavam sendo importadas implicitamente  * Modificação de atribuição de SetAllowedMethods e SetSkipAuthMethods nas rotas  + Adição de funções isMethodAllowed e isMethodSkipped para facilitar a lógica interna  - Modificação de Options na Types para ser um enumerado próprio da nova classe Security do Server  - Correção de compilação no Delphi da RALDBTypes  + Adição de várias funções na lista ThreadSafe para facilitar o uso  - Utilização de CharInSet na RALTools para resolver warning do Delphi  * Modificação severa no RALServer para incluir 3 options que não eram usados antes: FloodProtection, BruteForceProtection e PathTransversal  * Criação de classe Security como propriedade interna do RALServer para conter todas as definições de segurança  - Removida propriedade BruteForce do server e adicionada à Security  + Novas propriedades para adequar ao uso das Options  * Atualização de Versão para 0.9.4** (2024-03-26 – Mobius One)


### Added
- **feat:  - Novo helper .ToJSON para datasets de forma a facilitar converter o dataset inteiro para json.  - Nova função DataSetToJSON para versões do Delphi que não possuem class helpers.** (2025-09-29 – Mobius One)
  Fixed #114

- **feat: implementação de onServerError para centralizar os erros internos num mesmo método** (2025-08-18 – Mobius One)
  Fixed #111

- **fix: Ajuste de identação fix: Ajuste de versionamento fix: Correções de charset entre Lazarus e Delphi fix: Correções de typecast devido a mudança de charset fix: Correções na criptografia pra adequar ao novo charset fix: Correção de comunicação entre Delphi e Lazarus fix: Prevenção de erros diversos fix: Ajuste de detecção de separador para evitar laço sem necessidade fix: Correção de charset no cliente no Delphi unificando código de tratamento entre Delphi e Lazarus fix: Prevenção de erros através de uma constante EmptyStr para versões que não a possuem fix: Mudança no RALDBConnection para usar verbo POST ao invés do GET para enviar SQL no Body evitando problemas com body no GET fix: Otimização de case agrupando tipos comuns fix: Correção nas criptografias para produzirem o mesmo resultado tanto no Delphi quanto no Lazarus feat: Adição de decrypt e encrypt para stream na RALHashes** (2025-08-01 – Mobius One)

- **- implementado parse authentication para server UNIGUI, para correção de bug de erro de autenticação** (2025-04-15 – Fernando Castelano Banhos)

- **- RALRegister.pas -- implementado para Delphi, adicionar automaticamente as unit RALRequest, RALResponse, RALTypes ao adicionar um componente do tipo RALServer** (2025-01-24 – Fernando Castelano Banhos)
  - RALServer.pas
  -- implementado evento OnBeforeAnswer na class TRALModuleRoutes, para "filtros" antes da chamada do rota

- **- retirada do autocreate das ResponsePages do servidor, para nao ficar dando diferença entre versao compilada versao do conteudo - implmentado no SynopseServer o evento OnTerminate para identificar queda no servidor** (2024-11-27 – Fernando Castelano Banhos)

- **- implementacão em todos os clientes para detectar qualquer exceção** (2024-11-11 – Fernando Castelano Banhos)

- **- RALDBFiredacMemTable.pas, RALDBZeosMemTable.pas, RALDBBufDataset.pas -- implementa First ao dar um Open. Fixed #107** (2024-11-04 – Fernando Castelano Banhos)
  - RALDBStorageBSON.pas
  -- Correção para compilar o pacote no lazarus. Fixed #103
  - RALTools.pas
  -- implementado contador de processadores na funcao RALCPUCount, para delphi Linux. Fixed #101

- **- RALParams.pas -- implementação na função AddParam, protecao para o Delphi contra Resultado sujo (Result := nil)** (2024-11-04 – Fernando Castelano Banhos)
  - RALSaguiServer.pas
  -- correção TRALSaguiStringMap.Add, tem retorno PAnsiChar, logo StringRAL deve ser convertido para AnsiString

- **- nova versao 0.9.10-2 alpha - implementações pedidas por Endrigo, issue #104, relativo a criar eventos no DBModule: OnBeforeConnect, OnAfterConnect, OnErrorConnect e OnErrorQuery** (2024-10-30 – Fernando Castelano Banhos)

- **- implementações de melhorias na conversão de String para Bytes e vice-versa - correção RALJSON_lkJSON.inc, TTlkJSON trocado para TlkJSONbase** (2024-10-28 – Fernando Castelano Banhos)

- **- implementações para flexibilizar o uso de storage no dbware** (2024-10-25 – Fernando Castelano Banhos)

- **- implementação de envio sql com params null** (2024-10-25 – Fernando Castelano Banhos)

- **- Cripto AES - implementado melhoria de performance usando threads - corrigido bug do widestring no TRALBinaryWriter - corrigigo pequeno defeito na procedure OnFormBodyData na TRALParams** (2024-10-09 – Fernando Castelano Banhos)

- **- implementacao de params query** (2024-09-10 – Fernando Castelano Banhos)

- **- Implementação de captura todos custom headers - Adaptações de Compress e Encript** (2024-09-10 – Fernando Castelano Banhos)

- **- Implementado Wizard CGi para Lazarus - Melhoria do Server CGI para Delphi** (2024-09-09 – Fernando Castelano Banhos)

- **- RALRegister.pas -- Correção no nome do arquivo resources do lazarus** (2024-09-09 – Fernando Castelano Banhos)
  - RALCGIServer_FPC.inc
  -- implementacão dos EnvironmentVariable como fields params
  -- melhoria no response
  - RALfpHTTPClient.pas
  -- correção na variavel MaxRedirects (byte)

- **+ Códigos HTTP em constantes  * Ajuste de códigos hardcoded nas units do projeto  * Ajuste de proteção contra adição de param vazio em Params.AddFile** (2024-08-14 – Mobius One)

- *** Ajustes de Redirect nos clientes Lazarus  + AddBody e AddHeader na raiz do cliente para facilitar usabilidade  + Métodos Head e Trace que não estavam presentes  * Forçando TLS 1.0, 1.1 e 1.2 no cliente Indy para resolver erro em chamadas https** (2024-08-05 – Mobius One)

- **- implementado translate na unit ralconsts** (2024-07-25 – Fernando Castelano Banhos)

- **- Mudança na classe TRALHTTPHeaderInfo, create com parametro AOwner, para ser usado no TRALResponse, para responder os StatusCode atraves da propriedade ResponsePages do TRALServer - Constantes no RALConsts modificadas, retiradas page error codes e simplificadas com o uso da unit RALResponsePages** (2024-07-25 – Fernando Castelano Banhos)

- **- RALConsts.pas -- Constantes de Pages convertidas para funcão (RALTools.RALHTTPPageCode), usando as resourcestrings** (2024-07-25 – Fernando Castelano Banhos)
  - RALResponse.pas
  -- Função Answer(AStatusCode) nao altera mais o ResponseText
  - RALServer.pas
  -- implementado FPageCodes no TServer, criando um lista de possiveis StatusCode Response
  - RALTools.pas
  -- implementado funcao RALHTTPPageCode, para formatar RALPageCode com alguns StatusCode pré definidos
  - RALPageCodes.pas
  -- Unit Collection para insert Responses de StatusCode personalizadas

- **-- implementado traducoes para resourcesstrings** (2024-07-24 – Fernando Castelano Banhos)

- **- RALDBFireDAC.pas -- implementado funcao GetFieldTable** (2024-07-22 – Fernando Castelano Banhos)
  - RALDBFiredacMemTable.pas, RALDBZeosMemTable.pas, RALDBBufDataset.pas
  -- implementado protecao para identificar o fields incorreto
  -- implementado protecao para preenchido o Size somente para Fields Strings e Precision para fields Double
  - RALDBModule.pas
  -- separado em funcoes os resultados de Open, ExecSQL e GetInfoFields
  - RALDBSQLCache.pas
  -- correção de memory leak
  - RALDBStorageBIN.pas, RALDBStorageBSON.pas, RALDBStorageJSON.pas
  -- implementado protecao para preenchido o Size somente para Fields Strings e Precision para fields Double

- **- RALParams.pas -- implementação para AssignParamsText, com opcao de UrlEncoded, Synopse nao aceita Param Query sem ser encodado** (2024-07-21 – Fernando Castelano Banhos)

- **- RALDBFiredacMemTable.pas -- implementado resync no resetar o CacheUpdate Interno do FireDAC -- implementado no ApplyUpdates o Error de gravacao no Servidor** (2024-07-21 – Fernando Castelano Banhos)
  - RALDBStorageBIN.pas, RALDBStorageBSON.pas, RALDBStorageCSV.pas, RALDBStorageJSON.pas
  -- correção para Delphi - AddFieldDef
  - RALDBZeosMemTable.pas e RALDBBufDataset.pas
  -- implementado no ApplyUpdates o Error de gravacao no Servidor
  - RALStream.pas
  -- correção de leitura maior que o buffer da stream

- **- RALDBFireDACLinkReg.pas -- adicionado RegisterComponentEditor e RegisterPropertyEditor** (2024-07-21 – Fernando Castelano Banhos)
  - RALDBFiredacMemTable.pas, RALDBZeosMemTable.pas, RALDBBufDataset.pas
  -- removida procedure Open e refeito no SetActive
  - RALDBSQLCache.pas
  -- correção de bug de retorno de SQLCache com Qtd Maior
  - RALDBStorageBIN.pas
  -- correção de write flag do field
  -- implementado na leitura do fields as flags readonly e required
  - RALDBStorageBSON.pas, RALDBStorageCSV.pas, RALDBStorageJSON.pas
  -- implementado na leitura do fields as flags readonly e required
  - RALQueryStructure.pas
  -- removida, foi substituida pela RALDBSQLCache.pas

- **- RALDBFireDACLinkReg.pas -- implementacoes de propertyeditor e componenteditor para TRALDBFDMemTable** (2024-07-21 – Fernando Castelano Banhos)
  - RALDBFiredacMemTable.pas
  -- implementacoes do TRALDBFDMemTable baseadas no TRALDBBufDataset

- **- RALDBZeosLinkReg.pas -- implementado propertyeditor e componenteditor para o componente TRALDBZMemTable** (2024-07-21 – Fernando Castelano Banhos)
  - RALDBZeosMemTable.pas
  -- implementado funcionalidades baseadas nas implementacoes do TRALDBBufDataset
  - RALDBBufDataset.pas
  -- propriedade Connection mudada para RALConnection
  - RALDBSQLDBLinkReg.pas
  -- TRALBufDatasetTables trocado para TRALDBBufDatasetTables
  -- Limpeza de Units não utilizadas

- **- RALClient.pas -- correção para setar o parametro AExecBehavior na funcao ExecuteThread** (2024-07-16 – Fernando Castelano Banhos)
  - RALParams.pas
  -- função EncodeBody qdo tem um parametro e o mesmo ter o nome ral_param, passara se chamar ral_body
  afim do mesmo aparece no Body do Request ou Response
  - RALDBBase.pas, RALDBFireDAC.pas, RALDBZeos.pas, RALDBSQLDB.pas
  -- implementado funcao GetFieldTable para localizar a tabela de um SQL
  - RALDBModule.pas
  -- criado rota GetSQLFields, para converter um SQL em InfoFields
  -- limpeza na funcao GetFields
  - RALDBStorageBIN.pas, RALDBStorageBSON.pas, RALDBStorageJSON.pas
  -- Alteração do nome da funcao FieldProviderFlags para GetFieldProviderFlags
  - RALDBTypes.pas
  -- implementado classes TRALDBInfoField e TRALDBInfoFields
  -- Renoemedo funcao FieldProviderFlags da classe TRALDB para GetFieldProviderFlags
  -- implementado funcao SetFieldProviderFlags
  - RALDBSQLDBLinkReg.pas
  -- implementado classe TRALDBBufDatasetEditor para Editar os Fields e busca-los no Servidor

- **- RALParams.pas -- correções de param = nil** (2024-07-15 – Fernando Castelano Banhos)
  - RALDBModule.pas
  -- implementado ApplyUpdates
  -- adaptações referente a modificações na unit RALDBSQLCache.pas
  - RALDBSQLCache.pas
  -- implementado no response a propriedade Error para uso no ApplyUpdates
  -- implementado no response a propriedades RowsAffected, LastId para uso no ExecSQL
  - RALDBTypes.pas
  -- implementado AssignTo para a classe TRALDBUpdateSQL
  - RALDBZeos.pas
  -- corrigido bug quando Params = nil
  - RALDBBufDataset.pas
  -- criado propriedades RowsAffected e LastId
  -- implementado OnApplyUpdates
  -- correção na função CacheSQL, uso FieldByName quando o field nao existe causa raise
  -- correção na função OnQueryResponse, pegando stream incorreta
  - RALStream.pas
  -- pequena correção pra voltar o position do stream na funcao ReadStream

- **- RALStream.pas -- implementacao nova classe TRALBinaryWriter, facilitando ler e gravar binario em stream** (2024-07-14 – Fernando Castelano Banhos)
  - RALDBSQLCache.pas
  -- adaptações das leituras de escritas de stream com o uso da TRALBinaryWriter

- **- RALAuthentication.pas -- Correção de bug no Delphi, no TRALServerJWTAuth a propriedade AuthRoute não estava abrindo o TCollection de InputParams -- Mudança no Validate do TRALServerBasicAuth, adaptando as implementações do TRALAuthorization** (2024-07-12 – Fernando Castelano Banhos)
  - RALRequest.pas
  -- implementado no TRALAuthorization os objetos das autenticações, facilitando o usuario ler o objeto em suas rotas
  - RALToken.pas
  -- implementado objeto TRALAuthBasic para ser usado no TRALAuthorization

- **- RALRoutes.pas -- Correção de bug para funcionar em Delphi** (2024-07-11 – Fernando Castelano Banhos)
  - RALDBZeosMemTable.pas e RALDBBufDataset.pas
  -- Iniciadas as implementações para o ApplyUpdatesRemote

- **- RALRoutes.pas -- implementado rota relativa (params URI), direto na propriedade Route -- propriedade URIParams servira para completar os parametros adicionados na Route** (2024-07-01 – Fernando Castelano Banhos)
  - RALSwaggerExporter.pas e RALPostmanExporter.pas
  -- alterações devido as implementações do RALRoutes

- **- RALSwaggerModule.pas -- implementado propriedade AllowCORSVerbs para ignorar ou nao a rota OPTIONS** (2024-06-27 – Fernando Castelano Banhos)

- **- RALAuthentication.pas -- AuthToken do JWT - implementando Assign** (2024-06-26 – Fernando Castelano Banhos)
  - RALRoutes.pas
  -- TRALRouteParam e TRALBaseRoute implementados AssignTo

- **- Wizard Lazarus ainda em implementacao** (2024-06-25 – Fernando Castelano Banhos)

- **- RALWEBModule.pas -- Alterações para inicio de testes com o exemplo Mobius** (2024-06-24 – Fernando Castelano Banhos)
  - RALWizard
  -- Implementado StandAlone Application
  -- Implementado Console Application
  -- Iniciado Wizard para Lazarus

- **- RALRoutes.pas -- Criado TRALRouteURIParams apenas para setar a propriedade AllowURIParams** (2024-06-24 – Fernando Castelano Banhos)
  -- Implementado Wizard para Delphi, funcionando para testes

- **- RALRoutes.pas -- Renomeado TRALParamRoute para TRALRouteParam -- Renomeado TRALParamsRoute para TRALRouteParams** (2024-06-22 – Fernando Castelano Banhos)
  - RALCompress.pas, RALCompressBrotli.pas, RALCompressZLib.pas, RALCompressZStd.pas
  -- Renomeado função CheckDependence para CheckDependency
  - RALPostmanExporter.pas
  -- Implementado Params de URI e Params de Input
  - RALSwaggerExporter.pas
  -- Implementado Params de URI e Params de Input

- **- RALRoutes.pas -- Implementado params nas rotas, para efeito em swagger e postman (ainda em implemtacao)** (2024-06-20 – Fernando Castelano Banhos)
  - RALServer.pas
  -- Limpeza em função TRALModuleRoutes.CanAnswerRoute
  - RALAuthentication.pas
  -- Add SetAuthRoute em TRALAuthServer
  - RALSwaggerExporter.pas
  -- Implementado exportação dos parametros da rota
  - RALSwaggerModule.pas
  -- Corrigido Bug no CreateRoutes (Retirado Domain)

- **- RALAuthentication.pas -- Add propriedade AuthRoute no TRALAuthServer, afim de facilitar o roteamento pelo TServer -- TRALServerJWTAuth removido propriedade Route -- TRALServerJWTAuth adaptado com a propriedade AuthRoute -- Adicionado funcao CanAnswerRoute no TRALAuthServer** (2024-05-20 – Fernando Castelano Banhos)
  - RALRoutes.pas
  -- TRALRoute renomeado para TRALBaseRoute, com propriedades publicas e nao publicadas
  -- Recriado TRALRoute, com as propriedade publicadas
  - RALServer.pas
  -- Retirado funcao IsDomain do TRALModuleRoutes
  -- Adaptacoes do TRALServer para identificar um rota de autenticacao
  - RALWEBModule.pas
  -- Retirado funcao IsDomain
  - RALPostmanExporter.pas
  -- Removido função AuthToRoute e convertida para RouteToJSON
  - RALPostmanExporter.pas
  -- Removido função AuthToRoute e convertida para RouteToJSON
  - RALSwaggerExporter.pas
  -- Removido função AuthToRoute e convertida para RouteToJSON
  -- Correções de bugs que nao mostrava o botão "Autorize" quando tinha Authentication
  - RALSwaggerModule.pas
  -- Retirado funcao IsDomain
  - RALToken.pas
  -- Melorias na função IsValidToken

- **- RALServer.pas -- Correção no TRALModuleRoutes para buscar rotas qdo o module nao é IsDomain** (2024-05-13 – Fernando Castelano Banhos)
  - RALPostmanExporter.pas
  -- Implemntação das funcão ExportToStream
  - RALSwaggerExporter.pas
  -- Implementação da versao OpenAPI 3.0
  - RALSwaggerModule.pas
  -- Implementação da rota de swagger dinamica
  -- Possibilidade de baixar o layout Postman via ExternalDocs do Swagger
  - RALDBModule.pas
  -- Melhoria nas rotas criadas

- **- Adicionado Assets das paginas do swagger - Criado SwaggerFile.rc para gerar Resources - Implementado RALSwaggerExporter.pas para exportar json do SwaggerFile - Implementado RALSwaggerModule.pas um modulo web para mostrar a pagina do swagger** (2024-05-12 – Fernando Castelano Banhos)

- **- RALPostmanExporter.pas -- Implementado exportador das funções do server = submodules para Postman** (2024-05-08 – Fernando Castelano Banhos)

- **- Implementado StorageCSV - ainda falta o load from stream - correção de bug de charset no StorageJSON (acentos estava sendo exportados errados)** (2024-05-07 – Fernando Castelano Banhos)

- **- RALRoutes.pas -- Correção de Leak (faltou free FURIParams)** (2024-05-02 – Fernando Castelano Banhos)
  - RALServer.pas
  -- Melhor forma para criar uma TRALRoute (procedure CreateRoute)

- **- RALDBStorage.pas -- Implementado procedure ReadFieldDateTime de um Int64 (UnixDateTime)** (2024-05-01 – Fernando Castelano Banhos)
  - RALDBStorageJSON.pas
  -- Implementado LoadFromStream do TRALDBStorageJSON_RAW

- **- RALTypes.pas -- Adicionado tipo TRALDateTimeFormat;** (2024-05-01 – Fernando Castelano Banhos)
  - RALDBStorageJSON.pas
  -- Renomeado TRALJSONFormat para TRALJSONType
  -- Criado TRALJSONFormatOptions para conversao de datetime
  -- Adicionado propriedade FormatOptions para obter as opcoes do TRALJSONFormatOptions
  - RALDBTypes.pas
  -- Adicionado nova classe TRALDBUpdateSQL, em implementação
  - RALQueryStructure.pas
  -- Criado proteção para Dataset Zeos quando o cliente não tiver habilitado o DEFINE de SaveToStream no ZMemTable
  - RALDBZeos.pas
  -- SaveFromoStream renomeada para SaveToStream
  -- Exportação Nativa refeita para Delphi e Lazarus (SaveToStream)
  -- Função CanExportNative refeita
  - RALDBFireDAC.pas
  -- SaveFromoStream renomeada para SaveToStream
  - RALDBBase.pas
  -- SaveFromoStream renomeada para SaveToStream
  - RALDBZeosMemTable.pas
  -- Refeita procedure OnQueryResponse, para abrir nativo o ZMemTable
  - RALDBBufDataset.pas.pas
  -- Refeita procedure OnQueryResponse, para abrir nativo o TSQLQuery (BufDataset)
  - RALDBSQLDB.pas.pas
  -- SaveFromoStream renomeada para SaveToStream
  -- Exportação Nativa refeita para Lazarus (SaveToStream)

- **- Ajuste de Answer para responder Stream sem necessariamente precisar de um AddFile** (2024-05-01 – Mobius One)

- **- Implementado RALDBBufDataset.pas** (2024-04-29 – Fernando Castelano Banhos)

- **- Implementado RALDBFDMemTable - Registrado TRALDBFDMemTable para instalacao no Delphi - Movido TRALDBOnError para unit RALDBTypes.pas** (2024-04-29 – Fernando Castelano Banhos)

- **- RALDBTypes.pas -- Implementado função ParseSQLParams** (2024-04-28 – Fernando Castelano Banhos)
  - RALDBZeosMemTable.pas
  -- Renomedo classe TRALDBZeosMemTable para TRALDBZMemTable
  - RALDBSQLDB.pas
  -- Corrigido falta de uses RALMIMETypes

- **- RALClient.pas -- Setado valor default ExecBehavior no Create do RALClientMT** (2024-04-28 – Fernando Castelano Banhos)
  - RALDBZeosMemTable.pas
  -- Mudado propriedade Storage para TRALDBStorageLink
  - RALTools.pas
  -- Correção de bug no FixRoute, rota '/' era transformada em ''

- **- RALDBBase.pas -- Função SaveFromStream modificada para melhor adaptação devido alguns Datasets -- Implementado funcão CanExportNative para melhor controle de alguns Datasets** (2024-04-27 – Fernando Castelano Banhos)
  - RALQueryStructure.pas
  -- Modificado função GetQueryClass para detectar a classe por ParentClass
  - RALDBModule.pas
  -- Adaptaçãos para as funcoes SaveFromStream, CanExportNative do RALDBBase
  - RALDBZeosMemTable.pas
  -- Inicio das Implementaçao do RALZeosMemTable

- **- DBStorages (BIN, JSON, BSON) -- Movida as funcoes de ReadFieldXXX e para unit RALDBStorage.pas** (2024-04-27 – Fernando Castelano Banhos)
  - RALBase64.pas
  -- Implementada funcao DecodeAsStream de uma String

- **- DBStorage BIN reformulado - DBStorage JSON reformulado.. ainda não implementado - DBStorage BSON implementado** (2024-04-26 – Fernando Castelano Banhos)

- **- RALRegister.pas -- Implementado PropertyEditor para a propriedade BaseUrl do Cliente -- Implementado PropertyEditor para a propriedade CompressType do Cliente e Servidor** (2024-04-19 – Fernando Castelano Banhos)
  - RALRoutes.pas
  -- Funcao CanResponseRoute renomeada para CanAnswerRoute
  - RALCompress.pas
  -- Implementada funcao GetInstaledList para responder ao PropertyEditor.

- **- RALClient.pas -- Modificado para que o cliente execute a rota até q tenha um StatusCode > 0 ou a quantidade de testes seja > 3** (2024-04-17 – Fernando Castelano Banhos)
  - RALParams.pas
  -- Removido procedures AssignCookies e AssignCookiesText, por se tratar de cookies de Response, foram movidos para o RALResponse
  - RALResponse.pas
  -- Funcoes GetParamsCookies e GetParamsCookiesText, produzem headers de cookie para Response (Set-Cookie)
  - RAL Clientes Engines
  -- Melhora no tratamento de erros nas respostas da requisicao
  -- Implementação de envio de cookies para o servidor
  -- Clientes testados retornando StatusCode 500 e com respostas de texto, funcionando
  - RAL Server Engines
  -- Melhora na Resposta de Cookie para o cliente (Headers Set-Cookie)
  * Header Set-Cookie é unico para cada cookie.

- **- RALParams.pas -- Implementado funcao AssignCookies e AssignCookiesText para headers de Cookie - RALServer.pas -- Adicionado propriedade CookieLife para limitar a "vida" de um cookie** (2024-04-16 – Fernando Castelano Banhos)
  RAL Server Engines
  - Consertado headers de cookies

- **- Adicionado session no RALWebModule - Implementado "path" na resposta do cookie** (2024-04-15 – Fernando Castelano Banhos)

- **- Implementacao da propriedade ContentDispositionInline, para que as respostas RALWebModule seja no diretas no browser** (2024-04-14 – Fernando Castelano Banhos)

- **- Engines Server -- Implementado leitura (request) e escrita (response) de cookies** (2024-04-14 – Fernando Castelano Banhos)
  - RALCustomObjects.pas
  -- Implementado AddCookies, parse para cookie vindo do cliente
  - RALTools.pas
  -- Implementado funcao RALDateTimeToGMT, adaptada para delphi7, delphi novos e lazarus

- **- RALRoutes.pas -- Adaptação do TRALRoute para aceitar Params URI -- Removida propriedade RouteDomain -- Renomeada propriedade RouteName para Route -- Adicionada propriedade Name -- Removida propriedade RouteAddress do TRALRoutes -- Adicionada função CanResponseRoute no TRALRoutes** (2024-04-14 – Fernando Castelano Banhos)
  - RALServer.pas
  -- Adaptações para modificações do TRALRoutes e TRALRoute
  - RALWEBModule.pas
  -- Adaptações para modificações do TRALRoutes e TRALRoute
  -- RALTools.pas
  - Correção de bug - route aceitando path transversal

- **- RALServer -- Correção de header de CORS - RALCompressZLib -- Correção para compilar em Delphi - RALDBStorageJSON -- Implementação de charcase nas keys** (2024-04-11 – Fernando Castelano Banhos)

- **- RALUniGUIServer -- Modificação do SetActive para checar dependencias antes de iniciar - RALfpHTTPClient -- Implementado protecao no OnDestroy do TRALfpHttpServer verificando se está ativo** (2024-04-09 – Fernando Castelano Banhos)

- **- RALServer.pas -- Implementado chekagem de dependencias das bibliotecas (dll, so, dylib) com as classes de compressão instanciadas - RALServer Engines -- Mudanca na forma de Ativar (SetActive) os servidores, deve passar pela checagem primeiro - RALCompress e heranças -- Implementado função para checar se a biblioteca de compressão foi instanciada** (2024-04-09 – Fernando Castelano Banhos)

- **- Implementada nova classe RALCompressBrotli - Melhoria no Compress e Decompress da TParams - RALCompress.pas adaptada para compressão Brotli - Pequenas correções na RALCompressZLib e RALCompressZStd** (2024-04-08 – Fernando Castelano Banhos)

- **-- RALClient.pas - Implementação no ClientMT a possibilidade de controlar o request (Life Cicle), para isso foi adicionada a propriedade RequestLifeCicle --- Se RequestLifeCicle = True o request será clonado pela thread e liberado para ser destruido --- Se RequestLifeCicle = False o request será utilizado pela thread e destruido pela thread** (2024-04-06 – Fernando Castelano Banhos)
  -- Adicionado procedure Clone dos Objetos (TRALRequest, TRALParam, TRALHTTPHeaderInfo)

- **Implementacão do TRALFDQueryMT (com código duplicado da TRALFDQuery para estudos); * Mobius, por favor não remova as units do cabeçalho:-(** (2024-04-05 – Gustavo Souza)

- **Create advanced_issue_labeler.yml** (2024-04-01 – Mobius One)

- **Create issue_labeler.yml** (2024-04-01 – Mobius One)

- **Create config.yml** (2024-04-01 – Mobius One)

- **Create issue_labeler.yml** (2024-04-01 – Mobius One)

- **- Adaptacoes do RALClient para Lazarus e pequenas correções - Clientes Synopse, fpHTTP, netHTTP implementado multithread - Client Indy adaptado para Lazarus - Pequeno bug de compilação do SaguiServer** (2024-03-30 – Fernando Castelano Banhos)

- **- RALAuthentication.pas -- Mudanças em alguns métodos para implementação do clientes multithread - RALClient.pas -- Implementado base para cliente multithread -- Unit totalmente reformulada - RALParams.pas -- Pequenas correções para funcionar o EncodeBody - RALIndyClient.pas -- Implementado cliente multithread -- Unit totalmente reformulada - RALSaguiServer.pas -- Correção de bug no Start do servidor -- Correção de bug no StreamRead - RALThreadSafe.pas -- Revert e limpeza da unit** (2024-03-29 – Fernando Castelano Banhos)

- **- RALParams.pas -- Correção de EncodeBody, quando o Index[0] nao é um Body ou um Field, para resolver foi implementado a IndexKind - RALServer.pas -- Correção da autenticação do ProcessCommands** (2024-03-27 – Fernando Castelano Banhos)

- **- implementacao de lista de bloqueios multi client no RALServer - limpeza nos fontes da unit RALServer.pas - revert na constant RALDefaultPage (%ralengine%)** (2024-03-27 – Fernando Castelano Banhos)

- **-- DBWare - Implementacao da class TRALDB, mapeando os tipo de dados (TRALFieldType) - Adaptação do TRALFieldType para Storage, QueryStructure -- RALTypes.pas - Melhorada constante POSINISTR - Nova constante de DEFAULTBUFFERSTREAMSIZE, definido o tamanho máximo dos buffers - Removida funcao RALLowStr, constante POSINISTR resolve - Adaptações da constante POSINISTR em diversas units - Adaptações da constante DEFAULTBUFFERSTREAMSIZE em diversas units** (2024-03-24 – Fernando Castelano Banhos)

- **- Correção de bug nas definicoes de header Connection nos cliente engines - RALDBStorage -- Iniciando implementação de stream para um dataset - Pequenos ajustes em units para melhor utilização do RAL** (2024-03-19 – Fernando Castelano Banhos)

- **- Correção de Tipo ValidadeResquest para ValidateRequest - Melhorias no DBWare, avanços na implementacao - Nova Unit RALQueryStructure.pas, converte uma estrutura de query (SQL e Params) para BIN ou JSON** (2024-03-18 – Fernando Castelano Banhos)

- **-- Compress - Funcoes pertinentes ao assunto foram movidas para units RALCompress -- RALServer.pas - ProcessCommands mudado para procedure - Implementado procedure ValidadeRequest, um validador de requests (headers) para bloqueio de requisições indevidas antes do processamento de rotas -- Engines Servers - Adaptações das mudanças no RALServer.pas** (2024-03-17 – Fernando Castelano Banhos)

- **- implementação de autenticação para Server Sagui** (2024-03-16 – Fernando Castelano Banhos)

- **- RALParams -- Implementado alguns SetAs faltantes - Correção alguns servers não estavam enviando o ContentDisposition - Melhora na unit RALMultipartCoder** (2024-03-16 – Fernando Castelano Banhos)

- **-- RALSaguiServer.pas - Implementado leitura do RawBody (PayLoad) - Limpeza da unit + encapsulamentos** (2024-03-12 – Fernando Castelano Banhos)

- **Create workflow-issue.yaml** (2024-03-08 – Mobius One)

- **- Limpeza de variaveis redudantes entre TRALClient e TRALResponse - RALResponse -- Implementado TRALServerResponse e TRALClientResponse - Client Engines readaptados para o novo TRALResponse** (2024-03-06 – Fernando Castelano Banhos)

- **- RALCompress desacoplada - Compress ZStd implementada** (2024-03-04 – Fernando Castelano Banhos)

- **- Implementando LoadFromStream para Dataset** (2024-02-29 – Fernando Castelano Banhos)

- **- Adaptações em alguns pacotes para DBWare - Correção de typo na unit RALDBZeos - RALDBWare.pas -- Início da implementacao da função RALParamBinaryToQuery - RALDB.pas -- criada para simplificar funções e conversões de tipos da classe DB - Arquivos de Registro dos DBLink SQLDB e Zeos renomeados para ficar compativel ao pacote do FireDAC** (2023-12-04 – Fernando Castelano Banhos)

- **- RALParams.pas -- Proteção para evitar leak ao processar MultiPart - RALServer.pas -- Correção de otimização de string parameter - RALDatasetStorage.pas -- Implementado LoadFromStream - RALStorage.pas -- Remodelada para funciona com Value tipo TStream -- Implementado função para adicionar Fields tipo Blobs - RALStorageBIN.pas -- Nova Unit para escreve e ler dados binarios, tipo TDataset - RALStorageJSON.pas -- Adicionado Register da Class StorageLink - RALStream.pas -- Otimização na conversão de Stream para String - RALTools.pas -- Implementado funções: OnlyNumbers e RALStringToDateTime -- Excluída função StrIsUTF8** (2023-11-27 – Fernando Castelano Banhos)

- **- Classe TRALStorage criada como base para todos os exportadores - Classe TRALStorageJSON, implementado exportação para JSON - Classe TRALDatasetStorage exportando para um Storage - RALDBBase.pas -- Modificações do DatabaseOutPut para um TStorageLink - RALDBWare.pas -- Modificações do DatabaseOutPut para um TStorageLink -- Readaptações na exportação do OpenSQL** (2023-11-19 – Fernando Castelano Banhos)

- **- Implementação da Classe RALDBLink, para componentizar os Drivers (Zeos, LazSQLDB e FireDAC)** (2023-11-02 – Fernando Castelano Banhos)

- **- Limpeza de units - Implementado Conectores para LazSQL e FireDAC** (2023-11-02 – Fernando Castelano Banhos)

- **- Inicio das implementações para DBWare - RALRoutes.pas -- Adaptação da TRoute para TSubRoutes - RALServer.pas -- Implementado classe TRALSubRoutes** (2023-11-02 – Fernando Castelano Banhos)

- **- Implementação Server CGI** (2023-10-27 – Fernando Castelano Banhos)

- **- Implementação para CreateRoute para uso em modo console** (2023-10-22 – Fernando Castelano Banhos)

- **- Correções de pequenos bugs nas implementações da criptografia** (2023-10-15 – Fernando Castelano Banhos)

- **- RALAuthentication.pas -- EncodeBody do TRALParams foi modificada - RALClient.pas -- Implementado criptografia AES -- Adicionado Stream da ultima resposta como variável - RALConsts.pas -- Adicionado constante SupportedEncriptKind - RALParams.pas -- Adicionado propriedade CompressType -- Adicionado propriedade CriptoOptions - RALRequest.pas -- Adicionado propriedade Stream (conteúdo pós DecodeBody, descomprimida e descriptografada) - RALResponse.pas -- Adicionado propriedade CriptoKey -- Adaptações para as funcionalidades de Criptografia - RALServer.pas -- Adicionado propriedade CriptoOptions - RALTypes.pas -- Modificado tipo TRALCriptoType - RALfpHTTPServer.pas, RALIndyServer.pas, RALSynopseServer.pas -- Adaptações para as funcionalidades de Criptografia - RALfpHTTPClient.pas, RALIndyClient.pas, RALSynopseClient.pas, RALnetHTTPClient.pas -- Adaptações para as funcionalidades de Criptografia - RALCripto.pas -- Implementado Create para TRALCriptoOptions - RALCriptoAES.pas -- Implementado função para gerar S-BOX (encode e decode) - RALCustomObjects.pas -- Adicionado proriedades ContentEncoding e AcceptEncoding, para criptografia - RALTools.pas -- Implementado funções para converter TRALCriptoType para string e vice-versa** (2023-10-15 – Fernando Castelano Banhos)

- **- RALCripto.pas -- Adicionadas mais funções para trabalhar com Bytes - RALCriptoAES.pas -- Implementado Padding PKCS7** (2023-10-09 – Fernando Castelano Banhos)

- **- RALTypes.pas -- Adicionado Tipo TRALCriptoType - RALCripto.pas -- Corrigido no Lazarus, Saida em Bytes maior que o Size do Stream - RALBase64.pas -- Corrigido no Lazarus, Saida em Bytes maior que o Size do Stream - RALCriptoAES.pas -- Implementado Decrypt -- Melhorias nas performances no processamento** (2023-10-09 – Fernando Castelano Banhos)

- **- Inicio das implementações da classe base para Criptografia - Inicio das implementações da classe de Criptografia AES** (2023-10-07 – Fernando Castelano Banhos)

- **- RALServer.pas -- Implementado propriedade FavIcon, para responder /favicon.ico - RALnetHTTPClient.pas -- Bug faltando a unit RALConsts** (2023-09-25 – Fernando Castelano Banhos)

- **- RALJSON_xxx.inc -- Implementado proteção para funções quando a key não existir - RALToken.pas -- Token JWT implementado HS384** (2023-09-12 – Fernando Castelano Banhos)

- **- RALJSON_xxx.inc -- Implementado função ParseJSON de Stream** (2023-09-11 – Fernando Castelano Banhos)

- **- Correção de quebra de linha em vários arquivos - Renomeado função AddText para AddBody - Renomeado função RawBody para Body** (2023-09-10 – Fernando Castelano Banhos)

- **-RALAuthentication.pas -- Implementação do método OnBeforeGetToken para TRALAuthClient - RALClient.pas -- Implementação da funcão AddText, facilitando envio de Texto e JSON - RALRequest.pas -- Implementação da funcão AddText, facilitando envio de Texto e JSON - RALCustomObjects.pas -- Implementação da funcão AddText, facilitando envio de Texto e JSON -- Implementação da funcão RawBody - RALSynopseClient.pas -- Reimplementado devido bug de SendTimeout, ReceiveTimeout** (2023-09-10 – Fernando Castelano Banhos)

- **- Correção da função IsNilOrEmpty - Implementador função de GetClaim para TokenJWT** (2023-09-09 – Fernando Castelano Banhos)

- **- Implementações para Auth Digest** (2023-09-01 – Fernando Castelano Banhos)

- **- Implementações para token Digest - RALJSON - renomeados arquivos RALJSON_xxx.pas para RALJSON_xxx.inc** (2023-08-30 – Fernando Castelano Banhos)

- **- Implementacão da Authentication Digest Client (não concluida)** (2023-08-23 – Fernando Castelano Banhos)

- **- RALAuthentication - Inicio das implementações do OAuth1 - RALClient - Funções de GetToken separadas por tipo - RALConsts - Novo tipos TRALAuthTypes - RALRequest - Adicionado Propriedades Host, Protocol, HttpVersion - Server Engines - Implementado capturar propriedades Host, Protocol, HttpVersion para o request - RALCustomObjects - Nova Função para Adicionar Param Query - RALSHA1 - Novo tipo de Hash - TRALToken - Inicio das implementações do OAuth1** (2023-08-20 – Fernando Castelano Banhos)

- **- RALClient.pas - implementado Connection header (keep-alive ou close) - Clients Engines - implementado envio Connection header (keep-alive ou close) - Server Engines -- Implementado resposta do header Server -- Implementado resposta do header Connection de acordo com o header do Client - RALfpHTTPServer.pas - Adicionado propriedade QueueSize - RALSynopseServer.pas -- Adicionado propriedade QueueSize -- Adicionado propriedade PoolCount** (2023-08-18 – Fernando Castelano Banhos)

- **- RALServer.pas - Correção de AV para AddParamUri quando a rota não existe - RALCustomObjects.pas - Corrigido bug TRALHTTPHeaderInfo, FParams não estava sendo destruído** (2023-08-15 – Fernando Castelano Banhos)

- **- RALAuthentication.pas - adaptação para novas funções do RALParams - RALClient.pas -- adaptação para novas funções do RALParams -- implementado métodos Self result - RALParams.pas -- implementado nova função AddFile - Novo Classe TRALHTTPHeaderInfo implementada no RALReques, RALResponse, RALClient - Revert em RALTypes - removido RALStringList - Cliente (Indy, Synopse, fpHTTP) - readaptados com as modificacoes de Params e Classe TRALHTTPHeaderInfo** (2023-08-14 – Fernando Castelano Banhos)

- **- RALParams.pas - implementado novos métodos de AppendParams e AssignParams para facilitar no motores - RALTypes.pas - adicionando rpkCOOKIE no TRALParamKind - Server (Indy, Synopse, fpHTTP) - readaptados com as funções novas do RALParams.pas** (2023-08-13 – Fernando Castelano Banhos)

- **- RALTypes -- Movida função RALHTTPDecode para unit RALUrlCoder.pas (nova TRALHTTPCoder.DecodeURL) -- Inicio da implementação da TRALStringList para ser usada Headers, Cookies, ParamFields - RALTools -- Novas funcoes RALLowStr, RALHighStr, StrIsUTF8 -- RALUrlCoder - nova classes para codificar e decodificar de URL e HTML** (2023-08-13 – Fernando Castelano Banhos)

- **- Implementado função de SaveToFile para RALParams - Implementado GetMIMEContentExt** (2023-08-11 – Fernando Castelano Banhos)

- *** Ajuste de identação  + Criação de overload de create em autenticação basic  * Rearranjo de tipos internos para RALTypes e mensagens internas para RALConsts  * Ajuste de nomenclatura de params pra tornar mais intuitivo  + Criação de property Body para retornar o body da requisição nos params  * Modificação de assinatura do CreateRoutes para permitir uma forma diferenciada de criação de rotas  - Correção de IFDEFs em PascalRAL.inc  * Ajustes diversos no Synopseserver pra funcionar Synopse com Delphi, não testado em Lazarus  * Ajustes diversos no servidor de teste adicionando funcionalidades de teste** (2023-08-10 – Mobius One)

- **- Mudança na variável $ralengine; para %ralengine% - Mudança no Typo TRALMIMEDecoder para TRALMultipartDecoder - Renomeado RALMimeCoder.pas para RALMultipartCoder.pas - Implementado TRALMultipartEncoder - TRALResponse -> GetResponseStream e GetResponseText podem responder em MultiPart - TRALParams -> DecodeBody processando x-www-form-urlencoded para Fields** (2023-07-30 – Fernando Castelano Banhos)

- **- Implementado objeto para controle de IPv6 - Implementado IPv6 no Server do Synopse** (2023-07-29 – Fernando Castelano Banhos)

- **- Função MethodToRALMethod transferida para o TRALServer, renomeada para HTTPMethodToRALMethod - RALnetHTTPClient implementado métodos faltantes** (2023-07-29 – Fernando Castelano Banhos)

- **- Correção de Typo hcHEAD e hcTRACE para amHEAD e amTRACE - Implementação nos clientes e servers dos métodos faltantes - Nova função na RALTools MethodToRALMethod** (2023-07-29 – Fernando Castelano Banhos)

- **+ Função CreateRoute no Server.** (2023-07-27 – Mobius One)

- **- Mudança de TRALContentTypeHelper para TRALContentType (Delphi apontou erros) - Adicionado metodos de SetServerStatus, SetBlackIPList, SetWhiteIPList (Lazarus apresentada AccessViolation) - Implementado Cliente NetHttp** (2023-07-23 – Fernando Castelano Banhos)

- **- Adicionado funções de AppendParams e AcquireParams no TRALParams - Implementado TRALfpHttpClient - Implementado TRALSynopseClient - TRALMIMEDecoder implementado para leitura de multipart/formdata no TRALParams (DecodeBody)** (2023-07-23 – Fernando Castelano Banhos)

- **- Implementado TRALStringListSafe (RALThreadSafe.pas) - RALServer alterado como trabalhar com TRALStringListSafe nas listas de bloqueios** (2023-07-20 – Fernando Castelano Banhos)

- **- Implementado WhiteIPList no Server (liberando os IPs "localhost")** (2023-07-20 – Fernando Castelano Banhos)

- **- Correção de bug na busca da Route (RouteAddress) - Criado opção de BruteForceProtection para os Server** (2023-07-20 – Fernando Castelano Banhos)

- **- Implementado funções de conversão de Base64 para Base64Url - Adaptado fontes dos Hashes para funcionar com Base64Url - CreateToken JWT fontes adaptados para Base64Url** (2023-07-20 – Fernando Castelano Banhos)

- *** Ajuste de pacotes pra funcionar em todas as plataformas.  - Correção de overload no MimeCoder.  * Ajuste de pacote principal pra incluir todos os arquivos da .bpl.  * Início de implementação de funções de parse de params no server.** (2023-07-19 – Mobius One)

- **- Implementação do Server Synopse Mormot2** (2023-07-18 – Fernando Castelano Banhos)

- **- Implementado Decoder de MIME (body -> form-data)** (2023-07-16 – Fernando Castelano Banhos)

- **- Bug Na Funcao CallQuery do AuthJWTServer - Criado engine fpHTTP para inicio das implementações** (2023-07-16 – Fernando Castelano Banhos)

- **- Implementado JWT Client e Server - Base64 - iniciando base64 for URL** (2023-07-16 – Fernando Castelano Banhos)

- **- Troca dos nome do componente TRALBasicAuth para TRALBasicAuthServer - Troca dos nome do componente TRALJWTAuth para TRALJWTAuthServer - Mudança no nome do objeto TRALAuthClient para TRALAuthorization - Funcao FixRoutes transferida para RALTools - Implementado JWTServer** (2023-07-15 – Fernando Castelano Banhos)

- **- Implementado Auth Basic (Server e Client) - Correção de Abstract Error para RALClient - Liberado JWTAuth para Lazarus - Correções de indentações de fontes (RALRoutes, RALKeyPairs, RALToken)** (2023-07-15 – Fernando Castelano Banhos)

- **- Implementada RALJson.pas com o helpers - Adaptada RALToken.pas para funcionar com os helpers da RALJson.pas** (2023-07-14 – Fernando Castelano Banhos)

- **- RALAuthentication.pas - Criado Identificação nas Autenticações - RALRoutes.pas - Adicionado Authorization no TRALRequest - RALTypes.pas - Adicionando TRALAuthTypes - RALIndyServer.pas - implementado funcao OnParseAuthentication** (2023-07-10 – Fernando Castelano Banhos)

- **- Modificado JWTToken unificando tudo na unit RALToken.pas - Implementado unit RALKeyPairs.pas para tipagem de dados** (2023-07-09 – Fernando Castelano Banhos)

- **- Iniciado as implementações do Token JWT -** (2023-07-09 – Fernando Castelano Banhos)

- **- Unificado funções de Hash para unit RALHashes.pas - Implementado função de HashMD5 - Limpeza nas units RALSHA2_32.pas, RALSHA2_64.pas** (2023-07-09 – Fernando Castelano Banhos)

- **- Renomeado arquivo RALSHA2_256.pas para RALSHA2_32.pas (32 bits) (SHA2_224,SHA2_256) - Implementado RALSHA2_64.pas (64 bits) (SHA2_384,SHA2_512,SHA2_512_224,SHA2_512_256)** (2023-07-08 – Fernando Castelano Banhos)

- **- Melhorias na formatação da unit RALBase64 - Implementado RALHashes para servir de base - Implementado RALSHA2_256** (2023-07-08 – Fernando Castelano Banhos)

- **- Base64 implementada para Delphi e Lazarus** (2023-07-07 – Fernando Castelano Banhos)

- **- Inicio Implementações Cliente Base - Inicio Implementações Cliente Indy** (2023-07-05 – Fernando Castelano Banhos)

- **- Transportado classes TRALRequest, TRALResponse, TRALClientInfo, TRALParams, TRALParam da unit RALServer para RALRoutes - Implementado OnReply nos Routes** (2023-07-04 – Fernando Castelano Banhos)

- **- Adicionado const PosIniStr para futuras plataformas - Troca de .Free para FreeAndNil - Criado e implementado SSL para Indy** (2023-07-04 – Fernando Castelano Banhos)

- **- Implementado acesso por IPv4 e IPv6 no Indy** (2023-07-03 – Fernando Castelano Banhos)

- **- Implementações de Rotas (40%) - Adicionado ServerStatus e ShowServerStatus no Server - Adicionado Tipo CharRAL - Implementações do Indy (30%)** (2023-07-03 – Fernando Castelano Banhos)

- **- Implementado Routes (60%)** (2023-07-03 – Fernando Castelano Banhos)

- **Create FormSuggestionEN.yml** (2023-06-24 – Mobius One)

- **Create FormSuggestionPT.yml** (2023-06-24 – Mobius One)

- **Create FormBugReportEN** (2023-06-24 – Mobius One)

- **Create FormBugReportPT** (2023-06-24 – Mobius One)

- **Create FUNDING.yml** (2023-05-23 – Mobius One)


### Changed
- **Merge remote-tracking branch 'remotes/origin/master' into dev** (2025-10-25 – Mobius One)

- **AV em perda de conexão com o endpoint. Simular, tirar cabo de rede.** (2025-10-03 – ANTONIO GOMES)

- **AV em perda de conexão com o endpoint. Simular, tirar cabo de rede.** (2025-10-03 – ANTONIO GOMES)

- **Update README.md** (2025-09-17 – Mobius One)

- **Merge remote-tracking branch 'remotes/origin/dev'** (2025-09-11 – Mobius One)

- **Merge remote-tracking branch 'remotes/origin/dev'** (2025-08-18 – Mobius One)

- **- Correção de compilação para Lazarus para usar type helper ao invés de record helper  - Mudança do Helper de base64 para RALTypes, por padronização do código  - Ajuste de versionamento** (2025-06-26 – Mobius One)

- **- Correção de parse para params x-www-form-urlencoded cujo value é vazio  - Pequena otimização do parse melhorando um pouco o tempo de resposta dos servers  + Adição de função para devolver os params como um JSONObject: Params.AsJSON** (2025-06-23 – Mobius One)

- **Update config.yml** (2025-06-19 – Mobius One)

- **- Criação de uma classe intermediária para facilitar exportação de criptografia do pacote, RALHashes.pas #78  * Ajuste em todas as classes para trocar RALHashes -> RALHashBase  - Correção de IntegerOverflow nas hashes SHA  * Ajuste de mensagem de erro referente às classes de criptografia e hash  + Tratamento de entrada para evitar Invalid Pointer e Rangecheck Error  - Ajuste de versionamento** (2025-06-14 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-06-14 – Gustavo Souza)

- **- Correção de erros na criptografia;** (2025-06-14 – Gustavo Souza)

- **- Merge Dev -> Master  - Ajuste versionamento** (2025-06-13 – Mobius One)

- **- ajuste no server FPHttp para o max de conexoes permitidas pelo operacional - ajuste no server indy e sagui colocando as procedures para o protected - ajuste no server indy nas propriedades ListenQueue e MaxConnections acessando diretamente no motor** (2025-06-04 – Fernando Castelano Banhos)

- **- Ajuste de versionamento  + Exportação de propriedades para ajuste fino de limites no Indy, FpHttp e Sagui  - Correção de nomenclatura de arquivo de strings pt-br** (2025-06-04 – Mobius One)

- **- melhoria na ordem da propriedade do CompressType - mudança no tipo da propriedade DatabaseLink do TRALDBModule de StringRAL para String** (2025-05-11 – Fernando Castelano Banhos)

- **- mudança no tipo da propriedade EngineType do RALClient de StringRAL para String** (2025-05-11 – Fernando Castelano Banhos)

- **- adicionado compress type ctNone na lista de compressao dos componentes RALServer e RALCliente - correção de AV para descompressão de stream com size = 0** (2025-05-11 – Fernando Castelano Banhos)

- **- ajuste de versionamento** (2025-04-16 – Fernando Castelano Banhos)

- **- melhoria na adição de units requeridas pelo TRALClient, adicionando na unit correta** (2025-04-16 – Fernando Castelano Banhos)

- **- Ajuste de conversões implícitas.  * Ajuste de strings hardcoded para resource permitindo que sejam localizadas.  * Tradução de strings nos recursos.** (2025-03-29 – Mobius One)

- *** Ajuste de versionamento pra permitir controle por diretivas** (2025-03-27 – Mobius One)

- **- Adaptações nas classes Compress para facilitar a expansão futuras, padronizando os fontes como foi feito com as classes TRALClient e TRALDBWare** (2025-03-26 – Fernando Castelano Banhos)

- *** Correção de versionamento** (2025-03-25 – Mobius One)

- **- Correção de erros internos do Sagui com content de tamanho zero ou stream inválido  - Prevenção de erro de compactação em ZLib se caso o conteúdo a ser compactado for zero** (2025-03-25 – Mobius One)

- **-  melhoria na funcao RALCPUCount** (2025-03-21 – Fernando Castelano Banhos)

- **- Correção de Target de pacote pra win32 pra funcionar por padrão com todas as IDEs Delphi.  - Corrigidos package Indy e Synopse.** (2025-03-19 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-03-19 – Mobius One)

- *** Atualização de versão para 0.11.0-2  * Atualização de package sagui pra versão 3.5.2  * Ajuste de wizard e arquivos .dproj para funcionar com Delphi 12.3  * Ajuste de wizard pra funcionar com IDE 64b 12.3  * Ajuste no SynopseServer para encerrar corretamente o socket  * Ajuste de documentação no RALServer** (2025-03-19 – Mobius One)

- **- Removido componentes DBLinks (TRALDBFireDACLink, TRALDBZeosLink, TRALDBSQLDBLink) - Propriedade DatabaseLink do componente TRALDBModule, modificada e adaptada automaticamente aos Drivers - Ajustes no pacote RALDBPackage do Lazarus - Adaptações para o componente TRALDBModule funcione sem os DBLinks** (2025-03-18 – Fernando Castelano Banhos)

- **- correção do Server Sagui para aceitar compressão - correção do Server Sagui para capturar o payload (body) corretamente - correção da descompressão gzip para Delphi** (2025-02-26 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-02-25 – Fernando Castelano Banhos)

- **+ Criação de ícone para ClientRAL unificado  - Ajuste de arquivos de recurso pra incluir as modificações dos clientes  - Adição de uma condição pra usar o valor direto da ARoute se BaseURL tiver vazio, simplificando chamadas GET** (2025-02-17 – Mobius One)

- **- Adaptações para RALCliente funcionar - Correção de alguns leaks ao clonar RALRequest** (2025-02-14 – Fernando Castelano Banhos)

- **- RALNetHttp e RALDBFiredacDAO adaptado com o novo RALClient - Criado para Delphi e Lazarus funçoes para auto declarar as classes dos Engines   conforme selecionado** (2025-02-13 – Fernando Castelano Banhos)

- **- Unificado RALClienteMT e RALCliente - Mudanças e Adaptações nos clientes Indy, fpHTTP, Synopse - Adaptações das mudanças para a classe TRALDBConnection** (2025-02-13 – Fernando Castelano Banhos)

- **- Correção de compatibilidade de plataformas no pacote BSON em design-time para Delphi  * Atualização de versionamento** (2025-01-24 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2025-01-24 – Fernando Castelano Banhos)

- **Merge remote-tracking branch 'remotes/origin/dev'** (2025-01-06 – Mobius One)

- **- Removido submodule brookframework.** (2025-01-06 – Mobius One)

- **- Atualização de submodule.  - Removido submodule brookframework.  + Adição de libsagui untracked para instalação do SaguiRAL.** (2025-01-06 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-11-27 – Mobius One)

- **- Atualização de versão para 0.10.0.  - Mudança estrutural dos pacotes movendo pacote Storage para a base do projeto.  - Criação de categoria própria dos Storages, "RAL - Storage" na paleta de componentes.  - Atualização dos caminhos das imagens de ícone dos componentes.  - Ajuste de dependências de pacotes para incluir a mudança de caminho dos Storages.  - Removida dependência do DBPackage pra instalar os Storages que podem ser usados sem depender de DBWare.** (2024-11-27 – Mobius One)

- **- Correção de versionamento** (2024-11-24 – Mobius One)

- **- Conversão de RALMIMETypes para Singleton para reduzir tempo de carga  - Ajuste de coleta de MIMETypes do sistema otimizando a carga** (2024-11-24 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-11-11 – Gustavo Souza)

- **- Correção dos AVs em Exceptions (lifetime);** (2024-11-11 – Gustavo Souza)

- **- correção no indy client, retornando todos os exceptions** (2024-11-11 – Fernando Castelano Banhos)

- **- correçao multipart encode, charset com chars NUL** (2024-11-09 – Fernando Castelano Banhos)

- **-- correção do multipart decode, erro de charset** (2024-11-09 – Fernando Castelano Banhos)

- **- Ajuste de versionamento** (2024-11-06 – Mobius One)

- **+ Pacote Storage BSON para trafegar outro tipo de dados além de binário, JSON ou CSV, para Delphi e Lazarus.  * Ajuste dos pacotes, libpaths e outros ajustes menores no Delphi.  * Atualização do project group no Lazarus pra incluir o pacote BSON.** (2024-11-06 – Mobius One)

- **- correção do CustomDateTimeFormat** (2024-11-04 – Fernando Castelano Banhos)

- *** Ajuste de versionamento** (2024-11-01 – Mobius One)

- **- Atualização de Submódulo ZSTD** (2024-11-01 – Mobius One)

- **- correção para Swagger e Postman Exporter, nao funcionavam no Delphi Linux** (2024-11-01 – Fernando Castelano Banhos)

- **- Cprreção de versionamento** (2024-10-31 – Mobius One)

- **- Correções de instalação no Linux** (2024-10-31 – Mobius One)

- **- Propriedade Storage dos MemTable revertida para TRALDBStorageLink - Adaptações no Server (DBModule) para exportar o resultado conforme storage requisitado pelo Client** (2024-10-30 – Fernando Castelano Banhos)

- **- RALTypes.pas -- Movida TRALStringStream para unit RALStream -- funcoes StringToBytes e BytesToString renomeadas para StringToBytesUTF8 e BytesToStringUTF8** (2024-10-29 – Fernando Castelano Banhos)
  - RALStream.pas
  -- Função StringToStream reformuda, não converte para utf8
  -- Função StringToStreamUTF8, converte uma string para TRALStringStream (utf8)

- **- RALTypes -- novo type UInt64RAL, para alguns delphi mais antigos** (2024-10-28 – Fernando Castelano Banhos)
  - RALHashes.pas, RALMD5.pas, RALSHA2_32.pas, RALSHA2_64.pas, RALBase64.pas, RALStream.pas
  -- adaptações para o nome type UInt64RAL

- **- correção de estouro de "index of bounds", ao usar Delphi e Lazarus** (2024-10-28 – Fernando Castelano Banhos)

- **- Ajuste de versionamento do pacote** (2024-10-27 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-10-25 – Mobius One)

- **- Correção de diretiva em RALTools para compilar para FMX Linux  * pequena otimização em RALDBTypes reduzindo chamadas a Pos  * Atualização de arquivo pasdoc.pds com as novas units desde a última versão.** (2024-10-25 – Mobius One)

- **- correção para FireDAC, assign fields e fields do servidor - melhorias no setactive do Zeos, para não entrar em loop - mudanças na funcao RALFieldTypeToFieldType sftString -> ftWideString e sftMemo -> sftWideMemo** (2024-10-25 – Fernando Castelano Banhos)

- **- correção para nao enviar nulos no updateSQL** (2024-10-25 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-10-25 – Mobius One)

- **- adicionado novos parametros para conexao dbware com sqlite** (2024-10-24 – Fernando Castelano Banhos)

- **- RALFDMemTable -- Correção de loop ao buscar dados do servidor dbware** (2024-10-23 – Fernando Castelano Banhos)
  - RALDBFiredac
  -- correção para SQLite, stringmode Unicode

- **- PascalRAL.inc -- Adicição de alguns defines para facilitar no desenvolvimento** (2024-10-20 – Fernando Castelano Banhos)
  - RALTypes.pas
  -- adaptações das novas diretivas incluidas no PascalRAL.inc
  - RALDBStorageCSV.pas
  -- adaptações das novas diretivas incluidas no PascalRAL.inc
  -- adicionado propriedade de exportação usando UTF8BOM

- **- RALTypes.pas -- TRALStringStream criado funcoes de WriteString, WriteBytes, WriteStream** (2024-10-20 – Fernando Castelano Banhos)
  - RALDBStorageCSV.pas
  -- WriteStringToStream usando encoding UTF8 para escrever no Stream

- **- Correção de conversão de campos memos para unicode** (2024-10-20 – Fernando Castelano Banhos)

- **- RALRequest.pas -- TRALServerRequest, GetRequestText aplicado conversao de charset** (2024-10-20 – Fernando Castelano Banhos)
  - RALTypes.pas
  -- Correção de TRALStringStream.DataString, Posicionando Stream da Posicao 0
  - RALDBStorageJSON.pas
  -- Conversão de charset na exportação do json para unicode
  - RALDBStorageCSV.pas
  -- Pequenas correções e melhorias

- **- Correçao do ResponseText, agora trazendo o UTF8 convertido** (2024-10-19 – Fernando Castelano Banhos)

- **- Acertos de Charset UTF8 - Correção no RALPostmanExporter.pas, quote na palavra "token"** (2024-10-19 – Fernando Castelano Banhos)

- **- Correção de compilação para Lazarus  + Pacote de grupo de projetos para Lazarus, facilitando instalação.** (2024-10-16 – Mobius One)

- **- Correção de compatibilidade com FireDAC(AnyDAC) para Delphi XE2, contribuição de Endrigo Rodrigues  - Ajuste de dependências de pacotes e plataformas juntamente de dependências para compatibilidade com FireDAC do Delphi XE2** (2024-10-15 – Mobius One)

- **- Ajuste de dependência de pacotes e search paths** (2024-10-15 – Mobius One)

- **- Correção de compatibilidade para Delphi XE2, com auxílio do Endrigo.  - Conversão de units UTF-8 com BOM para sem BOM.** (2024-10-15 – Mobius One)

- *** Ajustes de compatibilide com Delphi XE2, contribuições de Endrigo** (2024-10-14 – Mobius One)

- **- correção de mais defeitos causados pela mudança na StringRAL para widestring - retirado a TRALStringStream e retornado para TStringStream - mudança na funcao StreamToString** (2024-10-11 – Fernando Castelano Banhos)

- **- Ajuste de diretiva FMX pra detectar o FMX em delphis XE3 ou superior  - Ajuste de Wizard Standalone Lazarus para incluir a dependência da LCL** (2024-10-03 – Mobius One)

- **- Correção de Abstract Error em DBConnection** (2024-09-17 – Mobius One)

- **+ cláusula de *.lps em gitignore pra não subir arquivo temporário  * Ajuste no README com ícone novo do Discord e ajuste do texto** (2024-09-14 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-09-11 – Mobius One)

- **- Adição de unit faltante no pacote principal  - Ajuste de dependências de pacotes Lazarus  - Correção de compatibilidade com nova versão do mORMot2  - Correção de uses em templates do wizard  - Reforma para forçar UTF8 internamente nas Streams do componente  - Ajuste de compatibilidade com Delphi 10.0 Seattle** (2024-09-11 – Mobius One)

- **- Adaptação para CGI no Delphi, aceitando compilar em outras plataformas** (2024-09-10 – Fernando Castelano Banhos)

- **- Wizard CGI para Delphi** (2024-09-09 – Fernando Castelano Banhos)

- *** Atualização de arquivo de fontes de ícones** (2024-09-09 – Mobius One)

- **- Pacote CGI Register** (2024-09-09 – Fernando Castelano Banhos)

- **- Novo Pacote CGI** (2024-09-09 – Fernando Castelano Banhos)

- **- separado pacote PascalRAL em dois: PascalRAL e PascalRALDsgn - Pacote CGI remontando** (2024-09-09 – Fernando Castelano Banhos)

- **+ Unit Compress faltante do pacote principal no Lazarus  - Correção de compatibilidade com Delphis anteriores ao 10.1 com o BufferedFileStream** (2024-09-05 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-09-03 – Fernando Castelano Banhos)

- **- RALCriptoAES.pas -- Removido uses Dialogs** (2024-09-03 – Fernando Castelano Banhos)
  - RALCGIServer.pas
  -- CGI remodelado para Delphi e Lazarus

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-09-03 – Mobius One)

- **- Correção de path no pacote bson pro Lazarus  - Remoção de dependência da LCL no pacote base para Lazarus** (2024-09-03 – Mobius One)

- **- server fphttp nao mostrada janela de autenticacao basic, quando usuario nao estava autenticado** (2024-09-02 – Fernando Castelano Banhos)

- **-- melhorias na velocidade de criptografia e descriptografia** (2024-08-30 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-08-27 – Fernando Castelano Banhos)

- **- RALDBZeosMemTable.pas -- corrição da funcao ZeosLoadFromStream, adicionado param TZMemTable** (2024-08-27 – Fernando Castelano Banhos)
  - corrigido criptografia em cliente e servidor

- **+ Arquivo de pacote faltante** (2024-08-26 – Mobius One)

- **+ Arquivos não inclusos no commit anterior** (2024-08-25 – Mobius One)

- **+ Submódulo BSON  + Pacote StorageBSON** (2024-08-25 – Mobius One)

- **- Correção de commit anterior, faltou incluir RALConsts nos uses** (2024-08-16 – Mobius One)

- *** Correção dos códigos HTTP restantes** (2024-08-14 – Mobius One)

- **-- correção para proteger de AV a funcao TRALJSONValue.AsString** (2024-07-25 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-07-25 – Mobius One)

- **- Remoção de unit inválida em DBPackage** (2024-07-25 – Mobius One)

- **-- correção no carregamento de fielddefs** (2024-07-23 – Fernando Castelano Banhos)

- *** Arquivo de fonte dos ícones novos** (2024-07-23 – Mobius One)

- **+ Ícones dos componentes DBWare novos  * Remodelagem dos ícones para um modelo novo  - Remoção de ícones não utilizados  * Correção dos fontes de ícones para incluir as mudanças  * Correção dos pacotes pra incluir os arquivos novos no Delphi  * Ajuste de descrição dos pacotes Lazarus  * Ajuste de dependências dos pacotes  * Ajuste de pacotes no Lazarus com arquivos novos e ícones novos  - Remoção de groupproj redundante** (2024-07-23 – Mobius One)

- **-- retirado resync do Zeos e Firedac, basta usar sem cacheupdates** (2024-07-21 – Fernando Castelano Banhos)

- **- Correções para funcionar em Delphi - Troca de Tipo QWord para UInt64** (2024-07-16 – Fernando Castelano Banhos)

- **- Limpeza e melhoria das units** (2024-07-16 – Fernando Castelano Banhos)

- **- RALDBBufDataset.pas -- Adicionado propriedade FieldInfo** (2024-07-16 – Fernando Castelano Banhos)
  - RALDBSQLDBLinkReg.pas
  -- Limpeza da unit

- **- Adaptações para compilar em Delphi** (2024-07-14 – Fernando Castelano Banhos)

- **- RALDBSQLCache -- classe criada para substituir TRALQueryStructure** (2024-07-14 – Fernando Castelano Banhos)
  - RALDBBase.pas, RALDBZeos.pas, RALDBSQLDB.pas
  -- renomeado propriedade DriverName para DriverType
  - RALDBModule.pas
  -- adaptacoes para a classe TRALDBSQLCache, substituido a TRALQueryStructure
  - RALDBBufDataset.pas
  -- adaptacoes para a classe TRALDBSQLCache, substituido a TRALQueryStructure
  -- alteração dos metodo OpenRemote para Open
  -- alteração dos metodo ExecSQLRemote para ExecSQL
  -- alteração dos metodo ApplyUpdatesRemote para ApplyUpdates
  - RALClient.pas
  -- diversas funcoes adicionadas parametro AExecBehavior = TRALExecBehavior, afim de trocar a execução pela chamada do metodo
  - RALTypes.pas
  -- Tipo TRALExecBehavior adicionado ebDefault

- **- RALServer.pas -- modificação para ServerStatus aparecer o DefaultPage qdo o mesmo for em vazio** (2024-07-12 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-07-11 – Fernando Castelano Banhos)

- *** Atualização de versão** (2024-07-11 – Mobius One)

- **- RALSwagger -- adicionado propriedade ShowCustomNames, para mostrar os names das rotas (json: summary) -- adicionado propriedade ServersUrl, para adicionar as urls do servidor** (2024-06-28 – Fernando Castelano Banhos)

- **- Wizard Form - Remodelado com Delphi7, para melhor adaptar a Delphi's Antigos** (2024-06-27 – Fernando Castelano Banhos)

- **- Limpeza nos fontes do Wizard - Correção do OpenDirectroy do Wizard Delphi** (2024-06-26 – Fernando Castelano Banhos)

- **- Packages Wizard prontos** (2024-06-26 – Fernando Castelano Banhos)

- **- Wizard Delphi e Lazarus pronto** (2024-06-26 – Fernando Castelano Banhos)

- **- Lazarus Wizard, ainda em Desenvolvimento - RALWEBModule.pas, correções para Lazarus** (2024-06-25 – Fernando Castelano Banhos)

- **- Correções de IFDEFs** (2024-06-24 – Fernando Castelano Banhos)

- **- Ajuste de pacote RALWizard.dpk.  - Ajustes de IFDEFs internos.  * Ajuste de internacionalização de dizeres no wizard form.** (2024-06-24 – Mobius One)

- **- Adaptações do engine CGI, com as modificações realizadas do RALServer** (2024-06-24 – Fernando Castelano Banhos)

- *** Melhoria de documentação do RALServer** (2024-06-18 – Mobius One)

- **+ Documentação de mais units  * Identação e revisão de documentação existente  * Atualização de versão para 0.9.7  * Conversão de strings hardcoded para constantes na RALConsts** (2024-06-10 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-06-09 – Mobius One)

- **- RALRoutes.pas e RALServer.pas -- Removido ContentType da Rota e Response** (2024-05-27 – Fernando Castelano Banhos)
  - RALDBModule.pas
  -- Criado função GetTables e GetFields

- **- RALRoutes.pas -- Adicionado propriedade ContentType no TRALBaseRoute** (2024-05-20 – Fernando Castelano Banhos)
  - RALServer.pas
  -- Adaptação do ContentType da Route para TRALResponse

- **- RALToken.pas -- Melhorias na funcao TRALJWT.IsValidToken;** (2024-05-20 – Fernando Castelano Banhos)
  - RALRoutes.pas
  -- Movida propriedade Callback do TRALBaseRoute para public

- **- RALParams.pas -- Mudanca na funcao GetAsBoolean e SetAsBoolean** (2024-05-17 – Fernando Castelano Banhos)
  - RALServer.pas
  -- TRALModuleRoutes, nova propriedade Domain
  - RALDBModule.pas, RALDBStorageCSV.pas, RALDBZeosMemTable.pas
  -- Limpeza de variaveis nao utilizadas
  - RALSwaggerExporter.pas e RALSwaggerModule.pas
  - Adicionado propriedades SwaggerFile e PostmanFile, para o caso de um swagger modificado
  - Mudança da propriedade Version para SystemVersion, conflito com o TRALComponent
  - Mudança da propriedade Description para SystemDescription
  - Removido propriedade Route e adapatado com a propriedade Domain do TRALModuleRoutes

- **- RALServer.pas -- Mudança na função CanAnswerRoute do TRALModuleRoutes, para SubModule que nao é domain** (2024-05-17 – Fernando Castelano Banhos)
  - RALDBZeosMemTable.pas
  -- Iniciado o ApplyUpdatesRemote
  - RALSwaggerExporter.pas
  -- Adicionado controle de tag do Postman
  -- Adicionado propriedade TermsOfService e License
  -- Retirado propriedade Host
  -- RALSwaggerModule.pas
  -- Adicionado controle de tag do Postman
  -- Adicionado propriedade TermsOfService e License

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-05-13 – Fernando Castelano Banhos)

- **+ ícone do SwaggerModule  * Ajuste dos pacotes para incluir o novo ícone  * Ajuste interno de arquivos de ícones  * Atualização de fontes de ícones** (2024-05-13 – Mobius One)

- **- Novos icones convertidos para svg** (2024-05-13 – Fernando Castelano Banhos)

- **-- Adicionado linhas de favicon do swagger no index** (2024-05-13 – Fernando Castelano Banhos)

- **- remoção dos assets do swagger** (2024-05-12 – Fernando Castelano Banhos)

- **- removidas as rotas SwaggerUI, SwaggerUIBundle e SwaggerUIStandalone, os arquivos serao carregadas diretamente do respositorio do github** (2024-05-12 – Fernando Castelano Banhos)

- **- RALSwaggerModule.pas -- Adaptações para Delphi -- Removido o uso de arquivos resources** (2024-05-12 – Fernando Castelano Banhos)

- **- RALServer.pas -- Adicionado funcionado para validar se o SSL esta habilitado** (2024-05-12 – Fernando Castelano Banhos)
  - RALSwaggerExporter.pas
  -- Adicionado propriedade SwaggerModule e Host
  - RALSwaggerModule.pas
  -- Readaptado como responder somente rotas e arquivos do swagger

- **Merge remote-tracking branch 'remotes/origin/dev'** (2024-05-10 – Mobius One)

- **+ ícones de StorageCSV e StorageXML  * Ajuste de pacotes DBWare para Delphi e Lazarus para incluir o novo componente  * Atualização de ícones Storage para facilitar a distinção entre eles  * Atualização de arquivos de ícones .dcr e .lrs com novo ícone  * Atualização de fontes de ícones** (2024-05-10 – Mobius One)

- **- RALServer.pas -- Adicionado propriedade SubModule, para ler a lista dos sub modulos -- Adicionado funcao GetListRoutes no TRALModuleRoutes** (2024-05-08 – Fernando Castelano Banhos)
  - RALDBStorageCSV.pas
  -- Iniciado o LoadFromStream, ainda nao terminado

- **+ Adição de submódulo brookframework para libsagui  - Ajuste de pacote SaguiRAL no Delphi pra incluir o path da lib externa do libsagui  - Remoção do arquivo libsagui.pas da base do componente para usar da lib externa** (2024-05-03 – Mobius One)

- **- Correção de path em pacote NetHttpRAL e SynopseRAL do Delphi** (2024-05-03 – Mobius One)

- **- Remoção de ícones 16x16  * Atualização de ícones WebModule, DBModule, ícones dos componentes FireDAC e Zeos  * Atualização de arquivos de fontes de ícones dcr e lrs  - Remoção de arquivos de fontes desnecessários  * Atualização de pacotes Delphi e Lazarus das novas units DB  * Correção de import de lrs nas units de registro dos componentes** (2024-05-03 – Mobius One)

- **- RALJSON (.inc) -- Modificação na funcao AsString para nao trazer como a string 'null'** (2024-05-02 – Fernando Castelano Banhos)
  - RALDBStorageJSON.pas
  -- Correção para carregar mais rapido e mostrar no grid

- **- RALDBStorageJSON.pas -- Pequena correção no TRALDBStorageJSON_RAW.ReadFields** (2024-05-01 – Fernando Castelano Banhos)

- **- RALDBStorageJSON.pas -- Correção para procedure ReadRecords do TRALDBStorageJSON_DBWare** (2024-05-01 – Fernando Castelano Banhos)

- **- RALDBStorageJSON.pas -- Pequena Correção no ReadRecords do TRALDBStorageJSON_RAW** (2024-05-01 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-05-01 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-05-01 – Mobius One)

- **- Arrumado Pacote RALDBZeos para Delphi** (2024-04-28 – Fernando Castelano Banhos)

- **- RALClient.pas -- Mudança ExecuteThread, quando ebSingleThread dava erros no Lazarus** (2024-04-28 – Fernando Castelano Banhos)
  - RALDBModule.pas
  -- Refeita o modo de responta do ExecSQL
  - RALDBZeosMemTable.pas
  -- Criado event TRALDBOnError para alertar dos exceptions
  - RALDBSQLDB.pas
  -- Correção de units faltando, usadas do SaveFromStream

- **- Unit renomeada para nome maiusculo** (2024-04-27 – Fernando Castelano Banhos)

- **- Unit renomeada para nome maiusculo** (2024-04-27 – Fernando Castelano Banhos)

- **Update Submodules** (2024-04-27 – Mobius One)

- **- RALDBStorage.pas -- Remoneado variaveis FFieldsNames, FFieldsTypes e FFieldsFounds para FFieldNames, FFieldTypes e FFoundFields -- Adicionado TRALDBStorageBSONLink na constante cStorageLinkClass** (2024-04-27 – Fernando Castelano Banhos)
  - RALDBStorageBSON.pas
  -- Renomeado TRALDBStorageBJONLink pra TRALDBStorageBSONLink
  -- Adicionado RegisterClass no initialization

- **- variaveis FFieldsNames, FFieldsTypes e FFieldsFounds movidas para RALDBStorage.pas** (2024-04-27 – Fernando Castelano Banhos)

- **- RALDBStorageBSON -- Correção para fields nulos** (2024-04-27 – Fernando Castelano Banhos)

- **- Storages Exportando e Importando - RALMIMETypes - adicionado rctAPPLICATIONBSON para content-type BSON** (2024-04-26 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-04-26 – Fernando Castelano Banhos)

- *** Ajuste de dependências de pacotes forçando uso de versão correta** (2024-04-22 – Mobius One)

- **- Correção de tipo (GetInstaledList para GetInstalledList)** (2024-04-22 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-04-22 – Mobius One)

- **- Conversão de pacotes de compressão de Runtime-only para Runtime + Designtime para permitir modularização da compressão no Delphi.** (2024-04-22 – Mobius One)

- **- Correção da propriedade BaseURL, preenchendo com o StringList Editor clicando em Cancel** (2024-04-22 – Fernando Castelano Banhos)

- **- Correções para instalação do pacote PascalRAL em Lazarus, devido adaptações de properties editors BaseURL e CompressType** (2024-04-22 – Fernando Castelano Banhos)

- **- Ajuste de pacote zstd para Lazarus  + Criação de pacote brotli para Lazarus** (2024-04-22 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-04-21 – Mobius One)

- **- Restauração de arquivos faltantes no ajuste de arquivos internos.  - Correção dos pacotes de compressão no Delphi pra adaptar à nova estrutura de arquivos.** (2024-04-21 – Mobius One)

- *** Atualização de submódulo ZSTD** (2024-04-21 – Mobius One)

- *** Ajuste de submódulos.  - Renomeio de pastas internas e ajuste de arquivos.** (2024-04-21 – Mobius One)

- **- RALRoutes.pas -- Adicionado propriedades de AllowURIParams e URIParams, para melhor controle das rotas** (2024-04-21 – Fernando Castelano Banhos)
  - RALCompressZStd.pas
  -- Removida função ZSTDIsLoaded

- **+ Criação de pasta "external" para as dependências externas, os binários ficarão no branch "externals".  * Atualização de groupproj pra forçar dependência caso instale os pacotes fora de ordem.  - Correção de incompatibilidade com Lazarus, ainda precisa determinar a forma de registrar as propriedades no Lazarus.  * Reorganização de arquivos internamente, migrando RALCompressBrotli.pas e RALCompressZStd.pas pras respectivas dependências externas.** (2024-04-20 – Mobius One)

- **- Correção do ContentDisposition, quando for html deve ser inline** (2024-04-14 – Fernando Castelano Banhos)

- **- RALParams.pas -- Adaptação da funcao AppendParamsUri para verificar se o param name ja existe - RALSynopseServer.pas -- Correção do server detectando todas conexoes como 127.0.0.1** (2024-04-14 – Fernando Castelano Banhos)

- **- RALRoutes.pas -- Aumento do peso para Params URI nao identificados** (2024-04-14 – Fernando Castelano Banhos)
  - RALServer.pas
  -- Correção de Free Notification no TRALModuleRoutes
  -- Melhoria na funcao CanResponseRoute do TRALModuleRoutes

- **- TRALClientInfo -- Adicionado propriedade Porta - RALDBStorage -- Movido propriedade CharCase do RALDBStorageJSON para RALDBStorage - RALIndyServer e RALSynopseServer -- Capturado propriedade porta no ClientInfo -- Resolvido captura de cookies** (2024-04-12 – Fernando Castelano Banhos)

- **- Formato do Json Float e Date em conformidade com padroes JSON** (2024-04-11 – Fernando Castelano Banhos)

- **- Limpeza da unit RALDBStorageJSON** (2024-04-10 – Fernando Castelano Banhos)

- **- DBStorage JSON adaptado para exportar em dois formatos** (2024-04-10 – Fernando Castelano Banhos)

- **Novo tipo TRALExecBehavior; Opção de Single ou Multi Thread no TRALClientMT; Opção de Single ou Multi Thread no TRALFDQuery;** (2024-04-05 – Gustavo Souza)

- **Ajustes para mudanças no client;** (2024-04-05 – Gustavo Souza)

- **-- Correção de propriedade IndexUrl faltante** (2024-04-05 – Fernando Castelano Banhos)

- **- Assets multithread convertidos para SVG - Assets server modificados, adicao de sombra** (2024-04-05 – Fernando Castelano Banhos)
  -- RALClient.pas
  - Modificado a propriedade BaseURL para TStringList
  - Removido a propriedade UseSSL
  - Removido procedure SetConnectTimeout, SetRequestTimeout, SetUseSSL, SetUserAgent do TRALClientHTTP
  - Adicionado propriedade IndexUrl para controlar qual das URLs da lista do BaseURL está ativa
  -- RALResponse.pas
  - Adicionado ErrorCode para capturar o erro de Socket na tentativa de conectar com o servidor
  - Reescrita da procedure Clear para limpar o ErrorCode
  -- Clientes Engines modificados para responder ErrorCode no Response
  -- RALCustomObjects.pas
  - Modificado procedure Clear para um procedure virtual

- *** Atualização de pacotes para Delphi 12.1** (2024-04-02 – Mobius One)

- *** Renomeio e correção de ícones de Brook -> Sagui** (2024-03-31 – Mobius One)

- **+ Ajuste de pacotes pra incluir clientes MT** (2024-03-31 – Mobius One)

- **- RALConsts.pas -- Respostas de HTML, modificadas pro padrao HTML5 - RALIndyServer.pas -- Problema de keep-alive resolvido** (2024-03-31 – Fernando Castelano Banhos)

- **- Correção do SaguiServer para testes de jMeter Lazarus Linux** (2024-03-31 – Fernando Castelano Banhos)

- **- Correção das chamadas do Critical Section; - Correção para subir Exception do IndyClient; - Inclusão do AException na procedure TRALThreadClientResponse; - Correção da criação e da chamada do TRALThreadClient; - Remoção do DoExecute e DoResponse do TRALThreadClient; - Criação do OnTerminateThread; - Alteração da resposta do Execute para o OnTerminateThread; - Detonante! Alucinante!** (2024-03-30 – Gustavo Souza)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-03-30 – Mobius One)

- **+ Ícones dos clientes MultiThread  * Ajuste dos fontes de ícones  * Atualização dos arquivos de recurso de ícone** (2024-03-30 – Mobius One)

- **- Adicionando Método OnResponse nas chamadas verbais do cliente multi-thread, ajudando a não clonar o obejto pai** (2024-03-30 – Fernando Castelano Banhos)

- **Melhorias na liberação de memória e multithread; Correção da execução do ExecSQLRemote para drivers que criam parâmetros ao inserir o texto SQL; Correção para retorno de erro no ApplyUpdatesRemote; Remoção de código não necessário; Inserção das quebras de linha que o Mobius tirou :-( ;** (2024-03-25 – Gustavo Souza)

- **- Ajuste de dependências de pacotes** (2024-03-22 – Mobius One)

- **- Início de identação em units DBFireDAC  - Correção de compiler em DBModule (Faltou ;)** (2024-03-19 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-03-16 – Fernando Castelano Banhos)

- **Pequenas correções;** (2024-03-16 – Gustavo Souza)

- **- Ajustes de diretivas de MacOS e POSIX** (2024-03-14 – Mobius One)

- *** Ajuste de Saguiserver para mostrar corretamente o código de versão.  + Mensagem de erro caso não ache a libsagui ou não seja possível carregar.** (2024-03-12 – Mobius One)

- **- Ajustes de IFDEF para Linux e MacOS  - Atualização de versão** (2024-03-12 – Mobius One)

- **- Ajuste de compilação em Lazarus  - Ajuste de pacote sagui no Lazarus** (2024-03-12 – Mobius One)

- **- Setado Engine Sagui sem versão para que a propriedade nao fique em branco** (2024-03-12 – Fernando Castelano Banhos)

- **- Correção da Adaptação do ContentDisposition para Client fpHTTP** (2024-03-12 – Fernando Castelano Banhos)

- **- Correção no processamento de ContentDisposition nos Params - Melhorado e Adaptado os Clients Engines para repassar o valor do ContentDisposition** (2024-03-12 – Fernando Castelano Banhos)

- **- RALParams.pas -- ContentDisposition alterado para inline - RALSaguiServer.pas -- Praticamente finalizado - testes maiores** (2024-03-11 – Fernando Castelano Banhos)

- **- Adicionado ContentDisposition no Request e Response - Modificado EncodeBody e DecodeBody para ContentDisposition - Atualizado Cliente e Server para ContentDisposition** (2024-03-11 – Fernando Castelano Banhos)

- **- Retirado FreeContent dos Response e EncodeBody - Separado Request em Server e Cliente - ContentType e CriptoKey migrados para TRALHTTPHeaderInfo - Engines Server e Clientes funcionais com as atualizações** (2024-03-11 – Fernando Castelano Banhos)

- **-- raldbpackage - Correção dos nomes dos Resourses RALDBStorage -Links -- RALRequest - Adicionado RawStream e RawText (conteudos descriptografas e descomprimidos) -- RALDBSQLDBLinkReg - Corrigido aba de install do componente** (2024-03-11 – Fernando Castelano Banhos)

- **- Correção para Response com apenas um param - Correção do DBZeos para Lazarus** (2024-03-11 – Fernando Castelano Banhos)

- **- Correções de falta de ; em alguns pontos da Params.  * Mudança do Params.Param[Index] para Params.Index[Index]; replicado pra todas as units que o utilizam.  - Ajuste de funções GetAs dos Params para usar valor Def reduzindo código.** (2024-03-11 – Mobius One)

- **Merge branch 'dev'** (2024-03-08 – Mobius One)

- **- Desacoplamento compress** (2024-03-07 – Fernando Castelano Banhos)

- **- Melhorando o compress - desacoplamentos** (2024-03-07 – Fernando Castelano Banhos)

- **- Correção de erros do Decoder do MultiPart** (2024-03-07 – Fernando Castelano Banhos)

- **Correção AsInteger para AsString;** (2024-03-06 – Gustavo Souza)

- **- Correção para exibir o ícone do novo pacote.  - Ajuste nos fontes dos ícones.  - Remoção de arquivos temporários que foram commitados.** (2024-03-06 – Mobius One)

- **- Correção de nomenclatura de pacote RALDBFireDACObjects  + Criação de ícones do novo pacote  * Ajuste de caminhos no fonte para novo pacote  * Ajuste do script de criação de dcr para incluir novo pacote  * Ajuste no groupproj pra incluir novo pacote** (2024-03-06 – Mobius One)

- **Correção de caminho na pkg e uses no pas;** (2024-03-06 – Gustavo Souza)

- **Inclusão do pacote FiredacDAORAL no projeto;** (2024-03-06 – Gustavo Souza)

- **Correção da função Clone;** (2024-03-06 – Gustavo Souza)

- **- Pequena melhoria no Initialize do AES** (2024-03-04 – Fernando Castelano Banhos)

- **- Correção da mudança da funcao SupportedCompressKind movida para classe TCompress** (2024-03-04 – Fernando Castelano Banhos)

- **Finalização do SSL para Indy;** (2024-03-04 – Gustavo Souza)

- **Correções no SSL do Indy; Criação da propriedade Key para uso de certificados criptografados;** (2024-03-04 – Gustavo Souza)

- **- RALIndyServer.pas -- Correção para SSL** (2024-03-04 – Fernando Castelano Banhos)
  - Convertidos assets de icones para SVG

- **- Adaptações do AssignParams para outros motores** (2024-03-04 – Fernando Castelano Banhos)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-03-04 – Fernando Castelano Banhos)

- **Correção do separador do cabeçalho;** (2024-03-04 – Gustavo Souza)

- **- Conversoes de String para Stream, passam a usar a unit RALStream** (2024-03-03 – Fernando Castelano Banhos)

- **Merge branch 'dev'** (2024-03-03 – Mobius One)

- *** Separação de DBWare do pacote principal  + Criação de pacote próprio para componentes DBWare: RALDBPackage  + Criação do pacote para Delphi e Lazarus  * Ajuste de dependências dos pacotes DB para incluir esse novo pacote como dependência  - Correção de função Clone em fpHttpClient  * Conversão de verbos HTTP em cliente para virtuais na base** (2024-03-03 – Mobius One)

- *** atualização de fontes de ícones do Figma  * ajuste de Register para incluir alterações do último commit** (2024-02-29 – Mobius One)

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-02-29 – Mobius One)

- *** reorganização de arquivos de ícones  - correção de nomenclatura de ícones  * atualização de dcr com os ícones do projeto  * atualização de fontes de ícones com as novas mudanças  * correção de pacotes Delphi e Lazarus com as mudanças** (2024-02-29 – Mobius One)

- **- Criado os StorageLinks para facilitar o multithread** (2024-02-29 – Fernando Castelano Banhos)

- **Revert "- Novo Engine Sagui -> em desenvolvimento"** (2024-02-29 – Mobius One)
  This reverts commit bc86bf8e67609eeb80f8f617da5730e073f11ca7.

- **Merge branch 'dev' of https://github.com/OpenSourceCommunityBrasil/PascalRAL into dev** (2024-02-29 – Fernando Castelano Banhos)

- **- Novo Engine Sagui -> Em Desenvolvimento - Classe Storage refeita, aumento velocidade de exportacao - Adaptação no módulo DBFireDAC para exportacao nativa** (2024-02-29 – Fernando Castelano Banhos)

- **- Ajuste de compressão para Lazarus** (2024-02-28 – Mobius One)

- *** ajuste de clone no SynopseClient que faltou no commit anterior  - correção de velocidade de compressão ZLib** (2024-02-28 – Mobius One)

- **- Correção do método Clone do RALClient para NetHttp, FpHTTP** (2024-02-28 – Mobius One)

- **- Novo Engine Sagui -> em desenvolvimento** (2024-02-28 – Fernando Castelano Banhos)

- **Correção do Clone;** (2024-02-28 – Gustavo Souza)

- **- Correção de AV em Clone do RALClient** (2024-02-28 – Mobius One)

- **- Ajuste no Client para trocar o TRALHTTPHeaderInfo para os tipos especializados** (2024-02-28 – Mobius One)

- **- Correção de função clone para permitir modificar o AOwner do componente** (2024-02-28 – Mobius One)

- **- Correção de Clone para permitir herança  * Ajuste de função Clone nos demais clientes Delphi** (2024-02-28 – Mobius One)

- **+ função Clone no RALClient pra retornar uma cópia do objeto** (2024-02-28 – Mobius One)

- **- Correção de pacotes dev** (2024-02-28 – Mobius One)

- **test** (2024-02-27 – Mobius One)

- **Merge branch 'master' into dev** (2024-02-27 – Mobius One)

- **+ ícones do brook framework  * atualização de script de ícones e fonte dos ícones  * identação do RALSynopseServer** (2024-02-27 – Mobius One)

- **- revert da remoção de arquivos do merge com o master** (2024-02-27 – Mobius One)

- **- Remoção de módulo DBWare do master pro dev  * Ajuste do master para lançamento de versão 1.0** (2024-02-24 – Mobius One)

- **+ ícones dos módulos DBModule e WebModule.  * atualização do fonte dos ícones.** (2024-02-24 – Mobius One)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2024-02-24 – Mobius One)

- **- Correção de pacote delphi  - Ajustes de identação  - Ajuste de variáveis primitivas pra usar tipada do projeto** (2024-02-24 – Mobius One)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2024-01-19 – Fernando Castelano Banhos)

- **- RALStorage.pas -- Iniciando simplificacao de tipos - RALWEBModule.pas -- Adicionado propriedade Routes para adicao de novas rotas -- Criado Rotas Default** (2024-01-19 – Fernando Castelano Banhos)

- **- Correção de IFDEFs em MIMETypes** (2024-01-19 – Mobius One)

- **- Ajustes em RALServer de tipos primitivos  - Ajustes de ordenação de métodos  + Ajuste de pacotes DBModule e WebModule no pacote principal Lazarus  - Ajuste de Documentação em RALMD5 e MIMETypes  * Ajuste de diretivas em PascalRAL.inc para sistemas operacionais** (2024-01-19 – Mobius One)

- **- Otimização de recursos para responder** (2024-01-19 – Fernando Castelano Banhos)

- **- Renomeadas as classes -- TRALSubRoutes para TRALModuleRoutes -- TRALDBWare para TRALDBModule** (2024-01-19 – Fernando Castelano Banhos)

- **- Modulo de Web criado** (2024-01-18 – Fernando Castelano Banhos)

- **- troca das propriedades Key e Secret para JSONKey e SignSecretKey** (2024-01-18 – Fernando Castelano Banhos)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2024-01-18 – git)

- **- RALClient.pas -- conserto da propriedade Key para JSONKey do JWT** (2024-01-18 – git)

- **- comentarios nas units de hashes - RALAuthentication.pas -- troca das propriedades Key e Secret para JSONKEY e SignSecretKey** (2024-01-18 – git)

- **- Correção de content-type faltante  - Mudança de funções de string de Tools pra Types por conter alto grau de IFDEF** (2024-01-12 – Mobius One)

- **- Correção de ContentType em vários pontos em que fazia o Response.Answer** (2024-01-11 – Mobius One)

- **- Documentação de mais 4 units  - Ajustes de nomes de função  - Ajustes de CORS, adicionando nova função e concentrando headers padrão em uma única função  - Ajuste de plataforma em todos os pacotes  - Renomeadas algumas funções em RALServer  - Remoção de funções redundantes** (2024-01-03 – Mobius One)

- **- Correção para uso TBufferedFileStream em Lazarus** (2023-12-22 – Fernando Castelano Banhos)

- **- Adicionado novo package UniGUIServer** (2023-12-21 – Fernando Castelano Banhos)

- **- CRC32 convertido para classe TRALHashes** (2023-12-20 – Fernando Castelano Banhos)

- *** Arquivo-fonte de ícones atualizado** (2023-12-14 – Mobius One)

- **Update FormSuggestionEN.yml** (2023-12-11 – Mobius One)

- **- ícones convertidos do frigma para SVG** (2023-12-08 – Fernando Castelano Banhos)

- **- Ajuste de compilação de pacote Lazarus  - Adição de função START e STOP no servidor** (2023-12-03 – Mobius One)

- **+ Criação de pacotes DBWare  + Criação de pacotes DBLink  * Atualização dos fontes de recursos para incluir novos ícones  - Correção de compilação de RALDBWare e RALStorage  * Atualização de versão dos fontes para 0.9** (2023-12-01 – Mobius One)

- *** Ajustes de otimização recomendados pelo Pascal Analyzer Lite  - Remoção de Sender nas rotas  - Correção de parâmetro inválido em ThreadSafe  - Atualização de strings em RALJSON_Delphi.inc e ThreadSafe para StringRAL** (2023-11-30 – Mobius One)

- **- RALStorage.pas -- Melhoria do tratamento na conversão de String para JSON** (2023-11-27 – Fernando Castelano Banhos)

- **- Novo Engine para UniGUI - RALParams.pas -- Pequenas correções de Typo -- Adicionado função para excluir TRALParam** (2023-11-20 – Fernando Castelano Banhos)

- **- Modificação para as units do package DBWare** (2023-11-19 – Fernando Castelano Banhos)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2023-11-10 – Mobius One)

- **- Correção de package do projeto pra incluir units faltantes e modificadas.** (2023-11-10 – Mobius One)

- **- Limpeza na unit RALAuthentication.pas - Adaptações no TRALParam para uso de Streams** (2023-10-29 – Fernando Castelano Banhos)

- **- Retirado tipo TRALOnFastReply para route tipo console** (2023-10-29 – Fernando Castelano Banhos)

- **** (2023-10-29 – Fernando Castelano Banhos)

- **- Correção no SaveToStream, voltando a Position para 0** (2023-10-26 – Fernando Castelano Banhos)

- **- Modificação para o EncodeBody sempre copiar o Stream do Param para um nova Stream** (2023-10-26 – Fernando Castelano Banhos)

- **- Limpeza e identação de fontes em algumas units - RALBase64.pas RALHashes.pas -- Adaptações de TStringStream para TStream** (2023-10-21 – Fernando Castelano Banhos)

- **- Renomeado unit RALCompressLib.pas para RALCompressZLib.pas** (2023-10-15 – Fernando Castelano Banhos)

- *** Ajuste de descrição do pacote Design  - correção de IFDEF em MIMETypes e ajuste para Delphi/Linux  + Métodos Start e Stop no servidor  * Ajustes dos pacotes do Lazarus e Delphi adicionando units faltantes** (2023-10-13 – Mobius One)

- **- Pequenas correções de typo** (2023-10-13 – Fernando Castelano Banhos)

- **- RALRoutes e RALServer.pas -- Adaptações para funcionar no modo CONSOLE - RALTypes.pas -- Adaptações para Testes com Delphi antigos e FRX - RALCripto.pas -- Adaptações trabalhar com Stream (unit RALStream) - RALCriptoAES.pas -- Adaptações para Delphi antigos - RALStream.pas -- Conjunto de funções para trabalhar com Stream** (2023-10-13 – Fernando Castelano Banhos)

- **- Codigo readaptado para funcionar no Delphi** (2023-10-10 – Fernando Castelano Banhos)

- **- RALCriptoAES.pas -- Melhorias de para performance de processamento - RALCripto.pas -- Classe reestruturada** (2023-10-07 – Fernando Castelano Banhos)

- *** Migração de testes para branch 'tests'.** (2023-10-03 – Mobius One)

- **- Melhoria de comentáriop da Types.  - Remoção de TODO em Tools.** (2023-10-03 – Mobius One)

- **- Correção de Tipo amOPTION para amOPTIONS - Correção da função GetAllowMethods da TRALRoute** (2023-10-02 – Fernando Castelano Banhos)

- **- Removida a propriedade AutoGetToken do Cliente e Transferida para AuthenticationClient** (2023-10-01 – Fernando Castelano Banhos)

- **- Alteração do Tipo amOPTION para amOPTIONS** (2023-10-01 – Fernando Castelano Banhos)

- **- Adaptações para funcionalidade CORS** (2023-10-01 – Fernando Castelano Banhos)

- *** Ajuste de TestServer para funcionar com a versão 0.8 em Lazarus e Delphi  - Correção de MimeTypes para Linux** (2023-09-27 – Mobius One)

- **- RALConsts.pas -- Adicionado SupportedCompressKind como padrão dos tipos aceitos - RALTools.pas -- Adicionado funções para converter tipos de Compress aceitos** (2023-09-23 – Fernando Castelano Banhos)

- **- RALRequest.pas -- Propriedade AcceptEncoding enviada para TRALHTTPHeaderInfo -- Propriedade AcceptCompress enviada para TRALHTTPHeaderInfo -- Função HasValidAcceptEncoding enviada para TRALHTTPHeaderInfo** (2023-09-23 – Fernando Castelano Banhos)
  - Retirado compressão 'br' dos Cliente Engines
  - Adicionado AcceptEncoding como header do Response nos Server Engines

- **- RALConsts.pas -- Adicionado pagina de erro RAL415Page - RALRequest.pas -- Criado função HasValidAcceptEncoding, para validar se o AcceptEncoding esta certo - RALCustomObjects.pas -- Criado função HasValidContenEncoding, para validar se o ContenEncoding esta certo - RALCompressLib.pas -- Renomeada classe TRALDeflateCompress para TRALCompressLib** (2023-09-23 – Fernando Castelano Banhos)

- **- Criado Tipo TRALCompressType - Adaptado EncodeBody e DecodeBody para usar TRALCompressType - Propriedade ContentEncoding e ContentCompress do TRALRequest enviadas para TRALHTTPHeaderInfo - Adaptações para os Servers e Clientes para EncodeBody e DecodeBody - Renomeado RALDeflateCompress.pas para RALCompressLib.pas** (2023-09-23 – Fernando Castelano Banhos)

- **- RALClient.pas -- Adicionado propriedade 'Compress' para envio e recebimento comprimidos em deflate - RALParams.pas -- AsString retorno '' mesmo que TRALParam não exista -- AsStream retorna nil mesma que TRALParam não exista -- DecodeBody e EncodeBody adicionado compressão deflate -- OnFormBodyData todos os retornos serão do tipo Body - RALRequest.pas -- Adicionado propriedades ContentEncoding e AcceptEncoding para identificar compress - RALResponse.pas -- Adicionado propriedade Compress para EncodeBody identificar se deve comprimir - RALServer's Engine -- Adicionado propriedades ContentEncoding e AcceptEncoding para identificar compress - RALDeflateCompress.pas -- Novo arquivo para comprimir deflate usando ZLib** (2023-09-23 – Fernando Castelano Banhos)

- *** Atualização pra versão 0.8 com fluxo básico de uso testado e funcionando.  - Correção de pacote lazarus  - Correção de alguns Typos e identação  - Mudança de ContentType padrão para Application/json** (2023-09-22 – Mobius One)

- **Função GetResponseStream nos Clients Engines** (2023-09-21 – Fernando Castelano Banhos)

- **- propriedade SSL removida do TRALServer e transferida para cada Engine Server** (2023-09-18 – Fernando Castelano Banhos)

- **- Adicionado RALResponse nos eventos de OnValidate e OnGetToken** (2023-09-13 – Fernando Castelano Banhos)

- **+ Função AsString em Routes para listar todas as rotas do servidor.** (2023-09-12 – Mobius One)

- **- Correção de Hashes, gerando hash em branco** (2023-09-12 – Fernando Castelano Banhos)

- **- RALRequest.pas -- Correção do propriedade Query, limpando parâmetros de URL - RALRoutes.pas -- Correção no RouteName caso começar com / - RALJSON_Delphi.inc -- Correção de EndLine ';'** (2023-09-12 – Fernando Castelano Banhos)

- **- RALClient.pas -- Correção na função BeforeSendUrl, sobrando um FreeAnNil(vHeader) - RALBase64.pas -- Melhora na função DecodeBase64 - RALJSON_xxx,inc -- Correção do ParseJSON(AStream)** (2023-09-11 – Fernando Castelano Banhos)

- **- RALAuthentication.pas -- Correção do ContentType na geração token JWT - RALClient.pas -- GetURL adicionando parâmetros de Query -- Correção AutoGetToken = false nao inseria Header de Authentication - RALParams.pas -- correção AssignParamsText estava adicionando & no fim - RALSynopseClient.pas -- correção de para receber o resultado dos envios - RALBase64.pas -- correção para Decode de tamanho não múltiplo de 4 - RALJson.pas -- Correção de Diretiva - RALJSON_Delphi.inc -- Função as AsString de Object e Array vinha como nula** (2023-09-11 – Fernando Castelano Banhos)

- **- Correção para instalar em Lazarus** (2023-09-10 – Fernando Castelano Banhos)

- **- Correções de RawBody para Body - RALClient.pas -- Adicionado propriedade AutoGetToken -- Alterado valores ConnectTimeout e RequestTimeout, antes estavam como zero** (2023-09-10 – Fernando Castelano Banhos)

- **- RALAuthentication -- Mudança nas funções GetHeader, recebendo todos parâmetros para adicionar header -- OnValidade do JWTToken melhorada (agora recebe o payload) - RALClient.pas -- Mudança nas funções GetHeader, recebendo todos parâmetros para adicionar header - RALParams.pas -- Mudança na função IsNilOrEmpty -- Adicionado função Size - RALToken.pas -- Melhorada funcão ValidToken do TRALJWT** (2023-09-09 – Fernando Castelano Banhos)

- **+ Ícone em formato .ico  * Correção de erros de compilação do pacote  + Função IsNilOrEmpty nos Params  * Renomeado ParamByName para Get em Params  + Função ParamByName em Request e Response através do ancestral comum na CustomObjects  + Funções Answer em Response para facilitar resposta de arquivos e stream  * Ajustes diversos de identação** (2023-09-06 – Mobius One)

- **+ Adição de ícones de DigestAuth para server e client Delphi e Lazarus.** (2023-08-22 – Mobius One)

- **- correção para HTTPS funcionar** (2023-08-17 – Fernando Castelano Banhos)

- *** Ajustes de identação  + Property Engine adicionada ao servidor  + Property ServerOptions  - Teste de utf8 no delphi que causa AV no motor Synopse** (2023-08-15 – Mobius One)

- **- adicionado bloqueio de tentativas para requisições de path transversal** (2023-08-15 – Fernando Castelano Banhos)

- *** Ajuste de identação  + Tratamento de Path Transversal** (2023-08-15 – Mobius One)

- **- correção de identações** (2023-08-15 – Fernando Castelano Banhos)

- **- RALClient - readaptações de funções de verbos para melhor compreensão - Clientes - Retorno do Servidor para o Response com Body e Headers** (2023-08-14 – Fernando Castelano Banhos)

- *** Ajustes de ícone splash e help para o melhor atual  * Melhoria de código, texto hardcoded movido para Consts  + Ícones com tamanhos diferentes usados para gerar o arquivo .bmp** (2023-08-13 – Mobius One)

- **-** (2023-08-12 – Fernando Castelano Banhos)

- **- RALJSON simplificado e adaptado para D7 ou maior - RALToken adaptado com as mudanças do novo RALJSON - RALClient adaptado com as mudanças do novo RALJSON - RALfpHTTPServer corrigido funcao HTTPMethodToRALMethod - Adicionado novos Types - TRALJSONType** (2023-08-12 – Fernando Castelano Banhos)

- *** Ajuste de posição da informação de versão do componente no splash e no about** (2023-08-12 – Mobius One)

- **- Correção de função RALHTTPDecode** (2023-08-11 – Mobius One)

- *** Otimização de MIMETypes  + Função HTTPDecode  - Correção de diretivas no PascalRAL.inc** (2023-08-11 – Mobius One)

- **+ Função para buscar extensão baseado no contenttype em RALMIMETypes** (2023-08-11 – Mobius One)

- *** Ajuste Layout servidor Lazarus de teste  - Correção de charset no lazarus  - Correção de função .AsString em Params para ser compatível com Lazarus** (2023-08-08 – Mobius One)

- *** Ajuste de ContentType pra incluir charset na resposta** (2023-08-07 – Mobius One)

- **- Correção para Server Indy - encode UTF8** (2023-08-07 – Fernando Castelano Banhos)

- **- Correção de charset para Delphi, encode UTF8** (2023-08-07 – Fernando Castelano Banhos)

- *** Conversão de vários arquivos do projeto para UTF8.  * Removido record Helper de string e modificado constante de MIMETypes pra simplificar o código.  * Ajuste de identação em algumas units.  * Ajuste de layout em servidor de teste para incluir novas funcionalidades de teste.** (2023-08-07 – Mobius One)

- *** Renomeado RespCode para StatusCode  + Função AsString em Params para listar todos os parâmetros  * Ajuste de identação** (2023-08-07 – Mobius One)
  + criação e configuração de rotas em uma classe separada no servidor de testes
  + criação de layout final em ferramenta de testes

- **+ Método sobrecarga para facilitar criação de autenticação básica  * Modificação de classe-base dos clientes para teste de stress  * Mudança de layout de teste de stress para novas funcionalidades do teste  + Classe base para os clientes RAL do Delphi** (2023-08-02 – Mobius One)

- **+ Criação de README multiidiomas** (2023-08-02 – Mobius One)

- **Update README.md** (2023-08-02 – Mobius One)

- **- Corrigido Send do ClientNetHTTP - Removido SetSSL do ClientNetHTTP (sem necessidade)** (2023-08-01 – Fernando Castelano Banhos)

- *** Ajuste de pacote NetHttp pra instalação.** (2023-07-31 – Mobius One)

- *** Ajuste em pacote NetHttpRAL, criação do register.  * Ajuste de uses em projeto principal do Delphi.  * Ajuste de uses no projeto do Lazarus.  * Ajuste de espaçamento.** (2023-07-31 – Mobius One)

- *** Ajustes de pacotes Lazarus  * Ajustes de pacotes Delphi  + Pacote inicial Synopse ainda não funciona, arrumarei no próximo commit  * Mudanças em algumas units migrando RALMethod para a Request saindo da Consts** (2023-07-31 – Mobius One)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2023-07-31 – Mobius One)

- **+ Teste de server Lazarus** (2023-07-31 – Mobius One)

- **- Adicionado funcionalidades com Indy para methods de HEAD, OPTION, TRACE** (2023-07-29 – Fernando Castelano Banhos)

- **- Comentado parâmetro automáticos de Header e Payload JWT** (2023-07-28 – Fernando Castelano Banhos)

- **- Melhoria na geração do token JWT - Ajuste de nomenclatura de funções - Correções de identação - Correção de Typo ExpSegs para ExpSecs - Mudança Typo AcquireParams para AssignParams** (2023-07-28 – Fernando Castelano Banhos)

- **- Corrigido erro de criar metodo OnReply para Routes do RALServer - Corrigido erro de AV ao setar um Description no Route** (2023-07-26 – Fernando Castelano Banhos)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2023-07-26 – Mobius One)

- **- Commit de arquivos iniciais de ferramentas de testes.** (2023-07-26 – Mobius One)

- **- Novos Eventos TRALOnClientTryBlocked e  TRALOnClientWasBlocked no RALServer - Limpeza de Header, Fields, Files, Content após capturar o request nos Servers (motores)** (2023-07-25 – Fernando Castelano Banhos)

- **- SetStatusServer setando para RALDefaultPage quando o mesmo for vazio** (2023-07-23 – Fernando Castelano Banhos)

- **- Função AcquireParams adicionado separator (padrao "=")** (2023-07-23 – Fernando Castelano Banhos)

- **** (2023-07-22 – Fernando Castelano Banhos)

- **+ Variáveis de timeout no Server e Client.  * Ajuste das classes pra adicionar timeout.  - Correção de typos e espaçamento.** (2023-07-22 – Mobius One)

- *** Ajuste de pacote pra incluir as classes novas: RALRequest.pas, RALResponse.pas, RALParams.pas  * Ajuste de identação  * Ajuste de Response pra incluir nova função de resposta  * Ajuste em todo o fonte pra adaptar à nova função  + Páginas padrão de erro na Consts  * Ajuste para responder página padrão nos erros comuns** (2023-07-21 – Mobius One)

- **- Revert propriedades WhiteIPList e BlackIPList para TStringList** (2023-07-20 – Fernando Castelano Banhos)

- **- Adicionado BlackIPList - Melhoria nas funções de bloqueio de clientes (autenticacao)** (2023-07-20 – Fernando Castelano Banhos)

- **- Renomeado BlockList para BlackList - Melhorias nas funções de bloqueios de BruteForce** (2023-07-20 – Fernando Castelano Banhos)

- **- Resolvido dependencias geradas das units RALRequest, RALResponse e RALParams** (2023-07-20 – Fernando Castelano Banhos)

- **- TRALParams, TRALRequest, TRALResponse transferidos para arquivos separados - TRALRoute refatorado com rota simples** (2023-07-20 – Fernando Castelano Banhos)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2023-07-19 – Mobius One)

- **- Conserto de Pacote Synopse para Lazarus** (2023-07-18 – Fernando Castelano Banhos)

- **** (2023-07-18 – Fernando Castelano Banhos)

- *** Atualização de fontes de ícones** (2023-07-17 – Mobius One)

- **- Limpeza de arquivos não usados  * Ajuste de ícones em pacotes do Delphi** (2023-07-17 – Mobius One)

- *** Ajuste de pacote principal no Lazarus  * Atualização de pacote de ícones de autenticação e ajuste do pacote  + Ícones separados entre server e client** (2023-07-17 – Mobius One)

- **+ Adição de arquivo de ícones do fpHttp  * Ajuste de pacote fpHttp para incluir os ícones** (2023-07-17 – Mobius One)

- **- Renomeado arquivo RALlfpHTTPRegister.pas para RALfpHTTPRegister.pas** (2023-07-17 – Fernando Castelano Banhos)

- **- Adicionado variavel Engine para setar o motor do Server (Status Check)** (2023-07-16 – Fernando Castelano Banhos)

- **- Criado Pacote fpHttp para Lazarus** (2023-07-16 – Fernando Castelano Banhos)

- **- Modificado TRALRoute para trabalhar com TStringStream - fpHttpServer 90% finalizado** (2023-07-16 – Fernando Castelano Banhos)

- **- Correções para instalar em Lazarus** (2023-07-16 – Fernando Castelano Banhos)

- **- KeyPairs - Propriedade ficou como read-only - corrigido** (2023-07-16 – Fernando Castelano Banhos)

- **Update README.md** (2023-07-11 – Mobius One)

- **+ Criação dos pacotes para Lazarus  * Ajustes de compilação dos fontes pra instalar no Lazarus  + Adição dos ícones  + Arquivo de recursos Lazarus  + Arquivo de includes pra concentrar as diretivas** (2023-07-10 – Mobius One)

- **+ Adição de ícones de autenticadores  + Adição de pacote design base para Delphi  + Fontes de imagens e assets  * Ajuste interno de tipos, migrando definições de tipo da Base pra Types e da Types pra Consts  * Ajuste do motor Indy com as mudanças** (2023-07-10 – Mobius One)

- **- Modificado ClientIndy (Host e Port) para (BaseURL) - GetJSON e SetJSON do TRALJWTPayload correções de Data para UnixDateTime - Criado Validador de Token JWT** (2023-07-09 – Fernando Castelano Banhos)

- **- Correção de conversao do digest para base64** (2023-07-09 – Fernando Castelano Banhos)

- **- Melhora nas velocidades das funcoes de encode e decode** (2023-07-07 – Fernando Castelano Banhos)

- **** (2023-07-06 – Fernando Castelano Banhos)

- *** Atualização de fontes dos ícones** (2023-07-05 – Mobius One)

- **+ Adição de ícones dos componentes  + Adição de script de geração dos ícones  * Ajustes de descrição dos pacotes  * Ajustes de plataformas disponíveis pro motor Indy** (2023-07-05 – Mobius One)

- *** Reorganização interna dos fontes do Indy** (2023-07-05 – Mobius One)

- **- Adicionado ResponseText e ResponseStream para facilitar Respostas diretas - Melhoria no DisplayName do TRALRoute (melhorando criacao da procedure OnReply pelo Delphi)** (2023-07-04 – Fernando Castelano Banhos)

- **- Reindentação de fontes após CRTL+D - Função VarToBytes formulada usando defines** (2023-07-04 – Fernando Castelano Banhos)

- **+ Classe de MIMETypes  * Substituição de todas as strings hardcoded para o MIMEType correspondente  * Mudança de marcador de posição inicial de string da Consts pra Types  * Ajuste nos fontes do projeto para inserir novas classes  * Ajuste de identação das units  * Mudança de pasta da RALTools** (2023-07-04 – Mobius One)

- **** (2023-07-04 – Fernando Castelano Banhos)

- **- Mudança na nomenclatura do tipo TRALMethod - Melhoria nos Params AsString conversão para Bytes - Criado unit RALTools** (2023-07-04 – Fernando Castelano Banhos)

- **- Mudança dos tipos das variáveis (string e interger) para (StringRAL e IntegerRAL) - Propriedade AllowedMethods e SkipAuthMethods adicionadas no Route - Adicionado novos tipos (TRALMethod, TRALMethods) - Adicionado propriedade Method no TRALRequest** (2023-07-03 – Fernando Castelano Banhos)

- **Update README.md** (2023-07-03 – Mobius One)

- **Update README.md** (2023-07-03 – Mobius One)

- **- Reformatado classes de response e request - Indy (30% completo) -- Funções de Encode e Decode de MultiParts criadas - Indy** (2023-07-03 – Fernando Castelano Banhos)

- **** (2023-07-01 – Fernando Castelano Banhos)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2023-06-26 – Mobius One)

- **+ Definições de ClientInfo e outras propriedades do servidor** (2023-06-26 – Mobius One)

- **Update FormSuggestionPT.yml** (2023-06-24 – Mobius One)

- **Update FormSuggestionEN.yml** (2023-06-24 – Mobius One)

- **Update README.md** (2023-06-20 – Mobius One)

- **+ Adição de ícones de componentes  + Criação de classe para definição de tipos básicos  * Ajuste em todos os fontes pra utilizar o tipo básico** (2023-06-12 – Mobius One)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2023-06-12 – Mobius One)

- **+ Identidade visual do projeto** (2023-06-12 – Mobius One)

- **Update README.md** (2023-06-12 – Mobius One)

- **Update README.md** (2023-06-12 – Mobius One)

- **Merge branch 'master' of https://github.com/OpenSourceCommunityBrasil/PascalRAL** (2023-06-12 – Mobius One)

- **+ Estrutura básica de arquivos** (2023-06-12 – Mobius One)

- **Update LICENSE** (2023-06-10 – Mobius One)

- **Update FUNDING.yml** (2023-05-23 – Mobius One)

- **Initial commit** (2023-05-23 – Mobius One)


### Fixed
- **fix:  - Update submodules** (2025-10-24 – Mobius One)

- **Merge pull request #116 from acgubamg/dev** (2025-10-03 – Mobius One)
  Fixed #115

- **fix: erro de compilação para Lazarus fix: configuração de diretivas para Delphi 13** (2025-08-20 – Mobius One)

- **fix: Correção de parse de www-form-url-encoded para detectar o separador do body diferente do header** (2025-08-11 – Mobius One)

- **fix: Correção de encoding de arquivos do projeto fix: Ajuste de versionamento** (2025-08-08 – Mobius One)

- **- correção de bug (incorrect length check) no Delphi para compressão gzip** (2025-02-25 – Fernando Castelano Banhos)

- **- RALDBStorageJSON.pas, correção de bug de chatset - RALfpHTTPClient.pas, adicionado excecao de readtimeout e connectiontimeout** (2024-11-11 – Fernando Castelano Banhos)

- **- packages raldbpackage.lpk e raldbzeoslink.lpk, altera modo debug para dwarf3 - correção RALDBZeosMemTable.pas, openremote e execsqlremote usando o sqlcache errado** (2024-11-10 – Fernando Castelano Banhos)

- **- correção de bug de param (body) = nil** (2024-11-04 – Fernando Castelano Banhos)

- **- Ajuste de UTF8 na classe de Base64 para corrigir autenticação  - Correção de AV em CriticalSession do ClienteMT Fixed #106** (2024-11-01 – Mobius One)

- *** Ajuste para enviar somente os campos alterados pelo RALDBConnection, contribuição de Endrigo Rodrigues** (2024-10-25 – Mobius One)
  Fixed #100

- **- Correção de ContentDisposition e ContentType no fpHttpServer** (2024-10-09 – Mobius One)
  Fixed #94

- **- Correção de erro #95** (2024-10-09 – Mobius One)
  Fixed #95

- **- correção de bug na funcao GetQueryClass, melhora no loop** (2024-09-10 – Fernando Castelano Banhos)

- **- Reorganização de ícones de componentes (ajuste final)  * Atualização de caminhos de arquivos de ícones  * Melhoria de debug no script gerador de .dcr  + Ícone de componente CGIServer  * Ajuste de versionamento de pacote pascalraldsgn.lpk  * Atualização de dcr e lrs** (2024-09-09 – Mobius One)

- *** Ajuste de identação em algumas Units  - Correção de strings Hardcoded convertendo para constantes.  * Ajuste de IFDEF em SQLCache.** (2024-07-23 – Mobius One)
  Fixed #81

- *** Renomeio de vários arquivos e pastas padronizando o pacote.  * Ajuste dos pacotes Delphi e Lazarus para incluir os arquivos modificados.  * Atualização de documentação pasdoc.** (2024-07-23 – Mobius One)
  Fixed #80
  Fixed #82
  Fixed #83
  Fixed #85

- **- RALJSON_FPC.inc -- correção de bug caso Self = nil** (2024-07-05 – Fernando Castelano Banhos)
  - RALPostmanExporter.pas
  -- correção para não exportar o usuario e senha para o Postman

- *** Atualização de fontes de ícones  * Atualização de arquivos de documentação  * Documentação de alguns fontes  * Alteração de módulo Web incluindo rota padrão '/' para evitar problema de loop infinito** (2024-06-09 – Mobius One)
  Fixed #75

- *** Ajuste de versionamento** (2024-05-10 – Mobius One)
  Fixed #42
  Fixed #44
  Fixed #64
  Fixed #74

- **- Correção de bug para Auth Basic, não aparecia o WWW-Authenticate** (2024-05-10 – Fernando Castelano Banhos)

- **- RALStorageBIN -- Correção de bug para escrita e leitura de Blobs** (2024-04-27 – Fernando Castelano Banhos)
  - RALStorageBSON
  -- Correção de bug para escrita e leitura de Blobs
  -- Reformulado WriteRecords e ReadRecords

- **- RALAuthentication.pas -- Modificação caso a propriedade ExpSecs = 0 o token gerado nao expirará** (2024-04-22 – Fernando Castelano Banhos)
  - RALRequest.pas
  -- Modificação na propriedade Query, faltou um fixroute
  - RALServer.pas
  -- Object TRALCORSOptions, removido propriedade Enabled, redundancia com amOPTIONS da rota
  -- ProcessCommands readaptado para validar autenticacao
  -- ProcessCommands adaptado para responder CORS

- **-- Correção de bug (falta de then)** (2024-04-21 – Fernando Castelano Banhos)

- **- RALRoutes.pas -- Possibilidade de poder a rota responder somente por uma rota completa** (2024-04-17 – Fernando Castelano Banhos)
  - RALWEBModule.pas
  -- Melhoria na CanResponseRoute, localizando somente rotas iguais
  - RALTools.pas
  -- Correção no fix route, retirando a ultima '/'

- **Update advanced_issue_labeler.yml** (2024-04-12 – Mobius One)

- **Update FormBugReportPT.yml** (2024-04-12 – Mobius One)

- **Update FormBugReportEN.yml** (2024-04-12 – Mobius One)

- **- Atualização de Versão  * Ajuste de constantes Hardcoded em Multipart para constantes nomeadas na RALConsts  * Mudança da constante DEFAULTBUFFERSTREAMSIZE da Types pra Consts  * Adição de uses da Consts em units que antes utilizavam somente a Types para a DEFAULTBUFFERSTREAMSIZE  * Mudança de Linebreak Hardcoded (#13#10) para constantes equivalentes  * Definida constante padrão HTTPLineBreak (#13#10) em conformidade com RFC 7578 e RFC 2616  * Documentação da unit RALMultipartCoder e RALMD5** (2024-04-02 – Mobius One)
  Fixed #42
  Fixed #44

- **Update issue_labeler.yml** (2024-04-01 – Mobius One)

- **Update FormBugReportPT.yml** (2024-04-01 – Mobius One)

- **Update FormBugReportEN.yml** (2024-04-01 – Mobius One)

- **Update FormBugReportEN.yml** (2024-04-01 – Mobius One)

- **- Correção de bug do Active para SaguiServer** (2024-03-31 – Fernando Castelano Banhos)

- **Update FormBugReportEN.yml** (2024-03-29 – Mobius One)

- **Update FormBugReportPT.yml** (2024-03-29 – Mobius One)

- **- corrigido bug do Cliente NetHTTP ao preencher headers - RALDBModule medificacao na leitura/identificacao de Header (MimeTypes)** (2024-03-19 – Fernando Castelano Banhos)

- **- corrigido bug de charset no RALMultipartCoder - dbware funcionando opensql para vcl** (2024-03-18 – Fernando Castelano Banhos)

- **- Ajuste de identação em Params e MultipartCoder  + Diretiva de debug no .inc** (2024-03-16 – Mobius One)

- **- Correção de Bug fatal descoberto pelo @tempraturbo de calculo quebra no multipart** (2024-03-15 – Fernando Castelano Banhos)

- **- Corrigido Bug de RTL ao adicionar TRALSaguiServer na tela - Adicionado propriedade LibPath para setar a dll do Sagui** (2024-03-12 – Fernando Castelano Banhos)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- **Update workflow-issue.yaml** (2024-03-08 – Mobius One)

- *** Documentação de mais algumas classes Fixed #45 Fixed #46 Fixed #52** (2024-03-08 – Mobius One)

- *** Documentação de classes  * rearranjo de classes seguindo o padrão de código** (2024-03-07 – Mobius One)
  Fixed #43
  Fixed #46
  Fixed #47
  Fixed #48
  Fixed #49
  Fixed #50
  Fixed #51

- **- Correção de bug - setando Key para SendUrl nos Clients** (2024-03-04 – Fernando Castelano Banhos)

- **- Conserto de bug para headers de motor AssignParams** (2024-03-04 – Fernando Castelano Banhos)

- **- Correção de Bug Cripto AES - Key Empty - Adaptações no database para compilar no Lazarus** (2024-03-03 – Fernando Castelano Banhos)

- **- Correção bug AppendParamsText - não carregava o último parâmetro** (2024-02-23 – Fernando Castelano Banhos)

- **- RALJson.pas -- Correção de Bug $ENDIF - RALServer.pas -- Melhoria para TRALSubRoutes, para identificar a rota - RALWEBModule.pas -- Processamento da rota melhorada** (2024-01-19 – Fernando Castelano Banhos)

- **- Documentação de algumas units  - Ajustes de definições de Diretivas centralizado na PascalRAL.inc  - Ajustes de funções Answer pra versão simplificada só com o StatusCode  - Redução de complexidade ciclomática da nova função Answer  - Limpeza de uses em RALCompressZLib  - Ajustes em units de JSON reduzindo total de IFDEFs** (2024-01-18 – Mobius One)
  Fixed #37
  Fixed #38
  Fixed #40
  Fixed #41

- **- Ajuste de nome de funções em RALCripto  - Correção de novos nomes nas units que as utilizam  + Documentação de novas units** (2024-01-04 – Mobius One)
  Fixed #35
  Fixed #36
  Fixed #39

- **- Revisão e documentação de mais units** (2023-12-29 – Mobius One)
  Fixed #27
  Fixed #28
  Fixed #29

- **- Início de ajustes de documentação  - Ajuste de ordem de pacotes no groupproj  - Correção de descrição do pacote UniGUI  - Otimização de RALStorage reduzindo complexidade ciclomática da função WriteField** (2023-12-28 – Mobius One)
  Fixed #26

- **Update FormBugReportPT.yml** (2023-12-11 – Mobius One)

- **Update FormBugReportEN.yml** (2023-12-11 – Mobius One)

- **- RALBase64.pas -- Aumento do Buffer de Leitura e Escrita do Base64 - RALStream.pas -- Correção de bug na Função SaveStream** (2023-10-29 – Fernando Castelano Banhos)

- **- Limpeza na unit RALUrlCoder.pas - Correção de Bugs no unit RALCGIServerDatamodule para Lazarus** (2023-10-29 – Fernando Castelano Banhos)

- **- Corrigido bug no SaveToStream** (2023-10-26 – Fernando Castelano Banhos)

- **- RALSynopseServer.pas -- Resolvido bug de compressão devido erros de TStringStream - RALCompressZLib.pas -- Compress e Decompress convertidas para TMemoryStream** (2023-10-15 – Fernando Castelano Banhos)

- **- RALParams.pas -- Nova função de SaveToFile - RALMultipartCoder.pas -- Correção bug de processamento de multipart de arquivo maior de 4096 bytes** (2023-09-21 – Fernando Castelano Banhos)

- **- RALAuthentication.pas -- Removido "var" dos RALResponse, desnecessário - RALJSON_xxx.inc -- Bug ao adicionar um jsonelement dentro de um jsonobject ou jsonarray gerava leaks devido o object RAL não ser destruído - RALToken.pas -- Correção do TokenDigest - RALTools.pas -- Função VarToBytes readaptada para funcionar em Lazarus e Delphi -- Função FixRoute melhorada, while do fix path transversal desnecessário** (2023-09-13 – Fernando Castelano Banhos)

- **- Correção de bug ao decodificar um body multipart** (2023-09-12 – Fernando Castelano Banhos)

- **- RALSynopseClient.pas -- Adaptações para melhor funcionamento do KeepAlive - RALJSON_lkJSON.inc -- Correção de bug no ParseJSON(Stream)** (2023-09-11 – Fernando Castelano Banhos)

- **- Bug DecodeAuth Synopse** (2023-09-08 – Fernando Castelano Banhos)

- **- Renomeado funcao ParamByNameAndKind para GetKind - Correção de Bug no DecodeAuth do Synopse** (2023-09-08 – Fernando Castelano Banhos)

- **- RALParams - correção de bug no processamento de Headers e Query - RALServer - WhiteIPList e BlackIPList convertidas para SafeList - Clients HTTP - Limpeza de variável de Response** (2023-08-17 – Fernando Castelano Banhos)

- **- correção de bug na busca de rota parcial** (2023-08-15 – Fernando Castelano Banhos)

- **- correção de bug de busca de rotas parciais** (2023-08-15 – Fernando Castelano Banhos)

- **- Corrigido bug de acesso por path transversal ao server** (2023-08-15 – Fernando Castelano Banhos)

- **- Corrigido pequenos bugs para compilar em Delphi** (2023-08-15 – Fernando Castelano Banhos)

- **- Correções de bugs para rodar no Delphi - RALnetHTTPClient.pas -- Adaptações com as novas funções do RALParams** (2023-08-15 – Fernando Castelano Banhos)

- **- RALTypes.pas - correção de circular reference - RALTools.pas - correção de bug na função RALMethodToHTTPMethod - RALUrlCoder.pas - diversos bugs corrigidos** (2023-08-13 – Fernando Castelano Banhos)

- **+ SSL para Server Indy na Aplicação de testes  + AboutInfo no Splash da IDE e no help  * Bump de versão pra 0.7 representando 70% dos recursos prontos e testados  * Mudança de layout em servidor de testes Delphi** (2023-08-12 – Mobius One)
  Fixed #14

- **- Adicionado Header Connection=close para corrigir bug no Postman** (2023-08-07 – Fernando Castelano Banhos)

- **- Ajustes no MultipartDecoder, bug de Leaks - SynopseServer, modificações para testes de stress - fpHTTPServer removido "Connection=close" para aumentar velocidade em testes de stress** (2023-07-31 – Fernando Castelano Banhos)

- **- EncodeBody do Params, codificando x-www-form-urlencoded - Eventos OnRequest e OnResponse do RALServer mudados de posição para fazer logs - fpHTTPServer corrigido bug ao desativar o servidor - SynopseServer correção de AV (Free) - MultipartEncoder corrigido Result da funcao AsStream** (2023-07-30 – Fernando Castelano Banhos)

- **- Correção no IndyServer para reconhecer método PATCH  * Ajuste de identação  * mudanças em teste de stress reduzindo tamanho de unit principal  * Testes iniciais de concorrência de requisições** (2023-07-28 – Mobius One)

- **- RALClient - correção de Request com Authentication e chamada com Header = nil - Adicionado Evento OnReponse no RALServer - Renomeado Evento OnClienteRequest para OnRequest no RALServer - Melhorando limpeza do Headers no RALIndyClient - Bug no RALIndyServer variável de resposta incorreta** (2023-07-26 – Fernando Castelano Banhos)

- **- Corrigido Bug de DecodeBody (ASource = nil) - Mudança na constante do SetEngine INDY (antes gsIdProductVersion para gsIdVersion)** (2023-07-26 – Fernando Castelano Banhos)

- **- Adicionando Engine nos clients - Adicionando propriedade UserAgent nos clients - Bug de Header no Synopse - corrigido** (2023-07-23 – Fernando Castelano Banhos)

- **- Conserto de Bug no ResponseStream e ResponseText (RALResponse.pas) - IndyServer AppendParams CustomHeader - corrigido - DelimitedText Synopse Server retirado** (2023-07-23 – Fernando Castelano Banhos)

- **- Correção de Bug de bloqueio de clientes** (2023-07-20 – Fernando Castelano Banhos)

- **- Adicionado FileName nas propriedades SSL do TRALfpHttpServer - Correção de Leaks (assim q fechava a aplicação) para TRALfpHttpServer - Bug de Leaks resolvido com FreeContentStream para Indy** (2023-07-18 – Fernando Castelano Banhos)

- **- Correção de bug de duplicidade de Uses** (2023-07-17 – Fernando Castelano Banhos)

- **- Correção de Bug (ao pegar o Item o mesmo era excluído)** (2023-07-16 – Fernando Castelano Banhos)

- **- Correção de pequenos bugs de inheriteds** (2023-07-09 – Fernando Castelano Banhos)

- **- Pequenos Bugs de MultiPart** (2023-07-05 – Fernando Castelano Banhos)

- **- Bug na funcao BytesToString** (2023-07-04 – Fernando Castelano Banhos)

- **Update FormBugReportPT.yml** (2023-06-24 – Mobius One)

- **Update FormBugReportEN.yml** (2023-06-24 – Mobius One)

- **Update FormBugReportEN.yml** (2023-06-24 – Mobius One)

- **Update FormBugReportPT.yml** (2023-06-24 – Mobius One)

- **Update FormBugReportPT.yml** (2023-06-24 – Mobius One)

- **Update FormBugReportEN.yml** (2023-06-24 – Mobius One)

- **Update FormBugReportEN.yml** (2023-06-24 – Mobius One)

- **Rename FormBugReportPT to FormBugReportPT.yml** (2023-06-24 – Mobius One)

- **Update FormBugReportEN.yml** (2023-06-24 – Mobius One)

- **Rename FormBugReportEN to FormBugReportEN.yml** (2023-06-24 – Mobius One)


### Removed
- **- RALClient.pas -- Correção para Compilar em Delphi** (2024-07-21 – Fernando Castelano Banhos)
  - RALDBConnection.pas
  -- Implementado funções para gerar SQL de insert, update, delete a partir dos Fields
  - RALDBBufDataset.pas
  -- Adicionado propriedade UpdateMode
  -- Condicoes para CacheSQL a partir de um SQL construido automaticamento pelo Connection

- **- RALClient.pas -- implementado Metodos (Get, Post, Put, Delete, Path) no modo single, onde podemos ter o RALResponse apos a chamada do métido** (2024-07-20 – Fernando Castelano Banhos)
  - RALDBTypes.pas
  -- implementado classe TRALDBInfoTable e TRALDBInfoTables, para ler os JSON do GetTables do TDBModule
  - RALDBZeos.pas
  -- modificações para os fields nao ficarem como readonly e required como false, devido o uso de TZReadOnlyQuery
  - RALDBBufDataset.pas
  -- Diversas funcoes movidas para a class TRALDBConnection
  -- retirada das propriedade client e modulename, movidas para TRALDBConnection
  -- implementado propriedade UpdateTable para ajudar no UpdateSQL
  - RALDBSQLDBLinkReg.pas
  -- implementado a property editor TRALBufDatasetTables, afim de listar as tables do servidor
  - RALSynopseServer.pas
  -- revolvido bug de AV, qdo finalizava a aplicacao
  - RALDBConnection.pas
  -- implementado TRALDBConnection, um conector cliente para se comunicar com o DBModule

- **Delete .github/ISSUE_TEMPLATE/issue_labeler.yml** (2024-04-01 – Mobius One)

- **Delete .github/workflows/workflow-issue.yaml** (2024-03-26 – Mobius One)

- *** reorganização de pacotes em uma estrutura mais escalável e intuitiva  * Ajustes em todos os pacotes com os novos caminhos em Delphi e Lazarus  + Pacote do SaguiRAL que encapsula a libsagui para motor de dados do servidor compatível com Delphi e Lazarus  * Ajustes no motor Sagui removendo as strings Hardcoded e movendo para a RALConsts podendo ser traduzidas  + Arquivo de ícones em recurso para Delphi e Lazarus  * Correção do arquivo de Registro do motor Sagui** (2024-03-12 – Mobius One)

- **- correção de merge com branches/Dev para remover pacotes DBWare  * Melhorias de cliente e função clone  - correção de lentidão de resposta do server** (2024-03-03 – Mobius One)

- **- Ajuste de pacotes adicionando units faltantes Delphi e Lazarus  - Ajuste de pacote SynopseRAL removendo gambi  + Ajuste de dependências no groupproj  * Pequenos ajustes de identação no pacote  + Checkbox pra realizar log no servidor de testes do Delphi  * Ajuste de servidor de testes Lazarus pra ficar semelhante ao Delphi  - Header padrão connection=close removido do motor synopse** (2023-08-17 – Mobius One)

- **- RALParams.pas - implementado adicionar params de URI - RALRoutes.pas - implementado busca de rotas parciais - RALServer.pas - implementado rotas parciais convertidas para paramsURI - RALTools.pas - FixRoute removendo '.' de rotas (segurança)** (2023-08-15 – Fernando Castelano Banhos)

- **- Correção de abstract error ao remover componente Authentication** (2023-07-13 – Fernando Castelano Banhos)

- **Delete FUNDING.yml** (2023-07-03 – Mobius One)

