# Plano: autenticação Digest e OAuth2 (branch `1.3-plugin`)

Escrito em 25/09/2026, a partir de uma análise do código de `dev` e de `1.2-plugin`
(o código de autenticação é o mesmo nos dois, com os mesmos defeitos). É o lugar
para retomar este trabalho: diz o que foi decidido, o que existe hoje, em que ordem
fazer e quando cada etapa está pronta.

## Decisões já tomadas

1. **Branch `1.3-plugin`** (checkout em `PascalRAL-1.3-plugins`). Muda muito do núcleo e
   deve passar de um mês. O `dev` virou a versão 1.2 e sai antes disso; uma
   implementação parcial lá atrapalharia o lançamento. O antigo `1.2-plugin` foi todo
   incorporado ao `dev`, e o `1.3-plugin` nasceu do `dev` em `0e5db09`.
2. **A autenticação passa a ser feita por plugins.** A estrutura de plugins e
   módulos (Tarefa 2 de `plano_execucao_novos_recursos.md`, na pasta `.agents` do
   `dev`) substitui os `if`s encadeados de `TRALServer.ProcessCommands`. Digest e
   OAuth2 nascem como plugins, e Basic/JWT migram para o mesmo contrato.
3. **OAuth 1.0a sai.** OAuth2 (RFC 6749) não é uma extensão do OAuth 1.0a
   (RFC 5849): são protocolos diferentes, sem compatibilidade entre si. O 1.0a
   assina cada pedido com HMAC; o 2 se apoia em TLS e em tokens bearer. Como o 1.0a
   não faz parte do 2, não será implementado, e as classes que existem dele são
   removidas (ver Etapa 1).
4. **Ordem dos papéis do OAuth2: escolha técnica** (abaixo). Nada entra no pacote
   antes de estar testado de ponta a ponta, conforme os critérios de "pronto" de
   cada etapa.

## O que existe hoje (para não reescrever às cegas)

Nenhum componente de Digest/OAuth está na paleta: `RALRegister.pas` registra só
Basic e JWT.

**Transversal**
- `TRALServer.DecodeAuth` só reconhece `Basic` e `Bearer`: `Digest` e `OAuth` chegam
  como `ratNone`.
- `TRALClientHTTP.BeforeSendUrl` chama `SetAuthHeader` **antes** de aplicar
  compressão e cripto ao corpo. Qualquer assinatura sobre o corpo é calculada sobre
  bytes diferentes dos enviados.
- `TRALClientHTTP.SetAuthToken` e `ResetToken` decidem por uma cadeia de `is`, e
  `ResetToken` só sabe limpar o JWT.

**Digest** (`TRALDigest`/`TRALDigestParams` em `RALToken.pas`; `TRALClientDigest`
e `TRALServerDigest` em `RALAuthentication.pas`)
- `TRALServerDigest` está vazio.
- `TRALDigest.GetHeader`:
  - `cnonce` = hash do `nc` (previsível; deveria ser aleatório);
  - a lista de `qop` oferecida vai inteira no cabeçalho, e o cálculo escolhe por `Pos`;
  - falta `algorithm=`; `userhash` e `charset` são ignorados;
  - numa exceção, libera o resultado e devolve um ponteiro solto;
  - `vHash` sem inicializar para um algoritmo desconhecido.
- `TRALClientDigest.SetAuthHeader`:
  - valores com `EncodeURL`, mas o Digest usa quoted-string, então espaço ou acento
    quebram;
  - `nc` incrementado só numa cópia, então todo pedido sai com `nc=00000001`.
- `IsAuthenticated` exige `opaque`, que é opcional (loop de desafio contra servidor
  sem `opaque`); `stale=true` nunca renova; `GetEntityBody` vaza o stream.
- `SetTokenDigest` faz um pedido extra, sem corpo, ao endpoint real só para obter o
  desafio.

**OAuth 1.0a** (a remover): `GetSignature` vazio; `Validate` regrava o timestamp com
`Now`; a base string foge da RFC 5849; as rotas `/initialize/` e `/authorize/` do
servidor estão vazias; `IsAuthenticated` do cliente está invertido; `SetTokenOAuth1`
grava o cabeçalho no response.

**OAuth2**: `TRALClientOAuth2`/`TRALServerOAuth2` vazias; `SetTokenOAuth2` é `TODO`.

**Recursos já disponíveis**
- Hashes: MD5, SHA-1, SHA-256, SHA-512/256 (`RALMD5`, `RALSHA1`, `RALSHA2_32`,
  `RALSHA2_64`).
- HMAC e JWT HS256/384/512 (`RALToken`); `RandomBytes` criptográfico (`RALTools`).
- OpenSSL carregável (`RALOpenSSL`, `RALCriptoOpenSSL`).
- Lock compartilhado do autenticador cliente (`TRALAuthClient.Lock`).
- `WWW-Authenticate` com motivo, e `RALChallengeText` para ASCII (vem do `dev`, 25/09).

## Etapa 0: pré-requisitos no 1.3

- **0.1 Trazer o `dev` para o branch: FEITO** (o `1.3-plugin` parte do `dev` em `0e5db09`).
  Era necessário porque as mudanças de autenticação de 25/09 só existiam no `dev`.
  Chegaram com ele: `AnswerChallenge`, `OnRenewToken`, `AddClaim` que substitui,
  `AddCookie` com vários cookies, mensagens nos `.inc` com BOM. Os casos `mig:` do
  orquestrador valem de regressão.
- **0.2 Tarefa 2 (plugins): IMPLEMENTADA, aguardando a verificação do usuário.**
  `src/base/RALPlugin.pas` + `plugins/RALSecurity.pas`, `RALCORS.pas`,
  `RALContent.pas`; o `ProcessCommands` virou dois laços (plugins, depois módulos,
  com `TRALBaseModule` para as rotas do servidor e a página de status). Servidor
  sem plugin funciona; `CompressType`, `CriptoOptions`, `MaxRequestSize`,
  `JSONBodyToParams`, `CORSOptions` e `Security` saíram do `TRALServer` (quebra de
  compatibilidade do 1.3). A rota é resolvida uma vez (`FindRoute`) e fica no
  request; o plugin de autenticação consulta o `SkipAuthMethods` dela, e sem
  plugin de autenticação nada é pedido. A força bruta ouve o veredito por
  `ppAuthResult` dentro de `Host.Authenticate`, antes dos módulos. Detalhes na
  seção Plugins do `CLAUDE.md`. Dos pontos abaixo, falta o do desafio: hoje o
  `Validate` de cada autenticador escreve o próprio `WWW-Authenticate`, e com
  dois autenticadores o 401 leva o do último que recusou - juntar os desafios
  é da Etapa 1. O que a autenticação pedia da Tarefa 2:
  - **Antes do roteamento:** um plugin de autenticação lê as credenciais do seu
    próprio esquema (substitui o `DecodeAuth`) e responde às próprias rotas (token
    endpoint etc.; substitui `CanAnswerRoute`/`BeforeValidate` como caso especial
    no `ProcessCommands`).
  - **Depois de resolver a rota:** validar sabendo qual é a rota, para
    `SkipAuthMethods` e para escopos por rota do OAuth2.
  - **Na resposta:** cada plugin contribui com o seu desafio. Com vários plugins
    ativos, o mesmo 401 leva vários `WWW-Authenticate` (RFC 9110 §11.6.1), por
    exemplo Basic e Digest juntos.
  - **Prioridade** interna (a Tarefa 2 sugere 500 para autenticação), depois de
    IP/segurança e antes das rotas.
  - **Compatibilidade:** `TRALServer.Authentication` continua funcionando e passa a
    alimentar a lista de plugins.

## Estado das Etapas 1 a 6 (26/09/2026): implementadas, aguardando a verificação do usuário

Tudo no checkout `PascalRAL-1.3-plugins`, sem commit. Pacotes compilando no
Delphi 13, Seattle e XE2 (`PascalRAL.dpk`) e no Lazarus 4.8 (runtime, design,
fpHTTP, Sagui, CGI, msquic). Casos novos em `RALOrquestrador\suite\CasosAutenticacao.pas`
(define `TEM_AUTH2`, onde `plugins\RALOAuth2.pas` existe). Detalhes de desenho na
seção "Authentication since 1.3" do `CLAUDE.md`.

- **Etapa 1: feita.** `TRALAuthTransport` + `Prepare`/`HandleChallenge` no
  `TRALAuthClient`; a cadeia de `is` saiu do `RALClient`; vários desafios num
  só 401 (`RALAddChallenge`); OAuth 1.0a removido (classes, `TRALOAuthAlgorithm`,
  `TRALOnGetTokenSecret`, `SetTokenOAuth1`, `ratOAuth`). **Desvio:** não há
  gancho `Sign` - o corpo só é codificado dentro do `SendUrl` de cada engine,
  depois do `SetAuthHeader`, e a fronteira multipart é aleatória; assinar o
  corpo como vai no fio exige mexer em todos os engines (fica com o D4).
- **Etapa 2: D1-D3 feitos**, D4 (`auth-int`) não. Vetores MD5 e SHA-256 da RFC
  7616 3.9.1 na suíte. Nonce sem estado (timestamp + HMAC), `stale`, cache de
  `nc`, um desafio por algoritmo. Unidade própria: `plugins\RALDigest.pas`.
  Interoperabilidade (servidor RAL/Indy avulso, 26/09): `curl.exe --digest`
  (8.13, Windows) acerta MD5 e MD5-sess e recusa a senha errada; os dois curl
  desta máquina fazem Digest por SSPI, que **não** responde a SHA-256 (nem
  envia o segundo pedido) - com SHA-256 oferecido primeiro, o curl do Windows
  não autentica, o que é limitação dele. SHA-256 validado com o `requests` do
  Python (2.33): três pedidos no mesmo nonce (nc crescente) e senha errada 401,
  também com MD5 e MD5-sess. Cliente RAL contra `HttpListener` do .NET: não
  feito - o Digest dele é o SSPI do Windows, contra contas do sistema.
  OAuth2 com o curl: token por client_credentials, Bearer aceito e recusado,
  `invalid_client`, introspecção e metadata conferem com as RFCs.
- **Etapas 3 a 6: feitas** em `plugins\RALOAuth2.pas` (emissor, validador e
  cliente no mesmo plugin/unidade, e `TRALOAuth2Loopback`). RS256/384/512 e
  ES256/384 via OpenSSL em `utils\RALJWS.pas`; validador por JWKS e por
  introspecção. Store em memória (`TRALOAuth2Store`) com métodos virtuais para
  persistir. Provedor externo (Keycloak etc.): não testado aqui.

## Etapa 1: contrato de autenticação e remoção do OAuth 1.0a

- **Servidor.** `TRALAuthServer` passa a ser (ou a embrulhar) um plugin de
  autenticação: identificar o esquema, validar, desafiar e servir rotas próprias.
- **Cliente** (não passa pelo `ProcessCommands`). `TRALAuthClient` ganha três
  ganchos virtuais, e a cadeia de `is` sai do `RALClient`:
  - `Prepare`: antes do envio; pode obter o token;
  - `Sign`: depois do `EncodeBody`, com o corpo exatamente como vai no fio;
  - `HandleChallenge`: recebe o 401 e diz se vale repetir.
- **Basic e JWT migram primeiro, sem mudar comportamento.** A prova é o grupo AUTH
  do orquestrador (e os casos `mig:` de JWT) com o mesmo resultado antes e depois,
  nos dois compiladores.
- **Remover o OAuth 1.0a:**
  - `TRALClientOAuth`, `TRALServerOAuth` e `TRALOAuth`;
  - `TRALOAuthAlgorithm` e `TRALOnGetTokenSecret` (só o 1.0a usa);
  - `SetTokenOAuth1` e `ratOAuth`.
  - `AuthType` não é publicado, então a mudança de ordinal do enum não afeta `.dfm`.
  - Commit com `remove` no assunto: o `changelog.yml` testa `remove` antes de `add`.
- Mensagens novas vão para a `RALConsts`, nos três `.inc`.

**Pronto quando:** matriz Delphi e FPC completas sem regressão, pacotes
compilando no XE2, Seattle, Delphi 13 e Lazarus.

## Etapa 2: Digest (RFC 7616, compatível com 2617)

Vem antes do OAuth2: é autocontido, tem vetores oficiais e dá para testar contra
ferramentas que já existem em qualquer Windows.

- **D1. Núcleo (`TRALDigest`):**
  - `cnonce` de `RandomBytes`;
  - escolha de um único `qop` (prefere `auth`);
  - `algorithm=` e `userhash` no cabeçalho;
  - MD5, SHA-256 e SHA-512-256, cada um com a variante `-sess`;
  - parser único de auth-params (quoted-string com escape, RFC 9110 §11);
  - função de verificação para o servidor.
- **D2. Plugin servidor (`TRALServerDigest`):**
  - Propriedades: `Realm`, conjunto de algoritmos (um desafio por algoritmo, o
    preferido primeiro), `NonceLifetime`, `UserHash`.
  - Nonce sem estado: `timestamp + HMAC(segredo do servidor)`. Vencido com
    assinatura válida gera `stale=true`.
  - Cache opcional de `nc` por nonce contra replay, seguro para várias threads.
  - Evento `OnGetCredential(User, Realm, var Password | var HA1)`: guardar o HA1
    dispensa senha em claro.
- **D3. Cliente (`TRALClientDigest`):**
  - `nc` incrementado sob o `Lock` (o autenticador é compartilhado entre clientes);
  - `IsAuthenticated` = há nonce;
  - desafio lido do 401 do próprio pedido, via `HandleChallenge` (acaba com o pedido
    extra sem corpo);
  - `stale` renova sem contar como falha;
  - sem `EncodeURL` nos valores.
- **D4. `qop=auth-int`** (opcional, pode ficar para depois): hash do corpo pelo
  gancho `Sign` no cliente; no servidor exige o corpo como chegou no fio, antes do
  `DecodeBody`, o que cada engine entrega num ponto diferente.

**Pronto quando:**
- Unidade: vetores da RFC 7616 §3.9.1 (Mufasa/"Circle of Life", MD5 e SHA-256)
  nos dois compiladores.
- Orquestrador (Delphi e FPC, todos os pares e transportes):
  - acerto em cada algoritmo;
  - usuário e senha errados;
  - nonce vencido com `stale`;
  - replay do mesmo `nc`;
  - Basic e Digest ativos ao mesmo tempo (dois desafios no 401).
- Interoperabilidade nos dois sentidos, com o que o Windows já traz:
  - `curl.exe --digest` contra o servidor RAL;
  - cliente RAL contra um `HttpListener` do .NET com
    `AuthenticationSchemes = Digest`, subido por PowerShell.

## Etapas 3 a 6: OAuth2 (RFC 6749/6750)

**Ordem escolhida.** Fatias verticais, cada uma testável de RAL para RAL, porque
emissor, validador e cliente se testam uns aos outros; um papel sozinho não se prova.

**Fora do escopo:**
- grant `password`: desaconselhado pela RFC 9700 / OAuth 2.1, e o `/gettoken` do JWT
  já atende quem precisa disso;
- DPoP, mTLS e fluxo implícito.

### Etapa 3: fatia A (`client_credentials` e `refresh_token`)
- **Emissor, no plugin `TRALServerOAuth2`:**
  - endpoint de token (`/oauth/token`, formulário urlencoded, RFC 6749 §3.2);
  - autenticação do cliente por `client_secret_basic` e `client_secret_post`;
  - clientes, segredos e refresh tokens ficam com a aplicação, via eventos;
  - tokens JWT HS256/384/512, reaproveitando `TRALJWT`;
  - resposta de erro padronizada (`error`, `error_description`).
- **Validador**, no mesmo plugin: os próprios tokens, com `iss`, `aud`, `exp`/`nbf` e
  tolerância de relógio, e desafio `Bearer` com `error=` (reaproveita
  `AnswerChallenge`).
- **Cliente (`TRALClientOAuth2`):**
  - `TokenURL` absoluta (o provedor costuma estar em outro host; hoje o cliente só
    monta rota relativa ao `BaseURL`), `ClientID`, `ClientSecret`,
    `ClientAuthMethod`, `Scope`, `Audience`;
  - renovação proativa antes de `expires_in`; no 401, `refresh_token` antes do grant
    completo;
  - erro do provedor vira a mensagem da exceção;
  - mesmo padrão de lock do JWT.

### Etapa 4: fatia B (introspecção e escopos)
- Emissor expõe `/oauth/introspect` (RFC 7662) e, opcionalmente, `/oauth/revoke`
  (RFC 7009).
- Validador ganha o modo introspecção (com cache por token), para tokens opacos ou
  de outro emissor, além do evento `OnValidateToken`.
- Escopo exigido por rota. Quando falta, o desafio sai com
  `error="insufficient_scope"` e `scope=`.

### Etapa 5: fatia C (`authorization_code` com PKCE, RFC 7636)
- **Emissor:** `/oauth/authorize` com evento de login e consentimento (a aplicação
  entrega a página), códigos de uso único e curta duração, PKCE S256 obrigatório.
- **Cliente:**
  - ajudante que monta a URL de autorização (`state`, `code_verifier`,
    `code_challenge`);
  - troca do código por token;
  - para desktop, redirect por loopback (RFC 8252) com um `TRALServer` em
    `127.0.0.1` numa porta efêmera.
- **Teste automatizável:** o orquestrador faz o papel do navegador, com um evento de
  login de teste que aprova sozinho.

### Etapa 6: fatia D (RS256/ES256, JWKS e metadata)
- **Emissor:** assina com RSA/ECDSA via OpenSSL (`RALOpenSSL`), publica `/jwks` e
  `/.well-known/oauth-authorization-server` (RFC 8414).
- **Validador:** busca o JWKS, escolhe por `kid` e guarda em cache. Isto é o que
  permite validar tokens de Keycloak, Entra ID etc.
- Traz dependência de OpenSSL na validação; deve ser opcional (capacidade própria).

**Pronto quando (cada fatia):**
- Unidade:
  - vetor S256 do PKCE (RFC 7636, apêndice B);
  - parse das respostas de token e de erro;
  - tokens vencidos e com `aud` errado.
- Orquestrador (Delphi e FPC, todos os pares e transportes), com emissor, validador
  e cliente RAL:
  - obtenção do token;
  - renovação no 401;
  - refresh token vencido;
  - escopo insuficiente;
  - introspecção de token revogado;
  - fluxo de código com PKCE (código reutilizado recusado, `code_verifier` errado
    recusado).
- Conformidade básica: `curl.exe` pedindo token ao emissor RAL com os parâmetros da
  RFC.
- Provedor externo (Keycloak etc.): verificação opcional, "pulada com motivo" quando
  não configurada.

## Como testar sem quebrar o orquestrador contra o `dev`

- Os casos novos vão numa unit comum às duas matrizes (como `CasosMigracao.pas`),
  por exemplo `CasosAutenticacao.pas`.
- A sonda de capacidades ganha `digest` e `oauth2`: um programa de três linhas que
  compila só onde as classes existem. Contra o `dev`, a capacidade sai "pulada com
  motivo", nunca verde.
- Rodar contra o 1.3 com `orqcli "-delphi=37.0" -ral=<PascalRAL-1.3-plugins>`; o CLI pega o
  primeiro Delphi do registro, e a matriz não compila no Seattle.

## Fechamento de cada etapa

- Pacotes compilando no XE2, Seattle, Delphi 13 e Lazarus; componentes novos
  registrados na paleta (`RALRegister.pas`, `.dcr` e `.lrs`).
- Documentação: CLAUDE.md do 1.3, `recursos.md` da wiki (hoje Digest e OAuth estão
  como "planned 1.1+") e `pasdoc.pds` para units novas.
- Sem commit antes de o responsável verificar.
