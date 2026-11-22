# Plano do migrador de projetos do RAL

Ferramenta que atualiza o **projeto do usuário** quando ele troca de versão do
RAL (1.1 → 2.0, por exemplo), para que o projeto abra e compile sem erro na
versão nova; e que diz, item por item, o que não conseguiu fazer sozinha e como
fazer à mão. Irmã do instalador (`PLANO_INSTALADOR.md`, seção 9): o instalador
troca o RAL das IDEs, o migrador troca o código que usa o RAL.

## Pedido do dono (13/10/2026)

> Ao final do processo de instalação/atualização pelo instalador, adicionar uma
> ferramenta extra de atualizar projetos existentes com o RAL prevendo as
> diferenças entre as versões e fazendo a atualização do projeto do usuário
> automaticamente, semelhante ao funcionamento do RESTDW2RAL.

Regras dadas:

- usuário na 1.1 instalando uma versão que quebra o código (a 2.0) roda a
  ferramenta, e ela faz os ajustes para o projeto **abrir sem erros**;
- feita em **Lazarus/FPC**, para rodar em todos os sistemas;
- o ideal é cobrir **100%** das mudanças; no caso muito específico em que não
  der, **dizer ao usuário o que não foi arrumado e explicar o que ele deve
  fazer**.

## O que existe hoje

Conferido em 13/10/2026.

- **O modelo: RESTDW2RAL** (`ral_restdw/ferramentas/conversor`). Um motor
  (`uConversor.pas`, 2148 linhas) e três cascas (console, janela Delphi, janela
  Lazarus) — "mexeu na regra, mexeu para os três". Trabalha byte a byte
  (`RawByteString`) para não estragar a codificação; `.dfm`/`.lfm` por linhas
  (`ConverterDFM`), `.pas`/`.dpr`/`.lpr` (`ConverterPAS`, `AjustarUses`), `.lpi`
  (`ConverterLPI`); simula antes (`Aplicar = False`), guarda `.bak`
  (`Backup`), e avisa por tipo (`TTipoAviso`: renomeado, removido, sem
  equivalente...). O que ele **não** tem e o migrador precisa: as regras são
  código (`cSemEquivalente`, `TTroca`), não dado; troca de palavra por busca de
  texto (`TrocaPalavra`), que não distingue comentário e string; um só "salto"
  (RDW → RAL), sem cadeia de versões; nenhuma verificação de que o resultado
  abre.
- **As quebras estão escritas na wiki**, em tabela:
  `PascalRAL-Wiki/atualizacao.md` (1.0 → 1.1, 1.1 → 1.2, 1.2 → 2.0),
  `migrar-para-2.0.md` (pacotes, pastas e units renomeados) e `plugins.md`
  ("O que mudou da 1.2 para a 2.0": propriedade do servidor → plugin). É o ponto
  de partida do inventário, não a garantia de que está tudo lá.
- **Um gabarito pronto:** o commit `677864d` de `PascalRAL-Samples-2.0` ("exemplos
  no RAL 2.0, com os plugins no lugar das propriedades do servidor") é o porte
  **manual** dos 45 exemplos da 1.x: 71 arquivos. O que ele fez é exatamente o
  que o migrador tem de fazer sozinho:
  - `.dfm`/`.lfm`: saem `CompressType`, `CriptoOptions.*`, `CORSOptions.*`,
    `Security.*` do servidor; entram `object RALCompressPlugin1:
    TRALCompressPlugin` (18 vezes) e `object RALCORSPlugin1: TRALCORSPlugin` (17)
    com `Server = <servidor>` e as opções movidas (`CORSOptions.AllowOrigin` →
    `Options.AllowOrigin`);
  - `.pas`: o campo de cada plugin novo na classe do formulário e a unit no
    `uses`;
  - `.dproj`: `DCC_UsePackage` com os nomes novos (`RALDBFireDACLink` →
    `RALDBFireDACPackage`, `RALBSONStorage` → `RALStorageBSONPackage`,
    `RALWizard` → `RALWizardDsgn`...);
  - `.lpi`/`.lpr`: `raldbzeoslink` → `raldbzeospackage`, e pacotes **novos**
    exigidos porque as units saíram do núcleo (`ralpluginauthpackage`,
    `ralstoragepackage`, `ralmoduleswaggerpackage`).
  - O commit `37a6638` de `PascalRAL-Samples` ("Correção de exemplos Delphi para
    versão 1.1 do RAL") é candidato a gabarito do 1.0 → 1.1 (não conferido).
- **Peças do instalador que servem aqui** (`PascalRAL-Instalador/src/classes`,
  sem LCL): `RALInst.Catalogo` (pacotes de uma versão, lidos de pasta ou de zip),
  `RALInst.GitHub` + `RALInst.Zip` (os fontes de qualquer versão, do cache),
  `RALInst.IDE*` (achar as IDEs para compilar e conferir), `RALInst.Processo`,
  `RALInst.Traducao` (os `.po`), os temas da tela. O recibo do instalador sabe
  de qual versão a IDE veio e para qual foi.
- **O FPC traz um parser de Pascal completo:** `fcl-passrc` (`PParser`,
  `PasTree`), que lê units em modo Delphi e ObjFPC e devolve classes, membros,
  visibilidade, `default` e assinaturas. Serve para extrair a API de cada versão
  do RAL (M0) sem escrever parser próprio. Para **editar** o código do usuário
  basta um tokenizador (M1): o parser seria frágil diante de código que não
  compila.

## Desenho

### A ideia central: cadeia de passos, regras como dado

- Cada quebra do RAL é um **passo** de uma versão para a seguinte
  (`1.0→1.1`, `1.1→1.2`, `1.2→2.0`). Migrar da 1.1 para a 2.0 aplica
  `1.1→1.2` e depois `1.2→2.0`, como migração de banco de dados. Versão nova do
  RAL que quebra algo = um passo novo, sem mexer nos anteriores.
- Cada passo é um **arquivo JSON de regras** com vocabulário **fechado** (o
  mesmo princípio das receitas do instalador: regra desconhecida recusa o
  arquivo inteiro; nenhuma regra executa comando). As regras vêm embutidas no
  executável; um `ralmigracao.json` na raiz da versão de destino do RAL
  acrescenta ou substitui (D-M1), para que uma versão nova traga as próprias
  regras sem release nova do migrador.
- Toda regra tem `id` (`2.0/plugin-cors`), a descrição, o link da wiki e o
  **texto do que fazer à mão** — usado quando ela não pode ser aplicada naquele
  ponto. Assim "100% coberto" quer dizer: toda quebra conhecida ou é feita, ou
  vira um item do relatório com a explicação; nunca some em silêncio.

### Vocabulário de regras

| Ação | Onde age | Exemplo |
| --- | --- | --- |
| `pacote-renomear` | `.dproj` (`DCC_UsePackage`), `.dpk` (`requires`), `.lpi`/`.lpk` (`RequiredPackages`), `.lpr` (unit do pacote no `uses`) | `RALDBZeosLink` → `RALDBZeosPackage` (+ `RALDBZeosDsgn` só onde é design) |
| `pacote-exigir-se-unit` | `.lpi`/`.lpk` | usa `RALAuthentication` → exige `ralpluginauthpackage` |
| `caminho-trocar` | `DCC_UnitSearchPath` do `.dproj`, `OtherUnitFiles` do `.lpi` | `$(PascalRAL)\base` → `core;common;storage...` |
| `unit-renomear` | `uses` de `.pas`/`.dpr`/`.lpr` | `RALIndyRegister` → `RALIndyReg` |
| `identificador-renomear` | código (tokens), `.dfm`/`.lfm`/`.fmx` (`object X: TClasse`) | `TRALSelfSigned` → `TRALSelfSignedPlugin` |
| `propriedade-renomear` | formulário e, quando dá para provar o tipo, código | `TRALSaguiServer.ConnectionLimit` → `MaxConnections` |
| `propriedade-para-componente` | formulário + `.pas` do formulário | `CORSOptions.*` do servidor → `object RALCORSPlugin1: TRALCORSPlugin` com `Server = <servidor>`, `Options.*`, o campo na classe e `RALCORS` no `uses` |
| `valor-padrao-materializar` | formulário | o `.dfm` não grava valor padrão: quando o padrão **mudou** entre as versões, escrever no formulário o valor que o projeto usava, que é a forma de dizer isso na versão nova (D-M5) |
| `propriedade-remover` | formulário | o que não tem equivalente; com valor diferente do padrão, vira item manual |
| `padrao-avisar` | código (tokens) | `TMemoryStream(...Content)` → manual: "leia como `TStream`" |

A condição de cada regra é declarada (tem a propriedade, tem o valor, a classe
é descendente de, a unit está no `uses`) e avaliada pelo motor; não há
expressão livre.

### Como a regra "propriedade → plugin" decide

É a mais importante da 2.0 e a que o porte manual dos samples mostra:

- `CompressType` com qualquer valor → cria `TRALCompressPlugin`: na 1.2
  `ctNone` já queria dizer "o que o cliente pedir", e é isso que o plugin faz
  com `ctNone`. Sem o plugin, o servidor deixaria de comprimir.
- `CriptoOptions.CriptType = crNone` e `Security.Options = []` → nada: não havia
  recurso ligado.
- `CORSOptions.*` → `TRALCORSPlugin` com as opções movidas uma a uma.
- Um plugin por servidor; dois servidores no mesmo formulário ganham dois.
- Nome do componente novo: `RAL<Plugin>1`, `2`... sem colidir com o que existe.
- Servidor criado **em código** (não no formulário) → o plugin é criado em
  código, ao lado da criação do servidor, quando a regra acha a declaração e o
  ponto de criação (D-M4, M5); quando não acha, item manual com o trecho exato a
  acrescentar, já com os valores que estavam lá.

### O que "funcionar na versão nova" quer dizer (D-M5)

O projeto migrado usa o RAL **do jeito que a versão nova pede** e continua
fazendo o que fazia. Três casos:

1. **A forma de usar mudou** (propriedade virou plugin, unit ou pacote mudou de
   nome): o migrador reescreve para a forma nova. É o caso da maioria.
2. **O padrão mudou** (a versão nova faz outra coisa quando nada foi
   configurado): o projeto antigo dependia do padrão antigo sem escrever nada.
   Na versão nova, a forma de pedir aquele comportamento é escrevê-lo; o
   migrador escreve o valor explícito e o relatório diz "o projeto continua
   fazendo X; para adotar o novo padrão Y, faça Z". Exemplo: um servidor criado
   em código na 1.1 aceitava qualquer origem de CORS sem nenhuma linha; na 1.2
   o CORS vem fechado, e a forma de continuar aberto é
   `CORSOptions.AllowOrigin := '*'` (na 2.0, no `TRALCORSPlugin`).
3. **Não dá para fazer sozinho com segurança**: item do relatório com o
   problema, o arquivo e a linha, e como atualizar à mão.

### Código que o migrador escreve

Tudo o que o migrador põe no projeto do usuário (campo de componente, `uses`,
trecho sugerido no relatório) tem de compilar na **IDE mais antiga que o RAL
suporta**, não só na do usuário: Delphi XE e FPC 3.2, em modo Delphi e ObjFPC.
Sem generics, sem `var` inline, sem string multilinha, sem nada que só as
versões novas aceitam — a mesma regra do código do RAL, que é escrito pensando
num porte futuro para o Delphi 7 (D-I1 do `PLANO_INSTALADOR.md`).

### Saber de que versão o projeto vem

1. Vindo do instalador: a versão anterior e a nova estão no recibo e chegam por
   argumento.
2. Sozinho: pistas no próprio projeto, cada uma com a evidência mostrada —
   nomes de pacote no `.dproj`/`.lpi` (`RALDBFireDACLink` é 1.x), propriedades
   que só existiram numa faixa de versões (`CompressType` no servidor é ≤ 1.2;
   `ebSingleThread` aparece na 1.2+). A tabela de "existe em quais versões" sai
   do inventário (M0).
3. Sem pista suficiente: pergunta, com a versão mais provável já escolhida.

### Garantir "abre sem erros" sem abrir a IDE

Depois de aplicar, o migrador **valida** o resultado contra a API da versão de
destino (M0): toda classe de `object X: TClasse` existe e é registrada; toda
propriedade gravada no formulário existe e é `published` naquela classe; toda
unit do RAL no `uses` existe; todo pacote do RAL citado existe no catálogo da
versão. É exatamente o que faria a IDE recusar o formulário ("Property X does
not exist", "Class not found"), conferido antes. Se houver uma IDE na máquina, a
etapa opcional "compilar para conferir" usa o motor do instalador
(`dcc32`/`lazbuild`) contra o RAL de destino.

### Segurança

- **Prévia primeiro**: o que muda, arquivo por arquivo (diff), e a lista do que
  vai ficar para o usuário — antes de gravar qualquer byte.
- **Cópia de segurança é opção do usuário** (D-M3), na tela de aplicar
  (`--backup` na `ralmigra`). Marcada: o migrador guarda num zip ao lado do
  projeto (`_ral-migracao/<de>-<para>-<data>.zip`) todo arquivo que vai mudar,
  e só então grava; o **Desfazer** restaura a partir dele. Desmarcada: grava
  direto no projeto, e a confirmação diz que não há como desfazer. Projeto num
  repositório git com mudanças não commitadas: aviso, nos dois casos.
- **Só para a frente** (D-M8): o migrador recusa migrar para uma versão mais
  velha que a de origem, e diz por quê. Voltar de versão no instalador serve
  para abrir um projeto antigo, que continua na versão antiga, e dar uma
  manutenção rápida — esse projeto não precisa de migração.
- **Fiel ao arquivo**: bytes, codificação (ANSI, UTF-8 com e sem BOM, `#nnn`
  do `.dfm`) e fim de linha preservados; só as linhas que a regra toca mudam.
- **Idempotente**: rodar de novo não muda nada.
- `.dfm` binário (D-M6): o migrador avisa que o formulário precisa estar em
  texto e oferece converter. Aceitou: converte (`ObjectBinaryToText` do FPC) e
  migra junto com o resto. Recusou: aquele formulário não é tocado, e o
  relatório o lista como não migrado, com o motivo.

### Idiomas

O migrador fala os mesmos idiomas do instalador (pt-BR, en-US, es-ES) e do mesmo
jeito: todo texto de tela e de núcleo é `resourcestring`, traduzido em tempo de
execução pelos `.po` embutidos (`RALInst.Traducao`), o idioma do sistema já vem
escolhido, e um `.po` ao lado do executável vale mais (tradutor testa sem
recompilar). Diferenças em relação ao instalador:

- **A `ralmigra` também é traduzida** (a `ralcli` é só em português): aqui o
  produto é o relatório, e quem roda pela linha de comando lê o mesmo texto.
- **Os textos das regras são traduzidos.** No instalador os textos das receitas
  ficaram em português, porque são raros na tela; no migrador a explicação de
  cada item manual **é** a resposta para o usuário. Cada campo de texto da
  regra é um objeto por idioma (`{ "pt-BR": ..., "en-US": ..., "es-ES": ... }`),
  para que um `ralmigracao.json` que chega com uma versão nova do RAL traga as
  traduções junto, sem depender de `.po`. Falta um idioma: usa o en-US, depois o
  pt-BR. O teste das regras embutidas falha se faltar algum dos três.
- **O relatório sai no idioma escolhido** e se basta sozinho: a wiki é só em
  português, então o link vai junto, mas a explicação do que fazer está no
  relatório, no idioma do usuário.

### Forma do produto

Núcleo sem LCL (`RALMig.*`), com duas cascas — janela LCL e `ralmigra`
(console) —, igual ao instalador e ao RESTDW2RAL. Mora no repositório do
instalador, pasta `migrador/`, e usa as units dele (D-M2). **Sai junto de cada
versão nova do RAL**, no mesmo release do instalador, para Windows, Linux e
macOS; a auto-atualização do instalador troca os dois executáveis (F21 do
`PLANO_INSTALADOR.md`), então quem tem o instalador tem sempre o migrador que
conhece a versão mais nova.

## Etapas

### M0 — Inventário das quebras e da API de cada versão

- Ferramenta `migrador/ferramentas/apidiff` (com `fcl-passrc`): para cada versão
  (`v1.0`, `1.1`, a 1.2 quando sair, `2.0`; dos zips do cache do instalador),
  extrai a API do RAL: units, classes e ancestrais, membros `published` com
  tipo e `default`, eventos com assinatura, métodos públicos, classes
  registradas na paleta (`RegisterComponents`), pacotes (do catálogo) e as units
  de cada pacote. Grava um `api-<versão>.json` (vai embutido, para a validação
  e a detecção de versão).
- Compara versões vizinhas e lista **toda** diferença que quebra: unit sumiu ou
  mudou de nome, classe sumiu, propriedade sumiu ou deixou de ser `published`,
  `default` mudou, assinatura de evento mudou, pacote renomeado ou dividido.
- Cruza com as tabelas da wiki. Cada diferença recebe um `id` de regra **ou**
  uma justificativa de por que não quebra projeto de usuário (interno, nunca
  publicado). Diferença sem nenhum dos dois entra na lista de pendentes — que
  no dia a dia é aviso e no rito de pré-lançamento tem de estar vazia (D-M7,
  M9). É assim que "100%" vira coisa que se confere, e não promessa.
- Também entram as quebras **de comportamento** que não aparecem na API (CORS
  fechado por padrão, `ebSingleThread`, 405 no lugar de 404, AES-CBC
  incompatível entre 1.0 e 1.1): vêm da wiki e viram itens de relatório, com o
  que fazer.

**Pronto quando:** `apidiff` gera a API de `v1.0`, `1.1`, `dev` e `2.0`; a
tabela de quebras dos três passos está em `migrador/regras/INVENTARIO.md` com
cada linha ligada a uma regra ou a uma justificativa (as lançadas, 1.0 → 1.1,
fechadas; o passo para a 2.0 pode ter pendentes enquanto ela não sai); e o teste
`tests/inventario` lista toda diferença nova sem nenhuma das duas.

### M1 — Ler e escrever sem estragar

- Tokenizador de Pascal (comentários `{ }`, `(* *)`, `//`, strings com `''`,
  `#nnn`, diretivas `{$...}`, `&` de identificador) que devolve posições no
  texto original — a edição troca só o trecho do token.
- Editor de `uses` (acrescentar, tirar, trocar; respeita `{$IFDEF}` dentro do
  `uses`, `in 'arquivo.pas'` e a quebra de linha que já existe).
- Leitor de formulário em árvore (`object`/`inherited`/`inline`, coleções
  `<item>`, listas `( )`, `{ }` binário) sobre as linhas originais, para poder
  inserir um `object` irmão e tirar propriedades sem reescrever o resto.
- `.dproj`, `.lpi`, `.lpk` por DOM mas gravando só o elemento alterado (o
  `ConverterLPI` do RESTDW2RAL faz por linha — conferir qual dos dois preserva
  melhor o arquivo).
- `.dpk`/`.dpr`/`.lpr` pelo tokenizador.

**Pronto quando:** ler e gravar sem regra nenhuma dá arquivo **idêntico byte a
byte** em todo `.pas`, `.dfm`, `.lfm`, `.fmx`, `.dproj`, `.lpi`, `.lpk`,
`.dpk`, `.dpr`, `.lpr` de `PascalRAL-Samples`, `PascalRAL-Samples-2.0` e das
demos do RDW em `ral_restdw/ferramentas/conversor/demos`; e o tokenizador acha
os identificadores certos num arquivo de teste com o nome dentro de comentário,
de string e de diretiva.

### M2 — Motor de regras, cadeia de versões e prévia

- Leitura das regras (embutidas como RCDATA + `ralmigracao.json` da versão de
  destino), vocabulário fechado, recusa com o nome do campo ou da ação.
- Textos das regras por idioma (desenho, "Idiomas"), com a ordem de reserva
  en-US → pt-BR.
- Cadeia: de `1.1` para `2.0` = `1.1→1.2` + `1.2→2.0`, aplicadas em memória,
  em ordem, sobre o resultado da anterior.
- Detecção da versão de origem (argumento, pistas do projeto, pergunta).
- Prévia: diff por arquivo + lista de itens manuais; nada gravado.
- Pasta com vários projetos (`.groupproj`, grupos do Lazarus) e units fora da
  pasta que o search path do projeto cita (perguntar se entram).

**Pronto quando:** `ralmigra --de=1.1 --para=2.0 <pasta>` sobre os samples 1.x
mostra a prévia sem gravar; a cadeia aplicada passo a passo dá o mesmo que um
passo só escrito à mão para o teste; uma regra com ação desconhecida é
recusada; rodar duas vezes dá zero mudanças na segunda; o teste das regras
embutidas falha quando uma delas não tem o texto nos três idiomas.

### M3 — Regras de projeto e de pacote

- `pacote-renomear`, `pacote-exigir-se-unit`, `caminho-trocar`, `unit-renomear`
  para os três passos (tabelas de `migrar-para-2.0.md` e do inventário).
- Pacote do usuário (`.dpk`) que exige `PascalRALDsgn` só para usar o RAL →
  `PascalRAL` na 2.0, quando o pacote não registra componente (não é
  `{$DESIGNONLY}` e não tem `Register`); senão, item manual explicando o
  `designide`.
- Search path com caminho absoluto para dentro de uma árvore do RAL (sem
  `$(PascalRAL)`): trocar as pastas mantendo a raiz.

**Pronto quando:** sobre a cópia dos samples 1.x (o pai de `677864d`), os
`.dproj`, `.lpi`, `.lpr` e `.dpk` migrados batem com os de `677864d` (a ordem
de `DCC_UsePackage` não conta); e um `.dpk` de teste que exige `PascalRALDsgn` e
`RALDBFireDACLink` sai com `PascalRAL` e `RALDBFireDACPackage`.

### M4 — Regras de formulário

- `identificador-renomear`, `propriedade-renomear`, `propriedade-remover`,
  `propriedade-para-componente`, `valor-padrao-materializar`.
- O componente novo entra no formulário (com `Left`/`Top` livres, sem cobrir
  outro), o campo na classe do `.pas` (seção `published` do designer, como a IDE
  faria) e a unit no `uses` da `interface`.
- Herança de formulário (`inherited`): a propriedade mora no ancestral; a regra
  age lá, e o descendente só perde a propriedade que repetia.
- Frames (`inline`) e DataModules, nos dois sistemas de formulário (VCL, FMX,
  LCL).

**Pronto quando:** os 45 projetos de `PascalRAL-Samples` (1.x) migrados ficam
**equivalentes** a `677864d` — árvore do formulário comparada sem posição e sem
o número no nome do componente novo —, e compilam contra a 2.0 na amostra de
sempre: XE2 (a mais velha daqui; o piso é o XE), Delphi 13 e uma sorteada no
meio; Lazarus com o FPC 3.2.x mais velho, o trunk e o Unleashed (comandos para o
dono, skill `ral-orquestrador`, se a compilação for pela bateria).

### M5 — Regras de código e itens manuais

- `identificador-renomear` no código pelo tokenizador (tipos, constantes,
  classes).
- **Reescrita automática no código** quando a regra consegue provar o que o
  código faz (D-M4): o tipo da variável vem de uma declaração que o tokenizador
  acha (campo do formulário, variável local, parâmetro) e é uma classe do RAL
  ou descendente dela. Exemplo: `Servidor.CompressType := ctGZip`, com
  `Servidor: TRALIndyServer` declarado na mesma unit, vira a criação do
  `TRALCompressPlugin` ligado ao servidor, no mesmo ponto, no estilo da IDE
  mais antiga (desenho, "Código que o migrador escreve"). Aparece na prévia
  como qualquer outra mudança.
- `padrao-avisar`: o que não dá para provar — `TMemoryStream(Param.Content)`,
  escrita em `Body.Content`, propriedade que virou plugin atribuída por uma
  variável de tipo que não se acha, eventos JWT de 1.0 → 1.1 que viraram `of
  object`. Cada um vira item com arquivo:linha, o que mudou, por que, o trecho
  pronto para colar e o link da wiki, no idioma escolhido.

**Pronto quando:** um projeto de teste com cada padrão do inventário sai com o
que é provável reescrito (e compilando no XE2 e no FPC 3.2) e um item manual por
ocorrência do resto, com a linha certa; e nenhum falso positivo no código dos
samples 2.0 (que já está migrado).

### M6 — Validação e compilação para conferir

- Validação offline contra `api-<destino>.json` (classes, propriedades
  `published`, units, pacotes), descrita no desenho. O que falhar vira item
  manual com a causa — e é sinal de regra faltando (volta para M0).
- "Compilar para conferir" (opcional): escolhe uma IDE da máquina que tenha o
  RAL de destino (o recibo do instalador diz), compila o projeto numa pasta
  temporária com o motor do instalador e mostra o primeiro erro de cada arquivo.

**Pronto quando:** a validação acusa um `.dfm` de teste com uma propriedade da
1.2 que nenhuma regra tirou, e passa limpo nos samples migrados; a compilação de
conferência roda num sample pelo Delphi 13 e pelo Lazarus 4.9.

### M7 — Relatório e desfazer

- Relatório em três blocos: **feito** (por arquivo, por regra), **precisa de
  você** (cada item com o que fazer), **comportamento que mudou** (o que foi
  preservado e o que passou a ser diferente). Gravado no projeto como
  `MIGRACAO-RAL-<de>-<para>.md` e `.html`, e mostrado na tela.
- Desfazer pela cópia de segurança, quando o usuário pediu a cópia, inclusive
  depois de fechar a ferramenta; sem cópia, o Desfazer não aparece.
- Código de saída da `ralmigra`: 0 = nada manual; 1 = sobrou item manual;
  2 = erro; 3 = recusado (versão de destino mais velha que a de origem).

**Pronto quando:** migrar com cópia de segurança e desfazer os samples deixa a
árvore idêntica à de antes (`git status` limpo); sem cópia, o Desfazer não é
oferecido; o relatório dos samples lista os itens manuais esperados e nenhum
outro; e `ralmigra --de=2.0 --para=1.1` recusa sem tocar em nada.

### M8 — Telas, CLI, instalador e entrega

- Janela LCL no visual do instalador (temas): idioma → projeto(s) → versões (a
  detectada, com a evidência) → prévia → aplicar → relatório → conferir
  compilando. Aberta pelo instalador, já vem no idioma que o usuário escolheu
  lá (argumento `--idioma=`), sem perguntar de novo.
- **Idiomas** (desenho, "Idiomas"): `resourcestring` em `RALMig.Mensagens` e
  `RALMig.Tela.Mensagens`, `.po` próprios em `migrador/languages`
  (pt-BR, en-US, es-ES) embutidos como RCDATA, o gerador de `.po` do instalador
  (`RALInst.Tela.GeradorPO`) reaproveitado, traduções revisadas à mão.
- `ralmigra` com os mesmos verbos (`detectar`, `previa`, `aplicar`, `desfazer`),
  traduzida, com `--idioma=` (padrão: o do sistema).
- O instalador chama a janela no fim de uma atualização (F21 do
  `PLANO_INSTALADOR.md`), já com a origem, o destino e o idioma.
- Workflow de release do instalador compila e publica o migrador junto, com o
  `SHA256SUMS` cobrindo os dois, para a auto-atualização trocar ambos; os testes
  sem IDE (M0, M1, M2, M7 e `tests/traducao` do migrador) entram no workflow.

**Pronto quando:** o dono migra um projeto real dele, da 1.1 para a 2.0, pela
janela aberta a partir do instalador, e o projeto abre na IDE sem nenhum aviso
de propriedade ou classe; o mesmo pela `ralmigra` em Linux; e a janela, a
`ralmigra` e o relatório saem inteiros em inglês e em espanhol (nenhum texto em
português sobrando — conferido por captura de tela e pelo teste de tradução).

### M9 — Processo: toda quebra nova traz a regra

- Regra de trabalho do RAL, **preferencial, não obrigatória por enquanto**
  (D-M7): commit que quebra API publicada ou muda padrão de propriedade traz a
  regra no `ralmigracao.json` da versão (e a linha na tabela de
  `atualizacao.md`). O dono está desovando o que está pendente; pode entrar
  commit sem a regra.
- Por isso o `apidiff` entre o último release e o `dev` **avisa** (lista as
  diferenças sem regra, no workflow e na `ralmigra inventario`), não bloqueia.
- **Rito de pré-lançamento** (o dono vai criar, mais para a frente: regressão e
  testes completos antes de lançar uma versão): é ali que a lista do `apidiff`
  tem de estar **vazia** — toda diferença com regra ou justificativa. É o
  momento em que a cobertura de 100% é cobrada.

**Pronto quando:** uma propriedade renomeada de propósito numa cópia do `dev`
aparece na lista do `apidiff` com o nome dela, e sai da lista ao acrescentar a
regra; o modo `--pre-lancamento` do mesmo teste falha enquanto a lista não está
vazia.

## Decisões

Tomadas pelo dono (14/10/2026):

- **D-M1 — Onde moram as regras, e como chegam.** A ferramenta é **enviada
  junto da versão nova do RAL**: as regras das versões já lançadas vêm
  embutidas no migrador, e cada versão nova do RAL traz o seu
  `ralmigracao.json`, que acrescenta ou substitui (como o manifesto do
  instalador).
- **D-M2 — Onde mora o código e como se atualiza.** No repositório do
  instalador, pasta `migrador/`, usando as units `RALInst.*`; sai no mesmo
  release, e **a auto-atualização do instalador atualiza o migrador junto**
  (F21 do `PLANO_INSTALADOR.md`).
- **D-M5 — O que fazer quando o RAL mudou.** "Se o usuário atualizou, atualiza
  o código para funcionar na versão nova; se a forma de usar mudou, muda também,
  automaticamente; se não for possível, informa o problema e explica como
  atualizar." Lido assim no desenho ("O que 'funcionar na versão nova' quer
  dizer"): forma de usar que mudou é reescrita; padrão que mudou vira o valor
  escrito explicitamente na forma nova, para o projeto continuar fazendo o que
  fazia, e o relatório diz como adotar o padrão novo; o resto é item manual
  explicado. **Confirmado com o exemplo do CORS** (14/10): sem o ajuste, para o
  usuário pareceria que a atualização quebrou o sistema, porque o que era aberto
  por padrão passou a depender de uma configuração que antes não existia. O
  migrador faz o ajuste **e** o relatório explica a mudança de comportamento
  padrão, para ele entender o que mudou (bloco "comportamento que mudou" da M7).
- **D-M4 — Até onde mexer no código (`.pas`)**, consequência da D-M5: **tudo o
  que a regra consegue provar é reescrito automaticamente**, inclusive
  atribuição em código (M5), e aparece na prévia; o que não dá para provar vira
  item manual com o trecho pronto. Sem a etapa de "sugestão para aceitar".
- **Idiomas** (pedido de 14/10/2026): o migrador é multi-idioma como o
  instalador — tela, `ralmigra`, relatório e os textos das regras (desenho,
  "Idiomas"; M2 e M8).
- **D-M3 — Onde gravar.** No próprio projeto, depois da prévia. A **cópia de
  segurança é uma opção**: marcada, o migrador faz o backup e depois atualiza
  os fontes; desmarcada, só sobrescreve (desenho, "Segurança"; M7).
- **D-M6 — `.dfm` binário.** Avisa que o formulário precisa estar em texto e
  oferece converter; o usuário aceitando, converte e atualiza; recusando, não
  faz nada naquele formulário (e o relatório diz que ficou de fora).
- **D-M7 — Regra junto da quebra: preferencialmente sim**, sem obrigar agora
  (há pendências a desovar primeiro). A cobrança vem no rito de
  pré-lançamento que o dono vai criar, com regressão e testes completos (M9).
- **D-M8 — Voltar de versão: não.** "Regrediu versão, prejuízo de quem
  regrediu." O instalador continua deixando voltar (para abrir um projeto grande
  antigo e dar manutenção rápida enquanto ele é atualizado); o migrador só anda
  para a frente e recusa o contrário.
- **D-M9 — RESTDW2RAL: separado por enquanto.** Pode ser integrado no futuro,
  e não só ele: a ideia é o migrador poder trazer **outros frameworks** para o
  RAL. Consequência para o desenho agora, sem custo: o conjunto de regras tem
  uma **origem** (`RAL` hoje; `RESTDW`, ou outro framework, depois), e a cadeia
  de passos parte dela — de "RESTDW 2.1" chega-se ao "RAL 2.0" do mesmo jeito
  que de "RAL 1.1". Nenhuma regra de outro framework entra neste plano.

Abertas: nenhuma.

## Estado

**13/10/2026:** só o plano. Nada implementado. Ordem sugerida: M0 e M1 em
paralelo (independentes), depois M2 → M3 → M4 (onde está o grosso da 2.0) → M5
→ M6 → M7 → M8; M9 assim que M0 existir.
**14/10/2026:** D-M1, D-M2, D-M4 e D-M5 respondidas; pedido novo: multi-idioma
(seção "Idiomas", M2, M8). Depois, D-M3, D-M6, D-M7, D-M8 e D-M9 também
(desenho "Segurança", M7, M9). Todas as decisões tomadas; nada implementado.
**19/10/2026 (sem commit): M0 feita.** Pasta `migrador/` criada:
- `src/RALMig.API.pas`: API de uma versão do RAL lida de pasta ou zip
  (`TOrigemLocal`/`TOrigemZip`) com `fcl-passrc`, em duas passadas
  (Delphi `VER370` e FPC 3.2.3, Windows) unidas; só a `interface`. Grava units
  (com a pasta), classes com ancestral e `published` achatado pelas ancestrais
  do RAL, `default`/`stored`, tipos procedurais, enums, rotinas, constantes,
  registradas (`RegisterComponents`, lido do texto) e pacotes Delphi/Lazarus
  (`TCatalogo`).
- `src/RALMig.Inventario.pas`: diff entre versões vizinhas em chaves
  `<tipo>:<alvo>` (`unit-sumiu`, `unit-pasta-mudou`, `classe-sumiu`,
  `classe-mudou-unit`, `ancestral-mudou`, `published-sumiu`, `published-tipo`,
  `default-mudou`, `evento-mudou`, `publico-sumiu`, `assinatura-mudou`,
  `registro-sumiu`, `tipo-sumiu`, `enum-valor-sumiu`, `rotina-sumiu`,
  `rotina-mudou`, `constante-sumiu`, `pacote-sumiu`, `pacote-perdeu-unit`);
  cobertura com glob; gerador do `INVENTARIO.md`.
- `ferramentas/apidiff` (`extrair`, `comparar`, `inventario`,
  `pendentes [--pre-lancamento]`), `api/api-{v1.0,1.1,dev,2.0}.json` (zips do
  cache do instalador; a 2.0 da pasta `PascalRAL-2.0`, árvore de trabalho),
  nenhum erro de parse nas quatro.
- `regras/cobertura.json` (a fonte) e `regras/INVENTARIO.md` (gerado): 20
  diferenças em `v1.0>1.1` (fechado), 12 em `1.1>dev`, 213 em `dev>2.0`, todas
  ligadas a uma regra ou a uma justificativa, **nenhuma pendente**; mais 34
  quebras de comportamento.
- `tests/inventario`: autoteste (`--testes`) e modo real; sai com 1 quando há
  pendente em passo fechado (ou em qualquer um, com `--pre-lancamento`).
  Conferido tirando um item da cobertura numa cópia (`--raiz=`).
- O levantamento da wiki e dos gabaritos, com as tabelas de→para que as M2 a
  M5 usam, está em `.agents/MIGRADOR_QUEBRAS.md`.

Build: `lazbuild -B migrador/ferramentas/apidiff/apidiff.lpi` (Lazarus 4.6);
teste: `fpc -B -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
-FEmigrador/lib/testes migrador/tests/inventario/inventario.lpr`. O teste ainda
não está no workflow (entra na M8).

Achados que mudam etapas seguintes:
- **Vocabulário (M2):** faltam `uses-adicionar` (a `1.1/criptotype-*`, as
  classes que foram para `RALDigest`/`RALOAuth2`/`RALCORS`/`RALSecurity` sem
  apelido), `caminho-trocar` também para `.dpk`, e uma ação para objeto de
  formulário sem equivalente (`TRALClientOAuth`/`TRALServerOAuth`, OAuth 1.0a).
- **Wiki (para corrigir):** `TCriptoType`/`ctAES*` → `TRALCriptoType`/`crAES*`
  (1.0 → 1.1, a única quebra provada pelo gabarito `37a6638`) não está em
  página nenhuma; o 405 por verbo fora de `AllowedMethods` já é da 1.1 (era
  403); XML, SOAP e `TRALSelfSigned` nunca existiram em `v1.0`/`1.1`/`dev`
  (são do `1.3-plugin`); `RALDigest`/`RALOAuth2` são units novas para quem
  declara essas classes.
- **CORS sem a linha `AllowOrigin`** (o caso 2 da D-M5): a 1.1 rodava aberto, a
  1.2 fecha. O gabarito `677864d` fechou o CORS do `Basic Server/Sagui` sem
  dizer; a regra `1.2/cors-materializar-asterisco` escreve `'*'`.
- **Gabarito `677864d` incompleto** para a M3: não troca `PascalRALDsgn` por
  `PascalRAL` nos pacotes de runtime nem acrescenta `RALPluginAuthPackage`/
  `RALStoragePackage`/`RALModule*Package` ao `DCC_UsePackage`; "bater com o
  `677864d`" vale para os nomes renomeados, não para esses.
- O diff não vê tipo (não classe) que mudou de unit, nem `default` dado no
  construtor (o CORS acima): esses vêm da cobertura de comportamento.
- A API `dev` é o zip do GitHub de hoje; a 2.0 tem commits locais não
  publicados. Regerar as duas (`apidiff extrair`) quando mudarem.
**19/10/2026 (sem commit, sem atualizar este bloco):** M1 a M8 escritas
(units da tabela de `MIGRADOR_ARQUITETURA.md`, telas, `ralmigra`, 14 testes em
`migrador/tests`, workflow).
**24/10/2026 (sem commit): conferido e fechado no Linux** (FPC 3.2.3 / Lazarus
4.6 do fpcupdeluxe), por um swarm de quatro agentes mais o coordenador. Os 14
testes de `migrador/tests` passam com `--testes` (o `traducao` sem argumento).
`ralmigrador` (Linux64), `ralmigra`, `RALInstaller` e `ralcli` compilam com `lazbuild -B`.
- **Defeitos de porte achados e corrigidos:**
  - `RALMig.Conferir` usava `RALInst.IDE.Delphi` fora de `{$IFDEF MSWINDOWS}`, e por isso
    a `ralmigra`, a janela e os testes `conferir`/`traducao` não compilavam fora do Windows.
  - Os `Resource_*` dos `.lpi` (do instalador e do migrador) usavam `\`, e o lazbuild do
    Linux/macOS não embutia regras, APIs, `.po` nem imagens. Agora usam `/`.
  - O workflow agora roda o `mig_motor` em todos os sistemas.
- **M3/M4:** um comparador (no rascunho, fora do repo) põe lado a lado a cópia 1.x
  (`677864d^`) migrada e o gabarito `677864d`, sem posição e sem o número do componente
  novo. Resultado: 0 diferenças fora dos desvios aceitos.
  - Desvios aceitos: `PascalRALDsgn`→`PascalRAL` e os pacotes 2.0 que o gabarito não pôs;
    o CORS `'*'` do Sagui; `MaxConnections`; e o `.lfm` corrompido do `mORMot2_Zeos`, que
    fica em "não migrados".
  - Defeito corrigido: `TProjetoXML.AcrescentarPacote` misturava `<Item>` e `ItemN`/`Count`.
  - Testes novos: lista `.lpi` nos dois estilos; frame, DataModule e `inline`.
- **M5:** nos `PascalRAL-Samples-2.0`, 0 alterações e 0 avisos de código (sem falso positivo).
  - Na cópia 1.x: 82 arquivos, 434 alterações e 9 itens manuais, depois de cortar 31.
    Cortes: 21 falsos positivos de `cliente-chamada-sincrona` (`var TRALResponse`, `eb*`
    escrito); o CORS de servidor criado em código passou a ser escrito, não avisado; e o
    `TRALServerBasicAuth` que casava `TRAL*Server`.
  - Um `case` que esvazia sai inteiro.
  - Fixtures `UPadroes12`, `UPlugins20` e `UPadroes20`.
  - `codigo --compilar` (Linux, fpHTTP) compila o código reescrito contra `PascalRAL-2.0/src`
    em ObjFPC e em modo Delphi.
  - Regra `2.0/oauth1-removido` passa a pegar `TRALClientOAuth`/`TRALServerOAuth` em `.pas`.
- **M6:** a validação acusa a propriedade da 1.2 no `.dfm` de teste, e os samples migrados
  saem com 0 itens.
  - O extrator grava `le-antigos` (nomes lidos por `DefineProperties`, ex.
    `TRALSaguiServer.ConnectionLimit`), que a validação aceita. Só o `api-2.0.json` foi
    regerado.
- **M7:** `aplicar --backup` + `desfazer` nos samples deixa `git status` vazio.
  - O relatório entra em `<zip>.depois.txt` (seção `[relatorios]`), e o desfazer o apaga
    se o hash bate. Depois, a cópia e `_ral-migracao/` somem.
  - Defeito corrigido: o `TUnZipper` gravava `_manifesto.txt` no cwd.
  - Códigos de saída: 1 com item manual; 2 em `desfazer` sem cópia; 3 em 2.0→1.1, sem tocar
    em nada.
- **M4 (Lazarus, só Linux):** os 18 `.lpi` de `Lazarus/` migrados dão o mesmo resultado
  que o gabarito. Os que falham, falham pela unit `Windows` do Cripto Sample, pelo
  `DrawFrameControl` do `server_bridge` e pelo `.lfm` corrompido. Nenhum falha por regra.
  - Achado no RAL 2.0 e corrigido em `PascalRAL-2.0`: `uses RALJSON` com o arquivo
    `RALJson.pas` quebrava o Swagger/Postman, o storage JSON, o `RALDBModule` e o
    `RALClient` no FPC/Linux. Agora é `RALJson` nos cinco.
- **M8:** os três `.po` estavam vazios. Agora têm todas as `resourcestring` do núcleo, das
  telas e das `RALInst.*` ligadas, com en-US e es-ES traduzidos à mão.
  - 26 entradas `RALInst.*` estavam vazias no es-ES do instalador. Foram preenchidas no
    `.po` do migrador; o `src/languages/ralinstaller.es-ES.po` talvez tenha a mesma lacuna.
- **Falta, só no Windows ou com o dono:**
  - compilar os samples migrados no XE2, no Delphi 13 e num sorteado (M4);
  - "compilar para conferir" pelo Delphi 13 e pelo Lazarus 4.9 (M6);
  - o projeto real do dono pela janela aberta do instalador e capturas en-US/es-ES (M8);
  - rodar o workflow de verdade.
- **Falta, pequeno:**
  - regerar `api-v1.0/1.1/dev` com `le-antigos`;
  - feitos depois (sem commit): `case` com rótulo enumerado sem o `X: ;`; `Get(url, cb)` ->
    `Get(url, cb, ebMultiThread)` (campos `acrescentar`, `acrescentar-unit` e `feito` do
    `padrao-avisar`; só com receptor provado e dois argumentos, o resto segue manual);
    relatório anterior vai na cópia e volta no desfazer; rodada sem passo
    (`Motor.SemPassos`) não grava relatório e o `De` do relatório é a origem real.
**25/10/2026 (sem commit): conferido no Windows, pela VM `win10`.**
- Instalador, `ralcli`, `ralmigra` e `ralmigrador` (`--bm=Windows64`) compilam no
  Lazarus/Windows. Os 14 testes do migrador e os 6 do instalador passam no Windows
  e no Linux.
- Defeito achado so' no Windows e corrigido: o `UsesUmaVez`
  (`RALMig.Acoes.Codigo`) guardava a unit pedida pelo endereco do
  `TTextoOriginal`. Um texto novo que nascia no endereco de um ja' liberado
  parecia ja' servido, e o `.pas` de um frame saia sem o `RALCORS`. Agora o
  registro usa `TTextoOriginal.Id` (unico) e `Geracao` (muda a cada
  carga/consolidacao).
- `ralmigra aplicar` no Windows sobre a copia 1.x dos samples da' o mesmo que no
  Linux (82 arquivos, 434 alteracoes, 9 itens manuais).
- M4, compilacao dos `.dpr` Delphi migrados contra a 2.0 (Win64), comparada com o
  gabarito `677864d` compilado do mesmo jeito: Delphi 13 23/25, 10.4 24/25, XE2
  4/25. Migrado e gabarito batem nos 25 projetos, nas tres versoes.
  - O que falha no 13 e no 10.4 e' do ambiente: Kwik (Android) e Zeos no 13.
  - No XE2 e' do proprio sample (FireDAC, `System.JSON`, `Vcl.WinXCtrls`,
    `WinHTTP`) e do Indy que vem com o XE2 (ver o `PLANO_REFORMA_2_0.md`).
- Falta: a janela pelo instalador e as capturas en-US/es-ES (M8), e o
  "compilar para conferir" pela tela (M6).
