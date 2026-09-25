# Plano de conclusão do RAL Installer

Levantamento do estado do instalador e plano para automatizar a instalação do
PascalRAL em **todas** as IDEs da máquina — Delphi e Lazarus no Windows, Lazarus
no Linux e no macOS — sem o usuário precisar abrir IDE nenhuma.

Escrito em 2026-09-16 a partir da árvore do branch `installer`, do repositório do
RAL (`PascalRAL-dev`) e da wiki. Os números de linha valem para o commit
`3ba7db7` mais as modificações não commitadas daquele momento (todas cosméticas).

Premissas do produto, conforme pedido:

- instalação fácil e **personalizada**: o usuário escolhe o que entra;
- instala em **várias IDEs** numa passada só;
- **versionamento independente** do pacote principal;
- **esperto o bastante** para achar pacote novo sozinho e deduzir a ordem;
- alternativa de **só apontar os fontes no library path**, sem instalar nada;
- **baixa e instala as dependências sozinho**, cada uma do jeito dela, por uma
  receita em JSON — dependência nova se configura sem recompilar o instalador;
- **escolhe a versão do RAL**: a estável mais recente por padrão, ou a tag, o
  release ou o branch que o usuário pedir;
- **se atualiza sozinho** a partir dos releases do próprio repositório.

## 0. A forma do produto

**Um executável só, compilado por FPC/Lazarus, para cada sistema.** É por isso
que a ferramenta foi escrita em Lazarus e não em Delphi: o Lazarus compila o
mesmo fonte para Windows, Linux e macOS, e o instalador precisa rodar nos três.

**Instalar no Delphi não exige projeto Delphi.** O instalador nunca abre a IDE e
nunca compila a si mesmo com o `dcc32`: ele *chama* as ferramentas de linha de
comando do Delphi — `dcc32`/`dcc64` para os pacotes, `brcc32` para os `.res` — e
escreve o registro. Tudo isso é `TProcess` e `TRegistry` a partir do binário FPC.
Nenhum `.dpr`, `.dproj` ou `.groupproj` do instalador precisa existir.

**O alcance do Delphi é o Windows, e só ele.** A IDE não existe nos outros
sistemas, então o binário de Linux e o de macOS instalam apenas em Lazarus. O
código que fala com o Delphi (registro, `dcc32`, `brcc32`, caminhos do BDS) vive
sob `{$IFDEF MSWINDOWS}`, e fora do Windows a tela de escolha de IDE nem oferece
o Delphi — não como falha, mas porque ali ele não existe.

| Binário | Delphi | Lazarus |
| --- | --- | --- |
| Windows (win32/win64) | sim — `dcc32`/`brcc32` + registro | sim — `lazbuild` |
| Linux (x86_64) | não se aplica | sim — `lazbuild` |
| macOS (x86_64/aarch64) | não se aplica | sim — `lazbuild` |

**O fluxo do usuário** (confirmado em 2026-09-17): baixa o instalador, abre,
escolhe — versão do RAL, IDEs, recursos e **uma pasta** —, e só então o
instalador baixa **tudo** para essa pasta (os fontes do RAL, os submódulos e os
fontes das dependências) e instala de acordo com a escolha, sozinho. Três
consequências que atravessam o plano:

1. **Nada é baixado antes de o usuário confirmar.** As telas de escolha precisam
   saber quais pacotes existem na versão escolhida *sem* o RAL em disco: o
   catálogo (F2) lê de uma origem abstrata, e na GUI essa origem é o GitHub —
   o **zip da versão**, baixado para o cache pelo `codeload.github.com` (fora do
   limite da API) e lido sem extrair; o mesmo zip vai para a pasta na execução
   (F8). Nada é gravado na pasta do usuário antes da confirmação.
2. **A pasta escolhida é permanente.** Library path, variáveis de ambiente,
   links de `.lpk` e o recibo apontam para ela: não é pasta temporária, e apagar
   depois quebra as IDEs. A tela tem de dizer isso, e o layout dentro dela é
   fixo e versionado (`<pasta>/PascalRAL/<versão>/`,
   `<pasta>/dependencias/<nome>/<versão>/`), para uma atualização não pisar na
   instalação que está funcionando.
3. **Baixar é uma etapa da execução, com falha própria.** Antes de compilar
   qualquer coisa, tudo o que o plano exige tem de estar na pasta; falha de rede
   no meio para a rodada sem ter tocado em IDE nenhuma.

O caso de quem desenvolve o próprio RAL continua valendo: apontar uma pasta
local já existente pula o download dos fontes do RAL (F8).

---

## 1. Estado atual

Aplicação Lazarus (LCL), wizard de 5 telas encadeadas por um `TNotebook` em
`fontes/umain.pas`: idioma → Delphi ou Lazarus → versões encontradas → recursos →
instalação. Tema claro/escuro e i18n própria, com gerador de `.po`
(`fontes/i18n_utils.pas`, botão `bTranslate` em `umain`).

| Peça | Unit | Estado |
| --- | --- | --- |
| Descoberta de Delphi | `fontes/delphiutils.pas` | Só registro: `Borland\Delphi`, `Borland\BDS`, `CodeGear\BDS`, `Embarcadero\BDS`, lendo `App`, `ProductVersion`, `Personalities` |
| Descoberta de Lazarus | `fontes/lazarusutils.pas` | Varre `A:\`…`Z:\` inteiros atrás de `lazarus.exe` + `lazbuild.exe`; botão de adicionar pasta manualmente |
| Base comum das IDEs | `fontes/ideutils.pas` | `TIDEObjectData` (nome, versão, exe, ícone, `TProcess`) e `TIDEFinder`; `Install` é **abstrato** |
| Modelo do manifesto | `fontes/installparser.pas` | `TInstaller` → `packages` (engines / databases / compression) + `depedancies`; leitura de JSON pronta, escrita não |
| Download | `fontes/githubutils.pas`, `fontes/http_client.pas`, `fontes/ralzipper.pas`, `fontes/tar_gzip.pas` | zipball da API do GitHub, unzip e `.tgz` (static do mORMot2) |
| Instalação Lazarus | `fontes/lazarusutils.pas` | Monta listas de `.lpk` e chama `lazbuild --build-ide= --add-package[-link]` |
| Instalação Delphi | — | **não existe** |

> **2026-09-25:** a tabela acima é a foto de 2026-09-16. As units das telas
> seguem hoje o padrão do RAL e moram em `fontes/telas` (ver "Revisão de
> 2026-09-25" no fim da §5): `umain` → `RALInst.Tela.Principal`, `ufrm_modelo`
> → `RALInst.Tela.Modelo`, `ufrm_idioma` → `RALInst.Tela.Idioma`, `ufrm_ide` →
> `RALInst.Tela.IDE`, `ufrm_ide_versions` → `RALInst.Tela.VersoesIDE`,
> `ufrm_ide_version` → `RALInst.Tela.ItemIDE`, `ufrm_recursos` →
> `RALInst.Tela.Recursos`, `ufrm_install` → `RALInst.Tela.Instalar`, `udm` →
> `RALInst.Tela.Imagens`, `utools` → `RALInst.Tela.Temas`, `ideutils` →
> `RALInst.Tela.Instalacao` (`TIDEObjectData` → `TIDETela`), `delphiutils` →
> `RALInst.Tela.Delphi`, `lazarusutils` → `RALInst.Tela.Lazarus`, `i18n_utils`
> → `RALInst.Tela.GeradorPO`; `tar_gzip` saiu (era `RALInst.Tar`).

Lixo a remover: `fontes/githubral.pas` (aponta para o repositório do
REST-DataWare, morto e substituído por `githubutils`), `assets_old/` (imagens
antigas) e `src/` (telas não versionadas de um desenho anterior).

---

## 2. Lacunas, em ordem de gravidade

1. **O lado Delphi é zero.** `Install` é abstrato em `fontes/ideutils.pas:29` e
   `TDelphiObjectData` não sobrescreve: marcar uma IDE Delphi e mandar instalar
   cai em *Abstract Error*. Falta compilar, registrar, library path, environment
   variable, saída de BPL/DCP e plataforma.

2. **Os `.res` não são versionados no repositório do RAL.** Nenhum `.res`
   rastreado (só os `.dcr`). Sobre um zip recém-baixado, `dcc32 PascalRAL.dpk`
   morre em `E1026 File not found: 'PascalRAL.res'`. Sem resolver isto, nenhum
   plano de compilação Delphi funciona.

3. **Nada confere erro.** `fontes/lazarusutils.pas:91` executa o `TProcess` e
   nunca lê `ExitStatus` nem a saída: falha de compilação passa como sucesso.
   Junto disso, `fontes/lazarusutils.pas:279` percorre `vNames.Count` indexando
   `vNamesRAL` (estoura ou imprime nome errado), e `--build-ide=` é disparado
   até três vezes por IDE, inclusive com lista vazia.

4. **Lazarus sem `--primary-config-path`.** Todas as instalações compartilham a
   configuração padrão. Numa máquina com uma dúzia de Lazarus (3.0, 3.2, 3.4,
   3.6, 4.0, 4.2, 4.6, …, mais o layout fpcupdeluxe com `config_lazarus` ao
   lado) o instalador escreve todas no mesmo lugar.

5. **Linux e macOS não compilam.** `fontes/lazarusutils.pas:352` chama
   `ListaDiretorios('/', AOnFind)` — um parâmetro a mais e um identificador que
   não existe; o ramo só não quebra porque ninguém compilou fora do Windows.
   Varrer `/` inteiro é inviável, a tela de IDE esconde o Delphi só em
   `{$IFDEF LINUX}` (nunca em `DARWIN`), e o `.lpi` tem build modes
   Windows32/64 e Linux32/64 — nenhum de macOS.

6. **Delphi não registrado não aparece.** O botão de adicionar pasta manual só
   existe para Lazarus (`fontes/telas/ufrm_ide_versions.pas:254`). É um caso
   real e comum: instalações inteiras em disco, com `bin\dcc32.exe` e
   `bin\rsvars.bat` perfeitamente utilizáveis, que o registro não menciona.

7. **O manifesto é rascunho.** Em `tests/Install2.json` todo `location` de DPK
   aponta para `pkg/Delphi/PascalRAL.dpk` e `version-code` é sempre `-1`; faltam
   pacotes que existem hoje no repositório (`PascalRALDsgn`, `NetHttpRAL`,
   `UniGUIRAL`, `XSocketRAL`, `RALWizard`, `RALDBFireDACLink`,
   `RALDBFireDACObjects`, `RALDBZeosLink`, `RALBSONStorage`, `cgiral`,
   `raldbbson`, `raldbsqldblink`, `raldbzeoslink`). O formato v2 também perdeu o
   que o v1 (`tests/Install.json`) tinha: `install-order`,
   `is-compile`/`is-install` e `library-paths`. E o caminho de produção nem lê
   manifesto: `fontes/installparser.pas:1197` é um comentário
   `// baixar do github` — só o `{$IFDEF INSTALL_TEST}` funciona.

8. **A ordem é a ordem do JSON.** Não há grafo. A boa notícia é que a
   dependência já está declarada nos próprios pacotes: `requires` no `.dpk`
   (`IndyRAL` → `PascalRALDsgn` → `PascalRAL`) e `RequiredPkgs` no `.lpk`
   (`indyral` → `indylaz`, `PascalRAL`).

9. **Sem matriz de capacidade por versão de IDE.** Nada impede marcar FireDAC no
   XE2 ou NetHTTP no Delphi 7. As fontes existem: `compatibilidade.md` na wiki e
   os símbolos de `src/base/PascalRAL.inc` (o cliente netHTTP, por exemplo, está
   sob `DELPHI10_1UP`).

10. **Sufixo do Indy.** Ninguém calcula `IndyCore160`/`IndySystem160`/
    `IndyProtocols160`. Hoje isso está chumbado no `Base_Win32` de
    `pkg/Delphi/Engine/IndyRAL.dproj`, certo só para XE2 com Indy atualizado.

11. **Faltam ainda:** modo "somente library path"; instalar Delphi e Lazarus na
    mesma passada (a tela zera a lista ao trocar de IDE — `fmain.IDE` é 0 ou 1);
    desinstalação e rollback; log em arquivo; CLI/silencioso; perfil salvo
    (`GetAsJSON` devolve `nil`); detecção de IDE aberta; tratamento de
    `Disabled Packages`; elevação. Mais um vazamento: `TInstaller.Destroy`
    (`fontes/installparser.pas:1214`) não libera `FDepedancies`.

---

## 3. Decisões de arquitetura

| Decisão | Escolha | Por quê |
| --- | --- | --- |
| Compilar Delphi com `msbuild` ou `dcc32`? | **`dcc32`/`dcc64` direto** | `msbuild` quebra com `MSB6003` em máquina com muitos componentes (o `DelphiLibraryPath` vira dezenas de KB de linha de comando) e os `.dproj` carregam listas podres de `DCC_UsePackage` — o `IndyCore160` é exatamente isso. Mesma conclusão a que o RALOrquestrador chegou. |
| E os `.res` que não existem? | **Gerar** um `.rc` mínimo por pacote e chamar `brcc32` antes de compilar | Não depende de mudar o repositório principal. Vale propor lá, em paralelo, que os `.res` passem a ser versionados. |
| Lista e ordem dos pacotes: manifesto ou descoberta? | **Descoberta** | Varrer `pkg/**`, fazer o parse de `requires`/`RequiredPkgs`, montar o grafo e ordenar topologicamente. O manifesto fica só com o que não é derivável: nome amigável, grupo, IDE mínima/máxima, dependência externa e library paths. É isto que faz o instalador reconhecer pacote novo sem release nova. |
| Como se ensina uma dependência nova ao instalador? | **Receita em JSON, vocabulário de ações fechado** | Cada dependência se instala de um jeito (mORMot2 é library path no Delphi e `.lpk` no Lazarus; Zeos é pacote; brotli é DLL; UniGUI é pago e só se detecta). Descrever isso em dado deixa o instalador aprender dependência nova sem release nova — e o vocabulário ser fechado, sem "rode este comando", impede que uma receita baixada da rede execute qualquer coisa na máquina. Detalhe na F7. |
| Como saber se `IndyCore` leva sufixo? | **Perguntar ao disco** | Procurar os `.dcp` em `<BDSCOMMONDIR>\Dcp` e `<BDS>\lib\<plat>\release`: existindo `IndyCore<N>.dcp`, usa o sufixado (Indy atualizado pelo usuário); só `IndyCore.dcp`, usa o nativo; nenhum dos dois, erro claro ("atualize o Indy ou desmarque o motor"), nunca um `-LU` chutado. |

Regra geral, herdada do orquestrador: **capacidade que não dá para confirmar sai
como pulada, com o motivo — nunca como sucesso silencioso.**

---

## 4. Arquitetura alvo

Núcleo sem UI, com GUI e CLI por cima — a regra mora na unit, a casca só
pergunta e mostra (mesmo desenho do conversor do `ral_restdw` e do orquestrador).

```
fontes/nucleo/
  RALInst.Tipos.pas        enums e records (tipo de IDE, plataforma, ação, resultado)
  RALInst.Log.pas          log em memo + arquivo: comando exato, saída crua, versões
  RALInst.IDE.pas          TIDEInstance (raiz, versão, plataformas, config, capacidades)
  RALInst.IDE.Delphi.pas   [MSWINDOWS] descoberta por registro + disco; tabela BDS -> produto -> sufixo
  RALInst.IDE.Lazarus.pas  descoberta por pastas; PCP por instalação; layout fpcupdeluxe
  RALInst.Catalogo.pas     parse de .dpk/.dproj/.lpk, grafo, ordenação topológica
  RALInst.Manifesto.pas    o que não é derivável (grupos, min/max IDE, deps externas, paths)
  RALInst.Fontes.pas       download de repositório, submódulos, descompactação
  RALInst.Versoes.pas      releases e tags do RAL; qual é a estável mais recente
  RALInst.Receita.pas      lê e executa a receita de instalação de uma dependência
  RALInst.AutoUpdate.pas   release do próprio instalador: comparar, baixar, trocar
  RALInst.Build.Delphi.pas [MSWINDOWS] dcc32/dcc64 + brcc32 + -LU calculado + conferência do .bpl
  RALInst.Build.Lazarus.pas lazbuild com --pcp/--lazarusdir/--cpu/--os, um --build-ide
  RALInst.Registro.Delphi.pas [MSWINDOWS] Known Packages, Disabled Packages, Search/Browsing Path, env vars
  RALInst.Plano.pas        plano de execução legível (dry-run) antes de tocar em nada
  RALInst.Recibo.pas       o que foi instalado em cada IDE -> desinstalar e atualizar
```

As units marcadas `[MSWINDOWS]` são as únicas que conhecem o Delphi, e saem do
binário nos outros sistemas: nada do núcleo comum, da UI ou da CLI depende delas.
Quem decide o que existe é `RALInst.IDE.pas` — a lista de IDEs suportadas chega
vazia de Delphi fora do Windows, e o resto do programa não precisa saber por quê.

---

## 5. Fases

Caminho crítico: **F3, F4 e F7** — compilar no Delphi, registrar no Delphi e
resolver as dependências. A F5 é ajuste sobre o que já roda; F8 e F12 dependem só
de rede e podem andar em paralelo, mas a F12 precisa da F11 pronta.

### F0 — Saneamento
Consertar o `BuscarIDE` não-Windows, esconder o Delphi fora do Windows na tela de
IDE — hoje a condição é `{$IFDEF LINUX}`, tem de ser `{$IFNDEF MSWINDOWS}`, senão
o macOS oferece uma IDE que não existe ali —, corrigir o
laço de log e o vazamento do `FDepedancies`, passar a ler `ExitStatus` e a saída
do `TProcess`, remover `githubral.pas`, `src/` e `assets_old/`, commitar o que
está solto na árvore.
**Pronto quando:** compila nos quatro alvos (win32, win64, linux64, macOS) e uma
falha de `lazbuild` aparece no log com a linha de comando e a saída do compilador.

**Estado (2026-09-17):** feito, falta commitar.
- `TIDEObjectData.Executar` (`fontes/ideutils.pas`) roda o processo com pipe,
  loga a linha de comando e a saída enquanto ele roda e devolve o `ExitStatus`;
  `Install` deixou de ser abstrato e diz "não suportado" em vez de *Abstract
  Error*. O Lazarus agora faz `--add-package-link`, `--add-package` e **um**
  `--build-ide=` no fim, pula lista vazia e para na primeira falha.
- `BuscarIDE` fora do Windows varre só raízes conhecidas (`/usr/lib/lazarus`,
  `/usr/share/lazarus`, `/opt`, `/Applications`, `$HOME`) com profundidade
  limitada. Delphi escondido com `{$IFNDEF MSWINDOWS}`; `delphiutils` só entra
  na `uses` no Windows. `tar_gzip.pas` não compilava fora do Windows (`BaseUnix`
  ausente, `Headder`, `FpLink` no lugar de `FpSymlink`).
- Corrigidos o laço de log, o vazamento de `FDepedancies` e o `Personalities`
  do `delphiutils` que gravava em `vKey` em vez de `vStr`.
- Removidos `fontes/githubral.pas`, `fontes/ralinstallparser.pas` (sem uso) e
  `assets_old/` — que **era** versionado, ao contrário do que a §1 dizia.
  `src/` **não** foi apagado: não é cópia de `fontes/`, e sim telas de um desenho
  anterior (`dmcomponents`, `frconfigrecursos`, `frinstallrecursos`) que não
  existem em lugar nenhum do git — decisão do dono.
- `.lpi`: o modo `Windows32` gerava x86_64 (não tinha `TargetCPU`); criados
  `MacOS64` e `MacOSARM64` com `cocoa` e sem `dsymutil`.
- Compilado: win32 (Lazarus 4.8), win64 e linux64 (4.6), macOS x86_64 (2.2.6 da
  raiz, a única com as units de darwin). macOS aarch64 não foi testado: nenhuma
  instalação da máquina tem o `ppca64`. A leitura de saída do processo compilou,
  mas ainda não foi exercitada com uma falha real do `lazbuild` pela tela.

### F1 — Descoberta de IDE
`TIDEInstance` com raiz, versão, plataformas disponíveis, caminho de configuração
e capacidades.
- **Delphi:** registro **e** varredura de pastas — `bin\rsvars.bat` entrega `BDS`
  e `BDSCOMMONDIR`, `bin\bds.exe` entrega o VerInfo. Tabela BDS → produto →
  sufixo em **dado**, não em `if` encadeado (o salto do 23.0 para o 37.0 mostra
  por quê). Instalação fora do registro entra na lista com aviso e com as ações
  que ela suporta.
- **Lazarus:** raízes candidatas configuráveis (pasta apontada pelo usuário,
  `%LOCALAPPDATA%`, `/usr/lib`, `/Applications`, `$HOME`) em vez de disco
  inteiro, com a varredura completa como opção explícita; PCP por instalação,
  incluindo o layout do fpcupdeluxe (`<raiz>\lazarus` com `fpc` e
  `config_lazarus` ao lado).
**Pronto quando:** a lista traz todas as IDEs da máquina, registradas ou não,
cada uma com versão, plataformas e configuração corretas.

**Estado (2026-09-17):** feito, falta commitar e testar pela tela.
- Núcleo sem LCL em `fontes/nucleo/` (não em `src/nucleo/`: `src/` é a pasta
  antiga não versionada): `RALInst.IDE` (`TIDEInstance`, `TIDEList`, base de
  varredura `TBuscaIDE`), `RALInst.IDE.Delphi` (só Windows) e
  `RALInst.IDE.Lazarus`. As units antigas viraram casca: `ideutils` embrulha
  uma `TIDEInstance`, `delphiutils` e `lazarusutils` perderam a descoberta.
- **Delphi:** registro em HKCU e em HKLM nas duas visões (o instalador da
  Embarcadero é 32 bits e grava em `WOW6432Node`), mais as pastas-mãe e avós das
  IDEs registradas, `Program Files` e as pastas que o usuário aponta. Cada pasta
  se identifica sozinha: `bin\dcc32.exe` obrigatório; versão pelo fim do
  `BDSCOMMONDIR` do `rsvars.bat`, pelo nome da pasta ou pelo sufixo de
  `bin\dcc32<N>.dll`/`DCC<N>.dll`; Delphi 7 por `delphi32.exe`. Tabela de 22
  produtos (D7 ao 13 Florence) com BDS, `VERxxx`, sufixo e chave de registro.
  Plataforma só conta com `lib\<plat>` **e** o `dcc` correspondente em `bin\`.
- Avisos do Delphi: fora do registro; nunca aberta nesta conta (sem chave em
  HKCU — aí some a capacidade de instalar na IDE, porque escrever antes da
  primeira abertura faz a IDE pular a cópia dos próprios pacotes); duas pastas
  da mesma versão dividindo a mesma chave; sufixo em disco diferente da tabela.
- **Lazarus:** PCP por `lazarus.cfg` → layout fpcupdeluxe (`config_lazarus` ao
  lado) → padrão do sistema; versão por `components/lazutils/lazversion.pas` →
  `ide/version.inc` → VerInfo; FPC pelo `environmentoptions.xml` da PCP e
  plataformas pelas pastas `units/<cpu-os>`. Avisos: PCP compartilhada, PCP que
  pertence a outro Lazarus, compilador não achado, pasta sem escrita.
  O `lazbuild` agora recebe `--primary-config-path` (antecipado da F5).
- Tela: a busca rápida roda ao escolher a IDE; o `+` vale para Delphi e Lazarus
  e aceita a pasta que contém várias IDEs; a lupa (só Lazarus) varre os discos
  inteiros. Avisos, origem, PCP e plataformas ficam na dica de cada linha.
- Verificado com `tests/descoberta/descobrir_ides.lpr` (console, win64 e win32)
  nesta máquina: 13 Delphi — os 5 registrados e os 8 só em disco, inclusive
  Delphi 7 e a cópia duplicada do XE2 — e 15 Lazarus apontando `D:\IDE\lazarus`,
  cada um com a própria PCP, em menos de 0,1 s. O nome da pasta mente em alguns
  (`3.6p` é Lazarus 2.0.13, `4.6` é 4.7): vale o que está na árvore. Programa
  compila em win32, win64, linux64 e macOS x86_64.
- Falta: a pasta apontada pelo usuário não é lembrada entre execuções (entra
  com o perfil da F9), e nenhuma raiz padrão do Lazarus existe nesta máquina —
  o caminho `C:\lazarus`/`Program Files` só foi exercitado pelo código, não por
  uma instalação real. Linux e macOS compilam, mas a descoberta não rodou neles.

### F2 — Catálogo e grafo
Parse de `.dpk` (`requires`, `{$RUNONLY}`, `LIBSUFFIX`), `.dproj`
(`DCC_UsePackage`, `DCC_Description`) e `.lpk` (`RequiredPkgs`, `Type`),
montagem do grafo, ordenação topológica e classificação compile-only × install.
Saída: um **plano** legível antes de qualquer escrita.
**Pronto quando:** um pacote novo colocado em `pkg/**` aparece na árvore de
recursos, na posição certa da ordem, sem editar o manifesto.

**Estado (2026-09-17):** núcleo feito e testado; a árvore de recursos da tela
**ainda não** usa o catálogo — falta commitar e ligar a tela.
- `fontes/nucleo/RALInst.Catalogo.pas`, sem LCL: varre `pkg/Delphi/**/*.dpk` e
  `pkg/Lazarus/**/*.lpk` (pulando `__history`, `__recovery`, `backup`, `lib`),
  lê nome, descrição, `{$RUNONLY}`/`{$DESIGNONLY}`/`{$LIBSUFFIX}`, `requires` e
  `contains` do `.dpk` (depois de tirar os três tipos de comentário) e `Name`,
  `Type`, `Description`, `RequiredPkgs` com `MinVersion` e `Files` do `.lpk`
  (DOM do FPC, não regex). Separa o `requires` em **internos** (pacotes do
  catálogo) e **externos** (`rtl`, `designide`, `FireDAC`, `indylaz`,
  `mormot2`…), ordena com Kahn (desempate: raiz antes de subpasta, depois nome)
  e oferece `Fechamento` (pedidos + dependências, em ordem) e `Plano` (texto
  legível, antes de qualquer escrita).
- **Implícitos:** o `DCC_UsePackage` do `.dproj` entra sem o sufixo numérico
  (`IndyCore160` → `IndyCore`), descontado o que o grafo já traz. No RAL real
  isso devolve exatamente `IndyCore, IndyProtocols, IndySystem` para o
  `IndyRAL` — o dado que a F3 precisa para resolver o sufixo. Mas os `.dproj`
  são sujos: aparecem também `RESTDWCore`, `uniGUI26Core`, `fmxFireDAC`…
  Implícito é **pista**, não exigência; a F3 não pode tratá-los como `-LU`
  obrigatório.
- **Avisos do plano:** fonte de `contains`/`Files` que não existe em disco
  (submódulo não baixado — a F7 entra aqui), pacote vazio (`XSocketRAL.dpk` não
  contém unidade nenhuma e hoje seria "instalado"), ciclo (fica sem ordem e vira
  erro) e pacote ilegível (vira erro sem derrubar o resto).
- Verificado com `tests/catalogo/catalogo.lpr`: `--testes` monta uma árvore
  sintética (pacote novo em subpasta com nome que ordenaria antes, `__history`,
  ciclo, `.dpk` quebrado, `RunTimeOnly`, submódulo ausente, `.dproj` com sufixo)
  e confere 32 afirmações, contra o disco e contra uma origem em memória — todas passam. Sobre o RAL real: 16 pacotes Delphi e
  14 Lazarus em `PascalRAL-dev` e em `PascalRAL-1.2`; sobre o
  `tests/PascalRAL.zip`, brotli e ZSTD saem como submódulos a baixar. Compila para
  win64, linux64 (linkado) e darwin (units).
- Diferenças que o catálogo revelou e um manifesto teria escondido:
  `RALDBFireDACObjects` não depende de `RALDBPackage`; `ralwizardpack.lpk` não
  exige `PascalRAL`; `pascalral.lpk` não declara `Type` (vale
  runtime+design); nenhum pacote de motor declara a biblioteca do motor no
  `.dpk`, só no `.lpk`.
- **Revisto pelo fluxo da §0:** o catálogo lia o disco, mas as opções são
  escolhidas antes do download. Agora ele lê uma `TOrigemArquivos` (listar,
  existe, ler, localizar; caminhos relativos com `/`), que o catálogo passa a
  possuir. `TOrigemLocal` é a árvore em disco; a origem do GitHub é da F8. O
  autoteste roda as mesmas conferências contra o disco e contra uma origem só em
  memória, que prova que o catálogo não toca o disco.
- **Submódulos pelo `.gitmodules`:** unidade que mora num submódulo não é mais
  "fonte ausente" — vira `Submodulos` do pacote (e `SubmodulosAusentes`, quando
  a origem não o tem). O plano diz quais submódulos baixar junto com o RAL e de
  onde. No `dev` real: `RALZStdCompress` → `src/others/ZSTD`,
  `ralbrotlicompress` → `pascal_brotli`, `raldbbson` → `kxBSON`.
- **Achado:** `RALBSONStorage.dpk` (Delphi) não lista a unidade do kxBSON no
  `contains`, ao contrário do `raldbbson.lpk`; o catálogo não tem como saber que
  o pacote Delphi precisa do submódulo. Ou o `.dpk` passa a listá-la, ou isso
  vira dado (F6).
- **Por que a tela ficou de fora:** a árvore de recursos (`ufrm_recursos`) e o
  motor Lazarus (`lazarusutils.Install`) leem a seleção do `TInstaller` do
  manifesto antigo. Trocar só a árvore deixaria dois modelos de seleção
  diferentes na mesma rodada. A troca entra junto com o motor que consome o
  plano (F3/F5) e com a origem remota do catálogo (F8).

### F3 — Motor de build Delphi *(só Windows)*
Tudo por linha de comando, a partir do executável FPC: `TProcess` chamando as
ferramentas que vêm com a IDE. `dcc32`/`dcc64` com `-U`/`-I`/`-LE`/`-LN`/`-NS`,
`.res` gerado via `brcc32`,
`-LU` montado a partir do `requires` mais as dependências externas resolvidas
(Indy sufixado ou não, FireDAC, mORMot2, Zeos), saída em `<BDSCOMMONDIR>\Bpl` e
`<BDSCOMMONDIR>\Dcp`. Conferência de sanidade pelo tamanho do `.bpl`: `IndyRAL`
em torno de 45 KB, `RALDBFireDACLink` em torno de 100 KB — 1,5 MB e 3 MB
significam terceiros linkados estaticamente, e a IDE vai recusar o pacote.
**Pronto quando:** compila a partir de um zip recém-baixado, em pelo menos duas
versões de Delphi diferentes, com os tamanhos dentro do esperado.

**Estado (2026-09-17):** motor pronto e exercitado contra o RAL real; falta ligar
na tela (depende da F4) e commitar.
- `fontes/nucleo/RALInst.Processo.pas`: executa e lê a saída enquanto o processo
  roda, sem LCL — serve GUI, CLI e testes. `fontes/nucleo/RALInst.Build.Delphi.pas`:
  `dcc32`/`dcc64` + `brcc32`, `--no-config`, `-B -Q`, sem informação de depuração,
  `-NS` do padrão mais o `DCC_Namespace` do `.dproj`, `-U`/`-I`/`-R` com o `lib\<plat>\release`
  da IDE, o `Dcp` da IDE, as pastas das unidades do pacote e as dos submódulos,
  `-O` para o `.obj` do brotli, `-LE`/`-LN` para `<BDSCOMMONDIR>\Bpl` e `\Dcp`.
- **`-LU`:** internas sempre (são compiladas na mesma rodada, antes); externas
  declaradas e implícitas só quando o `.dcp` existe em disco, com o sufixo da
  IDE resolvido pelo nome do arquivo. As implícitas passam por lista fechada
  (Indy, Zeos) mais o prefixo `FireDAC`, porque o `DCC_UsePackage` dos `.dproj`
  carrega lixo de outros projetos.
- **Falha não derruba a rodada inteira**, só quem depende do pacote que falhou.
  `F2613`/`F2063`/`E1026`/`E2202` viram *"dependência externa ausente: unidade X"*,
  e submódulo ausente é recusado **antes** de chamar o compilador, pelo que a F2
  já sabe.
- Verificado em `PascalRAL-dev` com Delphi 12 (BDS 23.0), saída em pasta
  temporária: **14 dos 16 pacotes compilam**, apontando mORMot2 e Zeos por
  `--caminho=`. Faltam só `SaguiRAL` (libsagui não está na máquina) e `UniGUIRAL`
  (comercial) — as duas dizem qual unidade faltou.
- Os tamanhos confirmam o que o plano previa: `IndyRAL` **47 KB** e
  `RALDBFireDACLink` **105 KB** com o `-LU` certo. Antes de o FireDAC inteiro
  entrar no `-LU`, o mesmo pacote saía com **1131 KB** — era a biblioteca
  linkada estaticamente, que a IDE recusaria.
- O aviso de tamanho só dispara quando o terceiro *deveria* ter vindo como
  pacote. `SynopseRAL` sai com 4,8 MB porque o mORMot2 é library path, sem
  pacote: ali o `.bpl` grande é o certo, e avisar seria mentira.
- Win64 compila (`IndyRAL` 84 KB). Sobre o `tests/PascalRAL.zip` (sem `.res` e
  sem submódulos), os `.res` são gerados por `brcc32` e o que depende de
  submódulo é recusado com o nome do submódulo.
- Ferramenta de verificação: `tests/build/compilar_delphi.lpr` (`--listar`,
  `--bds=`, `--plataforma=`, `--bpl=`, `--dcp=`, `--caminho=`, `--simular`).
- **Não escreve em `<BDSCOMMONDIR>\Bpl` por padrão nos testes** — todas as
  rodadas acima usaram pasta temporária, para não mexer no Delphi da máquina.
- Em aberto na F3: `PascalRALDsgn` compila também para Win64 (é
  runtime+design, sem `{$DESIGNONLY}`); instalar na IDE segue só Win32 (F4).

### F4 — Registro e paths do Delphi *(só Windows)*
`TRegistry` do FPC, sem passar por ferramenta nenhuma da IDE:
`Known Packages`, limpeza de `Disabled Packages`,
`Library\<plataforma>\Search Path` e `Browsing Path`, `Environment Variables`
(`$(PascalRAL)`), detecção de `bds.exe` em execução. O **modo "somente library
path"** é esta fase sem a F3. Todo caminho escrito aponta para a pasta que o
usuário escolheu (§0), nunca para temporário.
**Pronto quando:** abrir a IDE mostra os componentes na paleta; e, no modo
apenas-paths, um projeto compila com `uses RALServer` sem nada instalado.

**Estado (2026-09-18):** motor pronto e exercitado sobre uma **cópia** do
registro do Delphi 12; ligado na tela; falta a verificação na IDE de verdade.
- `fontes/nucleo/RALInst.Registro.Delphi.pas`: `Known Packages`, limpeza de
  `Disabled Packages`, `Library\<Plataforma>\Search Path` (só `Library` antes
  do XE2), `Environment Variables`, detecção de `bds.exe`/`delphi32.exe` **daquela
  pasta** em execução (testada com um processo falso: acusa a pasta certa e não
  a outra). Toda escrita guarda o valor de antes.
- `fontes/nucleo/RALInst.Instalar.Delphi.pas`: a rodada inteira — confere
  (chave existe, IDE fechada, pacotes conhecidos) antes de tocar em nada,
  compila (F3) por plataforma, e só então escreve, e só o que compilou. Saída
  dos `.bpl`/`.dcp` vem do `Package DPL/DCP Output` da própria IDE.
- **Library path aponta para os fontes, como a wiki ensina**: variável
  `$(PascalRAL)` = `<pasta>\src` e `$(PascalRAL)\base`, `\utils`... Trocar a
  pasta do RAL depois é mudar a variável. A pasta de cada submódulo que mora ao
  lado de uma unidade escolhida entra também (kxBSON, ZSTD, brotli). Browsing
  Path não é tocado: os fontes já estão no Search Path.
- Sem duplicar: o que já está na lista, escrito com variável ou por extenso,
  não entra de novo. Na máquina de desenvolvimento só entraram as três pastas
  de submódulo; a segunda rodada seguida faz **1** alteração (a limpeza do
  `Disabled Packages` plantado para o teste).
- O mesmo `.bpl` registrado de outra pasta sai de `Known Packages` (a IDE
  carregaria os dois). Outra cópia do RAL no library path vira aviso, com a
  unidade que denunciou.
- **Recibo** em JSON (`<AppConfigDir>\recibos\delphi-<bds>-<data>.json`): IDE,
  fontes, resultado por pacote e plataforma, e cada valor do registro com o
  antes e o depois. `DesfazerRecibo` volta o registro ao que era — conferido:
  instalar e desfazer na cópia dá **0** diferenças contra o registro original.
  Os `.bpl` gravados não voltam; isso fica para a F10.
- Ferramenta: `tests/instalar/instalar_delphi.lpr` (`--chave-teste` trabalha
  numa cópia em `HKCU\Software\RALInstaller-Teste`, `--reusar-chave`,
  `--somente-paths`, `--win64`, `--plano`, `--simular`, `--desfazer=<recibo>`).
- Falta: abrir o Delphi depois de uma instalação real e ver a paleta — é a
  verificação que o dono do projeto vai fazer.

### F5 — Motor Lazarus correto
`--primary-config-path` por instalação, `--lazarusdir`, `--cpu`/`--os`/`--ws`,
ordem vinda do grafo, **um** `--build-ide` no fim, erro lido do processo.
**Pronto quando:** duas instalações de Lazarus na mesma máquina recebem
conjuntos diferentes de pacotes sem uma pisar na outra.

**Estado (2026-09-18):** motor pronto e exercitado sobre uma **cópia** da
configuração do Lazarus 4.7 (`D:\IDE\lazarus\4.6`), sem `--build-ide`.
- `fontes/nucleo/RALInst.Instalar.Lazarus.pas`: `--primary-config-path` e
  `--lazarusdir` em toda chamada; `--add-package-link` com todos os `.lpk` da
  rodada, `--add-package` só com os de design, um `--build-ide=` no fim.
  Pacote com submódulo ou fonte ausente fica de fora, e quem depende dele também.
- Pacote externo que a IDE não conhece (nem em `packager/globallinks`, nem no
  `packagefiles.xml` da configuração) vira aviso no plano: nesse Lazarus,
  `indylaz` e `zcomponent`.
- **O teste real achou um erro da F2:** `.lpk` sem `<Type>` era tratado como
  runtime+design, mas o padrão do Lazarus é RunTime (ele só grava o que difere
  do padrão). O `pascalral.lpk` é assim, e o `lazbuild --add-package` o
  recusava. Corrigido no catálogo e no teste dele.
- Ferramenta: `tests/instalar/instalar_lazarus.lpr` (`--config-teste`,
  `--sem-build`, `--plano`, `--simular`).
- Falta: `--cpu`/`--os`/`--ws`, e o "pronto quando" (duas instalações com
  conjuntos diferentes) verificado de verdade com `--build-ide`.

### F6 — Manifesto real e matriz de capacidade
Manifesto v3 enxuto, publicado no repositório do RAL e baixado com cache local e
cópia embutida como reserva; matriz IDE × recurso a partir de
`compatibilidade.md` e de `PascalRAL.inc`. O manifesto tem de vir **da mesma
versão do RAL** que o usuário escolheu (é ela que descreve aqueles pacotes), com
a descoberta da F2 como rede de segurança quando os dois discordarem.
**Pronto quando:** recurso incompatível com a IDE escolhida aparece desmarcável
e explicado, em vez de falhar na compilação.

**Estado (2026-09-23):** feito; a matriz conferida contra as 13 instalações
Delphi e as 14 Lazarus desta máquina, só lendo; ligado na tela.
- **Mudança de desenho: a matriz sai do disco, não de uma tabela.** Pelo mesmo
  motivo da F2 — pacote novo não pode depender de alguém lembrar de escrever a
  regra. `RALInst.Compatibilidade`:
  - **Pacote que a IDE não tem:** o que o `requires` exige, e o Indy/FireDAC
    que o `.dproj` usa, tem de existir como `.dcp` em `lib\win32\release` ou
    `<BDSCOMMONDIR>\Dcp`. O XE2 e o 2010 não têm `FireDAC.dcp`.
  - **Unidade da RTL que a IDE não tem:** os `uses` das unidades do pacote
    (namespaces `System.`, `Vcl.`, `FireDAC.`...) têm de existir como `.dcu`
    em `lib\win32\release`. Os `{$IFDEF}` são avaliados com os símbolos daquela
    IDE (`DELPHIXE4UP` do `PascalRAL.inc`, `VER230`, `MSWINDOWS`, `UNICODE`);
    símbolo desconhecido e `{$IF expressão}` deixam o trecho de fora — na
    dúvida, não acusa. Assim o `NetHttpRAL` sai do XE8 para baixo
    (`System.Net.HttpClient`), e o `RALDBFireDACLink` sai do XE5, que entra no
    `{$IFDEF DELPHIXE4UP}` do `RALDBFireDAC.pas` e não tem
    `FireDAC.Stan.StorageBin`/`StorageJSON`.
  - **Dependência sem versão para aquele compilador:** vem da receita (abaixo).
    Se a IDE já tem a dependência, vale a que está lá (§8), seja qual for.
- **Manifesto só para o que não dá para deduzir** (`manifesto/ral.json`,
  embutido como `RCDATA MANIFESTO_RAL`; um `ralinstaller.json` na raiz da versão
  do RAL substitui — responde "onde mora o manifesto" da §8: na própria versão,
  com o embutido de reserva). Duas regras de pacote, da wiki (`SynopseRAL` e
  `SaguiRAL` a partir do 2009), e a versão de dependência que o RAL exige: o
  mORMot2 do `master` (saiu da receita, que voltou a `estavel`). Formato
  fechado como o das receitas: campo desconhecido recusa o manifesto inteiro, e
  um recusado não apaga o anterior.
- **Versão da dependência por compilador:** `fonte.versoes` na receita, regras
  com faixa (`delphi-min/max` pelo nome do produto — XE8, 10.1, 12 —,
  `lazarus-min/max`, `fpc-min/max`, `sistemas`) e `versao` ou `incompativel`;
  a primeira que vale decide. Zeos: `8.0-patches` no FPC 3.3; **nenhuma** do
  3.2.3 em diante (conferido: 3.2.3 e 3.2.4 falham nas duas; 3.3.1 compila o
  8.0-patches); a estável no resto. FPC de versão desconhecida não casa regra
  nenhuma — fica a padrão.
- **Versão do FPC:** `fpc -iV` quando o caminho não diz (o fpcupdeluxe não diz);
  antes, os Lazarus do fpcupdeluxe ficavam sem versão de compilador.
- **A mesma dependência em duas versões:** as pastas já eram
  `dependencias/<nome>/<versão>/`; `PastasDependencias` passou a ser
  `nome@versão=pasta` (`ChaveDependencia`), e cada IDE pede a sua
  (`VersaoDependencia` nos dois motores). `TBaixaDependencia.VersaoPedida`.
- **Tabela de produtos do Delphi** foi de `RALInst.IDE.Delphi` para
  `RALInst.IDE` (é dado, compila em qualquer sistema) com `ProdutoPorNome`.
- **Tela:** a árvore diz, por pacote, em quais IDEs marcadas ele fica de fora e
  por quê; fora de todas, fica indisponível e não marca. O plano de cada IDE
  lista "fica de fora: X — motivo", e só baixa a dependência na versão que
  alguma IDE marcada de fato pede. Leitura do zip da versão: ~230 ms por versão
  de IDE na primeira vez, depois em cache.
- **Rodadas reais:** XE2 (cópia do registro) — os três pacotes de fora
  anunciados no plano; Delphi 12 — 6/6 Zeos + 6/6 RAL com as dependências por
  versão (`mORMot2@master`, `Zeos@estavel`); Lazarus 4.7 (FPC 3.2.3) —
  `raldbzeoslink` fora, com o motivo, sem baixar o Zeos; Lazarus 4.99 (FPC
  3.3.1) — Zeos `8.0-patches`.
- **Achado no RAL, não no instalador:** o `dev` e o `1.2` não compilam mais no
  XE2 — `RALTools.pas:531` chama `AtomicIncrement`, que só existe do XE3 em
  diante (o `master` não tem isso). O instalador mostra o erro com a linha. Se
  for decisão do projeto subir o mínimo para XE3, é regra para o
  `ralinstaller.json` daquelas versões, não para o manifesto embutido.
- Ferramenta: `tests/compatibilidade/compatibilidade.lpr` (`--testes`: 43
  testes sem IDE; `<raiz ou .zip> [--lazarus=pasta]`: a matriz da máquina).
  `instalar_delphi` e `instalar_lazarus` ganharam `--manifesto=`.
- Falta: a checagem de unidade do lado Lazarus (hoje só faixa e dependência —
  pacote externo ausente continua aviso no plano).

**Estado (2026-09-23, segunda rodada):** Zeos, AnyDAC e plataformas.
- **Zeos: matriz de verdade** (zcore…zcomponentdesign pelo `lazbuild -B`, em
  cópias dos fontes e da configuração):

  | Zeos | FPC 3.2.2 | 3.2.3 | 3.2.4 | 3.3.1 |
  | --- | --- | --- | --- | --- |
  | 8.0.0-stable (OPM = tag do espelho) | ok | falha | falha | ok |
  | 8.0-patches (SVN do SF e espelho) | falha | falha | falha | ok |
  | trunk (espelho, 2026-08) | falha | falha | falha | ok |
  | `frones/ZeosLib` master (trunk 2025-07) | ok | ok | ok | ok |

  O defeito é **do fonte do Zeos, não do OPM**. A estável falha em
  `ZAbstractRODataset.pas(5929)` ("Expected another 1 array elements") nos
  *fixes* 3.2.3/3.2.4. O 8.0-patches e o trunk atuais: `ZeosLazarus.inc` define
  `HAVE_TFORMATSETTINGS_CREATE` no bloco `FPC_FULLVERSION>=30200`, mas
  `TFormatSettings.Create` só existe no FPC 3.3 (`ZCbor.pas(854)`). **Mover
  essa linha para o bloco `>= 30300` resolve**: conferido, o 8.0-patches passa a
  compilar nos quatro FPC. É para reportar no SourceForge.
- **Receita do Zeos:** FPC 3.3 → `8.0-patches`; FPC 3.2.3/3.2.x →
  `frones/ZeosLib:master` (regra com `github` próprio: a versão vira
  `dono/repo:ref`, a pasta `dependencias/Zeos/frones-master`); o resto →
  estável. Ponta a ponta no Lazarus 4.7 (FPC 3.2.3, configuração numa cópia):
  baixou o master do frones (commit `66c3ce2`), registrou, e o `raldbzeoslink`
  compilou contra ele.
- **Cópia já instalada que não compila (o alerta):** `verificacoes` no bloco da
  receita — arquivo da cópia, expressão, faixa e aviso. A raiz vem do link do
  `.lpk` em `packagefiles.xml`. Casou: o pacote que precisava dela fica de fora,
  com o aviso (o instalador não mexe na cópia do usuário, §8). Duas no Zeos: o
  define no lugar errado (com a correção de uma linha no aviso) e a 8.0.0
  "release" no FPC 3.2.3/3.2.x. Conferido contra as cópias desta máquina: o SVN
  acusa no 3.2.3 e não no 3.3.1; o do GitHub não acusa; o do OPM acusa no 3.2.3
  e não no 3.2.2 — igual à matriz. Link de `.lpk` que não existe mais deixou de
  contar como "já instalado".
- **AnyDAC (XE2 a XE4):** receita `firedac.json` ("FireDAC/AnyDAC", comercial,
  só detecção): vale o `FireDAC.dcp` da IDE ou o `AnyDAC_Comp_D*.dcp`. Campo
  novo `pacotes-ligados`: os `.dcp` da dependência entram no `-LU`, senão o
  `dcc32` embute as `uAD*` no `.bpl` e a IDE o recusa ao lado do AnyDAC. No XE2
  daqui, `RALDBFireDACLink` passou a caber e compilou até o código do RAL:
  **o ramo AnyDAC do RAL não compila** — `TADErrorEvent` recebe
  `AInitiator: IADStanObject`, o RAL declara `TObject` (`RALDBFireDAC.pas`).
  Dependência comercial não instalada também passou a deixar o pacote de fora
  na tela (uniGUI, AnyDAC), não só na execução.
- **O RAL no XE2:** além do `AtomicIncrement` do `dev`, o **1.1** também não
  compila: `RALClient.pas(991)` atribui `CharRAL` (Char) a um `UTF8String`.
  Com um `AnsiChar(...)` ali (só na cópia de teste), PascalRAL, PascalRALDsgn e
  RALDBPackage compilam.
- **Plataformas:** o catálogo lê do `.dproj` as plataformas habilitadas; fora
  da lista, o pacote é pulado naquela plataforma, com o motivo, e quem depende
  dele também. No Delphi 12 com Win64: `XSocketRAL` e `KwikRAL` pulados em
  Win64 ("o .dproj não habilita win64"), `IndyRAL` compilou. O relatório passou
  a dizer "pulado" (não "FALHOU") para pulado.
- `CompararVersoes` aceita `x`: `"fpc-max": "3.2.x"` é "qualquer 3.2".
- Ferramentas: `compatibilidade --testes` cobre a receita real do Zeos e as
  cópias da máquina; `instalar_lazarus --config=<pasta>` usa uma configuração
  preparada para o teste.

### F7 — Dependências: baixar e instalar sozinho
Marcar `SynopseRAL` tem de bastar: o instalador baixa o mORMot2 na versão
suportada mais recente, instala do jeito que aquela dependência pede, e só então
compila o pacote do RAL que precisava dela. O mesmo vale para Indy atualizado,
Zeos, libsagui, brotli, ZSTD e kxBSON — e cada uma se instala de um jeito
diferente, o que é exatamente o motivo de isto ser **dado, não código**.

**A receita, uma por dependência, em JSON**, publicada junto do manifesto:

```json
{
  "nome": "mORMot2",
  "fonte":     { "tipo": "github", "repo": "synopse/mORMot2", "pasta": "mormot2" },
  "versoes":   { "min": "2.1", "preferida": "estavel", "max": "" },
  "extras":    [ { "link": "https://synopse.info/files/mormot2static.tgz",
                   "formato": "tar_gzip", "pasta": "mormot2/static" } ],
  "deteccao":  [ { "acao": "envvar-existe", "nome": "mormot2" },
                 { "acao": "unidade-existe", "nome": "mormot.core.base.pas" } ],
  "requisitos":{ "ide-min": "2009", "plataformas": ["win32","win64","linux64"],
                 "pago": false },
  "delphi":    [ { "acao": "envvar", "nome": "mormot2", "valor": "{raiz}/src" },
                 { "acao": "libpath", "paths": ["$(mormot2)", "$(mormot2)/core", "..."] } ],
  "lazarus":   [ { "acao": "lpk", "arquivo": "packages/lazarus/mormot2.lpk",
                   "modo": "instalar" } ]
}
```

O vocabulário de ações é **fechado e pequeno**: `baixar-repo`, `baixar-arquivo`,
`descompactar`, `libpath`, `browsingpath`, `envvar`, `dpk` (compilar e/ou
instalar, com o sufixo da IDE resolvido pela regra da §3), `lpk`
(`--add-package` ou `--add-package-link`), `copiar-arquivo` (as DLLs da
libsagui, do brotli e do ZSTD). **Não existe ação "rode este comando"**: a
receita vem da rede, e uma receita capaz de executar qualquer coisa transformaria
o instalador num vetor de ataque. Ação que a receita pedir e o instalador não
conhecer é recusada com o nome dela no log, nunca ignorada em silêncio.

Regras que valem para toda dependência:

- **Detectar antes de baixar.** Achando a dependência já instalada (variável de
  ambiente apontando para ela, unidade no library path, `.dcp` presente, `.lpk`
  já instalado), o padrão é **usar a que está lá** e dizer isso; baixar por cima
  é escolha explícita do usuário. Ninguém quer descobrir que o instalador
  substituiu o mORMot2 de produção dele. *Em aberto (§8):* o fluxo da §0 diz
  "baixar tudo"; esta regra é a exceção proposta, a confirmar.
- **Versão suportada, não a última.** A receita declara faixa (`min`, `max`,
  `preferida`); o instalador pega a tag estável mais nova **dentro da faixa**.
  Sem faixa, a estável mais recente, e o log diz qual foi.
- **Pago é só detecção.** UniGUI e afins não têm download: a receita marca
  `"pago": true`, o instalador procura, e se não achar desmarca o recurso
  explicando que a dependência é comercial e precisa ser instalada pelo usuário.
- **Dependência entra no grafo da F2**, antes do pacote que a usa; falhar em
  instalar uma dependência derruba só os recursos que dependiam dela, não a
  rodada inteira.
- **Submódulos do RAL (ZSTD, pascal_brotli, kxBSON) não precisam de receita.** O
  zipball não os traz, mas a árvore do GitHub diz o commit exato de cada um
  (`mode 160000`, `type commit`) e o `.gitmodules` diz o repositório: baixa-se o
  zipball daquele commit direto para o caminho do submódulo. É a versão que o
  RAL fixou, sem faixa nem escolha. Receita fica para o que é dependência de
  verdade (mORMot2, Zeos, Indy, libsagui).

**Pronto quando:** numa IDE limpa, marcar `SynopseRAL` + `RALZStdCompress` baixa
e instala mORMot2 e ZSTD e compila os dois pacotes do RAL, sem o usuário apontar
pasta nenhuma; e uma dependência nova é atendida **só** acrescentando um arquivo
de receita, sem tocar no Pascal.

**Estado (2026-09-23):** feito e exercitado de ponta a ponta no Delphi 12 (registro
numa cópia) e em três Lazarus (configuração numa cópia); ligado na tela.
- **Quem precisa de qual receita sai dos próprios pacotes.** O catálogo passou a
  ler o caminho de busca de cada pacote (`DCC_UnitSearchPath` do `.dproj`,
  `OtherUnitFiles` do `.lpk`): o `SynopseRAL.dproj` declara `$(mormot2)`, e é
  assim que o Delphi diz que precisa do mORMot2, que não tem pacote. A receita
  diz o que **fornece** (variáveis e pacotes); `ZComponent`, `indylaz`,
  `zcomponent` e `mormot2` casam pelo `requires`. Só o que não é declarado em
  lugar nenhum (uniGUI) usa a lista `pacotes-ral`. O mesmo caminho de busca
  resolveu o `SaguiRAL` da F3: o `libsagui.pas` mora em `src\others`, que o
  `.dproj` declara e o `.dpk` não lista.
- `receitas/*.json` (mORMot2, Zeos, Indy, uniGUI), embutidas no `.exe` como
  `RCDATA RECEITA_*`; uma pasta `receitas` ao lado do executável ou na pasta de
  dados acrescenta ou substitui, **sem recompilar**. Vocabulário fechado —
  `variavel`, `libpath`, `dpk` (só Delphi), `lpk` (só Lazarus): ação ou campo
  desconhecido recusa a receita inteira, com o nome (testado com `executar`,
  campo estranho e `lpk` no bloco do Delphi).
- `RALInst.Receitas`, `RALInst.Dependencias` (download para
  `<pasta>/dependencias/<nome>/<versão>/`, com a marca e a troca segura da F8,
  mais os extras de release), `RALInst.Tar` (extrator `.tgz` novo: o
  `tar_gzip` antigo lia o tamanho errado e não conhecia `pax`; o novo bate
  arquivo por arquivo com o `tar` do Windows nos 108 arquivos do
  `mormot2static.tgz`).
- **Detectar antes de baixar** (resolve a pergunta da §8): variável da IDE,
  unidade no library path ou `.dcp` no Delphi; pacote registrado no Lazarus.
  Achou, usa a que está lá e diz onde; só baixa o que alguma IDE marcada não
  tem. `--ignorar-existentes` nas ferramentas força o caminho do download.
- **No Delphi:** `variavel` + `libpath` no registro, os caminhos entram no `-U`
  da compilação do RAL, e `dpk` compila e registra os pacotes da dependência com
  o mesmo motor da F3 (catálogo com `PastaDelphi` por versão do BDS). O que
  depende do que faltou fica de fora, com o motivo — e quem depende dele também.
  Rodada real: Zeos 6/6 + RAL 7/7 (IndyRAL 48 KB, RALDBFireDACLink 105,
  RALDBZeosLink 100, SynopseRAL 4947 com o mORMot2 baixado); UniGUIRAL fora,
  "comercial".
- **No Lazarus:** os `.lpk` da dependência entram antes dos do RAL nas mesmas
  chamadas do `lazbuild` (continua um `--build-ide`). Com Indy e mORMot2
  baixados, `indyral` e `synopseral` compilam (`lazbuild` do pacote sobre a
  cópia da configuração).
- **O que a rodada real ensinou** (e ficou no código):
  - `{$LIBSUFFIX}` muda o nome do `.bpl` (`ZCore280.bpl` — o pacote do Zeos
    para o Delphi 12 declara `'280'`, não o sufixo da IDE); o motor procurava
    `ZCore.bpl`.
  - O `ZPlain` do Zeos usa `Xml.*` sem exigir `xmlrtl`: a IDE resolve sozinha,
    o `dcc32` puro embute (1763 KB) e o pacote seguinte cai em `E2199`. O `-LU`
    passou a oferecer sempre os pacotes-base do Delphi (`rtl`, `vcl`,
    `xmlrtl`, `dbrtl`...) — só entra o que é usado; o `ZPlain` caiu para 581 KB
    e os tamanhos do RAL não mudaram.
  - `F2063` é erro de compilação dentro da unidade, não "dependência ausente":
    a mensagem agora cita o primeiro erro.
  - Caminho longo: exemplos do mORMot2 e documentação do Zeos passam de 260
    caracteres numa pasta funda. Extração e remoção usam `\\?\`. O **FPC não**:
    compilar do Lazarus numa pasta muito funda falha com "unidade não
    encontrada" — a pasta de instalação precisa ser curta.
  - **Versão da dependência importa.** O RAL 1.1 usa
    `mormot.core.os.security`, que a 2.4-stable do mORMot2 não tem: a receita
    usa o `master` (com os estáticos do último release). O Zeos depende do
    FPC: 8.0.0-stable compila no Delphi 12 e no FPC 3.2.2 (o do Lazarus
    oficial); 8.0-patches exige FPC 3.3; no FPC 3.2.3 (*fixes*, do
    fpcupdeluxe) nenhum dos dois compila. A receita usa a estável.
- Ferramentas: `tests/dependencias/dependencias.lpr` (`--listar`,
  `--exigidas=`, `--baixar=`); `instalar_delphi` e `instalar_lazarus` ganharam
  `--receitas=`, `--deps=` e `--ignorar-existentes`.
- Falta: libsagui (a DLL em tempo de execução); `copiar-arquivo` do
  vocabulário ainda não existe. (A versão por compilador e por versão do RAL
  veio na F6.)
### F8 — Qual versão do RAL instalar
Listar os *releases* e as *tags* do repositório do RAL e deixar o usuário
escolher; o padrão é sempre a **estável mais recente** — o release mais novo com
`draft: false` e `prerelease: false`. Também aceitar um branch (`dev`, `master`)
para quem acompanha o desenvolvimento, e uma **pasta local já baixada**, que é o
caso de quem desenvolve o próprio RAL. A versão escolhida vai para o recibo da
F10, e é ela que decide qual manifesto (F6) será usado.

Escolhida a versão, o catálogo daquela referência vem **sem baixar o RAL**
(§0): `TOrigemGitHub`, filha da `TOrigemArquivos` da F2, lista com uma chamada a
`git/trees/<ref>?recursive=1` (verificado no `dev`: 126 KB, sem truncar, com os
três submódulos e o commit de cada um) e lê os arquivos pelo
`raw.githubusercontent.com/<dono>/<repo>/<ref>/<caminho>`, fora do limite da
API. Cache em disco por SHA da árvore: a mesma versão não é consultada duas
vezes. O download de verdade (zipball da referência + zipball de cada submódulo
no commit fixado) só acontece na execução, para a pasta escolhida.
**Pronto quando:** abrir o instalador e mandar instalar, sem mexer em nada,
instala a última estável; e escolher uma tag antiga instala aquela, com a tela
dizendo qual versão está sendo instalada em cada IDE.

**Estado (2026-09-22):** feito e exercitado de ponta a ponta contra o GitHub e
o Delphi 12 (registro numa cópia); ligado na tela; falta a verificação pela GUI.
- **Mudança de desenho:** o catálogo das telas vem do **zip da versão**, não da
  árvore + arquivos crus. Uma requisição ao `codeload.github.com` (fora do
  limite da API; 2,5 MB para a 1.1) contra ~47 GETs, e o mesmo zip, guardado
  no cache, é o que vai para a pasta escolhida: nada é baixado duas vezes. A
  API fica para listar versões e, na execução, para o commit dos submódulos
  (`git/trees/<ref>`). O comentário do zip traz o commit exato.
- `RALInst.HTTP`: WinINet no Windows (TLS e proxy do sistema, sem DLL de
  OpenSSL ao lado do `.exe`), `fphttpclient` nos outros. `RALInst.GitHub`:
  releases, tags e ramos (sem os que não são código: `documentation`,
  `external`, `installer`, `tests`), a recomendada é a estável de número mais
  alto (`1.1` hoje; `v1.0` e `1.1` convivem, por isso `CompararTags`).
  `RALInst.Zip`: `TOrigemZip` (catálogo lido de dentro do zip) e a extração
  sem a pasta de topo, recusando entrada com `..`. `RALInst.Fontes`: a pasta
  `<pasta>/PascalRAL/<versão>/`.
- **Cota:** consulta com menos de 10 min no cache nem vai à rede, e a árvore de
  uma tag fica guardada para sempre. O ETag vai junto, mas **sem token o 304
  conta na cota** (conferido: 42 → 41 com `If-None-Match`) — só economiza
  banda. Sem rede ou sem cota, a lista guardada serve e o aviso diz de quando
  ela é. `GITHUB_TOKEN` no ambiente sobe o limite para 5000/h.
- **Submódulos:** só os que os pacotes escolhidos usam (o `pascal_brotli` tem
  9,3 MB). O teste real achou que o `RALBSONStorage` não trazia o kxBSON — o
  `.dpk` não lista as unidades dele. Regra nova no catálogo: **pacote que divide
  uma unidade com um pacote da outra IDE herda os submódulos dele** (o
  `raldbbson.lpk` lista o kxBSON). Com isso o library path do Delphi passa a
  usar a lista de cada pacote, em vez de todo submódulo "ao lado" (que punha o
  brotli no path de quem só pediu BSON).
- **A pasta é tratada como permanente:** extração numa pasta ao lado e troca no
  fim (falha no meio não estraga a que funciona); arquivo de marca
  `.ralinstaller.json` (versão, commit, submódulos); pasta que existe e não tem
  a marca **nunca** é tocada (testado); mesma versão já na pasta baixa só o
  submódulo que faltar; troca de versão mantém os submódulos que a anterior
  tinha.
- Ponta a ponta: `instalar_delphi github:estavel --pasta=... --chave-teste`
  baixou a 1.1 com o kxBSON, compilou 5/5 no Delphi 12 e registrou na cópia. O
  plano avisa quando `$(PascalRAL)` vai mudar de pasta — ela é da IDE, e os
  projetos do usuário que a usam passam a ver a pasta nova.
- Tela: versão (a estável mais recente já escolhida; a última opção é a pasta
  local, para quem desenvolve o RAL), pasta de instalação com o destino e o
  aviso de permanência, recursos do catálogo do zip. A execução tem duas etapas:
  **baixar** (falhou, nenhuma IDE é tocada) e **instalar**.
- Ferramentas: `tests/github/versoes_ral.lpr` (`--listar`, `--catalogo=`,
  `--baixar= --pasta=`); `instalar_delphi` aceita `github:<versão>`.
- Saíram do projeto `installparser`, `githubutils`, `ralzipper` e `http_client`
  (o manifesto de teste e o download antigo); `tar_gzip` fica para a F7.
- Em aberto: escolher a versão numa tela própria, antes das IDEs (§F9); o
  cache de zips não é limpo nunca.

### F9 — UI multi-IDE
Uma lista só, com Delphi e Lazarus juntos, seleção por IDE e por recurso, tela de
plano antes de executar, progresso real e log em arquivo por rodada.

Ordem do wizard que o fluxo da §0 pede: idioma → versão do RAL (F8) → IDEs
(F1) → recursos (catálogo remoto, F2/F8) → **pasta de destino**, com o aviso de
que ela é permanente → plano (o que será baixado, para onde, e o que será
instalado em cada IDE) → execução em duas etapas visíveis: **baixar** tudo,
depois **instalar**. Hoje a pasta está na tela de recursos (`lbedDownloadPath`)
e a execução baixa e instala sem separar as etapas.
**Pronto quando:** uma passada instala em várias IDEs de tipos diferentes e o
relatório final diz, por IDE, o que entrou, o que ficou de fora e por quê.

**Estado (2026-09-18), antecipado para dar o que verificar:** a tela de
recursos lê o **catálogo** (F2) de uma pasta local com os fontes, em vez do
manifesto de teste: pacotes agrupados como em `pkg/<IDE>` (principais, motores,
banco, compressão), marcar um pacote marca o que ele exige ("necessário"),
indisponível diz o porquê (submódulo ausente). Opções do Delphi: somente
library path e Win64. A tela de instalação mostra o **plano** de cada IDE
marcada antes de executar, pede confirmação, e grava o log da rodada em
`<AppConfigDir>\logs`. IDE Delphi sem chave em HKCU não pode ser marcada.
Continua uma IDE de um tipo por rodada, e a origem é pasta local — baixar a
versão escolhida é a F8.

**Estado (2026-09-23):** feito o "pronto quando"; conferido pela tela de
verdade (Delphi 12 + XE2 + Lazarus 4.99, 4.7 e 3.2 numa passada, até o plano,
sem clicar em Instalar).
- **Tela de IDE:** terceira opção "Delphi e Lazarus juntos" (`IDE = 2`, os dois
  destaques acesos); fora do Windows ela some com o Delphi.
- **Lista de IDEs:** as duas buscas juntas; as pastas acrescentadas à mão são
  lembradas em `<dados>\pastas-ide.txt` e entram na busca rápida seguinte.
  Corrigido no caminho: uma `TBuscaDelphi` nova (acrescentar pasta, varredura
  completa) não lia as chaves de HKCU e marcava toda IDE como "nunca aberta",
  sem poder marcar — `Finalizar` agora chama `LerChavesHKCU`.
- **Árvore de recursos unificada:** um nó por nome de pacote sem caixa
  (`IndyRAL` = `indyral.lpk`), "(só Delphi)"/"(só Lazarus)" quando existe de um
  lado só, "[fica de fora em Delphi XE2: …]" quando o manifesto/compatibilidade
  (F6) tira de uma parte das IDEs e "[indisponível: …]" quando tira de todas.
  Cada IDE recebe só os pacotes do tipo dela (`TEscolhaInstalacao.PacotesDoTipo`).
- **Plano:** downloads com a versão por IDE (`Zeos 8.0-patches` para o FPC 3.3,
  `frones/ZeosLib:master` para o 3.2.3+, nada para quem já tem), depois o plano
  de cada IDE com o que fica de fora e por quê.
- **Execução em duas etapas:** fontes → dependências (falha deixa de fora só
  quem precisava) → cada IDE; no fim, "==== Resumo" com uma linha por IDE (o que
  entrou, o que ficou de fora, avisos).
- **Versão** na barra de título e no cabeçalho (o "Installer Version 1.0" vinha
  dos `.po`; `AjustarVersao`). Aviso de versão nova (F12) ao abrir.
- Falta: versão do RAL e pasta de destino em páginas próprias (hoje a versão e
  a pasta estão na tela de recursos); barra de progresso real (hoje é o log).

### F10 — Desinstalar e reinstalar
Recibo em JSON por IDE (o que foi instalado, onde, qual versão do RAL e de cada
dependência, quais paths) → desinstalação e upgrade determinísticos, com rollback
quando um pacote falha no meio. O recibo distingue o que o instalador **instalou**
do que ele apenas **encontrou**: desinstalar não pode levar junto o mORMot2 que
já era do usuário.
**Pronto quando:** desinstalar devolve a IDE ao estado anterior, registro e
library path incluídos.

**Estado (2026-09-23):** feito e conferido nos dois lados, sobre cópias.
- **Recibo por rodada**, nos dois motores, em `<dados>\recibos`: IDE, versão do
  RAL (a marca da pasta), pacotes, e **dependências com a origem** —
  `instalada` (de onde, que versão) ou `encontrada` (onde estava). Desinstalar
  nunca toca a encontrada.
  - Delphi: as escritas no registro com o valor de antes (já existia) e,
    novo, os `.bpl`/`.dcp` gravados, com tamanho e data.
  - Lazarus (novo): as duas listas que o `lazbuild` muda — links de `.lpk`
    (`packagefiles.xml`) e pacotes da IDE (`StaticAutoInstallPackages` em
    `miscellaneousoptions.xml`) —, antes e depois.
- **Desfazer é aplicar o inverso da mudança sobre o estado atual**
  (`RALInst.Config.Lazarus`): o que a rodada acrescentou sai, o que ela trocou
  volta, e o que o usuário mudou depois fica. `.bpl` só é apagado se ainda tem
  o tamanho e a data do recibo. Recibos de uma IDE se desfazem do mais novo ao
  mais velho (desfazer um velho antes poria de volta valores que o novo já
  tinha trocado); desfeito vira `*.desfeito.json`. No Lazarus, tirar pacote da
  IDE reconstrói a IDE uma vez no fim.
- **Rollback:** no Lazarus, `lazbuild` que falha em qualquer etapa (link,
  marcar, `--build-ide`) devolve a configuração ao que era — o executável da
  IDE não mudou, e sem isso ela abriria pedindo para reconstruir com pacotes
  que não compilam. No Delphi o registro já só recebia o que compilou (F4).
- **IDE aberta:** `ProgramaEmExecucao` (`RALInst.Processo`, Toolhelp no
  Windows, `/proc` no Linux) passou a valer para o Lazarus também, na
  instalação e na desinstalação.
- **Conferido:**
  - Lazarus 4.7, configuração numa cópia: instalou RAL 1.1 + Indy + Zeos
    (frones), desinstalou, e as duas listas ficaram **idênticas** às de antes
    (inclusive os links do Zeos que a instalação tinha trocado); o `lazbuild`
    lê a configuração regravada.
  - Delphi 12, registro numa cópia: instalou IndyRAL + RALDBZeosLink,
    desinstalou, e a cópia ficou **idêntica** à chave de verdade (os `.bpl` que
    já estavam registrados antes voltaram), e os 10 `.bpl`/`.dcp` gerados
    foram apagados.
- **Achado no caminho (F7):** dependência encontrada pelo `.dcp` não dizia onde
  estão os fontes, e o `RALDBZeos.pas` inclui `ZComponent.inc` do Zeos — o
  RALDBZeosLink não compilava contra o Zeos que o usuário já tem. Campo novo
  `busca` na receita: as pastas do library path da IDE que têm aqueles
  arquivos entram na compilação. Com isso compilou contra o Zeos desta
  máquina (`$(zeos)\component`).
- **Tela:** a última página diz o que o instalador já pôs nas IDEs marcadas, e
  o link "Desinstalar o RAL destas IDEs" desfaz (com confirmação e log em
  `<dados>\logs\desinstalacao-*.log`).
- Ferramentas: `tests/desinstalar/desinstalar.lpr` (`--listar`,
  `--desfazer=<recibo>`, `--ide=<raiz>`); `tests/recibos/recibos.lpr` (a
  semântica do desfazer e a ida e volta dos dois XML); `--recibos=` nos dois
  `instalar_*`.
- Falta: "reinstalar" como ação própria (hoje é instalar de novo, que passa por
  cima e grava outro recibo).

### F11 — Entrega
CLI com os mesmos verbos da GUI (serve em sessão remota e em servidor de build),
versionamento próprio (hoje o `.lpi` só tem `MajorVersionNr=1`), build mode de
macOS e workflow de release — o repositório tem apenas `FUNDING.yml`, nenhum CI.
Os binários de Linux e macOS saem sem a metade Delphi, e isso é dito na página de
release e na própria tela, para ninguém procurar o que não está faltando.
**Pronto quando:** uma tag gera binários de Windows, Linux e macOS, todos do
mesmo fonte, e a versão do instalador é independente da versão do pacote.

**Estado (2026-09-23):** feito o que dá para conferir aqui; o workflow não foi
rodado (só roda no GitHub, com uma tag).
- **`RALInst.Rodada`** — a rodada inteira sem LCL: versão do RAL (GitHub ou
  pasta local) → catálogo do zip → IDEs, **Delphi e Lazarus juntas** → pacotes
  pelo nome sem caixa (`IndyRAL` vale para o `indyral.lpk`; nome de um lado só
  vale só para aquele) → plano → baixar (fontes, submódulos, dependências por
  IDE e versão) → instalar → desinstalar. Recibos e "IDE aberta" configuráveis
  (os testes apontam para cópias).
- **CLI `cli/ralcli`** (`cli/ralcli.lpi`, com as receitas e o manifesto
  embutidos como na tela): `ides`, `versoes`, `pacotes`, `plano`, `instalar`
  (pergunta antes, `--sim` não), `recibos`, `desinstalar`, `versao`,
  `atualizar`. `--ide=` aceita a raiz ou `#n` da lista. Opção desconhecida é
  erro (código 2). Conferido: plano misto Delphi 12 + Lazarus 3.2 + Lazarus 4.7
  com a dependência certa em cada um (Zeos do usuário no Delphi, o do OPM no
  3.2, o `frones` baixado no 4.7).
- **Rodada de ponta a ponta** (`tests/rodada`, configuração numa cópia): baixou
  o RAL 1.1, Indy e Zeos do GitHub, registrou, os três pacotes compilaram
  contra o baixado, e a desinstalação deixou a configuração **idêntica**.
- **Versão própria:** `RALInst.Versao` (`VersaoInstalador = '0.9.0'`) é a única
  fonte; a tela mostra na barra de título; o `.lpi` foi para 0.9.
- **Workflow** `.github/workflows/instalador.yml`, na tag
  `instalador-v<versão>`: confere que a tag bate com `RALInst.Versao`, grava a
  versão no VersionInfo dos dois `.lpi`, compila Windows64, Linux64 (GTK2) e
  macOS x86_64 com o `gcarreno/setup-lazarus`, roda os testes sem IDE, e
  publica os binários + `SHA256SUMS` com o texto dizendo que Linux e macOS são
  só Lazarus. Fora do Windows, a tela de IDE também diz isso.
- Falta: rodar o workflow de verdade (e o Windows32 e o macOS ARM, que o
  `setup-lazarus` não garante); assinar o binário do macOS.

### F12 — Auto-atualização do instalador
Depende da F11: só faz sentido quando existem releases com binário publicado.

Na abertura, consultar o release mais recente do repositório do instalador e
comparar com a versão do próprio executável (o VerInfo que a F11 passa a
preencher de verdade). Sendo mais novo, oferecer: baixa, troca e reinicia
sozinho, retomando na tela em que estava. Quem acabou de baixar o binário novo
não vê nada disso — a comparação dá igual e o programa segue.

Como a troca é feita, que é onde isso costuma quebrar:

- **Baixar para arquivo temporário e só então promover.** Conferir tamanho e
  hash antes de trocar; download interrompido nunca vira o executável em uso.
- **Windows não deixa apagar um `.exe` em execução, mas deixa renomear.** Então:
  renomeia o atual para `.old`, grava o novo no lugar, reinicia, e a execução
  seguinte apaga o `.old`. Falhando a gravação do novo, desfaz o rename — nunca
  ficar sem instalador.
- **Linux e macOS:** substituir o arquivo e **repor o bit de execução**; no macOS
  o binário assinado perde a assinatura ao ser trocado assim, o que precisa ser
  testado antes de prometer auto-update nesse sistema.
- **Reiniciar preservando a escolha da rodada** (IDEs marcadas, recursos, versão
  do RAL, pastas) num perfil temporário, e voltar com um argumento tipo
  `--pos-atualizacao` para a tela dizer o que acabou de acontecer.
- **Nunca atualizar sozinho na CLI.** Em servidor de build, o binário trocar de
  versão no meio de uma rodada é defeito, não recurso: lá a atualização só
  acontece com `--atualizar` explícito.
- **Falha de rede não é "está atualizado".** Sem internet, ou com a API do GitHub
  devolvendo 403 por limite de requisições, a tela diz *não deu para verificar* —
  são coisas diferentes e não podem virar a mesma mensagem.

**Pronto quando:** um binário de versão anterior, aberto numa máquina com
internet, volta atualizado e no mesmo ponto do wizard; e o mesmo binário, sem
internet, abre normalmente dizendo que não conseguiu verificar.

**Estado (2026-09-23):** feito e testado sem release publicado (não há nenhum
ainda: a consulta real diz "atualizado").
- `RALInst.AutoAtualizacao`: releases do próprio repositório com tag
  `instalador-v*` (os do RAL são `v1.0`, `1.1`...), sem rascunho nem
  pré-lançamento, o de versão mais alta; comparado com `VersaoInstalador`.
  **Falha de rede não é "atualizado"**: sem internet, sem cota, ou lista vinda
  de cache velho → `rvNaoVerificou`, e a tela diz na barra de título que não
  deu para verificar. Versão nova sem binário deste sistema também não vira
  "atualizado".
- Baixa para `<exe>.novo` ao lado do atual, confere **tamanho** e o **SHA-256**
  do `SHA256SUMS` do release (`RALInst.SHA256`, próprio: o `fpsha256` só existe
  do FPC 3.2.3 em diante), troca por rename (atual → `.old`, novo → nome; falhou
  a segunda parte, desfaz a primeira), repõe o bit de execução fora do Windows,
  reinicia com `--pos-atualizacao`; a abertura seguinte apaga o `.old`.
- Tela: consulta uma vez, depois que a janela aparece; oferece, com as notas do
  release. A consulta é no início do wizard, então "voltar no mesmo ponto" é
  voltar no começo.
- CLI: **nunca sozinha** — só `ralcli atualizar` (`--verificar` só diz).
- Testes (`tests/atualizacao`): vetores do FIPS 180-4 (inclusive 1 milhão de
  "a"); escolha do release numa lista montada (rascunho, pré-lançamento e tag
  do RAL ficam de fora); download de um servidor HTTP local de teste com hash
  certo, hash errado e tamanho errado; troca e troca que falha no meio.
- Falta: um release de verdade para o teste de ponta a ponta; conferir a troca
  no macOS (assinatura).

### Revisão de 2026-09-25 — o que o dono apontou testando a tela

Conferido pela tela de verdade (captura), em português, inglês e espanhol,
nos dois temas.
- **Banner seguia sempre o tema claro:** a faixa (`Panel1`) ganhou a cor do
  tema (`TEstiloTema.CorBanner`); o logo tem fundo transparente.
- **Idioma:** todo texto escrito em código virou `resourcestring`, com prefixo
  como no `RALConsts` (em/wm/cm): o núcleo em `RALInst.Mensagens`, as telas em
  `RALInst.Tela.Mensagens`. Os `.po` (`src/languages/ralinstaller.<idioma>.po`)
  estão embutidos no executável (RCDATA `PO_PT_BR`, `PO_EN_US`, `PO_ES_ES`);
  um `.po` em `languages/` ao lado do exe vale mais (tradutor testa sem
  recompilar). `RALInst.Traducao` (sem LCL) troca as `resourcestring`;
  `RALInst.Tela.Traducao` também passa os textos dos `.lfm` pelo tradutor do
  LCL, e cada página reescreve o que ela escreve em código
  (`TTelaModelo.AtualizarTextos`). Plano, log e relatório saem no idioma
  escolhido. O idioma do sistema já vem escolhido. Traduções revisadas à mão;
  o gerador (`RALInst.Tela.GeradorPO`, botão `bTranslate`) guarda as que já
  existem e só manda ao Google os textos novos. Teste: `tests/traducao`. A CLI
  continua só em português (os textos dela e os do núcleo).
- **Tela da IDE deslocada:** os ícones e os anéis de seleção são centralizados
  em código a cada `Resize`, com o tamanho escalado (`Scale96ToForm`), e o
  texto dos botões fica ancorado sobre a figura deles — a mesma coisa vale para
  as bandeiras. Os ícones das IDEs na lista não apareciam (o `bds.exe` é 32
  bits): lidos com `LoadLibraryEx` só como recurso.
- **Busca das IDEs congelava a tela:** roda numa thread (`RALInst.Tela.Tarefa`),
  com o progresso por `TThread.Queue` e o botão de parar funcionando. Também
  saíram da thread principal a lista de versões e a leitura do zip da versão
  (tela de recursos) e a consulta de versão nova do instalador.
- **Ordem dos recursos:** grupos na ordem pacotes base → motores → módulos
  DBWare → Swagger → compressão → storage (`CategoriaDoPacote`); dentro de cada
  grupo, primeiro o que os outros exigem (PascalRAL, depois PascalRALDsgn;
  RALDBPackage antes dos links).
- **Recurso que a escolha não aceita não é oferecido:** o que não serve em
  nenhuma IDE marcada (ou exige um que não serve) some da árvore e sai do que
  estava marcado; a linha de contagem diz quantos ficaram ocultos e a dica diz
  por quê. O que serve só em parte das IDEs continua com "[fica de fora em …]".
- **Padrão de código do RAL** (wiki `padrao-codigo`): `///` em inglês nas
  classes e métodos da interface, seções private/protected/public/published,
  membros em ordem alfabética (campos, construtor, destrutor, métodos,
  eventos, propriedades), `uses` por grupo (diretivas, IDE, dependência,
  projeto), margem de 90 colunas, units `RALInst.*` na pasta do escopo. A
  seção que o designer do Lazarus mantém (componentes e seus eventos) fica no
  topo da classe, como o Lazarus exige. Identificadores e comentários de
  implementação continuam em português, como o resto do instalador.
- Falta: conferir numa tela com escala de 125%/150%; os textos das receitas e
  do manifesto (`aviso`, `descricao`, `motivo`) são dados e continuam em
  português.

### Revisão 2 de 2026-09-25 — instalação existente, desinstalar, IDE de 64 bits

- **Pastas** (pedido do dono): o que tem tela em `src/telas` (`RALInst.Tela.*`
  com `.lfm` visual), o resto em `src/classes` (o núcleo sem LCL e as units de
  apoio das telas: `Tela.Delphi`, `Tela.Lazarus`, `Tela.Instalacao`,
  `Tela.Mensagens`, `Tela.Tarefa`, `Tela.Temas`, `Tela.Traducao`,
  `Tela.GeradorPO`, `Tela.Imagens`); `.po`/`.mo` em `src/languages`; imagens em
  `Assets/images`, documentos e arquivos soltos em `Assets/docs`. `fontes/`,
  `lang/` e `languages/` saíram. Onde as seções acima dizem `fontes/nucleo`,
  hoje é `src/classes`.
- **Instalação que não foi feita pelo instalador** (`RALInst.Existente`): o
  Delphi é lido do registro — `$(PascalRAL)` (a raiz diz os nomes dos pacotes
  daquela árvore), `Known Packages` e `Known Packages x64`, e as entradas do
  library path com `$(PascalRAL)` ou dentro da raiz; o Lazarus, dos links
  (`packagefiles.xml`) e da lista da IDE (`StaticAutoInstallPackages`). Conta
  como do RAL o nome do catálogo, da pasta `pkg/` da árvore apontada ou
  PascalRAL/PascalRALDsgn; RALRESTDW e `$(RDW2RAL)` nunca. A lista de IDEs
  mostra "— RAL instalado", a tela de recursos já vem com o que está instalado
  marcado e cada recurso diz "(instalado em …)".
- **Selecionar todos / Desmarcar todos** na tela de recursos.
- **Desinstalar = nenhum recurso marcado numa IDE que tem o RAL** (pergunta
  antes; sem RAL em nenhuma IDE marcada continua "Marque ao menos um recurso").
  A última página vira "Desinstalar". Ordem: desfaz os recibos do instalador
  (do mais novo ao mais velho), depois tira o que sobrou feito à mão — Known
  Packages das duas IDEs, `Disabled Packages*`, as entradas do RAL no library
  path, a variável; no Lazarus, links e pacotes da IDE, e reconstrói. Os
  `.bpl` de instalação à mão ficam no disco. O que foi tirado vira um registro
  em `<recibos>\desinstalacoes\*.json` (mesmo formato de recibo), que
  `DesfazerDesinstalacao` / `ralcli desinstalar --desfazer=<arquivo>` devolve.
  Pacotes que usam o RAL e não são dele (RALRESTDW) ficam e geram aviso. A
  `ralcli desinstalar` faz o mesmo. `DesfazerMudanca` passou a devolver também
  o que a mudança tirou.
- **IDE de 64 bits** (Delphi 12+, `bin64\bds.exe` + `lib\win64\release\designide.dcp`):
  o Win64 entra na rodada sozinho, os pacotes de design (do RAL e das
  dependências) compilam em Win64 e vão para `Known Packages x64`. Pacote que
  exige `designide` onde a plataforma não tem o `.dcp` dele (Win64 do Seattle,
  do XE2) é pulado — não é falha — e quem depende dele também, com o motivo.
- **`-LU`**: entraram `vclFireDAC`/`fmxFireDAC` (o cursor de espera do FireDAC
  vinha estático no RALDBFireDACObjects) e `DbxCommonDriver` (o JSON do RAL no
  XE2 é o `Data.DBXJSON`; sem ele o `PascalRAL.bpl` embutia `Data.DBXPlatform`).
- **Receita do Zeos**: Delphi 13 usa o ramo `8.0-patches` (a 8.0.0-stable não
  tem `packages/Delphi13`).
- **Testes**: `tests/existente --testes <raízes>` desinstala e desfaz em
  cópias (registro em `HKCU\Software\RALInstaller-Teste`, config do Lazarus em
  pasta temporária) e confere que o original não mudou; precisa das IDEs, então
  não está no workflow.
- **Instalação prática** (2026-09-25): Delphi 13 (IDE de 32 e de 64 bits, 15
  recursos incluindo Zeos, mORMot2 e Sagui, 30 `.bpl`), Seattle, XE2 e Lazarus
  4.9 (pasta 4.8, IDE reconstruída) — todas abertas depois, com os pacotes
  carregados e nada em `Disabled Packages`. Pela tela: a 1.1 do GitHub no
  Delphi 13 (baixa RAL, submódulos, mORMot2 e Zeos) e a desinstalação completa
  do Seattle.
- **Correções no PascalRAL-dev** (sem commit): `RALDBFireDACObjects` exige o
  `RALDBPackage`; Brotli no Delphi — o `.obj` COFF só liga do 10.1 em diante e
  com o nome exato do símbolo, Win64 e versões anteriores usam as DLLs
  (submódulo `pascal_brotli`, que é outro repositório); `RALClient`
  (`AnsiChar` no lugar de `CharRAL` numa `StringRAL`, Seattle); netHTTP
  (`CERT_CONTEXT` e constantes do WinHTTP próprios, `ProtocolVersion` só do
  Delphi 11, `SerialNum` do 10.3); `RALDBFiredacDAO` (`pidAllPlatforms`);
  `RALTools` (`TInterlocked` antes do XE3); `RALDBFireDAC` (evento de erro do
  AnyDAC); `RALDBFiredacMemTable` (helper de string, XE2); `libsagui` (sobre a
  alteração local que já estava lá: `Pcchar` = `PAnsiChar` no Delphi antigo,
  `UTF8ToString` antes do Seattle, `TrimRight(S)`, e sem `inline` em
  `CheckVersion`/`CheckLastError` no XE2, que dá erro interno URW1147 no
  `RALSaguiServer`). SaguiRAL conferido no XE2, Seattle, Delphi 13 e Lazarus.
- Falta: o `ZComponentDesign` do Zeos não vai para a IDE de 64 bits (o `.dproj`
  dele não habilita Win64); a 1.1 publicada não compila o Brotli em Win64 nem o
  resto que foi corrigido no dev.
---

## 6. Dados de referência

**Registro do Delphi** (HKCU primeiro, HKLM como reserva):
`\Software\Embarcadero\BDS\<ver>` (2010+), `\Software\CodeGear\BDS\<ver>`
(2007, 2009), `\Software\Borland\BDS\<ver>` (2005, 2006),
`\Software\Borland\Delphi\<ver>` (1 a 7). Valores úteis: `App`, `RootDir`,
`ProductVersion`, `Personalities`. Subchaves: `Known Packages`,
`Disabled Packages`, `Library\<plataforma>`, `Environment Variables`.

**Layout de uma instalação Delphi:** `bin\dcc32.exe`, `bin\dcc64.exe`,
`bin\brcc32.exe`, `bin\rsvars.bat` (dá `BDS` e `BDSCOMMONDIR`),
`lib\<plataforma>\release\*.dcp` (pacotes da própria IDE, **sem** sufixo no
`.dcp`), e `<BDSCOMMONDIR>\Bpl` / `<BDSCOMMONDIR>\Dcp` para o que o usuário
instala. O sufixo da versão aparece no nome do `.bpl`, e nos `.dcp` de terceiros
que o próprio usuário compilou — que é o caso do Indy atualizado.

**Forma do comando Delphi** (o `-LU` é o ponto):

```
dcc32 --no-config -B -Q -NS"System;System.Win;Winapi;Vcl;Data;Data.Win;Xml;Web;Soap;Datasnap"
      -U"<BDS>\lib\win32\release;<BDSCOMMONDIR>\Dcp;<src do RAL>"
      -I"<repo>\src\base;<repo>\src\languages"
      -LU"<requires resolvido>"
      -LE"<BDSCOMMONDIR>\Bpl" -LN"<BDSCOMMONDIR>\Dcp"  <pacote>.dpk
```

Caminhos sempre em forma Windows: o Git Bash reescreve `/c/...` e o compilador
morre com `F2039`.

**Forma do comando Lazarus:**

```
lazbuild --primary-config-path=<pcp> --lazarusdir=<raiz> --cpu=<x> --os=<y>
         --add-package-link <lpk...>          # dependências que só entram como link
lazbuild --primary-config-path=<pcp> --add-package <lpk...>
lazbuild --primary-config-path=<pcp> --build-ide=      # uma vez, no fim
```

**Design-time no Delphi é sempre Win32.** Win64, Linux64 e macOS só fazem
sentido para o runtime (compilar o `.dcp`/`.bpl` de runtime da plataforma alvo).

**Fontes da matriz de compatibilidade:** `PascalRAL-Wiki/compatibilidade.md`
(motores por IDE e por sistema), `PascalRAL-Wiki/instalação-Manual.md` (a ordem
manual que o instalador automatiza, e a lista de library paths por recurso) e
`src/base/PascalRAL.inc` (os símbolos `DELPHI*UP` que guardam cada recurso).

**API do GitHub** (o que a F7, a F8 e a F12 consultam):

```
GET /repos/<dono>/<repo>/releases/latest    release estável mais recente
GET /repos/<dono>/<repo>/releases           todos; filtrar draft e prerelease
GET /repos/<dono>/<repo>/tags               tags, para a lista completa de versões
GET /repos/<dono>/<repo>/zipball/<ref>      fonte de uma tag, branch ou commit
GET /repos/<dono>/<repo>/git/trees/<ref>?recursive=1
                                            árvore inteira; submódulo = mode 160000 + sha
https://raw.githubusercontent.com/<dono>/<repo>/<ref>/<caminho>
                                            arquivo cru, fora do limite da API
    release.assets[].browser_download_url   o binário publicado (auto-update)
```

Sem autenticação são 60 requisições por hora **por IP**: consulta com cache em
disco, e um 403 dessa API significa "não deu para verificar", nunca "não há
versão nova". Sem token, **o 304 de um pedido com `If-None-Match`
também conta** (conferido em 2026-09-22): o ETag economiza banda, não cota —
quem economiza cota é não perguntar (cache com validade). O zipball **não** traz submódulos — cada um é download próprio.
`tag_name` costuma vir com `v` na frente; comparar versão exige normalizar isso
antes, e comparar número a número, nunca como texto (`v1.10` > `v1.9`).

---

## 7. Armadilhas

- Escrever no registro com a IDE aberta é perdido: ela regrava tudo ao fechar.
- Pacote que falhou uma vez vai para `Disabled Packages` e continua ignorado
  mesmo depois de corrigido — limpar a entrada faz parte de instalar.
- Lazarus instalado em `Program Files` exige elevação para `--build-ide`.
- O zipball do GitHub não traz submódulos: ZSTD, pascal_brotli e kxBSON precisam
  de download próprio.
- `--no-config` no `dcc32` ignora o `.dproj`: sem `-LU` explícito, Indy e FireDAC
  são linkados estaticamente no `.bpl`, que compila limpo e a IDE recusa com erro
  de unidade duplicada.
- Motor e banco de dados são add-ons opcionais: cada um depende de biblioteca de
  terceiro. Falha de unidade não encontrada quase sempre é dependência ausente,
  não defeito do RAL — a mensagem tem que dizer isso.
- Dependência que já existe na máquina é do usuário, não do instalador: o padrão
  é usar a que está lá. Baixar por cima de um mORMot2 ou de um Zeos em uso quebra
  os projetos dele, e o instalador não tem como saber disso.
- No Windows não se apaga um `.exe` em execução — mas se renomeia. É por aí que
  a auto-atualização (F12) passa, e por isso ela nunca apaga nada antes de ter o
  binário novo gravado.

- `Select-Object -First` num pipeline com a `ralcli` mata o processo no meio
  da compilação (o PowerShell para quem está antes no pipe): grave em arquivo e
  filtre depois.
- O `.obj` de uma biblioteca C em Delphi: `external 'x'` sem `{$L}` vira import
  de DLL (o pacote compila e a IDE não carrega: "módulo não encontrado");
  `external name` para `.obj` só nos Delphi novos. Confira os imports com
  `tdump -em`.

---

## 8. Em aberto

- Versionar os `.res` no repositório do RAL, ou seguir gerando no instalador?
  (o plano segue gerando; a decisão é do projeto principal).
- O manifesto v3 mora em qual branch do RAL, e quem o atualiza quando um pacote
  novo entra? **F6 (2026-09-23):** um `ralinstaller.json` na raiz de cada
  versão do RAL, com o embutido no instalador de reserva; quase tudo é deduzido
  do disco, então pacote novo só precisa de regra quando o que o limita não
  aparece nos `uses` nem no `requires`. Falta o projeto decidir se cria o
  arquivo.
- Atualização do Indy pelo próprio instalador (baixar, compilar e instalar os
  pacotes do Indy com o sufixo da IDE) entra no escopo ou fica como
  pré-requisito documentado? Pela F7 ela é uma receita como as outras — o que
  falta decidir é se o instalador mexe no Indy que veio com a IDE.
- As receitas de dependência ficam no repositório do RAL, junto do manifesto, ou
  num repositório próprio com ciclo de vida separado? Elas mudam quando a
  *dependência* muda, não quando o RAL muda.
- Dependência que já existe na máquina (um mORMot2 com variável de ambiente, um
  Zeos instalado): o fluxo da §0 diz "baixar tudo para a pasta"; a F7 propõe
  usar a existente por padrão e baixar por cima só a pedido. **Decidido na F7
  (2026-09-23): usa a existente**; só baixa o que alguma IDE marcada não tem.
- Layout dentro da pasta escolhida: versionado por subpasta (proposta da §0) ou
  uma pasta só por produto, sobrescrita a cada atualização?
- Conferir o download com hash publicado no release, ou basta o HTTPS? Para o
  auto-update (F12) vale a pena o hash; para o zipball, discutível.
- Auto-update no macOS: trocar o binário invalida a assinatura. Testar antes de
  prometer; pode acabar sendo "avisa que há versão nova" só nesse sistema.
