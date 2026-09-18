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
   uma chamada `git/trees/<ref>?recursive=1` (a árvore inteira, com o commit de
   cada submódulo) mais os `.dpk`/`.lpk`/`.dproj`/`.gitmodules` crus pelo
   `raw.githubusercontent.com`, que não conta no limite da API (F8).
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

### F10 — Desinstalar e reinstalar
Recibo em JSON por IDE (o que foi instalado, onde, qual versão do RAL e de cada
dependência, quais paths) → desinstalação e upgrade determinísticos, com rollback
quando um pacote falha no meio. O recibo distingue o que o instalador **instalou**
do que ele apenas **encontrou**: desinstalar não pode levar junto o mORMot2 que
já era do usuário.
**Pronto quando:** desinstalar devolve a IDE ao estado anterior, registro e
library path incluídos.

### F11 — Entrega
CLI com os mesmos verbos da GUI (serve em sessão remota e em servidor de build),
versionamento próprio (hoje o `.lpi` só tem `MajorVersionNr=1`), build mode de
macOS e workflow de release — o repositório tem apenas `FUNDING.yml`, nenhum CI.
Os binários de Linux e macOS saem sem a metade Delphi, e isso é dito na página de
release e na própria tela, para ninguém procurar o que não está faltando.
**Pronto quando:** uma tag gera binários de Windows, Linux e macOS, todos do
mesmo fonte, e a versão do instalador é independente da versão do pacote.

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
versão nova". O zipball **não** traz submódulos — cada um é download próprio.
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

---

## 8. Em aberto

- Versionar os `.res` no repositório do RAL, ou seguir gerando no instalador?
  (o plano segue gerando; a decisão é do projeto principal).
- O manifesto v3 mora em qual branch do RAL, e quem o atualiza quando um pacote
  novo entra?
- Atualização do Indy pelo próprio instalador (baixar, compilar e instalar os
  pacotes do Indy com o sufixo da IDE) entra no escopo ou fica como
  pré-requisito documentado? Pela F7 ela é uma receita como as outras — o que
  falta decidir é se o instalador mexe no Indy que veio com a IDE.
- As receitas de dependência ficam no repositório do RAL, junto do manifesto, ou
  num repositório próprio com ciclo de vida separado? Elas mudam quando a
  *dependência* muda, não quando o RAL muda.
- Dependência que já existe na máquina (um mORMot2 com variável de ambiente, um
  Zeos instalado): o fluxo da §0 diz "baixar tudo para a pasta"; a F7 propõe
  usar a existente por padrão e baixar por cima só a pedido. Qual vale?
- Layout dentro da pasta escolhida: versionado por subpasta (proposta da §0) ou
  uma pasta só por produto, sobrescrita a cada atualização?
- Conferir o download com hash publicado no release, ou basta o HTTPS? Para o
  auto-update (F12) vale a pena o hash; para o zipball, discutível.
- Auto-update no macOS: trocar o binário invalida a assinatura. Testar antes de
  prometer; pode acabar sendo "avisa que há versão nova" só nesse sistema.
