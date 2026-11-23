# Arquitetura do migrador (M1–M9)

Divisão do `PLANO_MIGRADOR.md` em units, com dono e ordem. O plano manda; isto
só diz **onde** cada coisa mora, para que várias pessoas (ou agentes) trabalhem
em paralelo sem tocar no mesmo arquivo. Levantamento das quebras (as tabelas
de→para das regras): `.agents/MIGRADOR_QUEBRAS.md`. Inventário (M0):
`migrador/regras/INVENTARIO.md`, `cobertura.json`.

## Regras comuns

- Tudo em `migrador/`. Núcleo sem LCL em `migrador/src` (`RALMig.*`), telas em
  `migrador/src/telas` (`RALMig.Tela.*`), testes em `migrador/tests/<nome>/<nome>.lpr`
  (compilados com `fpc -B -Fumigrador/src -Fusrc/classes`, formato de
  `tests/catalogo/catalogo.lpr`: `GFalhas`, `--testes`, sai com 1 se falhou).
- Estilo do instalador: `{$mode ObjFPC}{$H+}`, identificadores em português,
  `///` em inglês, comentário de linha em português sem acento, **sem
  generics** (`TList`, `TStringList`, `TFPList`, classes próprias).
- Código que o migrador **escreve no projeto do usuário** compila no Delphi XE
  e no FPC 3.2 (modo Delphi e ObjFPC): sem generics, sem `var` inline, sem
  string multilinha.
- Todo texto que o usuário final lê é `resourcestring` (`RALMig.Mensagens` no
  núcleo, `RALMig.Tela.Mensagens` nas telas); textos de regra vêm do JSON por
  idioma (`pt-BR`, `en-US`, `es-ES`; reserva en-US → pt-BR).
- Bytes intocados: o arquivo é lido como bytes (`RawByteString`), editado por
  faixas, e o que nenhuma regra tocou sai idêntico. Fim de linha e codificação
  do arquivo são preservados nas linhas inseridas.
- Reusar do instalador (`src/classes`): `RALInst.Zip` (zip), `RALInst.Traducao`
  (`.po`), `RALInst.Processo`, `RALInst.Catalogo`, `RALInst.IDE*`,
  `RALInst.Build.Delphi`, `RALInst.Instalar.Lazarus`, `RALInst.Tela.Temas`.
- As telas são as do instalador (B12i, 23/11/2026): cada página do migrador é um frame `.lfm`
  sobre o `TTelaModelo` (`src/telas/RALInst.Tela.Modelo`), por `TTelaMig` (`RALMig.Tela.Pagina`);
  a janela fala com as páginas pela interface `IJanelaModelo`, a página de idioma é a
  `RALInst.Tela.Idioma`, e a tradução é a `RALInst.Tela.Traducao`. `--capturar=<pasta>` grava
  cada página nos dois temas e nos três idiomas (`RALInst.Tela.Captura`), nos dois programas.
- Testes sobre os samples trabalham em **cópias** (no `%TEMP%` ou na pasta de
  rascunho): `git -C <repo> archive <commit> | tar -x -C <cópia>`. Nunca gravar
  em `PascalRAL-Samples*` nem em `ral_restdw`.
- Build: `lazbuild -B` (Lazarus 4.6: `D:\IDE\Lazarus\4.6\lazarus\lazbuild.exe
  --pcp=D:\IDE\Lazarus\4.6\config_lazarus`); FPC dos testes:
  `D:\IDE\Lazarus\4.6\fpc\bin\x86_64-win64\fpc.exe`. Nenhum `.ps1`/`.bat`
  com caminho desta máquina no repositório.

## Units e etapas

| unit / arquivo | etapa | o que é |
| --- | --- | --- |
| `RALMig.API`, `RALMig.Inventario`, `ferramentas/apidiff`, `tests/inventario` | M0 | feito |
| `RALMig.Texto` | M1 | `TTextoOriginal`: bytes do arquivo, codificação (BOM UTF-8, UTF-8 sem BOM, ANSI), fim de linha dominante, linhas com offset; lista de edições (offset, tamanho, novo texto) aplicada de uma vez; `Alterado`, `Gravar` |
| `RALMig.Tokens` | M1 | tokenizador Pascal sobre bytes: identificador (com `&`), palavra-chave, número, string (`''`, `#nnn`, `^M`), comentário `{ }` `(* *)` `//`, diretiva `{$ }` `(*$ *)`, símbolo; cada token com offset/tamanho no original |
| `RALMig.Uses` | M1 | seções `uses`/`contains`/`requires` de `.pas`/`.dpr`/`.lpr`/`.dpk`: itens com nome, `in '...'`, posição, `{$IFDEF}` em volta; acrescentar / tirar / trocar gerando edições que respeitam a quebra de linha existente |
| `RALMig.Formulario` | M1 | árvore de `.dfm`/`.lfm`/`.fmx` em texto: nós `object`/`inherited`/`inline` (nome, classe, linhas), propriedades (nome com pontos, valor bruto multi-linha: listas `( )`, `< >` coleções, `{ }` binário, strings `#nnn`); tirar propriedade, trocar valor, inserir objeto filho; detectar formulário binário e converter (`ObjectBinaryToText`) |
| `RALMig.ProjetoXML` | M1 | `.dproj`/`.lpi`/`.lpk` editados no texto: achar elemento/atributo por caminho e trocar só aquele trecho; inserir elemento irmão com a indentação vizinha |
| `tests/leitura` | M1 | ida e volta sem regra = bytes idênticos em todos os arquivos dos samples e das demos do RDW; tokenizador com nome em comentário, string e diretiva |
| `RALMig.Regras` + `regras/*.json` | M2 | modelo e leitura das regras (vocabulário fechado, recusa com o nome da ação/campo), textos por idioma, passos com `origem` (`RAL`) e versões, cadeia `de→para`; regras embutidas (RCDATA) + `ralmigracao.json` da raiz do RAL de destino acrescenta/substitui por `id` |
| `tests/regras` | M2 | ação desconhecida recusada; regra embutida sem os três idiomas falha; cadeia 1.1→2.0 = 1.1→dev + dev→2.0 |
| `RALMig.Tipos` | M2 | tipos do resultado comuns a todos: `TArquivoMigrado` (caminho, `TTextoOriginal`, tipo), `TConjuntoArquivos`, `TItemFeito` (regra, arquivo, linha, descrição), `TItemManual` (regra, arquivo, linha, problema, o que fazer, trecho, wiki), `TItemComportamento`, `TResultadoMigracao` |
| `RALMig.Projeto` | M2 | acha os arquivos de uma pasta: projetos `.dproj`/`.lpi`/`.dpr`/`.lpr`/`.dpk`/`.lpk`, `.groupproj` e grupos do Lazarus (`.lpg`), units e formulários; units fora da pasta citadas pelo search path (lista para perguntar) |
| `RALMig.Motor` | M2 | aplica a cadeia em memória sobre o `TConjuntoArquivos`; registro de ações por nome (`RegistrarAcao('pacote-renomear', TAcaoPacoteRenomear)`); cada ação é uma classe `TAcaoMigracao` com `Aplicar(ARegra, AConjunto, AResultado)`; idempotente |
| `RALMig.Deteccao` | M2 | versão de origem: argumento, pistas (pacotes, propriedades, units) com evidência, usando `api/api-*.json` embutidos; "sem pista" devolve a mais provável |
| `RALMig.Previa` | M2 | diff unificado por arquivo (original × alterado), em texto |
| `RALMig.Acoes.Projeto` | M3 | `pacote-renomear`, `pacote-exigir-se-unit`, `caminho-trocar` (`.dproj`, `.lpi`, `.dpk`), `unit-renomear`, `uses-adicionar`, `PascalRALDsgn`→`PascalRAL` em pacote de runtime |
| `RALMig.Acoes.Formulario` | M4 | `identificador-renomear` (classe em `object X: TClasse`), `propriedade-renomear`, `propriedade-remover`, `propriedade-para-componente` (objeto novo, campo no `.pas`, unit no `uses`), `valor-padrao-materializar`, `objeto-sem-equivalente`; herança, frames, DataModules |
| `RALMig.Acoes.Codigo` | M5 | `identificador-renomear` no código (tokens), reescrita provada (atribuição de propriedade que virou plugin com o tipo achado na unit), `padrao-avisar` com trecho pronto |
| `RALMig.Validacao` | M6 | confere o resultado contra `api-<destino>.json`: classes, propriedades `published`, units, pacotes |
| `RALMig.Conferir` | M6 | "compilar para conferir" numa pasta temporária com o motor do instalador |
| `RALMig.Relatorio` | M7 | `MIGRACAO-RAL-<de>-<para>.md` e `.html`: feito / precisa de você / comportamento que mudou |
| `RALMig.Copia` | M7 | cópia de segurança em `_ral-migracao/<de>-<para>-<data>.zip`, desfazer, aviso de git com mudanças |
| `RALMig.Mensagens` | M2→M8 | `resourcestring` do núcleo (cada etapa acrescenta as suas) |
| `ralmigra.lpr`/`.lpi` | M2→M8 | console: `detectar`, `previa`, `aplicar [--backup]`, `desfazer`, `inventario`; `--de=`, `--para=`, `--idioma=`; códigos de saída da M7 |
| `ralmigrador.lpr`/`.lpi` + `RALMig.Tela.*` | M8 | janela LCL, modos `Windows64`, `Linux64`, `MacOS64` (saída `ralmigrador_w64.exe`, `_l64`, `_m64` na pasta `migrador/`, como o workflow espera) |
| `languages/ralmigrador.<idioma>.po` | M8 | `.po` embutidos; `RALInst.Traducao` ganha o prefixo do arquivo como variável (o instalador continua `ralinstaller`) |
| workflow `.github/workflows/instalador.yml` | M8/M9 | compila `ralmigrador` e `ralmigra`, roda os testes sem IDE do migrador, e o `apidiff pendentes` como aviso |

## Fases

1. **M1** ∥ **M2-regras** (`RALMig.Regras`, `regras/*.json`, `tests/regras`) ∥
   **`RALMig.Copia` + `RALMig.Previa` + M9**.
2. **M2-motor** (`RALMig.Tipos`, `Projeto`, `Motor`, `Deteccao`, `ralmigra`
   mínimo) + **M3**; depois **M4** ∥ **M5**.
3. **M6** ∥ **M7-relatório**; depois **M8** (telas, tradução, CLI final,
   instalador, workflow).
4. Testes finais: os "Pronto quando" de cada etapa que dá para rodar aqui.
