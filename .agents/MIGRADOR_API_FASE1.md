# API das units da fase 1 do migrador (M1, Cópia, Prévia)

Resumo para quem programa contra elas (motor M2, ações M3–M5, M6–M8). A
documentação completa está nos `///` de cada unit.

## Armadilhas

- A unit do `uses` chama-se `RALMig.&Uses` (arquivo `RALMig.Uses.pas`): `uses` é
  palavra reservada. Quem usa escreve `uses ..., RALMig.&Uses`.
- Offsets e linhas são 1-based. Edições valem sempre contra o **original**;
  árvore, itens de `uses` e elementos XML descrevem o original e não se
  atualizam. Para reanalisar depois de uma rodada de edições:
  `Texto.Consolidar`, depois `Ler` no editor (`TEditorUses`, `TFormulario`,
  `TProjetoXML`). O motor deve aplicar uma regra por vez com Consolidar entre
  regras que mexem no mesmo arquivo, ou juntar as edições de um arquivo sem
  sobreposição.
- Edições sobrepostas levantam `ETextoEdicao`. Dois `Tirar` em itens vizinhos
  do `uses` se sobrepõem: use `TirarVarias`.
- FPC 3.2.3: `string` com code page 65535 perde caracteres em concatenação.
  Bytes internos em `AnsiString` copiados com `SetString`; evite
  `SetCodePage(..., 65535)`.
- Texto novo entra em UTF-8; `TTextoOriginal` converte para ANSI quando o
  arquivo é ANSI. Valores string de formulário: montar com `ValorStringForm`
  (`#nnn` no `.dfm`; `utf8Direto=True` no `.lfm`). `InserirObjeto` não converte.

## RALMig.Texto

`TTextoOriginal`: `CarregarArquivo`, `CarregarBytes(bytes, caminho)`,
`RedefinirOriginal`, `Consolidar`; `Bytes`, `Caminho`, `Codificacao`
(`cdUtf8Bom`/`cdUtf8`/`cdAnsi`), `FimDeLinha`, `InicioConteudo`, `LinhaCount`,
`EdicaoCount`, `Resultado`, `Alterado`; `AdicionarEdicao(offset, tamanho,
novoUtf8)`, `Inserir`, `RemoverLinhas(ini, fim)`, `InserirAntesDaLinha(linha,
texto)`; `LinhaDoOffset`, `ColunaDoOffset`, `LinhaInicio`, `LinhaFim`,
`LinhaProxima`, `TextoDaLinha`, `Indentacao(linha)`; `NormalizarFimDeLinha`,
`ParaCodificacao`, `Gravar(caminho, forcar=False)`. Soltas: `LerBytes`,
`DetectarCodificacao`.

## RALMig.Tokens

`TokenizarPascal(bytes, inicio=1): TListaTokens`; `TToken` (`Tipo`, `Inicio`,
`Tamanho`, `Fim`); tipos `ttIdentificador`, `ttPalavraChave`, `ttNumero`,
`ttString`, `ttComentario`, `ttDiretiva`, `ttSimbolo`, `ttEspaco`.
`TListaTokens[i]`, `Count`, `Texto(i)`, `EhNome`, `EhNomeIgual(i, nome)`,
`EhSimbolo`, `EhIgnoravel`, `Proximo(i)`, `Anterior(i)`,
`AcharIdentificador(nome, desde)`, `IndiceDoOffset`, `NomeDiretiva`,
`ArgumentoDiretiva`. Todo byte pertence a um token; nunca levanta exceção.

## RALMig.&Uses

`TEditorUses.Create(texto)`; `TSecaoUses` (`suInterface`, `suImplementation`,
`suPrograma`, `suRequires`, `suContains`). `Clausulas[i]`/`Clausula(secao)`
(`TClausulaUses`: `Secao`, `Itens`, `Item(i)`, `Count`, `PontoEVirgula`),
`Achar`, `Contem`, `ContemEm(unit, secao)`. `TItemUses`: `Nome`, `NomeInicio`,
`NomeTamanho`, `Inicio`, `Tamanho`, `Arquivo` (`in '...'`), `Virgula`,
`DentroIfDef`, `Condicao`, `Linha`. Edição: `Acrescentar(unit, secao,
arquivoIn='')` (idempotente; cria a cláusula; não repete na implementation o
que está na interface), `Tirar`, `TirarVarias(TStrings)`, `Trocar(velha,
nova)`. Limite: dois `uses` na mesma seção (por `{$IFDEF}`) — só o primeiro.

## RALMig.Formulario

`TFormulario.Create(texto)` (levanta `EFormularioInvalido` com `.Linha`);
`Raiz`, `RaizCount`, `Raizes[i]`. `TNoFormulario`: `Tipo` (`tnObject`,
`tnInherited`, `tnInline`), `Nome`, `Classe`, `Indice`, `LinhaInicio`,
`LinhaFim`, `Indentacao`, `Pai`, `Filhos[i]`/`FilhoCount`,
`Propriedades[i]`/`PropriedadeCount`, `Propriedade(nome)`, `Filho(nome)`,
`Valor(nome)`. `TPropriedadeForm`: `Nome` (com pontos), `ValorBruto`,
`ValorInicio`, `ValorFim`, `LinhaInicio`, `LinhaFim`, `EhLista`, `EhColecao`,
`EhBinario`, `EhConjunto`, `EhTexto`, `MultiLinha`. `AcharObjeto(nome)`,
`ObjetosDaClasse(classe): TList` (quem chama libera). Edição:
`TirarPropriedade`, `TrocarValor`, `RenomearPropriedade`, `TrocarClasse`,
`RenomearObjeto`, `TirarObjeto`, `InserirPropriedade(no, texto)`,
`InserirObjeto(pai, depoisDe, texto)` (texto sem indentação; `depoisDe=nil` =
último filho). Soltas: `EhFormularioBinario`, `ConverterBinarioParaTexto`,
`ValorStringForm(utf8, utf8Direto)`, `ValorComoTexto(valor, out texto)`.

## RALMig.ProjetoXML

`TProjetoXML.Create(texto)` (levanta `EXmlInvalido`); `Raiz`; `Buscar(caminho):
TList` (quem chama libera; `//` busca em qualquer nível; `*` casa nome),
`Primeiro(caminho)`; `ValorTexto`, `ValorAtributo` (a `Condition` do
`PropertyGroup`: `ValorAtributo(el.Pai, 'Condition')`); `TrocarTexto`,
`TrocarAtributo`, `InserirIrmaoDepois`, `InserirFilho`; pacotes:
`ContainerDePacotes`, `ListarPacotes`, `AcrescentarPacote(container, nome)`
(cria `ItemN+1`, atualiza `Count`, False se já existe). Sem remoção de `ItemN`
com renumeração (falta).

## RALMig.Copia

`CriarCopia(pasta, de, para, AArquivos, out AZip, out AErro)` (antes de
gravar); **`RegistrarDepois(AZip, pasta, out AErro)` logo depois de gravar**
(sem ela o desfazer não sabe o que mudou depois); `ListarCopias(pasta):
TStringList`; `Desfazer(AZip, pasta, AMudados, out AErro)`;
`GitComMudancas(pasta, out ADetalhe)`; `PASTA_COPIAS`, `NOME_MANIFESTO`.

## RALMig.Previa

`DiffUnificado(nome, antes, depois, contexto=3): string` ('' sem mudança;
troca só de fim de linha também dá ''); `ResumoDiff(diff, out acrescentadas,
out tiradas)`; `TextoResumoDiff(diff)`.

## RALMig.Mensagens

`resourcestring` do núcleo, prefixos `em`/`wm`/`cm`, uma seção por unit; cada
etapa acrescenta a sua. As units da M1 têm as próprias `resourcestring` (mover
para cá quando for traduzir, M8).

## RALMig.Regras (M2, regras como dado)

Formato em `migrador/regras/LEIAME.md` (campos de cada ação, condições,
`padrao` de `padrao-avisar`, marcas `%arquivo%`...). Soltas:
`NormalizarVersao` (`v1.0`→`1.0`, `dev`→`1.2`, `1.3-plugin`/`2.0.x`→`2.0`),
`CompararVersoes`, `NormalizarIdioma`, `NomeDaAcao`, `TentarAcao`,
`RecursoDoPasso(de, para)` (RCDATA `REGRAS_1_0__1_1`, `REGRAS_1_1__1_2`,
`REGRAS_1_2__2_0`). `TConjuntoRegras`: `CarregarEmbutidas`, `CarregarDePasta`,
`CarregarArquivo`, `CarregarTexto`, `AplicarSobreposicaoDaRaiz(raizRAL)`
(`ralmigracao.json`), `Erros`, `Passos[i]`, `BuscarPasso`, `BuscarRegra(id)`,
`BuscarComportamento(id)`, `VersaoConhecida`, `ListarVersoes`,
`Cadeia(origem, de, para, out erro, out msg): TListaPassos` (quem chama dá
Free; `ceRetrocesso` = D-M8). `TPassoMigracao`: `Origem`, `De`, `Para`,
`Rotulo`, `Regras[i]`/`RegraCount`, `Comportamento[i]`/`CompCount`.
`TRegraMigracao`: `Id`, `Acao` (`TAcaoRegra`: `arPacoteRenomear`,
`arPacoteExigirSeUnit`, `arCaminhoTrocar`, `arUnitRenomear`, `arUsesAdicionar`,
`arIdentificadorRenomear`, `arPropriedadeRenomear`,
`arPropriedadeParaComponente`, `arValorPadraoMaterializar`,
`arPropriedadeRemover`, `arObjetoSemEquivalente`, `arPadraoAvisar`,
`arPacoteDesignParaRuntime`), `Onde`, `Condicao` (nil = sempre;
`TCondicaoRegra`: `Tipo`, `Propriedade`, `Valor`, `Classe`, `NomeUnit`,
`Opcao`, `Negar`, `Filhos`), `CriarQuando`, `Wiki`, `Texto('descricao' |
'manual' | 'comportamento', idioma)`, `TemCampo`, `Param(nome, padrão)`,
`ParamBool`, `Lista(nome)`, `Mapa(nome)` (linhas Nome=Valor), `PadraoTipo`.
`TComportamentoMigracao`: `Id`, `Wiki`, `Texto('descricao'|'fazer', idioma)`.
Os textos saem em UTF-8: o programa faz `DefaultSystemCodePage := CP_UTF8`.

## RALMig.Tipos (contrato entre motor e ações)

`TTipoArquivo` (`taDproj`, `taDpk`, `taLpi`, `taLpk`, `taLpr`, `taDpr`,
`taPas`, `taForm`, `taGroupproj`, `taLpg`, `taOutro`), `TipoDoArquivo`,
`TipoDoNome`, `TiposDaRegra(regra)`. `TArquivoMigrado` (`Caminho`,
`Relativo`, `Tipo`, `Novo`, `Ignorado`/`Motivo`, `Texto` carregado sob
demanda, `Alterado`, `Atual`). `TConjuntoArquivos` (`Pasta`, `Adicionar`,
`CriarArquivo`, `Buscar`, `Listar(tipos, lista)`, `FormularioDe(unit)`,
`UnidadeDe(form)`, `Consolidar`, `ListarAlterados`). `TResultadoMigracao`
(`De`, `Para`, `Idioma`, `Erros`, `AdicionarFeito`, `AdicionarManual(regra,
arquivo, linha, problema, fazer, trecho, wiki)`, `AdicionarComportamento`,
listas `Feitos`/`Manuais`/`Comportamentos`). `TContextoMigracao` (`Conjunto`,
`Resultado`, `Passo`, `ApiDe`, `ApiPara`, `Idioma`, `ConverterBinarios`).
`TAcaoMigracao.Aplicar(regra, contexto): boolean` (abstrata; edita só os textos
do contexto; idempotente; o motor consolida depois de cada regra);
`RegistrarAcao(TAcaoRegra, classe)` no `initialization` de cada
`RALMig.Acoes.*`; `ClasseDaAcao`. `PreencherMarcas(texto, ['arquivo', x,
...])`, `TextoRegra(regra, campo, contexto, marcas)`.

## Fase 2 (M2 motor, M3, M4, M5)

**RALMig.Motor — `TMotorMigracao`**: `Pasta` (pasta ou arquivo de projeto),
`De`, `Para`, `Idioma`, `ConverterBinarios`, `RaizRAL`, `PastaRegras`,
`PastaApi`; `CarregarRegras` (embutidas; sem RCDATA, `migrador/regras` achado
subindo do exe; com `RaizRAL`, o `ralmigracao.json`); `Regras`;
`Simular: TResultadoMigracao` (do motor, refeito a cada chamada; D-M8 antes;
regra recusada para tudo com erro; ação sem classe ou exceção numa ação =
erro com o id; comportamentos de cada passo entram no idioma; formulário
ignorado = item manual `ignorado`); `Gravar(ACopia)` (`CriarCopia` → grava →
`RegistrarDepois`; `ZipCopia`, `ErroGravacao`); `CodigoSaida` (`SAIDA_*`: 0
nada manual, 1 sobrou manual, 2 erro, 3 recusado); `Recusado`;
`Detectar(out evidências): versão`, `Confianca`; `ProjetosFora`, `UnitsFora`,
`Incluir(caminho)`; `Conjunto`, `Resultado`. O motor usa as três units de ação
(`RALMig.Acoes.Projeto`, `.Formulario`, `.Codigo`): quem usa o motor tem
todas registradas.

**RALMig.Projeto**: `TLocalizadorArquivos(pastaOuArquivo, converterBinarios)`
(`Executar`, `Conjunto`, `TomarConjunto`, `ProjetosFora`, `UnitsFora`,
`IncluirFora`, `Erros`); formulário binário sem conversão ou que não analisa =
`Ignorado` com motivo. Helpers: `ArquivosDoProjeto(conjunto, projeto, lista)`,
`UnitsUsadas(arquivos, dest)`, `EhArvoreRAL`.

**RALMig.Deteccao**: `DetectarVersao(conjunto, out evidências)`,
`DetectarVersaoEx(..., out TConfianca)`, `CarregarApiDaVersao(versão, pasta)`
(RCDATA `API_V1_0`, `API_1_1`, `API_DEV`, `API_2_0` ou arquivo),
`PastaApiAlternativa`.

**Ações**: `RALMig.Acoes.Projeto` (pacote-renomear, pacote-exigir-se-unit,
caminho-trocar, unit-renomear, uses-adicionar, pacote-design-para-runtime),
`RALMig.Acoes.Formulario` (propriedade-renomear, propriedade-para-componente,
propriedade-remover, valor-padrao-materializar, objeto-sem-equivalente),
`RALMig.Acoes.Codigo` (identificador-renomear, padrao-avisar;
`RenomearPropriedadeNoCodigo`, `PluginEmCodigo`, `UsesUmaVez`). `padrao-avisar`
`somente-relatorio` entra como comportamento, não como item manual.

**ralmigra** (`migrador/ralmigra.lpr`/`.lpi`, console): `detectar <pasta>`,
`previa <pasta> [--de= --para=]`, `aplicar <pasta> [--de= --para=] [--backup]
[--converter-binarios] [--ignorar-erros]`, `desfazer <pasta> [--zip=]`,
`inventario`; `--idioma=`, `--raiz-ral=`, `--regras=`, `--api=`. Build:
`lazbuild -B ralmigra.lpi` **de dentro de `migrador/`** (os caminhos dos
recursos são relativos). Mensagens novas como `resourcestring` nas units
(`emAcao*`, `cmAcao*`, `emMotor*`, `emProj*`, `cmDet*`, `emCli*`, `cmCli*`).

**Limitação conhecida**: a linha de um item manual é a do texto quando a regra
rodou; uma regra posterior que insira linhas acima desloca a linha no arquivo
final.
