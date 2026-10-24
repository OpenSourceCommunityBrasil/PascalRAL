# Regras do migrador

Um arquivo por passo de versão (`v1.0-1.1.json`, `1.1-1.2.json`, `1.2-2.0.json`).
Vocabulário **fechado**: ação, campo, condição ou idioma desconhecido recusa o
arquivo inteiro (a mensagem cita o campo ou a ação e o `id`). Nenhuma regra
executa comando. Leitura e modelo: `src/RALMig.Regras.pas`; teste:
`tests/regras/regras.lpr` (`--testes`, ou `--pasta=migrador/regras` para as
regras reais + a `cobertura.json`).

Versões são as públicas: `1.0`, `1.1`, `1.2`, `2.0` (`v1.0` = `1.0`; `dev` = `1.2`;
o ramo `1.3-plugin` e `2.0.x` = `2.0`). Só para a frente (D-M8). Os arquivos de
`regras/` ficam embutidos no executável como RCDATA `REGRAS_<de>_<para>`, com os
pontos das versões em `_` e `__` entre as duas: `REGRAS_1_0__1_1`,
`REGRAS_1_1__1_2`, `REGRAS_1_2__2_0`.

## O passo

```json
{ "formato": 1, "origem": "RAL", "de": "1.1", "para": "1.2",
  "regras": [ ... ], "comportamento": [ ... ] }
```

`origem` é de onde a cadeia parte (`RAL` hoje; D-M9). Um `ralmigracao.json` pode
trazer vários passos: `{ "formato": 1, "passos": [ {passo}, {passo} ] }`.

## A regra

Campos comuns: `id` (`<versão de destino>/<nome>`), `acao`, `onde` (lista de
tipos de arquivo: `dproj dpk lpi lpk lpr dpr pas form groupproj lpg`; pode ser `[]`
só em `padrao-avisar`), `condicao` (opcional, ver abaixo), `descricao`, `manual`,
`comportamento` (opcional: texto do bloco "comportamento que mudou" quando a regra
foi aplicada), `wiki` (link relativo da wiki, `pagina.md#ancora`). As regras de um
passo se aplicam **na ordem do arquivo**, cada uma vendo o resultado da anterior
(por isso `seguranca-descartar-vazio` vem depois dos plugins). Um `id` repetido
no arquivo é erro; num `ralmigracao.json`, o mesmo `id` substitui a regra.

Textos (`descricao`, `manual`, `comportamento`): objeto
`{ "pt-BR": ..., "en-US": ..., "es-ES": ... }`. Nas regras embutidas os três
idiomas são obrigatórios (modo estrito); num `ralmigracao.json` basta um. Falta o
idioma: usa en-US, depois pt-BR. Marcas que o motor troca: `%arquivo%` `%linha%`
`%nome%` (objeto, componente, identificador) `%classe%` `%propriedade%` `%valor%`
`%de%` `%para%` `%pacote%` `%unit%` `%pastas%` (lista) `%componente%`
`%variavel%` `%trecho%` (campo `trecho` da regra) `%alternativa%`.

Strings de lista dentro de um mapa (`para-se-unit`, `padrao`) viram texto
separado por vírgula na leitura.

## Ações e parâmetros (`!` = obrigatório)

| ação | parâmetros |
| --- | --- |
| `pacote-renomear` | `de!` `para!` `ide!` (`delphi`/`lazarus`/`ambos`; `ambos` pede `de-lazarus` e `para-lazarus`, que valem para o Lazarus) `no-uses` (troca também a unit de mesmo nome no `uses` do `.lpr`) `instalar-na-ide` (pacote de design só da IDE, para o relatório) |
| `pacote-exigir-se-unit` | `units!` (lista) `pacote!` `ide!` (`ambos` pede `pacote-lazarus`) |
| `caminho-trocar` | `de!` (pasta do RAL, só o trecho depois da raiz) `modo!` (`trocar`: troca `de` por `para`; `acrescentar`: mantém `de` e acrescenta) `para!` (lista, sempre entram) `para-se-unit` (mapa pasta → units: a pasta só entra se o projeto usa alguma delas) |
| `unit-renomear` | `mapa!` (unit antiga → nova) |
| `uses-adicionar` | `unit!` `quando-identificador!` (lista: o `.pas` usa algum e não tem a unit no `uses`) |
| `identificador-renomear` | `mapa!` (antigo → novo) `enumerado` (tipo enumerado dono dos valores, para o motor validar na API) |
| `propriedade-renomear` | `classe!` (glob) `propriedade!` `para!` |
| `propriedade-para-componente` | `classe-dona!` (glob, `TRAL*Server`) `propriedades!` (de origem) `mapeamento!` (origem → destino no componente) `componente!` `unit!` `nome-base!` (`RALCORSPlugin`, vira `RALCORSPlugin1`...) `ligacao!` (propriedade do componente que aponta o dono, `Server`) `criar-quando!` (condição, ou `{"tipo":"sempre"}`) `descartar` (propriedades que só somem; valem também quando o componente não é criado) `omitir-valores` (mapa destino → valor que não se escreve por ser o padrão do componente) |
| `valor-padrao-materializar` | `classe!` (glob) `propriedade!` `valor!` (como o `.dfm` escreve: `'*'`, `False`) `quando` (`ausente`) `modo` (`escrever` padrão, `so-relatar`, `perguntar`) |
| `propriedade-remover` | `classe!` `propriedades!` `valores-descartaveis` (mapa propriedade → lista de valores que são o padrão e podem sumir; valor diferente vira item manual; propriedade sem entrada sempre some) |
| `objeto-sem-equivalente` | `classes!` (objetos do formulário) `alternativa!` `unit` `identificadores` (nomes no código) |
| `padrao-avisar` | `padrao!` (mapa, ver abaixo) `trecho` (código pronto para o relatório) |
| `pacote-design-para-runtime` | `de!` `para!` `ide!` `manter-se` (`designonly`, `tem-register`: não troca se o pacote é de design) |

`padrao.tipo`: `identificador` (`nomes`, aceita `Classe.Membro`), `chamada-metodo`
(`classe`, `metodos`), `atribuicao-propriedade` (`classe`, `propriedade`),
`conversao-tipo` (`tipo-alvo`, `membro`), `ausencia-atribuicao` (`criacao-de`,
`propriedade`), `sobrescrita-metodo` (`ancestral`, `metodo`) e
`somente-relatorio` (sem padrão de código: só o texto). Campos de lista aceitam
texto ou lista.

## Condições

Objeto `{"tipo": ..., "negar": true|false}` mais: `tem-propriedade`
(`propriedade`: a linha existe no formulário), `valor-igual` e `valor-diferente`
(`propriedade`, `valor`; o diferente só vale se a linha existe), `valor-nao-vazio`
(`propriedade`; ausente = vazio), `classe-descende-de` (`classe`; o motor
avalia pelo `api-<destino>.json`), `unit-no-uses` (`unit`), `set-contem`
(`propriedade`, `opcao`), `alguma` e `todas` (`condicoes`: lista de condições),
`sempre`.

## Comportamento

Quebras que a API não mostra e que nenhum código reescreve ficam na lista
`comportamento` do passo: `id` (mesma forma do da regra, outro espaço de nomes),
`descricao!` (o que mudou), `fazer!` (o que o usuário faz), `wiki`. Entram no
bloco "comportamento que mudou" do relatório. Uma regra que também precisa de
aviso com padrão de código usa `padrao-avisar` e pode repetir o `id` ali.

## Cobertura

Todo `id` citado em `cobertura.json` (campo `regra`, separado por vírgula, e os
`comportamento`) tem de existir aqui; `regras --pasta=migrador/regras` confere e
lista os ids que a cobertura não cita (aviso).
