unit RALInst.Receitas;

{$mode ObjFPC}{$H+}

// F7: como cada dependencia de terceiro se instala, descrito em dado.
//
// Uma receita por dependencia, em JSON. Ela diz de onde baixar, o que a
// dependencia fornece (as variaveis e os pacotes que os pacotes do RAL
// declaram exigir), como detectar que ela ja esta instalada, e o que fazer
// em cada IDE. O vocabulario de acoes e FECHADO: variavel, libpath, dpk, lpk.
// Nao existe "rode este comando" — a receita vem da rede, e uma receita capaz
// de executar qualquer coisa faria do instalador um vetor de ataque. Acao ou
// campo que o instalador nao conhece recusa a receita inteira, com o nome.
//
// Quem precisa de qual receita sai dos proprios pacotes do RAL (F2): o
// requires ('ZComponent', 'indylaz', 'mormot2'), as variaveis do caminho de
// busca ($(mormot2) no SynopseRAL.dproj) e, para o que nao e declarado em
// lugar nenhum (uniGUI), a lista explicita "pacotes-ral" da receita.
//
// F6: a versao pode depender da IDE. "fonte.versoes" e uma lista de regras
// com faixa (delphi-min/max, lazarus-min/max, fpc-min/max, sistemas) e
// "versao" ou "incompativel"; a primeira que vale para a IDE decide, e sem
// nenhuma vale "versao". O Zeos usa isso para o FPC: 8.0-patches no 3.3,
// nenhuma no 3.2.3/3.2.4. A versao que cada RAL exige vem do manifesto
// (RALInst.Compatibilidade), antes destas regras.
//
//   {
//     "nome": "mORMot2", "descricao": "...", "site": "...", "pago": false,
//     "fonte":  { "github": "synopse/mORMot2", "versao": "estavel",
//                 "versoes": [ { "fpc-min": "3.3", "versao": "master" } ] },
//     "extras": [ { "asset": "mormot2static.tgz", "destino": "static" } ],
//     "pacotes-ral": [],
//     "delphi":  { "fornece": { "variaveis": ["mormot2"], "pacotes": [] },
//                  "deteccao": [ { "variavel": "mormot2" } ],
//                  "acoes": [ { "acao": "variavel", "nome": "mormot2", "valor": "{raiz}/src" },
//                             { "acao": "libpath", "caminhos": ["$(mormot2)", "$(mormot2)/core"] } ] },
//     "lazarus": { "fornece": { "pacotes": ["mormot2"] },
//                  "deteccao": [ { "pacote": "mormot2" } ],
//                  "acoes": [ { "acao": "lpk", "arquivo": "packages/lazarus/mormot2.lpk", "modo": "link" } ] }
//   }

interface

uses
  Classes, SysUtils, Contnrs, fpjson, RALInst.Catalogo, RALInst.IDE;

type
  { TCondicaoIDE }

  // F6: uma faixa de IDE, de compilador e de sistema, como o manifesto e as
  // receitas escrevem: "delphi-min": "XE8", "fpc-max": "3.2.2",
  // "sistemas": ["windows", "linux"]. Delphi pelo nome do produto (XE8, 10.1,
  // 12); Lazarus e FPC pelo numero.
  TCondicaoIDE = class
  public
    DelphiMin, DelphiMax: string;     // como escrito ('XE8')
    LazarusMin, LazarusMax: string;
    FPCMin, FPCMax: string;
    Sistemas: TStringList;
    constructor Create;
    destructor Destroy; override;
    // False (e o motivo) se um campo nao e valido
    function Ler(AObj: TJSONObject; out AErro: string): boolean;
    // a condicao fala deste tipo de IDE: nao tem faixa nenhuma, ou tem faixa
    // do tipo dela (fpc-* e lazarus-* so valem para o Lazarus)
    function Relevante(AIDE: TIDEInstance): boolean;
    // a IDE esta dentro da faixa. Versao do FPC desconhecida: ADesconhecidoAtende
    function Satisfaz(AIDE: TIDEInstance; ADesconhecidoAtende: boolean): boolean;
    // 'Delphi XE8 ou mais novo', 'FPC até 3.2.2'
    function Descrever(AIDE: TIDEInstance): string;
  end;

  { TRegraVersao }

  // qual versao de uma dependencia usar numa faixa de IDE; com Incompativel
  // preenchido, nenhuma versao serve ali e o texto diz por que
  TRegraVersao = class
  public
    Condicao: TCondicaoIDE;
    Versao: string;
    Incompativel: string;
    // outro repositorio para esta faixa (dono/repo); vazio = o da receita
    Github: string;
    // de onde a regra veio (receita ou manifesto), para o log
    Origem: string;
    constructor Create;
    destructor Destroy; override;
  end;

  { TVerificacao }

  // F6: o que torna uma copia JA INSTALADA da dependencia incompativel com a
  // IDE: um arquivo dela (relativo a raiz) casa com a expressao, e a IDE esta
  // na faixa. O instalador nao mexe na copia do usuario (§8) — avisa e deixa
  // de fora quem precisaria dela
  TVerificacao = class
  public
    Condicao: TCondicaoIDE;
    Arquivo: string;
    Expressao: string;
    Aviso: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TTipoAcao = (taVariavel, taLibPath, taDpk, taLpk);

  { TAcaoReceita }

  TAcaoReceita = class
  public
    Tipo: TTipoAcao;
    // variavel: nome e valor ({raiz} = pasta da dependencia)
    Nome: string;
    Valor: string;
    // libpath: caminhos, com {raiz} ou $(variavel)
    Caminhos: TStringList;
    // dpk: pasta dos .dpk por versao do BDS ('23.0=packages/Delphi12');
    // instala na IDE os de design
    Pastas: TStringList;
    // lpk: arquivo relativo a raiz; modo 'link' (so registra) ou 'instalar'
    Arquivo: string;
    Instalar: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TBlocoIDE }

  TBlocoIDE = class
  public
    Existe: boolean;
    // nomes que os pacotes do RAL exigem e que esta dependencia atende
    FornecePacotes: TStringList;
    ForneceVariaveis: TStringList;
    // tipo=valor: variavel=mormot2, unidade=mormot.core.base.pas,
    // dcp=ZComponent, pacote=mormot2
    Deteccao: TStringList;
    Acoes: TObjectList;
    // Delphi: .dcp da dependencia (com curinga: AnyDAC_Comp_D*) contra os
    // quais o pacote do RAL compila (-LU); sem isso o dcc32 embute as
    // unidades dela no .bpl, e a IDE recusa o pacote ao lado dos dela
    PacotesLigados: TStringList;
    // TVerificacao: copia instalada que nao serve nesta faixa de IDE
    Verificacoes: TObjectList;
    // Delphi: arquivos que o RAL precisa achar na copia JA INSTALADA (o
    // ZComponent.inc do Zeos, que o RALDBZeos inclui): as pastas do library
    // path da IDE que os tem entram no caminho de busca da compilacao
    Busca: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Acao(AIndex: integer): TAcaoReceita;
  end;

  { TReceita }

  TReceita = class
  private
    FDelphi: TBlocoIDE;
    FLazarus: TBlocoIDE;
    FPacotesRAL: TStringList;
    FExtras: TStringList;
    FVersoes: TObjectList;
  public
    Nome: string;
    Descricao: string;
    Site: string;
    Pago: boolean;
    // repositorio no GitHub (dono/repo) e versao: 'estavel', tag ou ramo
    Github: string;
    Versao: string;
    // de onde a receita veio (arquivo ou recurso), para o log
    Origem: string;
    constructor Create;
    destructor Destroy; override;

    function Bloco(ATipo: TTipoPacote): TBlocoIDE;
    // o pacote do RAL precisa desta dependencia?
    function Atende(APacote: TPacote): boolean;
    function PodeBaixar: boolean;

    property Delphi: TBlocoIDE read FDelphi;
    property Lazarus: TBlocoIDE read FLazarus;
    // pacotes do RAL que usam a dependencia sem declarar em lugar nenhum
    property PacotesRAL: TStringList read FPacotesRAL;
    // asset=destino|release (arquivo do release, extraido em <raiz>/<destino>)
    property Extras: TStringList read FExtras;
    // F6: "fonte.versoes", TRegraVersao na ordem: a primeira que valer para a
    // IDE decide; nenhuma, vale Versao
    property Versoes: TObjectList read FVersoes;
  end;

  { TReceitas }

  TReceitas = class(TObjectList)
  private
    FErros: TStringList;
    function GetItem(AIndex: integer): TReceita;
  public
    constructor Create;
    destructor Destroy; override;

    // False (e o motivo em Erros) se o JSON nao e uma receita valida; uma
    // receita com o mesmo nome de outra ja carregada a substitui
    function CarregarTexto(const ATexto, AOrigem: string): boolean;
    // todos os *.json da pasta
    procedure CarregarPasta(const APasta: string);
    // os recursos RCDATA RECEITA_* do executavel
    procedure CarregarRecursos;
    // o padrao do instalador: recursos, depois as pastas 'receitas' ao lado do
    // executavel e na pasta de dados (que acrescentam ou substituem)
    procedure CarregarPadrao;

    function Buscar(const ANome: string): TReceita;
    // receitas que os pacotes (e o que eles exigem) precisam, com os nomes
    // dos pacotes que exigem cada uma: Objects = TReceita, texto = nomes
    procedure Exigidas(ACatalogo: TCatalogo; ATipo: TTipoPacote; APacotes: TStrings;
      AResultado: TStrings);

    property Items[AIndex: integer]: TReceita read GetItem; default;
    property Erros: TStringList read FErros;
  end;

// {raiz} trocado pela pasta; '/' trocado pelo separador do sistema
function ExpandirRaiz(const ATexto, ARaiz: string): string;

// 'windows', 'linux' ou 'darwin': o sistema em que o instalador roda, que e
// onde o Lazarus instala
function SistemaAtual: string;

// { "fpc-min": "3.3", "versao": "8.0-patches" } ou { ..., "incompativel": "..." };
// nil (e o motivo) se nao e valida. ACamposExtras: os outros campos que o
// lugar aceita alem da condicao ("receita" no manifesto)
function LerRegraVersao(AObj: TJSONObject; const AOrigem: string;
  const ACamposExtras: array of string; out AErro: string): TRegraVersao;

// a versao que a regra pede, com o repositorio quando nao e o da receita:
// 'frones/ZeosLib:master' (':' nao existe em nome de ref do git)
function VersaoComFonte(const AGithub, AVersao: string): string;
procedure SepararVersao(const AVersao: string; out AGithub, ARef: string);

// verdadeiro se o texto casa com a expressao ('.' tambem casa quebra de linha)
function CasaExpressao(const ATexto, AExpressao: string): boolean;

const
  CamposCondicao: array[0..6] of string = ('delphi-min', 'delphi-max', 'lazarus-min',
    'lazarus-max', 'fpc-min', 'fpc-max', 'sistemas');

implementation

uses
  jsonparser, RegExpr, RALInst.Processo;

function VersaoComFonte(const AGithub, AVersao: string): string;
begin
  if AGithub = '' then
    Result := AVersao
  else
    Result := AGithub + ':' + AVersao;
end;

procedure SepararVersao(const AVersao: string; out AGithub, ARef: string);
begin
  AGithub := '';
  ARef := AVersao;
  if Pos(':', AVersao) > 0 then
  begin
    AGithub := Copy(AVersao, 1, Pos(':', AVersao) - 1);
    ARef := Copy(AVersao, Pos(':', AVersao) + 1, MaxInt);
  end;
end;

function CasaExpressao(const ATexto, AExpressao: string): boolean;
var
  vRegex: TRegExpr;
begin
  vRegex := TRegExpr.Create(AExpressao);
  try
    vRegex.ModifierS := True;
    Result := vRegex.Exec(ATexto);
  finally
    vRegex.Free;
  end;
end;

{ TVerificacao }

constructor TVerificacao.Create;
begin
  inherited Create;
  Condicao := TCondicaoIDE.Create;
end;

destructor TVerificacao.Destroy;
begin
  Condicao.Free;
  inherited Destroy;
end;

function SistemaAtual: string;
begin
  {$IF defined(MSWINDOWS)}
  Result := 'windows';
  {$ELSEIF defined(DARWIN)}
  Result := 'darwin';
  {$ELSE}
  Result := 'linux';
  {$ENDIF}
end;

function NomeDelphiValido(const ANome: string): boolean;
begin
  Result := (ANome = '') or (ProdutoPorNome(ANome) >= 0);
end;

function VersaoNumDelphi(const ANome: string): string;
var
  vIdx: integer;
begin
  Result := '';
  vIdx := ProdutoPorNome(ANome);
  if vIdx >= 0 then
    Result := DelphiProdutos[vIdx].VersaoNum;
end;

{ TCondicaoIDE }

constructor TCondicaoIDE.Create;
begin
  inherited Create;
  Sistemas := TStringList.Create;
  Sistemas.CaseSensitive := False;
end;

destructor TCondicaoIDE.Destroy;
begin
  Sistemas.Free;
  inherited Destroy;
end;

function TCondicaoIDE.Ler(AObj: TJSONObject; out AErro: string): boolean;
var
  vLista: TJSONArray;
  vInt: integer;
begin
  AErro := '';
  DelphiMin := AObj.Get('delphi-min', '');
  DelphiMax := AObj.Get('delphi-max', '');
  LazarusMin := AObj.Get('lazarus-min', '');
  LazarusMax := AObj.Get('lazarus-max', '');
  FPCMin := AObj.Get('fpc-min', '');
  FPCMax := AObj.Get('fpc-max', '');
  if not NomeDelphiValido(DelphiMin) then
    AErro := 'versão do Delphi desconhecida: ' + DelphiMin
  else if not NomeDelphiValido(DelphiMax) then
    AErro := 'versão do Delphi desconhecida: ' + DelphiMax;
  vLista := AObj.Get('sistemas', TJSONArray(nil));
  if vLista <> nil then
    for vInt := 0 to Pred(vLista.Count) do
    begin
      Sistemas.Add(LowerCase(vLista.Items[vInt].AsString));
      if (Sistemas[vInt] <> 'windows') and (Sistemas[vInt] <> 'linux') and
         (Sistemas[vInt] <> 'darwin') then
        AErro := 'sistema desconhecido: ' + Sistemas[vInt] + ' (windows, linux, darwin)';
    end;
  Result := AErro = '';
end;

function TCondicaoIDE.Relevante(AIDE: TIDEInstance): boolean;
var
  vDelphi, vLazarus: boolean;
begin
  vDelphi := (DelphiMin <> '') or (DelphiMax <> '');
  vLazarus := (LazarusMin <> '') or (LazarusMax <> '') or (FPCMin <> '') or (FPCMax <> '');
  if AIDE.Tipo = tiDelphi then
    Result := vDelphi or not vLazarus
  else
    Result := vLazarus or not vDelphi;
end;

function TCondicaoIDE.Satisfaz(AIDE: TIDEInstance; ADesconhecidoAtende: boolean): boolean;

  function Dentro(const AVersao, AMin, AMax: string): boolean;
  begin
    Result := True;
    if (AMin = '') and (AMax = '') then
      Exit;
    if AVersao = '' then
      Exit(ADesconhecidoAtende);
    if (AMin <> '') and (CompararVersoes(AVersao, AMin) < 0) then
      Exit(False);
    if (AMax <> '') and (CompararVersoes(AVersao, AMax) > 0) then
      Exit(False);
  end;

begin
  Result := False;
  if (Sistemas.Count > 0) and (Sistemas.IndexOf(SistemaAtual) < 0) then
    Exit;
  if AIDE.Tipo = tiDelphi then
    Result := Dentro(AIDE.Versao, VersaoNumDelphi(DelphiMin), VersaoNumDelphi(DelphiMax))
  else
    Result := Dentro(AIDE.Versao, LazarusMin, LazarusMax) and
              Dentro(AIDE.VersaoCompilador, FPCMin, FPCMax);
end;

function TCondicaoIDE.Descrever(AIDE: TIDEInstance): string;

  procedure Juntar(const ATexto: string);
  begin
    if ATexto = '' then
      Exit;
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + ATexto;
  end;

  function Faixa(const AProduto, AMin, AMax: string): string;
  begin
    if (AMin <> '') and (AMax <> '') and SameText(AMin, AMax) then
      Result := AProduto + ' ' + AMin
    else if (AMin <> '') and (AMax <> '') then
      Result := Format('%s de %s a %s', [AProduto, AMin, AMax])
    else if AMin <> '' then
      Result := Format('%s %s ou mais novo', [AProduto, AMin])
    else if AMax <> '' then
      Result := Format('%s até %s', [AProduto, AMax])
    else
      Result := '';
  end;

begin
  Result := '';
  if AIDE.Tipo = tiDelphi then
    Juntar(Faixa('Delphi', DelphiMin, DelphiMax))
  else
  begin
    Juntar(Faixa('Lazarus', LazarusMin, LazarusMax));
    Juntar(Faixa('FPC', FPCMin, FPCMax));
  end;
  if Sistemas.Count > 0 then
    Juntar('só em ' + StringReplace(Sistemas.CommaText, ',', ', ', [rfReplaceAll]));
end;

{ TRegraVersao }

constructor TRegraVersao.Create;
begin
  inherited Create;
  Condicao := TCondicaoIDE.Create;
end;

destructor TRegraVersao.Destroy;
begin
  Condicao.Free;
  inherited Destroy;
end;

function LerRegraVersao(AObj: TJSONObject; const AOrigem: string;
  const ACamposExtras: array of string; out AErro: string): TRegraVersao;
var
  vInt, vCampo: integer;
  vAchou: boolean;
  vNome: string;
begin
  Result := nil;
  AErro := '';
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vNome := AObj.Names[vInt];
    vAchou := (vNome = 'versao') or (vNome = 'incompativel') or (vNome = 'comentario') or
              (vNome = 'github');
    for vCampo := Low(CamposCondicao) to High(CamposCondicao) do
      vAchou := vAchou or (vNome = CamposCondicao[vCampo]);
    for vCampo := Low(ACamposExtras) to High(ACamposExtras) do
      vAchou := vAchou or (vNome = ACamposExtras[vCampo]);
    if not vAchou then
    begin
      AErro := Format('campo desconhecido "%s" em %s', [vNome, AOrigem]);
      Exit;
    end;
  end;

  Result := TRegraVersao.Create;
  Result.Origem := AOrigem;
  Result.Versao := AObj.Get('versao', '');
  Result.Incompativel := AObj.Get('incompativel', '');
  Result.Github := AObj.Get('github', '');
  if not Result.Condicao.Ler(AObj, AErro) then
    AErro := AOrigem + ': ' + AErro
  else if (Result.Github <> '') and ((Pos('/', Result.Github) = 0) or (Pos(':', Result.Github) > 0)) then
    AErro := AOrigem + ': github deveria ser "dono/repositorio"'
  else if (Result.Versao = '') = (Result.Incompativel = '') then
    AErro := AOrigem + ': cada regra tem "versao" ou "incompativel", e só um dos dois';
  if AErro <> '' then
    FreeAndNil(Result);
end;

function ExpandirRaiz(const ATexto, ARaiz: string): string;
begin
  Result := StringReplace(ATexto, '{raiz}', ExcludeTrailingPathDelimiter(ARaiz),
                          [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
  Result := StringReplace(Result, '\', PathDelim, [rfReplaceAll]);
end;

function NovaLista: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
end;

{ TAcaoReceita }

constructor TAcaoReceita.Create;
begin
  inherited Create;
  Caminhos := NovaLista;
  Pastas := NovaLista;
end;

destructor TAcaoReceita.Destroy;
begin
  Pastas.Free;
  Caminhos.Free;
  inherited Destroy;
end;

{ TBlocoIDE }

constructor TBlocoIDE.Create;
begin
  inherited Create;
  FornecePacotes := NovaLista;
  ForneceVariaveis := NovaLista;
  Deteccao := NovaLista;
  Acoes := TObjectList.Create(True);
  PacotesLigados := NovaLista;
  Verificacoes := TObjectList.Create(True);
  Busca := NovaLista;
end;

destructor TBlocoIDE.Destroy;
begin
  Busca.Free;
  Verificacoes.Free;
  PacotesLigados.Free;
  Acoes.Free;
  Deteccao.Free;
  ForneceVariaveis.Free;
  FornecePacotes.Free;
  inherited Destroy;
end;

function TBlocoIDE.Acao(AIndex: integer): TAcaoReceita;
begin
  Result := TAcaoReceita(Acoes[AIndex]);
end;

{ TReceita }

constructor TReceita.Create;
begin
  inherited Create;
  FDelphi := TBlocoIDE.Create;
  FLazarus := TBlocoIDE.Create;
  FPacotesRAL := NovaLista;
  FExtras := NovaLista;
  FVersoes := TObjectList.Create(True);
end;

destructor TReceita.Destroy;
begin
  FVersoes.Free;
  FExtras.Free;
  FPacotesRAL.Free;
  FLazarus.Free;
  FDelphi.Free;
  inherited Destroy;
end;

function TReceita.Bloco(ATipo: TTipoPacote): TBlocoIDE;
begin
  if ATipo = tpDelphi then
    Result := FDelphi
  else
    Result := FLazarus;
end;

function TReceita.Atende(APacote: TPacote): boolean;
var
  vBloco: TBlocoIDE;
  vNome: string;
begin
  Result := FPacotesRAL.IndexOf(APacote.Nome) >= 0;
  if Result then
    Exit;
  vBloco := Bloco(APacote.Tipo);
  if not vBloco.Existe then
    Exit;
  for vNome in APacote.Externos do
    if vBloco.FornecePacotes.IndexOf(vNome) >= 0 then
      Exit(True);
  for vNome in APacote.Implicitos do
    if vBloco.FornecePacotes.IndexOf(vNome) >= 0 then
      Exit(True);
  for vNome in APacote.Variaveis do
    if vBloco.ForneceVariaveis.IndexOf(vNome) >= 0 then
      Exit(True);
end;

function TReceita.PodeBaixar: boolean;
begin
  Result := not Pago and (Github <> '');
end;

{ TReceitas }

constructor TReceitas.Create;
begin
  inherited Create(True);
  FErros := TStringList.Create;
end;

destructor TReceitas.Destroy;
begin
  FErros.Free;
  inherited Destroy;
end;

function TReceitas.GetItem(AIndex: integer): TReceita;
begin
  Result := TReceita(inherited Items[AIndex]);
end;

function TReceitas.Buscar(const ANome: string): TReceita;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
    if SameText(Items[vInt].Nome, ANome) then
      Exit(Items[vInt]);
end;

function TReceitas.CarregarTexto(const ATexto, AOrigem: string): boolean;
const
  CamposRaiz: array[0..9] of string = ('nome', 'descricao', 'site', 'pago', 'fonte',
    'extras', 'pacotes-ral', 'delphi', 'lazarus', 'comentario');
var
  vJSON: TJSONData;
  vRaiz: TJSONObject;
  vReceita: TReceita;
  vErro: string;

  procedure Falhar(const AMotivo: string);
  begin
    if vErro = '' then
      vErro := AMotivo;
  end;

  procedure LerLista(ANo: TJSONData; ALista: TStrings; const ACampo: string);
  var
    vInt: integer;
  begin
    if ANo = nil then
      Exit;
    if not (ANo is TJSONArray) then
    begin
      Falhar(ACampo + ' deveria ser uma lista');
      Exit;
    end;
    for vInt := 0 to Pred(TJSONArray(ANo).Count) do
      ALista.Add(TJSONArray(ANo).Items[vInt].AsString);
  end;

  procedure ConferirCampos(AObj: TJSONObject; const APermitidos: array of string;
    const ALugar: string);
  var
    vInt, vPerm: integer;
    vAchou: boolean;
  begin
    for vInt := 0 to Pred(AObj.Count) do
    begin
      vAchou := False;
      for vPerm := Low(APermitidos) to High(APermitidos) do
        if AObj.Names[vInt] = APermitidos[vPerm] then
          vAchou := True;
      if not vAchou then
        Falhar(Format('campo desconhecido "%s" em %s', [AObj.Names[vInt], ALugar]));
    end;
  end;

  procedure LerBloco(ANo: TJSONData; ABloco: TBlocoIDE; const AIDE: string);
  var
    vObj, vFornece, vItem: TJSONObject;
    vLista: TJSONArray;
    vInt, vCampo: integer;
    vAcao: TAcaoReceita;
    vNomeAcao: string;
    vPastas: TJSONObject;
    vVerif: TVerificacao;
    vErroCond: string;
  begin
    if ANo = nil then
      Exit;
    if not (ANo is TJSONObject) then
    begin
      Falhar(AIDE + ' deveria ser um objeto');
      Exit;
    end;
    vObj := TJSONObject(ANo);
    ConferirCampos(vObj, ['fornece', 'deteccao', 'acoes', 'pacotes-ligados', 'verificacoes',
                          'busca', 'comentario'], AIDE);
    LerLista(vObj.Find('busca'), ABloco.Busca, AIDE + '.busca');
    if (ABloco.Busca.Count > 0) and (AIDE <> 'delphi') then
      Falhar('busca só existe no Delphi (no Lazarus o .lpk da dependência já diz as pastas)');
    ABloco.Existe := True;
    LerLista(vObj.Find('pacotes-ligados'), ABloco.PacotesLigados, AIDE + '.pacotes-ligados');

    // verificacoes: { "arquivo", "expressao", "aviso", faixa... }
    vLista := vObj.Get('verificacoes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          Falhar(AIDE + '.verificacoes: cada item e um objeto');
          Continue;
        end;
        vItem := vLista.Objects[vInt];
        ConferirCampos(vItem, ['arquivo', 'expressao', 'aviso', 'comentario', 'delphi-min',
          'delphi-max', 'lazarus-min', 'lazarus-max', 'fpc-min', 'fpc-max', 'sistemas'],
          AIDE + '.verificacoes');
        vVerif := TVerificacao.Create;
        ABloco.Verificacoes.Add(vVerif);
        vVerif.Arquivo := vItem.Get('arquivo', '');
        vVerif.Expressao := vItem.Get('expressao', '');
        vVerif.Aviso := vItem.Get('aviso', '');
        if (vVerif.Arquivo = '') or (vVerif.Expressao = '') or (vVerif.Aviso = '') then
          Falhar('verificacao sem arquivo, expressao ou aviso')
        else if (Pos('..', vVerif.Arquivo) > 0) or (Pos(':', vVerif.Arquivo) > 0) or
                (Copy(vVerif.Arquivo, 1, 1) = '/') then
          Falhar('verificacao: o arquivo e relativo a raiz da dependencia: ' + vVerif.Arquivo)
        else if not vVerif.Condicao.Ler(vItem, vErroCond) then
          Falhar(AIDE + '.verificacoes: ' + vErroCond)
        else
          try
            CasaExpressao('', vVerif.Expressao);
          except
            on E: Exception do
              Falhar('verificacao: expressao invalida: ' + E.Message);
          end;
      end;
    if (ABloco.PacotesLigados.Count > 0) and (AIDE <> 'delphi') then
      Falhar('pacotes-ligados só existe no Delphi (é o -LU do dcc32)');

    vFornece := vObj.Get('fornece', TJSONObject(nil));
    if vFornece <> nil then
    begin
      ConferirCampos(vFornece, ['pacotes', 'variaveis'], AIDE + '.fornece');
      LerLista(vFornece.Find('pacotes'), ABloco.FornecePacotes, AIDE + '.fornece.pacotes');
      LerLista(vFornece.Find('variaveis'), ABloco.ForneceVariaveis, AIDE + '.fornece.variaveis');
    end;

    // deteccao: lista de { "variavel": "x" } / { "unidade": "x" } / ...
    vLista := vObj.Get('deteccao', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          Falhar(AIDE + '.deteccao: cada item e um objeto');
          Continue;
        end;
        vItem := vLista.Objects[vInt];
        ConferirCampos(vItem, ['variavel', 'unidade', 'dcp', 'pacote'], AIDE + '.deteccao');
        for vCampo := 0 to Pred(vItem.Count) do
          ABloco.Deteccao.Add(vItem.Names[vCampo] + '=' + vItem.Items[vCampo].AsString);
      end;

    vLista := vObj.Get('acoes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          Falhar(AIDE + '.acoes: cada item e um objeto');
          Continue;
        end;
        vItem := vLista.Objects[vInt];
        vNomeAcao := vItem.Get('acao', '');
        vAcao := TAcaoReceita.Create;
        ABloco.Acoes.Add(vAcao);
        // o vocabulario fechado: o que nao esta aqui recusa a receita
        if vNomeAcao = 'variavel' then
        begin
          vAcao.Tipo := taVariavel;
          ConferirCampos(vItem, ['acao', 'nome', 'valor', 'comentario'], AIDE + '.acoes[variavel]');
          vAcao.Nome := vItem.Get('nome', '');
          vAcao.Valor := vItem.Get('valor', '');
          if (vAcao.Nome = '') or (vAcao.Valor = '') then
            Falhar('acao variavel sem nome ou valor');
        end
        else if vNomeAcao = 'libpath' then
        begin
          vAcao.Tipo := taLibPath;
          ConferirCampos(vItem, ['acao', 'caminhos', 'comentario'], AIDE + '.acoes[libpath]');
          LerLista(vItem.Find('caminhos'), vAcao.Caminhos, 'libpath.caminhos');
        end
        else if (vNomeAcao = 'dpk') and (AIDE = 'delphi') then
        begin
          vAcao.Tipo := taDpk;
          ConferirCampos(vItem, ['acao', 'pastas', 'instalar', 'comentario'], AIDE + '.acoes[dpk]');
          vPastas := vItem.Get('pastas', TJSONObject(nil));
          if vPastas = nil then
            Falhar('acao dpk sem "pastas" (versao do BDS -> pasta dos .dpk)')
          else
            for vCampo := 0 to Pred(vPastas.Count) do
              vAcao.Pastas.Add(vPastas.Names[vCampo] + '=' + vPastas.Items[vCampo].AsString);
          vAcao.Instalar := vItem.Get('instalar', True);
        end
        else if (vNomeAcao = 'lpk') and (AIDE = 'lazarus') then
        begin
          vAcao.Tipo := taLpk;
          ConferirCampos(vItem, ['acao', 'arquivo', 'modo', 'comentario'], AIDE + '.acoes[lpk]');
          vAcao.Arquivo := vItem.Get('arquivo', '');
          vAcao.Instalar := vItem.Get('modo', 'link') = 'instalar';
          if vAcao.Arquivo = '' then
            Falhar('acao lpk sem arquivo');
        end
        else
          Falhar(Format('ação desconhecida "%s" em %s: o instalador não executa ação ' +
                        'que não conhece', [vNomeAcao, AIDE]));
      end;
  end;

var
  vFonte, vExtra: TJSONObject;
  vLista: TJSONArray;
  vInt: integer;
  vAntiga: TReceita;
  vRegra: TRegraVersao;
  vErroRegra: string;
begin
  Result := False;
  vErro := '';
  vJSON := nil;
  vReceita := TReceita.Create;
  try
    try
      vJSON := GetJSON(ATexto);
    except
      on E: Exception do
      begin
        FErros.Add(AOrigem + ': JSON inválido: ' + E.Message);
        Exit;
      end;
    end;
    if not (vJSON is TJSONObject) then
    begin
      FErros.Add(AOrigem + ': a receita deveria ser um objeto JSON');
      Exit;
    end;
    vRaiz := TJSONObject(vJSON);
    ConferirCampos(vRaiz, CamposRaiz, 'receita');

    vReceita.Origem := AOrigem;
    vReceita.Nome := vRaiz.Get('nome', '');
    vReceita.Descricao := vRaiz.Get('descricao', '');
    vReceita.Site := vRaiz.Get('site', '');
    vReceita.Pago := vRaiz.Get('pago', False);
    if vReceita.Nome = '' then
      Falhar('receita sem nome');

    vFonte := vRaiz.Get('fonte', TJSONObject(nil));
    if vFonte <> nil then
    begin
      ConferirCampos(vFonte, ['github', 'versao', 'versoes', 'comentario'], 'fonte');
      vReceita.Github := vFonte.Get('github', '');
      vReceita.Versao := vFonte.Get('versao', 'estavel');
      if Pos('/', vReceita.Github) = 0 then
        Falhar('fonte.github deveria ser "dono/repositorio"');
      // F6: versao por faixa de IDE/compilador (o Zeos por FPC)
      vLista := vFonte.Get('versoes', TJSONArray(nil));
      if vLista <> nil then
        for vInt := 0 to Pred(vLista.Count) do
        begin
          if not (vLista.Items[vInt] is TJSONObject) then
          begin
            Falhar('fonte.versoes: cada item e um objeto');
            Continue;
          end;
          vRegra := LerRegraVersao(vLista.Objects[vInt], 'fonte.versoes', [], vErroRegra);
          if vRegra = nil then
            Falhar(vErroRegra)
          else
            vReceita.Versoes.Add(vRegra);
        end;
    end;

    vLista := vRaiz.Get('extras', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vExtra := vLista.Objects[vInt];
        ConferirCampos(vExtra, ['asset', 'destino', 'release', 'comentario'], 'extras');
        if vExtra.Get('asset', '') = '' then
          Falhar('extra sem asset');
        // asset=destino|release ('' = o release da propria versao; 'estavel' =
        // o release estavel mais recente, para quando a fonte e um ramo)
        vReceita.Extras.Add(vExtra.Get('asset', '') + '=' + vExtra.Get('destino', '') +
                            '|' + vExtra.Get('release', ''));
      end;

    LerLista(vRaiz.Find('pacotes-ral'), vReceita.PacotesRAL, 'pacotes-ral');
    LerBloco(vRaiz.Find('delphi'), vReceita.Delphi, 'delphi');
    LerBloco(vRaiz.Find('lazarus'), vReceita.Lazarus, 'lazarus');

    if vErro <> '' then
    begin
      FErros.Add(AOrigem + ': receita recusada: ' + vErro);
      Exit;
    end;

    vAntiga := Buscar(vReceita.Nome);
    if vAntiga <> nil then
      Remove(vAntiga);
    Add(vReceita);
    vReceita := nil;
    Result := True;
  finally
    vReceita.Free;
    vJSON.Free;
  end;
end;

procedure TReceitas.CarregarPasta(const APasta: string);
var
  vBusca: TSearchRec;
  vTexto: TStringList;
  vPasta: string;
begin
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vPasta + '*.json', faAnyFile, vBusca) = 0 then
  try
    repeat
      vTexto := TStringList.Create;
      try
        vTexto.LoadFromFile(vPasta + vBusca.Name);
        CarregarTexto(vTexto.Text, vPasta + vBusca.Name);
      finally
        vTexto.Free;
      end;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

const
  // RT_RCDATA: no Windows vem da unit Windows, nos outros do system
  RecursoDados = PChar(10);

function EnumerarReceita(ModuleHandle: TFPResourceHMODULE; ResourceType,
  ResourceName: PChar; lParam: PtrInt): LongBool; stdcall;
var
  vNome: string;
begin
  Result := True;
  if PtrUInt(ResourceName) shr 16 = 0 then
    Exit;
  vNome := string(ResourceName);
  if SameText(Copy(vNome, 1, 8), 'RECEITA_') then
    TStrings(Pointer(lParam)).Add(vNome);
end;

procedure TReceitas.CarregarRecursos;
var
  vNomes: TStringList;
  vNome: string;
  vStream: TResourceStream;
  vTexto: TStringList;
begin
  vNomes := TStringList.Create;
  try
    EnumResourceNames(HINSTANCE, RecursoDados, @EnumerarReceita, PtrInt(vNomes));
    for vNome in vNomes do
    begin
      vStream := TResourceStream.Create(HINSTANCE, vNome, RecursoDados);
      vTexto := TStringList.Create;
      try
        vTexto.LoadFromStream(vStream);
        CarregarTexto(vTexto.Text, 'recurso ' + vNome);
      finally
        vTexto.Free;
        vStream.Free;
      end;
    end;
  finally
    vNomes.Free;
  end;
end;

procedure TReceitas.CarregarPadrao;
begin
  CarregarRecursos;
  CarregarPasta(ExtractFilePath(ParamStr(0)) + 'receitas');
  CarregarPasta(PastaDadosInstalador + 'receitas');
end;

procedure TReceitas.Exigidas(ACatalogo: TCatalogo; ATipo: TTipoPacote;
  APacotes: TStrings; AResultado: TStrings);
var
  vLista: TList;
  vInt, vRec, vIdx: integer;
  vPacote: TPacote;
begin
  AResultado.Clear;
  vLista := TList.Create;
  try
    ACatalogo.Fechamento(ATipo, APacotes, vLista);
    for vRec := 0 to Pred(Count) do
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vPacote := TPacote(vLista[vInt]);
        if not Items[vRec].Atende(vPacote) then
          Continue;
        vIdx := AResultado.IndexOfObject(Items[vRec]);
        if vIdx < 0 then
          AResultado.AddObject(vPacote.Nome, Items[vRec])
        else
          AResultado[vIdx] := AResultado[vIdx] + ', ' + vPacote.Nome;
      end;
  finally
    vLista.Free;
  end;
end;

end.
