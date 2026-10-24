/// The (RAL version, IDE) pairs the project has checked (F16, decision D-I5).
/// The project tests by sampling, not every combination: tests/varredura
/// compiles each RAL version in the oldest IDE at hand (XE2), the newest
/// (Delphi 13) and one drawn in between, and in Lazarus the oldest FPC 3.2.x,
/// the newest trunk, one drawn in between and the FPC Unleashed, and writes
/// here what it found. The list accumulates between runs.
/// - A pair that passed is "verified"; one never checked is "not verified" and
///   still installs (D-I5), with that label.
/// - A pair that failed becomes a block of the manifest (TManifesto), with the
///   first error as the reason: the installer refuses it before downloading.
/// Embedded as RCDATA VERIFICADAS (manifesto/verificadas.json); a
/// verificadas.json beside the executable or in the data folder adds to it.
///
///   { "formato": 1,
///     "verificacoes": [
///       { "ral": "1.1.0", "ide": "delphi", "versao": "37.0", "resultado": "ok",
///         "nome": "Delphi 13 Florence", "data": "2026-10-18", "commit": "..." },
///       { "ral": "1.1.0", "ide": "delphi", "versao": "9.0", "resultado": "falhou",
///         "motivo": "RALClient.pas(991): E2010 ..." },
///       { "ral": "1.2.0", "ide": "lazarus", "versao": "4.9", "fpc": "3.2.3",
///         "variante": "oficial", "resultado": "ok" } ] }
unit RALInst.Verificadas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  RALInst.IDE;

type
  /// One checked pair.
  TVerificacaoIDE = class
  public
    Commit: string;
    Data: string;
    /// Lazarus: the FPC version ('3.2.3')
    FPC: string;
    Motivo: string;
    Nome: string;
    /// The RAL version, as RALConsts declares it ('1.1.0')
    RAL: string;
    /// 'ok' or 'falhou'
    Resultado: string;
    /// 'delphi' or 'lazarus'
    Tipo: string;
    /// Lazarus: 'oficial' or 'unleashed'
    Variante: string;
    /// Delphi: the BDS version ('37.0'); Lazarus: major.minor ('4.9')
    Versao: string;
    /// The pair is this IDE (kind, version, compiler and variant)
    function DaIDE(AIDE: TIDEInstance): boolean;
    /// Passed
    function Ok: boolean;
  end;

  /// Every checked pair, owned by the list.
  TVerificadas = class(TObjectList)
  private
    FErros: TStringList;
    function GetItem(AIndex: integer): TVerificacaoIDE;
  public
    constructor Create;
    destructor Destroy; override;
    /// The pair of a RAL version and an IDE; nil when never checked
    function Buscar(const AVersaoRAL: string; AIDE: TIDEInstance): TVerificacaoIDE;
    /// The embedded list plus the files beside the executable and in the data
    /// folder
    procedure CarregarPadrao;
    /// A verificadas.json file; False when it is not one (Erros says why)
    function CarregarArquivo(const AArquivo: string): boolean;
    /// False when the text is not a valid list
    function CarregarTexto(const ATexto, AOrigem: string): boolean;
    /// Notes a result, replacing the one of the same pair
    function Registrar(const AVersaoRAL, ACommit: string; AIDE: TIDEInstance;
      AOk: boolean; const AMotivo: string): TVerificacaoIDE;
    /// Writes the list (sorted by RAL version, kind and IDE version)
    procedure Salvar(const AArquivo: string);

    property Erros: TStringList read FErros;
    property Items[AIndex: integer]: TVerificacaoIDE read GetItem; default;
  end;

/// '4.9.0.0' -> '4.9'; '3.2.3-r3' -> '3.2.3' (APartes numbers)
function VersaoCom(const AVersao: string; APartes: integer): string;
/// The list the installer uses (loaded on first use, freed at the end)
function VerificadasPadrao: TVerificadas;

implementation

uses
  fpjson, jsonparser, StrUtils,
  RALInst.Mensagens, RALInst.Processo;

var
  GVerificadas: TVerificadas = nil;

function VerificadasPadrao: TVerificadas;
begin
  if GVerificadas = nil then
  begin
    GVerificadas := TVerificadas.Create;
    GVerificadas.CarregarPadrao;
  end;
  Result := GVerificadas;
end;

function VersaoCom(const AVersao: string; APartes: integer): string;
var
  vPartes: TStringArray;
  vInt: integer;
begin
  Result := '';
  vPartes := Trim(AVersao).Split(['.', '-', ' ']);
  for vInt := 0 to Pred(Length(vPartes)) do
  begin
    if (vInt >= APartes) or (vPartes[vInt] = '') or
       not (vPartes[vInt][1] in ['0'..'9']) then
      Break;
    if Result <> '' then
      Result := Result + '.';
    Result := Result + vPartes[vInt];
  end;
end;

{ TVerificacaoIDE }

function TVerificacaoIDE.DaIDE(AIDE: TIDEInstance): boolean;
var
  vVariante: string;
begin
  if AIDE.Tipo = tiDelphi then
    Result := SameText(Tipo, 'delphi') and SameText(Versao, AIDE.BDSVersao)
  else
  begin
    vVariante := LowerCase(AIDE.VarianteFPC);
    if vVariante = '' then
      vVariante := 'oficial';
    Result := SameText(Tipo, 'lazarus') and
              (CompararVersoes(Versao, VersaoCom(AIDE.Versao, 2)) = 0) and
              (CompararVersoes(FPC, VersaoCom(AIDE.VersaoCompilador, 3)) = 0) and
              SameText(IfThen(Variante = '', 'oficial', Variante), vVariante);
  end;
end;

function TVerificacaoIDE.Ok: boolean;
begin
  Result := SameText(Resultado, 'ok');
end;

{ TVerificadas }

constructor TVerificadas.Create;
begin
  inherited Create(True);
  FErros := TStringList.Create;
end;

destructor TVerificadas.Destroy;
begin
  FErros.Free;
  inherited Destroy;
end;

function TVerificadas.GetItem(AIndex: integer): TVerificacaoIDE;
begin
  Result := TVerificacaoIDE(inherited Items[AIndex]);
end;

function TVerificadas.Buscar(const AVersaoRAL: string;
  AIDE: TIDEInstance): TVerificacaoIDE;
var
  vInt: integer;
begin
  Result := nil;
  if AVersaoRAL = '' then
    Exit;
  for vInt := 0 to Pred(Count) do
    if (CompararVersoes(Items[vInt].RAL, AVersaoRAL) = 0) and
       Items[vInt].DaIDE(AIDE) then
      Exit(Items[vInt]);
end;

function TVerificadas.CarregarTexto(const ATexto, AOrigem: string): boolean;
var
  vJSON: TJSONData;
  vLista: TJSONArray;
  vItem: TJSONObject;
  vInt, vCampo, vExistente: integer;
  vNova: TVerificacaoIDE;
begin
  Result := False;
  try
    vJSON := GetJSON(ATexto);
  except
    on E: Exception do
    begin
      FErros.Add(Format(emJSONInvalido, [AOrigem, E.Message]));
      Exit;
    end;
  end;
  try
    if not (vJSON is TJSONObject) or (TJSONObject(vJSON).Get('formato', 0) <> 1) then
    begin
      FErros.Add(Format(emVerificadasFormato, [AOrigem]));
      Exit;
    end;
    vLista := TJSONObject(vJSON).Get('verificacoes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if not (vLista.Items[vInt] is TJSONObject) then
          Continue;
        vItem := vLista.Objects[vInt];
        // campo desconhecido recusa a linha, nao a lista: a varredura de uma
        // versao mais nova pode ter escrito algo a mais
        for vCampo := 0 to Pred(vItem.Count) do
          if AnsiIndexStr(vItem.Names[vCampo], ['ral', 'commit', 'ide', 'versao', 'fpc',
               'variante', 'nome', 'resultado', 'motivo', 'data']) < 0 then
            FErros.Add(Format(emCampoDesconhecido, [vItem.Names[vCampo], AOrigem]));
        vNova := TVerificacaoIDE.Create;
        vNova.RAL := vItem.Get('ral', '');
        vNova.Commit := vItem.Get('commit', '');
        vNova.Tipo := LowerCase(vItem.Get('ide', ''));
        vNova.Versao := vItem.Get('versao', '');
        vNova.FPC := vItem.Get('fpc', '');
        vNova.Variante := LowerCase(vItem.Get('variante', ''));
        vNova.Nome := vItem.Get('nome', '');
        vNova.Resultado := LowerCase(vItem.Get('resultado', ''));
        vNova.Motivo := vItem.Get('motivo', '');
        vNova.Data := vItem.Get('data', '');
        if (vNova.RAL = '') or (vNova.Versao = '') or
           ((vNova.Tipo <> 'delphi') and (vNova.Tipo <> 'lazarus')) or
           ((vNova.Resultado <> 'ok') and (vNova.Resultado <> 'falhou')) then
        begin
          vNova.Free;
          Continue;
        end;
        // o mesmo par de outra origem: vale a ultima carregada
        vExistente := -1;
        for vCampo := 0 to Pred(Count) do
          if SameText(Items[vCampo].RAL, vNova.RAL) and
             SameText(Items[vCampo].Tipo, vNova.Tipo) and
             SameText(Items[vCampo].Versao, vNova.Versao) and
             SameText(Items[vCampo].FPC, vNova.FPC) and
             SameText(Items[vCampo].Variante, vNova.Variante) then
            vExistente := vCampo;
        if vExistente >= 0 then
          Delete(vExistente);
        Add(vNova);
      end;
    Result := True;
  finally
    vJSON.Free;
  end;
end;

function TVerificadas.CarregarArquivo(const AArquivo: string): boolean;
var
  vTexto: TStringList;
begin
  Result := False;
  if not FileExists(AArquivo) then
    Exit;
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(AArquivo);
    Result := CarregarTexto(vTexto.Text, AArquivo);
  finally
    vTexto.Free;
  end;
end;

const
  RecursoDados = PChar(10);

procedure TVerificadas.CarregarPadrao;
var
  vStream: TResourceStream;
  vTexto: TStringList;
begin
  if FindResource(HINSTANCE, 'VERIFICADAS', RecursoDados) <> 0 then
  begin
    vStream := TResourceStream.Create(HINSTANCE, 'VERIFICADAS', RecursoDados);
    vTexto := TStringList.Create;
    try
      vTexto.LoadFromStream(vStream);
      CarregarTexto(vTexto.Text, cmVerificadasEmbutida);
    finally
      vTexto.Free;
      vStream.Free;
    end;
  end;
  CarregarArquivo(ExtractFilePath(ParamStr(0)) + 'verificadas.json');
  CarregarArquivo(PastaDadosInstalador + 'verificadas.json');
end;

function TVerificadas.Registrar(const AVersaoRAL, ACommit: string; AIDE: TIDEInstance;
  AOk: boolean; const AMotivo: string): TVerificacaoIDE;
var
  vInt: integer;
begin
  for vInt := Pred(Count) downto 0 do
    if (CompararVersoes(Items[vInt].RAL, AVersaoRAL) = 0) and Items[vInt].DaIDE(AIDE) then
      Delete(vInt);
  Result := TVerificacaoIDE.Create;
  Result.RAL := AVersaoRAL;
  Result.Commit := ACommit;
  Result.Nome := AIDE.Nome;
  Result.Data := FormatDateTime('yyyy"-"mm"-"dd', Now);
  if AOk then
    Result.Resultado := 'ok'
  else
    Result.Resultado := 'falhou';
  Result.Motivo := AMotivo;
  if AIDE.Tipo = tiDelphi then
  begin
    Result.Tipo := 'delphi';
    Result.Versao := AIDE.BDSVersao;
  end
  else
  begin
    Result.Tipo := 'lazarus';
    Result.Versao := VersaoCom(AIDE.Versao, 2);
    Result.FPC := VersaoCom(AIDE.VersaoCompilador, 3);
    Result.Variante := LowerCase(AIDE.VarianteFPC);
    if Result.Variante = '' then
      Result.Variante := 'oficial';
  end;
  Add(Result);
end;

function CompararVerificacoes(A, B: Pointer): integer;
var
  vA, vB: TVerificacaoIDE;
begin
  vA := TVerificacaoIDE(A);
  vB := TVerificacaoIDE(B);
  Result := CompararVersoes(vA.RAL, vB.RAL);
  if Result = 0 then
    Result := CompareText(vA.Tipo, vB.Tipo);
  if Result = 0 then
    Result := CompararVersoes(vA.Versao, vB.Versao);
  if Result = 0 then
    Result := CompararVersoes(vA.FPC, vB.FPC);
  if Result = 0 then
    Result := CompareText(vA.Variante, vB.Variante);
end;

procedure TVerificadas.Salvar(const AArquivo: string);
var
  vRaiz, vItem: TJSONObject;
  vLista: TJSONArray;
  vInt: integer;
  vTexto: TStringList;
begin
  Sort(@CompararVerificacoes);
  vRaiz := TJSONObject.Create;
  vTexto := TStringList.Create;
  try
    vRaiz.Add('formato', 1);
    vRaiz.Add('comentario', 'Escrito pelo tests/varredura: os pares versao do RAL x ' +
      'IDE que o projeto conferiu. "ok" aparece como verificada; "falhou" vira ' +
      'bloqueio do manifesto, com o motivo.');
    vLista := TJSONArray.Create;
    for vInt := 0 to Pred(Count) do
    begin
      vItem := TJSONObject.Create;
      vItem.Add('ral', Items[vInt].RAL);
      if Items[vInt].Commit <> '' then
        vItem.Add('commit', Items[vInt].Commit);
      vItem.Add('ide', Items[vInt].Tipo);
      vItem.Add('versao', Items[vInt].Versao);
      if Items[vInt].Tipo = 'lazarus' then
      begin
        vItem.Add('fpc', Items[vInt].FPC);
        vItem.Add('variante', Items[vInt].Variante);
      end;
      if Items[vInt].Nome <> '' then
        vItem.Add('nome', Items[vInt].Nome);
      vItem.Add('resultado', Items[vInt].Resultado);
      if Items[vInt].Motivo <> '' then
        vItem.Add('motivo', Items[vInt].Motivo);
      if Items[vInt].Data <> '' then
        vItem.Add('data', Items[vInt].Data);
      vLista.Add(vItem);
    end;
    vRaiz.Add('verificacoes', vLista);
    vTexto.Text := vRaiz.FormatJSON;
    ForceDirectories(ExtractFileDir(ExpandFileName(AArquivo)));
    vTexto.SaveToFile(AArquivo);
  finally
    vTexto.Free;
    vRaiz.Free;
  end;
end;

finalization
  FreeAndNil(GVerificadas);

end.
