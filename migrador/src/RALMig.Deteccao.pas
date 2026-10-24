/// Detects the RAL version a project was written for (stage M2): clues taken
/// from the package names in the project files, the units in the uses clauses
/// and the classes and published properties used in the forms, each checked
/// against the API of every version (api-<version>.json, embedded as RCDATA or
/// read from migrador/api). Every clue that tells versions apart is listed with
/// its evidence; the answer is the newest version that agrees with all of them.
unit RALMig.Deteccao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.API, RALMig.Tipos;

type
  /// How sure the detection is
  TConfianca = (cfNenhuma, cfBaixa, cfMedia, cfAlta);

var
  /// Folder of the api-*.json files used when they are not embedded; '' = look
  /// for migrador/api above the executable
  PastaApiAlternativa: string = '';

const
  /// Versions with an API file, oldest first
  VersoesDeApi: array[0..3] of string = ('1.0', '1.1', '1.2', '2.0');

/// The API of a public version ('1.0'...): embedded resource API_V1_0, API_1_1,
/// API_DEV, API_2_0, else <APastaApi>/api-*.json; nil when there is none.
/// The caller frees it.
function CarregarApiDaVersao(const AVersao, APastaApi: string): TApi;
/// Text of an embedded RCDATA resource; '' when there is none
function LerRecursoTexto(const ANome: string): string;
/// The most likely version ('' without any clue). AEvidencias is created here
/// (the caller frees it): one line per clue, 'file: name -> versions'.
function DetectarVersao(AConjunto: TConjuntoArquivos; out AEvidencias: TStrings): string;
function DetectarVersaoEx(AConjunto: TConjuntoArquivos; out AEvidencias: TStrings;
  out AConfianca: TConfianca): string;

implementation

uses
  RALMig.Formulario, RALMig.ProjetoXML, RALMig.&Uses, RALMig.Inventario;

resourcestring
  cmDetClasse = 'classe %s';
  cmDetPropriedade = '%s.%s';
  cmDetMais = ' (+%d)';
  cmDetSemFaixa = 'nenhuma';

const
  // RT_RCDATA: no Windows vem da unit Windows, nos outros do system
  RecursoDados = PChar(10);
  RecursosApi: array[0..3] of string = ('API_V1_0', 'API_1_1', 'API_DEV', 'API_2_0');
  ArquivosApi: array[0..3] of string = ('v1.0', '1.1', 'dev', '2.0');

function LerRecursoTexto(const ANome: string): string;
var
  vStream: TResourceStream;
begin
  Result := '';
  try
    vStream := TResourceStream.Create(HINSTANCE, ANome, RecursoDados);
    try
      SetLength(Result, vStream.Size);
      if vStream.Size > 0 then
        vStream.ReadBuffer(Result[1], vStream.Size);
    finally
      vStream.Free;
    end;
  except
    on EResNotFound do
      Result := '';
  end;
end;

function IndiceDaVersao(const AVersao: string): integer;
begin
  for Result := Low(VersoesDeApi) to High(VersoesDeApi) do
    if VersoesDeApi[Result] = AVersao then
      Exit;
  Result := -1;
end;

function PastaDeApi(const APasta: string): string;
begin
  Result := APasta;
  if Result = '' then
    Result := PastaApiAlternativa;
  if Result = '' then
  begin
    Result := AcharPastaMigrador;
    if Result <> '' then
      Result := Result + PathDelim + 'api';
  end;
end;

function CarregarApiDaVersao(const AVersao, APastaApi: string): TApi;
var
  vInd: integer;
  vTexto, vArq: string;
begin
  Result := nil;
  vInd := IndiceDaVersao(AVersao);
  if vInd < 0 then
    Exit;
  vTexto := LerRecursoTexto(RecursosApi[vInd]);
  if vTexto = '' then
  begin
    vArq := ArquivoApi(PastaDeApi(APastaApi), ArquivosApi[vInd]);
    if (PastaDeApi(APastaApi) = '') or not FileExists(vArq) then
      Exit;
    Result := TApi.Create;
    try
      Result.CarregarArquivo(vArq);
    except
      FreeAndNil(Result);
    end;
    Exit;
  end;
  Result := TApi.Create;
  try
    Result.CarregarJSON(vTexto);
  except
    FreeAndNil(Result);
  end;
end;

type
  { TPista }

  TPista = class
  public
    Chave: string;
    Arquivo: string;
    Nome: string;
    Mascara: integer;
    Vezes: integer;
  end;

  { TDetector }

  TDetector = class
  private
    FApis: array[0..3] of TApi;
    FTodas: integer;
    FPistas: TStringList;
    FNomesPacotes: array[0..3] of TStringList;
    FNomesUnits: array[0..3] of TStringList;
    FPublicadas: TStringList;
    function MascaraDe(AListas: array of TStringList; const ANome: string): integer;
    function MascaraClasse(const AClasse: string): integer;
    function MascaraPropriedade(const AClasse, APropriedade: string): integer;
    function Publicadas(AVersao: integer; const AClasse: string): TStringList;
    procedure Pista(const AChave, AArquivo, ANome: string; AMascara: integer);
    procedure PacotesDe(AArq: TArquivoMigrado);
    procedure PacoteNome(AArq: TArquivoMigrado; const ANome: string);
    procedure UnitsDe(AArq: TArquivoMigrado);
    procedure FormularioDe(AArq: TArquivoMigrado);
    procedure ObjetoDe(AArq: TArquivoMigrado; ANo: TNoFormulario);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Analisar(AConjunto: TConjuntoArquivos);
    function Escolher(out AConfianca: TConfianca): string;
    procedure Evidencias(ADest: TStrings);
  end;

function FaixaTexto(AMascara: integer): string;
var
  vInt, vIni, vFim: integer;
  vPartes: TStringList;
begin
  if AMascara = 0 then
    Exit(cmDetSemFaixa);
  if (AMascara and 7) = 7 then
  begin
    Result := '1.x';
    if (AMascara and 8) <> 0 then
      Result := Result + ', 2.0';
    Exit;
  end;
  vPartes := TStringList.Create;
  try
    vInt := 0;
    while vInt <= High(VersoesDeApi) do
    begin
      if (AMascara and (1 shl vInt)) = 0 then
      begin
        Inc(vInt);
        Continue;
      end;
      vIni := vInt;
      vFim := vInt;
      while (vFim < High(VersoesDeApi)) and ((AMascara and (1 shl (vFim + 1))) <> 0) do
        Inc(vFim);
      if vIni = vFim then
        vPartes.Add(VersoesDeApi[vIni])
      else
        vPartes.Add(VersoesDeApi[vIni] + '-' + VersoesDeApi[vFim]);
      vInt := vFim + 1;
    end;
    vPartes.Delimiter := ',';
    vPartes.StrictDelimiter := True;
    Result := StringReplace(vPartes.DelimitedText, ',', ', ', [rfReplaceAll]);
  finally
    vPartes.Free;
  end;
end;

{ TDetector }

constructor TDetector.Create;
var
  vInt, vPac: integer;
  vApi: TApi;
begin
  inherited Create;
  FPistas := TStringList.Create;
  FPistas.OwnsObjects := True;
  FPistas.CaseSensitive := False;
  FPublicadas := TStringList.Create;
  FPublicadas.OwnsObjects := True;
  FPublicadas.CaseSensitive := False;
  for vInt := Low(VersoesDeApi) to High(VersoesDeApi) do
  begin
    vApi := CarregarApiDaVersao(VersoesDeApi[vInt], '');
    FApis[vInt] := vApi;
    FNomesPacotes[vInt] := NovaOrdenada(False);
    FNomesUnits[vInt] := NovaOrdenada(False);
    if vApi = nil then
      Continue;
    FTodas := FTodas or (1 shl vInt);
    for vPac := 0 to vApi.PacoteCount - 1 do
      FNomesPacotes[vInt].Add(vApi.Pacotes[vPac].Nome);
    FNomesUnits[vInt].AddStrings(vApi.Unidades);
  end;
end;

destructor TDetector.Destroy;
var
  vInt: integer;
begin
  for vInt := Low(FApis) to High(FApis) do
  begin
    FApis[vInt].Free;
    FNomesPacotes[vInt].Free;
    FNomesUnits[vInt].Free;
  end;
  FPistas.Free;
  FPublicadas.Free;
  inherited Destroy;
end;

function TDetector.MascaraDe(AListas: array of TStringList; const ANome: string): integer;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := Low(AListas) to High(AListas) do
    if (FApis[vInt] <> nil) and (AListas[vInt].IndexOf(ANome) >= 0) then
      Result := Result or (1 shl vInt);
end;

function TDetector.MascaraClasse(const AClasse: string): integer;
var
  vInt: integer;
begin
  Result := 0;
  // only the RAL classes tell versions apart (a TForm1 could be anywhere)
  if not SameText(Copy(AClasse, 1, 4), 'TRAL') then
    Exit;
  for vInt := Low(FApis) to High(FApis) do
    if (FApis[vInt] <> nil) and (FApis[vInt].BuscarClasse(AClasse) <> nil) then
      Result := Result or (1 shl vInt);
end;

function TDetector.Publicadas(AVersao: integer; const AClasse: string): TStringList;
var
  vChave: string;
  vInd: integer;
  vClasse: TApiClasse;
  vTmp: TStringList;
  vInt: integer;
begin
  vChave := IntToStr(AVersao) + ':' + AClasse;
  vInd := FPublicadas.IndexOf(vChave);
  if vInd >= 0 then
    Exit(TStringList(FPublicadas.Objects[vInd]));
  Result := NovaOrdenada(False);
  vClasse := FApis[AVersao].BuscarClasse(AClasse);
  if vClasse <> nil then
  begin
    vTmp := CriarListaMembros;
    try
      FApis[AVersao].Publicadas(vClasse, vTmp);
      for vInt := 0 to vTmp.Count - 1 do
        Result.Add(vTmp[vInt]);
    finally
      vTmp.Free;
    end;
  end;
  FPublicadas.AddObject(vChave, Result);
end;

function TDetector.MascaraPropriedade(const AClasse, APropriedade: string): integer;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := Low(FApis) to High(FApis) do
    if (FApis[vInt] <> nil) and (FApis[vInt].BuscarClasse(AClasse) <> nil) and
       (Publicadas(vInt, AClasse).IndexOf(APropriedade) >= 0) then
      Result := Result or (1 shl vInt);
end;

procedure TDetector.Pista(const AChave, AArquivo, ANome: string; AMascara: integer);
var
  vInd: integer;
  vPista: TPista;
begin
  if (AMascara = 0) or (AMascara = FTodas) then
    Exit;
  vInd := FPistas.IndexOf(AChave);
  if vInd >= 0 then
  begin
    Inc(TPista(FPistas.Objects[vInd]).Vezes);
    Exit;
  end;
  vPista := TPista.Create;
  vPista.Chave := AChave;
  vPista.Arquivo := AArquivo;
  vPista.Nome := ANome;
  vPista.Mascara := AMascara;
  vPista.Vezes := 1;
  FPistas.AddObject(AChave, vPista);
end;

procedure TDetector.PacoteNome(AArq: TArquivoMigrado; const ANome: string);
var
  vNome: string;
begin
  vNome := Trim(ANome);
  if (vNome = '') or (Pos('$', vNome) > 0) then
    Exit;
  Pista('pacote:' + LowerCase(vNome), AArq.Relativo, vNome,
        MascaraDe(FNomesPacotes, vNome));
end;

procedure TDetector.PacotesDe(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vItens: TStringList;
  vInt, vCl: integer;
  vEd: TEditorUses;
begin
  if AArq.Tipo = taDpk then
  begin
    vEd := TEditorUses.Create(AArq.Texto);
    try
      if vEd.Clausula(suRequires) <> nil then
        for vInt := 0 to vEd.Clausula(suRequires).Count - 1 do
          PacoteNome(AArq, vEd.Clausula(suRequires).Item(vInt).Nome);
    finally
      vEd.Free;
    end;
    Exit;
  end;
  vXml := TProjetoXML.Create(AArq.Texto);
  vItens := TStringList.Create;
  try
    if AArq.Tipo = taDproj then
    begin
      vLista := vXml.Buscar('//DCC_UsePackage');
      try
        vItens.Delimiter := ';';
        vItens.StrictDelimiter := True;
        for vCl := 0 to vLista.Count - 1 do
        begin
          vItens.DelimitedText := vXml.ValorTexto(TElementoXML(vLista[vCl]));
          for vInt := 0 to vItens.Count - 1 do
            PacoteNome(AArq, vItens[vInt]);
        end;
      finally
        vLista.Free;
      end;
    end
    else
    begin
      vXml.ListarPacotes(vXml.ContainerDePacotes, vItens);
      for vInt := 0 to vItens.Count - 1 do
        PacoteNome(AArq, vItens[vInt]);
    end;
  finally
    vItens.Free;
    vXml.Free;
  end;
end;

procedure TDetector.UnitsDe(AArq: TArquivoMigrado);
var
  vEd: TEditorUses;
  vCl, vIt: integer;
  vNome: string;
begin
  vEd := TEditorUses.Create(AArq.Texto);
  try
    for vCl := 0 to vEd.Count - 1 do
      for vIt := 0 to vEd.Clausulas[vCl].Count - 1 do
      begin
        vNome := vEd.Clausulas[vCl].Item(vIt).Nome;
        Pista('unit:' + LowerCase(vNome), AArq.Relativo, vNome,
              MascaraDe(FNomesUnits, vNome));
      end;
  finally
    vEd.Free;
  end;
end;

procedure TDetector.ObjetoDe(AArq: TArquivoMigrado; ANo: TNoFormulario);
var
  vMasc, vInt, vPonto: integer;
  vProp: string;
begin
  vMasc := MascaraClasse(ANo.Classe);
  if vMasc <> 0 then
  begin
    Pista('classe:' + LowerCase(ANo.Classe), AArq.Relativo,
          Format(cmDetClasse, [ANo.Classe]), vMasc);
    for vInt := 0 to ANo.PropriedadeCount - 1 do
    begin
      vProp := ANo.Propriedades[vInt].Nome;
      vPonto := Pos('.', vProp);
      if vPonto > 0 then
        vProp := Copy(vProp, 1, vPonto - 1);
      Pista('prop:' + LowerCase(ANo.Classe + '.' + vProp), AArq.Relativo,
            Format(cmDetPropriedade, [ANo.Classe, vProp]),
            MascaraPropriedade(ANo.Classe, vProp));
    end;
  end;
  for vInt := 0 to ANo.FilhoCount - 1 do
    ObjetoDe(AArq, ANo.Filhos[vInt]);
end;

procedure TDetector.FormularioDe(AArq: TArquivoMigrado);
var
  vForm: TFormulario;
  vInt: integer;
begin
  try
    vForm := TFormulario.Create(AArq.Texto);
  except
    on EFormularioInvalido do
      Exit;
  end;
  try
    for vInt := 0 to vForm.RaizCount - 1 do
      ObjetoDe(AArq, vForm.Raizes[vInt]);
  finally
    vForm.Free;
  end;
end;

procedure TDetector.Analisar(AConjunto: TConjuntoArquivos);
var
  vLista: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  vLista := TList.Create;
  try
    AConjunto.Listar([taDproj, taDpk, taLpi, taLpk, taPas, taDpr, taLpr, taForm], vLista);
    for vInt := 0 to vLista.Count - 1 do
    begin
      vArq := TArquivoMigrado(vLista[vInt]);
      try
        case vArq.Tipo of
          taDproj, taDpk, taLpi, taLpk: PacotesDe(vArq);
          taPas, taDpr, taLpr: UnitsDe(vArq);
          taForm: FormularioDe(vArq);
        end;
        if vArq.Tipo in [taDpk] then
          UnitsDe(vArq);
      except
        on Exception do
          ; // a file that does not parse gives no clue
      end;
    end;
  finally
    vLista.Free;
  end;
end;

function TDetector.Escolher(out AConfianca: TConfianca): string;
var
  vVer, vPis, vPontos, vMelhor, vPerfeitos: integer;
  vMelhorPontos: integer;
  vPista: TPista;
begin
  Result := '';
  AConfianca := cfNenhuma;
  if FPistas.Count = 0 then
    Exit;
  vMelhor := -1;
  vMelhorPontos := -1;
  vPerfeitos := 0;
  for vVer := Low(VersoesDeApi) to High(VersoesDeApi) do
  begin
    if FApis[vVer] = nil then
      Continue;
    vPontos := 0;
    for vPis := 0 to FPistas.Count - 1 do
    begin
      vPista := TPista(FPistas.Objects[vPis]);
      if (vPista.Mascara and (1 shl vVer)) <> 0 then
        Inc(vPontos);
    end;
    if vPontos = FPistas.Count then
      Inc(vPerfeitos);
    if vPontos >= vMelhorPontos then
    begin
      vMelhorPontos := vPontos;
      vMelhor := vVer;
    end;
  end;
  if vMelhor < 0 then
    Exit;
  Result := VersoesDeApi[vMelhor];
  if vMelhorPontos < FPistas.Count then
    AConfianca := cfBaixa
  else if vPerfeitos = 1 then
    AConfianca := cfAlta
  else
    AConfianca := cfMedia;
end;

procedure TDetector.Evidencias(ADest: TStrings);
var
  vInt: integer;
  vPista: TPista;
  vLinha: string;
begin
  for vInt := 0 to FPistas.Count - 1 do
  begin
    vPista := TPista(FPistas.Objects[vInt]);
    vLinha := vPista.Arquivo + ': ' + vPista.Nome + ' -> ' + FaixaTexto(vPista.Mascara);
    if vPista.Vezes > 1 then
      vLinha := vLinha + Format(cmDetMais, [vPista.Vezes - 1]);
    ADest.Add(vLinha);
  end;
end;

function DetectarVersaoEx(AConjunto: TConjuntoArquivos; out AEvidencias: TStrings;
  out AConfianca: TConfianca): string;
var
  vDet: TDetector;
begin
  AEvidencias := TStringList.Create;
  vDet := TDetector.Create;
  try
    vDet.Analisar(AConjunto);
    Result := vDet.Escolher(AConfianca);
    vDet.Evidencias(AEvidencias);
  finally
    vDet.Free;
  end;
end;

function DetectarVersao(AConjunto: TConjuntoArquivos; out AEvidencias: TStrings): string;
var
  vConf: TConfianca;
begin
  Result := DetectarVersaoEx(AConjunto, AEvidencias, vConf);
end;

end.
