/// Runtime translation of the installer texts. The .po files are embedded in
/// the executable (RCDATA PO_PT_BR, PO_EN_US, PO_ES_ES), so the single exe
/// speaks every language; a languages/ralinstaller.<lang>.po beside it wins, so
/// a translator can test without recompiling. LCL-free: the GUI and the CLI use
/// it the same way (the GUI also hands the text to the LCL form translator).
unit RALInst.Traducao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  /// The translations of one .po file.
  TArquivoPO = class
  private
    /// lowercase identifier (msgctxt or '#:' reference); Objects = index in
    /// FTraducoes
    FPorIdentificador: TStringList;
    /// original text; Objects = index in FTraducoes
    FPorOriginal: TStringList;
    FTraducoes: TStringList;
    /// Keeps one entry
    procedure Adicionar(const AContexto, AReferencia, AOriginal, ATraducao: string);
  public
    constructor Create;
    destructor Destroy; override;
    /// Reads the text of a .po file
    procedure CarregarTexto(const ATexto: string);
    /// The translation of an identifier ('unit.name' or 'TForm.Comp.Prop'),
    /// else of the original text; '' when there is none
    function Traduzir(const AIdentificador, AOriginal: string): string;
  end;

const
  /// Languages the installer ships, in the order of the language screen
  IdiomasInstalador: array[0..2] of string = ('en-US', 'es-ES', 'pt-BR');
  /// Language of the original texts (the msgid of every .po)
  IdiomaOriginal = 'pt-BR';

/// The .po of a language: languages/ralinstaller.<lang>.po beside the
/// executable wins, else the embedded resource; nil when there is neither
function CarregarPO(const AIdioma: string): TArquivoPO;
/// The language currently applied to the resourcestrings
function IdiomaAtual: string;
/// The installer language closest to the system's: pt-* and es-* as such,
/// anything else en-US
function IdiomaDoSistema: string;
/// Raw text of the .po of a language ('' when there is none)
function TextoPO(const AIdioma: string): string;
/// Translates every resourcestring of the program into the language; the
/// original language restores them
procedure TraduzirMensagens(const AIdioma: string);

implementation

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  gettext;

const
  // RT_RCDATA: no Windows vem da unit Windows, nos outros do system
  RecursoDados = PChar(10);

var
  GIdioma: string = IdiomaOriginal;

// "texto com \"escape\"" -> texto com "escape"
function DesfazerEscape(const ATexto: string): string;
var
  vInt: integer;
begin
  Result := '';
  vInt := 1;
  while vInt <= Length(ATexto) do
  begin
    if (ATexto[vInt] = '\') and (vInt < Length(ATexto)) then
    begin
      Inc(vInt);
      case ATexto[vInt] of
        'n': Result := Result + #10;
        't': Result := Result + #9;
        'r': Result := Result + #13;
      else
        Result := Result + ATexto[vInt];
      end;
    end
    else
      Result := Result + ATexto[vInt];
    Inc(vInt);
  end;
end;

// o conteudo entre as aspas de uma linha do .po
function Aspas(const ALinha: string): string;
var
  vIni, vFim: integer;
begin
  Result := '';
  vIni := Pos('"', ALinha);
  vFim := LastDelimiter('"', ALinha);
  if (vIni > 0) and (vFim > vIni) then
    Result := DesfazerEscape(Copy(ALinha, vIni + 1, vFim - vIni - 1));
end;

{ TArquivoPO }

constructor TArquivoPO.Create;
begin
  inherited Create;
  FPorIdentificador := TStringList.Create;
  FPorIdentificador.Sorted := True;
  FPorIdentificador.Duplicates := dupIgnore;
  FPorIdentificador.CaseSensitive := True;
  FPorOriginal := TStringList.Create;
  FPorOriginal.Sorted := True;
  FPorOriginal.Duplicates := dupIgnore;
  FPorOriginal.CaseSensitive := True;
  FTraducoes := TStringList.Create;
end;

destructor TArquivoPO.Destroy;
begin
  FTraducoes.Free;
  FPorOriginal.Free;
  FPorIdentificador.Free;
  inherited Destroy;
end;

procedure TArquivoPO.Adicionar(const AContexto, AReferencia, AOriginal,
  ATraducao: string);
var
  vTraducao: string;
  vIdx: PtrInt;
begin
  if AOriginal = '' then
    Exit;
  // sem traducao, vale o original: e o que devolve o portugues ao voltar
  vTraducao := ATraducao;
  if vTraducao = '' then
    vTraducao := AOriginal;
  vIdx := FTraducoes.Add(vTraducao);
  if AContexto <> '' then
    FPorIdentificador.AddObject(LowerCase(AContexto), TObject(vIdx))
  else if AReferencia <> '' then
    FPorIdentificador.AddObject(LowerCase(AReferencia), TObject(vIdx));
  FPorOriginal.AddObject(AOriginal, TObject(vIdx));
end;

procedure TArquivoPO.CarregarTexto(const ATexto: string);
var
  vLinhas: TStringList;
  vLinha, vCampo, vContexto, vReferencia, vOriginal, vTraducao: string;
  vInt: integer;

  procedure Fechar;
  begin
    Adicionar(vContexto, vReferencia, vOriginal, vTraducao);
    vContexto := '';
    vReferencia := '';
    vOriginal := '';
    vTraducao := '';
    vCampo := '';
  end;

begin
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := ATexto;
    vCampo := '';
    vContexto := '';
    vReferencia := '';
    vOriginal := '';
    vTraducao := '';
    for vInt := 0 to Pred(vLinhas.Count) do
    begin
      vLinha := Trim(vLinhas[vInt]);
      if vLinha = '' then
        Fechar
      else if Copy(vLinha, 1, 3) = '#: ' then
      begin
        // uma entrada nova pode comecar sem linha em branco antes
        if vOriginal <> '' then
          Fechar;
        vReferencia := Trim(Copy(vLinha, 4, MaxInt));
      end
      else if vLinha[1] = '#' then
        Continue
      else if Copy(vLinha, 1, 8) = 'msgctxt ' then
      begin
        if vOriginal <> '' then
          Fechar;
        vCampo := 'c';
        vContexto := Aspas(vLinha);
      end
      else if Copy(vLinha, 1, 6) = 'msgid ' then
      begin
        vCampo := 'i';
        vOriginal := Aspas(vLinha);
      end
      else if Copy(vLinha, 1, 7) = 'msgstr ' then
      begin
        vCampo := 's';
        vTraducao := Aspas(vLinha);
      end
      else if vLinha[1] = '"' then
        case vCampo of
          'c': vContexto := vContexto + Aspas(vLinha);
          'i': vOriginal := vOriginal + Aspas(vLinha);
          's': vTraducao := vTraducao + Aspas(vLinha);
        end;
    end;
    Fechar;
  finally
    vLinhas.Free;
  end;
end;

function TArquivoPO.Traduzir(const AIdentificador, AOriginal: string): string;
var
  vIdx: integer;
begin
  Result := '';
  vIdx := FPorIdentificador.IndexOf(LowerCase(AIdentificador));
  if vIdx >= 0 then
    Exit(FTraducoes[PtrInt(FPorIdentificador.Objects[vIdx])]);
  vIdx := FPorOriginal.IndexOf(AOriginal);
  if vIdx >= 0 then
    Result := FTraducoes[PtrInt(FPorOriginal.Objects[vIdx])];
end;

function TextoPO(const AIdioma: string): string;
var
  vArquivo, vRecurso: string;
  vTexto: TStringList;
  vStream: TResourceStream;
begin
  Result := '';
  vTexto := TStringList.Create;
  try
    vArquivo := ExtractFilePath(ParamStr(0)) + 'languages' + PathDelim +
                'ralinstaller.' + AIdioma + '.po';
    if FileExists(vArquivo) then
    begin
      vTexto.LoadFromFile(vArquivo);
      Exit(vTexto.Text);
    end;
    vRecurso := 'PO_' + UpperCase(StringReplace(AIdioma, '-', '_', [rfReplaceAll]));
    if FindResource(HINSTANCE, PChar(vRecurso), RecursoDados) = 0 then
      Exit;
    vStream := TResourceStream.Create(HINSTANCE, vRecurso, RecursoDados);
    try
      vTexto.LoadFromStream(vStream);
      Result := vTexto.Text;
    finally
      vStream.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

function CarregarPO(const AIdioma: string): TArquivoPO;
var
  vTexto: string;
begin
  Result := nil;
  vTexto := TextoPO(AIdioma);
  if vTexto = '' then
    Exit;
  Result := TArquivoPO.Create;
  Result.CarregarTexto(vTexto);
end;

function IdiomaAtual: string;
begin
  Result := GIdioma;
end;

function IdiomaDoSistema: string;
var
  vIdioma, vReserva: string;
begin
  GetLanguageIDs(vIdioma, vReserva);
  vIdioma := LowerCase(vIdioma);
  if Copy(vIdioma, 1, 2) = 'pt' then
    Result := 'pt-BR'
  else if Copy(vIdioma, 1, 2) = 'es' then
    Result := 'es-ES'
  else
    Result := 'en-US';
end;

function TraduzirRecurso(AName, AValue: AnsiString; AHash: longint;
  AArg: pointer): AnsiString;
begin
  Result := TArquivoPO(AArg).Traduzir(AName, AValue);
end;

procedure TraduzirMensagens(const AIdioma: string);
var
  vPO: TArquivoPO;
begin
  // o original volta primeiro: traduzir de novo parte sempre do portugues
  ResetResourceTables;
  GIdioma := IdiomaOriginal;
  if SameText(AIdioma, IdiomaOriginal) then
    Exit;
  vPO := CarregarPO(AIdioma);
  if vPO = nil then
    Exit;
  try
    SetResourceStrings(@TraduzirRecurso, vPO);
    GIdioma := AIdioma;
  finally
    vPO.Free;
  end;
end;

end.
