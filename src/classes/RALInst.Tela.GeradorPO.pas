/// Developer tool behind the hidden "Translate" button: writes
/// src/languages/ralinstaller.<lang>.po with every text of the screens (.lfm) and
/// every resourcestring of the RALInst.* units. Translations already in the
/// current .po are kept; only new texts are machine-translated (Google), to be
/// reviewed by hand. Portuguese is the original: its msgstr stay empty.
unit RALInst.Tela.GeradorPO;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Traducao;

type
  /// Collects the texts of the program into one .po file.
  TGeradorPO = class
  private
    FAnteriores: TArquivoPO;
    FEntradas: TStringList;
    FIdentificadores: TStringList;
    FLang: string;
    /// Adds one entry (once per identifier)
    procedure Adicionar(const AIdentificador, AOriginal: string; AComContexto: boolean);
    /// The texts of one component and its sub-objects, by RTTI
    procedure AdicionarObjeto(AObjeto: TPersistent; const ACaminho: string);
    /// The translation to write: the one already in the .po, else Google's
    function Traducao(const AIdentificador, AOriginal: string): string;
    /// Machine translation of a Portuguese text; '' when it fails
    function TraduzirGoogle(const ATexto: string): string;
  public
    constructor Create(const ALang: string);
    destructor Destroy; override;
    /// Every .lfm text of the form, of its frames and of their components
    procedure AdicionarComponentes(ARaiz: TComponent);
    /// Every resourcestring of the RALInst.* units
    procedure AdicionarMensagens;
    /// Writes the .po file
    procedure Salvar(const AArquivo: string);
  end;

implementation

uses
  Forms, LCLType, TypInfo, HTTPDefs, fpjson, jsonparser,
  RALInst.HTTP;

var
  GGerador: TGeradorPO;

// "texto" com aspas e barras escapadas, em uma linha
function Escapar(const ATexto: string): string;
begin
  Result := StringReplace(ATexto, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;

function ColetarMensagem(AName, AValue: AnsiString; AHash: longint;
  AArg: pointer): AnsiString;
begin
  if SameText(Copy(AName, 1, 8), 'ralinst.') then
    GGerador.Adicionar(AName, AValue, False);
  // o valor volta intacto: coletar nao traduz nada
  Result := AValue;
end;

{ TGeradorPO }

constructor TGeradorPO.Create(const ALang: string);
begin
  inherited Create;
  FLang := ALang;
  FEntradas := TStringList.Create;
  FIdentificadores := TStringList.Create;
  FIdentificadores.Sorted := True;
  FIdentificadores.CaseSensitive := False;
  FAnteriores := CarregarPO(ALang);
end;

destructor TGeradorPO.Destroy;
begin
  FAnteriores.Free;
  FIdentificadores.Free;
  FEntradas.Free;
  inherited Destroy;
end;

procedure TGeradorPO.Adicionar(const AIdentificador, AOriginal: string;
  AComContexto: boolean);
begin
  if (AOriginal = '') or (FIdentificadores.IndexOf(AIdentificador) >= 0) then
    Exit;
  FIdentificadores.Add(AIdentificador);
  FEntradas.Add('#: ' + AIdentificador);
  if AComContexto then
    FEntradas.Add('msgctxt "' + AIdentificador + '"');
  FEntradas.Add('msgid "' + Escapar(AOriginal) + '"');
  FEntradas.Add('msgstr "' + Escapar(Traducao(AIdentificador, AOriginal)) + '"');
  FEntradas.Add('');
end;

procedure TGeradorPO.AdicionarComponentes(ARaiz: TComponent);
var
  vInt: integer;
  vComp: TComponent;
begin
  // o caminho e o que o tradutor do LCL monta: Classe.Componente.Propriedade
  AdicionarObjeto(ARaiz, ARaiz.ClassName);
  for vInt := 0 to Pred(ARaiz.ComponentCount) do
  begin
    vComp := ARaiz.Components[vInt];
    if vComp is TCustomFrame then
      AdicionarComponentes(vComp)
    else if vComp.Name <> '' then
      AdicionarObjeto(vComp, ARaiz.ClassName + '.' + vComp.Name);
  end;
end;

procedure TGeradorPO.AdicionarMensagens;
begin
  GGerador := Self;
  try
    SetResourceStrings(@ColetarMensagem, nil);
  finally
    GGerador := nil;
  end;
end;

procedure TGeradorPO.AdicionarObjeto(AObjeto: TPersistent; const ACaminho: string);
var
  vLista: PPropList;
  vTotal, vInt: integer;
  vProp: PPropInfo;
  vFilho: TObject;
begin
  vTotal := GetPropList(AObjeto.ClassInfo, vLista);
  try
    for vInt := 0 to Pred(vTotal) do
    begin
      vProp := vLista^[vInt];
      if vProp^.PropType = TypeInfo(TTranslateString) then
        Adicionar(ACaminho + '.' + vProp^.Name, GetStrProp(AObjeto, vProp), True)
      else if vProp^.PropType^.Kind = tkClass then
      begin
        // subcomponentes (o EditLabel do TLabeledEdit), nao os vizinhos
        vFilho := GetObjectProp(AObjeto, vProp);
        if (vFilho is TComponent) and
           (csSubComponent in TComponent(vFilho).ComponentStyle) then
          AdicionarObjeto(TPersistent(vFilho), ACaminho + '.' + vProp^.Name);
      end;
    end;
  finally
    FreeMem(vLista);
  end;
end;

procedure TGeradorPO.Salvar(const AArquivo: string);
var
  vArquivo: TStringList;
begin
  vArquivo := TStringList.Create;
  try
    vArquivo.Add('msgid ""');
    vArquivo.Add('msgstr ""');
    vArquivo.Add('"Language: ' + StringReplace(FLang, '-', '_', []) + '\n"');
    vArquivo.Add('"MIME-Version: 1.0\n"');
    vArquivo.Add('"Content-Type: text/plain; charset=UTF-8\n"');
    vArquivo.Add('"Content-Transfer-Encoding: 8bit\n"');
    vArquivo.Add('"X-Generator: RAL Installer\n"');
    vArquivo.Add('');
    vArquivo.AddStrings(FEntradas);
    ForceDirectories(ExtractFilePath(ExpandFileName(AArquivo)));
    vArquivo.SaveToFile(AArquivo);
  finally
    vArquivo.Free;
  end;
end;

function TGeradorPO.Traducao(const AIdentificador, AOriginal: string): string;
begin
  Result := '';
  if SameText(FLang, IdiomaOriginal) then
    Exit;
  if FAnteriores <> nil then
    Result := FAnteriores.Traduzir(AIdentificador, AOriginal);
  // o .po devolve o original quando nao ha traducao
  if (Result = '') or (Result = AOriginal) then
    Result := TraduzirGoogle(AOriginal);
end;

function TGeradorPO.TraduzirGoogle(const ATexto: string): string;
var
  vHttp: TClienteHTTP;
  vTexto: string;
  vJSON, vFrases: TJSONData;
  vInt: integer;
begin
  Result := '';
  // https://wiki.lazarus.freepascal.org/Using_Google_Translate
  vHttp := TClienteHTTP.Create;
  try
    if not vHttp.ObterTexto('https://translate.googleapis.com/translate_a/single?' +
         'client=gtx&sl=pt&tl=' + Copy(FLang, 1, 2) + '&dt=t&ie=UTF-8&oe=UTF-8&q=' +
         HTTPEncode(ATexto), vTexto) then
      Exit;
  finally
    vHttp.Free;
  end;
  try
    vJSON := GetJSON(vTexto);
  except
    Exit;
  end;
  try
    // [[["traducao","original",...], ...], ...]: as frases em ordem
    vFrases := vJSON.FindPath('[0]');
    if (vFrases <> nil) and (vFrases.JSONType = jtArray) then
      for vInt := 0 to Pred(vFrases.Count) do
        if (vFrases.Items[vInt].JSONType = jtArray) and
           (vFrases.Items[vInt].Count > 0) then
          Result := Result + vFrases.Items[vInt].Items[0].AsString;
  finally
    vJSON.Free;
  end;
end;

end.
