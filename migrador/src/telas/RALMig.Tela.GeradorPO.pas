/// Developer tool behind --gerar-po: writes languages/ralmigrador.<lang>.po with
/// every text of the window (components) and every resourcestring of the RALMig.*
/// units and of the RALInst.* units the program links. Translations already in
/// the current .po are kept; what is new takes the installer's translation of
/// the same text (src/languages/ralinstaller.<lang>.po) when there is one, and
/// otherwise stays empty to be written by hand (no network). Portuguese is the
/// original: its msgstr stay empty. The same .po serves ralmigrador and ralmigra.
unit RALMig.Tela.GeradorPO;

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
    FBase: TArquivoPO;
    FEntradas: TStringList;
    FIdentificadores: TStringList;
    FLang: string;
    FSemTraducao: integer;
    /// Adds one entry (once per identifier)
    procedure Adicionar(const AIdentificador, AOriginal: string; AComContexto: boolean);
    /// The texts of one component and its sub-objects, by RTTI
    procedure AdicionarObjeto(AObjeto: TPersistent; const ACaminho: string);
    /// The installer's .po of the language, when it is at hand
    function CarregarBase: TArquivoPO;
    function Traducao(const AIdentificador, AOriginal: string): string;
  public
    constructor Create(const ALang: string);
    destructor Destroy; override;
    /// Every .lfm-like text of the form, of its frames and of their components
    procedure AdicionarComponentes(ARaiz: TComponent);
    /// Every resourcestring of the RALMig.* and RALInst.* units
    procedure AdicionarMensagens;
    /// Writes the .po file
    procedure Salvar(const AArquivo: string);

    /// Entries written without a translation
    property SemTraducao: integer read FSemTraducao;
  end;

implementation

uses
  Forms, LCLType, TypInfo;

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
  if (SameText(Copy(AName, 1, 8), 'ralinst.')) or (SameText(Copy(AName, 1, 7), 'ralmig.'))
     or (SameText(Copy(AName, 1, 8), 'ralmigra')) then
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
  FBase := CarregarBase;
end;

destructor TGeradorPO.Destroy;
begin
  FBase.Free;
  FAnteriores.Free;
  FIdentificadores.Free;
  FEntradas.Free;
  inherited Destroy;
end;

procedure TGeradorPO.Adicionar(const AIdentificador, AOriginal: string;
  AComContexto: boolean);
var
  vTraducao: string;
begin
  if (AOriginal = '') or (FIdentificadores.IndexOf(AIdentificador) >= 0) then
    Exit;
  FIdentificadores.Add(AIdentificador);
  vTraducao := Traducao(AIdentificador, AOriginal);
  FEntradas.Add('#: ' + AIdentificador);
  if AComContexto then
    FEntradas.Add('msgctxt "' + AIdentificador + '"');
  FEntradas.Add('msgid "' + Escapar(AOriginal) + '"');
  FEntradas.Add('msgstr "' + Escapar(vTraducao) + '"');
  FEntradas.Add('');
  if (vTraducao = '') and not SameText(FLang, IdiomaOriginal) then
    Inc(FSemTraducao);
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

function TGeradorPO.CarregarBase: TArquivoPO;
var
  vArquivo: string;
  vTexto: TStringList;
begin
  Result := nil;
  // de migrador/ para a pasta do instalador e de la para src/languages
  vArquivo := ExpandFileName('..' + PathDelim + 'src' + PathDelim + 'languages' +
    PathDelim + 'ralinstaller.' + FLang + '.po');
  if not FileExists(vArquivo) then
    Exit;
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(vArquivo);
    Result := TArquivoPO.Create;
    Result.CarregarTexto(vTexto.Text);
  finally
    vTexto.Free;
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
    vArquivo.Add('"X-Generator: RAL Migrator\n"');
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
  if ((Result = '') or (Result = AOriginal)) and (FBase <> nil) then
    Result := FBase.Traduzir(AIdentificador, AOriginal);
  if Result = AOriginal then
    Result := '';
end;

end.
