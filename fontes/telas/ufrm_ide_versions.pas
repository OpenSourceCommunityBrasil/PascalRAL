unit ufrm_ide_versions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ufrm_modelo, ufrm_ide_version, ideutils, RALInst.IDE, RALInst.Catalogo,
  RALInst.Receitas;

type

  { Tfrm_ide_versions }

  Tfrm_ide_versions = class(Tfrm_modelo)
    dirSelect: TSelectDirectoryDialog;
    lbFind: TLabel;
    bAutoBusca: TSpeedButton;
    lbIDEListing: TLabel;
    lbSubTitle: TLabel;
    sbIDEVersions: TScrollBox;
    bAddVersion: TSpeedButton;
    bStopBusca: TSpeedButton;
    procedure bAddVersionClick(Sender: TObject);
    procedure bAutoBuscaClick(Sender: TObject);
    procedure bStopBuscaClick(Sender: TObject);
  private
    FTop: integer;
    FCancelBusca: boolean;
    FFilesFind: integer;
    // as instancias encontradas pertencem a esta lista; os frames so as mostram
    FLista: TIDEList;

    procedure clearVersions;
    procedure mostrarVersoes;
    // as buscas do tipo escolhido: uma, ou as duas (F9: Delphi e Lazarus juntos)
    procedure criarBuscas(ALista: TList);
    procedure iniciarBusca;
    procedure terminarBusca;

    procedure OnIDEFind(const APath: string; var ACancel: boolean);
  protected
    procedure SetIDE(AValue: integer); override;
    function validatePageNext : boolean; override;
    function validatePagePrior : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    // quantas IDEs estao marcadas
    function Marcadas: integer;
    // o plano de todas as IDEs marcadas, antes de executar
    function Plano(AEscolha: TEscolhaInstalacao): string;
    // as IDEs marcadas (TIDEObjectData, que pertencem a esta tela)
    procedure ListarMarcadas(ALista: TList);
    function installRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
    // F9: o relatorio final, uma entrada por IDE marcada
    function Resumos: string;
  end;

implementation

{$R *.lfm}

uses
  udm, lazarusutils, RALInst.IDE.Lazarus, RALInst.Processo
  {$IFDEF MSWINDOWS}, delphiutils, RALInst.IDE.Delphi{$ENDIF};

// as pastas que o usuario ja apontou com "adicionar pasta": entram na busca
// rapida das proximas vezes (IDE fora dos lugares de sempre, D:\IDE\lazarus)
function ArquivoPastas: string;
begin
  Result := PastaDadosInstalador + 'pastas-ide.txt';
end;

procedure LerPastas(ALista: TStrings);
begin
  ALista.Clear;
  if FileExists(ArquivoPastas) then
    try
      ALista.LoadFromFile(ArquivoPastas);
    except
      ALista.Clear;
    end;
end;

procedure LembrarPasta(const APasta: string);
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.CaseSensitive := False;
    LerPastas(vLista);
    if vLista.IndexOf(ExcludeTrailingPathDelimiter(APasta)) >= 0 then
      Exit;
    vLista.Add(ExcludeTrailingPathDelimiter(APasta));
    try
      ForceDirectories(ExtractFilePath(ArquivoPastas));
      vLista.SaveToFile(ArquivoPastas);
    except
      // lembrar e conveniencia: sem gravar, a busca so nao a acha sozinha
    end;
  finally
    vLista.Free;
  end;
end;

{ Tfrm_ide_versions }

procedure Tfrm_ide_versions.criarBuscas(ALista: TList);
var
  vBusca: TBuscaIDE;
begin
  // 0 - Delphi, 1 - Lazarus, 2 - os dois
  ALista.Clear;
  {$IFDEF MSWINDOWS}
    if (IDE = 0) or (IDE = 2) then
    begin
      vBusca := TBuscaDelphi.Create;
      vBusca.OnBusca := @OnIDEFind;
      ALista.Add(vBusca);
    end;
  {$ENDIF}
  if (IDE = 1) or (IDE = 2) then
  begin
    vBusca := TBuscaLazarus.Create;
    vBusca.OnBusca := @OnIDEFind;
    ALista.Add(vBusca);
  end;
end;

procedure LiberarBuscas(ALista: TList);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(ALista.Count) do
    TObject(ALista[vInt]).Free;
  ALista.Clear;
end;

procedure Tfrm_ide_versions.iniciarBusca;
begin
  FFilesFind := 0;
  FCancelBusca := False;
  bAutoBusca.Enabled := False;
  bAddVersion.Enabled := False;
  bStopBusca.Visible := True;
  Screen.Cursor := crHourGlass;
end;

procedure Tfrm_ide_versions.terminarBusca;
begin
  Screen.Cursor := crDefault;
  bAutoBusca.Enabled := True;
  bAddVersion.Enabled := True;
  bStopBusca.Visible := False;
  lbFind.Caption := '';
end;

procedure Tfrm_ide_versions.bAutoBuscaClick(Sender: TObject);
var
  vBuscas: TList;
  vInt: integer;
begin
  // a busca rapida ja rodou ao abrir a tela; este botao varre os discos inteiros
  iniciarBusca;
  vBuscas := TList.Create;
  try
    criarBuscas(vBuscas);
    for vInt := 0 to Pred(vBuscas.Count) do
    begin
      if TObject(vBuscas[vInt]) is TBuscaLazarus then
        TBuscaLazarus(vBuscas[vInt]).BuscarCompleta(FLista);
      TBuscaIDE(vBuscas[vInt]).Finalizar(FLista);
    end;
  finally
    LiberarBuscas(vBuscas);
    vBuscas.Free;
    terminarBusca;
  end;
  mostrarVersoes;
end;

procedure Tfrm_ide_versions.bStopBuscaClick(Sender: TObject);
begin
  FCancelBusca := True;
end;

procedure Tfrm_ide_versions.bAddVersionClick(Sender: TObject);
var
  vBuscas: TList;
  vAntes, vInt: integer;
  vConhecida: boolean;
begin
  if not dirSelect.Execute then
    Exit;

  // a pasta pode ser a propria IDE ou uma pasta com varias (D:\IDE\lazarus)
  iniciarBusca;
  vBuscas := TList.Create;
  try
    criarBuscas(vBuscas);
    vAntes := FLista.Count;
    vConhecida := FLista.BuscarPorRaiz(dirSelect.FileName) <> nil;
    for vInt := 0 to Pred(vBuscas.Count) do
    begin
      TBuscaIDE(vBuscas[vInt]).BuscarEm(FLista, dirSelect.FileName, 4);
      TBuscaIDE(vBuscas[vInt]).Finalizar(FLista);
    end;
  finally
    LiberarBuscas(vBuscas);
    vBuscas.Free;
    terminarBusca;
  end;

  mostrarVersoes;

  if (FLista.Count = vAntes) and not vConhecida then
    ShowMessage('Nenhuma instalação encontrada em ' + dirSelect.FileName)
  else
    LembrarPasta(dirSelect.FileName);
end;

procedure Tfrm_ide_versions.clearVersions;
var
  vInt: Integer;
begin
  for vInt := Pred(sbIDEVersions.ControlCount) downto 0 do
  begin
    if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
      sbIDEVersions.Controls[vInt].Free;
  end;
  FTop := 0;
end;

procedure Tfrm_ide_versions.mostrarVersoes;
var
  vInt: Integer;
  vDesmarcadas: TStringList;
  vFrm: Tfrm_ide_version;
  vObj: TIDEObjectData;
  vIDE: TIDEInstance;
begin
  // a lista inteira e redesenhada: uma IDE nova muda a ordem e os avisos
  // das outras. O que o usuario desmarcou continua desmarcado.
  vDesmarcadas := TStringList.Create;
  try
    for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
      if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
      begin
        vFrm := Tfrm_ide_version(sbIDEVersions.Controls[vInt]);
        if not vFrm.ckSelecionado.Checked then
          vDesmarcadas.Add(vFrm.ObjectData.Instancia.RootDir);
      end;

    sbIDEVersions.DisableAutoSizing;
    try
      clearVersions;
      for vInt := 0 to Pred(FLista.Count) do
      begin
        vIDE := FLista[vInt];
        {$IFDEF MSWINDOWS}
          if vIDE.Tipo = tiDelphi then
            vObj := TDelphiObjectData.Create(vIDE)
          else
        {$ENDIF}
          vObj := TLazarusObjectData.Create(vIDE);

        vFrm := Tfrm_ide_version.Create(Self, vObj);
        vFrm.Name := 'ide_version_' + IntToStr(vInt);
        vFrm.Parent := sbIDEVersions;
        vFrm.Top := FTop;
        vFrm.Align := alTop;
        if vDesmarcadas.IndexOf(vIDE.RootDir) >= 0 then
          vFrm.ckSelecionado.Checked := False;
        FTop := FTop + vFrm.Height;
      end;
    finally
      sbIDEVersions.EnableAutoSizing;
    end;
  finally
    vDesmarcadas.Free;
  end;
end;

procedure Tfrm_ide_versions.OnIDEFind(const APath: string; var ACancel: boolean);

  function CortePath(APasta: string) : string;
  var
    vPos, vIni: integer;
  begin
    if Length(APasta) > 90 then
    begin
      vIni := 0;
      APasta := ExcludeTrailingPathDelimiter(APasta);
      repeat
        vPos := Pos(PathDelim, APasta, vIni + 1);
        if vPos > 0 then
          vIni := vPos;
      until (Length(Copy(APasta, vPos, Length(APath))) <= 87) or (vPos = 0);
      if vPos = 0 then
        vPos := vIni;
      Result := '...' + Copy(APasta, vPos, Length(APasta));
      Result := IncludeTrailingPathDelimiter(Result);
    end
    else
    begin
      Result := APasta;
    end;
  end;
begin
  ACancel := FCancelBusca;
  FFilesFind := FFilesFind + 1;
  if FFilesFind = 200 then
  begin
    lbFind.Caption := CortePath(APath);
    Application.ProcessMessages;
    FFilesFind := 0;
  end;
end;

procedure Tfrm_ide_versions.SetIDE(AValue: integer);
var
  vMudou: boolean;
  vBuscas: TList;
  vInt: integer;
  vPastas: TStringList;
  vPasta: string;
begin
  // 0 - Delphi, 1 - Lazarus, 2 - os dois (F9)
  vMudou := IDE <> AValue;
  inherited SetIDE(AValue);

  // a varredura de discos inteiros so faz sentido para o Lazarus; o Delphi
  // e achado pelo registro e pelas pastas ao lado das IDEs registradas
  bAddVersion.Visible := (AValue >= 0) and (AValue <= 2);
  bAutoBusca.Visible := (AValue = 1) or (AValue = 2);

  if not vMudou then
    Exit;

  clearVersions;
  FLista.Clear;
  if (AValue < 0) or (AValue > 2) then
    Exit;

  // busca rapida (registro, raizes conhecidas e as pastas que o usuario ja
  // apontou antes) assim que a tela abre
  iniciarBusca;
  vBuscas := TList.Create;
  vPastas := TStringList.Create;
  try
    LerPastas(vPastas);
    criarBuscas(vBuscas);
    for vInt := 0 to Pred(vBuscas.Count) do
    begin
      TBuscaIDE(vBuscas[vInt]).BuscarPadrao(FLista);
      for vPasta in vPastas do
        if DirectoryExists(vPasta) then
          TBuscaIDE(vBuscas[vInt]).BuscarEm(FLista, vPasta, 4);
      TBuscaIDE(vBuscas[vInt]).Finalizar(FLista);
    end;
  finally
    vPastas.Free;
    LiberarBuscas(vBuscas);
    vBuscas.Free;
    terminarBusca;
  end;
  mostrarVersoes;
end;

function Tfrm_ide_versions.validatePageNext: boolean;
begin
  Result := not bStopBusca.Visible;
  if Result and (Marcadas = 0) then
  begin
    ShowMessage('Marque ao menos uma IDE para instalar.');
    Result := False;
  end;
end;

function Tfrm_ide_versions.validatePagePrior: boolean;
begin
  Result := not bStopBusca.Visible;
end;

constructor Tfrm_ide_versions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLista := TIDEList.Create(True);
  lbFind.Caption := '';
  FFilesFind := 0;
  FCancelBusca := False;
end;

destructor Tfrm_ide_versions.Destroy;
begin
  clearVersions;
  FreeAndNil(FLista);
  inherited Destroy;
end;

function Tfrm_ide_versions.Marcadas: integer;
var
  vInt: Integer;
begin
  Result := 0;
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
    if (sbIDEVersions.Controls[vInt] is Tfrm_ide_version) and
       Tfrm_ide_version(sbIDEVersions.Controls[vInt]).ckSelecionado.Checked then
      Inc(Result);
end;

function Tfrm_ide_versions.Plano(AEscolha: TEscolhaInstalacao): string;
var
  vInt: Integer;
  vFrm: Tfrm_ide_version;
begin
  Result := '';
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
    if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
    begin
      vFrm := Tfrm_ide_version(sbIDEVersions.Controls[vInt]);
      if vFrm.ckSelecionado.Checked then
        Result := Result + vFrm.ObjectData.Plano(AEscolha) + LineEnding;
    end;
end;

procedure Tfrm_ide_versions.ListarMarcadas(ALista: TList);
var
  vInt: Integer;
  vFrm: Tfrm_ide_version;
begin
  ALista.Clear;
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
    if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
    begin
      vFrm := Tfrm_ide_version(sbIDEVersions.Controls[vInt]);
      if vFrm.ckSelecionado.Checked then
        ALista.Add(vFrm.ObjectData);
    end;
end;

function Tfrm_ide_versions.Resumos: string;
var
  vLista: TList;
  vInt: integer;
begin
  Result := '';
  vLista := TList.Create;
  try
    ListarMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if TIDEObjectData(vLista[vInt]).Resumo <> '' then
        Result := Result + TIDEObjectData(vLista[vInt]).Resumo + LineEnding;
  finally
    vLista.Free;
  end;
end;

function Tfrm_ide_versions.installRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
var
  vInt: Integer;
  vFrm: Tfrm_ide_version;
begin
  // uma IDE que falha nao impede as outras: cada uma tem o seu relatorio
  Result := True;
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
  begin
    if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
    begin
      vFrm := Tfrm_ide_version(sbIDEVersions.Controls[vInt]);
      if not vFrm.installRAL(ALog, AEscolha) then
        Result := False;
    end;
  end;
end;

end.
