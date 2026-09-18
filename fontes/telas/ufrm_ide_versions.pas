unit ufrm_ide_versions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ufrm_modelo, ufrm_ide_version, ideutils, RALInst.IDE;

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
    function criarBusca: TBuscaIDE;
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
    function installRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
  end;

implementation

{$R *.lfm}

uses
  udm, lazarusutils, RALInst.IDE.Lazarus
  {$IFDEF MSWINDOWS}, delphiutils, RALInst.IDE.Delphi{$ENDIF};

{ Tfrm_ide_versions }

function Tfrm_ide_versions.criarBusca: TBuscaIDE;
begin
  // 0 - Delphi
  // 1 - Lazarus
  {$IFDEF MSWINDOWS}
    if IDE = 0 then
      Result := TBuscaDelphi.Create
    else
  {$ENDIF}
    Result := TBuscaLazarus.Create;
  Result.OnBusca := @OnIDEFind;
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
  vBusca: TBuscaIDE;
begin
  // a busca rapida ja rodou ao abrir a tela; este botao varre os discos inteiros
  iniciarBusca;
  vBusca := criarBusca;
  try
    if vBusca is TBuscaLazarus then
      TBuscaLazarus(vBusca).BuscarCompleta(FLista);
    vBusca.Finalizar(FLista);
  finally
    vBusca.Free;
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
  vBusca: TBuscaIDE;
  vAntes: integer;
  vConhecida: boolean;
begin
  if not dirSelect.Execute then
    Exit;

  // a pasta pode ser a propria IDE ou uma pasta com varias (D:\IDE\lazarus)
  iniciarBusca;
  vBusca := criarBusca;
  try
    vAntes := FLista.Count;
    vConhecida := FLista.BuscarPorRaiz(dirSelect.FileName) <> nil;
    vBusca.BuscarEm(FLista, dirSelect.FileName, 4);
    vBusca.Finalizar(FLista);
  finally
    vBusca.Free;
    terminarBusca;
  end;

  mostrarVersoes;

  if (FLista.Count = vAntes) and not vConhecida then
    ShowMessage('Nenhuma instalação encontrada em ' + dirSelect.FileName);
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
  vBusca: TBuscaIDE;
begin
  // 0 - Delphi
  // 1 - Lazarus
  vMudou := IDE <> AValue;
  inherited SetIDE(AValue);

  // a varredura de discos inteiros so faz sentido para o Lazarus; o Delphi
  // e achado pelo registro e pelas pastas ao lado das IDEs registradas
  bAddVersion.Visible := (AValue = 0) or (AValue = 1);
  bAutoBusca.Visible := AValue = 1;

  if not vMudou then
    Exit;

  clearVersions;
  FLista.Clear;
  if (AValue <> 0) and (AValue <> 1) then
    Exit;

  // busca rapida (registro e raizes conhecidas) assim que a tela abre
  iniciarBusca;
  vBusca := criarBusca;
  try
    vBusca.BuscarPadrao(FLista);
    vBusca.Finalizar(FLista);
  finally
    vBusca.Free;
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
