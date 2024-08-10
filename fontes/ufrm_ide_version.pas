unit ufrm_ide_version;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  delphiutils, lazarusutils, installparser, process;

type

  { Tfrm_ide_version }

  Tfrm_ide_version = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ckSelecionado: TCheckBox;
    lbIDEName: TLabel;
    lbPath: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    prcBuildIDE: TProcess;
  private
    FExecFile : string;
    FBuildFile : string;
    FDelphiVerion : TDelphiVersions;
    FIsDelphi : boolean;
  protected
    function GetIDEName: string;
    procedure SetExecFile(AValue: string);
    procedure SetIDEName(AValue: string);

    function logar(ALog : TMemo; AStr : string) : boolean;

    procedure LazarusInstallPackages(APackages : TStrings; AIsLink : boolean = false);
    procedure InstallLazarus(ALog: TMemo; AInstaller: TInstaller; APathDownload: string);
  public
    constructor Create(AOwner : TComponent); override;
    procedure installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload: string);
  published
    property IDEName : string read GetIDEName write SetIDEName;
    property ExecFile : string read FExecFile write SetExecFile;
    property BuildFile : string read FBuildFile write FBuildFile;
    property DelphiVerion : TDelphiVersions read FDelphiVerion write FDelphiVerion;
    property IsDelphi : boolean read FIsDelphi write FIsDelphi;
  end;

implementation

{$R *.lfm}

{ Tfrm_ide_version }

procedure Tfrm_ide_version.SetExecFile(AValue: string);
begin
  if FExecFile = AValue then
    Exit;

  FExecFile := AValue;
  if not FIsDelphi then
    FBuildFile := ExtractFilePath(FExecFile) + LazBuildFile;
  lbPath.Caption := ExtractFilePath(FExecFile);
end;

function Tfrm_ide_version.GetIDEName: string;
begin
  Result := lbIDEName.Caption;
end;

procedure Tfrm_ide_version.SetIDEName(AValue: string);
begin
  lbIDEName.Caption := AValue;
end;

function Tfrm_ide_version.logar(ALog: TMemo; AStr: string): boolean;
begin
  Result := ALog <> nil;
  if Result then
  begin
    ALog.Lines.Add(AStr);
    Application.ProcessMessages;
  end;
end;

procedure Tfrm_ide_version.LazarusInstallPackages(APackages: TStrings; AIsLink : boolean);
var
  vInt: Integer;
begin
  prcBuildIDE.Executable := FBuildFile;
  prcBuildIDE.Parameters.Clear;
  prcBuildIDE.Parameters.Add('--build-ide=');

  if APackages.Count > 0 then
  begin
    if AIsLink then
      prcBuildIDE.Parameters.Add('--add-package-link')
    else
      prcBuildIDE.Parameters.Add('--add-package');
  end;

  for vInt := 0 to Pred(APackages.Count) do
    prcBuildIDE.Parameters.Add(APackages.Strings[vInt]);

//  APackages.SaveToFile('d:\ral.txt');

  prcBuildIDE.Execute;
end;

procedure Tfrm_ide_version.InstallLazarus(ALog: TMemo; AInstaller: TInstaller;
  APathDownload: string);
var
  vInt1, vInt2: Integer;
  vLst, vPacks, vNames : TStringList;
  vPacksLink : TStringList;
  vPacksRAL, vNamesRAL : TStringList;
  vDepedancy : TDepedancy;
  vFile, vAux1 : string;
begin
  vPacks := TStringList.Create;
  vPacksLink := TStringList.Create;
  vNames := TStringList.Create;

  vPacksRAL := TStringList.Create;
  vNamesRAL := TStringList.Create;
  try
    vLst := TStringList.Create;
    try
      // pegando o name do package do rRAL
      vNamesRAL.Add(AInstaller.Packages.Name);

      // pegando o packqge do RAL
      vFile := IncludeTrailingPathDelimiter(APathDownload) +
               AInstaller.Packages.Folder;
      vFile := IncludeTrailingPathDelimiter(vFile) +
               AInstaller.Packages.Install.LPK;
      vPacksRAL.Add(vFile);

      // pegando dependencias do engines packges
      for vInt1 := 0 to Pred(AInstaller.Packages.Engines.Count) do
      begin
        if AInstaller.Packages.Engines.Engine[vInt1].Selected then
        begin
          // pegando o name do packqge
          vNamesRAL.Add(AInstaller.Packages.Engines.Engine[vInt1].Name);

          // pegando o packqge
          if AInstaller.Packages.Engines.Engine[vInt1].Install.LPK <> '' then
          begin
            vFile := IncludeTrailingPathDelimiter(APathDownload) +
                     AInstaller.Packages.Folder;
            vFile := IncludeTrailingPathDelimiter(vFile) +
                     AInstaller.Packages.Engines.Engine[vInt1].Install.LPK;
            vPacksRAL.Add(vFile);
          end;

          // pegando as dependencias
          for vInt2 := 0 to Pred(AInstaller.Packages.Engines.Engine[vInt1].Depedancy.Count) do
          begin
            vAux1 := AInstaller.Packages.Engines.Engine[vInt1].Depedancy.Strings[vInt2];
            if vLst.IndexOf(vAux1) < 0 then
              vLst.Add(vAux1);
          end;
        end;
      end;

      // verificando se alguma database foi selecionado pra install o RALDB
      for vInt1 := 0 to Pred(AInstaller.Packages.Databases.Packages.Count) do
      begin
        if AInstaller.Packages.Databases.Packages.Database[vInt1].Selected then
        begin
          AInstaller.Packages.Databases.Selected := True;
          Break;
        end;
      end;

      if AInstaller.Packages.Databases.Selected then begin
        // pegando o name do package
        vNamesRAL.Add(AInstaller.Packages.Databases.Name);

        // pegando o packqge
        if AInstaller.Packages.Databases.Install.LPK <> '' then
        begin
          vFile := IncludeTrailingPathDelimiter(APathDownload) +
                   AInstaller.Packages.Folder;
          vFile := IncludeTrailingPathDelimiter(vFile) +
                   AInstaller.Packages.Databases.Install.LPK;
          vPacksRAL.Add(vFile);
        end;
      end;

      // pegando dependencias do databases packages
      for vInt1 := 0 to Pred(AInstaller.Packages.Databases.Packages.Count) do
      begin
        if AInstaller.Packages.Databases.Packages.Database[vInt1].Selected then
        begin
          // pegando o name do package
          vNamesRAL.Add(AInstaller.Packages.Databases.Packages.Database[vInt1].Name);

          // pegando o package
          if AInstaller.Packages.Databases.Packages.Database[vInt1].Install.LPK <> '' then
          begin
            vFile := IncludeTrailingPathDelimiter(APathDownload) +
                     AInstaller.Packages.Folder;
            vFile := IncludeTrailingPathDelimiter(vFile) +
                     AInstaller.Packages.Databases.Packages.Database[vInt1].Install.LPK;
            vPacksRAL.Add(vFile);
          end;

          // pegando as dependencias
          for vInt2 := 0 to Pred(AInstaller.Packages.Databases.Packages.Database[vInt1].Depedancy.Count) do begin
            vAux1 := AInstaller.Packages.Databases.Packages.Database[vInt1].Depedancy.Strings[vInt2];
            if vLst.IndexOf(vAux1) < 0 then
              vLst.Add(vAux1);
          end;
        end;
      end;

      // pegando dependencias do compression packges
      for vInt1 := 0 to Pred(AInstaller.Packages.Compression.Count) do
      begin
        if AInstaller.Packages.Compression.Compression[vInt1].Selected then
        begin
          // pegando o name do packqge
          vNamesRAL.Add(AInstaller.Packages.Compression.Compression[vInt1].Name);

          // pegando o packqge
          if AInstaller.Packages.Compression.Compression[vInt1].Install.LPK <> '' then
          begin
            vFile := IncludeTrailingPathDelimiter(APathDownload) +
                     AInstaller.Packages.Folder;
            vFile := IncludeTrailingPathDelimiter(vFile) +
                     AInstaller.Packages.Compression.Compression[vInt1].Install.LPK;
            vPacksRAL.Add(vFile);
          end;

          // pegando as dependencias
          for vInt2 := 0 to Pred(AInstaller.Packages.Compression.Compression[vInt1].Depedancy.Count) do begin
            vAux1 := AInstaller.Packages.Compression.Compression[vInt1].Depedancy.Strings[vInt2];
            if vLst.IndexOf(vAux1) < 0 then
              vLst.Add(vAux1);
          end;
        end;
      end;

      // pegando a dependencia da lista e adicionando e criando uma lista do package
      for vInt1 := 0 to Pred(vLst.Count) do
      begin
        vDepedancy := AInstaller.getDepedancy(vLst.Strings[vInt1]);
        if vDepedancy <> nil then
          vDepedancy.Selected := True;
      end;
    finally
      FreeAndNil(vLst);
    end;

    // pegando as pedencias selecionadas e adicionando na lista de package
    for vInt1 := 0 to Pred(AInstaller.Depedancies.Count) do
    begin
      if AInstaller.Depedancies.Depedancy[vInt1].Selected then begin
        vDepedancy := AInstaller.Depedancies.Depedancy[vInt1];
        vNames.Add(vDepedancy.Name);

        vFile := '';
        if vDepedancy.Install.LPK <> '' then
        begin
          vAux1 := ExcludeTrailingPathDelimiter(ExtractFilePath(FBuildFile));
          if vDepedancy.Folder <> '' then
          begin
            vFile := IncludeTrailingPathDelimiter(APathDownload) + vDepedancy.Folder;
            vFile := IncludeTrailingPathDelimiter(vFile) + vDepedancy.Install.LPK;
          end
          else begin
            vFile := vDepedancy.Install.LPK;
          end;
          vFile := StringReplace(vFile,'%lazpath%', vAux1, [rfReplaceAll]);
          if vDepedancy.Install.LPKIsLink then
            vPacksLink.Add(vFile)
          else
            vPacks.Add(vFile)
        end;
      end;
    end;

    // instalando as dependencias
    for vInt1 := 0 to Pred(vNames.Count) do
      logar(ALog, 'Instando dependência: ' + vNames.Strings[vInt1]);

    // instalando as dependencias de link
    LazarusInstallPackages(vPacksLink, True);
    // instalando as dependencias
    LazarusInstallPackages(vPacks);

    // instalando os packages
    for vInt1 := 0 to Pred(vNames.Count) do
      logar(ALog, 'Instando pacotes: ' + vNamesRAL.Strings[vInt1]);

    LazarusInstallPackages(vPacksRAL);
  finally
    FreeAndNil(vPacks);
    FreeAndNil(vNames);
    FreeAndNil(vPacksLink);
    FreeAndNil(vPacksRAL);
    FreeAndNil(vNamesRAL);
  end;
end;

constructor Tfrm_ide_version.Create(AOwner: TComponent);
begin
  inherited;
  FIsDelphi := False;
end;

procedure Tfrm_ide_version.installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload : string);
begin
  if not ckSelecionado.Checked then
    Exit;

  logar(ALog, 'Iniciando instalação em: ' + IDEName);

  if not FIsDelphi then
    InstallLazarus(ALog, AInstaller, APathDownload);

  logar(ALog, 'Finalizado instalação em: ' + IDEName);
  logar(ALog, '');
end;

end.

