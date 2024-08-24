unit lazarusutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FileInfo, StdCtrls,
  ideutils, installparser;

type

  { TLazarusObjectData }

  TLazarusObjectData = class(TIDEObjectData)
  protected
    procedure SetExeFile(AValue: string); override;
    procedure InstallPackages(APackages : TStrings; AIsLink : boolean = false);
    procedure Install(ALog: TMemo; AInstaller: TInstaller; APathDownload: string); override;
  end;

  { TLazarusFinder }

  TLazarusFinder = class(TIDEFinder)
  protected
    procedure ListaDiretorios(APasta: string);
    procedure VerificarInstallPasta(APasta: string);
  public
    procedure BuscarIDE; override;
  end;

const
  {$IFDEF MSWINDOWS}
    LazBuildFile = 'lazbuild.exe';
    LazExecFile = 'lazarus.exe';
  {$ELSE}
    LazBuildFile = 'lazbuild';
    LazExecFile = 'lazarus';
  {$IFEND}

implementation

{ TLazarusObjectData }

procedure TLazarusObjectData.SetExeFile(AValue: string);
var
  vFileVerInfo: TFileVersionInfo;
begin
  inherited;

  vFileVerInfo := TFileVersionInfo.Create(nil);
  try
    vFileVerInfo.FileName := AValue;
    try
      vFileVerInfo.ReadFileInfo;

      Name := vFileVerInfo.VersionStrings.Values['ProductName'] + ' ' +
               vFileVerInfo.VersionStrings.Values['FileVersion'];

//      FPath := ExtractFilePath(AValue);
      Version := vFileVerInfo.VersionStrings.Values['FileVersion'];
    except
      raise;
    end;
  finally
    vFileVerInfo.Free;
  end;
end;

procedure TLazarusObjectData.InstallPackages(APackages: TStrings; AIsLink: boolean);
var
  vInt: Integer;
begin
  ProcessBuild.Executable := BuildFile;
  ProcessBuild.Parameters.Clear;
  ProcessBuild.Parameters.Add('--build-ide=');

  if APackages.Count > 0 then
  begin
    if AIsLink then
      ProcessBuild.Parameters.Add('--add-package-link')
    else
      ProcessBuild.Parameters.Add('--add-package');
  end;

  for vInt := 0 to Pred(APackages.Count) do
    ProcessBuild.Parameters.Add(APackages.Strings[vInt]);

//  APackages.SaveToFile('d:\ral.txt');

  ProcessBuild.Execute;
end;

procedure TLazarusObjectData.Install(ALog: TMemo; AInstaller: TInstaller;
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
          vAux1 := ExcludeTrailingPathDelimiter(ExtractFilePath(BuildFile));
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
    InstallPackages(vPacksLink, True);
    // instalando as dependencias
    InstallPackages(vPacks);

    // instalando os packages
    for vInt1 := 0 to Pred(vNames.Count) do
      logar(ALog, 'Instando pacotes: ' + vNamesRAL.Strings[vInt1]);

    InstallPackages(vPacksRAL);
  finally
    FreeAndNil(vPacks);
    FreeAndNil(vNames);
    FreeAndNil(vPacksLink);
    FreeAndNil(vPacksRAL);
    FreeAndNil(vNamesRAL);
  end;
end;

{ TLazarusFinder }

procedure TLazarusFinder.ListaDiretorios(APasta: string);
var
  vSearch : TSearchRec;
  vRet: integer;
  vCancel: boolean;
begin
  APasta := IncludeTrailingPathDelimiter(APasta);
  vCancel := False;

  if OnIDEFind <> nil then
    OnIDEFind(APasta, vCancel);

  if vCancel then
    Exit;

  VerificarInstallPasta(APasta);

  vRet := FindFirst(APasta + '*', faDirectory, vSearch);
  try
    while vRet = 0 do
    begin
       if (vSearch.Attr and faDirectory > 0) and (not FileIsSymlink(APasta + vSearch.Name)) and
          (vSearch.Name <> '.') and (vSearch.Name <> '..') and (vSearch.Name <> '') then
        ListaDiretorios(APasta + vSearch.Name);

      vRet := FindNext(vSearch);
    end;
  finally
    FindClose(vSearch);
  end;
end;

procedure TLazarusFinder.VerificarInstallPasta(APasta: string);
var
  vObj: TLazarusObjectData;
begin
  APasta := IncludeTrailingPathDelimiter(APasta);
  if FileExists(APasta + LazBuildFile) and
     FileExists(APasta + LazExecFile) then
  begin
    vObj := TLazarusObjectData.Create;
    vObj.ExeFile := APasta + LazExecFile;
    vObj.BuildFile := APasta + LazBuildFile;

    List.Add(vObj);
  end;
end;

procedure TLazarusFinder.BuscarIDE;
{$IFDEF MSWINDOWS}
  var
    vUnid: char;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    for vUnid := 'A' to 'Z' do
      ListaDiretorios(vUnid + ':\');
  {$ELSE}
    ListaDiretorios('/', AOnFind);
  {$ENDIF}
end;

end.

