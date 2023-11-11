unit lazarusutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, process;

type

  { TLazarusObjectData }

  TLazarusObjectData = class
  private
    FName: string;
    FPath: string;
    FVersion: string;
    procedure GetInstallData;
  public
    property Path: string read FPath write FPath;
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    constructor Create;
    destructor Destroy; override;
  end;

  TLazarusObjectList = TList;

  TLazInstaller = class
  private
    FLazarus: TLazarusObjectData;
    //function FindLazarusInstallDir: TIDEObject;
    procedure RebuildIDE;
    procedure Install;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TLazarusFinder }

  TLazarusFinder = class
  private
    function Comparar(str, tipo: ansistring): boolean;
  public
    procedure ListaArquivos(pasta, ext: string; var str: TStringList);
  end;

procedure FillCurrentLazarusVersion(Data: TLazarusObjectData);
procedure FillListLazarusVersions(AList: TList);
function ListInstalledLazarusVersions: TLazarusObjectList;

implementation

{$REGION 'custom functions'}
function GetLazarusLocalFolder: string;
begin
  //Result := Format('%slazarus', [IncludeTrailingPathDelimiter(GetLocalAppDataFolder)]);
  if not DirectoryExists(Result) then
    Result := '';
end;

function GetConfigLazarusValue(const AValue: string): string;
var
  LocalFolder: TFileName;
  FileName: TFileName;
  XmlDoc: olevariant;
  Node: olevariant;
begin
  Result := '';
  LocalFolder := GetLazarusLocalFolder;
  if LocalFolder <> '' then
  begin
    //FileName := Format('%s%s', [IncludeTrailingPathDelimiter(LocalFolder),
      //sLazarusConfigFile]);
    if FileExists(FileName) then
    begin
      //XmlDoc := CreateOleObject('Msxml2.DOMDocument.6.0');
      try
        XmlDoc.Async := False;
        XmlDoc.Load(FileName);
        XmlDoc.SetProperty('SelectionLanguage', 'XPath');

        if (XmlDoc.parseError.errorCode <> 0) then
          raise Exception.CreateFmt('Error in Xml Data %s', [XmlDoc.parseError]);

        Node := XmlDoc.selectSingleNode(AValue);
        //if not VarIsClear(Node) then
          Result := Node.Text;
      finally
        XmlDoc := Unassigned;
      end;
    end;
  end;
end;

function GetLazarusIDEFolder: string;
begin
  Result := GetConfigLazarusValue('//CONFIG/EnvironmentOptions/LazarusDirectory/@Value');
  if Result = '' then
    Result := GetConfigLazarusValue(
      '//CONFIG/EnvironmentOptions/LazarusDirectory/History/Item1/@Value');

end;

function GetLazarusIDEFileName: string;
begin
  //Result := Format('%s%s', [IncludeTrailingPathDelimiter(GetLazarusIDEFolder),
    //sLazarusIDEName]);
end;

function GetLazarusCompilerFileName: string;
begin
  Result := GetConfigLazarusValue('//CONFIG/EnvironmentOptions/CompilerFilename/@Value');
end;

function IsLazarusInstalled: boolean;
begin
  Result := FileExists(GetLazarusIDEFileName);
end;

{$ENDREGION}

procedure FillCurrentLazarusVersion(Data: TLazarusObjectData);
begin

end;

procedure FillListLazarusVersions(AList: TList);
var
  //VersionData: TDelphiVersionData;
  Found: boolean;
  FileName: string;
begin
  Found := IsLazarusInstalled;
  if Found then
  begin
    FileName := GetLazarusIDEFileName;
    {
      ExtractIconFileToImageList(ListView.SmallImages, Filename);
      Item := ListView.Items.Add;
      Item.ImageIndex := ListView.SmallImages.Count - 1;
      Item.Caption := Format('Lazarus %s',[uMisc.GetFileVersion(FileName)]);
      item.SubItems.Add(FileName);
      item.SubItems.Add(IntToStr(Ord(TSupportedIDEs.LazarusIDE)));
      Item.Data := nil;
    }
    //VersionData := TDelphiVersionData.Create;
    //VersionData.Path := FileName;
    // VersionData.Version:=;
    //VersionData.Name := Format('Lazarus %s', [DITE.Misc.GetFileVersion(FileName)]);
    //VersionData.IDEType := TSupportedIDEs.LazarusIDE;
    //VersionData.Icon := TIcon.Create;
    //VersionData.Version := TDelphiVersions.DelphiXE; //used for syntax highlight
    //ExtractIconFile(VersionData.Icon, FileName, SHGFI_SMALLICON);
    //AList.Add(VersionData);
  end;
end;

function ListInstalledLazarusVersions: TLazarusObjectList;
begin

end;

{ TLazarusFinder }

function TLazarusFinder.Comparar(str, tipo: ansistring): boolean;
begin
  Result := (tipo = 'A') and (SameText(ExtractFileName(str), 'environmentoptions.xml'));
end;

procedure TLazarusFinder.ListaArquivos(pasta, ext: string; var str: TStringList);
var
  F: TSearchRec;
  Ret: integer;
  bcomp: boolean;
  recursivo: boolean;
  onlyFolder: boolean;
begin
  recursivo := True;

  pasta := IncludeTrailingPathDelimiter(pasta);

  if ext = '' then
    ext := '*.*';

  onlyFolder := False;
  Ret := FindFirst(pasta + ext, faAnyFile, F);
  if Ret <> 0 then
  begin
    FindClose(F);
    Ret := FindFirst(pasta + '*', faAnyFile, F);
    onlyFolder := True;
  end;

  try
    while Ret = 0 do
    begin
      if (F.Attr and faDirectory > 0) then
      begin
        if (F.Name <> '.') and (F.Name <> '..') and (F.Name <> '') and (recursivo) then
          listaArquivos(pasta + F.Name, ext, str);
      end
      else if (F.Name <> '') and (not onlyFolder) then
      begin
        bcomp := comparar(pasta + F.Name, 'A');

        if bcomp then
          str.Add(pasta + F.Name);
      end;
      Ret := FindNext(F);
    end;
  finally
    FindClose(F);
  end;
end;

{ TLazarusObjectData }

procedure TLazarusObjectData.GetInstallData;
begin

end;

constructor TLazarusObjectData.Create;
begin

end;

destructor TLazarusObjectData.Destroy;
begin
  inherited Destroy;
end;

{ TLazInstaller }

procedure TLazInstaller.RebuildIDE;
begin
   // rebuild IDE
  {
   AProcess := TProcess.Create(nil);
   AProcess.Executable := IncludeTrailingPathDelimiter(edtPathLazarus.Text) + 'lazbuild';
   AProcess.Parameters.Add('--build-ide=');
   AProcess.Options := [poUsePipes, poStdErrToOutput];
   AProcess.ShowWindow := swoHIDE;
   AProcess.Execute;
  }
   //
end;

procedure TLazInstaller.Install;
var
  AProcess: TProcess;
begin
  {
  1- buscar diretório do lazarus
  2- identificar o lazbuild
  3- adicionar os .lpks via linha de comando e anexar ao lazbuild
  4- executar o build dos lpks
  5- executar rebuildide

  3:
  for i := 0 to strListACBr.Count - 1 do
  begin
    AProcess := TProcess.Create(nil);
    try
      AProcess.CommandLine:= concat(fPath,' --add-package-link ', strListACBr.Strings[i]);
      AProcess.Options := [poUsePipes, poStdErrToOutput];
      AProcess.ShowWindow := swoHIDE;
      AProcess.Execute;
    finally
      AProcess.Free;
    end;
  }
end;

constructor TLazInstaller.Create;
begin
  //FindLazarusInstallDir;
end;

destructor TLazInstaller.Destroy;
begin
  inherited Destroy;
end;

end.
