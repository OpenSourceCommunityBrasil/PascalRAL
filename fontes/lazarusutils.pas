unit lazarusutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, utools, FileInfo, Dialogs, LazFileUtils;

type
  TOnLazarusFind = procedure(APath: String; var ACancel : boolean) of object;

  { TLazarusObjectData }

  TLazarusObjectData = class
  private
    FName: string;
    FPath: string;
    FVersion: string;
    FExeFile: string;
    FIsLazarus: boolean;
  protected
    procedure SetExeFile(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    property ExeFile: string read FExeFile write SetExeFile;
    property Path: string read FPath write FPath;
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property IsLazarus: boolean read FIsLazarus write FIsLazarus;
  end;

  { TLazarusObjectList }

  TLazarusObjectList = class(TList)
  public
    procedure Clear; override;
  end;

  { TLazarusFinder }

  TLazarusFinder = class
  private
    FList : TLazarusObjectList;
  protected
    procedure ListaDiretorios(APasta: string; AOnFind: TOnLazarusFind);
    procedure ListaArquivos(APasta: string);
  public
    function BuscaLazarus(AOnFind :TOnLazarusFind) : TLazarusObjectList;
  end;

const
  {$IF Defined(Windows)}
    LazBuildFile = 'lazbuild.exe';
    LazExecFile = 'lazarus.exe';
  {$ELSEIF Defined(Linux)}
    LazBuildFile = 'lazbuild';
    LazExecFile = 'lazarus';
  {$IFEND}

function ListInstalledLazarusVersions(AOnFind :TOnLazarusFind): TLazarusObjectList;

implementation

function ListInstalledLazarusVersions(AOnFind :TOnLazarusFind) : TLazarusObjectList;
var
  vFinder: TLazarusFinder;
begin
  vFinder := TLazarusFinder.Create;
  try
    Result := vFinder.BuscaLazarus(AOnFind);
  finally
    FreeAndNil(vFinder);
  end;
end;

{ TLazarusFinder }

procedure TLazarusFinder.ListaDiretorios(APasta: string; AOnFind :TOnLazarusFind);
var
  vSearch : TSearchRec;
  vRet: integer;
  vCancel: boolean;
begin
  APasta := IncludeTrailingPathDelimiter(APasta);
  vCancel := False;
  if AOnFind <> nil then
    AOnFind(APasta, vCancel);

  if vCancel then
    Exit;

  ListaArquivos(APasta);

  vRet := FindFirst(APasta + '*', faDirectory, vSearch);
  try
    while vRet = 0 do
    begin
       if (vSearch.Attr and faDirectory > 0) and (not FileIsSymlink(APasta + vSearch.Name)) and
          (vSearch.Name <> '.') and (vSearch.Name <> '..') and (vSearch.Name <> '') then
        ListaDiretorios(APasta + vSearch.Name, AOnFind);

      vRet := FindNext(vSearch);
    end;
  finally
    FindClose(vSearch);
  end;
end;

procedure TLazarusFinder.ListaArquivos(APasta: string);
var
  vObj: TLazarusObjectData;
begin
  APasta := IncludeTrailingPathDelimiter(APasta);
  if FileExists(APasta + LazBuildFile) and
     FileExists(APasta + LazExecFile) then
  begin
    vObj := TLazarusObjectData.Create;
    vObj.ExeFile := APasta + LazExecFile;
    if vObj.IsLazarus then
      FList.Add(vObj)
    else
      FreeAndNil(vObj);
  end;
end;

function TLazarusFinder.BuscaLazarus(AOnFind :TOnLazarusFind) : TLazarusObjectList;
{$IFDEF MSWINDOWS}
  var
    vUnid: char;
{$ENDIF}
begin
  FList := TLazarusObjectList.Create;
  {$IFDEF MSWINDOWS}
    for vUnid := 'A' to 'Z' do
      ListaDiretorios(vUnid + ':\', AOnFind);
  {$ELSE}
    ListaDiretorios('/', AOnFind);
  {$ENDIF}
  Result := FList;
end;

{ TLazarusObjectData }

procedure TLazarusObjectData.SetExeFile(AValue: string);
var
  vFileVerInfo: TFileVersionInfo;
begin
  if FExeFile = AValue then
    Exit;

  FExeFile := AValue;
  FIsLazarus := False;

  vFileVerInfo := TFileVersionInfo.Create(nil);
  try
    vFileVerInfo.FileName := AValue;
    try
      vFileVerInfo.ReadFileInfo;

      FName := vFileVerInfo.VersionStrings.Values['ProductName'] + ' ' +
               vFileVerInfo.VersionStrings.Values['FileVersion'];

      FPath := ExtractFilePath(AValue);
      FVersion := vFileVerInfo.VersionStrings.Values['FileVersion'];
      FIsLazarus := True;
    except
      raise;
    end;
  finally
    vFileVerInfo.Free;
  end;
end;

constructor TLazarusObjectData.Create;
begin

end;

destructor TLazarusObjectData.Destroy;
begin
  inherited Destroy;
end;

{ TLazarusObjectList }

procedure TLazarusObjectList.Clear;
begin
  while Self.Count > 0 do begin
    TObject(Self.Items[Self.Count - 1]).Free;
    Self.Delete(Self.Count - 1);
  end;
  inherited Clear;
end;

end.
