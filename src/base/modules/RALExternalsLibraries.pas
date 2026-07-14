unit RALExternalsLibraries;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  {$IFDEF FPC}
    DynLibs,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  RALTypes;

type
  { TRALExternalsLibraries }

  TRALExternalsLibraries = class
  private
    FLibraries : TStringList;
    FProcs : TList;
    FCritSession : TCriticalSection;

    FLibraryName : StringRAL;
    FLibraryHandle : NativeInt;
  protected
    procedure CloseLibrary;
    procedure LoadLibrary;
    procedure LoadProcs; virtual; abstract;

    procedure LoadProc(var AProc : Pointer; AName: PAnsiChar);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddLibrary(ALibName : StringRAL);
  published
    property LibraryName : StringRAL read FLibraryName;
    property LibraryHandle : NativeInt read FLibraryHandle;
  end;

implementation

{ TRALExternalsLibraries }

procedure TRALExternalsLibraries.AddLibrary(ALibName: StringRAL);
begin
  FLibraries.Add(ALibName);
end;

procedure TRALExternalsLibraries.CloseLibrary;
var
  vInt : integer;
  vPoint : Pointer;
begin
  FCritSession.Acquire;
  try
    for vInt := 0 to Pred(FProcs.Count) do begin
      vPoint := FProcs.Items[vInt];
      Pointer(vPoint^) := nil;
    end;

    if FLibraryHandle <> 0 then
      FreeLibrary(FLibraryHandle);

    FLibraryHandle := 0;
    FLibraryName := '';
  finally
    FCritSession.Release;
  end;
end;

constructor TRALExternalsLibraries.Create;
begin
  inherited;
  FLibraries := TStringList.Create;
  FProcs := TList.Create;
  FCritSession := TCriticalSection.Create;

  FLibraryHandle := 0;
  FLibraryName := '';
end;

procedure TRALExternalsLibraries.LoadProc(var AProc : Pointer; AName: PAnsiChar);
begin
  if FLibraryHandle = 0 then
    raise Exception.Create('Livraria não foi aberta');

  AProc := GetProcAddress(FLibraryHandle, AName);
  FProcs.Add(@AProc);
end;

destructor TRALExternalsLibraries.Destroy;
begin
  CloseLibrary;
  FreeAndNil(FLibraries);
  FreeAndNil(FProcs);
  FreeAndNil(FCritSession);
  inherited;
end;

procedure TRALExternalsLibraries.LoadLibrary;
var
  vInt : integer;
begin
  CloseLibrary;

  FCritSession.Acquire;
  try
    for vInt := 0 to Pred(FLibraries.Count) do begin
      FLibraryHandle := SafeLoadLibrary(FLibraries.Strings[vInt]);
      if FLibraryHandle <> 0 then begin
        FLibraryName := FLibraries.Strings[vInt];
        LoadProcs;
        Break;
      end;
    end;
  finally
    FCritSession.Release;
  end;
end;

end.
