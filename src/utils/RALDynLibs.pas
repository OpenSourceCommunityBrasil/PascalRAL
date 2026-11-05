unit RALDynLibs;

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

interface

uses
  SysUtils,
  RALTypes;

type

  IRALDynLibs = interface
    ['{6B44033D-FDA5-4C45-9A95-34E5CE7EF32E}']
    function GetFilename: string;
    function GetHandle: NativeUInt;
    function TryBind(
      var Target;
      const Symbol: AnsiString
    ): Boolean;
    procedure Bind(
      var Target;
      const Symbol: AnsiString
    );
    property Filename: string read GetFilename;
    property Handle: NativeUInt read GetHandle;
  end;

  { Owns the library handle. Bound procedures are valid until this object is freed. }
  TRALDynLibs = class(TInterfacedObject, IRALDynLibs)
  private
    FHandle: NativeUInt;
    FFilename: string;
    function GetFilename: string;
    function GetHandle: NativeUInt;
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
    function TryBind(
      var Target;
      const Symbol: AnsiString
    ): Boolean;
    procedure Bind(
      var Target;
      const Symbol: AnsiString
    );
    property Filename: string read FFilename;
    property Handle: NativeUInt read FHandle;
  end;

implementation

uses
  {$IFDEF RALWindows}
  Windows;
  {$ELSE}
  Dynlibs;
  {$ENDIF}

constructor TRALDynLibs.Create(const Filename: string);
{$IFDEF RALWindows}
var
  Count: Cardinal;
{$ENDIF}
begin
  inherited Create;
  {$IFDEF RALWindows}
  FHandle := LoadLibrary(PChar(Filename));
  {$ELSE}
  FHandle := LoadLibrary(Filename);
  {$ENDIF}
  if FHandle = 0 then
    raise ERALException.Create('Cannot load library: ' + Filename + '. Check architecture and dependencies.');
  {$IFDEF RALWindows}
  SetLength(FFilename, 32768);
  Count := GetModuleFileName(FHandle, PChar(FFilename), Length(FFilename));
  if (Count = 0) or (Count >= Cardinal(Length(FFilename))) then
    raise ERALException.Create('Cannot resolve loaded library filename: ' + Filename);
  SetLength(FFilename, Count);
  {$ELSE}
  FFilename := Filename;
  {$ENDIF}
end;

destructor TRALDynLibs.Destroy;
begin
  if FHandle <> 0 then
  begin
    {$IFDEF RALWindows}
    FreeLibrary(FHandle);
    {$ELSE}
    UnloadLibrary(FHandle);
    {$ENDIF}
  end;
  inherited;
end;

function TRALDynLibs.GetFilename: string;
begin
  Result := FFilename;
end;

function TRALDynLibs.GetHandle: NativeUInt;
begin
  Result := FHandle;
end;

procedure TRALDynLibs.Bind(
  var Target;
  const Symbol: AnsiString
);
begin
  if not TryBind(Target, Symbol) then
    raise ERALException.Create('Library symbol unavailable: ' + string(Symbol));
end;

function TRALDynLibs.TryBind(
  var Target;
  const Symbol: AnsiString
): Boolean;
var
  Address: Pointer;
begin
  {$IFDEF RALWindows}
  Address := GetProcAddress(FHandle, PAnsiChar(Symbol));
  {$ELSE}
  Address := GetProcedureAddress(FHandle, Symbol);
  {$ENDIF}
  PPointer(@Target)^ := Address;
  Result := Address <> nil;
end;

end.
