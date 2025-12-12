/// Unit that stores cryptographic functions used in PascalRAL
unit RALCripto;

interface

uses
  Classes, SysUtils,
  RALTypes, RALStream, RALBase64, RALConsts;

type

  { TRALCriptoOptions }

  /// Class for the cryptographic definitions
  TRALCriptoOptions = class(TPersistent)
  private
    FCriptType: TRALCriptoType;
    FKey: StringRAL;
  public
    constructor Create;
  published
    property CriptType: TRALCriptoType read FCriptType write FCriptType;
    property Key: StringRAL read FKey write FKey;
  end;

  { TRALCripto }

  /// Base class for all crypto functions
  TRALCripto = class
  private
    FKey: StringRAL;
  protected
    procedure SetKey(const AValue: StringRAL); virtual;
  public
    function Decrypt(const AValue: StringRAL; ABinary : boolean = false): StringRAL; overload;
    function Decrypt(AValue: TBytes): StringRAL; overload;
    function Decrypt(AValue: TStream): StringRAL; overload;
    function DecryptAsBytes(const AValue: StringRAL): TBytes; overload;
    function DecryptAsBytes(AValue: TBytes): TBytes; overload;
    function DecryptAsBytes(AValue: TStream): TBytes; overload;
    function DecryptAsStream(AValue: TStream): TStream; virtual; abstract;
    function Encrypt(const AValue: StringRAL; ABinary : boolean = false): StringRAL; overload;
    function Encrypt(AValue: TBytes): StringRAL; overload;
    function Encrypt(AValue: TStream): StringRAL; overload;
    function EncryptAsBytes(const AValue: StringRAL): TBytes; overload;
    function EncryptAsBytes(AValue: TBytes): TBytes; overload;
    function EncryptAsBytes(AValue: TStream): TBytes; overload;
    function EncryptAsStream(AValue: TStream): TStream; virtual; abstract;
  published
    property Key: StringRAL read FKey write SetKey;
  end;

implementation

{ TRALCriptoOptions }

constructor TRALCriptoOptions.Create;
begin
  inherited;
  FKey := '';
  FCriptType := crNone;
end;

{ TRALCripto }

procedure TRALCripto.SetKey(const AValue: StringRAL);
begin
  if FKey = AValue then
    Exit;

  FKey := AValue;
end;

function TRALCripto.Encrypt(const AValue: StringRAL; ABinary : boolean): StringRAL;
var
  vStream: TStream;
begin
  if ABinary then
    vStream := StringToStream(AValue)
  else
    vStream := StringToStreamUTF8(AValue);

  try
    vStream.Position := 0;
    Result := Encrypt(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decrypt(const AValue: StringRAL; ABinary : boolean): StringRAL;
var
  vStream: TStream;
begin
  if AValue = '' then
    raise Exception.Create(emHMACEmptyText);

  if ABinary then
    vStream := StringToStream(AValue)
  else
    vStream := StringToStreamUTF8(AValue);

  try
    vStream.Position := 0;
    Result := Decrypt(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Encrypt(AValue: TStream): StringRAL;
var
  vStream: TStream;
begin
  { TODO -cCompatibilidade : Melhorar esse código pra compatibilizar com versões antigas do Delphi }
  vStream := nil;
  try
    vStream := EncryptAsStream(AValue);
    vStream.Position := 0;

    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decrypt(AValue: TStream): StringRAL;
var
  vStream: TStream;
begin
  { TODO -cCompatibilidade : Melhorar esse código pra compatibilizar com versões antigas do Delphi }
  vStream := nil;
  try
    vStream := DecryptAsStream(AValue);
    vStream.Position := 0;

    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Encrypt(AValue: TBytes): StringRAL;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Encrypt(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decrypt(AValue: TBytes): StringRAL;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Decrypt(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.EncryptAsBytes(const AValue: StringRAL): TBytes;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := EncryptAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.DecryptAsBytes(const AValue: StringRAL): TBytes;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := DecryptAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.EncryptAsBytes(AValue: TStream): TBytes;
var
  vResult: TStream;
begin
  vResult := EncryptAsStream(AValue);
  try
    Result := StreamToBytes(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

function TRALCripto.DecryptAsBytes(AValue: TStream): TBytes;
var
  vResult: TStream;
begin
  vResult := DecryptAsStream(AValue);
  try
    Result := StreamToBytes(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

function TRALCripto.EncryptAsBytes(AValue: TBytes): TBytes;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := EncryptAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.DecryptAsBytes(AValue: TBytes): TBytes;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := DecryptAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

end.
