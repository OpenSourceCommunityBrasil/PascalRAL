/// Unit that stores cryptographic functions used in PascalRAL
unit RALCripto;

interface

uses
  Classes, SysUtils,
  RALExceptions, RALErrorCodes, RALTypes, RALStream, RALBase64, RALConsts, RALHexadecimal;

type

  /// Class for the cryptographic definitions
  TRALCriptoOptions = class(TPersistent)
  private
    FCriptType: TRALCriptoType;
    FKey: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    /// The cipher.
    property CriptType: TRALCriptoType read FCriptType write FCriptType;
    /// The key text both ends share.
    property Key: StringRAL read FKey write FKey;
  end;

  /// How the input or output of a cipher is written: raw, base64 or hexadecimal.
  TRALCriptoInOutType = (cotNone, cotBase64, cotHEX);

  /// Base class for all crypto functions
  TRALCripto = class
  private
    FIntputType : TRALCriptoInOutType;
    FKey: StringRAL;
    FOutputType : TRALCriptoInOutType;
  protected
    /// Decodes AValue by IntputType, decrypts it and encodes the result by OutputType.
    function BeforeDecrypt(AValue: TStream): TStream;
    /// Decodes AValue by IntputType, encrypts it and encodes the result by OutputType.
    function BeforeEncrypt(AValue: TStream): TStream;
    /// Whether the cipher is ready to run.
    function CanCript : boolean; virtual;
    procedure SetKey(const AValue: StringRAL); virtual;
  public
    constructor Create;

    /// Decrypts a string (raw bytes with ABinary, else UTF-8).
    function Decrypt(const AValue: StringRAL; ABinary : boolean = false): StringRAL; overload;
    /// Decrypts a byte array.
    function Decrypt(AValue: TBytes): StringRAL; overload;
    /// Decrypts a stream.
    function Decrypt(AValue: TStream): StringRAL; overload;
    /// Decrypts a string into bytes.
    function DecryptAsBytes(const AValue: StringRAL): TBytes; overload;
    /// Decrypts a byte array into bytes.
    function DecryptAsBytes(AValue: TBytes): TBytes; overload;
    /// Decrypts a stream into bytes.
    function DecryptAsBytes(AValue: TStream): TBytes; overload;
    /// Decrypts AValue into a new stream, owned by the caller.
    function DecryptAsStream(AValue: TStream): TStream; virtual; abstract;
    /// Encrypts a string (raw bytes with ABinary, else UTF-8).
    function Encrypt(const AValue: StringRAL; ABinary : boolean = false): StringRAL; overload;
    /// Encrypts a byte array.
    function Encrypt(AValue: TBytes): StringRAL; overload;
    /// Encrypts a stream.
    function Encrypt(AValue: TStream): StringRAL; overload;
    /// Encrypts a string into bytes.
    function EncryptAsBytes(const AValue: StringRAL): TBytes; overload;
    /// Encrypts a byte array into bytes.
    function EncryptAsBytes(AValue: TBytes): TBytes; overload;
    /// Encrypts a stream into bytes.
    function EncryptAsBytes(AValue: TStream): TBytes; overload;
    /// Encrypts AValue into a new stream, owned by the caller.
    function EncryptAsStream(AValue: TStream): TStream; virtual; abstract;
  published
    /// How the input is written.
    property IntputType: TRALCriptoInOutType read FIntputType write FIntputType;
    /// The key text.
    property Key: StringRAL read FKey write SetKey;
    /// How the output is written.
    property OutputType: TRALCriptoInOutType read FOutputType write FOutputType;
  end;

implementation

uses
  RALTools;

{ TRALCriptoOptions }

procedure TRALCriptoOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALCriptoOptions then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

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
    raise ERALException.Create(ecHMACEmptyText);

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
  { TODO -cCompatibility : make this work on older Delphi versions }
  vStream := nil;
  try
    vStream := BeforeEncrypt(AValue);
    vStream.Position := 0;

    Result := StreamToByteString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.BeforeDecrypt(AValue: TStream): TStream;
var
  vStreamInput, vStreamEnc: TStream;
begin
  if not CanCript then
    Exit;

  case FIntputType of
    cotNone: begin
      Result := DecryptAsStream(AValue);
    end;
    cotBase64: begin
      vStreamInput := TRALBase64.DecodeAsStream(AValue);
      try
        Result := DecryptAsStream(vStreamInput);
      finally
        FreeAndNil(vStreamInput);
      end;
    end;
    cotHEX: begin
      vStreamInput := TRALHexadecimal.DecodeAsStream(AValue);
      try
        Result := DecryptAsStream(vStreamInput);
      finally
        FreeAndNil(vStreamInput);
      end;
    end;
  end;

  Result.Position := 0;

  case FOutputType of
    cotBase64: begin
      vStreamEnc := TRALBase64.EncodeAsStream(Result);
      FreeAndNil(Result);
      Result := vStreamEnc;
    end;
    cotHEX: begin
      vStreamEnc := TRALHexadecimal.EncodeAsStream(Result);
      FreeAndNil(Result);
      Result := vStreamEnc;
    end;
  end;
end;

function TRALCripto.BeforeEncrypt(AValue: TStream): TStream;
var
  vStreamInput, vStreamEnc : TStream;
begin
  if not CanCript then
    Exit;

  case FIntputType of
    cotNone : begin
      Result := EncryptAsStream(AValue);
    end;
    cotBase64: begin
      vStreamInput := TRALBase64.DecodeAsStream(AValue);
      try
        Result := EncryptAsStream(vStreamInput);
      finally
        FreeAndNil(vStreamInput);
      end;
    end;
    cotHEX: begin
      vStreamInput := TRALHexadecimal.DecodeAsStream(AValue);
      try
        Result := EncryptAsStream(vStreamInput);
      finally
        FreeAndNil(vStreamInput);
      end;
    end;
  end;

  Result.Position := 0;

  case FOutputType of
    cotBase64: begin
      vStreamEnc := TRALBase64.EncodeAsStream(Result);
      FreeAndNil(Result);
      Result := vStreamEnc;
    end;
    cotHEX: begin
      vStreamEnc := TRALHexadecimal.EncodeAsStream(Result);
      FreeAndNil(Result);
      Result := vStreamEnc;
    end;
  end;
end;

function TRALCripto.CanCript: boolean;
begin
  Result := True;
end;

constructor TRALCripto.Create;
begin
  FOutputType := cotNone;
end;

function TRALCripto.Decrypt(AValue: TStream): StringRAL;
var
  vStream: TStream;
begin
  { TODO -cCompatibility : make this work on older Delphi versions }
  vStream := nil;
  try
    vStream := BeforeDecrypt(AValue);
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
  vResult := BeforeEncrypt(AValue);
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
  vResult := BeforeDecrypt(AValue);
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
