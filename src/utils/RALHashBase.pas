/// Unit for the unidirectional hash algorithms
unit RALHashBase;

interface

uses
  Classes, SysUtils,
  RALBase64, RALTypes, RALTools, RALStream, RALConsts;

type
  TRALHashOutputType = (rhotNone, rhotHex, rhotBase64, rhotBase64Url);

  { TRALHashBase }

  TRALHashBase = class
  private
    FFinalized: boolean;
    FIndex: IntegerRAL;
    FInitialized: boolean;
    FLenBit: UInt64RAL;
    FOutputType: TRALHashOutputType;
  protected
    /// Used to compress the content, generating the hash
    procedure Compress; virtual;
    /// Convert hash (return Finalize) to base64
    function DigestToBase64(AValue: TBytes): StringRAL;
    /// Convert hash (return Finalize) to base64-url
    function DigestToBase64Url(AValue: TBytes): StringRAL;
    /// Convert hash (return Finalize) to hexadecimal
    function DigestToHex(AValue: TBytes): StringRAL;
    /// Finalize the hash and returns it
    function Finalize: TBytes; virtual;
    /// Returns the buffer pointer at a given position
    function GetBuffer(AIndex: IntegerRAL): Pointer; virtual; abstract;
    /// Returns the length of the buffer
    function GetBufLength: IntegerRAL; virtual; abstract;
    /// Generates a hash of a given piece of Stream content
    function GetDigest(AValue: TStream): TBytes; overload; virtual;
    /// Generates a hash of a given piece of UTF8String content
    function GetDigest(const AValue: StringRAL): TBytes; overload; virtual;
    /// Generates a hash of a given piece of Array of Bytes content
    function GetDigest(AValue: TBytes): TBytes; overload; virtual;
    /// Returns the position of the buffer
    function GetIndex: IntegerRAL;
    /// Returns the total buffer in bits
    function GetLenBit: UInt64RAL;
    /// Picks up incoming and compact content according to buffer length
    procedure HashBytes(AData: pbyte; ALength: IntegerRAL); virtual;
    /// Generates an HMAC hash of a given content
    function HMACAsDigest(AValue: TStream; AKey: TBytes): TBytes; virtual;
    /// Used to initialize the hash
    procedure Initialize; virtual;
    /// Used to insert more content that will generate the hash
    procedure UpdateBuffer(AValue: TStream); overload; virtual;
    /// Used to insert more content that will generate the hash
    procedure UpdateBuffer(const AValue: StringRAL); overload; virtual;
    /// Used to insert more content that will generate the hash
    procedure UpdateBuffer(AValue: TBytes); overload; virtual;
  public
    constructor Create(AOutputType: TRALHashOutputType = rhotHex);
    /// Returns a string hash from a string
    function HashAsString(const AValue: StringRAL): StringRAL; overload; virtual;
    /// Returns a string hash from a stream
    function HashAsString(AValue: TStream): StringRAL; overload; virtual;
    /// Returns a stream hash from a stream
    function HashAsStream(AValue: TStream): TStream;
    /// Returns a string of a HMAC generated given an UTF8String
    function HMACAsString(const AValue: StringRAL; const AKey: StringRAL): StringRAL; overload; virtual;
    /// Returns a string of a HMAC generated given a Stream
    function HMACAsString(AValue: TStream; const AKey: StringRAL): StringRAL; overload; virtual;
    /// Returns a string of a HMAC generated given an Array of bytes
    function HMACAsString(AValue: TBytes; const AKey: StringRAL): StringRAL; overload; virtual;
  published
    /// Identifies the formatting of the hash
    property OutputType: TRALHashOutputType read FOutputType write FOutputType;
  end;

implementation

{ TRALHashBase }

procedure TRALHashBase.Compress;
begin
  FIndex := 0;
end;

constructor TRALHashBase.Create(AOutputType: TRALHashOutputType);
begin
  FOutputType := AOutputType;
end;

function TRALHashBase.DigestToBase64(AValue: TBytes): StringRAL;
begin
  Result := TRALBase64.Encode(AValue);
end;

function TRALHashBase.DigestToBase64Url(AValue: TBytes): StringRAL;
begin
  Result := TRALBase64.Encode(AValue);
  Result := TRALBase64.ToBase64Url(Result);
end;

function TRALHashBase.DigestToHex(AValue: TBytes): StringRAL;
const
  HexChar: array[0..15] of CharRAL = ('0', '1', '2', '3', '4', '5', '6', '7',
                                      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  vInt: IntegerRAL;
begin
  vInt := 0;
  while vInt < Length(AValue) do
  begin
    Result := Result + HexChar[(AValue[vInt] shr 4) and $0f];
    Result := Result + HexChar[(AValue[vInt] and $0f)];
    Inc(vInt);
  end;
end;

function TRALHashBase.Finalize: TBytes;
begin
  FFinalized := True;
end;

function TRALHashBase.GetDigest(AValue: TStream): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALHashBase.GetDigest(const AValue: StringRAL): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALHashBase.GetDigest(AValue: TBytes): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALHashBase.GetIndex: IntegerRAL;
begin
  Result := FIndex;
end;

function TRALHashBase.GetLenBit: UInt64RAL;
begin
  Result := FLenBit;
end;

function TRALHashBase.HashAsStream(AValue: TStream): TStream;
var
  vDigest : TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  vDigest := Finalize;
  Result := nil;

  case OutputType of
    rhotNone: Result := BytesToStream(vDigest);
    rhotHex: Result := StringToStream(DigestToHex(vDigest));
    rhotBase64: Result := StringToStream(DigestToBase64(vDigest));
    rhotBase64Url: Result := StringToStream(DigestToBase64Url(vDigest));
  end;
end;

function TRALHashBase.HashAsString(AValue: TStream): StringRAL;
var
  vResult: TStream;
begin
  vResult := HashAsStream(AValue);
  try
    Result := StreamToString(vResult);
  finally
    vResult.Free;
  end;
end;

function TRALHashBase.HashAsString(const AValue: StringRAL): StringRAL;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := HashAsString(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALHashBase.HashBytes(AData: pbyte; ALength: IntegerRAL);
var
  vBufSize: integer;
  vBufLength: integer;
  vBuffer: Pointer;
begin
  vBufLength := GetBufLength;
  Inc(FLenBit, ALength * 8);

  while ALength > 0 do
  begin
    vBufSize := vBufLength - FIndex;
    if ALength < vBufSize then
      vBufSize := ALength;

    vBuffer := GetBuffer(FIndex);
    Move(AData^, vBuffer^, vBufSize);
    Inc(AData, vBufSize);

    if vBufSize + FIndex = vBufLength then
      Compress
    else
      FIndex := vBufSize;

    Dec(ALength, vBufSize);
  end;
end;

function TRALHashBase.HMACAsDigest(AValue: TStream; AKey: TBytes): TBytes;
var
  vKey: TBytes;
  vTemp1: TBytes;
  vTemp2: TBytes;
  vInt: integer;
begin
  if Length(AKey) > GetBufLength then
    vKey := GetDigest(AKey)
  else
    vKey := AKey;

  SetLength(vKey, GetBufLength);
  SetLength(vTemp1, GetBufLength);
  for vInt := Low(vKey) to High(vKey) do
    vTemp1[vInt] := vKey[vInt] xor $36;

  Initialize;
  UpdateBuffer(vTemp1);
  UpdateBuffer(AValue);
  vTemp2 := Finalize;

  SetLength(vTemp1, GetBufLength);
  for vInt := Low(vKey) to High(vKey) do
    vTemp1[vInt] := vKey[vInt] xor $5C;

  Initialize;
  UpdateBuffer(vTemp1);
  UpdateBuffer(vTemp2);
  Result := Finalize;
end;

function TRALHashBase.HMACAsString(const AValue: StringRAL; const AKey: StringRAL
  ): StringRAL;
var
  vStream: TStream;
begin
  if AValue = '' then
    Raise Exception.Create(emHMACEmptyText)
  else if AKey = '' then
    Raise Exception.Create(emCryptEmptyKey);

  vStream := StringToStreamUTF8(AValue);
  try
    vStream.Position := 0;
    Result := HMACAsString(vStream, AKey);
  finally
    vStream.Free;
  end;
end;

function TRALHashBase.HMACAsString(AValue: TStream; const AKey: StringRAL): StringRAL;
var
  vKey, vDigest: TBytes;
begin
  vKey := StringToBytesUTF8(AKey);
//  SetLength(vKey, Length(AKey));
//  Move(AKey[PosIniStr], vKey[0], Length(AKey));

  vDigest := HMACAsDigest(AValue, vKey);

  case OutputType of
    rhotNone: begin
      SetLength(Result, Length(vDigest));
      Move(vDigest[0], Result[POSINISTR], Length(vDigest));
    end;
    rhotHex: Result := DigestToHex(vDigest);
    rhotBase64: Result := DigestToBase64(vDigest);
    rhotBase64Url: Result := DigestToBase64Url(vDigest);
  end;
end;

function TRALHashBase.HMACAsString(AValue: TBytes; const AKey: StringRAL): StringRAL;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    vStream.Position := 0;
    Result := HMACAsString(vStream, AKey);
  finally
    vStream.Free;
  end;
end;

procedure TRALHashBase.Initialize;
begin
  FIndex := 0;
  FLenBit := 0;
  FInitialized := True;
  FFinalized := False;
end;

procedure TRALHashBase.UpdateBuffer(AValue: TStream);
var
  vInBuf: array of byte;
  vBytesRead: IntegerRAL;
  vPosition, vSize: Int64RAL;
begin
  if (not FInitialized) or (FFinalized) then
    Exit;

  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  if vSize > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vInBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vInBuf, vSize);

  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    HashBytes(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;
end;

procedure TRALHashBase.UpdateBuffer(const AValue: StringRAL);
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    UpdateBuffer(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALHashBase.UpdateBuffer(AValue: TBytes);
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    UpdateBuffer(vStream);
  finally
    vStream.Free;
  end;
end;

end.
