unit RALHashes;

interface

uses
  Classes, SysUtils,
  RALBase64, RALTypes, RALTools;

type
  TRALHashOutputType = (rhotHex,rhotBase64);

  TRALHashes = class
  private
    FOutputType : TRALHashOutputType;
    FLenBit : UInt64;
    FIndex : IntegerRAL;
  protected
    function GetBufLength : IntegerRAL; virtual; abstract;
    function GetBuffer(AIndex : IntegerRAL) : Pointer; virtual; abstract;
    function GetIndex : integer;
    function GetLenBit : UInt64;

    procedure Initialize; virtual;
    procedure Compress; virtual;
    function Finalize : TBytes; virtual; abstract;

    procedure HashBytes(AData : PByte; ALength : IntegerRAL); virtual;

    function DigestToHex(AValue : TBytes) : StringRAL;
    function DigestToBase64(AValue : TBytes) : StringRAL;

    procedure UpdateBuffer(AValue: TStream); overload; virtual;
    procedure UpdateBuffer(AValue: StringRAL); overload; virtual;
    procedure UpdateBuffer(AValue: TBytes); overload; virtual;

    function GetDigest(AValue: TStream) : TBytes; overload; virtual;
    function GetDigest(AValue: StringRAL) : TBytes; overload; virtual;
    function GetDigest(AValue: TBytes) : TBytes; overload; virtual;

    function HMACAsDigest(AValue: TStream; AKey: TBytes): TBytes; virtual;
  public
    constructor Create;

    function HashAsString(AValue: StringRAL): StringRAL; overload; virtual;
    function HashAsString(AValue: TStream): StringRAL; overload; virtual;

    function HashAsStream(AValue: TStream): TStringStream;

    function HMACAsString(AValue, AKey: StringRAL): StringRAL; overload; virtual;
    function HMACAsString(AValue : TStream; AKey: StringRAL): StringRAL; overload; virtual;
    function HMACAsString(AValue : TBytes; AKey: StringRAL): StringRAL; overload; virtual;
  published
    property OutputType : TRALHashOutputType read FOutputType write FOutputType;
  end;

implementation

{ TRALHashes }

procedure TRALHashes.Compress;
begin
  FIndex := 0;
end;

constructor TRALHashes.Create;
begin
  FOutputType := rhotHex;
end;

function TRALHashes.DigestToBase64(AValue: TBytes): StringRAL;
begin
  Result := TRALBase64.Encode(AValue);
  while (Result <> '') and (Result[Length(Result)] = '=') do
    Delete(Result,Length(Result),1);
end;

function TRALHashes.DigestToHex(AValue: TBytes): StringRAL;
const
  HexChar : array[0..15] of CharRAL = ('0','1','2','3','4','5','6','7','8','9',
                                       'a','b','c','d','e','f');
var
  vInt : IntegerRAL;
begin
  vInt := 0;
  while vInt < Length(AValue) do
  begin
    Result := Result + HexChar[(AValue[vInt] shr 4) and $0f];
    Result := Result + HexChar[(AValue[vInt] and $0f)];
    Inc(vInt);
  end;
end;

function TRALHashes.GetDigest(AValue: TStream): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALHashes.GetDigest(AValue: StringRAL): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALHashes.GetDigest(AValue: TBytes): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALHashes.GetIndex: integer;
begin
  Result := FIndex;
end;

function TRALHashes.GetLenBit: UInt64;
begin
  Result := FLenBit;
end;

function TRALHashes.HashAsStream(AValue: TStream): TStringStream;
var
  vDigest : TBytes;
  vResult : StringRAL;
begin
  Initialize;
  UpdateBuffer(AValue);
  vDigest := Finalize;

  case OutputType of
    rhotHex    : vResult := DigestToHex(vDigest);
    rhotBase64 : vResult := DigestToBase64(vDigest);
  end;

  Result := TStringStream.Create(vResult);
  Result.Position := 0;
end;

function TRALHashes.HashAsString(AValue: TStream): StringRAL;
var
  vResult : TStringStream;
begin
  vResult := HashAsStream(AValue);
  try
    Result := vResult.DataString
  finally
    vResult.Free;
  end;
end;

function TRALHashes.HashAsString(AValue: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := HashAsString(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALHashes.HashBytes(AData: PByte; ALength: IntegerRAL);
var
  vBufSize : Integer;
  vBufLength : Integer;
  vBuffer : Pointer;
begin
  vBufLength := GetBufLength;
  Inc(FLenBit,ALength*8);

  while ALength > 0 do
  begin
    vBufSize := vBufLength - FIndex;
    if ALength < vBufSize then
      vBufSize := ALength;

    vBuffer := GetBuffer(FIndex);
    Move(AData^,vBuffer^,vBufSize);
    Inc(AData,vBufSize);

    if vBufSize + FIndex = vBufLength then
      Compress
    else
      FIndex := vBufSize;

    Dec(ALength,vBufSize);
  end;
end;

function TRALHashes.HMACAsDigest(AValue: TStream; AKey: TBytes): TBytes;
var
  vKey : TBytes;
  vTemp1 : TBytes;
  vTemp2 : TBytes;
  vInt : integer;
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

function TRALHashes.HMACAsString(AValue, AKey: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := HMACAsString(vStream,AKey);
  finally
    vStream.Free;
  end;
end;

function TRALHashes.HMACAsString(AValue: TStream; AKey: StringRAL): StringRAL;
var
  vKey, vDigest : TBytes;
begin
  vKey := VarToBytes(AKey);
  vDigest := HMACAsDigest(AValue,vKey);

  case OutputType of
    rhotHex    : Result := DigestToHex(vDigest);
    rhotBase64 : Result := DigestToBase64(vDigest);
  end;
end;

function TRALHashes.HMACAsString(AValue: TBytes; AKey: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := HMACAsString(vStream,AKey);
  finally
    vStream.Free;
  end;
end;

procedure TRALHashes.Initialize;
begin
  FIndex := 0;
  FLenBit := 0;
end;

procedure TRALHashes.UpdateBuffer(AValue: TStream);
var
  vInBuf: array[0..4095] of Byte;
  vBytesRead : IntegerRAL;
  vPosition, vSize : Int64RAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    HashBytes(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;
end;

procedure TRALHashes.UpdateBuffer(AValue: StringRAL);
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    UpdateBuffer(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALHashes.UpdateBuffer(AValue: TBytes);
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    UpdateBuffer(vStream);
  finally
    vStream.Free;
  end;
end;

end.
