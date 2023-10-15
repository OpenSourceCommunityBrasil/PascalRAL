unit RALCripto;

interface

uses
  Classes, SysUtils,
  RALTypes, RALStream;

type

  { TRALCriptoOptions }

  TRALCriptoOptions = class(TPersistent)
  private
    FCriptType : TRALCriptoType;
    FKey : StringRAL;
  public
    constructor Create;
  published
    property Key : StringRAL read FKey write FKey;
    property CriptType : TRALCriptoType read FCriptType write FCriptType;
  end;

  { TRALCripto }

  TRALCripto = class
  private
    FKey : StringRAL;
  protected
    procedure SetKey(AValue : StringRAL); virtual;
  public
    function Encode(AValue: StringRAL): StringRAL; overload;
    function Decode(AValue: StringRAL): StringRAL; overload;

    function Encode(AValue: TStream): StringRAL; overload;
    function Decode(AValue: TStream): StringRAL; overload;

    function Encode(AValue: TBytes): StringRAL; overload;
    function Decode(AValue: TBytes): StringRAL; overload;

    function EncodeAsBytes(AValue: StringRAL): TBytes; overload;
    function DecodeAsBytes(AValue: StringRAL): TBytes; overload;

    function EncodeAsBytes(AValue: TStream): TBytes; overload;
    function DecodeAsBytes(AValue: TStream): TBytes; overload;

    function EncodeAsBytes(AValue: TBytes): TBytes; overload;
    function DecodeAsBytes(AValue: TBytes): TBytes; overload;

    function EncodeAsStream(AValue: TStream): TStream; overload; virtual; abstract;
    function DecodeAsStream(AValue: TStream): TStream; overload; virtual; abstract;
  published
    property Key : StringRAL read FKey write SetKey;
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

procedure TRALCripto.SetKey(AValue : StringRAL);
begin
  if FKey = AValue then
    Exit;

  FKey :=AValue;
end;

function TRALCripto.Encode(AValue : StringRAL) : StringRAL;
var
  vStream: TStream;
begin
  vStream := StringToStream(AValue);
  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decode(AValue : StringRAL) : StringRAL;
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Encode(AValue : TStream) : StringRAL;
var
  vResult: TStream;
begin
  vResult := EncodeAsStream(AValue);
  try
    Result := StreamToString(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

function TRALCripto.Decode(AValue : TStream) : StringRAL;
var
  vResult: TStringStream;
begin
  vResult := TStringStream(DecodeAsStream(AValue));
  try
    Result := vResult.DataString;
  finally
    FreeAndNil(vResult);
  end;
end;

function TRALCripto.Encode(AValue : TBytes) : StringRAL;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decode(AValue : TBytes) : StringRAL;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.EncodeAsBytes(AValue : StringRAL) : TBytes;
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := EncodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.DecodeAsBytes(AValue : StringRAL) : TBytes;
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := DecodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.EncodeAsBytes(AValue : TStream) : TBytes;
var
  vResult: TStream;
begin
  vResult := EncodeAsStream(AValue);
  try
    Result := StreamToBytes(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

function TRALCripto.DecodeAsBytes(AValue : TStream) : TBytes;
var
  vResult: TStream;
begin
  vResult := DecodeAsStream(AValue);
  try
    Result := StreamToBytes(vResult);
  finally
    FreeAndNil(vResult);
  end;
end;

function TRALCripto.EncodeAsBytes(AValue : TBytes) : TBytes;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := EncodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.DecodeAsBytes(AValue : TBytes) : TBytes;
var
  vStream: TStream;
begin
  vStream := BytesToStream(AValue);
  try
    Result := DecodeAsBytes(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

end.

