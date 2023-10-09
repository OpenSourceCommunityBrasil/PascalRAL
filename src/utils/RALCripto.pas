unit RALCripto;

interface

uses
  Classes, SysUtils,
  RALTypes;

type
  TRALCriptoOptions = class(TPersistent)
  private
    FKey : StringRAL;
  published
    property Key : StringRAL read FKey write FKey;
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

    function EncodeAsStream(AValue: TStream): TStringStream; overload; virtual; abstract;
    function DecodeAsStream(AValue: TStream): TStringStream; overload; virtual; abstract;
  published
    property Key : StringRAL read FKey write SetKey;
  end;

implementation

{ TRALCripto }

procedure TRALCripto.SetKey(AValue : StringRAL);
begin
  if FKey = AValue then
    Exit;

  FKey :=AValue;
end;

function TRALCripto.Encode(AValue : StringRAL) : StringRAL;
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(AValue);
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
  vResult: TStringStream;
begin
  vResult := TStringStream(EncodeAsStream(AValue));
  try
    Result := vResult.DataString;
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
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decode(AValue : TBytes) : StringRAL;
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
  vResult: TStringStream;
begin
  vResult := TStringStream(EncodeAsStream(AValue));
  try
    Result := vResult.Bytes;
    SetLength(Result, vResult.Size);
  finally
    FreeAndNil(vResult);
  end;
end;

function TRALCripto.DecodeAsBytes(AValue : TStream) : TBytes;
var
  vResult: TStringStream;
begin
  vResult := TStringStream(DecodeAsStream(AValue));
  try
    Result := vResult.Bytes;
    SetLength(Result, vResult.Size);
  finally
    FreeAndNil(vResult);
  end;
end;

end.

