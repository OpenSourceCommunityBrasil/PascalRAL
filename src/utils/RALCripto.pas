unit RALCripto;

interface

uses
  Classes, SysUtils;

type

  { TRALCripto }

  TRALCripto = class
  private
    FKey : string;
  protected
    procedure SetKey(AValue : string); virtual;
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

    function EncodeAsStream(AValue: TStream): TStringStream; overload;
    function DecodeAsStream(AValue: TStream): TStringStream; overload;
  published
    property Key : string read FKey write SetKey;
  end;

implementation

{ TRALCripto }

procedure TRALCripto.SetKey(AValue : string);
begin
  if FKey = AValue then
    Exit;

  FKey :=AValue;
end;

function TRALCripto.Encode(AValue : string) : string;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    vStream.Position := 0;
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Encode(AValue : TBytes) : string;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    vStream.Position := 0;
    Result := Encode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decode(AValue : string) : string;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    vStream.Position := 0;
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALCripto.Decode(AValue : TBytes) : string;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    vStream.Position := 0;
    Result := Decode(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

end.

