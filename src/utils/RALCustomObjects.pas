unit RALCustomObjects;

interface

uses
  Classes, SysUtils,
  RALParams, RALTypes, RALConsts;

type

  { TRALComponent }

  TRALComponent = class(TComponent)
  private
    function getVersion: string;
  published
    property Version: string read getVersion;
  end;

  { TRALHTTPHeaderInfo }

  TRALHTTPHeaderInfo = class
  private
    FParams: TRALParams;
  protected
    function GetParams: TRALParams;
  public
    constructor Create;
    destructor Destroy; override;

    function AddHeader(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddQuery(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddField(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddCookie(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddFile(AFileName: StringRAL): TRALHTTPHeaderInfo; overload; virtual;
    function AddFile(AStream: TStream; AFileName: StringRAL = ''): TRALHTTPHeaderInfo; overload; virtual;
    procedure Clear;
    function GetHeader(AName: StringRAL): StringRAL; virtual;
    function GetQuery(AName: StringRAL): StringRAL; virtual;
    function GetField(AName: StringRAL): StringRAL; virtual;
    function GetCookie(AName: StringRAL): StringRAL; virtual;
    function GetBody(AIdx: IntegerRAL): TRALParam; virtual;

    function ParamByName(AParamName: StringRAL): TRALParam;
  published
    property Params: TRALParams read GetParams;
  end;

implementation

{ TRALComponent }

function TRALComponent.getVersion: string;
begin
  Result := RALVERSION;
end;

{ TRALHTTPHeaderInfo }

function TRALHTTPHeaderInfo.GetParams: TRALParams;
begin
  Result := FParams;
end;

procedure TRALHTTPHeaderInfo.Clear;
begin
  FParams.ClearParams;
end;

constructor TRALHTTPHeaderInfo.Create;
begin
  inherited;
  FParams := TRALParams.Create;
end;

destructor TRALHTTPHeaderInfo.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TRALHTTPHeaderInfo.AddHeader(AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkHEADER);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddQuery(AName, AValue : StringRAL) : TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkQUERY);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddField(AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkFIELD);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddCookie(AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkCOOKIE);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddFile(AFileName: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddFile(AFileName);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddFile(AStream: TStream; AFileName: StringRAL): TRALHTTPHeaderInfo;
var
  vParam: TRALParam;
begin
  vParam := FParams.AddValue(AStream, rpkBODY);
  vParam.FileName := AFileName;
  Result := Self;
end;

function TRALHTTPHeaderInfo.GetHeader(AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.ParamByNameAndKind[AName, rpkHEADER];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetQuery(AName : StringRAL) : StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.ParamByNameAndKind[AName, rpkQUERY];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetField(AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.ParamByNameAndKind[AName, rpkFIELD];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetCookie(AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.ParamByNameAndKind[AName, rpkCOOKIE];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetBody(AIdx: IntegerRAL): TRALParam;
var
  vParam: TRALParam;
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FParams.Count do
  begin
    vParam := FParams.Param[vInt];
    if vParam.Kind = rpkBODY then
    begin
      if AIdx > 0 then
      begin
        AIdx := AIdx - 1;
      end
      else
      begin
        Result := vParam;
        Break;
      end;
    end;
  end;
end;

function TRALHTTPHeaderInfo.ParamByName(AParamName: StringRAL): TRALParam;
begin
  Result := FParams.Get[aParamName];
end;

end.
