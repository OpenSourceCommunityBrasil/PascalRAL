unit RALParams;

interface

uses
  Classes, SysUtils,
  RALTypes, RALMIMETypes;

type

  { TRALParam }

  TRALParam = class
  private
    FParamName: StringRAL;
    FContentType: StringRAL;
    FContent: TStringStream;
  protected
    function GetAsString: StringRAL;
    procedure SetAsString(const AValue : StringRAL);
    function GetAsStream: TStream;
    procedure SetAsStream(const AValue: TStream);
    function GetContentSize: Int64RAL;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property ParamName: StringRAL read FParamName write FParamName;
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read GetContentSize;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
  end;

  { TRALParams }

  TRALParams = class
  private
    FParams: TList;
    FNextParam: IntegerRAL;
  protected
    function GetParam(idx: IntegerRAL): TRALParam;
    function GetParamName(name: StringRAL): TRALParam;
    function NextParam : StringRAL;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: IntegerRAL;
    function AddParam(AName, AContent: StringRAL;
                      AType: StringRAL = TRALContentType.ctTEXTHTML): TRALParam; overload;
    function AddParam(AName: StringRAL; AContent: TStream;
                      AType: StringRAL = TRALContentType.ctAPPLICATIONOCTETSTREAM) : TRALParam; overload;
    function AddValue(AContent: StringRAL;
                      AType: StringRAL = TRALContentType.ctTEXTHTML): TRALParam; overload;
    function AddValue(AContent: TStream;
                      AType: StringRAL = TRALContentType.ctAPPLICATIONOCTETSTREAM): TRALParam; overload;
    function NewParam: TRALParam;
    procedure ClearParams;
    property Param[idx: IntegerRAL]: TRALParam read GetParam;
    property ParamName[name: StringRAL]: TRALParam read GetParamName;
  end;

implementation

{ TRALParam }

constructor TRALParam.Create;
begin
  inherited;
  FContent := TStringStream.Create;
  FContentType := TRALContentType.ctTEXTHTML;
end;

destructor TRALParam.Destroy;
begin
  FreeAndNil(FContent);
  inherited;
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := TStringStream.Create;
  Result.CopyFrom(FContent, FContent.Size);
  Result.Position := 0;
end;

function TRALParam.GetAsString: StringRAL;
begin
  Result := '';
  if FContent.Size > 0 then
  begin
    Result := FContent.DataString;
    FContent.Position := 0;
  end;
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SetAsStream(const AValue: TStream);
begin
  FContent.Size := 0;
  FContent.CopyFrom(AValue, AValue.Size);
  FContent.Position := 0;
end;

procedure TRALParam.SetAsString(const AValue: StringRAL);
begin
  FContent.Size := 0;
  FContent.WriteString(AValue);
  FContent.Position := 0;
end;

{ TRALParams }

function TRALParams.AddParam(AName, AContent, AType: StringRAL): TRALParam;
begin
  Result := ParamName[AName];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsString := AContent;
  Result.ContentType := AType;
end;

function TRALParams.AddParam(AName: StringRAL; AContent: TStream;
  AType: StringRAL): TRALParam;
begin
  Result := ParamName[AName];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsStream := AContent;
  Result.ContentType := AType;
end;

function TRALParams.AddValue(AContent: TStream; AType: StringRAL): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParam;
  Result.AsStream := AContent;
  Result.ContentType := AType;
end;

function TRALParams.AddValue(AContent, AType: StringRAL): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParam;
  Result.AsString := AContent;
  Result.ContentType := AType;
end;

procedure TRALParams.ClearParams;
begin
  while FParams.Count > 0 do
  begin
    TObject(FParams.Items[FParams.Count - 1]).Free;
    FParams.Delete(FParams.Count - 1);
  end;
end;

function TRALParams.Count: IntegerRAL;
begin
  Result := FParams.Count;
end;

constructor TRALParams.Create;
begin
  inherited;
  FParams := TList.Create;
  FNextParam := 0;
end;

destructor TRALParams.Destroy;
begin
  ClearParams;
  FreeAndNil(FParams);
  inherited;
end;

function TRALParams.GetParam(idx: IntegerRAL): TRALParam;
begin
  Result := nil;
  if (idx >= 0) and (idx < FParams.Count) then
    Result := TRALParam(FParams.Items[idx]);
end;

function TRALParams.GetParamName(name: StringRAL): TRALParam;
var
  idx: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;

  idx := 0;
  while idx < FParams.Count do
  begin
    vParam := TRALParam(FParams.Items[idx]);
    if SameText(vParam.ParamName, name) then
    begin
      Result := vParam;
      Break;
    end;

    idx := idx + 1;
  end;
end;

function TRALParams.NewParam: TRALParam;
begin
  Result := TRALParam.Create;
  FParams.Add(Result)
end;

function TRALParams.NextParam: StringRAL;
begin
  FNextParam := FNextParam + 1;
  Result := 'ralparam'+IntToStr(FNextParam);
end;

end.

