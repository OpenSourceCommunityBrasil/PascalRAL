unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALBase64;

type
  TRALClientInfo = class
  private
    FMACAddress: StringRAL;
    FIP: StringRAL;
    FUserAgent: StringRAL;
    procedure SetIP(const Value: StringRAL);
    procedure SetMACAddress(const Value: StringRAL);
    procedure SetUserAgent(const Value: StringRAL);
  public
    property IP: StringRAL read FIP write SetIP;
    property MACAddress: StringRAL read FMACAddress write SetMACAddress;
    property UserAgent: StringRAL read FUserAgent write SetUserAgent;
  end;

  TRALParam = class
  private
    FParamName: StringRAL;
    FContentType: StringRAL;
    FContent: TStream;
  protected
    function GetAsString: StringRAL;
    procedure SetAsString(const Value: StringRAL);
    function GetAsStream: TStream;
    procedure SetAsStream(const Value: TStream);
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

  TRALParams = class
  private
    FParams: TList;
    FNextParam : IntegerRAL;
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

  TRALAuthorization = class
  private
    FAuthType : TRALAuthTypes;
    FAuthString : StringRAL;
  protected
    function GetPassword: StringRAL;
    function GetUserName: StringRAL;
  public
    constructor Create;
  published
    property AuthType : TRALAuthTypes read FAuthType write FAuthType;
    property AuthString : StringRAL read FAuthString write FAuthString;
    property UserName : StringRAL read GetUserName;
    property Password : StringRAL read GetPassword;
  end;

  TRALRequest = class
  private
    FAuthorization : TRALAuthorization;
    FHeaders: TStringList;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
    FMethod: TRALMethod;
    FParams: TRALParams;
    FQuery: StringRAL;
  protected
    procedure SetHeaders(const Value: TStringList);
    procedure SetContentSize(const Value: Int64RAL);
    procedure SetContentType(const Value: StringRAL);
    procedure SetClientInfo(const Value: TRALClientInfo);
  public
    constructor Create;
    destructor Destroy; override;
    property ClientInfo: TRALClientInfo read FClientInfo write SetClientInfo;
    property ContentType: StringRAL read FContentType write SetContentType;
    property ContentSize: Int64RAL read FContentSize write SetContentSize;
    property Headers: TStringList read FHeaders write SetHeaders;
    property Params: TRALParams read FParams;
    property Method: TRALMethod read FMethod write FMethod;
    property Query: StringRAL read FQuery write FQuery;
    property Authorization : TRALAuthorization read FAuthorization write FAuthorization;
  end;

  TRALResponse = class
  private
    FHeaders: TStringList;
    FBody: TRALParams;
    FContentType: StringRAL;
    FRespCode: IntegerRAL;
    FResponse : TRALParam;
  protected
    procedure SetContentType(const AValue: StringRAL);
    procedure SetHeaders(const AValue: TStringList);
    function GetResponseStream: TStream;
    function GetResponseText: StringRAL;
    procedure SetResponseStream(const AValue: TStream);
    procedure SetResponseText(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
    property Body: TRALParams read FBody;
    property ResponseText : StringRAL read GetResponseText write SetResponseText;
    property ResponseStream : TStream read GetResponseStream write SetResponseStream;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
    property RespCode: IntegerRAL read FRespCode write FRespCode;
  end;

  TRALRoutes = class;
  TRALOnReply = procedure(Sender: TObject; ARequest: TRALRequest; var AResponse: TRALResponse) of object;

  TRALRoute = class(TCollectionItem)
  private
    FDisplayName : StringRAL;
    FDocument: StringRAL;
    FRouteList: TRALRoutes;
    FAllowedMethods: TRALMethods;
    FSkipAuthMethods: TRALMethods;
    FCallback: Boolean;
    FOnReply: TRALOnReply;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    function GetNamePath : string; override;
    function GetFullDocument: StringRAL;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Execute(ARequest: TRALRequest; var AResponse: TRALResponse);
    property FullDocument: StringRAL read GetFullDocument;
  published
    property DisplayName;
    property Document: StringRAL read FDocument write FDocument;
    property RouteList: TRALRoutes read FRouteList write FRouteList;
    // verbos que a rota responde
    property AllowedMethods: TRALMethods read FAllowedMethods write FAllowedMethods;
    // verbos que vão ignorar autenticação
    property SkipAuthMethods: TRALMethods read FSkipAuthMethods write FSkipAuthMethods;
    // se for uma rota de callback pra OAuth
    property Callback: Boolean read FCallback write FCallback;
    property OnReply: TRALOnReply read FOnReply write FOnReply;
  end;

  TRALRoutes = class(TOwnedCollection)
  protected
    function getRoute(address: StringRAL): TRALRoute;
    function findRoute(subdomain, address: StringRAL; partial: Boolean = false) : TRALRoute;
  public
    constructor Create(AOwner: TPersistent);
    property RouteAddress[address: StringRAL]: TRALRoute read getRoute;
  end;

implementation

{ TRALRoutes }

constructor TRALRoute.Create(ACollection: TCollection);
begin
  inherited;
  FRouteList := TRALRoutes.Create(Self);
  FAllowedMethods := [amALL];
  FSkipAuthMethods := [];
  FCallback := false;
  FDisplayName := 'ralevent'+IntToStr(Index);
  Changed(false);
end;

destructor TRALRoute.Destroy;
begin
  FreeAndNil(FRouteList);
  inherited;
end;

procedure TRALRoute.Execute(ARequest: TRALRequest; var AResponse: TRALResponse);
begin
  if Assigned(OnReply) then
  begin
    OnReply(Self, ARequest, AResponse);
  end
  else
  begin
    AResponse.RespCode := 404;
    AResponse.ContentType := TRALContentType.ctTEXTHTML;
  end;
end;

function TRALRoute.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TRALRoute.GetFullDocument: StringRAL;
begin
  Result := '';
  if (Collection <> nil) and (Collection.Owner is TRALRoute) then
    Result := TRALRoute(Collection.Owner).FullDocument;

  if Result <> '' then
    Result := Result + '/';

  Result := '/' + Result + FDocument + '/';
  Result := FixRoute(Result);
end;

function TRALRoute.GetNamePath: string;
begin
  Result := Collection.GetNamePath + FDisplayName;
  Result := ReplaceStr(Result,'.Routes','.r');
  Result := ReplaceStr(Result,'.RouteList','.rl');
end;

procedure TRALRoute.SetDisplayName(const Value: string);
begin
  if Trim(Value) <> '' then begin
    FDisplayName := Value;
    if FDocument = '' then
      FDocument := Value;
  end;
end;

{ RALRoutes }

constructor TRALRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALRoute);
end;

function TRALRoutes.findRoute(subdomain, address: StringRAL; partial: Boolean)
  : TRALRoute;
var
  vInt: IntegerRAL;
  vRoute: TRALRoute;
  vResp: TRALRoute;
  vaddr1, vaddr2: StringRAL;
  vpart: Boolean;
begin
  address := FixRoute(address);

  Result := nil;
  vInt := 0;
  vpart := false;
  vResp := nil;

  while vInt < Count do
  begin
    vRoute := TRALRoute(Items[vInt]);
    vaddr1 := FixRoute(subdomain + vRoute.Document);
    vaddr2 := Copy(address, 1, Length(vaddr1));
    if vRoute.RouteList.Count = 0 then
    begin
      if SameText(vaddr1, address) then
      begin
        vResp := vRoute;
        vpart := false;
      end
      else if (partial) and SameText(vaddr1, vaddr2) then
      begin
        vResp := vRoute;
        vpart := True;
      end;
    end
    else
    begin
      vResp := vRoute.RouteList.findRoute(vaddr1, address, partial);
    end;

    if (vResp <> nil) and (not vpart) then
    begin
      Result := vResp;
      Break;
    end;
    vInt := vInt + 1;
  end;

  if (Result = nil) and (vResp <> nil) then
    Result := vResp;
end;

function TRALRoutes.getRoute(address: StringRAL): TRALRoute;
begin
  Result := findRoute('', address, false);
  if Result = nil then
    Result := findRoute('', address, True);
end;

{ TRALClientInfo }

procedure TRALClientInfo.SetIP(const Value: StringRAL);
begin
  FIP := Value;
end;

procedure TRALClientInfo.SetMACAddress(const Value: StringRAL);
begin
  FMACAddress := Value;
end;

procedure TRALClientInfo.SetUserAgent(const Value: StringRAL);
begin
  FUserAgent := Value;
end;

{ TRALRequest }

constructor TRALRequest.Create;
begin
  inherited;
  FAuthorization := TRALAuthorization.Create;
  FHeaders := TStringList.Create;
  FClientInfo := TRALClientInfo.Create;
  FContentSize := 0;
  FParams := TRALParams.Create;
end;

destructor TRALRequest.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FClientInfo);
  FreeAndNil(FParams);
  FreeAndNil(FAuthorization);
  inherited;
end;

procedure TRALRequest.SetClientInfo(const Value: TRALClientInfo);
begin
  FClientInfo := Value;
end;

procedure TRALRequest.SetContentSize(const Value: Int64RAL);
begin
  FContentSize := Value;
end;

procedure TRALRequest.SetContentType(const Value: StringRAL);
begin
  FContentType := Value;
end;

procedure TRALRequest.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

{ TRALResponse }

constructor TRALResponse.Create;
begin
  inherited;
  FHeaders := TStringList.Create;
  FContentType := TRALContentType.ctTEXTHTML;
  FBody := TRALParams.Create;
  FResponse := nil; // pertence ao body;
end;

destructor TRALResponse.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FBody);
  FResponse := nil; // pertence ao body;
  inherited;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  Result := nil;
  if FResponse <> nil then
    Result := FResponse.AsStream;
end;

function TRALResponse.GetResponseText: StringRAL;
begin
  Result := '';
  if FResponse <> nil then
    Result := FResponse.AsString;
end;

procedure TRALResponse.SetContentType(const AValue: StringRAL);
begin
  FContentType := AValue;
  if (FResponse <> nil) and (FBody.Count = 1) then
    FResponse.ContentType := AValue;
end;

procedure TRALResponse.SetHeaders(const AValue: TStringList);
begin
  FHeaders := AValue;
end;

procedure TRALResponse.SetResponseStream(const AValue: TStream);
begin
  Body.ClearParams;
  if AValue.Size > 0 then begin
    FResponse := Body.AddValue(AValue);
    FResponse.ContentType := FContentType;
  end;
end;

procedure TRALResponse.SetResponseText(const AValue: StringRAL);
begin
  Body.ClearParams;
  if AValue <> '' then begin
    FResponse := Body.AddValue(AValue);
    FResponse.ContentType := FContentType;
  end;
end;

{ TRALParam }

constructor TRALParam.Create;
begin
  inherited;
  FContent := nil;
  FContentType := TRALContentType.ctTEXTHTML;
end;

destructor TRALParam.Destroy;
begin
  FreeAndNil(FContent);
  inherited;
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := TMemoryStream.Create;
  Result.CopyFrom(FContent, FContent.Size);
  Result.Position := 0;
end;

function TRALParam.GetAsString: StringRAL;
begin
  Result := '';
  if FContent.Size > 0 then
  begin
    if FContent.ClassType = TMemoryStream then begin
      FContent.Position := 0;
      SetLength(Result, FContent.Size);
      FContent.Read(Result[PosIniStr], FContent.Size);
    end
    else if FContent.ClassType = TStringStream then begin
      Result := TStringStream(FContent).DataString;
    end;

    FContent.Position := 0;
  end;
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SetAsStream(const Value: TStream);
begin
  if Assigned(FContent) then
    FreeAndNil(FContent);

  FContent := TStringStream.Create;
  FContent.CopyFrom(Value, Value.Size);
  FContent.Position := 0;
end;

procedure TRALParam.SetAsString(const Value: StringRAL);
begin
  if Assigned(FContent) then
    FreeAndNil(FContent);

  FContent := TStringStream.Create(Value);
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

{ TRALAuthorization }

constructor TRALAuthorization.Create;
begin
  inherited;
  FAuthType := ratNone;
  FAuthString := '';
end;

function TRALAuthorization.GetPassword: StringRAL;
var
  vString : StringRAL;
  vInt : IntegerRAL;
begin
  Result := '';
  if FAuthType = ratBasic then
  begin
    vString := TRALBase64.Decode(FAuthString);
    vInt := Pos(':',vString);
    if vInt > 0 then
      Result := Copy(vString, vInt+1, Length(vString));
  end;
end;

function TRALAuthorization.GetUserName: StringRAL;
var
  vString : StringRAL;
  vInt : IntegerRAL;
begin
  Result := '';
  if FAuthType = ratBasic then
  begin
    vString := TRALBase64.Decode(FAuthString);
    vInt := Pos(':',vString);
    if vInt > 0 then
      Result := Copy(vString, 1, vInt-1);
  end;
end;

end.
