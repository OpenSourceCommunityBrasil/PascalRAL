unit RALRoutes;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALTools;

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
    FParamName : StringRAL;
    FContentType: StringRAL;
    FContent : TStream;
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
    FParams : TList;
  protected
    function GetParam(idx: IntegerRAL): TRALParam;
    function GetParamName(name: StringRAL): TRALParam;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : IntegerRAL;
    function AddParam(AName, AContent : StringRAL; AType : StringRAL = 'text/plain') : TRALParam; overload;
    function AddParam(AName : StringRAL; AContent : TStream; AType : StringRAL = 'application/octed') : TRALParam; overload;
    function NewParam : TRALParam;

    procedure ClearParams;

    property Param[idx : IntegerRAL] : TRALParam read GetParam;
    property ParamName[name : StringRAL] : TRALParam read GetParamName;
  end;

  TRALRequest = class
  private
    FHeaders: TStringList;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
    FMethod : TRALMethod;
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
  end;

  TRALResponse = class
  private
    FHeaders: TStringList;
    FBody: TRALParams;
    FContentType: StringRAL;
    FRespCode : IntegerRAL;
  protected
    procedure SetContentType(const Value: StringRAL);
    procedure SetHeaders(const Value: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    property Body: TRALParams read FBody;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write SetHeaders;
    property RespCode: IntegerRAL read FRespCode write FRespCode;
  end;

  TRALRoutes = class;
  TRALOnReply = procedure(Sender : TObject; ARequest : TRALRequest; var AResponse : TRALResponse) of object;

  TRALRoute = class(TCollectionItem)
  private
    FDisplayName: StringRAL;
    FDocument: StringRAL;
    FRouteList: TRALRoutes;
    FAllowedMethods: TRALMethods;
    FSkipAuthMethods: TRALMethods;
    FCallback: Boolean;
    FOnReply : TRALOnReply;
  protected
    function GetDisplayName: StringRAL; override;
    procedure SetDisplayName(const Value: StringRAL); override;
    function GetFullDocument: StringRAL;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Execute(ARequest : TRALRequest; var AResponse : TRALResponse);

    property FullDocument : StringRAL read GetFullDocument;
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
    function findRoute(subdomain, address : StringRAL; partial : boolean = false) : TRALRoute;
    function fixAddress(address : StringRAL) : StringRAL;
  public
    constructor Create(AOwner : TPersistent);
    property RouteAddress[Address : StringRAL] : TRALRoute read getRoute;
  end;

implementation

{ TRALRoutes }

constructor TRALRoute.Create(Collection: TCollection);
begin
  inherited;
  FDisplayName := GetNamePath;
  FRouteList := TRALRoutes.Create(Self);
  FAllowedMethods := [amALL];
  FSkipAuthMethods := [];
  FCallback := False;
  Changed(False);
end;

destructor TRALRoute.Destroy;
begin
  FreeAndNil(FRouteList);
  inherited;
end;

procedure TRALRoute.Execute(ARequest: TRALRequest; var AResponse: TRALResponse);
begin
  if Assigned(OnReply) then begin
    OnReply(Self,ARequest,AResponse);
  end
  else begin
    AResponse.RespCode := 404;
    AResponse.ContentType := 'text/html';
  end;
end;

function TRALRoute.GetDisplayName: StringRAL;
begin
  Result := GetNamePath;
  if FDisplayName <> '' then
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
  Result := TRALRoutes(Collection).fixAddress(Result);
end;

procedure TRALRoute.SetDisplayName(const Value: StringRAL);
begin
  if Value <> '' then
    FDisplayName := Value
  else
    FDisplayName := GetNamePath;
  inherited;
end;

{ RALRoutes }

constructor TRALRoutes.Create(AOwner : TPersistent);
begin
  inherited Create(AOwner,TRALRoute);
end;

function TRALRoutes.findRoute(subdomain, address: StringRAL; partial : boolean): TRALRoute;
var
  vInt : IntegerRAL;
  vRoute : TRALRoute;
  vResp : TRALRoute;
  vaddr1, vaddr2 : StringRAL;
  vpart : boolean;
begin
  address := fixAddress(address);

  Result := nil;
  vInt := 0;
  vpart := False;
  vResp := nil;

  while vInt < Count do begin
    vRoute := TRALRoute(Items[vInt]);
    vaddr1 := fixAddress(subdomain + vRoute.Document);
    vaddr2 := Copy(address,1,Length(vaddr1));
    if vRoute.RouteList.Count = 0 then begin
      if SameText(vaddr1,address) then begin
        vResp := vRoute;
        vpart := False;
      end
      else if (partial) and SameText(vaddr1,vaddr2) then begin
        vResp := vRoute;
        vpart := True;
      end;
    end
    else begin
      vResp := vRoute.RouteList.findRoute(vaddr1,address,partial);
    end;

    if (vResp <> nil) and (not vpart) then begin
      Result := vResp;
      Break;
    end;
    vInt := vInt + 1;
  end;

  if (Result = nil) and (vResp <> nil) then
    Result := vResp;
end;

function TRALRoutes.fixAddress(address: StringRAL): StringRAL;
begin
  Result := '/'+address+'/';
  while Pos('//',Result) > 0 do
    Result := ReplaceStr(Result,'//','/');
end;

function TRALRoutes.getRoute(address: StringRAL): TRALRoute;
begin
  Result := findRoute('',address,False);
  if Result = nil then
    Result := findRoute('',address,True);
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
  FContentType := 'text/plain';
  FBody := TRALParams.Create;
end;

destructor TRALResponse.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FBody);
  inherited;
end;

procedure TRALResponse.SetContentType(const Value: StringRAL);
begin
  FContentType := Value;
end;

procedure TRALResponse.SetHeaders(const Value: TStringList);
begin
  FHeaders := Value;
end;

{ TRALParam }

constructor TRALParam.Create;
begin
  inherited;
  FContent := TBytesStream.Create;
  FContentType := 'text/plain';
end;

destructor TRALParam.Destroy;
begin
  FreeAndNil(FContent);
  inherited;
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := TMemoryStream.Create;
  Result.CopyFrom(FContent,FContent.Size);
  Result.Position := 0;
end;

function TRALParam.GetAsString: StringRAL;
begin
  Result := '';
  if FContent.Size > 0 then begin
    FContent.Position := 0;
    SetLength(Result,FContent.Size);
    FContent.Read(Result[PosIniStr],FContent.Size);

    FContent.Position := 0;
  end;
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SetAsStream(const Value: TStream);
begin
  Value.Position := 0;
  FContent.CopyFrom(Value,Value.Size);
  FContent.Position := 0;
end;

procedure TRALParam.SetAsString(const Value: StringRAL);
var
  vBytes : TBytes;
begin
  vBytes := VarToBytes(Value);

  FContent.Size := 0;
  FContent.Position := 0;
  FContent.Write(vBytes,Length(vBytes));

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

procedure TRALParams.ClearParams;
begin
  while FParams.Count > 0 do begin
    TObject(FParams.Items[FParams.Count-1]).Free;
    FParams.Delete(FParams.Count-1);
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
  idx : IntegerRAL;
  vParam : TRALParam;
begin
  Result := nil;

  idx := 0;
  while idx < FParams.Count do begin
    vParam := TRALParam(FParams.Items[idx]);
    if SameText(vParam.ParamName,name) then begin
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

end.
