/// Base unit for the module component that will enable DBWare on the server
unit RALDBModule;

interface

uses
  Classes, SysUtils, DB, SyncObjs,
  RALServer, RALRequest, RALResponse, RALDBBase, RALParams, RALMIMETypes,
  RALConsts, RALTypes, RALStorage, RALBase64, RALRoutes, RALJson, RALStorageTypes,
  RALDBSQLCache, RALStream, RALDBPool;

type
  TRALDBModule = class;

  { Fired before TRALDBModule runs a stored procedure, with what the catalog says
    of it and the params the call takes (typed by the catalog). AAllow starts
    True: set it False to refuse the call. A value written to AParams replaces
    what the client sent - a claim of the token, a header }
  TRALDBOnValidateProc = procedure(ASender: TObject; ARequest: TRALRequest;
    AProc: TRALDBProcInfo; AParams: TParams; var AAllow: Boolean) of object;

  { ERALDBProcError }

  /// A refused or failed stored procedure call, with the HTTP status it answers
  ERALDBProcError = class(Exception)
  private
    FStatusCode: IntegerRAL;
  public
    constructor Create(AStatusCode: IntegerRAL; const AMessage: string);

    /// HTTP status of the answer: 400, 403 or 404
    property StatusCode: IntegerRAL read FStatusCode;
  end;

  { TRALDBProcOptions }

  { How TRALDBModule exposes the stored procedures of its database: one route,
    Route + '/:name', off until Enabled. The database is the list - nothing is
    kept here - and the GRANT EXECUTE of the module's user is the real limit }
  TRALDBProcOptions = class(TPersistent)
  private
    FCatalogLife: IntegerRAL;
    FEnabled: boolean;
    FFilter: TStrings;
    FModule: TRALDBModule;
    FRoute: StringRAL;
    FSchemas: TStrings;
    FStorage: TRALStorageLink;
    function IsRouteStored: boolean;
    procedure SetEnabled(AValue: boolean);
    procedure SetFilter(AValue: TStrings);
    procedure SetRoute(AValue: StringRAL);
    procedure SetSchemas(AValue: TStrings);
    procedure SetStorage(AValue: TRALStorageLink);
  public
    constructor Create(AModule: TRALDBModule);
    destructor Destroy; override;

    /// Whether AName passes Filter (case insensitive; * and ? masks)
    function AllowsName(const AName: StringRAL): boolean;
    /// Whether ASchema is one of Schemas; any when Schemas is empty
    function AllowsSchema(const ASchema: StringRAL): boolean;
    procedure Assign(ASource: TPersistent); override;
  published
    /// Minutes the module keeps what the catalog said of a procedure; 0 asks on every call
    property CatalogLife: IntegerRAL read FCatalogLife write FCatalogLife default DEFAULTDBCATALOGLIFE;
    /// Creates the route; off, the stored procedures are not reachable (also by the RAL datasets)
    property Enabled: boolean read FEnabled write SetEnabled default False;
    /// Name masks the route answers, one per line (API_*); empty takes every procedure
    property Filter: TStrings read FFilter write SetFilter;
    /// Prefix of the route: Route/name
    property Route: StringRAL read FRoute write SetRoute stored IsRouteStored;
    /// Schemas whose procedures are reachable, one per line; empty is the connection's default
    property Schemas: TStrings read FSchemas write SetSchemas;
    /// Storage of the answer to a plain HTTP client whose Accept names none; empty is JSON
    property Storage: TRALStorageLink read FStorage write SetStorage;
  end;

  { TRALDBRequest }

  { The request of a DBWare route (see TRALModuleRequest): the core request plus
    what every one of those routes needs - the connection, taken from the pool
    on first use, and the statements the client sent, read from the body on
    first use. Both are given back when the request ends, whatever happened,
    so a route never writes the try/finally that returns the connection }
  TRALDBRequest = class(TRALModuleRequest)
  private
    FCoreResponse: TRALResponse;
    FDatabase: TRALDBBase;
    FSQLCache: TRALDBSQLCache;
  public
    destructor Destroy; override;
    /// The pooled connection bound to this request; raises emDBDriverMissing
    /// when there is no driver
    function Database: TRALDBBase;
    /// The statements of the body (opensql, execsql, applyupdates); raises
    /// emDBEmptyBody when there is no body
    function SQLCache: TRALDBSQLCache;
  end;

  { TRALDBResponse }

  { The response of a DBWare route: the core response plus the answers of the
    DBWare protocol, which the RAL datasets on the client read. Only the routes
    of TRALDBModule see these Answer overloads }
  TRALDBResponse = class(TRALModuleResponse)
  public
    /// The results of the statements of ASQLCache, as the 'Stream' body param
    procedure Answer(ASQLCache: TRALDBSQLCache); overload;
    /// The error as the datasets read it: the lone 'Exception' body param,
    /// 429 when the pool timed out, 500 otherwise
    procedure AnswerException(AException: Exception);
  end;

  /// Handler of a DBWare route (TRALDBModule.CreateDBRoute)
  TRALDBOnReply = procedure(ARequest: TRALDBRequest; AResponse: TRALDBResponse) of object;

  { TRALDBRoute }

  /// A route of TRALDBModule: answered by OnDBReply, with the module's request
  /// and response, unless a core handler (OnReply) was assigned
  TRALDBRoute = class(TRALRoute)
  private
    FOnDBReply: TRALDBOnReply;
  public
    property OnDBReply: TRALDBOnReply read FOnDBReply write FOnDBReply;
  end;

  { TRALDBModule }

  TRALDBModule = class(TRALModuleRoutes)
  private
    FAutoCommit: boolean;
    FCatalog: TStringList;
    FCatalogLock: TCriticalSection;
    FDatabase: StringRAL;
    FDatabaseLink: String;
    FDatabaseType: TRALDatabaseType;
    FHostname: StringRAL;
    FCharacterSet: StringRAL;
    FConnectionParams: TStrings;
    FLibLocation: String;
    FPassword: StringRAL;
    FPool: TRALDBConnectionPool;
    FPort: IntegerRAL;
    FProcOptions: TRALDBProcOptions;
    FProcRoute: TRALDBRoute;
    FUsername: StringRAL;

    FOnBeforeConnect: TRALDBOnConnect;
    FOnAfterConnect: TRALDBOnConnect;
    FOnErrorConnect: TRALDBOnError;
    FOnErrorQuery: TRALDBOnError;
    FOnValidateProc: TRALDBOnValidateProc;
    FOnValidateSQL: TRALDBOnValidateSQL;
    procedure SetConnectionParams(AValue: TStrings);
    procedure SetLibLocation(AValue: String);
    procedure SetProcOptions(AValue: TRALDBProcOptions);
    /// Asks OnValidateSQL about a statement from the wire; raises when refused
    procedure CheckSQL(ARequest: TRALRequest; const ASQL: StringRAL);
  protected
    { What the catalog says of AName, from the cache while CatalogLife lasts;
      nil when there is no such procedure. A copy, freed by the caller }
    function CachedProc(ADatabase: TRALDBBase; const AName: StringRAL;
      AParamNames: TStrings): TRALDBProcInfo;
    { The procedure AName of a call, after every rule: Enabled, the name, Filter,
      the catalog, Schemas and OnValidateProc; raises ERALDBProcError otherwise.
      AParams gets the catalog's types and directions. Freed by the caller }
    function CheckProc(ADatabase: TRALDBBase; ARequest: TRALRequest;
      const AName: StringRAL; AParams: TParams): TRALDBProcInfo;
    /// Runs the stored procedure of ADBSQL (etProc) and fills its response
    procedure ProcSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
    /// The route of the stored procedures: Route/:ral_procname
    procedure ExecProcRoute(ARequest: TRALDBRequest; AResponse: TRALDBResponse); virtual;
    { Saves AQuery for a client whose dataset is of AClientDriver: native when the
      driver and the link allow, else in AStorage (raises without one) }
    function SaveResultSet(ADatabase: TRALDBBase; AQuery: TDataSet;
      AClientDriver: TRALDBDriverType; AStorage: TRALStorageLink; out ANative: boolean;
      out AContentType: StringRAL): TMemoryStream;
    /// Creates, renames or drops the route of the stored procedures
    procedure UpdateProcRoute;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Fills AResponse with the error, answering 429 when the pool timed out
    procedure AnswerException(AResponse: TRALResponse; AException: Exception); virtual;
    /// Factory handed to the pool, so it can open connections on its own
    function CreatePoolConnection(ASender: TObject): TRALDBBase;
    /// Runs the OnDBReply of a TRALDBRoute with the module's request/response
    procedure ExecuteContext(ARoute: TRALRoute; ARequest: TRALModuleRequest;
      AResponse: TRALModuleResponse); override;
    function GetPoolOptions: TRALDBPoolOptions;
    /// The errors of the DBWare routes, answered as the datasets read them; a
    /// route the application added with CreateRoute keeps the server's answer
    function HandleException(ARequest: TRALRequest; AResponse: TRALResponse;
      AException: Exception): boolean; override;
    class function RequestClass: TRALModuleRequestClass; override;
    class function ResponseClass: TRALModuleResponseClass; override;
    class function RouteClass: TRALRouteClass; override;
    /// Opens the pool (PoolOptions.PrepareOnActivate)
    procedure ServerActivating; override;
    procedure SetPoolOptions(AValue: TRALDBPoolOptions);

    { The DBWare routes. Virtual, so a descendant extends one - audits
      opensql, say - by overriding it and calling inherited }
    procedure ApplyUpdates(ARequest: TRALDBRequest; AResponse: TRALDBResponse); virtual;
    procedure ExecSQL(ARequest: TRALDBRequest; AResponse: TRALDBResponse); virtual;
    function FindDatabaseDriver(ARequest: TRALRequest; AResponse: TRALResponse) : TRALDBBase;
    procedure GetFields(ARequest: TRALDBRequest; AResponse: TRALDBResponse); virtual;
    procedure GetSQLFields(ARequest: TRALDBRequest; AResponse: TRALDBResponse); virtual;
    procedure GetTables(ARequest: TRALDBRequest; AResponse: TRALDBResponse); virtual;
    procedure OpenSQL(ARequest: TRALDBRequest; AResponse: TRALDBResponse); virtual;

    procedure OpenSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
    procedure ExecSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
    function GetInfoFieldsStream(ADatabase: TRALDBBase; ADataset: TDataSet;
                                 ABinary: boolean): TStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Takes a connection from the pool, already bound to this request. Also usable
      from custom routes that need to reach the same database - a route made
      with CreateDBRoute gets it from ARequest.Database, returned by itself }
    function AcquireDatabase(ARequest: TRALRequest; AResponse: TRALResponse): TRALDBBase;
    /// A route answered by AReplyProc with the module's request and response
    function CreateDBRoute(const ARoute: StringRAL; AReplyProc: TRALDBOnReply;
      const ADescription: StringRAL = ''): TRALDBRoute;
    /// Forgets what the catalog said: the next call asks the database again
    procedure RefreshProcedures;
    /// Gives a connection taken by AcquireDatabase back to the pool
    procedure ReleaseDatabase(ADatabase: TRALDBBase);

    { Live pool, for statistics and for opening the initial connections through
      Pool.Prepare once the server is up }
    property Pool: TRALDBConnectionPool read FPool;
  published
    { True (the default): every statement and call commits on its own, and one
      that fails does not take the others. False: a request is one transaction -
      all its statements commit at the end, or the first failure undoes them all,
      the others answered as emDBRolledBack. A client may ask the same of one
      request (TRALDBConnection.StartTransaction) }
    property AutoCommit: boolean read FAutoCommit write FAutoCommit default True;
    { Name of the database this DBModule is connecting into }
    property Database: StringRAL read FDatabase write FDatabase;
    { DAO object that will be used to connect with the Database }
    property DatabaseLink: String read FDataBaseLink write FDataBaseLink;
    { Database engine that this DBModule is connecting into }
    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    { Server where the database is located }
    property Hostname: StringRAL read FHostname write FHostname;
    { Password to connect into the database }
    property Password: StringRAL read FPassword write FPassword;
    { Connection pool settings, editable in the Object Inspector at design time.
      Turning PoolOptions.Enabled on makes the module reuse open connections
      instead of opening one per request }
    property PoolOptions: TRALDBPoolOptions read GetPoolOptions write SetPoolOptions;
    { The route of the stored procedures, off by default: see TRALDBProcOptions }
    property ProcOptions: TRALDBProcOptions read FProcOptions write SetProcOptions;
    { Database server port }
    property Port: IntegerRAL read FPort write FPort;
    { Name of the database server user }
    property Username: StringRAL read FUsername write FUsername;
    { Location of the library (dll) that is used to connect with the database }
    property LibLocation: String read FLibLocation write SetLibLocation;
    { Connection charset; empty lets the driver choose (UTF8 on Firebird) }
    property CharacterSet: StringRAL read FCharacterSet write FCharacterSet;
    { Name=Value settings handed to the database library before it connects -
      Zeos Properties, FireDAC or sqldb Params. See TRALDBBase.ConnectionParams }
    property ConnectionParams: TStrings read FConnectionParams write SetConnectionParams;

    property OnBeforeConnect: TRALDBOnConnect read FOnBeforeConnect write FOnBeforeConnect;
    property OnAfterConnect: TRALDBOnConnect read FOnAfterConnect write FOnAfterConnect;
    property OnErrorConnect: TRALDBOnError read FOnErrorConnect write FOnErrorConnect;
    property OnErrorQuery: TRALDBOnError read FOnErrorQuery write FOnErrorQuery;
    { Every statement the routes opensql, execsql, applyupdates and getsqlfields
      run comes from the client as text. Without Authentication on the server
      that is the whole database open on the network; with it, every logged
      user can still run anything. This event is where the application says
      which statements it accepts: set AAllow to False and the request gets
      a 500 with the reason, nothing reaches the database }
    property OnValidateSQL: TRALDBOnValidateSQL read FOnValidateSQL write FOnValidateSQL;
    { Every stored procedure call, by the route or by a RAL dataset, after Filter
      and the catalog: refuses it, audits it, fills params the client does not send }
    property OnValidateProc: TRALDBOnValidateProc read FOnValidateProc write FOnValidateProc;
  end;

implementation

uses
  RALTools, RALMultipartCoder;

const
  /// Name of the URI param of the stored procedure route: no procedure param takes it
  cProcNameParam = 'ral_procname';

type
  { TRALDBProcCacheItem }

  /// What the catalog said of a procedure, and when
  TRALDBProcCacheItem = class
  public
    Info: TRALDBProcInfo;
    Stamp: TDateTime;
    destructor Destroy; override;
  end;

destructor TRALDBProcCacheItem.Destroy;
begin
  FreeAndNil(Info);
  inherited Destroy;
end;

/// Whether AText matches AMask, * any run and ? any one character, ignoring case
function MatchesMask(const AText, AMask: string): boolean;
var
  vText, vMask: string;
  vT, vM, vStarT, vStarM: IntegerRAL;
begin
  vText := UpperCase(AText);
  vMask := UpperCase(AMask);
  vT := 1;
  vM := 1;
  vStarT := 0;
  vStarM := 0;
  while vT <= Length(vText) do
  begin
    if (vM <= Length(vMask)) and ((vMask[vM] = '?') or (vMask[vM] = vText[vT])) then
    begin
      Inc(vT);
      Inc(vM);
    end
    else if (vM <= Length(vMask)) and (vMask[vM] = '*') then
    begin
      vStarM := vM;
      vStarT := vT;
      Inc(vM);
    end
    else if vStarM > 0 then
    begin
      // the last * takes one more character
      vM := vStarM + 1;
      Inc(vStarT);
      vT := vStarT;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;
  while (vM <= Length(vMask)) and (vMask[vM] = '*') do
    Inc(vM);
  Result := vM > Length(vMask);
end;

/// Whether AName is an identifier, schema.name allowed
function ValidProcName(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := AName <> '';
  for vInt := POSINISTR to RALHighStr(AName) do
    if not (AName[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$', '.']) then
    begin
      Result := False;
      Exit;
    end;
end;

{ Gives AParam, which came as text (JSON, a form, the query string), the type
  ADataType of the catalog. A param that already has a type - from the RAL wire -
  is left alone }
procedure TypeParam(AParam: TParam; ADataType: TFieldType);
var
  vText: string;
  vDate: TDateTime;
begin
  if not (AParam.DataType in [ftUnknown, ftString, ftWideString]) or
     (ADataType in [ftUnknown, ftString, ftWideString, ftFixedChar, ftFixedWideChar]) then
  begin
    if AParam.DataType = ftUnknown then
      AParam.DataType := ADataType;
    Exit;
  end;
  if AParam.IsNull then
  begin
    AParam.DataType := ADataType;
    Exit;
  end;
  vText := Trim(AParam.AsString);
  case ADataType of
    ftSmallint, ftInteger, ftWord:
      begin
        AParam.DataType := ADataType;
        AParam.AsInteger := StrToInt(vText);
      end;
    ftLargeint, ftAutoInc:
      AParam.AsLargeInt := StrToInt64(vText);
    ftFloat, ftCurrency:
      AParam.AsFloat := StrToFloat(vText, RALInvariantFormat);
    ftBCD, ftFMTBcd:
      AParam.AsFMTBCD := RALTextToBCD(StringRAL(vText));
    ftBoolean:
      AParam.AsBoolean := SameText(vText, 'true') or (vText = '1');
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      begin
        if not RALTryISO8601ToDateTime(StringRAL(vText), vDate) then
          vDate := StrToDateTime(vText);
        case ADataType of
          ftDate: AParam.AsDate := vDate;
          ftTime: AParam.AsTime := vDate;
        else
          AParam.AsDateTime := vDate;
        end;
      end;
    ftBlob, ftBytes, ftVarBytes:
      // binary goes as base64 in a text
      AParam.AsBlob := TRALBase64.DecodeAsBytes(StringRAL(vText));
    ftMemo, ftWideMemo:
      AParam.DataType := ADataType;
  else
    AParam.DataType := ADataType;
  end;
end;

{ ERALDBProcError }

constructor ERALDBProcError.Create(AStatusCode: IntegerRAL; const AMessage: string);
begin
  inherited Create(AMessage);
  FStatusCode := AStatusCode;
end;

{ TRALDBProcOptions }

constructor TRALDBProcOptions.Create(AModule: TRALDBModule);
begin
  inherited Create;
  FModule := AModule;
  FCatalogLife := DEFAULTDBCATALOGLIFE;
  FEnabled := False;
  FFilter := TStringList.Create;
  FRoute := DEFAULTDBPROCROUTE;
  FSchemas := TStringList.Create;
end;

destructor TRALDBProcOptions.Destroy;
begin
  FreeAndNil(FFilter);
  FreeAndNil(FSchemas);
  inherited Destroy;
end;

function TRALDBProcOptions.AllowsName(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := True;
  for vInt := 0 to Pred(FFilter.Count) do
  begin
    if Trim(FFilter[vInt]) = '' then
      Continue;
    Result := False;
    if MatchesMask(string(AName), Trim(FFilter[vInt])) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TRALDBProcOptions.AllowsSchema(const ASchema: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := True;
  for vInt := 0 to Pred(FSchemas.Count) do
  begin
    if Trim(FSchemas[vInt]) = '' then
      Continue;
    Result := False;
    if SameText(string(ASchema), Trim(FSchemas[vInt])) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TRALDBProcOptions.Assign(ASource: TPersistent);
begin
  if ASource is TRALDBProcOptions then
  begin
    FCatalogLife := TRALDBProcOptions(ASource).CatalogLife;
    FFilter.Assign(TRALDBProcOptions(ASource).Filter);
    FSchemas.Assign(TRALDBProcOptions(ASource).Schemas);
    Storage := TRALDBProcOptions(ASource).Storage;
    FRoute := TRALDBProcOptions(ASource).Route;
    FEnabled := TRALDBProcOptions(ASource).Enabled;
    if FModule <> nil then
      FModule.UpdateProcRoute;
  end
  else
    inherited Assign(ASource);
end;

function TRALDBProcOptions.IsRouteStored: boolean;
begin
  Result := FRoute <> DEFAULTDBPROCROUTE;
end;

procedure TRALDBProcOptions.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  if FModule <> nil then
    FModule.UpdateProcRoute;
end;

procedure TRALDBProcOptions.SetFilter(AValue: TStrings);
begin
  FFilter.Assign(AValue);
end;

procedure TRALDBProcOptions.SetRoute(AValue: StringRAL);
begin
  // the route alone: the name segment is the module's
  while (AValue <> '') and (AValue[POSINISTR] = '/') do
    Delete(AValue, POSINISTR, 1);
  while (AValue <> '') and (AValue[RALHighStr(AValue)] = '/') do
    Delete(AValue, RALHighStr(AValue), 1);
  if AValue = '' then
    AValue := DEFAULTDBPROCROUTE;
  if FRoute = AValue then
    Exit;
  FRoute := AValue;
  if FModule <> nil then
    FModule.UpdateProcRoute;
end;

procedure TRALDBProcOptions.SetSchemas(AValue: TStrings);
begin
  FSchemas.Assign(AValue);
end;

procedure TRALDBProcOptions.SetStorage(AValue: TRALStorageLink);
begin
  if FStorage = AValue then
    Exit;
  if (FStorage <> nil) and (FModule <> nil) then
    FStorage.RemoveFreeNotification(FModule);
  FStorage := AValue;
  if (FStorage <> nil) and (FModule <> nil) then
    FStorage.FreeNotification(FModule);
end;

{ TRALDBRequest }

destructor TRALDBRequest.Destroy;
begin
  FreeAndNil(FSQLCache);
  if FDatabase <> nil then
    TRALDBModule(Module).ReleaseDatabase(FDatabase);
  inherited Destroy;
end;

function TRALDBRequest.Database: TRALDBBase;
begin
  if FDatabase = nil then
  begin
    FDatabase := TRALDBModule(Module).AcquireDatabase(Core, FCoreResponse);
    if FDatabase = nil then
      raise Exception.Create(emDBDriverMissing);
  end;
  Result := FDatabase;
end;

function TRALDBRequest.SQLCache: TRALDBSQLCache;
var
  vMem: TStream;
begin
  if FSQLCache = nil then
  begin
    { the body where it is, not AsStream's copy of it: the cache only reads,
      and a batch of statements with their params can be large }
    vMem := Core.Body.Content;
    if (vMem = nil) or (vMem.Size = 0) then
      raise Exception.Create(emDBEmptyBody);
    FSQLCache := TRALDBSQLCache.Create;
    try
      FSQLCache.LoadFromStream(vMem);
    except
      FreeAndNil(FSQLCache);
      raise;
    end;
  end;
  Result := FSQLCache;
end;

{ TRALDBResponse }

procedure TRALDBResponse.Answer(ASQLCache: TRALDBSQLCache);
var
  vParam: TRALParam;
begin
  Core.ContentType := rctAPPLICATIONOCTETSTREAM;
  { the param takes the stream the cache built as it is - AddParam copied it
    once more, for a body that can be a whole result set }
  vParam := Core.Params.GetKind['Stream', rpkBODY];
  if vParam = nil then
    vParam := Core.Params.NewParam;
  vParam.ParamName := 'Stream';
  vParam.Kind := rpkBODY;
  vParam.AdoptStream(ASQLCache.ResponseToStream);
  vParam.ContentType := rctAPPLICATIONOCTETSTREAM;
end;

procedure TRALDBResponse.AnswerException(AException: Exception);
begin
  TRALDBModule(Module).AnswerException(Core, AException);
end;

{ TRALDBModule }

procedure TRALDBModule.SetLibLocation(AValue: String);
begin
  if FLibLocation = AValue then Exit;
  { empty is "the driver finds it"; an absolute path stays as it is: both were
    turned into a path under the executable's folder }
  if AValue = EmptyStr then
    FLibLocation := EmptyStr
  else if (ExtractFileDrive(AValue) <> '') or (AValue[1] = PathDelim) then
    FLibLocation := AValue
  else
    FLibLocation := ExpandFileName(ExtractFilePath(ParamStr(0)) + AValue);
end;

procedure TRALDBModule.AnswerException(AResponse: TRALResponse; AException: Exception);
begin
  // the two branches were swapped: a plain exception answered 429 and a pool
  // timeout answered 408. the intent on record ("pool exhaustion now answers
  // HTTP 429 instead of 503") is pool -> 429 and anything else -> 500.
  AResponse.ContentType := rctTEXTPLAIN;
  if AException is ERALDBPoolTimeout then
  begin
    { RAL's own words, which say nothing of the database }
    AResponse.StatusCode := HTTP_TooManyRequests;
    AResponse.Params.AddParam('Exception', AException.Message, rpkBODY);
  end
  else if AException is ERALDBProcError then
  begin
    { RAL's own words too: a refused or unknown procedure }
    AResponse.StatusCode := ERALDBProcError(AException).StatusCode;
    AResponse.Params.AddParam('Exception', AException.Message, rpkBODY);
  end
  else
  begin
    { the driver's, which names tables and quotes SQL: hidden with the
      server's HideErrorDetails }
    AResponse.StatusCode := HTTP_InternalError;
    AResponse.Params.AddParam('Exception', ErrorText(AException), rpkBODY);
  end;
end;

function TRALDBModule.CreateDBRoute(const ARoute: StringRAL;
  AReplyProc: TRALDBOnReply; const ADescription: StringRAL): TRALDBRoute;
begin
  Result := TRALDBRoute(NewRoute(ARoute, ADescription));
  Result.OnDBReply := AReplyProc;
end;

procedure TRALDBModule.ExecuteContext(ARoute: TRALRoute; ARequest: TRALModuleRequest;
  AResponse: TRALModuleResponse);
begin
  if (ARoute is TRALDBRoute) and Assigned(TRALDBRoute(ARoute).OnDBReply) then
  begin
    { the pool binds the connection to both sides of the request }
    TRALDBRequest(ARequest).FCoreResponse := AResponse.Core;
    TRALDBRoute(ARoute).OnDBReply(TRALDBRequest(ARequest), TRALDBResponse(AResponse));
  end
  else
  begin
    inherited;
  end;
end;

function TRALDBModule.HandleException(ARequest: TRALRequest;
  AResponse: TRALResponse; AException: Exception): boolean;
begin
  Result := (ARequest.ResolvedRoute is TRALDBRoute) and
    Assigned(TRALDBRoute(ARequest.ResolvedRoute).OnDBReply);
  if Result then
    AnswerException(AResponse, AException);
end;

class function TRALDBModule.RequestClass: TRALModuleRequestClass;
begin
  Result := TRALDBRequest;
end;

class function TRALDBModule.ResponseClass: TRALModuleResponseClass;
begin
  Result := TRALDBResponse;
end;

class function TRALDBModule.RouteClass: TRALRouteClass;
begin
  Result := TRALDBRoute;
end;

procedure TRALDBModule.ServerActivating;
begin
  inherited;
  if FPool.Options.PrepareOnActivate then
    FPool.Prepare;
end;

function TRALDBModule.CreatePoolConnection(ASender: TObject): TRALDBBase;
begin
  { the pool binds Request and Response itself on every acquire, so the driver is
    built here without them }
  Result := FindDatabaseDriver(nil, nil);
end;

function TRALDBModule.GetPoolOptions: TRALDBPoolOptions;
begin
  Result := FPool.Options;
end;

procedure TRALDBModule.SetPoolOptions(AValue: TRALDBPoolOptions);
begin
  FPool.Options.Assign(AValue);
end;

function TRALDBModule.AcquireDatabase(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALDBBase;
begin
  Result := FPool.Acquire(ARequest, AResponse);
end;

procedure TRALDBModule.ReleaseDatabase(ADatabase: TRALDBBase);
begin
  FPool.Release(ADatabase);
end;

procedure TRALDBModule.SetProcOptions(AValue: TRALDBProcOptions);
begin
  FProcOptions.Assign(AValue);
end;

procedure TRALDBModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (FProcOptions <> nil) and
     (AComponent = FProcOptions.Storage) then
    FProcOptions.FStorage := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TRALDBModule.UpdateProcRoute;
var
  vParam: TRALRouteParam;
begin
  if csDestroying in ComponentState then
    Exit;
  FreeAndNil(FProcRoute);
  if not FProcOptions.Enabled then
    Exit;

  FProcRoute := CreateDBRoute(FProcOptions.Route + '/:' + cProcNameParam,
    {$IFDEF FPC}@{$ENDIF}ExecProcRoute);
  FProcRoute.Name := FProcOptions.Route;
  FProcRoute.AllowedMethods := [amPOST, amOPTIONS];
  FProcRoute.Description.Add(cmDBProcDescription);

  vParam := TRALRouteParam(FProcRoute.URIParams.Add);
  vParam.Description.Text := cmDBProcName;
  vParam.ParamName := cProcNameParam;
  vParam.ParamType := prtString;
  vParam.Required := True;
end;

procedure TRALDBModule.RefreshProcedures;
begin
  FCatalogLock.Enter;
  try
    while FCatalog.Count > 0 do
    begin
      FCatalog.Objects[FCatalog.Count - 1].Free;
      FCatalog.Delete(FCatalog.Count - 1);
    end;
  finally
    FCatalogLock.Leave;
  end;
end;

function TRALDBModule.CachedProc(ADatabase: TRALDBBase; const AName: StringRAL;
  AParamNames: TStrings): TRALDBProcInfo;
var
  vKey: string;
  vNames: TStringList;
  vPos: IntegerRAL;
  vItem: TRALDBProcCacheItem;
  vInfo: TRALDBProcInfo;
begin
  Result := nil;
  // the names choose between PostgreSQL overloads: part of the key
  vNames := TStringList.Create;
  try
    if AParamNames <> nil then
      vNames.Assign(AParamNames);
    vNames.Sort;
    vKey := UpperCase(string(AName)) + '|' + UpperCase(vNames.CommaText);
  finally
    vNames.Free;
  end;

  FCatalogLock.Enter;
  try
    vPos := FCatalog.IndexOf(vKey);
    if vPos >= 0 then
    begin
      vItem := TRALDBProcCacheItem(FCatalog.Objects[vPos]);
      if (FProcOptions.CatalogLife > 0) and
         (Now - vItem.Stamp < FProcOptions.CatalogLife / MinsPerDay) then
      begin
        Result := TRALDBProcInfo.Create;
        Result.Assign(vItem.Info);
        Exit;
      end;
      vItem.Free;
      FCatalog.Delete(vPos);
    end;
  finally
    FCatalogLock.Leave;
  end;

  // the database answers outside the lock; a procedure it does not know is not kept
  vInfo := ADatabase.GetProcInfo(AName, AParamNames);
  if vInfo = nil then
    Exit;
  Result := vInfo;
  if FProcOptions.CatalogLife <= 0 then
    Exit;

  vItem := TRALDBProcCacheItem.Create;
  vItem.Info := TRALDBProcInfo.Create;
  vItem.Info.Assign(vInfo);
  vItem.Stamp := Now;
  FCatalogLock.Enter;
  try
    vPos := FCatalog.IndexOf(vKey);
    if vPos >= 0 then
    begin
      FCatalog.Objects[vPos].Free;
      FCatalog.Objects[vPos] := vItem;
    end
    else
      FCatalog.AddObject(vKey, vItem);
  finally
    FCatalogLock.Leave;
  end;
end;

function TRALDBModule.CheckProc(ADatabase: TRALDBBase; ARequest: TRALRequest;
  const AName: StringRAL; AParams: TParams): TRALDBProcInfo;
var
  vNames: TStringList;
  vInt: IntegerRAL;
  vProcName, vSchema: StringRAL;
  vProcParam, vParam: TParam;
  vAllow: boolean;
begin
  if not FProcOptions.Enabled then
    raise ERALDBProcError.Create(HTTP_Forbidden, emDBProcDisabled);
  if not ValidProcName(AName) then
    raise ERALDBProcError.Create(HTTP_BadRequest, emDBInvalidProcName);

  vProcName := AName;
  vInt := Pos(StringRAL('.'), vProcName);
  if vInt > 0 then
    Delete(vProcName, 1, vInt);
  if not FProcOptions.AllowsName(vProcName) then
    raise ERALDBProcError.Create(HTTP_NotFound, Format(emDBProcNotFound, [string(AName)]));

  vNames := TStringList.Create;
  try
    for vInt := 0 to Pred(AParams.Count) do
      if AParams.Items[vInt].ParamType in [ptUnknown, ptInput, ptInputOutput] then
        vNames.Add(AParams.Items[vInt].Name);
    Result := CachedProc(ADatabase, AName, vNames);
  finally
    vNames.Free;
  end;
  if Result = nil then
    raise ERALDBProcError.Create(HTTP_NotFound, Format(emDBProcNotFound, [string(AName)]));

  try
    vSchema := Result.Schema;
    if (vSchema <> '') and not FProcOptions.AllowsSchema(vSchema) then
      raise ERALDBProcError.Create(HTTP_NotFound, Format(emDBProcNotFound, [string(AName)]));

    // what came as text takes the catalog's type; the direction is the catalog's
    for vInt := 0 to Pred(Result.Params.Count) do
    begin
      vProcParam := Result.Params.Items[vInt];
      vParam := AParams.FindParam(vProcParam.Name);
      if vParam = nil then
        Continue;
      TypeParam(vParam, vProcParam.DataType);
      vParam.ParamType := vProcParam.ParamType;
    end;

    if Assigned(FOnValidateProc) then
    begin
      vAllow := True;
      FOnValidateProc(Self, ARequest, Result, AParams, vAllow);
      if not vAllow then
        raise ERALDBProcError.Create(HTTP_Forbidden, emDBProcRejected);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TRALDBModule.SaveResultSet(ADatabase: TRALDBBase; AQuery: TDataSet;
  AClientDriver: TRALDBDriverType; AStorage: TRALStorageLink; out ANative: boolean;
  out AContentType: StringRAL): TMemoryStream;
begin
  // the link has the say when there is one: it knows which dataset reads the answer
  ANative := ADatabase.CanExportNative;
  if ANative then
  begin
    if AStorage <> nil then
      ANative := AStorage.AllowNative(Ord(ADatabase.DriverType), Ord(AClientDriver))
    else
      ANative := AClientDriver = ADatabase.DriverType;
  end;

  Result := TMemoryStream.Create;
  try
    if ANative then
    begin
      if AStorage <> nil then
        AContentType := AStorage.NativeContentType
      else
        AContentType := rctAPPLICATIONOCTETSTREAM;
      ADatabase.SaveToStream(AQuery, Result, AContentType, ANative);
    end
    else
    begin
      { Answering without the native format takes the client's storage link,
        and the client does not always send one: TRALDBSQLCache.CreateStorage
        leaves it nil for rsfAuto, which is what a dataset with no Storage
        assigned asks for. Using it blindly killed the whole server with an
        access violation on an ordinary select - and the quickest way to get
        here is a client on the other compiler, because a driver that differs
        from the server's never takes the native path.

        Raising is the honest answer: the client cannot read this response
        either (LoadFromRALStorage raises emStorageClassNotFound with no link),
        so inventing a format here would only move the failure. }
      if AStorage = nil then
        raise Exception.Create(emStorageNotFound);

      ANative := False;
      AContentType := AStorage.ContentType;
      AStorage.SaveToStream(AQuery, Result);
    end;
    Result.Position := 0;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TRALDBModule.ProcSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL;
  AStorage: TRALStorageLink);
var
  vInfo: TRALDBProcInfo;
  vQuery: TDataSet;
  vRows: Int64RAL;
  vInt: IntegerRAL;
  vStream: TMemoryStream;
  vNative, vFirst: boolean;
  vContentType: StringRAL;
begin
  vInfo := CheckProc(ADatabase, ADatabase.Request, ADBSQL.SQL, ADBSQL.Params);
  try
    vRows := 0;
    vQuery := ADatabase.ExecProc(vInfo, ADBSQL.Params, vRows);
    try
      ADBSQL.Response.Native := False;
      ADBSQL.Response.ContentType := rctAPPLICATIONOCTETSTREAM;
      ADBSQL.Response.RowsAffected := vRows;
      ADBSQL.Response.LastId := 0;
      for vInt := 0 to Pred(ADBSQL.Params.Count) do
        if ADBSQL.Params.Items[vInt].ParamType in [ptOutput, ptInputOutput, ptResult] then
          TParam(ADBSQL.Response.OutParams.Add).Assign(ADBSQL.Params.Items[vInt]);

      { each result set as opensql answers one: native when the client's dataset
        is of the server's library, else in its storage }
      if vQuery <> nil then
      begin
        vFirst := True;
        repeat
          vStream := SaveResultSet(ADatabase, vQuery, ADBSQL.DriverType, AStorage, vNative,
            vContentType);
          if vFirst then
          begin
            ADBSQL.Response.Native := vNative;
            ADBSQL.Response.ContentType := vContentType;
            ADBSQL.Response.AdoptStream(vStream);
            vFirst := False;
          end
          else
            ADBSQL.Response.AddResultSet(vNative, vContentType, vStream);
        until not ADatabase.NextResultSet(vQuery);
      end;
    finally
      vQuery.Free;
    end;
  finally
    vInfo.Free;
  end;
end;

/// Runs ADBSQL as its ExecType says
procedure RunStatement(AModule: TRALDBModule; ADatabase: TRALDBBase; ADBSQL: TRALDBSQL;
  AStorage: TRALStorageLink);
begin
  case ADBSQL.ExecType of
    etExecute: AModule.ExecSQLResponse(ADatabase, ADBSQL, AStorage);
    etProc: AModule.ProcSQLResponse(ADatabase, ADBSQL, AStorage);
  else
    AModule.OpenSQLResponse(ADatabase, ADBSQL, AStorage);
  end;
end;

{ The first statement of ACache alone: committed when it worked, undone when it
  failed. Inside a transaction when AutoCommit is off or the client asked one }
procedure RunAlone(AModule: TRALDBModule; ADatabase: TRALDBBase; ACache: TRALDBSQLCache);
begin
  if (not AModule.AutoCommit) or ACache.Transaction then
    ADatabase.StartTransaction;
  try
    RunStatement(AModule, ADatabase, ACache.SQLList[0], ACache.Storage);
    ADatabase.Commit;
  except
    try
      ADatabase.Rollback;
    except
      // the error the client needs is the first one
    end;
    raise;
  end;
end;

{ The storage of the answer to a plain HTTP client: the one its Accept names, the
  module's ProcOptions.Storage, or JSON. AOwned when it was created here }
function ProcStorage(AModule: TRALDBModule; ARequest: TRALRequest;
  out AOwned: boolean): TRALStorageLink;
var
  vAccept: TStringList;
  vInt, vPos: IntegerRAL;
  vFormat: TRALStorageFormat;
  vClass: TRALStorageLinkClass;
  vType, vLinkType: string;
begin
  Result := nil;
  AOwned := True;
  vAccept := TStringList.Create;
  try
    vAccept.CommaText := LowerCase(string(ARequest.GetHeader('Accept')));
    for vInt := 0 to Pred(vAccept.Count) do
    begin
      vType := vAccept[vInt];
      vPos := Pos(';', vType);
      if vPos > 0 then
        vType := Copy(vType, 1, vPos - 1);
      vType := Trim(vType);
      if (vType = '') or (vType = '*/*') then
        Continue;
      for vFormat := Succ(rsfAuto) to High(TRALStorageFormat) do
      begin
        vClass := TRALStorageLink.GetStorageClass(vFormat);
        if vClass = nil then
          Continue;
        Result := vClass.Create(nil);
        vLinkType := LowerCase(string(Result.ContentType));
        vPos := Pos(';', vLinkType);
        if vPos > 0 then
          vLinkType := Copy(vLinkType, 1, vPos - 1);
        if Trim(vLinkType) = vType then
          Exit;
        FreeAndNil(Result);
      end;
    end;
  finally
    vAccept.Free;
  end;

  if AModule.ProcOptions.Storage <> nil then
  begin
    AOwned := False;
    Result := AModule.ProcOptions.Storage;
    Exit;
  end;

  vClass := TRALStorageLink.GetStorageClass(rsfJSON);
  if vClass = nil then
    raise Exception.Create(emStorageNotFound);
  Result := vClass.Create(nil);
end;

/// The output params of AParams as a JSON object, for a driver without an in-memory dataset
function OutputsJSON(AParams: TParams): TStream;
var
  vJSON: TRALJSONObject;
  vInt: IntegerRAL;
  vParam: TParam;
begin
  vJSON := TRALJSONObject.Create;
  try
    for vInt := 0 to Pred(AParams.Count) do
    begin
      vParam := AParams.Items[vInt];
      if not (vParam.ParamType in [ptOutput, ptInputOutput, ptResult]) then
        Continue;
      if vParam.IsNull then
        vJSON.Add(StringRAL(vParam.Name))
      else
        case vParam.DataType of
          ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
            vJSON.Add(StringRAL(vParam.Name), Int64RAL(vParam.AsLargeInt));
          ftFloat, ftCurrency:
            vJSON.Add(StringRAL(vParam.Name), DoubleRAL(vParam.AsFloat));
          ftBoolean:
            vJSON.Add(StringRAL(vParam.Name), vParam.AsBoolean);
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            vJSON.Add(StringRAL(vParam.Name), RALDateTimeToISO8601(vParam.AsDateTime, False));
        else
          vJSON.Add(StringRAL(vParam.Name), StringRAL(vParam.AsString));
        end;
    end;
    Result := StringToStreamUTF8(vJSON.ToJson);
  finally
    vJSON.Free;
  end;
end;

procedure TRALDBModule.ExecProcRoute(ARequest: TRALDBRequest; AResponse: TRALDBResponse);
var
  vDB: TRALDBBase;
  vName: StringRAL;
  vCache: TRALDBSQLCache;
  vParams: TParams;
  vParam: TParam;
  vRALParam: TRALParam;
  vJSON: TRALJSONValue;
  vValue: TRALJSONValue;
  vInfo: TRALDBProcInfo;
  vQuery, vOut: TDataSet;
  vRows: Int64RAL;
  vInt, vData: IntegerRAL;
  vStorage: TRALStorageLink;
  vOwned, vNative: boolean;
  vParts: TStringList;
  vStream: TStream;
  vContentType, vBody: StringRAL;
  vEncoder: TRALMultipartEncoder;
begin
  vName := ARequest.ParamByName(cProcNameParam).AsString;
  vDB := ARequest.Database;

  { a RAL dataset sends the call as a DBWare cache (etProc), and reads the answer
    in the same envelope: outputs and every result set }
  if Pos(StringRAL(rctAPPLICATIONOCTETSTREAM), LowerCase(ARequest.Core.ContentType)) > 0 then
  begin
    vCache := ARequest.SQLCache;
    if (vCache.Count = 0) or (vCache.SQLList[0].ExecType <> etProc) or
       not SameText(string(vCache.SQLList[0].SQL), string(vName)) then
      raise ERALDBProcError.Create(HTTP_BadRequest, Format(emDBProcNotFound, [string(vName)]));
    vCache.SQLList[0].Response.Clear;
    RunAlone(Self, vDB, vCache);
    AResponse.Answer(vCache);
    Exit;
  end;

  // a plain HTTP client: params from the query string, a form or a JSON object
  vParams := TParams.Create;
  vParts := TStringList.Create;
  vInfo := nil;
  vStorage := nil;
  vOwned := False;
  try
    for vInt := 0 to Pred(ARequest.Params.Count) do
    begin
      vRALParam := ARequest.Params.Index[vInt];
      if not (vRALParam.Kind in [rpkQUERY, rpkFIELD]) or
         (Pos(StringRAL('ral_'), LowerCase(vRALParam.ParamName)) = 1) or
         (vParams.FindParam(string(vRALParam.ParamName)) <> nil) then
        Continue;
      vParams.CreateParam(ftString, string(vRALParam.ParamName), ptInput).AsString :=
        string(vRALParam.AsString);
    end;

    { a JSON object, also from a client that labels it text/plain; a form or a
      multipart already came as fields }
    vBody := '';
    if Pos(StringRAL('multipart'), LowerCase(ARequest.Core.ContentType)) = 0 then
      vBody := StringRAL(Trim(string(ARequest.Core.RequestText)));
    if (Pos(StringRAL('json'), LowerCase(ARequest.Core.ContentType)) > 0) or
       ((vBody <> '') and (vBody[POSINISTR] = '{')) then
    begin
      if vBody <> '' then
      begin
        vJSON := TRALJSON.ParseJSON(vBody);
        try
          if not (vJSON is TRALJSONObject) then
            raise ERALDBProcError.Create(HTTP_BadRequest, emInvalidJSONFormat);
          for vInt := 0 to Pred(TRALJSONObject(vJSON).Count) do
          begin
            if vParams.FindParam(string(TRALJSONObject(vJSON).GetName(vInt))) <> nil then
              Continue;
            vValue := TRALJSONObject(vJSON).Get(vInt);
            vParam := vParams.CreateParam(ftString,
              string(TRALJSONObject(vJSON).GetName(vInt)), ptInput);
            if (vValue = nil) or vValue.IsNull then
              vParam.Clear
            else if vValue.JsonType in [rjtObject, rjtArray] then
              vParam.AsString := string(vValue.ToJson)
            else
              vParam.AsString := string(vValue.AsString);
          end;
        finally
          vJSON.Free;
        end;
      end;
    end;

    vInfo := CheckProc(vDB, ARequest.Core, vName, vParams);
    vStorage := ProcStorage(Self, ARequest.Core, vOwned);

    if (not FAutoCommit) then
      vDB.StartTransaction;
    try
      vRows := 0;
      vData := 0;
      vQuery := vDB.ExecProc(vInfo, vParams, vRows);
      try
        // everything is written before the commit, which may close the cursor
        vOut := vDB.OutputsDataset(vParams);
        if vOut <> nil then
          try
            vParts.AddObject('params=' + string(vStorage.ContentType),
              vStorage.SaveToStream(vOut));
          finally
            vOut.Free;
          end
        else if vInfo.HasOutput then
          vParts.AddObject('params=' + rctAPPLICATIONJSON, OutputsJSON(vParams));

        if vQuery <> nil then
          repeat
            vStream := SaveResultSet(vDB, vQuery, qtOther, vStorage, vNative, vContentType);
            Inc(vData);
            if vData = 1 then
              vParts.AddObject('data=' + string(vContentType), vStream)
            else
              vParts.AddObject('data' + IntToStr(vData) + '=' + string(vContentType), vStream);
          until not vDB.NextResultSet(vQuery);
      finally
        vQuery.Free;
      end;
      vDB.Commit;
    except
      try
        vDB.Rollback;
      except
        // the error the client needs is the first one
      end;
      raise;
    end;

    if vParts.Count = 0 then
      AResponse.Core.Answer(HTTP_NoContent)
    else if vParts.Count = 1 then
    begin
      vStream := TStream(vParts.Objects[0]);
      vParts.Objects[0] := nil;
      AResponse.Core.Answer(HTTP_OK, vStream, StringRAL(vParts.ValueFromIndex[0]), True);
    end
    else
    begin
      { one part per dataset, in order: params, data, data2... - each with its
        own Content-Type and Content-Disposition: inline; name="..." }
      vEncoder := TRALMultipartEncoder.Create;
      try
        vEncoder.MediaType := rctMULTIPARTMIXED;
        for vInt := 0 to Pred(vParts.Count) do
        begin
          vEncoder.AddPart(StringRAL(vParts.Names[vInt]), TStream(vParts.Objects[vInt]),
            StringRAL(vParts.ValueFromIndex[vInt]), True);
          vParts.Objects[vInt] := nil;
        end;
        AResponse.Core.Answer(HTTP_OK, vEncoder.AsStream, vEncoder.ContentType, True);
      finally
        vEncoder.Free;
      end;
    end;
  finally
    for vInt := 0 to Pred(vParts.Count) do
      vParts.Objects[vInt].Free;
    vParts.Free;
    if vOwned then
      vStorage.Free;
    vInfo.Free;
    vParams.Free;
  end;
end;

procedure TRALDBModule.CheckSQL(ARequest: TRALRequest; const ASQL: StringRAL);
var
  vAllow: Boolean;
begin
  if not Assigned(FOnValidateSQL) then
    Exit;
  vAllow := True;
  FOnValidateSQL(Self, ARequest, ASQL, vAllow);
  if not vAllow then
    raise Exception.Create(emDBSQLRejected);
end;

procedure TRALDBModule.OpenSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
var
  vQuery: TDataSet;
  vNative: Boolean;
  vContentType: StringRAL;
begin
  { the connection came from AcquireDatabase, which bound it to the request }
  CheckSQL(ADatabase.Request, ADBSQL.SQL);

  // the link has the say when there is one: it knows which dataset reads the answer
  vNative := ADatabase.CanExportNative;
  if vNative then
  begin
    if AStorage <> nil then
      vNative := AStorage.AllowNative(Ord(ADatabase.DriverType), Ord(ADBSQL.DriverType))
    else
      vNative := ADBSQL.DriverType = ADatabase.DriverType;
  end;

  if vNative or (ADBSQL.DriverType = ADatabase.DriverType) then
    vQuery := ADatabase.OpenNative(ADBSQL.SQL, ADBSQL.Params)
  else
    vQuery := ADatabase.OpenCompatible(ADBSQL.SQL, ADBSQL.Params);

  // the dataset belongs to us: the drivers create it without an owner
  try
    { handed over, not copied: Stream := held the result twice }
    ADBSQL.Response.AdoptStream(SaveResultSet(ADatabase, vQuery, ADBSQL.DriverType,
      AStorage, vNative, vContentType));
    ADBSQL.Response.Native := vNative;
    ADBSQL.Response.ContentType := vContentType;
    ADBSQL.Response.RowsAffected := 0;
    ADBSQL.Response.LastId := 0;
  finally
    FreeAndNil(vQuery);
  end;
end;

procedure TRALDBModule.ExecSQLResponse(ADatabase: TRALDBBase; ADBSQL: TRALDBSQL; AStorage: TRALStorageLink);
var
  vRowsAffect, vLastId: Int64RAL;
  vInt: IntegerRAL;
begin
  CheckSQL(ADatabase.Request, ADBSQL.SQL);
  ADatabase.ExecSQL(ADBSQL.SQL, ADBSQL.Params, vRowsAffect, vLastId);

  ADBSQL.Response.Native := False;
  ADBSQL.Response.ContentType := rctAPPLICATIONOCTETSTREAM;
  ADBSQL.Response.RowsAffected := vRowsAffect;
  ADBSQL.Response.LastId := vLastId;
  // a param marked as output - an execute procedure, a call - goes back
  for vInt := 0 to Pred(ADBSQL.Params.Count) do
    if ADBSQL.Params.Items[vInt].ParamType in [ptOutput, ptInputOutput, ptResult] then
      TParam(ADBSQL.Response.OutParams.Add).Assign(ADBSQL.Params.Items[vInt]);
end;

function TRALDBModule.GetInfoFieldsStream(ADatabase: TRALDBBase;
  ADataset: TDataSet; ABinary: boolean): TStream;
var
  vFields: TRALDBInfoFields;
  vInt: Integer;
  vField: TRALDBInfoField;
begin
  Result := nil; // both early exits and the binary path used to leave it undefined
  if not Assigned(ADataset) then exit;

  vFields:= TRALDBInfoFields.Create;
  try
    for vInt := 0 to Pred(ADataset.FieldCount) do
    begin
      vField := vFields.NewField;
      vField.TableName := ADatabase.GetFieldTable(ADataset, vInt);
      vField.FieldName := ADataset.Fields[vInt].FieldName;
      vField.FieldType := ADataset.Fields[vInt].DataType;
      vField.Flags := TRALDB.GetFieldProviderFlags(ADataset.Fields[vInt]);
      { which client gets this field in the native format - the one of this
        driver, when the driver exports natively at all (see OpenSQLResponse).
        A client building fields from the schema needs to know, or the ones it
        builds will not match what it loads }
      if ADatabase.CanExportNative then
        vField.NativeDriver := Ord(ADatabase.DriverType);

      vField.Length := 0;
      vField.Precision := 0;

      { a decimal's Size is its scale; its precision is a property of its own.
        Precision carried the scale and Scale stayed 0, which no client could
        rebuild an exact decimal field from }
      if ADataset.Fields[vInt].DataType in [ftBCD, ftFMTBcd] then
      begin
        vField.Precision := RALFieldPrecision(ADataset.Fields[vInt]);
        vField.Scale := ADataset.Fields[vInt].Size;
      end
      else
        vField.Length := ADataset.Fields[vInt].Size
    end;

    if not ABinary then
      Result := StringToStreamUTF8(vFields.AsJSON);
  finally
    FreeAndNil(vFields);
  end;
end;

function TRALDBModule.FindDatabaseDriver(ARequest: TRALRequest; AResponse: TRALResponse): TRALDBBase;
var
  vClass: TRALDBClass;
  vUnit: StringRAL;
begin
  Result := nil;
  vClass := nil;

  vClass := GetDatabaseClass(FDatabaseLink);

  if vClass <> nil then
  begin
    Result := vClass.Create;
    Result.DatabaseType := FDatabaseType;
    Result.Database := FDatabase;
    Result.Hostname := FHostname;
    Result.Username := FUsername;
    Result.Password := FPassword;
    Result.Port := FPort;
    Result.LibLocation := FLibLocation;
    Result.CharacterSet := FCharacterSet;
    if FConnectionParams.Count > 0 then
      Result.ConnectionParams := FConnectionParams;
    Result.Request := ARequest;
    Result.Response := AResponse;

    Result.OnBeforeConnect := FOnBeforeConnect;
    Result.OnAfterConnect := FOnAfterConnect;
    Result.OnErrorConnect := FOnErrorConnect;
    Result.OnErrorQuery := FOnErrorQuery;
  end
  else
  begin
    raise Exception.Create(emDBLinkMissing);
  end;
end;

procedure TRALDBModule.OpenSQL(ARequest: TRALDBRequest; AResponse: TRALDBResponse);
var
  vDB: TRALDBBase;
  vSQLCache: TRALDBSQLCache;
begin
  { the connection first: an exhausted pool answers 429 before the body is read }
  vDB := ARequest.Database;
  vSQLCache := ARequest.SQLCache;
  RunAlone(Self, vDB, vSQLCache);
  AResponse.Answer(vSQLCache);
end;

procedure TRALDBModule.ApplyUpdates(ARequest: TRALDBRequest;
  AResponse: TRALDBResponse);
var
  vDB: TRALDBBase;
  vSQLCache: TRALDBSQLCache;
  vDBSQL: TRALDBSQL;
  vInt, vFailed: IntegerRAL;
  vTransaction: boolean;
  vError: StringRAL;
begin
  vDB := ARequest.Database;
  vSQLCache := ARequest.SQLCache;
  { all or nothing when AutoCommit is off or the client asked it: the first
    failure undoes the others, and nothing stays open past the request }
  vTransaction := (not FAutoCommit) or vSQLCache.Transaction;
  if vTransaction then
    vDB.StartTransaction;
  vFailed := -1;
  for vInt := 0 to Pred(vSQLCache.Count) do
  begin
    vDBSQL := vSQLCache.SQLList[vInt];
    vDBSQL.Response.Clear;

    try
      RunStatement(Self, vDB, vDBSQL, vSQLCache.Storage);
      if not vTransaction then
        vDB.Commit;
    except
      { each statement's own failure, the driver's words - hidden like a
        500's with the server's HideErrorDetails }
      on e: Exception do
      begin
        vDBSQL.Response.StrError := ErrorText(e);
        if vTransaction then
        begin
          vFailed := vInt;
          Break;
        end;
        { each statement answers for itself: one failing does not lose the others }
        try
          vDB.Rollback;
        except
        end;
      end;
    end;
  end;

  if vTransaction then
  begin
    if vFailed >= 0 then
    begin
      try
        vDB.Rollback;
      except
      end;
      for vInt := 0 to Pred(vSQLCache.Count) do
        if vInt <> vFailed then
          vSQLCache.SQLList[vInt].Response.StrError := emDBRolledBack;
    end
    else
      try
        vDB.Commit;
      except
        on e: Exception do
        begin
          vError := ErrorText(e);
          for vInt := 0 to Pred(vSQLCache.Count) do
            vSQLCache.SQLList[vInt].Response.StrError := vError;
          try
            vDB.Rollback;
          except
          end;
        end;
      end;
  end;
  AResponse.Answer(vSQLCache);
end;

procedure TRALDBModule.ExecSQL(ARequest: TRALDBRequest; AResponse: TRALDBResponse);
var
  vDB: TRALDBBase;
  vSQLCache: TRALDBSQLCache;
  vDBSQL: TRALDBSQL;
begin
  vDB := ARequest.Database;
  vSQLCache := ARequest.SQLCache;
  vDBSQL := vSQLCache.SQLList[0];
  vDBSQL.Response.Clear;
  RunAlone(Self, vDB, vSQLCache);
  AResponse.Answer(vSQLCache);
end;

procedure TRALDBModule.GetTables(ARequest: TRALDBRequest; AResponse: TRALDBResponse);
var
  vDB: TRALDBBase;
  vSQL: TStringList;
  vSchema: StringRAL;
  vSystem: boolean;
  vQuery: TDataSet;
  vJSON: TRALJSONArray;
  vjObj: TRALJSONObject;
begin
  vDB := ARequest.Database;
  vSchema := ARequest.ParamByName('schema').AsString;
  vSystem := ARequest.ParamByName('system').AsBoolean;
  vQuery := nil;
  vJSON := nil;

  vSQL := TStringList.Create;
  try
    case FDatabaseType of
      dtFirebird : begin
        vSQL.Add('select rdb$relation_name, rdb$system_flag from rdb$relations');
        if not vSystem then
          vSQL.Add('where rdb$system_flag = 0');
        vSQL.Add('order by rdb$relation_name');
      end;
      dtSQLite: begin
        vSQL.Add('select name from sqlite_master');
        vSQL.Add('where type = ''table''');
      end;
      dtMySQL: begin
        vSQL.Add('show tables');
      end;
      dtPostgreSQL : begin
        vSQL.Add('select c.relname,');
        vSQL.Add('       case');
        vSQL.Add('         when (n.nspname = ''information_schema'') or');
        vSQL.Add('              (n.nspname = ''pg_catalog'') or (n.nspname = ''dbo'') or');
        vSQL.Add('              (n.nspname = ''sys'') or');
        vSQL.Add('              (substr(c.relname, 1, 3) = ''pg_'') then 1');
        vSQL.Add('         else 0');
        vSQL.Add('       end as systable, n.nspname');
        vSQL.Add('from pg_catalog.pg_class c');
        vSQL.Add('inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace');
        vSQL.Add('where c.relkind = ''r''');
        if not vSystem then begin
          vSQL.Add('  and n.nspname <> ''information_schema'' and ');
          vSQL.Add('      n.nspname <> ''pg_catalog'' and n.nspname <> ''dbo'' and');
          vSQL.Add('      n.nspname <> ''sys'' and substr(c.relname, 1, 3) <> ''pg_''');
        end;
        if vSchema <> '' then
          vSQL.Add('  and lower(n.nspname) = '+QuotedStr(LowerCase(vSchema)));
      end;
    end;

    vQuery := vDB.OpenNative(vSQL.Text, nil);
    try
      vJSON := TRALJSONArray.Create;
      try
        if not vQuery.IsUniDirectional then
          vQuery.First;

        while not vQuery.Eof do begin
          vjObj := TRALJSONObject.Create;
          vjObj.Add('table_name', vQuery.Fields[0].AsString);
          if vQuery.FieldCount > 1 then
            vjObj.Add('system_table', vQuery.Fields[1].AsInteger = 1)
          else
            vjObj.Add('system_table', False);

          if vQuery.FieldCount > 2 then
            vjObj.Add('schema_name', vQuery.Fields[2].AsString)
          else
            vjObj.Add('schema_name', EmptyStr);

          vJSON.Add(vjObj);

          vQuery.Next;
        end;

        AResponse.Answer(HTTP_OK, vJSON.ToJson, rctAPPLICATIONJSON);
      finally
        FreeAndNil(vJSON);
      end;
    finally
      FreeAndNil(vQuery);
    end;
  finally
    FreeAndNil(vSQL);
  end;
end;

procedure TRALDBModule.GetFields(ARequest: TRALDBRequest; AResponse: TRALDBResponse);
var
  vDB: TRALDBBase;
  vSQL: TStringList;
  vSchema, vTable: StringRAL;
  vQuery: TDataSet;
  vFields: TRALDBInfoFields;
  vField: TRALDBInfoField;
  vInt: IntegerRAL;

  procedure AddFieldAttribute(AAttribute: StringRAL);
  begin
    if vField.Attributes <> EmptyStr then
      vField.Attributes := vField.Attributes + ',';
    vField.Attributes := vField.Attributes + AAttribute;
  end;

  procedure AssignOthersDateTypeField(AType: StringRAL);
  var
    vInt: IntegerRAL;
  begin
    AType := LowerCase(AType);
    if (Pos(StringRAL('varchar'), AType) > 0) or (Pos(StringRAL('char'), AType) > 0) then
    begin
      vField.RALFieldType := sftString;

      vInt := Pos('(', AType);
      if vInt > 0 then
      begin
        Delete(AType, 1, vInt);
        vInt := Pos(')', AType);
        vField.Length := StrToInt(Copy(AType, 1, vInt-1));
      end
      else begin
        vField.Length := 255;
      end;
    end
    else if (Pos(StringRAL('text'), AType) > 0) or (Pos(StringRAL('json'), AType) > 0) or
            (Pos(StringRAL('uuid'), AType) > 0) then
    begin
      vField.RALFieldType := sftMemo;
    end
    else if (Pos(StringRAL('binary'), AType) > 0) or (Pos(StringRAL('blob'), AType) > 0) then
    begin
      vField.RALFieldType := sftBlob;
    end
    else if (Pos(StringRAL('date'), AType) > 0) or (Pos(StringRAL('time'), AType) > 0) then
    begin
      vField.RALFieldType := sftDateTime;
    end
    else if (Pos(StringRAL('double'), AType) > 0) or (Pos(StringRAL('numeric'), AType) > 0) or
            (Pos(StringRAL('decimal'), AType) > 0) or (Pos(StringRAL('real'), AType) > 0) or
            (Pos(StringRAL('float'), AType) > 0) then
    begin
      // NUMERIC and DECIMAL are exact, the others are floats
      if (Pos(StringRAL('numeric'), AType) > 0) or (Pos(StringRAL('decimal'), AType) > 0) then
        vField.RALFieldType := RALDecimalFieldType
      else
        vField.RALFieldType := sftDouble;

      vInt := Pos('(', AType);
      if vInt > 0 then
      begin
        Delete(AType, 1, vInt);
        vInt := Pos(',', AType);

        vField.Precision := StrToInt(Copy(AType, 1, vInt-1));

        Delete(AType, 1, vInt);
        vInt := Pos(')', AType);

        vField.Scale := StrToInt(Copy(AType, 1, vInt-1));
      end
      else begin
        vField.Precision := 15;
        vField.Scale := 2;
      end;
    end
    else if (Pos(StringRAL('tinyint'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftByte
      else
        vField.RALFieldType := sftShortInt;
    end
    else if (Pos(StringRAL('smallint'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftWord
      else
        vField.RALFieldType := sftSmallint;
    end
    else if (Pos(StringRAL('bigint'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftQWord
      else
        vField.RALFieldType := sftInt64;
    end
    else if (Pos(StringRAL('int'), AType) > 0) or (Pos(StringRAL('integer'), AType) > 0) then
    begin
      if Pos(StringRAL('unsigned'), AType) > 0 then
        vField.RALFieldType := sftCardinal
      else
        vField.RALFieldType := sftInteger;
    end;
  end;

  procedure AssignFirebirdDateTypeField;
  var
    vfbType: TRALFieldType;
  begin
    { a type not listed below - INT128 and DECFLOAT among them - travels as
      text; vfbType had no value at all for them, and indexed the type name
      table with whatever it held }
    vfbType := sftString;
    case vQuery.FieldByName('rdb$field_type').AsInteger of
      { an integer with a sub type is a NUMERIC (1) or a DECIMAL (2) stored in
        it - exact, as RALDecimalFieldType carries it }
      007: begin
            vfbType := sftSmallInt;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := RALDecimalFieldType;
      end;
      008: begin
            vfbType := sftInteger;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := RALDecimalFieldType;
      end;
      009: vfbType := sftInt64;
      010,
      011,
      027: vfbType := sftDouble;
      012,
      013,
      035: vfbType := sftDateTime;
      014,
      037,
      040: vfbType := sftString;
      023: vfbType := sftBoolean;  // BOOLEAN, Firebird 3
      028,
      029: vfbType := sftDateTime; // TIME and TIMESTAMP WITH TIME ZONE, Firebird 4
      016: begin
            vfbType := sftInt64;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := RALDecimalFieldType;
      end;
      261: begin
        vfbType := sftBlob;
        if vQuery.FieldByName('rdb$field_sub_type').AsInteger = 1 then
          vfbType := sftMemo;
      end;
    end;

    vField.RALFieldType := vfbType;

    if vQuery.FieldByName('rdb$null_flag').AsString = '0' then
      AddFieldAttribute('not_null');

    if vQuery.FieldByName('pk').AsInteger > 0 then
      AddFieldAttribute('pk');

    if vQuery.FieldByName('rdb$field_type').AsInteger in [14, 37, 40] then
    begin
      vField.Length := vQuery.FieldByName('rdb$field_length').AsInteger;
      // field com charset e colation
      if (vQuery.FieldByName('rdb$character_length').AsInteger > 0) and
         (vQuery.FieldByName('rdb$character_length').AsInteger < vField.Length) then
        vField.Length := vQuery.FieldByName('rdb$character_length').AsInteger;
    end
    else if vQuery.FieldByName('rdb$field_type').AsInteger in [7, 8, 16, 27] then
    begin
      // numeric
      if vfbType in [sftDouble, sftBCD] then
        vField.Precision := vQuery.FieldByName('rdb$field_precision').AsInteger;

      if (vQuery.FieldByName('rdb$field_scale').AsInteger < 0) then
      begin
        vField.Precision := 15;
        if (vQuery.FieldByName('rdb$field_precision').AsInteger > 0) then
          vField.Precision := vQuery.FieldByName('rdb$field_precision').AsInteger;
        vField.Scale := Abs(vQuery.FieldByName('rdb$field_scale').AsInteger);
      end;
    end;
  end;

  procedure AssignPostgresDateTypeField;
  var
    vpgType: TRALFieldType;
    vType, vTypMod: IntegerRAL;
  begin
    if vQuery.FieldByName('attnotnull').AsBoolean then
      AddFieldAttribute('not_null');

    if vQuery.FieldByName('pk').AsInteger > 0 then
      AddFieldAttribute('pk');

    vType := vQuery.FieldByName('typbasetype').AsInteger; // campos com domains
    vTypMod := vQuery.FieldByName('typtypmod').AsInteger;
    if vType = 0 then
    begin
      vType := vQuery.FieldByName('atttypid').AsInteger; // campos sem domain
      vTypMod := vQuery.FieldByName('atttypmod').AsInteger;
    end;

    { an OID not listed below - uuid, json, jsonb among them - travels as text;
      vpgType had no value at all for them, same as Firebird above }
    vpgType := sftString;
    case vType of
      16   : vpgType := sftBoolean;
      17   : vpgType := sftBlob;
      21   : vpgType := sftSmallInt;
      23   : vpgType := sftInteger;
      25   : vpgType := sftMemo;
      20,
      26   : vpgType := sftInt64;
      700,
      701  : vpgType := sftDouble;
      1700 : vpgType := RALDecimalFieldType; // numeric
      1083 : vpgType := sftDateTime; // time
      1042,
      1043: vpgType := sftString;
      1082,
      1114,
      1184 : vpgType := sftDateTime;
    end;

    vField.RALFieldType := vpgType;

    if (vType <> 1700) and (vTypMod > 0) then begin
      vField.Length := vTypMod - 4;
      if vField.Length < 0 then
        vField.Length := 0;
    end
    else if (vType = 1700) then begin
      vField.Precision := (vTypMod - 4) mod 65536;
      vField.Scale := (vTypMod - 4) div 65536;
    end;
  end;

begin
  vDB := ARequest.Database;
  vSchema := ARequest.ParamByName('schema').AsString;
  vTable := ARequest.ParamByName('table').AsString;
  vQuery := nil;

  { the name goes into the SQL text on SQLite and MySQL, so only an
    identifier is accepted - anything else was a SQL injection }
  for vInt := POSINISTR to RALHighStr(vTable) do
    if not (vTable[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$', '.']) then
      raise Exception.Create(emDBInvalidTableName);

  vSQL := TStringList.Create;
  try
    case FDatabaseType of
      dtFirebird: begin
        vTable := UpperCase(vTable);

        vSQL.Add('select f.rdb$field_type, f.rdb$field_sub_type, f.rdb$field_length,');
        vSQL.Add('       f.rdb$character_length, f.rdb$field_precision,');
        vSQL.Add('       f.rdb$field_scale, rf.rdb$field_name, rf.rdb$null_flag,');
        vSQL.Add('       rf.rdb$default_source, cs.rdb$character_set_name,');
        vSQL.Add('       cl.rdb$collation_name, fd.rdb$lower_bound, fd.rdb$upper_bound,');
        vSQL.Add('      (select count(*) as conta');
        vSQL.Add('       from rdb$relation_constraints c');
        vSQL.Add('       inner join rdb$index_segments s on s.rdb$index_name = c.rdb$index_name');
        vSQL.Add('       where c.rdb$relation_name = rf.rdb$relation_name and');
        vSQL.Add('             s.rdb$field_name = rf.rdb$field_name and');
        vSQL.Add('             c.rdb$constraint_type = ''PRIMARY KEY'') as pk');
        vSQL.Add('from rdb$fields f');
        vSQL.Add('left join rdb$relation_fields rf on rf.rdb$field_source = f.rdb$field_name');
        vSQL.Add('left join rdb$character_sets cs on cs.rdb$character_set_id = f.rdb$character_set_id');
        vSQL.Add('left join rdb$collations cl on cl.rdb$character_set_id = f.rdb$character_set_id and');
        vSQL.Add('     cl.rdb$collation_id = coalesce(f.rdb$collation_id,rf.rdb$collation_id)');
        vSQL.Add('left join rdb$field_dimensions fd on fd.rdb$field_name = f.rdb$field_name');
        vSQL.Add('where rf.rdb$relation_name = ' + QuotedStr(vTable));
        vSQL.Add('order by rf.rdb$field_position');
      end;
      dtSQLite: begin
        vSQL.Add('pragma table_info(' + vTable + ')');
      end;
      dtMySQL: begin
        vSQL.Add('show columns from ' + vTable);
      end;
      dtPostgreSQL: begin
        vSQL.Add('select t.typbasetype, t.typtypmod, a.atttypid, a.atttypmod,');
        vSQL.Add('       a.attname, a.attnotnull, n.nspname,');
        vSQL.Add('       pg_get_expr(d.adbin, d.adrelid) as pg_default,');
        vSQL.Add('	    (select count(*) from pg_catalog.pg_index i  ');
        vSQL.Add('       inner join pg_catalog.pg_attribute aa on aa.attrelid = i.indrelid and');
        vSQL.Add('		         aa.attnum = any(i.indkey) and aa.attname = a.attname');
        vSQL.Add('		   where i.indrelid = c.oid and i.indisprimary) as pk');
        vSQL.Add('from pg_catalog.pg_class c');
        vSQL.Add('inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace');
        vSQL.Add('inner join pg_catalog.pg_attribute a on a.attrelid = c.oid');
        vSQL.Add('inner join pg_catalog.pg_type t on a.atttypid = t.oid');
        vSQL.Add('left join pg_catalog.pg_attrdef d on d.adnum = a.attnum and d.adrelid = c.oid');
        vSQL.Add('where a.attnum > 0 and not a.attisdropped and');
        vSQL.Add('      lower(c.relname) = ' + QuotedStr(LowerCase(vTable)));
        if vSchema <> EmptyStr then
          vSQL.Add('  and lower(n.nspname) = ' + QuotedStr(LowerCase(vSchema)));
      end;
    end;

    vQuery := vDB.OpenNative(vSQL.Text, nil);
    try
      vFields := TRALDBInfoFields.Create;
      try
        if not vQuery.IsUniDirectional then
          vQuery.First;

        while not vQuery.Eof do begin
          vField := vFields.NewField;

          case FDatabaseType of
            dtFirebird: begin
              vField.FieldName := vQuery.FieldByName('rdb$field_name').AsString;
              AssignFirebirdDateTypeField;
            end;
            dtSQLite: begin
              vField.FieldName := vQuery.Fields[1].AsString;
              AssignOthersDateTypeField(vQuery.Fields[2].AsString);
              if vQuery.Fields[3].AsInteger = 1 then
                AddFieldAttribute('not_null');
              if vQuery.Fields[5].AsInteger = 1 then
                AddFieldAttribute('pk');
            end;
            dtMySQL: begin
              vField.FieldName := vQuery.Fields[0].AsString;
              AssignOthersDateTypeField(vQuery.Fields[1].AsString);
              if vQuery.Fields[2].AsString = 'NO' then
                AddFieldAttribute('not_null');
              if vQuery.Fields[3].AsString = 'PRI' then
                AddFieldAttribute('pk');
            end;
            dtPostgreSQL : begin
              vField.FieldName := vQuery.FieldByName('attname').AsString;
              vField.Schema := vQuery.FieldByName('nspname').AsString;
              AssignPostgresDateTypeField;
            end;
          end;

          vQuery.Next;
        end;

        AResponse.Answer(HTTP_OK, vFields.AsJSON, rctAPPLICATIONJSON);
      finally
        FreeAndNil(vFields);
      end;
    finally
      FreeAndNil(vQuery);
    end;
  finally
    FreeAndNil(vSQL);
  end;
end;

procedure TRALDBModule.GetSQLFields(ARequest: TRALDBRequest;
  AResponse: TRALDBResponse);
var
  vDB: TRALDBBase;
  vSQL: StringRAL;
  vQuery: TDataSet;
  vResult: TStream;
begin
  vDB := ARequest.Database;
  vSQL := ARequest.ParamByName('ral_body').AsString;
  CheckSQL(ARequest.Core, vSQL);
  vQuery := vDB.OpenNative(vSQL, nil);
  try
    vResult := GetInfoFieldsStream(vDB, vQuery, False);
    try
      AResponse.Answer(HTTP_OK, vResult, rctAPPLICATIONJSON);
    finally
      FreeAndNil(vResult);
    end;
  finally
    FreeAndNil(vQuery);
  end;
end;

constructor TRALDBModule.Create(AOwner: TComponent);
var
  vRoute: TRALRoute;
  vParam: TRALRouteParam;
begin
  inherited Create(AOwner);

  FAutoCommit := True;
  FCatalog := TStringList.Create;
  FCatalog.Sorted := True;
  FCatalogLock := TCriticalSection.Create;
  FConnectionParams := TStringList.Create;
  FProcOptions := TRALDBProcOptions.Create(Self);
  FPool := TRALDBConnectionPool.Create;
  FPool.OnCreateConnection := {$IFDEF FPC}@{$ENDIF}CreatePoolConnection;

  vRoute := CreateDBRoute('opensql', {$IFDEF FPC}@{$ENDIF}OpenSQL);
  vRoute.Name := 'opensql';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBOpenSQLDescription);

  vRoute := CreateDBRoute('execsql', {$IFDEF FPC}@{$ENDIF}ExecSQL);
  vRoute.Name := 'execsql';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBExecSQLDescription);

  vRoute := CreateDBRoute('applyupdates', {$IFDEF FPC}@{$ENDIF}ApplyUpdates);
  vRoute.Name := 'applyupdates';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBApplyUpdDescription);

  vRoute := CreateDBRoute('gettables', {$IFDEF FPC}@{$ENDIF}GetTables);
  vRoute.Name := 'gettables';
  vRoute.AllowedMethods := [amGET, amOPTIONS];
  vRoute.Description.Add(cmDBGetTablesDescription);

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBParamSchemaDescription;
  vParam.ParamName := 'schema';
  vParam.ParamType := prtString;
  vParam.Required := False;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBParamSystemDescription;
  vParam.ParamName := 'system';
  vParam.ParamType := prtBoolean;
  vParam.Required := False;

  vRoute := CreateDBRoute('getfields', {$IFDEF FPC}@{$ENDIF}GetFields);
  vRoute.Name := 'getfields';
  vRoute.AllowedMethods := [amGET, amOPTIONS];
  vRoute.Description.Add(cmDBGetFieldsDescription);

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBParamSchemaDescription;
  vParam.ParamName := 'schema';
  vParam.ParamType := prtString;
  vParam.Required := False;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBTableName;
  vParam.ParamName := 'table';
  vParam.ParamType := prtString;
  vParam.Required := True;

  vRoute := CreateDBRoute('getsqlfields', {$IFDEF FPC}@{$ENDIF}GetSQLFields);
  vRoute.Name := 'getsqlfields';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add(cmDBGetSQLFieldsDescription);

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBSQL;
  vParam.ParamName := 'sql';
  vParam.ParamType := prtString;
  vParam.Required := True;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := cmDBBinary;
  vParam.ParamName := 'binary';
  vParam.ParamType := prtBoolean;
  vParam.Required := False;
end;

procedure TRALDBModule.SetConnectionParams(AValue: TStrings);
begin
  FConnectionParams.Assign(AValue);
end;

destructor TRALDBModule.Destroy;
begin
  RefreshProcedures;
  FreeAndNil(FCatalog);
  FreeAndNil(FCatalogLock);
  // the route belongs to the routes of the module, freed with it
  FProcRoute := nil;
  FreeAndNil(FProcOptions);
  FreeAndNil(FPool);
  FreeAndNil(FConnectionParams);
  inherited Destroy;
end;

end.
