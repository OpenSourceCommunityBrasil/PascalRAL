unit RALResponsePages;

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALConsts;

type
  { TRALPageCode }

  TRALResponsePage = class(TCollectionItem)
  private
    FPage: TStrings;
    FStatusCode: IntegerRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure SetPage(const AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Page: TStrings read FPage write SetPage;
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode;
  end;

  { TRALResponsePages }

  TRALResponsePages = class(TOwnedCollection)
  protected
    function GetHTMLPage(AStatusCode: IntegerRAL): StringRAL;
  public
    constructor Create(AOwner: TPersistent);

    procedure CreateDefaultPages;
    procedure CreatePage(AStatusCode: IntegerRAL; AHTMLPage: StringRAL = '');
    class function GetHTTPResponsePage(AStatusCode: IntegerRAL): StringRAL;

    property HTMLPage[AStatusCode: IntegerRAL] : StringRAL read GetHTMLPage;
  end;

/// The reason phrase of an HTTP status (RFC 9110), in English; '' for one it does not know.
function RALHTTPReason(AStatusCode: IntegerRAL): StringRAL;

implementation

function RALHTTPReason(AStatusCode: IntegerRAL): StringRAL;
begin
  case AStatusCode of
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Content Too Large';
    414: Result := 'URI Too Long';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Range Not Satisfiable';
    417: Result := 'Expectation Failed';
    421: Result := 'Misdirected Request';
    422: Result := 'Unprocessable Content';
    423: Result := 'Locked';
    424: Result := 'Failed Dependency';
    425: Result := 'Too Early';
    426: Result := 'Upgrade Required';
    428: Result := 'Precondition Required';
    429: Result := 'Too Many Requests';
    431: Result := 'Request Header Fields Too Large';
    451: Result := 'Unavailable For Legal Reasons';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'HTTP Version Not Supported';
    507: Result := 'Insufficient Storage';
    511: Result := 'Network Authentication Required';
  else
    Result := '';
  end;
end;

{ TRALResponsePages }

function TRALResponsePages.GetHTMLPage(AStatusCode: IntegerRAL): StringRAL;
var
  vInt: IntegerRAL;
  vPage: TRALResponsePage;
  vFind: boolean;
begin
  Result := '';
  vFind := False;
  for vInt := 0 to Pred(Count) do
  begin
    vPage := TRALResponsePage(Items[vInt]);
    if vPage.StatusCode = AStatusCode then
    begin
      Result := vPage.Page.Text;
      vFind := True;
      Break;
    end;
  end;

  if not vFind then
    Result := GetHTTPResponsePage(AStatusCode);
end;

constructor TRALResponsePages.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALResponsePage);
end;

procedure TRALResponsePages.CreatePage(AStatusCode: IntegerRAL; AHTMLPage: StringRAL);
var
  vPage: TRALResponsePage;
begin
  vPage := TRALResponsePage(Add);
  vPage.StatusCode := AStatusCode;
  if AHTMLPage <> '' then
    vPage.Page.Text := AHTMLPage
  else
    vPage.Page.Text := GetHTTPResponsePage(AStatusCode);
end;

procedure TRALResponsePages.CreateDefaultPages;
begin
  CreatePage(HTTP_BadRequest, Format(RALPage, [SLangHTTP, 400, SError400, SError400Page]));
  CreatePage(HTTP_Unauthorized, Format(RALPage, [SLangHTTP, 401, SError401, SError401Page]));
  CreatePage(HTTP_Forbidden, Format(RALPage, [SLangHTTP, 403, SError403, SError403Page]));
  CreatePage(HTTP_NotFound, Format(RALPage, [SLangHTTP, 404, SError404, SError404Page]));
  CreatePage(HTTP_NotAcceptable, Format(RALPage, [SLangHTTP, 406, SError406, SError406Page]));
  CreatePage(HTTP_UnsupportedMedia, Format(RALPage, [SLangHTTP, 415, SError415, SError415Page]));
  CreatePage(HTTP_TooManyRequests, Format(RALPage, [SLangHTTP, 429, SError429, SError429Page]));
  CreatePage(HTTP_InternalError, Format(RALPage, [SLangHTTP, 500, SError500, SError500Page]));
  CreatePage(HTTP_NotImplemented, Format(RALPage, [SLangHTTP, 501, SError501, SError501Page]));
  CreatePage(HTTP_ServiceUnavailable, Format(RALPage, [SLangHTTP, 503, SError503, SError503Page]));
end;

class function TRALResponsePages.GetHTTPResponsePage(AStatusCode: IntegerRAL): StringRAL;
begin
  Result := '';
  case AStatusCode of
    HTTP_BadRequest:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError400, SError400Page]);
    HTTP_Unauthorized:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError401, SError401Page]);
    HTTP_Forbidden:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError403, SError403Page]);
    HTTP_NotFound:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError404, SError404Page]);
    HTTP_NotAcceptable:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError406, SError406Page]);
    HTTP_UnsupportedMedia:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError415, SError415Page]);
    HTTP_TooManyRequests:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError429, SError429Page]);
    HTTP_InternalError:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError500, SError500Page]);
    HTTP_NotImplemented:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError501, SError501Page]);
    HTTP_ServiceUnavailable:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError503, SError503Page]);
  end;
end;

{ TRALResponsePage }

procedure TRALResponsePage.AssignTo(Dest: TPersistent);
begin
  { TCollection.Assign copies item by item: without this, assigning
    ResponsePages raised }
  if Dest is TRALResponsePage then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

constructor TRALResponsePage.Create(ACollection: TCollection);
begin
  inherited;
  FStatusCode := 0;
  FPage := TStringList.Create;
end;

destructor TRALResponsePage.Destroy;
begin
  FreeAndNil(FPage);
  inherited;
end;

function TRALResponsePage.GetDisplayName: string;
begin
  Result := IntToStr(FStatusCode);
end;

procedure TRALResponsePage.SetPage(const AValue: TStrings);
begin
  FPage.Assign(AValue);
end;

end.
