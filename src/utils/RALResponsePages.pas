unit RALResponsePages;

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALConsts;

type
  { TRALPageCode }

  TRALResponsePage = class(TCollectionItem)
  private
    FStatusCode: IntegerRAL;
    FPage: TStrings;
  protected
    function GetDisplayName: string; override;

    procedure SetPage(const AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode;
    property Page: TStrings read FPage write SetPage;
  end;

  { TRALResponsePages }

  TRALResponsePages = class(TOwnedCollection)
  protected
    function GetHTMLPage(AStatusCode: IntegerRAL): StringRAL;
  public
    constructor Create(AOwner: TPersistent);

    procedure CreatePage(AStatusCode: IntegerRAL; AHTMLPage: StringRAL = '');
    procedure CreateDefaultPages;

    class function GetHTTPResponsePage(AStatusCode: IntegerRAL): StringRAL;
    property HTMLPage[AStatusCode: IntegerRAL] : StringRAL read GetHTMLPage;
  end;

implementation

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
  CreatePage(HTTP_UnsupportedMedia, Format(RALPage, [SLangHTTP, 415, SError415, SError415Page]));
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
    HTTP_UnsupportedMedia:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError415, SError415Page]);
    HTTP_InternalError:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError500, SError500Page]);
    HTTP_NotImplemented:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError501, SError501Page]);
    HTTP_ServiceUnavailable:
      Result := Format(RALPage, [SLangHTTP, AStatusCode, SError503, SError503Page]);
  end;
end;

{ TRALResponsePage }

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
