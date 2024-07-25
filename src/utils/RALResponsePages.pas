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
begin
  Result := '';
  for vInt := 0 to Pred(Count) do
  begin
    vPage := TRALResponsePage(Items[vInt]);
    if vPage.StatusCode = AStatusCode then
    begin
      Result := vPage.Page.Text;
      Exit;
    end;
  end;
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
  CreatePage(400, Format(RALPage, [SLangHTTP, 400, SError400, SError400Page]));
  CreatePage(401, Format(RALPage, [SLangHTTP, 401, SError401, SError401Page]));
  CreatePage(403, Format(RALPage, [SLangHTTP, 403, SError403, SError403Page]));
  CreatePage(404, Format(RALPage, [SLangHTTP, 404, SError404, SError404Page]));
  CreatePage(415, Format(RALPage, [SLangHTTP, 415, SError415, SError415Page]));
  CreatePage(500, Format(RALPage, [SLangHTTP, 500, SError500, SError500Page]));
  CreatePage(501, Format(RALPage, [SLangHTTP, 501, SError501, SError501Page]));
  CreatePage(503, Format(RALPage, [SLangHTTP, 503, SError503, SError503Page]));
end;

class function TRALResponsePages.GetHTTPResponsePage(AStatusCode: IntegerRAL): StringRAL;
begin
  Result := '';
  case AStatusCode of
    400 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError400, SError400Page]);
    401 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError401, SError401Page]);
    403 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError403, SError403Page]);
    404 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError404, SError404Page]);
    415 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError415, SError415Page]);
    500 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError500, SError500Page]);
    501 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError501, SError501Page]);
    503 : Result := Format(RALPage, [SLangHTTP, AStatusCode, SError503, SError503Page]);
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
