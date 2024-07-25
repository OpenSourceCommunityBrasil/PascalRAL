unit RALPageCodes;

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools;

type
  { TRALPageCode }

  TRALPageCode = class(TCollectionItem)
  private
    FPageCode: IntegerRAL;
    FPage: TStrings;
  protected
    function GetDisplayName: string; override;

    function GetPage: TStrings;
    procedure SetPage(const AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property PageCode: IntegerRAL read FPageCode write FPageCode;
    property Page: TStrings read GetPage write SetPage;
  end;

  { TRALPageCodes }

  TRALPageCodes = class(TOwnedCollection)
  public
    constructor Create(AOwner: TPersistent);

    function FindPageHTML(APageCode: IntegerRAL) : StringRAL;
  end;

implementation

{ TRALPageCodes }

constructor TRALPageCodes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALPageCode);
end;

function TRALPageCodes.FindPageHTML(APageCode: IntegerRAL): StringRAL;
var
  vInt: IntegerRAL;
  vPage: TRALPageCode;
begin
  Result := '';
  for vInt := 0 to Pred(Count) do
  begin
    vPage := TRALPageCode(Items[vInt]);
    if vPage.PageCode = APageCode then
    begin
      Result := vPage.Page.Text;
      Exit;
    end;
  end;

  Result := RALHTTPPageCode(APageCode);
end;

{ TRALPageCode }

constructor TRALPageCode.Create(ACollection: TCollection);
begin
  inherited;
  FPageCode := 0;
  FPage := TStringList.Create;
end;

destructor TRALPageCode.Destroy;
begin
  FreeAndNil(FPage);
  inherited;
end;

function TRALPageCode.GetDisplayName: string;
begin
  Result := IntToStr(FPageCode);
end;

function TRALPageCode.GetPage: TStrings;
begin
  if Trim(FPage.Text) = '' then
    FPage.Text := RALHTTPPageCode(FPageCode);

  Result := FPage;
end;

procedure TRALPageCode.SetPage(const AValue: TStrings);
begin
  FPage.Assign(AValue);
end;

end.
