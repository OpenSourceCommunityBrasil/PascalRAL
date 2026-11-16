{ Hypermedia for the JSON a route answers: by the Accept header, the same answer goes out
  as HAL (application/hal+json, with _links), as JSON:API (application/vnd.api+json,
  data and links) or as an HTML page (a template, or a page made from the data), with
  the links of the resource listed in the Object Inspector. The handlers keep answering
  plain JSON; the plugin rewrites it when the answer goes out. }
unit RALHypermedia;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALJson, RALUrlCoder, RALPlugin, RALRequest,
  RALResponse, RALRoutes, RALTemplate, RALHtml;

const
  /// Priority of the plugin: it only marks the request, so late is fine.
  RALPriorityHypermedia = 120;
  /// The media type of HAL.
  rctHAL = 'application/hal+json';
  /// The media type of JSON:API.
  rctJSONAPI = 'application/vnd.api+json';

type
  /// A format of the answer.
  TRALHypermediaFormat = (
    /// HAL: the object with _links (and _embedded for a list).
    hfHAL,
    /// JSON:API: data (type, id, attributes) and links.
    hfJSONAPI,
    /// An HTML page.
    hfHTML
  );
  /// The formats a plugin offers.
  TRALHypermediaFormats = set of TRALHypermediaFormat;

  { A resource: the routes it covers and its links. In a href template a name in braces
    comes from the path (a :name segment of Path) or from a field of the object. }
  TRALHypermediaResource = class(TCollectionItem)
  private
    FIdField: StringRAL;
    FItemHref: StringRAL;
    FLinks: TStrings;
    FPath: StringRAL;
    FResourceType: StringRAL;
    FSelfHref: StringRAL;
    FTemplate: StringRAL;
    FTitle: StringRAL;

    procedure SetLinks(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    { Whether the request path (segments) matches Path; the :name segments go to AParams
      as name=value. }
    function Matches(const APath: TRALRouteSegments; AParams: TStrings): Boolean;
  published
    /// The field of an object that is its id (the JSON:API id).
    property IdField: StringRAL read FIdField write FIdField;
    /// The link of each item of a list (/clientes/{id}); empty = no link per item.
    property ItemHref: StringRAL read FItemHref write FItemHref;
    /// More links, one rel=href template per line (pedidos=/clientes/{id}/pedidos).
    property Links: TStrings read FLinks write SetLinks;
    /// The routes it covers: segments, :name takes one (clientes/:id).
    property Path: StringRAL read FPath write FPath;
    /// The type of JSON:API and the name of the list in HAL's _embedded.
    property ResourceType: StringRAL read FResourceType write FResourceType;
    /// The self link; empty = the request's own path.
    property SelfHref: StringRAL read FSelfHref write FSelfHref;
    /// The template of the HTML page; empty = a page made from the data.
    property Template: StringRAL read FTemplate write FTemplate;
    /// The title of the HTML page.
    property Title: StringRAL read FTitle write FTitle;
  end;

  /// The resources of a plugin.
  TRALHypermediaResources = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TRALHypermediaResource;
  public
    /// Creates the TRALHypermediaResources.
    constructor Create(AOwner: TPersistent);

    /// Adds a resource.
    function Add: TRALHypermediaResource;

    /// The resource at AIndex.
    property Items[AIndex: Integer]: TRALHypermediaResource read GetItem; default;
  end;

  TRALHypermediaPlugin = class;

  { The work for one request: rewrites the answer when it goes out, and frees itself
    when the request ends. }
  TRALHypermediaJob = class
  private
    /// The format asked.
    FFormat: TRALHypermediaFormat;
    /// The :name values of the path.
    FParams: TStringList;
    /// The plugin (read only while the request runs).
    FPlugin: TRALHypermediaPlugin;
    /// The request's path.
    FRequestPath: StringRAL;
    /// The resource matched.
    FResource: TRALHypermediaResource;
  public
    constructor Create;
    destructor Destroy; override;

    /// The request ended: frees the job.
    procedure Finished(ARequest: TRALRequest);
    /// The answer goes out: rewrites it.
    procedure Wire(AResponse: TRALResponse);
  end;

  { The plugin. Link it to the server, list the resources, and answers of their
    routes go out as HAL, JSON:API or HTML when the client asks so in Accept (a plain
    application/json, or no Accept, keeps the JSON as it is). Only 2xx JSON answers
    are rewritten. }
  TRALHypermediaPlugin = class(TRALPlugin)
  private
    FFormats: TRALHypermediaFormats;
    FResources: TRALHypermediaResources;
    FTemplates: TRALTemplateEngine;

    procedure SetResources(AValue: TRALHypermediaResources);
    procedure SetTemplates(AValue: TRALTemplateEngine);
  protected
    class function DefaultPriority: IntegerRAL; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The format the Accept header AAccept asks among Formats (the first of the three
      types it lists); False when it asks none of them. }
    function FormatOf(const AAccept: StringRAL; out AFormat: TRALHypermediaFormat): Boolean;
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: Boolean); override;
    { The JSON text AJson of AResource rewritten in AFormat; AParams are the :name values
      of the path, APath the request path. }
    function Transform(AResource: TRALHypermediaResource; AFormat: TRALHypermediaFormat;
      const AJson, APath: StringRAL; AParams: TStrings): StringRAL;
  published
    /// The formats offered.
    property Formats: TRALHypermediaFormats read FFormats write FFormats
      default [hfHAL, hfJSONAPI, hfHTML];
    /// The resources.
    property Resources: TRALHypermediaResources read FResources write SetResources;
    /// The templates of the HTML pages (Template of a resource).
    property Templates: TRALTemplateEngine read FTemplates write SetTemplates;
  end;

implementation

const
  /// The media type of each format.
  cFormatType: array[TRALHypermediaFormat] of StringRAL = (rctHAL, rctJSONAPI,
    'text/html; charset=utf-8');

/// AHref with each name in braces taken from AParams, else from AObject (URL encoded).
function ExpandHref(const AHref: StringRAL; AParams: TStrings; AObject: TRALJSONObject): StringRAL;
var
  vText, vName, vValue: string;
  vStart, vStop: Integer;
  vField: TRALJSON;
begin
  vText := string(AHref);
  Result := '';
  vStart := Pos('{', vText);
  while vStart > 0 do
  begin
    vStop := Pos('}', Copy(vText, vStart + 1, MaxInt));
    if vStop = 0 then
      Break;
    vName := Copy(vText, vStart + 1, vStop - 1);
    vValue := '';
    if (AParams <> nil) and (AParams.IndexOfName(vName) >= 0) then
      vValue := AParams.Values[vName]
    else if AObject <> nil then
    begin
      vField := AObject.Find(StringRAL(vName));
      if (vField <> nil) and (vField.Kind in [rjkString, rjkNumber, rjkBoolean]) then
        vValue := string(vField.AsString);
    end;
    Result := Result + StringRAL(Copy(vText, 1, vStart - 1)) +
      TRALHTTPCoder.EncodeURL(StringRAL(vValue));
    vText := Copy(vText, vStart + vStop + 1, MaxInt);
    vStart := Pos('{', vText);
  end;
  Result := Result + StringRAL(vText);
end;

/// A {"href": AHref} object.
function HrefObject(const AHref: StringRAL): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('href', AHref);
end;

{ TRALHypermediaResource }

procedure TRALHypermediaResource.Assign(ASource: TPersistent);
var
  vSource: TRALHypermediaResource;
begin
  if ASource is TRALHypermediaResource then
  begin
    vSource := TRALHypermediaResource(ASource);
    FIdField := vSource.FIdField;
    FItemHref := vSource.FItemHref;
    FLinks.Assign(vSource.FLinks);
    FPath := vSource.FPath;
    FResourceType := vSource.FResourceType;
    FSelfHref := vSource.FSelfHref;
    FTemplate := vSource.FTemplate;
    FTitle := vSource.FTitle;
  end
  else
    inherited Assign(ASource);
end;

constructor TRALHypermediaResource.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIdField := 'id';
  FLinks := TStringList.Create;
end;

destructor TRALHypermediaResource.Destroy;
begin
  FreeAndNil(FLinks);
  inherited Destroy;
end;

function TRALHypermediaResource.GetDisplayName: string;
begin
  if FPath <> '' then
    Result := string(FPath)
  else
    Result := inherited GetDisplayName;
end;

function TRALHypermediaResource.Matches(const APath: TRALRouteSegments;
  AParams: TStrings): Boolean;
var
  vPattern: TRALRouteSegments;
  vIdx: Integer;
begin
  Result := False;
  vPattern := RALPathSegments(FPath);
  if Length(vPattern) <> Length(APath) then
    Exit;
  for vIdx := 0 to High(vPattern) do
    if (vPattern[vIdx] <> '') and (vPattern[vIdx][1] = ':') then
      AParams.Values[string(Copy(vPattern[vIdx], 2, MaxInt))] := string(APath[vIdx])
    else if not RALSameName(vPattern[vIdx], APath[vIdx]) then
      Exit;
  Result := True;
end;

procedure TRALHypermediaResource.SetLinks(AValue: TStrings);
begin
  FLinks.Assign(AValue);
end;

{ TRALHypermediaResources }

function TRALHypermediaResources.Add: TRALHypermediaResource;
begin
  Result := TRALHypermediaResource(inherited Add);
end;

constructor TRALHypermediaResources.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALHypermediaResource);
end;

function TRALHypermediaResources.GetItem(AIndex: Integer): TRALHypermediaResource;
begin
  Result := TRALHypermediaResource(inherited Items[AIndex]);
end;

{ TRALHypermediaJob }

constructor TRALHypermediaJob.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TRALHypermediaJob.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TRALHypermediaJob.Finished(ARequest: TRALRequest);
begin
  Free;
end;

procedure TRALHypermediaJob.Wire(AResponse: TRALResponse);
var
  vText: StringRAL;
begin
  if (AResponse.StatusCode < 200) or (AResponse.StatusCode > 299) or
     (RALPosText('json', AResponse.ContentType) = 0) then
    Exit;
  vText := AResponse.ResponseText;
  if vText = '' then
    Exit;
  try
    vText := FPlugin.Transform(FResource, FFormat, vText, FRequestPath, FParams);
  except
    // not JSON after all, or a template that failed: the answer goes as it was
    Exit;
  end;
  AResponse.Answer(AResponse.StatusCode, vText, cFormatType[FFormat]);
  AResponse.AddHeader('Vary', 'Accept');
end;

{ TRALHypermediaPlugin }

constructor TRALHypermediaPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormats := [hfHAL, hfJSONAPI, hfHTML];
  FResources := TRALHypermediaResources.Create(Self);
end;

class function TRALHypermediaPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityHypermedia;
end;

destructor TRALHypermediaPlugin.Destroy;
begin
  SetTemplates(nil);
  FreeAndNil(FResources);
  inherited Destroy;
end;

function TRALHypermediaPlugin.FormatOf(const AAccept: StringRAL;
  out AFormat: TRALHypermediaFormat): Boolean;
var
  vText: string;
  vBest, vPos: Integer;
  vFormat: TRALHypermediaFormat;
begin
  Result := False;
  AFormat := hfHAL;
  vText := LowerCase(string(AAccept));
  vBest := MaxInt;
  for vFormat := Low(TRALHypermediaFormat) to High(TRALHypermediaFormat) do
  begin
    if not (vFormat in FFormats) then
      Continue;
    case vFormat of
      hfHAL: vPos := Pos(rctHAL, vText);
      hfJSONAPI: vPos := Pos(rctJSONAPI, vText);
    else
      vPos := Pos('text/html', vText);
    end;
    if (vPos > 0) and (vPos < vBest) then
    begin
      vBest := vPos;
      AFormat := vFormat;
      Result := True;
    end;
  end;
end;

procedure TRALHypermediaPlugin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTemplates) then
    FTemplates := nil;
end;

function TRALHypermediaPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess];
end;

procedure TRALHypermediaPlugin.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: Boolean);
var
  vFormat: TRALHypermediaFormat;
  vRaw, vPath: TRALRouteSegments;
  vIdx: Integer;
  vJob: TRALHypermediaJob;
begin
  if not FormatOf(ARequest.GetHeader('Accept'), vFormat) then
    Exit;
  RALSplitRequestPath(ARequest, vRaw, vPath);
  vJob := TRALHypermediaJob.Create;
  for vIdx := 0 to FResources.Count - 1 do
  begin
    vJob.FParams.Clear;
    if FResources[vIdx].Matches(vPath, vJob.FParams) then
    begin
      vJob.FPlugin := Self;
      vJob.FResource := FResources[vIdx];
      vJob.FFormat := vFormat;
      vJob.FRequestPath := ARequest.Query;
      AResponse.AddWireHandler(vJob.Wire);
      ARequest.AddFinishHandler(vJob.Finished);
      Exit;
    end;
  end;
  vJob.Free;
end;

procedure TRALHypermediaPlugin.SetResources(AValue: TRALHypermediaResources);
begin
  FResources.Assign(AValue);
end;

procedure TRALHypermediaPlugin.SetTemplates(AValue: TRALTemplateEngine);
begin
  if FTemplates = AValue then
    Exit;
  if FTemplates <> nil then
    FTemplates.RemoveFreeNotification(Self);
  FTemplates := AValue;
  if FTemplates <> nil then
    FTemplates.FreeNotification(Self);
end;

function TRALHypermediaPlugin.Transform(AResource: TRALHypermediaResource;
  AFormat: TRALHypermediaFormat; const AJson, APath: StringRAL; AParams: TStrings): StringRAL;
var
  vData: TRALJSON;
  vSelf: StringRAL;

  { The links object of AResource for AObject: self plus Links. }
  function LinksOf(AObject: TRALJSONObject; const ASelf: StringRAL): TRALJSONObject;
  var
    vIdx: Integer;
    vName, vHref: string;
  begin
    Result := TRALJSONObject.Create;
    if AFormat = hfHAL then
      Result.Add('self', HrefObject(ASelf))
    else
      Result.Add('self', ASelf);
    for vIdx := 0 to AResource.Links.Count - 1 do
    begin
      vName := Trim(AResource.Links.Names[vIdx]);
      vHref := Trim(AResource.Links.ValueFromIndex[vIdx]);
      if (vName = '') or (vHref = '') then
        Continue;
      if AFormat = hfHAL then
        Result.Add(StringRAL(vName), HrefObject(ExpandHref(StringRAL(vHref), AParams, AObject)))
      else
        Result.Add(StringRAL(vName), ExpandHref(StringRAL(vHref), AParams, AObject));
    end;
  end;

  { An item of JSON:API: type, id, attributes (the fields but the id), links. }
  function ApiItem(AObject: TRALJSONObject; AWithLinks: Boolean): TRALJSONObject;
  var
    vIdx: Integer;
    vAttrs: TRALJSONObject;
    vId: TRALJSON;
    vHref: StringRAL;
    vNames: TStringList;
  begin
    // the link first: the fields move to attributes below
    vHref := '';
    if AWithLinks and (AResource.ItemHref <> '') then
      vHref := ExpandHref(AResource.ItemHref, nil, AObject);
    Result := TRALJSONObject.Create;
    Result.Add('type', AResource.ResourceType);
    vId := AObject.Find(AResource.IdField);
    if vId <> nil then
      Result.Add('id', vId.AsString);
    vAttrs := TRALJSONObject.Create;
    Result.Add('attributes', vAttrs);
    vNames := TStringList.Create;
    try
      for vIdx := 0 to AObject.Count - 1 do
        if AObject.Names[vIdx] <> AResource.IdField then
          vNames.Add(string(AObject.Names[vIdx]));
      for vIdx := 0 to vNames.Count - 1 do
        vAttrs.Add(StringRAL(vNames[vIdx]), AObject.Extract(StringRAL(vNames[vIdx])));
    finally
      vNames.Free;
    end;
    if vHref <> '' then
      Result.Add('links', TRALJSONObject.Create.Add('self', vHref));
  end;

  { The HTML page made from the data. }
  function AutoPage: StringRAL;
  var
    vHtml, vTitle: string;
    vIdx, vJdx: Integer;
    vList: TRALJSONArray;
    vObj: TRALJSONObject;
    vValue: TRALJSON;

    function Cell(AValue: TRALJSON): string;
    begin
      if AValue = nil then
        Result := ''
      else if AValue.Kind in [rjkObject, rjkArray] then
        Result := '<code>' + string(RALHtmlEscape(AValue.ToJSON)) + '</code>'
      else if AValue.Kind = rjkNull then
        Result := ''
      else
        Result := string(RALHtmlEscape(AValue.AsString));
    end;

  begin
    vTitle := string(AResource.Title);
    if vTitle = '' then
      vTitle := string(AResource.ResourceType);
    vHtml := '<!DOCTYPE html>'#10'<html><head><meta charset="utf-8"><title>' +
      string(RALHtmlEscape(StringRAL(vTitle))) + '</title></head><body>'#10'<h1>' +
      string(RALHtmlEscape(StringRAL(vTitle))) + '</h1>'#10;
    if vData.Kind = rjkObject then
    begin
      vObj := TRALJSONObject(vData);
      vHtml := vHtml + '<dl>'#10;
      for vIdx := 0 to vObj.Count - 1 do
        vHtml := vHtml + '<dt>' + string(RALHtmlEscape(vObj.Names[vIdx])) + '</dt><dd>' +
          Cell(vObj.Items[vIdx]) + '</dd>'#10;
      vHtml := vHtml + '</dl>'#10;
    end
    else if vData.Kind = rjkArray then
    begin
      vList := TRALJSONArray(vData);
      vHtml := vHtml + '<table>'#10;
      if (vList.Count > 0) and (vList[0].Kind = rjkObject) then
      begin
        vObj := TRALJSONObject(vList[0]);
        vHtml := vHtml + '<tr>';
        for vJdx := 0 to vObj.Count - 1 do
          vHtml := vHtml + '<th>' + string(RALHtmlEscape(vObj.Names[vJdx])) + '</th>';
        vHtml := vHtml + '</tr>'#10;
      end;
      for vIdx := 0 to vList.Count - 1 do
      begin
        vHtml := vHtml + '<tr>';
        if vList[vIdx].Kind = rjkObject then
        begin
          vObj := TRALJSONObject(vList[vIdx]);
          for vJdx := 0 to vObj.Count - 1 do
          begin
            vValue := vObj.Items[vJdx];
            if (vJdx = 0) and (AResource.ItemHref <> '') then
              vHtml := vHtml + '<td><a href="' + string(RALHtmlEscape(ExpandHref(
                AResource.ItemHref, nil, vObj))) + '">' + Cell(vValue) + '</a></td>'
            else
              vHtml := vHtml + '<td>' + Cell(vValue) + '</td>';
          end;
        end
        else
          vHtml := vHtml + '<td>' + Cell(vList[vIdx]) + '</td>';
        vHtml := vHtml + '</tr>'#10;
      end;
      vHtml := vHtml + '</table>'#10;
    end
    else
      vHtml := vHtml + '<p>' + Cell(vData) + '</p>'#10;
    vObj := nil;
    if vData.Kind = rjkObject then
      vObj := TRALJSONObject(vData);
    vHtml := vHtml + '<nav>';
    for vIdx := 0 to AResource.Links.Count - 1 do
      if Trim(AResource.Links.Names[vIdx]) <> '' then
        vHtml := vHtml + '<a rel="' + string(RALHtmlEscape(StringRAL(Trim(
          AResource.Links.Names[vIdx])))) + '" href="' + string(RALHtmlEscape(ExpandHref(
          StringRAL(Trim(AResource.Links.ValueFromIndex[vIdx])), AParams, vObj))) + '">' +
          string(RALHtmlEscape(StringRAL(Trim(AResource.Links.Names[vIdx])))) + '</a> ';
    vHtml := vHtml + '</nav>'#10'</body></html>'#10;
    Result := StringRAL(vHtml);
  end;

var
  vOut: TRALJSONObject;
  vList, vItems: TRALJSONArray;
  vIdx: Integer;
  vItem: TRALJSONObject;
begin
  vData := TRALJSON.ParseJSON(AJson);
  try
    if AResource.SelfHref <> '' then
      vSelf := ExpandHref(AResource.SelfHref, AParams, nil)
    else
      vSelf := APath;
    case AFormat of
      hfHAL:
      begin
        if vData.Kind = rjkObject then
        begin
          TRALJSONObject(vData).Add('_links', LinksOf(TRALJSONObject(vData), vSelf));
          Result := vData.ToJSON;
        end
        else if vData.Kind = rjkArray then
        begin
          vList := TRALJSONArray(vData);
          if AResource.ItemHref <> '' then
            for vIdx := 0 to vList.Count - 1 do
              if vList[vIdx].Kind = rjkObject then
                TRALJSONObject(vList[vIdx]).Add('_links', TRALJSONObject.Create.Add('self',
                  HrefObject(ExpandHref(AResource.ItemHref, nil, TRALJSONObject(vList[vIdx])))));
          vOut := TRALJSONObject.Create;
          try
            vOut.Add('_links', LinksOf(nil, vSelf));
            vOut.Add('_embedded', TRALJSONObject.Create.Add(AResource.ResourceType, vData));
            vData := nil;
            Result := vOut.ToJSON;
          finally
            vOut.Free;
          end;
        end
        else
          Result := AJson;
      end;
      hfJSONAPI:
      begin
        vOut := TRALJSONObject.Create;
        try
          if vData.Kind = rjkObject then
          begin
            vOut.Add('links', LinksOf(TRALJSONObject(vData), vSelf));
            vOut.Add('data', ApiItem(TRALJSONObject(vData), False));
          end
          else if vData.Kind = rjkArray then
          begin
            vOut.Add('links', LinksOf(nil, vSelf));
            vItems := TRALJSONArray.Create;
            vOut.Add('data', vItems);
            vList := TRALJSONArray(vData);
            for vIdx := 0 to vList.Count - 1 do
              if vList[vIdx].Kind = rjkObject then
              begin
                vItem := TRALJSONObject(vList[vIdx]);
                vItems.Add(ApiItem(vItem, True));
              end;
          end
          else
          begin
            vOut.Add('data', vData);
            vData := nil;
          end;
          Result := vOut.ToJSON;
        finally
          vOut.Free;
        end;
      end;
    else
      if (AResource.Template <> '') and (FTemplates <> nil) then
        Result := FTemplates.RenderText(AResource.Template, [vData])
      else
        Result := AutoPage;
    end;
  finally
    vData.Free;
  end;
end;

end.
