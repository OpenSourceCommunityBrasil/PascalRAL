/// HTMX (htmx.org): what an HTMX request says in headers, and the headers that steer it.
unit RALHtmx;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALRequest, RALResponse, RALParams, RALJson;

type
  /// What an HTMX request says about itself, read from its HX-* headers.
  TRALHtmxRequest = record
    /// HX-Boosted: the request comes from an element with hx-boost.
    Boosted: Boolean;
    /// HX-Current-URL: the URL the browser is on.
    CurrentURL: StringRAL;
    /// HX-History-Restore-Request: the browser restores a page missing from its cache.
    HistoryRestore: Boolean;
    /// HX-Request: the request was made by HTMX.
    IsHtmx: Boolean;
    /// HX-Prompt: what the user typed in an hx-prompt.
    Prompt: StringRAL;
    /// HX-Target: id of the target element, when it has one.
    Target: StringRAL;
    /// HX-Trigger: id of the element that triggered the request, when it has one.
    Trigger: StringRAL;
    /// HX-Trigger-Name: name of the element that triggered the request, when it has one.
    TriggerName: StringRAL;
  end;

  { The HTMX response headers. Each call sets its header on AResponse before
    the answer goes out (before the first Send of a stream). }
  TRALHtmx = class
  private
    /// Adds the event AEvent (with the JSON ADetail, or none) to the header AHeader.
    class procedure AddEvent(AResponse: TRALResponse; const AHeader, AEvent,
      ADetail: StringRAL);
  public
    { What ARequest says in its HX-* headers; every field empty or False for a
      request HTMX did not make. }
    class function Info(ARequest: TRALRequest): TRALHtmxRequest;
    /// Whether ARequest was made by HTMX (HX-Request: true).
    class function IsHtmx(ARequest: TRALRequest): Boolean;
    { HX-Location: a client-side redirect without a full reload, to AURL, or
      to the JSON object AJSON (path, target, swap...) when AURL starts with an
      opening brace. }
    class procedure Location(AResponse: TRALResponse; const AURL: StringRAL);
    /// HX-Push-Url: pushes AURL into the browser history; 'false' prevents it.
    class procedure PushUrl(AResponse: TRALResponse; const AURL: StringRAL);
    /// HX-Redirect: a full page redirect to AURL.
    class procedure Redirect(AResponse: TRALResponse; const AURL: StringRAL);
    /// HX-Refresh: the browser reloads the whole page.
    class procedure Refresh(AResponse: TRALResponse);
    /// HX-Replace-Url: replaces the current URL in the history with AURL.
    class procedure ReplaceUrl(AResponse: TRALResponse; const AURL: StringRAL);
    /// HX-Reselect: the CSS selector of the part of the answer that is swapped.
    class procedure Reselect(AResponse: TRALResponse; const ASelector: StringRAL);
    /// HX-Reswap: how the answer is swapped (innerHTML, outerHTML, beforeend...).
    class procedure Reswap(AResponse: TRALResponse; const ASwap: StringRAL);
    /// HX-Retarget: the CSS selector of the element the answer goes into.
    class procedure Retarget(AResponse: TRALResponse; const ASelector: StringRAL);
    /// HX-Trigger-After-Settle: as TriggerEvent, after the settle step.
    class procedure TriggerAfterSettle(AResponse: TRALResponse; const AEvent: StringRAL;
      const ADetail: StringRAL = '');
    /// HX-Trigger-After-Swap: as TriggerEvent, after the swap step.
    class procedure TriggerAfterSwap(AResponse: TRALResponse; const AEvent: StringRAL;
      const ADetail: StringRAL = '');
    { HX-Trigger: fires the client event AEvent when the answer arrives, with
      ADetail (JSON text: an object, a string in quotes, a number) as its
      detail. Called again it adds events to the same header. }
    class procedure TriggerEvent(AResponse: TRALResponse; const AEvent: StringRAL;
      const ADetail: StringRAL = '');
    { Whether ARequest wants only a fragment, not the whole page: made by HTMX,
      not boosted (hx-boost swaps the whole body) and not a history restore. }
    class function WantsFragment(ARequest: TRALRequest): Boolean;
  end;

implementation

const
  /// Value of the boolean HX-* headers.
  cHtmxTrue = 'true';

/// The value of the header AName of ARequest, '' when it has none.
function HeaderOf(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := ARequest.Params.GetKind[AName, rpkHEADER];
  if vParam <> nil then
    Result := vParam.AsString;
end;

/// The text of a JSON string for AText, quotes included.
function JSONText(const AText: StringRAL): StringRAL;
var
  vValue: TRALJSONString;
begin
  vValue := TRALJSONString.Create(AText);
  try
    Result := vValue.ToJSON;
  finally
    FreeAndNil(vValue);
  end;
end;

{ TRALHtmx }

class procedure TRALHtmx.AddEvent(AResponse: TRALResponse; const AHeader, AEvent,
  ADetail: StringRAL);
var
  vParam: TRALParam;
  vOld, vNew, vName: StringRAL;
  vNames: TStringList;
  vInt: IntegerRAL;
begin
  vParam := AResponse.Params.GetKind[AHeader, rpkHEADER];
  if vParam <> nil then
    vOld := vParam.AsString
  else
    vOld := '';

  { a header of names only stays a list of names; one detail turns the whole
    header into a JSON object, the names before it with a null detail }
  if (ADetail = '') and ((vOld = '') or (vOld[POSINISTR] <> '{')) then
  begin
    if vOld = '' then
      vNew := AEvent
    else
      vNew := vOld + ', ' + AEvent;
  end
  else
  begin
    if ADetail = '' then
      vName := JSONText(AEvent) + ':null'
    else
      vName := JSONText(AEvent) + ':' + ADetail;
    if vOld = '' then
      vNew := '{' + vName + '}'
    else if vOld[POSINISTR] = '{' then
      vNew := Copy(vOld, 1, Length(vOld) - 1) + ',' + vName + '}'
    else
    begin
      vNew := '{';
      vNames := TStringList.Create;
      try
        vNames.CommaText := string(vOld);
        for vInt := 0 to Pred(vNames.Count) do
          vNew := vNew + JSONText(StringRAL(Trim(vNames[vInt]))) + ':null,';
      finally
        FreeAndNil(vNames);
      end;
      vNew := vNew + vName + '}';
    end;
  end;
  AResponse.Params.ReplaceParam(AHeader, vNew, rpkHEADER);
end;

class function TRALHtmx.Info(ARequest: TRALRequest): TRALHtmxRequest;
begin
  Result.IsHtmx := HeaderOf(ARequest, 'HX-Request') = cHtmxTrue;
  Result.Boosted := HeaderOf(ARequest, 'HX-Boosted') = cHtmxTrue;
  Result.HistoryRestore := HeaderOf(ARequest, 'HX-History-Restore-Request') = cHtmxTrue;
  Result.CurrentURL := HeaderOf(ARequest, 'HX-Current-URL');
  Result.Prompt := HeaderOf(ARequest, 'HX-Prompt');
  Result.Target := HeaderOf(ARequest, 'HX-Target');
  Result.Trigger := HeaderOf(ARequest, 'HX-Trigger');
  Result.TriggerName := HeaderOf(ARequest, 'HX-Trigger-Name');
end;

class function TRALHtmx.IsHtmx(ARequest: TRALRequest): Boolean;
begin
  Result := HeaderOf(ARequest, 'HX-Request') = cHtmxTrue;
end;

class procedure TRALHtmx.Location(AResponse: TRALResponse; const AURL: StringRAL);
begin
  AResponse.Params.ReplaceParam('HX-Location', AURL, rpkHEADER);
end;

class procedure TRALHtmx.PushUrl(AResponse: TRALResponse; const AURL: StringRAL);
begin
  AResponse.Params.ReplaceParam('HX-Push-Url', AURL, rpkHEADER);
end;

class procedure TRALHtmx.Redirect(AResponse: TRALResponse; const AURL: StringRAL);
begin
  AResponse.Params.ReplaceParam('HX-Redirect', AURL, rpkHEADER);
end;

class procedure TRALHtmx.Refresh(AResponse: TRALResponse);
begin
  AResponse.Params.ReplaceParam('HX-Refresh', cHtmxTrue, rpkHEADER);
end;

class procedure TRALHtmx.ReplaceUrl(AResponse: TRALResponse; const AURL: StringRAL);
begin
  AResponse.Params.ReplaceParam('HX-Replace-Url', AURL, rpkHEADER);
end;

class procedure TRALHtmx.Reselect(AResponse: TRALResponse; const ASelector: StringRAL);
begin
  AResponse.Params.ReplaceParam('HX-Reselect', ASelector, rpkHEADER);
end;

class procedure TRALHtmx.Reswap(AResponse: TRALResponse; const ASwap: StringRAL);
begin
  AResponse.Params.ReplaceParam('HX-Reswap', ASwap, rpkHEADER);
end;

class procedure TRALHtmx.Retarget(AResponse: TRALResponse; const ASelector: StringRAL);
begin
  AResponse.Params.ReplaceParam('HX-Retarget', ASelector, rpkHEADER);
end;

class procedure TRALHtmx.TriggerEvent(AResponse: TRALResponse; const AEvent,
  ADetail: StringRAL);
begin
  AddEvent(AResponse, 'HX-Trigger', AEvent, ADetail);
end;

class procedure TRALHtmx.TriggerAfterSettle(AResponse: TRALResponse; const AEvent,
  ADetail: StringRAL);
begin
  AddEvent(AResponse, 'HX-Trigger-After-Settle', AEvent, ADetail);
end;

class procedure TRALHtmx.TriggerAfterSwap(AResponse: TRALResponse; const AEvent,
  ADetail: StringRAL);
begin
  AddEvent(AResponse, 'HX-Trigger-After-Swap', AEvent, ADetail);
end;

class function TRALHtmx.WantsFragment(ARequest: TRALRequest): Boolean;
var
  vInfo: TRALHtmxRequest;
begin
  vInfo := Info(ARequest);
  Result := vInfo.IsHtmx and (not vInfo.Boosted) and (not vInfo.HistoryRestore);
end;

end.
