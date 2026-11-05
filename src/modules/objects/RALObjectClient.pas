/// The typed client: objects sent and received with TRALClient, through the
/// same serializer and formatters as the server.
unit RALObjectClient;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALClient, RALRequest, RALResponse, RALObjectJSON,
  RALObjectBinding;

type
  { Calls an API with objects: the body goes as JSON, the answer is read into
    the result object when the call succeeds (2xx). The status is returned (0:
    no answer; the client's ErrorCode tells why) and the response kept in
    LastResponse until the next call. Not thread safe: one per thread, or use
    Clone of the TRALClient. }
  TRALObjectClient = class
  private
    FClient: TRALClient;
    FLastResponse: TRALResponse;
    FLastText: StringRAL;
    FOptions: TRALObjectJSONOptions;
    procedure SetOptions(const AValue: TRALObjectJSONOptions);
  public
    /// A typed client over AClient (not owned).
    constructor Create(AClient: TRALClient);
    destructor Destroy; override;

    /// DELETE ARoute.
    function Delete(const ARoute: StringRAL; AResult: TObject = nil): IntegerRAL;
    { Sends AMethod to ARoute with ABody (nil = no body) and reads a 2xx answer
      into AResult (nil = ignored). }
    function Execute(AMethod: TRALMethod; const ARoute: StringRAL; ABody: TObject;
      AResult: TObject): IntegerRAL;
    /// GET ARoute into AResult.
    function Get(const ARoute: StringRAL; AResult: TObject): IntegerRAL;
    /// PATCH ARoute with ABody.
    function Patch(const ARoute: StringRAL; ABody: TObject; AResult: TObject = nil): IntegerRAL;
    /// POST ARoute with ABody.
    function Post(const ARoute: StringRAL; ABody: TObject; AResult: TObject = nil): IntegerRAL;
    /// PUT ARoute with ABody.
    function Put(const ARoute: StringRAL; ABody: TObject; AResult: TObject = nil): IntegerRAL;

    /// The client the calls go through.
    property Client: TRALClient read FClient;
    /// The response of the last call (owned; replaced by the next).
    property LastResponse: TRALResponse read FLastResponse;
    /// The body of the last answer as text (the problem+json of an error).
    property LastText: StringRAL read FLastText;
    /// How objects are written and read; the binding's settings by default.
    property Options: TRALObjectJSONOptions read FOptions write SetOptions;
  end;

implementation

constructor TRALObjectClient.Create(AClient: TRALClient);
begin
  inherited Create;
  FClient := AClient;
  FOptions := TRALObjectJSONOptions.Create;
  FOptions.Assign(RALObjectSettings.JSON);
end;

destructor TRALObjectClient.Destroy;
begin
  FreeAndNil(FLastResponse);
  FreeAndNil(FOptions);
  inherited;
end;

procedure TRALObjectClient.SetOptions(const AValue: TRALObjectJSONOptions);
begin
  FOptions.Assign(AValue);
end;

function TRALObjectClient.Execute(AMethod: TRALMethod; const ARoute: StringRAL;
  ABody: TObject; AResult: TObject): IntegerRAL;
begin
  FreeAndNil(FLastResponse);
  FLastText := '';
  FClient.Request.Clear;
  FClient.Request.AddHeader('Accept', 'application/json');
  if ABody <> nil then
    FClient.Request.AddBody(TRALObjectJSON.ToText(ABody, FOptions), 'application/json');
  FClient.Execute(ARoute, AMethod, FLastResponse);
  Result := 0;
  if FLastResponse = nil then
    Exit;
  Result := FLastResponse.StatusCode;
  FLastText := FLastResponse.ResponseText;
  if (AResult <> nil) and (Result >= 200) and (Result < 300) and (FLastText <> '') then
    TRALObjectJSON.FromText(FLastText, AResult, FOptions);
end;

function TRALObjectClient.Delete(const ARoute: StringRAL; AResult: TObject): IntegerRAL;
begin
  Result := Execute(amDELETE, ARoute, nil, AResult);
end;

function TRALObjectClient.Get(const ARoute: StringRAL; AResult: TObject): IntegerRAL;
begin
  Result := Execute(amGET, ARoute, nil, AResult);
end;

function TRALObjectClient.Patch(const ARoute: StringRAL; ABody, AResult: TObject): IntegerRAL;
begin
  Result := Execute(amPATCH, ARoute, ABody, AResult);
end;

function TRALObjectClient.Post(const ARoute: StringRAL; ABody, AResult: TObject): IntegerRAL;
begin
  Result := Execute(amPOST, ARoute, ABody, AResult);
end;

function TRALObjectClient.Put(const ARoute: StringRAL; ABody, AResult: TObject): IntegerRAL;
begin
  Result := Execute(amPUT, ARoute, ABody, AResult);
end;

end.
