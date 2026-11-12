/// Antivirus hook for uploads: a scanner base class and the plugin that checks each file.
unit RALUploadScan;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALCustomObjects, RALRequest, RALResponse, RALParams,
  RALPlugin, RALErrorCodes;

const
  /// Priority of the upload scan plugin: after authentication, so refused ones skip it.
  RALPriorityUploadScan = 450;

type
  /// What a scanner found in a file.
  TRALUploadVerdict = (
    /// Nothing found: the upload goes on.
    uvClean,
    /// A threat was found: the upload is refused.
    uvInfected,
    /// The file could not be checked.
    uvError);

  /// What the plugin does with a file that could not be checked.
  TRALScanFailAction = (
    /// Refuses the request with 503 and Retry-After.
    sfaReject,
    /// Lets the file through.
    sfaAccept);

  { Fired for each file after the scanner (alone when there is no scanner, with
    AVerdict uvClean). The application may scan the file itself or override the
    verdict; AThreat is the threat name, or the reason for uvError. }
  TRALUploadScanEvent = procedure(Sender: TObject; ARequest: TRALRequest;
    AParam: TRALParam; var AVerdict: TRALUploadVerdict;
    var AThreat: StringRAL) of object;

  { Base of the scanners. A scanner is called from many request threads at once
    and must be thread-safe. }
  TRALUploadScanner = class(TRALComponent)
  public
    { Checks the content of AStream, read from its start, of the file AFileName.
      On uvInfected AThreat is the threat name, on uvError the reason. }
    function Scan(AStream: TStream; const AFileName: StringRAL;
      out AThreat: StringRAL): TRALUploadVerdict; virtual; abstract;
  end;

  { Checks every uploaded file of a request before any route runs. Infected
    files are answered 422, files that could not be checked 503 (see
    ScanFailAction). Nothing of the file is written anywhere. }
  TRALUploadScanPlugin = class(TRALPlugin)
  private
    FOnScanUpload: TRALUploadScanEvent;
    FScanBody: boolean;
    FScanFailAction: TRALScanFailAction;
    FScanner: TRALUploadScanner;

    /// Scans one param; True when the request goes on.
    function CheckParam(ARequest: TRALRequest; AResponse: TRALResponse;
      AParam: TRALParam): boolean;
    procedure SetScanner(const AValue: TRALUploadScanner);
  protected
    class function DefaultPriority: IntegerRAL; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
  published
    /// Fired for each file after the scanner; may override the verdict.
    property OnScanUpload: TRALUploadScanEvent read FOnScanUpload write FOnScanUpload;
    { Also scans a request whose whole body is one raw value, such as a PUT of
      a file. }
    property ScanBody: boolean read FScanBody write FScanBody default False;
    /// What happens to a file that could not be checked.
    property ScanFailAction: TRALScanFailAction read FScanFailAction
      write FScanFailAction default sfaReject;
    /// Scanner every file goes through; nil leaves the check to OnScanUpload.
    property Scanner: TRALUploadScanner read FScanner write SetScanner;
  end;

implementation

const
  /// Status of a request carrying an infected file.
  cStatusInfected = 422;
  /// Seconds the client is asked to wait when the scanner is down.
  cRetryAfter = '30';

{ TRALUploadScanPlugin }

constructor TRALUploadScanPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScanFailAction := sfaReject;
end;

function TRALUploadScanPlugin.CheckParam(ARequest: TRALRequest;
  AResponse: TRALResponse; AParam: TRALParam): boolean;
var
  vVerdict: TRALUploadVerdict;
  vThreat, vName: StringRAL;
  vStream: TStream;
begin
  Result := True;
  vVerdict := uvClean;
  vThreat := '';
  vName := AParam.FileName;
  if vName = '' then
    vName := AParam.ParamName;

  if FScanner <> nil then
  begin
    vStream := AParam.Content;
    if vStream = nil then
      Exit;
    vStream.Position := 0;
    try
      vVerdict := FScanner.Scan(vStream, vName, vThreat);
    except
      on E: Exception do
      begin
        vVerdict := uvError;
        vThreat := StringRAL(E.Message);
      end;
    end;
    vStream.Position := 0;
  end;

  if Assigned(FOnScanUpload) then
    FOnScanUpload(Self, ARequest, AParam, vVerdict, vThreat);

  case vVerdict of
    uvInfected:
      begin
        AResponse.AnswerProblem(cStatusInfected,
          StringRAL(Format('"%s" was refused: %s', [string(vName), string(vThreat)])));
        Result := False;
      end;
    uvError:
      if FScanFailAction = sfaReject then
      begin
        AResponse.AddHeader('Retry-After', cRetryAfter);
        AResponse.AnswerProblem(HTTP_ServiceUnavailable,
          StringRAL(Format(RALGetErrorMessage(ecScanFailed), [string(vName), string(vThreat)])));
        Result := False;
      end;
  end;
end;

class function TRALUploadScanPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityUploadScan;
end;

procedure TRALUploadScanPlugin.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FScanner) then
    FScanner := nil;
end;

function TRALUploadScanPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess];
end;

procedure TRALUploadScanPlugin.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse; var AHandled: boolean);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  if (FScanner = nil) and not Assigned(FOnScanUpload) then
    Exit;

  for vInt := 0 to ARequest.Params.Count - 1 do
  begin
    vParam := ARequest.Params.Index[vInt];
    if (vParam.Kind = rpkBODY) and (vParam.FileName <> '') then
      if not CheckParam(ARequest, AResponse, vParam) then
      begin
        AHandled := True;
        Exit;
      end;
  end;

  if FScanBody then
  begin
    vParam := ARequest.Params.SingleBody;
    if (vParam <> nil) and (vParam.FileName = '') then
      if not CheckParam(ARequest, AResponse, vParam) then
        AHandled := True;
  end;
end;

procedure TRALUploadScanPlugin.SetScanner(const AValue: TRALUploadScanner);
begin
  if FScanner = AValue then
    Exit;
  if FScanner <> nil then
    FScanner.RemoveFreeNotification(Self);
  FScanner := AValue;
  if FScanner <> nil then
    FScanner.FreeNotification(Self);
end;

end.
