unit RALSynopseServer;

interface

uses
  Classes, SysUtils, syncobjs,
  mormot.net.server, mormot.net.http, mormot.net.async, mormot.core.os,
  mormot.core.base,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams;

type

  { TRALSynopseSSL }

  TRALSynopseSSL = class(TRALSSL)
  private
    FCertificateFile: StringRAL;
    FPrivateKeyFile: StringRAL;
    FPrivateKeyPassword: StringRAL;
    FCACertificatesFile: StringRAL;
  published
    property CertificateFile : StringRAL read FCertificateFile write FCertificateFile;
    property PrivateKeyFile : StringRAL read FPrivateKeyFile write FPrivateKeyFile;
    property PrivateKeyPassword : StringRAL read FPrivateKeyPassword write FPrivateKeyPassword;
    property CACertificatesFile : StringRAL read FCACertificatesFile write FCACertificatesFile;
  end;

  { TRALSynopseServer }

  TRALSynopseServer = class(TRALServer)
  private
    FHttp : THttpServer;
  protected
    function CreateRALSSL: TRALSSL; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetSessionTimeout(const Value: IntegerRAL); override;

    procedure DecodeAuth(AHeaders: TStringList; AResult: TRALRequest);
    function OnCommandProcess(AContext : THttpServerRequestAbstract): Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALSynopseServer }

procedure TRALSynopseServer.SetActive(const AValue : boolean);
begin
  inherited;
  if AValue then begin
    FHttp := THttpServer.Create(IntToStr(Port),nil,nil,'');
    FHttp.OnRequest := {$IFDEF FPC}@{$ENDIF}OnCommandProcess;
    if SSL.Enabled then begin
      with SSL as TRALSynopseSSL do
        FHttp.WaitStarted(30,CertificateFile,PrivateKeyFile,PrivateKeyPassword,CACertificatesFile);
    end
    else begin
      FHttp.WaitStarted;
    end;
  end
  else begin
    if FHttp <> nil then
      FHttp.Free;
  end;
end;

procedure TRALSynopseServer.SetPort(const AValue : IntegerRAL);
var
  vActive: boolean;
begin
  inherited;
  vActive := Self.Active;

  Active := False;

  Active := vActive;
end;

procedure TRALSynopseServer.SetSessionTimeout(const Value: IntegerRAL);
begin
  inherited;
end;

procedure TRALSynopseServer.DecodeAuth(AHeaders : TStringList; AResult : TRALRequest);
var
  vStr, vAux: StringRAL;
  vInt: IntegerRAL;
begin
  if Authentication = nil then
    Exit;

  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vStr := AHeaders.Values['Authorization'];
  if vStr <> '' then begin
    vInt := Pos(' ', vStr);
    vAux := Trim(Copy(vStr, 1, vInt - 1));
    if SameText(vAux,'Basic') then
      AResult.Authorization.AuthType := ratBasic
    else if SameText(vAux,'Bearer') then
      AResult.Authorization.AuthType := ratBearer;
    AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
  end;
end;

function TRALSynopseServer.CreateRALSSL : TRALSSL;
begin
  inherited;
  Result := TRALSynopseSSL.Create;
end;

function TRALSynopseServer.OnCommandProcess(AContext : THttpServerRequestAbstract) : Cardinal;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt : IntegerRAL;
  vStringList : TStringList;
  vParamQuery : StringRAL;
begin
  vRequest := TRALRequest.Create;
  try
    with vRequest do
    begin
      ClientInfo.IP := AContext.RemoteIP;
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := AContext.UserAgent;

      ContentType := AContext.InContentType;
      ContentSize := Length(AContext.InContent);

      Query := AContext.Url;
      vInt := Pos('?',Query);
      if vInt > 0 then begin
        vParamQuery := Copy(Query, vInt + 1, Length(Query));
        Query := Copy(Query, 1, vInt - 1);

        Params.DecodeQuery(vParamQuery);
      end;

      Method := amGET;
      if SameText(AContext.Method,'POST') then
        Method := amPOST
      else if SameText(AContext.Method,'DELETE') then
        Method := amDELETE
      else if SameText(AContext.Method,'PUT') then
        Method := amPUT
      else if SameText(AContext.Method,'OPTION') then
        Method := amPUT;

      vStringList := TStringList.Create;
      try
        vStringList.NameValueSeparator := ':';
        vStringList.Text := AContext.InHeaders;

        for vInt := 0 to vStringList.Count - 1 do
          vStringList.ValueFromIndex[vInt] := TrimLeft(vStringList.ValueFromIndex[vInt]);

        DecodeAuth(vStringList,vRequest);
        Params.AppendParams(vStringList, rpkHEADER);
      finally
        FreeAndNil(vStringList);
      end;

      Params.DecodeBody(AContext.InContent, AContext.InContentType);
    end;

    vResponse := ProcessCommands(vRequest);

    try
      vStringList := TStringList.Create;
      try
        vResponse.Params.AcquireParams(vStringList, rpkHEADER);
        vStringList.NameValueSeparator := ':';
        AContext.OutCustomHeaders := vStringList.Text;
      finally
        FreeAndNil(vStringList);
      end;

      AContext.OutContent := vResponse.ResponseText;
      AContext.OutContentType := vResponse.ContentType;

      Result := vResponse.RespCode;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    FreeAndNil(vRequest);
  end;
end;

constructor TRALSynopseServer.Create(AOwner : TComponent);
begin
  inherited;
  FHttp := nil;
  SetEngine('Synopse ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

destructor TRALSynopseServer.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

end.
