unit RALCGIServerDatamodule;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$IFDEF FPC}
    httpdefs, fpHTTP, fpWeb, httpprotocol, webutil,
  {$ELSE}
    HTTPApp,
  {$ENDIF}
  RALServer, RALRequest, RALResponse, RALTools, RALTypes, RALConsts;

type
  {$IFDEF FPC}
    TWebModule = class(TFPWebModule);
    TWebRequest = TRequest;
    TWebResponse = TResponse;
  {$ENDIF}

  { TRALServerCGI }

  TRALServerCGI = class(TRALServer)
  public
    constructor Create(AOwner : TComponent); override;
  end;

  { TRALWebModule }

  TRALWebModule = class(TWebModule)
    procedure DataModuleCreate(Sender : TObject);
    procedure DataModuleDestroy(Sender : TObject);
    procedure HandlerAction(Sender: TObject; ARequest: TWebRequest;
                            AResponse: TWebResponse; var AHandled: Boolean);
  private
    { Private declarations }
    FRALServer : TRALServerCGI;
    {$IFDEF FPC}
      procedure DecodeAuth(ARequest: TWebRequest; AResult: TRALRequest);
    {$ENDIF}
  public
    { Public declarations }
    {$IFDEF FPC}
      function RequestToHtml(ARequest : TWebRequest) : TStringList;
    {$ENDIF}
    property RALServer : TRALServerCGI read FRALServer;
  end;

{$IFNDEF FPC}
var
  RALWebModule : TComponentClass = TRALWebModule;
{$ENDIF}

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{ TRALServerCGI }

constructor TRALServerCGI.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF FPC}
    SetEngine('CGI Lazarus');
  {$ELSE}
    SetEngine('CGI Delphi');
  {$ENDIF}
end;

{ TRALWebModule }

procedure TRALWebModule.DataModuleCreate(Sender : TObject);
begin
  FRALServer := TRALServerCGI.Create(nil);
  FRALServer.BruteForceProtection.Enabled := False;
end;

procedure TRALWebModule.DataModuleDestroy(Sender : TObject);
begin
  FRALServer.Free;
end;

{$IFDEF FPC}
  procedure TRALWebModule.DecodeAuth(ARequest: TWebRequest; AResult: TRALRequest);
  var
    vStr, vAux: StringRAL;
    vInt: IntegerRAL;
  begin
    if FRALServer.Authentication = nil then
      Exit;

    AResult.Authorization.AuthType := ratNone;
    AResult.Authorization.AuthString := '';

    vStr := ARequest.GetHeader(hhAuthorization);
    if vStr <> '' then begin
      vInt := Pos(' ', vStr);
      vAux := Trim(Copy(vStr, 1, vInt - 1));
      if SameText(vAux, 'Basic') then
        AResult.Authorization.AuthType := ratBasic
      else if SameText(vAux, 'Bearer') then
        AResult.Authorization.AuthType := ratBearer;
      AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
    end;
  end;

  function TRALWebModule.RequestToHtml(ARequest : TWebRequest) : TStringList;
  begin
    Result := TStringList.Create;
    DumpRequest(ARequest, Result, True);
  end;

  procedure TRALWebModule.HandlerAction(Sender: TObject; ARequest: TWebRequest;
                                        AResponse: TWebResponse; var AHandled: Boolean);
  var
    vRequest: TRALRequest;
    vResponse: TRALResponse;
    vInt: integer;
    vStr1, vStr2: StringRAL;
    vConnClose : boolean;
  begin
    AHandled := True;

    vRequest := TRALRequest.Create;
    try
      with vRequest do
      begin
        ClientInfo.IP := ARequest.RemoteAddress;
        if ClientInfo.IP = '' then
          ClientInfo.IP := ARequest.RemoteHost;

        ClientInfo.MACAddress := '';
        ClientInfo.UserAgent := ARequest.UserAgent;

        Query := ARequest.PathInfo;
        Params.AppendParamsUrl(ARequest.QueryString, rpkQUERY);

        Method := HTTPMethodToRALMethod(ARequest.Method);

        ContentType := ARequest.ContentType;
        ContentSize := ARequest.ContentLength;
        ContentEncoding := ARequest.ContentEncoding;
        AcceptEncoding := ARequest.AcceptEncoding;

        DecodeAuth(ARequest, vRequest);
        Params.AppendParams(ARequest.CustomHeaders, rpkHEADER);

        // fields tambem
        vInt := 0;
        while vInt < ARequest.FieldCount do
        begin
          vStr1 := ARequest.FieldNames[vInt];
          vStr2 := ARequest.FieldValues[vInt];

          Params.AddParam(vStr1, vStr2, rpkFIELD);

          vInt := vInt + 1;
        end;

        Params.AppendParams(ARequest.QueryFields,rpkQUERY);
        Params.AppendParams(ARequest.CookieFields,rpkCOOKIE);

        ContentEncription := ParamByName('Content-Encription').AsString;
        AcceptEncription := ParamByName('Accept-Encription').AsString;;

        Params.CompressType := ContentCompress;
        Params.CriptoOptions.CriptType := ContentCripto;
        Params.CriptoOptions.Key := FRALServer.CriptoOptions.Key;
        Stream := Params.DecodeBody(ARequest.Content, ARequest.ContentType);

        Host := ARequest.Host;
        vInt := Pos('/', ARequest.ProtocolVersion);
        if vInt > 0 then
        begin
          HttpVersion := Copy(ARequest.ProtocolVersion, 1, vInt-1);
          Protocol := Copy(ARequest.ProtocolVersion, vInt+1, 3);
        end
        else begin
          HttpVersion := 'HTTP';
          Protocol := '1.0';
        end;

        vConnClose := False;
        if Protocol = '1.0' then
          vConnClose := True;
        if SameText(ARequest.GetHeader(hhConnection), 'close') then
          vConnClose := True;

        ARequest.Content := '';
        ARequest.QueryFields.Clear;
        ARequest.CustomHeaders.Clear;
        ARequest.CookieFields.Clear;
        ARequest.Files.Clear;
      end;

      vResponse := FRALServer.ProcessCommands(vRequest);

      try
        with vResponse do
        begin
          AResponse.Code := StatusCode;

          if vResponse.ContentEncoding <> '' then
            AResponse.ContentEncoding := vResponse.ContentEncoding;

          if vResponse.AcceptEncoding <> '' then
            Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

          if vResponse.ContentEncription <> '' then
            Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

          AResponse.Server := 'RAL_CGILaz';
          if vConnClose then
            AResponse.Connection := 'close';

          Params.AssignParams(AResponse.CustomHeaders, rpkHEADER);
          Params.AssignParams(AResponse.CookieFields, rpkCOOKIE);

          AResponse.ContentStream := ResponseStream;

          AResponse.FreeContentStream := vResponse.FreeContent;
          AResponse.ContentType := ContentType;

          AResponse.SendContent;
        end;
      finally
        FreeAndNil(vResponse);
      end;
    finally
      FreeAndNil(vRequest);
    end;
  end;
{$ELSE}
  procedure TRALWebModule.HandlerAction(Sender: TObject; ARequest: TWebRequest;
                                        AResponse: TWebResponse; var AHandled: Boolean);
  var
    vRequest : TRALRequest;
    vResponse : TRALResponse;
  begin
    AHandled := True;

    vRequest := TRALRequest.Create;
    try
      with vRequest do
      begin
        ClientInfo.IP := ARequest.RemoteIP;
        ClientInfo.MACAddress := '';
        ClientInfo.UserAgent := ARequest.UserAgent;

        ContentType := ARequest.ContentType;
        ContentEncoding := ARequest.ContentEncoding;
        AcceptEncoding := ARequest.GetFieldByName('Accept-Encoding');
        ContentSize := ARequest.ContentLength;

        Query := ARequest.PathInfo;

        Method := HTTPMethodToRALMethod(ARequest.Method);

        ContentEncription := ParamByName('Content-Encription').AsString;
        AcceptEncription := ParamByName('Accept-Encription').AsString;;

        Params.CompressType := ContentCompress;
        Params.CriptoOptions.CriptType := ContentCripto;
        Params.CriptoOptions.Key := FRALServer.CriptoOptions.Key;
        Stream := Params.DecodeBody(ARequest.Content, ARequest.ContentType);

        Host := Request.Host;
        HttpVersion := 'HTTP';
        Protocol := '1.0';
      end;

      vResponse := FRALServer.ProcessCommands(vRequest);
      try
        Response.ContentStream := vResponse.ResponseStream;
        Response.ContentLength := Response.ContentStream.Size;
        Response.FreeContentStream := vResponse.FreeContent;
      finally
        vResponse.Free;
      end;
    finally
      vRequest.Free;
    end;
  end;
{$ENDIF}

end.
