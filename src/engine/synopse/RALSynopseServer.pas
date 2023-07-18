unit RALSynopseServer;

interface

uses
  Classes, SysUtils, syncobjs,
  mormot.net.server, mormot.net.http, mormot.net.async, mormot.core.os,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRoutes;

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
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    function CreateRALSSL: TRALSSL; override;

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

function TRALSynopseServer.CreateRALSSL : TRALSSL;
begin
  inherited;
  Result := TRALSynopseSSL.Create;
end;

function TRALSynopseServer.OnCommandProcess(AContext : THttpServerRequestAbstract) : Cardinal;
begin
  AContext.OutContent := 'Fernando';
  AContext.OutContentType := TEXT_CONTENT_TYPE;
  Result := HTTP_SUCCESS;
end;

constructor TRALSynopseServer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FHttp := nil;
end;

destructor TRALSynopseServer.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

end.
