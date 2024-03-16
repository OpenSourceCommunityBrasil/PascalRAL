unit RALDBFiredacDAO;

interface

uses
  System.SysUtils, System.Classes, Data.DB, System.TypInfo, System.Variants,
  FireDAC.comp.Client, FireDAC.Stan.Intf,
  RALClient, RALTypes, RALServer, RALWebModule, RALRequest, RALResponse;

type

  [ComponentPlatforms(pidAllPlatforms)]
  TRALFDQuery = class(TFDQuery)
  private
    vRALClient: TRALClient;
    vRALClientClone: TRALClient;
    vRALFDConnectionServer: StringRAL;
    vRowsAffectedRemote: Int64;
    vIP: StringRAL;
    vPort: StringRAL;
    procedure SetRALFDConnectionServer(const value: StringRAL);
    procedure SetRALClient(const value: TRALClient);
    procedure RebuildParams(ParamCount: IntegerRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenRemote;
    procedure ApplyUpdatesRemote;
    procedure ExecSQLRemote;
  published
    property IP: StringRAL read vIP write vIP;
    property Port: StringRAL read vPort write vPort;
    property RowsAffectedRemote: Int64RAL read vRowsAffectedRemote;
    property RALClient: TRALClient read vRALClient write SetRALClient;
    property RALFDConnectionServer: StringRAL read vRALFDConnectionServer
      write SetRALFDConnectionServer;
  end;

  TOnQueryError = procedure(ASender, AInitiator: TObject; var AException: Exception)
    of object;
  TOnQueryAfterOpen = procedure(DataSet: TDataSet) of object;

  [ComponentPlatforms(pidAllPlatforms)]
  TRALFDConnection = class(TFDConnection)
  private
    vRALServer: TRALServer;
    vRALWebModule: TRALWebModule;
    vOnQueryError: TOnQueryError;
    vOnQueryAfterOpen: TOnQueryAfterOpen;
    vDriverName: StringRAL;
    procedure SetOnQueryError(const value: TOnQueryError);
    procedure SetOnQueryAfterOpen(const value: TOnQueryAfterOpen);
    procedure SetRALServer(const value: TRALServer);
    procedure SetDriverName(const value: StringRAL);
    procedure OnReplyQuery(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RALServer: TRALServer read vRALServer write SetRALServer;
    property OnQueryError: TOnQueryError read vOnQueryError write SetOnQueryError;
    property OnQueryAfterOpen: TOnQueryAfterOpen read vOnQueryAfterOpen
      write SetOnQueryAfterOpen;
    property DriverName: StringRAL read vDriverName write SetDriverName;
  end;

resourcestring
  emTypeNotImplemented = 'Type not Implemented.';
  emInvalidServer = 'RALServer not configured.';

procedure Register;

implementation

uses
  RALRoutes;

{ TRESTDWClientSQLFD }

constructor TRALFDQuery.Create(AOwner: TComponent);
begin
  vRALClient := nil;

  inherited;
end;

destructor TRALFDQuery.Destroy;
begin
  if Assigned(vRALClientClone) then
    FreeAndNil(vRALClientClone);

  inherited;
end;

procedure TRALFDQuery.ExecSQLRemote;
var
  vBinaryWriter: TBinaryWriter;
  vStreamAux: TMemoryStream;
  vStringStreamAux: TStringStream;
  vBytesAux: TArray<Byte>;
  i, x, t, id: integer;
begin
  try
    try
      vBinaryWriter := nil;
      vStreamAux := nil;
      vStringStreamAux := nil;

      if Assigned(vRALClientClone) then
      begin
        FreeAndNil(vRALClientClone);
      end;

      vRALClientClone := vRALClient.Clone(Self);
      vStreamAux := TMemoryStream.Create;
      vBinaryWriter := TBinaryWriter.Create(vStreamAux);
      vStringStreamAux := TStringStream.Create(SQL.Text, TEncoding.UTF8);
      RebuildParams(Self.Params.Count);
      vStringStreamAux.Position := 0;
      vRALClientClone.Request.Params.AddParam('SQL', vStringStreamAux, rpkBODY);
      vRALClientClone.Request.Params.AddParam('ParamCount',
        Self.Params.Count.ToString, rpkBODY);

      for i := 0 to Self.Params.Count - 1 do
      begin
        SetLength(vBytesAux, Self.Params[i].GetDataSize);
        Self.Params[i].GetData(PByte(vBytesAux));

        if ((Self.Params[i].IsNull = false) and
          (VarType(Self.Params[i].value) = varString)) then
        begin
          t := 0;

          for x := Self.Params[i].GetDataSize - 1 downto 0 do
          begin
            if vBytesAux[x] = 0 then
              t := t + 1
            else
              break;
          end;

          SetLength(vBytesAux, Length(vBytesAux) - t);
        end;

        if Self.Params[i].IsNull then
          vRALClientClone.Request.Params.AddParam('N' + i.ToString, 'true', rpkBODY)
        else
          vRALClientClone.Request.Params.AddParam('N' + i.ToString, 'false', rpkBODY);

        TMemoryStream(vStreamAux).Clear;
        vBinaryWriter.Write(vBytesAux);
        vStreamAux.Position := 0;
        vRALClientClone.Request.Params.AddParam('P' + i.ToString, vStreamAux, rpkBODY);
        vRALClientClone.Request.Params.AddParam('F' + i.ToString,
          GetEnumName(Typeinfo(TFieldType), Ord(Self.Params[i].DataType)), rpkBODY);
      end;

      vRALClientClone.Request.Params.AddParam('Type', '2', rpkBODY);
      vRALClientClone.Post;
      if vRALClientClone.Response.StatusCode = 200 then
      begin
        Self.CommitUpdates;
        Self.vRowsAffectedRemote :=
          StrToInt(vRALClientClone.Response.ParamByName('AffectedRows').AsString);
      end
      else
        raise Exception.Create(vRALClientClone.Response.ResponseText);
    except
      on e: Exception do
      begin
        Self.Close;
        raise Exception.Create(e.Message);
      end;
    end;
  finally
    FreeAndNil(vRALClientClone);
    FreeAndNil(vBinaryWriter);
    FreeAndNil(vStreamAux);
    FreeAndNil(vStringStreamAux);
  end;
end;

procedure TRALFDQuery.ApplyUpdatesRemote;
var
  vAuxMemTable: TFDMemTable;
  vBinaryWriter: TBinaryWriter;
  vStreamAux: TMemoryStream;
  vStringStreamAux: TStringStream;
  vBytesAux: TArray<Byte>;
  i, x, t, id: integer;
begin
  try
    try
      vBinaryWriter := nil;
      vStreamAux := nil;
      vStringStreamAux := nil;
      vAuxMemTable := nil;

      if Assigned(vRALClientClone) then
      begin
        FreeAndNil(vRALClientClone);
      end;

      vRALClientClone := vRALClient.Clone(Self);
      vAuxMemTable := TFDMemTable.Create(Self);
      vStreamAux := TMemoryStream.Create;
      vBinaryWriter := TBinaryWriter.Create(vStreamAux);
      vStringStreamAux := TStringStream.Create(SQL.Text, TEncoding.UTF8);
      RebuildParams(Self.Params.Count);
      vStringStreamAux.Position := 0;
      vRALClientClone.Request.Params.AddParam('SQL', vStringStreamAux, rpkBODY);
      vRALClientClone.Request.Params.AddParam('ParamCount',
        Self.Params.Count.ToString, rpkBODY);

      for i := 0 to Self.Params.Count - 1 do
      begin
        SetLength(vBytesAux, Self.Params[i].GetDataSize);
        Self.Params[i].GetData(PByte(vBytesAux));

        if ((Self.Params[i].IsNull = false) and
          (VarType(Self.Params[i].value) = varString)) then
        begin
          t := 0;
          for x := Self.Params[i].GetDataSize - 1 downto 0 do
          begin
            if vBytesAux[x] = 0 then
              t := t + 1
            else
              break;
          end;
          SetLength(vBytesAux, Length(vBytesAux) - t);
        end;

        if Self.Params[i].IsNull then
          vRALClientClone.Request.Params.AddParam('N' + i.ToString, 'true', rpkBODY)
        else
          vRALClientClone.Request.Params.AddParam('N' + i.ToString, 'false', rpkBODY);

        TMemoryStream(vStreamAux).Clear;
        vBinaryWriter.Write(vBytesAux);
        vStreamAux.Position := 0;
        vRALClientClone.Request.Params.AddParam('P' + i.ToString, vStreamAux, rpkBODY);
        vRALClientClone.Request.Params.AddParam('F' + i.ToString,
          GetEnumName(Typeinfo(TFieldType), Ord(Self.Params[i].DataType)), rpkBODY);
      end;

      vRALClientClone.Request.Params.AddParam('Type', '1', rpkBODY);
      vStreamAux.Clear;
      vAuxMemTable.Data := Self.Delta;
      vAuxMemTable.SaveToStream(vStreamAux, sfBinary);
      vStreamAux.Position := 0;
      vRALClientClone.Request.Params.AddParam('Stream', vStreamAux, rpkBODY);
      vRALClientClone.Post;

      if vRALClientClone.Response.StatusCode = 200 then
      begin
        Self.CommitUpdates;

        Self.vRowsAffectedRemote :=
          StrToInt(vRALClientClone.Response.ParamByName('AffectedRows').AsString);
      end
      else
        raise Exception.Create(vRALClientClone.Response.ResponseText);
    except
      on e: Exception do
      begin
        Self.Close;
        raise Exception.Create(e.Message);
      end;
    end;
  finally
    FreeAndNil(vRALClientClone);
    FreeAndNil(vAuxMemTable);
    FreeAndNil(vBinaryWriter);
    FreeAndNil(vStreamAux);
    FreeAndNil(vStringStreamAux);
  end;
end;

procedure TRALFDQuery.OpenRemote;
var
  vBinaryWriter: TBinaryWriter;
  vStreamAux: TStream;
  vStringStreamAux: TStringStream;
  vBytesAux: TArray<Byte>;
  i, x, t, id: integer;
begin
  try
    try
      vBinaryWriter := nil;
      vStreamAux := nil;
      vStringStreamAux := nil;

      if Assigned(vRALClientClone) then
      begin
        FreeAndNil(vRALClientClone);
      end;

      vRALClientClone := vRALClient.Clone(Self);
      vStreamAux := TMemoryStream.Create;
      vBinaryWriter := TBinaryWriter.Create(vStreamAux);
      vStringStreamAux := TStringStream.Create(SQL.Text, TEncoding.UTF8);
      RebuildParams(Self.Params.Count);
      vStringStreamAux.Position := 0;
      vRALClientClone.Request.Params.AddParam('SQL', vStringStreamAux, rpkBODY);
      vRALClientClone.Request.Params.AddParam('ParamCount',
        Self.Params.Count.ToString, rpkBODY);

      for i := 0 to Self.Params.Count - 1 do
      begin
        SetLength(vBytesAux, Self.Params[i].GetDataSize);
        Self.Params[i].GetData(PByte(vBytesAux));

        if ((Self.Params[i].IsNull = false) and
          (VarType(Self.Params[i].value) = varString)) then
        begin
          t := 0;
          for x := Self.Params[i].GetDataSize - 1 downto 0 do
          begin
            if vBytesAux[x] = 0 then
              t := t + 1
            else
              break;
          end;
          SetLength(vBytesAux, Length(vBytesAux) - t);
        end;

        if Self.Params[i].IsNull then
          vRALClientClone.Request.Params.AddParam('N' + i.ToString, 'true', rpkBODY)
        else
          vRALClientClone.Request.Params.AddParam('N' + i.ToString, 'false', rpkBODY);

        TMemoryStream(vStreamAux).Clear;
        vBinaryWriter.Write(vBytesAux);
        vStreamAux.Position := 0;
        vRALClientClone.Request.Params.AddParam('P' + i.ToString, vStreamAux, rpkBODY);
        vRALClientClone.Request.Params.AddParam('F' + i.ToString,
          GetEnumName(Typeinfo(TFieldType), Ord(Self.Params[i].DataType)), rpkBODY);
      end;

      vRALClientClone.Request.Params.AddParam('Type', '0', rpkBODY);
      vRALClientClone.Post;

      if vRALClientClone.Response.StatusCode = 200 then
      begin
        TMemoryStream(vStreamAux).Clear;
        vRALClientClone.Response.ParamByName('Stream').SaveToStream(vStreamAux);
        vStreamAux.Position := 0;

        if Assigned(Self.Connection) = false then
        begin
          Self.Connection := TFDConnection.Create(Self);
        end;

        Self.LoadFromStream(vStreamAux, TFDStorageFormat.sfBinary);
        Self.vRowsAffectedRemote :=
          StrToInt(vRALClientClone.Response.ParamByName('AffectedRows').AsString);
        Self.CachedUpdates := true;
      end
      else
        raise Exception.Create(vRALClientClone.Response.ResponseText);
    except
      on e: Exception do
      begin
        Self.Close;
        raise Exception.Create(e.Message);
      end;
    end;
  finally
    FreeAndNil(vRALClientClone);
    FreeAndNil(vBinaryWriter);
    FreeAndNil(vStreamAux);
    FreeAndNil(vStringStreamAux);
  end;
end;

procedure TRALFDQuery.SetRALClient(const value: TRALClient);
begin
  if Assigned(vRALClientClone) then
    FreeAndNil(vRALClientClone);
  vRALClient := value;
end;

procedure TRALFDQuery.SetRALFDConnectionServer(const value: StringRAL);
begin
  vRALFDConnectionServer := value;
end;

procedure TRALFDQuery.RebuildParams(ParamCount: integer);
begin
  vRALClientClone.BaseURL := vIP + ':' + vPort;
  vRALClientClone.SetRoute(vRALFDConnectionServer + 'Route/Query');
end;

{ TRESTDWConnectionFD }

constructor TRALFDConnection.Create(AOwner: TComponent);
begin
  vRALWebModule := nil;
  inherited;
end;

destructor TRALFDConnection.Destroy;
begin
  if Assigned(vRALWebModule) then
  begin
    FreeAndNil(vRALWebModule);
  end;
  inherited;
end;

procedure TRALFDConnection.OnReplyQuery(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vQueryAux, vQueryAux2: TFDQuery;
  vAuxStream: TStream;
  vBinaryReader: TBinaryReader;
  vAuxParamStream: TStream;
  vAuxStringStream: TStream;
  vAuxPBytes: TArray<Byte>;
  i: integer;
  vAuxConnClone: TFDConnection;
begin
  try
    try
      vQueryAux := nil;
      vQueryAux2 := nil;
      vBinaryReader := nil;
      vAuxStream := nil;
      vAuxStringStream := nil;
      vAuxParamStream := nil;
      vAuxConnClone := nil;

      vAuxConnClone := TFDConnection(Self.CloneConnection);
      vAuxStream := TMemoryStream.Create;
      vAuxParamStream := TMemoryStream.Create;
      vBinaryReader := TBinaryReader.Create(vAuxParamStream);
      vAuxStringStream := TStringStream.Create('', TEncoding.UTF8);

      ARequest.ParamByName('SQL').SaveToStream(vAuxStringStream);
      vAuxStringStream.Position := 0;

      vQueryAux := TFDQuery.Create(Self);
      vQueryAux.Connection := vAuxConnClone;
      vQueryAux.SQL.Text := TStringStream(vAuxStringStream).DataString;

      if ARequest.ParamByName('Type').AsString = '1' then
      begin
        vQueryAux2 := TFDQuery.Create(Self);
        vQueryAux2.Connection := vAuxConnClone;
        vQueryAux2.SQL.Text := TStringStream(vAuxStringStream).DataString;
      end;

      if StrToInt(ARequest.ParamByName('ParamCount').AsString) > 0 then
      begin
        for i := 0 to StrToInt(ARequest.ParamByName('ParamCount').AsString) - 1 do
        begin
          TMemoryStream(vAuxParamStream).Clear;
          vAuxParamStream.Position := 0;
          ARequest.ParamByName('P' + i.ToString).SaveToStream(vAuxParamStream);
          vAuxParamStream.Position := 0;
          vAuxPBytes := vBinaryReader.ReadBytes(vAuxParamStream.Size);
          vAuxParamStream.Position := 0;
          vQueryAux.Params.Add;
          vQueryAux.Params[i].DataType :=
            TFieldType(GetEnumValue(Typeinfo(TFieldType),
            ARequest.ParamByName('F' + i.ToString).AsString));
          vQueryAux.Params[i].SetData(PByte(vAuxPBytes), Length(vAuxPBytes));

          if ARequest.ParamByName('N' + i.ToString).AsString = 'true' then
            vQueryAux.Params[i].Clear;

          if ARequest.ParamByName('Type').AsString = '1' then
          begin
            vQueryAux2.Params.Add;
            vQueryAux2.Params[i].DataType :=
              TFieldType(GetEnumValue(Typeinfo(TFieldType),
              ARequest.ParamByName('F' + i.ToString).AsString));
            vQueryAux2.Params[i].SetData(PByte(vAuxPBytes), Length(vAuxPBytes));

            if ARequest.ParamByName('N' + i.ToString).AsString = 'true' then
              vQueryAux2.Params[i].Clear;
          end;
        end;
      end;

      if Assigned(vOnQueryError) then
        vQueryAux.OnError := vOnQueryError;

      if Assigned(vOnQueryAfterOpen) then
        vQueryAux.AfterOpen := vOnQueryAfterOpen;

      if ARequest.ParamByName('Type').AsString = '1' then
      begin
        vQueryAux.Close;
        vQueryAux2.Close;
        vQueryAux.CachedUpdates := true;
        vQueryAux2.CachedUpdates := true;
        vQueryAux.Open;
        TMemoryStream(vAuxStream).Clear;
        ARequest.ParamByName('Stream').SaveToStream(vAuxStream);
        vAuxStream.Position := 0;
        vQueryAux2.LoadFromStream(vAuxStream);
        vQueryAux.MergeDataSet(vQueryAux2, dmDeltaMerge);
        vQueryAux.ApplyUpdates;
        AResponse.Params.AddParam('AffectedRows',
          vQueryAux2.Delta.DataView.Rows.Count.ToString, rpkBODY);
        vQueryAux.CommitUpdates;
        vQueryAux.CachedUpdates := false;
      end
      else if ARequest.ParamByName('Type').AsString = '0' then
      begin
        vQueryAux.Open;
        vQueryAux.SaveToStream(vAuxStream, TFDStorageFormat.sfBinary);
        vAuxStream.Position := 0;
        AResponse.Params.AddParam('Stream', vAuxStream, rpkBODY);
        AResponse.Params.AddParam('AffectedRows',
          vQueryAux.RowsAffected.ToString, rpkBODY);
        vQueryAux.Close;
      end
      else if ARequest.ParamByName('Type').AsString = '2' then
      begin
        vQueryAux.ExecSQL;
        AResponse.Params.AddParam('AffectedRows',
          vQueryAux.RowsAffected.ToString, rpkBODY);
        vQueryAux.Close;
      end
      else
      begin
        AResponse.StatusCode := 501;
        raise Exception.Create(emTypeNotImplemented);
      end;

      AResponse.StatusCode := 200;
    except
      on e: Exception do
      begin
        if AResponse.StatusCode <> 501 then
          AResponse.StatusCode := 500;
        AResponse.ResponseText := e.Message;
      end;
    end
  finally
    FreeAndNil(vQueryAux);
    FreeAndNil(vAuxStream);
    FreeAndNil(vBinaryReader);
    FreeAndNil(vAuxStringStream);
    FreeAndNil(vAuxParamStream);
    FreeAndNil(vAuxConnClone);
  end;
end;

procedure TRALFDConnection.SetDriverName(const value: StringRAL);
begin
  TFDConnection(Self).DriverName := value;
end;

procedure TRALFDConnection.SetOnQueryAfterOpen(const value: TOnQueryAfterOpen);
begin
  vOnQueryAfterOpen := value;
end;

procedure TRALFDConnection.SetOnQueryError(const value: TOnQueryError);
begin
  vOnQueryError := value;
end;

procedure TRALFDConnection.SetRALServer(const value: TRALServer);
var
  vRALRoute: TRALRoute;
begin
  vRALServer := value;

  if not(csDesigning in Self.Owner.ComponentState) then
  begin
    if not(Assigned(vRALServer)) then
      raise Exception.Create(emInvalidServer);

    if Assigned(vRALWebModule) then
    begin
      FreeAndNil(vRALWebModule);
    end;

    vRALWebModule := TRALWebModule.Create(Self);
    vRALWebModule.Server := vRALServer;

    vRALRoute := vRALWebModule.CreateRoute(Self.Name + 'Route/Query', OnReplyQuery);
  end;

end;

{ Register Components }

procedure Register;
begin
  RegisterComponents('RAL - DAO', [TRALFDQuery, TRALFDConnection]);
end;

end.
