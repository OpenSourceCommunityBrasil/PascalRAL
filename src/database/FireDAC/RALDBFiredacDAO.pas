unit RALDBFiredacDAO;

interface

{$I ..\..\base\PascalRAL.inc}

uses
  System.SysUtils, System.Classes, Data.DB, System.TypInfo, System.Variants,
  Firedac.Stan.Def, Firedac.Stan.StorageBin, Firedac.DApt, Firedac.comp.Client,
  Firedac.comp.DataSet, {$IFDEF HAS_FMX}Firedac.FMXUI.Wait, {$ELSE}Firedac.VCLUI.Wait,
{$ENDIF}
  Firedac.Stan.Intf,
  RALClient, RALRoutes, RALTypes, RALServer, RALWebModule, RALRequest, RALResponse,
  System.SyncObjs;

type

  TOnQueryRemoteFinish = procedure(ASender: TObject; AException: Exception) of object;

  [ComponentPlatforms(pidAllPlatforms)]
  TRALFDQuery = class(TFDQuery)
  private
    vQueryBehavior: TRALExecBehavior;
    vRALClient: TRALClientMT;
    vRALFDConnectionServer: StringRAL;
    vRowsAffectedRemote: Int64;
    vOnQueryRemoteFinish: TOnQueryRemoteFinish;
    vException: Exception;
    procedure SetRALFDConnectionServer(const value: StringRAL);
    procedure SetRALClient(const value: TRALClientMT);
    procedure ExecSQLRemoteResponse(Sender: TObject; AResponse: TRALResponse;
      AException: StringRAL);
    procedure ApplyUpdatesRemoteResponse(Sender: TObject; AResponse: TRALResponse;
      AException: StringRAL);
    procedure OpenRemoteResponse(Sender: TObject; AResponse: TRALResponse;
      AException: StringRAL);
    procedure SetOnQueryRemoteFinish(const value: TOnQueryRemoteFinish);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyUpdatesRemote;
    procedure ExecSQLRemote;
    procedure OpenRemote;
  published
    property QueryBehavior: TRALExecBehavior read vQueryBehavior write vQueryBehavior
      default ebMultiThread;
    property RALClient: TRALClientMT read vRALClient write SetRALClient;
    property RALFDConnectionServer: StringRAL read vRALFDConnectionServer
      write SetRALFDConnectionServer;
    property RowsAffectedRemote: Int64RAL read vRowsAffectedRemote;
    property OnQueryRemoteFinish: TOnQueryRemoteFinish read vOnQueryRemoteFinish
      write SetOnQueryRemoteFinish;
  end;

  TOnQueryError = procedure(ASender, AInitiator: TObject; var AException: Exception)
    of object;
  TOnQueryAfterOpen = procedure(DataSet: TDataSet) of object;

  [ComponentPlatforms(pidAllPlatforms)]
  TRALFDConnection = class(TFDConnection)
  private
    vDriverName: StringRAL;
    vRALServer: TRALServer;
    vRALWebModule: TRALWebModule;
    vOnQueryError: TOnQueryError;
    vOnQueryAfterOpen: TOnQueryAfterOpen;
    procedure SetDriverName(const value: StringRAL);
    procedure SetOnQueryError(const value: TOnQueryError);
    procedure SetOnQueryAfterOpen(const value: TOnQueryAfterOpen);
    procedure SetRALServer(const value: TRALServer);
    procedure OnReplyQuery(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DriverName: StringRAL read vDriverName write SetDriverName;
    property OnQueryError: TOnQueryError read vOnQueryError write SetOnQueryError;
    property OnQueryAfterOpen: TOnQueryAfterOpen read vOnQueryAfterOpen
      write SetOnQueryAfterOpen;
    property RALServer: TRALServer read vRALServer write SetRALServer;
  end;

resourcestring
  emTypeNotImplemented = 'Type not Implemented.';
  emInvalidServer = 'RALServer not configured.';

procedure Register;

implementation

{ TRALFDQueryMT }

constructor TRALFDQuery.Create(AOwner: TComponent);
begin
  vRALClient := nil;
  vException := nil;

  vQueryBehavior := ebMultiThread;

  inherited;
end;

destructor TRALFDQuery.Destroy;
begin
  if Assigned(vException) then
    FreeAndNil(vException);

  inherited;
end;

procedure TRALFDQuery.ExecSQLRemoteResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
begin
  try
    try
      if AException <> '' then
        raise Exception.Create(AException);

      if AResponse.StatusCode = 200 then
      begin
        Self.vRowsAffectedRemote := StrToInt(AResponse.ParamByName('AffectedRows')
          .AsString);
      end
      else
        raise Exception.Create(AResponse.ResponseText);
    except
      on e: Exception do
      begin
        Self.Close;

        vException := e.Create(e.Message);
      end;
    end;
  finally
    if Assigned(OnQueryRemoteFinish) then
      OnQueryRemoteFinish(Self, vException);
  end;
end;

procedure TRALFDQuery.ExecSQLRemote;
var
  vBinaryWriter: TBinaryWriter;
  vStreamAux: TMemoryStream;
  vStringStreamAux: TStringStream;
  vBytesAux: TArray<Byte>;
  i, x, t: integer;
  vRequest: TRALRequest;
begin
  try
    try
      vBinaryWriter := nil;
      vStreamAux := nil;
      vStringStreamAux := nil;
      vRequest := nil;

      if Assigned(vException) then
        FreeAndNil(vException);

      vRequest := RALClient.NewRequest;

      vStreamAux := TMemoryStream.Create;
      vBinaryWriter := TBinaryWriter.Create(vStreamAux);
      vStringStreamAux := TStringStream.Create(SQL.Text, TEncoding.UTF8);
      vStringStreamAux.Position := 0;

      vRequest.Params.AddParam('SQL', vStringStreamAux, rpkBODY);

      vRequest.Params.AddParam('ParamCount', Self.Params.Count.ToString, rpkBODY);

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
          vRequest.Params.AddParam('N' + i.ToString, 'true', rpkBODY)
        else
          vRequest.Params.AddParam('N' + i.ToString, 'false', rpkBODY);

        TMemoryStream(vStreamAux).Clear;
        vBinaryWriter.Write(vBytesAux);
        vStreamAux.Position := 0;

        vRequest.Params.AddParam('P' + i.ToString, vStreamAux, rpkBODY);

        vRequest.Params.AddParam('F' + i.ToString, GetEnumName(Typeinfo(TFieldType),
          Ord(Self.Params[i].DataType)), rpkBODY);
      end;

      vRequest.Params.AddParam('Type', '2', rpkBODY);

      vRALClient.ExecBehavior := Self.QueryBehavior;

      vRALClient.Post(vRALFDConnectionServer + 'Route/Query', vRequest,
        ExecSQLRemoteResponse);
    except
      on e: Exception do
      begin
        Self.Close;

        vException := e.Create(e.Message);
      end;
    end;
  finally
    // FreeAndNil(vRequest);
    FreeAndNil(vBinaryWriter);
    FreeAndNil(vStreamAux);
    FreeAndNil(vStringStreamAux);
    Finalize(vBytesAux);

    if QueryBehavior = ebSingleThread then
    begin
      if Assigned(vException) then
        raise vException;
    end;
  end;
end;

procedure TRALFDQuery.ApplyUpdatesRemoteResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
begin
  try
    try
      if AException <> '' then
        raise Exception.Create(AException);

      if AResponse.StatusCode = 200 then
      begin
        Self.CommitUpdates;

        Self.vRowsAffectedRemote := StrToInt(AResponse.ParamByName('AffectedRows')
          .AsString);
      end
      else
        raise Exception.Create(AResponse.ResponseText);
    except
      on e: Exception do
      begin
        Self.Close;

        vException := e.Create(e.Message);
      end;
    end;
  finally
    if Assigned(OnQueryRemoteFinish) then
      OnQueryRemoteFinish(Self, vException);
  end;
end;

procedure TRALFDQuery.ApplyUpdatesRemote;
var
  vAuxMemTable: TFDMemTable;
  vBinaryWriter: TBinaryWriter;
  vStreamAux: TMemoryStream;
  vStringStreamAux: TStringStream;
  vBytesAux: TArray<Byte>;
  i, x, t: integer;
  vRequest: TRALRequest;
begin
  try
    try
      vBinaryWriter := nil;
      vStreamAux := nil;
      vStringStreamAux := nil;
      vAuxMemTable := nil;
      vRequest := nil;

      if Assigned(vException) then
        FreeAndNil(vException);

      vRequest := RALClient.NewRequest;

      vAuxMemTable := TFDMemTable.Create(Self);
      vStreamAux := TMemoryStream.Create;
      vBinaryWriter := TBinaryWriter.Create(vStreamAux);
      vStringStreamAux := TStringStream.Create(SQL.Text, TEncoding.UTF8);
      vStringStreamAux.Position := 0;

      vRequest.Params.AddParam('SQL', vStringStreamAux, rpkBODY);

      vRequest.Params.AddParam('ParamCount', Self.Params.Count.ToString, rpkBODY);

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
          vRequest.Params.AddParam('N' + i.ToString, 'true', rpkBODY)
        else
          vRequest.Params.AddParam('N' + i.ToString, 'false', rpkBODY);

        TMemoryStream(vStreamAux).Clear;
        vBinaryWriter.Write(vBytesAux);
        vStreamAux.Position := 0;

        vRequest.Params.AddParam('P' + i.ToString, vStreamAux, rpkBODY);

        vRequest.Params.AddParam('F' + i.ToString, GetEnumName(Typeinfo(TFieldType),
          Ord(Self.Params[i].DataType)), rpkBODY);
      end;

      vRequest.Params.AddParam('Type', '1', rpkBODY);

      vStreamAux.Clear;

      vAuxMemTable.Data := Self.Delta;
      vAuxMemTable.SaveToStream(vStreamAux, sfBinary);
      vStreamAux.Position := 0;

      vRequest.Params.AddParam('Stream', vStreamAux, rpkBODY);

      vRALClient.ExecBehavior := Self.QueryBehavior;

      vRALClient.Post(vRALFDConnectionServer + 'Route/Query', vRequest,
        ApplyUpdatesRemoteResponse);
    except
      on e: Exception do
      begin
        Self.Close;

        vException := e.Create(e.Message);
      end;
    end;
  finally
    // FreeAndNil(vRequest);
    FreeAndNil(vAuxMemTable);
    FreeAndNil(vBinaryWriter);
    FreeAndNil(vStreamAux);
    FreeAndNil(vStringStreamAux);
    Finalize(vBytesAux);

    if QueryBehavior = ebSingleThread then
    begin
      if Assigned(vException) then
        raise vException;
    end;
  end;
end;

procedure TRALFDQuery.OpenRemoteResponse(Sender: TObject; AResponse: TRALResponse;
  AException: StringRAL);
var
  vStreamAux: TStream;
begin
  try
    try
      vStreamAux := nil;

      if AException <> '' then
        raise Exception.Create(AException);

      if AResponse.StatusCode = 200 then
      begin
        vStreamAux := TMemoryStream.Create;
        TMemoryStream(vStreamAux).Clear;
        vStreamAux.Position := 0;

        AResponse.ParamByName('Stream').SaveToStream(vStreamAux);
        vStreamAux.Position := 0;

        if Assigned(Self.Connection) = false then
        begin
          Self.Connection := TFDConnection.Create(Self);
        end;

        TThread.Synchronize(nil,
          procedure
          begin
            Self.LoadFromStream(vStreamAux, TFDStorageFormat.sfBinary);
          end);

        Self.vRowsAffectedRemote := StrToInt(AResponse.ParamByName('AffectedRows')
          .AsString);

        Self.CachedUpdates := true;
      end
      else
        raise Exception.Create(AResponse.ResponseText);
    except
      on e: Exception do
      begin
        Self.Close;

        vException := e.Create(e.Message);
      end;
    end;
  finally
    FreeAndNil(vStreamAux);

    if Assigned(OnQueryRemoteFinish) then
      OnQueryRemoteFinish(Self, vException);
  end;
end;

procedure TRALFDQuery.OpenRemote;
var
  vBinaryWriter: TBinaryWriter;
  vStreamAux: TStream;
  vStringStreamAux: TStringStream;
  vBytesAux: TArray<Byte>;
  i, x, t: integer;
  vRequest: TRALRequest;
begin
  try
    try
      vBinaryWriter := nil;
      vStreamAux := nil;
      vStringStreamAux := nil;
      vRequest := nil;

      if Assigned(vException) then
        FreeAndNil(vException);

      vRequest := RALClient.NewRequest;

      vStreamAux := TMemoryStream.Create;
      vBinaryWriter := TBinaryWriter.Create(vStreamAux);
      vStringStreamAux := TStringStream.Create(SQL.Text, TEncoding.UTF8);
      vStringStreamAux.Position := 0;

      vRequest.Params.AddParam('SQL', vStringStreamAux, rpkBODY);

      vRequest.Params.AddParam('ParamCount', Self.Params.Count.ToString, rpkBODY);

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
          vRequest.Params.AddParam('N' + i.ToString, 'true', rpkBODY)
        else
          vRequest.Params.AddParam('N' + i.ToString, 'false', rpkBODY);

        TMemoryStream(vStreamAux).Clear;
        vBinaryWriter.Write(vBytesAux);
        vStreamAux.Position := 0;

        vRequest.Params.AddParam('P' + i.ToString, vStreamAux, rpkBODY);

        vRequest.Params.AddParam('F' + i.ToString, GetEnumName(Typeinfo(TFieldType),
          Ord(Self.Params[i].DataType)), rpkBODY);
      end;

      vRequest.Params.AddParam('Type', '0', rpkBODY);

      vRALClient.ExecBehavior := Self.QueryBehavior;

      vRALClient.Post(vRALFDConnectionServer + 'Route/Query', vRequest,
        OpenRemoteResponse);
    except
      on e: Exception do
      begin
        Self.Close;

        vException := e.Create(e.Message);
      end;
    end;
  finally
    // FreeAndNil(vRequest);
    FreeAndNil(vBinaryWriter);
    FreeAndNil(vStreamAux);
    FreeAndNil(vStringStreamAux);
    Finalize(vBytesAux);

    if QueryBehavior = ebSingleThread then
    begin
      if Assigned(vException) then
        raise vException;
    end;
  end;
end;

procedure TRALFDQuery.SetOnQueryRemoteFinish(const value: TOnQueryRemoteFinish);
begin
  vOnQueryRemoteFinish := value;
end;

procedure TRALFDQuery.SetRALClient(const value: TRALClientMT);
begin
  vRALClient := value;
end;

procedure TRALFDQuery.SetRALFDConnectionServer(const value: StringRAL);
begin
  vRALFDConnectionServer := value;
end;

{ TRALFDConnection }

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
  vBytesAux: TArray<Byte>;
  vNeedAddParam: boolean;
  vAuxException: string;
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
      vAuxException := '';
      vNeedAddParam := true;

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
        if vQueryAux.Params.Count > 0 then
          vNeedAddParam := false;

        for i := 0 to StrToInt(ARequest.ParamByName('ParamCount').AsString) - 1 do
        begin
          TMemoryStream(vAuxParamStream).Clear;
          vAuxParamStream.Position := 0;

          ARequest.ParamByName('P' + i.ToString).SaveToStream(vAuxParamStream);
          vAuxParamStream.Position := 0;

          vBytesAux := vBinaryReader.ReadBytes(vAuxParamStream.Size);
          vAuxParamStream.Position := 0;

          if vNeedAddParam then
            vQueryAux.Params.Add;

          vQueryAux.Params[i].DataType :=
            TFieldType(GetEnumValue(Typeinfo(TFieldType),
            ARequest.ParamByName('F' + i.ToString).AsString));

          vQueryAux.Params[i].SetData(PByte(vBytesAux), Length(vBytesAux));

          if ARequest.ParamByName('N' + i.ToString).AsString = 'true' then
            vQueryAux.Params[i].Clear;

          if ARequest.ParamByName('Type').AsString = '1' then
          begin
            if vNeedAddParam then
              vQueryAux2.Params.Add;

            vQueryAux2.Params[i].DataType :=
              TFieldType(GetEnumValue(Typeinfo(TFieldType),
              ARequest.ParamByName('F' + i.ToString).AsString));

            vQueryAux2.Params[i].SetData(PByte(vBytesAux), Length(vBytesAux));

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
        vQueryAux.CachedUpdates := true;

        vQueryAux2.Close;
        vQueryAux2.CachedUpdates := true;

        vQueryAux.Open;

        TMemoryStream(vAuxStream).Clear;

        ARequest.ParamByName('Stream').SaveToStream(vAuxStream);
        vAuxStream.Position := 0;

        vQueryAux2.LoadFromStream(vAuxStream);

        vQueryAux.MergeDataSet(vQueryAux2, dmDeltaMerge);

        if vQueryAux.ApplyUpdates > 0 then
        begin
          vQueryAux.FilterChanges := [rtModified, rtInserted, rtDeleted, rtHasErrors];

          vQueryAux.First;

          while not(vQueryAux.Eof) do
          begin
            if Assigned(vQueryAux.RowError) then
            begin
              vAuxException := vQueryAux.RowError.Message;

              AResponse.Params.AddParam('AffectedRows', '0', rpkBODY);

              vQueryAux.CancelUpdates;

              vQueryAux.CachedUpdates := false;

              raise Exception.Create(vAuxException);
            end;

            vQueryAux.Next;
          end;
        end;

        AResponse.Params.AddParam('AffectedRows',
          vQueryAux2.Delta.DataView.Rows.Count.ToString, rpkBODY);
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
    FreeAndNil(vQueryAux2);
    FreeAndNil(vAuxStream);
    FreeAndNil(vBinaryReader);
    FreeAndNil(vAuxStringStream);
    FreeAndNil(vAuxParamStream);
    FreeAndNil(vAuxConnClone);
    Finalize(vAuxException);
    Finalize(vBytesAux);
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
