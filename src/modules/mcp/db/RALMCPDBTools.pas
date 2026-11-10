{ The DBWare bridge of the MCP server: each tool runs one fixed SQL statement
  with declared parameters over the database of a TRALDBModule. The model only
  fills the parameters; it never writes SQL. }
unit RALMCPDBTools;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALTools, RALBase64, RALCustomObjects, RALJson, RALDBBase, RALDBModule,
  RALMCPServer;

type
  /// A tool that runs one fixed statement.
  TRALMCPDBTool = class(TCollectionItem)
  private
    FDescription: StringRAL;
    FMaxRows: IntegerRAL;
    FName: StringRAL;
    FParams: TRALMCPParams;
    FReadOnly: Boolean;
    FRoles: StringRAL;
    FSQL: TStrings;

    procedure SetParams(AValue: TRALMCPParams);
    procedure SetSQL(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
  published
    /// What the tool does, for the model.
    property Description: StringRAL read FDescription write FDescription;
    /// The most rows a query answers; 0 = no limit.
    property MaxRows: IntegerRAL read FMaxRows write FMaxRows default 100;
    /// The tool name.
    property Name: StringRAL read FName write FName;
    { The parameters, each a :name of SQL; they make the input schema and are
      the only values the model gives. }
    property Params: TRALMCPParams read FParams write SetParams;
    { A query (answers its rows as structured content, and the tool is marked
      read-only); False = a statement that answers the rows it changed. }
    property ReadOnly: Boolean read FReadOnly write FReadOnly default True;
    /// Roles that may call the tool, separated by commas; empty = everyone.
    property Roles: StringRAL read FRoles write FRoles;
    /// The statement, fixed at design time.
    property SQL: TStrings read FSQL write SetSQL;
  end;

  /// The tools of a bridge.
  TRALMCPDBToolList = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMCPDBTool;
  public
    /// Adds the tool AName running ASQL and returns it.
    function AddTool(const AName, ADescription, ASQL: StringRAL): TRALMCPDBTool;
    /// The tool AName; nil when none.
    function Find(const AName: StringRAL): TRALMCPDBTool;

    /// The tools by position.
    property Items[AIndex: IntegerRAL]: TRALMCPDBTool read GetItem; default;
  end;

  { Publishes Tools in MCPServer, each running its fixed SQL on a connection
    of DBModule (its pool). Rows come back as structured content (an object
    whose rows member is the array of rows), mirrored in a text block. }
  TRALMCPDBTools = class(TRALComponent)
  private
    FDBModule: TRALDBModule;
    FMCPServer: TRALMCPServer;
    FTools: TRALMCPDBToolList;

    /// Runs the tool of the call.
    procedure CallTool(Sender: TObject; ACall: TRALMCPCall; AResult: TRALMCPToolResult);
    procedure SetDBModule(AValue: TRALDBModule);
    procedure SetMCPServer(AValue: TRALMCPServer);
    procedure SetTools(AValue: TRALMCPDBToolList);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Declares Tools in MCPServer again (done when MCPServer is set or loaded).
    procedure Publish;
  published
    /// The DBWare module whose connections run the statements.
    property DBModule: TRALDBModule read FDBModule write SetDBModule;
    /// The MCP server the tools are declared in.
    property MCPServer: TRALMCPServer read FMCPServer write SetMCPServer;
    /// The tools.
    property Tools: TRALMCPDBToolList read FTools write SetTools;
  end;

implementation

const
  /// The output schema of a query tool.
  cRowsSchema = '{"type":"object","properties":{"rows":{"type":"array","items":{"type":"object"}}},' +
    '"required":["rows"]}';
  /// The output schema of a statement tool.
  cAffectedSchema = '{"type":"object","properties":{"rowsAffected":{"type":"integer"}},' +
    '"required":["rowsAffected"]}';

/// AText of the RTL's string type as StringRAL.
function ToRAL(const AText: string): StringRAL;
begin
  {$IFDEF FPC}
  Result := AText;
  {$ELSE}
  Result := UTF8Encode(AText);
  {$ENDIF}
end;

/// StringRAL AText as the RTL's string type.
function FromRAL(const AText: StringRAL): string;
begin
  {$IFDEF FPC}
  Result := AText;
  {$ELSE}
  Result := UTF8ToString(AText);
  {$ENDIF}
end;

/// The value of AField as JSON; the caller owns it.
function FieldJSON(AField: TField): TRALJSON;
begin
  if AField.IsNull then
    Result := TRALJSONNull.Create
  else
    case AField.DataType of
      ftBoolean:
        Result := TRALJSONBoolean.Create(AField.AsBoolean);
      ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
        Result := TRALJSONNumber.Create(StringRAL(IntToStr(AField.AsLargeInt)));
      ftFloat, ftCurrency, ftBCD, ftFMTBcd:
        Result := TRALJSONNumber.Create(StringRAL(FloatToStr(AField.AsFloat, RALInvariantFormat)));
      ftDate, ftTime, ftDateTime, ftTimeStamp:
        Result := TRALJSONString.Create(RALDateTimeToISO8601(AField.AsDateTime, True));
      ftBlob, ftGraphic, ftVarBytes, ftBytes:
        Result := TRALJSONString.Create(TRALBase64.Encode(AField.AsBytes));
    else
      Result := TRALJSONString.Create(ToRAL(AField.AsString));
    end;
end;

{ TRALMCPDBTool }

constructor TRALMCPDBTool.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TRALMCPParams.Create(Self, TRALMCPParam);
  FSQL := TStringList.Create;
  FReadOnly := True;
  FMaxRows := 100;
end;

destructor TRALMCPDBTool.Destroy;
begin
  FParams.Free;
  FSQL.Free;
  inherited Destroy;
end;

procedure TRALMCPDBTool.Assign(ASource: TPersistent);
begin
  if ASource is TRALMCPDBTool then
  begin
    FName := TRALMCPDBTool(ASource).Name;
    FDescription := TRALMCPDBTool(ASource).Description;
    FParams.Assign(TRALMCPDBTool(ASource).Params);
    FSQL.Assign(TRALMCPDBTool(ASource).SQL);
    FReadOnly := TRALMCPDBTool(ASource).ReadOnly;
    FMaxRows := TRALMCPDBTool(ASource).MaxRows;
    FRoles := TRALMCPDBTool(ASource).Roles;
  end
  else
    inherited Assign(ASource);
end;

function TRALMCPDBTool.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FromRAL(FName);
end;

procedure TRALMCPDBTool.SetParams(AValue: TRALMCPParams);
begin
  FParams.Assign(AValue);
end;

procedure TRALMCPDBTool.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

{ TRALMCPDBToolList }

function TRALMCPDBToolList.AddTool(const AName, ADescription, ASQL: StringRAL): TRALMCPDBTool;
begin
  Result := TRALMCPDBTool(Add);
  Result.Name := AName;
  Result.Description := ADescription;
  Result.SQL.Text := FromRAL(ASQL);
end;

function TRALMCPDBToolList.Find(const AName: StringRAL): TRALMCPDBTool;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].Name = AName then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALMCPDBToolList.GetItem(AIndex: IntegerRAL): TRALMCPDBTool;
begin
  Result := TRALMCPDBTool(inherited Items[AIndex]);
end;

{ TRALMCPDBTools }

constructor TRALMCPDBTools.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTools := TRALMCPDBToolList.Create(Self, TRALMCPDBTool);
end;

destructor TRALMCPDBTools.Destroy;
begin
  FTools.Free;
  inherited Destroy;
end;

procedure TRALMCPDBTools.CallTool(Sender: TObject; ACall: TRALMCPCall;
  AResult: TRALMCPToolResult);
var
  vTool: TRALMCPDBTool;
  vDB: TRALDBBase;
  vParams: TParams;
  vParam: TParam;
  vQuery: TDataSet;
  vRows: TRALJSONArray;
  vRow, vAnswer: TRALJSONObject;
  vValue: TRALJSON;
  vInt, vCount: IntegerRAL;
  vAffected, vLastId: Int64RAL;
begin
  vTool := nil;
  if ACall.Params.Find('name') <> nil then
    vTool := FTools.Find(ACall.Params.Find('name').AsString);
  if (vTool = nil) or (FDBModule = nil) then
  begin
    AResult.Fail('The tool is not linked to a database');
    Exit;
  end;
  vParams := TParams.Create;
  try
    // Only the declared parameters reach the statement, as bound values.
    for vInt := 0 to vTool.Params.Count - 1 do
    begin
      vValue := ACall.Argument(vTool.Params[vInt].Name);
      vParam := vParams.CreateParam(ftString, FromRAL(vTool.Params[vInt].Name), ptInput);
      if (vValue = nil) or vValue.IsNull then
        vParam.Clear
      else
        case vValue.Kind of
          rjkNumber:
            if vTool.Params[vInt].ParamType = mptInteger then
            begin
              vParam.DataType := ftLargeint;
              vParam.Value := vValue.AsInt64;
            end
            else
            begin
              vParam.DataType := ftFloat;
              vParam.AsFloat := vValue.AsNumber;
            end;
          rjkBoolean:
          begin
            vParam.DataType := ftBoolean;
            vParam.AsBoolean := vValue.AsBoolean;
          end;
        else
          vParam.AsString := FromRAL(vValue.AsString);
        end;
    end;
    vDB := FDBModule.AcquireDatabase(nil, nil);
    try
      vAnswer := TRALJSONObject.Create;
      try
        if vTool.ReadOnly then
        begin
          vQuery := vDB.OpenCompatible(ToRAL(vTool.SQL.Text), vParams);
          try
            vRows := TRALJSONArray.Create;
            vAnswer.Add('rows', vRows);
            vCount := 0;
            while not vQuery.EOF and ((vTool.MaxRows <= 0) or (vCount < vTool.MaxRows)) do
            begin
              vRow := TRALJSONObject.Create;
              for vInt := 0 to vQuery.FieldCount - 1 do
                vRow.Add(ToRAL(vQuery.Fields[vInt].FieldName), FieldJSON(vQuery.Fields[vInt]));
              vRows.Add(vRow);
              Inc(vCount);
              vQuery.Next;
            end;
          finally
            vQuery.Free;
          end;
        end
        else
        begin
          vAffected := 0;
          vLastId := 0;
          vDB.ExecSQL(ToRAL(vTool.SQL.Text), vParams, vAffected, vLastId);
          vAnswer.Add('rowsAffected', Int64(vAffected));
        end;
        AResult.SetStructured(vAnswer);
        vAnswer := nil;
      finally
        vAnswer.Free;
      end;
    finally
      FDBModule.ReleaseDatabase(vDB);
    end;
  finally
    vParams.Free;
  end;
end;

procedure TRALMCPDBTools.Loaded;
begin
  inherited Loaded;
  Publish;
end;

procedure TRALMCPDBTools.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FDBModule then
      FDBModule := nil;
    if AComponent = FMCPServer then
      FMCPServer := nil;
  end;
end;

procedure TRALMCPDBTools.Publish;
var
  vInt: IntegerRAL;
  vSource: TRALMCPDBTool;
  vTool: TRALMCPTool;
begin
  if (FMCPServer = nil) or (csLoading in ComponentState) then
    Exit;
  for vInt := 0 to FTools.Count - 1 do
  begin
    vSource := FTools[vInt];
    vTool := FMCPServer.Tools.Find(vSource.Name);
    if vTool = nil then
      vTool := TRALMCPTool(FMCPServer.Tools.Add);
    vTool.Name := vSource.Name;
    vTool.Description := vSource.Description;
    vTool.Params.Assign(vSource.Params);
    vTool.Roles := vSource.Roles;
    if vSource.ReadOnly then
    begin
      vTool.OutputSchema := cRowsSchema;
      vTool.Hints := [thReadOnly, thIdempotent];
    end
    else
    begin
      vTool.OutputSchema := cAffectedSchema;
      vTool.Hints := [thDestructive];
    end;
    vTool.OnCall := {$IFDEF FPC}@{$ENDIF}CallTool;
  end;
  FMCPServer.NotifyToolsChanged;
end;

procedure TRALMCPDBTools.SetDBModule(AValue: TRALDBModule);
begin
  if FDBModule = AValue then
    Exit;
  if FDBModule <> nil then
    FDBModule.RemoveFreeNotification(Self);
  FDBModule := AValue;
  if FDBModule <> nil then
    FDBModule.FreeNotification(Self);
end;

procedure TRALMCPDBTools.SetMCPServer(AValue: TRALMCPServer);
begin
  if FMCPServer = AValue then
    Exit;
  if FMCPServer <> nil then
    FMCPServer.RemoveFreeNotification(Self);
  FMCPServer := AValue;
  if FMCPServer <> nil then
  begin
    FMCPServer.FreeNotification(Self);
    Publish;
  end;
end;

procedure TRALMCPDBTools.SetTools(AValue: TRALMCPDBToolList);
begin
  FTools.Assign(AValue);
end;

end.
