(* LiveBindings over JSON (Delphi XE3 and later): TRALJSONBindSourceAdapter is a
  TBindSourceAdapter whose records are the objects of a JSON array - loaded from
  text or from what a TRALClient answers - so a TAdapterBindSource shows them in
  VCL or FMX controls through the LiveBindings designer. The fields come from
  FieldDefs or from the data; edits, inserts and deletes change the JSON, which
  ToJSON gives back. Lazarus has no LiveBindings: there, the DBWare dataset. *)
unit RALLiveBindings;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Generics.Collections,
  Data.Bind.Components, Data.Bind.ObjectScope,
  RALTypes, RALJson, RALClient, RALResponse;

type
  /// Kinds of a field of the adapter.
  TRALJSONBindFieldKind = (jfString, jfInteger, jfFloat, jfBoolean);

  { A TBindSourceAdapter over a JSON array of objects. Bind it to a
    TAdapterBindSource (its Adapter property) and the controls to that }
  TRALJSONBindSourceAdapter = class(TBindSourceAdapter)
  private
    FClient: TRALClient;
    FFieldDefs: TStrings;
    FRootPath: string;
    FRoute: StringRAL;
    /// The records (owned).
    FRows: TRALJSONArray;

    procedure CreateFields;
    procedure FieldDefsChanged(Sender: TObject);
    procedure SetClient(const AValue: TRALClient);
    procedure SetFieldDefs(const AValue: TStrings);
  protected
    function AppendAt: Integer; override;
    function DeleteAt(AIndex: Integer): Boolean; override;
    function GetCanActivate: Boolean; override;
    function GetCanDelete: Boolean; override;
    function GetCanInsert: Boolean; override;
    function GetCanModify: Boolean; override;
    function GetCount: Integer; override;
    function InsertAt(AIndex: Integer): Integer; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The record the controls show; nil when there is none.
    function CurrentRow: TRALJSONObject;
    { GETs Route with Client and loads the JSON it answers; raises when the
      call fails or the answer is not 2xx }
    procedure Execute;
    { Loads AText: an array of objects, an object with the array at RootPath, or
      one object (one record). The fields are made again (from FieldDefs, or
      from the data when FieldDefs is empty) }
    procedure LoadFromJSON(const AText: string);
    /// The records, with the edits, as JSON text.
    function ToJSON: string;

    /// The records (owned by the adapter).
    property Rows: TRALJSONArray read FRows;
  published
    property Active;
    property AfterOpen;
    property AfterPost;
    property AfterScroll;
    property AutoEdit;
    property AutoPost;
    property BeforeOpen;
    property BeforePost;
    /// The client of Execute.
    property Client: TRALClient read FClient write SetClient;
    { The fields, one a line: "name" (text) or "name:integer", "name:float",
      "name:boolean", "name:string". Empty: every member of the data, its type
      taken from the values. Set them to have fields at design time }
    property FieldDefs: TStrings read FFieldDefs write SetFieldDefs;
    /// Dotted path to the array inside an answer that is an object ("data.items").
    property RootPath: string read FRootPath write FRootPath;
    /// The route Execute asks for.
    property Route: StringRAL read FRoute write FRoute;
  end;

implementation

type
  /// Reads the member of the field in the current record.
  TRALJSONReader<T> = class(TValueReader<T>)
  protected
    function Node: TRALJSON;
  end;

  TRALJSONStringReader = class(TRALJSONReader<string>)
  public
    function GetValue: string; override;
  end;

  TRALJSONIntegerReader = class(TRALJSONReader<Integer>)
  public
    function GetValue: Integer; override;
  end;

  TRALJSONFloatReader = class(TRALJSONReader<Double>)
  public
    function GetValue: Double; override;
  end;

  TRALJSONBooleanReader = class(TRALJSONReader<Boolean>)
  public
    function GetValue: Boolean; override;
  end;

  /// Writes the member of the field in the current record.
  TRALJSONWriter<T> = class(TValueWriter<T>)
  protected
    procedure Store(AValue: TRALJSON);
  end;

  TRALJSONStringWriter = class(TRALJSONWriter<string>)
  public
    procedure SetValue(const AValue: string); override;
  end;

  TRALJSONIntegerWriter = class(TRALJSONWriter<Integer>)
  public
    procedure SetValue(const AValue: Integer); override;
  end;

  TRALJSONFloatWriter = class(TRALJSONWriter<Double>)
  public
    procedure SetValue(const AValue: Double); override;
  end;

  TRALJSONBooleanWriter = class(TRALJSONWriter<Boolean>)
  public
    procedure SetValue(const AValue: Boolean); override;
  end;

{ TRALJSONReader<T> }

function TRALJSONReader<T>.Node: TRALJSON;
var
  vRow: TRALJSONObject;
begin
  Result := nil;
  vRow := TRALJSONBindSourceAdapter(FField.Owner).CurrentRow;
  if vRow <> nil then
    Result := vRow.Find(StringRAL(FField.MemberName));
  if (Result <> nil) and Result.IsNull then
    Result := nil;
end;

function TRALJSONStringReader.GetValue: string;
var
  vNode: TRALJSON;
begin
  vNode := Node;
  if vNode = nil then
    Result := ''
  else if vNode.Kind in [rjkObject, rjkArray] then
    Result := string(vNode.ToJSON)
  else
    Result := string(vNode.AsString);
end;

function TRALJSONIntegerReader.GetValue: Integer;
var
  vNode: TRALJSON;
begin
  vNode := Node;
  if vNode = nil then
    Result := 0
  else if vNode.Kind = rjkString then
    Result := StrToIntDef(string(vNode.AsString), 0)
  else
    Result := Integer(vNode.AsInteger);
end;

function TRALJSONFloatReader.GetValue: Double;
var
  vNode: TRALJSON;
begin
  vNode := Node;
  if vNode = nil then
    Result := 0
  else
    Result := vNode.AsNumber;
end;

function TRALJSONBooleanReader.GetValue: Boolean;
var
  vNode: TRALJSON;
begin
  vNode := Node;
  Result := (vNode <> nil) and (vNode.Kind = rjkBoolean) and vNode.AsBoolean;
end;

{ TRALJSONWriter<T> }

procedure TRALJSONWriter<T>.Store(AValue: TRALJSON);
var
  vRow: TRALJSONObject;
  vName: StringRAL;
begin
  vRow := TRALJSONBindSourceAdapter(FField.Owner).CurrentRow;
  if vRow = nil then
  begin
    AValue.Free;
    Exit;
  end;
  vName := StringRAL(FField.MemberName);
  // TRALJSONObject refuses a second member of a name: the old one goes first
  vRow.Extract(vName).Free;
  vRow.Add(vName, AValue);
end;

procedure TRALJSONStringWriter.SetValue(const AValue: string);
begin
  Store(TRALJSONString.Create(StringRAL(AValue)));
end;

procedure TRALJSONIntegerWriter.SetValue(const AValue: Integer);
begin
  Store(TRALJSONNumber.Create(StringRAL(IntToStr(AValue))));
end;

procedure TRALJSONFloatWriter.SetValue(const AValue: Double);
var
  vFormat: TFormatSettings;
begin
  vFormat := TFormatSettings.Create('en-US');
  vFormat.DecimalSeparator := '.';
  Store(TRALJSONNumber.Create(StringRAL(FloatToStr(AValue, vFormat))));
end;

procedure TRALJSONBooleanWriter.SetValue(const AValue: Boolean);
begin
  Store(TRALJSONBoolean.Create(AValue));
end;

{ TRALJSONBindSourceAdapter }

constructor TRALJSONBindSourceAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRows := TRALJSONArray.Create;
  FFieldDefs := TStringList.Create;
  TStringList(FFieldDefs).OnChange := FieldDefsChanged;
end;

destructor TRALJSONBindSourceAdapter.Destroy;
begin
  Active := False;
  ClearFields;
  FreeAndNil(FRows);
  FreeAndNil(FFieldDefs);
  inherited Destroy;
end;

procedure TRALJSONBindSourceAdapter.Loaded;
begin
  inherited Loaded;
  CreateFields;
end;

procedure TRALJSONBindSourceAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TRALJSONBindSourceAdapter.SetClient(const AValue: TRALClient);
begin
  if AValue = FClient then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALJSONBindSourceAdapter.SetFieldDefs(const AValue: TStrings);
begin
  FFieldDefs.Assign(AValue);
end;

procedure TRALJSONBindSourceAdapter.FieldDefsChanged(Sender: TObject);
var
  vActive: Boolean;
begin
  if csLoading in ComponentState then
    Exit;
  vActive := Active;
  Active := False;
  CreateFields;
  if vActive then
    Active := True;
end;

function TRALJSONBindSourceAdapter.CurrentRow: TRALJSONObject;
var
  vIndex: Integer;
begin
  Result := nil;
  vIndex := ItemIndex + ItemIndexOffset;
  if (FRows <> nil) and (vIndex >= 0) and (vIndex < FRows.Count) and
     (FRows.Get(vIndex) is TRALJSONObject) then
    Result := TRALJSONObject(FRows.Get(vIndex));
end;

procedure TRALJSONBindSourceAdapter.CreateFields;
var
  vNames: TStringList;
  vKinds: TList<TRALJSONBindFieldKind>;
  vInt, vRow, vIdx: Integer;
  vLine, vName, vType: string;
  vKind: TRALJSONBindFieldKind;
  vObj: TRALJSONObject;
  vNode: TRALJSON;
  vMember: IGetMemberObject;
begin
  ClearFields;
  vNames := TStringList.Create;
  vKinds := TList<TRALJSONBindFieldKind>.Create;
  try
    if FFieldDefs.Count > 0 then
      for vInt := 0 to FFieldDefs.Count - 1 do
      begin
        vLine := Trim(FFieldDefs[vInt]);
        if vLine = '' then
          Continue;
        vName := vLine;
        vType := 'string';
        if Pos(':', vLine) > 0 then
        begin
          vName := Trim(Copy(vLine, 1, Pos(':', vLine) - 1));
          vType := LowerCase(Trim(Copy(vLine, Pos(':', vLine) + 1, MaxInt)));
        end;
        if vType = 'integer' then
          vKind := jfInteger
        else if (vType = 'float') or (vType = 'double') or (vType = 'number') then
          vKind := jfFloat
        else if vType = 'boolean' then
          vKind := jfBoolean
        else
          vKind := jfString;
        vNames.Add(vName);
        vKinds.Add(vKind);
      end
    else
      // the members of the data, in the order they first appear
      for vRow := 0 to FRows.Count - 1 do
      begin
        if not (FRows.Get(vRow) is TRALJSONObject) then
          Continue;
        vObj := TRALJSONObject(FRows.Get(vRow));
        for vInt := 0 to vObj.Count - 1 do
        begin
          vName := string(vObj.Names[vInt]);
          vNode := vObj.Items[vInt];
          vIdx := vNames.IndexOf(vName);
          case vNode.Kind of
            rjkNumber:
              if Frac(vNode.AsNumber) = 0 then
                vKind := jfInteger
              else
                vKind := jfFloat;
            rjkBoolean: vKind := jfBoolean;
          else
            vKind := jfString;
          end;
          if vIdx < 0 then
          begin
            vNames.Add(vName);
            if vNode.IsNull then
              vKinds.Add(jfString)
            else
              vKinds.Add(vKind);
          end
          else if not vNode.IsNull then
          begin
            // a member seen as integer and as fraction is a float; any text wins
            if (vKinds[vIdx] = jfInteger) and (vKind = jfFloat) then
              vKinds[vIdx] := jfFloat
            else if (vKinds[vIdx] <> vKind) and not ((vKinds[vIdx] = jfFloat) and (vKind = jfInteger)) then
              vKinds[vIdx] := jfString;
          end;
        end;
      end;
    vMember := TBindSourceAdapterGetMemberObject.Create(Self);
    for vInt := 0 to vNames.Count - 1 do
      case vKinds[vInt] of
        jfInteger:
          Fields.Add(TBindSourceAdapterReadWriteField<Integer>.Create(Self, vNames[vInt],
            TBindSourceAdapterFieldType.Create('Integer', tkInteger), vMember,
            TRALJSONIntegerReader.Create, TRALJSONIntegerWriter.Create, mtInteger));
        jfFloat:
          Fields.Add(TBindSourceAdapterReadWriteField<Double>.Create(Self, vNames[vInt],
            TBindSourceAdapterFieldType.Create('Double', tkFloat), vMember,
            TRALJSONFloatReader.Create, TRALJSONFloatWriter.Create, mtFloat));
        jfBoolean:
          Fields.Add(TBindSourceAdapterReadWriteField<Boolean>.Create(Self, vNames[vInt],
            TBindSourceAdapterFieldType.Create('Boolean', tkEnumeration), vMember,
            TRALJSONBooleanReader.Create, TRALJSONBooleanWriter.Create, mtBoolean));
      else
        Fields.Add(TBindSourceAdapterReadWriteField<string>.Create(Self, vNames[vInt],
          TBindSourceAdapterFieldType.Create('string', tkUString), vMember,
          TRALJSONStringReader.Create, TRALJSONStringWriter.Create, mtText));
      end;
  finally
    vKinds.Free;
    vNames.Free;
  end;
end;

procedure TRALJSONBindSourceAdapter.LoadFromJSON(const AText: string);
var
  vRoot, vNode: TRALJSON;
  vParts: TStringList;
  vInt: Integer;
  vRows: TRALJSONArray;
begin
  vRoot := TRALJSON.ParseJSON(StringRAL(AText));
  try
    vNode := vRoot;
    if (FRootPath <> '') and (vRoot is TRALJSONObject) then
    begin
      vParts := TStringList.Create;
      try
        vParts.Delimiter := '.';
        vParts.StrictDelimiter := True;
        vParts.DelimitedText := FRootPath;
        for vInt := 0 to vParts.Count - 1 do
          if vNode is TRALJSONObject then
            vNode := TRALJSONObject(vNode).Find(StringRAL(vParts[vInt]))
          else
            vNode := nil;
      finally
        vParts.Free;
      end;
      if vNode = nil then
        raise Exception.Create('RootPath "' + FRootPath + '" is not in the answer.');
    end;
    // a copy of what was chosen, owned by the adapter
    if vNode is TRALJSONArray then
      vRows := TRALJSONArray(TRALJSON.ParseJSON(vNode.ToJSON))
    else
    begin
      vRows := TRALJSONArray.Create;
      if vNode is TRALJSONObject then
        vRows.Add(TRALJSON.ParseJSON(vNode.ToJSON));
    end;
  finally
    vRoot.Free;
  end;
  Active := False;
  ClearFields;
  FRows.Free;
  FRows := vRows;
  CreateFields;
  Active := True;
end;

procedure TRALJSONBindSourceAdapter.Execute;
var
  vResp: TRALResponse;
begin
  if FClient = nil then
    raise Exception.Create('TRALJSONBindSourceAdapter.Client is not set.');
  FClient.Get(FRoute, vResp);
  try
    if (vResp.ErrorCode <> 0) or (vResp.StatusCode < 200) or (vResp.StatusCode > 299) then
      raise Exception.CreateFmt('%s answered %d (error %d)', [string(FRoute), vResp.StatusCode,
        vResp.ErrorCode]);
    LoadFromJSON(string(vResp.ResponseText));
  finally
    vResp.Free;
  end;
end;

function TRALJSONBindSourceAdapter.ToJSON: string;
begin
  Result := string(FRows.ToJSON);
end;

function TRALJSONBindSourceAdapter.GetCanActivate: Boolean;
begin
  Result := FRows <> nil;
end;

function TRALJSONBindSourceAdapter.GetCount: Integer;
begin
  if FRows = nil then
    Result := 0
  else
    Result := FRows.Count;
end;

function TRALJSONBindSourceAdapter.GetCanModify: Boolean;
begin
  Result := True;
end;

function TRALJSONBindSourceAdapter.GetCanInsert: Boolean;
begin
  Result := True;
end;

function TRALJSONBindSourceAdapter.GetCanDelete: Boolean;
begin
  Result := True;
end;

function TRALJSONBindSourceAdapter.AppendAt: Integer;
begin
  FRows.Add(TRALJSONObject.Create);
  Result := FRows.Count - 1;
end;

function TRALJSONBindSourceAdapter.InsertAt(AIndex: Integer): Integer;
var
  vNew: TRALJSONArray;
  vInt: Integer;
begin
  // TRALJSONArray appends only: the array is made again with the new record in place
  vNew := TRALJSONArray.Create;
  for vInt := 0 to FRows.Count - 1 do
  begin
    if vInt = AIndex then
      vNew.Add(TRALJSONObject.Create);
    vNew.Add(TRALJSON.ParseJSON(FRows.Get(vInt).ToJSON));
  end;
  if (AIndex < 0) or (AIndex >= FRows.Count) then
  begin
    vNew.Add(TRALJSONObject.Create);
    AIndex := vNew.Count - 1;
  end;
  FRows.Free;
  FRows := vNew;
  Result := AIndex;
end;

function TRALJSONBindSourceAdapter.DeleteAt(AIndex: Integer): Boolean;
var
  vNew: TRALJSONArray;
  vInt: Integer;
begin
  Result := (AIndex >= 0) and (AIndex < FRows.Count);
  if not Result then
    Exit;
  vNew := TRALJSONArray.Create;
  for vInt := 0 to FRows.Count - 1 do
    if vInt <> AIndex then
      vNew.Add(TRALJSON.ParseJSON(FRows.Get(vInt).ToJSON));
  FRows.Free;
  FRows := vNew;
end;

end.
