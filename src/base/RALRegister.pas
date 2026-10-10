/// Registers the RAL components and their property editors in the IDE.
unit RALRegister;

{$I PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
    LResources, PropEdits, StringsPropEditDlg, CodeCache, SrcEditorIntf,
    LazIDEIntf, CodeToolManager, PackageIntf, ProjectIntf,
  {$ELSE}
    {$IFDEF DELPHI2005UP}
      ToolsAPI,
    {$ENDIF}
      DesignEditors, DesignIntf, StringsEdit,
  {$ENDIF}
  {$IFDEF RALWindows}
    Windows,
  {$ENDIF}
  Classes, SysUtils,
  // generic
  RALConsts, RALAuthentication, RALCompress, RALTypes, RALCustomObjects,
  // server
  RALServer, RALWebModule, RALSwaggerModule, RALStorageJSON, RALStorageBIN,
  RALStorageCSV, RALSecurity, RALCORS, RALContent, RALDigest, RALOAuth2, RALSelfSigned,
  RALConcurrency,
  // client
  RALClient;

type
  /// Property editor of TRALClient.BaseURL: one URL per line, in a dialog.
  TRALBaseURLEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  /// Property editor that lists the compressions linked into the program.
  TRALCompressEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Hides from the Object Inspector the properties a component calls irrelevant
    (TRALComponent.IsPropertyRelevant); a hidden value is kept and ignored. }
  TRALSelectionEditor = class(TSelectionEditor{$IFNDEF FPC}, ISelectionPropertyFilter{$ENDIF})
  public
    {$IFDEF FPC}
      /// Removes from AProperties what no selected component calls relevant.
      procedure FilterProperties(ASelection: TPersistentSelectionList;
                                 AProperties: TPropertyEditorList); override;
      function GetAttributes: TSelectionEditorAttributes; override;
    {$ELSE}
      /// Removes from ASelectionProperties what no selected component calls relevant.
      procedure FilterProperties(const ASelection: IDesignerSelections;
                                 const ASelectionProperties: IInterfaceList);
    {$ENDIF}
  end;

  /// Selection editor of the servers: adds the units a handler needs to the uses.
  TRALServerSelectionEditor = class(TRALSelectionEditor)
  public
    {$IFNDEF FPC}
      procedure RequiresUnits(Proc: TGetStrProc); override;
    {$ENDIF}
  end;

  /// Selection editor of the clients: also adds the unit of the chosen engine.
  TRALClientSelectionEditor = class(TRALSelectionEditor)
  public
    {$IFNDEF FPC}
      procedure RequiresUnits(Proc: TGetStrProc); override;
    {$ENDIF}
  end;

  /// Passes on only the sub-properties some selected component calls relevant.
  TRALNestedSieve = class
  private
    /// Callback that receives the sub-properties kept.
    FOuter: {$IFDEF FPC}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF};
    /// Selected RAL components.
    FOwners: TList;
    /// Name of the object property and a dot, put before each sub-property name.
    FPrefix: StringRAL;

    /// True when a selected component calls AName relevant, or none is selected.
    function IsRelevant(const AName: StringRAL): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// Hands AProp on when it is relevant.
    procedure PassOn({$IFDEF FPC}AProp: TPropertyEditor{$ELSE}const AProp: IProperty{$ENDIF});
  end;

  { Property editor of an object property, such as SSL, that hides the
    sub-properties the component does not use; it asks for the dotted name. }
  TRALNestedProperty = class(TClassProperty)
  public
    procedure GetProperties(Proc: {$IFDEF FPC}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF}); override;
  end;

  /// Property editor of TRALClient.EngineType: lists the registered engines.
  TRALClientEngines = class(TStringProperty)
  private
    {$IFDEF FPC}
       /// Adds the unit and the package of AEngine to the Lazarus project.
       procedure FPCRequiresUnits(AEngine : string);
    {$ENDIF}
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const AValue: string); override;
  end;


/// Registers the components and property editors in the IDE.
procedure Register;

implementation

procedure Register;
{$IFDEF DELPHI2005UP}
var
  AboutSvcs: IOTAAboutBoxServices;
{$ENDIF}
begin
  {$IFDEF DELPHI2005UP}
  // add project info to IDE's splash screen
  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap(RALPACKAGENAME,
      loadbitmap(HInstance, RALPACKAGESHORT), false, RALPACKAGELICENSEVERSION);

  // add project info to IDE's help panels
  if (BorlandIDEServices <> nil) and supports(BorlandIDEServices, IOTAAboutBoxServices,
    AboutSvcs) then
    AboutSvcs.AddPluginInfo(RALPACKAGESHORTLICENSE, RALPACKAGESHORT + sLineBreak +
      RALPACKAGENAME + sLineBreak + sLineBreak + RALPACKAGESITE,
      loadbitmap(HInstance, RALPACKAGESHORT), false, RALPACKAGELICENSE);
  {$ENDIF}

  // component registration process
  RegisterComponents('RAL - Server', [TRALServerBasicAuth, TRALServerJWTAuth,
    TRALServerDigest, TRALServerOAuth2]);
  RegisterComponents('RAL - Client', [TRALClient, TRALClientBasicAuth, TRALClientJWTAuth,
    TRALClientDigest, TRALClientOAuth2, TRALOAuth2Loopback]);
  RegisterComponents('RAL - Modules', [TRALWebModule, TRALSwaggerModule]);
  // plugins link to a server by their Server property
  RegisterComponents('RAL - Plugins', [TRALLimitsPlugin, TRALCompressPlugin,
    TRALCriptoPlugin, TRALWhiteListPlugin, TRALBlackListPlugin, TRALBruteForcePlugin,
    TRALFloodPlugin, TRALPathTraversalPlugin, TRALCORSPlugin, TRALJSONBodyPlugin,
    TRALSelfSignedPlugin, TRALSecurityHeadersPlugin, TRALConcurrencyPlugin]);
  RegisterComponents('RAL - Storage', [TRALStorageJSONLink, TRALStorageBINLink, TRALStorageCSVLink]);

  // registered for the base classes: the IDE also applies them to every descendant
  RegisterSelectionEditor(TRALServer, TRALServerSelectionEditor);
  RegisterSelectionEditor(TRALClient, TRALClientSelectionEditor);

  // property registration process
  RegisterPropertyEditor(TypeInfo(TStrings), TRALClient, 'BaseURL', TRALBaseURLEditor);
  RegisterPropertyEditor(TypeInfo(String), TRALClient, 'EngineType', TRALClientEngines);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALClient, 'CompressType', TRALCompressEditor);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALServer, 'CompressType', TRALCompressEditor);

  // by the base type, for any component: covers the SSL of every engine
  RegisterPropertyEditor(TypeInfo(TRALSSL), nil, '', TRALNestedProperty);
  RegisterPropertyEditor(TypeInfo(TRALClientSSL), nil, '', TRALNestedProperty);
end;

{ TRALBaseURLEditor }

{$IFDEF FPC}
  procedure TRALBaseURLEditor.Edit;
  var
    vStr : TStringsPropEditorFrm;
  begin
    inherited;
    vStr := TStringsPropEditorFrm.Create(nil);
    try
      vStr.Memo.Text := GetValue;
      if vStr.ShowModal = 1 then // mrOK
        SetValue(vStr.Memo.Text);
    finally
      FreeAndNil(vStr);
    end;
  end;
{$ELSE}
  procedure TRALBaseURLEditor.Edit;
  var
    vStr : TStringsEditDlg;
  begin
    inherited;
    vStr := TStringsEditDlg.Create(nil);
    try
      vStr.Memo.Text := GetValue;
      if vStr.ShowModal = 1 then // mrOK
        SetValue(vStr.Memo.Text);
    finally
      FreeAndNil(vStr);
    end;
  end;
{$ENDIF}

function TRALBaseURLEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TRALBaseURLEditor.GetValue: string;
begin
  Result := Trim(TRALClient(GetComponent(0)).BaseURL.Text);
end;

procedure TRALBaseURLEditor.SetValue(const Value: string);
begin
  TRALClient(GetComponent(0)).BaseURL.Text := Value;
end;

{ TRALCompressEditor }

function TRALCompressEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
  {$IFDEF FPC}
    Result := Result + [paPickList];
  {$ENDIF}
end;

procedure TRALCompressEditor.GetValues(Proc: TGetStrProc);
var
  vStr: TStringList;
  vInt: Integer;
begin
  vStr := TStringList.Create;
  try
    GetCompressList(vStr);
    for vInt := 0 to Pred(vStr.Count) do
      Proc(vStr.Strings[vInt]);
  finally
    FreeAndNil(vStr);
  end;
end;

{ TRALClientEngines }

{$IFDEF FPC}
  procedure TRALClientEngines.FPCRequiresUnits(AEngine : string);
  var
    vComp: TRALClient;
    vCode: TCodeBuffer;
    vSrcEdit: TSourceEditorInterface;
    vClass: TRALClientHTTPClass;
    vFile: TLazProjectFile;
    vPkg: TIDEPackage;
  begin
    if not LazarusIDE.BeginCodeTools then
      Exit;

    vComp := TRALClient(GetComponent(0));
    if (vComp = nil) or (vComp.Owner = nil) then
      Exit;

    vFile := LazarusIDE.GetProjectFileWithRootComponent(vComp.Owner);
    if vFile = nil then
      Exit;

    vSrcEdit := SourceEditorManagerIntf.SourceEditorIntfWithFilename(vFile.Filename);
    if vSrcEdit = nil then
      Exit;

    vCode := TCodeBuffer(vSrcEdit.CodeToolsBuffer);
    if vCode = nil then
      Exit;

    vClass := GetEngineClass(AEngine);
    if vClass <> nil then
    begin
      CodeToolBoss.AddUnitToMainUsesSection(vCode, vClass.UnitName, '');

      vPkg := PackageEditingInterface.IsPackageInstalled(vClass.PackageDependency);
      if vPkg <> nil then
      begin
        vFile := LazarusIDE.ActiveProject.CreateProjectFile(ChangeFileExt(vPkg.Filename, '.pas'));
        vFile.IsPartOfProject := False;
        LazarusIDE.ActiveProject.AddFile(vFile, True);
        LazarusIDE.ActiveProject.AddPackageDependency(vClass.PackageDependency);
      end;
    end;
  end;
{$ENDIF}

function TRALClientEngines.GetAttributes: TPropertyAttributes;
begin
  Result := [paSortList, paValueList];
  {$IFDEF FPC}
    Result := Result + [paPickList];
  {$ENDIF}
end;

procedure TRALClientEngines.GetValues(Proc: TGetStrProc);
var
  vInt : IntegerRAL;
  vList : TStringList;
begin
  vList := TStringList.Create;
  try
    GetEngineList(vList);
    for vInt := 0 to Pred(vList.Count) do
      Proc(vList.Strings[vInt]);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TRALClientEngines.SetValue(const AValue: string);
begin
  inherited SetValue(AValue);
  {$IFDEF FPC}
    FPCRequiresUnits(AValue);
  {$ENDIF}
end;

{ TRALNestedSieve }

constructor TRALNestedSieve.Create;
begin
  inherited Create;
  FOwners := TList.Create;
end;

destructor TRALNestedSieve.Destroy;
begin
  FOwners.Free;
  inherited;
end;

function TRALNestedSieve.IsRelevant(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  // hidden only when no selected component calls it relevant
  for vInt := 0 to FOwners.Count - 1 do
    if TRALComponent(FOwners[vInt]).IsPropertyRelevant(AName) then
      Exit(True);
  Result := FOwners.Count = 0;
end;

procedure TRALNestedSieve.PassOn({$IFDEF FPC}AProp: TPropertyEditor{$ELSE}const AProp: IProperty{$ENDIF});
begin
  if IsRelevant(FPrefix + StringRAL(AProp.GetName)) then
    FOuter(AProp);
end;

{ TRALNestedProperty }

procedure TRALNestedProperty.GetProperties(Proc: {$IFDEF FPC}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF});
var
  vSieve: TRALNestedSieve;
  vObj: TPersistent;
  vInt: IntegerRAL;
begin
  vSieve := TRALNestedSieve.Create;
  try
    for vInt := 0 to PropCount - 1 do
    begin
      vObj := GetComponent(vInt);
      if vObj is TRALComponent then
        vSieve.FOwners.Add(vObj);
    end;

    // no RAL component in the selection: nothing to hide
    if vSieve.FOwners.Count = 0 then
    begin
      inherited GetProperties(Proc);
      Exit;
    end;

    vSieve.FOuter := Proc;
    vSieve.FPrefix := StringRAL(GetName) + '.';
    inherited GetProperties({$IFDEF FPC}@{$ENDIF}vSieve.PassOn);
  finally
    vSieve.Free;
  end;
end;

{ TRALSelectionEditor }


{$IFDEF FPC}
  function TRALSelectionEditor.GetAttributes: TSelectionEditorAttributes;
  begin
    Result := [seaFilterProperties];
  end;

  procedure TRALSelectionEditor.FilterProperties(ASelection: TPersistentSelectionList;
    AProperties: TPropertyEditorList);
  var
    vProp, vSel: IntegerRAL;
    vName: StringRAL;
    vShow: boolean;
  begin
    if (ASelection = nil) or (AProperties = nil) then
      Exit;

    for vProp := AProperties.Count - 1 downto 0 do
    begin
      vName := StringRAL(AProperties[vProp].GetName);

      // hidden only when no selected component calls it relevant
      vShow := False;
      for vSel := 0 to ASelection.Count - 1 do
      begin
        if (ASelection[vSel] is TRALComponent) and
           (not TRALComponent(ASelection[vSel]).IsPropertyRelevant(vName)) then
          Continue;
        vShow := True;
        Break;
      end;

      if not vShow then
        AProperties.Delete(vProp); // the list owns the editor and frees it here
    end;
  end;
{$ELSE}
  procedure TRALSelectionEditor.FilterProperties(const ASelection: IDesignerSelections;
    const ASelectionProperties: IInterfaceList);
  var
    vProp, vSel: IntegerRAL;
    vName: StringRAL;
    vShow: boolean;
    vEditor: IProperty;
  begin
    if (ASelection = nil) or (ASelectionProperties = nil) then
      Exit;

    for vProp := ASelectionProperties.Count - 1 downto 0 do
    begin
      if not Supports(ASelectionProperties[vProp], IProperty, vEditor) then
        Continue;
      vName := StringRAL(vEditor.GetName);

      // hidden only when no selected component calls it relevant
      vShow := False;
      for vSel := 0 to ASelection.Count - 1 do
      begin
        if (ASelection[vSel] is TRALComponent) and
           (not TRALComponent(ASelection[vSel]).IsPropertyRelevant(vName)) then
          Continue;
        vShow := True;
        Break;
      end;

      if not vShow then
        ASelectionProperties.Delete(vProp);
    end;
  end;
{$ENDIF}

{$IFNDEF FPC}
  { TRALServerSelectionEditor }

  procedure TRALServerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
  begin
    inherited;
    Proc('RALRequest');
    Proc('RALResponse');
    Proc('RALTypes');
  end;

  { TRALClientSelectionEditor }

  procedure TRALClientSelectionEditor.RequiresUnits(Proc: TGetStrProc);
  var
    vClass : TRALClientHTTPClass;
    vComp : TRALClient;
    vInt : IntegerRAL;
  begin
    inherited;
    Proc('RALRequest');
    Proc('RALResponse');

    // the unit of each client's engine on the form
    if (Designer = nil) or (Designer.Root = nil) then
      Exit;

    for vInt := 0 to Pred(Designer.Root.ComponentCount) do
    begin
      if (Designer.Root.Components[vInt] is TRALClient) then
      begin
        vComp := TRALClient(Designer.Root.Components[vInt]);
        if vComp.EngineType <> '' then
        begin
          vClass := GetEngineClass(vComp.EngineType);
          if vClass <> nil then
            Proc(vClass.UnitName);
        end;
      end;
    end;
  end;
{$ENDIF}

{$IFDEF FPC}
initialization
{$I PascalRALDsgn.lrs}
{$ENDIF}

end.
