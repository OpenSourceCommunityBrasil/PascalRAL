/// Class to create(register) components on pallete
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
  RALStorageCSV,
  // client
  RALClient;

type
  TRALBaseURLEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  TRALCompressEditor = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Drops from the Object Inspector the published properties that say nothing
    about the component as it is configured right now - the mORMot2 server's
    HttpSysDomain outside smHttpSys, ShareConnection on an engine with one
    socket per object. What is irrelevant is decided by the component itself,
    in TRALComponent.IsPropertyRelevant, so this unit never has to know one
    engine from another - and an engine never has to link against the IDE.

    It is COMFORT, not enforcement: what is hidden keeps whatever value it had
    and is ignored, never refused, because it may simply be left over from
    another configuration. A choice that cannot work is a different matter and
    is never hidden - it raises, and says why.

    Delphi filters through a second interface on the selection editor,
    ISelectionPropertyFilter; Lazarus through a virtual method plus an
    attribute. Same idea, two shapes. }
  TRALSelectionEditor = class(TSelectionEditor{$IFNDEF FPC}, ISelectionPropertyFilter{$ENDIF})
  public
    {$IFDEF FPC}
      function GetAttributes: TSelectionEditorAttributes; override;
      procedure FilterProperties(ASelection: TPersistentSelectionList;
                                 AProperties: TPropertyEditorList); override;
    {$ELSE}
      procedure FilterProperties(const ASelection: IDesignerSelections;
                                 const ASelectionProperties: IInterfaceList);
    {$ENDIF}
  end;

  TRALServerSelectionEditor = class(TRALSelectionEditor)
  public
    {$IFNDEF FPC}
      procedure RequiresUnits(Proc: TGetStrProc); override;
    {$ENDIF}
  end;

  TRALClientSelectionEditor = class(TRALSelectionEditor)
  public
    {$IFNDEF FPC}
      procedure RequiresUnits(Proc: TGetStrProc); override;
    {$ENDIF}
  end;

  { A sieve ONE level down: it receives every sub-property the object editor
    was about to hand the Object Inspector, and passes on only those at least
    one of the selected components calls relevant - the same rule as the level
    above, which is why it is written the same way. }
  TRALNestedSieve = class
  private
    FOuter: {$IFDEF FPC}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF};
    FOwners: TList;
    FPrefix: StringRAL;
    function IsRelevant(const AName: StringRAL): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PassOn({$IFDEF FPC}AProp: TPropertyEditor{$ELSE}const AProp: IProperty{$ENDIF});
  end;

  { Drops, INSIDE an object property, the sub-properties the component does not
    use as it is configured right now - SSL.CertificateFile on a mORMot2 server
    in smHttpSys, where the certificate comes from the machine store through
    netsh and the file is never read.

    The name the component is asked is the DOTTED one, 'SSL.CertificateFile',
    so that a single IsPropertyRelevant answers for both levels and this unit
    still never has to know one engine from another.

    Same rule as the level above: what is hidden keeps the value it had and is
    ignored, never refused. }
  TRALNestedProperty = class(TClassProperty)
  public
    procedure GetProperties(Proc: {$IFDEF FPC}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF}); override;
  end;

  { TRALClientEngines }


  TRALClientEngines = class(TStringProperty)
  private
    {$IFDEF FPC}
       procedure FPCRequiresUnits(AEngine : string);
    {$ENDIF}
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const AValue: string); override;
  end;


procedure Register;

implementation

// this allow to put a nice entry in the delphi
// ide splash screen and about box

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
  RegisterComponents('RAL - Server', [TRALServerBasicAuth, TRALServerJWTAuth]);
  RegisterComponents('RAL - Client', [TRALClient, TRALClientBasicAuth, TRALClientJWTAuth]);
  RegisterComponents('RAL - Modules', [TRALWebModule, TRALSwaggerModule]);
  RegisterComponents('RAL - Storage', [TRALStorageJSONLink, TRALStorageBINLink, TRALStorageCSVLink]);

  { Registered for the BASE classes on purpose: the IDE walks up the hierarchy,
    so every engine's server and every client gets the property filter without
    a line of design-time code of its own. }
  RegisterSelectionEditor(TRALServer, TRALServerSelectionEditor);
  RegisterSelectionEditor(TRALClient, TRALClientSelectionEditor);

  // property registration process
  RegisterPropertyEditor(TypeInfo(TStrings), TRALClient, 'BaseURL', TRALBaseURLEditor);
  RegisterPropertyEditor(TypeInfo(String), TRALClient, 'EngineType', TRALClientEngines);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALClient, 'CompressType', TRALCompressEditor);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALServer, 'CompressType', TRALCompressEditor);

  { By the BASE TYPE, tied to no component: the IDE matches a descendant class
    with the editor registered for its ancestor, so this covers the SSL of
    every server engine at once. With no rule on the component nothing changes
    - IsPropertyRelevant answers True by default. }
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
  { hidden only when it says nothing for ANY of the selected ones - with a
    mixed selection the honest thing is to go on showing it }
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

    { no RAL component in the selection: nothing to sieve }
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

      { hidden only when it says nothing for EVERY selected component: with a
        mixed selection the honest thing is to go on showing it }
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

      { hidden only when it says nothing for EVERY selected component: with a
        mixed selection the honest thing is to go on showing it }
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

  { TRALClienteSelectionEditor }

  procedure TRALClientSelectionEditor.RequiresUnits(Proc: TGetStrProc);
  var
    vClass : TRALClientHTTPClass;
    vComp : TRALClient;
    vInt : IntegerRAL;
  begin
    inherited;
    Proc('RALRequest');
    Proc('RALResponse');

    // might be a better way of doing the code from here onwards?
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
