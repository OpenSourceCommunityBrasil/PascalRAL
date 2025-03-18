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
  RALConsts, RALAuthentication, RALCompress, RALTypes,
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
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {$IFNDEF FPC}
    TRALServerSelectionEditor = class(TSelectionEditor)
    public
      procedure RequiresUnits(Proc: TGetStrProc); override;
    end;

    TRALClientSelectionEditor = class(TSelectionEditor)
    public
      procedure RequiresUnits(Proc: TGetStrProc); override;
    end;
  {$ENDIF}

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

  {$IFNDEF FPC}
    RegisterSelectionEditor(TRALServer, TRALServerSelectionEditor);
    RegisterSelectionEditor(TRALClient,  TRALClientSelectionEditor);
  {$ENDIF}

  // property registration process
  RegisterPropertyEditor(TypeInfo(TStrings), TRALClient, 'BaseURL', TRALBaseURLEditor);
  RegisterPropertyEditor(TypeInfo(StringRAL), TRALClient, 'EngineType', TRALClientEngines);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALClient, 'CompressType', TRALCompressEditor);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALServer, 'CompressType', TRALCompressEditor);
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

procedure TRALCompressEditor.GetValues(Proc: TGetStrProc);
var
  vStr: TStringList;
  vInt: Integer;
begin
  vStr := TRALCompress.GetInstalledList;
  try
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

    vSrcEdit := SourceEditorManagerIntf.ActiveEditor;
    if vSrcEdit = nil then
      Exit;

    vCode := TCodeBuffer(vSrcEdit.CodeToolsBuffer);
    if vCode = nil then
      Exit;

    vComp := TRALClient(GetComponent(0));
    if vComp <> nil then
    begin
      vClass := GetEngineClass(AEngine);
      if vClass <> nil then begin
        CodeToolBoss.AddUnitToMainUsesSection(vCode, vClass.UnitName, '');

        vPkg := PackageEditingInterface.FindPackageWithName(vClass.PackageDependency);
        if vPkg <> nil then
        begin
          vFile := LazarusIDE.ActiveProject.CreateProjectFile(ChangeFileExt(vPkg.Filename, '.pas'));
          vFile.IsPartOfProject := False;
          LazarusIDE.ActiveProject.AddFile(vFile, True);
          LazarusIDE.ActiveProject.AddPackageDependency(vClass.PackageDependency);
        end;
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
