/// Unit that register RAL Database components in the IDE
unit RALDBRegister;

{$I ../base/PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, PropEdits, CodeCache, SrcEditorIntf, LazIDEIntf, CodeToolManager,
  PackageIntf, ProjectIntf,
  {$ELSE}
    {$IFDEF DELPHI2005UP}
      ToolsAPI,
    {$ENDIF}
      DesignEditors, DesignIntf,
  {$ENDIF}
  Classes, SysUtils,
  RALDBModule, RALDBConnection, RALDBBase, RALTypes;

type
  {$IFNDEF FPC}
    TRALDBModuleSelectionEditor = class(TSelectionEditor)
    public
      procedure RequiresUnits(Proc: TGetStrProc); override;
    end;
  {$ENDIF}

  { TRALDBDatabases }

  TRALDBDatabases = class(TStringProperty)
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

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALDBConnection]);
  RegisterComponents('RAL - Modules', [TRALDBModule]);

  RegisterPropertyEditor(TypeInfo(StringRAL), TRALDBModule, 'DatabaseLink', TRALDBDatabases);

  {$IFNDEF FPC}
    RegisterSelectionEditor(TRALDBModule,  TRALDBModuleSelectionEditor);
  {$ENDIF}
end;

{ TRALDBDatabases }

{$IFDEF FPC}
  procedure TRALDBDatabases.FPCRequiresUnits(AEngine: string);
  var
    vComp: TRALDBModule;
    vCode: TCodeBuffer;
    vSrcEdit: TSourceEditorInterface;
    vClass: TRALDBClass;
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

    vComp := TRALDBModule(GetComponent(0));
    if vComp <> nil then
    begin
      vClass := GetDatabaseClass(AEngine);
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

function TRALDBDatabases.GetAttributes: TPropertyAttributes;
begin
  Result := [paSortList, paValueList];
  {$IFDEF FPC}
    Result := Result + [paPickList];
  {$ENDIF}
end;

procedure TRALDBDatabases.GetValues(Proc: TGetStrProc);
var
  vInt : IntegerRAL;
  vList : TStringList;
begin
  vList := TStringList.Create;
  try
    GetDatabaseList(vList);
    for vInt := 0 to Pred(vList.Count) do
      Proc(vList.Strings[vInt]);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TRALDBDatabases.SetValue(const AValue: string);
begin
  inherited SetValue(AValue);
  {$IFDEF FPC}
    FPCRequiresUnits(AValue);
  {$ENDIF}
end;

{$IFNDEF FPC}
  { TRALDBModuleSelectionEditor }

  procedure TRALDBModuleSelectionEditor.RequiresUnits(Proc: TGetStrProc);
  var
    vClass : TRALDBClass;
    vComp : TRALDBModule;
    vInt : IntegerRAL;
  begin
    inherited;
    // might be a better way of doing the code from here onwards?
    if (Designer = nil) or (Designer.Root = nil) then
      Exit;

    for vInt := 0 to Pred(Designer.Root.ComponentCount) do
    begin
      if (Designer.Root.Components[vInt] is TRALDBModule) then
      begin
        vComp := TRALDBModule(Designer.Root.Components[vInt]);
        if vComp.DatabaseLink <> '' then
        begin
          vClass := GetDatabaseClass(vComp.DatabaseLink);
          if vClass <> nil then
            Proc(vClass.UnitName);
        end;
      end;
    end;
  end;
{$ENDIF}

{$IFDEF FPC}
initialization
{$I raldbpackage.lrs}
{$ENDIF}

end.
