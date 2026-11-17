/// Unit that register RAL Database components in the IDE
unit RALDBReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, PropEdits, ComponentEditors, Dialogs, CodeCache, SrcEditorIntf, LazIDEIntf,
  CodeToolManager, PackageIntf, ProjectIntf,
  {$ELSE}
    {$IFDEF DELPHI2005UP}
      ToolsAPI,
    {$ENDIF}
      DesignEditors, DesignIntf,
      {$IFDEF DELPHIXE2UP}Vcl.Dialogs,{$ELSE}Dialogs,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils,
  RALDBModule, RALDBCrudModule, RALDBConnection, RALDBBase, RALTypes, RALConsts;

type
  {$IFNDEF FPC}
    /// Adds the units of the chosen driver to the uses of the form.
    TRALDBModuleSelectionEditor = class(TSelectionEditor)
    public
      procedure RequiresUnits(Proc: TGetStrProc); override;
    end;
  {$ENDIF}

  { Verb of TRALDBCrudModule: reads the columns of every resource's table from the
    catalog of DBModule into its Fields (and KeyFields when empty). }
  TRALDBCrudModuleEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  /// Property editor that lists the registered drivers.
  TRALDBDatabases = class(TStringProperty)
  private
    {$IFDEF FPC}
       /// Adds the unit of AEngine to the uses and its package to the Lazarus project.
       procedure FPCRequiresUnits(AEngine : string);
    {$ENDIF}
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const AValue: string); override;
  end;

/// Registers the DBWare components and editors.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Client', [TRALDBConnection]);
  RegisterComponents('RAL - Modules', [TRALDBModule, TRALDBCrudModule]);

  RegisterPropertyEditor(TypeInfo(String), TRALDBModule, 'DatabaseLink', TRALDBDatabases);
  RegisterComponentEditor(TRALDBCrudModule, TRALDBCrudModuleEditor);

  {$IFNDEF FPC}
    RegisterSelectionEditor(TRALDBModule,  TRALDBModuleSelectionEditor);
  {$ENDIF}
end;

{ TRALDBCrudModuleEditor }

procedure TRALDBCrudModuleEditor.ExecuteVerb(AIndex: Integer);
var
  vModule: TRALDBCrudModule;
  vReport: TStringList;
  vInt: Integer;
  vResource: TRALDBCrudResource;
begin
  if AIndex <> 0 then
    Exit;
  vModule := TRALDBCrudModule(Component);
  vReport := TStringList.Create;
  try
    for vInt := 0 to vModule.Resources.Count - 1 do
    begin
      vResource := vModule.Resources[vInt];
      if (vResource.TableName = '') and (vResource.SelectSQL = '') then
        Continue;
      try
        vReport.Add(Format(cmDBCrudColumnsRead, [vResource.DisplayName,
          vModule.ReadColumns(vResource)]));
      except
        on E: Exception do
          vReport.Add(Format(cmDBCrudColumnsFailed, [vResource.DisplayName, E.Message]));
      end;
    end;
    Designer.Modified;
    ShowMessage(vReport.Text);
  finally
    vReport.Free;
  end;
end;

function TRALDBCrudModuleEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmDBCrudReadColumns;
end;

function TRALDBCrudModuleEditor.GetVerbCount: Integer;
begin
  Result := 1;
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
{$I raldbdsgn.lrs}
{$ENDIF}

end.
