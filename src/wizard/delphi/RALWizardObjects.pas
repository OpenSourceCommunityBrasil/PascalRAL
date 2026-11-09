/// The base of the source files the Delphi wizard generates.
unit RALWizardObjects;

interface

uses
  Classes, SysUtils, ToolsAPI;

{
ModuleIdent, FormIdent, AncestorIdent,
FFormClass, FUnitName, FEngine
}

type
  /// Base of the sources the wizard writes (IOTAFile).
  TRALWizardBaseFile = class(TInterfacedObject, IOTAFile)
  private
    FAncestorName: string;
    FAuth: integer;
    FEngine: integer;
    FFormClass: string;
    FFormName: string;
    FModuleName: string;
    FOptions: integer;
    FUnitName: string;
  public
    /// Keeps the names and the options of the source.
    constructor Create(AModulename, AFormName, AAncestorName, AFormClass,
                       AUnitName: string; AEngine, AAuth, AOptions : integer);

    /// IOTAFile: -1, a new file.
    function GetAge: TDateTime; virtual;
    /// IOTAFile: the source text.
    function GetSource: string; virtual;
  published
    /// Ancestor of the form.
    property AncestorName: string read FAncestorName write FAncestorName;
    /// Authentication chosen.
    property Auth: integer read FAuth write FAuth;
    /// Engine chosen.
    property Engine: integer read FEngine write FEngine;
    /// Class of the form or module.
    property FormClass: string read FFormClass write FFormClass;
    /// Name of the form.
    property FormName: string read FFormName write FFormName;
    /// Name of the module.
    property ModuleName: string read FModuleName write FModuleName;
    /// Optional modules chosen (bits).
    property Options: integer read FOptions write FOptions;
    /// Name of the unit.
    property UnitName: string read FUnitName write FUnitName;
  end;

implementation

{ TRALWizardBaseFile }

constructor TRALWizardBaseFile.Create(AModulename, AFormName, AAncestorName,
                                      AFormClass, AUnitName: string;
                                      AEngine, AAuth, AOptions : integer);
begin
  inherited Create;
  FModuleName := AModulename;
  FFormName := AFormName;
  FAncestorName := AAncestorName;
  FFormClass := AFormClass;
  FUnitName := AUnitName;
  FAuth := AAuth;
  FOptions := AOptions;
  FEngine := AEngine;
end;

function TRALWizardBaseFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TRALWizardBaseFile.GetSource: string;
begin
  Result := '';
end;

end.
