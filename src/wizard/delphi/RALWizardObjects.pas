unit RALWizardObjects;

interface

uses
  Classes, SysUtils, ToolsAPI;

{
ModuleIdent, FormIdent, AncestorIdent,
FFormClass, FUnitName, FEngine
}

type
  TRALWizardBaseFile = class(TInterfacedObject, IOTAFile)
  private
    FModuleName: string;
    FFormName: string;
    FAncestorName: string;
    FFormClass: string;
    FUnitName: string;
    FEngine: integer;
    FAuth: integer;
    FOptions: integer;
  public
    constructor Create(AModulename, AFormName, AAncestorName, AFormClass,
                       AUnitName: string; AEngine, AAuth, AOptions : integer);

    function GetSource: string; virtual;
    function GetAge: TDateTime; virtual;
  published
    property ModuleName: string read FModuleName write FModuleName;
    property FormName: string read FFormName write FFormName;
    property AncestorName: string read FAncestorName write FAncestorName;
    property FormClass: string read FFormClass write FFormClass;
    property UnitName: string read FUnitName write FUnitName;
    property Engine: integer read FEngine write FEngine;
    property Auth: integer read FAuth write FAuth;
    property Options: integer read FOptions write FOptions;
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
