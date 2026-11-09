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
    FAncestorName: string;
    FAuth: integer;
    FEngine: integer;
    FFormClass: string;
    FFormName: string;
    FModuleName: string;
    FOptions: integer;
    FUnitName: string;
  public
    constructor Create(AModulename, AFormName, AAncestorName, AFormClass,
                       AUnitName: string; AEngine, AAuth, AOptions : integer);

    function GetAge: TDateTime; virtual;
    function GetSource: string; virtual;
  published
    property AncestorName: string read FAncestorName write FAncestorName;
    property Auth: integer read FAuth write FAuth;
    property Engine: integer read FEngine write FEngine;
    property FormClass: string read FFormClass write FFormClass;
    property FormName: string read FFormName write FFormName;
    property ModuleName: string read FModuleName write FModuleName;
    property Options: integer read FOptions write FOptions;
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
