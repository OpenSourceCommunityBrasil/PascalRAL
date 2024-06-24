unit RALWizardObjects;

interface

uses
  Classes, SysUtils, ToolsAPI;

type
  TRALWizardBaseFile = class(TInterfacedObject)
  private
    FModuleName: string;
    FFormName: string;
    FAncestorName: string;
    FFrameName: string;
    FFrameUnit: string;
  public
    constructor Create(AModulename, AFormName, AAncestorName, AFrameName,
                       AFrameUnit: string);
  published
    property ModuleName: string read FModuleName write FModuleName;
    property FormName: string read FFormName write FFormName;
    property AncestorName: string read FAncestorName write FAncestorName;
    property FrameName: string read FFrameName write FFrameName;
    property FrameUnit: string read FFrameUnit write FFrameUnit;
  end;

implementation

{ TRALWizardBaseFile }

constructor TRALWizardBaseFile.Create(AModulename, AFormName, AAncestorName,
  AFrameName, AFrameUnit: string);
begin
  inherited Create;
  FModuleName := AModulename;
  FFormName := AFormName;
  FAncestorName := AAncestorName;
  FFrameName := AFrameName;
  FFrameUnit := AFrameUnit;
end;

end.
