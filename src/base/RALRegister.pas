unit RALRegister;

interface

uses
 Classes, SysUtils, RALBase,
 {$IFDEF FPC}
   LResources, LazarusPackageIntf, PropEdits, ComponentEditors
 {$ELSE}
   DesignIntf, DesignEditors //DsgnIntf
 {$ENDIF};

type
  TRALAboutFormProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TRALAboutInfo), nil, 'About', TRALAboutFormProperty);
end;

{ TRALAboutDialogProperty }

procedure TRALAboutDialogProperty.Edit;
begin
  inherited;
  ShowRALAboutForm;
end;

function TRALAboutDialogProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TRALAboutDialogProperty.GetValue: string;
begin
  Result := 'RAL - REST API Lite' ;
end;

end.
