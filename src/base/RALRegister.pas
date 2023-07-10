unit RALRegister;

interface

uses
//  {$IFDEF FPC}
//  LResources, LazarusPackageIntf, PropEdits, ComponentEditors,
//  {$ELSE}
//  DesignIntf, DesignEditors, // DsgnIntf
//  {$ENDIF}
  Classes, SysUtils,
  RALBase, RALAuthentication;

//type
//  TRALAboutFormProperty = class(TPropertyEditor)
//  public
//    procedure Edit; override;
//    function GetAttributes: TPropertyAttributes; override;
//    function GetValue: string; override;
//  end;

procedure Register;

implementation

procedure Register;
begin
//  RegisterPropertyEditor(TypeInfo(TRALAboutInfo), nil, 'About',
//    TRALAboutFormProperty);
  RegisterComponents('RAL - Authentication', [TRALBasicAuth, TRALJWTAuth])
end;

{ TRALAboutDialogProperty }

//procedure TRALAboutDialogProperty.Edit;
//begin
//  inherited;
//  ShowRALAboutForm;
//end;
//
//function TRALAboutDialogProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog, paReadOnly];
//end;
//
//function TRALAboutDialogProperty.GetValue: string;
//begin
//  Result := 'RAL - REST API Lite';
//end;

end.
