/// Class to create(register) components on pallete
unit RALRegister;

{$I PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  {$IFDEF DELPHI2005UP}
  ToolsAPI,
  {$ENDIF}
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  Classes, SysUtils, DesignEditors, DesignIntf, StringsEdit,
  RALConsts, RALAuthentication, RALWebModule, RALClient, RALCompress,
  RALTypes, RALServer;

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
  RegisterComponents('RAL - Client', [TRALClientBasicAuth, TRALClientJWTAuth]);
  RegisterComponents('RAL - Modules', [TRALWebModule]);
  RegisterPropertyEditor(TypeInfo(TStrings), TRALClientBase, 'BaseURL', TRALBaseURLEditor);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALClientBase, 'CompressType', TRALCompressEditor);
  RegisterPropertyEditor(TypeInfo(TRALCompressType), TRALServer, 'CompressType', TRALCompressEditor);
end;

{ TRALBaseURLEditor }

procedure TRALBaseURLEditor.Edit;
var
  vStr : TStringsEditDlg;
begin
  inherited;
  vStr := TStringsEditDlg.Create(nil);
  try
    vStr.Memo.Text := GetValue;
    vStr.ShowModal;
    SetValue(vStr.Memo.Text);
  finally
    FreeAndNil(vStr);
  end;
end;

function TRALBaseURLEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paValueEditable];
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
  vStr := TRALCompress.GetInstaledList;
  try
    for vInt := 0 to Pred(vStr.Count) do
      Proc(vStr.Strings[vInt]);
  finally
    FreeAndNil(vStr);
  end;
end;

{$IFDEF FPC}
initialization
{$I pascalral.lrs}
{$ENDIF}

end.
