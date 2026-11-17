/// Registers TRALMQTTDataSetLink in the IDE, with the list of its Subscription.
unit RALModuleMQTTDBReg;

{$I ..\..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, PropEdits,
  {$ELSE}
  DesignEditors, DesignIntf,
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALMQTTClient, RALMQTTDataSetLink;

type
  { Property editor of TRALMQTTDataSetLink.Subscription: the topic filters of the
    subscriptions of its client; any other filter can still be typed. }
  TRALMQTTSubscriptionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

/// Registers the component and its property editor.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DAO', [TRALMQTTDataSetLink]);
  RegisterPropertyEditor(TypeInfo(StringRAL), TRALMQTTDataSetLink, 'Subscription',
    TRALMQTTSubscriptionProperty);
end;

{ TRALMQTTSubscriptionProperty }

function TRALMQTTSubscriptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TRALMQTTSubscriptionProperty.GetValues(Proc: TGetStrProc);
var
  vLink: TRALMQTTDataSetLink;
  vInt: Integer;
  vItem: TRALMQTTSubscriptionItem;
begin
  vLink := TRALMQTTDataSetLink(GetComponent(0));
  if vLink.Client = nil then
    Exit;
  for vInt := 0 to vLink.Client.Subscriptions.Count - 1 do
  begin
    vItem := vLink.Client.Subscriptions[vInt];
    if vItem.TopicFilter <> '' then
      Proc(string(vItem.TopicFilter));
  end;
end;

{$IFDEF FPC}
initialization
{$I ralmodulemqttdbdsgn.lrs}
{$ENDIF}

end.
