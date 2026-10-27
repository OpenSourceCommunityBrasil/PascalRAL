/// Shows the messages an MQTT or MQTT-SN client receives in any TDataSet, one
/// row per message, so a grid shows them with no code.
unit RALMQTTDataSetLink;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALMQTTTypes, RALMQTTTopic, RALMQTTClient;

const
  /// Default of TRALMQTTDataSetLink.MaxRows.
  RALMQTTDataSetMaxRows = 1000;

type
  { Adds a row to DataSet for each message Client receives (of Subscription,
    or every message when it is empty), filling the fields it finds among
    Topic, Payload, QoS, Retain and Received; past MaxRows the oldest row
    goes. Any TDataSet that can append works: TFDMemTable, TClientDataSet,
    TBufDataset. Rows are added where the client's events run }
  TRALMQTTDataSetLink = class(TRALMQTTMessageListener)
  private
    FActive: boolean;
    FClient: TRALMQTTCustomClient;
    FDataSet: TDataSet;
    FMaxRows: IntegerRAL;
    FSubscription: StringRAL;

    procedure SetClient(AValue: TRALMQTTCustomClient);
    procedure SetDataSet(AValue: TDataSet);
    function Wanted(AMessage: TRALMQTTMessage): boolean;
    procedure Trim_;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Adds the row of AMessage (called by the client).
    procedure MessageArrived(AClient: TRALMQTTCustomClient; AMessage: TRALMQTTMessage);
      override;
  published
    /// Rows are added; False stops without unlinking.
    property Active: boolean read FActive write FActive default True;
    /// The client whose messages are shown.
    property Client: TRALMQTTCustomClient read FClient write SetClient;
    /// The dataset that gets the rows; opened by the application.
    property DataSet: TDataSet read FDataSet write SetDataSet;
    /// Rows kept; the oldest goes past it. 0 keeps all.
    property MaxRows: IntegerRAL read FMaxRows write FMaxRows default RALMQTTDataSetMaxRows;
    { Topic filter of the messages shown, usually one of Client.Subscriptions;
      empty shows every message }
    property Subscription: StringRAL read FSubscription write FSubscription;
  end;

implementation

{ TRALMQTTDataSetLink }

constructor TRALMQTTDataSetLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  FMaxRows := RALMQTTDataSetMaxRows;
end;

destructor TRALMQTTDataSetLink.Destroy;
begin
  SetClient(nil);
  SetDataSet(nil);
  inherited Destroy;
end;

procedure TRALMQTTDataSetLink.SetClient(AValue: TRALMQTTCustomClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
  begin
    FClient.RemoveListener(Self);
    FClient.RemoveFreeNotification(Self);
  end;
  FClient := AValue;
  if FClient <> nil then
  begin
    FClient.FreeNotification(Self);
    FClient.AddListener(Self);
  end;
end;

procedure TRALMQTTDataSetLink.SetDataSet(AValue: TDataSet);
begin
  if FDataSet = AValue then
    Exit;
  if FDataSet <> nil then
    FDataSet.RemoveFreeNotification(Self);
  FDataSet := AValue;
  if FDataSet <> nil then
    FDataSet.FreeNotification(Self);
end;

procedure TRALMQTTDataSetLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FClient then
    FClient := nil
  else if AComponent = FDataSet then
    FDataSet := nil;
end;

function TRALMQTTDataSetLink.Wanted(AMessage: TRALMQTTMessage): boolean;
var
  vItem: TRALMQTTSubscriptionItem;
begin
  if FSubscription = '' then
  begin
    Result := True;
    Exit;
  end;
  vItem := nil;
  if FClient <> nil then
    vItem := FClient.Subscriptions.Find(FSubscription);
  if vItem <> nil then
    Result := vItem.Matches(AMessage)
  else
    Result := RALMQTTTopicMatches(FSubscription, AMessage.Topic);
end;

procedure TRALMQTTDataSetLink.Trim_;
begin
  if (FMaxRows <= 0) or (FDataSet.RecordCount <= FMaxRows) then
    Exit;
  FDataSet.DisableControls;
  try
    while FDataSet.RecordCount > FMaxRows do
    begin
      FDataSet.First;
      FDataSet.Delete;
    end;
    FDataSet.Last;
  finally
    FDataSet.EnableControls;
  end;
end;

procedure TRALMQTTDataSetLink.MessageArrived(AClient: TRALMQTTCustomClient;
  AMessage: TRALMQTTMessage);
var
  vField: TField;
  vPayload: TBytes;
begin
  if (not FActive) or (FDataSet = nil) or (not FDataSet.Active) or not Wanted(AMessage) then
    Exit;
  FDataSet.Append;
  try
    vField := FDataSet.FindField('Topic');
    if vField <> nil then
      vField.AsString := string(AMessage.Topic);
    vField := FDataSet.FindField('Payload');
    if vField <> nil then
    begin
      // a binary field takes the bytes as they came, any other the text
      if vField.DataType in [ftBlob, ftBytes, ftVarBytes, ftGraphic] then
      begin
        vPayload := AMessage.Payload;
        vField.AsBytes := vPayload;
      end
      else
        vField.AsString := string(AMessage.AsString);
    end;
    vField := FDataSet.FindField('QoS');
    if vField <> nil then
      vField.AsInteger := Ord(AMessage.QoS);
    vField := FDataSet.FindField('Retain');
    if vField <> nil then
      vField.AsBoolean := AMessage.Retain;
    vField := FDataSet.FindField('Received');
    if vField <> nil then
      vField.AsDateTime := AMessage.Received;
    FDataSet.Post;
  except
    FDataSet.Cancel;
    raise;
  end;
  Trim_;
end;

end.
