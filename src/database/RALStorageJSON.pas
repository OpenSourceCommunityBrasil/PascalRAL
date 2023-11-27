unit RALStorageJSON;

interface

uses
  Classes, SysUtils,
  RALStorage, RALJSON, RALTypes, RALMIMETypes;

type
  { TRALStorageJSON }

  TRALStorageJSON = class(TRALStorage)
  private
    FRecords : int64;
  protected
    function WriteFields : boolean; override;
    function ReadFields: boolean; override;
  public
    procedure Post; override;
    procedure Close; override;
  end;

  { TRALStorageJSONLink }

  TRALStorageJSONLink = class(TRALStorageLink)
  protected
    function GetStorageClass : TRALStorageClass; override;
    function GetContentType : StringRAL; override;
  end;

implementation

{ TRALDataStorageJSON }

function TRALStorageJSON.WriteFields : boolean;
var
  vStr : string;
  vInt : integer;
begin
  Result := inherited WriteFields;

  if not Result then
    Exit;

  vStr := vStr + '{"fd":[';
  for vInt := 0 to Pred(FieldDefs.Count) do
  begin
    if vInt > 0 then
      vStr := vStr + ',';

    vStr := vStr + Format('["%s",%d,%d,%d]',[Fields[vInt].Name,
                                             Fields[vInt].DataTypeByte,
                                             Fields[vInt].Size,
                                             Fields[vInt].FlagByte]);
  end;
  vStr := vStr + '],"ln":[';

  FRecords := 0;

  Stream.Write(vStr[PosIniStr], Length(vStr));
end;

function TRALStorageJSON.ReadFields : boolean;
begin
  Result := False;
end;

procedure TRALStorageJSON.Post;
var
  vStr : StringRAL;
  vInt : Integer;
begin
  vStr := '';

  if FRecords > 0 then
    vStr := ',';

  vStr := vStr + '[';
  for vInt := 0 to Pred(FieldDefs.Count) do
  begin
    if vInt > 0 then
      vStr := vStr + ',';
    vStr := vStr + Fields[vInt].AsJSON;
  end;
  vStr := vStr + ']';

  Stream.Write(vStr[PosIniStr], Length(vStr));
  FRecords := FRecords + 1;
end;

procedure TRALStorageJSON.Close;
var
  vStr : StringRAL;
begin
  if (Stream <> nil) and (Stream.Position > 0) then
  begin
    vStr := ']}';
    Stream.Write(vStr[PosIniStr], Length(vStr));
  end;
  inherited Close;
end;

{ TRALStorageJSONLink }

function TRALStorageJSONLink.GetStorageClass: TRALStorageClass;
begin
  Result := TRALStorageJSON;
end;

function TRALStorageJSONLink.GetContentType : StringRAL;
begin
  Result := rctAPPLICATIONJSON;
end;

initialization
  RegisterClass(TRALStorageJSONLink);

end.

