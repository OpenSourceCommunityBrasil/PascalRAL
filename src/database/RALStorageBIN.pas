unit RALStorageBIN;

interface

uses
  Classes, SysUtils, DB,
  RALStorage, RALTypes, RALMIMETypes;

type
  { TRALStorageBIN }

  TRALStorageBIN = class(TRALStorage)
  private
    FRecords: Int64RAL;
    FPosRecords: Int64RAL;
  protected
    function WriteFields: boolean; override;
    function ReadFields: boolean; override;
    procedure Next; override;
    function EOF: boolean; override;
  public
    constructor Create; override;

    procedure Post; override;
    procedure Close; override;
  end;

  { TRALStorageBINLink }

  TRALStorageBINLink = class(TRALStorageLink)
  protected
    function GetStorageClass: TRALStorageClass; override;
    function GetContentType: StringRAL; override;
  end;

implementation

{ TRALDataStorageBIN }

function TRALStorageBIN.WriteFields: boolean;
var
  vInt: IntegerRAL;
  vIntAux: IntegerRAL;
  vByte: Byte;
  vStr: StringRAL;
begin
  Result := inherited WriteFields;

  if not Result then
    Exit;

  vInt := FieldDefs.Count;
  Stream.Write(vInt, SizeOf(vInt));

  for vInt := 0 to Pred(FieldDefs.Count) do
  begin
    // name
    vByte := Length(Fields[vInt].Name);
    Stream.Write(vByte, SizeOf(vByte));

    vStr := Fields[vInt].Name;
    Stream.Write(vStr[PosIniStr], vByte);

    // datatype
    vByte := Fields[vInt].DataTypeByte;
    Stream.Write(vByte, SizeOf(vByte));

    // size
    vIntAux := Fields[vInt].Size;
    Stream.Write(vIntAux, SizeOf(vIntAux));

    // precision
    vIntAux := Fields[vInt].Precision;
    Stream.Write(vIntAux, SizeOf(vIntAux));

    // flags
    vByte := Fields[vInt].FlagByte;
    Stream.Write(vByte, SizeOf(vByte));
  end;

  FPosRecords := Stream.Position;
  FRecords := 0;
  Stream.Write(FRecords, SizeOf(FRecords));

  Result := True;
end;

function TRALStorageBIN.ReadFields: boolean;
var
  vFields: IntegerRAL;
  vField: TRALStorageField;
  vInt, vIntAux: IntegerRAL;
  vByte: Byte;
  vStr: StringRAL;
begin
  FieldDefs.Clear;

  Stream.Read(vFields, SizeOf(vFields));

  for vInt := 0 to Pred(vFields) do
  begin
    vField := TRALStorageField(FieldDefs.Add);

    // name
    Stream.Read(vByte, SizeOf(vByte));
    SetLength(vStr, vByte);
    Stream.Read(vStr[PosIniStr], vByte);
    vField.Name := vStr;

    // datatype
    Stream.Read(vByte, SizeOf(vByte));
    vField.DataTypeByte := vByte;

    // size
    Stream.Read(vIntAux, SizeOf(vIntAux));
    vField.Size := vIntAux;

    // precision
    Stream.Read(vIntAux, SizeOf(vIntAux));
    vField.Precision := vIntAux;

    // flags
    Stream.Read(vByte, SizeOf(vByte));
    vField.FlagByte := vByte;
  end;

  FPosRecords := Stream.Position;
  Stream.Read(FRecords, SizeOf(FRecords));

  if FRecords > 0 then
    Next
  else
    FRecords := -1;

  Result := True;
end;

procedure TRALStorageBIN.Next;
var
  vInt: IntegerRAL;
  vInt64: Int64RAL;
  vBool: boolean;
  vBytes: TBytes;
begin
  if FRecords < 0 then
    Exit;

  FieldDefs.ClearFields;

  for vInt := 0 to Pred(FieldDefs.Count) do
  begin
    Stream.Read(vBool, SizeOf(vBool));

    if not vBool then
    begin
      vInt64 := Fields[vInt].Size;
      if Fields[vInt].DataType in [
      {$IFNDEF FPC}
        ftStream,
      {$ENDIF}
      ftString, ftBlob, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftOraBlob,
        ftOraClob, ftGuid, ftFixedWideChar, ftWideMemo] then
        Stream.Read(vInt64, SizeOf(vInt64));

      if vInt64 > 0 then
      begin
        SetLength(vBytes, vInt64);
        Stream.Read(vBytes[0], vInt64);
        Fields[vInt].AsBytes := vBytes;;
      end;
    end;
  end;
  FRecords := FRecords - 1;
end;

function TRALStorageBIN.EOF: boolean;
begin
  Result := FRecords = -1;
end;

constructor TRALStorageBIN.Create;
begin
  inherited Create;
  FRecords := 0;
  FPosRecords := 0;
end;

procedure TRALStorageBIN.Post;
var
  vInt: Integer;
  vInt64: Int64;
  vBytes: TBytes;
  vBool: boolean;
begin
  for vInt := 0 to Pred(FieldDefs.Count) do
  begin
    vBool := Fields[vInt].IsNull;
    Stream.Write(vBool, SizeOf(vBool));

    if not vBool then
    begin
      vBytes := Fields[vInt].AsBytes;

      vInt64 := Length(vBytes);
      if Fields[vInt].DataType in [
      {$IFNDEF FPC}
        ftStream,
      {$ENDIF}
      ftString, ftBlob, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftOraBlob,
        ftOraClob, ftGuid, ftFixedWideChar, ftWideMemo] then
        Stream.Write(vInt64, SizeOf(vInt64));

      if vInt64 > 0 then
        Stream.Write(vBytes[0], vInt64);
    end;
  end;

  FRecords := FRecords + 1;
end;

procedure TRALStorageBIN.Close;
begin
  if (Stream <> nil) and (Stream.Position > 0) then
  begin
    Stream.Position := FPosRecords;
    Stream.Write(FRecords, SizeOf(FRecords));

    Stream.Position := Stream.Size;
  end;
  inherited Close;
end;

{ TRALStorageBINLink }

function TRALStorageBINLink.GetStorageClass: TRALStorageClass;
begin
  Result := TRALStorageBIN;
end;

function TRALStorageBINLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONOCTETSTREAM;
end;

initialization

RegisterClass(TRALStorageBINLink);

end.
