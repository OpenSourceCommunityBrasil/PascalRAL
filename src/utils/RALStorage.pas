/// Base unit for the Storage exporter components
unit RALStorage;

interface

uses
  {$IFDEF FPC}
  bufstream,
  {$ENDIF}
  Classes, SysUtils, DB, DateUtils, TypInfo,
  RALTypes, RALCustomObjects, RALDBTypes, RALBase64, RALConsts,
  RALStream;

type
  TRALFieldCharCase = (fcNone, fcUpper, fcLower);
  TRALStorageFormat = (rsfAuto, rsfBIN, rsfJSON, rsfBSON, rsfCSV);

  { TRALStorage }

  TRALStorage = class(TPersistent)
  private
    FFieldCharCase: TRALFieldCharCase;
  protected
    // variaveis para uso nas units filhas como controle
    FFieldNames: array of StringRAL;
    FFieldTypes: array of TRALFieldType;
    FFoundFields: array of TField;

    function GetStoreVersion: byte;
    function CharCaseValue(AValue: StringRAL): StringRAL;

    procedure ReadFieldBoolean(AField: TField; AValue: Boolean);
    procedure ReadFieldByte(AField: TField; AValue: byte);
    procedure ReadFieldDateTime(AField: TField; AValue: TDateTime); overload;
    procedure ReadFieldDateTime(AField: TField; AValue: Int64RAL); overload;
    procedure ReadFieldDateTime(AField: TField; AValue: StringRAL); overload;
    procedure ReadFieldFloat(AField: TField; AValue: Double);
    procedure ReadFieldInteger(AField: TField; AValue: Integer);
    procedure ReadFieldInt64(AField: TField; AValue: Int64RAL);
    procedure ReadFieldLongWord(AField: TField; AValue: LongWord);
    procedure ReadFieldShortint(AField: TField; AValue: Shortint);
    procedure ReadFieldSmallint(AField: TField; AValue: Smallint);
    procedure ReadFieldStream(AField: TField; AValue: StringRAL); overload;
    procedure ReadFieldStream(AField: TField; AValue: TStream); overload;
    procedure ReadFieldString(AField: TField; AValue: StringRAL);
    procedure ReadFieldWord(AField: TField; AValue: Word);
  public
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); virtual; abstract;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); virtual; abstract;
  published
    property FieldCharCase: TRALFieldCharCase read FFieldCharCase write FFieldCharCase;
  end;

  TRALStorageClass = class of TRALStorage;
  TRALStorageLinkClass = class of TRALStorageLink;

  { TRALStorageLink }

  TRALStorageLink = class(TRALComponent)
  private
    FFieldCharCase: TRALFieldCharCase;
    FStorageFormat: TRALStorageFormat;
  protected
    function GetContentType: StringRAL; virtual;
    class function GetDeclaredStorageLink: TRALStorageLinkClass;
    class function GetDefaultStorage: TRALStorage; virtual;

    procedure SetStorageFormat(AFormat: TRALStorageFormat);
  public
    constructor Create(AOwner : TComponent); override;

    function GetStorage: TRALStorage; virtual;
    procedure LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream);
    procedure SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); overload;
    function SaveToStream(ADataset: TDataSet): TStream; overload;

    procedure SavePropsToStream(AStream : TStream); overload;
    procedure SavePropsToStream(AWriter : TRALBinaryWriter); overload; virtual;

    procedure LoadPropsFromStream(AStream : TStream); overload;
    procedure LoadPropsFromStream(AWriter : TRALBinaryWriter); overload; virtual;
    function Clone : TRALStorageLink; virtual;

    class function GetStorageClass(AFormat: TRALStorageFormat): TRALStorageLinkClass;
  published
    property ContentType: StringRAL read GetContentType;
    property FieldCharCase: TRALFieldCharCase read FFieldCharCase write FFieldCharCase;
    property StorageFormat: TRALStorageFormat read FStorageFormat;
  end;

implementation

const
  cStorageLinkClass: array[TRALStorageFormat] of StringRAL = ('', 'TRALStorageBINLink',
    'TRALStorageJSONLink', 'TRALStorageBSONLink', 'TRALStorageCSVLink');

  { TRALStorage }

function TRALStorage.GetStoreVersion: byte;
begin
  Result := 1;
end;

function TRALStorage.CharCaseValue(AValue: StringRAL): StringRAL;
begin
  case FieldCharCase of
    fcUpper:
      AValue := UpperCase(AValue);
    fcLower:
      AValue := LowerCase(AValue);
  end;
  Result := AValue;
end;

procedure TRALStorage.ReadFieldString(AField: TField; AValue: StringRAL);
begin
  if AField <> nil then
    AField.AsString := AValue;
end;

procedure TRALStorage.ReadFieldShortint(AField: TField; AValue: Shortint);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALStorage.ReadFieldByte(AField: TField; AValue: byte);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALStorage.ReadFieldLongWord(AField: TField; AValue: LongWord);
begin
  if AField <> nil then
    AField.AsLargeInt := AValue;
end;

procedure TRALStorage.ReadFieldSmallint(AField: TField; AValue: Smallint);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALStorage.ReadFieldWord(AField: TField; AValue: Word);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALStorage.ReadFieldInteger(AField: TField; AValue: Integer);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALStorage.ReadFieldInt64(AField: TField; AValue: Int64RAL);
begin
  if AField <> nil then
    AField.AsLargeInt := AValue;
end;

procedure TRALStorage.ReadFieldBoolean(AField: TField; AValue: Boolean);
begin
  if AField <> nil then
    AField.AsBoolean := AValue;
end;

procedure TRALStorage.ReadFieldFloat(AField: TField; AValue: Double);
begin
  if AField <> nil then
    AField.AsFloat := AValue;
end;

procedure TRALStorage.ReadFieldDateTime(AField: TField; AValue: TDateTime);
begin
  if AField <> nil then
    AField.AsDateTime := AValue;
end;

procedure TRALStorage.ReadFieldDateTime(AField: TField; AValue: Int64RAL);
begin
  if AField <> nil then
    AField.AsDateTime := UnixToDateTime(AValue);
end;

procedure TRALStorage.ReadFieldDateTime(AField: TField; AValue: StringRAL);
var
  vDate: TDateTime;
begin
  if AField <> nil then
  begin
    if not TryStrToDateTime(AValue, vDate) then
      vDate := ISO8601ToDate(AValue);
    AField.AsDateTime := vDate;
  end;
end;

procedure TRALStorage.ReadFieldStream(AField: TField; AValue: StringRAL);
var
  vMem: TStream;
begin
  if AField <> nil then
  begin
    vMem := TRALBase64.DecodeAsStream(AValue);
    try
      TBlobField(AField).LoadFromStream(vMem);
    finally
      FreeAndNil(vMem);
    end;
  end;
end;

procedure TRALStorage.ReadFieldStream(AField: TField; AValue: TStream);
begin
  if AField <> nil then
  begin
    AValue.Position := 0;
    TBlobField(AField).LoadFromStream(AValue);
  end;
end;

{ TRALStorageLink }

function TRALStorageLink.Clone: TRALStorageLink;
var
  vStorageLinkClass : TRALStorageLinkClass;
begin
  Result := nil;
  if FStorageFormat = rsfAuto then
    Exit;

  vStorageLinkClass := GetStorageClass(FStorageFormat);
  if vStorageLinkClass <> nil then begin
    Result := vStorageLinkClass.Create(nil);
    Result.FieldCharCase := FFieldCharCase;
  end;
end;

constructor TRALStorageLink.Create(AOwner: TComponent);
begin
  inherited;
  FStorageFormat := rsfAuto;
end;

function TRALStorageLink.GetContentType: StringRAL;
var
  vClassStor: TRALStorageLinkClass;
  vLink: TRALStorageLink;
begin
  vClassStor := GetDeclaredStorageLink;
  if vClassStor <> nil then
  begin
    vLink := vClassStor.Create(nil);
    try
      Result := vLink.ContentType;
    finally
      vLink.Free;
    end;
  end
  else
  begin
    raise Exception.Create(emStorageNotFound);
  end;
end;

class function TRALStorageLink.GetDeclaredStorageLink: TRALStorageLinkClass;
var
  vLinks: TRALStorageFormat;
begin
  Result := nil;
  for vLinks := Low(cStorageLinkClass) to High(cStorageLinkClass) do
  begin
    Result := TRALStorageLinkClass(GetClass(cStorageLinkClass[vLinks]));
    if Result <> nil then
      Break;
  end;
end;

class function TRALStorageLink.GetDefaultStorage: TRALStorage;
var
  vClassStor: TRALStorageLinkClass;
  vLink: TRALStorageLink;
begin
  vClassStor := GetDeclaredStorageLink;
  if vClassStor <> nil then
  begin
    vLink := vClassStor.Create(nil);
    try
      Result := vLink.GetStorage;
    finally
      vLink.Free;
    end;
  end
  else
  begin
    raise Exception.Create(emStorageNotFound);
  end;
end;

procedure TRALStorageLink.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vStor: TRALStorage;
begin
  vStor := GetStorage;
  try
    vStor.SaveToStream(ADataset, AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

function TRALStorageLink.SaveToStream(ADataset: TDataSet): TStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(ADataset, Result);
  Result.Position := 0;
end;

procedure TRALStorageLink.SetStorageFormat(AFormat: TRALStorageFormat);
begin
  FStorageFormat := AFormat;
end;

procedure TRALStorageLink.SavePropsToStream(AStream: TStream);
var
  vWriter: TRALBinaryWriter;
begin
  vWriter := TRALBinaryWriter.Create(AStream);
end;

procedure TRALStorageLink.SavePropsToStream(AWriter: TRALBinaryWriter);
begin
  AWriter.WriteByte(Ord(FStorageFormat));
  AWriter.WriteByte(Ord(FFieldCharCase));
end;

procedure TRALStorageLink.SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStream: TRALBufFileStream;
begin
  vStream := TRALBufFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(ADataset, vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALStorageLink.LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(ADataset, vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALStorageLink.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vStor: TRALStorage;
begin
  vStor := GetStorage;
  try
    vStor.LoadFromStream(ADataset, AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

procedure TRALStorageLink.LoadPropsFromStream(AWriter: TRALBinaryWriter);
begin
//  FStorageFormat := TRALStorageFormat(AWriter.ReadByte);
  FFieldCharCase := TRALFieldCharCase(AWriter.ReadByte);
end;

procedure TRALStorageLink.LoadPropsFromStream(AStream: TStream);
var
  vWriter: TRALBinaryWriter;
begin
  vWriter := TRALBinaryWriter.Create(AStream);
end;

function TRALStorageLink.GetStorage: TRALStorage;
begin
  Result := GetDefaultStorage;
end;

class function TRALStorageLink.GetStorageClass(AFormat: TRALStorageFormat): TRALStorageLinkClass;
var
  vStr: TStringList;
  vInt: IntegerRAL;
begin
  Result := nil;
  vStr := TStringList.Create;
  try
    case AFormat of
      rsfAuto : begin
        vStr.Add(cStorageLinkClass[rsfBIN]);
        vStr.Add(cStorageLinkClass[rsfJSON]);
        vStr.Add(cStorageLinkClass[rsfBSON]);
        vStr.Add(cStorageLinkClass[rsfCSV]);
      end;
      rsfBIN  : vStr.Add(cStorageLinkClass[rsfBIN]);
      rsfJSON : vStr.Add(cStorageLinkClass[rsfJSON]);
      rsfBSON : vStr.Add(cStorageLinkClass[rsfBSON]);
      rsfCSV  : vStr.Add(cStorageLinkClass[rsfCSV]);
    end;

    for vInt := 0 to Pred(vStr.Count) do
    begin
      Result := TRALStorageLinkClass(GetClass(vStr.Strings[vInt]));
      if Result <> nil then
        Break;
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

end.
