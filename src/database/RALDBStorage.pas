/// Base unit for the Storage exporter components
unit RALDBStorage;

interface

uses
  {$IFDEF FPC}
  bufstream,
  {$ENDIF}
  Classes, SysUtils, DB, DateUtils,
  RALTypes, RALCustomObjects, RALDBTypes, RALBase64, RALConsts;

type
  TRALFieldCharCase = (fcNone, fcUpper, fcLower);
  TRALStorageFormat = (rsfAuto, rsfBIN, rsfJSON, rsfBSON, rsfCSV);

  { TRALDBStorage }

  TRALDBStorage = class(TPersistent)
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

  TRALDBStorageClass = class of TRALDBStorage;
  TRALDBStorageLinkClass = class of TRALDBStorageLink;

  { TRALDBStorageLink }

  TRALDBStorageLink = class(TRALComponent)
  private
    FFieldCharCase: TRALFieldCharCase;
    FStorageFormat: TRALStorageFormat;
  protected
    function GetContentType: StringRAL; virtual;
    class function GetDeclaredStorageLink: TRALDBStorageLinkClass;
    class function GetDefaultStorage: TRALDBStorage; virtual;

    procedure SetStorageFormat(AFormat: TRALStorageFormat);
  public
    constructor Create(AOwner : TComponent); override;

    function GetStorage: TRALDBStorage; virtual;
    procedure LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream);
    procedure SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); overload;
    function SaveToStream(ADataset: TDataSet): TStream; overload;

    class function GetStorageClass(AFormat: TRALStorageFormat): TRALDBStorageLinkClass;
  published
    property ContentType: StringRAL read GetContentType;
    property FieldCharCase: TRALFieldCharCase read FFieldCharCase write FFieldCharCase;
    property StorageFormat: TRALStorageFormat read FStorageFormat;
  end;

implementation

const
  cStorageLinkClass: array[TRALStorageFormat] of StringRAL = ('', 'TRALDBStorageBINLink',
    'TRALDBStorageJSONLink', 'TRALDBStorageBSONLink', 'TRALDBStorageCSVLink');

  { TRALDBStorage }

function TRALDBStorage.GetStoreVersion: byte;
begin
  Result := 1;
end;

function TRALDBStorage.CharCaseValue(AValue: StringRAL): StringRAL;
begin
  case FieldCharCase of
    fcUpper:
      AValue := UpperCase(AValue);
    fcLower:
      AValue := LowerCase(AValue);
  end;
  Result := AValue;
end;

procedure TRALDBStorage.ReadFieldString(AField: TField; AValue: StringRAL);
begin
  if AField <> nil then
    AField.AsString := AValue;
end;

procedure TRALDBStorage.ReadFieldShortint(AField: TField; AValue: Shortint);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALDBStorage.ReadFieldByte(AField: TField; AValue: byte);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALDBStorage.ReadFieldLongWord(AField: TField; AValue: LongWord);
begin
  if AField <> nil then
    AField.AsLargeInt := AValue;
end;

procedure TRALDBStorage.ReadFieldSmallint(AField: TField; AValue: Smallint);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALDBStorage.ReadFieldWord(AField: TField; AValue: Word);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALDBStorage.ReadFieldInteger(AField: TField; AValue: Integer);
begin
  if AField <> nil then
    AField.AsInteger := AValue;
end;

procedure TRALDBStorage.ReadFieldInt64(AField: TField; AValue: Int64RAL);
begin
  if AField <> nil then
    AField.AsLargeInt := AValue;
end;

procedure TRALDBStorage.ReadFieldBoolean(AField: TField; AValue: Boolean);
begin
  if AField <> nil then
    AField.AsBoolean := AValue;
end;

procedure TRALDBStorage.ReadFieldFloat(AField: TField; AValue: Double);
begin
  if AField <> nil then
    AField.AsFloat := AValue;
end;

procedure TRALDBStorage.ReadFieldDateTime(AField: TField; AValue: TDateTime);
begin
  if AField <> nil then
    AField.AsDateTime := AValue;
end;

procedure TRALDBStorage.ReadFieldDateTime(AField: TField; AValue: Int64RAL);
begin
  if AField <> nil then
    AField.AsDateTime := UnixToDateTime(AValue);
end;

procedure TRALDBStorage.ReadFieldDateTime(AField: TField; AValue: StringRAL);
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

procedure TRALDBStorage.ReadFieldStream(AField: TField; AValue: StringRAL);
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

procedure TRALDBStorage.ReadFieldStream(AField: TField; AValue: TStream);
begin
  if AField <> nil then
  begin
    AValue.Position := 0;
    TBlobField(AField).LoadFromStream(AValue);
  end;
end;

{ TRALDBStorageLink }

constructor TRALDBStorageLink.Create(AOwner: TComponent);
begin
  inherited;
  FStorageFormat := rsfAuto;
end;

function TRALDBStorageLink.GetContentType: StringRAL;
var
  vClassStor: TRALDBStorageLinkClass;
  vLink: TRALDBStorageLink;
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

class function TRALDBStorageLink.GetDeclaredStorageLink: TRALDBStorageLinkClass;
var
  vLinks: TRALStorageFormat;
begin
  Result := nil;
  for vLinks := Low(cStorageLinkClass) to High(cStorageLinkClass) do
  begin
    Result := TRALDBStorageLinkClass(GetClass(cStorageLinkClass[vLinks]));
    if Result <> nil then
      Break;
  end;
end;

class function TRALDBStorageLink.GetDefaultStorage: TRALDBStorage;
var
  vClassStor: TRALDBStorageLinkClass;
  vLink: TRALDBStorageLink;
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

procedure TRALDBStorageLink.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorage;
  try
    vStor.SaveToStream(ADataset, AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

function TRALDBStorageLink.SaveToStream(ADataset: TDataSet): TStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(ADataset, Result);
  Result.Position := 0;
end;

procedure TRALDBStorageLink.SetStorageFormat(AFormat: TRALStorageFormat);
begin
  FStorageFormat := AFormat;
end;

procedure TRALDBStorageLink.SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
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

procedure TRALDBStorageLink.LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
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

procedure TRALDBStorageLink.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vStor: TRALDBStorage;
begin
  vStor := GetStorage;
  try
    vStor.LoadFromStream(ADataset, AStream);
  finally
    FreeAndNil(vStor);
  end;
end;

function TRALDBStorageLink.GetStorage: TRALDBStorage;
begin
  Result := GetDefaultStorage;
end;

class function TRALDBStorageLink.GetStorageClass(AFormat: TRALStorageFormat): TRALDBStorageLinkClass;
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
      Result := TRALDBStorageLinkClass(GetClass(vStr.Strings[vInt]));
      if Result <> nil then
        Break;
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

end.
