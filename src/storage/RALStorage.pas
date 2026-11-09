/// Base unit for the Storage exporter components
unit RALStorage;

interface

uses
  {$IFDEF FPC}
  bufstream,
  {$ENDIF}
  Classes, SysUtils, DB, DateUtils, TypInfo, FMTBcd,
  RALExceptions, RALErrorCodes, RALTypes, RALCustomObjects, RALStorageTypes, RALBase64,
  RALConsts, RALStream;

type
  /// How field names are cased when written.
  TRALFieldCharCase = (fcNone, fcUpper, fcLower);
  /// The storage formats; rsfAuto picks the first linked.
  TRALStorageFormat = (rsfAuto, rsfBIN, rsfJSON, rsfBSON, rsfCSV, rsfXML);

  /// Base of the storages: writes a dataset to a stream and reads it back.
  TRALStorage = class(TPersistent)
  private
    FFieldCharCase: TRALFieldCharCase;
  protected
    /// Names of the fields being written or read, for the descendants.
    FFieldNames: array of StringRAL;
    /// The RAL type of each field.
    FFieldTypes: array of TRALFieldType;
    /// The dataset field of each stored field, or nil.
    FFoundFields: array of TField;
    /// Which fields were read-only before loading.
    FWasReadOnly: array of boolean;

    /// AValue cased by FieldCharCase.
    function CharCaseValue(AValue: StringRAL): StringRAL;
    /// The version of the format written.
    function GetStoreVersion: byte;
    { Every format writes its records through TField, and a field the server
      reported as read-only - an aggregate like count(*), a computed column -
      refuses the assignment and takes the whole record with it: the dataset
      arrived with the right columns and no rows at all. Read-only describes
      what the server accepts on an update; it cannot mean the client may not
      fill in the row it was just sent. Each format lifts it around its record
      loop and puts it back afterwards, so what a grid allows does not change. }
    procedure LiftReadOnly;
    { An sftBCD value: through AsBCD, so a decimal field keeps every digit
      and a float or currency field converts as it would from any TBcd. }
    procedure ReadFieldBCD(AField: TField; const AValue: TBcd);
    /// Puts a boolean into AField; nil does nothing.
    procedure ReadFieldBoolean(AField: TField; AValue: Boolean);
    /// Puts a byte into AField; nil does nothing.
    procedure ReadFieldByte(AField: TField; AValue: byte);
    /// Puts a date into AField; nil does nothing.
    procedure ReadFieldDateTime(AField: TField; AValue: TDateTime); overload;
    /// Puts a unix time into AField; nil does nothing.
    procedure ReadFieldDateTime(AField: TField; AValue: Int64RAL); overload;
    /// Puts a date written as text into AField; nil does nothing.
    procedure ReadFieldDateTime(AField: TField; AValue: StringRAL); overload;
    /// Puts a number into AField; nil does nothing.
    procedure ReadFieldFloat(AField: TField; AValue: Double);
    /// Puts a 64-bit integer into AField; nil does nothing.
    procedure ReadFieldInt64(AField: TField; AValue: Int64RAL);
    /// Puts an integer into AField; nil does nothing.
    procedure ReadFieldInteger(AField: TField; AValue: Integer);
    /// Puts an unsigned 32-bit integer into AField; nil does nothing.
    procedure ReadFieldLongWord(AField: TField; AValue: LongWord);
    /// Puts a signed byte into AField; nil does nothing.
    procedure ReadFieldShortint(AField: TField; AValue: Shortint);
    /// Puts a 16-bit integer into AField; nil does nothing.
    procedure ReadFieldSmallint(AField: TField; AValue: Smallint);
    /// Puts a base64 blob into AField; nil does nothing.
    procedure ReadFieldStream(AField: TField; AValue: StringRAL); overload;
    /// Puts a blob into AField; nil does nothing.
    procedure ReadFieldStream(AField: TField; AValue: TStream); overload;
    /// Puts a text into AField; nil does nothing.
    procedure ReadFieldString(AField: TField; AValue: StringRAL);
    /// Puts an unsigned 16-bit integer into AField; nil does nothing.
    procedure ReadFieldWord(AField: TField; AValue: Word);
    /// Gives back the read-only flags lifted for loading.
    procedure RestoreReadOnly;
  public
    /// Loads ADataset from AStream.
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); virtual; abstract;
    /// Writes ADataset to AStream.
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); virtual; abstract;
  published
    /// How field names are cased when written.
    property FieldCharCase: TRALFieldCharCase read FFieldCharCase write FFieldCharCase;
  end;

  /// Class of a storage.
  TRALStorageClass = class of TRALStorage;
  /// Class of a storage link.
  TRALStorageLinkClass = class of TRALStorageLink;

  /// The component that picks a storage format for whoever saves or loads datasets.
  TRALStorageLink = class(TRALComponent)
  private
    FFieldCharCase: TRALFieldCharCase;
    FStorageFormat: TRALStorageFormat;
  protected
    function GetContentType: StringRAL; virtual;
    /// The first link class registered, or nil.
    class function GetDeclaredStorageLink: TRALStorageLinkClass;
    /// A storage of the first link registered, or nil; the caller frees it.
    class function GetDefaultStorage: TRALStorage; virtual;
    /// Sets the format the link stands for.
    procedure SetStorageFormat(AFormat: TRALStorageFormat);
  public
    constructor Create(AOwner : TComponent); override;

    { Whether a database answer may go in the native format of the server's driver
      to a client whose dataset is of AClientDriver; by default when both match. }
    function AllowNative(AServerDriver, AClientDriver: Byte): boolean; virtual;
    /// A new link of the same class and properties; the caller frees it.
    function Clone : TRALStorageLink; virtual;
    /// A new storage set up by the link; the caller frees it.
    function GetStorage: TRALStorage; virtual;
    { The link class of AFormat; rsfAuto picks the first registered in BIN, JSON, BSON,
      CSV, XML order. }
    class function GetStorageClass(AFormat: TRALStorageFormat): TRALStorageLinkClass;
    /// Loads ADataset from a file.
    procedure LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
    /// Loads ADataset from a stream.
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream);
    /// Reads the properties of the link from a stream.
    procedure LoadPropsFromStream(AStream : TStream); overload;
    /// Reads the properties of the link with a binary reader.
    procedure LoadPropsFromStream(AWriter : TRALBinaryWriter); overload; virtual;
    /// Content type the server's driver writes a native answer in.
    function NativeContentType: StringRAL; virtual;
    /// Called with the dataset the link serves, before its properties are sent.
    procedure PrepareForDataSet(ADataSet: TDataSet); virtual;
    /// Writes the properties of the link to a stream.
    procedure SavePropsToStream(AStream : TStream); overload;
    /// Writes the properties of the link with a binary writer.
    procedure SavePropsToStream(AWriter : TRALBinaryWriter); overload; virtual;
    /// Writes ADataset to a file.
    procedure SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
    /// Writes ADataset to a stream.
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); overload;
    /// ADataset in a new stream, owned by the caller.
    function SaveToStream(ADataset: TDataSet): TStream; overload;
  published
    /// The media type of the format.
    property ContentType: StringRAL read GetContentType;
    /// How field names are cased when written.
    property FieldCharCase: TRALFieldCharCase read FFieldCharCase write FFieldCharCase;
    /// The format of the link.
    property StorageFormat: TRALStorageFormat read FStorageFormat;
  end;

{ Called after each record a storage load posts: keeps the change log of a
  TBufDataset short, which every Post searches. }
procedure RALStoragePosted(ADataset: TDataSet);
/// Called once a storage load is over: what it brought is data, not changes to apply.
procedure RALStorageLoaded(ADataset: TDataSet);
/// Makes ALink the class of AFormat; each storage unit calls it in its initialization.
procedure RegisterStorageLink(AFormat: TRALStorageFormat; ALink: TRALStorageLinkClass);
{ Forgets ALink wherever it is registered; each storage unit calls it in its
  finalization. }
procedure UnregisterStorageLink(ALink: TRALStorageLinkClass);

implementation

uses
  {$IFDEF FPC}
  BufDataset,
  {$ENDIF}
  RALTools, RALMIMETypes;

procedure RALStoragePosted(ADataset: TDataSet);
begin
  {$IFDEF FPC}
  if (ADataset is TCustomBufDataset) and (ADataset.RecordCount mod 1000 = 0) then
    TCustomBufDataset(ADataset).MergeChangeLog;
  {$ENDIF}
end;

procedure RALStorageLoaded(ADataset: TDataSet);
begin
  {$IFDEF FPC}
  if (ADataset is TCustomBufDataset) and ADataset.Active then
    TCustomBufDataset(ADataset).MergeChangeLog;
  {$ENDIF}
end;

var
  { The class of each format, kept by the format's own unit: looking it up costs
    no lock, and a format whose package is unloaded leaves no class behind. }
  gStorageLinkClasses: array[TRALStorageFormat] of TRALStorageLinkClass;

/// The link class registered for AFormat, or nil (also for a value out of range).
function StorageLinkClassOf(AFormat: TRALStorageFormat): TRALStorageLinkClass;
begin
  { a format read off the wire is a byte cast to the enum: compared as Cardinal,
    since Delphi compares a small enum as a signed byte }
  Result := nil;
  if Cardinal(AFormat) > Cardinal(Ord(High(TRALStorageFormat))) then
    Exit;
  Result := gStorageLinkClasses[AFormat];
end;

procedure RegisterStorageLink(AFormat: TRALStorageFormat; ALink: TRALStorageLinkClass);
begin
  if AFormat <> rsfAuto then
    gStorageLinkClasses[AFormat] := ALink;
end;

procedure UnregisterStorageLink(ALink: TRALStorageLinkClass);
var
  vFormat: TRALStorageFormat;
begin
  for vFormat := Low(TRALStorageFormat) to High(TRALStorageFormat) do
    if gStorageLinkClasses[vFormat] = ALink then
      gStorageLinkClasses[vFormat] := nil;
end;

  { TRALStorage }

function TRALStorage.GetStoreVersion: byte;
begin
  Result := 1;
end;

procedure TRALStorage.LiftReadOnly;
var
  vInt: IntegerRAL;
begin
  SetLength(FWasReadOnly, Length(FFoundFields));
  for vInt := 0 to Pred(Length(FFoundFields)) do
  begin
    FWasReadOnly[vInt] := (FFoundFields[vInt] <> nil) and FFoundFields[vInt].ReadOnly;
    if FWasReadOnly[vInt] then
      FFoundFields[vInt].ReadOnly := False;
  end;
end;

procedure TRALStorage.RestoreReadOnly;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Pred(Length(FWasReadOnly)) do
    if FWasReadOnly[vInt] then
      FFoundFields[vInt].ReadOnly := True;
  SetLength(FWasReadOnly, 0);
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

procedure TRALStorage.ReadFieldBCD(AField: TField; const AValue: TBcd);
begin
  if AField <> nil then
    AField.AsBCD := AValue;
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
    { ISO 8601 first: 'Z' as written, an offset brought to local time (see
      RALTryISO8601ToDateTime); anything else in the local format, and what
      neither reads raises EConvertError }
    if not RALTryISO8601ToDateTime(AValue, vDate) then
      if not TryStrToDateTime(string(AValue), vDate) then
        vDate := RALISO8601ToDateTime(AValue);
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

function TRALStorageLink.AllowNative(AServerDriver, AClientDriver: Byte): boolean;
begin
  Result := AServerDriver = AClientDriver;
end;

function TRALStorageLink.NativeContentType: StringRAL;
begin
  Result := rctAPPLICATIONOCTETSTREAM;
end;

procedure TRALStorageLink.PrepareForDataSet(ADataSet: TDataSet);
begin
  // a link that adapts to the client's dataset overrides this
end;

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
    raise ERALException.Create(ecStorageNotFound);
  end;
end;

class function TRALStorageLink.GetDeclaredStorageLink: TRALStorageLinkClass;
var
  vLinks: TRALStorageFormat;
begin
  Result := nil;
  for vLinks := Low(TRALStorageFormat) to High(TRALStorageFormat) do
  begin
    Result := StorageLinkClassOf(vLinks);
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
    raise ERALException.Create(ecStorageNotFound);
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
  try
    SavePropsToStream(vWriter);
  finally
    FreeAndNil(vWriter);
  end;
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
    RALStorageLoaded(ADataset);
  finally
    FreeAndNil(vStor);
  end;
end;

procedure TRALStorageLink.LoadPropsFromStream(AWriter: TRALBinaryWriter);
begin
  FFieldCharCase := TRALFieldCharCase(AWriter.ReadEnum(Ord(High(TRALFieldCharCase))));
end;

procedure TRALStorageLink.LoadPropsFromStream(AStream: TStream);
var
  vWriter: TRALBinaryWriter;
begin
  vWriter := TRALBinaryWriter.Create(AStream);
  try
    LoadPropsFromStream(vWriter);
  finally
    FreeAndNil(vWriter);
  end;
end;

function TRALStorageLink.GetStorage: TRALStorage;
begin
  Result := GetDefaultStorage;
end;

class function TRALStorageLink.GetStorageClass(AFormat: TRALStorageFormat): TRALStorageLinkClass;
const
  // the order of preference of rsfAuto
  cAuto: array [0 .. 4] of TRALStorageFormat = (rsfBIN, rsfJSON, rsfBSON, rsfCSV, rsfXML);
var
  vInt: IntegerRAL;
begin
  if AFormat <> rsfAuto then
  begin
    Result := StorageLinkClassOf(AFormat);
    Exit;
  end;

  Result := nil;
  for vInt := Low(cAuto) to High(cAuto) do
  begin
    Result := StorageLinkClassOf(cAuto[vInt]);
    if Result <> nil then
      Break;
  end;
end;

end.
