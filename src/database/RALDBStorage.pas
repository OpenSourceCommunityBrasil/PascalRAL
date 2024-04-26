unit RALDBStorage;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects, RALDBTypes;

type
  TRALFieldCharCase = (fcNone, fcUpper, fcLower);

  { TRALDBStorage }

  TRALDBStorage = class(TPersistent)
  private
    FFieldCharCase : TRALFieldCharCase;
  protected
    function GetStoreVersion : byte;
    function CharCaseValue(AValue : StringRAL) : StringRAL;
  public
    procedure SaveToStream(ADataset : TDataSet; AStream : TStream); virtual; abstract;
    procedure LoadFromStream(ADataset : TDataSet; AStream : TStream); virtual; abstract;
  published
    property FieldCharCase : TRALFieldCharCase read FFieldCharCase write FFieldCharCase;
  end;

  TRALDBStorageClass = class of TRALDBStorage;
  TRALDBStorageClassLink = class of TRALDBStorageLink;

  { TRALDBStorageLink }

  TRALDBStorageLink = class(TRALComponent)
  private
    FFieldCharCase : TRALFieldCharCase;
  protected
    function GetContentType: StringRAL; virtual;
    class function GetDeclaredStorageLink : TRALDBStorageClassLink;
    class function GetDefaultStorage : TRALDBStorage; virtual;
  public
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); overload;
    function SaveToStream(ADataset: TDataSet): TStream; overload;
    procedure SaveToFile(ADataset: TDataSet; AFileName: StringRAL);

    procedure LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream);

    function GetStorage : TRALDBStorage; virtual;
  published
    property ContentType: StringRAL read GetContentType;
    property FieldCharCase : TRALFieldCharCase read FFieldCharCase write FFieldCharCase;
  end;

implementation

const
  cStorageLinkClass : array[0..1] of StringRAL = ('TRALDBStorageBINLink',
                                                  'TRALDBStorageJSONLink');

{ TRALDBStorage }

function TRALDBStorage.GetStoreVersion: byte;
begin
  Result := 1;
end;

function TRALDBStorage.CharCaseValue(AValue: StringRAL): StringRAL;
begin
  case FieldCharCase of
    fcUpper : AValue := UpperCase(AValue);
    fcLower : AValue := LowerCase(AValue);
  end;
  Result := AValue;
end;

{ TRALDBStorageLink }

function TRALDBStorageLink.GetContentType: StringRAL;
var
  vClassStor: TRALDBStorageClassLink;
  vLink : TRALDBStorageLink;
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
    raise Exception.Create('No TRALStorageLink found!');
  end;
end;

class function TRALDBStorageLink.GetDeclaredStorageLink: TRALDBStorageClassLink;
var
  vLinks : IntegerRAL;
begin
  Result := nil;
  for vLinks := Low(cStorageLinkClass) to High(cStorageLinkClass) do
  begin
    Result := TRALDBStorageClassLink(GetClass(cStorageLinkClass[vLinks]));
    if Result <> nil then
      Break;
  end;
end;

class function TRALDBStorageLink.GetDefaultStorage: TRALDBStorage;
var
  vClassStor: TRALDBStorageClassLink;
  vLink : TRALDBStorageLink;
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
    raise Exception.Create('No TRALStorageLink found!');
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

procedure TRALDBStorageLink.SaveToFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStream: TBufferedFileStream;
begin
  vStream := TBufferedFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(ADataset, vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALDBStorageLink.LoadFromFile(ADataset: TDataSet; AFileName: StringRAL);
var
  vStream : TFileStream;
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

end.
