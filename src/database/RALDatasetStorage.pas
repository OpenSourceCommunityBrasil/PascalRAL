unit RALDatasetStorage;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects;

type
  TRALStorageOption = (soFields, soFieldsParams, soData);
  TRALStorageOptions = set of TRALStorageOption;

  { TRALDatasetStorage }

  TRALDatasetStorage = class(TRALComponent)
  private
    FStorageOptions : TRALStorageOptions;
  public
    constructor Create(AOwner : TComponent); override;

    procedure SaveToStream(ADataset : TDataset; AStream : TStream); virtual; abstract; overload;
    function SaveToStream(ADataset : TDataset) : TStream; virtual; overload;

    procedure SaveToFile(ADataset : TDataset; AFile : StringRAL) virtual;

    procedure LoadFromStream(ADataset : TDataset; AStream : TStream); virtual; abstract;
    procedure LoadFromFile(ADataset : TDataset; AFile : StringRAL); virtual;
  published
    property StorageOptions : TRALStorageOptions read FStorageOptions write FStorageOptions;
  end;

implementation

{ TRALDatasetStorage }

constructor TRALDatasetStorage.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FStorageOptions := [soFields, soFieldsParams, soData];
end;

function TRALDatasetStorage.SaveToStream(ADataset : TDataset) : TStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(ADataset, Result);
end;

procedure TRALDatasetStorage.SaveToFile(ADataset : TDataset; AFile : StringRAL);
var
  vStream : TBufferedFileStream;
begin
  vStream := TBufferedFileStream.Create(AFile, fmCreate);
  try
    SaveToStream(ADataset, vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALDatasetStorage.LoadFromFile(ADataset : TDataset; AFile : StringRAL);
var
  vStream : TFileStream;
begin
  vStream := TFileStream.Create(AFile, fmOpenRead);
  try
    LoadFromStream(ADataset, vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

end.

