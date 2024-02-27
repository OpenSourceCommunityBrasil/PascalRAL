unit RALDatasetStorage;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, DB,
  RALStorage, RALTypes;

type

  { TRALDatasetStorage }

  TRALDatasetStorage = class(TPersistent)
  private
    FDataset : TDataSet;
    FStorageLink : TRALStorageLink;
  public
    procedure LoadFromStream(AStream : TStream);
    procedure SaveToStream(AStream : TStream);

    procedure LoadFromFile(AFileName : StringRAL);
    procedure SaveToFile(AFileName : StringRAL);
  published
    property Dataset : TDataSet read FDataset write FDataset;
    property StorageLink : TRALStorageLink read FStorageLink write FStorageLink;
  end;

implementation

{ TRALDatasetStorage }

procedure TRALDatasetStorage.LoadFromStream(AStream: TStream);
var
  vStorLink : TRALStorage;
  vInt : integer;
begin
  vStorLink := FStorageLink.StorageClass.Create;
  try
    vStorLink.Load(AStream, smRead);

    FDataset.Close;
    if FDataset.FieldCount = 0 then
      vStorLink.CreateFieldsDefs(FDataset.FieldDefs);
    FDataset.Open;

    FDataset.DisableControls;
    while not vStorLink.EOF do begin
      FDataset.Append;
      for vInt := 0 to Pred(FDataset.FieldCount) do
        vStorLink.ReadField(FDataset.Fields[vInt]);
      FDataset.Post;
      vStorLink.Next;
    end;
    FDataset.EnableControls;
    FDataset.First;
  finally
    FreeAndNil(vStorLink);
  end;
end;

procedure TRALDatasetStorage.SaveToStream(AStream: TStream);
var
  vStorLink : TRALStorage;
  vInt : integer;
  vBookMark : TBookMark;
begin
  vStorLink := FStorageLink.StorageClass.Create;
  try
    vStorLink.AssignFields(FDataset.Fields);
    vStorLink.Load(AStream, smWrite);

    FDataset.DisableControls;

    if not FDataset.IsUniDirectional then
    begin
      vBookMark := FDataset.GetBookmark;
      FDataset.First;
    end;

    while not FDataset.EOF do begin
      vStorLink.Append;
      for vInt := 0 to Pred(FDataset.FieldCount) do
        vStorLink.WriteField(FDataset.Fields[vInt]);
      vStorLink.Post;
      FDataset.Next;
    end;

    if not FDataset.IsUniDirectional then
    begin
      FDataset.GotoBookmark(vBookMark);
      FDataset.FreeBookmark(vBookMark);
    end;

    FDataset.EnableControls;
  finally
    vStorLink.Free;
  end;
end;

procedure TRALDatasetStorage.LoadFromFile(AFileName : StringRAL);
var
  vStream : TFileStream;
begin
  if FileExists(AFileName) then
  begin
    vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      vStream.Position := 0;
      LoadFromStream(vStream);
    finally
      FreeAndNil(vStream);
    end;
  end;
end;

procedure TRALDatasetStorage.SaveToFile(AFileName : StringRAL);
var
  vStream : TBufferedFileStream;
begin
  vStream := TBufferedFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

end.

