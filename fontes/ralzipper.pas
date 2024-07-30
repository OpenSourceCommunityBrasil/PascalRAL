unit ralzipper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Zipper;

type

  { TRALUnZipper }

  TRALUnZipper = class(TUnZipper)
  private
    FZipStream : TStream;
    FFolder: string;
  protected
    procedure RALOnOpenInputStream(Sender: TObject; var AStream: TStream);
    procedure RALOnCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  public
    constructor Create;
  published
    property ZipStream : TStream read FZipStream write FZipStream;
    property Folder : string read FFolder write FFolder;
  end;

implementation

{ TRALUnZipper }

procedure TRALUnZipper.RALOnOpenInputStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream := FZipStream;
end;

procedure TRALUnZipper.RALOnCreateStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  vFileName : string;
  vPos : integer;
begin
  AStream := nil;
  vFileName := AItem.ArchiveFileName;
  if FFolder <> '' then
  begin
    vPos := Pos('/', vFileName);
    vFileName := FFolder + Copy(vFileName, vPos, Length(vFileName));
  end;

  vFileName := IncludeTrailingPathDelimiter(OutputPath) + vFileName;
  if not AItem.IsDirectory then begin
    AStream := TFileStream.Create(vFileName, fmCreate);
  end
  else begin
    ForceDirectories(vFileName);
  end;
end;

constructor TRALUnZipper.Create;
begin
  inherited;
  Self.OnOpenInputStream := @RALOnOpenInputStream;
  Self.OnCreateStream := @RALOnCreateStream;
end;

end.

