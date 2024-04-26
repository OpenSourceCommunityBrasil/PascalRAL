unit RALDBStorageJSON;

interface

uses
  Classes, SysUtils, DB, DateUtils,
  RALTypes, RALDBStorage, RALBase64, RALStream, RALMIMETypes, RALDBTypes;

type
  TRALJSONFormat = (jfDBWare, jfCommon);

  TRALDBStorageJSON = class(TRALDBStorage)
  private
    FFields : Boolean;
    FRecords : Boolean;
    FJSONFormat : TRALJSONFormat;
  protected

  published
    property JSONFormat : TRALJSONFormat read FJSONFormat write FJSONFormat;
  end;

  { TRALDBStorageJSONLink }

  TRALDBStorageJSONLink = class(TRALDBStorageLink)
  private
    FJSONFormat : TRALJSONFormat;
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetStorage : TRALDBStorage; override;
  published
    property JSONFormat : TRALJSONFormat read FJSONFormat write FJSONFormat;
  end;

implementation

{ TRALDBStorageJSON }

{ TRALDBStorageJSONLink }

function TRALDBStorageJSONLink.GetContentType: StringRAL;
begin
  Result:=inherited GetContentType;
end;

constructor TRALDBStorageJSONLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TRALDBStorageJSONLink.GetStorage: TRALDBStorage;
begin
  Result:=inherited GetStorage;
end;

initialization
  RegisterClass(TRALDBStorageJSONLink);

end.
