/// Base unit for Database related components
unit RALDBBase;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects, RALDBTypes, RALDBStorage;

type
  TRALDatabaseType = (dtFirebird, dtSQLite, dtMySQL, dtPostgreSQL);

  // sempre jogar a qtOther pra frente, ela tem valor igual a 255
  TRALDBDriverType = (qtFiredac, qtZeos, qtLazSQL, qtOther = 255);

  { TRALDBBase }

  TRALDBBase = class(TPersistent)
  private
    FDatabase: StringRAL;
    FDatabaseType: TRALDatabaseType;
    FHostname: StringRAL;
    FUsername: StringRAL;
    FPassword: StringRAL;
    FPort: IntegerRAL;
  protected
    procedure Conectar; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    function CanExportNative: boolean; virtual;
    procedure ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                      var ALastInsertId: Int64RAL); virtual; abstract;
    function GetDriverType: TRALDBDriverType; virtual; abstract;
    function GetFieldTable(ADataset: TDataSet; AFieldIndex: IntegerRAL) : StringRAL; virtual; abstract;
    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; virtual; abstract;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; virtual; abstract;
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream;
                           var AContentType: StringRAL;
                           var ANative : boolean); virtual; abstract;
  published
    property Database: StringRAL read FDatabase write FDatabase;
    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    property DriverType: TRALDBDriverType read GetDriverType;
    property Hostname: StringRAL read FHostname write FHostname;
    property Username: StringRAL read FUsername write FUsername;
    property Password: StringRAL read FPassword write FPassword;
    property Port: IntegerRAL read FPort write FPort;
  end;

  TRALDBClass = class of TRALDBBase;

  TRALDBLink = class(TRALComponent)
  public
    function GetDBClass: TRALDBClass; virtual; abstract;
  end;

implementation

{ TRALDBBase }

function TRALDBBase.CanExportNative: boolean;
begin
  Result := False;
end;

end.
