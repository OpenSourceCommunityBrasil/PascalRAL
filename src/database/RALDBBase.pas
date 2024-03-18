unit RALDBBase;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects, RALDBStorage;

type
  TRALDatabaseType = (dtFirebird, dtSQLite, dtMySQL, dtPostgreSQL);
  TRALFormatStorage = (fsJSON, fsBIN);

  // sempre jogar a qtOther pra frente, ela tem valor igual a 255
  TRALDBDriverType = (qtFiredac, qtZeos, qtLazSQL, qtOther = 255);

  { TRALDBBase }

  TRALDBBase = class(TPersistent)
  private
    FDatabase: StringRAL;
    FHostname: StringRAL;
    FUsername: StringRAL;
    FPassword: StringRAL;
    FPort: IntegerRAL;

    FDatabaseType: TRALDatabaseType;
    FStorageOutPut: TRALDBStorageLink;
  protected
    procedure Conectar; virtual; abstract;
  public
    constructor Create; virtual; abstract;

    function OpenNative(ASQL: StringRAL; AParams: TParams): TDataset; virtual; abstract;
    function OpenCompatible(ASQL: StringRAL; AParams: TParams): TDataset; virtual; abstract;
    procedure ExecSQL(ASQL: StringRAL; AParams: TParams; var ARowsAffected: Int64RAL;
                      var ALastInsertId: Int64RAL); virtual; abstract;
    function GetDriverName: TRALDBDriverType; virtual; abstract;

    procedure SaveFromStream(ADataset: TDataSet; AStream: TStream;
                             AFormat: TRALFormatStorage); virtual; abstract;
  published
    property Database: StringRAL read FDatabase write FDatabase;
    property Hostname: StringRAL read FHostname write FHostname;
    property Username: StringRAL read FUsername write FUsername;
    property Password: StringRAL read FPassword write FPassword;
    property Port: IntegerRAL read FPort write FPort;

    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    property StorageOutPut: TRALDBStorageLink read FStorageOutPut write FStorageOutPut;
    property DriverName : TRALDBDriverType read GetDriverName;
  end;

  TRALDBClass = class of TRALDBBase;

  TRALDBLink = class(TRALComponent)
  public
    function GetDBClass: TRALDBClass; virtual; abstract;
  end;

implementation

{ TRALDBBase }

end.
