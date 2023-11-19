unit RALDBBase;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects, RALStorage;

type
  TRALDatabaseType = (dtFirebird, dtSQLite, dtMySQL, dtPostgreSQL);

  { TRALDBBase }

  TRALDBBase = class(TPersistent)
  private
    FDatabase : StringRAL;
    FHostname : StringRAL;
    FUsername : StringRAL;
    FPassword : StringRAL;
    FPort     : IntegerRAL;

    FDatabaseType : TRALDatabaseType;
    FStorageOutPut : TRALStorageLink;
  protected
    procedure Conectar; virtual; abstract;
  public
    constructor Create; virtual; abstract;

    function Open(ASQL : StringRAL; AParams : TParams) : TDataset; virtual; abstract;
    procedure ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                      var ALastInsertId : Int64RAL); virtual; abstract;
  published
    property Database : StringRAL read FDatabase write FDatabase;
    property Hostname : StringRAL read FHostname write FHostname;
    property Username : StringRAL read FUsername write FUsername;
    property Password : StringRAL read FPassword write FPassword;
    property Port     : IntegerRAL read FPort write FPort;

    property DatabaseType  : TRALDatabaseType read FDatabaseType write FDatabaseType;
    property StorageOutPut : TRALStorageLink read FStorageOutPut write FStorageOutPut;
  end;

  TRALDBClass = class of TRALDBBase;

  TRALDBLink = class(TRALComponent)
  public
    function GetDBClass : TRALDBClass; virtual; abstract;
  end;

implementation

end.

