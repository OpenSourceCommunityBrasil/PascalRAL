unit RALDBBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  RALTypes;

type
  TRALDatabaseType = (dtFirebird, dtSQLite, dtMySQL, dtPostgreSQL);
  TRALDatabaseOutPut = (dopJSON, dopBinary);

  { TRALDBBase }

  TRALDBBase = class(TPersistent)
  private
    FDatabase : StringRAL;
    FHostname : StringRAL;
    FUsername : StringRAL;
    FPassword : StringRAL;
    FPort     : IntegerRAL;

    FDatabaseType : TRALDatabaseType;
    FDatabaseOutPut : TRALDatabaseOutPut;
  protected
    procedure Conectar; virtual; abstract;
  public
    constructor Create; virtual;

    function Open(ASQL : StringRAL; AParams : TParams) : TDataset; virtual; abstract;
    procedure ExecSQL(ASQL : StringRAL; AParams : TParams; var ARowsAffected : Int64RAL;
                      var ALastInsertId : Int64RAL); virtual; abstract;
  published
    property Database : StringRAL read FDatabase write FDatabase;
    property Hostname : StringRAL read FHostname write FHostname;
    property Username : StringRAL read FUsername write FUsername;
    property Password : StringRAL read FPassword write FPassword;
    property Port     : IntegerRAL read FPort write FPort;

    property DatabaseType   : TRALDatabaseType read FDatabaseType write FDatabaseType;
    property DatabaseOutPut : TRALDatabaseOutPut read FDatabaseOutPut write FDatabaseOutPut;
  end;

  TRALDBClass = class of TRALDBBase;

implementation

{ TRALDBBase }

constructor TRALDBBase.Create;
begin
  inherited;
end;

end.

