/// Base unit for Database related components
unit RALDBBase;

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALCustomObjects, RALDBTypes, RALStorage, RALRequest,
  RALResponse;

type
  TRALDatabaseType = (dtFirebird, dtSQLite, dtMySQL, dtPostgreSQL);

  // sempre jogar a qtOther pra frente, ela tem valor igual a 255
  TRALDBDriverType = (qtFiredac, qtZeos, qtLazSQL, qtOther = 255);

  TRALDBOnConnect = procedure(ASender : TObject; ARequest : TRALRequest) of object;
  TRALDBOnError = procedure(ASender : TObject; AException : StringRAL; ARequest : TRALRequest) of object;

  { TRALDBBase }

  TRALDBBase = class(TPersistent)
  private
    FDatabase: StringRAL;
    FDatabaseType: TRALDatabaseType;
    FHostname: StringRAL;
    FUsername: StringRAL;
    FPassword: StringRAL;
    FPort: IntegerRAL;
    FRequest: TRALRequest;
    FResponse: TRALResponse;

    FOnBeforeConnect : TRALDBOnConnect;
    FOnAfterConnect : TRALDBOnConnect;
    FOnErrorConnect : TRALDBOnError;
    FOnErrorQuery : TRALDBOnError;
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

    class function DatabaseName : StringRAL; virtual; abstract;
    class function PackageDependency : StringRAL; virtual; abstract;
  published
    property Database: StringRAL read FDatabase write FDatabase;
    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    property DriverType: TRALDBDriverType read GetDriverType;
    property Hostname: StringRAL read FHostname write FHostname;
    property Username: StringRAL read FUsername write FUsername;
    property Password: StringRAL read FPassword write FPassword;
    property Port: IntegerRAL read FPort write FPort;
    property Request: TRALRequest read FRequest write FRequest;
    property Response: TRALResponse read FResponse write FResponse;

    property OnBeforeConnect: TRALDBOnConnect read FOnBeforeConnect write FOnBeforeConnect;
    property OnAfterConnect: TRALDBOnConnect read FOnAfterConnect write FOnAfterConnect;
    property OnErrorConnect: TRALDBOnError read FOnErrorConnect write FOnErrorConnect;
    property OnErrorQuery: TRALDBOnError read FOnErrorQuery write FOnErrorQuery;
  end;

  TRALDBClass = class of TRALDBBase;

  procedure RegisterDatabase(ADatabase: TRALDBClass);
  procedure UnregisterDatabase(ADatabase: TRALDBClass);
  function GetDatabaseClass(ADatabaseName: StringRAL): TRALDBClass;
  procedure GetDatabaseList(AList: TStrings);

implementation

var
  DatabasesDefs : TStringList;

procedure CheckDatabaseDefs;
begin
  if DatabasesDefs = nil then
  begin
    DatabasesDefs := TStringList.Create;
    DatabasesDefs.Sorted := True;
  end;
end;

procedure DoneEngineDefs;
begin
  FreeAndNil(DatabasesDefs);
end;

procedure RegisterDatabase(ADatabase: TRALDBClass);
begin
  CheckDatabaseDefs;
  if DatabasesDefs.IndexOfName(ADatabase.DatabaseName) < 0 then
    DatabasesDefs.Add(ADatabase.DatabaseName + '=' + ADatabase.ClassName);
end;

procedure UnregisterDatabase(ADatabase: TRALDBClass);
var
  vPos : IntegerRAL;
begin
  CheckDatabaseDefs;
  vPos := DatabasesDefs.IndexOfName(ADatabase.DatabaseName);
  if vPos >= 0 then
    DatabasesDefs.Delete(vPos);
end;

function GetDatabaseClass(ADatabaseName: StringRAL): TRALDBClass;
var
  vPos : IntegerRAL;
begin
  Result := nil;
  CheckDatabaseDefs;
  vPos := DatabasesDefs.IndexOfName(ADatabaseName);
  if vPos >= 0 then
    Result := TRALDBClass(GetClass(DatabasesDefs.ValueFromIndex[vPos]));
end;

procedure GetDatabaseList(AList: TStrings);
var
  vInt : IntegerRAL;
begin
  CheckDatabaseDefs;
  for vInt := 0 to Pred(DatabasesDefs.Count) do
    AList.Add(DatabasesDefs.Names[vInt]);
end;

{ TRALDBBase }

function TRALDBBase.CanExportNative: boolean;
begin
  Result := False;
end;

end.
