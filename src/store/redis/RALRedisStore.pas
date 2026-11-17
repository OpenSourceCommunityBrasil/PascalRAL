{ TRALRedisStore: the key-value store of the plugins and modules (sessions,
  rate limits, revoked tokens, cached answers) kept in Redis or Valkey, so
  several servers, on one machine or many, share it. }
unit RALRedisStore;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALConsts, RALTools, RALKeyValueStore, RALRedisClient;

type
  { A TRALKeyValueStore in the server of Client, every key under KeyPrefix.
    Each operation is one command or one Lua script, so it is atomic for every
    server sharing the database. The times to live are counted by the Redis
    server: Clock changes only what Current answers }
  TRALRedisStore = class(TRALKeyValueStore)
  private
    FClient: TRALRedisClient;
    FKeyPrefix: StringRAL;

    /// Client, or raises when none is linked.
    function ActiveClient: TRALRedisClient;
    /// The Redis key of AKey.
    function KeyOf(const AKey: StringRAL): RawByteString;
    procedure SetClient(AValue: TRALRedisClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function NowMs: Int64RAL; override;
  public
    constructor Create(AOwner: TComponent); override;

    { Forgets every key under KeyPrefix (with an empty KeyPrefix, every key of
      the database) }
    procedure Clear; override;
    function Delete(const AKey: StringRAL): boolean; override;
    function Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean; override;
    function Get(const AKey: StringRAL; out AValue: RawByteString): boolean; override;
    function Increment(const AKey: StringRAL; ADelta: Int64RAL;
      ATTL: Int64RAL = 0): Int64RAL; override;
    procedure Put(const AKey: StringRAL; const AValue: RawByteString;
      ATTL: Int64RAL = 0); override;
    function TimeToLive(const AKey: StringRAL): Int64RAL; override;
  published
    /// The Redis server, its database and its settings.
    property Client: TRALRedisClient read FClient write SetClient;
    { Put before every key, so applications sharing a database keep apart;
      'ral:' by default }
    property KeyPrefix: StringRAL read FKeyPrefix write FKeyPrefix;
  end;

implementation

const
  { INCRBY that gives a new key ATTL and leaves the TTL of an existing one:
    KEYS[1] the key, ARGV[1] the delta, ARGV[2] the TTL in ms }
  cIncrementScript =
    'local e = redis.call(''EXISTS'', KEYS[1]) ' +
    'local v = redis.call(''INCRBY'', KEYS[1], ARGV[1]) ' +
    'if e == 0 and tonumber(ARGV[2]) > 0 then redis.call(''PEXPIRE'', KEYS[1], ARGV[2]) end ' +
    'return v';
  /// The TTL of an existing key: PEXPIRE above 0, PERSIST at 0; 0 when missing.
  cExpireScript =
    'if redis.call(''EXISTS'', KEYS[1]) == 0 then return 0 end ' +
    'if tonumber(ARGV[1]) > 0 then redis.call(''PEXPIRE'', KEYS[1], ARGV[1]) ' +
    'else redis.call(''PERSIST'', KEYS[1]) end ' +
    'return 1';

{ TRALRedisStore }

constructor TRALRedisStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyPrefix := 'ral:';
end;

function TRALRedisStore.ActiveClient: TRALRedisClient;
begin
  if FClient = nil then
    raise ERALRedisError.Create(emRedisNoClient);
  Result := FClient;
end;

procedure TRALRedisStore.Clear;
var
  vKeys: TStringList;
  vArgs: array of RawByteString;
  vInt, vStart, vCount: IntegerRAL;
begin
  vKeys := TStringList.Create;
  try
    ActiveClient.Scan(RALRedisEscapeGlob(RawByteString(FKeyPrefix)) + '*', vKeys);
    // in batches, so one command never carries too many keys
    vStart := 0;
    while vStart < vKeys.Count do
    begin
      vCount := vKeys.Count - vStart;
      if vCount > 500 then
        vCount := 500;
      SetLength(vArgs, vCount + 1);
      vArgs[0] := 'DEL';
      for vInt := 0 to vCount - 1 do
        vArgs[vInt + 1] := RawByteString(vKeys[vStart + vInt]);
      ActiveClient.CommandInt(vArgs);
      Inc(vStart, vCount);
    end;
  finally
    vKeys.Free;
  end;
end;

function TRALRedisStore.Delete(const AKey: StringRAL): boolean;
begin
  Result := ActiveClient.CommandInt(['DEL', KeyOf(AKey)]) > 0;
end;

function TRALRedisStore.Expire(const AKey: StringRAL; ATTL: Int64RAL): boolean;
var
  vReply: TRALRedisReply;
begin
  vReply := ActiveClient.Eval(cExpireScript, [KeyOf(AKey)], [RawByteString(IntToStr(ATTL))]);
  try
    Result := vReply.AsInteger = 1;
  finally
    vReply.Free;
  end;
end;

function TRALRedisStore.Get(const AKey: StringRAL; out AValue: RawByteString): boolean;
begin
  Result := ActiveClient.GetValue(KeyOf(AKey), AValue);
end;

function TRALRedisStore.Increment(const AKey: StringRAL; ADelta: Int64RAL;
  ATTL: Int64RAL): Int64RAL;
var
  vReply: TRALRedisReply;
begin
  vReply := ActiveClient.Eval(cIncrementScript, [KeyOf(AKey)],
    [RawByteString(IntToStr(ADelta)), RawByteString(IntToStr(ATTL))]);
  try
    Result := vReply.AsInteger;
  finally
    vReply.Free;
  end;
end;

function TRALRedisStore.KeyOf(const AKey: StringRAL): RawByteString;
begin
  Result := RawByteString(FKeyPrefix) + RawByteString(AKey);
end;

procedure TRALRedisStore.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

function TRALRedisStore.NowMs: Int64RAL;
begin
  // the time of day in UTC, as every server sharing the database sees it
  Result := MilliSecondsBetween(RALDateTimeToGMT(Now), UnixDateDelta);
end;

procedure TRALRedisStore.Put(const AKey: StringRAL; const AValue: RawByteString;
  ATTL: Int64RAL);
begin
  ActiveClient.SetValue(KeyOf(AKey), AValue, ATTL);
end;

procedure TRALRedisStore.SetClient(AValue: TRALRedisClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

function TRALRedisStore.TimeToLive(const AKey: StringRAL): Int64RAL;
begin
  Result := ActiveClient.TimeToLive(KeyOf(AKey));
end;

end.
