unit RALToken;

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALSHA2_32, RALSHA2_64, RALHashes, RALBase64, RALKeyPairs,
  {$IFNDEF FPC}
    JSON
  {$ELSE}
    fpjson
  {$ENDIF}
  ;

type
  TRALJWTAlgorithm = (tjaHSHA256, tjaHSHA512);

  TRALJWTHeader = class(TPersistent)
  private
    FHeaderType : StringRAL;
    FAlgorithm : TRALJWTAlgorithm;
    FKeyID : StringRAL;
  protected
    function GetAsJSON: StringRAL;
    procedure SetAsJSON(const AValue: StringRAL);
    procedure Initialize;
  public
    constructor Create;
  published
    property HeaderType : StringRAL read FHeaderType;
    property Algorithm : TRALJWTAlgorithm read FAlgorithm write FAlgorithm;
    property KeyID : StringRAL read FKeyID write FKeyID;

    property AsJSON : StringRAL read GetAsJSON write SetAsJSON;
  end;

  TRALJWTPayload = class(TPersistent)
  private
    FAudience : StringRAL;
    FExpiration : TDateTime;
    FIssuedAt : TDateTime;
    FIssuer : StringRAL;
    FJWTId : StringRAL;
    FNotBefore : TDateTime;
    FSubject : StringRAL;
    FCustoms : TRALKeyPairs;
  protected
    function GetAsJSON: StringRAL;
    procedure SetAsJSON(const AValue: StringRAL);
    procedure Initialize;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Audience : StringRAL read FAudience write FAudience;
    property Expiration : TDateTime read FExpiration write FExpiration;
    property IssuedAt : TDateTime read FIssuedAt write FIssuedAt;
    property Issuer : StringRAL read FIssuer write FIssuer;
    property JWTId : StringRAL read FJWTId write FJWTId;
    property NotBefore : TDateTime read FNotBefore write FNotBefore;
    property Subject : StringRAL read FSubject write FSubject;
    property Customs : TRALKeyPairs read FCustoms write FCustoms;

    property AsJSON : StringRAL read GetAsJSON write SetAsJSON;
  end;

  TRALJWT = class
  private
    FAlgorithm : TRALJWTAlgorithm;
    FHeader: TRALJWTHeader;
    FPayload: TRALJWTPayload;
    FSignature: StringRAL;
    FSecret : StringRAL;
  protected
    constructor Create;
    destructor Destroy; override;

    function CreateToken(AHeader,APayload : StringRAL;
                         var ASignature : StringRAL) : StringRAL;
    function GetToken: StringRAL;
  public
    function ValidToken(const AValue: StringRAL) : boolean;

    property Token: StringRAL read GetToken;
  published
    property Header: TRALJWTHeader read FHeader write FHeader;
    property Payload: TRALJWTPayload read FPayload write FPayload;
    property Signature: StringRAL read FSignature;
    property Secret: StringRAL read FSecret write FSecret;
  end;

implementation

{ TRALJWTHeader }

constructor TRALJWTHeader.Create;
begin
  Initialize;
end;

function TRALJWTHeader.GetAsJSON: StringRAL;
var
  vJson : TJSONObject;
begin
  vJson := TJSONObject.Create;
  try
    vJson.AddPair('typ',FHeaderType);

    case FAlgorithm of
      tjaHSHA256: vJson.AddPair('alg','hs256');
      tjaHSHA512: vJson.AddPair('alg','hs512');
    end;

    if FKeyID <> '' then
      vJson.AddPair('kid',FKeyID);

    Result := vJson.ToJSON;
  finally
    FreeAndNil(vJson);
  end;
end;

procedure TRALJWTHeader.Initialize;
begin
  FHeaderType := 'jwt';
  FAlgorithm := tjaHSHA256;
  FKeyID := '';
end;

procedure TRALJWTHeader.SetAsJSON(const AValue: StringRAL);
var
  vJson : TJSONObject;
  vPair : TJSONPair;
  vInt : IntegerRAL;
  vPairName, vAux1 : StringRAL;
begin
  vJson := TJSONObject.ParseJSONValue(AValue) as TJSONObject;
  try
    if vJson <> nil then begin
      Initialize;
      vInt := 0;
      while vInt < vJson.Count do begin
        vPair := vJson.Pairs[vInt];
        vPairName := vPair.JsonString.Value;
        if SameText(vPairName,'typ') then begin
          FHeaderType := vPair.JsonValue.Value;
        end
        else if SameText(vPairName,'alg') then begin
          vAux1 := vPair.JsonValue.Value;

          FAlgorithm := tjaHSHA256;
          if SameText(vAux1,'hs256') then
            FAlgorithm := tjaHSHA256
          else if SameText(vAux1,'hs512') then
            FAlgorithm := tjaHSHA512;
        end
        else if SameText(vPairName,'kid') then begin
          FKeyID := vPair.JsonValue.Value;
        end;

        vInt := vInt + 1;
      end;
    end;
  finally
    FreeAndNil(vJson);
  end;
end;

{ TRALJWTPayload }

constructor TRALJWTPayload.Create;
begin
  FCustoms := TRALKeyPairs.Create(Self);
  Initialize;
end;

destructor TRALJWTPayload.Destroy;
begin
  FreeAndNil(FCustoms);
  inherited;
end;

function TRALJWTPayload.GetAsJSON: StringRAL;
var
  vJson : TJSONObject;
  vJsonValue : TJSONValue;
  vInt : IntegerRAL;
  vItem : TRALKeyPair;
  vInvalidName : boolean;
begin
  vJson := TJSONObject.Create;
  try
    if FAudience <> '' then
      vJson.AddPair('aud',FAudience);

    if FExpiration > 0 then
      vJson.AddPair('exp',TJSONNumber.Create(DateTimeToUnix(FExpiration)));

    if FIssuedAt > 0 then
      vJson.AddPair('iat',TJSONNumber.Create(DateTimeToUnix(FIssuedAt)));

    if FIssuer <> '' then
      vJson.AddPair('iss',FIssuer);

    if FJWTId <> '' then
      vJson.AddPair('jti',FJWTId);

    if FNotBefore > 0 then
      vJson.AddPair('nbf',TJSONNumber.Create(DateTimeToUnix(FNotBefore)));

    if FSubject <> '' then
      vJson.AddPair('sub',FSubject);

    vInt := 0;
    while vInt < FCustoms.Count do begin
      vItem := TRALKeyPair(FCustoms.Items[vInt]);
      vInvalidName := SameText(vItem.KeyName,'aud') or
                      SameText(vItem.KeyName,'exp') or
                      SameText(vItem.KeyName,'iat') or
                      SameText(vItem.KeyName,'iss') or
                      SameText(vItem.KeyName,'jti') or
                      SameText(vItem.KeyName,'nbf') or
                      SameText(vItem.KeyName,'sub');

      if not vInvalidName then begin
        if vItem.KeyType = ktString then begin
          vJson.AddPair(vItem.KeyName,vItem.KeyValue);
        end
        else if vItem.KeyType = ktInteger then begin
          vJsonValue := TJSONNumber.Create(StrToInt64(vItem.KeyValue));
          vJson.AddPair(vItem.KeyName,vJsonValue);
        end
        else if vItem.KeyType = ktFloat then begin
          vJsonValue := TJSONNumber.Create(StrToFloat(vItem.KeyValue));
          vJson.AddPair(vItem.KeyName,vJsonValue);
        end
        else if vItem.KeyType in [ktDate,ktTime,ktDateTime] then begin
          vJsonValue := TJSONNumber.Create(DateTimeToUnix(StrToDateTime(vItem.KeyValue)));
          vJson.AddPair(vItem.KeyName,vJsonValue);
        end
        else if vItem.KeyType = ktBoolean then begin
          if SameText(vItem.KeyValue,'true') then
            vJsonValue := TJSONBool.Create(True)
          else
            vJsonValue := TJSONBool.Create(False);
          vJson.AddPair(vItem.KeyName,vJsonValue);
        end
        else begin
          vJson.AddPair(vItem.KeyName,TJSONNull.Create);
        end;
      end;

      vInt := vInt + 1;
    end;

    Result := vJson.ToJSON;
  finally
    FreeAndNil(vJson);
  end;
end;

procedure TRALJWTPayload.Initialize;
begin
  FAudience := '';
  FExpiration := 0;
  FIssuedAt := 0;
  FIssuer := '';
  FJWTId := '';
  FNotBefore := 0;
  FSubject := '';
  FCustoms.Clear;
end;

procedure TRALJWTPayload.SetAsJSON(const AValue: StringRAL);
var
  vJson : TJSONObject;
  vPair : TJSONPair;
  vInt : IntegerRAL;
  vInt64 : Int64RAL;
  vPairName, vAux1 : StringRAL;
begin
  vJson := TJSONObject.ParseJSONValue(AValue) as TJSONObject;
  try
    if vJson <> nil then begin
      Initialize;
      vInt := 0;
      while vInt < vJson.Count do begin
        vPair := vJson.Pairs[vInt];
        vPairName := vPair.JsonString.Value;
        if SameText(vPairName,'aud') then begin
          FAudience := vPair.JsonValue.Value;
        end
        else if SameText(vPairName,'exp') then begin
          if vPair.JsonValue is TJSONNumber then
            FExpiration := UnixToDateTime(TJSONNumber(vPair.JsonValue).AsInt64)
          else
            FExpiration := StrToDateTimeDef(vPair.JsonValue.Value,0);
        end
        else if SameText(vPairName,'iat') then begin
          if vPair.JsonValue is TJSONNumber then
            FIssuedAt := UnixToDateTime(TJSONNumber(vPair.JsonValue).AsInt64)
          else
            FIssuedAt := StrToDateTimeDef(vPair.JsonValue.Value,0);
        end
        else if SameText(vPairName,'iss') then begin
          FIssuer := vPair.JsonValue.Value;
        end
        else if SameText(vPairName,'jti') then begin
          FJWTId := vPair.JsonValue.Value;
        end
        else if SameText(vPairName,'nbf') then begin
          if vPair.JsonValue is TJSONNumber then
            FNotBefore := UnixToDateTime(TJSONNumber(vPair.JsonValue).AsInt64)
          else
            FNotBefore := StrToDateTimeDef(vPair.JsonValue.Value,0);
        end
        else if SameText(vPairName,'sub') then begin
          FSubject := vPair.JsonValue.Value;
        end
        else begin
          if vPair.JsonValue is TJSONNumber then begin
            if not TryStrToInt64(vPair.JsonValue.Value,vInt64) then
              FCustoms.AddKey(vPairName,TJSONNumber(vPair.JsonValue).AsDouble)
            else
              FCustoms.AddKey(vPairName,TJSONNumber(vPair.JsonValue).AsInt64)
          end
          else if vPair.JsonValue is TJSONBool then begin
            FCustoms.AddKey(vPairName,TJSONBool(vPair.JsonValue).AsBoolean)
          end
          else begin
            FCustoms.AddKey(vPairName,vPair.JsonValue.Value)
          end;
        end;

        vInt := vInt + 1;
      end;
    end;
  finally
    FreeAndNil(vJson);
  end;
end;

{ TRALJWT }

constructor TRALJWT.Create;
begin
  inherited;
  FHeader := TRALJWTHeader.Create;
  FPayload := TRALJWTPayload.Create;
end;

function TRALJWT.CreateToken(AHeader, APayload: StringRAL;
                             var ASignature : StringRAL): StringRAL;
var
  vHash : TRALHashes;
begin
  case FAlgorithm of
    tjaHSHA256: begin
      vHash := TRALSHA2_32.Create;
      TRALSHA2_32(vHash).Version := rsv256;
    end;
    tjaHSHA512: begin
      vHash := TRALSHA2_64.Create;
      TRALSHA2_64(vHash).Version := rsv512;
    end;
  end;

  try
    vHash.OutputType := rhotBase64;

    Result := TRALBase64.Encode(AHeader) + '.' +
              TRALBase64.Encode(APayload);

    ASignature := vHash.HMACAsString(Result,FSecret);
    Result := Result + '.' + FSignature;
  finally
    FreeAndNil(vHash);
  end;
end;

destructor TRALJWT.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FPayload);
  inherited;
end;

function TRALJWT.GetToken: StringRAL;
begin
  Result := CreateToken(FHeader.AsJSON,FPayload.AsJSON,FSignature);
end;

function TRALJWT.ValidToken(const AValue: StringRAL) : boolean;
var
  vStr : TStringList;
  vInt : IntegerRAL;
  vValue : StringRAL;

  vHeader : StringRAL;
  vPayload : StringRAL;
  vSignature, vMySignature : StringRAL;

  vObjPayload : TRALJWTPayload;
begin
  Result := False;

  vValue := AValue;
  vStr := TStringList.Create;
  try
    repeat
      vInt := Pos('.',vValue);
      if vInt > 0 then begin
        vStr.Add(Copy(vValue,1,vInt-1));
        Delete(vValue,1,vInt);
      end;
    until vInt = 0;

    if vStr.Count = 3 then begin
      vHeader    := TRALBase64.Decode(vStr.Strings[0]);
      vPayload   := TRALBase64.Decode(vStr.Strings[1]);
      vSignature := vStr.Strings[2];

      CreateToken(vHeader,vPayload,vMySignature);

      if vMySignature = vSignature then begin
        vObjPayload := TRALJWTPayload.Create;
        try
          vObjPayload.AsJSON := vPayload;
          if (vObjPayload.Expiration > 0) and (vObjPayload.Expiration < Now) then
            Result := False
          else if (vObjPayload.NotBefore > 0) and (vObjPayload.NotBefore > Now) then
            Result := False
          else if (vObjPayload.NotBefore > 0) and (vObjPayload.Expiration > 0) and
                  (vObjPayload.Expiration < vObjPayload.NotBefore) then
            Result := False;
        finally
          vObjPayload.Free;
        end;
      end;
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

end.
