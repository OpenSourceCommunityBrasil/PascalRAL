unit RALToken;

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALSHA2_32, RALSHA2_64, RALHashes, RALBase64, RALKeyPairs,
  RALJson, RALTools;

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

    procedure createKeyID;
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

    procedure createJWTId;
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
    function CreateToken(AHeader,APayload : StringRAL;
                         var ASignature : StringRAL) : StringRAL;
    function GetToken: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;
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

procedure TRALJWTHeader.createKeyID;
var
  vBytes : TBytes;
begin
  vBytes := randomBytes(8);
  FKeyID := TRALBase64.Encode(vBytes);
end;

function TRALJWTHeader.GetAsJSON: StringRAL;
var
  vJson : TRALJSONObject;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('typ',FHeaderType);

    case FAlgorithm of
      tjaHSHA256: vJson.Add('alg', 'hs256');
      tjaHSHA512: vJson.Add('alg', 'hs512');
    end;

    if FKeyID <> '' then
      vJson.Add('kid', FKeyID);

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
  vJson : TRALJSONObject;
  vInt : IntegerRAL;
  vName : StringRAL;
  vValue : TRALJSONValue;
  vPairName, vAux1 : StringRAL;
begin
  vJson := TRALJSONObject(ParseJson(AValue));
  try
    if vJson <> nil then
    begin
      Initialize;
      vInt := 0;
      while vInt < vJson.Count do
      begin
        vName := vJson.Names[vInt];
        vValue := vJson.Get(vInt);
        if SameText(vName, 'typ') then
        begin
          FHeaderType := vValue.ToJSON;
        end
        else if SameText(vName, 'alg') then
        begin
          vAux1 := vValue.ToJSON;

          FAlgorithm := tjaHSHA256;
          if SameText(vAux1, 'hs256') then
            FAlgorithm := tjaHSHA256
          else if SameText(vAux1, 'hs512') then
            FAlgorithm := tjaHSHA512;
        end
        else if SameText(vName, 'kid') then
        begin
          FKeyID := vValue.ToJSON;
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

procedure TRALJWTPayload.createJWTId;
var
  vBytes : TBytes;
begin
  vBytes := randomBytes(10);
  FJWTId := TRALBase64.Encode(vBytes);
end;

destructor TRALJWTPayload.Destroy;
begin
  FreeAndNil(FCustoms);
  inherited;
end;

function TRALJWTPayload.GetAsJSON: StringRAL;
var
  vJson : TRALJSONObject;
  vInt : IntegerRAL;
  vItem : TRALKeyPair;
  vInvalidName : boolean;
begin
  vJson := TRALJSONObject.Create;
  try
    if FAudience <> '' then
      vJson.Add('aud', FAudience);

    if FExpiration > 0 then
      vJson.Add('exp', DateTimeToUnix(FExpiration));

    if FIssuedAt > 0 then
      vJson.Add('iat', DateTimeToUnix(FIssuedAt));

    if FIssuer <> '' then
      vJson.Add('iss', FIssuer);

    if FJWTId <> '' then
      vJson.Add('jti', FJWTId);

    if FNotBefore > 0 then
      vJson.Add('nbf', DateTimeToUnix(FNotBefore));

    if FSubject <> '' then
      vJson.Add('sub', FSubject);

    vInt := 0;
    while vInt < FCustoms.Count do begin
      vItem := TRALKeyPair(FCustoms.Items[vInt]);
      vInvalidName := SameText(vItem.KeyName, 'aud') or
                      SameText(vItem.KeyName, 'exp') or
                      SameText(vItem.KeyName, 'iat') or
                      SameText(vItem.KeyName, 'iss') or
                      SameText(vItem.KeyName, 'jti') or
                      SameText(vItem.KeyName, 'nbf') or
                      SameText(vItem.KeyName, 'sub');

      if not vInvalidName then begin
        if vItem.KeyType = ktString then begin
          vJson.Add(vItem.KeyName, vItem.KeyValue);
        end
        else if vItem.KeyType = ktInteger then begin
          vJson.Add(vItem.KeyName, StrToInt64(vItem.KeyValue));
        end
        else if vItem.KeyType = ktFloat then begin
          vJson.Add(vItem.KeyName, StrToFloat(vItem.KeyValue));
        end
        else if vItem.KeyType in [ktDate,ktTime,ktDateTime] then begin
          vJson.Add(vItem.KeyName, DateTimeToUnix(StrToDateTime(vItem.KeyValue)));
        end
        else if vItem.KeyType = ktBoolean then begin
          vJson.Add(vItem.KeyName, SameText(vItem.KeyValue, 'true'));
        end
        else begin
          vJson.Add(vItem.KeyName);
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
  vJson : TRALJSONObject;
  vInt : IntegerRAL;
  vInt64 : Int64RAL;
  vName : StringRAL;
  vValue : TRALJSONValue;
begin
  vJson := TRALJSONObject(ParseJson(AValue));
  try
    if vJson <> nil then
    begin
      Initialize;
      vInt := 0;
      while vInt < vJson.Count do
      begin
        vName := vJson.Names[vInt];
        vValue := vJson.Get(vInt);
        if SameText(vName, 'aud') then
        begin
          FAudience := vValue.ToJSON;
        end
        else if SameText(vName, 'exp') then
        begin
          if vValue is TRALJSONNumber then
            FExpiration := UnixToDateTime(vValue.AsInt64)
          else
            FExpiration := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if SameText(vName, 'iat') then
        begin
          if vValue is TRALJSONNumber then
            FIssuedAt := UnixToDateTime(vValue.AsInt64)
          else
            FIssuedAt := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if SameText(vName, 'iss') then
        begin
          FIssuer := vValue.AsString;
        end
        else if SameText(vName, 'jti') then
        begin
          FJWTId := vValue.AsString;
        end
        else if SameText(vName, 'nbf') then
        begin
          if vValue is TRALJSONNumber then
            FNotBefore := UnixToDateTime(vValue.AsInt64)
          else
            FNotBefore := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if SameText(vName, 'sub') then
        begin
          FSubject := vValue.AsString;
        end
        else begin
          if vValue is TRALJSONNumber then
          begin
            if not TryStrToInt64(vValue.AsString, vInt64) then
              FCustoms.AddKey(vName, vValue.AsFloat)
            else
              FCustoms.AddKey(vName, vValue.AsInt64)
          end
          else if vValue is TRALJSONBoolean then
          begin
            FCustoms.AddKey(vName, vValue.AsBoolean)
          end
          else begin
            FCustoms.AddKey(vName, vValue.AsString)
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
      vInt := Pos('.', vValue);
      if vInt > 0 then
      begin
        vStr.Add(Copy(vValue, 1, vInt - 1));
        Delete(vValue, 1, vInt);
      end;
    until vInt = 0;

    if vStr.Count = 3 then
    begin
      vHeader    := TRALBase64.Decode(vStr.Strings[0]);
      vPayload   := TRALBase64.Decode(vStr.Strings[1]);
      vSignature := vStr.Strings[2];

      CreateToken(vHeader, vPayload, vMySignature);

      if vMySignature = vSignature then
      begin
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
