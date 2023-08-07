unit RALToken;

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALSHA2_32, RALSHA2_64, RALHashes, RALBase64,
  RALJson, RALTools;

type
  TRALJWTAlgorithm = (tjaHSHA256, tjaHSHA512);

  { TRALJWTHeader }

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
    property AsJSON : StringRAL read GetAsJSON write SetAsJSON;
  published
    property HeaderType : StringRAL read FHeaderType;
    property Algorithm : TRALJWTAlgorithm read FAlgorithm write FAlgorithm;
    property KeyID : StringRAL read FKeyID write FKeyID;
  end;

  { TRALTokenParams }

  TRALTokenParams = class(TPersistent)
  private
    FAudience : StringRAL;
    FExpiration : TDateTime;
    FIssuedAt : TDateTime;
    FIssuer : StringRAL;
    FId : StringRAL;
    FNotBefore : TDateTime;
    FSubject : StringRAL;
    FCustomClaims : TStringList;
  protected
    function GetAsJSON: StringRAL;
    procedure SetAsJSON(const AValue: StringRAL);
    procedure Initialize;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddClaim(AKey : StringRAL; AValue : StringRAL);
    procedure DelClaim(AKey : StringRAL);

    procedure createNewId;
    property AsJSON : StringRAL read GetAsJSON write SetAsJSON;
  published
    property Audience : StringRAL read FAudience write FAudience;
    property Expiration : TDateTime read FExpiration write FExpiration;
    property IssuedAt : TDateTime read FIssuedAt write FIssuedAt;
    property Issuer : StringRAL read FIssuer write FIssuer;
    property Id : StringRAL read FId write FId;
    property NotBefore : TDateTime read FNotBefore write FNotBefore;
    property Subject : StringRAL read FSubject write FSubject;
  end;

  TRALJWT = class
  private
    FAlgorithm : TRALJWTAlgorithm;
    FHeader: TRALJWTHeader;
    FPayload: TRALTokenParams;
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
    property Algorithm: TRALJWTAlgorithm read FAlgorithm write FAlgorithm;
    property Header: TRALJWTHeader read FHeader write FHeader;
    property Payload: TRALTokenParams read FPayload write FPayload;
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
    vJson.Add('typ', FHeaderType);

    case FAlgorithm of
      tjaHSHA256: vJson.Add('alg', 'HS256');
      tjaHSHA512: vJson.Add('alg', 'HS512');
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
  FHeaderType := 'JWT';
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

{ TRALTokenParams }

procedure TRALTokenParams.AddClaim(AKey, AValue: StringRAL);
begin
  FCustomClaims.Add(AKey + '=' + AValue);
end;

constructor TRALTokenParams.Create;
begin
  inherited Create;
  FCustomClaims := TStringList.Create;
  FCustomClaims.Sorted := True;
  Initialize;
end;

procedure TRALTokenParams.createNewId;
var
  vBytes : TBytes;
begin
  vBytes := randomBytes(10);
  FId := TRALBase64.Encode(vBytes);
end;

procedure TRALTokenParams.DelClaim(AKey: StringRAL);
var
  vInt : IntegerRAL;
begin
  vInt := FCustomClaims.IndexOfName(AKey);
  if vInt >= 0 then
    FCustomClaims.Delete(vInt);
end;

destructor TRALTokenParams.Destroy;
begin
  FreeAndNil(FCustomClaims);
  inherited;
end;

function TRALTokenParams.GetAsJSON: StringRAL;
var
  vJson : TRALJSONObject;
  vInt : IntegerRAL;
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

    if FId <> '' then
      vJson.Add('jti', FId);

    if FNotBefore > 0 then
      vJson.Add('nbf', DateTimeToUnix(FNotBefore));

    if FSubject <> '' then
      vJson.Add('sub', FSubject);

    DelClaim('aud');
    DelClaim('exp');
    DelClaim('iat');
    DelClaim('iss');
    DelClaim('jti');
    DelClaim('nbf');
    DelClaim('sub');

    vInt := 0;
    while vInt < FCustomClaims.Count do
    begin
      vJson.Add(FCustomClaims.Names[vInt], FCustomClaims.ValueFromIndex[vInt]);
      vInt := vInt + 1;
    end;

    Result := vJson.ToJSON;
  finally
    FreeAndNil(vJson);
  end;
end;

procedure TRALTokenParams.Initialize;
begin
  FAudience := '';
  FExpiration := 0;
  FIssuedAt := 0;
  FIssuer := '';
  FId := '';
  FNotBefore := 0;
  FSubject := '';
  FCustomClaims.Clear;
end;

procedure TRALTokenParams.SetAsJSON(const AValue: StringRAL);
var
  vJson : TRALJSONObject;
  vInt : IntegerRAL;
  vInt64 : Int64RAL;
  vName : StringRAL;
  vValue : TRALJSONValue;
begin
  Initialize;
  vJson := TRALJSONObject(ParseJson(AValue));
  try
    if vJson <> nil then
    begin
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
          FId := vValue.AsString;
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
          AddClaim(vName, vValue.AsString);
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
  FPayload := TRALTokenParams.Create;
end;

function TRALJWT.CreateToken(AHeader, APayload: StringRAL;
                             var ASignature : StringRAL): StringRAL;
var
  vHash : TRALHashes;
  vStr : StringRAL;
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
    vHash.OutputType := rhotBase64Url;

    // json codifica / para \/
    AHeader := StringReplace(AHeader, '\/', '/', [rfReplaceAll]);
    APayload := StringReplace(APayload, '\/', '/', [rfReplaceAll]);

    vStr := TRALBase64.Encode(AHeader);
    vStr := TRALBase64.ToBase64Url(vStr);

    Result := vStr + '.';

    vStr := TRALBase64.Encode(APayload);
    vStr := TRALBase64.ToBase64Url(vStr);

    Result := Result + vStr;

    ASignature := vHash.HMACAsString(Result, FSecret);

    Result := Result + '.' + ASignature;
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
  Result := CreateToken(FHeader.AsJSON, FPayload.AsJSON, FSignature);
end;

function TRALJWT.ValidToken(const AValue: StringRAL) : boolean;
var
  vStr : TStringList;
  vInt : IntegerRAL;
  vValue : StringRAL;

  vHeader : StringRAL;
  vPayload : StringRAL;
  vSignature, vMySignature : StringRAL;

  vObjPayload : TRALTokenParams;
begin
  Result := False;

  vValue := AValue;
  vStr := TStringList.Create;
  try
    repeat
      vInt := Pos('.', vValue);
      if (vInt = 0) and (vValue <> '') then
        vInt := Length(vValue) + 1;

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
        Result := True;
        vObjPayload := TRALTokenParams.Create;
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
