unit RALToken;

interface

uses
  Classes, SysUtils,
  RALTypes, RALSHA2_32, RALSHA2_64, RALHashes, RALBase64,
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
    FCustoms : TStringList;
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

    property AsJSON : StringRAL read GetAsJSON write SetAsJSON;
  end;

  TRALJWT = class
  private
    FAlgorithm : TRALJWTAlgorithm;
    FHeader: StringRAL;
    FPayload: StringRAL;
    FSignature: StringRAL;
    FSecret : StringRAL;
  protected
    procedure SetToken(const AValue: StringRAL);
    function GetToken: StringRAL;
  public
    property Header: StringRAL read FHeader write FHeader;
    property Payload: StringRAL read FPayload write FPayload;
    property Signature: StringRAL read FSignature;
    property Secret: StringRAL read FSecret write FSecret;

    property Token: StringRAL read GetToken write SetToken;
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
    vJson.Free;
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
  FCustoms := TStringList.Create;
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
  vInt : IntegerRAL;
begin
  vJson := TJSONObject.Create;
  try
    if FAudience <> '' then
      vJson.AddPair('aud',FAudience);

    if FExpiration > 0 then
      vJson.AddPair('exp',TJSONNumber.Create(FExpiration));

    if FIssuedAt > 0 then
      vJson.AddPair('iat',TJSONNumber.Create(FIssuedAt));

    if FIssuer <> '' then
      vJson.AddPair('iss',FIssuer);

    if FJWTId <> '' then
      vJson.AddPair('jti',FJWTId);

    if FNotBefore > 0 then
      vJson.AddPair('nbf',TJSONNumber.Create(FNotBefore));

    if FSubject <> '' then
      vJson.AddPair('sub',FSubject);

    vInt := 0;
    while vInt < FCustoms.Count do begin

      vInt := vInt + 1;
    end;

    Result := vJson.ToJSON;
  finally
    vJson.Free;
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
            FExpiration := TJSONNumber(vPair.JsonValue).AsDouble
          else
            FExpiration := StrToDateTimeDef(vPair.JsonValue.Value,0);
        end
        else if SameText(vPairName,'iat') then begin
          if vPair.JsonValue is TJSONNumber then
            FIssuedAt := TJSONNumber(vPair.JsonValue).AsDouble
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
            FNotBefore := TJSONNumber(vPair.JsonValue).AsDouble
          else
            FNotBefore := StrToDateTimeDef(vPair.JsonValue.Value,0);
        end
        else if SameText(vPairName,'sub') then begin
          FSubject := vPair.JsonValue.Value;
        end;

        vInt := vInt + 1;
      end;
    end;
  finally
    FreeAndNil(vJson);
  end;
end;

{ TRALJWT }

function TRALJWT.GetToken: StringRAL;
var
  vHash : TRALHashes;
begin
  Result := TRALBase64.Encode(FHeader) + '.' +
            TRALBase64.Encode(FPayload);

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
    Result := Result + '.' +
              vHash.HMACAsString(Result,FSecret);
  finally
    FreeAndNil(vHash);
  end;
end;

procedure TRALJWT.SetToken(const AValue: StringRAL);
var
  vStr : TStringList;
  vInt : IntegerRAL;
  vValue : StringRAL;
begin
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
      FHeader := TRALBase64.Decode(vStr.Strings[0]);
      FPayload := TRALBase64.Decode(vStr.Strings[1]);
      FSignature := vStr.Strings[2];
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

end.
