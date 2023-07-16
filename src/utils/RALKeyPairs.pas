unit RALKeyPairs;

interface

uses
  Classes, SysUtils,
  RALTypes;

type
  TRALKeyType = (ktNone, ktString, ktInteger, ktFloat, ktDateTime,
                 ktDate, ktTime, ktBoolean);

  TRALKeyPair = class(TCollectionItem)
  private
    FKeyName : StringRAL;
    FKeyType : TRALKeyType;
    FKeyValue : StringRAL;
  protected
    procedure SetKeyValue(const AValue : StringRAL);
    procedure SetKeyType(const AValue: TRALKeyType);
    function GetDisplayName: string; override;
    procedure SetDisplayName(const AValue: string); override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property KeyName : StringRAL read FKeyName write FKeyName;
    property KeyType : TRALKeyType read FKeyType write SetKeyType;
    property KeyValue : StringRAL read FKeyValue write SetKeyValue;
  end;

  TRALKeyPairs = class(TOwnedCollection)
  public
    constructor Create(AOwner: TPersistent);

    procedure AddKey(AKey : StringRAL; AValue : StringRAL); overload;
    procedure AddKey(AKey : StringRAL; AValue : Int64RAL); overload;
    procedure AddKey(AKey : StringRAL; AValue : Extended); overload;
    procedure AddKey(AKey : StringRAL; AValue : TDateTime); overload;
    procedure AddKey(AKey : StringRAL; AValue : Boolean); overload;
  end;

implementation

const
  ctNotKeyInteger = 'Key Value not is a Integer';
  ctNotKeyFloat = 'Key Value not is a Float';
  ctNotKeyDate = 'Key Value not is a Date/Time';
  ctNotKeyBoolean = 'Key Value not is a Boolean';

{ TRALKeyPairs }

procedure TRALKeyPairs.AddKey(AKey, AValue: StringRAL);
begin
  with Add as TRALKeyPair do
  begin
    FKeyName := AKey;
    FKeyType := ktString;
    FKeyValue := AValue;
  end;
end;

procedure TRALKeyPairs.AddKey(AKey: StringRAL; AValue: Int64RAL);
begin
  with Add as TRALKeyPair do
  begin
    FKeyName := AKey;
    FKeyType := ktInteger;
    FKeyValue := IntToStr(AValue);
  end;
end;

procedure TRALKeyPairs.AddKey(AKey: StringRAL; AValue: Extended);
begin
  with Add as TRALKeyPair do
  begin
    FKeyName := AKey;
    FKeyType := ktFloat;
    FKeyValue := FloatToStr(AValue);
  end;
end;

procedure TRALKeyPairs.AddKey(AKey: StringRAL; AValue: TDateTime);
begin
  with Add as TRALKeyPair do
  begin
    FKeyName := AKey;
    if (Trunc(AValue) > 0) and (Frac(AValue) > 0) then
      FKeyType := ktDateTime
    else if (Trunc(AValue) = 0) and (Frac(AValue) > 0) then
      FKeyType := ktTime
    else if (Trunc(AValue) > 0) and (Frac(AValue) = 0) then
      FKeyType := ktDate;
    FKeyValue := DateTimeToStr(AValue);
  end;
end;

procedure TRALKeyPairs.AddKey(AKey: StringRAL; AValue: Boolean);
begin
  with Add as TRALKeyPair do
  begin
    FKeyName := AKey;
    FKeyType := ktBoolean;
    FKeyValue := 'false';
    if AValue then
      FKeyValue := 'true';
  end;
end;

constructor TRALKeyPairs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALKeyPair);
end;

{ TRALKeyPair }

constructor TRALKeyPair.Create(ACollection: TCollection);
begin
  inherited;
  FKeyName := 'key'+IntToStr(Index);
  FKeyValue := '';
  FKeyType := ktNone;
  Changed(false);
end;

function TRALKeyPair.GetDisplayName: string;
begin
  Result := FKeyName;
end;

procedure TRALKeyPair.SetDisplayName(const AValue: string);
begin
  inherited;
  if Trim(AValue) <> '' then
    FKeyName := AValue;
end;

procedure TRALKeyPair.SetKeyType(const AValue: TRALKeyType);
var
  vValue : StringRAL;
  vType : TRALKeyType;
begin
  vValue := FKeyValue;
  vType := FKeyType;

  if vValue <> '' then
  begin
    try
      FKeyType := AValue;
      KeyValue := vValue;
    except
      FKeyType := vType;
    end;
  end;
end;

procedure TRALKeyPair.SetKeyValue(const AValue: StringRAL);
var
  vDateTime : TDateTime;
  vInteger : Int64;
  vFloat : Extended;
begin
  if (AValue <> '') then
  begin
    if FKeyType = ktString then
    begin
      FKeyValue := AValue
    end
    else if (FKeyType = ktNone) then
    begin
      if TryStrToInt64(AValue,vInteger) then
      begin
        FKeyType := ktInteger;
        FKeyValue := AValue;
      end
      else if TryStrToFloat(AValue,vFloat) then
      begin
        FKeyType := ktFloat;
        FKeyValue := AValue;
      end
      else if TryStrToDateTime(AValue,vDateTime) then
      begin
        FKeyType := ktDateTime;
        if (vDateTime > 0) and (Frac(vDateTime) = 0) and
           (Trunc(vDateTime) > 0) then
        begin
          FKeyType := ktDate;
        end
        else if (vDateTime > 0) and (Frac(vDateTime) > 0) and
                (Trunc(vDateTime) = 0) then
        begin
          FKeyType := ktTime;
        end;
        FKeyValue := AValue;
      end
      else if SameText(AValue,'true') or SameText(AValue,'false') then
      begin
        FKeyType := ktBoolean;
        FKeyValue := AValue;
      end
      else
      begin
        FKeyType := ktString;
        FKeyValue := AValue;
      end;
    end
    else if FKeyType = ktInteger then
    begin
      if TryStrToInt64(AValue,vInteger) then
        FKeyValue := AValue
      else
        raise Exception.Create(ctNotKeyInteger);
    end
    else if FKeyType = ktFloat then
    begin
      if TryStrToFloat(AValue,vFloat) then
        FKeyValue := AValue
      else
        raise Exception.Create(ctNotKeyFloat);
    end
    else if FKeyType in [ktDate,ktTime,ktDateTime] then
    begin
      if TryStrToDateTime(AValue,vDateTime) then
      begin
        case FKeyType of
          ktDateTime : FKeyValue := DateTimeToStr(vDateTime);
          ktDate     : FKeyValue := DateToStr(vDateTime);
          ktTime     : FKeyValue := TimeToStr(vDateTime);
        end;
      end
      else
      begin
        raise Exception.Create(ctNotKeyDate);
      end;
    end
    else if FKeyType = ktBoolean then
    begin
      if SameText(AValue,'true') or SameText(AValue,'false') then
        FKeyValue := AValue
      else
        raise Exception.Create(ctNotKeyBoolean);
    end;
  end
  else
  begin
    FKeyType := ktNone; // string
    FKeyValue := '';
  end;
end;

end.
