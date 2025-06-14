/// Unit for AES Criptography functions
unit RALCriptoAES;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALCripto, RALTypes, RALConsts, RALTools;

type
  TRALAESType = (tAES128, tAES192, tAES256);

  { TRALThreadCriptoAES }
  TRALThreadCriptoAES = class(TThread)
  private
    FInput: PByte;
    FOutput: PByte;
    FWordKeys: PCardinal;

    FInputLen: IntegerRAL;
    FOutputLen: IntegerRAL;
    FWordKeysLen: IntegerRAL;

    FEncrypt: IntegerRAL;
  protected
    /// Decrypt cipher
    procedure DecMixColumns(AInput, AOutput: PByte);
    procedure DecSubShiftRows(AInput, AOutput: PByte);
    /// Encrypt cipher
    procedure EncMixColumns(AInput, AOutput: PByte);
    procedure EncSubShiftRows(AInput, AOutput: PByte);

    /// Encrypt and Decrypt
    procedure RoundKey(AInput, AOutput: PByte; AKey: PCardinal);

    procedure Execute; override;

    procedure EncryptAES;
    procedure DecryptAES;
  public
    procedure Encrypt;
    procedure Decrypt;

    property Input: PByte read FInput write FInput;
    property Output: PByte read FOutput write FOutput;
    property WordKeys: PCardinal read FWordKeys write FWordKeys;
  published
    property InputLen: IntegerRAL read FInputLen write FInputLen;
    property OutputLen: IntegerRAL read FOutputLen write FOutputLen;
    property WordKeysLen: IntegerRAL read FWordKeysLen write FWordKeysLen;
  end;

  { TRALCriptoAES }

  /// AES Criptography class
  TRALCriptoAES = class(TRALCripto)
  private
    FAESType: TRALAESType;
    FLogAES: TStringList;
    FWordKeys: array of Cardinal; // UInt32;

    FTotalThread: IntegerRAL;
    FOutputLen: IntegerRAL;
  protected
    function CheckKey: boolean;
    procedure ClearThreads(AList: TThreadList);

    /// Decrypt cipher
    function DecryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;
    /// Encrypt cipher
    function EncryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;

    /// Cypher Encrypt and Decrypt
    procedure KeyExpansion;
    procedure LogAES(const ALog: StringRAL; AInput: PByte);
    /// Key expansion
    function RotWord(AInt: Cardinal): Cardinal;
    procedure SetAESType(AValue: TRALAESType);
    procedure SetKey(const AValue: StringRAL); override;
    function SubWord(AInt: Cardinal): Cardinal;
    function WordToBytes(AInt: Cardinal): TBytes;

    class function Multi02(AValue: byte): byte;
    class function Multi(AMult: integer; AByte: byte): byte;
    class procedure GenerateSBox;
    class procedure GenerateRCON;
    class procedure InitializeAES;
  public
    constructor Create;
    destructor Destroy; override;

    function AESKeys(AIndex: integer): TBytes;
    function CountKeys: integer;
    function DecryptAsStream(AValue: TStream): TStream; override;
    function EncryptAsStream(AValue: TStream): TStream; override;
    function KeysToList: TStringList;
  published
    property AESType: TRALAESType read FAESType write SetAESType;
  end;

implementation

const
  cNumberRounds: array [TRALAESType] of integer = (10, 12, 14); // nr
  cKeyLength: array [TRALAESType] of integer = (4, 6, 8); // nk
  cBlockSize: integer = 4; // nb

var
  FDecSBOX: array [0 .. 255] of byte;
  FEncSBOX: array [0 .. 255] of byte;
  FMulti02: array [0 .. 255] of byte;
  FMulti03: array [0 .. 255] of byte;
  FMulti09: array [0 .. 255] of byte; // 09
  FMulti11: array [0 .. 255] of byte; // 0b
  FMulti13: array [0 .. 255] of byte; // 0d
  FMulti14: array [0 .. 255] of byte; // 0e
  FRCON: array [0 .. 255] of byte;

  { TRALThreadCriptoAES }

procedure TRALThreadCriptoAES.DecMixColumns(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
  vProx: IntegerRAL;
begin
  vProx := 0;
  for vInt := 0 to 15 do
  begin
    case vInt of
{$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      0, 4, 8, 12:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti14[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti11[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti13
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti09
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      1, 5, 9, 13:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti09[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti14[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti11
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti13
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      2, 6, 10, 14:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti13[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti09[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti14
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti11
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti11[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti13[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti09
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti14
            [PByte(LongInt(AInput) + vProx + 3)^];
          vProx := vProx + 4;
        end;
{$ELSE}
      0, 4, 8, 12:
        begin
          PByte(AOutput + vInt)^ := FMulti14[PByte(AInput + vProx + 0)^] xor FMulti11
            [PByte(AInput + vProx + 1)^] xor FMulti13[PByte(AInput + vProx + 2)^]
            xor FMulti09[PByte(AInput + vProx + 3)^];
        end;
      1, 5, 9, 13:
        begin
          PByte(AOutput + vInt)^ := FMulti09[PByte(AInput + vProx + 0)^] xor FMulti14
            [PByte(AInput + vProx + 1)^] xor FMulti11[PByte(AInput + vProx + 2)^]
            xor FMulti13[PByte(AInput + vProx + 3)^];
        end;
      2, 6, 10, 14:
        begin
          PByte(AOutput + vInt)^ := FMulti13[PByte(AInput + vProx + 0)^] xor FMulti09
            [PByte(AInput + vProx + 1)^] xor FMulti14[PByte(AInput + vProx + 2)^]
            xor FMulti11[PByte(AInput + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(AOutput + vInt)^ := FMulti11[PByte(AInput + vProx + 0)^] xor FMulti13
            [PByte(AInput + vProx + 1)^] xor FMulti09[PByte(AInput + vProx + 2)^]
            xor FMulti14[PByte(AInput + vProx + 3)^];
          vProx := vProx + 4;
        end;
{$IFEND}
    end;
  end;
end;

procedure TRALThreadCriptoAES.DecSubShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 13, 10, 07, 04, 01, 14, 11, 08, 05, 02, 15, 12,
    09, 06, 03);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
{$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PByte(LongInt(AOutput) + vInt)^ := FDecSBOX[PByte(LongInt(AInput) + vShift[vInt])^];
{$ELSE}
    PByte(AOutput + vInt)^ := FDecSBOX[PByte(AInput + vShift[vInt])^];
{$IFEND}
  end;
end;

procedure TRALThreadCriptoAES.EncMixColumns(AInput, AOutput: PByte);
var
  vInt: IntegerRAL;
  vProx: IntegerRAL;
begin
  vProx := 0;
  for vInt := 0 to 15 do
  begin
    case vInt of
{$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
      0, 4, 8, 12:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti02[PByte(LongInt(AInput) + vProx + 0)^]
            xor FMulti03[PByte(LongInt(AInput) + vProx + 1)^]
            xor PByte(LongInt(AInput) + vProx + 2)
            ^ xor PByte(LongInt(AInput) + vProx + 3)^;
        end;
      1, 5, 9, 13:
        begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)
            ^ xor FMulti02[PByte(LongInt(AInput) + vProx + 1)^] xor FMulti03
            [PByte(LongInt(AInput) + vProx + 2)^] xor PByte(LongInt(AInput) + vProx + 3)^;
        end;
      2, 6, 10, 14:
        begin
          PByte(LongInt(AOutput) + vInt)^ := PByte(LongInt(AInput) + vProx + 0)
            ^ xor PByte(LongInt(AInput) + vProx + 1)^ xor FMulti02
            [PByte(LongInt(AInput) + vProx + 2)^] xor FMulti03
            [PByte(LongInt(AInput) + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(LongInt(AOutput) + vInt)^ := FMulti03[PByte(LongInt(AInput) + vProx + 0)^]
            xor PByte(LongInt(AInput) + vProx + 1)^ xor PByte(LongInt(AInput) + vProx + 2)
            ^ xor FMulti02[PByte(LongInt(AInput) + vProx + 3)^];
          vProx := vProx + 4;
        end;
{$ELSE}
      0, 4, 8, 12:
        begin
          PByte(AOutput + vInt)^ := FMulti02[PByte(AInput + vProx + 0)^] xor FMulti03
            [PByte(AInput + vProx + 1)^] xor PByte(AInput + vProx + 2)
            ^ xor PByte(AInput + vProx + 3)^;
        end;
      1, 5, 9, 13:
        begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)^ xor FMulti02
            [PByte(AInput + vProx + 1)^] xor FMulti03[PByte(AInput + vProx + 2)^]
            xor PByte(AInput + vProx + 3)^;
        end;
      2, 6, 10, 14:
        begin
          PByte(AOutput + vInt)^ := PByte(AInput + vProx + 0)
            ^ xor PByte(AInput + vProx + 1)^ xor FMulti02[PByte(AInput + vProx + 2)^]
            xor FMulti03[PByte(AInput + vProx + 3)^];
        end;
      3, 7, 11, 15:
        begin
          PByte(AOutput + vInt)^ := FMulti03[PByte(AInput + vProx + 0)^]
            xor PByte(AInput + vProx + 1)^ xor PByte(AInput + vProx + 2)^ xor FMulti02
            [PByte(AInput + vProx + 3)^];
          vProx := vProx + 4;
        end;
{$IFEND}
    end;
  end;
end;

procedure TRALThreadCriptoAES.EncSubShiftRows(AInput, AOutput: PByte);
const
  vShift: array [0 .. 15] of byte = (00, 05, 10, 15, 04, 09, 14, 03, 08, 13, 02, 07, 12,
    01, 06, 11);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 15 do
  begin
{$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PByte(LongInt(AOutput) + vInt)^ := FEncSBOX[PByte(LongInt(AInput) + vShift[vInt])^];
{$ELSE}
    PByte(AOutput + vInt)^ := FEncSBOX[PByte(AInput + vShift[vInt])^];
{$IFEND}
  end;
end;

procedure TRALThreadCriptoAES.RoundKey(AInput, AOutput: PByte; AKey: PCardinal);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to 3 do
  begin
{$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
    PCardinal(PByte(LongInt(AInput) + (vInt * 4)))^ :=
      PCardinal(PByte(LongInt(AInput) + (vInt * 4)))^ xor AKey^;
{$ELSE}
    PCardinal(AInput + (vInt * 4))^ := PCardinal(AInput + (vInt * 4))^ xor AKey^;
{$IFEND}
    Inc(AKey);
  end;
end;

procedure TRALThreadCriptoAES.Execute;
begin
  if FEncrypt = 0 then
    EncryptAES
  else
    DecryptAES;
end;

procedure TRALThreadCriptoAES.EncryptAES;
var
  vPosKey: integer;
begin
  FOutputLen := FInputLen;
  while FInputLen > 0 do
  begin
    // mexe somente no input
    RoundKey(FInput, FOutput, FWordKeys);

    vPosKey := 4;
    while vPosKey < FWordKeysLen do
    begin
      Inc(FWordKeys, 4);
      // mexe no output , input se mantem
      EncSubShiftRows(FInput, FOutput);
      // pega o output do shit e joga no input
      EncMixColumns(FOutput, FInput);
      // mexe somente no input
      RoundKey(FInput, FOutput, FWordKeys);
      vPosKey := vPosKey + 4;
    end;

    Inc(FWordKeys, 4);
    // mexe no output , input se mantem
    EncSubShiftRows(FInput, FOutput);
    // mexe somente no output
    RoundKey(FOutput, FInput, FWordKeys);

    Inc(FInput, 16);
    Inc(FOutput, 16);
    FInputLen := FInputLen - 16;
    Dec(FWordKeys, FWordKeysLen);
  end;
end;

procedure TRALThreadCriptoAES.DecryptAES;
var
  vPosKey: integer;
begin
  FOutputLen := FInputLen;
  while FInputLen > 0 do
  begin
    // mexe somente no input
    RoundKey(FInput, FOutput, FWordKeys);

    vPosKey := FWordKeysLen - 4;
    while vPosKey > 0 do
    begin
      Dec(FWordKeys, 4);
      // pega o input e joga no output
      DecSubShiftRows(FInput, FOutput);
      // mexe somente no output
      RoundKey(FOutput, FInput, FWordKeys);

      // pega o output e joga pro input
      DecMixColumns(FOutput, FInput);
      vPosKey := vPosKey - 4;
    end;

    Dec(FWordKeys, 4);
    // pega o input e joga no output
    DecSubShiftRows(FInput, FOutput);
    RoundKey(FOutput, FInput, FWordKeys);

    Inc(FInput, 16);
    Inc(FOutput, 16);
    FInputLen := FInputLen - 16;
    Inc(FWordKeys, FWordKeysLen);
  end;
end;

procedure TRALThreadCriptoAES.Encrypt;
begin
  FEncrypt := 0;
{$IF (DEFINED(FPC) OR DEFINED(DELPHI2010UP))}
  Start;
{$ELSE}
  Resume;
{$IFEND}
end;

procedure TRALThreadCriptoAES.Decrypt;
begin
  FEncrypt := 1;
{$IF (DEFINED(FPC) OR DEFINED(DELPHI2010UP))}
  Start;
{$ELSE}
  Resume;
{$IFEND}
end;

{ TRALCriptoAES }

function TRALCriptoAES.RotWord(AInt: Cardinal): Cardinal;
var
  vNum: TBytes;
  vByte: byte;
begin
  vNum := WordToBytes(AInt);

  vByte := vNum[0];
  vNum[0] := vNum[1];
  vNum[1] := vNum[2];
  vNum[2] := vNum[3];
  vNum[3] := vByte;

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.SubWord(AInt: Cardinal): Cardinal;
var
  vNum: TBytes;
begin
  vNum := WordToBytes(AInt);
  vNum[0] := FEncSBOX[vNum[0]];
  vNum[1] := FEncSBOX[vNum[1]];
  vNum[2] := FEncSBOX[vNum[2]];
  vNum[3] := FEncSBOX[vNum[3]];

  Move(vNum[0], Result, 4);
end;

function TRALCriptoAES.WordToBytes(AInt: Cardinal): TBytes;
begin
  SetLength(Result, 4);
  Move(AInt, Result[0], 4);
end;

function TRALCriptoAES.EncryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;
var
  vPosKey, vNr: IntegerRAL;
  vSize, vTheads: IntegerRAL;
  vThread: TRALThreadCriptoAES;
  vThrList: TThreadList;
begin
  vNr := cNumberRounds[FAESType];
  vPosKey := cBlockSize * vNr;

  Result := AInputLen;
  vTheads := RALCPUCount;
  vSize := AInputLen div (vTheads - 1) div 16 * 16;
  if vSize < 16 then
    vSize := 16;

  FOutputLen := 0;
  FTotalThread := 0;

  vThrList := TThreadList.Create;
  try
    while FOutputLen < Result do
    begin
      if (FTotalThread < vTheads) and (AInputLen > 0) then
      begin
        if vSize > AInputLen then
          vSize := AInputLen;

        FTotalThread := FTotalThread + 1;

        vThread := TRALThreadCriptoAES.Create(True);

        vThrList.Add(vThread);

        vThread.Input := AInput;
        vThread.Output := AOutput;
        vThread.WordKeys := @FWordKeys[0];
        vThread.InputLen := vSize;
        vThread.WordKeysLen := vPosKey;
        vThread.Encrypt;

        Inc(AInput, vSize);
        Inc(AOutput, vSize);
        AInputLen := AInputLen - vSize;
      end
      else
      begin
        ClearThreads(vThrList);
        Sleep(1);
      end;
    end;
  finally
    ClearThreads(vThrList);
    FreeAndNil(vThrList);
  end;
end;

function TRALCriptoAES.DecryptAES(AInput, AOutput: PByte; AInputLen: integer): integer;
var
  vPosKey, vNr: IntegerRAL;
  vSize, vTheads: IntegerRAL;
  vThread: TRALThreadCriptoAES;
  vThrList: TThreadList;
begin
  vNr := cNumberRounds[FAESType];
  vPosKey := cBlockSize * vNr;

  Result := AInputLen;
  vTheads := RALCPUCount;
  vSize := AInputLen div (vTheads - 1) div 16 * 16;
  if vSize < 16 then
    vSize := 16;

  FOutputLen := 0;
  FTotalThread := 0;

  vThrList := TThreadList.Create;
  try
    while FOutputLen < Result do
    begin
      if (FTotalThread < vTheads) and (AInputLen > 0) then
      begin
        if vSize > AInputLen then
          vSize := AInputLen;

        FTotalThread := FTotalThread + 1;

        vThread := TRALThreadCriptoAES.Create(True);

        vThrList.Add(vThread);

        vThread.Input := AInput;
        vThread.Output := AOutput;
        vThread.WordKeys := @FWordKeys[vPosKey];
        vThread.InputLen := vSize;
        vThread.WordKeysLen := vPosKey;
        vThread.Decrypt;

        Inc(AInput, vSize);
        Inc(AOutput, vSize);
        AInputLen := AInputLen - vSize;
      end
      else
      begin
        ClearThreads(vThrList);
        Sleep(1);
      end;
    end;
  finally
    ClearThreads(vThrList);
    FreeAndNil(vThrList);
  end;
end;

procedure TRALCriptoAES.SetAESType(AValue: TRALAESType);
begin
  if FAESType = AValue then
    Exit;

  FAESType := AValue;
  KeyExpansion;
end;

procedure TRALCriptoAES.SetKey(const AValue: StringRAL);
begin
  inherited SetKey(AValue);
  KeyExpansion;
end;

function TRALCriptoAES.CheckKey: boolean;
begin
  if Length(FWordKeys) = 0 then
  begin
    Result := False;
    raise Exception.Create(emCryptEmptyKey);
  end
  else
  begin
    Result := True;
  end;
end;

procedure TRALCriptoAES.ClearThreads(AList: TThreadList);
var
  vList: TList;
  vInt: integer;
  vThread: TRALThreadCriptoAES;
begin
  vList := AList.LockList;
  for vInt := Pred(vList.Count) downto 0 do
  begin
    vThread := TRALThreadCriptoAES(vList.Items[vInt]);
    if vThread.Finished then
    begin
      FTotalThread := FTotalThread - 1;
      FOutputLen := FOutputLen + vThread.OutputLen;

      AList.Remove(vThread);
      vThread.Free;
    end;
  end;
  AList.UnlockList;
end;

procedure TRALCriptoAES.KeyExpansion;
var
  vTemp: Cardinal;
  vInt, vNk, vNb, vNr: IntegerRAL;
  vKey: TBytes;
begin
  SetLength(FWordKeys, 0);
  if Length(Key) = 0 then
    Exit;

  vNk := cKeyLength[FAESType];
  vNb := cBlockSize;
  vNr := cNumberRounds[FAESType];

  SetLength(vKey, 4 * vNk);
  FillChar(vKey[0], 4 * vNk, 0);
  SetLength(FWordKeys, vNb * (vNr + 1));

  vInt := 4 * vNk;
  if Length(Key) < vInt then
    vInt := Length(Key);

  if Length(Key) > 0 then
    Move(Key[PosIniStr], vKey[0], vInt);

  for vInt := 0 to Pred(vNk) do
    FWordKeys[vInt] := PCardinal(@vKey[4 * vInt])^;

  for vInt := vNk to Pred(vNb * (vNr + 1)) do
  begin
    vTemp := FWordKeys[vInt - 1];

    if (vInt mod vNk = 0) then
      vTemp := SubWord(RotWord(vTemp)) xor (FRCON[vInt div vNk])
    else if (vNk > 6) and (vInt mod vNk = 4) then
      vTemp := SubWord(vTemp);

    FWordKeys[vInt] := FWordKeys[vInt - vNk] xor vTemp;
  end;
end;

constructor TRALCriptoAES.Create;
begin
  inherited;
  FLogAES := TStringList.Create;
  FAESType := tAES128;
end;

destructor TRALCriptoAES.Destroy;
begin
  FLogAES.Free;
  inherited Destroy;
end;

function TRALCriptoAES.EncryptAsStream(AValue: TStream): TStream;
var
  vInBuf: array of byte;
  vOutBuf: array of byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize, vSizeBuf: Int64RAL;
  vPadding: IntegerRAL;
begin
  if not CheckKey then
    Exit;
  vPadding := 0;
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  vSizeBuf := vSize;
  if vSizeBuf > DEFAULTBUFFERSTREAMSIZE then
    vSizeBuf := (DEFAULTBUFFERSTREAMSIZE div 16) * 16
  else
    vSizeBuf := ((vSizeBuf div 16) + 1) * 16;

  if vSizeBuf < 32 then
    vSizeBuf := 32;

  SetLength(vInBuf, vSizeBuf);
  SetLength(vOutBuf, vSizeBuf);

  Result := TMemoryStream.Create;
  Result.Size := AValue.Size + 32;

  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf) - 16);

    // padding complemantar
    vPadding := vBytesRead mod 16;
    if vPadding <> 0 then
    begin
      if vBytesRead + (16 - vPadding) <= Length(vInBuf) then
      begin
        FillChar(vInBuf[vBytesRead], 16 - vPadding, 16 - vPadding);
        vBytesRead := vBytesRead + (16 - vPadding);
      end;
    end;

    vBytesWrite := EncryptAES(@vInBuf[0], @vOutBuf[0], vBytesRead);
    Result.Write(vOutBuf[0], vBytesWrite);

    vPosition := vPosition + (vBytesRead - (vBytesRead mod 16));
  end;

  // padding nao complementar
  if vPadding = 0 then
  begin
    FillChar(vInBuf[0], 16, 16);
    vBytesRead := 16;
    vBytesWrite := EncryptAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutBuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;
  Result.Size := vPosition;
  Result.Position := 0;
end;

function TRALCriptoAES.DecryptAsStream(AValue: TStream): TStream;
var
  vInBuf: array of byte;
  vOutBuf: array of byte;
  vBytesRead, vBytesWrite: IntegerRAL;
  vPosition, vSize, vSizeBuf: Int64RAL;
  vPad1, vPad2: byte;
begin
  if not CheckKey then
    Exit;

  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  vSizeBuf := vSize;
  if vSizeBuf > DEFAULTBUFFERSTREAMSIZE then
    vSizeBuf := (DEFAULTBUFFERSTREAMSIZE div 16) * 16;

  SetLength(vInBuf, vSizeBuf);
  SetLength(vOutBuf, vSizeBuf);

  Result := TMemoryStream.Create;
  Result.Size := AValue.Size;

  while vPosition < vSize do
  begin
    FillChar(vInBuf[0], Length(vInBuf), 0);

    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    vBytesWrite := DecryptAES(@vInBuf[0], @vOutBuf[0], vBytesRead);

    Result.Write(vOutBuf[0], vBytesWrite);

    vPosition := vPosition + vBytesRead;
  end;

  // verificando paddind
  Result.Position := Result.Size - 1;
  Result.Read(vPad1, 1);
  Result.Position := Result.Size - vPad1;
  while Result.Position < Result.Size do
  begin
    Result.Read(vPad2, 1);
    if vPad2 <> vPad1 then
      Break;
  end;

  // eliminando os padding
  if Result.Position = Result.Size then
    Result.Size := Result.Size - vPad1;

  Result.Position := 0;
end;

function TRALCriptoAES.AESKeys(AIndex: integer): TBytes;
var
  vInt: IntegerRAL;
  vBytes: TBytes;
begin
  if not CheckKey then
    Exit;

  Result := nil;
  if (AIndex >= 0) and (AIndex < CountKeys) then
  begin
    SetLength(Result, 16);
    for vInt := 0 to 3 do
    begin
      vBytes := WordToBytes(FWordKeys[(AIndex * 4) + vInt]);
      Move(vBytes[0], Result[vInt * 4], 4);
    end;
  end;
end;

function TRALCriptoAES.CountKeys: integer;
begin
  Result := cNumberRounds[FAESType] + 1;
end;

function TRALCriptoAES.KeysToList: TStringList;
var
  vInt1, vInt2: integer;
  vStr: StringRAL;
  vKey: TBytes;
begin
  if not CheckKey then
    Exit;

  Result := TStringList.Create;

  for vInt1 := 0 to Pred(CountKeys) do
  begin
    vKey := AESKeys(vInt1);
    vStr := '';
    for vInt2 := 0 to 15 do
    begin
      if vStr <> '' then
        vStr := vStr + ' ';
      vStr := vStr + IntToHex(vKey[vInt2], 2);
    end;
    Result.Add(vStr);
  end;
end;

procedure TRALCriptoAES.LogAES(const ALog: StringRAL; AInput: PByte);
var
  vInt: IntegerRAL;
  vStr: StringRAL;
begin
  FLogAES.Add(ALog);
  vStr := '';
  for vInt := 1 to 16 do
  begin
    if vStr <> '' then
      vStr := vStr + ' ';
    vStr := vStr + IntToHex(AInput^, 2);
    if vInt mod 4 = 0 then
    begin
      FLogAES.Add(vStr);
      vStr := '';
    end;
    Inc(AInput);
  end;
  FLogAES.Add('');
end;

class function TRALCriptoAES.Multi02(AValue: byte): byte;
begin
  Result := (AValue shl 1) xor ((AValue shr 7) * 283);
end;

class function TRALCriptoAES.Multi(AMult: integer; AByte: byte): byte;
var
  vInt1, vInt2: integer;
  vByte, vCalc: byte;
begin
  Result := 0;
  vInt1 := 0;
  while AMult > 0 do
  begin
    vByte := AMult and 1;

    if vByte = 1 then
    begin
      vCalc := AByte;
      for vInt2 := 1 to vInt1 do
        vCalc := Multi02(vCalc);
      Result := Result xor vCalc;
    end;

    AMult := AMult shr 1;
    vInt1 := vInt1 + 1;
  end;
end;

class procedure TRALCriptoAES.GenerateSBox;
var
  vInt: IntegerRAL;
  vMult: Cardinal;
  vBytes: array [0 .. 255] of byte;
  vByte: byte;
begin
  vByte := 1;
  for vInt := 0 to 255 do
  begin
    vBytes[vInt] := vByte;
    vByte := vByte xor Multi02(vByte);
  end;

  // DecSBOX é a posicao do byte no EncSBOX
  FEncSBOX[0] := 99; // 0x63;
  FDecSBOX[99] := 0; // 0x00

  FillChar(FDecSBOX, 256, 0);
  for vInt := 0 to 254 do
  begin
    vMult := vBytes[255 - vInt];
    vMult := vMult or (vMult shl 8);
    vMult := vMult xor (vMult shr 4) xor (vMult shr 5) xor (vMult shr 6)
      xor (vMult shr 7);

    FEncSBOX[vBytes[vInt]] := (vMult xor 99) and 255;
    FDecSBOX[FEncSBOX[vBytes[vInt]]] := vBytes[vInt];
  end;
end;

class procedure TRALCriptoAES.GenerateRCON;
var
  vInt: IntegerRAL;
  vMult: Cardinal;
begin
  FRCON[0] := 141;
  for vInt := 1 to 255 do
  begin
    vMult := FRCON[vInt - 1] * 2;
    if vMult > 255 then
      vMult := (vMult - 256) xor 27;
    FRCON[vInt] := vMult;
  end;
end;

class procedure TRALCriptoAES.InitializeAES;
var
  vByte: byte;
begin
  for vByte := 0 to 255 do
  begin
    // Encrypt
    FMulti02[vByte] := Multi02(vByte);
    FMulti03[vByte] := Multi(3, vByte);
    // Decrypt
    FMulti09[vByte] := Multi(09, vByte);
    FMulti11[vByte] := Multi(11, vByte);
    FMulti13[vByte] := Multi(13, vByte);
    FMulti14[vByte] := Multi(14, vByte);
  end;
  GenerateRCON;
  GenerateSBox;
end;

initialization

TRALCriptoAES.InitializeAES;

end.

