/// Server-Sent Events (text/event-stream): the writer the server sends with and the
/// incremental reader the client parses with.
unit RALSSE;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools;

type
  /// One event as the reader hands it over.
  TRALSSEEvent = record
    /// The data lines, joined by LF.
    Data: StringRAL;
    /// The event field; empty is the default event, 'message'.
    Event: StringRAL;
    /// The last id field seen, which a reconnection sends as Last-Event-ID.
    Id: StringRAL;
    /// The retry field, in milliseconds; -1 when the event had none.
    Retry: IntegerRAL;
  end;

  /// Fired for each complete event; ACancel stops the reading.
  TRALSSEOnEvent = procedure(ASender: TObject; const AEvent: TRALSSEEvent;
    var ACancel: boolean) of object;

  { TRALSSEParser }

  { Reads text/event-stream as it arrives, in pieces of any size, by the rules of
    the WHATWG HTML standard: a field without ':' is a field with an empty value, one
    space after ':' is dropped, retry takes digits only, a BOM at the start is
    skipped, an event without data is not dispatched. MaxEventSize bounds what the
    network can make it hold. }
  TRALSSEParser = class
  private
    /// The first line is still to come: a BOM at its start is skipped.
    FFirstLine: boolean;
    FCancelled: boolean;
    FData: StringRAL;
    FEvent: StringRAL;
    FHasData: boolean;
    FLastId: StringRAL;
    FLine: StringRAL;
    FMaxEventSize: IntegerRAL;
    FOnEvent: TRALSSEOnEvent;
    FPendingCR: boolean;
    FRetry: IntegerRAL;
    FSize: IntegerRAL;

    /// Dispatches the event gathered so far, if it has data.
    procedure DispatchEvent;
    /// Takes one complete line.
    procedure ProcessLine(const ALine: StringRAL);
  public
    constructor Create;

    /// Feeds ASize bytes; False once an event handler cancelled.
    function Feed(const ABuffer; ASize: IntegerRAL): boolean;
    /// Forgets the event being gathered and the text not yet ended by a line break.
    procedure Reset;

    /// True once an event handler set ACancel.
    property Cancelled: boolean read FCancelled;
    /// The id a reconnection sends as Last-Event-ID.
    property LastEventId: StringRAL read FLastId;
    /// Most bytes one event may take; past it, raises ERALSSEError. 0 is no limit.
    property MaxEventSize: IntegerRAL read FMaxEventSize write FMaxEventSize;
    /// Fired for each complete event.
    property OnEvent: TRALSSEOnEvent read FOnEvent write FOnEvent;
  end;

  /// An event past MaxEventSize.
  ERALSSEError = class(Exception);

{ One event as text/event-stream writes it: AData split on CR, LF and CRLF into
  data: lines, then event: and id: (CR, LF and NUL taken out, so a value cannot open
  a forged field), retry: when ARetry >= 0, and the blank line that ends it }
function RALSSEEvent(const AData: StringRAL; const AEvent: StringRAL = '';
  const AId: StringRAL = ''; ARetry: IntegerRAL = -1): StringRAL;
/// A comment line (': text'), which keeps a quiet stream alive and is never an event.
function RALSSEComment(const AText: StringRAL = 'ping'): StringRAL;

implementation

/// AValue without CR, LF and NUL: a field value stays on its own line
function SSEFieldValue(const AValue: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := RALSafeHeaderText(AValue);
  for vInt := Length(Result) downto 1 do
    if Result[vInt] = #0 then
      Delete(Result, vInt, 1);
end;

function RALSSEEvent(const AData: StringRAL; const AEvent: StringRAL;
  const AId: StringRAL; ARetry: IntegerRAL): StringRAL;
var
  vInt, vStart: IntegerRAL;
begin
  Result := '';
  if AEvent <> '' then
    Result := Result + 'event: ' + SSEFieldValue(AEvent) + #10;
  if AId <> '' then
    Result := Result + 'id: ' + SSEFieldValue(AId) + #10;
  if ARetry >= 0 then
    Result := Result + 'retry: ' + StringRAL(IntToStr(ARetry)) + #10;

  // a line per piece of the data, whatever line break it used
  vStart := 1;
  vInt := 1;
  while vInt <= Length(AData) do
  begin
    if (AData[vInt] = #13) or (AData[vInt] = #10) then
    begin
      Result := Result + 'data: ' + Copy(AData, vStart, vInt - vStart) + #10;
      if (AData[vInt] = #13) and (vInt < Length(AData)) and (AData[vInt + 1] = #10) then
        Inc(vInt);
      vStart := vInt + 1;
    end;
    Inc(vInt);
  end;
  Result := Result + 'data: ' + Copy(AData, vStart, MaxInt) + #10 + #10;
end;

function RALSSEComment(const AText: StringRAL): StringRAL;
begin
  Result := ': ' + SSEFieldValue(AText) + #10 + #10;
end;

{ TRALSSEParser }

constructor TRALSSEParser.Create;
begin
  inherited Create;
  FMaxEventSize := 1048576;
  FFirstLine := True;
  Reset;
end;

procedure TRALSSEParser.Reset;
begin
  FLine := '';
  FData := '';
  FEvent := '';
  FHasData := False;
  FRetry := -1;
  FSize := 0;
  FPendingCR := False;
end;

procedure TRALSSEParser.DispatchEvent;
var
  vEvent: TRALSSEEvent;
begin
  if FHasData then
  begin
    vEvent.Data := FData;
    vEvent.Event := FEvent;
    vEvent.Id := FLastId;
    vEvent.Retry := FRetry;
    if Assigned(FOnEvent) then
      FOnEvent(Self, vEvent, FCancelled);
  end;
  FData := '';
  FEvent := '';
  FHasData := False;
  FRetry := -1;
  FSize := 0;
end;

procedure TRALSSEParser.ProcessLine(const ALine: StringRAL);
var
  vPos, vInt: IntegerRAL;
  vField, vValue: StringRAL;
  vDigits: boolean;
begin
  if FFirstLine then
  begin
    FFirstLine := False;
    // the byte order mark, once, at the very start of the stream
    { compared byte by byte: a literal of bytes above 127 is converted by the
      compiler to the code page of the string type }
    if (Length(ALine) >= 3) and (Ord(ALine[1]) = $EF) and (Ord(ALine[2]) = $BB) and
       (Ord(ALine[3]) = $BF) then
    begin
      ProcessLine(Copy(ALine, 4, MaxInt));
      Exit;
    end;
  end;
  if ALine = '' then
  begin
    DispatchEvent;
    Exit;
  end;
  if ALine[1] = ':' then
    Exit; // a comment

  vPos := Pos(StringRAL(':'), ALine);
  if vPos > 0 then
  begin
    vField := Copy(ALine, 1, vPos - 1);
    vValue := Copy(ALine, vPos + 1, MaxInt);
    if (vValue <> '') and (vValue[1] = ' ') then
      Delete(vValue, 1, 1);
  end
  else
  begin
    vField := ALine;
    vValue := '';
  end;

  if vField = 'data' then
  begin
    if FHasData then
      FData := FData + #10 + vValue
    else
      FData := vValue;
    FHasData := True;
  end
  else if vField = 'event' then
    FEvent := vValue
  else if vField = 'id' then
  begin
    // an id holding NUL is ignored, as the standard says
    if Pos(StringRAL(#0), vValue) = 0 then
      FLastId := vValue;
  end
  else if vField = 'retry' then
  begin
    vDigits := vValue <> '';
    for vInt := 1 to Length(vValue) do
      if (vValue[vInt] < '0') or (vValue[vInt] > '9') then
        vDigits := False;
    if vDigits and (Length(vValue) <= 9) then
      FRetry := StrToInt(string(vValue));
  end;
  // any other field is ignored
end;

function TRALSSEParser.Feed(const ABuffer; ASize: IntegerRAL): boolean;
var
  vBytes: PAnsiChar;
  vInt, vStart: IntegerRAL;
  vChar: AnsiChar;

  procedure TakeText(AFrom, ACount: IntegerRAL);
  var
    vText: StringRAL;
  begin
    if ACount <= 0 then
      Exit;
    SetLength(vText, ACount);
    Move(vBytes[AFrom], vText[1], ACount);
    FLine := FLine + vText;
    Inc(FSize, ACount);
    if (FMaxEventSize > 0) and (FSize > FMaxEventSize) then
    begin
      Reset;
      raise ERALSSEError.Create(emSSEEventTooBig);
    end;
  end;

begin
  Result := not FCancelled;
  if (not Result) or (ASize <= 0) then
    Exit;
  vBytes := PAnsiChar(@ABuffer);
  vStart := 0;
  vInt := 0;

  while vInt < ASize do
  begin
    vChar := vBytes[vInt];
    if FPendingCR then
    begin
      // the LF of a CRLF that a previous piece cut in two
      FPendingCR := False;
      if vChar = #10 then
      begin
        Inc(vInt);
        vStart := vInt;
        Continue;
      end;
    end;
    if (vChar = #13) or (vChar = #10) then
    begin
      TakeText(vStart, vInt - vStart);
      ProcessLine(FLine);
      FLine := '';
      if FCancelled then
      begin
        Result := False;
        Exit;
      end;
      if vChar = #13 then
      begin
        if vInt + 1 < ASize then
        begin
          if vBytes[vInt + 1] = #10 then
            Inc(vInt);
        end
        else
          FPendingCR := True;
      end;
      vStart := vInt + 1;
    end;
    Inc(vInt);
  end;
  TakeText(vStart, ASize - vStart);
end;

end.
