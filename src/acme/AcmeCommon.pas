unit AcmeCommon;

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  RALTypes;

type
  TAcmeResponse = record
    Status: Integer;
    Body: string;
    Nonce: string;
    Location: string;
    RetryAfter: string;
  end;

function Base64Url(const Bytes: RawByteString): string;

function JsonQuote(const Value: string): string;

function JsonField(
  const Document: string;
  const Name: string
): string;

function JsonItems(
  const Document: string;
  const Name: string
): TStringList;

function ReadBytes(const Filename: string): RawByteString;

procedure SaveBytes(
  const Filename: string;
  const Bytes: RawByteString;
  PublicReadable: Boolean = False
);

procedure ValidateDomain(const Domain: string);

function TickMilliseconds: UInt64;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  BaseUnix,
  {$ENDIF}
  RALJSON;

function TickMilliseconds: UInt64;
begin
  Result := GetTickCount64;
end;

function Base64Url(const Bytes: RawByteString): string;
const
  Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
var
  I: Integer;
  N: Integer;
  A: Integer;
  B: Integer;
  C: Integer;
begin
  Result := '';
  I := 1;
  N := Length(Bytes);
  while I <= N do
  begin
    A := Ord(Bytes[I]);
    B := 0;
    C := 0;
    if I + 1 <= N then
      B := Ord(Bytes[I + 1]);
    if I + 2 <= N then
      C := Ord(Bytes[I + 2]);
    Result := Result + Alphabet[(A shr 2) + 1] + Alphabet[((A and 3) shl 4 or (B shr 4)) + 1];
    if I + 1 <= N then
      Result := Result + Alphabet[((B and 15) shl 2 or (C shr 6)) + 1];
    if I + 2 <= N then
      Result := Result + Alphabet[(C and 63) + 1];
    Inc(I, 3);
  end;
end;

function JsonQuote(const Value: string): string;
var
  ValueNode: TRALJSONString;
begin
  ValueNode := TRALJSONString.Create(Value);
  try
    Result := ValueNode.ToJSON;
  finally
    ValueNode.Free;
  end;
end;

function JsonField(
  const Document: string;
  const Name: string
): string;
var
  Root: TRALJSON;
  Value: TRALJSON;
begin
  Root := TRALJSON.ParseJSON(Document);
  try
    if not (Root is TRALJSONObject) then
      raise ERALException.Create('Expected a JSON object.');
    Value := Root.AsObject.Find(Name);
    if Value = nil then
      Result := ''
    else if Value is TRALJSONString then
      Result := Value.AsString
    else
      Result := Value.ToJSON;
  finally
    Root.Free;
  end;
end;

function JsonItems(
  const Document: string;
  const Name: string
): TStringList;
var
  Root: TRALJSON;
  Value: TRALJSON;
  Items: TRALJSONArray;
  I: Integer;
begin
  Result := TStringList.Create;
  try
    Root := TRALJSON.ParseJSON(Document);
    try
      if not (Root is TRALJSONObject) then
        raise ERALException.Create('Expected a JSON object.');
      Value := Root.AsObject.Find(Name);
      if not (Value is TRALJSONArray) then
        raise ERALException.Create('Expected JSON array: ' + Name);
      Items := Value.AsArray;
      for I := 0 to Items.Count - 1 do
        if Items[I] is TRALJSONString then
          Result.Add(Items[I].AsString)
        else
          Result.Add(Items[I].ToJSON);
    finally
      Root.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function ReadBytes(const Filename: string): RawByteString;
var
  F: TFileStream;
begin
  F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    if F.Size > 4194304 then
      raise ERALException.Create('File exceeds 4 MiB: ' + Filename);
    SetLength(Result, F.Size);
    if Length(Result) > 0 then
      F.ReadBuffer(Result[1], Length(Result));
  finally
    F.Free;
  end;
end;

procedure SaveBytes(
  const Filename: string;
  const Bytes: RawByteString;
  PublicReadable: Boolean
);
{$IFDEF MSWINDOWS}
const
  MoveFileWriteThrough = $00000008;
{$ENDIF}
var
  F: THandleStream;
  H: THandle;
  Temp: string;
  ID: TGUID;
begin
  CreateGUID(ID);
  Temp := Filename + '.' + GUIDToString(ID) + '.tmp';
  {$IFDEF MSWINDOWS}
  H := CreateFile(PChar(Temp), GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  if H = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  {$ELSE}
  if PublicReadable then
    H := fpOpen(PChar(Temp), O_WRONLY or O_CREAT or O_EXCL, &644)
  else
    H := fpOpen(PChar(Temp), O_WRONLY or O_CREAT or O_EXCL, &600);
  if LongInt(H) < 0 then
    raise ERALException.Create('Cannot create file: ' + Temp);
  {$ENDIF}
  try
    try
      F := THandleStream.Create(H);
      try
        if Length(Bytes) > 0 then
          F.WriteBuffer(Bytes[1], Length(Bytes));
      finally
        F.Free;
      end;
    finally
      FileClose(H);
    end;
    {$IFDEF MSWINDOWS}
    if not MoveFileEx(PChar(Temp), PChar(Filename), MOVEFILE_REPLACE_EXISTING or MoveFileWriteThrough) then
      RaiseLastOSError;
    {$ELSE}
    if not RenameFile(Temp, Filename) then
      raise ERALException.Create('Cannot replace file: ' + Filename);
    {$ENDIF}
  finally
    if FileExists(Temp) then
      SysUtils.DeleteFile(Temp);
  end;
end;

procedure ValidateDomain(const Domain: string);
var
  I: Integer;
  LabelLength: Integer;
begin
  if (Length(Domain) = 0) or (Length(Domain) > 253) then
    raise ERALException.Create('Invalid DNS name length.');
  LabelLength := 0;
  for I := 1 to Length(Domain) do
  begin
    if (Ord(Domain[I]) > 127) or not CharInSet(Domain[I], ['a'..'z','A'..'Z','0'..'9','-','.']) then
      raise ERALException.Create('Use DNS names in ASCII/punycode; HTTP-01 does not support wildcards.');
    if Domain[I] = '.' then
    begin
      if LabelLength = 0 then
        raise ERALException.Create('Invalid DNS label.');
      if Domain[I - 1] = '-' then
        raise ERALException.Create('Invalid DNS label.');
      LabelLength := 0;
    end
    else
    begin
      if (LabelLength = 0) and (Domain[I] = '-') then
        raise ERALException.Create('Invalid DNS label.');
      Inc(LabelLength);
      if LabelLength > 63 then
        raise ERALException.Create('DNS label exceeds 63 characters.');
    end;
  end;
  if (LabelLength = 0) or (Domain[Length(Domain)] = '-') then
    raise ERALException.Create('Invalid DNS name.');
end;

end.
