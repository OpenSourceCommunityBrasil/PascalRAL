/// Class with all the functions related to Stream on RAL
unit RALStream;

interface

{$I ..\base\PascalRAL.inc}

uses
  Classes, SysUtils,
  RALTypes, RALConsts;

type

  { TRALBinaryWriter }

  TRALBinaryWriter = class
  private
    FStream: TStream;
    /// Refuses a size prefix that announces more than the stream still holds
    procedure CheckSize(ASize: UInt64RAL);
  protected
    // read and write UTF-7
    function ReadSize: UInt64RAL;
    procedure WriteSize(ASize: UInt64RAL);
  public
    constructor Create(const AStream: TStream);

    procedure WriteShortint(AValue: Shortint);
    procedure WriteByte(AValue: Byte);
    procedure WriteSmallint(AValue: Smallint);
    procedure WriteWord(AValue: Word);
    procedure WriteInteger(AValue: IntegerRAL);
    procedure WriteLongWord(AValue: LongWord);
    procedure WriteInt64(AValue: Int64RAL);
    procedure WriteQWord(AValue: UInt64);
    procedure WriteBoolean(AValue: Boolean);
    procedure WriteFloat(AValue: Double);
    procedure WriteDateTime(AValue: TDateTime);
    procedure WriteStream(AValue: TStream);
    procedure WriteBytes(AValue: TBytes);
    procedure WriteString(AValue: StringRAL);
    procedure WriteChar(AValue: CharRAL);
    procedure WriteBytesDirect(AValue: TBytes);

    function ReadShortint: Shortint;
    function ReadByte: Byte;
    function ReadSmallint: Smallint;
    function ReadWord: Word;
    function ReadInteger: IntegerRAL;
    function ReadLongWord: LongWord;
    function ReadInt64: Int64RAL;
    function ReadQWord: UInt64;
    function ReadBoolean: Boolean;
    function ReadFloat: Double;
    function ReadDateTime: TDateTime;
    procedure ReadStream(AStream: TStream);
    function ReadBytes: TBytes;
    function ReadString: StringRAL;
    function ReadChar: CharRAL;
    function ReadBytesDirect(ALength: integer): TBytes;
  end;

  TRALStringStream = class(TMemoryStream)
  public
    constructor Create(AString: StringRAL); overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(ABytes: TBytes); overload;

    function DataString: StringRAL;

    procedure WriteBytes(ABytes: TBytes);
    procedure WriteString(AString: StringRAL);
    procedure WriteStream(AStream: TStream);
  end;

  { How a stream reaches the body decoder (TRALParams.DecodeBody), and so what
    the decoder may do with it:
    - boCopy: the caller keeps it and may free it right after the call - it is
      copied once. What the old setters (RequestStream :=) do;
    - boBorrowed: the caller keeps it alive for as long as the request (or the
      client response) lives, and nobody writes to it. Every server engine
      creates and frees its request inside one callback, so its own buffer is
      lent this way, at no cost;
    - boBorrowedWritable: lent as above, and the decoder may overwrite it -
      decrypting in place. Only for a buffer nobody reads afterwards (Indy's
      PostStream);
    - boOwned: handed over; freed with the params. The buffers RAL itself
      created to receive into }
  TRALBodyOwnership = (boCopy, boBorrowed, boBorrowedWritable, boOwned);

  { TRALMemoryView }

  { Read-only stream over memory that belongs to someone else - a buffer of an
    engine, of a C library, or of another stream. Writing or resizing raises.
    The memory must outlive the view }
  TRALMemoryView = class(TCustomMemoryStream)
  public
    constructor Create(APointer: Pointer; ASize: Int64RAL);
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(const NewSize: Int64); override;
  end;

  { TRALStringView }

  { Read-only stream over the bytes of a string, holding a reference to it: the
    string lives while the view does, and nothing is copied. RawByteString, so
    a StringRAL (UTF8String) or an engine's RawByteString goes in with no
    code page conversion }
  TRALStringView = class(TRALMemoryView)
  private
    FText: RawByteString;
  public
    constructor Create(const AText: RawByteString);
    property Text: RawByteString read FText;
  end;

  { TRALStreamSlice }

  { Read-only window (start, size) over another stream, which it does not own
    unless told to. Every read positions the parent itself, so several slices
    of one parent may be read in turn. Use RALStreamSlice, which answers a
    TRALMemoryView when the parent is contiguous memory }
  TRALStreamSlice = class(TStream)
  private
    FParent: TStream;
    FStart: Int64RAL;
    FSize: Int64RAL;
    FPos: Int64RAL;
    FOwnsParent: boolean;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AParent: TStream; AStart, ASize: Int64RAL;
      AOwnsParent: boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  end;

  { TRALChunkedStream }

  { A body kept as a list of blocks of BlockSize bytes instead of one
    contiguous allocation (.agents/PLANO_STREAM_UNICO.md, D8). Appending a
    block never moves what is already there - a TMemoryStream growing to
    hundreds of MB is reallocated over and over, and the memory manager may
    copy it every time - and with ReleaseOnRead every block already read is
    given back at once, which is what lets a transform hold its input and its
    output at the cost of one block instead of two whole bodies.

    Read fills the whole request across blocks: RAL code reads a size with a
    single Read (StreamToBytes), which TStream would allow to stop short }
  TRALChunkedStream = class(TStream)
  private
    FBlocks: TList;
    FBlockSize: IntegerRAL;
    FSize: Int64RAL;
    FPos: Int64RAL;
    FReleaseOnRead: boolean;
    FReleased: IntegerRAL;
    function Block(AIndex: IntegerRAL): PByte;
    procedure ReleaseBefore(APosition: Int64RAL);
  protected
    function GetSize: Int64; override;
  public
    constructor Create(ABlockSize: IntegerRAL = 0);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(const NewSize: Int64); override;

    property BlockSize: IntegerRAL read FBlockSize;
    { Every block that the reading position has left behind is freed. For a
      stream read once, front to back, by a transform; seeking back into a
      freed block raises }
    property ReleaseOnRead: boolean read FReleaseOnRead write FReleaseOnRead;
  end;

  { TRALTempFileStream }

  { A body kept in a file of its own, for bodies above SpoolAbove. Created
    with a unique name in RALSpoolFolder (the system temp folder when empty)
    and deleted when freed - also when the request ends in an exception. A
    process killed in the middle leaves the file behind; RALCleanSpoolFolder
    removes what is older than a day }
  TRALTempFileStream = class(TFileStream)
  private
    FFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  { TRALBodyStream }

  { The storage of a body whose size is not known in advance, or is: one
    contiguous block up to RALChunkAbove, a TRALChunkedStream above it, a
    TRALTempFileStream above SpoolAbove (when it is not 0). It starts small and
    moves to the next kind the moment a write crosses the limit - one copy of
    what it holds at that point, bounded by the limit. With AExpectedSize
    (a Content-Length, the size of the input of a cipher) the right kind is
    chosen at once and a contiguous block is allocated whole, so it never
    grows }
  TRALBodyStream = class(TStream)
  private
    FInner: TStream;
    FSpoolAbove: Int64RAL;
    procedure Promote(ANeeded: Int64RAL);
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AExpectedSize: Int64RAL = -1; ASpoolAbove: Int64RAL = 0);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(const NewSize: Int64); override;
    { Hands over the storage in use (TMemoryStream, TRALChunkedStream or
      TRALTempFileStream) and leaves this one empty: once a body is written,
      the param keeps the real storage, so a small body is still a plain
      TMemoryStream to whoever reads Content }
    function Detach: TStream;
    /// The storage in use right now
    property Inner: TStream read FInner;
  end;

  { TRALConcatStream }

  { Read-only stream made of several parts read one after the other, without
    joining them: the multipart of a response is its headers and the content
    of each param, and the engine reads it straight from here. Owns the parts
    handed with AOwned }
  TRALConcatStream = class(TStream)
  private
    FParts: TList;
    FOwned: TList;
    FSize: Int64RAL;
    FPos: Int64RAL;
  protected
    function GetSize: Int64; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AStream: TStream; AOwned: boolean); overload;
    procedure Add(const AText: RawByteString); overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  end;

var
  { Bodies up to this size stay one contiguous block (DEFAULTCHUNKABOVE) }
  RALChunkAbove: Int64RAL = DEFAULTCHUNKABOVE;
  { Usable size of each block of a chunked body (DEFAULTCHUNKSIZE) }
  RALChunkSize: IntegerRAL = DEFAULTCHUNKSIZE;
  { Folder of the spooled bodies; empty is the system temp folder }
  RALSpoolFolder: string = '';

/// A read-only window over AParent: a TRALMemoryView when the parent is
/// contiguous memory, a TRALStreamSlice otherwise
function RALStreamSlice(AParent: TStream; AStart, ASize: Int64RAL): TStream;
/// Copies ACount bytes of ASource, from where it is, to ADest in pieces of a
/// fixed buffer - TStream.CopyFrom on some compilers allocates as much as the
/// whole count
procedure RALCopyStream(ASource, ADest: TStream; ACount: Int64RAL);
/// Removes spooled bodies older than a day from RALSpoolFolder (a process
/// killed in the middle of a request leaves them behind)
procedure RALCleanSpoolFolder;
/// The folder spooled bodies go to
function RALSpoolPath: string;

/// Saves the stream into a file given the AFileName
procedure SaveStream(AStream: TStream; const AFileName: StringRAL);
/// Creates a TStream and write ABytes on it
function BytesToStream(ABytes: TBytes): TStream;
/// Converts a given AStream to TBytes
function StreamToBytes(AStream: TStream): TBytes;
// Converts a given AStream to an UTF8String
function StreamToString(AStream: TStream): StringRAL;
{ StreamToString, except that a TRALStringView over UTF-8 text hands its own
  string over - the reference, not a copy. What reads a body that an engine
  delivered as a string (mORMot2, fpHTTP, CGI) back as text }
function RALStreamText(AStream: TStream): StringRAL;
// Converts a given AStream to a byte string
function StreamToByteString(AStream: TStream): StringRAL;
// Creates a TStream and writes the given AStr into it
function StringToStreamUTF8(const AStr: StringRAL): TStream;
function StringToStream(const AStr: StringRAL): TStream;

implementation

{$IFNDEF FPC}
uses
  IOUtils;
{$ENDIF}

const
  { the pieces RALCopyStream and the promotions move at a time }
  cCopyBuffer = 65536;

procedure RaiseReadOnly;
begin
  raise EStreamError.Create(emStreamReadOnly);
end;

function RALStreamSlice(AParent: TStream; AStart, ASize: Int64RAL): TStream;
begin
  if AParent is TCustomMemoryStream then
    Result := TRALMemoryView.Create(PByte(TCustomMemoryStream(AParent).Memory) + AStart, ASize)
  else
    Result := TRALStreamSlice.Create(AParent, AStart, ASize);
end;

procedure RALCopyStream(ASource, ADest: TStream; ACount: Int64RAL);
var
  vBuf: array of Byte;
  vRead, vWant: IntegerRAL;
begin
  if ACount <= 0 then
    Exit;
  if ACount < cCopyBuffer then
    SetLength(vBuf, ACount)
  else
    SetLength(vBuf, cCopyBuffer);
  while ACount > 0 do
  begin
    vWant := Length(vBuf);
    if vWant > ACount then
      vWant := ACount;
    vRead := ASource.Read(vBuf[0], vWant);
    if vRead <= 0 then
      Break;
    ADest.WriteBuffer(vBuf[0], vRead);
    Dec(ACount, vRead);
  end;
end;

function RALSpoolPath: string;
begin
  Result := RALSpoolFolder;
  if Result = '' then
  {$IFDEF FPC}
    Result := GetTempDir;
  {$ELSE}
    Result := TPath.GetTempPath;
  {$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure RALCleanSpoolFolder;
var
  vRec: TSearchRec;
  vPath: string;
  vWhen: TDateTime;
begin
  vPath := RALSpoolPath;
  if FindFirst(vPath + 'ralspool_*.tmp', faAnyFile, vRec) <> 0 then
    Exit;
  try
    repeat
      {$IFDEF FPC}
      vWhen := FileDateToDateTime(vRec.Time);
      {$ELSE}
      vWhen := vRec.TimeStamp;
      {$ENDIF}
      { a day old: nothing alive spools for that long. A file in use cannot be
        deleted on Windows anyway; elsewhere the age is the protection }
      if Now - vWhen > 1 then
        DeleteFile(vPath + vRec.Name);
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
end;

{ TRALMemoryView }

constructor TRALMemoryView.Create(APointer: Pointer; ASize: Int64RAL);
begin
  inherited Create;
  SetPointer(APointer, ASize);
end;

function TRALMemoryView.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  RaiseReadOnly;
end;

procedure TRALMemoryView.SetSize(const NewSize: Int64);
begin
  RaiseReadOnly;
end;

{ TRALStringView }

constructor TRALStringView.Create(const AText: RawByteString);
begin
  FText := AText;
  inherited Create(Pointer(FText), Length(FText));
end;

{ TRALStreamSlice }

constructor TRALStreamSlice.Create(AParent: TStream; AStart, ASize: Int64RAL;
  AOwnsParent: boolean);
begin
  inherited Create;
  FParent := AParent;
  FStart := AStart;
  FSize := ASize;
  FPos := 0;
  FOwnsParent := AOwnsParent;
end;

destructor TRALStreamSlice.Destroy;
begin
  if FOwnsParent then
    FreeAndNil(FParent);
  inherited Destroy;
end;

function TRALStreamSlice.GetSize: Int64;
begin
  Result := FSize;
end;

function TRALStreamSlice.Read(var Buffer; Count: Longint): Longint;
var
  vLeft: Int64RAL;
begin
  Result := 0;
  vLeft := FSize - FPos;
  if (Count <= 0) or (vLeft <= 0) then
    Exit;
  if Count > vLeft then
    Count := vLeft;
  FParent.Position := FStart + FPos;
  Result := FParent.Read(Buffer, Count);
  Inc(FPos, Result);
end;

function TRALStreamSlice.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  RaiseReadOnly;
end;

function TRALStreamSlice.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPos := Offset;
    soCurrent: FPos := FPos + Offset;
    soEnd: FPos := FSize + Offset;
  end;
  if FPos < 0 then
    FPos := 0;
  Result := FPos;
end;

procedure TRALStreamSlice.SetSize(const NewSize: Int64);
begin
  RaiseReadOnly;
end;

{ TRALChunkedStream }

constructor TRALChunkedStream.Create(ABlockSize: IntegerRAL);
begin
  inherited Create;
  FBlocks := TList.Create;
  if ABlockSize <= 0 then
    ABlockSize := RALChunkSize;
  FBlockSize := ABlockSize;
  FSize := 0;
  FPos := 0;
  FReleased := 0;
end;

destructor TRALChunkedStream.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FBlocks.Count - 1 do
    if FBlocks[vInt] <> nil then
      FreeMem(FBlocks[vInt]);
  FreeAndNil(FBlocks);
  inherited Destroy;
end;

function TRALChunkedStream.Block(AIndex: IntegerRAL): PByte;
begin
  if AIndex < FReleased then
    raise EStreamError.Create(emStreamReleased);
  while AIndex >= FBlocks.Count do
    FBlocks.Add(GetMemory(FBlockSize));
  Result := FBlocks[AIndex];
end;

procedure TRALChunkedStream.ReleaseBefore(APosition: Int64RAL);
var
  vLast: IntegerRAL;
begin
  { the block that holds APosition is still needed }
  vLast := APosition div FBlockSize;
  if vLast > FBlocks.Count then
    vLast := FBlocks.Count;
  while FReleased < vLast do
  begin
    FreeMem(FBlocks[FReleased]);
    FBlocks[FReleased] := nil;
    Inc(FReleased);
  end;
end;

function TRALChunkedStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TRALChunkedStream.Read(var Buffer; Count: Longint): Longint;
var
  vDest: PByte;
  vOff, vPiece: Int64RAL;
begin
  Result := 0;
  vDest := @Buffer;
  while (Count > 0) and (FPos < FSize) do
  begin
    vOff := FPos mod FBlockSize;
    vPiece := FBlockSize - vOff;
    if vPiece > Count then
      vPiece := Count;
    if vPiece > FSize - FPos then
      vPiece := FSize - FPos;
    Move((Block(FPos div FBlockSize) + vOff)^, vDest^, vPiece);
    Inc(vDest, vPiece);
    Inc(FPos, vPiece);
    Inc(Result, vPiece);
    Dec(Count, vPiece);
  end;
  if FReleaseOnRead then
    ReleaseBefore(FPos);
end;

function TRALChunkedStream.Write(const Buffer; Count: Longint): Longint;
var
  vSrc: PByte;
  vOff, vPiece: Int64RAL;
begin
  Result := 0;
  vSrc := @Buffer;
  while Count > 0 do
  begin
    vOff := FPos mod FBlockSize;
    vPiece := FBlockSize - vOff;
    if vPiece > Count then
      vPiece := Count;
    Move(vSrc^, (Block(FPos div FBlockSize) + vOff)^, vPiece);
    Inc(vSrc, vPiece);
    Inc(FPos, vPiece);
    Inc(Result, vPiece);
    Dec(Count, vPiece);
  end;
  if FPos > FSize then
    FSize := FPos;
end;

function TRALChunkedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPos := Offset;
    soCurrent: FPos := FPos + Offset;
    soEnd: FPos := FSize + Offset;
  end;
  if FPos < 0 then
    FPos := 0;
  Result := FPos;
end;

procedure TRALChunkedStream.SetSize(const NewSize: Int64);
var
  vKeep: IntegerRAL;
begin
  if NewSize < 0 then
    Exit;
  { shrinking frees the blocks past the end; growing only allocates when
    something is written there }
  vKeep := (NewSize + FBlockSize - 1) div FBlockSize;
  while FBlocks.Count > vKeep do
  begin
    if FBlocks[FBlocks.Count - 1] <> nil then
      FreeMem(FBlocks[FBlocks.Count - 1]);
    FBlocks.Delete(FBlocks.Count - 1);
  end;
  if FReleased > FBlocks.Count then
    FReleased := FBlocks.Count;
  FSize := NewSize;
  if FPos > FSize then
    FPos := FSize;
end;

{ TRALTempFileStream }

constructor TRALTempFileStream.Create;
var
  vGUID: TGUID;
  vName: string;
begin
  CreateGUID(vGUID);
  vName := GUIDToString(vGUID);
  vName := Copy(vName, 2, Length(vName) - 2);
  FFileName := RALSpoolPath + 'ralspool_' + vName + '.tmp';
  inherited Create(FFileName, fmCreate);
end;

destructor TRALTempFileStream.Destroy;
begin
  inherited Destroy;
  if FFileName <> '' then
    DeleteFile(FFileName);
end;

{ TRALBodyStream }

type
  { TMemoryStream with its capacity in reach: a body whose size is known is
    allocated whole, once }
  TRALMemoryStream = class(TMemoryStream)
  public
    procedure Reserve(ASize: Int64RAL);
  end;

procedure TRALMemoryStream.Reserve(ASize: Int64RAL);
begin
  Capacity := ASize;
end;

constructor TRALBodyStream.Create(AExpectedSize: Int64RAL; ASpoolAbove: Int64RAL);
begin
  inherited Create;
  FSpoolAbove := ASpoolAbove;
  if (FSpoolAbove > 0) and (AExpectedSize > FSpoolAbove) then
    FInner := TRALTempFileStream.Create
  else if AExpectedSize > RALChunkAbove then
    FInner := TRALChunkedStream.Create
  else
  begin
    FInner := TRALMemoryStream.Create;
    if AExpectedSize > 0 then
      TRALMemoryStream(FInner).Reserve(AExpectedSize);
  end;
end;

destructor TRALBodyStream.Destroy;
begin
  FreeAndNil(FInner);
  inherited Destroy;
end;

function TRALBodyStream.Detach: TStream;
begin
  Result := FInner;
  FInner := TRALMemoryStream.Create;
  Result.Position := 0;
end;

procedure TRALBodyStream.Promote(ANeeded: Int64RAL);
var
  vNew: TStream;
  vPos: Int64RAL;
begin
  vNew := nil;
  if (FSpoolAbove > 0) and (ANeeded > FSpoolAbove) and
     not (FInner is TRALTempFileStream) then
    vNew := TRALTempFileStream.Create
  else if (ANeeded > RALChunkAbove) and (FInner is TMemoryStream) then
    vNew := TRALChunkedStream.Create;
  if vNew = nil then
    Exit;

  { one copy of what is held so far, bounded by the limit just crossed; from a
    chunked stream the blocks are given back as they are copied }
  try
    vPos := FInner.Position;
    FInner.Position := 0;
    if FInner is TRALChunkedStream then
      TRALChunkedStream(FInner).ReleaseOnRead := True;
    RALCopyStream(FInner, vNew, FInner.Size);
    vNew.Position := vPos;
  except
    vNew.Free;
    raise;
  end;
  FreeAndNil(FInner);
  FInner := vNew;
end;

function TRALBodyStream.GetSize: Int64;
begin
  Result := FInner.Size;
end;

function TRALBodyStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FInner.Read(Buffer, Count);
end;

function TRALBodyStream.Write(const Buffer; Count: Longint): Longint;
var
  vEnd: Int64RAL;
begin
  vEnd := FInner.Position + Count;
  if vEnd > FInner.Size then
    Promote(vEnd);
  Result := FInner.Write(Buffer, Count);
end;

function TRALBodyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FInner.Seek(Offset, Origin);
end;

procedure TRALBodyStream.SetSize(const NewSize: Int64);
begin
  if NewSize > FInner.Size then
    Promote(NewSize);
  FInner.Size := NewSize;
end;

{ TRALConcatStream }

constructor TRALConcatStream.Create;
begin
  inherited Create;
  FParts := TList.Create;
  FOwned := TList.Create;
  FSize := 0;
  FPos := 0;
end;

destructor TRALConcatStream.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FOwned.Count - 1 do
    TObject(FOwned[vInt]).Free;
  FreeAndNil(FOwned);
  FreeAndNil(FParts);
  inherited Destroy;
end;

procedure TRALConcatStream.Add(AStream: TStream; AOwned: boolean);
begin
  if AStream = nil then
    Exit;
  if AOwned then
    FOwned.Add(AStream);
  if AStream.Size = 0 then
    Exit;
  FParts.Add(AStream);
  Inc(FSize, AStream.Size);
end;

procedure TRALConcatStream.Add(const AText: RawByteString);
begin
  if AText <> '' then
    Add(TRALStringView.Create(AText), True);
end;

function TRALConcatStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TRALConcatStream.Read(var Buffer; Count: Longint): Longint;
var
  vDest: PByte;
  vInt: IntegerRAL;
  vStart, vPartSize, vOff, vPiece: Int64RAL;
  vPart: TStream;
begin
  Result := 0;
  vDest := @Buffer;
  vStart := 0;
  vInt := 0;
  while (Count > 0) and (vInt < FParts.Count) do
  begin
    vPart := TStream(FParts[vInt]);
    vPartSize := vPart.Size;
    if FPos < vStart + vPartSize then
    begin
      vOff := FPos - vStart;
      vPiece := vPartSize - vOff;
      if vPiece > Count then
        vPiece := Count;
      vPart.Position := vOff;
      vPiece := vPart.Read(vDest^, vPiece);
      if vPiece <= 0 then
        Break;
      Inc(vDest, vPiece);
      Inc(FPos, vPiece);
      Inc(Result, vPiece);
      Dec(Count, vPiece);
      { still inside this part when it returned less than asked }
      if FPos < vStart + vPartSize then
        Continue;
    end;
    Inc(vStart, vPartSize);
    Inc(vInt);
  end;
end;

function TRALConcatStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  RaiseReadOnly;
end;

function TRALConcatStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPos := Offset;
    soCurrent: FPos := FPos + Offset;
    soEnd: FPos := FSize + Offset;
  end;
  if FPos < 0 then
    FPos := 0;
  Result := FPos;
end;

procedure TRALConcatStream.SetSize(const NewSize: Int64);
begin
  RaiseReadOnly;
end;

function BytesToStream(ABytes: TBytes): TStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(ABytes[0], Length(ABytes));
  Result.Position := 0;
end;

function StreamToBytes(AStream: TStream): TBytes;
begin
  AStream.Position := 0;

  if AStream.InheritsFrom(TMemoryStream) then
  begin
    SetLength(Result, AStream.Size);
    Move(TMemoryStream(AStream).Memory^, Result[0], AStream.Size);
  end
  else
  begin
    SetLength(Result, AStream.Size);
    AStream.Read(Result[0], AStream.Size);
  end;
end;

procedure SaveStream(AStream: TStream; const AFileName: StringRAL);
var
  vFile: TFileStream;
begin
  AStream.Position := 0;

  vFile := TFileStream.Create(AFileName, fmCreate);
  try
    vFile.Size := AStream.Size;
    vFile.Position := 0;
    vFile.CopyFrom(AStream, AStream.Size);
  finally
    vFile.Free;
  end;
end;

function StringToStream(const AStr: StringRAL): TStream;
var
  vBytes : TBytes;
begin
  vBytes := StringToBytes(AStr);
  Result := BytesToStream(vBytes)
end;

function StringToStreamUTF8(const AStr: StringRAL): TStream;
begin
  Result := TRALStringStream.Create(AStr);
  Result.Position := 0;
end;

function StreamToByteString(AStream: TStream): StringRAL;
var
  vBytes: TBytes;
begin
  SetLength(vBytes, AStream.Size);
  AStream.Read(vBytes[0], AStream.Size);
  Result := BytesToString(vBytes)
end;

function RALStreamText(AStream: TStream): StringRAL;
begin
  { only a UTF-8 string is handed over as it is: assigning one in any other
    code page to a StringRAL would convert it, on both compilers }
  if (AStream is TRALStringView) and
     (StringCodePage(TRALStringView(AStream).Text) = 65001) then
    Result := StringRAL(TRALStringView(AStream).Text)
  else
    Result := StreamToString(AStream);
end;

function StreamToString(AStream: TStream): StringRAL;
var
  vSize, vDone: Int64RAL;
  vRead: IntegerRAL;
begin
  { straight into the string: it went through a TBytes first, and then
    BytesToStringUTF8 copied that again - two copies of the whole body to read
    it as text, every time. The bytes are UTF-8 already (StringRAL), so
    nothing needs converting. A TStringStream used to answer DataString, which
    on Delphi decodes with the ANSI code page }
  Result := '';
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  AStream.Position := 0;
  vSize := AStream.Size;
  SetLength(Result, vSize);
  vDone := 0;
  while vDone < vSize do
  begin
    vRead := AStream.Read((PAnsiChar(Pointer(Result)) + vDone)^, vSize - vDone);
    if vRead <= 0 then
      Break;
    Inc(vDone, vRead);
  end;
  if vDone < vSize then
    SetLength(Result, vDone);
end;

{ TRALBinaryWriter }

function TRALBinaryWriter.ReadSize: UInt64RAL;
var
  vMult: IntegerRAL;
  vByte: Byte;
begin
  Result := 0;
  vMult := 0;

  repeat
    if FStream.Position = FStream.Size then
      Exit;

    vByte := ReadByte;
    { widened before the shift: "(vByte and 127) shl 28" is 32-bit arithmetic
      on both compilers, so every size from 2 GB up wrapped to garbage }
    Result := Result + (UInt64RAL(vByte and 127) shl vMult);
    vMult := vMult + 7;
  until (vByte and 128) = 0;
end;

procedure TRALBinaryWriter.WriteSize(ASize: UInt64RAL);
var
  vByte: Byte;
begin
  while ASize >= 0 do begin
    vByte := ASize and 127;
    ASize := ASize shr 7;
    if ASize > 0 then
      vByte := vByte or 128;

    WriteByte(vByte);

    if ASize = 0 then
      Break;
  end;
end;

constructor TRALBinaryWriter.Create(const AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TRALBinaryWriter.WriteShortint(AValue: Shortint);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteByte(AValue: Byte);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteSmallint(AValue: Smallint);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteWord(AValue: Word);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteInteger(AValue: IntegerRAL);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteLongWord(AValue: LongWord);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteInt64(AValue: Int64RAL);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteQWord(AValue: UInt64);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteBoolean(AValue: Boolean);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteFloat(AValue: Double);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteDateTime(AValue: TDateTime);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteStream(AValue: TStream);
begin
  AValue.Position := 0;

  WriteSize(AValue.Size);
  FStream.CopyFrom(AValue, AValue.Size);
end;

procedure TRALBinaryWriter.WriteBytes(AValue: TBytes);
begin
  WriteSize(Length(AValue));
  if Length(AValue) > 0 then
    FStream.Write(AValue[0], Length(AValue));
end;

procedure TRALBinaryWriter.WriteString(AValue: StringRAL);
var
  vBytes : TBytes;
begin
  vBytes := StringToBytesUTF8(AValue);
  WriteSize(Length(vBytes));
  if Length(vBytes) > 0 then
    FStream.Write(vBytes[0], Length(vBytes));
end;

procedure TRALBinaryWriter.WriteBytesDirect(AValue: TBytes);
begin
  FStream.Write(AValue[0], Length(AValue));
end;

procedure TRALBinaryWriter.WriteChar(AValue: CharRAL);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

function TRALBinaryWriter.ReadShortint: Shortint;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadByte: Byte;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadSmallint: Smallint;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadWord: Word;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInteger: IntegerRAL;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadLongWord: LongWord;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInt64: Int64RAL;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadQWord: UInt64;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadBoolean: Boolean;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadFloat: Double;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadDateTime: TDateTime;
begin
  FStream.Read(Result, SizeOf(Result));
end;

{ the size prefix is a varint: nine bytes can announce 2^63 bytes. Allocating
  or copying what it says before checking what the stream still holds let a
  twenty-byte packet ask for gigabytes - out of memory on the server }
procedure TRALBinaryWriter.CheckSize(ASize: UInt64RAL);
begin
  if ASize > UInt64RAL(FStream.Size - FStream.Position) then
    raise Exception.Create(emStreamSizeBeyondEnd);
end;

procedure TRALBinaryWriter.ReadStream(AStream: TStream);
var
  vQWord : UInt64RAL;
begin
  vQWord := ReadSize;
  CheckSize(vQWord);
  AStream.CopyFrom(FStream, vQWord);
  AStream.Position := 0;
end;

function TRALBinaryWriter.ReadBytes: TBytes;
var
  vQWord: UInt64RAL;
begin
  vQWord := ReadSize;
  CheckSize(vQWord);
  SetLength(Result, vQWord);
  if vQWord > 0 then
    FStream.Read(Result[0], vQWord);
end;

function TRALBinaryWriter.ReadString: StringRAL;
var
  vQWord : UInt64RAL;
  vBytes : TBytes;
begin
  vQWord := ReadSize;
  CheckSize(vQWord);
  Result := '';
  if vQWord > 0 then
  begin
    SetLength(vBytes, vQWord);
    FStream.Read(vBytes[0], vQWord);
    Result := BytesToStringUTF8(vBytes);
  end;
end;

function TRALBinaryWriter.ReadBytesDirect(ALength: integer): TBytes;
begin
  SetLength(Result, ALength);
  FStream.Read(Result[0], ALength);
end;

function TRALBinaryWriter.ReadChar: CharRAL;
begin
  FStream.Read(Result, SizeOf(Result));
end;

{ TRALStringStream }

constructor TRALStringStream.Create(ABytes: TBytes);
begin
  inherited Create;
  WriteBytes(ABytes);
end;

constructor TRALStringStream.Create(AString: StringRAL);
var
  vBytes: TBytes;
begin
  inherited Create;
  WriteString(AString);
end;

constructor TRALStringStream.Create(AStream: TStream);
var
  vStream: TStringStream;
  vBytes: TBytes;
begin
  inherited Create;
  WriteStream(AStream);
end;

function TRALStringStream.DataString: StringRAL;
begin
  { one copy, from the memory straight into the string }
  SetString(Result, PAnsiChar(Memory), Size);
end;

procedure TRALStringStream.WriteBytes(ABytes: TBytes);
begin
  if Length(ABytes) > 0 then
    Write(ABytes[0], Length(ABytes));
end;

procedure TRALStringStream.WriteString(AString: StringRAL);
begin
  { from the string's memory: through a TBytes it was two copies, and
    AString[1] would make the string unique first - a third one when it is
    shared }
  if AString <> '' then
    WriteBuffer(Pointer(AString)^, Length(AString));
end;

procedure TRALStringStream.WriteStream(AStream: TStream);
begin
  { in pieces, not through a TBytes of the whole stream }
  AStream.Position := 0;
  RALCopyStream(AStream, Self, AStream.Size);
end;

end.
