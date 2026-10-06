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
    /// Reads exactly ACount bytes or raises - see there
    procedure ReadExact(var ABuffer; ACount: IntegerRAL);
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
    /// A byte that becomes an enum whose last member is AHigh, refused with
    /// emStorageInvalidBinary past it: cast first, a case over the value took
    /// a member's branch on Delphi Win32 and jumped out of its table on FPC
    function ReadEnum(AHigh: Byte): Byte;
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

  /// The capacity argument of TMemoryStream.Realloc, which changed type
  TRALStreamCapacity = {$IFDEF FPC}PtrInt{$ELSE}{$IFDEF DELPHI11UP}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF};

  { TRALMemoryStream }

  /// A TMemoryStream that holds a small content in a small block. Delphi
  /// rounds every capacity up to 8 KB and FPC to 4 KB, so a stream of a few
  /// bytes - a segment of a JWT, an HMAC, a short answer - took a block of
  /// that size: on Delphi a medium block, which FastMM serves under a single
  /// lock for every thread of the process. Below 8 KB the capacity is the
  /// size rounded to 64 bytes, growing by half at least; from 8 KB on it is
  /// the RTL's own
  TRALMemoryStream = class(TMemoryStream)
  protected
    function Realloc(var NewCapacity: TRALStreamCapacity): Pointer; override;
  public
    /// The capacity for ASize bytes, at once: a body whose size is known is
    /// allocated whole
    procedure Reserve(ASize: Int64RAL);
  end;

  TRALStringStream = class(TRALMemoryStream)
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

  { TRALFileStream }

  /// A file, or a window of one, read to be sent. It is opened with
  /// fmShareDenyNone - a file being served does not stop a new version from
  /// being published - and only when first read, unless AOpenNow. A window
  /// running past the end of the file is cut to what the file has when it is
  /// opened. Read-only: Write writes nothing
  TRALFileStream = class(TStream)
  private
    FFile: TFileStream;
    FFileName: string;
    FOffset: Int64;
    { the window as asked for, -1 to the end of the file, and as it is once
      the file is open }
    FAsked: Int64;
    FCount: Int64;
    FPosition: Int64;
    { where the file handle stands, so a sequential read seeks nothing; -1
      when unknown }
    FFilePos: Int64;
    procedure NeedFile;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(const AFileName: string; AOffset: Int64 = 0;
      ACount: Int64 = -1; AOpenNow: boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    /// Another stream over the same file and window, not open yet - what a
    /// param keeps when it hands this one over to be sent
    function Twin: TRALFileStream;
    /// The whole file, not a window of it
    function IsWholeFile: boolean;
    property FileName: string read FFileName;
  end;

  { TRALBufferStream }

  /// Read-only bytes kept alive by reference: a string, or a memory stream
  /// shared by reference count. Twin gives another stream over the same bytes -
  /// its own position, freed on its own - without copying them, which is how
  /// a body reaches the engine that frees what it sends while the param still
  /// holds it. Write writes nothing
  TRALBufferStream = class(TCustomMemoryStream)
  private
    FText: StringRAL;
    FKeeper: IInterface;
  public
    /// Over AText, which is not copied: the stream keeps a reference to it
    constructor Create(const AText: StringRAL); overload;
    /// Over AStream, which it TAKES: it is freed with the last stream sharing it
    constructor Create(AStream: TCustomMemoryStream); overload;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Twin: TRALBufferStream;
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

{ TRALMemoryStream }

const
  { below this, the capacity is RAL's: the size the RTLs round up to }
  cRALSmallStream = 8192;

function TRALMemoryStream.Realloc(var NewCapacity: TRALStreamCapacity): Pointer;
begin
  if NewCapacity > 0 then
  begin
    { growing, by half at least: a stream written a little at a time would
      otherwise be copied on every write }
    if (NewCapacity > Capacity) and (NewCapacity < Capacity + Capacity div 2) then
      NewCapacity := Capacity + Capacity div 2;
    if NewCapacity < cRALSmallStream then
    begin
      NewCapacity := (NewCapacity + 63) and not 63;
      Result := Memory;
      { GetMem and ReallocMem raise EOutOfMemory themselves }
      if NewCapacity <> Capacity then
      begin
        if Result = nil then
          GetMem(Result, NewCapacity)
        else
          ReallocMem(Result, NewCapacity);
      end;
      Exit;
    end;
  end;
  Result := inherited Realloc(NewCapacity);
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
{ Every [0] below is guarded by a length: on an empty array it is past the end,
  and $R+ refuses it even for a count of 0. Built that way, an empty POST - which
  Indy hands over as an empty stream - failed in here, and Indy answered its own
  "200 OK" page to every one of them }

function BytesToStream(ABytes: TBytes): TStream;
begin
  Result := TRALMemoryStream.Create;
  if Length(ABytes) > 0 then
    Result.Write(ABytes[0], Length(ABytes));
  Result.Position := 0;
end;

function StreamToBytes(AStream: TStream): TBytes;
begin
  AStream.Position := 0;
  SetLength(Result, AStream.Size);
  if Length(Result) = 0 then
    Exit;

  if AStream.InheritsFrom(TCustomMemoryStream) then
    Move(TCustomMemoryStream(AStream).Memory^, Result[0], Length(Result))
  else
    AStream.Read(Result[0], Length(Result));
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
  if Length(vBytes) > 0 then
    AStream.Read(vBytes[0], Length(vBytes));
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

  { a stream that ends before the size does is malformed, and ReadByte now
    says so: this used to answer whatever it had read so far - 0 at the very
    end - so a count read off the network could drive a caller's loop through
    billions of empty strings past the end of a twenty-byte body }
  repeat
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
  if Length(AValue) > 0 then
    FStream.Write(AValue[0], Length(AValue));
end;

procedure TRALBinaryWriter.WriteChar(AValue: CharRAL);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

function TRALBinaryWriter.ReadShortint: Shortint;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadByte: Byte;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadEnum(AHigh: Byte): Byte;
begin
  Result := ReadByte;
  if Result > AHigh then
    raise Exception.Create(emStorageInvalidBinary);
end;

function TRALBinaryWriter.ReadSmallint: Smallint;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadWord: Word;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInteger: IntegerRAL;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadLongWord: LongWord;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInt64: Int64RAL;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadQWord: UInt64;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadBoolean: Boolean;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadFloat: Double;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadDateTime: TDateTime;
begin
  ReadExact(Result, SizeOf(Result));
end;

{ Every fixed-size read goes through here. Stream.Read returns how much it
  actually read, and nothing looked: at the end of the stream the value came
  back as whatever the stack held - and these values come off the network,
  where one of them was cast straight to an enum that indexes a table. }
procedure TRALBinaryWriter.ReadExact(var ABuffer; ACount: IntegerRAL);
begin
  if FStream.Read(ABuffer, ACount) <> ACount then
    raise Exception.Create(emStreamSizeBeyondEnd);
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
  { nothing to copy is nothing: CopyFrom reads a count of 0 as "all of it,
    from the start", on both compilers, so an empty stream in the data copied
    the whole of FStream in its place - this field, the ones after it and the
    ones before - and every read after it ran past the end }
  if vQWord > 0 then
  begin
    { the whole size at once instead of growing step by step with the copy }
    if AStream is TMemoryStream then
      AStream.Size := AStream.Position + Int64(vQWord);
    AStream.CopyFrom(FStream, vQWord);
  end;
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
  if ALength > 0 then
    ReadExact(Result[0], ALength);
end;

function TRALBinaryWriter.ReadChar: CharRAL;
begin
  ReadExact(Result, SizeOf(Result));
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

{ a count of 0 copies the whole of AStream from its start, on both compilers -
  straight across, where a copy into an array first doubled the body in memory }
procedure TRALStringStream.WriteStream(AStream: TStream);
var
  vNeeded: Int64;
begin
  { room for all of it at once: the copy grew the buffer piece by piece, by
    8 KB at a time on Delphi, and each step could move the whole of it }
  vNeeded := Position + AStream.Size;
  if vNeeded > Capacity then
    Capacity := vNeeded;
  { in pieces, not through a TBytes of the whole stream }
  AStream.Position := 0;
  RALCopyStream(AStream, Self, AStream.Size);
end;

{ TRALFileStream }

constructor TRALFileStream.Create(const AFileName: string; AOffset: Int64;
  ACount: Int64; AOpenNow: boolean);
begin
  inherited Create;
  FFileName := AFileName;
  if AOffset < 0 then
    AOffset := 0;
  FOffset := AOffset;
  FAsked := ACount;
  FCount := ACount;
  FFilePos := -1;
  if AOpenNow then
    NeedFile;
end;

destructor TRALFileStream.Destroy;
begin
  FreeAndNil(FFile);
  inherited;
end;

procedure TRALFileStream.NeedFile;
var
  vLeft: Int64;
begin
  if FFile <> nil then
    Exit;
  FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  vLeft := FFile.Size - FOffset;
  if vLeft < 0 then
    vLeft := 0;
  if (FAsked < 0) or (FAsked > vLeft) then
    FCount := vLeft
  else
    FCount := FAsked;
end;

function TRALFileStream.GetSize: Int64;
begin
  NeedFile;
  Result := FCount;
end;

function TRALFileStream.Read(var Buffer; Count: Longint): Longint;
var
  vLeft: Int64;
begin
  Result := 0;
  NeedFile;
  vLeft := FCount - FPosition;
  if (Count <= 0) or (vLeft <= 0) then
    Exit;
  if Count > vLeft then
    Count := vLeft;
  if FFilePos <> FOffset + FPosition then
    FFilePos := FFile.Seek(FOffset + FPosition, soBeginning);
  Result := FFile.Read(Buffer, Count);
  if Result > 0 then
  begin
    Inc(FPosition, Result);
    Inc(FFilePos, Result);
  end;
end;

function TRALFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0; // read-only: WriteBuffer turns this into its EWriteError
end;

function TRALFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  { Position, and Seek(0, soCurrent) under it, opens nothing: only the end
    of the window needs the file }
  case Origin of
    soBeginning:
      FPosition := Offset;
    soCurrent:
      FPosition := FPosition + Offset;
    soEnd:
    begin
      NeedFile;
      FPosition := FCount + Offset;
    end;
  end;
  if FPosition < 0 then
    FPosition := 0;
  Result := FPosition;
end;

function TRALFileStream.Twin: TRALFileStream;
begin
  { the window as it was asked for: a whole file is measured again when the
    twin opens it }
  Result := TRALFileStream.Create(FFileName, FOffset, FAsked);
end;

function TRALFileStream.IsWholeFile: boolean;
begin
  Result := (FOffset = 0) and (FAsked < 0);
end;

{ TRALBufferStream }

type
  { what keeps a shared memory stream alive: freed - and the stream with it -
    when the last TRALBufferStream over it lets go }
  TRALStreamKeeper = class(TInterfacedObject)
  private
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
  end;

constructor TRALStreamKeeper.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

destructor TRALStreamKeeper.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

constructor TRALBufferStream.Create(const AText: StringRAL);
begin
  inherited Create;
  { a reference, not a copy: the string is copied on write by whoever else
    holds it, never under this stream }
  FText := AText;
  SetPointer(Pointer(FText), Length(FText));
end;

constructor TRALBufferStream.Create(AStream: TCustomMemoryStream);
begin
  inherited Create;
  FKeeper := TRALStreamKeeper.Create(AStream);
  SetPointer(AStream.Memory, AStream.Size);
end;

function TRALBufferStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0; // the bytes are shared: WriteBuffer turns this into its EWriteError
end;

function TRALBufferStream.Twin: TRALBufferStream;
begin
  Result := TRALBufferStream.Create(FText);
  Result.FKeeper := FKeeper;
  Result.SetPointer(Memory, Size);
end;

end.
