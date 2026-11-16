(* The GraphQL language (specification of October 2021): the lexer, the syntax
  tree and the parser of executable documents (operations and fragments) and of
  the type system language (SDL: schema, scalar, type, interface, union, enum,
  input, and their extend forms; directive definitions are read and ignored).
  Text is UTF-8 (StringRAL); positions are 1-based line and column. *)
unit RALGraphQLAST;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, Contnrs,
  RALTypes;

type
  /// A syntax error, with where it happened.
  ERALGraphQLSyntax = class(Exception)
  private
    FColumn: IntegerRAL;
    FLine: IntegerRAL;
  public
    /// The error AMessage at ALine, AColumn.
    constructor CreatePos(const AMessage: string; ALine, AColumn: IntegerRAL);

    /// Column of the error.
    property Column: IntegerRAL read FColumn;
    /// Line of the error.
    property Line: IntegerRAL read FLine;
  end;

  /// Kinds of a value literal.
  TRALGQLValueKind = (gvVariable, gvInt, gvFloat, gvString, gvBoolean, gvNull, gvEnum,
    gvList, gvObject);

  /// A node with its position in the text.
  TRALGQLNode = class
  public
    /// Column where the node starts.
    Column: IntegerRAL;
    /// Line where the node starts.
    Line: IntegerRAL;
  end;

  /// A value literal (or a variable).
  TRALGQLValue = class(TRALGQLNode)
  public
    /// The items of a list, or the fields of an object (TRALGQLObjectField).
    Items: TObjectList;
    /// What it is.
    Kind: TRALGQLValueKind;
    { The text: the number as written, the string decoded, 'true'/'false', the
      enum name, the variable name (without $) }
    Text: StringRAL;

    constructor Create(AKind: TRALGQLValueKind);
    destructor Destroy; override;
  end;

  /// One field of an object value.
  TRALGQLObjectField = class(TRALGQLNode)
  public
    /// Field name.
    Name: StringRAL;
    /// Its value (owned).
    Value: TRALGQLValue;

    destructor Destroy; override;
  end;

  /// A name and a value: an argument of a field or of a directive.
  TRALGQLArgument = class(TRALGQLNode)
  public
    /// Argument name.
    Name: StringRAL;
    /// Its value (owned).
    Value: TRALGQLValue;

    destructor Destroy; override;
  end;

  /// A directive applied somewhere: @name(args).
  TRALGQLDirective = class(TRALGQLNode)
  public
    /// The arguments (TRALGQLArgument).
    Args: TObjectList;
    /// Directive name, without @.
    Name: StringRAL;

    constructor Create;
    destructor Destroy; override;
    /// The argument AName, or nil.
    function Arg(const AName: StringRAL): TRALGQLArgument;
  end;

  /// Kinds of a type reference.
  TRALGQLTypeRefKind = (gtNamed, gtList, gtNonNull);

  /// A type as written: Name, [Type] or Type!.
  TRALGQLTypeRef = class(TRALGQLNode)
  public
    /// What it is.
    Kind: TRALGQLTypeRefKind;
    /// The named type (gtNamed).
    Name: StringRAL;
    /// The wrapped type (gtList, gtNonNull; owned).
    OfType: TRALGQLTypeRef;

    destructor Destroy; override;
    /// The named type at the bottom.
    function NamedType: StringRAL;
    /// The type as GraphQL writes it ('[User!]!').
    function ToText: StringRAL;
  end;

  /// Kinds of a selection.
  TRALGQLSelectionKind = (gsField, gsFragmentSpread, gsInlineFragment);

  /// A field, a fragment spread or an inline fragment.
  TRALGQLSelection = class(TRALGQLNode)
  public
    /// Response name when it differs from Name; empty otherwise.
    Alias: StringRAL;
    /// Arguments of a field (TRALGQLArgument).
    Args: TObjectList;
    /// Directives (TRALGQLDirective).
    Directives: TObjectList;
    /// What it is.
    Kind: TRALGQLSelectionKind;
    /// Field name, or the fragment name of a spread.
    Name: StringRAL;
    /// Sub-selections (TRALGQLSelection) of a field or an inline fragment.
    Selections: TObjectList;
    /// Type condition of an inline fragment; empty is none.
    TypeCondition: StringRAL;

    constructor Create(AKind: TRALGQLSelectionKind);
    destructor Destroy; override;
    /// The argument AName, or nil.
    function Arg(const AName: StringRAL): TRALGQLArgument;
    /// The response key: the alias, else the name.
    function ResponseKey: StringRAL;
  end;

  /// A variable an operation declares.
  TRALGQLVariableDef = class(TRALGQLNode)
  public
    /// Default value (owned); nil when none.
    DefaultValue: TRALGQLValue;
    /// Variable name, without $.
    Name: StringRAL;
    /// Its type (owned).
    TypeRef: TRALGQLTypeRef;

    destructor Destroy; override;
  end;

  /// Kinds of an operation.
  TRALGQLOperationType = (goQuery, goMutation, goSubscription);

  /// An operation (query, mutation, subscription) or a fragment definition.
  TRALGQLExecutable = class(TRALGQLNode)
  public
    /// Directives (TRALGQLDirective).
    Directives: TObjectList;
    /// Is a fragment definition.
    IsFragment: boolean;
    /// Name; empty for an anonymous operation.
    Name: StringRAL;
    /// Operation type (operations).
    Operation: TRALGQLOperationType;
    /// The selection set (TRALGQLSelection).
    Selections: TObjectList;
    /// Type condition (fragments).
    TypeCondition: StringRAL;
    /// Variables (TRALGQLVariableDef) of an operation.
    Variables: TObjectList;

    constructor Create;
    destructor Destroy; override;
  end;

  /// An argument, an input field or a field definition of the type system.
  TRALGQLFieldDef = class(TRALGQLNode)
  public
    /// Arguments of a field (TRALGQLFieldDef, as input values).
    Args: TObjectList;
    /// Default value of an input value (owned); nil when none.
    DefaultValue: TRALGQLValue;
    /// Reason of @deprecated; empty when not deprecated.
    DeprecationReason: StringRAL;
    /// Is @deprecated.
    Deprecated: boolean;
    /// The description written before it.
    Description: StringRAL;
    /// Name.
    Name: StringRAL;
    /// Its type (owned).
    TypeRef: TRALGQLTypeRef;

    constructor Create;
    destructor Destroy; override;
    /// The argument AName, or nil.
    function Arg(const AName: StringRAL): TRALGQLFieldDef;
  end;

  /// Kinds of a type definition.
  TRALGQLTypeKind = (gkScalar, gkObject, gkInterface, gkUnion, gkEnum, gkInputObject);

  /// A type definition of the SDL.
  TRALGQLTypeDef = class(TRALGQLNode)
  public
    /// The description written before it.
    Description: StringRAL;
    /// Enum values (TRALGQLFieldDef: name, description, deprecation).
    EnumValues: TObjectList;
    /// Fields of an object or interface, input fields of an input (TRALGQLFieldDef).
    Fields: TObjectList;
    /// Interfaces it implements.
    Interfaces: TStringList;
    /// An extension (extend type ...): the schema adds it to the type of its name.
    IsExtension: boolean;
    /// What it is.
    Kind: TRALGQLTypeKind;
    /// Type name.
    Name: StringRAL;
    /// Member types of a union.
    PossibleTypes: TStringList;
    /// URL of @specifiedBy of a scalar.
    SpecifiedByURL: StringRAL;

    constructor Create(AKind: TRALGQLTypeKind);
    destructor Destroy; override;
    /// The field (or input field, or enum value) AName, or nil.
    function Field(const AName: StringRAL): TRALGQLFieldDef;
  end;

  { A parsed document: the executable definitions and the type system
    definitions it had }
  TRALGQLDocument = class
  public
    /// Operations and fragments (TRALGQLExecutable).
    Definitions: TObjectList;
    /// Root operation types of a schema definition: query, mutation, subscription.
    MutationType, QueryType, SubscriptionType: StringRAL;
    /// Type definitions (TRALGQLTypeDef).
    Types: TObjectList;

    constructor Create;
    destructor Destroy; override;
    /// The fragment AName, or nil.
    function Fragment(const AName: StringRAL): TRALGQLExecutable;
  end;

{ Parses ASource. AExecutable accepts operations and fragments, ASchema type
  system definitions; the other kind raises. Raises ERALGraphQLSyntax }
function RALGraphQLParse(const ASource: StringRAL; AExecutable, ASchema: boolean): TRALGQLDocument;
/// A string as a GraphQL (and JSON) string literal, with its quotes.
function RALGraphQLQuote(const AText: StringRAL): StringRAL;

implementation

type
  /// Token kinds.
  TRALGQLTokenKind = (tkEOF, tkPunct, tkName, tkInt, tkFloat, tkString, tkBlockString);

  /// The lexer and the parser.
  TRALGQLParser = class
  private
    FExecutable: boolean;
    /// Start of the current line (byte index), for columns.
    FLineStart: IntegerRAL;
    FLine: IntegerRAL;
    FPos: IntegerRAL;
    FSchema: boolean;
    FSource: StringRAL;
    /// The current token.
    FKind: TRALGQLTokenKind;
    FText: StringRAL;
    FTokLine, FTokCol: IntegerRAL;

    procedure Error(const AMessage: string);
    procedure Next;
    procedure ReadBlockString;
    procedure ReadNumber;
    procedure ReadString;
    procedure SkipIgnored;

    function Expect(const APunct: StringRAL): boolean;
    function ExpectName: StringRAL;
    function ExpectKeyword(const AWord: StringRAL): boolean;
    function IsPunct(const APunct: StringRAL): boolean;
    function IsKeyword(const AWord: StringRAL): boolean;
    function Skip(const APunct: StringRAL): boolean;

    function ParseArguments(AConst: boolean): TObjectList;
    function ParseDescription: StringRAL;
    function ParseDirectives(AConst: boolean): TObjectList;
    function ParseExecutable: TRALGQLExecutable;
    function ParseFieldDef(AInput: boolean): TRALGQLFieldDef;
    function ParseFieldDefs(AInput: boolean; const AOpen, AClose: StringRAL): TObjectList;
    function ParseSelection: TRALGQLSelection;
    function ParseSelectionSet: TObjectList;
    function ParseType: TRALGQLTypeRef;
    procedure ParseTypeSystem(ADocument: TRALGQLDocument; const ADescription: StringRAL);
    function ParseValue(AConst: boolean): TRALGQLValue;
    function ParseVariableDefs: TObjectList;
  public
    constructor Create(const ASource: StringRAL);
    function Parse: TRALGQLDocument;
  end;

{ Appends the byte AChar to AText as it is: on Delphi, UTF8String + AnsiChar
  goes through the ANSI code page and encodes a byte above 127 twice }
procedure AppendByte(var AText: StringRAL; AChar: AnsiChar);
begin
  SetLength(AText, Length(AText) + 1);
  AText[Length(AText)] := AChar;
end;

function RALGraphQLQuote(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := '"';
  for vInt := 1 to Length(AText) do
  begin
    vChar := AnsiChar(AText[vInt]);
    case vChar of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      #0..#7, #11, #14..#31:
        Result := Result + StringRAL('\u' + IntToHex(Ord(vChar), 4));
    else
      AppendByte(Result, AnsiChar(AText[vInt]));
    end;
  end;
  Result := Result + '"';
end;

{ ERALGraphQLSyntax }

constructor ERALGraphQLSyntax.CreatePos(const AMessage: string; ALine, AColumn: IntegerRAL);
begin
  inherited Create(AMessage);
  FLine := ALine;
  FColumn := AColumn;
end;

{ TRALGQLValue }

constructor TRALGQLValue.Create(AKind: TRALGQLValueKind);
begin
  inherited Create;
  Kind := AKind;
  if AKind in [gvList, gvObject] then
    Items := TObjectList.Create(True);
end;

destructor TRALGQLValue.Destroy;
begin
  FreeAndNil(Items);
  inherited Destroy;
end;

{ TRALGQLObjectField }

destructor TRALGQLObjectField.Destroy;
begin
  FreeAndNil(Value);
  inherited Destroy;
end;

{ TRALGQLArgument }

destructor TRALGQLArgument.Destroy;
begin
  FreeAndNil(Value);
  inherited Destroy;
end;

{ TRALGQLDirective }

constructor TRALGQLDirective.Create;
begin
  inherited Create;
  Args := TObjectList.Create(True);
end;

destructor TRALGQLDirective.Destroy;
begin
  FreeAndNil(Args);
  inherited Destroy;
end;

function TRALGQLDirective.Arg(const AName: StringRAL): TRALGQLArgument;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Args.Count - 1 do
    if TRALGQLArgument(Args[vInt]).Name = AName then
    begin
      Result := TRALGQLArgument(Args[vInt]);
      Exit;
    end;
end;

{ TRALGQLTypeRef }

destructor TRALGQLTypeRef.Destroy;
begin
  FreeAndNil(OfType);
  inherited Destroy;
end;

function TRALGQLTypeRef.NamedType: StringRAL;
begin
  if Kind = gtNamed then
    Result := Name
  else
    Result := OfType.NamedType;
end;

function TRALGQLTypeRef.ToText: StringRAL;
begin
  case Kind of
    gtList: Result := '[' + OfType.ToText + ']';
    gtNonNull: Result := OfType.ToText + '!';
  else
    Result := Name;
  end;
end;

{ TRALGQLSelection }

constructor TRALGQLSelection.Create(AKind: TRALGQLSelectionKind);
begin
  inherited Create;
  Kind := AKind;
  Args := TObjectList.Create(True);
  Directives := TObjectList.Create(True);
  Selections := TObjectList.Create(True);
end;

destructor TRALGQLSelection.Destroy;
begin
  FreeAndNil(Args);
  FreeAndNil(Directives);
  FreeAndNil(Selections);
  inherited Destroy;
end;

function TRALGQLSelection.Arg(const AName: StringRAL): TRALGQLArgument;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Args.Count - 1 do
    if TRALGQLArgument(Args[vInt]).Name = AName then
    begin
      Result := TRALGQLArgument(Args[vInt]);
      Exit;
    end;
end;

function TRALGQLSelection.ResponseKey: StringRAL;
begin
  if Alias <> '' then
    Result := Alias
  else
    Result := Name;
end;

{ TRALGQLVariableDef }

destructor TRALGQLVariableDef.Destroy;
begin
  FreeAndNil(DefaultValue);
  FreeAndNil(TypeRef);
  inherited Destroy;
end;

{ TRALGQLExecutable }

constructor TRALGQLExecutable.Create;
begin
  inherited Create;
  Directives := TObjectList.Create(True);
  Selections := TObjectList.Create(True);
  Variables := TObjectList.Create(True);
end;

destructor TRALGQLExecutable.Destroy;
begin
  FreeAndNil(Directives);
  FreeAndNil(Selections);
  FreeAndNil(Variables);
  inherited Destroy;
end;

{ TRALGQLFieldDef }

constructor TRALGQLFieldDef.Create;
begin
  inherited Create;
  Args := TObjectList.Create(True);
end;

destructor TRALGQLFieldDef.Destroy;
begin
  FreeAndNil(Args);
  FreeAndNil(DefaultValue);
  FreeAndNil(TypeRef);
  inherited Destroy;
end;

function TRALGQLFieldDef.Arg(const AName: StringRAL): TRALGQLFieldDef;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Args.Count - 1 do
    if TRALGQLFieldDef(Args[vInt]).Name = AName then
    begin
      Result := TRALGQLFieldDef(Args[vInt]);
      Exit;
    end;
end;

{ TRALGQLTypeDef }

constructor TRALGQLTypeDef.Create(AKind: TRALGQLTypeKind);
begin
  inherited Create;
  Kind := AKind;
  EnumValues := TObjectList.Create(True);
  Fields := TObjectList.Create(True);
  Interfaces := TStringList.Create;
  PossibleTypes := TStringList.Create;
end;

destructor TRALGQLTypeDef.Destroy;
begin
  FreeAndNil(EnumValues);
  FreeAndNil(Fields);
  FreeAndNil(Interfaces);
  FreeAndNil(PossibleTypes);
  inherited Destroy;
end;

function TRALGQLTypeDef.Field(const AName: StringRAL): TRALGQLFieldDef;
var
  vInt: IntegerRAL;
  vList: TObjectList;
begin
  Result := nil;
  if Kind = gkEnum then
    vList := EnumValues
  else
    vList := Fields;
  for vInt := 0 to vList.Count - 1 do
    if TRALGQLFieldDef(vList[vInt]).Name = AName then
    begin
      Result := TRALGQLFieldDef(vList[vInt]);
      Exit;
    end;
end;

{ TRALGQLDocument }

constructor TRALGQLDocument.Create;
begin
  inherited Create;
  Definitions := TObjectList.Create(True);
  Types := TObjectList.Create(True);
end;

destructor TRALGQLDocument.Destroy;
begin
  FreeAndNil(Definitions);
  FreeAndNil(Types);
  inherited Destroy;
end;

function TRALGQLDocument.Fragment(const AName: StringRAL): TRALGQLExecutable;
var
  vInt: IntegerRAL;
  vDef: TRALGQLExecutable;
begin
  Result := nil;
  for vInt := 0 to Definitions.Count - 1 do
  begin
    vDef := TRALGQLExecutable(Definitions[vInt]);
    if vDef.IsFragment and (vDef.Name = AName) then
    begin
      Result := vDef;
      Exit;
    end;
  end;
end;

{ TRALGQLParser }

constructor TRALGQLParser.Create(const ASource: StringRAL);
begin
  inherited Create;
  FSource := ASource;
  FPos := 1;
  FLine := 1;
  FLineStart := 1;
  // a byte order mark is ignored
  if Copy(FSource, 1, 3) = #$EF#$BB#$BF then
    FPos := 4;
end;

procedure TRALGQLParser.Error(const AMessage: string);
begin
  raise ERALGraphQLSyntax.CreatePos('Syntax Error: ' + AMessage, FTokLine, FTokCol);
end;

procedure TRALGQLParser.SkipIgnored;
var
  vChar: AnsiChar;
begin
  while FPos <= Length(FSource) do
  begin
    vChar := AnsiChar(FSource[FPos]);
    case vChar of
      ' ', #9, ',':
        Inc(FPos);
      #10:
        begin
          Inc(FPos);
          Inc(FLine);
          FLineStart := FPos;
        end;
      #13:
        begin
          Inc(FPos);
          if (FPos <= Length(FSource)) and (FSource[FPos] = #10) then
            Inc(FPos);
          Inc(FLine);
          FLineStart := FPos;
        end;
      '#':
        while (FPos <= Length(FSource)) and not (AnsiChar(FSource[FPos]) in [#10, #13]) do
          Inc(FPos);
    else
      Exit;
    end;
  end;
end;

/// Appends the code point ACode as UTF-8 to AText.
procedure AppendUTF8(var AText: StringRAL; ACode: Cardinal);
begin
  if ACode < $80 then
    AppendByte(AText, AnsiChar(ACode))
  else if ACode < $800 then
  begin
    AppendByte(AText, AnsiChar($C0 or (ACode shr 6)));
    AppendByte(AText, AnsiChar($80 or (ACode and $3F)));
  end
  else if ACode < $10000 then
  begin
    AppendByte(AText, AnsiChar($E0 or (ACode shr 12)));
    AppendByte(AText, AnsiChar($80 or ((ACode shr 6) and $3F)));
    AppendByte(AText, AnsiChar($80 or (ACode and $3F)));
  end
  else
  begin
    AppendByte(AText, AnsiChar($F0 or (ACode shr 18)));
    AppendByte(AText, AnsiChar($80 or ((ACode shr 12) and $3F)));
    AppendByte(AText, AnsiChar($80 or ((ACode shr 6) and $3F)));
    AppendByte(AText, AnsiChar($80 or (ACode and $3F)));
  end;
end;

procedure TRALGQLParser.ReadString;
var
  vChar: AnsiChar;
  vHex: string;
  vCode, vLow: Cardinal;
begin
  Inc(FPos); // the opening quote
  FText := '';
  FKind := tkString;
  while True do
  begin
    if FPos > Length(FSource) then
      Error('Unterminated string.');
    vChar := AnsiChar(FSource[FPos]);
    if vChar in [#10, #13] then
      Error('Unterminated string.');
    if vChar = '"' then
    begin
      Inc(FPos);
      Exit;
    end;
    if vChar = '\' then
    begin
      Inc(FPos);
      if FPos > Length(FSource) then
        Error('Unterminated string.');
      vChar := AnsiChar(FSource[FPos]);
      case vChar of
        '"', '\', '/': AppendByte(FText, vChar);
        'b': FText := FText + #8;
        'f': FText := FText + #12;
        'n': FText := FText + #10;
        'r': FText := FText + #13;
        't': FText := FText + #9;
        'u':
          begin
            vHex := string(Copy(FSource, FPos + 1, 4));
            if (Length(vHex) < 4) or not TryStrToInt('$' + vHex, Integer(vCode)) then
              Error('Invalid Unicode escape sequence.');
            Inc(FPos, 4);
            // a surrogate pair is one code point
            if (vCode >= $D800) and (vCode <= $DBFF) and (Copy(FSource, FPos + 1, 2) = '\u') then
            begin
              vHex := string(Copy(FSource, FPos + 3, 4));
              if TryStrToInt('$' + vHex, Integer(vLow)) and (vLow >= $DC00) and (vLow <= $DFFF) then
              begin
                vCode := $10000 + ((vCode - $D800) shl 10) + (vLow - $DC00);
                Inc(FPos, 6);
              end;
            end;
            AppendUTF8(FText, vCode);
          end;
      else
        Error('Invalid character escape sequence: \' + string(vChar) + '.');
      end;
      Inc(FPos);
      Continue;
    end;
    AppendByte(FText, AnsiChar(FSource[FPos]));
    Inc(FPos);
  end;
end;

procedure TRALGQLParser.ReadBlockString;
var
  vRaw: StringRAL;
  vLines: TStringList;
  vInt, vIndent, vSpaces, vCommon: IntegerRAL;
  vLine: string;
begin
  Inc(FPos, 3);
  vRaw := '';
  while True do
  begin
    if FPos > Length(FSource) then
      Error('Unterminated string.');
    if Copy(FSource, FPos, 3) = '"""' then
    begin
      Inc(FPos, 3);
      Break;
    end;
    if Copy(FSource, FPos, 4) = '\"""' then
    begin
      vRaw := vRaw + '"""';
      Inc(FPos, 4);
      Continue;
    end;
    if FSource[FPos] = #10 then
    begin
      Inc(FLine);
      FLineStart := FPos + 1;
    end;
    AppendByte(vRaw, AnsiChar(FSource[FPos]));
    Inc(FPos);
  end;
  // the block string value: common indentation and blank first/last lines removed
  vLines := TStringList.Create;
  try
    vLines.Text := string(StringReplace(string(vRaw), #13#10, #10, [rfReplaceAll]));
    if (vRaw <> '') and (vRaw[Length(vRaw)] = #10) then
      vLines.Add('');
    vCommon := -1;
    for vInt := 1 to vLines.Count - 1 do
    begin
      vLine := vLines[vInt];
      vSpaces := 0;
      while (vSpaces < Length(vLine)) and (vLine[vSpaces + 1] in [' ', #9]) do
        Inc(vSpaces);
      if vSpaces = Length(vLine) then
        Continue;
      if (vCommon < 0) or (vSpaces < vCommon) then
        vCommon := vSpaces;
    end;
    if vCommon > 0 then
      for vInt := 1 to vLines.Count - 1 do
        vLines[vInt] := Copy(vLines[vInt], vCommon + 1, MaxInt);
    while (vLines.Count > 0) and (Trim(vLines[0]) = '') do
      vLines.Delete(0);
    while (vLines.Count > 0) and (Trim(vLines[vLines.Count - 1]) = '') do
      vLines.Delete(vLines.Count - 1);
    FText := '';
    for vIndent := 0 to vLines.Count - 1 do
    begin
      if vIndent > 0 then
        FText := FText + #10;
      FText := FText + StringRAL(vLines[vIndent]);
    end;
  finally
    vLines.Free;
  end;
  FKind := tkBlockString;
end;

procedure TRALGQLParser.ReadNumber;
var
  vStart: IntegerRAL;
  vFloat: boolean;

  procedure Digits;
  begin
    if (FPos > Length(FSource)) or not (AnsiChar(FSource[FPos]) in ['0'..'9']) then
      Error('Invalid number, expected digit.');
    while (FPos <= Length(FSource)) and (AnsiChar(FSource[FPos]) in ['0'..'9']) do
      Inc(FPos);
  end;

begin
  vStart := FPos;
  vFloat := False;
  if FSource[FPos] = '-' then
    Inc(FPos);
  if (FPos <= Length(FSource)) and (FSource[FPos] = '0') then
  begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (AnsiChar(FSource[FPos]) in ['0'..'9']) then
      Error('Invalid number, unexpected digit after 0.');
  end
  else
    Digits;
  if (FPos <= Length(FSource)) and (FSource[FPos] = '.') then
  begin
    vFloat := True;
    Inc(FPos);
    Digits;
  end;
  if (FPos <= Length(FSource)) and (AnsiChar(FSource[FPos]) in ['e', 'E']) then
  begin
    vFloat := True;
    Inc(FPos);
    if (FPos <= Length(FSource)) and (AnsiChar(FSource[FPos]) in ['+', '-']) then
      Inc(FPos);
    Digits;
  end;
  // a number may not be followed by a name start or a dot
  if (FPos <= Length(FSource)) and (AnsiChar(FSource[FPos]) in ['.', '_', 'a'..'z', 'A'..'Z']) then
    Error('Invalid number, expected digit.');
  FText := Copy(FSource, vStart, FPos - vStart);
  if vFloat then
    FKind := tkFloat
  else
    FKind := tkInt;
end;

procedure TRALGQLParser.Next;
var
  vChar: AnsiChar;
  vStart: IntegerRAL;
begin
  SkipIgnored;
  FTokLine := FLine;
  FTokCol := FPos - FLineStart + 1;
  if FPos > Length(FSource) then
  begin
    FKind := tkEOF;
    FText := '';
    Exit;
  end;
  vChar := AnsiChar(FSource[FPos]);
  case vChar of
    '!', '$', '&', '(', ')', ':', '=', '@', '[', ']', '{', '|', '}':
      begin
        FKind := tkPunct;
        FText := StringRAL(vChar);
        Inc(FPos);
      end;
    '.':
      begin
        if Copy(FSource, FPos, 3) <> '...' then
          Error('Unexpected character: ".".');
        FKind := tkPunct;
        FText := '...';
        Inc(FPos, 3);
      end;
    '_', 'a'..'z', 'A'..'Z':
      begin
        vStart := FPos;
        while (FPos <= Length(FSource)) and
          (AnsiChar(FSource[FPos]) in ['_', 'a'..'z', 'A'..'Z', '0'..'9']) do
          Inc(FPos);
        FKind := tkName;
        FText := Copy(FSource, vStart, FPos - vStart);
      end;
    '-', '0'..'9':
      ReadNumber;
    '"':
      if Copy(FSource, FPos, 3) = '"""' then
        ReadBlockString
      else
        ReadString;
  else
    Error('Unexpected character: "' + string(vChar) + '".');
  end;
end;

function TRALGQLParser.IsPunct(const APunct: StringRAL): boolean;
begin
  Result := (FKind = tkPunct) and (FText = APunct);
end;

function TRALGQLParser.IsKeyword(const AWord: StringRAL): boolean;
begin
  Result := (FKind = tkName) and (FText = AWord);
end;

function TRALGQLParser.Skip(const APunct: StringRAL): boolean;
begin
  Result := IsPunct(APunct);
  if Result then
    Next;
end;

function TRALGQLParser.Expect(const APunct: StringRAL): boolean;
begin
  if not IsPunct(APunct) then
  begin
    if FKind = tkEOF then
      Error('Expected "' + string(APunct) + '", found <EOF>.')
    else
      Error('Expected "' + string(APunct) + '", found "' + string(FText) + '".');
  end;
  Next;
  Result := True;
end;

function TRALGQLParser.ExpectKeyword(const AWord: StringRAL): boolean;
begin
  if not IsKeyword(AWord) then
    Error('Expected "' + string(AWord) + '", found "' + string(FText) + '".');
  Next;
  Result := True;
end;

function TRALGQLParser.ExpectName: StringRAL;
begin
  if FKind <> tkName then
  begin
    if FKind = tkEOF then
      Error('Expected Name, found <EOF>.')
    else
      Error('Expected Name, found "' + string(FText) + '".');
  end;
  Result := FText;
  Next;
end;

function TRALGQLParser.ParseValue(AConst: boolean): TRALGQLValue;
var
  vLine, vCol: IntegerRAL;
  vField: TRALGQLObjectField;
begin
  vLine := FTokLine;
  vCol := FTokCol;
  Result := nil;
  try
    if IsPunct('$') then
    begin
      if AConst then
        Error('Unexpected variable in a constant value.');
      Next;
      Result := TRALGQLValue.Create(gvVariable);
      Result.Text := ExpectName;
    end
    else if IsPunct('[') then
    begin
      Next;
      Result := TRALGQLValue.Create(gvList);
      while not Skip(']') do
      begin
        if FKind = tkEOF then
          Error('Expected "]", found <EOF>.');
        Result.Items.Add(ParseValue(AConst));
      end;
    end
    else if IsPunct('{') then
    begin
      Next;
      Result := TRALGQLValue.Create(gvObject);
      while not Skip('}') do
      begin
        vField := TRALGQLObjectField.Create;
        Result.Items.Add(vField);
        vField.Line := FTokLine;
        vField.Column := FTokCol;
        vField.Name := ExpectName;
        Expect(':');
        vField.Value := ParseValue(AConst);
      end;
    end
    else
      case FKind of
        tkInt:
          begin
            Result := TRALGQLValue.Create(gvInt);
            Result.Text := FText;
            Next;
          end;
        tkFloat:
          begin
            Result := TRALGQLValue.Create(gvFloat);
            Result.Text := FText;
            Next;
          end;
        tkString, tkBlockString:
          begin
            Result := TRALGQLValue.Create(gvString);
            Result.Text := FText;
            Next;
          end;
        tkName:
          begin
            if (FText = 'true') or (FText = 'false') then
              Result := TRALGQLValue.Create(gvBoolean)
            else if FText = 'null' then
              Result := TRALGQLValue.Create(gvNull)
            else
              Result := TRALGQLValue.Create(gvEnum);
            Result.Text := FText;
            Next;
          end;
      else
        if FKind = tkEOF then
          Error('Unexpected <EOF>.')
        else
          Error('Unexpected "' + string(FText) + '".');
      end;
    Result.Line := vLine;
    Result.Column := vCol;
  except
    Result.Free;
    raise;
  end;
end;

function TRALGQLParser.ParseArguments(AConst: boolean): TObjectList;
var
  vArg: TRALGQLArgument;
begin
  Result := TObjectList.Create(True);
  try
    if not Skip('(') then
      Exit;
    repeat
      vArg := TRALGQLArgument.Create;
      Result.Add(vArg);
      vArg.Line := FTokLine;
      vArg.Column := FTokCol;
      vArg.Name := ExpectName;
      Expect(':');
      vArg.Value := ParseValue(AConst);
    until Skip(')');
  except
    Result.Free;
    raise;
  end;
end;

function TRALGQLParser.ParseDirectives(AConst: boolean): TObjectList;
var
  vDir: TRALGQLDirective;
  vArgs: TObjectList;
begin
  Result := TObjectList.Create(True);
  try
    while IsPunct('@') do
    begin
      vDir := TRALGQLDirective.Create;
      Result.Add(vDir);
      vDir.Line := FTokLine;
      vDir.Column := FTokCol;
      Next;
      vDir.Name := ExpectName;
      vArgs := ParseArguments(AConst);
      FreeAndNil(vDir.Args);
      vDir.Args := vArgs;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALGQLParser.ParseType: TRALGQLTypeRef;
var
  vInner: TRALGQLTypeRef;
  vLine, vCol: IntegerRAL;
begin
  vLine := FTokLine;
  vCol := FTokCol;
  if Skip('[') then
  begin
    vInner := ParseType(); // () : the name alone is the result variable on FPC
    try
      Expect(']');
    except
      vInner.Free;
      raise;
    end;
    Result := TRALGQLTypeRef.Create;
    Result.Kind := gtList;
    Result.OfType := vInner;
  end
  else
  begin
    Result := TRALGQLTypeRef.Create;
    try
      Result.Kind := gtNamed;
      Result.Name := ExpectName;
    except
      Result.Free;
      raise;
    end;
  end;
  Result.Line := vLine;
  Result.Column := vCol;
  if Skip('!') then
  begin
    vInner := Result;
    Result := TRALGQLTypeRef.Create;
    Result.Kind := gtNonNull;
    Result.OfType := vInner;
    Result.Line := vLine;
    Result.Column := vCol;
  end;
end;

function TRALGQLParser.ParseVariableDefs: TObjectList;
var
  vDef: TRALGQLVariableDef;
  vDirectives: TObjectList;
begin
  Result := TObjectList.Create(True);
  try
    if not Skip('(') then
      Exit;
    repeat
      vDef := TRALGQLVariableDef.Create;
      Result.Add(vDef);
      vDef.Line := FTokLine;
      vDef.Column := FTokCol;
      Expect('$');
      vDef.Name := ExpectName;
      Expect(':');
      vDef.TypeRef := ParseType;
      if Skip('=') then
        vDef.DefaultValue := ParseValue(True);
      vDirectives := ParseDirectives(True);
      vDirectives.Free;
    until Skip(')');
  except
    Result.Free;
    raise;
  end;
end;

function TRALGQLParser.ParseSelection: TRALGQLSelection;
var
  vName: StringRAL;
  vLine, vCol: IntegerRAL;
  vList: TObjectList;
begin
  vLine := FTokLine;
  vCol := FTokCol;
  if Skip('...') then
  begin
    if (FKind = tkName) and (FText <> 'on') then
    begin
      Result := TRALGQLSelection.Create(gsFragmentSpread);
      try
        Result.Name := ExpectName;
        vList := ParseDirectives(False);
        FreeAndNil(Result.Directives);
        Result.Directives := vList;
      except
        Result.Free;
        raise;
      end;
    end
    else
    begin
      Result := TRALGQLSelection.Create(gsInlineFragment);
      try
        if IsKeyword('on') then
        begin
          Next;
          Result.TypeCondition := ExpectName;
        end;
        vList := ParseDirectives(False);
        FreeAndNil(Result.Directives);
        Result.Directives := vList;
        vList := ParseSelectionSet;
        FreeAndNil(Result.Selections);
        Result.Selections := vList;
      except
        Result.Free;
        raise;
      end;
    end;
  end
  else
  begin
    Result := TRALGQLSelection.Create(gsField);
    try
      vName := ExpectName;
      if Skip(':') then
      begin
        Result.Alias := vName;
        vName := ExpectName;
      end;
      Result.Name := vName;
      vList := ParseArguments(False);
      FreeAndNil(Result.Args);
      Result.Args := vList;
      vList := ParseDirectives(False);
      FreeAndNil(Result.Directives);
      Result.Directives := vList;
      if IsPunct('{') then
      begin
        vList := ParseSelectionSet;
        FreeAndNil(Result.Selections);
        Result.Selections := vList;
      end;
    except
      Result.Free;
      raise;
    end;
  end;
  Result.Line := vLine;
  Result.Column := vCol;
end;

function TRALGQLParser.ParseSelectionSet: TObjectList;
begin
  Result := TObjectList.Create(True);
  try
    Expect('{');
    repeat
      if FKind = tkEOF then
        Error('Expected Name, found <EOF>.');
      Result.Add(ParseSelection);
    until Skip('}');
  except
    Result.Free;
    raise;
  end;
end;

function TRALGQLParser.ParseExecutable: TRALGQLExecutable;
var
  vList: TObjectList;
begin
  Result := TRALGQLExecutable.Create;
  try
    Result.Line := FTokLine;
    Result.Column := FTokCol;
    if IsPunct('{') then
      Result.Operation := goQuery
    else if IsKeyword('fragment') then
    begin
      Next;
      Result.IsFragment := True;
      if IsKeyword('on') then
        Error('Unexpected Name "on".');
      Result.Name := ExpectName;
      ExpectKeyword('on');
      Result.TypeCondition := ExpectName;
    end
    else
    begin
      if IsKeyword('query') then
        Result.Operation := goQuery
      else if IsKeyword('mutation') then
        Result.Operation := goMutation
      else if IsKeyword('subscription') then
        Result.Operation := goSubscription
      else
        Error('Unexpected "' + string(FText) + '".');
      Next;
      if FKind = tkName then
        Result.Name := ExpectName;
      vList := ParseVariableDefs;
      FreeAndNil(Result.Variables);
      Result.Variables := vList;
    end;
    vList := ParseDirectives(False);
    FreeAndNil(Result.Directives);
    Result.Directives := vList;
    vList := ParseSelectionSet;
    FreeAndNil(Result.Selections);
    Result.Selections := vList;
  except
    Result.Free;
    raise;
  end;
end;

function TRALGQLParser.ParseDescription: StringRAL;
begin
  Result := '';
  if FKind in [tkString, tkBlockString] then
  begin
    Result := FText;
    Next;
  end;
end;

/// Reads @deprecated(reason:) of ADirectives into ADef.
procedure ApplyDeprecated(ADirectives: TObjectList; ADef: TRALGQLFieldDef);
var
  vInt: IntegerRAL;
  vDir: TRALGQLDirective;
begin
  for vInt := 0 to ADirectives.Count - 1 do
  begin
    vDir := TRALGQLDirective(ADirectives[vInt]);
    if vDir.Name = 'deprecated' then
    begin
      ADef.Deprecated := True;
      if vDir.Arg('reason') <> nil then
        ADef.DeprecationReason := vDir.Arg('reason').Value.Text
      else
        ADef.DeprecationReason := 'No longer supported';
    end;
  end;
end;

function TRALGQLParser.ParseFieldDef(AInput: boolean): TRALGQLFieldDef;
var
  vDirectives: TObjectList;
  vArgs: TObjectList;
begin
  Result := TRALGQLFieldDef.Create;
  try
    Result.Description := ParseDescription;
    Result.Line := FTokLine;
    Result.Column := FTokCol;
    Result.Name := ExpectName;
    if (not AInput) and IsPunct('(') then
    begin
      vArgs := ParseFieldDefs(True, '(', ')');
      FreeAndNil(Result.Args);
      Result.Args := vArgs;
    end;
    Expect(':');
    Result.TypeRef := ParseType;
    if AInput and Skip('=') then
      Result.DefaultValue := ParseValue(True);
    vDirectives := ParseDirectives(True);
    try
      ApplyDeprecated(vDirectives, Result);
    finally
      vDirectives.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALGQLParser.ParseFieldDefs(AInput: boolean; const AOpen, AClose: StringRAL): TObjectList;
begin
  Result := TObjectList.Create(True);
  try
    Expect(AOpen);
    repeat
      Result.Add(ParseFieldDef(AInput));
    until Skip(AClose);
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALGQLParser.ParseTypeSystem(ADocument: TRALGQLDocument; const ADescription: StringRAL);
var
  vDef: TRALGQLTypeDef;
  vKind: TRALGQLTypeKind;
  vDirectives, vList: TObjectList;
  vValue: TRALGQLFieldDef;
  vInt: IntegerRAL;
  vDir: TRALGQLDirective;
  vOp: StringRAL;
  vExtend: boolean;
begin
  vExtend := IsKeyword('extend');
  if vExtend then
    Next;
  if IsKeyword('schema') then
  begin
    Next;
    ParseDirectives(True).Free;
    Expect('{');
    repeat
      vOp := ExpectName;
      Expect(':');
      if vOp = 'query' then
        ADocument.QueryType := ExpectName
      else if vOp = 'mutation' then
        ADocument.MutationType := ExpectName
      else if vOp = 'subscription' then
        ADocument.SubscriptionType := ExpectName
      else
        Error('Unexpected "' + string(vOp) + '".');
    until Skip('}');
    Exit;
  end;
  if IsKeyword('directive') then
  begin
    // read and ignored: the executor knows the built-in directives
    Next;
    Expect('@');
    ExpectName;
    if IsPunct('(') then
      ParseFieldDefs(True, '(', ')').Free;
    if IsKeyword('repeatable') then
      Next;
    ExpectKeyword('on');
    Skip('|');
    repeat
      ExpectName;
    until not Skip('|');
    Exit;
  end;
  if IsKeyword('scalar') then
    vKind := gkScalar
  else if IsKeyword('type') then
    vKind := gkObject
  else if IsKeyword('interface') then
    vKind := gkInterface
  else if IsKeyword('union') then
    vKind := gkUnion
  else if IsKeyword('enum') then
    vKind := gkEnum
  else if IsKeyword('input') then
    vKind := gkInputObject
  else
  begin
    Error('Unexpected "' + string(FText) + '".');
    Exit;
  end;
  vDef := TRALGQLTypeDef.Create(vKind);
  ADocument.Types.Add(vDef);
  vDef.IsExtension := vExtend;
  vDef.Description := ADescription;
  vDef.Line := FTokLine;
  vDef.Column := FTokCol;
  Next;
  vDef.Name := ExpectName;
  if (vKind in [gkObject, gkInterface]) and IsKeyword('implements') then
  begin
    Next;
    Skip('&');
    repeat
      vDef.Interfaces.Add(string(ExpectName));
    until not Skip('&');
  end;
  vDirectives := ParseDirectives(True);
  try
    for vInt := 0 to vDirectives.Count - 1 do
    begin
      vDir := TRALGQLDirective(vDirectives[vInt]);
      if (vDir.Name = 'specifiedBy') and (vDir.Arg('url') <> nil) then
        vDef.SpecifiedByURL := vDir.Arg('url').Value.Text;
    end;
  finally
    vDirectives.Free;
  end;
  case vKind of
    gkObject, gkInterface:
      if IsPunct('{') then
      begin
        vList := ParseFieldDefs(False, '{', '}');
        FreeAndNil(vDef.Fields);
        vDef.Fields := vList;
      end;
    gkInputObject:
      if IsPunct('{') then
      begin
        vList := ParseFieldDefs(True, '{', '}');
        FreeAndNil(vDef.Fields);
        vDef.Fields := vList;
      end;
    gkUnion:
      if Skip('=') then
      begin
        Skip('|');
        repeat
          vDef.PossibleTypes.Add(string(ExpectName));
        until not Skip('|');
      end;
    gkEnum:
      if Skip('{') then
        repeat
          vValue := TRALGQLFieldDef.Create;
          vDef.EnumValues.Add(vValue);
          vValue.Description := ParseDescription;
          vValue.Line := FTokLine;
          vValue.Column := FTokCol;
          vValue.Name := ExpectName;
          vDirectives := ParseDirectives(True);
          try
            ApplyDeprecated(vDirectives, vValue);
          finally
            vDirectives.Free;
          end;
        until Skip('}');
  end;
end;

function TRALGQLParser.Parse: TRALGQLDocument;
var
  vDescription: StringRAL;
begin
  Result := TRALGQLDocument.Create;
  try
    Next;
    if FKind = tkEOF then
      Error('Unexpected <EOF>.');
    while FKind <> tkEOF do
    begin
      vDescription := '';
      if FSchema and (FKind in [tkString, tkBlockString]) then
        vDescription := ParseDescription;
      if IsPunct('{') or IsKeyword('query') or IsKeyword('mutation') or
         IsKeyword('subscription') or IsKeyword('fragment') then
      begin
        if not FExecutable then
          Error('Unexpected "' + string(FText) + '": executable definitions are not allowed here.');
        Result.Definitions.Add(ParseExecutable);
      end
      else if FKind = tkName then
      begin
        if not FSchema then
          Error('Unexpected "' + string(FText) + '".');
        ParseTypeSystem(Result, vDescription);
      end
      else
        Error('Unexpected "' + string(FText) + '".');
    end;
  except
    Result.Free;
    raise;
  end;
end;

function RALGraphQLParse(const ASource: StringRAL; AExecutable, ASchema: boolean): TRALGQLDocument;
var
  vParser: TRALGQLParser;
begin
  vParser := TRALGQLParser.Create(ASource);
  try
    vParser.FExecutable := AExecutable;
    vParser.FSchema := ASchema;
    Result := vParser.Parse;
  finally
    vParser.Free;
  end;
end;

end.
