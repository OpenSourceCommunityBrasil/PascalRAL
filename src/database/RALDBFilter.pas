{ The filter language of the URL for the data resources: the simple syntax
  (name_gt=10, _sort=-price) and RQL (and(eq(name,Ana),gt(age,30))), read into
  one tree and written as parameterized SQL over the columns of the catalog. }
unit RALDBFilter;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DB, Contnrs,
  RALTypes, RALTools, RALDBCatalog;

type
  /// The operation of a filter node.
  TRALDBFilterOp = (fopEq, fopNe, fopLt, fopLe, fopGt, fopGe, fopLike, fopContains,
    fopStarts, fopEnds, fopIn, fopOut, fopNull, fopNotNull, fopAnd, fopOr, fopNot);

  /// A syntax of filter the URL may use.
  TRALDBFilterSyntax = (fsSimple, fsRQL);
  /// A set of filter syntaxes.
  TRALDBFilterSyntaxes = set of TRALDBFilterSyntax;

  /// A filter the URL asked for that cannot be honoured: answered 400.
  ERALDBFilterError = class(Exception);

  /// One node of a filter: a comparison of Field with Values, or a group of Children.
  TRALDBFilterNode = class
  private
    FChildren: TObjectList;
    FField: StringRAL;
    FOp: TRALDBFilterOp;
    FValues: TStringList;
  public
    /// A node of AOp on AField.
    constructor Create(AOp: TRALDBFilterOp; const AField: StringRAL = '');
    destructor Destroy; override;

    /// Adds AChild (owned) to a group and returns it.
    function Add(AChild: TRALDBFilterNode): TRALDBFilterNode;

    /// The nodes of an and, or or not; owned.
    property Children: TObjectList read FChildren;
    /// The field compared, as the URL wrote it.
    property Field: StringRAL read FField;
    /// The operation.
    property Op: TRALDBFilterOp read FOp;
    /// The values compared with, as text.
    property Values: TStringList read FValues;
  end;

  { What the URL asked of a list: the filter, the order, the page and the
    fields. Sort holds a field per line, with '-' before it for descending. }
  TRALDBQuery = class
  private
    FFields: TStringList;
    FFilter: TRALDBFilterNode;
    FLimit: Int64RAL;
    FOffset: Int64RAL;
    FSort: TStringList;

    /// Reads the RQL expression starting at APos of AText.
    function ParseRQLNode(const AText: StringRAL; var APos: IntegerRAL): TRALDBFilterNode;
  public
    constructor Create;
    destructor Destroy; override;

    /// The ORDER BY list of Sort over ATable, quoted by ADialect; '' when none.
    function OrderSQL(ATable: TRALDBTableInfo; ADialect: TRALDBDialect): StringRAL;
    { Reads the query params AParams (Name=Value): the simple syntax, or RQL in
      a param named rql; ASyntaxes says which are taken. Raises
      ERALDBFilterError for a filter it cannot read. }
    procedure Parse(AParams: TStrings; ASyntaxes: TRALDBFilterSyntaxes = [fsSimple, fsRQL];
      AForce: Boolean = False; AForcedSyntax: TRALDBFilterSyntax = fsSimple);
    /// Reads the RQL expression AText into the filter, sort and limit.
    procedure ParseRQL(const AText: StringRAL);
    { The SQL condition of the filter, with :f1, :f2... params added to AParams
      typed by the columns of ATable; '' when there is no filter. Raises
      ERALDBFilterError for a field the table does not have. }
    function WhereSQL(ATable: TRALDBTableInfo; ADialect: TRALDBDialect;
      AParams: TParams): StringRAL;

    /// The fields asked for, one per line; empty = all.
    property Fields: TStringList read FFields;
    /// The root of the filter, an and of the conditions.
    property Filter: TRALDBFilterNode read FFilter;
    /// The page size asked for; 0 = none.
    property Limit: Int64RAL read FLimit write FLimit;
    /// The rows skipped.
    property Offset: Int64RAL read FOffset write FOffset;
    /// The order, a field per line, '-' for descending.
    property Sort: TStringList read FSort;
  end;

{ Sets AParam to AValue converted to the type of AColumn (nil = text);
  raises ERALDBFilterError when AValue does not fit it. }
procedure RALDBSetParamValue(AParam: TParam; AColumn: TRALDBColumnInfo; const AValue: StringRAL);

implementation

const
  /// The suffixes of the simple syntax and their operations.
  cSuffixes: array[0..11] of string = ('_gte', '_lte', '_gt', '_lt', '_ne', '_like',
    '_cont', '_sw', '_ew', '_in', '_nin', '_null');
  /// The operations of the suffixes.
  cSuffixOps: array[0..11] of TRALDBFilterOp = (fopGe, fopLe, fopGt, fopLt, fopNe, fopLike,
    fopContains, fopStarts, fopEnds, fopIn, fopOut, fopNull);

/// AText of the RTL's string type as StringRAL.
function ToRAL(const AText: string): StringRAL;
begin
  {$IFDEF FPC}
  Result := AText;
  {$ELSE}
  Result := UTF8Encode(AText);
  {$ENDIF}
end;

/// StringRAL AText as the RTL's string type.
function FromRAL(const AText: StringRAL): string;
begin
  {$IFDEF FPC}
  Result := AText;
  {$ELSE}
  Result := UTF8ToString(AText);
  {$ENDIF}
end;

procedure RALDBSetParamValue(AParam: TParam; AColumn: TRALDBColumnInfo; const AValue: StringRAL);
var
  vInt: Int64;
  vFloat: Double;
  vDate: TDateTime;
  vText: StringRAL;
begin
  if AColumn = nil then
  begin
    AParam.DataType := ftString;
    AParam.AsString := FromRAL(AValue);
    Exit;
  end;
  case AColumn.DataType of
    ftSmallint, ftInteger, ftWord, ftLargeint:
    begin
      if not TryStrToInt64(string(AValue), vInt) then
        raise ERALDBFilterError.CreateFmt('Not an integer for %s', [string(AColumn.Name)]);
      AParam.DataType := ftLargeint;
      AParam.Value := vInt;
    end;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
    begin
      if not RALTryStrToFloat(AValue, vFloat) then
        raise ERALDBFilterError.CreateFmt('Not a number for %s', [string(AColumn.Name)]);
      AParam.DataType := ftFloat;
      AParam.AsFloat := vFloat;
    end;
    ftBoolean:
    begin
      vText := RALTrim(AValue);
      AParam.DataType := ftBoolean;
      if RALSameName(vText, 'true') or (vText = '1') then
        AParam.AsBoolean := True
      else if RALSameName(vText, 'false') or (vText = '0') then
        AParam.AsBoolean := False
      else
        raise ERALDBFilterError.CreateFmt('Not a boolean for %s', [string(AColumn.Name)]);
    end;
    ftDate, ftTime, ftDateTime, ftTimeStamp:
    begin
      if not RALTryISO8601ToDateTime(AValue, vDate) then
        raise ERALDBFilterError.CreateFmt('Not an ISO 8601 date for %s', [string(AColumn.Name)]);
      AParam.DataType := ftDateTime;
      AParam.AsDateTime := vDate;
    end;
  else
    AParam.DataType := ftString;
    AParam.AsString := FromRAL(AValue);
  end;
end;

{ TRALDBFilterNode }

constructor TRALDBFilterNode.Create(AOp: TRALDBFilterOp; const AField: StringRAL);
begin
  inherited Create;
  FOp := AOp;
  FField := AField;
  FValues := TStringList.Create;
  FChildren := TObjectList.Create(True);
end;

destructor TRALDBFilterNode.Destroy;
begin
  FValues.Free;
  FChildren.Free;
  inherited Destroy;
end;

function TRALDBFilterNode.Add(AChild: TRALDBFilterNode): TRALDBFilterNode;
begin
  FChildren.Add(AChild);
  Result := AChild;
end;

{ TRALDBQuery }

constructor TRALDBQuery.Create;
begin
  inherited Create;
  FFilter := TRALDBFilterNode.Create(fopAnd);
  FSort := TStringList.Create;
  FFields := TStringList.Create;
end;

destructor TRALDBQuery.Destroy;
begin
  FFilter.Free;
  FSort.Free;
  FFields.Free;
  inherited Destroy;
end;

function TRALDBQuery.OrderSQL(ATable: TRALDBTableInfo; ADialect: TRALDBDialect): StringRAL;
var
  vInt: IntegerRAL;
  vName: StringRAL;
  vDesc: Boolean;
  vColumn: TRALDBColumnInfo;
begin
  Result := '';
  for vInt := 0 to FSort.Count - 1 do
  begin
    vName := ToRAL(FSort[vInt]);
    vDesc := Copy(vName, 1, 1) = '-';
    if (Copy(vName, 1, 1) = '-') or (Copy(vName, 1, 1) = '+') then
      Delete(vName, 1, 1);
    vColumn := ATable.FindColumn(vName);
    if vColumn = nil then
      raise ERALDBFilterError.CreateFmt('Unknown field %s', [string(vName)]);
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + ADialect.Quote(vColumn.Name);
    if vDesc then
      Result := Result + ' desc';
  end;
end;

procedure TRALDBQuery.Parse(AParams: TStrings; ASyntaxes: TRALDBFilterSyntaxes;
  AForce: Boolean; AForcedSyntax: TRALDBFilterSyntax);
var
  vInt, vSuffix, vPage, vPerPage: IntegerRAL;
  vName, vValue: string;
  vOp: TRALDBFilterOp;
  vNode: TRALDBFilterNode;
  vRQL: StringRAL;
  vList: TStringList;
begin
  vPage := 0;
  vPerPage := 0;
  vRQL := '';
  for vInt := 0 to AParams.Count - 1 do
  begin
    vName := AParams.Names[vInt];
    vValue := AParams.ValueFromIndex[vInt];
    if vName = '' then
      Continue;
    if (vName = 'rql') or (vName = 'filter-syntax') then
    begin
      if vName = 'rql' then
        vRQL := ToRAL(vValue);
      Continue;
    end;
    if not (fsSimple in ASyntaxes) or (AForce and (AForcedSyntax <> fsSimple)) then
      Continue;
    if vName = '_sort' then
    begin
      vList := TStringList.Create;
      try
        vList.CommaText := vValue;
        FSort.AddStrings(vList);
      finally
        vList.Free;
      end;
    end
    else if vName = '_fields' then
      FFields.CommaText := vValue
    else if (vName = '_limit') or (vName = '_per_page') then
    begin
      vPerPage := StrToIntDef(vValue, -1);
      if vPerPage < 0 then
        raise ERALDBFilterError.CreateFmt('Invalid %s', [vName]);
      FLimit := vPerPage;
    end
    else if vName = '_offset' then
    begin
      FOffset := StrToInt64Def(vValue, -1);
      if FOffset < 0 then
        raise ERALDBFilterError.Create('Invalid _offset');
    end
    else if vName = '_page' then
    begin
      vPage := StrToIntDef(vValue, 0);
      if vPage < 1 then
        raise ERALDBFilterError.Create('Invalid _page');
    end
    else if Copy(vName, 1, 1) = '_' then
      raise ERALDBFilterError.CreateFmt('Unknown parameter %s', [vName])
    else
    begin
      vOp := fopEq;
      for vSuffix := 0 to High(cSuffixes) do
        if (Length(vName) > Length(cSuffixes[vSuffix])) and
          (Copy(vName, Length(vName) - Length(cSuffixes[vSuffix]) + 1, MaxInt) = cSuffixes[vSuffix]) then
        begin
          vOp := cSuffixOps[vSuffix];
          vName := Copy(vName, 1, Length(vName) - Length(cSuffixes[vSuffix]));
          Break;
        end;
      if vOp = fopNull then
      begin
        if SameText(vValue, 'false') then
          vOp := fopNotNull
        else if not SameText(vValue, 'true') then
          raise ERALDBFilterError.CreateFmt('Invalid %s_null', [vName]);
      end;
      vNode := FFilter.Add(TRALDBFilterNode.Create(vOp, ToRAL(vName)));
      if vOp in [fopIn, fopOut] then
        vNode.Values.CommaText := vValue
      else if not (vOp in [fopNull, fopNotNull]) then
        vNode.Values.Add(vValue);
    end;
  end;
  if (vPage > 0) and (FLimit > 0) then
    FOffset := (vPage - 1) * FLimit;
  if vRQL <> '' then
  begin
    if not (fsRQL in ASyntaxes) or (AForce and (AForcedSyntax <> fsRQL)) then
      raise ERALDBFilterError.Create('RQL is not accepted');
    ParseRQL(vRQL);
  end;
end;

procedure TRALDBQuery.ParseRQL(const AText: StringRAL);
var
  vPos: IntegerRAL;
  vNode: TRALDBFilterNode;
begin
  vPos := 1;
  while vPos <= Length(AText) do
  begin
    vNode := ParseRQLNode(AText, vPos);
    // sort() and limit() fill the query and leave no node.
    if vNode <> nil then
      FFilter.Add(vNode);
    while (vPos <= Length(AText)) and (AText[vPos] in [',', '&', ' ']) do
      Inc(vPos);
  end;
end;

function TRALDBQuery.ParseRQLNode(const AText: StringRAL; var APos: IntegerRAL): TRALDBFilterNode;
var
  vName: StringRAL;
  vArgs: TStringList;
  vChildren: TObjectList;
  vStart: IntegerRAL;
  vQuote: AnsiChar;
  vOp: TRALDBFilterOp;
  vInt: IntegerRAL;
  vIsGroup: Boolean;

  procedure Fail;
  begin
    raise ERALDBFilterError.CreateFmt('Invalid RQL at %d', [APos]);
  end;

  /// Reads a literal argument.
  function Literal: StringRAL;
  begin
    if (APos <= Length(AText)) and (AText[APos] in ['''', '"']) then
    begin
      vQuote := AText[APos];
      Inc(APos);
      vStart := APos;
      while (APos <= Length(AText)) and (AText[APos] <> vQuote) do
        Inc(APos);
      if APos > Length(AText) then
        Fail;
      Result := Copy(AText, vStart, APos - vStart);
      Inc(APos);
    end
    else
    begin
      vStart := APos;
      while (APos <= Length(AText)) and not (AText[APos] in [',', ')', '(']) do
        Inc(APos);
      Result := RALTrim(Copy(AText, vStart, APos - vStart));
    end;
  end;

begin
  Result := nil;
  vStart := APos;
  while (APos <= Length(AText)) and (AText[APos] in ['a'..'z', 'A'..'Z']) do
    Inc(APos);
  vName := Copy(AText, vStart, APos - vStart);
  if (vName = '') or (APos > Length(AText)) or (AText[APos] <> '(') then
    Fail;
  Inc(APos);
  vIsGroup := RALSameName(vName, 'and') or RALSameName(vName, 'or') or RALSameName(vName, 'not');
  vArgs := TStringList.Create;
  vChildren := TObjectList.Create(False);
  try
    while (APos <= Length(AText)) and (AText[APos] <> ')') do
    begin
      if vIsGroup then
        vChildren.Add(ParseRQLNode(AText, APos))
      else
        vArgs.Add(FromRAL(Literal));
      while (APos <= Length(AText)) and (AText[APos] = ' ') do
        Inc(APos);
      if (APos <= Length(AText)) and (AText[APos] = ',') then
        Inc(APos);
    end;
    if APos > Length(AText) then
      Fail;
    Inc(APos);
    if vIsGroup then
    begin
      if RALSameName(vName, 'and') then
        Result := TRALDBFilterNode.Create(fopAnd)
      else if RALSameName(vName, 'or') then
        Result := TRALDBFilterNode.Create(fopOr)
      else
        Result := TRALDBFilterNode.Create(fopNot);
      for vInt := 0 to vChildren.Count - 1 do
        Result.Add(TRALDBFilterNode(vChildren[vInt]));
      vChildren.Clear;
      Exit;
    end;
    if RALSameName(vName, 'sort') then
    begin
      FSort.AddStrings(vArgs);
      Exit;
    end;
    if RALSameName(vName, 'limit') then
    begin
      if vArgs.Count > 0 then
        FLimit := StrToInt64Def(vArgs[0], 0);
      if vArgs.Count > 1 then
        FOffset := StrToInt64Def(vArgs[1], 0);
      Exit;
    end;
    if vArgs.Count < 1 then
      Fail;
    if RALSameName(vName, 'eq') then vOp := fopEq
    else if RALSameName(vName, 'ne') then vOp := fopNe
    else if RALSameName(vName, 'lt') then vOp := fopLt
    else if RALSameName(vName, 'le') then vOp := fopLe
    else if RALSameName(vName, 'gt') then vOp := fopGt
    else if RALSameName(vName, 'ge') then vOp := fopGe
    else if RALSameName(vName, 'like') then vOp := fopLike
    else if RALSameName(vName, 'contains') then vOp := fopContains
    else if RALSameName(vName, 'starts') then vOp := fopStarts
    else if RALSameName(vName, 'ends') then vOp := fopEnds
    else if RALSameName(vName, 'in') then vOp := fopIn
    else if RALSameName(vName, 'out') then vOp := fopOut
    else if RALSameName(vName, 'isnull') then vOp := fopNull
    else if RALSameName(vName, 'notnull') then vOp := fopNotNull
    else
    begin
      Fail;
      vOp := fopEq;
    end;
    if not (vOp in [fopNull, fopNotNull, fopIn, fopOut]) and (vArgs.Count <> 2) then
      Fail;
    Result := TRALDBFilterNode.Create(vOp, ToRAL(vArgs[0]));
    for vInt := 1 to vArgs.Count - 1 do
      Result.Values.Add(vArgs[vInt]);
  finally
    for vInt := 0 to vChildren.Count - 1 do
      TObject(vChildren[vInt]).Free;
    vChildren.Free;
    vArgs.Free;
  end;
end;

function TRALDBQuery.WhereSQL(ATable: TRALDBTableInfo; ADialect: TRALDBDialect;
  AParams: TParams): StringRAL;
var
  vCount: IntegerRAL;

  /// A new param for AValue typed by AColumn; returns its placeholder.
  function NewParam(AColumn: TRALDBColumnInfo; const AValue: StringRAL): StringRAL;
  var
    vParam: TParam;
  begin
    Inc(vCount);
    vParam := AParams.CreateParam(ftString, 'f' + IntToStr(vCount), ptInput);
    RALDBSetParamValue(vParam, AColumn, AValue);
    Result := StringRAL(':f' + IntToStr(vCount));
  end;

  /// The SQL of ANode.
  function NodeSQL(ANode: TRALDBFilterNode): StringRAL;
  const
    cCompare: array[fopEq..fopGe] of StringRAL = (' = ', ' <> ', ' < ', ' <= ', ' > ', ' >= ');
  var
    vColumn: TRALDBColumnInfo;
    vQuoted, vPart: StringRAL;
    vInt: IntegerRAL;
  begin
    Result := '';
    if ANode.Op in [fopAnd, fopOr, fopNot] then
    begin
      for vInt := 0 to ANode.Children.Count - 1 do
      begin
        vPart := NodeSQL(TRALDBFilterNode(ANode.Children[vInt]));
        if vPart = '' then
          Continue;
        if Result <> '' then
        begin
          if ANode.Op = fopOr then
            Result := Result + ' or '
          else
            Result := Result + ' and ';
        end;
        Result := Result + vPart;
      end;
      if Result <> '' then
      begin
        Result := '(' + Result + ')';
        if ANode.Op = fopNot then
          Result := 'not ' + Result;
      end;
      Exit;
    end;
    // Every name goes through the catalog: nothing of the URL reaches the SQL.
    vColumn := ATable.FindColumn(ANode.Field);
    if vColumn = nil then
      raise ERALDBFilterError.CreateFmt('Unknown field %s', [string(ANode.Field)]);
    vQuoted := ADialect.Quote(vColumn.Name);
    case ANode.Op of
      fopEq..fopGe:
        Result := vQuoted + cCompare[ANode.Op] + NewParam(vColumn, ToRAL(ANode.Values[0]));
      fopLike:
        Result := ADialect.LikeNoCase(vQuoted, NewParam(nil, ToRAL(ANode.Values[0])));
      fopContains:
        Result := ADialect.LikeNoCase(vQuoted, NewParam(nil, '%' + ToRAL(ANode.Values[0]) + '%'));
      fopStarts:
        Result := ADialect.LikeNoCase(vQuoted, NewParam(nil, ToRAL(ANode.Values[0]) + '%'));
      fopEnds:
        Result := ADialect.LikeNoCase(vQuoted, NewParam(nil, '%' + ToRAL(ANode.Values[0])));
      fopIn, fopOut:
      begin
        if ANode.Values.Count = 0 then
          raise ERALDBFilterError.CreateFmt('Empty list for %s', [string(ANode.Field)]);
        for vInt := 0 to ANode.Values.Count - 1 do
        begin
          if vInt > 0 then
            Result := Result + ', ';
          Result := Result + NewParam(vColumn, ToRAL(ANode.Values[vInt]));
        end;
        if ANode.Op = fopIn then
          Result := vQuoted + ' in (' + Result + ')'
        else
          Result := vQuoted + ' not in (' + Result + ')';
      end;
      fopNull:
        Result := vQuoted + ' is null';
      fopNotNull:
        Result := vQuoted + ' is not null';
    end;
  end;

begin
  vCount := AParams.Count;
  Result := NodeSQL(FFilter);
end;

end.
