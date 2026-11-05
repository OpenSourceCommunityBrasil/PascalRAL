/// Imports an OpenAPI 3.0/3.1 document (JSON): writes a Pascal unit with one
/// TRALObject class per schema of components/schemas (published properties,
/// portable to every compiler RAL supports) and, optionally, a client class
/// with one method per operation over TRALObjectClient. The command line tool
/// tools/ralopenapi wraps it.
unit RALOpenAPIImport;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALJson;

type
  /// What the importer writes.
  TRALOpenAPIImportOptions = record
    /// Name of the unit.
    UnitName: string;
    /// Write the client class too.
    WithClient: boolean;
    /// Name of the client class ('' = from the title).
    ClientName: string;
  end;

  /// The document is not one the importer reads.
  ERALOpenAPIImport = class(Exception);

{ The Pascal unit for the OpenAPI document AJSON. Raises ERALOpenAPIImport on
  a document without openapi 3. }
function RALOpenAPIToPascal(const AJSON: StringRAL; const AOptions: TRALOpenAPIImportOptions): string;
/// Default options: unit name AUnitName, with the client.
function RALOpenAPIOptions(const AUnitName: string): TRALOpenAPIImportOptions;
/// AName as a Pascal identifier: letters, digits and '_', not a reserved word.
function RALPascalIdent(const AName: string): string;

implementation

const
  cReserved: array[0..64] of string = ('and', 'array', 'as', 'asm', 'begin', 'case', 'class',
    'const', 'constructor', 'destructor', 'div', 'do', 'downto', 'else', 'end', 'except',
    'exports', 'file', 'finalization', 'finally', 'for', 'function', 'goto', 'if',
    'implementation', 'in', 'inherited', 'initialization', 'inline', 'interface', 'is',
    'label', 'library', 'mod', 'nil', 'not', 'object', 'of', 'or', 'out', 'packed',
    'procedure', 'program', 'property', 'raise', 'record', 'repeat', 'resourcestring', 'set',
    'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'while', 'with', 'xor', 'result');

function RALPascalIdent(const AName: string): string;
var
  vInt: Integer;
  vUpper: boolean;
begin
  Result := '';
  vUpper := True;
  for vInt := 1 to Length(AName) do
    if CharInSet(AName[vInt], ['A'..'Z', 'a'..'z', '0'..'9']) then
    begin
      if vUpper then
        Result := Result + UpperCase(AName[vInt])
      else
        Result := Result + AName[vInt];
      vUpper := False;
    end
    else
      vUpper := True;
  if (Result = '') or CharInSet(Result[1], ['0'..'9']) then
    Result := 'N' + Result;
  for vInt := 0 to High(cReserved) do
    if SameText(Result, cReserved[vInt]) then
    begin
      Result := Result + '_';
      Break;
    end;
end;

function RALOpenAPIOptions(const AUnitName: string): TRALOpenAPIImportOptions;
begin
  Result.UnitName := AUnitName;
  Result.WithClient := True;
  Result.ClientName := '';
end;

type
  TImporter = class
  private
    FDoc: TRALJSONObject;
    FSchemas: TRALJSONObject;
    FClasses: TStringList;   // class name -> order
    FDone: TStringList;
    FOut: TStringList;
    FLists: TStringList;     // list classes to declare: TFooList=TFoo
    FNames: TStringList;     // RALRegisterPropertyName calls
    FOptions: TRALOpenAPIImportOptions;

    function Member(AObj: TRALJSONValue; const AName: string): TRALJSONValue;
    function Text(AObj: TRALJSONValue; const AName: string): string;
    function RefName(const ARef: string): string;
    function ClassOf(const ASchemaName: string): string;
    function Resolve(ASchema: TRALJSONValue): TRALJSONValue;
    function PropType(ASchema: TRALJSONValue; out AOwned: string): string;
    procedure EmitClass(const AName: string);
    function IsClassSchema(const AName: string): boolean;
    procedure CollectProps(ASchema: TRALJSONValue; AProps, ARequired: TStringList;
      out AParent: string);
  public
    constructor Create(ADoc: TRALJSONObject; const AOptions: TRALOpenAPIImportOptions);
    destructor Destroy; override;
    function Run: string;
  end;

constructor TImporter.Create(ADoc: TRALJSONObject; const AOptions: TRALOpenAPIImportOptions);
begin
  inherited Create;
  FDoc := ADoc;
  FOptions := AOptions;
  FClasses := TStringList.Create;
  FDone := TStringList.Create;
  FOut := TStringList.Create;
  FLists := TStringList.Create;
  FNames := TStringList.Create;
  FSchemas := nil;
  if Member(Member(FDoc, 'components'), 'schemas') is TRALJSONObject then
    FSchemas := TRALJSONObject(Member(Member(FDoc, 'components'), 'schemas'));
end;

destructor TImporter.Destroy;
begin
  FClasses.Free;
  FDone.Free;
  FOut.Free;
  FLists.Free;
  FNames.Free;
  inherited;
end;

function TImporter.Member(AObj: TRALJSONValue; const AName: string): TRALJSONValue;
begin
  Result := nil;
  if AObj is TRALJSONObject then
    Result := TRALJSONObject(AObj).Get(StringRAL(AName));
end;

function TImporter.Text(AObj: TRALJSONValue; const AName: string): string;
var
  vValue: TRALJSONValue;
begin
  vValue := Member(AObj, AName);
  if (vValue = nil) or vValue.IsNull then
    Result := ''
  else if vValue.JsonType in [rjtObject, rjtArray] then
    Result := string(vValue.ToJson)
  else
    Result := string(vValue.AsString);
end;

function TImporter.RefName(const ARef: string): string;
var
  vPos: Integer;
begin
  Result := ARef;
  vPos := LastDelimiter('/', Result);
  if vPos > 0 then
    Result := Copy(Result, vPos + 1, MaxInt);
end;

function TImporter.ClassOf(const ASchemaName: string): string;
begin
  Result := 'T' + RALPascalIdent(ASchemaName);
end;

function TImporter.Resolve(ASchema: TRALJSONValue): TRALJSONValue;
var
  vRef: string;
begin
  Result := ASchema;
  vRef := Text(ASchema, '$ref');
  if (vRef <> '') and (FSchemas <> nil) then
    Result := FSchemas.Get(StringRAL(RefName(vRef)));
end;

{ the Pascal type of a property schema; AOwned is the class the constructor
  creates for it ('' for a simple type) }
function TImporter.PropType(ASchema: TRALJSONValue; out AOwned: string): string;
var
  vType, vFormat, vRef, vItemRef: string;
  vItems: TRALJSONValue;
begin
  AOwned := '';
  vRef := Text(ASchema, '$ref');
  if vRef <> '' then
  begin
    Result := ClassOf(RefName(vRef));
    { an enum or a primitive behind the reference stays a value }
    vType := Text(Resolve(ASchema), 'type');
    if (vType = 'string') or (vType = 'integer') or (vType = 'number') or (vType = 'boolean') then
      Result := PropType(Resolve(ASchema), AOwned)
    else
      AOwned := Result;
    Exit;
  end;
  vType := Text(ASchema, 'type');
  { OpenAPI 3.1: type: ["string", "null"] }
  if (vType <> '') and (vType[1] = '[') then
    vType := StringReplace(StringReplace(StringReplace(StringReplace(vType, '"null"', '', []),
      '[', '', []), ']', '', []), '"', '', [rfReplaceAll]);
  vType := StringReplace(vType, ',', '', [rfReplaceAll]);
  vFormat := Text(ASchema, 'format');
  if vType = 'integer' then
  begin
    if vFormat = 'int64' then
      Result := 'Int64'
    else
      Result := 'Integer';
  end
  else if vType = 'number' then
    Result := 'Double'
  else if vType = 'boolean' then
    Result := 'Boolean'
  else if vType = 'string' then
  begin
    if (vFormat = 'date-time') or (vFormat = 'date') then
      Result := 'TDateTime'
    else
      Result := 'StringRAL';
  end
  else if vType = 'array' then
  begin
    vItems := Member(ASchema, 'items');
    vItemRef := Text(vItems, '$ref');
    if (vItemRef <> '') and (Text(Resolve(vItems), 'type') <> 'string') then
    begin
      Result := ClassOf(RefName(vItemRef)) + 'List';
      if FLists.IndexOfName(Result) < 0 then
        FLists.Add(Result + '=' + ClassOf(RefName(vItemRef)));
    end
    else
      Result := 'TStrings';
    AOwned := Result;
    if Result = 'TStrings' then
      AOwned := 'TStringList';
  end
  else
    { an object without a reference, oneOf, anyOf: kept as its JSON text }
    Result := 'StringRAL';
end;

procedure TImporter.CollectProps(ASchema: TRALJSONValue; AProps, ARequired: TStringList;
  out AParent: string);
var
  vProps, vAll, vReq: TRALJSONValue;
  vInt: Integer;
  vSub: TRALJSONValue;
  vDummy: string;
begin
  AParent := '';
  vAll := Member(ASchema, 'allOf');
  if vAll is TRALJSONArray then
    for vInt := 0 to TRALJSONArray(vAll).Count - 1 do
    begin
      vSub := TRALJSONArray(vAll).Get(vInt);
      if (Text(vSub, '$ref') <> '') and (AParent = '') then
        AParent := RefName(Text(vSub, '$ref'))
      else
        CollectProps(Resolve(vSub), AProps, ARequired, vDummy);
    end;
  vProps := Member(ASchema, 'properties');
  if vProps is TRALJSONObject then
    for vInt := 0 to TRALJSONObject(vProps).Count - 1 do
      AProps.AddObject(string(TRALJSONObject(vProps).GetName(vInt)),
        TRALJSONObject(vProps).Get(vInt));
  vReq := Member(ASchema, 'required');
  if vReq is TRALJSONArray then
    for vInt := 0 to TRALJSONArray(vReq).Count - 1 do
      ARequired.Add(string(TRALJSONArray(vReq).Get(vInt).AsString));
end;

function TImporter.IsClassSchema(const AName: string): boolean;
var
  vSchema: TRALJSONValue;
  vKind: string;
begin
  vSchema := FSchemas.Get(StringRAL(AName));
  vKind := Text(vSchema, 'type');
  Result := (Member(vSchema, 'enum') = nil) and (vKind <> 'string') and (vKind <> 'integer') and
    (vKind <> 'number') and (vKind <> 'boolean') and (vKind <> 'array');
end;

procedure TImporter.EmitClass(const AName: string);
var
  vSchema: TRALJSONValue;
  vProps, vRequired, vOwned: TStringList;
  vParent, vType, vOwn, vProp, vField: string;
  vInt: Integer;
  vKind: string;
begin
  if FDone.IndexOf(AName) >= 0 then
    Exit;
  FDone.Add(AName);
  vSchema := FSchemas.Get(StringRAL(AName));
  if not IsClassSchema(AName) then
    Exit; // a value type, not a class
  vProps := TStringList.Create;
  vRequired := TStringList.Create;
  vOwned := TStringList.Create;
  try
    CollectProps(vSchema, vProps, vRequired, vParent);
    if vParent <> '' then
      EmitClass(vParent); // the parent is declared first
    if vParent = '' then
      vParent := 'TRALObject'
    else
      vParent := ClassOf(vParent);
    if Text(vSchema, 'description') <> '' then
      FOut.Add('  /// ' + StringReplace(StringReplace(Text(vSchema, 'description'), #13, ' ',
        [rfReplaceAll]), #10, ' ', [rfReplaceAll]))
    else
      FOut.Add('  /// Schema ' + AName + '.');
    FOut.Add('  ' + ClassOf(AName) + ' = class(' + vParent + ')');
    FOut.Add('  private');
    for vInt := 0 to vProps.Count - 1 do
    begin
      vType := PropType(TRALJSONValue(vProps.Objects[vInt]), vOwn);
      FOut.Add('    F' + RALPascalIdent(vProps[vInt]) + ': ' + vType + ';');
      if vOwn <> '' then
        vOwned.Add(RALPascalIdent(vProps[vInt]) + '=' + vOwn);
    end;
    FOut.Add('  public');
    if vOwned.Count > 0 then
    begin
      FOut.Add('    constructor Create; override;');
      FOut.Add('    destructor Destroy; override;');
    end;
    FOut.Add('  published');
    for vInt := 0 to vProps.Count - 1 do
    begin
      vType := PropType(TRALJSONValue(vProps.Objects[vInt]), vOwn);
      vProp := RALPascalIdent(vProps[vInt]);
      vField := 'F' + vProp;
      { the API's own name, so the class reads and writes it whatever the naming }
      FNames.Add('  RALRegisterPropertyName(' + ClassOf(AName) + ', ''' + vProp + ''', ''' +
        StringReplace(vProps[vInt], '''', '''''', [rfReplaceAll]) + ''');');
      if Text(TRALJSONValue(vProps.Objects[vInt]), 'description') <> '' then
        FOut.Add('    /// ' + StringReplace(StringReplace(Text(TRALJSONValue(vProps.Objects[vInt]),
          'description'), #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]))
      else if vRequired.IndexOf(vProps[vInt]) >= 0 then
        FOut.Add('    /// ' + vProps[vInt] + ' (required).')
      else
        FOut.Add('    /// ' + vProps[vInt] + '.');
      if vOwn <> '' then
        FOut.Add('    property ' + vProp + ': ' + vType + ' read ' + vField + ';')
      else
        FOut.Add('    property ' + vProp + ': ' + vType + ' read ' + vField + ' write ' + vField + ';');
    end;
    FOut.Add('  end;');
    FOut.Add('');
    { the bodies go to the implementation, kept in FClasses' objects }
    if vOwned.Count > 0 then
      FClasses.Add(ClassOf(AName) + '|' + StringReplace(vOwned.Text, sLineBreak, ';',
        [rfReplaceAll]));
  finally
    vProps.Free;
    vRequired.Free;
    vOwned.Free;
  end;
end;

function TImporter.Run: string;
var
  vImpl, vClient: TStringList;
  vInt, vJ, vK: Integer;
  vParts, vOwned: TStringList;
  vName, vClientName, vTitle: string;
  vPaths, vPathItem, vOp, vParams, vBody, vResp, vSchema: TRALJSONValue;
  vVerb, vPath, vMethodName, vArgs, vCall, vRoute, vBodyType, vResType, vOwn, vQuery: string;
  vVerbs: array[0..4] of string;
  vParam: TRALJSONValue;
  vPName, vPIn, vPType: string;
begin
  vVerbs[0] := 'get'; vVerbs[1] := 'post'; vVerbs[2] := 'put'; vVerbs[3] := 'delete';
  vVerbs[4] := 'patch';
  FOut.Add('/// Generated by RALOpenAPIImport from "' + Text(Member(FDoc, 'info'), 'title') +
    '" ' + Text(Member(FDoc, 'info'), 'version') + '. Do not edit: import again.');
  FOut.Add('unit ' + FOptions.UnitName + ';');
  FOut.Add('');
  FOut.Add('{$IFDEF FPC}');
  FOut.Add('  {$MODE DELPHI}');
  FOut.Add('{$ENDIF}');
  FOut.Add('');
  FOut.Add('interface');
  FOut.Add('');
  FOut.Add('uses');
  FOut.Add('  Classes, SysUtils, Contnrs, Variants,');
  FOut.Add('  RALTypes, RALObjectJSON, RALClient, RALObjectClient;');
  FOut.Add('');
  FOut.Add('type');
  { forward declarations of every class, so properties may refer to any }
  if FSchemas <> nil then
    for vInt := 0 to FSchemas.Count - 1 do
      if IsClassSchema(string(FSchemas.GetName(vInt))) then
        FOut.Add('  ' + ClassOf(string(FSchemas.GetName(vInt))) + ' = class;');
  FOut.Add('');
  vJ := FOut.Count; // the list classes go here, once known
  if FSchemas <> nil then
    for vInt := 0 to FSchemas.Count - 1 do
      EmitClass(string(FSchemas.GetName(vInt)));
  for vInt := FLists.Count - 1 downto 0 do
  begin
    FOut.Insert(vJ, '');
    FOut.Insert(vJ, '  ' + FLists.Names[vInt] + ' = class(TObjectList);');
    FOut.Insert(vJ, '  /// A list of ' + FLists.ValueFromIndex[vInt] + '.');
  end;

  vImpl := TStringList.Create;
  vClient := TStringList.Create;
  vParts := TStringList.Create;
  vOwned := TStringList.Create;
  try
    for vInt := 0 to FClasses.Count - 1 do
    begin
      vName := Copy(FClasses[vInt], 1, Pos('|', FClasses[vInt]) - 1);
      vOwned.Text := StringReplace(Copy(FClasses[vInt], Pos('|', FClasses[vInt]) + 1, MaxInt),
        ';', sLineBreak, [rfReplaceAll]);
      vImpl.Add('constructor ' + vName + '.Create;');
      vImpl.Add('begin');
      vImpl.Add('  inherited Create;');
      for vJ := 0 to vOwned.Count - 1 do
        if vOwned.Names[vJ] <> '' then
          if FLists.IndexOfName(vOwned.ValueFromIndex[vJ]) >= 0 then
            vImpl.Add('  F' + vOwned.Names[vJ] + ' := ' + vOwned.ValueFromIndex[vJ] + '.Create(True);')
          else
            vImpl.Add('  F' + vOwned.Names[vJ] + ' := ' + vOwned.ValueFromIndex[vJ] + '.Create;');
      vImpl.Add('end;');
      vImpl.Add('');
      vImpl.Add('destructor ' + vName + '.Destroy;');
      vImpl.Add('begin');
      for vJ := 0 to vOwned.Count - 1 do
        if vOwned.Names[vJ] <> '' then
          vImpl.Add('  F' + vOwned.Names[vJ] + '.Free;');
      vImpl.Add('  inherited;');
      vImpl.Add('end;');
      vImpl.Add('');
    end;

    if FOptions.WithClient and (Member(FDoc, 'paths') is TRALJSONObject) then
    begin
      vTitle := Text(Member(FDoc, 'info'), 'title');
      vClientName := FOptions.ClientName;
      if vClientName = '' then
        vClientName := 'T' + RALPascalIdent(vTitle) + 'Client';
      FOut.Add('  { The operations of the API, one method each: the status is returned,');
      FOut.Add('    the answer read into AResult on 2xx. }');
      FOut.Add('  ' + vClientName + ' = class(TRALObjectClient)');
      FOut.Add('  public');
      vPaths := Member(FDoc, 'paths');
      for vInt := 0 to TRALJSONObject(vPaths).Count - 1 do
      begin
        vPath := string(TRALJSONObject(vPaths).GetName(vInt));
        vPathItem := TRALJSONObject(vPaths).Get(vInt);
        for vK := 0 to High(vVerbs) do
        begin
          vOp := Member(vPathItem, vVerbs[vK]);
          if vOp = nil then
            Continue;
          vVerb := vVerbs[vK];
          vMethodName := Text(vOp, 'operationId');
          if vMethodName = '' then
            vMethodName := vVerb + '_' + vPath;
          vMethodName := RALPascalIdent(vMethodName);
          vArgs := '';
          vRoute := '''' + vPath + '''';
          vQuery := '';
          vParams := Member(vOp, 'parameters');
          if vParams is TRALJSONArray then
            for vJ := 0 to TRALJSONArray(vParams).Count - 1 do
            begin
              vParam := Resolve(TRALJSONArray(vParams).Get(vJ));
              vPName := Text(vParam, 'name');
              vPIn := Text(vParam, 'in');
              vPType := PropType(Member(vParam, 'schema'), vOwn);
              if vOwn <> '' then
                vPType := 'StringRAL';
              if (vPIn <> 'path') and (vPIn <> 'query') then
                Continue;
              vArgs := vArgs + 'const A' + RALPascalIdent(vPName) + ': ' + vPType + '; ';
              if vPIn = 'path' then
                vRoute := StringReplace(vRoute, '{' + vPName + '}', ''' + ToText(A' +
                  RALPascalIdent(vPName) + ') + ''', [rfReplaceAll])
              else
              begin
                if vQuery = '' then
                  vQuery := ' + ''?'
                else
                  vQuery := vQuery + ' + ''&';
                vQuery := vQuery + vPName + '='' + ToText(A' + RALPascalIdent(vPName) + ')';
              end;
            end;
          vBodyType := '';
          vBody := Member(Member(Member(Member(vOp, 'requestBody'), 'content'),
            'application/json'), 'schema');
          if vBody <> nil then
          begin
            vBodyType := PropType(vBody, vOwn);
            if vOwn = '' then
              vBodyType := 'TObject';
            vArgs := vArgs + 'ABody: ' + vBodyType + '; ';
          end;
          vResType := '';
          vResp := Member(vOp, 'responses');
          if vResp is TRALJSONObject then
            for vJ := 0 to TRALJSONObject(vResp).Count - 1 do
              if Copy(string(TRALJSONObject(vResp).GetName(vJ)), 1, 1) = '2' then
              begin
                vSchema := Member(Member(Member(Resolve(TRALJSONObject(vResp).Get(vJ)), 'content'),
                  'application/json'), 'schema');
                if vSchema <> nil then
                  vResType := 'TObject';
                Break;
              end;
          if vResType <> '' then
            vArgs := vArgs + 'AResult: TObject; ';
          if vArgs <> '' then
            vArgs := '(' + Copy(vArgs, 1, Length(vArgs) - 2) + ')';
          FOut.Add('    /// ' + UpperCase(vVerb) + ' ' + vPath + StringReplace(StringReplace(
            ' ' + Text(vOp, 'summary'), #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]));
          FOut.Add('    function ' + vMethodName + vArgs + ': IntegerRAL;');
          vCall := 'Execute(am' + UpperCase(vVerb) + ', StringRAL(' + vRoute + vQuery + '), ';
          if vBodyType <> '' then
            vCall := vCall + 'ABody, '
          else
            vCall := vCall + 'nil, ';
          if vResType <> '' then
            vCall := vCall + 'AResult)'
          else
            vCall := vCall + 'nil)';
          vClient.Add('function ' + vClientName + '.' + vMethodName + vArgs + ': IntegerRAL;');
          vClient.Add('begin');
          vClient.Add('  Result := ' + vCall + ';');
          vClient.Add('end;');
          vClient.Add('');
        end;
      end;
      FOut.Add('  end;');
      FOut.Add('');
    end;

    FOut.Add('implementation');
    FOut.Add('');
    if vClient.Count > 0 then
    begin
      FOut.Add('function ToText(const AValue: Variant): string;');
      FOut.Add('begin');
      FOut.Add('  Result := string(AValue);');
      FOut.Add('end;');
      FOut.Add('');
    end;
    FOut.AddStrings(vImpl);
    FOut.AddStrings(vClient);
    FOut.Add('initialization');
    FOut.AddStrings(FNames);
  for vInt := 0 to FLists.Count - 1 do
      FOut.Add('  RALRegisterListClass(' + FLists.Names[vInt] + ', ' + FLists.ValueFromIndex[vInt] + ');');
    if FSchemas <> nil then
      for vInt := 0 to FSchemas.Count - 1 do
        if IsClassSchema(string(FSchemas.GetName(vInt))) then
          FOut.Add('  RALRegisterObjectClass(' + ClassOf(string(FSchemas.GetName(vInt))) + ');');
    FOut.Add('');
    FOut.Add('end.');
    Result := FOut.Text;
  finally
    vImpl.Free;
    vClient.Free;
    vParts.Free;
    vOwned.Free;
  end;
end;

function RALOpenAPIToPascal(const AJSON: StringRAL; const AOptions: TRALOpenAPIImportOptions): string;
var
  vDoc: TRALJSONValue;
  vImporter: TImporter;
begin
  vDoc := TRALJSON.ParseJSON(AJSON);
  try
    if not (vDoc is TRALJSONObject) or (TRALJSONObject(vDoc).Get('openapi') = nil) or
       (Copy(string(TRALJSONObject(vDoc).Get('openapi').AsString), 1, 2) <> '3.') then
      raise ERALOpenAPIImport.Create('Not an OpenAPI 3 document (JSON)');
    vImporter := TImporter.Create(TRALJSONObject(vDoc), AOptions);
    try
      Result := vImporter.Run;
    finally
      vImporter.Free;
    end;
  finally
    vDoc.Free;
  end;
end;

end.
