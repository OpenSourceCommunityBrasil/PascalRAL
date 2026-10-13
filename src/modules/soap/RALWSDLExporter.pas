/// Generates the WSDL 1.1 of a SOAP module, with SOAP 1.1 and 1.2 bindings.
unit RALWSDLExporter;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALRoutes, RALXML, RALSOAPTypes;

type
  { Writes the WSDL of a module: XSD types, messages, port types, bindings and
    service. Operations of each style get a port type and bindings of their own,
    as WS-I Basic Profile 1.1 asks. }
  TRALWSDLExporter = class
  private
    /// Adds to ADefs the binding of AVersion for the operations of AStyle, after ABase.
    procedure AddBinding(ADefs: TRALXMLNode; AModule: TObject; AVersion: TRALSOAPVersion;
      AStyle: TRALSOAPStyle; const ABase: StringRAL);
    /// Adds an element per param to ASequence; xs:any for none when AAnyWhenEmpty.
    procedure AddElementParams(ASequence: TRALXMLNode; AParams: TRALRouteParams; AAnyWhenEmpty: boolean);
    /// Adds to ADefs the port type of the operations of AStyle, named after ABase.
    procedure AddPortType(ADefs: TRALXMLNode; AModule: TObject; AStyle: TRALSOAPStyle;
      const ABase: StringRAL);
  public
    /// The WSDL of AModule (a TRALSOAPModule) for a service at AURL.
    function Generate(AModule: TObject; const AURL: StringRAL): StringRAL;
    /// Writes the WSDL of AModule for a service at AURL into the file AFileName.
    procedure SaveToFile(AModule: TObject; const AURL: StringRAL; const AFileName: string);
  end;

implementation

uses
  RALSOAPModule;

/// Operation AIndex of AModule.
function OperationOf(AModule: TRALSOAPModule; AIndex: IntegerRAL): TRALSOAPOperation;
begin
  Result := TRALSOAPOperation(AModule.Operations.Items[AIndex]);
end;

/// AModule has an operation of AStyle.
function HasStyle(AModule: TRALSOAPModule; AStyle: TRALSOAPStyle): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to AModule.Operations.Count - 1 do
    if OperationOf(AModule, vInt).Style = AStyle then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TRALWSDLExporter.AddElementParams(ASequence: TRALXMLNode; AParams: TRALRouteParams;
  AAnyWhenEmpty: boolean);
var
  vInt: IntegerRAL;
  vParam: TRALRouteParam;
  vNode: TRALXMLNode;
begin
  for vInt := 0 to AParams.Count - 1 do
  begin
    vParam := TRALRouteParam(AParams.Items[vInt]);
    vNode := ASequence.AddChild('xs:element')
      .AddAttribute('name', vParam.ParamName)
      .AddAttribute('type', RALSOAPXSDType(vParam.ParamType));
    if not vParam.Required then
      vNode.AddAttribute('minOccurs', '0');
    if vParam.Description.Text <> '' then
      vNode.AddChild('xs:annotation').AddChild('xs:documentation')
        .WithText(StringRAL(TrimRight(vParam.Description.Text)));
  end;

  // an answer the WSDL cannot list (a dataset) is any content
  if (AParams.Count = 0) and AAnyWhenEmpty then
    ASequence.AddChild('xs:any')
      .AddAttribute('processContents', 'lax')
      .AddAttribute('minOccurs', '0')
      .AddAttribute('maxOccurs', 'unbounded');
end;

procedure TRALWSDLExporter.AddPortType(ADefs: TRALXMLNode; AModule: TObject;
  AStyle: TRALSOAPStyle; const ABase: StringRAL);
var
  vModule: TRALSOAPModule;
  vPortType, vOperation: TRALXMLNode;
  vInt: IntegerRAL;
  vOp: TRALSOAPOperation;
begin
  vModule := TRALSOAPModule(AModule);
  vPortType := ADefs.AddChild('wsdl:portType').AddAttribute('name', ABase + 'PortType');
  for vInt := 0 to vModule.Operations.Count - 1 do
  begin
    vOp := OperationOf(vModule, vInt);
    if vOp.Style <> AStyle then
      Continue;
    vOperation := vPortType.AddChild('wsdl:operation').AddAttribute('name', vOp.Name);
    if TrimRight(vOp.Description.Text) <> '' then
      vOperation.AddChild('wsdl:documentation')
        .WithText(StringRAL(TrimRight(vOp.Description.Text)));
    vOperation.AddChild('wsdl:input').AddAttribute('message', 'tns:' + vOp.Name + 'Request');
    vOperation.AddChild('wsdl:output').AddAttribute('message', 'tns:' + vOp.Name + 'Response');
  end;
end;

procedure TRALWSDLExporter.AddBinding(ADefs: TRALXMLNode; AModule: TObject;
  AVersion: TRALSOAPVersion; AStyle: TRALSOAPStyle; const ABase: StringRAL);
var
  vModule: TRALSOAPModule;
  vBinding, vOperation, vNode: TRALXMLNode;
  vInt: IntegerRAL;
  vOp: TRALSOAPOperation;
  vPrefix, vName, vStyle: StringRAL;
begin
  vModule := TRALSOAPModule(AModule);
  if AVersion = sv12 then
  begin
    vPrefix := 'soap12';
    vName := ABase + 'Soap12';
  end
  else
  begin
    vPrefix := 'soap';
    vName := ABase + 'Soap';
  end;
  if AStyle = ssRPC then
    vStyle := 'rpc'
  else
    vStyle := 'document';

  vBinding := ADefs.AddChild('wsdl:binding')
    .AddAttribute('name', vName)
    .AddAttribute('type', 'tns:' + ABase + 'PortType');
  vBinding.AddChild(vPrefix + ':binding')
    .AddAttribute('transport', 'http://schemas.xmlsoap.org/soap/http')
    .AddAttribute('style', vStyle);

  for vInt := 0 to vModule.Operations.Count - 1 do
  begin
    vOp := OperationOf(vModule, vInt);
    if vOp.Style <> AStyle then
      Continue;
    vOperation := vBinding.AddChild('wsdl:operation').AddAttribute('name', vOp.Name);
    vOperation.AddChild(vPrefix + ':operation')
      .AddAttribute('soapAction', vOp.EffectiveAction)
      .AddAttribute('style', vStyle);

    vNode := vOperation.AddChild('wsdl:input').AddChild(vPrefix + ':body')
      .AddAttribute('use', 'literal');
    if AStyle = ssRPC then
      vNode.AddAttribute('namespace', vOp.EffectiveNamespace);
    vNode := vOperation.AddChild('wsdl:output').AddChild(vPrefix + ':body')
      .AddAttribute('use', 'literal');
    if AStyle = ssRPC then
      vNode.AddAttribute('namespace', vOp.EffectiveNamespace);
  end;
end;

function TRALWSDLExporter.Generate(AModule: TObject; const AURL: StringRAL): StringRAL;
var
  vModule: TRALSOAPModule;
  vDoc: TRALXMLDocument;
  vDefs, vSchema, vSeq, vNode, vService: TRALXMLNode;
  vInt, vPart: IntegerRAL;
  vOp: TRALSOAPOperation;
  vParam: TRALRouteParam;
  vNamespace, vBase: StringRAL;
  vStyle: TRALSOAPStyle;
  vMixed: boolean;
begin
  vModule := TRALSOAPModule(AModule);
  vNamespace := vModule.Namespace;
  vMixed := HasStyle(vModule, ssDocument) and HasStyle(vModule, ssRPC);

  vDoc := TRALXMLDocument.Create;
  try
    vDoc.Indent := True;
    vDefs := vDoc.CreateRoot('wsdl:definitions')
      .AddAttribute('name', vModule.ServiceName)
      .AddAttribute('targetNamespace', vNamespace)
      .AddNamespace('wsdl', RALWSDLNamespace)
      .AddNamespace('soap', RALWSDLSOAP11Namespace)
      .AddNamespace('soap12', RALWSDLSOAP12Namespace)
      .AddNamespace('xs', RALXSDNamespace)
      .AddNamespace('tns', vNamespace);

    // types: one wrapper element per operation and per answer (document/literal wrapped)
    vSchema := vDefs.AddChild('wsdl:types').AddChild('xs:schema')
      .AddAttribute('targetNamespace', vNamespace)
      .AddAttribute('elementFormDefault', 'qualified');
    for vInt := 0 to vModule.Operations.Count - 1 do
    begin
      vOp := OperationOf(vModule, vInt);
      if vOp.Style = ssRPC then
        Continue;
      vSeq := vSchema.AddChild('xs:element').AddAttribute('name', vOp.Name)
        .AddChild('xs:complexType').AddChild('xs:sequence');
      AddElementParams(vSeq, vOp.InputParams, False);
      vSeq := vSchema.AddChild('xs:element').AddAttribute('name', vOp.Name + 'Response')
        .AddChild('xs:complexType').AddChild('xs:sequence');
      AddElementParams(vSeq, vOp.OutputParams, True);
    end;

    // messages
    for vInt := 0 to vModule.Operations.Count - 1 do
    begin
      vOp := OperationOf(vModule, vInt);
      vNode := vDefs.AddChild('wsdl:message').AddAttribute('name', vOp.Name + 'Request');
      if vOp.Style = ssRPC then
      begin
        for vPart := 0 to vOp.InputParams.Count - 1 do
        begin
          vParam := TRALRouteParam(vOp.InputParams.Items[vPart]);
          vNode.AddChild('wsdl:part').AddAttribute('name', vParam.ParamName)
            .AddAttribute('type', RALSOAPXSDType(vParam.ParamType));
        end;
      end
      else
        vNode.AddChild('wsdl:part').AddAttribute('name', 'parameters')
          .AddAttribute('element', 'tns:' + vOp.Name);

      vNode := vDefs.AddChild('wsdl:message').AddAttribute('name', vOp.Name + 'Response');
      if vOp.Style = ssRPC then
      begin
        for vPart := 0 to vOp.OutputParams.Count - 1 do
        begin
          vParam := TRALRouteParam(vOp.OutputParams.Items[vPart]);
          vNode.AddChild('wsdl:part').AddAttribute('name', vParam.ParamName)
            .AddAttribute('type', RALSOAPXSDType(vParam.ParamType));
        end;
      end
      else
        vNode.AddChild('wsdl:part').AddAttribute('name', 'parameters')
          .AddAttribute('element', 'tns:' + vOp.Name + 'Response');
    end;

    { port types and bindings, one set per style present; a module of one style
      keeps the plain names }
    for vStyle := Low(TRALSOAPStyle) to High(TRALSOAPStyle) do
    begin
      if (vModule.Operations.Count > 0) and not HasStyle(vModule, vStyle) then
        Continue;
      if (vModule.Operations.Count = 0) and (vStyle <> ssDocument) then
        Continue;
      vBase := vModule.ServiceName;
      if vMixed and (vStyle = ssRPC) then
        vBase := vBase + 'Rpc';
      AddPortType(vDefs, vModule, vStyle, vBase);
    end;
    for vStyle := Low(TRALSOAPStyle) to High(TRALSOAPStyle) do
    begin
      if (vModule.Operations.Count > 0) and not HasStyle(vModule, vStyle) then
        Continue;
      if (vModule.Operations.Count = 0) and (vStyle <> ssDocument) then
        Continue;
      vBase := vModule.ServiceName;
      if vMixed and (vStyle = ssRPC) then
        vBase := vBase + 'Rpc';
      if sv11 in vModule.Versions then
        AddBinding(vDefs, vModule, sv11, vStyle, vBase);
      if sv12 in vModule.Versions then
        AddBinding(vDefs, vModule, sv12, vStyle, vBase);
    end;

    // service: a port per binding, all at the same address
    vService := vDefs.AddChild('wsdl:service').AddAttribute('name', vModule.ServiceName);
    for vStyle := Low(TRALSOAPStyle) to High(TRALSOAPStyle) do
    begin
      if (vModule.Operations.Count > 0) and not HasStyle(vModule, vStyle) then
        Continue;
      if (vModule.Operations.Count = 0) and (vStyle <> ssDocument) then
        Continue;
      vBase := vModule.ServiceName;
      if vMixed and (vStyle = ssRPC) then
        vBase := vBase + 'Rpc';
      if sv11 in vModule.Versions then
        vService.AddChild('wsdl:port')
          .AddAttribute('name', vBase + 'Soap')
          .AddAttribute('binding', 'tns:' + vBase + 'Soap')
          .AddChild('soap:address').AddAttribute('location', AURL);
      if sv12 in vModule.Versions then
        vService.AddChild('wsdl:port')
          .AddAttribute('name', vBase + 'Soap12')
          .AddAttribute('binding', 'tns:' + vBase + 'Soap12')
          .AddChild('soap12:address').AddAttribute('location', AURL);
    end;

    Result := vDoc.SaveToString(True);
  finally
    vDoc.Free;
  end;
end;

procedure TRALWSDLExporter.SaveToFile(AModule: TObject; const AURL: StringRAL;
  const AFileName: string);
var
  vStream: TFileStream;
  vText: StringRAL;
begin
  vText := Generate(AModule, AURL);
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    if vText <> '' then
      vStream.WriteBuffer(Pointer(vText)^, Length(vText));
  finally
    vStream.Free;
  end;
end;

end.
