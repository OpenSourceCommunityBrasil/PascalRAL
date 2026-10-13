/// Types shared by the SOAP server and client: versions, namespaces, faults, XSD types.
unit RALSOAPTypes;

{$I ../../base/PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALRoutes, RALMIMETypes, RALXMLReader, RALXML;

const
  /// Envelope namespace of SOAP 1.1.
  RALSOAP11Namespace = 'http://schemas.xmlsoap.org/soap/envelope/';
  /// Envelope namespace of SOAP 1.2.
  RALSOAP12Namespace = 'http://www.w3.org/2003/05/soap-envelope';
  /// Namespace of WSDL 1.1.
  RALWSDLNamespace = 'http://schemas.xmlsoap.org/wsdl/';
  /// Namespace of the SOAP 1.1 binding of WSDL 1.1.
  RALWSDLSOAP11Namespace = 'http://schemas.xmlsoap.org/wsdl/soap/';
  /// Namespace of the SOAP 1.2 binding of WSDL 1.1.
  RALWSDLSOAP12Namespace = 'http://schemas.xmlsoap.org/wsdl/soap12/';
  /// Namespace of XML Schema.
  RALXSDNamespace = 'http://www.w3.org/2001/XMLSchema';
  /// Namespace of the XML Schema instance attributes.
  RALXSINamespace = 'http://www.w3.org/2001/XMLSchema-instance';

type
  /// Version of SOAP.
  TRALSOAPVersion = (sv11, sv12);
  /// Set of SOAP versions.
  TRALSOAPVersions = set of TRALSOAPVersion;

  /// Style of an operation: document/literal wrapped or rpc/literal.
  TRALSOAPStyle = (ssDocument, ssRPC);

  { Code of a fault: the four of SOAP 1.2 plus DataEncodingUnknown; SOAP 1.1 calls
    Sender Client and Receiver Server. }
  TRALSOAPFaultCode = (sfcVersionMismatch, sfcMustUnderstand, sfcDataEncodingUnknown,
    sfcSender, sfcReceiver);

  /// A SOAP fault: raised by a handler to answer it, and by the client when one arrives.
  ERALSOAPFault = class(Exception)
  private
    FCode: TRALSOAPFaultCode;
    FCodeText: StringRAL;
    FDetail: StringRAL;
    FHTTPStatus: IntegerRAL;
    FReason: StringRAL;
  public
    /// A fault of ACode with the text AReason and, optionally, the XML ADetail.
    constructor CreateFault(ACode: TRALSOAPFaultCode; const AReason: StringRAL;
      const ADetail: StringRAL = '');

    /// Code of the fault.
    property Code: TRALSOAPFaultCode read FCode write FCode;
    /// Code as written in the envelope, prefix included.
    property CodeText: StringRAL read FCodeText write FCodeText;
    /// XML of the detail element's content, or ''.
    property Detail: StringRAL read FDetail write FDetail;
    /// HTTP status the fault arrived with, on the client; 0 on the server.
    property HTTPStatus: IntegerRAL read FHTTPStatus write FHTTPStatus;
    /// Text of the fault.
    property Reason: StringRAL read FReason write FReason;
  end;

/// Envelope namespace of AVersion.
function RALSOAPNamespace(AVersion: TRALSOAPVersion): StringRAL;
/// Content type of an envelope of AVersion: text/xml (1.1), application/soap+xml (1.2).
function RALSOAPContentType(AVersion: TRALSOAPVersion): StringRAL;
/// HTTP status of a fault of ACode in AVersion: 400 for a 1.2 Sender, 500 otherwise.
function RALSOAPFaultStatus(AVersion: TRALSOAPVersion; ACode: TRALSOAPFaultCode): IntegerRAL;
/// Name of the fault code in AVersion, without prefix.
function RALSOAPFaultCodeName(AVersion: TRALSOAPVersion; ACode: TRALSOAPFaultCode): StringRAL;
/// Fault code named AName (prefix optional), in either version; sfcReceiver if unknown.
function RALSOAPFaultCodeOf(const AName: StringRAL): TRALSOAPFaultCode;
/// XSD type (with the xs prefix) of a route param type.
function RALSOAPXSDType(AType: TRALRouteParamType): StringRAL;
{ Writes into AEnvelope (a new document) an envelope of AVersion holding a fault of
  ACode with AReason and the XML ADetail. }
procedure RALSOAPBuildFault(AEnvelope: TRALXMLDocument; AVersion: TRALSOAPVersion;
  ACode: TRALSOAPFaultCode; const AReason, ADetail: StringRAL);
{ Returns the fault the envelope ADocument holds, as an exception the caller raises or
  frees; nil when it holds none. }
function RALSOAPReadFault(ADocument: TRALXMLDocument): ERALSOAPFault;
/// Value of the parameter AName of the media type AType, without quotes; '' when absent.
function RALMediaTypeParam(const AType, AName: StringRAL): StringRAL;
/// AText without the double quotes around it, if any.
function RALSOAPUnquote(const AText: StringRAL): StringRAL;

implementation

uses
  RALTools;

function RALSOAPNamespace(AVersion: TRALSOAPVersion): StringRAL;
begin
  if AVersion = sv12 then
    Result := RALSOAP12Namespace
  else
    Result := RALSOAP11Namespace;
end;

function RALSOAPContentType(AVersion: TRALSOAPVersion): StringRAL;
begin
  if AVersion = sv12 then
    Result := rctAPPLICATIONSOAPXML
  else
    Result := rctTEXTXML;
end;

function RALSOAPFaultStatus(AVersion: TRALSOAPVersion; ACode: TRALSOAPFaultCode): IntegerRAL;
begin
  // SOAP 1.2 part 2, 7.5.1: Sender is 400, the rest 500; SOAP 1.1 answers every fault 500
  if (AVersion = sv12) and (ACode = sfcSender) then
    Result := HTTP_BadRequest
  else
    Result := HTTP_InternalError;
end;

function RALSOAPFaultCodeName(AVersion: TRALSOAPVersion; ACode: TRALSOAPFaultCode): StringRAL;
begin
  case ACode of
    sfcVersionMismatch:
      Result := 'VersionMismatch';
    sfcMustUnderstand:
      Result := 'MustUnderstand';
    sfcDataEncodingUnknown:
      Result := 'DataEncodingUnknown';
    sfcSender:
      if AVersion = sv12 then Result := 'Sender' else Result := 'Client';
  else
    if AVersion = sv12 then Result := 'Receiver' else Result := 'Server';
  end;
end;

function RALSOAPFaultCodeOf(const AName: StringRAL): TRALSOAPFaultCode;
var
  vName: StringRAL;
  vPos: IntegerRAL;
begin
  vName := AName;
  vPos := Pos(StringRAL(':'), vName);
  if vPos > 0 then
    vName := Copy(vName, vPos + 1, MaxInt);
  // a 1.1 code may be refined with dots: Client.Authentication
  vPos := Pos(StringRAL('.'), vName);
  if vPos > 0 then
    vName := Copy(vName, 1, vPos - 1);

  if vName = 'VersionMismatch' then
    Result := sfcVersionMismatch
  else if vName = 'MustUnderstand' then
    Result := sfcMustUnderstand
  else if vName = 'DataEncodingUnknown' then
    Result := sfcDataEncodingUnknown
  else if (vName = 'Sender') or (vName = 'Client') then
    Result := sfcSender
  else
    Result := sfcReceiver;
end;

function RALSOAPXSDType(AType: TRALRouteParamType): StringRAL;
begin
  case AType of
    prtBoolean: Result := 'xs:boolean';
    prtInteger: Result := 'xs:long';
    prtNumber: Result := 'xs:double';
    prtDecimal: Result := 'xs:decimal';
    prtDate: Result := 'xs:date';
    prtTime: Result := 'xs:time';
    prtDateTime: Result := 'xs:dateTime';
    prtBase64: Result := 'xs:base64Binary';
    prtXML: Result := 'xs:anyType';
  else
    Result := 'xs:string';
  end;
end;

procedure RALSOAPBuildFault(AEnvelope: TRALXMLDocument; AVersion: TRALSOAPVersion;
  ACode: TRALSOAPFaultCode; const AReason, ADetail: StringRAL);
var
  vFault, vNode: TRALXMLNode;
begin
  AEnvelope.Clear;
  vFault := AEnvelope.CreateRoot('soap:Envelope')
    .AddNamespace('soap', RALSOAPNamespace(AVersion))
    .AddChild('soap:Body')
    .AddChild('soap:Fault');

  if AVersion = sv12 then
  begin
    vFault.AddChild('soap:Code').AddChild('soap:Value')
      .WithText('soap:' + RALSOAPFaultCodeName(AVersion, ACode));
    vFault.AddChild('soap:Reason').AddChild('soap:Text')
      .AddAttribute('xml:lang', 'en').WithText(AReason);
    if ADetail <> '' then
      vFault.AddChild('soap:Detail').AddXML(ADetail);
  end
  else
  begin
    // SOAP 1.1: the children of Fault are unqualified
    vFault.AddChild('faultcode').WithText('soap:' + RALSOAPFaultCodeName(AVersion, ACode));
    vFault.AddChild('faultstring').WithText(AReason);
    if ADetail <> '' then
    begin
      vNode := vFault.AddChild('detail');
      vNode.AddXML(ADetail);
    end;
  end;
end;

function RALSOAPReadFault(ADocument: TRALXMLDocument): ERALSOAPFault;
var
  vRoot, vBody, vFault, vNode: TRALXMLNode;
  vCode, vReason, vDetail: StringRAL;
  vChild: TRALXMLNode;
begin
  Result := nil;
  vRoot := ADocument.Root;
  if vRoot = nil then
    Exit;
  vBody := vRoot.Child('Body');
  if vBody = nil then
    Exit;
  vFault := vBody.Child('Fault');
  if vFault = nil then
    Exit;

  vCode := '';
  vReason := '';
  vDetail := '';
  if vFault.NamespaceURI = RALSOAP12Namespace then
  begin
    vNode := vFault.GetNode('Code/Value');
    if vNode <> nil then
      vCode := RALTrim(vNode.Text);
    vNode := vFault.GetNode('Reason/Text');
    if vNode <> nil then
      vReason := vNode.Text;
    vNode := vFault.Child('Detail');
  end
  else
  begin
    vNode := vFault.Child('faultcode');
    if vNode <> nil then
      vCode := RALTrim(vNode.Text);
    vNode := vFault.Child('faultstring');
    if vNode <> nil then
      vReason := vNode.Text;
    vNode := vFault.Child('detail');
  end;

  if vNode <> nil then
  begin
    vChild := vNode.FirstChild;
    while vChild <> nil do
    begin
      vDetail := vDetail + vChild.ToXML(False);
      vChild := vChild.NextSibling;
    end;
  end;

  Result := ERALSOAPFault.CreateFault(RALSOAPFaultCodeOf(vCode), vReason, vDetail);
  Result.CodeText := vCode;
end;

function RALMediaTypeParam(const AType, AName: StringRAL): StringRAL;
var
  vRest, vPart, vKey: StringRAL;
  vPos, vEq: IntegerRAL;
  vQuoted: boolean;
begin
  Result := '';
  vPos := Pos(StringRAL(';'), AType);
  if vPos = 0 then
    Exit;
  vRest := Copy(AType, vPos + 1, MaxInt);
  while vRest <> '' do
  begin
    // a parameter ends at the next ';' outside quotes
    vQuoted := False;
    vPos := 0;
    for vEq := 1 to Length(vRest) do
    begin
      if vRest[vEq] = '"' then
        vQuoted := not vQuoted
      else if (vRest[vEq] = ';') and not vQuoted then
      begin
        vPos := vEq;
        Break;
      end;
    end;
    if vPos = 0 then
    begin
      vPart := vRest;
      vRest := '';
    end
    else
    begin
      vPart := Copy(vRest, 1, vPos - 1);
      vRest := Copy(vRest, vPos + 1, MaxInt);
    end;

    vEq := Pos(StringRAL('='), vPart);
    if vEq = 0 then
      Continue;
    vKey := RALTrim(Copy(vPart, 1, vEq - 1));
    if RALSameName(vKey, AName) then
    begin
      Result := RALSOAPUnquote(RALTrim(Copy(vPart, vEq + 1, MaxInt)));
      Exit;
    end;
  end;
end;

function RALSOAPUnquote(const AText: StringRAL): StringRAL;
begin
  Result := RALTrim(AText);
  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

{ ERALSOAPFault }

constructor ERALSOAPFault.CreateFault(ACode: TRALSOAPFaultCode; const AReason,
  ADetail: StringRAL);
begin
  inherited Create(string(AReason));
  FCode := ACode;
  FReason := AReason;
  FDetail := ADetail;
  FCodeText := RALSOAPFaultCodeName(sv11, ACode);
end;

end.
