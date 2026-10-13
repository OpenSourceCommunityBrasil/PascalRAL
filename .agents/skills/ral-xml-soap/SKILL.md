---
name: ral-xml-soap
description: >
  Uses PascalRAL 2.0's XML and SOAP: the pull reader, writer and DOM (RALXML), datasets
  as XML (TRALStorageXMLLink), a SOAP 1.1/1.2 server with WSDL (TRALSOAPModule) and a
  SOAP client from a WSDL (TRALSOAPClient). Use for "parse/build XML", "SOAP service",
  "consume a WSDL", "web service for .NET/Java", or a SOAP Fault.
---

# XML and SOAP

Two packages: `RALModuleXMLPackage` (`src/modules/xml`: `RALXMLReader`, `RALXMLWriter`,
`RALXML`, `RALStorageXML`; requires `RALStoragePackage`) and `RALModuleSOAPPackage`
(+ `RALModuleSOAPDsgn`; `src/modules/soap`; requires the XML one). Pure Pascal, the
same on every compiler and platform.

## XML document

```pascal
uses RALXML;

Doc := TRALXMLDocument.Create;
try
  Doc.Indent := True;
  Doc.CreateRoot('order').AddAttribute('number', '42')
    .AddChild('customer').WithText('Maria');
  Doc.Root.AddChild('total').WithValue(19.9);
  Text := Doc.SaveToString;

  Doc.LoadFromString(Text);                  // or LoadFromFile / LoadFromStream
  Customer := Doc.GetText('/order/customer');
  Total := Doc.GetNode('/order/total').AsFloat;
finally
  Doc.Free;
end;
```

Typed values: `WithValue`, `WithDateTime` (ISO 8601), `WithBCD` (exact decimal),
`WithBase64`. A dataset: `AppendDataSet(Query, xtDBWare)`. For large input use the pull
reader (`RALXMLReader`) instead of the DOM. DOCTYPE is refused.

## Datasets as XML

`TRALStorageXMLLink` is a storage link like the BIN/JSON ones (`ral-dbware`); in
automatic mode it uses FireDAC's or `TBufDataset`'s native XML when both sides use the
same library. FireDAC's XML is MSXML (COM): RAL initializes COM around it for server
threads and console programs.

## SOAP server

1. `TRALSOAPModule` (palette `RAL - Modules`), `Server` set; `Domain` (`/calc`),
   `Namespace` (`urn:calc`), `ServiceName`.
2. `Operations`: one item per operation, with `Name`, `InputParams`/`OutputParams`
   (they generate the WSDL) and `OnExecute` - or `ResultDataSet` to answer a query
   without code.
3. The WSDL is any GET on the service (`/calc?wsdl`).

```pascal
procedure TForm1.SumExecute(ARequest: TRALSOAPRequest; AResponse: TRALSOAPResponse);
begin
  AResponse.AddResult('SumResult').WithValue(Int64(
    StrToInt(ARequest.ParamValue('a')) + StrToInt(ARequest.ParamValue('b'))));
end;
```

- The operation is chosen by `SOAPAction` (1.1) or the `action` of the Content-Type
  (1.2), else by the first element of the Body; also at `Domain/Name`. Only the head of
  the envelope is read before authentication.
- Every exception becomes a `Fault` in the version's format; `HideErrorDetails` applies.
- A service with both document and rpc operations gets one binding per style (WS-I).

## SOAP client

`TRALSOAPClient` **is** a `TRALClient` (engines, failover, TLS, auth). In the IDE: right
click > **Import WSDL...** fills `Operations`, `Namespace`, `BaseURL`, `ServicePath`;
the `TRALSOAPExecute` action calls an operation with no code; `ResultDataSet` loads a
dataset answer.

```pascal
with RALSOAPClient1.Operations['Sum'] do
begin
  Params['a'].Value := '2';
  Params['b'].Value := '3';
  if Execute then
    ShowMessage(Results.ValueOf('SumResult'));
end;
```

It sends uncompressed (`CompressType := ctNone`): most SOAP stacks (WCF answers 400)
do not read a gzip request. The charset of a request comes from its `Content-Type`
header, as SOAP requires.

Guides (Portuguese): wiki
[xml](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/xml),
[storage-xml](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/storage-xml),
[modulo-soap](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/modulo-soap),
[cliente-soap](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/cliente-soap).
