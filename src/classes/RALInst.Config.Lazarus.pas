/// The two lists of a Lazarus configuration that an installation changes.
/// lazbuild --add-package-link writes packagefiles.xml (UserPkgLinks: name and
/// file of every .lpk the IDE knows) and --add-package writes
/// miscellaneousoptions.xml (StaticAutoInstallPackages: the names that go into
/// the IDE when it is rebuilt). lazbuild has no way back; uninstalling edits the
/// two lists, and only their items: the rest of the files stays as it is.
/// Undoing applies the inverse of the before->after change to the CURRENT
/// state: what the installation added leaves, what it changed goes back, and
/// what the user changed afterwards (another package, another link) stays.
unit RALInst.Config.Lazarus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

/// AAtual becomes AAtual without the change AAntes->ADepois: what the change
/// added leaves, what it removed comes back. AComValor: lists of name=value
/// (links), where a changed value goes back to the one before, but only while
/// it is still what the change left
procedure DesfazerMudanca(AAtual, AAntes, ADepois: TStrings; AComValor: boolean);
/// Writes the IDE packages list, keeping the rest of the file
function GravarInstalados(const APcp: string; ALista: TStrings;
  out AErro: string): boolean;
/// Writes the links list, keeping the rest of the file (and, for links that
/// already existed, their version and last-used date)
function GravarLinks(const APcp: string; ALista: TStrings; out AErro: string): boolean;
/// Names of the packages that go into the IDE (miscellaneousoptions.xml)
procedure LerInstalados(const APcp: string; ALista: TStrings);
/// name=file of the package links (packagefiles.xml); empty without the file
procedure LerLinks(const APcp: string; ALista: TStrings);

implementation

uses
  DOM, XMLRead, XMLWrite,
  RALInst.Mensagens;

function NovaLista: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
end;

function Filho(ANo: TDOMNode; const ANome: string): TDOMNode;
begin
  Result := nil;
  if ANo <> nil then
    Result := ANo.FindNode(DOMString(ANome));
end;

function Valor(ANo: TDOMNode; const AFilho: string): string;
var
  vNo: TDOMNode;
begin
  Result := '';
  vNo := Filho(ANo, AFilho);
  if (vNo is TDOMElement) then
    Result := string(TDOMElement(vNo).GetAttribute('Value'));
end;

function Ler(const AArquivo: string): TXMLDocument;
begin
  Result := nil;
  if FileExists(AArquivo) then
    ReadXMLFile(Result, AArquivo);
end;

function NoLinks(ADoc: TXMLDocument): TDOMNode;
begin
  Result := Filho(ADoc.DocumentElement, 'UserPkgLinks');
end;

function NoInstalados(ADoc: TXMLDocument): TDOMNode;
begin
  Result := Filho(Filho(Filho(ADoc.DocumentElement, 'MiscellaneousOptions'),
                        'BuildLazarusOptions'), 'StaticAutoInstallPackages');
end;

procedure LerLinks(const APcp: string; ALista: TStrings);
var
  vDoc: TXMLDocument;
  vNo, vItem: TDOMNode;
begin
  ALista.Clear;
  vDoc := Ler(IncludeTrailingPathDelimiter(APcp) + 'packagefiles.xml');
  if vDoc = nil then
    Exit;
  try
    vNo := NoLinks(vDoc);
    if vNo = nil then
      Exit;
    vItem := vNo.FirstChild;
    while vItem <> nil do
    begin
      if (vItem.NodeType = ELEMENT_NODE) and (Valor(vItem, 'Name') <> '') then
        ALista.Add(Valor(vItem, 'Name') + '=' + Valor(vItem, 'Filename'));
      vItem := vItem.NextSibling;
    end;
  finally
    vDoc.Free;
  end;
end;

procedure LerInstalados(const APcp: string; ALista: TStrings);
var
  vDoc: TXMLDocument;
  vNo, vItem: TDOMNode;
begin
  ALista.Clear;
  vDoc := Ler(IncludeTrailingPathDelimiter(APcp) + 'miscellaneousoptions.xml');
  if vDoc = nil then
    Exit;
  try
    vNo := NoInstalados(vDoc);
    if vNo = nil then
      Exit;
    vItem := vNo.FirstChild;
    while vItem <> nil do
    begin
      if (vItem is TDOMElement) and (TDOMElement(vItem).GetAttribute('Value') <> '') then
        ALista.Add(string(TDOMElement(vItem).GetAttribute('Value')));
      vItem := vItem.NextSibling;
    end;
  finally
    vDoc.Free;
  end;
end;

// tira os filhos ItemN e devolve-os por nome (Name dos links, Value dos
// instalados), para reaproveitar os que continuam
procedure TirarItens(ANo: TDOMNode; AAntigos: TStrings; AComValor: boolean);
var
  vItem, vProximo: TDOMNode;
  vChave: string;
begin
  vItem := ANo.FirstChild;
  while vItem <> nil do
  begin
    vProximo := vItem.NextSibling;
    if (vItem.NodeType = ELEMENT_NODE) and
       (Copy(string(vItem.NodeName), 1, 4) = 'Item') then
    begin
      if AComValor then
        vChave := Valor(vItem, 'Name')
      else
        vChave := string(TDOMElement(vItem).GetAttribute('Value'));
      AAntigos.AddObject(vChave, ANo.RemoveChild(vItem));
    end
    else if vItem.NodeType = TEXT_NODE then
      // o espaco entre os itens antigos: o XMLWrite indenta de novo
      ANo.RemoveChild(vItem).Free;
    vItem := vProximo;
  end;
end;

// os ItemN sao numerados: um item novo ou removido renumera todos
function Renomear(ADoc: TXMLDocument; AItem: TDOMNode; const ANome: string): TDOMElement;
var
  vFilho: TDOMNode;
  vInt: integer;
begin
  Result := ADoc.CreateElement(DOMString(ANome));
  if AItem is TDOMElement then
    for vInt := 0 to Pred(TDOMElement(AItem).Attributes.Length) do
      Result.SetAttribute(TDOMElement(AItem).Attributes[vInt].NodeName,
                          TDOMElement(AItem).Attributes[vInt].NodeValue);
  while AItem.FirstChild <> nil do
  begin
    vFilho := AItem.RemoveChild(AItem.FirstChild);
    Result.AppendChild(vFilho);
  end;
  AItem.Free;
end;

procedure DefinirValor(ADoc: TXMLDocument; AItem: TDOMNode;
  const AFilho, AValor: string);
var
  vNo: TDOMNode;
begin
  vNo := Filho(AItem, AFilho);
  if vNo = nil then
  begin
    vNo := ADoc.CreateElement(DOMString(AFilho));
    AItem.AppendChild(vNo);
  end;
  TDOMElement(vNo).SetAttribute('Value', DOMString(AValor));
end;

function GravarLinks(const APcp: string; ALista: TStrings; out AErro: string): boolean;
var
  vArquivo: string;
  vDoc: TXMLDocument;
  vNo, vItem: TDOMNode;
  vAntigos: TStringList;
  vInt, vIdx: integer;
begin
  Result := False;
  AErro := '';
  vArquivo := IncludeTrailingPathDelimiter(APcp) + 'packagefiles.xml';
  vAntigos := NovaLista;
  vDoc := nil;
  try
    try
      vDoc := Ler(vArquivo);
      if vDoc = nil then
      begin
        vDoc := TXMLDocument.Create;
        vDoc.AppendChild(vDoc.CreateElement('CONFIG'));
      end;
      vNo := NoLinks(vDoc);
      if vNo = nil then
      begin
        vNo := vDoc.CreateElement('UserPkgLinks');
        TDOMElement(vNo).SetAttribute('Version', '3');
        vDoc.DocumentElement.AppendChild(vNo);
      end;
      TirarItens(vNo, vAntigos, True);
      for vInt := 0 to Pred(ALista.Count) do
      begin
        vIdx := vAntigos.IndexOf(ALista.Names[vInt]);
        if vIdx >= 0 then
        begin
          vItem := TDOMNode(vAntigos.Objects[vIdx]);
          vAntigos.Delete(vIdx);
        end
        else
        begin
          vItem := vDoc.CreateElement('Item');
          DefinirValor(vDoc, vItem, 'Name', ALista.Names[vInt]);
        end;
        DefinirValor(vDoc, vItem, 'Filename', ALista.ValueFromIndex[vInt]);
        vNo.AppendChild(Renomear(vDoc, vItem, 'Item' + IntToStr(vInt + 1)));
      end;
      TDOMElement(vNo).SetAttribute('Count', DOMString(IntToStr(ALista.Count)));
      WriteXMLFile(vDoc, vArquivo);
      Result := True;
    except
      on E: Exception do
        AErro := vArquivo + ': ' + E.Message;
    end;
  finally
    for vInt := 0 to Pred(vAntigos.Count) do
      vAntigos.Objects[vInt].Free;
    vAntigos.Free;
    vDoc.Free;
  end;
end;

function GravarInstalados(const APcp: string; ALista: TStrings;
  out AErro: string): boolean;
var
  vArquivo: string;
  vDoc: TXMLDocument;
  vNo, vItem: TDOMNode;
  vAntigos: TStringList;
  vInt: integer;
begin
  Result := False;
  AErro := '';
  vArquivo := IncludeTrailingPathDelimiter(APcp) + 'miscellaneousoptions.xml';
  vAntigos := NovaLista;
  vDoc := nil;
  try
    try
      vDoc := Ler(vArquivo);
      if vDoc = nil then
      begin
        // sem o arquivo nao ha lista, e uma lista vazia nao precisa dele
        if ALista.Count = 0 then
          Exit(True);
        AErro := Format(emArquivoNaoExiste, [vArquivo]);
        Exit;
      end;
      vNo := NoInstalados(vDoc);
      if vNo = nil then
      begin
        if ALista.Count = 0 then
          Exit(True);
        AErro := Format(emSemAutoInstall, [vArquivo]);
        Exit;
      end;
      TirarItens(vNo, vAntigos, False);
      for vInt := 0 to Pred(ALista.Count) do
      begin
        vItem := vDoc.CreateElement(DOMString('Item' + IntToStr(vInt + 1)));
        TDOMElement(vItem).SetAttribute('Value', DOMString(ALista[vInt]));
        vNo.AppendChild(vItem);
      end;
      TDOMElement(vNo).SetAttribute('Count', DOMString(IntToStr(ALista.Count)));
      WriteXMLFile(vDoc, vArquivo);
      Result := True;
    except
      on E: Exception do
        AErro := vArquivo + ': ' + E.Message;
    end;
  finally
    for vInt := 0 to Pred(vAntigos.Count) do
      vAntigos.Objects[vInt].Free;
    vAntigos.Free;
    vDoc.Free;
  end;
end;

procedure DesfazerMudanca(AAtual, AAntes, ADepois: TStrings; AComValor: boolean);
var
  vInt, vIdx: integer;
  vNome: string;
begin
  for vInt := 0 to Pred(ADepois.Count) do
  begin
    if AComValor then
      vNome := ADepois.Names[vInt]
    else
      vNome := ADepois[vInt];

    if AComValor then
    begin
      vIdx := AAtual.IndexOfName(vNome);
      // o usuario ja trocou ou tirou: fica como ele deixou
      if (vIdx < 0) or
         not SameFileName(AAtual.ValueFromIndex[vIdx], ADepois.ValueFromIndex[vInt]) then
        Continue;
      if AAntes.IndexOfName(vNome) < 0 then
        AAtual.Delete(vIdx)
      else if not SameFileName(AAntes.Values[vNome], ADepois.ValueFromIndex[vInt]) then
        AAtual.ValueFromIndex[vIdx] := AAntes.Values[vNome];
    end
    else if AAntes.IndexOf(vNome) < 0 then
    begin
      vIdx := AAtual.IndexOf(vNome);
      if vIdx >= 0 then
        AAtual.Delete(vIdx);
    end;
  end;

  // o que a mudanca tirou volta (desfazer uma desinstalacao), se ainda falta
  for vInt := 0 to Pred(AAntes.Count) do
    if AComValor then
    begin
      vNome := AAntes.Names[vInt];
      if (ADepois.IndexOfName(vNome) < 0) and (AAtual.IndexOfName(vNome) < 0) then
        AAtual.Add(AAntes[vInt]);
    end
    else if (ADepois.IndexOf(AAntes[vInt]) < 0) and
            (AAtual.IndexOf(AAntes[vInt]) < 0) then
      AAtual.Add(AAntes[vInt]);
end;

end.
