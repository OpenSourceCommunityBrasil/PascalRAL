/// What the installer downloaded and no installation uses any more (F19):
/// after an uninstall or an update, the folders of <folder>/PascalRAL/<version>
/// and <folder>/dependencias/<name>/<version> that no living receipt of any IDE
/// cites, the DLLs in <folder>/redist nobody cites, the zip cache and what a run
/// that fell in the middle left in the staging folder. Each line with its size.
/// Two rules keep this from deleting what is not the installer's:
/// - a folder without the installer's mark (.ralinstaller.json) is never
///   listed, whatever its name;
/// - a dependency the receipts call "encontrada" (it was the user's) is cited,
///   and so it is never listed.
/// Deleting has no undo: the record of what was deleted goes to
/// <receipts>\desinstalacoes\limpeza-*.json.
unit RALInst.Limpeza;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  RALInst.Processo;

type
  /// Kind of leftover.
  TTipoSobra = (tsRAL, tsDependencia, tsRedist, tsCache, tsPreparo);

  /// One leftover the cleanup offers to delete.
  TSobra = class
  public
    Caminho: string;
    /// Folder (True) or a single file
    Pasta: boolean;
    /// Kept by the user (unchecked): not deleted
    Manter: boolean;
    Tamanho: int64;
    Tipo: TTipoSobra;
    /// 'PascalRAL 1.1', 'Zeos 8.0-patches', 'libsagui-3.dll (win64)'
    function Descricao: string;
  end;

  /// The leftovers of the installation folders, owned by the list.
  TSobras = class(TObjectList)
  private
    function GetItem(AIndex: integer): TSobra;
  public
    /// Deletes what is not kept; the record of what was deleted goes to
    /// <APastaRecibos>\desinstalacoes. False when something could not be
    /// deleted (the log says what)
    function Apagar(const APastaRecibos: string; ALog: TLogLinha): boolean;
    /// Every leftover of the installation folders ABases (and the ones the
    /// receipts, undone or not, say were used), given the living receipts in
    /// APastaRecibos
    procedure Listar(ABases: TStrings; const APastaRecibos: string);
    /// Bytes of what is not kept
    function TotalBytes: int64;

    property Items[AIndex: integer]: TSobra read GetItem; default;
  end;

/// '12,3 MB'
function DescreverTamanho(ABytes: int64): string;
/// Size of a folder, recursively (long paths accepted)
function TamanhoPasta(const APasta: string): int64;

implementation

uses
  fpjson, jsonparser,
  RALInst.Fontes, RALInst.GitHub, RALInst.Mensagens, RALInst.Recibos;

function DescreverTamanho(ABytes: int64): string;
begin
  if ABytes >= 1024 * 1024 then
    Result := FormatFloat('0.0', ABytes / (1024 * 1024)) + ' MB'
  else if ABytes >= 1024 then
    Result := FormatFloat('0', ABytes / 1024) + ' KB'
  else
    Result := IntToStr(ABytes) + ' B';
end;

function TamanhoPasta(const APasta: string): int64;
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  Result := 0;
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(CaminhoLongo(vPasta + AllFilesMask), faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        Inc(Result, TamanhoPasta(vPasta + vBusca.Name))
      else
        Inc(Result, vBusca.Size);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

function Normalizar(const ACaminho: string): string;
begin
  Result := LowerCase(IncludeTrailingPathDelimiter(ExpandFileName(
              StringReplace(StringReplace(Trim(ACaminho), '/', PathDelim,
                                          [rfReplaceAll]),
                            '\', PathDelim, [rfReplaceAll]))));
end;

// todo texto de um JSON que parece um caminho (recibo: fontes, onde das
// dependencias, arquivos, links, valores do registro)
procedure TextosDoJSON(ADados: TJSONData; ALista: TStrings);
var
  vInt: integer;
  vTexto: string;
begin
  if ADados = nil then
    Exit;
  case ADados.JSONType of
    jtString:
      begin
        vTexto := ADados.AsString;
        if (Pos(':', vTexto) > 0) or (Copy(vTexto, 1, 1) = '/') then
          ALista.Add(LowerCase(StringReplace(vTexto, '/', PathDelim, [rfReplaceAll])));
      end;
    jtArray, jtObject:
      for vInt := 0 to Pred(ADados.Count) do
        TextosDoJSON(ADados.Items[vInt], ALista);
  end;
end;

{ TSobra }

function TSobra.Descricao: string;
var
  vPai: string;
begin
  Result := '';
  vPai := ExtractFileName(ExtractFileDir(ExcludeTrailingPathDelimiter(Caminho)));
  case Tipo of
    tsRAL:
      Result := 'PascalRAL ' + ExtractFileName(ExcludeTrailingPathDelimiter(Caminho));
    tsDependencia:
      Result := vPai + ' ' + ExtractFileName(ExcludeTrailingPathDelimiter(Caminho));
    tsRedist:
      Result := ExtractFileName(Caminho) + ' (' + vPai + ')';
    tsCache:
      Result := cmSobraCache;
    tsPreparo:
      Result := cmSobraPreparo;
  end;
end;

{ TSobras }

function TSobras.GetItem(AIndex: integer): TSobra;
begin
  Result := TSobra(inherited Items[AIndex]);
end;

function TSobras.TotalBytes: int64;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := 0 to Pred(Count) do
    if not Items[vInt].Manter then
      Inc(Result, Items[vInt].Tamanho);
end;

procedure TSobras.Listar(ABases: TStrings; const APastaRecibos: string);
var
  vRecibos: TRecibos;
  vCitados, vBases: TStringList;
  vInt: integer;
  vBusca, vBuscaVer: TSearchRec;
  vBase, vPasta, vRaiz, vFontes: string;

  function Citado(const APasta: string): boolean;
  var
    vChave, vCitado: string;
    vPos, vFim: integer;
  begin
    // algum recibo vivo cita a pasta, ou algo dentro dela, em qualquer lugar
    // do texto ('$(zeos) = D:\RAL\dependencias\Zeos\estavel', 'instalada|...')
    vChave := ExcludeTrailingPathDelimiter(Normalizar(APasta));
    for vCitado in vCitados do
    begin
      vPos := Pos(vChave, vCitado);
      if vPos = 0 then
        Continue;
      vFim := vPos + Length(vChave);
      if (vFim > Length(vCitado)) or
         (vCitado[vFim] in [PathDelim, ' ', ';', ')', '"', '|']) then
        Exit(True);
    end;
    Result := False;
  end;

  procedure Acrescentar(const ACaminho: string; ATipo: TTipoSobra; APasta: boolean);
  var
    vSobra: TSobra;
    vArquivo: TSearchRec;
  begin
    vSobra := TSobra.Create;
    vSobra.Caminho := ACaminho;
    vSobra.Tipo := ATipo;
    vSobra.Pasta := APasta;
    if APasta then
      vSobra.Tamanho := TamanhoPasta(ACaminho)
    else if FindFirst(ACaminho, faAnyFile, vArquivo) = 0 then
    begin
      vSobra.Tamanho := vArquivo.Size;
      SysUtils.FindClose(vArquivo);
    end;
    Add(vSobra);
  end;

  procedure AdicionarBase(const ABase: string);
  begin
    if (ABase <> '') and DirectoryExists(ABase) and
       (vBases.IndexOf(Normalizar(ABase)) < 0) then
      vBases.Add(Normalizar(ABase));
  end;

begin
  Clear;
  vRecibos := TRecibos.Create(True);
  vCitados := TStringList.Create;
  vCitados.Sorted := True;
  vCitados.Duplicates := dupIgnore;
  vBases := TStringList.Create;
  try
    if ABases <> nil then
      for vBase in ABases do
        AdicionarBase(vBase);
    // as pastas de instalacao que os recibos, desfeitos ou nao, usaram:
    // <base>/PascalRAL/<versao>
    vRecibos.Carregar(APastaRecibos, True);
    for vInt := 0 to Pred(vRecibos.Count) do
    begin
      vFontes := ExcludeTrailingPathDelimiter(vRecibos[vInt].Fontes);
      if SameText(ExtractFileName(ExtractFileDir(vFontes)), 'PascalRAL') then
        AdicionarBase(ExtractFileDir(ExtractFileDir(vFontes)));
    end;
    // o que os recibos vivos citam
    vRecibos.Carregar(APastaRecibos, False);
    for vInt := 0 to Pred(vRecibos.Count) do
    begin
      vCitados.Add(Normalizar(vRecibos[vInt].Fontes));
      TextosDoJSON(vRecibos[vInt].Dados, vCitados);
    end;

    for vBase in vBases do
    begin
      // <base>/PascalRAL/<versao>, com a marca e sem recibo vivo
      vPasta := vBase + 'PascalRAL' + PathDelim;
      if FindFirst(vPasta + AllFilesMask, faDirectory, vBusca) = 0 then
      try
        repeat
          if (vBusca.Name = '.') or (vBusca.Name = '..') or
             ((vBusca.Attr and faDirectory) = 0) then
            Continue;
          vRaiz := vPasta + vBusca.Name;
          if FileExists(IncludeTrailingPathDelimiter(vRaiz) + ArquivoMarca) and
             not Citado(vRaiz) then
            Acrescentar(vRaiz, tsRAL, True);
        until FindNext(vBusca) <> 0;
      finally
        SysUtils.FindClose(vBusca);
      end;

      // <base>/dependencias/<nome>/<versao>
      vPasta := vBase + 'dependencias' + PathDelim;
      if FindFirst(vPasta + AllFilesMask, faDirectory, vBusca) = 0 then
      try
        repeat
          if (vBusca.Name = '.') or (vBusca.Name = '..') or
             ((vBusca.Attr and faDirectory) = 0) then
            Continue;
          if FindFirst(vPasta + vBusca.Name + PathDelim + AllFilesMask, faDirectory,
                       vBuscaVer) = 0 then
          try
            repeat
              if (vBuscaVer.Name = '.') or (vBuscaVer.Name = '..') or
                 ((vBuscaVer.Attr and faDirectory) = 0) then
                Continue;
              vRaiz := vPasta + vBusca.Name + PathDelim + vBuscaVer.Name;
              if FileExists(IncludeTrailingPathDelimiter(vRaiz) + ArquivoMarca) and
                 not Citado(vRaiz) then
                Acrescentar(vRaiz, tsDependencia, True);
            until FindNext(vBuscaVer) <> 0;
          finally
            SysUtils.FindClose(vBuscaVer);
          end;
        until FindNext(vBusca) <> 0;
      finally
        SysUtils.FindClose(vBusca);
      end;

      // <base>/redist/<plataforma>/<arquivo> que nenhum recibo cita
      vPasta := vBase + 'redist' + PathDelim;
      if FindFirst(vPasta + AllFilesMask, faDirectory, vBusca) = 0 then
      try
        repeat
          if (vBusca.Name = '.') or (vBusca.Name = '..') or
             ((vBusca.Attr and faDirectory) = 0) then
            Continue;
          if FindFirst(vPasta + vBusca.Name + PathDelim + AllFilesMask, faAnyFile,
                       vBuscaVer) = 0 then
          try
            repeat
              if (vBuscaVer.Attr and faDirectory) <> 0 then
                Continue;
              vRaiz := vPasta + vBusca.Name + PathDelim + vBuscaVer.Name;
              if not Citado(vRaiz) then
                Acrescentar(vRaiz, tsRedist, False);
            until FindNext(vBuscaVer) <> 0;
          finally
            SysUtils.FindClose(vBuscaVer);
          end;
        until FindNext(vBusca) <> 0;
      finally
        SysUtils.FindClose(vBusca);
      end;
    end;

    // o cache de downloads (zips das versoes e das dependencias): baixa de novo
    // quando precisar
    vPasta := PastaCacheInstalador + 'zip';
    if DirectoryExists(vPasta) and (TamanhoPasta(vPasta) > 0) then
      Acrescentar(vPasta, tsCache, True);
    // o preparo de uma rodada que caiu no meio
    vPasta := PastaDadosInstalador + 'preparo';
    if DirectoryExists(vPasta) then
      Acrescentar(vPasta, tsPreparo, True);
  finally
    vBases.Free;
    vCitados.Free;
    vRecibos.Free;
  end;
end;

function TSobras.Apagar(const APastaRecibos: string; ALog: TLogLinha): boolean;
var
  vInt: integer;
  vSobra: TSobra;
  vRaiz, vItem: TJSONObject;
  vLista: TJSONArray;
  vPasta: string;
  vOk: boolean;

  procedure Logar(const ALinha: string);
  begin
    if Assigned(ALog) then
      ALog(ALinha);
  end;

begin
  Result := True;
  vRaiz := TJSONObject.Create;
  try
    vRaiz.Add('instalador', 'RALInstaller');
    vRaiz.Add('acao', 'limpeza');
    vRaiz.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
    vLista := TJSONArray.Create;
    vRaiz.Add('apagados', vLista);
    for vInt := 0 to Pred(Count) do
    begin
      vSobra := Items[vInt];
      if vSobra.Manter then
        Continue;
      // pasta sem a marca nunca e apagada (as de cache e preparo sao do
      // proprio instalador)
      if vSobra.Pasta and (vSobra.Tipo in [tsRAL, tsDependencia]) and
         not FileExists(IncludeTrailingPathDelimiter(vSobra.Caminho) + ArquivoMarca) then
        Continue;
      if vSobra.Pasta then
        vOk := ApagarPasta(vSobra.Caminho)
      else
        vOk := SysUtils.DeleteFile(vSobra.Caminho);
      if vOk then
      begin
        Logar(Format(cmSobraApagada, [vSobra.Caminho, DescreverTamanho(vSobra.Tamanho)]));
        vItem := TJSONObject.Create;
        vItem.Add('caminho', vSobra.Caminho);
        vItem.Add('tamanho', vSobra.Tamanho);
        vLista.Add(vItem);
      end
      else
      begin
        Logar(Format(emApagarArquivo, [vSobra.Caminho]));
        Result := False;
      end;
    end;
    if vLista.Count > 0 then
    begin
      vPasta := IncludeTrailingPathDelimiter(APastaRecibos) + 'desinstalacoes';
      ForceDirectories(vPasta);
      with TStringList.Create do
      try
        Text := vRaiz.FormatJSON;
        SaveToFile(ArquivoReciboLivre(vPasta, 'limpeza-' +
                                      FormatDateTime('yyyymmdd-hhnnss', Now)));
      finally
        Free;
      end;
    end;
  finally
    vRaiz.Free;
  end;
end;

end.
