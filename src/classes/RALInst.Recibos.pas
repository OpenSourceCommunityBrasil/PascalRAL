/// Installation receipts: listing and undoing (uninstalling). Every run in an
/// IDE writes a receipt: what changed, with the value before. Uninstalling
/// undoes the receipts of that IDE from newest to oldest: undoing an older one
/// first would put back values the newer one had already changed. An undone
/// receipt becomes <name>.desfeito.json and no longer counts.
/// - Delphi: the registry writes go back (RALInst.Registro.Delphi) and the
///   written .bpl/.dcp are deleted, only those still with the receipt's size
///   and date: one recompiled later by the user stays.
/// - Lazarus: the two configuration lists (links and installed) lose what the
///   run added and get back what it changed; the rest the user changed later
///   stays (RALInst.Config.Lazarus). With packages installed in the IDE, it is
///   rebuilt without them.
/// - A dependency the receipt calls "encontrada" is never touched: it was the
///   user's.
unit RALInst.Recibos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson,
  RALInst.Processo;

type
  /// One installation receipt.
  TRecibo = class
  private
    FDados: TJSONObject;
    FPacotes: TStringList;
  public
    Arquivo: string;
    Data: string;
    Desfeito: boolean;
    Fontes: string;
    IDENome: string;
    IDERaiz: string;
    /// 'delphi' or 'lazarus'
    Tipo: string;
    VersaoRAL: string;
    constructor Create;
    destructor Destroy; override;
    /// False when the file is not an installer receipt
    function Carregar(const AArquivo: string): boolean;
    /// One line for lists: 'Delphi 12 Athens — 2026-09-23 16:00 — RAL 1.1 (3
    /// packages)'
    function Descricao: string;

    property Dados: TJSONObject read FDados;
    property Pacotes: TStringList read FPacotes;
  end;

  /// Receipts of a folder, newest first.
  TRecibos = class(TObjectList)
  private
    function GetItem(AIndex: integer): TRecibo;
  public
    /// The folder's receipts, newest first
    procedure Carregar(const APasta: string; AIncluirDesfeitos: boolean = False);
    /// Those valid for the IDE (by root), newest first
    procedure DaIDE(const ARaiz: string; ALista: TList);

    property Items[AIndex: integer]: TRecibo read GetItem; default;
  end;

/// Undoes a receipt. Refuses when a newer receipt of the same IDE still holds
/// (ARecibos: those of the same folder), and when the IDE is open.
/// AReconstruirIDE: in Lazarus, rebuild the IDE without the packages (which is
/// what really removes them); False only updates the configuration, and the IDE
/// asks to rebuild when it opens
function DesfazerInstalacao(ARecibo: TRecibo; ARecibos: TRecibos; ALog: TLogLinha;
  AReconstruirIDE: boolean; AExigirIDEFechada: boolean = True): boolean;
/// Puts back what an uninstall took out, from its record in
/// <receipts>\desinstalacoes (the registry values, or the Lazarus links and
/// installed list). A Lazarus whose installed list changes asks to rebuild when
/// it opens
function DesfazerDesinstalacao(const AArquivo: string; ALog: TLogLinha): boolean;
/// Undoes every receipt of the IDE, newest to oldest; stops at the first one
/// that fails
function DesinstalarIDE(const APastaRecibos, ARaiz: string; ALog: TLogLinha;
  AReconstruirIDE: boolean; AExigirIDEFechada: boolean = True): boolean;

implementation

uses
  jsonparser,
  {$IFDEF MSWINDOWS} RALInst.Registro.Delphi, {$ENDIF}
  RALInst.Config.Lazarus, RALInst.Mensagens;

function MesmaRaiz(const A, B: string): boolean;
begin
  Result := SameFileName(IncludeTrailingPathDelimiter(ExpandFileName(A)),
                         IncludeTrailingPathDelimiter(ExpandFileName(B)));
end;

{ TRecibo }

constructor TRecibo.Create;
begin
  inherited Create;
  FPacotes := TStringList.Create;
end;

destructor TRecibo.Destroy;
begin
  FDados.Free;
  FPacotes.Free;
  inherited Destroy;
end;

function TRecibo.Carregar(const AArquivo: string): boolean;
var
  vTexto: TStringList;
  vJSON: TJSONData;
  vIDE, vRAL: TJSONObject;
  vLista: TJSONArray;
  vInt: integer;
  vPacote: string;
begin
  Result := False;
  Arquivo := AArquivo;
  Desfeito := Pos('.desfeito.', LowerCase(ExtractFileName(AArquivo))) > 0;
  vTexto := TStringList.Create;
  try
    try
      vTexto.LoadFromFile(AArquivo);
      vJSON := GetJSON(vTexto.Text);
    except
      Exit;
    end;
  finally
    vTexto.Free;
  end;
  if not (vJSON is TJSONObject) or
     (TJSONObject(vJSON).Get('instalador', '') <> 'RALInstaller') then
  begin
    vJSON.Free;
    Exit;
  end;
  FreeAndNil(FDados);
  FDados := TJSONObject(vJSON);
  Data := FDados.Get('data', '');
  Fontes := FDados.Get('fontes', '');
  vIDE := FDados.Get('ide', TJSONObject(nil));
  if vIDE <> nil then
  begin
    Tipo := vIDE.Get('tipo', '');
    IDENome := vIDE.Get('nome', '');
    IDERaiz := vIDE.Get('raiz', '');
  end;
  vRAL := FDados.Get('ral', TJSONObject(nil));
  if vRAL <> nil then
    VersaoRAL := vRAL.Get('versao', '');
  FPacotes.Clear;
  vLista := FDados.Get('pacotes', TJSONArray(nil));
  if vLista <> nil then
    for vInt := 0 to Pred(vLista.Count) do
    begin
      // Delphi: 'win32 IndyRAL: ok'; Lazarus: 'indyral'
      vPacote := vLista.Items[vInt].AsString;
      if Pos(':', vPacote) > 0 then
      begin
        if Pos(': ok', vPacote) = 0 then
          Continue;
        vPacote := Copy(vPacote, 1, Pos(':', vPacote) - 1);
        vPacote := Copy(vPacote, Pos(' ', vPacote) + 1, MaxInt);
      end;
      if FPacotes.IndexOf(vPacote) < 0 then
        FPacotes.Add(vPacote);
    end;
  Result := (Tipo <> '') and (IDERaiz <> '');
end;

function TRecibo.Descricao: string;
begin
  Result := IDENome + ' — ' + StringReplace(Copy(Data, 1, 16), 'T', ' ', []);
  if VersaoRAL <> '' then
    Result := Result + ' — RAL ' + VersaoRAL
  else if Fontes <> '' then
    Result := Result + ' — ' + ExcludeTrailingPathDelimiter(Fontes);
  Result := Result + Format(cmReciboPacotes, [FPacotes.Count]);
  if Desfeito then
    Result := Result + cmReciboDesfeito;
end;

{ TRecibos }

function TRecibos.GetItem(AIndex: integer): TRecibo;
begin
  Result := TRecibo(inherited Items[AIndex]);
end;

function CompararRecibos(AItem1, AItem2: Pointer): integer;
begin
  // data ISO: a ordem de texto e a ordem do tempo; mais novo primeiro
  Result := -CompareStr(TRecibo(AItem1).Data, TRecibo(AItem2).Data);
  if Result = 0 then
    Result := -CompareStr(TRecibo(AItem1).Arquivo, TRecibo(AItem2).Arquivo);
end;

procedure TRecibos.Carregar(const APasta: string; AIncluirDesfeitos: boolean);
var
  vBusca: TSearchRec;
  vRecibo: TRecibo;
begin
  Clear;
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + '*.json', faAnyFile,
               vBusca) = 0 then
  try
    repeat
      vRecibo := TRecibo.Create;
      if vRecibo.Carregar(IncludeTrailingPathDelimiter(APasta) + vBusca.Name) and
         (AIncluirDesfeitos or not vRecibo.Desfeito) then
        Add(vRecibo)
      else
        vRecibo.Free;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
  Sort(@CompararRecibos);
end;

procedure TRecibos.DaIDE(const ARaiz: string; ALista: TList);
var
  vInt: integer;
begin
  ALista.Clear;
  for vInt := 0 to Pred(Count) do
    if not Items[vInt].Desfeito and MesmaRaiz(Items[vInt].IDERaiz, ARaiz) then
      ALista.Add(Items[vInt]);
end;

procedure Logar(ALog: TLogLinha; const ALinha: string);
begin
  if Assigned(ALog) then
    ALog(ALinha);
end;

// os .bpl/.dcp que ainda sao os que a instalacao gravou
function ApagarArquivos(ADados: TJSONObject; ALog: TLogLinha): boolean;
var
  vLista: TJSONArray;
  vItem: TJSONObject;
  vInt: integer;
  vArquivo, vData: string;
  vBusca: TSearchRec;
begin
  Result := True;
  vLista := ADados.Get('arquivos', TJSONArray(nil));
  if vLista = nil then
    Exit;
  for vInt := 0 to Pred(vLista.Count) do
  begin
    vItem := vLista.Objects[vInt];
    vArquivo := vItem.Get('arquivo', '');
    if (vArquivo = '') or (FindFirst(vArquivo, faAnyFile, vBusca) <> 0) then
      Continue;
    vData := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss',
                            vBusca.TimeStamp);
    SysUtils.FindClose(vBusca);
    if (vBusca.Size <> vItem.Get('tamanho', int64(-1))) or
       (vData <> vItem.Get('data', '')) then
    begin
      Logar(ALog, Format(cmMantidoMudou, [vArquivo]));
      Continue;
    end;
    if SysUtils.DeleteFile(vArquivo) then
      Logar(ALog, Format(cmRegistroApagado, [vArquivo]))
    else
    begin
      Logar(ALog, Format(emApagarArquivo, [vArquivo]));
      Result := False;
    end;
  end;
end;

// lazbuild --build-ide= com a configuracao do recibo
function ReconstruirLazarus(AIDE: TJSONObject; ALog: TLogLinha): boolean;
var
  vExec: TExecucao;
begin
  vExec := TExecucao.Create;
  try
    vExec.Executavel := AIDE.Get('lazbuild', '');
    vExec.Parametros.Add('--primary-config-path=' +
                         ExcludeTrailingPathDelimiter(AIDE.Get('config', '')));
    vExec.Parametros.Add('--lazarusdir=' +
                         ExcludeTrailingPathDelimiter(AIDE.Get('raiz', '')));
    vExec.Parametros.Add('--build-ide=');
    vExec.Log := ALog;
    Result := vExec.Executar;
    if not Result then
      Logar(ALog, emReconstrucaoFalhou);
  finally
    vExec.Free;
  end;
end;

// APrecisaReconstruir: a lista de instalados mudou numa IDE que a instalacao
// tinha reconstruido — os pacotes so saem dela reconstruindo de novo
function DesfazerLazarus(ARecibo: TRecibo; ALog: TLogLinha;
  out APrecisaReconstruir: boolean): boolean;
var
  vIDE, vLaz, vObj: TJSONObject;
  vLista: TJSONArray;
  vConfig, vErro: string;
  vLinks, vInstalados, vAntes, vDepois, vInstAntes, vInstDepois: TStringList;
  vInt: integer;
begin
  Result := False;
  APrecisaReconstruir := False;
  vIDE := ARecibo.Dados.Get('ide', TJSONObject(nil));
  vLaz := ARecibo.Dados.Get('lazarus', TJSONObject(nil));
  if (vIDE = nil) or (vLaz = nil) then
  begin
    Logar(ALog, emReciboSemListas);
    Exit;
  end;
  vConfig := vIDE.Get('config', '');
  vLinks := TStringList.Create;
  vInstalados := TStringList.Create;
  vAntes := TStringList.Create;
  vDepois := TStringList.Create;
  vInstAntes := TStringList.Create;
  vInstDepois := TStringList.Create;
  try
    vLinks.CaseSensitive := False;
    vInstalados.CaseSensitive := False;
    vAntes.CaseSensitive := False;
    vDepois.CaseSensitive := False;
    vInstAntes.CaseSensitive := False;
    vInstDepois.CaseSensitive := False;

    vObj := vLaz.Get('links-antes', TJSONObject(nil));
    if vObj <> nil then
      for vInt := 0 to Pred(vObj.Count) do
        vAntes.Add(vObj.Names[vInt] + '=' + vObj.Items[vInt].AsString);
    vObj := vLaz.Get('links-depois', TJSONObject(nil));
    if vObj <> nil then
      for vInt := 0 to Pred(vObj.Count) do
        vDepois.Add(vObj.Names[vInt] + '=' + vObj.Items[vInt].AsString);
    vLista := vLaz.Get('instalados-antes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
        vInstAntes.Add(vLista.Items[vInt].AsString);
    vLista := vLaz.Get('instalados-depois', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
        vInstDepois.Add(vLista.Items[vInt].AsString);

    LerLinks(vConfig, vLinks);
    LerInstalados(vConfig, vInstalados);
    vInt := vInstalados.Count;
    DesfazerMudanca(vLinks, vAntes, vDepois, True);
    DesfazerMudanca(vInstalados, vInstAntes, vInstDepois, False);
    APrecisaReconstruir := (vInstalados.Count <> vInt) and
                           ARecibo.Dados.Get('reconstruiu-ide', False);
    if not GravarLinks(vConfig, vLinks, vErro) or
       not GravarInstalados(vConfig, vInstalados, vErro) then
    begin
      Logar(ALog, cmPrefixoErro + vErro);
      Exit;
    end;
    Logar(ALog, Format(cmConfiguracaoLimpa, [vConfig]));
    Result := True;
  finally
    vInstDepois.Free;
    vInstAntes.Free;
    vDepois.Free;
    vAntes.Free;
    vInstalados.Free;
    vLinks.Free;
  end;
end;

// o trabalho de DesfazerInstalacao; APrecisaReconstruir diz se ficou uma
// reconstrucao do Lazarus por fazer (DesinstalarIDE junta todas numa so)
function Desfazer(ARecibo: TRecibo; ARecibos: TRecibos; ALog: TLogLinha;
  AExigirIDEFechada: boolean; out APrecisaReconstruir: boolean): boolean;
var
  vLista: TList;
  vNovo: string;
begin
  Result := False;
  APrecisaReconstruir := False;
  if ARecibo.Desfeito then
  begin
    Logar(ALog, Format(cmReciboJaDesfeito, [ARecibo.Arquivo]));
    Exit;
  end;

  // do mais novo ao mais velho
  if ARecibos <> nil then
  begin
    vLista := TList.Create;
    try
      ARecibos.DaIDE(ARecibo.IDERaiz, vLista);
      if (vLista.Count > 0) and (TRecibo(vLista[0]) <> ARecibo) then
      begin
        vNovo := TRecibo(vLista[0]).Descricao;
        Logar(ALog, Format(emReciboMaisNovo, [vNovo]));
        Exit;
      end;
    finally
      vLista.Free;
    end;
  end;

  Logar(ALog, Format(cmDesfazendo, [ARecibo.Descricao]));
  if ARecibo.Tipo = 'lazarus' then
  begin
    if AExigirIDEFechada and
       ProgramaEmExecucao(['lazarus.exe', 'startlazarus.exe', 'lazarus',
                           'startlazarus'], ARecibo.IDERaiz) then
    begin
      Logar(ALog, Format(emIDEAbertaDesinstalar, [ARecibo.IDENome]));
      Exit;
    end;
    Result := DesfazerLazarus(ARecibo, ALog, APrecisaReconstruir);
  end
  else if ARecibo.Tipo = 'delphi' then
  begin
    {$IFDEF MSWINDOWS}
    if AExigirIDEFechada and
       ProgramaEmExecucao(['bds.exe', 'delphi32.exe'],
                          IncludeTrailingPathDelimiter(ARecibo.IDERaiz) + 'bin') then
    begin
      Logar(ALog, Format(emIDEAbertaDesinstalar, [ARecibo.IDENome]));
      Exit;
    end;
    Result := RALInst.Registro.Delphi.DesfazerRecibo(ARecibo.Arquivo, ALog);
    if not ApagarArquivos(ARecibo.Dados, ALog) then
      Result := False;
    {$ELSE}
    Logar(ALog, emReciboDelphiWindows);
    {$ENDIF}
  end
  else
    Logar(ALog, Format(emReciboTipo, [ARecibo.Tipo]));

  if Result then
  begin
    // desfeito nao vale mais; fica guardado para quem quiser ver
    vNovo := ChangeFileExt(ARecibo.Arquivo, '') + '.desfeito.json';
    if RenameFile(ARecibo.Arquivo, vNovo) then
    begin
      ARecibo.Arquivo := vNovo;
      ARecibo.Desfeito := True;
    end;
    Logar(ALog, cmDesfeito);
  end;
end;

function DesfazerInstalacao(ARecibo: TRecibo; ARecibos: TRecibos; ALog: TLogLinha;
  AReconstruirIDE: boolean; AExigirIDEFechada: boolean): boolean;
var
  vReconstruir: boolean;
begin
  Result := Desfazer(ARecibo, ARecibos, ALog, AExigirIDEFechada, vReconstruir);
  if Result and vReconstruir then
    if AReconstruirIDE then
      Result := ReconstruirLazarus(ARecibo.Dados.Get('ide', TJSONObject(nil)), ALog)
    else
      Logar(ALog, cmIDENaoReconstruidaDesinstalar);
end;

function DesfazerDesinstalacao(const AArquivo: string; ALog: TLogLinha): boolean;
var
  vRecibo: TRecibo;
  vReconstruir: boolean;
begin
  Result := False;
  vRecibo := TRecibo.Create;
  try
    if not vRecibo.Carregar(AArquivo) then
    begin
      Logar(ALog, Format(emReciboTipo, [AArquivo]));
      Exit;
    end;
    if vRecibo.Tipo = 'lazarus' then
    begin
      Result := DesfazerLazarus(vRecibo, ALog, vReconstruir);
      if Result and vReconstruir then
        Logar(ALog, cmIDENaoReconstruidaDesinstalar);
    end
    else if vRecibo.Tipo = 'delphi' then
    begin
      {$IFDEF MSWINDOWS}
      Result := RALInst.Registro.Delphi.DesfazerRecibo(AArquivo, ALog);
      {$ELSE}
      Logar(ALog, emReciboDelphiWindows);
      {$ENDIF}
    end
    else
      Logar(ALog, Format(emReciboTipo, [vRecibo.Tipo]));
  finally
    vRecibo.Free;
  end;
end;

function DesinstalarIDE(const APastaRecibos, ARaiz: string; ALog: TLogLinha;
  AReconstruirIDE: boolean; AExigirIDEFechada: boolean): boolean;
var
  vRecibos: TRecibos;
  vLista: TList;
  vInt: integer;
  vReconstruir, vPrecisa: boolean;
begin
  Result := True;
  vRecibos := TRecibos.Create(True);
  vLista := TList.Create;
  try
    vRecibos.Carregar(APastaRecibos);
    vRecibos.DaIDE(ARaiz, vLista);
    if vLista.Count = 0 then
    begin
      Logar(ALog, Format(cmNenhumaInstalacao, [ARaiz]));
      Exit;
    end;
    // reconstruir a cada recibo e perda de tempo: uma vez no fim, se algum
    // deles tirou pacote da IDE
    vReconstruir := False;
    for vInt := 0 to Pred(vLista.Count) do
    begin
      if not Desfazer(TRecibo(vLista[vInt]), vRecibos, ALog, AExigirIDEFechada,
                      vPrecisa) then
        Exit(False);
      vReconstruir := vReconstruir or vPrecisa;
    end;
    if vReconstruir then
      if AReconstruirIDE then
        Result := ReconstruirLazarus(TRecibo(vLista[0]).Dados.Get('ide',
                                       TJSONObject(nil)), ALog)
      else
        Logar(ALog, cmIDENaoReconstruidaDesinstalar);
  finally
    vLista.Free;
    vRecibos.Free;
  end;
end;

end.
