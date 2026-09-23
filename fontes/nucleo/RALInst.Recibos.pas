unit RALInst.Recibos;

{$mode ObjFPC}{$H+}

// F10: os recibos das instalacoes — listar e desfazer (desinstalar).
//
// Cada rodada numa IDE grava um recibo: o que mudou, com o valor de antes.
// Desinstalar e desfazer os recibos daquela IDE do mais novo ao mais velho:
// desfazer um mais velho antes poria de volta valores que o mais novo ja
// tinha trocado. Recibo desfeito vira <nome>.desfeito.json e nao volta a
// valer.
//
// - Delphi: as escritas no registro voltam (RALInst.Registro.Delphi) e os
//   .bpl/.dcp gravados sao apagados — so os que ainda tem o tamanho e a data
//   do recibo: um recompilado depois pelo usuario fica.
// - Lazarus: as duas listas da configuracao (links e instalados) perdem o que
//   a rodada acrescentou e recuperam o que ela trocou; o resto que o usuario
//   mudou depois fica (RALInst.Config.Lazarus). Havendo pacote instalado na
//   IDE, ela e reconstruida sem eles.
// - Dependencia que o recibo diz "encontrada" nunca e tocada: era do usuario.

interface

uses
  Classes, SysUtils, Contnrs, fpjson, RALInst.Processo;

type
  { TRecibo }

  TRecibo = class
  private
    FDados: TJSONObject;
    FPacotes: TStringList;
  public
    Arquivo: string;
    Tipo: string;          // 'delphi' ou 'lazarus'
    IDENome: string;
    IDERaiz: string;
    Data: string;
    Fontes: string;
    VersaoRAL: string;
    Desfeito: boolean;
    constructor Create;
    destructor Destroy; override;
    // False se o arquivo nao e um recibo do instalador
    function Carregar(const AArquivo: string): boolean;
    // uma linha para listas: 'Delphi 12 Athens — 2026-09-23 16:00 — RAL 1.1 (3 pacotes)'
    function Descricao: string;
    property Pacotes: TStringList read FPacotes;
    property Dados: TJSONObject read FDados;
  end;

  { TRecibos }

  TRecibos = class(TObjectList)
  private
    function GetItem(AIndex: integer): TRecibo;
  public
    // os recibos da pasta, do mais novo ao mais velho
    procedure Carregar(const APasta: string; AIncluirDesfeitos: boolean = False);
    // os que valem para a IDE (pela raiz), do mais novo ao mais velho
    procedure DaIDE(const ARaiz: string; ALista: TList);
    property Items[AIndex: integer]: TRecibo read GetItem; default;
  end;

// desfaz um recibo. Recusa se ha recibo mais novo da mesma IDE ainda valendo
// (ARecibos: os da mesma pasta), e se a IDE esta aberta. AReconstruirIDE:
// no Lazarus, reconstruir a IDE sem os pacotes (e o que tira eles dela de
// fato); False so atualiza a configuracao, e a IDE pede para reconstruir ao
// abrir
function DesfazerInstalacao(ARecibo: TRecibo; ARecibos: TRecibos; ALog: TLogLinha;
  AReconstruirIDE: boolean; AExigirIDEFechada: boolean = True): boolean;

// desfaz todos os recibos da IDE, do mais novo ao mais velho; para no
// primeiro que falhar
function DesinstalarIDE(const APastaRecibos, ARaiz: string; ALog: TLogLinha;
  AReconstruirIDE: boolean; AExigirIDEFechada: boolean = True): boolean;

implementation

uses
  jsonparser, RALInst.Config.Lazarus
  {$IFDEF MSWINDOWS}, RALInst.Registro.Delphi{$ENDIF};

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
  if not (vJSON is TJSONObject) or (TJSONObject(vJSON).Get('instalador', '') <> 'RALInstaller') then
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
  Result := Result + Format(' (%d pacote(s))', [FPacotes.Count]);
  if Desfeito then
    Result := Result + ' [desfeito]';
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
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + '*.json', faAnyFile, vBusca) = 0 then
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
    vData := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', FileDateToDateTime(vBusca.Time));
    SysUtils.FindClose(vBusca);
    if (vBusca.Size <> vItem.Get('tamanho', int64(-1))) or (vData <> vItem.Get('data', '')) then
    begin
      Logar(ALog, '  mantido (mudou depois da instalação): ' + vArquivo);
      Continue;
    end;
    if SysUtils.DeleteFile(vArquivo) then
      Logar(ALog, '  apagado: ' + vArquivo)
    else
    begin
      Logar(ALog, '  ERRO: não consegui apagar ' + vArquivo + ' (em uso?)');
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
    vExec.Parametros.Add('--lazarusdir=' + ExcludeTrailingPathDelimiter(AIDE.Get('raiz', '')));
    vExec.Parametros.Add('--build-ide=');
    vExec.Log := ALog;
    Result := vExec.Executar;
    if not Result then
      Logar(ALog, 'ERRO: a reconstrução da IDE falhou; a configuração já está sem os ' +
                  'pacotes, e a IDE pede para reconstruir ao abrir');
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
    Logar(ALog, 'ERRO: recibo do Lazarus sem as listas da configuração');
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
      Logar(ALog, 'ERRO: ' + vErro);
      Exit;
    end;
    Logar(ALog, '  configuração ' + vConfig + ': links e pacotes da instalação removidos');
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
    Logar(ALog, 'Recibo já desfeito: ' + ARecibo.Arquivo);
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
        Logar(ALog, 'ERRO: há uma instalação mais nova nesta IDE (' + vNovo + '); ' +
                    'desfaça ela antes, ou desinstale a IDE inteira');
        Exit;
      end;
    finally
      vLista.Free;
    end;
  end;

  Logar(ALog, 'Desfazendo ' + ARecibo.Descricao);
  if ARecibo.Tipo = 'lazarus' then
  begin
    if AExigirIDEFechada and ProgramaEmExecucao(['lazarus.exe', 'startlazarus.exe',
                                                  'lazarus', 'startlazarus'], ARecibo.IDERaiz) then
    begin
      Logar(ALog, 'ERRO: ' + ARecibo.IDENome + ' está aberto; feche a IDE antes de desinstalar');
      Exit;
    end;
    Result := DesfazerLazarus(ARecibo, ALog, APrecisaReconstruir);
  end
  else if ARecibo.Tipo = 'delphi' then
  begin
    {$IFDEF MSWINDOWS}
    if AExigirIDEFechada and ProgramaEmExecucao(['bds.exe', 'delphi32.exe'],
                                                IncludeTrailingPathDelimiter(ARecibo.IDERaiz) + 'bin') then
    begin
      Logar(ALog, 'ERRO: ' + ARecibo.IDENome + ' está aberto; feche a IDE antes de desinstalar');
      Exit;
    end;
    Result := RALInst.Registro.Delphi.DesfazerRecibo(ARecibo.Arquivo, ALog);
    if not ApagarArquivos(ARecibo.Dados, ALog) then
      Result := False;
    {$ELSE}
    Logar(ALog, 'ERRO: recibo do Delphi só se desfaz no Windows');
    {$ENDIF}
  end
  else
    Logar(ALog, 'ERRO: tipo de recibo desconhecido: ' + ARecibo.Tipo);

  if Result then
  begin
    // desfeito nao vale mais; fica guardado para quem quiser ver
    vNovo := ChangeFileExt(ARecibo.Arquivo, '') + '.desfeito.json';
    if RenameFile(ARecibo.Arquivo, vNovo) then
    begin
      ARecibo.Arquivo := vNovo;
      ARecibo.Desfeito := True;
    end;
    Logar(ALog, 'Desfeito.');
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
      Logar(ALog, '  a IDE não foi reconstruída: ela pede para reconstruir ao abrir');
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
      Logar(ALog, 'Nenhuma instalação do RAL registrada para ' + ARaiz);
      Exit;
    end;
    // reconstruir a cada recibo e perda de tempo: uma vez no fim, se algum
    // deles tirou pacote da IDE
    vReconstruir := False;
    for vInt := 0 to Pred(vLista.Count) do
    begin
      if not Desfazer(TRecibo(vLista[vInt]), vRecibos, ALog, AExigirIDEFechada, vPrecisa) then
        Exit(False);
      vReconstruir := vReconstruir or vPrecisa;
    end;
    if vReconstruir then
      if AReconstruirIDE then
        Result := ReconstruirLazarus(TRecibo(vLista[0]).Dados.Get('ide', TJSONObject(nil)), ALog)
      else
        Logar(ALog, '  a IDE não foi reconstruída: ela pede para reconstruir ao abrir');
  finally
    vLista.Free;
    vRecibos.Free;
  end;
end;

end.
