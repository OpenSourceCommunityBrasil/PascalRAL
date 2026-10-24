program fechamento;

{$mode ObjFPC}{$H+}

// Os "Pronto quando" da secao 9 do PLANO_INSTALADOR (F14 a F21), conferidos
// sobre COPIAS: o registro do Delphi numa chave de teste
// (HKCU\Software\RALInstaller-Teste-F\BDS\<bds>), a configuracao do Lazarus numa
// pasta temporaria, os .bpl/.dcp e os recibos na pasta de trabalho. Nenhuma IDE
// de verdade e tocada, e o Lazarus nao e reconstruido.
//
//   fechamento --trabalho=<pasta curta> [--cenarios=f14d,f14l,...]
//              [--delphi=37.0] [--lazarus=<pasta lazarus da instalacao>]
//              [--ral-dev=<arvore 1.x>] [--ral-20=<arvore 2.0>] [--xe2=<raiz>]
//
// Cenarios: f14d f14l (transacional), f15 (bloqueio da versao, pela CLI), f16
// (Delphi mais novo que a tabela, Unleashed), f17 (library path nas plataformas),
// f18d f18l (so runtime), f19 (limpeza), f20 (DLLs das dependencias), f21 (1.x ->
// 2.0 -> 1.x). Sem --cenarios, todos.
//
// Precisa de rede (a 1.1 e a dev vem do GitHub, a libsagui dos releases) e do
// Delphi 13 e do Lazarus 4.9 (FPC 3.2.3) desta maquina, ou dos que forem dados.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Windows, Registry, Classes, SysUtils, StrUtils,
  RALInst.Catalogo, RALInst.Config.Lazarus, RALInst.Fontes, RALInst.IDE,
  RALInst.IDE.Delphi, RALInst.IDE.Lazarus, RALInst.Instalar.Delphi, RALInst.Limpeza,
  RALInst.Processo, RALInst.Promocao, RALInst.Registro.Delphi, RALInst.Rodada,
  RALInst.Situacao;

type
  TSaida = class
  public
    Texto: TStringList;
    procedure Linha(const ALinha: string);
  end;

procedure TSaida.Linha(const ALinha: string);
begin
  WriteLn('      | ', ALinha);
  if Texto <> nil then
    Texto.Add(ALinha);
end;

const
  ChaveTeste = '\Software\RALInstaller-Teste-F';

var
  GSaida: TSaida;
  GFalhas: integer = 0;
  GOks: integer = 0;
  GTrab, GDev, GRAL20, GRaizLazarus, GBDS, GXE2, GConfigReal: string;
  GIDEs: TIDEList;
  GDelphi, GLazarus: TIDEInstance;

procedure Conferir(ACondicao: boolean; const ATexto: string);
begin
  if ACondicao then
  begin
    WriteLn('  ok    ', ATexto);
    Inc(GOks);
  end
  else
  begin
    WriteLn('  FALHA ', ATexto);
    Inc(GFalhas);
  end;
end;

function Opcao(const ANome, APadrao: string): string;
var
  vInt: integer;
begin
  Result := APadrao;
  for vInt := 1 to ParamCount do
    if SameText(Copy(ParamStr(vInt), 1, Length(ANome) + 3), '--' + ANome + '=') then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

{ registro }

procedure ApagarChave(const AChave: string);
var
  vReg: TRegistry;
  vSub: TStringList;
  vInt: integer;
begin
  vReg := TRegistry.Create(KEY_ALL_ACCESS);
  vSub := TStringList.Create;
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if not vReg.OpenKey(AChave, False) then
      Exit;
    vReg.GetKeyNames(vSub);
    vReg.CloseKey;
    for vInt := 0 to Pred(vSub.Count) do
      ApagarChave(AChave + '\' + vSub[vInt]);
    vReg.DeleteKey(AChave);
  finally
    vSub.Free;
    vReg.Free;
  end;
end;

// todos os valores da chave e das subchaves: 'Library\Win32|Search Path=...'
procedure FotoRegistro(const AChave, ASub: string; ALista: TStrings);
var
  vReg: TRegistry;
  vValores, vSubs: TStringList;
  vInt: integer;
begin
  vReg := TRegistry.Create(KEY_READ);
  vValores := TStringList.Create;
  vSubs := TStringList.Create;
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if not vReg.OpenKeyReadOnly(AChave + IfThen(ASub <> '', '\' + ASub, '')) then
      Exit;
    vReg.GetValueNames(vValores);
    for vInt := 0 to Pred(vValores.Count) do
      if vReg.GetDataType(vValores[vInt]) in [rdString, rdExpandString] then
        ALista.Add(ASub + '|' + vValores[vInt] + '=' + vReg.ReadString(vValores[vInt]));
    vReg.GetKeyNames(vSubs);
    vReg.CloseKey;
    for vInt := 0 to Pred(vSubs.Count) do
      FotoRegistro(AChave, IfThen(ASub <> '', ASub + '\', '') + vSubs[vInt], ALista);
  finally
    vSubs.Free;
    vValores.Free;
    vReg.Free;
  end;
end;

function Foto: TStringList;
begin
  Result := TStringList.Create;
  FotoRegistro(ChaveTeste + '\BDS\' + GBDS, '', Result);
  Result.Sort;
end;

function LerReg(const ASub, AValor: string): string;
var
  vReg: TRegistry;
begin
  Result := '';
  vReg := TRegistry.Create(KEY_READ);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if vReg.OpenKeyReadOnly(ChaveTeste + '\BDS\' + GBDS + '\' + ASub) then
    begin
      if vReg.ValueExists(AValor) then
        Result := vReg.ReadString(AValor);
      vReg.CloseKey;
    end;
  finally
    vReg.Free;
  end;
end;

// os nomes dos valores de uma subchave da copia
function ValoresReg(const ASub: string): string;
var
  vReg: TRegistry;
  vLista: TStringList;
begin
  Result := '';
  vReg := TRegistry.Create(KEY_READ);
  vLista := TStringList.Create;
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if vReg.OpenKeyReadOnly(ChaveTeste + '\BDS\' + GBDS + '\' + ASub) then
    begin
      vReg.GetValueNames(vLista);
      vReg.CloseKey;
    end;
    Result := vLista.Text;
  finally
    vLista.Free;
    vReg.Free;
  end;
end;

// as linhas que mudaram entre duas fotos (as chaves das linhas)
function Diferencas(AAntes, ADepois: TStrings): TStringList;
var
  vInt: integer;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  for vInt := 0 to Pred(AAntes.Count) do
    if ADepois.IndexOf(AAntes[vInt]) < 0 then
      Result.Add(Copy(AAntes[vInt], 1, Pos('=', AAntes[vInt]) - 1));
  for vInt := 0 to Pred(ADepois.Count) do
    if AAntes.IndexOf(ADepois[vInt]) < 0 then
      Result.Add(Copy(ADepois[vInt], 1, Pos('=', ADepois[vInt]) - 1));
end;

function Iguais(A, B: TStrings): boolean;
var
  vDif: TStringList;
begin
  vDif := Diferencas(A, B);
  try
    Result := vDif.Count = 0;
    if not Result then
      WriteLn('        diferenças: ', Copy(vDif.CommaText, 1, 600));
  finally
    vDif.Free;
  end;
end;

// copia nova da chave da IDE; ALimpar tira dela o RAL instalado a mao
procedure PrepararCopiaDelphi(ALimpar: boolean);
var
  vInst: TInstalacaoDelphi;
begin
  ApagarChave(ChaveTeste);
  if not CopiarChave(GDelphi.RegKey, ChaveTeste + '\BDS\' + GBDS) then
    raise Exception.Create('não foi possível copiar a chave do Delphi');
  if not ALimpar then
    Exit;
  vInst := TInstalacaoDelphi.Create(GDelphi, nil);
  try
    vInst.ChaveRegistro := ChaveTeste + '\BDS\' + GBDS;
    vInst.ExigirIDEFechada := False;
    vInst.PastaRecibos := GTrab + 'recibos-limpeza';
    vInst.Desinstalar;
  finally
    vInst.Free;
  end;
end;

{ arquivos }

// copia uma arvore do RAL sem o que o compilador e o git gravam
procedure CopiarArvore(const AOrigem, ADestino: string);
var
  vBusca: TSearchRec;
begin
  ForceDirectories(ADestino);
  if FindFirst(IncludeTrailingPathDelimiter(AOrigem) + AllFilesMask, faAnyFile,
               vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
      begin
        if AnsiIndexText(vBusca.Name, ['.git', 'lib', 'compiled',
             '__history', '__recovery', 'backup', 'dcu', 'bin', 'Testbench', 'demos',
             'tests', 'docs', 'samples']) >= 0 then
          Continue;
        // Win32/Win64 de saida do build; os de static_libs sao os .obj do brotli
        if (AnsiIndexText(vBusca.Name, ['Win32', 'Win64']) >= 0) and
           (Pos('static_libs', AOrigem) = 0) then
          Continue;
        CopiarArvore(IncludeTrailingPathDelimiter(AOrigem) + vBusca.Name,
                     IncludeTrailingPathDelimiter(ADestino) + vBusca.Name);
      end
      else
        CopiarArquivo(IncludeTrailingPathDelimiter(AOrigem) + vBusca.Name,
                      IncludeTrailingPathDelimiter(ADestino) + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

// os arquivos da pasta (recursivo): 'caminho=tamanho|data'
procedure FotoPasta(const APasta: string; ALista: TStrings);
var
  vBusca: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + AllFilesMask, faAnyFile,
               vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        FotoPasta(IncludeTrailingPathDelimiter(APasta) + vBusca.Name, ALista)
      else
        ALista.Add(IncludeTrailingPathDelimiter(APasta) + vBusca.Name + '=' +
                   IntToStr(vBusca.Size) + '|' + IntToStr(vBusca.Time));
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

function TemAnterior(const APasta: string): boolean;
var
  vLista: TStringList;
  vInt: integer;
begin
  Result := False;
  vLista := TStringList.Create;
  try
    FotoPasta(APasta, vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if Pos(SufixoAnterior + '=', vLista[vInt]) > 0 then
        Exit(True);
  finally
    vLista.Free;
  end;
end;

procedure Quebrar(const AArquivo: string);
var
  vTexto: TStringList;
  vInt: integer;
begin
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(AArquivo);
    for vInt := 0 to Pred(vTexto.Count) do
      if SameText(Trim(vTexto[vInt]), 'implementation') then
      begin
        vTexto.Insert(vInt + 1, 'erro de proposito do teste da F14;');
        Break;
      end;
    vTexto.SaveToFile(AArquivo);
  finally
    vTexto.Free;
  end;
end;

{ Lazarus }

procedure PrepararCopiaLazarus(const APasta: string);
var
  vBusca: TSearchRec;
  vOrigem: string;
begin
  vOrigem := GConfigReal;
  if DirectoryExists(APasta) then
    ApagarPasta(APasta);
  ForceDirectories(APasta);
  if FindFirst(vOrigem + AllFilesMask, faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Attr and faDirectory) = 0 then
        CopiarArquivo(vOrigem + vBusca.Name, IncludeTrailingPathDelimiter(APasta) +
                      vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
  GLazarus.ConfigDir := IncludeTrailingPathDelimiter(APasta);
end;

function FotoLazarus: TStringList;
var
  vLinks, vInst: TStringList;
  vInt: integer;
begin
  Result := TStringList.Create;
  vLinks := TStringList.Create;
  vInst := TStringList.Create;
  try
    LerLinks(GLazarus.ConfigDir, vLinks);
    LerInstalados(GLazarus.ConfigDir, vInst);
    for vInt := 0 to Pred(vLinks.Count) do
      Result.Add('link|' + LowerCase(vLinks[vInt]));
    for vInt := 0 to Pred(vInst.Count) do
      Result.Add('ide|' + LowerCase(vInst[vInt]) + '=');
    Result.Sort;
  finally
    vInst.Free;
    vLinks.Free;
  end;
end;

{ rodadas }

function NovaRodada(const ABase, ARecibos: string): TRodada;
begin
  Result := TRodada.Create;
  Result.Log := @GSaida.Linha;
  Result.PastaBase := ABase;
  Result.PastaRecibos := ARecibos;
  Result.ExigirIDEFechada := False;
  Result.ConstruirIDE := False;
  Result.ManterInstalados := False;
  Result.RegistroTeste := ChaveTeste;
  Result.PastaSaidaTeste := GTrab + 'saida';
  // o exe de teste nao embute as receitas: as do repositorio
  if Result.Receitas.Count = 0 then
    Result.Receitas.CarregarPasta(ExpandFileName(ExtractFilePath(ParamStr(0)) +
      '..' + PathDelim + '..' + PathDelim + 'receitas'));
end;

// AFonte: pasta local, ou 'github:<ref>'
function Rodar(ARodada: TRodada; AIDE: TIDEInstance; const AFonte, APacotes: string;
  out APlano: string): boolean;
begin
  Result := False;
  APlano := '';
  if Copy(AFonte, 1, 7) = 'github:' then
  begin
    if not ARodada.EscolherVersao(Copy(AFonte, 8, MaxInt)) then
    begin
      WriteLn('      ERRO: ', ARodada.Erro);
      Exit;
    end;
  end
  else
    ARodada.UsarPastaLocal(AFonte);
  if not ARodada.CarregarCatalogo then
  begin
    WriteLn('      ERRO: ', ARodada.Erro);
    Exit;
  end;
  ARodada.Pacotes.CommaText := APacotes;
  ARodada.IDEs.Clear;
  ARodada.AdicionarIDE(AIDE);
  APlano := ARodada.Plano;
  Result := ARodada.Executar;
  if ARodada.Erro <> '' then
    WriteLn('      erro: ', ARodada.Erro);
end;

function Desinstalar(AIDE: TIDEInstance; const ARecibos: string): boolean;
var
  vRodada: TRodada;
begin
  vRodada := NovaRodada(GTrab + 'base', ARecibos);
  try
    vRodada.AdicionarIDE(AIDE);
    Result := vRodada.Desinstalar(False);
  finally
    vRodada.Free;
  end;
end;

procedure Cabecalho(const ATexto: string);
begin
  WriteLn;
  WriteLn('==== ', ATexto);
end;

{ F14 }

procedure CenarioF14Delphi;
var
  vRodada: TRodada;
  vFoto0, vFoto1, vFotoAgora, vArq1, vArqAgora: TStringList;
  vPlano, vRecibos: string;
  vOk: boolean;
begin
  Cabecalho('F14 Delphi: uma atualização que não fecha o conjunto não muda nada');
  vRecibos := GTrab + 'f14d-recibos';
  ApagarPasta(vRecibos);
  ApagarPasta(GTrab + 'saida');
  PrepararCopiaDelphi(False);
  vFoto0 := Foto;
  vFoto1 := nil;
  vArq1 := TStringList.Create;
  vArqAgora := TStringList.Create;
  vRodada := NovaRodada(GTrab + 'base', vRecibos);
  try
    vOk := Rodar(vRodada, GDelphi, 'github:1.1', 'IndyRAL', vPlano);
    Conferir(vOk, '1.1 do GitHub instalada na cópia (IndyRAL)');
    Conferir(Pos('instala se compilar: PascalRAL, PascalRALDsgn, IndyRAL', vPlano) > 0,
             'o plano diz o conjunto mínimo');
    vFoto1 := Foto;
    FotoPasta(GTrab + 'saida', vArq1);

    // a dev com o motor quebrado
    ApagarPasta(GTrab + 'fontes\quebrado');
    CopiarArvore(GDev, GTrab + 'fontes\quebrado');
    Quebrar(GTrab + 'fontes\quebrado\src\engine\indy\RALIndyServer.pas');
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\quebrado', 'IndyRAL', vPlano);
    Conferir(not vOk, 'a atualização com o motor quebrado falha');
    Conferir(vRodada.IDEsIntactas.Count = 1, 'e diz que a IDE ficou como estava: ' +
             vRodada.IDEsIntactas.Text);
    vFotoAgora := Foto;
    try
      Conferir(Iguais(vFoto1, vFotoAgora), 'registro idêntico ao da 1.1');
    finally
      vFotoAgora.Free;
    end;
    FotoPasta(GTrab + 'saida', vArqAgora);
    Conferir(Iguais(vArq1, vArqAgora), 'os .bpl/.dcp da 1.1 intactos (tamanho e data)');
    Conferir(not TemAnterior(GTrab + 'saida'), 'nenhum .anterior');
    Conferir(not DirectoryExists(PastaPreparo('delphi-' + GBDS)), 'nenhuma pasta de preparo');

    // a dev boa: atualiza, sem deixar sobra
    ApagarPasta(GTrab + 'fontes\bom');
    CopiarArvore(GDev, GTrab + 'fontes\bom');
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    Conferir(vOk, 'a atualização para a dev boa dá certo');
    Conferir(Pos('Atualizar', vPlano + vRodada.Relatorio.Text) +
             Pos('Trocar', vPlano + vRodada.Relatorio.Text) > 0, 'como atualização/troca');
    Conferir(not TemAnterior(GTrab + 'saida'), 'nenhum .anterior depois da que deu certo');
    Conferir(not DirectoryExists(PastaPreparo('delphi-' + GBDS)), 'nem pasta de preparo');

    Conferir(Desinstalar(GDelphi, vRecibos), 'desfazer os recibos');
    vFotoAgora := Foto;
    try
      Conferir(Iguais(vFoto0, vFotoAgora), 'registro idêntico ao de antes de tudo');
    finally
      vFotoAgora.Free;
    end;
  finally
    vRodada.Free;
    vArqAgora.Free;
    vArq1.Free;
    vFoto1.Free;
    vFoto0.Free;
  end;
end;

procedure CenarioF14Lazarus;
var
  vRodada: TRodada;
  vFoto0, vFoto1, vAgora: TStringList;
  vPlano, vRecibos: string;
  vOk: boolean;
begin
  Cabecalho('F14 Lazarus: compila numa cópia; a configuração só muda se fechar');
  vRecibos := GTrab + 'f14l-recibos';
  ApagarPasta(vRecibos);
  PrepararCopiaLazarus(GTrab + 'laz\config');
  vFoto0 := FotoLazarus;
  vFoto1 := nil;
  vRodada := NovaRodada(GTrab + 'base', vRecibos);
  try
    ApagarPasta(GTrab + 'fontes\bom');
    CopiarArvore(GDev, GTrab + 'fontes\bom');
    vOk := Rodar(vRodada, GLazarus, GTrab + 'fontes\bom', 'fphttpral', vPlano);
    Conferir(vOk, 'dev instalada na cópia da configuração (fphttpral)');
    Conferir(Pos('compila antes numa cópia', vPlano) > 0, 'o plano diz que compila antes');
    vFoto1 := FotoLazarus;

    ApagarPasta(GTrab + 'fontes\quebrado');
    CopiarArvore(GDev, GTrab + 'fontes\quebrado');
    Quebrar(GTrab + 'fontes\quebrado\src\engine\fpHTTP\RALfpHTTPServer.pas');
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GLazarus, GTrab + 'fontes\quebrado', 'fphttpral', vPlano);
    Conferir(not vOk, 'a troca para o motor quebrado falha');
    Conferir(vRodada.IDEsIntactas.Count = 1, 'e a configuração ficou como estava: ' +
             vRodada.IDEsIntactas.Text);
    vAgora := FotoLazarus;
    try
      Conferir(Iguais(vFoto1, vAgora), 'links e pacotes da IDE idênticos');
    finally
      vAgora.Free;
    end;
    Conferir(Desinstalar(GLazarus, vRecibos), 'desfazer os recibos');
    vAgora := FotoLazarus;
    try
      Conferir(Iguais(vFoto0, vAgora), 'configuração idêntica à de antes de tudo');
    finally
      vAgora.Free;
    end;
  finally
    vRodada.Free;
    vFoto1.Free;
    vFoto0.Free;
  end;
end;

{ F15 }

procedure CenarioF15;
var
  vExec: TExecucao;
  vCLI: string;
begin
  Cabecalho('F15: a 1.1 no XE2 é recusada antes de baixar');
  vCLI := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\cli\ralcli.exe');
  if (GXE2 = '') or not FileExists(vCLI) then
  begin
    WriteLn('  --    sem XE2 ou sem a ralcli: pulado');
    Exit;
  end;
  vExec := TExecucao.Create;
  try
    vExec.Executavel := vCLI;
    vExec.Parametros.Add('instalar');
    vExec.Parametros.Add('--versao=1.1');
    vExec.Parametros.Add('--ide=' + GXE2);
    vExec.Parametros.Add('--pacotes=IndyRAL');
    vExec.Parametros.Add('--sim');
    vExec.Parametros.Add('--pasta=' + GTrab + 'f15');
    vExec.Log := @GSaida.Linha;
    Conferir(not vExec.Executar and (vExec.CodigoSaida = 1), 'ralcli instalar sai com 1');
    Conferir(Pos('não recebe esta versão do RAL', vExec.Saida.Text) > 0,
             'com o motivo (' + Trim(Copy(vExec.Saida.Text,
               Pos('ERRO:', vExec.Saida.Text), 160)) + ')');
    Conferir(Pos('Baixando', vExec.Saida.Text) = 0, 'sem baixar nada');
    Conferir(not DirectoryExists(GTrab + 'f15\PascalRAL'), 'nem criar a pasta');
  finally
    vExec.Free;
  end;
end;

{ F16 }

procedure CenarioF16;
var
  vBusca: TBuscaDelphi;
  vBuscaL: TBuscaLazarus;
  vIDE: TIDEInstance;
  vRaiz: string;
  vTexto: TStringList;
begin
  Cabecalho('F16: Delphi mais novo que a tabela e o FPC Unleashed');
  vRaiz := GTrab + 'ide38\Studio\38.0\';
  ForceDirectories(vRaiz + 'bin');
  ForceDirectories(vRaiz + 'lib\win32\release');
  CopiarArquivo(GDelphi.RootDir + 'bin\dcc32.exe', vRaiz + 'bin\dcc32.exe');
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(GDelphi.RootDir + 'bin\rsvars.bat');
    vTexto.Text := StringReplace(StringReplace(vTexto.Text, '37.0', '38.0', [rfReplaceAll]),
                                 GDelphi.RootDir, vRaiz, [rfReplaceAll, rfIgnoreCase]);
    vTexto.SaveToFile(vRaiz + 'bin\rsvars.bat');
  finally
    vTexto.Free;
  end;
  vBusca := TBuscaDelphi.Create;
  try
    vIDE := vBusca.InspecionarPasta(vRaiz);
    Conferir(vIDE <> nil, 'a pasta com BDS 38.0 aparece');
    if vIDE <> nil then
    try
      Conferir((Pos('38.0', vIDE.Nome) > 0) and (Pos('mais novo', vIDE.Nome) > 0),
               'como mais nova que o Delphi 13: ' + vIDE.Nome);
      Conferir((vIDE.Versao = '38.0') and (vIDE.VersaoCompilador = 'VER380') and
               (ciCompilar in vIDE.Capacidades), 'com versão, VER380 e o compilador');
    finally
      vIDE.Free;
    end;
  finally
    vBusca.Free;
  end;
  vBuscaL := TBuscaLazarus.Create;
  try
    vIDE := vBuscaL.InspecionarPasta('D:\IDE\Lazarus\unleashed\lazarus');
    if vIDE = nil then
      WriteLn('  --    sem o Unleashed nesta máquina')
    else
    try
      Conferir(SameText(vIDE.VarianteFPC, 'unleashed') and (Pos('Unleashed', vIDE.Nome) > 0),
               'o Unleashed aparece com o nome dele: ' + vIDE.Nome);
    finally
      vIDE.Free;
    end;
  finally
    vBuscaL.Free;
  end;
end;

{ F17 }

procedure CenarioF17;
const
  Instaladas: array[0..8] of string = ('Win32', 'Win64', 'Android32', 'Android64',
    'iOSDevice64', 'iOSSimARM64', 'Linux64', 'OSX64', 'OSXARM64');
  Fantasmas: array[0..3] of string = ('iOSDevice32', 'iOSSimulator', 'OSX32', 'Win64x');
var
  vRodada: TRodada;
  vFoto0, vAgora, vAntes, vDif: TStringList;
  vPlano, vRecibos, vPlat, vPath, vTexto: string;
  vOk, vSoLinux: boolean;
  vInt: integer;
  vExec: TExecucao;
  vUnidade: TStringList;
  vReg: TRegistroDelphi;
begin
  Cabecalho('F17: library path nas plataformas instaladas, e só nelas');
  vRecibos := GTrab + 'f17-recibos';
  ApagarPasta(vRecibos);
  ApagarPasta(GTrab + 'saida');
  PrepararCopiaDelphi(True);
  vFoto0 := Foto;
  ApagarPasta(GTrab + 'fontes\bom');
  CopiarArvore(GDev, GTrab + 'fontes\bom');
  vRodada := NovaRodada(GTrab + 'base', vRecibos);
  try
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    Conferir(vOk, 'dev instalada na cópia');
    for vPlat in Instaladas do
      Conferir(Pos('$(PascalRAL)', LerReg('Library\' + vPlat, 'Search Path')) > 0,
               'Search Path de ' + vPlat + ' com o RAL');
    for vPlat in Fantasmas do
      Conferir(vFoto0.IndexOf('Library\' + vPlat + '|Search Path=' +
                              LerReg('Library\' + vPlat, 'Search Path')) >= 0,
               vPlat + ' (chave sem plataforma instalada) não é tocada');

    // a unidade compila para Linux64 so com o Search Path escrito
    vReg := TRegistroDelphi.Create(GDelphi);
    vUnidade := TStringList.Create;
    vExec := TExecucao.Create;
    try
      vReg.Chave := ChaveTeste + '\BDS\' + GBDS;
      vPath := '';
      vTexto := LerReg('Library\Linux64', 'Search Path');
      for vPlat in vTexto.Split([';']) do
        if Trim(vPlat) <> '' then
          vPath := vPath + IfThen(vPath <> '', ';', '') + vReg.Expandir(Trim(vPlat), 'linux64');
      ForceDirectories(GTrab + 'f17lin\dcu');
      vUnidade.Text := 'unit TesteLinux;' + LineEnding + 'interface' + LineEnding +
                       'uses RALServer, RALIndyServer;' + LineEnding + 'implementation' +
                       LineEnding + 'end.';
      vUnidade.SaveToFile(GTrab + 'f17lin\TesteLinux.pas');
      vExec.Executavel := GDelphi.RootDir + 'bin\dcclinux64.exe';
      vExec.PastaTrabalho := GTrab + 'f17lin';
      vExec.Parametros.Add('-B');
      vExec.Parametros.Add('-Q');
      vExec.Parametros.Add('-NSSystem;Posix;Data;Xml;Web');
      vExec.Parametros.Add('-U' + vPath);
      vExec.Parametros.Add('-I' + vPath);
      vExec.Parametros.Add('-N0' + GTrab + 'f17lin\dcu');
      vExec.Parametros.Add('TesteLinux.pas');
      vExec.Log := @GSaida.Linha;
      vOk := vExec.Executar;
      Conferir(vOk and FileExists(GTrab + 'f17lin\dcu\RALIndyServer.dcu'),
               'RALServer e RALIndyServer compilam com o dcclinux64 só pelo Search Path');
    finally
      vExec.Free;
      vUnidade.Free;
      vReg.Free;
    end;

    Conferir(Desinstalar(GDelphi, vRecibos), 'desinstalar');
    vAgora := Foto;
    try
      Conferir(Iguais(vFoto0, vAgora), 'a cópia volta idêntica');
    finally
      vAgora.Free;
    end;

    // Linux64 "escondido": a IDE sem a plataforma nao recebe o path dela; com
    // ela de volta, a rodada seguinte so escreve o do Linux64
    vInt := GDelphi.Plataformas.IndexOf('linux64');
    GDelphi.Plataformas.Delete(vInt);
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    Conferir(vOk and (Pos('$(PascalRAL)', LerReg('Library\Linux64', 'Search Path')) = 0),
             'sem lib\linux64, o Linux64 fica de fora');
    GDelphi.Plataformas.Add('linux64');
    vAntes := Foto;
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    vAgora := Foto;
    vDif := Diferencas(vAntes, vAgora);
    try
      vSoLinux := vDif.Count > 0;
      for vInt := 0 to Pred(vDif.Count) do
        if Pos('Library\Linux64|', vDif[vInt]) <> 1 then
          vSoLinux := False;
      Conferir(vOk and vSoLinux, 'com a plataforma de volta, só o path do Linux64 muda: ' +
               vDif.CommaText);
      Conferir(Pos('mesmos fontes', LowerCase(vPlano)) + Pos('modific', LowerCase(vPlano)) +
               Pos('só o que falta', vPlano) > 0, 'sem recompilar (mesmos fontes)');
    finally
      vDif.Free;
      vAgora.Free;
      vAntes.Free;
    end;
    Conferir(Desinstalar(GDelphi, vRecibos), 'desinstalar');

    // um .dproj do nucleo sem Android: nada em Library\Android*
    ApagarPasta(GTrab + 'fontes\semandroid');
    CopiarArvore(GDev, GTrab + 'fontes\semandroid');
    vUnidade := TStringList.Create;
    try
      vUnidade.LoadFromFile(GTrab + 'fontes\semandroid\pkg\Delphi\PascalRAL.dproj');
      for vInt := Pred(vUnidade.Count) downto 0 do
        if Pos('<Platform value="Android', vUnidade[vInt]) > 0 then
          vUnidade.Delete(vInt);
      vUnidade.SaveToFile(GTrab + 'fontes\semandroid\pkg\Delphi\PascalRAL.dproj');
    finally
      vUnidade.Free;
    end;
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\semandroid', 'IndyRAL', vPlano);
    Conferir(vOk and (Pos('$(PascalRAL)', LerReg('Library\Android32', 'Search Path') +
                                          LerReg('Library\Android64', 'Search Path')) = 0),
             'o .dproj sem Android não escreve em Library\Android*');
    Conferir(Desinstalar(GDelphi, vRecibos), 'desinstalar');
    vAgora := Foto;
    try
      Conferir(Iguais(vFoto0, vAgora), 'a cópia volta idêntica');
    finally
      vAgora.Free;
    end;
  finally
    vRodada.Free;
    vFoto0.Free;
  end;
end;

{ F18 }

procedure CenarioF18Delphi;
var
  vRodada: TRodada;
  vFoto0, vAgora, vArq1, vArq2, vDif: TStringList;
  vPlano, vRecibos: string;
  vOk: boolean;
  vExec: TExecucao;
  vPrograma: TStringList;
begin
  Cabecalho('F18 Delphi: só runtime');
  vRecibos := GTrab + 'f18d-recibos';
  ApagarPasta(vRecibos);
  ApagarPasta(GTrab + 'saida');
  PrepararCopiaDelphi(True);
  vFoto0 := Foto;
  vArq1 := TStringList.Create;
  vArq2 := TStringList.Create;
  ApagarPasta(GTrab + 'fontes\bom');
  CopiarArvore(GDev, GTrab + 'fontes\bom');
  vRodada := NovaRodada(GTrab + 'base', vRecibos);
  try
    vRodada.SoRuntime := True;
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    Conferir(vOk, 'só runtime instalado');
    vAgora := Foto;
    vDif := Diferencas(vFoto0, vAgora);
    try
      Conferir(Pos('Known Packages', vDif.Text) = 0, 'Known Packages (e x64) não mudam');
    finally
      vDif.Free;
      vAgora.Free;
    end;
    Conferir(Pos('designide', vRodada.Relatorio.Text) > 0,
             '1.x: o relatório avisa do designide');
    FotoPasta(GTrab + 'saida', vArq1);

    // um programa com pacotes de runtime compila contra os .dcp
    vPrograma := TStringList.Create;
    vExec := TExecucao.Create;
    try
      ForceDirectories(GTrab + 'f18prog');
      vPrograma.Text := 'program TesteRuntime;' + LineEnding + '{$APPTYPE CONSOLE}' +
        LineEnding + 'uses RALServer, RALIndyServer;' + LineEnding + 'begin' + LineEnding +
        'end.';
      vPrograma.SaveToFile(GTrab + 'f18prog\TesteRuntime.dpr');
      vExec.Executavel := GDelphi.RootDir + 'bin\dcc32.exe';
      vExec.PastaTrabalho := GTrab + 'f18prog';
      vExec.Parametros.Add('-B');
      vExec.Parametros.Add('-Q');
      vExec.Parametros.Add('-NSSystem;Winapi;Vcl;Data;Xml;Web');
      vExec.Parametros.Add('-U' + GTrab + 'saida\' + GBDS + '\dcp;' + GDelphi.RootDir +
                           'lib\win32\release');
      // os pacotes que o IndyRAL usa entram tambem, como a IDE faz
      vExec.Parametros.Add('-LUrtl;PascalRAL;PascalRALDsgn;IndyRAL;IndySystem;IndyCore;' +
                           'IndyProtocols;designide');
      vExec.Parametros.Add('-E' + GTrab + 'f18prog');
      vExec.Parametros.Add('-N0' + GTrab + 'f18prog');
      vExec.Parametros.Add('TesteRuntime.dpr');
      vExec.Log := @GSaida.Linha;
      Conferir(vExec.Executar and FileExists(GTrab + 'f18prog\TesteRuntime.exe'),
               'um programa com "Build with runtime packages" compila');
    finally
      vExec.Free;
      vPrograma.Free;
    end;

    // com design: so registra
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    Conferir(vOk and (Pos('só entra na IDE', vPlano) > 0), 'trocar para "com design" ' +
             'só registra');
    FotoPasta(GTrab + 'saida', vArq2);
    Conferir(Iguais(vArq1, vArq2), 'nada recompila (os .bpl são os mesmos)');
    Conferir(Pos('IndyRAL', ValoresReg('Known Packages')) > 0, 'e o IndyRAL entra na IDE');

    Conferir(Desinstalar(GDelphi, vRecibos), 'desfazer tudo');
    vAgora := Foto;
    try
      Conferir(Iguais(vFoto0, vAgora), 'a cópia volta idêntica');
    finally
      vAgora.Free;
    end;
  finally
    vRodada.Free;
    vArq2.Free;
    vArq1.Free;
    vFoto0.Free;
  end;
end;

procedure CenarioF18Lazarus;
var
  vRodada: TRodada;
  vFoto0, vAgora, vInst0, vInst1: TStringList;
  vPlano, vRecibos, vLinha: string;
  vOk: boolean;
  vExec: TExecucao;
  vArq: TStringList;
begin
  Cabecalho('F18 Lazarus: só runtime');
  vRecibos := GTrab + 'f18l-recibos';
  ApagarPasta(vRecibos);
  PrepararCopiaLazarus(GTrab + 'laz\config');
  vFoto0 := FotoLazarus;
  vInst0 := TStringList.Create;
  vInst1 := TStringList.Create;
  ApagarPasta(GTrab + 'fontes\bom');
  CopiarArvore(GDev, GTrab + 'fontes\bom');
  vRodada := NovaRodada(GTrab + 'base', vRecibos);
  try
    LerInstalados(GLazarus.ConfigDir, vInst0);
    vRodada.SoRuntime := True;
    vOk := Rodar(vRodada, GLazarus, GTrab + 'fontes\bom', 'fphttpral', vPlano);
    Conferir(vOk, 'só runtime instalado');
    LerInstalados(GLazarus.ConfigDir, vInst1);
    // a copia pode vir com o RAL de outra pasta na IDE: trocar de fontes tira
    // esses pacotes (os links deles saem); o so runtime nao poe nenhum
    vOk := vInst1.IndexOf('fphttpral') < 0;
    for vLinha in vInst1 do
      if vInst0.IndexOf(vLinha) < 0 then
        vOk := False;
    Conferir(vOk, 'nada entra em StaticAutoInstallPackages');
    Conferir(Pos('reconstrói', vPlano) = 0, 'e a IDE não é reconstruída');

    // um .lpi que exige o fphttpral compila
    vArq := TStringList.Create;
    vExec := TExecucao.Create;
    try
      ForceDirectories(GTrab + 'f18lprog');
      vArq.Text := 'program testeruntime;' + LineEnding + 'uses RALfpHTTPServer;' +
                   LineEnding + 'begin' + LineEnding + 'end.';
      vArq.SaveToFile(GTrab + 'f18lprog\testeruntime.lpr');
      vArq.Text :=
        '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
        '<CONFIG><ProjectOptions><Version Value="12"/>' + LineEnding +
        '<General><Flags><MainUnitHasCreateFormStatements Value="False"/></Flags>' +
        '<Title Value="testeruntime"/></General>' + LineEnding +
        '<RequiredPackages><Item><PackageName Value="fphttpral"/></Item></RequiredPackages>' +
        LineEnding + '<Units><Unit><Filename Value="testeruntime.lpr"/><IsPartOfProject ' +
        'Value="True"/></Unit></Units></ProjectOptions>' + LineEnding +
        '<CompilerOptions><Version Value="11"/><Target><Filename Value="testeruntime"/>' +
        '</Target></CompilerOptions></CONFIG>';
      vArq.SaveToFile(GTrab + 'f18lprog\testeruntime.lpi');
      vExec.Executavel := GLazarus.BuildFile;
      vExec.Parametros.Add('--primary-config-path=' +
                           ExcludeTrailingPathDelimiter(GLazarus.ConfigDir));
      vExec.Parametros.Add('--lazarusdir=' + ExcludeTrailingPathDelimiter(GLazarus.RootDir));
      vExec.Parametros.Add(GTrab + 'f18lprog\testeruntime.lpi');
      vExec.Log := @GSaida.Linha;
      Conferir(vExec.Executar, 'um .lpi que exige o fphttpral compila');
    finally
      vExec.Free;
      vArq.Free;
    end;

    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GLazarus, GTrab + 'fontes\bom', 'fphttpral', vPlano);
    LerInstalados(GLazarus.ConfigDir, vInst1);
    Conferir(vOk and (vInst1.IndexOf('fphttpral') >= 0), 'trocar para "com design" ' +
             'põe o pacote na IDE');
    Conferir(Desinstalar(GLazarus, vRecibos), 'desfazer tudo');
    vAgora := FotoLazarus;
    try
      Conferir(Iguais(vFoto0, vAgora), 'configuração idêntica à de antes');
    finally
      vAgora.Free;
    end;
  finally
    vRodada.Free;
    vInst1.Free;
    vInst0.Free;
    vFoto0.Free;
  end;
end;

{ F19 }

procedure CenarioF19;
var
  vRodada: TRodada;
  vSobras: TSobras;
  vBases, vRecibo: TStringList;
  vBase, vRecibos, vPlano: string;
  vOk: boolean;
  vInt: integer;

  function Tem(const ATrecho: string): boolean;
  var
    vI: integer;
  begin
    Result := False;
    for vI := 0 to Pred(vSobras.Count) do
      if Pos(LowerCase(ATrecho), LowerCase(vSobras[vI].Caminho)) > 0 then
        Exit(True);
  end;

  function SoDoInstalador: integer;
  var
    vI: integer;
  begin
    Result := 0;
    for vI := 0 to Pred(vSobras.Count) do
      if vSobras[vI].Tipo in [tsRAL, tsDependencia, tsRedist] then
        Inc(Result);
  end;

begin
  Cabecalho('F19: limpar só o que ninguém usa');
  vBase := GTrab + 'f19base\';
  vRecibos := GTrab + 'f19-recibos';
  ApagarPasta(vBase);
  ApagarPasta(vRecibos);
  ApagarPasta(GTrab + 'saida');
  PrepararCopiaDelphi(True);
  PrepararCopiaLazarus(GTrab + 'laz\config');
  vSobras := TSobras.Create(True);
  vBases := TStringList.Create;
  vBases.Add(vBase);
  vRecibo := TStringList.Create;
  vRodada := NovaRodada(vBase, vRecibos);
  try
    vOk := Rodar(vRodada, GDelphi, 'github:1.1', 'IndyRAL', vPlano);
    Conferir(vOk, '1.1 no Delphi (cópia)');
    FreeAndNil(vRodada);
    vRodada := NovaRodada(vBase, vRecibos);
    vOk := Rodar(vRodada, GLazarus, 'github:dev', 'fphttpral', vPlano);
    Conferir(vOk, 'dev no Lazarus (cópia)');
    // uma dependencia que ja era do usuario (encontrada) e uma pasta sem a marca
    ForceDirectories(vBase + 'dependencias\Zeos\usuario');
    vRecibo.Text := '{}';
    vRecibo.SaveToFile(vBase + 'dependencias\Zeos\usuario\' + ArquivoMarca);
    vRecibo.Text := '{ "instalador": "RALInstaller", "data": "2026-10-18T10:00:00", ' +
      '"ide": { "tipo": "delphi", "nome": "IDE falsa", "raiz": "C:\\IDEFalsa\\" }, ' +
      '"fontes": "", "dependencias": [ { "nome": "Zeos", "origem": "encontrada", ' +
      '"onde": "$(zeos) = ' + StringReplace(vBase, '\', '\\', [rfReplaceAll]) +
      'dependencias\\Zeos\\usuario" } ], "pacotes": [] }';
    vRecibo.SaveToFile(IncludeTrailingPathDelimiter(vRecibos) + 'delphi-falsa.json');
    ForceDirectories(vBase + 'dependencias\Alheia\1.0');
    vRecibo.Text := 'nao e do instalador';
    vRecibo.SaveToFile(vBase + 'dependencias\Alheia\1.0\leiame.txt');

    Conferir(Desinstalar(GDelphi, vRecibos), 'desinstalar o Delphi');
    vSobras.Listar(vBases, vRecibos);
    Conferir(Tem('PascalRAL\1.1'), 'a 1.1, que só o Delphi usava, aparece');
    Conferir(not Tem('PascalRAL\dev'), 'a dev, que o Lazarus usa, não');
    Conferir(not Tem('Zeos\usuario') and not Tem('Alheia'),
             'nem a dependência encontrada nem a pasta sem marca');

    Conferir(Desinstalar(GLazarus, vRecibos), 'desinstalar o Lazarus');
    vSobras.Listar(vBases, vRecibos);
    Conferir(Tem('PascalRAL\1.1') and Tem('PascalRAL\dev'), 'agora as duas versões');
    // o cache e o preparo sao da maquina toda: o teste nao os apaga
    for vInt := 0 to Pred(vSobras.Count) do
      vSobras[vInt].Manter := vSobras[vInt].Tipo in [tsCache, tsPreparo];
    Conferir(vSobras.Apagar(vRecibos, @GSaida.Linha), 'apagar o confirmado');
    Conferir(not DirectoryExists(vBase + 'PascalRAL\1.1') and
             not DirectoryExists(vBase + 'PascalRAL\dev'), 'as versões saíram');
    Conferir(DirectoryExists(vBase + 'dependencias\Zeos\usuario'),
             'a dependência encontrada ficou intacta');
    Conferir(DirectoryExists(vBase + 'dependencias\Alheia\1.0'), 'a pasta sem marca também');
    vRecibo.Clear;
    FotoPasta(IncludeTrailingPathDelimiter(vRecibos) + 'desinstalacoes', vRecibo);
    Conferir(Pos('limpeza-', vRecibo.Text) > 0,
             'o que foi apagado vai para o registro de desinstalação');

    // uma pasta que nao e do instalador: lista vazia
    ForceDirectories(GTrab + 'f19alheia\PascalRAL\x');
    vBases.Clear;
    vBases.Add(GTrab + 'f19alheia');
    vSobras.Listar(vBases, GTrab + 'f19-vazio');
    Conferir(SoDoInstalador = 0, 'a lista de uma pasta sem a marca vem vazia');
  finally
    vRodada.Free;
    vRecibo.Free;
    vBases.Free;
    vSobras.Free;
  end;
end;

{ F20 }

procedure CenarioF20;
var
  vRodada: TRodada;
  vPlano, vRecibos, vBase: string;
  vOk: boolean;
  vLinha: string;
begin
  Cabecalho('F20: as DLLs das dependências e o path delas nas plataformas');
  vBase := GTrab + 'f20base\';
  vRecibos := GTrab + 'f20-recibos';
  ApagarPasta(vRecibos);
  ApagarPasta(GTrab + 'saida');
  PrepararCopiaDelphi(True);
  ApagarPasta(GTrab + 'fontes\bom');
  CopiarArvore(GDev, GTrab + 'fontes\bom');
  vRodada := NovaRodada(vBase, vRecibos);
  try
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'SaguiRAL', vPlano);
    Conferir(Pos('libsagui', vPlano) > 0, 'o plano baixa a libsagui');
    Conferir(vOk, 'SaguiRAL instalado');
    Conferir(FileExists(vBase + 'redist\win32\libsagui-3.dll'), 'a DLL em redist\win32');
    Conferir(FileExists(vBase + 'redist\win64\libsagui-3.dll'), 'e em redist\win64');
    Conferir(Pos('redist', vRodada.Relatorio.Text) > 0, 'o relatório diz onde');
    Conferir(Desinstalar(GDelphi, vRecibos), 'desinstalar');

    // o plano de um Delphi com Linux64: o mORMot2 tambem no path do Linux64
    FreeAndNil(vRodada);
    vRodada := NovaRodada(vBase, vRecibos);
    vRodada.IgnorarExistentes := True;
    vRodada.UsarPastaLocal(GTrab + 'fontes\bom');
    vRodada.CarregarCatalogo;
    vRodada.Pacotes.CommaText := 'SynopseRAL';
    vRodada.AdicionarIDE(GDelphi);
    vPlano := vRodada.Plano;
    vOk := False;
    for vLinha in vPlano.Split([LineEnding]) do
      if (Pos('library path dela em', vLinha) > 0) and (Pos('Linux64', vLinha) > 0) then
        vOk := True;
    Conferir(vOk, 'o plano mostra o mORMot2 também no path do Linux64');
  finally
    vRodada.Free;
  end;
end;

{ F21 }

procedure CenarioF21;
var
  vRodada: TRodada;
  vFoto0, vAgora: TStringList;
  vPlano, vRecibos, vPlat, vTexto: string;
  vOk: boolean;

  // nenhuma entrada $(PascalRAL)\X cuja pasta nao existe nos fontes atuais
  function PathLimpo(const AFontes: string): boolean;
  var
    vItem: string;
  begin
    Result := True;
    for vPlat in TStringArray.Create('Win32', 'Win64', 'Linux64') do
      for vItem in LerReg('Library\' + vPlat, 'Search Path').Split([';']) do
        if (Pos('$(PascalRAL)', vItem) = 1) and
           not DirectoryExists(AFontes + 'src' + Copy(Trim(vItem), 13, MaxInt)) then
        begin
          WriteLn('        sobra: ', vPlat, ' ', vItem);
          Result := False;
        end;
  end;

begin
  Cabecalho('F21: 1.x -> 2.0 -> 1.x sem sobra da outra versão');
  if not DirectoryExists(GRAL20) then
  begin
    WriteLn('  --    sem a árvore 2.0: pulado');
    Exit;
  end;
  vRecibos := GTrab + 'f21-recibos';
  ApagarPasta(vRecibos);
  ApagarPasta(GTrab + 'saida');
  PrepararCopiaDelphi(True);
  vFoto0 := Foto;
  ApagarPasta(GTrab + 'fontes\bom');
  CopiarArvore(GDev, GTrab + 'fontes\bom');
  ApagarPasta(GTrab + 'fontes\v20');
  CopiarArvore(GRAL20, GTrab + 'fontes\v20');
  vRodada := NovaRodada(GTrab + 'base', vRecibos);
  try
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    Conferir(vOk and PathLimpo(GTrab + 'fontes\bom\'), '1.x instalada, path limpo');
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vRodada.ManterInstalados := True;
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\v20', 'IndyRAL', vPlano);
    Conferir(vOk, '2.0 por cima');
    Conferir(PathLimpo(GTrab + 'fontes\v20\'), 'sem $(PascalRAL)\base da 1.x no path');
    Conferir(Pos('RALStoragePackage', vPlano) > 0, 'a herança marca os pacotes novos');
    vTexto := RALInst.Situacao.DescreverFontes(FontesDaPasta(GTrab + 'fontes\v20\', '', ''));
    Conferir(Pos('2.0.0', vTexto) > 0, 'a árvore 2.0 se apresenta como RAL 2.0.0: ' + vTexto);
    FreeAndNil(vRodada);
    vRodada := NovaRodada(GTrab + 'base', vRecibos);
    vOk := Rodar(vRodada, GDelphi, GTrab + 'fontes\bom', 'IndyRAL', vPlano);
    Conferir(vOk and PathLimpo(GTrab + 'fontes\bom\'), 'de volta à 1.x, sem o \core da 2.0');
    Conferir(Pos('RALStoragePackage', ValoresReg('Known Packages')) = 0,
             'nenhum pacote da 2.0 na IDE');
    Conferir(Desinstalar(GDelphi, vRecibos), 'desinstalar');
    vAgora := Foto;
    try
      Conferir(Iguais(vFoto0, vAgora), 'a cópia volta idêntica');
    finally
      vAgora.Free;
    end;
  finally
    vRodada.Free;
    vFoto0.Free;
  end;
end;

var
  GCenarios: TStringList;
  GBuscaD: TBuscaDelphi;
  GBuscaL: TBuscaLazarus;
  GInt: integer;
  GCenario: string;

begin
  SetConsoleOutputCP(CP_UTF8);
  DefaultSystemCodePage := CP_UTF8;
  SetTextCodePage(Output, CP_UTF8);
  GTrab := IncludeTrailingPathDelimiter(Opcao('trabalho', 'D:\ralft'));
  GBDS := Opcao('delphi', '37.0');
  GDev := IncludeTrailingPathDelimiter(Opcao('ral-dev', ExpandFileName(
            ExtractFilePath(ParamStr(0)) + '..\..\..\PascalRAL-dev')));
  GRAL20 := IncludeTrailingPathDelimiter(Opcao('ral-20', ExpandFileName(
            ExtractFilePath(ParamStr(0)) + '..\..\..\PascalRAL-2.0')));
  GRaizLazarus := Opcao('lazarus', 'D:\IDE\Lazarus\4.8\lazarus');
  GCenarios := TStringList.Create;
  GCenarios.CommaText := Opcao('cenarios',
    'f15,f16,f14d,f14l,f17,f18d,f18l,f20,f21,f19');
  GSaida := TSaida.Create;
  GIDEs := TIDEList.Create(True);
  try
    ForceDirectories(GTrab);
    GBuscaD := TBuscaDelphi.Create;
    try
      GBuscaD.BuscarPadrao(GIDEs);
      GBuscaD.Finalizar(GIDEs);
    finally
      GBuscaD.Free;
    end;
    GDelphi := nil;
    GXE2 := Opcao('xe2', '');
    for GInt := 0 to Pred(GIDEs.Count) do
    begin
      if (GIDEs[GInt].Tipo = tiDelphi) and (GIDEs[GInt].BDSVersao = GBDS) and
         (GIDEs[GInt].RegKey <> '') and (GDelphi = nil) then
        GDelphi := GIDEs[GInt];
      if (GXE2 = '') and (GIDEs[GInt].Tipo = tiDelphi) and
         (GIDEs[GInt].BDSVersao = '9.0') and (GIDEs[GInt].Origem = oiRegistro) then
        GXE2 := ExcludeTrailingPathDelimiter(GIDEs[GInt].RootDir);
    end;
    GBuscaL := TBuscaLazarus.Create;
    try
      GLazarus := GBuscaL.InspecionarPasta(GRaizLazarus);
    finally
      GBuscaL.Free;
    end;
    if (GDelphi = nil) or (GLazarus = nil) then
    begin
      WriteLn('falta o Delphi ', GBDS, ' registrado ou o Lazarus em ', GRaizLazarus);
      Halt(2);
    end;
    // a configuracao de verdade, de onde as copias saem
    GConfigReal := GLazarus.ConfigDir;
    WriteLn('Delphi : ', GDelphi.Nome, ' (', GDelphi.RootDir, ') -> cópia HKCU', ChaveTeste,
            '\BDS\', GBDS);
    WriteLn('Lazarus: ', GLazarus.Nome, ' FPC ', GLazarus.VersaoCompilador, ' (',
            GLazarus.RootDir, ')');
    WriteLn('1.x    : ', GDev);
    WriteLn('2.0    : ', GRAL20);
    WriteLn('trab.  : ', GTrab);

    for GCenario in GCenarios do
      try
        case LowerCase(GCenario) of
          'f14d': CenarioF14Delphi;
          'f14l': CenarioF14Lazarus;
          'f15':  CenarioF15;
          'f16':  CenarioF16;
          'f17':  CenarioF17;
          'f18d': CenarioF18Delphi;
          'f18l': CenarioF18Lazarus;
          'f19':  CenarioF19;
          'f20':  CenarioF20;
          'f21':  CenarioF21;
        else
          WriteLn('cenário desconhecido: ', GCenario);
        end;
      except
        on E: Exception do
          Conferir(False, GCenario + ': exceção ' + E.ClassName + ': ' + E.Message);
      end;
  finally
    ApagarChave(ChaveTeste);
    GLazarus.Free;
    GIDEs.Free;
    GSaida.Free;
    GCenarios.Free;
  end;
  WriteLn;
  WriteLn(Format('%d ok, %d falha(s)', [GOks, GFalhas]));
  Halt(Ord(GFalhas > 0));
end.
