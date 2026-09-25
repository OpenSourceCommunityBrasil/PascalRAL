program existente;

{$mode ObjFPC}{$H+}

// O RAL que uma IDE ja tem, instalado pelo instalador ou a mao:
//
//   existente <raiz da IDE> [<raiz da IDE>]...      so le e mostra
//   existente --testes <raiz da IDE>...              desinstala numa COPIA
//
// A leitura nao grava nada: serve nas IDEs de uso do dia a dia. Os testes
// desinstalam o RAL de uma copia e desfazem, conferindo cada passo:
// - Delphi: a chave do registro vai para HKCU\Software\RALInstaller-Teste
//   (apagada no fim);
// - Lazarus: os .xml da configuracao vao para uma pasta temporaria, e a IDE
//   nao e reconstruida.
// A chave e a configuracao verdadeiras sao conferidas no fim: nao mudam.

uses
  {$IFDEF MSWINDOWS}
  Windows, Registry, RALInst.IDE.Delphi, RALInst.Instalar.Delphi,
  RALInst.Registro.Delphi,
  {$ENDIF}
  Classes, SysUtils,
  RALInst.Config.Lazarus, RALInst.Existente, RALInst.IDE, RALInst.IDE.Lazarus,
  RALInst.Instalar.Lazarus, RALInst.Recibos;

type
  TSaida = class
    procedure Linha(const ALinha: string);
  end;

var
  GFalhas: integer = 0;
  GSaida: TSaida;

procedure TSaida.Linha(const ALinha: string);
begin
  WriteLn('   | ', ALinha);
end;

procedure Conferir(ACondicao: boolean; const ATexto: string);
begin
  if ACondicao then
    WriteLn('   ok    ', ATexto)
  else
  begin
    WriteLn('   FALHA ', ATexto);
    Inc(GFalhas);
  end;
end;

procedure Mostrar(AIDE: TIDEInstance);
var
  vExistente: TInstalacaoExistente;
  vLinha: string;
begin
  {$IFDEF MSWINDOWS}
  if AIDE.Tipo = tiDelphi then
    vExistente := DetectarDelphi(AIDE, '', nil)
  else
  {$ENDIF}
    vExistente := DetectarLazarus(AIDE, nil);
  try
    WriteLn('== ', AIDE.Nome, ' (', ExcludeTrailingPathDelimiter(AIDE.RootDir), ')',
            BoolToStr(AIDE.IDE64, ', com IDE de 64 bits', ''));
    if not vExistente.Existe then
    begin
      WriteLn('   sem RAL');
      Exit;
    end;
    WriteLn('   resumo: ', vExistente.Resumo);
    if vExistente.Variavel <> '' then
      WriteLn('   $(PascalRAL) = ', vExistente.Variavel);
    for vLinha in vExistente.Pacotes do
      WriteLn('   pacote: ', vLinha);
    for vLinha in vExistente.Pacotes64 do
      WriteLn('   pacote x64: ', vLinha);
    for vLinha in vExistente.Links do
      WriteLn('   link: ', vLinha);
    for vLinha in vExistente.Caminhos do
      WriteLn('   caminho: ', vLinha);
  finally
    vExistente.Free;
  end;
end;

// o registro de desfazer que a desinstalacao deixou
function RegistroDesfazer(const APastaRecibos: string): string;
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  Result := '';
  vPasta := IncludeTrailingPathDelimiter(APastaRecibos) + 'desinstalacoes' + PathDelim;
  if FindFirst(vPasta + '*.json', faAnyFile, vBusca) = 0 then
  try
    Result := vPasta + vBusca.Name;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

function PastaTeste(const ANome: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir) + 'ralinst-existente' + PathDelim +
            ANome + PathDelim;
  ForceDirectories(Result);
end;

procedure LimparPasta(const APasta: string);
var
  vBusca: TSearchRec;
begin
  if FindFirst(APasta + '*', faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
      begin
        LimparPasta(APasta + vBusca.Name + PathDelim);
        RemoveDir(APasta + vBusca.Name);
      end
      else
        SysUtils.DeleteFile(APasta + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

{$IFDEF MSWINDOWS}
const
  ChaveTeste = '\Software\RALInstaller-Teste';

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

// 'subchave|valor=dado' de tudo o que a desinstalacao pode mexer
function FotoRegistro(AIDE: TIDEInstance; const AChave: string): TStringList;
const
  Listas: array[0..4] of string = ('Known Packages', 'Known Packages x64',
    'Disabled Packages', 'Disabled Packages x64', 'Environment Variables');
var
  vReg: TRegistroDelphi;
  vValores, vSubs: TStringList;
  vSub, vValor: string;
begin
  Result := TStringList.Create;
  vReg := TRegistroDelphi.Create(AIDE);
  vValores := TStringList.Create;
  vSubs := TStringList.Create;
  try
    vReg.Chave := AChave;
    for vSub in Listas do
    begin
      vReg.ListarValores(vSub, vValores);
      for vValor in vValores do
        Result.Add(vSub + '|' + vValor + '=' + vReg.LerValor(vSub, vValor));
    end;
    vReg.ListarSubchaves('Library', vSubs);
    vSubs.Add('');
    for vSub in vSubs do
      Result.Add('Library\' + vSub + '|Search Path=' +
                 vReg.LerValor(ExcludeTrailingPathDelimiter('Library\' + vSub),
                               'Search Path'));
    Result.Sort;
  finally
    vSubs.Free;
    vValores.Free;
    vReg.Free;
  end;
end;

procedure TestarDelphi(AIDE: TIDEInstance);
var
  vInst: TInstalacaoDelphi;
  vAntes, vDepois, vReal: TStringList;
  vExistente, vDepoisExistente: TInstalacaoExistente;
  vPasta, vRegistro, vPlano, vLinha: string;
  vOk: boolean;
begin
  WriteLn('== teste Delphi: ', AIDE.Nome, ' (copia de HKCU', AIDE.RegKey, ')');
  vReal := FotoRegistro(AIDE, AIDE.RegKey);
  vPasta := PastaTeste('delphi');
  LimparPasta(vPasta);
  ApagarChave(ChaveTeste);
  vAntes := nil;
  vDepois := nil;
  vExistente := nil;
  vDepoisExistente := nil;
  vInst := nil;
  try
    Conferir(CopiarChave(AIDE.RegKey, ChaveTeste), 'copiar a chave para ' + ChaveTeste);
    vAntes := FotoRegistro(AIDE, ChaveTeste);
    vExistente := DetectarDelphi(AIDE, ChaveTeste, nil);
    Conferir(vExistente.Existe, 'a copia tem RAL: ' + vExistente.Resumo);

    vInst := TInstalacaoDelphi.Create(AIDE, nil);
    vInst.ChaveRegistro := ChaveTeste;
    vInst.PastaRecibos := vPasta;
    vInst.ExigirIDEFechada := False;
    vInst.Log := @GSaida.Linha;
    vPlano := vInst.PlanoDesinstalar;
    WriteLn(vPlano);
    if vExistente.Pacotes.Count > 0 then
      Conferir(Pos(vExistente.Pacotes.Names[0], vPlano) > 0, 'o plano cita o ' +
               vExistente.Pacotes.Names[0]);
    vOk := vInst.Desinstalar;
    Conferir(vOk, 'desinstalar');
    if vInst.Relatorio.Count > 0 then
      WriteLn(TrimRight(vInst.Relatorio.Text));

    vDepoisExistente := DetectarDelphi(AIDE, ChaveTeste, nil);
    Conferir(not vDepoisExistente.Existe, 'nada do RAL depois: ' +
             vDepoisExistente.Resumo);
    vDepois := FotoRegistro(AIDE, ChaveTeste);
    // o que nao e do RAL fica: RALRESTDW, $(RDW2RAL), os outros pacotes
    for vLinha in vAntes do
      if (Pos('RESTDW', UpperCase(vLinha)) > 0) and
         (Pos('KNOWN PACKAGES', UpperCase(vLinha)) = 1) then
        Conferir(vDepois.IndexOf(vLinha) >= 0, 'continua: ' + Copy(vLinha, 1, 90));
    Conferir(vDepois.Count > 0, 'a chave nao ficou vazia');

    vRegistro := RegistroDesfazer(vPasta);
    Conferir(vRegistro <> '', 'registro de desfazer: ' + vRegistro);
    if vRegistro <> '' then
    begin
      Conferir(DesfazerDesinstalacao(vRegistro, @GSaida.Linha), 'desfazer');
      vDepois.Free;
      vDepois := FotoRegistro(AIDE, ChaveTeste);
      Conferir(vDepois.Text = vAntes.Text, 'depois de desfazer, igual ao antes');
    end;
  finally
    vInst.Free;
    vDepoisExistente.Free;
    vExistente.Free;
    vDepois.Free;
    vAntes.Free;
    ApagarChave(ChaveTeste);
    LimparPasta(vPasta);
  end;
  vDepois := FotoRegistro(AIDE, AIDE.RegKey);
  try
    Conferir(vDepois.Text = vReal.Text, 'a chave verdadeira nao mudou');
  finally
    vDepois.Free;
    vReal.Free;
  end;
end;
{$ENDIF}

function LerArquivo(const AArquivo: string): string;
var
  vTexto: TStringList;
begin
  Result := '';
  if not FileExists(AArquivo) then
    Exit;
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(AArquivo);
    Result := vTexto.Text;
  finally
    vTexto.Free;
  end;
end;

procedure CopiarXml(const AOrigem, ADestino: string);
var
  vBusca: TSearchRec;
  vTexto: TStringList;
begin
  if FindFirst(AOrigem + '*.xml', faAnyFile, vBusca) = 0 then
  try
    repeat
      vTexto := TStringList.Create;
      try
        vTexto.LoadFromFile(AOrigem + vBusca.Name);
        vTexto.SaveToFile(ADestino + vBusca.Name);
      finally
        vTexto.Free;
      end;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

procedure Diferenca(AAntes, ADepois: TStrings);
var
  vLinha: string;
begin
  for vLinha in AAntes do
    if ADepois.IndexOf(vLinha) < 0 then
      WriteLn('   - ', vLinha);
  for vLinha in ADepois do
    if AAntes.IndexOf(vLinha) < 0 then
      WriteLn('   + ', vLinha);
end;

procedure TestarLazarus(AIDE: TIDEInstance);
var
  vCopia: TIDEInstance;
  vInst: TInstalacaoLazarus;
  vLinksAntes, vInstAntes, vLinks, vInst2: TStringList;
  vExistente, vDepoisExistente: TInstalacaoExistente;
  vPasta, vRecibos, vRegistro, vLinksReal, vInstReal: string;
  vInt, vForaRAL: integer;
begin
  WriteLn('== teste Lazarus: ', AIDE.Nome, ' (copia de ', AIDE.ConfigDir, ')');
  vLinksReal := LerArquivo(AIDE.ConfigDir + 'packagefiles.xml');
  vInstReal := LerArquivo(AIDE.ConfigDir + 'miscellaneousoptions.xml');
  vPasta := PastaTeste('lazarus-config');
  vRecibos := PastaTeste('lazarus-recibos');
  LimparPasta(vPasta);
  LimparPasta(vRecibos);
  CopiarXml(AIDE.ConfigDir, vPasta);

  vCopia := TIDEInstance.Create(tiLazarus);
  vLinksAntes := TStringList.Create;
  vInstAntes := TStringList.Create;
  vLinks := TStringList.Create;
  vInst2 := TStringList.Create;
  vExistente := nil;
  vDepoisExistente := nil;
  vInst := nil;
  try
    vCopia.Assign(AIDE);
    vCopia.ConfigDir := vPasta;
    LerLinks(vPasta, vLinksAntes);
    LerInstalados(vPasta, vInstAntes);
    vExistente := DetectarLazarus(vCopia, nil);
    Conferir(vExistente.Existe, 'a copia tem RAL: ' + vExistente.Resumo);

    vInst := TInstalacaoLazarus.Create(vCopia, nil);
    vInst.PastaRecibos := vRecibos;
    vInst.ExigirIDEFechada := False;
    vInst.ConstruirIDE := False;
    vInst.Log := @GSaida.Linha;
    WriteLn(vInst.PlanoDesinstalar);
    Conferir(vInst.Desinstalar, 'desinstalar');
    if vInst.Relatorio.Count > 0 then
      WriteLn(TrimRight(vInst.Relatorio.Text));

    vDepoisExistente := DetectarLazarus(vCopia, nil);
    Conferir(not vDepoisExistente.Existe, 'nada do RAL depois: ' +
             vDepoisExistente.Resumo);
    // o que nao e do RAL fica
    LerLinks(vPasta, vLinks);
    LerInstalados(vPasta, vInst2);
    vForaRAL := 0;
    for vInt := 0 to Pred(vLinksAntes.Count) do
      if not vExistente.DoRAL(vLinksAntes.Names[vInt]) then
        Inc(vForaRAL);
    Conferir(vLinks.Count = vForaRAL, Format('links que nao sao do RAL: %d de %d',
                                             [vLinks.Count, vForaRAL]));
    vForaRAL := 0;
    for vInt := 0 to Pred(vInstAntes.Count) do
      if not vExistente.DoRAL(vInstAntes[vInt]) then
        Inc(vForaRAL);
    Conferir(vInst2.Count = vForaRAL, Format('instalados que nao sao do RAL: %d de %d',
                                             [vInst2.Count, vForaRAL]));

    vRegistro := RegistroDesfazer(vRecibos);
    Conferir(vRegistro <> '', 'registro de desfazer: ' + vRegistro);
    if vRegistro <> '' then
    begin
      Conferir(DesfazerDesinstalacao(vRegistro, @GSaida.Linha), 'desfazer');
      LerLinks(vPasta, vLinks);
      LerInstalados(vPasta, vInst2);
      vLinks.Sort;
      vLinksAntes.Sort;
      vInst2.Sort;
      vInstAntes.Sort;
      Conferir(vLinks.Text = vLinksAntes.Text, 'links iguais ao antes');
      Conferir(vInst2.Text = vInstAntes.Text, 'instalados iguais ao antes');
      Diferenca(vLinksAntes, vLinks);
      Diferenca(vInstAntes, vInst2);
    end;
  finally
    vInst.Free;
    vDepoisExistente.Free;
    vExistente.Free;
    vInst2.Free;
    vLinks.Free;
    vInstAntes.Free;
    vLinksAntes.Free;
    vCopia.Free;
    LimparPasta(vPasta);
    LimparPasta(vRecibos);
  end;
  Conferir((LerArquivo(AIDE.ConfigDir + 'packagefiles.xml') = vLinksReal) and
           (LerArquivo(AIDE.ConfigDir + 'miscellaneousoptions.xml') = vInstReal),
           'a configuracao verdadeira nao mudou');
end;

var
  GInt, GPrimeiro: integer;
  GTestes: boolean;
  GIDE: TIDEInstance;
  GLaz: TBuscaLazarus;
  {$IFDEF MSWINDOWS}
  GDelphi: TBuscaDelphi;
  GLista: TIDEList;
  {$ENDIF}

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  GTestes := (ParamCount > 0) and (ParamStr(1) = '--testes');
  GPrimeiro := 1;
  if GTestes then
    GPrimeiro := 2;
  GSaida := TSaida.Create;
  GLaz := TBuscaLazarus.Create;
  {$IFDEF MSWINDOWS}
  GDelphi := TBuscaDelphi.Create;
  GLista := TIDEList.Create(True);
  {$ENDIF}
  try
    for GInt := GPrimeiro to ParamCount do
    begin
      GIDE := GLaz.InspecionarPasta(ParamStr(GInt));
      {$IFDEF MSWINDOWS}
      if GIDE = nil then
      begin
        // a chave do registro vem do Finalizar, como na tela
        GIDE := GDelphi.InspecionarPasta(ParamStr(GInt));
        if GIDE <> nil then
        begin
          GIDE := GLista.Adicionar(GIDE);
          GDelphi.Finalizar(GLista);
        end;
      end;
      {$ENDIF}
      if GIDE = nil then
        WriteLn('== nao e uma IDE: ', ParamStr(GInt))
      else if not GTestes then
        Mostrar(GIDE)
      {$IFDEF MSWINDOWS}
      else if GIDE.Tipo = tiDelphi then
        TestarDelphi(GIDE)
      {$ENDIF}
      else
        TestarLazarus(GIDE);
    end;
  finally
    {$IFDEF MSWINDOWS}
    GDelphi.Free;
    {$ENDIF}
    GLaz.Free;
    GSaida.Free;
  end;
  if GTestes then
  begin
    WriteLn;
    if GFalhas = 0 then
      WriteLn('todos os testes passaram')
    else
      WriteLn(GFalhas, ' falha(s)');
    ExitCode := Ord(GFalhas > 0);
  end;
end.
