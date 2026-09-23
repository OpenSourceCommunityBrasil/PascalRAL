unit RALInst.Registro.Delphi;

{$mode ObjFPC}{$H+}

// F4: o que a IDE do Delphi le do registro ao abrir — pacotes conhecidos,
// library path e variaveis de ambiente. Escrita direta com TRegistry, sem
// ferramenta nenhuma da IDE.
//
// Toda escrita guarda o valor de antes: e o que vai para o recibo, e o que a
// desinstalacao (F10) vai desfazer. Nada e apagado sem ficar anotado.

{$IFNDEF MSWINDOWS}
  {$FATAL RALInst.Registro.Delphi so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils, fpjson, RALInst.IDE, RALInst.Processo;

type
  TAlteracaoRegistro = record
    // relativa a HKCU
    Chave: string;
    Valor: string;
    Existia: boolean;
    Antes: string;
    Depois: string;
    Removido: boolean;
  end;

  { TRegistroDelphi }

  TRegistroDelphi = class
  private
    FIDE: TIDEInstance;
    FChave: string;
    FLog: TLogLinha;
    FSimular: boolean;
    FAlteracoes: array of TAlteracaoRegistro;
    function GetAlteracao(AIndex: integer): TAlteracaoRegistro;
    function GetTotalAlteracoes: integer;
    procedure Logar(const ALinha: string);
    procedure Anotar(const AChave, AValor: string; AExistia: boolean;
      const AAntes, ADepois: string; ARemovido: boolean);
    function Escrever(const ASubchave, AValor, AConteudo: string): boolean;
    function Remover(const ASubchave, AValor: string): boolean;
    procedure ListarValores(const ASubchave: string; ALista: TStrings);
    function MesmaPasta(const A, B: string): boolean;
  public
    constructor Create(AIDE: TIDEInstance);

    // a chave da IDE existe em HKCU: sem ela a IDE nunca foi aberta, e
    // escrever antes disso atrapalha a primeira abertura
    function ChaveExiste: boolean;
    // bds.exe (ou delphi32.exe) desta instalacao rodando: o que for escrito
    // agora a IDE regrava ao fechar, e a instalacao se perde
    function IDEEmExecucao: boolean;

    // 'Library\Win32' do XE2 em diante; 'Library' antes disso (uma plataforma so)
    function ChaveLibrary(const APlataforma: string): string;
    function LerValor(const ASubchave, AValor: string): string;
    // $(BDS), $(BDSCOMMONDIR), $(Platform), as variaveis da propria IDE e as
    // do sistema; o que nao conhece fica como esta
    function Expandir(const ACaminho: string; const APlataforma: string = 'win32'): string;
    // a IDE aceita $(Nome) no library path? (Delphi 7 nao)
    function AceitaVariaveis: boolean;

    // Environment Variables da IDE (nao as do Windows)
    function DefinirVariavel(const ANome, AValor: string): boolean;
    // acrescenta ao fim de uma lista de caminhos ('Search Path', 'Browsing
    // Path'), sem repetir o que ja esta la escrito de outro jeito; devolve
    // quantos entraram, ou -1 em erro
    function AdicionarCaminhos(const APlataforma, AValor: string; ACaminhos: TStrings): integer;
    // pastas da lista que tem unidade com o mesmo nome de uma das nossas e
    // que nao sao nossas: outra copia do RAL, que vai brigar com esta
    procedure CaminhosConflitantes(const APlataforma: string; ANossos,
      AUnidades, AResultado: TStrings);
    // Known Packages; tira o mesmo .bpl de Disabled Packages (a IDE desabilita
    // um pacote que falhou uma vez e nao tenta de novo) e remove registro do
    // mesmo .bpl em outra pasta, que faria a IDE carregar dois
    function RegistrarPacote(const ABpl, ADescricao: string): boolean;

    function AlteracoesJSON: TJSONArray;

    property IDE: TIDEInstance read FIDE;
    // relativa a HKCU; por padrao a da IDE. Os testes apontam para uma copia
    property Chave: string read FChave write FChave;
    property Log: TLogLinha read FLog write FLog;
    // diz o que escreveria, sem escrever
    property Simular: boolean read FSimular write FSimular;
    property TotalAlteracoes: integer read GetTotalAlteracoes;
    property Alteracoes[AIndex: integer]: TAlteracaoRegistro read GetAlteracao;
  end;

// nome da plataforma como o registro do Delphi escreve: win32 -> Win32
function PlataformaRegistro(const APlataforma: string): string;

// desfaz, na ordem inversa, as escritas no registro anotadas num recibo da
// instalacao (o que existia volta ao valor de antes; o que foi criado e
// apagado). Os .bpl gravados nao voltam: isso e a desinstalacao completa (F10)
function DesfazerRecibo(const AArquivo: string; ALog: TLogLinha): boolean;

// copia os valores de texto de uma chave de HKCU (e subchaves) para outra;
// usado pelos testes para trabalhar sobre uma copia do registro da IDE
function CopiarChave(const AOrigem, ADestino: string): boolean;

implementation

uses
  Windows, Registry, StrUtils, jsonparser;

function PlataformaRegistro(const APlataforma: string): string;
const
  Nomes: array[0..8, 0..1] of string = (
    ('win32', 'Win32'), ('win64', 'Win64'), ('win64x', 'Win64x'),
    ('linux64', 'Linux64'), ('osx64', 'OSX64'), ('osxarm64', 'OSXARM64'),
    ('android', 'Android32'), ('android64', 'Android64'),
    ('iosdevice64', 'iOSDevice64')
  );
var
  vInt: integer;
begin
  for vInt := Low(Nomes) to High(Nomes) do
    if SameText(Nomes[vInt, 0], APlataforma) then
      Exit(Nomes[vInt, 1]);
  Result := APlataforma;
end;

function DesfazerRecibo(const AArquivo: string; ALog: TLogLinha): boolean;
var
  vTexto: TStringList;
  vDados: TJSONData;
  vLista: TJSONArray;
  vItem: TJSONObject;
  vReg: TRegistry;
  vInt: integer;
  vChave, vValor: string;

  procedure Logar(const ALinha: string);
  begin
    if Assigned(ALog) then
      ALog(ALinha);
  end;

begin
  Result := True;
  vTexto := TStringList.Create;
  vReg := TRegistry.Create(KEY_READ or KEY_WRITE);
  vDados := nil;
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    vTexto.LoadFromFile(AArquivo);
    vDados := GetJSON(vTexto.Text);
    vLista := TJSONObject(vDados).Get('registro', TJSONArray(nil));
    if vLista = nil then
      Exit;

    for vInt := Pred(vLista.Count) downto 0 do
    begin
      vItem := vLista.Objects[vInt];
      vChave := vItem.Get('chave', '');
      if SameText(Copy(vChave, 1, 4), 'HKCU') then
        Delete(vChave, 1, 4);
      vValor := vItem.Get('valor', '');
      try
        if vItem.Get('existia', False) then
        begin
          if vReg.OpenKey(vChave, True) then
          begin
            vReg.WriteString(vValor, vItem.Get('antes', ''));
            vReg.CloseKey;
            Logar('  restaurado: ' + vChave + '\' + vValor);
          end
          else
          begin
            Logar('ERRO: não abriu HKCU' + vChave);
            Result := False;
          end;
        end
        else if vReg.OpenKey(vChave, False) then
        begin
          if vReg.ValueExists(vValor) then
          begin
            vReg.DeleteValue(vValor);
            Logar('  apagado: ' + vChave + '\' + vValor);
          end;
          vReg.CloseKey;
        end;
      except
        on E: Exception do
        begin
          Logar('ERRO: ' + vChave + '\' + vValor + ': ' + E.Message);
          Result := False;
        end;
      end;
    end;
  finally
    vDados.Free;
    vReg.Free;
    vTexto.Free;
  end;
end;

function CopiarChave(const AOrigem, ADestino: string): boolean;
var
  vOrigem, vDestino: TRegistry;
  vValores, vSubchaves: TStringList;
  vInt: integer;
begin
  Result := False;
  vOrigem := TRegistry.Create(KEY_READ);
  vDestino := TRegistry.Create(KEY_READ or KEY_WRITE);
  vValores := TStringList.Create;
  vSubchaves := TStringList.Create;
  try
    vOrigem.RootKey := HKEY_CURRENT_USER;
    vDestino.RootKey := HKEY_CURRENT_USER;
    if not vOrigem.OpenKeyReadOnly(AOrigem) then
      Exit;
    if not vDestino.OpenKey(ADestino, True) then
      Exit;

    vOrigem.GetValueNames(vValores);
    for vInt := 0 to Pred(vValores.Count) do
      if vOrigem.GetDataType(vValores[vInt]) in [rdString, rdExpandString] then
        vDestino.WriteString(vValores[vInt], vOrigem.ReadString(vValores[vInt]));

    vOrigem.GetKeyNames(vSubchaves);
    vOrigem.CloseKey;
    vDestino.CloseKey;

    Result := True;
    for vInt := 0 to Pred(vSubchaves.Count) do
      Result := CopiarChave(AOrigem + '\' + vSubchaves[vInt],
                            ADestino + '\' + vSubchaves[vInt]) and Result;
  finally
    vSubchaves.Free;
    vValores.Free;
    vDestino.Free;
    vOrigem.Free;
  end;
end;

{ TRegistroDelphi }

constructor TRegistroDelphi.Create(AIDE: TIDEInstance);
begin
  inherited Create;
  FIDE := AIDE;
  FChave := AIDE.RegKey;
end;

function TRegistroDelphi.GetAlteracao(AIndex: integer): TAlteracaoRegistro;
begin
  Result := FAlteracoes[AIndex];
end;

function TRegistroDelphi.GetTotalAlteracoes: integer;
begin
  Result := Length(FAlteracoes);
end;

procedure TRegistroDelphi.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

procedure TRegistroDelphi.Anotar(const AChave, AValor: string; AExistia: boolean;
  const AAntes, ADepois: string; ARemovido: boolean);
var
  vInt: integer;
begin
  vInt := Length(FAlteracoes);
  SetLength(FAlteracoes, vInt + 1);
  FAlteracoes[vInt].Chave := AChave;
  FAlteracoes[vInt].Valor := AValor;
  FAlteracoes[vInt].Existia := AExistia;
  FAlteracoes[vInt].Antes := AAntes;
  FAlteracoes[vInt].Depois := ADepois;
  FAlteracoes[vInt].Removido := ARemovido;
end;

function TRegistroDelphi.ChaveExiste: boolean;
var
  vReg: TRegistry;
begin
  Result := False;
  if FChave = '' then
    Exit;
  vReg := TRegistry.Create(KEY_READ);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    Result := vReg.KeyExists(FChave);
  finally
    vReg.Free;
  end;
end;

function TRegistroDelphi.IDEEmExecucao: boolean;
begin
  // bds.exe/delphi32.exe desta instalacao (<raiz>\bin)
  Result := ProgramaEmExecucao(['bds.exe', 'delphi32.exe'], FIDE.RootDir + 'bin');
end;

function TRegistroDelphi.ChaveLibrary(const APlataforma: string): string;
begin
  // o XE2 (BDS 9.0) trouxe Win64 e passou a separar o library path por
  // plataforma; antes dele so ha Win32, direto em Library
  if (FIDE.BDSVersao <> '') and (CompararVersoes(FIDE.BDSVersao, '9.0') >= 0) then
    Result := 'Library\' + PlataformaRegistro(APlataforma)
  else
    Result := 'Library';
end;

function TRegistroDelphi.AceitaVariaveis: boolean;
begin
  // o Delphi 7 nao tem Environment Variables propria nem $(Nome) no path
  Result := FIDE.BDSVersao <> '';
end;

function TRegistroDelphi.LerValor(const ASubchave, AValor: string): string;
var
  vReg: TRegistry;
begin
  Result := '';
  vReg := TRegistry.Create(KEY_READ);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if vReg.OpenKeyReadOnly(FChave + '\' + ASubchave) then
    begin
      if vReg.ValueExists(AValor) then
        Result := vReg.ReadString(AValor);
      vReg.CloseKey;
    end;
  finally
    vReg.Free;
  end;
end;

procedure TRegistroDelphi.ListarValores(const ASubchave: string; ALista: TStrings);
var
  vReg: TRegistry;
begin
  ALista.Clear;
  vReg := TRegistry.Create(KEY_READ);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if vReg.OpenKeyReadOnly(FChave + '\' + ASubchave) then
    begin
      vReg.GetValueNames(ALista);
      vReg.CloseKey;
    end;
  finally
    vReg.Free;
  end;
end;

function TRegistroDelphi.Escrever(const ASubchave, AValor, AConteudo: string): boolean;
var
  vReg: TRegistry;
  vExistia: boolean;
  vAntes: string;
begin
  Result := False;
  vReg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    vExistia := False;
    vAntes := '';
    if vReg.OpenKeyReadOnly(FChave + '\' + ASubchave) then
    begin
      vExistia := vReg.ValueExists(AValor);
      if vExistia then
        vAntes := vReg.ReadString(AValor);
      vReg.CloseKey;
    end;

    if vExistia and (vAntes = AConteudo) then
      Exit(True);

    Anotar(ASubchave, AValor, vExistia, vAntes, AConteudo, False);
    if FSimular then
      Exit(True);

    try
      if not vReg.OpenKey(FChave + '\' + ASubchave, True) then
      begin
        Logar('ERRO: não foi possível abrir HKCU' + FChave + '\' + ASubchave);
        Exit;
      end;
      vReg.WriteString(AValor, AConteudo);
      vReg.CloseKey;
      Result := True;
    except
      on E: Exception do
        Logar('ERRO: não foi possível gravar ' + ASubchave + '\' + AValor + ': ' + E.Message);
    end;
  finally
    vReg.Free;
  end;
end;

function TRegistroDelphi.Remover(const ASubchave, AValor: string): boolean;
var
  vReg: TRegistry;
begin
  Result := False;
  vReg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if not vReg.OpenKey(FChave + '\' + ASubchave, False) then
      Exit;
    try
      if not vReg.ValueExists(AValor) then
        Exit(True);
      Anotar(ASubchave, AValor, True, vReg.ReadString(AValor), '', True);
      if FSimular then
        Exit(True);
      Result := vReg.DeleteValue(AValor);
    finally
      vReg.CloseKey;
    end;
  finally
    vReg.Free;
  end;
end;

function TRegistroDelphi.Expandir(const ACaminho: string; const APlataforma: string): string;
var
  vIni, vFim: integer;
  vNome, vValor: string;
  vPassos: integer;
begin
  Result := ACaminho;
  vPassos := 0;
  vIni := Pos('$(', Result);
  // limite de passos: variavel que referencia a si mesma nao trava a busca
  while (vIni > 0) and (vPassos < 50) do
  begin
    Inc(vPassos);
    vFim := PosEx(')', Result, vIni);
    if vFim = 0 then
      Break;
    vNome := Copy(Result, vIni + 2, vFim - vIni - 2);

    if SameText(vNome, 'BDS') then
      vValor := ExcludeTrailingPathDelimiter(FIDE.RootDir)
    else if SameText(vNome, 'BDSBIN') then
      vValor := FIDE.RootDir + 'bin'
    else if SameText(vNome, 'BDSLIB') then
      vValor := FIDE.RootDir + 'lib'
    else if SameText(vNome, 'BDSINCLUDE') then
      vValor := FIDE.RootDir + 'include'
    else if SameText(vNome, 'BDSCOMMONDIR') then
      vValor := ExcludeTrailingPathDelimiter(FIDE.CommonDir)
    else if SameText(vNome, 'Platform') then
      vValor := PlataformaRegistro(APlataforma)
    else if SameText(vNome, 'DELPHI') then
      vValor := ExcludeTrailingPathDelimiter(FIDE.RootDir)
    else
    begin
      vValor := LerValor('Environment Variables', vNome);
      if vValor = '' then
        vValor := SysUtils.GetEnvironmentVariable(vNome);
    end;

    if vValor = '' then
      vIni := PosEx('$(', Result, vFim)
    else
    begin
      Result := Copy(Result, 1, vIni - 1) + vValor + Copy(Result, vFim + 1, MaxInt);
      vIni := Pos('$(', Result);
    end;
  end;
end;

function TRegistroDelphi.MesmaPasta(const A, B: string): boolean;
begin
  Result := SameText(ExcludeTrailingPathDelimiter(ExpandFileName(Trim(A))),
                     ExcludeTrailingPathDelimiter(ExpandFileName(Trim(B))));
end;

function TRegistroDelphi.DefinirVariavel(const ANome, AValor: string): boolean;
var
  vAntes: string;
begin
  vAntes := LerValor('Environment Variables', ANome);
  Result := Escrever('Environment Variables', ANome, AValor);
  if Result and (vAntes <> AValor) then
  begin
    if vAntes = '' then
      Logar(Format('  variável $(%s) = %s', [ANome, AValor]))
    else
      Logar(Format('  variável $(%s) = %s  (era %s)', [ANome, AValor, vAntes]));
  end;
end;

function TRegistroDelphi.AdicionarCaminhos(const APlataforma, AValor: string;
  ACaminhos: TStrings): integer;
var
  vAtual, vNovo: string;
  vItens, vExpandidos: TStringList;
  vInt: integer;
  vExpandido: string;
begin
  Result := 0;
  vAtual := LerValor(ChaveLibrary(APlataforma), AValor);

  vItens := TStringList.Create;
  vExpandidos := TStringList.Create;
  try
    vItens.StrictDelimiter := True;
    vItens.Delimiter := ';';
    vItens.DelimitedText := vAtual;
    vExpandidos.CaseSensitive := False;
    for vInt := 0 to Pred(vItens.Count) do
      if Trim(vItens[vInt]) <> '' then
        vExpandidos.Add(LowerCase(ExcludeTrailingPathDelimiter(
                          Expandir(Trim(vItens[vInt]), APlataforma))));

    vNovo := vAtual;
    for vInt := 0 to Pred(ACaminhos.Count) do
    begin
      // o que ja esta la, escrito com variavel ou por extenso, nao repete
      vExpandido := LowerCase(ExcludeTrailingPathDelimiter(Expandir(ACaminhos[vInt], APlataforma)));
      if vExpandidos.IndexOf(vExpandido) >= 0 then
        Continue;
      vExpandidos.Add(vExpandido);
      if (vNovo <> '') and (vNovo[Length(vNovo)] <> ';') then
        vNovo := vNovo + ';';
      vNovo := vNovo + ACaminhos[vInt];
      Logar(Format('  %s %s += %s', [PlataformaRegistro(APlataforma), AValor, ACaminhos[vInt]]));
      Inc(Result);
    end;

    if Result > 0 then
      if not Escrever(ChaveLibrary(APlataforma), AValor, vNovo) then
        Result := -1;
  finally
    vExpandidos.Free;
    vItens.Free;
  end;
end;

procedure TRegistroDelphi.CaminhosConflitantes(const APlataforma: string;
  ANossos, AUnidades, AResultado: TStrings);
var
  vItens, vNossos: TStringList;
  vInt, vUni: integer;
  vPasta: string;
begin
  AResultado.Clear;
  vItens := TStringList.Create;
  vNossos := TStringList.Create;
  try
    vNossos.CaseSensitive := False;
    for vInt := 0 to Pred(ANossos.Count) do
      vNossos.Add(ExcludeTrailingPathDelimiter(Expandir(ANossos[vInt], APlataforma)));

    vItens.StrictDelimiter := True;
    vItens.Delimiter := ';';
    vItens.DelimitedText := LerValor(ChaveLibrary(APlataforma), 'Search Path');
    for vInt := 0 to Pred(vItens.Count) do
    begin
      if Trim(vItens[vInt]) = '' then
        Continue;
      vPasta := ExcludeTrailingPathDelimiter(Expandir(Trim(vItens[vInt]), APlataforma));
      if (Pos('$(', vPasta) > 0) or (vNossos.IndexOf(vPasta) >= 0) then
        Continue;
      for vUni := 0 to Pred(AUnidades.Count) do
        if FileExists(IncludeTrailingPathDelimiter(vPasta) + AUnidades[vUni]) then
        begin
          AResultado.Add(vItens[vInt] + '  (' + AUnidades[vUni] + ')');
          Break;
        end;
    end;
  finally
    vNossos.Free;
    vItens.Free;
  end;
end;

function TRegistroDelphi.RegistrarPacote(const ABpl, ADescricao: string): boolean;
var
  vValores: TStringList;
  vInt, vAntes: integer;
  vNomeBpl: string;
begin
  vNomeBpl := ExtractFileName(ABpl);
  vValores := TStringList.Create;
  try
    // o mesmo .bpl registrado de outra pasta: a IDE carregaria os dois e
    // recusaria o segundo por unidade duplicada
    ListarValores('Known Packages', vValores);
    for vInt := 0 to Pred(vValores.Count) do
      if SameText(ExtractFileName(Expandir(vValores[vInt])), vNomeBpl) and
         not MesmaPasta(Expandir(vValores[vInt]), ABpl) then
      begin
        Logar('  Known Packages -= ' + vValores[vInt] + '  (mesmo pacote em outra pasta)');
        Remover('Known Packages', vValores[vInt]);
      end;

    // pacote que falhou ao carregar uma vez fica desabilitado para sempre,
    // mesmo depois de corrigido
    ListarValores('Disabled Packages', vValores);
    for vInt := 0 to Pred(vValores.Count) do
      if SameText(ExtractFileName(Expandir(vValores[vInt])), vNomeBpl) then
      begin
        Logar('  Disabled Packages -= ' + vValores[vInt]);
        Remover('Disabled Packages', vValores[vInt]);
      end;
  finally
    vValores.Free;
  end;

  vAntes := Length(FAlteracoes);
  Result := Escrever('Known Packages', ABpl, ADescricao);
  if Result and (Length(FAlteracoes) > vAntes) then
    Logar('  Known Packages += ' + ABpl)
  else if Result then
    Logar('  Known Packages: ' + vNomeBpl + ' já registrado');
end;

function TRegistroDelphi.AlteracoesJSON: TJSONArray;
var
  vInt: integer;
  vObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  for vInt := 0 to High(FAlteracoes) do
  begin
    vObj := TJSONObject.Create;
    vObj.Add('chave', 'HKCU' + FChave + '\' + FAlteracoes[vInt].Chave);
    vObj.Add('valor', FAlteracoes[vInt].Valor);
    vObj.Add('existia', FAlteracoes[vInt].Existia);
    if FAlteracoes[vInt].Existia then
      vObj.Add('antes', FAlteracoes[vInt].Antes);
    if FAlteracoes[vInt].Removido then
      vObj.Add('removido', True)
    else
      vObj.Add('depois', FAlteracoes[vInt].Depois);
    Result.Add(vObj);
  end;
end;

end.
