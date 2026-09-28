/// What a run changes in an IDE that may already have RAL: the sources the IDE
/// uses against the ones the run installs.
/// - The sources the IDE uses come from the newest installer receipt of that
///   IDE, while it matches what the IDE points to ($(PascalRAL) in Delphi, the
///   pascalral.lpk link in Lazarus); without such a receipt, from the folder
///   the IDE points to (a hand installation).
/// - The same sources are the same folder with the same commit (a GitHub
///   version) or, for a local folder, no source file changed since the
///   installation. With the same sources the packages decide: the same ones
///   are nothing to do, others only enter or leave.
/// - Other sources are an update, a downgrade (the version number says which)
///   or a switch (another branch, another folder, a hand installation): the
///   packages are compiled again.
unit RALInst.Situacao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Existente;

type
  /// What a run does in one IDE, compared with what the IDE already has.
  TMudancaRAL = (
    /// the IDE has no RAL
    mrInstalar,
    /// the same sources and packages: nothing to do
    mrNada,
    /// the same sources: only packages enter or leave
    mrModificar,
    /// a newer version, or a new commit of the same branch
    mrAtualizar,
    /// an older version
    mrVoltar,
    /// other sources: another branch, another folder, a local folder or a hand
    /// installation of unknown version
    mrTrocar,
    /// the same local folder, with sources changed since the installation
    mrRecompilar,
    /// the same, done again on request
    mrReinstalar
  );

  /// A set of changes (the screens count the IDEs by kind of change).
  TMudancasRAL = set of TMudancaRAL;

  /// RAL sources: those an IDE uses or those a run installs.
  TFontesRAL = record
    Commit: string;
    /// When the installer put them in the IDE (0 when unknown)
    Data: TDateTime;
    /// The installer put them there (a receipt matches the IDE)
    DoInstalador: boolean;
    /// Something of RAL is in the IDE
    Existe: boolean;
    /// Root of the sources, with the trailing delimiter ('' when unknown)
    Pasta: string;
    /// As GitHub writes it ('1.1', 'dev'); '' for a local folder
    Versao: string;
  end;

/// The results and files the installer left in the IDE from exactly these
/// sources: the IDE's receipts from newest to oldest, while they are from
/// them. AResultados: 'win32 IndyRAL=ok'; AArquivos: file=size|date, the file
/// as ChaveArquivo writes it
procedure CompiladoDasFontes(const APastaRecibos, ARaizIDE: string;
  const AFontes: TFontesRAL; AResultados, AArquivos: TStrings);
/// Newest change of a source file (.pas, .inc, .lpk, .dpk...) under src and
/// pkg of the folder; 0 when there is none
function DataDosFontes(const APasta: string): TDateTime;
/// '1.1 (abc1234)', 'da pasta D:\RAL', 'instalado à mão'
function DescreverFontes(const AFontes: TFontesRAL): string;
/// One line, for the plan and the summary: 'Atualizar o RAL 1.1 -> dev'
function DescreverMudanca(AMudanca: TMudancaRAL; const AAtual,
  ANova: TFontesRAL): string;
/// The sources an IDE uses: the newest receipt of the IDE while it matches
/// what the IDE points to, else the folder the IDE points to. AExistente may
/// be nil (no RAL)
function FontesDaIDE(const ARaizIDE: string; AExistente: TInstalacaoExistente;
  const APastaRecibos: string): TFontesRAL;
/// The sources a run installs from a folder: the given version and commit, or
/// those of the installer's mark in the folder (a local folder has none)
function FontesDaPasta(const APasta, AVersao, ACommit: string): TFontesRAL;
/// Is the file still the one noted in AArquivos (size|date)?
function IgualAoCompilado(const AArquivo: string; AArquivos: TStrings): boolean;
/// Compiles the packages again: every change but nothing and modify
function MudancaCompleta(AMudanca: TMudancaRAL): boolean;
/// How the sources change; mrNada is the same sources (the packages decide
/// between nothing and modify)
function MudancaDeFontes(const AAtual, ANova: TFontesRAL): TMudancaRAL;
/// The same folder, however written
function MesmaPasta(const A, B: string): boolean;

implementation

uses
  StrUtils,
  RALInst.Fontes, RALInst.GitHub, RALInst.Mensagens, RALInst.Recibos;

const
  // o que muda o que o compilador gera; .res e .rc o proprio instalador grava
  ExtensoesFonte: array[0..9] of string = ('.pas', '.pp', '.inc', '.lpk', '.dpk',
    '.dproj', '.lfm', '.dfm', '.lrs', '.dcr');

function MesmaPasta(const A, B: string): boolean;
begin
  Result := (A <> '') and (B <> '') and
            SameFileName(IncludeTrailingPathDelimiter(ExpandFileName(A)),
                         IncludeTrailingPathDelimiter(ExpandFileName(B)));
end;

function VersaoNumerica(const AVersao: string): boolean;
var
  vTexto: string;
begin
  vTexto := AVersao;
  if (vTexto <> '') and (vTexto[1] in ['v', 'V']) then
    Delete(vTexto, 1, 1);
  Result := (vTexto <> '') and (vTexto[1] in ['0'..'9']);
end;

function FontesDaPasta(const APasta, AVersao, ACommit: string): TFontesRAL;
var
  vRepo, vVersao, vCommit: string;
begin
  Result := Default(TFontesRAL);
  if APasta <> '' then
    Result.Pasta := IncludeTrailingPathDelimiter(APasta);
  Result.Versao := AVersao;
  Result.Commit := ACommit;
  // sem versao dada: a marca da pasta diz de onde ela veio
  if (AVersao = '') and (ACommit = '') and (APasta <> '') then
  begin
    LerMarca(APasta, vRepo, vVersao, vCommit);
    Result.Versao := vVersao;
    Result.Commit := vCommit;
  end;
end;

function FontesDaIDE(const ARaizIDE: string; AExistente: TInstalacaoExistente;
  const APastaRecibos: string): TFontesRAL;
var
  vRecibos: TRecibos;
  vLista: TList;
  vRecibo: TRecibo;
  vRepo, vVersao, vCommit: string;
begin
  Result := Default(TFontesRAL);
  if (AExistente = nil) or not AExistente.Existe then
    Exit;
  Result.Existe := True;
  vRecibos := TRecibos.Create(True);
  vLista := TList.Create;
  try
    vRecibos.Carregar(APastaRecibos);
    vRecibos.DaIDE(ARaizIDE, vLista);
    // o recibo mais novo so vale enquanto a IDE aponta para os fontes dele:
    // quem trocou a pasta a mao depois tem outra instalacao
    if vLista.Count > 0 then
    begin
      vRecibo := TRecibo(vLista[0]);
      if (AExistente.Fontes = '') or MesmaPasta(vRecibo.Fontes, AExistente.Fontes) then
      begin
        Result.DoInstalador := True;
        Result.Pasta := IncludeTrailingPathDelimiter(vRecibo.Fontes);
        Result.Versao := vRecibo.VersaoRAL;
        Result.Commit := vRecibo.CommitRAL;
        Result.Data := DataDoRecibo(vRecibo.Data);
        Exit;
      end;
    end;
  finally
    vLista.Free;
    vRecibos.Free;
  end;
  // instalado a mao (ou por um recibo que ja nao vale): a pasta que a IDE
  // aponta, e a marca dela quando a pasta e do instalador
  if AExistente.Fontes <> '' then
  begin
    Result.Pasta := IncludeTrailingPathDelimiter(AExistente.Fontes);
    LerMarca(Result.Pasta, vRepo, vVersao, vCommit);
    Result.Versao := vVersao;
    Result.Commit := vCommit;
  end;
end;

procedure DataMaisNova(const APasta: string; var AData: TDateTime);
var
  vBusca: TSearchRec;
  vPasta, vNome: string;
begin
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vPasta + AllFilesMask, faAnyFile, vBusca) = 0 then
  try
    repeat
      vNome := vBusca.Name;
      if (vNome = '.') or (vNome = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
      begin
        // o que o compilador e o editor gravam dentro dos fontes
        if (vNome[1] = '.') or (Copy(vNome, 1, 2) = '__') or
           (AnsiIndexText(vNome, ['lib', 'compiled', 'backup']) >= 0) then
          Continue;
        DataMaisNova(vPasta + vNome, AData);
      end
      else if (AnsiIndexText(ExtractFileExt(vNome), ExtensoesFonte) >= 0) and
              (vBusca.TimeStamp > AData) then
        AData := vBusca.TimeStamp;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

function DataDosFontes(const APasta: string): TDateTime;
begin
  Result := 0;
  if APasta = '' then
    Exit;
  DataMaisNova(IncludeTrailingPathDelimiter(APasta) + 'src', Result);
  DataMaisNova(IncludeTrailingPathDelimiter(APasta) + 'pkg', Result);
end;

function MudancaDeFontes(const AAtual, ANova: TFontesRAL): TMudancaRAL;
var
  vComparacao: integer;
begin
  if not AAtual.Existe then
    Exit(mrInstalar);
  if AAtual.Pasta = '' then
    Exit(mrTrocar);

  if MesmaPasta(AAtual.Pasta, ANova.Pasta) then
  begin
    // da mesma pasta, mas a mao: nao ha como saber com o que foi compilado
    if not AAtual.DoInstalador then
      Exit(mrTrocar);
    // versao do GitHub: o commit diz se os fontes sao os mesmos
    if ANova.Commit <> '' then
    begin
      if SameText(AAtual.Commit, ANova.Commit) then
        Exit(mrNada);
      if AAtual.Commit = '' then
        Exit(mrTrocar);
      // a mesma pasta com outro commit: o ramo andou
      Exit(mrAtualizar);
    end;
    // pasta local: os mesmos enquanto nenhum fonte mudou depois da instalacao
    if (AAtual.Data > 0) and (DataDosFontes(ANova.Pasta) <= AAtual.Data) then
      Exit(mrNada);
    Exit(mrRecompilar);
  end;

  if (AAtual.Versao <> '') and (ANova.Versao <> '') then
  begin
    if SameText(AAtual.Versao, ANova.Versao) then
    begin
      // a mesma versao em outra pasta
      if (ANova.Commit <> '') and SameText(AAtual.Commit, ANova.Commit) then
        Exit(mrTrocar);
      Exit(mrAtualizar);
    end;
    if VersaoNumerica(AAtual.Versao) and VersaoNumerica(ANova.Versao) then
    begin
      vComparacao := CompararTags(ANova.Versao, AAtual.Versao);
      if vComparacao > 0 then
        Exit(mrAtualizar);
      if vComparacao < 0 then
        Exit(mrVoltar);
    end;
  end;
  Result := mrTrocar;
end;

function MudancaCompleta(AMudanca: TMudancaRAL): boolean;
begin
  Result := not (AMudanca in [mrNada, mrModificar]);
end;

function DescreverFontes(const AFontes: TFontesRAL): string;
begin
  if AFontes.Versao <> '' then
  begin
    Result := AFontes.Versao;
    if AFontes.Commit <> '' then
      Result := Result + ' (' + Copy(AFontes.Commit, 1, 7) + ')';
  end
  else if AFontes.Pasta <> '' then
    Result := Format(cmFontesDaPasta, [ExcludeTrailingPathDelimiter(AFontes.Pasta)])
  else
    Result := cmFontesAMao;
end;

function DescreverMudanca(AMudanca: TMudancaRAL; const AAtual,
  ANova: TFontesRAL): string;
begin
  Result := '';
  case AMudanca of
    mrInstalar:
      Result := Format(cmMudancaInstalar, [DescreverFontes(ANova)]);
    mrNada:
      Result := Format(cmMudancaNada, [DescreverFontes(ANova)]);
    mrModificar:
      Result := Format(cmMudancaModificar, [DescreverFontes(ANova)]);
    mrAtualizar:
      Result := Format(cmMudancaAtualizar, [DescreverFontes(AAtual),
                                            DescreverFontes(ANova)]);
    mrVoltar:
      Result := Format(cmMudancaVoltar, [DescreverFontes(AAtual),
                                         DescreverFontes(ANova)]);
    mrTrocar:
      Result := Format(cmMudancaTrocar, [DescreverFontes(AAtual),
                                         DescreverFontes(ANova)]);
    mrRecompilar:
      Result := Format(cmMudancaRecompilar, [DescreverFontes(ANova)]);
    mrReinstalar:
      Result := Format(cmMudancaReinstalar, [DescreverFontes(ANova)]);
  end;
end;

procedure CompiladoDasFontes(const APastaRecibos, ARaizIDE: string;
  const AFontes: TFontesRAL; AResultados, AArquivos: TStrings);
var
  vRecibos: TRecibos;
  vLista: TList;
  vRecibo: TRecibo;
  vInt, vItem: integer;
  vDataFontes: TDateTime;
begin
  AResultados.Clear;
  AArquivos.Clear;
  vRecibos := TRecibos.Create(True);
  vLista := TList.Create;
  try
    vRecibos.Carregar(APastaRecibos);
    vRecibos.DaIDE(ARaizIDE, vLista);
    vDataFontes := -1;
    // do mais novo ao mais velho: um recibo de outros fontes gravou por cima
    // do que os anteriores compilaram, e a busca para nele
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vRecibo := TRecibo(vLista[vInt]);
      if not MesmaPasta(vRecibo.Fontes, AFontes.Pasta) then
        Break;
      if AFontes.Commit <> '' then
      begin
        if not SameText(vRecibo.CommitRAL, AFontes.Commit) then
          Break;
      end
      else
      begin
        // pasta local: vale o que foi compilado depois da ultima mudanca
        if vDataFontes < 0 then
          vDataFontes := DataDosFontes(AFontes.Pasta);
        if DataDoRecibo(vRecibo.Data) < vDataFontes then
          Break;
      end;
      // o mais novo vale: o que ja esta na lista nao e trocado
      for vItem := 0 to Pred(vRecibo.Resultados.Count) do
        if AResultados.IndexOfName(vRecibo.Resultados.Names[vItem]) < 0 then
          AResultados.Add(vRecibo.Resultados[vItem]);
      for vItem := 0 to Pred(vRecibo.Arquivos.Count) do
        if AArquivos.IndexOfName(vRecibo.Arquivos.Names[vItem]) < 0 then
          AArquivos.Add(vRecibo.Arquivos[vItem]);
    end;
  finally
    vLista.Free;
    vRecibos.Free;
  end;
end;

function IgualAoCompilado(const AArquivo: string; AArquivos: TStrings): boolean;
var
  vInfo: string;
  vPos: integer;
begin
  vInfo := AArquivos.Values[ChaveArquivo(AArquivo)];
  vPos := Pos('|', vInfo);
  Result := (vPos > 0) and
            ArquivoComoNoRecibo(AArquivo, StrToInt64Def(Copy(vInfo, 1, vPos - 1), -1),
                                Copy(vInfo, vPos + 1, MaxInt));
end;

end.
