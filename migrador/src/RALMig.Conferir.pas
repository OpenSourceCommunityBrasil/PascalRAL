/// "Compile to check" (stage M6, optional): compiles the migrated project in a
/// temporary folder with the installer's tools, against the sources of the
/// target RAL, and returns the first error of each file. Nothing is registered
/// and nothing of the real IDE is touched:
/// - Delphi: dcc32 --no-config, with the IDE's own lib folder and every source
///   folder of the RAL on the search path, no runtime packages (the units are
///   linked into the executable); the registry is never read or written;
/// - Lazarus: lazbuild with --primary-config-path pointing at a COPY of the
///   configuration (the copy gets the links of the RAL packages of the target
///   sources); the real configuration is only read.
/// The project is copied to a temporary folder and compiled there; the folder
/// is deleted at the end.
unit RALMig.Conferir;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  RALInst.IDE, RALInst.Processo, RALMig.Tipos;

type
  /// An IDE that can compile a project against a RAL: the IDE, the root of the
  /// RAL sources it should use and where that came from
  TIDEConferencia = class
  public
    /// Owned
    IDE: TIDEInstance;
    /// Root of the RAL sources (the folder that holds src/ and pkg/)
    RaizRAL: string;
    /// 'recibo <file>' or 'manual'
    Origem: string;
    destructor Destroy; override;
    /// 'Delphi 13 Florence, RAL em D:\...'
    function Descricao: string;
  end;

  /// The candidates, owned by the list
  TIDEsConferencia = class(TObjectList)
  private
    function GetItem(AIndex: integer): TIDEConferencia;
  public
    property Items[AIndex: integer]: TIDEConferencia read GetItem; default;
  end;

/// The IDEs of this machine whose newest installer receipt says they have the
/// RAL of that version (1.1, 1.2, 2.0...), with the sources the receipt points
/// to. APastaRecibos: '' = the installer's folder. Candidates are added to
/// ALista.
procedure IDEsComRAL(const AVersao: string; ALista: TIDEsConferencia;
  const APastaRecibos: string = '');
/// Adds an IDE given by hand (no receipt): the root of the IDE and the root of
/// the RAL sources. Returns nil (nothing added) when ARaizIDE is not an IDE or
/// ARaizRAL has no src folder.
function AdicionarIDEManual(ALista: TIDEsConferencia; const ARaizIDE,
  ARaizRAL: string): TIDEConferencia;

/// Copies the project's folder (APastaCopia, or the folder of the project file)
/// to a temporary folder, compiles AProjeto there and deletes the folder.
/// AProjeto: .dpr/.dproj (Delphi) or .lpi (Lazarus), according to the IDE.
/// APrimeiroErro: one line 'file(line): message' for the first error of each
/// file ('' when it compiled). ALog receives the compiler's output.
function CompilarProjeto(AIDE: TIDEInstance; const AProjeto, ARaizRAL: string;
  out APrimeiroErro: string; ALog: TLogLinha = nil;
  const APastaCopia: string = ''): boolean;
/// Same for the result still in memory: the folder of the set is copied and
/// the migrated text of every changed or new file replaces its copy. AProjeto
/// is a path inside the folder of the set.
function CompilarConjunto(AIDE: TIDEInstance; AConjunto: TConjuntoArquivos;
  const AProjeto, ARaizRAL: string; out APrimeiroErro: string;
  ALog: TLogLinha = nil): boolean;

/// 'file(line): message' of the first error of each file in the compiler output
/// (Delphi and FPC share the format); one line per file
function PrimeiroErroPorArquivo(ASaida: TStrings): string;

implementation

uses
  StrUtils, {$IFDEF MSWINDOWS} RALInst.IDE.Delphi, {$ENDIF} RALInst.IDE.Lazarus, RALInst.Recibos,
  RALInst.Promocao, RALInst.Fontes, RALInst.Config.Lazarus,
  {$IFDEF MSWINDOWS} RALInst.Build.Delphi, {$ENDIF}
  RALMig.Regras;

resourcestring
  emConfProjetoAusente = 'O projeto não existe: %s';
  emConfTipoErrado = 'O projeto %s não é de uma IDE %s.';
  emConfSemCompilador = 'Compilador da IDE não encontrado: %s';
  emConfSemCopia = 'Não foi possível criar a pasta temporária %s.';
  emConfSemFonte = 'Não achei o arquivo principal do projeto (.dpr) ao lado de %s.';
  emConfSemRAL = 'Os fontes do RAL não estão em %s (falta a pasta src).';
  emConfSaida = 'O compilador terminou com código %d e não disse o erro.';
  emConfLinks = 'lazbuild não registrou os pacotes do RAL na cópia da configuração: %s';
  {$IFNDEF MSWINDOWS}
  emConfSoWindows = 'O Delphi só compila no Windows.';
  {$ENDIF}
  cmConfOrigemManual = 'manual';
  cmConfOrigemRecibo = 'recibo %s';
  cmConfDescricao = '%s, RAL em %s';

const
  /// Folders of a project that are output or history, never copied
  PASTAS_FORA: array[0..7] of string = (
    '.git', '__history', '__recovery', '_ral-migracao', 'Win32', 'Win64', '.vs',
    'backup');
  /// The 32 bytes of an empty, valid .res
  RES_VAZIO: array[0..31] of byte = (
    0, 0, 0, 0, 32, 0, 0, 0, 255, 255, 0, 0, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0);

{ TIDEConferencia }

destructor TIDEConferencia.Destroy;
begin
  IDE.Free;
  inherited Destroy;
end;

function TIDEConferencia.Descricao: string;
begin
  Result := Format(cmConfDescricao, [IDE.Nome, RaizRAL]);
end;

{ TIDEsConferencia }

function TIDEsConferencia.GetItem(AIndex: integer): TIDEConferencia;
begin
  Result := TIDEConferencia(inherited Items[AIndex]);
end;

{ Candidatas }

function InspecionarIDE(const ATipo, ARaiz: string): TIDEInstance;
var
  vBusca: TBuscaIDE;
begin
  Result := nil;
  {$IFDEF MSWINDOWS}
  if SameText(ATipo, 'delphi') then
    vBusca := TBuscaDelphi.Create
  else
  {$ENDIF}
  if SameText(ATipo, 'lazarus') then
    vBusca := TBuscaLazarus.Create
  else
    Exit;
  try
    Result := vBusca.InspecionarPasta(ARaiz);
  finally
    vBusca.Free;
  end;
end;

function RaizJaNaLista(ALista: TIDEsConferencia; const ARaiz: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 0 to ALista.Count - 1 do
    if SameFileName(ALista[vInt].IDE.RootDir, IncludeTrailingPathDelimiter(ARaiz)) then
      Exit(True);
end;

function NovaCandidata(ALista: TIDEsConferencia; AIDE: TIDEInstance;
  const ARaizRAL, AOrigem: string): TIDEConferencia;
begin
  Result := TIDEConferencia.Create;
  Result.IDE := AIDE;
  Result.RaizRAL := IncludeTrailingPathDelimiter(ARaizRAL);
  Result.Origem := AOrigem;
  ALista.Add(Result);
end;

procedure IDEsComRAL(const AVersao: string; ALista: TIDEsConferencia;
  const APastaRecibos: string);
var
  vRecibos: TRecibos;
  vVistas: TStringList;
  vInt: integer;
  vRecibo: TRecibo;
  vIDE: TIDEInstance;
begin
  vRecibos := TRecibos.Create;
  vVistas := TStringList.Create;
  vVistas.CaseSensitive := False;
  try
    if APastaRecibos = '' then
      vRecibos.Carregar(PastaDadosInstalador + 'recibos')
    else
      vRecibos.Carregar(APastaRecibos);
    // newest first: the first receipt of an IDE says what it has now
    for vInt := 0 to vRecibos.Count - 1 do
    begin
      vRecibo := vRecibos[vInt];
      if vVistas.IndexOf(vRecibo.IDERaiz) >= 0 then
        Continue;
      vVistas.Add(vRecibo.IDERaiz);
      if (NormalizarVersao(vRecibo.VersaoRAL) <> NormalizarVersao(AVersao)) or
         not DirectoryExists(IncludeTrailingPathDelimiter(vRecibo.Fontes) + 'src') or
         RaizJaNaLista(ALista, vRecibo.IDERaiz) then
        Continue;
      vIDE := InspecionarIDE(vRecibo.Tipo, vRecibo.IDERaiz);
      if vIDE <> nil then
        NovaCandidata(ALista, vIDE, vRecibo.Fontes,
          Format(cmConfOrigemRecibo, [ExtractFileName(vRecibo.Arquivo)]));
    end;
  finally
    vVistas.Free;
    vRecibos.Free;
  end;
end;

function AdicionarIDEManual(ALista: TIDEsConferencia; const ARaizIDE,
  ARaizRAL: string): TIDEConferencia;
var
  vIDE: TIDEInstance;
begin
  Result := nil;
  if not DirectoryExists(IncludeTrailingPathDelimiter(ARaizRAL) + 'src') then
    Exit;
  vIDE := nil;
  if DirectoryExists(IncludeTrailingPathDelimiter(ARaizIDE) + 'bin') and
     FileExists(IncludeTrailingPathDelimiter(ARaizIDE) + 'bin' + PathDelim + 'dcc32.exe') then
    vIDE := InspecionarIDE('delphi', ARaizIDE);
  if vIDE = nil then
    vIDE := InspecionarIDE('lazarus', ARaizIDE);
  // fpcupdeluxe: the root given holds lazarus\ and fpc\
  if vIDE = nil then
    vIDE := InspecionarIDE('lazarus', IncludeTrailingPathDelimiter(ARaizIDE) + 'lazarus');
  if vIDE <> nil then
    Result := NovaCandidata(ALista, vIDE, ARaizRAL, cmConfOrigemManual);
end;

{ Erros }

/// The file of a compiler line 'path\x.pas(12,3) Error: ...' and the line
function ArquivoELinha(const ATrecho: string; out ALinha: string): string;
var
  vAbre: integer;
begin
  ALinha := '';
  vAbre := RPos('(', ATrecho);
  if (vAbre > 0) and (ATrecho[Length(ATrecho)] = ')') then
  begin
    ALinha := Copy(ATrecho, vAbre + 1, Length(ATrecho) - vAbre - 1);
    if Pos(',', ALinha) > 0 then
      ALinha := Copy(ALinha, 1, Pos(',', ALinha) - 1);
    Result := ExtractFileName(Trim(Copy(ATrecho, 1, vAbre - 1)));
  end
  else
    Result := '';
end;

function MarcaDeErro(const ALinha: string; out ATexto: string): string;
var
  vMarca: string;
  vPos: integer;
begin
  Result := '';
  ATexto := '';
  for vMarca in TStringArray.Create(' Error: ', ' Fatal: ') do
  begin
    vPos := Pos(vMarca, ALinha);
    if vPos > 0 then
    begin
      ATexto := Trim(Copy(ALinha, vPos + Length(vMarca), MaxInt));
      Exit(Trim(Copy(ALinha, 1, vPos - 1)));
    end;
  end;
  if (Copy(ALinha, 1, 7) = 'Error: ') or (Copy(ALinha, 1, 7) = 'Fatal: ') then
    ATexto := Trim(Copy(ALinha, 8, MaxInt));
end;

function PrimeiroErroPorArquivo(ASaida: TStrings): string;
var
  vInt: integer;
  vVistos: TStringList;
  vTrecho, vTexto, vArquivo, vLinha: string;
begin
  Result := '';
  vVistos := TStringList.Create;
  vVistos.CaseSensitive := False;
  try
    for vInt := 0 to ASaida.Count - 1 do
    begin
      vTrecho := MarcaDeErro(ASaida[vInt], vTexto);
      if vTexto = '' then
        Continue;
      vArquivo := ArquivoELinha(vTrecho, vLinha);
      if vVistos.IndexOf(vArquivo) >= 0 then
        Continue;
      vVistos.Add(vArquivo);
      if Result <> '' then
        Result := Result + LineEnding;
      if vArquivo = '' then
        Result := Result + vTexto
      else
        Result := Result + vArquivo + '(' + vLinha + '): ' + vTexto;
    end;
  finally
    vVistos.Free;
  end;
end;

{ Pasta temporaria }

var
  GSequencia: integer = 0;

function PastaTemporaria: string;
begin
  Inc(GSequencia);
  Result := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-conferir-' +
            IntToStr(GetProcessID) + '-' + IntToStr(GSequencia) + PathDelim;
end;

function PastaIgnorada(const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := Low(PASTAS_FORA) to High(PASTAS_FORA) do
    if SameText(ANome, PASTAS_FORA[vInt]) then
      Exit(True);
end;

procedure CopiarArvore(const ADe, APara: string);
var
  vBusca: TSearchRec;
begin
  ForceDirectories(APara);
  if FindFirst(ADe + AllFilesMask, faAnyFile, vBusca) <> 0 then
    Exit;
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
      begin
        if not PastaIgnorada(vBusca.Name) then
          CopiarArvore(ADe + vBusca.Name + PathDelim, APara + vBusca.Name + PathDelim);
      end
      else
        CopiarArquivo(ADe + vBusca.Name, APara + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

/// Lists the folders under ARaiz (not hidden, not design-time) with a trailing
/// delimiter dropped
procedure ListarPastas(const ARaiz: string; ALista: TStrings);
var
  vBusca: TSearchRec;
begin
  ALista.Add(ExcludeTrailingPathDelimiter(ARaiz));
  if FindFirst(ARaiz + AllFilesMask, faDirectory, vBusca) <> 0 then
    Exit;
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') or (vBusca.Name[1] = '.') or
         ((vBusca.Attr and faDirectory) = 0) or SameText(vBusca.Name, 'design') then
        Continue;
      ListarPastas(ARaiz + vBusca.Name + PathDelim, ALista);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

function Juntar(ALista: TStrings): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to ALista.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + ALista[vInt];
  end;
end;

procedure GravarBytes(const ACaminho: string; const ABytes; ATamanho: integer);
var
  vFluxo: TFileStream;
begin
  vFluxo := TFileStream.Create(ACaminho, fmCreate);
  try
    vFluxo.WriteBuffer(ABytes, ATamanho);
  finally
    vFluxo.Free;
  end;
end;

{ Delphi }

{$IFDEF MSWINDOWS}

/// The text of the elements <ANome> of a .dproj (all property groups)
procedure ElementosDoDproj(const ADproj, ANome: string; ALista: TStrings);
var
  vTexto: TStringList;
  vConteudo, vAbre, vFecha: string;
  vIni, vFim: integer;
begin
  if not FileExists(ADproj) then
    Exit;
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(ADproj);
    vConteudo := vTexto.Text;
  finally
    vTexto.Free;
  end;
  vAbre := '<' + ANome + '>';
  vFecha := '</' + ANome + '>';
  vIni := Pos(vAbre, vConteudo);
  while vIni > 0 do
  begin
    vFim := PosEx(vFecha, vConteudo, vIni);
    if vFim = 0 then
      Break;
    ALista.Add(Copy(vConteudo, vIni + Length(vAbre), vFim - vIni - Length(vAbre)));
    vIni := PosEx(vAbre, vConteudo, vFim);
  end;
end;

/// The search path the .dproj declares that can be resolved here: $(PascalRAL)
/// becomes the RAL root, a relative folder is taken from the project folder,
/// the rest of the macros are dropped
procedure CaminhosDoDproj(const ADproj, ARaizRAL, APasta: string; ALista: TStrings);
var
  vElementos, vItens: TStringList;
  vInt, vIt: integer;
  vItem: string;
begin
  vElementos := TStringList.Create;
  vItens := TStringList.Create;
  try
    vItens.Delimiter := ';';
    vItens.StrictDelimiter := True;
    ElementosDoDproj(ADproj, 'DCC_UnitSearchPath', vElementos);
    for vInt := 0 to vElementos.Count - 1 do
    begin
      vItens.DelimitedText := vElementos[vInt];
      for vIt := 0 to vItens.Count - 1 do
      begin
        vItem := StringReplace(Trim(vItens[vIt]), '$(PascalRAL)',
          ExcludeTrailingPathDelimiter(ARaizRAL), [rfReplaceAll, rfIgnoreCase]);
        if (vItem = '') or (Pos('$(', vItem) > 0) then
          Continue;
        if not ((ExtractFileDrive(vItem) <> '') or (vItem[1] in ['\', '/'])) then
          vItem := APasta + vItem;
        if DirectoryExists(vItem) then
          ALista.Add(ExcludeTrailingPathDelimiter(ExpandFileName(vItem)));
      end;
    end;
  finally
    vItens.Free;
    vElementos.Free;
  end;
end;

procedure NamespacesDoDproj(const ADproj: string; ALista: TStrings);
var
  vElementos, vItens: TStringList;
  vInt, vIt: integer;
begin
  ALista.Delimiter := ';';
  ALista.StrictDelimiter := True;
  ALista.DelimitedText := NamespacesPadrao + ';Vcl.Touch;Vcl.Samples;Vcl.Shell';
  vElementos := TStringList.Create;
  vItens := TStringList.Create;
  try
    vItens.Delimiter := ';';
    vItens.StrictDelimiter := True;
    ElementosDoDproj(ADproj, 'DCC_Namespace', vElementos);
    for vInt := 0 to vElementos.Count - 1 do
    begin
      vItens.DelimitedText := vElementos[vInt];
      for vIt := 0 to vItens.Count - 1 do
        if (Trim(vItens[vIt]) <> '') and (Pos('$(', vItens[vIt]) = 0) and
           (ALista.IndexOf(Trim(vItens[vIt])) < 0) then
          ALista.Add(Trim(vItens[vIt]));
    end;
  finally
    vItens.Free;
    vElementos.Free;
  end;
end;

function CompilarDelphi(AIDE: TIDEInstance; const APasta, AProjeto, ARaizRAL,
  ATrabalho: string; ALog: TLogLinha; out AErros: string): boolean;
var
  vFonte, vCompilador: string;
  vBusca, vNamespaces: TStringList;
  vExec: TExecucao;
  vDproj: string;
begin
  Result := False;
  AErros := '';
  vCompilador := AIDE.BuildFile;
  if not FileExists(vCompilador) then
    vCompilador := AIDE.RootDir + 'bin' + PathDelim + 'dcc32.exe';
  if not FileExists(vCompilador) then
  begin
    AErros := Format(emConfSemCompilador, [vCompilador]);
    Exit;
  end;
  vFonte := ChangeFileExt(AProjeto, '.dpr');
  if not FileExists(vFonte) then
  begin
    AErros := Format(emConfSemFonte, [AProjeto]);
    Exit;
  end;
  // the repository does not version the .res: an empty valid one stands in
  if not FileExists(ChangeFileExt(vFonte, '.res')) then
    GravarBytes(ChangeFileExt(vFonte, '.res'), RES_VAZIO, SizeOf(RES_VAZIO));
  vDproj := ChangeFileExt(AProjeto, '.dproj');
  vBusca := TStringList.Create;
  vNamespaces := TStringList.Create;
  vExec := TExecucao.Create;
  try
    vBusca.Add(AIDE.RootDir + 'lib' + PathDelim + 'win32' + PathDelim + 'release');
    ListarPastas(ARaizRAL + 'src' + PathDelim, vBusca);
    ListarPastas(APasta, vBusca);
    CaminhosDoDproj(vDproj, ARaizRAL, APasta, vBusca);
    NamespacesDoDproj(vDproj, vNamespaces);
    ForceDirectories(ATrabalho + 'dcu');
    ForceDirectories(ATrabalho + 'exe');
    vExec.Executavel := vCompilador;
    vExec.Parametros.Add('--no-config');
    vExec.Parametros.Add('-B');
    vExec.Parametros.Add('-Q');
    vExec.Parametros.Add('-$D-');
    vExec.Parametros.Add('-$L-');
    vExec.Parametros.Add('-$Y-');
    vExec.Parametros.Add('-NS' + Juntar(vNamespaces));
    vExec.Parametros.Add('-U' + Juntar(vBusca));
    vExec.Parametros.Add('-I' + Juntar(vBusca));
    vExec.Parametros.Add('-R' + Juntar(vBusca));
    vExec.Parametros.Add('-E' + ExcludeTrailingPathDelimiter(ATrabalho + 'exe'));
    vExec.Parametros.Add('-N0' + ExcludeTrailingPathDelimiter(ATrabalho + 'dcu'));
    vExec.Parametros.Add(ExtractFileName(vFonte));
    vExec.PastaTrabalho := APasta;
    vExec.Log := ALog;
    Result := vExec.Executar;
    if not Result then
    begin
      AErros := PrimeiroErroPorArquivo(vExec.Saida);
      if AErros = '' then
        AErros := Format(emConfSaida, [vExec.CodigoSaida]);
    end;
  finally
    vExec.Free;
    vNamespaces.Free;
    vBusca.Free;
  end;
end;

{$ENDIF}

{ Lazarus }

/// Every .lpk under the folder
procedure ListarPacotesLpk(const APasta: string; ALista: TStrings);
var
  vBusca: TSearchRec;
  vRaiz: string;
begin
  vRaiz := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vRaiz + AllFilesMask, faAnyFile, vBusca) <> 0 then
    Exit;
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        ListarPacotesLpk(vRaiz + vBusca.Name, ALista)
      else if SameText(ExtractFileExt(vBusca.Name), '.lpk') then
        ALista.Add(vRaiz + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

/// Links to every .lpk of the RAL sources go into the copy of the configuration
/// (after dropping the links of the same names that point elsewhere)
function LigarPacotesNaCopia(AIDE: TIDEInstance; const ACopia, ARaizRAL: string;
  ALog: TLogLinha; out AErro: string): boolean;
var
  vPacotes, vLinks: TStringList;
  vExec: TExecucao;
  vInt, vIdx: integer;
  vNome: string;
begin
  Result := True;
  AErro := '';
  vPacotes := TStringList.Create;
  vLinks := TStringList.Create;
  vLinks.CaseSensitive := False;
  vExec := TExecucao.Create;
  try
    ListarPacotesLpk(ARaizRAL + 'pkg', vPacotes);
    if vPacotes.Count = 0 then
      Exit;
    LerLinks(ACopia, vLinks);
    for vInt := 0 to vPacotes.Count - 1 do
    begin
      vNome := ChangeFileExt(ExtractFileName(vPacotes[vInt]), '');
      vIdx := vLinks.IndexOfName(vNome);
      while vIdx >= 0 do
      begin
        vLinks.Delete(vIdx);
        vIdx := vLinks.IndexOfName(vNome);
      end;
    end;
    GravarLinks(ACopia, vLinks, AErro);
    vExec.Executavel := AIDE.BuildFile;
    vExec.Log := ALog;
    vExec.Parametros.Add('--primary-config-path=' + ExcludeTrailingPathDelimiter(ACopia));
    vExec.Parametros.Add('--lazarusdir=' + ExcludeTrailingPathDelimiter(AIDE.RootDir));
    vExec.Parametros.Add('--add-package-link');
    vExec.Parametros.AddStrings(vPacotes);
    Result := vExec.Executar;
    if not Result then
      AErro := Format(emConfLinks, [PrimeiroErroPorArquivo(vExec.Saida)]);
  finally
    vExec.Free;
    vLinks.Free;
    vPacotes.Free;
  end;
end;

function CopiarConfiguracao(AIDE: TIDEInstance; const ADestino: string): boolean;
var
  vBusca: TSearchRec;
begin
  ForceDirectories(ADestino);
  if (AIDE.ConfigDir <> '') and
     (FindFirst(IncludeTrailingPathDelimiter(AIDE.ConfigDir) + AllFilesMask, faAnyFile,
                vBusca) = 0) then
  try
    repeat
      if (vBusca.Attr and faDirectory) = 0 then
        CopiarArquivo(IncludeTrailingPathDelimiter(AIDE.ConfigDir) + vBusca.Name,
          ADestino + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
  Result := DirectoryExists(ADestino);
end;

function CompilarLazarus(AIDE: TIDEInstance; const APasta, AProjeto, ARaizRAL,
  ATrabalho: string; ALog: TLogLinha; out AErros: string): boolean;
var
  vCopia: string;
  vExec: TExecucao;
begin
  Result := False;
  AErros := '';
  if not FileExists(AIDE.BuildFile) then
  begin
    AErros := Format(emConfSemCompilador, [AIDE.BuildFile]);
    Exit;
  end;
  vCopia := ATrabalho + 'config' + PathDelim;
  if not CopiarConfiguracao(AIDE, vCopia) then
  begin
    AErros := Format(emConfSemCopia, [vCopia]);
    Exit;
  end;
  if not LigarPacotesNaCopia(AIDE, vCopia, ARaizRAL, ALog, AErros) then
    Exit;
  vExec := TExecucao.Create;
  try
    vExec.Executavel := AIDE.BuildFile;
    vExec.Parametros.Add('--primary-config-path=' + ExcludeTrailingPathDelimiter(vCopia));
    vExec.Parametros.Add('--lazarusdir=' + ExcludeTrailingPathDelimiter(AIDE.RootDir));
    vExec.Parametros.Add('-B');
    vExec.Parametros.Add(AProjeto);
    vExec.PastaTrabalho := APasta;
    vExec.Log := ALog;
    Result := vExec.Executar;
    if not Result then
    begin
      AErros := PrimeiroErroPorArquivo(vExec.Saida);
      if AErros = '' then
        AErros := Format(emConfSaida, [vExec.CodigoSaida]);
    end;
  finally
    vExec.Free;
  end;
end;

{ Publico }

/// The project in the copy, '' (and AErros) when it cannot be compiled by this IDE
function ProjetoNaCopia(AIDE: TIDEInstance; const AProjeto, APastaOrigem,
  APastaCopia: string; out AErros: string): string;
var
  vExt: string;
begin
  Result := APastaCopia + Copy(ExpandFileName(AProjeto), Length(APastaOrigem) + 1, MaxInt);
  vExt := LowerCase(ExtractFileExt(AProjeto));
  AErros := '';
  if ((AIDE.Tipo = tiDelphi) and not ((vExt = '.dpr') or (vExt = '.dproj'))) or
     ((AIDE.Tipo = tiLazarus) and (vExt <> '.lpi')) then
  begin
    AErros := Format(emConfTipoErrado, [ExtractFileName(AProjeto),
      IfThen(AIDE.Tipo = tiDelphi, 'Delphi', 'Lazarus')]);
    Result := '';
  end;
end;

function CompilarNaCopia(AIDE: TIDEInstance; const AProjetoCopia, ARaizRAL,
  ATrabalho: string; ALog: TLogLinha; out AErros: string): boolean;
var
  vPasta: string;
begin
  vPasta := ExtractFilePath(AProjetoCopia);
  if AIDE.Tipo = tiLazarus then
    Result := CompilarLazarus(AIDE, vPasta, AProjetoCopia, ARaizRAL, ATrabalho, ALog, AErros)
  else
  begin
    {$IFDEF MSWINDOWS}
    Result := CompilarDelphi(AIDE, vPasta, AProjetoCopia, ARaizRAL, ATrabalho, ALog, AErros);
    {$ELSE}
    Result := False;
    AErros := emConfSoWindows;
    {$ENDIF}
  end;
end;

function CompilarProjeto(AIDE: TIDEInstance; const AProjeto, ARaizRAL: string;
  out APrimeiroErro: string; ALog: TLogLinha; const APastaCopia: string): boolean;
var
  vOrigem, vTrabalho, vCopia, vProjeto, vRaiz: string;
begin
  Result := False;
  APrimeiroErro := '';
  vRaiz := IncludeTrailingPathDelimiter(ExpandFileName(ARaizRAL));
  if not FileExists(AProjeto) then
  begin
    APrimeiroErro := Format(emConfProjetoAusente, [AProjeto]);
    Exit;
  end;
  if not DirectoryExists(vRaiz + 'src') then
  begin
    APrimeiroErro := Format(emConfSemRAL, [vRaiz]);
    Exit;
  end;
  vOrigem := APastaCopia;
  if vOrigem = '' then
    vOrigem := ExtractFilePath(ExpandFileName(AProjeto));
  vOrigem := IncludeTrailingPathDelimiter(ExpandFileName(vOrigem));
  vTrabalho := PastaTemporaria;
  vCopia := vTrabalho + 'projeto' + PathDelim;
  try
    CopiarArvore(vOrigem, vCopia);
    vProjeto := ProjetoNaCopia(AIDE, AProjeto, vOrigem, vCopia, APrimeiroErro);
    if vProjeto = '' then
      Exit;
    Result := CompilarNaCopia(AIDE, vProjeto, vRaiz, vTrabalho, ALog, APrimeiroErro);
  finally
    ApagarPasta(vTrabalho);
  end;
end;

function CompilarConjunto(AIDE: TIDEInstance; AConjunto: TConjuntoArquivos;
  const AProjeto, ARaizRAL: string; out APrimeiroErro: string; ALog: TLogLinha): boolean;
var
  vTrabalho, vCopia, vProjeto, vRaiz, vProjetoAbs: string;
  vAlterados: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  Result := False;
  APrimeiroErro := '';
  vRaiz := IncludeTrailingPathDelimiter(ExpandFileName(ARaizRAL));
  if not DirectoryExists(vRaiz + 'src') then
  begin
    APrimeiroErro := Format(emConfSemRAL, [vRaiz]);
    Exit;
  end;
  vProjetoAbs := ExpandFileName(AProjeto);
  if not ((ExtractFileDrive(AProjeto) <> '') or (Copy(AProjeto, 1, 1) = PathDelim)) then
    vProjetoAbs := ExpandFileName(AConjunto.Pasta + AProjeto);
  vTrabalho := PastaTemporaria;
  vCopia := vTrabalho + 'projeto' + PathDelim;
  vAlterados := TList.Create;
  try
    CopiarArvore(AConjunto.Pasta, vCopia);
    AConjunto.ListarAlterados(vAlterados);
    for vInt := 0 to vAlterados.Count - 1 do
    begin
      vArq := TArquivoMigrado(vAlterados[vInt]);
      if SameText(Copy(vArq.Caminho, 1, Length(AConjunto.Pasta)), AConjunto.Pasta) then
      begin
        ForceDirectories(ExtractFilePath(vCopia + vArq.Relativo));
        vArq.Texto.Gravar(StringReplace(vCopia + vArq.Relativo, '/', PathDelim,
          [rfReplaceAll]), True);
      end;
    end;
    vProjeto := ProjetoNaCopia(AIDE, vProjetoAbs, AConjunto.Pasta, vCopia, APrimeiroErro);
    if vProjeto = '' then
      Exit;
    Result := CompilarNaCopia(AIDE, vProjeto, vRaiz, vTrabalho, ALog, APrimeiroErro);
  finally
    vAlterados.Free;
    ApagarPasta(vTrabalho);
  end;
end;

end.
