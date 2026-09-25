/// Discovery of Delphi installations. Windows only: Delphi does not run
/// elsewhere, and whoever uses this unit includes it under {$IFDEF MSWINDOWS}.
/// Two sources, because neither is enough alone: the registry says where the
/// IDEs the official installer registered are, but loses those copied,
/// restored or reinstalled over; the disk finds them all, and each folder
/// identifies itself: bin\rsvars.bat gives BDS and BDSCOMMONDIR,
/// bin\dcc32<N>.dll gives the suffix.
unit RALInst.IDE.Delphi;

{$mode ObjFPC}{$H+}

{$IFNDEF MSWINDOWS}
  {$FATAL RALInst.IDE.Delphi so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALInst.IDE;

type
  /// Finds Delphi installations in the registry and on disk.
  TBuscaDelphi = class(TBuscaIDE)
  private
    FChavesHKCU: TStringList;
    /// Registry capabilities and warnings of one installation
    procedure CompletarRegistro(AIDE: TIDEInstance);
    /// Keys of the versions already opened in this account (HKCU): a search
    /// that skipped the registry (chosen folder, disk scan) needs them too, or
    /// every IDE looks like it was never opened
    procedure LerChavesHKCU;
    /// Installations the registry knows (HKCU and HKLM, both views)
    procedure LerRegistro(ALista: TIDEList);
  protected
    function PastaIgnorada(const ANome: string): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BuscarPadrao(ALista: TIDEList); override;
    procedure Finalizar(ALista: TIDEList); override;
    function InspecionarPasta(const APasta: string): TIDEInstance; override;
  end;

const
  /// Folder in lib\ -> compiler in bin\ -> normalized platform name
  DelphiPlataformas: array[0..11] of array[0..2] of string = (
    ('win32',        'dcc32.exe',          'win32'),
    ('win64',        'dcc64.exe',          'win64'),
    ('osx32',        'dccosx.exe',         'osx32'),
    ('osx64',        'dccosx64.exe',       'osx64'),
    ('osxarm64',     'dccosxarm64.exe',    'osxarm64'),
    ('linux64',      'dcclinux64.exe',     'linux64'),
    ('android',      'dccaarm.exe',        'android'),
    ('android64',    'dccaarm64.exe',      'android64'),
    ('iosDevice32',  'dcciosarm.exe',      'iosdevice32'),
    ('iosDevice64',  'dcciosarm64.exe',    'iosdevice64'),
    ('iossimulator', 'dccios32.exe',       'iossimulator'),
    ('iossimarm64',  'dcciossimarm64.exe', 'iossimarm64')
  );

implementation

uses
  Windows, Registry, RegExpr,
  RALInst.Mensagens;

const
  RegBases: array[0..3] of string = (
    '\Software\Borland\Delphi',
    '\Software\Borland\BDS',
    '\Software\CodeGear\BDS',
    '\Software\Embarcadero\BDS'
  );

// valor de uma linha "@SET NOME=valor" do rsvars.bat
function LerRsvars(const AArquivo, ANome: string): string;
var
  vLinhas: TStringList;
  vInt, vPos: integer;
  vLinha, vPrefixo: string;
begin
  Result := '';
  if not FileExists(AArquivo) then
    Exit;
  vPrefixo := 'SET ' + UpperCase(ANome) + '=';
  vLinhas := TStringList.Create;
  try
    vLinhas.LoadFromFile(AArquivo);
    for vInt := 0 to Pred(vLinhas.Count) do
    begin
      vLinha := Trim(vLinhas[vInt]);
      if (vLinha <> '') and (vLinha[1] = '@') then
        Delete(vLinha, 1, 1);
      vPos := Pos(vPrefixo, UpperCase(vLinha));
      if vPos = 1 then
        Exit(Trim(Copy(vLinha, Length(vPrefixo) + 1, MaxInt)));
    end;
  finally
    vLinhas.Free;
  end;
end;

// sufixo pelo proprio compilador: bin\dcc32290.dll, bin\DCC140.dll,
// ou pela RTL: bin\rtl290.bpl
function SufixoNoDisco(const ABin: string): string;
var
  vSearch: TSearchRec;
  vRegex: TRegExpr;
begin
  Result := '';
  vRegex := TRegExpr.Create('^(?:dcc32|dcc|rtl)(\d{2,3})\.(?:dll|bpl)$');
  try
    vRegex.ModifierI := True;
    if FindFirst(ABin + '*.*', faAnyFile, vSearch) = 0 then
    try
      repeat
        if vRegex.Exec(vSearch.Name) then
        begin
          Result := vRegex.Match[1];
          Break;
        end;
      until FindNext(vSearch) <> 0;
    finally
      SysUtils.FindClose(vSearch);
    end;
  finally
    vRegex.Free;
  end;
end;

// ultima parte de um caminho que parece versao do BDS ('23.0')
function BDSNoCaminho(const ACaminho: string): string;
var
  vRegex: TRegExpr;
begin
  Result := '';
  vRegex := TRegExpr.Create('(\d{1,2}\.0)\\?$');
  try
    if vRegex.Exec(Trim(ACaminho)) then
      Result := vRegex.Match[1];
  finally
    vRegex.Free;
  end;
end;

function ChaveExiste(ARaiz: HKEY; AAcesso: LongWord; const AChave: string): boolean;
var
  vReg: TRegistry;
begin
  vReg := TRegistry.Create(AAcesso);
  try
    vReg.RootKey := ARaiz;
    Result := vReg.KeyExists(AChave);
  finally
    vReg.Free;
  end;
end;

{ TBuscaDelphi }

function TBuscaDelphi.PastaIgnorada(const ANome: string): boolean;
begin
  // arvores grandes que nunca contem uma IDE Delphi, inclusive as do Lazarus
  // quando o usuario aponta uma pasta que mistura as duas
  Result := inherited PastaIgnorada(ANome) or
            SameText(ANome, 'lib') or SameText(ANome, 'source') or
            SameText(ANome, 'Samples') or SameText(ANome, 'Demos') or
            SameText(ANome, 'include') or SameText(ANome, 'ObjRepos') or
            SameText(ANome, 'lazarus') or SameText(ANome, 'fpc') or
            SameText(ANome, 'fpcsrc') or SameText(ANome, 'config_lazarus');
end;

constructor TBuscaDelphi.Create;
begin
  inherited Create;
  FChavesHKCU := TStringList.Create;
  FChavesHKCU.CaseSensitive := False;
end;

destructor TBuscaDelphi.Destroy;
begin
  FreeAndNil(FChavesHKCU);
  inherited Destroy;
end;

function TBuscaDelphi.InspecionarPasta(const APasta: string): TIDEInstance;
var
  vRaiz, vBin, vRsvars, vBDS, vSufixo, vExe: string;
  vProd, vInt: integer;
  vTemSubpastas: boolean;
begin
  Result := nil;
  vRaiz := NormalizarPasta(APasta);
  vBin := vRaiz + 'bin' + PathDelim;

  // sem compilador de linha de comando nao ha o que o instalador faca
  if not FileExists(vBin + 'dcc32.exe') then
    Exit;

  vRsvars := vBin + 'rsvars.bat';

  // qual Delphi e: BDSCOMMONDIR termina na versao do BDS
  // ('...\Studio\23.0'), a pasta costuma ter o mesmo nome, e o sufixo do
  // compilador desempata o resto (Delphi 7 nao tem nenhum dos dois)
  vSufixo := SufixoNoDisco(vBin);
  vBDS := BDSNoCaminho(LerRsvars(vRsvars, 'BDSCOMMONDIR'));
  if vBDS = '' then
    vBDS := BDSNoCaminho(vRaiz);

  vProd := ProdutoPorBDS(vBDS);
  if vProd < 0 then
  begin
    if FileExists(vBin + 'delphi32.exe') and not FileExists(vBin + 'bds.exe') then
      vProd := ProdutoPorSufixo('70')
    else
      vProd := ProdutoPorSufixo(vSufixo);
  end;

  Result := TIDEInstance.Create(tiDelphi);
  Result.RootDir := vRaiz;
  Result.BuildFile := vBin + 'dcc32.exe';
  Result.Origem := oiDisco;

  vExe := '';
  if FileExists(vBin + 'bds.exe') then
    vExe := vBin + 'bds.exe'
  else if FileExists(vBin + 'delphi32.exe') then
    vExe := vBin + 'delphi32.exe';
  Result.ExeFile := vExe;
  // a IDE de 64 bits carrega pacotes de design compilados para Win64: so vale
  // quando ha o bds.exe dela e o designide.dcp de Win64 para compilar contra
  Result.IDE64 := FileExists(vRaiz + 'bin64' + PathDelim + 'bds.exe') and
    FileExists(vRaiz + 'lib' + PathDelim + 'win64' + PathDelim + 'release' + PathDelim +
               'designide.dcp');

  Result.CommonDir := LerRsvars(vRsvars, 'BDSCOMMONDIR');
  if Result.CommonDir <> '' then
    Result.CommonDir := IncludeTrailingPathDelimiter(Result.CommonDir);

  if vProd >= 0 then
  begin
    Result.Nome := DelphiProdutos[vProd].Nome;
    Result.BDSVersao := DelphiProdutos[vProd].BDS;
    Result.Versao := DelphiProdutos[vProd].VersaoNum;
    Result.VersaoCompilador := DelphiProdutos[vProd].Compilador;
    Result.SufixoPacote := DelphiProdutos[vProd].Sufixo;
    if DelphiProdutos[vProd].BDS = '' then
      Result.RegKey := DelphiProdutos[vProd].RegBase + '\7.0'
    else
      Result.RegKey := DelphiProdutos[vProd].RegBase + '\' +
                       DelphiProdutos[vProd].BDS;

    if (vSufixo <> '') and (vSufixo <> Result.SufixoPacote) then
      Result.Avisos.Add(Format(wmDelphiSufixo,
        [vSufixo, Result.Nome, Result.SufixoPacote]));
  end
  else
  begin
    // versao que a tabela nao conhece: lista, mas nao deixa instalar
    Result.Nome := cmDelphiNaoReconhecido;
    if vBDS <> '' then
      Result.Nome := Result.Nome + ' (BDS ' + vBDS + ')';
    Result.BDSVersao := vBDS;
    Result.SufixoPacote := vSufixo;
    Result.Versao := '0';
    Result.Avisos.Add(wmDelphiDesconhecido);
  end;

  // plataformas: precisa da pasta em lib\ e do compilador em bin\
  vTemSubpastas := False;
  for vInt := Low(DelphiPlataformas) to High(DelphiPlataformas) do
    if DirectoryExists(vRaiz + 'lib' + PathDelim + DelphiPlataformas[vInt][0]) then
    begin
      vTemSubpastas := True;
      if FileExists(vBin + DelphiPlataformas[vInt][1]) then
        Result.Plataformas.Add(DelphiPlataformas[vInt][2]);
    end;
  // Delphi 7 e 2010 guardam as units direto em lib\
  if not vTemSubpastas then
    Result.Plataformas.Add('win32');

  if vProd >= 0 then
  begin
    Result.Capacidades := [ciCompilar, ciLibraryPath];
    if vExe <> '' then
      Result.Capacidades := Result.Capacidades + [ciInstalarNaIDE];
  end;
end;

procedure TBuscaDelphi.CompletarRegistro(AIDE: TIDEInstance);
begin
  // sem chave conhecida nao ha onde registrar pacote nem library path
  if AIDE.RegKey = '' then
  begin
    AIDE.Capacidades := AIDE.Capacidades - [ciInstalarNaIDE, ciLibraryPath];
    Exit;
  end;

  // a IDE copia os padroes para HKCU na primeira vez que abre; escrever la
  // antes disso faz ela pular a copia e abrir sem os proprios pacotes
  if FChavesHKCU.IndexOf(AIDE.RegKey) < 0 then
  begin
    AIDE.Capacidades := AIDE.Capacidades - [ciInstalarNaIDE, ciLibraryPath];
    if AIDE.ExeFile <> '' then
      AIDE.Avisos.Add(wmDelphiNuncaAberta);
  end;

  if AIDE.Origem <> oiRegistro then
    AIDE.Avisos.Add(Format(wmDelphiForaDoRegistro, [AIDE.RegKey]));
end;

procedure TBuscaDelphi.LerRegistro(ALista: TIDEList);
var
  vReg: TRegistry;
  vVersoes: TStringList;
  vBase, vChave, vRaiz: string;
  vIntBase, vIntVer, vIntRaiz: integer;
  vIDE: TIDEInstance;
  vRaizes: array[0..2] of HKEY;
  vAcessos: array[0..2] of LongWord;
begin
  // HKCU tem a configuracao; RootDir e App as vezes so estao em HKLM, e o
  // instalador do Delphi e 32 bits: um binario win64 so ve a chave dele
  // pedindo a visao de 32 bits (WOW6432Node)
  vRaizes[0] := HKEY_CURRENT_USER;  vAcessos[0] := KEY_READ;
  vRaizes[1] := HKEY_LOCAL_MACHINE; vAcessos[1] := KEY_READ or KEY_WOW64_32KEY;
  vRaizes[2] := HKEY_LOCAL_MACHINE; vAcessos[2] := KEY_READ or KEY_WOW64_64KEY;

  vVersoes := TStringList.Create;
  try
    for vIntRaiz := Low(vRaizes) to High(vRaizes) do
      for vIntBase := Low(RegBases) to High(RegBases) do
      begin
        vBase := RegBases[vIntBase];
        vReg := TRegistry.Create(vAcessos[vIntRaiz]);
        try
          vReg.RootKey := vRaizes[vIntRaiz];
          if not vReg.OpenKeyReadOnly(vBase) then
            Continue;
          vReg.GetKeyNames(vVersoes);
          vReg.CloseKey;

          for vIntVer := 0 to Pred(vVersoes.Count) do
          begin
            vChave := vBase + '\' + vVersoes[vIntVer];
            if BDSNoCaminho(vVersoes[vIntVer]) = '' then
              Continue;

            if vRaizes[vIntRaiz] = HKEY_CURRENT_USER then
              FChavesHKCU.Add(vChave);

            if not vReg.OpenKeyReadOnly(vChave) then
              Continue;
            try
              vRaiz := '';
              if vReg.ValueExists('RootDir') then
                vRaiz := vReg.ReadString('RootDir');
              if (vRaiz = '') and vReg.ValueExists('App') then
                vRaiz := ExtractFilePath(ExcludeTrailingPathDelimiter(
                           ExtractFilePath(vReg.ReadString('App'))));
            finally
              vReg.CloseKey;
            end;

            // chave sem RootDir e resto de IDE desinstalada ou so a
            // configuracao do usuario; quem decide se ha IDE e o disco
            if vRaiz = '' then
              Continue;

            vIDE := InspecionarPasta(vRaiz);
            if vIDE = nil then
              Continue;
            vIDE.Origem := oiRegistro;
            vIDE.RegKey := vChave;
            ALista.Adicionar(vIDE);
          end;
        finally
          vReg.Free;
        end;
      end;
  finally
    vVersoes.Free;
  end;
end;

procedure TBuscaDelphi.BuscarPadrao(ALista: TIDEList);
var
  vRaizes: TStringList;
  vInt: integer;
  vPF: string;

  procedure AdicionarRaiz(const APasta: string);
  begin
    if (APasta <> '') and DirectoryExists(APasta) then
      vRaizes.Add(NormalizarPasta(APasta));
  end;

begin
  LerRegistro(ALista);

  vRaizes := TStringList.Create;
  try
    vRaizes.Sorted := True;
    vRaizes.Duplicates := dupIgnore;

    // quem instala uma IDE fora do padrao costuma instalar as outras ao
    // lado: a pasta-mae e a avo de cada IDE registrada entram na busca
    for vInt := 0 to Pred(ALista.Count) do
    begin
      AdicionarRaiz(ExtractFilePath(ExcludeTrailingPathDelimiter(
        ALista[vInt].RootDir)));
      AdicionarRaiz(ExtractFilePath(ExcludeTrailingPathDelimiter(
        ExtractFilePath(ExcludeTrailingPathDelimiter(ALista[vInt].RootDir)))));
    end;

    for vPF in TStringArray.Create(
                 SysUtils.GetEnvironmentVariable('ProgramFiles(x86)'),
                 SysUtils.GetEnvironmentVariable('ProgramFiles')) do
      if vPF <> '' then
      begin
        AdicionarRaiz(IncludeTrailingPathDelimiter(vPF) + 'Embarcadero');
        AdicionarRaiz(IncludeTrailingPathDelimiter(vPF) + 'CodeGear');
        AdicionarRaiz(IncludeTrailingPathDelimiter(vPF) + 'Borland');
      end;

    for vInt := 0 to Pred(RaizesExtras.Count) do
      AdicionarRaiz(RaizesExtras[vInt]);

    for vInt := 0 to Pred(vRaizes.Count) do
      BuscarEm(ALista, vRaizes[vInt], 3);
  finally
    vRaizes.Free;
  end;
end;

procedure TBuscaDelphi.LerChavesHKCU;
var
  vReg: TRegistry;
  vVersoes: TStringList;
  vIntBase, vIntVer: integer;
begin
  vVersoes := TStringList.Create;
  vReg := TRegistry.Create(KEY_READ);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    for vIntBase := Low(RegBases) to High(RegBases) do
    begin
      if not vReg.OpenKeyReadOnly(RegBases[vIntBase]) then
        Continue;
      vReg.GetKeyNames(vVersoes);
      vReg.CloseKey;
      for vIntVer := 0 to Pred(vVersoes.Count) do
        if FChavesHKCU.IndexOf(RegBases[vIntBase] + '\' +
                               vVersoes[vIntVer]) < 0 then
          FChavesHKCU.Add(RegBases[vIntBase] + '\' + vVersoes[vIntVer]);
    end;
  finally
    vReg.Free;
    vVersoes.Free;
  end;
end;

procedure TBuscaDelphi.Finalizar(ALista: TIDEList);
var
  vInt, vOutra: integer;
begin
  LerChavesHKCU;
  for vInt := 0 to Pred(ALista.Count) do
  begin
    if ALista[vInt].Tipo <> tiDelphi then
      Continue;

    CompletarRegistro(ALista[vInt]);

    for vOutra := 0 to Pred(ALista.Count) do
      if (vOutra <> vInt) and (ALista[vOutra].Tipo = tiDelphi) and
         (ALista[vInt].RegKey <> '') and
         SameText(ALista[vOutra].RegKey, ALista[vInt].RegKey) then
        ALista[vInt].Avisos.Add(Format(wmDelphiDivideRegistro, [ALista[vOutra].RootDir]));
  end;

  inherited Finalizar(ALista);
end;

end.
