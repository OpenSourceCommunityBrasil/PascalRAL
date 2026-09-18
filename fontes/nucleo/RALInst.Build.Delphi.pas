unit RALInst.Build.Delphi;

{$mode ObjFPC}{$H+}

// Compila os pacotes do RAL num Delphi, pela linha de comando, a partir do
// executavel FPC: `dcc32`/`dcc64` para os pacotes e `brcc32` para o `.res` que
// falta. Nenhum projeto Delphi, nenhum msbuild, a IDE nunca e aberta.
//
// Tres coisas que fazem a diferenca entre um .bpl que a IDE aceita e um que
// ela recusa:
//
// - `--no-config` ignora o dcc32.cfg da maquina, que e o que faz a compilacao
//   ser a mesma em toda maquina; em troca, tudo tem de ser dito na linha.
// - sem `-LU`, o compilador linka Indy, FireDAC e afins *dentro* do .bpl: ele
//   compila limpo e a IDE recusa com erro de unidade duplicada. O `-LU` sai do
//   `requires` mais as dependencias implicitas (o Indy do IndyRAL), cada uma
//   confirmada pelo `.dcp` em disco — e o sufixo do Indy atualizado sai do
//   proprio nome do arquivo, nunca de um chute.
// - design-time no Delphi e sempre Win32.

{$IFNDEF MSWINDOWS}
  {$FATAL RALInst.Build.Delphi so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils, RALInst.IDE, RALInst.Catalogo, RALInst.Processo;

type
  TResultadoPacote = record
    Nome: string;
    Ok: boolean;
    Pulado: boolean;
    Motivo: string;
    Bpl: string;
    Dcp: string;
    TamanhoBpl: int64;
    Segundos: double;
  end;

  { TBuildDelphi }

  TBuildDelphi = class
  private
    FIDE: TIDEInstance;
    FCatalogo: TCatalogo;
    FRaizFontes: string;
    FPlataforma: string;
    FPastaBpl: string;
    FPastaDcp: string;
    FPastaDcu: string;
    FCaminhosExtras: TStringList;
    FLog: TLogLinha;
    FSimular: boolean;
    FResultados: array of TResultadoPacote;
    function GetResultado(AIndex: integer): TResultadoPacote;
    function GetTotal: integer;
    procedure Logar(const ALinha: string);
    procedure LogarProcesso(const ALinha: string);
    function Compilador: string;
    function Ferramenta(const ANome: string): string;
    function LibRelease: string;
    function PastaBplPadrao: string;
    function PastaDcpPadrao: string;
    function BuscarDcp(const ANome: string; out ANomeReal: string): boolean;
    function GarantirRes(APacote: TPacote): boolean;
    function EsperaPacoteDeTerceiro(APacote: TPacote): boolean;
    procedure MontarLU(APacote: TPacote; ALista: TStrings);
    procedure MontarBusca(APacote: TPacote; ALista: TStrings);
    procedure MontarObjetos(APacote: TPacote; ALista: TStrings);
    procedure MontarNamespaces(APacote: TPacote; ALista: TStrings);
    function CompilarPacote(APacote: TPacote; var AResultado: TResultadoPacote): boolean;
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;

    // pacotes na ordem que o catalogo deu (F2); devolve False se algum falhou
    function Compilar(APacotes: TList): boolean;
    function Relatorio: string;

    property IDE: TIDEInstance read FIDE;
    // 'win32' (padrao) ou 'win64'
    property Plataforma: string read FPlataforma write FPlataforma;
    // saida: por padrao <BDSCOMMONDIR>\Bpl e \Dcp, onde a IDE procura
    property PastaBpl: string read FPastaBpl write FPastaBpl;
    property PastaDcp: string read FPastaDcp write FPastaDcp;
    // .dcu intermediarios; por padrao <fontes>\compiled\<bds>\<plataforma>
    property PastaDcu: string read FPastaDcu write FPastaDcu;
    // library paths das dependencias externas (mORMot2, Zeos...) — F7
    property CaminhosExtras: TStringList read FCaminhosExtras;
    property Log: TLogLinha read FLog write FLog;
    // monta e mostra os comandos sem executar nenhum
    property Simular: boolean read FSimular write FSimular;
    property Total: integer read GetTotal;
    property Resultados[AIndex: integer]: TResultadoPacote read GetResultado;
  end;

const
  // familias de pacotes de terceiros que valem como dependencia implicita: o
  // DCC_UsePackage dos .dproj do RAL tambem carrega lixo de outros projetos
  // (RESTDWCore, uniGUI26Core...), e por isso a lista e fechada
  ImplicitosConhecidos: array[0..10] of string = (
    'IndyCore', 'IndySystem', 'IndyProtocols',
    'FireDAC', 'FireDACCommon', 'FireDACCommonDriver',
    'ZComponent', 'ZCore', 'ZDbc', 'ZParseSql', 'ZPlain'
  );

  // namespaces que todo pacote precisa; o .dproj acrescenta os dele
  NamespacesPadrao = 'System;System.Win;Winapi;Vcl;Vcl.Imaging;Data;Data.Win;' +
                     'Xml;Web;Soap;Datasnap;Bde';

implementation

uses
  StrUtils, RegExpr;

function LerArquivoTexto(const AArquivo: string): string;
var
  vLista: TStringList;
begin
  Result := '';
  if not FileExists(AArquivo) then
    Exit;
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(AArquivo);
    Result := vLista.Text;
  finally
    vLista.Free;
  end;
end;

function TamanhoArquivo(const AArquivo: string): int64;
var
  vSearch: TSearchRec;
begin
  Result := 0;
  if FindFirst(AArquivo, faAnyFile, vSearch) = 0 then
  begin
    Result := vSearch.Size;
    FindClose(vSearch);
  end;
end;

// 'F2613 Unit 'libsagui' not found' / 'E1026 File not found: 'x.obj'' -> o nome
function UnidadeFaltando(ASaida: TStrings): string;
var
  vRegex: TRegExpr;
  vInt: integer;
begin
  Result := '';
  vRegex := TRegExpr.Create('(?:F2613 Unit|F2063 Could not compile used unit|' +
                            'E1026 File not found:|E2202 Required package)\s*''?([^'']+)''?');
  try
    for vInt := 0 to Pred(ASaida.Count) do
      if vRegex.Exec(ASaida[vInt]) then
        Exit(Trim(vRegex.Match[1]));
  finally
    vRegex.Free;
  end;
end;

function Juntar(ALista: TStrings; const ASeparador: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to Pred(ALista.Count) do
  begin
    if Result <> '' then
      Result := Result + ASeparador;
    Result := Result + ALista[vInt];
  end;
end;

{ TBuildDelphi }

constructor TBuildDelphi.Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
begin
  inherited Create;
  FIDE := AIDE;
  FCatalogo := ACatalogo;
  FPlataforma := 'win32';
  FCaminhosExtras := TStringList.Create;

  if (FCatalogo <> nil) and (FCatalogo.Origem is TOrigemLocal) then
    FRaizFontes := TOrigemLocal(FCatalogo.Origem).Raiz;

end;

destructor TBuildDelphi.Destroy;
begin
  FCaminhosExtras.Free;
  inherited Destroy;
end;

function TBuildDelphi.GetResultado(AIndex: integer): TResultadoPacote;
begin
  Result := FResultados[AIndex];
end;

function TBuildDelphi.GetTotal: integer;
begin
  Result := Length(FResultados);
end;

procedure TBuildDelphi.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

procedure TBuildDelphi.LogarProcesso(const ALinha: string);
begin
  Logar('    ' + ALinha);
end;

function TBuildDelphi.Ferramenta(const ANome: string): string;
begin
  Result := FIDE.RootDir + 'bin' + PathDelim + ANome;
end;

function TBuildDelphi.Compilador: string;
begin
  if SameText(FPlataforma, 'win64') then
    Result := Ferramenta('dcc64.exe')
  else
    Result := Ferramenta('dcc32.exe');
end;

function TBuildDelphi.PastaBplPadrao: string;
begin
  // Win32 fica na raiz; as outras plataformas ganham subpasta, do mesmo jeito
  // que a IDE organiza
  Result := FIDE.CommonDir + 'Bpl';
  if not SameText(FPlataforma, 'win32') then
    Result := Result + PathDelim + FPlataforma;
end;

function TBuildDelphi.PastaDcpPadrao: string;
begin
  Result := FIDE.CommonDir + 'Dcp';
  if not SameText(FPlataforma, 'win32') and
     DirectoryExists(Result + PathDelim + FPlataforma) then
    Result := Result + PathDelim + FPlataforma;
end;

function TBuildDelphi.LibRelease: string;
begin
  Result := FIDE.RootDir + 'lib' + PathDelim + LowerCase(FPlataforma) +
            PathDelim + 'release';
end;

function TBuildDelphi.BuscarDcp(const ANome: string; out ANomeReal: string): boolean;
var
  vPastas: TStringList;
  vPasta: string;
begin
  // o Indy atualizado pelo usuario ganha o sufixo da IDE (IndyCore290.dcp); o
  // nativo nao tem sufixo. Quem decide e o disco, nos dois casos.
  Result := False;
  ANomeReal := '';
  vPastas := TStringList.Create;
  try
    if FPastaDcp <> '' then
      vPastas.Add(FPastaDcp);
    // a pasta padrao da IDE entra mesmo quando a saida foi desviada: e onde
    // moram os .dcp de terceiros que o usuario ja instalou (Zeos, Indy novo)
    if FIDE.CommonDir <> '' then
      vPastas.Add(PastaDcpPadrao);
    vPastas.Add(LibRelease);
    vPastas.AddStrings(FCaminhosExtras);

    for vPasta in vPastas do
    begin
      if vPasta = '' then
        Continue;
      if (FIDE.SufixoPacote <> '') and
         FileExists(IncludeTrailingPathDelimiter(vPasta) + ANome + FIDE.SufixoPacote + '.dcp') then
      begin
        ANomeReal := ANome + FIDE.SufixoPacote;
        Exit(True);
      end;
      if FileExists(IncludeTrailingPathDelimiter(vPasta) + ANome + '.dcp') then
      begin
        ANomeReal := ANome;
        Exit(True);
      end;
    end;
  finally
    vPastas.Free;
  end;
end;

function TBuildDelphi.GarantirRes(APacote: TPacote): boolean;
var
  vRes, vRc: string;
  vLista: TStringList;
  vParams: TStringList;
begin
  // o repositorio do RAL nao versiona os .res; sobre um zip recem-baixado o
  // dcc32 morre em E1026. Um .rc vazio vira um .res valido de 32 bytes.
  Result := True;
  vRes := ChangeFileExt(APacote.Arquivo, '.res');
  if FileExists(vRes) then
    Exit;

  vRc := ChangeFileExt(APacote.Arquivo, '.rc');
  Logar('  gerando ' + ExtractFileName(vRes) + ' (não vem no repositório)');
  if FSimular then
    Exit;

  vLista := TStringList.Create;
  vParams := TStringList.Create;
  try
    if not FileExists(vRc) then
    begin
      vLista.Add('// gerado pelo RAL Installer: o repositório não versiona os .res');
      vLista.SaveToFile(vRc);
    end;
    vParams.Add('-fo' + vRes);
    vParams.Add(vRc);
    Result := RALInst.Processo.Executar(Ferramenta('brcc32.exe'), vParams,
                                        @LogarProcesso);
    if not Result then
      Logar('  ERRO: brcc32 não gerou ' + ExtractFileName(vRes));
  finally
    vParams.Free;
    vLista.Free;
  end;
end;

function TBuildDelphi.EsperaPacoteDeTerceiro(APacote: TPacote): boolean;
var
  vInt: integer;
  vNome: string;
begin
  Result := False;
  for vInt := 0 to Pred(APacote.Implicitos.Count) do
  begin
    vNome := APacote.Implicitos[vInt];
    if (AnsiIndexText(vNome, ImplicitosConhecidos) >= 0) or
       (Copy(LowerCase(vNome), 1, 7) = 'firedac') then
      Exit(True);
  end;
  for vInt := 0 to Pred(APacote.Externos.Count) do
    if AnsiIndexText(APacote.Externos[vInt], ImplicitosConhecidos) >= 0 then
      Exit(True);
end;

procedure TBuildDelphi.MontarLU(APacote: TPacote; ALista: TStrings);
var
  vNome, vReal: string;
  vInt: integer;

  procedure Tentar(const ANome: string; AObrigatorio: boolean);
  begin
    if ALista.IndexOf(ANome) >= 0 then
      Exit;
    if BuscarDcp(ANome, vReal) then
      ALista.Add(vReal)
    else if AObrigatorio then
      Logar('  AVISO: pacote exigido sem .dcp em disco: ' + ANome +
            ' (o compilador vai linkar as unidades dele dentro do .bpl)');
  end;

begin
  // dependencias internas entram sempre: o .dcp delas e gerado nesta mesma
  // rodada, antes deste pacote, e nao adianta perguntar ao disco
  for vInt := 0 to Pred(APacote.Internos.Count) do
    if ALista.IndexOf(APacote.Internos[vInt]) < 0 then
      ALista.Add(APacote.Internos[vInt]);

  // externas declaradas no requires: rtl, dbrtl, vcl, designide, dcldb...
  for vInt := 0 to Pred(APacote.Externos.Count) do
    Tentar(APacote.Externos[vInt], True);

  // implicitas: so as conhecidas, e so as que existem em disco
  for vInt := 0 to Pred(APacote.Implicitos.Count) do
  begin
    vNome := APacote.Implicitos[vInt];
    // o FireDAC e uma familia grande (um pacote por driver) e todos os
    // pedacos precisam entrar, senao o resto vem estatico no .bpl
    if (AnsiIndexText(vNome, ImplicitosConhecidos) >= 0) or
       (Copy(LowerCase(vNome), 1, 7) = 'firedac') then
      Tentar(vNome, False);
  end;
end;

procedure TBuildDelphi.MontarBusca(APacote: TPacote; ALista: TStrings);
var
  vUnidade, vPasta, vSub, vCandidata: string;
  vInt: integer;
begin
  ALista.Add(LibRelease);
  if FPastaDcp <> '' then
    ALista.Add(FPastaDcp);
  // a pasta padrao da IDE tem os .dcp de terceiros que o usuario instalou
  // (Zeos, Indy atualizado) e nao pode sair da busca nem quando a saida desta
  // rodada foi desviada para outro lugar
  if (FIDE.CommonDir <> '') and (ALista.IndexOf(PastaDcpPadrao) < 0) then
    ALista.Add(PastaDcpPadrao);
  if FPastaDcu <> '' then
    ALista.Add(FPastaDcu);

  // as pastas dos fontes do proprio pacote: o .dpk traz o caminho de cada
  // unidade, mas as unidades usam outras que ele nao lista (o kxBSON, por
  // exemplo, so aparece no .lpk)
  for vUnidade in APacote.Unidades do
  begin
    vPasta := ExcludeTrailingPathDelimiter(ExtractFilePath(
                FRaizFontes + StringReplace(vUnidade, '/', PathDelim, [rfReplaceAll])));
    if ALista.IndexOf(vPasta) < 0 then
      ALista.Add(vPasta);
  end;

  // os submodulos fazem parte da arvore do RAL, mas nem todo pacote Delphi
  // lista as unidades deles (o RALBSONStorage.dpk nao lista o kxBSON): as
  // pastas entram na busca de qualquer jeito
  for vInt := 0 to Pred(FCatalogo.Submodulos.Count) do
  begin
    vPasta := IncludeTrailingPathDelimiter(FRaizFontes +
                StringReplace(FCatalogo.Submodulos.Names[vInt], '/', PathDelim, [rfReplaceAll]));
    for vSub in TStringArray.Create('', 'Source', 'src') do
    begin
      vCandidata := ExcludeTrailingPathDelimiter(vPasta + vSub);
      if DirectoryExists(vCandidata) and (ALista.IndexOf(vCandidata) < 0) then
        ALista.Add(vCandidata);
    end;
  end;

  ALista.AddStrings(FCaminhosExtras);
end;

procedure TBuildDelphi.MontarObjetos(APacote: TPacote; ALista: TStrings);
var
  vUnidade, vPasta, vCandidata: string;

  procedure Tentar(const APastaObj: string);
  begin
    if DirectoryExists(APastaObj) and (ALista.IndexOf(APastaObj) < 0) then
      ALista.Add(APastaObj);
  end;

begin
  // {$L 'libbrotli.obj'}: o .obj nao fica ao lado da unidade, e sim numa
  // subpasta por compilador e plataforma. Sem isto o brotli morre em E1026.
  for vUnidade in APacote.Unidades do
  begin
    vPasta := IncludeTrailingPathDelimiter(ExtractFilePath(
                FRaizFontes + StringReplace(vUnidade, '/', PathDelim, [rfReplaceAll])));
    for vCandidata in TStringArray.Create(
          'static_libs' + PathDelim + 'delphi' + PathDelim + LowerCase(FPlataforma),
          'static_libs' + PathDelim + LowerCase(FPlataforma),
          'obj' + PathDelim + LowerCase(FPlataforma),
          LowerCase(FPlataforma)) do
      Tentar(ExcludeTrailingPathDelimiter(vPasta + vCandidata));
  end;
end;

procedure TBuildDelphi.MontarNamespaces(APacote: TPacote; ALista: TStrings);
var
  vDproj, vItem: string;
  vRegex: TRegExpr;
begin
  ALista.Delimiter := ';';
  ALista.StrictDelimiter := True;
  ALista.DelimitedText := NamespacesPadrao;

  vDproj := ChangeFileExt(APacote.Arquivo, '.dproj');
  if not FileExists(vDproj) then
    Exit;

  vRegex := TRegExpr.Create('<DCC_Namespace>([^<]*)</DCC_Namespace>');
  try
    if vRegex.Exec(LerArquivoTexto(vDproj)) then
      repeat
        for vItem in vRegex.Match[1].Split([';']) do
          if (Trim(vItem) <> '') and (Pos('$(', vItem) = 0) and
             (ALista.IndexOf(Trim(vItem)) < 0) then
            ALista.Add(Trim(vItem));
      until not vRegex.ExecNext;
  finally
    vRegex.Free;
  end;
end;

function TBuildDelphi.CompilarPacote(APacote: TPacote;
  var AResultado: TResultadoPacote): boolean;
var
  vParams, vLU, vBusca, vObjetos, vNamespaces: TStringList;
  vExec: TExecucao;
  vInicio: QWord;
  vArquivoDpk, vFaltando: string;
begin
  AResultado := Default(TResultadoPacote);
  AResultado.Nome := APacote.Nome;

  vArquivoDpk := APacote.Arquivo;
  if not FileExists(vArquivoDpk) then
  begin
    AResultado.Motivo := 'arquivo não encontrado: ' + vArquivoDpk;
    Exit(False);
  end;

  // design-time no Delphi e sempre Win32: para as outras plataformas so faz
  // sentido o pacote de runtime
  if not SameText(FPlataforma, 'win32') and (APacote.Uso = upDesign) then
  begin
    AResultado.Pulado := True;
    AResultado.Ok := True;
    AResultado.Motivo := 'pacote de design-time: só existe em Win32';
    Exit(True);
  end;

  // o catalogo ja sabe o que falta; chamar o compilador para ouvir F1026 e
  // desperdicio, e a mensagem dele nao diz o que fazer
  if APacote.SubmodulosAusentes.Count > 0 then
  begin
    AResultado.Motivo := 'submódulo não baixado: ' +
      StringReplace(APacote.SubmodulosAusentes.CommaText, ',', ', ', [rfReplaceAll]);
    Logar('  ' + AResultado.Motivo + '. O zip do GitHub não traz submódulo: ' +
          'baixe-o antes (o instalador faz isso na etapa de download).');
    Exit(False);
  end;
  if APacote.FontesAusentes.Count > 0 then
  begin
    AResultado.Motivo := 'fonte ausente na árvore: ' +
      StringReplace(APacote.FontesAusentes.CommaText, ',', ', ', [rfReplaceAll]);
    Exit(False);
  end;

  if not GarantirRes(APacote) then
  begin
    AResultado.Motivo := 'não foi possível gerar o .res';
    Exit(False);
  end;

  vParams := TStringList.Create;
  vLU := TStringList.Create;
  vBusca := TStringList.Create;
  vObjetos := TStringList.Create;
  vNamespaces := TStringList.Create;
  vExec := TExecucao.Create;
  try
    MontarLU(APacote, vLU);
    MontarBusca(APacote, vBusca);
    MontarObjetos(APacote, vObjetos);
    MontarNamespaces(APacote, vNamespaces);

    // --no-config: nada da maquina entra sem estar escrito aqui
    vParams.Add('--no-config');
    vParams.Add('-B');
    vParams.Add('-Q');
    vParams.Add('-$D-');
    vParams.Add('-$L-');
    vParams.Add('-$Y-');
    vParams.Add('-NS' + Juntar(vNamespaces, ';'));
    vParams.Add('-U' + Juntar(vBusca, ';'));
    vParams.Add('-I' + Juntar(vBusca, ';'));
    vParams.Add('-R' + Juntar(vBusca, ';'));
    if vObjetos.Count > 0 then
      vParams.Add('-O' + Juntar(vObjetos, ';'));
    if vLU.Count > 0 then
      vParams.Add('-LU' + Juntar(vLU, ';'));
    vParams.Add('-LE' + ExcludeTrailingPathDelimiter(FPastaBpl));
    vParams.Add('-LN' + ExcludeTrailingPathDelimiter(FPastaDcp));
    vParams.Add('-N0' + ExcludeTrailingPathDelimiter(FPastaDcu));
    vParams.Add(ExtractFileName(vArquivoDpk));

    vExec.Executavel := Compilador;
    vExec.Parametros.Assign(vParams);
    vExec.PastaTrabalho := ExtractFilePath(vArquivoDpk);
    vExec.Log := @LogarProcesso;

    if FSimular then
    begin
      Logar('  (simulação) ' + vExec.LinhaComando);
      AResultado.Ok := True;
      AResultado.Pulado := True;
      AResultado.Motivo := 'simulação';
      Exit(True);
    end;

    ForceDirectories(FPastaBpl);
    ForceDirectories(FPastaDcp);
    ForceDirectories(FPastaDcu);

    vInicio := GetTickCount64;
    Result := vExec.Executar;
    AResultado.Segundos := (GetTickCount64 - vInicio) / 1000;

    AResultado.Bpl := IncludeTrailingPathDelimiter(FPastaBpl) +
                      ChangeFileExt(ExtractFileName(vArquivoDpk), '.bpl');
    AResultado.Dcp := IncludeTrailingPathDelimiter(FPastaDcp) +
                      ChangeFileExt(ExtractFileName(vArquivoDpk), '.dcp');

    if not Result then
    begin
      AResultado.Motivo := Format('dcc terminou com código %d', [vExec.CodigoSaida]);
      if vExec.Erro <> '' then
        AResultado.Motivo := vExec.Erro
      else
      begin
        // unidade que falta quase nunca e defeito do RAL: e a biblioteca de
        // terceiro que o recurso exige e que ninguem instalou ainda
        vFaltando := UnidadeFaltando(vExec.Saida);
        if vFaltando <> '' then
        begin
          AResultado.Motivo := 'dependência externa ausente: unidade ' + vFaltando +
                               ' não foi encontrada';
          Logar('  ' + AResultado.Motivo + '. Instale a biblioteca do recurso, ' +
                'aponte a pasta dela, ou desmarque este pacote.');
        end;
      end;
      Exit(False);
    end;

    // o compilador pode terminar em 0 e nao ter gravado nada onde esperamos
    if not FileExists(AResultado.Bpl) then
    begin
      AResultado.Motivo := 'compilou, mas o .bpl não apareceu em ' + FPastaBpl;
      Exit(False);
    end;

    AResultado.TamanhoBpl := TamanhoArquivo(AResultado.Bpl);
    AResultado.Ok := True;

    // .bpl gordo demais para o tamanho do pacote quase sempre significa
    // terceiro linkado estaticamente — a IDE recusa isso com erro de unidade
    // duplicada, e e melhor saber agora
    // ... mas so quando o terceiro *deveria* ter vindo como pacote. O mORMot2
    // e usado por library path, sem pacote nenhum: ali um .bpl grande e o
    // esperado, e avisar seria mentira.
    if (APacote.Unidades.Count <= 6) and (AResultado.TamanhoBpl > 500 * 1024) and
       EsperaPacoteDeTerceiro(APacote) then
      Logar(Format('  AVISO: %s tem %d KB para %d unidade(s): confira o -LU, ' +
                   'pode ter linkado biblioteca de terceiro dentro do pacote',
                   [ExtractFileName(AResultado.Bpl), AResultado.TamanhoBpl div 1024,
                    APacote.Unidades.Count]));
  finally
    vExec.Free;
    vNamespaces.Free;
    vObjetos.Free;
    vBusca.Free;
    vLU.Free;
    vParams.Free;
  end;
end;

function TBuildDelphi.Compilar(APacotes: TList): boolean;
var
  vInt, vDep: integer;
  vPacote: TPacote;
  vResultado: TResultadoPacote;
  vFalhados: TStringList;
  vCulpado: string;
begin
  Result := True;
  SetLength(FResultados, 0);

  if FRaizFontes = '' then
  begin
    Logar('ERRO: os fontes do RAL precisam estar em disco para compilar.');
    Exit(False);
  end;
  if not (ciCompilar in FIDE.Capacidades) then
  begin
    Logar('ERRO: ' + FIDE.Nome + ' não tem compilador de linha de comando utilizável.');
    Exit(False);
  end;
  if not FileExists(Compilador) then
  begin
    Logar('ERRO: compilador não encontrado: ' + Compilador);
    Exit(False);
  end;
  if FIDE.Plataformas.IndexOf(LowerCase(FPlataforma)) < 0 then
  begin
    Logar('ERRO: ' + FIDE.Nome + ' não compila para ' + FPlataforma);
    Exit(False);
  end;

  // saida padrao: onde a IDE procura o que o usuario instalou
  if (FPastaBpl = '') and (FIDE.CommonDir <> '') then
    FPastaBpl := PastaBplPadrao;
  if (FPastaDcp = '') and (FIDE.CommonDir <> '') then
    FPastaDcp := PastaDcpPadrao;
  if (FPastaBpl = '') or (FPastaDcp = '') then
  begin
    Logar('ERRO: não sei onde gravar .bpl/.dcp desta IDE (BDSCOMMONDIR vazio).');
    Exit(False);
  end;

  if FPastaDcu = '' then
    FPastaDcu := FRaizFontes + 'compiled' + PathDelim + 'delphi' + PathDelim +
                 FIDE.BDSVersao + PathDelim + LowerCase(FPlataforma);

  Logar(Format('Compilando %d pacote(s) em %s (%s)',
               [APacotes.Count, FIDE.Nome, FPlataforma]));
  Logar('  .bpl -> ' + FPastaBpl);
  Logar('  .dcp -> ' + FPastaDcp);

  vFalhados := TStringList.Create;
  try
    vFalhados.CaseSensitive := False;
    for vInt := 0 to Pred(APacotes.Count) do
    begin
      vPacote := TPacote(APacotes[vInt]);
      if vPacote.Tipo <> tpDelphi then
        Continue;

      Logar(Format('[%d/%d] %s', [vInt + 1, APacotes.Count, vPacote.Nome]));

      // pacote que falhou derruba quem depende dele, e só quem depende dele:
      // compilar o resto gera erro em cascata e esconde a causa, mas parar a
      // rodada inteira perde os pacotes que não têm nada com isso
      vCulpado := '';
      for vDep := 0 to Pred(vPacote.Internos.Count) do
        if vFalhados.IndexOf(vPacote.Internos[vDep]) >= 0 then
        begin
          vCulpado := vPacote.Internos[vDep];
          Break;
        end;

      if vCulpado <> '' then
      begin
        vResultado := Default(TResultadoPacote);
        vResultado.Nome := vPacote.Nome;
        vResultado.Pulado := True;
        vResultado.Motivo := 'depende de ' + vCulpado + ', que falhou';
        Logar('  pulado: ' + vResultado.Motivo);
        vFalhados.Add(vPacote.Nome);
      end
      else if not CompilarPacote(vPacote, vResultado) then
      begin
        Logar('  FALHOU: ' + vResultado.Motivo);
        vFalhados.Add(vPacote.Nome);
        Result := False;
      end
      else if vResultado.Pulado then
        Logar('  pulado: ' + vResultado.Motivo)
      else
        Logar(Format('  ok: %s (%d KB, %.1f s)',
                     [ExtractFileName(vResultado.Bpl),
                      vResultado.TamanhoBpl div 1024, vResultado.Segundos]));

      SetLength(FResultados, Length(FResultados) + 1);
      FResultados[High(FResultados)] := vResultado;
    end;
  finally
    vFalhados.Free;
  end;
end;

function TBuildDelphi.Relatorio: string;
var
  vInt: integer;
  vLista: TStringList;
  vEstado: string;
begin
  vLista := TStringList.Create;
  try
    vLista.Add(Format('%-24s %-10s %-10s %s', ['pacote', 'resultado', 'tamanho', 'observação']));
    for vInt := 0 to Pred(Total) do
    begin
      if not FResultados[vInt].Ok then
        vEstado := 'FALHOU'
      else if FResultados[vInt].Pulado then
        vEstado := 'pulado'
      else
        vEstado := 'ok';
      vLista.Add(Format('%-24s %-10s %7d KB %s',
        [FResultados[vInt].Nome, vEstado, FResultados[vInt].TamanhoBpl div 1024,
         FResultados[vInt].Motivo]));
    end;
    Result := vLista.Text;
  finally
    vLista.Free;
  end;
end;

end.
