unit ufrm_recursos;

{$mode ObjFPC}{$H+}

// Versao do RAL, pasta de instalacao e recursos (F8 + F2).
//
// A versao vem do GitHub — a estavel mais recente ja vem escolhida — e o
// catalogo dela e lido do zip da versao, guardado no cache: as opcoes
// aparecem sem nada ter sido gravado na pasta do usuario. O download de
// verdade, para <pasta>/PascalRAL/<versao>, so acontece na execucao (§0 do
// plano). A ultima opcao da lista e uma pasta local com os fontes, o caso de
// quem desenvolve o proprio RAL: nada e baixado.
//
// Marcar um pacote marca o que ele exige; o que entra so por dependencia
// aparece como "necessario".

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ComCtrls,
  Buttons, ufrm_modelo, LCLType, ExtCtrls,
  ideutils, RALInst.IDE, RALInst.Catalogo, RALInst.GitHub, RALInst.Processo, RALInst.Receitas,
  RALInst.Compatibilidade;

type
  // a tela das IDEs diz quais estao marcadas (TIDEObjectData)
  TListarIDEs = procedure(ALista: TList) of object;

  TTiposPacote = set of TTipoPacote;

  { Tfrm_recursos }

  Tfrm_recursos = class(Tfrm_modelo)
    bAddVersion: TSpeedButton;
    cbVersao: TComboBox;
    ckSomentePaths: TCheckBox;
    ckWin64: TCheckBox;
    dirSelect: TSelectDirectoryDialog;
    Label2: TLabel;
    lbDestino: TLabel;
    lbInfo: TLabel;
    lbSomentePaths: TLabel;
    lbVersao: TLabel;
    lbWin64: TLabel;
    lbedDownloadPath: TLabeledEdit;
    lbSubTitle: TLabel;
    tvRecursos: TTreeView;
    procedure bAddVersionClick(Sender: TObject);
    procedure cbVersaoChange(Sender: TObject);
    procedure lbedDownloadPathEditingDone(Sender: TObject);
    procedure lbSomentePathsClick(Sender: TObject);
    procedure lbWin64Click(Sender: TObject);
    procedure tvRecursosDblClick(Sender: TObject);
    procedure tvRecursosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FRepo: TRepoGitHub;
    FVersoes: TVersoesRAL;
    FVersoesCarregadas: boolean;
    FModoLocal: boolean;
    // o catalogo das telas: do zip da versao, ou da pasta local
    FCatalogo: TCatalogo;
    // o da pasta final, depois do download: e com ele que se instala
    FCatalogoInstalar: TCatalogo;
    FCarregado: string;
    FErroCatalogo: string;
    FPastaBase: string;
    FPastaLocal: string;
    // o que o usuario marcou; as dependencias sao calculadas a cada desenho
    FEscolhidos: TStringList;
    FEscolha: TEscolhaInstalacao;
    FUltimoProgresso: int64;
    // F7: as receitas (embutidas + pastas 'receitas') e as dependencias que a
    // rodada vai baixar (nome=pasta)
    FReceitas: TReceitas;
    FDependencias: TStringList;
    // F6: o manifesto da versao escolhida e a conferencia de compatibilidade
    // com as IDEs marcadas; refeita quando a versao ou as IDEs mudam
    FManifesto: TManifesto;
    FCompat: TCompatibilidade;
    FOnListarIDEs: TListarIDEs;
    // raiz da IDE|receita=onde ja esta ('' se nao esta): perguntar ao registro
    // a cada desenho da arvore seria lento
    FDeteccoes: TStringList;
    FIDEsVistas: string;
    // os tipos de IDE da rodada: Delphi, Lazarus ou os dois (F9)
    function Tipos: TTiposPacote;
    // as IDEs marcadas de um tipo; todas as marcadas
    procedure IDEsDoTipo(ALista: TList; ATipo: TTipoPacote);
    procedure IDEsMarcadas(ALista: TList);
    // os escolhidos que existem naquele tipo, com o nome do catalogo
    // (IndyRAL escolhido vale indyral no Lazarus)
    procedure NomesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
    // o mesmo recurso no outro tipo de IDE; nil se nao ha, ou se a rodada e
    // de um tipo so
    function Par(APacote: TPacote): TPacote;
    // o motivo quando nao da para marcar: indisponivel em todo tipo da rodada
    function IndisponivelEmTodos(APacote: TPacote): string;
    function AssinaturaIDEs: string;
    function Compat: TCompatibilidade;
    function Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
    function PastaInstalada(AIDE: TIDEInstance; AReceita: TReceita): string;
    // '' se cabe em todas as IDEs marcadas; senao 'Delphi XE2: motivo' por IDE
    // (ATodas diz se nao cabe em nenhuma)
    function ForaDasIDEs(APacote: TPacote; out ATodas: boolean): string;
    function Versao: TVersaoRAL;
    function NomeGrupo(const AGrupo: string): string;
    function Indisponivel(APacote: TPacote): string;
    procedure CarregarVersoes;
    procedure TrocarModo;
    procedure CarregarCatalogo;
    procedure AtualizarDestino;
    procedure EscolherPadrao;
    procedure MontarArvore;
    procedure Alternar(APacote: TPacote; AMarcar: boolean);
    procedure AtualizarOpcoes;
    procedure Progresso(const ALidos, ATotal: int64);
    procedure SubmodulosDaEscolha(ALista: TStrings);
  protected
    procedure SetIDE(AValue: integer); override;
    function validatePageNext : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AoMostrar; override;
    // onde os fontes estao, ou vao estar depois do download
    function PastaFontes: string;
    // a escolha completa para os motores de instalacao
    function Escolha: TEscolhaInstalacao;
    // a parte do plano que diz de onde vem o RAL
    function PlanoFontes: string;
    // etapa 1 da execucao: os fontes na pasta final (baixa se preciso); nada
    // de IDE e tocado se falhar
    function PrepararFontes(ALog: TLogLinha): boolean;
    // F7: decide o que baixar (so o que alguma IDE marcada nao tem, na versao
    // que cada uma pede — F6) e devolve essa parte do plano; as IDEs passam a
    // saber para onde cada uma vai
    function PlanejarDependencias: string;
    // etapa 1b: baixa o que foi planejado; o que falhar some da escolha, e os
    // pacotes que dependiam dela ficam de fora na instalacao
    function BaixarDependencias(ALog: TLogLinha): boolean;
    property OnListarIDEs: TListarIDEs read FOnListarIDEs write FOnListarIDEs;
  published
    property Catalogo: TCatalogo read FCatalogo;
  end;

implementation

{$R *.lfm}

uses
  StrUtils, udm, RALInst.Zip, RALInst.Fontes, RALInst.Dependencias;

const
  ImgMarcado = 3;
  ImgDesmarcado = 4;
  TextoLocal = 'Pasta local com os fontes (desenvolvimento do RAL)';

{ Tfrm_recursos }

function Tfrm_recursos.Tipos: TTiposPacote;
begin
  // 0 - Delphi, 1 - Lazarus, 2 - os dois
  case IDE of
    0: Result := [tpDelphi];
    1: Result := [tpLazarus];
    2: Result := [tpDelphi, tpLazarus];
  else
    Result := [];
  end;
end;

procedure Tfrm_recursos.NomesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
var
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  for vInt := 0 to Pred(FEscolhidos.Count) do
  begin
    vPacote := FCatalogo.Buscar(ATipo, FEscolhidos[vInt]);
    if (vPacote <> nil) and (ALista.IndexOf(vPacote.Nome) < 0) then
      ALista.Add(vPacote.Nome);
  end;
end;

function Tfrm_recursos.Par(APacote: TPacote): TPacote;
begin
  Result := nil;
  if Tipos <> [tpDelphi, tpLazarus] then
    Exit;
  if APacote.Tipo = tpDelphi then
    Result := FCatalogo.Buscar(tpLazarus, APacote.Nome)
  else
    Result := FCatalogo.Buscar(tpDelphi, APacote.Nome);
end;

function Tfrm_recursos.IndisponivelEmTodos(APacote: TPacote): string;
var
  vPar: TPacote;
begin
  Result := Indisponivel(APacote);
  if Result = '' then
    Exit;
  vPar := Par(APacote);
  if (vPar <> nil) and (Indisponivel(vPar) = '') then
    Result := '';
end;

function Tfrm_recursos.Versao: TVersaoRAL;
begin
  Result := nil;
  if (cbVersao.ItemIndex >= 0) and not FModoLocal then
    Result := TVersaoRAL(cbVersao.Items.Objects[cbVersao.ItemIndex]);
end;

function Tfrm_recursos.NomeGrupo(const AGrupo: string): string;
var
  vRaiz: string;
begin
  vRaiz := AGrupo;
  if Pos('/', vRaiz) > 0 then
    vRaiz := Copy(vRaiz, 1, Pos('/', vRaiz) - 1);

  if vRaiz = '' then
    Result := 'Pacotes principais'
  else if SameText(vRaiz, 'Engine') then
    Result := 'Motores HTTP (engines)'
  else if SameText(vRaiz, 'Database') then
    Result := 'Banco de dados'
  else if SameText(vRaiz, 'compression') then
    Result := 'Compressão'
  else
    Result := vRaiz;
end;

function Tfrm_recursos.Indisponivel(APacote: TPacote): string;
var
  vTodas: boolean;
  vFora: string;
begin
  Result := '';
  // do zip da versao, submodulo "ausente" e so um download a mais
  if (APacote.SubmodulosAusentes.Count > 0) and not FCatalogo.Origem.BaixaSubmodulos then
    Result := 'falta o submódulo ' + APacote.SubmodulosAusentes.CommaText +
              ' (git submodule update --init)'
  else if APacote.FontesAusentes.Count > 0 then
    Result := 'falta o fonte ' + APacote.FontesAusentes[0]
  else if APacote.Ordem < 0 then
    Result := 'dependência circular'
  else
  begin
    // F6: nao cabe em nenhuma das IDEs marcadas
    vFora := ForaDasIDEs(APacote, vTodas);
    if vTodas then
      Result := vFora;
  end;
end;

procedure Tfrm_recursos.IDEsMarcadas(ALista: TList);
begin
  ALista.Clear;
  if Assigned(FOnListarIDEs) then
    FOnListarIDEs(ALista);
end;

procedure Tfrm_recursos.IDEsDoTipo(ALista: TList; ATipo: TTipoPacote);
var
  vInt: integer;
  vTipo: TTipoIDE;
begin
  IDEsMarcadas(ALista);
  if ATipo = tpDelphi then
    vTipo := tiDelphi
  else
    vTipo := tiLazarus;
  for vInt := Pred(ALista.Count) downto 0 do
    if TIDEObjectData(ALista[vInt]).Instancia.Tipo <> vTipo then
      ALista.Delete(vInt);
end;

function Tfrm_recursos.AssinaturaIDEs: string;
var
  vLista: TList;
  vInt: integer;
begin
  Result := '';
  vLista := TList.Create;
  try
    IDEsMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      Result := Result + TIDEObjectData(vLista[vInt]).Instancia.RootDir + ';';
  finally
    vLista.Free;
  end;
end;

function Tfrm_recursos.Compat: TCompatibilidade;
begin
  if FCompat = nil then
  begin
    FCompat := TCompatibilidade.Create(FCatalogo, FManifesto, FReceitas);
    FCompat.Detectar := @Detectar;
    FCompat.PastaInstalada := @PastaInstalada;
  end;
  Result := FCompat;
end;

function Tfrm_recursos.Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
var
  vLista: TList;
  vInt, vIdx: integer;
  vChave: string;
begin
  vChave := AIDE.RootDir + '|' + AReceita.Nome;
  vIdx := FDeteccoes.IndexOfName(vChave);
  if vIdx >= 0 then
    Exit(FDeteccoes.ValueFromIndex[vIdx]);
  Result := '';
  vLista := TList.Create;
  try
    IDEsMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if TIDEObjectData(vLista[vInt]).Instancia = AIDE then
        Result := TIDEObjectData(vLista[vInt]).DependenciaInstalada(AReceita, Escolha);
  finally
    vLista.Free;
  end;
  FDeteccoes.Add(vChave + '=' + Result);
end;

function Tfrm_recursos.PastaInstalada(AIDE: TIDEInstance; AReceita: TReceita): string;
var
  vLista: TList;
  vInt: integer;
begin
  Result := '';
  vLista := TList.Create;
  try
    IDEsMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if TIDEObjectData(vLista[vInt]).Instancia = AIDE then
        Result := TIDEObjectData(vLista[vInt]).PastaDependencia(AReceita, Escolha);
  finally
    vLista.Free;
  end;
end;

function Tfrm_recursos.ForaDasIDEs(APacote: TPacote; out ATodas: boolean): string;
var
  vLista: TList;
  vInt, vFora: integer;
  vMotivo: string;
  vIDE: TIDEInstance;
begin
  Result := '';
  ATodas := False;
  vFora := 0;
  vLista := TList.Create;
  try
    // as IDEs do tipo do pacote: o IndyRAL.dpk nao tem nada com o Lazarus
    IDEsDoTipo(vLista, APacote.Tipo);
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vIDE := TIDEObjectData(vLista[vInt]).Instancia;
      vMotivo := Compat.Motivo(APacote, vIDE);
      if vMotivo = '' then
        Continue;
      Inc(vFora);
      if Result <> '' then
        Result := Result + '; ';
      // uma IDE so: o motivo ja cita o nome dela quase sempre
      if vLista.Count = 1 then
        Result := vMotivo
      else
        Result := Result + vIDE.Nome + ': ' + vMotivo;
    end;
    ATodas := (vFora > 0) and (vFora = vLista.Count);
  finally
    vLista.Free;
  end;
end;

procedure Tfrm_recursos.Progresso(const ALidos, ATotal: int64);
begin
  if (ALidos - FUltimoProgresso < 256 * 1024) and (ALidos <> ATotal) then
    Exit;
  FUltimoProgresso := ALidos;
  if ATotal > 0 then
    lbInfo.Caption := Format('baixando... %.1f de %.1f MB', [ALidos / 1048576, ATotal / 1048576])
  else
    lbInfo.Caption := Format('baixando... %.1f MB', [ALidos / 1048576]);
  Application.ProcessMessages;
end;

procedure Tfrm_recursos.CarregarVersoes;
var
  vInt: integer;
begin
  FVersoesCarregadas := True;
  cbVersao.Items.Clear;
  lbInfo.Caption := 'Consultando as versões no GitHub...';
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  try
    if FRepo.ListarVersoes(FVersoes) then
      for vInt := 0 to Pred(FVersoes.Count) do
        cbVersao.Items.AddObject(FVersoes[vInt].Descricao, FVersoes[vInt]);
  finally
    Screen.Cursor := crDefault;
  end;
  cbVersao.Items.AddObject(TextoLocal, nil);

  if FVersoes.Count = 0 then
  begin
    // sem rede e sem cache: sobra a pasta local
    cbVersao.ItemIndex := cbVersao.Items.Count - 1;
    lbInfo.Caption := '';
    ShowMessage('Não foi possível consultar as versões do PascalRAL: ' + FRepo.Erro +
                LineEnding + LineEnding + 'Só dá para instalar a partir de uma pasta local.');
  end
  else
  begin
    // a primeira e a estavel mais recente
    cbVersao.ItemIndex := 0;
    if FRepo.Aviso <> '' then
      lbInfo.Caption := FRepo.Aviso;
  end;
  TrocarModo;
end;

procedure Tfrm_recursos.TrocarModo;
begin
  FModoLocal := (cbVersao.ItemIndex >= 0) and
                (cbVersao.Items.Objects[cbVersao.ItemIndex] = nil);
  if FModoLocal then
  begin
    lbedDownloadPath.EditLabel.Caption := 'Pasta dos fontes do PascalRAL (a que contém pkg e src)';
    lbedDownloadPath.Text := FPastaLocal;
  end
  else
  begin
    lbedDownloadPath.EditLabel.Caption := 'Pasta de instalação (os fontes do RAL e das dependências ficam nela)';
    lbedDownloadPath.Text := FPastaBase;
  end;
  AtualizarDestino;
  CarregarCatalogo;
end;

function Tfrm_recursos.PastaFontes: string;
var
  vVersao: TVersaoRAL;
begin
  vVersao := Versao;
  if FModoLocal then
    Result := FPastaLocal
  else if (vVersao <> nil) and (FPastaBase <> '') then
    Result := IncludeTrailingPathDelimiter(FPastaBase) + 'PascalRAL' + PathDelim +
              vVersao.NomePasta
  else
    Result := '';
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

procedure Tfrm_recursos.AtualizarDestino;
begin
  if FModoLocal then
    lbDestino.Caption := 'Os fontes são usados onde estão; nada é baixado.'
  else if PastaFontes <> '' then
    lbDestino.Caption := 'O RAL fica em ' + ExcludeTrailingPathDelimiter(PastaFontes) +
                         '. Pasta permanente: as IDEs passam a apontar para ela.'
  else
    lbDestino.Caption := 'Escolha a pasta onde o PascalRAL será instalado.';
  lbDestino.Hint := lbDestino.Caption;
  lbDestino.ShowHint := True;
end;

procedure Tfrm_recursos.CarregarCatalogo;
var
  vChave, vZip, vPasta: string;
  vVersao: TVersaoRAL;
  vAntes: TStringList;
  vInt: integer;
begin
  vVersao := Versao;
  if FModoLocal then
    vChave := 'local:' + Trim(FPastaLocal)
  else if vVersao <> nil then
    vChave := 'github:' + vVersao.Ref
  else
    vChave := '';
  if SameText(vChave, FCarregado) then
    Exit;
  FCarregado := vChave;
  FErroCatalogo := '';
  FreeAndNil(FCatalogoInstalar);
  FreeAndNil(FCompat);
  FManifesto.Limpar;

  // o que o usuario tinha marcado sobrevive a troca de versao, no que existir
  vAntes := TStringList.Create;
  try
    vAntes.Assign(FEscolhidos);
    FCatalogo.Limpar;
    FEscolhidos.Clear;
    tvRecursos.Items.Clear;

    Screen.Cursor := crHourGlass;
    try
      if FModoLocal then
      begin
        vPasta := Trim(FPastaLocal);
        if vPasta = '' then
          FErroCatalogo := 'Escolha a pasta dos fontes do PascalRAL.'
        else if not DirectoryExists(vPasta) then
          FErroCatalogo := 'Pasta não encontrada.'
        else if not FCatalogo.Carregar(vPasta) then
          FErroCatalogo := 'Nenhum pacote do RAL nesta pasta (ela deve conter pkg e src).';
      end
      else if vVersao <> nil then
      begin
        // o zip da versao vai para o cache; o ramo muda, a tag nao
        FUltimoProgresso := 0;
        lbInfo.Caption := 'Lendo a versão ' + vVersao.Ref + '...';
        Application.ProcessMessages;
        if not FRepo.BaixarZip(vVersao.Ref, vVersao.Tipo = tvRamo, vZip) then
          FErroCatalogo := FRepo.Erro
        else
          try
            if not FCatalogo.Carregar(TOrigemZip.Create(vZip, 'PascalRAL ' + vVersao.Ref)) then
              FErroCatalogo := 'A versão ' + vVersao.Ref + ' não tem pacotes do RAL.';
          except
            on E: Exception do
              FErroCatalogo := 'Não foi possível ler a versão ' + vVersao.Ref + ': ' + E.Message;
          end;
      end;
    finally
      Screen.Cursor := crDefault;
    end;

    if FErroCatalogo <> '' then
    begin
      FCatalogo.Limpar;
      lbInfo.Caption := FErroCatalogo;
      Exit;
    end;

    // F6: o manifesto e o desta versao (ralinstaller.json nela), senao o
    // embutido no instalador
    FManifesto.CarregarPadrao(FCatalogo.Origem);

    for vInt := 0 to Pred(vAntes.Count) do
      if ((tpDelphi in Tipos) and (FCatalogo.Buscar(tpDelphi, vAntes[vInt]) <> nil)) or
         ((tpLazarus in Tipos) and (FCatalogo.Buscar(tpLazarus, vAntes[vInt]) <> nil)) then
        FEscolhidos.Add(vAntes[vInt]);
    if FEscolhidos.Count = 0 then
      EscolherPadrao;
    MontarArvore;
  finally
    vAntes.Free;
  end;
end;

procedure Tfrm_recursos.EscolherPadrao;
var
  vLista: TList;
  vInt: integer;
  vPacote: TPacote;
  vTipo: TTipoPacote;
begin
  // o nucleo do RAL (pacotes da raiz de pkg/<IDE>), menos o assistente, de
  // cada tipo de IDE da rodada; o nome vale para os dois (sem caixa)
  FEscolhidos.Clear;
  vLista := TList.Create;
  try
    for vTipo in Tipos do
    begin
      FCatalogo.Listar(vTipo, vLista);
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vPacote := TPacote(vLista[vInt]);
        if (vPacote.Grupo = '') and (Pos('wizard', LowerCase(vPacote.Nome)) = 0) and
           (Indisponivel(vPacote) = '') and (FEscolhidos.IndexOf(vPacote.Nome) < 0) then
          FEscolhidos.Add(vPacote.Nome);
      end;
    end;
  finally
    vLista.Free;
  end;
end;

procedure Tfrm_recursos.MontarArvore;
var
  vLista, vFechamento: TList;
  vGrupos, vChaves, vNomes: TStringList;
  vInt, vIdx, vMarcados: integer;
  vPacote, vPar: TPacote;
  vGrupo, vNo: TTreeNode;
  vTexto, vMotivo, vFora: string;
  vMarcado, vTodas, vDoisTipos: boolean;
  vTipo: TTipoPacote;

  function NoFechamento(APac: TPacote): boolean;
  begin
    Result := (APac <> nil) and (vFechamento.IndexOf(APac) >= 0);
  end;

begin
  tvRecursos.Items.BeginUpdate;
  vLista := TList.Create;
  vFechamento := TList.Create;
  vGrupos := TStringList.Create;
  // um no por nome (IndyRAL e indyral sao o mesmo recurso); Objects = o
  // primeiro pacote achado com o nome
  vChaves := TStringList.Create;
  vChaves.CaseSensitive := False;
  vNomes := TStringList.Create;
  try
    tvRecursos.Items.Clear;
    FreeAndNil(FCatalogoInstalar);
    vDoisTipos := Tipos = [tpDelphi, tpLazarus];
    // o que ja entra (escolhido ou exigido), de cada tipo
    for vTipo in Tipos do
    begin
      FCatalogo.Listar(vTipo, vLista);
      for vInt := 0 to Pred(vLista.Count) do
        if vChaves.IndexOf(TPacote(vLista[vInt]).Nome) < 0 then
          vChaves.AddObject(TPacote(vLista[vInt]).Nome, TPacote(vLista[vInt]));
      NomesDoTipo(vTipo, vNomes);
      FCatalogo.Fechamento(vTipo, vNomes, vLista);
      for vInt := 0 to Pred(vLista.Count) do
        vFechamento.Add(vLista[vInt]);
    end;

    vMarcados := 0;
    for vInt := 0 to Pred(vChaves.Count) do
    begin
      vPacote := TPacote(vChaves.Objects[vInt]);
      vPar := Par(vPacote);

      vIdx := vGrupos.IndexOf(NomeGrupo(vPacote.Grupo));
      if vIdx < 0 then
      begin
        vGrupo := tvRecursos.Items.Add(nil, NomeGrupo(vPacote.Grupo));
        vGrupos.AddObject(NomeGrupo(vPacote.Grupo), vGrupo);
      end
      else
        vGrupo := TTreeNode(vGrupos.Objects[vIdx]);

      vTexto := vPacote.Nome;
      if vPacote.Descricao <> '' then
        vTexto := vTexto + '  —  ' + vPacote.Descricao
      else if (vPar <> nil) and (vPar.Descricao <> '') then
        vTexto := vTexto + '  —  ' + vPar.Descricao;
      // com as duas IDEs, o que so existe de um lado diz qual
      if vDoisTipos and (vPar = nil) then
        if vPacote.Tipo = tpDelphi then
          vTexto := vTexto + '  (só Delphi)'
        else
          vTexto := vTexto + '  (só Lazarus)';

      vMarcado := NoFechamento(vPacote) or NoFechamento(vPar);
      if vMarcado then
        Inc(vMarcados);
      vMotivo := IndisponivelEmTodos(vPacote);
      if vMotivo <> '' then
        vTexto := vTexto + '  [indisponível: ' + vMotivo + ']'
      else
      begin
        if vMarcado and (FEscolhidos.IndexOf(vPacote.Nome) < 0) then
          vTexto := vTexto + '  (necessário)';
        // cabe em parte das IDEs marcadas: nas outras fica de fora
        vFora := ForaDasIDEs(vPacote, vTodas);
        if vPar <> nil then
        begin
          vMotivo := ForaDasIDEs(vPar, vTodas);
          if vMotivo <> '' then
            if vFora <> '' then
              vFora := vFora + '; ' + vMotivo
            else
              vFora := vMotivo;
        end;
        if vFora <> '' then
          vTexto := vTexto + '  [fica de fora em ' + vFora + ']';
      end;

      vNo := tvRecursos.Items.AddChild(vGrupo, vTexto);
      vNo.Data := vPacote;
      if vMarcado then
        vNo.ImageIndex := ImgMarcado
      else
        vNo.ImageIndex := ImgDesmarcado;
      vNo.SelectedIndex := vNo.ImageIndex;
    end;

    tvRecursos.FullExpand;
    lbInfo.Caption := Format('%d recurso(s) nesta versão; %d marcado(s)',
                             [vChaves.Count, vMarcados]);
    lbInfo.ShowHint := FCatalogo.Erros.Count > 0;
    if FCatalogo.Erros.Count > 0 then
    begin
      lbInfo.Caption := lbInfo.Caption + Format(' — %d aviso(s)', [FCatalogo.Erros.Count]);
      lbInfo.Hint := FCatalogo.Erros.Text;
    end;
  finally
    vNomes.Free;
    vChaves.Free;
    vGrupos.Free;
    vFechamento.Free;
    vLista.Free;
    tvRecursos.Items.EndUpdate;
  end;
end;

procedure Tfrm_recursos.Alternar(APacote: TPacote; AMarcar: boolean);
var
  vFechamento: TList;
  vNomes: TStringList;
  vMotivo: string;
  vInt, vDep: integer;
  vQuem: string;
  vTipo: TTipoPacote;
begin
  vMotivo := IndisponivelEmTodos(APacote);
  if AMarcar and (vMotivo <> '') then
  begin
    ShowMessage(APacote.Nome + ' está indisponível: ' + vMotivo + '.');
    Exit;
  end;

  if AMarcar then
  begin
    if FEscolhidos.IndexOf(APacote.Nome) < 0 then
      FEscolhidos.Add(APacote.Nome);
  end
  else
  begin
    vInt := FEscolhidos.IndexOf(APacote.Nome);
    if vInt >= 0 then
      FEscolhidos.Delete(vInt);

    // continua marcado se outro escolhido depende dele: dizer quem
    vFechamento := TList.Create;
    vNomes := TStringList.Create;
    try
      vQuem := '';
      for vTipo in Tipos do
      begin
        NomesDoTipo(vTipo, vNomes);
        FCatalogo.Fechamento(vTipo, vNomes, vFechamento);
        for vInt := 0 to Pred(vFechamento.Count) do
          for vDep := 0 to Pred(TPacote(vFechamento[vInt]).Internos.Count) do
            if SameText(TPacote(vFechamento[vInt]).Internos[vDep], APacote.Nome) and
               (Pos(' ' + TPacote(vFechamento[vInt]).Nome, vQuem) = 0) then
              vQuem := vQuem + ' ' + TPacote(vFechamento[vInt]).Nome;
      end;
      if vQuem <> '' then
        ShowMessage(APacote.Nome + ' continua marcado: é necessário para' + vQuem + '.');
    finally
      vNomes.Free;
      vFechamento.Free;
    end;
  end;

  MontarArvore;
end;

procedure Tfrm_recursos.AtualizarOpcoes;
var
  vDelphi: boolean;
begin
  // as duas opcoes so existem no Delphi
  {$IFDEF MSWINDOWS}
  vDelphi := (IDE = 0) or (IDE = 2);
  {$ELSE}
  vDelphi := False;
  {$ENDIF}
  ckSomentePaths.Visible := vDelphi;
  lbSomentePaths.Visible := vDelphi;
  ckWin64.Visible := vDelphi;
  lbWin64.Visible := vDelphi;
end;

procedure Tfrm_recursos.bAddVersionClick(Sender: TObject);
begin
  if DirectoryExists(lbedDownloadPath.Text) then
    dirSelect.InitialDir := lbedDownloadPath.Text;
  if dirSelect.Execute then
  begin
    lbedDownloadPath.Text := dirSelect.FileName;
    lbedDownloadPathEditingDone(nil);
  end;
end;

procedure Tfrm_recursos.cbVersaoChange(Sender: TObject);
begin
  TrocarModo;
end;

procedure Tfrm_recursos.lbedDownloadPathEditingDone(Sender: TObject);
begin
  if FModoLocal then
  begin
    FPastaLocal := Trim(lbedDownloadPath.Text);
    CarregarCatalogo;
  end
  else
  begin
    FPastaBase := Trim(lbedDownloadPath.Text);
    FreeAndNil(FCatalogoInstalar);
  end;
  AtualizarDestino;
end;

procedure Tfrm_recursos.lbSomentePathsClick(Sender: TObject);
begin
  ckSomentePaths.Checked := not ckSomentePaths.Checked;
end;

procedure Tfrm_recursos.lbWin64Click(Sender: TObject);
begin
  ckWin64.Checked := not ckWin64.Checked;
end;

procedure Tfrm_recursos.tvRecursosDblClick(Sender: TObject);
var
  vNode: TTreeNode;
begin
  vNode := tvRecursos.Selected;
  if (vNode = nil) or (vNode.Data = nil) then
    Exit;
  Alternar(TPacote(vNode.Data), vNode.ImageIndex <> ImgMarcado);
end;

procedure Tfrm_recursos.tvRecursosKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  vNode: TTreeNode;
begin
  vNode := tvRecursos.Selected;
  if (vNode = nil) or (vNode.Data = nil) then
    Exit;

  case Key of
    VK_INSERT: Alternar(TPacote(vNode.Data), True);
    VK_DELETE: Alternar(TPacote(vNode.Data), False);
    VK_SPACE:  Alternar(TPacote(vNode.Data), vNode.ImageIndex <> ImgMarcado);
  else
    Exit;
  end;
  Key := 0;
end;

procedure Tfrm_recursos.SetIDE(AValue: integer);
var
  vMudou: boolean;
begin
  vMudou := AValue <> IDE;
  inherited SetIDE(AValue);
  AtualizarOpcoes;
  // os nomes dos pacotes mudam de uma IDE para a outra
  if vMudou and (FCatalogo.Count > 0) then
  begin
    EscolherPadrao;
    MontarArvore;
  end;
end;

procedure Tfrm_recursos.AoMostrar;
begin
  if not FVersoesCarregadas then
    CarregarVersoes;
  // voltou da tela das IDEs com outra escolha: o que cabe em cada uma muda
  if AssinaturaIDEs <> FIDEsVistas then
  begin
    FIDEsVistas := AssinaturaIDEs;
    FreeAndNil(FCompat);
    FDeteccoes.Clear;
    if FCatalogo.Count > 0 then
      MontarArvore;
  end;
end;

function Tfrm_recursos.validatePageNext: boolean;
begin
  Result := False;
  if not FModoLocal then
    FPastaBase := Trim(lbedDownloadPath.Text)
  else
    FPastaLocal := Trim(lbedDownloadPath.Text);
  CarregarCatalogo;
  if FCatalogo.Count = 0 then
  begin
    if FErroCatalogo <> '' then
      ShowMessage(FErroCatalogo)
    else
      ShowMessage('Escolha a versão do PascalRAL.');
    Exit;
  end;
  if not FModoLocal then
  begin
    if FPastaBase = '' then
    begin
      ShowMessage('Escolha a pasta de instalação.');
      Exit;
    end;
    if PastaDeOutro(PastaFontes) then
    begin
      ShowMessage('A pasta ' + ExcludeTrailingPathDelimiter(PastaFontes) +
                  ' já existe e não foi criada pelo instalador. Escolha outra pasta de instalação.');
      Exit;
    end;
  end;
  if FEscolhidos.Count = 0 then
  begin
    ShowMessage('Marque ao menos um recurso.');
    Exit;
  end;
  Result := True;
end;

procedure Tfrm_recursos.SubmodulosDaEscolha(ALista: TStrings);
var
  vFechamento: TList;
  vNomes: TStringList;
  vInt, vSub: integer;
  vTipo: TTipoPacote;
begin
  ALista.Clear;
  vFechamento := TList.Create;
  vNomes := TStringList.Create;
  try
    // os submodulos de todos os tipos de IDE da rodada: a pasta e uma so
    for vTipo in Tipos do
    begin
      NomesDoTipo(vTipo, vNomes);
      FCatalogo.Fechamento(vTipo, vNomes, vFechamento);
      for vInt := 0 to Pred(vFechamento.Count) do
        for vSub := 0 to Pred(TPacote(vFechamento[vInt]).Submodulos.Count) do
          if ALista.IndexOf(TPacote(vFechamento[vInt]).Submodulos[vSub]) < 0 then
            ALista.Add(TPacote(vFechamento[vInt]).Submodulos[vSub]);
    end;
  finally
    vNomes.Free;
    vFechamento.Free;
  end;
end;

function Tfrm_recursos.PlanoFontes: string;
var
  vVersao: TVersaoRAL;
  vSubs: TStringList;
begin
  vVersao := Versao;
  if FModoLocal or (vVersao = nil) then
    Exit('Fontes do RAL: ' + ExcludeTrailingPathDelimiter(PastaFontes) +
         ' (pasta local, nada é baixado)' + LineEnding);

  vSubs := TStringList.Create;
  try
    SubmodulosDaEscolha(vSubs);
    Result := 'Baixar o PascalRAL ' + vVersao.Ref;
    if (FCatalogo.Origem is TOrigemZip) and (TOrigemZip(FCatalogo.Origem).Commit <> '') then
      Result := Result + ' (commit ' + Copy(TOrigemZip(FCatalogo.Origem).Commit, 1, 7) + ')';
    Result := Result + ' para ' + ExcludeTrailingPathDelimiter(PastaFontes) + LineEnding;
    if vSubs.Count > 0 then
      Result := Result + '  com os submódulos: ' + vSubs.CommaText + LineEnding;
  finally
    vSubs.Free;
  end;
end;

function Tfrm_recursos.PrepararFontes(ALog: TLogLinha): boolean;
var
  vVersao: TVersaoRAL;
  vPreparo: TPreparoFontes;
begin
  Result := True;
  vVersao := Versao;
  if FModoLocal or (vVersao = nil) then
    Exit;

  FreeAndNil(FCatalogoInstalar);
  FRepo.Log := ALog;
  vPreparo := TPreparoFontes.Create(FRepo, vVersao.Ref, vVersao.Tipo = tvRamo);
  try
    vPreparo.PastaBase := FPastaBase;
    vPreparo.Log := ALog;
    SubmodulosDaEscolha(vPreparo.Submodulos);
    Result := vPreparo.Executar;
    if not Result then
    begin
      ALog('ERRO: ' + vPreparo.Erro);
      ALog('Nenhuma IDE foi alterada.');
      Exit;
    end;

    // daqui em diante vale a pasta de verdade, com os submodulos
    FCatalogoInstalar := TCatalogo.Create;
    if not FCatalogoInstalar.Carregar(vPreparo.PastaDestino) then
    begin
      ALog('ERRO: os fontes baixados em ' + vPreparo.PastaDestino + ' não têm pacotes.');
      FreeAndNil(FCatalogoInstalar);
      Result := False;
    end;
  finally
    FRepo.Log := nil;
    vPreparo.Free;
  end;
end;

function Tfrm_recursos.PlanejarDependencias: string;
var
  vIDEs, vLista: TList;
  vFora, vNomes, vExigidas, vPedidas: TStringList;
  vInt, vIDE, vPac: integer;
  vReceita: TReceita;
  vBaixa: TBaixaDependencia;
  vBloco: TBlocoIDE;
  vInstancia: TIDEInstance;
  vVersao, vMotivo, vChave: string;
  vTipo: TTipoPacote;
begin
  Result := '';
  FDependencias.Clear;
  FEscolha.PastasDependencias.Clear;
  if FCatalogo.Count = 0 then
    Exit;
  vIDEs := TList.Create;
  vLista := TList.Create;
  vFora := TStringList.Create;
  vNomes := TStringList.Create;
  vExigidas := TStringList.Create;
  // nome@versao = para quais pacotes; Objects = a receita
  vPedidas := TStringList.Create;
  Screen.Cursor := crHourGlass;
  try
    // cada IDE pede o que ela nao tem, na versao que ela aceita (o Zeos do
    // FPC 3.3 nao e o do Delphi); o que nao cabe nela nao pede nada
    IDEsMarcadas(vIDEs);
    for vIDE := 0 to Pred(vIDEs.Count) do
    begin
      vInstancia := TIDEObjectData(vIDEs[vIDE]).Instancia;
      vLista.Clear;
      vFora.Clear;
      if vInstancia.Tipo = tiDelphi then
        vTipo := tpDelphi
      else
        vTipo := tpLazarus;
      NomesDoTipo(vTipo, vNomes);
      FCatalogo.Fechamento(vTipo, vNomes, vLista);
      Compat.Filtrar(vLista, vInstancia, vFora);
      vNomes.Clear;
      for vPac := 0 to Pred(vLista.Count) do
        vNomes.Add(TPacote(vLista[vPac]).Nome);
      FReceitas.Exigidas(FCatalogo, vTipo, vNomes, vExigidas);
      for vInt := 0 to Pred(vExigidas.Count) do
      begin
        vReceita := TReceita(vExigidas.Objects[vInt]);
        vBloco := vReceita.Bloco(vTipo);
        // comercial, ou sem como instalar neste tipo de IDE: o plano da IDE
        // diz se ela ja tem ou se falta
        if not vReceita.PodeBaixar or not vBloco.Existe or (vBloco.Acoes.Count = 0) then
          Continue;
        // a que ja esta vale (§8)
        if Detectar(vInstancia, vReceita) <> '' then
          Continue;
        vVersao := Compat.VersaoDependencia(vReceita, vInstancia, vMotivo);
        if vMotivo <> '' then
          Continue;
        vChave := ChaveDependencia(vReceita.Nome, vVersao);
        if vPedidas.IndexOfName(vChave) < 0 then
          vPedidas.AddObject(vChave + '=' + vExigidas[vInt], vReceita);
      end;
    end;

    for vInt := 0 to Pred(vPedidas.Count) do
    begin
      vReceita := TReceita(vPedidas.Objects[vInt]);
      vChave := vPedidas.Names[vInt];
      vBaixa := TBaixaDependencia.Create(vReceita);
      try
        vBaixa.PastaBase := FPastaBase;
        vBaixa.VersaoPedida := Copy(vChave, Pos('@', vChave) + 1, MaxInt);
        if vBaixa.ResolverVersao then
        begin
          FDependencias.Values[vChave] := vBaixa.PastaDestino;
          Result := Result + Format('Baixar %s %s%s para %s (para %s)',
            [vReceita.Nome, vBaixa.Ref,
             IfThen(SameText(vBaixa.Fonte, vReceita.Github), '', ' de ' + vBaixa.Fonte),
             ExcludeTrailingPathDelimiter(vBaixa.PastaDestino), vPedidas.ValueFromIndex[vInt]]) +
            LineEnding;
        end
        else
          Result := Result + Format('ERRO: %s — sem ela, %s fica de fora',
                                    [vBaixa.Erro, vPedidas.ValueFromIndex[vInt]]) + LineEnding;
      finally
        vBaixa.Free;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    vPedidas.Free;
    vExigidas.Free;
    vNomes.Free;
    vFora.Free;
    vLista.Free;
    vIDEs.Free;
  end;
  FEscolha.PastasDependencias.Assign(FDependencias);
end;

function Tfrm_recursos.BaixarDependencias(ALog: TLogLinha): boolean;
var
  vInt: integer;
  vReceita: TReceita;
  vBaixa: TBaixaDependencia;
  vChave: string;
begin
  Result := True;
  for vInt := Pred(FDependencias.Count) downto 0 do
  begin
    // nome@versao: a mesma dependencia pode vir em duas versoes
    vChave := FDependencias.Names[vInt];
    vReceita := FReceitas.Buscar(Copy(vChave, 1, Pos('@', vChave) - 1));
    if vReceita = nil then
      Continue;
    vBaixa := TBaixaDependencia.Create(vReceita);
    try
      vBaixa.PastaBase := FPastaBase;
      vBaixa.VersaoPedida := Copy(vChave, Pos('@', vChave) + 1, MaxInt);
      vBaixa.Log := ALog;
      vBaixa.Repo.OnProgresso := @Progresso;
      if not vBaixa.Executar then
      begin
        ALog('ERRO: ' + vBaixa.Erro);
        FDependencias.Delete(vInt);
        Result := False;
      end;
    finally
      vBaixa.Free;
    end;
  end;
  FEscolha.PastasDependencias.Assign(FDependencias);
end;

function Tfrm_recursos.Escolha: TEscolhaInstalacao;
begin
  if FCatalogoInstalar <> nil then
    FEscolha.Catalogo := FCatalogoInstalar
  else
    FEscolha.Catalogo := FCatalogo;
  FEscolha.Receitas := FReceitas;
  FEscolha.Manifesto := FManifesto;
  FEscolha.PastasDependencias.Assign(FDependencias);
  FEscolha.PastaFontes := PastaFontes;
  FEscolha.Pacotes.Assign(FEscolhidos);
  FEscolha.SomenteLibraryPath := ckSomentePaths.Visible and ckSomentePaths.Checked;
  FEscolha.Win64 := ckWin64.Visible and ckWin64.Checked;
  Result := FEscolha;
end;

constructor Tfrm_recursos.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRepo := TRepoGitHub.Create(DonoRAL, RepoRAL);
  FRepo.OnProgresso := @Progresso;
  FVersoes := TVersoesRAL.Create(True);
  FCatalogo := TCatalogo.Create;
  FEscolhidos := TStringList.Create;
  FEscolhidos.CaseSensitive := False;
  FEscolha := TEscolhaInstalacao.Create;
  FPastaBase := IncludeTrailingPathDelimiter(GetUserDir) + 'RAL';
  FDependencias := TStringList.Create;
  FDependencias.CaseSensitive := False;
  // as receitas vem embutidas no executavel; uma pasta 'receitas' ao lado dele
  // ou na pasta de dados acrescenta ou substitui, sem recompilar
  FReceitas := TReceitas.Create;
  FReceitas.CarregarPadrao;
  FManifesto := TManifesto.Create;
  FDeteccoes := TStringList.Create;
  lbInfo.Caption := '';
  AtualizarOpcoes;
end;

destructor Tfrm_recursos.Destroy;
begin
  FreeAndNil(FCompat);
  FreeAndNil(FDeteccoes);
  FreeAndNil(FManifesto);
  FreeAndNil(FReceitas);
  FreeAndNil(FDependencias);
  FreeAndNil(FEscolha);
  FreeAndNil(FEscolhidos);
  FreeAndNil(FCatalogoInstalar);
  FreeAndNil(FCatalogo);
  FreeAndNil(FVersoes);
  FreeAndNil(FRepo);
  inherited Destroy;
end;

end.
