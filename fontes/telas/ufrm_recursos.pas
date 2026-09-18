unit ufrm_recursos;

{$mode ObjFPC}{$H+}

// Escolha dos recursos a partir do catalogo (F2): os pacotes que existem de
// verdade na pasta do RAL, agrupados como no repositorio (pkg/<IDE>/Engine,
// Database, compression...). Marcar um pacote marca o que ele exige; o que
// entra so por dependencia aparece como "necessario".
//
// Por enquanto a origem e uma pasta local com os fontes (o caso de quem
// desenvolve o RAL); baixar do GitHub a versao escolhida e a F8.

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ComCtrls,
  Buttons, ufrm_modelo, LCLType, ExtCtrls,
  ideutils, RALInst.Catalogo;

type

  { Tfrm_recursos }

  Tfrm_recursos = class(Tfrm_modelo)
    bAddVersion: TSpeedButton;
    ckSomentePaths: TCheckBox;
    ckWin64: TCheckBox;
    dirSelect: TSelectDirectoryDialog;
    Label2: TLabel;
    lbInfo: TLabel;
    lbSomentePaths: TLabel;
    lbWin64: TLabel;
    lbedDownloadPath: TLabeledEdit;
    lbSubTitle: TLabel;
    tvRecursos: TTreeView;
    procedure bAddVersionClick(Sender: TObject);
    procedure lbedDownloadPathEditingDone(Sender: TObject);
    procedure lbSomentePathsClick(Sender: TObject);
    procedure lbWin64Click(Sender: TObject);
    procedure tvRecursosDblClick(Sender: TObject);
    procedure tvRecursosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FCatalogo: TCatalogo;
    FPastaCarregada: string;
    // o que o usuario marcou; as dependencias sao calculadas a cada desenho
    FEscolhidos: TStringList;
    FEscolha: TEscolhaInstalacao;
    function TipoPacote: TTipoPacote;
    function NomeGrupo(const AGrupo: string): string;
    function Indisponivel(APacote: TPacote): string;
    procedure CarregarCatalogo;
    procedure EscolherPadrao;
    procedure MontarArvore;
    procedure Alternar(APacote: TPacote; AMarcar: boolean);
    procedure AtualizarOpcoes;
  protected
    procedure SetIDE(AValue: integer); override;
    function validatePageNext : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    // a escolha completa para os motores de instalacao
    function Escolha: TEscolhaInstalacao;
  published
    property Catalogo: TCatalogo read FCatalogo;
  end;

implementation

{$R *.lfm}

uses
  udm;

const
  ImgMarcado = 3;
  ImgDesmarcado = 4;

{ Tfrm_recursos }

function Tfrm_recursos.TipoPacote: TTipoPacote;
begin
  // 0 - Delphi, 1 - Lazarus
  if IDE = 0 then
    Result := tpDelphi
  else
    Result := tpLazarus;
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
begin
  Result := '';
  if APacote.SubmodulosAusentes.Count > 0 then
    Result := 'falta o submódulo ' + APacote.SubmodulosAusentes.CommaText +
              ' (git submodule update --init)'
  else if APacote.FontesAusentes.Count > 0 then
    Result := 'falta o fonte ' + APacote.FontesAusentes[0]
  else if APacote.Ordem < 0 then
    Result := 'dependência circular';
end;

procedure Tfrm_recursos.CarregarCatalogo;
var
  vPasta: string;
begin
  vPasta := Trim(lbedDownloadPath.Text);
  if SameText(vPasta, FPastaCarregada) then
    Exit;
  FPastaCarregada := vPasta;

  FCatalogo.Limpar;
  FEscolhidos.Clear;
  tvRecursos.Items.Clear;

  if vPasta = '' then
  begin
    lbInfo.Caption := 'Escolha a pasta dos fontes do PascalRAL.';
    Exit;
  end;
  if not DirectoryExists(vPasta) then
  begin
    lbInfo.Caption := 'Pasta não encontrada.';
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    if not FCatalogo.Carregar(vPasta) then
    begin
      lbInfo.Caption := 'Nenhum pacote do RAL nesta pasta (ela deve conter pkg e src).';
      Exit;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  EscolherPadrao;
  MontarArvore;
end;

procedure Tfrm_recursos.EscolherPadrao;
var
  vLista: TList;
  vInt: integer;
  vPacote: TPacote;
begin
  // o nucleo do RAL (pacotes da raiz de pkg/<IDE>), menos o assistente
  FEscolhidos.Clear;
  vLista := TList.Create;
  try
    FCatalogo.Listar(TipoPacote, vLista);
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TPacote(vLista[vInt]);
      if (vPacote.Grupo = '') and (Pos('wizard', LowerCase(vPacote.Nome)) = 0) and
         (Indisponivel(vPacote) = '') then
        FEscolhidos.Add(vPacote.Nome);
    end;
  finally
    vLista.Free;
  end;
end;

procedure Tfrm_recursos.MontarArvore;
var
  vLista, vFechamento: TList;
  vGrupos: TStringList;
  vInt, vIdx: integer;
  vPacote: TPacote;
  vGrupo, vNo: TTreeNode;
  vTexto, vMotivo: string;
  vMarcado: boolean;
begin
  tvRecursos.Items.BeginUpdate;
  vLista := TList.Create;
  vFechamento := TList.Create;
  vGrupos := TStringList.Create;
  try
    tvRecursos.Items.Clear;
    FCatalogo.Listar(TipoPacote, vLista);
    FCatalogo.Fechamento(TipoPacote, FEscolhidos, vFechamento);

    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TPacote(vLista[vInt]);

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
        vTexto := vTexto + '  —  ' + vPacote.Descricao;

      vMarcado := vFechamento.IndexOf(vPacote) >= 0;
      vMotivo := Indisponivel(vPacote);
      if vMotivo <> '' then
        vTexto := vTexto + '  [indisponível: ' + vMotivo + ']'
      else if vMarcado and (FEscolhidos.IndexOf(vPacote.Nome) < 0) then
        vTexto := vTexto + '  (necessário)';

      vNo := tvRecursos.Items.AddChild(vGrupo, vTexto);
      vNo.Data := vPacote;
      if vMarcado then
        vNo.ImageIndex := ImgMarcado
      else
        vNo.ImageIndex := ImgDesmarcado;
      vNo.SelectedIndex := vNo.ImageIndex;
    end;

    tvRecursos.FullExpand;
    lbInfo.Caption := Format('%d pacote(s) nesta versão; %d marcado(s)',
                             [vLista.Count, vFechamento.Count]);
    if FCatalogo.Erros.Count > 0 then
    begin
      lbInfo.Caption := lbInfo.Caption + Format(' — %d aviso(s)', [FCatalogo.Erros.Count]);
      lbInfo.Hint := FCatalogo.Erros.Text;
      lbInfo.ShowHint := True;
    end;
  finally
    vGrupos.Free;
    vFechamento.Free;
    vLista.Free;
    tvRecursos.Items.EndUpdate;
  end;
end;

procedure Tfrm_recursos.Alternar(APacote: TPacote; AMarcar: boolean);
var
  vFechamento: TList;
  vMotivo: string;
  vInt, vDep: integer;
  vQuem: string;
begin
  vMotivo := Indisponivel(APacote);
  if AMarcar and (vMotivo <> '') then
  begin
    ShowMessage(APacote.Nome + ' está indisponível nesta pasta: ' + vMotivo + '.');
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
    try
      FCatalogo.Fechamento(TipoPacote, FEscolhidos, vFechamento);
      if vFechamento.IndexOf(APacote) >= 0 then
      begin
        vQuem := '';
        for vInt := 0 to Pred(vFechamento.Count) do
          for vDep := 0 to Pred(TPacote(vFechamento[vInt]).Internos.Count) do
            if SameText(TPacote(vFechamento[vInt]).Internos[vDep], APacote.Nome) then
              vQuem := vQuem + ' ' + TPacote(vFechamento[vInt]).Nome;
        ShowMessage(APacote.Nome + ' continua marcado: é necessário para' + vQuem + '.');
      end;
    finally
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
  vDelphi := IDE = 0;
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
    CarregarCatalogo;
  end;
end;

procedure Tfrm_recursos.lbedDownloadPathEditingDone(Sender: TObject);
begin
  CarregarCatalogo;
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

function Tfrm_recursos.validatePageNext: boolean;
begin
  Result := False;
  CarregarCatalogo;
  if FCatalogo.Count = 0 then
  begin
    ShowMessage('Escolha a pasta dos fontes do PascalRAL (a que contém pkg e src).');
    Exit;
  end;
  if FEscolhidos.Count = 0 then
  begin
    ShowMessage('Marque ao menos um recurso.');
    Exit;
  end;
  Result := True;
end;

function Tfrm_recursos.Escolha: TEscolhaInstalacao;
begin
  FEscolha.Catalogo := FCatalogo;
  FEscolha.Pacotes.Assign(FEscolhidos);
  FEscolha.SomenteLibraryPath := ckSomentePaths.Visible and ckSomentePaths.Checked;
  FEscolha.Win64 := ckWin64.Visible and ckWin64.Checked;
  Result := FEscolha;
end;

constructor Tfrm_recursos.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCatalogo := TCatalogo.Create;
  FEscolhidos := TStringList.Create;
  FEscolhidos.CaseSensitive := False;
  FEscolha := TEscolhaInstalacao.Create;
  lbInfo.Caption := 'Escolha a pasta dos fontes do PascalRAL.';
  AtualizarOpcoes;
end;

destructor Tfrm_recursos.Destroy;
begin
  FreeAndNil(FEscolha);
  FreeAndNil(FEscolhidos);
  FreeAndNil(FCatalogo);
  inherited Destroy;
end;

end.
