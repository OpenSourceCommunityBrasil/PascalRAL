/// Project page: the folder (or project file) to migrate, the projects found in
/// it, what lies outside it (with a box to bring it along) and the undo of an
/// earlier migration of the same folder.
unit RALMig.Tela.Projeto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, CheckLst, Dialogs, Graphics,
  RALMig.Tela.Estado, RALMig.Tela.Modelo;

type
  /// Project page.
  TPaginaProjeto = class(TPaginaMig)
  private
    FAchados: TLabel;
    FBtnArquivo: TBotaoMig;
    FBtnDesfazer: TBotaoMig;
    FBtnPasta: TBotaoMig;
    FCaminho: TEdit;
    FCopias: TLabel;
    FFora: TCheckListBox;
    FForaCaminhos: TStringList;
    FForaTitulo: TLabel;
    FLbCaminho: TLabel;
    FProjetos: TListBox;
    procedure CaminhoEditado(Sender: TObject);
    procedure DesfazerClick(Sender: TObject);
    procedure EscolherArquivo(Sender: TObject);
    procedure EscolherPasta(Sender: TObject);
    procedure ForaClickCheck(Sender: TObject);
    /// Reads the folder again and fills the lists
    procedure Reler(const APasta: string);
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    destructor Destroy; override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function PodeAvancar(out AMotivo: string): boolean; override;
    /// Fills the lists from the state
    procedure Atualizar;
    /// Takes a folder or project file (the command line)
    procedure DefinirCaminho(const ACaminho: string);
  end;

implementation

uses
  RALMig.Tela.Mensagens;

const
  PADROES_PROJETO = '*.dproj;*.dpr;*.dpk;*.groupproj;*.lpi;*.lpr;*.lpk;*.lpg';

{ TPaginaProjeto }

constructor TPaginaProjeto.Criar(AOwner: TComponent; AEstado: TEstadoMig);
begin
  inherited Criar(AOwner, AEstado);
  FForaCaminhos := TStringList.Create;
  FLbCaminho := NovoRotulo(MARGEM, 190, LARGURA_UTIL);
  FCaminho := TEdit.Create(Self);
  FCaminho.Parent := Self;
  Colocar(FCaminho, MARGEM, 214, 560, 28);
  FCaminho.OnEditingDone := @CaminhoEditado;
  FBtnPasta := NovoBotao(MARGEM + 572, 212, 112, 32);
  FBtnPasta.OnClick := @EscolherPasta;
  FBtnArquivo := NovoBotao(MARGEM + 692, 212, 112, 32);
  FBtnArquivo.OnClick := @EscolherArquivo;
  FAchados := NovoRotulo(MARGEM, 254, LARGURA_UTIL);
  FProjetos := NovaLista(MARGEM, 276, LARGURA_UTIL, 118);
  FForaTitulo := NovoRotulo(MARGEM, 402, LARGURA_UTIL);
  FFora := TCheckListBox.Create(Self);
  FFora.Parent := Self;
  Colocar(FFora, MARGEM, 424, LARGURA_UTIL, 84);
  FFora.OnClickCheck := @ForaClickCheck;
  FCopias := NovoRotulo(MARGEM, 522, 560);
  FCopias.Tag := -1;
  FCopias.Font.Color := RGBToColor(255, 170, 0);
  FBtnDesfazer := NovoBotao(MARGEM + 572, 516, 232, 36);
  FBtnDesfazer.OnClick := @DesfazerClick;
end;

destructor TPaginaProjeto.Destroy;
begin
  FForaCaminhos.Free;
  inherited Destroy;
end;

procedure TPaginaProjeto.AoMostrar;
begin
  Atualizar;
end;

procedure TPaginaProjeto.Atualizar;
var
  vInt: integer;
begin
  FCaminho.Text := Estado.Pasta;
  FProjetos.Items.Assign(Estado.Projetos);
  FAchados.Caption := Format(cmProjetosAchados, [Estado.Projetos.Count]);
  if (Estado.Pasta <> '') and (Estado.ArquivosAchados = 0) then
    FAchados.Caption := cmSemProjetos;
  FFora.Items.Clear;
  FForaCaminhos.Clear;
  for vInt := 0 to Estado.ForaProjetos.Count - 1 do
  begin
    FFora.Items.Add(Format(cmForaProjeto, [Estado.ForaProjetos[vInt]]));
    FForaCaminhos.Add(Estado.ForaProjetos[vInt]);
  end;
  for vInt := 0 to Estado.ForaUnits.Count - 1 do
  begin
    FFora.Items.Add(Format(cmForaUnit, [Estado.ForaUnits[vInt]]));
    FForaCaminhos.Add(Estado.ForaUnits[vInt]);
  end;
  FForaTitulo.Visible := FFora.Items.Count > 0;
  FFora.Visible := FFora.Items.Count > 0;
  FCopias.Visible := Estado.ContarCopias > 0;
  FBtnDesfazer.Visible := FCopias.Visible;
  if FCopias.Visible then
    FCopias.Caption := Format(cmCopiaAnterior, [Estado.ContarCopias]);
  Mudou;
end;

procedure TPaginaProjeto.AtualizarTextos;
begin
  Titular(cmProjetoTitulo, cmProjetoSub);
  FLbCaminho.Caption := cmProjetoCaminho;
  FBtnPasta.Texto := cmEscolherPasta;
  FBtnArquivo.Texto := cmEscolherArquivo;
  FForaTitulo.Caption := cmForaTitulo;
  FBtnDesfazer.Texto := cmDesfazerAnterior;
  Atualizar;
end;

procedure TPaginaProjeto.CaminhoEditado(Sender: TObject);
var
  vTexto: string;
begin
  vTexto := Trim(FCaminho.Text);
  if vTexto <> Estado.Pasta then
    Reler(vTexto);
end;

procedure TPaginaProjeto.DefinirCaminho(const ACaminho: string);
begin
  Reler(ACaminho);
end;

procedure TPaginaProjeto.DesfazerClick(Sender: TObject);
var
  vMensagem: string;
  vOk: boolean;
begin
  if not FBtnDesfazer.Enabled then
    Exit;
  if not Confirmar(cmDesfazerTitulo, cmDesfazerPergunta) then
    Exit;
  vOk := Estado.DesfazerUltima(vMensagem);
  if vOk then
    MessageDlg(cmDesfazerTitulo, vMensagem, mtInformation, [mbOK], 0)
  else
    MessageDlg(cmDesfazerTitulo, vMensagem, mtError, [mbOK], 0);
  Reler(Estado.Pasta);
end;

procedure TPaginaProjeto.EscolherArquivo(Sender: TObject);
var
  vDialogo: TOpenDialog;
begin
  vDialogo := TOpenDialog.Create(Self);
  try
    vDialogo.Filter := cmFiltroProjetos + '|' + PADROES_PROJETO + '|' + cmFiltroTodos +
      '|*.*';
    if vDialogo.Execute then
      Reler(vDialogo.FileName);
  finally
    vDialogo.Free;
  end;
end;

procedure TPaginaProjeto.EscolherPasta(Sender: TObject);
var
  vDialogo: TSelectDirectoryDialog;
begin
  vDialogo := TSelectDirectoryDialog.Create(Self);
  try
    vDialogo.Title := cmEscolherPastaTitulo;
    if DirectoryExists(Estado.Pasta) then
      vDialogo.InitialDir := Estado.Pasta;
    if vDialogo.Execute then
      Reler(vDialogo.FileName);
  finally
    vDialogo.Free;
  end;
end;

procedure TPaginaProjeto.ForaClickCheck(Sender: TObject);
var
  vInt: integer;
begin
  for vInt := 0 to FFora.Items.Count - 1 do
    Estado.IncluirFora(FForaCaminhos[vInt], FFora.Checked[vInt]);
end;

function TPaginaProjeto.PodeAvancar(out AMotivo: string): boolean;
begin
  Result := Estado.ArquivosAchados > 0;
  AMotivo := '';
  if (not Result) and (Estado.Pasta <> '') then
    AMotivo := cmSemProjetos;
end;

procedure TPaginaProjeto.Reler(const APasta: string);
begin
  Screen.Cursor := crHourGlass;
  try
    Estado.Pasta := APasta;
  finally
    Screen.Cursor := crDefault;
  end;
  Atualizar;
end;

end.
