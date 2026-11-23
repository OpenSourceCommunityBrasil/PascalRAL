/// Project page: the folder (or project file) to migrate, the projects found in
/// it, what lies outside it (with a box to bring it along) and the undo of an
/// earlier migration of the same folder.
unit RALMig.Tela.Projeto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, CheckLst, Dialogs, Graphics,
  RALMig.Tela.Pagina;

type
  /// Project page.
  TTelaProjeto = class(TTelaMig)
    clbFora: TCheckListBox;
    edCaminho: TEdit;
    lbAchados: TLabel;
    lbArquivo: TLabel;
    lbCaminho: TLabel;
    lbCopias: TLabel;
    lbDesfazer: TLabel;
    lbForaTitulo: TLabel;
    lbPasta: TLabel;
    lbxProjetos: TListBox;
    procedure clbForaClickCheck(Sender: TObject);
    procedure edCaminhoEditingDone(Sender: TObject);
    procedure lbArquivoClick(Sender: TObject);
    procedure lbDesfazerClick(Sender: TObject);
    procedure lbPastaClick(Sender: TObject);
  private
    FForaCaminhos: TStringList;
    /// Reads the folder again and fills the lists
    procedure Reler(const APasta: string);
  public
    constructor Create(AOwner: TComponent); override;
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

{$R *.lfm}

uses
  RALMig.Tela.Mensagens;

const
  PADROES_PROJETO = '*.dproj;*.dpr;*.dpk;*.groupproj;*.lpi;*.lpr;*.lpk;*.lpg';

{ TTelaProjeto }

constructor TTelaProjeto.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForaCaminhos := TStringList.Create;
end;

destructor TTelaProjeto.Destroy;
begin
  FForaCaminhos.Free;
  inherited Destroy;
end;

procedure TTelaProjeto.AoMostrar;
begin
  Atualizar;
end;

procedure TTelaProjeto.Atualizar;
var
  vInt: integer;
begin
  edCaminho.Text := Estado.Pasta;
  lbxProjetos.Items.Assign(Estado.Projetos);
  lbAchados.Caption := Format(cmProjetosAchados, [Estado.Projetos.Count]);
  if (Estado.Pasta <> '') and (Estado.ArquivosAchados = 0) then
    lbAchados.Caption := cmSemProjetos;
  clbFora.Items.Clear;
  FForaCaminhos.Clear;
  for vInt := 0 to Estado.ForaProjetos.Count - 1 do
  begin
    clbFora.Items.Add(Format(cmForaProjeto, [Estado.ForaProjetos[vInt]]));
    FForaCaminhos.Add(Estado.ForaProjetos[vInt]);
  end;
  for vInt := 0 to Estado.ForaUnits.Count - 1 do
  begin
    clbFora.Items.Add(Format(cmForaUnit, [Estado.ForaUnits[vInt]]));
    FForaCaminhos.Add(Estado.ForaUnits[vInt]);
  end;
  lbForaTitulo.Visible := clbFora.Items.Count > 0;
  clbFora.Visible := clbFora.Items.Count > 0;
  lbCopias.Visible := Estado.ContarCopias > 0;
  lbDesfazer.Visible := lbCopias.Visible;
  if lbCopias.Visible then
    lbCopias.Caption := Format(cmCopiaAnterior, [Estado.ContarCopias]);
  Mudou;
end;

procedure TTelaProjeto.AtualizarTextos;
begin
  inherited AtualizarTextos;
  Titular(cmProjetoTitulo, cmProjetoSub);
  lbCaminho.Caption := cmProjetoCaminho;
  lbPasta.Caption := cmEscolherPasta;
  lbArquivo.Caption := cmEscolherArquivo;
  lbForaTitulo.Caption := cmForaTitulo;
  lbDesfazer.Caption := cmDesfazerAnterior;
  Atualizar;
end;

procedure TTelaProjeto.clbForaClickCheck(Sender: TObject);
var
  vInt: integer;
begin
  for vInt := 0 to clbFora.Items.Count - 1 do
    Estado.IncluirFora(FForaCaminhos[vInt], clbFora.Checked[vInt]);
end;

procedure TTelaProjeto.DefinirCaminho(const ACaminho: string);
begin
  Reler(ACaminho);
end;

procedure TTelaProjeto.edCaminhoEditingDone(Sender: TObject);
var
  vTexto: string;
begin
  vTexto := Trim(edCaminho.Text);
  if vTexto <> Estado.Pasta then
    Reler(vTexto);
end;

procedure TTelaProjeto.lbArquivoClick(Sender: TObject);
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

procedure TTelaProjeto.lbDesfazerClick(Sender: TObject);
var
  vMensagem: string;
  vOk: boolean;
begin
  if not lbDesfazer.Enabled then
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

procedure TTelaProjeto.lbPastaClick(Sender: TObject);
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

function TTelaProjeto.PodeAvancar(out AMotivo: string): boolean;
begin
  Result := Estado.ArquivosAchados > 0;
  AMotivo := '';
  if (not Result) and (Estado.Pasta <> '') then
    AMotivo := cmSemProjetos;
end;

procedure TTelaProjeto.Reler(const APasta: string);
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
