/// Preview page: runs the migration in memory (nothing is written) and shows
/// what would change: the files with the unified diff of each, the items that
/// need the user, the behavior that changed and the errors.
unit RALMig.Tela.Previa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, Forms,
  RALMig.Tipos, RALMig.Tela.Pagina;

type
  /// Preview page.
  TTelaPrevia = class(TTelaMig)
    ckIgnorar: TCheckBox;
    lbIgnorar: TLabel;
    lbResumo: TLabel;
    lbxArquivos: TListBox;
    mmComportamento: TMemo;
    mmDiff: TMemo;
    mmErros: TMemo;
    mmManual: TMemo;
    pcAbas: TPageControl;
    tsArquivos: TTabSheet;
    tsComportamento: TTabSheet;
    tsErros: TTabSheet;
    tsManual: TTabSheet;
    procedure ckIgnorarClick(Sender: TObject);
    procedure lbxArquivosSelectionChange(Sender: TObject; User: boolean);
  private
    FAlterados: TList;
    FResultado: TResultadoMigracao;
    procedure MostrarComportamento;
    procedure MostrarManual;
    procedure PreencherArquivos;
    procedure PreencherTextos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function PodeAvancar(out AMotivo: string): boolean; override;

    /// Result of the last simulation (owned by the engine)
    property Resultado: TResultadoMigracao read FResultado;
  end;

implementation

{$R *.lfm}

uses
  RALMig.Relatorio, RALMig.Tela.Mensagens;

{ TTelaPrevia }

constructor TTelaPrevia.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlterados := TList.Create;
end;

destructor TTelaPrevia.Destroy;
begin
  FAlterados.Free;
  inherited Destroy;
end;

procedure TTelaPrevia.AoMostrar;
begin
  Screen.Cursor := crHourGlass;
  try
    FResultado := Estado.Simular;
    FAlterados.Clear;
    if Estado.Motor.Conjunto <> nil then
      Estado.Motor.Conjunto.ListarAlterados(FAlterados);
    PreencherTextos;
  finally
    Screen.Cursor := crDefault;
  end;
  Mudou;
end;

procedure TTelaPrevia.AtualizarTextos;
begin
  inherited AtualizarTextos;
  Titular(cmPreviaTitulo, cmPreviaSub);
  lbIgnorar.Caption := cmIgnorarErros;
  if FResultado <> nil then
    PreencherTextos;
end;

procedure TTelaPrevia.ckIgnorarClick(Sender: TObject);
begin
  Estado.IgnorarErros := ckIgnorar.Checked;
  Mudou;
end;

procedure TTelaPrevia.lbxArquivosSelectionChange(Sender: TObject; User: boolean);
var
  vInd: integer;
begin
  vInd := lbxArquivos.ItemIndex;
  mmDiff.Clear;
  if (vInd >= 0) and (vInd < FAlterados.Count) then
    mmDiff.Text := Estado.DiffDe(TArquivoMigrado(FAlterados[vInd]));
end;

procedure TTelaPrevia.MostrarComportamento;
var
  vInt: integer;
  vItem: TItemComportamento;
begin
  mmComportamento.Clear;
  for vInt := 0 to FResultado.ComportamentoCount - 1 do
  begin
    vItem := FResultado.Comportamentos[vInt];
    mmComportamento.Lines.Add(vItem.Descricao);
    if vItem.Fazer <> '' then
      mmComportamento.Lines.Add('  ' + Format(cmItemFazer, [vItem.Fazer]));
    if vItem.Wiki <> '' then
      mmComportamento.Lines.Add('  ' + Format(cmItemWiki, [LinkDaWiki(vItem.Wiki)]));
    mmComportamento.Lines.Add('');
  end;
end;

procedure TTelaPrevia.MostrarManual;
var
  vInt: integer;
  vItem: TItemManual;
  vLocal: string;
begin
  mmManual.Clear;
  for vInt := 0 to FResultado.ManualCount - 1 do
  begin
    vItem := FResultado.Manuais[vInt];
    vLocal := vItem.Arquivo;
    if vItem.Linha > 0 then
      vLocal := Format(cmItemLocal, [vItem.Arquivo, vItem.Linha]);
    mmManual.Lines.Add(vLocal);
    mmManual.Lines.Add('  ' + vItem.Problema);
    if vItem.Fazer <> '' then
      mmManual.Lines.Add('  ' + Format(cmItemFazer, [vItem.Fazer]));
    if vItem.Trecho <> '' then
      mmManual.Lines.Add(vItem.Trecho);
    if vItem.Wiki <> '' then
      mmManual.Lines.Add('  ' + Format(cmItemWiki, [LinkDaWiki(vItem.Wiki)]));
    mmManual.Lines.Add('');
  end;
end;

function TTelaPrevia.PodeAvancar(out AMotivo: string): boolean;
begin
  AMotivo := '';
  Result := False;
  if (FResultado = nil) or (Estado.Motor.Conjunto = nil) then
  begin
    if FResultado <> nil then
      AMotivo := cmPreviaSemConjunto;
    Exit;
  end;
  Result := (FResultado.Erros.Count = 0) or Estado.IgnorarErros;
  if not Result then
    AMotivo := Format(cmPreviaErros, [FResultado.Erros.Count]);
end;

procedure TTelaPrevia.PreencherArquivos;
var
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  lbxArquivos.Items.Clear;
  for vInt := 0 to FAlterados.Count - 1 do
  begin
    vArq := TArquivoMigrado(FAlterados[vInt]);
    if vArq.Novo then
      lbxArquivos.Items.Add(Format(cmArquivoNovo, [vArq.Relativo]))
    else
      lbxArquivos.Items.Add(vArq.Relativo);
  end;
  if lbxArquivos.Items.Count > 0 then
    lbxArquivos.ItemIndex := 0;
  lbxArquivosSelectionChange(lbxArquivos, False);
end;

procedure TTelaPrevia.PreencherTextos;
var
  vDados: TDadosRelatorio;
  vConjunto: TConjuntoArquivos;
begin
  vConjunto := Estado.Motor.Conjunto;
  vDados := DadosRelatorio(FResultado, vConjunto, '', '', False);
  lbResumo.Caption := LinhaResumo(vDados);
  tsArquivos.Caption := Format(cmAbaArquivos, [FAlterados.Count]);
  tsManual.Caption := Format(cmAbaManual, [FResultado.ManualCount]);
  tsComportamento.Caption := Format(cmAbaComportamento,
    [FResultado.ComportamentoCount]);
  tsErros.Caption := Format(cmAbaErros, [FResultado.Erros.Count]);
  tsErros.TabVisible := FResultado.Erros.Count > 0;
  mmErros.Lines.Assign(FResultado.Erros);
  ckIgnorar.Visible := FResultado.Erros.Count > 0;
  lbIgnorar.Visible := ckIgnorar.Visible;
  ckIgnorar.Checked := Estado.IgnorarErros;
  if FResultado.Erros.Count > 0 then
    pcAbas.ActivePage := tsErros
  else
    pcAbas.ActivePage := tsArquivos;
  PreencherArquivos;
  MostrarManual;
  MostrarComportamento;
  if (FAlterados.Count = 0) and (FResultado.Erros.Count = 0) then
    mmDiff.Text := cmNenhumAlterado;
end;

end.
