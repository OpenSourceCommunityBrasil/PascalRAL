/// Preview page: runs the migration in memory (nothing is written) and shows
/// what would change: the files with the unified diff of each, the items that
/// need the user, the behavior that changed and the errors.
unit RALMig.Tela.Previa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, Forms,
  RALMig.Tipos, RALMig.Tela.Estado, RALMig.Tela.Modelo;

type
  /// Preview page.
  TPaginaPrevia = class(TPaginaMig)
  private
    FAbaArquivos: TTabSheet;
    FAbaComportamento: TTabSheet;
    FAbaErros: TTabSheet;
    FAbaManual: TTabSheet;
    FAbas: TPageControl;
    FAlterados: TList;
    FComportamento: TMemo;
    FDiff: TMemo;
    FErros: TMemo;
    FIgnorar: TCheckBox;
    FLista: TListBox;
    FManual: TMemo;
    FResultado: TResultadoMigracao;
    FResumo: TLabel;
    procedure ArquivoSelecionado(Sender: TObject; AUsuario: boolean);
    procedure IgnorarClick(Sender: TObject);
    procedure MostrarComportamento;
    procedure MostrarManual;
    procedure PreencherArquivos;
    procedure PreencherTextos;
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    destructor Destroy; override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function PodeAvancar(out AMotivo: string): boolean; override;

    /// Result of the last simulation (owned by the engine)
    property Resultado: TResultadoMigracao read FResultado;
  end;

implementation

uses
  RALMig.Relatorio, RALMig.Tela.Mensagens;

{ TPaginaPrevia }

constructor TPaginaPrevia.Criar(AOwner: TComponent; AEstado: TEstadoMig);
begin
  inherited Criar(AOwner, AEstado);
  FAlterados := TList.Create;
  FResumo := NovoRotulo(MARGEM, 186, LARGURA_UTIL, 10, True);
  FAbas := TPageControl.Create(Self);
  FAbas.Parent := Self;
  Colocar(FAbas, MARGEM, 214, LARGURA_UTIL, 342);
  FAbaArquivos := FAbas.AddTabSheet;
  FAbaManual := FAbas.AddTabSheet;
  FAbaComportamento := FAbas.AddTabSheet;
  FAbaErros := FAbas.AddTabSheet;
  FLista := TListBox.Create(Self);
  FLista.Parent := FAbaArquivos;
  FLista.Align := alLeft;
  FLista.Width := Esc(250);
  FLista.OnSelectionChange := @ArquivoSelecionado;
  FDiff := NovoMemo(FAbaArquivos, 0, 0, 0, 0);
  FDiff.BorderSpacing.Left := Esc(6);
  FManual := NovoMemo(FAbaManual, 0, 0, 0, 0);
  FManual.WordWrap := True;
  FComportamento := NovoMemo(FAbaComportamento, 0, 0, 0, 0);
  FComportamento.WordWrap := True;
  FErros := NovoMemo(FAbaErros, 0, 0, 0, 0);
  FErros.WordWrap := True;
  FIgnorar := NovaCaixa(MARGEM, 560, LARGURA_UTIL);
  FIgnorar.OnClick := @IgnorarClick;
end;

destructor TPaginaPrevia.Destroy;
begin
  FAlterados.Free;
  inherited Destroy;
end;

procedure TPaginaPrevia.AoMostrar;
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

procedure TPaginaPrevia.ArquivoSelecionado(Sender: TObject; AUsuario: boolean);
var
  vInd: integer;
begin
  vInd := FLista.ItemIndex;
  FDiff.Clear;
  if (vInd >= 0) and (vInd < FAlterados.Count) then
    FDiff.Text := Estado.DiffDe(TArquivoMigrado(FAlterados[vInd]));
end;

procedure TPaginaPrevia.AtualizarTextos;
begin
  Titular(cmPreviaTitulo, cmPreviaSub);
  FIgnorar.Caption := cmIgnorarErros;
  if FResultado <> nil then
    PreencherTextos;
end;

procedure TPaginaPrevia.IgnorarClick(Sender: TObject);
begin
  Estado.IgnorarErros := FIgnorar.Checked;
  Mudou;
end;

procedure TPaginaPrevia.MostrarComportamento;
var
  vInt: integer;
  vItem: TItemComportamento;
begin
  FComportamento.Clear;
  for vInt := 0 to FResultado.ComportamentoCount - 1 do
  begin
    vItem := FResultado.Comportamentos[vInt];
    FComportamento.Lines.Add(vItem.Descricao);
    if vItem.Fazer <> '' then
      FComportamento.Lines.Add('  ' + Format(cmItemFazer, [vItem.Fazer]));
    if vItem.Wiki <> '' then
      FComportamento.Lines.Add('  ' + Format(cmItemWiki, [LinkDaWiki(vItem.Wiki)]));
    FComportamento.Lines.Add('');
  end;
end;

procedure TPaginaPrevia.MostrarManual;
var
  vInt: integer;
  vItem: TItemManual;
  vLocal: string;
begin
  FManual.Clear;
  for vInt := 0 to FResultado.ManualCount - 1 do
  begin
    vItem := FResultado.Manuais[vInt];
    vLocal := vItem.Arquivo;
    if vItem.Linha > 0 then
      vLocal := Format(cmItemLocal, [vItem.Arquivo, vItem.Linha]);
    FManual.Lines.Add(vLocal);
    FManual.Lines.Add('  ' + vItem.Problema);
    if vItem.Fazer <> '' then
      FManual.Lines.Add('  ' + Format(cmItemFazer, [vItem.Fazer]));
    if vItem.Trecho <> '' then
      FManual.Lines.Add(vItem.Trecho);
    if vItem.Wiki <> '' then
      FManual.Lines.Add('  ' + Format(cmItemWiki, [LinkDaWiki(vItem.Wiki)]));
    FManual.Lines.Add('');
  end;
end;

function TPaginaPrevia.PodeAvancar(out AMotivo: string): boolean;
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

procedure TPaginaPrevia.PreencherArquivos;
var
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  FLista.Items.Clear;
  for vInt := 0 to FAlterados.Count - 1 do
  begin
    vArq := TArquivoMigrado(FAlterados[vInt]);
    if vArq.Novo then
      FLista.Items.Add(Format(cmArquivoNovo, [vArq.Relativo]))
    else
      FLista.Items.Add(vArq.Relativo);
  end;
  if FLista.Items.Count > 0 then
    FLista.ItemIndex := 0;
  ArquivoSelecionado(FLista, False);
end;

procedure TPaginaPrevia.PreencherTextos;
var
  vDados: TDadosRelatorio;
  vConjunto: TConjuntoArquivos;
begin
  vConjunto := Estado.Motor.Conjunto;
  vDados := DadosRelatorio(FResultado, vConjunto, '', '', False);
  FResumo.Caption := LinhaResumo(vDados);
  FAbaArquivos.Caption := Format(cmAbaArquivos, [FAlterados.Count]);
  FAbaManual.Caption := Format(cmAbaManual, [FResultado.ManualCount]);
  FAbaComportamento.Caption := Format(cmAbaComportamento,
    [FResultado.ComportamentoCount]);
  FAbaErros.Caption := Format(cmAbaErros, [FResultado.Erros.Count]);
  FAbaErros.TabVisible := FResultado.Erros.Count > 0;
  FErros.Lines.Assign(FResultado.Erros);
  FIgnorar.Visible := FResultado.Erros.Count > 0;
  FIgnorar.Checked := Estado.IgnorarErros;
  if FResultado.Erros.Count > 0 then
    FAbas.ActivePage := FAbaErros
  else
    FAbas.ActivePage := FAbaArquivos;
  PreencherArquivos;
  MostrarManual;
  MostrarComportamento;
  if (FAlterados.Count = 0) and (FResultado.Erros.Count = 0) then
    FDiff.Text := cmNenhumAlterado;
end;

end.
