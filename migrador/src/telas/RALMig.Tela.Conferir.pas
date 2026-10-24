/// Last page (optional): compiles the migrated project against the RAL of the
/// target, in a temporary folder, with an IDE the installer knows (by its
/// receipts) or one the user points to, and shows the first error of each file.
unit RALMig.Tela.Conferir;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Dialogs, Graphics,
  RALMig.Conferir, RALMig.Tela.Estado, RALMig.Tela.Modelo;

type
  /// Compile-to-check page.
  TPaginaConferir = class(TPaginaMig)
  private
    FBtnCompilar: TBotaoMig;
    FBtnOutra: TBotaoMig;
    FCompilando: boolean;
    FComboProjeto: TComboBox;
    FDica: TLabel;
    FIDEs: TIDEsConferencia;
    FLbIDEs: TLabel;
    FLbProjeto: TLabel;
    FListaIDEs: TListBox;
    FSaida: TMemo;
    procedure CompilarClick(Sender: TObject);
    procedure IDESelecionada(Sender: TObject; AUsuario: boolean);
    procedure LogarLinha(const ALinha: string);
    procedure OutraClick(Sender: TObject);
    procedure PreencherIDEs;
    procedure PreencherProjetos;
    function ProjetoAbsoluto: string;
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    destructor Destroy; override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function RotuloAvancar: string; override;
  end;

implementation

uses
  RALInst.IDE, RALInst.Processo, RALMig.Regras, RALMig.Tela.Mensagens;

procedure ProcessarMensagens;
begin
  Application.ProcessMessages;
end;

{ TPaginaConferir }

constructor TPaginaConferir.Criar(AOwner: TComponent; AEstado: TEstadoMig);
begin
  inherited Criar(AOwner, AEstado);
  FIDEs := TIDEsConferencia.Create(True);
  FDica := NovoRotulo(MARGEM, 186, LARGURA_UTIL);
  Colocar(FDica, MARGEM, 186, LARGURA_UTIL, 44);
  FLbIDEs := NovoRotulo(MARGEM, 236, LARGURA_UTIL);
  FListaIDEs := NovaLista(MARGEM, 258, LARGURA_UTIL, 96);
  FListaIDEs.OnSelectionChange := @IDESelecionada;
  FBtnOutra := NovoBotao(MARGEM, 362, 220, 32);
  FBtnOutra.OnClick := @OutraClick;
  FLbProjeto := NovoRotulo(MARGEM, 404, 300);
  FComboProjeto := TComboBox.Create(Self);
  FComboProjeto.Parent := Self;
  Colocar(FComboProjeto, MARGEM, 426, 540, 28);
  FComboProjeto.Style := csDropDownList;
  FBtnCompilar := NovoBotao(MARGEM + 560, 424, 180, 32);
  FBtnCompilar.OnClick := @CompilarClick;
  FSaida := NovoMemo(Self, MARGEM, 464, LARGURA_UTIL, 112);
end;

destructor TPaginaConferir.Destroy;
begin
  FIDEs.Free;
  inherited Destroy;
end;

procedure TPaginaConferir.AoMostrar;
begin
  Screen.Cursor := crHourGlass;
  try
    FIDEs.Clear;
    if Estado.Para <> '' then
      IDEsComRAL(NormalizarVersao(Estado.Para), FIDEs);
  finally
    Screen.Cursor := crDefault;
  end;
  PreencherIDEs;
end;

procedure TPaginaConferir.AtualizarTextos;
begin
  Titular(cmConfTitulo, cmConfSub);
  FLbIDEs.Caption := Format(cmConfIDEs, [Estado.Para]);
  FBtnOutra.Texto := cmConfOutraIDE;
  FLbProjeto.Caption := cmConfProjeto;
  FBtnCompilar.Texto := cmCompilar;
  if FIDEs.Count = 0 then
    FDica.Caption := Format(cmConfNenhuma, [Estado.Para])
  else
    FDica.Caption := cmConfDica;
end;

procedure TPaginaConferir.CompilarClick(Sender: TObject);
var
  vIDE: TIDEConferencia;
  vErro: string;
  vOk: boolean;
begin
  if FCompilando or not FBtnCompilar.Enabled then
    Exit;
  if FListaIDEs.ItemIndex < 0 then
  begin
    FSaida.Lines.Add(emConfSemIDE);
    Exit;
  end;
  if ProjetoAbsoluto = '' then
  begin
    FSaida.Lines.Add(emConfSemProjeto);
    Exit;
  end;
  vIDE := FIDEs[FListaIDEs.ItemIndex];
  FCompilando := True;
  AoEsperarProcesso := @ProcessarMensagens;
  Screen.Cursor := crHourGlass;
  FBtnCompilar.Habilitar(False);
  try
    FSaida.Clear;
    FSaida.Lines.Add(cmCompilando);
    vOk := CompilarProjeto(vIDE.IDE, ProjetoAbsoluto, vIDE.RaizRAL, vErro,
      @LogarLinha);
    FSaida.Lines.Add('');
    if vOk then
      FSaida.Lines.Add(cmCompilou)
    else
    begin
      FSaida.Lines.Add(cmNaoCompilou);
      FSaida.Lines.Add(vErro);
    end;
  finally
    Screen.Cursor := crDefault;
    AoEsperarProcesso := nil;
    FBtnCompilar.Habilitar(True);
    FCompilando := False;
  end;
end;

procedure TPaginaConferir.IDESelecionada(Sender: TObject; AUsuario: boolean);
begin
  PreencherProjetos;
end;

procedure TPaginaConferir.LogarLinha(const ALinha: string);
begin
  FSaida.Lines.Add(ALinha);
  Application.ProcessMessages;
end;

procedure TPaginaConferir.OutraClick(Sender: TObject);
var
  vIDE, vRAL: TSelectDirectoryDialog;
  vNova: TIDEConferencia;
begin
  vIDE := TSelectDirectoryDialog.Create(Self);
  vRAL := TSelectDirectoryDialog.Create(Self);
  try
    vIDE.Title := cmEscolherRaizIDE;
    vRAL.Title := cmEscolherRaizRAL;
    if not vIDE.Execute then
      Exit;
    vRAL.InitialDir := ExtractFilePath(ExcludeTrailingPathDelimiter(vIDE.FileName));
    if not vRAL.Execute then
      Exit;
    vNova := AdicionarIDEManual(FIDEs, vIDE.FileName, vRAL.FileName);
    if vNova = nil then
    begin
      MessageDlg(cmConfTitulo, emIDEInvalida, mtError, [mbOK], 0);
      Exit;
    end;
    PreencherIDEs;
    FListaIDEs.ItemIndex := FListaIDEs.Items.Count - 1;
    PreencherProjetos;
  finally
    vRAL.Free;
    vIDE.Free;
  end;
end;

procedure TPaginaConferir.PreencherIDEs;
var
  vInt: integer;
begin
  FListaIDEs.Items.Clear;
  for vInt := 0 to FIDEs.Count - 1 do
    FListaIDEs.Items.Add(FIDEs[vInt].Descricao);
  if FListaIDEs.Items.Count > 0 then
    FListaIDEs.ItemIndex := 0;
  PreencherProjetos;
  AtualizarTextos;
end;

procedure TPaginaConferir.PreencherProjetos;
var
  vInt: integer;
  vExt: string;
  vDelphi: boolean;
begin
  FComboProjeto.Items.Clear;
  if FListaIDEs.ItemIndex >= 0 then
  begin
    vDelphi := FIDEs[FListaIDEs.ItemIndex].IDE.Tipo = tiDelphi;
    for vInt := 0 to Estado.Projetos.Count - 1 do
    begin
      vExt := LowerCase(ExtractFileExt(Estado.Projetos[vInt]));
      // o .dproj e o .dpr sao o mesmo projeto: fica o que a IDE abre
      if (vDelphi and ((vExt = '.dproj') or (vExt = '.dpr'))) or
         ((not vDelphi) and (vExt = '.lpi')) then
        FComboProjeto.Items.Add(Estado.Projetos[vInt]);
    end;
  end;
  if FComboProjeto.Items.Count > 0 then
    FComboProjeto.ItemIndex := 0;
  FBtnCompilar.Habilitar(FComboProjeto.Items.Count > 0);
end;

function TPaginaConferir.ProjetoAbsoluto: string;
begin
  Result := '';
  if FComboProjeto.ItemIndex < 0 then
    Exit;
  Result := Estado.PastaProjeto + StringReplace(
    FComboProjeto.Items[FComboProjeto.ItemIndex], '/', PathDelim, [rfReplaceAll]);
end;

function TPaginaConferir.RotuloAvancar: string;
begin
  Result := cmConcluir;
end;

end.
