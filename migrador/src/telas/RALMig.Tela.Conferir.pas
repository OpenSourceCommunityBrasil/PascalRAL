/// Last page (optional): compiles the migrated project against the RAL of the
/// target, in a temporary folder, with an IDE the installer knows (by its
/// receipts) or one the user points to, and shows the first error of each file.
unit RALMig.Tela.Conferir;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Dialogs, Graphics,
  RALMig.Conferir, RALMig.Tela.Pagina;

type
  /// Compile-to-check page.
  TTelaConferir = class(TTelaMig)
    cbProjeto: TComboBox;
    lbCompilar: TLabel;
    lbDica: TLabel;
    lbIDEs: TLabel;
    lbOutra: TLabel;
    lbProjeto: TLabel;
    lbxIDEs: TListBox;
    mmSaida: TMemo;
    procedure lbCompilarClick(Sender: TObject);
    procedure lbOutraClick(Sender: TObject);
    procedure lbxIDEsSelectionChange(Sender: TObject; User: boolean);
  private
    FCompilando: boolean;
    FIDEs: TIDEsConferencia;
    procedure LogarLinha(const ALinha: string);
    procedure PreencherIDEs;
    procedure PreencherProjetos;
    function ProjetoAbsoluto: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function RotuloAvancar: string; override;
  end;

implementation

{$R *.lfm}

uses
  RALInst.IDE, RALInst.Processo, RALMig.Regras, RALMig.Tela.Mensagens;

procedure ProcessarMensagens;
begin
  Application.ProcessMessages;
end;

{ TTelaConferir }

constructor TTelaConferir.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIDEs := TIDEsConferencia.Create(True);
end;

destructor TTelaConferir.Destroy;
begin
  FIDEs.Free;
  inherited Destroy;
end;

procedure TTelaConferir.AoMostrar;
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

procedure TTelaConferir.AtualizarTextos;
begin
  inherited AtualizarTextos;
  Titular(cmConfTitulo, cmConfSub);
  lbIDEs.Caption := Format(cmConfIDEs, [Estado.Para]);
  lbOutra.Caption := cmConfOutraIDE;
  lbProjeto.Caption := cmConfProjeto;
  lbCompilar.Caption := cmCompilar;
  if FIDEs.Count = 0 then
    lbDica.Caption := Format(cmConfNenhuma, [Estado.Para])
  else
    lbDica.Caption := cmConfDica;
end;

procedure TTelaConferir.lbCompilarClick(Sender: TObject);
var
  vIDE: TIDEConferencia;
  vErro: string;
  vOk: boolean;
begin
  if FCompilando or not lbCompilar.Enabled then
    Exit;
  if lbxIDEs.ItemIndex < 0 then
  begin
    mmSaida.Lines.Add(emConfSemIDE);
    Exit;
  end;
  if ProjetoAbsoluto = '' then
  begin
    mmSaida.Lines.Add(emConfSemProjeto);
    Exit;
  end;
  vIDE := FIDEs[lbxIDEs.ItemIndex];
  FCompilando := True;
  AoEsperarProcesso := @ProcessarMensagens;
  Screen.Cursor := crHourGlass;
  HabilitarLink(lbCompilar, False);
  try
    mmSaida.Clear;
    mmSaida.Lines.Add(cmCompilando);
    vOk := CompilarProjeto(vIDE.IDE, ProjetoAbsoluto, vIDE.RaizRAL, vErro,
      @LogarLinha);
    mmSaida.Lines.Add('');
    if vOk then
      mmSaida.Lines.Add(cmCompilou)
    else
    begin
      mmSaida.Lines.Add(cmNaoCompilou);
      mmSaida.Lines.Add(vErro);
    end;
  finally
    Screen.Cursor := crDefault;
    AoEsperarProcesso := nil;
    HabilitarLink(lbCompilar, True);
    FCompilando := False;
  end;
end;

procedure TTelaConferir.lbOutraClick(Sender: TObject);
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
    lbxIDEs.ItemIndex := lbxIDEs.Items.Count - 1;
    PreencherProjetos;
  finally
    vRAL.Free;
    vIDE.Free;
  end;
end;

procedure TTelaConferir.lbxIDEsSelectionChange(Sender: TObject; User: boolean);
begin
  PreencherProjetos;
end;

procedure TTelaConferir.LogarLinha(const ALinha: string);
begin
  mmSaida.Lines.Add(ALinha);
  Application.ProcessMessages;
end;

procedure TTelaConferir.PreencherIDEs;
var
  vInt: integer;
begin
  lbxIDEs.Items.Clear;
  for vInt := 0 to FIDEs.Count - 1 do
    lbxIDEs.Items.Add(FIDEs[vInt].Descricao);
  if lbxIDEs.Items.Count > 0 then
    lbxIDEs.ItemIndex := 0;
  PreencherProjetos;
  AtualizarTextos;
end;

procedure TTelaConferir.PreencherProjetos;
var
  vInt: integer;
  vExt: string;
  vDelphi: boolean;
begin
  cbProjeto.Items.Clear;
  if lbxIDEs.ItemIndex >= 0 then
  begin
    vDelphi := FIDEs[lbxIDEs.ItemIndex].IDE.Tipo = tiDelphi;
    for vInt := 0 to Estado.Projetos.Count - 1 do
    begin
      vExt := LowerCase(ExtractFileExt(Estado.Projetos[vInt]));
      // o .dproj e o .dpr sao o mesmo projeto: fica o que a IDE abre
      if (vDelphi and ((vExt = '.dproj') or (vExt = '.dpr'))) or
         ((not vDelphi) and (vExt = '.lpi')) then
        cbProjeto.Items.Add(Estado.Projetos[vInt]);
    end;
  end;
  if cbProjeto.Items.Count > 0 then
    cbProjeto.ItemIndex := 0;
  HabilitarLink(lbCompilar, cbProjeto.Items.Count > 0);
end;

function TTelaConferir.ProjetoAbsoluto: string;
begin
  Result := '';
  if cbProjeto.ItemIndex < 0 then
    Exit;
  Result := Estado.PastaProjeto + StringReplace(
    cbProjeto.Items[cbProjeto.ItemIndex], '/', PathDelim, [rfReplaceAll]);
end;

function TTelaConferir.RotuloAvancar: string;
begin
  Result := cmConcluir;
end;

end.
