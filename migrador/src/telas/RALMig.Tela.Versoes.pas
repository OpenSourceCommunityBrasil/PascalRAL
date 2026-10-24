/// Version page (source detected with its evidence, target, "apply from the
/// chosen version") and the binary forms page (convert them or leave them out).
unit RALMig.Tela.Versoes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms,
  RALMig.Tela.Estado, RALMig.Tela.Modelo;

type
  { TPaginaVersoes }

  /// Source and target versions.
  TPaginaVersoes = class(TPaginaMig)
  private
    FCaixaForcar: TCheckBox;
    FComboDe: TComboBox;
    FComboPara: TComboBox;
    FDetectada: TLabel;
    FEvidencias: TMemo;
    FLbDe: TLabel;
    FLbEvidencias: TLabel;
    FLbPara: TLabel;
    procedure ComboChange(Sender: TObject);
    procedure ForcarClick(Sender: TObject);
    procedure MostrarDeteccao;
    procedure SelecionarNoCombo(ACombo: TComboBox; const AVersao: string);
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function PodeAvancar(out AMotivo: string): boolean; override;
  end;

  { TPaginaBinarios }

  /// Binary forms: convert to text or leave out (D-M6).
  TPaginaBinarios = class(TPaginaMig)
  private
    FConverter: TRadioButton;
    FDeixar: TRadioButton;
    FExplicacao: TLabel;
    FLista: TListBox;
    FNota: TLabel;
    procedure EscolhaClick(Sender: TObject);
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    procedure AoMostrar; override;
    function AoAvancar: boolean; override;
    procedure AtualizarTextos; override;
    function Pular: boolean; override;
  end;

implementation

uses
  RALMig.Deteccao, RALMig.Regras, RALMig.Tela.Mensagens;

{ TPaginaVersoes }

constructor TPaginaVersoes.Criar(AOwner: TComponent; AEstado: TEstadoMig);
var
  vInt: integer;
begin
  inherited Criar(AOwner, AEstado);
  FLbDe := NovoRotulo(MARGEM, 190, 300);
  FComboDe := TComboBox.Create(Self);
  FComboDe.Parent := Self;
  Colocar(FComboDe, MARGEM, 212, 200, 28);
  FLbPara := NovoRotulo(MARGEM + 250, 190, 300);
  FComboPara := TComboBox.Create(Self);
  FComboPara.Parent := Self;
  Colocar(FComboPara, MARGEM + 250, 212, 200, 28);
  for vInt := Low(VersoesDeApi) to High(VersoesDeApi) do
  begin
    FComboDe.Items.Add(VersoesDeApi[vInt]);
    FComboPara.Items.Add(VersoesDeApi[vInt]);
  end;
  FComboDe.Style := csDropDownList;
  FComboPara.Style := csDropDownList;
  FComboDe.OnChange := @ComboChange;
  FComboPara.OnChange := @ComboChange;
  FDetectada := NovoRotulo(MARGEM, 254, LARGURA_UTIL, 10, True);
  FLbEvidencias := NovoRotulo(MARGEM, 284, LARGURA_UTIL);
  FEvidencias := NovoMemo(Self, MARGEM, 306, LARGURA_UTIL, 176);
  FCaixaForcar := NovaCaixa(MARGEM, 494, LARGURA_UTIL);
  FCaixaForcar.OnClick := @ForcarClick;
end;

procedure TPaginaVersoes.AoMostrar;
begin
  Screen.Cursor := crHourGlass;
  try
    Estado.Detectar;
  finally
    Screen.Cursor := crDefault;
  end;
  // o que veio da linha de comando fica; o resto parte do que se detectou
  if Estado.De = '' then
    Estado.De := VersaoDeLista(Estado.VersaoDetectada);
  if Estado.Para = '' then
    Estado.Para := VersoesDeApi[High(VersoesDeApi)];
  SelecionarNoCombo(FComboDe, Estado.De);
  SelecionarNoCombo(FComboPara, Estado.Para);
  FCaixaForcar.Checked := Estado.ForcarDe;
  MostrarDeteccao;
  Mudou;
end;

procedure TPaginaVersoes.AtualizarTextos;
begin
  Titular(cmVersoesTitulo, cmVersoesSub);
  FLbDe.Caption := cmVersaoDe;
  FLbPara.Caption := cmVersaoPara;
  FLbEvidencias.Caption := cmEvidencias;
  FCaixaForcar.Caption := cmForcarDe;
  MostrarDeteccao;
end;

procedure TPaginaVersoes.ComboChange(Sender: TObject);
begin
  if FComboDe.ItemIndex >= 0 then
    Estado.De := FComboDe.Items[FComboDe.ItemIndex];
  if FComboPara.ItemIndex >= 0 then
    Estado.Para := FComboPara.Items[FComboPara.ItemIndex];
  Mudou;
end;

procedure TPaginaVersoes.ForcarClick(Sender: TObject);
begin
  Estado.ForcarDe := FCaixaForcar.Checked;
end;

procedure TPaginaVersoes.MostrarDeteccao;
var
  vConfianca: string;
begin
  FEvidencias.Lines.Assign(Estado.Evidencias);
  if Estado.VersaoDetectada = '' then
  begin
    FDetectada.Caption := cmNaoDetectada;
    Exit;
  end;
  case Estado.Confianca of
    cfAlta: vConfianca := cmConfAlta;
    cfMedia: vConfianca := cmConfMedia;
  else
    vConfianca := cmConfBaixa;
  end;
  FDetectada.Caption := Format(cmDetectada, [Estado.VersaoDetectada, vConfianca]);
  if Estado.DeInformado and (VersaoDeLista(Estado.VersaoDetectada) <> Estado.De) then
    FDetectada.Caption := FDetectada.Caption + ' ' + cmDeInformado;
end;

function TPaginaVersoes.PodeAvancar(out AMotivo: string): boolean;
begin
  Result := False;
  AMotivo := '';
  if (Estado.De = '') or (Estado.Para = '') then
    AMotivo := emEscolhaVersoes
  else if Estado.DestinoMaisVelho then
    AMotivo := Format(emDestinoMaisVelho, [Estado.Para, Estado.De])
  else if NormalizarVersao(Estado.De) = NormalizarVersao(Estado.Para) then
    AMotivo := emMesmaVersao
  else
    Result := True;
end;

procedure TPaginaVersoes.SelecionarNoCombo(ACombo: TComboBox; const AVersao: string);
begin
  ACombo.ItemIndex := ACombo.Items.IndexOf(VersaoDeLista(AVersao));
end;

{ TPaginaBinarios }

constructor TPaginaBinarios.Criar(AOwner: TComponent; AEstado: TEstadoMig);
begin
  inherited Criar(AOwner, AEstado);
  FExplicacao := NovoRotulo(MARGEM, 190, LARGURA_UTIL);
  Colocar(FExplicacao, MARGEM, 190, LARGURA_UTIL, 56);
  FLista := NovaLista(MARGEM, 256, LARGURA_UTIL, 170);
  FConverter := TRadioButton.Create(Self);
  FConverter.Parent := Self;
  Colocar(FConverter, MARGEM, 440, LARGURA_UTIL, 24);
  FConverter.Checked := True;
  FConverter.OnClick := @EscolhaClick;
  FDeixar := TRadioButton.Create(Self);
  FDeixar.Parent := Self;
  Colocar(FDeixar, MARGEM, 470, LARGURA_UTIL, 24);
  FDeixar.OnClick := @EscolhaClick;
  FNota := NovoRotulo(MARGEM, 504, LARGURA_UTIL);
  Colocar(FNota, MARGEM, 504, LARGURA_UTIL, 56);
end;

procedure TPaginaBinarios.AoMostrar;
begin
  FLista.Items.Assign(Estado.Binarios);
  FConverter.Checked := Estado.ConverterBinarios;
  FDeixar.Checked := not Estado.ConverterBinarios;
end;

function TPaginaBinarios.AoAvancar: boolean;
begin
  Estado.ConverterBinarios := FConverter.Checked;
  Result := True;
end;

procedure TPaginaBinarios.AtualizarTextos;
begin
  Titular(cmBinTitulo, cmBinSub);
  FExplicacao.Caption := cmBinExplicacao;
  FConverter.Caption := cmBinConverter;
  FDeixar.Caption := cmBinDeixar;
  FNota.Caption := cmBinNota;
end;

procedure TPaginaBinarios.EscolhaClick(Sender: TObject);
begin
  Estado.ConverterBinarios := FConverter.Checked;
end;

function TPaginaBinarios.Pular: boolean;
begin
  Result := Estado.Binarios.Count = 0;
end;

end.
