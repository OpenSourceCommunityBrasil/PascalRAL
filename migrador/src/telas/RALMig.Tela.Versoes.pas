/// Version page: the source detected with its evidence, the target, and "apply
/// from the chosen version".
unit RALMig.Tela.Versoes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms,
  RALMig.Tela.Pagina;

type
  /// Source and target versions.
  TTelaVersoes = class(TTelaMig)
    cbDe: TComboBox;
    cbPara: TComboBox;
    ckForcar: TCheckBox;
    lbDe: TLabel;
    lbDetectada: TLabel;
    lbEvidencias: TLabel;
    lbForcar: TLabel;
    lbPara: TLabel;
    mmEvidencias: TMemo;
    procedure cbDeChange(Sender: TObject);
    procedure ckForcarClick(Sender: TObject);
  private
    procedure MostrarDeteccao;
    procedure SelecionarNoCombo(ACombo: TComboBox; const AVersao: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function PodeAvancar(out AMotivo: string): boolean; override;
  end;

implementation

{$R *.lfm}

uses
  RALMig.Deteccao, RALMig.Regras, RALMig.Tela.Estado, RALMig.Tela.Mensagens;

{ TTelaVersoes }

constructor TTelaVersoes.Create(AOwner: TComponent);
var
  vInt: integer;
begin
  inherited Create(AOwner);
  for vInt := Low(VersoesDeApi) to High(VersoesDeApi) do
  begin
    cbDe.Items.Add(VersoesDeApi[vInt]);
    cbPara.Items.Add(VersoesDeApi[vInt]);
  end;
end;

procedure TTelaVersoes.AoMostrar;
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
  SelecionarNoCombo(cbDe, Estado.De);
  SelecionarNoCombo(cbPara, Estado.Para);
  ckForcar.Checked := Estado.ForcarDe;
  MostrarDeteccao;
  Mudou;
end;

procedure TTelaVersoes.AtualizarTextos;
begin
  inherited AtualizarTextos;
  Titular(cmVersoesTitulo, cmVersoesSub);
  lbDe.Caption := cmVersaoDe;
  lbPara.Caption := cmVersaoPara;
  lbEvidencias.Caption := cmEvidencias;
  lbForcar.Caption := cmForcarDe;
  MostrarDeteccao;
end;

procedure TTelaVersoes.cbDeChange(Sender: TObject);
begin
  if cbDe.ItemIndex >= 0 then
    Estado.De := cbDe.Items[cbDe.ItemIndex];
  if cbPara.ItemIndex >= 0 then
    Estado.Para := cbPara.Items[cbPara.ItemIndex];
  Mudou;
end;

procedure TTelaVersoes.ckForcarClick(Sender: TObject);
begin
  Estado.ForcarDe := ckForcar.Checked;
end;

procedure TTelaVersoes.MostrarDeteccao;
var
  vConfianca: string;
begin
  mmEvidencias.Lines.Assign(Estado.Evidencias);
  if Estado.VersaoDetectada = '' then
  begin
    lbDetectada.Caption := cmNaoDetectada;
    Exit;
  end;
  case Estado.Confianca of
    cfAlta: vConfianca := cmConfAlta;
    cfMedia: vConfianca := cmConfMedia;
  else
    vConfianca := cmConfBaixa;
  end;
  lbDetectada.Caption := Format(cmDetectada, [Estado.VersaoDetectada, vConfianca]);
  if Estado.DeInformado and (VersaoDeLista(Estado.VersaoDetectada) <> Estado.De) then
    lbDetectada.Caption := lbDetectada.Caption + ' ' + cmDeInformado;
end;

function TTelaVersoes.PodeAvancar(out AMotivo: string): boolean;
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

procedure TTelaVersoes.SelecionarNoCombo(ACombo: TComboBox; const AVersao: string);
begin
  ACombo.ItemIndex := ACombo.Items.IndexOf(VersaoDeLista(AVersao));
end;

end.
