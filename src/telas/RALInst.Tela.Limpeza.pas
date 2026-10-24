/// The cleanup confirmation (F19): after an uninstall, the list of what the
/// installer downloaded and no IDE uses any more, each line with its size, ALL
/// CHECKED (decision D-I4): the user unchecks what to keep. There is no undo,
/// and the dialog says so. Built in code: it is a list and two buttons.
unit RALInst.Tela.Limpeza;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Limpeza;

/// Shows the leftovers checked; True when the user confirmed (the unchecked
/// ones get Manter); False keeps everything
function ConfirmarLimpeza(ASobras: TSobras): boolean;

implementation

uses
  Forms, Controls, StdCtrls, CheckLst, ExtCtrls, ButtonPanel,
  RALInst.Tela.Mensagens;

type
  /// Keeps the total line in step with the checks.
  TContador = class
  public
    Lista: TCheckListBox;
    Sobras: TSobras;
    Total: TLabel;
    procedure Mudou(Sender: TObject);
  end;

procedure TContador.Mudou(Sender: TObject);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(Lista.Count) do
    Sobras[vInt].Manter := not Lista.Checked[vInt];
  Total.Caption := Format(cmLimpezaTotal, [DescreverTamanho(Sobras.TotalBytes)]);
end;

function ConfirmarLimpeza(ASobras: TSobras): boolean;
var
  vForm: TForm;
  vTexto, vTotal: TLabel;
  vLista: TCheckListBox;
  vBotoes: TButtonPanel;
  vContador: TContador;
  vInt: integer;
begin
  Result := False;
  if ASobras.Count = 0 then
    Exit;
  vForm := TForm.Create(nil);
  vContador := TContador.Create;
  try
    vForm.Caption := cmLimpezaTitulo;
    vForm.Position := poMainFormCenter;
    vForm.BorderStyle := bsDialog;
    vForm.Width := vForm.Scale96ToForm(640);
    vForm.Height := vForm.Scale96ToForm(360);

    vTexto := TLabel.Create(vForm);
    vTexto.Parent := vForm;
    vTexto.Align := alTop;
    vTexto.BorderSpacing.Around := vForm.Scale96ToForm(8);
    vTexto.WordWrap := True;
    vTexto.AutoSize := True;
    vTexto.Caption := cmLimpezaPergunta;

    vBotoes := TButtonPanel.Create(vForm);
    vBotoes.Parent := vForm;
    vBotoes.ShowButtons := [pbOK, pbCancel];
    vBotoes.OKButton.Caption := cmLimpezaApagar;
    vBotoes.CancelButton.Caption := cmLimpezaManterTudo;

    vTotal := TLabel.Create(vForm);
    vTotal.Parent := vForm;
    vTotal.Align := alBottom;
    vTotal.BorderSpacing.Around := vForm.Scale96ToForm(8);

    vLista := TCheckListBox.Create(vForm);
    vLista.Parent := vForm;
    vLista.Align := alClient;
    vLista.BorderSpacing.Left := vForm.Scale96ToForm(8);
    vLista.BorderSpacing.Right := vForm.Scale96ToForm(8);
    // tudo marcado (D-I4): o usuario desmarca o que quer manter
    for vInt := 0 to Pred(ASobras.Count) do
    begin
      vLista.Items.Add(Format('%s  —  %s  (%s)', [ASobras[vInt].Descricao,
        DescreverTamanho(ASobras[vInt].Tamanho), ASobras[vInt].Caminho]));
      vLista.Checked[vInt] := not ASobras[vInt].Manter;
    end;
    vContador.Lista := vLista;
    vContador.Sobras := ASobras;
    vContador.Total := vTotal;
    vLista.OnClickCheck := @vContador.Mudou;
    vContador.Mudou(nil);

    Result := vForm.ShowModal = mrOK;
    if Result then
      for vInt := 0 to Pred(vLista.Count) do
        ASobras[vInt].Manter := not vLista.Checked[vInt];
  finally
    vContador.Free;
    vForm.Free;
  end;
end;

end.
