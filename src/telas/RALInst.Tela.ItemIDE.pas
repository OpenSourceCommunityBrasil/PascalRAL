/// One row of the IDE list: check box, icon, name, folder, warnings and the RAL
/// the IDE already has.
unit RALInst.Tela.ItemIDE;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  RALInst.IDE, RALInst.Tela.Instalacao;

type
  /// Row of the IDE list; the IDE shell belongs to the row.
  TItemIDE = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ckSelecionado: TCheckBox;
    imIcone: TImage;
    lbIDEName: TLabel;
    lbPath: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    FObjectData: TIDETela;
    procedure SetObjectData(AObjectData: TIDETela);
  public
    constructor Create(AOwner: TComponent; AObjectData: TIDETela); reintroduce;
    destructor Destroy; override;
    /// Rewrites the texts from the instance (the warnings change when another
    /// IDE enters the list, and the language may have changed)
    procedure Atualizar;
    /// False when the IDE is checked and the installation failed
    function InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;

    property ObjectData: TIDETela read FObjectData;
  end;

implementation

{$R *.lfm}

uses
  RALInst.Tela.Mensagens;

{ TItemIDE }

constructor TItemIDE.Create(AOwner: TComponent; AObjectData: TIDETela);
begin
  inherited Create(AOwner);
  SetObjectData(AObjectData);
end;

destructor TItemIDE.Destroy;
begin
  // a TIDEInstance pertence a lista da tela; so a casca e liberada aqui
  FreeAndNil(FObjectData);
  inherited Destroy;
end;

procedure TItemIDE.Atualizar;
var
  vIDE: TIDEInstance;
  vDica: TStringList;
begin
  vIDE := FObjectData.Instancia;

  lbIDEName.Caption := vIDE.Nome;
  lbPath.Caption := ExcludeTrailingPathDelimiter(vIDE.RootDir);
  if vIDE.Avisos.Count > 0 then
    lbPath.Caption := lbPath.Caption + Format(cmAvisosIDE, [vIDE.Avisos.Count]);
  // o RAL que a IDE ja tem, instalado pelo instalador ou a mao
  if FObjectData.TemRAL then
    lbPath.Caption := lbPath.Caption + cmRALNaIDE;

  // o detalhe fica na dica: plataformas, configuracao e o porque de cada aviso
  vDica := TStringList.Create;
  try
    vDica.Add(vIDE.Descricao);
    vDica.Add(Format(cmDicaOrigem, [NomeOrigem(vIDE.Origem)]));
    if FObjectData.TemRAL then
      vDica.Add(Format(cmDicaRAL, [FObjectData.ResumoRAL]));
    if vIDE.ConfigDir <> '' then
      vDica.Add(Format(cmDicaConfiguracao, [vIDE.ConfigDir, vIDE.ConfigOrigem]));
    if vIDE.RegKey <> '' then
      vDica.Add(Format(cmDicaRegistro, [vIDE.RegKey]));
    vDica.AddStrings(vIDE.Avisos);
    Hint := Trim(vDica.Text);
  finally
    vDica.Free;
  end;
  ShowHint := True;
  ParentShowHint := False;
  lbIDEName.ShowHint := True;
  lbPath.ShowHint := True;
  lbIDEName.Hint := Hint;
  lbPath.Hint := Hint;

  // sem compilador reconhecido nao ha o que instalar; e o Delphi precisa da
  // chave no registro (IDE aberta ao menos uma vez) para receber pacote ou
  // library path: o motivo esta nos avisos da dica
  ckSelecionado.Enabled := (ciCompilar in vIDE.Capacidades) and
    ((vIDE.Tipo <> tiDelphi) or (ciLibraryPath in vIDE.Capacidades));
  if not ckSelecionado.Enabled then
    ckSelecionado.Checked := False;
end;

function TItemIDE.InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
begin
  Result := True;
  if ckSelecionado.Checked then
    Result := FObjectData.InstallRAL(ALog, AEscolha);
end;

procedure TItemIDE.SetObjectData(AObjectData: TIDETela);
begin
  FObjectData := AObjectData;
  imIcone.Picture.Assign(FObjectData.Icon);
  Atualizar;
end;

end.
