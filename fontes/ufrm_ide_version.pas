unit ufrm_ide_version;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  ideutils, RALInst.IDE;

type

  { Tfrm_ide_version }

  Tfrm_ide_version = class(TFrame)
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
    FObjectData : TIDEObjectData;
  protected
    procedure SetObjectData(AObjectData : TIDEObjectData);
  public
    constructor Create(AOwner : TComponent; AObjectData : TIDEObjectData); reintroduce;
    // reescreve os textos a partir da instancia (os avisos mudam quando outra
    // IDE entra na lista)
    procedure Atualizar;
    destructor Destroy; override;

    // False se a IDE esta marcada e a instalacao falhou
    function installRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;

    property ObjectData : TIDEObjectData read FObjectData;
  end;

implementation

{$R *.lfm}

{ Tfrm_ide_version }

procedure Tfrm_ide_version.SetObjectData(AObjectData: TIDEObjectData);
begin
  FObjectData := AObjectData;
  imIcone.Picture.Assign(FObjectData.Icon);
  Atualizar;
end;

procedure Tfrm_ide_version.Atualizar;
var
  vIDE: TIDEInstance;
  vDica: TStringList;
begin
  vIDE := FObjectData.Instancia;

  lbIDEName.Caption := vIDE.Nome;
  lbPath.Caption := ExcludeTrailingPathDelimiter(vIDE.RootDir);
  if vIDE.Avisos.Count > 0 then
    lbPath.Caption := lbPath.Caption + Format('  (%d aviso(s))', [vIDE.Avisos.Count]);

  // o detalhe fica na dica: plataformas, configuracao e o porque de cada aviso
  vDica := TStringList.Create;
  try
    vDica.Add(vIDE.Descricao);
    vDica.Add('Origem: ' + NomeOrigem(vIDE.Origem));
    if vIDE.ConfigDir <> '' then
      vDica.Add('Configuração: ' + vIDE.ConfigDir + ' (' + vIDE.ConfigOrigem + ')');
    if vIDE.RegKey <> '' then
      vDica.Add('Registro: HKCU' + vIDE.RegKey);
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
  // library path — o motivo esta nos avisos da dica
  ckSelecionado.Enabled := (ciCompilar in vIDE.Capacidades) and
    ((vIDE.Tipo <> tiDelphi) or (ciLibraryPath in vIDE.Capacidades));
  if not ckSelecionado.Enabled then
    ckSelecionado.Checked := False;
end;

constructor Tfrm_ide_version.Create(AOwner: TComponent; AObjectData: TIDEObjectData);
begin
  inherited Create(AOwner);
  SetObjectData(AObjectData);
end;

destructor Tfrm_ide_version.Destroy;
begin
  // a TIDEInstance pertence a lista da tela; so a casca e liberada aqui
  FreeAndNil(FObjectData);
  inherited Destroy;
end;

function Tfrm_ide_version.installRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
begin
  Result := True;
  if ckSelecionado.Checked then
    Result := FObjectData.InstallRAL(ALog, AEscolha);
end;

end.

