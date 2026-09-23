unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ufrm_modelo, ufrm_idioma, ufrm_ide, ufrm_ide_versions, ufrm_recursos,
  ufrm_install, utools, i18n_utils;

type

  { Tfmain }

  Tfmain = class(TForm)
    bTranslate: TButton;
    ntPages: TNotebook;
    procedure bTranslateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPgIdioma: Tfrm_idioma;
    FPgIDE: Tfrm_ide;
    FPgIDEVersions: Tfrm_ide_versions;
    FPgRecursos: Tfrm_recursos;
    FPgInstall: Tfrm_install;

    FLanguage: TLanguages;
    FPage: integer;
    FTheme: TThemes;
    FIDE: integer;

    function criarFrame(AClass : Tfrm_modelo_class) : Tfrm_modelo;
    procedure SetIDE(AValue: integer);
    procedure SetLanguage(AValue: TLanguages);
    procedure SetPage(AValue: integer);
    procedure SetTheme(AValue: TThemes);

    procedure translate(ALang : string);
    // F12: depois que a janela aparece, uma vez
    procedure VerificarAtualizacao(AData: PtrInt);
    // o numero da versao no cabecalho de cada pagina vem do codigo, nao da
    // traducao (que dizia 1.0); chamado depois de toda traducao
    procedure AjustarVersao;
  public
    procedure NextPage;
    procedure PriorPage;

    // o plano de todas as IDEs marcadas com os recursos escolhidos
    function PlanoInstalacao: string;
    function installRAL(ALog : TMemo): boolean;
    // F10: o que o instalador ja instalou nas IDEs marcadas (os recibos), uma
    // linha por IDE; vazio se nada
    function InstalacoesExistentes: string;
    // F10: desfaz as instalacoes registradas nas IDEs marcadas, da mais nova
    // a mais velha
    function DesinstalarRAL: boolean;
  published
    property Theme : TThemes read FTheme write SetTheme;
    property Language : TLanguages read FLanguage write SetLanguage;
    property IDE : integer read FIDE write SetIDE;
    property Page : integer read FPage write SetPage;
  end;

var
  fmain: Tfmain;

implementation

{$R *.lfm}

uses
  ideutils, RALInst.Processo, RALInst.Recibos, RALInst.Versao, RALInst.AutoAtualizacao;

{ Tfmain }

procedure Tfmain.FormCreate(Sender: TObject);
begin
  FPgIdioma := Tfrm_idioma(criarFrame(Tfrm_idioma));
  FPgIDE := Tfrm_ide(criarFrame(Tfrm_ide));
  FPgIDEVersions := Tfrm_ide_versions(criarFrame(Tfrm_ide_versions));
  FPgRecursos := Tfrm_recursos(criarFrame(Tfrm_recursos));
  FPgInstall := Tfrm_install(criarFrame(Tfrm_install));
  // F6: a tela de recursos confere cada pacote contra as IDEs marcadas
  FPgRecursos.OnListarIDEs := @FPgIDEVersions.ListarMarcadas;

  Theme := tDark;
  Language := lEnglish;
  IDE := -1;

  // F12: a versao na barra de titulo; o .old de uma atualizacao anterior sai;
  // a consulta ao GitHub so depois que a janela aparece
  Caption := 'RAL Installer ' + VersaoInstalador;
  AjustarVersao;
  LimparAtualizacaoAnterior;
  Application.QueueAsyncCall(@VerificarAtualizacao, 0);
end;

procedure Tfmain.AjustarVersao;
var
  vInt: integer;
  vRotulo: TLabel;
  vTexto: string;
begin
  for vInt := 0 to Pred(ComponentCount) do
    if Components[vInt] is Tfrm_modelo then
    begin
      vRotulo := Tfrm_modelo(Components[vInt]).lVersion;
      // 'Installer Version 1.0' -> 'Installer Version 0.9.0', na lingua da traducao
      vTexto := TrimRight(vRotulo.Caption);
      if (vTexto <> '') and (LastDelimiter(' ', vTexto) > 0) then
        vRotulo.Caption := Copy(vTexto, 1, LastDelimiter(' ', vTexto)) + VersaoInstalador;
    end;
end;

procedure Tfmain.VerificarAtualizacao(AData: PtrInt);
var
  vAtu: TAtualizacao;
  vNovo: string;
  vInt: integer;
begin
  for vInt := 1 to ParamCount do
    if ParamStr(vInt) = '--pos-atualizacao' then
    begin
      ShowMessage('O RAL Installer foi atualizado para a versão ' + VersaoInstalador + '.');
      Exit;
    end;

  vAtu := TAtualizacao.Create;
  try
    Screen.Cursor := crHourGlass;
    try
      case vAtu.Verificar of
        rvAtualizado:
          Exit;
        rvNaoVerificou:
          begin
            // sem internet ou sem cota nao e "esta atualizado": diz que nao deu
            Caption := 'RAL Installer ' + VersaoInstalador +
                       ' — não foi possível verificar se há versão nova';
            Hint := vAtu.Erro;
            ShowHint := True;
            Exit;
          end;
      end;
    finally
      Screen.Cursor := crDefault;
    end;

    if MessageDlg('Versão nova do RAL Installer',
         Format('A versão %s está disponível (esta é a %s).', [vAtu.VersaoNova, VersaoInstalador]) +
         LineEnding + LineEnding + Copy(vAtu.Notas, 1, 600) + LineEnding + LineEnding +
         'Baixar e reiniciar agora?', mtInformation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

    Screen.Cursor := crHourGlass;
    try
      if not vAtu.Baixar(vNovo) or not vAtu.Trocar(vNovo) then
      begin
        Screen.Cursor := crDefault;
        ShowMessage('A atualização não foi feita: ' + vAtu.Erro + LineEnding +
                    'O instalador continua na versão ' + VersaoInstalador + '.');
        Exit;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    if vAtu.Reiniciar(['--pos-atualizacao']) then
      Application.Terminate
    else
      ShowMessage('Atualizado para a versão ' + vAtu.VersaoNova + '; abra o instalador de novo. (' +
                  vAtu.Erro + ')');
  finally
    vAtu.Free;
  end;
end;

procedure Tfmain.bTranslateClick(Sender: TObject);
begin
  translate('pt-BR');
  translate('en-US');
  translate('es-ES');
end;

function Tfmain.criarFrame(AClass: Tfrm_modelo_class): Tfrm_modelo;
var
  vPage : TPage;
begin
  vPage := TPage.Create(Self);
  vPage.Parent := ntPages;

  Result := AClass.Create(Self);
  Result.Parent := vPage;
  Result.Align := alClient;
end;

procedure Tfmain.SetIDE(AValue: integer);
begin
  if FIDE = AValue then
    Exit;

  FIDE := AValue;

  FPgIDE.IDE := AValue;
  FPgIDEVersions.IDE := AValue;
  FPgRecursos.IDE := AValue;
end;

procedure Tfmain.SetLanguage(AValue: TLanguages);
begin
  if FLanguage = AValue then
    Exit;

  FLanguage := AValue;

  FPgIdioma.Language := AValue;
  FPgIDE.Language := AValue;
  FPgIDEVersions.Language := AValue;
  FPgRecursos.Language := AValue;
  FPgInstall.Language := AValue;
  AjustarVersao;
end;

procedure Tfmain.SetPage(AValue: integer);
begin
  if FPage = AValue then
    Exit;

  if (AValue >= 0) and (AValue < ntPages.PageCount) then
  begin
    FPage := AValue;
    ntPages.PageIndex := AValue;
    if (ntPages.Page[AValue].ControlCount > 0) and
       (ntPages.Page[AValue].Controls[0] is Tfrm_modelo) then
      Tfrm_modelo(ntPages.Page[AValue].Controls[0]).AoMostrar;
  end;
end;

procedure Tfmain.SetTheme(AValue: TThemes);
begin
  if FTheme = AValue then
    Exit;

  FTheme := AValue;

  FPgIdioma.Theme := AValue;
  FPgIDE.Theme := AValue;
  FPgIDEVersions.Theme := AValue;
  FPgRecursos.Theme := AValue;
  FPgInstall.Theme := AValue;
end;

procedure Tfmain.translate(ALang: string);
var
  vTrans : TTranslate;
  vStream : TStream;
  vFileStream : TFileStream;
begin
  vTrans := TTranslate.Create;
  try
    vTrans.Lang := ALang;
    vTrans.addComponentes(Self);
    vTrans.addUnit('utools');

    vStream := vTrans.SaveToStream;
    try
      vFileStream := TFileStream.Create('./languages/ralinstaller.'+ALang+'.po', fmCreate);
      try
        vFileStream.CopyFrom(vStream, vStream.Size);
      finally
        FreeAndNil(vFileStream);
      end;
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vTrans);
  end;
end;

procedure Tfmain.NextPage;
begin
  Page := FPage + 1;
end;

procedure Tfmain.PriorPage;
begin
  Page := FPage - 1;
end;

function Tfmain.PlanoInstalacao: string;
begin
  // as dependencias primeiro: o plano de cada IDE precisa saber para onde
  // cada uma vai ser baixada
  Result := FPgRecursos.PlanoFontes +
            FPgRecursos.PlanejarDependencias + LineEnding +
            FPgIDEVersions.Plano(FPgRecursos.Escolha);
end;

function Tfmain.InstalacoesExistentes: string;
var
  vIDEs, vDaIDE: TList;
  vRecibos: TRecibos;
  vInt: integer;
  vIDE: TIDEObjectData;
begin
  Result := '';
  vIDEs := TList.Create;
  vDaIDE := TList.Create;
  vRecibos := TRecibos.Create(True);
  try
    vRecibos.Carregar(PastaDadosInstalador + 'recibos');
    FPgIDEVersions.ListarMarcadas(vIDEs);
    for vInt := 0 to Pred(vIDEs.Count) do
    begin
      vIDE := TIDEObjectData(vIDEs[vInt]);
      vRecibos.DaIDE(vIDE.Instancia.RootDir, vDaIDE);
      if vDaIDE.Count = 0 then
        Continue;
      Result := Result + '  ' + TRecibo(vDaIDE[0]).Descricao;
      if vDaIDE.Count > 1 then
        Result := Result + Format(' (e mais %d instalação(ões) antes)', [vDaIDE.Count - 1]);
      Result := Result + LineEnding;
    end;
  finally
    vRecibos.Free;
    vDaIDE.Free;
    vIDEs.Free;
  end;
end;

function Tfmain.DesinstalarRAL: boolean;
var
  vIDEs: TList;
  vInt: integer;
  vIDE: TIDEObjectData;
begin
  Result := True;
  vIDEs := TList.Create;
  try
    FPgIDEVersions.ListarMarcadas(vIDEs);
    for vInt := 0 to Pred(vIDEs.Count) do
    begin
      vIDE := TIDEObjectData(vIDEs[vInt]);
      FPgInstall.LogarLinha('==== ' + vIDE.Name + ' (' +
        ExcludeTrailingPathDelimiter(vIDE.Instancia.RootDir) + ')');
      if not DesinstalarIDE(PastaDadosInstalador + 'recibos', vIDE.Instancia.RootDir,
                            @FPgInstall.LogarLinha, True) then
        Result := False;
      FPgInstall.LogarLinha('');
    end;
  finally
    vIDEs.Free;
  end;
end;

function Tfmain.installRAL(ALog: TMemo): boolean;
begin
  // etapa 1: os fontes na pasta final; se falhar, nenhuma IDE e tocada
  Result := FPgRecursos.PrepararFontes(@FPgInstall.LogarLinha);
  if not Result then
    Exit;
  // etapa 1b: as dependencias que alguma IDE nao tem; o que falhar deixa de
  // fora so os pacotes que precisavam dela
  if not FPgRecursos.BaixarDependencias(@FPgInstall.LogarLinha) then
    Result := False;
  FPgInstall.LogarLinha('');
  // etapa 2: cada IDE marcada
  if not FPgIDEVersions.installRAL(ALog, FPgRecursos.Escolha) then
    Result := False;
  // F9: o relatorio final, por IDE: o que entrou, o que ficou de fora e por que
  FPgInstall.LogarLinha('==== Resumo');
  FPgInstall.LogarLinha(TrimRight(FPgIDEVersions.Resumos));
end;

end.

