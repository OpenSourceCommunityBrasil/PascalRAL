/// Screen shell of a Delphi IDE. The rules live in the core: discovery in
/// RALInst.IDE.Delphi, compilation in RALInst.Build.Delphi, registry in
/// RALInst.Registro.Delphi and the whole run in RALInst.Instalar.Delphi.
unit RALInst.Tela.Delphi;

{$mode ObjFPC}{$H+}

{$IFNDEF MSWINDOWS}
  {$FATAL RALInst.Tela.Delphi so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.Instalar.Delphi, RALInst.Receitas, RALInst.Tela.Instalacao;

type
  /// Shell of a Delphi installation.
  TIDETelaDelphi = class(TIDETela)
  private
    /// The core engine, configured with the user's choice
    function Criar(AEscolha: TEscolhaInstalacao): TInstalacaoDelphi;
  protected
    function Desinstalar(AEscolha: TEscolhaInstalacao): boolean; override;
    function Install(AEscolha: TEscolhaInstalacao): boolean; override;
  public
    function DependenciaInstalada(AReceita: TReceita;
      AEscolha: TEscolhaInstalacao): string; override;
    function Plano(AEscolha: TEscolhaInstalacao): string; override;
  end;

implementation

uses
  RALInst.Mensagens, RALInst.Tela.Mensagens;

{ TIDETelaDelphi }

function TIDETelaDelphi.Criar(AEscolha: TEscolhaInstalacao): TInstalacaoDelphi;
begin
  Result := TInstalacaoDelphi.Create(Instancia, AEscolha.Catalogo);
  AEscolha.PacotesDoTipo(tpDelphi, Result.Pacotes);
  if AEscolha.PastaFontes <> '' then
    Result.RaizFontes := AEscolha.PastaFontes;
  Result.Receitas := AEscolha.Receitas;
  Result.PastasDependencias.Assign(AEscolha.PastasDependencias);
  Result.Manifesto := AEscolha.Manifesto;
  Result.SomenteLibraryPath := AEscolha.SomenteLibraryPath;
  if AEscolha.Win64 and (Instancia.Plataformas.IndexOf('win64') >= 0) then
    Result.Plataformas.Add('win64');
  Result.Log := @LogarLinha;
end;

function TIDETelaDelphi.DependenciaInstalada(AReceita: TReceita;
  AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoDelphi;
begin
  vInst := Criar(AEscolha);
  try
    vInst.Log := nil;
    Result := vInst.DependenciaInstalada(AReceita);
  finally
    vInst.Free;
  end;
end;

function TIDETelaDelphi.Desinstalar(AEscolha: TEscolhaInstalacao): boolean;
var
  vInst: TInstalacaoDelphi;
  vInt: integer;
begin
  vInst := Criar(AEscolha);
  try
    if not TemRAL then
    begin
      LogarLinha(cmDesinstalarNadaAFazer);
      Resumo := Format(cmResumoSemRAL, [Name]);
      Exit(True);
    end;
    Result := vInst.Desinstalar;
    if vInst.Relatorio.Count > 0 then
    begin
      LogarLinha('');
      LogarLinha(TrimRight(vInst.Relatorio.Text));
    end;
    if Result then
      Resumo := Format(cmResumoDesinstalado, [Name])
    else
      Resumo := Format(cmResumoDesinstalarErro, [Name]);
    for vInt := 0 to Pred(vInst.Avisos.Count) do
      Resumo := Resumo + LineEnding + '    ' + vInst.Avisos[vInt];
  finally
    vInst.Free;
  end;
end;
function TIDETelaDelphi.Install(AEscolha: TEscolhaInstalacao): boolean;
var
  vInst: TInstalacaoDelphi;
begin
  vInst := Criar(AEscolha);
  try
    // numa rodada com as duas IDEs, pode nao haver nada deste lado
    if vInst.Pacotes.Count = 0 then
    begin
      LogarLinha(Format(cmNenhumRecursoExiste, ['Delphi']));
      Resumo := Format(cmNadaAInstalarTipo, [Name, 'Delphi']);
      Exit(True);
    end;
    Result := vInst.Executar;
    if vInst.Relatorio.Count > 0 then
    begin
      LogarLinha('');
      LogarLinha(TrimRight(vInst.Relatorio.Text));
    end;
    Resumir(Result, vInst.Avisos);
  finally
    vInst.Free;
  end;
end;

function TIDETelaDelphi.Plano(AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoDelphi;
begin
  vInst := Criar(AEscolha);
  try
    vInst.Log := nil;
    if AEscolha.Desinstalar then
      Exit(vInst.PlanoDesinstalar);
    if vInst.Pacotes.Count = 0 then
      Exit(Format(cmNenhumRecursoExistePlano, [Name, 'Delphi']) + LineEnding);
    Result := vInst.Plano;
  finally
    vInst.Free;
  end;
end;

end.
