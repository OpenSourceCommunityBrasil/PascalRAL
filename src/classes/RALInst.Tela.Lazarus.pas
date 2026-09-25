/// Screen shell of a Lazarus IDE. The run (links, checked packages and one
/// --build-ide at the end, with that installation's configuration) lives in
/// RALInst.Instalar.Lazarus.
unit RALInst.Tela.Lazarus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.Instalar.Lazarus, RALInst.Receitas, RALInst.Tela.Instalacao;

type
  /// Shell of a Lazarus installation.
  TIDETelaLazarus = class(TIDETela)
  private
    /// The core engine, configured with the user's choice
    function Criar(AEscolha: TEscolhaInstalacao): TInstalacaoLazarus;
  protected
    function Desinstalar(AEscolha: TEscolhaInstalacao): boolean; override;
    function Install(AEscolha: TEscolhaInstalacao): boolean; override;
  public
    function DependenciaInstalada(AReceita: TReceita;
      AEscolha: TEscolhaInstalacao): string; override;
    function PastaDependencia(AReceita: TReceita;
      AEscolha: TEscolhaInstalacao): string; override;
    function Plano(AEscolha: TEscolhaInstalacao): string; override;
  end;

implementation

uses
  RALInst.Mensagens, RALInst.Tela.Mensagens;

{ TIDETelaLazarus }

function TIDETelaLazarus.Criar(AEscolha: TEscolhaInstalacao): TInstalacaoLazarus;
begin
  Result := TInstalacaoLazarus.Create(Instancia, AEscolha.Catalogo);
  AEscolha.PacotesDoTipo(tpLazarus, Result.Pacotes);
  if AEscolha.PastaFontes <> '' then
    Result.RaizFontes := AEscolha.PastaFontes;
  Result.Receitas := AEscolha.Receitas;
  Result.PastasDependencias.Assign(AEscolha.PastasDependencias);
  Result.Manifesto := AEscolha.Manifesto;
  Result.Log := @LogarLinha;
end;

function TIDETelaLazarus.DependenciaInstalada(AReceita: TReceita;
  AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoLazarus;
begin
  vInst := Criar(AEscolha);
  try
    vInst.Log := nil;
    Result := vInst.DependenciaInstalada(AReceita);
  finally
    vInst.Free;
  end;
end;

function TIDETelaLazarus.Desinstalar(AEscolha: TEscolhaInstalacao): boolean;
var
  vInst: TInstalacaoLazarus;
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
function TIDETelaLazarus.Install(AEscolha: TEscolhaInstalacao): boolean;
var
  vInst: TInstalacaoLazarus;
begin
  vInst := Criar(AEscolha);
  try
    // numa rodada com as duas IDEs, pode nao haver nada deste lado
    if vInst.Pacotes.Count = 0 then
    begin
      LogarLinha(Format(cmNenhumRecursoExiste, ['Lazarus']));
      Resumo := Format(cmNadaAInstalarTipo, [Name, 'Lazarus']);
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

function TIDETelaLazarus.PastaDependencia(AReceita: TReceita;
  AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoLazarus;
begin
  vInst := Criar(AEscolha);
  try
    vInst.Log := nil;
    Result := vInst.PastaDependencia(AReceita);
  finally
    vInst.Free;
  end;
end;

function TIDETelaLazarus.Plano(AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoLazarus;
begin
  vInst := Criar(AEscolha);
  try
    vInst.Log := nil;
    if AEscolha.Desinstalar then
      Exit(vInst.PlanoDesinstalar);
    if vInst.Pacotes.Count = 0 then
      Exit(Format(cmNenhumRecursoExistePlano, [Name, 'Lazarus']) + LineEnding);
    Result := vInst.Plano;
  finally
    vInst.Free;
  end;
end;

end.
