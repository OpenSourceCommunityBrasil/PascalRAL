unit lazarusutils;

{$mode ObjFPC}{$H+}

// Casca de UI do Lazarus. A rodada (links, pacotes marcados e um --build-ide
// no fim, com a configuracao daquela instalacao) mora em
// RALInst.Instalar.Lazarus.

interface

uses
  Classes, SysUtils,
  ideutils, RALInst.Instalar.Lazarus, RALInst.Receitas, RALInst.Catalogo;

type

  { TLazarusObjectData }

  TLazarusObjectData = class(TIDEObjectData)
  private
    function Criar(AEscolha: TEscolhaInstalacao): TInstalacaoLazarus;
  protected
    function Install(AEscolha: TEscolhaInstalacao): boolean; override;
  public
    function Plano(AEscolha: TEscolhaInstalacao): string; override;
    function DependenciaInstalada(AReceita: TReceita; AEscolha: TEscolhaInstalacao): string; override;
    function PastaDependencia(AReceita: TReceita; AEscolha: TEscolhaInstalacao): string; override;
  end;

implementation

{ TLazarusObjectData }

function TLazarusObjectData.Criar(AEscolha: TEscolhaInstalacao): TInstalacaoLazarus;
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

function TLazarusObjectData.Install(AEscolha: TEscolhaInstalacao): boolean;
var
  vInst: TInstalacaoLazarus;
begin
  vInst := Criar(AEscolha);
  try
    // F9: numa rodada com as duas IDEs, pode nao haver nada deste lado
    if vInst.Pacotes.Count = 0 then
    begin
      LogarLinha('Nenhum dos recursos escolhidos existe no Lazarus.');
      Resumo := Name + ': nada a instalar (os recursos escolhidos não existem no Lazarus)';
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

function TLazarusObjectData.DependenciaInstalada(AReceita: TReceita;
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

function TLazarusObjectData.PastaDependencia(AReceita: TReceita;
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

function TLazarusObjectData.Plano(AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoLazarus;
begin
  vInst := Criar(AEscolha);
  try
    vInst.Log := nil;
    if vInst.Pacotes.Count = 0 then
      Exit(Name + ': nenhum dos recursos escolhidos existe no Lazarus' + LineEnding);
    Result := vInst.Plano;
  finally
    vInst.Free;
  end;
end;

end.
