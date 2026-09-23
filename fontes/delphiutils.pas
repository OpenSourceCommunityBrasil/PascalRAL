unit delphiutils;

{$mode ObjFPC}{$H+}

// Casca de UI do Delphi. A regra mora no nucleo: descoberta em
// RALInst.IDE.Delphi, compilacao em RALInst.Build.Delphi, registro em
// RALInst.Registro.Delphi e a rodada inteira em RALInst.Instalar.Delphi.

{$IFNDEF MSWINDOWS}
  {$FATAL delphiutils so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  ideutils, RALInst.Instalar.Delphi, RALInst.Receitas, RALInst.Catalogo;

type

  { TDelphiObjectData }

  TDelphiObjectData = class(TIDEObjectData)
  private
    function Criar(AEscolha: TEscolhaInstalacao): TInstalacaoDelphi;
  protected
    function Install(AEscolha: TEscolhaInstalacao): boolean; override;
  public
    function Plano(AEscolha: TEscolhaInstalacao): string; override;
    function DependenciaInstalada(AReceita: TReceita; AEscolha: TEscolhaInstalacao): string; override;
  end;

implementation

{ TDelphiObjectData }

function TDelphiObjectData.Criar(AEscolha: TEscolhaInstalacao): TInstalacaoDelphi;
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

function TDelphiObjectData.Install(AEscolha: TEscolhaInstalacao): boolean;
var
  vInst: TInstalacaoDelphi;
begin
  vInst := Criar(AEscolha);
  try
    // F9: numa rodada com as duas IDEs, pode nao haver nada deste lado
    if vInst.Pacotes.Count = 0 then
    begin
      LogarLinha('Nenhum dos recursos escolhidos existe no Delphi.');
      Resumo := Name + ': nada a instalar (os recursos escolhidos não existem no Delphi)';
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

function TDelphiObjectData.DependenciaInstalada(AReceita: TReceita;
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

function TDelphiObjectData.Plano(AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoDelphi;
begin
  vInst := Criar(AEscolha);
  try
    vInst.Log := nil;
    if vInst.Pacotes.Count = 0 then
      Exit(Name + ': nenhum dos recursos escolhidos existe no Delphi' + LineEnding);
    Result := vInst.Plano;
  finally
    vInst.Free;
  end;
end;

end.
