unit lazarusutils;

{$mode ObjFPC}{$H+}

// Casca de UI do Lazarus. A rodada (links, pacotes marcados e um --build-ide
// no fim, com a configuracao daquela instalacao) mora em
// RALInst.Instalar.Lazarus.

interface

uses
  Classes, SysUtils,
  ideutils, RALInst.Instalar.Lazarus;

type

  { TLazarusObjectData }

  TLazarusObjectData = class(TIDEObjectData)
  private
    function Criar(AEscolha: TEscolhaInstalacao): TInstalacaoLazarus;
  protected
    function Install(AEscolha: TEscolhaInstalacao): boolean; override;
  public
    function Plano(AEscolha: TEscolhaInstalacao): string; override;
  end;

implementation

{ TLazarusObjectData }

function TLazarusObjectData.Criar(AEscolha: TEscolhaInstalacao): TInstalacaoLazarus;
begin
  Result := TInstalacaoLazarus.Create(Instancia, AEscolha.Catalogo);
  Result.Pacotes.Assign(AEscolha.Pacotes);
  Result.Log := @LogarLinha;
end;

function TLazarusObjectData.Install(AEscolha: TEscolhaInstalacao): boolean;
var
  vInst: TInstalacaoLazarus;
begin
  vInst := Criar(AEscolha);
  try
    Result := vInst.Executar;
    if vInst.Relatorio.Count > 0 then
    begin
      LogarLinha('');
      LogarLinha(TrimRight(vInst.Relatorio.Text));
    end;
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
    Result := vInst.Plano;
  finally
    vInst.Free;
  end;
end;

end.
