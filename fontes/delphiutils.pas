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
  ideutils, RALInst.Instalar.Delphi;

type

  { TDelphiObjectData }

  TDelphiObjectData = class(TIDEObjectData)
  private
    function Criar(AEscolha: TEscolhaInstalacao): TInstalacaoDelphi;
  protected
    function Install(AEscolha: TEscolhaInstalacao): boolean; override;
  public
    function Plano(AEscolha: TEscolhaInstalacao): string; override;
  end;

implementation

{ TDelphiObjectData }

function TDelphiObjectData.Criar(AEscolha: TEscolhaInstalacao): TInstalacaoDelphi;
begin
  Result := TInstalacaoDelphi.Create(Instancia, AEscolha.Catalogo);
  Result.Pacotes.Assign(AEscolha.Pacotes);
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

function TDelphiObjectData.Plano(AEscolha: TEscolhaInstalacao): string;
var
  vInst: TInstalacaoDelphi;
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
