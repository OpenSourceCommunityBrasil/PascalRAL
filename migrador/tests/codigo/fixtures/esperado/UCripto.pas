unit UCripto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALHashes, RALTypes;

type
  TCifra = class
  private
    FTipo: TRALCriptoType;
  public
    function Cifrar(const ATexto: string; ATipo: TRALCriptoType): string;
    procedure Padrao;
  end;

implementation

{ TCriptoType and ctAES128 in this comment stay as they are }

function TCifra.Cifrar(const ATexto: string; ATipo: TRALCriptoType): string;
var
  vTipo: TRALCriptoType;
begin
  // ctAES256 in a line comment does not change
  vTipo := crAES192;
  Result := 'TCriptoType ctAES128 inside a string';
  {$IFDEF CTAES128}
  Result := Result + ' ';
  {$ENDIF}
  if ATipo = crAES128 then
    vTipo := crAES256;
end;

procedure TCifra.Padrao;
begin
  FTipo := crAES128;
end;

end.
