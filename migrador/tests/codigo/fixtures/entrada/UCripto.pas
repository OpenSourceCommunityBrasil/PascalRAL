unit UCripto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALHashes;

type
  TCifra = class
  private
    FTipo: TCriptoType;
  public
    function Cifrar(const ATexto: string; ATipo: TCriptoType): string;
    procedure Padrao;
  end;

implementation

{ TCriptoType and ctAES128 in this comment stay as they are }

function TCifra.Cifrar(const ATexto: string; ATipo: TCriptoType): string;
var
  vTipo: TCriptoType;
begin
  // ctAES256 in a line comment does not change
  vTipo := ctAES192;
  Result := 'TCriptoType ctAES128 inside a string';
  {$IFDEF CTAES128}
  Result := Result + ' ';
  {$ENDIF}
  if ATipo = ctAES128 then
    vTipo := ctAES256;
end;

procedure TCifra.Padrao;
begin
  FTipo := ctAES128;
end;

end.
