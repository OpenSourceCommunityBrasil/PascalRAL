/// The engine rule of an installation: RAL does nothing without a transport
/// engine, so a run that installs RAL in an IDE must install at least one of the
/// engine packages (the Engine folder of pkg/<IDE>) that fit that IDE. The
/// installer refuses such a run instead of warning: a RAL with no engine
/// installs cleanly and then answers nothing.
unit RALInst.Motores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.Compatibilidade, RALInst.IDE;

/// True when the package is an engine: its folder in pkg/<IDE> is Engine.
function EhMotor(APacote: TPacote): boolean;
/// The package kind an IDE installs.
function TipoDaIDE(AIDE: TIDEInstance): TTipoPacote;
/// The engines of the catalog that fit AIDE, by name, in installation order.
procedure MotoresDaIDE(ACatalogo: TCatalogo; ACompat: TCompatibilidade;
  AIDE: TIDEInstance; ALista: TStrings);
{ '' when AEscolhidos holds an engine that fits AIDE, or no package of AIDE's
  kind at all (nothing is installed there: a run with nothing checked
  uninstalls); otherwise why RAL cannot be installed there. }
function FaltaMotor(ACatalogo: TCatalogo; ACompat: TCompatibilidade;
  AIDE: TIDEInstance; AEscolhidos: TStrings): string;

implementation

uses
  RALInst.Mensagens;

function EhMotor(APacote: TPacote): boolean;
var
  vGrupo: string;
begin
  vGrupo := LowerCase(APacote.Grupo);
  if Pos('/', vGrupo) > 0 then
    vGrupo := Copy(vGrupo, 1, Pos('/', vGrupo) - 1);
  Result := vGrupo = 'engine';
end;

function TipoDaIDE(AIDE: TIDEInstance): TTipoPacote;
begin
  if AIDE.Tipo = tiDelphi then
    Result := tpDelphi
  else
    Result := tpLazarus;
end;

function Cabe(APacote: TPacote; ACompat: TCompatibilidade; AIDE: TIDEInstance): boolean;
begin
  Result := (APacote.FontesAusentes.Count = 0) and (APacote.Ordem >= 0) and
            ((ACompat = nil) or (ACompat.Motivo(APacote, AIDE) = ''));
end;

procedure MotoresDaIDE(ACatalogo: TCatalogo; ACompat: TCompatibilidade;
  AIDE: TIDEInstance; ALista: TStrings);
var
  vLista: TList;
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  vLista := TList.Create;
  try
    ACatalogo.Listar(TipoDaIDE(AIDE), vLista);
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TPacote(vLista[vInt]);
      if EhMotor(vPacote) and Cabe(vPacote, ACompat, AIDE) then
        ALista.Add(vPacote.Nome);
    end;
  finally
    vLista.Free;
  end;
end;

function FaltaMotor(ACatalogo: TCatalogo; ACompat: TCompatibilidade;
  AIDE: TIDEInstance; AEscolhidos: TStrings): string;
var
  vMotores: TStringList;
  vInt: integer;
  vPacote: TPacote;
  vAlgum: boolean;
begin
  Result := '';
  vAlgum := False;
  vMotores := TStringList.Create;
  try
    MotoresDaIDE(ACatalogo, ACompat, AIDE, vMotores);
    for vInt := 0 to Pred(AEscolhidos.Count) do
    begin
      vPacote := ACatalogo.Buscar(TipoDaIDE(AIDE), AEscolhidos[vInt]);
      if vPacote = nil then
        Continue;
      vAlgum := True;
      if vMotores.IndexOf(vPacote.Nome) >= 0 then
        Exit;
    end;
    // nada desta IDE: ela nao recebe RAL nesta rodada
    if not vAlgum then
      Exit;
    if vMotores.Count = 0 then
      Result := Format(emIDESemMotorPossivel, [AIDE.Nome])
    else
      Result := Format(emSemMotor, [AIDE.Nome,
        StringReplace(vMotores.CommaText, ',', ', ', [rfReplaceAll])]);
  finally
    vMotores.Free;
  end;
end;

end.
