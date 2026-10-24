/// The engine rule of an installation: RAL does nothing without a transport
/// engine, so a run that installs RAL in an IDE must install at least one of the
/// engine packages (the Engine folder of pkg/<IDE>) that fit that IDE. The
/// installer refuses such a run instead of warning: a RAL with no engine
/// installs cleanly and then answers nothing.
/// The same idea after compiling (F14): a run only reaches the IDE when what
/// compiled makes a working RAL — the core, the design package when the IDE
/// gets one, and at least one engine. Otherwise the IDE stays as it was.
unit RALInst.Motores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.Compatibilidade, RALInst.IDE;

/// F14: what FaltaConjunto requires, for the plan: 'PascalRAL, PascalRALDsgn,
/// um motor entre IndyRAL, SynopseRAL; o resto e opcional'
function DescreverConjunto(ALista: TList; ADesign: boolean): string;
/// True when the package is an engine: its folder in pkg/<IDE> is Engine.
function EhMotor(APacote: TPacote): boolean;
{ F14: '' when what compiled (AOk, by name) makes a working RAL out of the
  run (ALista, TPacote): the core, the design package when ADesign and the run
  has it, and at least one of the run's engines. Otherwise why not, with the
  reason of what failed (AMotivos: name=reason). Optional packages (database,
  compression...) never count: they stay out alone. }
function FaltaConjunto(ALista: TList; AOk, AMotivos: TStrings;
  ADesign: boolean): string;
{ '' when AEscolhidos holds an engine that fits AIDE, or no package of AIDE's
  kind at all (nothing is installed there: a run with nothing checked
  uninstalls); otherwise why RAL cannot be installed there. A RAL version the
  manifest blocks for the IDE (F15) says that reason. }
function FaltaMotor(ACatalogo: TCatalogo; ACompat: TCompatibilidade;
  AIDE: TIDEInstance; AEscolhidos: TStrings): string;
/// The engines of the catalog that fit AIDE, by name, in installation order.
procedure MotoresDaIDE(ACatalogo: TCatalogo; ACompat: TCompatibilidade;
  AIDE: TIDEInstance; ALista: TStrings);
/// The package kind an IDE installs.
function TipoDaIDE(AIDE: TIDEInstance): TTipoPacote;

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

function Obrigatorio(APacote: TPacote; ADesign: boolean): boolean;
begin
  // o nucleo sempre; o de design (PascalRALDsgn, pascalraldsgn) so com a IDE
  Result := SameText(APacote.Nome, 'PascalRAL') or
            (ADesign and SameText(APacote.Nome, 'PascalRALDsgn'));
end;

function FaltaConjunto(ALista: TList; AOk, AMotivos: TStrings;
  ADesign: boolean): string;
var
  vInt: integer;
  vPacote: TPacote;
  vMotores: TStringList;

  function MotivoDe(const ANome: string): string;
  begin
    Result := '';
    if AMotivos <> nil then
      Result := AMotivos.Values[ANome];
    if Result = '' then
      Result := cmConjuntoSemMotivo;
  end;

begin
  Result := '';
  vMotores := TStringList.Create;
  try
    for vInt := 0 to Pred(ALista.Count) do
    begin
      vPacote := TPacote(ALista[vInt]);
      if Obrigatorio(vPacote, ADesign) and (AOk.IndexOf(vPacote.Nome) < 0) then
        Exit(Format(cmConjuntoNaoCompilou, [vPacote.Nome, MotivoDe(vPacote.Nome)]));
      if EhMotor(vPacote) then
        vMotores.Add(vPacote.Nome);
    end;
    // a rodada sem motor nenhum e assunto de FaltaMotor (recusa antes)
    if vMotores.Count = 0 then
      Exit;
    for vInt := 0 to Pred(vMotores.Count) do
      if AOk.IndexOf(vMotores[vInt]) >= 0 then
        Exit;
    for vInt := 0 to Pred(vMotores.Count) do
    begin
      if Result <> '' then
        Result := Result + '; ';
      Result := Result + vMotores[vInt] + ': ' + MotivoDe(vMotores[vInt]);
    end;
    Result := Format(cmConjuntoNenhumMotor, [Result]);
  finally
    vMotores.Free;
  end;
end;

function DescreverConjunto(ALista: TList; ADesign: boolean): string;
var
  vInt: integer;
  vPacote: TPacote;
  vMotores: string;
begin
  Result := '';
  vMotores := '';
  for vInt := 0 to Pred(ALista.Count) do
  begin
    vPacote := TPacote(ALista[vInt]);
    if Obrigatorio(vPacote, ADesign) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + vPacote.Nome;
    end
    else if EhMotor(vPacote) then
    begin
      if vMotores <> '' then
        vMotores := vMotores + ', ';
      vMotores := vMotores + vPacote.Nome;
    end;
  end;
  if Pos(',', vMotores) > 0 then
    vMotores := Format(cmConjuntoMotor, [vMotores]);
  if vMotores <> '' then
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + vMotores;
  end;
  Result := Format(cmConjuntoFuncional, [Result]);
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
  vMotivo: string;
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
    // F15: a versao inteira nao serve nesta IDE: e esse o motivo, nao o motor
    vMotivo := '';
    if ACompat <> nil then
      vMotivo := ACompat.MotivoVersao(AIDE);
    if vMotivo <> '' then
      Result := Format(emVersaoNaoServe, [AIDE.Nome, vMotivo])
    else if vMotores.Count = 0 then
      Result := Format(emIDESemMotorPossivel, [AIDE.Nome])
    else
      Result := Format(emSemMotor, [AIDE.Nome,
        StringReplace(vMotores.CommaText, ',', ', ', [rfReplaceAll])]);
  finally
    vMotores.Free;
  end;
end;

end.
