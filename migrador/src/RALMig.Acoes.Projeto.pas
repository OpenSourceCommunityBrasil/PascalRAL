/// Rule actions that act on projects and packages (stage M3): rename a package
/// (.dproj DCC_UsePackage, .dpk requires, .lpi/.lpk PackageName, the unit in the
/// .lpr uses), require a package when the project uses its units, change the
/// search paths, rename units in the uses clauses, add a unit to the uses when
/// an identifier needs it, and turn a design-time package requirement into the
/// runtime package. Registered at initialization (RegistrarAcao).
unit RALMig.Acoes.Projeto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.Regras, RALMig.Texto, RALMig.Tipos;

type
  { TAcaoPorArquivo }

  /// Base: runs ProcessarArquivo on every file of the kinds of the rule; a
  /// failure on one file goes to the result and the others go on
  TAcaoPorArquivo = class(TAcaoMigracao)
  protected
    FRegra: TRegraMigracao;
    FCtx: TContextoMigracao;
    procedure ProcessarArquivo(AArq: TArquivoMigrado); virtual; abstract;
    procedure Feito(AArq: TArquivoMigrado; ALinha: integer; const ATexto: string);
    procedure Manual(AArq: TArquivoMigrado; ALinha: integer;
      const AMarcas: array of string);
    function IdeDelphi: boolean;
    function IdeLazarus: boolean;
  public
    function Aplicar(ARegra: TRegraMigracao; AContexto: TContextoMigracao): boolean; override;
  end;

  { TAcaoPacoteRenomear }

  TAcaoPacoteRenomear = class(TAcaoPorArquivo)
  private
    function NomeDe(ADelphi: boolean): string;
    function NomePara(ADelphi: boolean): string;
    procedure RenomearDproj(AArq: TArquivoMigrado);
    procedure RenomearDpk(AArq: TArquivoMigrado);
    procedure RenomearLazarus(AArq: TArquivoMigrado);
    procedure RenomearNoUses(AArq: TArquivoMigrado; ADelphi: boolean);
  protected
    procedure ProcessarArquivo(AArq: TArquivoMigrado); override;
  end;

  { TAcaoPacoteExigirSeUnit }

  TAcaoPacoteExigirSeUnit = class(TAcaoPorArquivo)
  private
    function UnitUsada(AArq: TArquivoMigrado): string;
    procedure ExigirDproj(AArq: TArquivoMigrado; const APacote, AUnit: string);
    procedure ExigirDpk(AArq: TArquivoMigrado; const APacote, AUnit: string);
    procedure ExigirLazarus(AArq: TArquivoMigrado; const APacote, AUnit: string);
  protected
    procedure ProcessarArquivo(AArq: TArquivoMigrado); override;
  end;

  { TAcaoCaminhoTrocar }

  TAcaoCaminhoTrocar = class(TAcaoPorArquivo)
  private
    FUsadas: TStringList;
    function UsaUnit(AArq: TArquivoMigrado; const AUnits: string): boolean;
    function TransformarLista(AArq: TArquivoMigrado; const ALista: string;
      out AMudou: boolean): string;
    procedure TrocarDproj(AArq: TArquivoMigrado);
    procedure TrocarLazarus(AArq: TArquivoMigrado);
    procedure TrocarDpk(AArq: TArquivoMigrado);
  protected
    procedure ProcessarArquivo(AArq: TArquivoMigrado); override;
  public
    destructor Destroy; override;
  end;

  { TAcaoUnitRenomear }

  TAcaoUnitRenomear = class(TAcaoPorArquivo)
  protected
    procedure ProcessarArquivo(AArq: TArquivoMigrado); override;
  end;

  { TAcaoUsesAdicionar }

  TAcaoUsesAdicionar = class(TAcaoPorArquivo)
  protected
    procedure ProcessarArquivo(AArq: TArquivoMigrado); override;
  end;

  { TAcaoPacoteDesignParaRuntime }

  TAcaoPacoteDesignParaRuntime = class(TAcaoPorArquivo)
  private
    function SoParaDesign(AArq: TArquivoMigrado): boolean;
    function UnitTemRegister(AArq: TArquivoMigrado; const AUnit, AArquivo: string): boolean;
    procedure TrocarDproj(AArq: TArquivoMigrado);
    procedure TrocarDpk(AArq: TArquivoMigrado);
  protected
    procedure ProcessarArquivo(AArq: TArquivoMigrado); override;
  end;

/// Replaces the item ADe of a ';' list by APara (APara = '' removes it); an
/// APara that is already in the list is not repeated. AMudou tells if it changed
function TrocarItemLista(const ALista, ADe, APara: string; out AMudou: boolean): string;
/// Appends the item to a ';' list, before a trailing '$(...)' macro item
function AcrescentarItemLista(const ALista, AItem: string): string;
/// The ';' list has the item (no case)
function ListaTemItem(const ALista, AItem: string): boolean;
/// The list has a RAL package (PascalRAL*, RAL*, *RAL)
function TemPacoteRAL(const ALista: string): boolean;

implementation

uses
  RALMig.Tokens, RALMig.&Uses, RALMig.ProjetoXML, RALMig.Projeto;

resourcestring
  emAcaoFalha = 'Regra %s, arquivo %s: %s';
  cmAcaoPacoteTrocado = 'Pacote %s trocado por %s';
  cmAcaoPacoteAcrescentado = 'Pacote %s acrescentado (o projeto usa %s)';
  cmAcaoCaminhoTrocado = 'Caminhos de busca: %s -> %s';
  cmAcaoCaminhoDpk = 'Caminho de %s: %s -> %s';
  cmAcaoUnitTrocada = 'Unit %s trocada por %s no uses (%d vez(es))';
  cmAcaoUnitAcrescentada = 'Unit %s acrescentada ao uses (o código usa %s)';
  cmAcaoDesignParaRuntime = 'Pacote %s trocado por %s (o pacote não registra componentes)';

type
  { TItensLista }

  TItensLista = class
  public
    class procedure Dividir(const ALista: string; ADest: TStringList);
    class function Juntar(ALista: TStrings): string;
  end;

class procedure TItensLista.Dividir(const ALista: string; ADest: TStringList);
var
  vIni, vPos: integer;
begin
  vIni := 1;
  repeat
    vPos := vIni;
    while (vPos <= Length(ALista)) and (ALista[vPos] <> ';') do
      Inc(vPos);
    ADest.Add(Copy(ALista, vIni, vPos - vIni));
    vIni := vPos + 1;
  until vIni > Length(ALista) + 1;
end;

class function TItensLista.Juntar(ALista: TStrings): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to ALista.Count - 1 do
  begin
    if vInt > 0 then
      Result := Result + ';';
    Result := Result + ALista[vInt];
  end;
end;

function IndiceDoItem(ALista: TStrings; const AItem: string): integer;
begin
  for Result := 0 to ALista.Count - 1 do
    if SameText(Trim(ALista[Result]), AItem) then
      Exit;
  Result := -1;
end;

function TrocarItemLista(const ALista, ADe, APara: string; out AMudou: boolean): string;
var
  vItens, vNovos: TStringList;
  vInt: integer;
begin
  AMudou := False;
  vItens := TStringList.Create;
  vNovos := TStringList.Create;
  try
    TItensLista.Dividir(ALista, vItens);
    for vInt := 0 to vItens.Count - 1 do
    begin
      if not SameText(Trim(vItens[vInt]), ADe) or SameText(ADe, APara) then
        vNovos.Add(vItens[vInt])
      else
      begin
        AMudou := True;
        if (APara <> '') and (IndiceDoItem(vItens, APara) < 0) and
           (IndiceDoItem(vNovos, APara) < 0) then
          vNovos.Add(APara);
      end;
    end;
    if AMudou then
      Result := TItensLista.Juntar(vNovos)
    else
      Result := ALista;
  finally
    vNovos.Free;
    vItens.Free;
  end;
end;

function ListaTemItem(const ALista, AItem: string): boolean;
var
  vItens: TStringList;
begin
  vItens := TStringList.Create;
  try
    TItensLista.Dividir(ALista, vItens);
    Result := IndiceDoItem(vItens, AItem) >= 0;
  finally
    vItens.Free;
  end;
end;

function AcrescentarItemLista(const ALista, AItem: string): string;
var
  vItens: TStringList;
  vFim: integer;
begin
  vItens := TStringList.Create;
  try
    TItensLista.Dividir(ALista, vItens);
    vFim := vItens.Count;
    if (vFim > 0) and (Copy(Trim(vItens[vFim - 1]), 1, 2) = '$(') then
      Dec(vFim);
    vItens.Insert(vFim, AItem);
    Result := TItensLista.Juntar(vItens);
  finally
    vItens.Free;
  end;
end;

function TemPacoteRAL(const ALista: string): boolean;
var
  vItens: TStringList;
  vInt: integer;
  vNome: string;
begin
  Result := False;
  vItens := TStringList.Create;
  try
    TItensLista.Dividir(ALista, vItens);
    for vInt := 0 to vItens.Count - 1 do
    begin
      vNome := LowerCase(Trim(vItens[vInt]));
      if (Copy(vNome, 1, 2) = '$(') or (Length(vNome) < 3) then
        Continue;
      if (Copy(vNome, 1, 3) = 'ral') or (Copy(vNome, 1, 9) = 'pascalral') or
         (Copy(vNome, Length(vNome) - 2, 3) = 'ral') then
        Exit(True);
    end;
  finally
    vItens.Free;
  end;
end;

{ TAcaoPorArquivo }

function TAcaoPorArquivo.Aplicar(ARegra: TRegraMigracao;
  AContexto: TContextoMigracao): boolean;
var
  vLista: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  Result := True;
  FRegra := ARegra;
  FCtx := AContexto;
  vLista := TList.Create;
  try
    AContexto.Conjunto.Listar(TiposDaRegra(ARegra), vLista);
    for vInt := 0 to vLista.Count - 1 do
    begin
      vArq := TArquivoMigrado(vLista[vInt]);
      try
        ProcessarArquivo(vArq);
      except
        on E: Exception do
          AContexto.Resultado.Erros.Add(Format(emAcaoFalha, [ARegra.Id, vArq.Relativo, E.Message]));
      end;
    end;
  finally
    vLista.Free;
  end;
end;

procedure TAcaoPorArquivo.Feito(AArq: TArquivoMigrado; ALinha: integer;
  const ATexto: string);
begin
  FCtx.Resultado.AdicionarFeito(FRegra.Id, AArq.Relativo, ALinha, ATexto);
end;

procedure TAcaoPorArquivo.Manual(AArq: TArquivoMigrado; ALinha: integer;
  const AMarcas: array of string);
var
  vMarcas: array of string;
  vInt: integer;
begin
  vMarcas := nil;
  SetLength(vMarcas, Length(AMarcas) + 2);
  vMarcas[0] := 'arquivo';
  vMarcas[1] := AArq.Relativo;
  for vInt := 0 to High(AMarcas) do
    vMarcas[vInt + 2] := AMarcas[vInt];
  FCtx.Resultado.AdicionarManual(FRegra.Id, AArq.Relativo, ALinha,
    TextoRegra(FRegra, 'descricao', FCtx, vMarcas),
    TextoRegra(FRegra, 'manual', FCtx, vMarcas), '', FRegra.Wiki);
end;

function TAcaoPorArquivo.IdeDelphi: boolean;
begin
  Result := (FRegra.Param('ide', 'ambos') = 'delphi') or (FRegra.Param('ide', 'ambos') = 'ambos');
end;

function TAcaoPorArquivo.IdeLazarus: boolean;
begin
  Result := (FRegra.Param('ide', 'ambos') = 'lazarus') or (FRegra.Param('ide', 'ambos') = 'ambos');
end;

{ XML helpers }

/// The texts of the elements, changed by AFunc-like callers; here: the first
/// line of the first element, to report
function LinhaDoElemento(AArq: TArquivoMigrado; AEl: TElementoXML): integer;
begin
  Result := AArq.Texto.LinhaDoOffset(AEl.InicioTag);
end;

{ TAcaoPacoteRenomear }

function TAcaoPacoteRenomear.NomeDe(ADelphi: boolean): string;
begin
  Result := FRegra.Param('de');
  if (not ADelphi) and (FRegra.Param('ide') = 'ambos') then
    Result := FRegra.Param('de-lazarus', Result);
end;

function TAcaoPacoteRenomear.NomePara(ADelphi: boolean): string;
begin
  Result := FRegra.Param('para');
  if (not ADelphi) and (FRegra.Param('ide') = 'ambos') then
    Result := FRegra.Param('para-lazarus', Result);
end;

procedure TAcaoPacoteRenomear.RenomearDproj(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vInt, vLinha: integer;
  vNovo: string;
  vMudou: boolean;
begin
  vLinha := 0;
  vXml := TProjetoXML.Create(AArq.Texto);
  try
    vLista := vXml.Buscar('//DCC_UsePackage');
    try
      for vInt := 0 to vLista.Count - 1 do
      begin
        vNovo := TrocarItemLista(vXml.ValorTexto(TElementoXML(vLista[vInt])),
                                 NomeDe(True), NomePara(True), vMudou);
        if not vMudou then
          Continue;
        vXml.TrocarTexto(TElementoXML(vLista[vInt]), vNovo);
        if vLinha = 0 then
          vLinha := LinhaDoElemento(AArq, TElementoXML(vLista[vInt]));
      end;
    finally
      vLista.Free;
    end;
  finally
    vXml.Free;
  end;
  if vLinha > 0 then
    Feito(AArq, vLinha, Format(cmAcaoPacoteTrocado, [NomeDe(True), NomePara(True)]));
end;

procedure TAcaoPacoteRenomear.RenomearDpk(AArq: TArquivoMigrado);
var
  vEd: TEditorUses;
  vItem: TItemUses;
begin
  vEd := TEditorUses.Create(AArq.Texto);
  try
    if (vEd.Clausula(suRequires) = nil) or
       not vEd.ContemEm(NomeDe(True), suRequires) then
      Exit;
    vItem := vEd.Achar(NomeDe(True));
    if vEd.Trocar(NomeDe(True), NomePara(True)) > 0 then
      Feito(AArq, vItem.Linha, Format(cmAcaoPacoteTrocado, [NomeDe(True), NomePara(True)]));
  finally
    vEd.Free;
  end;
end;

procedure TAcaoPacoteRenomear.RenomearLazarus(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vNomes: TStringList;
  vInt, vLinha: integer;
  vEl: TElementoXML;
  vDe, vPara: string;
begin
  vDe := NomeDe(False);
  vPara := NomePara(False);
  vLinha := 0;
  vXml := TProjetoXML.Create(AArq.Texto);
  vNomes := TStringList.Create;
  try
    vNomes.CaseSensitive := False;
    if vXml.ContainerDePacotes <> nil then
      vXml.ListarPacotes(vXml.ContainerDePacotes, vNomes);
    vLista := vXml.Buscar('//PackageName');
    try
      for vInt := 0 to vLista.Count - 1 do
      begin
        vEl := TElementoXML(vLista[vInt]);
        if (not SameText(vXml.ValorAtributo(vEl, 'Value'), vDe)) or (vDe = vPara) then
          Continue;
        if (vNomes.IndexOf(vPara) >= 0) then
          Manual(AArq, LinhaDoElemento(AArq, vEl), ['pacote', vDe])
        else
        begin
          vXml.TrocarAtributo(vEl, 'Value', vPara);
          if vLinha = 0 then
            vLinha := LinhaDoElemento(AArq, vEl);
        end;
      end;
    finally
      vLista.Free;
    end;
  finally
    vNomes.Free;
    vXml.Free;
  end;
  if vLinha > 0 then
    Feito(AArq, vLinha, Format(cmAcaoPacoteTrocado, [vDe, vPara]));
end;

procedure TAcaoPacoteRenomear.RenomearNoUses(AArq: TArquivoMigrado; ADelphi: boolean);
var
  vEd: TEditorUses;
  vItem: TItemUses;
begin
  vEd := TEditorUses.Create(AArq.Texto);
  try
    vItem := vEd.Achar(NomeDe(ADelphi));
    if vItem = nil then
      Exit;
    if vEd.Trocar(NomeDe(ADelphi), NomePara(ADelphi)) > 0 then
      Feito(AArq, vItem.Linha, Format(cmAcaoUnitTrocada, [NomeDe(ADelphi), NomePara(ADelphi), 1]));
  finally
    vEd.Free;
  end;
end;

procedure TAcaoPacoteRenomear.ProcessarArquivo(AArq: TArquivoMigrado);
begin
  case AArq.Tipo of
    taDproj: if IdeDelphi then RenomearDproj(AArq);
    taDpk: if IdeDelphi then RenomearDpk(AArq);
    taLpi, taLpk: if IdeLazarus then RenomearLazarus(AArq);
    taLpr, taPas:
      if IdeLazarus and FRegra.ParamBool('no-uses') then
        RenomearNoUses(AArq, False);
    taDpr:
      if IdeDelphi and FRegra.ParamBool('no-uses') then
        RenomearNoUses(AArq, True);
  end;
end;

{ TAcaoPacoteExigirSeUnit }

function TAcaoPacoteExigirSeUnit.UnitUsada(AArq: TArquivoMigrado): string;
var
  vEscopo: TList;
  vUsadas: TStringList;
  vInt: integer;
begin
  Result := '';
  vEscopo := TList.Create;
  vUsadas := TStringList.Create;
  try
    ArquivosDoProjeto(FCtx.Conjunto, AArq, vEscopo);
    UnitsUsadas(vEscopo, vUsadas);
    for vInt := 0 to FRegra.Lista('units').Count - 1 do
      if vUsadas.IndexOf(LowerCase(FRegra.Lista('units')[vInt])) >= 0 then
        Exit(FRegra.Lista('units')[vInt]);
  finally
    vUsadas.Free;
    vEscopo.Free;
  end;
end;

procedure TAcaoPacoteExigirSeUnit.ExigirDproj(AArq: TArquivoMigrado;
  const APacote, AUnit: string);
var
  vXml: TProjetoXML;
  vLista: TList;
  vInt, vLinha: integer;
  vTexto: string;
begin
  vLinha := 0;
  vXml := TProjetoXML.Create(AArq.Texto);
  try
    vLista := vXml.Buscar('//DCC_UsePackage');
    try
      for vInt := 0 to vLista.Count - 1 do
      begin
        vTexto := vXml.ValorTexto(TElementoXML(vLista[vInt]));
        if (not TemPacoteRAL(vTexto)) or ListaTemItem(vTexto, APacote) then
          Continue;
        vXml.TrocarTexto(TElementoXML(vLista[vInt]), AcrescentarItemLista(vTexto, APacote));
        if vLinha = 0 then
          vLinha := LinhaDoElemento(AArq, TElementoXML(vLista[vInt]));
      end;
    finally
      vLista.Free;
    end;
  finally
    vXml.Free;
  end;
  if vLinha > 0 then
    Feito(AArq, vLinha, Format(cmAcaoPacoteAcrescentado, [APacote, AUnit]));
end;

procedure TAcaoPacoteExigirSeUnit.ExigirDpk(AArq: TArquivoMigrado;
  const APacote, AUnit: string);
var
  vEd: TEditorUses;
  vCl: TClausulaUses;
  vInt: integer;
  vTemRAL: boolean;
begin
  vEd := TEditorUses.Create(AArq.Texto);
  try
    vCl := vEd.Clausula(suRequires);
    if (vCl = nil) or vEd.ContemEm(APacote, suRequires) then
      Exit;
    vTemRAL := False;
    for vInt := 0 to vCl.Count - 1 do
      if TemPacoteRAL(vCl.Item(vInt).Nome) then
        vTemRAL := True;
    if not vTemRAL then
      Exit;
    vEd.Acrescentar(APacote, suRequires);
    Feito(AArq, vCl.Item(0).Linha, Format(cmAcaoPacoteAcrescentado, [APacote, AUnit]));
  finally
    vEd.Free;
  end;
end;

procedure TAcaoPacoteExigirSeUnit.ExigirLazarus(AArq: TArquivoMigrado;
  const APacote, AUnit: string);
var
  vXml: TProjetoXML;
  vCont: TElementoXML;
begin
  vXml := TProjetoXML.Create(AArq.Texto);
  try
    vCont := vXml.ContainerDePacotes;
    if vCont = nil then
      Manual(AArq, 1, ['pacote', APacote, 'unit', AUnit])
    else if vXml.AcrescentarPacote(vCont, APacote) then
      Feito(AArq, LinhaDoElemento(AArq, vCont), Format(cmAcaoPacoteAcrescentado, [APacote, AUnit]));
  finally
    vXml.Free;
  end;
end;

procedure TAcaoPacoteExigirSeUnit.ProcessarArquivo(AArq: TArquivoMigrado);
var
  vDelphi: boolean;
  vPacote, vUnit: string;
begin
  vDelphi := AArq.Tipo in [taDproj, taDpk];
  if (vDelphi and not IdeDelphi) or ((not vDelphi) and not IdeLazarus) then
    Exit;
  vPacote := FRegra.Param('pacote');
  if (not vDelphi) and (FRegra.Param('ide') = 'ambos') then
    vPacote := FRegra.Param('pacote-lazarus', LowerCase(vPacote));
  vUnit := UnitUsada(AArq);
  if vUnit = '' then
    Exit;
  case AArq.Tipo of
    taDproj: ExigirDproj(AArq, vPacote, vUnit);
    taDpk: ExigirDpk(AArq, vPacote, vUnit);
    taLpi, taLpk: ExigirLazarus(AArq, vPacote, vUnit);
  end;
end;

{ TAcaoCaminhoTrocar }

destructor TAcaoCaminhoTrocar.Destroy;
begin
  FUsadas.Free;
  inherited Destroy;
end;

function TAcaoCaminhoTrocar.UsaUnit(AArq: TArquivoMigrado; const AUnits: string): boolean;
var
  vEscopo: TList;
  vUnits: TStringList;
  vInt: integer;
begin
  Result := False;
  if FUsadas = nil then
  begin
    FUsadas := TStringList.Create;
    vEscopo := TList.Create;
    try
      ArquivosDoProjeto(FCtx.Conjunto, AArq, vEscopo);
      UnitsUsadas(vEscopo, FUsadas);
    finally
      vEscopo.Free;
    end;
  end;
  vUnits := TStringList.Create;
  try
    vUnits.CommaText := AUnits;
    for vInt := 0 to vUnits.Count - 1 do
      if FUsadas.IndexOf(LowerCase(vUnits[vInt])) >= 0 then
        Exit(True);
  finally
    vUnits.Free;
  end;
end;

/// Splits a path on \ and /
procedure SegmentosDoCaminho(const ACaminho: string; ADest: TStringList);
var
  vInt, vIni: integer;
begin
  vIni := 1;
  for vInt := 1 to Length(ACaminho) + 1 do
    if (vInt > Length(ACaminho)) or (ACaminho[vInt] in ['\', '/']) then
    begin
      ADest.Add(Copy(ACaminho, vIni, vInt - vIni));
      vIni := vInt + 1;
    end;
end;

/// Does the path end in src\<de>? ABase = the path up to and including 'src'
function CaminhoTermina(const ACaminho, ADe: string; out ABase: string;
  out ADelim: char; out ABarraFinal: boolean): boolean;
var
  vSegs, vDe: TStringList;
  vInt, vK, vN: integer;
begin
  Result := False;
  ADelim := '\';
  if (Pos('/', ACaminho) > 0) and (Pos('\', ACaminho) = 0) then
    ADelim := '/';
  vSegs := TStringList.Create;
  vDe := TStringList.Create;
  try
    SegmentosDoCaminho(ACaminho, vSegs);
    ABarraFinal := (vSegs.Count > 0) and (vSegs[vSegs.Count - 1] = '');
    if ABarraFinal then
      vSegs.Delete(vSegs.Count - 1);
    SegmentosDoCaminho(ADe, vDe);
    vK := vDe.Count + 1;
    vN := vSegs.Count;
    if vN < vK then
      Exit;
    if not SameText(vSegs[vN - vK], 'src') then
      Exit;
    for vInt := 0 to vDe.Count - 1 do
      if not SameText(vSegs[vN - vDe.Count + vInt], vDe[vInt]) then
        Exit;
    ABase := '';
    for vInt := 0 to vN - vK - 1 do
      ABase := ABase + vSegs[vInt] + ADelim;
    ABase := ABase + 'src';
    Result := True;
  finally
    vDe.Free;
    vSegs.Free;
  end;
end;

function NormalizarCaminho(const ACaminho: string): string;
begin
  Result := LowerCase(StringReplace(Trim(ACaminho), '/', '\', [rfReplaceAll]));
  while (Result <> '') and (Result[Length(Result)] = '\') do
    Delete(Result, Length(Result), 1);
end;

function TAcaoCaminhoTrocar.TransformarLista(AArq: TArquivoMigrado;
  const ALista: string; out AMudou: boolean): string;
var
  vItens, vSaida, vNovas: TStringList;
  vInt, vMap: integer;
  vBase, vItem, vPasta: string;
  vDelim: char;
  vBarra, vTrocar: boolean;
  vMapa: TStringList;

  function Montar(const APasta: string): string;
  begin
    Result := vBase + vDelim + StringReplace(StringReplace(APasta, '\', vDelim, [rfReplaceAll]),
      '/', vDelim, [rfReplaceAll]);
    if vBarra then
      Result := Result + vDelim;
  end;

  function JaTem(const ACaminho: string): boolean;
  var
    vK: integer;
  begin
    Result := False;
    for vK := 0 to vItens.Count - 1 do
      if NormalizarCaminho(vItens[vK]) = NormalizarCaminho(ACaminho) then
        Exit(True);
    for vK := 0 to vSaida.Count - 1 do
      if NormalizarCaminho(vSaida[vK]) = NormalizarCaminho(ACaminho) then
        Exit(True);
  end;

begin
  AMudou := False;
  Result := ALista;
  vTrocar := FRegra.Param('modo') = 'trocar';
  vMapa := FRegra.Mapa('para-se-unit');
  vItens := TStringList.Create;
  vSaida := TStringList.Create;
  vNovas := TStringList.Create;
  try
    TItensLista.Dividir(ALista, vItens);
    for vInt := 0 to vItens.Count - 1 do
    begin
      vItem := Trim(vItens[vInt]);
      if (vItem = '') or not CaminhoTermina(vItem, FRegra.Param('de'), vBase, vDelim, vBarra) then
      begin
        vSaida.Add(vItens[vInt]);
        Continue;
      end;
      vNovas.Clear;
      for vMap := 0 to FRegra.Lista('para').Count - 1 do
        vNovas.Add(Montar(FRegra.Lista('para')[vMap]));
      for vMap := 0 to vMapa.Count - 1 do
      begin
        vPasta := vMapa.Names[vMap];
        if UsaUnit(AArq, vMapa.ValueFromIndex[vMap]) then
          vNovas.Add(Montar(vPasta));
      end;
      if not vTrocar then
        vSaida.Add(vItens[vInt]);
      for vMap := 0 to vNovas.Count - 1 do
        if NormalizarCaminho(vNovas[vMap]) = NormalizarCaminho(vItem) then
        begin
          if vTrocar then
            vSaida.Add(vItens[vInt]);
        end
        else if not JaTem(vNovas[vMap]) then
          vSaida.Add(vNovas[vMap]);
    end;
    Result := TItensLista.Juntar(vSaida);
    AMudou := Result <> ALista;
  finally
    vNovas.Free;
    vSaida.Free;
    vItens.Free;
  end;
end;

procedure TAcaoCaminhoTrocar.TrocarDproj(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vInt, vLinha: integer;
  vNovo, vAntes, vPrimeiro: string;
  vMudou: boolean;
begin
  vLinha := 0;
  vXml := TProjetoXML.Create(AArq.Texto);
  try
    vLista := vXml.Buscar('//DCC_UnitSearchPath');
    try
      for vInt := 0 to vLista.Count - 1 do
      begin
        vAntes := vXml.ValorTexto(TElementoXML(vLista[vInt]));
        vNovo := TransformarLista(AArq, vAntes, vMudou);
        if not vMudou then
          Continue;
        vXml.TrocarTexto(TElementoXML(vLista[vInt]), vNovo);
        if vLinha = 0 then
        begin
          vLinha := LinhaDoElemento(AArq, TElementoXML(vLista[vInt]));
          vPrimeiro := FRegra.Param('de');
        end;
      end;
    finally
      vLista.Free;
    end;
  finally
    vXml.Free;
  end;
  if vLinha > 0 then
    Feito(AArq, vLinha, Format(cmAcaoCaminhoTrocado, [vPrimeiro, FRegra.Lista('para').CommaText]));
end;

procedure TAcaoCaminhoTrocar.TrocarLazarus(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vInt, vLinha: integer;
  vNovo: string;
  vMudou: boolean;
begin
  vLinha := 0;
  vXml := TProjetoXML.Create(AArq.Texto);
  try
    vLista := vXml.Buscar('//SearchPaths/OtherUnitFiles');
    try
      for vInt := 0 to vLista.Count - 1 do
      begin
        vNovo := TransformarLista(AArq, vXml.ValorAtributo(TElementoXML(vLista[vInt]), 'Value'), vMudou);
        if not vMudou then
          Continue;
        vXml.TrocarAtributo(TElementoXML(vLista[vInt]), 'Value', vNovo);
        if vLinha = 0 then
          vLinha := LinhaDoElemento(AArq, TElementoXML(vLista[vInt]));
      end;
    finally
      vLista.Free;
    end;
  finally
    vXml.Free;
  end;
  if vLinha > 0 then
    Feito(AArq, vLinha, Format(cmAcaoCaminhoTrocado, [FRegra.Param('de'), FRegra.Lista('para').CommaText]));
end;

procedure TAcaoCaminhoTrocar.TrocarDpk(AArq: TArquivoMigrado);
var
  vEd: TEditorUses;
  vCl: TClausulaUses;
  vInt: integer;
  vItem: TItemUses;
  vDir, vBase, vNovaPasta, vArquivoNovo, vTextoItem: string;
  vDelim: char;
  vBarra: boolean;
  vPos: integer;
begin
  vEd := TEditorUses.Create(AArq.Texto);
  try
    vCl := vEd.Clausula(suContains);
    if vCl = nil then
      Exit;
    for vInt := 0 to vCl.Count - 1 do
    begin
      vItem := vCl.Item(vInt);
      vDir := ExtractFilePath(StringReplace(vItem.Arquivo, '/', '\', [rfReplaceAll]));
      if (vItem.Arquivo = '') or
         not CaminhoTermina(vDir, FRegra.Param('de'), vBase, vDelim, vBarra) then
        Continue;
      vNovaPasta := '';
      if FCtx.ApiPara <> nil then
        vNovaPasta := FCtx.ApiPara.PastaDaUnidade(vItem.Nome);
      if Copy(vNovaPasta, 1, 4) <> 'src/' then
      begin
        Manual(AArq, vItem.Linha, ['unit', vItem.Nome, 'pastas', FRegra.Lista('para').CommaText]);
        Continue;
      end;
      vArquivoNovo := vBase + vDelim + StringReplace(Copy(vNovaPasta, 5, MaxInt), '/', vDelim,
        [rfReplaceAll]) + vDelim + ExtractFileName(StringReplace(vItem.Arquivo, '/', '\', [rfReplaceAll]));
      vTextoItem := Copy(AArq.Texto.Bytes, vItem.Inicio, vItem.Tamanho);
      vPos := Pos(vItem.Arquivo, vTextoItem);
      if vPos > 0 then
      begin
        AArq.Texto.AdicionarEdicao(vItem.Inicio + vPos - 1, Length(vItem.Arquivo), vArquivoNovo);
        Feito(AArq, vItem.Linha, Format(cmAcaoCaminhoDpk, [vItem.Nome, vItem.Arquivo, vArquivoNovo]));
      end;
    end;
  finally
    vEd.Free;
  end;
end;

procedure TAcaoCaminhoTrocar.ProcessarArquivo(AArq: TArquivoMigrado);
begin
  FreeAndNil(FUsadas);
  case AArq.Tipo of
    taDproj: TrocarDproj(AArq);
    taLpi, taLpk: TrocarLazarus(AArq);
    taDpk: TrocarDpk(AArq);
  end;
end;

{ TAcaoUnitRenomear }

procedure TAcaoUnitRenomear.ProcessarArquivo(AArq: TArquivoMigrado);
var
  vEd: TEditorUses;
  vMapa: TStringList;
  vInt, vVezes, vLinha: integer;
  vItem: TItemUses;
begin
  if not (AArq.Tipo in [taPas, taDpr, taLpr, taDpk]) then
    Exit;
  vMapa := FRegra.Mapa('mapa');
  vEd := TEditorUses.Create(AArq.Texto);
  try
    for vInt := 0 to vMapa.Count - 1 do
    begin
      vItem := vEd.Achar(vMapa.Names[vInt]);
      if (vItem = nil) or (vItem.Nome = vMapa.ValueFromIndex[vInt]) then
        Continue;
      vLinha := vItem.Linha;
      vVezes := vEd.Trocar(vMapa.Names[vInt], vMapa.ValueFromIndex[vInt]);
      if vVezes > 0 then
        Feito(AArq, vLinha, Format(cmAcaoUnitTrocada,
          [vMapa.Names[vInt], vMapa.ValueFromIndex[vInt], vVezes]));
    end;
  finally
    vEd.Free;
  end;
end;

{ TAcaoUsesAdicionar }

function PrimeiraPalavra(ATokens: TListaTokens; const APalavra: string): integer;
begin
  for Result := 0 to ATokens.Count - 1 do
    if (ATokens[Result].Tipo = ttPalavraChave) and ATokens.EhNomeIgual(Result, APalavra) then
      Exit;
  Result := ATokens.Count;
end;

procedure TAcaoUsesAdicionar.ProcessarArquivo(AArq: TArquivoMigrado);
var
  vEd: TEditorUses;
  vUnit, vAchado: string;
  vInt, vImpl, vAnt, vPrimeiro: integer;
  vIds: TStringList;
begin
  vUnit := FRegra.Param('unit');
  if (AArq.Tipo <> taPas) or
     SameText(ChangeFileExt(ExtractFileName(AArq.Caminho), ''), vUnit) then
    Exit;
  vEd := TEditorUses.Create(AArq.Texto);
  try
    if (vEd.Tipo <> tuUnit) or vEd.Contem(vUnit) then
      Exit;
    vIds := FRegra.Lista('quando-identificador');
    vPrimeiro := -1;
    for vInt := 0 to vEd.Tokens.Count - 1 do
    begin
      if vEd.Tokens[vInt].Tipo <> ttIdentificador then
        Continue;
      vAnt := vEd.Tokens.Anterior(vInt);
      if (vAnt >= 0) and vEd.Tokens.EhSimbolo(vAnt, '.') then
        Continue;
      if vIds.IndexOf(vEd.Tokens.Texto(vInt)) >= 0 then
      begin
        vPrimeiro := vInt;
        Break;
      end;
    end;
    if vPrimeiro < 0 then
      Exit;
    vAchado := vEd.Tokens.Texto(vPrimeiro);
    vImpl := PrimeiraPalavra(vEd.Tokens, 'implementation');
    if vPrimeiro < vImpl then
      vEd.Acrescentar(vUnit, suInterface)
    else
      vEd.Acrescentar(vUnit, suImplementation);
    Feito(AArq, AArq.Texto.LinhaDoOffset(vEd.Tokens[vPrimeiro].Inicio),
      Format(cmAcaoUnitAcrescentada, [vUnit, vAchado]));
  finally
    vEd.Free;
  end;
end;

{ TAcaoPacoteDesignParaRuntime }

function TAcaoPacoteDesignParaRuntime.UnitTemRegister(AArq: TArquivoMigrado;
  const AUnit, AArquivo: string): boolean;
var
  vFonte: TArquivoMigrado;
  vLista: TList;
  vInt, vAnt: integer;
  vToks: TListaTokens;
begin
  Result := True; // not found: cannot tell, so do not convert
  vFonte := nil;
  if AArquivo <> '' then
    vFonte := FCtx.Conjunto.Buscar(ExtractFilePath(AArq.Caminho) +
      StringReplace(AArquivo, '\', PathDelim, [rfReplaceAll]));
  if vFonte = nil then
  begin
    vLista := TList.Create;
    try
      FCtx.Conjunto.Listar([taPas], vLista);
      for vInt := 0 to vLista.Count - 1 do
        if SameText(ChangeFileExt(ExtractFileName(TArquivoMigrado(vLista[vInt]).Caminho), ''), AUnit) then
          vFonte := TArquivoMigrado(vLista[vInt]);
    finally
      vLista.Free;
    end;
  end;
  if vFonte = nil then
    Exit;
  Result := False;
  vToks := TokenizarPascal(vFonte.Texto.Bytes, vFonte.Texto.InicioConteudo);
  try
    for vInt := 0 to vToks.Count - 1 do
      if (vToks[vInt].Tipo = ttIdentificador) and vToks.EhNomeIgual(vInt, 'Register') then
      begin
        vAnt := vToks.Anterior(vInt);
        if (vAnt >= 0) and vToks.EhNomeIgual(vAnt, 'procedure') then
          Exit(True);
      end;
  finally
    vToks.Free;
  end;
end;

function TAcaoPacoteDesignParaRuntime.SoParaDesign(AArq: TArquivoMigrado): boolean;
var
  vToks: TListaTokens;
  vInt: integer;
  vEd: TEditorUses;
  vCl: TClausulaUses;
begin
  Result := False;
  vToks := TokenizarPascal(AArq.Texto.Bytes, AArq.Texto.InicioConteudo);
  try
    for vInt := 0 to vToks.Count - 1 do
      if (vToks[vInt].Tipo = ttDiretiva) and (vToks.NomeDiretiva(vInt) = 'DESIGNONLY') and
         (UpperCase(vToks.ArgumentoDiretiva(vInt)) <> 'OFF') then
        Exit(True);
  finally
    vToks.Free;
  end;
  vEd := TEditorUses.Create(AArq.Texto);
  try
    vCl := vEd.Clausula(suContains);
    if vCl <> nil then
      for vInt := 0 to vCl.Count - 1 do
        if UnitTemRegister(AArq, vCl.Item(vInt).Nome, vCl.Item(vInt).Arquivo) then
          Exit(True);
  finally
    vEd.Free;
  end;
end;

procedure TAcaoPacoteDesignParaRuntime.TrocarDproj(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vInt, vLinha: integer;
  vNovo: string;
  vMudou: boolean;
begin
  vLinha := 0;
  vXml := TProjetoXML.Create(AArq.Texto);
  try
    vLista := vXml.Buscar('//DCC_UsePackage');
    try
      for vInt := 0 to vLista.Count - 1 do
      begin
        vNovo := TrocarItemLista(vXml.ValorTexto(TElementoXML(vLista[vInt])),
                                 FRegra.Param('de'), FRegra.Param('para'), vMudou);
        if not vMudou then
          Continue;
        vXml.TrocarTexto(TElementoXML(vLista[vInt]), vNovo);
        if vLinha = 0 then
          vLinha := LinhaDoElemento(AArq, TElementoXML(vLista[vInt]));
      end;
    finally
      vLista.Free;
    end;
  finally
    vXml.Free;
  end;
  if vLinha > 0 then
    Feito(AArq, vLinha, Format(cmAcaoDesignParaRuntime, [FRegra.Param('de'), FRegra.Param('para')]));
end;

procedure TAcaoPacoteDesignParaRuntime.TrocarDpk(AArq: TArquivoMigrado);
var
  vEd: TEditorUses;
  vItem: TItemUses;
  vLinha: integer;
begin
  vEd := TEditorUses.Create(AArq.Texto);
  try
    if (vEd.Clausula(suRequires) = nil) or
       not vEd.ContemEm(FRegra.Param('de'), suRequires) then
      Exit;
    vItem := vEd.Achar(FRegra.Param('de'));
    vLinha := vItem.Linha;
  finally
    vEd.Free;
  end;
  if SoParaDesign(AArq) then
  begin
    Manual(AArq, vLinha, ['pacote', FRegra.Param('de')]);
    Exit;
  end;
  vEd := TEditorUses.Create(AArq.Texto);
  try
    if vEd.Trocar(FRegra.Param('de'), FRegra.Param('para')) > 0 then
      Feito(AArq, vLinha, Format(cmAcaoDesignParaRuntime, [FRegra.Param('de'), FRegra.Param('para')]));
  finally
    vEd.Free;
  end;
end;

procedure TAcaoPacoteDesignParaRuntime.ProcessarArquivo(AArq: TArquivoMigrado);
begin
  case AArq.Tipo of
    taDproj: TrocarDproj(AArq);
    taDpk: TrocarDpk(AArq);
  end;
end;

initialization
  RegistrarAcao(arPacoteRenomear, TAcaoPacoteRenomear);
  RegistrarAcao(arPacoteExigirSeUnit, TAcaoPacoteExigirSeUnit);
  RegistrarAcao(arCaminhoTrocar, TAcaoCaminhoTrocar);
  RegistrarAcao(arUnitRenomear, TAcaoUnitRenomear);
  RegistrarAcao(arUsesAdicionar, TAcaoUsesAdicionar);
  RegistrarAcao(arPacoteDesignParaRuntime, TAcaoPacoteDesignParaRuntime);

end.
