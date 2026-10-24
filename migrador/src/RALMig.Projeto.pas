/// Finds the files of a migration (stage M2): the projects, groups, units and
/// forms of a folder (recursively), and lists what the projects cite outside
/// of it (projects of a group, units by search path or "in '...'"), so that the
/// screen can ask whether they come along. Also the scope helpers the project
/// actions use: which source files belong to a project and which units they use.
unit RALMig.Projeto;

{$mode ObjFPC}{$H+}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, SysUtils,
  RALMig.Texto, RALMig.Tipos;

type
  { TLocalizadorArquivos }

  /// Builds the TConjuntoArquivos of a folder (or of one project file's folder)
  TLocalizadorArquivos = class
  private
    FApenasBase: string;
    FConjunto: TConjuntoArquivos;
    FConverterBinarios: boolean;
    FErros: TStringList;
    FPasta: string;
    FProjetosFora: TStringList;
    FUnitsFora: TStringList;
    FDono: boolean;
    procedure AdicionarArquivo(const ACaminho: string);
    procedure ConferirFormulario(AArq: TArquivoMigrado);
    function PermiteProjeto(const ACaminho: string): boolean;
    procedure Varrer(const ADir: string);
    procedure CitarProjetoFora(const ACaminho: string);
    procedure CitarUnitFora(const ACaminho: string);
    procedure LerGrupos;
    procedure LerGrupo(AArq: TArquivoMigrado);
    procedure LerCitacoes;
    procedure LerCitacoesPascal(AArq: TArquivoMigrado);
    procedure LerCitacoesXml(AArq: TArquivoMigrado);
    procedure CitarPastaDeBusca(const ADirProjeto, AItem: string);
  public
    /// APastaOuArquivo: a folder, or a project file (then its folder is used
    /// and of the projects only that one and its twin .dpr/.lpr are taken)
    constructor Create(const APastaOuArquivo: string; AConverterBinarios: boolean);
    destructor Destroy; override;
    procedure Executar;
    /// Hands the set to the caller (who frees it)
    function TomarConjunto: TConjuntoArquivos;
    /// Adds what the user accepted: a file, or every .pas/.dfm/.lfm/.fmx of a
    /// folder (not recursive)
    procedure IncluirFora(const ACaminho: string);

    property Conjunto: TConjuntoArquivos read FConjunto;
    property Erros: TStringList read FErros;
    property Pasta: string read FPasta;
    /// Projects (full paths) cited by a group but outside the folder
    property ProjetosFora: TStringList read FProjetosFora;
    /// Files cited by "in '...'" or folders cited by search paths, outside the
    /// folder, that hold units (nothing enters without IncluirFora)
    property UnitsFora: TStringList read FUnitsFora;
  end;

/// True when the path is inside the folder (APasta with trailing delimiter)
function CaminhoDentro(const APasta, ACaminho: string): boolean;
/// Is the folder (or an ancestor) a RAL source tree? (src/base|core/RALServer.pas)
function EhArvoreRAL(const ADir: string): boolean;
/// The source files (.pas .dpr .lpr .dpk) that belong to a project file: the
/// ones in its folder and below, except subfolders with a project of their own,
/// and the files its main source cites with "in '...'"
procedure ArquivosDoProjeto(AConjunto: TConjuntoArquivos; AProjeto: TArquivoMigrado;
  ALista: TList);
/// Units named in the uses/requires/contains of the files, lower case, sorted
procedure UnitsUsadas(AArquivos: TList; ADest: TStringList);

implementation

uses
  RALMig.Formulario, RALMig.ProjetoXML, RALMig.&Uses;

resourcestring
  emProjSemCaminho = 'Não existe: %s';
  emProjLeitura = 'Não foi possível ler %s: %s';
  emProjBinario = 'Formulário binário: converta para texto no Delphi ' +
    '(botão direito > Text DFM) ou deixe o migrador converter.';
  emProjFormInvalido = 'Formulário que não pode ser lido (linha %d): %s. ' +
    'Corrija na IDE e rode o migrador de novo.';

const
  PASTAS_IGNORADAS: array[0..13] of string = ('__history', '__recovery', 'backup',
    'lib', '_ral-migracao', '.git', '.svn', '.hg', 'win32', 'win64', 'osx64',
    'osxarm64', 'linux64', 'node_modules');
  PREFIXOS_SAIDA: array[0..3] of string = ('x86_64-', 'i386-', 'aarch64-', 'arm-');

function CaminhoDentro(const APasta, ACaminho: string): boolean;
var
  vPasta: string;
begin
  vPasta := IncludeTrailingPathDelimiter(APasta);
  Result := SameText(Copy(ACaminho, 1, Length(vPasta)), vPasta);
end;

function EhArvoreRAL(const ADir: string): boolean;
var
  vDir, vAnt: string;
  vNivel: integer;
begin
  Result := False;
  vDir := ExcludeTrailingPathDelimiter(ExpandFileName(ADir));
  for vNivel := 0 to 6 do
  begin
    if FileExists(vDir + '/src/base/RALServer.pas') or
       FileExists(vDir + '/src/core/RALServer.pas') or
       FileExists(vDir + '/base/RALServer.pas') or
       FileExists(vDir + '/core/RALServer.pas') then
      Exit(True);
    vAnt := vDir;
    vDir := ExcludeTrailingPathDelimiter(ExtractFilePath(vDir));
    if (vDir = '') or (vDir = vAnt) then
      Break;
  end;
end;

function PastaIgnorada(const ANome: string): boolean;
var
  vInt: integer;
  vNome: string;
begin
  vNome := LowerCase(ANome);
  for vInt := Low(PASTAS_IGNORADAS) to High(PASTAS_IGNORADAS) do
    if vNome = PASTAS_IGNORADAS[vInt] then
      Exit(True);
  for vInt := Low(PREFIXOS_SAIDA) to High(PREFIXOS_SAIDA) do
    if Copy(vNome, 1, Length(PREFIXOS_SAIDA[vInt])) = PREFIXOS_SAIDA[vInt] then
      Exit(True);
  Result := False;
end;

function EhArquivoDeProjeto(ATipo: TTipoArquivo): boolean;
begin
  Result := ATipo in [taDproj, taDpk, taLpi, taLpk, taLpr, taDpr];
end;

{ TLocalizadorArquivos }

constructor TLocalizadorArquivos.Create(const APastaOuArquivo: string;
  AConverterBinarios: boolean);
var
  vCaminho: string;
begin
  inherited Create;
  FErros := TStringList.Create;
  FProjetosFora := TStringList.Create;
  FProjetosFora.Sorted := True;
  FProjetosFora.Duplicates := dupIgnore;
  FUnitsFora := TStringList.Create;
  FUnitsFora.Sorted := True;
  FUnitsFora.Duplicates := dupIgnore;
  FDono := True;
  FConverterBinarios := AConverterBinarios;
  vCaminho := ExpandFileName(APastaOuArquivo);
  if DirectoryExists(vCaminho) then
    FPasta := IncludeTrailingPathDelimiter(vCaminho)
  else if FileExists(vCaminho) then
  begin
    FPasta := ExtractFilePath(vCaminho);
    if EhArquivoDeProjeto(TipoDoArquivo(vCaminho)) then
      FApenasBase := LowerCase(ChangeFileExt(vCaminho, ''));
  end
  else
  begin
    FPasta := IncludeTrailingPathDelimiter(vCaminho);
    FErros.Add(Format(emProjSemCaminho, [APastaOuArquivo]));
  end;
  FConjunto := TConjuntoArquivos.Create(FPasta);
end;

destructor TLocalizadorArquivos.Destroy;
begin
  if FDono then
    FConjunto.Free;
  FErros.Free;
  FProjetosFora.Free;
  FUnitsFora.Free;
  inherited Destroy;
end;

function TLocalizadorArquivos.TomarConjunto: TConjuntoArquivos;
begin
  Result := FConjunto;
  FDono := False;
end;

function TLocalizadorArquivos.PermiteProjeto(const ACaminho: string): boolean;
begin
  Result := (FApenasBase = '') or
            (LowerCase(ChangeFileExt(ACaminho, '')) = FApenasBase);
end;

procedure TLocalizadorArquivos.ConferirFormulario(AArq: TArquivoMigrado);
begin
  try
    if EhFormularioBinario(AArq.Texto.Bytes) then
    begin
      if not FConverterBinarios then
      begin
        AArq.Ignorado := True;
        AArq.Motivo := emProjBinario;
        Exit;
      end;
      ConverterBinarioParaTexto(AArq.Texto);
    end;
    // formulario que nao analisa (objeto sem end...) fica de fora uma vez so,
    // com a linha, em vez de cada regra tropecar nele; nada e "consertado"
    TFormulario.Create(AArq.Texto).Free;
  except
    on E: EFormularioInvalido do
    begin
      AArq.Ignorado := True;
      AArq.Motivo := Format(emProjFormInvalido, [E.Linha, E.Message]);
    end;
    on E: Exception do
    begin
      AArq.Ignorado := True;
      AArq.Motivo := Format(emProjLeitura, [AArq.Relativo, E.Message]);
    end;
  end;
end;

procedure TLocalizadorArquivos.AdicionarArquivo(const ACaminho: string);
var
  vTipo: TTipoArquivo;
  vArq: TArquivoMigrado;
begin
  vTipo := TipoDoArquivo(ACaminho);
  if vTipo = taOutro then
    Exit;
  if EhArquivoDeProjeto(vTipo) and not PermiteProjeto(ACaminho) then
    Exit;
  vArq := FConjunto.Adicionar(ACaminho);
  if vTipo = taForm then
    ConferirFormulario(vArq);
end;

procedure TLocalizadorArquivos.Varrer(const ADir: string);
var
  vBusca: TSearchRec;
begin
  if FindFirst(ADir + '*', faAnyFile, vBusca) <> 0 then
    Exit;
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
      begin
        if {$IFDEF UNIX}((vBusca.Attr and faSymLink) = 0) and{$ENDIF}
           not PastaIgnorada(vBusca.Name) then
          Varrer(ADir + vBusca.Name + PathDelim);
      end
      else
        AdicionarArquivo(ADir + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

procedure TLocalizadorArquivos.CitarProjetoFora(const ACaminho: string);
var
  vCaminho: string;
begin
  vCaminho := ExpandFileName(ACaminho);
  if FileExists(vCaminho) and not CaminhoDentro(FPasta, vCaminho) then
    FProjetosFora.Add(vCaminho);
end;

procedure TLocalizadorArquivos.CitarUnitFora(const ACaminho: string);
var
  vCaminho: string;
begin
  vCaminho := ExpandFileName(ACaminho);
  if FileExists(vCaminho) and not CaminhoDentro(FPasta, vCaminho) then
    FUnitsFora.Add(vCaminho);
end;

function CaminhoDoSistema(const ACaminho: string): string;
begin
  Result := StringReplace(ACaminho, '\', PathDelim, [rfReplaceAll]);
  Result := StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
end;

procedure TLocalizadorArquivos.LerGrupo(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vInt, vAt: integer;
  vEl: TElementoXML;
  vValor: string;
begin
  vXml := TProjetoXML.Create(AArq.Texto);
  try
    vLista := vXml.Buscar('//*');
    try
      for vInt := 0 to vLista.Count - 1 do
      begin
        vEl := TElementoXML(vLista[vInt]);
        for vAt := 0 to vEl.AtributoCount - 1 do
        begin
          if not (SameText(vEl.Atributos[vAt].Nome, 'Include') or
                  SameText(vEl.Atributos[vAt].Nome, 'FileName')) then
            Continue;
          vValor := vXml.ValorAtributo(vEl, vEl.Atributos[vAt].Nome);
          if EhArquivoDeProjeto(TipoDoArquivo(vValor)) then
            CitarProjetoFora(ExtractFilePath(AArq.Caminho) + CaminhoDoSistema(vValor));
        end;
      end;
    finally
      vLista.Free;
    end;
  finally
    vXml.Free;
  end;
end;

procedure TLocalizadorArquivos.LerGrupos;
var
  vLista: TList;
  vInt: integer;
begin
  vLista := TList.Create;
  try
    FConjunto.Listar([taGroupproj, taLpg], vLista);
    for vInt := 0 to vLista.Count - 1 do
      try
        LerGrupo(TArquivoMigrado(vLista[vInt]));
      except
        on E: Exception do
          FErros.Add(Format(emProjLeitura, [TArquivoMigrado(vLista[vInt]).Relativo, E.Message]));
      end;
  finally
    vLista.Free;
  end;
end;

procedure TLocalizadorArquivos.CitarPastaDeBusca(const ADirProjeto, AItem: string);
var
  vItem, vDir: string;
  vBusca: TSearchRec;
  vTemPas: boolean;
begin
  vItem := Trim(AItem);
  if (vItem = '') or (Pos('$', vItem) > 0) then
    Exit;
  vDir := CaminhoDoSistema(vItem);
  if ExtractFileDrive(vDir) = '' then
    vDir := ADirProjeto + vDir;
  vDir := IncludeTrailingPathDelimiter(ExpandFileName(vDir));
  if not DirectoryExists(vDir) or CaminhoDentro(FPasta, vDir) or
     SameText(vDir, FPasta) or EhArvoreRAL(vDir) then
    Exit;
  vTemPas := FindFirst(vDir + '*.pas', faAnyFile, vBusca) = 0;
  if vTemPas then
    FindClose(vBusca);
  if vTemPas then
    FUnitsFora.Add(vDir);
end;

procedure TLocalizadorArquivos.LerCitacoesPascal(AArq: TArquivoMigrado);
var
  vEd: TEditorUses;
  vCl, vIt: integer;
  vArquivo: string;
begin
  vEd := TEditorUses.Create(AArq.Texto);
  try
    for vCl := 0 to vEd.Count - 1 do
      for vIt := 0 to vEd.Clausulas[vCl].Count - 1 do
      begin
        vArquivo := vEd.Clausulas[vCl].Item(vIt).Arquivo;
        if vArquivo <> '' then
          CitarUnitFora(ExtractFilePath(AArq.Caminho) + CaminhoDoSistema(vArquivo));
      end;
  finally
    vEd.Free;
  end;
end;

procedure TLocalizadorArquivos.LerCitacoesXml(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vLista: TList;
  vInt: integer;
  vItens: TStringList;
  vDir, vPath: string;
begin
  vDir := ExtractFilePath(AArq.Caminho);
  vXml := TProjetoXML.Create(AArq.Texto);
  vItens := TStringList.Create;
  try
    vItens.Delimiter := ';';
    vItens.StrictDelimiter := True;
    if AArq.Tipo = taDproj then
    begin
      vLista := vXml.Buscar('//DCC_UnitSearchPath');
      try
        for vInt := 0 to vLista.Count - 1 do
          vItens.DelimitedText := vItens.DelimitedText + ';' + vXml.ValorTexto(TElementoXML(vLista[vInt]));
      finally
        vLista.Free;
      end;
      vLista := vXml.Buscar('//DCCReference');
      try
        for vInt := 0 to vLista.Count - 1 do
        begin
          vPath := vXml.ValorAtributo(TElementoXML(vLista[vInt]), 'Include');
          if TipoDoArquivo(vPath) = taPas then
            CitarUnitFora(vDir + CaminhoDoSistema(vPath));
        end;
      finally
        vLista.Free;
      end;
    end
    else
    begin
      vLista := vXml.Buscar('//OtherUnitFiles');
      try
        for vInt := 0 to vLista.Count - 1 do
          vItens.DelimitedText := vItens.DelimitedText + ';' + vXml.ValorAtributo(TElementoXML(vLista[vInt]), 'Value');
      finally
        vLista.Free;
      end;
    end;
    for vInt := 0 to vItens.Count - 1 do
      CitarPastaDeBusca(vDir, vItens[vInt]);
  finally
    vItens.Free;
    vXml.Free;
  end;
end;

procedure TLocalizadorArquivos.LerCitacoes;
var
  vLista: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  vLista := TList.Create;
  try
    FConjunto.Listar([taDproj, taLpi, taLpk, taDpr, taLpr, taDpk], vLista);
    for vInt := 0 to vLista.Count - 1 do
    begin
      vArq := TArquivoMigrado(vLista[vInt]);
      try
        if vArq.Tipo in [taDpr, taLpr, taDpk] then
          LerCitacoesPascal(vArq)
        else
          LerCitacoesXml(vArq);
      except
        on E: Exception do
          FErros.Add(Format(emProjLeitura, [vArq.Relativo, E.Message]));
      end;
    end;
  finally
    vLista.Free;
  end;
end;

procedure TLocalizadorArquivos.Executar;
begin
  if not DirectoryExists(FPasta) then
    Exit;
  Varrer(FPasta);
  LerGrupos;
  LerCitacoes;
end;

procedure TLocalizadorArquivos.IncluirFora(const ACaminho: string);
var
  vBusca: TSearchRec;
  vDir: string;
begin
  if DirectoryExists(ACaminho) then
  begin
    vDir := IncludeTrailingPathDelimiter(ACaminho);
    if FindFirst(vDir + '*', faAnyFile, vBusca) = 0 then
    try
      repeat
        if (vBusca.Attr and faDirectory) = 0 then
          if TipoDoArquivo(vBusca.Name) in [taPas, taForm] then
            AdicionarArquivo(vDir + vBusca.Name);
      until FindNext(vBusca) <> 0;
    finally
      FindClose(vBusca);
    end;
  end
  else if FileExists(ACaminho) then
    AdicionarArquivo(ACaminho);
end;

{ Escopo de um projeto }

procedure ArquivosDoProjeto(AConjunto: TConjuntoArquivos; AProjeto: TArquivoMigrado;
  ALista: TList);
var
  vDir: string;
  vProjetos, vCandidatos, vSubpastas: TList;
  vInt, vSub: integer;
  vArq: TArquivoMigrado;
  vFora: boolean;
  vPrincipal: TArquivoMigrado;
  vEd: TEditorUses;
  vCl, vIt: integer;
begin
  vDir := ExtractFilePath(AProjeto.Caminho);
  vProjetos := TList.Create;
  vCandidatos := TList.Create;
  vSubpastas := TList.Create;
  try
    AConjunto.Listar([taDproj, taLpi, taDpk, taLpk], vProjetos);
    for vInt := 0 to vProjetos.Count - 1 do
    begin
      vArq := TArquivoMigrado(vProjetos[vInt]);
      if CaminhoDentro(vDir, vArq.Caminho) and
         not SameText(ExtractFilePath(vArq.Caminho), vDir) then
        vSubpastas.Add(vArq);
    end;
    AConjunto.Listar([taPas, taDpr, taLpr, taDpk], vCandidatos);
    for vInt := 0 to vCandidatos.Count - 1 do
    begin
      vArq := TArquivoMigrado(vCandidatos[vInt]);
      if not CaminhoDentro(vDir, vArq.Caminho) then
        Continue;
      vFora := False;
      for vSub := 0 to vSubpastas.Count - 1 do
        if CaminhoDentro(ExtractFilePath(TArquivoMigrado(vSubpastas[vSub]).Caminho),
                         vArq.Caminho) then
          vFora := True;
      if not vFora then
        ALista.Add(vArq);
    end;
    // units that the main source cites by path from outside the folder
    vPrincipal := AConjunto.Buscar(ChangeFileExt(AProjeto.Caminho, '.dpr'));
    if vPrincipal = nil then
      vPrincipal := AConjunto.Buscar(ChangeFileExt(AProjeto.Caminho, '.lpr'));
    if vPrincipal <> nil then
    begin
      vEd := TEditorUses.Create(vPrincipal.Texto);
      try
        for vCl := 0 to vEd.Count - 1 do
          for vIt := 0 to vEd.Clausulas[vCl].Count - 1 do
            if vEd.Clausulas[vCl].Item(vIt).Arquivo <> '' then
            begin
              vArq := AConjunto.Buscar(vDir + CaminhoDoSistema(vEd.Clausulas[vCl].Item(vIt).Arquivo));
              if (vArq <> nil) and (ALista.IndexOf(vArq) < 0) then
                ALista.Add(vArq);
            end;
      finally
        vEd.Free;
      end;
    end;
  finally
    vSubpastas.Free;
    vCandidatos.Free;
    vProjetos.Free;
  end;
end;

procedure UnitsUsadas(AArquivos: TList; ADest: TStringList);
var
  vInt, vCl, vIt: integer;
  vEd: TEditorUses;
begin
  ADest.Sorted := True;
  ADest.Duplicates := dupIgnore;
  ADest.CaseSensitive := False;
  for vInt := 0 to AArquivos.Count - 1 do
  begin
    vEd := TEditorUses.Create(TArquivoMigrado(AArquivos[vInt]).Texto);
    try
      for vCl := 0 to vEd.Count - 1 do
        for vIt := 0 to vEd.Clausulas[vCl].Count - 1 do
          ADest.Add(LowerCase(vEd.Clausulas[vCl].Item(vIt).Nome));
    finally
      vEd.Free;
    end;
  end;
end;

end.
