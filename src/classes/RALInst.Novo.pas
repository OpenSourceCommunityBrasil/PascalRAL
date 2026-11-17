/// New RAL 2.0 projects from templates ('ralcli novo'): a server (engine,
/// plugins, a CRUD resource over SQLite) or a client, for Delphi (.dpr, .dproj)
/// and Lazarus (.lpr, .lpi). The components live in a data module (.dfm/.lfm),
/// (unit uDados), so the project is edited in the Object Inspector, not in code. The
/// templates are RCDATA resources of the executable (MODELO_*, from the
/// modelos folder of the repository); a modelos folder next to the executable
/// replaces them. The template language is a small Mustache: {{name}},
/// {{#name}}...{{/name}} (kept when the value is not empty nor 0) and
/// {{^name}}...{{/name}} (the opposite); a line holding only a section tag goes
/// away whole.
unit RALInst.Novo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  /// What the project is.
  TNovoTipo = (ntServidor, ntCliente);

  /// What 'ralcli novo' was asked.
  TNovoOpcoes = record
    /// Make the CRUD resource /itens over SQLite (servers).
    Crud: boolean;
    /// Write the Delphi project.
    Delphi: boolean;
    /// Write into a folder that already has files.
    Forcar: boolean;
    /// Write the Lazarus project.
    Lazarus: boolean;
    /// Engine; empty is the default of each IDE.
    Motor: string;
    /// Project name (a Pascal identifier).
    Nome: string;
    /// Folder where the project folder is made.
    Pasta: string;
    /// Plugins, comma separated: cors, compress, log, health, metrics.
    Plugins: string;
    /// Port of the server, or the one the client calls.
    Porta: integer;
    /// Server or client.
    Tipo: TNovoTipo;
  end;

{ Writes the project of AOpcoes, one line per file in ALog; False with the
  reason in ALog when something is wrong (nothing is written then) }
function NovoProjeto(const AOpcoes: TNovoOpcoes; ALog: TStrings): boolean;
/// Renders the template AModelo with the values of AValores (name=value).
function RenderizarModelo(const AModelo: string; AValores: TStrings): string;
/// The template ANome (programa_servidor, dados_cliente_form...); '' when there is none.
function TextoDoModelo(const ANome: string): string;
/// The engines of a project of ATipo in Delphi (ADelphi) or Lazarus, comma separated.
function MotoresValidos(ATipo: TNovoTipo; ADelphi: boolean): string;

implementation

const
  // RT_RCDATA: no Windows vem da unit Windows, nos outros do system
  RecursoDados = PChar(10);
  /// Plugins a server template knows.
  cPlugins = 'cors,compress,log,health,metrics';

type
  /// One engine of one IDE.
  TNovoMotor = record
    /// The name in --motor.
    Nome: string;
    /// Unit of the server or client class.
    Unidade: string;
    /// Class of the server (servers).
    Classe: string;
    /// TRALClient.EngineType (clients).
    EngineType: string;
    /// Lazarus package of the engine.
    Pacote: string;
  end;

const
  cServidoresDelphi: array[0..2] of TNovoMotor = (
    (Nome: 'indy'; Unidade: 'RALIndyServer'; Classe: 'TRALIndyServer'; EngineType: ''; Pacote: ''),
    (Nome: 'synopse'; Unidade: 'RALSynopseServer'; Classe: 'TRALSynopseServer'; EngineType: ''; Pacote: ''),
    (Nome: 'sagui'; Unidade: 'RALSaguiServer'; Classe: 'TRALSaguiServer'; EngineType: ''; Pacote: ''));
  cServidoresLazarus: array[0..3] of TNovoMotor = (
    (Nome: 'fphttp'; Unidade: 'RALfpHTTPServer'; Classe: 'TRALfpHttpServer'; EngineType: ''; Pacote: 'fphttpral'),
    (Nome: 'indy'; Unidade: 'RALIndyServer'; Classe: 'TRALIndyServer'; EngineType: ''; Pacote: 'IndyRAL'),
    (Nome: 'synopse'; Unidade: 'RALSynopseServer'; Classe: 'TRALSynopseServer'; EngineType: ''; Pacote: 'SynopseRAL'),
    (Nome: 'sagui'; Unidade: 'RALSaguiServer'; Classe: 'TRALSaguiServer'; EngineType: ''; Pacote: 'SaguiRAL'));
  cClientesDelphi: array[0..2] of TNovoMotor = (
    (Nome: 'nethttp'; Unidade: 'RALnetHTTPClient'; Classe: ''; EngineType: 'netHttp'; Pacote: ''),
    (Nome: 'indy'; Unidade: 'RALIndyClient'; Classe: ''; EngineType: 'Indy'; Pacote: ''),
    (Nome: 'synopse'; Unidade: 'RALSynopseClient'; Classe: ''; EngineType: 'mORMot2'; Pacote: ''));
  cClientesLazarus: array[0..2] of TNovoMotor = (
    (Nome: 'fphttp'; Unidade: 'RALfpHTTPClient'; Classe: ''; EngineType: 'fpHttp'; Pacote: 'fphttpral'),
    (Nome: 'indy'; Unidade: 'RALIndyClient'; Classe: ''; EngineType: 'Indy'; Pacote: 'IndyRAL'),
    (Nome: 'synopse'; Unidade: 'RALSynopseClient'; Classe: ''; EngineType: 'mORMot2'; Pacote: 'SynopseRAL'));

function TextoDoModelo(const ANome: string): string;
var
  vArquivo: string;
  vStream: TStream;
  vBytes: RawByteString;
begin
  Result := '';
  // a modelos folder next to the executable wins (to try a template without rebuilding)
  vArquivo := ExtractFilePath(ParamStr(0)) + 'modelos' + PathDelim + ANome + '.txt';
  vStream := nil;
  try
    if FileExists(vArquivo) then
      vStream := TFileStream.Create(vArquivo, fmOpenRead or fmShareDenyWrite)
    else
      try
        vStream := TResourceStream.Create(HINSTANCE, 'MODELO_' + UpperCase(ANome), RecursoDados);
      except
        Exit;
      end;
    SetLength(vBytes, vStream.Size);
    if vStream.Size > 0 then
      vStream.ReadBuffer(vBytes[1], vStream.Size);
    Result := string(vBytes);
  finally
    vStream.Free;
  end;
end;

/// Whether the value of AName in AValores turns a section on.
function Ligado(AValores: TStrings; const AName: string): boolean;
var
  vValor: string;
begin
  vValor := AValores.Values[AName];
  Result := (vValor <> '') and (vValor <> '0');
end;

/// Takes away the lines that hold only a section tag (their indentation and line break).
function TirarLinhasDeTag(const ATexto: string): string;
var
  vLinhas: TStringList;
  vInt: integer;
  vLinha: string;
begin
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := ATexto;
    Result := '';
    for vInt := 0 to vLinhas.Count - 1 do
    begin
      vLinha := Trim(vLinhas[vInt]);
      if (Copy(vLinha, 1, 3) = '{{#') or (Copy(vLinha, 1, 3) = '{{^') or (Copy(vLinha, 1, 3) = '{{/') then
        if (Copy(vLinha, Length(vLinha) - 1, 2) = '}}') and (Pos('}}', vLinha) = Length(vLinha) - 1) then
        begin
          Result := Result + vLinha; // the tag alone, without the line
          Continue;
        end;
      Result := Result + vLinhas[vInt] + LineEnding;
    end;
  finally
    vLinhas.Free;
  end;
end;

function Renderizar(const ATexto: string; AValores: TStrings): string;
var
  vPos, vFim, vNivel, vBusca, vFimTag: integer;
  vTag, vNome, vDentro: string;
  vInverso: boolean;
begin
  Result := '';
  vPos := 1;
  while vPos <= Length(ATexto) do
  begin
    vFim := Pos('{{', Copy(ATexto, vPos, MaxInt));
    if vFim = 0 then
    begin
      Result := Result + Copy(ATexto, vPos, MaxInt);
      Break;
    end;
    Result := Result + Copy(ATexto, vPos, vFim - 1);
    vPos := vPos + vFim - 1;
    vFimTag := Pos('}}', Copy(ATexto, vPos + 2, MaxInt));
    if vFimTag = 0 then
      raise Exception.Create('modelo com {{ sem }}');
    vTag := Copy(ATexto, vPos + 2, vFimTag - 1);
    vPos := vPos + 2 + vFimTag + 1;
    if (vTag <> '') and (vTag[1] in ['#', '^']) then
    begin
      vInverso := vTag[1] = '^';
      vNome := Copy(vTag, 2, MaxInt);
      // the matching close, counting sections of the same name inside
      vNivel := 1;
      vBusca := vPos;
      vFim := 0;
      while vBusca <= Length(ATexto) do
      begin
        if (Copy(ATexto, vBusca, Length(vNome) + 5) = '{{#' + vNome + '}}') or
           (Copy(ATexto, vBusca, Length(vNome) + 5) = '{{^' + vNome + '}}') then
          Inc(vNivel)
        else if Copy(ATexto, vBusca, Length(vNome) + 5) = '{{/' + vNome + '}}' then
        begin
          Dec(vNivel);
          if vNivel = 0 then
          begin
            vFim := vBusca;
            Break;
          end;
        end;
        Inc(vBusca);
      end;
      if vFim = 0 then
        raise Exception.Create('modelo sem {{/' + vNome + '}}');
      vDentro := Copy(ATexto, vPos, vFim - vPos);
      if Ligado(AValores, vNome) <> vInverso then
        Result := Result + Renderizar(vDentro, AValores);
      vPos := vFim + Length(vNome) + 5;
    end
    else
      Result := Result + AValores.Values[Trim(vTag)];
  end;
end;

function RenderizarModelo(const AModelo: string; AValores: TStrings): string;
begin
  Result := Renderizar(TirarLinhasDeTag(AModelo), AValores);
end;

function MotoresValidos(ATipo: TNovoTipo; ADelphi: boolean): string;
var
  vInt: integer;
begin
  Result := '';
  if ATipo = ntServidor then
  begin
    if ADelphi then
      for vInt := 0 to High(cServidoresDelphi) do
        Result := Result + ',' + cServidoresDelphi[vInt].Nome
    else
      for vInt := 0 to High(cServidoresLazarus) do
        Result := Result + ',' + cServidoresLazarus[vInt].Nome;
  end
  else if ADelphi then
    for vInt := 0 to High(cClientesDelphi) do
      Result := Result + ',' + cClientesDelphi[vInt].Nome
  else
    for vInt := 0 to High(cClientesLazarus) do
      Result := Result + ',' + cClientesLazarus[vInt].Nome;
  Delete(Result, 1, 1);
end;

/// The engine AMotor (the first of the table when empty); False when unknown.
function AcharMotor(ATipo: TNovoTipo; ADelphi: boolean; const AMotor: string;
  out AAchado: TNovoMotor): boolean;

  function Buscar(const ALista: array of TNovoMotor): boolean;
  var
    vInt: integer;
  begin
    Result := False;
    for vInt := 0 to High(ALista) do
      if (AMotor = '') or SameText(ALista[vInt].Nome, AMotor) then
      begin
        AAchado := ALista[vInt];
        Exit(True);
      end;
  end;

begin
  if ATipo = ntServidor then
  begin
    if ADelphi then
      Result := Buscar(cServidoresDelphi)
    else
      Result := Buscar(cServidoresLazarus);
  end
  else if ADelphi then
    Result := Buscar(cClientesDelphi)
  else
    Result := Buscar(cClientesLazarus);
end;

function NomeValido(const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := (ANome <> '') and (ANome[1] in ['A'..'Z', 'a'..'z', '_']) and
    not SameText(ANome, 'Dados') and not SameText(ANome, 'uDados');
  if Result then
    for vInt := 2 to Length(ANome) do
      if not (ANome[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
        Exit(False);
end;

/// The RequiredPackages block of the .lpi.
function PacotesLPI(const APacotes: array of string): string;
var
  vInt: integer;
begin
  Result := '    <RequiredPackages Count="' + IntToStr(Length(APacotes)) + '">' + LineEnding;
  for vInt := 0 to High(APacotes) do
    Result := Result + '      <Item' + IntToStr(vInt + 1) + '>' + LineEnding +
      '        <PackageName Value="' + APacotes[vInt] + '"/>' + LineEnding +
      '      </Item' + IntToStr(vInt + 1) + '>' + LineEnding;
  Result := Result + '    </RequiredPackages>';
end;

/// Saves AText to AFile as it is (UTF-8, the line breaks of the templates).
procedure Gravar(const AFile, AText: string);
var
  vStream: TFileStream;
  vBytes: RawByteString;
begin
  vBytes := RawByteString(AText);
  vStream := TFileStream.Create(AFile, fmCreate);
  try
    if vBytes <> '' then
      vStream.WriteBuffer(vBytes[1], Length(vBytes));
  finally
    vStream.Free;
  end;
end;

function NovoProjeto(const AOpcoes: TNovoOpcoes; ALog: TStrings): boolean;
var
  vRaiz, vPasta, vPlugin, vBase: string;
  vPlugins, vValores, vPacotes: TStringList;
  vInt, vIDE: integer;
  vDelphi: boolean;
  vMotor: TNovoMotor;
  vGuid: TGUID;
  vArquivos: TStringList;
  vBusca: TSearchRec;
  vPacotesArr: array of string;

  function Modelo(const ANome: string): string;
  begin
    Result := TextoDoModelo(ANome);
    if Result = '' then
      raise Exception.Create('o modelo ' + ANome + ' nao esta neste executavel');
    Result := RenderizarModelo(Result, vValores);
  end;

begin
  Result := False;
  if not NomeValido(AOpcoes.Nome) then
  begin
    ALog.Add('nome invalido: "' + AOpcoes.Nome + '" (letras, digitos e _, comecando por letra; "Dados" e "uDados" sao do data module)');
    Exit;
  end;
  if not (AOpcoes.Delphi or AOpcoes.Lazarus) then
  begin
    ALog.Add('escolha a IDE: --ide=delphi, --ide=lazarus ou --ide=delphi,lazarus');
    Exit;
  end;
  if (AOpcoes.Porta < 1) or (AOpcoes.Porta > 65535) then
  begin
    ALog.Add('porta invalida: ' + IntToStr(AOpcoes.Porta));
    Exit;
  end;
  if (AOpcoes.Tipo = ntCliente) and ((AOpcoes.Plugins <> '') or AOpcoes.Crud) then
  begin
    ALog.Add('--plugins e --crud sao do servidor');
    Exit;
  end;
  vPlugins := TStringList.Create;
  vValores := TStringList.Create;
  vPacotes := TStringList.Create;
  vArquivos := TStringList.Create;
  try
    vPlugins.CommaText := LowerCase(AOpcoes.Plugins);
    for vPlugin in vPlugins do
      if Pos(',' + vPlugin + ',', ',' + cPlugins + ',') = 0 then
      begin
        ALog.Add('plugin desconhecido: ' + vPlugin + ' (conhecidos: ' + cPlugins + ')');
        Exit;
      end;
    for vIDE := 0 to 1 do
    begin
      vDelphi := vIDE = 0;
      if (vDelphi and not AOpcoes.Delphi) or ((not vDelphi) and not AOpcoes.Lazarus) then
        Continue;
      if not AcharMotor(AOpcoes.Tipo, vDelphi, AOpcoes.Motor, vMotor) then
      begin
        if vDelphi then
          ALog.Add('motor desconhecido no Delphi: ' + AOpcoes.Motor + ' (validos: ' +
            MotoresValidos(AOpcoes.Tipo, True) + ')')
        else
          ALog.Add('motor desconhecido no Lazarus: ' + AOpcoes.Motor + ' (validos: ' +
            MotoresValidos(AOpcoes.Tipo, False) + ')');
        Exit;
      end;
    end;
    vRaiz := IncludeTrailingPathDelimiter(ExpandFileName(AOpcoes.Pasta)) + AOpcoes.Nome;
    if DirectoryExists(vRaiz) and not AOpcoes.Forcar then
      if FindFirst(IncludeTrailingPathDelimiter(vRaiz) + '*', faAnyFile, vBusca) = 0 then
      try
        repeat
          if (vBusca.Name <> '.') and (vBusca.Name <> '..') then
          begin
            ALog.Add('a pasta ' + vRaiz + ' ja tem arquivos (use --forcar para escrever por cima)');
            Exit;
          end;
        until FindNext(vBusca) <> 0;
      finally
        FindClose(vBusca);
      end;

    // everything checked: the files
    for vIDE := 0 to 1 do
    begin
      vDelphi := vIDE = 0;
      if (vDelphi and not AOpcoes.Delphi) or ((not vDelphi) and not AOpcoes.Lazarus) then
        Continue;
      AcharMotor(AOpcoes.Tipo, vDelphi, AOpcoes.Motor, vMotor);
      vPasta := vRaiz;
      if AOpcoes.Delphi and AOpcoes.Lazarus then
      begin
        if vDelphi then
          vPasta := vPasta + PathDelim + 'Delphi'
        else
          vPasta := vPasta + PathDelim + 'Lazarus';
      end;
      ForceDirectories(vPasta);
      vValores.Clear;
      vValores.Values['NOME'] := AOpcoes.Nome;
      vValores.Values['NOME_ARQ'] := AOpcoes.Nome;
      vValores.Values['PORTA'] := IntToStr(AOpcoes.Porta);
      if vDelphi then
        vValores.Values['DELPHI'] := '1'
      else
        vValores.Values['LAZARUS'] := '1';
      vValores.Values['MOTOR_UNIT'] := vMotor.Unidade;
      vValores.Values['MOTOR_CLASS'] := vMotor.Classe;
      vValores.Values['CLIENTE_UNIT'] := vMotor.Unidade;
      vValores.Values['CLIENTE_MOTOR'] := vMotor.EngineType;
      for vPlugin in vPlugins do
        vValores.Values[UpperCase(vPlugin)] := '1';
      if (vPlugins.IndexOf('log') >= 0) or (vPlugins.IndexOf('health') >= 0) or
         (vPlugins.IndexOf('metrics') >= 0) then
        vValores.Values['OBS'] := '1';
      if AOpcoes.Crud then
      begin
        vValores.Values['CRUD'] := '1';
        if vDelphi then
        begin
          vValores.Values['DB_LINK'] := 'FireDAC';
          vValores.Values['DB_UNITS'] := 'RALDBFireDAC, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper.Stat,' +
            LineEnding + '  FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.ConsoleUI.Wait';
        end
        else
        begin
          vValores.Values['DB_LINK'] := 'SQLDB';
          vValores.Values['DB_UNITS'] := 'RALDBSQLDB';
        end;
      end;
      // the packages of the Lazarus project
      vPacotes.Clear;
      vPacotes.Add('LCLBase');
      vPacotes.Add('PascalRAL');
      vPacotes.Add(vMotor.Pacote);
      if vValores.Values['OBS'] <> '' then
        vPacotes.Add('RALPluginObservabilityPackage');
      if AOpcoes.Crud then
      begin
        vPacotes.Add('RALDBPackage');
        vPacotes.Add('raldbsqldbpackage');
      end;
      SetLength(vPacotesArr, vPacotes.Count);
      for vInt := 0 to vPacotes.Count - 1 do
        vPacotesArr[vInt] := vPacotes[vInt];
      vValores.Values['PACOTES_LPI'] := PacotesLPI(vPacotesArr);
      CreateGUID(vGuid);
      vValores.Values['GUID'] := GUIDToString(vGuid);

      if AOpcoes.Tipo = ntServidor then
        vBase := 'servidor'
      else
        vBase := 'cliente';
      vArquivos.Clear;
      if vDelphi then
      begin
        vArquivos.Values[AOpcoes.Nome + '.dpr'] := 'programa_' + vBase;
        vArquivos.Values[AOpcoes.Nome + '.dproj'] := 'projeto_dproj';
        vArquivos.Values['uDados.pas'] := 'dados_' + vBase;
        vArquivos.Values['uDados.dfm'] := 'dados_' + vBase + '_form';
      end
      else
      begin
        vArquivos.Values[AOpcoes.Nome + '.lpr'] := 'programa_' + vBase;
        vArquivos.Values[AOpcoes.Nome + '.lpi'] := 'projeto_lpi';
        vArquivos.Values['uDados.pas'] := 'dados_' + vBase;
        vArquivos.Values['uDados.lfm'] := 'dados_' + vBase + '_form';
      end;
      for vInt := 0 to vArquivos.Count - 1 do
      begin
        Gravar(IncludeTrailingPathDelimiter(vPasta) + vArquivos.Names[vInt],
          Modelo(vArquivos.ValueFromIndex[vInt]));
        ALog.Add(IncludeTrailingPathDelimiter(vPasta) + vArquivos.Names[vInt]);
      end;
    end;
    Result := True;
  finally
    vArquivos.Free;
    vPacotes.Free;
    vValores.Free;
    vPlugins.Free;
  end;
end;

end.
