program apidiff;

{$mode ObjFPC}{$H+}

// Ferramenta de desenvolvedor do migrador (etapa M0): extrai a API publica de
// uma versao do RAL, compara duas versoes e confere a cobertura das quebras.
//
//   apidiff extrair <zip|pasta> <rotulo> [--saida=<pasta>]
//   apidiff comparar <api-a.json> <api-b.json>
//   apidiff inventario [--api=<pasta>] [--cobertura=<arquivo>] [--saida=<arquivo>]
//   apidiff pendentes [--pre-lancamento] [--api=<pasta>] [--cobertura=<arquivo>]
//   apidiff aviso <api-ultimo-release.json> <api-dev.json> [--cobertura=<arquivo>] [--pre-lancamento]
//
// pendentes sai com 1 quando ha diferenca sem regra nem justificativa num passo
// fechado (ou em qualquer passo com --pre-lancamento).
//
// aviso compara duas APIs avulsas (o ultimo release e o dev, no workflow) e lista
// o que nao tem regra nem justificativa no passo da cobertura cujo de/para bata
// com os rotulos das duas (sem esse passo, nada cobre). Com GITHUB_ACTIONS
// escreve anotacoes ::warning::. Sai com 0; com --pre-lancamento, com 1 se sobrar
// alguma.

uses
  Classes, SysUtils, Contnrs,
  RALInst.Catalogo, RALInst.Zip,
  RALMig.API, RALMig.Inventario;

var
  GPosicionais: TStringList;
  GOpcoes: TStringList;

function Opcao(const ANome, APadrao: string): string;
begin
  Result := GOpcoes.Values[ANome];
  if Result = '' then
    Result := APadrao;
end;

function TemOpcao(const ANome: string): boolean;
begin
  Result := GOpcoes.IndexOfName(ANome) >= 0;
end;

procedure LerArgumentos;
var
  vInt: integer;
  vArg: string;
begin
  for vInt := 1 to ParamCount do
  begin
    vArg := ParamStr(vInt);
    if Copy(vArg, 1, 2) = '--' then
    begin
      Delete(vArg, 1, 2);
      if Pos('=', vArg) = 0 then
        vArg := vArg + '=1';
      GOpcoes.Add(vArg);
    end
    else
      GPosicionais.Add(vArg);
  end;
end;

procedure Uso;
begin
  WriteLn('apidiff extrair <zip|pasta> <rotulo> [--saida=<pasta>]');
  WriteLn('apidiff comparar <api-a.json> <api-b.json>');
  WriteLn('apidiff inventario [--api=<pasta>] [--cobertura=<arquivo>] [--saida=<arquivo>]');
  WriteLn('apidiff pendentes [--pre-lancamento] [--api=<pasta>] [--cobertura=<arquivo>]');
  WriteLn('apidiff aviso <api-ultimo-release.json> <api-dev.json> [--cobertura=<arquivo>] [--pre-lancamento]');
end;

/// Hash of HEAD of a git checkout, read from the files ('' when it cannot)
function CommitDaPasta(const APasta: string): string;
var
  vLista: TStringList;
  vHead, vRef, vArq: string;
  vInt: integer;
begin
  Result := '';
  vArq := IncludeTrailingPathDelimiter(APasta) + '.git' + PathDelim + 'HEAD';
  if not FileExists(vArq) then
    Exit;
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(vArq);
    vHead := Trim(vLista.Text);
    if Copy(vHead, 1, 5) <> 'ref: ' then
      Exit(vHead);
    vRef := Copy(vHead, 6, MaxInt);
    vArq := IncludeTrailingPathDelimiter(APasta) + '.git' + PathDelim +
            StringReplace(vRef, '/', PathDelim, [rfReplaceAll]);
    if FileExists(vArq) then
    begin
      vLista.LoadFromFile(vArq);
      Exit(Trim(vLista.Text));
    end;
    vArq := IncludeTrailingPathDelimiter(APasta) + '.git' + PathDelim + 'packed-refs';
    if FileExists(vArq) then
    begin
      vLista.LoadFromFile(vArq);
      for vInt := 0 to Pred(vLista.Count) do
        if Pos(' ' + vRef, vLista[vInt]) > 0 then
          Exit(Copy(vLista[vInt], 1, Pos(' ', vLista[vInt]) - 1));
    end;
  finally
    vLista.Free;
  end;
end;

function PastaMigrador: string;
begin
  Result := AcharPastaMigrador;
  if Result = '' then
    Result := GetCurrentDir;
end;

function CmdExtrair: integer;
var
  vOrigem: TOrigemArquivos;
  vApi: TApi;
  vCaminho, vRotulo, vSaida: string;
begin
  Result := 2;
  if GPosicionais.Count < 3 then
  begin
    Uso;
    Exit;
  end;
  vCaminho := ExpandFileName(GPosicionais[1]);
  vRotulo := GPosicionais[2];
  if DirectoryExists(vCaminho) then
    vOrigem := TOrigemLocal.Create(vCaminho)
  else if FileExists(vCaminho) then
    vOrigem := TOrigemZip.Create(vCaminho, 'zip ' + ExtractFileName(vCaminho))
  else
  begin
    WriteLn('Nao existe: ', vCaminho);
    Exit;
  end;
  vApi := ExtrairAPI(vOrigem, vRotulo);
  try
    if DirectoryExists(vCaminho) then
    begin
      vApi.Origem := 'pasta ' + ExtractFileName(ExcludeTrailingPathDelimiter(vCaminho)) +
                     ' (arvore de trabalho)';
      vApi.Commit := CommitDaPasta(vCaminho);
    end;
    vSaida := ArquivoApi(Opcao('saida', PastaMigrador + PathDelim + 'api'), vRotulo);
    vApi.SalvarArquivo(vSaida);
    WriteLn(vSaida);
    WriteLn(Format('units=%d classes=%d tipos=%d rotinas=%d constantes=%d registradas=%d ' +
                   'pacotes=%d erros=%d', [vApi.Unidades.Count, vApi.Classes.Count,
                   vApi.Tipos.Count, vApi.Rotinas.Count, vApi.Constantes.Count,
                   vApi.Registradas.Count, vApi.PacoteCount, vApi.Erros.Count]));
    WriteLn(vApi.Erros.Text);
    Result := 0;
  finally
    vApi.Free;
  end;
end;

function CmdComparar: integer;
var
  vA, vN: TApi;
  vDif: TDiferencas;
  vInt: integer;
begin
  Result := 2;
  if GPosicionais.Count < 3 then
  begin
    Uso;
    Exit;
  end;
  vA := TApi.Create;
  vN := TApi.Create;
  vDif := TDiferencas.Create;
  try
    vA.CarregarArquivo(GPosicionais[1]);
    vN.CarregarArquivo(GPosicionais[2]);
    CompararAPIs(vA, vN, vDif);
    for vInt := 0 to Pred(vDif.Count) do
      WriteLn(vDif[vInt].Chave, #9, vDif[vInt].Detalhe);
    Result := 0;
  finally
    vDif.Free;
    vA.Free;
    vN.Free;
  end;
end;

/// Loads the coverage; False (with the reasons printed) when it is invalid
function CarregarCobertura(ACob: TCobertura): boolean;
var
  vArq: string;
  vInt: integer;
begin
  vArq := Opcao('cobertura', PastaMigrador + PathDelim + 'regras' + PathDelim + 'cobertura.json');
  Result := False;
  if not FileExists(vArq) then
    WriteLn('Nao existe: ', vArq)
  else if not ACob.CarregarArquivo(vArq) then
  begin
    WriteLn('Cobertura invalida (', vArq, '):');
    for vInt := 0 to Pred(ACob.Erros.Count) do
      WriteLn('  ', ACob.Erros[vInt]);
  end
  else
    Result := True;
end;

procedure ProcessarTodos(ACob: TCobertura; AResultados: TObjectList);
var
  vInt: integer;
  vApi: string;
begin
  vApi := Opcao('api', PastaMigrador + PathDelim + 'api');
  for vInt := 0 to Pred(ACob.PassoCount) do
    AResultados.Add(ProcessarPasso(ACob.Passos[vInt], vApi));
end;

procedure GravarTexto(const AArquivo, ATexto: string);
var
  vStream: TFileStream;
begin
  ForceDirectories(ExtractFilePath(ExpandFileName(AArquivo)));
  vStream := TFileStream.Create(AArquivo, fmCreate);
  try
    if ATexto <> '' then
      vStream.WriteBuffer(ATexto[1], Length(ATexto));
  finally
    vStream.Free;
  end;
end;

function CmdInventario: integer;
var
  vCob: TCobertura;
  vRes: TObjectList;
  vSaida: string;
begin
  Result := 2;
  vCob := TCobertura.Create;
  vRes := TObjectList.Create(True);
  try
    if not CarregarCobertura(vCob) then
      Exit;
    ProcessarTodos(vCob, vRes);
    vSaida := Opcao('saida', PastaMigrador + PathDelim + 'regras' + PathDelim + 'INVENTARIO.md');
    GravarTexto(vSaida, GerarInventario(vCob, vRes));
    WriteLn(vSaida);
    Result := 0;
  finally
    vRes.Free;
    vCob.Free;
  end;
end;

procedure ListarPasso(AResultado: TResultadoPasso; var APendentesFechado: boolean;
  APreLancamento: boolean);
var
  vInt: integer;
  vDif: TDiferenca;
begin
  if AResultado.Erro <> '' then
  begin
    WriteLn('[', AResultado.Passo.Rotulo, '] ', AResultado.Erro);
    APendentesFechado := True;
    Exit;
  end;
  WriteLn(Format('[%s] %s: %d diferencas, %d pendentes, %d itens de cobertura sem correspondencia',
          [AResultado.Passo.Rotulo, BoolToStr(AResultado.Passo.Fechado, 'fechado', 'aberto'),
           AResultado.Diferencas.Count, AResultado.Pendentes.Count, AResultado.Obsoletos.Count]));
  for vInt := 0 to Pred(AResultado.Pendentes.Count) do
  begin
    vDif := TDiferenca(AResultado.Pendentes[vInt]);
    WriteLn('  ', vDif.Chave, #9, vDif.Detalhe);
  end;
  for vInt := 0 to Pred(AResultado.Obsoletos.Count) do
    WriteLn('  aviso: sem correspondencia: ', AResultado.Obsoletos[vInt]);
  if (AResultado.Pendentes.Count > 0) and (AResultado.Passo.Fechado or APreLancamento) then
    APendentesFechado := True;
end;

function CmdPendentes: integer;
var
  vCob: TCobertura;
  vRes: TObjectList;
  vInt: integer;
  vFalha: boolean;
begin
  Result := 2;
  vCob := TCobertura.Create;
  vRes := TObjectList.Create(True);
  try
    if not CarregarCobertura(vCob) then
      Exit;
    ProcessarTodos(vCob, vRes);
    vFalha := False;
    for vInt := 0 to Pred(vRes.Count) do
      ListarPasso(TResultadoPasso(vRes[vInt]), vFalha, TemOpcao('pre-lancamento'));
    if vFalha then
      Result := 1
    else
      Result := 0;
  finally
    vRes.Free;
    vCob.Free;
  end;
end;

function CmdAviso: integer;
var
  vCob: TCobertura;
  vA, vN: TApi;
  vRes: TResultadoPasso;
begin
  Result := 2;
  if GPosicionais.Count < 3 then
  begin
    Uso;
    Exit;
  end;
  vCob := TCobertura.Create;
  vA := TApi.Create;
  vN := TApi.Create;
  try
    if not CarregarCobertura(vCob) then
      Exit;
    vA.CarregarArquivo(GPosicionais[1]);
    vN.CarregarArquivo(GPosicionais[2]);
    vRes := CompararComCobertura(vA, vN, vCob);
    try
      Write(TextoAviso(vRes, GetEnvironmentVariable('GITHUB_ACTIONS') <> ''));
      WriteLn(Format('[%s] %d diferencas, %d sem regra nem justificativa',
              [vRes.Passo.Rotulo, vRes.Diferencas.Count, vRes.Pendentes.Count]));
      if TemOpcao('pre-lancamento') and (vRes.Pendentes.Count > 0) then
        Result := 1
      else
        Result := 0;
    finally
      vRes.Free;
    end;
  finally
    vN.Free;
    vA.Free;
    vCob.Free;
  end;
end;

var
  GVerbo: string;

begin
  GPosicionais := TStringList.Create;
  GOpcoes := TStringList.Create;
  try
    LerArgumentos;
    if GPosicionais.Count = 0 then
    begin
      Uso;
      Halt(2);
    end;
    GVerbo := LowerCase(GPosicionais[0]);
    if GVerbo = 'extrair' then
      ExitCode := CmdExtrair
    else if GVerbo = 'comparar' then
      ExitCode := CmdComparar
    else if GVerbo = 'inventario' then
      ExitCode := CmdInventario
    else if GVerbo = 'pendentes' then
      ExitCode := CmdPendentes
    else if GVerbo = 'aviso' then
      ExitCode := CmdAviso
    else
    begin
      Uso;
      ExitCode := 2;
    end;
  finally
    GPosicionais.Free;
    GOpcoes.Free;
  end;
end.
