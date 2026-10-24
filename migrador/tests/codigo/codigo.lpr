program codigo;

{$mode ObjFPC}{$H+}

// Regras de codigo do migrador (etapa M5): RALMig.Acoes.Codigo.
//   codigo --testes                autoteste: aplica as regras de codigo dos passos
//                                  (1.0>1.1, 1.1>1.2, 1.2>2.0) as fixtures de
//                                  migrador/tests/codigo/fixtures/entrada, compara com
//                                  fixtures/esperado byte a byte, confere os itens
//                                  manuais (regra@linha), nome em comentario/string/
//                                  diretiva intacto e segunda rodada sem mudancas
//   codigo --compilar              alem disso, compila o resultado contra o RAL 2.0 com
//                                  o FPC (modos Delphi e ObjFPC)
//   codigo --sem-falso-positivo    roda as regras de codigo do passo 1.2>2.0 sobre os
//                                  .pas/.dpr/.lpr do PascalRAL-Samples-2.0 (copia em
//                                  memoria, nada e gravado): zero mudancas, zero itens
//                                  manuais (fora os somente-relatorio)
//   --explorar --de=<v> --para=<v>  so mostra o que as regras fariam (sem exigir zero)
//   --samples=<pasta>  --ral=<PascalRAL-2.0>  --fpc=<fpc.exe>  --saida=<pasta>
//                                  (saida: grava o resultado de cada fixture)
//
// Sai com 1 se algo falhou. Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/codigo/codigo.lpr

uses
  Classes, SysUtils, Process,
  RALMig.Texto, RALMig.Tokens, RALMig.&Uses, RALMig.API, RALMig.Regras,
  RALMig.Tipos, RALMig.Acoes.Codigo, RALMig.Acoes.Projeto, RALMig.Acoes.Formulario;

var
  GFalhas: integer = 0;
  GRaiz: string = '';
  GTemp: string = '';
  GSaida: string = '';
  GSamples: string = 'D:\Projetos\Delphi\OpenSourceCommunityBrasil\PascalRAL\PascalRAL-Samples-2.0';
  GRal2: string = '';
{$IFDEF MSWINDOWS}
  GFpc: string = 'D:\IDE\Lazarus\4.6\fpc\bin\x86_64-win64\fpc.exe';
  GLazUtils: string = 'D:\IDE\Lazarus\4.6\lazarus\components\lazutils\lib\x86_64-win64';
{$ELSE}
  GFpc: string = 'fpc';
  GLazUtils: string = '';
{$ENDIF}
  GRegras: TConjuntoRegras;
  GApis: TStringList;
  GCompilar: boolean = False;
  GDe: string = '1.2';
  GPara: string = '2.0';
  GExplorar: boolean = False;

procedure Conferir(ACondicao: boolean; const ADescricao: string);
begin
  if ACondicao then
    WriteLn('  ok    ', ADescricao)
  else
  begin
    WriteLn('  FALHA ', ADescricao);
    Inc(GFalhas);
  end;
end;

function Visivel(const AText: string): string;
begin
  Result := StringReplace(StringReplace(AText, #13, '\r', [rfReplaceAll]), #10, '\n', [rfReplaceAll]);
end;

{ ambiente }

function AcharRaiz: string;
var
  vDir: string;
  i: integer;
begin
  for i := 0 to 1 do
  begin
    if i = 0 then
      vDir := ExpandFileName(GetCurrentDir)
    else
      vDir := ExtractFilePath(ExpandFileName(ParamStr(0)));
    vDir := ExcludeTrailingPathDelimiter(vDir);
    while vDir <> '' do
    begin
      if DirectoryExists(vDir + PathDelim + 'migrador' + PathDelim + 'regras') then
        Exit(IncludeTrailingPathDelimiter(vDir));
      if ExtractFileDir(vDir) = vDir then
        Break;
      vDir := ExtractFileDir(vDir);
    end;
  end;
  Result := '';
end;

function ApiDaVersao(const AVersao: string): TApi;
var
  vInd: integer;
  vArq: string;
begin
  vInd := GApis.IndexOf(AVersao);
  if vInd >= 0 then
    Exit(TApi(GApis.Objects[vInd]));
  if AVersao = '1.0' then
    vArq := 'api-v1.0.json'
  else if AVersao = '1.1' then
    vArq := 'api-1.1.json'
  else if AVersao = '1.2' then
    vArq := 'api-dev.json'
  else
    vArq := 'api-2.0.json';
  Result := TApi.Create;
  Result.CarregarArquivo(GRaiz + 'migrador' + PathDelim + 'api' + PathDelim + vArq);
  GApis.AddObject(AVersao, Result);
end;

procedure Preparar;
var
  vOk: boolean;
begin
  GRaiz := AcharRaiz;
  if GRaiz = '' then
  begin
    WriteLn('Nao achei migrador/regras: rode da raiz do instalador.');
    Halt(2);
  end;
  GApis := TStringList.Create;
  GRegras := TConjuntoRegras.Create;
  vOk := GRegras.CarregarDePasta(GRaiz + 'migrador' + PathDelim + 'regras', True);
  if not vOk then
  begin
    WriteLn(GRegras.Erros.Text);
    Halt(2);
  end;
  GTemp := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-codigo-' + IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(GTemp);
  if GRal2 = '' then
    GRal2 := ExtractFilePath(ExcludeTrailingPathDelimiter(GRaiz)) + 'PascalRAL-2.0';
end;

{ aplicacao das regras de codigo }

procedure UsesAdicionar(ARegra: TRegraMigracao; AContexto: TContextoMigracao);
var
  vArqs: TList;
  vInt, i: integer;
  vArq: TArquivoMigrado;
  vToks: TListaTokens;
  vEd: TEditorUses;
  vUsa: boolean;
  vSecao: TSecaoUses;
begin
  vArqs := TList.Create;
  try
    AContexto.Conjunto.Listar([taPas, taDpr, taLpr], vArqs);
    for vInt := 0 to vArqs.Count - 1 do
    begin
      vArq := TArquivoMigrado(vArqs[vInt]);
      vToks := TokenizarPascal(vArq.Texto.Bytes, vArq.Texto.InicioConteudo);
      vEd := TEditorUses.Create(vArq.Texto);
      try
        vUsa := False;
        for i := 0 to vToks.Count - 1 do
          if (vToks[i].Tipo = ttIdentificador) and
            (ARegra.Lista('quando-identificador').IndexOf(vToks.Texto(i)) >= 0) then
            vUsa := True;
        if vUsa and not vEd.Contem(ARegra.Param('unit')) then
        begin
          if vEd.Tipo = tuPrograma then
            vSecao := suPrograma
          else
            vSecao := suInterface;
          vEd.Acrescentar(ARegra.Param('unit'), vSecao);
        end;
      finally
        vEd.Free;
        vToks.Free;
      end;
    end;
  finally
    vArqs.Free;
  end;
end;

function EhCodigo(ARegra: TRegraMigracao): boolean;
begin
  Result := (TiposDaRegra(ARegra) * [taPas, taDpr, taLpr]) <> [];
end;

procedure Despachar(ARegra: TRegraMigracao; AContexto: TContextoMigracao; ASoMinhas: boolean);
var
  vAcao: TAcaoMigracao;
begin
  case ARegra.Acao of
    arIdentificadorRenomear:
      begin
        vAcao := TAcaoIdentificadorRenomear.Create;
        try
          vAcao.Aplicar(ARegra, AContexto);
        finally
          vAcao.Free;
        end;
      end;
    arPadraoAvisar:
      begin
        vAcao := TAcaoPadraoAvisar.Create;
        try
          vAcao.Aplicar(ARegra, AContexto);
        finally
          vAcao.Free;
        end;
      end;
    arPropriedadeRenomear:
      if EhCodigo(ARegra) then
        RenomearPropriedadeNoCodigo(ARegra, AContexto);
    arPropriedadeParaComponente:
      if EhCodigo(ARegra) then
        PluginEmCodigo(ARegra, AContexto);
    arUsesAdicionar:
      if not ASoMinhas then
        UsesAdicionar(ARegra, AContexto);
    arUnitRenomear, arObjetoSemEquivalente:
      if not ASoMinhas then
      begin
        vAcao := ClasseDaAcao(ARegra.Acao).Create;
        try
          vAcao.Aplicar(ARegra, AContexto);
        finally
          vAcao.Free;
        end;
      end;
  end;
end;

procedure Rodar(AConj: TConjuntoArquivos; AResultado: TResultadoMigracao;
  const ADe, APara: string; ASoMinhas: boolean);
var
  vCad: TListaPassos;
  vErro: TCadeiaErro;
  vMsg: string;
  vCtx: TContextoMigracao;
  i, j: integer;
begin
  vCad := GRegras.Cadeia(OrigemRAL, ADe, APara, vErro, vMsg);
  vCtx := TContextoMigracao.Create;
  try
    if vErro <> ceNenhum then
    begin
      Conferir(False, 'cadeia ' + ADe + '>' + APara + ': ' + vMsg);
      Exit;
    end;
    vCtx.Conjunto := AConj;
    vCtx.Resultado := AResultado;
    vCtx.Idioma := 'pt-BR';
    for i := 0 to vCad.Count - 1 do
    begin
      vCtx.Passo := vCad[i];
      vCtx.ApiDe := ApiDaVersao(vCad[i].De);
      vCtx.ApiPara := ApiDaVersao(vCad[i].Para);
      for j := 0 to vCad[i].RegraCount - 1 do
      begin
        Despachar(vCad[i].Regras[j], vCtx, ASoMinhas);
        AConj.Consolidar;
      end;
    end;
  finally
    vCtx.Free;
    vCad.Free;
  end;
end;

{ fixtures }

function CopiarArquivo(const ADe, APara: string): boolean;
var
  vBytes: AnsiString;
  vFs: TFileStream;
begin
  vBytes := LerBytes(ADe);
  ForceDirectories(ExtractFilePath(APara));
  vFs := TFileStream.Create(APara, fmCreate);
  try
    if vBytes <> '' then
      vFs.WriteBuffer(vBytes[1], Length(vBytes));
  finally
    vFs.Free;
  end;
  Result := True;
end;

procedure GravarBytes(const ACaminho: string; const ABytes: RawByteString);
var
  vFs: TFileStream;
begin
  ForceDirectories(ExtractFilePath(ACaminho));
  vFs := TFileStream.Create(ACaminho, fmCreate);
  try
    if ABytes <> '' then
      vFs.WriteBuffer(ABytes[1], Length(ABytes));
  finally
    vFs.Free;
  end;
end;

function Fixture(const ASub, ANome: string): string;
begin
  Result := GRaiz + 'migrador' + PathDelim + 'tests' + PathDelim + 'codigo' + PathDelim +
    'fixtures' + PathDelim + ASub + PathDelim + ANome + '.pas';
end;

procedure DespejarItens(const ACaminho: string; AResultado: TResultadoMigracao);
var
  vLista: TStringList;
  i: integer;
begin
  vLista := TStringList.Create;
  try
    for i := 0 to AResultado.ManualCount - 1 do
      with AResultado.Manuais[i] do
      begin
        vLista.Add('[' + Regra + '] ' + Arquivo + ':' + IntToStr(Linha));
        vLista.Add('  problema: ' + Problema);
        vLista.Add('  fazer:    ' + Fazer);
        vLista.Add('  trecho:   ' + StringReplace(Trecho, #10, ' | ', [rfReplaceAll]));
        vLista.Add('  wiki:     ' + Wiki);
      end;
    for i := 0 to AResultado.FeitoCount - 1 do
      with AResultado.Feitos[i] do
        vLista.Add('feito [' + Regra + '] ' + Arquivo + ':' + IntToStr(Linha) + ' ' + Descricao);
    vLista.SaveToFile(ACaminho);
  finally
    vLista.Free;
  end;
end;

function ListaDeItens(AResultado: TResultadoMigracao): TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to AResultado.ManualCount - 1 do
    if AResultado.Manuais[i].Arquivo <> '' then
      Result.Add(AResultado.Manuais[i].Regra + '@' + IntToStr(AResultado.Manuais[i].Linha));
  Result.Sort;
end;

function JuntarOrdenado(const AItens: array of string): string;
var
  vLista: TStringList;
  i: integer;
begin
  vLista := TStringList.Create;
  try
    for i := 0 to High(AItens) do
      vLista.Add(AItens[i]);
    vLista.Sort;
    Result := vLista.CommaText;
  finally
    vLista.Free;
  end;
end;

function Compilar(const ANome: string; const ABytes: RawByteString; const AModo: string): boolean;
var
  vProc: TProcess;
  vPasta, vArq, vSaida: string;
  vBytes: RawByteString;
  vBuf: array[0..4095] of char;
  vLidos: integer;
  vFus: array of string;
  vLinhas: TStringList;
  i: integer;
begin
  vPasta := GTemp + 'cc-' + AModo + PathDelim + ANome + PathDelim;
  ForceDirectories(vPasta);
  vBytes := ABytes;
  if AModo = 'delphi' then
    vBytes := StringReplace(vBytes, '{$mode objfpc}', '{$mode delphi}', [rfIgnoreCase]);
  vArq := vPasta + ANome + '.pas';
  GravarBytes(vArq, vBytes);
  vFus := nil;
  SetLength(vFus, 12);
  vFus[0] := '-Fu' + GLazUtils;
  vFus[1] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'core';
  vFus[2] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'common';
  vFus[3] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'compress';
  vFus[4] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'crypto';
  vFus[5] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'plugins';
  vFus[6] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'plugins' + DirectorySeparator + 'auth';
  vFus[7] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'plugins' + DirectorySeparator + 'tls';
  vFus[8] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'engine' + DirectorySeparator + 'fpHTTP';
  vFus[9] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'storage';
  vFus[10] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'bindings';
  vFus[11] := '-Fu' + GRal2 + DirectorySeparator + 'src' + DirectorySeparator + 'modules' + DirectorySeparator + 'web';
  FillChar(vBuf, SizeOf(vBuf), 0);
  vProc := TProcess.Create(nil);
  try
    vProc.Executable := GFpc;
    vProc.Parameters.Add('-Mobjfpc');
    vProc.Parameters.Add('-Sh');
    vProc.Parameters.Add('-Cn');
    vProc.Parameters.Add('-s');
    vProc.Parameters.Add('-ve');
    vProc.Parameters.Add('-FU' + vPasta);
    for i := 0 to High(vFus) do
      vProc.Parameters.Add(vFus[i]);
    vProc.Parameters.Add(vArq);
    vProc.Options := [poUsePipes, poNoConsole, poStderrToOutput];
    vProc.Execute;
    vSaida := '';
    repeat
      vLidos := vProc.Output.Read(vBuf, SizeOf(vBuf));
      if vLidos > 0 then
        vSaida := vSaida + Copy(vBuf, 0, vLidos);
    until vLidos <= 0;
    vProc.WaitOnExit;
    Result := vProc.ExitStatus = 0;
    if not Result then
    begin
      vLinhas := TStringList.Create;
      try
        vLinhas.Text := vSaida;
        for i := 0 to vLinhas.Count - 1 do
          if (Pos('Error', vLinhas[i]) > 0) or (Pos('Fatal', vLinhas[i]) > 0) then
            WriteLn('        ', vLinhas[i]);
      finally
        vLinhas.Free;
      end;
    end;
  finally
    vProc.Free;
  end;
end;

procedure Caso(const ANome, ADe, APara: string; const AItens: array of string;
  ACompilar: boolean);
var
  vPasta, vPasta2, vEsperado: string;
  vConj, vConj2: TConjuntoArquivos;
  vRes, vRes2: TResultadoMigracao;
  vArq, vArq2: TArquivoMigrado;
  vItens: TStringList;
  vAtual, vEsp: RawByteString;
begin
  WriteLn('caso ', ANome, ' (', ADe, ' > ', APara, ')');
  vPasta := GTemp + ANome + PathDelim;
  CopiarArquivo(Fixture('entrada', ANome), vPasta + ANome + '.pas');
  vConj := TConjuntoArquivos.Create(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    vArq := vConj.Adicionar(vPasta + ANome + '.pas');
    Rodar(vConj, vRes, ADe, APara, False);
    vAtual := vArq.Atual;
    if GSaida <> '' then
      GravarBytes(IncludeTrailingPathDelimiter(GSaida) + ANome + '.pas', vAtual);
    if GSaida <> '' then
      DespejarItens(IncludeTrailingPathDelimiter(GSaida) + ANome + '.itens.txt', vRes);
    vEsperado := Fixture('esperado', ANome);
    if FileExists(vEsperado) then
    begin
      vEsp := LerBytes(vEsperado);
      Conferir(vAtual = vEsp, 'saida igual ao esperado');
      if vAtual <> vEsp then
      begin
        WriteLn('---- obtido ----');
        WriteLn(Visivel(vAtual));
      end;
    end
    else
    begin
      Conferir(False, 'falta o esperado ' + vEsperado);
      WriteLn('---- obtido ----');
      WriteLn(vAtual);
    end;
    vItens := ListaDeItens(vRes);
    try
      Conferir(vItens.CommaText = JuntarOrdenado(AItens),
        'itens manuais (regra@linha): ' + vItens.CommaText);
      if vItens.CommaText <> JuntarOrdenado(AItens) then
        WriteLn('        esperado: ', JuntarOrdenado(AItens));
    finally
      vItens.Free;
    end;
    Conferir(vRes.Erros.Count = 0, 'sem erros do motor ' + vRes.Erros.Text);
    // second round over the result: no change
    vPasta2 := GTemp + ANome + '-2' + PathDelim;
    GravarBytes(vPasta2 + ANome + '.pas', vAtual);
    vConj2 := TConjuntoArquivos.Create(vPasta2);
    vRes2 := TResultadoMigracao.Create;
    try
      vArq2 := vConj2.Adicionar(vPasta2 + ANome + '.pas');
      Rodar(vConj2, vRes2, ADe, APara, False);
      Conferir(not vArq2.Alterado, 'segunda rodada nao muda nada');
      if vArq2.Alterado then
        WriteLn(Visivel(vArq2.Atual));
      Conferir(vRes2.ManualCount = vRes.ManualCount, 'segunda rodada: os mesmos itens manuais');
    finally
      vRes2.Free;
      vConj2.Free;
    end;
    if ACompilar and GCompilar then
    begin
      Conferir(Compilar(ANome, vAtual, 'objfpc'), 'compila contra o RAL 2.0 (FPC, ObjFPC)');
      Conferir(Compilar(ANome, vAtual, 'delphi'), 'compila contra o RAL 2.0 (FPC, Delphi)');
    end;
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

{ formulario e regra sintetica }

function Q(const ATexto: string): string;
begin
  Result := StringReplace(ATexto, '''', '"', [rfReplaceAll]);
end;

procedure TesteClasseNoFormulario;
var
  vRegras: TConjuntoRegras;
  vPasta: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
  vCtx: TContextoMigracao;
  vRegra: TRegraMigracao;
  vJson, vTx: string;
  vPas, vDfm: TArquivoMigrado;
  vAcao: TAcaoIdentificadorRenomear;
begin
  WriteLn('caso classe em formulario (regra sintetica)');
  vTx := '{''pt-BR'':''x'',''en-US'':''x'',''es-ES'':''x''}';
  vJson := Q('{''formato'':1,''origem'':''RAL'',''de'':''1.0'',''para'':''1.1'',''regras'':[' +
    '{''id'':''1.1/x'',''acao'':''identificador-renomear'',''onde'':[''pas'',''form''],' +
    '''mapa'':{''TFoo'':''TBar''},''descricao'':' + vTx + ',''manual'':' + vTx + '}]}');
  vRegras := TConjuntoRegras.Create;
  vConj := nil;
  vRes := TResultadoMigracao.Create;
  vCtx := TContextoMigracao.Create;
  try
    Conferir(vRegras.CarregarTexto(vJson, 'sintetico', False), 'regra sintetica carrega ' + vRegras.Erros.Text);
    vRegra := vRegras.BuscarRegra('1.1/x');
    vPasta := GTemp + 'form' + PathDelim;
    GravarBytes(vPasta + 'U.pas', 'unit U;'#13#10'interface'#13#10'type'#13#10'  TFoo = class end;'#13#10 +
      'var'#13#10'  F: TFoo; { TFoo }'#13#10'implementation'#13#10'end.'#13#10);
    GravarBytes(vPasta + 'U.dfm', 'object Form1: TForm1'#13#10'  object Foo1: TFoo'#13#10'  end'#13#10'end'#13#10);
    vConj := TConjuntoArquivos.Create(vPasta);
    vPas := vConj.Adicionar(vPasta + 'U.pas');
    vDfm := vConj.Adicionar(vPasta + 'U.dfm');
    vCtx.Conjunto := vConj;
    vCtx.Resultado := vRes;
    vCtx.Idioma := 'pt-BR';
    vAcao := TAcaoIdentificadorRenomear.Create;
    try
      vAcao.Aplicar(vRegra, vCtx);
    finally
      vAcao.Free;
    end;
    vConj.Consolidar;
    Conferir(Pos('object Foo1: TBar', vDfm.Atual) > 0, 'object Foo1: TFoo virou TBar no formulario');
    Conferir(Pos('F: TBar; { TFoo }', vPas.Atual) > 0, 'F: TFoo virou TBar no codigo, comentario intacto');
    Conferir(Pos(#13#10'  TBar = class', vPas.Atual) > 0, 'declaracao da classe renomeada, CRLF preservado');
  finally
    vCtx.Free;
    vRes.Free;
    vConj.Free;
    vRegras.Free;
  end;
end;

{ sem falso positivo }

procedure Coletar(const APasta: string; AConj: TConjuntoArquivos);
var
  vSr: TSearchRec;
  vExt: string;
begin
  if FindFirst(APasta + '*', faAnyFile, vSr) <> 0 then
    Exit;
  try
    repeat
      if (vSr.Name = '.') or (vSr.Name = '..') then
        Continue;
      if (vSr.Attr and faDirectory) <> 0 then
      begin
        if (vSr.Name <> '__history') and (vSr.Name <> '.git') and (vSr.Name <> 'lib') then
          Coletar(APasta + vSr.Name + PathDelim, AConj);
      end
      else
      begin
        vExt := LowerCase(ExtractFileExt(vSr.Name));
        if (vExt = '.pas') or (vExt = '.dpr') or (vExt = '.lpr') then
          AConj.Adicionar(APasta + vSr.Name);
      end;
    until FindNext(vSr) <> 0;
  finally
    FindClose(vSr);
  end;
end;

procedure SemFalsoPositivo;
var
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
  vAlt: TList;
  i, vManuais: integer;
begin
  WriteLn('sem falso positivo: ', GSamples);
  if not DirectoryExists(GSamples) then
  begin
    Conferir(False, 'pasta dos samples existe');
    Exit;
  end;
  vConj := TConjuntoArquivos.Create(GSamples);
  vRes := TResultadoMigracao.Create;
  vAlt := TList.Create;
  try
    Coletar(IncludeTrailingPathDelimiter(GSamples), vConj);
    WriteLn('  arquivos de codigo: ', vConj.Count);
    Rodar(vConj, vRes, GDe, GPara, True);
    vConj.ListarAlterados(vAlt);
    for i := 0 to vAlt.Count - 1 do
      WriteLn('  ALTEROU ', TArquivoMigrado(vAlt[i]).Relativo);
    vManuais := 0;
    for i := 0 to vRes.ManualCount - 1 do
      if vRes.Manuais[i].Arquivo <> '' then
      begin
        Inc(vManuais);
        WriteLn('  ITEM ', vRes.Manuais[i].Regra, ' ', vRes.Manuais[i].Arquivo, ':', vRes.Manuais[i].Linha);
      end;
    if GExplorar then
    begin
      if GSaida <> '' then
        for i := 0 to vAlt.Count - 1 do
          GravarBytes(IncludeTrailingPathDelimiter(GSaida) + ExtractFileName(TArquivoMigrado(vAlt[i]).Caminho),
            TArquivoMigrado(vAlt[i]).Atual);
      WriteLn('  (exploracao: ', vAlt.Count, ' arquivos mudariam, ', vManuais, ' itens manuais, ', vRes.FeitoCount, ' feitos)');
      Exit;
    end;
    Conferir(vAlt.Count = 0, 'nenhum arquivo dos samples 2.0 mudou');
    Conferir(vManuais = 0, 'nenhum item manual nos samples 2.0');
    Conferir(vRes.Erros.Count = 0, 'sem erros do motor ' + vRes.Erros.Text);
  finally
    vAlt.Free;
    vRes.Free;
    vConj.Free;
  end;
end;

{ main }

procedure Testes;
begin
  // the lines are those of the text when the rule ran (later rules may add lines)
  Caso('UCripto', '1.0', '1.1', [], False);
  Caso('UAvisos10', '1.0', '1.1', ['1.1/addcookie-obsoleto@39', '1.1/dbbase-descendente@13',
    '1.1/jwt-eventos-of-object@28', '1.1/jwt-eventos-of-object@29'], False);
  Caso('USagui', '1.1', '1.2', ['1.2/cliente-chamada-sincrona@40',
    '1.2/params-body-obsoleto@44', '1.2/params-por-origem@42', '1.2/sagui-maxconnections@27',
    '1.2/webmodule-domain@43'], False);
  Caso('UPadroes12', '1.1', '1.2', ['1.2/cliente-chamada-sincrona@35',
    '1.2/cliente-chamada-sincrona@38', '1.2/cookie-samesite-zero@32', '1.2/cookie-samesite-zero@39',
    '1.2/cors-codigo-aviso@52', '1.2/memtable-excecoes@44'], False);
  Caso('UServidor', '1.2', '2.0', [], True);
  Caso('UPlugins20', '1.2', '2.0', [], True);
  Caso('UPadroes20', '1.2', '2.0', ['2.0/classe-security-sumiu@22', '2.0/oauth1-removido@19', '2.0/oauth1-removido@20',
    '2.0/plugin-blacklist@32', '2.0/plugin-securityheaders@33', '2.0/plugin-whitelist@31'], False);
  Caso('UNaoProvado', '1.2', '2.0', ['2.0/plugin-compress@24', '2.0/plugin-compress@30',
    '2.0/plugin-compress@32', '2.0/plugin-cors@25'], False);
  Caso('UAvisos20', '1.2', '2.0', ['2.0/auth-attemptof@13', '2.0/classe-auth-server-base@21',
    '2.0/corpo-somente-leitura@32', '2.0/digest-stale@32'], False);
  TesteClasseNoFormulario;
end;

var
  gArg: integer;
  gTestes, gFalso: boolean;
  gTxt: string;

begin
  DefaultSystemCodePage := CP_UTF8;
  gTestes := False;
  gFalso := False;
  for gArg := 1 to ParamCount do
  begin
    gTxt := ParamStr(gArg);
    if gTxt = '--testes' then
      gTestes := True
    else if gTxt = '--compilar' then
    begin
      gTestes := True;
      GCompilar := True;
    end
    else if gTxt = '--sem-falso-positivo' then
      gFalso := True
    else if Copy(gTxt, 1, 10) = '--explorar' then
      GExplorar := True
    else if Copy(gTxt, 1, 5) = '--de=' then
      GDe := Copy(gTxt, 6, MaxInt)
    else if Copy(gTxt, 1, 7) = '--para=' then
      GPara := Copy(gTxt, 8, MaxInt)
    else if Copy(gTxt, 1, 10) = '--samples=' then
      GSamples := Copy(gTxt, 11, MaxInt)
    else if Copy(gTxt, 1, 6) = '--ral=' then
      GRal2 := Copy(gTxt, 7, MaxInt)
    else if Copy(gTxt, 1, 11) = '--lazutils=' then
      GLazUtils := Copy(gTxt, 12, MaxInt)
    else if Copy(gTxt, 1, 6) = '--fpc=' then
      GFpc := Copy(gTxt, 7, MaxInt)
    else if Copy(gTxt, 1, 8) = '--saida=' then
      GSaida := Copy(gTxt, 9, MaxInt);
  end;
  if not (gTestes or gFalso) then
    gTestes := True;
  Preparar;
  if gTestes then
    Testes;
  if gFalso then
    SemFalsoPositivo;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('tudo certo')
  else
    WriteLn(GFalhas, ' falha(s)');
  if GFalhas > 0 then
    Halt(1);
end.
