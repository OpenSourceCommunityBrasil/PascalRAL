program copia;

{$mode ObjFPC}{$H+}

// Cópia de segurança do migrador (etapa M7).
//   copia --testes
// Trabalha numa pasta temporária; nunca toca em projeto de verdade.
//
// Compilar (da pasta do teste):
//   fpc -B -Fu../../src -Fu../../../src/classes copia.lpr

uses
  Classes, SysUtils,
  RALInst.Processo, RALMig.Copia;

const
  GitExe = {$IFDEF MSWINDOWS}'git.exe'{$ELSE}'git'{$ENDIF};

var
  GFalhas: integer = 0;
  GRaiz: string;

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

{ Helpers }

procedure Gravar(const ACaminho: string; const ABytes: RawByteString);
var
  vFluxo: TFileStream;
begin
  ForceDirectories(ExtractFilePath(ACaminho));
  vFluxo := TFileStream.Create(ACaminho, fmCreate);
  try
    if ABytes <> '' then
      vFluxo.WriteBuffer(ABytes[1], Length(ABytes));
  finally
    vFluxo.Free;
  end;
end;

function Ler(const ACaminho: string): RawByteString;
var
  vFluxo: TFileStream;
begin
  Result := '';
  vFluxo := TFileStream.Create(ACaminho, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, vFluxo.Size);
    if vFluxo.Size > 0 then
      vFluxo.ReadBuffer(Result[1], vFluxo.Size);
  finally
    vFluxo.Free;
  end;
end;

function Caminho(const ARel: string): string;
begin
  Result := GRaiz + StringReplace(ARel, '/', PathDelim, [rfReplaceAll]);
end;

procedure ApagarArvore(const APasta: string);
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vPasta + '*', faAnyFile, vBusca) = 0 then
  begin
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        ApagarArvore(vPasta + vBusca.Name)
      else
        DeleteFile(vPasta + vBusca.Name);
    until FindNext(vBusca) <> 0;
    FindClose(vBusca);
  end;
  RemoveDir(APasta);
end;

procedure NovaPasta(const ANome: string);
begin
  GRaiz := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-copia-' + ANome + PathDelim;
  ApagarArvore(ExcludeTrailingPathDelimiter(GRaiz));
  ForceDirectories(GRaiz);
end;

/// Bytes with every value, to prove the restore is byte by byte
function BytesTodos: RawByteString;
var
  vInt: integer;
begin
  SetLength(Result, 600);
  for vInt := 1 to 600 do
    Result[vInt] := Chr((vInt * 7) mod 256);
end;

{ Tests }

procedure TesteIdaEVolta;
var
  vArquivos, vMudados: TStringList;
  vZip, vErro: string;
  vOk: boolean;
  vOrigPas, vOrigDfm, vOrigBin: RawByteString;
begin
  WriteLn('copia, alteracao, desfazer');
  NovaPasta('ida');
  vOrigPas := 'unit U;'#13#10'// caf'#233#13#10'end.'#13#10;
  vOrigDfm := 'object F: TForm'#10'end'#10;
  vOrigBin := BytesTodos;
  Gravar(Caminho('U.pas'), vOrigPas);
  Gravar(Caminho('sub/F.dfm'), vOrigDfm);
  Gravar(Caminho('sub/dados.bin'), vOrigBin);
  Gravar(Caminho('intocado.txt'), 'nao esta na lista');

  vArquivos := TStringList.Create;
  vMudados := TStringList.Create;
  try
    vArquivos.Add('U.pas');
    vArquivos.Add('sub\F.dfm');
    vArquivos.Add('sub/dados.bin');
    vArquivos.Add('NovaUnit.pas');
    vArquivos.Add('novas/Outra.pas');
    vOk := CriarCopia(GRaiz, '1.1', '2.0', vArquivos, vZip, vErro);
    Conferir(vOk and (vErro = ''), 'CriarCopia: ' + vErro);
    Conferir(FileExists(vZip), 'o zip existe');
    Conferir(Pos(PASTA_COPIAS + PathDelim, vZip) > 0, 'dentro de ' + PASTA_COPIAS);
    Conferir(Pos('1.1-2.0-', ExtractFileName(vZip)) = 1, 'nome <de>-<para>-<data>: ' + ExtractFileName(vZip));

    // a "migracao": altera, apaga, cria
    Gravar(Caminho('U.pas'), 'unit U; // migrada'#13#10);
    DeleteFile(Caminho('sub/F.dfm'));
    Gravar(Caminho('sub/dados.bin'), 'x');
    Gravar(Caminho('NovaUnit.pas'), 'unit NovaUnit;');
    Gravar(Caminho('novas/Outra.pas'), 'unit Outra;');
    Conferir(RegistrarDepois(vZip, GRaiz, vErro), 'RegistrarDepois: ' + vErro);
    // o usuario mexe depois da migracao
    Gravar(Caminho('U.pas'), 'unit U; // migrada e editada'#13#10);

    vOk := Desfazer(vZip, GRaiz, vMudados, vErro);
    Conferir(vOk and (vErro = ''), 'Desfazer: ' + vErro);
    Conferir(Ler(Caminho('U.pas')) = vOrigPas, 'U.pas igual byte a byte (CRLF e ANSI)');
    Conferir(FileExists(Caminho('sub/F.dfm')) and (Ler(Caminho('sub/F.dfm')) = vOrigDfm),
             'arquivo apagado pela migracao voltou');
    Conferir(Ler(Caminho('sub/dados.bin')) = vOrigBin, 'binario igual byte a byte');
    Conferir(not FileExists(Caminho('NovaUnit.pas')), 'arquivo novo apagado');
    Conferir(not FileExists(Caminho('novas/Outra.pas')), 'arquivo novo em subpasta apagado');
    Conferir(not DirectoryExists(Caminho('novas')), 'a subpasta criada pela migracao sumiu');
    Conferir(Ler(Caminho('intocado.txt')) = 'nao esta na lista', 'o que nao estava na lista continua');
    Conferir(vMudados.Count = 1, 'um arquivo mudado depois da migracao: ' + vMudados.CommaText);
    Conferir(vMudados.IndexOf('U.pas') = 0, 'e e o U.pas');

    // desfazer de novo e idempotente
    Conferir(Desfazer(vZip, GRaiz, nil, vErro), 'Desfazer duas vezes: ' + vErro);
    Conferir(Ler(Caminho('U.pas')) = vOrigPas, 'continua igual');
  finally
    vMudados.Free;
    vArquivos.Free;
    ApagarArvore(ExcludeTrailingPathDelimiter(GRaiz));
  end;
end;

procedure TesteListar;
var
  vArquivos, vLista: TStringList;
  vZip1, vZip2, vZip3, vErro: string;
begin
  WriteLn('ListarCopias, mais nova primeiro');
  NovaPasta('lista');
  Gravar(Caminho('U.pas'), 'a');
  vArquivos := TStringList.Create;
  try
    vArquivos.Add('U.pas');
    vLista := ListarCopias(GRaiz);
    try
      Conferir(vLista.Count = 0, 'sem copias = lista vazia');
    finally
      vLista.Free;
    end;
    Conferir(CriarCopia(GRaiz, '1.0', '1.1', vArquivos, vZip1, vErro), 'copia 1');
    Conferir(CriarCopia(GRaiz, '1.1', '2.0', vArquivos, vZip2, vErro), 'copia 2');
    Conferir(CriarCopia(GRaiz, '1.1', '2.0', vArquivos, vZip3, vErro), 'copia 3 (mesmo segundo)');
    Conferir((vZip1 <> vZip2) and (vZip2 <> vZip3), 'nomes diferentes');
    vLista := ListarCopias(GRaiz);
    try
      Conferir(vLista.Count = 3, 'tres copias');
      if vLista.Count = 3 then
      begin
        Conferir(vLista[0] = vZip3, 'a mais nova primeiro');
        Conferir(vLista[2] = vZip1, 'a mais velha por ultimo');
      end;
    finally
      vLista.Free;
    end;
  finally
    vArquivos.Free;
    ApagarArvore(ExcludeTrailingPathDelimiter(GRaiz));
  end;
end;

procedure TesteRecusas;
var
  vArquivos, vMudados: TStringList;
  vZip, vErro: string;
begin
  WriteLn('recusas');
  NovaPasta('recusa');
  Gravar(Caminho('U.pas'), 'a');
  vArquivos := TStringList.Create;
  vMudados := TStringList.Create;
  try
    Conferir(not CriarCopia(GRaiz + 'nao-existe', '1.1', '2.0', vArquivos, vZip, vErro) and (vErro <> ''),
             'pasta que nao existe');
    Conferir(not CriarCopia(GRaiz, '1.1', '2.0', vArquivos, vZip, vErro) and (vErro <> ''),
             'lista vazia');
    vArquivos.Add('..\fora.pas');
    Conferir(not CriarCopia(GRaiz, '1.1', '2.0', vArquivos, vZip, vErro) and (vErro <> ''),
             'caminho que sai da pasta');
    Conferir(not DirectoryExists(GRaiz + PASTA_COPIAS) or (ListarCopias(GRaiz).Count = 0),
             'nenhum zip ficou para tras');
    Conferir(not Desfazer(GRaiz + 'nada.zip', GRaiz, vMudados, vErro) and (vErro <> ''),
             'desfazer de zip que nao existe');
    Gravar(GRaiz + 'lixo.zip', 'isto nao e um zip');
    Conferir(not Desfazer(GRaiz + 'lixo.zip', GRaiz, vMudados, vErro) and (vErro <> ''),
             'desfazer de arquivo que nao e zip');
  finally
    vMudados.Free;
    vArquivos.Free;
    ApagarArvore(ExcludeTrailingPathDelimiter(GRaiz));
  end;
end;

procedure TesteRelatorios;
var
  vArquivos, vRel, vMantidos: TStringList;
  vZip, vErro: string;
  vCwd: string;
begin
  WriteLn('relatorios e copia usada saem no desfazer');
  NovaPasta('relat');
  vCwd := GetCurrentDir;
  Gravar(Caminho('U.pas'), 'unit U;');
  vArquivos := TStringList.Create;
  vRel := TStringList.Create;
  vMantidos := TStringList.Create;
  try
    vArquivos.Add('U.pas');
    Conferir(CriarCopia(GRaiz, '1.1', '2.0', vArquivos, vZip, vErro), 'CriarCopia: ' + vErro);
    Gravar(Caminho('U.pas'), 'unit U; // migrada');
    Conferir(RegistrarDepois(vZip, GRaiz, vErro), 'RegistrarDepois: ' + vErro);
    Gravar(Caminho('MIGRACAO-RAL-1.1-2.0.md'), '# relatorio');
    Gravar(Caminho('MIGRACAO-RAL-1.1-2.0.html'), '<html/>');
    vRel.Add('MIGRACAO-RAL-1.1-2.0.md');
    vRel.Add('MIGRACAO-RAL-1.1-2.0.html');
    Conferir(RegistrarRelatorios(vZip, GRaiz, vRel, vErro), 'RegistrarRelatorios: ' + vErro);
    // o .html e editado depois: fica
    Gravar(Caminho('MIGRACAO-RAL-1.1-2.0.html'), '<html>editado</html>');
    ListarRelatoriosMantidos(vZip, GRaiz, vMantidos);
    Conferir((vMantidos.Count = 1) and (vMantidos[0] = 'MIGRACAO-RAL-1.1-2.0.html'),
             'o relatorio editado e listado como mantido: ' + vMantidos.CommaText);
    Conferir(Desfazer(vZip, GRaiz, nil, vErro), 'Desfazer: ' + vErro);
    Conferir(Ler(Caminho('U.pas')) = 'unit U;', 'U.pas voltou');
    Conferir(not FileExists(Caminho('MIGRACAO-RAL-1.1-2.0.md')), 'relatorio intacto removido');
    Conferir(FileExists(Caminho('MIGRACAO-RAL-1.1-2.0.html')), 'relatorio editado mantido');
    Conferir(not FileExists(Caminho('_manifesto.txt')) and
             not FileExists(IncludeTrailingPathDelimiter(vCwd) + '_manifesto.txt'),
             'o manifesto nao e extraido para o projeto');
    DescartarCopia(vZip, GRaiz);
    Conferir(not FileExists(vZip), 'DescartarCopia apaga o zip');
    Conferir(not DirectoryExists(Caminho(PASTA_COPIAS)), 'e a pasta de copias, vazia');
    // copia antiga (sem a secao de relatorios): Desfazer segue igual
    Conferir(CriarCopia(GRaiz, '1.1', '2.0', vArquivos, vZip, vErro), 'CriarCopia 2: ' + vErro);
    Conferir(RegistrarDepois(vZip, GRaiz, vErro), 'RegistrarDepois 2: ' + vErro);
    Conferir(Desfazer(vZip, GRaiz, nil, vErro), 'Desfazer sem secao de relatorios: ' + vErro);
  finally
    vMantidos.Free;
    vRel.Free;
    vArquivos.Free;
    ApagarArvore(ExcludeTrailingPathDelimiter(GRaiz));
  end;
end;

procedure TesteGit;
var
  vDetalhe: string;
  vExec: TExecucao;
begin
  WriteLn('git');
  NovaPasta('git');
  try
    // a pasta temporaria nao esta num repositorio: False, sem erro
    Conferir(not GitComMudancas(GRaiz, vDetalhe), 'pasta sem git = False');
    Conferir(not GitComMudancas(GRaiz + 'nao-existe', vDetalhe), 'pasta que nao existe = False');
    if ExeSearch(GitExe, GetEnvironmentVariable('PATH')) <> '' then
    begin
      vExec := TExecucao.Create;
      try
        vExec.Executavel := ExeSearch(GitExe, GetEnvironmentVariable('PATH'));
        vExec.PastaTrabalho := GRaiz;
        vExec.Parametros.Add('init');
        if vExec.Executar then
        begin
          Conferir(not GitComMudancas(GRaiz, vDetalhe), 'repositorio vazio e limpo = False');
          Gravar(Caminho('novo.txt'), 'x');
          Conferir(GitComMudancas(GRaiz, vDetalhe) and (Pos('novo.txt', vDetalhe) > 0),
                   'arquivo nao versionado = True, com o nome: ' + vDetalhe);
        end
        else
          WriteLn('  (git init falhou: caso do repositorio nao rodado)');
      finally
        vExec.Free;
      end;
    end
    else
      WriteLn('  (sem git no PATH: caso do repositorio nao rodado)');
  finally
    ApagarArvore(ExcludeTrailingPathDelimiter(GRaiz));
  end;
end;

begin
  if (ParamCount < 1) or (ParamStr(1) <> '--testes') then
  begin
    WriteLn('copia --testes');
    Halt(2);
  end;
  TesteIdaEVolta;
  TesteListar;
  TesteRecusas;
  TesteRelatorios;
  TesteGit;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('tudo certo')
  else
    WriteLn(GFalhas, ' falha(s)');
  if GFalhas > 0 then
    Halt(1);
end.
