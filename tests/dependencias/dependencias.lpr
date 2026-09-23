program dependencias;

{$mode ObjFPC}{$H+}

// F7 pela linha de comando.
//
//   dependencias --receitas=<pasta> --listar
//   dependencias --receitas=<pasta> --exigidas=<raiz do RAL> [delphi|lazarus] [pacote ...]
//   dependencias --receitas=<pasta> --baixar=<nome> --pasta=<pasta de instalacao>
//
// Sem --receitas, usa a pasta receitas/ do repositorio do instalador.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, RALInst.Catalogo, RALInst.Receitas, RALInst.Dependencias
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TSaida = class
  public
    FUltimo: int64;
    procedure Linha(const ALinha: string);
    procedure Progresso(const ALidos, ATotal: int64);
  end;

procedure TSaida.Linha(const ALinha: string);
begin
  WriteLn(ALinha);
end;

procedure TSaida.Progresso(const ALidos, ATotal: int64);
begin
  if ALidos - FUltimo >= 4 * 1024 * 1024 then
  begin
    FUltimo := ALidos;
    WriteLn(Format('    %.0f MB', [ALidos / 1048576]));
  end;
end;

function NomesAcoes(ABloco: TBlocoIDE): string;
const
  Nomes: array[TTipoAcao] of string = ('variavel', 'libpath', 'dpk', 'lpk');
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to Pred(ABloco.Acoes.Count) do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Nomes[ABloco.Acao(vInt).Tipo];
  end;
end;

var
  GSaida: TSaida;
  GReceitas: TReceitas;
  GCatalogo: TCatalogo;
  GBaixa: TBaixaDependencia;
  GNomes, GExigidas: TStringList;
  GParam, GPasta, GPastaReceitas, GAcao, GAlvo: string;
  GTipo: TTipoPacote;
  GReceita: TReceita;
  vInt: integer;
  vT0: QWord;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  GSaida := TSaida.Create;
  GReceitas := TReceitas.Create;
  GCatalogo := TCatalogo.Create;
  GNomes := TStringList.Create;
  GExigidas := TStringList.Create;
  try
    GPastaReceitas := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim +
                                     '..' + PathDelim + 'receitas');
    GTipo := tpDelphi;
    for vInt := 1 to ParamCount do
    begin
      GParam := ParamStr(vInt);
      if Copy(GParam, 1, 11) = '--receitas=' then
        GPastaReceitas := Copy(GParam, 12, MaxInt)
      else if GParam = '--listar' then
        GAcao := 'listar'
      else if Copy(GParam, 1, 11) = '--exigidas=' then
      begin
        GAcao := 'exigidas';
        GAlvo := Copy(GParam, 12, MaxInt);
      end
      else if Copy(GParam, 1, 9) = '--baixar=' then
      begin
        GAcao := 'baixar';
        GAlvo := Copy(GParam, 10, MaxInt);
      end
      else if Copy(GParam, 1, 8) = '--pasta=' then
        GPasta := Copy(GParam, 9, MaxInt)
      else if SameText(GParam, 'lazarus') then
        GTipo := tpLazarus
      else if SameText(GParam, 'delphi') then
        GTipo := tpDelphi
      else
        GNomes.Add(GParam);
    end;

    GReceitas.CarregarPasta(GPastaReceitas);
    for vInt := 0 to Pred(GReceitas.Erros.Count) do
      WriteLn('ERRO: ', GReceitas.Erros[vInt]);

    if GAcao = 'listar' then
    begin
      WriteLn(Format('%d receita(s) em %s', [GReceitas.Count, GPastaReceitas]));
      for vInt := 0 to Pred(GReceitas.Count) do
      begin
        GReceita := GReceitas[vInt];
        WriteLn('  ', GReceita.Nome, '  —  ', GReceita.Descricao);
        if GReceita.Pago then
          WriteLn('    comercial: só detecção (', GReceita.Site, ')')
        else
          WriteLn('    fonte: github ', GReceita.Github, ' @ ', GReceita.Versao);
        if GReceita.Delphi.Existe then
          WriteLn('    Delphi:  fornece ', GReceita.Delphi.FornecePacotes.CommaText,
                  ' ', GReceita.Delphi.ForneceVariaveis.CommaText,
                  '; detecta ', GReceita.Delphi.Deteccao.CommaText,
                  '; ações ', NomesAcoes(GReceita.Delphi));
        if GReceita.Lazarus.Existe then
          WriteLn('    Lazarus: fornece ', GReceita.Lazarus.FornecePacotes.CommaText,
                  '; detecta ', GReceita.Lazarus.Deteccao.CommaText,
                  '; ações ', NomesAcoes(GReceita.Lazarus));
        if GReceita.PacotesRAL.Count > 0 then
          WriteLn('    pacotes do RAL: ', GReceita.PacotesRAL.CommaText);
      end;
    end
    else if GAcao = 'exigidas' then
    begin
      if not GCatalogo.Carregar(GAlvo) then
      begin
        WriteLn('nenhum pacote em ', GAlvo);
        Halt(1);
      end;
      if GNomes.Count = 0 then
        for vInt := 0 to Pred(GCatalogo.Count) do
          if GCatalogo[vInt].Tipo = GTipo then
            GNomes.Add(GCatalogo[vInt].Nome);
      GReceitas.Exigidas(GCatalogo, GTipo, GNomes, GExigidas);
      WriteLn(NomeTipoPacote(GTipo), ': ', GExigidas.Count, ' dependência(s)');
      for vInt := 0 to Pred(GExigidas.Count) do
        WriteLn('  ', TReceita(GExigidas.Objects[vInt]).Nome, '  <- ', GExigidas[vInt]);
    end
    else if GAcao = 'baixar' then
    begin
      GReceita := GReceitas.Buscar(GAlvo);
      if (GReceita = nil) or (GPasta = '') then
      begin
        WriteLn('receita desconhecida ou falta --pasta=');
        Halt(2);
      end;
      GBaixa := TBaixaDependencia.Create(GReceita);
      try
        GBaixa.PastaBase := GPasta;
        GBaixa.Log := @GSaida.Linha;
        GBaixa.Repo.OnProgresso := @GSaida.Progresso;
        vT0 := GetTickCount64;
        if not GBaixa.ResolverVersao then
        begin
          WriteLn('ERRO: ', GBaixa.Erro);
          Halt(1);
        end;
        WriteLn(GReceita.Nome, ': versão ', GBaixa.Ref);
        if not GBaixa.Executar then
        begin
          WriteLn('ERRO: ', GBaixa.Erro);
          Halt(1);
        end;
        WriteLn(Format('pronto em %s (%d s)', [GBaixa.PastaDestino, (GetTickCount64 - vT0) div 1000]));
      finally
        GBaixa.Free;
      end;
    end
    else
    begin
      WriteLn('uso: dependencias [--receitas=pasta] --listar');
      WriteLn('     dependencias --exigidas=<raiz do RAL> [delphi|lazarus] [pacote ...]');
      WriteLn('     dependencias --baixar=<nome> --pasta=<pasta de instalacao>');
      Halt(2);
    end;
  finally
    GExigidas.Free;
    GNomes.Free;
    GCatalogo.Free;
    GReceitas.Free;
    GSaida.Free;
  end;
end.
