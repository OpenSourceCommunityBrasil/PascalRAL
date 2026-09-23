program rodada;

{$mode ObjFPC}{$H+}

// F11: a rodada inteira (RALInst.Rodada), a mesma da CLI, sobre COPIAS:
//
//   rodada <pasta do Lazarus> --config=<copia da configuracao> --pasta=<pasta curta>
//          --recibos=<pasta> [--versao=<ref>] [--pacotes=a,b] [--desinstalar]
//
// Baixa a versao do RAL e as dependencias para --pasta, instala na copia da
// configuracao (sem reconstruir a IDE: o executavel e o de verdade) e grava o
// recibo em --recibos; com --desinstalar, desfaz em seguida.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, RALInst.Processo, RALInst.IDE, RALInst.IDE.Lazarus, RALInst.Rodada
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TSaida = class
  public
    procedure Linha(const ALinha: string);
  end;

procedure TSaida.Linha(const ALinha: string);
begin
  WriteLn(ALinha);
end;

function Opcao(const ANome: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 2 to ParamCount do
    if Copy(ParamStr(vInt), 1, Length(ANome) + 3) = '--' + ANome + '=' then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

var
  GSaida: TSaida;
  GBusca: TBuscaLazarus;
  GIDE: TIDEInstance;
  GRodada: TRodada;
  GOk: boolean;
  vInt: integer;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  if (ParamCount < 1) or (Opcao('config') = '') or (Opcao('pasta') = '') or (Opcao('recibos') = '') then
  begin
    WriteLn('uso: rodada <pasta do Lazarus> --config=<copia> --pasta=<pasta> --recibos=<pasta> ' +
            '[--versao=<ref>] [--pacotes=a,b] [--desinstalar]');
    Halt(2);
  end;
  GSaida := TSaida.Create;
  GBusca := TBuscaLazarus.Create;
  GRodada := TRodada.Create;
  try
    GIDE := GBusca.InspecionarPasta(ParamStr(1));
    if GIDE = nil then
    begin
      WriteLn('não é um Lazarus: ', ParamStr(1));
      Halt(1);
    end;
    // a copia: a configuracao de verdade nao e tocada
    GIDE.ConfigDir := IncludeTrailingPathDelimiter(Opcao('config'));
    WriteLn('IDE: ', GIDE.Nome, ' (FPC ', GIDE.VersaoCompilador, '), configuração ', GIDE.ConfigDir);

    GRodada.Log := @GSaida.Linha;
    GRodada.PastaBase := Opcao('pasta');
    GRodada.PastaRecibos := Opcao('recibos');
    GRodada.ExigirIDEFechada := False;
    GRodada.ConstruirIDE := False;
    if not GRodada.EscolherVersao(Opcao('versao')) or not GRodada.CarregarCatalogo then
    begin
      WriteLn('ERRO: ', GRodada.Erro);
      Halt(1);
    end;
    if Opcao('pacotes') <> '' then
      GRodada.Pacotes.CommaText := Opcao('pacotes');
    GRodada.AdicionarIDE(GIDE);

    WriteLn('== plano');
    WriteLn(GRodada.Plano);
    WriteLn('== execução');
    GOk := GRodada.Executar;
    WriteLn('== resumo');
    for vInt := 0 to Pred(GRodada.Relatorio.Count) do
      WriteLn('  ', GRodada.Relatorio[vInt]);
    WriteLn('execução: ', BoolToStr(GOk, 'ok', 'com erro'));

    if ParamStr(ParamCount) = '--desinstalar' then
    begin
      WriteLn('== desinstalação');
      if not GRodada.Desinstalar(False) then
        GOk := False;
      for vInt := 0 to Pred(GRodada.Relatorio.Count) do
        WriteLn('  ', GRodada.Relatorio[vInt]);
    end;
    Halt(Ord(not GOk));
  finally
    GRodada.Free;
    GBusca.Free;
    GSaida.Free;
  end;
end.
