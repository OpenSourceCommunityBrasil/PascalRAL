program desinstalar;

{$mode ObjFPC}{$H+}

// F10 pela linha de comando: os recibos das instalacoes e o caminho de volta.
//
//   desinstalar [--recibos=pasta] --listar [--todos]
//   desinstalar [--recibos=pasta] --desfazer=<recibo.json> [opcoes]
//   desinstalar [--recibos=pasta] --ide=<raiz da IDE> [opcoes]
//
//   --recibos=<pasta>       padrao: a pasta de dados do instalador
//   --todos                 lista tambem os recibos ja desfeitos
//   --sem-reconstruir       Lazarus: so a configuracao, sem --build-ide
//   --ignorar-ide-aberta    nao confere se a IDE esta aberta (testes sobre
//                           copias do registro e da configuracao)

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, RALInst.Processo, RALInst.Recibos
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

var
  GSaida: TSaida;
  GRecibos: TRecibos;
  GRecibo: TRecibo;
  GPasta, GParam, GDesfazer, GIDE: string;
  GListar, GTodos, GReconstruir, GConferirIDE, GOk: boolean;
  vInt: integer;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  GPasta := PastaDadosInstalador + 'recibos';
  GReconstruir := True;
  GConferirIDE := True;
  for vInt := 1 to ParamCount do
  begin
    GParam := ParamStr(vInt);
    if Copy(GParam, 1, 10) = '--recibos=' then
      GPasta := Copy(GParam, 11, MaxInt)
    else if GParam = '--listar' then
      GListar := True
    else if GParam = '--todos' then
      GTodos := True
    else if Copy(GParam, 1, 11) = '--desfazer=' then
      GDesfazer := Copy(GParam, 12, MaxInt)
    else if Copy(GParam, 1, 6) = '--ide=' then
      GIDE := Copy(GParam, 7, MaxInt)
    else if GParam = '--sem-reconstruir' then
      GReconstruir := False
    else if GParam = '--ignorar-ide-aberta' then
      GConferirIDE := False
    else
    begin
      WriteLn('opção desconhecida: ', GParam);
      Halt(2);
    end;
  end;

  GSaida := TSaida.Create;
  GRecibos := TRecibos.Create(True);
  try
    GRecibos.Carregar(GPasta, GTodos);
    if GListar then
    begin
      WriteLn(Format('%d recibo(s) em %s', [GRecibos.Count, GPasta]));
      for vInt := 0 to Pred(GRecibos.Count) do
      begin
        WriteLn('  ', GRecibos[vInt].Descricao);
        WriteLn('      ', GRecibos[vInt].Arquivo);
        WriteLn('      ', GRecibos[vInt].Pacotes.CommaText);
      end;
    end
    else if GDesfazer <> '' then
    begin
      GRecibo := nil;
      for vInt := 0 to Pred(GRecibos.Count) do
        if SameFileName(ExpandFileName(GRecibos[vInt].Arquivo), ExpandFileName(GDesfazer)) then
          GRecibo := GRecibos[vInt];
      if GRecibo = nil then
      begin
        WriteLn('recibo não encontrado (ou já desfeito) em ', GPasta, ': ', GDesfazer);
        Halt(1);
      end;
      GOk := DesfazerInstalacao(GRecibo, GRecibos, @GSaida.Linha, GReconstruir, GConferirIDE);
      Halt(Ord(not GOk));
    end
    else if GIDE <> '' then
    begin
      GOk := DesinstalarIDE(GPasta, GIDE, @GSaida.Linha, GReconstruir, GConferirIDE);
      Halt(Ord(not GOk));
    end
    else
    begin
      WriteLn('uso: desinstalar [--recibos=pasta] --listar [--todos]');
      WriteLn('     desinstalar [--recibos=pasta] --desfazer=<recibo.json> [--sem-reconstruir] [--ignorar-ide-aberta]');
      WriteLn('     desinstalar [--recibos=pasta] --ide=<raiz da IDE> [--sem-reconstruir] [--ignorar-ide-aberta]');
      Halt(2);
    end;
  finally
    GRecibos.Free;
    GSaida.Free;
  end;
end.
