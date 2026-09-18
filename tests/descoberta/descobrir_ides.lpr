program descobrir_ides;

{$mode ObjFPC}{$H+}

// Lista as IDEs que a descoberta encontra nesta maquina, sem GUI.
//   descobrir_ides                 busca padrao (registro + raizes conhecidas)
//   descobrir_ides <pasta> [...]   busca padrao + estas pastas

uses
  Classes, SysUtils, RALInst.IDE, RALInst.IDE.Lazarus
  {$IFDEF MSWINDOWS}, RALInst.IDE.Delphi{$ENDIF};

procedure Mostrar(ALista: TIDEList);
var
  vInt, vAviso: integer;
  vIDE: TIDEInstance;
begin
  for vInt := 0 to Pred(ALista.Count) do
  begin
    vIDE := ALista[vInt];
    WriteLn(vIDE.Nome, '  (', NomeOrigem(vIDE.Origem), ')');
    WriteLn('  raiz ........ ', vIDE.RootDir);
    WriteLn('  versao ...... ', vIDE.Versao, ' ', vIDE.VersaoCompilador);
    WriteLn('  build ....... ', vIDE.BuildFile);
    WriteLn('  ide ......... ', vIDE.ExeFile);
    if vIDE.Tipo = tiDelphi then
    begin
      WriteLn('  registro .... HKCU', vIDE.RegKey);
      WriteLn('  sufixo ...... ', vIDE.SufixoPacote);
      WriteLn('  common ...... ', vIDE.CommonDir);
    end
    else
    begin
      WriteLn('  config ...... ', vIDE.ConfigDir, '  [', vIDE.ConfigOrigem, ']');
      WriteLn('  fpc ......... ', vIDE.CompilerFile);
    end;
    WriteLn('  plataformas . ', vIDE.Plataformas.CommaText);
    WriteLn('  capacidades . ',
      BoolToStr(ciCompilar in vIDE.Capacidades, 'compilar ', ''),
      BoolToStr(ciInstalarNaIDE in vIDE.Capacidades, 'instalar ', ''),
      BoolToStr(ciLibraryPath in vIDE.Capacidades, 'librarypath', ''));
    for vAviso := 0 to Pred(vIDE.Avisos.Count) do
      WriteLn('  AVISO: ', vIDE.Avisos[vAviso]);
    WriteLn;
  end;
end;

procedure Rodar(ABusca: TBuscaIDE; ALista: TIDEList);
var
  vInt: integer;
begin
  for vInt := 1 to ParamCount do
    ABusca.RaizesExtras.Add(ParamStr(vInt));
  ABusca.BuscarPadrao(ALista);
  ABusca.Finalizar(ALista);
end;

var
  vLista: TIDEList;
  vBusca: TBuscaIDE;
  vInicio: QWord;
begin
  vLista := TIDEList.Create(True);
  try
    vInicio := GetTickCount64;
    {$IFDEF MSWINDOWS}
      vBusca := TBuscaDelphi.Create;
      try
        Rodar(vBusca, vLista);
      finally
        vBusca.Free;
      end;
    {$ENDIF}
    vBusca := TBuscaLazarus.Create;
    try
      Rodar(vBusca, vLista);
    finally
      vBusca.Free;
    end;
    Mostrar(vLista);
    WriteLn(vLista.Count, ' instalacoes em ', GetTickCount64 - vInicio, ' ms');
  finally
    vLista.Free;
  end;
end.
