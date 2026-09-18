program compilar_delphi;

{$mode ObjFPC}{$H+}

// Compila pacotes do RAL num Delphi da maquina, sem abrir a IDE.
//
//   compilar_delphi <raiz do RAL> [opcoes] [pacote ...]
//
//   --bds=23.0      qual Delphi (padrao: o mais novo achado)
//   --plataforma=win32|win64
//   --bpl=<pasta>   saida dos .bpl (padrao: <BDSCOMMONDIR>\Bpl)
//   --dcp=<pasta>   saida dos .dcp (padrao: <BDSCOMMONDIR>\Dcp)
//   --caminho=<p>   library path extra (pode repetir): mORMot2, Zeos...
//   --simular       monta e mostra os comandos, sem executar
//   --listar        so lista os Delphi encontrados
//
// Sem pacotes, compila o catalogo inteiro do Delphi, na ordem do grafo.
// Exemplo: compilar_delphi D:\...\PascalRAL-dev --bds=23.0 IndyRAL

uses
  Classes, SysUtils, StrUtils, RALInst.IDE, RALInst.IDE.Delphi, RALInst.Catalogo,
  RALInst.Build.Delphi;

type

  { TSaida }

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
  GIDEs: TIDEList;
  GBusca: TBuscaDelphi;
  GCatalogo: TCatalogo;
  GBuild: TBuildDelphi;
  GPacotes: TList;
  GNomes: TStringList;
  GIDE: TIDEInstance;
  GRaiz, GBDS, GPlataforma, GParam, GBpl, GDcp: string;
  GExtras: TStringList;
  GSimular, GListar: boolean;
  vInt, vPrimeiro: integer;
  vOk: boolean;

begin
  if ParamCount = 0 then
  begin
    WriteLn('uso: compilar_delphi <raiz do RAL> [--bds=23.0] [--plataforma=win32] ',
            '[--bpl=pasta] [--dcp=pasta] [--caminho=pasta] [--simular] [--listar] [pacote ...]');
    Halt(2);
  end;

  GRaiz := '';
  if Copy(ParamStr(1), 1, 2) <> '--' then
    GRaiz := ParamStr(1);
  GPlataforma := 'win32';
  GNomes := TStringList.Create;
  GExtras := TStringList.Create;
  GSimular := False;
  GListar := False;

  if GRaiz = '' then
    vPrimeiro := 1
  else
    vPrimeiro := 2;
  for vInt := vPrimeiro to ParamCount do
  begin
    GParam := ParamStr(vInt);
    if Copy(GParam, 1, 6) = '--bds=' then
      GBDS := Copy(GParam, 7, MaxInt)
    else if Copy(GParam, 1, 13) = '--plataforma=' then
      GPlataforma := Copy(GParam, 14, MaxInt)
    else if Copy(GParam, 1, 6) = '--bpl=' then
      GBpl := Copy(GParam, 7, MaxInt)
    else if Copy(GParam, 1, 6) = '--dcp=' then
      GDcp := Copy(GParam, 7, MaxInt)
    else if Copy(GParam, 1, 10) = '--caminho=' then
      GExtras.Add(Copy(GParam, 11, MaxInt))
    else if GParam = '--simular' then
      GSimular := True
    else if GParam = '--listar' then
      GListar := True
    else if Copy(GParam, 1, 2) = '--' then
    begin
      WriteLn('opção desconhecida: ', GParam);
      Halt(2);
    end
    else
      GNomes.Add(GParam);
  end;

  GSaida := TSaida.Create;
  GIDEs := TIDEList.Create(True);
  GBusca := TBuscaDelphi.Create;
  GCatalogo := TCatalogo.Create;
  GPacotes := TList.Create;
  try
    GBusca.BuscarPadrao(GIDEs);
    GBusca.Finalizar(GIDEs);

    if GListar then
    begin
      for vInt := 0 to Pred(GIDEs.Count) do
        WriteLn(Format('%-8s %-24s %s', [GIDEs[vInt].BDSVersao, GIDEs[vInt].Nome,
                                         GIDEs[vInt].RootDir]));
      Halt(0);
    end;

    // sem --bds, o mais novo (a lista ja vem ordenada da versao maior para a menor)
    GIDE := nil;
    for vInt := 0 to Pred(GIDEs.Count) do
      if (GIDEs[vInt].Tipo = tiDelphi) and
         ((GBDS = '') or SameText(GIDEs[vInt].BDSVersao, GBDS)) then
      begin
        GIDE := GIDEs[vInt];
        Break;
      end;

    if GIDE = nil then
    begin
      WriteLn('Delphi não encontrado', IfThen(GBDS <> '', ' para BDS ' + GBDS, ''));
      Halt(1);
    end;

    WriteLn('IDE:     ', GIDE.Nome, '  (', GIDE.RootDir, ')');
    WriteLn('sufixo:  ', GIDE.SufixoPacote);
    WriteLn('fontes:  ', GRaiz);
    WriteLn;

    if not GCatalogo.Carregar(GRaiz) then
    begin
      WriteLn('nenhum pacote em ', GRaiz);
      Halt(1);
    end;

    if GNomes.Count = 0 then
      GCatalogo.Listar(tpDelphi, GPacotes)
    else
      GCatalogo.Fechamento(tpDelphi, GNomes, GPacotes);

    if GNomes.Count = 0 then
      WriteLn(GCatalogo.Plano(tpDelphi))
    else
      WriteLn(GCatalogo.Plano(tpDelphi, GNomes));
    WriteLn;

    GBuild := TBuildDelphi.Create(GIDE, GCatalogo);
    try
      GBuild.Plataforma := GPlataforma;
      GBuild.Log := @GSaida.Linha;
      GBuild.Simular := GSimular;
      GBuild.CaminhosExtras.AddStrings(GExtras);
      if GBpl <> '' then
        GBuild.PastaBpl := GBpl;
      if GDcp <> '' then
        GBuild.PastaDcp := GDcp;

      vOk := GBuild.Compilar(GPacotes);
      WriteLn;
      WriteLn(GBuild.Relatorio);
      if not vOk then
        Halt(1);
    finally
      GBuild.Free;
    end;
  finally
    GPacotes.Free;
    GCatalogo.Free;
    GBusca.Free;
    GIDEs.Free;
    GNomes.Free;
    GExtras.Free;
    GSaida.Free;
  end;
end.
