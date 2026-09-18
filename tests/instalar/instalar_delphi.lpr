program instalar_delphi;

{$mode ObjFPC}{$H+}

// Instala pacotes do RAL num Delphi da maquina — compila, escreve o registro
// e grava o recibo —, a mesma rodada que o instalador grafico faz.
//
//   instalar_delphi <raiz do RAL> [opcoes] [pacote ...]
//
//   --bds=23.0        qual Delphi (padrao: o mais novo achado)
//   --win64           tambem compila o runtime e poe o library path de Win64
//   --somente-paths   so library path e variavel, sem compilar nem instalar
//   --bpl=<pasta>     saida dos .bpl (padrao: a que a IDE usa)
//   --dcp=<pasta>     saida dos .dcp (padrao: a que a IDE usa)
//   --caminho=<p>     library path extra para compilar (mORMot2, Zeos...)
//   --chave-teste     trabalha numa COPIA do registro da IDE
//                     (HKCU\Software\RALInstaller-Teste\BDS\<ver>), sem tocar
//                     na IDE de verdade; a copia e refeita a cada rodada
//   --reusar-chave    como --chave-teste, mas usa a copia que ja esta la
//   --plano           so mostra o plano
//   --simular         monta tudo e diz o que escreveria, sem escrever
//   --desfazer=<recibo> volta o registro ao que era antes da rodada daquele
//                     recibo (os .bpl gravados ficam); dispensa a raiz
//
// Sem pacotes, instala PascalRALDsgn (e o que ele exige).
// Exemplo: instalar_delphi D:\...\PascalRAL-dev --bds=23.0 --chave-teste
//            --bpl=D:\temp\bpl --dcp=D:\temp\dcp IndyRAL

uses
  Classes, SysUtils, StrUtils, Registry, Windows,
  RALInst.IDE, RALInst.IDE.Delphi, RALInst.Catalogo, RALInst.Registro.Delphi,
  RALInst.Instalar.Delphi;

type
  TSaida = class
  public
    procedure Linha(const ALinha: string);
  end;

procedure TSaida.Linha(const ALinha: string);
begin
  WriteLn(ALinha);
end;

procedure ApagarChave(const AChave: string);
var
  vReg: TRegistry;
  vSub: TStringList;
  vInt: integer;
begin
  vReg := TRegistry.Create(KEY_ALL_ACCESS);
  vSub := TStringList.Create;
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    if not vReg.OpenKey(AChave, False) then
      Exit;
    vReg.GetKeyNames(vSub);
    vReg.CloseKey;
    for vInt := 0 to Pred(vSub.Count) do
      ApagarChave(AChave + '\' + vSub[vInt]);
    vReg.DeleteKey(AChave);
  finally
    vSub.Free;
    vReg.Free;
  end;
end;

var
  GSaida: TSaida;
  GIDEs: TIDEList;
  GBusca: TBuscaDelphi;
  GCatalogo: TCatalogo;
  GInst: TInstalacaoDelphi;
  GIDE: TIDEInstance;
  GNomes, GExtras: TStringList;
  GRaiz, GBDS, GParam, GBpl, GDcp, GChaveTeste: string;
  GWin64, GSomentePaths, GChave, GReusar, GPlano, GSimular, GOk: boolean;
  vInt: integer;

begin
  SetConsoleOutputCP(CP_UTF8);
  if Copy(ParamStr(1), 1, 11) = '--desfazer=' then
  begin
    GSaida := TSaida.Create;
    WriteLn('desfazendo ', Copy(ParamStr(1), 12, MaxInt));
    if DesfazerRecibo(Copy(ParamStr(1), 12, MaxInt), @GSaida.Linha) then
      Halt(0)
    else
      Halt(1);
  end;

  if (ParamCount = 0) or (Copy(ParamStr(1), 1, 2) = '--') then
  begin
    WriteLn('uso: instalar_delphi <raiz do RAL> [--bds=23.0] [--win64] [--somente-paths] ',
            '[--bpl=pasta] [--dcp=pasta] [--caminho=pasta] [--chave-teste] [--plano] ',
            '[--simular] [pacote ...]');
    WriteLn('     instalar_delphi --desfazer=<recibo.json>');
    Halt(2);
  end;

  GRaiz := ParamStr(1);
  GNomes := TStringList.Create;
  GExtras := TStringList.Create;
  GWin64 := False;
  GSomentePaths := False;
  GChave := False;
  GReusar := False;
  GPlano := False;
  GSimular := False;

  for vInt := 2 to ParamCount do
  begin
    GParam := ParamStr(vInt);
    if Copy(GParam, 1, 6) = '--bds=' then
      GBDS := Copy(GParam, 7, MaxInt)
    else if GParam = '--win64' then
      GWin64 := True
    else if GParam = '--somente-paths' then
      GSomentePaths := True
    else if Copy(GParam, 1, 6) = '--bpl=' then
      GBpl := Copy(GParam, 7, MaxInt)
    else if Copy(GParam, 1, 6) = '--dcp=' then
      GDcp := Copy(GParam, 7, MaxInt)
    else if Copy(GParam, 1, 10) = '--caminho=' then
      GExtras.Add(Copy(GParam, 11, MaxInt))
    else if GParam = '--chave-teste' then
      GChave := True
    else if GParam = '--reusar-chave' then
    begin
      GChave := True;
      GReusar := True;
    end
    else if GParam = '--plano' then
      GPlano := True
    else if GParam = '--simular' then
      GSimular := True
    else if Copy(GParam, 1, 2) = '--' then
    begin
      WriteLn('opção desconhecida: ', GParam);
      Halt(2);
    end
    else
      GNomes.Add(GParam);
  end;
  if GNomes.Count = 0 then
    GNomes.Add('PascalRALDsgn');

  GSaida := TSaida.Create;
  GIDEs := TIDEList.Create(True);
  GBusca := TBuscaDelphi.Create;
  GCatalogo := TCatalogo.Create;
  try
    GBusca.BuscarPadrao(GIDEs);
    GBusca.Finalizar(GIDEs);

    GIDE := nil;
    for vInt := 0 to Pred(GIDEs.Count) do
      if (GIDEs[vInt].Tipo = tiDelphi) and (GIDEs[vInt].RegKey <> '') and
         ((GBDS = '') or SameText(GIDEs[vInt].BDSVersao, GBDS)) then
      begin
        GIDE := GIDEs[vInt];
        Break;
      end;
    if GIDE = nil then
    begin
      WriteLn('Delphi registrado não encontrado', IfThen(GBDS <> '', ' para BDS ' + GBDS, ''));
      Halt(1);
    end;

    if not GCatalogo.Carregar(GRaiz) then
    begin
      WriteLn('nenhum pacote em ', GRaiz);
      Halt(1);
    end;

    GInst := TInstalacaoDelphi.Create(GIDE, GCatalogo);
    try
      GInst.Pacotes.AddStrings(GNomes);
      if GWin64 then
        GInst.Plataformas.Add('win64');
      GInst.SomenteLibraryPath := GSomentePaths;
      GInst.CaminhosExtras.AddStrings(GExtras);
      GInst.PastaBpl := GBpl;
      GInst.PastaDcp := GDcp;
      GInst.Simular := GSimular;
      GInst.Log := @GSaida.Linha;

      if GChave then
      begin
        GChaveTeste := '\Software\RALInstaller-Teste\BDS\' + GIDE.BDSVersao;
        if not GReusar then
          ApagarChave(GChaveTeste);
        if not GReusar and not CopiarChave(GIDE.RegKey, GChaveTeste) then
        begin
          WriteLn('não foi possível copiar HKCU', GIDE.RegKey, ' para HKCU', GChaveTeste);
          Halt(1);
        end;
        WriteLn('registro: cópia em HKCU', GChaveTeste, ' (a IDE não é tocada)');
        GInst.ChaveRegistro := GChaveTeste;
        GInst.ExigirIDEFechada := False;
        GInst.PastaRecibos := GetTempDir + 'ralinstaller-teste-recibos';
      end;

      WriteLn(GInst.Plano);
      if GPlano then
        Halt(0);

      GOk := GInst.Executar;
      WriteLn;
      WriteLn(GInst.Relatorio.Text);
      if not GOk then
        Halt(1);
    finally
      GInst.Free;
    end;
  finally
    GCatalogo.Free;
    GBusca.Free;
    GIDEs.Free;
    GExtras.Free;
    GNomes.Free;
    GSaida.Free;
  end;
end.
