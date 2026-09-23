program instalar_lazarus;

{$mode ObjFPC}{$H+}

// Instala pacotes do RAL num Lazarus — a mesma rodada que o instalador
// grafico faz: --add-package-link, --add-package e um --build-ide no fim.
//
//   instalar_lazarus <raiz do RAL> <pasta do Lazarus> [opcoes] [pacote ...]
//
//   --config-teste   trabalha numa COPIA da configuracao (primary config
//                    path) daquele Lazarus, numa pasta temporaria; a
//                    configuracao de verdade nao e tocada. Implica --sem-build
//                    (reconstruir a IDE grava o executavel na pasta dela)
//   --sem-build      registra e marca os pacotes, sem --build-ide
//   --recibos=<pasta> onde gravar o recibo (padrao: a pasta de dados)
//   --config=<pasta> usa esta configuracao (uma copia preparada para o teste);
//                    implica --sem-build
//   --plano          so mostra o plano
//   --simular        mostra os comandos, sem executar
//
// Sem pacotes, instala pascalraldsgn (e o que ele exige).

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, RALInst.IDE, RALInst.IDE.Lazarus, RALInst.Catalogo,
  RALInst.Instalar.Lazarus, RALInst.Receitas, RALInst.Dependencias, RALInst.Compatibilidade
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

procedure CopiarPasta(const AOrigem, ADestino: string);
var
  vBusca: TSearchRec;
  vDe, vPara: string;
  vLer, vGravar: TFileStream;
begin
  ForceDirectories(ADestino);
  vDe := IncludeTrailingPathDelimiter(AOrigem);
  vPara := IncludeTrailingPathDelimiter(ADestino);
  if FindFirst(vDe + AllFilesMask, faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        CopiarPasta(vDe + vBusca.Name, vPara + vBusca.Name)
      else
      begin
        vLer := TFileStream.Create(vDe + vBusca.Name, fmOpenRead or fmShareDenyNone);
        try
          vGravar := TFileStream.Create(vPara + vBusca.Name, fmCreate);
          try
            vGravar.CopyFrom(vLer, 0);
          finally
            vGravar.Free;
          end;
        finally
          vLer.Free;
        end;
      end;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

procedure ApagarPasta(const APasta: string);
var
  vBusca: TSearchRec;
  vDe: string;
begin
  vDe := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vDe + AllFilesMask, faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        ApagarPasta(vDe + vBusca.Name)
      else
        SysUtils.DeleteFile(vDe + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
  RemoveDir(APasta);
end;

var
  GSaida: TSaida;
  GIDEs: TIDEList;
  GBusca: TBuscaLazarus;
  GCatalogo: TCatalogo;
  GInst: TInstalacaoLazarus;
  GIDE: TIDEInstance;
  GNomes: TStringList;
  GRaiz, GLazarus, GParam, GCopia, GConfig, GRecibos: string;
  GConfigTeste, GSemBuild, GPlano, GSimular, GOk: boolean;
  GReceitas: TReceitas;
  GReceita: TReceita;
  GExigidas: TStringList;
  GBaixa: TBaixaDependencia;
  GManifesto: TManifesto;
  GArqManifesto, GVersaoDep, GMotivoDep: string;
  GPastaReceitas, GPastaDeps: string;
  GIgnorar: boolean;
  vInt: integer;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  if (ParamCount < 2) or (Copy(ParamStr(1), 1, 2) = '--') then
  begin
    WriteLn('uso: instalar_lazarus <raiz do RAL> <pasta do Lazarus> [--config-teste] ',
            '[--sem-build] [--plano] [--simular] [pacote ...]');
    Halt(2);
  end;

  GRaiz := ParamStr(1);
  GLazarus := ParamStr(2);
  GNomes := TStringList.Create;
  GConfigTeste := False;
  GSemBuild := False;
  GPlano := False;
  GSimular := False;
  for vInt := 3 to ParamCount do
  begin
    GParam := ParamStr(vInt);
    if GParam = '--config-teste' then
      GConfigTeste := True
    else if GParam = '--sem-build' then
      GSemBuild := True
    else if GParam = '--plano' then
      GPlano := True
    else if GParam = '--simular' then
      GSimular := True
    else if Copy(GParam, 1, 11) = '--receitas=' then
      GPastaReceitas := Copy(GParam, 12, MaxInt)
    else if Copy(GParam, 1, 12) = '--manifesto=' then
      GArqManifesto := Copy(GParam, 13, MaxInt)
    else if Copy(GParam, 1, 10) = '--recibos=' then
      GRecibos := Copy(GParam, 11, MaxInt)
    else if Copy(GParam, 1, 9) = '--config=' then
    begin
      // uma configuracao preparada para o teste; implica --sem-build
      GConfig := Copy(GParam, 10, MaxInt);
      GSemBuild := True;
    end
    else if Copy(GParam, 1, 7) = '--deps=' then
      GPastaDeps := Copy(GParam, 8, MaxInt)
    else if GParam = '--ignorar-existentes' then
      GIgnorar := True
    else if Copy(GParam, 1, 2) = '--' then
    begin
      WriteLn('opção desconhecida: ', GParam);
      Halt(2);
    end
    else
      GNomes.Add(GParam);
  end;
  if GNomes.Count = 0 then
    GNomes.Add('pascalraldsgn');

  GSaida := TSaida.Create;
  GIDEs := TIDEList.Create(True);
  GBusca := TBuscaLazarus.Create;
  GCatalogo := TCatalogo.Create;
  GReceitas := TReceitas.Create;
  try
    GIDE := GBusca.InspecionarPasta(GLazarus);
    if GIDE = nil then
    begin
      WriteLn('não é uma instalação de Lazarus: ', GLazarus);
      Halt(1);
    end;
    GIDEs.Adicionar(GIDE);

    if not GCatalogo.Carregar(GRaiz) then
    begin
      WriteLn('nenhum pacote em ', GRaiz);
      Halt(1);
    end;

    if GConfig <> '' then
    begin
      WriteLn('configuração: ', GConfig);
      GIDE.ConfigDir := IncludeTrailingPathDelimiter(GConfig);
    end;

    if GConfigTeste then
    begin
      GCopia := GetTempDir + 'ralinstaller-teste-pcp';
      ApagarPasta(GCopia);
      if (GIDE.ConfigDir <> '') and DirectoryExists(GIDE.ConfigDir) then
        CopiarPasta(GIDE.ConfigDir, GCopia)
      else
        ForceDirectories(GCopia);
      WriteLn('configuração: cópia de ', GIDE.ConfigDir, ' em ', GCopia);
      GIDE.ConfigDir := IncludeTrailingPathDelimiter(GCopia);
      GSemBuild := True;
    end;

    GInst := TInstalacaoLazarus.Create(GIDE, GCatalogo);
    try
      GInst.Pacotes.AddStrings(GNomes);
      GInst.ConstruirIDE := not GSemBuild;
      if GRecibos <> '' then
        GInst.PastaRecibos := GRecibos;
      // numa copia da configuracao, a IDE aberta nao atrapalha
      if (GConfig <> '') or GConfigTeste then
        GInst.ExigirIDEFechada := False;
      GInst.Simular := GSimular;
      GInst.Log := @GSaida.Linha;

      // F7: receitas e, com --deps=, o download do que a IDE nao tem
      if GPastaReceitas <> '' then
      begin
        GReceitas.CarregarPasta(GPastaReceitas);
        for vInt := 0 to Pred(GReceitas.Erros.Count) do
          WriteLn('ERRO: ', GReceitas.Erros[vInt]);
        GInst.Receitas := GReceitas;
        GInst.IgnorarExistentes := GIgnorar;
      end;

      // F6: o manifesto da versao do RAL; sem ele, o do repositorio do instalador
      GManifesto := TManifesto.Create;
      GManifesto.CarregarPadrao(GCatalogo.Origem);
      if GManifesto.Origem = '' then
      begin
        if GArqManifesto = '' then
          GArqManifesto := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' +
                                          PathDelim + 'manifesto' + PathDelim + 'ral.json');
        GManifesto.CarregarArquivo(GArqManifesto);
      end;
      for vInt := 0 to Pred(GManifesto.Erros.Count) do
        WriteLn('ERRO: ', GManifesto.Erros[vInt]);
      WriteLn('manifesto: ', GManifesto.Origem);
      GInst.Manifesto := GManifesto;
      if GPastaDeps <> '' then
      begin
        GExigidas := TStringList.Create;
        try
          GReceitas.Exigidas(GCatalogo, tpLazarus, GNomes, GExigidas);
          for vInt := 0 to Pred(GExigidas.Count) do
          begin
            GReceita := TReceita(GExigidas.Objects[vInt]);
            if not GReceita.PodeBaixar or not GReceita.Lazarus.Existe or
               (not GIgnorar and (GInst.DependenciaInstalada(GReceita) <> '')) then
              Continue;
            // F6: a versao que este FPC pede (o Zeos 8.0-patches no 3.3)
            GVersaoDep := GInst.VersaoDependencia(GReceita, GMotivoDep);
            if GMotivoDep <> '' then
            begin
              WriteLn('não baixa ', GReceita.Nome, ': ', GMotivoDep);
              Continue;
            end;
            GBaixa := TBaixaDependencia.Create(GReceita);
            try
              GBaixa.PastaBase := GPastaDeps;
              GBaixa.Log := @GSaida.Linha;
              GBaixa.VersaoPedida := GVersaoDep;
              if GPlano then
              begin
                if GBaixa.ResolverVersao then
                  GInst.PastasDependencias.Values[ChaveDependencia(GReceita.Nome, GVersaoDep)] :=
                    GBaixa.PastaDestino;
              end
              else if GBaixa.Executar then
                GInst.PastasDependencias.Values[ChaveDependencia(GReceita.Nome, GVersaoDep)] :=
                  GBaixa.PastaDestino
              else
                WriteLn('ERRO: ', GBaixa.Erro);
            finally
              GBaixa.Free;
            end;
          end;
        finally
          GExigidas.Free;
        end;
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
    GReceitas.Free;
    GCatalogo.Free;
    GBusca.Free;
    GIDEs.Free;
    GNomes.Free;
    GSaida.Free;
  end;
end.
