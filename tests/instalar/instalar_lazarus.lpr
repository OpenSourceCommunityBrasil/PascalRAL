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
//   --plano          so mostra o plano
//   --simular        mostra os comandos, sem executar
//
// Sem pacotes, instala pascalraldsgn (e o que ele exige).

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, RALInst.IDE, RALInst.IDE.Lazarus, RALInst.Catalogo,
  RALInst.Instalar.Lazarus
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
  GRaiz, GLazarus, GParam, GCopia: string;
  GConfigTeste, GSemBuild, GPlano, GSimular, GOk: boolean;
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
      GInst.Simular := GSimular;
      GInst.Log := @GSaida.Linha;

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
    GNomes.Free;
    GSaida.Free;
  end;
end.
