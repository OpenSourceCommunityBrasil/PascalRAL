program versoes_ral;

{$mode ObjFPC}{$H+}

// F8 pela linha de comando: as versoes do RAL no GitHub, o catalogo de uma
// versao sem baixa-la para pasta nenhuma, e o download para uma pasta.
//
//   versoes_ral --listar
//   versoes_ral --catalogo=<ref> [delphi|lazarus] [pacote ...]
//   versoes_ral --baixar=<ref> --pasta=<pasta> [delphi|lazarus] [pacote ...]
//
// <ref> pode ser 'estavel' (a estavel mais recente). Com --baixar, os
// submodulos baixados sao os que os pacotes pedidos usam (sem pacotes: os do
// catalogo inteiro do tipo).

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, RALInst.Catalogo, RALInst.GitHub, RALInst.Zip, RALInst.Fontes
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
  // uma linha por MB
  if ALidos - FUltimo >= 1024 * 1024 then
  begin
    FUltimo := ALidos;
    if ATotal > 0 then
      WriteLn(Format('    %.1f de %.1f MB', [ALidos / 1048576, ATotal / 1048576]))
    else
      WriteLn(Format('    %.1f MB', [ALidos / 1048576]));
  end;
end;

var
  GSaida: TSaida;
  GRepo: TRepoGitHub;
  GVersoes: TVersoesRAL;
  GCatalogo: TCatalogo;
  GPreparo: TPreparoFontes;
  GNomes: TStringList;
  GLista: TList;
  GTipo: TTipoPacote;
  GRef, GPasta, GParam, GZip, GAcao: string;
  GVersao: TVersaoRAL;
  vInt, vSub: integer;
  vT0: QWord;

function ResolverRef(const ARef: string): TVersaoRAL;
begin
  Result := nil;
  if not GRepo.ListarVersoes(GVersoes) then
  begin
    WriteLn('ERRO: ', GRepo.Erro);
    Halt(1);
  end;
  if GRepo.Aviso <> '' then
    WriteLn('aviso: ', GRepo.Aviso);
  if SameText(ARef, 'estavel') then
    Result := GVersoes.Recomendada
  else
    Result := GVersoes.Buscar(ARef);
  if Result = nil then
  begin
    WriteLn('ERRO: versão desconhecida: ', ARef);
    Halt(1);
  end;
end;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  GSaida := TSaida.Create;
  GRepo := TRepoGitHub.Create(DonoRAL, RepoRAL);
  GRepo.Log := @GSaida.Linha;
  GRepo.OnProgresso := @GSaida.Progresso;
  GVersoes := TVersoesRAL.Create(True);
  GCatalogo := TCatalogo.Create;
  GNomes := TStringList.Create;
  GLista := TList.Create;
  try
    GTipo := tpDelphi;
    GAcao := '';
    for vInt := 1 to ParamCount do
    begin
      GParam := ParamStr(vInt);
      if GParam = '--listar' then
        GAcao := 'listar'
      else if Copy(GParam, 1, 11) = '--catalogo=' then
      begin
        GAcao := 'catalogo';
        GRef := Copy(GParam, 12, MaxInt);
      end
      else if Copy(GParam, 1, 9) = '--baixar=' then
      begin
        GAcao := 'baixar';
        GRef := Copy(GParam, 10, MaxInt);
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

    if GAcao = '' then
    begin
      WriteLn('uso: versoes_ral --listar');
      WriteLn('     versoes_ral --catalogo=<ref|estavel> [delphi|lazarus] [pacote ...]');
      WriteLn('     versoes_ral --baixar=<ref|estavel> --pasta=<pasta> [delphi|lazarus] [pacote ...]');
      Halt(2);
    end;

    if GAcao = 'listar' then
    begin
      vT0 := GetTickCount64;
      if not GRepo.ListarVersoes(GVersoes) then
      begin
        WriteLn('ERRO: ', GRepo.Erro);
        Halt(1);
      end;
      if GRepo.Aviso <> '' then
        WriteLn('aviso: ', GRepo.Aviso);
      for vInt := 0 to Pred(GVersoes.Count) do
        WriteLn('  ', GVersoes[vInt].Descricao);
      WriteLn(Format('(%d versões, %d ms; cache em %s)',
                     [GVersoes.Count, GetTickCount64 - vT0, GRepo.PastaCache]));
      Halt(0);
    end;

    GVersao := ResolverRef(GRef);
    WriteLn('versão: ', GVersao.Descricao);

    // o catalogo vem do zip no cache, sem extrair nada
    vT0 := GetTickCount64;
    if not GRepo.BaixarZip(GVersao.Ref, GVersao.Tipo = tvRamo, GZip) then
    begin
      WriteLn('ERRO: ', GRepo.Erro);
      Halt(1);
    end;
    if not GCatalogo.Carregar(TOrigemZip.Create(GZip, 'github.com/' + DonoRAL + '/' +
                                                RepoRAL + ' @ ' + GVersao.Ref)) then
    begin
      WriteLn('ERRO: nenhum pacote nesta versão');
      Halt(1);
    end;
    WriteLn(Format('catálogo lido do zip em %d ms (commit %s)',
                   [GetTickCount64 - vT0, TOrigemZip(GCatalogo.Origem).Commit]));

    if GNomes.Count = 0 then
      GCatalogo.Listar(GTipo, GLista)
    else
      GCatalogo.Fechamento(GTipo, GNomes, GLista);

    if GAcao = 'catalogo' then
    begin
      if GNomes.Count = 0 then
        WriteLn(GCatalogo.Plano(GTipo))
      else
        WriteLn(GCatalogo.Plano(GTipo, GNomes));
      Halt(0);
    end;

    if GPasta = '' then
    begin
      WriteLn('ERRO: --baixar precisa de --pasta=');
      Halt(2);
    end;
    GPreparo := TPreparoFontes.Create(GRepo, GVersao.Ref, GVersao.Tipo = tvRamo);
    try
      GPreparo.PastaBase := GPasta;
      GPreparo.Log := @GSaida.Linha;
      for vInt := 0 to Pred(GLista.Count) do
        for vSub := 0 to Pred(TPacote(GLista[vInt]).Submodulos.Count) do
          GPreparo.Submodulos.Add(TPacote(GLista[vInt]).Submodulos[vSub]);
      WriteLn('submódulos necessários: ', GPreparo.Submodulos.CommaText);
      vT0 := GetTickCount64;
      if not GPreparo.Executar then
      begin
        WriteLn('ERRO: ', GPreparo.Erro);
        Halt(1);
      end;
      WriteLn(Format('fontes prontos em %s (%d ms)', [GPreparo.PastaDestino, GetTickCount64 - vT0]));
    finally
      GPreparo.Free;
    end;
  finally
    GLista.Free;
    GNomes.Free;
    GCatalogo.Free;
    GVersoes.Free;
    GRepo.Free;
    GSaida.Free;
  end;
end.
