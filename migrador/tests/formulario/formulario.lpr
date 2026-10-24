program formulario;

{$mode ObjFPC}{$H+}

// Acoes de formulario do migrador (etapa M4).
//   formulario --testes      casos sinteticos (CORS, compressao, cripto, dois
//                            servidores, nome que colide, .lfm, formulario
//                            herdado, segunda rodada, valor nao descartavel...)
//   formulario --gabarito    migra as copias 1.x dos samples (pai de 677864d,
//                            extraido com git archive para a pasta temporaria)
//                            e compara cada formulario, como arvore, com o do
//                            677864d; lista as diferencas previstas e as outras
//   --samples=<repo>         repositorio PascalRAL-Samples-2.0 (padrao: o irmao
//                            do PascalRAL-Instalador)
//   --lista                  no --gabarito, lista cada formulario conferido
//
// Compilar (da raiz do PascalRAL-Instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/formulario/formulario.lpr
// Rodar da raiz do PascalRAL-Instalador.

uses
  Classes, SysUtils, StrUtils,
  RALMig.Texto, RALMig.Tokens, RALMig.Regras, RALMig.API, RALMig.Tipos,
  RALMig.Formulario, RALMig.&Uses, RALMig.Acoes.Formulario;

var
  GFalhas: integer = 0;
  GRegras: TConjuntoRegras = nil;
  GApis: array[0..2] of TApi;
  GSamples: string = '';
  GLista: boolean = False;
  GTemp: string = '';

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

function Tem(const ATexto, ATrecho: string): boolean;
begin
  Result := Pos(ATrecho, ATexto) > 0;
end;

function Conta(const ATexto, ATrecho: string): integer;
var
  vPos: integer;
begin
  Result := 0;
  vPos := Pos(ATrecho, ATexto);
  while vPos > 0 do
  begin
    Inc(Result);
    vPos := PosEx(ATrecho, ATexto, vPos + 1);
  end;
end;

{ environment }

function AcharRaiz: string;
var
  vDir: string;
  vInt: integer;
begin
  Result := '';
  vDir := ExtractFilePath(ExpandFileName(ParamStr(0)));
  for vInt := 1 to 8 do
  begin
    if DirectoryExists(vDir + 'migrador' + PathDelim + 'regras') then
      Exit(vDir);
    vDir := ExtractFilePath(ExcludeTrailingPathDelimiter(vDir));
  end;
  vDir := IncludeTrailingPathDelimiter(GetCurrentDir);
  if DirectoryExists(vDir + 'migrador' + PathDelim + 'regras') then
    Result := vDir;
end;

procedure Preparar;
var
  vRaiz: string;
  vInt: integer;
const
  Nomes: array[0..2] of string = ('api-1.1.json', 'api-dev.json', 'api-2.0.json');
begin
  vRaiz := AcharRaiz;
  if vRaiz = '' then
  begin
    WriteLn('Nao achei migrador/regras: rode da raiz do PascalRAL-Instalador.');
    Halt(2);
  end;
  GRegras := TConjuntoRegras.Create;
  if not GRegras.CarregarDePasta(vRaiz + 'migrador' + PathDelim + 'regras', True) then
  begin
    WriteLn('Regras: ', GRegras.Erros.Text);
    Halt(2);
  end;
  for vInt := 0 to 2 do
  begin
    GApis[vInt] := TApi.Create;
    GApis[vInt].CarregarArquivo(vRaiz + 'migrador' + PathDelim + 'api' + PathDelim + Nomes[vInt]);
  end;
  if GSamples = '' then
    GSamples := ExtractFilePath(ExcludeTrailingPathDelimiter(vRaiz)) +
      'PascalRAL-Samples-2.0';
  GTemp := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig_formulario' + PathDelim;
  ForceDirectories(GTemp);
end;

{ file helpers }

procedure Escrever(const ACaminho, AConteudo: string);
var
  vLista: TStringStream;
begin
  ForceDirectories(ExtractFilePath(ACaminho));
  vLista := TStringStream.Create(AConteudo);
  try
    vLista.SaveToFile(ACaminho);
  finally
    vLista.Free;
  end;
end;

function Juntar(const ALinhas: array of string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to High(ALinhas) do
    Result := Result + ALinhas[vInt] + #13#10;
end;

procedure ApagarPasta(const APasta: string);
var
  vBusca: TSearchRec;
  vDir: string;
begin
  vDir := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vDir + '*', faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        ApagarPasta(vDir + vBusca.Name)
      else
        DeleteFile(vDir + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
  RemoveDir(vDir);
end;

procedure ListarArquivos(const APasta: string; ALista: TStrings; ARecursivo: boolean);
var
  vBusca: TSearchRec;
  vDir, vExt: string;
begin
  vDir := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vDir + '*', faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
      begin
        if ARecursivo then
          ListarArquivos(vDir + vBusca.Name, ALista, True);
      end
      else
      begin
        vExt := LowerCase(ExtractFileExt(vBusca.Name));
        if (vExt = '.pas') or (vExt = '.dfm') or (vExt = '.lfm') or (vExt = '.fmx') then
          ALista.Add(vDir + vBusca.Name);
      end;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

{ running the form rules }

function EhAcaoDeFormulario(AAcao: TAcaoRegra): boolean;
begin
  Result := AAcao in [arPropriedadeRenomear, arPropriedadeParaComponente,
    arPropriedadeRemover, arValorPadraoMaterializar, arObjetoSemEquivalente];
end;

/// Applies the form rules of one step, in order, consolidating after each
procedure AplicarPasso(AConj: TConjuntoArquivos; const ADe, APara: string;
  AApiDe, AApiPara: TApi; ARes: TResultadoMigracao);
var
  vCtx: TContextoMigracao;
  vPasso: TPassoMigracao;
  vInt: integer;
  vAcao: TAcaoMigracao;
begin
  vPasso := GRegras.BuscarPasso(OrigemRAL, ADe, APara);
  vCtx := TContextoMigracao.Create;
  try
    vCtx.Conjunto := AConj;
    vCtx.Resultado := ARes;
    vCtx.Passo := vPasso;
    vCtx.ApiDe := AApiDe;
    vCtx.ApiPara := AApiPara;
    vCtx.Idioma := IdiomaPtBR;
    for vInt := 0 to vPasso.RegraCount - 1 do
      if EhAcaoDeFormulario(vPasso.Regras[vInt].Acao) then
      begin
        vAcao := ClasseDaAcao(vPasso.Regras[vInt].Acao).Create;
        try
          vAcao.Aplicar(vPasso.Regras[vInt], vCtx);
        finally
          vAcao.Free;
        end;
        AConj.Consolidar;
      end;
  finally
    vCtx.Free;
  end;
end;

procedure AplicarCadeia(AConj: TConjuntoArquivos; ARes: TResultadoMigracao;
  A11: boolean);
begin
  if A11 then
    AplicarPasso(AConj, '1.1', '1.2', GApis[0], GApis[1], ARes);
  AplicarPasso(AConj, '1.2', '2.0', GApis[1], GApis[2], ARes);
end;

function NovoConjunto(const APasta: string): TConjuntoArquivos;
var
  vLista: TStringList;
  vInt: integer;
begin
  Result := TConjuntoArquivos.Create(APasta);
  vLista := TStringList.Create;
  try
    ListarArquivos(APasta, vLista, False);
    for vInt := 0 to vLista.Count - 1 do
      Result.Adicionar(vLista[vInt]);
  finally
    vLista.Free;
  end;
end;

function Atual(AConj: TConjuntoArquivos; const ANome: string): string;
begin
  Result := AConj.Buscar(ANome).Atual;
end;

function FormularioOk(const ATexto: string): boolean;
var
  vTxt: TTextoOriginal;
  vFrm: TFormulario;
begin
  Result := False;
  vTxt := TTextoOriginal.Create;
  try
    vTxt.CarregarBytes(ATexto, 'x.dfm');
    try
      vFrm := TFormulario.Create(vTxt);
      vFrm.Free;
      Result := True;
    except
      on EFormularioInvalido do
        Result := False;
    end;
  finally
    vTxt.Free;
  end;
end;

{ synthetic cases }

const
  PAS_BASE =
    'unit Unit1;'#13#10 +
    'interface'#13#10 +
    'uses'#13#10 +
    '  Classes, Forms, RALIndyServer;'#13#10 +
    'type'#13#10 +
    '  TForm1 = class(TForm)'#13#10 +
    '    Server: TRALIndyServer;'#13#10 +
    '    Memo1: TMemo;'#13#10 +
    '  private'#13#10 +
    '    { Private declarations }'#13#10 +
    '  end;'#13#10 +
    'var'#13#10 +
    '  Form1: TForm1;'#13#10 +
    'implementation'#13#10 +
    '{$R *.dfm}'#13#10 +
    'end.'#13#10;

function ServidorDfm(const ANome: string; AComCors: boolean): string;
begin
  Result :=
    '  object ' + ANome + ': TRALIndyServer'#13#10 +
    '    Active = False'#13#10 +
    '    CompressType = ctNone'#13#10;
  if AComCors then
    Result := Result +
      '    CORSOptions.AllowHeaders.Strings = ('#13#10 +
      '      ''Content-Type'''#13#10 +
      '      ''Origin'')'#13#10 +
      '    CORSOptions.AllowOrigin = ''*'''#13#10 +
      '    CORSOptions.MaxAge = 86400'#13#10;
  Result := Result +
    '    CriptoOptions.CriptType = crNone'#13#10 +
    '    Port = 8000'#13#10 +
    '    Security.Options = []'#13#10 +
    '    Left = 192'#13#10 +
    '    Top = 8'#13#10 +
    '  end'#13#10;
end;

function FormDfm(const ACorpo: string): string;
begin
  Result := 'object Form1: TForm1'#13#10 +
    '  Left = 0'#13#10 +
    '  Top = 0'#13#10 +
    '  Caption = ''Form1'''#13#10 +
    '  object Memo1: TMemo'#13#10 +
    '    Left = 8'#13#10 +
    '    Top = 80'#13#10 +
    '    Width = 100'#13#10 +
    '    Height = 50'#13#10 +
    '  end'#13#10 + ACorpo + 'end'#13#10;
end;

function NovaPasta(const ANome: string): string;
begin
  Result := GTemp + ANome + PathDelim;
  ApagarPasta(Result);
  ForceDirectories(Result);
end;

procedure TesteCors;
var
  vPasta, vForm, vPas, vForm2: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('CORS + compressao + cripto sem chave');
  vPasta := NovaPasta('cors');
  Escrever(vPasta + 'unit1.pas', PAS_BASE);
  Escrever(vPasta + 'unit1.dfm', FormDfm(ServidorDfm('Server', True)));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vForm := Atual(vConj, 'unit1.dfm');
    vPas := Atual(vConj, 'unit1.pas');
    Conferir(FormularioOk(vForm), 'o formulario migrado continua valido');
    Conferir(Tem(vForm, 'object RALCORSPlugin1: TRALCORSPlugin'), 'plugin de CORS criado');
    Conferir(Tem(vForm, '    Server = Server'), 'plugin ligado ao servidor');
    Conferir(Tem(vForm, '    Options.AllowOrigin = ''*'''), 'AllowOrigin movido');
    Conferir(Tem(vForm, '    Options.AllowHeaders.Strings = ('#13#10'      ''Content-Type'''#13#10 +
      '      ''Origin'')'), 'lista AllowHeaders.Strings multi-linha movida');
    Conferir(Tem(vForm, '    Options.MaxAge = 86400'), 'MaxAge movido');
    Conferir(not Tem(vForm, 'CORSOptions'), 'CORSOptions saiu do servidor');
    Conferir(Tem(vForm, 'object RALCompressPlugin1: TRALCompressPlugin'),
      'CompressType = ctNone gera TRALCompressPlugin');
    Conferir(not Tem(vForm, 'CompressType'), 'sem CompressType no plugin (ctNone e o padrao)');
    Conferir(not Tem(vForm, 'CriptoOptions') and not Tem(vForm, 'TRALCriptoPlugin'),
      'cifra crNone e sem chave: nada criado, linha some');
    Conferir(not Tem(vForm, 'Security.Options'), 'Security.Options = [] sumiu');
    Conferir(Tem(vPas, '    RALCompressPlugin1: TRALCompressPlugin;'#13#10 +
      '    RALCORSPlugin1: TRALCORSPlugin;'#13#10 + '    Memo1: TMemo;'),
      'campos logo depois do campo do servidor, na ordem dos plugins');
    Conferir(Tem(vPas, 'RALContent') and Tem(vPas, 'RALCORS'), 'units no uses da interface');
    Conferir(vRes.ManualCount = 0, 'nenhum item manual');
    // second round: nothing changes
    vForm2 := vForm;
    vConj.Free;
    vConj := NovoConjunto(vPasta);
    vConj.Buscar('unit1.dfm').Texto.RedefinirOriginal(vForm);
    vConj.Buscar('unit1.pas').Texto.RedefinirOriginal(vPas);
    AplicarCadeia(vConj, vRes, False);
    Conferir(Atual(vConj, 'unit1.dfm') = vForm2, 'segunda rodada: formulario sem mudanca');
    Conferir(Atual(vConj, 'unit1.pas') = vPas, 'segunda rodada: unit sem mudanca');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteSemCors;
var
  vPasta, vForm: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('CompressType ausente, servidor sem CORS');
  vPasta := NovaPasta('semcors');
  Escrever(vPasta + 'unit1.pas', PAS_BASE);
  Escrever(vPasta + 'unit1.dfm', FormDfm(
    '  object Server: TRALIndyServer'#13#10'    Port = 8000'#13#10'    Left = 192'#13#10 +
    '    Top = 8'#13#10'  end'#13#10));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vForm := Atual(vConj, 'unit1.dfm');
    Conferir(Tem(vForm, 'TRALCompressPlugin'), 'CompressType ausente tambem cria o plugin');
    Conferir(not Tem(vForm, 'TRALCORSPlugin'), 'sem AllowOrigin nao ha plugin de CORS');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteCriptoComChave;
var
  vPasta, vForm: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('Cripto com chave, limite e conjunto de seguranca');
  vPasta := NovaPasta('cripto');
  Escrever(vPasta + 'unit1.pas', PAS_BASE);
  Escrever(vPasta + 'unit1.dfm', FormDfm(
    '  object Server: TRALIndyServer'#13#10 +
    '    CriptoOptions.CriptType = crAES256'#13#10 +
    '    CriptoOptions.Key = ''segredo'''#13#10 +
    '    JSONBodyToParams = True'#13#10 +
    '    MaxRequestSize = 1024'#13#10 +
    '    Port = 8000'#13#10 +
    '    Security.BruteForce.ExpirationTime = 1800000'#13#10 +
    '    Security.BruteForce.MaxTry = 5'#13#10 +
    '    Security.FloodTimeInterval = 30'#13#10 +
    '    Security.Options = [rsoBruteForceProtection]'#13#10 +
    '    Left = 192'#13#10 +
    '    Top = 8'#13#10 +
    '  end'#13#10));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vForm := Atual(vConj, 'unit1.dfm');
    Conferir(FormularioOk(vForm), 'formulario valido');
    Conferir(Tem(vForm, 'object RALCriptoPlugin1: TRALCriptoPlugin') and
      Tem(vForm, '    Key = ''segredo'''), 'plugin de cripto com a chave');
    Conferir(Tem(vForm, 'TRALLimitsPlugin') and Tem(vForm, 'MaxRequestSize = 1024'),
      'plugin de limites');
    Conferir(Tem(vForm, 'TRALJSONBodyPlugin'), 'plugin JSONBody');
    Conferir(Tem(vForm, 'object RALBruteForcePlugin1: TRALBruteForcePlugin') and
      Tem(vForm, 'MaxTry = 5') and Tem(vForm, 'ExpirationTime = 1800000'),
      'plugin de forca bruta com MaxTry e ExpirationTime');
    Conferir(not Tem(vForm, 'TRALFloodPlugin'), 'flood desligado: sem plugin');
    Conferir(not Tem(vForm, 'Security.') and not Tem(vForm, 'CriptoOptions') and
      not Tem(vForm, 'JSONBodyToParams'),
      'nenhuma propriedade antiga sobra no servidor (opcao consumida, [] descartado)');
    Conferir(vRes.ManualCount = 0, 'nenhum item manual');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteDoisServidoresENome;
var
  vPasta, vForm, vPas: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('Dois servidores e nome que colide');
  vPasta := NovaPasta('dois');
  Escrever(vPasta + 'unit1.pas', StringReplace(PAS_BASE,
    '    Server: TRALIndyServer;'#13#10,
    '    Server: TRALIndyServer;'#13#10'    Server2: TRALIndyServer;'#13#10 +
    '    RALCORSPlugin1: TMemo;'#13#10, []));
  Escrever(vPasta + 'unit1.dfm', FormDfm(
    ServidorDfm('Server', True) + ServidorDfm('Server2', True) +
    '  object RALCORSPlugin1: TMemo'#13#10'    Left = 300'#13#10'  end'#13#10));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vForm := Atual(vConj, 'unit1.dfm');
    vPas := Atual(vConj, 'unit1.pas');
    Conferir(Conta(vForm, ': TRALCORSPlugin') = 2, 'um plugin de CORS por servidor');
    Conferir(Conta(vForm, ': TRALCompressPlugin') = 2, 'um plugin de compressao por servidor');
    Conferir(Tem(vForm, 'object RALCORSPlugin2: TRALCORSPlugin') and
      Tem(vForm, 'object RALCORSPlugin3: TRALCORSPlugin'),
      'nome que colide pula para 2 e 3');
    Conferir(Tem(vForm, 'object RALCORSPlugin1: TMemo'), 'o objeto que ja existia continua');
    Conferir(Tem(vPas, 'RALCORSPlugin2: TRALCORSPlugin;') and
      Tem(vPas, 'RALCORSPlugin3: TRALCORSPlugin;'), 'campos dos dois no .pas');
    Conferir(Conta(vPas, 'RALContent') = 1, 'RALContent entra uma vez no uses');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteLfm;
var
  vPasta, vForm: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('.lfm (LCL)');
  vPasta := NovaPasta('lfm');
  Escrever(vPasta + 'unit1.pas', StringReplace(PAS_BASE, #13#10, #10, [rfReplaceAll]));
  Escrever(vPasta + 'unit1.lfm', StringReplace(FormDfm(
    '  object Server: TRALIndyServer'#13#10 +
    '    CORSOptions.AllowHeaders.Strings = ('#13#10 +
    '      ''Content-Type'''#13#10 +
    '    )'#13#10 +
    '    CORSOptions.AllowOrigin = ''*'''#13#10 +
    '    Left = 32'#13#10'    Top = 16'#13#10'  end'#13#10), #13#10, #10, [rfReplaceAll]));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vForm := Atual(vConj, 'unit1.lfm');
    Conferir(Tem(vForm, 'object RALCORSPlugin1: TRALCORSPlugin'), 'plugin criado no .lfm');
    Conferir(Tem(vForm, '    Options.AllowHeaders.Strings = (' + #10 + '      ''Content-Type'''#10 +
      '    )'), 'lista com ")" em linha propria preservada');
    Conferir(Pos(#13, vForm) = 0, 'fim de linha do arquivo (LF) preservado');
    Conferir(Pos(#13, Atual(vConj, 'unit1.pas')) = 0, 'fim de linha da unit (LF) preservado');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteHerdado;
var
  vPasta, vFilho, vPai, vPasFilho: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('Formulario herdado (inherited) e fora do conjunto');
  vPasta := NovaPasta('herdado');
  Escrever(vPasta + 'pai.pas', StringReplace(PAS_BASE, 'Unit1', 'Pai', []));
  Escrever(vPasta + 'pai.dfm', FormDfm(ServidorDfm('Server', True)));
  Escrever(vPasta + 'filho.pas',
    'unit Filho;'#13#10'interface'#13#10'uses Pai;'#13#10'type'#13#10 +
    '  TFilho = class(TForm1)'#13#10'  end;'#13#10'implementation'#13#10'end.'#13#10);
  Escrever(vPasta + 'filho.dfm',
    'inherited Filho: TFilho'#13#10'  Caption = ''x'''#13#10 +
    '  inherited Server: TRALIndyServer'#13#10 +
    '    CORSOptions.AllowOrigin = ''*'''#13#10 +
    '    CompressType = ctNone'#13#10 +
    '  end'#13#10'end'#13#10);
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vPai := Atual(vConj, 'pai.dfm');
    vFilho := Atual(vConj, 'filho.dfm');
    vPasFilho := Atual(vConj, 'filho.pas');
    Conferir(Tem(vPai, 'TRALCORSPlugin') and Tem(vPai, 'TRALCompressPlugin'),
      'o plugin nasce no formulario ancestral');
    Conferir(not Tem(vFilho, 'CORSOptions') and not Tem(vFilho, 'CompressType'),
      'no descendente so some a propriedade que repetia');
    Conferir(not Tem(vFilho, 'Plugin'), 'o descendente nao ganha um segundo plugin');
    Conferir(not Tem(vPasFilho, 'Plugin') and (vPasFilho = 'unit Filho;'#13#10'interface'#13#10 +
      'uses Pai;'#13#10'type'#13#10'  TFilho = class(TForm1)'#13#10'  end;'#13#10 +
      'implementation'#13#10'end.'#13#10), 'a unit do descendente nao muda');
    Conferir(vRes.ManualCount = 0, 'valor igual ao do ancestral: nenhum item manual');
  finally
    vRes.Free;
    vConj.Free;
  end;
  // ancestor outside of the set: manual item, nothing touched
  vPasta := NovaPasta('herdado2');
  Escrever(vPasta + 'filho.pas',
    'unit Filho;'#13#10'interface'#13#10'type'#13#10'  TFilho = class(TForm1)'#13#10 +
    '  end;'#13#10'implementation'#13#10'end.'#13#10);
  Escrever(vPasta + 'filho.dfm',
    'inherited Filho: TFilho'#13#10'  inherited Server: TRALIndyServer'#13#10 +
    '    CORSOptions.AllowOrigin = ''*'''#13#10'  end'#13#10'end'#13#10);
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    Conferir(Tem(Atual(vConj, 'filho.dfm'), 'CORSOptions.AllowOrigin'),
      'ancestral fora do conjunto: propriedade intacta');
    Conferir(vRes.ManualCount > 0, 'ancestral fora do conjunto: item manual');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteDescendenteUsuario;
var
  vPasta, vForm: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('Classe do usuario que descende de servidor RAL');
  vPasta := NovaPasta('usuario');
  Escrever(vPasta + 'unit1.pas', StringReplace(PAS_BASE, 'TRALIndyServer;',
    'TMeuServer;', []) + 'type'#13#10'  TMeuServer = class(TRALIndyServer)'#13#10'  end;'#13#10);
  Escrever(vPasta + 'unit1.dfm', FormDfm(StringReplace(ServidorDfm('Server', True),
    'TRALIndyServer', 'TMeuServer', [])));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vForm := Atual(vConj, 'unit1.dfm');
    Conferir(Tem(vForm, 'TRALCORSPlugin'), 'classe do usuario com ancestral RAL provado: migrada');
  finally
    vRes.Free;
    vConj.Free;
  end;
  vPasta := NovaPasta('desconhecida');
  Escrever(vPasta + 'unit1.pas', PAS_BASE);
  Escrever(vPasta + 'unit1.dfm', FormDfm(StringReplace(ServidorDfm('Server', True),
    'TRALIndyServer', 'TOutroServer', [])));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    Conferir(not Tem(Atual(vConj, 'unit1.dfm'), 'TRALCORSPlugin') and (vRes.ManualCount > 0),
      'classe que nao da para provar: item manual, nada escrito');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteNaoDescartavel;
var
  vPasta, vForm: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('Propriedade com valor que nao e o padrao');
  vPasta := NovaPasta('naodescartavel');
  Escrever(vPasta + 'unit1.pas', PAS_BASE);
  Escrever(vPasta + 'unit1.dfm', FormDfm(
    '  object Server: TRALIndyServer'#13#10 +
    '    Security.Options = [rsoPathTransvBlackList, rsoOutra]'#13#10 +
    '    Left = 192'#13#10'    Top = 8'#13#10'  end'#13#10));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vForm := Atual(vConj, 'unit1.dfm');
    Conferir(Tem(vForm, 'TRALPathTraversalPlugin'), 'opcao conhecida vira plugin');
    Conferir(Tem(vForm, 'Security.Options = [rsoOutra]'), 'a opcao desconhecida fica no servidor');
    Conferir(vRes.ManualCount = 1, 'e vira item manual (nao e o padrao [])');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteRenomearEPadrao;
var
  vPasta, vForm: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('1.1 -> 1.2: renomear e materializar');
  vPasta := NovaPasta('onze');
  Escrever(vPasta + 'unit1.pas', StringReplace(PAS_BASE, 'TRALIndyServer', 'TRALSaguiServer', []));
  Escrever(vPasta + 'unit1.dfm', FormDfm(
    '  object Server: TRALSaguiServer'#13#10 +
    '    ConnectionLimit = 10'#13#10 +
    '    Port = 8000'#13#10 +
    '    Left = 192'#13#10'    Top = 8'#13#10'  end'#13#10 +
    '  object Wm: TRALWebModule'#13#10'    Left = 300'#13#10'  end'#13#10 +
    '  object Cli: TRALClient'#13#10'    Left = 400'#13#10'  end'#13#10 +
    '  object Qry: TRALFDQuery'#13#10'    Left = 500'#13#10'  end'#13#10));
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarPasso(vConj, '1.1', '1.2', GApis[0], GApis[1], vRes);
    vForm := Atual(vConj, 'unit1.dfm');
    Conferir(Tem(vForm, 'MaxConnections = 10') and not Tem(vForm, 'ConnectionLimit'),
      'ConnectionLimit renomeada para MaxConnections');
    Conferir(Tem(vForm, 'CORSOptions.AllowOrigin = ''*'''), 'CORS aberto escrito explicitamente');
    Conferir(Tem(vForm, 'QueryBehavior = ebMultiThread'), 'QueryBehavior escrito');
    Conferir(not Tem(vForm, 'UseApplicationPathAsRoot'),
      'modo "perguntar" nao escreve nada no formulario');
    Conferir(vRes.ManualCount >= 1, 'modo "perguntar" vira item manual com o valor sugerido');
    Conferir(vRes.ComportamentoCount >= 1, 'o comportamento que continua e relatado');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure TesteFrameEDataModule;
var
  vPasta, vFrame, vDm, vForm, vPas: string;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
begin
  WriteLn('Frame (inline) e DataModule');
  vPasta := NovaPasta('frame');
  Escrever(vPasta + 'frame1.pas', StringReplace(StringReplace(PAS_BASE, 'Unit1', 'Frame1', []),
    'TForm1 = class(TForm)', 'TFrame1 = class(TFrame)', []));
  Escrever(vPasta + 'frame1.dfm', 'object Frame1: TFrame1'#13#10'  Left = 0'#13#10 +
    ServidorDfm('Server', True) + 'end'#13#10);
  Escrever(vPasta + 'dm1.pas', StringReplace(StringReplace(PAS_BASE, 'Unit1', 'Dm1', []),
    'TForm1 = class(TForm)', 'TDataModule1 = class(TDataModule)', []));
  Escrever(vPasta + 'dm1.dfm', 'object DataModule1: TDataModule1'#13#10'  Height = 100'#13#10 +
    ServidorDfm('Server', True) + 'end'#13#10);
  Escrever(vPasta + 'unit2.pas', 'unit Unit2;'#13#10'interface'#13#10'uses Frame1;'#13#10 +
    'type'#13#10'  TForm2 = class(TForm)'#13#10'    Frame11: TFrame1;'#13#10'  end;'#13#10 +
    'implementation'#13#10'end.'#13#10);
  Escrever(vPasta + 'unit2.dfm', 'object Form2: TForm2'#13#10'  inline Frame11: TFrame1'#13#10 +
    '    Left = 8'#13#10'    inherited Server: TRALIndyServer'#13#10 +
    '      CORSOptions.AllowOrigin = ''*'''#13#10'      CompressType = ctNone'#13#10 +
    '    end'#13#10'  end'#13#10'end'#13#10);
  vConj := NovoConjunto(vPasta);
  vRes := TResultadoMigracao.Create;
  try
    AplicarCadeia(vConj, vRes, False);
    vFrame := Atual(vConj, 'frame1.dfm');
    vDm := Atual(vConj, 'dm1.dfm');
    vForm := Atual(vConj, 'unit2.dfm');
    vPas := Atual(vConj, 'frame1.pas');
    Conferir(Tem(vFrame, 'TRALCORSPlugin') and Tem(vFrame, 'TRALCompressPlugin') and
      FormularioOk(vFrame), 'frame: plugins criados no proprio frame');
    Conferir(Tem(vPas, ': TRALCORSPlugin;') and Tem(vPas, ', RALCORS'),
      'frame: campo e unit no .pas do frame');
    Conferir(Tem(vDm, 'object RALCORSPlugin1: TRALCORSPlugin') and FormularioOk(vDm) and
      Tem(vDm, '  Height = 100'#13#10), 'DataModule: plugins criados, propriedades preservadas');
    Conferir(Tem(Atual(vConj, 'dm1.pas'), ': TRALCompressPlugin;'),
      'DataModule: campo no .pas');
    Conferir(not Tem(vForm, 'CORSOptions') and not Tem(vForm, 'Plugin') and FormularioOk(vForm),
      'inline: so some a propriedade repetida, sem segundo plugin');
  finally
    vRes.Free;
    vConj.Free;
  end;
end;

procedure RodarTestes;
begin
  TesteCors;
  TesteSemCors;
  TesteCriptoComChave;
  TesteDoisServidoresENome;
  TesteLfm;
  TesteHerdado;
  TesteFrameEDataModule;
  TesteDescendenteUsuario;
  TesteNaoDescartavel;
  TesteRenomearEPadrao;
end;

{ gabarito: migrated 1.x samples against 677864d }

function ExtrairCommit(const ACommit, ADestino: string): boolean;
var
  vCmd: string;
begin
  if DirectoryExists(ADestino) and (FileExists(ADestino + '.ok')) then
    Exit(True);
  ApagarPasta(ADestino);
  ForceDirectories(ADestino);
  vCmd := '/c git -C "' + GSamples + '" archive ' + ACommit + ' | tar -x -C "' +
    ExcludeTrailingPathDelimiter(ADestino) + '"';
  Result := ExecuteProcess('cmd.exe', vCmd) = 0;
  if Result then
    Escrever(ADestino + '.ok', 'ok');
end;

function NomeNormalizado(const ANome: string): string;
var
  vFim: integer;
begin
  Result := ANome;
  vFim := Length(Result);
  while (vFim > 0) and (Result[vFim] in ['0'..'9']) do
    Dec(vFim);
  if (vFim < Length(Result)) and (Copy(Result, 1, 3) = 'RAL') and
     (Pos('Plugin', Result) > 0) then
    Result := Copy(Result, 1, vFim);
end;

function ValorNormalizado(const AValor: string): string;
var
  vInt: integer;
  vAspas: boolean;
begin
  Result := '';
  vAspas := False;
  for vInt := 1 to Length(AValor) do
  begin
    if AValor[vInt] = '''' then
      vAspas := not vAspas;
    if vAspas or not (AValor[vInt] in [' ', #9, #10, #13]) then
      Result := Result + AValor[vInt];
  end;
end;

function EhPosicao(const ANome: string): boolean;
begin
  Result := SameText(ANome, 'Left') or SameText(ANome, 'Top') or
            SameText(ANome, 'ExplicitLeft') or SameText(ANome, 'ExplicitTop');
end;

procedure ColetarArvore(ANo: TNoFormulario; const APai: string; ALista: TStrings);
var
  vCaminho: string;
  vInt: integer;
const
  Tipos: array[TTipoNo] of string = ('object', 'inherited', 'inline');
begin
  vCaminho := APai + '/' + NomeNormalizado(ANo.Nome);
  ALista.Add(vCaminho + ' @ ' + Tipos[ANo.Tipo] + ' ' + ANo.Classe);
  for vInt := 0 to ANo.PropriedadeCount - 1 do
    if not EhPosicao(ANo.Propriedades[vInt].Nome) then
      ALista.Add(vCaminho + ' . ' + ANo.Propriedades[vInt].Nome + ' = ' +
        ValorNormalizado(ANo.Propriedades[vInt].ValorBruto));
  for vInt := 0 to ANo.FilhoCount - 1 do
    ColetarArvore(ANo.Filhos[vInt], vCaminho, ALista);
end;

function ArvoreDe(const ABytes: string; ALista: TStrings): boolean;
var
  vTxt: TTextoOriginal;
  vFrm: TFormulario;
  vInt: integer;
begin
  Result := False;
  vTxt := TTextoOriginal.Create;
  try
    vTxt.CarregarBytes(ABytes, 'x.dfm');
    try
      vFrm := TFormulario.Create(vTxt);
    except
      on EFormularioInvalido do
        Exit;
    end;
    try
      for vInt := 0 to vFrm.RaizCount - 1 do
        ColetarArvore(vFrm.Raizes[vInt], '', ALista);
      Result := True;
    finally
      vFrm.Free;
    end;
  finally
    vTxt.Free;
  end;
end;

/// Plugin fields and units of a unit, normalized, one per line
procedure ResumoPas(const ABytes: string; ALista: TStrings);
var
  vTxt: TTextoOriginal;
  vTok: TListaTokens;
  vInt, vProx: integer;
  vEd: TEditorUses;
  vCl: TClausulaUses;
  vNome: string;
begin
  vTxt := TTextoOriginal.Create;
  try
    vTxt.CarregarBytes(ABytes, 'x.pas');
    vTok := TokenizarPascal(vTxt.Bytes, vTxt.InicioConteudo);
    try
      for vInt := 0 to vTok.Count - 2 do
        if (vTok[vInt].Tipo = ttIdentificador) and (Copy(vTok.Texto(vInt), 1, 3) = 'RAL') and
           (Pos('Plugin', vTok.Texto(vInt)) > 0) then
        begin
          vProx := vTok.Proximo(vInt);
          if (vProx < vTok.Count) and vTok.EhSimbolo(vProx, ':') then
          begin
            vProx := vTok.Proximo(vProx);
            ALista.Add('campo ' + NomeNormalizado(vTok.Texto(vInt)) + ': ' + vTok.Texto(vProx));
          end;
        end;
    finally
      vTok.Free;
    end;
    vEd := TEditorUses.Create(vTxt);
    try
      vCl := vEd.Clausula(suInterface);
      if vCl <> nil then
        for vInt := 0 to vCl.Count - 1 do
        begin
          vNome := vCl.Item(vInt).Nome;
          if (vNome = 'RALContent') or (vNome = 'RALCORS') or (vNome = 'RALSecurity') then
            ALista.Add('uses ' + vNome);
        end;
    finally
      vEd.Free;
    end;
  finally
    vTxt.Free;
  end;
end;

function PropriedadeDoServidor1x(const ALinha: string): boolean;
begin
  Result := (ALinha[1] = '-') and ((Pos(' . CompressType =', ALinha) > 0) or
    (Pos(' . CORSOptions.', ALinha) > 0) or (Pos(' . CriptoOptions.', ALinha) > 0) or
    (Pos(' . Security.', ALinha) > 0));
end;

/// Reason why a difference is expected by a decision of the plan, '' if none.
/// ANaoPortou: the 677864d left the server of this form with its 1.x properties.
function DiferencaPrevista(const ALinha: string; ANaoPortou: boolean): string;
begin
  Result := '';
  if (ALinha[1] = '+') and
     ((Pos('RALCORSPlugin', ALinha) > 0) or (ALinha = '+uses RALCORS')) then
    Result := 'D-M5: CORS aberto na 1.1 continua aberto (o gabarito fechou o CORS)'
  else if ANaoPortou and (PropriedadeDoServidor1x(ALinha) or
          ((ALinha[1] = '+') and ((Pos('Plugin', ALinha) > 0) or (Pos('+uses RAL', ALinha) = 1)))) then
    Result := 'o 677864d nao portou este formulario (o servidor segue com propriedades 1.x, que a 2.0 nao abre)'
  else if (Pos(' . MaxConnections =', ALinha) > 0) and (ALinha[1] = '+') then
    Result := 'ConnectionLimit -> MaxConnections: o gabarito manteve o nome antigo, que ainda funciona'
  else if (Pos(' . ConnectionLimit =', ALinha) > 0) and (ALinha[1] = '-') then
    Result := 'ConnectionLimit -> MaxConnections: o gabarito manteve o nome antigo, que ainda funciona'
  else if Pos('-uses RAL', ALinha) = 1 then
    Result := 'unit acrescentada pelo codigo que cria o plugin (RALMig.Acoes.Codigo, outra unit)';
end;

procedure Diferencas(AMigrado, AGabarito: TStrings; ADest: TStrings);
var
  vInt: integer;
begin
  for vInt := 0 to AMigrado.Count - 1 do
    if AGabarito.IndexOf(AMigrado[vInt]) < 0 then
      ADest.Add('+' + AMigrado[vInt]);
  for vInt := 0 to AGabarito.Count - 1 do
    if AMigrado.IndexOf(AGabarito[vInt]) < 0 then
      ADest.Add('-' + AGabarito[vInt]);
end;

procedure ConferirPar(const ARelativo, AMigradoForm, AMigradoPas, AGabForm,
  AGabPas: string; var AIguais, APrevistos, AFalhas: integer);
var
  vA, vB, vDif: TStringList;
  vInt: integer;
  vPrev: string;
  vTemFalha, vTemPrev, vNaoPortou: boolean;
begin
  vA := TStringList.Create;
  vB := TStringList.Create;
  vDif := TStringList.Create;
  try
    vA.Sorted := True;
    vB.Sorted := True;
    vA.Duplicates := dupIgnore;
    vB.Duplicates := dupIgnore;
    if not ArvoreDe(AGabForm, vB) then
    begin
      WriteLn('  (gabarito nao lido) ', ARelativo);
      Exit;
    end;
    if not ArvoreDe(AMigradoForm, vA) then
    begin
      Inc(APrevistos);
      WriteLn('  prev. ', ARelativo, '   [o formulario 1.x esta quebrado na origem e o migrador o recusa]');
      Exit;
    end;
    if (AMigradoPas <> '') and (AGabPas <> '') then
    begin
      ResumoPas(AMigradoPas, vA);
      ResumoPas(AGabPas, vB);
    end;
    Diferencas(vA, vB, vDif);
    vTemFalha := False;
    vTemPrev := False;
    vNaoPortou := False;
    for vInt := 0 to vDif.Count - 1 do
      if PropriedadeDoServidor1x(vDif[vInt]) then
        vNaoPortou := True;
    for vInt := 0 to vDif.Count - 1 do
    begin
      vPrev := DiferencaPrevista(vDif[vInt], vNaoPortou);
      if vPrev <> '' then
        vTemPrev := True
      else
        vTemFalha := True;
    end;
    if vTemFalha then
      Inc(AFalhas)
    else if vTemPrev then
      Inc(APrevistos)
    else
      Inc(AIguais);
    if vTemFalha or vTemPrev or GLista then
    begin
      if vTemFalha then
        WriteLn('  FALHA ', ARelativo)
      else if vTemPrev then
        WriteLn('  prev. ', ARelativo)
      else
        WriteLn('  ok    ', ARelativo);
      for vInt := 0 to vDif.Count - 1 do
      begin
        vPrev := DiferencaPrevista(vDif[vInt], vNaoPortou);
        if vPrev <> '' then
          WriteLn('          ', vDif[vInt], '   [prevista: ', vPrev, ']')
        else
          WriteLn('          ', vDif[vInt]);
      end;
    end;
  finally
    vDif.Free;
    vB.Free;
    vA.Free;
  end;
end;

function LerTudo(const ACaminho: string): string;
var
  vTxt: TTextoOriginal;
begin
  Result := '';
  if not FileExists(ACaminho) then
    Exit;
  vTxt := TTextoOriginal.Create;
  try
    vTxt.CarregarArquivo(ACaminho);
    Result := vTxt.Bytes;
  finally
    vTxt.Free;
  end;
end;

procedure CopiarArquivo(const AOrigem, ADestino: string);
begin
  Escrever(ADestino, LerTudo(AOrigem));
end;

procedure RodarGabarito;
var
  vVelho, vNovo, vTrab: string;
  vForms, vArqs: TStringList;
  vPastas: TStringList;
  vInt, vArq: integer;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
  vRel, vDir, vPas: string;
  vMigr: TArquivoMigrado;
  vIguais, vPrev, vFalhas, vApps: integer;
begin
  if not DirectoryExists(GSamples) then
  begin
    WriteLn('Repositorio dos samples nao achado: ', GSamples);
    Inc(GFalhas);
    Exit;
  end;
  vVelho := GTemp + 'gabarito_pai' + PathDelim;
  vNovo := GTemp + 'gabarito_677864d' + PathDelim;
  vTrab := GTemp + 'gabarito_trab' + PathDelim;
  if not (ExtrairCommit('677864d~1', vVelho) and ExtrairCommit('677864d', vNovo)) then
  begin
    WriteLn('git archive falhou');
    Inc(GFalhas);
    Exit;
  end;
  ApagarPasta(vTrab);
  vForms := TStringList.Create;
  vArqs := TStringList.Create;
  vPastas := TStringList.Create;
  vPastas.Sorted := True;
  vPastas.Duplicates := dupIgnore;
  vIguais := 0;
  vPrev := 0;
  vFalhas := 0;
  vApps := 0;
  try
    ListarArquivos(vVelho, vForms, True);
    for vInt := 0 to vForms.Count - 1 do
    begin
      vRel := Copy(vForms[vInt], Length(vVelho) + 1, MaxInt);
      CopiarArquivo(vForms[vInt], vTrab + vRel);
      if SameText(ExtractFileExt(vRel), '.dfm') or SameText(ExtractFileExt(vRel), '.lfm') or
         SameText(ExtractFileExt(vRel), '.fmx') then
        vPastas.Add(ExtractFilePath(vRel));
    end;
    for vInt := 0 to vPastas.Count - 1 do
    begin
      Inc(vApps);
      vDir := vTrab + vPastas[vInt];
      vConj := NovoConjunto(vDir);
      vRes := TResultadoMigracao.Create;
      try
        AplicarCadeia(vConj, vRes, True);
        for vArq := 0 to vConj.Count - 1 do
        begin
          vMigr := vConj[vArq];
          if vMigr.Tipo <> taForm then
            Continue;
          vRel := vPastas[vInt] + ExtractFileName(vMigr.Caminho);
          vPas := '';
          if vConj.UnidadeDe(vMigr) <> nil then
            vPas := vConj.UnidadeDe(vMigr).Atual;
          if not FileExists(vNovo + vRel) then
          begin
            WriteLn('  (sem par no 677864d) ', vRel);
            Continue;
          end;
          ConferirPar(vRel, vMigr.Atual, vPas, LerTudo(vNovo + vRel),
            LerTudo(vNovo + ChangeFileExt(vRel, '.pas')), vIguais, vPrev, vFalhas);
        end;
      finally
        vRes.Free;
        vConj.Free;
      end;
    end;
  finally
    vPastas.Free;
    vArqs.Free;
    vForms.Free;
  end;
  WriteLn;
  WriteLn('Aplicacoes (pastas com formulario): ', vApps);
  WriteLn('Formularios: ', vIguais + vPrev + vFalhas, '  iguais: ', vIguais,
    '  so com diferenca prevista: ', vPrev, '  com diferenca nao prevista: ', vFalhas);
  Conferir(vFalhas = 0, 'nenhum formulario com diferenca nao prevista');
end;

{ main }

var
  vInt: integer;
  vArg: string;
  vTestes, vGab: boolean;

begin
  DefaultSystemCodePage := CP_UTF8;
  vTestes := False;
  vGab := False;
  for vInt := 1 to ParamCount do
  begin
    vArg := ParamStr(vInt);
    if vArg = '--testes' then
      vTestes := True
    else if vArg = '--gabarito' then
      vGab := True
    else if vArg = '--lista' then
      GLista := True
    else if Copy(vArg, 1, 10) = '--samples=' then
      GSamples := Copy(vArg, 11, MaxInt);
  end;
  if not (vTestes or vGab) then
  begin
    WriteLn('uso: formulario --testes | --gabarito [--lista] [--samples=<repo>]');
    Halt(2);
  end;
  Preparar;
  if vTestes then
    RodarTestes;
  if vGab then
    RodarGabarito;
  WriteLn;
  if GFalhas > 0 then
  begin
    WriteLn(GFalhas, ' falha(s)');
    Halt(1);
  end;
  WriteLn('tudo certo');
end.
