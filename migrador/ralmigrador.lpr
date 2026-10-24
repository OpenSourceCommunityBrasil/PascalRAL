program ralmigrador;

{$mode objfpc}{$H+}

// Janela do migrador do RAL (etapa M8). Argumentos (o instalador chama com os tres
// primeiros, RALInst.Migrador.AbrirMigrador):
//   --de=<versao> --para=<versao>  versoes ja escolhidas (1.0, 1.1, 1.2, 2.0)
//   --idioma=pt-BR|en-US|es-ES     pula a tela de idioma
//   --pasta=<pasta ou projeto>     ja abre com esta pasta
//   --gerar-po                     escreve languages/ralmigrador.<idioma>.po e sai
//   --autoteste                    percorre as telas sem interacao e sai com 0 se
//                                  tudo correu bem (log em ralmigrador-autoteste.log
//                                  na pasta temporaria); nada e gravado no projeto
//
// Recursos embutidos (RCDATA) no .lpi: REGRAS_*, API_*, PO_* e as imagens do tema.

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  winpeimagereader,
  elfreader,
  machoreader,
  Classes, SysUtils, Forms,
  RALInst.Traducao,
  // as mensagens da console entram so para o gerador de .po coletar tambem as
  // delas: um .po serve aos dois executaveis
  RALMig.Console.Mensagens,
  RALMig.Acoes.Projeto, RALMig.Acoes.Formulario, RALMig.Acoes.Codigo,
  RALMig.Tela.Aplicar, RALMig.Tela.Conferir, RALMig.Tela.Estado,
  RALMig.Tela.GeradorPO, RALMig.Tela.Idioma, RALMig.Tela.Mensagens,
  RALMig.Tela.Modelo, RALMig.Tela.Previa, RALMig.Tela.Principal,
  RALMig.Tela.Projeto, RALMig.Tela.Versoes;

{$R *.res}

var
  GArgumentos: TArgumentosMig;
  GIdioma: string;

begin
  DefaultSystemCodePage := CP_UTF8;
  PrefixoPO := 'ralmigrador';
  RequireDerivedFormResource := False;
  GArgumentos := LerArgumentosMig;
  Application.Title := 'RAL Migrator';
  Application.Scaled := False;
  Application.Initialize;
  Application.CreateForm(TTelaMigPrincipal, TelaMigPrincipal);
  if GArgumentos.GerarPO then
  begin
    // coletar so vale a partir do portugues, que e o original de todo texto
    TraduzirMensagens(IdiomaOriginal);
    for GIdioma in IdiomasInstalador do
      TelaMigPrincipal.GerarPO(GIdioma);
    Exit;
  end;
  TelaMigPrincipal.Iniciar(GArgumentos);
  Application.Run;
end.
