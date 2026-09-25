program ralinstaller;

{$mode objfpc}{$H+}

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
  Forms,
  {$IFDEF MSWINDOWS}
  RALInst.Tela.Delphi,
  {$ENDIF}
  RALInst.Tela.GeradorPO,
  RALInst.Tela.IDE,
  RALInst.Tela.Idioma,
  RALInst.Tela.Imagens,
  RALInst.Tela.Instalacao,
  RALInst.Tela.Instalar,
  RALInst.Tela.ItemIDE,
  RALInst.Tela.Lazarus,
  RALInst.Tela.Mensagens,
  RALInst.Tela.Modelo,
  RALInst.Tela.Principal,
  RALInst.Tela.Recursos,
  RALInst.Tela.Tarefa,
  RALInst.Tela.Temas,
  RALInst.Tela.Traducao,
  RALInst.Tela.VersoesIDE;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'RAL Installer';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TImagens, Imagens);
  Application.CreateForm(TTelaPrincipal, TelaPrincipal);
  Application.Run;
end.