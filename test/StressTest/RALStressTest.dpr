program RALStressTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  uPrincipal in 'src\Telas\uPrincipal.pas' {fPrincipal},
  DAO.REST in 'src\DAO\DAO.REST.pas',
  DAO.Base in 'src\DAO\DAO.Base.pas',
  TestUnit in 'src\Classes\TestUnit.pas',
  DAO.RALIndy in 'src\DAO\DAO.RALIndy.pas',
  DAO.RALSynopse in 'src\DAO\DAO.RALSynopse.pas',
  DAO.RALNetHttp in 'src\DAO\DAO.RALNetHttp.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfPrincipal, fPrincipal);
  Application.Run;
end.
