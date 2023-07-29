program RALStressTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  uPrincipal in 'src\Telas\uPrincipal.pas' {fPrincipal},
  uRESTDAO in 'src\DAO\uRESTDAO.pas',
  uConsts in 'src\uConsts.pas',
  uResultado in 'src\Telas\uResultado.pas' {fResultado},
  DAOBase in 'src\DAO\DAOBase.pas',
  TestUnit in 'src\TestUnit.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfPrincipal, fPrincipal);
  Application.Run;
end.
