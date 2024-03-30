program RALTestServerDelphi;

uses
  System.StartUpCopy,
  FMX.Forms,
  ServerTest in 'src\ServerTest.pas' {Form1},
  uRotas in 'src\uRotas.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
