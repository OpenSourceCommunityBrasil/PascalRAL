/// The project migrator (PLANO_MIGRADOR.md), seen from the installer: where its
/// executable is and how to open it after a run that changed the RAL version of
/// an IDE (F21). The migrator is another program, with another life cycle (it
/// changes with every RAL break), shipped in the same release as the installer
/// and kept beside it: the installer's self-update swaps both.
unit RALInst.Migrador;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

/// The migrator's executable beside this program: the release name
/// (ralmigrador_w64.exe) or the plain one (ralmigrador.exe); '' when neither
/// is there
function ArquivoMigrador: string;
/// Where the migrator goes beside this program when it is not there yet (the
/// release name of this system)
function DestinoMigrador: string;
/// Opens the migrator window already with the source and the target version
/// ('1.1', '2.0') and the language ('pt-BR'); False (and the reason) when it
/// is not there or does not start
function AbrirMigrador(const ADe, APara, AIdioma: string; out AErro: string): boolean;

implementation

uses
  Process,
  RALInst.Mensagens, RALInst.Versao;

function DestinoMigrador: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + AssetDestaPlataforma(ProgramaMigrador);
end;

function ArquivoMigrador: string;
begin
  Result := DestinoMigrador;
  if FileExists(Result) then
    Exit;
  Result := ExtractFilePath(ParamStr(0)) + ProgramaMigrador + ExtensaoExecutavel;
  if not FileExists(Result) then
    Result := '';
end;

function AbrirMigrador(const ADe, APara, AIdioma: string; out AErro: string): boolean;
var
  vProcesso: TProcess;
begin
  Result := False;
  AErro := '';
  if ArquivoMigrador = '' then
  begin
    AErro := Format(emMigradorAusente, [ExtractFilePath(ParamStr(0))]);
    Exit;
  end;
  vProcesso := TProcess.Create(nil);
  try
    vProcesso.Executable := ArquivoMigrador;
    if ADe <> '' then
      vProcesso.Parameters.Add('--de=' + ADe);
    if APara <> '' then
      vProcesso.Parameters.Add('--para=' + APara);
    if AIdioma <> '' then
      vProcesso.Parameters.Add('--idioma=' + AIdioma);
    // a janela do migrador segue sozinha: o instalador nao espera
    vProcesso.Options := [];
    try
      vProcesso.Execute;
      Result := True;
    except
      on E: Exception do
        AErro := Format(emMigradorAbrir, [E.Message]);
    end;
  finally
    vProcesso.Free;
  end;
end;

end.
