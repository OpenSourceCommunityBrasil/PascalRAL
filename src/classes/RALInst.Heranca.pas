/// The passage from RAL 1.x to 2.0. 2.0 renamed packages and moved features out
/// of the core into packages of their own; an IDE updated from 1.x keeps what it
/// had only if those packages come marked. This table says, for each package a
/// 1.x install may carry, which 2.0 packages now hold its features. It is data
/// for one passage only: a 2.0 install never goes through it.
unit RALInst.Heranca;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo;

/// True when the RAL tree at ARaiz is a 1.x one (src/base/RALServer.pas).
function ArvoreDa1x(const ARaiz: string): boolean;
/// The 2.0 packages that hold what ANome had in 1.x (Delphi and Lazarus names
/// alike, case-insensitive); empty when ANome is not a 1.x package.
procedure HerdeirosDe(const ANome: string; ALista: TStrings);
{ Adds to ADestino, once each, the heirs of ANomes that the catalog has, when
  the installation is a 1.x one: its tree is 1.x, or it holds a package 2.0 no
  longer has. A 2.0 installation adds nothing. }
procedure IncluirHerdeiros(const AFontes: string; ANomes: TStrings;
  ACatalogo: TCatalogo; ATipo: TTipoPacote; ADestino: TStrings);
/// True when AFontes is a 1.x tree or ANomes holds a 1.x package the catalog lacks
function InstalacaoDa1x(const AFontes: string; ANomes: TStrings;
  ACatalogo: TCatalogo; ATipo: TTipoPacote): boolean;
/// What ANome became in 2.0, comma-separated; '' when it has no heir
function TrocadoPor(const ANome: string): string;
/// Every package name a 1.x RAL had, so that one is still recognized as RAL's
/// after its tree is gone.
procedure NomesDa1x(ALista: TStrings);

implementation

type
  THeranca = record
    Antigo: string;
    Novos: string;
  end;

const
  { o nucleo da 1.x tinha os storages, a autenticacao, o certificado, o Web e o
    Swagger; os pacotes renomeados viram o novo e, onde a 2.0 separou o design,
    o par. No Delphi so o PascalRALDsgn fica nos Known Packages (o PascalRAL e
    de runtime), por isso os dois levam os mesmos herdeiros }
  HERANCAS: array [0 .. 25] of THeranca = (
    (Antigo: 'PascalRAL'; Novos: 'RALStoragePackage,RALPluginAuthPackage,' +
      'RALPluginTLSPackage,RALModuleWebPackage,RALModuleSwaggerPackage'),
    (Antigo: 'PascalRALDsgn'; Novos: 'RALStoragePackage,RALPluginAuthPackage,' +
      'RALPluginTLSPackage,RALModuleWebPackage,RALModuleSwaggerPackage'),
    (Antigo: 'RALDBPackage'; Novos: 'RALDBDsgn'),
    (Antigo: 'RALDBFireDACLink'; Novos: 'RALDBFireDACPackage,RALDBFireDACDsgn'),
    (Antigo: 'RALDBFireDACObjects'; Novos: 'RALDBFireDACDAOPackage'),
    (Antigo: 'RALDBZeosLink'; Novos: 'RALDBZeosPackage,RALDBZeosDsgn'),
    (Antigo: 'raldbsqldblink'; Novos: 'raldbsqldbpackage,raldbsqldbdsgn'),
    (Antigo: 'RALBSONStorage'; Novos: 'RALStorageBSONPackage'),
    (Antigo: 'raldbbson'; Novos: 'RALStorageBSONPackage'),
    (Antigo: 'RALZStdCompress'; Novos: 'RALCompressZStdPackage'),
    (Antigo: 'RALBrotliCompress'; Novos: 'RALCompressBrotliPackage'),
    (Antigo: 'RALXMLPackage'; Novos: 'RALModuleXMLPackage'),
    (Antigo: 'RALSOAPPackage'; Novos: 'RALModuleSOAPPackage,RALModuleSOAPDsgn'),
    (Antigo: 'RALWizard'; Novos: 'RALWizardDsgn'),
    (Antigo: 'ralwizardpack'; Novos: 'RALWizardDsgn'),
    (Antigo: 'IndyRAL'; Novos: ''),
    (Antigo: 'SynopseRAL'; Novos: ''),
    (Antigo: 'SaguiRAL'; Novos: ''),
    (Antigo: 'MsQuicRAL'; Novos: ''),
    (Antigo: 'NetHttpRAL'; Novos: ''),
    (Antigo: 'OkHttpRAL'; Novos: ''),
    (Antigo: 'KwikRAL'; Novos: ''),
    (Antigo: 'UniGUIRAL'; Novos: ''),
    (Antigo: 'XSocketRAL'; Novos: ''),
    (Antigo: 'fphttpral'; Novos: ''),
    (Antigo: 'cgiral'; Novos: ''));

function ArvoreDa1x(const ARaiz: string): boolean;
begin
  Result := (ARaiz <> '') and FileExists(IncludeTrailingPathDelimiter(ARaiz) +
    'src' + PathDelim + 'base' + PathDelim + 'RALServer.pas');
end;

procedure HerdeirosDe(const ANome: string; ALista: TStrings);
var
  vInt: integer;
  vNovos: TStringList;
begin
  ALista.Clear;
  for vInt := Low(HERANCAS) to High(HERANCAS) do
    if SameText(HERANCAS[vInt].Antigo, ANome) then
    begin
      vNovos := TStringList.Create;
      try
        vNovos.StrictDelimiter := True;
        vNovos.CommaText := HERANCAS[vInt].Novos;
        ALista.AddStrings(vNovos);
      finally
        vNovos.Free;
      end;
      Exit;
    end;
end;

procedure IncluirHerdeiros(const AFontes: string; ANomes: TStrings;
  ACatalogo: TCatalogo; ATipo: TTipoPacote; ADestino: TStrings);
var
  vHerdeiros: TStringList;
  vNome, vHerdeiro: integer;
  vPacote: TPacote;
begin
  if not InstalacaoDa1x(AFontes, ANomes, ACatalogo, ATipo) then
    Exit;
  vHerdeiros := TStringList.Create;
  try
    for vNome := 0 to Pred(ANomes.Count) do
    begin
      HerdeirosDe(ANomes[vNome], vHerdeiros);
      for vHerdeiro := 0 to Pred(vHerdeiros.Count) do
      begin
        vPacote := ACatalogo.Buscar(ATipo, vHerdeiros[vHerdeiro]);
        if (vPacote <> nil) and (ADestino.IndexOf(vPacote.Nome) < 0) then
          ADestino.Add(vPacote.Nome);
      end;
    end;
  finally
    vHerdeiros.Free;
  end;
end;

function InstalacaoDa1x(const AFontes: string; ANomes: TStrings;
  ACatalogo: TCatalogo; ATipo: TTipoPacote): boolean;
var
  vInt, vHeranca: integer;
begin
  Result := ArvoreDa1x(AFontes);
  if Result or (ANomes = nil) or (ACatalogo = nil) then
    Exit;
  // a pasta ja tem a 2.0, a IDE ainda os nomes da 1.x
  for vInt := 0 to Pred(ANomes.Count) do
    if ACatalogo.Buscar(ATipo, ANomes[vInt]) = nil then
      for vHeranca := Low(HERANCAS) to High(HERANCAS) do
        if SameText(HERANCAS[vHeranca].Antigo, ANomes[vInt]) then
          Exit(True);
end;

function TrocadoPor(const ANome: string): string;
var
  vNovos: TStringList;
begin
  vNovos := TStringList.Create;
  try
    HerdeirosDe(ANome, vNovos);
    Result := StringReplace(vNovos.CommaText, ',', ', ', [rfReplaceAll]);
  finally
    vNovos.Free;
  end;
end;

procedure NomesDa1x(ALista: TStrings);
var
  vInt: integer;
begin
  ALista.Clear;
  for vInt := Low(HERANCAS) to High(HERANCAS) do
    ALista.Add(HERANCAS[vInt].Antigo);
end;

end.
