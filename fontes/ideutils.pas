unit ideutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Forms,
  utools, RALInst.IDE, RALInst.Catalogo, RALInst.Receitas, RALInst.Compatibilidade;

type

  { TEscolhaInstalacao }

  // o que o usuario escolheu na tela de recursos; vale para todas as IDEs
  // marcadas (os nomes sao do tipo de IDE da rodada: IndyRAL no Delphi,
  // indyral no Lazarus)
  TEscolhaInstalacao = class
  private
    FCatalogo: TCatalogo;
    FPacotes: TStringList;
    FSomenteLibraryPath: boolean;
    FWin64: boolean;
    FPastaFontes: string;
    FReceitas: TReceitas;
    FPastasDependencias: TStringList;
    FManifesto: TManifesto;
  public
    constructor Create;
    destructor Destroy; override;

    // F9: os escolhidos que existem neste tipo de IDE, com o nome do catalogo
    // (numa rodada com Delphi e Lazarus, IndyRAL vale indyral no Lazarus, e o
    // que so existe de um lado nao vai para o outro)
    procedure PacotesDoTipo(ATipo: TTipoPacote; ALista: TStrings);

    // pertence a tela de recursos
    property Catalogo: TCatalogo read FCatalogo write FCatalogo;
    property Pacotes: TStringList read FPacotes;
    // Delphi: so library path, sem compilar nem instalar pacote
    property SomenteLibraryPath: boolean read FSomenteLibraryPath write FSomenteLibraryPath;
    // Delphi: tambem compila o runtime e o library path de Win64
    property Win64: boolean read FWin64 write FWin64;
    // onde os fontes do RAL estao, ou vao estar depois do download
    property PastaFontes: string read FPastaFontes write FPastaFontes;
    // F7: as receitas (pertencem a tela de recursos) e onde cada dependencia
    // baixada esta, ou vai estar (nome@versao=pasta, ChaveDependencia)
    property Receitas: TReceitas read FReceitas write FReceitas;
    property PastasDependencias: TStringList read FPastasDependencias;
    // F6: o manifesto da versao escolhida (pertence a tela de recursos)
    property Manifesto: TManifesto read FManifesto write FManifesto;
  end;

  { TIDEObjectData }

  // casca de UI sobre uma TIDEInstance do nucleo: icone, log na tela e o
  // motor de instalacao de cada tipo de IDE. A instancia pertence a lista da
  // busca, nao a esta classe.
  TIDEObjectData = class
  private
    FInstancia: TIDEInstance;
    FIcon: TGraphic;
    FLog: TMemo;
    FResumo: string;
    function GetBuildFile: string;
    function GetExeFile: string;
    function GetName: string;
    function GetVersion: string;
  protected
    // destino das linhas do nucleo (TLogLinha)
    procedure LogarLinha(const ALinha: string);
    // F9: a linha do relatorio final: o que entrou, o que ficou de fora e por que
    procedure Resumir(AOk: boolean; AAvisos: TStrings);
    function Install(AEscolha: TEscolhaInstalacao): boolean; virtual;
  public
    constructor Create(AInstancia: TIDEInstance); virtual;
    destructor Destroy; override;

    // o que a instalacao vai fazer nesta IDE, sem fazer nada
    function Plano(AEscolha: TEscolhaInstalacao): string; virtual;
    // onde a dependencia ja esta nesta IDE ('' se nao esta ou se a receita nao
    // serve a este tipo de IDE)
    function DependenciaInstalada(AReceita: TReceita; AEscolha: TEscolhaInstalacao): string; virtual;
    // a raiz da copia instalada da dependencia ('' se nao se sabe)
    function PastaDependencia(AReceita: TReceita; AEscolha: TEscolhaInstalacao): string; virtual;
    function InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;

    property Instancia: TIDEInstance read FInstancia;
    // o resumo da ultima instalacao nesta IDE (uma ou mais linhas)
    property Resumo: string read FResumo write FResumo;
    property Version: string read GetVersion;
    property BuildFile: string read GetBuildFile;
    property ExeFile: string read GetExeFile;
    property Name: string read GetName;
    property Icon: TGraphic read FIcon;
  end;

  TIDEObjectDataClass = class of TIDEObjectData;

implementation

{ TEscolhaInstalacao }

constructor TEscolhaInstalacao.Create;
begin
  inherited Create;
  FPacotes := TStringList.Create;
  FPacotes.CaseSensitive := False;
  FPastasDependencias := TStringList.Create;
  FPastasDependencias.CaseSensitive := False;
end;

destructor TEscolhaInstalacao.Destroy;
begin
  FPastasDependencias.Free;
  FPacotes.Free;
  inherited Destroy;
end;

procedure TEscolhaInstalacao.PacotesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
var
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  for vInt := 0 to Pred(FPacotes.Count) do
  begin
    vPacote := nil;
    if FCatalogo <> nil then
      vPacote := FCatalogo.Buscar(ATipo, FPacotes[vInt]);
    if (vPacote <> nil) and (ALista.IndexOf(vPacote.Nome) < 0) then
      ALista.Add(vPacote.Nome);
  end;
end;

{ TIDEObjectData }

function TIDEObjectData.GetBuildFile: string;
begin
  Result := FInstancia.BuildFile;
end;

function TIDEObjectData.GetExeFile: string;
begin
  Result := FInstancia.ExeFile;
end;

function TIDEObjectData.GetName: string;
begin
  Result := FInstancia.Nome;
end;

function TIDEObjectData.GetVersion: string;
begin
  Result := FInstancia.Versao;
end;

procedure TIDEObjectData.LogarLinha(const ALinha: string);
begin
  if FLog = nil then
    Exit;
  FLog.Lines.Add(ALinha);
  Application.ProcessMessages;
end;

procedure TIDEObjectData.Resumir(AOk: boolean; AAvisos: TStrings);
var
  vInt: integer;
begin
  if AOk then
    FResumo := Name + ': instalado'
  else
    FResumo := Name + ': terminou com erro (veja o log acima)';
  if AAvisos <> nil then
    for vInt := 0 to Pred(AAvisos.Count) do
      FResumo := FResumo + LineEnding + '    ' + AAvisos[vInt];
end;

function TIDEObjectData.Install(AEscolha: TEscolhaInstalacao): boolean;
begin
  LogarLinha('ERRO: instalação ainda não suportada nesta IDE: ' + Name);
  Result := False;
end;

function TIDEObjectData.Plano(AEscolha: TEscolhaInstalacao): string;
begin
  Result := Name + ': instalação ainda não suportada';
end;

function TIDEObjectData.PastaDependencia(AReceita: TReceita;
  AEscolha: TEscolhaInstalacao): string;
begin
  Result := '';
end;

function TIDEObjectData.DependenciaInstalada(AReceita: TReceita;
  AEscolha: TEscolhaInstalacao): string;
begin
  Result := '';
end;

constructor TIDEObjectData.Create(AInstancia: TIDEInstance);
begin
  inherited Create;
  FInstancia := AInstancia;
  if FInstancia.ExeFile <> '' then
    FIcon := GetIconExeFile(FInstancia.ExeFile)
  else
    FIcon := GetIconExeFile(FInstancia.BuildFile);
end;

destructor TIDEObjectData.Destroy;
begin
  FreeAndNil(FIcon);
  inherited Destroy;
end;

function TIDEObjectData.InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
begin
  FLog := ALog;
  FResumo := '';
  try
    LogarLinha('==== ' + Name + ' (' + ExcludeTrailingPathDelimiter(FInstancia.RootDir) + ')');
    Result := Install(AEscolha);
    if FResumo = '' then
      Resumir(Result, nil);
    if Result then
      LogarLinha('==== ' + Name + ': concluído')
    else
      LogarLinha('==== ' + Name + ': terminou com erro (detalhes acima)');
    LogarLinha('');
  finally
    FLog := nil;
  end;
end;

end.
