unit ideutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Forms,
  utools, RALInst.IDE, RALInst.Catalogo;

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
  public
    constructor Create;
    destructor Destroy; override;

    // pertence a tela de recursos
    property Catalogo: TCatalogo read FCatalogo write FCatalogo;
    property Pacotes: TStringList read FPacotes;
    // Delphi: so library path, sem compilar nem instalar pacote
    property SomenteLibraryPath: boolean read FSomenteLibraryPath write FSomenteLibraryPath;
    // Delphi: tambem compila o runtime e o library path de Win64
    property Win64: boolean read FWin64 write FWin64;
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
    function GetBuildFile: string;
    function GetExeFile: string;
    function GetName: string;
    function GetVersion: string;
  protected
    // destino das linhas do nucleo (TLogLinha)
    procedure LogarLinha(const ALinha: string);
    function Install(AEscolha: TEscolhaInstalacao): boolean; virtual;
  public
    constructor Create(AInstancia: TIDEInstance); virtual;
    destructor Destroy; override;

    // o que a instalacao vai fazer nesta IDE, sem fazer nada
    function Plano(AEscolha: TEscolhaInstalacao): string; virtual;
    function InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;

    property Instancia: TIDEInstance read FInstancia;
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
end;

destructor TEscolhaInstalacao.Destroy;
begin
  FPacotes.Free;
  inherited Destroy;
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

function TIDEObjectData.Install(AEscolha: TEscolhaInstalacao): boolean;
begin
  LogarLinha('ERRO: instalação ainda não suportada nesta IDE: ' + Name);
  Result := False;
end;

function TIDEObjectData.Plano(AEscolha: TEscolhaInstalacao): string;
begin
  Result := Name + ': instalação ainda não suportada';
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
  try
    LogarLinha('==== ' + Name + ' (' + ExcludeTrailingPathDelimiter(FInstancia.RootDir) + ')');
    Result := Install(AEscolha);
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
