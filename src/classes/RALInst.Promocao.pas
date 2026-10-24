/// The swap of compiled files into the IDE folders, all or nothing (F14). A run
/// compiles into a staging folder (<data>\preparo\...), checks that what it
/// compiled makes a working RAL, and only then promotes the files: the file
/// being replaced gets the '.anterior' suffix (renaming works with the IDE
/// closed, which the run already requires), the new one takes its place. When
/// a promotion or the registry write after it fails, Desfazer puts every
/// '.anterior' back; Confirmar deletes them at the end of a run that worked.
unit RALInst.Promocao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Processo;

type
  /// The files of one run that leave the staging folder for the IDE folders.
  TPromocao = class
  private
    FErro: string;
    /// destination=source, in the order they were added
    FItens: TStringList;
    FLog: TLogLinha;
    /// Destinations already promoted; Objects <> nil when an '.anterior' was
    /// kept for it
    FPromovidos: TStringList;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
  public
    constructor Create;
    destructor Destroy; override;
    /// A file to promote; the same destination twice keeps the last source
    procedure Adicionar(const AOrigem, ADestino: string);
    /// Deletes the '.anterior' files: the run worked and nothing goes back
    procedure Confirmar;
    /// Puts the replaced files back, newest promotion first
    procedure Desfazer;
    /// The destination of every file of the run
    procedure ListarDestinos(ALista: TStrings);
    /// Promotes everything; on the first failure it undoes what it did and
    /// returns False (Erro says why)
    function Promover: boolean;
    /// How many files the run would promote
    function Total: integer;

    /// Why Promover failed
    property Erro: string read FErro;
    property Log: TLogLinha read FLog write FLog;
  end;

const
  /// Suffix of the file a promotion replaces, until the run confirms
  SufixoAnterior = '.anterior';

/// Copies a file keeping its date (the receipts note size and date)
function CopiarArquivo(const AOrigem, ADestino: string): boolean;
/// FNV-1a of a text: a short stable name for a folder per IDE root
function HashTexto(const ATexto: string): cardinal;
/// Moves a file: a rename on the same volume, else copy and delete. False when
/// the destination could not be written (the source stays)
function MoverArquivo(const AOrigem, ADestino: string): boolean;
/// Staging folder of a run in one IDE: <data>\preparo\<name>
function PastaPreparo(const ANome: string): string;

implementation

uses
  RALInst.Mensagens;

function HashTexto(const ATexto: string): cardinal;
var
  vInt: integer;
begin
  Result := 2166136261;
  for vInt := 1 to Length(ATexto) do
  begin
    Result := Result xor Ord(ATexto[vInt]);
    {$PUSH}{$Q-}{$R-}
    Result := Result * 16777619;
    {$POP}
  end;
end;

function PastaPreparo(const ANome: string): string;
begin
  Result := IncludeTrailingPathDelimiter(PastaDadosInstalador + 'preparo' + PathDelim +
                                         ANome);
end;

function CopiarArquivo(const AOrigem, ADestino: string): boolean;
var
  vDe, vPara: TFileStream;
begin
  Result := False;
  try
    vDe := TFileStream.Create(AOrigem, fmOpenRead or fmShareDenyWrite);
    try
      vPara := TFileStream.Create(ADestino, fmCreate);
      try
        vPara.CopyFrom(vDe, 0);
      finally
        vPara.Free;
      end;
    finally
      vDe.Free;
    end;
    // a data e o que o recibo anota; a copia leva a do original
    FileSetDate(ADestino, FileAge(AOrigem));
    Result := True;
  except
    SysUtils.DeleteFile(ADestino);
  end;
end;

function MoverArquivo(const AOrigem, ADestino: string): boolean;
begin
  ForceDirectories(ExtractFileDir(ADestino));
  Result := RenameFile(AOrigem, ADestino);
  // outro volume (a pasta de dados no C:, a do Bpl em outro disco)
  if not Result and FileExists(AOrigem) and not FileExists(ADestino) then
  begin
    Result := CopiarArquivo(AOrigem, ADestino);
    if Result then
      SysUtils.DeleteFile(AOrigem);
  end;
end;

{ TPromocao }

constructor TPromocao.Create;
begin
  inherited Create;
  FItens := TStringList.Create;
  FItens.CaseSensitive := False;
  FPromovidos := TStringList.Create;
end;

destructor TPromocao.Destroy;
begin
  FPromovidos.Free;
  FItens.Free;
  inherited Destroy;
end;

procedure TPromocao.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

procedure TPromocao.Adicionar(const AOrigem, ADestino: string);
begin
  if (AOrigem = '') or (ADestino = '') then
    Exit;
  FItens.Values[ADestino] := AOrigem;
end;

function TPromocao.Total: integer;
begin
  Result := FItens.Count;
end;

function TPromocao.Promover: boolean;
var
  vInt: integer;
  vDestino, vOrigem, vAnterior: string;
  vGuardou: boolean;
begin
  Result := True;
  FErro := '';
  FPromovidos.Clear;
  for vInt := 0 to Pred(FItens.Count) do
  begin
    vDestino := FItens.Names[vInt];
    vOrigem := FItens.ValueFromIndex[vInt];
    if not FileExists(vOrigem) then
    begin
      FErro := Format(emPromoverSemArquivo, [vOrigem]);
      Break;
    end;
    vAnterior := vDestino + SufixoAnterior;
    vGuardou := False;
    if FileExists(vDestino) then
    begin
      // sobra de uma rodada que caiu no meio: o .anterior dela ja nao vale
      if FileExists(vAnterior) then
        SysUtils.DeleteFile(vAnterior);
      if not RenameFile(vDestino, vAnterior) then
      begin
        FErro := Format(emPromoverRenomear, [vDestino]);
        Break;
      end;
      vGuardou := True;
    end;
    if not MoverArquivo(vOrigem, vDestino) then
    begin
      FErro := Format(emPromoverGravar, [vDestino]);
      if vGuardou then
        RenameFile(vAnterior, vDestino);
      Break;
    end;
    FPromovidos.AddObject(vDestino, TObject(PtrInt(Ord(vGuardou))));
  end;
  if FErro <> '' then
  begin
    Logar(cmPrefixoErro + FErro);
    Desfazer;
    Result := False;
  end
  else if FPromovidos.Count > 0 then
    Logar(Format(cmPromovidos, [FPromovidos.Count]));
end;

procedure TPromocao.Desfazer;
var
  vInt: integer;
  vDestino: string;
begin
  for vInt := Pred(FPromovidos.Count) downto 0 do
  begin
    vDestino := FPromovidos[vInt];
    SysUtils.DeleteFile(vDestino);
    if FPromovidos.Objects[vInt] <> nil then
      if RenameFile(vDestino + SufixoAnterior, vDestino) then
        Logar(Format(cmPromocaoDesfeita, [vDestino]))
      else
        Logar(Format(emPromocaoDesfazer, [vDestino + SufixoAnterior]));
  end;
  FPromovidos.Clear;
end;

procedure TPromocao.ListarDestinos(ALista: TStrings);
var
  vInt: integer;
begin
  ALista.Clear;
  for vInt := 0 to Pred(FItens.Count) do
    ALista.Add(FItens.Names[vInt]);
end;

procedure TPromocao.Confirmar;
var
  vInt: integer;
begin
  for vInt := 0 to Pred(FPromovidos.Count) do
    if FPromovidos.Objects[vInt] <> nil then
      SysUtils.DeleteFile(FPromovidos[vInt] + SufixoAnterior);
  FPromovidos.Clear;
end;

end.
