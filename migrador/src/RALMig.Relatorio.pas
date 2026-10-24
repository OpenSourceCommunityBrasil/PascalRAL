/// Report of a migration (M7), in three blocks: what was done (per file, and
/// inside it per rule), what needs the user (each manual item: where, the
/// problem, what to do, the code ready to paste and the wiki link) and the
/// behaviour that changed. The same content goes out as Markdown and as a
/// self-contained HTML page (inline CSS, light and dark through
/// prefers-color-scheme, no script, user code escaped).
///
/// The fixed labels are resourcestrings of this unit (M8 translates them); the
/// text of each item already comes in the language of the result.
///
/// GravarRelatorio writes MIGRACAO-RAL-<from>-<to>.md and .html in the project
/// folder AFTER the migration was written: they are not part of the safety copy
/// and the undo neither restores nor deletes them (the user keeps the report of
/// a migration that was undone, or deletes it by hand).
unit RALMig.Relatorio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.Tipos;

const
  /// Where the pages named in the "wiki" field of a rule live
  URL_WIKI = 'https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/';

type
  /// Everything the report is made of
  TDadosRelatorio = record
    Resultado: TResultadoMigracao;
    /// The files of the run (changed list, ignored files); may be nil
    Conjunto: TConjuntoArquivos;
    /// Project folder; empty = the folder of Conjunto
    Pasta: string;
    /// Safety copy made by this run, empty when there is none
    ZipCopia: string;
    Data: TDateTime;
    /// True after the files were written; False for a preview
    Gravado: boolean;
  end;

/// Fills the record (Data = now)
function DadosRelatorio(AResultado: TResultadoMigracao; AConjunto: TConjuntoArquivos;
  const APasta, AZipCopia: string; AGravado: boolean): TDadosRelatorio;
/// URL of the wiki for 'pagina.md#ancora' (a full http(s) URL is kept as it is)
function LinkDaWiki(const AWiki: string): string;
/// 'MIGRACAO-RAL-<from>-<to>.<ext>' (AExtensao without the dot)
function NomeRelatorio(const ADe, APara, AExtensao: string): string;
/// One line with the counts (changed files, done, manual, behaviours, errors)
function LinhaResumo(const ADados: TDadosRelatorio): string;
function GerarMarkdown(const ADados: TDadosRelatorio): string;
function GerarHTML(const ADados: TDadosRelatorio): string;
/// Writes the .md and the .html in APasta (UTF-8, no BOM), replacing the ones
/// of the same versions. False (with AErro) when one could not be written
function GravarRelatorio(const APasta: string; const ADados: TDadosRelatorio;
  out AMarkdown, AHtml, AErro: string): boolean;

implementation

resourcestring
  rmTitulo = 'Migração do RAL %s para %s';
  rmPasta = 'Pasta';
  rmData = 'Data';
  rmVersoes = 'Versões';
  rmVersoesValor = 'RAL %s → RAL %s';
  rmArquivos = 'Arquivos alterados';
  rmContagens = 'Contagens';
  rmCopia = 'Cópia de segurança';
  rmDesfazer = 'Para desfazer';
  rmSemCopia = 'Sem cópia: não há como desfazer.';
  rmPrevia = 'Prévia: nada foi gravado.';
  rmNadaAlterado = 'Nenhum arquivo foi alterado.';
  rmResumo = '%d arquivo(s) alterado(s), %d alteração(ões), %d item(ns) para ' +
    'fazer à mão, %d comportamento(s) que mudou(aram), %d erro(s).';
  rmSecErros = 'Erros';
  rmSecFeito = 'Feito';
  rmSecManual = 'Precisa de você';
  rmSecComportamento = 'Comportamento que mudou';
  rmSecIgnorados = 'Arquivos não migrados';
  rmNenhumFeito = 'Nenhuma alteração foi feita.';
  rmNenhumManual = 'Nada para fazer à mão.';
  rmNenhumComportamento = 'Nenhum comportamento mudou entre estas versões.';
  rmAlteracoes = '%d alteração(ões)';
  rmLinha = 'linha %d';
  rmNovo = '(arquivo novo)';
  rmSemItem = 'Arquivo alterado.';
  rmProjeto = '(projeto)';
  rmProblema = 'Problema';
  rmFazer = 'O que fazer';
  rmTrecho = 'Trecho pronto';
  rmWiki = 'Wiki';
  rmRodape = 'Este relatório foi gravado depois da migração e não faz parte do ' +
    'desfazer: ele continua aqui depois de desfazer; apague-o à mão se quiser.';

const
  CSS_RELATORIO =
    ':root{color-scheme:light dark;--fg:#1b1f23;--bg:#fff;--mut:#586069;' +
    '--bd:#d0d7de;--cd:#f6f8fa;--ac:#0969da;--er:#cf222e;--erbg:#ffebe9}' +
    '@media (prefers-color-scheme:dark){:root{--fg:#e6edf3;--bg:#0d1117;' +
    '--mut:#9198a1;--bd:#30363d;--cd:#161b22;--ac:#4493f8;--er:#ff7b72;' +
    '--erbg:#3d1d20}}' +
    'body{margin:0;background:var(--bg);color:var(--fg);' +
    'font:15px/1.5 system-ui,-apple-system,"Segoe UI",Roboto,sans-serif}' +
    'main{max-width:60rem;margin:0 auto;padding:1.5rem 16px 3rem}' +
    'h1{font-size:1.6rem}' +
    'h2{border-bottom:1px solid var(--bd);padding-bottom:.3rem;margin-top:2rem}' +
    'h3{font-size:1.05rem;margin-top:1.4rem;overflow-wrap:anywhere}' +
    'pre{background:var(--cd);border:1px solid var(--bd);border-radius:6px;' +
    'padding:.75rem;overflow:auto}' +
    'code{font-family:ui-monospace,Consolas,monospace;font-size:.9em;' +
    'background:var(--cd);padding:.1em .3em;border-radius:4px;' +
    'overflow-wrap:anywhere}' +
    'pre code{background:none;padding:0;overflow-wrap:normal}' +
    'a{color:var(--ac)}ul{padding-left:1.4rem}' +
    '.rot{font-weight:600}.mut{color:var(--mut);font-size:.9em}' +
    '.erro{background:var(--erbg);border:1px solid var(--er);' +
    'border-radius:6px;padding:.5rem .75rem;margin:.5rem 0}';

{ Text helpers }

function QuebrasLF(const ATexto: string): string;
begin
  Result := StringReplace(ATexto, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function EscMd(const ATexto: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 1 to Length(ATexto) do
  begin
    if ATexto[vInt] in ['\', '`', '*', '_', '[', ']', '<', '>', '&', '|', '~'] then
      Result := Result + '\';
    Result := Result + ATexto[vInt];
  end;
end;

function CodigoMd(const ATexto: string): string;
begin
  if Pos('`', ATexto) > 0 then
    Result := '`` ' + ATexto + ' ``'
  else
    Result := '`' + ATexto + '`';
end;

function EscHtml(const ATexto: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 1 to Length(ATexto) do
    case ATexto[vInt] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
    else
      Result := Result + ATexto[vInt];
    end;
end;

function UmaLinha(const ATexto: string): string;
begin
  Result := Trim(StringReplace(QuebrasLF(ATexto), #10, ' ', [rfReplaceAll]));
end;

function EhHttp(const AUrl: string): boolean;
begin
  Result := (Copy(LowerCase(AUrl), 1, 8) = 'https://') or
            (Copy(LowerCase(AUrl), 1, 7) = 'http://');
end;

{ Writers: the same report goes through one of these }

type
  /// Format-neutral output of the report (headings, paragraphs, bullets...)
  TEscritor = class
  protected
    FSaida: string;
    procedure Add(const ATexto: string);
  public
    procedure Inicio(const ATitulo, AIdioma: string); virtual; abstract;
    procedure Fim; virtual; abstract;
    procedure Titulo(ANivel: integer; const ATexto: string); virtual; abstract;
    procedure Paragrafo(const ATexto: string); virtual; abstract;
    procedure ParagrafoRotulado(const ARotulo, ATexto: string); virtual; abstract;
    procedure Item(ANivel: integer; const ATexto: string); virtual; abstract;
    procedure ItemRotulado(const ARotulo, ATexto: string; AMono: boolean); virtual; abstract;
    procedure ItemMono(ANivel: integer; const AMono, ATexto: string); virtual; abstract;
    procedure Bloco(const ACodigo: string); virtual; abstract;
    procedure Ligacao(const ARotulo, AUrl: string); virtual; abstract;
    procedure Alerta(const ATexto: string); virtual; abstract;
    property Saida: string read FSaida;
  end;

  { TEscritorMarkdown }

  TEscritorMarkdown = class(TEscritor)
  private
    FNaLista: boolean;
    procedure FimLista;
  public
    procedure Inicio(const ATitulo, AIdioma: string); override;
    procedure Fim; override;
    procedure Titulo(ANivel: integer; const ATexto: string); override;
    procedure Paragrafo(const ATexto: string); override;
    procedure ParagrafoRotulado(const ARotulo, ATexto: string); override;
    procedure Item(ANivel: integer; const ATexto: string); override;
    procedure ItemRotulado(const ARotulo, ATexto: string; AMono: boolean); override;
    procedure ItemMono(ANivel: integer; const AMono, ATexto: string); override;
    procedure Bloco(const ACodigo: string); override;
    procedure Ligacao(const ARotulo, AUrl: string); override;
    procedure Alerta(const ATexto: string); override;
  end;

  { TEscritorHtml }

  TEscritorHtml = class(TEscritor)
  private
    FProfundidade: integer;
    procedure FecharListas;
    procedure AbrirItem(ANivel: integer; const AConteudo: string);
  public
    procedure Inicio(const ATitulo, AIdioma: string); override;
    procedure Fim; override;
    procedure Titulo(ANivel: integer; const ATexto: string); override;
    procedure Paragrafo(const ATexto: string); override;
    procedure ParagrafoRotulado(const ARotulo, ATexto: string); override;
    procedure Item(ANivel: integer; const ATexto: string); override;
    procedure ItemRotulado(const ARotulo, ATexto: string; AMono: boolean); override;
    procedure ItemMono(ANivel: integer; const AMono, ATexto: string); override;
    procedure Bloco(const ACodigo: string); override;
    procedure Ligacao(const ARotulo, AUrl: string); override;
    procedure Alerta(const ATexto: string); override;
  end;

procedure TEscritor.Add(const ATexto: string);
begin
  FSaida := FSaida + ATexto;
end;

{ TEscritorMarkdown }

procedure TEscritorMarkdown.FimLista;
begin
  if FNaLista then
    Add(#10);
  FNaLista := False;
end;

{$push}{$hints off}
procedure TEscritorMarkdown.Inicio(const ATitulo, AIdioma: string);
begin
  FSaida := '';
  FNaLista := False;
  Add('# ' + EscMd(UmaLinha(ATitulo)) + #10#10);
end;
{$pop}

procedure TEscritorMarkdown.Fim;
begin
  FimLista;
end;

procedure TEscritorMarkdown.Titulo(ANivel: integer; const ATexto: string);
begin
  FimLista;
  Add(StringOfChar('#', ANivel) + ' ' + EscMd(UmaLinha(ATexto)) + #10#10);
end;

procedure TEscritorMarkdown.Paragrafo(const ATexto: string);
begin
  FimLista;
  Add(StringReplace(EscMd(Trim(QuebrasLF(ATexto))), #10, '  ' + #10, [rfReplaceAll]) +
      #10#10);
end;

procedure TEscritorMarkdown.ParagrafoRotulado(const ARotulo, ATexto: string);
begin
  FimLista;
  Add('**' + EscMd(ARotulo) + ':** ' +
      StringReplace(EscMd(Trim(QuebrasLF(ATexto))), #10, '  ' + #10, [rfReplaceAll]) +
      #10#10);
end;

procedure TEscritorMarkdown.Item(ANivel: integer; const ATexto: string);
begin
  Add(StringOfChar(' ', 2 * ANivel) + '- ' + EscMd(UmaLinha(ATexto)) + #10);
  FNaLista := True;
end;

procedure TEscritorMarkdown.ItemRotulado(const ARotulo, ATexto: string; AMono: boolean);
var
  vTexto: string;
begin
  if AMono then
    vTexto := CodigoMd(UmaLinha(ATexto))
  else
    vTexto := EscMd(UmaLinha(ATexto));
  Add('- **' + EscMd(ARotulo) + ':** ' + vTexto + #10);
  FNaLista := True;
end;

procedure TEscritorMarkdown.ItemMono(ANivel: integer; const AMono, ATexto: string);
begin
  Add(StringOfChar(' ', 2 * ANivel) + '- ' + CodigoMd(UmaLinha(AMono)));
  if UmaLinha(ATexto) <> '' then
    Add(' ' + EscMd(UmaLinha(ATexto)));
  Add(#10);
  FNaLista := True;
end;

procedure TEscritorMarkdown.Bloco(const ACodigo: string);
var
  vCerca: string;
begin
  FimLista;
  vCerca := '```';
  while Pos(vCerca, ACodigo) > 0 do
    vCerca := vCerca + '`';
  Add(vCerca + 'pascal' + #10 + TrimRight(QuebrasLF(ACodigo)) + #10 + vCerca + #10#10);
end;

procedure TEscritorMarkdown.Ligacao(const ARotulo, AUrl: string);
var
  vUrl: string;
begin
  FimLista;
  if not EhHttp(AUrl) then
  begin
    Paragrafo(ARotulo + ': ' + AUrl);
    Exit;
  end;
  vUrl := StringReplace(AUrl, ' ', '%20', [rfReplaceAll]);
  vUrl := StringReplace(vUrl, '(', '%28', [rfReplaceAll]);
  vUrl := StringReplace(vUrl, ')', '%29', [rfReplaceAll]);
  Add('**' + EscMd(ARotulo) + ':** [' + EscMd(AUrl) + '](' + vUrl + ')' + #10#10);
end;

procedure TEscritorMarkdown.Alerta(const ATexto: string);
var
  vLinhas: TStringList;
  vInt: integer;
begin
  FimLista;
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := Trim(QuebrasLF(ATexto));
    for vInt := 0 to vLinhas.Count - 1 do
      Add('> ' + EscMd(vLinhas[vInt]) + #10);
  finally
    vLinhas.Free;
  end;
  Add(#10);
end;

{ TEscritorHtml }

procedure TEscritorHtml.FecharListas;
begin
  while FProfundidade > 0 do
  begin
    Add('</li></ul>' + #10);
    Dec(FProfundidade);
  end;
end;

procedure TEscritorHtml.AbrirItem(ANivel: integer; const AConteudo: string);
begin
  while FProfundidade > ANivel + 1 do
  begin
    Add('</li></ul>' + #10);
    Dec(FProfundidade);
  end;
  if FProfundidade = ANivel + 1 then
    Add('</li>' + #10);
  while FProfundidade < ANivel + 1 do
  begin
    Add('<ul>' + #10);
    Inc(FProfundidade);
  end;
  Add('<li>' + AConteudo);
end;

procedure TEscritorHtml.Inicio(const ATitulo, AIdioma: string);
var
  vIdioma: string;
begin
  FSaida := '';
  FProfundidade := 0;
  vIdioma := AIdioma;
  if vIdioma = '' then
    vIdioma := 'pt-BR';
  Add('<!DOCTYPE html>' + #10 + '<html lang="' + EscHtml(vIdioma) + '">' + #10 +
      '<head>' + #10 + '<meta charset="utf-8">' + #10 +
      '<meta name="viewport" content="width=device-width, initial-scale=1">' + #10 +
      '<title>' + EscHtml(UmaLinha(ATitulo)) + '</title>' + #10 +
      '<style>' + CSS_RELATORIO + '</style>' + #10 + '</head>' + #10 +
      '<body>' + #10 + '<main>' + #10 +
      '<h1>' + EscHtml(UmaLinha(ATitulo)) + '</h1>' + #10);
end;

procedure TEscritorHtml.Fim;
begin
  FecharListas;
  Add('</main>' + #10 + '</body>' + #10 + '</html>' + #10);
end;

procedure TEscritorHtml.Titulo(ANivel: integer; const ATexto: string);
begin
  FecharListas;
  Add(Format('<h%d>%s</h%d>', [ANivel, EscHtml(UmaLinha(ATexto)), ANivel]) + #10);
end;

procedure TEscritorHtml.Paragrafo(const ATexto: string);
begin
  FecharListas;
  Add('<p>' + StringReplace(EscHtml(Trim(QuebrasLF(ATexto))), #10, '<br>' + #10,
      [rfReplaceAll]) + '</p>' + #10);
end;

procedure TEscritorHtml.ParagrafoRotulado(const ARotulo, ATexto: string);
begin
  FecharListas;
  Add('<p><span class="rot">' + EscHtml(ARotulo) + ':</span> ' +
      StringReplace(EscHtml(Trim(QuebrasLF(ATexto))), #10, '<br>' + #10,
      [rfReplaceAll]) + '</p>' + #10);
end;

procedure TEscritorHtml.Item(ANivel: integer; const ATexto: string);
begin
  AbrirItem(ANivel, EscHtml(UmaLinha(ATexto)));
end;

procedure TEscritorHtml.ItemRotulado(const ARotulo, ATexto: string; AMono: boolean);
var
  vTexto: string;
begin
  vTexto := EscHtml(UmaLinha(ATexto));
  if AMono then
    vTexto := '<code>' + vTexto + '</code>';
  AbrirItem(0, '<span class="rot">' + EscHtml(ARotulo) + ':</span> ' + vTexto);
end;

procedure TEscritorHtml.ItemMono(ANivel: integer; const AMono, ATexto: string);
var
  vConteudo: string;
begin
  vConteudo := '<code>' + EscHtml(UmaLinha(AMono)) + '</code>';
  if UmaLinha(ATexto) <> '' then
    vConteudo := vConteudo + ' ' + EscHtml(UmaLinha(ATexto));
  AbrirItem(ANivel, vConteudo);
end;

procedure TEscritorHtml.Bloco(const ACodigo: string);
begin
  FecharListas;
  Add('<pre><code>' + EscHtml(TrimRight(QuebrasLF(ACodigo))) + '</code></pre>' + #10);
end;

procedure TEscritorHtml.Ligacao(const ARotulo, AUrl: string);
begin
  FecharListas;
  if EhHttp(AUrl) then
    Add('<p><span class="rot">' + EscHtml(ARotulo) + ':</span> <a href="' +
        EscHtml(AUrl) + '">' + EscHtml(AUrl) + '</a></p>' + #10)
  else
    Paragrafo(ARotulo + ': ' + AUrl);
end;

procedure TEscritorHtml.Alerta(const ATexto: string);
begin
  FecharListas;
  Add('<div class="erro">' + StringReplace(EscHtml(Trim(QuebrasLF(ATexto))), #10,
      '<br>' + #10, [rfReplaceAll]) + '</div>' + #10);
end;

{ Data of the report }

function DadosRelatorio(AResultado: TResultadoMigracao; AConjunto: TConjuntoArquivos;
  const APasta, AZipCopia: string; AGravado: boolean): TDadosRelatorio;
begin
  Result.Resultado := AResultado;
  Result.Conjunto := AConjunto;
  Result.Pasta := APasta;
  Result.ZipCopia := AZipCopia;
  Result.Data := Now;
  Result.Gravado := AGravado;
end;

function LinkDaWiki(const AWiki: string): string;
var
  vPagina, vAncora: string;
  vPos: integer;
begin
  Result := Trim(AWiki);
  if (Result = '') or EhHttp(Result) then
    Exit;
  vPagina := Result;
  vAncora := '';
  vPos := Pos('#', vPagina);
  if vPos > 0 then
  begin
    vAncora := Copy(vPagina, vPos, MaxInt);
    vPagina := Copy(vPagina, 1, vPos - 1);
  end;
  if LowerCase(Copy(vPagina, Length(vPagina) - 2, 3)) = '.md' then
    Delete(vPagina, Length(vPagina) - 2, 3);
  Result := URL_WIKI + StringReplace(vPagina, ' ', '-', [rfReplaceAll]) + vAncora;
end;

function NomeSeguro(const ATexto: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 1 to Length(ATexto) do
    if ATexto[vInt] in ['0'..'9', 'a'..'z', 'A'..'Z', '.', '_', '-'] then
      Result := Result + ATexto[vInt]
    else
      Result := Result + '_';
  if Result = '' then
    Result := 'x';
end;

function NomeRelatorio(const ADe, APara, AExtensao: string): string;
begin
  Result := 'MIGRACAO-RAL-' + NomeSeguro(ADe) + '-' + NomeSeguro(APara) + '.' + AExtensao;
end;

function PastaDoRelatorio(const ADados: TDadosRelatorio): string;
begin
  Result := ADados.Pasta;
  if (Result = '') and (ADados.Conjunto <> nil) then
    Result := ADados.Conjunto.Pasta;
end;

function ContarAlterados(const ADados: TDadosRelatorio): integer;
var
  vLista: TList;
begin
  Result := 0;
  if ADados.Conjunto = nil then
    Exit;
  vLista := TList.Create;
  try
    ADados.Conjunto.ListarAlterados(vLista);
    Result := vLista.Count;
  finally
    vLista.Free;
  end;
end;

/// Path with '/', relative to the project folder when it is inside it
function NomeRelativo(const ADados: TDadosRelatorio; const ACaminho: string): string;
var
  vPasta: string;
begin
  Result := StringReplace(ACaminho, '\', '/', [rfReplaceAll]);
  vPasta := PastaDoRelatorio(ADados);
  if vPasta = '' then
    Exit;
  vPasta := StringReplace(IncludeTrailingPathDelimiter(vPasta), '\', '/', [rfReplaceAll]);
  if SameText(Copy(Result, 1, Length(vPasta)), vPasta) then
    Result := Copy(Result, Length(vPasta) + 1, MaxInt);
end;

function LinhaResumo(const ADados: TDadosRelatorio): string;
var
  vRes: TResultadoMigracao;
begin
  vRes := ADados.Resultado;
  Result := Format(rmResumo, [ContarAlterados(ADados), vRes.FeitoCount,
    vRes.ManualCount, vRes.ComportamentoCount, vRes.Erros.Count]);
end;

{ Sections }

procedure SecaoCabecalho(const ADados: TDadosRelatorio; E: TEscritor);
var
  vRes: TResultadoMigracao;
  vPasta, vZip: string;
  vAlterados: integer;
begin
  vRes := ADados.Resultado;
  vPasta := PastaDoRelatorio(ADados);
  vAlterados := ContarAlterados(ADados);
  E.ItemRotulado(rmVersoes, Format(rmVersoesValor, [vRes.De, vRes.Para]), False);
  E.ItemRotulado(rmData, FormatDateTime('yyyy-mm-dd hh:nn', ADados.Data), False);
  E.ItemRotulado(rmPasta, vPasta, True);
  E.ItemRotulado(rmArquivos, IntToStr(vAlterados), False);
  E.ItemRotulado(rmContagens, LinhaResumo(ADados), False);
  if not ADados.Gravado then
    E.Item(0, rmPrevia)
  else if ADados.ZipCopia <> '' then
  begin
    vZip := NomeRelativo(ADados, ADados.ZipCopia);
    E.ItemRotulado(rmCopia, vZip, True);
    E.ItemRotulado(rmDesfazer, 'ralmigra desfazer "' + vPasta + '"', True);
  end
  else if vAlterados = 0 then
    E.Item(0, rmNadaAlterado)
  else
    E.Item(0, rmSemCopia);
end;

procedure SecaoErros(const ADados: TDadosRelatorio; E: TEscritor);
var
  vInt: integer;
begin
  if ADados.Resultado.Erros.Count = 0 then
    Exit;
  E.Titulo(2, rmSecErros);
  for vInt := 0 to ADados.Resultado.Erros.Count - 1 do
    E.Alerta(ADados.Resultado.Erros[vInt]);
end;

/// Files with something to tell: changed ones and the ones a rule reported
function NomesDeArquivos(const ADados: TDadosRelatorio): TStringList;
var
  vLista: TList;
  vInt: integer;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  Result.CaseSensitive := False;
  for vInt := 0 to ADados.Resultado.FeitoCount - 1 do
    Result.Add(NomeRelativo(ADados, ADados.Resultado.Feitos[vInt].Arquivo));
  if ADados.Conjunto = nil then
    Exit;
  vLista := TList.Create;
  try
    ADados.Conjunto.ListarAlterados(vLista);
    for vInt := 0 to vLista.Count - 1 do
      Result.Add(TArquivoMigrado(vLista[vInt]).Relativo);
  finally
    vLista.Free;
  end;
end;

procedure FeitosDaRegra(const ADados: TDadosRelatorio; E: TEscritor;
  const AArquivo, ARegra: string; ACount: integer);
var
  vInt: integer;
  vFeito: TItemFeito;
  vTexto: string;
begin
  E.ItemMono(0, ARegra, Format(rmAlteracoes, [ACount]));
  for vInt := 0 to ADados.Resultado.FeitoCount - 1 do
  begin
    vFeito := ADados.Resultado.Feitos[vInt];
    if not SameText(NomeRelativo(ADados, vFeito.Arquivo), AArquivo) or
       not SameText(vFeito.Regra, ARegra) then
      Continue;
    vTexto := vFeito.Descricao;
    if vFeito.Linha > 0 then
      vTexto := Format(rmLinha, [vFeito.Linha]) + ': ' + vTexto;
    E.Item(1, vTexto);
  end;
end;

procedure FeitosDoArquivo(const ADados: TDadosRelatorio; E: TEscritor;
  const AArquivo: string);
var
  vRegras: TStringList;
  vInt, vInd: integer;
  vFeito: TItemFeito;
  vArq: TArquivoMigrado;
  vTitulo: string;
begin
  vTitulo := AArquivo;
  vArq := nil;
  if ADados.Conjunto <> nil then
    vArq := ADados.Conjunto.Buscar(AArquivo);
  if (vArq <> nil) and vArq.Novo then
    vTitulo := vTitulo + ' ' + rmNovo;
  E.Titulo(3, vTitulo);
  vRegras := TStringList.Create;
  try
    vRegras.CaseSensitive := False;
    for vInt := 0 to ADados.Resultado.FeitoCount - 1 do
    begin
      vFeito := ADados.Resultado.Feitos[vInt];
      if not SameText(NomeRelativo(ADados, vFeito.Arquivo), AArquivo) then
        Continue;
      vInd := vRegras.IndexOf(vFeito.Regra);
      if vInd < 0 then
        vInd := vRegras.AddObject(vFeito.Regra, TObject(PtrInt(0)));
      vRegras.Objects[vInd] := TObject(PtrInt(vRegras.Objects[vInd]) + 1);
    end;
    if vRegras.Count = 0 then
      E.Paragrafo(rmSemItem);
    for vInt := 0 to vRegras.Count - 1 do
      FeitosDaRegra(ADados, E, AArquivo, vRegras[vInt], PtrInt(vRegras.Objects[vInt]));
  finally
    vRegras.Free;
  end;
end;

procedure SecaoFeito(const ADados: TDadosRelatorio; E: TEscritor);
var
  vNomes: TStringList;
  vInt: integer;
begin
  E.Titulo(2, rmSecFeito);
  vNomes := NomesDeArquivos(ADados);
  try
    if vNomes.Count = 0 then
      E.Paragrafo(rmNenhumFeito);
    for vInt := 0 to vNomes.Count - 1 do
      FeitosDoArquivo(ADados, E, vNomes[vInt]);
  finally
    vNomes.Free;
  end;
end;

/// Manual items ordered by file and line (indexes into the result)
function OrdemDosManuais(AResultado: TResultadoMigracao): TStringList;
var
  vInt: integer;
  vItem: TItemManual;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupAccept;
  for vInt := 0 to AResultado.ManualCount - 1 do
  begin
    vItem := AResultado.Manuais[vInt];
    Result.AddObject(LowerCase(vItem.Arquivo) + #1 + Format('%.8d', [vItem.Linha]) +
      #1 + Format('%.8d', [vInt]), TObject(PtrInt(vInt)));
  end;
end;

procedure ItemManual(const ADados: TDadosRelatorio; E: TEscritor; AItem: TItemManual);
var
  vOnde, vLink: string;
begin
  vOnde := NomeRelativo(ADados, AItem.Arquivo);
  if vOnde = '' then
    vOnde := rmProjeto
  else if AItem.Linha > 0 then
    vOnde := vOnde + ':' + IntToStr(AItem.Linha);
  E.Titulo(3, vOnde + ' - ' + AItem.Regra);
  E.ParagrafoRotulado(rmProblema, AItem.Problema);
  E.ParagrafoRotulado(rmFazer, AItem.Fazer);
  if Trim(AItem.Trecho) <> '' then
  begin
    E.Paragrafo(rmTrecho + ':');
    E.Bloco(AItem.Trecho);
  end;
  vLink := LinkDaWiki(AItem.Wiki);
  if vLink <> '' then
    E.Ligacao(rmWiki, vLink);
end;

procedure SecaoManual(const ADados: TDadosRelatorio; E: TEscritor);
var
  vOrdem: TStringList;
  vInt: integer;
begin
  E.Titulo(2, rmSecManual);
  if ADados.Resultado.ManualCount = 0 then
    E.Paragrafo(rmNenhumManual);
  vOrdem := OrdemDosManuais(ADados.Resultado);
  try
    for vInt := 0 to vOrdem.Count - 1 do
      ItemManual(ADados, E, ADados.Resultado.Manuais[PtrInt(vOrdem.Objects[vInt])]);
  finally
    vOrdem.Free;
  end;
end;

procedure SecaoComportamento(const ADados: TDadosRelatorio; E: TEscritor);
var
  vInt: integer;
  vItem: TItemComportamento;
  vLink: string;
begin
  E.Titulo(2, rmSecComportamento);
  if ADados.Resultado.ComportamentoCount = 0 then
    E.Paragrafo(rmNenhumComportamento);
  for vInt := 0 to ADados.Resultado.ComportamentoCount - 1 do
  begin
    vItem := ADados.Resultado.Comportamentos[vInt];
    E.Titulo(3, vItem.Id);
    E.Paragrafo(vItem.Descricao);
    if Trim(vItem.Fazer) <> '' then
      E.ParagrafoRotulado(rmFazer, vItem.Fazer);
    vLink := LinkDaWiki(vItem.Wiki);
    if vLink <> '' then
      E.Ligacao(rmWiki, vLink);
  end;
end;

procedure SecaoIgnorados(const ADados: TDadosRelatorio; E: TEscritor);
var
  vInt: integer;
  vArq: TArquivoMigrado;
  vTitulo: boolean;
begin
  if ADados.Conjunto = nil then
    Exit;
  vTitulo := False;
  for vInt := 0 to ADados.Conjunto.Count - 1 do
  begin
    vArq := ADados.Conjunto[vInt];
    if not vArq.Ignorado then
      Continue;
    if not vTitulo then
      E.Titulo(2, rmSecIgnorados);
    vTitulo := True;
    E.ItemMono(0, vArq.Relativo, vArq.Motivo);
  end;
end;

procedure MontarRelatorio(const ADados: TDadosRelatorio; E: TEscritor);
begin
  E.Inicio(Format(rmTitulo, [ADados.Resultado.De, ADados.Resultado.Para]),
           ADados.Resultado.Idioma);
  SecaoErros(ADados, E);
  SecaoCabecalho(ADados, E);
  SecaoFeito(ADados, E);
  SecaoManual(ADados, E);
  SecaoComportamento(ADados, E);
  SecaoIgnorados(ADados, E);
  if ADados.Gravado then
    E.Paragrafo(rmRodape);
  E.Fim;
end;

function GerarComEscritor(const ADados: TDadosRelatorio; E: TEscritor): string;
begin
  try
    MontarRelatorio(ADados, E);
    Result := StringReplace(E.Saida, #10, LineEnding, [rfReplaceAll]);
  finally
    E.Free;
  end;
end;

function GerarMarkdown(const ADados: TDadosRelatorio): string;
begin
  Result := GerarComEscritor(ADados, TEscritorMarkdown.Create);
end;

function GerarHTML(const ADados: TDadosRelatorio): string;
begin
  Result := GerarComEscritor(ADados, TEscritorHtml.Create);
end;

procedure GravarTexto(const ACaminho, ATexto: string);
var
  vFluxo: TFileStream;
begin
  vFluxo := TFileStream.Create(ACaminho, fmCreate);
  try
    if ATexto <> '' then
      vFluxo.WriteBuffer(ATexto[1], Length(ATexto));
  finally
    vFluxo.Free;
  end;
end;

function GravarRelatorio(const APasta: string; const ADados: TDadosRelatorio;
  out AMarkdown, AHtml, AErro: string): boolean;
begin
  Result := False;
  AErro := '';
  AMarkdown := IncludeTrailingPathDelimiter(APasta) +
    NomeRelatorio(ADados.Resultado.De, ADados.Resultado.Para, 'md');
  AHtml := IncludeTrailingPathDelimiter(APasta) +
    NomeRelatorio(ADados.Resultado.De, ADados.Resultado.Para, 'html');
  try
    GravarTexto(AMarkdown, GerarMarkdown(ADados));
    GravarTexto(AHtml, GerarHTML(ADados));
    Result := True;
  except
    on E: Exception do
      AErro := E.Message;
  end;
end;

end.
