unit RALInst.Tar;

{$mode ObjFPC}{$H+}

// Extracao de .tar.gz (.tgz), sem LCL — os estaticos do mORMot2 vem assim.
//
// Entende o tar ustar (nome + prefixo), os nomes longos do GNU tar (tipo 'L')
// e os cabecalhos pax (tipo 'x', com path=). Link simbolico so e criado fora
// do Windows; outros tipos (dispositivo, fifo) sao pulados. Entrada com '..'
// ou caminho absoluto e recusada: o arquivo vem da rede.

interface

uses
  Classes, SysUtils;

// ARetirarNiveis tira as primeiras pastas de cada caminho (1 = a pasta de topo)
function ExtrairTgz(const AArquivo, ADestino: string; ARetirarNiveis: integer;
  out AErro: string): boolean;

implementation

uses
  gzio, RALInst.Processo{$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  TCabecalhoTar = packed record
    Nome: array[0..99] of AnsiChar;
    Modo: array[0..7] of AnsiChar;
    Uid: array[0..7] of AnsiChar;
    Gid: array[0..7] of AnsiChar;
    Tamanho: array[0..11] of AnsiChar;
    Data: array[0..11] of AnsiChar;
    Soma: array[0..7] of AnsiChar;
    Tipo: AnsiChar;
    Link: array[0..99] of AnsiChar;
    Magia: array[0..5] of AnsiChar;
    Versao: array[0..1] of AnsiChar;
    Usuario: array[0..31] of AnsiChar;
    Grupo: array[0..31] of AnsiChar;
    DevMaior: array[0..7] of AnsiChar;
    DevMenor: array[0..7] of AnsiChar;
    Prefixo: array[0..154] of AnsiChar;
    Resto: array[0..11] of AnsiChar;
  end;

function Campo(const ABuf: array of AnsiChar): string;
var
  vTam: integer;
begin
  vTam := 0;
  while (vTam <= High(ABuf)) and (ABuf[vTam] <> #0) do
    Inc(vTam);
  SetString(Result, PAnsiChar(@ABuf[0]), vTam);
end;

function Octal(const ABuf: array of AnsiChar): int64;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := 0 to High(ABuf) do
    if ABuf[vInt] in ['0'..'7'] then
      Result := Result * 8 + (Ord(ABuf[vInt]) - Ord('0'))
    else if (ABuf[vInt] = #0) or ((ABuf[vInt] = ' ') and (Result > 0)) then
      Break;
end;

function LerTudo(AGz: gzFile; ATamanho: int64; ADestino: TStream): boolean;
var
  vBuf: array[0..65535] of byte;
  vFalta, vBlocos, vPedir, vLidos: int64;
begin
  // o conteudo ocupa blocos de 512; o que passa do tamanho e enchimento
  vBlocos := ((ATamanho + 511) div 512) * 512;
  vFalta := ATamanho;
  Result := True;
  while vBlocos > 0 do
  begin
    vPedir := vBlocos;
    if vPedir > SizeOf(vBuf) then
      vPedir := SizeOf(vBuf);
    vLidos := gzread(AGz, @vBuf[0], vPedir);
    if vLidos <= 0 then
      Exit(False);
    if (ADestino <> nil) and (vFalta > 0) then
      if vLidos <= vFalta then
        ADestino.WriteBuffer(vBuf[0], vLidos)
      else
        ADestino.WriteBuffer(vBuf[0], vFalta);
    Dec(vFalta, vLidos);
    if vFalta < 0 then
      vFalta := 0;
    Dec(vBlocos, vLidos);
  end;
end;

function RetirarNiveis(const ACaminho: string; ANiveis: integer): string;
var
  vInt, vPos: integer;
begin
  Result := StringReplace(ACaminho, '\', '/', [rfReplaceAll]);
  while Copy(Result, 1, 2) = './' do
    Delete(Result, 1, 2);
  for vInt := 1 to ANiveis do
  begin
    vPos := Pos('/', Result);
    if vPos = 0 then
      Exit('');
    Delete(Result, 1, vPos);
  end;
end;

function ExtrairTgz(const AArquivo, ADestino: string; ARetirarNiveis: integer;
  out AErro: string): boolean;
var
  vGz: gzFile;
  vCab: TCabecalhoTar;
  vLidos: integer;
  vNome, vNomeLongo, vRel, vAlvo, vPax, vLinha, vDestino: string;
  vTamanho: int64;
  vSaida: TFileStream;
  vTexto: TStringStream;
  vPos: integer;
  vVazios: integer;
begin
  Result := False;
  AErro := '';
  vDestino := IncludeTrailingPathDelimiter(ADestino);
  ForceDirectories(vDestino);

  vGz := gzopen(AArquivo, 'rb');
  if vGz = nil then
  begin
    AErro := 'não foi possível abrir ' + AArquivo;
    Exit;
  end;
  try
    vNomeLongo := '';
    vVazios := 0;
    repeat
      vLidos := gzread(vGz, @vCab, SizeOf(vCab));
      if vLidos < SizeOf(vCab) then
        Break;
      // dois blocos zerados marcam o fim
      if vCab.Nome[0] = #0 then
      begin
        Inc(vVazios);
        if vVazios >= 2 then
          Break;
        Continue;
      end;
      vVazios := 0;

      vTamanho := Octal(vCab.Tamanho);
      vNome := Campo(vCab.Nome);
      if (Campo(vCab.Magia) = 'ustar') and (Campo(vCab.Prefixo) <> '') then
        vNome := Campo(vCab.Prefixo) + '/' + vNome;
      if vNomeLongo <> '' then
      begin
        vNome := vNomeLongo;
        vNomeLongo := '';
      end;

      case vCab.Tipo of
        'L', 'x':
          begin
            // o nome longo (ou o pax com path=) vale para a proxima entrada
            vTexto := TStringStream.Create('');
            try
              if not LerTudo(vGz, vTamanho, vTexto) then
                raise Exception.Create('tar truncado');
              if vCab.Tipo = 'L' then
              begin
                // o nome vem terminado em #0
                vNomeLongo := vTexto.DataString;
                vNomeLongo := Copy(vNomeLongo, 1, Pos(#0, vNomeLongo + #0) - 1);
              end
              else
              begin
                // registros "<tamanho> chave=valor\n"
                vPax := vTexto.DataString;
                while vPax <> '' do
                begin
                  vPos := Pos(#10, vPax);
                  if vPos = 0 then
                    vPos := Length(vPax) + 1;
                  vLinha := Copy(vPax, 1, vPos - 1);
                  Delete(vPax, 1, vPos);
                  vLinha := Copy(vLinha, Pos(' ', vLinha) + 1, MaxInt);
                  if Copy(vLinha, 1, 5) = 'path=' then
                    vNomeLongo := Copy(vLinha, 6, MaxInt);
                end;
              end;
            finally
              vTexto.Free;
            end;
            Continue;
          end;
      end;

      vRel := RetirarNiveis(vNome, ARetirarNiveis);
      if (vRel <> '') and ((Pos('..', vRel) > 0) or (vRel[1] = '/') or (Pos(':', vRel) > 0)) then
      begin
        AErro := 'entrada suspeita no tar, ignorada: ' + vNome;
        vRel := '';
      end;
      vAlvo := vDestino + StringReplace(vRel, '/', PathDelim, [rfReplaceAll]);

      case vCab.Tipo of
        '0', #0, '7':
          if vRel = '' then
            LerTudo(vGz, vTamanho, nil)
          else
          begin
            CriarPastas(ExtractFilePath(vAlvo));
            vSaida := TFileStream.Create(CaminhoLongo(vAlvo), fmCreate);
            try
              if not LerTudo(vGz, vTamanho, vSaida) then
                raise Exception.Create('tar truncado em ' + vNome);
            finally
              vSaida.Free;
            end;
            {$IFDEF UNIX}
            FpChmod(vAlvo, Octal(vCab.Modo));
            {$ENDIF}
          end;
        '5':
          if vRel <> '' then
            CriarPastas(vAlvo);
        '2':
          begin
            {$IFDEF UNIX}
            if vRel <> '' then
            begin
              ForceDirectories(ExtractFilePath(vAlvo));
              FpSymlink(PChar(Campo(vCab.Link)), PChar(vAlvo));
            end;
            {$ENDIF}
          end;
      else
        // link fisico, dispositivo, pax global, ...: o conteudo e pulado
        LerTudo(vGz, vTamanho, nil);
      end;
    until False;
    Result := True;
  except
    on E: Exception do
      AErro := 'não foi possível extrair ' + ExtractFileName(AArquivo) + ': ' + E.Message;
  end;
  gzclose(vGz);
end;

end.
