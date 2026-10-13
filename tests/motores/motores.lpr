program motores;

{$mode ObjFPC}{$H+}

// A regra do motor e a passagem da 1.x para a 2.0, sem GUI e sem tocar em IDE:
//   motores <raiz de um RAL 2.0>
// Le o catalogo da arvore e confere: quem e motor, FaltaMotor com e sem motor
// (Delphi e Lazarus), os herdeiros de cada pacote da 1.x e quando uma
// instalacao conta como 1.x.

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.Heranca, RALInst.IDE, RALInst.Motores;

var
  GFalhas: integer = 0;

procedure Conferir(ACondicao: boolean; const ADescricao: string);
begin
  if ACondicao then
    WriteLn('  ok    ', ADescricao)
  else
  begin
    WriteLn('  FALHA ', ADescricao);
    Inc(GFalhas);
  end;
end;

function Lista(const ATexto: string): TStringList;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.CommaText := ATexto;
end;

procedure TestarMotores(ACatalogo: TCatalogo);
var
  vDelphi, vLazarus: TIDEInstance;
  vNomes, vMotores: TStringList;
  vMotivo: string;
begin
  WriteLn('motores:');
  Conferir(EhMotor(ACatalogo.Buscar(tpDelphi, 'IndyRAL')), 'IndyRAL e motor');
  Conferir(not EhMotor(ACatalogo.Buscar(tpDelphi, 'PascalRAL')), 'PascalRAL nao e motor');
  Conferir(not EhMotor(ACatalogo.Buscar(tpDelphi, 'RALPluginAuthPackage')),
           'RALPluginAuthPackage nao e motor');
  vDelphi := TIDEInstance.Create(tiDelphi);
  vLazarus := TIDEInstance.Create(tiLazarus);
  vMotores := TStringList.Create;
  try
    vDelphi.Nome := 'Delphi teste';
    vLazarus.Nome := 'Lazarus teste';
    MotoresDaIDE(ACatalogo, nil, vDelphi, vMotores);
    Conferir(vMotores.IndexOf('IndyRAL') >= 0, 'Delphi tem IndyRAL entre os motores');
    Conferir(vMotores.IndexOf('PascalRAL') < 0, 'PascalRAL fora da lista de motores');
    MotoresDaIDE(ACatalogo, nil, vLazarus, vMotores);
    Conferir(vMotores.IndexOf('fphttpral') >= 0, 'Lazarus tem fphttpral entre os motores');

    vNomes := Lista('PascalRAL,PascalRALDsgn,RALStoragePackage');
    try
      vMotivo := FaltaMotor(ACatalogo, nil, vDelphi, vNomes);
      Conferir(Pos('Delphi teste', vMotivo) > 0, 'sem motor no Delphi: recusa e diz a IDE');
      Conferir(Pos('IndyRAL', vMotivo) > 0, 'e sugere os motores que cabem');
      Conferir(FaltaMotor(ACatalogo, nil, vLazarus, vNomes) <> '', 'sem motor no Lazarus: recusa');
      vNomes.Add('indyral');
      Conferir(FaltaMotor(ACatalogo, nil, vDelphi, vNomes) = '',
               'com IndyRAL (grafia qualquer) passa');
      vNomes.Delete(vNomes.Count - 1);
      vNomes.Add('fphttpral');
      Conferir(FaltaMotor(ACatalogo, nil, vLazarus, vNomes) = '', 'com fphttpral o Lazarus passa');
      Conferir(FaltaMotor(ACatalogo, nil, vDelphi, vNomes) <> '',
               'fphttpral nao serve de motor ao Delphi');
      vNomes.Clear;
      Conferir(FaltaMotor(ACatalogo, nil, vDelphi, vNomes) = '',
               'nada marcado (desinstalar): nao recusa');
      vNomes.CommaText := 'cgiral,fphttpral';
      Conferir(FaltaMotor(ACatalogo, nil, vDelphi, vNomes) = '',
               'so pacotes que o Delphi nao tem: nada a instalar nele, nao recusa');
    finally
      vNomes.Free;
    end;
  finally
    vMotores.Free;
    vLazarus.Free;
    vDelphi.Free;
  end;
end;

procedure TestarHeranca(ACatalogo: TCatalogo; const ARaiz: string);
var
  vNomes, vHerdeiros, vDestino: TStringList;
  vInt: integer;
begin
  WriteLn('heranca:');
  vHerdeiros := TStringList.Create;
  vNomes := TStringList.Create;
  vDestino := TStringList.Create;
  try
    HerdeirosDe('RALDBFireDACLink', vHerdeiros);
    Conferir(vHerdeiros.CommaText = 'RALDBFireDACPackage,RALDBFireDACDsgn',
             'RALDBFireDACLink vira o par FireDAC');
    HerdeirosDe('ralwizardpack', vHerdeiros);
    Conferir(vHerdeiros.CommaText = 'RALWizardDsgn', 'ralwizardpack vira RALWizardDsgn');
    HerdeirosDe('IndyRAL', vHerdeiros);
    Conferir(vHerdeiros.Count = 0, 'motor nao tem herdeiro');
    HerdeirosDe('NaoExiste', vHerdeiros);
    Conferir(vHerdeiros.Count = 0, 'nome desconhecido nao tem herdeiro');
    Conferir(TrocadoPor('RALZStdCompress') = 'RALCompressZStdPackage',
             'TrocadoPor diz o novo nome');
    Conferir(TrocadoPor('IndyRAL') = '', 'TrocadoPor de motor e vazio');

    // todo herdeiro existe no catalogo de algum IDE
    NomesDa1x(vNomes);
    for vInt := 0 to Pred(vNomes.Count) do
    begin
      HerdeirosDe(vNomes[vInt], vHerdeiros);
      while vHerdeiros.Count > 0 do
      begin
        Conferir((ACatalogo.Buscar(tpDelphi, vHerdeiros[0]) <> nil) or
                 (ACatalogo.Buscar(tpLazarus, vHerdeiros[0]) <> nil),
                 vNomes[vInt] + ' -> ' + vHerdeiros[0] + ' existe na 2.0');
        vHerdeiros.Delete(0);
      end;
    end;

    // quando a instalacao conta como 1.x
    vNomes.Free;
    vNomes := Lista('PascalRALDsgn,IndyRAL');
    Conferir(not InstalacaoDa1x(ARaiz, vNomes, ACatalogo, tpDelphi),
             'arvore 2.0 com nomes que a 2.0 tem: nao e 1.x');
    IncluirHerdeiros(ARaiz, vNomes, ACatalogo, tpDelphi, vDestino);
    Conferir(vDestino.Count = 0, 'e nao herda nada');
    vNomes.Add('RALWizard');
    Conferir(InstalacaoDa1x(ARaiz, vNomes, ACatalogo, tpDelphi),
             'arvore 2.0, IDE com RALWizard: e 1.x');
    IncluirHerdeiros(ARaiz, vNomes, ACatalogo, tpDelphi, vDestino);
    Conferir(vDestino.IndexOf('RALWizardDsgn') >= 0, 'RALWizard herda RALWizardDsgn');
    Conferir(vDestino.IndexOf('RALPluginAuthPackage') >= 0,
             'PascalRALDsgn herda a autenticacao (o PascalRAL nao fica nos Known Packages)');
    Conferir(vDestino.IndexOf('RALModuleSwaggerPackage') >= 0, 'e o Swagger');
    Conferir(vDestino.IndexOf('IndyRAL') < 0, 'motor nao entra como herdeiro');
    vDestino.Clear;
    IncluirHerdeiros(ARaiz, vNomes, ACatalogo, tpLazarus, vDestino);
    Conferir(vDestino.IndexOf('RALPluginAuthPackage') >= 0, 'no Lazarus tambem');
  finally
    vDestino.Free;
    vNomes.Free;
    vHerdeiros.Free;
  end;
end;

var
  vCatalogo: TCatalogo;
  vRaiz: string;
begin
  if ParamCount < 1 then
  begin
    WriteLn('uso: motores <raiz de um RAL 2.0>');
    Halt(2);
  end;
  vRaiz := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
  if ArvoreDa1x(vRaiz) then
  begin
    WriteLn('a arvore e da 1.x: passe uma 2.0');
    Halt(2);
  end;
  vCatalogo := TCatalogo.Create;
  try
    if not vCatalogo.Carregar(vRaiz) then
    begin
      WriteLn('catalogo nao carregou: ', vCatalogo.Erros.Text);
      Halt(2);
    end;
    TestarMotores(vCatalogo);
    TestarHeranca(vCatalogo, vRaiz);
  finally
    vCatalogo.Free;
  end;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('todos os testes passaram')
  else
  begin
    WriteLn(GFalhas, ' falha(s)');
    Halt(1);
  end;
end.
