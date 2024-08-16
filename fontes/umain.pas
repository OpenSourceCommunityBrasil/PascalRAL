unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ufrm_modelo, ufrm_idioma, ufrm_ide, ufrm_ide_versions, ufrm_recursos,
  ufrm_install, utools, i18n_utils;

type

  { Tfmain }

  Tfmain = class(TForm)
    bTranslate: TButton;
    ntPages: TNotebook;
    procedure bTranslateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPgIdioma: Tfrm_idioma;
    FPgIDE: Tfrm_ide;
    FPgIDEVersions: Tfrm_ide_versions;
    FPgRecursos: Tfrm_recursos;
    FPgInstall: Tfrm_install;

    FLanguage: TLanguages;
    FPage: integer;
    FTheme: TThemes;
    FIDE: integer;

    function criarFrame(AClass : Tfrm_modelo_class) : Tfrm_modelo;
    procedure SetIDE(AValue: integer);
    procedure SetLanguage(AValue: TLanguages);
    procedure SetPage(AValue: integer);
    procedure SetTheme(AValue: TThemes);

    procedure translate(ALang : string);
  public
    procedure NextPage;
    procedure PriorPage;

    procedure installRAL(ALog : TMemo);
    procedure downloadRAL(ALog: TMemo);
    function checkDepedancy(ALog : TMemo) : boolean;
  published
    property Theme : TThemes read FTheme write SetTheme;
    property Language : TLanguages read FLanguage write SetLanguage;
    property IDE : integer read FIDE write SetIDE;
    property Page : integer read FPage write SetPage;
  end;

var
  fmain: Tfmain;

implementation

{$R *.lfm}

{ Tfmain }

procedure Tfmain.FormCreate(Sender: TObject);
begin
  FPgIdioma := Tfrm_idioma(criarFrame(Tfrm_idioma));
  FPgIDE := Tfrm_ide(criarFrame(Tfrm_ide));
  FPgIDEVersions := Tfrm_ide_versions(criarFrame(Tfrm_ide_versions));
  FPgRecursos := Tfrm_recursos(criarFrame(Tfrm_recursos));
  FPgInstall := Tfrm_install(criarFrame(Tfrm_install));

  Theme := tDark;
  Language := lEnglish;
  IDE := -1;
end;

procedure Tfmain.bTranslateClick(Sender: TObject);
begin
  translate('pt-BR');
  translate('en-US');
  translate('es-ES');
end;

function Tfmain.criarFrame(AClass: Tfrm_modelo_class): Tfrm_modelo;
var
  vPage : TPage;
begin
  vPage := TPage.Create(Self);
  vPage.Parent := ntPages;

  Result := AClass.Create(Self);
  Result.Parent := vPage;
  Result.Align := alClient;
end;

procedure Tfmain.SetIDE(AValue: integer);
begin
  if FIDE = AValue then
    Exit;

  FIDE := AValue;

  FPgIDE.IDE := AValue;
  FPgIDEVersions.IDE := AValue;
  FPgRecursos.IDE := AValue;
end;

procedure Tfmain.SetLanguage(AValue: TLanguages);
begin
  if FLanguage = AValue then
    Exit;

  FLanguage := AValue;

  FPgIdioma.Language := AValue;
  FPgIDE.Language := AValue;
  FPgIDEVersions.Language := AValue;
  FPgRecursos.Language := AValue;
  FPgInstall.Language := AValue;
end;

procedure Tfmain.SetPage(AValue: integer);
begin
  if FPage = AValue then
    Exit;

  if (AValue >= 0) and (AValue < ntPages.PageCount) then
  begin
    FPage := AValue;
    ntPages.PageIndex := AValue;
  end;
end;

procedure Tfmain.SetTheme(AValue: TThemes);
begin
  if FTheme = AValue then
    Exit;

  FTheme := AValue;

  FPgIdioma.Theme := AValue;
  FPgIDE.Theme := AValue;
  FPgIDEVersions.Theme := AValue;
  FPgRecursos.Theme := AValue;
  FPgInstall.Theme := AValue;
end;

procedure Tfmain.translate(ALang: string);
var
  vTrans : TTranslate;
  vStream : TStream;
  vFileStream : TFileStream;
begin
  vTrans := TTranslate.Create;
  try
    vTrans.Lang := ALang;
    vTrans.addComponentes(Self);
    vTrans.addUnit('utools');

    vStream := vTrans.SaveToStream;
    try
      vFileStream := TFileStream.Create('./languages/ralinstaller.'+ALang+'.po', fmCreate);
      try
        vFileStream.CopyFrom(vStream, vStream.Size);
      finally
        FreeAndNil(vFileStream);
      end;
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vTrans);
  end;
end;

procedure Tfmain.NextPage;
begin
  Page := FPage + 1;
end;

procedure Tfmain.PriorPage;
begin
  Page := FPage - 1;
end;

procedure Tfmain.installRAL(ALog: TMemo);
begin
  FPgIDEVersions.installRAL(ALog, FPgRecursos.Install, FPgRecursos.PathDownload);
end;

procedure Tfmain.downloadRAL(ALog: TMemo);
begin
  FPgRecursos.Install.Download(FPgRecursos.PathDownload, ALog);
end;

function Tfmain.checkDepedancy(ALog: TMemo): boolean;
begin
  Result := FPgRecursos.Install.CheckDepedancy(ALog);
end;

end.

