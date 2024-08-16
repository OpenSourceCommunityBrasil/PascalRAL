unit i18n_utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Buttons, Forms, HTTPDefs,
  Translations, fpjson, http_client, utools;

type

  { TTranslate }

  TTranslate = class
  private
    FFile : TStringList;
    FMsgs : TStringList;
    FLang : string;
  protected
    function fixText(AText : string) : string;
    procedure create_i18n(AComp : TComponent; AClass : string);
    function google_translate(AText : string) : string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure addUnit(AUnit : string);
    procedure addComponentes(AComp : TComponent);

    function SaveToStream : TStream;
  published
    property Lang : string read FLang write FLang;
  end;

implementation

{ TTranslate }

function TTranslate.fixText(AText: string): string;
begin
  Result := AText;
  Result := StringReplace(Result, #13, '/n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '/r', [rfReplaceAll]);
end;

procedure TTranslate.create_i18n(AComp: TComponent; AClass: string);
var
  vInt: integer;
  vComp: TComponent;
  vText, vTrans: string;
begin
{
#: tfrm_ide_versions.baddversion.hint
msgctxt "tfrm_ide_versions.baddversion.hint"
msgid "Adicionar um IDE pela sua pasta de instalação"
msgstr ""
}
  for vInt := 0 to Pred(AComp.ComponentCount) do begin
    vComp :=  AComp.Components[vInt];

    if vComp is TLabel then begin
      if TLabel(vComp).Caption <> '' then begin
        vText := fixText(TLabel(vComp).Caption);
        vTrans := google_translate(vText);

        FFile.Add('#: ' + AClass + '.' + vComp.Name + '.Caption');
        FFile.Add('msgctxt "' + AClass + '.' + vComp.Name + '.Caption"');
        FFile.Add('msgid "' + vText + '"');
        FFile.Add('msgstr "' + vTrans + '"');
        FFile.Add('');
      end;
    end
    else if vComp is TSpeedButton then begin
      if TSpeedButton(vComp).Caption <> '' then begin
        vText := fixText(TSpeedButton(vComp).Caption);
        vTrans := google_translate(vText);

        FFile.Add('#: ' + AClass + '.' + vComp.Name + '.Caption');
        FFile.Add('msgctxt "' + AClass + '.' + vComp.Name + '.Caption"');
        FFile.Add('msgid "' + vText + '"');
        FFile.Add('msgstr "' + vTrans + '"');
        FFile.Add('');
      end;
    end
    else if vComp.InheritsFrom(TFrame) then begin
      create_i18n(vComp, vComp.ClassName);
    end;

    if vComp.InheritsFrom(TControl) then begin
      if TControl(vComp).Hint <> '' then begin
        vText := fixText(TControl(vComp).Hint);
        vTrans := google_translate(vText);

        FFile.Add('#: ' + AClass + '.' + vComp.Name + '.Hint');
        FFile.Add('msgctxt "' + AClass + '.' + vComp.Name + '.Hint"');
        FFile.Add('msgid "' + vText + '"');
        FFile.Add('msgstr "' + vTrans + '"');
        FFile.Add('');
      end;
    end;
  end;
end;

function TTranslate.google_translate(AText: string): string;
var
  vURL: string;
  vHttp: THttpClient;
  vStream: TStream;
  vInt: integer;
  vjdResponse, vjdTranslation, vjdTranslationArray: TJSONData;
  vjaTranslation, vjaTranslationArray: TJSONArray;
begin
  Result := '';

  vInt := FMsgs.IndexOfName(AText);
  if vInt >= 0 then begin
    Result := FMsgs.ValueFromIndex[vInt];
    Exit;
  end;

  //https://wiki.lazarus.freepascal.org/Using_Google_Translate
  vURL := 'https://translate.googleapis.com/translate_a/single?client=gtx'
          +'&q='+HTTPEncode(AText)
          +'&sl=pt'
          +'&tl='+Copy(FLang, 1, 2)
          +'&dt=t'
          +'&ie=UTF-8&oe=UTF-8';

  vHttp := THttpClient.Create;
  try
    vStream := vHttp.DownloadLink(vURL);
    try
      vjdResponse := GetJSON(vStream);
      try
        vjdTranslation:= vjdResponse.FindPath('[0]');
        if (vjdTranslation <> nil) and (vjdTranslation.JSONType = jtArray) then
        begin
          vjaTranslation:= TJSONArray(vjdTranslation);
          for vInt:= 0 to Pred(vjaTranslation.Count) do
          begin
            vjdTranslationArray := vjaTranslation[vInt];
            if (vjdTranslationArray <> nil) and (vjdTranslationArray.JSONType = jtArray) then
            begin
              vjaTranslationArray:= TJSONArray(vjdTranslationArray);
              Result := Trim(vjaTranslationArray[0].AsString);
            end;
          end;
        end;
      finally
        FreeAndNil(vjdResponse);
      end;
    finally
      vStream.Free;
    end;
  finally
    FreeAndNil(vHttp);
  end;

  FMsgs.Add(AText+'='+Result);
end;

constructor TTranslate.Create;
begin
  inherited;
  FFile := TStringList.Create;
  FMsgs := TStringList.Create;
  FMsgs.Sorted := True;
end;

destructor TTranslate.Destroy;
begin
  FreeAndNil(FFile);
  FreeAndNil(FMsgs);
  inherited Destroy;
end;

function UnitTranslate(AName, AValue : AnsiString; {%H-}AHash : Longint; AArg: Pointer) : ansiString;
var
  vTrans : TTranslate;
begin
  vTrans := TTranslate(AArg);
  Result := vTrans.google_translate(AValue);

  vTrans.FFile.Add('#: ' + AName);
//  vTrans.FFile.Add('msgctxt "' + AName + '"');
  vTrans.FFile.Add('msgid "' + AValue + '"');
  vTrans.FFile.Add('msgstr "' + Result + '"');
  vTrans.FFile.Add('');
end;

procedure TTranslate.addUnit(AUnit: string);
begin
  SetUnitResourceStrings(AUnit, @UnitTranslate, Self);
end;

procedure TTranslate.addComponentes(AComp: TComponent);
begin
  create_i18n(AComp, AComp.ClassName);
end;

function TTranslate.SaveToStream: TStream;
var
  vFile : TStringList;
  vInt : integer;
begin
  vFile := TStringList.Create;
  try
    vFile.Add('msgid ""');
    vFile.Add('msgstr ""');
    vFile.Add('"Language: '+FLang+'\n"');
    vFile.Add('"MIME-Version: 1.0\n"');
    vFile.Add('"Content-Type: text/plain; charset=UTF-8\n"');
    vFile.Add('"Content-Transfer-Encoding: 8bit\n"');
    vFile.Add('"X-Generator: RALInstaller 1.0.0\n"');
    vFile.Add('');

    for vInt := 0 to Pred(FFile.Count) do
      vFile.Add(FFile.Strings[vInt]);

    Result := TMemoryStream.Create;
    vFile.SaveToStream(Result);
    Result.Position := 0;
  finally
    FreeAndNil(vFile);
  end;
end;

end.

