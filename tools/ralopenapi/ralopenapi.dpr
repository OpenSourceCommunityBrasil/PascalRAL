/// ralopenapi: writes a Pascal unit (TRALObject classes and a typed client)
/// from an OpenAPI 3.0/3.1 document in JSON.
///
///   ralopenapi <document.json> <Unit.pas> [-noclient] [-client=TMyClient]
///
/// The unit is named after the output file. Compile it with PascalRAL and
/// RALModuleObjectsPackage.
program ralopenapi;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}{$IFDEF UNIX}cthreads,{$ENDIF}{$ENDIF}
  Classes, SysUtils,
  RALTypes, RALOpenAPIImport;

var
  vIn, vOut: TStringList;
  vOptions: TRALOpenAPIImportOptions;
  vInt: Integer;
  vText: RawByteString;
  vStream: TFileStream;
begin
  if ParamCount < 2 then
  begin
    WriteLn('ralopenapi <document.json> <Unit.pas> [-noclient] [-client=TName]');
    ExitCode := 2;
    Exit;
  end;
  vOptions := RALOpenAPIOptions(ChangeFileExt(ExtractFileName(ParamStr(2)), ''));
  for vInt := 3 to ParamCount do
    if ParamStr(vInt) = '-noclient' then
      vOptions.WithClient := False
    else if Pos('-client=', ParamStr(vInt)) = 1 then
      vOptions.ClientName := Copy(ParamStr(vInt), 9, MaxInt);
  vIn := nil;
  vOut := TStringList.Create;
  try
    try
      vStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
      try
        SetLength(vText, vStream.Size);
        if vStream.Size > 0 then
          vStream.ReadBuffer(vText[1], vStream.Size);
      finally
        vStream.Free;
      end;
      { a BOM is not JSON }
      if Copy(vText, 1, 3) = #$EF#$BB#$BF then
        Delete(vText, 1, 3);
      vOut.Text := RALOpenAPIToPascal(StringRAL(vText), vOptions);
      vOut.SaveToFile(ParamStr(2));
      WriteLn(ParamStr(2), ': ', vOut.Count, ' lines');
    except
      on E: Exception do
      begin
        WriteLn('ralopenapi: ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    vIn.Free;
    vOut.Free;
  end;
end.
