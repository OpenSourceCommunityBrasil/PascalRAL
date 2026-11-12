/// ralprotoc: writes a Pascal unit per .proto file (Protocol Buffers messages,
/// and gRPC clients and services when the file has services).
///
///   ralprotoc [-I<folder>]... [-out=<folder>] [-unit=<Name>] [-nogrpc] [-imports]
///     <file.proto>...
///
/// -I adds a folder where imports are looked for; -out is where the units go
/// (the current folder by default); -unit names the unit of a single file
/// (otherwise its base name in PascalCase); -imports writes the units of the
/// imported files too (never google/protobuf, which is RALProtobufWKT);
/// -nogrpc leaves the services out. Compile the units with
/// RALModuleProtobufPackage (and RALModuleGrpcPackage for services).
program ralprotoc;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}{$IFDEF UNIX}cthreads,{$ENDIF}{$ENDIF}
  Classes, SysUtils,
  RALTypes, RALProtoSchema, RALProtoCodeGen;

/// Writes the unit of AFile into AFolder.
procedure WriteUnit(ASchema: TRALProtoSchema; AFile: TRALProtoFileDef;
  const AFolder: string; const AOptions: TRALProtoCodeGenOptions);
var
  vText: StringRAL;
  vName: string;
  vStream: TFileStream;
begin
  vText := RALProtoToPascal(ASchema, AFile, AOptions);
  if AOptions.PascalUnit <> '' then
    vName := string(AOptions.PascalUnit)
  else
    vName := string(AFile.PascalUnit);
  vName := IncludeTrailingPathDelimiter(AFolder) + vName + '.pas';
  vStream := TFileStream.Create(vName, fmCreate);
  try
    if vText <> '' then
      vStream.WriteBuffer(vText[1], Length(vText));
  finally
    vStream.Free;
  end;
  WriteLn(vName);
end;

var
  /// The files loaded and their imports.
  vSchema: TRALProtoSchema;
  /// The .proto files named on the command line.
  vFiles: TStringList;
  /// What the generator writes for the named files.
  vOptions: TRALProtoCodeGenOptions;
  /// What the generator writes for an import (its own unit name).
  vImportOptions: TRALProtoCodeGenOptions;
  /// The folder the units go to.
  vOut: string;
  /// The argument being read.
  vArg: string;
  /// Whether the imports get units too.
  vImports: Boolean;
  /// A loop index.
  vInt: Integer;
  /// The file being written.
  vFile: TRALProtoFileDef;
begin
  vOptions := RALProtoCodeGenOptions;
  vOut := '.';
  vImports := False;
  vFiles := TStringList.Create;
  vSchema := TRALProtoSchema.Create;
  try
    try
      for vInt := 1 to ParamCount do
      begin
        vArg := ParamStr(vInt);
        if Copy(vArg, 1, 2) = '-I' then
          vSchema.SearchPaths.Add(Copy(vArg, 3, MaxInt))
        else if Copy(vArg, 1, 5) = '-out=' then
          vOut := Copy(vArg, 6, MaxInt)
        else if Copy(vArg, 1, 6) = '-unit=' then
          vOptions.PascalUnit := StringRAL(Copy(vArg, 7, MaxInt))
        else if vArg = '-nogrpc' then
          vOptions.Grpc := False
        else if vArg = '-imports' then
          vImports := True
        else if Copy(vArg, 1, 1) = '-' then
          raise Exception.Create('unknown option ' + vArg)
        else
          vFiles.Add(vArg);
      end;
      if vFiles.Count = 0 then
      begin
        WriteLn('ralprotoc [-I<folder>]... [-out=<folder>] [-unit=<Name>] [-nogrpc] [-imports] <file.proto>...');
        ExitCode := 2;
        Exit;
      end;
      if (vFiles.Count > 1) and (vOptions.PascalUnit <> '') then
        raise Exception.Create('-unit names one file only');
      // the folder of each file is a search path, as protoc's -I.
      for vInt := 0 to vFiles.Count - 1 do
        if (ExtractFilePath(vFiles[vInt]) <> '') and
           (vSchema.SearchPaths.IndexOf(ExtractFilePath(vFiles[vInt])) < 0) then
          vSchema.SearchPaths.Add(ExtractFilePath(vFiles[vInt]));
      for vInt := 0 to vFiles.Count - 1 do
        vSchema.Load(StringRAL(vFiles[vInt]));
      for vInt := 0 to vSchema.FileCount - 1 do
      begin
        vFile := vSchema.Files[vInt];
        if vFiles.IndexOf(string(vFile.FileName)) >= 0 then
          WriteUnit(vSchema, vFile, vOut, vOptions)
        else if vImports and (vFile.PascalUnit <> 'RALProtobufWKT') then
        begin
          vImportOptions := vOptions;
          vImportOptions.PascalUnit := '';
          WriteUnit(vSchema, vFile, vOut, vImportOptions);
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn('ralprotoc: ', E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    vSchema.Free;
    vFiles.Free;
  end;
end.
