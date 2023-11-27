{******************************************************************************
 Project : libBinRes
 Version : 0.9 aka "Lite", that only includes one class, namely TAppVersionInfo
 Description: Freepascal library for working with resources compiled into
              executables in Lazarus. Written and tested on Ubuntu 12.04.
 Copyright (c) 2014, Krzysztof Kamil Jacewicz. All rights reserved.
 Contact: k.k.jacewicz@gmail.com

| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| The name of Krzysztof Kamil Jacewicz  may not be used to endorse or promote  |
| products derived from this software without specific prior written           |
| permission.                                                                  |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Developer of this code is Krzysztof Kamil Jacewicz (Poland).             |
| The code is Copyright (c) 1999-2010.                                         |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
|                            |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================

 ******************************************************************************}

Unit libBinRes;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Resource,
  VersionResource, VersionTypes;

Const
  C_DEF_VER_FORMAT             = '{mjr}.{mnr}.{rev}.{bld}';

{******************************************************************************
 USER'S MANUAL:

 Table of content:
       1. Simplest use
       2. Formatted output
       3. Accessing version numbers as integers
       4. About formatting
       5. Advanced


 1. Simplest use:
    (...)
    uses (...), libBinRes;
    (...)
    //something like "1.0.0.0"
      ShowMessage(libBinRes.AppVersionInfo.VersionStr);
    (...)

 2. Formatted output:
    (...)
    uses (...), libBinRes;
    (...)
    //something like "v1.0, rev.0 build 0"
      ShowMessage(libBinRes.AppVersionInfo.VersionStrEx['v{mjr}.{mnr}, rev.{rev} build {bld}']);
    (...)

 3. Accessing version numbers as integers:
    (...)
    uses (...), libBinRes;
    (...)
    var
      minor,major,revision,build_no : integer;
    (...)
      major    := libBinRes.AppVersionInfo.Major;
      minor    := libBinRes.AppVersionInfo.Minor;
      revision := libBinRes.AppVersionInfo.Revision;
      build_no := libBinRes.AppVersionInfo.BuildNo;
    (...)

 4. About formatting
    The VersionStrEx property can format the string with version numbers following
    developer's liking. The Property takes a string as an argument, then substitutes:
    - every "{mjr}" substring with a number of major version,
    - every "{mnr}" substring with a number of minor version,
    - every "{rev}" substring with a revision number,
    - every "{bld}" substring with a build number
    If the passed string is an empty string, then the default formatting will be
    applied, as defined in the C_DEF_VER_FORMAT constant, which is used when calling
    the VersionStr Property.

 5. Advanced
    the libBinRes.AppVersionInfo is a function returning an instance of TAppVersionInfo class.
    The instance is only being created at run-time when first accessed. Means, if you
    do add libBinRes unit to your "uses" clause, but never actually call libBinRes.VersionInf,
    then the instance will never be created.
    The unit will automatically free the instance at the end of your program execution
    (at unloading of the library, from "Finalize" section), if off course, there
    is any instance to be freed.
    Alternatively, you could choose to create your own instance of the TAppVersionInfo
    class and call your object explicitely, then free it explicitely when no more needed.

 ******************************************************************************}

type
  { TAppVersionInfo }

  TAppVersionInfo = class
  private
    FProvidedHinstance : THandle;
    FMajor             : integer;
    FMinor             : integer;
    FRevision          : integer;
    FBuildNo           : integer;
    Function  GetMajorAsStr: string;
    Function  GetMinorAsStr: string;
    Function  GetRevisionAsStr: string;
    Function  GetBuildNoAsStr: string;
    Function  GetVersionStr: string;
    Function  GetVersionStrEx(ver_format: string): string;
  public
    Constructor Create; overload;
    Constructor Create(const AProvidedHinstance : THandle); overload;
    Procedure Reload;
    Property Major: integer read FMajor;
    Property Minor: integer read FMinor;
    Property Revision: integer read FRevision;
    Property BuildNo: integer read FBuildNo;
    Property MajorAsStr: string read GetMajorAsStr;
    Property MinorAsStr: string read GetMinorAsStr;
    Property RevisionAsStr: string read GetRevisionAsStr;
    Property BuildNoAsStr: string read GetBuildNoAsStr;
    Property VersionStr: string read GetVersionStr;
    Property VersionStrEx[ver_format: string]: string read GetVersionStrEx; default;
  end;

Function AppVersionInfo: TAppVersionInfo;

implementation
Var
  FVersionInfo                   : TAppVersionInfo = nil;

Function AppVersionInfo: TAppVersionInfo;
 begin
   if not Assigned(FVersionInfo)then FVersionInfo := TAppVersionInfo.Create;
   Result := FVersionInfo;
 end;

{ TAppVersionInfo }

Constructor TAppVersionInfo.Create;
 begin
   inherited Create;
   FProvidedHInstance := -1;
   Reload;
 end;

Constructor TAppVersionInfo.Create(const AProvidedHinstance : THandle); overload;
 begin
   inherited Create;
   FProvidedHInstance := AProvidedHinstance;
   Reload;
 end;

Procedure TAppVersionInfo.Reload;
 var
   AVersionResource   : TVersionResource;
   AResourceStream    : TResourceStream;
 begin
 //Allocate resources, and load data:
   AVersionResource := TVersionResource.Create;
   if not Assigned(AVersionResource)then exit;
   Try
     if(FProvidedHInstance<0)
       then AResourceStream := TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION))
       else AResourceStream := TResourceStream.CreateFromID(FProvidedHInstance, 1, PChar(RT_VERSION));
     if Assigned(AResourceStream)then
        try
          AVersionResource.SetCustomRawDataStream(AResourceStream);
          with AVersionResource.FixedInfo do
            begin
             FMajor    := FileVersion[0];
             FMinor    := FileVersion[1];
             FRevision := FileVersion[2];
             FBuildNo  := FileVersion[3];
            end;
          AVersionResource.SetCustomRawDataStream(nil);
        finally
          FreeAndNil(AResourceStream);
        end;
   Finally
   //Free resources:
     FreeAndNil(AVersionResource);
   End;
 end;

Function TAppVersionInfo.GetMajorAsStr: string;
         begin Result := IntToStr(Major); end;

Function TAppVersionInfo.GetMinorAsStr: string;
         begin Result := IntToStr(Minor); end;

Function TAppVersionInfo.GetRevisionAsStr: string;
         begin Result := IntToStr(Revision); end;

Function TAppVersionInfo.GetBuildNoAsStr: string;
         begin Result := IntToStr(BuildNo); end;

Function TAppVersionInfo.GetVersionStr: string;
         begin Result := VersionStrEx[C_DEF_VER_FORMAT]; end;

Function TAppVersionInfo.GetVersionStrEx(ver_format: string): string;
          begin
            if(ver_format='')then ver_format:=C_DEF_VER_FORMAT;
            Result := StringReplace(ver_format,'{mjr}',MajorAsStr,[rfReplaceAll]);
            Result := StringReplace(Result    ,'{mnr}',MinorAsStr,[rfReplaceAll]);
            Result := StringReplace(Result    ,'{rev}',RevisionAsStr,[rfReplaceAll]);
            Result := StringReplace(Result    ,'{bld}',BuildNoAsStr,[rfReplaceAll]);
          end;

Finalization
 if Assigned(FVersionInfo)then FreeAndNil(FVersionInfo);
end.
