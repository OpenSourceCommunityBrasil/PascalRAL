{ Prints the capability symbols PascalRAL.inc defines for the compiler that builds
  this program, and exercises each one it claims: if a symbol promises a feature the
  compiler lacks, this program does not compile. Build it with every compiler of the
  floor matrix after changing the symbols. }
program capabilities;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

{$I ..\..\..\src\core\PascalRAL.inc}

{$IFDEF FPC}
  {$IFDEF RALHasAnonMethods}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
  {$IFDEF RALHasAttributes}
    {$MODESWITCH PREFIXEDATTRIBUTES}
  {$ENDIF}
{$ENDIF}

uses
  Classes, SysUtils, TypInfo
  {$IF Defined(RALHasAttributes) or Defined(RALHasExtRTTI) or Defined(RALHasVirtualInterface)}
  , Rtti
  {$IFEND}
  {$IFDEF RALHasGenerics}
  , Generics.Collections
  {$ENDIF};

{$IFDEF RALHasAnonMethods}
type
  /// A handler written as an anonymous method.
  TProbeRef = reference to procedure(AValue: Integer);
{$ENDIF}

{$IFDEF RALHasAttributes}
type
  /// An attribute with an argument.
  ProbeAttribute = class(TCustomAttribute)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    property Text: string read FText;
  end;

  /// A class carrying the attribute.
  [Probe('route')]
  TProbeTarget = class
  end;

constructor ProbeAttribute.Create(const AText: string);
begin
  FText := AText;
end;
{$ENDIF}

{$IFDEF RALHasExtRTTI}
type
  /// A class whose private field and public method are reached through RTTI.
  {$RTTI EXPLICIT FIELDS([vcPrivate]) METHODS([vcPublic])}
  TProbeFields = class
  private
    FValue: Integer;
  public
    function Twice(AValue: Integer): Integer;
  end;

function TProbeFields.Twice(AValue: Integer): Integer;
begin
  Result := AValue * 2;
end;
{$ENDIF}

{$IFDEF RALHasVirtualInterface}
type
  /// An interface answered by TVirtualInterface.
  {$M+}
  IProbeProxy = interface(IInvokable)
    ['{6A0F2E0B-6C4F-4B5A-9C8E-1F2D3C4B5A6A}']
    function Sum(A, B: Integer): Integer;
  end;
  {$M-}

  /// The handler of the proxy (a method: XE2 refuses this signature under $M+).
  TProbeInvoker = class
  public
    procedure Invoke(AMethod: TRttiMethod; const AArgs: TArray<TValue>; out AResult: TValue);
  end;

procedure TProbeInvoker.Invoke(AMethod: TRttiMethod; const AArgs: TArray<TValue>;
  out AResult: TValue);
begin
  AResult := AArgs[1].AsInteger + AArgs[2].AsInteger;
end;
{$ENDIF}

{$IFDEF RALHasGenerics}
type
  /// A generic class with constraints.
  TProbeBox<T: class, constructor> = class
  public
    function Make: T;
  end;

function TProbeBox<T>.Make: T;
begin
  Result := T.Create;
end;
{$ENDIF}

/// Writes one line: the symbol and whether its probe passed.
procedure Report(const ASymbol: string; APassed: Boolean);
begin
  if APassed then
    WriteLn(ASymbol, ': yes')
  else
    WriteLn(ASymbol, ': FAILED');
end;

var
  vFailed: Boolean;
  vFound: Boolean;
{$IFDEF RALHasAnonMethods}
  vRef: TProbeRef;
  vSum: Integer;
{$ENDIF}
{$IF Defined(RALHasAttributes) or Defined(RALHasExtRTTI)}
  vContext: TRttiContext;
{$IFEND}
{$IFDEF RALHasAttributes}
  vAttr: TCustomAttribute;
{$ENDIF}
{$IFDEF RALHasExtRTTI}
  vFields: TProbeFields;
{$ENDIF}
{$IFDEF RALHasVirtualInterface}
  vInvoker: TProbeInvoker;
  vProxy: TVirtualInterface;
  vIntf: IProbeProxy;
{$ENDIF}
{$IFDEF RALHasGenerics}
  vBox: TProbeBox<TStringList>;
  vList: TObjectList<TStringList>;
{$ENDIF}
begin
  vFailed := False;
  {$IFDEF FPC}
  WriteLn('FPC ', {$I %FPCVERSION%}, ' ', {$I %FPCTARGETOS%}, '-', {$I %FPCTARGETCPU%});
  {$ELSE}
  WriteLn('Delphi, CompilerVersion ', FloatToStr(CompilerVersion));
  {$ENDIF}

  {$IFDEF RALHasAnonMethods}
  vSum := 0;
  vRef := procedure(AValue: Integer)
    begin
      vSum := vSum + AValue;
    end;
  vRef(2);
  Report('RALHasAnonMethods', vSum = 2);
  vFailed := vFailed or (vSum <> 2);
  {$ELSE}
  WriteLn('RALHasAnonMethods: no');
  {$ENDIF}

  {$IFDEF RALHasAttributes}
  vFound := False;
  vContext := TRttiContext.Create;
  for vAttr in vContext.GetType(TProbeTarget).GetAttributes do
    if (vAttr is ProbeAttribute) and (ProbeAttribute(vAttr).Text = 'route') then
      vFound := True;
  Report('RALHasAttributes', vFound);
  vFailed := vFailed or (not vFound);
  {$ELSE}
  WriteLn('RALHasAttributes: no');
  {$ENDIF}

  {$IFDEF RALHasExtRTTI}
  vContext := TRttiContext.Create;
  vFields := TProbeFields.Create;
  try
    vContext.GetType(TProbeFields).GetField('FValue').SetValue(vFields, 7);
    vFound := (vFields.FValue = 7) and
      (vContext.GetType(TProbeFields).GetMethod('Twice').Invoke(vFields, [21]).AsInteger = 42);
  finally
    vFields.Free;
  end;
  Report('RALHasExtRTTI', vFound);
  vFailed := vFailed or (not vFound);
  {$ELSE}
  WriteLn('RALHasExtRTTI: no');
  {$ENDIF}

  {$IFDEF RALHasVirtualInterface}
  vInvoker := TProbeInvoker.Create;
  try
    vProxy := TVirtualInterface.Create(TypeInfo(IProbeProxy), vInvoker.Invoke);
    vFound := Supports(vProxy, IProbeProxy, vIntf) and (vIntf.Sum(2, 3) = 5);
    vIntf := nil;
  finally
    vInvoker.Free;
  end;
  Report('RALHasVirtualInterface', vFound);
  vFailed := vFailed or (not vFound);
  {$ELSE}
  WriteLn('RALHasVirtualInterface: no');
  {$ENDIF}

  {$IFDEF RALHasGenerics}
  vList := TObjectList<TStringList>.Create(True);
  vBox := TProbeBox<TStringList>.Create;
  try
    vList.Add(vBox.Make);
    vFound := vList.Count = 1;
  finally
    vBox.Free;
    vList.Free;
  end;
  Report('RALHasGenerics', vFound);
  vFailed := vFailed or (not vFound);
  {$ELSE}
  WriteLn('RALHasGenerics: no');
  {$ENDIF}

  if vFailed then
    ExitCode := 1;
end.
