{ The [Inject] attribute of the service container: a field of a service (or of any
  object the container makes) marked [Inject] is filled with the service of its
  interface, or with the one named in the attribute, right after the instance is
  made - so it is ready in the methods, not yet in the constructor. Only where the
  compiler has attributes and extended RTTI; elsewhere the unit is empty and the
  constructor asks the container itself. }
unit RALServiceInject;

{$I ..\..\core\PascalRAL.inc}

interface

{$IFDEF RALHasExtRTTI}
uses
  SysUtils, Rtti, TypInfo,
  RALRequest, RALServiceContainer;

type
  /// Marks a field to be filled by the container: by its interface, or by name.
  InjectAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    /// The service of the field's interface.
    constructor Create; overload;
    /// The service named AName.
    constructor Create(const AName: string); overload;

    /// The name of the service; empty = by the field's interface.
    property Name: string read FName;
  end;
{$ENDIF}

implementation

{$IFDEF RALHasExtRTTI}

{ InjectAttribute }

constructor InjectAttribute.Create;
begin
  inherited Create;
  FName := '';
end;

constructor InjectAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

/// Fills the fields of AInstance marked [Inject].
procedure InjectFields(AContainer: TRALServiceContainer; AInstance: TObject;
  ARequest: TRALRequest);
var
  vContext: TRttiContext;
  vField: TRttiField;
  vAttr: TCustomAttribute;
  vName: string;
  vGUID: TGUID;
  vIntf: IInterface;
  vValue: TValue;
begin
  vContext := TRttiContext.Create;
  try
    for vField in vContext.GetType(AInstance.ClassType).GetFields do
      for vAttr in vField.GetAttributes do
      begin
        if not (vAttr is InjectAttribute) then
          Continue;
        vName := InjectAttribute(vAttr).Name;
        if vField.FieldType is TRttiInterfaceType then
        begin
          vGUID := TRttiInterfaceType(vField.FieldType).GUID;
          vIntf := nil;
          if vName <> '' then
          begin
            if not Supports(AContainer.Resolve(vName, ARequest), vGUID, vIntf) then
              vIntf := nil;
          end
          else
            AContainer.Resolve(vGUID, vIntf, ARequest);
          TValue.Make(@vIntf, vField.FieldType.Handle, vValue);
          vField.SetValue(AInstance, vValue);
        end
        else if (vField.FieldType.TypeKind = tkClass) and (vName <> '') then
          vField.SetValue(AInstance, TValue.From<TObject>(
            AContainer.ResolveObject(vName, ARequest)));
      end;
  finally
    vContext.Free;
  end;
end;

initialization
  RALServiceAfterCreateHook := InjectFields;

finalization
  if @RALServiceAfterCreateHook = @InjectFields then
    RALServiceAfterCreateHook := nil;

{$ENDIF}

end.
