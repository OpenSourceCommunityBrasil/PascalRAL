/// Exceptions with stable error codes and localized messages.
unit RALExceptions;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

interface

uses
  SysUtils,
  RALErrorCodes;

const
  { Existing exceptions created with only a message have no assigned code. }
  RAL_ERROR_UNSPECIFIED = RALErrorCodes.RAL_ERROR_UNSPECIFIED;

type
  /// Exception whose Message includes the assigned nonzero RAL error code.
  ERALException = class(Exception)
  private
    FErrorCode: Integer;
    /// Stores the code and prefixes Message when the code is nonzero.
    procedure InitializeErrorCode(AErrorCode: Integer);
  public
    /// Creates an exception from the localized message template for the code.
    constructor Create(AErrorCode: Integer); overload;
    /// Creates an exception with a custom message and code zero.
    constructor Create(const Msg: string); reintroduce; overload;
    /// Creates an exception with an explicit code and custom message.
    constructor Create(AErrorCode: Integer; const Msg: string); overload;
    /// Copies Message, HelpContext and the RAL error code from an existing exception.
    constructor CreateCopy(AException: Exception);
    /// Formats the localized message for the code with the supplied arguments.
    constructor CreateFmt(AErrorCode: Integer; const Args: array of const); overload;
    /// Formats a custom message and assigns code zero.
    constructor CreateFmt(const Msg: string; const Args: array of const); reintroduce; overload;
    /// Formats a custom message and assigns the explicit code.
    constructor CreateFmt(AErrorCode: Integer; const Msg: string;
      const Args: array of const); overload;
    /// Language-independent error identifier; zero means unspecified.
    property ErrorCode: Integer read FErrorCode;
  end;

implementation

procedure ERALException.InitializeErrorCode(AErrorCode: Integer);
begin
  FErrorCode := AErrorCode;
  if FErrorCode <> RAL_ERROR_UNSPECIFIED then
    Message := Format('[RAL %d] %s', [FErrorCode, Message]);
end;

constructor ERALException.Create(AErrorCode: Integer);
begin
  inherited Create(RALGetErrorMessage(AErrorCode));
  InitializeErrorCode(AErrorCode);
end;

constructor ERALException.CreateCopy(AException: Exception);
begin
  inherited Create(AException.Message);
  HelpContext := AException.HelpContext;
  if AException is ERALException then
    FErrorCode := ERALException(AException).ErrorCode
  else
    FErrorCode := RAL_ERROR_UNSPECIFIED;
end;

constructor ERALException.CreateFmt(AErrorCode: Integer; const Args: array of const);
begin
  inherited CreateFmt(RALGetErrorMessage(AErrorCode), Args);
  InitializeErrorCode(AErrorCode);
end;

constructor ERALException.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := RAL_ERROR_UNSPECIFIED;
end;

constructor ERALException.Create(AErrorCode: Integer; const Msg: string);
begin
  inherited Create(Msg);
  InitializeErrorCode(AErrorCode);
end;

constructor ERALException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
  FErrorCode := RAL_ERROR_UNSPECIFIED;
end;

constructor ERALException.CreateFmt(AErrorCode: Integer; const Msg: string;
  const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
  InitializeErrorCode(AErrorCode);
end;

end.
