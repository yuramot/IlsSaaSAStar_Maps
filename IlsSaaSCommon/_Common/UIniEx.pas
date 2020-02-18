unit UIniEx;
//------------------------------------------------------------------------------
// модуль расширения стандартного TIniFile
//------------------------------------------------------------------------------
// теперь функции чтения не только читают, но и записывают обратно
//  значения по умолчанию, если изначально их там не было
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, IniFiles, Ils.Utils;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TIniFileEx = class(TIniFile)
  public
    function ReadString(
      const Section, Ident, Default: string
    ): string; override;
    function ReadInteger(
      const Section, Ident: string;
      Default: Longint
    ): Longint; override;
    function ReadBool(
      const Section, Ident: string;
      Default: Boolean
    ): Boolean; override;
    function ReadILSDateTime(
      const Section, Ident: string;
      const Default: TDateTime
    ): TDateTime; virtual;
    function ReadOnlyILSDateTime(
      const Section, Ident: string;
      const Default: TDateTime
    ): TDateTime;
    procedure WriteILSDateTime(
      const Section, Ident: string;
      const Value: TDateTime
    ); virtual;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TIniFileEx
//------------------------------------------------------------------------------

function TIniFileEx.ReadString(
  const Section, Ident, Default: string
): string;
begin
  Result := inherited ReadString(Section, Ident, Default);
  if (Result = Default) then
  begin
    try
      WriteString(Section, Ident, Default);
    except
      // не бросаем исключения на чтении
    end;
  end;
end;

function TIniFileEx.ReadInteger(
  const Section, Ident: string;
  Default: Longint
): Longint;
begin
  Result := inherited ReadInteger(Section, Ident, Default);
  if (Result = Default) then
  begin
    try
      WriteInteger(Section, Ident, Default);
    except
      // не бросаем исключения на чтении
    end;
  end;
end;

function TIniFileEx.ReadBool(
  const Section, Ident: string;
  Default: Boolean
): Boolean;
begin
  Result := inherited ReadBool(Section, Ident, Default);
  if (Result = Default) then
  begin
    try
      WriteBool(Section, Ident, Default);
    except
      // не бросаем исключения на чтении
    end;
  end;
end;

function TIniFileEx.ReadILSDateTime(
  const Section, Ident: string;
  const Default: TDateTime
): TDateTime;
begin
  Result := ReadOnlyILSDateTime(Section, Ident, Default);
  if (Result = Default) then
  begin
    try
      WriteILSDateTime(Section, Ident, Default);
    except
      // не бросаем исключения на чтении
    end;
  end;
end;

function TIniFileEx.ReadOnlyILSDateTime(
  const Section, Ident: string;
  const Default: TDateTime
): TDateTime;
begin
  Result := IlsToDateTime(inherited ReadString(Section, Ident, ''), Default);
end;

procedure TIniFileEx.WriteILSDateTime(
  const Section, Ident: string;
  const Value: TDateTime
);
begin
  WriteString(Section, Ident, DateTimeToIls(Value));
end;

end.

