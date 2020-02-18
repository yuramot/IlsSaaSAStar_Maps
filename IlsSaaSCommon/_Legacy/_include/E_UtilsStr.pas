unit E_UtilsStr;
//------------------------------------------------------------------------------
// Модуль подпрограмм манипуляций со строками
//------------------------------------------------------------------------------
// Содержит утилиты по манипуляциям со строками
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils;

//------------------------------------------------------------------------------
//! преобразовать string в ShortString
//------------------------------------------------------------------------------
function String2Short(
  const AString: string
): ShortString;

//------------------------------------------------------------------------------
//! преобразовать ShortString в string
//------------------------------------------------------------------------------
function Short2String(
  const AString: ShortString
): string;

//------------------------------------------------------------------------------
//! заменить все вхождения символа в строке на другой
//------------------------------------------------------------------------------
function ReplaceSymbol(
  const AStr: string;
  const AFrom: Char;
  const ATo: Char
): string;

//------------------------------------------------------------------------------
//! добавить символ в конец строки, если его там ещё нет
//------------------------------------------------------------------------------
function IncludeLastDelimiter(
  const AStr: string;
  const ADelim: Char
): string;

//------------------------------------------------------------------------------
//! удалить символ из конца строки, если он совпадает с заданным
//! удаляется только 1 символ
//------------------------------------------------------------------------------
function ExcludeLastDelimiter(
  const AStr: string;
  const ADelim: Char
): string;

//------------------------------------------------------------------------------
//! найти позицию первого вхождения символа
//! возвращает 0 если символ не найден
//------------------------------------------------------------------------------
function PosFirstDelimiter(
  const AStr: string;
  const ADelim: Char
): Integer;

//------------------------------------------------------------------------------
//! найти позицию последнего вхождения символа
//! возвращает 0 если символ не найден
//------------------------------------------------------------------------------
function PosLastDelimiter(
  const AStr: string;
  const ADelim: Char
): Integer;

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
function ConvHexToByte(
  var RPtr: PAnsiChar
): Byte;

//------------------------------------------------------------------------------
implementation

function String2Short(
  const AString: string
): ShortString;
var
  //!
  LString: AnsiString;
//------------------------------------------------------------------------------
begin
  LString := AnsiString( AString );
  Result := ShortString( LString );
end;

function Short2String(
  const AString: ShortString
): string;
var
  //!
  LString: AnsiString;
//------------------------------------------------------------------------------
begin
  LString := AnsiString( AString );
  Result := string( AString );
end;

function ReplaceSymbol(
  const AStr: string;
  const AFrom: Char;
  const ATo: Char
): string;
var
  //!
  LPtr: PChar;
//------------------------------------------------------------------------------
begin
  Result := AStr;
  if ( Result = '' ) then Exit;
  UniqueString( Result );
  LPtr := PChar( Result );
  while ( LPtr^ <> #0 ) do
  begin
    if ( LPtr^ = AFrom ) then
      LPtr^ := ATo;
    Inc( LPtr );
  end;
end;

function IncludeLastDelimiter(
  const AStr: string;
  const ADelim: Char
): string;
begin
  if ( AStr = '' ) then Exit( ADelim );
  Result := AStr;
  if ( Result[Length( Result )] <> ADelim ) then
    Result := Result + ADelim;
end;

function ExcludeLastDelimiter(
  const AStr: string;
  const ADelim: Char
): string;
begin
  if ( AStr = '' ) then Exit( '' );
  if ( AStr[Length( AStr )] = ADelim ) then
    Result := Copy( AStr, 1, Length( AStr ) - 1 )
  else
    Result := AStr;
end;

function PosFirstDelimiter(
  const AStr: string;
  const ADelim: Char
): Integer;
begin
  for Result := 1 to Length( AStr ) do
  begin
    if ( AStr[Result] = ADelim ) then Exit;
  end;
  Result := 0;
end;

function PosLastDelimiter(
  const AStr: string;
  const ADelim: Char
): Integer;
begin
  for Result := Length( AStr ) downto 1 do
  begin
    if ( AStr[Result] = ADelim ) then Exit;
  end;
  Result := 0;
end;

function ConvHexToByte(
  var RPtr: PAnsiChar
): Byte;
var
  //!
  UpChar: AnsiChar;
//------------------------------------------------------------------------------
begin
  Result := 0;
  UpChar := UpCase( RPtr^ );
  if ( UpChar >= '1' ) and ( UpChar <= '9' ) then
    Result := Result + Ord( UpChar ) - 48
  else if ( UpChar >= 'A' ) and ( UpChar <= 'F' ) then
    Result := Result + Ord( UpChar ) - 55
  else
    raise Exception.Create( 'Не шестнадцатеричный символ.' );
  Inc( RPtr );
  //
  Result := Result shl 4;
  UpChar := UpCase( RPtr^ );
  if ( UpChar >= '1' ) and ( UpChar <= '9' ) then
    Result := Result + Ord( UpChar ) - 48
  else if ( UpChar >= 'A' ) and ( UpChar <= 'F' ) then
    Result := Result + Ord( UpChar ) - 55
  else
    raise Exception.Create( 'Не шестнадцатеричный символ.' );
  Inc( RPtr );
end;

end.

