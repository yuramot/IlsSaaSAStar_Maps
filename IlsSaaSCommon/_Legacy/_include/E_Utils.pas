unit E_Utils;
//------------------------------------------------------------------------------
// Модуль маленьких полезных подпрограмм
//------------------------------------------------------------------------------
// Содержит процедуры и функции, выполняющие сервисную работу
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  T_Common;

//------------------------------------------------------------------------------
//! атомарные OR 1/2/4-байтовые
//------------------------------------------------------------------------------
procedure LockedOr8(
  AResult: Pointer;
  AArg: Pointer
); register;

procedure LockedOr16(
  AResult: Pointer;
  AArg: Pointer
); register;

procedure LockedOr32(
  AResult: Pointer;
  AArg: Pointer
); register;

//------------------------------------------------------------------------------
//! зеркально перевернуть байты в 8/4/2-байтном числе
//------------------------------------------------------------------------------
function Swap64(
  const AArg: UInt64
): UInt64; register;

function Swap32(
  const AArg: Cardinal
): Cardinal; register;

function Swap16(
  const AArg: Word
): Word; register;

//------------------------------------------------------------------------------
//! вернуть массив чисел из строки чисел с разделителями
//! ТОЛЬКО целые неотрицательные числа
//! возвращает True, если строка успешно преобразована
//------------------------------------------------------------------------------
function ConvertStr2IntArray(
  const AStr: string;
  const ADelimiter: Char;
  var RArray: TMyIntegerArray
): Boolean;

//------------------------------------------------------------------------------
//! выдать индекс строки в массиве строк
//! возвращает -1 если строка не найдена
//------------------------------------------------------------------------------
function IsStrInArray(
  const AStrArray: TMyStringArray;
  const AStr: string
): Integer;

//------------------------------------------------------------------------------
//! выдать индекс числа в массиве чисел
//! возвращает -1 если число не найдено
//------------------------------------------------------------------------------
function IsIntInArray(
  const AIntArray: TMyIntegerArray;
  const AInt: Integer
): Integer;

//------------------------------------------------------------------------------
//! вернуть строку даты в формате MS SQL сервера
//------------------------------------------------------------------------------
function SQLDate(
  const ADate: TDateTime
): string;

//------------------------------------------------------------------------------
//! вернуть строку времени в формате MS SQL сервера
//------------------------------------------------------------------------------
function SQLTime(
  const ATime: TDateTime
): string;

//------------------------------------------------------------------------------
//! вернуть строку даты и времени в формате MS SQL сервера
//------------------------------------------------------------------------------
function SQLDateTime(
  const ADateTime: TDateTime
): string;

//------------------------------------------------------------------------------
//! вернуть сформированную строку функции CONVERT даты для выполнения MS SQL сервером
//------------------------------------------------------------------------------
function SQLDateConvert(
  const ADate: TDateTime
): string;

//------------------------------------------------------------------------------
//! вернуть сформированную строку функции CONVERT времени для выполнения MS SQL сервером
//------------------------------------------------------------------------------
function SQLTimeConvert(
  const ATime: TDateTime
): string;

//------------------------------------------------------------------------------
//! вернуть сформированную строку функции CONVERT даты и времени для выполнения MS SQL сервером
//------------------------------------------------------------------------------
function SQLDateTimeConvert(
  const ADateTime: TDateTime
): string;

//------------------------------------------------------------------------------
//! изменить порядок байт в слове с я321 на я123
//------------------------------------------------------------------------------
function TransformColor(
  const AColor: Cardinal
): Cardinal; register;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! строковые константы преобразования для SQL-сервера
//------------------------------------------------------------------------------
  CSQLDate: string = 'yyyymmdd';
  CSQLTime: string = 'hh":"nn":"ss';
  CSQLDateTime: string = 'yyyymmdd hh:nn:ss';
  CSQLCDate: string = 'CONVERT(datetime, ''%s'', 112)';
  CSQLCTime: string = 'CONVERT(datetime, ''%s'', 108)';
  CSQLCDateTime: string = 'CONVERT(datetime, ''%s'', 120)';

procedure LockedOr8(
  AResult: Pointer;
  AArg: Pointer
);
asm
  mov   cl,[edx];
  lock  or [eax],cl;
end;

procedure LockedOr16(
  AResult: Pointer;
  AArg: Pointer
);
asm
  mov   cx,[edx];
  lock  or [eax],cx;
end;

procedure LockedOr32(
  AResult: Pointer;
  AArg: Pointer
);
asm
  mov   ecx,[edx];
  lock  or [eax],ecx;
end;

function Swap64(
  const AArg: UInt64
): UInt64;
asm
  lea   ecx,AArg;
  mov   edx,[ecx];
  mov   eax,[ecx + 4];
  bswap edx;
  bswap eax;
end;

function Swap32(
  const AArg: Cardinal
): Cardinal;
asm
  bswap eax;
end;

function Swap16(
  const AArg: Word
): Word;
asm
  xchg  ah, al;
end;

function ConvertStr2IntArray(
  const AStr: string;
  const ADelimiter: Char;
  var RArray: TMyIntegerArray
): Boolean;
var
  //!
  WorkStr: string;
  //!
  CurPos: Integer;
  //!
  CifNum: Integer;
  //!
  WorkChar: Char;
//------------------------------------------------------------------------------
begin
  Result := True;
  WorkStr := AStr + ADelimiter;
  CifNum := 0;
  CurPos := 1;
  while ( CurPos <= Length( WorkStr ) ) do
  begin
    WorkChar := WorkStr[CurPos];
    if ( WorkChar >= '0' ) and ( WorkChar <= '9' ) then
    begin // цифра
      Inc( CifNum );
    end
    else if ( WorkChar = ADelimiter ) then
    begin // разделитель
      if ( CifNum <> 0 ) then
      begin // были цифры
        SetLength( RArray, Length( RArray ) + 1 );
        RArray[High( RArray )] := StrToInt( Copy( WorkStr, CurPos - CifNum, CifNum ) );
        CifNum := 0;
      end;
    end
    else
    begin // что-то другое
      Exit( False );
    end;
    Inc( CurPos );
  end;
end;

function IsStrInArray(
  const AStrArray: TMyStringArray;
  const AStr: string
): Integer;
begin
  for Result := Low( AStrArray ) to High( AStrArray ) do
  begin
    if ( AStrArray[Result] = AStr ) then Exit;
  end;
  Result := -1;
end;

function IsIntInArray(
  const AIntArray: TMyIntegerArray;
  const AInt: Integer
): Integer;
begin
  for Result := Low( AIntArray ) to High( AIntArray ) do
  begin
    if ( AIntArray[Result] = AInt ) then Exit;
  end;
  Result := -1;
end;

function SQLDate(
  const ADate: TDateTime
): string;
begin
// формат: yyyymmdd
// использовать с: CONVERT(datetime, 'SQLDate', 112)
  Result := FormatDateTime( cSQLDate, ADate );
end;

function SQLTime(
  const ATime: TDateTime
): string;
begin
// формат: hh:mi:ss
// использовать с: CONVERT(datetime, 'SQLTime', 108)
  Result := FormatDateTime( cSQLTime, ATime );
end;

function SQLDateTime(
  const ADateTime: TDateTime
): string;
begin
// формат: yyyy-mm-dd hh:mi:ss
// использовать с: CONVERT(datetime, 'SQLDateTime', 120)
  Result := FormatDateTime( cSQLDateTime, ADateTime );
end;

function SQLDateConvert(
  const ADate: TDateTime
): string;
begin
  Result := SQLDate( ADate );
  Result := Format( cSQLCDate, [Result] );
end;

function SQLTimeConvert(
  const ATime: TDateTime
): string;
begin
  Result := SQLTime( ATime );
  Result := Format( cSQLCTime, [Result] );
end;

function SQLDateTimeConvert(
  const ADateTime: TDateTime
): string;
begin
  Result := SQLDateTime( ADateTime );
  Result := Format( cSQLCDateTime, [Result] );
end;

function TransformColor(
  const AColor: Cardinal
): Cardinal;
asm
  bswap eax;
  shr   eax,8;
end;

end.

