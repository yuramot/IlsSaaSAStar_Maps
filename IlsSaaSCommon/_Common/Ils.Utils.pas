unit Ils.Utils;
//------------------------------------------------------------------------------
// модуль маленьких полезных подпрограмм
//------------------------------------------------------------------------------
// содержит процедуры и функции, выполняющие сервисную работу
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Windows;

type
  TIlsDateTimeFormatVariant = (fvFull, fvTimeOnly, fvDateOnly);

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
//! получить номер минимального/максимального установленного бита
//! result = -1, если аргумент = 0
//------------------------------------------------------------------------------
function GetMinBitIndex(
  AArg: UInt64
): Integer;

function GetMaxBitIndex(
  AArg: UInt64
): Integer;

//------------------------------------------------------------------------------
//! получить текущие дату и время, как UTC
//------------------------------------------------------------------------------
function NowUTC(): TDateTime;

//------------------------------------------------------------------------------
//! дамп данных в шестнадцатиричном виде
//------------------------------------------------------------------------------
function LogDataHex(
  const AHeader: string;
  const AData: Pointer;
  const ADataLen: Integer
): string;

//------------------------------------------------------------------------------
//! дамп данных в символьном виде
//! *** не забывать, что для не-ansi байт будет использоваться текущая локаль ***
//------------------------------------------------------------------------------
function LogDataChar(
  const AHeader: string;
  const AData: Pointer;
  const ADataLen: Integer
): string;

//------------------------------------------------------------------------------
//! маскировка символов JSON
//------------------------------------------------------------------------------
function MaskJSONSymbols(
  const AStr: string
): string;

//------------------------------------------------------------------------------
//! конвертировать строку формата ИЛС в TDateTime
//------------------------------------------------------------------------------
function IlsToDateTime(
  const AStr: string;
  const ADefault: TDateTime = 0
): TDateTime;


function TryIlsToDateTime(
  const AStr: string;
  out ODT: TDateTime
): Boolean;

//------------------------------------------------------------------------------
//! конвертировать TDateTime в строку формата ИЛС
//------------------------------------------------------------------------------
function DateTimeToIls(
  const ADT: TDateTime;
  const AFormatVariant: TIlsDateTimeFormatVariant = fvFull
): string;

//------------------------------------------------------------------------------
//! TBD
//------------------------------------------------------------------------------
function TrimLeft(AStr: string; ACh: Char): string;

//------------------------------------------------------------------------------
implementation

{$IFDEF WIN32}

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

{$ELSE}

function Swap64(
  const AArg: UInt64
): UInt64;
begin
  Result := ((AArg and UInt64($ff00000000000000)) shr 56)
         or ((AArg and UInt64($00ff000000000000)) shr 40)
         or ((AArg and UInt64($0000ff0000000000)) shr 24)
         or ((AArg and UInt64($000000ff00000000)) shr 8)
         or ((AArg and UInt64($00000000ff000000)) shl 8)
         or ((AArg and UInt64($0000000000ff0000)) shl 24)
         or ((AArg and UInt64($000000000000ff00)) shl 40)
         or ((AArg and UInt64($00000000000000ff)) shl 56);
end;

function Swap32(
  const AArg: Cardinal
): Cardinal;
begin
  Result := ((AArg and $ff000000) shr 24)
         or ((AArg and $00ff0000) shr 8)
         or ((AArg and $0000ff00) shl 8)
         or ((AArg and $000000ff) shl 24);
end;

function Swap16(
  const AArg: Word
): Word;
begin
  Result := (Word(Lo(AArg)) shl 8) or Hi(AArg);
end;

{$ENDIF}

function GetMinBitIndex(
  AArg: UInt64
): Integer;
begin
  if (AArg = 0) then
    Exit(-1);
  Result := 0;
  while not Odd(AArg) do
  begin
    Inc(Result);
    AArg := AArg shr 1;
  end;
end;

function GetMaxBitIndex(
  AArg: UInt64
): Integer;
begin
  Result := -1;
  while (AArg <> 0) do
  begin
    Inc(Result);
    AArg := AArg shr 1;
  end;
end;

function NowUTC(): TDateTime;
var
  //!
  SystemTime: TSystemTime;
//------------------------------------------------------------------------------
begin
  GetSystemTime(SystemTime);
  with SystemTime do Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;

function LogDataHex(
  const AHeader: string;
  const AData: Pointer;
  const ADataLen: Integer
): string;
var
  //!
  DataPos: PByte;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  Result := AHeader + ' (байт = ' + IntToStr(ADataLen) + ')';
  if (ADataLen = 0) then
    Exit;
  Result := Result + #13#10;
  DataPos := AData;
  for I := 1 to ADataLen do
  begin
    Result := Result + '(' + IntToHex(DataPos^, 2) + ')';
    if (I mod 16 = 0) then
      Result := Result + #13#10;
    Inc(DataPos);
  end;
end;

function LogDataChar(
  const AHeader: string;
  const AData: Pointer;
  const ADataLen: Integer
): string;
var
  //!
  TempAnsi: AnsiString;
//------------------------------------------------------------------------------
begin
  Result := AHeader + ' (байт = ' + IntToStr(ADataLen) + ')';
  if (ADataLen = 0) then Exit;
  SetLength(TempAnsi, ADataLen);
  MoveMemory(@TempAnsi[1], AData, ADataLen);
  Result := Result + #13#10 + string(TempAnsi);
end;

function MaskJSONSymbols(
  const AStr: string
): string;
begin
  Result := StringReplace(StringReplace(AStr, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll])
end;

function IlsToDateTime(
  const AStr: string;
  const ADefault: TDateTime = 0
): TDateTime;
var
  //!
  FS: TFormatSettings;
//------------------------------------------------------------------------------
begin
  FS.DateSeparator := '-';
  FS.TimeSeparator := ':';
  FS.LongTimeFormat := 'hh:nn:ss.zzz';
  FS.ShortTimeFormat := 'hh:nn:ss.zzz';
  FS.LongDateFormat := 'yyyy-mm-dd';
  FS.ShortDateFormat := 'yyyy-mm-dd';
  FS.DecimalSeparator := '.';
  Result := StrToDateTimeDef(AStr, ADefault, FS);
end;

function TryIlsToDateTime(
  const AStr: string;
  out ODT: TDateTime
): Boolean;
var
  //!
  FS: TFormatSettings;
//------------------------------------------------------------------------------
begin
  FS.DateSeparator := '-';
  FS.TimeSeparator := ':';
  FS.LongTimeFormat := 'hh:nn:ss.zzz';
  FS.ShortTimeFormat := 'hh:nn:ss.zzz';
  FS.LongDateFormat := 'yyyy-mm-dd';
  FS.ShortDateFormat := 'yyyy-mm-dd';
  FS.DecimalSeparator := '.';
  Result := TryStrToDateTime(AStr, ODT, FS);
end;

function DateTimeToIls(
  const ADT: TDateTime;
  const AFormatVariant: TIlsDateTimeFormatVariant = fvFull
): string;
begin
  case AFormatVariant of
    fvDateOnly : Result := 'yyyy-mm-dd';
    fvTimeOnly : Result := 'hh:nn:ss.zzz';
    else Result := 'yyyy-mm-dd hh:nn:ss.zzz';
  end;
  Result := FormatDateTime(Result, ADT);
end;

function TrimLeft(AStr: string; ACh: Char): string;
begin
  Result := AStr;
  while (Length(Result) > 0) and (Result[1] = aCh) do
    Delete(Result, 1, 1);
end;

end.

