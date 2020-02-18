unit E_GeoHash;
//------------------------------------------------------------------------------
// реализация гео-хэширования
//------------------------------------------------------------------------------
// HashEncode - процедура
//  кодирование массива точек в строку со списком гео-хэшей
//
// аргументы:
//  ACoords - массив точек
//  ARegion - строка, задающая регион кодирования
//    'GR' - Россия, 8 символов
//    'GQ' - северо-восточная часть ЗШ, 10 символов
//    'GW' - весь ЗШ, 12 символов
//  RHash - результирующая строка с префиксом региона и списком гео-хэшей
//
// !!! ВНИМАНИЕ: координаты точек не проверяются на совпадение с заданным регионом !!!
//
// HashDecode - функция
//  декодирование строки со списком гео-хэшей в массив точек
// аргументы:
//  AHash - строка с префиксом региона и списком гео-хэшей
//  КCoords - массив точек (точки _добавляются_ в этот массив)
// результат:
//  True, если раскодирование прошло успешно
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  T_Points;

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
procedure HashEncode(
  const ACoords: TGeoPointArray;
  const ARegion: string;
  var RHash: string
);

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
function HashDecode(
  const AHash: string;
  var RCoords: TGeoPointArray
): Boolean;

//------------------------------------------------------------------------------
implementation

const
//------------------------------------------------------------------------------
//! массив конвертации букв в номера
//------------------------------------------------------------------------------
  CHashFromLetters: array[48..122] of Integer = (
     0,  1,  2,  3,  4,  5,  6,  7, // 30-37, '0'..'7'
     8,  9, -1, -1, -1, -1, -1, -1, // 38-3F, '8','9'
    -1, -1, 10, 11, 12, 13, 14, 15, // 40-47, 'B'..'G'
    16, -1, 17, 18, -1, 19, 20, -1, // 48-4F, 'H','J','K','M','N'
    21, 22, 23, 24, 25, 26, 27, 28, // 50-57, 'P'..'W'
    29, 30, 31, -1, -1, -1, -1, -1, // 58-5F, 'X'..'Z'
    -1, -1, 10, 11, 12, 13, 14, 15, // 60-67, 'b'..'g'
    16, -1, 17, 18, -1, 19, 20, -1, // 68-6F, 'h','j','k','m','n'
    21, 22, 23, 24, 25, 26, 27, 28, // 70-77, 'p'..'w'
    29, 30, 31                      // 78-7A, 'x'..'z'
  );

//------------------------------------------------------------------------------
//! массив конвертации номеров в буквы
//------------------------------------------------------------------------------
  CHashToLetters: array[0..31] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'j', 'k', 'm', 'n', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  );

//------------------------------------------------------------------------------
//! конвертируем букву в номер
//------------------------------------------------------------------------------
function Letter2Num(
  const ALetter: Char
): Integer;
begin
  if ( Ord( ALetter ) < Low( CHashFromLetters ) ) or ( Ord( ALetter ) > High( CHashFromLetters ) ) then
    Result := -1
  else
    Result := CHashFromLetters[Ord( ALetter )];
end;

//------------------------------------------------------------------------------
//! внутренняя прцедура кодирования точки в гео-хэш
//------------------------------------------------------------------------------
procedure EncodePoint(
  const ACoords: TGeoPoint;
  const ABoundMin: TGeoPoint;
  const ABoundMax: TGeoPoint;
  const APerc: Integer;
  var RHash: string
);
var
  //! рабочие переменные
  WorkBoundMin, WorkBoundMax: TGeoPoint;
  //! рабочие переменные
  LatiMid, LongiMid: Double;
  //! бинарное представление хэша
  BinHash: Integer;
  //! номер бита
  Bit: Integer;
  //! позиция в массиве Rez
  Poz: Integer;
  //! массив хранящий генерируемый код
  Rez: array[0..12] of Char;
  //! флаг чётного бита
  EvenBit: Boolean;
//------------------------------------------------------------------------------
begin
  WorkBoundMin := ABoundMin;
  WorkBoundMax := ABoundMax;
  BinHash := 0;
  Bit := 5;
  EvenBit := False;
  FillChar( Rez, SizeOf( Rez ), #0 );
  Poz := 0;
  while ( Poz < APerc ) do
  begin
    BinHash := BinHash shl 1;
    EvenBit := not EvenBit;
    if EvenBit then
    begin
      LongiMid := ( WorkBoundMin.Longitude + WorkBoundMax.Longitude ) * 0.5;
      if ( ACoords.Longitude >= LongiMid ) then
      begin
        Inc( BinHash );
        WorkBoundMin.Longitude := LongiMid;
      end
      else
        WorkBoundMax.Longitude := LongiMid;
    end
    else
    begin
      LatiMid := ( WorkBoundMin.Latitude + WorkBoundMax.Latitude ) * 0.5;
      if ( ACoords.Latitude >= LatiMid ) then
      begin
        Inc( BinHash );
        WorkBoundMin.Latitude := LatiMid;
      end
      else
        WorkBoundMax.Latitude := LatiMid;
    end;
    Dec( Bit );
    if ( Bit = 0 ) then
    begin
      Bit := 5;
      Rez[Poz] := CHashToLetters[BinHash];
      BinHash := 0;
      Inc( Poz );
    end;
  end;
  RHash := RHash + string( Rez );
end;

//------------------------------------------------------------------------------
//! внутренняя функция декодирования гео-хэша в точку
//------------------------------------------------------------------------------
function DecodePoint(
  const RHash: string;
  const ABoundMin: TGeoPoint;
  const ABoundMax: TGeoPoint;
  const APerc: Integer;
  var ACoords: TGeoPoint
): Boolean;
var
  //! рабочие переменные
  WorkBoundMin, WorkBoundMax: TGeoPoint;
  //! рабочие переменные
  LatiMid, LongiMid: Double;
  //! бинарное представление хэша
  BinHash: Integer;
  //! позиция бита
  BitPos: Integer;
  //! значение бита
  BitX: Integer;
  //! итератор по буквам
  WorkChar: Char;
  //! флаг чётного бита
  EvenBit: Boolean;
//------------------------------------------------------------------------------
begin
  Result := False;
  WorkBoundMin := ABoundMin;
  WorkBoundMax := ABoundMax;
  EvenBit := False;
  for WorkChar in RHash do
  begin
    BinHash := Letter2Num( WorkChar );
    if ( BinHash = -1 ) then Exit;
    for BitPos := 4 downto 0 do
    begin
      BitX := ( BinHash shr BitPos ) and 1;
      EvenBit := not EvenBit;
      if EvenBit then
      begin
        LongiMid := ( WorkBoundMin.Longitude + WorkBoundMax.Longitude ) * 0.5;
        if ( BitX <> 0 ) then
          WorkBoundMin.Longitude := LongiMid
        else
          WorkBoundMax.Longitude := LongiMid;
      end
      else
      begin
        LatiMid := ( WorkBoundMin.Latitude + WorkBoundMax.Latitude ) * 0.5;
        if ( BitX <> 0 ) then
          WorkBoundMin.Latitude := LatiMid
        else
          WorkBoundMax.Latitude := LatiMid;
      end;
    end;
  end;
  ACoords.Latitude := ( WorkBoundMin.Latitude + WorkBoundMax.Latitude ) * 0.5;
  ACoords.Longitude := ( WorkBoundMin.Longitude + WorkBoundMax.Longitude ) * 0.5;
  Result := True;
end;

//------------------------------------------------------------------------------
// interface
//------------------------------------------------------------------------------

procedure HashEncode(
  const ACoords: TGeoPointArray;
  const ARegion: string;
  var RHash: string
);
var
  //! итератор по точкам массива
  Iter: TGeoPoint;
  //! границы региона
  BoundMin, BoundMax: TGeoPoint;
  //! точность, символов
  Perc: Integer;
//------------------------------------------------------------------------------
begin
  RHash := ARegion;
  if ( ARegion = 'GR' ) then
  begin
    BoundMin.Latitude := 40;
    BoundMin.Longitude := 20;
    BoundMax.Latitude := 80;
    BoundMax.Longitude := 160;
    Perc := 8;
  end
  else if ( ARegion = 'GQ' ) then
  begin
    BoundMin.Latitude := 0;
    BoundMin.Longitude := 0;
    BoundMax.Latitude := 90;
    BoundMax.Longitude := 180;
    Perc := 10;
  end
  else if ( ARegion = 'GW' ) then
  begin
    BoundMin.Latitude := -90;
    BoundMin.Longitude := -180;
    BoundMax.Latitude := 90;
    BoundMax.Longitude := 180;
    Perc := 12;
  end
  else Exit;
  for Iter in ACoords do
  begin
    EncodePoint( Iter, BoundMin, BoundMax, Perc, RHash );
  end;
end;

function HashDecode(
  const AHash: string;
  var RCoords: TGeoPointArray
): Boolean;
var
  //! точность, символов
  Perc: Integer;
  //!
  TempInt: Integer;
  //!
  WorkStr: string;
  //!
  HashStr: string;
  //!
  Coords: TGeoPoint;
  //! границы региона
  BoundMin, BoundMax: TGeoPoint;
//------------------------------------------------------------------------------
begin
  Result := False;
  HashStr := Copy( AHash, 1, 2 );
  if ( HashStr = 'GR' ) then
  begin
    BoundMin.Latitude := 40;
    BoundMin.Longitude := 20;
    BoundMax.Latitude := 80;
    BoundMax.Longitude := 160;
    Perc := 8;
  end
  else if ( HashStr = 'GQ' ) then
  begin
    BoundMin.Latitude := 0;
    BoundMin.Longitude := 0;
    BoundMax.Latitude := 90;
    BoundMax.Longitude := 180;
    Perc := 10;
  end
  else if ( HashStr = 'GW' ) then
  begin
    BoundMin.Latitude := -90;
    BoundMin.Longitude := -180;
    BoundMax.Latitude := 90;
    BoundMax.Longitude := 180;
    Perc := 12;
  end
  else Exit;
  WorkStr := Copy( AHash, 3, MaxInt );
  if ( ( Length( WorkStr ) mod Perc ) <> 0 ) then Exit;
  repeat
    HashStr := Copy( WorkStr, 1, Perc );
    if not DecodePoint( HashStr, BoundMin, BoundMax, Perc, Coords ) then Exit;
    TempInt := Length( RCoords );
    SetLength( RCoords, TempInt + 1 );
    RCoords[TempInt] := Coords;
    WorkStr := Copy( WorkStr, Perc + 1, MaxInt );
    if ( WorkStr = '' ) then Break;
  until False;
  Result := True;
end;

end.

