﻿unit Geo.Hash;
//------------------------------------------------------------------------------
// реализация гео-хэширования
//------------------------------------------------------------------------------
//  набор фунций для гео-кодирования и декодирования
//
//  подробное описания функций - перед ними
//
//  ВАЖНО: все функции используют только общемировые ([-90;90],[-180;180]) координаты
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, StrUtils, Math,
  Geo.Pos;

//------------------------------------------------------------------------------
type

  TGeoHashValue = UInt32;//UInt64;

  TGeoHashStringFormat = (gfPath, gfSpeed, gfTypeAndZone, gfType, gfZone, gfLandMark, gfPointsArray);

const
  CGeoHashStringFormatInfoLen: array[TGeoHashStringFormat] of Integer = (0, 3, 18, 2, 16, 19, 0);
  CGeoHashStringFormatInfoPref: array[TGeoHashStringFormat] of string = ('GW', 'SL', 'TZ', 'TL', 'ZZ', 'LM', 'PA');

type
  TGeoPathPoint = record
    p: TGeoPos; //point
    s: Integer; //speed
    t: Integer; //type
    z: UInt64;  //zone
    f: UInt64;  //feature
    c: Boolean; //cross
  end;

  TGeoPathPointArray = TArray<TGeoPathPoint>;
//------------------------------------------------------------------------------
//! статический класс-инкапсулятор
//------------------------------------------------------------------------------
  TGeoHash = class
  public
//------------------------------------------------------------------------------
//! функция кодирования точки в бинарный гео-хэш
//------------------------------------------------------------------------------
    class function EncodePointBin(
      const ALat: Double;
      const ALong: Double
    ): Int64;
//------------------------------------------------------------------------------
//! функция кодирования точки в строковый гео-хэш
//!   APrec - кол-во символов
//!   !!! APrec ДОЛЖНО БЫТЬ в интервале 1-12, ПРОВЕРКИ НЕТ !!!
//------------------------------------------------------------------------------
    class function EncodePointString(
      const ALat: Double;
      const ALong: Double;
      const APrec: Integer
    ): string; overload;
    class function EncodePointString(
      const APos: TGeoPos;
      const APrec: Integer
    ): string; overload;
//------------------------------------------------------------------------------
//! функция конвертации бинарного гео-хэша в строковый
//!   APrec - кол-во символов
//!   !!! APrec ДОЛЖНО БЫТЬ в интервале 1-12, ПРОВЕРКИ НЕТ !!!
//------------------------------------------------------------------------------
    class function ConvertBinToString(
      const AHash: Int64;
      const APrec: Integer
    ): string;
//------------------------------------------------------------------------------
//! функция конвертации строкового гео-хэша в бинарный
//!   !!! длина строки ДОЛЖНА БЫТЬ в интервале 1-12, ПРОВЕРКИ НЕТ !!!
//------------------------------------------------------------------------------
    class function ConvertStringToBin(
      const AHash: string
    ): Int64;
//------------------------------------------------------------------------------
//! процедура декодирования бинарного гео-хэша в точку
//------------------------------------------------------------------------------
    class procedure DecodePointBin(
      const AHash: Int64;
      var RLat: Double;
      var RLong: Double
    ); overload;
//------------------------------------------------------------------------------
//! процедура декодирования бинарного гео-хэша в точку
//------------------------------------------------------------------------------
    class function DecodePointBin(
      const AHash: Int64
    ):TGeoPos; overload;
//------------------------------------------------------------------------------
//! функция декодирования строкового гео-хэша в точку
//!   !!! кол-во символов в строке ДОЛЖНО БЫТЬ в интервале 1-12, ПРОВЕРКИ НЕТ !!!
//!   return: true - если декодирование успешно; false - если нет
//------------------------------------------------------------------------------
    class function DecodePointString(
      const AHash: string;
      var RLat: Double;
      var RLong: Double
    ): Boolean; overload;
//------------------------------------------------------------------------------
//! функция декодирования строкового гео-хэша в точку
//!   !!! кол-во символов в строке ДОЛЖНО БЫТЬ в интервале 1-12, ПРОВЕРКИ НЕТ !!!
//!   return: TGeoPos
//------------------------------------------------------------------------------
    class function DecodePointString(
      const AHash: string
    ): TGeoPos; overload;
//------------------------------------------------------------------------------
//! функция кодирования массива точек в строку гео-хэша
//!   !!! строка будет содержать префикс GW + один хэш = 12 символов !!!
//------------------------------------------------------------------------------
    class function EncodeArrayWorld(
      const ACoords: TGeoPosArray;
      const AFormat: TGeoHashStringFormat = gfPath
    ): string;
//------------------------------------------------------------------------------
//! функция кодирования массива точек в строку гео-хэша
//!   префикса нет
//------------------------------------------------------------------------------
    class function EncodeArrayAny(
      const ACoords: TGeoPosArray;
      const APrec: Integer
    ): string;
//------------------------------------------------------------------------------
//! функция кодирования массива точек в строку гео-хэша
//!   префикс и формат зависят от параметра AFormat
//------------------------------------------------------------------------------
    class function EncodeArrayAnyFormat(
      const ACoords: TGeoPathPointArray;
      const APrec: Integer;
      const AFormat: TGeoHashStringFormat
    ): string;
//------------------------------------------------------------------------------
//! функция декодирования строки гео-хэша в массив точек
//!   !!! строка должна содержать префикс GW + один хэш = 12 символов !!!
//!   return: true - если декодирование успешно; false - если нет
//------------------------------------------------------------------------------
    class function DecodeArrayWorld(
      const AHash: string;
      var RCoords: TGeoPosArray
    ): Boolean;
//------------------------------------------------------------------------------
//! функция декодирования строки гео-хэша в массив точек
//!   префикса нет
//!   return: true - если декодирование успешно; false - если нет
//------------------------------------------------------------------------------
    class function DecodeArrayAny(
      const AHash: string;
      const APrec: Integer;
      var RCoords: TGeoPosArray
    ): Boolean;

    class procedure GetAreaBoundingBox(out aBoundMin, aBoundMax: TGeoPos);

    class function EncodePoint(const aPoint: TGeoPos; const aPrecision: Integer): TGeoHashValue; overload;
    class function EncodePoint(const aLatitude, aLongitude: Double; const aPrecision: Integer): TGeoHashValue; overload;
    class function EncodePoint(const aPoint, aBoundMin, aBoundMax: TGeoPos; const aPrecision: Integer): TGeoHashValue; overload;

    class function DecodePoint(
      const aHash: TGeoHashValue; const aBoundMin, aBoundMax: TGeoPos;
      const aPrecision: Integer; var aPoint, aPointBoundMin, aPointBoundMax: TGeoPos): Boolean; overload;
    class function DecodePoint(const aHash: TGeoHashValue; var aCoords: TGeoPos; const aPrecision: Integer): Boolean; overload;
    // > 0 - слева, < 0 - справа, = 0 - на векторе
    class function PosPointFromVector(APointVector, BPointVector, CPoint: TGeoPos): integer; //overload;
    class function GeoHashStringFormat(const AFormat: Integer): TGeoHashStringFormat;

  end;

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
  if (Ord(ALetter) < Low(CHashFromLetters)) or (Ord(ALetter) > High(CHashFromLetters)) then
    Result := -1
  else
    Result := CHashFromLetters[Ord(ALetter)];
end;

//------------------------------------------------------------------------------
//! TGeoHash
//------------------------------------------------------------------------------

class function TGeoHash.EncodePointBin(
  const ALat: Double;
  const ALong: Double
): Int64;
var
  //!
  WorkBoundMin, WorkBoundMax: TGeoPos;
  //!
  LatiMid, LongiMid: Double;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  WorkBoundMin.Latitude := -90;
  WorkBoundMin.Longitude := -180;
  WorkBoundMax.Latitude := 90;
  WorkBoundMax.Longitude := 180;
  Result := 0;
  for I := 59 downto 0 do
  begin
    Result := Result shl 1;
    if Odd(I) then
    begin
      LongiMid := (WorkBoundMin.Longitude + WorkBoundMax.Longitude) * 0.5;
      if (ALong >= LongiMid) then
      begin
        Inc(Result);
        WorkBoundMin.Longitude := LongiMid;
      end
      else
        WorkBoundMax.Longitude := LongiMid;
    end
    else
    begin
      LatiMid := (WorkBoundMin.Latitude + WorkBoundMax.Latitude) * 0.5;
      if (ALat >= LatiMid) then
      begin
        Inc(Result);
        WorkBoundMin.Latitude := LatiMid;
      end
      else
        WorkBoundMax.Latitude := LatiMid;
    end;
  end;
end;

class function TGeoHash.EncodePointString(
  const ALat: Double;
  const ALong: Double;
  const APrec: Integer
): string;
begin
  Result := ConvertBinToString(EncodePointBin(ALat, ALong), APrec);
end;

class function TGeoHash.EncodePointString(
  const APos: TGeoPos;
  const APrec: Integer
): string;
begin
  Result := EncodePointString(APos.Latitude, APos.Longitude, APrec);
end;

class function TGeoHash.ConvertBinToString(
  const AHash: Int64;
  const APrec: Integer
): string;
var
  //!
  Temp64: Int64;
  //!
  I: Integer;
  //!
  Rez: array[0..12] of Char;
//------------------------------------------------------------------------------
begin
  FillChar(Rez, SizeOf(Rez), 0);
  Temp64 := AHash shr ((12 - APrec) * 5);
  for I := APrec - 1 downto 0 do
  begin
    Rez[I] := CHashToLetters[Temp64 and $1f];
    Temp64 := Temp64 shr 5;
  end;
  Result := string(Rez);
end;

class function TGeoHash.ConvertStringToBin(
  const AHash: string
): Int64;
var
  //!
  C: Char;
  //!
  CRep: Integer;
//------------------------------------------------------------------------------
begin
  Result := 0;
  for C in AHash do
  begin
    CRep := Letter2Num(C);
    if (CRep = -1) then
      Exit(0);
    Result := Result shl 5;
    Result := Result + CRep;
  end;
  Result := Result shl ((12 - Length(AHash)) * 5);
end;

class procedure TGeoHash.DecodePointBin(
  const AHash: Int64;
  var RLat: Double;
  var RLong: Double
);
var
  //!
  WorkHash: Int64;
  //!
  WorkBoundMin, WorkBoundMax: TGeoPos;
  //!
  LatiMid, LongiMid: Double;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  WorkHash := AHash shl 4;
  WorkBoundMin.Latitude := -90;
  WorkBoundMin.Longitude := -180;
  WorkBoundMax.Latitude := 90;
  WorkBoundMax.Longitude := 180;
  for I := 59 downto 0 do
  begin
    if Odd(I) then
    begin
      LongiMid := (WorkBoundMin.Longitude + WorkBoundMax.Longitude) * 0.5;
      if (WorkHash < 0) then
        WorkBoundMin.Longitude := LongiMid
      else
        WorkBoundMax.Longitude := LongiMid;
    end
    else
    begin
      LatiMid := (WorkBoundMin.Latitude + WorkBoundMax.Latitude) * 0.5;
      if (WorkHash < 0) then
        WorkBoundMin.Latitude := LatiMid
      else
        WorkBoundMax.Latitude := LatiMid;
    end;
    WorkHash := WorkHash shl 1;
  end;
  RLat := (WorkBoundMin.Latitude + WorkBoundMax.Latitude) * 0.5;
  RLong := (WorkBoundMin.Longitude + WorkBoundMax.Longitude) * 0.5;
end;

class function TGeoHash.DecodePointBin(
  const AHash: Int64
):TGeoPos;
begin
  DecodePointBin(AHash, Result.Latitude, Result.Longitude);
end;

class function TGeoHash.DecodePointString(
  const AHash: string;
  var RLat: Double;
  var RLong: Double
): Boolean;
var
  //!
  WorkBoundMin, WorkBoundMax: TGeoPos;
  //!
  LatiMid, LongiMid: Double;
  //!
  BinHash: Integer;
  //!
  BitPos: Integer;
  //!
  EvenBit: Boolean;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  Result := False;
  WorkBoundMin.Latitude := -90;
  WorkBoundMin.Longitude := -180;
  WorkBoundMax.Latitude := 90;
  WorkBoundMax.Longitude := 180;
  EvenBit := False;
  for I := 1 to Length(AHash) do
  begin
    BinHash := Letter2Num(AHash[I]);
    if (BinHash = -1) then Exit;
    for BitPos := 4 downto 0 do
    begin
      EvenBit := not EvenBit;
      if EvenBit then
      begin
        LongiMid := (WorkBoundMin.Longitude + WorkBoundMax.Longitude) * 0.5;
        if Odd(BinHash shr BitPos) then
          WorkBoundMin.Longitude := LongiMid
        else
          WorkBoundMax.Longitude := LongiMid;
      end
      else
      begin
        LatiMid := (WorkBoundMin.Latitude + WorkBoundMax.Latitude) * 0.5;
        if Odd(BinHash shr BitPos) then
          WorkBoundMin.Latitude := LatiMid
        else
          WorkBoundMax.Latitude := LatiMid;
      end;
    end;
  end;
  RLat := (WorkBoundMin.Latitude + WorkBoundMax.Latitude) * 0.5;
  RLong := (WorkBoundMin.Longitude + WorkBoundMax.Longitude) * 0.5;
  Result := True;
end;

class function TGeoHash.DecodePointString(
  const AHash: string
): TGeoPos;
begin
  DecodePointString(AHash, Result.Latitude, Result.Longitude);
end;

class function TGeoHash.EncodeArrayWorld(
      const ACoords: TGeoPosArray;
      const AFormat: TGeoHashStringFormat
): string;
var
  //!
  TempStr: string;
  //!
  MoveFrom, MoveTo: Pointer;
  //!
  I: Integer;
  APrefix: string;
//------------------------------------------------------------------------------
begin
  APrefix := CGeoHashStringFormatInfoPref[AFormat];
  Result := '';
  if Length(ACoords) = 0 then
    Exit;
  Result := APrefix;
  SetLength(Result, Length(ACoords) * 12 + 2);
  for I := Low(ACoords) to High(ACoords) do
  begin
    TempStr := EncodePointString(ACoords[I].Latitude, ACoords[I].Longitude, 12);
    MoveFrom := @(TempStr[1]);
    MoveTo := @(Result[I * 12 + 3]);
    Move(MoveFrom^, MoveTo^, 12 * SizeOf(Char));
  end;
end;

class function TGeoHash.EncodeArrayAny(
  const ACoords: TGeoPosArray;
  const APrec: Integer
): string;
var
  //!
  TempStr: string;
  //!
  MoveFrom, MoveTo: Pointer;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  SetLength(Result, Length(ACoords) * APrec);
  for I := Low(ACoords) to High(ACoords) do
  begin
    TempStr := EncodePointString(ACoords[I].Latitude, ACoords[I].Longitude, APrec);
    MoveFrom := @(TempStr[1]);
    MoveTo := @(Result[I * APrec + 1]);
    Move(MoveFrom^, MoveTo^, APrec * SizeOf(Char));
  end;
end;

class function TGeoHash.EncodeArrayAnyFormat(const ACoords: TGeoPathPointArray;
  const APrec: Integer; const AFormat: TGeoHashStringFormat): string;
var
  //!
  TempStr: string;
  //!
  MoveFrom, MoveTo: Pointer;
  //!
  I: Integer;
  //!
  PrefixLen: Integer;
  PointLen: Integer;

  AltInfoLen: Integer;
  AltInfo: string;
//------------------------------------------------------------------------------
begin
  case AFormat of
    gfPath: begin
      Result := 'GW';
      AltInfoLen := CGeoHashStringFormatInfoLen[AFormat];//0;
      PointLen := APrec + AltInfoLen;
    end;
    gfSpeed: begin
      Result := 'SL';
      AltInfoLen := CGeoHashStringFormatInfoLen[AFormat];//3; //speed = 000
      PointLen := APrec + AltInfoLen;
    end;
    gfType: begin
      Result := 'TL';
      AltInfoLen := CGeoHashStringFormatInfoLen[AFormat];//2; //speed = 000
      PointLen := APrec + AltInfoLen;
    end;
    gfZone: begin
      Result := 'ZZ';
      AltInfoLen := CGeoHashStringFormatInfoLen[AFormat];//16; //zone = 0000000000000000
      PointLen := APrec + AltInfoLen;
    end;
    gfTypeAndZone: begin
      Result := 'TZ';
      AltInfoLen := CGeoHashStringFormatInfoLen[AFormat];//2 + 16; //type = 00, zone=0000000000000000
      PointLen := APrec + AltInfoLen;
    end;
    gfLandMark: begin
      Result := 'LM';
      AltInfoLen := CGeoHashStringFormatInfoLen[AFormat];//1+2+16; //1 = landmark, type = 00, zone=0000000000000000
      PointLen := APrec + AltInfoLen;
    end;
    else begin
      Result := 'GW';
      AltInfoLen := 0;
      PointLen := APrec + AltInfoLen;
    end;
  end;

  PrefixLen := Length(Result);
  SetLength(Result, Length(ACoords) * PointLen - AltInfoLen + PrefixLen);
  for I := Low(ACoords) to High(ACoords) do
  begin
    AltInfo := '';
    if I > Low(ACoords) then
      case AFormat of
        gfSpeed: AltInfo := Format('%.3d', [ACoords[I].s]);
        gfType: AltInfo := Format('%.2d', [ACoords[I].t]);
        gfZone: AltInfo := IntToHex(ACoords[I].z, 16);
        gfTypeAndZone: AltInfo := IntToHex(ACoords[I].t, 2) + IntToHex(ACoords[I].z, 16); //2 - type, 16 - zone
        gfLandMark: AltInfo := IfThen(ACoords[I].c, 'Z', 'N') + IntToHex(ACoords[I].t, 2) + IntToHex(ACoords[I].z, 16); //1 - landmark, 2 - type, 16 - zone
      end;
    TempStr := AltInfo + EncodePointString(ACoords[I].p.Latitude, ACoords[I].p.Longitude, APrec);
    MoveFrom := @(TempStr[1]);
    MoveTo := @(Result[IfThen(I=0, 0, (I-1)*PointLen+APrec) + PrefixLen + 1]);
//    MoveTo := @(Result[I * PointLen + PrefixLen + 1]);
    Move(MoveFrom^, MoveTo^, IfThen(I=0, APrec, PointLen) * SizeOf(Char));
  end;
end;

class function TGeoHash.DecodeArrayWorld(
  const AHash: string;
  var RCoords: TGeoPosArray
): Boolean;
var
  WorkStr: string;
  HashType: string;
  InfoLen: integer;
  I: Integer;
  SFormat: TGeoHashStringFormat;
//------------------------------------------------------------------------------
begin
  SetLength(RCoords, 0);
  if (Length(AHash) = 2) then Exit(True);
  Result := False;
  if (Length(AHash) < 2) then Exit;
  HashType := copy(AHash, 1, 2);
  InfoLen := 0;
  for SFormat := Low(CGeoHashStringFormatInfoPref) to High(CGeoHashStringFormatInfoPref) do
    if HashType = CGeoHashStringFormatInfoPref[SFormat] then
      InfoLen := 12 + CGeoHashStringFormatInfoLen[SFormat];
  if InfoLen = 0 then Exit;

  if (((Length(AHash) - 2 - 12) mod InfoLen) <> 0) then Exit;
  WorkStr := Copy(AHash, 3, MaxInt);
  SetLength(RCoords, ((Length(WorkStr)-12) div InfoLen)+1);
  for I := Low(RCoords) to High(RCoords) do
  begin
    if not DecodePointString(
      Copy(WorkStr, I * InfoLen + 1, 12),
      RCoords[I].Latitude,
      RCoords[I].Longitude
    ) then Exit;
  end;
  Result := True;
end;

class function TGeoHash.DecodeArrayAny(
  const AHash: string;
  const APrec: Integer;
  var RCoords: TGeoPosArray
): Boolean;
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  SetLength(RCoords, 0);
  Result := False;
  if ((APrec < 1) or (APrec > 12)) then Exit;
  if ((Length(AHash) mod APrec) <> 0) then Exit;
  SetLength(RCoords, (Length(AHash) div APrec));
  for I := Low(RCoords) to High(RCoords) do
  begin
    if not DecodePointString(
      Copy(AHash, (I + 1) * APrec, APrec),
      RCoords[I].Latitude,
      RCoords[I].Longitude
    ) then Exit;
  end;
  Result := True;
end;

class function TGeoHash.EncodePoint(const aPoint, aBoundMin, aBoundMax: TGeoPos; const aPrecision: Integer): TGeoHashValue;
var
  WorkBoundMin, WorkBoundMax: TGeoPos;
  LatiMid, LongiMid: Double;
  i: Integer;
  EvenBit: Boolean;
begin
  WorkBoundMin := aBoundMin;
  WorkBoundMax := aBoundMax;
  Result := 0;
  EvenBit := False;
  i := 0;
  while (i < aPrecision) do
  begin
    Result := Result shl 1;
    EvenBit := not EvenBit;
    if EvenBit then
    begin
      LongiMid := (WorkBoundMin.Longitude + WorkBoundMax.Longitude) * 0.5;
      if (aPoint.Longitude >= LongiMid) then
      begin
        Inc(Result);
        WorkBoundMin.Longitude := LongiMid;
      end
      else
        WorkBoundMax.Longitude := LongiMid;
    end
    else
    begin
      LatiMid := (WorkBoundMin.Latitude + WorkBoundMax.Latitude) * 0.5;
      if (aPoint.Latitude >= LatiMid) then
      begin
        Inc(Result);
        WorkBoundMin.Latitude := LatiMid;
      end
      else
        WorkBoundMax.Latitude := LatiMid;
    end;
      Inc(i);
  end;
end;

class function TGeoHash.EncodePoint(const aPoint: TGeoPos; const aPrecision: Integer): TGeoHashValue;
var
  BoundMin, BoundMax: TGeoPos;
begin
  GetAreaBoundingBox(BoundMin, BoundMax);
  Result := EncodePoint(aPoint, BoundMin, BoundMax, aPrecision);
end;

class function TGeoHash.EncodePoint(const aLatitude, aLongitude: Double; const aPrecision: Integer): TGeoHashValue;
begin
  Result := EncodePoint(TGeoPos.Create(aLatitude, aLongitude), aPrecision);
end;

class function TGeoHash.DecodePoint(const aHash: TGeoHashValue;
  var aCoords: TGeoPos; const aPrecision: Integer): Boolean;
var
  BoundMin, BoundMax: TGeoPos;
  PointBoundMin, PointBoundMax: TGeoPos;
begin
  GetAreaBoundingBox(BoundMin, BoundMax);
  Result := DecodePoint(aHash, BoundMin, BoundMax, aPrecision, aCoords, PointBoundMin, PointBoundMax);
end;

class function TGeoHash.DecodePoint(const aHash: TGeoHashValue; const aBoundMin,
  aBoundMax: TGeoPos; const aPrecision: Integer; var aPoint, aPointBoundMin,
  aPointBoundMax: TGeoPos): Boolean;
var
  LatiMid, LongiMid: Double;
  BitPos: Integer;
  BitX: Integer;
  EvenBit: Boolean;
begin
  aPointBoundMin := aBoundMin;
  aPointBoundMax := aBoundMax;
  EvenBit := False;

  for BitPos := aPrecision - 1 downto 0 do
  begin
    BitX := (aHash shr BitPos) and 1;
    EvenBit := not EvenBit;
    if EvenBit then
    begin
      LongiMid := (aPointBoundMin.Longitude + aPointBoundMax.Longitude) * 0.5;
      if BitX <> 0 then
        aPointBoundMin.Longitude := LongiMid
      else
        aPointBoundMax.Longitude := LongiMid;
    end
    else
    begin
      LatiMid := (aPointBoundMin.Latitude + aPointBoundMax.Latitude) * 0.5;
      if BitX <> 0 then
        aPointBoundMin.Latitude := LatiMid
      else
        aPointBoundMax.Latitude := LatiMid;
    end;
  end;

  aPoint.Latitude := ( aPointBoundMin.Latitude + aPointBoundMax.Latitude ) * 0.5;
  aPoint.Longitude := ( aPointBoundMin.Longitude + aPointBoundMax.Longitude ) * 0.5;
  Result := True;
end;

class function TGeoHash.PosPointFromVector(APointVector, BPointVector, CPoint: TGeoPos): integer;
var
  s: Double;
begin
  s := (BPointVector.Longitude - APointVector.Longitude) *
       (CPoint.Latitude - APointVector.Latitude) -
       (BPointVector.Latitude - APointVector.Latitude) *
       (CPoint.Longitude - APointVector.Longitude);
{
          aLinesSort.Estimation := (P1.Longitude - pRight.Point.Longitude) *
                                   (pCenter.Point.Latitude - pRight.Point.Latitude) -
                                   (P1.Latitude - pRight.Point.Latitude) *
                                   (pCenter.Point.Longitude - pRight.Point.Longitude);
 }
  if s > 0 then
    Result := 1
  else
  if s < 0 then
    Result := -1
  else
    Result := 0;
end;

class function TGeoHash.GeoHashStringFormat(
  const AFormat: Integer): TGeoHashStringFormat;
begin
  Result := gfPath;
  if (AFormat >= Ord(Low(TGeoHashStringFormat))) and (AFormat <= Ord(High(TGeoHashStringFormat))) then
    Result := TGeoHashStringFormat(AFormat);
end;

class procedure TGeoHash.GetAreaBoundingBox(out aBoundMin, aBoundMax: TGeoPos);
begin
{  aBoundMin := TGeoPos.Create(-90, -180);
  aBoundMax := TGeoPos.Create(90, 180);}
  aBoundMin.Latitude := -90;
  aBoundMin.Longitude := -180;
  aBoundMax.Latitude := 90;
  aBoundMax.Longitude := 180;
end;

end.

