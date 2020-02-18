unit E_UtilsGeo;
//------------------------------------------------------------------------------
// Модуль гео-подпрограмм
//------------------------------------------------------------------------------
// Содержит процедуры и функции для работы с гео-координатами (в широком смысле)
//
// *** аргументы указываются в градусах, если не указано иное ***
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Math,
  T_Points, E_UtilsStr;

//------------------------------------------------------------------------------
//! перевести строку WZ в массив координат
//! формат строки <широта1>W<долгота1>Z...<широтаN>W<долготаN>Z
//------------------------------------------------------------------------------
procedure ParseWZ(
  const AStr: string;
  var RArray: TGeoPointArray
);

//------------------------------------------------------------------------------
//! рассчитать дистанцию (в километрах) по гео-координатам
//------------------------------------------------------------------------------
function GeoLength(
  const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double
): Double;

//------------------------------------------------------------------------------
//! рассчитать дистанцию по гео-координатам
//! использует медленный, но точный метод
//! результат будет в тех же единицах, что и указанный радиус
//! *** принимает аргументы в радианах ***
//------------------------------------------------------------------------------
function GeoLengthLongRad(
  const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double;
  const ARadius: Double
): Double;

//------------------------------------------------------------------------------
//! получить пиксельные координаты по гео-координатам
//------------------------------------------------------------------------------
procedure GeoConvToFixed(
  const AScale: Double;
  const AFromLat, AFromLong: Double;
  const ACenterLat, ACenterLong: Double;
  const ACenterX, ACenterY: Integer;
  var RToX, RToY: Integer
);

//------------------------------------------------------------------------------
//! получить пиксельные координаты по гео-координатам для Graphics32
//! TFloat в Graphics32 = Single
//------------------------------------------------------------------------------
procedure GeoConvToFixedSingle(
  const AScale: Double;
  const AFromLat, AFromLong: Double;
  const ACenterLat, ACenterLong: Double;
  const ACenterX, ACenterY: Integer;
  var RToX, RToY: Single
);

//------------------------------------------------------------------------------
//! получить гео-координатам по пиксельным координатам
//------------------------------------------------------------------------------
procedure GeoConvFromFixed(
  const AScale: Double;
  const AFromX, AFromY: Integer;
  const ACenterX, ACenterY: Integer;
  const ACenterLat, ACenterLong: Double;
  var RToLat, RToLong: Double
);

//------------------------------------------------------------------------------
//! рассчитать угол направления по точкам
//------------------------------------------------------------------------------
function DirectionAngle(
  const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double
): Double;

//------------------------------------------------------------------------------
implementation

procedure ParseWZ(
  const AStr: string;
  var RArray: TGeoPointArray
);
var
  //!
  PozW, PozZ: integer;
  //!
  StrW, StrZ: string;
  //!
  WorkStr: string;
//------------------------------------------------------------------------------
begin
  SetLength( RArray, 0 );
  WorkStr := AStr;
  repeat
    PozW := PosFirstDelimiter( WorkStr, 'W' );
    PozZ := PosFirstDelimiter( WorkStr, 'Z' );
    if ( PozW = 0 ) or ( PozZ = 0 ) then Exit;
    StrW := Copy( WorkStr, 1, PozW - 1 );
    StrZ := Copy( WorkStr, PozW + 1, PozZ - PozW - 1 );
    SetLength( RArray, Length( RArray ) + 1 );
    RArray[High( RArray )].Latitude := StrToFloat( StrW );
    RArray[High( RArray )].Longitude := StrToFloat( StrZ );
    WorkStr := Copy( WorkStr, PozZ + 1, MaxInt );
  until False;
end;

function GeoLength(
  const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double
): Double;
begin
  //*** 111.225 - это среднее значение ((меридиан + экватор) / 2), в реальности оно будет больше на экваторе
  //*** но для наших целей такого приближения достаточно
  Result := 111.225 * Hypot( ALatitudeFrom - ALatitudeTo, ( ALongitudeFrom - ALongitudeTo ) * Cos( DegToRad( ALatitudeFrom ) ) );
end;

function GeoLengthLongRad(
  const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double;
  const ARadius: Double
): Double;
begin
  Result := ArcCos( Sin( ALatitude1 ) * Sin( ALatitude2 ) + Cos( ALatitude1 ) * Cos( ALatitude2 ) * Cos( ALongitude1 - ALongitude2 ) ) * ARadius;
end;

procedure GeoConvToFixed(
  const AScale: Double;
  const AFromLat, AFromLong: Double;
  const ACenterLat, ACenterLong: Double;
  const ACenterX, ACenterY: Integer;
  var RToX, RToY: Integer
);
begin
  RToX := Trunc( ( AFromLong - ACenterLong ) * AScale ) + ACenterX;
  RToY := Trunc( ( ACenterLat - AFromLat ) * AScale / Cos( DegToRad( ( AFromLat + ACenterLat ) * 0.5 ) ) ) + ACenterY;
end;

procedure GeoConvToFixedSingle(
  const AScale: Double;
  const AFromLat, AFromLong: Double;
  const ACenterLat, ACenterLong: Double;
  const ACenterX, ACenterY: Integer;
  var RToX, RToY: Single
);
begin
  RToX := Int( ( AFromLong - ACenterLong ) * AScale + ACenterX + 1 );
  RToY := Int( ( ACenterLat - AFromLat ) * AScale / Cos( DegToRad( ( AFromLat + ACenterLat ) * 0.5 ) ) + ACenterY + 1 );
end;

procedure GeoConvFromFixed(
  const AScale: Double;
  const AFromX, AFromY: Integer;
  const ACenterX, ACenterY: Integer;
  const ACenterLat, ACenterLong: Double;
  var RToLat, RToLong: Double
);
begin
  RToLat := Cos( DegToRad( ACenterLat ) ) * ( ACenterY - AFromY ) / AScale + ACenterLat;
  RToLong := ( AFromX - ACenterX ) / AScale + ACenterLong;
end;

function DirectionAngle(
  const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double
): Double;
begin
  Result := ArcTan( ( ALatitudeTo - ALatitudeFrom ) / ( ( ALongitudeTo - ALongitudeFrom ) * Cos( DegToRad( ALatitudeTo ) ) ) );
  if ( ALongitudeTo < ALongitudeFrom ) then
  begin
    if ( ALatitudeTo > ALatitudeFrom ) then
      Result := Result + Pi
    else
      Result := Result - Pi;
  end;
end;

end.

