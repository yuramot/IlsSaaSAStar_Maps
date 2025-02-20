﻿unit Geo.Calcs;
//------------------------------------------------------------------------------
// модуль простых операций с гео координатами
//------------------------------------------------------------------------------
// функции с постфиксом Rad принимают аргументы в радианах
// функции с постфиксом Deg принимают аргументы в градусах
// функции без постфиксов принимают любые аргументы
//
// угловые результаты любых функций выражаются ТОЛЬКО в радианах
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Math,
  Geo.Pos;

type
  TGeoCalcs = class
  public
    // Проверка координат на корректность
    class function CheckGeoCoordinates(const AGeoPoint: TGeoPos): Boolean;
    // Привести координаты пары точек к общему виду (при переходе через линию дат; за основу берётся положительная долгота)
    class function CastGeoCoordinates(var AGeoPoint1, AGeoPoint2: TGeoPos): Boolean;
    // Привести координаты пары точек к общему виду (при переходе через линию дат; за основу берётся первая точка)
    class function CastGeoCoordinates2(var AGeoPoint1, AGeoPoint2: TGeoPos): Boolean;
    // Угловое расстояние между точками
    class function AngularLengthRadRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double; inline;
    class function AngularLengthDegRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double; overload; inline;
    class function AngularLengthDegRad(const APos1, APos2: TGeoPos): Double; overload; inline;
    // Расстояние между точками, км
    class function GeoLengthKmRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double; inline;
    class function GeoLengthKmDeg(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double; overload; inline;
    class function GeoLengthKmDeg(const APos1, APos2: TGeoPos): Double; overload; inline;
    // Расстояние между точками, м
    class function GeoLengthRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double; inline;
    class function GeoLengthDeg(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double; overload; inline;
    class function GeoLengthDeg(const APos1, APos2: TGeoPos): Double; overload; inline;
    // Получить радиус круга из трёх точек
    class function GetRadius(const APoint1, APoint2, APoint3: TGeoPos): Double;
    // Возвращает угол между направлением на север и данным вектором
    class function GetAzimuthRad(const AFromPos, AToPos: TGeoPos): Double;
    class function GetAzimuthDeg(const AFromPos, AToPos: TGeoPos): Double;
    // Возвращает угол между двумя векторами в радианах
    class function GetAngleRad(const APos1, APos2, APos3: TGeoPos): Double;
    // Физика
    class function GetSpeedKmH(const AFromPoint, AToPoint: TGeoTrackPoint): Double;
    class function GetSpeedMS(const AFromPoint, AToPoint: TGeoTrackPoint): Double;
    class function GetTangentialAcceleration(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
    // Угловая скорость, скорость по дуге
    class function GetAngularSpeed(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
    class function GetArcSpeed(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
    // Ускорение
    class function GetRadialAcceleration(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
    class function GetFullAcceleration(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
    // Проверка на правый поворот
    class function IsRightTurn(const ALatitude1, ALongitude1, ALatitude2, ALongitude2, ALatitude3, ALongitude3: Double): Boolean; overload;
    class function IsRightTurn(const APos1, APos2, APos3: TGeoPos): Boolean; overload;
    // Кратчайшее расстояние от точки до отрезка
    class function GeoDistancePointToLine(const APoint, APoint1, APoint2: TGeoPos; var APPoint: TGeoPos): Double;
    class function GeoDistancePointToLineM(const APoint, APoint1, APoint2: TGeoPos; var APPoint: TGeoPos): Double;
    class function GeoDistancePointToLineDeg(const ALatitudePoint, ALongitudePoint, ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
    class function GeoDistancePointToLineMDeg(const ALatitudePoint, ALongitudePoint, ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
    //
    class function GeoDistancePointToZoneMDeg(const ALatitudePoint, ALongitudePoint: Double; const AZone: TGeoPosArray): Double;
    class function GeoDistancePointToPolyLineMDeg(const ALatitudePoint, ALongitudePoint: Double; const APolyLine: TGeoPosArray): Double;
    // Попадание в зону
    class function GeoPointInZone(const ALatitude, ALongitude: Double; const AZone: TGeoPosArray): Boolean;
    // Рамки
    class function PointBounding(const ACenter: TGeoPos; const ARadius: Double): TGeoSquare;
    class function CircleBounding(const ACircle: TGeoCircle): TGeoSquare;
  end;

//------------------------------------------------------------------------------
implementation

class function TGeoCalcs.CheckGeoCoordinates(const AGeoPoint: TGeoPos): Boolean;
begin
  Result := AGeoPoint.IsValid();
end;

class function TGeoCalcs.CircleBounding(const ACircle: TGeoCircle): TGeoSquare;
begin
  Result := PointBounding(ACircle.Pos, ACircle.Radius);
end;

class function TGeoCalcs.CastGeoCoordinates(var AGeoPoint1, AGeoPoint2: TGeoPos): Boolean;
begin
  if not (CheckGeoCoordinates(AGeoPoint1) and CheckGeoCoordinates(AGeoPoint2)) then
    Exit(False);

  if AGeoPoint1.Longitude > AGeoPoint2.Longitude + 180 then
    AGeoPoint2.Longitude := AGeoPoint2.Longitude + 360
  else if AGeoPoint2.Longitude > AGeoPoint1.Longitude + 180 then
    AGeoPoint1.Longitude := AGeoPoint1.Longitude + 360;

  Result := True;
end;

class function TGeoCalcs.CastGeoCoordinates2(var AGeoPoint1,
  AGeoPoint2: TGeoPos): Boolean;
begin
  if not (CheckGeoCoordinates(AGeoPoint1) and CheckGeoCoordinates(AGeoPoint2)) then
    Exit(False);

  if Abs(AGeoPoint1.Longitude - AGeoPoint2.Longitude) > 180 then
    AGeoPoint2.Longitude := IfThen(AGeoPoint1.Longitude > AGeoPoint2.Longitude, AGeoPoint2.Longitude + 360, AGeoPoint2.Longitude - 360);

  Result := True;
end;

class function TGeoCalcs.AngularLengthRadRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
begin
  if (ALatitude1 = ALatitude2) and (ALongitude1 = ALongitude2) then
    Exit(0);

  Result:= Sin(ALatitude1) * Sin(ALatitude2) + Cos(ALatitude1) * Cos(ALatitude2) * Cos(ALongitude1 - ALongitude2);
  // защита от неточных вычислений
  if Abs(Result) <= 1 then
    Result := ArcCos(Result)
  else if Result > 1 then
    Result := 0
  else
    Result := Pi;
end;

class function TGeoCalcs.AngularLengthDegRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
begin
  Result := AngularLengthRadRad(DegToRad(ALatitude1), DegToRad(ALongitude1), DegToRad(ALatitude2), DegToRad(ALongitude2));
end;

class function TGeoCalcs.AngularLengthDegRad(const APos1, APos2: TGeoPos): Double;
begin
  Result := AngularLengthRadRad(DegToRad(APos1.Latitude), DegToRad(APos1.Longitude), DegToRad(APos2.Latitude), DegToRad(APos2.Longitude));
end;

class function TGeoCalcs.GeoLengthKmRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
begin
  Result := AngularLengthRadRad(ALatitude1, ALongitude1, ALatitude2, ALongitude2) * CEvEarthRadiusKm;
end;

class function TGeoCalcs.GeoLengthKmDeg(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
begin
  Result := AngularLengthDegRad(ALatitude1, ALongitude1, ALatitude2, ALongitude2) * CEvEarthRadiusKm;
end;

class function TGeoCalcs.GeoLengthKmDeg(const APos1, APos2: TGeoPos): Double;
begin
  Result := AngularLengthDegRad(APos1, APos2) * CEvEarthRadiusKm;
end;

class function TGeoCalcs.GeoLengthRad(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
begin
  Result := AngularLengthRadRad(ALatitude1, ALongitude1, ALatitude2, ALongitude2) * CEvEarthRadius;
end;

class function TGeoCalcs.GeoLengthDeg(const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
begin
  Result := AngularLengthDegRad(ALatitude1, ALongitude1, ALatitude2, ALongitude2) * CEvEarthRadius;
end;

class function TGeoCalcs.GeoLengthDeg(const APos1, APos2: TGeoPos): Double;
begin
  Result := AngularLengthDegRad(APos1, APos2) * CEvEarthRadius;
end;

class function TGeoCalcs.GetRadius(const APoint1, APoint2, APoint3: TGeoPos): Double;
var
  CastGeoPoint1, CastGeoPoint2, CastGeoPoint3: TGeoPos;
  Point1, Point2, Point3: TCartesianPos;
  S12, S23, S13: Double;
  RDenominator: Double;
begin
  CastGeoPoint1 := APoint1;
  CastGeoPoint2 := APoint2;
  CastGeoPoint3 := APoint3;
  CastGeoCoordinates(CastGeoPoint1, CastGeoPoint2);
  CastGeoCoordinates(CastGeoPoint1, CastGeoPoint3);
  Point1 := CastGeoPoint1.ToCartesianPos(APoint2.Longitude);
  Point2 := CastGeoPoint2.ToCartesianPos(APoint2.Longitude);
  Point3 := CastGeoPoint3.ToCartesianPos(APoint2.Longitude);
  S12 := Hypot(Point2.X - Point1.X, Point2.Y - Point1.Y);
  S13 := Hypot(Point3.X - Point1.X, Point3.Y - Point1.Y);
  S23 := Hypot(Point3.X - Point2.X, Point3.Y - Point2.Y);
  RDenominator := (Point2.X - Point1.X) * (Point3.Y - Point2.Y) - (Point2.Y - Point1.Y) * (Point3.X - Point2.X);
  if RDenominator = 0 then
    Result := Infinity
  else
    Result := S12 * S23 * S13 / 2 / Abs(RDenominator);
end;

class function TGeoCalcs.GetSpeedKmH(const AFromPoint, AToPoint: TGeoTrackPoint): Double;
var
  Point1, Point2: TGeoPos;
begin
  Point1 := AFromPoint.Pos.Pos;
  Point2 := AToPoint.Pos.Pos;
  if not CastGeoCoordinates(Point1, Point2) or (AFromPoint.DateTime = AToPoint.DateTime) then
    Exit(0);

  Result := GeoLengthKmDeg(Point1, Point2) / ((AToPoint.DateTime - AFromPoint.DateTime) * HoursPerDay);
end;

class function TGeoCalcs.GetSpeedMS(const AFromPoint, AToPoint: TGeoTrackPoint): Double;
var
  Point1, Point2: TGeoPos;
begin
  Point1 := AFromPoint.Pos.Pos;
  Point2 := AToPoint.Pos.Pos;
  if not CastGeoCoordinates(Point1, Point2) or (AFromPoint.DateTime = AToPoint.DateTime) then
    Exit(0);

  Result := GeoLengthDeg(Point1, Point2) / ((AToPoint.DateTime - AFromPoint.DateTime) * SecsPerDay);
end;

class function TGeoCalcs.GetTangentialAcceleration(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
var
  S12, S13, t12, t13: Double;
begin
  if (AFirstPoint.DateTime >= AMiddlePoint.DateTime)
  or (AMiddlePoint.DateTime >= ALastPoint.DateTime) then
    Exit(0);

  S12 := GeoLengthDeg(AFirstPoint.Pos.Pos.Latitude, AFirstPoint.Pos.Pos.Longitude, AMiddlePoint.Pos.Pos.Latitude, AMiddlePoint.Pos.Pos.Longitude);
  S13 := S12 + GeoLengthDeg(AMiddlePoint.Pos.Pos.Latitude, AMiddlePoint.Pos.Pos.Longitude, ALastPoint.Pos.Pos.Latitude, ALastPoint.Pos.Pos.Longitude);
  t12 := (AMiddlePoint.DateTime - AFirstPoint.DateTime) * SecsPerDay;
  t13 := (ALastPoint.DateTime - AFirstPoint.DateTime) * SecsPerDay;
  // формула для ускорения по трём точкам, считая движение равноускоренным
  Result := 2 * (S13 * t12 - S12 * t13) / (t12 * t13 * t13 - t13  * t12 * t12);
end;

class function TGeoCalcs.GetRadialAcceleration(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
var
  R, w: Double;
begin
  if (ALastPoint.DateTime - AMiddlePoint.DateTime) * SecsPerDay < 0.001 then
    Exit(0);

  w := GetAngularSpeed(AFirstPoint, AMiddlePoint, ALastPoint);
  R := GetRadius(AFirstPoint.Pos.Pos, AMiddlePoint.Pos.Pos, ALastPoint.Pos.Pos);
  if not IsInfinite(R) then
    Result := R * Sqr(w)
  else
    Result := 0;
end;

class function TGeoCalcs.GetFullAcceleration(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
begin
  Result := Hypot(GetTangentialAcceleration(AFirstPoint, AMiddlePoint, ALastPoint), GetRadialAcceleration(AFirstPoint, AMiddlePoint, ALastPoint))
end;

class function TGeoCalcs.GetAzimuthRad(const AFromPos, AToPos: TGeoPos): Double;
begin
  if AFromPos.Latitude = 90 then
    Exit(0);

  Result := GetAngleRad(TGeoPos.Create(90, 0), AFromPos, AToPos);
  if ((AToPos.Longitude < AFromPos.Longitude) and (Abs(AToPos.Longitude - AFromPos.Longitude) < 180))
  or ((AToPos.Longitude > AFromPos.Longitude) and (Abs(AToPos.Longitude - AFromPos.Longitude) > 180)) then
    Result := -Result;

  // перевод из формата (-pi; pi] в формат [0; 2*pi)
  if Result < 0 then
    Result := Result + 2 * Pi;
end;

class function TGeoCalcs.GetAzimuthDeg(const AFromPos, AToPos: TGeoPos): Double;
begin
  Result := RadToDeg(GetAzimuthRad(AFromPos, AToPos));
end;

class function TGeoCalcs.GetAngleRad(const APos1, APos2, APos3: TGeoPos): Double;
var
  AngLen12, AngLen23, AngLen13: Double;
begin
  if not CheckGeoCoordinates(APos1) or not CheckGeoCoordinates(APos2) or not CheckGeoCoordinates(APos3)
  or (APos1 = APos2) or (APos2 = APos3) or (APos1 = APos3) then
    Exit(0);

  AngLen12 := AngularLengthDegRad(APos1, APos2);
  AngLen23 := AngularLengthDegRad(APos2, APos3);
  AngLen13 := AngularLengthDegRad(APos1, APos3);

  if (AngLen12 = 0) or (AngLen23 = 0) then
    Exit(0);

  Result := (Cos(AngLen13) - Cos(AngLen12) * Cos(AngLen23)) / (Sin(AngLen12) * Sin(AngLen23));
  // защита от неточных вычислений
  if Abs(Result) <= 1 then
    Result := ArcCos(Result)
  else if Result > 1 then
    Result := 0
  else
    Result := Pi;
end;

class function TGeoCalcs.GetAngularSpeed(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
var
  ArcAngle: Double;
begin
  if (AFirstPoint.DateTime >= AMiddlePoint.DateTime) or (AMiddlePoint.DateTime >= ALastPoint.DateTime)
  or (AFirstPoint.Pos.Pos = AMiddlePoint.Pos.Pos) or (AMiddlePoint.Pos.Pos = ALastPoint.Pos.Pos) then
    Exit(0);

  ArcAngle := 2 * (Pi - GetAngleRad(AFirstPoint.Pos.Pos, AMiddlePoint.Pos.Pos, ALastPoint.Pos.Pos));
  Result := ArcAngle / ((ALastPoint.DateTime - AFirstPoint.DateTime) * SecsPerDay);
end;

class function TGeoCalcs.GetArcSpeed(const AFirstPoint, AMiddlePoint, ALastPoint: TGeoTrackPoint): Double;
begin
  Result := GetAngularSpeed(AFirstPoint, AMiddlePoint, ALastPoint) * GetRadius(AFirstPoint.Pos.Pos, AMiddlePoint.Pos.Pos, ALastPoint.Pos.Pos);
end;

class function TGeoCalcs.IsRightTurn(const ALatitude1, ALongitude1, ALatitude2, ALongitude2, ALatitude3, ALongitude3: Double): Boolean;
begin
  Result := (ALatitude1 * ALongitude2 - ALongitude1 * ALatitude2
          +  ALatitude2 * ALongitude3 - ALongitude2 * ALatitude3
          +  ALatitude3 * ALongitude1 - ALongitude3 * ALatitude1) > 0;
end;

class function TGeoCalcs.IsRightTurn(const APos1, APos2, APos3: TGeoPos): Boolean;
begin
  Result := (APos1.Latitude * APos2.Longitude - APos1.Longitude * APos2.Latitude
          +  APos2.Latitude * APos3.Longitude - APos2.Longitude * APos3.Latitude
          +  APos3.Latitude * APos1.Longitude - APos3.Longitude * APos1.Latitude) > 0;
end;

class function TGeoCalcs.PointBounding(const ACenter: TGeoPos;
  const ARadius: Double): TGeoSquare;
begin
  if (ACenter.Latitude < -90)
      or (ACenter.Latitude > 90)
      or (ACenter.Longitude <= -180)
      or (ACenter.Longitude > 180)
      or (ARadius > 1E6) then
  begin
    Result.NorthWest := TGeoPos.Create(0, 0);
    Result.SouthEast := TGeoPos.Create(0, 0);
    Exit;
  end;

  // Проверка на полюса
  if ACenter.Latitude + ARadius / CEvDegLength > 90 then
    Result.NorthWest.Latitude := 90
  else
    Result.NorthWest.Latitude := ACenter.Latitude + ARadius / CEvDegLength;

  if ACenter.Latitude - ARadius / CEvDegLength < -90 then
    Result.SouthEast.Latitude := -90
  else
    Result.SouthEast.Latitude := ACenter.Latitude - ARadius / CEvDegLength;

  Result.NorthWest.Longitude := ACenter.Longitude - ARadius / CEvDegLength / Cos(DegToRad(ACenter.Longitude));
  Result.SouthEast.Longitude := ACenter.Longitude + ARadius / CEvDegLength / Cos(DegToRad(ACenter.Longitude));

  if Result.NorthWest.Longitude <= -180 then
    Result.NorthWest.Longitude := Result.NorthWest.Longitude + 360;

  if Result.SouthEast.Longitude > 180 then
    Result.NorthWest.Longitude := Result.NorthWest.Longitude - 360;
end;

class function TGeoCalcs.GeoDistancePointToLine(const APoint, APoint1,
  APoint2: TGeoPos; var APPoint: TGeoPos): Double;
var
  x1, y1, x2, y2, CorrLon1, CorrLon2, Denominator, t: Double;
begin
  // нивелируем эффект прокрутки при пересечении линии дат
  if (APoint1.Longitude - APoint.Longitude > 180) then
    CorrLon1 := APoint1.Longitude - 360
  else if (APoint.Longitude - APoint1.Longitude > 180) then
    CorrLon1 := APoint1.Longitude + 360
  else
    CorrLon1 := APoint1.Longitude;
  if (APoint2.Longitude - APoint.Longitude > 180) then
    CorrLon2 := APoint2.Longitude - 360
  else if (APoint.Longitude - APoint2.Longitude > 180) then
    CorrLon2 := APoint2.Longitude + 360
  else
    CorrLon2 := APoint2.Longitude;
  // разворачиваем в декартову систему координат (с единицей "градус широты")
  // за начало координат берём точку, для которой ищется расстояние
  x1 := (CorrLon1 - APoint.Longitude) * Cos(DegToRad(APoint.Latitude));
  y1 := APoint1.Latitude - APoint.Latitude;
  x2 := (CorrLon2 - APoint.Longitude) * Cos(DegToRad(APoint.Latitude));
  y2 := APoint2.Latitude - APoint.Latitude;
  // сам расчёт
  if (x1 * (x1 - x2) + y1 * (y1 - y2) <= 0) then  // проекция точки оказалась ближе к точке 1 ИЛИ отрезок - это точка
  begin
    Result := Hypot(x1, y1);
    APPoint.Latitude := APoint1.Latitude;
    APPoint.Longitude := APoint1.Longitude;
  end else
  if (x2 * (x2 - x1) + y2 * (y2 - y1) < 0) then  // проекция точки оказалась ближе к точке 2
  begin
    Result := Hypot(x2, y2);
    APPoint.Latitude := APoint2.Latitude;
    APPoint.Longitude := APoint2.Longitude;
  end else                                                // проекция попадает на отрезок
  begin
    Result := Abs(x2 * y1 - y2 * x1) / Hypot((y2 - y1), (x2 - x1));
    Denominator := (CorrLon2 - CorrLon1) * (CorrLon2 - CorrLon1) +
                    (APoint2.Latitude - APoint1.Latitude) * (APoint2.Latitude - APoint1.Latitude);
    if (Denominator = 0) then
    begin
      APPoint.Latitude := APoint1.Latitude;
      APPoint.Longitude := APoint1.Longitude;
      Exit;
    end;
    t := (APoint.Longitude * (CorrLon2 - CorrLon1) -
         (CorrLon2 - CorrLon1) * CorrLon1 +
         APoint.Latitude * (APoint2.Latitude - APoint1.Latitude) -
         (APoint2.Latitude - APoint1.Latitude) * APoint1.Latitude) / Denominator;
    APPoint.Longitude := CorrLon1 + (CorrLon2 - CorrLon1) * t;
    APPoint.Latitude := APoint1.Latitude + (APoint2.Latitude - APoint1.Latitude) * t;
  end;
end;

class function TGeoCalcs.GeoDistancePointToLineDeg(const ALatitudePoint, ALongitudePoint, ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double): Double;
var
  x1, y1, x2, y2, CorrLon1, CorrLon2: Double;
begin
  // нивелируем эффект прокрутки при пересечении линии дат
  if (ALongitude1 - ALongitudePoint > 180) then
    CorrLon1 := ALongitude1 - 360
  else if (ALongitudePoint - ALongitude1 > 180) then
    CorrLon1 := ALongitude1 + 360
  else
    CorrLon1 := ALongitude1;
  if (ALongitude2 - ALongitudePoint > 180) then
    CorrLon2 := ALongitude2 - 360
  else if (ALongitudePoint - ALongitude2 > 180) then
    CorrLon2 := ALongitude2 + 360
  else
    CorrLon2 := ALongitude2;
  // разворачиваем в декартову систему координат (с единицей "градус широты")
  // за начало координат берём точку, для которой ищется расстояние
  x1 := (CorrLon1 - ALongitudePoint) * Cos(DegToRad(ALatitudePoint));
  y1 := ALatitude1 - ALatitudePoint;
  x2 := (CorrLon2 - ALongitudePoint) * Cos(DegToRad(ALatitudePoint));
  y2 := ALatitude2 - ALatitudePoint;
  // сам расчёт
  if (x1 * (x1 - x2) + y1 * (y1 - y2) <= 0) then  // проекция точки оказалась ближе к точке 1 ИЛИ отрезок - это точка
  begin
    Result := Hypot(x1, y1);
  end else
  if (x2 * (x2 - x1) + y2 * (y2 - y1) < 0) then  // проекция точки оказалась ближе к точке 2
  begin
    Result := Hypot(x2, y2);
  end else                                                // проекция попадает на отрезок
  begin
    Result := Abs(x2 * y1 - y2 * x1) / Hypot((y2 - y1), (x2 - x1));
  end;
end;

class function TGeoCalcs.GeoDistancePointToLineM(const APoint, APoint1,
  APoint2: TGeoPos; var APPoint: TGeoPos): Double;
begin
  Result :=
    GeoDistancePointToLine(APoint, APoint1, APoint2, APPoint) * CEvDegLength;
end;

class function TGeoCalcs.GeoDistancePointToLineMDeg(const ALatitudePoint,
  ALongitudePoint, ALatitude1, ALongitude1, ALatitude2,
  ALongitude2: Double): Double;
begin
  Result :=
    GeoDistancePointToLineDeg(
      ALatitudePoint, ALongitudePoint,
      ALatitude1, ALongitude1,
      ALatitude2, ALongitude2) * CEvDegLength;
end;

class function TGeoCalcs.GeoDistancePointToZoneMDeg(const ALatitudePoint, ALongitudePoint: Double; const AZone: TGeoPosArray): Double;
var
  i: Integer;
  r: Double;
begin
  Result := INFINITE;
  i := Length(AZone);
  if i >= 2 then
  begin
    Result := TGeoCalcs.GeoDistancePointToLineMDeg(ALatitudePoint, ALongitudePoint, AZone[0].Latitude, AZone[0].Longitude, AZone[i-1].Latitude, AZone[i-1].Longitude);
    for i := 0 to Length(AZone) - 2 do
    begin
      r := TGeoCalcs.GeoDistancePointToLineMDeg(ALatitudePoint, ALongitudePoint, AZone[i].Latitude, AZone[i].Longitude, AZone[i+1].Latitude, AZone[i+1].Longitude);
      if r < Result then
        Result := r;
    end;
  end;
end;

class function TGeoCalcs.GeoDistancePointToPolyLineMDeg(const ALatitudePoint, ALongitudePoint: Double; const APolyLine: TGeoPosArray): Double;
var
  i: Integer;
  r: Double;
begin
  Result := INFINITE;
  for i := 0 to Length(APolyLine) - 2 do
  begin
    r := TGeoCalcs.GeoDistancePointToLineMDeg(ALatitudePoint, ALongitudePoint, APolyLine[i].Latitude, APolyLine[i].Longitude, APolyLine[i+1].Latitude, APolyLine[i+1].Longitude);
    if r < Result then
      Result := r;
  end;
end;


class function TGeoCalcs.GeoPointInZone(const ALatitude, ALongitude: Double; const AZone: TGeoPosArray): Boolean;
var
  I, J: Integer;
//------------------------------------------------------------------------------
begin
  Result := False;
  J := High(AZone);
  for I := Low(AZone) to High(AZone) do
  begin
    if (
      (
        ((AZone[I].Latitude <= ALatitude) and (ALatitude < AZone[J].Latitude))
        or
        ((AZone[J].Latitude <= ALatitude) and (ALatitude < AZone[I].Latitude))
      )
      and
      (
        ALongitude
          > (AZone[J].Longitude - AZone[I].Longitude)
            * (ALatitude - AZone[I].Latitude)
            / (AZone[J].Latitude - AZone[I].Latitude)
            + AZone[I].Longitude
      )
    ) then
      Result := not Result;
    J := I;
  end;
end;

end.
