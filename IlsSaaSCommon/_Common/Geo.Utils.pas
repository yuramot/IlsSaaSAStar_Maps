unit Geo.Utils;

interface

uses
  Geo.Calcs, Geo.Pos, clipper;

type
  TGeoUtils = class
  public
    class function ExpandPolygonBy(var AZone: TGeoPosArray; const ARadius: Double): Boolean;
  end;


implementation

class function TGeoUtils.ExpandPolygonBy(var AZone: TGeoPosArray; const ARadius: Double): Boolean;
var
  i, l, k, z: Integer;
  GeoPos, GeoPos2: TGeoPos;
  CPos: TCartesianPos;
  DZone: TPath;
  RZones: TPaths;
begin
  if ARadius <= 0 then
    Exit(True);

  Result := False;

  try
    l := Length(AZone);
    SetLength(DZone, l);
    for i := 0 to l - 1 do
    begin
      GeoPos2 := GeoPos;
      GeoPos := AZone[i];
      if i > 0 then
        TGeoCalcs.CastGeoCoordinates2(GeoPos2, GeoPos);

      CPos := GeoPos.ToCartesianPos(0);
      DZone[i].X := Round(CPos.X);
      DZone[i].Y := Round(CPos.Y);
    end;

    with TClipperOffset.Create() do
      try
        AddPath(DZone, jtSquare, etClosedPolygon);
        Execute(RZones, ARadius);
      finally
        Free;
      end;

    if Length(RZones) > 0 then
    begin
      Result := True;
      z := 0;
      l := 0;
      for k := 0 to Length(RZones) - 1 do
        if Length(RZones[k]) > l then
        begin
          z := k;
          l := Length(RZones[k]);
        end;

      l := Length(RZones[0]);
      SetLength(AZone, l);
      for i := 0 to l - 1 do
        AZone[i] := TGeoPos.Create(TCartesianPos.Create(RZones[z][i].X, RZones[z][i].Y));
    end;
  finally
    DZone := nil;
    RZones := nil;
  end;
end;

end.
