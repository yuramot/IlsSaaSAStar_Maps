unit Geo.Pos;

interface

uses
  SysUtils, Math;

const
  CEvDegLengthKm = 111.225;   //! ������� �������� ����� ������� ((�������� + �������) / 2), ��
  CEvDegLength = 111225.;     //! ������� �������� ����� ������� ((�������� + �������) / 2), �
  CEvEarthRadiusKm = 6371.;   //! ������� ������ �����, ��
  CEvEarthRadius = 6371000.;  //! ������� ������ �����, �
  CTolerance = 1E-5;          //! ����������� ���������� ������������� ����������� ��� ���������
  CMpsToKmh = 3.6;
  CKmhToMps = 1 / CMpsToKmh;

type
  TCartesianPos = packed record
    X, Y: Double;
    constructor Create(const AX, AY: Double);
  end;

  PGeoPos =  ^TGeoPos;

  TGeoPos = packed record
    Latitude: Double;
    Longitude: Double;

    class operator Equal(a, b: TGeoPos) : Boolean;
    function IsValid: Boolean;
    function IsSame(ALatitude: Double; ALongitude: Double): Boolean;
    function ToCartesianPos(XOffset: Double): TCartesianPos;
    function ToGeoHash(aPrec: Integer = 12): string;
    function ToHash: Int64;
    function ToString: string;
    constructor Create(
      const ALatitude: Double;
      const ALongitude: Double
    ); overload;
    constructor Create(
      const ACartesianPos: TCartesianPos;
      const ALongitudeOffset: Double = 0); overload;
  end;

  TGeoPosArray = TArray<TGeoPos>;

  PGeoPosFull = ^TGeoPosFull;

  TGeoPosFull = packed record
    Pos: TGeoPos;
    Altitude: Double;

    function IsValid: Boolean;

    constructor Create(
      ALatitude: Double;
      ALongitude: Double;
      AAltitude: Double = 0
    );
  end;

  TGeoPosFullArray = TArray<TGeoPosFull>;

  TGeoTrackPoint = packed record
    Pos: TGeoPosFull;
    DateTime: TDateTime;
    SatellitesCount: Integer;
    ProviderID: Smallint;
    class operator Equal(a, b: TGeoTrackPoint): Boolean;
    constructor Create(
      const ALatitude: Double;
      const ALongitude: Double;
      const ADateTime: TDateTime;
      const ASatCount: Integer;
      const AAltitude: Double = 0
    );
  end;

  PGeoSquare = ^TGeoSquare;

  TGeoSquare = record
    NorthWest: TGeoPos;
    SouthEast: TGeoPos;
  end;

  TGeoSquareArray = TArray<TGeoSquare>;

  TGeoCircle = record
    Pos: TGeoPos;
    Radius: Double;

    // ���������� ����� ���������� � ������ ����������� �� ������
    function CirclePoint(const ADirection: Double): TGeoPos;
  end;

implementation

uses
  Geo.Hash;

{ TCartesianPos }

constructor TCartesianPos.Create(const AX, AY: Double);
begin
  X := AX;
  Y := AY;
end;

{ TGeoPos }

constructor TGeoPos.Create(const ALatitude, ALongitude: Double);
begin
  Latitude  := ALatitude;
  Longitude := ALongitude;
end;

constructor TGeoPos.Create(const ACartesianPos: TCartesianPos; const ALongitudeOffset: Double);
begin
  Latitude := ACartesianPos.Y / CEvDegLength;
  if Abs(Latitude) = 90 then
    Longitude := 0
  else if Abs(Latitude) < 90 then
  begin
    Longitude := ALongitudeOffset + ACartesianPos.X / CEvDegLength / Cos(DegToRad(Latitude));
    if Abs(Longitude) > 180 then
      Longitude := Longitude - 360 * (Round(Longitude + Sign(Longitude) * 180) div 360);
  end
  else
  begin
    Latitude := 0;
    Longitude := 0;
  end;
end;

function TGeoPos.IsValid: Boolean;
begin
  Result := not ((Latitude < -90) or (Latitude > 90) or (Longitude < -180) or (Longitude > 180));
end;

class operator TGeoPos.Equal(a, b: TGeoPos) : Boolean;
begin
  Result :=
    SameValue(a.Latitude, b.Latitude, CTolerance) and
    SameValue(a.Longitude, b.Longitude, CTolerance);
end;

function TGeoPos.IsSame(ALatitude: Double; ALongitude: Double): Boolean;
begin
  Result := (ALatitude = Latitude) and (ALongitude = Longitude);
end;


function TGeoPos.ToCartesianPos(XOffset: Double): TCartesianPos;
begin
  Result.X := (Longitude - XOffset) * cEvDegLength * Cos(DegToRad(Latitude));
  Result.Y := Latitude * cEvDegLength;
end;

function TGeoPos.ToGeoHash(aPrec: Integer = 12): string;
begin
  Result := TGeoHash.EncodePointString(Latitude, Longitude, aPrec);
end;

function TGeoPos.ToHash: Int64;
begin
  Result := TGeoHash.EncodePointBin(Latitude, Longitude);
end;

function TGeoPos.ToString: string;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  Result := FloatToStr(Latitude, fs) + ' ' + FloatToStr(Longitude, fs);
end;

{ TGeoPosFull }

constructor TGeoPosFull.Create(ALatitude, ALongitude: Double; AAltitude: Double = 0);
begin
  Pos.Latitude  := ALatitude;
  Pos.Longitude := ALongitude;
  Altitude := AAltitude;
end;

function TGeoPosFull.IsValid: Boolean;
begin
  Result := Pos.IsValid;
end;

{ TGeoTrackPoint }

constructor TGeoTrackPoint.Create(
  const ALatitude: Double;
  const ALongitude: Double;
  const ADateTime: TDateTime;
  const ASatCount: Integer;
  const AAltitude: Double = 0
);
begin
  Pos.Create(ALatitude, ALongitude, AAltitude);
  DateTime := ADateTime;
  SatellitesCount := ASatCount;
end;

class operator TGeoTrackPoint.Equal(a, b: TGeoTrackPoint): Boolean;
begin
  Result := (a.DateTime = b.DateTime) and (a.SatellitesCount = b.SatellitesCount) and (a.ProviderID = b.ProviderID) and (a.Pos.Pos = b.Pos.Pos);
end;

{ TGeoCircle }

function TGeoCircle.CirclePoint(const ADirection: Double): TGeoPos;
begin
  if (Pos.Latitude + Radius / CEvDegLength >= 90)
      or (Pos.Latitude - Radius / CEvDegLength <= -90) then
    Exit(Pos);

  Result.Latitude := Pos.Latitude + Radius / CEvDegLength + Cos(DegToRad(ADirection));
  Result.Longitude := Pos.Longitude + Radius / CEvDegLength + Sin(DegToRad(ADirection));
end;

end.


