unit E_GeoHashSearch;
interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs, MSAccess, Math;

const
  cCurrentApiVersionMajor     = 1;
  cCurrentApiVersionMinor     = 5;
  cCurrentApiVersionRevision  = 6;

  cCurrentDataVersionMajor    = 5;
  cCurrentDataVersionMinor    = 4;
  cCurrentDataVersionRevision = 1;

  cHashBlockSize = 1024;
  cHashSizeInBitsMax = 12*5;
  cBuildingsHashBucketSizeInBits  = 13;//12;
  cTownsHashBucketSizeInBits      = 13;//12;
  cRoutesHashBucketSizeInBits     = 13;//12;
  cBuildingsHashBucketSize = 1 shl (cBuildingsHashBucketSizeInBits);
  cTownsHashBucketSize = 1 shl (cTownsHashBucketSizeInBits);
  cRoutesHashBucketSize = 1 shl (cRoutesHashBucketSizeInBits);

  cGeoHashBuildingsPrecision = 32;// 7*5;//7 - optimal for 300m = 0ms
  cGeoHashTownsPrecision = 32;//7*5;//7 - optimal for 300m = 0ms
  cGeoHashRoutesPrecision = 32;//7*5;

  cIdxHashesBufferSize = 100000;
  cRoutesIdxHashesBufferSize = 10000;

type

  TSearchFileType = (ftUnknown, ftIdPoints, ftIdxHash, ftIdxHashPoints);
  TSearchRecordType = (rtUnknown, rtType, rtVersion, rtChangedDateTime);

  TGeoHashValue = UInt32;//UInt64;

  TLatLonPoint = packed record
    Latitude: Double;
    Longitude: Double;
    constructor Create(aLatitude: Double; aLongitude: Double);
  end;

  PIDLatLonPoint = ^TIdLatLonPoint;

  TGeoHashIdxRecord = packed record
    GeoHash: TGeoHashValue;
    Idx: UInt32;
  end;

  TGeoHashIdxArray = TArray<TGeoHashIdxRecord>;

  TIdLatLonPoint = packed record
    ID: UInt32;
    p: TLatLonPoint;

    constructor Create(aLatitude: Double; aLongitude: Double); overload;
    constructor Create(aID: Int64; aLatitude: Double; aLongitude: Double); overload;
  end;

  TLinePointNames = (pnLeft, pnCenter, pnRight);

  TLinePoints = packed record
    Left: Int32;
    Center: UInt32;
    Right: Int32;
  end;

  TIdxGeoHashLines = packed record
    GeoHash: TGeoHashValue;
    PointsPos: TLinePoints;
  end;

  TGeoPointsHashedArray = TArray<TIDLatLonPoint>;

  TGeoHashBlock = class;

  PGeoHashBlock = ^TGeoHashBlock;

  TGeoHashEntryBlock = class
  protected
    FNextLink: TGeoHashBlock;
    FCapacity: Integer;
  public
    procedure Add(const aPoint: TGeoHashIdxRecord);
    function FindPoints(aHash: Int64; var aPoints: TGeoHashIdxArray): Boolean;

    constructor Create(aCapacity: Integer);
    destructor Destroy; override;
  end;

  TGeoHashBlock = class(TGeoHashEntryBlock)
  private
    Hashes: TArray<TGeoHashIdxRecord>;
    UsedCount: Integer;
  public
    procedure Add(const aPoint: TGeoHashIdxRecord);
    function FindPoints(aHash: Int64; var aPoints: TGeoHashIdxArray): Boolean;

    constructor Create(aCapacity: Integer);
    destructor Destroy; override;
  end;

  TGeoHashSearchFileType = packed record
    RecordID: Byte;
    FileType: Byte;
  end;

  TGeoHashSearchFileVersion = packed record
    Minor: Byte;
    Major: Byte;
  end;

  TGeoHashSearchFileHeaderVersion = packed record
    RecordID: Byte;
    FileVersion: TGeoHashSearchFileVersion;
  end;

  TGeoHashSearchFileHeaderChangedDateTime = packed record
    RecordID: Byte;
    ChangedDateTime: TDateTime
  end;

  TGeoHashSearchFileHeader = packed record
    FileHeaderType: TGeoHashSearchFileType;
    Version: TGeoHashSearchFileHeaderVersion;
    ChangedDateTime: TGeoHashSearchFileHeaderChangedDateTime;
    constructor Create(
      aFileType: TSearchFileType;
      aMajor: Byte; aMinor: Byte;
      aChangedDateTime: TDateTime);
  end;

  TGeoHashMemorySearch = class
  private
    FInited: Boolean;
    FHashBucketSize: UInt32;
    FHashBucket: TArray<TGeoHashEntryBlock>;

    FLoadingCriticalSection: TCriticalSection;

    class procedure GetRegionBoundingBox(out aBoundMin: TLatLonPoint; out aBoundMax: TLatLonPoint);

    class function EncodePoint(const aPoint: TLatLonPoint; const aPrecision: Integer): TGeoHashValue; overload;
    class function EncodePoint(const aLatitude, aLongitude: Double; const aPrecision: Integer): TGeoHashValue; overload;
    class function EncodePoint(const aPoint, aBoundMin, aBoundMax: TLatLonPoint; const aPrecision: Integer): TGeoHashValue; overload;

    class function DecodePoint(const aHash: TGeoHashValue; var aCoords: TLatLonPoint; const aPrecision: Integer): Boolean; overload;
    class function DecodePoint(
      const aHash: TGeoHashValue; const aBoundMin, aBoundMax: TLatLonPoint;
      const aPrecision: Integer; var aPoint, aPointBoundMin, aPointBoundMax: TLatLonPoint): Boolean; overload;

    class function GeoLength(const aFrom, aTo: TLatLonPoint): Double; overload;
    class function GeoLength(const aLatitudeFrom, aLongitudeFrom, aLatitudeTo, aLongitudeTo: Double): Double; overload;
    class function GeoLengthRad(const aLatitude1, aLongitude1, aLatitude2, aLongitude2: Double): Double;
    class function GeoLengthDeg(const aLatitude1, aLongitude1, aLatitude2, aLongitude2: Double): Double;


    function FindPointsByHash(const aHash: Int64; var aPoints: TGeoHashIdxArray): Boolean;
    function FindPointsByHashes(
      const aHashes: TArray<TGeoHashValue>;
      var aPoints: TGeoHashIdxArray): Boolean;

    procedure Init;
    procedure Shutdown;

  public
    procedure Clear;

    class function GetSurroundingHashes(
      const aRadius: Double; const aHash: TGeoHashValue;
      const aPrecision: Integer; var aHashes: TArray<TGeoHashValue>): Boolean; overload;

    procedure Add(const aPoint: TGeoHashIdxRecord);

    constructor Create(aHashBucketSize: Integer);
    destructor Destroy; override;
  end;

  TGeoHashSearch = class(TGeoHashMemorySearch)
  private
    FDataFileName: string;
    FIdxFileName: string;
    FCsvFileName: string;

    FDataFile: TFileStream;
    FIdxFile: TFileStream;

    FPrecision: Integer;
//    FChangedDateTime: TDateTime;

    FConnection: TMSConnection;
    FQuery: TMSQuery;

    FTimeout: Integer;

    FLoaded: Boolean;
    FTryToLoadFromDB: Boolean;
    FUseCsvFile: Boolean;

    procedure SetConnectionStr(const Value: string);
    procedure SetDataFileName(const Value: string); virtual;
    function RemoveTrailingPathDelimiters(const aPath: string): string;
    procedure InitDataFile(const aFileName: string; var aFile: TFileStream);
    procedure FinalizeDataFile(aHeader: TGeoHashSearchFileHeader; aFile: TFileStream);
    function IsDataFileActual: Boolean; overload; virtual; abstract;
    function IsIdxFileActual: Boolean; overload; virtual; abstract;
    function IsFileActual(aFileStream: TFileStream; aSearchFileType: TSearchFileType): Boolean; overload;
  public
    property ConnectionStr: string write SetConnectionStr;
    property Timeout: Integer read FTimeout write FTimeout;
    property DataFileName: string read FDataFileName write SetDataFileName;
    property TryToLoadFromDB: Boolean read FTryToLoadFromDB write FTryToLoadFromDB;

    property UseCsvFile: Boolean read FUseCsvFile write FUseCsvFile;

    function DataFileOpen(const aFileName: string; const aCreate: Boolean; out aFile: TFileStream): Boolean;
    procedure InitHeader(out aHeader: TGeoHashSearchFileHeader; aFile: TFileStream; aFileType: TSearchFileType);

    function LoadFromDB: Integer; virtual; abstract;
    function LoadFromFiles: Integer; virtual; abstract;

    function Load(const aPrecision: Integer): Boolean;

    function FindNearestHashesInRadius(
      const aLatitude, aLongitude: Double;
      const aRadius: Double; const aPrecision: Integer;
      var aHashIdxArray: TGeoHashIdxArray;
      const aLog: TStringList = nil): Boolean;
    function FindNearestPointInRadius(
      const aLatitude, aLongitude: Double;
      const aRadius: Double;
      const aPrecision: Integer;
      out aIdx: Integer;
      const aLog: TStringList = nil): Boolean; virtual;

    constructor Create(aHashBucketSize: Integer);
    destructor Destroy; override;
  end;

  TGeoHashBuildingsSearch = class(TGeoHashSearch)
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromDB: Integer; override;
    function LoadFromFiles: Integer; override;
  end;

  TGeoHashTownsSearch = class(TGeoHashSearch)
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromDB: Integer; override;
    function LoadFromFiles: Integer; override;
  end;

  TGeoHashRoutesSearch = class(TGeoHashSearch)
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromDB: Integer; override;
    function LoadFromFiles: Integer; override;

    function FindNearestRouteInRadius(
      const aLatitude, aLongitude: Double;
      const aRouteRadius, aSpeedRadius: Double;
      const aPrecision: Integer;
      out aIdx: Integer;
      out aIdxAround: TArray<Integer>;
      const aLog: TStringList): Boolean; virtual;
  end;

  TGeoHashPolygonSearch = class(TGeoHashSearch)
  private
//      function IsGeoPointInPolygon( const aPoint: TLatLonPoint; const aPolygon: TArray<TLatLonPoint> ): Boolean;
  public
//    function IsDataFileActual: Boolean; override;
//    function IsIdxFileActual: Boolean; override;
//    function LoadFromDB: Integer; override;
//    function LoadFromFiles: Integer; override;
//
//    function FindClosestRouteInRadius(
//      const aLatitude, aLongitude: Double;
//      const aRadius: Double; const aPrecision: Integer;
//      const aLog: TStringList): Integer; virtual;
  end;

  TGeoAreaPolygonSearch = class(TGeoHashPolygonSearch)

  end;

  TGeoDistrictPolygonSearch = class(TGeoHashPolygonSearch)

  end;

  TGeoHashLinksGetter = class
  strict protected
    class var FGeoHashAddressGetter: TGeoHashLinksGetter;
  private
    FGHSBuildings: TGeoHashBuildingsSearch;
    FGHSTowns: TGeoHashTownsSearch;
    FGHSRoutes: TGeoHashRoutesSearch;

    FTryToLoadFromDB: Boolean;
    FUseCsvFiles: Boolean;

    procedure SetConnectionStr(const Value: string);
    procedure SetTimeout(const Value: Integer);
    procedure SetWorkingPath(const Value: string);

    function LoadAddresses(const aBuildingPrecision: Integer; const aTownPrecision: Integer): Boolean;
    function LoadRoutes(const aRoutesPrecision: Integer): Boolean;
    function GetTryToLoadFromDB: Boolean;
    procedure SetTryToLoadFromDB(const Value: Boolean);
    function GetUseCsvFiles: Boolean;
    procedure SetUseCsvFiles(const Value: Boolean);

  public
    property ConnectionStr: string write SetConnectionStr;
    property Timeout: Integer write SetTimeout;
    property WorkingPath: string write SetWorkingPath;
    property TryToLoadFromDB: Boolean read GetTryToLoadFromDB write SetTryToLoadFromDB;
    property UseCsvFiles: Boolean read GetUseCsvFiles write SetUseCsvFiles;

    function CheckAddressDataLoaded: Boolean;
    function CheckRoutesDataLoaded: Boolean;
    function CheckDataLoaded: Boolean;

    function FindNearestAddressInRadius(
      const aLatitude, aLongitude: Double; const aBuildingRadius: Double; const aTownRadius: Double;
      const aBuildingPrecision: Integer; const aTownPrecision: Integer;
      out aBuildingID: Integer; out aTownID: Integer;
      const aLog: TStringList = nil): Boolean;

    function FindNearestRouteInRadius(
      const aLatitude, aLongitude: Double;
      const aRouteRadius, aSpeedRadius: Double;
      const aRoutePrecision: Integer;
      out aRouteID: Integer; out aRoutesAround: TArray<Integer>;
      const aLog: TStringList = nil): Boolean;
  public
    class function GeoHashAddressGetter: TGeoHashLinksGetter;
    class constructor Create; overload;
    class destructor Destroy;

    function GetApiVersion: string;
    function GetFilesVersion: string;
    function IsFilesAvailable(out RFileNames: TArray<string>): Boolean;

    constructor Create; overload;
    destructor Destroy; override;
  end;

function GeoHashAddressGetter: TGeoHashLinksGetter;

implementation

function IsGeoPointInPolygon( const aPoint: TLatLonPoint; const aPolygon: TArray<TLatLonPoint> ): Boolean;
var
  VertexCount: Integer;
  i, j: Integer;
begin
  Result := False;
  VertexCount := Length(aPolygon);

  for i := 0 to VertexCount - 1 do
  begin
    j := (i + 1) mod VertexCount;

    if (aPoint.Latitude > aPolygon[i].Latitude) xor
       (aPoint.Latitude > aPolygon[j].Latitude) then
      if (aPoint.Longitude > aPolygon[i].Longitude + (aPolygon[j].Longitude - aPolygon[i].Longitude) *
         (aPoint.Latitude - aPolygon[i].Latitude) / (aPolygon[j].Latitude - aPolygon[i].Latitude)) then
        Result := not Result;
  end;
end;


function GeoHashAddressGetter: TGeoHashLinksGetter;
begin
  Result := TGeoHashLinksGetter.GeoHashAddressGetter;
end;

function GetCurrentVersionFileNameSuffix: string;
begin
  Result :=  '.' + IntToStr(cCurrentDataVersionMajor) + '.' + IntToStr(cCurrentDataVersionMinor);
end;


{ TGeoHash }

procedure TGeoHashMemorySearch.Add(const aPoint: TGeoHashIdxRecord);
var
  PartHash: TGeoHashValue;
begin
  PartHash := aPoint.GeoHash and (FHashBucketSize - 1);

  Assert(PartHash < FHashBucketSize);

  FHashBucket[PartHash].Add(aPoint);
end;

procedure TGeoHashMemorySearch.Clear;
var
  i: Integer;
begin
  for i := 0 to FHashBucketSize - 1 do
    FHashBucket[i].Free;

  Init;
end;

procedure TGeoHashMemorySearch.Init;
var
  i: Integer;
begin
  FLoadingCriticalSection.Enter;
  try
    if not FInited then
    begin
      SetLength(FHashBucket, FHashBucketSize);

      for i := 0 to FHashBucketSize - 1 do
        FHashBucket[i] := TGeoHashEntryBlock.Create(cHashBlockSize);

      FInited := True;
    end;
  finally
    FLoadingCriticalSection.Leave;
  end;
end;

procedure TGeoHashMemorySearch.Shutdown;
var
  i: Integer;
begin
  FLoadingCriticalSection.Enter;
  try
    if FInited then
    begin
      for i := 0 to FHashBucketSize - 1 do
        FHashBucket[i].Free;

      FHashBucket := nil;
      FInited := False;
    end;
  finally
    FLoadingCriticalSection.Leave;
  end;
end;

constructor TGeoHashMemorySearch.Create(aHashBucketSize: Integer);
begin
  FInited := False;
  FLoadingCriticalSection := TCriticalSection.Create;
  FHashBucketSize := aHashBucketSize;
end;

destructor TGeoHashMemorySearch.Destroy;
begin
  Shutdown;
  FLoadingCriticalSection.Free;
end;

function TGeoHashMemorySearch.FindPointsByHash(const aHash: Int64; var aPoints: TGeoHashIdxArray): Boolean;
var
  PartHash: TGeoHashValue;
begin
  PartHash := aHash and (FHashBucketSize - 1);

  Assert(PartHash < FHashBucketSize);

  Result := FHashBucket[PartHash].FindPoints(aHash, aPoints);
end;

function TGeoHashMemorySearch.FindPointsByHashes(
  const aHashes: TArray<TGeoHashValue>;
  var aPoints: TGeoHashIdxArray): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(aHashes) - 1 do
    FindPointsByHash(aHashes[i], aPoints);
end;


class function TGeoHashMemorySearch.DecodePoint(const aHash: TGeoHashValue;
  var aCoords: TLatLonPoint; const aPrecision: Integer): Boolean;
var
  BoundMin, BoundMax: TLatLonPoint;
  PointBoundMin, PointBoundMax: TLatLonPoint;
begin
  TGeoHashMemorySearch.GetRegionBoundingBox(BoundMin, BoundMax);

  Result := DecodePoint(aHash, BoundMin, BoundMax, aPrecision, aCoords, PointBoundMin, PointBoundMax);
end;

class function TGeoHashMemorySearch.DecodePoint(const aHash: TGeoHashValue; const aBoundMin,
  aBoundMax: TLatLonPoint; const aPrecision: Integer; var aPoint, aPointBoundMin,
  aPointBoundMax: TLatLonPoint): Boolean;
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

class function TGeoHashMemorySearch.EncodePoint(const aPoint, aBoundMin, aBoundMax: TLatLonPoint; const aPrecision: Integer): TGeoHashValue;
var
  WorkBoundMin, WorkBoundMax: TLatLonPoint;
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

class function TGeoHashMemorySearch.EncodePoint(const aPoint: TLatLonPoint; const aPrecision: Integer): TGeoHashValue;
var
  BoundMin, BoundMax: TLatLonPoint;
begin
  TGeoHashMemorySearch.GetRegionBoundingBox(BoundMin, BoundMax);

  Result := EncodePoint(aPoint, BoundMin, BoundMax, aPrecision);
end;

class function TGeoHashMemorySearch.EncodePoint(const aLatitude, aLongitude: Double; const aPrecision: Integer): TGeoHashValue;
begin
  Result := EncodePoint(TLatLonPoint.Create(aLatitude, aLongitude), aPrecision);
end;

class function TGeoHashMemorySearch.GeoLength(const aFrom, aTo: TLatLonPoint): Double;
begin
  Result := GeoLength(aFrom.Latitude, aFrom.Longitude, aTo.Latitude, aTo.Longitude);
end;

class function TGeoHashMemorySearch.GeoLength(const aLatitudeFrom, aLongitudeFrom, aLatitudeTo, aLongitudeTo: Double): Double;
begin
  //*** 111.225 - это среднее значение ((меридиан + экватор) / 2), в реальности оно будет больше на экваторе
  //*** но для наших целей такого приближения достаточно
  Result := 111.225 * Hypot(ALatitudeFrom - ALatitudeTo, (ALongitudeFrom - ALongitudeTo) * Cos(DegToRad(ALatitudeFrom)));
end;

class function TGeoHashMemorySearch.GeoLengthRad(const aLatitude1, aLongitude1, aLatitude2, aLongitude2: Double): Double;
begin
  Result := ArcCos(Sin(aLatitude1) * Sin(aLatitude2) + Cos(aLatitude1) * Cos(aLatitude2) * Cos(aLongitude1 - aLongitude2)) * 6371;
end;

class function TGeoHashMemorySearch.GeoLengthDeg(const aLatitude1, aLongitude1, aLatitude2, aLongitude2: Double): Double;
begin
  Result := GeoLengthRad(DegToRad(aLatitude1), DegToRad(aLongitude1), DegToRad(aLatitude2), DegToRad(aLongitude2));
end;

{ TGeoHashBlock }

constructor TGeoHashBlock.Create(aCapacity: Integer);
begin
  inherited Create(aCapacity);
  UsedCount := 0;
  SetLength(Hashes, aCapacity);
end;

destructor TGeoHashBlock.Destroy;
begin
  inherited;
end;

function TGeoHashBlock.FindPoints(aHash: Int64; var aPoints: TGeoHashIdxArray): Boolean;
  procedure FindPoints2(aBlock: TGeoHashBlock; aHash: Int64; var aPoints: TGeoHashIdxArray);
  var
    i: Integer;
  begin
    for i := 0 to aBlock.UsedCount - 1 do
      if aBlock.Hashes[i].GeoHash = aHash then
      begin
        SetLength(aPoints, Length(aPoints) + 1);
        aPoints[Length(aPoints) - 1] := aBlock.Hashes[i];
      end;

    if Assigned(aBlock.FNextLink) then
      FindPoints2(aBlock.FNextLink, aHash, aPoints);
  end;
begin
  FindPoints2(Self, aHash, aPoints);
  Result := Length(aPoints) > 0
end;

procedure TGeoHashBlock.Add(const aPoint: TGeoHashIdxRecord);
begin
  if UsedCount < cHashBlockSize then
  begin
    Inc(UsedCount);
    Hashes[UsedCount - 1] := aPoint;
  end
  else
  begin
    if not Assigned(FNextLink) then
      FNextLink := TGeoHashBlock.Create(cHashBlockSize);
    FNextLink.Add(aPoint);
  end;
end;


{ TLatLonPoint }

constructor TLatLonPoint.Create(aLatitude, aLongitude: Double);
begin
  Latitude := aLatitude;
  Longitude := aLongitude;
end;

{ TGeoHashMemorySearch }

class procedure TGeoHashMemorySearch.GetRegionBoundingBox(out aBoundMin: TLatLonPoint; out aBoundMax: TLatLonPoint);
begin
  aBoundMin.Latitude := -90;
  aBoundMin.Longitude := -180;
  aBoundMax.Latitude := 90;
  aBoundMax.Longitude := 180;
end;

class function TGeoHashMemorySearch.GetSurroundingHashes(
  const aRadius: Double; const aHash: TGeoHashValue;
  const aPrecision: Integer; var aHashes: TArray<TGeoHashValue>): Boolean;
var
  BoundMin: TLatLonPoint;
  BoundMax: TLatLonPoint;
  Location: TLatLonPoint;
  LocBoundMin: TLatLonPoint;
  LocBoundMax: TLatLonPoint;
  LatBoxSize: Double;
  LonBoxSize: Double;
  LatBoxMid: Double;
  LonBoxMid: Double;
  LatBoxSizeInMeters: Double;
  LonBoxSizeInMeters: Double;
  LatRadiusInHashes: Integer;
  LonRadiusInHashes: Integer;
  x, y: Integer;
  i: Integer;

  function MakeGeoPoint(aLoc: TLatLonPoint; aLat, aLon: Integer)
    : TLatLonPoint;
  begin
    Result := aLoc;
    Result.Latitude := Result.Latitude + aLat * LatBoxSize;
    Result.Longitude := Result.Longitude + aLon * LonBoxSize;
  end;

begin
  Result := False;

  TGeoHashMemorySearch.GetRegionBoundingBox(BoundMin, BoundMax);

  if not TGeoHashMemorySearch.DecodePoint(aHash, BoundMin, BoundMax, aPrecision, Location, LocBoundMin, LocBoundMax) then
    Exit;
  LatBoxSize := LocBoundMax.Latitude - LocBoundMin.Latitude;
  LonBoxSize := LocBoundMax.Longitude - LocBoundMin.Longitude;
  LatBoxMid := (LocBoundMax.Latitude + LocBoundMin.Latitude)/2;
  LonBoxMid := (LocBoundMax.Longitude + LocBoundMin.Longitude)/2;
  LatBoxSizeInMeters := TGeoHashMemorySearch.GeoLength(LocBoundMin.Latitude, LonBoxMid, LocBoundMax.Latitude, LonBoxMid)*1000;
  LonBoxSizeInMeters := TGeoHashMemorySearch.GeoLength(LatBoxMid, LocBoundMin.Longitude, LatBoxMid, LocBoundMax.Longitude)*1000;

  LatRadiusInHashes := Ceil(aRadius/LatBoxSizeInMeters);
  LonRadiusInHashes := Ceil(aRadius/LonBoxSizeInMeters);

  SetLength(aHashes, (LatRadiusInHashes*2+1) * (LonRadiusInHashes*2+1));
  for y := -LatRadiusInHashes to LatRadiusInHashes do
    for x := -LonRadiusInHashes to LonRadiusInHashes do
    begin
      i := (x + LonRadiusInHashes) + (y + LatRadiusInHashes) * (LonRadiusInHashes * 2 + 1);
      aHashes[i] := TGeoHashMemorySearch.EncodePoint(
        TLatLonPoint.Create(
          LatBoxMid + LatBoxSize * y,
          LonBoxMid + LonBoxSize * x),
          aPrecision);
    end;
  Result := Length(aHashes) > 0;
end;

{ TGeoHashLinksGetter }

function TGeoHashLinksGetter.CheckDataLoaded: Boolean;
begin
  Result := CheckAddressDataLoaded and CheckRoutesDataLoaded;
end;

function TGeoHashLinksGetter.CheckAddressDataLoaded: Boolean;
begin
  Result := LoadAddresses(cGeoHashBuildingsPrecision, cGeoHashTownsPrecision);
end;

function TGeoHashLinksGetter.CheckRoutesDataLoaded: Boolean;
begin
  Result := LoadRoutes(cGeoHashRoutesPrecision);
end;


function TGeoHashLinksGetter.FindNearestAddressInRadius(
  const aLatitude, aLongitude: Double; const aBuildingRadius, aTownRadius: Double;
  const aBuildingPrecision, aTownPrecision: Integer;
  out aBuildingID, aTownID: Integer;
  const aLog: TStringList): Boolean;
begin
  Result := True;

  CheckAddressDataLoaded;

  aBuildingID := -1;
  aTownID := -1;

  if not FGHSBuildings.FindNearestPointInRadius(aLatitude, aLongitude, aBuildingRadius, aBuildingPrecision, aBuildingID, aLog) then
    if not FGHSTowns.FindNearestPointInRadius(aLatitude, aLongitude, aTownRadius, aTownPrecision, aTownID, aLog) then
      Exit(False);
end;

function TGeoHashLinksGetter.FindNearestRouteInRadius(
  const aLatitude, aLongitude: Double;
  const aRouteRadius, aSpeedRadius: Double;
  const aRoutePrecision: Integer;
  out aRouteID: Integer; out aRoutesAround: TArray<Integer>;
  const aLog: TStringList): Boolean;
begin
  CheckRoutesDataLoaded;

  Result :=
    FGHSRoutes.FindNearestRouteInRadius(
      aLatitude, aLongitude, aRouteRadius, aSpeedRadius, aRoutePrecision, aRouteID, aRoutesAround, aLog);
end;

function TGeoHashLinksGetter.LoadAddresses(const aBuildingPrecision: Integer; const aTownPrecision: Integer): Boolean;
begin
  FGHSBuildings.Load(aBuildingPrecision);
  FGHSTowns.Load(aTownPrecision);
  Result := FGHSBuildings.FLoaded and FGHSTowns.FLoaded;
end;

function TGeoHashLinksGetter.LoadRoutes(const aRoutesPrecision: Integer): Boolean;
begin
  FGHSRoutes.Load(aRoutesPrecision);
  Result := FGHSRoutes.FLoaded;
end;

procedure TGeoHashLinksGetter.SetConnectionstr(const Value: string);
begin
  FGHSBuildings.ConnectionStr := Value;
  FGHSTowns.ConnectionStr := Value;
  FGHSRoutes.ConnectionStr := Value;
end;

procedure TGeoHashLinksGetter.SetTimeout(const Value: Integer);
begin
  FGHSBuildings.Timeout := Value;
  FGHSTowns.Timeout := Value;
  FGHSRoutes.Timeout := Value;
end;

procedure TGeoHashLinksGetter.SetTryToLoadFromDB(const Value: Boolean);
begin
  FTryToLoadFromDB := Value;
  FGHSBuildings.TryToLoadFromDB := Value;
  FGHSTowns.TryToLoadFromDB := Value;
  FGHSRoutes.TryToLoadFromDB := Value;
end;

procedure TGeoHashLinksGetter.SetUseCsvFiles(const Value: Boolean);
begin
  FUseCsvFiles := Value;
  FGHSBuildings.UseCsvFile := Value;
  FGHSTowns.UseCsvFile := Value;
  FGHSRoutes.UseCsvFile := Value;
end;

procedure TGeoHashLinksGetter.SetWorkingPath(const Value: string);
begin
  FGHSBuildings.DataFileName := Value + 'Buildings';
  FGHSTowns.DataFileName := Value + 'Towns';
  FGHSRoutes.DataFileName := Value + 'Routes';
end;

class function TGeoHashLinksGetter.GeoHashAddressGetter: TGeoHashLinksGetter;
begin
  Result := FGeoHashAddressGetter;
end;

function TGeoHashLinksGetter.GetApiVersion: string;
begin
  Result :=
    IntToStr(cCurrentApiVersionMajor) + '.' +
    IntToStr(cCurrentApiVersionMinor) + '.' +
    IntToStr(cCurrentApiVersionRevision);
end;

function TGeoHashLinksGetter.GetFilesVersion: string;
begin
  Result :=
    IntToStr(cCurrentDataVersionMajor) + '.' +
    IntToStr(cCurrentDataVersionMinor) + '.' +
    IntToStr(cCurrentDataVersionRevision);
end;

function TGeoHashLinksGetter.IsFilesAvailable(out RFileNames: TArray<string>): Boolean;
begin
  SetLength(RFileNames, 3);
  RFileNames[0] := FGHSBuildings.DataFileName;
  RFileNames[1] := FGHSTowns.DataFileName;
  RFileNames[2] := FGHSRoutes.DataFileName;
  Result :=
    FileExists(RFileNames[0]) and
    FileExists(RFileNames[1]) and
    FileExists(RFileNames[2]);
end;

function TGeoHashLinksGetter.GetTryToLoadFromDB: Boolean;
begin
  Result := FTryToLoadFromDB;
end;

function TGeoHashLinksGetter.GetUseCsvFiles: Boolean;
begin
  Result := FUseCsvFiles;
end;

class constructor TGeoHashLinksGetter.Create;
begin
  FGeoHashAddressGetter := TGeoHashLinksGetter.Create;
end;

class destructor TGeoHashLinksGetter.Destroy;
begin
  FGeoHashAddressGetter.Destroy;
end;

constructor TGeoHashLinksGetter.Create;
begin
  FUseCsvFiles := False;
  FGHSBuildings := TGeoHashBuildingsSearch.Create(cBuildingsHashBucketSize);
  FGHSTowns := TGeoHashTownsSearch.Create(cTownsHashBucketSize);
  FGHSRoutes := TGeoHashRoutesSearch.Create(cRoutesHashBucketSize);
end;

destructor TGeoHashLinksGetter.Destroy;
begin
  FGHSBuildings.Free;
  FGHSTowns.Free;
  FGHSRoutes.Free;
  inherited;
end;

{ TGeoHashSearch }

function TGeoHashSearch.DataFileOpen(const aFileName: string; const aCreate: Boolean; out aFile: TFileStream): Boolean;
var
  fm: Word;
begin
  if FileExists(aFileName) then
    fm := fmOpenRead + fmShareDenyNone
  else
    if not aCreate then
      Exit(False)
    else
{$ifdef DEBUG}
      fm := fmCreate + fmOpenReadWrite + fmShareDenyWrite;
{$else}
      fm := fmCreate + fmOpenReadWrite + fmShareExclusive;
{$endif}

  ForceDirectories(RemoveTrailingPathDelimiters(ExtractFilePath(aFileName)));

  aFile := TFileStream.Create(aFileName, fm);
  Result := Assigned(aFile);

  if Result then
    aFile.Position := 0;
end;

constructor TGeoHashSearch.Create(aHashBucketSize: Integer);
begin
  inherited Create(aHashBucketSize);
  FConnection := TMSConnection.Create(nil);
  FQuery := TMSQuery.Create(nil);
  FQuery.Connection := FConnection;
  FDataFile := nil;
  FIdxFile := nil;
  FTryToLoadFromDB := False;
  FUseCsvFile := False;
end;

destructor TGeoHashSearch.Destroy;
begin
  FQuery.Free;
  FConnection.Free;
  FDataFile.Free;
  FIdxFile.Free;
  inherited;
end;

procedure TGeoHashSearch.FinalizeDataFile(aHeader: TGeoHashSearchFileHeader; aFile: TFileStream);
begin
  aFile.Seek(0, soBeginning);
  aFile.Write(aHeader, SizeOf(aHeader));
end;

function TGeoHashSearch.FindNearestHashesInRadius(
  const aLatitude, aLongitude, aRadius: Double;
  const aPrecision: Integer;
  var aHashIdxArray: TGeoHashIdxArray;
  const aLog: TStringList = nil): Boolean;
var
  Hashes: TArray<TGeoHashValue>;
begin
  Result := False;

  if not FInited then
    Exit;

  GetSurroundingHashes(aRadius, EncodePoint(TLatLonPoint.Create(aLatitude, aLongitude), aPrecision), aPrecision, Hashes);
  FindPointsByHashes(Hashes, aHashIdxArray);
  Result := Length(aHashIdxArray) > 0;
end;

function TGeoHashSearch.FindNearestPointInRadius(
  const aLatitude, aLongitude: Double;
  const aRadius: Double;
  const aPrecision: Integer;
  out aIdx: Integer;
  const aLog: TStringList): Boolean;
var
  Points: TGeoHashIdxArray;
  i: Integer;
  IdLatLonPoint: TIdLatLonPoint;
  Distance: Double;
  MinDistance: Double;
begin
  Result := False;
  aIdx := -1;
  MinDistance := MaxInt;

  FindNearestHashesInRadius(aLatitude, aLongitude, aRadius, aPrecision, Points, aLog);
  for i := Low(Points) to High(Points) do
  begin
    if not Assigned(FDataFile) then
      DataFileOpen(FDataFileName, False, FDataFile);

    FDataFile.Position := Points[i].Idx;
    if FDataFile.Read(IdLatLonPoint, SizeOf(IdLatLonPoint)) = SizeOf(IdLatLonPoint) then
    begin
      Distance := GeoLength(aLatitude, aLongitude, IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude);
      if Assigned(aLog) then
        aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f)', [IdLatLonPoint.ID, aLatitude, aLongitude, Distance, IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude]));
      if Distance < MinDistance then
      begin
        MinDistance := Distance;
        aIdx := IdLatLonPoint.ID;
        Result := True;
        if Assigned(aLog) then
          aLog.Add(Format('Found new closest %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f)', [IdLatLonPoint.ID, aLatitude, aLongitude, Distance, IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude]));
      end;
    end;
  end;
end;

procedure TGeoHashSearch.InitDataFile(const aFileName: string; var aFile: TFileStream);
begin
  FreeAndNil(aFile);
  DeleteFile(aFileName);
  DataFileOpen(aFileName, True, aFile);
end;

procedure TGeoHashSearch.InitHeader(out aHeader: TGeoHashSearchFileHeader; aFile: TFileStream; aFileType: TSearchFileType);
begin
  FillChar(aHeader, SizeOf(aHeader), 0);
  aFile.Seek(0, soBeginning);
  aFile.Write(aHeader, SizeOf(aHeader));
  aHeader := TGeoHashSearchFileHeader.Create(aFileType, cCurrentDataVersionMajor, cCurrentDataVersionMinor, Now);
end;

function TGeoHashSearch.IsFileActual(aFileStream: TFileStream; aSearchFileType: TSearchFileType): Boolean;
var
  Header: TGeoHashSearchFileHeader;
begin
  Result :=
    (aFileStream.Read(Header, SizeOf(Header)) = SizeOf(Header)) and
    (Header.FileHeaderType.RecordID = Byte(rtType)) and
    (Header.FileHeaderType.FileType = Byte(aSearchFileType)) and
    (Header.Version.RecordID = Byte(rtVersion)) and
    (Header.Version.FileVersion.Major = cCurrentDataVersionMajor) and
    (Header.Version.FileVersion.Minor = cCurrentDataVersionMinor) and
    (Header.ChangedDateTime.RecordID = Byte(rtChangedDateTime))// and
//    (Header.ChangedDateTime.ChangedDateTime = FChangedDateTime)
end;

function TGeoHashSearch.Load(const aPrecision: Integer): Boolean;
begin
  if FLoaded then
    Exit(FLoaded);

  FLoadingCriticalSection.Enter;
  try
    Init;

    FPrecision := aPrecision;

    if DataFileOpen(FDataFileName, False, FDataFile) and IsDataFileActual and
       DataFileOpen(FIdxFileName, False, FIdxFile) and IsIdxFileActual then
      LoadFromFiles
    else
      if FTryToLoadFromDB then
        LoadFromDB;

    FreeAndNil(FDataFile);
    FreeAndNil(FIdxFile);

    FConnection.Close;

    FLoaded := True;
  finally
    FLoadingCriticalSection.Leave;
  end;

  Result := FLoaded;
end;

function TGeoHashSearch.RemoveTrailingPathDelimiters(
  const aPath: string): string;
var
  i: Integer;
begin
  i := Length(aPath);
  while i > 0 do
    if aPath[i] = '\' then
      Dec(i)
    else
      Break;

  Result := Copy(aPath, 1, i);
end;

procedure TGeoHashSearch.SetDataFileName(const Value: string);
var
  DataFileName: string;
  IdxFileName: string;
  CsvFileName: string;
begin
  DataFileName := Value + GetCurrentVersionFileNameSuffix + '.bin';
  IdxFileName := Value + GetCurrentVersionFileNameSuffix + '.idx';
  CsvFileName := Value + GetCurrentVersionFileNameSuffix + '.csv';
  if (FDataFileName <> DataFileName) then
  begin
    if FLoaded then
    begin
      FLoaded := False;
      Shutdown;
      Init;
    end;

    FDataFileName := DataFileName;
    FIdxFileName := IdxFileName;
    FCsvFileName := CsvFileName;
  end;
end;

procedure TGeoHashSearch.SetConnectionStr(const Value: string);
begin
  if FConnection.ConnectString = Value then
    Exit;

  if FConnection.Connected then
    FConnection.Disconnect;

  FConnection.ConnectString := Value;
end;

{ TGeoHashBuildingsSearch }

function TGeoHashBuildingsSearch.IsDataFileActual: Boolean;
begin
  Result := IsFileActual(FDataFile, ftIdPoints);
end;

function TGeoHashBuildingsSearch.IsIdxFileActual: Boolean;
begin
  Result := IsFileActual(FIdxFile, ftIdxHash);
end;

function TGeoHashBuildingsSearch.LoadFromDB: Integer;
const
  cLoadDiv = 250000;
var
  i: Integer;
  Count: Integer;
  LoadCount: Integer;

  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;
  IdLatLonPoint: TIdLatLonPoint;
  IdxHashPoint: TGeoHashIdxRecord;

  bLoadFromCsvFile: Boolean;
  f: TextFile;
  sl: TStringList;
  s: string;
begin
  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);

  bLoadFromCsvFile := FileExists(FCsvFileName) and UseCsvFile;

  Assign(f, FCsvFileName);
  if FileExists(FCsvFileName) then
    Reset(f);

  if bLoadFromCsvFile and (FileSize(f) > 0) then
  begin
    sl := TStringList.Create;
    while not Eof(f) do
    begin
      ReadLn(f, s);
      sl.Delimiter := ';';
      sl.DelimitedText := s;
      if sl.Count = 3 then
      begin
        //1531294;59,868688;30,293949
        IdLatLonPoint :=
          TIdLatLonPoint.Create(
            StrToIntDef(sl.Strings[0], -1),
            StrToFloatDef(sl.Strings[1], 0),
            StrToFloatDef(sl.Strings[2], 0));

        IdxHashPoint.GeoHash := EncodePoint(IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude, FPrecision);
        IdxHashPoint.Idx := FDataFile.Position;

        FDataFile.Write(IdLatLonPoint, SizeOf(IdLatLonPoint));
        FIdxFile.Write(IdxHashPoint, SizeOf(IdxHashPoint));
        Add(IdxHashPoint);
      end;
    end;
    sl.Free;
  end
  else
  begin
    Rewrite(f);
    if not FConnection.Connected then
      FConnection.Connect;

    FQuery.Close;
    FQuery.SQL.Text := 'select count(*) as cnt from mtbl_Buildings';
    FQuery.Open;

    Count := FQuery.FieldByName('cnt').AsInteger;
    LoadCount :=  Ceil(Count/cLoadDiv);

    InitHeader(DataHeader, FDataFile, ftIdPoints);
    InitHeader(IdxHeader, FIdxFile, ftIdxHash);

    FQuery.Close;

    FQuery.SQL.Text :=
//      'SELECT ID, Latitude, Longitude ' +
//      'FROM ( ' +
//      '   SELECT ID, Latitude, Longitude, ' +
//      '    ROW_NUMBER() OVER (ORDER BY ID) AS [RowNumber] ' +
//      '    FROM mtbl_Buildings ' +
//      '' +
//      ') OrderedOrders ' +
//      'WHERE RowNumber >= :o1 AND RowNumber < :o2 + :o3';
      'select ' +
      'id, ' +
      'Latitude, ' +
      'Longitude, ' +
      'Address ' +
      'from ' +
      '( ' +
      '	SELECT ' +
      '	ROW_NUMBER() OVER (ORDER BY bk.ID) AS rn, ' +
      '	bk.id, ' +
      '	bk.Latitude, ' +
      '	bk.Longitude, ' +
      '	( ' +
      '		CASE WHEN RTRIM(ak.[Name]+'' ''+ak.[Type]) <> RTRIM(dk.[Name]+'' ''+dk.[Type]) ' +
      '		THEN RTRIM(ak.[Name]+'' ''+ak.[Type]+''.'') + '', '' ' +
      '		ELSE '''' END + ' +
      '		CASE WHEN RTRIM(dk.[Name]+'' ''+dk.[Type]) <> RTRIM(tk.[Name]+'' ''+tk.[Type]) ' +
      '		THEN RTRIM(dk.[Name]+'' ''+dk.[Type]+''.'') + '', '' +  RTRIM(tk.[Name]+'' ''+tk.[Type]+''.'') ' +
      '		ELSE RTRIM(tk.[Name]+'' ''+tk.[Type]+''.'') END + ' +
      '		'', '' + ' +
      '		sk.[Name] + '' '' + sk.[Type] + ''., '' + ' +
      '		bk.HouseNumber + '' '' + bk.CorpsNumber + '' '' + bk.BuildingNumber) AS Address ' +
      ' ' +
      '	FROM mtbl_Buildings bk ' +
      '	INNER JOIN mtbl_Streets sk ON sk.ID = bk.StreetID ' +
      '	INNER JOIN mtbl_Towns tk ON tk.ID = sk.TownID ' +
      '	INNER JOIN mtbl_Districts dk ON  dk.ID = tk.DistrictID ' +
      '	INNER JOIN mtbl_Areas ak ON  ak.ID = dk.AreaID ' +
      ') as oo ' +
      'where rn >= :o1 and rn < :o2 + :o3 ';


    Result := 0;

    for i := 0 to LoadCount do
    begin
      if FQuery.Active then
        FQuery.Close;

      FQuery.ParamByName('o1').AsInteger := i * cLoadDiv + 1;
      FQuery.ParamByName('o2').AsInteger := i * cLoadDiv + 1;
      FQuery.ParamByName('o3').AsInteger := cLoadDiv;

      FQuery.Open;

      while not FQuery.Eof do
      begin
        IdLatLonPoint := TIdLatLonPoint.Create(
            FQuery.FieldByName('ID').AsInteger,
            FQuery.FieldByName('Latitude').AsFloat,
            FQuery.FieldByName('Longitude').AsFloat);

        IdxHashPoint.GeoHash := EncodePoint(IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude, FPrecision);
        IdxHashPoint.Idx := FDataFile.Position;

        FDataFile.Write(IdLatLonPoint, SizeOf(IdLatLonPoint));
        FIdxFile.Write(IdxHashPoint, SizeOf(IdxHashPoint));
        Add(IdxHashPoint);

        s :=
          IntToStr(FQuery.FieldByName('ID').AsInteger) + ';' +
          FloatToStr(FQuery.FieldByName('Latitude').AsFloat) + ';' +
          FloatToStr(FQuery.FieldByName('Longitude').AsFloat)+ ';' +
          FQuery.FieldByName('Address').AsString;
        WriteLn(f, s);

        FQuery.Next;
        Inc(Result);
      end;

      FQuery.Close;
    end;
  end;

  FinalizeDataFile(DataHeader, FDataFile);
  FinalizeDataFile(IdxHeader, FIdxFile);

  FreeAndNil(FDataFile);
  FreeAndNil(FIdxFile);

  CloseFile(f);
end;

function TGeoHashBuildingsSearch.LoadFromFiles: Integer;
var
  IdxHashesBuffer: array[1..cIdxHashesBufferSize] of TGeoHashIdxRecord;
  LoadCount: UInt64;
  LastLoadBytesCount: UInt64;
  i: Integer;
  j: Integer;
  BuffSize: UInt64;
begin
  Result := 0;
  DivMod((FIdxFile.Size - FIdxFile.Position), SizeOf(TGeoHashIdxRecord) * cIdxHashesBufferSize, LoadCount, LastLoadBytesCount);
  for i := 0 to LoadCount do
  begin
    BuffSize := IfThen(i = LoadCount, LastLoadBytesCount div SizeOf(TGeoHashIdxRecord), cIdxHashesBufferSize);
    if FIdxFile.Read(IdxHashesBuffer, SizeOf(TGeoHashIdxRecord) * BuffSize) = SizeOf(TGeoHashIdxRecord) * BuffSize then
      for j := 1 to BuffSize do
      begin
        Add(IdxHashesBuffer[j]);
        Inc(Result);
      end;
  end;
end;

{ TGeoHashTownsSearch }

function TGeoHashTownsSearch.IsDataFileActual: Boolean;
begin
  Result := IsFileActual(FDataFile, ftIdPoints);
end;

function TGeoHashTownsSearch.IsIdxFileActual: Boolean;
begin
  Result := IsFileActual(FIdxFile, ftIdxHash);
end;

function TGeoHashTownsSearch.LoadFromDB: Integer;
var
  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;
  IdLatLonPoint: TIdLatLonPoint;
  IdxHashPoint: TGeoHashIdxRecord;
  bLoadFromCsvFile: Boolean;
  f: TextFile;
  s: string;
  sl: TStringList;
begin
  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);

  InitHeader(DataHeader, FDataFile, ftIdPoints);
  InitHeader(IdxHeader, FIdxFile, ftIdxHash);

  bLoadFromCsvFile := FileExists(FCsvFileName) and UseCsvFile;

  Assign(f, FCsvFileName);
  if FileExists(FCsvFileName) then
    Reset(f);

  if bLoadFromCsvFile and (FileSize(f) > 0) then
  begin
    sl := TStringList.Create;
    while not Eof(f) do
    begin
      ReadLn(f, s);
      sl.Delimiter := ';';
      sl.DelimitedText := s;
      if sl.Count = 3 then
      begin
        //1531294;59,868688;30,293949
        IdLatLonPoint :=
          TIdLatLonPoint.Create(
            StrToIntDef(sl.Strings[0], -1),
            StrToFloatDef(sl.Strings[1], 0),
            StrToFloatDef(sl.Strings[2], 0));

        IdxHashPoint.GeoHash := EncodePoint(IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude, FPrecision);
        IdxHashPoint.Idx := FDataFile.Position;

        FDataFile.Write(IdLatLonPoint, SizeOf(IdLatLonPoint));
        FIdxFile.Write(IdxHashPoint, SizeOf(IdxHashPoint));
        Add(IdxHashPoint);
      end;
    end;
    sl.Free;
  end
  else
  begin
    Rewrite(f);
    if not FConnection.Connected then
      FConnection.Connect;

//    FQuery.Close;
//    FQuery.SQL.Text := 'select count(*) as cnt from mtbl_Towns';
//    FQuery.Open;

    if FQuery.Active then
      FQuery.Close;

    FQuery.SQL.Text := //'SELECT ID, Latitude, Longitude FROM mtbl_Towns ORDER BY ID';

      'SELECT ' +
      'tk.ID, tk.Latitude, tk.Longitude, ' +
      '( ' +
      '  CASE WHEN RTRIM(RTRIM(ak.[Name])+'' ''+RTRIM(ak.[Type])) <> RTRIM(RTRIM(dk.[Name])+'' ''+RTRIM(dk.[Type])) ' +
      '  THEN RTRIM(RTRIM(ak.[Name])+'' ''+RTRIM(ak.[Type])+''.'') + '', '' ' +
      '  ELSE '''' END + ' +
      '  CASE WHEN RTRIM(RTRIM(dk.[Name])+'' ''+RTRIM(dk.[Type])) <> RTRIM(RTRIM (tk.[Name])+'' ''+RTRIM(tk.[Type])) ' +
      '  THEN RTRIM(RTRIM(dk.[Name])+'' '' + RTRIM(dk.[Type])+''.'') + '', '' +  RTRIM(RTRIM(tk.[Name])+'' ''+RTRIM(tk.[Type])+''.'') ' +
      '  ELSE RTRIM(RTRIM(tk.[Name]) + '' '' + RTRIM(tk.[Type])+''.'') END) as Address ' +
      'FROM mtbl_Towns tk ' +
      'INNER JOIN mtbl_Districts dk ON  dk.ID = tk.DistrictID ' +
      'INNER JOIN mtbl_Areas ak ON  ak.ID = dk.AreaID ' +
      'ORDER BY tk.ID';

    Result := 0;


    FQuery.Open;

    while not FQuery.Eof do
    begin
      IdLatLonPoint := TIdLatLonPoint.Create(
          FQuery.FieldByName('ID').AsInteger,
          FQuery.FieldByName('Latitude').AsFloat,
          FQuery.FieldByName('Longitude').AsFloat);

      IdxHashPoint.GeoHash := EncodePoint(IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude, FPrecision);
      IdxHashPoint.Idx := FDataFile.Position;

      FDataFile.Write(IdLatLonPoint, SizeOf(IdLatLonPoint));
      FIdxFile.Write(IdxHashPoint, SizeOf(IdxHashPoint));
      Add(IdxHashPoint);

      s :=
        IntToStr(FQuery.FieldByName('ID').AsInteger) + ';' +
        FloatToStr(FQuery.FieldByName('Latitude').AsFloat) + ';' +
        FloatToStr(FQuery.FieldByName('Longitude').AsFloat) + ';' +
        FQuery.FieldByName('Address').AsString;
      WriteLn(f, s);

      FQuery.Next;
      Inc(Result);
    end;

    FQuery.Close;
  end;

  FinalizeDataFile(DataHeader, FDataFile);
  FinalizeDataFile(IdxHeader, FIdxFile);

  FreeAndNil(FDataFile);
  FreeAndNil(FIdxFile);

  CloseFile(f);
end;

function TGeoHashTownsSearch.LoadFromFiles: Integer;
var
  IdxHashesBuffer: array[1..cIdxHashesBufferSize] of TGeoHashIdxRecord;
  LoadCount: UInt64;
  LastLoadBytesCount: UInt64;
  i: Integer;
  j: Integer;
  BuffSize: UInt64;
begin
  Result := 0;
  DivMod((FIdxFile.Size - FIdxFile.Position), SizeOf(TGeoHashIdxRecord) * cIdxHashesBufferSize, LoadCount, LastLoadBytesCount);
  for i := 0 to LoadCount do
  begin
    BuffSize := IfThen(i = LoadCount, LastLoadBytesCount div SizeOf(TGeoHashIdxRecord), cIdxHashesBufferSize);
    if FIdxFile.Read(IdxHashesBuffer, SizeOf(TGeoHashIdxRecord) * BuffSize) = SizeOf(TGeoHashIdxRecord) * BuffSize then
      for j := 1 to BuffSize do
      begin
        Add(IdxHashesBuffer[j]);
        Inc(Result);
      end;
  end;
end;

{ TGeoHashRoutesSearch }

function TGeoHashRoutesSearch.FindNearestRouteInRadius(
  const aLatitude, aLongitude: Double;
  const aRouteRadius, aSpeedRadius: Double;
  const aPrecision: Integer;
  out aIdx: Integer; out aIdxAround: TArray<Integer>;
  const aLog: TStringList): Boolean;

  function GeoDistancePointToLine(const aLat, aLon, aLat1, aLon1, aLat2, aLon2: Double): Double;
  var
    x1, y1, x2, y2, CorrLon1, CorrLon2: Double;
  begin
    // Нивелируем эффект прокрутки при пересечении линии дат
    if aLon1 - aLon > 180 then
      CorrLon1 := aLon1 - 360
    else if aLon - aLon1 > 180 then
      CorrLon1 := aLon1 + 360
    else
      CorrLon1 := aLon1;

    if aLon2 - aLon > 180 then
      CorrLon2 := aLon2 - 360
    else if aLon - aLon2 > 180 then
      CorrLon2 := aLon2 + 360
    else
      CorrLon2 := aLon2;

    // Разворачиваем в декартову систему координат (с единицей "градус широты"). За начало координат белём точку (aLat, aLon)
    x1 := (CorrLon1 - aLon) * Cos(DegToRad(aLat));
    y1 := aLat1 - alat;

    x2 := (CorrLon2 - aLon) * Cos(DegToRad(aLat));
    y2 := aLat2 - alat;

    if (y2 = y1) and (x2 = x1) then
      Result := 0
    else if x1 * (x1 - x2) + y1 * (y1 - y2) < 0 then  // Если проекция точки оказалась за точкой (х1, y1)
      Result := Sqrt(Sqr(x1) + Sqr(y1))
    else if x2 * (x2 - x1) + y2 * (y2 - y1) < 0 then  // Если проекция точки оказалась за точкой (х2, y2)
      Result := Sqrt(Sqr(x2) + Sqr(y2))
    else                                              // Если проекция попадает на отрезок (х1, y1) - (x2, y2)
      Result := Abs(x2 * y1 - y2 * x1)/Sqrt(Sqr(y2 - y1) + Sqr(x2 - x1));
    Result := Result * 111.225                        // Переводим в километры
  end;

  function Found(const aIdAround: TArray<Integer>; const aID: Integer): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(aIdAround) to High(aIdAround) do
      if aIdAround[i] = aID then
        Exit(True);
  end;

var
  Points: TGeoHashIdxArray;
  i: Integer;
  Distance: Double;
  MinDistance: Double;
  IdxGeoHashLines: TIdxGeoHashLines;
  LinePoint: TIdLatLonPoint;
  pLeft, pCenter, pRight: TIdLatLonPoint;
  dLeft, dCenter, dRight: Double;
  Idx: Integer;
begin
  Result := False;
  MinDistance := Infinity;
  aIdx := -1;
  aIdxAround := nil;

  FindNearestHashesInRadius(aLatitude, aLongitude, aRouteRadius, aPrecision, Points, aLog);
  for i := Low(Points) to High(Points) do
  begin
    dCenter := Infinity;
    dLeft   := Infinity;
    dRight  := Infinity;
    if not Assigned(FDataFile) then
      DataFileOpen(FDataFileName, False, FDataFile);

    if not Assigned(FIdxFile) then
      DataFileOpen(FIdxFileName, False, FIdxFile);


    FIdxFile.Position := Points[i].Idx;

    if FIdxFile.Read(IdxGeoHashLines, SizeOf(IdxGeoHashLines)) = SizeOf(IdxGeoHashLines) then
    begin
      if IdxGeoHashLines.PointsPos.Center < 0 then
        Continue;

      FDataFile.Position := IdxGeoHashLines.PointsPos.Center;
      FDataFile.Read(pCenter, SizeOf(TIdLatLonPoint));
      dCenter := GeoLength(aLatitude, aLongitude, pCenter.p.Latitude, pCenter.p.Longitude);

      if Assigned(aLog) then
        aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f)', [pCenter.ID, aLatitude, aLongitude, dCenter, pCenter.p.Latitude, pCenter.p.Longitude]));

      if IdxGeoHashLines.PointsPos.Left >= 0 then
      begin
        FDataFile.Position := IdxGeoHashLines.PointsPos.Left;
        FDataFile.Read(pLeft, SizeOf(TIdLatLonPoint));
        dLeft := GeoDistancePointToLine(aLatitude, aLongitude, pLeft.p.Latitude, pLeft.p.Longitude, pCenter.p.Latitude, pCenter.p.Longitude);
        if Assigned(aLog) then
          aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f):(%8.10f, %8.10f)', [pCenter.ID, aLatitude, aLongitude, dLeft, pLeft.p.Latitude, pLeft.p.Longitude, pCenter.p.Latitude, pCenter.p.Longitude]));
      end;

      if IdxGeoHashLines.PointsPos.Right >= 0 then
      begin
        FDataFile.Position := IdxGeoHashLines.PointsPos.Right;
        FDataFile.Read(pRight, SizeOf(TIdLatLonPoint));
        dRight := GeoDistancePointToLine(aLatitude, aLongitude, pRight.p.Latitude, pRight.p.Longitude, pCenter.p.Latitude, pCenter.p.Longitude);
        if Assigned(aLog) then
          aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f):(%8.10f, %8.10f)', [pCenter.ID, aLatitude, aLongitude, dRight, pRight.p.Latitude, pRight.p.Longitude, pCenter.p.Latitude, pCenter.p.Longitude]));
      end;

      Distance := Min(Min(dCenter, dLeft), dRight);

      if Distance < MinDistance then
      begin
        MinDistance := Distance;
        aIdx := pCenter.ID;
        Result := True;
        if Assigned(aLog) then
          aLog.Add(Format('Found new closest %d: %8.10f', [pCenter.ID, Distance]));

//        if MinDistance = 0 then
//          Break;
      end;

      if GeoLengthDeg(pCenter.p.Latitude, pCenter.p.Longitude , aLatitude, aLongitude) * 1000 <= aSpeedRadius then
        if not Found(aIdxAround, pCenter.ID) then
        begin
          Idx := Length(aIdxAround);
          SetLength(aIdxAround, Idx + 1);
          aIdxAround[Idx] := pCenter.ID;
        end;

      if Assigned(aLog) then
        aLog.Add('-----------------------------');
    end;
  end;
end;

function TGeoHashRoutesSearch.IsDataFileActual: Boolean;
begin
  Result := IsFileActual(FDataFile, ftIdPoints);
end;

function TGeoHashRoutesSearch.IsIdxFileActual: Boolean;
begin
  Result := IsFileActual(FIdxFile, ftIdxHashPoints);
end;

//var
//  gHashMock: UInt32;
//
//function HashMock: UInt32;
//begin
//  Result := gHashMock;
//  Inc(gHashMock);
//end;
//
//procedure ResetHashMock;
//begin
// gHashMock := 1;
//end;

function TGeoHashRoutesSearch.LoadFromDB: Integer;

  function GetSqlResult(aSql: string): Integer;
  begin
    FQuery.Close;
    FQuery.SQL.Text := aSql;
    FQuery.Open;
    Result := FQuery.FieldByName('result').AsInteger;
  end;

const
  cLoadDiv = 100;
var
  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;

  MaxCount: Integer;
  LoadCount: Integer;
  i: Integer;

  Id: Integer;
  PointsCount: Integer;
  PointNo: Integer;
  Latitude: Double;
  Longitude: Double;

  LinePoint: TIdLatLonPoint;

  PosRight,
  PosCenter,
  PosLeft: Integer;

  HashCenter,
  HashRight : TGeoHashValue;

  s: string;
  sl: TStringList;

  CsvFileName: string;
  f: TextFile;

  procedure ResetPointsFlow;
  begin
    PosRight := -1;
    PosCenter := -1;
    HashCenter := 0;
  end;

  procedure SaveAndAddPoint(aPosLeft, aPosCenter, aPosRight: Int32; aHashCenter: TGeoHashValue);
  var
    IdxGeoHashLines: TIdxGeoHashLines;
    IdxGeoHash: TGeoHashIdxRecord;
  begin
    IdxGeoHashLines.GeoHash := aHashCenter;
    IdxGeoHashLines.PointsPos.Left  := aPosLeft;
    IdxGeoHashLines.PointsPos.Center   := aPosCenter;
    IdxGeoHashLines.PointsPos.Right := aPosRight;

    IdxGeoHash.GeoHash := aHashCenter;
    IdxGeoHash.Idx := FIdxFile.Position;

    FIdxFile.Write(IdxGeoHashLines, SizeOf(IdxGeoHashLines));

    Add(IdxGeoHash);
  end;

  procedure ProcessData;
  begin
    PosLeft := PosCenter;
    PosCenter := PosRight;
    PosRight := FDataFile.Position;

    LinePoint.ID := Id;
    LinePoint.p.Latitude := Latitude;
    LinePoint.p.Longitude := Longitude;
    FDataFile.Write(LinePoint, SizeOf(TIdLatLonPoint));

    HashCenter := HashRight;
    HashRight := EncodePoint(Latitude, Longitude, FPrecision);// HashMock;

    if PointNo > 1 then
    begin
      SaveAndAddPoint(PosLeft, PosCenter, PosRight, HashCenter);
      if PointNo = PointsCount then
      begin
        SaveAndAddPoint(PosCenter, PosRight, -1, HashRight);
        ResetPointsFlow;
      end;
    end;
  end;

var
  bLoadFromCsvFile: Boolean;

begin
//  ResetHashMock;
  Result := 0;

  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);

  bLoadFromCsvFile := FileExists(FCsvFileName) and UseCsvFile;

  Assign(f, FCsvFileName);
  if FileExists(FCsvFileName) then
    Reset(f);

  InitHeader(DataHeader, FDataFile, ftIdPoints);
  InitHeader(IdxHeader, FIdxFile, ftIdxHashPoints);

  if bLoadFromCsvFile and (FileSize(f) > 0) then
  begin
    MaxCount := -1;

    sl := TStringList.Create;
    ResetPointsFlow;
    while not Eof(f) do
    begin
      ReadLn(f, s);
      sl.Delimiter := ';';
      sl.DelimitedText := s;
      if sl.Count = 5 then
      begin
        //1531294;23;59,868688;30,293949
        Id          := StrToIntDef(sl.Strings[0], -1);
        PointsCount := StrToIntDef(sl.Strings[1], 0);
        PointNo     := StrToIntDef(sl.Strings[2], 0);
        Latitude    := StrToFloatDef(sl.Strings[3], 0);
        Longitude   := StrToFloatDef(sl.Strings[4], 0);

        if Id >= 0 then
          ProcessData;
      end;
    end;
    sl.Free;
  end
  else
  begin
    Rewrite(f);
    if not FConnection.Connected then
      FConnection.Connect;

    MaxCount := GetSqlResult('select count(1) as result from mtbl_Routes');
    LoadCount :=  Ceil(MaxCount/cLoadDiv);

    FQuery.Close;

    FQuery.SQL.Text :=
      'DECLARE	@return_value int ' +
      'EXEC @return_value = [dbo].[sprc_GetRoutePoints] ' + //sprc_GetRouteLines
      '  @aFrom = :AStart, ' +
      '	 @aCount = :ACount';// +
      //'SELECT	''Return_value'' = @return_value';

    for i := 0 to LoadCount do
    begin

      if FQuery.Active then
        FQuery.Close;

      FQuery.ParamByName('AStart').AsInteger := i * cLoadDiv + 1;
      FQuery.ParamByName('ACount').AsInteger := cLoadDiv;

      FQuery.Open;

      FQuery.First;

      ResetPointsFlow;

      while not FQuery.Eof do
      begin
        Id          := FQuery.FieldByName('Id').AsInteger;
        PointsCount := FQuery.FieldByName('PointsCount').AsInteger;
        PointNo     := FQuery.FieldByName('PointNo').AsInteger;
        Latitude    := FQuery.FieldByName('Latitude').AsFloat;
        Longitude   := FQuery.FieldByName('Longitude').AsFloat;

        s := IntToStr(id) + ';' + IntToStr(PointsCount) + ';' + IntToStr(PointNo) + ';' + FloatToStr(Latitude) + ';' + FloatToStr(Longitude);// + #13#10;
        WriteLn(f, s);

        ProcessData;

        FQuery.Next;
        Inc(Result);
      end;

      FQuery.Close;
    end;
  end;

  FinalizeDataFile(DataHeader, FDataFile);
  FinalizeDataFile(IdxHeader, FIdxFile);

  FreeAndNil(FDataFile);
  FreeAndNil(FIdxFile);

  CloseFile(f);
end;

function TGeoHashRoutesSearch.LoadFromFiles: Integer;
var
  IdxHashesBuffer: array[0..cRoutesIdxHashesBufferSize - 1] of TIdxGeoHashLines;
  IdxGeoHashLines: TIdxGeoHashLines;
  IdxGeoHash: TGeoHashIdxRecord;
  LoadCount: UInt64;
  LastLoadBytesCount: UInt64;
  i: Integer;
  j: Integer;
  IdxFilePos: Integer;
  BuffSize: UInt64;
begin
  Result := 0;
  DivMod((FIdxFile.Size - FIdxFile.Position), SizeOf(TIdxGeoHashLines) * cRoutesIdxHashesBufferSize, LoadCount, LastLoadBytesCount);
  for i := 0 to LoadCount do
  begin
    BuffSize := IfThen(i = LoadCount, LastLoadBytesCount div SizeOf(TIdxGeoHashLines), cRoutesIdxHashesBufferSize);
    IdxFilePos := FIdxFile.Position;
    if FIdxFile.Read(IdxHashesBuffer, SizeOf(TIdxGeoHashLines) * BuffSize) = SizeOf(TIdxGeoHashLines) * BuffSize then
      for j := 0 to BuffSize - 1 do
      begin
        IdxGeoHashLines := IdxHashesBuffer[j];
        IdxGeoHash.GeoHash := IdxGeoHashLines.GeoHash;
        IdxGeoHash.Idx     := IdxFilePos + j * SizeOf(TIdxGeoHashLines);
        Add(IdxGeoHash);
        Inc(Result);
      end;
  end;
end;

{ TGeoHashSearchFileHeader }

constructor TGeoHashSearchFileHeader.Create(aFileType: TSearchFileType; aMajor,
  aMinor: Byte; aChangedDateTime: TDateTime);
begin
  FileHeaderType.RecordID := Ord(rtType);
  FileHeaderType.FileType := Ord(aFileType);
  Version.RecordID := Ord(rtVersion);
  Version.FileVersion.Major := aMajor;
  Version.FileVersion.Minor := aMinor;
  ChangedDateTime.RecordID := Ord(rtChangedDateTime);
  ChangedDateTime.ChangedDateTime := aChangedDateTime
end;

{ TGeoHashEntryBlock }

procedure TGeoHashEntryBlock.Add(const aPoint: TGeoHashIdxRecord);
begin
  if not Assigned(FNextLink) then
    FNextLink := TGeoHashBlock.Create(FCapacity);
  FNextLink.Add(aPoint);
end;

constructor TGeoHashEntryBlock.Create(aCapacity: Integer);
begin
  FCapacity := aCapacity;
  FNextLink := nil;
end;

destructor TGeoHashEntryBlock.Destroy;
begin
  FNextLink.Free;
end;

function TGeoHashEntryBlock.FindPoints(aHash: Int64;
  var aPoints: TGeoHashIdxArray): Boolean;
begin
  Result := False;
  if Assigned(FNextLink) then
    Result := FNextLink.FindPoints(aHash, aPoints);
end;

{ TIDLatLonGeoHashedPoint }

constructor TIdLatLonPoint.Create(aLatitude, aLongitude: Double);
begin
  ID := MaxInt;
  p.Latitude := aLatitude;
  p.Longitude := aLongitude;
end;

constructor TIdLatLonPoint.Create(aID: Int64; aLatitude, aLongitude: Double);
begin
  ID := aID;
  p.Latitude := aLatitude;
  p.Longitude := aLongitude;
end;

end.


