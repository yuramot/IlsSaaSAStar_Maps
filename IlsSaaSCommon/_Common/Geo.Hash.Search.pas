unit Geo.Hash.Search;
interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs, Math,
  Windows, Geo.Pos, Geo.Hash, Geo.Calcs, System.Generics.Defaults;

const

  cCurrentApiVersionMajor     = 1;
  cCurrentApiVersionMinor     = 7;
  cCurrentApiVersionRevision  = 1;

  cCurrentDataVersionMajor    = 6;
  cCurrentDataVersionMinor    = 1;
  cCurrentDataVersionRevision = 1;

  cHashBlockSize                  = 1024*4;
  cHashSizeInBitsMax              = 12*5;
  cBuildingsHashBucketSizeInBits  = 13;//12;
  cTownsHashBucketSizeInBits      = 13;//12;
  cRoutesHashBucketSizeInBits     = 11;//12;
  cDistrictsHashBucketSizeInBits  = 13;//12;
  cAreasHashBucketSizeInBits      = 13;//12;
  cBuildingsHashBucketSize        = 1 shl (cBuildingsHashBucketSizeInBits);
  cTownsHashBucketSize            = 1 shl (cTownsHashBucketSizeInBits);
  cRoutesHashBucketSize           = 1 shl (cRoutesHashBucketSizeInBits);
  cDistrictsHashBucketSize        = 1 shl (cDistrictsHashBucketSizeInBits);
  cAreasHashBucketSize            = 1 shl (cAreasHashBucketSizeInBits);

  cGeoHashPrecision               = 32;
  cGeoHashBuildingsPrecision      = 32;// 7*5;//7 - optimal for 300m = 0ms
  cGeoHashTownsPrecision          = 32;//7*5;//7 - optimal for 300m = 0ms
  cGeoHashRoutesPrecision         = 32;//7*5;
  cGeoHashDistrictsPrecision      = 32;//7*5;
  cGeoHashAreasPrecision          = 32;//7*5;

  cDefBuildingRadius = 200;
  cDefTownRadius = 4000;
  cDefRouteRadius = 200;
  cDefSpeedRadius = 25;

  cIdxHashesBufferSize = 1000000;
  cRoutesIdxHashesBufferSize = 1000000;

  CMaxRouteDistance = 200;

  cGeoHashWrongCoordinates = '�������������� �� ����������.';//'�������� ����������.';
  cGeoHashAddressUndefined = '����� �� ��������...';

  cExtBin = '.bin';
  cExtIdx = '.idx';
  cExtCsv = '.csv';
  cExtAddr = '.address';

type

  TSearchFileType = (ftUnknown, ftIdPoints, ftIdxHash, ftIdxHashPoints);
  TSearchRecordType = (rtUnknown, rtType, rtVersion, rtChangedDateTime);

  TLatLonArray = TArray<TGeoPos>;

  TUniversalIDVariant = (ivGUID, ivInt64, ivByteArray);

  TUniversalIDRecord = packed record
    case IdVariant: TUniversalIDVariant  of
      ivGUID: (AsGUID: TGUID);
      ivInt64: (AsInt64Rec: packed record
        Hi: UInt64;
        Lo: Uint64;
      end);
      ivByteArray: (AsByteArray: array [0..15] of Byte);
//     end;//case
//    constructor Create(aGUID : TGUID); //overload;
  end;

  TGeoHashIdxRecord = packed record
    GeoHash: TGeoHashValue;
    Idx: UInt32;
//    AddresIdx: TUniversalIDRecord; //���� ������������
    AddresIdx: UInt32; //���� ������������
  end;

  TGeoHashIdxArray = TArray<TGeoHashIdxRecord>;

  TIntIdLatLonPoint = packed record
    ID: UInt32;
    p: TGeoPos;
  end;

  TIdLatLonPoint = packed record
    ID: TUniversalIDRecord;
//    ID: UInt32;
    p: TGeoPos;

    constructor Create(aLatitude: Double; aLongitude: Double); overload;
    constructor Create(aID: TUniversalIDRecord; aLatitude: Double; aLongitude: Double); overload;
    constructor Create(sID: string; aLatitude: Double; aLongitude: Double); overload;
  end;

  PIDLatLonPoint = ^TIdLatLonPoint;

  TGeoHashIdxArrRecord = packed record
    AreaID : Byte;
    Idx: UInt32;
    AddresIdx: UInt32;
    ArrayCount : UInt32;
    MinHash : UInt32;
    MaxHash : UInt32;
//    PointsArray: TGeoPointArray;
  end;

  TGeoLineRecord = packed record
    Idx: UInt32;
    AddresIdx: UInt32;
    ArrayCount : UInt32;
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

  TIDLines = packed record
    OSM_ID : Int64;
    HashStart: Int64;
    HashEnd: Int64;
    function ToString: string;
    function ReverseID: TIDLines;
    function StrToIDLines(const ARouteID: string): TIDLines;
    function StrToIDLinesReverse(const ARouteID: string): TIDLines;
    function Equal(const AOsmID, AStart, AEnd: Int64): Boolean;
  end;

  TIDLinesAzimDist = packed record
    ID: TIDLines;
    Azimuth: Double;
    Distance: Double;
    IdxDataPos : int64;
  end;

  TLinesSort = packed record
    Estimation : Double;
    DistToLine : Double;
    AzimuthAngle : Double;
    ID : TIDLines;
    IdxDataPos : int64;
  end;

  TLinesSortArray = TArray<TLinesSort>;

  TRouteBinData = packed record
    RouteID : TIDLines;
    Point : TGeoPos;
    AddresIdx : UInt32;
  end;

  TBoundingBoxes = packed record
    PointMin : TGeoPos;
    PointMax : TGeoPos;
  end;

  TArea = packed record
    AreaID : Byte;
    Idx: UInt32;
    AddresIdx: UInt32;
    Box : TBoundingBoxes;
    PointsArray : TGeoPosArray;
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
      var aHashes: TArray<TGeoHashValue>): Boolean; overload;

    procedure Add(const aPoint: TGeoHashIdxRecord);

    constructor Create(aHashBucketSize: Integer);
    destructor Destroy; override;
  end;

  TGeoHashSearch = class(TGeoHashMemorySearch)
  private
    FDataFileName: string;
    FIdxFileName: string;
    FCsvFileName: string;
    FAddressDataFileName: string;

    FDataFile: TFileStream;
    FIdxFile: TFileStream;
    FAddressDataFile: TFileStream;
    FAddressStreamReader: TStreamReader;
    FIdxGeoHashLines : array of TIdxGeoHashLines;

    FLoaded: Boolean;
    FLoadedDict: TDictionary<string, Integer>;
    FTryToLoadFromCSV: Boolean;
    FRecordCount : UInt32;
    FAzimuthPrice : integer;
    FDistancePrice : integer;
    FAzimuthDeviation : integer;

    FCSData: TCriticalSection;

    procedure SetDataFileName(const Value: string; const AHashStr: string); virtual;
    function RemoveTrailingPathDelimiters(const aPath: string): string;
    procedure InitDataFile(const aFileName: string; var aFile: TFileStream);
    procedure FinalizeDataFile(aHeader: TGeoHashSearchFileHeader; aFile: TFileStream);
    function IsDataFileActual: Boolean; overload; virtual; abstract;
    function IsIdxFileActual: Boolean; overload; virtual; abstract;
    function IsFileActual(aFileStream: TFileStream; aSearchFileType: TSearchFileType): Boolean; overload;
  public
//    property DataFileName: string read FDataFileName write SetDataFileName;
    property RecordCount: UInt32 read FRecordCount write FRecordCount;
    property TryToLoadFromCSV: Boolean read FTryToLoadFromCSV write FTryToLoadFromCSV;

    function DataFileOpen(const aFileName: string; const aCreate: Boolean; out aFile: TFileStream): Boolean;
    procedure InitHeader(out aHeader: TGeoHashSearchFileHeader; aFile: TFileStream; aFileType: TSearchFileType);

    procedure SetAzParams(AzPrice, DistPrice, AzDev : integer);
    function LoadFromCSV(AHashStr: string): Integer; virtual; abstract;
    function LoadFromFiles(AHashStr: string): Integer; virtual; abstract;

    function Load(const ALatitude, ALongitude: Double) : Boolean;

    function FindNearestHashesInRadius(
      const aLatitude, aLongitude: Double;
      const aRadius: Double;
      out aHashIdxArray: TGeoHashIdxArray;
      const aLog: TStringList = nil): Boolean;

    function FindNearestPointInRadius(
      const aLatitude, aLongitude: Double;
      const aRadius: Double;
      out aIdx: UInt32;
//      out aAddressIdx: TUniversalIDRecord;
      out aAddressIdx: UInt32;
      const aLog: TStringList = nil): Boolean; virtual;

    function CheckArea(
      const AArea: TGeoPosArray;
      const APoint: TGeoPos
    ): Boolean;

    function GetAddress(const aIdx: Integer;const aLog: TStringList = nil): string; virtual;
    function GetUniversalID(const aIdx: Integer;const aLog: TStringList = nil): TUniversalIDRecord; virtual;

    constructor Create(aHashBucketSize: Integer);
    destructor Destroy; override;
  end;

  TGeoHashBuildingsSearch = class(TGeoHashSearch)
  private
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromCSV(AHashStr: string): Integer; override;
    function LoadFromFiles(AHashStr: string): Integer; override;
    constructor Create(aHashBucketSize: Integer);
  end;

  TGeoHashTownsSearch = class(TGeoHashSearch)
  private
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromCSV(AHashStr: string): Integer; override;
    function LoadFromFiles(AHashStr: string): Integer; override;
  end;

  TGeoHashDistrictsSearch = class(TGeoHashSearch)
  private
    BorderArray : TArray<TArea>;
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromCSV(AHashStr: string): Integer; override;
    function LoadFromFiles(AHashStr: string): Integer; override;
    function SearchPointInArea(const aLatitude, aLongitude : Double;
                               out aIdx: UInt32;
                               out aAddressIdx: UInt32;
                               const aLog: TStringList) : Boolean;
  end;

  TGeoHashAreasSearch = class(TGeoHashSearch)
  private
    BorderArray : TArray<TArea>;
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromCSV(AHashStr: string): Integer; override;
    function LoadFromFiles(AHashStr: string): Integer; override;
    function SearchPointInArea(const aLatitude, aLongitude : Double;
                               out aIdx: UInt32;
                               out aAddressIdx: UInt32;
                               const aLog: TStringList) : Boolean;
  end;

  TGeoHashRoutesSearch = class(TGeoHashSearch)
  public
    function IsDataFileActual: Boolean; override;
    function IsIdxFileActual: Boolean; override;
    function LoadFromCSV(AHashStr: string): Integer; override;
    function LoadFromFiles(AHashStr: string): Integer; override;
    function LoadFromDataFiles(AOffset: Int64) : TRouteBinData; //override;

    function FindNearestRouteInRadius(
      const aLatitude, aLongitude: Double;
      const aRouteRadius, aSpeedRadius, aAzimuth: Double;
      var aIdx: TIDLines;
      var aIdxAround: TArray<TIDLines>;
      var aCurDistance, aCurAzimuth : Double;
      const aLog: TStringList): Boolean; virtual;

    function FindRouteNearestToPoint(
      const aLatitude, aLongitude: Double;
      const aRouteRadius: Double;
      var aIdxAround: TArray<TIDLinesAzimDist>;
      const ReturnCount: Integer;
      const aLog: TStringList;
      const ToRightOnly: Boolean = False): Boolean; virtual;
  end;

  TGeoHashPolygonSearch = class(TGeoHashSearch)
  private
//      function IsGeoPointInPolygon( const aPoint: TGeoPos; const aPolygon: TArray<TGeoPos> ): Boolean;
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

  TGeoHashSearchEngine = class
  strict protected
    class var FGeoHashSearchEngine: TGeoHashSearchEngine;
  private
    FGHSBuildings: TGeoHashBuildingsSearch;
    FGHSTowns: TGeoHashTownsSearch;
    FGHSRoutes: TGeoHashRoutesSearch;
    FGHSDistricts: TGeoHashDistrictsSearch;
    FGHSAreas: TGeoHashAreasSearch;

    FTryToLoadFromCSV: Boolean;
    FAzimuthPrice : integer;
    FDistancePrice : integer;
    FAzimuthDeviation : integer;

//    procedure SetWorkingPath(const Value: string);

    function LoadAddresses(const ALatitude, ALongitude: Double; const aBuildingPrecision: Integer; const aTownPrecision: Integer): Boolean;
    function LoadRoutes(const ALatitude, ALongitude: Double; const aRoutesPrecision: Integer): Boolean;
    function GetTryToLoadFromCsv: Boolean;
    procedure SetTryToLoadFromCsv(const Value: Boolean);
    function GetRoutesCount: UInt32;
    procedure SetRoutesCount(aCount : UInt32);

  public
    property RoutesCount : UInt32 read GetRoutesCount write SetRoutesCount;
//    property WorkingPath: string write SetWorkingPath;
    property TryToLoadFromCsv: Boolean read GetTryToLoadFromCsv write SetTryToLoadFromCsv;

    function CheckAddressDataLoaded(const ALatitude, ALongitude: Double): Boolean;
    function CheckRoutesDataLoaded(const ALatitude, ALongitude: Double): Boolean;
//    function CheckDataLoaded: Boolean;
    procedure SetAzParams(AzPrice, DistPrice, AzDev : integer);

    function FindNearestAddressInRadius(
      const aLatitude, aLongitude: Double;
      const aBuildingRadius: Double; const aTownRadius: Double;
      out aBuildingID: TUniversalIDRecord; out aTownID: TUniversalIDRecord;
      out aDistrictID: TUniversalIDRecord; out aAreaID: TUniversalIDRecord;
      out aAddress: string; out aAOLevel : byte;
      const aLog: TStringList = nil; aNeedAddress : Boolean = false): Boolean;

    function FindNearestRouteInRadius(
      const aLatitude, aLongitude: Double;
      const aRouteRadius, aSpeedRadius, aAzimuth: Double;
      out aRouteID: TIDLines; out aRoutesAround: TArray<TIDLines>;
      out aDistance, aCurAzimuth : double;
      const aLog: TStringList = nil): Boolean;

    function FindNearestAddressIDInRadius(
      const aLatitude, aLongitude: Double;
      const aBuildingRadius: Double; const aTownRadius: Double;
      out aAddressID: TUniversalIDRecord; out aAOLevel : byte;
      out aAddress: string; const aLog: TStringList = nil;
      aNeedAddress : Boolean = false): Boolean;

    function FindRouteNearestToPoint(
      const aLatitude, aLongitude: Double;
      const aRouteRadius: Double;
      out aIdxAround: TArray<TIDLinesAzimDist>;
      const ReturnCount: Integer;
      const aLog: TStringList;
      const ToRightOnly: Boolean = False): Boolean; virtual;

    function LoadFromDataFiles(const ALatitude, ALongitude: Double; AOffset: Int64) : TRouteBinData; virtual;
  public
    class function GeoHashAddressGetter: TGeoHashSearchEngine;
    class constructor Create; overload;
    class destructor Destroy;

    function GetApiVersion: string;
    function GetFilesVersion: string;
    function IsFilesAvailable(out RFileNames: TArray<string>): Boolean;

    constructor Create; overload;
    destructor Destroy; override;
  end;

function GeoHashSearchEngine: TGeoHashSearchEngine;

implementation

function GetAzimuth(P1, P2: TGeoPos): double;
var
  Lat1, Lon1, Lat2, Lon2, x, y : Double;
begin
  Lat1 := P1.Latitude/180*Pi;
  Lon1 := P1.Longitude/180*Pi;
  Lat2 := P2.Latitude/180*Pi;
  Lon2 := P2.Longitude/180*Pi;
  y := sin(Lon2-Lon1) * cos(Lat2);
  x := cos(Lat1)*sin(Lat2) - sin(Lat1)*cos(Lat2)*cos(Lon2-Lon1);
  Result := Math.arctan2(y, x)/Pi*180;
  if Result < 0 then
    Result := 360 + Result;
end;

procedure QuickSortEstimation(var A: array of TLinesSort; Lo, Hi, Direct: Integer);
var
  l, r: Integer;
  p: Double;
  tmpL: TLinesSort;
begin
  l := Lo;
  r := Hi;
  p := A[(l + r) div 2].Estimation;
  repeat
    if Direct = 0 then
    begin
      while A[l].Estimation < p do inc(l);
      while A[r].Estimation > p do dec(r);
    end else
    begin
      while A[l].Estimation > p do inc(l);
      while A[r].Estimation < p do dec(r);
    end;
    if l <= r then
    begin
      tmpL := A[l];
      A[l] := A[r];
      A[r] := tmpL;
      inc(l);
      dec(r);
    end;
  until l >= r;
  if Lo < r then QuickSortEstimation(A, Lo, r, Direct);
  if l < Hi then QuickSortEstimation(A, l, Hi, Direct);
end;

procedure QuickSortDist(var A: array of TLinesSort; Lo, Hi, Direct: Integer);
var
  l, r: Integer;
  p: Double;
  tmpL: TLinesSort;
begin
  l := Lo;
  r := Hi;
  p := A[(l + r) div 2].DistToLine;
  repeat
    if Direct = 0 then
    begin
      while A[l].DistToLine < p do inc(l);
      while A[r].DistToLine > p do dec(r);
    end else
    begin
      while A[l].DistToLine > p do inc(l);
      while A[r].DistToLine < p do dec(r);
    end;
    if l <= r then
    begin
      tmpL := A[l];
      A[l] := A[r];
      A[r] := tmpL;
      inc(l);
      dec(r);
    end;
  until l >= r;
  if Lo < r then QuickSortDist(A, Lo, r, Direct);
  if l < Hi then QuickSortDist(A, l, Hi, Direct);
end;

function MinDistToLine(P, PLine1, PLine2: TGeoPos; var Dist: Double): boolean;
var
  x1, y1, x2, y2, CorrLon1, CorrLon2: Double;
  aLat, aLon, aLat1, aLon1, aLat2, aLon2: Double;
begin
  if PLine1 = PLine2 then
  begin
    Dist := TGeoCalcs.GeoLengthDeg(P.Latitude, P.Longitude, PLine1.Latitude, PLine1.Longitude);
    Result := True;
    Exit;
  end;
  aLat := P.Latitude;
  aLon := P.Longitude;
  aLat1 := PLine1.Latitude;
  aLon1 := PLine1.Longitude;
  aLat2 := PLine2.Latitude;
  aLon2 := PLine2.Longitude;
  // ���������� ������ ��������� ��� ����������� ����� ���
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

  // ������������� � ��������� ������� ��������� (� �������� "������ ������"). �� ������ ��������� ���� ����� (aLat, aLon)
  x1 := (CorrLon1 - aLon) * Cos(DegToRad(aLat));
  y1 := aLat1 - alat;

  x2 := (CorrLon2 - aLon) * Cos(DegToRad(aLat));
  y2 := aLat2 - alat;

  if (y2 = y1) and (x2 = x1) then
  begin
    Result := true;
    Dist := 0;
  end
  else if x1 * (x1 - x2) + y1 * (y1 - y2) < 0 then // ���� �������� ����� ��������� �� ������ (�1, y1)
  begin
    Result := false;
    Dist := Sqrt(Sqr(x1) + Sqr(y1));
  end
  else if x2 * (x2 - x1) + y2 * (y2 - y1) < 0 then // ���� �������� ����� ��������� �� ������ (�2, y2)
  begin
    Result := false;
    Dist := Sqrt(Sqr(x2) + Sqr(y2));
  end
  else
  begin // ���� �������� �������� �� ������� (�1, y1) - (x2, y2)
    Result := true;
    Dist := Abs(x2 * y1 - y2 * x1)/Sqrt(Sqr(y2 - y1) + Sqr(x2 - x1));
  end;
  Dist := Dist * CEvDegLength; // ��������� � �����
end;

function MinDistToLineWithAzimuth(P, PLine1, PLine2: TGeoPos; CurAzimuth : Double;
AAzimuthPrice,ADistancePrice: integer;
var Dist, AzimuthAngle, Price: Double): boolean;
var
  aAzimuth : Double;
  aDist : Double;
begin
  aDist := -2;
  Result := MinDistToLine(P, PLine1, PLine2, aDist);
  Dist := aDist;
  aAzimuth := GetAzimuth(PLine1,PLine2);
  AzimuthAngle := Min(Abs(360-(CurAzimuth-aAzimuth)),Abs(360-(aAzimuth-CurAzimuth)));
  AzimuthAngle := Min(Abs(CurAzimuth-aAzimuth),AzimuthAngle);
  Price := AzimuthAngle * AAzimuthPrice + Dist * ADistancePrice;
{  if CurAzimuth = 0 then
  begin

  end;}
end;

function MinDistToLineWithSide(P, PLine1, PLine2: TGeoPos;
                              var Dist, Azimuth, Side: Double): boolean;
begin
  Result := MinDistToLine(P, PLine1, PLine2, Dist);
  Azimuth := GetAzimuth(PLine1,PLine2);
  Side := TGeoHash.PosPointFromVector(PLine1, PLine2, P);
end;

function Found(const aIdAround: TArray<TIDLines>; const aID: TIDLines): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(aIdAround) to High(aIdAround) do
    if (aIdAround[i].OSM_ID = aID.OSM_ID) and
       (aIdAround[i].HashStart = aID.HashStart) and
       (aIdAround[i].HashEnd = aID.HashEnd) then
      Exit(True);
end;

function GeoDistancePointToLine(const aLat, aLon, aLat1, aLon1, aLat2, aLon2: Double): Double;
var
  x1, y1, x2, y2, CorrLon1, CorrLon2: Double;
begin
  // ���������� ������ ��������� ��� ����������� ����� ���
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

  // ������������� � ��������� ������� ��������� (� �������� "������ ������"). �� ������ ��������� ���� ����� (aLat, aLon)
  x1 := (CorrLon1 - aLon) * Cos(DegToRad(aLat));
  y1 := aLat1 - alat;

  x2 := (CorrLon2 - aLon) * Cos(DegToRad(aLat));
  y2 := aLat2 - alat;

  if (y2 = y1) and (x2 = x1) then
    Result := 0
  else if x1 * (x1 - x2) + y1 * (y1 - y2) < 0 then  // ���� �������� ����� ��������� �� ������ (�1, y1)
    Result := Sqrt(Sqr(x1) + Sqr(y1))
  else if x2 * (x2 - x1) + y2 * (y2 - y1) < 0 then  // ���� �������� ����� ��������� �� ������ (�2, y2)
    Result := Sqrt(Sqr(x2) + Sqr(y2))
  else                                              // ���� �������� �������� �� ������� (�1, y1) - (x2, y2)
    Result := Abs(x2 * y1 - y2 * x1)/Sqrt(Sqr(y2 - y1) + Sqr(x2 - x1));
  Result := Result * CEvDegLengthKm;                // ��������� � ���������
end;

function IsGeoPointInPolygon( const aPoint: TGeoPos; const aPolygon: TArray<TGeoPos> ): Boolean;
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


function GeoHashSearchEngine: TGeoHashSearchEngine;
begin
  Result := TGeoHashSearchEngine.GeoHashAddressGetter;
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
  Result := False;
  for i := 0 to Length(aHashes) - 1 do
    FindPointsByHash(aHashes[i], aPoints);
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


class function TGeoHashMemorySearch.GetSurroundingHashes(
  const aRadius: Double; const aHash: TGeoHashValue;
  var aHashes: TArray<TGeoHashValue>): Boolean;
var
  BoundMin: TGeoPos;
  BoundMax: TGeoPos;
  Location: TGeoPos;
  LocBoundMin: TGeoPos;
  LocBoundMax: TGeoPos;
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

  function MakeGeoPoint(aLoc: TGeoPos; aLat, aLon: Integer)
    : TGeoPos;
  begin
    Result := aLoc;
    Result.Latitude := Result.Latitude + aLat * LatBoxSize;
    Result.Longitude := Result.Longitude + aLon * LonBoxSize;
  end;

begin
  Result := False;

  TGeoHash.GetAreaBoundingBox(BoundMin, BoundMax);

  if not TGeoHash.DecodePoint(aHash, BoundMin, BoundMax, cGeoHashPrecision, Location, LocBoundMin, LocBoundMax) then
    Exit;

  LatBoxSize := LocBoundMax.Latitude - LocBoundMin.Latitude;
  LonBoxSize := LocBoundMax.Longitude - LocBoundMin.Longitude;
  LatBoxMid := (LocBoundMax.Latitude + LocBoundMin.Latitude)/2;
  LonBoxMid := (LocBoundMax.Longitude + LocBoundMin.Longitude)/2;
  LatBoxSizeInMeters := TGeoCalcs.GeoLengthDeg(LocBoundMin.Latitude, LonBoxMid, LocBoundMax.Latitude, LonBoxMid);
  LonBoxSizeInMeters := TGeoCalcs.GeoLengthDeg(LatBoxMid, LocBoundMin.Longitude, LatBoxMid, LocBoundMax.Longitude);

  LatRadiusInHashes := Ceil(aRadius/LatBoxSizeInMeters);
  LonRadiusInHashes := Ceil(aRadius/LonBoxSizeInMeters);

  SetLength(aHashes, (LatRadiusInHashes*2+1) * (LonRadiusInHashes*2+1));
  for y := -LatRadiusInHashes to LatRadiusInHashes do
    for x := -LonRadiusInHashes to LonRadiusInHashes do
    begin
      i := (x + LonRadiusInHashes) + (y + LatRadiusInHashes) * (LonRadiusInHashes * 2 + 1);
      aHashes[i] := TGeoHash.EncodePoint(
        TGeoPos.Create(
          LatBoxMid + LatBoxSize * y,
          LonBoxMid + LonBoxSize * x),
          cGeoHashPrecision);
    end;
  Result := Length(aHashes) > 0;
end;

{ TGeoHashLinksGetter }
{
function TGeoHashSearchEngine.CheckDataLoaded: Boolean;
begin
  Result := CheckAddressDataLoaded and CheckRoutesDataLoaded;
end;               }

function TGeoHashSearchEngine.CheckAddressDataLoaded(const ALatitude, ALongitude: Double): Boolean;
begin
  Result := LoadAddresses(ALatitude, ALongitude, cGeoHashBuildingsPrecision, cGeoHashTownsPrecision);
end;

function TGeoHashSearchEngine.CheckRoutesDataLoaded(const ALatitude, ALongitude: Double): Boolean;
begin
  Result := LoadRoutes(ALatitude, ALongitude, cGeoHashRoutesPrecision);
end;


function TGeoHashSearchEngine.FindNearestAddressIDInRadius(const aLatitude,
  aLongitude, aBuildingRadius, aTownRadius: Double;
  out aAddressID: TUniversalIDRecord; out aAOLevel: byte;
  out aAddress: string; const aLog: TStringList; aNeedAddress : Boolean): Boolean;
var
  BuildingID, TownID, DistrictID, AreaID : TUniversalIDRecord;
begin
  Result := GeoHashSearchEngine.FindNearestAddressInRadius(
      aLatitude, aLongitude, aBuildingRadius,
      aTownRadius, BuildingID, TownID, DistrictID, AreaID, aAddress, aAOLevel, aLog, aNeedAddress);
  case aAOLevel of
  8 :
    begin
      aAddressID := BuildingID;
    end;
  4 :
    begin
      aAddressID := TownID;
    end;
  3 :
    begin
      aAddressID := DistrictID;
    end;
  1 :
    begin
      aAddressID := AreaID;
    end;
  end;
end;

function TGeoHashSearchEngine.FindNearestAddressInRadius(
  const aLatitude, aLongitude: Double;
  const aBuildingRadius, aTownRadius: Double;
  out aBuildingID: TUniversalIDRecord; out aTownID: TUniversalIDRecord;
  out aDistrictID: TUniversalIDRecord; out aAreaID: TUniversalIDRecord;
  out aAddress: string; out aAOLevel : byte;
  const aLog: TStringList; aNeedAddress : Boolean): Boolean;
var
  AddressIdx, BuildingIdx, TownIdx, DistrictIdx, AreaIdx: UInt32;
  BuildingRadius, TownRadius: Double;
begin
  Result := True;
  aAOLevel := 0;
  aBuildingID.AsInt64Rec.Hi := 0;
  aBuildingID.AsInt64Rec.Lo := 0;
  aTownID.AsInt64Rec.Hi := 0;
  aTownID.AsInt64Rec.Lo := 0;
  aDistrictID.AsInt64Rec.Hi := 0;
  aDistrictID.AsInt64Rec.Lo := 0;
  aAreaID.AsInt64Rec.Hi := 0;
  aAreaID.AsInt64Rec.Lo := 0;
  AddressIdx := 0;
  aAddress := '';

  BuildingRadius := IfThen(aBuildingRadius = 0, cDefBuildingRadius, aBuildingRadius);
  TownRadius := IfThen(aTownRadius = 0, cDefTownRadius, aTownRadius);

  CheckAddressDataLoaded(aLatitude, aLongitude);
//!!
  if FGHSBuildings.FindNearestPointInRadius(aLatitude, aLongitude,
                   BuildingRadius, BuildingIdx, AddressIdx, aLog) then
  begin
    if aNeedAddress then
    aAddress := FGHSBuildings.GetAddress(AddressIdx, aLog);
    aBuildingID := FGHSBuildings.GetUniversalID(BuildingIdx, aLog);
    aAOLevel := 8;
  end
  else
    if FGHSTowns.FindNearestPointInRadius(aLatitude, aLongitude, TownRadius, TownIdx, AddressIdx, aLog) then
    begin
      if aNeedAddress then
      aAddress := FGHSTowns.GetAddress(AddressIdx, aLog);
      aTownID := FGHSTowns.GetUniversalID(TownIdx, aLog);
      aAOLevel := 4;
    end
    else
      if FGHSDistricts.SearchPointInArea(aLatitude, aLongitude, DistrictIdx, AddressIdx, aLog) then
      begin
        if aNeedAddress then
        aAddress := FGHSDistricts.GetAddress(AddressIdx, aLog);
        aDistrictID := FGHSDistricts.GetUniversalID(DistrictIdx, aLog);
        aAOLevel := 3;
      end
      else
        if FGHSAreas.SearchPointInArea(aLatitude, aLongitude, AreaIdx, AddressIdx, aLog) then
        begin
          if aNeedAddress then
          aAddress := FGHSAreas.GetAddress(AddressIdx, aLog);
          aAreaID := FGHSAreas.GetUniversalID(AreaIdx, aLog);
          aAOLevel := 1;
        end
        else
          Exit(False);
end;

function TGeoHashSearchEngine.FindNearestRouteInRadius(
  const aLatitude, aLongitude: Double;
  const aRouteRadius, aSpeedRadius, aAzimuth: Double;
//  out aRouteID: Integer; out aRoutesAround: TArray<Integer>;
  out aRouteID: TIDLines; out aRoutesAround: TArray<TIDLines>;
      out aDistance, aCurAzimuth : double;
  const aLog: TStringList): Boolean;
var
  RouteRadius, SpeedRadius: Double;
begin
  RouteRadius := IfThen(aRouteRadius = 0, cDefRouteRadius, aRouteRadius);
  SpeedRadius := IfThen(aSpeedRadius = 0, cDefSpeedRadius, aSpeedRadius);

  CheckRoutesDataLoaded(aLatitude, aLongitude);

  Result :=
    FGHSRoutes.FindNearestRouteInRadius(
      aLatitude, aLongitude, RouteRadius, SpeedRadius,
      aAzimuth, aRouteID, aRoutesAround, aDistance, aCurAzimuth, aLog);
end;


function TGeoHashSearchEngine.FindRouteNearestToPoint(const aLatitude,
  aLongitude, aRouteRadius: Double;
  out aIdxAround: TArray<TIDLinesAzimDist>; const ReturnCount: Integer;
  const aLog: TStringList; const ToRightOnly: Boolean): Boolean;
var
  RouteRadius: Double;
begin
  RouteRadius := IfThen(aRouteRadius = 0, cDefRouteRadius, aRouteRadius);

  CheckRoutesDataLoaded(aLatitude, aLongitude);

  Result :=
    FGHSRoutes.FindRouteNearestToPoint(
      aLatitude, aLongitude, RouteRadius,
      aIdxAround, ReturnCount, aLog, ToRightOnly);
end;

function TGeoHashSearchEngine.LoadAddresses(const ALatitude, ALongitude: Double; const aBuildingPrecision, aTownPrecision: Integer): Boolean;
var
  HashStr: string;
begin
  HashStr := TGeoHash.EncodePointString(ALatitude, ALongitude, 3);
{  if FGHSTowns.DataFileName = '' then
    FGHSTowns.SetDataFileName := ExtractFilePath(GetModuleName(0)) + 'GeoData\Towns';
  if FGHSBuildings.DataFileName = '' then
    FGHSBuildings.DataFileName := ExtractFilePath(GetModuleName(0)) + 'GeoData\Buildings';
  if FGHSDistricts.DataFileName = '' then
    FGHSDistricts.DataFileName := ExtractFilePath(GetModuleName(0)) + 'GeoData\Districts';
  if FGHSAreas.DataFileName = '' then
    FGHSAreas.DataFileName := ExtractFilePath(GetModuleName(0)) + 'GeoData\Areas';}
  FGHSTowns.SetDataFileName(ExtractFilePath(GetModuleName(0)) + 'GeoData\Towns', HashStr);
  FGHSBuildings.SetDataFileName(ExtractFilePath(GetModuleName(0)) + 'GeoData\Buildings', HashStr);
  FGHSDistricts.SetDataFileName(ExtractFilePath(GetModuleName(0)) + 'GeoData\Districts', HashStr);
  FGHSAreas.SetDataFileName(ExtractFilePath(GetModuleName(0)) + 'GeoData\Areas', HashStr);
  FGHSBuildings.Load(ALatitude, ALongitude);
  FGHSTowns.Load(ALatitude, ALongitude);
  FGHSDistricts.Load(ALatitude, ALongitude);
  FGHSAreas.Load(ALatitude, ALongitude);
//  Result := True;
  Result := FGHSBuildings.FLoaded and FGHSTowns.FLoaded and
            FGHSDistricts.FLoaded and FGHSAreas.FLoaded;
//  Result := FGHSBuildings.FLoaded;
end;

function TGeoHashSearchEngine.LoadFromDataFiles(const ALatitude, ALongitude: Double; AOffset: Int64): TRouteBinData;
begin
  CheckRoutesDataLoaded(ALatitude, ALongitude);
  Result :=  FGHSRoutes.LoadFromDataFiles(AOffset);
end;

function TGeoHashSearchEngine.LoadRoutes(const ALatitude, ALongitude: Double; const aRoutesPrecision: Integer): Boolean;
var
  HashStr: string;
begin
//  if FGHSRoutes.DataFileName = '' then
//    FGHSRoutes.DataFileName := ExtractFilePath(GetModuleName(0)) + 'GeoData\Routes';
  HashStr := TGeoHash.EncodePointString(ALatitude, ALongitude, 3);
  FGHSRoutes.SetDataFileName(ExtractFilePath(GetModuleName(0)) + 'GeoData\Routes', HashStr);
  Result := FGHSRoutes.Load(ALatitude, ALongitude);
//  Result := FGHSRoutes.FLoaded;
end;


procedure TGeoHashSearchEngine.SetAzParams(AzPrice, DistPrice, AzDev: integer);
begin
  FAzimuthPrice := AzPrice;
  FDistancePrice :=  DistPrice;
  FAzimuthDeviation := AzDev;
  FGHSRoutes.SetAzParams(AzPrice, DistPrice, AzDev);
end;

procedure TGeoHashSearchEngine.SetRoutesCount(aCount: UInt32);
begin
  FGHSRoutes.RecordCount := aCount;
end;

procedure TGeoHashSearchEngine.SetTryToLoadFromCsv(const Value: Boolean);
begin
  FTryToLoadFromCSV := Value;
  FGHSBuildings.TryToLoadFromCSV := Value;
  FGHSTowns.TryToLoadFromCSV := Value;
  FGHSRoutes.TryToLoadFromCSV := Value;
  FGHSDistricts.TryToLoadFromCSV := Value;
  FGHSAreas.TryToLoadFromCSV := Value;
end;

{procedure TGeoHashSearchEngine.SetWorkingPath(const Value: string);
begin
  FGHSBuildings.DataFileName := Value + 'Buildings';
  FGHSTowns.DataFileName := Value + 'Towns';
  FGHSRoutes.DataFileName := Value + 'Routes';
  FGHSDistricts.DataFileName := Value + 'Districts';
  FGHSAreas.DataFileName := Value + 'Areas';
end;            }

class function TGeoHashSearchEngine.GeoHashAddressGetter: TGeoHashSearchEngine;
begin
  Result := FGeoHashSearchEngine;
end;

function TGeoHashSearchEngine.GetApiVersion: string;
begin
  Result :=
    IntToStr(cCurrentApiVersionMajor) + '.' +
    IntToStr(cCurrentApiVersionMinor) + '.' +
    IntToStr(cCurrentApiVersionRevision);
end;

function TGeoHashSearchEngine.GetFilesVersion: string;
begin
  Result :=
    IntToStr(cCurrentDataVersionMajor) + '.' +
    IntToStr(cCurrentDataVersionMinor) + '.' +
    IntToStr(cCurrentDataVersionRevision);
end;

function TGeoHashSearchEngine.GetRoutesCount: UInt32;
begin
  Result := FGHSRoutes.RecordCount;
end;

function TGeoHashSearchEngine.IsFilesAvailable(out RFileNames: TArray<string>): Boolean;
begin
  SetLength(RFileNames, 5);
  RFileNames[0] := FGHSBuildings.FDataFileName;
  RFileNames[1] := FGHSTowns.FDataFileName;
  RFileNames[2] := FGHSRoutes.FDataFileName;
  RFileNames[3] := FGHSDistricts.FDataFileName;
  RFileNames[4] := FGHSAreas.FDataFileName;
  Result :=
    FileExists(RFileNames[0]) and
    FileExists(RFileNames[1]) and
    FileExists(RFileNames[2]) and
    FileExists(RFileNames[3]) and
    FileExists(RFileNames[4]);
end;

function TGeoHashSearchEngine.GetTryToLoadFromCsv: Boolean;
begin
  Result := FTryToLoadFromCSV;
end;

class constructor TGeoHashSearchEngine.Create;
begin
  FGeoHashSearchEngine := TGeoHashSearchEngine.Create;
end;

class destructor TGeoHashSearchEngine.Destroy;
begin
  FGeoHashSearchEngine.Destroy;
end;

constructor TGeoHashSearchEngine.Create;
begin
  FGHSBuildings := TGeoHashBuildingsSearch.Create(cBuildingsHashBucketSize);
  FGHSTowns := TGeoHashTownsSearch.Create(cTownsHashBucketSize);
  FGHSRoutes := TGeoHashRoutesSearch.Create(cRoutesHashBucketSize);
  FGHSDistricts := TGeoHashDistrictsSearch.Create(cDistrictsHashBucketSize);
  FGHSAreas := TGeoHashAreasSearch.Create(cAreasHashBucketSize);
end;

destructor TGeoHashSearchEngine.Destroy;
begin
  FGHSBuildings.Free;
  FGHSTowns.Free;
  FGHSRoutes.Free;
  FGHSAreas.Free;
  FGHSDistricts.Free;
end;

{ TGeoHashSearch }

function TGeoHashSearch.DataFileOpen(const aFileName: string; const aCreate: Boolean; out aFile: TFileStream): Boolean;
var
  fm: Word;
begin
  Result := False;
  if Assigned(aFile) then
    Exit;

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

function TGeoHashSearch.CheckArea(const AArea: TGeoPosArray;
  const APoint: TGeoPos): Boolean;
var
  //!
  A, B: TGeoPos;
  //!
  PrevUnder, CurrUnder: Boolean;
  //!
  T: Double;
  //!
  Prev: Integer;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  Result := False;
  if (Length(AArea) < 3) then Exit;
  Prev := High(AArea);
  PrevUnder := AArea[Prev].Latitude < APoint.Latitude;
  for I := Low(AArea) to High(AArea) do
  begin
    CurrUnder := AArea[I].Latitude < APoint.Latitude;
    A.Latitude := AArea[Prev].Latitude - APoint.Latitude;
    A.Longitude := AArea[Prev].Longitude - APoint.Longitude;
    B.Latitude := AArea[I].Latitude - APoint.Latitude;
    B.Longitude := AArea[I].Longitude - APoint.Longitude;
    T := A.Longitude * (B.Latitude - A.Latitude) - A.Latitude * (B.Longitude - A.Longitude);
    if CurrUnder and (not PrevUnder) then
    begin
      if (T > 0) then
        Result := not Result;
    end;
    if (not CurrUnder) and PrevUnder then
    begin
      if (T < 0) then
        Result := not Result;
    end;
    Prev := I;
    PrevUnder := CurrUnder;
  end;
end;

constructor TGeoHashSearch.Create(aHashBucketSize: Integer);
begin
  inherited Create(aHashBucketSize);
  FDataFile := nil;
  FIdxFile := nil;
  FAddressDataFile := nil;
  FAddressStreamReader := nil;
  FAzimuthPrice := 1;
  FDistancePrice := 1;
  FAzimuthDeviation := 25;
//!!  FTryToLoadFromCSV := False;
  FTryToLoadFromCSV := True;
  FLoadedDict := TDictionary<string, Integer>.Create;

  FCSData := TCriticalsection.Create;
  SetLength(FIdxGeoHashLines,0);
end;

destructor TGeoHashSearch.Destroy;
begin
  FDataFile.Free;
  FIdxFile.Free;
  FAddressStreamReader.Free;
  FAddressDataFile.Free;

  FLoadedDict.Free;

  FCSData.Free;

  inherited;
end;

procedure TGeoHashSearch.FinalizeDataFile(aHeader: TGeoHashSearchFileHeader; aFile: TFileStream);
begin
  aFile.Seek(0, soBeginning);
  aFile.Write(aHeader, SizeOf(aHeader));
end;

function TGeoHashSearch.FindNearestHashesInRadius(
  const aLatitude, aLongitude, aRadius: Double;
  out aHashIdxArray: TGeoHashIdxArray;
  const aLog: TStringList = nil): Boolean;
var
  Hashes: TArray<TGeoHashValue>;
begin
  Result := False;

  if not FInited then
    Exit;

  GetSurroundingHashes(
    aRadius,
    TGeoHash.EncodePoint(
      TGeoPos.Create(
        aLatitude, aLongitude),
      cGeoHashPrecision),
    Hashes);

  FindPointsByHashes(Hashes, aHashIdxArray);
  Result := Length(aHashIdxArray) > 0;
end;

function TGeoHashSearch.FindNearestPointInRadius(
  const aLatitude, aLongitude: Double;
  const aRadius: Double;
  out aIdx: UInt32;
//  out aAddressIdx: TUniversalIDRecord;
  out aAddressIdx: UInt32;
  const aLog: TStringList): Boolean;
var
  Points: TGeoHashIdxArray;
  i: Integer;
  IdLatLonPoint: TIdLatLonPoint;
  Distance: Double;
  MinDistance: Double;
begin
  Result := False;
  aIdx := 0;
//  aAddressIdx.AsInt64Rec.Hi := 0;
//  aAddressIdx.AsInt64Rec.Lo := 0;
  aAddressIdx := 0;
  MinDistance := MaxInt;

  FindNearestHashesInRadius(aLatitude, aLongitude, aRadius, Points, aLog);
  for i := Low(Points) to High(Points) do
  begin
    if not Assigned(FDataFile) then
      DataFileOpen(FDataFileName, False, FDataFile);

    FCSData.Enter;
    try
      FDataFile.Position := Points[i].Idx;
      if FDataFile.Read(IdLatLonPoint, SizeOf(IdLatLonPoint)) = SizeOf(IdLatLonPoint) then
      begin
        Distance := TGeoCalcs.GeoLengthDeg(aLatitude, aLongitude,
                    IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude);
        if Assigned(aLog) then
          aLog.Add(Format('                  %s: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f)',
          [GUIDToString(IdLatLonPoint.ID.AsGUID), aLatitude, aLongitude, Distance, IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude]));
        if Distance < MinDistance then
        begin
          MinDistance := Distance;
          aIdx := Points[i].Idx;//IdLatLonPoint.ID;
          aAddressIdx := Points[i].AddresIdx;
          Result := True;
          if Assigned(aLog) then
            aLog.Add(Format('Found new closest %s: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f)',
            [GUIDToString(IdLatLonPoint.ID.AsGUID), aLatitude, aLongitude, Distance, IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude]));
        end;
      end;
    finally
      FCSData.Leave;
    end;
  end;
end;

function TGeoHashSearch.GetAddress(const aIdx: Integer;
  const aLog: TStringList): string;
begin
  Result := '';
  if not Assigned(FAddressDataFile) then
  begin
    DataFileOpen(FAddressDataFileName, False, FAddressDataFile);
    FAddressStreamReader := TStreamReader.Create(FAddressDataFile, TEncoding.UTF8, True);
  end;

  if aIdx < FAddressDataFile.Size then
  begin
    FAddressStreamReader.DiscardBufferedData;
    FAddressDataFile.Position := aIdx;
    Result := Trim(FAddressStreamReader.ReadLine);
  end;
end;

function TGeoHashSearch.GetUniversalID(const aIdx: Integer;
  const aLog: TStringList): TUniversalIDRecord;
begin
  Result.AsInt64Rec.Hi := 0;
  Result.AsInt64Rec.Lo := 0;
  if not Assigned(FDataFile) then
  begin
    DataFileOpen(FDataFileName, False, FDataFile);
//    FAddressStreamReader := TStreamReader.Create(FDataFile, TEncoding.ANSI, True);
  end;

  if aIdx < FDataFile.Size then
  begin
//    FAddressStreamReader.DiscardBufferedData;
    FCSData.Enter;
    try
      FDataFile.Position := aIdx;
      FDataFile.ReadBuffer(Result,SizeOf(TUniversalIDRecord));
    finally
      FCSData.Leave;
    end;
//    Result := Trim(FAddressStreamReader.ReadLine);
  end;
end;

procedure TGeoHashSearch.InitDataFile(const aFileName: string; var aFile: TFileStream);
begin
  FreeAndNil(aFile);
  DeleteFile(PWideChar(aFileName));
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

function TGeoHashSearch.Load(const ALatitude, ALongitude: Double) : Boolean;
var
  HashStr: string;
begin
  HashStr := TGeoHash.EncodePointString(ALatitude, ALongitude, 3);
  FLoadingCriticalSection.Enter;
  try
    if FLoadedDict.ContainsKey(HashStr) then
      Exit(True);

    Init;

    if DataFileOpen(FDataFileName, False, FDataFile) and IsDataFileActual and
       DataFileOpen(FIdxFileName, False, FIdxFile) and IsIdxFileActual then
      FRecordCount := LoadFromFiles(HashStr)
    else
      if FTryToLoadFromCSV then
        FRecordCount := LoadFromCSV(HashStr);

    FreeAndNil(FDataFile);
    FreeAndNil(FIdxFile);
    FreeAndNil(FAddressDataFile);

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

procedure TGeoHashSearch.SetAzParams(AzPrice, DistPrice, AzDev: integer);
begin
  FAzimuthPrice := AzPrice;
  FDistancePrice :=  DistPrice;
  FAzimuthDeviation := AzDev;
end;

procedure TGeoHashSearch.SetDataFileName(const Value: string; const AHashStr: string);
var
  DataFileName: string;
  IdxFileName: string;
  CsvFileName: string;
  AddressFileName: string;
begin
  DataFileName := Value + GetCurrentVersionFileNameSuffix + '_' + AHashStr + cExtBin;
  IdxFileName := Value + GetCurrentVersionFileNameSuffix + '_' + AHashStr + cExtIdx;
  CsvFileName := Value + GetCurrentVersionFileNameSuffix + '_' + AHashStr + cExtCsv;
  AddressFileName := Value + GetCurrentVersionFileNameSuffix + '_' + AHashStr + cExtAddr;

    FDataFileName := DataFileName;
    FIdxFileName := IdxFileName;
    FCsvFileName := CsvFileName;
    FAddressDataFileName := AddressFileName;
{  if (FDataFileName <> DataFileName) then
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
    FAddressDataFileName := AddressFileName;
  end;}
end;


{ TGeoHashBuildingsSearch }

constructor TGeoHashBuildingsSearch.Create(aHashBucketSize: Integer);
begin
  inherited Create(aHashBucketSize);
  FAddressDataFile := nil;
end;

function TGeoHashBuildingsSearch.IsDataFileActual: Boolean;
begin
  Result := IsFileActual(FDataFile, ftIdPoints);
end;

function TGeoHashBuildingsSearch.IsIdxFileActual: Boolean;
begin
  Result := IsFileActual(FIdxFile, ftIdxHash);
end;

function TGeoHashBuildingsSearch.LoadFromCSV(AHashStr: string): Integer;
var
  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;
  IdLatLonPoint: TIdLatLonPoint;
  IdxHashPoint: TGeoHashIdxRecord;

  CsvFS: TFileStream;
  CsvReader: TStreamReader;
  AddressWriter: TStreamWriter;
  bHasAddress: Boolean;
  sl: TStringList;
  s: string;
begin
  Result := 0;
  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);
  InitDataFile(FAddressDataFileName, FAddressDataFile);

  AddressWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.UTF8);

  InitHeader(DataHeader, FDataFile, ftIdPoints);
  InitHeader(IdxHeader, FIdxFile, ftIdxHash);


  CsvFS := nil;
  CsvReader := nil;
  try
    if FileExists(FCsvFileName) then
    begin
      CsvFS := TFileStream.Create(FCsvFileName, fmOpenRead + fmShareDenyNone);
      CsvReader := TStreamReader.Create(CsvFS, TEncoding.UTF8, True);
    end;

    if Assigned(CsvFS) and (CsvFS.Size > 0) then
    begin
      sl := TStringList.Create;
      try
        while CsvFS.Position < CsvFS.Size do
        begin
          s := CsvReader.ReadLine;
          sl.Delimiter := ';';
          sl.StrictDelimiter := True;
          sl.DelimitedText := s;
          bHasAddress := sl.Count >= 3;
          if sl.Count >= 3 then
          begin
            //1531294;59,868688;30,293949
            IdLatLonPoint :=
              TIdLatLonPoint.Create(
                sl.Strings[0],
                StrToFloatDef(sl.Strings[1], 0),
                StrToFloatDef(sl.Strings[2], 0));

            IdxHashPoint.GeoHash := TGeoHash.EncodePoint(IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude, cGeoHashPrecision);
            IdxHashPoint.Idx := FDataFile.Position;
            IdxHashPoint.AddresIdx := IfThen(bHasAddress, FAddressDataFile.Position , $FFFFFFFF);

            FDataFile.Write(IdLatLonPoint, SizeOf(IdLatLonPoint));
            FIdxFile.Write(IdxHashPoint, SizeOf(IdxHashPoint));
            AddressWriter.Write(sl.Strings[3] + #13#10);
            Add(IdxHashPoint);
          end;
        end;
      finally
        sl.Free;
      end;
    end;

    FinalizeDataFile(DataHeader, FDataFile);
    FinalizeDataFile(IdxHeader, FIdxFile);
  finally
    FreeAndNil(FDataFile);
    FreeAndNil(FIdxFile);
    FreeAndNil(FAddressDataFile);

    AddressWriter.Free;
    CsvReader.Free;
    CsvFS.Free
  end;
end;

function TGeoHashBuildingsSearch.LoadFromFiles(AHashStr: string): Integer;
var
  IdxHashesBuffer: TArray<TGeoHashIdxRecord>; //array[1..cIdxHashesBufferSize] of TGeoHashIdxRecord;
  LoadCount: UInt64;
  LastLoadBytesCount: UInt64;
  i: Integer;
  j: Integer;
  BuffSize: Integer;
begin
  Result := 0;
  SetLength(IdxHashesBuffer, cIdxHashesBufferSize);
  DivMod((FIdxFile.Size - FIdxFile.Position), SizeOf(TGeoHashIdxRecord) * cIdxHashesBufferSize, LoadCount, LastLoadBytesCount);
  for i := 0 to LoadCount do
  begin
    BuffSize := IfThen(i = LoadCount, LastLoadBytesCount div SizeOf(TGeoHashIdxRecord), cIdxHashesBufferSize);
    if (FIdxFile.Read(IdxHashesBuffer[0], SizeOf(TGeoHashIdxRecord) * BuffSize) = SizeOf(TGeoHashIdxRecord) * BuffSize) then
      for j := 0 to BuffSize - 1 do
      begin
        Add(IdxHashesBuffer[j]);
        Inc(Result);
      end;
  end;
  IdxHashesBuffer := nil;
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

function TGeoHashTownsSearch.LoadFromCSV(AHashStr: string): Integer;
var
  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;
  IdLatLonPoint: TIdLatLonPoint;
  IdxHashPoint: TGeoHashIdxRecord;
  CsvFS: TFileStream;
  CsvReader: TStreamReader;
  AddresWriter: TStreamWriter;
  bHasAddress: Boolean;
  sl: TStringList;
  s: string;
begin
  Result := 0;

  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);
  InitDataFile(FAddressDataFileName, FAddressDataFile);

  AddresWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.UTF8);

  InitHeader(DataHeader, FDataFile, ftIdPoints);
  InitHeader(IdxHeader, FIdxFile, ftIdxHash);

  CsvFS := nil;
  CsvReader := nil;
  try
    if FileExists(FCsvFileName) then
    begin
      CsvFS := TFileStream.Create(FCsvFileName, fmOpenRead + fmShareDenyNone);
      CsvReader := TStreamReader.Create(CsvFS, TEncoding.UTF8, True)
    end;

    if Assigned(CsvFS) and (CsvFS.Size > 0) then
    begin
      sl := TStringList.Create;
      try
        while CsvFS.Position < CsvFS.Size do
        begin
          s := CsvReader.ReadLine;
          sl.Delimiter := ';';
          sl.StrictDelimiter := True;
          sl.DelimitedText := s;
          bHasAddress := sl.Count > 3;
          if sl.Count >= 3 then
          begin
            //1531294;59,868688;30,293949
            IdLatLonPoint :=
              TIdLatLonPoint.Create(
                sl.Strings[0],
                StrToFloatDef(sl.Strings[1], 0),
                StrToFloatDef(sl.Strings[2], 0));

            IdxHashPoint.GeoHash := TGeoHash.EncodePoint(IdLatLonPoint.p.Latitude, IdLatLonPoint.p.Longitude, cGeoHashPrecision);
            IdxHashPoint.Idx := FDataFile.Position;
            IdxHashPoint.AddresIdx := IfThen(bHasAddress, FAddressDataFile.Position , $FFFFFFFF);

            FDataFile.Write(IdLatLonPoint, SizeOf(IdLatLonPoint));
            FIdxFile.Write(IdxHashPoint, SizeOf(IdxHashPoint));
            AddresWriter.Write(sl.Strings[3] + #13#10);
            Add(IdxHashPoint);
          end;
        end;
      finally
        sl.Free;
      end;
    end;

    FinalizeDataFile(DataHeader, FDataFile);
    FinalizeDataFile(IdxHeader, FIdxFile);
  finally
    FreeAndNil(FDataFile);
    FreeAndNil(FIdxFile);
    FreeAndNil(FAddressDataFile);

    AddresWriter.Free;
    CsvReader.Free;
    CsvFS.Free
  end;
end;

function TGeoHashTownsSearch.LoadFromFiles(AHashStr: string): Integer;
var
  IdxHashesBuffer: TArray<TGeoHashIdxRecord>; //array[1..cIdxHashesBufferSize] of TGeoHashIdxRecord;
  LoadCount: UInt64;
  LastLoadBytesCount: UInt64;
  i: Integer;
  j: Integer;
  BuffSize: Integer;
begin
  Result := 0;
  SetLength(IdxHashesBuffer, cIdxHashesBufferSize);
  DivMod((FIdxFile.Size - FIdxFile.Position), SizeOf(TGeoHashIdxRecord) * cIdxHashesBufferSize, LoadCount, LastLoadBytesCount);
  for i := 0 to LoadCount do
  begin
    BuffSize := IfThen(i = LoadCount, LastLoadBytesCount div SizeOf(TGeoHashIdxRecord), cIdxHashesBufferSize);
    if (FIdxFile.Read(IdxHashesBuffer[0], SizeOf(TGeoHashIdxRecord) * BuffSize) = SizeOf(TGeoHashIdxRecord) * BuffSize) then
      for j := 0 to BuffSize - 1 do
      begin
        Add(IdxHashesBuffer[j]);
        Inc(Result);
      end;
  end;
  IdxHashesBuffer := nil;
end;


{ TGeoHashRoutesSearch }

function TGeoHashRoutesSearch.FindNearestRouteInRadius(
  const aLatitude, aLongitude: Double;
  const aRouteRadius, aSpeedRadius, aAzimuth: Double;
  var aIdx: TIDLines; var aIdxAround: TArray<TIDLines>;
  var aCurDistance, aCurAzimuth : Double;
  const aLog: TStringList): Boolean;

var
  Points: TGeoHashIdxArray;
  i: Integer;
  IdxGeoHashLines: TIdxGeoHashLines;
  pLeft, pCenter, pRight: TRouteBinData;
  dCenter: Double;
  P1 : TGeoPos;
  aLinesSort : TLinesSort;
  aLinesArray : TLinesSortArray;
  Ticks : Cardinal;
  HashStr: string;
begin
  Result := False;
  aIdx.OSM_ID := -1;
  aIdxAround := nil;

  SetLength(Points,0);
  pLeft.RouteID.OSM_ID := 0;
  pCenter.RouteID.OSM_ID := 0;
  pRight.RouteID.OSM_ID := 0;
  aCurDistance := -1;
  aCurAzimuth := -1;
  SetLength(aLinesArray,0);
  FindNearestHashesInRadius(aLatitude, aLongitude, aRouteRadius, Points, aLog);
//  RecordCount := RecordCount + Length(Points);
  for i := Low(Points) to High(Points) do
  begin
    HashStr := TGeoHash.EncodePointString(aLatitude, aLongitude, 3);
    SetDataFileName(ExtractFilePath(GetModuleName(0)) + 'GeoData\Routes', HashStr);
    dCenter := Infinity;
    if not Assigned(FDataFile) then
      DataFileOpen(FDataFileName, False, FDataFile);

    if not Assigned(FIdxFile) then
      DataFileOpen(FIdxFileName, False, FIdxFile);

//    FIdxFile.Position := Points[i].Idx;
//----TUT v 12-15 raz
      Ticks := GetTickCount;
    IdxGeoHashLines := FIdxGeoHashLines[Points[i].Idx div SizeOf(IdxGeoHashLines)];
//    if FIdxFile.Read(IdxGeoHashLines, SizeOf(IdxGeoHashLines)) = SizeOf(IdxGeoHashLines) then
    begin
      RecordCount := RecordCount + GetTickCount - Ticks;
//      if IdxGeoHashLines.PointsPos.Center < 0 then
//        Continue;
      FCSData.Enter;
      try
//        FDataFile.Position := IdxGeoHashLines.PointsPos.Center;
        FDataFile.Position := IdxGeoHashLines.PointsPos.Center * SizeOf(TRouteBinData) + SizeOf(TGeoHashSearchFileHeader);
        Ticks := GetTickCount;
//!!      FDataFile.Read(pCenter, SizeOf(TIdLatLonPoint));
//----TUT
        FDataFile.Read(pCenter, SizeOf(TRouteBinData));
      finally
        FCSData.Leave
      end;
      RecordCount := RecordCount + GetTickCount - Ticks;
//!!      dCenter := GeoLength(aLatitude, aLongitude, pCenter.p.Latitude, pCenter.p.Longitude);
//      dCenter := GeoLength(aLatitude, aLongitude, pCenter.Point.Latitude, pCenter.Point.Longitude);

      if Assigned(aLog) then
        aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f)',
        [pCenter.RouteID.OSM_ID, aLatitude, aLongitude, dCenter, pCenter.Point.Latitude, pCenter.Point.Longitude]));

      if IdxGeoHashLines.PointsPos.Left >= 0 then
      begin
        FCSData.Enter;
        try
          FDataFile.Position := IdxGeoHashLines.PointsPos.Left * SizeOf(TRouteBinData) + SizeOf(TGeoHashSearchFileHeader);
//          FDataFile.Position := IdxGeoHashLines.PointsPos.Left;
//        FDataFile.Read(pLeft, SizeOf(TIdLatLonPoint));
//     v 1,5 raza
          Ticks := GetTickCount;
          FDataFile.Read(pLeft, SizeOf(TRouteBinData));
        finally
          FCSData.Leave;
        end;
        RecordCount := RecordCount + GetTickCount - Ticks;
//!!        dLeft := GeoDistancePointToLine(aLatitude, aLongitude, pLeft.Point.Latitude, pLeft.Point.Longitude, pCenter.Point.Latitude, pCenter.Point.Longitude);
        P1.Latitude := aLatitude;
        P1.Longitude := aLongitude;

        MinDistToLineWithAzimuth(P1, pLeft.Point, pCenter.Point, aAzimuth,
                                 FAzimuthPrice, FDistancePrice,
                                 aLinesSort.DistToLine,aLinesSort.AzimuthAngle,aLinesSort.Estimation);
        if aLinesSort.DistToLine <= aSpeedRadius then
        begin
          aLinesSort.IdxDataPos := IdxGeoHashLines.PointsPos.Left;
          aLinesSort.ID := pLeft.RouteID;
          SetLength(aLinesArray,Length(aLinesArray)+1);
          aLinesArray[Length(aLinesArray)-1] := aLinesSort;
        end;
{        if MinDistToLine(P1, pLeft.Point, pCenter.Point, dLeft) then
        begin
          if Assigned(aLog) then
            aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f):(%8.10f, %8.10f)',
            [pCenter.RouteID.OSM_ID, aLatitude, aLongitude, dLeft, pLeft.Point.Latitude, pLeft.Point.Longitude, pCenter.Point.Latitude, pCenter.Point.Longitude]));

          if dLeft < aSpeedRadius then
          begin
            dAzimuth := GetAzimuth(pLeft.Point,pCenter.Point);
            MinAngle := Min(Abs(360-(dAzimuth-aAzimuth)),Abs(360-(aAzimuth-dAzimuth)));
            MinAngle := Min(Abs(dAzimuth-aAzimuth),MinAngle);
            aCurDistance := dLeft;
            aCurAzimuth := dAzimuth;
            if MinAngle <= cAzimuthDeviation then
            begin
              aIdx := pCenter.RouteID;
              Result := True;
              if Assigned(aLog) then
                aLog.Add(Format('Found new closest %d: %8.10f',
                               [pCenter.RouteID.OSM_ID, aCurDistance]));
              if not Found(aIdxAround, pCenter.RouteID) then
              begin
                Idx := Length(aIdxAround);
                SetLength(aIdxAround, Idx + 1);
                aIdxAround[Idx] := pCenter.RouteID;
              end;
              Exit;
            end;
          end;
        end;}
      end;

      if IdxGeoHashLines.PointsPos.Right >= 0 then
      begin
        FCSData.Enter;
        try
          FDataFile.Position := IdxGeoHashLines.PointsPos.Right * SizeOf(TRouteBinData) + SizeOf(TGeoHashSearchFileHeader);
//          FDataFile.Position := IdxGeoHashLines.PointsPos.Right;
//        FDataFile.Read(pLeft, SizeOf(TIdLatLonPoint));
          Ticks := GetTickCount;
        //TUT v 3-5 raz
          FDataFile.Read(pRight, SizeOf(TRouteBinData));
        finally
          FCSData.Leave;
        end;
        RecordCount := RecordCount + GetTickCount - Ticks;
//!!        dLeft := GeoDistancePointToLine(aLatitude, aLongitude, pLeft.Point.Latitude, pLeft.Point.Longitude, pCenter.Point.Latitude, pCenter.Point.Longitude);
        P1.Latitude := aLatitude;
        P1.Longitude := aLongitude;

        MinDistToLineWithAzimuth(P1, pCenter.Point, pRight.Point, aAzimuth,
//!!        MinDistToLineWithAzimuth(P1, pRight.Point, pCenter.Point, aAzimuth,
        FAzimuthPrice, FDistancePrice,
        aLinesSort.DistToLine,aLinesSort.AzimuthAngle,aLinesSort.Estimation);
        if aLinesSort.DistToLine <= aSpeedRadius then
        begin
          aLinesSort.IdxDataPos := IdxGeoHashLines.PointsPos.Right;
          aLinesSort.ID := pRight.RouteID;
          SetLength(aLinesArray,Length(aLinesArray)+1);
          aLinesArray[Length(aLinesArray)-1] := aLinesSort;
        end;
{        if MinDistToLine(P1, pCenter.Point, pRight.Point, dRight) then
        begin
          if Assigned(aLog) then
            aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f):(%8.10f, %8.10f)',
            [pCenter.RouteID.OSM_ID, aLatitude, aLongitude, dRight, pRight.Point.Latitude, pRight.Point.Longitude, pCenter.Point.Latitude, pCenter.Point.Longitude]));

          if dRight < aSpeedRadius then
          begin
            dAzimuth := GetAzimuth(pCenter.Point,pRight.Point);
            MinAngle := Min(Abs(360-(dAzimuth-aAzimuth)),Abs(360-(aAzimuth-dAzimuth)));
            MinAngle := Min(Abs(dAzimuth-aAzimuth),MinAngle);
            aCurDistance := dRight;
            aCurAzimuth := dAzimuth;
            if MinAngle <= cAzimuthDeviation then
            begin
              aIdx := pCenter.RouteID;
              Result := True;
              if Assigned(aLog) then
                aLog.Add(Format('Found new closest %d: %8.10f',
                               [pCenter.RouteID.OSM_ID, aCurDistance]));
              if not Found(aIdxAround, pCenter.RouteID) then
              begin
                Idx := Length(aIdxAround);
                SetLength(aIdxAround, Idx + 1);
                aIdxAround[Idx] := pCenter.RouteID;
              end;
              Exit;
            end;
          end;
        end;}
      end;

{      if IdxGeoHashLines.PointsPos.Right >= 0 then
      begin
        FDataFile.Position := IdxGeoHashLines.PointsPos.Right;
        FDataFile.Read(pRight, SizeOf(TRouteBinData));
        dRight := GeoDistancePointToLine(aLatitude, aLongitude, pRight.Point.Latitude, pRight.Point.Longitude, pCenter.Point.Latitude, pCenter.Point.Longitude);
        if Assigned(aLog) then
          aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f):(%8.10f, %8.10f)',
          [pCenter.RouteID.OSM_ID, aLatitude, aLongitude, dRight, pRight.Point.Latitude, pRight.Point.Longitude, pCenter.Point.Latitude, pCenter.Point.Longitude]));
      end;

      Distance := Min(Min(dCenter, dLeft), dRight);

      if Distance < MinDistance then
      begin
        MinDistance := Distance;
        aIdx := pCenter.RouteID;
        Result := True;
        if Assigned(aLog) then
          aLog.Add(Format('Found new closest %d: %8.10f',
                         [pCenter.RouteID.OSM_ID, Distance]));

//        if MinDistance = 0 then
//          Break;
      end;

//!!      if GeoLengthDeg(pCenter.p.Latitude, pCenter.p.Longitude , aLatitude, aLongitude) * 1000 <= aSpeedRadius then
      P1.Latitude := aLatitude;
      P1.Longitude := aLongitude;
      P2.Latitude := pCenter.Point.Latitude;
      P2.Longitude := pCenter.Point.Longitude;
      if GeoLength(P1 , P2) <= aSpeedRadius then
        if not Found(aIdxAround, pCenter.RouteID) then
        begin
          Idx := Length(aIdxAround);
          SetLength(aIdxAround, Idx + 1);
          aIdxAround[Idx] := pCenter.RouteID;
        end;
 }
      if Assigned(aLog) then
        aLog.Add('-----------------------------');
    end;// if FIdxFile.Read(IdxGeoHashLines,
  end; // while
//        Ticks := GetTickCount;
  if Length(aLinesArray) > 0 then
  begin
    QuickSortEstimation(aLinesArray, Low(aLinesArray), High(aLinesArray), 0);
    i := 0;
    while (i<Length(aLinesArray)) and ((aLinesArray[i].DistToLine > aSpeedRadius)
      or (aLinesArray[i].AzimuthAngle > FAzimuthDeviation)) do
      inc(i);
    if i < Length(aLinesArray) then
    begin
      Result := True;
      aIdx := aLinesArray[i].ID;
      aCurDistance := aLinesArray[i].DistToLine;
      aCurAzimuth := aLinesArray[i].AzimuthAngle;
    end else
    begin
      Result := False;
      aCurDistance := aLinesArray[0].DistToLine;
      aCurAzimuth := aLinesArray[0].AzimuthAngle;
    end;
  end;
//        RecordCount := RecordCount + GetTickCount - Ticks;
end;

function TGeoHashRoutesSearch.FindRouteNearestToPoint(const aLatitude,
  aLongitude, aRouteRadius: Double;
  var aIdxAround: TArray<TIDLinesAzimDist>; const ReturnCount: Integer;
  const aLog: TStringList;
  const ToRightOnly: Boolean): Boolean;

  function AlreadyHasRoute(const aArray: TArray<TIDLinesAzimDist>; const aID: TIDLines) : Boolean;
  var
    i: Integer;
  begin
    Result := False;
    i := 0;
    while i < Length(aArray) do
    begin
      if (aArray[i].ID.OSM_ID = aID.OSM_ID) and
         (aArray[i].ID.HashStart = aID.HashStart) and
         (aArray[i].ID.HashEnd = aID.HashEnd) then
        Exit(True);
      Inc(i);
    end;

  end;
var
  Points: TGeoHashIdxArray;
  i, RCount: Integer;
  IdxGeoHashLines: TIdxGeoHashLines;
  pLeft, pCenter, pRight: TRouteBinData;
  dCenter: Double;
  P1 : TGeoPos;
  aLinesSort : TLinesSort;
  aLinesArray : TLinesSortArray;
//  EqualPoints : Boolean;
  CenterPos, LeftPos, RightPos : Int64;
begin
  Result := False;
  aIdxAround := nil;

  SetLength(Points,0);
  pLeft.RouteID.OSM_ID := 0;
  pCenter.RouteID.OSM_ID := 0;
  pRight.RouteID.OSM_ID := 0;
  SetLength(aLinesArray,0);
  FindNearestHashesInRadius(aLatitude, aLongitude, aRouteRadius, Points, aLog);
  for i := Low(Points) to High(Points) do
  begin
    dCenter := Infinity;
    if not Assigned(FDataFile) then
      DataFileOpen(FDataFileName, False, FDataFile);

    if not Assigned(FIdxFile) then
      DataFileOpen(FIdxFileName, False, FIdxFile);

//      Ticks := GetTickCount;
//    P1.Latitude := aLatitude;
//    P1.Longitude := aLongitude;
//    TGeoHash.DecodePoint(Points[i].GeoHash, P2, cGeoHashPrecision);
//    Distance := TGeoCalcs.GeoLengthDeg(P1,P2);
//    TGeoHash.DecodePointBin(Points[i].GeoHash, P1.Latitude, P1.Longitude);

    IdxGeoHashLines := FIdxGeoHashLines[Points[i].Idx div SizeOf(IdxGeoHashLines)];
    CenterPos:= Int64(IdxGeoHashLines.PointsPos.Center) * SizeOf(TRouteBinData) + SizeOf(TGeoHashSearchFileHeader);
    LeftPos := Int64(IdxGeoHashLines.PointsPos.Left) * SizeOf(TRouteBinData) + SizeOf(TGeoHashSearchFileHeader);
    RightPos := Int64(IdxGeoHashLines.PointsPos.Right) * SizeOf(TRouteBinData) + SizeOf(TGeoHashSearchFileHeader);
    begin
//      RecordCount := RecordCount + GetTickCount - Ticks;
      FCSData.Enter;
      try
//        FDataFile.Position := IdxGeoHashLines.PointsPos.Center;
        FDataFile.Position := CenterPos;
//        Ticks := GetTickCount;
        FDataFile.Read(pCenter, SizeOf(TRouteBinData));
      finally
        FCSData.Leave;
      end;
//      RecordCount := RecordCount + GetTickCount - Ticks;

      if Assigned(aLog) then
        aLog.Add(Format('                  %d: (%8.10f, %8.10f) <- %8.10f -> (%8.10f, %8.10f)',
        [pCenter.RouteID.OSM_ID, aLatitude, aLongitude, dCenter, pCenter.Point.Latitude, pCenter.Point.Longitude]));

      if IdxGeoHashLines.PointsPos.Left >= 0 then
      begin
        FCSData.Enter;
        try
          FDataFile.Position := LeftPos;
 //         FDataFile.Position := IdxGeoHashLines.PointsPos.Left;
//          Ticks := GetTickCount;
          FDataFile.Read(pLeft, SizeOf(TRouteBinData));
//          EqualPoints := pLeft.Point = pCenter.Point;
        finally
          FCSData.Leave;
        end;
//        RecordCount := RecordCount + GetTickCount - Ticks;
        P1.Latitude := aLatitude;
        P1.Longitude := aLongitude;
        MinDistToLineWithSide(P1, pLeft.Point, pCenter.Point,
                              aLinesSort.DistToLine,aLinesSort.AzimuthAngle,aLinesSort.Estimation);

        if aLinesSort.DistToLine <= aRouteRadius then
        begin
          aLinesSort.IdxDataPos := IdxGeoHashLines.PointsPos.Left;
          aLinesSort.ID := pLeft.RouteID;
          SetLength(aLinesArray,Length(aLinesArray)+1);
          aLinesArray[Length(aLinesArray)-1] := aLinesSort;
        end;
      end;

      if IdxGeoHashLines.PointsPos.Right >= 0 then
      begin
        FCSData.Enter;
        try
          FDataFile.Position := RightPos;
//          FDataFile.Position := IdxGeoHashLines.PointsPos.Right;
//          Ticks := GetTickCount;
          FDataFile.Read(pRight, SizeOf(TRouteBinData));
//          EqualPoints := pRight.Point = pCenter.Point;
        finally
          FCSData.Leave;
        end;

        P1.Latitude := aLatitude;
        P1.Longitude := aLongitude;

        MinDistToLineWithSide(P1, pCenter.Point, pRight.Point,
                              aLinesSort.DistToLine,aLinesSort.AzimuthAngle,aLinesSort.Estimation);

        if aLinesSort.DistToLine <= aRouteRadius then
        begin
          aLinesSort.IdxDataPos := IdxGeoHashLines.PointsPos.Right;
          aLinesSort.ID := pRight.RouteID;
          SetLength(aLinesArray,Length(aLinesArray)+1);
          aLinesArray[Length(aLinesArray)-1] := aLinesSort;
        end;
      end;

      if Assigned(aLog) then
        aLog.Add('-----------------------------');
    end;
  end;

  if Length(aLinesArray) > 0 then
  begin
    QuickSortDist(aLinesArray, Low(aLinesArray), High(aLinesArray), 0);
    if ToRightOnly then
    begin
      SetLength(aIdxAround,1);
      i := 0;
      // ���� ������ -1 � ����������� ���������� �� ������
      while (i < Length(aLinesArray)) and (aLinesArray[i].Estimation >= 0) do
        inc(i);

      if i = Length(aLinesArray) then
        Exit(False);

      aIdxAround[0].IdxDataPos := aLinesArray[i].IdxDataPos;
      aIdxAround[0].ID := aLinesArray[i].ID;
      aIdxAround[0].Azimuth := aLinesArray[i].AzimuthAngle;
      aIdxAround[0].Distance := aLinesArray[i].DistToLine;
    end else
    begin
      i := 0;
      RCount := 0;
      while (RCount < ReturnCount) and (i < Length(aLinesArray)) do
      begin
        if (i = 0) or not AlreadyHasRoute(aIdxAround, aLinesArray[i].ID) then
        begin
          SetLength(aIdxAround, Length(aIdxAround) + 1);
          aIdxAround[RCount].ID := aLinesArray[i].ID;
          aIdxAround[RCount].IdxDataPos := aLinesArray[i].IdxDataPos;
          aIdxAround[RCount].Azimuth := aLinesArray[i].AzimuthAngle;
          aIdxAround[RCount].Distance := aLinesArray[i].DistToLine;
          inc(RCount);
        end;
        inc(i);
      end;
    end;
    Result := True;
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

function TGeoHashRoutesSearch.LoadFromCSV(AHashStr: string): Integer;
const
  cLoadDiv = 100;
var
  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;

//  Id: Integer;
  PointsCount: Integer;
  PointNo: Integer;
  Latitude: Double;
  Longitude: Double;

//  LinePoint: TIntIdLatLonPoint;
//  IdxLine : TIDLines;
  RouteBinData : TRouteBinData;

  PosRight,
  PosCenter,
  PosLeft: Integer;

  HashCenter,
  HashRight : TGeoHashValue;

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

    SetLength(FIdxGeoHashLines,Length(FIdxGeoHashLines)+1);
    FIdxGeoHashLines[Length(FIdxGeoHashLines)-1] := IdxGeoHashLines;

    IdxGeoHash.GeoHash := aHashCenter;
    IdxGeoHash.Idx := FIdxFile.Position;
    IdxGeoHash.AddresIdx := 0;

    FIdxFile.Write(IdxGeoHashLines, SizeOf(TIdxGeoHashLines));

    Add(IdxGeoHash);
  end;

{  procedure ProcessData;
  begin
    PosLeft := PosCenter;
    PosCenter := PosRight;
    PosRight := FDataFile.Position;

    LinePoint.ID := Id;
    LinePoint.p.Latitude := Latitude;
    LinePoint.p.Longitude := Longitude;
    FDataFile.Write(LinePoint, SizeOf(TIdLatLonPoint));

    HashCenter := HashRight;
    HashRight := EncodePoint(Latitude, Longitude, cGeoHashPrecision);

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
}
  procedure ProcessData;
  begin
    PosLeft := PosCenter;
    PosCenter := PosRight;
    PosRight := FDataFile.Position;

{    IdxLine.OSM_ID := StrToInt64(sl.Strings[1]);
    IdxLine.HashStart := StrToInt64(sl.Strings[2]);
    IdxLine.HashEnd := StrToInt64(sl.Strings[3]);
    FDataFile.Write(IdxLine, SizeOf(TIDLines));}

    HashCenter := HashRight;
    HashRight := TGeoHash.EncodePoint(Latitude, Longitude, cGeoHashPrecision);

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

  procedure SaveMessageToTextFile(aMessage : string);
  var
    F : TextFile;
    fs : TFormatSettings;
  begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
    try
    AssignFile(F,ExtractFilePath(GetModuleName(0)) + 'GeoData\PointLog.txt');
    if FileExists(ExtractFilePath(GetModuleName(0)) + 'GeoData\PointLog.txt') then
      Append(F)
    else
      Rewrite(f);
//    Writeln(F,FloatToStr(La,fs)+' '+FloatToStr(Lo,fs));
    Writeln(F,aMessage);
    finally
    CloseFile(F);
    end;
  end;
var
  CsvFS: TFileStream;
  CsvReader: TStreamReader;
  sl: TStringList;
  s: string;
  AddresWriter: TStreamWriter;     //     ������� ���������
  aPointsArrayStart, aPointsArray : TGeoPosArray;
//  IdxLine : TIDLines;
  //GeoLine : TGeoLineRecord;
  Distance, LatitudeInc, LongitudeInc : Double;
  i,j,k, NeedSection : Integer;
  CurRouteID : TIDLines;
begin
  Result := 0;
  CurRouteID.OSM_ID := 0;
  CurRouteID.HashStart := 0;
  CurRouteID.HashEnd := 0;
  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);
  InitDataFile(FAddressDataFileName, FAddressDataFile);

  AddresWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.UTF8);

  InitHeader(DataHeader, FDataFile, ftIdPoints);
  InitHeader(IdxHeader, FIdxFile, ftIdxHashPoints);

  CsvFS := nil;
  CsvReader := nil;
  try
    if FileExists(FCsvFileName) then
    begin
      CsvFS := TFileStream.Create(FCsvFileName, fmOpenRead + fmShareDenyNone);
      CsvReader := TStreamReader.Create(CsvFS, TEncoding.UTF8, True)
    end;

    if Assigned(CsvFS) and (CsvFS.Size > 0) then
    begin
      sl := TStringList.Create;
//      SetLength(FIdxGeoHashLines,0);
      try
        ResetPointsFlow;
        while CsvFS.Position < CsvFS.Size do
        begin
          s := CsvReader.ReadLine;
          sl.Delimiter := #9;//';';
          sl.QuoteChar := '^';
          sl.StrictDelimiter := True;
          sl.DelimitedText := s;
          if sl.Count > 3 then
          begin
            SetLength(aPointsArray,1);
            SetLength(aPointsArrayStart,0);
            TGeoHash.DecodeArrayWorld(sl.Strings[4], aPointsArrayStart);

            i := 1;
            j := 1;
            aPointsArray[0] := aPointsArrayStart[0];
            while I < Length(aPointsArrayStart)  do
            begin
              Distance := TGeoCalcs.GeoLengthDeg(aPointsArrayStart[i-1], aPointsArrayStart[i]);
              if Distance > CMaxRouteDistance then
              begin
                NeedSection :=  trunc(Distance/CMaxRouteDistance)+1;
                LatitudeInc := (aPointsArrayStart[i].Latitude-aPointsArrayStart[i-1].Latitude)/NeedSection;
                LongitudeInc := (aPointsArrayStart[i].Longitude-aPointsArrayStart[i-1].Longitude)/NeedSection;
                for k := 1 to NeedSection-1 do
                begin
                  SetLength(aPointsArray, Length(aPointsArray)+1);
                  aPointsArray[j].Latitude := aPointsArrayStart[i-1].Latitude + LatitudeInc*k;
                  aPointsArray[j].Longitude := aPointsArrayStart[i-1].Longitude + LongitudeInc*k;
                  inc(j);
                end;
                SetLength(aPointsArray, Length(aPointsArray)+1);
                aPointsArray[j] := aPointsArrayStart[i];
                inc(j);
              end else
              begin
                SetLength(aPointsArray, Length(aPointsArray)+1);
                aPointsArray[j] := aPointsArrayStart[i];
                inc(j);
              end;
              inc(i);
            end;

            i := 0;
            try
              while I < Length(aPointsArray)  do
              begin
                PosLeft := PosCenter;
                PosCenter := PosRight;
//                PosRight := FDataFile.Position;
                PosRight := (FDataFile.Position - SizeOf(TGeoHashSearchFileHeader)) div SizeOf(TRouteBinData);

                RouteBinData.RouteID.OSM_ID := StrToInt64(sl.Strings[1]);
                RouteBinData.RouteID.HashStart := StrToInt64(sl.Strings[2]);
                RouteBinData.RouteID.HashEnd := StrToInt64(sl.Strings[3]);
                RouteBinData.Point.Latitude := aPointsArray[i].Latitude;
                RouteBinData.Point.Longitude := aPointsArray[i].Longitude;
                RouteBinData.AddresIdx := FAddressDataFile.Position;
                if (RouteBinData.RouteID.HashStart<0) or
                   (RouteBinData.RouteID.HashEnd<0) then
                   SaveMessageToTextFile(Format('osm_id=%g, start=%g, end=%g',
                       [RouteBinData.RouteID.OSM_ID,RouteBinData.RouteID.HashStart,RouteBinData.RouteID.HashEnd]));
  {              if not ((RouteBinData.RouteID.OSM_ID = CurRouteID.OSM_ID) and
                       (RouteBinData.RouteID.HashStart = CurRouteID.HashStart) and
                       (RouteBinData.RouteID.HashEnd = CurRouteID.HashEnd)) then
                begin
                  CurRouteID := RouteBinData.RouteID;
                  FDataFile.Write(RouteBinData, SizeOf(TRouteBinData));
                end;}
                FDataFile.Write(RouteBinData, SizeOf(TRouteBinData));

                HashCenter := HashRight;
                HashRight := TGeoHash.EncodePoint(aPointsArray[i].Latitude, aPointsArray[i].Longitude, cGeoHashPrecision);

                if i > 0 then
                begin
                  SaveAndAddPoint(PosLeft, PosCenter, PosRight, HashCenter);
                  if i = Length(aPointsArray)-1 then
                  begin
                    SaveAndAddPoint(PosCenter, PosRight, -1, HashRight);
                    ResetPointsFlow;
                  end;
                end;
                inc(i);
              end;
            except
//                on E : Exception do
//                  ToLog('i=' +IntToStr(i) + sl.ToString + ' ' + E.Message);
            end;
//TODO: ^
// ^ �� ����� ��� �������� ����, ������ ��� � ���� ������ ��� �� �����
// ^ � � try - except ������������ ����
            Result := Result + i;
            //end;

//            if Id >= 0 then
//              ProcessData;
          end;
          AddresWriter.Write(sl.Strings[0] + #13#10);
        end;
      finally
        sl.Free;
      end;
    end;

    if not Assigned(FLoadedDict) then
      FLoadedDict := TDictionary<string,Integer>.Create;
    FLoadedDict.AddOrSetValue(AHashStr, Result);
    FinalizeDataFile(DataHeader, FDataFile);
    FinalizeDataFile(IdxHeader, FIdxFile);

    FreeAndNil(FDataFile);
    FreeAndNil(FIdxFile);

  finally
    AddresWriter.Free;
    CsvReader.Free;
    CsvFS.Free;
  end;
end;

function TGeoHashRoutesSearch.LoadFromDataFiles(AOffset: Int64): TRouteBinData;
var
  ARouteBinData : TRouteBinData;
begin
  if not Assigned(FDataFile) then
    DataFileOpen(FDataFileName, False, FDataFile);

    FCSData.Enter;
    try
      FDataFile.Position := AOffset;
      FDataFile.Read(ARouteBinData, SizeOf(TRouteBinData));
    finally
      FCSData.Leave;
    end;
    Result := ARouteBinData;
end;

function TGeoHashRoutesSearch.LoadFromFiles(AHashStr: string): Integer;
var
  IdxHashesBuffer: TArray<TIdxGeoHashLines>;
  IdxGeoHashLines: TIdxGeoHashLines;
  IdxGeoHash: TGeoHashIdxRecord;
  LoadCount: UInt64;
  LastLoadBytesCount: UInt64;
  i: Integer;
  j: Integer;
  IdxFilePos: Integer;
  BuffSize: Integer;
  aPoint: TGeoPos;
begin
  IdxHashesBuffer := nil;
  Result := 0;
  SetLength(IdxHashesBuffer, cRoutesIdxHashesBufferSize);
  DivMod((FIdxFile.Size - FIdxFile.Position), SizeOf(TIdxGeoHashLines) * cRoutesIdxHashesBufferSize, LoadCount, LastLoadBytesCount);
//  SetLength(FIdxGeoHashLines,0);
  for i := 0 to LoadCount do
  begin
    BuffSize := IfThen(i = LoadCount, LastLoadBytesCount div SizeOf(TIdxGeoHashLines), cRoutesIdxHashesBufferSize);
    IdxFilePos := FIdxFile.Position;
    if (FIdxFile.Read(IdxHashesBuffer[0], SizeOf(TIdxGeoHashLines) * BuffSize) = SizeOf(TIdxGeoHashLines) * BuffSize) then
    begin
      for j := 0 to BuffSize - 1 do
      begin
        SetLength(FIdxGeoHashLines,Length(FIdxGeoHashLines)+1);
        IdxGeoHashLines := IdxHashesBuffer[j];
        FIdxGeoHashLines[Length(FIdxGeoHashLines)-1] := IdxGeoHashLines;
        IdxGeoHash.GeoHash := IdxGeoHashLines.GeoHash;
        IdxGeoHash.Idx     := IdxFilePos + j * SizeOf(TIdxGeoHashLines);
        IdxGeoHash.AddresIdx     := 0;
        TGeoHash.DecodePoint(IdxGeoHash.GeoHash, aPoint, cGeoHashPrecision);
        Add(IdxGeoHash);
//        Inc(Result);
      end;
      Result := Result + BuffSize;
    end;
  end;
  if not Assigned(FLoadedDict) then
    FLoadedDict := TDictionary<string,Integer>.Create;
  FLoadedDict.AddOrSetValue(AHashStr, Result);

  IdxHashesBuffer := nil;

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
  ID.AsInt64Rec.Hi := 0;
  ID.AsInt64Rec.Lo := 0;
  p.Latitude := aLatitude;
  p.Longitude := aLongitude;
end;

constructor TIdLatLonPoint.Create(aID: TUniversalIDRecord; aLatitude, aLongitude: Double);
begin
  ID := aID;
  p.Latitude := aLatitude;
  p.Longitude := aLongitude;
end;

function TGeoHashDistrictsSearch.IsDataFileActual: Boolean;
begin
  Result := IsFileActual(FDataFile, ftIdPoints);
end;

function TGeoHashDistrictsSearch.IsIdxFileActual: Boolean;
begin
  Result := IsFileActual(FIdxFile, ftIdxHashPoints);
end;

function TGeoHashDistrictsSearch.LoadFromCSV(AHashStr: string): Integer;
var
  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;
  IdGUID: TUniversalIDRecord;
  IdxHashPoint: TGeoHashIdxArrRecord;
  CsvFS: TFileStream;
  CsvReader: TStreamReader;
  AddresWriter: TStreamWriter;
  bHasAddress: Boolean;
  sl: TStringList;
  s: string;
  aPointsArray : TGeoPosArray;
  I, J : integer;
  MinLat, MinLon, MaxLat, MaxLon : Double;
begin
  Result := 0;
  SetLength(BorderArray,0);
  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);
  InitDataFile(FAddressDataFileName, FAddressDataFile);

  AddresWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.UTF8);

  InitHeader(DataHeader, FDataFile, ftIdPoints);
  InitHeader(IdxHeader, FIdxFile, ftIdxHashPoints);

  CsvFS := nil;
  CsvReader := nil;
  try
    if FileExists(FCsvFileName) then
    begin
      CsvFS := TFileStream.Create(FCsvFileName, fmOpenRead + fmShareDenyNone);
      CsvReader := TStreamReader.Create(CsvFS, TEncoding.UTF8, True)
    end;

    j := 0;
    if Assigned(CsvFS) and (CsvFS.Size > 0) then
    begin
      sl := TStringList.Create;
      try
        while CsvFS.Position < CsvFS.Size do
        begin
          s := CsvReader.ReadLine;
          sl.Delimiter := ';';
          sl.StrictDelimiter := True;
          sl.DelimitedText := s;
          bHasAddress := sl.Count > 3;
          if sl.Count >= 3 then
          begin
            //1531294;59,868688;30,293949
            MinLat := 90;
            MinLon := 180;
            MaxLat := 0;
            MaxLon := 0;

            SetLength(aPointsArray,0);
            TGeoHash.DecodeArrayWorld(sl.Strings[3], aPointsArray);
            for I := 0 to Length(aPointsArray)-1 do
            begin
              if MinLat > aPointsArray[i].Latitude then
                 MinLat := aPointsArray[i].Latitude;
              if MinLon > aPointsArray[i].Longitude then
                 MinLon := aPointsArray[i].Longitude;
              if MaxLat < aPointsArray[i].Latitude then
                 MaxLat := aPointsArray[i].Latitude;
              if MaxLon < aPointsArray[i].Longitude then
                 MaxLon := aPointsArray[i].Longitude;
            end;

            IdxHashPoint.ArrayCount := Length(aPointsArray);
            IdxHashPoint.MinHash := TGeoHash.EncodePoint(MinLat,MinLon, cGeoHashPrecision);
            IdxHashPoint.MaxHash := TGeoHash.EncodePoint(MaxLat,MaxLon, cGeoHashPrecision);
            IdxHashPoint.AreaID := StrToInt(sl.Strings[1]);
            IdxHashPoint.Idx := FDataFile.Position;
            IdxHashPoint.AddresIdx := IfThen(bHasAddress, FAddressDataFile.Position , $FFFFFFFF);

            IdGUID.AsGUID := StringToGUID(sl.Strings[0]);

            FDataFile.Write(IdGUID, SizeOf(TUniversalIDRecord));
            FIdxFile.Write(IdxHashPoint, SizeOf(TGeoHashIdxArrRecord));

            for I := 0 to Length(aPointsArray)-1 do
              FIdxFile.Write(aPointsArray[i], SizeOf(TGeoPos));
            AddresWriter.Write(sl.Strings[2] + #13#10);

            SetLength(BorderArray,  Length(BorderArray)+1);
            BorderArray[j].PointsArray := aPointsArray;
            BorderArray[j].Idx := IdxHashPoint.Idx;
            BorderArray[j].AreaID := IdxHashPoint.AreaID;
            BorderArray[j].AddresIdx := IdxHashPoint.AddresIdx;
            BorderArray[j].Box.PointMin.Latitude := MinLat;
            BorderArray[j].Box.PointMin.Longitude := MinLon;
            BorderArray[j].Box.PointMax.Latitude := MaxLat;
            BorderArray[j].Box.PointMax.Longitude := MaxLon;
            inc(j);
//!!            Add(IdxHashPoint);
          end;
        end;
      finally
        sl.Free;
      end;
    end;

    FinalizeDataFile(DataHeader, FDataFile);
    FinalizeDataFile(IdxHeader, FIdxFile);
  finally
    FreeAndNil(FDataFile);
    FreeAndNil(FIdxFile);
    FreeAndNil(FAddressDataFile);

    AddresWriter.Free;
    CsvReader.Free;
    CsvFS.Free
  end;
end;

function TGeoHashDistrictsSearch.LoadFromFiles(AHashStr: string): Integer;
var
  i: Integer;
  aGeoHashIdxRecord : TGeoHashIdxArrRecord;
  aPointsArray: TGeoPosArray;
begin
  SetLength(BorderArray,0);
  i := 0;
  while FIdxFile.Position < FIdxFile.Size do
  begin
    FIdxFile.Read(aGeoHashIdxRecord, SizeOf(TGeoHashIdxArrRecord));
    SetLength(aPointsArray,aGeoHashIdxRecord.ArrayCount);
    if aGeoHashIdxRecord.ArrayCount > 0 then
      FIdxFile.Read(aPointsArray[0], SizeOf(TGeoPos)*aGeoHashIdxRecord.ArrayCount);
    SetLength(BorderArray,  Length(BorderArray)+1);
    TGeoHash.DecodePoint(aGeoHashIdxRecord.MinHash,BorderArray[i].Box.PointMin,32);
    TGeoHash.DecodePoint(aGeoHashIdxRecord.MaxHash,BorderArray[i].Box.PointMax,32);
    BorderArray[i].PointsArray := aPointsArray;
    BorderArray[i].Idx := aGeoHashIdxRecord.Idx;
    BorderArray[i].AreaID := aGeoHashIdxRecord.AreaID;
    BorderArray[i].AddresIdx := aGeoHashIdxRecord.AddresIdx;
    inc(i);
  end;
  Result := i;
end;

function TGeoHashDistrictsSearch.SearchPointInArea(const aLatitude,
  aLongitude: Double; out aIdx, aAddressIdx: UInt32;
  const aLog: TStringList): Boolean;
var
  i : Integer;
  aPoint : TGeoPos;
begin
  Result := False;
  aIdx := 0;
  aPoint.Latitude := aLatitude;
  aPoint.Longitude := aLongitude;
  aAddressIdx := 0;

  for I := 0 to Length(BorderArray) - 1 do
  begin
    if (BorderArray[i].Box.PointMin.Latitude<=aLatitude) and
       (BorderArray[i].Box.PointMin.Longitude<=aLongitude) and
       (BorderArray[i].Box.PointMax.Latitude>=aLatitude) and
       (BorderArray[i].Box.PointMax.Longitude>=aLongitude)  then
     begin
       if CheckArea(BorderArray[i].PointsArray, aPoint)
       then
       begin
         aIdx := BorderArray[i].Idx;
         aAddressIdx := BorderArray[i].AddresIdx;
         Result := True;
         if Assigned(aLog) then
          aLog.Add(Format('i=%d: point(%8.10f %8.10f)',
                         [i, aLatitude, aLongitude]));
         Exit;
       end;
     end;
  end;
end;

constructor TIdLatLonPoint.Create(sID: string; aLatitude, aLongitude: Double);
begin
  ID.AsGUID := StringToGUID(sID);
  p.Latitude := aLatitude;
  p.Longitude := aLongitude;
end;

function TGeoHashAreasSearch.IsDataFileActual: Boolean;
begin
  Result := IsFileActual(FDataFile, ftIdPoints);
end;

function TGeoHashAreasSearch.IsIdxFileActual: Boolean;
begin
  Result := IsFileActual(FIdxFile, ftIdxHashPoints);
end;

function TGeoHashAreasSearch.LoadFromCSV(AHashStr: string): Integer;
var
  DataHeader: TGeoHashSearchFileHeader;
  IdxHeader: TGeoHashSearchFileHeader;
  IdGUID: TUniversalIDRecord;
  IdxHashPoint: TGeoHashIdxArrRecord;
  CsvFS: TFileStream;
  CsvReader: TStreamReader;
  AddresWriter: TStreamWriter;
  bHasAddress: Boolean;
  sl: TStringList;
  s: string;
  aPointsArray : TGeoPosArray;
  I, J : integer;
  MinLat, MinLon, MaxLat, MaxLon : Double;
begin
  Result := 0;
  SetLength(BorderArray,0);
  InitDataFile(FDataFileName, FDataFile);
  InitDataFile(FIdxFileName, FIdxFile);
  InitDataFile(FAddressDataFileName, FAddressDataFile);

  AddresWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.UTF8);

  InitHeader(DataHeader, FDataFile, ftIdPoints);
  InitHeader(IdxHeader, FIdxFile, ftIdxHashPoints);

  CsvFS := nil;
  CsvReader := nil;
  try
    if FileExists(FCsvFileName) then
    begin
      CsvFS := TFileStream.Create(FCsvFileName, fmOpenRead + fmShareDenyNone);
      CsvReader := TStreamReader.Create(CsvFS, TEncoding.UTF8, True)
    end;

    j := 0;
    if Assigned(CsvFS) and (CsvFS.Size > 0) then
    begin
      sl := TStringList.Create;
      try
        while CsvFS.Position < CsvFS.Size do
        begin
          s := CsvReader.ReadLine;
          sl.Delimiter := ';';
          sl.StrictDelimiter := True;
          sl.DelimitedText := s;
          bHasAddress := sl.Count > 3;
          if sl.Count >= 3 then
          begin
            //1531294;59,868688;30,293949
            MinLat := 90;
            MinLon := 180;
            MaxLat := 0;
            MaxLon := 0;

            SetLength(aPointsArray,0);
            TGeoHash.DecodeArrayWorld(sl.Strings[3], aPointsArray);
            for I := 0 to Length(aPointsArray)-1 do
            begin
              if MinLat > aPointsArray[i].Latitude then
                 MinLat := aPointsArray[i].Latitude;
              if MinLon > aPointsArray[i].Longitude then
                 MinLon := aPointsArray[i].Longitude;
              if MaxLat < aPointsArray[i].Latitude then
                 MaxLat := aPointsArray[i].Latitude;
              if MaxLon < aPointsArray[i].Longitude then
                 MaxLon := aPointsArray[i].Longitude;
            end;

            IdxHashPoint.ArrayCount := Length(aPointsArray);
            IdxHashPoint.MinHash := TGeoHash.EncodePoint(MinLat,MinLon, cGeoHashPrecision);
            IdxHashPoint.MaxHash := TGeoHash.EncodePoint(MaxLat,MaxLon, cGeoHashPrecision);
            IdxHashPoint.AreaID := StrToInt(sl.Strings[1]);
            IdxHashPoint.Idx := FDataFile.Position;
            IdxHashPoint.AddresIdx := IfThen(bHasAddress, FAddressDataFile.Position , $FFFFFFFF);

            IdGUID.AsGUID := StringToGUID(sl.Strings[0]);

            FDataFile.Write(IdGUID, SizeOf(TUniversalIDRecord));
            FIdxFile.Write(IdxHashPoint, SizeOf(TGeoHashIdxArrRecord));

            for I := 0 to Length(aPointsArray)-1 do
              FIdxFile.Write(aPointsArray[i], SizeOf(TGeoPos));
            AddresWriter.Write(sl.Strings[2] + #13#10);

            SetLength(BorderArray,  Length(BorderArray)+1);
            BorderArray[j].PointsArray := aPointsArray;
            BorderArray[j].Idx := IdxHashPoint.Idx;
            BorderArray[j].AreaID := IdxHashPoint.AreaID;
            BorderArray[j].AddresIdx := IdxHashPoint.AddresIdx;
            BorderArray[j].Box.PointMin.Latitude := MinLat;
            BorderArray[j].Box.PointMin.Longitude := MinLon;
            BorderArray[j].Box.PointMax.Latitude := MaxLat;
            BorderArray[j].Box.PointMax.Longitude := MaxLon;
            inc(j);
//!!            Add(IdxHashPoint);
          end;
        end;
      finally
        sl.Free;
      end;
    end;

    FinalizeDataFile(DataHeader, FDataFile);
    FinalizeDataFile(IdxHeader, FIdxFile);
  finally
    FreeAndNil(FDataFile);
    FreeAndNil(FIdxFile);
    FreeAndNil(FAddressDataFile);

    AddresWriter.Free;
    CsvReader.Free;
    CsvFS.Free
  end;
end;

function TGeoHashAreasSearch.LoadFromFiles(AHashStr: string): Integer;
var
  i: Integer;
  aGeoHashIdxRecord : TGeoHashIdxArrRecord;
  aPointsArray: TGeoPosArray;
begin
  SetLength(BorderArray,0);
  i := 0;
  while FIdxFile.Position < FIdxFile.Size do
  begin
    FIdxFile.Read(aGeoHashIdxRecord, SizeOf(TGeoHashIdxArrRecord));
    SetLength(aPointsArray,aGeoHashIdxRecord.ArrayCount);
    if aGeoHashIdxRecord.ArrayCount > 0 then
      FIdxFile.Read(aPointsArray[0], SizeOf(TGeoPos)*aGeoHashIdxRecord.ArrayCount);
    SetLength(BorderArray,  Length(BorderArray)+1);
    TGeoHash.DecodePoint(aGeoHashIdxRecord.MinHash,BorderArray[i].Box.PointMin,32);
    TGeoHash.DecodePoint(aGeoHashIdxRecord.MaxHash,BorderArray[i].Box.PointMax,32);
    BorderArray[i].PointsArray := aPointsArray;
    BorderArray[i].Idx := aGeoHashIdxRecord.Idx;
    BorderArray[i].AreaID := aGeoHashIdxRecord.AreaID;
    BorderArray[i].AddresIdx := aGeoHashIdxRecord.AddresIdx;
    inc(i);
  end;
  Result := i;
end;

function TGeoHashAreasSearch.SearchPointInArea(const aLatitude,
  aLongitude: Double; out aIdx, aAddressIdx: UInt32;
  const aLog: TStringList): Boolean;
var
  i : Integer;
  aPoint : TGeoPos;
begin
  Result := False;
  aIdx := 0;
  aPoint.Latitude := aLatitude;
  aPoint.Longitude := aLongitude;
  aAddressIdx := 0;

  for I := 0 to Length(BorderArray) - 1 do
  begin
    if (BorderArray[i].Box.PointMin.Latitude<=aLatitude) and
       (BorderArray[i].Box.PointMin.Longitude<=aLongitude) and
       (BorderArray[i].Box.PointMax.Latitude>=aLatitude) and
       (BorderArray[i].Box.PointMax.Longitude>=aLongitude)  then
     begin
       if CheckArea(BorderArray[i].PointsArray, aPoint)
       then
       begin
         aIdx := BorderArray[i].Idx;
         aAddressIdx := BorderArray[i].AddresIdx;
         Result := True;
         if Assigned(aLog) then
          aLog.Add(Format('i=%d: point(%8.10f %8.10f)',
                         [i, aLatitude, aLongitude]));
         Exit;
       end;
     end;
  end;
end;

{ TIDLines }

function TIDLines.Equal(const AOsmID, AStart, AEnd: Int64): Boolean;
begin
  Result := (OSM_ID = AOsmID) and (HashStart = AStart) and (HashEnd = AEnd);
end;

function TIDLines.ReverseID: TIDLines;
begin
  Result.OSM_ID := OSM_ID;
  Result.HashStart := HashEnd;
  Result.HashEnd := HashStart;
end;

function TIDLines.StrToIDLines(const ARouteID: string): TIDLines;
var
  s: string;
  i: Integer;
begin
  s := ARouteID;
  Result.OSM_ID := 1;
  if s[1] = '-' then
  begin
    Result.OSM_ID := -1;
    Delete(s,1,1);
  end;
  i := Pos('-',s);
  if i < 1 then Exit;
  Result.OSM_ID := Result.OSM_ID*StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);

  i := Pos('-',s);
  if i < 1 then Exit;
  Result.HashStart := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);

  Result.HashEnd := StrToInt64(s);
end;

function TIDLines.StrToIDLinesReverse(const ARouteID: string): TIDLines;
var
  s: string;
  i: Integer;
begin
  s := ARouteID;
  Result.OSM_ID := 1;
  if s[1] = '-' then
  begin
    Result.OSM_ID := -1;
    Delete(s,1,1);
  end;
  i := Pos('-',s);
  if i < 1 then Exit;
  Result.OSM_ID := Result.OSM_ID*StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);

  i := Pos('-',s);
  if i < 1 then Exit;
  Result.HashEnd := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);

  Result.HashStart := StrToInt64(s);
end;

function TIDLines.ToString: string;
begin
  Result := IntToStr(OSM_ID)+'-'+IntToStr(HashStart)+'-'+IntToStr(HashEnd);
end;

end.


