unit AStar64.Typ;

interface

uses
  SysUtils,
  Geo.Hash;

const
  RT_motorway       = 11;
  RT_motorway_link  = 12;
  RT_trunk          = 13;
  RT_trunk_link     = 14;
  RT_primary        = 15;
  RT_primary_link   = 16;
  RT_secondary      = 21;
  RT_secondary_link = 22;
  RT_tertiary       = 31;
  RT_tertiary_link  = 32;
  RT_residential    = 41;
  RT_road           = 42;
  RT_unclassified   = 43;
  RT_service        = 51;
  RT_living_street  = 63;
  RT_pedestrian     = 62;

  RT_BackwardDef = RT_secondary;

  RB_motorway       = 1 shl 0;
  RB_motorway_link  = 1 shl 1;
  RB_trunk          = 1 shl 2;
  RB_trunk_link     = 1 shl 3;
  RB_primary        = 1 shl 4;
  RB_primary_link   = 1 shl 5;
  RB_secondary      = 1 shl 6;
  RB_secondary_link = 1 shl 7;
  RB_tertiary       = 1 shl 8;
  RB_tertiary_link  = 1 shl 9;
  RB_residential    = 1 shl 10;
  RB_road           = 1 shl 11;
  RB_unclassified   = 1 shl 12;
  RB_service        = 1 shl 13;
  RB_living_street  = 1 shl 14;
  RB_pedestrian     = 1 shl 15;
  RB__AllMask = (RB_pedestrian shl 1) - 1;

  RS_motorway       = 110;
  RS_motorway_link  = 50;
  RS_trunk          = 90;
  RS_trunk_link     = 50;
  RS_primary        = 70;
  RS_primary_link   = 50;
  RS_secondary      = 60;
  RS_secondary_link = 50;
  RS_tertiary       = 60;
  RS_tertiary_link  = 50;
  RS_residential    = 20;

  RS_road           = 60;
  RS_unclassified   = 60;

  RS_living_street  = 10; // было 20
  RS_service        = 0;  // было 20; обнаружилось, что это спец дороги - ездить по ним нельзя
  RS_pedestrian     = 0;  // нельзя ездить по пешеходным дорогам

  RS_default        = 60;

  RS_reverse        = 60;

type
  TRoadLengthByZoneArray = array[0..63] of Double;  // 512

  PRoadLengthByTypeRecord = ^TRoadLengthByTypeRecord;
  TRoadLengthByTypeRecord = packed record
    Zones         : UInt64; // 8
    Motorway      : Double; // 16
    MotorwayLink  : Double;
    Trunk         : Double; // 32
    TrunkLink     : Double;
    Primary       : Double;
    PrimaryLink   : Double;
    Secondary     : Double; // 64
    SecondaryLink : Double;
    Tertiary      : Double;
    TertiaryLink  : Double;
    Residential   : Double; // 96
    Road          : Double;
    Unclassified  : Double;
    Service       : Double;
    LivingStreet  : Double; // 128
    Reserved      : Double; // 136
    procedure Init; inline;
    procedure AddRoadLen(ARoadType: Integer; ADistance: Double);
    constructor Create(AZone: UInt64);
  end;
  TRoadLengthByTypeRecordArray = TArray<TRoadLengthByTypeRecord>;

  PRoadLengthAggregateRecord = ^TRoadLengthAggregateRecord;
  TRoadLengthAggregateRecord = packed record
    Zones: UInt64;
    RoadType: UInt32;
    RoadLength: Double;
  end;
  TRoadLengthAggregateRecordArray = TArray<TRoadLengthAggregateRecord>;

  PStatRecord = ^TStatRecord;
  TStatRecord = packed record
    OrdinaryLen: Double;
    ThinGraphLen: Double;
  end;

  PRoadSpeedsRecord = ^TRoadSpeedsRecord;
  TRoadSpeedsRecord = packed record
    Motorway      : UInt8;  // 1
    MotorwayLink  : UInt8;
    Trunk         : UInt8;
    TrunkLink     : UInt8;  // 4
    Primary       : UInt8;
    PrimaryLink   : UInt8;
    Secondary     : UInt8;
    SecondaryLink : UInt8;  // 8
    Tertiary      : UInt8;
    TertiaryLink  : UInt8;
    Residential   : UInt8;
    Road          : UInt8;  // 12
    Unclassified  : UInt8;
    Service       : UInt8;
    LivingStreet  : UInt8;
//TODO:    Pedestrian    : Uint8;
    reverse       : UInt8;  // 16
    function GetSpeedByRoadType(const ARoadType: Integer): Integer;
    function GetDuration(const ARoadLength: TRoadLengthByTypeRecord): Double; overload;
    function GetDuration(const ARoadLength: TRoadLengthAggregateRecord): Double; overload;
  end;

  PAstarRequest = ^TAstarRequest;
  TAstarRequest = packed record
    Version: UInt64;
    FromLatitude: Double;
    FromLongitude: Double;
    ToLatitude: Double;
    ToLongitude: Double;
    ZonesLimit: UInt64;
    RoadSpeedsRecord: TRoadSpeedsRecord;
    FormatVariant: Integer;
    LenTreshold: Double;
    Timeout: Integer;
    Distance: Double;
    Duration: Double;
    BufferSize: Integer;
    HashString: PAnsiChar;
    function ToString: string;
  end;

  PAstarRequest3 = ^TAstarRequest3;
  TAstarRequest3 = packed record
    Version: UInt64;
    FromLatitude: Double;
    FromLongitude: Double;
    ToLatitude: Double;
    ToLongitude: Double;
    ZonesLimit: UInt64;
    RoadTypeLimit: UInt64;
    OsmTypeLimit: UInt64;
    FeatureLimit: UInt64;
    FormatVariant: Integer;
    LenTreshold: Double;
    Timeout: Integer;
    Distance: Double;
    Stat: TStatRecord;
    RoadLengthBufferSize: Integer;
    RoadLengthByZoneByType: PRoadLengthByTypeRecord;
    RoadLengthCount: Integer;
    BufferSize: Integer;
    HashString: PAnsiChar;
    SignsLimit: UInt64;
    function FormatStartPos: Integer;
    function FormatIncrement: Integer;
    function ToString: string;
  end;

  PAstarRequest4 = ^TAstarRequest4;
  TAstarRequest4 = packed record
    Version: UInt64;
    FromLatitude: Double;
    FromLongitude: Double;
    ToLatitude: Double;
    ToLongitude: Double;
    ZonesLimit: UInt64;
    RoadTypeLimit: UInt64;
    OsmTypeLimit: UInt64;
    Feature: UInt64;
    FormatVariant: Integer;
    LenTreshold: Double;
    Timeout: Integer;
    Distance: Double;
    Stat: TStatRecord;
    RoadLengthAggregateBufferSize: Integer;
    RoadLengthAggregate: PRoadLengthAggregateRecord;
    RoadLengthCount: Integer;
    BufferSize: Integer;
    HashString: PAnsiChar;
    SignsLimit: UInt64;
    function FormatStartPos: Integer;
    function FormatIncrement: Integer;
    function ToString: string;
  end;

  TAStarFeature = (afLandMark);
  TAStarFeatureSet = set of TAStarFeature;

type
  TRT2RBRelation = record
    RT: Integer;
    RB: Integer;
  end;

const
  CRoadType2BinArray: array[0..15] of TRT2RBRelation = (
    (RT: RT_motorway;       RB: RB_motorway),
    (RT: RT_motorway_link;  RB: RB_motorway_link),
    (RT: RT_trunk;          RB: RB_trunk),
    (RT: RT_trunk_link;     RB: RB_trunk_link),
    (RT: RT_primary;        RB: RB_primary),
    (RT: RT_primary_link;   RB: RB_primary_link),
    (RT: RT_secondary;      RB: RB_secondary),
    (RT: RT_secondary_link; RB: RB_secondary_link),
    (RT: RT_tertiary;       RB: RB_tertiary),
    (RT: RT_tertiary_link;  RB: RB_tertiary_link),
    (RT: RT_residential;    RB: RB_residential),
    (RT: RT_road;           RB: RB_road),
    (RT: RT_unclassified;   RB: RB_unclassified),
    (RT: RT_service;        RB: RB_service),
    (RT: RT_living_street;  RB: RB_living_street),
    (RT: RT_pedestrian;     RB: RB_pedestrian)
  );

  CRoadSpeedRecordDef: TRoadSpeedsRecord = (
    Motorway      : RS_motorway;
    MotorwayLink  : RS_motorway_link;
    Trunk         : RS_trunk;
    TrunkLink     : RS_trunk_link;
    Primary       : RS_primary;
    PrimaryLink   : RS_primary_link;
    Secondary     : RS_secondary;
    SecondaryLink : RS_secondary_link;
    Tertiary      : RS_tertiary;
    TertiaryLink  : RS_tertiary_link;
    Residential   : RS_residential;
    Road          : RS_road;
    Unclassified  : RS_unclassified;
    Service       : RS_service;
    LivingStreet  : RS_living_street;
//TODO:    Pedestrian    : RS_pedestrian;
    reverse       : RS_reverse;
  );

type
  TRoadType = (
    rtUnknown,
    rtMotorway , rtMotorwayLink,
    rtTrunk    , rtTrunkLink,
    rtPrimary  , rtPrimaryLink,
    rtSecondary, rtSecondaryLink,
    rtTertiary , rtTertiaryLink,
    rtResidential,
    rtRoad,
    rtUnclassified,
    rtService,
    rtLiving_street,
    rtPedestrian
  );
  TRoadTypeSet = set of TRoadType;

  TRoadWorkSet = record
    Full: TRoadTypeSet;
    Reduced: TRoadTypeSet;
  end;

const
  CReductionTresholdDef = 50;

  CReductionFactorStartDef = 1; // возможные границы [0 - 2]
                                // 2 - более ограниченный набор крупных дорог; 0 - весь спектр, включая подворотни
  CReductionFactorDef = 0;
  CReductionFactorMin = 0;

  CReductionSet1 =
    [
       rtResidential
      ,rtService
      ,rtLiving_street
      ,rtPedestrian
//      ,rtRoad
//      ,rtUnclassified
    ];

  CReductionSet2 = CReductionSet1 +
    [
       rtTertiary
      ,rtTertiaryLink
    ];

  CReductionSet3 = CReductionSet2 +
    [
       rtSecondary
      ,rtSecondaryLink
//      ,rtRoad
//      ,rtUnclassified
    ];

function GetAStarFeatureSet(const AIntFeature: Int64): TAStarFeatureSet;
function SafeDiv(const a, b: Double; const def: Double = 0): Double;
function OSMToRoadType(const AOSMType: Integer): TRoadType;
function GetSpeedByOSM(const AOSMType: Integer): Integer;
function IsRoadFits(const AOSMType: Integer; const ARoadReqs: TRoadTypeSet = []): Boolean;
function GetReducedRoadWorkSet(const ASet: TRoadTypeSet; const AReductionFactor: UInt8): TRoadTypeSet;
function GetFullRoadWorkSet(const ARoadSpeeds: TRoadSpeedsRecord): TRoadTypeSet;
procedure RoadBin2Type(const ABin: Integer; out RTypes: TArray<Integer>);
procedure RoadType2Bin(const ATypes: TArray<Integer>; out RBin: Integer);

implementation

function GetAStarFeatureSet(const AIntFeature: Int64): TAStarFeatureSet;
var
  Feature: TAStarFeature;
begin
  Result := [];
  for Feature := Low(TAStarFeature) to High(TAStarFeature) do
  begin
    if (AIntFeature and (1 shl Ord(Feature))) <> 0 then
      Include(Result, Feature);
  end;
end;

function SafeDiv(const a, b: Double; const def: Double = 0): Double;
begin
  Result := def;
  if b <> 0 then
    Result := a / b;
end;

function OSMToRoadType(const AOSMType: Integer): TRoadType;
begin
  case AOSMType of
    RT_motorway       : Result := rtMotorway;
    RT_motorway_link  : Result := rtMotorwayLink;
    RT_trunk          : Result := rtTrunk;
    RT_trunk_link     : Result := rtTrunkLink;
    RT_primary        : Result := rtPrimary;
    RT_primary_link   : Result := rtPrimaryLink;
    RT_secondary      : Result := rtSecondary;
    RT_secondary_link : Result := rtSecondaryLink;
    RT_tertiary       : Result := rtTertiary;
    RT_tertiary_link  : Result := rtTertiaryLink;
    RT_residential    : Result := rtResidential;
    RT_road           : Result := rtRoad;
    RT_unclassified   : Result := rtUnclassified;
    RT_service        : Result := rtService;
    RT_living_street  : Result := rtLiving_street;
    RT_pedestrian     : Result := rtPedestrian;
    else                Result := rtUnknown;
  end;
end;

function GetSpeedByOSM(const AOSMType: Integer): Integer;
begin
  case AOSMType of
    RT_motorway       : Result := RS_motorway;
    RT_motorway_link  : Result := RS_motorway_link;
    RT_trunk          : Result := RS_trunk;
    RT_trunk_link     : Result := RS_trunk_link;
    RT_primary        : Result := RS_primary;
    RT_primary_link   : Result := RS_primary_link;
    RT_secondary      : Result := RS_secondary;
    RT_secondary_link : Result := RS_secondary_link;
    RT_tertiary       : Result := RS_tertiary;
    RT_tertiary_link  : Result := RS_tertiary_link;
    RT_residential    : Result := RS_residential;
    RT_road           : Result := RS_road;
    RT_unclassified   : Result := RS_unclassified;
    RT_service        : Result := RS_service;
    RT_living_street  : Result := RS_living_street;
    RT_pedestrian     : Result := RS_pedestrian;
    else                Result := RS_default;
  end;
end;

function IsRoadFits(const AOSMType: Integer; const ARoadReqs: TRoadTypeSet): Boolean;
begin
  Result := (ARoadReqs = []) or (OSMToRoadType(AOSMType) in ARoadReqs);
end;

function GetReducedRoadWorkSet(const ASet: TRoadTypeSet; const AReductionFactor: UInt8): TRoadTypeSet;
begin
  case AReductionFactor of
    0: Result := ASet;
    1: Result := ASet - CReductionSet1;
    2: Result := ASet - CReductionSet2;
    else Result := ASet - CReductionSet3;
  end;
end;

function GetFullRoadWorkSet(const ARoadSpeeds: TRoadSpeedsRecord): TRoadTypeSet;
begin
  Result := [];
  with ARoadSpeeds do
  begin
    if Motorway > 0 then Include(Result, rtMotorway);
    if MotorwayLink > 0 then Include(Result, rtMotorwayLink);
    if Trunk > 0 then Include(Result, rtTrunk);
    if TrunkLink > 0 then Include(Result, rtTrunkLink);
    if Primary > 0 then Include(Result, rtPrimary);
    if PrimaryLink > 0 then Include(Result, rtPrimaryLink);
    if Secondary > 0 then Include(Result, rtSecondary);
    if SecondaryLink > 0 then Include(Result, rtSecondaryLink);
    if Tertiary > 0 then Include(Result, rtTertiary);
    if TertiaryLink > 0 then Include(Result, rtTertiaryLink);
    if Residential > 0 then Include(Result, rtResidential);
    if Road > 0 then Include(Result, rtRoad);
    if Unclassified > 0 then Include(Result, rtUnclassified);
    if Service > 0 then Include(Result, rtService);
    if LivingStreet > 0 then Include(Result, rtLiving_street);
//TODO:    if Pedestrian > 0 then Include(Result, rtPedestrian);
  end;
end;

procedure RoadBin2Type(const ABin: Integer; out RTypes: TArray<Integer>);
var
  I: Integer;
begin
  SetLength(RTypes, 0);
  for I := Low(CRoadType2BinArray) to High(CRoadType2BinArray) do
  begin
    if (ABin and CRoadType2BinArray[I].RB) <> 0 then
    begin
      SetLength(RTypes, Length(RTypes) + 1);
      RTypes[High(RTypes)] := CRoadType2BinArray[I].RT;
    end;
  end;
end;

procedure RoadType2Bin(const ATypes: TArray<Integer>; out RBin: Integer);
var
  I, J: Integer;
begin
  RBin := 0;
  for J := Low(ATypes) to High(ATypes) do
  begin
    for I := Low(CRoadType2BinArray) to High(CRoadType2BinArray) do
    begin
      if CRoadType2BinArray[I].RT = ATypes[J] then
      begin
        RBin := RBin or CRoadType2BinArray[I].RB;
        Break;
      end;
    end;
  end;
end;

{ TRoadSpeedsRecord }

function TRoadSpeedsRecord.GetDuration(const ARoadLength: TRoadLengthByTypeRecord): Double;
begin
  Result :=
    SafeDiv(ARoadLength.Motorway      , Motorway     ) +
    SafeDiv(ARoadLength.MotorwayLink  , MotorwayLink ) +
    SafeDiv(ARoadLength.Trunk         , Trunk        ) +
    SafeDiv(ARoadLength.TrunkLink     , TrunkLink    ) +
    SafeDiv(ARoadLength.Primary       , Primary      ) +
    SafeDiv(ARoadLength.PrimaryLink   , PrimaryLink  ) +
    SafeDiv(ARoadLength.Secondary     , Secondary    ) +
    SafeDiv(ARoadLength.SecondaryLink , SecondaryLink) +
    SafeDiv(ARoadLength.Tertiary      , Tertiary     ) +
    SafeDiv(ARoadLength.TertiaryLink  , TertiaryLink ) +
    SafeDiv(ARoadLength.Residential   , Residential  ) +
    SafeDiv(ARoadLength.Road          , Road         ) +
    SafeDiv(ARoadLength.Unclassified  , Unclassified ) +
    SafeDiv(ARoadLength.Service       , Service      ) +
    SafeDiv(ARoadLength.LivingStreet  , LivingStreet );
end;

function TRoadSpeedsRecord.GetDuration(const ARoadLength: TRoadLengthAggregateRecord): Double;
begin
  Result := SafeDiv(ARoadLength.RoadLength, GetSpeedByRoadType(ARoadLength.RoadType), 0);
end;

function TRoadSpeedsRecord.GetSpeedByRoadType(const ARoadType: Integer): Integer;
begin
  case ARoadType of
    RT_motorway       : Result := Motorway;
    RT_motorway_link  : Result := MotorwayLink;
    RT_trunk          : Result := Trunk;
    RT_trunk_link     : Result := TrunkLink;
    RT_primary        : Result := Primary;
    RT_primary_link   : Result := PrimaryLink;
    RT_secondary      : Result := Secondary;
    RT_secondary_link : Result := SecondaryLink;
    RT_tertiary       : Result := Tertiary;
    RT_tertiary_link  : Result := TertiaryLink;
    RT_residential    : Result := Residential;
    RT_road           : Result := Road;
    RT_unclassified   : Result := Unclassified;
    else                Result := RS_default;
  end;
end;

{ TRoadLengthByTypeRecord }

procedure TRoadLengthByTypeRecord.AddRoadLen(ARoadType: Integer; ADistance: Double);

  procedure AddLen(var R: Double; const A: Double);
  begin
    R := R + A;
  end;

begin
  begin
    case OSMToRoadType(ARoadType) of
      rtMotorway      : AddLen(Motorway, ADistance);
      rtMotorwayLink  : AddLen(MotorwayLink, ADistance);
      rtTrunk         : AddLen(Trunk, ADistance);
      rtTrunkLink     : AddLen(TrunkLink, ADistance);
      rtPrimary       : AddLen(Primary, ADistance);
      rtPrimaryLink   : AddLen(PrimaryLink, ADistance);
      rtSecondary     : AddLen(Secondary, ADistance);
      rtSecondaryLink : AddLen(SecondaryLink, ADistance);
      rtTertiary      : AddLen(Tertiary, ADistance);
      rtTertiaryLink  : AddLen(TertiaryLink, ADistance);
      rtResidential   : AddLen(Residential, ADistance);
      rtRoad          : AddLen(Road, ADistance);
      rtUnclassified  : AddLen(Unclassified, ADistance);
      rtService       : AddLen(Service, ADistance);
      rtLiving_street : AddLen(LivingStreet, ADistance);
    end;
  end;
end;

constructor TRoadLengthByTypeRecord.Create(AZone: UInt64);
begin
  Init;
  Zones := AZone;
end;

procedure TRoadLengthByTypeRecord.Init;
begin
  FillChar(Self, SizeOf(TRoadLengthByTypeRecord), 0);
end;

{ TAstarRequest }

function TAstarRequest.ToString: string;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create('ru-ru');
  fs.DecimalSeparator := '.';
  with RoadSpeedsRecord do
  begin
    Result := 'REQUEST='
      + '('
        + IntToStr(Version) + ','
        + '['
        + FloatToStrF(FromLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(FromLongitude, ffFixed, 12, 8, fs)
        + ']-['
        + FloatToStrF(ToLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(ToLongitude, ffFixed, 12, 8, fs)
        + '],'
        + IntToStr(ZonesLimit)
      + '), ('
        + IntToStr(Motorway) + ','
        + IntToStr(MotorwayLink) + ','
        + IntToStr(Trunk) + ','
        + IntToStr(TrunkLink) + ','
        + IntToStr(Primary) + ','
        + IntToStr(PrimaryLink) + ','
        + IntToStr(Secondary) + ','
        + IntToStr(SecondaryLink) + ','
        + IntToStr(Tertiary) + ','
        + IntToStr(TertiaryLink) + ','
        + IntToStr(Residential) + ','
        + IntToStr(Road) + ','
        + IntToStr(Unclassified) + ','
        + IntToStr(Service) + ','
        + IntToStr(LivingStreet) + ','
//        + IntToStr(Pedestrian) + ','
        + IntToStr(reverse)
      + '), ('
        + IntToStr(FormatVariant) + ','
        + FloatToStrF(LenTreshold, ffFixed, 5, 6, fs) + 'km,'
        + IntToStr(Timeout) + 's,'
        + FloatToStrF(Distance, ffFixed, 5, 6, fs) + 'km,'
        + FloatToStrF(MinsPerDay * Duration, ffFixed, 4, 6, fs) + 'min,'
        + IntToStr(BufferSize)
      + ')';
  end;
end;

{ TAstarRequest3 }

function TAstarRequest3.FormatIncrement: Integer;
begin
  case FormatVariant of
    0: Result := 12;
    5: Result := 31;
    else Result := 15;
  end;
end;

function TAstarRequest3.FormatStartPos: Integer;
begin
  case FormatVariant of
    0: Result := 3;
    5: Result := 3;
    else Result := 3;
  end;
end;

function TAstarRequest3.ToString: string;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create('ru-ru');
  fs.DecimalSeparator := '.';
  Result := 'REQUEST='
    + '('
      + IntToStr(Version) + ','
      + '['
      + FloatToStrF(FromLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(FromLongitude, ffFixed, 12, 8, fs)
      + ']-['
      + FloatToStrF(ToLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(ToLongitude, ffFixed, 12, 8, fs)
      + '],'
      + IntToStr(ZonesLimit)
    + '), ('
      + IntToStr(FormatVariant) + ','
      + FloatToStrF(LenTreshold, ffFixed, 5, 6, fs) + 'km,'
      + IntToStr(Timeout) + 's,'
      + FloatToStrF(Distance, ffFixed, 5, 6, fs) + 'km,'
      + IntToStr(BufferSize)
    + '), ('
      + IntToStr(RoadTypeLimit) + ','
      + IntToStr(OsmTypeLimit) + ','
      + IntToStr(FeatureLimit) + ','
      + IntToStr(SignsLimit)
    + ')';
end;

{ TAstarRequest4 }

function TAstarRequest4.FormatIncrement: Integer;
begin
  Result := 12;
  if (Ord(Low(TGeoHashStringFormat)) <= FormatVariant) and (Ord(High(TGeoHashStringFormat)) >= FormatVariant) then
    Result := 12 + CGeoHashStringFormatInfoLen[TGeoHashStringFormat(FormatVariant)];
end;

function TAstarRequest4.FormatStartPos: Integer;
begin
  case FormatVariant of
    0: Result := 3;
    else Result := 3;
  end;
end;

function TAstarRequest4.ToString: string;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create('ru-ru');
  fs.DecimalSeparator := '.';
  Result := 'REQUEST='
    + '('
      + IntToStr(Version) + ','
      + '['
      + FloatToStrF(FromLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(FromLongitude, ffFixed, 12, 8, fs)
      + ']-['
      + FloatToStrF(ToLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(ToLongitude, ffFixed, 12, 8, fs)
      + '],'
      + IntToStr(ZonesLimit)
    + '), ('
      + IntToStr(FormatVariant) + ','
      + FloatToStrF(LenTreshold, ffFixed, 5, 6, fs) + 'km,'
      + IntToStr(Timeout) + 's,'
      + FloatToStrF(Distance, ffFixed, 5, 6, fs) + 'km,'
      + IntToStr(BufferSize)
    + '), ('
      + IntToStr(RoadTypeLimit) + ','
      + IntToStr(OsmTypeLimit) + ','
      + IntToStr(Feature) + ','
      + IntToStr(SignsLimit)
    + ')';
end;

end.

