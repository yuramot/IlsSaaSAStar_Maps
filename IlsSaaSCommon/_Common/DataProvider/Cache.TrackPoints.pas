unit Cache.TrackPoints;

interface

uses
  SysUtils,
  Cache.Root,
  Adapter.MySQL,
  Geo.Pos,
  ZConnection, ZDataset,
  Ils.MySql.Conf,
  uTrackPoints,
  Math,
  Generics.Collections,
  Ils.Logger;

//------------------------------------------------------------------------------
type


  TTrackCache = class(TCache)
  public
    procedure PushPicture(
      const AObj: TCacheDataObjectAbstract
    );

    procedure UpdateOdometer(
      const ADT: TDateTime;
      const ADelta: Double
    );

    function GetBeforeByType(
      const ADT: TDateTime;
      const AType: TPointType
    ): TTrackPoint;

    function GetAfterByType(
      const ADT: TDateTime;
      const AType: TPointType
    ): TTrackPoint;
  end;

  TTrackPointCacheDataAdapter = class(TCacheAdapterMySQL)
  private
    FInsertPictureQry: TZQuery;
    FInsertSensorRawQry: TZQuery;
    FUpdateLastInfoQry: TZQuery;
    FImeiInsertQry: TZQuery;
    FOdometerUpdateQry: TZQuery;

    FDTMarkAfterByTypeQry: TZQuery;
    FDTMarkBeforeByTypeQry: TZQuery;

    FImeiHash: TDictionary<Int64, Int64>;

    procedure InsertNewImei(AIMEI: Int64);
  public
    procedure Flush(
      const ACache: TCache
    ); override;

    procedure Add(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); override;

    procedure PushPicture(
      const AObj: TCacheDataObjectAbstract
    );

    procedure UpdateOdometer(
      const ACache: TCache;
      const ADT: TDateTime; const ADelta: Double
    );

    procedure Change(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); override;

    function MakeObjFromReadQuery(
      const AQuery: TZQuery
    ): TCacheDataObjectAbstract; override;

    function GetDTMarkBeforeByType(
      const ACache: TCache;
      const ADT: TDateTime;
      const AType: TPointType
    ): TDateTime;

    function GetDTMarkAfterByType(
      const ACache: TCache;
      const ADT: TDateTime;
      const AType: TPointType
    ): TDateTime;

    constructor Create(
      const AReadConnection, AWriteConnection: TZConnection
    );

    destructor Destroy; override;
  end;

  TTrackPointCacheDictionary = TCacheDictionary<Int64>;

implementation

{ TTrackPointCacheDataAdapter }

constructor TTrackPointCacheDataAdapter.Create(
  const AReadConnection, AWriteConnection: TZConnection
);
//var
//  SQLInsert: string;
//  SQLUpdate: string;
//  SQLReadBefore: string;
//  SQLReadAfter: string;
//  SQLReadRange: string;
//  SQLDeleteOne: string;
//  SQLInsertPicture: string;
//  SQLInsertSensorRaw: string;
//  SQLUpdateLastInfo: string;
//  SQLImeiInsert: string;
//  SQLOdometerUpdate: string;
const
  FieldsSQL =
      ' Type = :Type, Speed = :Speed,'
    + ' Azimuth = :Azimuth,'
    + ' Distance = :Distance, Duration = :Duration,'
    + ' AngularSpeed = :AngularSpeed, RadialAcc = :RadialAcc,'
    + ' TangentialAcc = :TangentialAcc, Odometer = :Odometer'
    ;

  Fields2SQL =
      ' Recieved = VALUES(Recieved), Offset = VALUES(Offset),'
    + ' Valid = VALUES(Valid), SatCount = VALUES(SatCount), Engine = VALUES(Engine),'
    + ' Latitude = VALUES(Latitude), Longitude = VALUES(Longitude), Altitude = VALUES(Altitude),'
    + ' Type = VALUES(Type), Speed = VALUES(Speed), DeviceSpeed = VALUES(DeviceSpeed),'
    + ' Azimuth = VALUES(Azimuth), DeviceAzimuth = VALUES(DeviceAzimuth),'
    + ' Distance = VALUES(Distance), Duration = VALUES(Duration),'
    + ' AngularSpeed = VALUES(AngularSpeed), RadialAcc = VALUES(RadialAcc),'
    + ' TangentialAcc = VALUES(TangentialAcc), Odometer = VALUES(Odometer)'
    ;

  ReadRangeSQL = 'SELECT *'
    + ' FROM track'
    + ' WHERE IMEI = :imei'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to'
    ;
  ReadBeforeSQL = 'SELECT MAX(DT) as DT'
    + ' FROM track'
    + ' WHERE IMEI = :imei'
    + ' AND DT < :dt'
    ;
  ReadAfterSQL = 'SELECT MIN(DT) as DT'
    + ' FROM  track'
    + ' WHERE IMEI = :imei'
    + ' AND DT > :dt'
    ;
  InsertSQL =
    'insert into track set'
    + ' IMEI = :IMEI, DT = :DT,'
    + ' Offset = :Offset, Recieved = :Recieved,'
    + ' Valid = :Valid, SatCount = :SatCount, Engine = :Engine,'
    + ' Latitude = :Latitude, Longitude = :Longitude, Altitude = :Altitude,'
    + ' DeviceSpeed = :DeviceSpeed, DeviceAzimuth = :DeviceAzimuth,'
    + FieldsSQL
    + ' on duplicate key update'
    + Fields2SQL
    ;
  UpdateSQL =
    'update track set'
    + FieldsSQL
    + ' where'
    + ' IMEI = :IMEI'
    + ' and DT = :DT'
    ;
  DeleteOneSQL =
    '*';
  InsertPictureSQL =
    'insert ignore into trackpicture set'
    + ' IMEI = :IMEI,'
    + ' DT = :DT,'
    + ' Recieved = :Recieved,'
    + ' Picture = :Picture'
    ;
  InsertSensorRawSQL =
    'insert ignore into sensorraw set'
    + ' IMEI = :IMEI, DT = :DT,'
    + ' Raw = :Raw'
    ;
  UpdateLastInfoSQL =
    'CALL Mon_UpdateTrackerLastInfo('
    + ' :IMEI, :DT, :Recieved, :Valid,'
    + ' :SatCount, :Latitude, :Longitude,'
    + ' :Speed, :Azimuth, :Type, :Odometer'
    + ' );'
//    'insert into trackerlastinfo set'
//    + ' IMEI = :IMEI, DT = :DT, Recieved = :Recieved, Valid = :Valid,'
//    + ' SatCount = :SatCount, Latitude = :Latitude, Longitude = :Longitude,'
//    + ' Speed = :Speed, Azimuth = :Azimuth, Type = :Type, Odometer = :Odometer'
//    + ' on duplicate key update'
//    + ' DT = VALUES(DT), Recieved = VALUES(Recieved), Valid = VALUES(Valid),'
//    + ' SatCount = VALUES(SatCount), Latitude = VALUES(Latitude), Longitude = VALUES(Longitude),'
//    + ' Speed = VALUES(Speed), Azimuth = VALUES(Azimuth), Type = VALUES(Type), Odometer = VALUES(Odometer)'
    ;

  ImeiInsertSQL =
    'insert ignore into'
    + ' tracker(IMEI, TrackerModelID, AccountID)'
    + ' values (:IMEI, :TrackerModelID, :AccountID);'
    ;


  OdometerUpdateSQL =
    'update track set odometer = odometer + :OdometerDelta'
    + ' where IMEI = :IMEI and DT >= :DT and Type in (0, 1);'
    ;

  DTMarkAfterByTypeSQL =
    'SELECT MIN(dt) AS DT FROM Track'
    + ' WHERE'
    + ' DT > :DT'
    + ' AND IMEI = :IMEI'
    + ' AND TYPE = :TYPE'
  ;

  DTMarkBeforeByTypeSQL =
    'SELECT MAX(dt) AS DT FROM Track'
    + ' WHERE'
    + ' DT < :DT'
    + ' AND IMEI = :IMEI'
    + ' AND TYPE = :TYPE'
  ;

begin
  inherited Create(
    AReadConnection, AWriteConnection,
    InsertSQL, UpdateSQL, ReadBeforeSQL, ReadAfterSQL, ReadRangeSQL, DeleteOneSQL);

  FInsertPictureQry := TZQuery.Create(nil);
  FInsertPictureQry.Connection := AWriteConnection;
  FInsertPictureQry.SQL.Text := InsertPictureSQL;

  FInsertSensorRawQry := TZQuery.Create(nil);
  FInsertSensorRawQry.Connection := AWriteConnection;
  FInsertSensorRawQry.SQL.Text := InsertSensorRawSQL;

  FUpdateLastInfoQry := TZQuery.Create(nil);
  FUpdateLastInfoQry.Connection := AWriteConnection;
  FUpdateLastInfoQry.SQL.Text := UpdateLastInfoSQL;

  FImeiInsertQry := TZQuery.Create(nil);
  FImeiInsertQry.Connection := AWriteConnection;
  FImeiInsertQry.SQL.Text := ImeiInsertSQL;

  FOdometerUpdateQry := TZQuery.Create(nil);
  FOdometerUpdateQry.Connection := AWriteConnection;
  FOdometerUpdateQry.SQL.Text := OdometerUpdateSQL;

  FDTMarkAfterByTypeQry := TZQuery.Create(nil);
  FDTMarkAfterByTypeQry.Connection := AReadConnection;
  FDTMarkAfterByTypeQry.SQL.Text := DTMarkAfterByTypeSQL;

  FDTMarkBeforeByTypeQry := TZQuery.Create(nil);
  FDTMarkBeforeByTypeQry.Connection := AReadConnection;
  FDTMarkBeforeByTypeQry.SQL.Text := DTMarkBeforeByTypeSQL;

  FImeiHash := TDictionary<Int64, Int64>.Create;
end;


destructor TTrackPointCacheDataAdapter.Destroy;
begin
  FInsertPictureQry.Free;
  FInsertSensorRawQry.Free;
  FUpdateLastInfoQry.Free;
  FOdometerUpdateQry.Free;

  FImeiHash.Free;

  inherited;
end;

procedure TTrackPointCacheDataAdapter.Flush(const ACache: TCache);
begin
  //
end;

function TTrackPointCacheDataAdapter.GetDTMarkAfterByType(
  const ACache: TCache;
  const ADT: TDateTime;
  const AType: TPointType): TDateTime;
begin
  Result := 0;
  FillParamsFromKey(ACache.Key, FDTMarkAfterByTypeQry);

  with FDTMarkAfterByTypeQry do
  begin
    Close;

    ParamByName('DT').AsDateTime := ADT;
    ParamByName('type').AsInteger := Ord(AType);

    Open;
    if RecordCount > 0 then
      Result := FieldByName('DT').AsDateTime;
  end;
end;

function TTrackPointCacheDataAdapter.GetDTMarkBeforeByType(
  const ACache: TCache;
  const ADT: TDateTime;
  const AType: TPointType): TDateTime;
begin
  Result := 0;
  FillParamsFromKey(ACache.Key, FDTMarkBeforeByTypeQry);

  with FDTMarkBeforeByTypeQry do
  begin
    Close;

    ParamByName('DT').AsDateTime := ADT;
    ParamByName('type').AsInteger := Ord(AType);

    Open;
    if RecordCount > 0 then
      Result := FieldByName('DT').AsDateTime;
  end;
end;

procedure TTrackPointCacheDataAdapter.InsertNewImei(AIMEI: Int64);

  function ModelByIMEI(AIMEI2: Int64): Integer;
  var
    VendorImei: Integer;
  begin
    VendorImei := StrToIntDef(Copy(IntToStr(AIMEI2), 1, 2), 0);
    case VendorImei of
      35: Result := 8;//model 8, vendor 6 Teltonika
      86: Result := 9;//model 9, vendor 10 Navtelecom
      else Result := 0;
    end;
  end;

var
  TrackerModel: Integer;
begin
  if FImeiHash.ContainsKey(AIMEI) then
  begin
    FImeiHash[AIMEI] := FImeiHash[AIMEI] + 1;
    Exit;
  end;

  try
    FImeiInsertQry.ParamByName('imei').AsLargeInt           := AIMEI;
    TrackerModel := ModelByIMEI(AIMEI);

    FImeiInsertQry.ParamByName('TrackerModelID').AsInteger  := TrackerModel;//Unknown of vendor detected
    FImeiInsertQry.ParamByName('AccountID').AsInteger       := 1;//Root
    FImeiInsertQry.ExecSQL;

    FImeiHash.Add(AIMEI, 1);
//    ToLog('IMEI ' + IntToStr(AIMEI) + ' inserted.');

  except
    on E: Exception do
      ToLog('error on insert IMEI - ' + E.ClassName + ':' + E.Message);
  end;
end;

procedure TTrackPointCacheDataAdapter.Add(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  FillParamsFromKey(ACache.Key, FInsertQry);
  FillParamsFromKey(ACache.Key, FInsertSensorRawQry);
  FillParamsFromKey(ACache.Key, FUpdateLastInfoQry);

  with (AObj as TTrackPoint), FInsertQry do
  begin
    Close;

    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;
    ParamByName('Offset').AsLargeInt := Offset;
    ParamByName('Valid').AsInteger := IfThen(DevicePoint.ValidCoordinates, 1, 0);
    ParamByName('SatCount').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
    ParamByName('Engine').AsInteger := IfThen(DevicePoint.EngineWorks, 1, 0);
    ParamByName('Latitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
    ParamByName('Longitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
    ParamByName('Altitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Altitude;

//    case PointType of
//      ptStop,
//      ptMove: ParamByName('Type').AsInteger := CPointType[PointType].Code;
//      ptUnknown,
//      ptInvalid,
//      ptTimeIncorrect,
//      ptDuplicated,
//      ptGeoIncorrect,
//      ptTimeHasMSec: ParamByName('Type').AsInteger := CPointType[ptUnknown].Code;
//      ptPhysicsIncorrect,
//      ptValid,
//      ptUnique,
//      ptGeoCorrect,
//      ptPhysicsCorrect,
//      ptTimeHasNoMSec: ParamByName('Type').AsInteger := CPointType[ptStop].Code;
//    end;

    ParamByName('Type').AsInteger := CPointType[PointType].Code;

    ParamByName('DeviceSpeed').AsFloat := IfThen(IsInfinite(DevicePoint.Velocity), 0, DevicePoint.Velocity);
    ParamByName('DeviceAzimuth').AsFloat := IfThen(IsInfinite(DevicePoint.Azimuth), 0, DevicePoint.Azimuth);

    ParamByName('Speed').AsFloat := IfThen(IsInfinite(SpeedKmh), 0, SpeedKmh);
    ParamByName('Azimuth').AsFloat := IfThen(IsInfinite(Azimuth), 0, Azimuth);
    ParamByName('Distance').AsFloat := IfThen(IsInfinite(Distance), 0, Distance);
    ParamByName('Duration').AsFloat := IfThen(IsInfinite(Duration), 0, Duration);
    ParamByName('AngularSpeed').AsFloat := IfThen(IsInfinite(AngularSpeed), 0, AngularSpeed);
    ParamByName('RadialAcc').AsFloat := IfThen(IsInfinite(RadialAcceleration), 0, RadialAcceleration);
    ParamByName('TangentialAcc').AsFloat := IfThen(IsInfinite(TangentialAcceleration), 0, TangentialAcceleration);

    ParamByName('Odometer').AsFloat := IfThen(IsInfinite(Odometer), 0, Odometer);

    ExecSQL;
  end;

  with (AObj as TTrackPoint), FInsertSensorRawQry do
  begin
    Close;

    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Raw').AsString := DevicePoint.SensorsJsonString;

    ExecSQL();
  end;

  with (AObj as TTrackPoint), FUpdateLastInfoQry do
  if PointType in CProperOdoPointTypes then
  begin
    Close;

    ParamByName('SATCOUNT').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
    ParamByName('LATITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
    ParamByName('LONGITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
    ParamByName('SPEED').AsFloat := IfThen(not IsInfinite(DevicePoint.Velocity), DevicePoint.Velocity, 0);
    ParamByName('AZIMUTH').AsFloat := IfThen(not IsInfinite(DevicePoint.Azimuth), DevicePoint.Azimuth, 0);
    ParamByName('TYPE').AsInteger := CPointType[PointType].Code;
    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;

    ExecSQL();
  end;

  with (AObj as TTrackPoint) do
    InsertNewImei(DevicePoint.DeviceID);

end;

procedure TTrackPointCacheDataAdapter.Change(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  FillParamsFromKey(ACache.Key, FUpdateQry);

  with (AObj as TTrackPoint), FUpdateQry do
  begin
    Close;

    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Type').AsInteger := CPointType[PointType].Code;

    ParamByName('Speed').AsFloat := IfThen(IsInfinite(SpeedKmh), 0, SpeedKmh);
    ParamByName('Azimuth').AsFloat := IfThen(IsInfinite(Azimuth), 0, Azimuth);
    ParamByName('Distance').AsFloat := IfThen(IsInfinite(Distance), 0, Distance);
    ParamByName('Duration').AsFloat := IfThen(IsInfinite(Duration), 0, Duration);
    ParamByName('AngularSpeed').AsFloat := IfThen(IsInfinite(AngularSpeed), 0, AngularSpeed);
    ParamByName('RadialAcc').AsFloat := IfThen(IsInfinite(RadialAcceleration), 0, RadialAcceleration);
    ParamByName('TangentialAcc').AsFloat := IfThen(IsInfinite(TangentialAcceleration), 0, TangentialAcceleration);

    ParamByName('Odometer').AsFloat := IfThen(IsInfinite(Odometer), 0, Odometer);

    ExecSQL;
  end;
end;

function TTrackPointCacheDataAdapter.MakeObjFromReadQuery(
  const AQuery: TZQuery
): TCacheDataObjectAbstract;
begin
  with AQuery do
  begin
    Result :=
      TTrackPoint.Create(
        TDevicePoint.Create(
          TGeoTrackPoint.Create(
            FieldByName('Latitude').AsFloat,
            FieldByName('Longitude').AsFloat,
            FieldByName('DT').AsDateTime,
            FieldByName('SatCount').AsInteger,
            FieldByName('Altitude').AsFloat
          ),
          nil,
          FieldByName('IMEI').AsLargeInt,
          FieldByName('Valid').AsInteger = 1,
          FieldByName('Recieved').AsDateTime,
          FieldByName('Engine').AsInteger = 1,
          '',//FieldByName('Json').AsString,
          FieldByName('Distance').AsFloat,
          FieldByName('DeviceSpeed').AsFloat,
          FieldByName('Azimuth').AsFloat,
          0
        ), poDatabase,
        GetPointTypeByCode(FieldByName('Type').AsInteger),
        FieldByName('Odometer').AsFloat,
        FieldByName('Offset').AsLargeInt
      );
  end;
end;

procedure TTrackPointCacheDataAdapter.PushPicture(const AObj: TCacheDataObjectAbstract);
begin
  with (AObj as TTrackPicture), FInsertPictureQry do
  begin
    Close;

    ParamByName('Imei').AsLargeInt := Imei;
    ParamByName('DT').AsDateTime := DateTime;
    ParamByName('Recieved').AsDateTime := Recieved;
    ParamByName('Picture').AsBlob := Picture;

    ExecSQL();
  end;
end;

procedure TTrackPointCacheDataAdapter.UpdateOdometer(
  const ACache: TCache;
  const ADT: TDateTime; const ADelta: Double);
begin
  FillParamsFromKey(ACache.Key, FOdometerUpdateQry);

  with FOdometerUpdateQry do
  begin
    Close;

    ParamByName('DT').AsDateTime := ADT;
    ParamByName('OdometerDelta').AsFloat := ADelta;

    ExecSQL;
  end;
end;

{ TTrackCache }

function TTrackCache.GetAfterByType(const ADT: TDateTime; const AType: TPointType): TTrackPoint;
begin
  Result := TTrackPoint(Get((FAdapter as TTrackPointCacheDataAdapter).GetDTMarkAfterByType(Self, ADT, AType)));
end;

function TTrackCache.GetBeforeByType(const ADT: TDateTime; const AType: TPointType): TTrackPoint;
begin
  Result := TTrackPoint(Get((FAdapter as TTrackPointCacheDataAdapter).GetDTMarkBeforeByType(Self, ADT, AType)));
end;

procedure TTrackCache.PushPicture(const AObj: TCacheDataObjectAbstract);
begin
  (FAdapter as TTrackPointCacheDataAdapter).PushPicture(AObj);
end;

procedure TTrackCache.UpdateOdometer(const ADT: TDateTime; const ADelta: Double);
begin
  (FAdapter as TTrackPointCacheDataAdapter).UpdateOdometer(Self, ADT, ADelta);
end;

end.

