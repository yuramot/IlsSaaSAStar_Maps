unit uTrackCacheMySQL;

interface

uses
  Windows, SysUtils, Registry, Data.DB, Data.Win.ADODB, uMoveEvents,
  uTrackPoints, Winapi.ActiveX, Generics.Collections, uSensors,
  DateUtils, Math, StrUtils, Ils.Logger, System.IniFiles, Ils.Utils,
  ZAbstractConnection, ZConnection, ZAbstractRODataset, ZAbstractDataset,
  ZDataset, ZDbcIntfs, Ils.MySql.Conf, Vcl.ExtCtrls, Ils.Utils.Svc, Geo.Pos,
  Event.Cnst, DP.Root;

type

  TTrackAnalyzerCache = class(TCacheRoot)
  protected
    FQueryInsertSensorRaw: TZQuery;
    FQueryUpdateLastInfo: TZQuery;
    FQueryInsertPicture: TZQuery;
    //! вставить запись в БД
    procedure ExecDBInsert(
      const AObj: IDataObj
    ); override;
    //! обновить запись в БД
    procedure ExecDBUpdate(
      const AObj: IDataObj
    ); override;
    //! преобразователь из записи БД в объект
    function MakeObjFromReadReq(
      const AQuery: TZQuery
    ): IDataObj; override;
  public
    procedure PushPicture(const AObj: IDataObj);
    constructor Create(
      const AIMEI: Int64;
      const AReadConnection: TZConnection;
      const AWriteConnection: TZConnection;
      const ACacheCapacityLimit: Integer;
      const ACacheInterval: Double
    );
    destructor Destroy; override;
  end;

implementation

{ TTrackAnalyzerCache }

constructor TTrackAnalyzerCache.Create(
  const AIMEI: Int64;
  const AReadConnection: TZConnection;
  const AWriteConnection: TZConnection;
  const ACacheCapacityLimit: Integer;
  const ACacheInterval: Double);
const
  CReqReadRange = 'SELECT *'
    + ' FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CReqReadBefore = 'SELECT *'
    + ' FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CReqReadAfter = 'SELECT *'
    + ' FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
//  CReqInsert = 'INSERT IGNORE INTO track'
//    + ' (IMEI, DT, EventTypeID, Duration, Latitude, Longitude, Radius, Dispersion)'
//    + ' VALUES (:imei, :dt, :event_type, :dur, :la, :lo, :radius, :dispersion)';
  CReqInsert =
    'INSERT IGNORE INTO Track (' +
    ' Imei, DT, Recieved, Offset, Valid, SatCount, Engine,' +
    ' Latitude, Longitude, Altitude, Type, Speed, DeviceSpeed, Azimuth, DeviceAzimuth, Distance,' +
    ' Duration, AngularSpeed, RadialAcc, TangentialAcc) VALUES'+
    '(:Imei, :DT, :Recieved, :Offset, :Valid, :SatCount, :Engine, ' +
    ':Latitude, :Longitude, :Altitude, :Type, :Speed, :DeviceSpeed, :Azimuth, :DeviceAzimuth, :Distance, ' +
    ':Duration, :AngularSpeed, :RadialAcc, :TangentialAcc)';

  CReqUpdate =
    'UPDATE Track SET DT = DT, Type = :Type, Speed = :Speed,' +
    ' Azimuth = :Azimuth, Distance = :Distance, AngularSpeed = :AngularSpeed,' +
    ' Duration = :Duration, RadialAcc = :RadialAcc, TangentialAcc = :TangentialAcc' +
    ' WHERE Imei = :Imei AND DT = :DT';

  CReqDeleteRange = 'DELETE FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';

  CReqLastPresentDT = 'SELECT MAX(DT) AS DT FROM track WHERE IMEI = :IMEI';

  CReqInsertSensors =
    'INSERT IGNORE INTO SensorRaw (Imei, DT, Raw) VALUES (:Imei, :DT, :Raw)';

  CReqUpdateLastInfo =
    'INSERT IGNORE INTO trackerlastinfo SET ' +
    '`IMEI` = :IMEI, ' +
    '`SatCount` = :SATCOUNT, `Latitude` = :LATITUDE, ' +
    '`Longitude` = :LONGITUDE, `Speed` = :SPEED, `Azimuth` = :AZIMUTH, ' +
    '`Type` = :TYPE, `DT` = :DT, `Recieved` = :Recieved  ' +
    'ON DUPLICATE KEY UPDATE ' +
    '`SatCount` = :SATCOUNT2, `Latitude` = :LATITUDE2, ' +
    '`Longitude` = :LONGITUDE2, `Speed` = :SPEED2, `Azimuth` = :AZIMUTH2, ' +
    '`Type` = :TYPE2, `DT` = :DT2, `Recieved` = :Recieved2';

  CReqInsertPicture =
    'INSERT IGNORE INTO trackpicture(IMEI, DT, Recieved, Picture) ' +
    'VALUES(:IMEI, :DT, :Recieved, :Picture)';

begin
  inherited Create(
    AReadConnection, AWriteConnection, True, ACacheCapacityLimit, ACacheInterval,
    CReqReadRange, CReqReadBefore, CReqReadAfter, CReqInsert, CReqUpdate, CReqDeleteRange, CReqLastPresentDT);

  FQueryInsertSensorRaw := TZQuery.Create(nil);
  FQueryInsertSensorRaw.Connection := AWriteConnection;
  FQueryInsertSensorRaw.SQL.Text := CReqInsertSensors;
  FQueryInsertSensorRaw.ParamByName('imei').AsLargeInt := AIMEI;

  FQueryUpdateLastInfo := TZQuery.Create(nil);
  FQueryUpdateLastInfo.Connection := AWriteConnection;
  FQueryUpdateLastInfo.SQL.Text := CReqUpdateLastInfo;
  FQueryUpdateLastInfo.ParamByName('imei').AsLargeInt := AIMEI;

  FQueryInsertPicture := TZQuery.Create(nil);
  FQueryInsertPicture.Connection := AWriteConnection;
  FQueryInsertPicture.SQL.Text := CReqInsertPicture;
  FQueryInsertPicture.ParamByName('imei').AsLargeInt := AIMEI;

  FQueryInsert.ParamByName('imei').AsLargeInt := AIMEI;
  FQueryUpdate.ParamByName('imei').AsLargeInt := AIMEI;
  FQueryReadRange.ParamByName('imei').AsLargeInt := AIMEI;
  FQueryReadBefore.ParamByName('imei').AsLargeInt := AIMEI;
  FQueryReadAfter.ParamByName('imei').AsLargeInt := AIMEI;
  FQueryDeleteRange.ParamByName('imei').AsLargeInt := AIMEI;
  FQueryLastPresentDT.ParamByName('imei').AsLargeInt := AIMEI;
end;

destructor TTrackAnalyzerCache.Destroy;
begin
  FQueryInsertSensorRaw.Free;
  FQueryUpdateLastInfo.Free;
  FQueryInsertPicture.Free;

  inherited;
end;

procedure TTrackAnalyzerCache.ExecDBInsert(const AObj: IDataObj);
begin
  with (AObj as TTrackPoint), FQueryInsert do
  begin
    Close;

    ParamByName('Imei').AsLargeInt := DevicePoint.DeviceID;
    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;
    ParamByName('Offset').AsLargeInt := Offset;
    ParamByName('Valid').AsInteger := IfThen(DevicePoint.ValidCoordinates, 1, 0);
    ParamByName('SatCount').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
    ParamByName('Engine').AsInteger := IfThen(DevicePoint.EngineWorks, 1, 0);
    ParamByName('Latitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
    ParamByName('Longitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
    ParamByName('Altitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Altitude;
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

    ExecSQL();

    Changed := False;
  end;

  with (AObj as TTrackPoint), FQueryInsertSensorRaw do
  begin
    Close;

    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Raw').AsString := DevicePoint.SensorsJsonString;

    ExecSQL();
  end;

  with (AObj as TTrackPoint), FQueryUpdateLastInfo do
  begin
    Close;

    ParamByName('SATCOUNT').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
    ParamByName('SATCOUNT2').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
    ParamByName('LATITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
    ParamByName('LATITUDE2').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
    ParamByName('LONGITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
    ParamByName('LONGITUDE2').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
    ParamByName('SPEED').AsFloat := DevicePoint.Velocity;
    ParamByName('SPEED2').AsFloat := DevicePoint.Velocity;
    ParamByName('AZIMUTH').AsFloat := DevicePoint.Azimuth;
    ParamByName('AZIMUTH2').AsFloat := DevicePoint.Azimuth;
    ParamByName('TYPE').AsInteger := CPointType[PointType].Code;
    ParamByName('TYPE2').AsInteger := CPointType[PointType].Code;
    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('DT2').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;
    ParamByName('Recieved2').AsDateTime := DevicePoint.RecievedDateTime;

    ExecSQL();
  end;


end;

procedure TTrackAnalyzerCache.ExecDBUpdate(const AObj: IDataObj);
begin
  with (AObj as TTrackPoint), FQueryUpdate do
  begin
    Close;

    ParamByName('Imei').AsLargeInt := DevicePoint.DeviceID;
    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
    ParamByName('Type').AsInteger := CPointType[PointType].Code;

    ParamByName('Speed').AsFloat := IfThen(IsInfinite(SpeedKmh), 0, SpeedKmh);
    ParamByName('Azimuth').AsFloat := IfThen(IsInfinite(Azimuth), 0, Azimuth);
    ParamByName('Distance').AsFloat := IfThen(IsInfinite(Distance), 0, Distance);
    ParamByName('Duration').AsFloat := IfThen(IsInfinite(Duration), 0, Duration);
    ParamByName('AngularSpeed').AsFloat := IfThen(IsInfinite(AngularSpeed), 0, AngularSpeed);
    ParamByName('RadialAcc').AsFloat := IfThen(IsInfinite(RadialAcceleration), 0, RadialAcceleration);
    ParamByName('TangentialAcc').AsFloat := IfThen(IsInfinite(TangentialAcceleration), 0, TangentialAcceleration);

    ExecSQL();

    Changed := False;
  end;
end;

function TTrackAnalyzerCache.MakeObjFromReadReq(
  const AQuery: TZQuery): IDataObj;
var
  sensors: TSensorsRawArray;
begin
  SetLength(sensors, 0);
  with AQuery do
  begin
    Result := TTrackPoint.Create(
      TDevicePoint.Create(
        TGeoTrackPoint.Create(
          FieldByName('Latitude').AsFloat,
          FieldByName('Longitude').AsFloat,
          FieldByName('DT').AsDateTime,
          FieldByName('SatCount').AsInteger,
          FieldByName('Altitude').AsFloat
        ),
        sensors,
        FieldByName('IMEI').AsLargeInt,
        FieldByName('Valid').AsInteger = 1,
        FieldByName('Recieved').AsDateTime,
        FieldByName('Engine').AsInteger = 1
      ),
      poTracker,
      FieldByName('Distance').AsFloat,
      FieldByName('Duration').AsFloat,
      FieldByName('Azimuth').AsFloat,
      FieldByName('Speed').AsFloat,
      FieldByName('TangentialAcc').AsFloat,
      FieldByName('RadialAcc').AsFloat,
      FieldByName('AngularSpeed').AsFloat,
      GetPointTypeByCode(FieldByName('Type').AsInteger)
    );
  end;
end;

procedure TTrackAnalyzerCache.PushPicture(const AObj: IDataObj);
begin
  with (AObj as TTrackPicture), FQueryInsertPicture do
  begin
    Close;

    ParamByName('Imei').AsLargeInt := Imei;
    ParamByName('DT').AsDateTime := DateTime;
    ParamByName('Recieved').AsDateTime := Recieved;
    ParamByName('Picture').AsBlob := Picture;

    ExecSQL();
  end;
end;

end.


