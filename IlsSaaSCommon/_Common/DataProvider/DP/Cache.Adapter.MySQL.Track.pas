unit Cache.Adapter.MySQL.Track;

interface

uses
  Windows, SysUtils, Registry, Data.DB, Data.Win.ADODB, uMoveEvents,
  uTrackPoints, Winapi.ActiveX, Generics.Collections,
  DateUtils, Math, StrUtils, Ils.Logger, System.IniFiles, Ils.Utils,
  ZAbstractConnection, ZConnection, ZAbstractRODataset, ZAbstractDataset,
  ZDataset, ZDbcIntfs, Ils.MySql.Conf, Vcl.ExtCtrls, Ils.Utils.Svc, Geo.Pos,
  Event.Cnst, Cache.Root, Adapter.MySQL, JSonDataObjects,
  Ils.JSon.Utils, Ils.JSon.Names, synacode, uSensors;

type

  TTrackPointOrigin = (poUnknown, poTracker, poBulkInsert, poDatabaseTemp, poDatabase, poDuplicate);

  TTrackObject = class(TCacheDataObjectAbstract)
  end;

  TTrackPicture = class;

  TTrackPoint = class;

  TTrackObjectFactory = class
    class function Parse(const AStr: string; const AOffset: Int64): TCacheDataObjectAbstract;
  end;

  TTrackPicture = class(TTrackObject)
  private
    FImei: Int64;
    FPicture: TBytes;
    FDateTime: TDateTime;
    FRecieved: TDateTime;
    FOffset: Int64;
  public
    property Imei: Int64 read FImei;
    property Picture: TBytes read FPicture;
    property DateTime: TDateTime read FDateTime;
    property Recieved: TDateTime read FRecieved;
    property Offset: Int64 read FOffset;
    constructor Create(const AJsonObject: TJsonObject; const AOffset: Int64); overload;
    function AsBinary: TBytes;
  end;

//  TTrackPoint = class(TDataObj)
  TTrackPoint = class(TTrackObject)
  private
    FDevicePoint: TDevicePoint;
    FDistance: Double;
    FDuration: Double;
    FAzimuth: Double;
    FSpeedMs: Double;
    FTangentialAcceleration: Double;
    FRadialAcceleration: Double;
    FAngularSpeed: Double;
    FPointType: TPointType;
    FChecksPassed: TPointCheckSet;
    FCalculatedPhysics: TCalculatedPhysicSet;
    FChanged: Boolean;
    FOrigin: TTrackPointOrigin;
    FOffset: Int64;
    FFaultSet: TFaults;
    FOriginalJson: string;
    procedure SetAngularSpeed(const Value: Double);
    procedure SetAzimuth(const Value: Double);
    procedure SetCalculatedPhysics(const Value: TCalculatedPhysicSet);
    procedure SetChanged(const Value: Boolean);
    procedure SetDistance(const Value: Double);
    procedure SetRadialAcceleration(const Value: Double);
    procedure SetSpeedMs(const Value: Double);
    procedure SetSpeedKmh(const Value: Double);
    function GetSpeedKmh: Double;
//    procedure SetStatus(const Value: TPointStatus);
    procedure SetChecksPassed(const Value: TPointCheckSet);
    procedure SetTangentialAcceleration(const Value: Double);
    procedure SetDevicePoint(const Value: TDevicePoint);
    procedure SetOrigin(const Value: TTrackPointOrigin);
    procedure SetPointType(const Value: TPointType);
    procedure SetDuration(const Value: Double);
  protected
    constructor Create(tp: TTrackPoint); overload;
    function GetDTMark(): TDateTime; override;
  public
    property DevicePoint: TDevicePoint read FDevicePoint write SetDevicePoint;
    property Distance: Double read FDistance write SetDistance;
    property Azimuth: Double read FAzimuth write SetAzimuth;
    property SpeedMs: Double read FSpeedMs write SetSpeedMs;
    property SpeedKmh: Double read GetSpeedKmh write SetSpeedKmh;
    property TangentialAcceleration: Double read FTangentialAcceleration write SetTangentialAcceleration;
    property RadialAcceleration: Double read FRadialAcceleration write SetRadialAcceleration;
    property AngularSpeed: Double read FAngularSpeed write SetAngularSpeed;
    property ChecksPassed: TPointCheckSet read FChecksPassed write SetChecksPassed;
    property CalculatedPhysics: TCalculatedPhysicSet read FCalculatedPhysics write SetCalculatedPhysics;
    property Changed: Boolean read FChanged write SetChanged;
    property Origin: TTrackPointOrigin read FOrigin write SetOrigin;
    property PointType: TPointType read FPointType write SetPointType;
    property Offset: Int64 read FOffset;
    property FaultSet: TFaults read FFaultSet write FFaultSet;
    property Duration: Double read FDuration write SetDuration;
    procedure Assign(tp: TTrackPoint);
    function Clone: TCacheDataObjectAbstract; override;
    function AsJsonObject: TJsonObject;

    constructor Create(
      const APoint: TDevicePoint;
      const AOrigin: TTrackPointOrigin;
      const ADistance: Double;
      const ADuration: Double;
      const AAzimuth: Double;
      const ASpeedKmh: Double;
      const ATangentialAcceleration: Double;
      const ARadialAcceleration: Double;
      const AAngularSpeed: Double;
      const APointType: TPointType); overload;

    constructor Create(const APoint: TDevicePoint; const AOrigin: TTrackPointOrigin; const APointType: TPointType); overload;
    constructor Create(const APoint: TDevicePoint; const AGeneration: string; const APointType: TPointType = ptUnknown; const AOffset: Int64 = -1); overload;
    constructor Create(const AJsonString: string; AOffset: Int64); overload;
    constructor Create(const AJsonObject: TJsonObject; const AOffset: Int64); overload;
  end;

//  TTrackAnalyzerCache = class(TCacheAdapterMySQL)
//  TTrackPointCacheDataAdapter = class(TCacheAdapterMySQL)
//  protected
//    FQueryInsertSensorRaw: TZQuery;
//    FQueryUpdateLastInfo: TZQuery;
//    FQueryInsertPicture: TZQuery;
//    //! вставить запись в БД
//    procedure ExecDBInsert(
//      const AObj: IDataObj
//    ); override;
//    //! обновить запись в БД
//    procedure ExecDBUpdate(
//      const AObj: IDataObj
//    ); override;
//    //! преобразователь из записи БД в объект
//    function MakeObjFromReadReq(
//      const AQuery: TZQuery
//    ): IDataObj; override;
//  public
//    procedure PushPicture(const AObj: IDataObj);
//    constructor Create(
//      const AIMEI: Int64;
//      const AReadConnection: TZConnection;
//      const AWriteConnection: TZConnection;
//      const ACacheCapacityLimit: Integer;
//      const ACacheInterval: Double
//    );
//    destructor Destroy; override;
//  end;


  TTrackPointCacheDataAdapter = class(TCacheAdapterMySQL)
  private
    FQueryInsertSensorRaw: TZQuery;
    FQueryUpdateLastInfo: TZQuery;
    FQueryInsertPicture: TZQuery;
  public
    //! обновить все изменённые объекты
    procedure Flush(
      const ACache: TCache
    ); override;
    //! изменение объекта
    procedure Action(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract;
      const AIsAdded: Boolean
    ); override;
    //! подгрузить данные из источника
    procedure LoadRange(
      const ACache: TCache;
      const AFrom: TDateTime;
      const ATo: TDateTime;
      var RObjects: TCacheDataObjectAbstractArray
    ); override;

    function MakeObjFromReadQuery(
      const AQuery: TZQuery
    ): TCacheDataObjectAbstract; override;

    procedure PushPicture(const AObj: TCacheDataObjectAbstract);
    constructor Create(
      const AReadConnection, AWriteConnection: TZConnection
    );
    destructor Destroy;

  end;

implementation

{ TTrackObjectFactory }

class function TTrackObjectFactory.Parse(const AStr: string; const AOffset: Int64): TCacheDataObjectAbstract;
var
  Json: TJsonObject;
begin
  Json := nil;
  Result := nil;
  try
    try
      Json := TJsonObject.Parse(AStr) as TJsonObject;

      if IsCameraMessage(Json) then
        Result := TTrackPicture.Create(Json, AOffset)
      else
      begin
        Result := TTrackPoint.Create(Json, AOffset);
        if TTrackPoint(Result).PointType = ptInvalid then
          FreeAndNil(Result);
      end;
    except
      on E: Exception do begin
        ToLog('Ошибка парсинга точки: ' + E.Message);
        Exit;
      end;
    end;
  finally
    Json.Free;
  end;

end;

{ TTrackPicture }

function TTrackPicture.AsBinary: TBytes;
begin
//  Result := DecodeBase64(AnsiString(FJson.O[JF2Str(jfSensors)].S[MakeSensorName(stCamera)]));
end;

constructor TTrackPicture.Create(const AJsonObject: TJsonObject; const AOffset: Int64);
begin
  FOffset := AOffset;
  FImei := AJsonObject[JF2Str(jfImei)].LongValue;
  FDateTime := IlsToDateTime(AJsonObject.S[JF2Str(jfDateTime)]);
  if AJsonObject[JF2Str(jfReceived)].IsNull then
    FRecieved := FDateTime
  else
    FRecieved := IlsToDateTime(AJsonObject.S[JF2Str(jfReceived)]);

  FPicture := BytesOf(DecodeBase64(AnsiString(AJsonObject.Path[JF2Str(jfSensors) + '.' + MakeSensorName(stCamera)].Value)));
end;

{ TTrackPoint }

function TTrackPoint.AsJsonObject: TJsonObject;
var
  SensorsJson: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.S[CJsonField[jfGeneration]] := DevicePoint.Generation;
  Result.I[CJsonField[jfDeviceType]] := DevicePoint.DeviceType;
  Result.U[CJsonField[jfImei]] := DevicePoint.DeviceID;
  Result[CJsonField[jfReceived]] := DateTimeToIls(DevicePoint.RecievedDateTime);
  Result[CJsonField[jfDateTime]] := DateTimeToIls(DevicePoint.GeoTrackPoint.DateTime);
  Result.I[CJsonField[jfEngineWorks]] := Ord(DevicePoint.EngineWorks);
  Result.I[CJsonField[jfSatellitesValid]] := Ord(DevicePoint.ValidCoordinates);
  Result.I[CJsonField[jfSatellitesCount]] := DevicePoint.GeoTrackPoint.SatellitesCount;
  Result.F[CJsonField[jfLatitude]] := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
  Result.F[CJsonField[jfLongitude]] := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
  Result.F[CJsonField[jfAltitude]] := DevicePoint.GeoTrackPoint.Pos.Altitude;
  if DevicePoint.Distance <> NegInfinity then
    Result.F[CJsonField[jfLen]] := DevicePoint.Distance;

  if DevicePoint.Velocity <> NegInfinity then
    Result.F[CJsonField[jfDeviceVelocity]] := DevicePoint.Velocity;

  if SpeedKmh <> NegInfinity then
    Result.F[CJsonField[jfVelocity]] := SpeedKmh;

  if not IsInfinite(DevicePoint.Azimuth) then
    Result.F[CJsonField[jfDeviceAzimuth]] := DevicePoint.Azimuth;

  if not IsInfinite(Azimuth) then
    Result.F[CJsonField[jfAzimuth]] := Azimuth;

  SensorsJson := TJsonObject.Parse(DevicePoint.SensorsJsonString) as TJsonObject;
  Result.O[CJsonField[jfSensors]] := SensorsJson;
end;

procedure TTrackPoint.Assign(tp: TTrackPoint);
var
  i: Integer;
begin
  FDevicePoint.Generation := tp.FDevicePoint.Generation;
  FDevicePoint.GeoTrackPoint := tp.FDevicePoint.GeoTrackPoint;
  FDevicePoint.DeviceID := tp.FDevicePoint.DeviceID;
  FDevicePoint.ValidCoordinates := tp.FDevicePoint.ValidCoordinates;
  FDevicePoint.EngineWorks := tp.FDevicePoint.EngineWorks;
  FDevicePoint.RecievedDateTime := tp.FDevicePoint.RecievedDateTime;
  FDevicePoint.DeviceType := tp.FDevicePoint.DeviceType;
  FDevicePoint.SensorsJsonString := tp.FDevicePoint.SensorsJsonString;
  FDevicePoint.Velocity := tp.FDevicePoint.Velocity;
  FDevicePoint.Distance := tp.FDevicePoint.Distance;
  FDevicePoint.Azimuth := tp.FDevicePoint.Azimuth;
  SetLength(FDevicePoint.Sensors, Length(tp.FDevicePoint.Sensors));
  for i := 0 to Length(FDevicePoint.Sensors) - 1 do
    FDevicePoint.Sensors[i] := tp.FDevicePoint.Sensors[i];

  FOffset := tp.FOffset;
  FPointType := tp.FPointType;
  FDistance := tp.FDistance;
  FDuration := tp.FDuration;
  FAzimuth := tp.FAzimuth;
  FSpeedMs := tp.FSpeedMs;
  FTangentialAcceleration := tp.FTangentialAcceleration;
  FRadialAcceleration := tp.FRadialAcceleration;
  FAngularSpeed := tp.FAngularSpeed;
//  FStatus := tp.FStatus;
  FChecksPassed := tp.FChecksPassed;
  FCalculatedPhysics := tp.FCalculatedPhysics;
  FChanged := tp.FChanged;
  FOrigin := tp.FOrigin;
  FOriginalJson := tp.FOriginalJson;
end;

constructor TTrackPoint.Create(const APoint: TDevicePoint; const AOrigin: TTrackPointOrigin; const APointType: TPointType);
begin
  FDevicePoint := APoint;
  FCalculatedPhysics := [];
  FFaultSet := [];

  FChanged := False;
  FOrigin := AOrigin;

  FDistance := NegInfinity;
  FDuration := NegInfinity;
  FAzimuth := NegInfinity;
  FSpeedMs := NegInfinity;
  FTangentialAcceleration := NegInfinity;
  FRadialAcceleration := NegInfinity;
  FAngularSpeed := NegInfinity;
  FPointType := APointType;
  FChecksPassed := [];
  Origin := poTracker;
end;

function TTrackPoint.Clone: TCacheDataObjectAbstract;
begin
  Result := TTrackPoint.Create(Self);
end;

constructor TTrackPoint.Create(tp: TTrackPoint);
begin
  Assign(tp);
end;

procedure TTrackPoint.SetAngularSpeed(const Value: Double);
begin
  if not SameValue(FAngularSpeed, Value, CTolerance) then
  begin
//    FChanged := True;
    FAngularSpeed := Value;
  end;
  Include(FCalculatedPhysics, pcsAngularSpeed);
end;

procedure TTrackPoint.SetAzimuth(const Value: Double);
begin
  if not SameValue(FAzimuth, Value, CTolerance) then
  begin
//    FChanged := True;
    FAzimuth := Value;
  end;
  Include(FCalculatedPhysics, pcsAzimuth);
end;

procedure TTrackPoint.SetCalculatedPhysics(const Value: TCalculatedPhysicSet);
begin
  if FCalculatedPhysics <> Value then
  begin
//    FChanged := True;
    FCalculatedPhysics := Value;
  end;
end;

procedure TTrackPoint.SetDevicePoint(const Value: TDevicePoint);
begin
  if FDevicePoint <> Value then
  begin
//    FChanged := True;
    FDevicePoint := Value;
  end;
end;

procedure TTrackPoint.SetDistance(const Value: Double);
begin
  if not SameValue(FDistance, Value, CTolerance) then
  begin
    FChanged := True;
    FDistance := Value;
  end;
  Include(FCalculatedPhysics, pcsDistance);
end;

procedure TTrackPoint.SetDuration(const Value: Double);
begin
  if not SameValue(FDuration, Value, CTolerance) then
  begin
    FChanged := True;
    FDuration := Value;
  end;
  Include(FCalculatedPhysics, pcsDuration);
end;

procedure TTrackPoint.SetOrigin(const Value: TTrackPointOrigin);
begin
  if FOrigin <> Value then
  begin
    if Value in [poDatabaseTemp, poDatabase] then
      FChanged := False;

    FOrigin := Value;
  end;
end;

procedure TTrackPoint.SetPointType(const Value: TPointType);
begin
  if FPointType <> Value then
  begin
    FChanged := True;
    FPointType := Value;
  end;
end;

procedure TTrackPoint.SetRadialAcceleration(const Value: Double);
begin
  if not SameValue(FRadialAcceleration, Value, CTolerance) then
  begin
    FChanged := True;
    FRadialAcceleration := Value;
  end;
  Include(FCalculatedPhysics, pcsRadialAcceleration);
end;

procedure TTrackPoint.SetSpeedMs(const Value: Double);
begin
  if not SameValue(FSpeedMs, Value, CTolerance) then
  begin
    FChanged := True;
    FSpeedMs := Value;
  end;
  Include(FCalculatedPhysics, pcsSpeed);
end;

procedure TTrackPoint.SetSpeedKmh(const Value: Double);
begin
  SpeedMs := Value * CKmhToMps;
end;

function TTrackPoint.GetDTMark: TDateTime;
begin
  Result := DevicePoint.GeoTrackPoint.DateTime;
end;

function TTrackPoint.GetSpeedKmh: Double;
begin
  Result := NegInfinity;
  if SpeedMs = NegInfinity then
    Exit;

  Result := SpeedMs * CMpsToKmh;
end;

procedure TTrackPoint.SetChecksPassed(const Value: TPointCheckSet);
begin
  if FChecksPassed <> Value then
  begin
//    FChanged := True;
    FChecksPassed := Value;
  end;
end;

procedure TTrackPoint.SetTangentialAcceleration(const Value: Double);
begin
  if not SameValue(FTangentialAcceleration, Value, CTolerance) then
  begin
    FChanged := True;
    FTangentialAcceleration := Value;
  end;
  Include(FCalculatedPhysics, pcsTangentialAcceleration);
end;

procedure TTrackPoint.SetChanged(const Value: Boolean);
begin
  FChanged := Value;
end;

constructor TTrackPoint.Create(const APoint: TDevicePoint; const AGeneration: string; const APointType: TPointType = ptUnknown; const AOffset: Int64 = -1);
begin
  FDevicePoint := APoint;
  FDevicePoint.Generation := AGeneration;
  FCalculatedPhysics := [];
  FFaultSet := [];

  FOrigin := poUnknown;

  FTangentialAcceleration := NegInfinity;
  FRadialAcceleration := NegInfinity;
  FAngularSpeed := NegInfinity;
  FDistance := NegInfinity;
  FPointType := APointType;
  FChecksPassed := [];
  Origin := poTracker;
  FOffset := AOffset;
  FChanged := False;
end;

constructor TTrackPoint.Create(const AJsonString: string; AOffset: Int64);
var
  PointJSONObject: TJSONObject;
begin
  PointJSONObject := nil;
  try
    PointJSONObject := TJSONObject.Parse(AJsonString) as TJsonObject;
    Create(PointJSONObject, AOffset);
  except
    on E: Exception do
      ToLog('Ошибка парсинга JSON: ' + E.Message)
  end;
  PointJSONObject.Free;
end;

constructor TTrackPoint.Create(const AJsonObject: TJsonObject; const AOffset: Int64);
var
  SensorsJSONObject: TJSONObject;
  SensorValue: TJsonDataValueHelper;
  SensorName: string;
  SensorMeasure: TSensorMeasure;
  SensorNumber: Integer;
  SensorsCount: Integer;
  i, j: Integer;
  Generation: string;
  Latitude, Longitude: Double;
  DateTime: TDateTime;
  SatCount: Byte;
  CoordValid: Boolean;
  DeviceID: UInt64;
  DeviceType: Byte;
  Len: Double;
  Speed: Double;
  Azimuth: Double;
  Altitude: Double;
  RecievedTime: TDateTime;
  EngineWorks: Boolean;
  Sensors: TSensorsRawArray;
  SensorIdx: Integer;
  SensorsJsonString: string;
  jf: TJsonField;
begin
  SensorsJSONObject := nil;
  CoordValid := False;
  SatCount := 0;
  Len := NegInfinity;
  Speed := NegInfinity;
  Altitude := 0;
  Azimuth := 0;
  RecievedTime := -1;
  EngineWorks := False;
  DateTime := 0;
  Latitude := NegInfinity;
  Longitude := NegInfinity;
  DeviceID := 0;

  if not Assigned(AJsonObject) then
  begin
    PointType := ptInvalid;
    Exit;
  end;

  FOriginalJson := AJsonObject.ToJSON();

  try
    if AJsonObject.Values[JF2Str(jfImei)].IsNull
        or (AJsonObject.Values[JF2Str(jfSatellitesCount)].IsNull and AJsonObject.Values[JF2Str(jfSatellitesValid)].IsNull)
        or AJsonObject.Values[JF2Str(jfDateTime)].IsNull
        or AJsonObject.Values[JF2Str(jfLatitude)].IsNull
        or AJsonObject.Values[JF2Str(jfLongitude)].IsNull then
    begin
      ToLog('Оффсет: ' + IntToStr(AOffset) + ': Ошибка парсинга JSON: отсутствует одно из обязательных полей(' + AJsonObject.ToJSON + ')');
      PointType := ptInvalid;
      Exit;
    end;

    Generation := '';

    for jf := Succ(Low(TJsonField)) to High(TJsonField) do
      with AJsonObject.Values[JF2Str(jf)] do
        if not IsNull then
          case jf of
            jfGeneration:
              Generation := Value;
            jfImei:
              DeviceID := ULongValue;
            jfSatellitesCount:
              SatCount := IntValue;
            jfSatellitesValid:
              CoordValid := BoolValue;
            jfDeviceType:
              DeviceType := IntValue;
            jfLatitude:
              Latitude := FloatValue;
            jfLongitude:
              Longitude := FloatValue;
            jfDateTime:
              DateTime := IlsToDateTime(Value);
            jfLen:
              Len := FloatValue;
            jfVelocity:
              Speed := FloatValue;
            jfAzimuth:
              Azimuth := FloatValue;
            jfAltitude:
              Altitude := FloatValue;
            jfSensors:
              SensorsJSONObject := AJsonObject.O[JF2Str(jf)];
            jfReceived:
              RecievedTime := IlsToDateTime(Value);
            jfEngineWorks:
              EngineWorks := BoolValue;
          end;

    RecievedTime := IfThen(RecievedTime < 0, DateTime, RecievedTime);
    SensorsJsonString := SensorsJSONObject.ToJSON;
    SensorsCount := 0;
    for i := 0 to SensorsJSONObject.Count - 1 do
      case StrToIntDef(Generation, 1) of
        1: begin
//          if SensorsJSONObject.Names[i] = MakeSensorName(stBin8) then
          if smBinary8 = GetSensorMeasure(SensorsJSONObject.Names[i]) then
            Inc(SensorsCount, 8)
          else
            Inc(SensorsCount);
        end;
        2: begin
          if smBinary8 = GetSensorMeasure(SensorsJSONObject.Names[i]) then
            Inc(SensorsCount, 8)
          else
            Inc(SensorsCount);
        end;
      end;

    SetLength(Sensors, SensorsCount);
    SensorNumber := 0;
    for i := 0 to SensorsJSONObject.Count - 1 do
    begin
      SensorName := SensorsJSONObject.Names[i];
      SensorMeasure := GetSensorMeasure(SensorName);
      SensorValue := SensorsJSONObject.Values[SensorName];

      case SensorMeasure of
        smBinary:
          begin
            Sensors[SensorNumber].Value.SensorType := stBit;
            Sensors[SensorNumber].Value.AsBit := (SensorValue.IntValue = 1);
            Inc(SensorNumber)
          end;
        smBinary8:
          begin
            for j := 0 to 7 do
            begin
              Sensors[SensorNumber + j].Id := j + 1;
              Sensors[SensorNumber + j].Name := SensorName + '.' + IntToStr(j);
              Sensors[SensorNumber + j].Value.SensorType := stBit;
              Sensors[SensorNumber + j].Value.AsBit := ((SensorValue.IntValue) and (1 shl j)) = 1;
            end;
            Inc(SensorNumber, 8);
          end;
        smInteger:
          begin
            Sensors[SensorNumber].Name := SensorName;
            Sensors[SensorNumber].Value.SensorType := stInteger;
            Sensors[SensorNumber].Value.AsInteger := SensorValue.IntValue;
            Inc(SensorNumber)
          end;
        smFloat:
          begin
            Sensors[SensorNumber].Name := SensorName;
            Sensors[SensorNumber].Value.SensorType := stDouble;
            Sensors[SensorNumber].Value.AsDouble := SensorValue.FloatValue;
            Inc(SensorNumber)
          end;
        smRaw:
          ;
      else
        begin
        end;
      end;
    end;

    Create(
      TDevicePoint.Create(
        TGeoTrackPoint.Create(
          Latitude,
          Longitude,
          DateTime,
          SatCount,
          Altitude
        ),
        Sensors,
        DeviceID,
        CoordValid or (AJsonObject.Values[JF2Str(jfSatellitesCount)].IsNull and (SatCount >= 4)),
        RecievedTime,
        EngineWorks,
        SensorsJsonString,
        Len,
        Speed,
        Azimuth,
        DeviceType
      ),
      Generation,
      ptUnknown,
      AOffset
    );
  except
    on E: Exception do
    begin
      PointType := ptInvalid;
      ToLog('Ошибка создания точки: ' + E.Message + '; оффсет: ' + IntToStr(AOffset));
    end;
  end;
end;

constructor TTrackPoint.Create(const APoint: TDevicePoint;
  const AOrigin: TTrackPointOrigin; const ADistance, ADuration, AAzimuth,
  ASpeedKmh, ATangentialAcceleration, ARadialAcceleration, AAngularSpeed: Double;
  const APointType: TPointType);
begin
  FDevicePoint := APoint;
  FCalculatedPhysics := [];
  FFaultSet := [];
  FChecksPassed := [];

  FOrigin := AOrigin;

  FDistance := ADistance;
  FDuration := ADuration;
  FAzimuth := AAzimuth;
  SpeedKmh := ASpeedKmh;
  FTangentialAcceleration := ATangentialAcceleration;
  FRadialAcceleration := ARadialAcceleration;
  FAngularSpeed := AAngularSpeed;
  FPointType := APointType;
  FChanged := False;
end;



{ TTrackPointCacheDataAdapter }

procedure TTrackPointCacheDataAdapter.Action(const ACache: TCache;
  const AObj: TCacheDataObjectAbstract; const AIsAdded: Boolean);
begin
  inherited;
  with TTrackPoint(AObj) do
  begin
    if AIsAdded then
    begin
      FillParamsFromKey(ACache.Key, FQueryInsert);
      FQueryInsert.ParamByName(CSQLParamDT).AsDateTime := DTMark;

      FQueryInsert.ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;
      FQueryInsert.ParamByName('Offset').AsLargeInt := Offset;
      FQueryInsert.ParamByName('Valid').AsInteger := IfThen(DevicePoint.ValidCoordinates, 1, 0);
      FQueryInsert.ParamByName('SatCount').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
      FQueryInsert.ParamByName('Engine').AsInteger := IfThen(DevicePoint.EngineWorks, 1, 0);
      FQueryInsert.ParamByName('Latitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
      FQueryInsert.ParamByName('Longitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
      FQueryInsert.ParamByName('Altitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Altitude;
      FQueryInsert.ParamByName('Type').AsInteger := CPointType[PointType].Code;
      FQueryInsert.ParamByName('DeviceSpeed').AsFloat := IfThen(IsInfinite(DevicePoint.Velocity), 0, DevicePoint.Velocity);
      FQueryInsert.ParamByName('DeviceAzimuth').AsFloat := IfThen(IsInfinite(DevicePoint.Azimuth), 0, DevicePoint.Azimuth);
      FQueryInsert.ParamByName('Speed').AsFloat := IfThen(IsInfinite(SpeedKmh), 0, SpeedKmh);
      FQueryInsert.ParamByName('Azimuth').AsFloat := IfThen(IsInfinite(Azimuth), 0, Azimuth);
      FQueryInsert.ParamByName('Distance').AsFloat := IfThen(IsInfinite(Distance), 0, Distance);
      FQueryInsert.ParamByName('Duration').AsFloat := IfThen(IsInfinite(Duration), 0, Duration);
      FQueryInsert.ParamByName('AngularSpeed').AsFloat := IfThen(IsInfinite(AngularSpeed), 0, AngularSpeed);
      FQueryInsert.ParamByName('RadialAcc').AsFloat := IfThen(IsInfinite(RadialAcceleration), 0, RadialAcceleration);
      FQueryInsert.ParamByName('TangentialAcc').AsFloat := IfThen(IsInfinite(TangentialAcceleration), 0, TangentialAcceleration);
      FQueryInsert.ExecSQL();

      FQueryInsertSensorRaw.Close;
      FQueryInsertSensorRaw.ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
      FQueryInsertSensorRaw.ParamByName('Raw').AsString := DevicePoint.SensorsJsonString;
      FQueryInsertSensorRaw.ExecSQL();


      FQueryUpdateLastInfo.Close;
      FQueryUpdateLastInfo.ParamByName('SATCOUNT').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
      FQueryUpdateLastInfo.ParamByName('SATCOUNT2').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
      FQueryUpdateLastInfo.ParamByName('LATITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
      FQueryUpdateLastInfo.ParamByName('LATITUDE2').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
      FQueryUpdateLastInfo.ParamByName('LONGITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
      FQueryUpdateLastInfo.ParamByName('LONGITUDE2').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
      FQueryUpdateLastInfo.ParamByName('SPEED').AsFloat := DevicePoint.Velocity;
      FQueryUpdateLastInfo.ParamByName('SPEED2').AsFloat := DevicePoint.Velocity;
      FQueryUpdateLastInfo.ParamByName('AZIMUTH').AsFloat := DevicePoint.Azimuth;
      FQueryUpdateLastInfo.ParamByName('AZIMUTH2').AsFloat := DevicePoint.Azimuth;
      FQueryUpdateLastInfo.ParamByName('TYPE').AsInteger := CPointType[PointType].Code;
      FQueryUpdateLastInfo.ParamByName('TYPE2').AsInteger := CPointType[PointType].Code;
      FQueryUpdateLastInfo.ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
      FQueryUpdateLastInfo.ParamByName('DT2').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
      FQueryUpdateLastInfo.ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;
      FQueryUpdateLastInfo.ParamByName('Recieved2').AsDateTime := DevicePoint.RecievedDateTime;
      FQueryUpdateLastInfo.ExecSQL();
    end
    else
    begin
      FQueryUpdate.Close;

      FillParamsFromKey(ACache.Key, FQueryUpdate);
      FQueryUpdate.ParamByName(CSQLParamDT).AsDateTime := DTMark;

      FQueryUpdate.ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
      FQueryUpdate.ParamByName('Type').AsInteger := CPointType[PointType].Code;

      FQueryUpdate.ParamByName('Speed').AsFloat := IfThen(IsInfinite(SpeedKmh), 0, SpeedKmh);
      FQueryUpdate.ParamByName('Azimuth').AsFloat := IfThen(IsInfinite(Azimuth), 0, Azimuth);
      FQueryUpdate.ParamByName('Distance').AsFloat := IfThen(IsInfinite(Distance), 0, Distance);
      FQueryUpdate.ParamByName('Duration').AsFloat := IfThen(IsInfinite(Duration), 0, Duration);
      FQueryUpdate.ParamByName('AngularSpeed').AsFloat := IfThen(IsInfinite(AngularSpeed), 0, AngularSpeed);
      FQueryUpdate.ParamByName('RadialAcc').AsFloat := IfThen(IsInfinite(RadialAcceleration), 0, RadialAcceleration);
      FQueryUpdate.ParamByName('TangentialAcc').AsFloat := IfThen(IsInfinite(TangentialAcceleration), 0, TangentialAcceleration);

      FQueryUpdate.ExecSQL();

    end;
  end;
//  if not AIsAdded then
//    TEventWaypoint(AObj).Origin := ooLoaded;
//  Flush(ACache);
end;

constructor TTrackPointCacheDataAdapter.Create(
  const AReadConnection, AWriteConnection: TZConnection
);
//constructor TTrackPointCacheDataAdapter.Create(
//  const AIMEI: Int64;
//  const AReadConnection: TZConnection;
//  const AWriteConnection: TZConnection;
//  const ACacheCapacityLimit: Integer;
//  const ACacheInterval: Double);
const
  CSQLReadRange = 'SELECT *'
    + ' FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLReadBefore = 'SELECT *'
    + ' FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CSQLReadAfter = 'SELECT *'
    + ' FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
//  CReqInsert = 'INSERT IGNORE INTO track'
//    + ' (IMEI, DT, EventTypeID, Duration, Latitude, Longitude, Radius, Dispersion)'
//    + ' VALUES (:imei, :dt, :event_type, :dur, :la, :lo, :radius, :dispersion)';
  CSQLInsert =
    'INSERT IGNORE INTO Track (' +
    ' Imei, DT, Recieved, Offset, Valid, SatCount, Engine,' +
    ' Latitude, Longitude, Altitude, Type, Speed, DeviceSpeed, Azimuth, DeviceAzimuth, Distance,' +
    ' Duration, AngularSpeed, RadialAcc, TangentialAcc) VALUES'+
    '(:Imei, :DT, :Recieved, :Offset, :Valid, :SatCount, :Engine, ' +
    ':Latitude, :Longitude, :Altitude, :Type, :Speed, :DeviceSpeed, :Azimuth, :DeviceAzimuth, :Distance, ' +
    ':Duration, :AngularSpeed, :RadialAcc, :TangentialAcc)';

  CSQLUpdate =
    'UPDATE Track SET DT = DT, Type = :Type, Speed = :Speed,' +
    ' Azimuth = :Azimuth, Distance = :Distance, AngularSpeed = :AngularSpeed,' +
    ' Duration = :Duration, RadialAcc = :RadialAcc, TangentialAcc = :TangentialAcc' +
    ' WHERE Imei = :Imei AND DT = :DT';

  CReqDeleteOne = 'DELETE FROM track'
    + ' WHERE IMEI = :IMEI'
    + ' AND DT = :dt';

  CSQLInsertSensors =
    'INSERT IGNORE INTO SensorRaw (Imei, DT, Raw) VALUES (:Imei, :DT, :Raw)';

  CSQLUpdateLastInfo =
    'INSERT IGNORE INTO trackerlastinfo SET ' +
    '`IMEI` = :IMEI, ' +
    '`SatCount` = :SATCOUNT, `Latitude` = :LATITUDE, ' +
    '`Longitude` = :LONGITUDE, `Speed` = :SPEED, `Azimuth` = :AZIMUTH, ' +
    '`Type` = :TYPE, `DT` = :DT, `Recieved` = :Recieved  ' +
    'ON DUPLICATE KEY UPDATE ' +
    '`SatCount` = :SATCOUNT2, `Latitude` = :LATITUDE2, ' +
    '`Longitude` = :LONGITUDE2, `Speed` = :SPEED2, `Azimuth` = :AZIMUTH2, ' +
    '`Type` = :TYPE2, `DT` = :DT2, `Recieved` = :Recieved2';

  CSQLInsertPicture =
    'INSERT IGNORE INTO trackpicture(IMEI, DT, Recieved, Picture) ' +
    'VALUES(:IMEI, :DT, :Recieved, :Picture)';

begin
  inherited Create(
    AReadConnection, AWriteConnection,
    CSQLInsert, CSQLUpdate, CSQLReadBefore, CSQLReadAfter, CSQLReadRange, CReqDeleteOne);

  FQueryInsertSensorRaw := TZQuery.Create(nil);
  FQueryInsertSensorRaw.Connection := AWriteConnection;
  FQueryInsertSensorRaw.SQL.Text := CSQLInsertSensors;
//  FQueryInsertSensorRaw.ParamByName('imei').AsLargeInt := AIMEI;

  FQueryUpdateLastInfo := TZQuery.Create(nil);
  FQueryUpdateLastInfo.Connection := AWriteConnection;
  FQueryUpdateLastInfo.SQL.Text := CSQLUpdateLastInfo;
//  FQueryUpdateLastInfo.ParamByName('imei').AsLargeInt := AIMEI;

  FQueryInsertPicture := TZQuery.Create(nil);
  FQueryInsertPicture.Connection := AWriteConnection;
  FQueryInsertPicture.SQL.Text := CSQLInsertPicture;
//  FQueryInsertPicture.ParamByName('imei').AsLargeInt := AIMEI;

//  FQueryInsert.ParamByName('imei').AsLargeInt := AIMEI;
//  FQueryUpdate.ParamByName('imei').AsLargeInt := AIMEI;
//  FQueryReadRange.ParamByName('imei').AsLargeInt := AIMEI;
//  FQueryReadBefore.ParamByName('imei').AsLargeInt := AIMEI;
//  FQueryReadAfter.ParamByName('imei').AsLargeInt := AIMEI;
//  FQueryDeleteRange.ParamByName('imei').AsLargeInt := AIMEI;
//  FQueryLastPresentDT.ParamByName('imei').AsLargeInt := AIMEI;
end;

destructor TTrackPointCacheDataAdapter.Destroy;
begin
  FQueryInsertSensorRaw.Free;
  FQueryUpdateLastInfo.Free;
  FQueryInsertPicture.Free;

  inherited;
end;

procedure TTrackPointCacheDataAdapter.Flush(const ACache: TCache);
begin
  inherited;

end;

procedure TTrackPointCacheDataAdapter.LoadRange(const ACache: TCache;
  const AFrom, ATo: TDateTime; var RObjects: TCacheDataObjectAbstractArray);
begin
  inherited;

end;

function TTrackPointCacheDataAdapter.MakeObjFromReadQuery(
  const AQuery: TZQuery): TCacheDataObjectAbstract;
begin

end;

//procedure TTrackPointCacheDataAdapter.ExecDBInsert(const AObj: IDataObj);
//begin
//  with (AObj as TTrackPoint), FQueryInsert do
//  begin
//    Close;
//
//    ParamByName('Imei').AsLargeInt := DevicePoint.DeviceID;
//    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
//    ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;
//    ParamByName('Offset').AsLargeInt := Offset;
//    ParamByName('Valid').AsInteger := IfThen(DevicePoint.ValidCoordinates, 1, 0);
//    ParamByName('SatCount').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
//    ParamByName('Engine').AsInteger := IfThen(DevicePoint.EngineWorks, 1, 0);
//    ParamByName('Latitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
//    ParamByName('Longitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
//    ParamByName('Altitude').AsFloat := DevicePoint.GeoTrackPoint.Pos.Altitude;
//    ParamByName('Type').AsInteger := CPointType[PointType].Code;
//
//    ParamByName('DeviceSpeed').AsFloat := IfThen(IsInfinite(DevicePoint.Velocity), 0, DevicePoint.Velocity);
//    ParamByName('DeviceAzimuth').AsFloat := IfThen(IsInfinite(DevicePoint.Azimuth), 0, DevicePoint.Azimuth);
//
//    ParamByName('Speed').AsFloat := IfThen(IsInfinite(SpeedKmh), 0, SpeedKmh);
//    ParamByName('Azimuth').AsFloat := IfThen(IsInfinite(Azimuth), 0, Azimuth);
//    ParamByName('Distance').AsFloat := IfThen(IsInfinite(Distance), 0, Distance);
//    ParamByName('Duration').AsFloat := IfThen(IsInfinite(Duration), 0, Duration);
//    ParamByName('AngularSpeed').AsFloat := IfThen(IsInfinite(AngularSpeed), 0, AngularSpeed);
//    ParamByName('RadialAcc').AsFloat := IfThen(IsInfinite(RadialAcceleration), 0, RadialAcceleration);
//    ParamByName('TangentialAcc').AsFloat := IfThen(IsInfinite(TangentialAcceleration), 0, TangentialAcceleration);
//
//    ExecSQL();
//
//    Changed := False;
//  end;
//
//  with (AObj as TTrackPoint), FQueryInsertSensorRaw do
//  begin
//    Close;
//
//    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
//    ParamByName('Raw').AsString := DevicePoint.SensorsJsonString;
//
//    ExecSQL();
//  end;
//
//  with (AObj as TTrackPoint), FQueryUpdateLastInfo do
//  begin
//    Close;
//
//    ParamByName('SATCOUNT').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
//    ParamByName('SATCOUNT2').AsInteger := DevicePoint.GeoTrackPoint.SatellitesCount;
//    ParamByName('LATITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
//    ParamByName('LATITUDE2').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
//    ParamByName('LONGITUDE').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
//    ParamByName('LONGITUDE2').AsFloat := DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
//    ParamByName('SPEED').AsFloat := DevicePoint.Velocity;
//    ParamByName('SPEED2').AsFloat := DevicePoint.Velocity;
//    ParamByName('AZIMUTH').AsFloat := DevicePoint.Azimuth;
//    ParamByName('AZIMUTH2').AsFloat := DevicePoint.Azimuth;
//    ParamByName('TYPE').AsInteger := CPointType[PointType].Code;
//    ParamByName('TYPE2').AsInteger := CPointType[PointType].Code;
//    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
//    ParamByName('DT2').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
//    ParamByName('Recieved').AsDateTime := DevicePoint.RecievedDateTime;
//    ParamByName('Recieved2').AsDateTime := DevicePoint.RecievedDateTime;
//
//    ExecSQL();
//  end;
//
//
//end;

//procedure TTrackPointCacheDataAdapter.ExecDBUpdate(const AObj: IDataObj);
//begin
//  with (AObj as TTrackPoint), FQueryUpdate do
//  begin
//    Close;
//
//    ParamByName('Imei').AsLargeInt := DevicePoint.DeviceID;
//    ParamByName('DT').AsDateTime := DevicePoint.GeoTrackPoint.DateTime;
//    ParamByName('Type').AsInteger := CPointType[PointType].Code;
//
//    ParamByName('Speed').AsFloat := IfThen(IsInfinite(SpeedKmh), 0, SpeedKmh);
//    ParamByName('Azimuth').AsFloat := IfThen(IsInfinite(Azimuth), 0, Azimuth);
//    ParamByName('Distance').AsFloat := IfThen(IsInfinite(Distance), 0, Distance);
//    ParamByName('Duration').AsFloat := IfThen(IsInfinite(Duration), 0, Duration);
//    ParamByName('AngularSpeed').AsFloat := IfThen(IsInfinite(AngularSpeed), 0, AngularSpeed);
//    ParamByName('RadialAcc').AsFloat := IfThen(IsInfinite(RadialAcceleration), 0, RadialAcceleration);
//    ParamByName('TangentialAcc').AsFloat := IfThen(IsInfinite(TangentialAcceleration), 0, TangentialAcceleration);
//
//    ExecSQL();
//
//    Changed := False;
//  end;
//end;
//
//function TTrackPointCacheDataAdapter.MakeObjFromReadReq(
//  const AQuery: TZQuery): IDataObj;
//var
//  sensors: TSensorsRawArray;
//begin
//  SetLength(sensors, 0);
//  with AQuery do
//  begin
//    Result := TTrackPoint.Create(
//      TDevicePoint.Create(
//        TGeoTrackPoint.Create(
//          FieldByName('Latitude').AsFloat,
//          FieldByName('Longitude').AsFloat,
//          FieldByName('DT').AsDateTime,
//          FieldByName('SatCount').AsInteger,
//          FieldByName('Altitude').AsFloat
//        ),
//        sensors,
//        FieldByName('IMEI').AsLargeInt,
//        FieldByName('Valid').AsInteger = 1,
//        FieldByName('Recieved').AsDateTime,
//        FieldByName('Engine').AsInteger = 1
//      ),
//      poTracker,
//      FieldByName('Distance').AsFloat,
//      FieldByName('Duration').AsFloat,
//      FieldByName('Azimuth').AsFloat,
//      FieldByName('Speed').AsFloat,
//      FieldByName('TangentialAcc').AsFloat,
//      FieldByName('RadialAcc').AsFloat,
//      FieldByName('AngularSpeed').AsFloat,
//      GetPointTypeByCode(FieldByName('Type').AsInteger)
//    );
//  end;
//end;

procedure TTrackPointCacheDataAdapter.PushPicture(const AObj: TCacheDataObjectAbstract);
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


