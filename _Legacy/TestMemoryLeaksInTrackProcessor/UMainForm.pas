unit UMainForm;

interface

uses
  Vcl.SvcMgr, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  uPointsReciever, uTrackDistributor, Geo.Calcs, uTrackChecker,
  uPointsRecieverKafka, uPointsRecieverPnt, uPointsRecieverHttp,
  System.IniFiles, uTrackDataAdapters, Ils.Logger,
  Data.DB, Data.Win.ADODB, Winapi.ActiveX, uTrackPoints, UConfigJson,
  uTrackAnalyzer, uTrackDataAdaptersMySQL, UPointsRecieverConfigMySql,
  Ils.MySql.EnumPublisher, ZAbstractRODataset, ZAbstractDataset, ZDataset,
  ZAbstractConnection, ZConnection, Ils.MySQL.Conf, Ils.Utils.Debug, JsonDataObjects,
  Ils.Kafka, Ils.Kafka.Settings, Ils.Utils.Svc, WinSvc, StrUtils, Ils.Json.Names,
  Event.Cnst, Ils.Utils, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls;

const
  CServiceName = 'ILSSaaSTrackProcessorService';
  CServiceDisplayName = 'ILS SaaS Track Processor';

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    FPointsReciever: TPointsReciever;
    FKafkaErrorsProducer: TKafkaProducer;
    FKafkaServiceProducer: TKafkaProducer;
    FKafkaEventsProducer: TKafkaProducer;
    procedure Init;
    procedure Shutdown;
    function OnParking(AREvent: TMoveEventSystem; const ATrackPoint: TTrackPoint): Boolean;
    procedure OnFault(APoint: TTrackPoint; AFaults: TFaults);
  public
    { Public declarations }
  end;

const
  CInputSection = 'Input';
  COutputSection = 'Output';
  CAnalyzerSection = 'Analyzer';
  CRecieverSection = 'Reciever';
  CMySQLSection = 'mysql';
  CCacheSection = 'Cache';
  CGeohashSection = 'Geohash';
  CPhysicsChecksSection = 'Physics';
  CGapChecksSection = 'GapFilter';
  CDeviceSection = 'Device';
  CKafkaSection = 'Kafka';
  CHttpSection = 'http';
  CKafkaProcessedSection = 'kafka_processed';
  CKafkaEventsSection = 'kafka_events';
  CPositionFileExt = '.kafka.offset';

var
  MainForm: TMainForm;
  //a: TStringList;

implementation

{$R *.dfm}

{ TForm1 }

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  Init;
  ToLog('started');
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  Shutdown;
  ToLog('stopped');
end;

procedure TMainForm.Init;
const
  CKafkaServiceQueue = 'kafka_service';
var
  IniFile: TIniFile;
  DatabaseConfig: TMySQlDatabaseConfig;
  KafkaOffsetIni: TIniFile;
  KafkaOffset: Int64;
  KafkaRecieverParams: TKafkaConsumerParams;

  KafkaServiceParams: TKafkaProducerParams;
  KafkaProcessedParams: TKafkaProducerParams;
  KafkaEventsParams: TKafkaProducerParams;
begin
  IniFile := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), GetConfigSuffix + '.ini'));
  try
    DatabaseConfig := TMySQLDatabaseConfig.Create(IniFile);

    ToLog(DatabaseConfig.Host + ':' + IntToStr(DatabaseConfig.Port)+'@'+DatabaseConfig.Login+'/'+DatabaseConfig.Password);

    KafkaRecieverParams := TKafkaConsumerParams.Create(
      IniFile,
      CKafkaSection,
      '127.0.0.1',
      ChangeFileExt(ExtractFileName(GetModuleName(HInstance)), '') + '-' + IntToStr(GetCurrentThreadId()),
      'device-pool');

    KafkaProcessedParams := TKafkaProducerParams.Create(IniFile, CKafkaProcessedSection, '127.0.0.1', 'processed', '', 100000);
    KafkaServiceParams := TKafkaProducerParams.Create(IniFile, CKafkaServiceQueue, '127.0.0.1', 'service', '', 100000);
    KafkaEventsParams := TKafkaProducerParams.Create(IniFile, CKafkaEventsSection, '127.0.0.1', 'event', '', 100000);

    KafkaOffsetIni := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), GetConfigSuffix + CPositionFileExt));
    KafkaOffset := StrToInt64Def(KafkaOffsetIni.ReadString('kafka', 'offset', '-2'), -2);
    KafkaOffsetIni.Free;
    Tolog(KafkaRecieverParams.BootStrap + ';' + KafkaRecieverParams.Group+';'+KafkaRecieverParams.Topic+';'+IntToStr(KafkaOffset));

    FPointsReciever := TPointsRecieverKafka.Create(
      KafkaRecieverParams,
      KafkaProcessedParams,
      KafkaServiceParams,
      TTrackDistributor.Create(
        IniFile.ReadInteger(CAnalyzerSection, 'MaxTrackPointsCount', 100),
        TTrackAnalyzerInputDataAdapterMySQL.Create(
          DatabaseConfig,
          IniFile.ReadInteger(CCacheSection, 'Interval', 300)
        ),
        TTrackAnalyzerOutputDataAdapterMySQL.Create(
          DatabaseConfig,
          IniFile.ReadInteger(COutputSection, 'ReconnectInterval', 1),
          OnParking,
          IniFile.ReadInteger(COutputSection, 'MaxMoveEventsCount', 0)
        ),
        IniFile.ReadInteger(COutputSection, 'BulkInsertCount', 0),
        IniFile.ReadInteger(COutputSection, 'FlushTimeoutInSeconds', 60),
        OnFault,
        OnParking,
        TPointsRecieverConfigMySql.Create(DatabaseConfig, nil, ToLog),
        IniFile.ReadInteger(CAnalyzerSection, 'IterationsNumberForTryToPushAgain', 100),
        IniFile.ReadInteger(COutputSection, 'MaxFaledAttemptsNumber', 1)
      ),
      ChangeFileExt(GetModuleName(HInstance), GetConfigSuffix + CPositionFileExt),
      IniFile.ReadInteger(CKafkaSection, 'WriteOffsetPeriod', 1),
      IniFile.ReadInteger(CKafkaSection, 'RestartInterval', 30)
    );
    FPointsReciever.AddChecker(TTrackCheckerSpeed.Create);
    FPointsReciever.AddChecker(TTrackCheckerTangentialAcceleration.Create);
    FPointsReciever.AddChecker(TTrackCheckerRadialAcceleration.Create);
    FPointsReciever.Start;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.OnFault(APoint: TTrackPoint; AFaults: TFaults);
var
  jo: TJsonObject;
  e: TFaultType;
begin
  if not Assigned(FKafkaErrorsProducer) then
    Exit;

  jo := APoint.AsJsonObject;
  try
    for e in AFaults do
      jo.A['e'].Add(Ord(e));

      FKafkaErrorsProducer.Produce(AnsiString(jo.ToString));
  finally
    jo.Free;
  end;
end;

function TMainForm.OnParking(AREvent: TMoveEventSystem;
  const ATrackPoint: TTrackPoint): Boolean;
var
  JsonObject, JsonObjectEvent: TJsonObject;

begin
  Result := True;
  try
    //if Assigned(FKafkaEventsProducer) then
    begin
      JsonObjectEvent := TJsonObject.Create;

      JsonObjectEvent.I[CJsonEventField[jefType]] := Ord(AREvent.EventType);
      JsonObjectEvent.L[CJsonEventField[jefImei]] := AREvent.Imei;
      JsonObjectEvent.S[CJsonEventField[jefBegin]] := DateTimeToIls(AREvent.NewDateBegin);
      JsonObjectEvent.F[CJsonEventField[jefDuration]] := AREvent.Duration;
      JsonObjectEvent.F[CJsonEventField[jefLatitude]] := ATrackPoint.DevicePoint.GeoTrackPoint.Pos.Pos.Latitude;
      JsonObjectEvent.F[CJsonEventField[jefLongitude]] := ATrackPoint.DevicePoint.GeoTrackPoint.Pos.Pos.Longitude;
      JsonObject := TJsonObject.Create;
      JsonObject.O['eventsystem_' + CEventClassInfo[ecMove].Name] := JsonObjectEvent;
      //FKafkaEventsProducer.Produce(JsonObject.ToString);
      JsonObject.Free;
    end;
  except
    on E: Exception do
    begin
      ToLog('Не удалось создать json события. Точка: ' + IntToStr(ATrackPoint.Offset) + E.Message);
      Result := False;
    end;
  end;
end;

procedure TMainForm.Shutdown;
begin
  FPointsReciever.Free;
  FKafkaErrorsProducer.Free;
  FKafkaServiceProducer.Free;
  FKafkaEventsProducer.Free;
end;

end.
