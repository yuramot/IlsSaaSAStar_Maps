unit Ils.Json.Names;

//------------------------------------------------------------------------------
interface

uses
  SysUtils;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! список протоколов
//------------------------------------------------------------------------------
  dtUnknown = 0;
  dtNovacom = 1;
  dtWialon = 2;
  dtMintrans = 3;
  dtNavtelecom = 4;
  dtRusAgroTaxi = 5;
  dtRuptela = 6;
  dtExpeditor = 7;

//------------------------------------------------------------------------------
//! корневые поля JSON'ов точек
//------------------------------------------------------------------------------
{!
  неизвестно,
  версия, imei прибора, количество спутников(*), координаты корректны, тип протокола,
  широта(**), долгота(**), дата и время точки, дата и время приёма, пробег(*), скорость(*),
  нажата тревожная кнопка(***), работает ли мотор(*), направление(*), высота(*), датчики(*)
}
(*
  * - необязятельный параметр
  ** - обязательны для точки, НО отсутствуют для "точек" снимков
  *** - есть, если нажата ТК
*)
type
  TJsonField = (
    jfUnknown,
    jfGeneration, jfImei, jfSatellitesCount, jfSatellitesValid, jfDeviceType,
    jfLatitude, jfLongitude, jfDateTime, jfReceived, jfLen, jfVelocity,
    jfPanic, jfEngineWorks, jfAzimuth, jfAltitude, jfSensors,
    jfDeviceVelocity, jfDeviceAzimuth
  );

const
  CJsonField: array[TJsonField] of string = (
    'unknown',
    'g', 'i', 'sc', 'sv', 't',
    'la', 'lo', 'dt', 'r', 'l', 'v',
    'panic', 'ew', 'd', 'a', 's',
    'dv', 'dd'
  );

//------------------------------------------------------------------------------
//! типы данных сенсоров
//------------------------------------------------------------------------------
type
  TSensorMeasure = (
    smUnknown,
    smBinary,
    smBinary8,
    smBinary16,
    smBinary32,
    smInteger,
    smFloat,
    smRaw,
    smString
  );

const
  CSensorMeasureStr: array[TSensorMeasure] of string = (
    'unknown',
    '1',
    '8',
    '16',
    '32',
    'i',
    'f',
    'raw',
    's'
  );

//------------------------------------------------------------------------------
//! список сенсоров
//------------------------------------------------------------------------------
type
  TSensorType = (
    stUnknown,  // <неизвестный>
    stVExt,     // внешнее питание - мВ
    stVInt,     // внутреннее питание - мВ
    stAin,      // аналоговые входы
    stBin,      // цифровые входы
    stBin8,     // цифровые входы
    stBout,     // цифровые выходы
    stBout8,    // цифровые выходы
    stCamera,   // .СНИМОК КАМЕРЫ.
    stTemp,     // температура - градусы C
    stCounter,  // счётчик
    stMileage,  // полный пробег - км
    stMoto,     // моточасы - с
    stFuelTemp, // температура топлива - градусы C
    stFuel,     // топливо RS232/RS485 - для тарировки
    stCANFuelP,       // CAN-топливо - %
    stCANFuelL,       // CAN-топливо - л
    stCANFuelConsume, // CAN-полный расход топлива - л
    stCANRPM,         // CAN-обороты двигателя
    stCANDistance,    // CAN-полный пробег
    stCANCoolTemp,    // CAN-температура охлаждающей жидкости - градусы C
    stCANAxleLoad,    // CAN-нагрузка на ось - кг
    stCANPAccel,      // CAN-педаль газа - %
    stCANPStop,       // CAN-педаль тормоза - %
    stCANFilterP,     // CAN-уровень жидкости в дизельном фильтре выхлопных газов - %
    stCANFilterL,     // CAN-уровень жидкости в дизельном фильтре выхлопных газов - л
    stCANEngineLoad,  // CAN-нагрузка двигателя - %
    stCANEngineTime,  // CAN-время работы двигателя - с
    stCANTillDTM,     // CAN-расстояние до ТО - км
    stCANVelocity,    // CAN-скорость - км/ч
    stCANInfoSt,      // CAN-флаги состояния безопасности
    stCANSecurity,    // CAN-события состояния безопасности
    stCANAlarmSt,     // CAN-контроллеры аварии
    stCANFaultSt,     // CAN-состояние ламп индикации неисправностей
    stCANFault,       // CAN-диагностический код неисправности
    stHDOP, // hdop
    stVDOP, // vdop
    stPDOP, // pdop
    stTDOP, // tdop
    stGDOP, // gdop
    stFlowsensSt,         // ДУТ: статус датчика
    stFlowsensTotalCons,  // ДУТ: суммарный расход топлива - л
    stFlowsensTripCons,   // ДУТ: расход топлива за поездку - л
    stFlowsensFlow,       // ДУТ: текущая скорость потока - л/ч
    stFlowsensFeedCons,   // ДУТ: суммарный объем топлива камеры подачи - л
    stFlowsensFeedFlow,   // ДУТ: текущая скорость потока камеры подачи - л/ч
    stFlowsensFeedTemp,   // ДУТ: температура камеры подачи - градусы C
    stFlowsensReturnCons, // ДУТ: суммарный объем топлива камеры обратки - л
    stFlowsensReturnFlow, // ДУТ: текущая скорость потока камеры обратки - л/ч
    stFlowsensReturnTemp, // ДУТ: температура камеры обратки - градусы C
    stBDTimeInc,  // приращение времени - с
    stBDAccelX,   // линейное ускорение по X - g
    stBDAccelY,   // линейное ускорение по Y - g
    stBDAccelZ,   // линейное ускорение по Z - g
    stBDAccelModule,  // модуль вектора ускорения - g
    stBDAccelMax,     // максимальное значение положительного ускорения за период - g
    stBDBrakeMax,     // максимальное значение отрицательного ускорения (торможения) за период - g
    stBDCrnMax,       // максимальное значение бокового ускорения за период - g
    stBDAngMax,       // максимальное значение углового ускорения за период - градус/с^2
    stBDVertMax,      // максимальное значение вертикального ускорения за период - g
    stBDSpeedMax,     // максимальное значение скорости за период - км/ч
    stBDDuration,     // длительность превышения порога - с
    stBDSpeedState,   // состояние порогов скорости
    stBDAllState,     // состояние порогов ускорения
    stFreq,     // частота на аналогово-частотном датчике - гц
    stFuelFreq, // частота на выходе датчика уровня топлива - гц
    stEngineHoursWork,  // пользовательские моточасы 1 (работа под нагрузкой) - с
    stEngineHoursIdle,  // пользовательские моточасы 2 (работа без нагрузки) - с
    stHPTemp,     // высокоточный датчик температуры - градусы C
    stHPHumidity, // высокоточный датчик влажности - %
    stGeoState,   // информация о нахождении в геозонах
    stAccelState, // состояние виртуальных датчиков акселерометра
    stTiltLocal,  // внутренний датчик угла наклона: угол наклона относительно местной вертикали - градусы
    stTiltPitch,  // внутренний датчик наклона: угол тангажа - градусы
    stTiltRoll,   // внутренний датчик наклона: угол крена - градусы
    stTiltX,      // внешний датчик угла наклона: отклонение по оси X
    stTiltY,      // внешний датчик угла наклона: отклонение по оси Y
    stTiltZ,      // внешний датчик угла наклона: отклонение по оси Z
    stPassCount,  // счётчик пассажиропотока
    stTacho,      // данные от тахографа
    stTachoMode,  // режим работы тахографа/карта
    stTachoState, // флаги состояния от тахографа
    stTachoSpeed, // скорость по тахографу - км/ч
    stTachoOdom,  // одометр по тахографу - км
    stTachoTime,  // время по тахографу - с от 1970 (unix time)
    stTachoDmStatus,    // текущее состояние водителя принятое от дисплейного модуля
    stTachoDmIndex,     // индекс последнего полученного/прочитанного сообщения на дисплейном модуле
    stFridgeState,      // РУ: состояние
    stFridgeTemp,       // РУ: температура рефрижератора - градусы C
    stFridgeSetTemp,    // РУ: температура установленная - градусы C
    stFridgeTempOut,    // РУ: температура окружающего воздуха - градусы C
    stFridgeTempIn,     // РУ: температура ОЖ - градусы C
    stFridgeV,          // РУ: напряжение аккумулятора - мВ
    stFridgeA,          // РУ: сила тока аккумулятора - мВ
    stFridgeMotoInt,    // РУ: моточасы работы от двигателя - ч
    stFridgeMotoExt,    // РУ: моточасы работы от сети - ч
    stFridgeCompMode,   // РУ: конфигурация компрессора
    stFridgeEngineRpm,  // РУ: состояние двигателя, обороты
    stFridgeEngineMode, // РУ: состояние двигателя, режим
    stFridgeFault,      // РУ: код ошибки
    stFridgeFaultCount, // РУ: количество ошибок
    stShinNumber,   // датчик давления в шинах: № колеса
    stShinPressure, // датчик давления в шинах: давление - бар
    stShinTemp,     // датчик давления в шинах: температура - градусы C
    stAutoinfStatus,  // статус автоинформатора
    stLastGeoID,      // ID последней геозоны
    stLastStopID,     // ID последней остановки
    stRouteID,        // ID текущего маршрута
    stCamStatus // статус камеры
  );

const
  CSensorNamePrefix: array[TSensorType] of string = (
    'unknown',  //stUnknown
    'vext',     //stVExt
    'vint',     //stVInt
    'ain',      //stAin
    'bin',      //stBin
    'bin',      //stBin8
    'bout',     //stBout
    'bout',     //stBout8
    'cam',      //stCamera
    'temp',     //stTemp
    'cnt',      //stCounter
    'mil',      //stMileage
    'moto',     //stMoto
    'fueltemp', //stFuelTemp
    'fuel',     //stFuel
    'fuelp',            //stCANFuelP
    'fuell',            //stCANFuelL
    'fuelconsume',      //stCANFuelConsume
    'rpm',              //stCANRPM
    'distance',         //stCANDistance
    'cool',             //stCANCoolTemp
    'axle',             //stCANAxleLoad
    'paccel',           //stCANPAccel
    'pstop',            //stCANPStop
    'filterp',          //stCANFilterP
    'filterl',          //stCANFilterL
    'eload',            //stCANEngineLoad
    'etime',            //stCANEngineTime
    'till_dtm',         //stCANTillDTM
    'vel',              //stCANVelocity
    'can_info_st',      //stCANInfoSt
    'can_security_evt', //stCANSecurity
    'can_alarm_st',     //stCANAlarmSt
    'can_fault_st',     //stCANFaultSt
    'can_fault',        //stCANFault
    'hdop', //stHDOP
    'vdop', //stVDOP
    'pdop', //stPDOP
    'tdop', //stTDOP
    'gdop', //stGDOP
    'flowsens_st',              //stFlowsensSt
    'flowsens_total_cons',      //stFlowsensTotalCons
    'flowsens_trip_cons',       //stFlowsensTripCons
    'flowsens_flow_spd',        //stFlowsensFlow
    'flowsens_feed_cons',       //stFlowsensFeedCons
    'flowsens_feed_flow_spd',   //stFlowsensFeedFlow
    'flowsens_feed_temp',       //stFlowsensFeedTemp
    'flowsens_return_cons',     //stFlowsensReturnCons
    'flowsens_return_flow_spd', //stFlowsensReturnFlow
    'flowsens_return_temp',     //stFlowsensReturnTemp
    'timeinc',  //stBDTimeInc
    'accx',     //stBDAccelX
    'accy',     //stBDAccelY
    'accz',     //stBDAccelZ
    'wln_accel_module', //stBDAccelModule
    'wln_accel_max',    //stBDAccelMax
    'wln_brk_max',      //stBDBrakeMax
    'wln_crn_max',      //stBDCrnMax
    'wln_ang_max',      //stBDAngMax
    'wln_vert_max',     //stBDVertMax
    'wln_spd_max',      //stBDSpeedMax
    'thld_duration',    //stBDDuration
    'thld_spd_st',      //stBDSpeedState
    'thld_all_st',      //stBDAllState
    'freq',       //stFreq
    'fuel_freq',  //stFuelFreq
    'engine_hours_work',  //stEngineHoursWork
    'engine_hours_idle',  //stEngineHoursIdle
    'hp_temp',      //stHPTemp
    'hp_humidity',  //stHPHumidity
    'geo_st',   //stGeoState
    'accel_st', //stAccelState
    'int_tilt_sens_local',  //stTiltLocal
    'int_tilt_sens_pitch',  //stTiltPitch
    'int_tilt_sens_roll',   //stTiltRoll
    'ext_tilt_sens_x',      //stTiltX
    'ext_tilt_sens_y',      //stTiltY
    'ext_tilt_sens_z',      //stTiltZ
    'p_count',  //stPassCount
    'tacho',        //stTacho
    'tacho_mode',   //stTachoMode
    'tacho_state',  //stTachoState
    'tacho_speed',  //stTachoSpeed
    'tacho_odom',   //stTachoOdom
    'tacho_time',   //stTachoTime
    'dm_status',  //stTachoDmStatus
    'dm_mess_n',  //stTachoDmIndex
    'fridge_st',              //stFridgeState
    'fridge_temp',            //stFridgeTemp
    'fridge_set_temp',        //stFridgeSetTemp
    'fridge_outside_temp',    //stFridgeTempOut
    'fridge_coolant_temp',    //stFridgeTempIn
    'fridge_pwr_vlt',         //stFridgeV
    'fridge_pwr_cur',         //stFridgeA
    'fridge_eng_moto_hours',  //stFridgeMotoInt
    'fridge_elec_moto_hours', //stFridgeMotoExt
    'fridge_comp_mode',       //stFridgeCompMode
    'fridge_engine_rpm',      //stFridgeEngineRpm
    'fridge_engine_mode',     //stFridgeEngineMode
    'fridge_fault',           //stFridgeFault
    'fridge_fault_count',     //stFridgeFaultCount
    'tpms_number',    //stShinNumber
    'tpms_pressure',  //stShinPressure
    'tpms_temp',      //stShinTemp
    'autoinf_status', //stAutoinfStatus
    'last_geo_id',    //stLastGeoID
    'last_stop_id',   //stLastStopID
    'cur_route_id',   //stRouteID
    'camstatus' //stCamStatus
  );

  CSensorMeasureByType: array[TSensorType] of TSensorMeasure = (
    smUnknown,        //stUnknown
    smInteger,        //stVExt
    smInteger,        //stVInt
    smInteger,        //stAin
    smBinary,         //stBin
    smBinary8,        //stBin8
    smBinary,         //stBout
    smBinary8,        //stBout8
    smRaw,            //stCamera
    smInteger,        //stTemp
    smInteger,        //stCounter
    smFloat,          //stMileage
    smInteger,        //stMoto
    smInteger,        //stFuelTemp
    smInteger,        //stFuel
    smInteger,        //stCANFuelP
    smFloat,          //stCANFuelL
    smFloat,          //stCANFuelConsume
    smInteger,        //stCANRPM
    smFloat,          //stCANDistance
    smInteger,        //stCANCoolTemp
    smInteger,        //stCANAxleLoad
    smInteger,        //stCANPAccel
    smInteger,        //stCANPStop
    smInteger,        //stCANFilterP
    smFloat,          //stCANFilterL
    smInteger,        //stCANEngineLoad
    smInteger,        //stCANEngineTime
    smInteger,        //stCANTillDTM
    smInteger,        //stCANVelocity
    smBinary16,       //stCanInfoSt
    smInteger,        //stCanSecurity
    smBinary32,       //stCanAlarmSt
    smBinary8,        //stCanFaultSt
    smInteger,        //stCanFault
    smFloat,          //stHDOP
    smFloat,          //stVDOP
    smFloat,          //stPDOP
    smFloat,          //stTDOP
    smFloat,          //stGDOP
    smInteger,        //stFlowsensSt,
    smFloat,          //stFlowsensTotalCons,
    smFloat,          //stFlowsensTripCons,
    smFloat,          //stFlowsensFlow,
    smFloat,          //stFlowsensFeedCons,
    smFloat,          //stFlowsensFeedFlow,
    smFloat,          //stFlowsensFeedTemp,
    smFloat,          //stFlowsensReturnCons,
    smFloat,          //stFlowsensReturnFlow,
    smFloat,          //stFlowsensReturnTemp
    smFloat,          //stBDTimeInc
    smFloat,          //stBDAccelX
    smFloat,          //stBDAccelY
    smFloat,          //stBDAccelZ
    smFloat,          //stBDAccelModule
    smFloat,          //stBDAccelMax
    smFloat,          //stBDBrakeMax
    smFloat,          //stBDCrnMax
    smFloat,          //stBDAngMax
    smFloat,          //stBDVertMax
    smInteger,        //stBDSpeedMax
    smFloat,          //stBDDuration
    smInteger,        //stBDSpeedState
    smInteger,        //stBDAllState
    smInteger,        //stFreq
    smInteger,        //stFuelFreq
    smInteger,        //stEngineHoursWork
    smInteger,        //stEngineHoursIdle
    smFloat,          //stHPTemp
    smFloat,          //stHPHumidity
    smInteger,        //stGeoState
    smInteger,        //stAccelState
    smFloat,          //stTiltLocal
    smFloat,          //stTiltPitch
    smFloat,          //stTiltRoll
    smInteger,        //stTiltX
    smInteger,        //stTiltY
    smInteger,        //stTiltZ
    smInteger,        //stPassCount
    smInteger,        //stTacho
    smInteger,        //stTachoMode
    smInteger,        //stTachoState
    smInteger,        //stTachoSpeed
    smFloat,          //stTachoOdom
    smInteger,        //stTachoTime
    smInteger,        //stTachoDmStatus
    smInteger,        //stTachoDmIndex
    smInteger,        //stFridgeState
    smFloat,          //stFridgeTemp
    smFloat,          //stFridgeSetTemp
    smFloat,          //stFridgeTempOut
    smFloat,          //stFridgeTempIn
    smInteger,        //stFridgeV
    smInteger,        //stFridgeA
    smFloat,          //stFridgeMotoInt
    smFloat,          //stFridgeMotoExt
    smInteger,        //stFridgeCompMode
    smInteger,        //stFridgeEngineRpm
    smInteger,        //stFridgeEngineMode
    smInteger,        //stFridgeFault
    smInteger,        //stFridgeFaultCount
    smInteger,        //stShinNumber
    smFloat,          //stShinPressure
    smInteger,        //stShinTemp
    smInteger,        //stAutoinfStatus
    smInteger,        //stLastGeoID
    smInteger,        //stLastStopID
    smInteger,        //stRouteID
    smInteger         //stCamStatus
  );

type
  TJsonEventField = (
    jefType,
    jefImei,
    jefBegin,
    jefDuration,
    jefLatitude,
    jefLongitude,
    jefRadius,
    jefDispersion,
    jefUpdate
  );

const
  CJsonEventField: array[TJsonEventField] of string = (
    'EventTypeID',
    'IMEI',
    'DT',
    'Duration',
    'Latitude',
    'Longitude',
    'Radius',
    'Dispersion',
    'Update'
  );


function JF2Str(const AJF: TJsonField): string;
function Str2JF(const AStr: string): TJsonField;

function MakeSensorName(const AType: TSensorType): string; overload;
function MakeSensorName(const AType: TSensorType; const AIdx: Integer): string; overload;

function GetSensorIdx(const AName: string): Integer;
function GetSensorName(const AName: string): string;
function GetSensorMeasure(const AName: string): TSensorMeasure;


//------------------------------------------------------------------------------
implementation

function JF2Str(const AJF: TJsonField): string;
begin
  Result := CJsonField[AJF];
end;

function Str2JF(const AStr: string): TJsonField;
var
  jf: TJsonField;
begin
  Result := jfUnknown;
  for jf := Succ(Low(TJsonField)) to High(TJsonField) do
    if AStr = CJsonField[jf] then
      Exit(jf);
end;

function MakeSensorName(const AType: TSensorType): string;
begin
  Result :=
      CSensorNamePrefix[AType]
    + ':' + CSensorMeasureStr[CSensorMeasureByType[AType]];
end;

function MakeSensorName(const AType: TSensorType; const AIdx: Integer): string;
begin
  Result :=
      CSensorNamePrefix[AType]
    + '_' + IntToStr(AIdx)
    + ':' + CSensorMeasureStr[CSensorMeasureByType[AType]];
end;

function GetSensorIdx(const AName: string): Integer;
var
  ColonPos: Integer;
  UnderPos: Integer;
begin
  Result := -1;
  UnderPos :=  Pos('_', AName);
  ColonPos :=  Pos(':', AName);
  if (UnderPos > 1) and (ColonPos > 1) then
    Result := StrToIntDef(Copy(AName, UnderPos + 1, ColonPos - UnderPos - 1), -1);
end;

function GetSensorName(const AName: string): string;
var
  ColonPos: Integer;
  UnderPos: Integer;
begin
  Result := AName;
  UnderPos :=  Pos('_', AName);
  ColonPos :=  Pos(':', AName);
  if UnderPos > 1 then
    Result := Copy(AName, 1, UnderPos)
  else if ColonPos > 1 then
    Result := Copy(AName, 1, ColonPos);
end;

function GetSensorMeasure(const AName: string): TSensorMeasure;
var
  ColonPos: Integer;
  SensorMeasureStr: string;
begin
  Result := smUnknown;
  ColonPos :=  Pos(':', AName);
  if ColonPos > 1 then
  begin
    SensorMeasureStr := Copy(AName, ColonPos + 1, Length(AName) - ColonPos);
    for Result := Low(TSensorMeasure) to High(TSensorMeasure) do
      if CSensorMeasureStr[Result] = SensorMeasureStr then
        Exit;

    Result := smUnknown;
  end;
end;

end.

