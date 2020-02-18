﻿unit T_Points;
//------------------------------------------------------------------------------
// Модуль содержит описание различных типов структур
//
// !!!
//  вопреки названию содержит только описания структур, хранящихся
//  в файлах или БД
//
//  все структуры, записывающиеся в файлы, объявляются как packed
//
//  после введения в эксплуатацию, структуры не могут быть изменены
// !!!
//------------------------------------------------------------------------------
// Список:
//
//  гео-точка
//  информаия о машине (внутренняя)
//  информация о плановой точке (внутренняя)
//  точка в универсальном формате ИЛС - для файлов данных
//  фактические и плановые точки - для записи в blob-поял БД
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! запись гео-координат
(*!
  содержит информациею о гео-координатах (широта и долгота)
*)
//------------------------------------------------------------------------------
  PGeoPoint = ^TGeoPoint;

  TGeoPoint = packed record
    //! широта
    Latitude: Double;
    //! долгота
    Longitude: Double;
  end;

  TGeoPointArray = array of TGeoPoint;

//------------------------------------------------------------------------------
//! запись с активными машинами
(*!
  содержит информацию об активных машинах
*)
//------------------------------------------------------------------------------
  PActiveDevice = ^TActiveDevice;

  TActiveDevice = record
    //! номер прибора
    DeviceID: Integer;
    //! координаты машины
    Coordinates: TGeoPoint;
    //! последние дата и время
    PointDT: TDateTime;
    //! номер рейса
    TripNumber: string[51]; // 52 байта
  end;

  TActiveDeviceArray = array of TActiveDevice;

//------------------------------------------------------------------------------
//! запись с активными плановыми точками
(*!
  содержит информацию об активных машинах
*)
//------------------------------------------------------------------------------
  PActivePlanPoint = ^TActivePlanPoint;

  TActivePlanPoint = record
    //! идентификатор
    ID: Integer;
    //! номер прибора
    DeviceID: Integer;
    //! тип точки
    PointType: Integer;
    //! номер точки
    Number: Integer;
    //! номер точки по факту
    ActualNumber: Integer;
    //! координаты машины
    Coordinates: TGeoPoint;
    //! плановые дата и время прибытия
    PlanDTArr: TDateTime;
    //! фактические дата и время прибытия
    FactDTArr: TDateTime;
    //! фактические дата и время прибытия (предыдущие)
    FactDTArrPrev: TDateTime;
    //! фактическое время обслуживания - продолжительность
    FactTimeUnload: TTime;
    //! фактическое время обслуживания - продолжительность (предыдущее)
    FactTimeUnloadPrev: TTime;
    //! дата и время следующей плановой точки
    NextPlanPointDTArr: TDateTime;
    //! индекс следующей плановой точки
    NextPlanPointIndex: Integer;
    //! номер рейса
    TripNumber: string[51]; // 52 байта
    //! код заказа
    OrderID: string[51]; // 52 байта
    //! температура
    Temperature: Double;
  end;

  TActivePlanPointArray = array of TActivePlanPoint;

//------------------------------------------------------------------------------
//! запись со строкой накладной для плановой точки для мобильных устройств
(*!
  содержит информацию об 1 строке накладной, которая используется мобильным
  устройством в привязке к плановой точке
*)
//------------------------------------------------------------------------------
  PMobileWayBill = ^TMobileWayBill;

  TMobileWayBill = packed record
    //! идентификатор поля накладной
    ID: Integer;
    //! идентификатор мобильного устройства
    DeviceID: Integer;
    //! идентификатор провайдера
    ProviderID: Word;
    //! код заказа
    OrderID: string[50];
    //! идентификатор строки накладной
    GoodID: Integer;
    //! номер строки накладной
    GoodStringNumber: Integer;
    //! наименование товарной позиции
    GoodName: string[255];
    //! номер поля в строке накладной
    Number: Word;
    //! наименование поля
    Name: string[255];
    //! значение поля
    Value: string[255];
    //! разрешение на исправление
    AllowChange: string[1];
  end;

  TMobileWayBillArray = array of TMobileWayBill;

//------------------------------------------------------------------------------
//! запись с изменением строки накладной для плановой точки для мобильных устройств
(*!
  содержит информацию об изменениии 1 строке накладной,
  отсылается мобильным утройством при изменении накладной
*)
//------------------------------------------------------------------------------
  PMobileWayBillChange = ^TMobileWayBillChange;

  TMobileWayBillChange = packed record
    //! идентификатор мобильного устройства
    DeviceID: Integer;
    //! идентификатор провайдера
    ProviderID: Word;
    //! дата и время создания
    ChangeDateTime: TDateTime;
    //! код заказа
    OrderID: string[50];
    //! идентификатор строки накладной
    GoodID: integer;
    //! идентификатор поля накладной
    ID: integer;
    //! новое значение
    Value: string[255];
  end;

  TMobileWayBillChangeArray = array of TMobileWayBillChange;

//------------------------------------------------------------------------------
//! запись с сообщением для экспедитора
(*!
  содержит информацию о сообщении, которое отправляется или принимается экспедитором
*)
//------------------------------------------------------------------------------
  PMobileMessage = ^TMobileMessage;

  TMobileMessage = packed record
    //! идентификатор мобильного устройства
    DeviceID: Integer;
    //! идентификатор провайдера
    ProviderID: Word;
    //! дата и время создания
    MesDateTime: TDateTime;
    //! текст
    Comment: string[255];
  end;

  TMobileMessageArray = array of TMobileMessage;

//------------------------------------------------------------------------------
//! запись со статусом плановой точки для мобильных устройств
(*!
  содержит информацию со статусом плановой точки
*)
//------------------------------------------------------------------------------
  PMobilePlanPointStatus = ^TMobilePlanPointStatus;

  TMobilePlanPointStatus = packed record
    //! идентификатор мобильного устройства
    DeviceID: Integer;
    //! идентификатор провайдера
    ProviderID: Word;
    //! идентификотор плановой точки
    PlanPointID: string[50];
    //! дата и время создания
    CreateDateTime: TDateTime;
    //! статус
    StatusID: Byte;
    //! комментарий к статусу
    Comment: string[255];
    //! дата и время планового прибытия
    DateTimeArr: TDateTime;
  end;

  TMobilePlanPointStatusArray = array of TMobilePlanPointStatus;

//------------------------------------------------------------------------------
//! запись с плановой точкой для мобильных устройств
(*!
  содержит информацию о плановой точке, которая используется мобильным устройством
*)
//------------------------------------------------------------------------------
  PMobilePlanPoint = ^TMobilePlanPoint;

  TMobilePlanPoint = packed record
    //! идентификатор точки
    ID: string[50];
    //! идентификатор мобильного устройства
    DeviceID: Integer;
    //! идентификатор провайдера
    ProviderID: Word;
    //! название
    Name: string[100];
    //! адрес
    Address: string[255];
    //! координаты
    Coordinates: TGeoPoint;
    //! позиция в маршруте
    Index: Integer;
    //! время начала работы точки по плану
    PlanTimeBeg: TDateTime;
    //! время окончания работы точки по плану
    PlanTimeEnd: TDateTime;
    //! время прибытия в точку по плану
    PlanTimeArr: TDateTime;
    //! тип точки
    (*
      0 - начальная
      1 - промежуточная
      2 - конечная
    *)
    PointType: Byte;
    //! комментарий
    Comment: string[255];
    //! статус обслуживания точки
    (*
      0 - новая
      1 - принята
      2 - обслужена
      3 - обслужена не полностью
      4 - не обслужена
    *)
    Status: Byte;
  end;

  TMobilePlanPointArray = array of TMobilePlanPoint;

//------------------------------------------------------------------------------
//! запись с точкой (данными) прибора универсального формата
(*!
  содержит точку от прибора в универсальном формате ИЛС
*)
//------------------------------------------------------------------------------
  PDevicePointUniversal = ^TDevicePointUniversal;

  TDevicePointUniversal = packed record
    //! номер прибора
    DeviceID: Integer;
    //! количество спутников
    SatellitesNum: Byte;
    //! код соединения (провайдера) - в железных приборах не используется!
    ProviderID: Word;
    //! тип устройства - НЕ используется!
    DeviceType: Byte;
    //! дата и время точки (!по Гринвичу!)
    PointDateTime: TDateTime;
    //! широта (градусы)
    Latitude: Single;
    //! долгота (градусы)
    Longitude: Single;
    //! скорость, км/ч - НЕ используется!
    Speed: Word;
    //! азимут, 2 градуса - НЕ используется!
    Azimut: Byte;
    //! пробег, 1 м - НЕ используется!
    Length: Int32;
    //! топливо, поступившее в бак (0,1 л) - НЕ используется!
    FuelInEngine: Word;
    //! топливо в баке (1/512 от объема бака) - НЕ используется!
    Fuel: Word;
    //! цифровые двухуровневые параметры (!8 штук!)
    Param2: Byte;
    //! многоуровневые параметры
    ParamN: array[0..19] of Integer;
    //! номер запроса, либо 255 (если без него) - НЕ используется!
    QueryID: Byte;
    //! идентификатор водителя (либо резерв) - НЕ используется!
    DriverID: array[0..5] of Byte;
  end;

  TDevicePointUniversalArray = array of TDevicePointUniversal;

//------------------------------------------------------------------------------
implementation

end.

