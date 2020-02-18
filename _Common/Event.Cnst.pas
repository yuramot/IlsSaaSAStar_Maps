unit Event.Cnst;
//------------------------------------------------------------------------------
// модуль определения событий для БД
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  Ils.MySql.EnumPublisher;

//------------------------------------------------------------------------------
const
  CEventTypeTable = 'eventclass';
  CEventKindTable = 'eventtype';
  CEventSystemKindTable = 'eventsystemtype';
  CEventGeoZoneKindTable = 'eventgeotype';
  CEventTripKindTable = 'eventtriptype';

//------------------------------------------------------------------------------
type
  TEventClass = (
    ecBinarySensor,
    ecMultilevelSensor,
    ecGeoZone,
    ecMove,
    ecTrip,
    ecWaypoint
  );

  TEventClasses = set of TEventClass;

  TEventUserType = (
    eutBinaryFall,        //0
    eutBinaryFront,       //1
    eutRangeIn,           //2
    eutRangeOut,          //3
    egzEnterZone,         //4
    egzLeaveZone,         //5
    egzEnterDepot,        //6
    egzLeaveDepot,        //7
    estTrackStop,         //8
    estTrackMove,         //9
    ettDepotEnter,        //10
    ettDepotLeave,        //11
    ettTripStarted,       //12
    ettTripFinished,      //13
    ettTripInRange,       //14
    ettTripOutOfRange,    //15
    ettTripLatePoint,     //16
    ettTripLeadPoint,     //17
    ettTripNotPoint,      //18
    ewpWaypointMoveIn,    //19
    ewpWaypointMoveOut,   //20
    ewpWaypointEnter,     //21
    ewpWaypointLeave      //22
  );


  TEventUserTypes = set of TEventUserType;

  TEventSystemType = (estVehicleStop, estVehicleMove);

const
  CEventSystemType: array [TEventUserType] of string = (
    'BinaryFall',
    'BinaryFront',
    'RangeIn',
    'RangeOut',
    'EnterZone',
    'LeaveZone',
    'EnterDepot',
    'LeaveDepot',
    'Stop',
    'Move',
    'DepotEnter',
    'DepotLeave',
    'TripStarted',
    'TripFinished',
    'TripInRange',
    'TripOutOfRange',
    'TripLatePoint',
    'TripLeadPoint',
    'TripNotPoint',
    'WaypointMoveIn',
    'WaypointMoveOut',
    'WaypointEnter',
    'WaypointLeave'
  );

type
  TEventSystemTypes = set of TEventUserType;


//------------------------------------------------------------------------------
const
  CEventClassInfo: array[TEventClass] of TEnumTextInfo = (
    (Name: 'binary';      Comment: 'События по бинарным датчикам'),
    (Name: 'multilevel';  Comment: 'События по многоуровневым датчикам'),
    (Name: 'geo';         Comment: 'События по геообъектам'),
    (Name: 'move';        Comment: 'События по типу движения'),
    (Name: 'trip';        Comment: 'События по плановому маршруту'),
    (Name: 'waypoint';    Comment: 'События по путевым точкам')
  );

  CEventUserTypeInfo: array[TEventUserType] of TEnumTextInfo = (
    (ClassID: Ord(ecBinarySensor);     Name: 'Двухуровневый датчик выключен';   Comment: '';                                              Name2: 'Двухуровневый датчик включён'),
    (ClassID: Ord(ecBinarySensor);     Name: 'Двухуровневый датчик включён';    Comment: '';                                              Name2: 'Двухуровневый датчик выключен'),
    (ClassID: Ord(ecMultilevelSensor); Name: 'Вход в диапазон';                 Comment: 'Вход значения датчика в заданный диапазон';     Name2: 'Выход из диапазона'),
    (ClassID: Ord(ecMultilevelSensor); Name: 'Выход из диапазона';              Comment: 'Выход значения датчика из заданного диапазона'; Name2: 'Вход в диапазон'),
    (ClassID: Ord(ecGeoZone);          Name: 'Вход в зону';                     Comment: 'Первая точка в зоне';                           Name2: 'Выход из зоны'),
    (ClassID: Ord(ecGeoZone);          Name: 'Выход из зоны';                   Comment: 'Первая точка вне зоны';                         Name2: 'Вход в зону'),
    (ClassID: Ord(ecGeoZone);          Name: 'Вход в точку';                    Comment: 'Первое положение внутри точки';                 Name2: 'Выход из точки'),
    (ClassID: Ord(ecGeoZone);          Name: 'Выход из точки';                  Comment: 'Первое положение вне точки';                    Name2: 'Вход в точку'),
    (ClassID: Ord(ecMove);             Name: 'Остановка';                       Comment: 'Точка остановки';                               Name2: 'Движение'),
    (ClassID: Ord(ecMove);             Name: 'Движение';                        Comment: 'Точка начала движения';                         Name2: 'Остановка'),
    (ClassID: Ord(ecTrip);             Name: 'Прибытие в точку';                Comment: '';                                              Name2: 'Убытие из точки'),
    (ClassID: Ord(ecTrip);             Name: 'Убытие из точки';                 Comment: '';                                              Name2: 'Прибытие в точку'),
    (ClassID: Ord(ecTrip);             Name: 'Начало исполнения плана';         Comment: '';                                              Name2: ''),
    (ClassID: Ord(ecTrip);             Name: 'Окончание исполнения плана';      Comment: '';                                              Name2: ''),
    (ClassID: Ord(ecTrip);             Name: 'Возврат на плановый трек';        Comment: '';                                              Name2: 'Отклонение от планового трека'),
    (ClassID: Ord(ecTrip);             Name: 'Отклонение от планового трека';   Comment: '';                                              Name2: 'Возврат на плановый трек'),
    (ClassID: Ord(ecTrip);             Name: 'Опоздание на плановую точку';     Comment: '';                                              Name2: ''),
    (ClassID: Ord(ecTrip);             Name: 'Опережение плановой точки';       Comment: '';                                              Name2: ''),
    (ClassID: Ord(ecTrip);             Name: 'Остановка вне плановой точки';    Comment: '';                                              Name2: ''),
    (ClassID: Ord(ecWaypoint);         Name: 'Приближение к плановой точке';    Comment: '';                                              Name2: 'Удаление от плановой точки'),
    (ClassID: Ord(ecWaypoint);         Name: 'Удаление от плановой точки';      Comment: '';                                              Name2: 'Приближение к плановой точке'),
    (ClassID: Ord(ecWaypoint);         Name: 'Вход в плановую точку';           Comment: '';                                              Name2: 'Выход из плановуй точки'),
    (ClassID: Ord(ecWaypoint);         Name: 'Выход из плановуй точки';         Comment: '';                                              Name2: 'Вход в плановую точку')
  );

//------------------------------------------------------------------------------
implementation

end.

