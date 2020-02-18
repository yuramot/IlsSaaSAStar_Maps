unit Ils.PlanFact.Status;

interface

uses
  Ils.MySql.EnumPublisher;

type

  TWaypointStatus = (
    wsNotVisited,             //не посещена
    wsVisitInProgress,        //обслуживается
    wsVisited,                //посещена
    wsVisitedOutrunning,      //посещена с допустимым опережением
    wsVisitedDelay,           //посещена с допустимым опозданием
    wsVisitedOutrunningHard,  //посещена с опережением
    wsVisitedDelayHard        //посещена с опозданием
  );

const

  CWaypointStatusKindTable = 'waypointstatus';

  CWaypointStatusInfo: array[TWaypointStatus] of TEnumTextInfo = (
    (Name: 'NotVisited';              Comment: 'не посещена'),
    (Name: 'VisitInProgress';         Comment: 'обслуживается'),
    (Name: 'Visited';                 Comment: 'посещена'),
    (Name: 'VisitedOutrunning';       Comment: 'посещена с допустимым опережением'),
    (Name: 'VisitedDelay';            Comment: 'посещена с допустимым опозданием'),
    (Name: 'VisitedOutrunningHard';   Comment: 'посещена с опережением'),
    (Name: 'VisitedDelayHard';        Comment: 'посещена с опозданием')
  );

type

  TTripStatus = (
    tsToExecute                   = 100,  // Рейс поставлен на исполнение
    // начало движения из гаража без груза
    tsStarted                     = 101,  // Рейс начат, груза нет
    // исполняемые
    tsInProgress                  = 102,  // Рейс в процессе, машина с грузом
    tsInProgressOutrunning        = 103,  // Рейс в процессе, машина с грузом, есть опережение
    tsInProgressDelay             = 104,  // Рейс в процессе, машина с грузом, есть опоздание
    // возврат в гараж без груза
    tsReturn                      = 105,  // Возврат на базу, груза нет
    tsReturnOutrunning            = 106,  // Возврат на базу, груза нет, есть опережение
    tsReturnDelay                 = 107,  // Возврат на базу, груза нет, есть опоздание
    // закрытые успешные
    tsFinished                    = 200,  // Завершён
    tsFinishedOutrunning          = 201,  // Завершён с опережением
    tsFinishedDelay               = 202,  // Завершён с опозданием
    // закрытые проваленные
    tsFailed                      = 203,  // Не выполнен
    tsCancelled                   = 204,  // Отменён
    // частично закрытые успешные
    tsPartiallyFinished           = 205,  // Завершён, есть непосещённые точки
    tsPartiallyFinishedOutrunning = 206,  // Завершён с опережением, есть непосещённые точки
    tsPartiallyFinishedDelay      = 207,  // Завершён с опозданием, есть непосещённые точки
    // закрытые как некорректные
    tsInvalid                     = 208,  // Отклонён, как некорректный
    // шаблоны для Факт-План РАИ
    tsFPTemplate                  = 300  // Шаблон для генерации рейсов АФП
  );

const

  CTripStatusKindTable = 'tripstatus';

  CTripTrackTrackableStatuses = [
    tsStarted,
    tsInProgress,
    tsInProgressOutrunning,
    tsInProgressDelay
  ];

  CTripTrackNotificableStatuses = [
    tsInProgress,
    tsInProgressOutrunning,
    tsInProgressDelay
  ];

type

  TTripCheckError = (
    tchsOK,
    tchsDuplicatePointStart // дублирование времени плановых точек
  );

implementation

end.

