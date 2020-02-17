unit AStar64.Common;
//------------------------------------------------------------------------------
// модуль внешних определений AStar64.dll
//
// содержит определения, необходимые как внутри dll,
//  так и для определения экспортируемых функций
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

const
  ERROR_ASTAR_SUCCESS                   = 0;

  ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER  = -1;
  ERROR_ASTAR_REQUEST_NOT_ASSIGNED      = -2;
  ERROR_ASTAR_REQUEST_WRONG_VERSION     = -3;
  ERROR_ASTAR_HAS_WRONG_FILE_VERSION    = -4;
  ERROR_ASTAR_TIMEOUT                   = -5;
  ERROR_ASTAR_NOT_ENOUGH_MEMORY         = -6;
  ERROR_ASTAR_FILE_NOT_FOUND            = -7;
  ERROR_ASTAR_NO_WAY_FOUND              = -8;
  ERROR_ASTAR_NEAR_EDGE_NOT_FOUND       = -9;
  ERROR_UNKNOWN_ERROR                   = -10;
  ERROR_ASTAR_ZONE_FILE_WRONG_SIZE      = -11;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! запись метаданных - версия №1
//------------------------------------------------------------------------------
  PMetaDataV1 = ^TMetaDataV1;

  TMetaDataV1 = packed record
    //! версия
    Version: Integer; // 4
    //! тип дороги
    RoadType: Integer; // 8
    //! время в пути к этому моменту
    RouteTime: Double; // 16
    //! зоны
    Zones: UInt64; // 24
    //! суперполе ограничений (*** на данный момент - максимальная скорость в младшем байте ***)
    Restrictions: UInt64; // 32
  end;

//------------------------------------------------------------------------------
//! прототип процедуры обратного вызова
//------------------------------------------------------------------------------
  TSpeedCallbackFunc = function(
    //! ссылка на внешние данные
    const AHandle: Pointer;
    //! ссылка на передаваемые данные
    const AMetaData: Pointer
  ): Integer; stdcall;

//------------------------------------------------------------------------------
implementation

end.

