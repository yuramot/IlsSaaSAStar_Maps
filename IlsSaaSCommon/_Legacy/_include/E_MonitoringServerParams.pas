unit E_MonitoringServerParams;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Windows, Registry,
  E_Logger;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//--------------------- class TMonitoringServerParams --------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! Запись с параметрами Службы Monitoring Server
(*!
  Содержит параметры подключения к БД и другие настройки службы
*)
//------------------------------------------------------------------------------
type
  TMonitoringServerParams = record
    //! Период опроса директории на наличие новых данных
    Period: Smallint;
    //! Строка соединения с базой данных
    DBConnectionString: string[255];
    //! Таймаут соединения с БД
    DBTimeOut: Smallint;
    //! Рабочая директория
    WorkDir: string[255];
    //! Количество дней от текущей даты, определяющие данные для архивации
    ArcAfter: Smallint;
    //! Время ежедневной архивации
    ArcTime: TTime;
    //! Часовой пояс (разница с Гринвичем в часах)
    TimeZone: Integer;
    //! Признак объединения последовательных остановок в стоянку
    StopJoin: Boolean;
    //! Признак использования модуля экспедитора для анализа плана-факта
    PlanFactByExpeditor: Boolean;
  end;

//------------------------------------------------------------------------------
//! Функция записи параметров в реестр
//------------------------------------------------------------------------------
{! @param in
   rcMonitoringParams - запись с параметрами
   @return
   Вернет True в случае удачного выполнения. Иначе - False.
}
function SaveMonitoringParamsToRegistry(
  var AMonitoringParams: TMonitoringServerParams;
  const ALogger: TLogger
): Boolean;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! Функция загрузки параметров из реестра
//------------------------------------------------------------------------------
{! @param in
   rcMonitoringParams - запись с параметрами
   @return
   Вернет True в случае удачного выполнения. Иначе - False.
}
function LoadMonitoringParamsFromRegistry(
  var RMonitoringParams: TMonitoringServerParams;
  const ALogger: TLogger
): Boolean;

//------------------------------------------------------------------------------
//! Функция проверки существования ветки параметров в реестре
//------------------------------------------------------------------------------
{! @return
   Вернет True если указанный ключ существует. Иначе - False.
}
function MonitoringParamsKeyExists(): Boolean;

//------------------------------------------------------------------------------
const
  //! Ветка реестра с параметрами
  cMonitoringParamsKey = '\System\CurrentControlSet\Services\ILSMonitoringServer\Parameters';
  //! Название параметра в реестре
  cParamName = 'MonitoringServerParams';

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// Реализация функции записи параметров в реестр
//------------------------------------------------------------------------------
function SaveMonitoringParamsToRegistry(
  var AMonitoringParams: TMonitoringServerParams;
  const ALogger: TLogger
): Boolean;
var
  //! Объект для работы с реестром
  LRegistry: TRegistry;
//------------------------------------------------------------------------------
begin
  Result := True;
  LRegistry := TRegistry.Create();
  LRegistry.RootKey := HKEY_LOCAL_MACHINE;
  try
    LRegistry.OpenKey( cMonitoringParamsKey , True );
    LRegistry.WriteBinaryData( cParamName, AMonitoringParams, SizeOf( AMonitoringParams ) );
    LRegistry.CloseKey();
  except
    on Ex: Exception
    do begin
      Result := False;
      ALogger.ErrorToLog( 'Ошибка записи параметров в реестр:'#13#10 + Ex.Message );
    end;
  end;
  LRegistry.Free();
end;

//------------------------------------------------------------------------------
// Реализация функции загрузки параметров из реестра
//------------------------------------------------------------------------------
function LoadMonitoringParamsFromRegistry(
  var RMonitoringParams: TMonitoringServerParams;
  const ALogger: TLogger
): Boolean;
var
  //! Объект для работы с реестром
  LRegistry: TRegistry;
  //! Число байт
  LByteCount: Integer;
//------------------------------------------------------------------------------
begin
  Result := True;
  LRegistry := TRegistry.Create();
  LRegistry.RootKey := HKEY_LOCAL_MACHINE;
  if not LRegistry.KeyExists( cMonitoringParamsKey )
  then begin
    Result := False;
    ALogger.ErrorToLog( 'Отсутствует ветка реестра. Возможно, служба не установлена.' );
  end
  else begin
    try
      LRegistry.OpenKey( cMonitoringParamsKey, True );
      FillChar( RMonitoringParams, SizeOf( RMonitoringParams ), 0 );
      LByteCount := LRegistry.ReadBinaryData( cParamName, RMonitoringParams, SizeOf( RMonitoringParams ) );
      if ( LByteCount < SizeOf( RMonitoringParams ) )
      then LRegistry.ReadBinaryData( cParamName, RMonitoringParams, LByteCount );
      LRegistry.CloseKey;
    except
      on Ex: Exception
      do begin
        Result := False;
        ALogger.ErrorToLog( 'Ошибка чтения параметров из реестра:'#13#10 + Ex.Message );
      end;
    end;
  end;
  LRegistry.Free();
end;

//------------------------------------------------------------------------------
// Реализация функции проверки существования ветки параметров в реестре
//------------------------------------------------------------------------------
function MonitoringParamsKeyExists(): Boolean;
var
  //! Объект для работы с реестром
  LRegistry: TRegistry;
//------------------------------------------------------------------------------
begin
  LRegistry := TRegistry.Create();
  LRegistry.RootKey := HKEY_LOCAL_MACHINE;
  Result := LRegistry.KeyExists( cMonitoringParamsKey );
  LRegistry.Free();
end;

end.

