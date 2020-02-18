unit E_Logger;
//------------------------------------------------------------------------------
// Модуль реализует класс логирования
//------------------------------------------------------------------------------
// Потоко-безопасный логгер
//
// Запись в файл лога ToLog() добавляет запись в лог;
// Запись в файл лога ошибок ErrorToLog() добавляет запись в лог ошибок (суффикс имени файла "_ERR")
//
// В конструктор передаются 3 параметра:
//  ALogFileName - ПОЛНЫЙ путь И ИМЯ файла лога (!однако расширение всегда берётся стандартное!)
//    для базового лога можно использовать аргумент ParamStr( 0 )
//
//  ASizeLimit - размер файла лога (в байтах)
//    после превышения размера текущий файл переименовывается в архивный, а запись продолжается в новый
//    не рекомендуется делать файлы более 10 (лучше 5) мегабайт, т.к. медленно открываются блокнотом
//
//  ANumLimit - кол-во архивных файлов логов
//    совсем старые файлы удаляются
//    не может быть менее 1 файла!
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, SyncObjs, Windows, Math,
  C_FileNames, E_Files, E_UtilsStr, E_UtilsFiles;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс логирования
//------------------------------------------------------------------------------
  TLogger = class sealed
  strict private
    //!
    FFileName: string;
    //!
    FErrorFileName: string;
    //!
    FLocker: TCriticalSection;
    //!
    FSizeLimit: DWORD;
    //!
    FNumLimit: DWORD;
    //!
    procedure ArchLog(
      const AFileName: string
    );
  public
    //!
    constructor Create(
      const ALogFileName: string;
      const ASizeLimit: DWORD;
      const ANumLimit: DWORD
    );
    //!
    destructor Destroy(); override;
    //!
    procedure ToLog(
      const AMessage: string
    );
    //!
    procedure ErrorToLog(
      const AMessage: string
    );
  end;

//------------------------------------------------------------------------------
//! класс логирования в один файл для нескольких приложений
//------------------------------------------------------------------------------
  TMultiAppLogger = class sealed
  strict private
    //!
    FFileName: string;
    //!
    FErrorFileName: string;
    //!
    FLocker: TCriticalSection;
    //!
    FSizeLimit: DWORD;
    //!
    FNumLimit: DWORD;
    //!
    procedure ArchLog(
      const AFileName: string
    );
  public
    //!
    constructor Create(
      const ALogFileName: string;
      const ASizeLimit: DWORD;
      const ANumLimit: DWORD
    );
    //!
    destructor Destroy(); override;
    //!
    procedure ToLog(
      const AMessage: string
    );
    //!
    procedure ErrorToLog(
      const AMessage: string
    );
  end;

//------------------------------------------------------------------------------
//! класс логирования с архивацией в отдельный каталог
//------------------------------------------------------------------------------
  TArchLogger = class sealed
  strict private
    //!
    FFileName: string;
    //!
    FErrorFileName: string;
    //!
    FArchPath: string;
    //!
    FLocker: TCriticalSection;
    //!
    FSizeLimit: DWORD;
    //!
    procedure ArchLog(
      const AFileName: string
    );
  public
    //!
    constructor Create(
      const ALogFileName: string;
      const ASizeLimit: DWORD;
      const AArchPath: string
    );
    //!
    destructor Destroy(); override;
    //!
    procedure ToLog(
      const AMessage: string
    );
    //!
    procedure ErrorToLog(
      const AMessage: string
    );
  end;

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
//! сообщение об ошибке
//------------------------------------------------------------------------------
  CLLogNameError: string = 'Некорректное имя файла лога';

//------------------------------------------------------------------------------
//! формат даты/времени файла архива
//------------------------------------------------------------------------------
  CArchFile: string = '"_"yyyymmdd"_"hhnn';

//------------------------------------------------------------------------------
//! шаблон формата даты/времени файла архива для поиска
//------------------------------------------------------------------------------
  CArchFileForFind: string = '_????????_????';

//------------------------------------------------------------------------------
//! шаблон формата строки лога
//------------------------------------------------------------------------------
  CLogStrFormat: string = '%s %s'#13#10;

//------------------------------------------------------------------------------
// TLogger
//------------------------------------------------------------------------------

constructor TLogger.Create(
  const ALogFileName: string;
  const ASizeLimit: DWORD;
  const ANumLimit: DWORD
);
var
  //!
  LStr: string;
  //!
  LFileH: THandle;
  //!
  LError: DWORD;
  //!
  LExtPos: Integer;
//------------------------------------------------------------------------------
begin
  inherited Create();
  //
  FSizeLimit := ASizeLimit;
  FNumLimit := ANumLimit;
  // файл лога и файл лога ошибок
  LStr := ChangeFileExt( ALogFileName, CLogDefExtDotted );
  LExtPos := PosLastDelimiter( LStr, CExtDelim );
  if ( LExtPos = 0 ) then
    raise Exception.Create( CLLogNameError );
  FFileName := LStr;
  FErrorFileName := Copy( LStr, 1, LExtPos - 1 ) + CErrorLogSuffix + CLogDefExtDotted;
  // для проверки на правильность имени откроем файл
  LFileH := CreateFile(
    PChar( FFileName ),
    GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_DELETE,
    nil,
    CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if ( LFileH = INVALID_HANDLE_VALUE ) then
  begin
    LError := GetLastError();
    if ( LError <> ERROR_FILE_EXISTS ) then // если это просто "файл уже существует" - то всё ок
      RaiseLastOSError( LError );
  end
  else
    CloseHandle( LFileH ); // закроем файл - на данный момент он нам не нужен
  // создаём рабочие компоненты
  FLocker := TCriticalSection.Create();
end;

destructor TLogger.Destroy();
begin
  FLocker.Free();
  //
  inherited Destroy();
end;

procedure TLogger.ArchLog(
  const AFileName: string
);
var
  //!
  LStr: string;
  //!
  LFileH: THandle;
  //!
  LFileSizeLow: DWORD;
  //!
  LFileSizeHigh: DWORD;
  //!
  LCurFileWriteDTDOS: Integer;
  //!
  LCurFileWriteDT: TDateTime;
  //!
  LMinFileWriteDT: TDateTime;
  //!
  LNum: DWORD;
  //!
  LSearchRec: TWIN32FindData;
//------------------------------------------------------------------------------
begin
  // проверяем размер
  LFileH := CreateFile(
    PChar( AFileName ),
    0,
    FILE_SHARE_READ or FILE_SHARE_DELETE or FILE_SHARE_WRITE,
    nil,
    OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if ( LFileH <> INVALID_HANDLE_VALUE ) then
  begin
    LFileSizeLow := GetFileSize( LFileH, @LFileSizeHigh );
    CloseHandle( LFileH );
    if ( LFileSizeLow <> INVALID_FILE_SIZE ) then
    begin
      if ( LFileSizeLow > FSizeLimit ) or ( LFileSizeHigh <> 0 ) then
      begin
        LStr := Copy( AFileName, 1, PosLastDelimiter( AFileName, CExtDelim ) - 1 ) + FormatDateTime( CArchFile, Now() ) + CLogDefExtDotted;
        MoveFile( PChar( AFileName ), PChar( LStr ) );
      end;
    end;
  end;
  // проверяем количество
  LStr := Copy( AFileName, 1, PosLastDelimiter( AFileName, CExtDelim ) - 1 ) + CArchFileForFind + CLogDefExtDotted;
  LFileH := Windows.FindFirstFile( PChar( LStr ), LSearchRec );
  if ( LFileH <> INVALID_HANDLE_VALUE ) then
  begin
    LNum := 1;
    LMinFileWriteDT := Infinity;
    repeat
      // эти две строки - конвертация даты-времени последней записи в файл в тип TDateTime
      FileTimeToDosDateTime( LSearchRec.ftLastWriteTime, LongRec( LCurFileWriteDTDOS ).Hi, LongRec( LCurFileWriteDTDOS ).Lo );
      LCurFileWriteDT := FileDateToDateTime ( LCurFileWriteDTDOS );
      //
      if ( LMinFileWriteDT > LCurFileWriteDT ) then
      begin
        LMinFileWriteDT := LCurFileWriteDT;
        LStr := QExtractFilePath( AFileName ) + string( LSearchRec.cFileName );
      end;
      if not Windows.FindNextFile( LFileH, LSearchRec ) then Break;
      Inc( LNum );
    until False;
    Windows.FindClose( LFileH );
    if ( LNum > FNumLimit ) then
      DeleteFile( PChar( LStr ) );
  end;
end;

procedure TLogger.ToLog(
  const AMessage: string
);
var
  //!
  LFormatted: string;
  //!
  LFile: TFileHandler;
//------------------------------------------------------------------------------
begin
  if ( AMessage = '' ) then Exit;
  try
    FLocker.Acquire();
    try
      // проверка
      ArchLog( FFileName );
      // форматируем
      LFormatted := Format( CLogStrFormat, [FormatDateTime( CDateTimeFormat, Now() ), AMessage] );
      // пишем
      LFile := TFileHandler.Create( FFileName, famWrite );
      try
        LFile.ReadFromMem( @LFormatted[1], Length( LFormatted ) * SizeOf( Char ) );
      finally
        LFile.Free();
      end;
    finally
      FLocker.Release();
    end;
  except
    // игнорируем все ошибки
  end;
end;

procedure TLogger.ErrorToLog(
  const AMessage: string
);
var
  //!
  LFormatted: string;
  //!
  LFile: TFileHandler;
//------------------------------------------------------------------------------
begin
  if ( AMessage = '' ) then Exit;
  try
    FLocker.Acquire();
    try
      // проверка
      ArchLog( FErrorFileName );
      // форматируем
      LFormatted := Format( CLogStrFormat, [FormatDateTime( CDateTimeFormat, Now() ), AMessage] );
      LFile := TFileHandler.Create( FErrorFileName, famWrite );
      try
        LFile.ReadFromMem( @LFormatted[1], Length( LFormatted ) * SizeOf( Char ) );
      finally
        LFile.Free();
      end;
    finally
      FLocker.Release();
    end;
  except
    // игнорируем все ошибки
  end;
end;

//------------------------------------------------------------------------------
// TMultiAppLogger
//------------------------------------------------------------------------------

constructor TMultiAppLogger.Create(
  const ALogFileName: string;
  const ASizeLimit: DWORD;
  const ANumLimit: DWORD
);
var
  //!
  LStr: string;
  //!
  LFileH: THandle;
  //!
  LError: DWORD;
  //!
  LExtPos: Integer;
//------------------------------------------------------------------------------
begin
  inherited Create();
  //
  FSizeLimit := ASizeLimit;
  FNumLimit := ANumLimit;
  // файл лога и файл лога ошибок
  LStr := ChangeFileExt( ALogFileName, CLogDefExtDotted );
  LExtPos := PosLastDelimiter( LStr, CExtDelim );
  if ( LExtPos = 0 ) then
    raise Exception.Create( CLLogNameError );
  FFileName := LStr;
  FErrorFileName := Copy( LStr, 1, LExtPos - 1 ) + CErrorLogSuffix + CLogDefExtDotted;
  // для проверки на правильность имени откроем файл
  LFileH := CreateFile(
    PChar( FFileName ),
    GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_DELETE,
    nil,
    CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if ( LFileH = INVALID_HANDLE_VALUE ) then
  begin
    LError := GetLastError();
    if ( LError <> ERROR_FILE_EXISTS ) then // если это просто "файл уже существует" - то всё ок
      RaiseLastOSError( LError );
  end
  else
    CloseHandle( LFileH ); // закроем файл - на данный момент он нам не нужен
  // создаём рабочие компоненты
  FLocker := TCriticalSection.Create();
end;

destructor TMultiAppLogger.Destroy();
begin
  inherited Destroy();
end;

procedure TMultiAppLogger.ArchLog(
  const AFileName: string
);
var
  //!
  LStr: string;
  //!
  LFileH: THandle;
  //!
  LFileSizeLow: DWORD;
  //!
  LFileSizeHigh: DWORD;
  //!
  LCurFileWriteDTDOS: Integer;
  //!
  LCurFileWriteDT: TDateTime;
  //!
  LMinFileWriteDT: TDateTime;
  //!
  LNum: DWORD;
  //!
  LSearchRec: TWIN32FindData;
//------------------------------------------------------------------------------
begin
  // проверяем размер
  LFileH := CreateFile(
    PChar( AFileName ),
    0,
    FILE_SHARE_READ or FILE_SHARE_DELETE or FILE_SHARE_WRITE,
    nil,
    OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if ( LFileH <> INVALID_HANDLE_VALUE ) then
  begin
    LFileSizeLow := GetFileSize( LFileH, @LFileSizeHigh );
    CloseHandle( LFileH );
    if ( LFileSizeLow <> INVALID_FILE_SIZE ) then
    begin
      if ( LFileSizeLow > FSizeLimit ) or ( LFileSizeHigh <> 0 ) then
      begin
        LStr := Copy( AFileName, 1, PosLastDelimiter( AFileName, CExtDelim ) - 1 ) + FormatDateTime( CArchFile, Now() ) + CLogDefExtDotted;
        MoveFile( PChar( AFileName ), PChar( LStr ) );
      end;
    end;
  end;
  // проверяем количество
  LStr := Copy( AFileName, 1, PosLastDelimiter( AFileName, CExtDelim ) - 1 ) + CArchFileForFind + CLogDefExtDotted;
  LFileH := Windows.FindFirstFile( PChar( LStr ), LSearchRec );
  if ( LFileH <> INVALID_HANDLE_VALUE ) then
  begin
    LNum := 1;
    LMinFileWriteDT := Infinity;
    repeat
      // эти две строки - конвертация даты-времени последней записи в файл в тип TDateTime
      FileTimeToDosDateTime( LSearchRec.ftLastWriteTime, LongRec( LCurFileWriteDTDOS ).Hi, LongRec( LCurFileWriteDTDOS ).Lo );
      LCurFileWriteDT := FileDateToDateTime ( LCurFileWriteDTDOS );
      //
      if ( LMinFileWriteDT > LCurFileWriteDT ) then
      begin
        LMinFileWriteDT := LCurFileWriteDT;
        LStr := QExtractFilePath( AFileName ) + string( LSearchRec.cFileName );
      end;
      if not Windows.FindNextFile( LFileH, LSearchRec ) then Break;
      Inc( LNum );
    until False;
    Windows.FindClose( LFileH );
    if ( LNum > FNumLimit ) then
      DeleteFile( PChar( LStr ) );
  end;
end;

procedure TMultiAppLogger.ToLog(
  const AMessage: string
);
var
  //!
  LFormatted: string;
  //!
  LFile: TFileHandler;
  //!
  LFlag: Boolean;
//------------------------------------------------------------------------------
begin
  if ( AMessage = '' ) then Exit;
  LFile := nil;
  try
    FLocker.Acquire();
    try
      // проверка
      ArchLog( FFileName );
      // форматируем
      LFormatted := Format( CLogStrFormat, [FormatDateTime( CDateTimeFormat, Now() ), AMessage] );
// !!! ЭКСПЕРИМЕНТАЛЬНО !!! может в бесконечный цикл !!!
      LFlag := True;
      while LFlag do
      begin
        try
          LFile := TFileHandler.Create( FFileName, famWrite );
          LFlag := False;
        except
          //
        end;
        Windows.Sleep( 0 );
      end;
      LFlag := True;
      while LFlag do
      begin
        try
          LFile.ReadFromMem( @LFormatted[1], Length( LFormatted ) * SizeOf( Char ) );
          LFlag := False;
        except
          //
        end;
        Windows.Sleep( 0 );
      end;
// !!!
      LFile.Free();
    finally
      FLocker.Release();
    end;
  except
    // игнорируем все ошибки
  end;
end;

procedure TMultiAppLogger.ErrorToLog(
  const AMessage: string
);
var
  //!
  LFormatted: string;
  //!
  LFile: TFileHandler;
  //!
  LFlag: Boolean;
//------------------------------------------------------------------------------
begin
  if ( AMessage = '' ) then Exit;
  LFile := nil;
  try
    FLocker.Acquire();
    try
      // проверка
      ArchLog( FErrorFileName );
      // форматируем
      LFormatted := Format( CLogStrFormat, [FormatDateTime( CDateTimeFormat, Now() ), AMessage] );
// !!! ЭКСПЕРИМЕНТАЛЬНО !!! может в бесконечный цикл !!!
      LFlag := True;
      while LFlag do
      begin
        try
          LFile := TFileHandler.Create( FErrorFileName, famWrite );
          LFlag := False;
        except
          //
        end;
        Windows.Sleep( 0 );
      end;
      LFlag := True;
      while LFlag do
      begin
        try
          LFile.ReadFromMem( @LFormatted[1], Length( LFormatted ) * SizeOf( Char ) );
          LFlag := False;
        except
          //
        end;
        Windows.Sleep( 0 );
      end;
// !!!
      LFile.Free();
    finally
      FLocker.Release();
    end;
  except
    // игнорируем все ошибки
  end;
end;

//------------------------------------------------------------------------------
// TArchLogger
//------------------------------------------------------------------------------

constructor TArchLogger.Create(
  const ALogFileName: string;
  const ASizeLimit: DWORD;
  const AArchPath: string
);
var
  //!
  LStr: string;
  //!
  LFileH: THandle;
  //!
  LError: DWORD;
  //!
  LExtPos: Integer;
//------------------------------------------------------------------------------
begin
  inherited Create();
  //
  FSizeLimit := ASizeLimit;
  FArchPath := AArchPath;
  // файл лога и файл лога ошибок
  LStr := ChangeFileExt( ALogFileName, CLogDefExtDotted );
  LExtPos := PosLastDelimiter( LStr, CExtDelim );
  if ( LExtPos = 0 ) then
    raise Exception.Create( CLLogNameError );
  FFileName := LStr;
  FErrorFileName := Copy( LStr, 1, LExtPos - 1 ) + CErrorLogSuffix + CLogDefExtDotted;
  // для проверки на правильность имени откроем файл
  LFileH := CreateFile(
    PChar( FFileName ),
    GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_DELETE,
    nil,
    CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if ( LFileH = INVALID_HANDLE_VALUE ) then
  begin
    LError := GetLastError();
    if ( LError <> ERROR_FILE_EXISTS ) then // если это просто "файл уже существует" - то всё ок
      RaiseLastOSError( LError );
  end
  else
    CloseHandle( LFileH ); // закроем файл - на данный момент он нам не нужен
  // создаём рабочие компоненты
  FLocker := TCriticalSection.Create();
end;

destructor TArchLogger.Destroy();
begin
  FLocker.Free();
  //
  inherited Destroy();
end;

procedure TArchLogger.ArchLog(
  const AFileName: string
);
label R;
var
  //! размер файла
  FileSize: Int64;
  //!
  CurFileWriteDT: TDateTime;
  //!
  MinFileWriteDT: TDateTime;
  //! имя файла без пути и расширения
  PureFileName: string;
  //! путь и имя файла в архиве
  FileDestName: string;
  //! шаблон поиска в архиве
  FileTemplate: string;
  //! путь и имя файла дл удаления
  FileToDelete: string;
  //! дескриптор файла при поиске
  FileH: THandle;
  //! запись поиска
  SearchRec: TWIN32FindData;
  //! для конвертации даты-времени записи в файл
  CurFileWriteDTDOS: Integer;
  //!
  Counter: DWORD;
//------------------------------------------------------------------------------
begin
  // проверяем размер
  FileSize := TFileHandler.GetFileSize( AFileName );
  if ( FileSize < Int64( FSizeLimit ) ) then Exit;
  // размер превышен - перемещаем
  PureFileName := QExcludeFileExt( QExcludeFilePath( AFileName ) );
  FileDestName := FArchPath + PureFileName + FormatDateTime( CArchFile, Now() ) + CLogDefExtDotted;
  MoveFileEx( PChar( AFileName ), PChar( FileDestName ), MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH );
  // проверяем количество (уже в папке архива)
  FileTemplate := FArchPath + PureFileName + CArchFileForFind + CLogDefExtDotted;
R:
  FileH := Windows.FindFirstFile( PChar( FileTemplate ), SearchRec );
  if ( FileH = INVALID_HANDLE_VALUE ) then Exit;
  Counter := 1;
  MinFileWriteDT := Infinity;
  repeat
    // эти две строки - конвертация даты-времени последней записи в файл в тип TDateTime
    FileTimeToDosDateTime( SearchRec.ftLastWriteTime, LongRec( CurFileWriteDTDOS ).Hi, LongRec( CurFileWriteDTDOS ).Lo );
    CurFileWriteDT := FileDateToDateTime ( CurFileWriteDTDOS );
    //
    if ( MinFileWriteDT > CurFileWriteDT ) then
    begin
      MinFileWriteDT := CurFileWriteDT;
      FileToDelete := FArchPath + string( SearchRec.cFileName );
    end;
    if not Windows.FindNextFile( FileH, SearchRec ) then Break;
    Inc( Counter );
  until False;
  Windows.FindClose( FileH );
  //
  if ( Counter > 1 ) then
  begin
    // удаляем архив
    DeleteFile( PChar( FileToDelete ) );
    // повторить до удаления всех ненужных архивов
    goto R;
  end;
end;

procedure TArchLogger.ToLog(
  const AMessage: string
);
var
  //!
  LFormatted: string;
  //!
  LFile: TFileHandler;
//------------------------------------------------------------------------------
begin
  if ( AMessage = '' ) then Exit;
  try
    FLocker.Acquire();
    try
      // проверка
      ArchLog( FFileName );
      // форматируем
      LFormatted := Format( CLogStrFormat, [FormatDateTime( CDateTimeFormat, Now() ), AMessage] );
      // пишем
      LFile := TFileHandler.Create( FFileName, famWrite );
      try
        LFile.ReadFromMem( @LFormatted[1], Length( LFormatted ) * SizeOf( Char ) );
      finally
        LFile.Free();
      end;
    finally
      FLocker.Release();
    end;
  except
    // игнорируем все ошибки
  end;
end;

procedure TArchLogger.ErrorToLog(
  const AMessage: string
);
var
  //!
  LFormatted: string;
  //!
  LFile: TFileHandler;
//------------------------------------------------------------------------------
begin
  if ( AMessage = '' ) then Exit;
  try
    FLocker.Acquire();
    try
      // проверка
      ArchLog( FErrorFileName );
      // форматируем
      LFormatted := Format( CLogStrFormat, [FormatDateTime( CDateTimeFormat, Now() ), AMessage] );
      LFile := TFileHandler.Create( FErrorFileName, famWrite );
      try
        LFile.ReadFromMem( @LFormatted[1], Length( LFormatted ) * SizeOf( Char ) );
      finally
        LFile.Free();
      end;
    finally
      FLocker.Release();
    end;
  except
    // игнорируем все ошибки
  end;
end;

end.

