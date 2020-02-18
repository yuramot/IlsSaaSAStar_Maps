unit UFiles;
//------------------------------------------------------------------------------
// модуль реализует класс доступа к файлам
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Windows;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! режим открытия файла
//------------------------------------------------------------------------------
  TFileAccessMode = (famRead, famReadStrict, famWrite, famWriteStrict);

//------------------------------------------------------------------------------
//! класс доступа к файлам
//------------------------------------------------------------------------------
  TFileHandler = class
  private
    //! windows handle
    FHandle: THandle;
    //! режим открытия
    FMode: TFileAccessMode;
    //! текущий размер
    FSize: Int64;
    //! текущая позиция
    FPosition: Int64;
    //!
    procedure Set_Size(
      ASize: Int64
    );
    //!
    procedure Set_Position(
      APosition: Int64
    );
  public
    //! размер
    property Size: Int64 read FSize write Set_Size;
    //! позиция
    property Position: Int64 read FPosition write Set_Position;
    //!
    constructor Create(
      const AFileName: string;
      const AMode: TFileAccessMode
    );
    //!
    destructor Destroy(); override;
    //! чтение из памяти в файл
    //!  = запись в наш файл
    procedure ReadFromMem(
      const AMem: Pointer;
      const ASize: DWORD
    );
    //! запись в память из файла
    //!  = чтение из нашего файла
    procedure WriteToMem(
      const AMem: Pointer;
      const ASize: DWORD
    );
    //! чтение из внешнего файла в файл
    //!  = запись в наш файл
    procedure ReadFromFile(
      const AFileName: string
    );
    //! запись во внешний файл из файла
    //!  = чтение из нашего файла
    procedure WriteToFile(
      const AFileName: string
    );
    //! переименование файла - делает несколько попыток (CTryCount)
    //! классовая ф.
    {!
      @return
      Вернёт True при успешном переименовании, иначе False
    }
    class function RenameFile(
      const AFrom: string;
      const ATo: string
    ): Boolean;
    //! удаление файла - делает несколько попыток (CTryCount)
    //! классовая ф.
    {!
      @return
      Вернёт True при успешном удалении, иначе False
    }
    class function DeleteFile(
      const AFileName: string
    ): Boolean;
    //! получить размер файла
    //! классовая ф.
    {!
      @return
      Вернёт размер файла при успешном его определении, иначе -1
    }
    class function GetFileSize(
      const AFileName: string
    ): Int64;
    //! попытаться открыть файл конечное число (COpenCount) раз
    //! классовая ф.
    {!
      @param in
      AFileName - полный путь к файлу
      @param in
      AMode - режим открытия файла
      @param out
      RFile - класс файла
      @param out
      RErrorMes - сообщение об ошибке
      @return
      Вернет True при успешном открытии, иначе - False
    }
    class function TryCreate(
      const AFileName: string;
      const AMode: TFileAccessMode;
      var RFile: TFileHandler;
      var RErrorMes: string
    ): Boolean;
  end;

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
//! количество попыток для переименования и удаления
//------------------------------------------------------------------------------
  CTryCount = 10;

//------------------------------------------------------------------------------
//! количество попыток для открытия/создания
//------------------------------------------------------------------------------
  COpenCount = 100;

//------------------------------------------------------------------------------
//! размер буфера (= размеру страницы)
//------------------------------------------------------------------------------
  CThreshold = 4096;

//------------------------------------------------------------------------------
//! размер буфера как Int64
//------------------------------------------------------------------------------
  CThresholdL: Int64 = CThreshold;

//------------------------------------------------------------------------------
//! сообщения об ошибках
//------------------------------------------------------------------------------
  CFHWrongOperation: string = 'Операция противоречит режиму открытия файла';
  CFHWriteError: string = 'Ошибка записи в файл: записано меньше запрошенного';
  CFHReadError: string = 'Ошибка чтения из файла: прочитано меньше запрошенного';
  CFHReadSizeError: string = 'Попытка задания размера для файла на чтение';
  CFHReadPositionError: string = 'Попытка задания позиции более размера для файла на чтение'#13#10'( %u / %u )';

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! для функции GetFileSize
//------------------------------------------------------------------------------
  TRecordQWord = packed record
    case Boolean of
      False: (QW: UInt64);
      True: (DW0, DW1: Uint32);
  end;

//------------------------------------------------------------------------------
// TFileHandler
//------------------------------------------------------------------------------

constructor TFileHandler.Create(
  const AFileName: string;
  const AMode: TFileAccessMode
);
var
  //!
  AccessR: DWORD;
  //!
  ShareR: DWORD;
  //!
  Position: DWORD;
  //!
  ErrorCode: DWORD;
//------------------------------------------------------------------------------
begin
  inherited Create();
  //
  FMode := AMode;
  //
  if (AMode = famRead) or (AMode = famReadStrict) then
  begin
    AccessR := GENERIC_READ;
    Position := FILE_BEGIN; // по-умолчанию читаем сначала
  end
  else
  begin // (AMode = famWrite) or (AMode = famWriteStrict) // и другого не дано, чтобы там компилятор себе не думал
    AccessR := GENERIC_WRITE;
    Position := FILE_END; // по-умолчанию пишем в конец
  end;
  //
  ShareR := 0;
  case AMode of
    famRead: ShareR := FILE_SHARE_WRITE or FILE_SHARE_READ;
    famWrite: ShareR := FILE_SHARE_READ or FILE_SHARE_DELETE;
  end;
  //
  FHandle := CreateFile(
    PChar(AFileName),
    AccessR,
    ShareR,
    nil,
    OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
//    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH, // немедленно записывать на диск <- !!! НЕ ИСПОЛЬЗОВАТЬ !!!
    0
  );
  //
  ErrorCode := GetLastError();
  if (FHandle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError(ErrorCode);
  // берём размер
  if not SetFilePointerEx(FHandle, 0, @FSize, FILE_END) then
    RaiseLastOSError();
  // устанавливаем позицию
  if not SetFilePointerEx(FHandle, 0, @FPosition, Position) then
    RaiseLastOSError();
end;

destructor TFileHandler.Destroy();
begin
  if (FHandle <> INVALID_HANDLE_VALUE) and (FHandle <> 0) then
    CloseHandle(FHandle);
  //
  inherited Destroy();
end;

procedure TFileHandler.Set_Size(
  ASize: Int64
);
begin
  if (FMode = famRead) or (FMode = famReadStrict) then
    raise Exception.Create(CFHReadSizeError);
  if not SetFilePointerEx(FHandle, ASize, @FPosition, FILE_BEGIN) then
    RaiseLastOSError();
  if not SetEndOfFile(FHandle) then
    RaiseLastOSError();
end;

procedure TFileHandler.Set_Position(
  APosition: Int64
);
begin
  if (FMode = famRead) or (FMode = famReadStrict) then
  begin
    if (APosition > FSize) then // отсутствие проверки на равенство - не ошибка
      raise Exception.CreateFmt(CFHReadPositionError, [APosition, FSize]);
  end;
  if not SetFilePointerEx(FHandle, APosition, @FPosition, FILE_BEGIN) then
    RaiseLastOSError();
end;

procedure TFileHandler.ReadFromMem(
  const AMem: Pointer;
  const ASize: DWORD
);
var
  //!
  Actual: DWORD;
//------------------------------------------------------------------------------
begin
  if (FMode = famRead) or (FMode = famReadStrict) then
    raise Exception.Create(CFHWrongOperation);
  if not Windows.WriteFile(FHandle, AMem^, ASize, Actual, nil) then
    RaiseLastOSError();
  if (ASize <> Actual) then
    raise Exception.Create(CFHWriteError);
  FPosition := FPosition + Int64(ASize);
  if (FSize < FPosition) then
    FSize := FPosition;
end;

procedure TFileHandler.WriteToMem(
  const AMem: Pointer;
  const ASize: DWORD
);
var
  //!
  Actual: DWORD;
//------------------------------------------------------------------------------
begin
  if (FMode = famWrite) or (FMode = famWriteStrict) then
    raise Exception.Create(CFHWrongOperation);
  if not Windows.ReadFile(FHandle, AMem^, ASize, Actual, nil) then
    RaiseLastOSError();
  if (ASize <> Actual) then
    raise Exception.Create(CFHReadError);
  FPosition := FPosition + Int64(ASize);
end;

procedure TFileHandler.ReadFromFile(
  const AFileName: string
);
var
  //!
  FileInput: TFileHandler;
  //!
  Buffer: Pointer;
  //!
  Rem: Integer;
//------------------------------------------------------------------------------
begin
  if (FMode = famRead) or (FMode = famReadStrict) then
    raise Exception.Create(CFHWrongOperation);
  FileInput := TFileHandler.Create(AFileName, famReadStrict);
  try
    GetMem(Buffer, CThreshold);
    try
      while (FileInput.Size - FileInput.Position > CThresholdL) do
      begin
        FileInput.WriteToMem(Buffer, CThreshold);
        ReadFromMem(Buffer, CThreshold);
      end;
      Rem := Integer(FileInput.Size - FileInput.Position);
      FileInput.WriteToMem(Buffer, Rem);
      ReadFromMem(Buffer, Rem);
    finally
      FreeMem(Buffer);
    end;
  finally
    FileInput.Free();
  end;
end;

procedure TFileHandler.WriteToFile(
  const AFileName: string
);
var
  //!
  FileOutput: TFileHandler;
  //!
  Buffer: Pointer;
  //!
  Rem: Integer;
//------------------------------------------------------------------------------
begin
  if (FMode = famWrite) or (FMode = famWriteStrict) then
    raise Exception.Create(CFHWrongOperation);
  FileOutput := TFileHandler.Create(AFileName, famWriteStrict);
  try
    GetMem(Buffer, CThreshold);
    try
      Position := 0; // пишем всегда весь файл
      while (Size - Position > CThresholdL) do
      begin
        WriteToMem(Buffer, CThreshold);
        FileOutput.ReadFromMem(Buffer, CThreshold);
      end;
      Rem := Integer(Size - Position);
      WriteToMem(Buffer, Rem);
      FileOutput.ReadFromMem(Buffer, Rem);
    finally
      FreeMem(Buffer);
    end;
  finally
    FileOutput.Free();
  end;
end;

class function TFileHandler.RenameFile(
  const AFrom: string;
  const ATo: string
): Boolean;
var
  //!
  Conter: Integer;
//------------------------------------------------------------------------------
begin
  Result := False;
  if not SysUtils.FileExists(AFrom) then Exit;
  if SysUtils.FileExists(ATo) then Exit;
  Conter := CTryCount;
  repeat
    if SysUtils.RenameFile(AFrom, ATo) then Exit(True);
    Dec(Conter);
    Windows.Sleep(1); // ждём не менее цикла
  until (Conter = 0);
end;

class function TFileHandler.DeleteFile(
  const AFileName: string
): Boolean;
var
  //!
  Conter: Integer;
//------------------------------------------------------------------------------
begin
  Result := True;
  if not SysUtils.FileExists(AFileName) then Exit;
  Conter := CTryCount;
  repeat
    if SysUtils.DeleteFile(AFileName) then Exit;
    Dec(Conter);
    Windows.Sleep(1); // ждём не менее цикла
  until (Conter = 0);
  Result := False;
end;

class function TFileHandler.GetFileSize(
  const AFileName: string
): Int64;
var
  //!
  FileAttr: TWin32FileAttributeData;
  //!
  Rez: TRecordQWord;
//------------------------------------------------------------------------------
begin
  Result := -1;
  if (AFileName = '') then Exit;
  if not Windows.GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @FileAttr) then Exit;
  Rez.DW0 := FileAttr.nFileSizeLow;
  Rez.DW1 := FileAttr.nFileSizeHigh;
  Result := Rez.QW;
end;

class function TFileHandler.TryCreate(
  const AFileName: string;
  const AMode: TFileAccessMode;
  var RFile: TFileHandler;
  var RErrorMes: string
): Boolean;
var
  //!
  Counter: Integer;
//------------------------------------------------------------------------------
begin
  Result := False;
  RFile := nil;
  RErrorMes := '';
  for Counter := 0 to COpenCount do
  begin
    try
      RFile := TFileHandler.Create(AFileName, AMode);
      Exit(True);
    except
      on Ex: Exception do
      begin
        RErrorMes := Ex.Message;
        Windows.Sleep(1); // ждём не менее цикла
      end;
    end;
  end;
end;

end.

