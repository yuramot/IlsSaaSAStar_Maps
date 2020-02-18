unit Files.MMF;
//------------------------------------------------------------------------------
// модуль реализует управление отображаемыми в память файлами
//------------------------------------------------------------------------------
// в конструкторе задаются:
//  AFileName - имя файла
//  ARecordSize - размер записи файла (не может быть 0)
//  AReadOnly - открыть файл в режиме "только чтение"
//
// *** поддерживается оператор IN для типизированных указателей ***
//
// свойство Length - текущее количество элементов массива (эквивалент System.Length())
//  - не доступно для записи для файлов "только чтение"
//  - !!!ВАЖНО!!! если при задании размера возникла исключительная ситуация
//                то файлы останутся неотображёнными (используйте ReMap())
//
// свойство Item (свойство по умолчанию) - возвращает указатель по индексу
//
// *** !!! критически важно !!! ***
//  изменение свойсва Length повлечёт переотображение файла;
//  таким образом указатели станут недействительны
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Windows, Math,
  UFiles; // GetFileSize

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс MMF
//------------------------------------------------------------------------------
  TMMFile = class
  private
    //!
    FRecordSize: Integer;
    //!
    FReadOnly: Boolean;
    //!
    FFileHandle: THandle;
    //!
    FMMHandle: THandle;
    //!
    FMapAddr: System.PByte;
    //!
    FLength: Integer;
    //!
    procedure Map();
    //!
    procedure UnMap();
    //!
    procedure SetLength(
      Value: Integer
    );
    //!
    function GetItem(
      Index: Integer
    ): Pointer;
  public
    //!
    {!
      AFileName - имя файла
      ARecordSize - размер записи файла (не может быть 0)
      AReadOnly - открыть файл в режиме "только чтение"
    }
    constructor Create(
      const AFileName: string;
      const ARecordSize: Integer;
      const AReadOnly: Boolean
    );
    //!
    destructor Destroy(); override;
    //!
    procedure ReMap();
    //!
    property Length: Integer read FLength write SetLength;
    //!
    property Item[Index: Integer]: Pointer read GetItem; default;
  public
    type
      TMMFileEnumerator = class
      private
        FMMFile: TMMFile;
        FRef: System.PByte;
        FIdx: Integer;
        function GetCurrent(): Pointer; inline;
      public
        constructor Create(AMMFile: TMMFile);
        property Current: Pointer read GetCurrent;
        function MoveNext(): Boolean; inline;
      end;
  public
    //!
    function GetEnumerator(): TMMFileEnumerator;
  end;

//------------------------------------------------------------------------------
implementation

resourcestring
  RCWrongRecordSize = 'Размер записи некорректен';
  RCNotMultiple = 'Размер файла не кратен размеру записи';
  RCTooLarge = 'Количество записей в файле превышает 2147483647';
  RCNoLengthChange = 'В режиме "только чтение" изменение количества записей невозможно';
  RCWrongRecordCount = 'Запрошенное количество записей некорректно';
  RCWrongIndex = 'Запрошенный индекс записи некорректен';

//------------------------------------------------------------------------------
// TMMFile
//------------------------------------------------------------------------------

constructor TMMFile.Create(
  const AFileName: string;
  const ARecordSize: Integer;
  const AReadOnly: Boolean
);
var
  //!
  Div64, Mod64: UInt64;
//------------------------------------------------------------------------------
begin
  inherited Create();
  //
  FRecordSize := ARecordSize;
  FReadOnly := AReadOnly;
  //
  if (ARecordSize <= 0) then
    raise Exception.CreateRes(@RCWrongRecordSize);
  if AReadOnly and (not FileExists(AFileName)) then
    Exit; // не создавать пустой ReadOnly файл
  FFileHandle := CreateFile(
    PChar(AFileName),
    IfThen(AReadOnly, GENERIC_READ, GENERIC_READ or GENERIC_WRITE),
    FILE_SHARE_READ or FILE_SHARE_DELETE,
    nil,
    OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  if (FFileHandle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError();
  DivMod(TFileHandler.GetFileSize(AFileName), ARecordSize, Div64, Mod64);
  if (Mod64 <> 0) then
    raise Exception.CreateRes(@RCNotMultiple);
  if (Div64 > MaxInt) then
    raise Exception.CreateRes(@RCTooLarge);
  FLength := Div64;
  Map();
end;

destructor TMMFile.Destroy();
begin
  UnMap();
  CloseHandle(FFileHandle);
  //
  inherited Destroy();
end;

procedure TMMFile.ReMap();
begin
  UnMap();
  Map();
end;

procedure TMMFile.Map();
begin
  if (FLength = 0) then
    Exit;
  FMMHandle := CreateFileMapping(FFileHandle, nil, IfThen(FReadOnly, PAGE_READONLY, PAGE_READWRITE), 0, 0, nil);
  if (FMMHandle = 0) then
    RaiseLastOSError();
  FMapAddr := MapViewOfFile(FMMHandle, IfThen(FReadOnly, FILE_MAP_READ, FILE_MAP_WRITE), 0, 0, 0);
  if not Assigned(FMapAddr) then
    RaiseLastOSError();
end;

procedure TMMFile.UnMap();
begin
  UnmapViewOfFile(FMapAddr);
  FMapAddr := nil;
  CloseHandle(FMMHandle);
  FMMHandle := 0;
end;

procedure TMMFile.SetLength(
  Value: Integer
);
var
  //!
  OldLength: Integer;
//------------------------------------------------------------------------------
begin
  OldLength := FLength;
  if FReadOnly then
    raise Exception.CreateRes(@RCNoLengthChange);
  if (Value < 0) then
    raise Exception.CreateRes(@RCWrongRecordCount);
  UnMap();
  if not SetFilePointerEx(FFileHandle, Int64(Value) * Int64(FRecordSize), nil, FILE_BEGIN) then
    RaiseLastOSError();
  if not SetEndOfFile(FFileHandle) then
    RaiseLastOSError();
  FLength := Value;
  Map();
  // документация microsoft говорит что контент при расширении неопределён, так что...
  if (Value > OldLength) then
    FillChar((FMapAddr + NativeInt(OldLength) * NativeInt(FRecordSize))^, (Value - OldLength) * FRecordSize, 0);
end;

function TMMFile.GetItem(
  Index: Integer
): Pointer;
begin
  if (Index < 0) or (Index >= FLength) then
    raise Exception.CreateRes(@RCWrongIndex);
  Result := FMapAddr + NativeInt(Index) * NativeInt(FRecordSize);
end;

function TMMFile.GetEnumerator(): TMMFileEnumerator;
begin
  Result := TMMFileEnumerator.Create(Self);
end;

//------------------------------------------------------------------------------
// TMMFile.TKeyEnumerator
//------------------------------------------------------------------------------

constructor TMMFile.TMMFileEnumerator.Create(
  AMMFile: TMMFile
);
begin
  FMMFile := AMMFile;
  FRef := AMMFile.FMapAddr - AMMFile.FRecordSize;
  FIdx := AMMFile.FLength;
end;

function TMMFile.TMMFileEnumerator.GetCurrent(): Pointer;
begin
  Result := FRef;
end;

function TMMFile.TMMFileEnumerator.MoveNext(): Boolean;
begin
  Result := FIdx <> 0;
  if Result then
  begin
    FRef := FRef + FMMFile.FRecordSize;
    Dec(FIdx);
  end;
end;

end.

