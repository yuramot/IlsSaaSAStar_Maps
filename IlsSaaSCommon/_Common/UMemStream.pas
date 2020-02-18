unit UMemStream;
//------------------------------------------------------------------------------
// модуль реализует поток памяти по принципу FIFO,
//  но с возможностью повторного чтения
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  Ils.Utils;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! поток памяти по принципу FIFO
//------------------------------------------------------------------------------
  TMyMemStream = class
  private
    //! указатель на буфер памяти
    FData: PByte;
    //! указатель на текую позицию чтения
    FPosition: PByte;
    //! объём данных в буфере
    FSize: Integer;
    //! объём буфера
    FCapacity: Integer;
    //! проперть
    function Get_Remaining(): Integer;
  public
    //!
    constructor Create();
    //!
    destructor Destroy(); override;
    //! прочесть байт
    function ReadByte(
      var RWhere: UInt8
    ): Boolean;
    //! прочесть слово = 2 байта
    //! AReverse = true - развернуть порядок байт
    function ReadWord(
      const AReverse: Boolean;
      var RWhere: UInt16
    ): Boolean;
    //! прочесть двойное слово = 4 байта
    //! AReverse = true - развернуть порядок байт
    function ReadDWord(
      const AReverse: Boolean;
      var RWhere: UInt32
    ): Boolean;
    //! прочесть четверное слово = 8 байт
    //! AReverse = true - развернуть порядок байт
    function ReadQWord(
      const AReverse: Boolean;
      var RWhere: UInt64
    ): Boolean;
    //! считать данные
    function ReadData(
      const ANumber: Integer;
      const RWhere: Pointer
    ): Boolean;
    //! пропустить данные
    function SkipData(
      const ANumber: Integer
    ): Boolean;
    //! добавить данные
    procedure AddData(
      const ANumber: Integer;
      const AFrom: Pointer
    );
    //! полностью очистить буфер
    procedure ClearAll();
    //! очистить буфер от данных, прочитанных ранее через ReadXXX
    procedure ClearReaded();
    //! вернуть указатель буфера на момент последнего ClearReaded
    //! т.е. отменить предыдущие операции чтения
    procedure UnreadReaded();
    //! объём непрочитанных данных
    property Remaining: Integer read Get_Remaining;
  end;

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
//! переменные управление буфером данных
//! гранулярность выделения памяти = 256 байт
//------------------------------------------------------------------------------
  CMemPage = Integer($00000100);
  CMemGran = Integer($FFFFFF00);
  CMemSmoother = Integer($000000FF);

//------------------------------------------------------------------------------
// TMyMemStream
//------------------------------------------------------------------------------

constructor TMyMemStream.Create();
begin
  inherited Create();
  //
  ClearAll(); // тут FData = nil
end;

destructor TMyMemStream.Destroy();
begin
  if Assigned(FData) then // на всякий пожарный
    FreeMem(FData);
  //
  inherited Destroy();
end;

function TMyMemStream.ReadByte(
  var RWhere: UInt8
): Boolean;
begin
  if (FPosition - FData > NativeInt(FSize - 1)) then Exit(False);
  RWhere := PByte(FPosition)^;
  FPosition := FPosition + 1;
  Result := True;
end;

function TMyMemStream.ReadWord(
  const AReverse: Boolean;
  var RWhere: UInt16
): Boolean;
begin
  if (FPosition - FData > NativeInt(FSize - 2)) then Exit(False);
  if AReverse then
    RWhere :=  Swap16(PWord(FPosition)^)
  else
    RWhere := PWord(FPosition)^;
  FPosition := FPosition + 2;
  Result := True;
end;

function TMyMemStream.ReadDWord(
  const AReverse: Boolean;
  var RWhere: UInt32
): Boolean;
begin
  if (FPosition - FData > NativeInt(FSize - 4)) then Exit(False);
  if AReverse then
    RWhere :=  Swap32(PCardinal(FPosition)^)
  else
    RWhere := PCardinal(FPosition)^;
  FPosition := FPosition + 4;
  Result := True;
end;

function TMyMemStream.ReadQWord(
  const AReverse: Boolean;
  var RWhere: UInt64
): Boolean;
begin
  if (FPosition - FData > NativeInt(FSize - 8)) then Exit(False);
  if AReverse then
    RWhere :=  Swap64(PUInt64(FPosition)^)
  else
    RWhere := PUInt64(FPosition)^;
  FPosition := FPosition + 8;
  Result := True;
end;

function TMyMemStream.ReadData(
  const ANumber: Integer;
  const RWhere: Pointer
): Boolean;
begin
  if (FPosition - FData > NativeInt(FSize - ANumber)) then Exit(False);
  Move(FPosition^, RWhere^, ANumber);
  FPosition := FPosition + ANumber;
  Result := True;
end;

function TMyMemStream.SkipData(
  const ANumber: Integer
): Boolean;
begin
  if (FPosition - FData > NativeInt(FSize - ANumber)) then Exit(False);
  FPosition := FPosition + ANumber;
  Result := True;
end;

procedure TMyMemStream.AddData(
  const ANumber: Integer;
  const AFrom: Pointer
);
var
  //! новый объём данных в буфере
  NewSize: Integer;
  //! новый объём буфера
  NewSizeGran: Integer;
  //! позиция чтения - числом
  Shift: NativeInt;
  //! позиция добавления новых данных
  AddPos: PByte;
//------------------------------------------------------------------------------
begin
  // новый объём данных
  NewSize := FSize + ANumber;
  // если не хватает текущего буфера
  if (FCapacity < NewSize) then
  begin // то выделим новый и перезапишем
    // сохраним позицию чтения как число
    Shift := FPosition - FData;
    // новый полный объём буфера
    NewSizeGran := (NewSize + CMemSmoother) and CMemGran;
    // выделяем новый размер штатными средствами - сохраняем старые данные
    ReallocMem(FData, NewSizeGran);
    // новый объём буфера
    FCapacity := NewSizeGran;
    // новая позиция
    FPosition := FData + Shift;
  end;
  // указатель позиции добавления
  AddPos := FData + FSize;
  // добавляем новое
  Move(AFrom^, AddPos^, ANumber);
  // устанавливаем новый объём данных
  FSize := NewSize;
end;

procedure TMyMemStream.ClearAll();
begin
  if Assigned(FData) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  GetMem(FData, CMemPage);
  FPosition := FData;
  FSize := 0;
  FCapacity := CMemPage;
end;

procedure TMyMemStream.ClearReaded();
var
  //! новый буфер
  NewBuffer: PByte;
  //! новый объём данных в буфере
  NewSize: Integer;
  //! новый объём буфера
  NewSizeGran: Integer;
//------------------------------------------------------------------------------
begin
  if (FPosition = FData) then Exit; // ничего не прочитано
  if (FPosition - FData = NativeInt(FSize)) then
    ClearAll() // всё прочитано
  else
  begin // частично прочитано
    // новый объём данных
    NewSize := Get_Remaining();
    // новый полный объём буфера
    NewSizeGran := (NewSize + CMemSmoother) and CMemGran;
    // выделяем память
    GetMem(NewBuffer, NewSizeGran);
    // перемещаем остаток
    Move(FPosition^, NewBuffer^, NewSize);
    // освобождаем старую
    FreeMem(FData);
    // новые параметры
    FData := NewBuffer;
    FPosition := NewBuffer; // теперь с нуля
    FSize := NewSize;
    FCapacity := NewSizeGran;
  end;
end;

procedure TMyMemStream.UnreadReaded();
begin
  FPosition := FData;
end;

function TMyMemStream.Get_Remaining(): Integer;
begin
  Result := FSize - Integer(FPosition - FData);
end;

//------------------------------------------------------------------------------
initialization
  IsMultiThread := True;

end.

