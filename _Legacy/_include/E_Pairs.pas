unit E_Pairs;
//------------------------------------------------------------------------------
// Модуль реализует класс парных значений Integer - Integer
//------------------------------------------------------------------------------
//
// содержит пары "ключ - значение" типов Integer - Integer
//
// !!! НИ ключ, НИ значение не могут повторяться !!!
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! запись парных значений Integer Integer
//------------------------------------------------------------------------------
  PPairII = ^TPairII;

  TPairII = packed record
    Key: Integer;
    Value: Integer;
  end;

  TPairIIArray = array of TPairII;

//------------------------------------------------------------------------------
//! класс парных значений Integer Integer
//------------------------------------------------------------------------------
  TIntPairs = class
  private
    //! массив
    FArray: TPairIIArray;
    //! блокировщик
    FLocker: TMultiReadExclusiveWriteSynchronizer;
  public
    //!
    constructor Create();
    //!
    destructor Destroy(); override;
    //! полная очистка
    procedure ClearAll();
    //! добавление пары
    procedure Add(
      const AKey: Integer;
      const AValue: Integer
    );
    //! удаление по ключу
    procedure RemoveKey(
      const AKey: Integer
    );
    //! удаление по значению
    procedure RemoveValue(
      const AValue: Integer
    );
    //! проверка на наличие ключа в списке
    function ContainsKey(
      const AKey: Integer
    ): Boolean;
    //! проверка на наличие значения в списке
    function ContainsValue(
      const AValue: Integer
    ): Boolean;
    //! получить ключ по значению
    function GetKey(
      const AValue: Integer
    ): Integer;
    //! получить значение по ключу
    function GetValue(
      const AKey: Integer
    ): Integer;
  end;

//------------------------------------------------------------------------------
implementation

const
  CDoubleKey: string = 'Дубликация ключа => %d';
  CDoubleValue: string = 'Дубликация значения => %d';
  CNoKey: string = 'Для данного ключа не найдено значения => %d';
  CNoValue: string = 'Для данного значения не найдено ключа => %d';

//------------------------------------------------------------------------------
// TFileHandler
//------------------------------------------------------------------------------

constructor TIntPairs.Create();
begin
  inherited Create();
  //
  FLocker := TMultiReadExclusiveWriteSynchronizer.Create();
end;

destructor TIntPairs.Destroy();
begin
  ClearAll();
  //
  FLocker.Free();
  //
  inherited Destroy();
end;

procedure TIntPairs.ClearAll();
begin
  FLocker.BeginWrite();
  try
    SetLength( FArray, 0 );
  finally
    FLocker.EndWrite();
  end;
end;

procedure TIntPairs.Add(
  const AKey: Integer;
  const AValue: Integer
);
begin
  FLocker.BeginWrite();
  try
    // в Contains будет вложение Read во Write, но это нормально
    if ContainsKey( AKey ) then
      raise Exception.CreateFmt( CDoubleKey, [AKey] );
    if ContainsValue( AValue ) then
      raise Exception.CreateFmt( CDoubleValue, [AValue] );
    //
    SetLength( FArray, Length( FArray ) + 1 );
    FArray[High( FArray )].Key := AKey;
    FArray[High( FArray )].Value := AValue;
  finally
    FLocker.EndWrite();
  end;
end;

procedure TIntPairs.RemoveKey(
  const AKey: Integer
);
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  FLocker.BeginWrite();
  try
    for I := Low( FArray ) to High( FArray ) do
    begin
      if ( FArray[I].Key = AKey ) then
      begin
        if ( I <> High( FArray ) ) then
          FArray[I] := FArray[High( FArray )];
        SetLength( FArray, High( FArray ) );
        Exit;
      end;
    end;
  finally
    FLocker.EndWrite();
  end;
end;

procedure TIntPairs.RemoveValue(
  const AValue: Integer
);
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  FLocker.BeginWrite();
  try
    for I := Low( FArray ) to High( FArray ) do
    begin
      if ( FArray[I].Value = AValue ) then
      begin
        if ( I <> High( FArray ) ) then
          FArray[I] := FArray[High( FArray )];
        SetLength( FArray, High( FArray ) );
        Exit;
      end;
    end;
  finally
    FLocker.EndWrite();
  end;
end;

function TIntPairs.ContainsKey(
  const AKey: Integer
): Boolean;
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  FLocker.BeginRead();
  try
    for I := Low( FArray ) to High( FArray ) do
    begin
      if ( FArray[I].Key = AKey ) then Exit( True );
    end;
    Result := False;
  finally
    FLocker.EndRead();
  end;
end;

function TIntPairs.ContainsValue(
  const AValue: Integer
): Boolean;
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  FLocker.BeginRead();
  try
    for I := Low( FArray ) to High( FArray ) do
    begin
      if ( FArray[I].Value = AValue ) then Exit( True );
    end;
    Result := False;
  finally
    FLocker.EndRead();
  end;
end;

function TIntPairs.GetKey(
  const AValue: Integer
): Integer;
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  Result := 0; // ! фиктивный ворнингогаситель
  FLocker.BeginRead();
  try
    for I := Low( FArray ) to High( FArray ) do
    begin
      if ( FArray[I].Value = AValue ) then Exit( FArray[I].Key );
    end;
    raise Exception.CreateFmt( CNoValue, [AValue] );
  finally
    FLocker.EndRead();
  end;
end;

function TIntPairs.GetValue(
  const AKey: Integer
): Integer;
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  Result := 0; // ! фиктивный ворнингогаситель
  FLocker.BeginRead();
  try
    for I := Low( FArray ) to High( FArray ) do
    begin
      if ( FArray[I].Key = AKey ) then Exit( FArray[I].Value );
    end;
    raise Exception.CreateFmt( CNoKey, [AKey] );
  finally
    FLocker.EndRead();
  end;
end;

end.

