﻿unit UStructArray;
//------------------------------------------------------------------------------
// юнит класса - массива
//
// шаблонный класс, инкапсулирующий массив из записей
// суть класса - в методах загрузки и сохранения из/в файлы
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Classes;

type
//------------------------------------------------------------------------------
//! абстрактный класс-инкапсулятор
//------------------------------------------------------------------------------
  TStructArray<T> = class
  public
    //!
    class procedure LoadFromFile(
      const AFileName: string;
      var RArray: TArray<T>
    );
    //!
    class procedure SaveToFile(
      const AArray: TArray<T>;
      const AFileName: string
    );
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TStructArray<T>
//------------------------------------------------------------------------------

class procedure TStructArray<T>.LoadFromFile(
  const AFileName: string;
  var RArray: TArray<T>
);
var
  //!
  FStream: TFileStream;
  //!
  FSize: Int64;
//------------------------------------------------------------------------------
begin
  if not FileExists(AFileName) then
  begin
    SetLength(RArray, 0);
    Exit;
  end;

  FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    FSize := FStream.Size;
    if (FSize mod SizeOf(T) <> 0) then
      raise Exception.Create('размер файла "' + AFileName + '" не кратен размеру структуры');
    SetLength(RArray, FSize div SizeOf(T));
    if FSize > 0 then
      FStream.ReadBuffer(RArray[0], FSize);
  finally
    FStream.Free();
  end;
end;

class procedure TStructArray<T>.SaveToFile(
  const AArray: TArray<T>;
  const AFileName: string
);
var
  //!
  FStream: TFileStream;
//------------------------------------------------------------------------------
begin
//**
  FStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyNone);
  try
    FStream.WriteBuffer(AArray[0], Length(AArray) * SizeOf(T));
  finally
    FStream.Free();
  end;
end;

end.

