unit Watermarks.INI;
//------------------------------------------------------------------------------
// класс watermark'ов
//  работающий на ini-файлах
//
// параметры конструктора:
//  AFileName      - имя файла
//  ASectionName   - название скции в файле
//  ASaveInterval  - интервал между сохранениями, секунд
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, IniFiles,
  Classes, // TStringList
  System.Generics.Collections,
  Watermarks.Base,
  Ils.Utils;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TSavedPair = TPair<string, Boolean>;

  TSavedStorage = class(TDictionary<string, Boolean>);

//------------------------------------------------------------------------------
//! класс watermark'ов
//------------------------------------------------------------------------------
  TWatermarksINI = class sealed(TWatermarksTimed)
  private
    //!
    FSaved: TSavedStorage;
    //!
    FIniFile: TIniFile;
    //!
    FSectionName: string;
    //!
    procedure LoadFile();
    //!
    procedure StoreFile();
  protected
    //!
    procedure OnCycleTimer(
      Sender: TObject
    ); override;
  public
    //!
    constructor Create(
      const AFileName: string;
      const ASectionName: string;
      const ASaveInterval: Integer
    );
    //!
    destructor Destroy(); override;
    //!
    procedure SetWatermark(
      const AKey: string;
      const ATimestamp: TDateTime
    ); override;
    //!
    procedure DeleteWatermark(
      const AKey: string
    ); override;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TWatermarksINI
//------------------------------------------------------------------------------

constructor TWatermarksINI.Create(
  const AFileName: string;
  const ASectionName: string;
  const ASaveInterval: Integer
);
begin
  inherited Create(ASaveInterval);
  //
  FSectionName := ASectionName;
  FIniFile := TIniFile.Create(AFileName);
  FSaved := TSavedStorage.Create();
  LoadFile();
end;

destructor TWatermarksINI.Destroy();
begin
  StoreFile();
  FIniFile.Free();
  FSaved.Free();
  //
  inherited Destroy();
end;

procedure TWatermarksINI.SetWatermark(
  const AKey: string;
  const ATimestamp: TDateTime
);
begin
  FLocker.Acquire();
  try
    inherited;
    FSaved.AddOrSetValue(AKey, False);
  finally
    FLocker.Release();
  end;
end;

procedure TWatermarksINI.DeleteWatermark(
  const AKey: string
);
begin
  FLocker.Acquire();
  try
    inherited;
    FSaved.Remove(AKey);
    FIniFile.DeleteKey(FSectionName, AKey);
  finally
    FLocker.Release();
  end;
end;

procedure TWatermarksINI.OnCycleTimer(
  Sender: TObject
);
begin
  StoreFile();
end;

procedure TWatermarksINI.LoadFile();
var
  //!
  LoadSL: TStringList;
  //!
  TempStr: string;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  LoadSL := TStringList.Create();
  try
    FIniFile.ReadSectionValues(FSectionName, LoadSL);
    for I := 0 to LoadSL.Count - 1 do
    begin
      TempStr := LoadSL.Names[I];
      FWM.Add(TempStr, IlsToDateTime(LoadSL.Values[TempStr]));
    end;
  finally
    LoadSL.Free();
  end;
end;

procedure TWatermarksINI.StoreFile();
var
  //!
  KeyIter: string;
//------------------------------------------------------------------------------
begin
  FLocker.Acquire();
  try
    for KeyIter in FWM.Keys do
    begin
      if FSaved.ContainsKey(KeyIter) then
      begin
        FIniFile.WriteString(FSectionName, KeyIter, DateTimeToIls(FWM[KeyIter]));
        FSaved.Remove(KeyIter);
      end;
    end;
  finally
    FLocker.Release();
  end;
end;

end.

