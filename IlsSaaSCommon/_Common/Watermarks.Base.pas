unit Watermarks.Base;
//------------------------------------------------------------------------------
// класс watermark'ов
//
// базовый класс не является абстрактным;
// он полностью функционален, но работает только в памяти.
//
// + абстрактный класс с сохранением по таймеру
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, SyncObjs, ExtCtrls,
  System.Generics.Collections;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс-словарь watermark'ов
//------------------------------------------------------------------------------
  TWatermarksDataPair = TPair<string, TDateTime>;

  TWatermarksDataStorage = class(TDictionary<string, TDateTime>);

//------------------------------------------------------------------------------
//! базовый класс watermark'ов
//------------------------------------------------------------------------------
  TWatermarksBase = class
  protected
    //!
    FWM: TWatermarksDataStorage;
    //!
    FLocker: TCriticalSection;
  public
    //!
    constructor Create();
    //!
    destructor Destroy(); override;
    //!
    procedure SetWatermark(
      const AKey: string;
      const ATimestamp: TDateTime
    ); virtual;
    //!
    function GetWatermark(
      const AKey: string;
      out ATimestamp: TDateTime
    ): Boolean; virtual;
    //!
    procedure DeleteWatermark(
      const AKey: string
    ); virtual;
  end;

//------------------------------------------------------------------------------
//! базовый класс watermark'ов
//------------------------------------------------------------------------------
  TWatermarksTimed = class abstract(TWatermarksBase)
  private
    //!
    FCycleTimer: TTimer;
  protected
    //!
    procedure OnCycleTimer(
      Sender: TObject
    ); virtual; abstract;
  public
    //!
    constructor Create(
      const ASaveInterval: Integer
    );
    //!
    destructor Destroy(); override;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TWatermarksBase
//------------------------------------------------------------------------------

constructor TWatermarksBase.Create();
begin
  inherited Create();
  //
  FWM := TWatermarksDataStorage.Create();
  FLocker := TCriticalSection.Create();
end;

destructor TWatermarksBase.Destroy();
begin
  FLocker.Free();
  FWM.Free();
  //
  inherited Destroy();
end;

procedure TWatermarksBase.SetWatermark(
  const AKey: string;
  const ATimestamp: TDateTime
);
begin
  FLocker.Acquire();
  try
    FWM.AddOrSetValue(AKey, ATimestamp);
  finally
    FLocker.Release();
  end;
end;

function TWatermarksBase.GetWatermark(
  const AKey: string;
  out ATimestamp: TDateTime
): Boolean;
begin
  FLocker.Acquire();
  try
    Result := FWM.ContainsKey(AKey);
    if Result then
      ATimestamp := FWM[AKey];
  finally
    FLocker.Release();
  end;
end;

procedure TWatermarksBase.DeleteWatermark(
  const AKey: string
);
begin
  FLocker.Acquire();
  try
    FWM.Remove(AKey);
  finally
    FLocker.Release();
  end;
end;

//------------------------------------------------------------------------------
// TWatermarksTimed
//------------------------------------------------------------------------------

constructor TWatermarksTimed.Create(
  const ASaveInterval: Integer
);
begin
  inherited Create();
  //
  FCycleTimer := TTimer.Create(nil);
  FCycleTimer.Interval := ASaveInterval * 1000;
  FCycleTimer.OnTimer := OnCycleTimer;
end;

destructor TWatermarksTimed.Destroy();
begin
  FCycleTimer.Free();
  //
  inherited Destroy();
end;

end.

