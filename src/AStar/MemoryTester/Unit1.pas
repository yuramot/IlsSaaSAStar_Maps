unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AStar64.Intf, AStar64.Typ, Ils.Logger,
  Vcl.ExtCtrls, Vcl.StdCtrls, AStar64.Files;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    pnl1: TPanel;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
//    function Route(const AFromLa, AFromLo, AToLa, AToLo: Double;
//      const ASpeed: Integer; const AZones: Int64; out ADistance: Double;
//      out ADuration: TDateTime; out APath: string;
//      out ACalcDuration: TDateTime): Integer;
    function Route2(const AFromLa, AFromLo, AToLa, AToLo: Double;
      const ASpeed: Integer; const AZones: Int64;
      const AFeatures : Int64;
      out ADistance: Double;
      out ADuration: TDateTime; out APath: string;
      out ACalcDuration: TDateTime): Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
  procedure TestCompare(la1, lo1, la2, lo2: Double);
  const
    cmax = 10;
  var
    dist: Double;
    dur: TDateTime;
    path: string;
    cd: TDateTime;
    i: Integer;
    s: TDateTime;
    f: TDateTime;
    d0, d1: Double;
  begin
//    Route2(la1, lo1, la2, lo2, 90, 0, 1, dist, dur, path, cd); Exit;
//    ToLog(path);

//    i := 3;
//    while i < Length(path) do
//    begin
//      ss := Copy(path, i, 12);
//      ToLog(ss);
//      inc(i, 12);
//    end;


//    exit;

    s := now;
//    mmo1.Lines.Add('started '+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', s));
    for i := 0 to cmax - 1 do
      Route2(
        la1, lo1, la2, lo2,
        90, 0, 0, dist, dur, path, cd);
    f := Now;
    d0 := (f - s);
    s := now;
    gt := 0;
    gc := 0;
    for i := 0 to cmax - 1 do
      Route2(
        la1, lo1, la2, lo2,
        90, 0, 1, dist, dur, path, cd);
    f := Now;
    d1 := (f - s);
    mmo1.Lines.Add(
      'stopped '+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', f) +
      ' duration f=0 total('+IntToStr(cmax)+') = ' + FormatDateTime('nn:ss.zzz', d0) + ' one = ' + FormatDateTime('nn:ss.zzz', (d0)/cmax)+
      ' duration f=1 total('+IntToStr(cmax)+') = ' + FormatDateTime('nn:ss.zzz', d1) + ' one = ' + FormatDateTime('nn:ss.zzz', (d1)/cmax)+
      ' d0/d1 = ' + FormatFloat('0.####', d0/d1) +
      ' gt = ' + FormatDateTime('nn:ss.zzz', d1) +
      ' gc = ' + IntToStr(gc)
      );
  end;
begin
//  s := now;
//  mmo1.Lines.Add('started '+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', s));
//
//  for i := 0 to 9 do
//    Route2(
//      55.5,37.0,56.0,38.0,
//      90, 0, 0, dist, dur, path, cd);
//  f := Now;
//  mmo1.Lines.Add('stopped '+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', f) + ' duration per 10 = ' + FormatDateTime('nn:ss.zzz', f - s) + ' one = ' + FormatDateTime('nn:ss.zzz', (f - s)/10) );


//  Application.ProcessMessages;
//  TestCompare(55.50,37.00,56.00,38.00);
//  Application.ProcessMessages;
//  TestCompare(55.55,37.20,55.90,37.90);
//  Application.ProcessMessages;
//  TestCompare(55.60,37.25,55.85,37.48);
//  Application.ProcessMessages;
//  TestCompare(55.63,37.22,55.96,37.79);
//  Application.ProcessMessages;
//  TestCompare(55.74,37.33,55.78,37.88);
//  Application.ProcessMessages;
//  TestCompare(55.691159,37.640377,55.858816,37.470047);
  Application.ProcessMessages;
  TestCompare(55.659953,37.626230, 55.858816,37.470047);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  btn1Click(btn1);
end;


{
function TForm1.Route(
  const AFromLa: Double;
  const AFromLo: Double;
  const AToLa: Double;
  const AToLo: Double;
  const ASpeed : Integer;
  const AZones : Int64;
  out ADistance: Double;
  out ADuration: TDateTime;
  out APath: string;
  out ACalcDuration: TDateTime
): Integer;
var
  ar: TAstarRequest;

  KStart, KEnd: TLargeInteger;
  KCounterPerSec: TLargeInteger;

begin
  Result := 0;
  ADistance := 0;
  ADuration := 0;
  APath := '';

//  if LoadAStar64('igf') then
    try
      QueryPerformanceFrequency( KCounterPerSec );
      QueryPerformanceCounter( KStart );

      with ar do
      begin
        Version := 1;
        HashString    := AllocMem(1024*1024);

        FromLatitude := AFromLa;
        FromLongitude := AFromLo;
        ToLatitude := AToLa;
        ToLongitude := AToLo;

  //      GetRoadSpeedsDefault(@RoadSpeedsRecord);
        GetRoadSpeedsKToDef(@RoadSpeedsRecord, ASpeed, 0);

        ZonesLimit    := AZones;
        FormatVariant := 0;
        LenTreshold   := 15;
        Timeout       := 3000;
        Distance      := 0;
        Duration      := 0;
        BufferLen     := 1024*1024;
      end;

      Result := CreateRouteWithPath3(@ar);
      QueryPerformanceCounter( KEnd );
//      sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';

      if (Result = 0) then
        APath := string(AnsiString(ar.HashString));

      ADistance := ar.Distance;
      ADuration := ar.Duration;
      ACalcDuration := ( KEnd - KStart ) / KCounterPerSec;
    finally
      FreeMem(ar.HashString);
//      UnloadAStar64;
    end;
end;
//}

{
function TForm1.Route(
  const AFromLa: Double;
  const AFromLo: Double;
  const AToLa: Double;
  const AToLo: Double;
  const ASpeed : Integer;
  const AZones : Int64;
  out ADistance: Double;
  out ADuration: TDateTime;
  out APath: string;
  out ACalcDuration: TDateTime
): Integer;
var
//  ar: TAstarRequest;
  ar3: TAstarRequest3;

  KStart, KEnd: TLargeInteger;
  KCounterPerSec: TLargeInteger;

//  a: Pointer;

begin
//  Result := 0;
  ADistance := 0;
  ADuration := 0;
  APath := '';

//  if LoadAStar64('igf') then
    try
      QueryPerformanceFrequency( KCounterPerSec );
      QueryPerformanceCounter( KStart );

      with ar3 do
      begin
        Version := 1;
        HashString    := AllocMem(1024*1024);

        FromLatitude := AFromLa;
        FromLongitude := AFromLo;
        ToLatitude := AToLa;
        ToLongitude := AToLo;

  //      GetRoadSpeedsDefault(@RoadSpeedsRecord);
//        GetRoadSpeedsKToDef(@RoadSpeedsRecord, ASpeed, 0);

        ZonesLimit    := AZones;
        FormatVariant := 0;
        LenTreshold   := 15;
        Timeout       := 3000;
        Distance      := 0;
//        Duration      := 0;
        BufferSize     := 1024*1024;

        RoadLengthBufferSize := 10*SizeOf(TRoadLengthByTypeRecord);
        RoadLengthByZoneByType := AllocMem(RoadLengthBufferSize);

        FeatureLimit  := 0;
      end;

//      a := AstarGetObject;
      Result := AStarCalc(@ar3);


//      CreateRouteWithPath3(@ar);
      QueryPerformanceCounter( KEnd );
//      sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';

      if (Result = 0) then
        APath := string(AnsiString(ar3.HashString));

      ADistance := ar3.Distance;
//      ADuration := ar3.Duration;
      ADuration := 0;
      ACalcDuration := ( KEnd - KStart ) / KCounterPerSec;
    finally
      FreeMem(ar3.HashString);
      FreeMem(ar3.RoadLengthByZoneByType);
//      UnloadAStar64;
    end;
end;
}

function TForm1.Route2(
  const AFromLa: Double;
  const AFromLo: Double;
  const AToLa: Double;
  const AToLo: Double;
  const ASpeed : Integer;
  const AZones : Int64;
  const AFeatures : Int64;
  out ADistance: Double;
  out ADuration: TDateTime;
  out APath: string;
  out ACalcDuration: TDateTime
): Integer;
var
//  ar: TAstarRequest;
  ar4: TAstarRequest4;

  KStart, KEnd: TLargeInteger;
  KCounterPerSec: TLargeInteger;

//  a: Pointer;

begin
//  Result := 0;
  ADistance := 0;
  ADuration := 0;
  APath := '';

//  if LoadAStar64('igf') then
    try
      QueryPerformanceFrequency( KCounterPerSec );
      QueryPerformanceCounter( KStart );

      with ar4 do
      begin
        Version := 1;
        HashString    := AllocMem(1024*1024);

        FromLatitude := AFromLa;
        FromLongitude := AFromLo;
        ToLatitude := AToLa;
        ToLongitude := AToLo;

  //      GetRoadSpeedsDefault(@RoadSpeedsRecord);
//        GetRoadSpeedsKToDef(@RoadSpeedsRecord, ASpeed, 0);

        ZonesLimit    := AZones;
        FormatVariant := 0;
        LenTreshold   := 15;
        Timeout       := 3000;
        Distance      := 0;
//        Duration      := 0;
        BufferSize     := 1024*1024;

        RoadLengthAggregateBufferSize := 10000*SizeOf(TRoadLengthAggregateRecord);
        RoadLengthAggregate := AllocMem(RoadLengthAggregateBufferSize);

        Feature  := AFeatures;
      end;

//      a := AstarGetObject;
      Result := AStarCalc4(@ar4);


//      CreateRouteWithPath3(@ar);
      QueryPerformanceCounter( KEnd );
//      sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';

      if (Result = 0) then
        APath := string(AnsiString(ar4.HashString));

      ADistance := ar4.Distance;
//      ADuration := ar3.Duration;
      ADuration := 0;
      ACalcDuration := ( KEnd - KStart ) / KCounterPerSec;
    finally
      FreeMem(ar4.HashString);
      FreeMem(ar4.RoadLengthAggregate);
//      UnloadAStar64;
    end;
end;


initialization
//  LoadAStar64('igf');

finalization
//  UnloadAStar64;


end.
