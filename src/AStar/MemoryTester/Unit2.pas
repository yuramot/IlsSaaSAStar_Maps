unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AStar64.Intf, AStar64.Typ, Ils.Logger,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
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
      const ASpeed: Integer; const AZones: Int64; out ADistance: Double;
      out ADuration: TDateTime; out APath: string;
      out ACalcDuration: TDateTime): Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btn1Click(Sender: TObject);
var
  dist: Double;
  dur: TDateTime;
  path: string;
  cd: TDateTime;
  i: Integer;
  s: TDateTime;
  f: TDateTime;
begin
s := now;
mmo1.Lines.Add('started'+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', s));

for i := 0 to 49 do//49 do

//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(
//    54.622086, 39.693146,
//    56.013226, 37.162746,
//    90,0,dist,dur,path,cd);

//  Route(
  Route2(
//    54.6225662, 39.6926842,
//    56.0128138, 37.1615184,
//    55.579443,37.759206,55.669376,37.625964,
//    55.563719,37.822991,55.533369,37.523335,
//    90, 8, dist, dur, path, cd);
//    55.495870,37.551427, 55.507829,37.533316,
//    90, 8, dist, dur, path, cd);
//55.525913,37.818872,55.649409,37.661484,

55.525913,37.818872,55.649409,37.661484, //москва
//53.386973,49.468956, 53.216607,50.266217, //татарский кондей
//53.387044,49.468946, 53.216607,50.266217, //странный маршрут
//55.760833,37.777260, 55.750881,37.810994, //без тонкого
//53.446922,49.488415,53.255581,50.210890,
//54.651540,39.551495, 55.756148,37.636999, //крешится
//55.697057,37.496233,55.459437,38.129110, //крешится

//55.591691,37.728973,55.586840,37.717128, //двойной проход

//55.750881,37.810994, 55.760833,37.777260, //без тонкого
//53.354908, 50.210370, 53.430388,49.480021,
//53.430388,49.480021, 53.354908, 50.210370,
    90, 0, dist, dur, path, cd);
f := Now;
mmo1.Lines.Add('stopped'+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', f) + ' duration per ' + FormatDateTime('nn:ss.zzz', f - s));

  ToLog(path);

//    55.639880, 37.925565, 55.883476, 37.314302,
//    90, 8, dist, dur, path, cd);

//    55.563315,37.768950,55.616086,37.715216,
//    90,0,dist,dur,path,cd)




//  Route(53.446922,49.488415,53.255581,50.210890, 90, 0, dist, dur, path, cd);



//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(55,36,54,33,90,0,dist,dur,path,cd);
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  dist: Double;
  dur: TDateTime;
  path: string;
  cd: TDateTime;
  i: Integer;
  s: TDateTime;
  f: TDateTime;
begin
  btn1Click(btn1);
  exit;

s := now;
mmo1.Lines.Add('started'+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', s));

for i := 0 to 49 do

//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(
//    54.622086, 39.693146,
//    56.013226, 37.162746,
//    90,0,dist,dur,path,cd);

//  Route(
  Route2(
//    54.6225662, 39.6926842,
//    56.0128138, 37.1615184,
//    55.579443,37.759206,55.669376,37.625964,
//    55.563719,37.822991,55.533369,37.523335,
//    90, 8, dist, dur, path, cd);
//    55.495870,37.551427, 55.507829,37.533316,
//    90, 8, dist, dur, path, cd);
//55.525913,37.818872,55.649409,37.661484,

55.525913,37.818872,55.649409,37.661484, //москва
//53.386973,49.468956, 53.216607,50.266217, //татарский кондей
//53.387044,49.468946, 53.216607,50.266217, //странный маршрут
//55.760833,37.777260, 55.750881,37.810994, //без тонкого
//53.446922,49.488415,53.255581,50.210890,
//54.651540,39.551495, 55.756148,37.636999, //крешится
//55.697057,37.496233,55.459437,38.129110, //крешится

//55.591691,37.728973,55.586840,37.717128, //двойной проход

//55.750881,37.810994, 55.760833,37.777260, //без тонкого
//53.354908, 50.210370, 53.430388,49.480021,
//53.430388,49.480021, 53.354908, 50.210370,
    90, 0, dist, dur, path, cd);
f := Now;
mmo1.Lines.Add('stopped'+ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', f) + ' duration per ' + FormatDateTime('nn:ss.zzz', f - s));

  ToLog(path);

//    55.639880, 37.925565, 55.883476, 37.314302,
//    90, 8, dist, dur, path, cd);

//    55.563315,37.768950,55.616086,37.715216,
//    90,0,dist,dur,path,cd)




//  Route(53.446922,49.488415,53.255581,50.210890, 90, 0, dist, dur, path, cd);



//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(55,36,54,33,90,0,dist,dur,path,cd);
//  Route(55,36,54,33,90,0,dist,dur,path,cd);
end;


{
function TForm2.Route(
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
function TForm2.Route(
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

function TForm2.Route2(
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

        Feature  := 1;
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
