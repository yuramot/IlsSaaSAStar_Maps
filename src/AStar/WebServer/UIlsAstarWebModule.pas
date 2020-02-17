unit UIlsAstarWebModule;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, Windows,
  Ils.Utils.Debug,
  AStar64.DynImport, AStar64.Typ,
  JsonDataObjects, Geo.Hash, Geo.Pos, Geo.Calcs,
  Geo.Hash.Search, System.Math;
//  AStar64.Intf;

const
  CBuffSize = 1024 * 1024 * 5;
  CBuffSizeDouble = CBuffSize * 2;
  CLenTresholdDef = 20;
  CTimeOutDef = 300;

type
  TwmAstar = class(TWebModule)
    procedure WebModule2DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure wmAstarGetRouteAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure wmAstarGetRoute2Action(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure wmAstarGetSpeedDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure wmAstarGetRouteNearestEdgesAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure wmAstarGetRouteTZAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    function Route(
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
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TwmAstar;

implementation

{$R *.dfm}

procedure TwmAstar.WebModule2DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>WAT?</body></html>';
end;

procedure TwmAstar.wmAstarGetRoute2Action(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  ar: TAstarRequest;

  Distance: Double;
  Duration: TDateTime;
  Pth: string;
  CalcDuration: TDateTime;

  PerfStart, PerfEnd: TLargeInteger;
  PerfCounterPerSec: TLargeInteger;
  Rez: Integer;
  fs: TFormatSettings;
begin
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.Content := '{"e":"rror"}';
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  ar.Version        := 1;
  ar.FromLatitude   := StrToFloatDef(Request.ContentFields.Values['body[FromLatitude]'], 0, fs);
  ar.FromLongitude  := StrToFloatDef(Request.ContentFields.Values['body[FromLongitude]'], 0, fs);
  ar.ToLatitude     := StrToFloatDef(Request.ContentFields.Values['body[ToLatitude]'], 0, fs);
  ar.ToLongitude    := StrToFloatDef(Request.ContentFields.Values['body[ToLongitude]'], 0, fs);
  ar.ZonesLimit     := StrToInt64Def(Request.ContentFields.Values['body[ZonesLimit]'], 0);
  ar.RoadSpeedsRecord.Motorway      := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Motorway]'], 0);
  ar.RoadSpeedsRecord.MotorwayLink  := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][MotorwayLink]'], 0);
  ar.RoadSpeedsRecord.Trunk         := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Trunk]'], 0);
  ar.RoadSpeedsRecord.TrunkLink     := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][TrunkLink]'], 0);
  ar.RoadSpeedsRecord.Primary       := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Primary]'], 0);
  ar.RoadSpeedsRecord.PrimaryLink   := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][PrimaryLink]'], 0);
  ar.RoadSpeedsRecord.Secondary     := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Secondary]'], 0);
  ar.RoadSpeedsRecord.SecondaryLink := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][SecondaryLink]'], 0);
  ar.RoadSpeedsRecord.Tertiary      := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Tertiary]'], 0);
  ar.RoadSpeedsRecord.TertiaryLink  := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][TertiaryLink]'], 0);
  ar.RoadSpeedsRecord.Residential   := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Residential]'], 0);
  ar.RoadSpeedsRecord.Road          := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Road]'], 0);
  ar.RoadSpeedsRecord.Unclassified  := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Unclassified]'], 0);
  ar.RoadSpeedsRecord.Service       := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][Service]'], 0);
  ar.RoadSpeedsRecord.LivingStreet  := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][LivingStreet]'], 0);
  ar.RoadSpeedsRecord.reverse       := StrToIntDef(Request.ContentFields.Values['body[RoadSpeedRecord][reverse]'], 0);
  ar.FormatVariant  := StrToIntDef(Request.ContentFields.Values['body[FormatVariant]'], 0);
  ar.LenTreshold    := StrToIntDef(Request.ContentFields.Values['body[LenThreshold]'], CLenTresholdDef);
  ar.Timeout        := StrToIntDef(Request.ContentFields.Values['body[Timeout]'], CTimeOutDef);
  ar.BufferSize     := CBuffSize;
  ar.HashString     := AllocMem(CBuffSize);
  try
{$ifdef ASTAR_LOAD_DYNAMIC}
    if LoadAStar64('igf') then
{$endif}
      try
        QueryPerformanceFrequency(PerfCounterPerSec);
        QueryPerformanceCounter(PerfStart);


        Rez := CreateRouteWithPath3(@ar);
        QueryPerformanceCounter(PerfEnd);
  //      sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';

        if Rez = 0 then
          Pth := string(AnsiString(ar.HashString));

        Distance := ar.Distance;
        Duration := ar.Duration;
        CalcDuration := ( PerfEnd - PerfStart ) / PerfCounterPerSec;

        with TJsonObject.Create do
        begin
          I['result'] := Rez;
          S['calctime'] := Format('%.6f', [CalcDuration], fs);
          S['distance'] := Format('%.3f', [Distance], fs);
          S['time'] := Format('%.1f', [Duration * 60 * 24], fs);
          S['path'] := Pth;

          Response.Content := ToJSON(False);
          Free;
        end;

      finally
{$ifdef ASTAR_LOAD_DYNAMIC}
        UnloadAStar64;
{$endif}
      end;
  finally
    FreeMem(ar.HashString);
  end;

  Handled := True;
end;

procedure TwmAstar.wmAstarGetRouteAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  FromLa: Double;
  FromLo: Double;
  ToLa: Double;
  ToLo: Double;
  Speed : Integer;
  Zones : Int64;
  Distance: Double;
  Duration: TDateTime;
  Path: string;
  fs: TFormatSettings;
  CalcDuration: TDateTime;
  Res: Integer;
  MemUsed: Int64;
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';

  if (not TryStrToFloat(Request.QueryFields.Values['from.la'], FromLa, fs)) or
     (not TryStrToFloat(Request.QueryFields.Values['from.lo'], FromLo, fs)) or
     (not TryStrToFloat(Request.QueryFields.Values['to.la'], ToLa, fs)) or
     (not TryStrToFloat(Request.QueryFields.Values['to.lo'], ToLo, fs)) or
     (not TryStrToInt(Request.QueryFields.Values['speed'], Speed)) or
     (not TryStrToInt64(Request.QueryFields.Values['zone'], Zones)) then
  begin
    Response.Content := '{"e":"rror"}';
    Exit;
  end;

  Res := Route(FromLa, FromLo, ToLa, ToLo, Speed, Zones, Distance, Duration, Path, CalcDuration);



//      sbMain.Panels[2].Text := Format('Длина: %.3f км // Время: %.1f мин', [Distance, Duration * 60 * 24]);


  MemUsed := MemoryUsed;

  Response.Content :=
    '{' +
    '"result":'+IntToStr(Res)+',' +
    '"calctime":'+ Format('%.6f', [CalcDuration], fs) + ','+
    '"memory":'+ IntToStr(MemUsed) + ','+
    '"distance":'+Format('%.3f', [Distance], fs)+',' +
    '"time":'+Format('%.1f', [Duration * 60 * 24], fs)+',' +
    '"path":"'+path+'"' +
    '}'

//  Response.Content :=
//    '{<br>' +
//    '"from.la"='+FloatToStr(FromLa)+',<br>' +
//    '"from.lo"='+FloatToStr(FromLo)+',<br>' +
//    '"to.la"='+FloatToStr(ToLa)+',<br>' +
//    '"to.lo"='+FloatToStr(ToLo)+',<br>' +
//    '"speed"='+IntToStr(Speed)+',<br>' +
//    '"zone"='+InttoStr(Zones)+'<br>' +
//    '}<br>'
end;



procedure TwmAstar.wmAstarGetRouteNearestEdgesAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  fs: TFormatSettings;
  Latitude: Double;
  Longitude: Double;
  Lines: TArray<TIDLinesAzimDist>;
  Radius: Integer;
  Count: Integer;
  Line: TIDLinesAzimDist;
  jo: TJsonObject;

begin
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.Content := '{"e":"rror"}';
//  GeoHashSearchEngine.WorkingPath := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance))) + '..\GeoData\';
  GeoHashSearchEngine.WorkingPath := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance))) + 'GeoData\';

  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  Latitude   := StrToFloatDef(Request.ContentFields.Values['body[Latitude]'], 0, fs);
  Longitude  := StrToFloatDef(Request.ContentFields.Values['body[Longitude]'], 0, fs);
  Radius  := StrToIntDef(Request.ContentFields.Values['body[Radius]'], 1000);
  Count  := StrToIntDef(Request.ContentFields.Values['body[Count]'], 2);

  GeoHashSearchEngine.FindRouteNearestToPoint(
    Latitude, Longitude,
    Radius, Lines, Count, nil);

  jo := TJsonObject.Create;
  try
      for Line in Lines do
      begin
        with jo.A['lines'].AddObject do begin
          S['HashStart']  :=  TGeoHash.ConvertBinToString(Line.ID.HashStart, 12);
          S['HashEnd']    :=  TGeoHash.ConvertBinToString(Line.ID.HashEnd, 12);
          I['OsmId']      :=  Line.ID.OSM_ID;
          S['GW']         :=  'GW'+TGeoHash.ConvertBinToString(Line.ID.HashStart, 12)+TGeoHash.ConvertBinToString(Line.ID.HashEnd, 12);
        end;
      end;

      Response.Content := jo.ToJSON(False);
  finally
    jo.Free;
  end;
end;

procedure TwmAstar.wmAstarGetRouteTZAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  function GetLatLonTypeZone(var aas: String; var ap1, ap2: TGeoPos; var at: Byte; var az: UInt64): Boolean;
  const
    cpl = 12 + 16 + 2;
  begin
    Result := False;
    if Length(aas) >= (cpl + 12) then
    begin
      TGeoHash.DecodePointString(Copy(aas, 1, 12), ap1.Latitude, ap1.Longitude);
      TGeoHash.DecodePointString(Copy(aas, cpl + 1, 12), ap2.Latitude, ap2.Longitude);
      at := StrToInt('$' + Copy(aas, 13, 2));
      az := StrToInt64('$' + Copy(aas, 15, 16));
      Delete(aas, 1, cpl);
      Result := True;
    end;
  end;
var
  ar: TAstarRequest;

  Distance: Double;
  Duration: TDateTime;
  Pth: string;
  Pth2: string;
  CalcDuration: TDateTime;
  PerfStart, PerfEnd: TLargeInteger;
  PerfCounterPerSec: TLargeInteger;
  Rez: Integer;
  fs: TFormatSettings;
  p1, p2: TGeoPos;
  LastP: TGeoPos;
  t: Byte;
  bit: Byte;
  z: UInt64;
begin
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.Content := '{"e":"rror"}';
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  ar.Version        := 1;
  ar.FromLatitude   := StrToFloatDef(Request.ContentFields.Values['body[FromLatitude]'], 0, fs);
  ar.FromLongitude  := StrToFloatDef(Request.ContentFields.Values['body[FromLongitude]'], 0, fs);
  ar.ToLatitude     := StrToFloatDef(Request.ContentFields.Values['body[ToLatitude]'], 0, fs);
  ar.ToLongitude    := StrToFloatDef(Request.ContentFields.Values['body[ToLongitude]'], 0, fs);
  ar.ZonesLimit     := 0;


  ar.FormatVariant  := 2;
  ar.LenTreshold    := StrToIntDef(Request.ContentFields.Values['body[LenThreshold]'], CLenTresholdDef);
  ar.Timeout        := StrToIntDef(Request.ContentFields.Values['body[Timeout]'], CTimeOutDef);
  ar.BufferSize     := CBuffSizeDouble;
  ar.HashString     := AllocMem(CBuffSizeDouble);
  try
{$ifdef ASTAR_LOAD_DYNAMIC}
    if LoadAStar64('igf') then
{$endif}
      try
        GetRoadSpeedsDefault(@ar.RoadSpeedsRecord);
  	    ar.RoadSpeedsRecord.Tertiary      := 20;
        ar.RoadSpeedsRecord.TertiaryLink  := 20;
    	  ar.RoadSpeedsRecord.Residential   := 10;
//        ar.RoadSpeedsRecord.Road          := 20;
//    	  ar.RoadSpeedsRecord.Unclassified  := 20;
        ar.RoadSpeedsRecord.LivingStreet  := 0;
        ar.RoadSpeedsRecord.Service       := 0;
        ar.RoadSpeedsRecord.reverse       := 40;
        QueryPerformanceFrequency(PerfCounterPerSec);
        QueryPerformanceCounter(PerfStart);


        Rez := CreateRouteWithPath3(@ar);
        QueryPerformanceCounter(PerfEnd);
  //      sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';

        Pth := '';
        if Rez = 0 then
          Pth := string(AnsiString(ar.HashString));

        Distance := ar.Distance;
        Duration := ar.Duration;
        CalcDuration := ( PerfEnd - PerfStart ) / PerfCounterPerSec;

        Pth2 := Pth;
        Delete(Pth2, 1, 2);

        with TJsonObject.Create do
        begin
          I['result'] := Rez;
          S['calctime'] := Format('%.6f', [CalcDuration], fs);
          S['distance'] := Format('%.3f', [Distance], fs);
          S['time'] := Format('%.1f', [Duration * 60 * 24], fs);

          LastP := TGeoPos.Create(-1000, -1000);
          //Pth := 'TZ';
          while GetLatLonTypeZone(Pth2, p1, p2, t, z) do begin
            L['zones-crossed-mask'] := L['zones-crossed-mask'] or z;
            for bit := 0 to 63 do
              if (z and (UInt64(1) shl bit)) > 0 then
                O['zones'].F[IntToStr(bit)] := O['zones'].F[IntToStr(bit)] + TGeoCalcs.GeoLengthKmDeg(p1, p2);

            O['road-types'].F[IntToStr(t)] := O['road-types'].F[IntToStr(t)] + TGeoCalcs.GeoLengthKmDeg(p1, p2);
            //Pth := Pth + p1.ToGeoHash + IntToHex(t, 2);
            LastP := p2;
          end;
//          if LastP.IsValid then
//            Pth := Pth + LastP.ToGeoHash;

          S['path'] := Pth;

          Response.Content := ToJSON(False);
          Free;
        end;
        Delete(Pth2, 1, 12);

      finally
{$ifdef ASTAR_LOAD_DYNAMIC}
        UnloadAStar64;
{$endif}
      end;
  finally
    FreeMem(ar.HashString);
  end;

  Handled := True;
end;

procedure TwmAstar.wmAstarGetSpeedDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  rs: TRoadSpeedsRecord;
begin
  Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Response.Content := '{"e":"rror"}';
{$ifdef ASTAR_LOAD_DYNAMIC}
  if LoadAStar64('igf') then
{$endif}
    try
      with TJsonObject.Create do
      begin
        GetRoadSpeedsDefault(@rs);

        I['Motorway'] := rs.Motorway;
        I['MotorwayLink'] := rs.MotorwayLink;
        I['Trunk'] := rs.Trunk;
        I['TrunkLink'] := rs.TrunkLink;
        I['Primary'] := rs.Primary;
        I['PrimaryLink'] := rs.PrimaryLink;
        I['Secondary'] := rs.Secondary;
        I['SecondaryLink'] := rs.SecondaryLink;
        I['Tertiary'] := rs.Tertiary;
        I['TertiaryLink'] := rs.TertiaryLink;
        I['Residential'] := rs.Residential;
        I['Road'] := rs.Road;
        I['Unclassified'] := rs.Unclassified;
        I['Service'] := rs.Service;
        I['LivingStreet'] := rs.LivingStreet;
        I['reverse'] := rs.reverse;

        Response.Content := ToJSON(False);
        Free;
      end;

    finally
{$ifdef ASTAR_LOAD_DYNAMIC}
      UnloadAStar64;
{$endif}
    end;
end;

function TwmAstar.Route(
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
//  Result := 0;
  ADistance := 0;
  ADuration := 0;
  APath := '';

{$ifdef ASTAR_LOAD_DYNAMIC}
  if LoadAStar64('igf') then
{$endif}
    try
      QueryPerformanceFrequency( KCounterPerSec );
      QueryPerformanceCounter( KStart );

      with ar do
      begin
        Version := 1;
        HashString    := AllocMem(CBuffSize);

        FromLatitude := AFromLa;
        FromLongitude := AFromLo;
        ToLatitude := AToLa;
        ToLongitude := AToLo;

  //      GetRoadSpeedsDefault(@RoadSpeedsRecord);
        GetRoadSpeedsKToDef(@RoadSpeedsRecord, ASpeed, 0);

        ZonesLimit    := AZones;
        FormatVariant := 0;
        LenTreshold   := CLenTresholdDef;
        Timeout       := CTimeOutDef;
        Distance      := 0;
        Duration      := 0;
        BufferSize    := CBuffSize;
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
{$ifdef ASTAR_LOAD_DYNAMIC}
      UnloadAStar64;
{$endif}
    end;
end;

initialization
{$ifndef ASTAR_LOAD_DYNAMIC}
  LoadAStar64('igf');
{$endif}

finalization
{$ifndef ASTAR_LOAD_DYNAMIC}
  UnloadAStar64;
{$endif}

end.
