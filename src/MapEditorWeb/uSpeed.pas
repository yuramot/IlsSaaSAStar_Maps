unit uSpeed;

interface

uses AStar64.Files, AStar64.FileStructs, Geo.Hash.Search, Geo.Hash, AStar64.Extra,
AStar64.Areas, Winapi.Windows, Generics.Collections, System.DateUtils, System.SysUtils,
System.Types;

const
  CHashChars: string = '0123456789bcdefghjkmnpqrstuvwxyz';
  PathDelim = '\';

type
  TDoubleArr = array of Double;
  TImeiDt = packed record
    Imei: Int64;
    Dt: TDateTime;
  end;

  TWeekTime = packed record
    TimeOfDay: TTimePeriods;
    DayOfWeek: Byte; // из 7 дней пока используем 2 (будний -1 , выходной-2)
  end;

  TSpeedStat = packed record
    Speed: Byte;
    Count: Integer;
  end;

  TSpeedStatArr = array [TTimePeriods,1..2] of TSpeedStat;

  THashVectorStatRec = packed record
    HashVector: THashVector;
    StatArr: TSpeedStatArr;
  end;

  THashVectorStatDict = TDictionary<THashVector,TSpeedStatArr>;

  TImeiDtDict = TDictionary<TImeiDt, Double>;
  TImeiDtRouteDict = TDictionary<TImeiDt, TIDLines>;
  TSpeedDict = TDictionary<TWeekTime, TDoubleArr>;
  TRouteSpeedDict = TDictionary<TIDLines, TSpeedDict>;
  TRouteSpeedControlDict = TDictionary<TIDLines, TSpeedControl>;
  TRouteHashDict = TDictionary<string, TRouteSpeedControlDict>;
  THoldRecDict = TDictionary<string, THoldRec>;
  TSpeedStatDict = TDictionary<TWeekTime,TSpeedStat>;
  THashVectorStat = TDictionary<THashVector, TSpeedStatDict>;

  TSpeed = class
  private
  public
    function FillRouteSpeedDefault(const ARootPath: string;
                                   const AAccounts: TIntegerDynArray;
                                   const ARouteID: TIDLines): TSpeedControl;
    class function FillAllRouteSpeed(const ARootPath: string;
                               const AAccounts: TIntegerDynArray;
                               const AHash: string): Boolean;
    class function SaveRouteSpeed(const ARootPath: string;
                                  const AAccounts: TIntegerDynArray;
                                  const ARouteID: TIDLines;
                                  const ASpeed: TSpeedControl;
                                  var ALoadTick: Cardinal;
                                  var ASaveTick: Cardinal): Boolean;
    class function SaveRouteSpeedDict(const ARootPath: string;
                                  const AAccounts: TIntegerDynArray;
                                  const ARoutesDict: TRouteHashDict;
                                  var ALoadTick: Cardinal;
                                  var ASaveTick: Cardinal): Boolean;
    class function GetWeekTime(ADt: TDateTime): TWeekTime;
    class function GetAvgSpeed(ASpeedArr: TDoubleArr): TSpeedStat;
    class procedure LoadHoldRecDict(const ARootPath: string;
                                    const AAccounts: TIntegerDynArray;
                                    const ARouteSpeedDict: TRouteSpeedDict;
                                    var AHoldRecDict: THoldRecDict);
    class procedure SaveHoldRecDict(const ARootPath: string;
                                    const AAccounts: TIntegerDynArray;
                                    const AHoldRecDict: THoldRecDict);
    class procedure Join2DictTo1(AImeiDtDict: TImeiDtDict; AImeiDtRouteDict: TImeiDtRouteDict; var ARouteSpeedDict: TRouteSpeedDict);
    class procedure SetNewSpeed(const ARouteSpeedDict: TRouteSpeedDict;
                                       var AHoldRecDict: THoldRecDict);
    class function GetHashVectorStat(const ARouteSpeedDict: TRouteSpeedDict): THashVectorStat;
  end;

implementation

{ TSpeed }

class function TSpeed.FillAllRouteSpeed(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const AHash: string): Boolean;
var
  i: Integer;
  Holder: THoldRec;
  Speed: TSpeedControl;
begin
  TGFAdapter.LoadFileType(ARootPath, AAccounts, AHash, Holder, [ftEdgeF]);
  SetLength(Holder.Speeds, Length(Holder.EdgeForward));
  for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
  begin
    FillChar(Speed, SizeOf(TSpeedControl), Byte(Holder.EdgeForward[i].MaxSpeed));
    Holder.Speeds[i] := Speed;
  end;
  try
    TGFAdapter.Save(ARootPath, AAccounts[0], AHash, Holder, [ftSpeed]);
    Result := True;
  except
    Result := False;
  end;
end;

class procedure TSpeed.LoadHoldRecDict(const ARootPath: string;
                                       const AAccounts: TIntegerDynArray;
                                       const ARouteSpeedDict: TRouteSpeedDict;
                                       var AHoldRecDict: THoldRecDict);
var
  RouteID: TIDLines;
  sHash: string;
  Holder: THoldRec;
begin
  if not Assigned(AHoldRecDict) then
    AHoldRecDict := TDictionary<string,THoldRec>.Create;

  AHoldRecDict.Clear;
  for RouteID in ARouteSpeedDict.Keys do
  begin
    sHash := TGeoHash.ConvertBinToString(RouteID.HashStart and CAreaHashMask, 5);
    if not AHoldRecDict.TryGetValue(sHash, Holder) then
    begin
      TGFAdapter.LoadFileType(ARootPath, AAccounts, sHash, Holder, [ftEdgeF, ftSpeed]);
      AHoldRecDict.AddOrSetValue(sHash, Holder);
    end;
  end;
end;

function TSpeed.FillRouteSpeedDefault(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const ARouteID: TIDLines): TSpeedControl;
var
  i: Integer;
  Holder: THoldRec;
  sHash: string;
begin
  FillChar(Result, SizeOf(TSpeedControl), 0);
  sHash := TGeoHash.ConvertBinToString(ARouteID.HashStart and CAreaHashMask, 5);
  TGFAdapter.LoadFileType(ARootPath, AAccounts, sHash, Holder, [ftEdgeF]);
  for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
  begin
    if Holder.EdgeForward[i].EqualRouteID(ARouteID) then
    begin
      FillChar(Result, SizeOf(TSpeedControl), Byte(Holder.EdgeForward[i].MaxSpeed));
      Exit;
    end;
  end;
end;

class function TSpeed.GetAvgSpeed(ASpeedArr: TDoubleArr): TSpeedStat;
var
  D: Double;
  i: Integer;
begin
  D := 0;
  for I := Low(ASpeedArr) to High(ASpeedArr) do
    D := D + ASpeedArr[i];
  Result.Speed := Byte(Round(D/Length(ASpeedArr)));
  Result.Count := Length(ASpeedArr);
end;

class function TSpeed.GetHashVectorStat(
  const ARouteSpeedDict: TRouteSpeedDict): THashVectorStat;
var
  RouteID: TIDLines;
  SpeedDict: TSpeedDict;
  WeekTime: TWeekTime;
  DoubleArr: TDoubleArr;
  SpeedStat, SpeedStatTemp: TSpeedStat;
  HashVector: THashVector;
  SpeedStatDict: TSpeedStatDict;
begin
  Result := TDictionary<THashVector, TSpeedStatDict>.Create;
  for RouteID in ARouteSpeedDict.Keys do
  begin
    HashVector.HashFrom := RouteID.HashStart;
    HashVector.HashTo := RouteID.HashEnd;
    if not Result.TryGetValue(HashVector, SpeedStatDict) then
      SpeedStatDict := TDictionary<TWeekTime,TSpeedStat>.Create;

    SpeedDict := ARouteSpeedDict.Items[RouteID];
    for WeekTime in SpeedDict.Keys do
    begin
      DoubleArr := SpeedDict.Items[WeekTime];
      SpeedStat := GetAvgSpeed(DoubleArr);
      if SpeedStatDict.TryGetValue(WeekTime, SpeedStatTemp) then
      begin
        SpeedStat.Speed := Byte(Round((SpeedStat.Speed*SpeedStat.count +
                                       SpeedStatTemp.Speed*SpeedStatTemp.count)/
                                       (SpeedStat.count+SpeedStatTemp.count)));
        SpeedStat.count := SpeedStat.count + SpeedStatTemp.count;
      end;
      SpeedStatDict.AddOrSetValue(WeekTime, SpeedStat);
    end;
    Result.AddOrSetValue(HashVector, SpeedStatDict);
  end;

{  for sHash in AHoldRecDict.Keys do
  begin
    AHoldRecDict.TryGetValue(sHash, Holder);
    if Length(Holder.EdgeForward)<>Length(Holder.Speeds) then
      SetLength(Holder.Speeds, Length(Holder.EdgeForward));

    for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
    begin
      RouteID.OSM_ID := Holder.EdgeForward[i].ID;
      RouteID.HashStart := Holder.EdgeForward[i].HashVector.HashFrom;
      RouteID.HashEnd := Holder.EdgeForward[i].HashVector.HashTo;
      if ARouteSpeedDict.TryGetValue(RouteID, SpeedDict) then
      begin
        for WeekTime in SpeedDict.Keys do
        begin
          SpeedDict.TryGetValue(WeekTime, DoubleArr);
          Speed := GetAvgSpeed(DoubleArr);
          if Speed.Speed > 0 then
            Holder.Speeds[i].SpeedWeekDays[WeekTime.DayOfWeek][WeekTime.TimeOfDay] := Speed.Speed;
        end;
      end;

    end;
  end;}
end;

class function TSpeed.GetWeekTime(ADt: TDateTime): TWeekTime;
begin
  case HourOf(ADt) of
    0..5: Result.TimeOfDay := tpNight;
    6..11: Result.TimeOfDay := tpMorning;
    12..17: Result.TimeOfDay := tpDay;
    else Result.TimeOfDay := tpEvening;
  end;
//  Result.DayOfWeek := DayOfTheWeek(ADt);
  if DayOfTheWeek(ADt) in [DayMonday..DayFriday] then
    Result.DayOfWeek := 1
  else
    Result.DayOfWeek := 2;
end;

class procedure TSpeed.Join2DictTo1(AImeiDtDict: TImeiDtDict; AImeiDtRouteDict: TImeiDtRouteDict; var ARouteSpeedDict: TRouteSpeedDict);
var
  ImeiDt: TImeiDt;
  WeekTime: TWeekTime;
  Speed: Double;
  SpeedArr: TDoubleArr;
  SpeedDict: TSpeedDict;
  RouteID: TIDLines;
begin
  for ImeiDt in AImeiDtDict.Keys do
  begin
    if AImeiDtRouteDict.TryGetValue(ImeiDt, RouteID) then
    begin
      AImeiDtDict.TryGetValue(ImeiDt, Speed);
      WeekTime := TSpeed.GetWeekTime(ImeiDt.Dt);

      if ARouteSpeedDict.TryGetValue(RouteID, SpeedDict) then
      begin
        if SpeedDict.TryGetValue(WeekTime, SpeedArr) then
          SetLength(SpeedArr, Length(SpeedArr) + 1)
        else
          SetLength(SpeedArr, 1);
        SpeedArr[High(SpeedArr)] := Speed;
        SpeedDict.AddOrSetValue(WeekTime, SpeedArr);
      end else
      begin
        SpeedDict := TDictionary<TWeekTime, TDoubleArr>.Create;
        SetLength(SpeedArr, 1);
        SpeedArr[High(SpeedArr)] := Speed;
        SpeedDict.AddOrSetValue(WeekTime, SpeedArr);
      end;
      ARouteSpeedDict.AddOrSetValue(RouteID, SpeedDict);
    end;
  end;

end;

class procedure TSpeed.SaveHoldRecDict(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const AHoldRecDict: THoldRecDict);
var
  Holder: THoldRec;
  sHash: string;
begin
  for sHash in AHoldRecDict.Keys do
  begin
    if AHoldRecDict.TryGetValue(sHash, Holder) then
      if Length(Holder.Speeds) > 0 then
        TGFAdapter.Save(ARootPath, AAccounts[0], sHash, Holder, [ftSpeed]);
  end;
end;

class function TSpeed.SaveRouteSpeed(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const ARouteID: TIDLines;
  const ASpeed: TSpeedControl;
  var ALoadTick: Cardinal;
  var ASaveTick: Cardinal): Boolean;
var
  i, k : Integer;
  Holder: THoldRec;
  sHash: string;
  Speed: TSpeedControl;
  Iter: TTimePeriods;
  iT: Cardinal;

begin
  Result := False;
  sHash := TGeoHash.ConvertBinToString(ARouteID.HashStart and CAreaHashMask, 5);
  iT := GetTickCount;
  TGFAdapter.LoadFileType(ARootPath, AAccounts, sHash, Holder, [ftEdgeF, ftSpeed]);
  ALoadTick := ALoadTick + GetTickCount - iT;
  iT := GetTickCount;
  for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
  begin
    if Holder.EdgeForward[i].EqualRouteID(ARouteID) then
    begin
      //если нет скорости, то ставим скорость дороги
      for Iter := Low(TTimePeriods) to High(TTimePeriods) do
        for k := DayMonday to DaySunday do
          if ASpeed.SpeedWeekDays[k][Iter] <> 0 then
            Speed.SpeedWeekDays[k][Iter] := ASpeed.SpeedWeekDays[k][Iter]
          else
            Speed.SpeedWeekDays[k][Iter]:= Byte(Holder.EdgeForward[i].MaxSpeed);

      Holder.Speeds[i] := Speed;
      try
        TGFAdapter.Save(ARootPath, AAccounts[0], sHash, Holder, [ftSpeed]);
        Result := True;
      except
        Result := False;
      end;
      ASaveTick := ASaveTick + GetTickCount - iT;
      Exit;
    end;
  end;
  ASaveTick := ASaveTick + GetTickCount - iT;
end;

class function TSpeed.SaveRouteSpeedDict(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const ARoutesDict: TRouteHashDict; var ALoadTick,
  ASaveTick: Cardinal): Boolean;
var
  RouteSpeed: TRouteSpeedControlDict;
  sHash: string;
  i, k: Integer;
  Holder: THoldRec;
  iT: Cardinal;
  RouteID: TIDLines;
  SpeedControl: TSpeedControl;
  Iter: TTimePeriods;
begin
  Result := True;
  for sHash in ARoutesDict.Keys do
  begin
    if not ARoutesDict.TryGetValue(sHash, RouteSpeed) then Continue;

    iT := GetTickCount;
    TGFAdapter.LoadFileType(ARootPath, AAccounts, sHash, Holder, [ftEdgeF, ftSpeed]);
    ALoadTick := ALoadTick + GetTickCount - iT;

    if Length(Holder.Speeds) <> Length(Holder.EdgeForward) then
      SetLength(Holder.Speeds, Length(Holder.EdgeForward));

    for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
    begin
      RouteID.OSM_ID := Holder.EdgeForward[i].ID;
      RouteID.HashStart := Holder.EdgeForward[i].HashVector.HashFrom;
      RouteID.HashEnd := Holder.EdgeForward[i].HashVector.HashTo;

      if RouteSpeed.TryGetValue(RouteID, SpeedControl) then
      begin
        for Iter := Low(TTimePeriods) to High(TTimePeriods) do
          for k := DayMonday to DaySunday do
            if SpeedControl.SpeedWeekDays[k][Iter] <> 0 then
              Holder.Speeds[i].SpeedWeekDays[k][Iter] := SpeedControl.SpeedWeekDays[k][Iter]
            else
              Holder.Speeds[i].SpeedWeekDays[k][Iter]:= Byte(Holder.EdgeForward[i].MaxSpeed);
      end else
      begin
        for Iter := Low(TTimePeriods) to High(TTimePeriods) do
          for k := DayMonday to DaySunday do
            if Holder.Speeds[i].SpeedWeekDays[k][Iter] = 0 then
              Holder.Speeds[i].SpeedWeekDays[k][Iter]:= Byte(Holder.EdgeForward[i].MaxSpeed);
      end;

    end;

    try
      iT := GetTickCount;
      TGFAdapter.Save(ARootPath, AAccounts[0], sHash, Holder, [ftSpeed]);
      ASaveTick := ASaveTick + GetTickCount - iT;
    except on E: Exception do
      begin
        Result := False;
      end;
    end;
  end;

end;

class procedure TSpeed.SetNewSpeed(const ARouteSpeedDict: TRouteSpeedDict;
  var AHoldRecDict: THoldRecDict);
var
  sHash: string;
  Holder: THoldRec;
  RouteID: TIDLines;
  i: Integer;
  SpeedDict: TSpeedDict;
  WeekTime: TWeekTime;
  DoubleArr: TDoubleArr;
//  Speed: Byte;
  Speed: TSpeedStat;
begin
  for sHash in AHoldRecDict.Keys do
  begin
    AHoldRecDict.TryGetValue(sHash, Holder);
    if Length(Holder.EdgeForward)<>Length(Holder.Speeds) then
      SetLength(Holder.Speeds, Length(Holder.EdgeForward));

    for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
    begin
      RouteID.OSM_ID := Holder.EdgeForward[i].ID;
      RouteID.HashStart := Holder.EdgeForward[i].HashVector.HashFrom;
      RouteID.HashEnd := Holder.EdgeForward[i].HashVector.HashTo;
      if ARouteSpeedDict.TryGetValue(RouteID, SpeedDict) then
      begin
        for WeekTime in SpeedDict.Keys do
        begin
          SpeedDict.TryGetValue(WeekTime, DoubleArr);
          Speed := GetAvgSpeed(DoubleArr);
          if Speed.Speed > 0 then
            Holder.Speeds[i].SpeedWeekDays[WeekTime.DayOfWeek][WeekTime.TimeOfDay] := Speed.Speed;
        end;
      end;

    end;
  end;
end;

end.
