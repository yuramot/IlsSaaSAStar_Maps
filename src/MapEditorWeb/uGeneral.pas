unit uGeneral;

interface

uses
  JsonDataObjects, Ils.Json.Names, Ils.Json.Utils, Geo.Hash.Search, System.SysUtils,
  System.Generics.Collections, System.Classes, Ils.Logger, UFiles, System.IniFiles,
  AStar64.Files, AStar64.Areas, AStar64.Extra, Geo.Pos, Ils.Redis.Conf,
  Redis.Client, Geo.Hash, AStar64.FileStructs, System.Types;

const
 CDateTimeFormat       : string = 'yyyy"."mm"."dd" "hh":"nn":"ss"."zzz';
 C1KmLongitude = 0.01595863;
 C1KmLatitude = 0.008993216;
 CSAVE_ROUTE_NAME_ACCESS_DENIED = 12;
 CSAVE_ROUTE_FILE_ACCESS_DENIED = 13;
 CSAVE_ALL_ZONE_FILES_ERROR = 14;
 CREDIS_ERROR = 15;
 CLOAD_ROUTE_NAME_ACCESS_DENIED = 16;
 COTHER_ERROR = 17;
 CAllErrors = 17;

 //типы эксепшенов, которые пишутся в редис
 CErrorNames : array [1..CAllErrors] of string =
               ('ASTAR_REQUEST_TOO_SHORT_BUFFER', //-1
                'ASTAR_REQUEST_NOT_ASSIGNED',     //-2
                'ASTAR_REQUEST_WRONG_VERSION',    //-3
                'ASTAR_HAS_WRONG_FILE_VERSION',   //-4
                'ASTAR_TIMEOUT',                  //-5
                'ASTAR_NOT_ENOUGH_MEMORY',        //-6
                'ASTAR_FILE_NOT_FOUND',           //-7
                'ASTAR_NO_WAY_FOUND',             //-8
                'ASTAR_NEAR_EDGE_NOT_FOUND',      //-9
                'ASTAR_ERROR_UNKNOWN_ERROR',      //-10
                'ASTAR_ZONE_FILE_WRONG_SIZE',     //-11
                'SAVE_ROUTE_NAME_ACCESS_DENIED',
                'SAVE_ROUTE_FILE_ACCESS_DENIED',
                'SAVE_ALL_ZONE_FILES_ERROR',
                'REDIS_CONNECTION_ERROR',
                'LOAD_ROUTE_NAME_ACCESS_DENIED',
                'OTHER_ERROR');
type
  TChangeBorder = packed record
    LastChange: TDateTime;
    TopLeft: TGeoPos;
    BottomRight: TGeoPos;
  end;

  TAccountChangeBorder = TDictionary<Integer,TChangeBorder>;
  //Архитектура зависимостей для аккаунтов
  TAccountsArch = TDictionary<Integer, TIntegerDynArray>;
  TErrArray = array [1..CAllErrors] of Integer;

  TGeneral = class
  private
  public
    class var Ffs: TFormatSettings;
    class var FPort: Integer;
    class var FRedisPostPeriod: Integer;
    class var FTimerInterval: Cardinal;
    class var FTimerRedisInterval: Cardinal;
    class var FRedisConf: TRedisConf;
    class var FUseAStarDll: boolean;
    class var FRedisConnected: boolean;
    class var FRedisCli: TRedisClient;
    class var FMinRealRouteLengthM: Integer;
    class var FMinDistToLineM: Integer;
    class var FAStarTimeout: Integer;
    class var FDetailLog: Boolean;
    class var FRadius: Integer;
    class var FSuffix: string;
    class var FRedisInstanceName: string;
    class var FRedisInstance: string;
    class var FRedisWatchdogInstanceName: string;
    class var FRedisWatchdogInstance: string;
    class var FLandMarkMaxDist: Double;
    class var FCountCPUForLandMarkCalc: Integer;
    class var FLandMarkCalcTimeout: Integer;

    class var FStop: Boolean;
    class var FAccountsArch: TAccountsArch;
    class var FAccountChangeBorder: TAccountChangeBorder;
    class var FRedisBorderIncKm: Integer;
    //счетчик эксепшенов, которые пишутся в редис
    class var FRedisWatchdogArr, FTempArr: TErrArray;
    class var FBorderValues: array of string;

    class procedure Start(AIniFileName: string = '');
    class procedure Stop;
    class procedure ClearWatchdog(var AArr: TErrArray);
    class function GetJsonForRedis(const AAccounts: TIntegerDynArray;TopLeft, BottomRight: TGeoPos): TJsonObject;
    class function GetJsonForWatchdog(AArray: TErrArray): TJsonObject;
    class function GetForWatchdog(ARedisCli: TRedisClient; var AArray: TErrArray): Boolean;
    class procedure IncWatchdog(ANum: integer);
    class procedure SendJsonForWatchdog(AJson: TJsonObject; ARedisCli: TRedisClient);
    class procedure SendWatchdog;
    class procedure SendAccountBorder(const AAccounts: TIntegerDynArray; ARedisCli: TRedisClient);
    class function IntArrToString(AArr: array of Integer): string;
    class function GetCorrectBorder(ABorder: TGeoPosArray): TGeoPosArray;


  end;

implementation

{ TGeneral }

class procedure TGeneral.ClearWatchdog(var AArr: TErrArray);
var
  i: Integer;
begin
  for I := Low(AArr) to High(AArr) do
    AArr[i] := 0;
end;

class function TGeneral.GetCorrectBorder(ABorder: TGeoPosArray): TGeoPosArray;
begin
  Result := ABorder;
  if Length(ABorder)<>2 then
    Exit;

  if ABorder[0].Latitude < ABorder[1].Latitude then
  begin
    Result[0].Latitude := ABorder[1].Latitude;
    Result[1].Latitude := ABorder[0].Latitude;
  end;
  if ABorder[0].Longitude > ABorder[1].Longitude then
  begin
    Result[0].Longitude := ABorder[1].Longitude;
    Result[1].Longitude := ABorder[0].Longitude;
  end;
end;

class function TGeneral.GetForWatchdog(ARedisCli: TRedisClient; var AArray: TErrArray): Boolean;
var
  sValue, sKey: string;
  Json: TJsonObject;
  i: Integer;
begin
  Result := False;
  sKey := FRedisWatchdogInstance+'_'+FRedisWatchdogInstanceName;
//  ToLog('TGeneral.GetForWatchdog');
  if FRedisConf.Enabled and Assigned(FRedisCli) then
  begin
    try
      FRedisCli.Disconnect;
      FRedisCli.Connect;
      if FRedisConf.Password <> '' then
        FRedisCli.AUTH(FRedisConf.Password);
      if not ARedisCli.EXISTS(sKey) then
      begin
        Exit(True);
      end;
      if ARedisCli.GET(sKey, sValue) then
      begin
//        ToLog('ARedisCli.GET='+sValue+ ' key='+skey);
        FRedisConnected := True;
        Result := True;
        if FDetailLog then
          ToLog('Get from Redis' + ' Key = ' + sKey + ' Value =' +  sValue);
        Json := TJsonObject(TJsonObject.Parse(sValue));
        for I := Low(AArray) to High(AArray) do
          AArray[i] := Json.I[CErrorNames[i]];
//        ToLog('GetForWatchdog. AArray='+IntArrToString(AArray));
      end else
        if FDetailLog then
          ToLog('Not get from Redis' + ' Key = ' + sKey + ' Value =' +  sValue);
    except on E : Exception do
      begin
        TGeneral.IncWatchdog(CREDIS_ERROR);
        FRedisConnected := False;
        ToLog('TGeneral.GetForWatchdog. Redis client NOT connected '+FRedisConf.Host+':'+IntToStr(FRedisConf.Port)+'. '+E.Message);
      end;
    end;
  end;
end;

class function TGeneral.GetJsonForRedis(const AAccounts: TIntegerDynArray;
  TopLeft, BottomRight: TGeoPos): TJsonObject;
var
  PointsArr1: TGeoPathPointArray;
  sBorder: string;
  i: Integer;
begin
  Result := TJsonObject.Create;
  //расширяем прямоугольник
  TopLeft.Latitude := TopLeft.Latitude + C1KmLatitude * FRedisBorderIncKm;
  TopLeft.Longitude := TopLeft.Longitude - C1KmLongitude * FRedisBorderIncKm;
  BottomRight.Latitude := BottomRight.Latitude - C1KmLatitude * FRedisBorderIncKm;
  BottomRight.Longitude := BottomRight.Longitude + C1KmLongitude * FRedisBorderIncKm;
  SetLength(PointsArr1,2);
  PointsArr1[0].p := TopLeft;
  PointsArr1[1].p := BottomRight;
  sBorder := TGeoHash.EncodeArrayAnyFormat(PointsArr1, 12, gfPath);
  Result.S['Operation'] := 'ReCalc';
  for I := High(AAccounts) downto Low(AAccounts) do
    Result.A['Accounts'].Add(AAccounts[i]);
  Result.S['Border'] := sBorder;
end;


class function TGeneral.GetJsonForWatchdog(AArray: TErrArray): TJsonObject;
var
  i: Integer;
begin
  Result := TJsonObject.Create;
  Result.S['LastTime'] := FormatDateTime(CDateTimeFormat, Now());
  for I := Low(AArray) to High(AArray) do
    Result.I[CErrorNames[i]] := AArray[i];
end;

class procedure TGeneral.IncWatchdog(ANum: integer);
begin
  if ANum in [Low(FRedisWatchdogArr)..High(FRedisWatchdogArr)] then
    Inc(FRedisWatchdogArr[ANum]);
end;

class function TGeneral.IntArrToString(AArr: array of Integer): string;
var
  i: Integer;
begin
  Result := '';
  for I := Low(AArr) to High(AArr) do
    Result := Result + IntToStr(AArr[i]) + ',';
  Delete(Result, Length(Result),1);
end;

class procedure TGeneral.SendAccountBorder(const AAccounts: TIntegerDynArray;
  ARedisCli: TRedisClient);
var
  Border: TChangeBorder;
  sValue, sKey: string;
  iRPUSH, i: Integer;
begin
  sKey := FRedisInstance+'_'+FRedisInstanceName;
  if FAccountChangeBorder.TryGetValue(AAccounts[0], Border) then
  begin
    if Border.LastChange <> 0 then
    begin
      sValue := GetJsonForRedis(AAccounts, Border.TopLeft, Border.BottomRight).ToString;
      SetLength(FBorderValues, Length(FBorderValues) + 1);
      FBorderValues[High(FBorderValues)] := sValue;
      ToLog('Length FBorderValues='+IntToStr(Length(FBorderValues))+
            ', LastVal='+FBorderValues[High(FBorderValues)]);
    end;
    if FRedisConf.Enabled and Assigned(FRedisCli) and (Length(FBorderValues) > 0) then
    try
      FRedisCli.Disconnect;
      FRedisCli.Connect;
      if FRedisConf.Password <> '' then
        FRedisCli.AUTH(FRedisConf.Password);
      FRedisConnected := True;
      iRPUSH := ARedisCli.RPUSH(sKey, FBorderValues);
      if iRPUSH > 0 then
      begin
//        if FDetailLog then
        begin
          sValue := '';
          for I := Low(FBorderValues) to High(FBorderValues) do
            sValue := sValue + FBorderValues[i] + ',';
          Delete(sValue, Length(sValue), 1);
          if FDetailLog then
            ToLog('Send to Redis'+' Res='+IntToStr(iRPUSH) + ' Key = ' + sKey+
               ' Value =' +  sValue );
        end;
        SetLength(FBorderValues, 0);
      end else
        if FDetailLog then
          ToLog('Not send to Redis' + ' Res='+IntToStr(iRPUSH)+
               ' Key = ' + sKey);
    except on E: Exception do
      begin
        FRedisConnected := False;
        TGeneral.IncWatchdog(CREDIS_ERROR);
        ToLog('TGeneral.SendAccountBorder. Redis client NOT connected '+FRedisConf.Host+':'+IntToStr(FRedisConf.Port)+'. '+E.Message);
      end;
    end;

{    Border.LastChange := 0;
    Border.TopLeft.Latitude := 0;
    Border.TopLeft.Longitude := 180;
    Border.BottomRight.Latitude := 90;
    Border.BottomRight.Longitude := 0;
    FAccountChangeBorder.AddOrSetValue(AAccounts[0], Border);}
  end;
end;

class procedure TGeneral.SendJsonForWatchdog(AJson: TJsonObject; ARedisCli: TRedisClient);
var
  sValue, sKey: string;
//  sValues: array of string;
begin
  sValue := AJson.ToString;
  sKey := FRedisWatchdogInstance+'_'+FRedisWatchdogInstanceName;
//  SetLength(sValues, 1);
//  sValues[0] := sValue;

  if FRedisConf.Enabled and Assigned(FRedisCli) then
  try
    FRedisCli.Disconnect;
    FRedisCli.Connect;
    FRedisConnected := True;
    if FRedisConf.Password <> '' then
      FRedisCli.AUTH(FRedisConf.Password);
    if ARedisCli.&SET(sKey, sValue) then
    begin
      ClearWatchdog(FRedisWatchdogArr);
//      ToLog('SendJsonForWatchdog. FRedisWatchdogArr = ' +IntArrToString(FRedisWatchdogArr));
//      ToLog('SendJsonForWatchdog. FTempArr = ' +IntArrToString(FTempArr));
      if FDetailLog then
        ToLog('Send to Redis'+
             ' Value =' +  sValue + ' Key = ' + sKey)
    end else
      if FDetailLog then
        ToLog('Not send to Redis' +
             ' Value =' +  sValue + ' Key = ' + sKey);
  except on E: Exception do
    begin
      TGeneral.IncWatchdog(CREDIS_ERROR);
      FRedisConnected := False;
      ToLog('TGeneral.SendJsonForWatchdog. Redis client NOT connected '+FRedisConf.Host+':'+IntToStr(FRedisConf.Port)+'. '+E.Message);
    end;
  end;

end;

class procedure TGeneral.SendWatchdog;
var
  i: Integer;
begin
  //получаем текущее значение
//  ToLog('TGeneral.SendWatchdog');
  ClearWatchdog(FTempArr);
  if GetForWatchdog(FRedisCli, FTempArr) then
  begin
//      ToLog('TGeneral.SendWatchdog FRedisWatchdogArr = ' +IntArrToString(FRedisWatchdogArr));
//      ToLog('TGeneral.SendWatchdog FTempArr = ' +IntArrToString(FTempArr));
    //складываем с имеющимися
    for i := Low(FTempArr) to High(FTempArr) do
      FTempArr[i] := FTempArr[i] + FRedisWatchdogArr[i];
    //отправляем сумму
    SendJsonForWatchdog(GetJsonForWatchdog(FTempArr), FRedisCli);
  end;
end;

class procedure TGeneral.Start(AIniFileName: string);
var
  Ini: TIniFile;
  FileName: string;
begin
  FStop := False;
  FileName := GetModuleName(HInstance);
  Ffs := TFormatSettings.Create;
  Ffs.DecimalSeparator := '.';

  if AIniFileName = '' then
    ini := TIniFile.Create(ChangeFileExt(FileName, '.ini'))
  else
    ini := TIniFile.Create(AIniFileName);

  ToLog('Load ini ' + Ini.FileName);
  try
    ClearWatchdog(FRedisWatchdogArr);
    SetLength(FBorderValues, 0);
    FRedisInstanceName := ini.ReadString('redis', 'instance_name', 'release');
    FRedisInstance := ini.ReadString('redis', 'instance', 'ProjectDistancesTasks');
    FRedisWatchdogInstanceName := ini.ReadString('redis', 'watchdog_instance_name', 'watchdog_release');
    FRedisWatchdogInstance := ini.ReadString('redis', 'watchdog_instance', 'watchdog');
    FTimerInterval := ini.ReadInteger('main', 'timerinterval', 1000);
    FTimerRedisInterval := ini.ReadInteger('main', 'timerredisinterval', 60);
    FRedisPostPeriod := ini.ReadInteger('main', 'redispostperiod', 60);
    FMinRealRouteLengthM := ini.ReadInteger('main', 'minrealroutelengthm', 50);
    FMinDistToLineM := ini.ReadInteger('main', 'mindisttolinem', 5);
    FCountCPUForLandMarkCalc := ini.ReadInteger('main', 'cpuforlandmarkcalc', 1);
    FAStarTimeout := ini.ReadInteger('main', 'astartimeout', 30);
    FDetailLog := ini.ReadBool('main', 'detaillog', false);
    FUseAStarDll := ini.ReadBool('main', 'useastardll', false);
    FRadius := ini.ReadInteger('main', 'radius', 5);
    FPort := ini.ReadInteger('web', 'port', 7653);
    FLandMarkMaxDist := ini.ReadFloat('main', 'landmarkmaxdist', 10);
    FLandMarkCalcTimeout := ini.ReadInteger('main', 'landmarkcalctimeout', 30);

    FAccountChangeBorder := TDictionary<Integer, TChangeBorder>.Create;
    FAccountsArch := TDictionary<Integer, TIntegerDynArray>.Create;

    FRedisConf := TRedisConf.Create(ini);
    FRedisCli := TRedisClient.Create(FRedisConf.Host, FRedisConf.Port, FRedisConf.SSL);
    FRedisConnected := False;
    SetLength(FBorderValues, 0);
    if FRedisConf.Enabled then
    try
      FRedisCli.Connect;
      FRedisConnected := True;
      ToLog('Redis client connected '+FRedisConf.Host+':'+IntToStr(FRedisConf.Port));
      if FRedisConf.Password <> '' then
        FRedisCli.AUTH(FRedisConf.Password);
    except on E: Exception do
      begin
        TGeneral.IncWatchdog(CREDIS_ERROR);
        FRedisConnected := False;
        ToLog('TGeneral.Start. Redis client NOT connected '+FRedisConf.Host+':'+IntToStr(FRedisConf.Port));
      end;
    end;
//    GetForWatchdog(FRedisCli);
  finally
    Ini.Free;
  end;
end;

class procedure TGeneral.Stop;
begin
  FStop := True;
  FAccountChangeBorder.Free;
  FAccountsArch.Free;
  if Assigned(FRedisCli) then
    FRedisCli.Free;
end;

end.

