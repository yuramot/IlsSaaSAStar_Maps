unit Ils.Json.Utils;

interface

uses
  SysUtils, JsonDataObjects, Ils.Json.Names, StrUtils;

function GetJsonVersion(AJson: TJsonObject): Integer;
function IsCameraMessage(AJson: TJsonObject): Boolean;

function IntToBin(Value: integer; Digits: integer): string;
function GetAdditionalStat(const AJSONO: TJsonObject; const AType: string): string;
function GetBin8_1(const AJSONO: TJsonObject): string;
function GetVoltage(const AJSONO: TJsonObject): string;

implementation

function GetJsonVersion(AJson: TJsonObject): Integer;
begin
  Result := AJson.I['g'];
end;

function IsCameraMessage(AJson: TJsonObject): Boolean;
begin
  Result := AJson.Path['s.cam:raw'].Typ = jdtString;
end;


function IntToBin(Value: integer; Digits: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Digits - 1 do
    if Value and (1 shl i) > 0 then
      Result := '1' + Result
    else
      Result := '0' + Result;
end;

function GetAdditionalStat(const AJSONO: TJsonObject; const AType: string): string;
var
  fs: TFormatSettings;
begin
  Result := '';
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  try
    Result :=
      IfThen(LowerCase('Device.Geo') = LowerCase(AType),
                MakeSensorName(stBin8, 1) + '=0b' + IntToBin(AJSONO.O[JF2Str(jfSensors)].I[MakeSensorName(stBin8, 1)], 8)
        + ';' + JF2Str(jfSatellitesCount) + '=' + FormatFloat('00', AJSONO.I[JF2Str(jfSatellitesCount)])
        + ';' + JF2Str(jfSatellitesValid) + '=' + FormatFloat('0', AJSONO.I[JF2Str(jfSatellitesValid)])
        + ';' + JF2Str(jfLatitude)        + '=' + FormatFloat(' 00.000000;-00.000000', AJSONO.F[JF2Str(jfLatitude)], fs)
        + ';' + JF2Str(jfLongitude)       + '=' + FormatFloat(' 000.000000;-000.000000', AJSONO.F[JF2Str(jfLongitude)], fs)
        + ';' + JF2Str(jfVelocity)        + '=' + FormatFloat('000.00', AJSONO.F[JF2Str(jfVelocity)], fs)
        + ';' + JF2Str(jfAzimuth)         + '=' + FormatFloat(' 000;-000', AJSONO.I[JF2Str(jfAzimuth)])
        + ';' + MakeSensorName(stVExt)    + '=' + IntToStr(AJSONO.O[JF2Str(jfSensors)].I[MakeSensorName(stVExt)])
        + ';' + MakeSensorName(stTemp, 1) + '=' + IntToStr(AJSONO.O[JF2Str(jfSensors)].I[MakeSensorName(stTemp, 1)])
  {$ifdef PROTOCOL_NOVACOM}
        + ';' + 'nova_2'    + '=' + IntToStr(AJSONO.O[JF2Str(jfSensors)].I['nova_2:i'])
        + ';' + 'nova_72'    + '=' + IntToStr(AJSONO.O[JF2Str(jfSensors)].I['nova_72:i'])
  {$endif}
      , ''
      )
      ;
  except

  end;
end;

function GetBin8_1(const AJSONO: TJsonObject): string;
begin

  Result := '';
  try
    Result := IntToBin(AJSONO.O[JF2Str(jfSensors)].I[MakeSensorName(stBin8, 1)], 8);
  except

  end;
end;

function GetVoltage(const AJSONO: TJsonObject): string;
begin
  Result := MakeSensorName(stBin8, 1) + '=0x' + IntToBin(0, 8);
  try
    Result := MakeSensorName(stBin8, 1) + '=0x' + IntToBin(AJSONO.O[JF2Str(jfSensors)].I[MakeSensorName(stBin8, 1)], 8);
  except

  end;
end;

end.

