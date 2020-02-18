unit Ils.Files.Pnt;

interface

uses
  System.Classes, IniFiles, SysUtils, Math, JsonDataObjects,
  Ils.Json.Names, Ils.Json.Utils, Generics.Collections, Windows,
  Ils.Utils;

const
  CFilePosIniFileName = '';
  CRenamedFileName = '';
  CPntFileName = '';

type

  TJsonMessageType = (mtUnknown, mtGeo, mtJpeg);

  TPntPoint = packed record
    DeviceID: Longword;
    SatelitesCount: Byte;
    ProviderID: Smallint;
    DeviceType: Byte;
    DateTime: TDateTime;
    Latitude: Single;
    Longitude: Single;
    Speed: Smallint;            //км/ч
    Azimuth: Byte;              //градусы
    Length: Integer;            //м
    FuelInEngine: Smallint;     //топливо, поступившее в бак (не используется)
    Fuel: Smallint;             //топливо в баке (1/512 от объема бака) (не используется)
    SingleLevelSensors: Byte;
    MultiLevelSensors: array[1..20] of Integer;
    QueryID: Byte;              // не используется
    m_iDriverID: array[0..5] of Byte; // не используется
  private
//    function GetJsonMessageType(aJson: TJsonObject): TJsonMessageType;
//    function GetJsonJpegString(aJson: TJsonObject): string;
//    function AsJsonStr: string;
  public
    constructor Create(AJson: TJsonObject; AJsonMappingStr: string; AImeiHash: TDictionary<string,integer>);
  end;

  TPntPointArray = TArray<TPntPoint>;

  TPntFile = class
  const
    CFilePosIniFileName = 'R000000.ini';
    CPntFileName = 'R000000.pnt';
    CRenamedFileName = 'R000000.pntint';
    CMultiSensorsCount = 20;
    CSizeOfPntRecord = SizeOf(TPntPoint);
  private
    FPntFilePath: string;
    FPntStream: TFileStream;
    FLastRead: TDateTime;

    FNewPos: Int64;
    FSize: Int64;
    FProgress: Double;
    FStepTime: TDateTime;
    FStepCount: Integer;
    Fops: Double;
    FConfifmed: Boolean;
  public
    function Get(const ACount: Integer; const AAutoReadConfirm: Boolean): TPntPointArray;
    procedure Confirm;

    constructor Create(const APntFilePath: string);
    destructor Destroy; override;
  end;


implementation

{ TPntFile }

constructor TPntFile.Create(const APntFilePath: string);
begin
  FPntStream := nil;
  FPntFilePath := IncludeTrailingPathDelimiter(APntFilePath);
  FLastRead := Now;
  FConfifmed := True;
end;

destructor TPntFile.Destroy;
begin
  FPntStream.Free;
  inherited;
end;

function TPntFile.Get(const ACount: Integer; const AAutoReadConfirm: Boolean): TPntPointArray;
var
  FilePosIniFile: TIniFile;

  function OpenFile: Boolean;
  begin
    if Assigned(FPntStream) then
      Exit(True);

    try
      if FileExists(FPntFilePath + CRenamedFileName) then
      begin
        FPntStream := TFileStream.Create(FPntFilePath + CRenamedFileName, fmOpenRead);
        FPntStream.Seek(StrToInt64Def(FilePosIniFile.ReadString('position', 'pos', '0'), 0), soFromBeginning);
      end
      else if FileExists(FPntFilePath + CPntFileName) and RenameFile(FPntFilePath + CPntFileName, FPntFilePath + CRenamedFileName) then
      begin
        FPntStream := TFileStream.Create(FPntFilePath + CRenamedFileName, fmOpenRead);
        FilePosIniFile.WriteInteger('position', 'pos', 0);
      end;
    finally
    end;

    Result := Assigned(FPntStream);
  end;
var
  ReadSize: Integer;
begin
  Result := nil;
  FStepCount := ACount;
  ReadSize := 0;
  FilePosIniFile := TIniFile.Create(FPntFilePath + CFilePosIniFileName);

  try
    if not OpenFile() then
      Exit;

    SetLength(Result, ACount);
    ReadSize := FPntStream.Read(Result[0], CSizeOfPntRecord * ACount);
    if ReadSize < CSizeOfPntRecord * ACount then
    begin
      FreeAndNil(FPntStream);
      DeleteFile(PWideChar(FPntFilePath + CRenamedFileName));
      FilePosIniFile.WriteInteger('position', 'pos', 0);
      FNewPos := 0;
      FSize := 0;
    end
    else
    begin
      FNewPos := FPntStream.Position;
      FSize := FPntStream.Size;
      FilePosIniFile.WriteString('position', 'pos', IntToStr(FNewPos));
    end;

    FStepTime := Now - FLastRead;
    FProgress := IfThen(FSize = 0, 0, FNewPos / FSize * 100);

    FConfifmed := False;

    if AAutoReadConfirm then
      Confirm;

  finally
    SetLength(Result, ReadSize div CSizeOfPntRecord);
    FilePosIniFile.Free;
  end;
  FLastRead := Now;
//  if ReadSize mod CSizeOfPntRecord = 0 then
//    for i := 0 to ReadSize div CSizeOfPntRecord - 1 do
//      if PntPointToTrackPoint(PntPoints[i], TrackPoint) then
//        FPoints.Add(TrackPoint);
//
//  PntPoints := nil;
//
//  Result := FPoints.Count > 0;
end;

procedure TPntFile.Confirm;
var
  FilePosIniFile: TIniFile;

begin
  if FConfifmed then
    Exit;

  FilePosIniFile := TIniFile.Create(FPntFilePath + CFilePosIniFileName);

  try
    FilePosIniFile.WriteString('position', 'steptime', FormatDateTime('hh:nn:ss.zzz', (FStepTime)));
    FilePosIniFile.WriteInteger('position', 'stepsize', FStepCount);
    FilePosIniFile.WriteInteger('position', 'points', FNewPos div 123);

    FilePosIniFile.WriteString('position', 'progress', Format('%.2f', [FProgress]) + '%');

    FilePosIniFile.WriteString('position', 'ops', Format('%.2f', [Fops]));
    FilePosIniFile.WriteDateTime('position', 'lastupdated', Now);
    FConfifmed := True;
  finally
    FilePosIniFile.Free
  end;

end;

{ TPntPoint }

{
function TPntPoint.AsJsonStr: string;
var
  JSONObj: TJsonObject;
  i: Integer;
begin
  JSONObj := TJsonObject.Create();
  try
    JSONObj.I['g'] := 2; // версия
    JSONObj.I['t'] := 99;//APntPoint.DeviceType; // протокол
    JSONObj.S['i'] := IntToStr(DeviceID); // IMEI

    JSONObj.S['dt'] := FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss"."zzz', DateTime); // время
//      JSONObj.B['ew'] := (Var8 and $80) <> 0; // работает ли двигатель
//      JSONObj.B['sv'] := (Var8 and $02) <> 0; // валидны ли координаты
    JSONObj.I['sc'] := SatelitesCount;//Var8 shr 2; // количество спутников
    JSONObj.F['la'] := Latitude;
    JSONObj.F['lo'] := Longitude;
    JSONObj.F['a'] := 0;
    JSONObj.F['v'] := Speed; // дробное (*** округляем, ибо без этого - фигня ***)
    JSONObj.I['d'] := Azimuth * 2; // азимут

    JSONObj.O['s'].I[MakeSensorName(stBin8, 1)] := SingleLevelSensors and $FF;
//      for i := 0 to 7 do
//        AddSensor(JSONObj.A['s'], GetSensorName(snBin, i + 1), CSenType[smBin], ((SingleLevelSensors shr i) and 1));

    for i := 1 to 20 do
      JSONObj.O['s'].I[MakeSensorName(stAin, i)] := MultiLevelSensors[i];

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;
}

const
  CJesonMappingDef =
    '';

constructor TPntPoint.Create(AJson: TJsonObject; AJsonMappingStr: string; AImeiHash: TDictionary<string,integer>);
var
  Mapping: TJsonObject;
  i, j: Integer;
  k: string;
  v: string;
begin
  Mapping := nil;

  DeviceID            := Longword(-1);
  SatelitesCount      := 0;
  ProviderID          := 0;
  DeviceType          := 0;
  DateTime            := 0;
  Latitude            := 0;
  Longitude           := 0;
  Speed               := 0; //км/ч
  Azimuth             := 0; //градусы
  Length              := 0; //м
  FuelInEngine        := 0; //топливо, поступившее в бак (не используется)
  Fuel                := 0; //топливо в баке (1/512 от объема бака) (не используется)
  SingleLevelSensors  := 0;

  for j := 1 to 20 do
    MultiLevelSensors[j] := 0;

  if AJsonMappingStr <> '' then
    try
      Mapping := TJsonObject(TJsonObject.Parse(AJsonMappingStr));
    except

    end;


  try
    if Assigned(AImeiHash) and AImeiHash.ContainsKey(Trim(aJson.S['i'])) then
      DeviceID            := AImeiHash.Items[Trim(aJson.S['i'])]
    else
      DeviceID            := aJson.I['i'];

    SatelitesCount      := aJson.I['sc'];
    ProviderID          := 0;
    DeviceType          := aJson.I['t'];
    DateTime            := IlsToDateTime(aJson.S['dt']);
    Latitude            := IfThen((aJson.F['la'] > 90)  or (aJson.F['la'] < -90), 0, aJson.F['la']);
    Longitude           := IfThen((aJson.F['lo'] > 180) or (aJson.F['lo'] < -180), 0, aJson.F['lo']);;
    Speed               := Round(IfThen((aJson.F['v'] > MaxInt) or (aJson.F['v'] < Pred(-Maxint)), 9999, aJson.F['v']));       //км/ч
    Azimuth             := aJson.I['d'];              //градусы
    Length              := Round(IfThen((aJson.F['l'] > MaxInt) or (aJson.F['l'] < Pred(-Maxint)), 9999, aJson.F['l']))*1000;  //м
    FuelInEngine        := 0;                         //топливо, поступившее в бак (не используется)
    Fuel                := 0;                         //топливо в баке (1/512 от объема бака) (не используется)
    SingleLevelSensors  := 0;

    case GetJsonVersion(AJson) of
      1: begin
        for i := 0 to aJson.A['s'].Count - 1 do
          if aJson.A['s'].O[i].Contains('bin_1:8') then
          begin
            SingleLevelSensors := aJson.A['s'].O[i].I['bin_1:8'];
            Break;
          end;

        for j := 3  to 20 do
          for i := 0 to aJson.A['s'].Count - 1 do
            if aJson.A['s'].O[i].Contains(MakeSensorName(stAin, j - 2)) then
            begin
              MultiLevelSensors[j] := aJson.A['s'].O[i].I[MakeSensorName(stAin, j - 2)];
              Break;
            end;

        MultiLevelSensors[1] := aJson.I['vext'];
        MultiLevelSensors[2] := aJson.I['vint'];
      end;
      2: begin
        MultiLevelSensors[1] := aJson.O['s'].I[MakeSensorName(stVExt)];
        MultiLevelSensors[2] := aJson.O['s'].I[MakeSensorName(stVInt)];

        case AJson.I['t'] of
          dtNovacom:begin
            SingleLevelSensors := aJson.O['s'].I['nova_2:i'] shl 1;
            MultiLevelSensors[1] := aJson.O['s'].I['nova_66:i'];
            MultiLevelSensors[7] := aJson.O['s'].I['nova_72:i'];
          end;
          dtNavtelecom: begin
            SingleLevelSensors := aJson.O['s'].I['bin_1:8'];
            for j := 3  to 20 do
              MultiLevelSensors[j] := aJson.O['s'].I[MakeSensorName(stAin, j - 2)];

            MultiLevelSensors[7]  := aJson.O['s'].I[MakeSensorName(stTemp, 1)];
            MultiLevelSensors[13] := aJson.O['s'].I[MakeSensorName(stCANDistance)];
            MultiLevelSensors[15] := aJson.O['s'].I[MakeSensorName(stCANFuelL)];
            MultiLevelSensors[16] := aJson.O['s'].I[MakeSensorName(stCANRPM)];
            MultiLevelSensors[19] := aJson.O['s'].I[MakeSensorName(stCANFuelP)];
          end;
          else begin
          end;
        end;

        if Assigned(Mapping) then
        begin
          for i := 0 to Mapping.O[AJson.S['t']].Count - 1 do begin
            k := Mapping.O[AJson.S['t']].Names[i];
            v := Mapping.O[AJson.S['t']].Values[k];
            try
              case GetSensorMeasure(v) of
                smFloat: MultiLevelSensors[StrToInt(k)] := Trunc(aJson.O['s'].F[v] * 1000);
                else MultiLevelSensors[StrToInt(k)] := aJson.O['s'].I[v];
              end;
            except

            end;
          end;
        end;
      end;
    end;

    QueryID             := 0;                   // не используется
    m_iDriverID[0]      := 0;                   // не используется
    m_iDriverID[1]      := 0;
    m_iDriverID[2]      := 0;
    m_iDriverID[3]      := 0;
    m_iDriverID[4]      := 0;
    m_iDriverID[5]      := 0;

  finally
    Mapping.Free;
  end;


end;

//function TPntPoint.GetJsonJpegString(aJson: TJsonObject): string;
//begin
//  Result := '';
//  case GetJsonVersion(aJson) of
//    1:
//      with aJson.A['s'] do
//        if Count >= 1 then
//          Result := (aJson.A['s'].O[0].S[MakeSensorName(stCamera)]);
//
//    2:
//      with aJson.O['s'] do
//        Result := (aJson.O['s'].S[MakeSensorName(stCamera)]);
//  end;
//end;

//function TPntPoint.GetJsonMessageType(aJson: TJsonObject): TJsonMessageType;
//begin
//  Result := mtGeo;
//  if GetJsonJpegString(aJson) <> '' then
//    Result := mtJpeg;
//end;


end.
