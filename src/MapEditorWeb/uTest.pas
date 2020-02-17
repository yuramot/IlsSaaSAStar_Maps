unit uTest;

interface

uses IlsMapEditorWebModule, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, Web.HTTPApp,
  Ils.Logger, JsonDataObjects, AStar64.FileStructs, AStar64.Areas,
  Ils.Json.Names, Ils.Json.Utils, Geo.Hash, Geo.Pos, uZC,
  System.Generics.Collections, Geo.Hash.Search, AStar64.Files, Geo.Calcs,
  Vcl.ExtCtrls, Redis.Client, uSign, System.Math, IdHTTP, IdSSLOpenSSL,
  System.SyncObjs, System.DateUtils, uADStanIntf, uADStanOption, uADStanParam,
  uADStanError, uADDatSManager, uADPhysIntf, uADDAptIntf, uADStanAsync,
  uADDAptManager, uADGUIxIntf, uADStanDef, uADStanPool, uADPhysManager,
  uADGUIxFormsWait, uADPhysMySQL, uADCompGUIx, Data.DB, uADCompClient,
  uADCompDataSet, UMapEditor, AStar64.Extra, uGeneral, System.Types, System.IniFiles,
  AStar64.LandMark, AStar64.Typ, AStar64.Intf;

type
  GetAddres = class(TThread)
  private
     m_bisRuning : Boolean;
     m_iStatusComplete : Integer;
     FStartTime: TDateTime;
     FLog: string;
     FRequest: string;
    { Private declarations }
  protected
    procedure Execute; override;
    function GetHttpreqwestResult(strHttpReqwest, sLog: string): string;

  public
    constructor Create(ARequest, ALog: string);
    property Runing : Boolean read m_bisRuning;
    property StartTime : TDateTime read FStartTime;
  end;

  GetAStar = class(TThread)
  private
     m_bisRuning : Boolean;
     m_iStatusComplete : Integer;
     FStartTime: TDateTime;
     FLog: string;
     FPoints: string;
     FSpeed, FLimit: integer;
     FZone, FSign: Uint64;
     FAccounts: TIntegerDynArray;
     FMapEditor: TMapEditor;
  protected
    procedure Execute; override;
    function GetAStarResult(APoints: string; ASpeed, ALimit: integer;
                            AZone, ASign: Uint64; AAccounts: TIntegerDynArray;
                            ALog: string): string;
  public
    constructor Create(APoints: string; ASpeed, ALimit: integer;
                            AZone, ASign: Uint64; AAccounts: TIntegerDynArray;
                            ALog: string);
    destructor Destroy(); override;
    property Runing : Boolean read m_bisRuning;
    property StartTime : TDateTime read FStartTime;
  end;

  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    edt1: TEdit;
    btn1: TButton;
    mmo1: TMemo;
    edtRectangle: TEdit;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    btn6: TButton;
    btn7: TButton;
    btn8: TButton;
    Button1: TButton;
    Button2: TButton;
    edDelete: TEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    tmr1: TTimer;
    dlgOpen: TOpenDialog;
    q1: TADQuery;
    ADConnection1: TADConnection;
    ADGUIxWaitCursor1: TADGUIxWaitCursor;
    ADPhysMySQLDriverLink1: TADPhysMySQLDriverLink;
    q2: TADQuery;
    chkDll: TCheckBox;
    btn9: TButton;
    btn10: TButton;
    btn11: TButton;
    btn13: TButton;
    btn12: TButton;
    btn14: TButton;
    btnSeparateRoute: TButton;
    btn15: TButton;
    btn16: TButton;
    btn17: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btn9Click(Sender: TObject);
    procedure btn10Click(Sender: TObject);
    procedure btn11Click(Sender: TObject);
    procedure btn13Click(Sender: TObject);
    procedure btn12Click(Sender: TObject);
    procedure btn14Click(Sender: TObject);
    procedure btnSeparateRouteClick(Sender: TObject);
    procedure btn15Click(Sender: TObject);
    procedure btn16Click(Sender: TObject);
    procedure btn17Click(Sender: TObject);
  private
    FServer: TIdHTTPWebBrokerBridge;
    fs: TFormatSettings;
    FLocalFolder : string;
    procedure StartServer;
    function GetHttpreqwestResult(strHttpReqwest: string): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  lstThreadList : TList<GetAddres>;//список потоков
  lstThreadAStarList : TList<GetAStar>;//список потоков

implementation

{$R *.dfm}

uses
  Winapi.ShellApi;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TForm1.btn10Click(Sender: TObject);
var
  i: Integer;
  AWay: TGeoPosArray;
  AWays: array of TGeoPosArray;
  s: string;
  SList: TStringList;
begin
  if dlgOpen.Execute then
  begin
    mmo1.Clear;
    SList := TStringList.Create;
    SList.LoadFromFile(dlgOpen.FileName);
    for I := 0 to SList.Count - 1 do
    begin
      s := SList[i];
      if TGeoHash.DecodeArrayWorld(s, AWay) then
      begin
        SetLength(AWays, Length(AWays) + 1);
        AWays[High(AWays)] := AWay;
      end;
      s := AWay[Low(AWay)].ToString + ' - ' + AWay[High(AWay)].ToString;
      mmo1.Lines.Add(s);
    end;
  end;
end;

procedure TForm1.btn11Click(Sender: TObject);
var
  P: TGeoPos;
  sHash, sHash5: string;
begin
  TGeoHash.DecodePointBin(StrToInt64(edt1.Text), P.Latitude, P.Longitude);
  sHash := TGeoHash.EncodePointString(P.Latitude, P.Longitude, 12);
  sHash5 := TGeoHash.ConvertBinToString(StrToInt64(edt1.Text) and CAreaHashMask, 5);
  mmo1.Lines.Add(edt1.Text + ' = ' + P.ToString+', ' +sHash+', '+sHash5);
end;

procedure TForm1.btn12Click(Sender: TObject);
var
  SList: TStringList;
  Points, Rectangle, MinMax: TGeoPosArray;
  i, k, iPos, ID: Integer;
  s, AreasName: string;
  JSon: TJsonObject;
begin
  dlgOpen.Filter := 'Point files|*.lmd';
  if dlgOpen.Execute then
  begin
    try
      JSon := TJsonObject.Create;
      SList := TStringList.Create;
      ID := 1;
      for K := 0 to dlgOpen.Files.Count - 1 do
      begin
        SetLength(Points, 0);
        SList.LoadFromFile(dlgOpen.Files[k]);
        if SList.Count < 2 then
          Exit;
        for I := 0 to SList.Count - 1 do
        begin
          s := SList[i];
          iPos := Pos(#9, s);
          if iPos > 0 then
          begin
            SetLength(Points, Length(Points) + 1);
            Points[High(Points)].Latitude := StrToFloat(Copy(s,1,iPos-1), fs);
            Points[High(Points)].Longitude := StrToFloat(Copy(s,iPos + 1,Length(s) - iPos), fs);
          end;
        end;
        SetLength(MinMax, 2);
        MinMax[0].Create(-90,180);
        MinMax[1].Create(90,-180);
        for I := Low(Points) to High(Points) do
        begin
          if Points[i].Latitude > MinMax[0].Latitude then
            MinMax[0].Latitude := Points[i].Latitude;
          if Points[i].Latitude < MinMax[1].Latitude then
            MinMax[1].Latitude := Points[i].Latitude;
          if Points[i].Longitude > MinMax[1].Longitude then
            MinMax[1].Longitude := Points[i].Longitude;
          if Points[i].Longitude < MinMax[0].Longitude then
            MinMax[0].Longitude := Points[i].Longitude;
        end;
        SetLength(Rectangle, 4);
        Rectangle[0] := MinMax[0];
        Rectangle[1].Create(MinMax[0].Latitude,MinMax[1].Longitude);
        Rectangle[2] := MinMax[1];
        Rectangle[3].Create(MinMax[1].Latitude,MinMax[0].Longitude);
        with Json.A['Areas'].AddObject do
        begin
          I['ID'] := ID;
          S['Name'] := ChangeFileExt(ExtractFileName(dlgOpen.Files[k]),'');
          S['GeoHashID'] := TGeoHash.EncodeArrayWorld(MinMax, gfPointsArray);
          I['State'] := 0;
          S['Polygon'] := TGeoHash.EncodeArrayWorld(Rectangle);
          S['LandMarks'] := TGeoHash.EncodeArrayWorld(Points, gfPointsArray);
        end;
        inc(ID);
      end;
      SList.Text := JSon.ToString;
      AreasName := ExtractFilePath(dlgOpen.Files[0]) + 'Areas.txt';
      SList.SaveToFile(AreasName);
      mmo1.Text := JSon.ToString;
      mmo1.Lines.Add('Сохранено в ' + AreasName);
    finally
      SList.Free;
    end;
  end;
end;

procedure TForm1.btn13Click(Sender: TObject);
var
  Points: TGeoPosArray;
  Json: TJsonObject;
  SList: TStringList;
  i: Integer;
begin
  if dlgOpen.Execute then
  begin
    SList := TStringList.Create;
    Json := TJsonObject.Create;
    SList.LoadFromFile(dlgOpen.FileName);
    Json := TJsonObject(Json.Parse(SList.Text));
    TGeoHash.DecodeArrayWorld(Json.S['Path'], Points);
    SList.Clear;
    for I := Low(Points) to High(Points) do
      SList.Add(Points[i].ToString);

    SList.SaveToFile(ChangeFileExt(dlgOpen.FileName, '.points'));
    SList.Free;
    Json.Free;
  end;

end;

procedure TForm1.btn14Click(Sender: TObject);
var
  LandMarkArr: array of TLandMarkIdxLenFileRecord;
  IdxFile: TFileStream;
  i: Integer;
  HashVector: THashVector;
  HashVectorDict: TDictionary<THashVector,Integer>;
begin
  dlgOpen.Filter := 'Idx files|*.idx';
  if dlgOpen.Execute then
  begin
    HashVectorDict := TDictionary<THashVector,Integer>.Create;
    IdxFile := TFileStream.Create(dlgOpen.FileName, fmOpenRead and fmShareDenyNone);
    try
      SetLength(LandMarkArr, IdxFile.Size div SizeOf(TLandMarkIdxLenFileRecord));
      IdxFile.ReadBuffer(LandMarkArr[0], IdxFile.Size);
      mmo1.Clear;
      mmo1.Lines.Add('k.v.HashFrom, k.v.HashTo, k.z, i, l, Reserved');
//      mmo1.Lines.Add('k.v.HashFrom, k.v.HashTo, k.z ');
      for i := Low(LandMarkArr) to High(LandMarkArr) do
      begin
{        mmo1.Lines.Add(IntToStr(LandMarkArr[i].k.v.HashFrom)+'-'+
                       IntToStr(LandMarkArr[i].k.v.HashTo)+' '+
                       IntToStr(LandMarkArr[i].k.z));   }
        mmo1.Lines.Add(IntToStr(LandMarkArr[i].k.v.HashFrom)+'-'+
                       IntToStr(LandMarkArr[i].k.v.HashTo)+' '+
                       IntToStr(LandMarkArr[i].k.z)+' '+
                       IntToStr(LandMarkArr[i].i)+' '+
                       FloatToStr(LandMarkArr[i].l)+' '+
                       IntToStr(LandMarkArr[i].RESERVED));
        HashVectorDict.AddOrSetValue(LandMarkArr[i].k.v, 0);
      end;
      mmo1.Lines.Add('----------------');
      for HashVector in HashVectorDict.Keys do
      begin
        mmo1.Lines.Add(IntToStr(HashVector.HashFrom)+'-'+
                       IntToStr(HashVector.HashTo));
      end;
    finally
      IdxFile.Free;
      HashVectorDict.Free;
    end;
  end;

end;

procedure TForm1.btn15Click(Sender: TObject);
var
  APoints: TGeoPosArray;
  PointsArr: TGeoPathPointArray;
  i: Integer;
  s: string;
begin
  s := '';
  if edt1.Text <> '' then
    s := edt1.Text
  else
  if mmo1.Lines.Text <> '' then
    s := mmo1.Lines.Text;

  if s = '' then Exit;


  TGeoHash.DecodeArrayWorld(s, APoints);
  SetLength(PointsArr, Length(APoints));
  for I := Low(PointsArr) to High(PointsArr) do
    PointsArr[i].p := APoints[i];
  s := TGeoHash.EncodeArrayAnyFormat(PointsArr, 12, gfPath);
  mmo1.Clear;
  mmo1.Lines.Add('var Path = '''+s+''';');
  mmo1.Lines.Add('var latlngs = Geohash.decodePoints(Path);');
  mmo1.Lines.Add('for (var i = 0; i < latlngs.length; i++) {if (latlngs[i][0] <= 1.1 && latlngs[i][1] <= 1.1) {latlngs.splice(i, 1);}}');
  mmo1.Lines.Add('var layer = L.polyline(latlngs, {weight: 6,color: '''+'orange'+''',dashArray: '''+''',fillOpacity: 1,snapIgnore : true});');
  mmo1.Lines.Add('layer.addTo($.ils.Map._map);');
  mmo1.Lines.Add('layer.bringToBack();');

end;

procedure TForm1.btn16Click(Sender: TObject);
var
  FIdxFN, FDatFN: string;
  fsIdx, fsDat: TFileStream;
  ir: TLandMarkIdxLenFileRecord;
  srDat: TStreamReader;
  HashStr: string;
  APoints: TGeoPosArray;
  Dist1, Dist2: Double;
begin
  dlgOpen.Filter := 'Idx files|*.idx';
  if dlgOpen.Execute then
  begin
    FIdxFN := dlgOpen.FileName;
    FDatFN := ChangeFileExt(FIdxFN,'.dat');
  end else
    Exit;

  fsIdx := TFileStream.Create(FIdxFN, fmOpenRead + fmShareDenyNone);
  fsDat := TFileStream.Create(FDatFN, fmOpenRead + fmShareDenyNone);

  try
    fsIdx.Seek(0, soFromBeginning);
    srDat := TStreamReader.Create(fsDat, TEncoding.ANSI, True);
    while (fsIdx.Read(ir, SizeOf(ir)) = SizeOf(ir)) do
    begin
      if (ir.i < fsDat.Size) then
      begin
        srDat.DiscardBufferedData;
        fsDat.Position := ir.i;
        HashStr := Trim(srDat.ReadLine);
        TGeoHash.DecodeArrayWorld(HashStr, APoints);

        Dist1 := TGeoCalcs.GeoLengthDeg(ir.k.v.PointFrom.Latitude, ir.k.v.PointFrom.Longitude,
                                        APoints[0].Latitude, APoints[0].Longitude);
        Dist2 := TGeoCalcs.GeoLengthDeg(ir.k.v.PointTo.Latitude, ir.k.v.PointTo.Longitude,
                                        APoints[High(APoints)].Latitude, APoints[High(APoints)].Longitude);

          mmo1.Lines.Add(IntToStr(ir.k.v.HashFrom)+'-'+
                         IntToStr(ir.k.v.HashTo)+' '+
                         IntToStr(ir.k.z)+' '+
                         'Dist1=' +IntToStr(Round(Dist1))+' '+
                         'Dist2=' +IntToStr(Round(Dist2)));
{        if (Dist1>100) or (Dist2>100) then
          mmo1.Lines.Add(IntToStr(ir.k.v.HashFrom)+'-'+
                         IntToStr(ir.k.v.HashTo)+' '+
                         IntToStr(ir.k.z)+' '+
                         'Dist1=' +IntToStr(Round(Dist1,0))+' '+
                         'Dist2=' +IntToStr(Round(Dist2,0))+' - Внимание!')
          else
          mmo1.Lines.Add(IntToStr(ir.k.v.HashFrom)+'-'+
                         IntToStr(ir.k.v.HashTo)+' '+
                         IntToStr(ir.k.z)+' '+
                         'Dist1=' +FloatToStr(Dist1)+' '+
                         'Dist2=' +FloatToStr(Dist2)+' - Ok');}

      end;
    end;

  finally
    fsIdx.Free;
    fsDat.Free;
    srDat.Free;
  end;

end;

procedure TForm1.btn17Click(Sender: TObject);
const
  CArrowMinDegree = 0.10471975511966; // 6 градусов
var
  Rez: Integer;
  KStart, KEnd: TLargeInteger;
  KCounterPerSec: TLargeInteger;
  RezStr: string;
  TempStr, Step: string;
  RCoords: TGeoPosArray;
//  ar: TAstarRequest3;
  ar: TAstarRequest3;

  HashVector: THashVector;
  Z: UInt64;
  s: string;
  i, ATimeout: Integer;
  RNWHash, RSEHash: Int64;
  AAccounts: TIntegerDynArray;

begin
  s := edt1.Text;
  if (s = '') or (Pos('-',s)=0) or (Pos(' ',s)=0)  then Exit;
  i := Pos('-',s);
  HashVector.HashFrom := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);
  i := Pos(' ',s);
  HashVector.HashTo := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);
  Z := StrToInt64(s);
  SetLength(AAccounts, 2);
  AAccounts[0] := 83;
  AAccounts[1] := 0;
  ATimeout := 60;

  SetLength(RCoords, 2);
  RCoords[0] := HashVector.PointFrom;
  RCoords[1] := HashVector.PointTo;

  try
  Step := ' Step 1';
  QueryPerformanceFrequency( KCounterPerSec );
  QueryPerformanceCounter( KStart );

  Step := ' Step 2';
  with ar do
  begin
    if TGeneral.FDetailLog then
      ToLog(s);
    Version := 1;
    FromLatitude := RCoords[0].Latitude;
    FromLongitude := RCoords[0].Longitude;
    ToLatitude := RCoords[1].Latitude;
    ToLongitude := RCoords[1].Longitude;
    ZonesLimit    := Z;
    SignsLimit    := 0;
    FormatVariant := 1;
    LenTreshold   := 15;
    Timeout       := ATimeout;//GetTimeout(s, 30000);
    Distance      := 0;
    BufferSize    := 1024*1024;
    HashString    := AllocMem(1024*1024);
  end;
    Step := ' Step 3';

    try
      Rez := AStarCalcLandmarksAcc(@ar,   RNWHash, RSEHash, @AAccounts[0]);
      ToLog(' Timeout '+IntToStr(ATimeout));
      Step := ' Step 4';

      QueryPerformanceCounter( KEnd );
      if (Rez = 0) then
      begin
        RezStr := string(ar.HashString);//RezChar
        mmo1.Lines.Text := RezStr;
        if Length(RezStr) > 24 then
        begin
          if TGeneral.FDetailLog then
            ToLog(' Path = '+RezStr)
          else
            ToLog(' Path length = '+IntToStr(Length(RezStr)));
        end else
          ToLog(' Path length < 24');
        SetLength(RCoords, 0);
        case ar.FormatVariant of
          0: i := 3;
          else i := 3;
        end;

        repeat
          TempStr := Copy(RezStr, i, 12);
          if (TempStr = '') then Break;
          SetLength(RCoords, Length(RCoords) + 1);
          TGeoHash.DecodePointString(
            TempStr,
            RCoords[High(RCoords)].Latitude,
            RCoords[High(RCoords)].Longitude
          );

          case ar.FormatVariant of
            0: Inc(i, 12);
            else //if I = 3 then
                //Inc(I, 12)
              //else
                Inc(i, 15);
          end;

        until False;
        {d := 0;
        for i := 0 to ar.RoadLengthCount - 1 do
          d := d + CRoadSpeedRecordDef.GetDuration(TRoadLengthByTypeRecordArray(ar.RoadLengthByZoneByType)[i]) * 60;
         }
      end
      else
      begin
        TGeneral.IncWatchdog(Abs(Rez));
        mmo1.Lines.Text := '{"Error":"'+CErrorNames[Abs(Rez)]+'"}';
        ToLog(' Error : ' + CErrorNames[Abs(Rez)]);
        RezStr := string(ar.HashString);
        if TGeneral.FDetailLog then
          ToLog(Step +' GetAStarAcc. Rez='+IntTostr(rez));
      end;
    except on E : Exception do
      ToLog(Step +' AStar Exception: '+E.Message);
    end;

  finally
    FreeMem(ar.HashString);
//    FreeMem(ar.RoadLengthByZoneByType);
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  APoints: TGeoPosArray;
  i: Integer;
  s: string;
  P: TGeoPos;
begin
  mmo1.Clear;
  if edt1.Text <> '' then
  begin
    s := edt1.Text;
    i := Pos('-',s);
    if i > 0 then
    begin
      SetLength(APoints,2);
      TGeoHash.DecodePointBin(StrToInt64(Copy(s,1,i-1)), P.Latitude, P.Longitude);
      APoints[0] := P;
      Delete(s,1,i);
      TGeoHash.DecodePointBin(StrToInt64(s), P.Latitude, P.Longitude);
      APoints[1] := P;
    end else
      TGeoHash.DecodeArrayWorld(edt1.Text, APoints);
    for i := Low(APoints) to High(APoints) do
      mmo1.Lines.Add('$.ils.Map._map.setView(['+FloatToStr(APoints[i].Latitude,fs)+
                     ', '+FloatToStr(APoints[i].Longitude,fs)+'], 18)');
//      mmo1.Lines.Add(FloatToStr(APoints[i].Latitude) + ' ' + FloatToStr(APoints[i].Longitude));
    if Length(APoints) = 2 then
      mmo1.Lines.Add('http://release.intelogis.ru/Med.htm?FromLatitude='+FloatToStr(APoints[0].Latitude, fs) +
                     '&FromLongitude='+FloatToStr(APoints[0].Longitude, fs) +
                     '&ToLatitude='+FloatToStr(APoints[1].Latitude, fs) +
                     '&ToLongitude='+FloatToStr(APoints[1].Longitude, fs) +
                     '&ZonesLimit=0&layout=RouteTest&debug=3&lang=&app=');

  end;

end;

procedure TForm1.btn2Click(Sender: TObject);
var
  s: string;
  TopLeftLa, TopLeftLo, BottomRightLa, BottomRightLo : double;
  iRoadType : Integer;
  iZone : UInt64;
  i: Integer;
  iTick : Cardinal;
begin
  iTick := GetTickCount;
  iRoadType := -1;
  iZone := High(UInt64);
  s := Trim(edtRectangle.Text);
  i := Pos(' ',s);
  TopLeftLa := StrToFloat(Copy(s,1,i-1), fs);
  Delete(s,1,i);
  i := Pos(' ',s);
  TopLeftLo := StrToFloat(Copy(s,1,i-1), fs);
  Delete(s,1,i);
  i := Pos(' ',s);
  BottomRightLa := StrToFloat(Copy(s,1,i-1), fs);
  Delete(s,1,i);
  i := Pos(' ',s);
  if i > 0 then
  begin
    BottomRightLo := StrToFloat(Copy(s,1,i-1), fs);
    Delete(s,1,i);
    i := Pos(' ',s);
    if i > 0 then
    begin
      iRoadType := StrToInt(Copy(s,1,i-1));
      Delete(s,1,i);
      iZone := StrToInt64(s);
    end else
      iRoadType := StrToInt(s);
  end else
    BottomRightLo := StrToFloat(s, fs);

  mmo1.Lines.Text := 'P1-P2 ' +FloatToStr(TGeoCalcs.GeoLengthKmDeg(TopLeftLa, TopLeftLo, BottomRightLa, BottomRightLo)) + ' km';
  mmo1.Lines.Add('P1-^ ' +FloatToStr(TGeoCalcs.GeoLengthKmDeg(TopLeftLa, TopLeftLo, TopLeftLa + C1KmLatitude, TopLeftLo)) + ' km');
  mmo1.Lines.Add('P1-< ' +FloatToStr(TGeoCalcs.GeoLengthKmDeg(TopLeftLa, TopLeftLo, TopLeftLa, TopLeftLo - C1KmLongitude)) + ' km');
  mmo1.Lines.Add('P2-> ' +FloatToStr(TGeoCalcs.GeoLengthKmDeg(BottomRightLa, BottomRightLo, BottomRightLa, BottomRightLo + C1KmLongitude)) + ' km');
  mmo1.Lines.Add('P2-| ' +FloatToStr(TGeoCalcs.GeoLengthKmDeg(BottomRightLa, BottomRightLo, BottomRightLa - C1KmLatitude, BottomRightLo)) + ' km');
  mmo1.Lines.Add('P1+1-P2+1 ' + FloatToStr(TGeoCalcs.GeoLengthKmDeg(TopLeftLa + C1KmLatitude, TopLeftLo - C1KmLongitude, BottomRightLa - C1KmLatitude, BottomRightLo + C1KmLongitude))  + ' km');
//  mmo1.Lines.Add(TMapEditor.GetJsonForRedis('GWucgjc64e2j21ucfksfctb02h').ToString);
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  CsvFS: TFileStream;
  CsvReader: TStreamReader;
  FAddressDataFile: TFileStream;
  AddressWriter, IdxWriter: TStreamWriter;
  FIdxFile: TFileStream;
  sl: TStringList;
  AAddrIdx: TAddrIdx;
  s, sName, FCsvFileName, FAddressDataFileName, FIdxFileName: string;
begin
  FCsvFileName := FLocalFolder + '\osm.csv';
  FIdxFileName := FLocalFolder + '\osm.idx';
  FAddressDataFileName := FLocalFolder + '\osm.dat';

  FIdxFile := TFileStream.Create(FIdxFileName, fmCreate + fmOpenReadWrite);
  FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmCreate + fmOpenReadWrite);
{  if FileExists(FIdxFileName) then
  begin
  end;
  if FileExists(FAddressDataFileName) then
  begin
  end; }

//  InitDataFile(FIdxFileName, FIdxFile);
//  InitDataFile(FAddressDataFileName, FAddressDataFile);
  AddressWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.ANSI);
  IdxWriter := TStreamWriter.Create(FIdxFile, TEncoding.ANSI);
  CsvFS := nil;
  CsvReader := nil;
  try
    if FileExists(FCsvFileName) then
    begin
      CsvFS := TFileStream.Create(FCsvFileName, fmOpenRead + fmShareDenyNone);
      CsvReader := TStreamReader.Create(CsvFS, TEncoding.ANSI, True);
    end;

    sName := '';
    if Assigned(CsvFS) and (CsvFS.Size > 0) then
    begin
      sl := TStringList.Create;
      try
        while CsvFS.Position < CsvFS.Size do
        begin
          s := CsvReader.ReadLine;
          sl.Delimiter := #9;
          sl.StrictDelimiter := True;
          sl.DelimitedText := s;
          if sl.Count > 1 then
          begin
            if sName <> sl.Strings[1] then
            begin
              AAddrIdx.Idx := FAddressDataFile.Position;
              AddressWriter.Write(sl.Strings[1] + #13#10);
              sName := sl.Strings[1];
            end;
            AAddrIdx.OsmID := StrToInt64(sl.Strings[0]);
//            IdxWriter.Write(AAddrIdx);
            FIdxFile.Write(AAddrIdx, SizeOf(TAddrIdx));
          end;
        end;
      finally
        sl.Free;
      end;
    end;

  finally
    FreeAndNil(FIdxFile);
    FreeAndNil(FAddressDataFile);

    AddressWriter.Free;
    IdxWriter.Free;
    CsvReader.Free;
    CsvFS.Free
  end;
  ShowMessage('Finish');

end;

procedure TForm1.btn4Click(Sender: TObject);
var
  MapEditor: TMapEditor;
  osm_id: Int64;
  sHash: TWayArray;
  Accounts: TIntegerDynArray;
  s: string;
  i: Integer;
begin
  MapEditor := TMapEditor.Create(5,1,nil);
  try
    SetLength(Accounts, 1);
    Accounts[0] := 1;
    s := Trim(edt1.Text);
    i := Pos(' ', s);
    if i = 0 then Exit;
    osm_id := StrToInt(Copy(s,1,i-1));
    Delete(s,1,i);
    i := Pos(',', s);
    while i > 0 do
    begin
      SetLength(sHash, Length(sHash) + 1);
      sHash[High(sHash)] := Copy(s,1,i - 1);
      Delete(s,1,i);
      i := Pos(',', s);
    end;
    SetLength(sHash, Length(sHash) + 1);
    sHash[High(sHash)] := s;

    mmo1.Lines.Text := IntToStr(osm_id) + ' - ' +MapEditor.GetRouteNameByOSMHash(osm_id, sHash, Accounts);
  finally
    MapEditor.Free;
  end;
end;

procedure TForm1.btn6Click(Sender: TObject);
var
  Accounts: TIntegerDynArray;
  SignList: TStringList;
  RootPath, s, TempStr: string;
  TabPos: Integer;
  curID, IncludeID: Byte;
  I, j: Integer;
  SignInfo: TSignInfo;
  SignTypeInfo: TSignTypeInfo;
  RouteSignArr: TRouteSignArray;
  SignDictionary: TSignDictionary;
  RouteID: TIDLines;
  FSign: TSign;
begin
  FSign := TSign.Create(1);
  SignList := TStringList.Create();
  RootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
  SetLength(Accounts, 1);
  Accounts[0] := 1;
  SignDictionary := FSign.LoadSignFile(RootPath, Accounts);
  if dlgOpen.Execute then
  begin
    FSign.LoadSignTypeFile(RootPath);
    for j := 0 to dlgOpen.Files.Count - 1 do
    begin
      SignList.LoadFromFile(dlgOpen.Files[j]);
      s := ExtractFileName(dlgOpen.Files[j]);
      Delete(s,1,5);
      Delete(s,Length(s)-3,4);
      curID := StrToInt(s);
      if FSign.FSignType.TryGetValue(curID, SignTypeInfo) then
      for I := 0 to SignList.Count - 1 do
      begin
        TempStr := SignList[I];
        // ID
        TabPos := Pos(#9, TempStr);
        RouteID.OSM_ID := StrToInt64(Copy(TempStr, 1, TabPos - 1));
        Delete(TempStr, 1, TabPos);
        // HashStart
        TabPos := Pos(#9, TempStr);
        RouteID.HashStart := StrToInt64(Copy(TempStr, 1, TabPos - 1));
        Delete(TempStr, 1, TabPos);
        // hashend
        RouteID.HashEnd := StrToInt64(TempStr);

        SignDictionary.TryGetValue(RouteID, SignInfo);
        SignInfo.Account := 1;
        FSign.SetBit(SignInfo.Sign, curID);
        SignDictionary.AddOrSetValue(RouteID, SignInfo);

        SignDictionary.TryGetValue(RouteID.ReverseID, SignInfo);
        SignInfo.Account := 1;
        FSign.SetBit(SignInfo.Sign, curID);
        SignDictionary.AddOrSetValue(RouteID.ReverseID, SignInfo);

        if SignTypeInfo.Include <> '' then
        begin
          s := SignTypeInfo.Include;
          TabPos := Pos(',', s);
          while TabPos > 0 do
          begin
            IncludeID := StrToInt(Copy(s, 1, TabPos - 1));
            Delete(s, 1, TabPos);
            TabPos := Pos(',', s);

            SignDictionary.TryGetValue(RouteID, SignInfo);
            SignInfo.Account := 1;
            FSign.SetBit(SignInfo.Sign, IncludeID);
            SignDictionary.AddOrSetValue(RouteID, SignInfo);

            SignDictionary.TryGetValue(RouteID.ReverseID, SignInfo);
            SignInfo.Account := 1;
            FSign.SetBit(SignInfo.Sign, IncludeID);
            SignDictionary.AddOrSetValue(RouteID.ReverseID, SignInfo);
          end;
          IncludeID := StrToInt(s);
          SignDictionary.TryGetValue(RouteID, SignInfo);
          SignInfo.Account := 1;
          FSign.SetBit(SignInfo.Sign, IncludeID);
          SignDictionary.AddOrSetValue(RouteID, SignInfo);

          SignDictionary.TryGetValue(RouteID.ReverseID, SignInfo);
          SignInfo.Account := 1;
          FSign.SetBit(SignInfo.Sign, IncludeID);
          SignDictionary.AddOrSetValue(RouteID.ReverseID, SignInfo);
        end;
      end;
    end;
  end;
  SetLength(RouteSignArr, 0);
  for RouteID in SignDictionary.Keys do
  begin
    if SignDictionary.TryGetValue(RouteID, SignInfo) then
    begin
      SetLength(RouteSignArr, Length(RouteSignArr) + 1);
      RouteSignArr[High(RouteSignArr)].RouteID := RouteID;
      RouteSignArr[High(RouteSignArr)].Sign := SignInfo.Sign;
    end;
  end;

  if FSign.SaveSignFile(RootPath, 1, RouteSignArr) then
    mmo1.Lines.Add('RouteSign file saved');
  SignList.Free();
  FSign.Free;
end;

procedure TForm1.btn7Click(Sender: TObject);
var
  Accounts: TIntegerDynArray;
  SignList: TStringList;
  RootPath: string;
  FSign: TSign;
begin
  SetLength(Accounts, 1);
  Accounts[0] := 1;
  FSign := TSign.Create;
  SignList := TStringList.Create();
  try
    RootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
    if FSign.CreateSignAll(RootPath, Accounts) then
      mmo1.Lines.Add('Sign files saved');
  finally
    SignList.Free;
  end;
end;

procedure TForm1.btn9Click(Sender: TObject);
var
  PointsArr: TGeoPathPointArray;
  sWay, s, sLa, sLo: string;
  i, iPos, iP : Integer;
  P: TGeoPos;
begin
  SetLength(PointsArr, 0);
  for i := 0 to mmo1.Lines.Count - 1 do
  begin
    s := mmo1.Lines[i];
    iPos := Pos(' ', s);
    if iPos > 0 then
    begin
      sLa := Copy(s,1,iPos-1);
      if not (sLa[Length(sLa)] in ['0'..'9']) then
        Delete(sLa,Length(sLa),1);
      iP := Pos('.', sLa);
      if iP>0 then
        sLa[iP]:=',';
      if TryStrToFloat(sLa, P.Latitude) then
      begin
        Delete(s,1,iPos);
        sLo := s;
        iP := Pos('.', sLo);
        if iP>0 then
          sLo[iP]:=',';
        if TryStrToFloat(sLo, P.Longitude) then
        begin
          SetLength(PointsArr, Length(PointsArr) + 1);
          PointsArr[High(PointsArr)].p := P;
        end;
      end;
    end;
  end;
  sWay := '';
  if Length(PointsArr) > 0 then
    sWay := TGeoHash.EncodeArrayAnyFormat(PointsArr, 12, gfPath);
  edt1.Text := sWay;
end;

procedure TForm1.btnSeparateRouteClick(Sender: TObject);
var
  MapEditor: TMapEditor;
  ID: TIDLines;
  Accounts: TIntegerDynArray;
  RootPath: string;
begin
  ID := ID.StrToIDLines(edt1.Text);
  SetLength(Accounts, 1);
  Accounts[0] := 83;
  RootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));

  MapEditor := TMapEditor.Create(5, 1, nil);
  try
    if MapEditor.SeparateRoute(RootPath, Accounts, ID) and
       MapEditor.SeparateRoute(RootPath, Accounts, ID.ReverseID) then
      if MapEditor.SaveAllFiles(Accounts, C5FileTypes) then
         mmo1.Lines.Add('Дорога в обе стороны успешно разделена '+edt1.Text);
  finally
    MapEditor.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  FMapEditor: TMapEditor;
  Accounts: TIntegerDynArray;
begin
  SetLength(Accounts, 2);
  Accounts[0] := 1;
  Accounts[1] := 2;
  FMapEditor := TMapEditor.Create(10,1,nil);
  try
    ToLog('Start AStar');
    mmo1.Lines.Text := FMapEditor.GetAStarAcc(edt1.Text,90, 0, 3, 30, 0, 0, Accounts);
    ToLog('Finish AStar');
  finally
    FMapEditor.Free;
    SetLength(Accounts, 0);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  MapEditor: TMapEditor;
begin
  TGeneral.Start;
  MapEditor := TMapEditor.Create(5, 1, nil);
  try
    mmo1.Lines.Text := MapEditor.ParseJson(edt1.Text);
  finally
    MapEditor.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  strDbAddres: string;
  i : Integer;
  ctNewThread : GetAddres;
  ict : Integer;//счётчик для цикла потоков
  RequestArr: array of string;
  Accounts: TIntegerDynArray;
begin
  SetLength(Accounts, 2);
  Accounts[0] := 1;
  Accounts[1] := 4;
  SetLength(RequestArr, 3);
  RequestArr[0] := mmo1.Lines.text;
  RequestArr[1] := 'http://localhost:7653/?{"Type":"GetAstarPath","Path":"GWucfjtpe3yvnbucfkkvxvhgyr","Zones":"0","Account":{"Current":"2","Parents":["1"]}}';
  RequestArr[2] := 'http://localhost:7653/?{"Type":"UpdateRoads","Features":[{"UpdateType":0,"Border":"GWuccvwy2788qmucfs7qf9x23t","RoadType":[11,12,13,14,15,16],"join":1}],"Account":{"Current":"2","Parents":["1"]}}';

  try
    try
      lstThreadList := TList<GetAddres>.Create;
      i := 0;
      repeat
        Application.ProcessMessages;
        if (i < Length(RequestArr)) and (3 >= lstThreadList.Count ) then
        begin
          strDbAddres := RequestArr[i];

          ctNewThread := GetAddres.Create(strDbAddres, 'Log'+IntToStr(i));
          ctNewThread.Start;
          lstThreadList.Add(ctNewThread);

          //strReqwest := ToURLString(strReqwest);

          inc(i);
//          Inc(icountrequest);
        end;
        Sleep(50);

        //проверяем завершённые потоки и удаляем их
        for ict := 0 to lstThreadList.Count - 1 do
        begin
          if not lstThreadList[ict].Runing then
          begin
            lstThreadList[ict].Terminate;
            lstThreadList[ict].Free;
            lstThreadList.Delete(ict);

            Break;
          end  else
          begin
            if SecondsBetween(Now,lstThreadList[ict].StartTime) > 40 then
            begin
              ToLog('Поток завершён принудительно: ' + lstThreadList[ict].FLog);

              lstThreadList[ict].Terminate;
              lstThreadList[ict].Free;
              lstThreadList.Delete(ict);

              Break;
            end;
          end;
        end;

        if (lstThreadList.Count = 0) then
           Break;
      until (i>=Length(RequestArr)) and (lstThreadList.Count = 0) ;
    except
      on E: Exception do
      begin
        ToLog( 'Ошибка при запуске потоков (' + E.Message + ')');
      end;
    end;
  finally
    lstThreadList.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  strDbAddres: string;
  i  : Integer;
  ctNewThread : GetAddres;
  ict : Integer;//счётчик для цикла потоков
  RequestArr: array of string;
begin
  SetLength(RequestArr, 3);
  RequestArr[0] := 'http://localhost:7653/?{"Type":"GetAstarPath","Path":"GWucfs8u208h2pucfkmtgp8p04","Zones":"0","Account":{"Current":"1","Parents":[]}}';
  RequestArr[1] := 'http://localhost:7653/?{"Type":"GetAstarPath","Path":"GWucfkmtgp8p04ucfs8u208h2p","Zones":"0","Account":{"Current":"1","Parents":[]}}';
  RequestArr[2] := 'http://localhost:7653/?{"Type":"GetAstarPath","Path":"GWubgmtnqen8vgubgmv8n8rj54","Zones":"0","Account":{"Current":"1","Parents":[]}}';

  try
    try
      lstThreadList := TList<GetAddres>.Create;
      i := 0;
      repeat
        Application.ProcessMessages;
        if (i < Length(RequestArr)) and (2 >= lstThreadList.Count ) then
        begin
          strDbAddres := RequestArr[i];

          ctNewThread := GetAddres.Create(strDbAddres,'Log'+IntToStr(i));
          ctNewThread.Start;
          lstThreadList.Add(ctNewThread);
          inc(i);
        end;
        Sleep(50);

        //проверяем завершённые потоки и удаляем их
        for ict := 0 to lstThreadList.Count - 1 do
        begin
          if not lstThreadList[ict].Runing then
          begin
            lstThreadList[ict].Terminate;
            lstThreadList[ict].Free;
            lstThreadList.Delete(ict);

            Break;
          end  else
          begin
            if SecondsBetween(Now,lstThreadList[ict].StartTime) > 40 then
            begin
              ToLog('Поток завершён принудительно: ' + lstThreadList[ict].FLog);

              lstThreadList[ict].Terminate;
              lstThreadList[ict].Free;
              lstThreadList.Delete(ict);

              Break;
            end;
          end;
        end;

        if (lstThreadList.Count = 0) then
           Break;
      until (i>=Length(RequestArr)) and (lstThreadList.Count = 0) ;
    except
      on E: Exception do
      begin
        ToLog( 'Ошибка при запуске потоков (' + E.Message + ')');
      end;
    end;
  finally
    lstThreadList.Free;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  FRedisCli: TRedisClient;
//  Json: TJsonObject;
begin
  TGeneral.Start;
  FRedisCli := TRedisClient.Create(TGeneral.FRedisConf.Host, TGeneral.FRedisConf.Port, TGeneral.FRedisConf.SSL);
  try
  TGeneral.FRedisConnected := False;
  try
    FRedisCli.Connect;
    TGeneral.FRedisConnected := True;
    ToLog('Redis client connected '+TGeneral.FRedisConf.Host+':'+IntToStr(TGeneral.FRedisConf.Port));
    if TGeneral.FRedisConf.Password <> '' then
      FRedisCli.AUTH(TGeneral.FRedisConf.Password);
  except
    TGeneral.FRedisConnected := False;
    ToLog('Redis client NOT connected '+TGeneral.FRedisConf.Host+':'+IntToStr(TGeneral.FRedisConf.Port));
  end;
{  TGeneral.GetForWatchdog(FRedisCli);
  TGeneral.IncWatchdog(0);
  TGeneral.IncWatchdog(3);
  Json := TGeneral.GetJsonForWatchdog;
  mmo1.Lines.Text := Json.ToString;
  TGeneral.SendJsonForWatchdog(Json, FRedisCli);}
  TGeneral.Stop;
  finally
    FRedisCli.Free;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  strDbAddres: string;
  i : Integer;
  ctNewThread : GetAStar;
  ict : Integer;//счётчик для цикла потоков
  RequestArr: array of string;
  Accounts: TIntegerDynArray;
begin
  SetLength(Accounts, 2);
  Accounts[0] := 1;
  Accounts[1] := 4;
  SetLength(RequestArr, 3);
//  RequestArr[0] := 'GWubfnw0h9xtwtubgmsz3ynfyr';
  RequestArr[0] := mmo1.Lines.Text;// 'GWucfs8u208h2pucfkmtgp8p04';
  RequestArr[1] := mmo1.Lines.Text;//'GWucfkmtgp8p04ucfs8u208h2p';
  RequestArr[2] := mmo1.Lines.Text;//'GWubgmtnqen8vgubgmv8n8rj54';

  try
    try
      lstThreadAStarList := TList<GetAStar>.Create;
      i := 0;
      repeat
        Application.ProcessMessages;
        if (i < Length(RequestArr)) and (3 >= lstThreadAStarList.Count ) then
        begin
          strDbAddres := RequestArr[i];

          ctNewThread := GetAStar.Create(strDbAddres,90,0,0,0,Accounts,'Log'+IntToStr(i));
          ctNewThread.Start;
          lstThreadAStarList.Add(ctNewThread);
          inc(i);
        end;
        Sleep(50);

        //проверяем завершённые потоки и удаляем их
        for ict := 0 to lstThreadAStarList.Count - 1 do
        begin
          if not lstThreadAStarList[ict].Runing then
          begin
            lstThreadAStarList[ict].Terminate;
            lstThreadAStarList[ict].Free;
            lstThreadAStarList.Delete(ict);

            Break;
          end  else
          begin
            if SecondsBetween(Now,lstThreadAStarList[ict].StartTime) > 40 then
            begin
              ToLog('Поток завершён принудительно: ' + lstThreadAStarList[ict].FLog);

              lstThreadAStarList[ict].Terminate;
              lstThreadAStarList[ict].Free;
              lstThreadAStarList.Delete(ict);

              Break;
            end;
          end;
        end;

        if (lstThreadAStarList.Count = 0) then
           Break;
      until (i>=Length(RequestArr)) and (lstThreadAStarList.Count = 0) ;
    except
      on E: Exception do
      begin
        ToLog( 'Ошибка при запуске потоков (' + E.Message + ')');
      end;
    end;
  finally
    lstThreadAStarList.Free;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  s, sKey: string;
  i: Integer;
  List: TList<string>;
begin
  s := edt1.Text;
  List := TList<string>.Create;
  if s[1] = 'S' then
    Delete(s,1,2)
  else
    Exit;
  if ((Length(s)+3) mod 15) <> 0 then
  begin
    mmo1.Text := 'Length = '+IntToStr(Length(s));
  end;
  for I := 1 to ((Length(s)+3) div 15) do
  begin
    sKey := Copy(s,1,5);
    Delete(s,1,15);
    if not List.Contains(sKey) then
      List.Add(sKey);
  end;
  mmo1.Clear;
  for sKey in List do
    mmo1.Lines.Add(sKey);
  List.Free;
end;

procedure TForm1.ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
{  Application.ProcessMessages;
  TGeneral.Stop;
  FServer.Active := False;
  FServer.Bindings.Clear;
  ToLog('WebServer Stoped');}
  TGeneral.Stop;
  Sleep(1000);
  FServer.Active := False;
  ToLog('TILSMapEditorWebService.StopServer. FServer.Active');
  FServer.Bindings.Clear;
  ToLog('TILSMapEditorWebService.StopServer. FServer.Bindings.Clear');
  FServer.Free;
  ToLog('=============WebServer Stop=============');

end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  TGeneral.Start();
  EditPort.Text := IntToStr(TGeneral.FPort);
{  ini := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), '.ini'));
  try
    EditPort.Text := IntToStr(ini.ReadInteger('web', 'port', 7653));
  finally
    Ini.Free;
  end;}
  FLocalFolder := ExtractFileDir(GetModuleName(HInstance));
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FServer.Active then
    ButtonStopClick(Self);
end;

function TForm1.GetHttpreqwestResult(strHttpReqwest: string): string;
var
  HTTPReqw : TIdHTTP;
  IDSSL : TIdSSLIOHandlerSocketOpenSSL;
  strResultReqw : string;
begin
  HTTPReqw := TIdHTTP.Create;
  IDSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  try
    try

      HTTPReqw.IOHandler := IDSSL;
      HTTPReqw.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko';
      HTTPReqw.Request.Accept := 'text/html';
      HTTPReqw.Request.AcceptLanguage := 'ru,en-us;q=0.7,en;q=0.3';
      HTTPReqw.Request.AcceptCharSet := 'windows-1251,utf-8;q=0.7,*;q=0.7';
      HTTPReqw.ReadTimeout :=50000;
      HTTPReqw.ConnectTimeout := 50000;

      strResultReqw := HTTPReqw.Get(Trim(strHttpReqwest));

      if Pos('200 OK', HTTPReqw.Response.ResponseText) <> 0 then
      begin
        Result := strResultReqw;
      end else
      begin
        Result := '';
      end;

    except
      on E: Exception do
      begin
        ToLog('Ошибка при выполнении Http запроса:' + strHttpReqwest + '  ('+ E.Message + ')');
        Result := '';
      end;
    end;
  finally
    HTTPReqw.Free;
    IDSSL.Free;
  end;
end;

procedure TForm1.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    TGeneral.FPort := FServer.DefaultPort;
    FServer.Active := True;

    ToLog('WebServer Started');
    GetHttpreqwestResult('http://localhost:'+EditPort.Text+'/?{"Type":"GetZonesProgress","Account":{"Current":"1","Parents":[]}}');
    TGeneral.FUseAStarDll := chkDll.Checked;
  end;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
end;

{ GetAddresYaXml }

constructor GetAddres.Create(ARequest, ALog: string);
begin
  FRequest := ARequest;
  FLog := ALog;
  FStartTime := Now();
  inherited Create(True);
end;

procedure GetAddres.Execute;
var
  strResultReqw: string;
  cs: TCriticalSection;
begin
  cs := TCriticalSection.Create;
  try
    //cs.Enter;
    m_bisRuning := True;//поток работает!!

    m_iStatusComplete := 0;

    try
      strResultReqw := GetHttpreqwestResult(FRequest, FLog);
      ToLog(FLog + ' Length=' + IntToStr(Length(strResultReqw)));
//       m_iStatusComplete := -1;

    except
      on E: Exception do
      begin
        ToLog('Ошибка при обработке запроса '+ FLog + ' (' + E.Message + ')');
        m_iStatusComplete := -1;
      end;
    end;
  finally
     //cs.Leave;
     Sleep(20);
    cs.Free;
    m_bisRuning := False;//поток завершился
  end;

end;


function GetAddres.GetHttpreqwestResult(strHttpReqwest, sLog: string): string;
var
  HTTPReqw : TIdHTTP;
  IDSSL : TIdSSLIOHandlerSocketOpenSSL;
  strResultReqw : string;
begin
  ToLog('Start Get '+sLog);
  HTTPReqw := TIdHTTP.Create;
  IDSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  try
    try
      HTTPReqw.IOHandler := IDSSL;
      HTTPReqw.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko';
      HTTPReqw.Request.Accept := 'text/html';
      HTTPReqw.Request.AcceptLanguage := 'ru,en-us;q=0.7,en;q=0.3';
      HTTPReqw.Request.AcceptCharSet := 'windows-1251,utf-8;q=0.7,*;q=0.7';
      HTTPReqw.ReadTimeout :=50000;
      HTTPReqw.ConnectTimeout := 50000;

      strResultReqw := HTTPReqw.Get(Trim(strHttpReqwest));

      if Pos('200 OK', HTTPReqw.Response.ResponseText) <> 0 then
      begin
        Result := strResultReqw;
      end else
      begin
        Result := '';
      end;
      ToLog('End Get '+sLog);
    except
      on E: Exception do
      begin
        ToLog('Ошибка при выполнении Http запроса:' + strHttpReqwest + '  ('+ E.Message + ')');
        Result := '';
      end;
    end;
  finally
    HTTPReqw.Free;
    IDSSL.Free;
  end;
end;


{ GetAStar }

constructor GetAStar.Create(APoints: string; ASpeed, ALimit: integer; AZone,
  ASign: Uint64; AAccounts: TIntegerDynArray; ALog: string);
var
  i: Integer;
begin
  FPoints := APoints;
  FLog := ALog;
  FSpeed := ASpeed;
  FLimit := ALimit;
  FZone := AZone;
  FSign := ASign;
  SetLength(FAccounts, Length(AAccounts));
  for I := Low(AAccounts) to High(AAccounts) do
    FAccounts[i] := AAccounts[i];
//  FAccounts := AAccounts;
  FStartTime := Now();
  FMapEditor := TMapEditor.Create;
  inherited Create(True);
end;

destructor GetAStar.Destroy;
begin
  FMapEditor.Free;
  inherited;
end;

procedure GetAStar.Execute;
var
  strResultReqw: string;
  cs: TCriticalSection;
begin
  cs := TCriticalSection.Create;
  try
    cs.Enter;
    m_bisRuning := True;//поток работает!!

    m_iStatusComplete := 0;

    try
      strResultReqw := GetAStarResult(FPoints, FSpeed, FLimit, FZone, FSign, FAccounts, FLog);
//       m_iStatusComplete := -1;

    except
      on E: Exception do
      begin
        ToLog( 'Ошибка при обработке запроса '+ FLog +' (' + E.Message + ')');
        m_iStatusComplete := -1;
      end;
    end;
  finally
     cs.Leave;
     cs.Free;
     Sleep(20);

    m_bisRuning := False;//поток завершился
  end;

end;

function GetAStar.GetAStarResult(APoints: string; ASpeed, ALimit: integer;
  AZone, ASign: Uint64; AAccounts: TIntegerDynArray; ALog: string): string;
begin
  ToLog('Start AStar ' + ALog);
  Result := FMapEditor.GetAStarAcc(APoints,ASpeed, ALimit, AZone, 1, 30, ASign, AAccounts);
  ToLog('End AStar ' + ALog);
end;


end.
