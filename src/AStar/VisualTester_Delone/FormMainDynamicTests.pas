unit FormMainDynamicTests;

interface

uses
  SysUtils, Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Math, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  AStar64.DynImport, AStar64.Common, Geo.Pos, UGeoHash,
  ILSMapControlEx_TLB, Delaunay,
  ULogger;

type
  TMainForm = class(TForm)
    pMap: TPanel;
    sbMain: TStatusBar;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    pnl1: TPanel;
    mmoPoints: TMemo;
    btn1: TButton;
    procedure cbPredefinedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AllKeyPress(Sender: TObject; var Key: Char);
    procedure bMakePathClick(Sender: TObject);
    procedure bChangeClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    fs: TFormatSettings;
    //!
    FBitmapGreen: IObgBitmap;
    //!
    FBitmapBlue: IObgBitmap;
    //!
    FBitmapRed: IObgBitmap;
    //!
    FBitmapYellow: IObgBitmap;
    //!
    FBitmapCian: IObgBitmap;
    //!
    FMap: TILSMapEx;
    //!
    FZ1Line: ILayerLine;
    FZ2Line: ILayerLine;
    FZ3Line: ILayerLine;
    FLayerLine: ILayerLine;
    //!
    FLayerPath: ILayerLine;
    //!
    FLayerPathA: ILayerBitmap;
    //!
    FPBitmap: IObgBitmap;
    //! ТЕСТ
//    FLayerPoly: ILayerContour;
    //!
    FLatiFrom, FLongiFrom, FLatiTo, FLongiTo: Double;
    FSpeed: Integer;
    FMask: UInt64;
    //!
    FStructure: TAStarParam;
    //!
    FCarPass: Integer;
    //!
    TheMesh: TDelaunay;

    procedure MapClickLeft(
      ASender: TObject;
      ALatitude: Double;
      ALongitude: Double;
      AX: Integer;
      AY: Integer;
      AKeyState: TKeybState
    );
    //!
    procedure MapClickRight(
      ASender: TObject;
      ALatitude: Double;
      ALongitude: Double;
      AX: Integer;
      AY: Integer;
      AKeyState: TKeybState
    );
    //!
    procedure SetLine();
    procedure SetLines();
    //!
    procedure SetupStructure();
    procedure ReadEditString;
    procedure MakeEditString;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
//! выдаём скорость на основе "уровня" дороги
//------------------------------------------------------------------------------
function RoadSpeedByType(
  const AType: Integer
): Integer;
begin
  case AType of
    11: Result := 110;
    12: Result := 108;
    13: Result := 90;
    14: Result := 88;
    15: Result := 70;
    16: Result := 68;
    21: Result := 60;
    22: Result := 59;
    31: Result := 60;
    32: Result := 59;
    41, 42, 43: Result := 40;
    else Result := 20;
{
    11, 12: Result := 110;
    13, 14: Result := 90;
    15, 16: Result := 70; // 70?
    21, 22: Result := 60;
    31, 32: Result := 60;
    41, 42, 43: Result := 40;
    else Result := 20;
}
  end;
end;


function GetDigits(var s: string): string;
var
  Started: Boolean;
begin
  Result := '';
  Started := False;
  while Length(s) > 0 do
  begin
    if (s[1] in ['0'..'9','.']) then
    begin
      if not Started  then
        Started := True;;
      Result := Result + s[1];
    end else
      if Started then
        Break;

    Delete(s, 1, 1);
  end;

end;

function GetGeo(var s: string): Double;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  Result := StrToFloatDef(GetDigits(s), 0, fs);
end;

function GetSpeed(var s: string): Integer;
begin
  Result := StrToIntDef(GetDigits(s), 90);
end;

function GetZones(var s: string): UInt64;
begin
  Result := StrToInt64Def(GetDigits(s), 0);
end;

function GetTimeout(var s: string; const ADef: Integer): UInt64;
begin
  Result := StrToInt64Def(GetDigits(s), ADef);
end;


function CallBack(
  const AHandle: Pointer;
  const AMetaData: Pointer
): Integer; stdcall;
begin
  Result := RoadSpeedByType(PMetaDataV1(AMetaData)^.RoadType);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMap := TILSMapEx.Create( nil );
  FMap.SetParentComponent( pMap );
  FMap.Align := alClient;
  FMap.LoadMap( mpYandex );
  FMap.CenterMapOnCoords( 55.75, 37.625 );
//  FMap.Zoom := 12;
  //
  FMap.OnMClick := MapClickLeft;
  FMap.OnMClickRight := MapClickRight;
  //

  FBitmapGreen := CoObgBitmap.Create();
  FBitmapGreen.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_5.ico', False, False );
  FBitmapBlue := CoObgBitmap.Create();
  FBitmapBlue.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_6.ico', False, False );
  FBitmapRed := CoObgBitmap.Create();
  FBitmapRed.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_7.ico', False, False );
  FBitmapYellow := CoObgBitmap.Create();
  FBitmapYellow.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_8.ico', False, False );
  FBitmapCian := CoObgBitmap.Create();
  FBitmapCian.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_9.ico', False, False );

  //
  FLayerLine := CoLayerLine.Create();
  FMap.AddLayer( FLayerLine );
  FLayerLine.Color := $005878b8;
  FLayerLine.Visible := True;


  FZ1Line := CoLayerLine.Create();
  FMap.AddLayer( FZ1Line );
  FZ1Line.Color := $002878b8;
  FZ1Line.Visible := True;
  FZ2Line := CoLayerLine.Create();
  FMap.AddLayer( FZ2Line );
  FZ2Line.Color := $005828b8;
  FZ2Line.Visible := True;
  FZ3Line := CoLayerLine.Create();
  FMap.AddLayer( FZ3Line );
  FZ3Line.Color := $00587828;
  FZ3Line.Visible := True;
  //
  FLayerPath := CoLayerLine.Create();
  FMap.AddLayer( FLayerPath );
  FLayerPath.Color := $00a050a0;
  FLayerPath.Visible := True;
  //
  FPBitmap := CoObgBitmap.Create();
  FPBitmap.FillColor := $00a050a0;
  FPBitmap.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\Arrow_c_w.bmp', True, True );
  //
  FLayerPathA := CoLayerBitmap.Create();
  FMap.AddLayer( FLayerPathA );
  //
//  SetFilePath( PWideChar( ExtractFilePath( ParamStr( 0 ) ) ) );
  //
(*
  FLayerPoly := CoLayerContour.Create();
  FMap.AddLayer( FLayerPoly );
  FLayerPoly.FillColor := $00c09070;
  FLayerPoly.Visible := True;
  FLayerPoly.AddPoint( 55.744278, 37.843216, 1 );
  FLayerPoly.AddPoint( 55.764414, 37.369430, 2 );
  FLayerPoly.AddPoint( 55.906834, 37.541918, 3 );
  FLayerPoly.AddPoint( 55.576495, 37.687912, 4 );
*)
  Edit1Change(Edit1);
  TheMesh := TDelaunay.Create;
  TheMesh.TargetForm := MainForm;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
(*
  FLayerPoly.Visible := False;
  FLayerPoly := nil;
*)
  //
  FLayerPathA.Visible := False;
  FLayerPathA := nil;
  FLayerPath.Visible := False;
  FLayerPath := nil;
  FLayerLine.Visible := False;
  FLayerLine := nil;
  FZ1Line.Visible := False;
  FZ1Line := nil;
  FZ2Line.Visible := False;
  FZ2Line := nil;
  FZ3Line.Visible := False;
  FZ3Line := nil;
  FPBitmap := nil;
  FreeAndNil( FMap );
end;

procedure TMainForm.AllKeyPress(Sender: TObject; var Key: Char);
begin
  if ( Key <> #8) and ( Key <> ',' ) and ( ( Key < '0' ) or ( Key > '9' ) ) then
    Key := #0;
end;

procedure TMainForm.cbPredefinedChange(Sender: TObject);
var
  //!
  ACriterion: Integer;
//------------------------------------------------------------------------------
begin
end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  ReadEditString;
  SetLine;
end;

function DirectionAngle(
  const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double
): Double;
begin
  Result := ArcTan( ( ALatitudeTo - ALatitudeFrom ) / ( ( ALongitudeTo - ALongitudeFrom ) * Cos( DegToRad( ALatitudeTo ) ) ) );
  if ( ALongitudeTo < ALongitudeFrom ) then
  begin
    if ( ALatitudeTo > ALatitudeFrom ) then
      Result := Result + Pi
    else
      Result := Result - Pi;
  end;
end;




procedure TMainForm.bMakePathClick(Sender: TObject);
const
  CArrowMinDegree = 0.10471975511966; // 6 градусов
var
  Rez: Integer;
//  FromLatitude: Double;
//  FromLongitude: Double;
//  ToLatitude: Double;
//  ToLongitude: Double;
  Distance: Double;
  Duration: Double;
  I: Integer;
  //! координаты
  PrevLatitude, PrevLongitude, Latitude, Longitude: Double;
  //!
  Di, Du: Integer;
  //! корневой элемент пути
  PathRoot: PCoords;
  //! указатель для цикла
  FieldCoords: PCoords;
  //! счётчик отрезков плана
  CounterMoves, CounterRot: Integer;
  //! временная
  TempFloat: Double;
  //! направления
  Dir, PrevDir: Double;
  //!
  KStart, KEnd: TLargeInteger;
  //! число тиков в секунду
  KCounterPerSec: TLargeInteger;
  //! результат PAnsiChar
//  RezChar: PAnsiChar;
  //! результат string
  RezStr: string;
  //! доп. переменная
  TempStr: string;
  //!
  RCoords: TGeoPosArray;
  //!
  Strs: TStringList;
//------------------------------------------------------------------------------
  ar: TAstarRequest;
  CitySpeed: Integer;
  Speed:  Integer;

  s: string;
begin
  s := Edit1.Text;
  LoadAStar64;

  try
//    FromLatitude := FLatiFrom;
//    FromLongitude := FLongiFrom;
//    ToLatitude := FLatiTo;
//    ToLongitude := FLongiTo;
  //  Rez := CreateRouteWithSpeedline(
  QueryPerformanceFrequency( KCounterPerSec );
  QueryPerformanceCounter( KStart );

  Speed := 58;
  with ar do
  begin
    ToLog(s);
    Version := 1;
    FromLatitude := GetGeo(s);
    FromLongitude := GetGeo(s);
    ToLatitude := GetGeo(s);
    ToLongitude := GetGeo(s);
    Speed := GetSpeed(s);
//    with RoadSpeedsRecord do begin
//        CitySpeed := Speed;
//        Motorway := Speed;
//        MotorwayLink := Trunc(Speed / 2);
//        Trunk := Speed;
//        TrunkLink := Trunc(Speed / 2);
//        Primary := CitySpeed;
//        PrimaryLink := Trunc(CitySpeed / 2);
//        Secondary := CitySpeed;
//        SecondaryLink := Trunc(CitySpeed / 2);
//        Tertiary := CitySpeed;
//        TertiaryLink := Trunc(CitySpeed / 2);
//        Residential := CitySpeed;
//        Road := Speed;
//        Unclassified := Speed;
//        reverse := 60;
//    end;
//    GetRoadSpeedsDefault(@RoadSpeedsRecord);
    GetRoadSpeedsKToDef(@RoadSpeedsRecord, Speed, 0);

    ZonesLimit    := GetZones(s);
    FormatVariant := 2;
    LenTreshold   := 15;
    Timeout       := GetTimeout(s, 300000);
    Distance      := 0;
    Duration      := 0;
    BufferLen     := 1024*1024;
    HashString    := AllocMem(1024*1024);
  end;

  Rez := CreateRouteWithPath3(@ar);
//    Rez := CreateRouteWithPath(
//      nil,
//      nil,
//      FromLatitude,
//      FromLongitude,
//      ToLatitude,
//      ToLongitude,
//      Distance,
//      Duration,
//      RezChar
//    );
  QueryPerformanceCounter( KEnd );
  sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';
    if (Rez = 0) then
    begin
      RezStr := string(AnsiString(ar.HashString));//RezChar
      ToLog(RezStr);
      FreeMem(ar.HashString);
//      Strs := TStringList.Create();
//      Strs.Text := RezStr + #13#10;
//      Strs.SaveToFile(ExtractFilePath(ParamStr(0)) + 'log.txt');
//      Strs.Free();
  //    TGeoHash.DecodeArrayWorld(RezStr, RCoords);
  //
      SetLength(RCoords, 0);
      case ar.FormatVariant of
        0: I := 3;
        else I := 3;
      end;


      repeat
        TempStr := Copy(RezStr, I, 12);
        if (TempStr = '') then Break;
        SetLength(RCoords, Length(RCoords) + 1);
        TGeoHash.DecodePointString(
          TempStr,
          RCoords[High(RCoords)].Latitude,
          RCoords[High(RCoords)].Longitude
        );

        case ar.FormatVariant of
          0: Inc(I, 12);
          else //if I = 3 then
              //Inc(I, 12)
            //else
              Inc(I, 15);
        end;

      until False;
  //
      FLayerPath.ChainOn();
      FLayerPath.ClearAllPoints();
      for I := Low(RCoords) to High(RCoords) do
      begin
        FLayerPath.AddPoint(I, RCoords[I].Latitude, RCoords[I].Longitude);
      end;
      FLayerPath.ChainOff();
      sbMain.Panels[2].Text := Format('Длина: %.3f км // Время: %.1f мин', [ar.Distance, ar.Duration * 60 * 24]);
    end
    else
    begin
      RezStr := string(AnsiString(ar.HashString));
      ToLog('Rez'+IntTostr(rez));
      ToLog(RezStr);
      FLayerPath.ChainOn();
      FLayerPath.ClearAllPoints();
      FLayerPath.ChainOff();
      sbMain.Panels[2].Text := 'Rez = ' + IntToStr(Rez);
    end;

  finally
    UnloadAStar64;
  end;
end;

procedure TMainForm.btn1Click(Sender: TObject);
begin
  SetLines;
end;

procedure TMainForm.Button3Click(Sender: TObject);
type
  TAreaRecord = record
    //! номер (0-63)
    Number: Integer;
    //! используется/не используется
    Active: Integer;
    //! текстовый гео-хэш
    AreaHash: string;
    //! бинарное представление
    Area: TGeoPosArray;
  end;
  TAreaRecordArray = TArray<TAreaRecord>;

//------------------------------------------------------------------------------
var
  CmdArray: TAreaRecordArray;

const
  CCmdFile = 'ImplementedZones.txt';

  procedure ParseZones();
  var
    //!
    I: Integer;
  //------------------------------------------------------------------------------
  begin
    // парсинг
    for I := Low(CmdArray) to High(CmdArray) do
    begin
      TGeoHash.DecodeArrayWorld(CmdArray[I].AreaHash, CmdArray[I].Area);
    end;
  end;

  procedure ReadCmdFile();
  var
    //!
    CmdList: TStringList;
    //!
    TempStr: string;
    //!
    TabPos: Integer;
    //!
    I: Integer;
  //------------------------------------------------------------------------------
  begin
    CmdList := TStringList.Create();
    CmdList.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + CCmdFile);
    SetLength(CmdArray, CmdList.Count);
    for I := 0 to CmdList.Count - 1 do
    begin
      TempStr := CmdList[I];
      // _имя
      TabPos := Pos(#9, TempStr);
      Delete(TempStr, 1, TabPos);
      // _приоритет
      TabPos := Pos(#9, TempStr);
      CmdArray[I].Number := StrToInt(Copy(TempStr, 1, TabPos - 1));
      Delete(TempStr, 1, TabPos);
      // _необходимость
      TabPos := Pos(#9, TempStr);
      CmdArray[I].Active := StrToInt(Copy(TempStr, 1, TabPos - 1));
      Delete(TempStr, 1, TabPos);
      // _цвет
      TabPos := Pos(#9, TempStr);
      Delete(TempStr, 1, TabPos);
      // _координаты
      CmdArray[I].AreaHash := TempStr;
    end;
    CmdList.Free();
  end;

  procedure DrawZone(var AZLine: ILayerLine; const aArea: TGeoPosArray);
  var
    I: Integer;
  begin
      AZLine.ChainOn();
      AZLine.ClearAllPoints;
      for I := Low(aArea) to High(aArea) do
      begin
        AZLine.AddPoint(I, aArea[I].Latitude, aArea[I].Longitude);
      end;
      AZLine.ChainOff();

  end;

begin
  ReadCmdFile;
  ParseZones;
  DrawZone(FZ1Line, CmdArray[0].Area);
  DrawZone(FZ2Line, CmdArray[1].Area);
  DrawZone(FZ3Line, CmdArray[2].Area);
end;

procedure TMainForm.MapClickLeft(
  ASender: TObject;
  ALatitude: Double;
  ALongitude: Double;
  AX: Integer;
  AY: Integer;
  AKeyState: TKeybState
);
begin
  fs := TFormatSettings.Create('ru-ru');
  fs.DecimalSeparator := '.';
  mmoPoints.Lines.Add(format('%.6f %.6f', [ALatitude, ALongitude],fs));
{  ReadEditString;
  FLatiFrom := ALatitude;
  FLongiFrom := ALongitude;
  MakeEditString;
  FLatiFrom := ALatitude;
  FLongiFrom := ALongitude;
  TheMesh.AddPoint(ALongitude, ALatitude); //add a point to the mesh
  TheMesh.Mesh;          //triangulate the mesh
  TheMesh.Draw;   //draw the mesh on the forms canvas}
end;

procedure TMainForm.MapClickRight(
  ASender: TObject;
  ALatitude: Double;
  ALongitude: Double;
  AX: Integer;
  AY: Integer;
  AKeyState: TKeybState
);
begin
  ReadEditString;
  FLatiTo := ALatitude;
  FLongiTo := ALongitude;
  MakeEditString;
end;

function GeoLengthLongRad(
  const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double;
  const ARadius: Double
): Double;
begin
  Result := ArcCos( Sin( ALatitude1 ) * Sin( ALatitude2 ) + Cos( ALatitude1 ) * Cos( ALatitude2 ) * Cos( ALongitude1 - ALongitude2 ) ) * ARadius;
end;

procedure TMainForm.SetLine();
var
  //!
  Len: Double;
//------------------------------------------------------------------------------
begin
  Len := GeoLengthLongRad(
    DegToRad( FLatiFrom ),
    DegToRad( FLongiFrom ),
    DegToRad( FLatiTo ),
    DegToRad( FLongiTo ),
    6371.302
  );
  sbMain.Panels[0].Text := Format( 'Расстояние: %.3f Км', [Len] );
  //
  FLayerLine.ChainOn();
  FLayerLine.ClearAllPoints();
  FLayerLine.AddPoint( 0, FLatiFrom, FLongiFrom );
  FLayerLine.AddPoint( 1, FLatiTo, FLongiTo );
  FLayerLine.ChainOff();
end;

procedure TMainForm.SetLines;
var
  i: Integer;
  Lat, Lon: Double;
  s: string;
begin
//  TheMesh := TDelaunay.Create;
  FLayerLine.ChainOn();
  FLayerLine.ClearAllPoints();
  for I := 0 to mmoPoints.Lines.Count - 1 do
    begin
      s := mmoPoints.Lines[i];
      Lat := GetGeo(s)*100;
      Lon := GetGeo(s)*100;
//      FLayerLine.AddPoint( i, Lat, Lon );
//      ToLog(Format('i=%d Lat=%g Lon=%g',[i,Lat,Lon]));
      TheMesh.AddPoint(Lon,Lat); //add a point to the mesh
    end;
  TheMesh.Mesh;          //triangulate the mesh
  ToLog(Format('HowMany=%d',[TheMesh.HowMany]));
//    TheMesh.Draw;   //draw the mesh on the forms canvas
  if (TheMesh.HowMany > 0) then
  begin
    For i:= 1 To TheMesh.HowMany do
    begin
      FLayerLine.AddPoint( (i-1)*3, TheMesh.Vertex^[TheMesh.Triangle^[i].vv0].Latitude/100,
                                    TheMesh.Vertex^[TheMesh.Triangle^[i].vv0].Longitude/100 );
      FLayerLine.AddPoint( (i-1)*3+1, TheMesh.Vertex^[TheMesh.Triangle^[i].vv1].Latitude/100,
                                      TheMesh.Vertex^[TheMesh.Triangle^[i].vv1].Longitude/100 );
      FLayerLine.AddPoint( (i-1)*3+2, TheMesh.Vertex^[TheMesh.Triangle^[i].vv2].Latitude/100,
                                      TheMesh.Vertex^[TheMesh.Triangle^[i].vv2].Longitude/100 );
      ToLog(Format('i=%d %.6f %.6f',[(i-1)*3,TheMesh.Vertex^[TheMesh.Triangle^[i].vv0].Latitude/100,
                                                 TheMesh.Vertex^[TheMesh.Triangle^[i].vv0].Longitude/100],fs));
      ToLog(Format('i=%d %.6f %.6f',[(i-1)*3+1,TheMesh.Vertex^[TheMesh.Triangle^[i].vv1].Latitude/100,
                                                 TheMesh.Vertex^[TheMesh.Triangle^[i].vv1].Longitude/100],fs));
      ToLog(Format('i=%d %.6f %.6f',[(i-1)*3+2,TheMesh.Vertex^[TheMesh.Triangle^[i].vv2].Latitude/100,
                                                 TheMesh.Vertex^[TheMesh.Triangle^[i].vv2].Longitude/100],fs));
//    TempBuffer.Canvas.Polygon([Point(Trunc(Vertex^[Triangle^[i].vv0].Longitude), Trunc(Vertex^[Triangle^[i].vv0].Latitude)),
//                                  Point(Trunc(Vertex^[Triangle^[i].vv1].Longitude), Trunc(Vertex^[Triangle^[i].vv1].Latitude)),
//                                  Point(Trunc(Vertex^[Triangle^[i].vv2].Longitude), Trunc(Vertex^[Triangle^[i].vv2].Latitude))]);
    end;
  end;
//  FLayerLine.AddPoint( 0, FLatiFrom, FLongiFrom );
//  FLayerLine.AddPoint( 1, FLatiTo, FLongiTo );
  FLayerLine.ChainOff();

{  FLayerLine.ChainOn();
  FLayerLine.ClearAllPoints();
  FLayerLine.AddPoint( 0, FLatiFrom, FLongiFrom );
  FLayerLine.AddPoint( 1, FLatiTo, FLongiTo );
  FLayerLine.ChainOff();}
end;

procedure TMainForm.SetupStructure();
begin
end;

procedure TMainForm.ReadEditString;
var
  s: string;
begin
  s := Edit1.Text;
  FLatiFrom := GetGeo(s);
  FLongiFrom := GetGeo(s);
  FLatiTo := GetGeo(s);
  FLongiTo := GetGeo(s);
  FSpeed := GetSpeed(s);
  FMask := GetZones(s);
end;

procedure TMainForm.MakeEditString;
var
  s: string;
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create('ru-ru');
  fs.DecimalSeparator := '.';
  Edit1.Text := format('[%.6f,%.6f]-[%.6f,%.6f] %d %d', [FLatiFrom, FLongiFrom, FLatiTo, FLongiTo, FSpeed, FMask],  fs);
//  [57.325853,61.901568]-[57.376582,62.291365] 58 0
end;

procedure TMainForm.bChangeClick(Sender: TObject);
var
  l: Double;
  s: string;
//------------------------------------------------------------------------------
begin
  ReadEditString;

  l := FLatiTo;
  FLatiTo := FLatiFrom;
  FLatiFrom := l;
  l := FLongiTo;
  FLongiTo := FLongiFrom;
  FLongiFrom := l;

  MakeEditString;

  SetLine();
end;

end.

