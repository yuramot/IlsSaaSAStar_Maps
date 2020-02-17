unit ULPCCore;

interface

uses
  SysUtils, IoUtils, Types, System.Generics.Collections,
  LandmarkPathCalculator,  AStar64.LandMark,
  Geo.Pos, Geo.Hash, Geo.Hash.Search, ULPCThread, System.Classes;

procedure ProcessAccs(
  const APathList: TArray<string>;
  const ATimeout: Integer;
  const ARecalcAll: Boolean;
  const AAccount: Integer
);

implementation

procedure CalcBorders(
  const ACoords: TGeoPosArray;
  out RBorder: TGeoPosArray;
  out RNW: TGeoPos;
  out RSE: TGeoPos
);
var
  Iter: TGeoPos;
begin
  RNW.Latitude := -90;
  RNW.Longitude := 180;
  RSE.Latitude := 90;
  RSE.Longitude := -180;
  for Iter in ACoords do
  begin
    if (RNW.Latitude < Iter.Latitude) then
      RNW.Latitude := Iter.Latitude;
    if (RNW.Longitude > Iter.Longitude) then
      RNW.Longitude := Iter.Longitude;
    if (RSE.Latitude > Iter.Latitude) then
      RSE.Latitude := Iter.Latitude;
    if (RSE.Longitude < Iter.Longitude) then
      RSE.Longitude := Iter.Longitude;
  end;
  SetLength(RBorder, 4);
  RBorder[0] := RNW;
  RBorder[1].Create(RNW.Latitude,RSE.Longitude);
  RBorder[2] := RSE;
  RBorder[3].Create(RSE.Latitude,RNW.Longitude);
end;

procedure ProcessAccs(
  const APathList: TArray<string>;
  const ATimeout: Integer;
  const ARecalcAll: Boolean;
  const AAccount: Integer
);
var
  LandmarkFilesList: TStringDynArray;
  LandmarkFileName: string;
  PathStr: string;
  SwitchVal: string;
  LMFile: Text;
  Geos: TGeoPosArray;
  NW, SE: TGeoPos;
  GeosHigh: Integer;
  Areas: TLandMarkAreaDictionary;
  ID: Integer;
  AreaInfo: TLandMarkAreaInfo;
//  LandMarkMatrix: TLandMarkMatrix;
  RootPath: string;
  iPos: Integer;
  sAccount: string;
  Accounts: TIntegerDynArray;

begin
  FileMode := 0;
  GeosHigh := 0;
  ID := 1;
  SetLength(Accounts, 1);
  RootPath := ExtractFilePath(ParamStr(0));
  Areas := TLandMarkAreaDictionary.Create;// <Integer, TLandMarkAreaInfo>.Create;
  Accounts[0] := AAccount;
  try
    for PathStr in APathList do
    begin
      Log('Now process folder ' + PathStr);
      sAccount := PathStr;
      Delete(sAccount, Length(sAccount),1);
      iPos := Pos('\',sAccount);
      while iPos > 0 do
      begin
        Delete(sAccount, 1, iPos);
        iPos := Pos('\',sAccount);
      end;
//      Accounts[0] := StrToInt(sAccount);

      LandmarkFilesList := TDirectory.GetFiles(PathStr, '*.lmd');
      if (Length(LandmarkFilesList) = 0) then
        Continue;
      for LandmarkFileName in LandmarkFilesList do
      begin
        SetLength(Geos, 0);
        GeosHigh := 0;
        SetLength(AreaInfo.LandMarks, 0);
        SetLength(AreaInfo.Polygon, 0);
        AreaInfo.LandMarksHash := '';
        AreaInfo.PolygonHash := '';
        Log('Now process landmark file ' + LandmarkFileName);
        AssignFile(LMFile, LandmarkFileName);
        Reset(LMFile);
        try
          while not Eof(LMFile) do
          begin
            SetLength(Geos, GeosHigh + 1);
            Readln(LMFile, Geos[GeosHigh].Latitude, Geos[GeosHigh].Longitude);
            Inc(GeosHigh);
          end;
        finally
          CloseFile(LMFile);
        end;
        CalcBorders(Geos, AreaInfo.Polygon, NW, SE);
        if not FindCmdLineSwitch('t', SwitchVal, False, [clstValueAppended]) then
          SwitchVal := '3';

        AreaInfo.State := stNoCalc;
        AreaInfo.Name := ChangeFileExt(ExtractFileName(LandmarkFileName),'');
        AreaInfo.GeoHashID := 'PA' + NW.ToGeoHash+SE.ToGeoHash;
        AreaInfo.PolygonHash := TGeoHash.EncodeArrayWorld(AreaInfo.Polygon);
        AreaInfo.LandMarks := Geos;
        AreaInfo.LandMarksHash := TGeoHash.EncodeArrayWorld(Geos,gfPointsArray);
//        LandMarkMatrix := TLandMarkMatrix.Create('', '');

//        TLPC.GenMulti(PathStr, TGeoHash.EncodePointString(NW, 12) + '_' + TGeoHash.EncodePointString(SE, 12),
        TLPC.GenMulti(RootPath, TGeoHash.EncodePointString(NW, 12) + '_' + TGeoHash.EncodePointString(SE, 12),
                      Geos, Accounts, nil, StrToIntDef(SwitchVal, 4), ATimeout, ARecalcAll, sAccount);
//        TLPC.GetCalcMulti(RootPath, Accounts, NW.ToGeoHash + '_' + SE.ToGeoHash, Geos, nil, StrToIntDef(SwitchVal, 4), ATimeout, LandMarkMatrix);
//        TLPC.GetCalcMulti(RootPath, Accounts, nil, StrToIntDef(SwitchVal, 4), ATimeout, LandMarkMatrix);
        Areas.AddOrSetValue(ID, AreaInfo);
        Inc(ID);
      end;
{      if sAccount = '' then
        Areas.Save(RootPath, Accounts[0])
      else
        Areas.Save(RootPath, StrToInt(sAccount));}
      Areas.Save(RootPath, StrToInt(sAccount));
      Areas.Clear;
    end;
  finally
    Areas.Free;
  end;
end;

end.

