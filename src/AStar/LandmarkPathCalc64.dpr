program LandmarkPathCalc64;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Windows,
  ULPCCore in 'ULPCCore.pas',
  LandmarkPathCalculator in 'LandmarkPathCalculator.pas',
  Geo.Pos in '..\_Common\Geo.Pos.pas',
  Geo.Hash in '..\_Common\Geo.Hash.pas',
  Geo.Calcs in '..\_Common\Geo.Calcs.pas',
  Ils.Utils in '..\_Common\Ils.Utils.pas',
  UStructArray in '..\_Common\UStructArray.pas',
  AStar64.LandMark in 'AStar64.LandMark.pas',
  AStar64.FileStructs in '..\_Common\AStar64.FileStructs.pas',
  AStar64.Areas in '..\_Common\AStar64.Areas.pas',
  AStar64.Files in '..\_Common\AStar64.Files.pas',
  AStar64.DynImport in 'AStar64.DynImport.pas',
  ULPCThread in 'ULPCThread.pas',
  Ils.Logger in '..\_Common\Ils.Logger.pas',
  Geo.Hash.Search in '..\_Common\Geo.Hash.Search.pas',
  Ils.Json.Names in '..\_Common\Ils.Json.Names.pas',
  Ils.Json.Utils in '..\_Common\Ils.Json.Utils.pas',
  JsonDataObjects in '..\..\lib\Json\JsonDataObjects.pas';

var
  GProgPath: string;
  TempStr: string;
  GPathList: TArray<string>;
  GSR: TSearchRec;
  I: Integer;
  Timeout: Integer;
  Account: Integer;
  RecalcAll: Boolean;
//  Account: Integer;

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

procedure AddPath(
  const APath: string
);
begin
  SetLength(GPathList, Length(GPathList) + 1);
  GPathList[High(GPathList)] := IncludeTrailingPathDelimiter(GProgPath + APath);
end;

begin
  try
    if ParamCount = 0 then
    begin
      Writeln('Запуск программы обязателен с параметрами');
      Writeln('Пример: LandmarkPathCalc.exe 1 -g:1 -t:3 -timeout:900 -calc:all');
      Writeln('1 - папка с lmd-файлами');
      Writeln('-t - количество задействуемых процессоров');
      Writeln('-g - папка с графами (по умолчанию 1)');
      Writeln('-timeout - таймаут при расчете треков, по умолчанию 60');
      Writeln('-calc - если не all, то считает непросчитанные');
      Readln;
      Exit;
    end;
    Timeout := 60;
    RecalcAll := False;
    Account := 1;
//    Account := 1;
    DecimalSeparator := '.';
    GProgPath := ExtractFilePath(ParamStr(0));
    if FindCmdLineSwitch('timeout', TempStr, False, [clstValueAppended]) then
       Timeout := StrToInt(TempStr);
    if FindCmdLineSwitch('calc', TempStr, False, [clstValueAppended]) then
       RecalcAll := TempStr='all';
    if FindCmdLineSwitch('g', TempStr, False, [clstValueAppended]) then
       Account := StrToInt(TempStr);
    if (ParamCount > Ord(FindCmdLineSwitch('t', TempStr, False, [clstValueAppended]))) then
    begin
//      Writeln('ParamCount ='+IntToStr(ParamCount));
      for I := 1 to ParamCount do
      begin
        if (ParamStr(I)[1] <> '-') then
        begin
          AddPath(ParamStr(I));
//          Account := StrToInt(ParamStr(I));
        end;
      end;
    end
    else
    begin
      if (SysUtils.FindFirst(GProgPath + '*', faDirectory, GSR) = 0) then
      begin
        try
          repeat
            if ((GSR.Attr and faDirectory) <> GSR.Attr)
            or (GSR.Name = '.')
            or (GSR.Name = '..') then
              Continue;
            AddPath(GSR.Name);
          until (SysUtils.FindNext(GSR) <> 0);
        finally
          SysUtils.FindClose(GSR);
        end;
      end;
    end;
  //  Writeln('ProcessAccs, Timeout='+IntToStr(Timeout));
    ProcessAccs(GPathList, Timeout, RecalcAll, Account);
  except
    on E: Exception do
    begin
      Log('Global eroor: ' + E.ClassName + ': ' + E.Message);
    end;
  end;
end.

