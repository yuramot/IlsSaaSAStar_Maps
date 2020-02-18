unit MonitoringLoaderMain;

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, SvcMgr, Dialogs,
  IniFiles, ActiveX,
  IdHTTP, IdSSLOpenSSL, IdMultipartFormData, JsonDataObjects, Ils.Files.Pnt,
  Generics.Collections, synacode, Ils.Json.Names, Ils.Json.Utils, DateUtils,
  C_FileNames, E_UtilsFiles, E_Logger, E_Threads, E_Files, E_FileVersionUtils,
  Ils.Utils,
  Data.DB, Data.Win.ADODB;

//------------------------------------------------------------------------------
type
  TSensorMap = TStringList;
//  TSensorMap = TDictionary<Integer, TStringList>;
  TDeviceMap = class(TDictionary<Integer, TSensorMap>)
    function ToJson: string;
    procedure Clear; reintroduce;
    destructor Destroy; override;
  end;

  TProviderMap = class(TDictionary<Integer, TDeviceMap>)
    procedure AddMap(AProviderID: Integer; ADeviceType: Integer; ASensorMap: TStringList);
    procedure Clear; reintroduce;
    destructor Destroy; override;
  end;


  TILSLoader = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    //! список провайдеров
    FInfo: TStringList;
    //! логгер
    FLogger: TLogger;
    //! центральный класс
    FCoreThread: TThreadInfinite;
    //! HTTP сервер
    FServer: TIdHTTP;
    //! ID текущего обрабатываемого провайдера
    FProvID: string;
    //! каталог текущего обрабатываемого провайдера
    FProvPath: string;
    //! собранный адрес обращения
    FWhere: string;

    FKPAddress: string;

    FKPCommand: string;

    FKPUser: string;

    FKPPassword: string;

    FKPCount: Integer;

    FKPOffset: Int64;

    FKPSensorMap: TProviderMap;

    Fpid: string;

    FImeiHash: TDictionary<string, integer>;

    FDBConnection: string;
  public
    //!
    function GetServiceController: TServiceController; override;
    //! собственно сама рабочая процедура
    procedure InternalAllWork();
    //! выполнение обычного запроса к серверу
    function SendCommand(
      const ACommand: string
    ): Boolean;
    //! реализация передачи данных серверу; ответ - строка
    function SendDataStream(
      const ARequest: string;
      const ADataStream: TIdMultiPartFormDataStream
    ): string;
    //! выполнения запроса к серверу для записи данных в поток
    //! функция чистит входной поток
    function GetResponseForRequest(
      const ARequest: string;
      var RResponse: TMemoryStream
    ): Boolean;
    //! выполнения запроса к серверу; ответ - строка
    function GetResponse(
      const ARequest: string
    ): string;
    //!
    function GetDataAndWriteInFileNew(
      const AExt: string
    ): Boolean;

    function GetKafkaDataAndWriteInFileNew(
      const AExt: string
    ): Boolean;
    //! отправка данных серверу
    procedure SendDataNew(
      const ACommand: string;
      const AFileExt: string
    );
    //! отправка на сервер любых файлов
    procedure SendAnyFile();
    //! приём с файл-сервера любых файлов
    procedure GetAnyFile();
    //! функция проверки соединения с сервером
    function CheckServerOk(): Boolean;
  end;

//------------------------------------------------------------------------------
var
  ILSLoader: TILSLoader;

//------------------------------------------------------------------------------
implementation

{$R *.DFM}

//------------------------------------------------------------------------------
const

  cServerSec = 'server';
  cLoginSec = 'login';

//------------------------------------------------------------------------------
//! приставка обращения
//------------------------------------------------------------------------------
//а если мы переедем на https://?
//  CHttp = 'http://';

//------------------------------------------------------------------------------
//! путь к WebDataModule.exe
//------------------------------------------------------------------------------
  CWDMRoute = '/cgi-bin/WebDataModule.exe?';

  cDefTimeout = 60;
  cDefServerAddress = 'http://1.ils-glonass.ru';
  cDefProxyAddress = '';
  cDefProxyPort = 0;
  cDefProxyUser = '';
  cDefProxyPassword = '';


//------------------------------------------------------------------------------
//! команды файл-серверу
//------------------------------------------------------------------------------
  CSrvTest = 'GetTst';
  CFileGetInfo = 'GFileI';
  CFileReceive = 'GFileT';
  CFileConfirm = 'GFileO';
  CFileNameGet = '&File=';
  CFileSend = 'SendAFile';
  CFileNameSend = '&FileName=';
  CUniInfo = 'G2Q';
  CUniReceive = 'G2G';
  CUniConfirm = 'G2O';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ILSLoader.Controller(CtrlCode);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function TILSLoader.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

//------------------------------------------------------------------------------
// рабочая процедура
//------------------------------------------------------------------------------
procedure AllWork(
  const ARef: Pointer
);
begin
  TILSLoader( ARef ).InternalAllWork();;
end;

//------------------------------------------------------------------------------
// TILSLoaders
//------------------------------------------------------------------------------

procedure TILSLoader.ServiceStart(
  Sender: TService;
  var Started: Boolean
);
var
  //! файл настроек
  INIFile: TIniFile;
  //! тайм-аут связи
  Timeout: Integer;
  //! конечный сервер связи
  ServerUrl: string;
  //! прокси сервер
  ProxyAddr: string;
  //! порт прокси сервера
  ProxyPort: Integer;
  //! юзернэйм прокси сервера
  ProxyUser: string;
  //! пароль прокси сервера
  ProxyPass: string;
  imei: TStringList;
  s: string;
  id: Integer;
//------------------------------------------------------------------------------
begin
  Started := False;
{$IFDEF DEBUG}
  Windows.Sleep( 10000 );
{$ENDIF}
  //
  FLogger := TLogger.Create( ParamStr( 0 ), 5 * 1024 * 1024, 10 );
  FLogger.ToLog( '============ СТАРТ ============' );
  FLogger.ToLog('Версия приложения: ' + GetApplicationVersion());
  FLogger.ErrorToLog('Версия приложения: ' + GetApplicationVersion());
  // загрузка параметров
  try
    INIFile := TIniFile.Create( ChangeFileExt( ParamStr( 0 ), CIniDefExtDotted ) );
  except
    on Ex: Exception do
    begin
      FLogger.ErrorToLog( 'Ошибка чтения параметров из INI-файла:'#13#10 + Ex.Message );
      Exit;
    end;
  end;
  try
    Timeout := INIFile.ReadInteger( 'Main Params', 'Timeout', cDefTimeout);
    FLogger.ToLog( Format( 'Тайм-аут связи => "%d"', [Timeout] ) );
    ServerUrl := INIFile.ReadString( 'Main Params', 'ServerUrl', cDefServerAddress);
    FLogger.ToLog( Format( 'Адрес сервера => "%s"', [ServerUrl] ) );
    ProxyAddr := INIFile.ReadString( 'Main Params', 'ProxyAddr', cDefProxyAddress);
    FLogger.ToLog( Format( 'Адрес прокси => "%s"', [ProxyAddr] ) );
    ProxyPort := INIFile.ReadInteger( 'Main Params', 'ProxyPort', cDefProxyPort);
    FLogger.ToLog( Format( 'Порт прокси => "%d"', [ProxyPort] ) );
    ProxyUser := INIFile.ReadString( 'Main Params', 'ProxyUser', cDefProxyUser);
    FLogger.ToLog( Format( 'Юзернэйм прокси => "%s"', [ProxyUser] ) );
    ProxyPass := INIFile.ReadString( 'Main Params', 'ProxyPass', cDefProxyPassword);
    FLogger.ToLog( Format( 'Пароль прокси => "%s"', [ProxyPass] ) );

//    FConnectionString := INIFile.ReadString( 'Main Params', 'DBConnection', '');
    FKPSensorMap := TProviderMap.Create;
    FImeiHash := TDictionary<string,integer>.Create;

    imei := TStringList.Create;
    try
      INIFile.ReadSection('imei', imei);
      for s in imei do
      begin
        id := INIFile.ReadInteger('imei', s, 0);
        if id > 0 then
        begin
          if FImeiHash.ContainsKey(s) then
            FImeiHash.ExtractPair(s);

          FImeiHash.Add(s, id);
        end;
      end;

      FLogger.ToLog('imei mapping:');
      for s in FImeiHash.Keys do
        FLogger.ToLog(s + '=' + IntToStr(FImeiHash.Items[s]));

    finally
      imei.Free;
    end;

  except
    on Ex: Exception do
    begin
      FLogger.ErrorToLog( 'Ошибка чтения параметров из INI-файла:'#13#10 + Ex.Message );
      INIFile.Free();
      Exit;
    end;
  end;
  INIFile.Free();
  //
  FWhere := ServerUrl + CWDMRoute;
  FInfo := TStringList.Create();
  FInfo.LoadFromFile( QExtractFilePath( ParamStr( 0 ) ) + 'ILSLoader.cfg' );
  FLogger.ToLog( 'Содержимое управляющего файла:'#13#10 + FInfo.DelimitedText );
  FServer := TIdHTTP.Create( nil );
  FServer.ReadTimeout := Timeout * 1000;
  FServer.ProxyParams.ProxyServer   := ProxyAddr;
  FServer.ProxyParams.ProxyPort     := ProxyPort;
  FServer.ProxyParams.ProxyUsername := ProxyUser;
  FServer.ProxyParams.ProxyPassword := ProxyPass;
  FServer.HandleRedirects := True;
  FCoreThread := TThreadInfinite.Create( AllWork, nil, Self );
  // успех
  FLogger.ToLog( '============ запущен ============' );
  Started := True;
end;

procedure TILSLoader.ServiceStop(
  Sender: TService;
  var Stopped: Boolean
);
begin
  Stopped := True;
  FLogger.ToLog( '============ остановлен ============' );
  //
  FCoreThread.Free();
  FServer.Free();
  FInfo.Free();
  FImeiHash.Free;
  FKPSensorMap.Free;
  // успех
  FLogger.ToLog( '============ СТОП ============' );
  FLogger.Free();
end;

procedure TILSLoader.InternalAllWork();
var
  //!
  I: Integer;
  ini: TIniFile;
  t: Integer;
  d: string;
  DeviceTypes: TStringList;
  SensorMap: TStringList;
begin
  ini := TIniFile.Create( ChangeFileExt( ParamStr( 0 ), CIniDefExtDotted ) );
  try
    for I := 0 to FInfo.Count - 1 do
    begin
      try
        FProvID := FInfo.Names[I];
        FProvPath := QExtractFilePath( ParamStr( 0 ) ) + FInfo.Values[FProvID] + CPathDelim;
        FProvID := Format( '%.5d', [StrToInt( FProvID )] ); // всегда 5 цифр (заодно проверка, что это действительно цифры)


        Fpid := IntToStr(StrToInt(FProvID));

        FDBConnection := ini.ReadString('sql.' + Fpid, 'DBConnection', '');

        FKPAddress      := ini.ReadString('kafka.' + Fpid, 'address', 'https://2.ils-glonass.ru:8888');
//        FLogger.ToLog(FKPAddress);
        FKPCommand      := ini.ReadString('kafka.' + Fpid, 'command', '/GetTrack');
//        FLogger.ToLog(FKPCommand);
        FKPUser         := ini.ReadString('kafka.' + Fpid, 'user', '');
//        FLogger.ToLog(FKPUser);
        FKPPassword     := ini.ReadString('kafka.' + Fpid, 'password', '');
//        FLogger.ToLog(FKPPassword);
        FKPCount        := ini.ReadInteger('kafka.' + Fpid, 'count', 100);
//        FLogger.ToLog(IntToStr(FKPCount));
        FKPOffset       := StrToInt64Def(ini.ReadString('kafka.' + Fpid, 'offset', ''), -2);
//        FLogger.ToLog(IntToStr(FKPOffset));

        DeviceTypes := TStringList.Create;
        SensorMap := TStringList.Create;

        try
          FKPSensorMap.Clear;

          ini.ReadSections('mapping.' + Fpid, DeviceTypes);
          for t := 0 to DeviceTypes.Count - 1 do
          begin
            ini.ReadSectionValues(DeviceTypes[t], SensorMap);
            try
              d := Copy(DeviceTypes[t], Length('mapping.' + Fpid + '.') + 1, Length(DeviceTypes[t]) - Length('mapping.' + Fpid + '.'));
              FKPSensorMap.AddMap(StrToInt(fpid), StrToInt(d), SensorMap);
            except

            end;
          end;
        finally
          DeviceTypes.Free;
          SensorMap.Free;
        end;

        // получаем точки из кафки
        GetKafkaDataAndWriteInFileNew( CDeviceDataExt );

        // проверяем сервер
        if not CheckServerOk() then Continue;
        // получаем команды управления списком датчиков
        if not GetDataAndWriteInFileNew( 'dvc' ) then Continue;
        // получаем изменения накладных
        if not GetDataAndWriteInFileNew( CChangeBillExt ) then Continue;
        // получаем сообщения
        if not GetDataAndWriteInFileNew( CSrvMessagesExt ) then Continue;
        // получаем статусы
        if not GetDataAndWriteInFileNew( CExpStatusOfExt ) then Continue;
        // получаем точки
        if not GetDataAndWriteInFileNew( CDeviceDataExt ) then Continue;
        // отсылаем плановые точки
        SendDataNew( 'SetExpPnt', CExpPlanExt );
        // отсылаем сообщения
        SendDataNew( 'SetExpMsg', CExpMessagesExt );
        // отсылаем накладные
        SendDataNew( 'SetExpWay', CExpBillExt );
        // отсылаем "любые" файлы
        SendAnyFile();
        // принимаем "любые" файлы
        GetAnyFile();
      except
        on Ex: Exception do
        begin
          FLogger.ErrorToLog( 'Ошибка верхнего уровня при обработке строки №' + IntToStr( I + 1 ) + #13#10 + Ex.Message );
        end;
      end;
    end;
  finally
    ini.Free;
  end;
end;

function TILSLoader.SendCommand(
  const ACommand: string
): Boolean;
var
  //! код ошибки
  ErrorCode: Cardinal;
//------------------------------------------------------------------------------
begin
  Result := True;
  try
    FServer.Get( ACommand );
  except
    on Ex: Exception
    do begin
      Result := False;
      ErrorCode := GetLastError();
      FLogger.ErrorToLog( 'Ошибка SendCommand (' + ACommand + '):'#13#10 + Ex.Message );
      if ( ErrorCode = 10054 ) or ( ErrorCode = 10053 )
      then begin
        FServer.Disconnect();
        FServer.Connect();
      end;
    end;
  end;
end;

function TILSLoader.SendDataStream(
  const ARequest: string;
  const ADataStream: TIdMultiPartFormDataStream
): string;
var
  //! код ошибки
  ErrorCode: Cardinal;
//------------------------------------------------------------------------------
begin
  try
    FServer.Request.ContentType := 'multipart/form-data';
    FServer.HandleRedirects := True;
    Result := FServer.Post( ARequest, ADataStream );
  except
    on Ex: Exception
    do begin
      ErrorCode := GetLastError();
      FLogger.ErrorToLog( 'Ошибка SendDataStream (' + ARequest + '):'#13#10 + Ex.Message );
      if ( ErrorCode = 10054 ) or ( ErrorCode = 10053 )
      then begin
        FServer.Disconnect();
        FServer.Connect();
      end;
      Result := 'Ошибка SendDataStream: ' + Ex.Message;
    end;
  end;
  // очистка типа ???
  FServer.Request.ContentType := '';
end;

function TILSLoader.GetResponseForRequest(
  const ARequest: string;
  var RResponse: TMemoryStream
): Boolean;
var
  //! код ошибки
  ErrorCode: Cardinal;
//------------------------------------------------------------------------------
begin
  Result := True;
  RResponse.Size := 0;
  try
    FServer.Get( ARequest, RResponse );
  except
    on Ex: Exception
    do begin
      Result := False;
      ErrorCode := GetLastError();
      FLogger.ErrorToLog( 'Ошибка GetResponseForRequest (' + ARequest + '):'#13#10 + Ex.Message );
      if ( ErrorCode = 10054 ) or ( ErrorCode = 10053 )
      then begin
        FServer.Disconnect();
        FServer.Connect();
      end;
    end;
  end;
end;

function TILSLoader.GetResponse(
  const ARequest: string
): string;
var
  //! код ошибки
  ErrorCode: Cardinal;
//------------------------------------------------------------------------------
begin
  try
    Result := FServer.Get( ARequest );
  except
    on Ex: Exception
    do begin
      ErrorCode := GetLastError();
      FLogger.ErrorToLog( 'Ошибка GetResponse (' + ARequest + '):'#13#10 + Ex.Message );
      if ( ErrorCode = 10054 ) or ( ErrorCode = 10053 )
      then begin
        FServer.Disconnect();
        FServer.Connect();
      end;
      Result := 'Ошибка GetResponse: ' + Ex.Message;
    end;
  end;
end;

function TILSLoader.GetDataAndWriteInFileNew(
  const AExt: string
): Boolean;
var
  //! текст команды запросы длины
  CommandSize: string;
  //! текст команды запросы самого файла
  CommandFile: string;
  //! текст подтверждения
  CommandConfirm: string;
  //! имя сохраняемого файла
  FileName: string;
  //! текст ошибки открытия файла
  ErrorMes: string;
  //! сохраняемый файл
  StoredFile: TFileHandler;
  //! поток - ответ от сервера
  MemStream: TMemoryStream;
  //! размер ожидаемого файла
  GettedSize: Int64;
//------------------------------------------------------------------------------
begin
  Result := False;
  // команды
  CommandSize := FWhere + CUniInfo + AExt + FProvID;
  CommandFile := FWhere + CUniReceive + AExt + FProvID;
  CommandConfirm := FWhere + CUniConfirm + AExt + FProvID;
  // поток ответа
  MemStream := TMemoryStream.Create();
  try
    // запрос длины
    if not GetResponseForRequest( CommandSize, MemStream ) then Exit;
    if ( MemStream.Size <> 8 ) then
    begin // не приняли размер
      FLogger.ErrorToLog( 'GetDataAndWriteInFileNew (' + CommandSize + '):'#13#10'ошибка приёма длины' );
      Exit;
    end;
    MemStream.Position := 0;
    MemStream.Read( GettedSize, 8 );
    if ( GettedSize = 0 ) then Exit( True ); // нет файла для получения
    if ( GettedSize > $7fffffff ) then
    begin // большой размер
      FLogger.ErrorToLog( 'GetDataAndWriteInFileNew (' + CommandSize + '):'#13#10'слишком большой размер => ' + IntToStr( GettedSize ) );
      Exit;
    end;
    MemStream.Clear();
    // запрос файла
    if not GetResponseForRequest( CommandFile, MemStream ) then Exit;
    if ( MemStream.Size <> GettedSize ) then
    begin // неверный размер принятого файла
      FLogger.ErrorToLog( 'GetDataAndWriteInFileNew (' + CommandFile + '):'#13#10'несовпадающий размер принятого файла' );
      Exit;
    end;
    // сохраняем на диск
    FileName := FProvPath + CToClientSrvPrefix + CZeroNamePart + CExtDelim + AExt;
    if not TFileHandler.TryCreate( FileName, famWriteStrict, StoredFile, ErrorMes ) then
    begin
      FLogger.ErrorToLog( 'GetDataAndWriteInFileNew: не удалось открыть на запись файл "' + FileName + '":'#13#10 + ErrorMes );
      Exit;
    end;
    try
      StoredFile.ReadFromMem( MemStream.Memory, GettedSize );
    finally
      StoredFile.Free();
    end;
    // посылаем подтверждение приёма = удаление на сервере
    if not SendCommand( CommandConfirm ) then
      FLogger.ErrorToLog( 'GetDataAndWriteInFileNew: ошибка посылки подтверждения' );
    Result := True;
  finally
    MemStream.Free();
  end;
end;

//

function TILSLoader.GetKafkaDataAndWriteInFileNew(
  const AExt: string
): Boolean;

  function AddImageRecord(aSN: Integer; aDT: TDateTime; AConnectionString: string): Boolean;
  var
    qry: TADOQuery;
  begin
    Result := True;
    CoInitialize(nil);
    try
      qry := TADOQuery.Create(nil);
      try
        qry.ConnectionString := AConnectionString;
        qry.SQL.Text := 'INSERT INTO utbl_Pictures2(DeviceID, PictureDateTime) VALUES (:DeviceID, :PictureDateTime);';
        qry.Parameters.ParamByName('DeviceID').DataType := ftInteger;
        qry.Parameters.ParamByName('DeviceID').Value := aSN;
        qry.Parameters.ParamByName('PictureDateTime').DataType := ftDateTime;
        qry.Parameters.ParamByName('PictureDateTime').Value := aDT;
        qry.ExecSQL;
      finally
        qry.Free;
        CoUninitialize;
      end;
    except
      on E: Exception do
        FLogger.ErrorToLog(E.Message);
    end;
  end;

  function MakeImageFileName(aSN: Integer; aDT: TDateTime): string;
  var
    sn: string;
    week: string;
    date: string;
    time: string;
    year: string;
  begin
    sn := Format('%.6d', [aSN]);
    week := InttoStr(WeekOfTheYear(aDT));
    year := IntToStr(YearOf(aDT));
    date := FormatDateTime('yyyy-mm-dd', aDT);
    time := FormatDateTime('hh-nn-ss-zzz', aDT);
    Result :=
      sn + '\' + year + '\' + week  + '\' +
      sn + '_' + date + '_' + time;
  end;

  function SaveAsPnt(s: string): Int64;
  var
    json: TJsonObject;
    i: Integer;
    pp: TPntPoint;
    PntFileName: string;
    fm: Integer;
    fs: TFileStream;
    jfs: TFileStream;
    sn: string;
    DataRaw: AnsiString;
    ImagesDataFolder: string;
    ifn: string;
    ifm: Integer;
    isn: Integer;
    idt: TDateTime;
    Map: string;
  begin
    Result := -1;
    json := nil;

    PntFileName := FProvPath + CToClientSrvPrefix + CZeroNamePart + CExtDelim + AExt;// FPntFilePath + 'R000000.pnt';
    ForceDirectories(FProvPath);

    ImagesDataFolder := FProvPath + 'Images\';
    fm := fmOpenWrite + fmShareDenyNone;//fmShareExclusive;
    if not FileExists(PntFileName) then
      fm := fm + fmCreate;

    fs := TFileStream.Create(PntFileName, fm);
    fs.Seek(0, soFromEnd);
    try
      json := TJsonObject.Parse(s) as TJsonObject;


      for i := 0 to json.A['a'].Count -1 do begin
        if IsCameraMessage(json.A['a'].Items[i].ObjectValue) then
        begin
          sn := json.A['a'].O[i].S['i'];
          idt := IlsToDateTime(json.A['a'].O[i].S['dt']);

          if FImeiHash.ContainsKey(sn) then
            sn := Format('%.6d', [FImeiHash.Items[sn]]);

          isn := StrToInt64Def(sn, 999999);

          DataRaw := DecodeBase64(AnsiString(json.A['a'].O[i].O[JF2Str(jfSensors)].S[MakeSensorName(stCamera)]));
          if Length(DataRaw) > 0 then
          begin
            ifn :=
              ImagesDataFolder +
              MakeImageFileName(isn, idt + 3 / HoursPerDay)+
              '.jpeg';

            ForceDirectories(ExtractFilePath(ifn));

            ifm := fmOpenWrite + fmShareDenyNone;//fmShareExclusive;
            if not FileExists(ifn) then
              ifm := ifm + fmCreate;

            jfs := TFileStream.Create(ifn, ifm);
            try
              jfs.Write(DataRaw[1], Length(DataRaw));
            finally
              jfs.Free;
            end;
            AddImageRecord(isn, idt + 3 / HoursPerDay, FDBConnection);
          end else
            FLogger.ToLog('картинка для ' + sn + ' нулевого размера');
        end
        else
        begin
          try
            Map := '';
            if FKPSensorMap.ContainsKey(StrToInt(Fpid)) then
              Map := FKPSensorMap[StrToInt(Fpid)].ToJson;

            pp := TPntPoint.Create(TJsonObject(json.A['a'].O[i]), Map, FImeiHash);
          except
            on E: exception do
            begin
              FLogger.ToLog(IntToStr(i) + ': ' + json.A['a'].O[i].ToString);
              raise;
            end;
          end;
          fs.Write(pp, SizeOf(pp));
        end;
      end;
      Result := json.L['o'];
    finally
      json.Free;
      fs.Free;
    end;
  end;

var
  ini: TIniFile;

  procedure StoreOffset(AOffset: Int64);
  begin
    FKPOffset := AOffset;
    ini.WriteString('kafka.' + Fpid, 'offset', IntToStr(FKPOffset));
  end;

var
  HTTP: TIdHTTP;
  Offset: Int64;
  s: string;
begin
  Result := True;
  try
    if (FKPAddress = '') or (FKPCommand = '') or (FKPUser = '') then
      Exit;

    ini := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), '.ini'));
    HTTP := TIdHTTP.Create(nil);

    HTTP.ProxyParams.ProxyServer    := FServer.ProxyParams.ProxyServer;
    HTTP.ProxyParams.ProxyPort      := FServer.ProxyParams.ProxyPort;
    HTTP.ProxyParams.ProxyUsername  := FServer.ProxyParams.ProxyUsername;
    HTTP.ProxyParams.ProxyPassword  := FServer.ProxyParams.ProxyPassword;

    if Pos('https://', LowerCase(FKPAddress)) = 1 then
      HTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

    try
      s := HTTP.Get(FKPAddress + FKPCommand + '?l='+FKPUser+'&p='+FKPPassword+'&c='+IntToStr(FKPCount)+'&o='+IntToStr(FKPOffset));

      Offset := SaveAsPnt(s);
      if Offset >= 0 then
        StoreOffset(Offset);
    finally
      ini.Free;
      HTTP.Free;
    end;

  except
    on E: Exception do begin
      FLogger.ToLog('Ошибка сохранения точек из кафки: ' + E.Message);
    end;

  end;
end;


procedure TILSLoader.SendDataNew(
  const ACommand: string;
  const AFileExt: string
);
var
  //! размер файла
  SizeOfFile: Int64;
  //! поток
  MultiPartDataStream: TIdMultiPartFormDataStream;
  //! рабочий файл
  WorkFileName: string;
  //! рабочий файл - переименованный
  RenamedFileName: string;
  //! ответ сервера
  ResponseText: string;
//------------------------------------------------------------------------------
begin
  WorkFileName := FProvPath + CFromClientSrvPrefix + CZeroNamePart + CExtDelim + AFileExt;
  RenamedFileName := FProvPath + CNameSend + CExtDelim + AFileExt;
  if not FileExists( RenamedFileName ) then
  begin
    if FileExists( WorkFileName ) then
    begin
      if not TFileHandler.RenameFile( WorkFileName, RenamedFileName ) then
      begin
        FLogger.ErrorToLog( 'SendDataNew: не удалось переименовать файл для отправки "' + WorkFileName + '"' );
        Exit;
      end;
    end;
  end;
  if not FileExists( RenamedFileName ) then Exit; // нечего отправлять
  SizeOfFile := TFileHandler.GetFileSize( RenamedFileName );
  if ( SizeOfFile = -1 ) then
  begin
    FLogger.ErrorToLog( 'SendDataNew: ошибка определения размера файла "' + RenamedFileName + '"' );
    Exit;
  end;
  if ( SizeOfFile = 0 ) then
  begin
    FLogger.ToLog( 'SendDataNew: переименованный файл нулевой длины, удаляем "' + RenamedFileName + '"' );
    if not TFileHandler.DeleteFile( RenamedFileName ) then
      FLogger.ErrorToLog( 'SendDataNew: не удалось удалить файл нулевой длины "' + RenamedFileName + '"' );
    Exit;
  end;
  MultiPartDataStream := TIdMultiPartFormDataStream.Create();
  try
    // загрузка файла в TIdMultiPartFormDataStream
    MultiPartDataStream.AddFile( 'myfile', RenamedFileName, 'multipart/form-data' );
    MultiPartDataStream.AddFormField( 'EmptyField', '' ); // это вообще зачем ???
    ResponseText := SendDataStream( FWhere + ACommand + FProvID, MultiPartDataStream );
  finally
    MultiPartDataStream.Free();
  end;
  if ( ResponseText = 'OK' ) then
  begin
    // удаляем файл
    if not TFileHandler.DeleteFile( RenamedFileName ) then
      FLogger.ErrorToLog( 'SendDataNew: не удалось удалить файл после отправки "' + RenamedFileName + '"' );
  end
  else
    FLogger.ErrorToLog( 'SendDataNew: ответ сервера на передачу файла не OK "' + RenamedFileName + '"' );
end;

procedure TILSLoader.SendAnyFile();
var
  //! размер файла
  SizeOfFile: Int64;
  //! имя файла
  FileName: string;
  //! запрос
  ReqText: string;
  //! ответ сервера
  ResponseText: string;
  //! поток
  MultiPartDataStream: TIdMultiPartFormDataStream;
  //!
  SR: TSearchRec;
//------------------------------------------------------------------------------
begin
  // ищем 1 файл по маске
  if ( SysUtils.FindFirst(
    FProvPath + CFromClientSrvPrefix + '*' + CExpAnyExtDotted,
    faNormal,
    SR
  ) <> 0 ) then Exit;
  FileName := FProvPath + SR.Name;
  SysUtils.FindClose( SR );
  FLogger.ToLog( 'SendAnyFile: найден файл для отправки "' + FileName + '"' );
  SizeOfFile := TFileHandler.GetFileSize( FileName );
  if ( SizeOfFile = -1 ) then
  begin
    FLogger.ErrorToLog( 'SendAnyFile: ошибка определения размера файла "' + FileName + '"' );
    Exit;
  end;
  if ( SizeOfFile = 0 ) then
  begin
    FLogger.ToLog( 'SendAnyFile: найденный файл нулевой длины, удаляем "' + FileName + '"' );
    if not TFileHandler.DeleteFile( FileName ) then
      FLogger.ErrorToLog( 'SendAnyFile: не удалось удалить файл нулевой длины "' + FileName + '"' );
    Exit;
  end;
  MultiPartDataStream := TIdMultiPartFormDataStream.Create();
  try
    // загрузка файла в TIdMultiPartFormDataStream
    MultiPartDataStream.AddFile( 'myfile', FileName, 'multipart/form-data' );
    MultiPartDataStream.AddFormField( 'EmptyField', '' ); // это вообще зачем ???
    ReqText := string( EncodeURL( AnsiString( FWhere + CFileSend + FProvID + CFileNameSend + QExcludeFilePath( FileName ) ) ) );
    ResponseText := SendDataStream( ReqText, MultiPartDataStream );
  finally
    MultiPartDataStream.Free();
  end;
  if ( ResponseText = 'OK' ) then
  begin
    // удаляем файл
    if not TFileHandler.DeleteFile( FileName ) then
      FLogger.ErrorToLog( 'SendAnyFile: не удалось удалить файл после отправки "' + FileName + '"' );
  end
  else
    FLogger.ErrorToLog( 'SendAnyFile: ответ сервера на передачу файла не OK "' + FileName + '"' );
end;

procedure TILSLoader.GetAnyFile();
var
  //! размер ожидаемого файла
  GettedSize: Int64;
  //! длина имени
  NameSize: Integer;
  //! поток - ответ от сервера
  MemStream: TMemoryStream;
  //! сохраняемый файл
  StoredFile: TFileHandler;
  //! текст ошибки открытия файла
  ErrorMes: string;
  //! текст команды запроса имени и длины файла
  CommandNameSize: string;
  //! текст команды запроса самого файла
  CommandReceive: string;
  //! текст подтверждения
  CommandConfirm: string;
  //! имя файла
  FileName: string;
  //! переданное имя файла
  FileNameAnsi: array of AnsiChar;
  //
  FileNameRef: PAnsiChar;
//------------------------------------------------------------------------------
begin
  // команды
  CommandNameSize := FWhere + CFileGetInfo + FProvID;
  CommandReceive := FWhere + CFileReceive + FProvID;
  CommandConfirm := FWhere + CFileConfirm + FProvID;
  // поток ответа
  MemStream := TMemoryStream.Create();
  try
    // 1. получаем длину и имя
    if not GetResponseForRequest( CommandNameSize, MemStream ) then Exit;
    if ( MemStream.Size < 8 ) then
    begin // не приняли размер
      FLogger.ErrorToLog( 'GetAnyFile (' + CommandNameSize + '):'#13#10'ошибка приёма длины' );
      Exit;
    end;
    MemStream.Position := 0;
    MemStream.Read( GettedSize, 8 );
    if ( GettedSize = -1 ) then Exit; // нет файла для получения
    if ( GettedSize > $7fffffff ) then
    begin // большой размер
      FLogger.ErrorToLog( 'GetAnyFile (' + CommandNameSize + '):'#13#10'слишком большой размер => ' + IntToStr( GettedSize ) );
      Exit;
    end;
    // выделяем имя
    //*** знаю, что полная шиза, но я не умею работать с нетипизированными типами и не хочу уметь ***
    NameSize := Integer( MemStream.Size ) - 8;
    SetLength( FileNameAnsi, NameSize );
    FileNameRef := @FileNameAnsi[0];
    MemStream.Read( FileNameRef^, NameSize );
    FileName := string( AnsiString( FileNameRef ) );
    CommandReceive := string( EncodeURL( AnsiString( CommandReceive + CFileNameGet + FileName ) ) );
    CommandConfirm := string( EncodeURL( AnsiString( CommandConfirm + CFileNameGet + FileName ) ) );
    // 2. получаем файл
    MemStream.Clear(); // очистка потока
    if not GetResponseForRequest( CommandReceive, MemStream ) then Exit;
    if ( MemStream.Size <> GettedSize ) then
    begin // неверный размер принятого файла
      FLogger.ErrorToLog( 'GetAnyFile (' + CommandReceive + '):'#13#10'несовпадающий размер принятого файла' );
      Exit;
    end;
    // сохраняем на диск
    FileName := FProvPath + FileName;
    if not TFileHandler.TryCreate( FileName, famWriteStrict, StoredFile, ErrorMes ) then
    begin
      FLogger.ErrorToLog( 'GetAnyFile: не удалось открыть на запись файл "' + FileName + '":'#13#10 + ErrorMes );
      Exit;
    end;
    try
      StoredFile.ReadFromMem( MemStream.Memory, GettedSize );
    finally
      StoredFile.Free();
    end;
    // 3. посылаем подтверждение приёма
    SendCommand( CommandConfirm );
  finally
    MemStream.Free();
  end;
end;

function TILSLoader.CheckServerOk(): Boolean;
var
  //! ответ сервера
  ResponseText: string;
//------------------------------------------------------------------------------
begin
  Result := True;
  ResponseText := GetResponse( FWhere + CSrvTest );
  if ( ResponseText <> 'OK' ) then
  begin
    Result := False;
    FLogger.ErrorToLog( 'Неверный ответ на приветствие от веб-сервера' );
  end;
end;

{ TDeviceMap }

procedure TDeviceMap.Clear;
var
  k: Integer;
begin
  for k in Keys do
    Items[k].Free;

  inherited Clear;
end;

destructor TDeviceMap.Destroy;
begin
  Clear;

  inherited;
end;

function TDeviceMap.ToJson: string;
var
  i: Integer;
  c: Integer;
  k: Integer;
  j: TJsonObject;
begin
  j := TJsonObject.Create;

  c:= 0;
  for k in Keys do
    for i := 0 to Items[k].Count - 1 do
    begin
      Inc(c);
      j.O[IntToStr(k)].S[Items[k].Names[i]] := Items[k].Values[Items[k].Names[i]];
    end;

  if c > 0 then
    Result := j.ToJSON
  else
    Result := '';

  j.Free;
end;

{ TProviderMap }

procedure TProviderMap.AddMap(AProviderID, ADeviceType: Integer;
  ASensorMap: TStringList);
begin
  if not ContainsKey(AProviderID) then
    Add(AProviderID, TDeviceMap.Create);
  if not Items[AProviderID].ContainsKey(ADeviceType) then
    Items[AProviderID].Add(ADeviceType, TStringList.Create);
  Items[AProviderID].Items[ADeviceType].Assign(ASensorMap);
end;

procedure TProviderMap.Clear;
var
  k: Integer;
begin
  for k in Keys do
    Items[k].Free;

  inherited Clear;
end;

destructor TProviderMap.Destroy;
begin
  Clear;

  inherited;
end;

end.

