unit SenderMain;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, SvcMgr, Classes, Windows, Messages, Graphics, Controls, Dialogs,
  ExtCtrls, SyncObjs, DBClient, ActiveX, IniFiles, StrUtils,
  blcksock,
  C_FileNames, T_Common,
  E_Utils, E_UtilsFiles, E_UtilsStr, E_Logger, E_Files, E_Threads;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс сервиса
//------------------------------------------------------------------------------
  TILS_CommonSender = class(TService)
    tCheck: TTimer;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure tCheckTimer(Sender: TObject);
  private
    //! ID сервиса
    FID: Integer;
    //! корневая рабочая папка (с завершающей наклонной)
    FWorkFolder: string;
    //! адрес сервера
    FServerAddr: string;
    //! порт сервера
    FServerPort: Integer;
    //! адрес прокси
    FProxyAddr: string;
    //! порт прокси
    FProxyPort: Integer;
    //! лог
    FLogger: TLogger;
    //! сокет для связи
    FSoket: TTCPBlockSocket;
    //! процедура поиска файлов
    procedure DoSearch();
    //! процедура отправки файлов
    procedure DoSend(
      const AFileName: string
    );
  public
    function GetServiceController: TServiceController; override;
  end;

//------------------------------------------------------------------------------
var

//------------------------------------------------------------------------------
//! переменная главного окна
//------------------------------------------------------------------------------
  ILS_CommonSender: TILS_CommonSender;

//------------------------------------------------------------------------------
implementation

{$R *.DFM}

//------------------------------------------------------------------------------
// TILS_ServFileInterflow
//------------------------------------------------------------------------------

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ILS_CommonSender.Controller(CtrlCode);
end;

function TILS_CommonSender.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TILS_CommonSender.ServiceStart(
  Sender: TService;
  var Started: Boolean
);
var
  //! ini-файл
  INIFile: TIniFile;
//------------------------------------------------------------------------------
begin
{$IFDEF DEBUG}
  Windows.Sleep( 10000 );
{$ENDIF}
  Started := False;
  // логгер
  FLogger := TLogger.Create( ParamStr( 0 ), 5 * 1024 * 1024, 3 );
  FLogger.ToLog( '============ СЛУЖБА ОТПРАВКИ ФАЙЛОВ ============' );
  FLogger.ToLog( ParamStr( 0 ) );
  FLogger.ToLog( '=== запускается ===' );
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
    FID := INIFile.ReadInteger( 'Main Params', 'ID', 0 );
    FLogger.ToLog( Format( 'Идентификатор клиента => "%d"', [FID] ) );
    FWorkFolder := IncludeLastDelimiter( INIFile.ReadString( 'Main Params', 'WorkFolder', '' ), CPathDelim );
    FLogger.ToLog( Format( 'Папка обработки данных => "%s"', [FWorkFolder] ) );
    FServerAddr := INIFile.ReadString( 'Main Params', 'ServerAddr', '' );
    FLogger.ToLog( Format( 'Адрес сервера => "%s"', [FServerAddr] ) );
    FServerPort := INIFile.ReadInteger( 'Main Params', 'ServerPort', 0 );
    FLogger.ToLog( Format( 'Порт сервера => "%d"', [FServerPort] ) );
    FProxyAddr := INIFile.ReadString( 'Main Params', 'ProxyAddr', '' );
    FLogger.ToLog( Format( 'Адрес сервера => "%s"', [FProxyAddr] ) );
    FProxyPort := INIFile.ReadInteger( 'Main Params', 'ProxyPort', 0 );
    FLogger.ToLog( Format( 'Порт сервера => "%d"', [FProxyPort] ) );
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
  FSoket := TTCPBlockSocket.Create();
  //
  FLogger.ToLog( '============ запущена ============' );
  //
  Started := True;
end;

procedure TILS_CommonSender.ServiceStop(
  Sender: TService;
  var Stopped: Boolean
);
begin
  Stopped := True;
  FLogger.ToLog( '=== останавливается ===' );
  FSoket.Free();
  FLogger.ToLog( '============ остановлена ============' );
  FLogger.Free();
end;

procedure TILS_CommonSender.tCheckTimer(
  Sender: TObject
);
begin
  tCheck.Enabled := False;
  try
    DoSearch();
  finally
    tCheck.Enabled := True;
  end;
end;

procedure TILS_CommonSender.DoSearch();
var
  //!
  SR: TSearchRec;
//------------------------------------------------------------------------------
begin
  try
    if ( SysUtils.FindFirst( FWorkFolder + '*.*', faNormal, SR ) <> 0 ) then Exit;
    try
      repeat
        // хм... непонятно, как такое может быть, а вот бывает
        if ( SR.Name = '' ) then Continue;
        // передача
        DoSend( FWorkFolder + SR.Name );
      until ( SysUtils.FindNext( SR ) <> 0 );
    finally
      SysUtils.FindClose( SR );
    end;
  except
    on Ex: Exception do
    begin
      FLogger.ErrorToLog( 'Ошибка в процедуре поиска:'#13#10 + Ex.Message );
    end;
  end;
end;

procedure TILS_CommonSender.DoSend(
  const AFileName: string
);
var
  //! размер файла (в байтах)
  SendFileSize: Int64;
  //! длина имени файла без пути (в символах)
  PureFileNameLen: Integer;
  //! имя файла без пути
  PureFileName: string;
  //! байт ответа
  AnswerByte: Byte;
  //! класс файла
  FileH: TFileHandler;
  //! память файла
  FileMem: Pointer;
//------------------------------------------------------------------------------
begin
  // контроллер
  ILS_CommonSender.ServiceThread.ProcessRequests( False );
  if Terminated then Exit;
  //
  try
    GetMem( FileMem, 8192 );
    try
      SendFileSize := TFileHandler.GetFileSize( AFileName );
      if ( SendFileSize = -1 ) then
      begin
        FLogger.ErrorToLog( 'Ошибка определения размера файла "' + AFileName + '"' );
        Exit;
      end;
      //
      PureFileName := QExcludeFilePath( AFileName );
      PureFileNameLen := Length( PureFileName );
      //
      FSoket.Connect( FServerAddr, IntToStr( FServerPort ) );
      try
        // посылаем наш ID
        FSoket.SendBuffer( @FID, 4 );
        // ждём ответа на ID
//while ( FSoket.WaitingData > 0 ) do
        FSoket.RecvBuffer( @AnswerByte, 1 );
        if ( AnswerByte <> 6 ) then
        begin
          FLogger.ErrorToLog( 'Нет подтверждения на отправку ID для файла "' + AFileName + '"' );
          Exit;
        end;
        // посылаем имя
        FSoket.SendBuffer( @PureFileNameLen, 4 );
        FSoket.SendBuffer( @(PureFileName[1]), PureFileNameLen shl 1 );
        // ждём ответа на имя
//while ( FSoket.WaitingData > 0 ) do
        FSoket.RecvBuffer( @AnswerByte, 1 );
        if ( AnswerByte <> 6 ) then
        begin
          FLogger.ErrorToLog( 'Нет подтверждения на отправку размера файла "' + AFileName + '"' );
          Exit;
        end;
        // посылаем файл
        FileH := TFileHandler.Create( AFileName, famReadStrict );
        FSoket.SendBuffer( @SendFileSize, 8 );
        try
          while ( SendFileSize > 8192 )  do
          begin
            FileH.WriteToMem( FileMem, 8192 );
            FSoket.SendBuffer( FileMem, 8192);
            SendFileSize := SendFileSize - 8192;
          end;
          if ( SendFileSize <> 0 ) then
          begin
            FileH.WriteToMem( FileMem, Integer( SendFileSize ) );
            FSoket.SendBuffer( FileMem, Integer( SendFileSize ) );
          end;
        finally
          FileH.Free();
        end;
        // ждём ответа на файл
//while ( FSoket.WaitingData > 0 ) do
        FSoket.RecvBuffer( @AnswerByte, 1 );
        if ( AnswerByte <> 6 ) then
        begin
          FLogger.ErrorToLog( 'Нет подтверждения на отправку файла "' + AFileName + '"' );
          Exit;
        end;
        // удаляем файл
        if not TFileHandler.DeleteFile( AFileName ) then
        begin
          FLogger.ErrorToLog( 'Ошибка удаления файла "' + AFileName + '" после отправки -> дубликация файла' );
        end;
      finally
        FSoket.CloseSocket();
      end;
    finally
      FreeMem( FileMem );
    end;
  except
    on Ex: Exception do
    begin
      FLogger.ErrorToLog( 'Ошибка в процедуре отправки:'#13#10 + Ex.Message );
    end;
  end;
end;

//------------------------------------------------------------------------------
initialization
  IsMultiThread := True;
  SetMinimumBlockAlignment( mba16Byte );

end.

