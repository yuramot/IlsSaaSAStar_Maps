//------------------------------------------------------------------------------
//! @author Polyakova M.V. Copyright (C) 2013 ILS LLC. All Rights Reserved.
//!
{ ! @file

  @brief
  Функции для работы с реестром

  @details
  В данном модуле реализованы следующие функции:
  - считывание настроек ConnectionDBParams из реестра
  - запись настроек в реестр
}

//------------------------------------------------------------------------------

unit MCLF_RWSR_SettingsRegistry;

interface

//------------------------------------------------------------------------------
uses

  Windows, Messages, SysUtils, Classes, Registry, Vcl.Dialogs;
//------------------------------------------------------------------------------

type

  //! Запись с параметрами соединения с БД
  {!  Содержит строку подключения к БД, таймаут
  и ID последнего пользователя на данном компьюторе }

  TConnectionDBParams = record

    //! Таймаут подключения к БД
    m_iTimeout: Smallint;

    //! Строка соединения с БД
    m_sConnectionString: string[255];

    //! ID последнего пользователя
    m_iLastUserID: Integer;

  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------- class TConnectionParamsInRegistry --------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! Оператор настроек
{ !  Класс функций для работы с реестром:
  считывание, запись настроек }
//------------------------------------------------------------------------------

  TConnectionParamsInRegistry = class( TObject )

  private

     m_sMainRegistryKey : string;
     m_sNameRegistry : string;

  public

    //! Конструктор по умолчанию
    constructor Create (
              const sMainRegistryKey : string = '\SOFTWARE\ILSystems\ILSMonitoring';
              const sNameRegistry : string = 'Params' );

    //! Деструктор класса, отменяем виртуальный метод уничтожения TObject
    destructor Destroy; override;

    //! Проверяем, запущена ли программа от имени администратора
    function ProcessIsElevated: Boolean;

    //! Считывание настроек из реестра
    { !
      @param out
      rcConnectionParams - настройки из реестра
      @return
      True при успешном выполнении }
    function ReadSettingsFromRegistry ( var rcConnectionParams : TConnectionDBParams ): Boolean;

    //! Запись настроек в реестр
    { ! @param in
      rcConnectionParams - настройки
      @return
      True при успешном выполнении }
    function WriteSettingsToRegistry ( var rcConnectionParams : TConnectionDBParams; const bNeedCreateKey : Boolean = False ): Boolean;

    //! Проверка установлена ли служба
    {
      @return
      True если установлена
    }
    function KeyExists(   ): Boolean;

    //! Проверка корректности настроек
    function AreSettingsCorrect ( const rcConnectionParams : TConnectionDBParams ): Boolean;

    //!Шифровка строки
    { @param  in
      sEncrypted Строка, подлежащая шифровке
      @return
      Зашифрованная строка
      @author
      Перепёлкин
    }
    function EncryptString( AString: AnsiString ): AnsiString;

    //!Расшифровка строки (для чтения из реестра), зашифрованной с помощью утилиты GetDBConnection (ф-ция EncryptString)
    { @param  in
      sEncrypted Строка, подлежащая расшифровке
      @return
      Расшифрованная строка
      @author
      Максименко
    }
    function DecryptString( sEncrypted: AnsiString ): AnsiString;

    //! Реализация записи в реестр
    function WriteToRegistry( const sRootKey: HKEY; var iParams : Integer; const bNeedCreateKey : Boolean = False ): Boolean;

  end;

const

  //! сдвиг в таблице Ansi(используется в функциях шифровки и расшифровки строк)
  bEncryptionShift: Byte = 100;

//var

  //! признак того, что предложение перезапустить программу под админом уже показывалось
  // (чтобы сообщение не показывалось при каждом обращении к реестру)
  // bAdminMessageShown: Boolean = False ;

var

  //! Если false, то данные в реестре незашифрованы
  bRegistryCrypted: Boolean;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
// Конструктор класса
//------------------------------------------------------------------------------

constructor TConnectionParamsInRegistry.Create(
              const sMainRegistryKey : string = '\SOFTWARE\ILSystems\ILSMonitoring';
              const sNameRegistry : string = 'Params' );
begin

  inherited Create();

  m_sMainRegistryKey := sMainRegistryKey;
  m_sNameRegistry := sNameRegistry;

end;

//------------------------------------------------------------------------------
// Проверка корректности настроек
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.AreSettingsCorrect( const rcConnectionParams : TConnectionDBParams ): Boolean;

//------------------------------------------------------------------------------
var

  // Признак ошибки
  bErr: Boolean;
//------------------------------------------------------------------------------

begin

  bErr := False;

  // Проверяем заполненность параметров
  if ( ( rcConnectionParams.m_sConnectionString = '' ) or ( rcConnectionParams.m_iTimeOut = 0 ) ) then
  begin

    bErr := True;

  end;

  Result := not bErr;

end;

//------------------------------------------------------------------------------
// Функция возвращает True если пользователь входит в группу
//------------------------------------------------------------------------------

function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID; var
  IsMember: BOOL): BOOL; stdcall; external advapi32;

//------------------------------------------------------------------------------
// Проверяем, запущена ли программа от имени администратора
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.ProcessIsElevated: Boolean;
//*** Максименко
const

  // ID группы администраторов
  AdminGroup = $00000220;

  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

var

  // указатель на токен
  TokenHandle: THandle;

  // буфер, в который функция GetTokenInformation пишет информацию о процессе
  bufferTokenElevation: TOKEN_ELEVATION;

  // размер буфера bufferTokenElevation
  iBufferLength: Cardinal;

  //указатель на структуру SID_IDENTIFIER_AUTHORITY
  pIdentifierAuthority: TSIDIdentifierAuthority;

  // указатель на идентификатор безопасности
  pSid: Windows.PSID;

  // результат проверки внешней функцией CheckTokenMembership
  bIsMember: BOOL;

begin

  TokenHandle := 0;

  if CheckWin32Version( 6, 0 ) then    //Vista или выше
  begin

    if OpenProcessToken( GetCurrentProcess, TOKEN_QUERY, TokenHandle ) then
    begin

      if GetTokenInformation( TokenHandle, TokenElevation, @bufferTokenElevation, SizeOf( bufferTokenElevation ), iBufferLength ) then
      begin

        if ( bufferTokenElevation.TokenIsElevated <> 0 ) then
        begin

          Result := True;

        end
        else
        begin

          Result := False;

        end;
      end
      else
      begin

        Result := false;

      end;
    end
    else
    begin

      Result := False;

    end;

  end
  else
  begin

    pIdentifierAuthority := SECURITY_NT_AUTHORITY;

    try

      if AllocateAndInitializeSid( pIdentifierAuthority, 2, $00000020, AdminGroup, 0, 0, 0, 0, 0, 0, pSid ) then
      begin

        if not CheckTokenMembership( 0, pSid, bIsMember ) then

          Result := False

        else

          Result := bIsMember;

      end;

    finally

      FreeSid(pSid);

    end;

  end;

end;


//------------------------------------------------------------------------------
// Считывание настроек из реестра
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.ReadSettingsFromRegistry ( var rcConnectionParams : TConnectionDBParams ): Boolean;

//------------------------------------------------------------------------------
var

  // Переменная для работы с реестром
  Reg: TRegistry;

  // Признак ошибки
  bErr: Boolean;

  // Переменная, в которую пишем тип параметра
  regdatatypeParam: TRegDataType;
//------------------------------------------------------------------------------

begin

  bErr := False;

  // Читаем параметры из реестра
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  if ( Reg.KeyExists( m_sMainRegistryKey ) = True ) then
  begin

    // Читаем параметры
    try

      Reg.OpenKeyReadOnly ( m_sMainRegistryKey );
      Reg.ReadBinaryData( m_sNameRegistry, rcConnectionParams, SizeOf( rcConnectionParams ) );
      (*if
         //( Reg.ReadBinaryData( m_sNameRegistry, rcConnectionParams, SizeOf( rcConnectionParams ) ) = 0 ) and
         ( ProcessIsElevated () = False ) and ( bAdminMessageShown = False ) then
      begin

        ShowMessage( 'Невозможно прочитать данные из реестра. Попробуйте перезапустить программу от имени администратора!' );
        bAdminMessageShown := True;

      end; *)

      //если зашифровано
      if Pos( 'Password', rcConnectionParams.m_sConnectionString ) = 0 then
      begin

        rcConnectionParams.m_sConnectionString := DecryptString( rcConnectionParams.m_sConnectionString );
        bRegistryCrypted := True;

      end
      else
      begin

        // Если данные в реестре незашифрованы, то не пускаем пользователя и рекомендуем обновить настройки в GetDBConnection
        bErr := False;  //bErr := True;
        bRegistryCrypted := False;

        {mAuthorization.cxTextEditPassword.Enabled := False;
        fmAuthorization.cxButtonOk.Enabled := False;
        fmAuthorization.cxLabelErrMess.Caption := 'Перенастройте соединение с помощью утилиты GetDBConnection!';}

      end;

      Reg.CloseKey;

    except

      on E: Exception do
      begin

        bErr := True;

        (*
        исключений при отсутствии доступа не наблюдается, поэтому убрано
        if {( ProcessIsElevated ( ) = False ) and} ( bAdminMessageShown = False ) then
        begin

          ShowMessage( 'Невозможно прочитать данные из реестра. Попробуйте перезапустить программу от имени администратора!!' );
          bAdminMessageShown := True;

        end;    *)

      end;

    end;

  end
  else
  begin

    bErr := True;

  end;

  Reg.Free();

  Result := not bErr;

end;

//------------------------------------------------------------------------------
// Проверка установлена ли служба
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.KeyExists( ): Boolean;

//------------------------------------------------------------------------------
var

  // Переменная для работы с реестром
  ctRegistry: TRegistry;

  // Признак ошибки
  bErr: Boolean;

//------------------------------------------------------------------------------

begin

  bErr := False;

  ctRegistry := TRegistry.Create;
  ctRegistry.RootKey := HKEY_LOCAL_MACHINE;

  if ( ctRegistry.KeyExists ( m_sMainRegistryKey ) = False ) then
  begin

    bErr := True;

  end;

  ctRegistry.Free;

  Result := not bErr;

end;

//------------------------------------------------------------------------------
// Реализация записи параметров в реестр
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.WriteSettingsToRegistry ( var rcConnectionParams : TConnectionDBParams; const bNeedCreateKey : Boolean = False ): Boolean;

//------------------------------------------------------------------------------
var

  // Переменная для работы с реестром
  ctRegistry: TRegistry;

  // Признак ошибки
  bErr: Boolean;
//------------------------------------------------------------------------------

begin

  bErr := False;

  ctRegistry := TRegistry.Create;
  ctRegistry.RootKey := HKEY_LOCAL_MACHINE;

  if (( ctRegistry.KeyExists( m_sMainRegistryKey ) = False ) and ( bNeedCreateKey = True )) then
  begin

    ctRegistry.CreateKey( m_sMainRegistryKey );

  end;

  if ( ctRegistry.KeyExists( m_sMainRegistryKey ) = True ) then
  begin

    try

      ctRegistry.OpenKey( m_sMainRegistryKey, True );
      rcConnectionParams.m_sConnectionString := EncryptString(rcConnectionParams.m_sConnectionString);
      ctRegistry.WriteBinaryData( m_sNameRegistry, rcConnectionParams, SizeOf( rcConnectionParams ) );
      ctRegistry.CloseKey;

    except

      on E: Exception do
      begin

        bErr := True;

      end;

    end;

  end;

  ctRegistry.Free();

  Result := not bErr;

end;

//------------------------------------------------------------------------------
// Шифровка строки (для записи в реестр)
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.EncryptString( AString: AnsiString ): AnsiString;
var

  // Счетчик номера символа в строке
  i: Integer;

  // номер в таблице ANSI расшифрованного байта
  EncryptedCharNumber: Byte;

  // строка для записи результата расшифровки
  EncryptedString: string;
begin
  EncryptedString := '';
  for i := 1 to Length(AString) do
  begin
    EncryptedCharNumber := ( Ord(AString[i]) + bEncryptionShift ) mod 256;
    EncryptedString := EncryptedString + AnsiChar(EncryptedCharNumber);
  end;

  Result := EncryptedString;

end;

//------------------------------------------------------------------------------
// Расшифровка строки (для чтения из реестра), зашифрованной с помощью утилиты
// GetDBConnection (ф-ция EncryptString)
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.DecryptString( sEncrypted: AnsiString ): AnsiString;
var

  // Счетчик номера символа в строке
  i: Integer;

  // номер в таблице ANSI расшифрованного байта
  bDecryptedChar: Byte;

  // строка для записи результата расшифровки
  sDecrypted: string;
begin

  bDecryptedChar := 0;
  sDecrypted := '';

  for i := 1 to Length( sEncrypted ) do
  begin

    {в зашифрованной строке все символы "сдвинуты" по таблице ANSI на bEncryptionShift позиций,
    для расшифровки "сдвигаем" обратно}
    bDecryptedChar := ( Ord( sEncrypted[ i ] ) + ( 256 - bEncryptionShift ) ) mod 256;
    sDecrypted := sDecrypted + AnsiChar( bDecryptedChar );

  end;

  Result := sDecrypted;

end;

//------------------------------------------------------------------------------
// Реализация записи в реестр
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.WriteToRegistry ( const sRootKey: HKEY; var iParams : Integer; const bNeedCreateKey : Boolean = False ): Boolean;

//------------------------------------------------------------------------------
var

  // Переменная для работы с реестром
  ctRegistry: TRegistry;

  // Признак ошибки
  bErr: Boolean;
//------------------------------------------------------------------------------

begin

  bErr := False;

  ctRegistry := TRegistry.Create;
  ctRegistry.RootKey := sRootKey;

  if (( ctRegistry.KeyExists( m_sMainRegistryKey ) = False ) and ( bNeedCreateKey = True )) then
  begin

    ctRegistry.CreateKey( m_sMainRegistryKey );

  end;

  if ( ctRegistry.KeyExists( m_sMainRegistryKey ) = True ) then
  begin

    try

      ctRegistry.OpenKey( m_sMainRegistryKey, True );
      ctRegistry.WriteInteger( m_sNameRegistry, iParams );
      ctRegistry.CloseKey;

    except

      on E: Exception do
      begin

        bErr := True;

      end;

    end;

  end;

  ctRegistry.Free();

  Result := not bErr;

end;


//------------------------------------------------------------------------------
// Деструктор класса
//------------------------------------------------------------------------------

destructor TConnectionParamsInRegistry.Destroy;
begin

  inherited Destroy;

end;

end.

