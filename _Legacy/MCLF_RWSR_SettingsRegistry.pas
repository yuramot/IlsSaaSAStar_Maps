//------------------------------------------------------------------------------
//! @author Polyakova M.V. Copyright (C) 2013 ILS LLC. All Rights Reserved.
//!
{ ! @file

  @brief
  ������� ��� ������ � ��������

  @details
  � ������ ������ ����������� ��������� �������:
  - ���������� �������� ConnectionDBParams �� �������
  - ������ �������� � ������
}

//------------------------------------------------------------------------------

unit MCLF_RWSR_SettingsRegistry;

interface

//------------------------------------------------------------------------------
uses

  Windows, Messages, SysUtils, Classes, Registry, Vcl.Dialogs;
//------------------------------------------------------------------------------

type

  //! ������ � ����������� ���������� � ��
  {!  �������� ������ ����������� � ��, �������
  � ID ���������� ������������ �� ������ ���������� }

  TConnectionDBParams = record

    //! ������� ����������� � ��
    m_iTimeout: Smallint;

    //! ������ ���������� � ��
    m_sConnectionString: string[255];

    //! ID ���������� ������������
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
//! �������� ��������
{ !  ����� ������� ��� ������ � ��������:
  ����������, ������ �������� }
//------------------------------------------------------------------------------

  TConnectionParamsInRegistry = class( TObject )

  private

     m_sMainRegistryKey : string;
     m_sNameRegistry : string;

  public

    //! ����������� �� ���������
    constructor Create (
              const sMainRegistryKey : string = '\SOFTWARE\ILSystems\ILSMonitoring';
              const sNameRegistry : string = 'Params' );

    //! ���������� ������, �������� ����������� ����� ����������� TObject
    destructor Destroy; override;

    //! ���������, �������� �� ��������� �� ����� ��������������
    function ProcessIsElevated: Boolean;

    //! ���������� �������� �� �������
    { !
      @param out
      rcConnectionParams - ��������� �� �������
      @return
      True ��� �������� ���������� }
    function ReadSettingsFromRegistry ( var rcConnectionParams : TConnectionDBParams ): Boolean;

    //! ������ �������� � ������
    { ! @param in
      rcConnectionParams - ���������
      @return
      True ��� �������� ���������� }
    function WriteSettingsToRegistry ( var rcConnectionParams : TConnectionDBParams; const bNeedCreateKey : Boolean = False ): Boolean;

    //! �������� ����������� �� ������
    {
      @return
      True ���� �����������
    }
    function KeyExists(   ): Boolean;

    //! �������� ������������ ��������
    function AreSettingsCorrect ( const rcConnectionParams : TConnectionDBParams ): Boolean;

    //!�������� ������
    { @param  in
      sEncrypted ������, ���������� ��������
      @return
      ������������� ������
      @author
      ���������
    }
    function EncryptString( AString: AnsiString ): AnsiString;

    //!����������� ������ (��� ������ �� �������), ������������� � ������� ������� GetDBConnection (�-��� EncryptString)
    { @param  in
      sEncrypted ������, ���������� �����������
      @return
      �������������� ������
      @author
      ����������
    }
    function DecryptString( sEncrypted: AnsiString ): AnsiString;

    //! ���������� ������ � ������
    function WriteToRegistry( const sRootKey: HKEY; var iParams : Integer; const bNeedCreateKey : Boolean = False ): Boolean;

  end;

const

  //! ����� � ������� Ansi(������������ � �������� �������� � ����������� �����)
  bEncryptionShift: Byte = 100;

//var

  //! ������� ����, ��� ����������� ������������� ��������� ��� ������� ��� ������������
  // (����� ��������� �� ������������ ��� ������ ��������� � �������)
  // bAdminMessageShown: Boolean = False ;

var

  //! ���� false, �� ������ � ������� �������������
  bRegistryCrypted: Boolean;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
// ����������� ������
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
// �������� ������������ ��������
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.AreSettingsCorrect( const rcConnectionParams : TConnectionDBParams ): Boolean;

//------------------------------------------------------------------------------
var

  // ������� ������
  bErr: Boolean;
//------------------------------------------------------------------------------

begin

  bErr := False;

  // ��������� ������������� ����������
  if ( ( rcConnectionParams.m_sConnectionString = '' ) or ( rcConnectionParams.m_iTimeOut = 0 ) ) then
  begin

    bErr := True;

  end;

  Result := not bErr;

end;

//------------------------------------------------------------------------------
// ������� ���������� True ���� ������������ ������ � ������
//------------------------------------------------------------------------------

function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID; var
  IsMember: BOOL): BOOL; stdcall; external advapi32;

//------------------------------------------------------------------------------
// ���������, �������� �� ��������� �� ����� ��������������
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.ProcessIsElevated: Boolean;
//*** ����������
const

  // ID ������ ���������������
  AdminGroup = $00000220;

  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

var

  // ��������� �� �����
  TokenHandle: THandle;

  // �����, � ������� ������� GetTokenInformation ����� ���������� � ��������
  bufferTokenElevation: TOKEN_ELEVATION;

  // ������ ������ bufferTokenElevation
  iBufferLength: Cardinal;

  //��������� �� ��������� SID_IDENTIFIER_AUTHORITY
  pIdentifierAuthority: TSIDIdentifierAuthority;

  // ��������� �� ������������� ������������
  pSid: Windows.PSID;

  // ��������� �������� ������� �������� CheckTokenMembership
  bIsMember: BOOL;

begin

  TokenHandle := 0;

  if CheckWin32Version( 6, 0 ) then    //Vista ��� ����
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
// ���������� �������� �� �������
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.ReadSettingsFromRegistry ( var rcConnectionParams : TConnectionDBParams ): Boolean;

//------------------------------------------------------------------------------
var

  // ���������� ��� ������ � ��������
  Reg: TRegistry;

  // ������� ������
  bErr: Boolean;

  // ����������, � ������� ����� ��� ���������
  regdatatypeParam: TRegDataType;
//------------------------------------------------------------------------------

begin

  bErr := False;

  // ������ ��������� �� �������
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  if ( Reg.KeyExists( m_sMainRegistryKey ) = True ) then
  begin

    // ������ ���������
    try

      Reg.OpenKeyReadOnly ( m_sMainRegistryKey );
      Reg.ReadBinaryData( m_sNameRegistry, rcConnectionParams, SizeOf( rcConnectionParams ) );
      (*if
         //( Reg.ReadBinaryData( m_sNameRegistry, rcConnectionParams, SizeOf( rcConnectionParams ) ) = 0 ) and
         ( ProcessIsElevated () = False ) and ( bAdminMessageShown = False ) then
      begin

        ShowMessage( '���������� ��������� ������ �� �������. ���������� ������������� ��������� �� ����� ��������������!' );
        bAdminMessageShown := True;

      end; *)

      //���� �����������
      if Pos( 'Password', rcConnectionParams.m_sConnectionString ) = 0 then
      begin

        rcConnectionParams.m_sConnectionString := DecryptString( rcConnectionParams.m_sConnectionString );
        bRegistryCrypted := True;

      end
      else
      begin

        // ���� ������ � ������� �������������, �� �� ������� ������������ � ����������� �������� ��������� � GetDBConnection
        bErr := False;  //bErr := True;
        bRegistryCrypted := False;

        {mAuthorization.cxTextEditPassword.Enabled := False;
        fmAuthorization.cxButtonOk.Enabled := False;
        fmAuthorization.cxLabelErrMess.Caption := '������������� ���������� � ������� ������� GetDBConnection!';}

      end;

      Reg.CloseKey;

    except

      on E: Exception do
      begin

        bErr := True;

        (*
        ���������� ��� ���������� ������� �� �����������, ������� ������
        if {( ProcessIsElevated ( ) = False ) and} ( bAdminMessageShown = False ) then
        begin

          ShowMessage( '���������� ��������� ������ �� �������. ���������� ������������� ��������� �� ����� ��������������!!' );
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
// �������� ����������� �� ������
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.KeyExists( ): Boolean;

//------------------------------------------------------------------------------
var

  // ���������� ��� ������ � ��������
  ctRegistry: TRegistry;

  // ������� ������
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
// ���������� ������ ���������� � ������
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.WriteSettingsToRegistry ( var rcConnectionParams : TConnectionDBParams; const bNeedCreateKey : Boolean = False ): Boolean;

//------------------------------------------------------------------------------
var

  // ���������� ��� ������ � ��������
  ctRegistry: TRegistry;

  // ������� ������
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
// �������� ������ (��� ������ � ������)
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.EncryptString( AString: AnsiString ): AnsiString;
var

  // ������� ������ ������� � ������
  i: Integer;

  // ����� � ������� ANSI ��������������� �����
  EncryptedCharNumber: Byte;

  // ������ ��� ������ ���������� �����������
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
// ����������� ������ (��� ������ �� �������), ������������� � ������� �������
// GetDBConnection (�-��� EncryptString)
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.DecryptString( sEncrypted: AnsiString ): AnsiString;
var

  // ������� ������ ������� � ������
  i: Integer;

  // ����� � ������� ANSI ��������������� �����
  bDecryptedChar: Byte;

  // ������ ��� ������ ���������� �����������
  sDecrypted: string;
begin

  bDecryptedChar := 0;
  sDecrypted := '';

  for i := 1 to Length( sEncrypted ) do
  begin

    {� ������������� ������ ��� ������� "��������" �� ������� ANSI �� bEncryptionShift �������,
    ��� ����������� "��������" �������}
    bDecryptedChar := ( Ord( sEncrypted[ i ] ) + ( 256 - bEncryptionShift ) ) mod 256;
    sDecrypted := sDecrypted + AnsiChar( bDecryptedChar );

  end;

  Result := sDecrypted;

end;

//------------------------------------------------------------------------------
// ���������� ������ � ������
//------------------------------------------------------------------------------

function TConnectionParamsInRegistry.WriteToRegistry ( const sRootKey: HKEY; var iParams : Integer; const bNeedCreateKey : Boolean = False ): Boolean;

//------------------------------------------------------------------------------
var

  // ���������� ��� ������ � ��������
  ctRegistry: TRegistry;

  // ������� ������
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
// ���������� ������
//------------------------------------------------------------------------------

destructor TConnectionParamsInRegistry.Destroy;
begin

  inherited Destroy;

end;

end.

