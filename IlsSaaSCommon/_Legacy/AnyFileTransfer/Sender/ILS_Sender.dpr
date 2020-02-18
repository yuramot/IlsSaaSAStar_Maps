program ILS_Sender;

uses
  Vcl.SvcMgr,
  SenderMain in 'SenderMain.pas' {ILS_CommonSender: TService},
  blcksock in '..\..\_include\Synapse\blcksock.pas',
  synafpc in '..\..\_include\Synapse\synafpc.pas',
  synsock in '..\..\_include\Synapse\synsock.pas',
  synautil in '..\..\_include\Synapse\synautil.pas',
  synacode in '..\..\_include\Synapse\synacode.pas',
  synaip in '..\..\_include\Synapse\synaip.pas',
  C_FileNames in '..\..\_Include\C_FileNames.pas',
  T_Common in '..\..\_Include\T_Common.pas',
  E_Logger in '..\..\_Include\E_Logger.pas',
  E_Threads in '..\..\_Include\E_Threads.pas',
  E_Files in '..\..\_Include\E_Files.pas',
  E_Utils in '..\..\_Include\E_Utils.pas',
  E_UtilsFiles in '..\..\_include\E_UtilsFiles.pas',
  E_UtilsStr in '..\..\_include\E_UtilsStr.pas';

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TILS_CommonSender, ILS_CommonSender);
  Application.Run;
end.

