program ILS_Receiver;

uses
  Vcl.SvcMgr,
  ReceiverMain in 'ReceiverMain.pas' {ILS_CommonReceiver: TService},
  blcksock in '..\..\_Include\Synapse\blcksock.pas',
  synaip in '..\..\_Include\Synapse\synaip.pas',
  synautil in '..\..\_Include\Synapse\synautil.pas',
  synafpc in '..\..\_Include\Synapse\synafpc.pas',
  synacode in '..\..\_Include\Synapse\synacode.pas',
  synsock in '..\..\_Include\Synapse\synsock.pas',
  C_FileNames in '..\..\_Include\C_FileNames.pas',
  T_Common in '..\..\_Include\T_Common.pas',
  T_Points in '..\..\_Include\T_Points.pas',
  E_Logger in '..\..\_Include\E_Logger.pas',
  E_Threads in '..\..\_Include\E_Threads.pas',
  E_Files in '..\..\_Include\E_Files.pas',
  E_Utils in '..\..\_Include\E_Utils.pas',
  E_MemStream in '..\..\_Include\E_MemStream.pas',
  E_ThreadManager in '..\..\_include\E_ThreadManager.pas',
  E_Pairs in '..\..\_include\E_Pairs.pas',
  E_UtilsStr in '..\..\_include\E_UtilsStr.pas',
  E_UtilsFiles in '..\..\_include\E_UtilsFiles.pas';

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TILS_CommonReceiver, ILS_CommonReceiver);
  GService := ILS_CommonReceiver;
  Application.Run;
end.

