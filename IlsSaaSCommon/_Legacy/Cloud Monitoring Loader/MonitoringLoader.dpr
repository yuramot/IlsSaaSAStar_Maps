program MonitoringLoader;

{$R 'Product_Version.res' 'Product_Version.rc'}

uses
  SvcMgr,
  MonitoringLoaderMain in 'MonitoringLoaderMain.pas' {ILSLoader: TService},
  E_UtilsFiles in '..\_include\E_UtilsFiles.pas',
  C_FileNames in '..\_include\C_FileNames.pas',
  E_Threads in '..\_include\E_Threads.pas',
  E_Logger in '..\_include\E_Logger.pas',
  E_Files in '..\_include\E_Files.pas',
  E_UtilsStr in '..\_include\E_UtilsStr.pas',
  synacode in '..\_include\Synapse\synacode.pas',
  E_FileVersionUtils in '..\_include\E_FileVersionUtils.pas',
  JsonDataObjects in '..\..\..\lib\Json\JsonDataObjects.pas',
  Ils.Files.Pnt in '..\..\_Common\Ils.Files.Pnt.pas',
  Ils.Json.Utils in '..\..\_Common\Ils.Json.Utils.pas',
  Ils.Json.Names in '..\..\_Common\Ils.Json.Names.pas',
  Ils.Utils in '..\..\_Common\Ils.Utils.pas';

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TILSLoader, ILSLoader);
  Application.Run;
end.
