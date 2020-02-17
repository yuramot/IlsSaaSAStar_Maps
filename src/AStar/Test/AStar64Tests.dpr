program AStar64Tests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestAStar64_Core in 'TestAStar64_Core.pas',
  UFileStructures in '..\..\_Common\UFileStructures.pas',
  UGeoHash in '..\..\_Common\UGeoHash.pas',
  UStructArray in '..\..\_Common\UStructArray.pas',
  UGeoSimple in '..\..\_Common\UGeoSimple.pas',
  UFiles in '..\..\_Common\UFiles.pas',
  AStar64.Intf in '..\AStar64.Intf.pas',
  AStar64.Extra in '..\AStar64.Extra.pas',
  AStar64.Core in '..\AStar64.Core.pas',
  AStar64.Common in '..\AStar64.Common.pas',
  UTimeWatcher in '..\..\_Common\UTimeWatcher.pas',
  Geo.Pos in '..\..\_Common\Geo.Pos.pas',
  AStar64.FileStructs in '..\..\_Common\AStar64.FileStructs.pas',
  AStar64.Areas in '..\..\_Common\AStar64.Areas.pas';

{R *.RES}

begin
//  ReportMemoryLeaksOnShutdown := True;
//  FullDebugModeScanMemoryPoolBeforeEveryOperation := True;
//  FullDebugModeRegisterAllAllocsAsExpectedMemoryLeak := True;

//  EnableMemoryLeakReporting := True;

  DUnitTestRunner.RunRegisteredTests;
end.

