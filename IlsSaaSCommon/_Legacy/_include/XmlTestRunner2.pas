(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Aez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Aez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

{
 Contributor : Laurent Laffont <llaffont@altaiire.fr>
}

unit XMLTestRunner2;

interface
uses
  SysUtils,
  Classes,
  TestFramework,
//  TestUtilsUnit,
  TextTestRunner;

const
   DEFAULT_FILENAME = 'dunit-report.xml';

type
  TXMLTestListener = class(TTextTestListener)//TInterfacedObject, ITestListener, ITestListenerX)
  private
     FOutputFile : TextFile;
     FFileName : String;

  protected
     startTime : Integer;
     dtStartTime : TDateTime;

     testStart : TDateTime;
     FSuiteStack : TStringList;

     procedure writeReport(str: String);

     function GetCurrentSuiteName : string;
  public
    // implement the ITestListener interface
    procedure AddSuccess(test: ITest); override;
    procedure AddError(error: TTestFailure); override;
    procedure AddFailure(failure: TTestFailure); override;
    function  ShouldRunTest(test :ITest):boolean; override;
    procedure StartSuite(suite: ITest); override;
    procedure EndSuite(suite: ITest); override;
    procedure StartTest(test: ITest); override;
    procedure EndTest(test: ITest); override;
    procedure TestingStarts; override;
    procedure TestingEnds(testResult: TTestResult); override;
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    constructor Create; overload;
    constructor Create(outputFile : String); overload;
    destructor Destroy; override;

    class function RunTest(suite: ITest; outputFile:String): TTestResult; overload;
    class function RunRegisteredTests(outputFile:String): TTestResult;
    class function text2sgml(text : String) : String;
    class function StringReplaceAll (text,byt,mot : string ) :string;

    //:Report filename. If an empty string, then standard output is used (compile with -CC option)
    property FileName : String read FFileName write FFileName;
  end;

{: Run the given test suite
}
function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;
function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;

implementation

uses Forms, Windows;

const
  CRLF = #13#10;
  MAX_DEEP = 5;

var
  fs: TFormatSettings;

{ TXMLTestListener }

constructor TXMLTestListener.Create;
begin
   Create(DEFAULT_FILENAME);
end;

constructor TXMLTestListener.Create(outputFile : String);
begin
   inherited Create;
   FileName := outputFile;
   FSuiteStack := TStringList.Create;
end;

{:
 Write F in the report file or on standard output if none specified
}
procedure TXMLTestListener.writeReport(str : String);
var
  myByte: Byte;
begin
   if TTextRec(FOutputFile).Mode = fmOutput then
   begin
//      writeln(FOutputFile, str)

    for myByte in TEncoding.UTF8.GetBytes(str) do
      Write(FOutputFile, AnsiChar(myByte));
    Writeln(FOutputFile);
   end
   else
      writeln(str);
end;

const
  TrueFalse : array[Boolean] of string = ('False', 'True');

procedure TXMLTestListener.AddSuccess(test: ITest);
begin
  inherited;
  if test.tests.Count<=0 then
  begin
    writeReport(Format('<test-case name="%s%s" executed="%s" success="True" time="%1.3f" result="Pass"/>',
                       [GetCurrentSuiteName, test.GetName, TrueFalse[test.Enabled], test.ElapsedTestTime / 1000], fs));
  end;
end;

procedure TXMLTestListener.AddError(error: TTestFailure);
begin
  inherited;
  writeReport(Format('<test-case name="%s%s" execute="%s" success="False" time="%1.3f" result="Error">',
                     [GetCurrentSuiteName, error.FailedTest.GetName, TrueFalse[error.FailedTest.Enabled], error.FailedTest.ElapsedTestTime / 1000], fs));
  writeReport(Format('<failure name="%s" location="%s">', [error.ThrownExceptionName, error.LocationInfo]));
  writeReport('<message>' + text2sgml(error.ThrownExceptionMessage) + '</message>');
//TODO:  writeReport('<stack-trace>' + text2sgml(GetJclStackAsString) + '</stack-trace>');
  writeReport('</failure>');
  writeReport('</test-case>');
end;

procedure TXMLTestListener.AddFailure(failure: TTestFailure);
begin
  inherited;
  writeReport(Format('<test-case name="%s%s" execute="%s" success="False" time="%1.3f" result="Failure">',
                     [GetCurrentSuiteName, failure.FailedTest.GetName, TrueFalse[failure.FailedTest.Enabled], failure.FailedTest.ElapsedTestTime / 1000], fs));
  writeReport(Format('<failure name="%s" location="%s">', [failure.ThrownExceptionName, failure.LocationInfo]));
  writeReport('<message>' + text2sgml(failure.ThrownExceptionMessage) + '</message>');
//TODO:  writeReport('<stack-trace>' + text2sgml(GetJclStackAsString) + '</stack-trace>');
  writeReport('</failure>');
  writeReport('</test-case>');
end;


procedure TXMLTestListener.StartTest(test: ITest);
begin
end;

procedure TXMLTestListener.EndTest(test: ITest);
begin
end;

procedure TXMLTestListener.TestingStarts;
var
  myByte: Byte;
begin
  inherited;
  startTime := GetTickCount;
  dtStartTime := Now;

  if FFileName<>'' then
  begin
    AssignFile(FOutputFile, FFileName);//, TEncoding.UTF8);
    Rewrite(FOutputFile);

    for myByte in TEncoding.UTF8.GetPreamble
      do write(FOutputFile, AnsiChar(myByte));
//    for myByte in TEncoding.UTF8.GetPreamble do
//      write(FOutputFile, AnsiChar(myByte));
  end;

  writeReport('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>');
//  writeReport('<?xml version="1.0" encoding="windows-1251" standalone="yes" ?>');
  writeReport(Format('<test-results total="%d" notrun="%d" date="%s" time="%s">',
                     [RegisteredTests.CountTestCases,
                      RegisteredTests.CountTestCases - RegisteredTests.CountEnabledTestCases,
                      DateToStr(Now),
                      TimeToStr(Now)]));
end;

procedure TXMLTestListener.TestingEnds(testResult: TTestResult);
var
   runTime : Double;
   successRate : Integer;
begin
  inherited;
  runtime := (GetTickCount - startTime) / 1000;
  if testResult.RunCount > 0 then
    successRate :=  Trunc(
      ((testResult.runCount - testResult.failureCount - testResult.errorCount)
        /testResult.runCount)
        *100)
  else
    successRate := 100;

  writeReport('<statistics>'+CRLF+
              '<stat name="tests" value="'+intToStr(testResult.runCount)+'" />'+CRLF+
              '<stat name="failures" value="'+intToStr(testResult.failureCount)+'" />'+CRLF+
              '<stat name="errors" value="'+intToStr(testResult.errorCount)+'" />'+CRLF+
              '<stat name="success-rate" value="'+intToStr(successRate)+'%" />'+CRLF+
              '<stat name="started-at" value="'+DateTimeToStr(dtStartTime)+'" />'+CRLF+
              '<stat name="finished-at" value="'+DateTimeToStr(now)+'" />'+CRLF+
              Format('<stat name="runtime" value="%1.3f"/>', [runtime], fs)+CRLF+
              '</statistics>'+CRLF+
              '</test-results>');

  if TTextRec(FOutputFile).Mode = fmOutput then
    Close(FOutputFile);
end;

class function TXMLTestListener.RunTest(suite: ITest; outputFile:String): TTestResult;
begin
  Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

class function TXMLTestListener.RunRegisteredTests(outputFile:String): TTestResult;
begin
  Result := RunTest(registeredTests, outputFile);
end;

function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
  Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
  Result := RunTest(registeredTests, outputFile);
end;


procedure TXMLTestListener.Status(test: ITest; const Msg: string);
begin
  writeReport(Format('INFO: %s: %s', [test.Name, Msg]));
end;

procedure TXMLTestListener.Warning(test :ITest; const Msg :string);
begin
  writeReport(Format('WARNING: %s: %s', [test.Name, Msg]));
end;

function TXMLTestListener.ShouldRunTest(test: ITest): boolean;
begin
  Result := test.Enabled;
  if not Result then
    writeReport(Format('<test-case name="%s%s" executed="False"/>',
                       [GetCurrentSuiteName, test.GetName]));
end;

procedure TXMLTestListener.EndSuite(suite: ITest);
begin
  if CompareText(suite.Name, ExtractFileName(Application.ExeName)) = 0 then
    Exit;
  writeReport('</results>');
  writeReport('</test-suite>');
  FSuiteStack.Delete(0);
end;

procedure TXMLTestListener.StartSuite(suite: ITest);
var
  s : string;
begin
   if CompareText(suite.Name, ExtractFileName(Application.ExeName)) = 0 then
     Exit;
   s := GetCurrentSuiteName + suite.Name;
   writeReport(Format('<test-suite name="%s" total="%d" notrun="%d">', [s, suite.CountTestCases, suite.CountTestCases - suite.CountEnabledTestCases]));
   FSuiteStack.Insert(0, suite.getName);
   writeReport('<results>');
end;

{:
 Replace byt string by mot in text string
 }
class function TXMLTestListener.StringReplaceAll (text,byt,mot : string ) :string;
var
  plats : integer;
begin
  While pos(byt,text) > 0 do
  begin
    plats := pos(byt,text);
    delete (text,plats,length(byt));
    insert (mot,text,plats);
  end;
  Result := text;
end;

{:
 Replace special character by sgml compliant characters
 }
class function TXMLTestListener.text2sgml(text : String) : String;
begin
  text := stringreplaceall (text,'<','&lt;');
  text := stringreplaceall (text,'>','&gt;');
  result := text;
end;

destructor TXMLTestListener.Destroy;
begin
  FreeAndNil(FSuiteStack);
  inherited Destroy;
end;

function TXMLTestListener.GetCurrentSuiteName: string;
var
  c : Integer;
begin
  Result := '';
  for c := 0 to FSuiteStack.Count - 1 do
    Result := FSuiteStack[c] + '.' + Result;
end;

initialization
  fs := TFormatSettings.Create(0);
  fs.DecimalSeparator := '.';
end.
