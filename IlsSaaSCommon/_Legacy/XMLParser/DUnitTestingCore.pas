unit DUnitTestingCore;

interface

uses
  System.Math, System.SysUtils, System.Variants, System.Generics.Collections, System.Generics.Defaults,
  TestFramework, TestStructureUnit, RTTI, Classes, DUnitXMLParser;

type

  TVarArray = array of Variant;
  TCoreTestCaseClass = class of TCoreTestCase;

  TTestClassesList = TList<TCoreTestCaseClass>;

  // TestSuites
  TCoreTestSuite = class (TTestSuite)
  private
    FSuiteName: string;
  public
    constructor Create(ASuiteName: string; Tests: TTestCaseDictionary; ATestClass: TCoreTestCaseClass); overload;
    procedure AddTests(Tests: TTestCaseDictionary; aTestClass: TCoreTestCaseClass); virtual;
  end;

  //TestCases
  TCoreTestCase = class (TTestCase)
  protected
    FFolderName: string;
    FSuitePath: string;
    FSuiteName: string;
    FTestClass: TCoreTestCaseClass;
    FMethodName: string;
    ExpectedResult: Variant;
    FailMessage: string;
    Operation: string;
    function VarToBool(Arg: Variant): Boolean;
    procedure AssertResults<T>(aExpectedResult: Variant; aActualResult: T; aOperation: string; aFailMessageTemplate: string);
  public
    constructor Create(TestCase: TTestCaseRec); reintroduce; overload;
    procedure AssertResult<T>(aActualResult: T);
    procedure AssertDoubleResults(ActualResult: Double);
    procedure AssertBooleanResults(ActualResult: Boolean);
    function CompareFiles(FileName: string; EtalonFileName: string): boolean;
    function IsArray(Arg: Variant): Boolean;
    function IsStructure(Arg: Variant): Boolean;
    //function GetParameters: TVarArray;
    function GetStructure(RecStr: Variant): TArray<Variant>;
    function GetArray(ArrStr: Variant): TArray<Variant>;
    function ArrayLength(ArrStr: Variant): integer;
  end;

  procedure PrepareToTest(TestsFileName: string);
  procedure RegisterTestClass(aClass: TCoreTestCaseClass);
  procedure CreateDUnitTests(ASuites: TSuiteList; Tests: TTestCaseDictionary; ATestClass: TCoreTestCaseClass);
  procedure CreateTests;

var
  TestClassesList: TTestClassesList;

implementation


constructor TCoreTestCase.Create(TestCase: TTestCaseRec);
begin
  inherited Create(TestCase.MethodName);

  //FFolderName := TestCase.TestCaseClass;
  FFolderName := '';
  FSuitePath := TestCase.SuiteName;
  FSuiteName := TestCase.SuiteName;
  FTestName := TestCase.TestCaseName;
  FMethodName := TestCase.MethodName;
end;


function TCoreTestCase.VarToBool(Arg: Variant): Boolean;
begin
  Result := LowerCase(VarToStr(Arg)) = 'true';
end;


{function TCoreTestCase.GetParameters: TVarArray;
var
  ParamValue: TInputDataArray;
  DataLen: integer;
  Index: integer;
begin
  ParamValue := DataArray.Items[Self.FTestName];
  DataLen := Length(ParamValue);
  SetLength(Result, DataLen - 3);
  for Index := 0 to DataLen - 4 do
  begin
    if LowerCase(ParamValue[Index].AsString) = 'now' then
      Result[Index] := Now()
    else
    if LowerCase(ParamValue[Index].AsString) = 'beg_now' then
      Result[Index] := Date() + StrToTime('00:00:00')
    else
    if LowerCase(ParamValue[Index].AsString) = 'end_now' then
      Result[Index] := Date() + StrToTime('23:59:59')
    else
      Result[Index] := ParamValue[Index].AsVariant;
  end;
  Self.ExpectedResult := ParamValue[DataLen - 3].AsVariant;
  Self.FailMessage := ParamValue[DataLen - 2].AsString;
  Self.Operation := ParamValue[DataLen - 1].AsString;
end;}


function TCoreTestCase.IsStructure(Arg: Variant): Boolean;
begin
  Result := Pos('struct', VarToStr(Arg)) > 0;
end;


function TCoreTestCase.GetStructure(RecStr: Variant): TArray<Variant>;
const
  Delimiter = ';';
var
  DelimiterPos: Integer;
  Index: integer;
  TempStr: string;
begin
  Index := 0;
  TempStr := Trim(RecStr);
  TempStr := StringReplace(TempStr, 'struct(', '', [rfReplaceAll]);
  TempStr := StringReplace(TempStr, ')', '', [rfReplaceAll]);
  DelimiterPos := Pos(Delimiter, TempStr);
  while DelimiterPos > 0 do
  begin
    SetLength(Result, Index + 1);
    Result[Index] := Copy(TempStr, 1, DelimiterPos - 1);
    Delete(TempStr, 1, DelimiterPos);
    DelimiterPos := Pos(Delimiter, TempStr);
    Inc(Index);
  end;
  SetLength(Result, Index + 1);
  Result[Index] := TempStr;
end;


function TCoreTestCase.IsArray(Arg: Variant): Boolean;
begin
  Result := Pos('array', VarToStr(Arg)) > 0;
end;


function TCoreTestCase.GetArray(ArrStr: Variant): TArray<Variant>;
const
  Delimiter = ';';

var
  CharIndex: integer;
  ArrayIndex: Integer;
  TempStr: string;
  ResultStr: string;
  IsStruct: Boolean;
begin
  ResultStr := '';
  ArrayIndex := 0;
  IsStruct := False;
  TempStr := Trim(ArrStr);
  TempStr := StringReplace(TempStr, 'array[', '', [rfReplaceAll]);
  TempStr := StringReplace(TempStr, ']', '', [rfReplaceAll]);

  for CharIndex := 1 to Length(TempStr) do
  begin
    if TempStr[CharIndex] = '(' then
      IsStruct := True;
    if TempStr[CharIndex] = ')' then
      IsStruct := False;

    if not IsStruct and (TempStr[CharIndex] = Delimiter) then
    begin
      SetLength(Result, ArrayIndex + 1);
      Result[ArrayIndex] := ResultStr;
      inc(ArrayIndex);
      ResultStr := '';
    end
    else
    if CharIndex = Length(TempStr) then
    begin
      ResultStr := ResultStr + TempStr[CharIndex];
      SetLength(Result, ArrayIndex + 1);
      Result[ArrayIndex] := ResultStr;
    end
    else
    begin
      ResultStr := ResultStr + TempStr[CharIndex];
    end;

  end;
end;


function TCoreTestCase.ArrayLength(ArrStr: Variant): integer;
const
  Delimiter = ';';
var
  TempStr: string;
  CharIndex: Integer;
  IsStruct: Boolean;
begin
  TempStr := VarToStr(ArrStr);
  Result := 0;
  IsStruct := False;
  if TempStr <> 'array[]' then
  begin
    for CharIndex := 1 to Length(TempStr) do
    begin
      if TempStr[CharIndex] = '(' then
        IsStruct := True;
      if TempStr[CharIndex] = ')' then
        IsStruct := False;
      if not IsStruct and (TempStr[CharIndex] = Delimiter) then
        inc(Result);
    end;
    Result := Result + 1;
  end;
end;


procedure TCoreTestCase.AssertResult<T>(aActualResult: T);
begin
  AssertResults<T>(ExpectedResult, aActualResult, Operation, FailMessage);
end;


procedure TCoreTestCase.AssertResults<T>(aExpectedResult: Variant; aActualResult: T; aOperation: string; aFailMessageTemplate: string);
var
  AssertionResult: Boolean;
  ActualResultValue: TValue;
  ExpectedResultValue: TValue;
  FailMessageValue: String;
begin
  if aOperation = 'except' then
    CheckException(fMethod, Exception, '')
  else
  begin
    ActualResultValue := TValue.From<T>(aActualResult);
    ExpectedResultValue := TValue.From<Variant>(aExpectedResult);
    if aOperation = 'equals' then
      AssertionResult := ActualResultValue.AsVariant = ExpectedResultValue.AsVariant;
    if aOperation = 'not equals' then
      AssertionResult := ActualResultValue.AsVariant <> ExpectedResultValue.AsVariant;
    if aOperation = 'larger than' then
      AssertionResult := ActualResultValue.AsVariant > ExpectedResultValue.AsVariant;
    if aOperation = 'equals or larger than' then
      AssertionResult := ActualResultValue.AsVariant >= ExpectedResultValue.AsVariant;
    if aOperation = 'less than' then
      AssertionResult := ActualResultValue.AsVariant < ExpectedResultValue.AsVariant;
    if aOperation = 'equals or less than' then
      AssertionResult := ActualResultValue.AsVariant <= ExpectedResultValue.AsVariant;
    if aOperation = 'contains' then
      AssertionResult := Pos( VarToStr(ExpectedResultValue.AsVariant), VarToStr(ActualResultValue.AsVariant)) > 0;
    if aOperation = 'not contains' then
      AssertionResult := Pos(VarToStr(ExpectedResultValue.AsVariant), VarToStr(ActualResultValue.AsVariant)) = 0;

    FailMessageValue := StringReplace(aFailMessageTemplate, '%r', VarToStr(ActualResultValue.AsVariant), [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%o', Operation, [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%e', VarToStr(ExpectedResultValue.AsVariant), [rfReplaceAll]);
    if Pos(' not not', FailMessageValue) > 0 then
      FailMessageValue := StringReplace(FailMessageValue, ' not not', '', [rfReplaceAll]);

    Check(AssertionResult, FailMessageValue);
  end;
end;


procedure TCoreTestCase.AssertDoubleResults(ActualResult: Double);
var
  ExpectedResultValue: string;
  ActualResultValue: string;
  TrimValue: Integer;
begin
  ExpectedResultValue := StringReplace(VarToStr(ExpectedResult), ',', '.', [rfReplaceAll]);
  TrimValue := Length(ExpectedResultValue) - Pos('.', ExpectedResultValue);
  ActualResultValue := StringReplace(FloatToStr(RoundTo(ActualResult, -TrimValue)), ',', '.', [rfReplaceAll]);
  AssertResults<String>(ExpectedResultValue, ActualResultValue, Operation, FailMessage);
end;


procedure TCoreTestCase.AssertBooleanResults(ActualResult: Boolean);
var
  ExpectedResultValue: string;
  ActualResultValue: string;
begin
  ExpectedResultValue := LowerCase(VarToStr(ExpectedResult));
  ActualResultValue := LowerCase(BoolToStr(ActualResult));
  AssertResults<String>(ExpectedResultValue, ActualResultValue, Operation, FailMessage);
end;


function TCoreTestCase.CompareFiles(FileName: string; EtalonFileName: string): boolean;
var
  FileMem: TMemoryStream;
  EtalonFileMem: TMemoryStream;
begin
  FileMem := TMemoryStream.Create;
  EtalonFileMem := TMemoryStream.Create;
  try
    FileMem.LoadFromFile(FileName);
    EtalonFileMem.LoadFromFile(EtalonFileName);
    if FileMem.Size <> EtalonFileMem.Size then
      Result := False
    else
      Result := CompareMem(FileMem.Memory, EtalonFileMem.Memory, FileMem.Size);
  finally
    FileMem.Free();
    EtalonFileMem.Free();
  end;
end;


constructor TCoreTestSuite.Create(ASuiteName: string; Tests: TTestCaseDictionary; ATestClass: TCoreTestCaseClass);
begin
  inherited Create(ASuiteName);
  FSuiteName := ASuiteName;
  AddTests(Tests, ATestClass);
end; // Create


procedure TCoreTestSuite.AddTests(Tests: TTestCaseDictionary; ATestClass: TCoreTestCaseClass);
var
  TestCaseKey: string;
  MethodName: string;
  TestName: string;
  SuiteName: string;
  TestCaseRec: TTestCaseRec;

begin
  // For all tests from file
  for TestCaseKey in Tests.Keys do
  begin
    // Get method for current test
    MethodName := TestCaseKey;

    // Get suite name for current test
    SuiteName := Tests[TestCaseKey].SuiteName;
    if SuiteName = '' then
       SuiteName := ATestClass.ClassName;
    TestCaseRec := Tests[TestCaseKey];
    TestCaseRec.SuiteName := StringReplace(SuiteName, '.', ' ', [rfReplaceAll]);

    // Get test case name for current test
    TestName := Tests[TestCaseKey].TestCaseName;
    if TestName = '' then
      TestName := MethodName;
    TestCaseRec.TestCaseName := StringReplace(TestName, '.', ';', [rfReplaceAll]);

    Tests[TestCaseKey] := TestCaseRec;

    // If is current suite and test class then add test to suite
    if (Tests[TestCaseKey].SuiteName = Self.FSuiteName) and
       (TestCaseRec.TestClassName = ATestClass.ClassName)
    then
      Self.AddTest(ATestClass.Create(Tests[TestCaseKey]) as ITest);

  end;
end; // AddTests


procedure PrepareToTest(TestsFileName: string);
begin
  LoadTestsFromXML(TestsFileName, SuiteList, TestCaseDictionary);
end; // PrepareToTest


procedure RegisterTestClass(aClass: TCoreTestCaseClass);
begin
  if not Assigned(TestClassesList) then
    TestClassesList := TList<TCoreTestCaseClass>.Create;
  TestClassesList.Add(aClass);
end;


procedure CreateDUnitTests(ASuites: TSuiteList; Tests: TTestCaseDictionary; ATestClass: TCoreTestCaseClass);
var
  iSuiteIndex: integer;
  Suite: TCoreTestSuite;
begin
  for iSuiteIndex := 0 to ASuites.Count - 1 do
  begin
    if ASuites[iSuiteIndex].SuiteClassName = ATestClass.ClassName then
    begin
      Suite := TCoreTestSuite.Create(ASuites[iSuiteIndex].SuiteName, Tests, ATestClass);
      RegisterTest(Suite);
    end;
  end;
end;


procedure CreateTests;
var
  TestClass: TCoreTestCaseClass;
begin
  for TestClass in TestClassesList do
    CreateDUnitTests(SuiteList, TestCaseDictionary, TestClass);
end;


end.
