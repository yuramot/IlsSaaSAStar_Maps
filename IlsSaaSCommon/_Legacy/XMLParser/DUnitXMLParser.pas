unit DUnitXMLParser;

interface

uses
  System.SysUtils, System.Variants,  Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc, TestStructureUnit, RTTI,
  System.Generics.Collections;

type
  TTestsXMLParser = class
  private
    FXMLFileName: string;
    FXMLDocument: IXMLDocument;
    function LoadXML: Boolean;
    function LoadSuite(ASuiteNode: IXMLNode): Boolean;
    procedure LoadTestCase(ASuiteName: string; ASuiteClassName: string; ATestCaseNode: IXMLNode);
    procedure LoadTestData(ATestNode: IXMLNode; out AData: TDataArray);
  public
    constructor Create(aXMLFileName: string);
    procedure RunXMLParsing(var aTestCaseList: TTestCaseDictionary; var aSuiteList: TSuiteList);
  end;

  procedure LoadTestsFromXML(aXMLFileName: string; var aSuiteList: TSuiteList; var aTestCaseList: TTestCaseDictionary);

implementation

constructor TTestsXMLParser.Create(aXMLFileName: string);
begin
  FXMLDocument := TXMLDocument.Create(nil);
  FXMLFileName := aXMLFileName;
end;

function TTestsXMLParser.LoadSuite(ASuiteNode: IXMLNode): Boolean;
var
  SuiteRec: TSuiteRec;
  i: Integer;
begin
  if ASuiteNode.NodeName = 'TestSuite' then
  begin
    SuiteRec.SuiteName := ASuiteNode.Attributes['Name'];
    SuiteRec.SuiteClassName := ASuiteNode.Attributes['ClassName'];
    SuiteList.Add(SuiteRec);
    for i := 0 to ASuiteNode.ChildNodes.Count - 1 do
      if ASuiteNode.ChildNodes[i].NodeName = 'TestCase' then
        LoadTestCase(SuiteRec.SuiteName, SuiteRec.SuiteClassName, ASuiteNode.ChildNodes[i]);

    Result := True;
  end
  else
    Result := False;
end;

procedure TTestsXMLParser.LoadTestData(ATestNode: IXMLNode; out AData: TDataArray);
var
  i, j: Integer;
  a: TDataArray;
begin
  if ATestNode.AttributeNodes.Count > 0 then
  begin
    SetLength(AData, ATestNode.AttributeNodes.Count);
    for i := 0 to ATestNode.AttributeNodes.Count - 1 do
    begin
      AData[i].DataType := dtString;
      AData[i].ValueName := ATestNode.AttributeNodes[i].NodeName;
      AData[i].Value := ATestNode.AttributeNodes[i].NodeValue;
    end
  end
  else
  begin
    SetLength(AData, ATestNode.ChildNodes.Count);
    for i := 0 to ATestNode.ChildNodes.Count - 1 do
    begin
      AData[i].DataType := dtData;
      AData[i].ValueName := ATestNode.ChildNodes[i].NodeName;
      LoadTestData(ATestNode.ChildNodes[i], a);
      SetLength(AData[i].Value2, Length(a));
      for j := Low(AData[i].Value2) to High(AData[i].Value2) do
        AData[i].Value2[j] := a[j];
    end
  end;
end;

procedure TTestsXMLParser.LoadTestCase(ASuiteName: string; ASuiteClassName: string; ATestCaseNode: IXMLNode);
var
  TestCaseRec: TTestCaseRec;
  TestRec: TTestRec;
  i, j: Integer;
begin
  TestCaseRec.TestCaseName := ATestCaseNode.Attributes['Name'];
  TestCaseRec.TestClassName := ASuiteClassName;
  TestCaseRec.MethodName := ATestCaseNode.Attributes['MethodName'];
  TestCaseRec.SuiteName := ASuiteName;
  TestCaseRec.Tests := TTestList.Create();
  for i := 0 to ATestCaseNode.ChildNodes.Count - 1 do
    if ATestCaseNode.ChildNodes[i].NodeName = 'Test' then
    begin
      TestRec.Name := ATestCaseNode.ChildNodes[i].Attributes['Name'];
      TestRec.TestCaseName := TestCaseRec.TestCaseName;
      TestRec.Operation := ATestCaseNode.ChildNodes[i].Attributes['Operation'];
      for j := 0 to ATestCaseNode.ChildNodes[i].ChildNodes.Count - 1 do
        if ATestCaseNode.ChildNodes[i].ChildNodes[j].NodeName = 'InputData' then
          LoadTestData(ATestCaseNode.ChildNodes[i].ChildNodes[j], TestRec.InputParameters)
        else if ATestCaseNode.ChildNodes[i].ChildNodes[j].NodeName = 'OutputData' then
          LoadTestData(ATestCaseNode.ChildNodes[i].ChildNodes[j], TestRec.OutputParameters)
        else if ATestCaseNode.ChildNodes[i].ChildNodes[j].NodeName = 'ExpectedResult' then
          TestRec.FailedMessageText := ATestCaseNode.ChildNodes[i].ChildNodes[j].Attributes['FailMessageText'];

      TestCaseRec.Tests.Add(TestRec);
    end;
//  TestCaseDictionary.Add(ATestCaseNode.Attributes['MethodName'], TestCaseRec);
  TestCaseDictionary.Add(ATestCaseNode.Attributes['Name'], TestCaseRec);
end;

function TTestsXMLParser.LoadXML: Boolean;
begin
  Result := false;
  try
    if FileExists(FXMLFileName) then
    begin
      FXMLDocument.LoadFromFile(FXMLFileName);
      if not FXMLDocument.IsEmptyDoc then
        Result := True;
    end
    else
      Result := False;
    FXMLDocument.Active := Result;
  except
    FreeAndNil(FXMLDocument);
  end;
end;


procedure TTestsXMLParser.RunXMLParsing(var aTestCaseList: TTestCaseDictionary; var aSuiteList: TSuiteList);
var
  RootNode: IXMLNode;
  i: Integer;
begin
  if not LoadXML or (FXMLDocument.ChildNodes.Count <> 1) then
    Exit;

  RootNode := FXMLDocument.ChildNodes[0];
  for i := 0 to RootNode.ChildNodes.Count - 1 do
    LoadSuite(RootNode.ChildNodes[i]);

  FXMLDocument.Active := false;
end;


procedure LoadTestsFromXML(aXMLFileName: string; var aSuiteList: TSuiteList; var aTestCaseList: TTestCaseDictionary);
var
  XMLParser: TTestsXMLParser;
begin
  XMLParser := TTestsXMLParser.Create(aXMLFileName);
  try
    XMLParser.RunXMLParsing(aTestCaseList, aSuiteList);
  finally
    FreeAndNil(XMLParser);
  end;
end;

end.
