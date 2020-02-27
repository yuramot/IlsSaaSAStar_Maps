  if IsConsole then

{$IFDEF XML_OUTPUT}
    with XMLTestRunner2.RunRegisteredTests(outputFileName) do
{$ELSE}
    with TextTestRunner.RunRegisteredTests(rxbHaltOnFailures) do
{$ENDIF}
      Free
  else
    GUITestRunner.RunRegisteredTests;
