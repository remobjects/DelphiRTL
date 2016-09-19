namespace DelphiRTL.Tests;

interface

uses
  RemObjects.Elements.EUnit;

implementation

begin
  {$IF TOFFEE}
  writeLn(Foundation.NSProcessInfo.processInfo.environment().description);
  for a in Foundation.NSProcessInfo.processInfo.arguments do
    writeLn(a);
  {$ENDIF}
  var lTests := Discovery.DiscoverTests();
  //Runner.RunTests(lTests) withListener(Runner.DefaultListener);
  Runner.RunTests(lTests) withListener(new ConsoleTestListener);
end.
