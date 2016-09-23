namespace DelphiRTL.Tests.Main;

interface

uses
 java.util,
  RemObjects.Elements.EUnit;

type
  ConsoleApp = class
  public
    class method Main(args: array of String);
  end;

implementation

class method ConsoleApp.Main(args: array of String);
begin
  try
    var TestItems := Discovery.DiscoverTests;
 
    Runner.RunTests(TestItems) withListener(new ConsoleTestListener);
  except
    on E: Exception do
      writeLn("Unable to perform test: " + E.Message); 
  end;

  writeLn("Press any key to exit."); 
  system.in.read;
end;

end.
