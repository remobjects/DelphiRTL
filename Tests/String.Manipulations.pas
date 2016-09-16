namespace DelphiRTL.Tests.Shared;

uses
  RemObjects.Elements.EUnit,
  Sugar,
  Elements.RTL.Delphi;

type
  StringManipulations = public class(Test)
  public

    method DefaultStringTypeTests();
    begin
      var x {: DelphiString} := 'hello';
      Assert.IsTrue(x is DelphiString);      

      var ds: DelphiString := 'hello';
      Assert.IsTrue(ds is DelphiString);      

      //var ws: WideString := 'hello';
      //Assert.IsTrue(ws is DelphiString);  // Invalid IL code in DelphiRTL.Tests.Shared.StringManipulations:DefaultStringTypeTests (): IL_006c: isinst    0x01000002

      var s : String := 'hello';
      Assert.IsTrue(s is PlatformString);      
    end;
    
    method IndexerTests();
    begin
      var x: DelphiString := 'house';
      x[1] := 'm';
      Assert.AreEqual(x, 'mouse');

      x.Remove(0, 1);
      Assert.AreEqual(x, 'ouse');
      
      x.Insert(0, "sp");
      Assert.AreEqual(x, 'spouse');
    end;
  end;

end.
