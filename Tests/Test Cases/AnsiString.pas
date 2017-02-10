namespace DelphiRTL.Tests.Shared;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi;

type
  AnsiStringUsage = public class(Test)
  public

    /*method IndexerTests();
    begin
      var x : AnsiString := 'hello';
      Assert.AreEqual(x[1], 'h');
      
    end;*/

    method InsertTests();
    begin
      var lStr: AnsiString := 'hello friends';
      var lSubStr: AnsiString := 'my';
      lStr.Insert(0, lSubStr);
      Assert.AreEqual(lStr, 'myhello friends');
    end;
  end;

end.
