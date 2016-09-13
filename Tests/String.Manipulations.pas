namespace DelphiRTL.Tests.Shared;

uses
  RemObjects.Elements.EUnit,
  Sugar,
  Elements.RTL.Delphi;

type
  StringManipulations = public class(Test)
  private
  protected
  public
    
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
