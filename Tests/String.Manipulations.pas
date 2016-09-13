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

      //Assert.AreEqual(x, 'mouse2');
    end;
  end;

end.
