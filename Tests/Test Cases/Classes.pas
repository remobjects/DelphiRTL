namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
   Elements.RTL.Delphi;

type
  StringListUsage = public class(Test)
  public
    method AddTests;
    begin
      var lList := new TStringList;
      lList.Add('Test');
      lList.Add('String');
      Assert.AreEqual(lList.Count, 2);
      Assert.AreEqual(lList[0], 'Test');
      Assert.AreEqual(lList[1], 'String');
    end;

    method AddObjectTests;
    begin
      var lList := new TStringList;
      lList.AddObject('One', lList);
      lList.AddObject('Two', lList);
      Assert.AreEqual(lList.Count, 2);
      Assert.AreEqual(lList[0], 'One');
      Assert.AreEqual(lList[1], 'Two');
      Assert.AreEqual(lList.Objects[0], lList);
      Assert.AreEqual(lList.Objects[1], lList);
    end;

    method ClearTests;
    begin
      var lList := new TStringList;
      lList.Add('Test');
      lList.Add('String');
      lList.Clear;
      Assert.AreEqual(lList.Count, 0);
    end;

    method DeleteTests;
    begin
      var lList := new TStringList;
      lList.AddObject('One', lList);
      lList.AddObject('Two', lList);
      lList.AddObject('Three', lList);
      lList.Delete(2);
      Assert.AreEqual(lList.Count, 2);
      Assert.AreEqual(lList[0], 'One');
      Assert.AreEqual(lList[1], 'Two');

      lList.Delete(0);
      Assert.AreEqual(lList.Count, 1);
      Assert.AreEqual(lList[0], 'Two');

      lList.Delete(0);
      Assert.AreEqual(lList.Count, 0);
    end;

    method ExchangeTests;
    begin
      var lList := new TStringList;
      lList.AddObject('One', lList);
      lList.AddObject('Two', lList);
      lList.AddObject('Three', lList);

      lList.Exchange(0, 2);
      Assert.AreEqual(lList[0], 'Three');
      Assert.AreEqual(lList[2], 'One');
    end;

  end;



end.
