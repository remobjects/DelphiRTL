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

    method FindTests;
    begin
      var lList := new TStringList;
      lList.Add('A string on a list');
      lList.Add('test string');
      lList.Add('on the road');

      var lIndex: Integer;
      var lRes: Boolean;
      lRes := lList.Find('test string', var lIndex);
      Assert.AreEqual(lRes, true);
      Assert.AreEqual(lIndex, 1);
      lList.CaseSensitive := true;
      lIndex := -1;
      lRes := lList.Find('Test String', var lIndex);
      Assert.AreEqual(lRes, false);

      lList.CaseSensitive := false;
      lIndex := -1;
      lRes := lList.Find('Test String', var lIndex);
      Assert.AreEqual(lRes, true);
      Assert.AreEqual(lIndex, 1);
    end;

    method IndexOfTests;
    begin
      var lList := new TStringList;
      lList.Add('A string on a list');
      lList.Add('test string');
      lList.Add('on the road');
 
      var lIndex := lList.IndexOf('test string');
      Assert.AreEqual(lIndex, 1);

      lIndex := lList.IndexOf('on the road');
      Assert.AreEqual(lIndex, 2);
    end;

    method InsertTests;
    begin
      var lList := new TStringList;
      lList.Add('One');
      lList.Add('Two');
      lList.Add('Three');

      lList.Insert(1, 'One.Five');
      Assert.AreEqual(lList.Count, 4);
      Assert.AreEqual(lList[1], 'One.Five');

      lList.Insert(0, 'Zero');
      Assert.AreEqual(lList[0], 'Zero');
      Assert.AreEqual(lList[1], 'One');
    end;

    method InsertObjectTests;
    begin
      var lList := new TStringList;
      lList.Add('One');
      lList.Add('Two');
      lList.Add('Three');
      
      lList.InsertObject(1, 'One.Five', lList);
      Assert.AreEqual(lList[1], 'One.Five');
      Assert.AreEqual(lList.Objects[1], lList);
    end;

    method SortTests;
    begin
      var lList := new TStringList;
      lList.Add('Zero');
      lList.Add('More');
      lList.Add('About');

      lList.Sort;
      Assert.AreEqual(lList[0], 'About');
      Assert.AreEqual(lList[1], 'More');
      Assert.AreEqual(lList[2], 'Zero');
    end;
  end;

end.
