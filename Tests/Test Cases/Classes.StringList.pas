namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  Elements.RTL.Delphi;

type
  StringListUsage = public class(Test)
  private
    fList: TStringList;
  public
    method Setup; override;
    begin
      fList := new TStringList;
      fList.AddObject('One', fList);
      fList.AddObject('Two', fList);
      fList.AddObject('Three', fList);
    end;

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
      fList.Exchange(0, 2);
      Assert.AreEqual(fList[0], 'Three');
      Assert.AreEqual(fList[2], 'One');
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
      fList.Insert(1, 'One.Five');
      Assert.AreEqual(fList.Count, 4);
      Assert.AreEqual(fList[1], 'One.Five');

      fList.Insert(0, 'Zero');
      Assert.AreEqual(fList[0], 'Zero');
      Assert.AreEqual(fList[1], 'One');
    end;

    method InsertObjectTests;
    begin
      fList.InsertObject(1, 'One.Five', fList);
      Assert.AreEqual(fList[1], 'One.Five');
      Assert.AreEqual(fList.Objects[1], fList);
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

  StringsUsage = public class(Test)
  private
    fList: TStringList;
    fLastIndex: Integer;
  public
    method Setup; override;
    begin
      fList := new TStringList;
      fLastIndex := 0;
    end;

    method AddPairTests;
    begin
      fList.AddPair('Ford', 'Focus');
      fList.AddPair('Honda', 'Civic');
      fList.AddPair('Chevrolet', 'Impala');

      Assert.AreEqual(fList.Values['Ford'], 'Focus');
      Assert.AreEqual(fList.Values['Chevrolet'], 'Impala');
      fList.AddPair('Dodge', 'Viper', fList);
      Assert.AreEqual(FList.Objects[fLastIndex + 3], FList);
    end;

    method AppendTests;
    begin
      fList.Append('One string');
      Assert.AreEqual(fList[fLastIndex], 'One string');
    end;

    method AddStringsTests;
    begin
      var lAnother := new TStringList;
      lAnother.Add('One');
      lAnother.Add('Two');

      fList.AddStrings(lAnother);
      Assert.AreEqual(lAnother[fLastIndex + 1], 'Two');

      var lArray := new DelphiString[3];
      lArray[0] := 'First One';
      lArray[1] := 'Second One';
      lArray[2] := 'Third One';
      fList.AddStrings(lArray);
      Assert.AreEqual(fList[fLastIndex + 4], 'Third One');

      var lArray2 := new DelphiString[3];
      lArray2[0] := 'Ferrari';
      lArray2[1] := 'Porsche';
      lArray2[2] := 'Tesla';
      var lObjects := new TObject[3];
      lObjects[0] := fList;
      lObjects[1] := fList;
      lObjects[2] := fList;

      fList.AddStrings(lArray2, lObjects);
      Assert.AreEqual(fList[fLastIndex + 7], 'Tesla');
    end;
  end;

end.
