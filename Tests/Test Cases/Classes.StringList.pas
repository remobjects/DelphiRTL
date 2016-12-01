namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

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
    fNewList: TStringList;
  public
    method Setup; override;
    begin
      fList := new TStringList;
      fLastIndex := 0;

      fNewList := new TStringList;
      fNewList.AddPair('Ferrari', '488');
      fNewList.AddPair('Porsche', '911');
      fNewList.AddPair('Lamborghini', 'Huracan');
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

    method AssignTests;
    begin
      var lList := new TStringList;
      lList.Add('another one');
      lList.Duplicates := TDuplicates.dupIgnore;
      fList.Assign(lList);
      Assert.AreEqual(fList.Duplicates, TDuplicates.dupIgnore);
      Assert.AreEqual(fList.Count, 1);
    end;

    method SetStringsTests;
    begin
      var lList := new TStringList;
      lList.Add('another one');
      lList.Add('more');
      lList.Duplicates := TDuplicates.dupAccept;
      fList.SetStrings(lList);
      Assert.AreEqual(fList.Count, 2);
      Assert.AreEqual(fList.Duplicates, TDuplicates.dupIgnore);
    end;

    method EqualsTests;
    begin
      var lList := new TStringList;
      var lAnList := new TStringList;
      lList.Add('One String');
      lList.Add('Another string');
      lAnList.Add('One String');
      Assert.AreEqual(lList.equals(lAnList), false);
      lAnList.Add('Another string');
      Assert.AreEqual(lList.equals(lAnList), true);
    end;

    method IndexOfNameTests;
    begin
      Assert.AreEqual(fNewList.IndexOfName('Porsche'), 1);
      Assert.AreEqual(fNewList.IndexOfName('Lamborghini'), 2);
    end;

    method IndexOfObjectTests;
    begin
      var lNewList := new TStringList;
      lNewList.AddObject('One', lNewList);
      lNewList.AddObject('Two', fList);
      lNewList.AddObject('Three', self);

      Assert.AreEqual(lNewList.IndexOfObject(fList), 1);
    end;

    method ToStringArrayTests;
    begin
      fList.Add('One String');
      fList.Add('Another one');
      var lArray := fList.ToStringArray;

      Assert.AreEqual(lArray.length, fLastIndex + 2);
      Assert.AreEqual(lArray[fLastIndex + 1], 'Another one');
    end;

    method ToObjectArrayTests;
    begin
      var lNewList := new TStringList;
      lNewList.AddObject('One', lNewList);
      lNewList.AddObject('Two', fList);
      lNewList.AddObject('Three', self);
      var lArray := lNewList.ToObjectArray;

      Assert.AreEqual(lArray.length, 3);
      Assert.AreEqual(lArray[1], fList);
    end;

    method CommaTests;
    begin
      var lNewList := new TStringList;
      lNewList.AddObject('One', lNewList);
      lNewList.AddObject('Two', fList);
      lNewList.AddObject('Three', self);

      Assert.AreEqual(lNewList.CommaText, 'One,Two,Three');      
    end;

    method DelimitedText;
    begin
      var lNewList := new TStringList;
      lNewList.AddObject('One', lNewList);
      lNewList.AddObject('Two', fList);
      lNewList.AddObject('Three', self);

      lNewList.Delimiter := ':';
      Assert.AreEqual(lNewList.DelimitedText, 'One:Two:Three');      
    end;

    method NamesTests;
    begin
      Assert.AreEqual(fNewList.Names[0], 'Ferrari');
      Assert.AreEqual(fNewList.Names[2], 'Lamborghini');
    end;

    method KeyNamesTests;
    begin
      var lNewList := new TStringList;
      lNewList.AddPair('Ferrari', '488');
      lNewList.AddPair('Porsche', '911');
      lNewList.AddPair('Lamborghini', 'Huracan');
      lNewList.Add('no key value');

      Assert.AreEqual(lNewList.KeyNames[0], 'Ferrari');
      Assert.AreEqual(lNewList.KeyNames[3], 'no key value');
    end;

    method ValuesTests;
    begin
      Assert.AreEqual(fNewList.Values['Ferrari'], '488');
      Assert.AreEqual(fNewList.Values['Lamborghini'], 'Huracan');
    end;

    method ValuesFromIndexTests;
    begin
      Assert.AreEqual(fNewList.ValueFromIndex[0], '488');
      Assert.AreEqual(fNewList.ValueFromIndex[2], 'Huracan');
    end;
  end;
end.
