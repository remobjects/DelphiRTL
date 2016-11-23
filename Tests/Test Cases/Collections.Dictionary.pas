namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL;

{$IF COOPER OR ECHOES}
type
  DictionaryUsage = public class(Test)
  private
    fDict: TDictionary<DelphiString, Integer>;
  public
    method Setup; override;
    begin
      fDict := new TDictionary<DelphiString, Integer>;
    end;

    {$IF ECHOES OR COOPER} // TODO TOFFEE
    method CreateTests;
    begin
      var lTmp := new TList<TPair<DelphiString, Integer>>;
      var lPair := new Sugar.collections.KeyValuePair<DelphiString, Integer>('One', 1);
      lTmp.Add(lPair);
      var lPair2 := new Sugar.collections.KeyValuePair<DelphiString, Integer>('Two', 2);
      lTmp.Add(lPair2);
      var lDict := new TDictionary<DelphiString, Integer>(lTmp);
      Assert.AreEqual(lDict.Count, 2);
    end;
    {$ENDIF}

    method AddTests;
    begin
      fDict.Add('One', 1);
      Assert.AreEqual(fDict.Count, 1);
      fDict.Add('Two', 2);
      Assert.AreEqual(fDict.Count, 2);
    end;

    method RemoveTests;
    begin
      var lOne: DelphiString := 'One';
      var lTwo: DelphiString := 'Two';
      fDict.Add(lOne, 1);
      fDict.Add(lTwo, 2);
      Assert.AreEqual(fDict.Count, 2);
      fDict.Remove(lOne);
      Assert.AreEqual(fDict.Count, 1);
      fDict.Remove(lOne);
      Assert.AreEqual(fDict.Count, 1);
      fDict.Remove(lTwo);
      Assert.AreEqual(fDict.Count, 0);
    end;

    method ExtractPairTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      var lItem := fDict.ExtractPair('One');
      Assert.AreEqual(lItem.Key, 'One');
      Assert.AreEqual(lItem.Value, 1);      
    end;

    method ClearTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      fDict.Clear;
      Assert.AreEqual(fDict.Count, 0);
    end;

    {$IF ECHOES} // TODO check errors
    method TryGetValueTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      fDict.Add('Three', 3);
      var lValue: Integer;
      Assert.AreEqual(fDict.TryGetValue('Two', out lValue), true);
      Assert.AreEqual(lValue, 2);
      Assert.AreEqual(fDict.TryGetValue('Four', out lValue), false);
    end;
    {$ENDIF}

    method AddOrSetValueTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      fDict.AddOrSetValue('Three', 3);
      Assert.AreEqual(fDict.Count, 3);
      Assert.AreEqual(fDict['Three'], 3);
      fDict.AddOrSetValue('Two', 22);
      Assert.AreEqual(fDict.Count, 3);
      Assert.AreEqual(fDict['Two'], 22);
    end;

    method ContainsKeyTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      Assert.AreEqual(fDict.ContainsKey('One'), true);
      Assert.AreEqual(fDict.ContainsKey('Three'), false);
    end;

    method ContainsValueTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      Assert.AreEqual(fDict.ContainsValue(2), true);
      Assert.AreEqual(fDict.ContainsValue(4), false);
    end;

    method ToArrayTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      var lArray := fDict.ToArray;
      Assert.AreEqual(lArray.length, 2);
      var lOneFound := false;
      var lTwoFound := false;
      for lItem in lArray do begin
        if lItem.Key.CompareTo('One') = 0 then
        //if lItem.Key = 'One' then  // TODO error on cooper
          lOneFound := true;
        if lItem.Key.CompareTo('Two') = 0 then
          lTwoFound := true;
      end;
      Assert.AreEqual(lOneFound, true);
      Assert.AreEqual(lTwoFound, true);
    end;
  end;
{$ENDIF}
end.
