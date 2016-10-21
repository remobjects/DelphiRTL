namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  Elements.RTL.Delphi;

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
      lTmp.Add(('One', 1));
      lTmp.Add(('Two', 2));
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
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      Assert.AreEqual(fDict.Count, 2);
      fDict.Remove('One');
      Assert.AreEqual(fDict.Count, 1);
      fDict.Remove('One');
      Assert.AreEqual(fDict.Count, 1);
      fDict.Remove('Two');
      Assert.AreEqual(fDict.Count, 0);
    end;

    method ExtractPairTests;
    begin
      fDict.Add('One', 1);
      fDict.Add('Two', 2);
      var lItem := fDict.ExtractPair('One');
      Assert.AreEqual(lItem[0], 'One');
      Assert.AreEqual(lItem[1], 1);      
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
      var lItem := lArray[0];
      Assert.AreEqual(lItem[0], 'One');
      Assert.AreEqual(lItem[1], 1);
      lItem := lArray[1];
      Assert.AreEqual(lItem[0], 'Two');
      Assert.AreEqual(lItem[1], 2);
    end;
  end;

end.
