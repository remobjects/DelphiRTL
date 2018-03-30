namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

{$IF NOT TOFFEE}
type
  ListUsage = public class(Test)
  private
    fList: TList<DelphiString>;
  public
    method Setup; override;
    begin
      fList := new TList<DelphiString>;
    end;

    method AddTests;
    begin
      fList.Add('string 1');
      fList.Add('string 2');
      Assert.AreEqual(fList.Count, 2);
      Assert.AreEqual(fList[1], 'string 2');
    end;

    {$IF ECHOES} // TODO check error for other platforms
    method AddRangeTests;
    begin
      var lArray := new DelphiString[2];
      lArray[0] := 'Ferrari';
      lArray[1] := 'Tesla';
      fList.AddRange(lArray);
      Assert.AreEqual(fList.Count, 2);
      Assert.AreEqual(fList[1], 'Tesla');

      var lAnList := new TList<DelphiString>;
      lAnList.Add('From another list 1');
      lAnList.Add('From another list 2');
      lAnList.Add('From another list 3');
      fList.AddRange(lAnList);
      Assert.AreEqual(fList.Count, 5);
      Assert.AreEqual(fList[4], 'From another list 3');
    end;
    {$ENDIF}

    method InsertTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');
      fList.Insert(1, 'String 1.5');
      Assert.AreEqual(fList.Count, 3);
      Assert.AreEqual(fList[1], 'String 1.5');
    end;

    {$IF ECHOES}
    method InsertRangeTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');

      var lArray := new DelphiString[2];
      lArray[0] := 'Ferrari';
      lArray[1] := 'Tesla';
      fList.InsertRange(1, lArray);
      Assert.AreEqual(fList.Count, 4);
      Assert.AreEqual(fList[1], 'Ferrari');

      var lAnList := new TList<DelphiString>;
      lAnList.Add('From another list 1');
      lAnList.Add('From another list 2');
      lAnList.Add('From another list 3');
      fList.InsertRange(2, lAnList);
      Assert.AreEqual(fList.Count, 7);
      Assert.AreEqual(fList[2], 'From another list 1');
    end;
    {$ENDIF}

    method RemoveTests;
    begin
      var lStr: DelphiString := 'String 1';
      fList.Add(lStr);
      fList.Add('String 2');
      fList.Remove(lStr);
      Assert.AreEqual(fList.Count, 1);
      Assert.AreEqual(fList[0], 'String 2');
    end;

    method RemoveItemTests;
    begin
      var lStr: DelphiString := 'String 1';
      fList.Add(lStr);
      fList.Add('String 2');
      fList.Add(lStr);
      fList.RemoveItem(lStr, TDirection.FromEnd);
      Assert.AreEqual(fList.Count, 2);
      Assert.AreEqual(fList[1], 'String 2');

      fList.RemoveItem(lStr, TDirection.FromBeginning);
      Assert.AreEqual(fList.Count, 1);
      Assert.AreEqual(fList[0], 'String 2');
    end;

    method DeleteTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');
      fList.Delete(1);
      Assert.AreEqual(fList.Count, 1);
      var lItem: DelphiString := fList[0];
      Assert.AreEqual(lItem, DelphiString('String 1'));

      fList.Delete(0);
      Assert.AreEqual(fList.Count, 0);
    end;

    method DeleteRangeTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');
      fList.Add('String 3');
      fList.Add('String 4');
      fList.Add('String 5');
      fList.DeleteRange(1, 2);
      Assert.AreEqual(fList.Count, 3);
      Assert.AreEqual(fList[1], 'String 4');
      Assert.AreEqual(fList[2], 'String 5');
    end;

    method ExtractItemTests;
    begin
      var lStr: DelphiString := 'String 1';
      fList.Add(lStr);
      fList.Add('String 2');
      var lStr2 := fList.ExtractItem(lStr, TDirection.FromBeginning);
      Assert.AreEqual(fList.Count, 1);
      Assert.AreEqual(lStr2, lStr);
    end;

    method ExtractTests;
    begin
      var lStr: DelphiString := 'String 1';
      fList.Add(lStr);
      fList.Add('String 2');
      var lStr2 := fList.Extract(lStr);
      Assert.AreEqual(fList.Count, 1);
      Assert.AreEqual(lStr2, lStr);
    end;

    method ExchangeTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');
      fList.Add('String 3');
      fList.Exchange(0, 2);
      var lItem: DelphiString := fList[2];
      Assert.AreEqual(fList[2], 'String 1');
      lItem := fList[0];
      Assert.AreEqual(lItem, 'String 3');
    end;

    method MoveTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');
      fList.Add('String 3');
      fList.Move(0, 2);
      Assert.AreEqual(fList[1], 'String 1');
      Assert.AreEqual(fList[2], 'String 3');
      fList.Clear;

      fList.Add('String 1');
      fList.Add('String 2');
      fList.Add('String 3');
      fList.Move(2, 0);
      Assert.AreEqual(fList[0], 'String 3');
      Assert.AreEqual(fList[1], 'String 1');
    end;

    method FirstTests;
    begin
      fList.Add('Ferrari');
      fList.Add('Porsche');
      fList.Add('Tesla');
      Assert.AreEqual(fList.First, 'Ferrari');
    end;

    method LastTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');
      fList.Add('String 3');
      Assert.AreEqual(fList.Last, 'String 3');
    end;

    method ClearTests;
    begin
      fList.Add('String 1');
      fList.Add('String 2');
      fList.Add('String 3');
      fList.Clear;
      Assert.AreEqual(fList.Count, 0);
    end;

    method ContainsTests;
    begin
      fList.Add('Ferrari');
      fList.Add('Porsche');
      fList.Add('Tesla');

      Assert.AreEqual(fList.Contains('Porsche'), true);
      Assert.AreEqual(fList.Contains('Ford'), false);
    end;

    method IndexOfTests;
    begin
      fList.Add('Ferrari');
      fList.Add('Porsche');
      fList.Add('Tesla');

      Assert.AreEqual(fList.IndexOf('Porsche'), 1);
      Assert.AreEqual(fList.IndexOf('Ford'), -1);
    end;

    method IndexOfItemsTests;
    begin
      var lStr: DelphiString := 'Ferrari';
      fList.Add(lStr);
      fList.Add('Porsche');
      fList.Add('Tesla');
      fList.Add(lStr);

      Assert.AreEqual(fList.IndexOfItem(lStr, TDirection.FromEnd), 3);
      Assert.AreEqual(fList.IndexOfItem(lStr, TDirection.FromBeginning), 0);
    end;

    method LastIndexOfTests;
    begin
      var lStr: DelphiString := 'Ferrari';
      fList.Add(lStr);
      fList.Add('Porsche');
      fList.Add('Tesla');
      fList.Add(lStr);

      Assert.AreEqual(fList.LastIndexOf(lStr), 3);
      fList.Delete(3);
      Assert.AreEqual(fList.LastIndexOf(lStr), 0);
    end;

    method ReverseTests;
    begin
      fList.Add('Ferrari');
      fList.Add('Porsche');
      fList.Add('Tesla');

      fList.Reverse;
      var lStr: DelphiString := fList[0];
      Assert.AreEqual(lStr, 'Tesla');
      lStr := fList[1];
      Assert.AreEqual(lStr, 'Porsche');
      lStr := fList[2];
      Assert.AreEqual(lStr, 'Ferrari');
    end;

/*    method ToArrayTests;
    begin
      fList.Add('Ferrari');
      fList.Add('Porsche');
      fList.Add('Tesla');
      var lArray := fList.ToArray;
      Assert.AreEqual(lArray[0], 'Ferrari');
      Assert.AreEqual(lArray[1], 'Porsche');
      Assert.AreEqual(lArray[2], 'Tesla');
    end;*/

    method ItemsTests;
    begin
      fList.Add('Ferrari');
      fList.Add('Porsche');
      fList.Add('Tesla');

      Assert.AreEqual(fList[1], 'Porsche');
      fList[1] := 'Renault';
      Assert.AreEqual(fList[1], 'Renault');
    end;
  end;
{$ENDIF}
end.