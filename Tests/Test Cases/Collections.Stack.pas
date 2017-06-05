namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

{$IF NOT TOFFEE}
type
  StackUsage = public class(Test)
  private
    fStack: TStack<DelphiString>;
  public
    method Setup; override;
    begin
      fStack := new TStack<DelphiString>;
    end;

    method PushTests;
    begin
      fStack.Push('One String');
      Assert.AreEqual(fStack.Count, 1);
      fStack.Push('Another string');
      Assert.AreEqual(fStack.Count, 2);
    end;

    method PopTests;
    begin
      fStack.Push('One String');
      fStack.Push('Another string');
      var lItem := fStack.Pop;
      Assert.AreEqual(fStack.Count, 1);
      Assert.AreEqual(lItem, 'Another string');
      fStack.Pop;
      Assert.AreEqual(fStack.Count, 0);
    end;

    method ClearTests;
    begin
      fStack.Push('One string');
      fStack.Push('Another string');
      Assert.AreEqual(fStack.Count, 2);
      fStack.Clear;
      Assert.AreEqual(fStack.Count, 0);
    end;

    method PeekTests;
    begin
      fStack.Push('One string');
      fStack.Push('Another string');
      var lItem: DelphiString := fStack.Peek;
      Assert.AreEqual(fStack.Count, 2);
      Assert.AreEqual(lItem, 'Another string');
    end;

/*
    method ToArrayTests;
    begin
      fStack.Push('One string');
      fStack.Push('Another string');

      var lArray := fStack.ToArray;
      Assert.AreEqual(lArray[0], 'Another string');
      Assert.AreEqual(lArray[1], 'One string');
    end;
*/

    {$IF ECHOES OR COOPER} // TODO review for TOFFEE
    method CreateTests;
    begin
      var lTmp := new TList<DelphiString>;
      lTmp.Add('One string');
      lTmp.Add('Another string');
      var lStack := new TStack<DelphiString>(lTmp);
      Assert.AreEqual(lStack.Count, 2);
    end;
    {$ENDIF}
  end;
{$ENDIF}
end.