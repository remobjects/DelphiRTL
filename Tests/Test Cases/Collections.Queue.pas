namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

{$IF NOT TOFFEE}
type
  QueueUsage = public class(Test)
  private
    fQueue: TQueue<DelphiString>;
  public
    method Setup; override;
    begin
      fQueue := new TQueue<DelphiString>;
    end;

    {$IF ECHOES OR COOPER} // review TOFFEE
    method CreateTests;
    begin
      var lTmp := new TList<DelphiString>;
      lTmp.Add('One string');
      lTmp.Add('Another string');
      var lQueue := new TQueue<DelphiString>(lTmp);
      Assert.AreEqual(lQueue.Count, 2);
    end;
    {$ENDIF}

    method EnqueueTests;
    begin
      fQueue.Enqueue('a test');
      Assert.AreEqual(fQueue.Count, 1);
      fQueue.Enqueue('another test');
      Assert.AreEqual(fQueue.Count, 2);
    end;

    method DequeueTests;
    begin
      fQueue.Enqueue('a test');
      fQueue.Enqueue('another test');
      var lItem: DelphiString := fQueue.Dequeue;
      Assert.AreEqual(fQueue.Count, 1);
      Assert.AreEqual(lItem, 'a test');
      fQueue.Dequeue;
      Assert.AreEqual(fQueue.Count, 0);
    end;

    method PeekTests;
    begin
      fQueue.Enqueue('a test');
      fQueue.Enqueue('another test');
      var lItem := fQueue.Peek;
      Assert.AreEqual(fQueue.Count, 2);
      Assert.AreEqual(lItem, 'a test');
    end;

    method ClearTests;
    begin
      fQueue.Enqueue('a test');
      fQueue.Enqueue('another test');
      fQueue.Clear;
      Assert.AreEqual(fQueue.Count, 0);
    end;

/*    method ToArrayTests;
    begin
      fQueue.Enqueue('a test');
      fQueue.Enqueue('another test');
      var lArray := fQueue.ToArray;
      Assert.AreEqual(lArray[0], 'a test');
      Assert.AreEqual(lArray[1], 'another test');
    end;*/
  end;
{$ENDIF}
end.