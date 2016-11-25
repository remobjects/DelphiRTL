namespace RemObjects.Elements.RTL.Delphi;

interface

type
  TQueue<T> = public class(TEnumerable<T>){$IF TOFFEE}where T is class;{$ENDIF}
  private
    fQueue: Sugar.Collections.Queue<T>;
    method GetCapacity: Integer;
    method SetCapacity(aCount: Integer);
    method Initialize;
  protected
    method &Notify(const Item: T; Action: TCollectionNotification); virtual;
    method GetSequence: ISequence<T>; override; iterator;
  public
    constructor;
    constructor(Collection: TEnumerable<T>);
    class method Create: TQueue<T>;
    class method Create(const Collection: TEnumerable<T>): TQueue<T>;
    method Enqueue(const Value: T);
    method Dequeue: T;
    method Extract: T; inline;
    method Peek: T;
    method Clear;
    method TrimExcess; inline;
    property Count: Integer read fQueue.Count;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T>;
    method ToArray: array of T; override;
  end;

implementation

method TQueue<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  if OnNotify <> nil then
    OnNotify(self, Item, Action);
end;

method TQueue<T>.GetSequence: ISequence<T>;
begin
  var lArray := fQueue.ToArray;
  for lItem in lArray do
    yield lItem;
end;

constructor TQueue<T>;
begin
  Initialize;
end;

constructor TQueue<T>(Collection: TEnumerable<T>);
begin
  Initialize;
  for lItem in Collection do
    Enqueue(lItem);
end;

class method TQueue<T>.Create: TQueue<T>;
begin
  result := new TQueue<T>;
end;

class method TQueue<T>.Create(const Collection: TEnumerable<T>): TQueue<T>;
begin
  result := new TQueue<T>(Collection);
end;

method TQueue<T>.Enqueue(const Value: T);
begin
  fQueue.Enqueue(Value);
  &Notify(Value, TCollectionNotification.cnAdded);
end;

method TQueue<T>.Dequeue: T;
begin
  result := fQueue.Dequeue;
  &Notify(result, TCollectionNotification.cnRemoved);
end;

method TQueue<T>.Extract: T;
begin
  result := Dequeue;
end;

method TQueue<T>.Peek: T;
begin
  result := fQueue.Peek;
end;

method TQueue<T>.Clear;
begin
  var lArray := fQueue.ToArray;
  fQueue.Clear;
  for lItem in lArray do
    &Notify(lItem, TCollectionNotification.cnRemoved);
end;

method TQueue<T>.TrimExcess;
begin
  // NO OP
end;

method TQueue<T>.GetCapacity: Integer;
begin
  result := fQueue.Count;
end;

method TQueue<T>.SetCapacity(aCount: Integer);
begin
  // NO OP
end;

method TQueue<T>.ToArray: array of T;
begin
  raise new Exception("Not implemented.");
end;

method TQueue<T>.Initialize;
begin
  fQueue := new Sugar.Collections.Queue<T>;
end;


end.
