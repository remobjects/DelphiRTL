namespace Elements.RTL.Delphi;

interface

type
  TStack<T> = public class(TEnumerable<T>)
  private
    fStack: Sugar.Collections.Stack<T>;
    method GetCapacity: Integer;
    method SetCapacity(aValue: Integer);
    method Initialize;
  protected
    method &Notify(const Item: T; Action: TCollectionNotification); virtual;
    method GetSequence: ISequence<T>; override;
  public
    constructor;
    constructor(const Collection: TEnumerable<T>);
    class method Create: TStack<T>;
    class method Create(const Collection: TEnumerable<T>): TStack<T>;
    method Clear;
    method Push(const Value: T);
    method Pop: T;
    method Peek: T;
    method Extract: T; inline;
    method TrimExcess; inline;
    method ToArray: array of T; override;
    property Count: Integer read fStack.Count;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T>;
  end;

implementation

method TStack<T>.GetCapacity: Integer;
begin
  result := fStack.Count;
end;

method TStack<T>.Notify(Item: T; Action: TCollectionNotification);
begin
 if OnNotify <> nil then
   OnNotify(self, Item, Action);
end;

method TStack<T>.GetSequence: ISequence<T>;
begin
  //result := fStack;
end;

constructor TStack<T>;
begin
  Initialize;
end;

constructor TStack<T>(Collection: TEnumerable<T>);
begin
  Initialize;
  for lItem in Collection do
    Push(lItem);
end;

class method TStack<T>.Create: TStack<T>;
begin
  result := new TStack<T>;
end;

class method TStack<T>.Create(Collection: TEnumerable<T>): TStack<T>;
begin
  result := new TStack<T>(Collection);
end;

method TStack<T>.Clear;
begin
  var lArray := ToArray;
  fStack.Clear;

  for lItem in lArray do
    &&Notify(lItem, TCollectionNotification.cnRemoved);
end;

method TStack<T>.Push(Value: T);
begin
  fStack.Push(Value);
  &&Notify(Value, TCollectionNotification.cnAdded);
end;

method TStack<T>.Pop: T;
begin
  result := fStack.Pop;
  &Notify(result, TCollectionNotification.cnRemoved);
end;

method TStack<T>.Peek: T;
begin
  result := fStack.Peek;
end;

method TStack<T>.Extract: T;
begin
  result := Pop;
end;

method TStack<T>.TrimExcess;
begin
  // NO OP
end;

method TStack<T>.SetCapacity(aValue: Integer);
begin
  // NO OP
end;

method TStack<T>.ToArray: array of T;
begin
  result := fStack.ToArray;
end;

method TStack<T>.Initialize;
begin
  fStack := new Sugar.Collections.Stack<T>;
end;

end.
