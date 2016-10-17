namespace Elements.RTL.Delphi;

interface

uses
  Sugar,
  Sugar.Linq;

type
  TArray<T> = class
  end;

  IComparer<T> = interface
    function Compare(const Left, Right: T): Integer;
  end;

  IEqualityComparer<T> = public interface
    function &Equals(const Left, Right: T): Boolean;
    function GetHashCode(const Value: T): Integer;
  end;

  //IEnumerable = public ISequence<Object>;
  IEnumerable<T> = public ISequence<T>;

  TEnumerable<T> = public abstract class(ISequence<T>)
  private
    method ToArrayImpl(Count: Integer): TArray<T>;
  protected

    method GetSequence: ISequence<T>; virtual; abstract;

    // Temporary workaround for 74077: Allow GetSequence() to actually be used to implement ISequence
    {$IF COOPER}
    method &iterator: java.util.Iterator<T>;
    begin
      result := Iterable<T>(GetSequence()).iterator;
    end;
    {$ELSEIF ECHOES}
    method GetNonGenericEnumerator: System.Collections.IEnumerator; implements System.Collections.IEnumerable.GetEnumerator;
    begin
      result := GetEnumerator();
    end;
    method GetEnumerator: System.Collections.Generic.IEnumerator<T>; //implements System.Collections.Generic.IEnumerable<T>.GetEnumerator<T>;
    begin  
      exit System.Collections.Generic.IEnumerable<T>(GetSequence()).GetEnumerator;
    end;
    {$ELSEIF TOFFEE}
    method countByEnumeratingWithState(aState: ^NSFastEnumerationState) objects(aStackbuf: ^T) count(len: NSUInteger): NSUInteger;
    begin
      var currentSequence: NSArray<T>;
      if aState^.state = 0 then begin
        currentSequence := GetSequence().array;
        aState^.extra[0] := NSInteger(bridge<CFArrayRef>(currentSequence));
      end
      else begin
        currentSequence := bridge<NSArray>(^Void(aState^.extra[0]));
      end;

      var i := 0;
      var count := currentSequence.count;
      while (aState^.state+i < count) and (i < len) do begin
        aStackbuf[i] := currentSequence[aState^.state+i];
        inc(i);
      end;
      
      aState^.state := aState^.state+i;
      aState^.itemsPtr := ^id(aStackbuf);
      result := i;
    end;
    {$ENDIF}

  public
    //destructor Destroy; override;
    //method GetEnumerator: TEnumerator<T>;
    method ToArray: array of T; virtual;
  end;

  arrayofT<T> = array of T;
  TDirection = public enum (FromBeginning, FromEnd) of Integer;
  TEmptyFunc<T> = public block (const L, R: T): Boolean;
  TCollectionNotification = public enum (cnAdded, cnRemoved, cnExtracted) of Integer;
  TCollectionNotifyEvent<T> = public block(Sender: TObject; const Item: T; Action: TCollectionNotification);
 
  TList<T> = class(TEnumerable<T>)
  private
    fList: Sugar.Collections.List<T>;
    fComparer: IComparer<T>;
    fOnNotify: TCollectionNotifyEvent<T>;
    method GetItems: array of T;
    method SetCapacity(Value: Integer);
    method SetCount(Value: Integer);
    method GetItem(aIndex: Integer): T;
    method SetItem(aIndex: Integer; Value: T);
    method Initialize;
  protected
    method ItemValue(const Item: T): Integer;
    method GetSequence: ISequence<T>; override;
    method &Notify(const Item: T; Action: TCollectionNotification); virtual;
  public
    constructor; 
    constructor(const aComparer: IComparer<T>);
    constructor(const aCollection: TEnumerable<T>);
    class method Create: TList<T>;
    class method Create(const AComparer: IComparer<T>): TList<T>;
    class method Create(const Collection: TEnumerable<T>): TList<T>;
    class method Error(const Msg: String; Data: Integer); virtual;
    method &Add(const Value: T): Integer;
    method AddRange(const Values: array of T);
    method AddRange(const Collection: IEnumerable<T>);
    method AddRange(const Collection: TEnumerable<T>); 
    method Insert(aIndex: Integer; const Value: T);
    method InsertRange(aIndex: Integer; const Values: array of T);
    method InsertRange(aIndex: Integer; const Collection: IEnumerable<T>);
    //method InsertRange(aIndex: Integer; const Collection: TEnumerable<T>);  what;'s the point? TEnumerable<T> is IEnumerable<T>
    method Pack;
    method Pack(const IsEmpty: TEmptyFunc<T>);
    method &Remove(const Value: T): Integer; inline;
    method RemoveItem(const Value: T; Direction: TDirection): Integer;
    method Delete(aIndex: Integer);
    method DeleteRange(aIndex, aCount: Integer);
    method ExtractItem(const Value: T; Direction: TDirection): T;
    method Extract(const Value: T): T; inline;
    method Exchange(Index1, Index2: Integer); 
    method Move(CurIndex, NewIndex: Integer);
    method First: T;
    method Last: T;
    method Clear;
    method Expand: TList<T>;
    method Contains(const Value: T): Boolean;
    method IndexOf(const Value: T): Integer;
    method IndexOfItem(const Value: T; Direction: TDirection): Integer;
    method LastIndexOf(const Value: T): Integer;
    method &Reverse;
    method Sort;
    method Sort(const AComparer: IComparer<T>);
    method BinarySearch(const Item: T; out Index: Integer): Boolean;
    method BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean;
    method TrimExcess;
    method ToArray: array of T; override; final;
    property Capacity: Integer read fList.Count write SetCapacity;
    property Count: Integer read fList.Count write SetCount;
    property Items[aIndex: Integer]: T read GetItem write SetItem; default;
    property List: array of T read GetItems;

    property OnNotify: TCollectionNotifyEvent<T> read fOnNotify write fOnNotify;
  end;


implementation

method TEnumerable<T>.ToArray: array of T;
begin
  {$IF COOPER}
  result := (ToList() as Sugar.Collections.List<T>).ToArray();
  {$ELSE}
  result := ToList().ToArray();
  {$ENDIF}
end;

method TEnumerable<T>.ToArrayImpl(Count: Integer): TArray<T>;
begin
  
end;

method TList<T>.ItemValue(Item: T): Integer;
begin

end;

method TList<T>.GetSequence: ISequence<T>;
begin
  result := fList;
end;

method TList<T>.&Notify(Item: T; Action: TCollectionNotification);
begin
  if fOnNotify <> nil then
    fOnNotify(Self, Item, Action);
end;

constructor TList<T>;
begin
  Initialize;
end;

constructor TList<T>(aComparer: IComparer<T>);
begin
  Initialize;
  fComparer := aComparer;
end;

constructor TList<T>(aCollection: TEnumerable<T>);
begin
  Initialize;
  AddRange(aCollection);
end;

class method TList<T>.Error(Msg: String; Data: Integer);
begin

end;

method TList<T>.Add(Value: T): Integer;
begin
  fList.Add(Value);
  &Notify(Value, TCollectionNotification.cnAdded);
  result := fList.Count - 1;
end;

method TList<T>.AddRange(Values: array of T);
begin
  fList.AddRange(Values);
end;

method TList<T>.AddRange(Collection: IEnumerable<T>);
begin
  InsertRange(fList.Count, Collection);  
end;

method TList<T>.AddRange(Collection: TEnumerable<T>);
begin
  InsertRange(fList.Count, Collection);  
end;

method TList<T>.Insert(aIndex: Integer; Value: T);
begin
  fList.Insert(aIndex, Value);
end;

method TList<T>.InsertRange(aIndex: Integer; Values: array of T);
begin
  fList.InsertRange(aIndex, Values);
end;

method TList<T>.InsertRange(aIndex: Integer; Collection: IEnumerable<T>);
begin
  var lList := Collection.ToList; // we don't want to enum it twice?
  for each i in lList do begin
    fList.Insert(aIndex, i);
    inc(aIndex);
  end;
  for each i in lList do begin
     &Notify(i, TCollectionNotification.cnAdded);
  end;
end;

method TList<T>.Pack;
begin
  // NO OP
end;

method TList<T>.Pack(IsEmpty: TEmptyFunc<T>);
begin
  // NO OP
end;

method TList<T>.Remove(Value: T): Integer;
begin
  result := RemoveItem(Value, TDirection.FromBeginning);
end;

method TList<T>.RemoveItem(Value: T; Direction: TDirection): Integer;
begin
  result := IndexOfItem(Value, Direction);
  if result >= 0 then begin
    fList.RemoveAt(result);
    &Notify(Value, TCollectionNotification.cnRemoved);
  end;
end;

method TList<T>.Delete(aIndex: Integer);
begin
  var lOldItem := fList[aIndex];
  fList.RemoveAt(aIndex);  
  &Notify(lOldItem, TCollectionNotification.cnRemoved);
end;

method TList<T>.DeleteRange(aIndex: Integer; aCount: Integer);
begin
  var lTmp: Sugar.Collections.List<T>;
  for i: Integer := 0 to aCount - 1 do
    lTmp.add(fList[aIndex + i]);

  fList.RemoveRange(aIndex, aCount);

  for lItem in lTmp do
    &Notify(lItem, TCollectionNotification.cnRemoved);
end;

method TList<T>.ExtractItem(Value: T; Direction: TDirection): T;
begin
  var lIndex := IndexOfItem(Value, Direction);
  if lIndex >= 0 then begin
    result := fList[lIndex];
    fList.RemoveAt(lIndex);
    &Notify(result, TCollectionNotification.cnExtracted);
  end
  else
    result := nil;
end;

method TList<T>.Extract(Value: T): T;
begin
  result := ExtractItem(Value, TDirection.FromBeginning);
end;

method TList<T>.Exchange(Index1: Integer; Index2: Integer);
begin
  var lTmp: T;
  lTmp := fList[Index1];
  fList[Index1] := fList[Index2];
  fList[Index2] := lTmp;
end;

method TList<T>.Move(CurIndex: Integer; NewIndex: Integer);
begin
  fList.Insert(NewIndex, fList[CurIndex]);
  if NewIndex <= CurIndex then 
    inc(CurIndex);
  fList.RemoveAt(CurIndex);
end;

method TList<T>.First: T;
begin
  result := fList[0];
end;

method TList<T>.Last: T;
begin
  result := fList[fList.Count - 1];
end;

method TList<T>.Clear;
begin
  fList.Clear;
end;

method TList<T>.Expand: TList<T>;
begin
  // NO OP
end;

method TList<T>.Contains(Value: T): Boolean;
begin
  result := fList.Contains(Value);
end;

method TList<T>.IndexOf(Value: T): Integer;
begin
  result := fList.IndexOf(Value);
end;

method TList<T>.IndexOfItem(Value: T; Direction: TDirection): Integer;
begin
  result := if Direction = TDirection.FromBeginning then fList.IndexOf(Value) else fList.LastIndexOf(Value);
end;

method TList<T>.LastIndexOf(Value: T): Integer;
begin
  result := fList.LastIndexOf(Value);
end;

method TList<T>.Reverse;
begin
  var lTmp := new Sugar.Collections.List<T>;
  for i: Integer := Count - 1 downto 0 do
    lTmp[Count - 1 - i];
  fList := lTmp;
end;

method TList<T>.Sort;
begin
  
end;

method TList<T>.Sort(AComparer: IComparer<T>);
begin

end;

method TList<T>.BinarySearch(Item: T; out &Index: Integer): Boolean;
begin

end;

method TList<T>.BinarySearch(Item: T; out &Index: Integer; AComparer: IComparer<T>): Boolean;
begin

end;

method TList<T>.TrimExcess;
begin
  // NO OP
end;

method TList<T>.ToArray: array of T;
begin
  result := fList.ToArray;
end;

method TList<T>.SetCapacity(Value: Integer);
begin
  // NO OP, for compatibility
end;

method TList<T>.SetCount(Value: Integer);
begin
  // NO OP, for compatibility
end;

method TList<T>.GetItem(aIndex: Integer): T;
begin
  result := fList[aIndex];
end;

method TList<T>.SetItem(aIndex: Integer; Value: T);
begin
  var lOldItem := fList[aIndex];
  fList[aIndex] := Value;
  &Notify(lOldItem, TCollectionNotification.cnRemoved);
  &Notify(Value, TCollectionNotification.cnAdded);
end;

class method TList<T>.Create: TList<T>;
begin
  result := new TList<T>;
end;

class method TList<T>.Create(AComparer: IComparer<T>): TList<T>;
begin
  result := new TList<T>(AComparer);
end;

class method TList<T>.Create(Collection: TEnumerable<T>): TList<T>;
begin
  result := new TList<T>(Collection);
end;

method TList<T>.GetItems: array of T;
begin
  result := fList.ToArray;
end;

method TList<T>.Initialize;
begin
  fList := new Sugar.Collections.List<T>;
end;

end.
