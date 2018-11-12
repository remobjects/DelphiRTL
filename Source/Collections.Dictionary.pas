namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL;

type
  TPair<T, U> = public KeyValuePair<T, U>;
  TDictionary<TKey,TValue> = public class(TEnumerable<TPair<TKey,TValue>>)
    where TKey is class, TValue is class;
  private
    fDict: Dictionary<TKey, TValue>;
    method GetItem(const aKey: TKey): TValue;
    method SetItem(const aKey: TKey; aValue: TValue);
    method GetKeys: ISequence<TKey>;
    method GetValues: ISequence<TValue>;
    method Initialize(aCapacity: Integer := 0);
    method AddCollection(aCollection: TEnumerable<TPair<TKey,TValue>>);
    method DoRemove(aKey: TKey; aAction: TCollectionNotification): TValue;
  protected
    method KeyNotify(const Key: TKey; Action: TCollectionNotification); virtual;
    method ValueNotify(const Value: TValue; Action: TCollectionNotification); virtual;
    method GetSequence: ISequence<TPair<TKey,TValue>>; override; iterator;

  public
    {$IF ISLAND}
    method GetEnumerator: IEnumerator<TPair<TKey,TValue>>; override;
    {$ENDIF}
    constructor(aCapacity: Integer := 0);
    constructor(const Collection: TEnumerable<TPair<TKey,TValue>>);

    class method Create(aCapacity: Integer := 0): TDictionary<TKey,TValue>;
    class method Create(const aComparer: IEqualityComparer<TKey>): TDictionary<TKey,TValue>;
    class method Create(aCapacity: Integer; const aComparer: IEqualityComparer<TKey>): TDictionary<TKey,TValue>;
    class method Create(const Collection: TEnumerable<TPair<TKey,TValue>>): TDictionary<TKey,TValue>;
    class method Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const aComparer: IEqualityComparer<TKey>): TDictionary<TKey,TValue>;

    method &Add(const Key: TKey; const Value: TValue);
    method &Remove(const Key: TKey);
    method ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    method Clear;
    method TrimExcess;
    method TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    method AddOrSetValue(const Key: TKey; const Value: TValue);
    method ContainsKey(const Key: TKey): Boolean;
    method ContainsValue(const Value: TValue): Boolean;
    method ToArray: array of TPair<TKey,TValue>; override;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read fDict.Count;

    property Keys: ISequence<TKey> read GetKeys;
    property Values: ISequence<TValue> read GetValues;
    property OnKeyNotify: TCollectionNotifyEvent<TKey>;
    property OnValueNotify: TCollectionNotifyEvent<TValue>;
  end;

implementation

method TDictionary<TKey,TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  if OnKeyNotify <> nil then
    OnKeyNotify(self, Key, Action);
end;

method TDictionary<TKey,TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  if OnValueNotify <> nil then
    OnValueNotify(self, Value, Action);
end;

method TDictionary<TKey,TValue>.GetSequence: ISequence<TPair<TKey,TValue>>;
begin
  for each lKey in fDict.Keys do
    yield new KeyValuePair<TKey,TValue>(lKey, fDict.Item[lKey]);
end;

{$IF ISLAND}
method TDictionary<TKey,TValue>.GetEnumerator: IEnumerator<TPair<TKey,TValue>>;
begin
  result := fDict.GetSequence.GetEnumerator;
end;
{$ENDIF}

constructor TDictionary<TKey,TValue>(aCapacity: Integer := 0);
begin
  Initialize(aCapacity);
end;

constructor TDictionary<TKey,TValue>(const Collection: TEnumerable<TPair<TKey,TValue>>);
begin
  Initialize;
  AddCollection(Collection);
end;

method TDictionary<TKey,TValue>.Add(const Key: TKey; const Value: TValue);
begin
  fDict.Add(Key, Value);
  KeyNotify(Key, TCollectionNotification.cnAdded);
  ValueNotify(Value, TCollectionNotification.cnAdded);
end;

method TDictionary<TKey,TValue>.DoRemove(aKey: TKey; aAction: TCollectionNotification): TValue;
begin
  if not TryGetValue(aKey, out result) then
    exit(default(TValue));
  fDict.Remove(aKey);
  KeyNotify(aKey, aAction);
  ValueNotify(result, aAction);
end;

method TDictionary<TKey,TValue>.Remove(const Key: TKey);
begin
  DoRemove(Key, TCollectionNotification.cnRemoved);
end;

method TDictionary<TKey,TValue>.ExtractPair(const Key: TKey): TPair<TKey,TValue>;
begin
  result := new KeyValuePair<TKey, TValue>(Key, DoRemove(Key, TCollectionNotification.cnExtracted));
end;

method TDictionary<TKey,TValue>.Clear;
begin
  var lArray := ToArray;
  fDict.RemoveAll;
  for lItem in lArray do begin
    KeyNotify(lItem.Key, TCollectionNotification.cnRemoved);
    ValueNotify(lItem.Value, TCollectionNotification.cnRemoved);
  end;
end;

method TDictionary<TKey,TValue>.TrimExcess;
begin
  // NO OP
end;

method TDictionary<TKey,TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
begin
  result := fDict.ContainsKey(Key);
  if result then
    Value := Items[Key];
end;

method TDictionary<TKey,TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);
begin
  if ContainsKey(Key) then begin
    var lOldValue := Items[Key];
    Items[Key] := Value;
    ValueNotify(lOldValue, TCollectionNotification.cnRemoved);
    ValueNotify(Value, TCollectionNotification.cnAdded);
  end
  else
    &Add(Key, Value);
end;

method TDictionary<TKey,TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  result := fDict.ContainsKey(Key);
end;

method TDictionary<TKey,TValue>.ContainsValue(const Value: TValue): Boolean;
begin
  result := fDict.ContainsValue(Value);
end;

method TDictionary<TKey,TValue>.ToArray: array of TPair<TKey,TValue>;
begin
  result := new TPair<TKey,TValue>[fDict.Count];
  var i := 0;
  for each lKey in fDict.Keys do begin
    result[i] := new KeyValuePair<TKey, TValue>(lKey, fDict.Item[lKey]);
    inc(i);
  end;
end;

method TDictionary<TKey,TValue>.GetItem(const aKey: TKey): TValue;
begin
  result := fDict.Item[aKey];
end;

method TDictionary <TKey,TValue>.SetItem(const aKey: TKey; aValue: TValue);
begin
  fDict[aKey] := aValue;
end;

method TDictionary<TKey,TValue>.GetKeys: ISequence<TKey>;
begin
  result := fDict.Keys;
end;

method TDictionary<TKey,TValue>.GetValues: ISequence<TValue>;
begin
  result := fDict.Values;
end;

method TDictionary<TKey,TValue>.Initialize(aCapacity: Integer);
begin
  fDict := new Dictionary<TKey,TValue>(aCapacity);
end;

method TDictionary<TKey,TValue>.AddCollection(aCollection: TEnumerable<TPair<TKey,TValue>>);
begin
  for lItem in aCollection do
    AddOrSetValue(lItem.Key, lItem.Value);
end;

class method TDictionary<TKey,TValue>.Create(aCapacity: Integer := 0): TDictionary<TKey,TValue>;
begin
  result := new TDictionary<TKey,TValue>(aCapacity);
end;

class method TDictionary<TKey,TValue>.Create(const aComparer: IEqualityComparer<TKey>): TDictionary<TKey,TValue>;
begin
  result := new TDictionary<TKey,TValue>(0);
end;

class method TDictionary<TKey,TValue>.Create(aCapacity: Integer; const aComparer: IEqualityComparer<TKey>): TDictionary<TKey,TValue>;
begin
  result := new TDictionary<TKey,TValue>(aCapacity);
end;

class method TDictionary<TKey,TValue>.Create(const Collection: TEnumerable<TPair<TKey,TValue>>): TDictionary<TKey,TValue>;
begin
  result := new TDictionary<TKey,TValue>(Collection);
end;

class method TDictionary<TKey,TValue>.Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const aComparer: IEqualityComparer<TKey>): TDictionary<TKey,TValue>;
begin
  result := new TDictionary<TKey,TValue>(Collection);
end;

end.