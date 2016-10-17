namespace Elements.RTL.Delphi;

interface

type
  TPair<TKey, TValue> = tuple of (TKey, TValue);
  TDictionary<TKey,TValue> = public class(TEnumerable<TPair<TKey,TValue>>)
  private
    fDict: Sugar.Collections.Dictionary<TKey, TValue>;
    method GetItem(aKey: TKey): TValue;
    method SetItem(aKey: TKey; aValue: TValue);
    method GetKeys: ISequence<TKey>;
    method GetValues: ISequence<TValue>;
    method Initialize;
  protected    
    method KeyNotify(const Key: TKey; Action: TCollectionNotification); virtual;
    method ValueNotify(const Value: TValue; Action: TCollectionNotification); virtual;
    method GetSequence: ISequence<TPair<TKey,TValue>>; override;
  public
    constructor(ACapacity: Integer := 0);
    constructor(const AComparer: IEqualityComparer<TKey>);
    constructor(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
    constructor(const Collection: TEnumerable<TPair<TKey,TValue>>);
    constructor(const Collection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey>);
    
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

method TDictionary<TKey,TValue>.KeyNotify(Key: TKey; Action: TCollectionNotification);
begin

end;

method TDictionary<TKey,TValue>.ValueNotify(Value: TValue; Action: TCollectionNotification);
begin

end;

method TDictionary<TKey,TValue>.GetSequence: ISequence<TPair<TKey,TValue>>;
begin

end;

constructor TDictionary<TKey,TValue>(ACapacity: Integer := 0);
begin

end;

constructor TDictionary<TKey,TValue>(AComparer: IEqualityComparer<TKey>);
begin

end;

constructor TDictionary<TKey,TValue>(ACapacity: Integer; AComparer: IEqualityComparer<TKey>);
begin

end;

constructor TDictionary<TKey,TValue>(Collection: TEnumerable<TPair<TKey,TValue>>);
begin

end;

constructor TDictionary<TKey,TValue>(Collection: TEnumerable<TPair<TKey,TValue>>; AComparer: IEqualityComparer<TKey>);
begin

end;

method TDictionary<TKey,TValue>.Add(Key: TKey; Value: TValue);
begin

end;

method TDictionary<TKey,TValue>.Remove(Key: TKey);
begin

end;

method TDictionary<TKey,TValue>.ExtractPair(Key: TKey): TPair<TKey,TValue>;
begin

end;

method TDictionary<TKey,TValue>.Clear;
begin

end;

method TDictionary<TKey,TValue>.TrimExcess;
begin

end;

method TDictionary<TKey,TValue>.TryGetValue(Key: TKey; out Value: TValue): Boolean;
begin

end;

method TDictionary<TKey,TValue>.AddOrSetValue(Key: TKey; Value: TValue);
begin

end;

method TDictionary<TKey,TValue>.ContainsKey(Key: TKey): Boolean;
begin

end;

method TDictionary<TKey,TValue>.ContainsValue(Value: TValue): Boolean;
begin

end;

method TDictionary<TKey,TValue>.ToArray: array of TPair<TKey,TValue>;
begin

end;

method TDictionary<TKey,TValue>.GetItem(aKey: TKey): TValue;
begin

end;

method TDictionary <TKey,TValue>.SetItem(aKey: TKey; aValue: TValue);
begin

end;

method TDictionary<TKey,TValue>.GetKeys: ISequence<TKey>;
begin

end;

method TDictionary<TKey,TValue>.GetValues: ISequence<TValue>;
begin

end;

method TDictionary<TKey,TValue>.Initialize;
begin
  fDict := new Sugar.Collections.Dictionary<TKey,TValue>;
end;


end.
