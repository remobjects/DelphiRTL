﻿namespace ProjectRCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TValueType = public enum(vaNull, vaList, vaInt8, vaInt16, vaInt32, vaExtended, vaString, vaIdent, vaFalse, vaTrue, vaBinary, vaSet, vaLString,
    vaNil, vaCollection, vaSingle, vaCurrency, vaDate, vaWString, vaInt64, vaUTF8String, vaDouble);

  TReaderProc = public block(Reader: TReader);
  TWriterProc = public block(Writer: TWriter);
  TStreamProc = public block(Stream: TStream);

  TFiler = public abstract class(TObject)
  private
    fRoot: TComponent;
    fAncestor: TPersistent;
    fIgnoreChildren: Boolean;
  protected
    fStream: TStream;
    method SetRoot(aValue: TComponent); virtual;
  public
    constructor(aStream: TStream; BufSize: Integer);
    //destructor Destroy; override;
    method DefineProperty(Name: String; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); virtual; abstract;
    method DefineBinaryProperty(Name: String; ReadData, WriteData: TStreamProc; HasData: Boolean); virtual; abstract;
    method FlushBuffer; virtual; abstract;
    property Root: TComponent read fRoot write SetRoot;
    property Ancestor: TPersistent read fAncestor write fAncestor;
    property IgnoreChildren: Boolean read fIgnoreChildren write fIgnoreChildren;
  end;

  TReader = public class(TFiler)
  private
    fParent: TComponent;
    fOwner: TComponent;
    method ReadComponentData(aInstance: TComponent);
  protected
    method ReadProperty(aInstance: TComponent /*TPersistent*/);
  public
    const FilerSignature: UInt32 = $30465054; // 'TPF0'
    method EndOfList: Boolean;
    method ReadComponent(aComponent: TComponent): TComponent;
    method ReadData(Instance: TComponent);
    method ReadPropValue(aValueType: TValueType; aProperty: PropertyInfo): Object;
    method ReadRootComponent(Root: TComponent): TComponent;
    method ReadSignature;
    method ReadStr: String;
    method ReadValue: TValueType;
    property Owner: TComponent read fOwner write fOwner;
  end;

  TWriter = public class(TObject)
  end;

  ComponentsHelper = public static class
  public
    class method CreateComponent(aClassName: String; aOwner: TComponent): TComponent;
    class method CreateComponent(aType: &Type; aOwner: TComponent): TComponent;
  end;

implementation

method TFiler.SetRoot(aValue: TComponent);
begin
  fRoot := aValue;
end;

constructor TFiler(aStream: TStream; BufSize: Integer);
begin
  fStream := aStream;
end;

method TReader.ReadComponentData(aInstance: TComponent);
begin
  while not EndOfList do ReadProperty(aInstance);
  ReadValue; // skip end of list
  while not EndOfList do ReadComponent(nil);
  ReadValue; // skip end of list
end;

method TReader.ReadPropValue(aValueType: TValueType; aProperty: PropertyInfo): Object;
begin
  var lValue: Integer;
  case aValueType of
    TValueType.vaInt8: begin
      fStream.ReadData(var lValue, sizeOf(Byte));
      exit Byte(lValue);
    end;

    TValueType.vaInt16: begin
      fStream.ReadData(var lValue, sizeOf(SmallInt));
      exit SmallInt(lValue);
    end;

    TValueType.vaInt32: begin
      fStream.ReadData(var lValue, sizeOf(Integer));
      exit lValue;
    end;

    TValueType.vaInt64: begin
      var lInt64: Int64;
      fStream.ReadData(var lInt64, sizeOf(Int64));
      exit lInt64;
    end;

    TValueType.vaString, TValueType.vaUTF8String, TValueType.vaLString: begin
      if aValueType = TValueType.vaLString then
        fStream.Read(var lValue, sizeOf(Integer))
      else
        fStream.ReadData(var lValue, sizeOf(Byte));

      var lBytes := new Byte[lValue];
      if aValueType = TValueType.vaUTF8String then
        exit Encoding.UTF8.GetString(lBytes)
      else
        exit Encoding.Default.GetString(lBytes);
    end;

    TValueType.vaWString: begin

    end;

    TValueType.vaIdent: begin
      fStream.ReadData(var lValue, sizeOf(Byte));
      var lBytes := new Byte[lValue];
      exit Encoding.UTF8.GetString(lBytes)
    end;
  end;
end;

method TReader.ReadProperty(aInstance: TComponent/*TPersistent*/);
begin
  var lName := ReadStr;
  var lValue := ReadValue;
  var lType := typeOf(aInstance);
  var lProperty := lType.Properties.Where(a -> (a.Name = lName)).FirstOrDefault;
  if lProperty = nil then raise new Exception('Can not get property ' + lName);
  var lPropValue := ReadPropValue(lValue, lProperty);
  lProperty.SetValue(aInstance, nil, lPropValue);
end;

method TReader.EndOfList: Boolean;
begin
  result := ReadValue = TValueType.vaNull;
  fStream.Position := fStream.Position - 1;
end;

method TReader.ReadRootComponent(Root: TComponent): TComponent;
begin
  ReadSignature; // Skip 'TPF0'
  ReadStr;
  Root.Name := ReadStr;
  fOwner := Root;
  fParent := Root;
  ReadComponentData(Root);
end;

method TReader.ReadData(Instance: TComponent);
begin

end;

method TReader.ReadComponent(aComponent: TComponent): TComponent;
begin
  var lClass := ReadStr;
  var lName := ReadStr;
  result := aComponent;
  if result = nil then begin
    result := ComponentsHelper.CreateComponent(lClass, fOwner);
    result.Name := lName;
    // TODO setParent!
  end;
  var lOldParent := fParent;
  fParent := result;
  ReadComponentData(Root);
  fParent := lOldParent;
end;

method TReader.ReadSignature;
begin
  var lSig: UInt32;
  fStream.ReadData(var lSig);
  if lSig <> FilerSignature then
    raise new Exception("Invalid dfm format");
end;

method TReader.ReadStr: String;
begin
  var lTotal: Byte;
  fStream.ReadData(var lTotal);
  var lArray := new Byte[lTotal];
  fStream.Read(var lArray, lTotal);
  result := Encoding.UTF8.GetString(lArray);
end;

method TReader.ReadValue: TValueType;
begin
  var lByte: Byte;
  fStream.ReadData(var lByte);
  result := TValueType(lByte);
end;

method ComponentsHelper.CreateComponent(aClassName: String; aOwner: TComponent): TComponent;
begin
  var lType := &Type.AllTypes.Where(a -> a.Name = aClassName).FirstOrDefault;
  if lType = nil then raise new Exception('Can not get ' + aClassName + ' type');
  result := CreateComponent(lType, aOwner);
end;

method ComponentsHelper.CreateComponent(aType: &Type; aOwner: TComponent): TComponent;
begin
  var lCtor: MethodInfo;
  var lCtors := aType.Methods.Where(a -> ((MethodFlags.Constructor in a.Flags) and (a.Arguments.Count = 1)));
  if lCtors.Count > 1 then begin
    for each lTemp in lCtors do begin
      var lArguments := lTemp.Arguments.ToList;
      if lArguments[0].Type = typeOf(TComponent) then begin
        lCtor := lTemp;
        break;
      end;
    end;
  end
  else
    lCtor := lCtors.FirstOrDefault;

  if lCtor = nil then raise new Exception('No default constructor could be found!');
  var lRealCtor := ComponentCtorHelper(lCtor.Pointer);
  if lRealCtor = nil then raise new Exception('No default constructor could be found!');
  var lNew := DefaultGC.New(@result, aType.SizeOfType);
  result := InternalCalls.Cast<TComponent>(lNew);
  lRealCtor(result, aOwner);
end;

end.