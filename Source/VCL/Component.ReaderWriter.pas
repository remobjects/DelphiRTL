namespace RemObjects.Elements.RTL.Delphi.VCL;

{$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS OR LINUX) AND NOT DARWIN) OR ECHOESWPF OR (MACOS AND NOT (ISLAND AND DARWIN))}

interface

uses
  RemObjects.Elements.RTL.Delphi,
  RemObjects.Elements.RTL,
  {$IF ECHOESWPF OR MACOS}
  RemObjects.Elements.RTL.Reflection
  {$ENDIF};

type
  TControlCtor = procedure(aInst: Object; aOwner: TComponent);
  TValueType = public enum(vaNull, vaList, vaInt8, vaInt16, vaInt32, vaExtended, vaString, vaIdent, vaFalse, vaTrue, vaBinary, vaSet, vaLString,
    vaNil, vaCollection, vaSingle, vaCurrency, vaDate, vaWString, vaInt64, vaUTF8String, vaDouble);

  {$IF MACOS}
  Del1 = delegate(a: IntPtr);
  Del1Helper = public procedure (aInst: id; aSel: SEL; a: IntPtr);
  {$ENDIF}

  TReaderProc = public procedure(Reader: TReader) of object;
  TWriterProc = public procedure(Writer: TWriter) of object;
  TStreamProc = public procedure(Stream: TStream) of object;

  TFiler = public abstract class(TObject)
  private
    fRoot: TComponent;
    fAncestor: TPersistent;
    fIgnoreChildren: Boolean;
  protected
    fStream: TStream;
    method SetRoot(aValue: TComponent); virtual;
  public
    const FilerSignature: UInt32 = $30465054; // 'TPF0'
    constructor(aStream: TStream; BufSize: Integer);
    method DefineProperty(aName: String; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); virtual; abstract;
    method DefineBinaryProperty(aName: String; ReadData: TStreamProc; WriteData: TStreamProc; HasData: Boolean); virtual; abstract;
    //method FlushBuffer; virtual; abstract;
    property Root: TComponent read fRoot write SetRoot;
    property Ancestor: TPersistent read fAncestor write fAncestor;
    property IgnoreChildren: Boolean read fIgnoreChildren write fIgnoreChildren;
  end;

  {$IF TOFFEE}
  PropertyInfo = &Property;
  {$ENDIF}

  TReader = public partial class(TFiler)
  private
    fParent: TComponent;
    fOwner: TComponent;
    fCurrentProperty: String;
    method ReadComponentData(aInstance: TComponent);
    method FindProperty(aType: &Type; aName: String): PropertyInfo;
  protected
    method ReadProperty(aInstance: TPersistent);
  public
    method DefineProperty(aName: String; aReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); override;
    method DefineBinaryProperty(aName: String; aReadData: TStreamProc; WriteData: TStreamProc; HasData: Boolean); override;
    method EndOfList: Boolean;
    method ReadComponent(aComponent: TComponent): TComponent;
    method ReadData(Instance: TComponent);
    method ReadPropValue(aInstance: TObject; aValueType: TValueType; aProperty: PropertyInfo): Object;
    method ReadRootComponent(aRoot: TComponent): TComponent;
    method ReadSignature;
    method ReadStr: String;
    method ReadValue: TValueType;
    property Owner: TComponent read fOwner write fOwner;
  end;

  TWriter = public class(TFiler)
  public
    method DefineProperty(aName: String; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); override;
    method DefineBinaryProperty(aName: String; ReadData: TStreamProc; WriteData: TStreamProc; HasData: Boolean); override;
    method WriteListBegin;
    method WriteListEnd;
    method WriteSignature;
    method WriteValue(aValue: TValueType);
    method WriteUTF8Str(aValue: string);
    method WriteString(aValue: string);
    method WriteIdent(aIdent: string);
    method WriteInteger(aValue: Integer);
    method WriteInt64(aValue: Int64);
    method WriteDouble(aValue: Double);
    method WriteSet(aValue: Byte);
    method &Write(aBuffer: array of Byte; aOffset: Integer; aCount: Integer);
    method WriteVar(aValue: Integer);

  end;

  TResourceId = public {$IF ISLAND AND WINDOWS} ^Void {$ELSE} Object {$ENDIF};

  TResourceStream = class(TCustomMemoryStream)
  public
    {$IF ISLAND AND WINDOWS}
    constructor(Instance: THandle; ResName: String; ResType: TResourceId);
    {$ELSEIF WEBASSEMBLY}
    constructor(Instance: THandle; aResName: String);
    constructor(Instance: THandle; aResName: String; aContent: String);
    {$ENDIF}
    //constructor(Instance: THandle; ResID: Integer; ResType: PChar);
    //method &Write(Buffer; Count: Longint): Longint; override; final;
    //method &Write(Buffer: TBytes; Offset, Count: Longint): Longint; override; final;
    method ReadComponent(aInstance: TComponent);
  end;

  TParserToken = public enum(toEOF = Char(0), toSymbol = Char(1), toString = Char(2), toInteger = Char(3), toFloat = Char(4), toWString = Char(5), toOtCharacter = Char(6)) of Integer;

  TParser = public class
  private
    fStream: TStream;
    fToken: TParserToken;
    fTokenValue: String;
    method PeekChar: Char;
    method NextChar: Char;
    method SkipToNext;
  public
    constructor(aStream: TStream/*;aOnError: TParserErrorEvent = nil*/);
    constructor(aStream: TStream; FormatSettings: TFormatSettings/*; aOnError: TParserErrorEvent = nil*/);
    //destructor Destroy; override;
    method CheckToken(T: TParserToken);
    method CheckTokenSymbol(S: string);
    //method Error(Ident: string);
    //method ErrorStr(Message: string);
    //method HexToBinary(Stream: TStream);
    method NextToken: TParserToken;
    method SourcePos: LongInt;
    method TokenComponentIdent: string;
    method TokenFloat: Double;
    method TokenInt: Int64;
    method TokenString: String;
    method TokenSymbolIs(S: String): Boolean;
    method TokenStringUntil(C: Char);
    property Token: TParserToken read fToken;
    property TokenValue: String read fTokenValue;
  end;

  ComponentsHelper = public static class
  public
    class method CreateComponent(aClassName: String; aOwner: TComponent): TComponent;
    class method CreateComponent(aType: &Type; aOwner: TComponent): TComponent;
  end;

  ObjectConverter = public class
  private
    fWriter: TWriter;
    fParser: TParser;
    fName: String;
    fMem: TMemoryStream;
    fOutput: TStream;
    method ObjectToBinary;
    method PropertyToBinary;
    method PropertyValueToBinary;
    method AddResHeader;
    method WriteResFile;
  public
    constructor(aInput: TStream; aOutput: TStream);
    method ToBinary;
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

method TReader.DefineProperty(aName: String; aReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
begin

end;

method TReader.DefineBinaryProperty(aName: String; aReadData: TStreamProc; WriteData: TStreamProc; HasData: Boolean);
begin
  if String.Compare(fCurrentProperty, aName) = 0 then begin
    var lNewStream := new TMemoryStream();
    var lSize: Int32;
    fStream.ReadData(var lSize);
    var lBuffer := new Byte[lSize];
    fStream.Read(var lBuffer, lSize);
    lNewStream.Write(lBuffer, lSize);
    lNewStream.Position := 0;
    aReadData(lNewStream);
    fCurrentProperty := '';
  end;
end;

method TReader.ReadComponentData(aInstance: TComponent);
begin
  while not EndOfList do ReadProperty(aInstance);
  ReadValue; // skip end of list
  while not EndOfList do ReadComponent(nil);
  ReadValue; // skip end of list
end;

method TReader.ReadPropValue(aInstance: TObject; aValueType: TValueType; aProperty: PropertyInfo): Object;
begin
  var lValue: Integer;
  var lInt8: Byte;
  var lInt16: SmallInt;
  case aValueType of
    TValueType.vaInt8: begin
      fStream.ReadData(var lInt8);
      exit lInt8;
    end;

    TValueType.vaInt16: begin
      fStream.ReadData(var lInt16);
      exit lInt16;
    end;

    TValueType.vaInt32: begin
      fStream.ReadData(var lValue);
      exit lValue;
    end;

    TValueType.vaInt64: begin
      var lInt64: Int64;
      fStream.ReadData(var lInt64);
      exit lInt64;
    end;

    TValueType.vaString, TValueType.vaUTF8String, TValueType.vaLString: begin
      if aValueType = TValueType.vaLString then
        fStream.ReadData(var lValue)
      else begin
        fStream.ReadData(var lInt8);
        lValue := lInt8;
      end;

      var lBytes := new Byte[lValue];
      fStream.Read(var lBytes, lValue);
      if aValueType = TValueType.vaUTF8String then
        exit RemObjects.Elements.RTL.Encoding.UTF8.GetString(lBytes)
      else
        exit RemObjects.Elements.RTL.Encoding.UTF8.GetString(lBytes);
    end;

    TValueType.vaWString: begin

    end;

    TValueType.vaIdent: begin
      fStream.ReadData(var lInt8);
      var lBytes := new Byte[lInt8];
      fStream.Read(var lBytes, lInt8);
      var lIdent := RemObjects.Elements.RTL.Encoding.UTF8.GetString(lBytes);

      {$IF ISLAND}
      if (aProperty.Type.Flags and IslandTypeFlags.TypeKindMask) = IslandTypeFlags.Delegate then begin // delegate case
        var lType := typeOf(Root);
        var lMethod := lType.Methods.Where(a -> (a.Name = lIdent)).FirstOrDefault;
        var lDelegate := Utilities.NewDelegate(aProperty.Type.RTTI, Root, lMethod.Pointer);

        exit lDelegate;
      end
      else begin
        if (aProperty.Type.Flags and IslandTypeFlags.TypeKindMask) = IslandTypeFlags.EnumFlags then begin
          var lConstant := aProperty.Type.Constants.Where(a -> (a.Name = lIdent)).FirstOrDefault;
          if lConstant <> nil then begin
            exit lConstant.Value;
          end
        end
        else begin
          //var lGlobals := &Type.AllTypes.Where(a -> (a.Name = 'RemObjects.Elements.RTL.Delphi.VCL.__Global')).FirstOrDefault;
          var lGlobals := typeOf(RemObjects.Elements.RTL.Delphi.VCL.__Global);
          // TODO constants values!!
          {$IF ISLAND AND LINUX}
          if lGlobals = nil then
            exit 0;
          if lGlobals.Constants = nil then
            exit 0;
          exit 0;
          {$ENDIF}
          var lConstant := lGlobals.Constants.Where(a -> a.Name = lIdent).FirstOrDefault;
          if lConstant = nil then begin
            //lGlobals := &Type.AllTypes.Where(a -> (a.Name = 'RemObjects.Elements.RTL.Delphi.__Global')).FirstOrDefault;
            lGlobals := typeOf(RemObjects.Elements.RTL.Delphi.__Global);
            lConstant := lGlobals.Constants.Where(a -> a.Name = lIdent).FirstOrDefault;
          end;

          if lConstant ≠ nil then
            exit lConstant.Value
          else
            exit 0;
        end;
      end;
      {$ELSEIF ECHOESWPF}
      var lType := aProperty.PropertyType;
      if lType.IsSubclassOf(TypeOf(MulticastDelegate)) then begin
        exit System.Delegate.CreateDelegate(lType, Root, lIdent, true);
      end
      else begin
        var lConstant: FieldInfo := nil;
        if lType.IsEnum then
          exit &Enum.Parse(lType, lIdent, true)
        else begin
          var lGlobals := System.Type.GetType('RemObjects.Elements.RTL.Delphi.VCL.__Global', false);
          var lFields := lGlobals.GetFields(BindingFlags.Public or BindingFlags.Static or BindingFlags.FlattenHierarchy);
          lConstant := lFields.Where(a -> a.Name = lIdent).FirstOrDefault;
          if lConstant = nil then begin
            lGlobals := System.Type.GetType('RemObjects.Elements.RTL.Delphi.__Global', false);
            if lGlobals ≠ nil then begin
              lFields := lGlobals.GetFields(BindingFlags.Public or BindingFlags.Static or BindingFlags.FlattenHierarchy);
              lConstant := lFields.Where(a -> a.Name = lIdent).FirstOrDefault;
            end;
          end;

          if lConstant ≠ nil then
            exit lConstant.GetRawConstantValue()
          else
            exit 0;
        end;
      end;
      {$ELSEIF TOFFEE}
      // TODO
      if aProperty.Type.IsDelegate then begin
        //var lType := typeOf(Root);
        var lType := new &Type withClass(Root.class);
        var lEvent: &Method := nil;
        for each lMethod in lType.Methods do begin
          var lName := lMethod.Name;
          var lPos := lName.IndexOf(':');
          if lPos > 0 then
            lName := lName.SubString(0, lPos);

          if lName = lIdent then begin
            lEvent := lMethod;
            break;
          end;
        end;

        if lEvent ≠ nil then begin
          var lPtr := Root.methodForSelector(lEvent.Selector);
          var r: Del1 := (arg1: IntPtr) -> begin Del1Helper(lPtr)(Root, lEvent.Selector, arg1); end;
          var lSetMethod := NSSelectorFromString('set' + aProperty.Name + ':');
          var lSignature := aInstance.methodSignatureForSelector(lSetMethod);
          var lInvoke := NSInvocation.invocationWithMethodSignature(lSignature);
          lInvoke.target := aInstance;
          lInvoke.selector := lSetMethod;
          var lArg := ^Void(@r);
          lInvoke.setArgument(lArg) atIndex(2);
          lInvoke.invoke;
        end;

        exit nil;
      end
      else begin
        var lType := aProperty.Type;
        // enum, constants
        //var lGlobals := &Type.AllTypes;
        //WriteLn('Getting all types');
        //for each lType in &Type.AllTypes do
          //writeLn(lType.Name);
        //var lProps := lGlobals.Properties;
        //writeLn(lProps.Count);
      end;
      {$ENDIF}
    end;

    TValueType.vaSet: begin
      while not EndOfList do begin
        var lString := ReadStr;
      end;
      ReadValue; // End of List
      exit 0; // TODO
    end;

    TValueType.vaList: begin
      var lIsTStrings: Boolean;
      lIsTStrings := typeOf(aInstance).IsSubclassOf(typeOf(TStrings));
      if lIsTStrings then begin
        var lStrings := aInstance as TStrings;
        while not EndOfList do begin
          ReadValue;
          lStrings.Add(ReadStr);
        end;
        ReadValue; // End of List
      end;
      exit nil;
    end;

    TValueType.vaCollection: begin
      var lCollection := aProperty.GetValue(aInstance, []);
      var lType := typeOf(lCollection);
      {$IF ECHOES}
      var lAddMethod := lType.GetMethods().Where(a->a.Name = 'Add').FirstOrDefault;
      {$ELSEIF ISLAND}
      var lAddMethod := lType.Methods.Where(a->a.Name = 'Add').FirstOrDefault;
      {$ELSEIF TOFFEE}
      var lAddMethod := lType.Methods.Where(a->a.Name = 'Add:').FirstOrDefault;
      {$ENDIF}
      while not EndOfList do begin
        ReadValue; // Skip BeginList
        var lNewItem := TCollectionItem(lAddMethod.Invoke(lCollection, []));
        while not EndOfList do
          ReadProperty(lNewItem);
        ReadValue; // Skip EndList
      end;
      ReadValue; // Skip EndList
      exit nil;
    end;

    TValueType.vaFalse:
      exit false;

    TValueType.vaTrue:
      exit true;
  end;
end;

method TReader.FindProperty(aType: &Type; aName: String): PropertyInfo;
begin
  result := nil;
  {$IF (ISLAND AND (WEBASSEMBLY OR WINDOWS OR LINUX))}
  var lType := aType;
  while lType <> nil do begin
    result := lType.Properties.Where(a -> (a.Name = aName)).FirstOrDefault;
    if result <> nil then
      exit;
    if lType.RTTI^.ParentType <> nil then
      lType := new &Type(lType.RTTI^.ParentType)
    else
      lType := nil;
  end;
  {$ELSEIF ECHOESWPF}
  var lType := aType;
  while lType <> nil do begin
    result := lType.GetProperty(aName);
    if result <> nil then begin
      exit;
    end;
    lType := lType.BaseType;
  end;
  {$ELSEIF TOFFEE}
  var lInstanceType := aType;
  var lProperty: PropertyInfo := nil;
  var lParent: &Class := nil;
  var lCurrentClass := aType.TypeClass;
  while lProperty = nil do begin
    var lProps := lInstanceType.Properties;
    for each lProp in lProps do begin
      if lProp.Name = aName then begin
        lProperty := lProp;
        break;
      end;
    end;
    lParent := class_getSuperclass(lCurrentClass);
    if lCurrentClass ≠ lParent then begin
      lCurrentClass := lParent;
      if lParent ≠ nil then
        lInstanceType := new &RemObjects.Elements.RTL.Reflection.Type withClass(lParent)
      else
        break;
    end
    else
      break;
  end;
  result := lProperty;
  {$ENDIF}
end;

method TReader.ReadProperty(aInstance: TPersistent);
begin
  var lName := ReadStr;
  var lValue := ReadValue;
  {$IF TOFFEE}
  var lType := new &Type withclass(aInstance.class);
  {$ELSE}
  var lType := typeOf(aInstance);
  {$ENDIF}
  var lProperty: PropertyInfo;
  var lInstance: Object := aInstance;

  if lName.IndexOf('.') > 0 then begin
    var lProps := lName.Split('.');
    for i: Integer := 0 to lProps.Count - 2 do begin
      lProperty := FindProperty(lType, lProps[i]);
      if lProperty = nil then
        raise new Exception('Can not get property ' + lProps[i]);

      {$IF LINUX AND ARM}
      // workaround for linux/arm here, ObjectGetter(lProperty.Reader)
      var lPropValue := ObjectGetter(lProperty.ReadMethod)(lInstance);
      {$ELSE}
      var lPropValue := lProperty.GetValue(lInstance, []);
      {$ENDIF}
      lInstance := lPropValue;
      {$IF TOFFEE}
      lType := new &Type withclass(lInstance.class);
      {$ELSE}
      lType := typeOf(lInstance);
      {$ENDIF}
      //lType := typeOf(lInstance);
    end;
    lName := lProps[lProps.Count - 1];
  end;

  {$IF TOFFEE}
  var lIsTStrings := lType.IsSubclassOf(new &Type withClass(TStrings.class));
  {$ELSE}
  var lIsTStrings := lType.IsSubclassOf(typeOf(TStrings));
  {$ENDIF}

  if not lIsTStrings then
    lProperty := FindProperty(lType, lName);

  if lProperty = nil then begin
    fCurrentProperty := lName;
    (lInstance as TPersistent).DefineProperties(self);
    if fCurrentProperty <> '' then
      raise new Exception('Can not get property ' + lName);
  end
  else begin
    var lPropValue := ReadPropValue(lInstance, lValue, lProperty);

    if (lValue ≠ TValueType.vaList) and (lValue ≠ TValueType.vaCollection) then begin
      {$IF WEBASSEMBLY}
      DynamicHelpers.SetMember(lInstance, lName, 0, [lPropValue]);
      {$ELSEIF ISLAND}
      lProperty := FindProperty(lType, lName);
      if lProperty = nil then
        raise new Exception('Can not get property ' + lName);
      {$IF LINUX AND ARM}
      // workaround about the object setter if lPropValue is a delegate...
      if typeOf(lPropValue).IsDelegate then begin
        ObjectSetter(lProperty.WriteMethod)(lInstance, lPropValue);
        exit;
      end;
      {$ENDIF}
      if lPropValue ≠ nil then // TODO
        lProperty.SetValue(lInstance, nil, lPropValue);
      {$ELSEIF ECHOESWPF}
      var lCurrentProp := lType.GetProperty(lName, BindingFlags.Public or BindingFlags.Instance);
      if lCurrentProp = nil then
        raise new Exception('Can not get property ' + lName);
      lCurrentProp.SetValue(lInstance, lPropValue, nil);
      {$ELSEIF TOFFEE}
      lProperty := FindProperty(lType, lName);
      if lProperty = nil then
        raise new Exception('Can not get property ' + lName);
      if lPropValue ≠ nil then // TODO
        lProperty.SetValue(lInstance, nil, lPropValue);
      {$ENDIF}
    end;
  end;
end;

method TReader.EndOfList: Boolean;
begin
  result := ReadValue = TValueType.vaNull;
  fStream.Position := fStream.Position - 1;
end;

method TReader.ReadRootComponent(aRoot: TComponent): TComponent;
begin
  aRoot.SetComponentState(TComponentStateEnum.csLoading);
  ReadSignature; // Skip 'TPF0'
  ReadStr;
  var lName := ReadStr;
  aRoot.Name := lName;
  //Root.Name := ReadStr;
  fOwner := aRoot;
  fParent := aRoot;
  Root := aRoot;
  ReadComponentData(aRoot);
  aRoot.RemoveComponentState(TComponentStateEnum.csLoading);
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
    //TControl(result).Parent := TControl(fParent);
  end;
  result.SetComponentState(TComponentStateEnum.csLoading);
  TControl(result).Parent := TNativeControl(fParent);
  var lOldParent := fParent;
  fParent := result;
  ReadComponentData(result);
  fParent := lOldParent;
  result.RemoveComponentState(TComponentStateEnum.csLoading);

  {$IF ISLAND}
  DynamicHelpers.SetMember(Root, lName, 0, [result]);
  {$ELSEIF ECHOESWPF}
  var lCurrentField := Root.GetType().GetField(lName, BindingFlags.Public or BindingFlags.Instance or BindingFlags.IgnoreCase);
  if lCurrentField ≠ nil then
    lCurrentField.SetValue(Root, result)
  else begin
    var lCurrentProp := Root.GetType().GetProperty(lName, BindingFlags.Public or BindingFlags.Instance or BindingFlags.IgnoreCase);
    if lCurrentProp = nil then
      raise new Exception('Can not get property ' + lName)
    else
      lCurrentProp.SetValue(Root, result, nil);
  end;
  {$ELSEIF TOFFEE}
  var lIvarInfos: ^rtl.Ivar;
  var lIvarCount: UInt32;
  //lIvarInfos := class_copyIvarList(typeOf(Root).TypeClass, var lIvarCount);
  lIvarInfos := class_copyIvarList(Root.class, var lIvarCount);

  var lNameIvar: rtl.Ivar := nil;
  for i: Int32 := 0 to lIvarCount - 1 do begin
    var lVar: rtl.Ivar;
    lVar := lIvarInfos[i];
    if NSString(lName).caseInsensitiveCompare(rtl.ivar_getName(lVar)) = NSComparisonResult.NSOrderedSame then begin
      lNameIvar := lVar;
      break;
    end;
  end;

  if lNameIvar ≠ nil then
    rtl.object_setIvar(Root, lNameIvar, result)
  else
    raise new Exception('Can not get ' + lName + ' field on ' + Root.Name);
  {$ENDIF}
  result.Loaded;
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
  result := RemObjects.Elements.RTL.Encoding.UTF8.GetString(lArray);
end;

method TReader.ReadValue: TValueType;
begin
  var lByte: Byte;
  fStream.ReadData(var lByte);
  result := TValueType(lByte);
end;

method ComponentsHelper.CreateComponent(aClassName: String; aOwner: TComponent): TComponent;
begin
  {$IF ISLAND}
  var lType := &Type.AllTypes.Where(a -> a.Name = 'RemObjects.Elements.RTL.Delphi.VCL.' + aClassName).FirstOrDefault;
  {$ELSEIF ECHOESWPF}
  var lType := &Type.GetType('RemObjects.Elements.RTL.Delphi.VCL.' + aClassName);
  {$ELSEIF TOFFEE}
  //var lType := RemObjects.Elements.RTL.Reflection.Type.GetType('__RemObjects_Elements_RTL_Delphi_VCL_' + aClassName);
  var lType := RemObjects.Elements.RTL.Reflection.Type.GetType(aClassName);
  // TODO
  {$ENDIF}
  if lType = nil then raise new Exception('Can not get ' + aClassName + ' type');
  result := CreateComponent(lType, aOwner);
end;

method ComponentsHelper.CreateComponent(aType: &Type; aOwner: TComponent): TComponent;
begin
  {$IF ISLAND}
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

  if lCtor = nil then raise new Exception('No default constructor can be found!');
  var lNew := DefaultGC.New(aType.RTTI, aType.SizeOfType);
  result := InternalCalls.Cast<TComponent>(lNew);
  //lCtor.Invoke(result, [aOwner]);
  var lCaller := TControlCtor(lCtor.Pointer);
  lCaller(result, aOwner);
  {$ELSEIF ECHOESWPF}
  result := TComponent(Activator.CreateInstance(aType, [aOwner]));
  {$ELSEIF MACOS}
  var lInstanceType := aType;
  var lCtor: RemObjects.Elements.RTL.Reflection.Method := nil;
  var lParent: &Class := nil;
  var lCurrentClass := aType.TypeClass;
  while lCtor = nil do begin
    var lMethods := lInstanceType.Methods;
    for each lMethod in lMethods do begin
      if lMethod.Name = 'init:' then begin
        lCtor := lMethod;
        break;
      end;
  end;
  lParent := class_getSuperclass(lCurrentClass);
  if lCurrentClass ≠ lParent then begin
    lCurrentClass := lParent;
    if lParent ≠ nil then
      lInstanceType := new &RemObjects.Elements.RTL.Reflection.Type withClass(lParent)
    else
      break;
    end
    else
      break;
  end;
  if lCtor = nil then raise new Exception('No constructor can be found!');

  var lNew := aType.TypeClass.alloc();
  var lInvokation := NSInvocation.invocationWithMethodSignature(lNew.methodSignatureForSelector(lCtor.Selector));
  lInvokation.setSelector(lCtor.Selector);
  lInvokation.setTarget(lNew);
  lInvokation.setArgument(@aOwner) atIndex(2);
  lInvokation.invoke();
  lInvokation.getReturnValue(@result);
  {$ENDIF}
end;

method ObjectConverter.ObjectToBinary;
begin
  fParser.NextToken;
  var lValue := fParser.TokenValue; // object name
  fParser.NextToken; // skip ':'
  fParser.NextToken;
  var lValue2 := fParser.TokenValue; // class
  fWriter.WriteUTF8Str(lValue2);
  fWriter.WriteUTF8Str(lValue);
  fParser.NextToken;
  while (fParser.Token <> TParserToken.toEOF) and not (fParser.TokenSymbolIs('end')) do begin
    if fParser.TokenSymbolIs('object') then begin
      fWriter.WriteListEnd;
      ObjectToBinary;
    end
    else
      PropertyToBinary;

    fParser.NextToken;
  end;
  fWriter.WriteListEnd;
end;

method ObjectConverter.PropertyValueToBinary;
begin
  case fParser.Token of
    TParserToken.toString: begin
      fWriter.WriteString(fParser.TokenValue);
    end;

    TParserToken.toSymbol: begin
      fWriter.WriteIdent(fParser.TokenValue);
    end;

    TParserToken.toInteger: begin
      fWriter.WriteInteger(fParser.TokenInt);
    end;

    TParserToken.toFloat: begin
      fWriter.WriteDouble(fParser.TokenFloat);
    end;

    TParserToken.toOtCharacter: begin
      if fParser.TokenValue = '[' then begin
        fWriter.WriteValue(TValueType.vaSet);
        fParser.NextToken;
        if fParser.TokenString <> ']' then begin
          fWriter.WriteUTF8Str(fParser.TokenString);
          fParser.NextToken;
          while fParser.TokenString = ',' do begin
            fParser.NextToken;
            fWriter.WriteUTF8Str(fParser.TokenString);
            fParser.NextToken;
          end;
        end;
        fWriter.WriteListEnd;
      end
      else
        if fParser.TokenValue = '(' then begin
          fWriter.WriteListBegin;
          fParser.NextToken;
          while fParser.TokenString ≠ ')' do begin
            fWriter.WriteString(fParser.TokenString);
            fParser.NextToken;
            if fParser.TokenString = ',' then
              fParser.NextToken;
          end;
          fWriter.WriteListEnd;
        end
        else
          if fParser.TokenValue = '<' then begin
            fWriter.WriteValue(TValueType.vaCollection);
            fParser.NextToken;
            while fParser.TokenValue ≠ '>' do begin
              if fParser.TokenValue ≠ 'item' then
                raise new Exception('Wrong collection value!');
              fWriter.WriteListBegin;
              fParser.NextToken; // item
              while fParser.TokenValue ≠ 'end' do begin
                PropertyToBinary;
                fParser.NextToken;
              end;
              fWriter.WriteListEnd;
              fParser.NextToken; // end
            end;
            //fParser.NextToken;
            fWriter.WriteListEnd;
          end
          else
            if fParser.TokenValue = '{' then begin
              fWriter.WriteValue(TValueType.vaBinary);
              //fParser.NextToken;
              //writeLn(fParser.TokenValue);
              fParser.TokenStringUntil('}');
              fWriter.WriteVar(Integer(fParser.TokenValue.Length / 2));
              var lData := Convert.HexStringToByteArray(fParser.TokenValue);
              fWriter.Write(lData, 0, length(lData));
              fParser.NextToken; // Skip }
            end;
    end;
  end;
end;

method ObjectConverter.PropertyToBinary;
begin
  fWriter.WriteUTF8Str(fParser.TokenValue);
  fParser.NextToken;
  if fParser.TokenValue = '=' then begin
    fParser.NextToken;
    PropertyValueToBinary;
  end;
  //else
    //error;
end;

constructor ObjectConverter(aInput: TStream; aOutput: TStream);
begin
  fParser := new TParser(aInput);
  fOutput := aOutput;
  fMem := new TMemoryStream;
  fWriter := new TWriter(fMem, 100);
end;

/*
typedef struct {
  DWORD DataSize;
  DWORD HeaderSize;
  DWORD TYPE;
  DWORD NAME;
  DWORD DataVersion;
  WORD  MemoryFlags;
  WORD  LanguageId;
  DWORD Version;
  DWORD Characteristics;
} RESOURCEHEADER;
*/

method ObjectConverter.AddResHeader;
begin
  {$IF ISLAND AND WINDOWS}
  var lNameBytes := RemObjects.Elements.RTL.Encoding.UTF16LE.GetBytes(fName);
  var lNameLength := length(lNameBytes);
  fOutput.Write([$00, $00, $00, $00, $20, $00, $00, $00, $FF, $FF, $00, $00, $FF, $FF, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00], 32); // Standard .res header
  fOutput.WriteData(rtl.DWORD(fMem.Size));
  fOutput.WriteData(rtl.DWORD(30 + lNameLength));
  fOutput.WriteData(Word($FFFF)); // RT_RCDATA
  fOutput.WriteData(Word(10)); // RT_RCDATA
  fOutput.WriteData(lNameBytes, length(lNameBytes));
  fOutput.WriteData(Word(0)); // zero terminated string
  fOutput.WriteData(rtl.DWORD(0)); // DataVersion
  fOutput.WriteData(Word(0)); // MemoryFlags
  fOutput.WriteData(Word($0409)); // LanguageID -> English
  fOutput.WriteData(rtl.DWORD(0)); // Version
  fOutput.WriteData(rtl.DWORD(0)); // Characteristics
  {$ELSE}
  // Nothing to do...
  {$ENDIF}
end;

method ObjectConverter.WriteResFile;
begin
  AddResHeader;
  fMem.Position := 0;
  fOutput.CopyFrom(fMem, fMem.Size);
end;

method ObjectConverter.ToBinary;
begin
  fWriter.WriteSignature;
  fParser.NextToken;
  if fParser.TokenSymbolIs('object') then begin
    fParser.NextToken;
    var lValue := fParser.TokenValue; // object name
    fParser.NextToken; // skip ':'
    fParser.NextToken;
    var lValue2 := fParser.TokenValue; // class
    fName := lValue2.ToUpper;
    fWriter.WriteUTF8Str(lValue2);
    fWriter.WriteUTF8Str(lValue);
    fParser.NextToken;
    while (fParser.Token <> TParserToken.toEOF) and not (fParser.TokenSymbolIs('end')) do begin
      if fParser.TokenSymbolIs('object') then begin
        fWriter.WriteListEnd;
        ObjectToBinary;
      end
      else
        PropertyToBinary;

      fParser.NextToken;
    end;
    fWriter.WriteListEnd;
    fWriter.WriteListEnd;
  end;
  //else
    //Error
  WriteResFile;
end;

{$IF ISLAND AND WINDOWS}
constructor TResourceStream(Instance: THandle; ResName: String; ResType: TResourceId);
begin
  var lModule: rtl.HMODULE := 0;
  var lResName := ResName.ToCharArray;
  var lResource := rtl.FindResource(lModule, @lResName[0], rtl.LPCWSTR(rtl.RT_RCDATA));
  if lResource = nil then raise new Exception('Can not locale resource: ' + ResName);
  var lHandle := rtl.LoadResource(lModule, lResource);
  if lHandle = nil then raise new Exception('Can not load resource: ' + ResName);
  var lPointer := rtl.LockResource(lHandle);
  var lSize := rtl.SizeofResource(lModule, lResource);
  Size := lSize;
  &Write(lPointer, lSize);
end;
{$ELSEIF WEBASSEMBLY}
constructor TResourceStream(Instance: THandle; aResName: String);
begin
  var lContent := Browser.AjaxRequestBinary('wasm/resources/' + aResName);
  var lInput := new TMemoryStream();
  lInput.Write(lContent, 0, lContent.Length);
  var lResHeaderSize := 62 + ((aResName.Length - 4) * 2); // -4 because .dfm is not included in resource name
  lInput.Position := lResHeaderSize;
  CopyFrom(lInput, lInput.Size - lResHeaderSize);
end;

constructor TResourceStream(Instance: THandle; aResName: String; aContent: String);
begin
  var lInput := new TMemoryStream();
  var lArray := new Byte[aContent.Length];
  for i: Integer := 0 to aContent.Length - 1 do
    lArray[i] := Byte(chr(aContent[i]));
  lInput.Write(lArray, 0, lArray.Length);
  var lResHeaderSize := 62 + ((aResName.Length - 4) * 2); // -4 because .dfm is not included in resource name
  lInput.Position := lResHeaderSize;
  CopyFrom(lInput, lInput.Size - lResHeaderSize);
end;
{$ENDIF}

method TResourceStream.ReadComponent(aInstance: TComponent);
begin
  var lReader := new TReader(self, Size);
  lReader.ReadRootComponent(aInstance);
end;

constructor TParser(aStream: TStream);
begin
  fStream := aStream;
end;

method TParser.CheckToken(T: TParserToken);
begin
  if not (fToken = TParserToken.toSymbol) then
    raise new Exception('Wrong token');
end;

method TParser.CheckTokenSymbol(S: string);
begin
  if not (fToken = TParserToken.toSymbol) and (String.Compare(S, fTokenValue) = 0) then
    raise new Exception('Wrong token');
end;

method TParser.NextToken: TParserToken;
begin
  SkipToNext;
  var lChar := PeekChar;

  case lChar of
    '_', 'a'..'z', 'A'..'Z': begin
      fTokenValue := lChar;
      NextChar;
      while PeekChar in ['_', '.', 'a'..'z', 'A'..'Z', '0'..'9'] do
        fTokenValue := fTokenValue + NextChar;

      fToken := TParserToken.toSymbol;
    end;

    '0'..'9', '-': begin
      fTokenValue := lChar;
      NextChar;
      while PeekChar in ['0'..'9', '.'] do
        fTokenValue := fTokenValue + NextChar;

      fToken := if fTokenValue.IndexOf('.') >= 0 then TParserToken.toFloat else TParserToken.toInteger;
    end;

    '''': begin
      fTokenValue := ''; // dismiss '
      NextChar;
      while PeekChar <> "'" do
        fTokenValue := fTokenValue + NextChar;
      NextChar; // dismis '
      fToken := TParserToken.toString;
    end;

    ':', '#', '=', '[', ']', '(', ')', '<', '>', '{', '}': begin
      fTokenValue := lChar;
      NextChar;
      fToken := TParserToken.toOtCharacter;
    end;
  end;

  result := fToken;
end;

method TParser.SourcePos: LongInt;
begin
  result := fStream.Position;
end;

method TParser.TokenComponentIdent: String;
begin
  result := fTokenValue;
end;

method TParser.TokenFloat: Double;
begin
  result := Convert.ToDouble(fTokenValue, Locale.Invariant);
end;

method TParser.TokenInt: Int64;
begin
  result:= Convert.ToInt64(fTokenValue);
end;

method TParser.TokenString: String;
begin
  result:= fTokenValue;
end;

method TParser.TokenSymbolIs(S: String): Boolean;
begin
  result := (fToken = TParserToken.toSymbol) and (String.Compare(S, fTokenValue) = 0);
end;

method TParser.TokenStringUntil(C: Char);
begin
  SkipToNext;
  fTokenValue := '';
  var lChar := PeekChar;
  while lChar ≠ C do begin
    fTokenValue := fTokenValue + lChar;
    SkipToNext;
    NextChar;
    SkipToNext;
    lChar := PeekChar;
  end;
end;

constructor TParser(aStream: TStream; FormatSettings: TFormatSettings);
begin

end;

method TParser.PeekChar: Char;
begin
  var lData: Byte;
  fStream.ReadData(var lData);
  result := chr(lData);
  fStream.Position := fStream.Position - 1;
end;

method TParser.NextChar: Char;
begin
  var lData: Byte;
  fStream.ReadData(var lData);
  result := Char(lData);
end;

method TParser.SkipToNext;
begin
  while (fStream.Position < fStream.Size) and (PeekChar in [' ', #13, #10]) do
    fStream.Position := fStream.Position + 1;
end;

method TWriter.DefineProperty(aName: String; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
begin

end;

method TWriter.DefineBinaryProperty(aName: String; ReadData: TStreamProc; WriteData: TStreamProc; HasData: Boolean);
begin

end;

method TWriter.WriteListBegin;
begin
  fStream.WriteData(Byte(TValueType.vaList));
end;

method TWriter.WriteListEnd;
begin
  fStream.WriteData(Byte(TValueType.vaNull));
end;

method TWriter.WriteSignature;
begin
  fStream.WriteData(FilerSignature);
end;

method TWriter.WriteUTF8Str(aValue: string);
begin
  var lBytes := RemObjects.Elements.RTL.Encoding.UTF8.GetBytes(aValue);
  var lTotal := if RemObjects.Elements.System.length(lBytes) > 255 then 255 else length(lBytes);
  fStream.WriteData(Byte(lTotal));
  fStream.Write(lBytes, 0, lTotal);
end;

method TWriter.WriteValue(aValue: TValueType);
begin
  fStream.WriteData(Byte(aValue));
end;

method TWriter.WriteString(aValue: string);
begin
  var lBytes := RemObjects.Elements.RTL.Encoding.UTF8.GetBytes(aValue);
  var lTotal := RemObjects.Elements.System.length(lBytes);
  fStream.WriteData(Byte(TValueType.vaString));
  fStream.WriteData(Byte(lTotal));
  fStream.Write(lBytes, lTotal);
end;

method TWriter.WriteIdent(aIdent: string);
begin
  if String.Compare(aIdent, 'True') = 0 then
    fStream.WriteData(Byte(TValueType.vaTrue))
  else
    if String.Compare(aIdent, 'False') = 0 then
      fStream.WriteData(Byte(TValueType.vaFalse))
    else begin
      fStream.WriteData(Byte(TValueType.vaIdent));
      WriteUTF8Str(aIdent);
    end;
end;

method TWriter.WriteInteger(aValue: Integer);
begin
  //if (aValue >= Byte.MinValue) and (aValue <= Byte.MaxValue) then begin
    if (aValue >= 0) and (aValue <= 255) then begin
    fStream.WriteData(Byte(TValueType.vaInt8));
    fStream.WriteData(Byte(aValue));
  end
  else
    //if (aValue >= SmallInt.MinValue) and (aValue <= SmallInt.MaxValue) then begin
    if (aValue >= -32768) and (aValue <= 32767) then begin
      fStream.WriteData(Byte(TValueType.vaInt16));
      fStream.WriteData(SmallInt(aValue));
    end
    else begin
      fStream.WriteData(Byte(TValueType.vaInt32));
      fStream.WriteData(Integer(aValue));
    end;
end;

method TWriter.WriteInt64(aValue: Int64);
begin
  fStream.WriteData(Byte(TValueType.vaInt64));
  fStream.WriteData(aValue);
end;

method TWriter.WriteDouble(aValue: Double);
begin
  fStream.WriteData(Byte(TValueType.vaDouble));
  fStream.WriteData(aValue);
end;

method TWriter.WriteSet(aValue: Byte);
begin
  fStream.WriteData(Byte(TValueType.vaSet));
  fStream.WriteData(aValue);
end;

method TWriter.Write(aBuffer: array of Byte; aOffset: Integer; aCount: Integer);
begin
  fStream.Write(aBuffer, aOffset, aCount);
end;

method TWriter.WriteVar(aValue: Integer);
begin
  fStream.WriteData(aValue);
end;

type
  ObjectGetter = function(aInst: Object): Object;
  ObjectSetter = procedure(aInst: Object; v: Object);

{$ENDIF}

end.