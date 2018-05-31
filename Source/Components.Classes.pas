namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  RemObjects.Elements.RTL.Delphi, rtl;

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
    const FilerSignature: UInt32 = $30465054; // 'TPF0'
    constructor(aStream: TStream; BufSize: Integer);
    //method DefineProperty(Name: String; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); virtual; abstract;
    //method DefineBinaryProperty(Name: String; ReadData, WriteData: TStreamProc; HasData: Boolean); virtual; abstract;
    //method FlushBuffer; virtual; abstract;
    property Root: TComponent read fRoot write SetRoot;
    property Ancestor: TPersistent read fAncestor write fAncestor;
    property IgnoreChildren: Boolean read fIgnoreChildren write fIgnoreChildren;
  end;

  TReader = public class(TFiler)
  private
    fParent: TComponent;
    fOwner: TComponent;
    method ReadComponentData(aInstance: TComponent);
    method FindProperty(aType: &Type; aName: String): PropertyInfo;
  protected
    method ReadProperty(aInstance: TComponent /*TPersistent*/);
  public
    method EndOfList: Boolean;
    method ReadComponent(aComponent: TComponent): TComponent;
    method ReadData(Instance: TComponent);
    method ReadPropValue(aInstance: TComponent; aValueType: TValueType; aProperty: PropertyInfo): Object;
    method ReadRootComponent(aRoot: TComponent): TComponent;
    method ReadSignature;
    method ReadStr: String;
    method ReadValue: TValueType;
    property Owner: TComponent read fOwner write fOwner;
  end;

  TWriter = public class(TFiler)
  public
    method WriteListBegin;
    method WriteListEnd;
    method WriteSignature;
    method WriteUTF8Str(aValue: string);
    method WriteString(aValue: string);
    method WriteIdent(aIdent: string);
    method WriteInteger(aValue: Integer);
    method WriteInt64(aValue: Int64);
    method WriteDouble(aValue: Double);
  end;

  TResourceStream = class(TCustomMemoryStream)
  public
    constructor(Instance: THandle; ResName: String; ResType: PCHAR);
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
    method ObjectToBinary;
    method PropertyToBinary;
    method PropertyValueToBinary;
    method AddResHeader;
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

method TReader.ReadComponentData(aInstance: TComponent);
begin
  while not EndOfList do ReadProperty(aInstance);
  ReadValue; // skip end of list
  while not EndOfList do ReadComponent(nil);
  ReadValue; // skip end of list
end;

method TReader.ReadPropValue(aInstance: TComponent; aValueType: TValueType; aProperty: PropertyInfo): Object;
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
        exit RemObjects.Elements.RTL.Encoding.Default.GetString(lBytes);
    end;

    TValueType.vaWString: begin

    end;

    TValueType.vaIdent: begin
      fStream.ReadData(var lInt8);
      var lBytes := new Byte[lInt8];
      fStream.Read(var lBytes, lInt8);
      var lIdent := RemObjects.Elements.RTL.Encoding.UTF8.GetString(lBytes);

      if (aProperty.Type.Flags and IslandTypeFlags.Delegate) <> 0 then begin // delegate case
        var lType := typeOf(Root);
        var lMethod := lType.Methods.Where(a -> (a.Name = lIdent)).FirstOrDefault;
        //var lDelegate := Utilities.NewDelegate(lType.RTTI, Root, lMethod.Pointer);
        var lDelegate := Utilities.NewDelegate(aProperty.Type.RTTI, Root, lMethod.Pointer);

        exit lDelegate;
      end
      else begin
        var lConstant := aProperty.Type.Constants.Where(a -> (a.Name = lIdent)).FirstOrDefault;
        if lConstant <> nil then
          exit lConstant.Value
        else
          exit 0; // TODO check!!!
      end;
    end;

    TValueType.vaSet: begin
      fStream.ReadData(var lInt8);
      exit lInt8;
    end;

    TValueType.vaFalse:
      exit false;

    TValueType.vaTrue:
      exit true;
  end;
end;

method TReader.FindProperty(aType: &Type; aName: String): PropertyInfo;
begin
  var lType := aType;
  while aType <> nil do begin
    result := lType.Properties.Where(a -> (a.Name = aName)).FirstOrDefault;
    if result <> nil then begin
      writeLn('Property on class: ' + lType.Name);
      exit;
    end;
    lType := new &Type(lType.RTTI^.ParentType);
  end;
end;

method TReader.ReadProperty(aInstance: TComponent/*TPersistent*/);
begin
  var lName := ReadStr;
  var lValue := ReadValue;
  var lType := typeOf(aInstance);
  var lProperty: PropertyInfo;
  var lInstance: Object := aInstance;

  if lName.IndexOf('.') > 0 then begin
    var lProps := lName.Split('.');
    for i: Integer := 0 to lProps.Count - 2 do begin
      lProperty := FindProperty(lType, lProps[i]);
      if lProperty = nil then begin
        writeLn('property not found...');
        raise new Exception('Can not get property ' + lProps[i]);
      end;
      var lPropValue := lProperty.GetValue(lInstance, nil);
      lInstance := lPropValue;
      lType := typeOf(lInstance);
    end;
    lName := lProps[lProps.Count - 1];
  end;

  writeLn('Finding property... ' + lName);
  writeLn('on type: ' + lType.Name);
  lProperty := FindProperty(lType, lName);
  if lProperty = nil then raise new Exception('Can not get property ' + lName);
  writeLn('Reading prop value...');
  var lPropValue := ReadPropValue(TComponent(lInstance), lValue, lProperty);
  writeLn('Setting prop value');
  DynamicHelpers.SetMember(lInstance, lName, 0, [lPropValue]);

  //lProperty.SetValue(aInstance, [], lPropValue);
  writeLn('Property ok');
end;

method TReader.EndOfList: Boolean;
begin
  result := ReadValue = TValueType.vaNull;
  fStream.Position := fStream.Position - 1;
end;

method TReader.ReadRootComponent(aRoot: TComponent): TComponent;
begin
  ReadSignature; // Skip 'TPF0'
  ReadStr;
  var lName := ReadStr;
  aRoot.Name := lName;
  //Root.Name := ReadStr;
  writeLn(aRoot.Name);
  fOwner := aRoot;
  fParent := aRoot;
  Root := aRoot;
  ReadComponentData(aRoot);
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
    writeLn('Parent:' + fParent.Name);
    TControl(result).Parent := TControl(fParent);
  end;
  var lOldParent := fParent;
  fParent := result;
  ReadComponentData(result);
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
  writeLn('Finding type: ' + aClassName);
  var lType := &Type.AllTypes.Where(a -> a.Name = 'RemObjects.Elements.RTL.Delphi.' + aClassName).FirstOrDefault;
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
  var lNew := DefaultGC.New(aType.RTTI, aType.SizeOfType);
  result := InternalCalls.Cast<TComponent>(lNew);
  lCtor.Invoke(result, [aOwner]);
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
        fParser.NextToken;
        fWriter.WriteString('[]');
        // TODO
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
  fWriter := new TWriter(aOutput, 100);
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
  var lNameBytes := RemObjects.Elements.RTL.Encoding.UTF16LE.GetBytes(fName);
  var lNameLength := length(lNameBytes);
  fHeader := new TMemoryStream;
  fHeader.WriteData(DWORD(fOutput.Size));
  fHeader.WriteData(DWORD(30 + lNameLength));
  fHeader.WriteData(DWORD(10)); // RT_RCDATA
  fHeader.Write(lNameBytes, 0);
  fHeader.Write(Word(0)); // zero terminated string
  fHeader.Write(WORD(0)); // DataVersion
  fHeader.Write(WORD(0)); // MemoryFlags
  fHeader.Write(WORD($0409)); // LanguageID -> English
  fHeader.Write(DWORD(0)); // Version
  fHeader.Write(DWORD(0)); // Characteristics
end;

method ObjectConverter.ToBinary;
begin
  fWriter.WriteSignature;
  fParser.NextToken;
  if fParser.TokenSymbolIs('object') then begin
    fParser.NextToken;
    var lValue := fParser.TokenValue; // object name
    fName := lValue;
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
  //else
    //Error
end;

constructor TResourceStream(Instance: THandle; ResName: String; ResType: PChar);
begin
  var lModule: rtl.HMODULE := 0;
  var lResName := ResName.ToCharArray;
  var lResource := rtl.FindResource(lModule, @lResName[0], LPCWSTR(rtl.RT_RCDATA));
  if lResource = nil then raise new Exception('Can not locale resource: ' + lResName);
  var lPointer := rtl.LockResource(lResource);
  var lSize := rtl.SizeofResource(lModule, lResource);
  Size := lSize;
  &Write(lPointer, lSize);
end;

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

    ':', '#', '=', '[', ']': begin
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
  result := Convert.ToDouble(fTokenValue);
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
  var lTotal := if length(lBytes) > 255 then 255 else length(lBytes);
  fStream.WriteData(Byte(lTotal));
  fStream.Write(lBytes, 0, lTotal);
end;

method TWriter.WriteString(aValue: string);
begin
  var lBytes := RemObjects.Elements.RTL.Encoding.UTF8.GetBytes(aValue);
  var lTotal := length(lBytes);
  fStream.WriteData(Byte(TValueType.vaUTF8String));
  fStream.WriteData(Byte(lTotal));
  fStream.Write(lBytes, lTotal);
end;

method TWriter.WriteIdent(aIdent: string);
begin
  fStream.WriteData(Byte(TValueType.vaIdent));
  WriteUTFf8Str(aIdent);
end;

method TWriter.WriteInteger(aValue: Integer);
begin
  if (aValue >= Byte.MinValue) and (aValue <= Byte.MaxValue) then begin
    fStream.WriteData(Byte(TValueType.vaInt8));
    fStream.WriteData(Byte(aValue));
  end
  else
    if (aValue >= SmallInt.MinValue) and (aValue <= SmallInt.MaxValue) then begin
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

end.