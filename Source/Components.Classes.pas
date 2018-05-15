namespace ProjectRCL;

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
    fLookupRoot: TComponent;
    fAncestor: TPersistent;
    fIgnoreChildren: Boolean;
  protected
    fStream: TStream;
    method SetRoot(Value: TComponent); virtual;
  public
    constructor(aStream: TStream; BufSize: Integer);
    //destructor Destroy; override;
    method DefineProperty(Name: String; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); virtual; abstract;
    method DefineBinaryProperty(Name: String; ReadData, WriteData: TStreamProc; HasData: Boolean); virtual; abstract;
    method FlushBuffer; virtual; abstract;
    property Root: TComponent read fRoot write SetRoot;
    property LookupRoot: TComponent read fLookupRoot;
    property Ancestor: TPersistent read fAncestor write fAncestor;
    property IgnoreChildren: Boolean read fIgnoreChildren write fIgnoreChildren;
  end;

  TReader = public class(TFiler)
  private
    // TODO change name
    method ReadComponentData(aInstance: TComponent);
  protected
    method ReadProperty(aInstance: TComponent /*TPersistent*/);
  public
    const FilerSignature: UInt32 = $30465054; // 'TPF0'
    method EndOfList: Boolean;
    method ReadComponent(Component: TComponent): TComponent;
    method ReadData(Instance: TComponent);
    method ReadRootComponent(Root: TComponent): TComponent;
    method ReadSignature;
    method ReadStr: String;
    method ReadValue: TValueType;

  end;

  TWriter = public class(TObject)
  end;

implementation

method TFiler.SetRoot(Value: TComponent);
begin

end;

constructor TFiler(aStream: TStream; BufSize: Integer);
begin
  fStream := aStream;
end;

method TReader.ReadComponentData(aInstance: TComponent);
begin
  while not EndOfList do ReadProperty(aInstance);

end;

method TReader.ReadProperty(aInstance: TComponent/*TPersistent*/);
begin
  var lName := ReadStr;

end;

method TReader.EndOfList: Boolean;
begin
  result := ReadValue = TValueType.vaNull;
  fStream.Position := fStream.Position - 1;
end;

method TReader.ReadRootComponent(Root: TComponent): TComponent;
begin
  // Skip 'TPF0'
  ReadSignature;
  ReadStr;
  Root.Name := ReadStr;
  ReadComponentData(Root);
end;

method TReader.ReadData(Instance: TComponent);
begin

end;

method TReader.ReadComponent(Component: TComponent): TComponent;
begin
  ReadStr;
  Root.Name := ReadStr;
  ReadComponentData(Root);
  // TODO
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


end.