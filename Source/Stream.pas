namespace RemObjects.Elements.RTL.Delphi;

interface

{$GLOBALS ON}

uses
  RemObjects.Elements.RTL;

const
  fmCreate = $FF00;

type
  TSeekOrigin = public enum (soBeginning, soCurrent, soEnd) of Integer;

  TStream = public class(TObject)
  private
    method GetPosition: Int64;
    method SetPosition(const Pos: Int64);
    method &Skip(Amount: Integer): Integer;
    method CheckBufferRead(aBytesRead: LongInt; aSizeToRead: LongInt);
    method CheckBufferReadCount(aBytesRead: LongInt; aCount: LongInt);
  protected
    method GetSize: Int64; virtual;
    method SetSize(NewSize: LongInt); virtual;
    method SetSize(const NewSize: Int64); virtual;
  public
    method &Read(Buffer: TBytes; Offset, Count: LongInt): LongInt; virtual;
    method &Write(const Buffer: TBytes; Offset, Count: LongInt): LongInt; virtual;

    {$IF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
    method &Read(Buffer: Pointer; Count: LongInt): LongInt; virtual;
    method &Write(Buffer: Pointer; Count: LongInt): LongInt; virtual;
    {$ELSEIF WEBASSEMBLY}
    method Read1Byte: Byte;
    method Read2Bytes: UInt16;
    method Read4Bytes: UInt32;
    method Read8Bytes: UInt64;
    method Write1Byte(aValue: Byte);
    method Write2Bytes(aValue: UInt16);
    method Write4Bytes(aValue: UInt32);
    method Write8Bytes(aValue: UInt64);
    {$ENDIF}

    method &Read(var Buffer: TBytes; Count: LongInt): LongInt; inline;
    method &Write(const Buffer: TBytes; Count: LongInt): LongInt; inline;

    method ReadBytes(Count: LongInt): TBytes;
    method ReadData(const Buffer: TBytes; Count: LongInt): LongInt; inline;
    method ReadData(var Buffer: Int32): LongInt;
    method ReadData(var Buffer: Int32; Count: LongInt): LongInt;
    {$IF NOT COOPER}
    method ReadData(var Buffer: Boolean): LongInt;
    method ReadData(var Buffer: Boolean; Count: LongInt): LongInt;
    method ReadData(var Buffer: Byte): LongInt;
    method ReadData(var Buffer: Byte; Count: LongInt): LongInt;
    method ReadData(var Buffer: Char): LongInt;
    method ReadData(var Buffer: Char; Count: LongInt): LongInt;
    method ReadData(var Buffer: ShortInt): LongInt;
    method ReadData(var Buffer: ShortInt; Count: LongInt): LongInt;
    method ReadData(var Buffer: Int16): LongInt;
    method ReadData(var Buffer: Int16; Count: LongInt): LongInt;
    method ReadData(var Buffer: UInt16): LongInt;
    method ReadData(var Buffer: UInt16; Count: LongInt): LongInt;
    method ReadData(var Buffer: UInt32): LongInt;
    method ReadData(var Buffer: UInt32; Count: LongInt): LongInt;
    method ReadData(var Buffer: UInt64): LongInt;
    method ReadData(var Buffer: UInt64; Count: LongInt): LongInt;
    method ReadData(var Buffer: Int64): LongInt;
    method ReadData(var Buffer: Int64; Count: LongInt): LongInt;
    method ReadData(var Buffer: Single): LongInt;
    method ReadData(var Buffer: Single; Count: LongInt): LongInt;
    method ReadData(var Buffer: Double): LongInt;
    method ReadData(var Buffer: Double; Count: LongInt): LongInt;
    {$ENDIF}

    method WriteData(const Buffer: TBytes; Count: LongInt): LongInt;
    method WriteData(Buffer: Int32): LongInt;
    method WriteData(Buffer: Int32; Count: LongInt): LongInt;

    {$IF NOT COOPER}
    method WriteData(Buffer: Boolean): LongInt;
    method WriteData(Buffer: Boolean; Count: LongInt): LongInt;
    method WriteData(Buffer: Char): LongInt;
    method WriteData(Buffer: Char; Count: LongInt): LongInt;
    method WriteData(Buffer: ShortInt): LongInt;
    method WriteData(Buffer: ShortInt; Count: LongInt): LongInt;
    method WriteData(Buffer: Int16): LongInt;
    method WriteData(Buffer: Int16; Count: LongInt): LongInt;
    method WriteData(Buffer: Int64): LongInt;
    method WriteData(Buffer: Int64; Count: LongInt): LongInt;
    method WriteData(Buffer: Single): LongInt;
    method WriteData(Buffer: Single; Count: LongInt): LongInt;
    method WriteData(Buffer: Double): LongInt;
    method WriteData(Buffer: Double; Count: LongInt): LongInt;
    method WriteData(Buffer: Byte): LongInt;
    method WriteData(Buffer: Byte; Count: LongInt): LongInt;
    method WriteData(Buffer: UInt16): LongInt;
    method WriteData(Buffer: UInt16; Count: LongInt): LongInt;
    method WriteData(Buffer: UInt32): LongInt;
    method WriteData(Buffer: UInt32; Count: LongInt): LongInt;
    method WriteData(Buffer: UInt64): LongInt;
    method WriteData(Buffer: UInt64; Count: LongInt): LongInt;
    {$ENDIF}

    method Seek(Offset: LongInt; Origin: Word): LongInt; virtual;
    method Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual;
    method Seek(const Offset: Int64; Origin: Word): Int64; inline;

    method ReadBufferData(var Buffer: Int32);
    method ReadBufferData(var Buffer: Int32; Count: LongInt);
    {$IF NOT COOPER}
    method ReadBufferData(var Buffer: Boolean);
    method ReadBufferData(var Buffer: Boolean; Count: LongInt);
    method ReadBufferData(var Buffer: Char);
    method ReadBufferData(var Buffer: Char; Count: LongInt);
    method ReadBufferData(var Buffer: ShortInt);
    method ReadBufferData(var Buffer: ShortInt; Count: LongInt);
    method ReadBufferData(var Buffer: Byte);
    method ReadBufferData(var Buffer: Byte; Count: LongInt);
    method ReadBufferData(var Buffer: Int16);
    method ReadBufferData(var Buffer: Int16; Count: LongInt);
    method ReadBufferData(var Buffer: UInt16);
    method ReadBufferData(var Buffer: UInt16; Count: LongInt);
    method ReadBufferData(var Buffer: UInt32);
    method ReadBufferData(var Buffer: UInt32; Count: LongInt);
    method ReadBufferData(var Buffer: Int64);
    method ReadBufferData(var Buffer: Int64; Count: LongInt);
    method ReadBufferData(var Buffer: UInt64);
    method ReadBufferData(var Buffer: UInt64; Count: LongInt);
    method ReadBufferData(var Buffer: Single);
    method ReadBufferData(var Buffer: Single; Count: LongInt);
    method ReadBufferData(var Buffer: Double);
    method ReadBufferData(var Buffer: Double; Count: LongInt);
    {$ENDIF}

    method WriteBufferData(var Buffer: Integer; Count: LongInt);

    method ReadString(Count: LongInt; aEncoding: TEncoding := TEncoding.UTF16LE): DelphiString;
    method WriteString(aString: DelphiString; aEncoding: TEncoding := TEncoding.UTF16LE): LongInt;

    method CopyFrom(const Source: TStream; Count: Int64): Int64;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
  end;

  {$IFNDEF WEBASSEMBLY}
  THandleStream = public class(TStream)
  protected
    fHandle: THandle;
    method SetSize(NewSize: LongInt); override;
  public
    constructor(aHandle: THandle);
    method &Read(Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    method &Write(const Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    {$IF ISLAND OR TOFFEE}
    method &Read(Buffer: Pointer; Count: LongInt): LongInt; override;
    {$ENDIF}

    method SetSize(const NewSize: Int64); override;

    method Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read fHandle;
  end;

  TFileStream = public class(THandleStream)
  private
    fFileName: DelphiString;
  public
    constructor(const aFileName: DelphiString; Mode: Word);
    constructor(const aFileName: DelphiString; Mode: Word; Rights: Cardinal);
    finalizer;
    class method Create(const aFileName: DelphiString; Mode: Word): TFileStream; static;
    class method Create(const aFileName: DelphiString; Mode: Word; Rights: Cardinal): TFileStream; static;

    method Close;
    property FileName: DelphiString read fFileName;
  end;
  {$ENDIF}

  TCustomMemoryStream = public class(TStream)
  protected
    fData: MemoryStream;
  public
    constructor;
    class method Create: TCustomMemoryStream; static;
    method &Read(Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    {$IF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
    method &Read(Buffer: Pointer; Count: LongInt): LongInt; override;
    {$ENDIF}
    method Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    method SaveToStream(aStream: TStream); virtual;
    {$IF NOT WEBASSEMBLY}
    method SaveToFile(const aFileName: DelphiString);
    {$ENDIF}
    method &Write(const Buffer: TBytes; Offset, Count: LongInt): LongInt; override;
    property Memory: TBytes read fData.Bytes;
  end;

  TMemoryStream = public class(TCustomMemoryStream)
  protected
    property Capacity: LongInt read fData.Length write fData.SetLength;
  public
    constructor;
    class method Create: TCustomMemoryStream; static;
    method Clear;
    method LoadFromStream(aStream: TStream);
    {$IF NOT WEBASSEMBLY}
    method LoadFromFile(const aFileName: DelphiString);
    {$ENDIF}
    method SetSize(const NewSize: Int64); override;
    method SetSize(NewSize: LongInt); override;
  end;

  TBytesStream = public class(TMemoryStream)
  public
    constructor(const aBytes: TBytes);
    class method Create(const aBytes: TBytes): TBytesStream; static;
    property Bytes: TBytes read fData.Bytes;
  end;

implementation

method TStream.GetPosition: Int64;
begin
  result := Seek(0, TSeekOrigin.soCurrent);
end;

method TStream.SetPosition(const Pos: Int64);
begin
  Seek(Pos, TSeekOrigin.soBeginning);
end;

method TStream.GetSize: Int64;
begin
  var lPos := Seek(0, TSeekOrigin.soCurrent);
  result := Seek(0, TSeekOrigin.soEnd);
  Seek(lPos, TSeekOrigin.soBeginning);
end;

method TStream.SetSize(NewSize: LongInt);
begin
  // NO OP
end;

method TStream.SetSize(const NewSize: Int64);
begin
  SetSize(LongInt(NewSize));
end;

method TStream.Skip(Amount: Integer): Integer;
begin
  var lPos := Position;
  result := Seek(Amount, TSeekOrigin.soCurrent) - lPos;
end;

method TStream.CheckBufferRead(aBytesRead: LongInt; aSizeToRead: LongInt);
begin
  if aBytesRead <> aSizeToRead then
    raise new Exception('Error reading from stream');
end;

method TStream.CheckBufferReadCount(aBytesRead: LongInt; aCount: LongInt);
begin
  if (aCount <> 0) and (aBytesRead <> aCount) then
    raise new Exception('Error reading from stream');
end;

method TStream.Read(Buffer: TBytes; Offset: LongInt; Count: LongInt): LongInt;
begin
  result := 0;
end;

method TStream.Write(const Buffer: TBytes; Offset: LongInt; Count: LongInt): LongInt;
begin
  result := 0;
end;

{$IF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
method TStream.Read(Buffer: Pointer; Count: LongInt): LongInt;
begin
  result := 0;
end;

method TStream.Write(Buffer: Pointer; Count: LongInt): LongInt;
begin
  var lBuf := new Byte[Count];
  {$IF ISLAND}
  {$IFDEF WINDOWS}ExternalCalls.memcpy(@lBuf[0], Buffer, Count){$ELSEIF POSIX}rtl.memcpy(@lBuf[0], Buffer, Count){$ENDIF};
  {$ELSEIF TOFFEE}
  memcpy(@lBuf[0], Buffer, Count);
  {$ENDIF}
  result := &Write(lBuf, 0, Count);
end;
{$ELSEIF WEBASSEMBLY}
method TStream.Read1Byte: Byte;
begin
  var lBuf := new Byte[sizeOf(result)];
  &Read(lBuf, 0, lBuf.Length);
  result := lBuf[0];
end;

method TStream.Read2Bytes: UInt16;
begin
  var lBuf := new Byte[sizeOf(result)];
  &Read(lBuf, 0, lBuf.Length);
  result := lBuf[0] or (lBuf[1] shl 8);
end;

method TStream.Read4Bytes: UInt32;
begin
  var lBuf := new Byte[sizeOf(result)];
  &Read(lBuf, 0, lBuf.Length);
  result := lBuf[0] or (lBuf[1] shl 8) or (lBuf[2] shl 16) or (lBuf[3] shl 24);
end;

method TStream.Read8Bytes: UInt64;
begin
  var lBuf := new Byte[sizeOf(result)];
  &Read(lBuf, 0, lBuf.Length);
  result := lBuf[0] or (lBuf[1] shl 8) or (lBuf[2] shl 16) or (lBuf[3] shl 24) or
    (UInt64(lBuf[4]) shl 32) or (UInt64(lBuf[5]) shl 40) or (UInt64(lBuf[6]) shl 48) or (UInt64(lBuf[7]) shl 56);
end;

method TStream.Write1Byte(aValue: Byte);
begin
  var lBuf := new Byte[sizeOf(aValue)];
  lBuf[0] := aValue;
  &Write(lBuf, sizeOf(aValue));
end;

method TStream.Write2Bytes(aValue: UInt16);
begin
  var lBuf := new Byte[sizeOf(aValue)];
  lBuf[0] := aValue and $00FF;
  lBuf[1] := (aValue and $FF00) shr 8;
end;

method TStream.Write4Bytes(aValue: UInt32);
begin
end;

method TStream.Write8Bytes(aValue: UInt64);
begin

end;
{$ENDIF}

method TStream.Read(var Buffer: TBytes; Count: LongInt): LongInt;
begin
  result := &Read(Buffer, 0, Count);
end;

method TStream.Write(const Buffer: TBytes; Count: LongInt): LongInt;
begin
  result := &Write(Buffer, 0, Count);
end;

method TStream.ReadData(const Buffer: TBytes; Count: LongInt): LongInt;
begin
  result := &Read(Buffer, 0, Count);
end;

method TStream.ReadData(var Buffer: Int32): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF COOPER}
  var lTemp := java.nio.ByteBuffer.wrap(ReadBytes(result));
  Buffer := lTemp.getInt;
  {$ELSEIF ECHOES}
  Buffer := BitConverter.ToInt32(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WebAssembly}
  Buffer := Int32(Read4Bytes);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Int32; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadBytes(Count: LongInt): TBytes;
begin
  result := new Byte[Count];
  &Read(var result, Count);
end;

{$IF NOT COOPER}
method TStream.ReadData(var Buffer: Boolean): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToBoolean(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Boolean(Read1Byte);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Boolean; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: Byte): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  var lValue := ReadBytes(result);
  Buffer := lValue[0];
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Read1Byte;
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Byte; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: Char): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToChar(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Char(Read2Bytes);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Char; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: ShortInt): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  var lValue := ReadBytes(result);
  Buffer := ShortInt(lValue[0]);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := ShortInt(Read1Byte);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: ShortInt; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: Int16): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToInt16(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY)  OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Int16(Read2Bytes);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Int16; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: UInt16): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToUInt16(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Read2Bytes;
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: UInt16; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: UInt32): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToUInt32(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Read4Bytes;
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: UInt32; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: Int64): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToInt64(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Int64(Read8Bytes);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Int64; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: UInt64): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToUInt64(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Read4Bytes;
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: UInt64; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: Single): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToSingle(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Single(Read4Bytes);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Single; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;

method TStream.ReadData(var Buffer: Double): LongInt;
begin
  result := sizeOf(Buffer);
  {$IF ECHOES}
  Buffer := BitConverter.ToDouble(ReadBytes(result), 0);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  &Read(@Buffer, result);
  {$ELSEIF WEBASSEMBLY}
  Buffer := Double(Read8Bytes);
  {$ENDIF}
end;

method TStream.ReadData(var Buffer: Double; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := ReadData(var Buffer) + &Skip(Count - lSize)
  else
    result := ReadData(var Buffer);
end;
{$ENDIF}

method TStream.WriteData(const Buffer: TBytes; Count: LongInt): LongInt;
begin
  result := &Write(Buffer, 0, Count);
end;

method TStream.WriteData(Buffer: Int32): LongInt;
begin
  {$IF COOPER}
  result := sizeOf(Int32);
  var lTmp := java.nio.ByteBuffer.allocate(result);
  lTmp.putInt(Buffer);
  var lArray := lTmp.array;
  &Write(lArray, 0, result);
  {$ELSEIF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Int32; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

{$IF NOT COOPER}
method TStream.WriteData(Buffer: Boolean): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Boolean; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: Char): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Char; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: ShortInt): LongInt;
begin
  {$IF ECHOES}
  result := sizeOf(Buffer);
  var lBuf := new Byte[result];
  lBuf[0] := Buffer;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: ShortInt; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: Int16): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Int16; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: Int64): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Int64; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: Single): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Single; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: Double): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Double; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: Byte): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := sizeOf(Byte);
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: Byte; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: UInt16): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: UInt16; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: UInt32): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: UInt32; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;

method TStream.WriteData(Buffer: UInt64): LongInt;
begin
  {$IF ECHOES}
  var lBuf := BitConverter.GetBytes(Buffer);
  result := lBuf.Length;
  &Write(lBuf, 0, result);
  {$ELSEIF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
  result := &Write(@Buffer, sizeOf(Buffer));
  {$ELSEIF WEBASSEMBLY}
  {$ENDIF}
end;

method TStream.WriteData(Buffer: UInt64; Count: LongInt): LongInt;
begin
  var lSize := sizeOf(Buffer);
  if Count > lSize then
    result := WriteData(Buffer) + Skip(Count - lSize)
  else
    result := WriteData(Buffer);
end;
{$ENDIF}

method TStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;

method TStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result := 0;
end;

method TStream.Seek(const Offset: Int64; Origin: Word): Int64;
begin
  result := Seek(Offset, TSeekOrigin(Origin));
end;

method TStream.ReadBufferData(var Buffer: Int32);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Int32));
end;

method TStream.ReadBufferData(var Buffer: Int32; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

{$IF NOT COOPER}
method TStream.ReadBufferData(var Buffer: Boolean);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Boolean));
end;

method TStream.ReadBufferData(var Buffer: Boolean; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: Char);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Char));
end;

method TStream.ReadBufferData(var Buffer: Char; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: ShortInt);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(ShortInt));
end;

method TStream.ReadBufferData(var Buffer: ShortInt; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: Byte);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Byte));
end;

method TStream.ReadBufferData(var Buffer: Byte; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: Int16);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Int16));
end;

method TStream.ReadBufferData(var Buffer: Int16; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: UInt16);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(UInt16));
end;

method TStream.ReadBufferData(var Buffer: UInt16; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: UInt32);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(UInt32));
end;

method TStream.ReadBufferData(var Buffer: UInt32; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: Int64);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Int64));
end;

method TStream.ReadBufferData(var Buffer: Int64; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: UInt64);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(UInt64));
end;

method TStream.ReadBufferData(var Buffer: UInt64; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: Single);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Single));
end;

method TStream.ReadBufferData(var Buffer: Single; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;

method TStream.ReadBufferData(var Buffer: Double);
begin
  CheckBufferRead(ReadData(var Buffer), sizeOf(Double));
end;

method TStream.ReadBufferData(var Buffer: Double; Count: LongInt);
begin
  CheckBufferRead(ReadData(var Buffer, Count), Count);
end;
{$ENDIF}

method TStream.WriteBufferData(var Buffer: Integer; Count: LongInt);
begin
  WriteData(Buffer, Count);
end;

method TStream.ReadString(Count: LongInt; aEncoding: TEncoding := TEncoding.UTF16LE): Delphistring;
begin
  var lTotal := if Count > Size - Position then Size - Position else Count;
  var lBytes := new Byte[lTotal];
  &Read(var lBytes, lTotal);
  result := aEncoding.GetString(lBytes);
end;

method TStream.WriteString(aString: DelphiString; aEncoding: TEncoding := TEncoding.UTF16LE): LongInt;
begin
  var lBytes := aEncoding.GetBytes(aString);
  &Write(lBytes, 0, length(lBytes));
end;

method TStream.CopyFrom(const Source: TStream; Count: Int64): Int64;
const
  bufSize = 4 * 1024;
begin
  if Source = nil then raise new Exception('Source is null');
  var lBuf := new Byte[bufSize];
  while true do begin
    var lRest := Source.Read(var lBuf, bufSize);
    if lRest > 0 then lRest := &Write(lBuf, lRest);
    if lRest <> bufSize then break;
  end;
end;

{$IFNDEF WEBASSEMBLY}
method THandleStream.SetSize(NewSize: LongInt);
begin
  SetSize(Int64(NewSize));
end;

method THandleStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, TSeekOrigin.soBeginning);
end;

constructor THandleStream(aHandle: THandle);
begin
  inherited constructor;
  fHandle := aHandle;
end;

method THandleStream.Read(Buffer: TBytes; Offset: LongInt; Count: LongInt): LongInt;
begin
  result := FileRead(fHandle, var Buffer, Offset, Count);
end;

method THandleStream.Write(const Buffer: TBytes; Offset: LongInt; Count: LongInt): LongInt;
begin
  result := FileWrite(fHandle, Buffer, Offset, Count);
end;

{$IF ISLAND OR TOFFEE}
method THandleStream.Read(Buffer: Pointer; Count: LongInt): LongInt;
begin
  var lBuf := new Byte[Count];
  result := &Read(lBuf, 0, Count);
  {$IF ISLAND}
  {$IFDEF WINDOWS}ExternalCalls.memcpy(Buffer, @lBuf[0], Count){$ELSEIF POSIX}rtl.memcpy(Buffer, @lBuf[0], Count){$ENDIF};
  {$ELSEIF TOFFEE}
  memcpy(Buffer, @lBuf[0], Count);
  {$ENDIF}
end;
{$ENDIF}

method THandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result := FileSeek(fHandle, Offset, Integer(Origin));
end;

constructor TFileStream(const aFileName: DelphiString; Mode: Word);
begin
  constructor(aFileName, Mode, 0);
end;

constructor TFileStream(const aFileName: DelphiString; Mode: Word; Rights: Cardinal);
begin
  var lHandle: THandle;
  if Mode and fmCreate <> 0 then
    lHandle := FileCreate(aFileName, Mode, Rights)
  else
    lHandle := FileOpen(aFileName, Mode);
  inherited constructor(lHandle);
  fFileName := aFileName;
end;

class method TFileStream.Create(const aFileName: DelphiString; Mode: Word): TFileStream;
begin
  result := new TFileStream(aFileName, Mode);
end;

class method TFileStream.Create(const aFileName: DelphiString; Mode: Word; Rights: Cardinal): TFileStream;
begin
  result := new TFileStream(aFileName, Mode, Rights);
end;

finalizer TFileStream;
begin
  Close;
end;

method TFileStream.Close;
begin
  if fHandle <> INVALID_HANDLE_VALUE then begin
    FileClose(fHandle);
    fHandle := INVALID_HANDLE_VALUE;
  end;
end;
{$ENDIF}

constructor TCustomMemoryStream;
begin
  fData := new MemoryStream();
end;

class method TCustomMemoryStream.Create: TCustomMemoryStream;
begin
  result := new TCustomMemoryStream();
end;

method TCustomMemoryStream.Read(Buffer: TBytes; Offset: LongInt; Count: LongInt): LongInt;
begin
  result := fData.read(Buffer, Offset, Count);
end;

{$IF (ISLAND AND NOT WEBASSEMBLY) OR TOFFEE}
method TCustomMemoryStream.&Read(Buffer: Pointer; Count: LongInt): LongInt;
begin
  {$IF ISLAND}
  {$IFDEF WINDOWS}ExternalCalls.memcpy(Buffer, @fData.Bytes[Position], Count){$ELSEIF POSIX}rtl.memcpy(Buffer, @fData.Bytes[Position], Count){$ENDIF};
  {$ELSEIF TOFFEE}
  memcpy(Buffer, @fData.Bytes[Position], Count);
  {$ENDIF}
  fData.Position := fData.Position + Count;
  result := Count;
end;
{$ENDIF}

method TCustomMemoryStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  var lOrigin: SeekOrigin;
  case Origin of
    TSeekOrigin.soBeginning: lOrigin := SeekOrigin.Begin;
    TSeekOrigin.soCurrent: lOrigin := SeekOrigin.Current;
    TSeekOrigin.soEnd: lOrigin := SeekOrigin.End;
  end;
  result := fData.Seek(Offset, lOrigin);
end;

method TCustomMemoryStream.SaveToStream(aStream: TStream);
begin
  aStream.Write(fData.Bytes, 0, fData.Length);
end;

{$IF NOT WEBASSEMBLY}
method TCustomMemoryStream.SaveToFile(const aFileName: DelphiString);
begin
  var lTmp := new TFileStream(aFileName, fmCreate or fmOpenWrite);
  SaveToStream(lTmp);
end;
{$ENDIF}

method TCustomMemoryStream.Write(const Buffer: TBytes; Offset: LongInt; Count: LongInt): LongInt;
begin
  result := fData.Write(Buffer, Offset, Count);
end;

constructor TMemoryStream;
begin
  inherited;
end;

class method TMemoryStream.Create: TCustomMemoryStream;
begin
  result := new TMemoryStream();
end;

method TMemoryStream.Clear;
begin
  fData.SetLength(0);
end;

method TMemoryStream.LoadFromStream(aStream: TStream);
begin
  SetSize(aStream.Size);
  Position := 0;
  CopyFrom(aStream, aStream.Size);
end;

{$IF NOT WEBASSEMBLY}
method TMemoryStream.LoadFromFile(const aFileName: DelphiString);
begin
  var lTmp := new TFileStream(aFileName, fmOpenRead);
  LoadFromStream(lTmp);
end;
{$ENDIF}

method TMemoryStream.SetSize(const NewSize: Int64);
begin
  fData.SetLength(NewSize);
end;

method TMemoryStream.SetSize(NewSize: LongInt);
begin
  fData.SetLength(NewSize);
end;

constructor TBytesStream(const aBytes: TBytes);
begin
  fData := new MemoryStream(aBytes.Length);
  fData.Write(aBytes, aBytes.Length);
end;

class method TBytesStream.Create(const aBytes: TBytes): TBytesStream;
begin
  result := new TBytesStream(aBytes);
end;
//{$ENDIF}


end.