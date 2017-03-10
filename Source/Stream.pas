namespace RemObjects.Elements.RTL.Delphi;

interface

type
  TSeekOrigin = (soBeginning, soCurrent, soEnd);

  /*TStream = public class(TObject)
  private
    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    procedure SetSize64(const NewSize: Int64);
  protected
    function GetSize: Int64; virtual;
    procedure SetSize(NewSize: Longint); virtual;
    procedure SetSize(const NewSize: Int64); virtual;
  public
    function &Read(Buffer: TBytes; Offset, Count: Longint): Longint; virtual;
    function &Write(const Buffer: TBytes; Offset, Count: Longint): Longint; virtual;

    function &Read(var Buffer: TBytes; Count: Longint): Longint;
    function &Write(const Buffer: TBytes; Count: Longint): Longint;

    function ReadData(const Buffer: TBytes; Count: Longint): Longint;
    function ReadData(var Buffer: Boolean): Longint;
    function ReadData(var Buffer: Boolean; Count: Longint): Longint;
    function ReadData(var Buffer: AnsiChar): Longint;
    function ReadData(var Buffer: AnsiChar; Count: Longint): Longint;
    function ReadData(var Buffer: Char): Longint;
    function ReadData(var Buffer: Char; Count: Longint): Longint;
    function ReadData(var Buffer: ShortInt): Longint;
    function ReadData(var Buffer: ShortInt; Count: Longint): Longint;
    function ReadData(var Buffer: Byte): Longint;
    function ReadData(var Buffer: Byte; Count: Longint): Longint;
    function ReadData(var Buffer: Int16): Longint;
    function ReadData(var Buffer: Int16; Count: Longint): Longint;
    function ReadData(var Buffer: UInt16): Longint;
    function ReadData(var Buffer: UInt16; Count: Longint): Longint;
    function ReadData(var Buffer: Int32): Longint;
    function ReadData(var Buffer: Int32; Count: Longint): Longint;
    function ReadData(var Buffer: UInt32): Longint;
    function ReadData(var Buffer: UInt32; Count: Longint): Longint;
    function ReadData(var Buffer: Int64): Longint;
    function ReadData(var Buffer: Int64; Count: Longint): Longint;
    function ReadData(var Buffer: UInt64): Longint;
    function ReadData(var Buffer: UInt64; Count: Longint): Longint;
    function ReadData(var Buffer: Single): Longint;
    function ReadData(var Buffer: Single; Count: Longint): Longint;
    function ReadData(var Buffer: Double): Longint;
    function ReadData(var Buffer: Double; Count: Longint): Longint;
    function ReadData(var Buffer: Extended): Longint;
    function ReadData(var Buffer: Extended; Count: Longint): Longint;

    function WriteData(const Buffer: TBytes; Count: Longint): Longint;

    function WriteData(const Buffer: Boolean): Longint;
    function WriteData(const Buffer: Boolean; Count: Longint): Longint;
    function WriteData(const Buffer: AnsiChar): Longint;
    function WriteData(const Buffer: AnsiChar; Count: Longint): Longint;
    function WriteData(const Buffer: Char): Longint;
    function WriteData(const Buffer: Char; Count: Longint): Longint;
    function WriteData(const Buffer: ShortInt): Longint;
    function WriteData(const Buffer: ShortInt; Count: Longint): Longint;
    function WriteData(const Buffer: Byte): Longint;
    function WriteData(const Buffer: Byte; Count: Longint): Longint;
    function WriteData(const Buffer: Int16): Longint;
    function WriteData(const Buffer: Int16; Count: Longint): Longint;
    function WriteData(const Buffer: UInt16): Longint;
    function WriteData(const Buffer: UInt16; Count: Longint): Longint;
    function WriteData(const Buffer: Int32): Longint;
    function WriteData(const Buffer: Int32; Count: Longint): Longint;
    function WriteData(const Buffer: UInt32): Longint;
    function WriteData(const Buffer: UInt32; Count: Longint): Longint;
    function WriteData(const Buffer: Int64): Longint;
    function WriteData(const Buffer: Int64; Count: Longint): Longint;
    function WriteData(const Buffer: UInt64): Longint;
    function WriteData(const Buffer: UInt64; Count: Longint): Longint;
    function WriteData(const Buffer: Single): Longint;
    function WriteData(const Buffer: Single; Count: Longint): Longint;
    function WriteData(const Buffer: Double): Longint;
    function WriteData(const Buffer: Double; Count: Longint): Longint;
    function WriteData(const Buffer: Extended): Longint;
    function WriteData(const Buffer: Extended; Count: Longint): Longint;

    function Seek(Offset: Longint; Origin: Word): Longint; virtual;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual;
    function Seek(const Offset: Int64; Origin: Word): Int64; deprecated; inline;

    procedure ReadBuffer(var Buffer: TBytes; Count: Longint);
    procedure ReadBuffer(var Buffer: TBytes; Offset, Count: Longint);

    procedure ReadBufferData(var Buffer: Boolean);
    procedure ReadBufferData(var Buffer: Boolean; Count: Longint);
    procedure ReadBufferData(var Buffer: AnsiChar);
    procedure ReadBufferData(var Buffer: AnsiChar; Count: Longint);
    procedure ReadBufferData(var Buffer: Char);
    procedure ReadBufferData(var Buffer: Char; Count: Longint);
    procedure ReadBufferData(var Buffer: ShortInt);
    procedure ReadBufferData(var Buffer: ShortInt; Count: Longint);
    procedure ReadBufferData(var Buffer: Byte);
    procedure ReadBufferData(var Buffer: Byte; Count: Longint);
    procedure ReadBufferData(var Buffer: Int16);
    procedure ReadBufferData(var Buffer: Int16; Count: Longint);
    procedure ReadBufferData(var Buffer: UInt16);
    procedure ReadBufferData(var Buffer: UInt16; Count: Longint);
    procedure ReadBufferData(var Buffer: Int32);
    procedure ReadBufferData(var Buffer: Int32; Count: Longint);
    procedure ReadBufferData(var Buffer: UInt32);
    procedure ReadBufferData(var Buffer: UInt32; Count: Longint);
    procedure ReadBufferData(var Buffer: Int64);
    procedure ReadBufferData(var Buffer: Int64; Count: Longint);
    procedure ReadBufferData(var Buffer: UInt64);
    procedure ReadBufferData(var Buffer: UInt64; Count: Longint);
    procedure ReadBufferData(var Buffer: Single);
    procedure ReadBufferData(var Buffer: Single; Count: Longint);
    procedure ReadBufferData(var Buffer: Double);
    procedure ReadBufferData(var Buffer: Double; Count: Longint);
    procedure ReadBufferData(var Buffer: Extended);
    procedure ReadBufferData(var Buffer: Extended; Count: Longint);

    procedure WriteBuffer(const Buffer: TBytes; Count: Longint);
    procedure WriteBuffer(const Buffer: TBytes; Offset, Count: Longint);

    procedure WriteBufferData(var Buffer: Integer; Count: Longint);

    function CopyFrom(const Source: TStream; Count: Int64): Int64;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
  end;

  THandleStream = class(TStream)
  protected
    FHandle: THandle;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: THandle);
    function &Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function &Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read FHandle;
  end;


  TFileStream = class(THandleStream)
  private
    FFileName: string;
  public
    constructor(const AFileName: string; Mode: Word);
    constructor(const AFileName: string; Mode: Word; Rights: Cardinal);
    property FileName: string read FFileName;
  end;

  TCustomMemoryStream = class(TStream)
  private
    //FMemory: Pointer;
    FSize, FPosition: NativeInt;
  protected
    //procedure SetPointer(Ptr: Pointer; const Size: NativeInt);
  public
    function &Read(var Buffer; Count: Longint): Longint; override;
    function &Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string);
    //property Memory: Pointer read FMemory;
  end;

{ TMemoryStream }

  TMemoryStream = class(TCustomMemoryStream)
  private
    FCapacity: Longint;
    procedure SetCapacity(NewCapacity: Longint);
  protected
    //function Realloc(var NewCapacity: Longint): Pointer; virtual;
    property Capacity: Longint read FCapacity write SetCapacity;
  public
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
  end;

{ TBytesStream }

  TBytesStream = class(TMemoryStream)
  private
    FBytes: TBytes;
  protected
    //function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    constructor(const ABytes: TBytes);
    property Bytes: TBytes read FBytes;
  end;

*/

implementation


end.
