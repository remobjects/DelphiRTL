namespace RemObjects.Elements.RTL.Delphi;

interface

uses 
  RemObjects.Elements.RTL;

{$GLOBALS ON}

const
  fmOpenRead = $0000;
  fmOpenWrite = $0001;
  fmOpenReadWrite = $0002;
  fmExclusive = $0004;

  fmShareCompat = $0000; 
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead = $0030;
  fmShareDenyNone = $0040;

  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  SecsPerHour   = SecsPerMin * MinsPerHour;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;  
  DateDelta = 693594;

type
  TBytes = public TArray<Byte>;
  TEncoding = public Encoding;

  TSysLocale = public record
  public
    DefaultLCID: TLocaleID;
    PriLangID: Integer;
    SubLangID: Integer;
    FarEast: Boolean;
    MiddleEast: Boolean;
  end;

  TTimeStamp = public record
    Time: Integer;      
    Date: Integer;      
  end;

  TFormatSettings = public record
  public
    CurrencyString: String;
    CurrencyFormat: Byte;
    CurrencyDecimals: Byte;
    DateSeparator: Char;
    TimeSeparator: Char;
    ListSeparator: Char;
    ShortDateFormat: String;
    LongDateFormat: String;
    TimeAMString: String;
    TimePMString: String;
    ShortTimeFormat: String;
    LongTimeFormat: String;
/*    ShortMonthNames: array[1..12] of string;
    LongMonthNames: array[1..12] of string;
    ShortDayNames: array[1..7] of string;
    LongDayNames: array[1..7] of string;*/
    //EraInfo: array of TEraInfo;
    ThousandSeparator: Char;
    DecimalSeparator: Char;
    TwoDigitYearCenturyWindow: Word;
    NegCurrFormat: Byte;
    NormalizedLocaleName: String;
    //class function Create: TFormatSettings; static; inline;
    //class function Create(Locale: TLocaleID): TFormatSettings; static;
    //class function Create(const LocaleName: string): TFormatSettings; static;
    //class function Invariant: TFormatSettings; static;
    class constructor;
  end;

var  
  FormatSettings: TFormatSettings;
  SysLocale: TSysLocale;


// File functions

function FileOpen(const FileName: DelphiString; Mode: Cardinal): THandle;
function FileCreate(const FileName: String): THandle;
function FileCreate(const FileName: String; Rights: Integer): THandle;
function FileCreate(const FileName: String; Mode: Cardinal; Rights: Integer): THandle;
/*function FileSystemAttributes(const Path: string): TFileSystemAttributes;

function FileRead(Handle: THandle; var Buffer; Count: Cardinal): Integer;
function FileWrite(Handle: THandle; const Buffer; Count: Cardinal): Integer; */
function FileRead(Handle: THandle; var Buffer: array of Byte; Offset, Count: Cardinal): Integer; 
function FileWrite(Handle: THandle; const Buffer: array of Byte; Offset, Count: Cardinal): Integer; 
function FileSeek(Handle: THandle; Offset, Origin: Integer): Integer;
function FileSeek(Handle: THandle; const Offset: Int64; Origin: Integer): Int64;
procedure FileClose(Handle: THandle);
/*function FileAge(const FileName: string; out FileDateTime: TDateTime;
  FollowLink: Boolean = True): Boolean; 
*/
function FileExists(const FileName: DelphiString; FollowLink: Boolean := true): Boolean;
function DirectoryExists(const Directory: DelphiString; FollowLink: Boolean := true): Boolean;
/*
function ForceDirectories(Dir: string): Boolean;

function FindFirst(const Path: string; Attr: Integer;   var F: TSearchRec): Integer;
function FindNext(var F: TSearchRec): Integer;
procedure FindClose(var F: TSearchRec);

function FileGetDate(Handle: THandle): LongInt;
function FileGetDateTimeInfo(const FileName: string;
  out DateTime: TDateTimeInfoRec; FollowLink: Boolean = True): Boolean;
function FileSetDate(const FileName: string; Age: LongInt): Integer; 

function FileSetDate(Handle: THandle; Age: Integer): Integer;  platform;
function FileGetAttr(const FileName: string; FollowLink: Boolean = True): Integer; platform;

function FileSetAttr(const FileName: string; Attr: Integer; FollowLink: Boolean = True): Integer; platform;
function FileIsReadOnly(const FileName: string): Boolean;
function FileSetReadOnly(const FileName: string; ReadOnly: Boolean): Boolean;
*/
function DeleteFile(const FileName: DelphiString): Boolean;
function RenameFile(const OldName, NewName: DelphiString): Boolean;
function ChangeFileExt(const FileName, aExtension: DelphiString): DelphiString; 
/*
function ChangeFilePath(const FileName, Path: string): string; 
function ExtractFilePath(const FileName: string): string; 
function ExtractFileDir(const FileName: string): string; 
function ExtractFileDrive(const FileName: string): string; 
*/
function ExtractFileName(const FileName: DelphiString): DelphiString;
function ExtractFileExt(const FileName: DelphiString): DelphiString;
function GetHomePath: DelphiString;
function ExpandFileName(const FileName: DelphiString): DelphiString;
/*
function ExpandUNCFileName(const FileName: string): string; 
function ExtractRelativePath(const BaseName, DestName: string): string; 
function IsRelativePath(const Path: string): Boolean;
function ExtractShortPathName(const FileName: string): string; 
function FileSearch(const Name, DirList: string): string;
function DiskFree(Drive: Byte): Int64;
function DiskSize(Drive: Byte): Int64;
function FileDateToDateTime(FileDate: LongInt): TDateTime;
function DateTimeToFileDate(DateTime: TDateTime): LongInt; */
function GetCurrentDir: DelphiString;
//function SetCurrentDir(const Dir: string): Boolean;
function CreateDir(const Dir: DelphiString): Boolean;
function RemoveDir(const Dir: DelphiString): Boolean;


implementation

type
TInternalFileHandles = static class
private
  {$IF ISLAND}
  class var fLock: Monitor := new Monitor;
  {$ELSE}
  class var fLock: Object := new Object;
  {$ENDIF}
  class var fSlots: array of Object;
  class var fFirstEmpty: Integer := 0; 
  class var fFirstFree: Integer := -1;
  class method CopyArray(oldArray: array of Object; var newArray: array of Object);
  begin
    for i: Integer := 0 to length(oldArray) - 1 do
      newArray[i] := oldArray[i];
  end;
public
  class method Allocate(aValue: Object): Int64;
  begin 
    locking fLock do begin 
      if (fFirstFree = -1) and (fFirstEmpty >= length(fSlots)) then begin 
        var lNewArray := new Object[length(fSlots) + 128];        
        CopyArray(fSlots, var lNewArray);
        fSlots := lNewArray;
      end;
      if fFirstEmpty < length(fSlots) then begin
        fSlots[fFirstEmpty] := aValue;
        inc(fFirstEmpty);
        exit fFirstEmpty; // +1 value since gchandle 0 is reserved.
      end;
      if fFirstFree <> -1 then begin 
        result := fFirstFree;
        var lFirstFree := Int64(fSlots[result]);
        fFirstFree := lFirstFree;
        fSlots[result] := aValue;
        inc(result);
        exit;
      end;
    end;
  end;
    
  class method Get(aValue: Int64): Object;
  begin
    dec(aValue);
    result := fSlots[aValue];
  end;

  class method Free(aValue: Int64);
  begin 
    dec(aValue);
    locking fLock do begin 
      fSlots[aValue] := fFirstFree;
      fFirstFree := aValue;
    end;
  end;
end;


///////////////////////////////////////////////////////////////////// File functions

function DeleteFile(const FileName: DelphiString): Boolean;
begin
  result := true;
  try
    FileUtils.Delete(FileName);
  except
    result := false;
  end;
end;

function RenameFile(const OldName, NewName: DelphiString): Boolean;
begin
  result := true;
  try
    FileUtils.Move(OldName, NewName);
  except
    result := false;
  end;
end;

function ChangeFileExt(const FileName, aExtension: DelphiString): DelphiString;
begin
  result := Path.ChangeExtension(FileName, aExtension);
end;

function ExtractFileName(const FileName: DelphiString): DelphiString;
begin
  result := Path.GetFileName(FileName);
end;

function ExtractFileExt(const FileName: DelphiString): DelphiString;
begin
  result := Path.GetExtension(FileName);
end;

function GetHomePath: DelphiString;
begin
  result := Environment.UserHomeFolder.FullPath;
end;

function ExpandFileName(const FileName: DelphiString): DelphiString;
begin
  result := Path.GetFullPath(FileName);
end;

function GetCurrentDir: DelphiString;
begin
  result := Environment.CurrentDirectory;
end;

function CreateDir(const Dir: DelphiString): Boolean;
begin
  result := true;
  try
    var lFolder := new Folder(Dir);
    lFolder.Create;
    //Folder.Create(Dir); // compiler error on isLand
  except
    result := false;
  end;
end;

function RemoveDir(const Dir: DelphiString): Boolean;
begin
  result := true;
  try
    var lFolder := new Folder(Dir);
    lFolder.Delete;
    //Folder.Delete(Dir);
  except
    result := false;
  end;
end;

function FileOpen(const FileName: DelphiString; Mode: Cardinal): THandle;
begin
  var lMode: FileOpenMode;
  if Mode = fmOpenRead then
    lMode := FileOpenMode.ReadOnly;
  if ((Mode and fmOpenWrite) <> 0) or ((Mode and fmOpenReadWrite) <> 0) then
    lMode := FileOpenMode.ReadWrite;

  var lHandle := new FileHandle(FileName, lMode);
  result := TInternalFileHandles.Allocate(lHandle);
end;

function FileCreate(const FileName: String): THandle;
begin
  var lHandle := new FileHandle(FileName, FileOpenMode.Create);
  result := TInternalFileHandles.Allocate(lHandle);
end;

function FileCreate(const FileName: String; Rights: Integer): THandle;
begin
  result := FileCreate(FileName);
end;

function FileCreate(const FileName: String; Mode: Cardinal; Rights: Integer): THandle;
begin
  result := FileCreate(FileName);
end;

function FileRead(Handle: THandle; var Buffer: array of Byte; Offset, Count: Cardinal): Integer; 
begin
  var lHandle := FileHandle(TInternalFileHandles.Get(Handle));
  result := lHandle.Read(Buffer, Offset, Count);
end;

function FileWrite(Handle: THandle; const Buffer: array of Byte; Offset, Count: Cardinal): Integer; 
begin
  var lHandle := FileHandle(TInternalFileHandles.Get(Handle));
  lHandle.Write(Buffer, Offset, Count);
  result := Count;
end;

function FileSeek(Handle: THandle; Offset, Origin: Integer): Integer; 
begin
  result := FileSeek(Handle, Int64(Offset), Origin);
end;

function FileSeek(Handle: THandle; const Offset: Int64; Origin: Integer): Int64;
begin
  var lHandle := FileHandle(TInternalFileHandles.Get(Handle));
  var lOrigin: SeekOrigin;
  case Origin of
    0: lOrigin := SeekOrigin.Begin;
    1: lOrigin := SeekOrigin.Current;
    2: lOrigin := SeekOrigin.End;
  end;
  lHandle.Seek(Offset, lOrigin);
  result := lHandle.Position;
end;

procedure FileClose(Handle: THandle);
begin
  TInternalFileHandles.Free(Handle);
end;

function FileExists(const FileName: DelphiString; FollowLink: Boolean := true): Boolean;
begin
  try
    result := FileUtils.Exists(FileName);
  except
    result := false;
  end;
end;

function DirectoryExists(const Directory: DelphiString; FollowLink: Boolean := true): Boolean;
begin
  result := true;
  try
    var lFolder := new Folder(Directory);
    result := lFolder.Exists;
    //result := Folder.Exists(Directory);    
  except
    result := false;
  end;
end;

class constructor TFormatSettings;
begin
  SysLocale.DefaultLCID := Locale.Current;

end;


end.
