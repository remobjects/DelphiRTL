namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  {$IF ISLAND}
  RemObjects.Elements.System,
  {$ENDIF}
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
  FMSecsPerDay: Double = MSecsPerDay;
  DateDelta = 693594;
  UnixDateDelta = 25569;

type
  {$IF COOPER}
  TBytes = public array of Byte;
  {$ELSE}
  TBytes = public TArray<Byte>;
  {$ENDIF}
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
  public
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

  TArchitecture = public (arIntelX86, arIntelX64, arARM32, arARM64);
  TPlatform = public (pfWindows, pfMacOS, pfiOS, pfAndroid, pfWinRT, pfLinux, pfNET, pfJava, pfTVOS, pfWatchOS);

  TOSVersion = public record
  private
    class var fArchitecture: TArchitecture;
    class var fBuild: Integer;
    class var fMajor: Integer;
    class var fMinor: Integer;
    class var fName: String;
    class var fPlatform: TPlatform;
    class var fServicePackMajor: Integer;
    class var fServicePackMinor: Integer;    
    class var fPathDelim: String;
    class var fDriveDelim: String;
    class var fPathSep: String;
    {$IF ISLAND AND WINDOWS}class method GetOSInfo; static;{$ENDIF} 
    class constructor;
  public
    class method Check(aMajor: Integer): Boolean; static;
    class method Check(aMajor, aMinor: Integer): Boolean; static;
    class method Check(aMajor, aMinor, aServicePackMajor: Integer): Boolean; static;
    class method ToString: DelphiString; static;
    class property Architecture: TArchitecture read fArchitecture;
    class property Build: Integer read fBuild;
    class property Major: Integer read fMajor;
    class property Minor: Integer read fMinor;
    class property Name: String read fName;
    class property Platform: TPlatform read FPlatform;
    class property ServicePackMajor: Integer read fServicePackMajor;
    class property ServicePackMinor: Integer read fServicePackMinor;
    class property PathDelim: String read fPathDelim;
    class property DriveDelim: String read fDriveDelim;
    class property PathSep: String read fPathSep;
  end;

var
  FormatSettings: TFormatSettings;
  SysLocale: TSysLocale;

{ File functions }
function FileOpen(const FileName: DelphiString; Mode: Cardinal): THandle;
function FileCreate(const FileName: String): THandle;
function FileCreate(const FileName: String; Rights: Integer): THandle;
function FileCreate(const FileName: String; Mode: Cardinal; Rights: Integer): THandle;
function FileRead(Handle: THandle; var Buffer: array of Byte; Offset, Count: Cardinal): Integer;
function FileWrite(Handle: THandle; const Buffer: array of Byte; Offset, Count: Cardinal): Integer;
function FileSeek(Handle: THandle; Offset, Origin: Integer): Integer;
function FileSeek(Handle: THandle; const Offset: Int64; Origin: Integer): Int64;
procedure FileClose(Handle: THandle);
function FileExists(const FileName: DelphiString; FollowLink: Boolean := true): Boolean;
function DirectoryExists(const Directory: DelphiString; FollowLink: Boolean := true): Boolean;
function ForceDirectories(Dir: DelphiString): Boolean;
function DeleteFile(const FileName: DelphiString): Boolean;
function RenameFile(const OldName, NewName: DelphiString): Boolean;
function ChangeFileExt(const FileName, aExtension: DelphiString): DelphiString;
function ChangeFilePath(const FileName, Path: DelphiString): DelphiString;
function ExtractFilePath(const FileName: DelphiString): DelphiString;
function ExtractFileDir(const FileName: DelphiString): DelphiString;
function ExtractFileDrive(const FileName: DelphiString): DelphiString;
function IsDelimiter(const Delimiters, S: DelphiString; aIndex: Integer): Boolean; inline;
function LastDelimiter(const Delimiters, S: DelphiString): Integer; inline;
function IncludeTrailingPathDelimiter(const S: DelphiString): DelphiString;
function ExcludeTrailingPathDelimiter(const S: DelphiString): DelphiString;
function ExtractFileName(const FileName: DelphiString): DelphiString;
function ExtractFileExt(const FileName: DelphiString): DelphiString;
function GetHomePath: DelphiString;
function ExpandFileName(const FileName: DelphiString): DelphiString;
function IsRelativePath(const Path: DelphiString): Boolean;
function GetCurrentDir: DelphiString;
function CreateDir(const Dir: DelphiString): Boolean;
function RemoveDir(const Dir: DelphiString): Boolean;

{$IF ISLAND AND WINDOWS}
function FileSetAttr(const FileName: DelphiString; Attr: Integer; FollowLink: Boolean := True): Integer;
//function FileSetDate(Handle: THandle; Age: Integer): Integer;
function FileGetAttr(const FileName: DelphiString; FollowLink: Boolean := True): Integer;
function DiskFree(Drive: Byte): Int64;
function DiskSize(Drive: Byte): Int64;
{$ENDIF}
/*
{$IF ISLAND}
function FileAge(const FileName: string; out FileDateTime: TDateTime; FollowLink: Boolean := True): Boolean;
function FileIsReadOnly(const FileName: string): Boolean;
function FileSetReadOnly(const FileName: string; ReadOnly: Boolean): Boolean;
function FileSystemAttributes(const Path: string): TFileSystemAttributes;
function SetCurrentDir(const Dir: string): Boolean;
function FindFirst(const Path: string; Attr: Integer;   var F: TSearchRec): Integer;
function FindNext(var F: TSearchRec): Integer;
procedure FindClose(var F: TSearchRec);
function FileGetDate(Handle: THandle): LongInt;
function FileGetDateTimeInfo(const FileName: string; out DateTime: TDateTimeInfoRec; FollowLink: Boolean = True): Boolean;
function FileSetDate(const FileName: string; Age: LongInt): Integer;
function FileDateToDateTime(FileDate: LongInt): TDateTime;
function DateTimeToFileDate(DateTime: TDateTime): LongInt;
{$ENDIF}
*/

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

//
// File functions
//

function DeleteFile(const FileName: DelphiString): Boolean;
begin
  result := true;
  try
    File.Delete(FileName);
  except
    result := false;
  end;
end;

function RenameFile(const OldName, NewName: DelphiString): Boolean;
begin
  result := true;
  try
    File.Move(OldName, NewName);
  except
    result := false;
  end;
end;

function ChangeFileExt(const FileName, aExtension: DelphiString): DelphiString;
begin
  result := Path.ChangeExtension(FileName, aExtension);
end;

function ChangeFilePath(const FileName, Path: DelphiString): DelphiString;
begin
  result := IncludeTrailingPathDelimiter(Path) + ExtractFileName(FileName);
end;

function ExtractFilePath(const FileName: DelphiString): DelphiString;
begin
  var lPos := FileName.LastDelimiter(TOSVersion.PathDelim);
  if lPos > 0 then
    result := FileName.SubString(0, lPos + 1)
  else
    result := '';
end;

function ExtractFileDir(const FileName: DelphiString): DelphiString;
begin
  var lPos := FileName.LastDelimiter(TOSVersion.PathDelim + TOSVersion.DriveDelim);
  if (lPos > 0) and (FileName.Chars[lPos] = TOSVersion.PathDelim) then
    dec(lPos);
  result := FileName.SubString(0, lPos + 1);
end;

function ExtractFileDrive(const FileName: DelphiString): DelphiString;
begin
  if (FileName.Length >= 2) and (FileName.Chars[1] = TOSVersion.DriveDelim) then
    result := FileName.SubString(0, 2)
  else
    result := '';
end;

function IsDelimiter(const Delimiters, S: DelphiString; aIndex: Integer): Boolean;
begin
  result := S.IsDelimiter(Delimiters, aIndex);
end;

function LastDelimiter(const Delimiters, S: DelphiString): Integer;
begin
  result := S.LastDelimiter(Delimiters);
end;

function IncludeTrailingPathDelimiter(const S: DelphiString): DelphiString;
begin
  if (S.Length > 0) and (S.Chars[S.Length - 1] <> TOSVersion.PathDelim) then
    result := S + TOSVersion.PathDelim
  else
    result := S;
end;

function ExcludeTrailingPathDelimiter(const S: DelphiString): DelphiString;
begin
  if (S.Length > 0) and (S.Chars[S.Length - 1] = TOSVersion.PathDelim) then
    result := S.SubString(0, S.Length - 1)
  else
    result := S;
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

function IsRelativePath(const Path: DelphiString): Boolean;
begin
  if TOSVersion.Platform = TPlatform.pfWindows then
    result := (Path.Length > 0) and (Path.Chars[0] <> TOSVersion.PathDelim) and (Path.Chars[1] <> ':')
  else
    result := (Path.Length > 0) and (Path.Chars[0] <> TOSVersion.PathDelim);
end;

function GetCurrentDir: DelphiString;
begin
  result := Environment.CurrentDirectory;
end;

function CreateDir(const Dir: DelphiString): Boolean;
begin
  result := true;
  try
    Folder.Create(Dir); 
  except
    result := false;
  end;
end;

function RemoveDir(const Dir: DelphiString): Boolean;
begin
  result := true;
  try
    Folder.Delete(Dir);
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
    result := File.Exists(FileName);
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

function ForceDirectories(Dir: DelphiString): Boolean;
begin
  result := true;
  if Dir = '' then
    raise new Exception('Can not create directory');

  var lDir := ExcludeTrailingPathDelimiter(Dir);
  var lList := new TStringList();
  if DirectoryExists(lDir) then
    exit;

  {$IF ISLAND AND WINDOWS}
  repeat
    lList.Add(lDir);
    lDir := ExtractFileDir(lDir);
  until (lDir.Length < 3) or (ExtractFileDir(lDir) = lDir);
  {$ELSE}
  repeat
    lList.Add(lDir);
    lDir := ExpandFileName(lDir);
  until lDir = '';
  {$ENDIF}

  for i: Integer := lList.Count - 1 downto 0 do begin
    result := CreateDir(lList[i]);
    if not result then 
      exit;
  end;
end;

class constructor TFormatSettings;
begin
  SysLocale.DefaultLCID := Locale.Current;

end;

{$IF ISLAND AND WINDOWS}
procedure CallDiskFreeSpace(Drive: Byte; var aFreeSpace: Int64; var aDiskSize: Int64);
begin
  var lDrive: RemObjects.Elements.System.String := Char(Drive + $40) + ':' + PathDelim;
  var lChars := lDrive.ToCharArray();
  var lFreeSpace, lTotalSpace: rtl.ULARGE_INTEGER;
  rtl.GetDiskFreeSpaceEx(@lChars[0], @lFreeSpace, @lTotalSpace, nil);
  aFreeSpace := lFreeSpace.QuadPart;
  aDiskSize := lTotalSpace.QuadPart;
end;

function DiskSize(Drive: Byte): Int64;
begin
  var lFreeSpace, lDiskSize: Int64;
  CallDiskFreeSpace(Drive, var lFreeSpace, var lDiskSize);
  result := lDiskSize;
end;

function FileSetAttr(const FileName: DelphiString; Attr: Integer; FollowLink: Boolean := True): Integer;
begin
  var lArray := PlatformString(FileName).ToCharArray(true);
  result := Integer(rtl.SetFileAttributes(@lArray[0], Attr));
end;

function FileGetAttr(const FileName: DelphiString; FollowLink: Boolean := True): Integer;
begin
  var lArray := PlatformString(FileName).ToCharArray(true);
  result := rtl.GetFileAttributes(@lArray[0]); 
end;

function DiskFree(Drive: Byte): Int64;
begin
  var lFreeSpace, lDiskSize: Int64;
  CallDiskFreeSpace(Drive, var lFreeSpace, var lDiskSize);
  result := lFreeSpace;
end;
{$ENDIF}

{$IF ISLAND AND WINDOWS}
class method TOSVersion.GetOSInfo;
begin
  fName := 'Windows';
  var lVersion := Environment.OSVersion;
  if lVersion <> '' then begin
    var lNumbers := lVersion.split('.');
    if lNumbers.Count > 0 then TryStrToInt(lNumbers[0], out fMajor);
    if lNumbers.Count > 1 then TryStrToInt(lNumbers[1], out fMinor);
  end;
end;
{$ENDIF}

class constructor TOSVersion;
begin
  {$IF ISLAND OR TOFFEE}
    {$IF ARM}
      {$IF CPU64}
        fArchitecture := TArchitecture.arARM64; 
      {$ELSE}
        fArchitecture := TArchitecture.arARM32; 
      {$ENDIF}
    {$ELSE}
      {$IF CPU64}
        fArchitecture := TArchitecture.arIntel64; 
      {$ELSE}
        fArchitecture := TArchitecture.arIntelX86; 
      {$ENDIF}  
    {$ENDIF}
  {$ENDIF}

  {$IF ISLAND}
  fName := Environment.OSName;
  {$IF WINDOWS}
  fPlatform := TPlatform.pfWindows;
  {$ELSEIF LINUX}
  fPlatform := TPlatform.pfLinux;
  {$ELSEIF ANDROID}
  fPlatform := TPlatform.pfAndroid;
  {$ENDIF}
  {$ELSEIF COOPER}
  fName := System.getProperty('os.name');
  var lTemp := System.getProperty('os.version');
  if lTemp <> '' then begin
    var lNumbers := lTemp.split('.');
    if lNumbers.length > 0 then TryStrToInt(lNumbers[0], out fMajor);
    if lNumbers.length > 1 then TryStrToInt(lNumbers[1], out fMinor);
  end;
  var lOSName := fName.ToLowerInvariant;
  if lOSName.Contains("windows") then fPlatform := TPlatform.pfWindows
  else if lOSName.Contains("linux") then fPlatform := TPlatform.pfLinux
  else if lOSName.Contains("mac") then fPlatform := TPlatform.pfMacOS
  else fPlatform := TPlatform.pfJava;
  {$ELSEIF ECHOES}
  var lOS := System.Environment.OSVersion;
  fName := lOS.VersionString;
  fMajor := lOS.Version.Major;
  fMinor := lOS.Version.Minor;
  fServicePackMajor := lOS.Version.MajorRevision;
  fServicePackMinor := lOS.Version.MinorRevision;
  fBuild := lOS.Version.Build;
  if fName.ToLower.Contains("windows") then fPlatform := TPlatform.pfWindows
  else fPlatform := TPlatform.pfNET;
  {$ELSEIF TOFFEE}
  fMajor := NSProcessInfo.processInfo.operatingSystemVersion.majorVersion;
  fMinor := NSProcessInfo.processInfo.operatingSystemVersion.minorVersion;
  fName := NSProcessInfo.processInfo.operatingSystemName;
  {$IF IOS}
  fPlatform := TPlatform.pfIOS;
  {$ELSEIF MACOS}
  fPlatform := TPlatform.pfMACOS;
  {$ELSEIF TVOS}
  fPlatform := TPlatform.pfTVOS;
  {$ELSEIF WATCHOS}
  fPlatform := TPlatform.pfWatchOS;
  {$ENDIF}
  {$ENDIF}
  if fPlatform = TPlatform.pfWindows then begin
    fPathDelim := '\';
    fDriveDelim := ':';
    fPathSep := ';';  
  end
  else begin
    fPathDelim := '/';
    fDriveDelim := ''; 
    fPathSep := ':';
  end;
end;

class method TOSVersion.Check(aMajor: Integer): Boolean;
begin
  result := fMajor >= aMajor;
end;

class method TOSVersion.Check(aMajor, aMinor: Integer): Boolean;
begin
  result := (fMajor > aMajor) or ((fMajor = aMajor) and (fMinor >= aMinor));
end;

class method TOSVersion.Check(aMajor, aMinor, aServicePackMajor: Integer): Boolean;
begin
  Result := Check(aMajor, aMinor) or ((fMajor = aMajor) and (fMinor = aMinor) and (fServicePackMajor >= aServicePackMajor));
end;

class method TOSVersion.ToString: DelphiString;
begin
  result := String.Format('%s (Version %d.%d.%d)', [fName, fMajor, fMinor, fServicePackMajor]);
end;


end.