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
  INVALID_HANDLE_VALUE = THandle(-1);

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
  {$IF COOPER OR TOFFEE}
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
    CurrencyString: DelphiString;
    CurrencyFormat: Byte;
    CurrencyDecimals: Byte;
    DateSeparator: DelphiString;
    TimeSeparator: Char;
    ListSeparator: Char;
    ShortDateFormat: DelphiString;
    LongDateFormat: DelphiString;
    TimeAMString: DelphiString;
    TimePMString: DelphiString;
    ShortTimeFormat: DelphiString;
    LongTimeFormat: DelphiString;
/*    ShortMonthNames: array[1..12] of DelphiString;
    LongMonthNames: array[1..12] of DelphiString;
    ShortDayNames: array[1..7] of DelphiString;
    LongDayNames: array[1..7] of DelphiString;*/
    //EraInfo: array of TEraInfo;
    ThousandSeparator: Char;
    DecimalSeparator: Char;
    TwoDigitYearCenturyWindow: Word;
    NegCurrFormat: Byte;
    NormalizedLocaleName: String;
    Locale: TLocaleID;
    class function Create: TFormatSettings; static;
    class function Create(aLocale: TLocaleID): TFormatSettings; static;
    class function Create(aLocaleName: DelphiString): TFormatSettings; static;
    class function Invariant: TFormatSettings; static;
    class constructor;
  end;

  TArchitecture = public (arIntelX86, arIntelX64, arARM32, arARM64);
  TPlatform = public (pfWindows, pfMacOS, pfiOS, pfAndroid, pfWinRT, pfLinux, pfNET, pfJava, pfTVOS, pfWatchOS);

  TOSVersion = public static class
  private
    class var fLineBreak: String;
    class var fArchitecture: TArchitecture := TArchitecture.arIntelX86;
    class var fBuild: Integer := 0;
    class var fMajor: Integer := 0;
    class var fMinor: Integer;
    class var fName: String;
    class var fPlatform: TPlatform;
    class var fServicePackMajor: Integer := 0;
    class var fServicePackMinor: Integer := 0;
    class var fPathDelim: String;
    class var fDriveDelim: String;
    class var fPathSep: String;
    {$IF ISLAND AND WINDOWS}
    class method GetOSInfo; static;
    {$ENDIF}
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
    class property LineBreak: String read fLineBreak;
  end;

// Generic Anonymous method declarations
type
  TProc = block;
  {$IF NOT COOPER AND NOT TOFFEE}
  TProc<T> = block(Arg1: T);
  TProc<T1,T2> = block(Arg1: T1; Arg2: T2);
  TProc<T1,T2,T3> = block(Arg1: T1; Arg2: T2; Arg3: T3);
  TProc<T1,T2,T3,T4> = block(Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4);
  {$ENDIF}

  TFunc<TResult> = block(): TResult;
  {$IF NOT COOPER AND NOT TOFFEE}
  TFunc<T,TResult> = block(Arg1: T): TResult;
  TFunc<T1,T2,TResult> = block(Arg1: T1; Arg2: T2): TResult;
  TFunc<T1,T2,T3,TResult> = block(Arg1: T1; Arg2: T2; Arg3: T3): TResult;
  TFunc<T1,T2,T3,T4,TResult> = block(Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): TResult;
  {$ENDIF}

  TPredicate<T> = block(Arg1: T): Boolean;

  ENotImplemented = public class(Exception);
  EArgumentException = public class(Exception);
  EArgumentOutOfRangeException = public class(EArgumentException);
  EArgumentNilException = public class(EArgumentException);
  EOutOfMemory = public class({$IFDEF ECHOES}System.OutOfMemoryException{$ELSE}Exception{$ENDIF ECHOES});
  EConvertError = public class(Exception);
  EAbstractError = public class(Exception);

var
  FormatSettings: TFormatSettings := TFormatSettings.Create; public;
  SysLocale: TSysLocale; public;

{ File functions }
{$IF NOT WEBASSEMBLY}
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
{$ENDIF}

{$IF (ISLAND AND NOT WEBASSEMBLY) AND WINDOWS}
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

{$IF ISLAND AND WINDOWS}
type
[CallingConvention(CallingConvention.Stdcall)]
TRTLGetVersionFunc = public function(lpVersionInformation: ^rtl.OSVERSIONINFO): rtl.DWORD;
{$ENDIF}

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

{$IFNDEF WEBASSEMBLY}
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
  var lHandle := FileHandle(TInternalFileHandles.Get(Handle));
  lHandle.Close;
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
{$ENDIF}

class function TFormatSettings.Create: TFormatSettings;
begin
  result := Create(Locale.Current);
end;

class function TFormatSettings.Create(aLocale: TLocaleID): TFormatSettings;
begin
  result.Locale  := aLocale;
  {$IF COOPER}
  var lFormat := java.text.SimpleDateFormat(java.text.DateFormat.getDateInstance(java.text.DateFormat.LONG, java.util.Locale(aLocale)));
  result.LongDateFormat := lFormat.toPattern;
  lFormat := java.text.SimpleDateFormat(java.text.DateFormat.getDateInstance(java.text.DateFormat.SHORT, java.util.Locale(aLocale)));
  result.ShortDateFormat := lFormat.toPattern;
  result.LongTimeFormat := 'hh:mm:ss';
  result.ShortTimeFormat := 'hh:mm';
  var lDateSymbols := lFormat.getDateFormatSymbols;
  if lDateSymbols.AmPmStrings.length > 1 then begin
    result.TimeAMString := lDateSymbols.AmPmStrings[0];
    result.TimePMString := lDateSymbols.AmPmStrings[1];
  end;
  result.DateSeparator := '/';

  var lCurrency := java.util.Currency.getInstance(java.util.Locale(aLocale));
  result.CurrencyString := lCurrency.getSymbol;
  result.CurrencyDecimals := lCurrency.DefaultFractionDigits;
  var lSymbols := new java.text.DecimalFormat().getDecimalFormatSymbols;
  result.DecimalSeparator := lSymbols.getDecimalSeparator;
  result.ThousandSeparator := lSymbols.getGroupingSeparator;
  {$ELSEIF ECHOES}
  var lLocale := System.Globalization.CultureInfo(aLocale);
  result.LongDateFormat := lLocale.DateTimeFormat.LongDatePattern;
  result.ShortDateFormat := lLocale.DateTimeFormat.ShortDatePattern;
  result.LongTimeFormat := lLocale.DateTimeFormat.LongTimePattern;
  result.ShortTimeFormat := lLocale.DateTimeFormat.ShortTimePattern;
  result.TimePMString := lLocale.DateTimeFormat.PMDesignator;
  result.TimeAMString := lLocale.DateTimeFormat.AMDesignator;
  result.DateSeparator := lLocale.DateTimeFormat.DateSeparator;

  result.CurrencyString := lLocale.NumberFormat.CurrencySymbol;
  if length(lLocale.NumberFormat.CurrencyGroupSizes) > 0 then
    result.CurrencyDecimals := lLocale.NumberFormat.CurrencyGroupSizes[0];
  if lLocale.NumberFormat.NumberDecimalSeparator.Length > 0 then
    result.DecimalSeparator := lLocale.NumberFormat.NumberDecimalSeparator[0];
  if lLocale.NumberFormat.NumberGroupSeparator.Length > 0 then
    result.ThousandSeparator := lLocale.NumberFormat.NumberGroupSeparator[0];
  {$ELSEIF ISLAND AND WINDOWS}
  //var lLocale: rtl.LCID; // TODO
  var lBuffer := new Char[100];
  var lTotal: Integer;
  var lTemp: DelphiString;

  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_SLONGDATE, @lBuffer[0], lBuffer.Length);
  result.LongDateFormat := DelphiString.Create(lBuffer, 0, lTotal - 1);
  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_SSHORTDATE, @lBuffer[0], lBuffer.Length);
  result.ShortDateFormat := DelphiString.Create(lBuffer, 0, lTotal - 1);
  result.LongTimeFormat := 'hh:mm:ss';
  result.ShortTimeFormat := 'hh:mm';

  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_S1159, @lBuffer[0], lBuffer.Length);
  result.TimeAMString := DelphiString.Create(lBuffer, 0, lTotal - 1);
  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_S2359, @lBuffer[0], lBuffer.Length);
  result.TimePMString := DelphiString.Create(lBuffer, 0, lTotal - 1);
  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_STIME, @lBuffer[0], lBuffer.Length);
  result.DateSeparator := DelphiString.Create(lBuffer, 0, lTotal - 1);
  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_SDATE, @lBuffer[0], lBuffer.Length);
  lTemp := DelphiString.Create(lBuffer, 0, lTotal - 1);
  if lTemp.Length > 0 then
    result.TimeSeparator := lTemp.Chars[0];

  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_SCURRENCY, @lBuffer[0], lBuffer.Length);
  result.CurrencyString := DelphiString.Create(lBuffer, 0, lTotal - 1);
  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_SDECIMAL, @lBuffer[0], lBuffer.Length);
  lTemp := DelphiString.Create(lBuffer, 0, lTotal - 1);
  if lTemp.Length > 0 then
     result.DecimalSeparator := lTemp.Chars[0];
  lTotal := rtl.GetLocaleInfo(rtl.LOCALE_NAME_USER_DEFAULT, rtl.LOCALE_STHOUSAND, @lBuffer[0], lBuffer.Length);
  lTemp := DelphiString.Create(lBuffer, 0, lTotal).SubString(0, 1);
  if lTemp.Length > 0 then
  result.ThousandSeparator := lTemp.Chars[0];
  {$ELSEIF TOFFEE}
  var lLocale := NSLocale(aLocale);
  var lDateFormatter := new NSDateFormatter();
  lDateFormatter.locale := lLocale;
  lDateFormatter.dateStyle := NSDateFormatterStyle.NSDateFormatterFullStyle;
  lDateFormatter.timeStyle := NSDateFormatterStyle.NSDateFormatterNoStyle;
  result.LongDateFormat := lDateFormatter.dateFormat;

  lDateFormatter.dateStyle := NSDateFormatterStyle.NSDateFormatterShortStyle;
  lDateFormatter.timeStyle := NSDateFormatterStyle.NSDateFormatterNoStyle;
  result.ShortDateFormat := lDateFormatter.dateFormat;

  lDateFormatter.dateStyle := NSDateFormatterStyle.NSDateFormatterNoStyle;
  lDateFormatter.timeStyle := NSDateFormatterStyle.NSDateFormatterMediumStyle;
  result.LongTimeFormat := lDateFormatter.dateFormat;

  lDateFormatter.dateStyle := NSDateFormatterStyle.NSDateFormatterNoStyle;
  lDateFormatter.timeStyle := NSDateFormatterStyle.NSDateFormatterShortStyle;
  result.ShortTimeFormat := lDateFormatter.dateFormat;

  var lNumberFormatter := new NSNumberFormatter();
  lNumberFormatter.locale := lLocale;
  result.CurrencyString := lNumberFormatter.currencySymbol;
  if lNumberFormatter.decimalSeparator.length > 0 then
    result.DecimalSeparator := lNumberFormatter.decimalSeparator[0];
  if lNumberFormatter.groupingSeparator.length > 0 then
  result.ThousandSeparator := lNumberFormatter.groupingSeparator[0];
  {$ENDIF}
end;

class function TFormatSettings.Create(aLocaleName: DelphiString): TFormatSettings;
begin
  var aLocale := TLanguages.GetLocaleIDFromLocaleName(aLocaleName);
  result := Create(aLocale);
end;

class function TFormatSettings.Invariant: TFormatSettings;
begin
  result.CurrencyString := #$00A4;
  result.CurrencyFormat := 0;
  result.CurrencyDecimals := 2;
  result.DateSeparator := '/';
  result.TimeSeparator := ':';
  result.ListSeparator := ',';
  result.ShortDateFormat := 'MM/dd/yyyy';
  result.LongDateFormat := 'dddd, dd MMMMM yyyy HH:mm:ss';
  result.TimeAMString := 'AM';
  result.TimePMString := 'PM';
  result.ShortTimeFormat := 'HH:mm';
  result.LongTimeFormat := 'HH:mm:ss';
end;

class constructor TFormatSettings;
begin
  {$IF NOT COOPER}
  SysLocale.DefaultLCID := Locale.Current;
  {$ENDIF}
end;

{$IF (ISLAND AND NOT WEBASSEMBLY) AND WINDOWS}
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
        fArchitecture := TArchitecture.arIntelX64;
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
  fPlatform := TPlatform.pfiOS;
  {$ELSEIF MACOS}
  fPlatform := TPlatform.pfMacOS;
  {$ELSEIF TVOS}
  fPlatform := TPlatform.pfTVOS;
  {$ELSEIF WATCHOS}
  fPlatform := TPlatform.pfWatchOS;
  {$ENDIF}
  {$ENDIF}
  {$IF ISLAND AND WINDOWS}
  var lBytes := RemObjects.Elements.System.String('ntdll.dll').ToCharArray(true);
  var lLibrary := rtl.LoadLibrary(rtl.LPCWSTR(@lBytes[0]));
  var lGetVersion: TRTLGetVersionFunc;
  var lFunc := RemObjects.Elements.System.String('RtlGetVersion').ToAnsiChars(true);
  var lVer: rtl.OSVERSIONINFO;
  lVer.dwOSVersionInfoSize := sizeOf(lVer);
  if lLibrary ≠ nil then begin
    lGetVersion := TRTLGetVersionFunc(rtl.GetProcAddress(lLibrary, rtl.LPCSTR(@lFunc[0])));
    if lGetVersion ≠ nil then begin
      if lGetVersion(@lVer) = 0 then begin
        fMajor := lVer.dwMajorVersion;
        fMinor := lVer.dwMinorVersion;
        fBuild := lVer.dwBuildNumber;
      end;
    end;
    rtl.FreeLibrary(lLibrary);
  end;

  if fMajor = 0 then begin
    // if RtlGetVersion is not available (for some reason..), use standard way
    // GetVersionEx returns Windows 8.1 even for Win 10, so primary option is better.
    rtl.GetVersionEx(@lVer);
    fMajor := lVer.dwMajorVersion;
    fMinor := lVer.dwMinorVersion;
    fBuild := lVer.dwBuildNumber;
  end;
  {$ENDIF}
  if fPlatform = TPlatform.pfWindows then begin
    fPathDelim := '\';
    fDriveDelim := ':';
    fPathSep := ';';
    fLineBreak := #13#10;
  end
  else begin
    fPathDelim := '/';
    fDriveDelim := '';
    fPathSep := ':';
    fLineBreak := #10;
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
  result := RTL2String.Format('%s (Version %d.%d.%d)', [fName, fMajor, fMinor, fServicePackMajor]);
end;


end.