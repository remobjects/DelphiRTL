namespace RemObjects.Elements.RTL.Delphi;

interface

uses 
  Sugar;

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
  TEncoding = public Sugar.Encoding;

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

function UpperCase(const S: DelphiString): DelphiString;
function UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function LowerCase(const S: DelphiString): DelphiString; 
function LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function CompareStr(const S1, S2: DelphiString): Integer; inline;
function CompareStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer;
function SameStr(const S1, S2: DelphiString): Boolean; inline;
function SameStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
function CompareText(const S1, S2: DelphiString): Integer; 
function CompareText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer; 
function SameText(const S1, S2: DelphiString): Boolean; inline;
function SameText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
function AnsiUpperCase(const S: DelphiString): DelphiString; inline;
function AnsiLowerCase(const S: DelphiString): DelphiString; inline;
function AnsiCompareStr(const S1, S2: DelphiString): Integer; inline;
function AnsiSameStr(const S1, S2: DelphiString): Boolean; inline;
function AnsiCompareText(const S1, S2: DelphiString): Integer; inline;
function AnsiSameText(const S1, S2: DelphiString): Boolean; inline;
//function AnsiLastChar(const S: UnicodeString): PWideChar; 
function Trim(const S: DelphiString): DelphiString;
function TrimLeft(const S: DelphiString): DelphiString;
function TrimRight(const S: DelphiString): DelphiString;
function QuotedStr(const S: DelphiString): DelphiString;
function AnsiQuotedStr(const S: DelphiString; Quote: Char): DelphiString;
function AnsiDequotedStr(const S: DelphiString; aQuote: Char): DelphiString;
//function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle = tlbsCRLF): string; 
//function IsValidIdent(const Ident: string; AllowDots: Boolean = False): Boolean;
function IntToStr(Value: Integer): DelphiString;
function IntToStr(Value: Int64): DelphiString;
function UIntToStr(Value: Cardinal): DelphiString;
function UIntToStr(Value: UInt64): DelphiString;
function IntToHex(Value: Integer; Digits: Integer := sizeOf(Integer) * 2): DelphiString;
function IntToHex(Value: Int64; Digits: Integer := sizeOf(Int64) * 2): DelphiString;
function IntToHex(Value: UInt64; Digits: Integer := sizeOf(UInt64) * 2): DelphiString;
function StrToInt(const S: DelphiString): Integer;
//function StrToIntDef(const S: string; Default: Integer): Integer; 
//function TryStrToInt(const S: string; out Value: Integer): Boolean; 
function StrToInt64(const S: DelphiString): Int64;
//function StrToInt64Def(const S: string; const Default: Int64): Int64; 
//function TryStrToInt64(const S: string; out Value: Int64): Boolean; 
function StrToUInt64(const S: DelphiString): UInt64;
//function StrToUInt64Def(const S: string; const Default: UInt64): UInt64; 
//function TryStrToUInt64(const S: string; out Value: UInt64): Boolean; 
function StrToBool(const S: DelphiString): Boolean;
//function StrToBoolDef(const S: string; const Default: Boolean): Boolean; 
//function TryStrToBool(const S: string; out Value: Boolean): Boolean; 
function BoolToStr(B: Boolean; UseBoolStrs: Boolean := False): DelphiString;

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

// Datetime functions
type
  TDayTable = array[0..11] of Word;

const
  MonthDays: array [Boolean] of TDayTable =
    [[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
     [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]];

function IsLeapYear(Year: Word): Boolean;
function StrToDateTime(const S: DelphiString): TDateTime; inline;
function StrToDateTime(const S: DelphiString; aFormatSettings: TFormatSettings): TDateTime; 
function StrToDateTimeDef(const S: DelphiString; aDefault: TDateTime): TDateTime; inline;
function StrToDateTimeDef(const S: DelphiString; const aDefault: TDateTime; aFormatSettings: TFormatSettings): TDateTime;
function TryStrToDateTime(const S: DelphiString; out aValue: TDateTime): Boolean; inline;
function TryStrToDateTime(const S: DelphiString; out aValue: TDateTime; aFormatSettings: TFormatSettings): Boolean;
function TryEncodeDate(aYear, aMonth, aDay: Word; out aDate: TDateTime): Boolean;
function TryEncodeTime(aHour, aMin, aSec, aMSec: Word; out aTime: TDateTime): Boolean;
function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out Value: TDateTime): Boolean;

function DateTimeToTimeStamp(aDateTime: TDateTime): TTimeStamp;
function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
function MSecsToTimeStamp(MSecs: Int64): TTimeStamp;
function TimeStampToMSecs(const TimeStamp: TTimeStamp): Int64;

function EncodeDate(Year, Month, Day: Word): TDateTime;
function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;

procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);
function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean;
procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);

/*
//{$IFDEF MSWINDOWS}
{ DateTimeToSystemTime converts a date and time from Delphi's TDateTime
  format into the Win32 API's TSystemTime format. }

procedure DateTimeToSystemTime(const DateTime: TDateTime; var SystemTime: TSystemTime);

{ SystemTimeToDateTime converts a date and time from the Win32 API's
  TSystemTime format into Delphi's TDateTime format. }

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;

{ TrySystemTimeToDateTime converts a date and time from the Win32 API's
  TSystemTime format into Delphi's TDateTime format without raising an
  EConvertError exception. }

function TrySystemTimeToDateTime(const SystemTime: TSystemTime; out DateTime: TDateTime): Boolean;
{$ENDIF}
*/

function DayOfWeek(const DateTime: TDateTime): Word;
function Date: TDateTime;
function Time: TDateTime;
function Now: TDateTime;
function CurrentYear: Word;
function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer := 1): TDateTime;
procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer := 1);
procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);
procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
// Peding DateTime funcs
/*function DateToStr(const DateTime: TDateTime): string;  inline;
function DateToStr(const DateTime: TDateTime; const AFormatSettings: TFormatSettings): string;  inline;
function TimeToStr(const DateTime: TDateTime): string;  inline;
function TimeToStr(const DateTime: TDateTime; const AFormatSettings: TFormatSettings): string;  inline;
function DateTimeToStr(const DateTime: TDateTime): string;  inline;
function DateTimeToStr(const DateTime: TDateTime; const AFormatSettings: TFormatSettings): string;  inline;
function StrToDate(const S: string): TDateTime;  inline;
function StrToDate(const S: string; const AFormatSettings: TFormatSettings): TDateTime; 
function StrToDateDef(const S: string; const Default: TDateTime): TDateTime;  inline;
function StrToDateDef(const S: string; const Default: TDateTime; const AFormatSettings: TFormatSettings): TDateTime; 
function TryStrToDate(const S: string; out Value: TDateTime): Boolean;  inline;
function TryStrToDate(const S: string; out Value: TDateTime; const AFormatSettings: TFormatSettings): Boolean; 
function StrToTime(const S: string): TDateTime;  inline;
function StrToTime(const S: string; const AFormatSettings: TFormatSettings): TDateTime; 
function StrToTimeDef(const S: string; const Default: TDateTime): TDateTime;  inline;
function StrToTimeDef(const S: string; const Default: TDateTime; const AFormatSettings: TFormatSettings): TDateTime; 
function TryStrToTime(const S: string; out Value: TDateTime): Boolean;  inline;
function TryStrToTime(const S: string; out Value: TDateTime; const AFormatSettings: TFormatSettings): Boolean; 
function FormatDateTime(const Format: string; DateTime: TDateTime): string;  inline;
function FormatDateTime(const Format: string; DateTime: TDateTime; const AFormatSettings: TFormatSettings): string; 
procedure DateTimeToString(var Result: string; const Format: string; DateTime: TDateTime);  inline;
procedure DateTimeToString(var Result: string; const Format: string; DateTime: TDateTime; const AFormatSettings: TFormatSettings); 
const
  MinDateTime: TDateTime = -657434.0;      
  MaxDateTime: TDateTime =  2958465.99999; 
function FloatToDateTime(const Value: Extended): TDateTime;
function TryFloatToDateTime(const Value: Extended; out AResult: TDateTime): Boolean;
*/


implementation

type
TInternalFileHandles = static class
private
  class var fLock: Object := new Object;
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

function UpperCase(const S: DelphiString): DelphiString;
begin
  var lTmp := new Char[S.Length];
  for i: Integer := 0 to S.Length - 1 do
  if S.Chars[i] in ['a'..'z'] then
    lTmp[i] := Char(Integer(S[i]) xor $0020)
  else
    lTmp[i] := S[i];
  result := DelphiString.Create(lTmp);
end;

function UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; 
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := UpperCase(S)
  else
    result := S.ToUpper(TLanguages.UserDefaultLocale);
end;

function LowerCase(const S: DelphiString): DelphiString;
begin
  var lTmp := new Char[S.Length];
  for i: Integer := 0 to S.Length - 1 do
  if S.Chars[i] in ['A'..'Z'] then
    lTmp[i] := Char(Integer(S[i]) or $0020)
  else
    lTmp[i] := S[i];
  result := DelphiString.Create(lTmp);
end;

function LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := LowerCase(S)
  else
    result := S.ToLower(TLanguages.UserDefaultLocale);
end;

function CompareStr(const S1, S2: DelphiString): Integer;
begin
  result := DelphiString.CompareOrdinal(S1, S2);
end;

function CompareStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := CompareStr(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale);
end;

function SameStr(const S1, S2: DelphiString): Boolean;
begin
  result := DelphiString.Equals(S1, S2);
end;

function SameStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := SameStr(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale) = 0;
end;

function CompareText(const S1, S2: DelphiString): Integer; 
begin
  result := DelphiString.CompareOrdinal(UpperCase(S1), UpperCase(S2));
end;

function CompareText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer; 
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := CompareText(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale);
end;

function SameText(const S1, S2: DelphiString): Boolean;
begin
  result := CompareText(S1, S2) = 0;
end;

function SameText(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
begin
  if LocaleOptions = TLocaleOptions.loInvariantLocale then
    result := SameText(S1, S2)
  else
    result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale) = 0;
end;

function AnsiUpperCase(const S: DelphiString): DelphiString;
begin
  result := S.ToUpper;
end;

function AnsiLowerCase(const S: DelphiString): DelphiString;
begin
  result := S.ToLower;
end;

function AnsiCompareStr(const S1, S2: DelphiString): Integer;
begin
  result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale);  
end;

function AnsiSameStr(const S1, S2: DelphiString): Boolean;
begin
  result := DelphiString.Compare(S1, S2, TLanguages.UserDefaultLocale) = 0;  
end;

function AnsiCompareText(const S1, S2: DelphiString): Integer;
begin
  result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale);
end;

function AnsiSameText(const S1, S2: DelphiString): Boolean;
begin
  result := DelphiString.Compare(S1, S2, true, TLanguages.UserDefaultLocale) = 0;
end;

function Trim(const S: DelphiString): DelphiString;
begin
  result := S.Trim;
end;

function TrimLeft(const S: DelphiString): DelphiString;
begin
  result := S.TrimLeft;
end;

function TrimRight(const S: DelphiString): DelphiString;
begin
  result := S.TrimRight;
end;

function QuotedStr(const S: DelphiString): DelphiString;
begin
  result := S.QuotedString;
end;

function AnsiQuotedStr(const S: DelphiString; Quote: Char): DelphiString;
begin
  result := S.QuotedString(Quote);
end;

function AnsiDequotedStr(const S: DelphiString; aQuote: Char): DelphiString;
begin
  result := S.DeQuotedString(aQuote);
end;

function IntToStr(Value: Integer): DelphiString;
begin
  result := Sugar.Convert.ToString(Value);
end;

function IntToStr(Value: Int64): DelphiString;
begin
  result := Sugar.Convert.ToString(Value);
end;

function UIntToStr(Value: Cardinal): DelphiString;
begin
  result := Sugar.Convert.ToString(Value);
end;

function UIntToStr(Value: UInt64): DelphiString;
begin
  result := Sugar.Convert.ToString(Int64(Value));
end;

function IntToHex(Value: Integer; Digits: Integer): DelphiString;
begin
  result := Sugar.Convert.ToHexString(Value, Digits);
end;

function IntToHex(Value: Int64; Digits: Integer): DelphiString;
begin
  result := Sugar.Convert.ToHexString(Value, Digits);
end;

function IntToHex(Value: UInt64; Digits: Integer := sizeOf(UInt64) * 2): DelphiString;
begin
  result := Sugar.Convert.ToHexString(Value, Digits);
end;

function StrToInt(const S: DelphiString): Integer;
begin
  result := S.ToInteger;
end;

function StrToInt64(const S: DelphiString): Int64;
begin
  result := S.ToInt64;
end;

function StrToUInt64(const S: DelphiString): UInt64;
begin
  result := S.ToInt64;
end;

function StrToBool(const S: DelphiString): Boolean;
begin
  result := S.ToBoolean;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean := False): DelphiString;
begin
  result := Sugar.Convert.ToString(B);  
end;

///////////////////////////////////////////////////////////////////// File functions

function DeleteFile(const FileName: DelphiString): Boolean;
begin
  result := true;
  try
    Sugar.IO.FileUtils.Delete(FileName);
  except
    result := false;
  end;
end;

function RenameFile(const OldName, NewName: DelphiString): Boolean;
begin
  result := true;
  try
    Sugar.IO.FileUtils.Move(OldName, NewName);
  except
    result := false;
  end;
end;

function ChangeFileExt(const FileName, aExtension: DelphiString): DelphiString;
begin
  result := Sugar.IO.Path.ChangeExtension(FileName, aExtension);
end;

function ExtractFileName(const FileName: DelphiString): DelphiString;
begin
  result := Sugar.IO.Path.GetFileName(FileName);
end;

function ExtractFileExt(const FileName: DelphiString): DelphiString;
begin
  result := Sugar.IO.Path.GetExtension(FileName);
end;

function GetHomePath: DelphiString;
begin
  result := Sugar.IO.Folder.UserHomeFolder.FullPath;
end;

function ExpandFileName(const FileName: DelphiString): DelphiString;
begin
  result := Sugar.IO.Path.GetFullPath(FileName);
end;

function GetCurrentDir: DelphiString;
begin
  result := Sugar.Environment.CurrentDirectory;
end;

function CreateDir(const Dir: DelphiString): Boolean;
begin
  result := true;
  try
    Sugar.IO.Folder.Create(Dir);
  except
    result := false;
  end;
end;

function RemoveDir(const Dir: DelphiString): Boolean;
begin
  result := true;
  try
    Sugar.IO.Folder.Delete(Dir);
  except
    result := false;
  end;
end;

function FileOpen(const FileName: DelphiString; Mode: Cardinal): THandle;
begin
  var lMode: Sugar.IO.FileOpenMode;
  if Mode = fmOpenRead then
    lMode := Sugar.IO.FileOpenMode.ReadOnly;
  if ((Mode and fmOpenWrite) <> 0) or ((Mode and fmOpenReadWrite) <> 0) then
    lMode := Sugar.IO.FileOpenMode.ReadWrite;

  var lHandle := new Sugar.IO.FileHandle(FileName, lMode);
  result := TInternalFileHandles.Allocate(lHandle);
end;

function FileCreate(const FileName: String): THandle;
begin
  var lHandle := new Sugar.IO.FileHandle(FileName, Sugar.IO.FileOpenMode.Create);
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
  var lHandle := Sugar.IO.FileHandle(TInternalFileHandles.Get(Handle));
  result := lHandle.Read(Buffer, Offset, Count);
end;

function FileWrite(Handle: THandle; const Buffer: array of Byte; Offset, Count: Cardinal): Integer; 
begin
  var lHandle := Sugar.IO.FileHandle(TInternalFileHandles.Get(Handle));
  lHandle.Write(Buffer, Offset, Count);
  result := Count;
end;

function FileSeek(Handle: THandle; Offset, Origin: Integer): Integer; 
begin
  result := FileSeek(Handle, Int64(Offset), Origin);
end;

function FileSeek(Handle: THandle; const Offset: Int64; Origin: Integer): Int64;
begin
  var lHandle := Sugar.IO.FileHandle(TInternalFileHandles.Get(Handle));
  var lOrigin: Sugar.IO.SeekOrigin;
  case Origin of
    0: lOrigin := Sugar.IO.SeekOrigin.Begin;
    1: lOrigin := Sugar.IO.SeekOrigin.Current;
    2: lOrigin := Sugar.IO.SeekOrigin.End;
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
    Sugar.IO.FileUtils.Delete(FileName);
  except
    result := false;
  end;
end;

function DirectoryExists(const Directory: DelphiString; FollowLink: Boolean := true): Boolean;
begin
  result := true;
  try
    result := Sugar.IO.Folder.Exists(Directory);    
  except
    result := false;
  end;
end;

// Datetime functions

class constructor TFormatSettings;
begin
  SysLocale.DefaultLCID := Sugar.Locale.Current;

end;

function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out Value: TDateTime): Boolean;
begin
  var lDate: TDateTime;
  var lTime: TDateTime;
  result := TryEncodeDate(AYear, AMonth, ADay, out lDate);
  if result then
  begin
    result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, out lTime);
    if result then
      if lTime >= 0 then
        Value := lDate + lTime
      else
        Value := lDate - lTime
  end;
end;

function TryEncodeDate(aYear, aMonth, aDay: Word; out aDate: TDateTime): Boolean;
begin
  var lIsLeap := IsLeapYear(aYear);
  if (aDay >= 1) and (aDay <= MonthDays[lIsLeap, aMonth - 1]) and (aMonth >= 1) and (aMonth <= 12) and (aYear >= 1) and (aYear <= 12) then begin
    var lDays := (aYear - 1) * 365 + (aYear - 1) div 4 - (aYear - 1) div 100 + (aYear - 1) div 400;
    for i: Integer := 1 to aMonth - 1 do
      inc(lDays, MonthDays[lIsLeap, i - 1]);
    aDate := lDays + aDay - DateDelta;
    result := true;
  end
  else 
     result := false;  
end;

function TryEncodeTime(aHour, aMin, aSec, aMSec: Word; out aTime: TDateTime): Boolean;
begin
  if (aHour < HoursPerDay) and (aMin < MinsPerDay) and (aSec < SecsPerMin) and (aMSec < MSecsPerSec) then begin
    var lTime := (aHour * (MinsPerHour * SecsPerMin * MSecsPerSec)) + (aMin * (SecsPerMin * MSecsPerSec)) + (aSec * MSecsPerSec) + aMSec;
    aTime := lTime / MSecsPerDay;
    result := true;
  end
  else
     result := false;
end;

function DateTimeToTimeStamp(aDateTime: TDateTime): TTimeStamp;
begin
  var lTmp := aDateTime - Math.Floor(aDateTime);
  result.Time := Math.Round(Math.Abs(lTmp) * MSecsPerDay);
  //result.Time := Math.Round(Math.Abs(Math.Frac(aDateTime)) * MSecsPerDay);
  result.Date := Integer(Math.Truncate(aDateTime)) + DateDelta;
end;

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
begin
  var lTmp := (TimeStamp.Date - DateDelta) * MSecsPerDay;

  dec(lTmp, DateDelta);
  lTmp := lTmp * MSecsPerDay;
  if lTmp >= 0 then
    inc(lTmp, TimeStamp.Time)
  else
    dec(lTmp, TimeStamp.Time);
  result := lTmp / MSecsPerDay;
end;

function MSecsToTimeStamp(MSecs: Int64): TTimeStamp;
begin
  if MSecs <= 0 then
    raise new Exception("Wrong msec value");

  result.Date := MSecs div MSecsPerDay;
  result.Time := MSecs mod MSecsPerDay;
end;

function TimeStampToMSecs(const TimeStamp: TTimeStamp): Int64;
begin
  result := (TimeStamp.Date * MSecsPerDay) + TimeStamp.Time;  
end;

function IsLeapYear(Year: Word): Boolean;
begin
  result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function StrToDateTime(const S: DelphiString): TDateTime;
begin
  TryStrToDateTime(S, out result);
end;

function StrToDateTime(const S: DelphiString; aFormatSettings: TFormatSettings): TDateTime; 
begin
  TryStrToDateTime(S, out result, aFormatSettings);
end;

function StrToDateTimeDef(const S: DelphiString; aDefault: TDateTime): TDateTime;
begin
  if not TryStrToDateTime(S, out result) then
    result := aDefault;
end;

function StrToDateTimeDef(const S: DelphiString; const aDefault: TDateTime; aFormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToDateTime(S, out result, aFormatSettings) then
    result := aDefault;
end;

function TryStrToDateTime(const S: DelphiString; out aValue: TDateTime): Boolean;
begin
  result := TryStrToDateTime(S, out aValue, FormatSettings);
end;

function TryStrToDateTime(const S: DelphiString; out aValue: TDateTime; aFormatSettings: TFormatSettings): Boolean;
begin
  {$IF COOPER}
  var lFormat := new java.text.SimpleDateFormat;
  var lDateTime := lFormat.parse(S);
  var lCal := java.util.Calendar.getInstance;
  lCal.setTime(lDateTime);
  result := TryEncodeDateTime(lCal.get(java.util.Calendar.YEAR), lCal.get(java.util.Calendar.MONTH), lCal.get(java.util.Calendar.DAY_OF_MONTH),
    lCal.get(java.util.Calendar.HOUR), lCal.get(java.util.Calendar.MINUTE), lCal.get(java.util.Calendar.SECOND), lCal.get(java.util.Calendar.MILLISECOND), out aValue);
  {$ELSEIF ECHOES}
  var lDateTime := System.DateTime.Parse(S, SysLocale.DefaultLCID);
  result := TryEncodeDateTime(lDateTime.Year, lDateTime.Month, lDateTime.Day, lDateTime.Hour, lDateTime.Minute, lDateTime.Second, lDateTime.Millisecond, out aValue);
  {$ELSEIF TOFFEE}
  var lDateFormatter := new NSDateFormatter;
  var lDateTime := lDateFormatter.dateFromString(NSString(S));
  var lCalendar := NSCalendar.currentCalendar;
  var lComponents := lCalendar.components(NSCalendarUnit.CalendarUnitYear or NSCalendarUnit.CalendarUnitMonth or NSCalendarUnit.CalendarUnitDay or 
    NSCalendarUnit.CalendarUnitHour or NSCalendarUnit.CalendarUnitMinute or NSCalendarUnit.CalendarUnitSecond or NSCalendarUnit.NSCalendarUnitNanosecond) fromDate(lDateTime);
  result := TryEncodeDateTime(lComponents.year, lComponents.month, lComponents.day, lComponents.hour, lComponents.minute, lComponents.second, (lComponents.nanosecond / 1000), out aValue);
  {$ENDIF}
end;

function EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  if not TryEncodeDate(Year, Month, Day, out result) then
    raise new Exception("Date encode Error");
end;

function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
  if not TryEncodeTime(Hour, Min, Sec, MSec, out result) then
    raise new Exception("Time encode Error");
end;

procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);
begin
  var lTotal := Integer(Math.Truncate(DateTime));
  var lDays := 1;
  Year := 1900;
  var lLastDays := 0;
  while lDays < lTotal do begin
    lLastDays := lDays;
    if IsLeapYear(Year) then
      inc(lDays, 366)
    else
      inc(lDays, 365);
    if lDays < lTotal then
      inc(Year);
  end;

  Month := 1;
  lLastDays := lTotal - lLastDays;
  while lLastDays > 0 do
  begin
    if lLastDays > MonthDays[IsLeapYear(Year)][Month] then
      dec(lLastDays, MonthDays[IsLeapYear(Year)][Month])
    else
      Break;
    inc(Month);
  end;
end;

function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean;
begin
  DecodeDate(DateTime, var Year, var Month, var Day);
  var lTmp := DateTimeToTimeStamp(DateTime);
  DOW := lTmp.Date mod 7 + 1;
end;

procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);
begin
  var lTmp := DateTime - Math.Floor(DateTime);
  var lNumber := Math.Round(lTmp * MSecsPerDay);
  Hour := lNumber div (MinsPerHour * SecsPerMin * MSecsPerSec);
  var lRem := lNumber mod (MinsPerHour * SecsPerMin * MSecsPerSec);
  Min := lRem div (SecsPerMin * MSecsPerSec);
  lRem := lRem mod (SecsPerMin * MSecsPerSec);
  Sec := lRem div MSecsPerSec;
  MSec := lRem mod MSecsPerSec;
end;

function DayOfWeek(const DateTime: TDateTime): Word;
begin
  var lYear, lMonth, lDay: Word;
  DecodeDateFully(DateTime, var lYear, var lMonth, var lDay, var result);
end;

function Date: TDateTime;
begin
  var lTmp := DateTime.Today.Date;
  result := EncodeDate(lTmp.Year, lTmp.Month, lTmp.Day);
end;

function Time: TDateTime;
begin
  var lTmp := DateTime.Today;
  result := EncodeTime(lTmp.Hour, lTmp.Minute, lTmp.Second, 0); // TODO miliseconds?
end;

function Now: TDateTime;
begin
  var lTmp := DateTime.Today;
  TryEncodeDateTime(lTmp.Year, lTmp.Month, lTmp.Day, lTmp.Hour, lTmp.Minute, lTmp.Second, 0, out result); 
end;

function CurrentYear: Word;
begin
  result := DateTime.Today.Year;
end;

function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer := 1): TDateTime;
begin
  var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
  DecodeDate(DateTime, var lYear, var lMonth, var lDay);
  DecodeTime(DateTime, var lHour, var lMin, var lSec, var lMSec);
  IncAMonth(var lYear, var lMonth, var lDay, NumberOfMonths);
  TryEncodeDateTime(lYear, lMonth, lDay, lHour, lMin, lSec, lMSec, out result);
end;

procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer := 1);
begin
  var lDate := new DateTime(Year, Month, Day);
  lDate.AddMonths(NumberOfMonths);
  Year := lDate.Year;
  Month := lDate.Month;
  Day := lDate.Day;
end;

procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);
begin
  DateTime := Math.Truncate(DateTime);
  var lNewTime := NewTime - Math.Truncate(NewTime);
  if DateTime >= 0 then
    DateTime := DateTime + Math.Abs(lNewTime)
  else
    DateTime := DateTime - Math.Abs(lNewTime);
end;

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
begin
  var lTmp := NewDate;
  ReplaceTime(var lTmp, DateTime);
  DateTime := lTmp;
end;

end.
