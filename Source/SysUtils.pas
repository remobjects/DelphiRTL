namespace RemObjects.Elements.RTL.Delphi;

interface
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

type
  TBytes = public TArray<Byte>;

function UpperCase(const S: DelphiString): DelphiString;
function UpperCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function LowerCase(const S: DelphiString): DelphiString; 
function LowerCase(const S: DelphiString; LocaleOptions: TLocaleOptions): DelphiString; inline;
function CompareStr(const S1, S2: DelphiString): Integer; inline;
function CompareStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Integer;
function SameStr(const S1, S2: DelphiString): Boolean; inline;
function SameStr(const S1, S2: DelphiString; LocaleOptions: TLocaleOptions): Boolean;
/*
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
*/
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
//function AnsiLastChar(const S: UnicodeString): PWideChar; overload;
function Trim(const S: DelphiString): DelphiString;
function TrimLeft(const S: DelphiString): DelphiString;
function TrimRight(const S: DelphiString): DelphiString;
function QuotedStr(const S: DelphiString): DelphiString;
function AnsiQuotedStr(const S: DelphiString; Quote: Char): DelphiString;
function AnsiDequotedStr(const S: DelphiString; aQuote: Char): DelphiString;
//function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle = tlbsCRLF): string; overload;
//function IsValidIdent(const Ident: string; AllowDots: Boolean = False): Boolean;
function IntToStr(Value: Integer): DelphiString;
function IntToStr(Value: Int64): DelphiString;
function UIntToStr(Value: Cardinal): DelphiString;
function UIntToStr(Value: UInt64): DelphiString;
function IntToHex(Value: Integer; Digits: Integer := sizeOf(Integer) * 2): DelphiString;
function IntToHex(Value: Int64; Digits: Integer := sizeOf(Int64) * 2): DelphiString;
function IntToHex(Value: UInt64; Digits: Integer := sizeOf(UInt64) * 2): DelphiString;
function StrToInt(const S: DelphiString): Integer;
//function StrToIntDef(const S: string; Default: Integer): Integer; overload;
//function TryStrToInt(const S: string; out Value: Integer): Boolean; overload;
function StrToInt64(const S: DelphiString): Int64;
//function StrToInt64Def(const S: string; const Default: Int64): Int64; overload;
//function TryStrToInt64(const S: string; out Value: Int64): Boolean; overload;
function StrToUInt64(const S: DelphiString): UInt64;
//function StrToUInt64Def(const S: string; const Default: UInt64): UInt64; overload;
//function TryStrToUInt64(const S: string; out Value: UInt64): Boolean; overload;
function StrToBool(const S: DelphiString): Boolean;
//function StrToBoolDef(const S: string; const Default: Boolean): Boolean; overload;
//function TryStrToBool(const S: string; out Value: Boolean): Boolean; overload;
function BoolToStr(B: Boolean; UseBoolStrs: Boolean := False): DelphiString;

// File functions


function FileOpen(const FileName: DelphiString; Mode: LongWord): THandle;
function FileCreate(const FileName: string): THandle;
function FileCreate(const FileName: string; Rights: Integer): THandle;
function FileCreate(const FileName: string; Mode: LongWord; Rights: Integer): THandle;
/*function FileSystemAttributes(const Path: string): TFileSystemAttributes;

function FileRead(Handle: THandle; var Buffer; Count: LongWord): Integer;
function FileWrite(Handle: THandle; const Buffer; Count: LongWord): Integer; overload;*/
function FileRead(Handle: THandle; var Buffer: TBytes; Offset, Count: LongWord): Integer; 
function FileWrite(Handle: THandle; const Buffer:TBytes; Offset, Count: LongWord): Integer; 
function FileSeek(Handle: THandle; Offset, Origin: Integer): Integer;
function FileSeek(Handle: THandle; const Offset: Int64; Origin: Integer): Int64;
procedure FileClose(Handle: THandle);
/*function FileAge(const FileName: string; out FileDateTime: TDateTime;
  FollowLink: Boolean = True): Boolean; overload;
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
function FileSetDate(const FileName: string; Age: LongInt): Integer; overload;

function FileSetDate(Handle: THandle; Age: Integer): Integer; overload; platform;
function FileGetAttr(const FileName: string; FollowLink: Boolean = True): Integer; platform;

function FileSetAttr(const FileName: string; Attr: Integer; FollowLink: Boolean = True): Integer; platform;
function FileIsReadOnly(const FileName: string): Boolean;
function FileSetReadOnly(const FileName: string; ReadOnly: Boolean): Boolean;
*/
function DeleteFile(const FileName: DelphiString): Boolean;
function RenameFile(const OldName, NewName: DelphiString): Boolean;
function ChangeFileExt(const FileName, aExtension: DelphiString): DelphiString; 
/*
function ChangeFilePath(const FileName, Path: string): string; overload;
function ExtractFilePath(const FileName: string): string; overload;
function ExtractFileDir(const FileName: string): string; overload;
function ExtractFileDrive(const FileName: string): string; overload;
*/
function ExtractFileName(const FileName: DelphiString): DelphiString;
function ExtractFileExt(const FileName: DelphiString): DelphiString;
function GetHomePath: DelphiString;
function ExpandFileName(const FileName: DelphiString): DelphiString;
/*
function ExpandUNCFileName(const FileName: string): string; overload;
function ExtractRelativePath(const BaseName, DestName: string): string; overload;
function IsRelativePath(const Path: string): Boolean;
function ExtractShortPathName(const FileName: string): string; overload;
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
  class var fLock: Object := new Object;
  class var fSlots: array of Object;
  class var fFirstEmpty: Integer := 0; 
  class var fFirstFree: Integer := -1;
public
  class method Allocate(aValue: Object): NativeInt;
  begin 
    if aValue = nil then raise new ArgumentException('Invalid object value');
    locking fLock do begin 
      if (fFirstFree = -1) and (fFirstEmpty >= length(fSlots)) then begin 
        var lNewArray := new Object[length(fSlots) + 1024];
        &Array.Copy(fSlots, lNewArray, length(fSlots));
        fSlots := lNewArray;
      end;
      if fFirstEmpty < length(fSlots) then begin
        fSlots[fFirstEmpty] := aValue;
        inc(fFirstEmpty);
        exit fFirstEmpty; // +1 value since gchandle 0 is reserved.
      end;
      if fFirstFree <> -1 then begin 
        result := fFirstFree;
        var lFirstFree := NativeInt(fSlots[result]);
        assert((lFirstFree and 1) <> 0);
        fFirstFree := lFirstFree shr 1;
        fSlots[result] := aValue;
        inc(result);
        exit;
      end;
    end;
  end;
    
  class method Get(aValue: NativeInt): Object;
  begin
    if aValue = 0 then raise new ArgumentException('Invalid GC Handle'); // not valid
    dec(aValue);
    if aValue >= length(fSlots) then raise new ArgumentException('Invalid GC Handle');
    // we shouldn't need a lock as the value can't *update* underneath us
    result := fSlots[aValue];
    if (NativeInt(result) = 0) or ((NativeInt(result) and 1) <> 0) then raise new ArgumentException('Invalid GC Handle'); // not valid
  end;

  class method Free(aValue: NativeInt);
  begin 
    if aValue = 0 then raise new ArgumentException('Invalid GC Handle'); // not valid
    dec(aValue);
    if aValue >= length(fSlots) then raise new ArgumentException('Invalid GC Handle');
    locking fLock do begin 
      var lValue := fSlots[aValue];
      if (NativeInt(lValue) = 0) or ((NativeInt(lValue) and 1) <> 0) then raise new ArgumentException('Invalid GC Handle');
      //fSlots[aValue] := Object(^Void((fFirstFree shl 1) or 1)); TODO
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

function FileOpen(const FileName: DelphiString; Mode: LongWord): THandle;
begin
  var lMode: Sugar.IO.FileOpenMode;
  if Mode = fmOpenRead then
    lMode := Sugar.IO.FileOpenMode.ReadOnly;
  if ((Mode and fmOpenWrite) <> 0) or ((Mode and fmOpenReadWrite) <> 0) then
    lMode := Sugar.IO.FileOpenMode.ReadWrite;

  var lHandle := new Sugar.IO.FileHandle(FileName, lMode);
  result := TInternalFileHandles.Allocate(lHandle);
end;

function FileCreate(const FileName: string): THandle;
begin
  var lHandle := new Sugar.IO.FileHandle(FileName, Sugar.IO.FileOpenMode.Create);
  result := TInternalFileHandles.Allocate(lHandle);
end;

function FileCreate(const FileName: string; Rights: Integer): THandle;
begin
  result := FileCreate(FileName);
end;

function FileCreate(const FileName: string; Mode: LongWord; Rights: Integer): THandle;
begin
  result := FileCreate(FileName);
end;

function FileRead(Handle: THandle; var Buffer: TBytes; Offset, Count: LongWord): Integer; 
begin
  var lHandle := Sugar.IO.FileHandle(TInternalFileHandles.Get(Handle));
  result := lHandle.Read(Buffer, Offset, Count);
end;

function FileWrite(Handle: THandle; const Buffer:TBytes; Offset, Count: LongWord): Integer; 
begin
  var lHandle := Sugar.IO.FileHandle(TInternalFileHandles.Get(Handle));
  lHandle.Write(Buffer, Offset, Count);
  result := Count;
end;

{ FileSeek changes the current position of the file given by Handle to be
  Offset bytes relative to the point given by Origin. Origin = 0 means that
  Offset is relative to the beginning of the file, Origin = 1 means that
  Offset is relative to the current position, and Origin = 2 means that
  Offset is relative to the end of the file. The return value is the new
  current position, relative to the beginning of the file, or -1 if an error
  occurred. }

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


end.
