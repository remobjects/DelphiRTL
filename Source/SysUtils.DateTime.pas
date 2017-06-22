namespace RemObjects.Elements.RTL.Delphi;

interface

{$GLOBALS ON}

uses
  RemObjects.Elements.RTL;

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

function DayOfWeek(const DateTime: TDateTime): Word;
function Date: TDateTime;
function Time: TDateTime;
function Now: TDateTime;
function CurrentYear: Word;
function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer := 1): TDateTime;
procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer := 1);
procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);
procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
// Pending DateTime funcs
function DateToStr(const DateTime: TDateTime): DelphiString; inline;
function DateToStr(const DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;  inline;
function TimeToStr(const DateTime: TDateTime): DelphiString; inline;
function TimeToStr(const DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;  inline;
function DateTimeToStr(const DateTime: TDateTime): DelphiString; inline;
function DateTimeToStr(const DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;  inline;

function StrToDate(const S: DelphiString): TDateTime; inline;
function StrToDate(const S: DelphiString; const aFormatSettings: TFormatSettings): TDateTime;
function StrToDateDef(const S: DelphiString; const aDefault: TDateTime): TDateTime; inline;
function StrToDateDef(const S: DelphiString; const aDefault: TDateTime; const aFormatSettings: TFormatSettings): TDateTime;
function TryStrToDate(const S: DelphiString; out Value: TDateTime): Boolean; inline;
function TryStrToDate(const S: DelphiString; out aValue: TDateTime; const aFormatSettings: TFormatSettings): Boolean;

function StrToTime(const S: DelphiString): TDateTime; inline;
function StrToTime(const S: DelphiString; const aFormatSettings: TFormatSettings): TDateTime;
function StrToTimeDef(const S: DelphiString; const aDefault: TDateTime): TDateTime; inline;
function StrToTimeDef(const S: DelphiString; const aDefault: TDateTime; const aFormatSettings: TFormatSettings): TDateTime;
function TryStrToTime(const S: DelphiString; out aValue: TDateTime): Boolean; inline;
function TryStrToTime(const S: DelphiString; out aValue: TDateTime; const aFormatSettings: TFormatSettings): Boolean;

function FormatDateTime(const Format: DelphiString; DateTime: TDateTime): DelphiString;  inline;
function FormatDateTime(const Format: DelphiString; DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;
procedure DateTimeToString(var aResult: DelphiString; const Format: DelphiString; DateTime: TDateTime);  inline;
procedure DateTimeToString(var aResult: DelphiString; const Format: DelphiString; DateTime: TDateTime; const aFormatSettings: TFormatSettings);

function DateTimeToUnix(const aValue: TDateTime): Int64;
function UnixToDateTime(const aValue: Int64): TDateTime;

implementation

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
  if (aDay >= 1) and (aDay <= MonthDays[lIsLeap, aMonth - 1]) and (aMonth >= 1) and (aMonth <= 12) and (aYear >= 1) and (aYear <= 9999) then begin
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
  if (aHour < HoursPerDay) and (aMin < MinsPerHour) and (aSec < SecsPerMin) and (aMSec < MSecsPerSec) then begin
    var lTime := (aHour * (MinsPerHour * SecsPerMin * MSecsPerSec)) + (aMin * (SecsPerMin * MSecsPerSec)) + (aSec * MSecsPerSec) + aMSec;
    aTime := lTime / FMSecsPerDay;
    result := true;
  end
  else
     result := false;
end;

function DateTimeToTimeStamp(aDateTime: TDateTime): TTimeStamp;
begin
  var lTmp := aDateTime - Math.Floor(aDateTime);
  result.Time := Math.Round(Math.Abs(lTmp) * MSecsPerDay);
  result.Date := Integer(Math.Truncate(aDateTime)) + DateDelta;
end;

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
begin
  var lTmp: Int64 := TimeStamp.Date;
  dec(lTmp, DateDelta);
  lTmp := lTmp * MSecsPerDay;
  if lTmp >= 0 then
    inc(lTmp, TimeStamp.Time)
  else
    dec(lTmp, TimeStamp.Time);
  result := lTmp / FMSecsPerDay;
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
  result := (Int64(TimeStamp.Date) * Int64(MSecsPerDay)) + TimeStamp.Time;
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
  Day := lTotal - lLastDays;
  while Day > 0 do
  begin
    if Day > MonthDays[IsLeapYear(Year)][Month-1] then
      dec(Day, MonthDays[IsLeapYear(Year)][Month-1])
    else
      Break;
    inc(Month);
  end;
  if Day = 0 then
    Day := 1;
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
  var lNumber := Math.Round(Math.Abs(lTmp) * FMSecsPerDay);
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
  lDate := lDate.AddMonths(NumberOfMonths);
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

function DateToStr(const DateTime: TDateTime): DelphiString;
begin
  result := DateToStr(DateTime, FormatSettings);
end;

function DateToStr(const DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;
begin
  DateTimeToString(var result, aFormatSettings.ShortDateFormat, DateTime, aFormatSettings);
end;

function TimeToStr(const DateTime: TDateTime): DelphiString;
begin
  result := TimeToStr(DateTime, FormatSettings);
end;

function TimeToStr(const DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;
begin
  DateTimeToString(var result, aFormatSettings.LongTimeFormat, DateTime, aFormatSettings);
end;

function DateTimeToStr(const DateTime: TDateTime): DelphiString;
begin
  result := DateTimeToStr(DateTime, FormatSettings);
end;

function DateTimeToStr(const DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;
begin
  DateTimeToString(var result, aFormatSettings.ShortDateFormat + ' ' + aFormatSettings.LongTimeFormat, DateTime, aFormatSettings)
end;

function StrToDate(const S: DelphiString): TDateTime;
begin
  if not TryStrToDate(S, out result, FormatSettings) then
    raise new Exception("Date to string error");
end;

function StrToDate(const S: DelphiString; const aFormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToDate(S, out result, aFormatSettings) then
    raise new Exception("Date to string error");
end;

function StrToDateDef(const S: DelphiString; const aDefault: TDateTime): TDateTime;
begin
  if not TryStrToDate(S, out result, FormatSettings) then
    result := aDefault;
end;

function StrToDateDef(const S: DelphiString; const aDefault: TDateTime; const aFormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToDate(S, out result, aFormatSettings) then
    result := aDefault;
end;

function TryStrToDate(const S: DelphiString; out Value: TDateTime): Boolean;
begin
  result := TryStrToDate(S, out Value, FormatSettings);
end;

function TryStrToDate(const S: DelphiString; out aValue: TDateTime; const aFormatSettings: TFormatSettings): Boolean;
begin
  {$IF COOPER}
  var lFormat := new java.text.SimpleDateFormat(aFormatSettings.ShortDateFormat);
  var lDateTime := lFormat.parse(S);
  var lCal := java.util.Calendar.getInstance;
  lCal.setTime(lDateTime);
  result := TryEncodeDateTime(lCal.get(java.util.Calendar.YEAR), lCal.get(java.util.Calendar.MONTH), lCal.get(java.util.Calendar.DAY_OF_MONTH),
    lCal.get(java.util.Calendar.HOUR), lCal.get(java.util.Calendar.MINUTE), lCal.get(java.util.Calendar.SECOND), lCal.get(java.util.Calendar.MILLISECOND), out aValue);
  {$ELSEIF ECHOES}
  var lFormats := new String[1];
  var lDateTime: System.DateTime;
  lFormats[0] := aFormatSettings.ShortDateFormat;
  result := System.DateTime.TryParseExact(S, lFormats, System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.None, out lDateTime);
  if result then
    result := TryEncodeDateTime(lDateTime.Year, lDateTime.Month, lDateTime.Day, lDateTime.Hour, lDateTime.Minute, lDateTime.Second, lDateTime.Millisecond, out aValue);
  {$ELSEIF TOFFEE}
  var lDateFormatter := new NSDateFormatter;
  lDateFormatter.locale := NSLocale.localeWithLocaleIdentifier("en_US_POSIX");
  lDateFormatter.dateFormat := aFormatSettings.ShortDateFormat;
  var lDateTime := lDateFormatter.dateFromString(NSString(S));
  var lCalendar := NSCalendar.currentCalendar;
  var lComponents := lCalendar.components(NSCalendarUnit.CalendarUnitYear or NSCalendarUnit.CalendarUnitMonth or NSCalendarUnit.CalendarUnitDay or
    NSCalendarUnit.CalendarUnitHour or NSCalendarUnit.CalendarUnitMinute or NSCalendarUnit.CalendarUnitSecond or NSCalendarUnit.NSCalendarUnitNanosecond) fromDate(lDateTime);
  result := TryEncodeDateTime(lComponents.year, lComponents.month, lComponents.day, lComponents.hour, lComponents.minute, lComponents.second, (lComponents.nanosecond / 1000), out aValue);
  {$ENDIF}
end;

function StrToTime(const S: DelphiString): TDateTime;
begin
  result := StrToTime(S, FormatSettings);
end;

function StrToTime(const S: DelphiString; const aFormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToTime(S, out result, aFormatSettings) then
    raise new Exception("Time to string error");
end;

function StrToTimeDef(const S: DelphiString; const aDefault: TDateTime): TDateTime;
begin
  result := StrToTimeDef(S, aDefault, FormatSettings);
end;

function StrToTimeDef(const S: DelphiString; const aDefault: TDateTime; const aFormatSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToTime(S, out result, aFormatSettings) then
    result := aDefault;
end;

function TryStrToTime(const S: DelphiString; out aValue: TDateTime): Boolean;
begin
  result := TryStrToTime(S, out aValue, FormatSettings);
end;

function TryStrToTime(const S: DelphiString; out aValue: TDateTime; const aFormatSettings: TFormatSettings): Boolean;
begin
  var lAMPM := (S.IndexOf(aFormatSettings.TimeAMString) >= 0) or (S.IndexOf('AM') >= 0) or
    (S.IndexOf(aFormatSettings.TimePMString) >= 0) or (S.IndexOf('PM') >= 0);
  var lFormats := new String[2];
  {$IF COOPER OR TOFFEE}
  if lAMPM then begin
    lFormats[0] := "hh" + aFormatSettings.TimeSeparator + "mm" + aFormatSettings.TimeSeparator + "ss a";
    lFormats[1] := "hh" + aFormatSettings.TimeSeparator + "mm a";
  end
  else begin
    lFormats[0] := "HH" + aFormatSettings.TimeSeparator + "mm" + aFormatSettings.TimeSeparator + "ss";
    lFormats[1] := "HH" + aFormatSettings.TimeSeparator + "mm";
  end;
  {$ENDIF}

  {$IF COOPER}
  var lFormat := new java.text.SimpleDateFormat(lFormats[0]);
  var lDateTime := lFormat.parse(S);
  if lDateTime = nil then begin
    lFormat := new java.text.SimpleDateFormat(lFormats[1]);
    lDateTime := lFormat.parse(S);
  end;
  if lDateTime <> nil then begin
    var lCal := java.util.Calendar.getInstance;
    lCal.setTime(lDateTime);
    result := TryEncodeDateTime(lCal.get(java.util.Calendar.YEAR), lCal.get(java.util.Calendar.MONTH), lCal.get(java.util.Calendar.DAY_OF_MONTH),
    lCal.get(java.util.Calendar.HOUR), lCal.get(java.util.Calendar.MINUTE), lCal.get(java.util.Calendar.SECOND), lCal.get(java.util.Calendar.MILLISECOND), out aValue);
  end
  else
    result := false;
  {$ELSEIF ECHOES}
  if lAMPM then begin
    lFormats[0] := "hh" + aFormatSettings.TimeSeparator + "mm" + aFormatSettings.TimeSeparator + "ss tt";
    lFormats[1] := "hh" + aFormatSettings.TimeSeparator + "mm tt";
  end
  else begin
    lFormats[0] := "HH" + aFormatSettings.TimeSeparator + "mm" + aFormatSettings.TimeSeparator + "ss";
    lFormats[1] := "HH" + aFormatSettings.TimeSeparator + "mm";
  end;

  var lDateTime: System.DateTime;
  result := System.DateTime.TryParseExact(S, lFormats, System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.None, out lDateTime);
  if result then
    result := TryEncodeDateTime(lDateTime.Year, lDateTime.Month, lDateTime.Day, lDateTime.Hour, lDateTime.Minute, lDateTime.Second, lDateTime.Millisecond, out aValue);
  {$ELSEIF TOFFEE}
  var lDateFormatter := new NSDateFormatter;
  lDateFormatter.locale := NSLocale.localeWithLocaleIdentifier("en_US_POSIX");
  lDateFormatter.dateFormat := lFormats[0];
  var lDateTime := lDateFormatter.dateFromString(NSString(S));
  if lDateTime = nil then begin
    lDateFormatter.dateFormat := lFormats[1];
    lDateTime := lDateFormatter.dateFromString(NSString(S));
  end;

  if lDateTime <> nil then begin
    var lCalendar := NSCalendar.currentCalendar;
    var lComponents := lCalendar.components(NSCalendarUnit.CalendarUnitYear or NSCalendarUnit.CalendarUnitMonth or NSCalendarUnit.CalendarUnitDay or
      NSCalendarUnit.CalendarUnitHour or NSCalendarUnit.CalendarUnitMinute or NSCalendarUnit.CalendarUnitSecond or NSCalendarUnit.NSCalendarUnitNanosecond) fromDate(lDateTime);
    result := TryEncodeDateTime(lComponents.year, lComponents.month, lComponents.day, lComponents.hour, lComponents.minute, lComponents.second, (lComponents.nanosecond / 1000), out aValue);
  end
  else
     result := false;
  {$ENDIF}
end;

function FormatDateTime(const Format: DelphiString; DateTime: TDateTime): DelphiString;
begin
  result := FormatDateTime(Format, DateTime, FormatSettings);
end;

function FormatDateTime(const Format: DelphiString; DateTime: TDateTime; const aFormatSettings: TFormatSettings): DelphiString;
begin
  DateTimeToString(var result, Format, DateTime, aFormatSettings);
end;

procedure DateTimeToString(var aResult: DelphiString; const Format: DelphiString; DateTime: TDateTime);
begin
  DateTimeToString(var aResult, Format, DateTime, FormatSettings);
end;

function FixFormatString(aFormat: DelphiString): DelphiString;
begin
  result := aFormat;
  var lLastHourPos := -1;
  for i: Integer := 0 to aFormat.Length - 1 do begin
    case aFormat.Chars[i] of
      'n': begin
        result.Chars[i] := 'm';
        lLastHourPos := -1;
      end;

      'y', 's', 'd', 'M': lLastHourPos := -1;
       
      'm': if lLastHourPos = -1 then result.Chars[i] := 'M';

      'h': lLastHourPos := i;
    end;
  end;
end;

procedure DateTimeToString(var aResult: DelphiString; const Format: DelphiString; DateTime: TDateTime; const aFormatSettings: TFormatSettings);
begin
  var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
  DecodeDateTime(DateTime, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
  var lFormat := Format;
  if not DelphiString.IsNullOrEmpty(Format) then
    lFormat := FixFormatString(Format);
  {$IF COOPER}
  var lCalendar := java.util.Calendar.getInstance;
  lCalendar.set(lYear, lMonth - 1, lDay, lHour, lMin, lSec);
  var lDateFormat := new java.text.SimpleDateFormat(lFormat);
  var lDateTime := lCalendar.getTime;
  aResult := lDateFormat.format(lDateTime);
  {$ELSEIF ECHOES}
  var lDateTime := new System.DateTime(lYear, lMonth, lDay, lHour, lMin, lSec, lMSec);
  aResult := lDateTime.ToString(lFormat);
  {$ELSEIF ISLAND AND WINDOWS}
  //TODO
  {$ELSEIF TOFFEE}
  var lFormatter := new NSDateFormatter;
  lFormatter.dateFormat := lFormat;
  var lCalendar := NSCalendar.currentCalendar;
  var lComponents := new NSDateComponents;
  lComponents.year := lYear;
  lComponents.month := lMonth;
  lComponents.day := lDay;
  lComponents.hour := lHour;
  lComponents.minute := lMin;
  lComponents.second := lSec;
  lComponents.nanosecond := lMSec * 1000;
  var lDateTime := lCalendar.dateFromComponents(lComponents);
  aResult := lFormatter.stringFromDate(lDateTime);
  {$ENDIF}
end;

function DateTimeToUnix(const aValue: TDateTime): Int64;
begin
  result := Math.Round((aValue - UnixDateDelta) * SecsPerDay);
end;

function UnixToDateTime(const aValue: Int64): TDateTime;
begin
  result := (aValue / SecsPerDay) + UnixDateDelta;
end;

end.