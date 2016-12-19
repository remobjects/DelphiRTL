namespace RemObjects.Elements.RTL.Delphi;

interface

uses
  Sugar;

{$GLOBALS ON}

function DateOf(const aValue: TDateTime): TDateTime; inline;
function TimeOf(const aValue: TDateTime): TDateTime; inline;

function IsInLeapYear(const aValue: TDateTime): Boolean;
function IsPM(const aValue: TDateTime): Boolean; inline;
function IsAM(const aValue: TDateTime): Boolean; inline;
function IsValidDate(const aYear, aMonth, aDay: Word): Boolean;
function IsValidTime(const aHour, aMinute, aSecond, aMilliSecond: Word): Boolean;
function IsValidDateTime(const aYear, aMonth, aDay, aHour, aMinute, aSecond, aMilliSecond: Word): Boolean; inline;
function IsValidDateDay(const aYear, aDayOfYear: Word): Boolean;
function IsValidDateWeek(const aYear, aWeekOfYear, aDayOfWeek: Word): Boolean;
function IsValidDateMonthWeek(const aYear, aMonth, aWeekOfMonth, aDayOfWeek: Word): Boolean;
function WeeksInYear(const aValue: TDateTime): Word; inline; 
function WeeksInAYear(const aYear: Word): Word;
function DaysInYear(const aValue: TDateTime): Word; inline;
function DaysInAYear(const aYear: Word): Word; inline;
function DaysInMonth(const aValue: TDateTime): Word;
function DaysInAMonth(const aYear, aMonth: Word): Word;
function Today: TDateTime;
function Yesterday: TDateTime;
function Tomorrow: TDateTime;
function IsToday(const aValue: TDateTime): Boolean;
function IsSameDay(const aValue, aBasis: TDateTime): Boolean;

function YearOf(const aValue: TDateTime): Word;
function MonthOf(const aValue: TDateTime): Word;
function WeekOf(const aValue: TDateTime): Word;
function DayOf(const aValue: TDateTime): Word;
function HourOf(const aValue: TDateTime): Word;
function MinuteOf(const aValue: TDateTime): Word;
function SecondOf(const aValue: TDateTime): Word;
function MilliSecondOf(const aValue: TDateTime): Word;

function StartOfTheYear(const aValue: TDateTime): TDateTime;
function EndOfTheYear(const aValue: TDateTime): TDateTime;
function StartOfAYear(const aYear: Word): TDateTime;
function EndOfAYear(const aYear: Word): TDateTime;

function StartOfTheMonth(const aValue: TDateTime): TDateTime;
function EndOfTheMonth(const aValue: TDateTime): TDateTime;
function StartOfAMonth(const aYear, aMonth: Word): TDateTime;
function EndOfAMonth(const aYear, aMonth: Word): TDateTime;

function StartOfTheWeek(const aValue: TDateTime): TDateTime;
function EndOfTheWeek(const aValue: TDateTime): TDateTime;
function StartOfAWeek(const aYear, aWeekOfYear: Word; const aDayOfWeek: Word := 1): TDateTime;
function EndOfAWeek(const aYear, aWeekOfYear: Word; const aDayOfWeek: Word := 7): TDateTime;

function StartOfTheDay(const aValue: TDateTime): TDateTime; inline;
function EndOfTheDay(const aValue: TDateTime): TDateTime;
function StartOfADay(const aYear, aMonth, aDay: Word): TDateTime; 
function EndOfADay(const aYear, aMonth, aDay: Word): TDateTime; 
function StartOfADay(const aYear, aDayOfYear: Word): TDateTime; 
function EndOfADay(const aYear, aDayOfYear: Word): TDateTime; 

function MonthOfTheYear(const aValue: TDateTime): Word; inline;
function WeekOfTheYear(const aValue: TDateTime): Word;
function WeekOfTheYear(const aValue: TDateTime; var aYear: Word): Word; 
function DayOfTheYear(const aValue: TDateTime): Word;
function HourOfTheYear(const aValue: TDateTime): Word;
function MinuteOfTheYear(const aValue: TDateTime): Cardinal;
function SecondOfTheYear(const aValue: TDateTime): Cardinal;
function MilliSecondOfTheYear(const aValue: TDateTime): Int64;

function WeekOfTheMonth(const aValue: TDateTime): Word;
function WeekOfTheMonth(const aValue: TDateTime; var aYear, aMonth: Word): Word; 
function DayOfTheMonth(const aValue: TDateTime): Word; inline;
function HourOfTheMonth(const aValue: TDateTime): Word;
function MinuteOfTheMonth(const aValue: TDateTime): Word;
function SecondOfTheMonth(const aValue: TDateTime): Cardinal;
function MilliSecondOfTheMonth(const aValue: TDateTime): Cardinal;

function DayOfTheWeek(const aValue: TDateTime): Word;
function HourOfTheWeek(const aValue: TDateTime): Word;
function MinuteOfTheWeek(const aValue: TDateTime): Word;
function SecondOfTheWeek(const aValue: TDateTime): Cardinal;
function MilliSecondOfTheWeek(const aValue: TDateTime): Cardinal;

function HourOfTheDay(const aValue: TDateTime): Word; inline;
function MinuteOfTheDay(const aValue: TDateTime): Word;
function SecondOfTheDay(const aValue: TDateTime): Cardinal;
function MilliSecondOfTheDay(const aValue: TDateTime): Cardinal;
function MinuteOfTheHour(const aValue: TDateTime): Word; inline;
function SecondOfTheHour(const aValue: TDateTime): Word;
function MilliSecondOfTheHour(const aValue: TDateTime): Cardinal;
function SecondOfTheMinute(const aValue: TDateTime): Word; inline;
function MilliSecondOfTheMinute(const aValue: TDateTime): Cardinal;
function MilliSecondOfTheSecond(const aValue: TDateTime): Word; inline;

function DateTimeToMilliseconds(const aDateTime: TDateTime): Int64;

function WithinPastYears(const aNow, aThen: TDateTime; const aYears: Integer): Boolean; inline;
function WithinPastMonths(const aNow, aThen: TDateTime; const aMonths: Integer): Boolean; inline;
function WithinPastWeeks(const aNow, aThen: TDateTime; const aWeeks: Integer): Boolean; inline;
function WithinPastDays(const aNow, aThen: TDateTime; const aDays: Integer): Boolean; inline;
function WithinPastHours(const aNow, aThen: TDateTime; const aHours: Int64): Boolean; inline;
function WithinPastMinutes(const aNow, aThen: TDateTime; const aMinutes: Int64): Boolean; inline;
function WithinPastSeconds(const aNow, aThen: TDateTime; const aSeconds: Int64): Boolean; inline;
function WithinPastMilliSeconds(const aNow, aThen: TDateTime; const aMilliSeconds: Int64): Boolean; inline;

function YearsBetween(const aNow, aThen: TDateTime): Integer;
function MonthsBetween(const aNow, aThen: TDateTime): Integer;
function WeeksBetween(const aNow, aThen: TDateTime): Integer;
function DaysBetween(const aNow, aThen: TDateTime): Integer;
function HoursBetween(const aNow, aThen: TDateTime): Int64;
function MinutesBetween(const aNow, aThen: TDateTime): Int64;
function SecondsBetween(const aNow, aThen: TDateTime): Int64;
function MilliSecondsBetween(const aNow, aThen: TDateTime): Int64;

/*function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean := True): Boolean;
function DateInRange(ADate: TDate; AStartDate, AEndDate: TDate; AInclusive: Boolean := True): Boolean;
function TimeInRange(ATime: TTime; AStartTime, AEndTime: TTime; AInclusive: Boolean := True): Boolean;

function YearSpan(const ANow, AThen: TDateTime): Double;
function MonthSpan(const ANow, AThen: TDateTime): Double;
function WeekSpan(const ANow, AThen: TDateTime): Double;
function DaySpan(const ANow, AThen: TDateTime): Double;
function HourSpan(const ANow, AThen: TDateTime): Double;
function MinuteSpan(const ANow, AThen: TDateTime): Double;
function SecondSpan(const ANow, AThen: TDateTime): Double;
function MilliSecondSpan(const ANow, AThen: TDateTime): Double;

function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer := 1): TDateTime; inline;
function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer := 1): TDateTime; inline;
*/
function IncDay(const aValue: TDateTime; const aNumberOfDays: Integer := 1): TDateTime; inline;
/*
function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64 := 1): TDateTime; inline;
function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64 := 1): TDateTime; inline;
function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64 := 1): TDateTime; inline;
function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64 := 1): TDateTime;

function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
*/
//function EncodeDateWeek(const AYear, AWeekOfYear: Word; {ISO 8601} const ADayOfWeek: Word := 1): TDateTime;
//procedure DecodeDateWeek(const AValue: TDateTime; out AYear, {ISO 8601} AWeekOfYear, ADayOfWeek: Word);

function EncodeDateDay(const aYear, aDayOfYear: Word): TDateTime;
procedure DecodeDateDay(const aValue: TDateTime; out aYear, aDayOfYear: Word);
/*
function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, {ISO 8601x} ADayOfWeek: Word): TDateTime;
procedure DecodeDateMonthWeek(const AValue: TDateTime; {ISO 8601x} out AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);

function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AValue: TDateTime): Boolean;
function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; {ISO 8601} out AValue: TDateTime; const ADayOfWeek: Word := 1): Boolean;
function TryEncodeDateDay(const AYear, ADayOfYear: Word; out AValue: TDateTime): Boolean;
function TryEncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, {ISO 8601x} ADayOfWeek: Word; var AValue: TDateTime): Boolean;

function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;
function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime;
function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime;
function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime;

function RecodeDate(const AValue: TDateTime; const AYear, AMonth, ADay: Word): TDateTime;
function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;

function TryRecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AResult: TDateTime): Boolean;

function CompareDateTime(const A, B: TDateTime): TValueRelationship;
function SameDateTime(const A, B: TDateTime): Boolean;
function CompareDate(const A, B: TDateTime): TValueRelationship;
function SameDate(const A, B: TDateTime): Boolean;
function CompareTime(const A, B: TDateTime): TValueRelationship;
function SameTime(const A, B: TDateTime): Boolean;

function NthDayOfWeek(const AValue: TDateTime): Word;
procedure DecodeDayOfWeekInMonth(const AValue: TDateTime; out AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);
function EncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word): TDateTime;
function TryEncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word; out AValue: TDateTime): Boolean;

procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; const ABaseDate: TDateTime = 0);
procedure InvalidDateWeekError(const AYear, AWeekOfYear, ADayOfWeek: Word);
procedure InvalidDateDayError(const AYear, ADayOfYear: Word);
procedure InvalidDateMonthWeekError(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
procedure InvalidDayOfWeekInMonthError(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);

function DateTimeToJulianDate(const AValue: TDateTime): Double;
function JulianDateToDateTime(const AValue: Double): TDateTime;
function TryJulianDateToDateTime(const AValue: Double; out ADateTime: TDateTime): Boolean;

function DateTimeToModifiedJulianDate(const AValue: TDateTime): Double;
function ModifiedJulianDateToDateTime(const AValue: Double): TDateTime;
function TryModifiedJulianDateToDateTime(const AValue: Double; out ADateTime: TDateTime): Boolean;

function DateTimeToUnix(const AValue: TDateTime; AInputIsUTC: Boolean := True): Int64;
function UnixToDateTime(const AValue: Int64; AReturnUTC: Boolean := True): TDateTime;
*/
const
  DaysPerWeek = 7;
  WeeksPerFortnight = 2;
  MonthsPerYear = 12;
  YearsPerDecade = 10;
  YearsPerCentury = 100;
  YearsPerMillennium = 1000;

  DayMonday = 1;
  DayTuesday = 2;
  DayWednesday = 3;
  DayThursday = 4;
  DayFriday = 5;
  DaySaturday = 6;
  DaySunday = 7;

  MonthJanuary = 1;
  MonthFebruary = 2;
  MonthMarch = 3;
  MonthApril = 4;
  MonthMay = 5;
  MonthJune = 6;
  MonthJuly = 7;
  MonthAugust = 8;
  MonthSeptember = 9;
  MonthOctober = 10;
  MonthNovember = 11;
  MonthDecember = 12;
  
  OneHour = 1 / HoursPerDay;
  OneMinute = 1 / MinsPerDay;
  OneSecond = 1 / SecsPerDay;
  OneMillisecond = 1 / MSecsPerDay;

  EpochAsJulianDate = 2415018.5;
  EpochAsUnixDate   = -2209161600;
  //DaysPerYear: array [Boolean] of Word = (365, 366);
  RecodeLeaveFieldAsIs = high(Word);

var
  ApproxDaysPerMonth: Double := 30.4375;
  ApproxDaysPerYear: Double  := 365.25;
/*
type
  ELocalTimeInvalid = class(Exception);
  EDateTimeException = class(Exception);

  TLocalTimeType = (
    lttStandard,
    lttDaylight,
    lttAmbiguous,
    lttInvalid
  );

  TTimeZone = abstract class
  private
    class var FLocal: TTimeZone;
    class constructor Create;
    //class destructor Destroy;
    function GetAbbreviationForNow: DelphiString; inline;
    function GetDisplayNameForNow: DelphiString; inline;
    function GetUtcOffsetInSeconds(const ADateTime: TDateTime; const ForceDaylight: Boolean): Int64;
    function GetCurrentUtcOffset: TTimeSpan; inline;
  protected
    procedure DoGetOffsetsAndType(const ADateTime: TDateTime; out AOffset, ADstSave: Int64; out AType: TLocalTimeType); virtual; abstract;
    function DoGetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean): DelphiString; virtual; abstract;
    function DoGetID: DelphiString; virtual; abstract;
  public
    function GetUtcOffset(const ADateTime: TDateTime; const ForceDaylight: Boolean := False): TTimeSpan; inline;
    function ToUniversalTime(const ADateTime: TDateTime; const ForceDaylight: Boolean := False): TDateTime; inline;
    function ToLocalTime(const ADateTime: TDateTime): TDateTime;
    function GetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean := False): DelphiString;
    function GetAbbreviation(const ADateTime: TDateTime; const ForceDaylight: Boolean := False): DelphiString;
    function GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType; inline;
    function IsStandardTime(const ADateTime: TDateTime; const ForceDaylight: Boolean := False): Boolean;
    function IsInvalidTime(const ADateTime: TDateTime): Boolean; inline;
    function IsAmbiguousTime(const ADateTime: TDateTime): Boolean; inline;
    function IsDaylightTime(const ADateTime: TDateTime; const ForceDaylight: Boolean := False): Boolean;
    property ID: DelphiString read DoGetID;
    property DisplayName: DelphiString read GetDisplayNameForNow;
    property Abbreviation: DelphiString read GetAbbreviationForNow;
    property UtcOffset: TTimeSpan read GetCurrentUtcOffset;
    class property Local: TTimeZone read FLocal;
  end;

function ISO8601ToDate(const AISODate: DelphiString; AReturnUTC: Boolean := True): TDateTime;
function TryISO8601ToDate(const AISODate: DelphiString; out Value: TDateTime; AReturnUTC: Boolean := True): Boolean;
function DateToISO8601(const ADate: TDateTime; AInputIsUTC: Boolean := True): DelphiString;
*/

implementation

function DateOf(const aValue: TDateTime): TDateTime;
begin
  result := Math.Truncate(aValue);
end;

function TimeOf(const aValue: TDateTime): TDateTime;
begin
  result := Math.Abs(aValue) - Math.Abs(Math.Truncate(aValue));
end;

function IsInLeapYear(const aValue: TDateTime): Boolean;
begin
  result := IsLeapYear(YearOf(aValue));
end;

function IsPM(const aValue: TDateTime): Boolean;
begin
  result := HourOf(aValue) >= 12;
end;

function IsAM(const aValue: TDateTime): Boolean;
begin
  result := HourOf(aValue) < 12;
end;

function IsValidDate(const aYear, aMonth, aDay: Word): Boolean;
begin
  result := (aDay >= 1) and (aDay <= DaysInAMonth(aYear, aMonth)) and (aMonth >= 1) and
   (aMonth <= 12) and (aYear >= 1) and (aYear <= 9999);
end;

function IsValidTime(const aHour, aMinute, aSecond, aMilliSecond: Word): Boolean;
begin
  result := (aMilliSecond < MSecsPerSec) and (aSecond < SecsPerMin) and (aMinute < MinsPerHour) and (aHour < HoursPerDay);
end;

function IsValidDateTime(const aYear, aMonth, aDay, aHour, aMinute, aSecond, aMilliSecond: Word): Boolean;
begin
  result := IsValidDate(aYear, aMonth, aDay) and IsValidTime(aHour, aMonth, aSecond, aMilliSecond);
end;

function IsValidDateDay(const aYear, aDayOfYear: Word): Boolean;
begin
  result := (aYear >= 1) and (aYear <= 9999) and (aDayOfYear >= 1) and (aDayOfYear <= DaysInAYear(aYear));
end;

function IsValidDateWeek(const aYear, aWeekOfYear, aDayOfWeek: Word): Boolean;
begin
  result := (aYear >= 1) and (aYear <= 9999) and (aWeekOfYear >= 1) and (aWeekOfYear <= WeeksInAYear(aYear)) and
    (aDayOfWeek >= 1) and (aDayOfWeek <= 7);
end;

function IsValidDateMonthWeek(const aYear, aMonth, aWeekOfMonth, aDayOfWeek: Word): Boolean;
begin
  result := (aYear >= 1) and (aYear <= 9999) and (aMonth >= 1)  and (aMonth <= DaysInAMonth(aYear, aMonth)) and 
    (aWeekOfMonth >= 1) and (aWeekOfMonth <= 5) and (aDayOfWeek >= 1) and (aDayOfWeek <= 7);
end;

function WeeksInYear(const aValue: TDateTime): Word;
begin
  result := WeeksInAYear(YearOf(aValue));
end;

function WeeksInAYear(const aYear: Word): Word;                       
begin
  var lTmp := EncodeDate(aYear, 12, 31);
  result := (DayOfTheYear(lTmp) - DayOfWeek(lTmp) + 10) div 7;
end;

function DaysInYear(const aValue: TDateTime): Word;
begin
  result := DaysInAYear(YearOf(aValue));
end;

function DaysInAYear(const aYear: Word): Word;
begin
  result := if IsLeapYear(aYear) then 366 else 365;
end;

function DaysInMonth(const aValue: TDateTime): Word;
begin
  var lDay, lMonth, lYear: Word;
  DecodeDate(aValue, var lYear, var lMonth, var lDay);
  result := DaysInAMonth(lYear, lMonth);
end;

function DaysInAMonth(const aYear, aMonth: Word): Word;
begin
  result := MonthDays[IsLeapYear(aYear)][aMonth - 1];
end;

function Today: TDateTime;
begin
  result := Date;  
end;

function Yesterday: TDateTime;
begin
  result := IncDay(Date, -1);
end;

function Tomorrow: TDateTime;
begin
  result := IncDay(Date);
end;

function IsToday(const aValue: TDateTime): Boolean;
begin
  result := Math.Truncate(Date) = Math.Truncate(aValue);
end;

function IsSameDay(const aValue, aBasis: TDateTime): Boolean;
begin
  result := DateOf(aValue) = DateOf(aBasis);
end;

function YearOf(const aValue: TDateTime): Word;
begin
  var lMonth, lDay: Word;
  DecodeDate(aValue, var result, var lMonth, var lDay);
end;

function MonthOf(const aValue: TDateTime): Word;
begin
  var lYear, lDay: Word;
  DecodeDate(aValue, var lYear, var result, var lDay);
end;

function WeekOf(const aValue: TDateTime): Word;
begin
  result := WeekOfTheYear(aValue);
end;

function DayOf(const aValue: TDateTime): Word;
begin
  var lYear, lMonth: Word;
  DecodeDate(aValue, var lYear, var lMonth, var result);
end;

function HourOf(const aValue: TDateTime): Word;
begin
  var lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var result, var lMin, var lSec, var lMSec);
end;

function MinuteOf(const aValue: TDateTime): Word;
begin
  var lHour, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var result, var lSec, var lMSec);
end;

function SecondOf(const aValue: TDateTime): Word;
begin
  var lHour, lMin, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var result, var lMSec);
end;

function MilliSecondOf(const aValue: TDateTime): Word;
begin
  var lHour, lMin, lSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var result);
end;

function StartOfTheYear(const aValue: TDateTime): TDateTime;
begin
  result := StartOfAYear(YearOf(aValue));
end;

function EndOfTheYear(const aValue: TDateTime): TDateTime;
begin
  result := EndOfTheYear(YearOf(aValue));
end;

function StartOfAYear(const aYear: Word): TDateTime;
begin
  result := EncodeDate(aYear, 1, 1);
end;

function EndOfAYear(const aYear: Word): TDateTime;
begin
  TryEncodeDateTime(aYear, 12, 31, 23, 59, 59, 999, out result);
end;

function StartOfTheMonth(const aValue: TDateTime): TDateTime;
begin
  var lYear, lMonth, lDay: Word;
  DecodeDate(aValue, var lYear, var lMonth, var lDay);
  result := StartOfAMonth(lYear, lMonth);
end;

function EndOfTheMonth(const aValue: TDateTime): TDateTime;
begin
  var lYear, lMonth, lDay: Word;
  DecodeDate(aValue, var lYear, var lMonth, var lDay);
  result := EndOfAMonth(lYear, lMonth);
end;

function StartOfAMonth(const aYear, aMonth: Word): TDateTime;
begin
  result := EncodeDate(aYear, aMonth, 1);
end;

function EndOfAMonth(const aYear, aMonth: Word): TDateTime;
begin
  TryEncodeDateTime(aYear, aMonth, MonthDays[IsLeapYear(aYear)][aMonth - 1], 23, 59, 59, 999, out result);
end;

function StartOfTheWeek(const aValue: TDateTime): TDateTime;
begin
  // TODO
end;

function EndOfTheWeek(const aValue: TDateTime): TDateTime;
begin
  // TODO
end;

function StartOfAWeek(const aYear, aWeekOfYear: Word; const aDayOfWeek: Word := 1): TDateTime;
begin
  // TODO
end;

function EndOfAWeek(const aYear, aWeekOfYear: Word; const aDayOfWeek: Word := 7): TDateTime;
begin
  // TODO
end;

function StartOfTheDay(const aValue: TDateTime): TDateTime;
begin
  // TODO
end;

function EndOfTheDay(const aValue: TDateTime): TDateTime;
begin
  // TODO
end;

function StartOfADay(const aYear, aMonth, aDay: Word): TDateTime; 
begin
  TryEncodeDateTime(aYear, aMonth, aDay, 0, 0, 0, 0, out result);
end;

function EndOfADay(const aYear, aMonth, aDay: Word): TDateTime; 
begin
  TryEncodeDateTime(aYear, aMonth, aDay, 23, 59, 59, 999, out result);
end;

function StartOfADay(const aYear, aDayOfYear: Word): TDateTime; 
begin
  // TODO
end;

function EndOfADay(const aYear, aDayOfYear: Word): TDateTime; 
begin
  // TODO
end;

function MonthOfTheYear(const aValue: TDateTime): Word;
begin
  result := MonthOf(aValue);
end;

function WeekOfTheYear(const aValue: TDateTime): Word;
begin
  result := (DayOfTheYear(aValue) - DayOfWeek(aValue) + 10) div 7;
end;

function WeekOfTheYear(const aValue: TDateTime; var aYear: Word): Word; 
begin
  var lMonth, lDay, lDOW: Word;
  DecodeDateFully(aValue, var aYear, var lMonth, var lDay, var lDOW);
  result := WeekOfTheYear(aValue);
end;

function DayOfTheYear(const aValue: TDateTime): Word;
begin
  result := Integer(Math.Truncate(aValue) - Math.Truncate(StartOfTheYear(aValue))) + 1;
end;

function HourOfTheYear(const aValue: TDateTime): Word;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := (DayOfTheYear(aValue)  - 1) * HoursPerDay + lHour;
end;

function MinuteOfTheYear(const aValue: TDateTime): Cardinal;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := HourOfTheYear(aValue) * MinsPerHour + lMin;
end;

function SecondOfTheYear(const aValue: TDateTime): Cardinal;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := MinuteOfTheYear(aValue) * SecsPerMin + lSec;
end;

function MilliSecondOfTheYear(const aValue: TDateTime): Int64;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := SecondOfTheYear(aValue) * MSecsPerSec + lMSec;
end;

function WeekOfTheMonth(const aValue: TDateTime): Word;
begin
  // TODO  
end;

function WeekOfTheMonth(const aValue: TDateTime; var aYear, aMonth: Word): Word; 
begin
  // TODO
end;

function DayOfTheMonth(const aValue: TDateTime): Word; 
begin
  result := DayOf(aValue);
end;

function HourOfTheMonth(const aValue: TDateTime): Word;
begin
  result := (DayOfTheMonth(aValue) - 1) * HoursPerDay + HourOf(aValue);
end;

function MinuteOfTheMonth(const aValue: TDateTime): Word;
begin
  result := HourOfTheMonth(aValue) * MinsPerHour + MinuteOf(aValue);
end;

function SecondOfTheMonth(const aValue: TDateTime): Cardinal;
begin
  result := MinuteOfTheMonth(aValue) * SecsPerMin + SecondOf(aValue);
end;

function MilliSecondOfTheMonth(const aValue: TDateTime): Cardinal;
begin
  result := SecondOfTheMonth(aValue) * MSecsPerSec + MilliSecondOf(aValue);
end;

function DayOfTheWeek(const aValue: TDateTime): Word;
begin
  result := (Integer(Math.Truncate(aValue)) + DateDelta  - 1) mod 7 + 1;
end;

function HourOfTheWeek(const aValue: TDateTime): Word;
begin
  result := (DayOfTheWeek(aValue) - 1) * HoursPerDay + HourOf(aValue);
end;

function MinuteOfTheWeek(const aValue: TDateTime): Word;
begin
  result := HourOfTheWeek(aValue) * MinsPerHour + MinuteOf(aValue);
end;

function SecondOfTheWeek(const aValue: TDateTime): Cardinal;
begin
  result := MinuteOfTheWeek(aValue) * SecsPerMin + SecondOf(aValue);
end;

function MilliSecondOfTheWeek(const aValue: TDateTime): Cardinal;
begin
  result := SecondOfTheWeek(aValue) * MSecsPerSec + MilliSecondOf(aValue);
end;

function HourOfTheDay(const aValue: TDateTime): Word;
begin
  result := HourOf(aValue)
end;

function MinuteOfTheDay(const aValue: TDateTime): Word;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := lHour * MinsPerHour + lMin;
end;

function SecondOfTheDay(const aValue: TDateTime): Cardinal;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := (lHour * MinsPerHour + lMin) * SecsPerMin + lSec;
end;

function MilliSecondOfTheDay(const aValue: TDateTime): Cardinal;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := ((lHour * MinsPerHour + lMin) * SecsPerMin + lSec) * MSecsPerSec + lMSec;
end;

function MinuteOfTheHour(const aValue: TDateTime): Word;
begin
  result := MinuteOf(aValue);
end;

function SecondOfTheHour(const aValue: TDateTime): Word;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := lMin * SecsPerMin + lSec;
end;

function MilliSecondOfTheHour(const aValue: TDateTime): Cardinal;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := (lMin * SecsPerMin + lSec) * MSecsPerSec + lMSec;
end;

function SecondOfTheMinute(const aValue: TDateTime): Word;
begin
  result := SecondOf(aValue);
end;

function MilliSecondOfTheMinute(const aValue: TDateTime): Cardinal;
begin
  var lHour, lMin, lSec, lMSec: Word;
  DecodeTime(aValue, var lHour, var lMin, var lSec, var lMSec);
  result := lSec * MSecsPerSec + lMSec;
end;

function MilliSecondOfTheSecond(const aValue: TDateTime): Word;
begin
  result := MilliSecondOf(aValue);
end;

function WithinPastYears(const aNow, aThen: TDateTime; const aYears: Integer): Boolean;
begin
  result := YearsBetween(aNow, aThen) <= aYears;
end;

function WithinPastMonths(const aNow, aThen: TDateTime; const aMonths: Integer): Boolean;
begin
  result := MonthsBetween(aNow, aThen) <= aMonths;
end;

function WithinPastWeeks(const aNow, aThen: TDateTime; const aWeeks: Integer): Boolean;
begin
  result := WeeksBetween(aNow, aThen) <= aWeeks;
end;

function WithinPastDays(const aNow, aThen: TDateTime; const aDays: Integer): Boolean;
begin
  result := DaysBetween(aNow, aThen) <= aDays;
end;

function WithinPastHours(const aNow, aThen: TDateTime; const aHours: Int64): Boolean;
begin
  result := HoursBetween(aNow, aThen) <= aHours;
end;

function WithinPastMinutes(const aNow, aThen: TDateTime; const aMinutes: Int64): Boolean;
begin
  result := MinutesBetween(aNow, aThen) <= aMinutes;
end;

function WithinPastSeconds(const aNow, aThen: TDateTime; const aSeconds: Int64): Boolean;
begin
  result := SecondsBetween(aNow, aThen) <= aSeconds;
end;

function WithinPastMilliSeconds(const aNow, aThen: TDateTime; const aMilliSeconds: Int64): Boolean;
begin
  result := MilliSecondsBetween(aNow, aThen) <= aMilliSeconds;
end;

function YearsBetween(const aNow, aThen: TDateTime): Integer;
begin
  result := Integer(Math.Truncate(Math.Abs(Integer(Math.Truncate(aNow - aThen))) / ApproxDaysPerYear));
end;

function MonthsBetween(const aNow, aThen: TDateTime): Integer;
begin
  result := Integer(Math.Truncate(Math.Abs(Integer(Math.Truncate(aNow - aThen))) / ApproxDaysPerMonth));
end;

function WeeksBetween(const aNow, aThen: TDateTime): Integer;
begin
  result := Integer(Math.Truncate(Math.Abs(Integer(Math.Truncate(aNow - aThen))) / DaysPerWeek));
end;

function DaysBetween(const aNow, aThen: TDateTime): Integer;
begin
  result := Math.Abs(Integer(Math.Truncate(aNow - aThen)));
end;

function DateTimeToMilliseconds(const aDateTime: TDateTime): Int64;
begin
  var lTimeStamp := DateTimeToTimeStamp(aDateTime);
  result := lTimeStamp.Date * MSecsPerDay + lTimeStamp.Time;
end;

function HoursBetween(const aNow, aThen: TDateTime): Int64;
begin
  result := Math.Abs(DateTimeToMilliseconds(aNow) - DateTimeToMilliseconds(aThen)) div (MSecsPerSec * SecsPerMin * MinsPerHour);
end;

function MinutesBetween(const aNow, aThen: TDateTime): Int64;
begin
  result := Math.Abs(DateTimeToMilliseconds(aNow) - DateTimeToMilliseconds(aThen)) div (MSecsPerSec * SecsPerMin);
end;

function SecondsBetween(const aNow, aThen: TDateTime): Int64;
begin
  result := Math.Abs(DateTimeToMilliseconds(aNow) - DateTimeToMilliseconds(aThen)) div MSecsPerSec;
end;

function MilliSecondsBetween(const aNow, aThen: TDateTime): Int64;
begin
  Result := Math.Abs(DateTimeToMilliseconds(aNow) - DateTimeToMilliseconds(aThen));
end;

function IncDay(const aValue: TDateTime; const aNumberOfDays: Integer := 1): TDateTime;
begin
  var lDays := Integer(Math.Truncate(aValue));
  var lTime := aValue - lDays;
  lDays := lDays + aNumberOfDays;
  result := lDays + lTime;
end;

function EncodeDateDay(const aYear, aDayOfYear: Word): TDateTime;
begin
  result := StartOfAYear(aYear) + aDayOfYear - 1;
end;

procedure DecodeDateDay(const aValue: TDateTime; out aYear, aDayOfYear: Word);
begin
  aYear := YearOf(aValue);
  aDayOfYear := DayOfTheYear(aValue);
end;

end.
