namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

type
  DateUtilsUsage = public class(Test)    
  public
    method DateOfTests;
    begin
      var lDate := EncodeDateTime(2017, 5, 5, 17, 40, 45, 50);
      var lDateNew := DateOf(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDateNew, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 5);
      Assert.AreEqual(lDay, 5);
    end;

    method TimeOfTests;
    begin
      var lDate := EncodeDateTime(2017, 5, 5, 17, 40, 45, 50);
      var lTime := TimeOf(lDate);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lTime, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 17);
      Assert.AreEqual(lMin, 40);
      Assert.AreEqual(lSec, 45);
      Assert.AreEqual(lMSec, 50);
    end;

    method IsInLeapYearTests;
    begin
      var lDate := EncodeDate(2016, 5, 5);
      Assert.AreEqual(IsInLeapYear(lDate), true);
      lDate := EncodeDate(2017, 1, 5);
      Assert.AreEqual(IsInLeapYear(lDate), false);
    end;

    method IsPMTests;
    begin
      var lTime := EncodeTime(18, 0, 10, 100);
      Assert.AreEqual(IsPM(lTime), true);
      lTime := EncodeTime(9, 20, 10, 100);
      Assert.AreEqual(IsPM(lTime), false);
    end;

    method IsAMTests;
    begin
      var lTime := EncodeTime(18, 0, 10, 100);
      Assert.AreEqual(IsAM(lTime), false);
      lTime := EncodeTime(9, 20, 10, 100);
      Assert.AreEqual(IsAM(lTime), true);
    end;

    method IsValidDateTests;
    begin
      Assert.AreEqual(IsValidDate(3500, 10, 10), true);
      Assert.AreEqual(IsValidDate(4000, 10, 33), false);
    end;

    method IsValidTimeTests;
    begin
      Assert.AreEqual(IsValidTime(10, 25, 45, 900), true);
      Assert.AreEqual(IsValidTime(25, 25, 45, 900), false);
      Assert.AreEqual(IsValidTime(10, 61, 45, 900), false);
      Assert.AreEqual(IsValidTime(10, 25, 621, 900), false);
      Assert.AreEqual(IsValidTime(10, 25, 45, 9000), false);
      Assert.AreEqual(IsValidTime(0, 0, 45, 900), true);
    end;

    method IsValidDateTimeTests;
    begin
      Assert.AreEqual(IsValidDateTime(2017, 12, 12, 17, 23, 59, 599), true);
      Assert.AreEqual(IsValidDateTime(22017, 12, 12, 17, 23, 59, 599), false);
      Assert.AreEqual(IsValidDateTime(2017, 12, 12, 25, 17, 59, 599), false);
      Assert.AreEqual(IsValidDateTime(2017, 12, 12, 17, 63, 59, 1599), false);
      Assert.AreEqual(IsValidDateTime(2017, 12, 12, 170, 23, 59, 599), false);
      Assert.AreEqual(IsValidDateTime(2017, 12, 32, 17, 23, 59, 599), false);
    end;

    method IsValidDateDayTests;
    begin
      Assert.AreEqual(IsValidDateDay(2016, 366), true);
      Assert.AreEqual(IsValidDateDay(2015, 366), false);      
      Assert.AreEqual(IsValidDateDay(2016, 367), false);
    end;

    method IsValidDateWeekTests;
    begin
      Assert.AreEqual(IsValidDateWeek(2016, 52, 1), true);
      Assert.AreEqual(IsValidDateWeek(2016, 55, 3), false);
    end;

    method IsValidDateMonthWeekTests;
    begin
      Assert.AreEqual(IsValidDateMonthWeek(2016, 4, 3, 1), true);
      Assert.AreEqual(IsValidDateMonthWeek(2016, 2, 5, 4), true);
      Assert.AreEqual(IsValidDateMonthWeek(2016, 2, 5, 1), true);
      Assert.AreEqual(IsValidDateMonthWeek(2016, 2, 6, 1), false);
      Assert.AreEqual(IsValidDateMonthWeek(2016, 2, 5, 0), false);
      Assert.AreEqual(IsValidDateMonthWeek(2016, 2, 5, 8), false);
      Assert.AreEqual(IsValidDateMonthWeek(2016, 13, 5, 1), false);
    end;

    method WeeksInYearTests;
    begin
      Assert.AreEqual(WeeksInAYear(2015), 53);
      Assert.AreEqual(WeeksInAYear(2016), 52);
      Assert.AreEqual(WeeksInAYear(1980), 52);
      Assert.AreEqual(WeeksInAYear(1925), 53);
    end;

    method WeeksInAYearTests;
    begin
      var lDate := EncodeDate(2015, 1, 1);
      Assert.AreEqual(WeeksInYear(lDate), 53);
      lDate := EncodeDate(2016, 1, 1);
      Assert.AreEqual(WeeksInYear(lDate), 52);
      lDate := EncodeDate(1980, 1, 1);
      Assert.AreEqual(WeeksInYear(lDate), 52);
      lDate := EncodeDate(1925, 1, 1);
      Assert.AreEqual(WeeksInYear(lDate), 53);
    end;

    method DaysInYearTests;
    begin
      var lDate := EncodeDate(2016, 1, 1);
      Assert.AreEqual(DaysInYear(lDate), 366);

      lDate := EncodeDate(2015, 1, 1);
      Assert.AreEqual(DaysInYear(lDate), 365);
    end;

    method DaysInAYearTests;
    begin
      Assert.AreEqual(DaysInAYear(2016), 366);
      Assert.AreEqual(DaysInAYear(2100), 365);
      Assert.AreEqual(DaysInAYear(2400), 366);
    end;

    method DaysInMonthTests;
    begin
      var lDate := EncodeDate(2400, 2, 1);
      Assert.AreEqual(DaysInMonth(lDate), 29);

      lDate := EncodeDate(2400, 1, 1);
      Assert.AreEqual(DaysInMonth(lDate), 31);

      lDate := EncodeDate(2015, 2, 1);
      Assert.AreEqual(DaysInMonth(lDate), 28);

      lDate := EncodeDate(2015, 12, 1);
      Assert.AreEqual(DaysInMonth(lDate), 31);
    end;

    method DaysInAMonthTests;
    begin
      Assert.AreEqual(DaysInAMonth(2400, 2), 29);
      Assert.AreEqual(DaysInAMonth(2400, 1), 31);
      Assert.AreEqual(DaysInAMonth(2015, 2), 28);
      Assert.AreEqual(DaysInAMonth(2015, 12), 31);      
    end;

    method TodayTests;
    begin
      Assert.AreEqual(DateOf(Now), Today);
    end;

    method YesterdayTests;
    begin
      Assert.AreEqual(DateOf(IncDay(Now, -1)), Yesterday);
    end;

    method TomorrowTests;
    begin
      Assert.AreEqual(DateOf(IncDay(Now, 1)), Tomorrow);
    end;

    method IsTodayTests;
    begin
      Assert.IsTrue(IsToday(Now));
    end;

    method IsSameDayTests;
    begin
      var lDate1 := EncodeDateTime(2017, 2, 2, 17, 23, 21, 456);
      var lDate2 := EncodeDateTime(2017, 2, 2, 0, 2, 2, 56);
      Assert.IsTrue(IsSameDay(lDate1, lDate2));
      lDate2 := EncodeDateTime(2017, 1, 2, 0, 2, 2, 56);
      Assert.IsFalse(IsSameDay(lDate1, lDate2));
    end;

    method YearOfTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      Assert.AreEqual(YearOf(lDate), 2017);
      lDate := EncodeDate(1900, 12, 31);
      Assert.AreEqual(YearOf(lDate), 1900);
    end;

    method MonthOfTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      Assert.AreEqual(MonthOf(lDate), 1);
      lDate := EncodeDate(3040, 6, 1);
      Assert.AreEqual(MonthOf(lDate), 6);
    end;

    method WeekOfTests(const aValue: TDateTime);
    begin
      var lDate := EncodeDate(2017, 1, 1);
      Assert.AreEqual(WeekOf(lDate), 1);
      lDate := EncodeDate(2017, 12, 31);
      Assert.AreEqual(WeekOf(lDate), 52);
    end;

    method DayOfTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      Assert.AreEqual(DayOf(lDate), 1);
      lDate := EncodeDate(2017, 12, 31);
      Assert.AreEqual(DayOf(lDate), 31);
    end;

    method HourOfTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 10, 51, 47, 100);
      Assert.AreEqual(HourOf(lDate), 10);
      lDate := EncodeDateTime(2017, 1, 1, 0, 51, 47, 100);
      Assert.AreEqual(HourOf(lDate), 0);
    end;

    method MinuteOfTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 10, 51, 47, 100);
      Assert.AreEqual(MinuteOf(lDate), 51);
      lDate := EncodeDateTime(2017, 1, 1, 0, 0, 47, 100);
      Assert.AreEqual(MinuteOf(lDate), 0);
    end;

    method SecondOfTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 10, 51, 47, 100);
      Assert.AreEqual(SecondOf(lDate), 47);
      lDate := EncodeDateTime(2017, 1, 1, 0, 51, 1, 100);
      Assert.AreEqual(SecondOf(lDate), 1);
    end;

    method MilliSecondOfTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 10, 51, 47, 100);
      Assert.AreEqual(MilliSecondOf(lDate), 100);
      lDate := EncodeDateTime(2017, 1, 1, 0, 51, 47, 999);
      Assert.AreEqual(MilliSecondOf(lDate), 999);
    end;

    method StartOfTheYearTests;
    begin
      var lDate := EncodeDate(2017, 12, 31);
      var lNewDate := StartOfTheYear(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 1);
    end;

    method EndOfTheYearTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      var lNewDate := EndOfTheYear(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 12);
      Assert.AreEqual(lDay, 31);
    end;

    method StartOfAYearTests;
    begin
      var lDate := StartOfAYear(3000);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 3000);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 1);
    end;

    method EndOfAYearTests;
    begin
      var lNewDate := EndOfAYear(2000);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2000);
      Assert.AreEqual(lMonth, 12);
      Assert.AreEqual(lDay, 31);
    end;

    method StartOfTheMonthTests;
    begin
      var lDate := EncodeDate(2017, 3, 25);
      var lNewDate := StartOfTheMonth(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 1);
    end;

    method EndOfTheMonthTests;
    begin
      var lDate := EncodeDate(2017, 3, 25);
      var lNewDate := EndOfTheMonth(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 31);
    end;

    method StartOfAMonthTests;
    begin
      var lNewDate := StartOfAMonth(2017, 3);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 1);
    end;

    method EndOfAMonthTests;
    begin
      var lNewDate := EndOfAMonth(2017, 3);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 31);
    end;

    method StartOfTheWeekTests;
    begin
      var lDate := EncodeDate(2017, 3, 25);
      var lNewDate := StartOfTheWeek(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 20);
    end;

    method EndOfTheWeekTests;
    begin
      var lDate := EncodeDate(2017, 3, 25);
      var lNewDate := EndOfTheWeek(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lNewDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 26);
    end;

    method StartOfTheDayTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 20, 9, 56, 10, 400);
      var lDateNew := StartOfTheDay(lDate);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDateNew, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 20);      
      Assert.AreEqual(lHour, 0);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
    end;

    method EndOfTheDayTests;
    begin
      var lDate := EncodeDateTime(1917, 11, 21, 9, 56, 10, 400);
      var lDateNew := EndOfTheDay(lDate);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDateNew, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      Assert.AreEqual(lYear, 1917);
      Assert.AreEqual(lMonth, 11);
      Assert.AreEqual(lDay, 21);      
      Assert.AreEqual(lHour, 23);
      Assert.AreEqual(lMin, 59);
      Assert.AreEqual(lSec, 59);
      Assert.AreEqual(lMSec, 999);
    end;

    method StartOfADayTests;
    begin
      var lDateNew := StartOfADay(2017, 17);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDateNew, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 17);      
      Assert.AreEqual(lHour, 0);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
    end;

    method EndOfADayTests;
    begin
      var lDateNew := EndOfADay(2000, 10);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDateNew, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      Assert.AreEqual(lYear, 2000);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 10);      
      Assert.AreEqual(lHour, 23);
      Assert.AreEqual(lMin, 59);
      Assert.AreEqual(lSec, 59);
      Assert.AreEqual(lMSec, 999);
    end;

    method StartOfADayTests2;
    begin
      var lDateNew := StartOfADay(3400, 12, 31);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDateNew, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      Assert.AreEqual(lYear, 3400);
      Assert.AreEqual(lMonth, 12);
      Assert.AreEqual(lDay, 31);      
      Assert.AreEqual(lHour, 0);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
    end;

    method EndOfADayTests2;
    begin
      var lDateNew := EndOfADay(4599, 10, 1);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDateNew, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      Assert.AreEqual(lYear, 4599);
      Assert.AreEqual(lMonth, 10);
      Assert.AreEqual(lDay, 1);      
      Assert.AreEqual(lHour, 23);
      Assert.AreEqual(lMin, 59);
      Assert.AreEqual(lSec, 59);
      Assert.AreEqual(lMSec, 999);
    end;

    method MonthOfTheYearTests;
    begin
      var lDate := EncodeDate(3800, 6, 12);
      var lMonth := MonthOfTheYear(lDate);
      Assert.AreEqual(lMonth, 6);

      lDate := EncodeDate(3800, 1, 12);
      lMonth := MonthOfTheYear(lDate);
      Assert.AreEqual(lMonth, 1);
    end;

    method WeekOfTheYearTests;
    begin
      var lDate := EncodeDate(2016, 12, 31);
      var lWeek := WeekOfTheYear(lDate);
      Assert.AreEqual(lWeek, 52);

      lDate := EncodeDate(2016, 1, 6);
      lWeek := WeekOfTheYear(lDate);
      Assert.AreEqual(lWeek, 1);
    end;

    method WeekOfTheYearTests2;
    begin
      var lDate := EncodeDate(2016, 12, 31);
      var lYear: Word;
      var lWeek := WeekOfTheYear(lDate, var lYear);
      Assert.AreEqual(lWeek, 52);
      Assert.AreEqual(lYear, 2016);

      lDate := EncodeDate(2016, 1, 6);
      lWeek := WeekOfTheYear(lDate, var lYear);
      Assert.AreEqual(lWeek, 1);   
      Assert.AreEqual(lYear, 2016);
    end;

    method DayOfTheYearTests;
    begin
      var lDate := EncodeDate(2017, 12, 31);
      var lDay := DayOfTheYear(lDate);
      Assert.AreEqual(lDay, 365);

      lDate := EncodeDate(2016, 12, 31);
      lDay := DayOfTheYear(lDate);
      Assert.AreEqual(lDay, 366);
    end;

    method HourOfTheYearTests;
    begin
      var lDate := EncodeDate(2017, 6, 6);
      var lHour := HourOfTheYear(lDate);
      Assert.AreEqual(lHour, 3744);

      lDate := EncodeDateTime(2017, 10, 1, 16, 41, 2, 100);
      lHour := HourOfTheYear(lDate);
      Assert.AreEqual(lHour, 6568);
    end;

    method MinuteOfTheYearTests;
    begin
      var lDate := EncodeDateTime(2017, 10, 1, 16, 41, 2, 100);
      var lMin := MinuteOfTheYear(lDate);
      Assert.AreEqual(lMin, 394121);

      lDate := EncodeDateTime(2017, 2, 1, 3, 41, 2, 100);
      lMin := MinuteOfTheYear(lDate);
      Assert.AreEqual(lMin, 44861);
    end;

    method SecondOfTheYearTests;
    begin
      var lDate := EncodeDateTime(2000, 2, 14, 6, 54, 3, 100);
      var lSec := SecondOfTheYear(lDate);
      Assert.AreEqual(lSec, 3826443);

      lDate := EncodeDateTime(3544, 11, 21, 17, 12, 3, 900);
      lSec := SecondOfTheYear(lDate);
      Assert.AreEqual(lSec, 28141923);
    end;

    method MilliSecondOfTheYearTests;
    begin
      var lDate := EncodeDateTime(3544, 11, 21, 17, 12, 3, 900);
      var lMSec := MilliSecondOfTheYear(lDate);
      Assert.AreEqual(lMSec, 28141923900);
    end;

    method DayOfTheMonthTests;
    begin
      var lDate := EncodeDateTime(3544, 11, 21, 17, 12, 3, 900);
      var lDay := DayOfTheMonth(lDate);
      Assert.AreEqual(lDay, 21);
    end;

    method HourOfTheMonthTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lHour := HourOfTheMonth(lDate);
      Assert.AreEqual(lHour, 497);

      lDate := EncodeDate(2017, 02, 10);
      lHour := HourOfTheMonth(lDate);
      Assert.AreEqual(lHour, 216);
    end;

    method MinuteOfTheMonthTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lMin := MinuteOfTheMonth(lDate);
      Assert.AreEqual(lMin, 29832);

      lDate := EncodeDate(2017, 02, 10);
      lMin := MinuteOfTheMonth(lDate);
      Assert.AreEqual(lMin, 12960);
    end;

    method SecondOfTheMonthTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lSec := SecondOfTheMonth(lDate);
      Assert.AreEqual(lSec, 1789923);

      lDate := EncodeDate(2017, 02, 10);
      lSec := SecondOfTheMonth(lDate);
      Assert.AreEqual(lSec, 777600);
    end;

    method MilliSecondOfTheMonthTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lSec := MilliSecondOfTheMonth(lDate);
      Assert.AreEqual(lSec, 1789923900);

      lDate := EncodeDate(2017, 02, 10);
      lSec := MilliSecondOfTheMonth(lDate);
      Assert.AreEqual(lSec, 777600000);
    end;

    method DayOfTheWeekTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lDay := DayOfTheWeek(lDate);
      Assert.AreEqual(lDay, 2);

      lDate := EncodeDate(2017, 02, 10);
      lDay := DayOfTheWeek(lDate);
      Assert.AreEqual(lDay, 5);
    end;

    method HourOfTheWeekTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lHour := HourOfTheWeek(lDate);
      Assert.AreEqual(lHour, 41);

      lDate := EncodeDate(2017, 02, 10);
      lHour := HourOfTheWeek(lDate);
      Assert.AreEqual(lHour, 96);
    end;

    method MinuteOfTheWeekTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lMin := MinuteOfTheWeek(lDate);
      Assert.AreEqual(lMin, 2472);

      lDate := EncodeDate(2017, 02, 10);
      lMin := MinuteOfTheWeek(lDate);
      Assert.AreEqual(lMin, 5760);
    end;

    method SecondOfTheWeekTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lSec := SecondOfTheWeek(lDate);
      Assert.AreEqual(lSec, 148323);

      lDate := EncodeDate(2017, 02, 10);
      lSec := SecondOfTheWeek(lDate);
      Assert.AreEqual(lSec, 345600);
    end;

    method MilliSecondOfTheWeekTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lMSec := MilliSecondOfTheWeek(lDate);
      Assert.AreEqual(lMSec, 148323900);

      lDate := EncodeDate(2017, 02, 10);
      lMSec := MilliSecondOfTheWeek(lDate);
      Assert.AreEqual(lMSec, 345600000);
    end;

    method HourOfTheDayTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 17, 12, 3, 900);
      var lHour := HourOf(lDate);
      Assert.AreEqual(lHour, 17);

      lDate := EncodeDateTime(2017, 03, 21, 1, 12, 3, 900);
      lHour := HourOf(lDate);
      Assert.AreEqual(lHour, 1);
    end;

    method MinuteOfTheDayTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := MinuteOfTheDay(lDate);
      Assert.AreEqual(lData, 1265);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := MinuteOfTheDay(lDate);
      Assert.AreEqual(lData, 144);
    end;

    method SecondOfTheDayTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := SecondOfTheDay(lDate);
      Assert.AreEqual(lData, 75903);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := SecondOfTheDay(lDate);
      Assert.AreEqual(lData, 8650);
    end;

    method MilliSecondOfTheDayTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := MilliSecondOfTheDay(lDate);
      Assert.AreEqual(lData, 75903900);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := MilliSecondOfTheDay(lDate);
      Assert.AreEqual(lData, 8650015);
    end;
    
    method MinuteOfTheHourTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := MinuteOfTheHour(lDate);
      Assert.AreEqual(lData, 5);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := MinuteOfTheHour(lDate);
      Assert.AreEqual(lData, 24);
    end;
    
    method SecondOfTheHourTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := SecondOfTheHour(lDate);
      Assert.AreEqual(lData, 303);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := SecondOfTheHour(lDate);
      Assert.AreEqual(lData, 1450);
    end;

    method MilliSecondOfTheHourTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := MilliSecondOfTheHour(lDate);
      Assert.AreEqual(lData, 303900);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := MilliSecondOfTheHour(lDate);
      Assert.AreEqual(lData, 1450015);
    end;

    method SecondOfTheMinuteTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := SecondOfTheMinute(lDate);
      Assert.AreEqual(lData, 3);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := SecondOfTheMinute(lDate);
      Assert.AreEqual(lData, 10);
    end;

    method MilliSecondOfTheMinuteTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := MilliSecondOfTheMinute(lDate);
      Assert.AreEqual(lData, 3900);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := MilliSecondOfTheMinute(lDate);
      Assert.AreEqual(lData, 10015);
    end;

    method MilliSecondOfTheSecondTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lData := MilliSecondOfTheSecond(lDate);
      Assert.AreEqual(lData, 900);

      lDate := EncodeDateTime(2017, 02, 10, 2, 24, 10, 15);
      lData := MilliSecondOfTheSecond(lDate);
      Assert.AreEqual(lData, 15);
    end;

    method WithinPastYearsTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2019, 05, 1, 1, 7, 3, 900);

      Assert.AreEqual(WithinPastYears(lDate, lDate2, 2), true);
      Assert.AreEqual(WithinPastYears(lDate, lDate2, 1), false);
    end;

    method WithinPastMonthsTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2017, 06, 1, 1, 7, 3, 900);

      Assert.AreEqual(WithinPastMonths(lDate, lDate2, 1), false);
      Assert.AreEqual(WithinPastMonths(lDate, lDate2, 2), true);
    end;

    method WithinPastWeeksTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2017, 03, 1, 1, 7, 3, 900);

      Assert.AreEqual(WithinPastWeeks(lDate, lDate2, 3), true);
      Assert.AreEqual(WithinPastWeeks(lDate, lDate2, 1), false);
    end;

    method WithinPastDaysTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2017, 03, 1, 1, 7, 3, 900);

      Assert.AreEqual(WithinPastDays(lDate, lDate2, 20), true);
      Assert.AreEqual(WithinPastDays(lDate, lDate2, 10), false);
    end;

    method WithinPastHoursTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2017, 03, 21, 7, 7, 3, 900);

      Assert.AreEqual(WithinPastHours(lDate, lDate2, 22), true);
      Assert.AreEqual(WithinPastHours(lDate, lDate2, 5), false);
    end;

    method WithinPastMinutesTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2017, 03, 21, 21, 17, 3, 900);

      Assert.AreEqual(WithinPastMinutes(lDate, lDate2, 20), true);
      Assert.AreEqual(WithinPastMinutes(lDate, lDate2, 5), false);
    end;

    method WithinPastSecondsTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2017, 03, 21, 21, 6, 13, 900);

      Assert.AreEqual(WithinPastSeconds(lDate, lDate2, 80), true);
      Assert.AreEqual(WithinPastSeconds(lDate, lDate2, 60), false);
    end;

    method WithinPastMilliSecondsTests;
    begin
      var lDate := EncodeDateTime(2017, 03, 21, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2017, 03, 21, 21, 6, 13, 900);

      Assert.AreEqual(WithinPastMilliSeconds(lDate, lDate2, 80000), true);
      Assert.AreEqual(WithinPastMilliSeconds(lDate, lDate2, 60000), false);
    end;

    method DateTimeInRangeTests;
    begin
      var lDate := EncodeDateTime(2017, 12, 31, 21, 5, 3, 900);
      var lStart := EncodeDate(2017, 12, 30);
      var lEnd := EncodeDate(2018, 1, 1);
      var lEnd2 := EncodeDateTime(2017, 12, 31, 20, 59, 3, 199);

      Assert.AreEqual(DateTimeInRange(lDate, lStart, lEnd), true);
      Assert.AreEqual(DateTimeInRange(lDate, lStart, lEnd2), false);
    end;

    method DateInRangeTests;
    begin
      var lDate := EncodeDateTime(2017, 12, 31, 21, 5, 3, 900);
      var lStart := EncodeDate(2017, 12, 30);
      var lEnd := EncodeDate(2018, 1, 1);
      var lEnd2 := EncodeDateTime(2017, 12, 31, 20, 59, 3, 199);
      var lEnd3 := EncodeDateTime(2017, 12, 30, 20, 59, 3, 199);

      Assert.AreEqual(DateInRange(lDate, lStart, lEnd), true);
      Assert.AreEqual(DateInRange(lDate, lStart, lEnd2), true);
      Assert.AreEqual(DateInRange(lDate, lStart, lEnd3, false), false);
    end;

    method TimeInRangeTests;
    begin
      var lDate := EncodeDateTime(2017, 12, 31, 21, 5, 3, 900);
      var lStart := EncodeDateTime(2017, 12, 30, 14, 20, 3, 100);
      var lEnd := EncodeDateTime(2018, 1, 1, 22, 32, 23, 1);
      var lEnd2 := EncodeDateTime(2017, 12, 31, 20, 59, 3, 199);

      Assert.AreEqual(TimeInRange(lDate, lStart, lEnd), true);
      Assert.AreEqual(TimeInRange(lDate, lStart, lEnd2), false);
    end;

    method YearSpanTests;
    begin
      var lDate := EncodeDateTime(2017, 12, 31, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2019, 12, 31, 21, 5, 3, 900);
      var lDate3 := EncodeDateTime(2020, 2, 20, 21, 5, 3, 900);

      Assert.AreEqual(YearSpan(lDate, lDate2) >= 2, false);
      Assert.AreEqual(YearSpan(lDate, lDate3) >= 2, true);
    end;

    method MonthSpanTests;
    begin
      var lDate := EncodeDateTime(2017, 12, 31, 21, 5, 3, 900);
      var lDate2 := EncodeDateTime(2019, 12, 31, 21, 5, 3, 900);
      var lDate3 := EncodeDateTime(2017, 11, 12, 21, 5, 3, 900);

      Assert.AreEqual(MonthSpan(lDate, lDate2) >= 23, true);
      Assert.AreEqual(MonthSpan(lDate, lDate3) >= 2, false);
    end;

    method WeekSpanTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      var lDate2 := EncodeDate(2017, 1, 8);
      var lDate3 := EncodeDate(2017, 2, 1);

      Assert.AreEqual(WeekSpan(lDate, lDate2), 1);
      Assert.AreEqual(WeekSpan(lDate, lDate3) >= 4, true);
    end;

    method DaySpanTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      var lDate2 := EncodeDate(2017, 1, 8);
      var lDate3 := EncodeDate(2017, 1, 21);

      Assert.AreEqual(DaySpan(lDate, lDate2), 7);
      Assert.AreEqual(DaySpan(lDate, lDate3), 20);
    end;

    method HourSpanTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      var lDate2 := EncodeDateTime(2017, 1, 1, 2, 0, 0, 100);
      var lDate3 := EncodeDateTime(2017, 1, 2, 1, 0, 0, 1);
      var lDate4 := EncodeDate(2020, 1, 1);
      var lDate5 := EncodeDate(2019, 12, 31);

      Assert.AreEqual(HourSpan(lDate, lDate2) >= 1, true);
      Assert.AreEqual(HourSpan(lDate, lDate3) >= 24, true);
      Assert.AreEqual(HourSpan(lDate4, lDate5) >= 24, true);
    end;

    method MinuteSpanTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      var lDate2 := EncodeDateTime(2017, 1, 1, 1, 10, 0, 100);
      var lDate3 := EncodeDateTime(2017, 1, 1, 2, 0, 0, 10);

      Assert.AreEqual(MinuteSpan(lDate, lDate2) >= 10, true);
      Assert.AreEqual(MinuteSpan(lDate, lDate3) >= 60, true);
    end;

    method SecondSpanTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      var lDate2 := EncodeDateTime(2017, 1, 2, 1, 0, 0, 100);
      var lDate3 := EncodeDateTime(2018, 1, 1, 1, 0, 0, 10);

      Assert.AreEqual(SecondSpan(lDate, lDate2) >= 86400, true);
      Assert.AreEqual(SecondSpan(lDate, lDate3) >= 86400 * 365, true);
    end;

    method MilliSecondSpanTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      var lDate2 := EncodeDateTime(2017, 1, 2, 1, 0, 0, 100);
      var lDate3 := EncodeDateTime(2018, 1, 1, 1, 0, 0, 10);

      Assert.AreEqual(MilliSecondSpan(lDate, lDate2) >= 86400 * 1000, true);
      Assert.AreEqual(MilliSecondSpan(lDate, lDate3) >= 86400 * 365 * 1000, true);
    end;

    method IncYearTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := IncYear(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2018);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 1);

      lDate := IncYear(lDate, 2);
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2020);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 1);
    end;

    method IncWeekTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := IncWeek(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 8);

      lDate := IncWeek(lDate, 2);
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 22);
    end;

    method IncDayTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := IncDay(lDate);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 2);

      lDate := IncDay(lDate, 10);
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 12);
    end;

    method IncHourTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := IncHour(lDate);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 2);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);

      lDate := IncHour(lDate, 10);
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 12);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
    end;

    method IncMinuteTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := IncMinute(lDate);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 1);
      Assert.AreEqual(lMin, 1);
      Assert.AreEqual(lSec, 0);

      lDate := IncMinute(lDate, 10);
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 1);
      Assert.AreEqual(lMin, 11);
      Assert.AreEqual(lSec, 0);
    end;

    method IncSecondTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := IncSecond(lDate);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 1);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 1);

      lDate := IncSecond(lDate, 10);
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 1);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 11);
    end;

    method IncMilliSecondTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := IncMilliSecond(lDate);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 1);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
      Assert.AreEqual(lMSec, 2);

      lDate := IncMilliSecond(lDate, 10);
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 1);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
      Assert.AreEqual(lMSec, 12);
    end;

    method EncodeDateDayTests;
    begin
      var lDate := EncodeDateDay(2017, 10);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 10);

      lDate := EncodeDateDay(2017, 243);
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 8);
      Assert.AreEqual(lDay, 31);
    end;

    method DecodeDateDayTests;
    begin
      var lDate := EncodeDateDay(2017, 243);
      var lYear, lDay: Word;
      DecodeDateDay(lDate, out lYear, out lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lDay, 243);
    end;

    method RecodeYearTests;
    begin
      var lDate := EncodeDate(2017, 3, 3);
      lDate := RecodeYear(lDate, 2020);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2020);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 3);
    end;

    method RecodeMonthTests;
    begin
      var lDate := EncodeDate(2017, 3, 3);
      lDate := RecodeMonth(lDate, 12);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 12);
      Assert.AreEqual(lDay, 3);
    end;

    method RecodeDayTests;
    begin
      var lDate := EncodeDate(2017, 3, 3);
      lDate := RecodeDay(lDate, 28);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lDay, 28);
    end;

    method RecodeHourTests;
    begin
      var lDate := EncodeTime(12, 34, 56, 543);
      lDate := RecodeHour(lDate, 22);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 22);
      Assert.AreEqual(lMin, 34);
      Assert.AreEqual(lSec, 56);
    end;

    method RecodeMinuteTests;
    begin
      var lDate := EncodeTime(12, 34, 56, 543);
      lDate := RecodeMinute(lDate, 50);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 12);
      Assert.AreEqual(lMin, 50);
      Assert.AreEqual(lSec, 56);
    end;

    method RecodeSecondTests;
    begin
      var lDate := EncodeTime(12, 34, 56, 543);
      lDate := RecodeSecond(lDate, 10);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 12);
      Assert.AreEqual(lMin, 34);
      Assert.AreEqual(lSec, 10);
    end;

    method RecodeMilliSecondTests;
    begin
      var lDate := EncodeTime(12, 34, 56, 543);
      lDate := RecodeMilliSecond(lDate, 999);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 12);
      Assert.AreEqual(lMin, 34);
      Assert.AreEqual(lSec, 56);
      Assert.AreEqual(lMSec, 999);
    end;

    method RecodeDateTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := RecodeDate(lDate, 2022, 2, 2);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2022);
      Assert.AreEqual(lMonth, 2);
      Assert.AreEqual(lDay, 2);
    end;

    method RecodeTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := RecodeTime(lDate, 17, 45, 31, 562);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 17);
      Assert.AreEqual(lMin, 45);
      Assert.AreEqual(lSec, 31);
      Assert.AreEqual(lMSec, 562);
    end;

    method RecodeDateTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      lDate := RecodeDateTime(lDate, 2020, 6, 5, 17, 45, 31, 562);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDate, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      
      Assert.AreEqual(lYear, 2020);
      Assert.AreEqual(lMonth, 6);
      Assert.AreEqual(lDay, 5);      
      Assert.AreEqual(lHour, 17);
      Assert.AreEqual(lMin, 45);
      Assert.AreEqual(lSec, 31);
      Assert.AreEqual(lMSec, 562);
    end;

    method TryRecodeDateTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      Assert.AreEqual(TryRecodeDateTime(lDate, 2020, 6, 5, 17, 45, 31, 562, out lDate), true);
      Assert.AreEqual(TryRecodeDateTime(lDate, 2020, 6, 35, 17, 45, 31, 562, out lDate), false);
    end;

    method CompareDateTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      var lDate2 := EncodeDateTime(2017, 6, 1, 1, 0, 0, 1);
      Assert.AreEqual(CompareDateTime(lDate, lDate2), LessThanValue);

      var lDate3 := EncodeDateTime(2016, 6, 1, 1, 0, 0, 1);
      Assert.AreEqual(CompareDateTime(lDate, lDate3), GreaterThanValue);
      Assert.AreEqual(CompareDateTime(lDate, lDate), EqualsValue);
    end;

    method SameDateTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);
      var lDate2 := EncodeDateTime(2017, 6, 1, 1, 0, 0, 1);
      var lDate3 := EncodeDateTime(2017, 1, 1, 1, 0, 0, 1);

      Assert.AreEqual(SameDateTime(lDate, lDate2), false);
      Assert.AreEqual(SameDateTime(lDate, lDate3), true);
    end;

    method CompareDateTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 2, 0, 1);
      var lDate2 := EncodeDateTime(2017, 6, 1, 1, 0, 0, 1);
      var lDate3 := EncodeDateTime(2016, 1, 1, 1, 0, 0, 1);

      Assert.AreEqual(CompareDate(lDate, lDate2), LessThanValue);
      Assert.AreEqual(CompareDate(lDate, lDate3), GreaterThanValue);
    end;

    method SameDateTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 2, 0, 1);
      var lDate2 := EncodeDateTime(2017, 6, 1, 1, 0, 0, 1);
      var lDate3 := EncodeDateTime(2017, 1, 1, 1, 23, 0, 1);

      Assert.AreEqual(SameDate(lDate, lDate2), false);
      Assert.AreEqual(SameDate(lDate, lDate3), true);
    end;

    method CompareTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 2, 0, 1);
      var lDate2 := EncodeDateTime(2017, 6, 1, 1, 0, 0, 1);
      var lDate3 := EncodeDateTime(2017, 4, 1, 1, 2, 0, 1);
      var lDate4 := EncodeDateTime(2017, 1, 1, 1, 3, 0, 1);

      Assert.AreEqual(CompareTime(lDate, lDate2), GreaterThanValue);
      Assert.AreEqual(CompareTime(lDate, lDate3), EqualsValue);
      Assert.AreEqual(CompareTime(lDate, lDate4), LessThanValue);
    end;

    method SameTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 2, 0, 1);
      var lDate2 := EncodeDateTime(2017, 6, 1, 1, 0, 0, 1);
      var lDate3 := EncodeDateTime(2017, 4, 1, 1, 2, 0, 1);
      var lDate4 := EncodeDateTime(2017, 1, 1, 1, 3, 0, 1);

      Assert.AreEqual(SameTime(lDate, lDate2), false);
      Assert.AreEqual(SameTime(lDate, lDate3), true);
      Assert.AreEqual(SameTime(lDate, lDate4), false);
    end;

    method NthDayOfWeekTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 1, 2, 0, 1);
      Assert.AreEqual(NthDayOfWeek(lDate), 1);

      lDate := EncodeDateTime(2017, 3, 1, 1, 2, 0, 1);
      Assert.AreEqual(NthDayOfWeek(lDate), 1);
    end;

    method DecodeDayOfWeekInMonthTests;
    begin
      var lDate := EncodeDateTime(2017, 3, 1, 1, 2, 0, 1);
      var lYear, lMonth, lNthDayOfWeek, lDayOfWeek: Word;
      DecodeDayOfWeekInMonth(lDate, out lYear, out lMonth, out lNthDayOfWeek, out lDayOfWeek);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 3);
      Assert.AreEqual(lNthDayOfWeek, 1);
      Assert.AreEqual(lDayOfWeek, 3);
    end;

  end;
end.
