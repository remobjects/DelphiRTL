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
  end;
end.
