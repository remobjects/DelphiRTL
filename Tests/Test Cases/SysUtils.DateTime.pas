namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

type
  SysUtilsDateTimeUsage = public class(Test)
  public
    method IsLeapYearTests;
    begin
      Assert.IsTrue(IsLeapYear(2016));
      Assert.IsFalse(IsLeapYear(2015));
      Assert.IsFalse(IsLeapYear(1700));
      Assert.IsTrue(IsLeapYear(2004));
    end;

    method DateTimeToTimeStampTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 18, 31, 30, 100);
      var lTemp: TTimeStamp := DateTimeToTimeStamp(lDate);
      Assert.AreEqual(lTemp.Time, 66690100);
      Assert.AreEqual(lTemp.Date, 736330);
    end;

    method TimeStampToDateTimeTests;
    begin
      var lTemp: TTimeStamp;
      lTemp.Time := 66690100;
      lTemp.Date := 736330;
      var lDate := TimeStampToDateTime(lTemp);
      var lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Word;
      DecodeDateTime(lDate, out lYear, out lMonth, out lDay, out lHour, out lMin, out lSec, out lMSec);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 1);
      Assert.AreEqual(lHour, 18);
      Assert.AreEqual(lMin, 31);
      Assert.AreEqual(lSec, 30);
      Assert.AreEqual(lMSec, 100);
    end;

    method MSecsToTimeStampTests;
    begin
      // This is EncodeDateTime(2017, 1, 1, 18, 31, 30, 100);
      var lTemp := MSecsToTimeStamp(63618978690100);
      Assert.AreEqual(lTemp.Time, 66690100);
      Assert.AreEqual(lTemp.Date, 736330);
    end;

    method TimeStampToMSecsTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 1, 18, 31, 30, 100);
      var lTemp: TTimeStamp := DateTimeToTimeStamp(lDate);
      var lMSecs := TimeStampToMSecs(lTemp);
      Assert.AreEqual(lMSecs, 63618978690100);
    end;

    method EncodeDateTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 1);

      lDate := EncodeDate(2000, 12, 31);
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2000);
      Assert.AreEqual(lMonth, 12);
      Assert.AreEqual(lDay, 31);
    end;

    method EncodeTimeTests;
    begin
      var lTime := EncodeTime(17, 40, 20, 100);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lTime, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 17);
      Assert.AreEqual(lMin, 40);
      Assert.AreEqual(lSec, 20);
      Assert.AreEqual(lMSec, 100);

      lTime := EncodeTime(0, 0, 1, 100);
      DecodeTime(lTime, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 0);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 1);
      Assert.AreEqual(lMSec, 100);

      lTime := EncodeTime(12, 00, 00, 1);
      DecodeTime(lTime, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 12);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
      Assert.AreEqual(lMSec, 1);
    end;

    method DecodeDateTests;
    begin
      var lDate := EncodeDate(2000, 1, 1);
      var lYear, lMonth, lDay: Word;
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2000);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 1);
    end;

    method DecodeDateFullyTests;
    begin
      var lYear, lMonth, lDay, lDOW: Word;
      var lDate := EncodeDateTime(2017, 1, 17, 17, 27, 23, 199);
      DecodeDateFully(lDate, var lYear, var lMonth, var lDay, var lDOW);
      Assert.AreEqual(lYear, 2017);
      Assert.AreEqual(lMonth, 1);
      Assert.AreEqual(lDay, 17);
      Assert.AreEqual(lDOW, 3);
    end;

    method DecodeTimeTests;
    begin
      var lTime := EncodeTime(0, 0, 0, 1);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lTime, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 0);
      Assert.AreEqual(lMin, 0);
      Assert.AreEqual(lSec, 0);
      Assert.AreEqual(lMSec, 1);

      lTime := EncodeTime(23, 59, 59, 999);
      DecodeTime(lTime, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 23);
      Assert.AreEqual(lMin, 59);
      Assert.AreEqual(lSec, 59);
      Assert.AreEqual(lMSec, 999);
    end;

    method DayOfWeekTests;
    begin
      var lDate := EncodeDate(2017, 1, 1);
      Assert.AreEqual(1, DayOfWeek(lDate));

      lDate := EncodeDate(2017, 1, 19);
      Assert.AreEqual(5, DayOfWeek(lDate));

      lDate := EncodeDate(2019, 3, 1);
      Assert.AreEqual(6, DayOfWeek(lDate));
    end;

    method DateTests;
    begin
      var lDate := Date;
      var lYear := YearOf(lDate);
      Assert.AreEqual(lYear, CurrentYear);
    end;

    method TimeTests;
    begin
      var lTime := Time;
      var lHour := HourOf(lTime);
      var lHour2 := HourOf(Now);
      Assert.AreEqual(lHour, lHour2);
    end;

    method NowTests;
    begin
      Assert.AreEqual(YearOf(Now), CurrentYear);
    end;

    method CurrentYearTests;
    begin
      Assert.AreEqual(CurrentYear, YearOf(Date));
    end;

    method IncMonthTests;
    begin
      var lDate := EncodeDate(2017, 1, 19);
      var lNewDate := IncMonth(lDate);
      Assert.AreEqual(MonthOf(lNewDate), 2);

      lNewDate := IncMonth(lDate, 2);
      Assert.AreEqual(MonthOf(lNewDate), 3);
    end;

    method IncAMonthTests;
    begin
      var lYear, lMonth, lDay: Word;
      lYear := 2017;
      lMonth := 1;
      lDay := 1;
      IncAMonth(var lYear, var lMonth, var lDay);
      Assert.AreEqual(lMonth, 2);
      IncAMonth(var lYear, var lMonth, var lDay, 3);
      Assert.AreEqual(lMonth, 5);

      IncAMonth(var lYear, var lMonth, var lDay, 8);
      Assert.AreEqual(lYear, 2018);
      Assert.AreEqual(lMonth, 1);
    end;

    method ReplaceTimeTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 19, 20, 50, 30, 120);
      var lNewTime := EncodeTime(10, 2, 3, 4);
      ReplaceTime(var lDate, lNewTime);
      var lHour, lMin, lSec, lMSec: Word;
      DecodeTime(lDate, var lHour, var lMin, var lSec, var lMSec);
      Assert.AreEqual(lHour, 10);
      Assert.AreEqual(lMin, 2);
      Assert.AreEqual(lSec, 3);
      Assert.AreEqual(lMSec, 4);
    end;

    method ReplaceDateTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 19, 20, 50, 30, 120);
      var lNewDate := EncodeDate(2018, 10, 10);
      var lYear, lMonth, lDay: Word;
      ReplaceDate(var lDate, lNewDate);
      DecodeDate(lDate, var lYear, var lMonth, var lDay);
      Assert.AreEqual(lYear, 2018);
      Assert.AreEqual(lMonth, 10);
      Assert.AreEqual(lDay, 10);
    end;

    method DateTimeToStringTests;
    begin
      var lDate := EncodeDateTime(2017, 1, 19, 20, 50, 30, 120);
      {$IF COOPER}
      var lString: nullable DelphiString;
      {$ELSE}
      var lString: DelphiString;
      {$ENDIF}
      DateTimeToString(var lString, 'dd/mm/yyyy', lDate, FormatSettings);
      Assert.AreEqual(lString, '19/01/2017');

      DateTimeToString(var lString, 'hh:mm:ss', lDate, FormatSettings);
      Assert.AreEqual(lString, '20:50:30');

      DateTimeToString(var lString, 'hh:nn:ss', lDate, FormatSettings);
      Assert.AreEqual(lString, '20:50:30');
    end;

  end;

end.