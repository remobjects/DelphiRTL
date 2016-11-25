namespace DelphiRTL.Tests.Shared;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

type
  StringManipulations = public class(Test)
  public

    method DefaultStringTypeTests();
    begin
      var x {: DelphiString} := 'hello';
      Assert.IsTrue(x is DelphiString);      
    
      var ds: DelphiString := 'hello';
      Assert.IsTrue(ds is DelphiString);      
    
      //var ws: WideString := 'hello';
      //Assert.IsTrue(ws is DelphiString);  // Invalid IL code in DelphiRTL.Tests.Shared.StringManipulations:DefaultStringTypeTests (): IL_006c: isinst    0x01000002
    
      var s : String := 'hello';
      Assert.IsTrue(s is PlatformString);      
    end;
    
    method IndexerTests();
    begin
      var x: DelphiString := 'house';
      x[1] := 'm';
      Assert.AreEqual(x, 'mouse');
    
      x.Remove(0, 1);
      Assert.AreEqual(x, 'ouse');
    
      x.Insert(0, "sp");
      Assert.AreEqual(x, 'spouse');
    end;
    
    method IndexOfTests;
    begin
      var x: DelphiString := '';
      x := 'Mack';
      Assert.AreEqual(x.IndexOf('', 2), 0, 'IndexOf 1');
      Assert.AreEqual(x.IndexOf('', 10), 0, 'IndexOf 2');
      Assert.AreEqual(x.IndexOf('M'), 0, 'IndexOf 3');
      Assert.AreEqual(x.IndexOf('M', 1), -1, 'IndexOf 4');
      
      x := 'Hello Hello';
      Assert.AreEqual(x.IndexOf('ll', 1), 2, 'IndexOf 5');
      Assert.AreEqual(x.IndexOf('ll', 2), 2, 'IndexOf 6');
      Assert.AreEqual(x.IndexOf('ll', 3), 8, 'IndexOf 7');
    
      x := 'Hello Hello';
      Assert.AreEqual(x.IndexOf('l',0, 2), -1, 'IndexOf 8');
      Assert.AreEqual(x.IndexOf('l',0, 3), 2, 'IndexOf 9');
      Assert.AreEqual(x.IndexOf('Hello', 0, 5), 0, 'IndexOf 10');
      Assert.AreEqual(x.IndexOf('llo', 6, 5), 8, 'IndexOf 11');
    
      x := 'a string in the street';
      Assert.AreEqual(x.IndexOf('car'), -1, 'IndexOf 12');
      Assert.AreEqual(x.IndexOf('a '), 0, 'IndexOf 13');
      Assert.AreEqual(x.IndexOf('street'), 16, 'IndexOf 14');
    end;
    
    method IndexOfAnyTests;
    begin
      var lToFind := new Char[5];
      var x: DelphiString := '';
      lToFind := ['a', 'z', '&', '(', ')'];
      
      x := 'a string in the street';
      Assert.AreEqual(x.IndexOfAny(lToFind), 0, 'IndexOfAny 1');
      x := 'virtual z';
      Assert.AreEqual(x.IndexOfAny(lToFind), 5, 'IndexOfAny 2');
      x := 'vmz';
      Assert.AreEqual(x.IndexOfAny(lToFind), 2, 'IndexOfAny 3');
      x := 'string&street';
      Assert.AreEqual(x.IndexOfAny(lToFind), 6, 'IndexOfAny 4');
      x := '(string)';
      Assert.AreEqual(x.IndexOfAny(lToFind), 0, 'IndexOfAny 5');
    
      x := 'a string';
      Assert.AreEqual(x.IndexOfAny(lToFind, 1), -1, 'IndexOfAny 6');
      Assert.AreEqual(x.IndexOfAny(lToFind, 0), 0, 'IndexOfAny 7');
      x := 'a string&';
      Assert.AreEqual(x.IndexOfAny(lToFind, 8), 8, 'IndexOfAny 8');
      Assert.AreEqual(x.IndexOfAny(lToFind, 3), 8, 'IndexOfAny case 9');
      Assert.AreEqual(x.IndexOfAny(lToFind, 7, 2), 8, 'IndexOfAny case 10');
      Assert.AreEqual(x.IndexOfAny(lToFind, 5, 2), -1, 'IndexOfAny case 11');
    end;
    
    method LastIndexOfTests;
    begin
      var x: DelphiString := '';
      x := 'Done';
      Assert.AreEqual(x.LastIndexOf('x', 2), -1, 'LastIndexOf 1');
      Assert.AreEqual(x.LastIndexOf('e'), 3, 'LastIndexOf 2');
      Assert.AreEqual(x.LastIndexOf('D', 2), 0, 'LastIndexOf 3');
      Assert.AreEqual(x.LastIndexOf('e', 2), -1, 'LastIndexOf 4');
    
      x := 'A drone is funny';
      Assert.AreEqual(x.LastIndexOf('n'), 14, 'LastIndexOf 5');
      Assert.AreEqual(x.LastIndexOf('A'), 0, 'LastIndexOf 6');
      Assert.AreEqual(x.LastIndexOf('drone'), 2, 'LastIndexOf 7');
      Assert.AreEqual(x.LastIndexOf('is', 3), -1, 'LastIndexOf 8');
      Assert.AreEqual(x.LastIndexOf('is', 8), -1, 'LastIndexOf 9');
      Assert.AreEqual(x.LastIndexOf('is', 10), 8, 'LastIndexOf 10');
      Assert.AreEqual(x.LastIndexOf('is', 9), 8, 'LastIndexOf 11');
      Assert.AreEqual(x.LastIndexOf('is', 10, 2), -1, 'LastIndexOf 12');
      Assert.AreEqual(x.LastIndexOf('is', 9, 1), -1, 'LastIndexOf 13');
      Assert.AreEqual(x.LastIndexOf('is', 9, 2), 8, 'LastIndexOf 14');
      Assert.AreEqual(x.LastIndexOf('o', 4), 4, 'LastIndexOf 15');
      Assert.AreEqual(x.LastIndexOf('o', 4, 1), 4, 'LastIndexOf 16');
      Assert.AreEqual(x.LastIndexOf('o', 5, 1), -1, 'LastIndexOf 17');
      Assert.AreEqual(x.LastIndexOf('o', 5, 2), 4, 'LastIndexOf 18');
    end;
    
    method PadLeftTests;
    begin
      var x: DelphiString := '';
      x := 'A string';
      Assert.AreEqual(x.PadLeft(10), '  A string');
      Assert.AreEqual(x.PadLeft(10, 'x'), 'xxA string');
      Assert.AreEqual(x.PadLeft(8), 'A string');
      Assert.AreEqual(x.PadLeft(8, 'x'), 'A string');
    end;
    
    method PadRightTests;
    begin
      var x: DelphiString := '';
      x := 'A string';
      Assert.AreEqual(x.PadRight(10), 'A string  ');
      Assert.AreEqual(x.PadRight(10, 'x'), 'A stringxx');
      Assert.AreEqual(x.PadRight(8), 'A string');
      Assert.AreEqual(x.PadRight(8, 'x'), 'A string');
    end;
    
    method TrimTests;
    begin
      var lToTrim := new Char[4];
      var x: DelphiString := '';
      lToTrim := ['&', '(', '[', '<'];
      x := 'String functions';
      Assert.AreEqual(x.Trim, 'String functions');
      x := ' String functions ';
      Assert.AreEqual(x.Trim, 'String functions');
      x := '<<<String functions[';
      Assert.AreEqual(x.Trim(lToTrim), 'String functions');
      x := '<<<<<String functions<a';
      Assert.AreEqual(x.Trim(lToTrim), 'String functions<a');
    end;
    
    method TrimLeftTests;
    begin
      var lToTrim := new Char[4];
      var x: DelphiString := '';
      lToTrim := ['&', '(', '[', '<'];
      x := 'String functions';
      Assert.AreEqual(x.TrimLeft(lToTrim), 'String functions');
      x := '&String functions ';
      Assert.AreEqual(x.TrimLeft(lToTrim), 'String functions ');
      x := '&[[String functions =';
      Assert.AreEqual(x.TrimLeft(lToTrim), 'String functions =');
      x := '<<<<<<&([String functions=';
      Assert.AreEqual(x.TrimLeft(lToTrim), 'String functions=');
      x := '   String functions';
      Assert.AreEqual(x.TrimLeft, 'String functions');
    end;
    
    method TrimRightTests;
    begin
      var lToTrim := new Char[4];
      var x: DelphiString := '';
      lToTrim := ['&', '(', '[', '<'];
      x := 'String functions1';
      Assert.AreEqual(x.TrimRight(lToTrim), 'String functions1');
      x := ' String functions2&';
      Assert.AreEqual(x.TrimRight(lToTrim), ' String functions2');
      x := ' =String functions3&[[';
      Assert.AreEqual(x.TrimRight(lToTrim), ' =String functions3');
      x := '=String functions4<<<<<<&([';
      Assert.AreEqual(x.TrimRight(lToTrim), '=String functions4');
      x := 'String functions5    ';
      Assert.AreEqual(x.TrimRight, 'String functions5');
    end;
    
    method StartsWithTests;
    begin
      var x: DelphiString := '';
      x := 'String in the street';
      Assert.AreEqual(x.StartsWith('String'), true, 'StartsWith 1');
      Assert.AreEqual(x.StartsWith('string'), false, 'StartsWith');
      Assert.AreEqual(x.StartsWith('string', true), true, 'StartsWith 3');
      x := 'string';
      Assert.AreEqual(x.StartsWith('string'), true, 'StartsWith 4');
      Assert.AreEqual(x.StartsWith('s'), true, 'StartsWith 5'); // NREs before hitting StartsWith()
    end;
    
    method EndsWithTests;
    begin
      var x: DelphiString := '';
      x := 'This is an string';
      Assert.AreEqual(x.EndsWith('string'), true, 'EndsWith 1');
      Assert.AreEqual(x.EndsWith('g'), true, 'EndsWith 2'); // Stack overflow
      Assert.AreEqual(x.EndsWith('String'), False, 'EndsWith 3');
      Assert.AreEqual(x.EndsWith('sTRING', true), true, 'EndsWith 4');
      Assert.AreEqual(x.EndsWith('sTRING', false), false, 'EndsWith 5');
      x := 'This';
      Assert.AreEqual(x.EndsWith('This'), true, 'EndsWith 6');
    end;     

    method JoinTests;
    begin
      var x: DelphiString := '';
      var lStrings := new DelphiString[4];
      lStrings := ['string', 'in', 'the', 'street'];
      Assert.AreEqual(x.Join(',', lStrings), 'string,in,the,street');
      Assert.AreEqual(x.Join('##', lStrings), 'string##in##the##street');      
      Assert.AreEqual(x.Join('#', lStrings, 0, 1), 'string');
      Assert.AreEqual(x.Join('##', lStrings, 0, 1), 'string');
      Assert.AreEqual(x.Join('#', lStrings, 0, 2), 'string#in');
      Assert.AreEqual(x.Join('##', lStrings, 0, 2), 'string##in');
    end;

    method SubStringTests;
    begin
      var x: DelphiString := '';
      x := 'test string';
      Assert.AreEqual(x.Substring(5), 'string');
      Assert.AreEqual(x.Substring(0), 'test string');
      Assert.AreEqual(x.Substring(0, 4), 'test');
      Assert.AreEqual(x.SubString(5, 2), 'st');
    end;

    method ContainsTests;
    begin
      var x: DelphiString := '';
      x := '';
      Assert.AreEqual(x.Contains('nothing'), false, 'Contains 1');
      x := 'this is a test';
      Assert.AreEqual(x.Contains('this'), true, 'Contains 2');
      Assert.AreEqual(x.Contains('test'), true, 'Contains 3');
      Assert.AreEqual(x.Contains('this is a test string or not'), false, 'Contains 4');
      Assert.AreEqual(x.Contains('is a'), true, 'Contains 5');
      Assert.AreEqual(x.Contains('t'), true, 'Contains 6'); 
      Assert.AreEqual(x.Contains('this is a test'), true, 'Contains 7');
      Assert.AreEqual(x.Contains('st'), true, 'Contains 8');
      Assert.AreEqual(x.Contains('a'), true, 'Contains 9'); 
      Assert.AreEqual(x.Contains('x'), false, 'Contains 10'); 
    end;

    method CopyTests;
    begin
      var x: DelphiString := 'String test!';      
      Assert.AreEqual(x.Copy(x), 'String test!');
      x := 'x';  
      Assert.AreEqual(x.Copy(x), 'x');
    end;

    method CopyToTests;
    begin
      var x: DelphiString := 'Test string';      
      var lTarget := new Char[11];
      var lEqual := new Char[11];
      lEqual := ['T', 'e', 's', 't', ' ', 's', 't', 'r', 'i', 'n', 'g'];
      x.CopyTo(0, var lTarget, 0, 11);
      for i: integer := 0 to lEqual.length - 1 do
        Assert.AreEqual(lTarget[i], lEqual[i]);
      var lTarget2 := new Char[6];
      var lEqual2 := new Char[6];
      lEqual2 := ['s', 't', 'r', 'i', 'n', 'g'];
      x.CopyTo(5, var lTarget2, 0, 6);
      for i: integer := 0 to lEqual2.length - 1 do
        Assert.AreEqual(lTarget2[i], lEqual2[i]);
    end;

    method CountCharTests;
    begin
      var x: DelphiString := 'Test string';      
      
      Assert.AreEqual(x.CountChar('t'), 2);
      Assert.AreEqual(x.CountChar('g'), 1);
      x := 'aaaaaaaaaa';
      Assert.AreEqual(x.CountChar('a'), 10);
    end;

    method DeQuotedStringTests;
    begin
      var x: DelphiString := '''Test string''';      
      Assert.AreEqual(x.DeQuotedString, 'Test string');
      x := '''Test ''''string''';
      Assert.AreEqual(x.DeQuotedString, 'Test ''string');

      x := '"Come on"';
      Assert.AreEqual(x.DeQuotedString('"'), 'Come on');
      x := '"Come ""on"';
      Assert.AreEqual(x.DeQuotedString('"'), 'Come "on');
      x := '"Come on';
      Assert.AreEqual(x.DeQuotedString('"'), '"Come on');
    end;

    method QuotedStringTests;
    begin
      var x: DelphiString;
      x := 'Test string';
      Assert.AreEqual(x.QuotedString, '''Test string''');

      x := '''Test ''" string''';
      Assert.AreEqual(x.QuotedString, '''''''Test ''''" string''''''');

      x := 'Test '' string';
      Assert.AreEqual(x.QuotedString, '''Test '''' string''');

      x := 'Test string';
      Assert.AreEqual(x.QuotedString('"'), '"Test string"');

      x := 'Test " string';
      Assert.AreEqual(x.QuotedString('"'), '"Test "" string"');
    end;

    method InsertTests;
    begin
      var x: DelphiString;
      x := 'Test string';
      Assert.AreEqual(x.Insert(0, 'Super'), 'SuperTest string');
      x := 'Test string';
      Assert.AreEqual(x.Insert(11, 'A'), 'Test stringA');
      x := 'Test string';
      Assert.AreEqual(x.Insert(4, 'ing'), 'Testing string');
      x := 'Test string';
      Assert.AreEqual(x.Insert(4, '1'), 'Test1 string');
      x := 'xxx';
      Assert.AreEqual(x.Insert(0, '1'), '1xxx');
    end;

    method &RemoveTests;
    begin
      var x: DelphiString;
      x := 'Test string';
      Assert.AreEqual(x.Remove(4), 'Test');
      x := 'Test string';
      Assert.AreEqual(x.Remove(4, 1), 'Teststring');
      x := 'Test string';
      Assert.AreEqual(x.Remove(0, 1), 'est string');
      x := 'Test string';
      Assert.AreEqual(x.Remove(10, 1), 'Test strin');
    end;

    method IsEmptyTests;
    begin
      var x: DelphiString := '';
      Assert.AreEqual(x.IsEmpty, true);
      x := ' ';
      Assert.AreEqual(x.IsEmpty, false);
    end;
    
    method IsNullOrEmptyTests;
    begin
      Assert.AreEqual(DelphiString.IsNullOrEmpty(''), true);
      Assert.AreEqual(DelphiString.IsNullOrEmpty(' '), false);
    end;

    method IsNullOrWhiteSpaceTests;
    begin
      Assert.AreEqual(DelphiString.IsNullOrWhiteSpace(''), true);
      Assert.AreEqual(DelphiString.IsNullOrWhiteSpace(' '), true);
      Assert.AreEqual(DelphiString.IsNullOrWhiteSpace(' s'), false);
    end;

    method ReplaceTests;
    begin
      var x: DelphiString := 'Test string';
      Assert.AreEqual(x.Replace('t', 'x'), 'Tesx sxring');
      Assert.AreEqual(x.Replace('x', 'v'), 'Test string');
      {$IF ECHOES OR TOFFEE} // T76259
      Assert.AreEqual(x.Replace('t', 'x', [TReplaceFlags.rfReplaceAll]), 'Tesx sxring');
      Assert.AreEqual(x.Replace('T', 'x'), 'xest string');
      Assert.AreEqual(x.Replace('t', 'x', [TReplaceFlags.rfIgnoreCase]), 'xest string');
      Assert.AreEqual(x.Replace('t', 'x', [TReplaceFlags.rfReplaceAll, TReplaceFlags.rfIgnoreCase]), 'xesx sxring');
      {$ENDIF}

      x := 'This is a test string';
      Assert.AreEqual(x.Replace('This', 'Maybe'), 'Maybe is a test string');
      Assert.AreEqual(x.Replace('string', 'procedure'), 'This is a test procedure');

      {$IF ECHOES OR TOFFEE} // T76259
      x := 'This this xxx this';
      Assert.AreEqual(x.Replace('This', 'Wall'), 'Wall this xxx this');
      Assert.AreEqual(x.Replace('this', 'that', [TReplaceFlags.rfReplaceAll]), 'This that xxx that');
      Assert.AreEqual(x.Replace('this', 'that', [TReplaceFlags.rfReplaceAll, TReplaceFlags.rfIgnoreCase]), 'that that xxx that');
      {$ENDIF}
    end;

    method SplitTests;
    begin
      var x: DelphiString := '';
      var lCharSep := new Char[4];
      var lRes: array of DelphiString;
      lCharSep := ['|', '#', '@', '%'];
      x := 'one|two|three#four@five%six|seven@eight@nine%ten';

      lRes := x.Split(lCharSep);
      Assert.AreEqual(lRes.Length, 10);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[9], 'ten');
      
      lRes := x.Split(lCharSep, 4);
      Assert.AreEqual(lRes.Length, 4);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[3], 'four@five%six|seven@eight@nine%ten');

      x := 'one|two|three##four@five%six|seven@eight@nine%ten';
      lRes := x.Split(lCharSep);
      Assert.AreEqual(lRes.Length, 11);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[10], 'ten');
      
      lRes := x.Split(lCharSep, TStringSplitOptions.ExcludeEmpty);
      Assert.AreEqual(lRes.Length, 10);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[9], 'ten');

      lRes := x.Split(lCharSep, 4, TStringSplitOptions.ExcludeEmpty);
      Assert.AreEqual(lRes.Length, 4);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[3], 'four@five%six|seven@eight@nine%ten');

      var lStringSep := new DelphiString[4];
      lStringSep := ['||', '##', '@@', '%%'];

      x := 'one||two||three##four@@five%%six||seven@@eight@@nine%%ten';
      lRes := x.Split(lStringSep);
      Assert.AreEqual(lRes.Length, 10);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[9], 'ten');
      
      lRes := x.Split(lStringSep, 4);
      Assert.AreEqual(lRes.Length, 4);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[3], 'four@@five%%six||seven@@eight@@nine%%ten');

      x := 'one||two||three####four@@five%%six||seven@@eight@@nine%%ten';
      lRes := x.Split(lStringSep);
      Assert.AreEqual(lRes.Length, 11);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[10], 'ten');
      
      lRes := x.Split(lStringSep, TStringSplitOptions.ExcludeEmpty);
      Assert.AreEqual(lRes.Length, 10);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[9], 'ten');

      lRes := x.Split(lStringSep, 4, TStringSplitOptions.ExcludeEmpty);
      Assert.AreEqual(lRes.Length, 4);
      Assert.AreEqual(lRes[0], 'one');
      Assert.AreEqual(lRes[3], 'four@@five%%six||seven@@eight@@nine%%ten'); 
    end;

    method CompareToTests;
    begin
      var x: DelphiString := 'Do not touch';

      Assert.AreEqual(x.CompareTo('Do not touch'), 0, 'CompareTo 1');
      Assert.AreEqual(x.CompareTo('Do not Touch') <> 0, true, 'CompareTo 2');
      Assert.AreEqual(x.CompareTo('Do not') <> 0, true, 'CompareTo 3');
      Assert.AreEqual(x.CompareTo('Do not touch   ') <> 0, true, 'CompareTo 4');
    end;

    method LowerCaseTests;
    begin
      var x: DelphiString := 'Do not touch';
      Assert.AreEqual(DelphiString.LowerCase(x), 'do not touch');
      Assert.AreEqual(DelphiString.LowerCase('do not touch'), 'do not touch');
      Assert.AreEqual(DelphiString.LowerCase('Do not touch1'), 'do not touch1');
    end;

    method UpperCaseTests;
    begin
      var x: DelphiString := 'do not touch';
      Assert.AreEqual(DelphiString.UpperCase(x), 'DO NOT TOUCH');
      Assert.AreEqual(DelphiString.UpperCase('DO NOT TOUCH'), 'DO NOT TOUCH');
      Assert.AreEqual(DelphiString.UpperCase('Do not touch1'), 'DO NOT TOUCH1');
    end;

    method ConvertToTests;
    begin
      Assert.AreEqual(DelphiString.ToBoolean('true'), true, 'Convert 1 ToBoolean');
      Assert.AreEqual(DelphiString.ToBoolean('false'), false, 'Convert 2 ToBoolean');

      Assert.AreEqual(DelphiString.ToInteger('0'), 0, 'Convert 3 ToInteger');
      Assert.AreEqual(DelphiString.ToInteger('123'), 123, 'Convert 4 ToInteger');
      Assert.AreEqual(DelphiString.ToInteger('-13'), -13, 'Convert 5 ToInteger');
      Assert.AreEqual(DelphiString.ToInteger('2043323'), 2043323, 'Convert 6 ToInteger');

      Assert.AreEqual(DelphiString.ToInt64('20433231'), 20433231, 'Convert 7 ToInt64');

      Assert.AreEqual(DelphiString.ToDouble('0.0'), 0.0, 'Convert 8 ToDouble');
      Assert.AreEqual(DelphiString.ToDouble('12345.234'), 12345.234, 'Convert 9 ToDouble');
      Assert.AreEqual(DelphiString.ToDouble('-12.12'), -12.12, 'Convert 10 ToDouble');
      Assert.AreEqual(DelphiString.ToSingle('123.123'), 123.123, 'Convert 11 ToSingle');
      Assert.AreEqual(DelphiString.ToExtended('123456.1234'), 123456.1234, 'Convert 12 ToExtended');
      Assert.AreEqual(DelphiString.ToExtended('0.0'), 0.0, 'Convert 14 ToExtended');
      Assert.AreEqual(DelphiString.ToExtended('-1.0'), -1.0, 'Convert 14 ToExtended');
    end;

    method ParseTests;
    begin
      Assert.AreEqual(DelphiString.LowerCase(DelphiString.Parse(true)), 'true');  // .Net is returning 'True' and 'False'
      Assert.AreEqual(DelphiString.LowerCase(DelphiString.Parse(false)), 'false');
      Assert.AreEqual(DelphiString.Parse(0), '0');
      Assert.AreEqual(DelphiString.Parse(123456), '123456');
      Assert.AreEqual(DelphiString.Parse(-123456), '-123456');
      Assert.AreEqual(DelphiString.Parse(123456.123), Sugar.Convert.ToString(123456.123));  // Locale changes '.' and ','
      Assert.AreEqual(DelphiString.Parse(-123456.123), Sugar.Convert.ToString(-123456.123));
    end;

    method CompareTextTests;
    begin
      Assert.AreEqual(DelphiString.CompareText('Hello', 'Hello'), 0);
      Assert.AreEqual(DelphiString.CompareText('Hello', 'hello'), 0);
      Assert.AreEqual(DelphiString.CompareText('hell', 'hello') < 0, true);
      Assert.AreEqual(DelphiString.CompareText('hello', 'hell') > 0, true);
    end;

    method EqualsTests;
    begin
      var x: DelphiString := 'Do not touch';
      var y: DelphiString := 'Test string';
      Assert.AreEqual(DelphiString.Equals(x, x), true);
      Assert.AreEqual(DelphiString.Equals(x, y), false);
      Assert.AreEqual(DelphiString.Equals(x, 'Do not touch'), true);
      Assert.AreEqual(x.Equals('Do not touch'), true);
      Assert.AreEqual(x.Equals(y), false);
      Assert.AreEqual(x.Equals(x), true);
    end;

    method CompareTests;
    begin
      var x: DelphiString := 'Do not touch';
      Assert.AreEqual(DelphiString.Compare(x, 'Do not touch'), 0, 'Compare 1');
      Assert.AreEqual(DelphiString.Compare(x, 'DO NOT TOUCH', [TCompareOption.coIgnoreCase]), 0, 'Compare 2');
      Assert.AreEqual(DelphiString.Compare(x, 'DO NOT TOUCH', true), 0, 'Compare 3');
      Assert.AreEqual(DelphiString.Compare(x, 'DO NOT TOUCH', false) <> 0, true, 'Compare 4');
      Assert.AreEqual(DelphiString.Compare(x, 2, 'DO NOT TOUCH', 2, 6, true), 0, 'Compare 5');
      var y: DelphiString := 'Do not go';
      Assert.AreEqual(DelphiString.Compare(x, y) <> 0, true, 'Compare 6');
      Assert.AreEqual(DelphiString.Compare(x, 0, y, 0, 6), 0, 'Compare 7');
      Assert.AreEqual(DelphiString.Compare(x, 2, y, 2, 4), 0, 'Compare 8');
      Assert.AreEqual(DelphiString.Compare(x, 2, y, 2, 6) <> 0, true, 'Compare 9');
      Assert.AreEqual(DelphiString.Compare(x, 0, y, 0, 8) <> 0, true, 'Compare 10');
      x := 'do not';
      y := 'do not touch';
      Assert.AreEqual(DelphiString.Compare(x, y) <> 0, true, 'Compare 11');
    end;

    method ToLowerTests;
    begin
      var x: DelphiString := 'SMALL STRING';
      Assert.AreEqual(x.ToLower, 'small string');

      x := 'NIÑO';
      Assert.AreEqual(x.ToLower(TLanguages.GetLocaleIDFromLocaleName('es-es')), 'niño');
    end;

    method ToUpperTests;
    begin
      var x: DelphiString := 'small string';
      Assert.AreEqual(x.ToUpper, 'SMALL STRING');

      x := 'niño';
      Assert.AreEqual(x.ToUpper(TLanguages.GetLocaleIDFromLocaleName('es-es')), 'NIÑO');
    end;

    method CreateFromArrayTests;
    begin
      var lArray := new Char[5];
      lArray := ['H', 'E', 'L', 'L', 'O'];
      var x: DelphiString := DelphiString.Create(lArray);
      Assert.AreEqual(x, 'HELLO');
      x := DelphiString.Create(lArray, 2, 3);
      Assert.AreEqual(x, 'LLO');
    end;

    method LastIndexOfAnyTests;
    begin
      var lToFind := new Char[5];
      var x: DelphiString := '';
      lToFind := ['a', 'z', '&', '(', ')'];
      
      x := 'string one z';
      Assert.AreEqual(x.LastIndexOfAny(lToFind), 11, 'LastIndexOfAny 1');
      x := 'z string';
      Assert.AreEqual(x.LastIndexOfAny(lToFind), 0, 'LastIndexOfAny 2');
      x := 'string&street';
      Assert.AreEqual(x.LastIndexOfAny(lToFind), 6, 'LastIndexOfAny 3');
      x := '(string)';
      Assert.AreEqual(x.LastIndexOfAny(lToFind), 7, 'LastIndexOfAny 4');
    
      x := 'string a';
      Assert.AreEqual(x.LastIndexOfAny(lToFind, 6), -1, 'LastIndexOfAny 5');
      x := 'a string';
      Assert.AreEqual(x.LastIndexOfAny(lToFind, 7), 0, 'LastIndexOfAny 6');
      x := 'a string(12';
      Assert.AreEqual(x.LastIndexOfAny(lToFind, 10, 2), -1, 'LastIndexOfAny 7');
      Assert.AreEqual(x.LastIndexOfAny(lToFind, 10, 3), 8, 'LastIndexOfAny 8');
    end;
    
    method LanguagesTests;
    begin
      var lTmp := TLanguages.UserDefaultLocale;
      Assert.AreEqual(lTmp.Identifier <> '', true);
      lTmp := TLanguages.GetLocaleIDFromLocaleName('en-us');
      Assert.AreEqual(lTmp.Identifier <> '', true);
    end;
  end; 
end.
