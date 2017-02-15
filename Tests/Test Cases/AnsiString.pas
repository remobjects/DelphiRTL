namespace DelphiRTL.Tests.Shared;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi;

type
  AnsiStringUsage = public class(Test)
  public
    method IndexerTests();
    begin
      var x : AnsiString := 'hello';
      Assert.AreEqual(x[1], AnsiChar('h'));      
    end;

    method InsertTests;
    begin
      var x: AnsiString;
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

    method RemoveTests;
    begin
      var x: AnsiString;
      x := 'Test string';
      Assert.AreEqual(x.Remove(4), 'Test');
      x := 'Test string';
      Assert.AreEqual(x.Remove(4, 1), 'Teststring');
      x := 'Test string';
      Assert.AreEqual(x.Remove(0, 1), 'est string');
      x := 'Test string';
      Assert.AreEqual(x.Remove(10, 1), 'Test strin');
    end;

    method CopyToTests;
    begin
      var x: AnsiString := 'Test string';      
      var lTarget := new Byte[11];
      x.CopyTo(0, var lTarget, 0, 11);
      Assert.AreEqual(new AnsiString(lTarget), 'Test string');

      var lTarget2 := new Byte[6];
      x.CopyTo(5, var lTarget2, 0, 6);
      Assert.AreEqual(new AnsiString(lTarget2), 'string');
    end;

    method ToUpperTests;
    begin
      var x: AnsiString := 'come on';
      var y := x.ToUpper;
      Assert.AreEqual(y, 'COME ON');
    end;

    method ToLowerTests;
    begin
      var x: AnsiString := 'COME ON';
      var y := x.ToLower;
      Assert.AreEqual(y, 'come on');
    end;

    method TrimTests;
    begin
      var x: AnsiString := '   Come On  ';
      Assert.AreEqual(x.Trim, 'Come On');
      x := ' String functions ';
      Assert.AreEqual(x.Trim, 'String functions');
    end;
    
    method TrimLeftTests;
    begin
      var x: AnsiString := '  Come On  ';
      Assert.AreEqual(x.TrimLeft, 'Come On  ');
      x := 'Come On  ';
      Assert.AreEqual(x.TrimLeft, 'Come On  ');
    end;
    
    method TrimRightTests;
    begin
      var x: AnsiString := '  Come On  ';
      Assert.AreEqual(x.TrimRight, '  Come On');
      x := 'Come On';
      Assert.AreEqual(x.TrimRight, 'Come On');
    end;

    method IndexOfTests;
    begin
      var x: AnsiString := '';
      x := 'Mack';
      Assert.AreEqual(x.IndexOf('', 2), -1, 'IndexOf 1');
      Assert.AreEqual(x.IndexOf('', 10), -1, 'IndexOf 2');
      Assert.AreEqual(x.IndexOf('M'), 0, 'IndexOf 3');
      Assert.AreEqual(x.IndexOf('M', 1), -1, 'IndexOf 4');
      
      x := 'Hello Hello';
      Assert.AreEqual(x.IndexOf('ll', 1), 2, 'IndexOf 5');
      Assert.AreEqual(x.IndexOf('ll', 2), 2, 'IndexOf 6');
      Assert.AreEqual(x.IndexOf('ll', 3), 8, 'IndexOf 7');
    
      x := 'a string in the street';
      Assert.AreEqual(x.IndexOf('car'), -1, 'IndexOf 8');
      Assert.AreEqual(x.IndexOf('a '), 0, 'IndexOf 9');
      Assert.AreEqual(x.IndexOf('street'), 16, 'IndexOf 10');
    end;

    method ContainsTests;
    begin
      var x: AnsiString := '';
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

    method StartsWithTests;
    begin
      var x: AnsiString := '';
      x := 'String in the street';
      Assert.AreEqual(x.StartsWith('String'), true, 'StartsWith 1');
      Assert.AreEqual(x.StartsWith('string'), false, 'StartsWith 2');
      x := 'string';
      Assert.AreEqual(x.StartsWith('string'), true, 'StartsWith 3');
      Assert.AreEqual(x.StartsWith('s'), true, 'StartsWith 4'); 
    end;
    
    method EndsWithTests;
    begin
      var x: AnsiString := '';
      x := 'This is an string';
      Assert.AreEqual(x.EndsWith('string'), true, 'EndsWith 1');
      Assert.AreEqual(x.EndsWith('g'), true, 'EndsWith 2'); 
      Assert.AreEqual(x.EndsWith('String'), False, 'EndsWith 3');
      x := 'This';
      Assert.AreEqual(x.EndsWith('This'), true, 'EndsWith 4');
    end;     

    method ReplaceTests;
    begin
      var x: AnsiString := 'Test string';
      Assert.AreEqual(x.Replace('t', 'x'), 'Tesx string');
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

    method FillCharTests;
    begin
      var x: AnsiString := '';
      x.FillChar(10, AnsiChar('x'));
      Assert.AreEqual(x, 'xxxxxxxxxx');
    end;

    method CompareStrTests;
    begin
      Assert.AreEqual(CompareStr(AnsiString('aaaaaaa'), AnsiString('bbbb')) < 0, true);
      Assert.AreEqual(CompareStr(AnsiString('bbbb'), AnsiString('aa')) > 0, true);
      Assert.AreEqual(CompareStr(AnsiString('Test string'), AnsiString('test string')) <> 0, true);
      Assert.AreEqual(CompareStr(AnsiString('Test string'), AnsiString('Test string')) = 0, true);
      Assert.AreEqual(CompareStr(AnsiString('aaaa'), AnsiString('aaax')) < 0, true);
      Assert.AreEqual(CompareStr(AnsiString('zaaa'), AnsiString('aaaa')) > 0, true);
    end;

    method SameStrTests;
    begin
      Assert.AreEqual(SameStr(AnsiString('aaaaaaa'), AnsiString('bbbb')), false);
      Assert.AreEqual(SameStr(AnsiString('bbbb'), AnsiString('aa')), false);
      Assert.AreEqual(SameStr(AnsiString('Test string'), AnsiString('test string')), false);
      Assert.AreEqual(SameStr(AnsiString('Test string'), AnsiString('Test string')), true);
      Assert.AreEqual(SameStr(AnsiString('aaaa'), AnsiString('aaax')), false);
      Assert.AreEqual(SameStr(AnsiString('zaaa'), AnsiString('aaaa')), false);
    end;

    method CompareTextTests;
    begin
      Assert.AreEqual(CompareText(AnsiString('aaaaaaa'), AnsiString('bbbb')) < 0, true);
      Assert.AreEqual(CompareText(AnsiString('bbbb'), AnsiString('aa')) > 0, true);
      Assert.AreEqual(CompareText(AnsiString('Test string'), AnsiString('test string')) = 0, true);
      Assert.AreEqual(CompareText(AnsiString('Test string'), AnsiString('Test string')) = 0, true);
      Assert.AreEqual(CompareText(AnsiString('aaaa'), AnsiString('aaax')) < 0, true);
      Assert.AreEqual(CompareText(AnsiString('zaaa'), AnsiString('aaaa')) > 0, true);
    end;

    method SameTextTests;
    begin
      Assert.AreEqual(SameText(AnsiString('aaaaaaa'), AnsiString('bbbb')), false);
      Assert.AreEqual(SameText(AnsiString('bbbb'), AnsiString('aa')), false);
      Assert.AreEqual(SameText(AnsiString('Test string'), AnsiString('test string')), true);
      Assert.AreEqual(SameText(AnsiString('Test string'), AnsiString('Test string')), true);
      Assert.AreEqual(SameText(AnsiString('aaaa'), AnsiString('aaax')), false);
      Assert.AreEqual(SameText(AnsiString('zaaa'), AnsiString('aaaa')), false);
    end;

    method PosTests;
    begin
      //1 based
      var x: AnsiString := '';
      x := 'Mack';
      Assert.AreEqual(Pos('M', x), 1);
      Assert.AreEqual(Pos('', x), 0);
      
      x := 'Hello Hello';
      Assert.AreEqual(Pos('ll', x), 3);
    
      x := 'a string in the street';
      Assert.AreEqual(Pos('street', x), 17);
    end;

    method CopyTests;
    begin
      // 1 based
      var x: AnsiString := 'Test1234';
      var y := &Copy(x, 5, 4);
      Assert.AreEqual(y, '1234');
    end;

    method DeleteTests;
    begin
      // 1 based
      var x: AnsiString := 'Test1234';
      DeleteA(var x, 5, 4);
      Assert.AreEqual(x, 'Test');
      DeleteA(var x, 1, 1);
      Assert.AreEqual(x, 'est');
    end;

    method InsertFuncTests;
    begin
      var x: AnsiString := 'Test1234';
      Insert('5678', var x, 9);
      Assert.AreEqual(x, 'Test12345678');
      Insert('0', var x, 1);
      Assert.AreEqual(x, '0Test12345678');
    end;

    method UpperCaseTests;
    begin
      var x: AnsiString := 'come on';
      var y := UpperCase(x);
      Assert.AreEqual(y, 'COME ON');
    end;

    method LowerCaseTests;
    begin
      var x: AnsiString := 'COME ON';
      var y := LowerCase(x);
      Assert.AreEqual(y, 'come on');
    end;

    method TrimFuncTests;
    begin
      var x: AnsiString := '   Come On  ';
      Assert.AreEqual(Trim(x), 'Come On');
      x := ' String functions ';
      Assert.AreEqual(Trim(x), 'String functions');
    end;
    
    method TrimLeftFuncTests;
    begin
      var x: AnsiString := '  Come On  ';
      Assert.AreEqual(TrimLeft(x), 'Come On  ');
      x := 'Come On  ';
      Assert.AreEqual(TrimLeft(x), 'Come On  ');
    end;
    
    method TrimRightFuncTests;
    begin
      var x: AnsiString := '  Come On  ';
      Assert.AreEqual(TrimRight(x), '  Come On');
      x := 'Come On';
      Assert.AreEqual(TrimRight(x), 'Come On');
    end;

    method ContainsTextTests;
    begin
      var x: AnsiString := '';
      x := '';
      Assert.AreEqual(ContainsText(x, 'nothing'), false, 'Contains 1');
      x := 'this is a test';
      Assert.AreEqual(ContainsText(x, 'this'), true, 'Contains 2');
      Assert.AreEqual(ContainsText(x, 'test'), true, 'Contains 3');
      Assert.AreEqual(ContainsText(x, 'this is a test string or not'), false, 'Contains 4');
      Assert.AreEqual(ContainsText(x, 'is a'), true, 'Contains 5');
      Assert.AreEqual(ContainsText(x, 't'), true, 'Contains 6'); 
      Assert.AreEqual(ContainsText(x, 'this is a test'), true, 'Contains 7');
      Assert.AreEqual(ContainsText(x, 'st'), true, 'Contains 8');
      Assert.AreEqual(ContainsText(x, 'a'), true, 'Contains 9'); 
      Assert.AreEqual(ContainsText(x, 'x'), false, 'Contains 10'); 
    end;

    method StartsTextTests;
    begin
      var x: AnsiString := '';
      x := 'String in the street';
      Assert.AreEqual(StartsText('String', x), true, 'StartsWith 1');
      Assert.AreEqual(StartsText('string', x), false, 'StartsWith 2');
      x := 'string';
      Assert.AreEqual(StartsText('string', x), true, 'StartsWith 3');
      Assert.AreEqual(StartsText('s', x), true, 'StartsWith 4'); 
    end;
    
    method EndsTextTests;
    begin
      var x: AnsiString := '';
      x := 'This is an string';
      Assert.AreEqual(EndsText('string', x), true, 'EndsWith 1');
      Assert.AreEqual(EndsText('g', x), true, 'EndsWith 2'); 
      Assert.AreEqual(EndsText('String', x), False, 'EndsWith 3');
      x := 'This';
      Assert.AreEqual(EndsText('This', x), true, 'EndsWith 4');
    end;     

    method FillCharFuncTests;
    begin
      var x: AnsiString := '';
      FillChar(var x, 10, AnsiChar('x'));
      Assert.AreEqual(x, 'xxxxxxxxxx');
    end;

    method ReplaceFuncTests;
    begin
      var x: AnsiString := 'Test string';
      Assert.AreEqual(ReplaceText(x, 't', 'x'), 'xesx sxring');
      Assert.AreEqual(ReplaceText(x, 'x', 'v'), 'Test string');
      {$IF ECHOES OR TOFFEE} // T76259
      Assert.AreEqual(StringReplace(x, 't', 'x', [TReplaceFlags.rfReplaceAll]), 'Tesx sxring');
      Assert.AreEqual(ReplaceText(x, 'T', 'x'), 'xesx sxring');
      Assert.AreEqual(StringReplace(x, 't', 'x', [TReplaceFlags.rfIgnoreCase]), 'xest string');
      Assert.AreEqual(StringReplace(x, 't', 'x', [TReplaceFlags.rfReplaceAll, TReplaceFlags.rfIgnoreCase]), 'xesx sxring');
      {$ENDIF}

      x := 'This is a test string';
      Assert.AreEqual(ReplaceText(x, 'This', 'Maybe'), 'Maybe is a test string');
      Assert.AreEqual(ReplaceText(x, 'string', 'procedure'), 'This is a test procedure');

      {$IF ECHOES OR TOFFEE} // T76259
      x := 'This this xxx this';
      Assert.AreEqual(ReplaceText(x, 'This', 'Wall'), 'Wall Wall xxx Wall');
      Assert.AreEqual(StringReplace(x, 'this', 'that', [TReplaceFlags.rfReplaceAll]), 'This that xxx that');
      Assert.AreEqual(StringReplace(x, 'this', 'that', [TReplaceFlags.rfReplaceAll, TReplaceFlags.rfIgnoreCase]), 'that that xxx that');
      {$ENDIF}
    end;
  end;

end.
