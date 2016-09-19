namespace DelphiRTL.Tests.Shared;

interface

uses
  RemObjects.Elements.EUnit;
  //Elements.RTL.Delphi;

type
  StringManipulations = public class(Test)
  private
  protected
  public    
    method IndexerTests();
    // Sugar added method tests
    method IndexOfTests;
    method IndexOfAnyTests;
    method LastIndexOfTests;
    method PadLeftTests;
    method PadRightTests;
    method TrimTests;
    method TrimLeftTests;
    method TrimRightTests;
    method StartsWithTests;
    method EndsWithTests;
    //method JoinTests;
    //method SubStringTests;
    // end of Sugar tests

  end;

implementation

method StringManipulations.IndexerTests();
begin
  var x: Elements.RTL.Delphi.DelphiString := 'house';
  x[1] := 'm';
  Assert.AreEqual(x, 'mouse');

  x.Remove(0, 1);
  Assert.AreEqual(x, 'ouse');

  x.Insert(0, "sp");
  Assert.AreEqual(x, 'spouse');
end;

method StringManipulations.IndexOfTests;
begin
  var x: Elements.RTL.Delphi.DelphiString := '';
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

method StringManipulations.IndexOfAnyTests;
begin
  var lToFind := new Char[5];
  var x: Elements.RTL.Delphi.DelphiString := '';
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

method StringManipulations.LastIndexOfTests;
begin
  var x: Elements.RTL.Delphi.DelphiString := '';
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

method StringManipulations.PadLeftTests;
begin
  var x: Elements.RTL.Delphi.DelphiString := '';
  x := 'A string';
  Assert.AreEqual(x.PadLeft(10), '  A string');
  Assert.AreEqual(x.PadLeft(10, 'x'), 'xxA string');
  Assert.AreEqual(x.PadLeft(8), 'A string');
  Assert.AreEqual(x.PadLeft(8, 'x'), 'A string');
end;

method StringManipulations.PadRightTests;
begin
  var x: Elements.RTL.Delphi.DelphiString := '';
  x := 'A string';
  Assert.AreEqual(x.PadRight(10), 'A string  ');
  Assert.AreEqual(x.PadRight(10, 'x'), 'A stringxx');
  Assert.AreEqual(x.PadRight(8), 'A string');
  Assert.AreEqual(x.PadRight(8, 'x'), 'A string');
end;

method StringManipulations.TrimTests;
begin
  var lToTrim := new Char[4];
  var x: Elements.RTL.Delphi.DelphiString := '';
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

method StringManipulations.TrimLeftTests;
begin
  var lToTrim := new Char[4];
  var x: Elements.RTL.Delphi.DelphiString := '';
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

method StringManipulations.TrimRightTests;
begin
  var lToTrim := new Char[4];
  var x: Elements.RTL.Delphi.DelphiString := '';
  lToTrim := ['&', '(', '[', '<'];
  x := 'String functions';
  Assert.AreEqual(x.TrimRight(lToTrim), 'String functions');
  x := ' String functions&';
  Assert.AreEqual(x.TrimRight(lToTrim), ' String functions');
  x := ' =String functions&[[';
  Assert.AreEqual(x.TrimRight(lToTrim), ' =String functions');
  x := '=String functions<<<<<<&([';
  Assert.AreEqual(x.TrimRight(lToTrim), '=String functions');
  x := 'String functions    ';
  Assert.AreEqual(x.TrimRight, 'String functions');
end;

method StringManipulations.StartsWithTests;
begin
  var x: Elements.RTL.Delphi.DelphiString := '';
  x := 'String in the street';
  Assert.AreEqual(x.StartsWith('String'), true, 'StartsWith 1');
  Assert.AreEqual(x.StartsWith('string'), false, 'StartsWith');
  Assert.AreEqual(x.StartsWith('string', true), true, 'StartsWith 3');
  x := 'string';
  Assert.AreEqual(x.StartsWith('string'), true, 'StartsWith 4');
  //Assert.AreEqual(x.StartsWith('s'), true, 'StartsWith 5'); // Stack overflow
end;

method StringManipulations.EndsWithTests;
begin
  var x: Elements.RTL.Delphi.DelphiString := '';
  x := 'This is an string';
  Assert.AreEqual(x.EndsWith('string'), true, 'EndsWith 1');
 // Assert.AreEqual(x.EndsWith('g'), true, 'EndsWith 2'); // Stack overflow
  Assert.AreEqual(x.EndsWith('String'), False, 'EndsWith 3');
  Assert.AreEqual(x.EndsWith('sTRING', true), true, 'EndsWith 4');
  Assert.AreEqual(x.EndsWith('sTRING', false), false, 'EndsWith 5');
  x := 'This';
  Assert.AreEqual(x.EndsWith('This'), true, 'EndsWith 6');
end;




end.
