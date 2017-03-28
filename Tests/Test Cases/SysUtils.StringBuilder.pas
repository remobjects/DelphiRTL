namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi;

type
  StringBuilderUsage = public class(Test)
  private
    fData: TStringBuilder;
  public
    method Setup; override;
    begin
      fData := new TStringBuilder();
    end;

    method AppendTests;
    begin
      fData.Append(1);
      Assert.AreEqual(fData[0], '1');

      fData.Append(3.0);
      Assert.AreEqual(fData[1], '3');
    end;

    method Append2Tests;
    begin
      fData.Append('C');
      Assert.AreEqual(fData[0], 'C');

      fData.Append('Another');
      Assert.AreEqual(fData[1], 'A');
      Assert.AreEqual(fData.ToString(1, 7), 'Another');
    end;

    method Append3Tests;
    begin
      fData.Append(true);
      Assert.AreEqual(fData.ToString, 'True');

      fData.Clear;
      fData.Append('X', 4);
      Assert.AreEqual(fData.ToString, 'XXXX');

      fData.Clear;
      var lArray := ['A', 'B', 'C'];
      fData.Append(lArray, 0, 3);
      Assert.AreEqual(fData.ToString, 'ABC');
    end;

    method ClearTests;
    begin
      fData.Append('ComeOn');
      Assert.AreEqual(fData.Length > 0, true);

      fData.Clear;
      Assert.AreEqual(fData.Length, 0);
    end;

    method AppendLineTests;
    begin
      fData.AppendLine('One Line');
      Assert.AreEqual(fData.Chars[8], #13);
    end;

    method CopyToTests;
    begin
      var lArray := new Char[11];
      fData.Append('one line');
      fData.CopyTo(0, lArray, 0, fData.Length);
      Assert.AreEqual(new String(lArray, 0, 8), 'one line');

      fData.Append('YYY');
      fData.CopyTo(8, lArray, 8, 3);
      Assert.AreEqual(new String(lArray), 'one lineYYY');
    end;

    method EqualsTests;
    begin
      fData.Append('One single line');
      var lAnother := new TStringBuilder('One single line');
      var lAnotherOne := new TStringBuilder('One line');

      Assert.AreEqual(fData.Equals(lAnother), true);
      Assert.AreEqual(fData.Equals(lAnotherOne), false);
    end;

    method InsertTests;
    begin
      fData.Append('0123456789');
      fData.Insert(1, 'A');
      fData.Insert(2, 0);
      fData.Insert(3, 'ComeOn');
      fData.Insert(9, 3.14);
      Assert.AreEqual(fData.toString, '0A0ComeOn3.14123456789');
    end;

    method RemoveTests;
    begin
      fData.Append('Test string');
      fData.Remove(1, 3);
      Assert.AreEqual(fData.toString, 'T string');
      fData.Remove(0, 1);
      Assert.AreEqual(fData.toString, ' string');
    end;

    method ReplaceTests;
    begin
      fData.Append('Test string');
      fData.Replace('T', 'X');
      Assert.AreEqual(fData.toString, 'Xest string');

      fData.Replace('string', 'procedure');
      Assert.AreEqual(fData.toString, 'Xest procedure');

      fData.Clear;
      fData.Append('test test test');
      fData.Replace('test', 'change', 0, 4);
      Assert.AreEqual(fData.toString, 'change test test');

      fData.Replace('e', 'x', 7, 8);
      Assert.AreEqual(fData.toString, 'change txst txst');
    end;

    method ToStringTests;
    begin
      fData.Append('0123456789');
      Assert.AreEqual(fData.ToString(3, 2), '34');
      Assert.AreEqual(fData.ToString, '0123456789');
    end;
  end;
end.