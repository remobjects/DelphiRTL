namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

type
  SysUtilsUsage = public class(Test)    
  public
    method FileOpenTests;
    begin
      var lFile := FileOpen('..\..\Test.INI', fmOpenRead);
      Assert.AreEqual(lFile > 0, true);
      FileClose(lFile);
    end;

    method FileReadTests;
    begin
      var lFile := FileOpen('..\..\Test.INI', fmOpenRead);
      var lArray := new Byte[1024];
      var lRes := FileRead(lFile, var lArray, 0, 1024);
      Assert.AreEqual(lRes, 907);
    end;

    method FileCloseTests;
    begin
      var lFile := FileOpen('..\..\Test.INI', fmOpenRead);
      Assert.AreEqual(lFile > 0, true);
      FileClose(lFile);
    end;

    method FileSeekTests;
    begin
      var lFile := FileOpen('..\..\Test.INI', fmOpenRead);
      var lSeek := FileSeek(lFile, 100, 0);
      Assert.AreEqual(lSeek, 100);
      var lArray := new Byte[1024];
      var lRes := FileRead(lFile, var lArray, 0, 1024);
      Assert.AreEqual(lRes, 807);
      FileClose(lFile);
    end;

    method FileExistsTests;
    begin
      Assert.AreEqual(FileExists('..\..\Test.INI'), true);
      Assert.AreEqual(FileExists('..\..\Test1asd.INI'), false);
    end;

    method DirectoryExistsTests;
    begin
      Assert.AreEqual(DirectoryExists('c:\'), true);
      Assert.AreEqual(DirectoryExists('..\..\Testssss\'), false);
    end;

    method ExtractFileExtTests;
    begin
      var lFile := ExtractFileExt('c:\sample\test.ini');
      Assert.AreEqual(lFile, '.ini');

      lFile := ExtractFileExt('c:\test.ini');
      Assert.AreEqual(lFile, '.ini');
    end;

    method ExtractFileNameTests;
    begin
      var lFile := ExtractFileName('c:\sample\test.ini');
      Assert.AreEqual(lFile, 'test.ini');

      lFile := ExtractFileName('c:\test.ini');
      Assert.AreEqual(lFile, 'test.ini');

      lFile := ExtractFileName('c:\test');
      Assert.AreEqual(lFile, 'test');
    end;

    method ChangeFileExtTests;
    begin
      var lFile := ChangeFileExt('c:\sample\test.ini', '.bat');
      Assert.AreEqual(lFile, 'c:\sample\test.bat');

      lFile := ChangeFileExt('c:\test.ini', '.bat');
      Assert.AreEqual(lFile, 'c:\test.bat');
    end;
  end;

end.
