namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi,
  RemObjects.Elements.RTL;

type
  SysUtilsUsage = public class(Test)
  private
    fTestPath: DelphiString;
  public
    method Setup; override;
    begin
      fTestPath := Path.Combine(Environment.TempFolder, 'Test.INI');      
      var lContent := new TFileStream(fTestPath, fmCreate or fmOpenWrite);
      var lData: Integer := 100;
      for i: Integer := 0 to 249 do
        lContent.WriteData(lData);
      lContent.Close;
    end;

    method Teardown; override;
    begin
      DeleteFile(fTestPath);
    end;

    method FileOpenTests;
    begin
      var lFile := FileOpen(fTestPath, fmOpenRead);
      Assert.AreEqual(lFile > 0, true);
      FileClose(lFile);
    end;

    method FileReadTests;
    begin
      var lFile := FileOpen(fTestPath, fmOpenRead);
      var lArray := new Byte[1024];
      var lRes := FileRead(lFile, var lArray, 0, 1024);
      Assert.AreEqual(lRes, 1000);
      FileClose(lFile);
    end;

    method FileCloseTests;
    begin
      var lFile := FileOpen(fTestPath, fmOpenRead);
      Assert.AreEqual(lFile > 0, true);
      FileClose(lFile);
    end;

    method FileSeekTests;
    begin
      var lFile := FileOpen(fTestPath, fmOpenRead);
      var lSeek := FileSeek(lFile, 100, 0);
      Assert.AreEqual(lSeek, 100);
      var lArray := new Byte[1024];
      var lRes := FileRead(lFile, var lArray, 0, 1024);
      Assert.AreEqual(lRes, 900);
      FileClose(lFile);
    end;

    method FileExistsTests;
    begin
      Assert.AreEqual(FileExists(fTestPath), true);
      Assert.AreEqual(FileExists('..\..\Test1asd.INI'), false);
    end;

    method DirectoryExistsTests;
    begin
      Assert.AreEqual(DirectoryExists(Environment.TempFolder), true);
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
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        var lFile := ExtractFileName('c:\sample\test.ini');
        Assert.AreEqual(lFile, 'test.ini');

        lFile := ExtractFileName('c:\test.ini');
        Assert.AreEqual(lFile, 'test.ini');

        lFile := ExtractFileName('c:\test');
        Assert.AreEqual(lFile, 'test');
      end;
    end;

    method ChangeFileExtTests;
    begin
      var lFile := ChangeFileExt('c:\sample\test.ini', '.bat');
      Assert.AreEqual(lFile, 'c:\sample\test.bat');

      lFile := ChangeFileExt('c:\test.ini', '.bat');
      Assert.AreEqual(lFile, 'c:\test.bat');
    end;

    method ChangeFilePathTests;
    begin
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        var lNew := ChangeFilePath('c:\one\two\test.txt', 'c:\three');
        Assert.AreEqual(lNew, 'c:\three\test.txt');
      end;
    end;

    method ExtractFilePathTests;
    begin
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        var lNew := ExtractFilePath('c:\onedir\test.txt');
        Assert.AreEqual('c:\onedir\', lNew);
      end;
    end;

    method ExtractFileDirTests;
    begin
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        var lNew := ExtractFileDir('c:\onedir\test.txt');
        Assert.AreEqual('c:\onedir', lNew);
      end;
    end;

    method ExtractFileDriveTests;
    begin
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        var lNew := ExtractFileDrive('c:\onedir\test.txt');
        Assert.AreEqual('c:', lNew);
      end;
    end;

    method IsDelimiterTests;
    begin
      Assert.IsTrue(IsDelimiter('|#', '#ComeOn', 0));
      Assert.IsFalse(IsDelimiter('|#', '#ComeOn', 1));
    end;

    method LastDelimiterTests;
    begin
      Assert.AreEqual(LastDelimiter('|#', '#Come|On#'), 8);
      Assert.AreEqual(LastDelimiter('|#', '#Come|On'), 5);
      Assert.AreEqual(LastDelimiter('|#', '#ComeOn'), 0);
      Assert.AreEqual(LastDelimiter('|#', 'ComeOn'), -1);
    end;

    method IncludeTrailingPathDelimiterTests;
    begin
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        var lNew := IncludeTrailingPathDelimiter('c:\test');
        Assert.AreEqual(lNew, 'c:\test\');
        lNew := IncludeTrailingPathDelimiter('c:\test\');
        Assert.AreEqual(lNew, 'c:\test\');
      end;
    end;

    method ExcludeTrailingPathDelimiterTests;
    begin
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        var lNew := ExcludeTrailingPathDelimiter('c:\test\');
        Assert.AreEqual(lNew, 'c:\test');
        lNew := ExcludeTrailingPathDelimiter('c:\test');
        Assert.AreEqual(lNew, 'c:\test')
      end;
    end;

    method IsRelativePathTests;
    begin
      if TOSVersion.Platform = TPlatform.pfWindows then begin
        Assert.IsTrue(IsRelativePath('..\test\'));
        Assert.IsFalse(IsRelativePath('c:\test'));
      end;
    end;
  end;

end.