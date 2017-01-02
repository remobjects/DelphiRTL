namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi;

type
  IniFilesUsage = public class(Test)
  private
    fData: TIniFile;
  public
    method Setup; override;
    begin
      {$IF COOPER} // TODO
      fData := new TIniFile('..\..\Test.INI',  Encoding.Default, false);
      {$ELSE}
      fData := new TIniFile('..\..\Test.INI', TEncoding.Default, false);
      {$ENDIF}
    end;

    method ReadSectionsTests;
    begin
      var lValues := TStringList.Create;
      fData.ReadSections(lValues);
      Assert.AreEqual(lValues.Count, 9);
    end;

    method ReadSectionTests;
    begin
      var lValues := TStringList.Create;
      fData.ReadSection('FilePos', lValues);
      Assert.AreEqual(lValues.Count > 0, true);
    end;

    method SectionExistsTests;
    begin
      Assert.AreEqual(fData.SectionExists('Config'), true);
      Assert.AreEqual(fData.SectionExists('Section1'), false);
    end;

    method ReadSectionValuesTests;
    begin
      var lValues := TStringList.Create;
      fData.ReadSectionValues('Config', lValues);
      Assert.AreEqual(lValues.Count > 0, true);
    end;

    method EraseSectionTests;
    begin
      Assert.AreEqual(fData.SectionExists('Config'), true);
      fData.EraseSection('Config');
      Assert.AreEqual(fData.SectionExists('Config'), false);
    end;

    method DeleteKeyTests;
    begin
      var lValues := TStringList.Create;
      fData.ReadSection('Config', lValues);
      Assert.AreEqual(lValues.IndexOf('Language') >= 0, true);

      fData.DeleteKey('Config', 'Language');

      fData.ReadSection('Config', lValues);
      Assert.AreEqual(lValues.IndexOf('Language'), -1);
    end;

    method ValueExistsTests;
    begin
      Assert.AreEqual(fData.ValueExists('Config', 'Language'), true);
      Assert.AreEqual(fData.ValueExists('ConfigNo', 'Language'), false);
      Assert.AreEqual(fData.ValueExists('Config', 'NoLanguage'), false);
    end;

    method ReadStringTests;
    begin
      Assert.AreEqual(fData.ReadString('Config', 'Language', 'No'), 'ENGLISH');
      Assert.AreEqual(fData.ReadString('ConfigNo', 'Language', 'No'), 'No');
      Assert.AreEqual(fData.ReadString('Config', 'LanguageNo', 'No'), 'No');
    end;

    method ReadIntegerTests;
    begin
      Assert.AreEqual(fData.ReadInteger('FilePos', 'Width', -1), 1860);
      Assert.AreEqual(fData.ReadInteger('FilePosNo', 'Width', -1), -1);
      Assert.AreEqual(fData.ReadInteger('FilePos', 'WidthNo', -1), -1);
    end;

    method ReadBooleanTests;
    begin
      Assert.AreEqual(fData.ReadBool('FilePos', 'AutoSavePos', false), true);
      Assert.AreEqual(fData.ReadBool('FilePosNo', 'Width', false), false);
      Assert.AreEqual(fData.ReadBool('FilePos', 'WidthNo', false), false);
    end;

    method WriteStringTests;
    begin
      fData.WriteString('Config', 'Data', 'More');
      Assert.AreEqual(fData.ReadString('Config', 'Data', 'No'), 'More');
    end;

    method WriteIntegerTests;
    begin
      fData.WriteInteger('Config', 'Data', 100);
      Assert.AreEqual(fData.ReadInteger('Config', 'Data', -1), 100);
    end;

    method WriteBooleanTests;
    begin
      fData.WriteBool('Config', 'Data', true);
      Assert.AreEqual(fData.ReadBool('Config', 'Data', false), true);
    end;
  end;

end.
