namespace DelphiRTL.Tests.Shared.Test_Cases;

{$IF ISLAND AND WINDOWS}

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi;

type
  SystemRegistryUsage = public class(Test)
  private
    fReg: TRegistry;
  public
    method Setup; override;
    begin
      fReg := new TRegistry();
    end;

    method OpenKeyTests;
    begin
      Assert.AreEqual(fReg.OpenKey('Software\Microsoft\Windows\CurrentVersion', false), true);
      fReg.CloseKey;      
    end;

    method CreateKeyExistsReadWriteTests;
    begin
      Assert.AreEqual(fReg.CreateKey('Software\RemObjectsTests'), true);
      Assert.AreEqual(fReg.KeyExists('Software\RemObjectsTests'), true);
      Assert.AreEqual(fReg.OpenKey('Software\RemObjectsTests', false), true);

      fReg.WriteInteger('Check integer', 123);
      fReg.WriteString('Check string', 'Come on!');
      fReg.WriteBool('Check boolean', true);
      fReg.WriteFloat('Check float', 8.0);

      Assert.AreEqual(fReg.ReadInteger('Check integer'), 123);
      Assert.AreEqual(fReg.ReadString('Check string'), 'Come on!');
      Assert.AreEqual(fReg.ReadBool('Check boolean'), true);
      Assert.AreEqual(fReg.ReadFloat('Check float'), 8.0);

      var lValues := TStringList.Create;
      fReg.GetValueNames(lValues);
      Assert.IsTrue(lValues.Count > 0);
      Assert.IsTrue(lValues.IndexOf('Check integer') >= 0);
           
      fReg.CloseKey;
      Assert.AreEqual(fReg.DeleteKey('Software\RemObjectsTests'), true);
      Assert.AreEqual(fReg.KeyExists('Software\RemObjectsTests'), false);      
    end;

    method EnumKeysTests;
    begin
      Assert.IsTrue(fReg.OpenKey('Software\Microsoft\Windows\CurrentVersion', false));
      Assert.IsTrue(fReg.HasSubKeys);
      var lKeys := TStringList.Create;
      fReg.GetKeyNames(lKeys);
      Assert.IsTrue(lKeys.Count > 0);
      fReg.CloseKey;      
    end;
  end;

{$ENDIF}

end.