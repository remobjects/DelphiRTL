namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi,
  RemObjects.Elements.RTL;

type
  StreamUsage = public class(Test)
  public
    method MemoryStreamTests;
    begin
      var lStream := TMemoryStream.Create;
      var lArray := new Byte[5];
      var lToRead := new Byte[5];
      lArray := [9, 8, 7, 6, 5];
      Assert.AreEqual(lStream.Write(lArray, 5), 5);

      lStream.Position := 0;
      Assert.AreEqual(lStream.Position, 0);

      lStream.Read(var lToRead, 1);
      Assert.AreEqual(lStream.Position, 1);
      Assert.AreEqual(lToRead[0], 9);
      lStream.Read(var lToRead, 4);
      Assert.AreEqual(lToRead[0], 8);

      var lNewStream := TMemoryStream.Create;
      lNewStream.Size := 5;
      lNewStream.CopyFrom(lStream, lStream.Size);

      lStream.Seek(2, TSeekOrigin.soBeginning);
      Assert.AreEqual(lStream.Position, 2);
    end;

    method FileStreamTests;
    begin
      var lPath := Path.Combine(Environment.TempFolder, 'Tests');
      var lStream := TFileStream.Create(lPath, fmCreate);
      var lArray := new Byte[5];
      var lToRead := new Byte[5];
      lArray := [9, 8, 7, 6, 5];
      Assert.AreEqual(lStream.Write(lArray, 5), 5);
      lStream.Close;
      lStream := TFileStream.Create(lPath, fmOpenRead);

      lStream.Read(var lToRead, 1);
      Assert.AreEqual(lStream.Position, 1);
      Assert.AreEqual(lToRead[0], 9);
      lStream.Read(var lToRead, 4);
      Assert.AreEqual(lToRead[0], 8);

      var lNewStream := TMemoryStream.Create;
      lNewStream.Size := 5;
      lNewStream.CopyFrom(lStream, lStream.Size);

      lStream.Seek(2, TSeekOrigin.soBeginning);
      Assert.AreEqual(lStream.Position, 2);

      lStream.Close;
      DeleteFile(lPath);
    end;

    method StreamFunctionsTests;
    begin
      var lStream := TMemoryStream.Create;

      var lInteger: Integer := 1234567890;
      lStream.WriteData(lInteger);
      lStream.Position := 0;
      var lInt2: Integer;
      lStream.ReadData(var lInt2);
      Assert.AreEqual(lInteger, lInt2);
      var lOldPos: Int64;

      {$IF NOT COOPER}
      lOldPos := lStream.Position;
      var lShortInt: ShortInt := 27;
      lStream.WriteData(lShortInt);
      lStream.Position := lOldPos;
      var lShortInt2: ShortInt;
      lStream.ReadData(var lShortInt2);
      Assert.AreEqual(lShortInt, lShortInt2);

      lOldPos := lStream.Position;
      var lByte: Byte := 191;
      lStream.WriteData(lByte);
      lStream.Position := lOldPos;
      var lByte2: Byte;
      lStream.ReadData(var lByte2);
      Assert.AreEqual(lByte, lByte2);

      lOldPos := lStream.Position;
      var lChar: Char := 'C';
      lStream.WriteData(lChar);
      lStream.Position := lOldPos;
      var lChar2: Char;
      lStream.ReadData(var lChar2);
      Assert.AreEqual(lChar, lChar2);

      lOldPos := lStream.Position;
      var lBool: Boolean := true;
      lStream.WriteData(lBool);
      lStream.Position := lOldPos;
      var lBool2: Boolean;
      lStream.ReadData(var lBool2);
      Assert.AreEqual(lBool, lBool2);

      lOldPos := lStream.Position;
      var lInt16: Int16 := 23456;
      lStream.WriteData(lInt16);
      lStream.Position := lOldPos;
      var lInt162: Int16;
      lStream.ReadData(var lInt162);
      Assert.AreEqual(lInt16, lInt162);

      lOldPos := lStream.Position;
      var lInt32: Int32 := -235173;
      lStream.WriteData(lInt32);
      lStream.Position := lOldPos;
      var lInt322: Int32;
      lStream.ReadData(var lInt322);
      Assert.AreEqual(lInt32, lInt322);

      lOldPos := lStream.Position;
      var lInt64: Int64 := 123456712114890;
      lStream.WriteData(lInt64);
      lStream.Position := lOldPos;
      var lInt642: Int64;
      lStream.ReadData(var lInt642);
      Assert.AreEqual(lInt64, lInt642);

      lOldPos := lStream.Position;
      var lUInt16: UInt16 := 52345;
      lStream.WriteData(lUInt16);
      lStream.Position := lOldPos;
      var lUInt162: UInt16;
      lStream.ReadData(var lUInt162);
      Assert.AreEqual(lUInt16, lUInt162);

      lOldPos := lStream.Position;
      var lUInt32: UInt32 := 52345;
      lStream.WriteData(lUInt32);
      lStream.Position := lOldPos;
      var lUInt322: UInt32;
      lStream.ReadData(var lUInt322);
      Assert.AreEqual(lUInt32, lUInt322);

      lOldPos := lStream.Position;
      var lUInt64: UInt64 := 1234567890;
      lStream.WriteData(lUInt64);
      lStream.Position := lOldPos;
      var lUInt642: UInt64;
      lStream.ReadData(var lUInt642);
      Assert.AreEqual(lUInt64, lUInt642);
      {$ENDIF}

      lOldPos := lStream.Position;
      var lAnotherInt: Integer := 134567890;
      lStream.WriteData(lAnotherInt);
      lStream.Position := lOldPos;
      var lAnotherInt2: Integer;
      lStream.ReadBufferData(var lAnotherInt2, sizeOf(lAnotherInt2));
      Assert.AreEqual(lAnotherInt, lAnotherInt2);

      {$IF NOT COOPER}
      lOldPos := lStream.Position;
      var lAnotherBool: Boolean := true;
      lStream.WriteData(lAnotherBool);
      lStream.Position := lOldPos;
      var lAnotherBool2: Boolean;
      lStream.ReadBufferData(var lAnotherBool2, sizeOf(lAnotherBool2));
      Assert.AreEqual(lAnotherBool, lAnotherBool2);

      lOldPos := lStream.Position;
      var lAnotherChar: Char := 'x';
      lStream.WriteData(lAnotherChar);
      lStream.Position := lOldPos;
      var lAnotherChar2: Char;
      lStream.ReadBufferData(var lAnotherChar2, sizeOf(lAnotherChar2));
      Assert.AreEqual(lAnotherChar, lAnotherChar2);

      lOldPos := lStream.Position;
      var lAnotherByte: Byte := 87;
      lStream.WriteData(lAnotherByte);
      lStream.Position := lOldPos;
      var lAnotherByte2: Byte;
      lStream.ReadBufferData(var lAnotherByte2, sizeOf(lAnotherByte2));
      Assert.AreEqual(lAnotherByte, lAnotherByte2);

      lOldPos := lStream.Position;
      var lAnotherInt16: Int16 := -901;
      lStream.WriteData(lAnotherInt16);
      lStream.Position := lOldPos;
      var lAnotherInt162: Int16;
      lStream.ReadBufferData(var lAnotherInt162, sizeOf(lAnotherInt162));
      Assert.AreEqual(lAnotherInt16, lAnotherInt162);

      lOldPos := lStream.Position;
      var lAnotherUInt16: UInt16 := 981;
      lStream.WriteData(lAnotherUInt16);
      lStream.Position := lOldPos;
      var lAnotherUInt162: UInt16;
      lStream.ReadBufferData(var lAnotherUInt162, sizeOf(lAnotherUInt162));
      Assert.AreEqual(lAnotherUInt16, lAnotherUInt162);

      lOldPos := lStream.Position;
      var lAnotherSingle: Single := 981.2;
      lStream.WriteData(lAnotherSingle);
      lStream.Position := lOldPos;
      var lAnotherSingle2: Single;
      lStream.ReadBufferData(var lAnotherSingle2, sizeOf(lAnotherSingle2));
      Assert.AreEqual(lAnotherSingle, lAnotherSingle2);

      lOldPos := lStream.Position;
      var lAnotherDouble: Double := -9831.45;
      lStream.WriteData(lAnotherDouble);
      lStream.Position := lOldPos;
      var lAnotherDouble2: Double;
      lStream.ReadBufferData(var lAnotherDouble2, sizeOf(lAnotherDouble2));
      Assert.AreEqual(lAnotherDouble, lAnotherDouble2);
      {$ENDIF}
    end;

    method StreamTextTests;
    begin
      var lMemStream := new TMemoryStream();
      lMemStream.WriteString('Testing TextStream');
      
      lMemStream.Position := 0;
      var lString := lMemStream.ReadString(36);
      Assert.AreEqual(lString, 'Testing TextStream');
    end;
  end;

end.