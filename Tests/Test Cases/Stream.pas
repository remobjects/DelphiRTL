namespace DelphiRTL.Tests.Shared.Test_Cases;

uses
  RemObjects.Elements.EUnit,
  RemObjects.Elements.RTL.Delphi;

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

      var lTmp := lStream.Read(var lToRead, 1);
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

    /*method FileStreamTests;
    begin
      var lPath := Environment.UserHomeFolder + '\tests';
      writeLn(lPath);
      var lStream := new FileStream(lPath, FileOpenMode.Create);      
      var lArray := new Byte[5];
      var lToRead := new Byte[5];
      lArray := [9, 8, 7, 6, 5];
      Assert.AreEqual(lStream.Write(lArray, 5), 5);
      lStream.Close;
      lStream := new FileStream(lPath, FileOpenMode.ReadOnly);

      var lTmp := lStream.Read(lToRead, 1);
      Assert.AreEqual(lStream.Position, 1);
      Assert.AreEqual(lToRead[0], 9);
      lStream.Read(lToRead, 4);
      Assert.AreEqual(lToRead[0], 8);
      
      var lNewStream := new MemoryStream(5);
      lStream.CopyTo(lNewStream);
      
      lStream.Seek(2, SeekOrigin.Begin);
      Assert.AreEqual(lStream.Position, 2);

      lStream.Close;
      File.Delete(lPath);
    end;*/
  end;    

end.
