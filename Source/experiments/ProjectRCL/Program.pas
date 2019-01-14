namespace ProjectRCL;

uses
  RemObjects.Elements.RTL.Delphi, RemObjects.Elements.RTL.Delphi.VCL;

type

  TForm7 = public class(TForm6)
  end;

  TSicario = class of TForm6;

  Program = class
  public
    class method Check(aValue: TSicario);
    begin
      writeLn(aValue.ActualType.Name);
    end;
    class method Main(args: array of String): Int32;
    begin
      {var lDfm := new TFileStream('C:\dev\ro\DelphiRTL\Source\experiments\ProjectRCL\withListItems.dfm', fmOpenRead);
      var lRes := new TFileStream('C:\dev\ro\DelphiRTL\Source\experiments\ProjectRCL\ComeOnListItems.res', fmCreate or fmOpenWrite);
      var lConverter := new ObjectConverter(lDfm, lRes);
      writeLn('To convert...');
      lConverter.ToBinary;
      writeLn('Converted!');}

      // this is the default VCL prject code

      {Application := new TApplication(nil);
      Application.Initialize;
      var lForm := new TForm(nil);
      var lButton := new TButton(lForm);
      lButton.Left := 100;
      lButton.Top := 100;
      lButton.Width := 80;
      lButton.Height := 40;
      lButton.Parent := lForm;
      //Application.MainFormOnTaskbar := True;
      //Application.CreateForm(typeOf(TForm6), var Form6);
      Application.Run;}



      Application := new TApplication(nil);
      Application.Initialize;
      //Application.MainFormOnTaskbar := True;
      Application.CreateForm(typeOf(TForm6), var Form6);
      Application.Run;
    end;
  end;
end.