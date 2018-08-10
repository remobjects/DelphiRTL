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
      /*var lDfm := new TFileStream('C:\dev\ro\DelphiRTL\Source\experiments\ProjectRCL\Unit2-strings.dfm', fmOpenRead);
      var lRes := new TFileStream('C:\dev\ro\DelphiRTL\Source\experiments\ProjectRCL\Unit2-strings.res', fmCreate or fmOpenWrite);
      var lConverter := new ObjectConverter(lDfm, lRes);
      lConverter.ToBinary;*/

      // this is the default VCL prject code

      //Check(TForm7);

      Application := new TApplication(nil);
      Application.Initialize;
      //Application.MainFormOnTaskbar := True;
      Application.CreateForm(typeOf(TForm6), var Form6);
      Form6.comboBox1.Items.Add('Vamos 1');
      Form6.comboBox1.Items.Add('Vamos 2');
      Application.Run;
    end;
  end;

//var
  //Form45: TComponent;
  //Form45: TForm6;

end.