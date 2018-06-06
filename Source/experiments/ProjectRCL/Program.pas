namespace ProjectRCL;

uses
  RemObjects.Elements.RTL.Delphi;

type

  Program = class
  public
    class method Main(args: array of String): Int32;
    begin
      /*var lDfm := new TFileStream('c:\dev\ro\Unit6.dfm', fmOpenRead);
      var lRes := new TFileStream('C:\Users\Diego\Documents\ok.res', fmCreate or fmOpenWrite);
      var lConverter := new ObjectConverter(lDfm, lRes);
      lConverter.ToBinary;*/

      // this is the default VCL prject code
      Application.Initialize;
      //Application.MainFormOnTaskbar := True;
      Application.CreateForm(typeOf(TForm7), var Form45);
      Application.Run;


    end;

  end;

var
  //Form45: TForm55;
  Form45: TComponent;

end.