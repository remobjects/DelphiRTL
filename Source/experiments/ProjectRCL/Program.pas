namespace RemObjects.Elements.RTL.Delphi;

uses
  RemObjects.Elements.RTL.Delphi;

type
  TForm55 = public class(TForm)
      //Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  Program = class
  public
    class var Form55: TForm55;
    class method Main(args: array of String): Int32;
    begin
      // this is the default VCL prject code
      Application.Initialize;
      //Application.MainFormOnTaskbar := True;
      Application.CreateForm(typeOf(TForm55), var Form55);
      Application.Run;
    end;

  end;

end.