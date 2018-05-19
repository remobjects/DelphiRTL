namespace ProjectRCL;

uses
  RemObjects.Elements.RTL.Delphi;

type
  TForm55 = public class(TForm)
      //Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    {constructor (aOwner: TComponent);
    begin
      writeLn('here..');
    end;}
  end;


  Program = class
  public
    class method Main(args: array of String): Int32;
    begin
      // this is the default VCL prject code
      Application.Initialize;
      //Application.MainFormOnTaskbar := True;
      Application.CreateForm(typeOf(TForm55), var Form45);
      Application.Run;
    end;

  end;

var
  //Form45: TForm55;
  Form45: TComponent;

end.