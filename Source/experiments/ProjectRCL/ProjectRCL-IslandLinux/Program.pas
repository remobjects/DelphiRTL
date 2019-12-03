namespace ProjectRCLIslandLinux;

interface

uses
  RemObjects.Elements.RTL.Delphi, RemObjects.Elements.RTL.Delphi.VCL;

type
  Program = public static class
  public
    method Main(aArguments: array of String): Int32;
  end;

implementation

method Program.Main(aArguments: array of String): Int32;
begin
  writeLn('yes? 1');
  Application := new TApplication(nil);
  writeLn('yes? 2');
  Application.Initialize;
  writeLn('yes? 3');
  Application.CreateForm(typeOf(TForm6), var Form6);
  writeLn('yes? 4');
  Application.Run;
  writeLn('The magic happens here.');
  //var x: RemObjects.Elements.RTL.Delphi.VCL.TButton;
end;

end.