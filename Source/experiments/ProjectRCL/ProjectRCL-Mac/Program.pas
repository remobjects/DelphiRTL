namespace ProjectRCLMac;

interface

uses
  Foundation, RemObjects.Elements.RTL.Delphi, RemObjects.Elements.RTL.Delphi.VCL;

type
  Program = public static class
  public
    method Main(aArguments: array of String): Int32;
  end;

implementation

method Program.Main(aArguments: array of String): Int32;
begin
  Application := new TApplication(nil);
  Application.Initialize;
  Application.CreateForm(typeOf(TForm6), var Form6);
  Application.Run;
  writeLn('The magic happens here.');
end;

end.