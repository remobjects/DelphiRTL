namespace ProjectRCLWebAssembly;

uses
  RemObjects.Elements.RTL.Delphi, RemObjects.Elements.RTL.Delphi.VCL;

type
  [Export]
  Program = public class
  public

    method HelloWorld;
    begin
      writeLn('HelloWorld');
      var el := WebAssembly.GetElementById('helloWorld');
      if el = nil then begin
        writeLn('Element by ID test is null!');
        exit;
      end;
      var t2 := WebAssembly.CreateTextNode('Hello from Elements WebAssembly!');
      el.appendChild(t2);

      Application.Initialize;
      //Application.MainFormOnTaskbar := True;
      Application.CreateForm(typeOf(TForm6), var Form6);
      Application.Run;
    end;

  end;

end.