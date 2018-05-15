namespace ProjectRCL;

type
  Program = class
  public

    class method Main(args: array of String): Int32;
    begin
      // this is the default VCL prject code
      Application.Initialize;
      //Application.MainFormOnTaskbar := True;
      //Application.CreateForm(TForm5, var Form5);
      Application.Run;
    end;

  end;

end.