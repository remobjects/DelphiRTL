﻿namespace ProjectRCLWPF;

uses
  RemObjects.Elements.RTL.Delphi, RemObjects.Elements.RTL.Delphi.VCL;

type
  Program = class
  public
    [STAThread]
    class method Main(args: array of PlatformString): Int32;
    begin

      Application := new TApplication(nil);
      Application.Initialize;
      Application.CreateForm(typeOf(TForm6), var Form6);
      Application.Run;
      writeLn('The magic happens here.');


      {Application := new TApplication(nil);
      Application.Initialize;
      var lForm := new TForm(nil);

      Application.Run;
      writeLn('The magic happens here.');
      }

    end;
  end;

end.