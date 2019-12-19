namespace ProjectRCLIslandLinux;

interface

uses
  RemObjects.Elements.RTL.Delphi, RemObjects.Elements.RTL.Delphi.VCL;

type
  TForm6 = public class(TForm)
    //Button1: TButton;
  private
  { Private declarations }
  public
  { Public declarations }
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  writeLn('Yes????');
  ShowMessage("ok from ShowMessage");
end;

end.