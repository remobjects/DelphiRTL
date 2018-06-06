namespace ProjectRCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TForm7 = public class(TForm)
    //Button1: TButton;
  private
  { Private declarations }
  public
  { Public declarations }
    button1: TButton;
    edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  end;

  var
    Form7: TForm7;

implementation

procedure TForm7.Button1Click(Sender: TObject);
begin
  ShowMessage('Clicked!');
end;

end.