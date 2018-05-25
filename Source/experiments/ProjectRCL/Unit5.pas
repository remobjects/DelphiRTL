namespace ProjectRCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TForm5 = public class(TForm)
    //Button1: TButton;
  private
  { Private declarations }
  public
  { Public declarations }
    button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

  var
    Form5: TForm5;

implementation

procedure TForm5.Button1Click(Sender: TObject);
begin
  ShowMessage('Clicked!');
end;

end.