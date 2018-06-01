namespace ProjectRCL;

interface

uses
  RemObjects.Elements.RTL.Delphi;

type
  TForm6 = public class(TForm)
    //Button1: TButton;
  private
  { Private declarations }
  public
  { Public declarations }
    button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  ShowMessage('Clicked!');
end;

end.