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
    edit1: TEdit;
    label1: TLabel;
    radioButton1: TRadioButton;
    checkBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  label1.Width := 200;
  label1.Caption := 'All right!';
  ShowMessage('Clicked!');
end;

end.