namespace ProjectRCL;

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
    button1: TButton;
    button2: TButton;
    edit1: TEdit;
    label1: TLabel;
    radioButton1: TRadioButton;
    checkBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  //label1.Width := 200;
  //label1.Caption := 'All right!';
  //ShowMessage('Clicked!');
  ShowMessage(edit1.Text);
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  edit1.Text := 'Blah!';
end;


end.