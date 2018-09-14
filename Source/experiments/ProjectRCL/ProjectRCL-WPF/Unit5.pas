namespace ProjectRCLWPF;

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
    button2: TButton;
    edit1: TEdit;
    label1: TLabel;
    radioButton1: TRadioButton;
    CheckBox1: TCheckBox;
    listBox1: TListBox;
    comboBox1: TComboBox;
    groupBox1: TGroupBox;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var key: Char);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  //ShowMessage('WPF!!');
  //label1.Width := 200;
  //label1.Caption := 'All right!';
  //ShowMessage('Clicked!');
  //ShowMessage(edit1.Text);

  listBox1.Items.Add(edit1.Text);
  comboBox1.Items.Add(edit1.Text);
  edit1.OnKeyPress := @Edit1OnKeyPress;
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  edit1.Text := 'Blah!';
end;

procedure TForm6.ComboBox1Select(Sender: TObject);
begin
  ShowMessage('Yes!');
end;

procedure TForm6.Edit1KeyPress(Sender: TObject; var key: Char);
begin
  ShowMessage(key);
end;

end.