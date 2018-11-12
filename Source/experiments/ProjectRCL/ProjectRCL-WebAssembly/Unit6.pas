namespace ProjectRCLWebAssembly;

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
    listbox1: TListBox;
    comboBox1: TComboBox;
    panel1: TPanel;
    groupbox1: TGroupBox;

    panel2: TPanel;
    label2: TLabel;
    label3: TLabel;
    label4: TLabel;
    label5: TLabel;
    label6: TLabel;
    checkBox2: TCheckBox;
    checkBox3: TCheckBox;
    checkBox4: TCheckBox;
    edit2: TEdit;
    edit3: TEdit;

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  //label1.Width := 200;
  //label1.Caption := 'All right!';
  //button1.Caption := 'Yes!!';
  listbox1.Items.Add(edit1.Text);
  comboBox1.Items.Add(edit1.Text);
  if Application.MainForm = nil then
    writeLn('MainForm nil')
  else
    writeLn('MainForm NO nil');

  ShowMessage('Clicked!');
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  comboBox1.Items.Add(edit1.Text);
end;

procedure TForm6.ComboBox1Select(Sender: TObject);
begin
  ShowMessage('Yes!');
end;


end.