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
    Button2: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    RadioButton1: TRadioButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var key: Char);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  writeLn('Yes????');
  //ShowMessage("ok from ShowMessage");
  ListBox1.Items.Add('Dame 1');
  ComboBox1.Items.Add('Dame 1');
end;

procedure TForm6.Edit1KeyPress(Sender: TObject; var key: Char);
begin

end;

end.