﻿namespace ProjectRCLMac;

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
    checkBox1: TCheckBox;
    listBox1: TListBox;
    comboBox1: TComboBox;

    listView1: TListView;
    treeView1: TTreeView;
    {
    groupBox1: TGroupBox;
    panel1: TPanel;

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
    edit3: TEdit;}



    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Edit1OnKeyPress(Sender: TObject; var key: Char);
  end;

  var
    Form6: TForm6;

implementation

procedure TForm6.Button1Click(Sender: TObject);
begin
  writeLn('Yes????');
  //var lNode := treeView1.Items.Add(nil, 'New Node!');
  //treeView1.Items.AddChild(lNode, 'New Child!');
  //treeView1.Items.AddChildFirst(lNode, 'New First Child!');
  //lNode.Expanded := false;

  ShowMessage('no?');
  {
  if listView1.ViewStyle ≠ TViewStyle.vsReport then
    listView1.ViewStyle := TViewStyle.vsReport;
  writeLn('antes');
  var lColumn := listView1.Columns.Add;
  lColumn.Caption := 'Loncho!';

  lColumn := listView1.Columns.Add;
  lColumn.Caption := 'Loncho 2';

  lColumn := listView1.Columns.Add;
  lColumn.Caption := 'Loncho 3';


  var lItem := listView1.Items.Add;
  lItem.Caption := 'Item 1!!';
  lItem.SubItems.Add('Roncho 2');
  lItem.SubItems.Add('Roncho 3');
  }

  //ShowMessage('Clicked!');
  //listBox1.Items.Add('Item 1');
  //comboBox1.Items.Add('Item 1');
  {
  //label1.Width := 200;
  //label1.Caption := 'All right!';
  //ShowMessage('Clicked!');
  //ShowMessage(edit1.Text);
  listBox1.Items.Add(edit1.Text);
  comboBox1.Items.Add(edit1.Text);
  edit1.OnKeyPress := @Edit1OnKeyPress;
  }
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  {
  edit1.Text := 'Blah!';
  }
end;

procedure TForm6.ComboBox1Select(Sender: TObject);
begin
  //ShowMessage('Yes!');
end;

procedure TForm6.Edit1OnKeyPress(Sender: TObject; var key: Char);
begin
  //ShowMessage(key);
end;

end.