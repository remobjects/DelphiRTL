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

// Text dfm:
{
object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Form5'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 248
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
end
}

implementation

procedure TForm5.Button1Click(Sender: TObject);
begin
  //ShowMessage('Clicked!');
end;

end.