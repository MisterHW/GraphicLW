object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 593
  ClientWidth = 822
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox1: TGroupBox
    Left = 544
    Top = 0
    Width = 272
    Height = 137
    Caption = 'GroupBox1'
    Color = clSilver
    ParentColor = False
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Caption = 'new Point'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 16
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 1
    end
    object Button3: TButton
      Left = 16
      Top = 86
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 2
    end
    object Button4: TButton
      Left = 192
      Top = 24
      Width = 75
      Height = 25
      Caption = 'OFF'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 97
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 4
    end
    object Button6: TButton
      Left = 97
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 5
    end
    object Button7: TButton
      Left = 194
      Top = 86
      Width = 75
      Height = 25
      Caption = 'save'
      TabOrder = 6
      OnClick = Button7Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 544
    Top = 143
    Width = 272
    Height = 442
    Caption = 'GroupBox2'
    Color = clSilver
    ParentColor = False
    TabOrder = 1
    object Memo1: TMemo
      Left = 7
      Top = 20
      Width = 258
      Height = 415
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object CheckBox1: TCheckBox
    Left = 200
    Top = 456
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 2
  end
end
