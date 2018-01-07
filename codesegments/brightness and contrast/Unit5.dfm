object Form5: TForm5
  Left = 719
  Top = 252
  BorderStyle = bsNone
  Caption = 'brightness and contrast adjustment'
  ClientHeight = 161
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox1: TGroupBox
    Left = 0
    Top = -4
    Width = 375
    Height = 130
    TabOrder = 0
    object Label1: TLabel
      Left = 11
      Top = 9
      Width = 47
      Height = 16
      Caption = 'contrast'
    end
    object Label2: TLabel
      Left = 11
      Top = 62
      Width = 62
      Height = 16
      Caption = 'brightness'
    end
    object Label3: TLabel
      Left = 325
      Top = 31
      Width = 33
      Height = 16
      Caption = '100%'
    end
    object Label4: TLabel
      Left = 325
      Top = 84
      Width = 26
      Height = 16
      Caption = '50%'
    end
    object TrackBar1: TTrackBar
      Left = 2
      Top = 23
      Width = 317
      Height = 41
      Max = 255
      Min = -255
      TabOrder = 0
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 2
      Top = 78
      Width = 317
      Height = 40
      Max = 255
      Min = -255
      TabOrder = 1
      OnChange = TrackBar2Change
      OnKeyPress = TrackBar2KeyPress
    end
    object Panel1: TPanel
      Left = 155
      Top = 11
      Width = 11
      Height = 11
      TabOrder = 2
      OnClick = Panel1Click
    end
  end
  object Button1: TButton
    Left = 282
    Top = 128
    Width = 92
    Height = 31
    Caption = 'apply'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 186
    Top = 128
    Width = 92
    Height = 31
    Caption = 'cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 10
    Top = 132
    Width = 119
    Height = 21
    Caption = 'show preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object Panel2: TPanel
    Left = 155
    Top = 60
    Width = 11
    Height = 11
    TabOrder = 4
    OnClick = Panel2Click
  end
end
