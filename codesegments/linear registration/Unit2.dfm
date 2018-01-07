object Form2: TForm2
  Left = 244
  Top = 604
  BorderStyle = bsToolWindow
  Caption = 'LinReg tool'
  ClientHeight = 455
  ClientWidth = 130
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 129
    Height = 65
    Caption = 'source points'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 48
      Width = 23
      Height = 13
      Caption = 'clear'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Button3: TButton
      Left = 8
      Top = 16
      Width = 49
      Height = 25
      Caption = 'record'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 66
    Width = 129
    Height = 65
    Caption = 'destination points'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 23
      Height = 13
      Caption = 'clear'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Button4: TButton
      Left = 8
      Top = 16
      Width = 49
      Height = 25
      Caption = 'record'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button4Click
    end
    object Button10: TButton
      Left = 72
      Top = 16
      Width = 49
      Height = 25
      Caption = 'from src'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = Button10Click
    end
  end
  object Button1: TButton
    Left = 67
    Top = 428
    Width = 59
    Height = 25
    Caption = 'finish'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 3
    Top = 428
    Width = 59
    Height = 25
    Caption = 'abort'
    TabOrder = 3
    OnClick = Button2Click
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 131
    Width = 129
    Height = 71
    Caption = 'workflow'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    object Label4: TLabel
      Left = 88
      Top = 29
      Width = 37
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '[...]'
      OnDblClick = Label4DblClick
    end
    object CropToDestOuter: TCheckBox
      Left = 8
      Top = 12
      Width = 97
      Height = 17
      Caption = 'crop destination'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 0
    end
    object SaveResult: TCheckBox
      Left = 8
      Top = 28
      Width = 81
      Height = 17
      Caption = 'save image'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 1
    end
    object Button9: TButton
      Left = 6
      Top = 44
      Width = 118
      Height = 21
      Caption = 'reassign points'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = Button9Click
    end
  end
  object GroupBox4: TGroupBox
    Left = 0
    Top = 203
    Width = 129
    Height = 94
    Caption = 'info'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    object infobox: TMemo
      Left = 5
      Top = 13
      Width = 119
      Height = 75
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Lines.Strings = (
        'record source points first')
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
  end
  object GB05: TGroupBox
    Left = 0
    Top = 298
    Width = 129
    Height = 127
    Caption = 'additional'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    object Label3: TLabel
      Left = 8
      Top = 16
      Width = 30
      Height = 13
      Caption = 'border'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object bordertoptext: TEdit
      Left = 80
      Top = 16
      Width = 41
      Height = 21
      BevelKind = bkSoft
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '0'
      OnClick = bordertoptextClick
    end
    object borderrighttext: TEdit
      Left = 80
      Top = 39
      Width = 41
      Height = 21
      BevelKind = bkSoft
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '0'
      OnClick = bordertoptextClick
    end
    object borderlefttext: TEdit
      Left = 36
      Top = 39
      Width = 41
      Height = 21
      BevelKind = bkSoft
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = '0'
      OnClick = bordertoptextClick
    end
    object borderbottomtext: TEdit
      Left = 80
      Top = 62
      Width = 41
      Height = 21
      BevelKind = bkSoft
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Text = '0'
      OnClick = bordertoptextClick
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 88
      Width = 113
      Height = 17
      Caption = 'interpolate NN'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 104
      Width = 113
      Height = 17
      Caption = 'interpolate BILIN.'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      TabStop = True
    end
  end
end
