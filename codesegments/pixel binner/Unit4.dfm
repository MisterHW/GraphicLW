object Form4: TForm4
  Left = 443
  Top = 483
  BorderStyle = bsNone
  Caption = 'pixel binning tool'
  ClientHeight = 254
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Button1: TButton
    Left = 1
    Top = 220
    Width = 93
    Height = 31
    Caption = 'abort'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 242
    Top = 220
    Width = 93
    Height = 31
    Caption = 'apply'
    TabOrder = 1
    OnClick = Button2Click
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 336
    Height = 89
    Caption = 'Step 1: define binning size'
    TabOrder = 2
    object Label1: TLabel
      Left = 20
      Top = 26
      Width = 55
      Height = 16
      Caption = 'width [px]'
    end
    object Label2: TLabel
      Left = 20
      Top = 55
      Width = 58
      Height = 16
      Caption = 'height[px]'
    end
    object Label3: TLabel
      Left = 138
      Top = 26
      Width = 71
      Height = 16
      Caption = 'original size'
    end
    object Label4: TLabel
      Left = 138
      Top = 55
      Width = 61
      Height = 16
      Caption = 'target size'
    end
    object orgwlabel: TLabel
      Left = 220
      Top = 26
      Width = 42
      Height = 16
      AutoSize = False
      Caption = 'old W'
    end
    object newwlabel: TLabel
      Left = 219
      Top = 55
      Width = 42
      Height = 16
      AutoSize = False
      Caption = 'new W'
    end
    object orghlabel: TLabel
      Left = 288
      Top = 26
      Width = 42
      Height = 16
      AutoSize = False
      Caption = 'old H'
    end
    object newhlabel: TLabel
      Left = 288
      Top = 55
      Width = 42
      Height = 16
      AutoSize = False
      Caption = 'new H'
    end
    object Label5: TLabel
      Left = 270
      Top = 26
      Width = 13
      Height = 16
      AutoSize = False
      Caption = 'x'
    end
    object Label6: TLabel
      Left = 269
      Top = 55
      Width = 13
      Height = 16
      AutoSize = False
      Caption = 'x'
    end
    object BinWidth: TEdit
      Left = 84
      Top = 26
      Width = 35
      Height = 20
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvRaised
      BorderStyle = bsNone
      TabOrder = 0
      Text = '1'
      OnChange = BinWidthChange
    end
    object BinHeight: TEdit
      Left = 84
      Top = 55
      Width = 35
      Height = 20
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvRaised
      BorderStyle = bsNone
      TabOrder = 1
      Text = '1'
    end
    object UpDown1: TUpDown
      Left = 120
      Top = 26
      Width = 12
      Height = 20
      Associate = BinWidth
      Min = 1
      Max = 64
      Position = 1
      TabOrder = 2
      OnClick = UpDown1Click
    end
    object UpDown3: TUpDown
      Left = 120
      Top = 55
      Width = 12
      Height = 20
      Associate = BinHeight
      Min = 1
      Max = 64
      Position = 1
      TabOrder = 3
      OnClick = UpDown3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 130
    Width = 336
    Height = 88
    Caption = 'Step 2: set amplification ratio'
    TabOrder = 3
    object amplabel: TLabel
      Left = 256
      Top = 59
      Width = 36
      Height = 16
      Caption = '100 %'
    end
    object Label11: TLabel
      Left = 20
      Top = 59
      Width = 51
      Height = 16
      Caption = 'average'
    end
    object Label14: TLabel
      Left = 217
      Top = 59
      Width = 24
      Height = 16
      Caption = 'add'
    end
    object TrackBar1: TTrackBar
      Left = 10
      Top = 20
      Width = 237
      Height = 30
      Max = 100
      Position = 1
      TabOrder = 0
      OnChange = TrackBar1Change
    end
  end
  object Button3: TButton
    Left = 242
    Top = 93
    Width = 93
    Height = 31
    Caption = 'preview'
    TabOrder = 4
    OnClick = Button3Click
  end
end
