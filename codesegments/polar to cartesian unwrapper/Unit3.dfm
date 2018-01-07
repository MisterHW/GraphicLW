object Form3: TForm3
  Left = 526
  Top = 254
  BorderStyle = bsToolWindow
  Caption = 'Polar unwrapper'
  ClientHeight = 154
  ClientWidth = 170
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    170
    154)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 169
    Height = 49
    Caption = 'origin and base angle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Button1: TButton
      Left = 7
      Top = 16
      Width = 75
      Height = 25
      Caption = 'record'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 86
      Top = 16
      Width = 75
      Height = 25
      Caption = 'preview'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Button3: TButton
    Left = 4
    Top = 125
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'abort'
    TabOrder = 1
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 88
    Top = 125
    Width = 79
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'finish'
    TabOrder = 2
    OnClick = Button4Click
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 50
    Width = 169
    Height = 71
    Caption = 'Info'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 105
      Height = 13
      Caption = 'right click to set points'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 124
      Height = 13
      Caption = 'press enter to store points,'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 32
      Width = 126
      Height = 13
      Caption = 'left + drag to modify points.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
end
