object bbalancerform: Tbbalancerform
  Left = 192
  Top = 107
  BorderStyle = bsToolWindow
  Caption = 'brightness balancer'
  ClientHeight = 743
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = -1
    Width = 281
    Height = 162
    Hint = 'Click on the mask layer thumbnail to change the mask layer'
    TabOrder = 0
    object Label1: TLabel
      Left = 39
      Top = 8
      Width = 62
      Height = 13
      Caption = 'working layer'
    end
    object Label3: TLabel
      Left = 183
      Top = 8
      Width = 50
      Height = 13
      Caption = 'mask layer'
    end
    object Image1: TImage
      Left = 8
      Top = 24
      Width = 129
      Height = 129
    end
    object Image2: TImage
      Left = 144
      Top = 24
      Width = 129
      Height = 129
      OnClick = Image2Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 707
    Width = 283
    Height = 36
    Align = alBottom
    TabOrder = 1
    object abortbutton: TButton
      Left = 122
      Top = 6
      Width = 75
      Height = 25
      Caption = 'abort'
      TabOrder = 0
      OnClick = abortbuttonClick
    end
    object applybutton: TButton
      Left = 202
      Top = 6
      Width = 75
      Height = 25
      Caption = 'apply'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = applybuttonClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 162
    Width = 281
    Height = 207
    Caption = 'gradient strength analysis (image homogenity)'
    TabOrder = 2
    object Label2: TLabel
      Left = -1
      Top = 152
      Width = 274
      Height = 13
      Alignment = taCenter
      Caption = '( 2x grid size should be below the highest local frequency )'
      Transparent = True
    end
    object suggestionlabel: TLabel
      Left = 8
      Top = 168
      Width = 265
      Height = 33
      Alignment = taCenter
      AutoSize = False
      Caption = '- no suggestions available- '
      Layout = tlCenter
    end
    object Label6: TLabel
      Left = 268
      Top = 188
      Width = 8
      Height = 13
      Caption = '?'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label6Click
    end
    object Panel2: TPanel
      Left = 8
      Top = 16
      Width = 265
      Height = 126
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 369
    Width = 281
    Height = 336
    Caption = 'balancer settings'
    TabOrder = 3
    object Label4: TLabel
      Left = 16
      Top = 50
      Width = 75
      Height = 13
      Caption = 'grid constant'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object gclabel: TLabel
      Left = 201
      Top = 78
      Width = 62
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '32'
      Color = clGray
      ParentColor = False
      Transparent = True
    end
    object Label5: TLabel
      Left = 207
      Top = 109
      Width = 3
      Height = 13
    end
    object zeropresetlabel: TLabel
      Left = 201
      Top = 160
      Width = 62
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '128'
      Color = clGray
      ParentColor = False
      Transparent = True
    end
    object Label8: TLabel
      Left = 32
      Top = 252
      Width = 165
      Height = 13
      Caption = 'reference interval center (0..100) %'
    end
    object Label9: TLabel
      Left = 32
      Top = 276
      Width = 171
      Height = 13
      Caption = 'reference interval width  (0..+/-50) %'
    end
    object Label10: TLabel
      Left = 16
      Top = 196
      Width = 170
      Height = 13
      Caption = 'statistical analysis parameters'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label11: TLabel
      Left = 266
      Top = 196
      Width = 8
      Height = 13
      Caption = '?'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label11Click
    end
    object Label12: TLabel
      Left = 16
      Top = 108
      Width = 98
      Height = 13
      Caption = 'brightness preset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 32
      Top = 220
      Width = 153
      Height = 13
      Caption = 'statistics mask threshold (0..255)'
    end
    object cubesizetracker: TTrackBar
      Left = 30
      Top = 64
      Width = 196
      Height = 41
      Max = 128
      Min = 4
      Frequency = 5
      Position = 32
      TabOrder = 0
      TickMarks = tmBoth
      OnChange = cubesizetrackerChange
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 24
      Width = 201
      Height = 17
      Caption = 'use 2nd order statistics (slower, better)'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object brightnesstracker: TTrackBar
      Left = 30
      Top = 146
      Width = 196
      Height = 41
      Max = 255
      Frequency = 5
      Position = 128
      TabOrder = 2
      TickMarks = tmBoth
      OnChange = brightnesstrackerChange
    end
    object CheckBox3: TCheckBox
      Left = 32
      Top = 128
      Width = 209
      Height = 17
      Caption = 'use preset to fit brightness (zero order)'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object QuantileCenter: TEdit
      Left = 216
      Top = 248
      Width = 49
      Height = 21
      TabOrder = 4
      Text = '80'
      OnClick = QuantileCenterClick
    end
    object QuantileWidth: TEdit
      Left = 216
      Top = 272
      Width = 49
      Height = 21
      TabOrder = 5
      Text = '3'
      OnClick = QuantileWidthClick
    end
    object Button1: TButton
      Left = 152
      Top = 301
      Width = 115
      Height = 25
      Caption = 'show graph'
      TabOrder = 6
      OnClick = Button1Click
    end
    object MaskThreshold: TEdit
      Left = 216
      Top = 216
      Width = 49
      Height = 21
      TabOrder = 7
      Text = '128'
      OnClick = MaskThresholdClick
    end
  end
end
