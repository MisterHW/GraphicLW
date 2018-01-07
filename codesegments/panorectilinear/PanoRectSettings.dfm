object PanoRectSettingsDlg: TPanoRectSettingsDlg
  Left = 358
  Top = 131
  BorderStyle = bsToolWindow
  Caption = 'viewport settings'
  ClientHeight = 281
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox1: TGroupBox
    Left = 0
    Top = 2
    Width = 308
    Height = 277
    Caption = 'viewport settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = GroupBox1Click
    object Label1: TLabel
      Left = 15
      Top = 25
      Width = 166
      Height = 16
      Caption = 'equivalent focal length (mm)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 15
      Top = 86
      Width = 108
      Height = 16
      Caption = 'polar angle (theta)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 15
      Top = 148
      Width = 115
      Height = 16
      Caption = 'azimutal angle (phi)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 15
      Top = 209
      Width = 94
      Height = 16
      Caption = 'viewport tilt (psi)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ftracker: TTrackBar
      Tag = 2000
      Left = 73
      Top = 38
      Width = 227
      Height = 51
      HelpType = htKeyword
      Max = 80000
      Min = 300
      Frequency = 1000
      Position = 2000
      TabOrder = 0
      TickMarks = tmBoth
      OnChange = TrackerChange
      OnKeyPress = TrackerKeyPress
    end
    object ftext: TEdit
      Left = 15
      Top = 52
      Width = 55
      Height = 22
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 1
      Text = '20'
      OnChange = TextChange
      OnClick = EditClick
    end
    object thetatext: TEdit
      Left = 15
      Top = 113
      Width = 55
      Height = 22
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 2
      Text = '90'
      OnChange = TextChange
      OnClick = EditClick
    end
    object thetatracker: TTrackBar
      Left = 73
      Top = 100
      Width = 227
      Height = 50
      HelpType = htKeyword
      Max = 9000
      Min = -9000
      Frequency = 1000
      TabOrder = 3
      TickMarks = tmBoth
      OnChange = TrackerChange
      OnKeyPress = TrackerKeyPress
    end
    object phitext: TEdit
      Left = 15
      Top = 175
      Width = 55
      Height = 22
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 4
      Text = '0'
      OnChange = TextChange
      OnClick = EditClick
    end
    object phitracker: TTrackBar
      Left = 73
      Top = 161
      Width = 227
      Height = 51
      HelpType = htKeyword
      Max = 18000
      Min = -18000
      Frequency = 1000
      TabOrder = 5
      TickMarks = tmBoth
      OnChange = TrackerChange
      OnKeyPress = TrackerKeyPress
    end
    object psitext: TEdit
      Left = 15
      Top = 236
      Width = 55
      Height = 22
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 6
      Text = '0'
      OnChange = TextChange
      OnClick = EditClick
    end
    object psitracker: TTrackBar
      Left = 73
      Top = 223
      Width = 227
      Height = 50
      HelpType = htKeyword
      Max = 18000
      Min = -18000
      Frequency = 1000
      TabOrder = 7
      TickMarks = tmBoth
      OnChange = TrackerChange
      OnKeyPress = TrackerKeyPress
    end
  end
end
