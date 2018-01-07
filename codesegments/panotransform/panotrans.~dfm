object panotransform: Tpanotransform
  Left = 571
  Top = 195
  BorderStyle = bsToolWindow
  Caption = 'pano transformer'
  ClientHeight = 430
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    250
    430)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 79
    Width = 250
    Height = 201
    Caption = 'angular coordinates [in degrees]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 11
      Top = 19
      Width = 89
      Height = 13
      Caption = 'polar angle (Theta)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 11
      Top = 75
      Width = 96
      Height = 13
      Caption = 'azimuthal angle (phi)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 11
      Top = 131
      Width = 75
      Height = 13
      Caption = 'viewport tilt (psi)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object thetatext: TEdit
      Left = 12
      Top = 42
      Width = 45
      Height = 18
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 0
      Text = '90'
      OnChange = thetatextChange
      OnClick = thetatextClick
    end
    object thetatracker: TTrackBar
      Left = 56
      Top = 31
      Width = 185
      Height = 41
      HelpType = htKeyword
      Max = 1800
      Frequency = 100
      Position = 900
      TabOrder = 1
      TickMarks = tmBoth
      OnChange = thetatrackerChange
      OnKeyPress = thetatrackerKeyPress
    end
    object phitracker: TTrackBar
      Left = 56
      Top = 87
      Width = 185
      Height = 41
      Max = 1800
      Min = -1800
      Frequency = 100
      TabOrder = 2
      TickMarks = tmBoth
      OnChange = thetatrackerChange
      OnKeyPress = phitrackerKeyPress
    end
    object phitext: TEdit
      Left = 12
      Top = 98
      Width = 45
      Height = 18
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 3
      Text = '0'
      OnChange = phitextChange
      OnClick = thetatextClick
    end
    object psitracker: TTrackBar
      Left = 56
      Top = 143
      Width = 185
      Height = 41
      Max = 1800
      Min = -1800
      Frequency = 100
      TabOrder = 4
      TickMarks = tmBoth
      OnChange = thetatrackerChange
      OnKeyPress = psitrackerKeyPress
    end
    object psitext: TEdit
      Left = 12
      Top = 154
      Width = 45
      Height = 18
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 5
      Text = '0'
      OnChange = psitextChange
      OnClick = thetatextClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 392
    Width = 250
    Height = 39
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    object ApplyToolButton: TButton
      Left = 170
      Top = 9
      Width = 75
      Height = 25
      Caption = 'apply'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = ApplyToolButtonClick
    end
    object abortbutton: TButton
      Left = 4
      Top = 9
      Width = 75
      Height = 25
      Caption = 'abort'
      TabOrder = 1
      OnClick = abortbuttonClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = -1
    Width = 250
    Height = 79
    Caption = 'image ranges'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 117
      Height = 13
      Caption = 'angular range left-to-right'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 32
      Width = 130
      Height = 13
      Caption = 'angular range top-to-bottom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object AlphaHText: TEdit
      Left = 148
      Top = 10
      Width = 37
      Height = 18
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 0
      Text = '360'
      OnChange = AlphaHTextChange
      OnClick = thetatextClick
    end
    object AlphaVText: TEdit
      Left = 148
      Top = 33
      Width = 37
      Height = 18
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 1
      Text = '180'
      OnChange = AlphaHTextChange
      OnClick = thetatextClick
    end
    object setrangebutton: TButton
      Left = 195
      Top = 21
      Width = 43
      Height = 20
      Caption = 'set'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = setrangebuttonClick
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 56
      Width = 233
      Height = 17
      Caption = 'expand to full panorama (no clipping errors)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object GroupBox4: TGroupBox
    Left = 0
    Top = 280
    Width = 249
    Height = 113
    Caption = 'additional parameters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    object Label6: TLabel
      Left = 11
      Top = 17
      Width = 191
      Height = 13
      Caption = 'polar pinching / extrusion (vertical offset)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object pinchtext: TEdit
      Left = 12
      Top = 42
      Width = 45
      Height = 18
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 0
      Text = '0'
      OnChange = pinchtextChange
      OnClick = thetatextClick
    end
    object pinchtracker: TTrackBar
      Left = 56
      Top = 32
      Width = 185
      Height = 41
      Max = 50000
      Min = -50000
      Frequency = 100
      TabOrder = 1
      TickMarks = tmBoth
      OnChange = thetatrackerChange
      OnKeyPress = pinchtrackerKeyPress
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 72
      Width = 233
      Height = 17
      Caption = 'send layer change notifications'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 88
      Width = 233
      Height = 17
      Caption = 'show h/v rulers'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox3Click
    end
  end
end
