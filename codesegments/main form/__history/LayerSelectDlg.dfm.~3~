object LayerSelectDlg: TLayerSelectDlg
  Left = 323
  Top = 123
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsSingle
  Caption = 'Layer selection dialog'
  ClientHeight = 615
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnDblClick = FormDblClick
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnMouseDown = FormMouseDown
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 575
    Width = 862
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    OnMouseUp = Panel1MouseUp
    DesignSize = (
      862
      40)
    object LayerCaptionLabel: TLabel
      Left = 10
      Top = 2
      Width = 19
      Height = 16
      Caption = '-     '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = LayerCaptionLabelDblClick
    end
    object LayerInfoLabel: TLabel
      Left = 10
      Top = 20
      Width = 19
      Height = 16
      Caption = '-     '
    end
    object okbutton: TButton
      Left = 764
      Top = 6
      Width = 93
      Height = 31
      Anchors = [akTop, akRight]
      Caption = 'ok'
      TabOrder = 0
      OnClick = okbuttonClick
      OnMouseUp = okbuttonMouseUp
    end
    object abortbutton: TButton
      Left = 666
      Top = 6
      Width = 92
      Height = 31
      Anchors = [akTop, akRight]
      Caption = 'abort'
      TabOrder = 1
      OnClick = abortbuttonClick
      OnMouseUp = abortbuttonMouseUp
    end
  end
  object DockOptPopup: TPopupMenu
    OnPopup = DockOptPopupPopup
    Left = 8
    Top = 8
    object docktotop: TMenuItem
      Caption = 'dock to top'
      OnClick = DockTopClick
    end
    object docktoleft2: TMenuItem
      Caption = 'dock to left'
      OnClick = DockLeftClick
    end
    object docktoright1: TMenuItem
      Caption = 'dock to right'
      OnClick = docktoright1Click
    end
    object docktobottom1: TMenuItem
      Caption = 'dock to bottom'
      OnClick = docktobottom1Click
    end
    object centerwindow1: TMenuItem
      Caption = 'center to parent'
      Checked = True
      OnClick = centerwindow1Click
    end
  end
end
