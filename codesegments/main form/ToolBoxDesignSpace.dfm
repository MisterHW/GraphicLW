object ToolBoxDesignFrame: TToolBoxDesignFrame
  Left = 583
  Top = 534
  Caption = 'ToolBox Design Space'
  ClientHeight = 479
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 698
    Height = 479
    ActivePage = NavigatorBoxes
    Align = alClient
    TabOrder = 0
    object NavigatorBoxes: TTabSheet
      Caption = 'NavigatorBoxes'
      ExplicitLeft = 8
      ExplicitTop = 31
      object NavigatorGroupBox: TGroupBox
        Left = 16
        Top = 3
        Width = 273
        Height = 273
        Caption = 'Workspace Navigator'
        TabOrder = 0
      end
    end
  end
end
