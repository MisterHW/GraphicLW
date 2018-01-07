object ScanForm: TScanForm
  Left = 391
  Top = 278
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ScanForm'
  ClientHeight = 47
  ClientWidth = 172
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object BtnScan: TButton
    Left = 6
    Top = 23
    Width = 56
    Height = 19
    Caption = 'Scannen'
    TabOrder = 0
    OnClick = BtnScanClick
  end
  object CBGUI: TCheckBox
    Left = 6
    Top = 6
    Width = 163
    Height = 13
    Caption = 'GUI der Twain-Software anzeigen'
    TabOrder = 1
  end
end
