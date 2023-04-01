object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 19
  object lbVersionStatus: TLabel
    Left = 16
    Top = 336
    Width = 94
    Height = 19
    Caption = 'lbVersionStatus'
  end
  object lbVersionCurrent: TLabel
    Left = 16
    Top = 312
    Width = 94
    Height = 19
    Caption = 'lbVersionStatus'
  end
  object btnLaunch: TButton
    Left = 448
    Top = 344
    Width = 115
    Height = 41
    Caption = 'Launch!'
    TabOrder = 0
    OnClick = btnLaunchClick
  end
  object btnVersionCheck: TButton
    Left = 16
    Top = 360
    Width = 115
    Height = 33
    Caption = 'Version check'
    TabOrder = 1
    OnClick = btnVersionCheckClick
  end
  object meLog: TMemo
    Left = 16
    Top = 40
    Width = 569
    Height = 257
    Lines.Strings = (
      'meLog')
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
