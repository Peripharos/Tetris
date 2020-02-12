object FrmStartScreen: TFrmStartScreen
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'FrmStartScreen'
  ClientHeight = 871
  ClientWidth = 848
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    848
    871)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 168
    Top = 240
    Width = 61
    Height = 107
    Caption = 'T'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -107
    Font.Name = '@PMingLiU-ExtB'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 245
    Top = 240
    Width = 61
    Height = 107
    Caption = 'E'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -107
    Font.Name = '@PMingLiU-ExtB'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 328
    Top = 240
    Width = 61
    Height = 107
    Caption = 'T'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -107
    Font.Name = '@PMingLiU-ExtB'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 416
    Top = 240
    Width = 67
    Height = 107
    Caption = 'R'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -107
    Font.Name = '@PMingLiU-ExtB'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 512
    Top = 240
    Width = 34
    Height = 107
    Caption = 'I'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clPurple
    Font.Height = -107
    Font.Name = '@PMingLiU-ExtB'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 576
    Top = 240
    Width = 56
    Height = 107
    Caption = 'S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4227327
    Font.Height = -107
    Font.Name = '@PMingLiU-ExtB'
    Font.Style = []
    ParentFont = False
  end
  object LblCreator: TLabel
    Left = 544
    Top = 400
    Width = 162
    Height = 33
    Caption = 'by Peripharos'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LblLevelStatic: TLabel
    Left = 134
    Top = 592
    Width = 138
    Height = 24
    Caption = 'Start mit Level:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LblLevel: TLabel
    Left = 310
    Top = 592
    Width = 11
    Height = 24
    Alignment = taRightJustify
    Caption = '1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LblPlayer: TLabel
    Left = 134
    Top = 656
    Width = 68
    Height = 24
    Caption = 'Spieler:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object BtnLevel: TUpDown
    Left = 326
    Top = 591
    Width = 17
    Height = 25
    Min = 1
    Max = 15
    Position = 1
    TabOrder = 0
    OnChangingEx = BtnLevelChangingEx
  end
  object BtnGo: TButton
    Left = 512
    Top = 644
    Width = 145
    Height = 45
    Caption = 'Start'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = BtnGoClick
  end
  object EdtPlayer: TEdit
    Left = 232
    Top = 653
    Width = 185
    Height = 32
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object BtnScoreBoard: TButton
    Left = 696
    Top = 824
    Width = 137
    Height = 39
    Anchors = [akLeft, akBottom]
    Caption = 'Scoreboard'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = BtnScoreBoardClick
  end
end
