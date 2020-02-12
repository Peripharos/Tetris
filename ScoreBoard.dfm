object FrmScoreBoard: TFrmScoreBoard
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Scoreboard'
  ClientHeight = 542
  ClientWidth = 444
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  GlassFrame.Enabled = True
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 444
    Height = 542
    Align = alClient
    BorderStyle = bsNone
    Color = clBlack
    ColCount = 3
    DefaultColWidth = 50
    DefaultRowHeight = 30
    FixedColor = clMedGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    OnDrawCell = GridDrawCell
    ExplicitWidth = 333
    ExplicitHeight = 470
    ColWidths = (
      50
      246
      144)
  end
end
