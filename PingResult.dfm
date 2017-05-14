object PingResult: TPingResult
  Left = 0
  Top = 0
  Caption = #36830#36890#24615#27979#35797
  ClientHeight = 445
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ComponentHolder: TPanel
    Left = 0
    Top = 0
    Width = 625
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object BtnStartPing: TBitBtn
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 27
      Align = alLeft
      Caption = #24320#22987#27979#35797
      TabOrder = 0
      OnClick = BtnStartPingClick
    end
    object BtnReadConfigure: TBitBtn
      AlignWithMargins = True
      Left = 84
      Top = 3
      Width = 75
      Height = 27
      Align = alLeft
      Caption = #35835#21462#37197#32622
      TabOrder = 1
    end
    object ProgressBar1: TProgressBar
      AlignWithMargins = True
      Left = 165
      Top = 3
      Width = 457
      Height = 27
      Align = alClient
      TabOrder = 2
    end
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 36
    Width = 619
    Height = 406
    Align = alClient
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NumOrder'
        Title.Caption = #24207#21495
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NodeName'
        Title.Caption = #33410#28857#21517#31216
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TimeOutIndicator'
        Title.Caption = #26159#21542#36229#26102
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LowestDelay'
        Title.Caption = #26368#20302#24310#36831
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'HighestDelay'
        Title.Caption = #26368#39640#24310#36831
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AverageDelay'
        Title.Caption = #24179#22343#24310#36831
        Visible = True
      end
      item
        Expanded = False
        Visible = True
      end>
  end
  object Memo1: TMemo
    Left = 24
    Top = 56
    Width = 553
    Height = 353
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
end
