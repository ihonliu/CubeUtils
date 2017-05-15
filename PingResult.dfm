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
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 16
  object ComponentHolder: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 445
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 426
      Width = 139
      Height = 16
      Align = alBottom
      Caption = 'SS-Cube'#33410#28857#36830#36890#24615#27979#35797'V0.1'
      ExplicitWidth = 143
    end
    object PingProgressBar: TProgressBar
      AlignWithMargins = True
      Left = 3
      Top = 183
      Width = 139
      Height = 26
      Align = alTop
      TabOrder = 0
    end
    object GridPanel1: TGridPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 139
      Height = 174
      Align = alTop
      ColumnCollection = <
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = BtnStartPing
          Row = 0
        end
        item
          Column = 0
          Control = BtnReadConf
          Row = 1
        end
        item
          Column = 0
          Control = BtnStop
          Row = 2
        end
        item
          Column = 0
          Control = BitBtn3
          Row = 3
        end>
      Locked = True
      RowCollection = <
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end>
      TabOrder = 1
      object BtnStartPing: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 131
        Height = 37
        Align = alClient
        Caption = #24320#22987#27979#35797
        TabOrder = 0
        OnClick = BtnStartPingClick
      end
      object BtnReadConf: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 47
        Width = 131
        Height = 37
        Align = alClient
        Caption = #35835#21462#35774#32622
        TabOrder = 1
      end
      object BtnStop: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 90
        Width = 131
        Height = 37
        Align = alClient
        Caption = #20572#27490#27979#35797
        TabOrder = 2
        OnClick = BtnStopClick
      end
      object BitBtn3: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 133
        Width = 131
        Height = 37
        Align = alClient
        Caption = #27809#20160#20040#21365#29992#30340#25353#38062
        TabOrder = 3
      end
    end
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 148
    Top = 3
    Width = 474
    Height = 439
    Align = alClient
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = #24494#36719#38597#40657
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
  object LogMemo: TMemo
    Left = 145
    Top = 0
    Width = 480
    Height = 445
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
end
