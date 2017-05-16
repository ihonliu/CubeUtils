object PingResult: TPingResult
  Left = 0
  Top = 0
  Caption = #36830#36890#24615#27979#35797
  ClientHeight = 582
  ClientWidth = 1115
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object ComponentHolder: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 536
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 517
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
        Caption = 'Terminate'
        TabOrder = 3
        OnClick = BitBtn3Click
      end
    end
  end
  object StringGrid1: TStringGrid
    Left = 145
    Top = 0
    Width = 479
    Height = 536
    Align = alClient
    ColCount = 6
    RowCount = 2
    ParentColor = True
    TabOrder = 2
    ColWidths = (
      64
      215
      47
      47
      46
      45)
    RowHeights = (
      24
      24)
  end
  object LogMemo: TMemo
    Left = 0
    Top = 536
    Width = 1115
    Height = 46
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    Visible = False
  end
  object DBGrid1: TDBGrid
    Left = 624
    Top = 0
    Width = 491
    Height = 536
    Align = alRight
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = #24494#36719#38597#40657
    TitleFont.Style = []
  end
  object SQLTable1: TSQLTable
    Left = 472
    Top = 336
  end
end
