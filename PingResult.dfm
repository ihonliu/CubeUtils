object PingResult: TPingResult
  Left = 0
  Top = 0
  Caption = #36830#36890#24615#27979#35797
  ClientHeight = 582
  ClientWidth = 763
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
    Width = 153
    Height = 582
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 560
      Width = 147
      Height = 19
      Align = alBottom
      Caption = 'SS-Cube'#33410#28857#36830#36890#24615#27979#35797'V0.1'
      WordWrap = True
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 215
      Width = 147
      Height = 16
      Align = alTop
      Caption = '  '#30456#20851#20449#24687
      ExplicitWidth = 50
    end
    object PingProgressBar: TProgressBar
      AlignWithMargins = True
      Left = 3
      Top = 183
      Width = 147
      Height = 26
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 139
    end
    object GridPanel1: TGridPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 147
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
      ExplicitWidth = 139
      object BtnStartPing: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 139
        Height = 37
        Align = alClient
        Caption = #24320#22987#27979#35797
        TabOrder = 0
        OnClick = BtnStartPingClick
        ExplicitWidth = 131
      end
      object BtnReadConf: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 47
        Width = 139
        Height = 37
        Align = alClient
        Caption = #35835#21462#35774#32622
        TabOrder = 1
        ExplicitWidth = 131
      end
      object BtnStop: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 90
        Width = 139
        Height = 37
        Align = alClient
        Caption = #20572#27490#27979#35797
        TabOrder = 2
        OnClick = BtnStopClick
        ExplicitWidth = 131
      end
      object BitBtn3: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 133
        Width = 139
        Height = 37
        Align = alClient
        Caption = 'Terminate'
        TabOrder = 3
        OnClick = BitBtn3Click
        ExplicitWidth = 131
      end
    end
    object LogMemo: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 237
      Width = 147
      Height = 317
      Align = alClient
      TabOrder = 2
      ExplicitLeft = 19
      ExplicitTop = 367
      ExplicitHeight = 339
    end
    object BitBtn1: TBitBtn
      Left = 7
      Top = 529
      Width = 75
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 3
      Visible = False
      OnClick = BitBtn1Click
    end
  end
  object StringGrid1: TStringGrid
    AlignWithMargins = True
    Left = 156
    Top = 3
    Width = 604
    Height = 576
    Align = alClient
    ColCount = 6
    RowCount = 2
    ParentColor = True
    TabOrder = 1
    ExplicitLeft = 148
    ExplicitWidth = 1041
    ColWidths = (
      64
      305
      56
      57
      56
      58)
    RowHeights = (
      24
      24)
  end
end
