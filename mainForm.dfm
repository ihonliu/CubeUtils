object mainForm: TmainForm
  Left = 394
  Top = 198
  BorderStyle = bsSingle
  Caption = 'CubeUtilsD'
  ClientHeight = 178
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = OnCreate
  PixelsPerInch = 96
  TextHeight = 16
  object InfoLabel: TLabel
    Left = 8
    Top = 22
    Width = 132
    Height = 16
    Caption = 'SS-Cube'#33258#21160#19979#36733#37197#32622#31243#24207
  end
  object NameLabel: TLabel
    Left = 8
    Top = 8
    Width = 123
    Height = 19
    Caption = 'CubeUtils goes here'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = #24494#36719#38597#40657
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
  end
  object LoginBtn: TBitBtn
    Left = 87
    Top = 144
    Width = 75
    Height = 25
    Caption = #30331#20837
    TabOrder = 2
    OnClick = LoginBtnClick
  end
  object EmailEdit: TLabeledEdit
    Left = 8
    Top = 64
    Width = 235
    Height = 24
    HelpType = htKeyword
    EditLabel.Width = 28
    EditLabel.Height = 16
    EditLabel.Caption = 'Email'
    ImeMode = imDisable
    TabOrder = 0
  end
  object PasswordEdit: TLabeledEdit
    Left = 8
    Top = 114
    Width = 235
    Height = 24
    Hint = #22823#20110'6'#20301#65292#23567#20110'20'#20301
    EditLabel.Width = 22
    EditLabel.Height = 16
    EditLabel.Caption = #23494#30721
    ImeMode = imDisable
    MaxLength = 20
    ParentShowHint = False
    PasswordChar = #9679
    ShowHint = True
    TabOrder = 1
  end
  object ExitBtn: TBitBtn
    Left = 168
    Top = 144
    Width = 75
    Height = 25
    Caption = #36864#20986
    TabOrder = 3
    OnClick = ExitBtnClick
  end
  object ActivityIndicator: TActivityIndicator
    Left = 8
    Top = 144
    FrameDelay = 30
    IndicatorSize = aisSmall
  end
  object GridPanel: TGridPanel
    Left = 8
    Top = 44
    Width = 235
    Height = 94
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = SaveConf
        Row = 0
      end
      item
        Column = 1
        Control = TestPing
        Row = 0
      end
      item
        Column = 0
        Control = SaveLink
        Row = 1
      end
      item
        Column = 1
        Control = ShowQR
        Row = 1
      end>
    ParentBackground = False
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 5
    Visible = False
    object SaveConf: TBitBtn
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 111
      Height = 41
      Align = alClient
      Caption = #20445#23384'SS'#37197#32622
      TabOrder = 0
      OnClick = SaveConfClick
    end
    object TestPing: TBitBtn
      AlignWithMargins = True
      Left = 120
      Top = 3
      Width = 112
      Height = 41
      Align = alClient
      Caption = #27979#35797#33410#28857#36830#36890#24615'('#19981#21487#29992')'
      TabOrder = 1
    end
    object SaveLink: TBitBtn
      AlignWithMargins = True
      Left = 3
      Top = 50
      Width = 111
      Height = 41
      Align = alClient
      Caption = #20445#23384#20026'SS-Link('#19981#21487#29992')'
      TabOrder = 2
      ExplicitLeft = 0
      ExplicitWidth = 114
    end
    object ShowQR: TBitBtn
      AlignWithMargins = True
      Left = 120
      Top = 50
      Width = 112
      Height = 41
      Align = alClient
      Caption = #26174#31034#20108#32500#30721'('#19981#21487#29992')'
      TabOrder = 3
      ExplicitWidth = 113
    end
  end
end
