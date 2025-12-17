object optionsForm: ToptionsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 441
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 529
    Height = 97
    Caption = 'Locations'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 27
      Width = 73
      Height = 15
      Caption = 'GP2 Location:'
    end
    object Label2: TLabel
      Left = 16
      Top = 59
      Width = 73
      Height = 15
      Caption = 'GP3 Location:'
    end
    object edtGP2Loc: TEdit
      Left = 104
      Top = 24
      Width = 305
      Height = 23
      TabOrder = 0
    end
    object edtGp3Loc: TEdit
      Left = 104
      Top = 56
      Width = 305
      Height = 23
      TabOrder = 1
    end
    object btnGP2Browse: TButton
      Left = 432
      Top = 23
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 2
      OnClick = btnGP2BrowseClick
    end
    object btnGPBrowse: TButton
      Left = 432
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 3
      OnClick = btnGPBrowseClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 111
    Width = 526
    Height = 162
    Caption = 'Jam Format Detection'
    TabOrder = 1
    object Label3: TLabel
      Left = 10
      Top = 27
      Width = 497
      Height = 75
      Caption = 
        'Jam Editor attempts to work out what each JAM file is - HW JAMs ' +
        'have a specific format whereas there'#39's not an easy way to work o' +
        'ut GP2 or GP3 SW JAMs.'#13#10#13#10'Jam Editor makes an assumption based o' +
        'n JAM'#39's location, or will keep a record of JAMs located in arbit' +
        'ary/non GP2/GP3 locations. You can clear this here:'
      WordWrap = True
    end
    object Button1: TButton
      Left = 144
      Top = 120
      Width = 217
      Height = 25
      Caption = 'Clear JAM Format Records'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
