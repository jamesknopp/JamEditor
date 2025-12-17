object JamBatchForm: TJamBatchForm
  Left = 0
  Top = 0
  Caption = 'Jam Batch Converter'
  ClientHeight = 765
  ClientWidth = 1196
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 345
    Width = 1196
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 420
  end
  object Panel2: TPanel
    Left = 0
    Top = 348
    Width = 1196
    Height = 417
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    ExplicitWidth = 1190
    ExplicitHeight = 400
    object Label4: TLabel
      Left = 9
      Top = 6
      Width = 57
      Height = 15
      Caption = 'Input JAM:'
    end
    object Label5: TLabel
      Left = 793
      Top = 16
      Width = 90
      Height = 15
      Caption = 'Output Location:'
    end
    object Label6: TLabel
      Left = 793
      Top = 112
      Width = 92
      Height = 15
      Caption = 'Output Filename:'
    end
    object btnAddFile: TButton
      Left = 9
      Top = 175
      Width = 75
      Height = 25
      Caption = 'Add File(s)'
      TabOrder = 0
      OnClick = btnAddFileClick
    end
    object btnDel: TButton
      Left = 9
      Top = 239
      Width = 119
      Height = 25
      Caption = 'Remove Selected'
      Enabled = False
      TabOrder = 1
      OnClick = btnDelClick
    end
    object btnRun: TButton
      Left = 9
      Top = 371
      Width = 75
      Height = 25
      Caption = 'Run Batch'
      TabOrder = 2
      OnClick = btnRunClick
    end
    object btnScanFolder: TButton
      Left = 90
      Top = 175
      Width = 144
      Height = 25
      Caption = 'Add && Scan Folder'
      TabOrder = 3
      OnClick = btnScanFolderClick
    end
    object btnBrowseOutput: TButton
      Left = 793
      Top = 66
      Width = 349
      Height = 25
      Caption = 'Browse'
      TabOrder = 4
      OnClick = btnBrowseOutputClick
    end
    object Button4: TButton
      Left = 90
      Top = 371
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 5
    end
    object edtOutputPath: TEdit
      Left = 793
      Top = 37
      Width = 349
      Height = 23
      TabOrder = 6
      OnChange = edtOutputPathChange
    end
    object GroupBox1: TGroupBox
      Left = 365
      Top = 6
      Width = 422
      Height = 163
      Caption = 'Palette Controls (Only for GP2 && GP3 SW JAMS)'
      TabOrder = 7
      object panel: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 19
        Width = 419
        Height = 125
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 0
        object Label2: TLabel
          Left = 10
          Top = 10
          Width = 188
          Height = 15
          Caption = 'Input Image Simplification Method:'
          StyleName = 'Windows'
        end
        object Label1: TLabel
          Left = 10
          Top = 71
          Width = 71
          Height = 15
          Caption = 'Blur Amount:'
          StyleName = 'Windows'
        end
        object Label3: TLabel
          Left = 113
          Top = 71
          Width = 102
          Height = 15
          Caption = 'Simplify Threshold:'
          StyleName = 'Windows'
        end
        object cbSimplify: TComboBox
          Left = 10
          Top = 29
          Width = 208
          Height = 23
          Style = csDropDownList
          ItemIndex = 3
          TabOrder = 0
          Text = 'Neighbour Threshold (Usually best)'
          StyleName = 'Windows'
          OnChange = cbSimplifyChange
          Items.Strings = (
            'Quad Tree Method (Blocky)'
            'Seed Threshold'
            'Mean Region '
            'Neighbour Threshold (Usually best)')
        end
        object chkSimpPalette: TCheckBox
          Left = 248
          Top = 49
          Width = 136
          Height = 18
          Caption = 'Simplify All Palettes'
          TabOrder = 1
          StyleName = 'Windows'
          OnClick = chkSimpPaletteClick
        end
        object chkDoMatte: TCheckBox
          Left = 248
          Top = 73
          Width = 135
          Height = 18
          Caption = 'Don'#39't Soften Matte'
          TabOrder = 2
          StyleName = 'Windows'
          OnClick = chkDoMatteClick
        end
        object seBlur: TNumberBox
          Left = 10
          Top = 92
          Width = 97
          Height = 23
          MinValue = 1.000000000000000000
          MaxValue = 10.000000000000000000
          TabOrder = 3
          Value = 1.000000000000000000
          SpinButtonOptions.Placement = nbspCompact
          StyleName = 'Windows'
          UseMouseWheel = True
          OnChange = seBlurChange
        end
        object seThreshold: TNumberBox
          Left = 115
          Top = 90
          Width = 103
          Height = 23
          MaxValue = 100.000000000000000000
          TabOrder = 4
          Value = 20.000000000000000000
          SpinButtonOptions.Placement = nbspCompact
          StyleName = 'Windows'
          UseMouseWheel = True
          OnChange = seThresholdChange
        end
        object chkDoPalette: TCheckBox
          Left = 248
          Top = 97
          Width = 161
          Height = 17
          Caption = 'Do Not Generate Palettes'
          TabOrder = 5
          StyleName = 'Windows'
          OnClick = chkDoPaletteClick
        end
      end
    end
    object GroupBox2: TGroupBox
      Left = 9
      Top = 62
      Width = 350
      Height = 107
      Caption = 'Output JAM Format'
      TabOrder = 8
      object radioGP2: TRadioButton
        Left = 13
        Top = 24
        Width = 113
        Height = 17
        Caption = 'GP2 JAM'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = radioGP2Click
      end
      object radioGP3: TRadioButton
        Left = 13
        Top = 47
        Width = 113
        Height = 17
        Caption = 'GP3 SW JAM'
        TabOrder = 1
        OnClick = radioGP3Click
      end
      object radioGP3HW: TRadioButton
        Left = 13
        Top = 70
        Width = 113
        Height = 17
        Caption = 'GP3 HW JAM'
        TabOrder = 2
        OnClick = radioGP3HWClick
      end
    end
    object strJamFile: TEdit
      Left = 9
      Top = 25
      Width = 350
      Height = 23
      ReadOnly = True
      TabOrder = 9
    end
    object edtFilename: TEdit
      Left = 793
      Top = 144
      Width = 349
      Height = 23
      TabOrder = 10
      OnChange = edtFilenameChange
    end
    object chkScanAllFolders: TCheckBox
      Left = 254
      Top = 175
      Width = 163
      Height = 17
      Caption = 'Scan All child folders?'
      TabOrder = 11
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1196
    Height = 345
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 5
    Caption = 'Panel1'
    TabOrder = 0
    ExplicitWidth = 1190
    object lvBatch: TListView
      Left = 5
      Top = 5
      Width = 1186
      Height = 335
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'File'
        end
        item
          AutoSize = True
          Caption = 'Input JAM'
        end
        item
          AutoSize = True
          Caption = 'Output JAM'
        end
        item
          AutoSize = True
          Caption = 'Output Location'
        end
        item
          AutoSize = True
          Caption = 'Status'
        end
        item
          AutoSize = True
          Caption = 'Pal Simplification Method'
        end
        item
          AutoSize = True
          Caption = 'Blur Amount'
        end
        item
          AutoSize = True
          Caption = 'Simplify Threshold'
        end
        item
          AutoSize = True
          Caption = 'Simplify All Pals'
        end
        item
          AutoSize = True
          Caption = 'Soften Matte'
        end
        item
          AutoSize = True
          Caption = 'No Pal Generation'
        end>
      DoubleBuffered = True
      FullDrag = True
      MultiSelect = True
      StyleName = 'Windows'
      ReadOnly = True
      ParentDoubleBuffered = False
      PopupMenu = jamBatchPopup
      SortType = stBoth
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = lvBatchClick
      OnKeyDown = lvBatchKeyDown
      OnSelectItem = lvBatchSelectItem
    end
  end
  object jamBatchPopup: TPopupMenu
    OnPopup = jamBatchPopupPopup
    Left = 664
    Top = 700
    object AddFiles01: TMenuItem
      Caption = 'Add File'
      OnClick = btnAddFileClick
    end
    object AddScanFolder1: TMenuItem
      Caption = 'Add && Scan Folder'
      OnClick = btnScanFolderClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object DeleteItems1: TMenuItem
      Caption = 'Delete Item(s)'
      Enabled = False
      OnClick = DeleteItems1Click
    end
  end
end
