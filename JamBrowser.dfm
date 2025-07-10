object JamBrowser: TJamBrowser
  Left = 0
  Top = 0
  Caption = 'Jam Browser'
  ClientHeight = 766
  ClientWidth = 1198
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object SplitterLeft: TSplitter
    Left = 310
    Top = 40
    Width = 4
    Height = 684
    ExplicitLeft = 256
    ExplicitHeight = 532
  end
  object Splitter1: TSplitter
    Left = 894
    Top = 40
    Width = 4
    Height = 684
    Align = alRight
    ExplicitLeft = 620
    ExplicitTop = -95
    ExplicitHeight = 536
  end
  object SplitterFilmstrip: TSplitter
    Left = 0
    Top = 724
    Width = 1198
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = -372
    ExplicitTop = 437
    ExplicitWidth = 996
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 728
    Width = 1198
    Height = 38
    Panels = <
      item
        Width = 300
      end>
    ExplicitTop = 711
    ExplicitWidth = 1192
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 1198
    Height = 40
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 1192
    object jamLoading: TLabel
      Left = 1112
      Top = 12
      Width = 63
      Height = 15
      Caption = 'jamLoading'
      Visible = False
    end
    object SliderThumbSize: TTrackBar
      Left = 1
      Top = 1
      Width = 280
      Height = 38
      Align = alLeft
      Max = 256
      Min = 64
      Position = 128
      TabOrder = 0
      OnChange = SliderThumbSizeChange
    end
    object ProgressBar: TProgressBar
      Left = 314
      Top = 8
      Width = 574
      Height = 26
      Smooth = True
      Step = 1
      TabOrder = 1
      Visible = False
      StyleName = 'Windows'
    end
  end
  object PanelFolders: TPanel
    Left = 0
    Top = 40
    Width = 310
    Height = 684
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 667
    object directoryTree: TVirtualDrawTree
      Left = 1
      Top = 1
      Width = 308
      Height = 682
      Align = alClient
      DefaultNodeHeight = 19
      Header.AutoSizeIndex = 0
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
      Images = SystemImages
      TabOrder = 0
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
      OnChange = directoryTreeChange
      OnCompareNodes = directoryTreeCompareNodes
      OnDrawNode = directoryTreeDrawNode
      OnFreeNode = directoryTreeFreeNode
      OnGetImageIndex = directoryTreeGetImageIndex
      OnGetNodeWidth = directoryTreeGetNodeWidth
      OnInitChildren = directoryTreeInitChildren
      OnInitNode = directoryTreeInitNode
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coEditable, coStyleColor]
          Position = 0
          Width = 304
        end>
    end
  end
  object PanelPreview: TPanel
    Left = 898
    Top = 40
    Width = 300
    Height = 684
    Align = alRight
    TabOrder = 2
    ExplicitLeft = 892
    ExplicitHeight = 667
    object ImagePreview: TImage
      Left = 1
      Top = 1
      Width = 298
      Height = 312
      Align = alTop
      Center = True
      Proportional = True
      Stretch = True
    end
    object lblFilename: TLabel
      Left = 6
      Top = 334
      Width = 59
      Height = 15
      Caption = 'File Name: '
    end
    object lblJamType: TLabel
      Left = 6
      Top = 355
      Width = 54
      Height = 15
      Caption = 'JAM Type:'
    end
    object lblDimensions: TLabel
      Left = 6
      Top = 376
      Width = 68
      Height = 15
      Caption = 'Dimensions: '
    end
    object lblTexs: TLabel
      Left = 6
      Top = 398
      Width = 107
      Height = 15
      Caption = 'Number of Textures:'
    end
  end
  object jamListView: TEasyListview
    Left = 314
    Top = 40
    Width = 580
    Height = 684
    Align = alClient
    CellSizes.ReportThumb.Height = 135
    CellSizes.ReportThumb.Width = 112
    CellSizes.Thumbnail.Height = 128
    CellSizes.Thumbnail.Width = 128
    EditManager.Font.Charset = DEFAULT_CHARSET
    EditManager.Font.Color = clWindowText
    EditManager.Font.Height = -12
    EditManager.Font.Name = 'Segoe UI'
    EditManager.Font.Style = []
    Header.Columns.Items = {
      0600000001000000110000005445617379436F6C756D6E53746F726564FFFECE
      000600000080080001010001000000000000015F000000FFFFFF1F0001000000
      00000000000000000000000000000000}
    Header.Height = 23
    PaintInfoGroup.MarginBottom.CaptionIndent = 4
    PaintInfoItem.CheckFlat = True
    PaintInfoItem.GridLines = True
    PaintInfoItem.HorzGridLine = True
    PaintInfoItem.VertGridLine = True
    PaintInfoItem.ImageIndent = 10
    PaintInfoItem.ShowBorder = False
    PaintInfoItem.VAlignment = cvaBottom
    ShowThemedBorder = False
    TabOrder = 3
    View = elsThumbnail
    OnItemDblClick = jamListViewItemDblClick
    OnItemFreeing = jamListViewItemFreeing
    OnItemImageDraw = jamListViewItemImageDraw
    OnItemImageDrawIsCustom = jamListViewItemImageDrawIsCustom
    OnItemSelectionChanged = jamListViewItemSelectionChanged
    ExplicitWidth = 574
    ExplicitHeight = 667
  end
  object SystemImages: TImageList
    Left = 164
    Top = 316
  end
end
