object RCRPreviewForm: TRCRPreviewForm
  Left = 0
  Top = 0
  Caption = 'RCR Car Preview'
  ClientHeight = 660
  ClientWidth = 580
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  TextHeight = 15
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 580
    Height = 120
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 0
    object lblGame: TLabel
      Left = 16
      Top = 14
      Width = 36
      Height = 15
      Caption = 'Game:'
    end
    object cboGame: TComboBox
      Left = 90
      Top = 10
      Width = 220
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = cboGameChange
    end
    object lblAngle: TLabel
      Left = 16
      Top = 44
      Width = 65
      Height = 15
      Caption = 'RCR Angle:'
    end
    object cboAngle: TComboBox
      Left = 90
      Top = 40
      Width = 130
      Height = 23
      Style = csDropDownList
      TabOrder = 1
    end
    object lblLivery: TLabel
      Left = 16
      Top = 74
      Width = 35
      Height = 15
      Caption = 'Livery:'
    end
    object cboLivery: TComboBox
      Left = 90
      Top = 70
      Width = 220
      Height = 23
      Style = csDropDownList
      TabOrder = 2
    end
    object btnRender: TButton
      Left = 326
      Top = 10
      Width = 100
      Height = 29
      Caption = 'Render'
      Default = True
      TabOrder = 3
      OnClick = btnRenderClick
    end
    object lblStatus: TLabel
      Left = 326
      Top = 48
      Width = 240
      Height = 60
      AutoSize = False
      Caption = 'Select options above and click Render.'
      WordWrap = True
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 120
    Width = 580
    Height = 540
    Align = alClient
    TabOrder = 1
    object imgPreview: TImage
      Left = 0
      Top = 0
      Width = 256
      Height = 256
      AutoSize = True
    end
  end
end
