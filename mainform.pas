unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Math,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.NumberBox, Vcl.ExtDlgs,
  Vcl.CustomizeDlg, Vcl.Menus,
  Vcl.ImgList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Vcl.VirtualImageList,

  JamGeneral, JamSW, JamHW, gp2mask, JamPalette, GeneralHelpers,
  System.ImageList,
  Vcl.ToolWin, Vcl.Tabs, Vcl.Samples.Spin, Vcl.Mask, Vcl.Grids, Vcl.ValEdit,
  Vcl.CheckLst, Vcl.Imaging.jpeg, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees.DrawTree;

type

  TJamTreeNodeID = (jamDimensions, jamPos, jamFlags, jamColour, jamUnk, jamID,
    canvasDimensions, rootNode);

  TJamTreeNode = class
    data: integer;
    jamID: integer;
    editNode: boolean;
    nodeType: TJamTreeNodeID;
  end;

  PJamTreeNode = ^TJamTreeNode;

  TFormMain = class(TForm)
    JamTree: TTreeView;
    dlgOpenJam: TOpenDialog;
    ScrollBox1: TScrollBox;
    ImageCanvas: TImage;
    dlgSaveJam: TSaveDialog;
    canvasPopupMenu: TPopupMenu;
    popupImportTexture: TMenuItem;
    popupExportTexture: TMenuItem;
    importDialog: TOpenPictureDialog;
    exportDialog: TSaveDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Image1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    mainMenuClose: TMenuItem;
    N1: TMenuItem;
    mainMenuSave: TMenuItem;
    mainMenuSaveAs: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    N4: TMenuItem;
    Browser1: TMenuItem;
    mainMenuAddTexture: TMenuItem;
    N5: TMenuItem;
    mainMenuCut: TMenuItem;
    mainMenuCopy: TMenuItem;
    mainMenuPaste: TMenuItem;
    N6: TMenuItem;
    mainMenuModifyPalette: TMenuItem;
    N7: TMenuItem;
    mainMenuTextureProperties: TMenuItem;
    mainMenuImportTexture: TMenuItem;
    mainMenuImportCanvas: TMenuItem;
    N8: TMenuItem;
    mainMenuExportTexture: TMenuItem;
    mainMenuExportCanvas: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    toolBar_Save: TToolButton;
    ToolButton4: TToolButton;
    toolBar_AddTexture: TToolButton;
    toolBar_DeleteTexture: TToolButton;
    ToolButton7: TToolButton;
    toolBar_ImportTexture: TToolButton;
    toolBar_ExportTexture: TToolButton;
    ToolButton10: TToolButton;
    toolBar_GP2PAL: TToolButton;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    rightPanel: TPanel;
    toolBar_GP3PAL: TToolButton;
    ToolButton11: TToolButton;
    toolBar_PalPrev: TToolButton;
    toolBar_Pal1: TToolButton;
    toolBar_Pal2: TToolButton;
    toolBar_Pal3: TToolButton;
    toolBar_Pal4: TToolButton;
    toolBar_PalNext: TToolButton;
    VirtualImageList1: TVirtualImageList;
    ImageCollection1: TImageCollection;
    btnDrawData: TButton;
    panel_textureProperties: TPanel;
    Label4: TLabel;
    tex_X: TSpinEdit;
    tex_Y: TSpinEdit;
    tex_width: TSpinEdit;
    tex_height: TSpinEdit;
    tex_ID: TSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    panel_TexProperties_Generic: TPanel;
    panel_PalPreview: TPanel;
    PaintBoxPalette: TPaintBox;
    panel_TexPreview: TPanel;
    ImageEntry: TImage;
    panel_flags: TPanel;
    Label10: TLabel;
    tex_flags: TCheckListBox;
    panel_PaletteEdit: TPanel;
    Label2: TLabel;
    comboSimpMethod: TComboBox;
    chkBoxSimpPal: TCheckBox;
    chkBoxTrans: TCheckBox;
    numBox_BlurAmount: TNumberBox;
    Label1: TLabel;
    Label3: TLabel;
    numBox_SimpThresh: TNumberBox;
    btnGenPal: TButton;
    btnRemovePal: TButton;
    btnPal0: TButton;
    btnPal1: TButton;
    btnPal2: TButton;
    btnPal3: TButton;
    panel_RCR: TPanel;
    rcrOdd: TButton;
    rcrEven: TButton;
    Label11: TLabel;
    rcrReset: TButton;
    newTex: TButton;
    leftPanel: TPanel;
    timer_JamRedrawPals: TTimer;
    Button1: TButton;
    timer_redrawTree: TTimer;
    Button2: TButton;
    chkGP2Car: TCheckBox;
    Button3: TButton;
    N9: TMenuItem;
    popUpAddTexture: TMenuItem;
    N10: TMenuItem;
    popUpDeleteTexture: TMenuItem;
    N11: TMenuItem;
    mainMenuDeleteTexture: TMenuItem;
    procedure JamTreeChange(Sender: TObject; Node: TTreeNode);
    procedure PaintBoxPalettePaint(Sender: TObject);
    procedure btnLoadJamClick(Sender: TObject);
    procedure btnGP2palClick(Sender: TObject);
    procedure btnGP3palClick(Sender: TObject);
    procedure btnPal0Click(Sender: TObject);
    procedure btnPal1Click(Sender: TObject);
    procedure btnPal2Click(Sender: TObject);
    procedure btnPal3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDrawDataClick(Sender: TObject);
    procedure ImageCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageCanvasMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);

    procedure btnSaveJamClick(Sender: TObject);
    procedure ImportTexture(Sender: TObject);
    procedure btnExportTextureClick(Sender: TObject);
    procedure toolBar_PalPrevClick(Sender: TObject);
    procedure toolBar_PalNextClick(Sender: TObject);
    procedure comboSimpMethodChange(Sender: TObject);
    procedure numBox_SimpThreshChange(Sender: TObject);
    procedure numBox_BlurAmountChange(Sender: TObject);
    procedure chkBoxSimpPalClick(Sender: TObject);
    procedure chkBoxTransClick(Sender: TObject);
    procedure btnGenPalClick(Sender: TObject);
    procedure btnRemovePalClick(Sender: TObject);
    procedure Browser1Click(Sender: TObject);
    procedure Splitter2Paint(Sender: TObject);
    procedure tex_flagsClickCheck(Sender: TObject);
    procedure tex_IDChange(Sender: TObject);
    procedure tex_XChange(Sender: TObject);
    procedure tex_YChange(Sender: TObject);
    procedure tex_widthChange(Sender: TObject);
    procedure tex_heightChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure ScrollBox1Click(Sender: TObject);
    procedure rcrOddClick(Sender: TObject);
    procedure rcrEvenClick(Sender: TObject);
    procedure rcrResetClick(Sender: TObject);
    procedure AddNewTexture(Sender: TObject);
    procedure JamTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure JamTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: boolean);
    procedure mainMenuAddTextureClick(Sender: TObject);
    procedure mainMenuImportTextureClick(Sender: TObject);
    procedure timer_JamRedrawPalsTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure timer_redrawTreeTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure popUpDeleteTextureClick(Sender: TObject);

    procedure mainMenuExportCanvasClick(Sender: TObject);
    procedure canvasPopupMenuPopup(Sender: TObject);

  public
    FJamFile: TJamFile;
    FHWJamFile: THWJamFile;
    UpdatingFromCode: boolean;
    SelectFromCanvas: boolean;
    procedure RefreshCanvas;
    procedure RefreshPalette();
    procedure DrawTexture();
    procedure PalChange(palID: integer);
    procedure JamReGen;
    procedure TreeReDraw;
    procedure DrawTree;
    procedure SelectTexture(jamID: integer);
    procedure DeSelectTexture();
    procedure NewJam(filename: string; hwJAM: boolean; height: integer);
    procedure LoadJam(filename: string);
    procedure UpdateJamData(jamID: integer);
    procedure SelectTreeTex();
    procedure ClearPaletteImg();
    function AddJamTreeNode(Tree: TTreeView; Parent: TTreeNode;
      const Caption: string; data: integer; jamID: integer; editNode: boolean;
      nodeType: TJamTreeNodeID): TTreeNode;
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses JamBrowser;

procedure SaveExpandedNodes(Tree: TTreeView; ExpandedPaths: TStrings;
  out SelectedPath: string);
var
  Node: TTreeNode;
  function GetNodePath(Node: TTreeNode): string;
  begin
    Result := '';
    while Node <> nil do
    begin
      Result := Node.Text + '/' + Result;
      Node := Node.Parent;
    end;
    Result := '/' + Result;
  end;

begin
  ExpandedPaths.Clear;
  Node := Tree.Items.GetFirstNode;
  while Node <> nil do
  begin
    if Node.Expanded then
      ExpandedPaths.Add(GetNodePath(Node));
    Node := Node.GetNext;
  end;

  if Assigned(Tree.Selected) then
    SelectedPath := GetNodePath(Tree.Selected)
  else
    SelectedPath := '';
end;

procedure RestoreExpandedNodes(Tree: TTreeView; ExpandedPaths: TStrings;
  const SelectedPath: string);
var
  Node: TTreeNode;

  function FindNodeByPath(Path: string): TTreeNode;
  var
    Parts: TArray<string>;
    I: integer;
    Current: TTreeNode;
  begin
    Result := nil;
    if Path = '' then
      Exit;

    Parts := Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);
    Current := nil;

    for I := 0 to High(Parts) do
    begin
      Result := nil;
      if Current = nil then
        Current := Tree.Items.GetFirstNode
      else
        Current := Current.getFirstChild;

      while Current <> nil do
      begin
        if SameText(Current.Text, Parts[I]) then
        begin
          Result := Current;
          Break;
        end;
        Current := Current.getNextSibling;
      end;

      if Result = nil then
        Exit;
      Current := Result;
    end;

    Result := Current;
  end;

var
  I: integer;
  NodePath: string;
begin
  for I := 0 to ExpandedPaths.Count - 1 do
  begin
    NodePath := ExpandedPaths[I];
    Node := FindNodeByPath(NodePath);
    if Assigned(Node) then
      Node.Expand(False);
  end;

  Node := FindNodeByPath(SelectedPath);
  if Assigned(Node) then
    Tree.Selected := Node;
end;

function TFormMain.AddJamTreeNode(Tree: TTreeView; Parent: TTreeNode;
  const Caption: string; data: integer; jamID: integer; editNode: boolean;
  nodeType: TJamTreeNodeID): TTreeNode;
var
  jamNode: TJamTreeNode;
begin

  jamNode := TJamTreeNode.Create;
  jamNode.data := data;
  jamNode.jamID := jamID;
  jamNode.editNode := editNode;
  jamNode.nodeType := nodeType;

  Result := Tree.Items.AddChild(Parent, Caption);
  Result.data := jamNode;

end;

procedure TFormMain.DrawTree();
var
  JamRoot, ItemsRoot, Node, ScalingNode, UnTexNode, UnkNode, CanvasHeightNode
    : TTreeNode;
  I, j: integer;
  E: TJamEntry;
  hwEntry: THWJamEntry;
  Tree: TTreeView;
begin
  Tree := JamTree;
  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;

    if boolHWJAM then
    begin
      JamRoot := Tree.Items.Add(nil, FHWJamFile.JamFileName);
      ItemsRoot := AddJamTreeNode(Tree, JamRoot,
        Format('JAM Items: %d', [FHWJamFile.FHeader.NumItems]),
        FHWJamFile.FHeader.NumItems, -1, False, rootNode);

      for I := 0 to FHWJamFile.FEntries.Count - 1 do
      begin
        hwEntry := FHWJamFile.FEntries[I];

        Node := AddJamTreeNode(Tree, ItemsRoot, Format('ID:%d  [%d x %d]',
          [hwEntry.Info.jamID, hwEntry.Info.Width, hwEntry.Info.height]), 0, I,
          False, jamID);

        AddJamTreeNode(Tree, Node, Format('X:%d', [hwEntry.Info.X]),
          hwEntry.Info.X, I, True, jamPos);
        AddJamTreeNode(Tree, Node, Format('Y:%d', [hwEntry.Info.Y]),
          hwEntry.Info.Y, I, True, jamPos);
        AddJamTreeNode(Tree, Node, Format('Width:%d', [hwEntry.Info.Width]),
          hwEntry.Info.Width, I, False, jamDimensions);
        AddJamTreeNode(Tree, Node, Format('Height:%d', [hwEntry.Info.height]),
          hwEntry.Info.height, I, False, jamDimensions);
        AddJamTreeNode(Tree, Node, Format('JamFlags:%d', [hwEntry.Info.jamFlags]
          ), hwEntry.Info.jamFlags, I, False, jamFlags);
      end;

      AddJamTreeNode(Tree, JamRoot, Format('Canvas Size: W: %d x H: %d',
        [intjamMaxWidth, FHWJamFile.FHeader.JamTotalHeight]),
        FHWJamFile.FHeader.JamTotalHeight, -1, True, canvasDimensions);
    end
    else
    begin
      with FJamFile do
      begin
        JamRoot := Tree.Items.Add(nil, JamFileName);

        ItemsRoot := AddJamTreeNode(Tree, JamRoot,
          Format('JAM Items: %d', [FHeader.NumItems]), FHeader.NumItems, -1,
          False, rootNode);

        for I := 0 to FEntries.Count - 1 do
        begin
          E := FEntries[I];

          Node := AddJamTreeNode(Tree, ItemsRoot, Format('ID:%d  [%d x %d]',
            [E.Info.jamID, E.Info.Width, E.Info.height]), 0, I, False, jamID);

          AddJamTreeNode(Tree, Node, Format('X:%d', [E.Info.X]), E.Info.X, I,
            True, jamPos);
          AddJamTreeNode(Tree, Node, Format('Y:%d', [E.Info.Y]), E.Info.Y, I,
            True, jamPos);
          AddJamTreeNode(Tree, Node, Format('Width:%d', [E.Info.Width]),
            E.Info.Width, I, False, jamDimensions);
          AddJamTreeNode(Tree, Node, Format('Height:%d', [E.Info.height]),
            E.Info.height, I, False, jamDimensions);
          AddJamTreeNode(Tree, Node, Format('JamFlags:%d', [E.Info.jamFlags]),
            E.Info.jamFlags, I, False, jamFlags);
          AddJamTreeNode(Tree, Node, Format('PaletteSize:%d',
            [E.Info.PaletteSizeDiv4]), E.Info.PaletteSizeDiv4, I, False, jamID);

          ScalingNode := AddJamTreeNode(Tree, Node, 'Scaling Info', 0, I,
            False, jamID);
          AddJamTreeNode(Tree, ScalingNode, Format('Idx08:%d', [E.Info.Idx08]),
            E.Info.Idx08, I, False, jamID);
          AddJamTreeNode(Tree, ScalingNode, Format('Idx0A:%d', [E.Info.Idx0A]),
            E.Info.Idx0A, I, False, jamID);

          UnTexNode := AddJamTreeNode(Tree, Node, 'Untextured Colour', 0, I,
            False, jamID);
          AddJamTreeNode(Tree, UnTexNode, Format('Primary: %d', [E.Info.Idx16]),
            E.Info.Idx16, I, True, jamColour);
          AddJamTreeNode(Tree, UnTexNode, Format('Secondary: %d', [E.Info.Idx17]
            ), E.Info.Idx17, I, True, jamColour);

          UnkNode := AddJamTreeNode(Tree, Node, 'Unknown Data', 0, I,
            False, jamUnk);
          AddJamTreeNode(Tree, UnkNode, Format('UNK:%d', [E.Info.Unk]),
            E.Info.Unk, I, False, jamUnk);
          AddJamTreeNode(Tree, UnkNode, Format('Idx0E:%d', [E.Info.Idx0E]),
            E.Info.Idx0E, I, False, jamUnk);
          for j := 0 to 7 do
            AddJamTreeNode(Tree, UnkNode, Format('Idx18_0%d:%d',
              [j, E.Info.Idx18[j]]), E.Info.Idx18[j], I, False, jamUnk);
        end;
        CanvasHeightNode := AddJamTreeNode(Tree, JamRoot,
          Format('Canvas Size: W: %d x H: %d', [intjamMaxWidth,
          FHeader.JamTotalHeight]), FHeader.JamTotalHeight, -1, True,
          canvasDimensions);
      end;
    end;

    Tree.TopItem.Expanded := True;
  finally
    Tree.Items.EndUpdate;

  end;
end;

procedure TFormMain.mainMenuExportCanvasClick(Sender: TObject);
var
  tempBMP: TBitmap;
begin
  if boolJamLoaded = True then
  begin
    if exportDialog.Execute then
      tempBMP := FJamFile.DrawJamCanvas(False);
    tempBMP.PixelFormat := pf8bit;
    tempBMP.Palette := creategpxpal;
    tempBMP.SaveToFile(exportDialog.filename);
  end;

end;

procedure TFormMain.ToolButton1Click(Sender: TObject);
begin
if boolJamLoaded then
deselecttexture();

boolTexSelected := false;

  NewJam('test', False, 256);
end;

procedure TFormMain.TreeReDraw();
var
  Expanded: TStringList;
  SelectedPath: string;
begin
  Expanded := TStringList.Create;
  try
    SaveExpandedNodes(JamTree, Expanded, SelectedPath);
    DrawTree;
    RestoreExpandedNodes(JamTree, Expanded, SelectedPath);
  finally
    Expanded.Free;
  end;
end;

procedure TFormMain.JamReGen;
var
  tmpCanvas: TBitmap;
  bmpPal: TBitmap;
  I: integer;

begin
  if not boolJamLoaded then
    Exit;

  I := intSelectedTexture;
  if boolHWJAM then
  begin
    tmpCanvas := FHWJamFile.FEntries[I].FOriginalTex;
    tmpCanvas := StretchF(tmpCanvas, FHWJamFile.FEntries[I].Info.Width,
      FHWJamFile.FEntries[I].Info.height);


    FHWJamFile.FEntries[I].FTexture.height := FHWJamFile.FEntries[I]
      .Info.height;
    FHWJamFile.FEntries[I].FTexture.Width := FHWJamFile.FEntries[I].Info.Width;
    FHWJamFile.FEntries[I].FTexture := tmpCanvas;

    DrawTexture;
  end

  else
  begin

    intSimplifyThreshold := Round(numBox_SimpThresh.Value);
    intSimplifyMethod := comboSimpMethod.ItemIndex;
    intBlurThreshold := Round(numBox_BlurAmount.Value);
    boolSimpifyAllPals := chkBoxSimpPal.Checked;
    boolProtectTrans := chkBoxTrans.Checked;

    boolGp2Livery := chkGP2Car.Checked;

    tmpCanvas := TBitmap.Create;
    try
      tmpCanvas.Assign(FJamFile.FEntries[I].FOriginalTex);

      tmpCanvas := StretchF(tmpCanvas, FJamFile.FEntries[I].FInfo.Width,
        FJamFile.FEntries[I].FInfo.height);

      if FJamFile.FEntries[I].PaletteSizeDiv4 > 0 then
        generatePal := True;

      if generatePal then
      begin
        if assigned(FJamFile.FEntries[I].FTexture) then
        begin
           freeandnil(FJamFile.FEntries[I].FTexture);
           FJamFile.FEntries[I].FTexture := TBitmap.Create;
        end;

        FJamFile.FEntries[I].FTexture := FJamFile.GenerateGPxBMP(tmpCanvas, I);
        generatePal := False;

      end
      else
      begin
        bmpPal := TBitmap.Create;
        bmpPal.Assign(tmpCanvas);
        bmpPal.PixelFormat := pf8bit;
        bmpPal := CreateGPxPalBMP(tmpCanvas);
        bmpPal.Palette := creategpxpal;
        FJamFile.EncodeTexture(I, bmpPal);
        bmppal.free;
      end;

      FJamFile.CachePaletteBMP(I);

      RefreshPalette;
      RefreshCanvas;
      DrawTexture;

      chkBoxTrans.Checked := DetectTransCol(FJamFile.FEntries[I].FTexture);

    finally
      tmpCanvas.Free;
    end;
  end;
end;

procedure TFormMain.DrawTexture;
begin
  if (not boolJamLoaded) or (intSelectedTexture < 0) then
    Exit;

  if boolHWJAM then
    ImageEntry.Picture.Bitmap := FHWJamFile.FEntries
      [intSelectedTexture].FTexture
  else
    ImageEntry.Picture.Bitmap := FJamFile.FEntries[intSelectedTexture]
      .FCachedTex[intPaletteID];
end;

procedure TFormMain.PalChange(palID: integer);
var
  X: integer;
  btnArray: array [0 .. 3] of TToolButton;
begin

  if boolJamLoaded = True then
  begin

    btnArray[0] := toolBar_Pal1;
    btnArray[1] := toolBar_Pal2;
    btnArray[2] := toolBar_Pal3;
    btnArray[3] := toolBar_Pal4;

    intPaletteID := palID;

    for X := 0 to 3 do
      if X = intPaletteID then
        btnArray[X].down := True;

    RefreshPalette();
    RefreshCanvas;
    if intSelectedTexture > -1 then
      DrawTexture
  end;
end;

procedure TFormMain.Browser1Click(Sender: TObject);
begin
  JamBrowser.JamBrowserFrm.Show;
end;

procedure TFormMain.btnDrawDataClick(Sender: TObject);
begin
  if boolJamLoaded = True then
  begin

    boolDrawOutlines := not boolDrawOutlines;
    RefreshCanvas;
  end;
end;

procedure TFormMain.btnExportTextureClick(Sender: TObject);
var
  tempBMP: TBitmap;
begin
  if boolJamLoaded = True then
  begin
    if exportDialog.Execute then
    if boolHWJAM then
    FHWJamFile.ExportTexture(intSelectedTexture, exportDialog.FileName)
    else
    begin
      tempBMP := FJamFile.FEntries[intSelectedTexture].FCachedTex[0];
      tempBMP.PixelFormat := pf8bit;
      tempBMP.Palette := creategpxpal;
      tempBMP.SaveToFile(exportDialog.filename);
    end;
  end;
end;

procedure TFormMain.btnGP2palClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to 255 do
    GPXPal[I] := Gp2Pal[I];

  if boolJamLoaded = True then
  begin

    RefreshPalette;
    RefreshCanvas;

    chkGP2Car.Visible := True;
    boolGP2Jam := True;
    boolGP3Jam := False;
    boolHWJAM := False;
  end;

end;

procedure TFormMain.btnGP3palClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to 255 do
    GPXPal[I] := Gp3Pal[I];

  if boolJamLoaded = True then
  begin

    RefreshPalette;
    RefreshCanvas;

    chkGP2Car.Visible := False;

    boolGP2Jam := False;
    boolGP3Jam := True;
    boolHWJAM := False;

  end;
end;

procedure TFormMain.NewJam(filename: string; hwJAM: boolean; height: integer);
var
  I: integer;
begin
  if hwJAM then
  begin
    boolHWJAM := True;
    if panel_PalPreview.Visible then
      ClearPaletteImg;

    panel_PalPreview.Visible := False;
    panel_PaletteEdit.Visible := False;

    FHWJamFile := THWJamFile.Create;
  end
  else
  begin
    boolHWJAM := False;

    if assigned(FJamFile) then
    FJamFile.free;

    FJamFile := TJamFile.Create;

    FJamFile.CreateNewJAM(filename, height);

    for I := 0 to 255 do
      localGpxPal[I] := GPXPal[I];

    PalChange(0);

    panel_PaletteEdit.Visible := True;
  end;

  intSelectedTexture := -1;
  intJamZoom := 1;

  DrawTree;

  RefreshCanvas;

  SetStretchBltMode(ImageCanvas.Canvas.Handle, HALFTONE);

  panel_RCR.Visible := False;

  boolJamLoaded := True;

end;

procedure TFormMain.LoadJam(filename: string);
var
  I: integer;
begin
boolTexSelected := false;

  if isHWJAM(filename) then
  begin
    boolHWJAM := True;
    if Assigned(FHWJamFile) then
      FreeAndNil(FHWJamFile);

    if Assigned(FJamFile) then
      FreeAndNil(FJamFile);

    if not Assigned(FHWJamFile) then
      FHWJamFile := THWJamFile.Create;

    boolJamLoaded := FHWJamFile.LoadFromFile(filename);
  end
  else
  begin
    boolHWJAM := False;

        if Assigned(FHWJamFile) then
      FreeAndNil(FHWJamFile);

    if Assigned(FJamFile) then
      FreeAndNil(FJamFile);


    if not Assigned(FJamFile) then
      FJamFile := TJamFile.Create;

    boolJamLoaded := FJamFile.LoadFromFile(filename, False);
  end;

  intSelectedTexture := -1;
  intJamZoom := 1;

  if boolJamLoaded then
  begin

    if boolHWJAM then
    begin
      if panel_PalPreview.Visible then
        ClearPaletteImg;

      panel_PalPreview.Visible := False;
      panel_PaletteEdit.Visible := False;

      toolBar_PalPrev.Visible := False;
      toolBar_Pal1.Visible := False;
      toolBar_Pal2.Visible := False;
      toolBar_Pal3.Visible := False;
      toolBar_Pal4.Visible := False;
      toolBar_PalNext.Visible := False;
      toolBar_GP2PAL.Enabled := False;
      toolBar_GP3PAL.Enabled := False;

      toolBar_Save.Enabled := True;
      toolBar_AddTexture.Enabled := True;

    end
    else
    begin
      for I := 0 to 255 do
        localGpxPal[I] := GPXPal[I];

      PalChange(0);

      panel_PaletteEdit.Visible := True;

      toolBar_PalPrev.Visible := True;
      toolBar_Pal1.Visible := True;
      toolBar_Pal2.Visible := True;
      toolBar_Pal3.Visible := True;
      toolBar_Pal4.Visible := True;
      toolBar_PalNext.Visible := True;
      toolBar_GP2PAL.Enabled := True;
      toolBar_GP3PAL.Enabled := True;

      toolBar_Save.Enabled := True;
      toolBar_AddTexture.Enabled := True;
    end;

    DrawTree;

    RefreshCanvas;

    SetStretchBltMode(ImageCanvas.Canvas.Handle, HALFTONE);

    if boolrcrJAM then
    begin
      panel_RCR.Visible := True;
      panel_PaletteEdit.Visible := False;
    end
    else
      panel_RCR.Visible := False;
  end;

end;

procedure TFormMain.btnLoadJamClick(Sender: TObject);
begin
  if dlgOpenJam.Execute then
  begin
    if (dlgOpenJam.filename <> '') and FileExists(dlgOpenJam.filename) then
    begin
      if boolHWJAM = True then
      begin
        if Assigned(FHWJamFile) then
        begin
          DeSelectTexture;
          FHWJamFile.Free;
          FHWJamFile := nil;
          JamTree.Items.Clear;
          boolJamLoaded := False;
        end;
      end
      else if Assigned(FJamFile) then
      begin
        DeSelectTexture;
        FJamFile.Free;
        FJamFile := nil;
        JamTree.Items.Clear;
        boolJamLoaded := False;
      end;
      loadjam(dlgOpenJam.FileName);
    end;
  end;
end;



procedure TFormMain.btnPal0Click(Sender: TObject);
begin
  PalChange(0);
end;

procedure TFormMain.btnPal1Click(Sender: TObject);
begin
  PalChange(1);
end;

procedure TFormMain.btnPal2Click(Sender: TObject);
begin
  PalChange(2);
end;

procedure TFormMain.btnPal3Click(Sender: TObject);
begin
  PalChange(3);
end;

procedure TFormMain.btnSaveJamClick(Sender: TObject);
begin
  if dlgSaveJam.Execute then
  begin
  if boolhwJam then
    FHWJamFile.SaveToFile(dlgSaveJam.filename)
  else
    FJamFile.SaveToFile(dlgSaveJam.filename)
  end;
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  I: integer;
begin

  for I := 0 to FJamFile.FEntries.Count - 1 do
    FJamFile.EncodeCanvas(I);

  ImageEntry.Picture.Bitmap := FJamFile.DrawJamCanvas(False);
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  if boolJamLoaded = True then
  begin
    if exportDialog.Execute then
      ImageCanvas.Picture.SaveToFile(exportDialog.filename);
  end;

end;

procedure TFormMain.Button3Click(Sender: TObject);
// var
// X, Y, Count: Integer;
// Color: TColor;
// Line: PRGBTripleArray;
// ByteList: TStringList;
// PixelVal: string;
// bmp : TBitmap;
// LineBuffer: string;
begin
  // bmp := TBitmap.create;
  // bmp.LoadFromFIle('C:\Users\james\OneDrive\coding\++WORKINGCODE++\gp3Jam2000_v006\assets\gp2Livery_MASK.bmp');
  //
  // Bmp.PixelFormat := pf24bit;
  // ByteList := TStringList.Create;
  // try
  // Count := 0;
  // LineBuffer := '';
  // for Y := 0 to Bmp.Height - 1 do
  // begin
  // Line := Bmp.ScanLine[Y];
  // for X := 0 to Bmp.Width - 1 do
  // begin
  // Color := RGB(Line[X].rgbtRed, Line[X].rgbtGreen, Line[X].rgbtBlue);
  // if Color = clBlack then
  // LineBuffer := LineBuffer + '$00,'
  // else
  // LineBuffer := LineBuffer + '$01,';
  //
  // Inc(Count);
  // if Count mod 16 = 0 then
  // begin
  // ByteList.Add(LineBuffer); // line ends in a comma
  // LineBuffer := '';
  // end;
  // end;
  // end;
  //
  // // Add remaining values (if bitmap width × height not divisible by 16)
  // if LineBuffer <> '' then
  // ByteList.Add(LineBuffer); // keep comma at end
  //
  // ByteList.SaveToFile('C:\Users\james\OneDrive\coding\++WORKINGCODE++\gp3Jam2000_v006\assets\gp2Livery_MASK.txt');
  // finally
  // ByteList.Free;
  // end;

  ImageCanvas.Picture.Bitmap := CreateTransparencyMatte
    (FJamFile.DrawFullJam(True));
end;

procedure TFormMain.rcrOddClick(Sender: TObject);
var
  X, Y, w: integer;
  srcLine, dstLine: PRGBTripleArray;
  srcBMP, dstBMP, scaledBMP: TBitmap;
  newHeight, newWidth: integer;
begin
  srcBMP := nil;
  dstBMP := nil;
  scaledBMP := nil;
  try
    // Get a cloned source bitmap (assuming DrawCanvas returns a new instance)
    srcBMP := FJamFile.DrawFullJam(True);
    srcBMP.PixelFormat := pf24bit;

    dstBMP := TBitmap.Create;
    dstBMP.PixelFormat := pf24bit;
    dstBMP.Width := srcBMP.Width;
    dstBMP.height := srcBMP.height;

    w := srcBMP.Width;

    for Y := 0 to srcBMP.height - 1 do
    begin
      srcLine := srcBMP.ScanLine[Y];
      dstLine := dstBMP.ScanLine[Y];
      X := 0;
      while X < w do
      begin
        if (X mod 2 = 0) and (X + 1 < w) then
        begin
          dstLine[X] := srcLine[X + 1];
          dstLine[X + 1] := srcLine[X + 1];
          Inc(X, 2);
        end
        else
        begin
          dstLine[X] := srcLine[X];
          Inc(X);
        end;
      end;
    end;

    newWidth := Round(dstBMP.Width * intJamZoom);
    newHeight := Round(dstBMP.height * intJamZoom);

    scaledBMP := TBitmap.Create;
    scaledBMP.Width := newWidth;
    scaledBMP.height := newHeight;
    scaledBMP.PixelFormat := pf24bit;

    SetStretchBltMode(scaledBMP.Canvas.Handle, HALFTONE);
    StretchBlt(scaledBMP.Canvas.Handle, 0, 0, newWidth, newHeight,
      dstBMP.Canvas.Handle, 0, 0, dstBMP.Width, dstBMP.height, SRCCOPY);

    ImageCanvas.Picture.Assign(scaledBMP);
    // Don't assign scaledBmp.Bitmap directly
  finally
    srcBMP.Free;
    dstBMP.Free;
    scaledBMP.Free;
  end;
end;

procedure TFormMain.rcrResetClick(Sender: TObject);
begin
  RefreshCanvas;
end;

procedure TFormMain.rcrEvenClick(Sender: TObject);
var
  X, Y, w: integer;
  srcLine, dstLine: PRGBTripleArray;
  srcBMP, dstBMP, scaledBMP: TBitmap;
  newHeight, newWidth: integer;
begin
  srcBMP := nil;
  dstBMP := nil;
  scaledBMP := nil;
  try
    // Get a cloned source bitmap (assuming DrawCanvas returns a new instance)
    srcBMP := FJamFile.DrawFullJam(False);
    srcBMP.PixelFormat := pf24bit;

    dstBMP := TBitmap.Create;
    dstBMP.PixelFormat := pf24bit;
    dstBMP.Width := srcBMP.Width;
    dstBMP.height := srcBMP.height;

    w := srcBMP.Width;

    for Y := 0 to srcBMP.height - 1 do
    begin
      srcLine := srcBMP.ScanLine[Y];
      dstLine := dstBMP.ScanLine[Y];
      X := 0;
      while X < w do
      begin
        if (X mod 2 = 0) then
        begin
          // Copy even column to itself and next (odd) column if valid
          dstLine[X] := srcLine[X];
          if X + 1 < w then
            dstLine[X + 1] := srcLine[X];
          Inc(X, 2);
        end
        else
        begin
          // Only reached if image width is odd and last column is odd
          dstLine[X] := srcLine[X];
          Inc(X);
        end;
      end;
    end;

    newWidth := Round(dstBMP.Width * intJamZoom);
    newHeight := Round(dstBMP.height * intJamZoom);

    scaledBMP := TBitmap.Create;
    scaledBMP.Width := newWidth;
    scaledBMP.height := newHeight;
    scaledBMP.PixelFormat := pf24bit;

    SetStretchBltMode(scaledBMP.Canvas.Handle, HALFTONE);
    StretchBlt(scaledBMP.Canvas.Handle, 0, 0, newWidth, newHeight,
      dstBMP.Canvas.Handle, 0, 0, dstBMP.Width, dstBMP.height, SRCCOPY);

    ImageCanvas.Picture.Assign(scaledBMP);
    // Don't assign scaledBmp.Bitmap directly
  finally
    srcBMP.Free;
    dstBMP.Free;
    scaledBMP.Free;
  end;
end;

procedure TFormMain.btnGenPalClick(Sender: TObject);

begin
  generatePal := True;
  JamReGen;
  TreeReDraw;
end;

procedure TFormMain.btnRemovePalClick(Sender: TObject);
begin
  FJamFile.ZeroPalette(intSelectedTexture);
  // PalChange(0);
  TreeReDraw;
  // RefreshCanvas;
end;

procedure TFormMain.canvasPopupMenuPopup(Sender: TObject);
begin

  popUpDeleteTexture.Enabled := boolTexSelected;
  popupImportTexture.Enabled := boolTexSelected;
  popupExportTexture.Enabled := boolTexSelected;
  popUpAddTexture.Enabled := boolJamLoaded;
end;

procedure TFormMain.chkBoxSimpPalClick(Sender: TObject);
begin
  // PaletteReGen;
end;

procedure TFormMain.chkBoxTransClick(Sender: TObject);
begin
  // PaletteReGen;
end;

procedure TFormMain.comboSimpMethodChange(Sender: TObject);
begin
  // PaletteReGen;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to 255 do
    GPXPal[I] := Gp2Pal[I];

  for I := 0 to 255 do
    localGpxPal[I] := GPXPal[I];

  intPaletteID := 0;

  intJamZoom := 1;

  intSimplifyThreshold := 20;
  intSimplifyMethod := 3;
  intBlurThreshold := 1;
  // intSimplifyDist := 20;
  boolSimpifyAllPals := False;
  boolProtectTrans := False;

  boolJamLoaded := False;

  boolHWJAM := False;

  UpdatingFromCode := False;

  timer_redrawTree.Enabled := False;

  comboSimpMethod.ItemIndex := intSimplifyMethod;

  numBox_SimpThresh.Value := Round(intSimplifyThreshold);
  numBox_BlurAmount.Value := intBlurThreshold;
  chkBoxSimpPal.Checked := boolSimpifyAllPals;
  chkBoxTrans.Checked := boolProtectTrans;

  with ImageEntry.Picture.Bitmap do
  begin
    Width := 1;
    height := 1;
    Canvas.Brush.Color := panel_TexPreview.Color; // or any background color
    Canvas.FillRect(Rect(0, 0, Width, height));
  end;

  PaintBoxPalette.Canvas.Brush.Color := panel_PalPreview.Color; // or clBtnFace
  PaintBoxPalette.Canvas.FillRect(PaintBoxPalette.ClientRect);

end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
const
  ZoomStep = 0.1;
var
  OldZoom, NewZoom: Double;
  CursorPos: TPoint;
  ImgPos: TPoint;
  RelX, RelY: Double;
begin

  if boolJamLoaded = False then
    Exit;

  // Convert screen to ScrollBox client coordinates
  CursorPos := ScrollBox1.ScreenToClient(MousePos);

  // Only zoom if the mouse is inside the ScrollBox
  if not PtInRect(ScrollBox1.ClientRect, CursorPos) then
    Exit;

  OldZoom := intJamZoom;

  if WheelDelta > 0 then
    intJamZoom := intJamZoom + ZoomStep
  else
    intJamZoom := Max(0.1, intJamZoom - ZoomStep);

  NewZoom := intJamZoom;

  // Preserve position under mouse
  CursorPos := ScrollBox1.ScreenToClient(MousePos);
  ImgPos := Point(ImageCanvas.Left, ImageCanvas.Top);
  RelX := (CursorPos.X - ImgPos.X) / OldZoom;
  RelY := (CursorPos.Y - ImgPos.Y) / OldZoom;

  intJamZoom := NewZoom;

  // Recenter scroll based on relative mouse position
  ScrollBox1.HorzScrollBar.Position := Round((RelX * NewZoom) - CursorPos.X);
  ScrollBox1.VertScrollBar.Position := Round((RelY * NewZoom) - CursorPos.Y);

  RefreshCanvas;
  Handled := True;
end;

procedure TFormMain.ImageCanvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  I, jamX, jamY, jamW, jamH: integer;
begin
  if Button <> mbLeft then
    Exit;

  if not boolJamLoaded or (Button <> mbLeft) then
    Exit;

  UpdatingFromCode := True;
  SelectFromCanvas := True;
  intSelectedTexture := -1;

  if boolHWJAM then
  begin
    for I := 0 to FHWJamFile.Entries.Count - 1 do
    begin
      with FHWJamFile.Entries[I].Info do
      begin
        jamX := Round(X * intJamZoom);
        jamY := Round(Y * intJamZoom);
        jamW := Round(Width * intJamZoom);
        jamH := Round(height * intJamZoom);
      end;

      if (X >= jamX) and (X < jamX + jamW) and (Y >= jamY) and (Y < jamY + jamH)
      then
      begin
        intSelectedTexture := I;
        Break;
      end;
    end;
  end
  else
  begin
    for I := 0 to FJamFile.Entries.Count - 1 do
    begin
      with FJamFile.Entries[I].Info do
      begin
        jamX := Round(X * intJamZoom);
        jamY := Round(Y * intJamZoom);
        jamW := Round(Width * intJamZoom);
        jamH := Round(height * intJamZoom);
      end;

      if (X >= jamX) and (X < jamX + jamW) and (Y >= jamY) and (Y < jamY + jamH)
      then
      begin
        intSelectedTexture := I;
        Break;
      end;
    end;
  end;

  if intSelectedTexture = -1 then
    DeSelectTexture
  else
    SelectTexture(intSelectedTexture);

  UpdatingFromCode := False;
  SelectFromCanvas := False;
end;

procedure TFormMain.JamTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: boolean);
var
  JNode: TJamTreeNode;
  TextRect: TRect;
  TextWidth: integer;
  SwatchRect: TRect;
  SwatchSize: integer;
  ColorValue: TColor;
  Canvas: TCanvas;
begin
  if Stage <> cdPostPaint then
    Exit;
  if not Assigned(Node) or not Assigned(Node.data) then
    Exit;

  JNode := TJamTreeNode(Node.data);
  if not(JNode.nodeType in [jamColour]) then
    Exit; // draw only on colour nodes

  Canvas := Sender.Canvas;

  // 1. Get the area where the node is drawn
  TextRect := Node.DisplayRect(True);

  // 2. Measure text width
  TextWidth := Canvas.TextWidth(Node.Text);

  // 3. Define swatch size and location (right of text, with margin)
  SwatchSize := 12;
  SwatchRect.Left := TextRect.Left + TextWidth + 8;
  SwatchRect.Top := TextRect.Top + ((TextRect.height - SwatchSize) div 2);
  SwatchRect.Right := SwatchRect.Left + SwatchSize;
  SwatchRect.Bottom := SwatchRect.Top + SwatchSize;

  // 4. Get the color to draw (example assumes grayscale from Data)
  ColorValue := RGBFromTRGB(GPXPal[JNode.data]); // Example grayscale

  // 5. Draw the color swatch
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := ColorValue;
    FillRect(SwatchRect);

    Pen.Color := clBlack;
    Rectangle(SwatchRect); // border
  end;

  DefaultDraw := True;
end;

procedure TFormMain.JamTreeChange(Sender: TObject; Node: TTreeNode);
var
  jamNode: TJamTreeNode;
begin
  if UpdatingFromCode = False then
  begin
    if not Assigned(Node) or not Assigned(Node.data) then
    begin
      DeSelectTexture;
      Exit;
    end;

    jamNode := TJamTreeNode(Node.data);

    case jamNode.nodeType of
      jamDimensions:
        begin
          SelectTexture(jamNode.jamID);
        end;

      jamPos:
        begin
          SelectTexture(jamNode.jamID);
        end;

      jamFlags:
        begin
          SelectTexture(jamNode.jamID);
        end;

      jamColour:
        begin
          SelectTexture(jamNode.jamID);
        end;

      jamUnk:
        begin
          SelectTexture(jamNode.jamID);
        end;

      jamID:
        begin
          SelectTexture(jamNode.jamID);
        end;

      canvasDimensions:
        begin
          DeSelectTexture();
        end;

      rootNode:
        begin
          DeSelectTexture();
        end;
    end;
  end;
end;

procedure TFormMain.JamTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.data) then
  begin
    TObject(Node.data).Free;
    Node.data := nil;
  end;
end;

procedure TFormMain.RefreshCanvas;
var
  bmp, scaledBMP: TBitmap;
  newWidth, newHeight: integer;
begin

  if boolHWJAM = False then
    bmp := FJamFile.DrawFullJam(True);

  if boolHWJAM = True then
    bmp := FHWJamFile.DrawCanvas(True);

  newWidth := Round(bmp.Width * intJamZoom);
  newHeight := Round(bmp.height * intJamZoom);

  try
    scaledBMP := TBitmap.Create;
    scaledBMP.height := newHeight;
    scaledBMP.Width := newWidth;

    SetStretchBltMode(scaledBMP.Canvas.Handle, HALFTONE);

    StretchBlt(scaledBMP.Canvas.Handle, 0, 0, newWidth, newHeight,
      bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.height, SRCCOPY);

    if boolHWJAM = False then
      scaledBMP := FJamFile.DrawOutlines(scaledBMP);

    if boolHWJAM = True then
      scaledBMP := FHWJamFile.DrawOutlines(scaledBMP);

    ImageCanvas.height := newHeight;
    ImageCanvas.Width := newWidth;
    ImageCanvas.Picture.Bitmap := scaledBMP;

  finally
    freeandnil(scaledBMP);
    freeandnil(bmp);
  end;

end;

procedure TFormMain.RefreshPalette();
begin
  if boolTexSelected then
    if FJamFile.Entries[intSelectedTexture].Info.PaletteSizeDiv4 = 0 then
      panel_PalPreview.Visible := False
    else
    begin
      panel_PalPreview.Visible := True;
      PaintBoxPalette.Repaint;
    end;
  PaintBoxPalette.Invalidate;
end;

procedure TFormMain.Splitter2Paint(Sender: TObject);
var
  Width: integer;
  height: integer;
  ratio: Double;
begin

  if boolJamLoaded = False then
    Exit;

  ratio := ImageEntry.Picture.Graphic.height / ImageEntry.Picture.Graphic.Width;

  ImageEntry.height := Round(ImageEntry.Width * ratio);

  if ImageEntry.height > 256 then
    ImageEntry.height := 256;

end;

procedure TFormMain.tex_flagsClickCheck(Sender: TObject);
begin
  if UpdatingFromCode then
    Exit;
  UpdateJamData(intSelectedTexture);
end;

procedure TFormMain.tex_heightChange(Sender: TObject);
begin
  if UpdatingFromCode then
    Exit;
  UpdateJamData(intSelectedTexture);
end;

procedure TFormMain.tex_IDChange(Sender: TObject);
begin
  if UpdatingFromCode then
    Exit;
  UpdateJamData(intSelectedTexture);
end;

procedure TFormMain.tex_widthChange(Sender: TObject);
begin
  if UpdatingFromCode then
    Exit;
  UpdateJamData(intSelectedTexture);
end;

procedure TFormMain.tex_XChange(Sender: TObject);
begin
  if UpdatingFromCode then
    Exit;
  UpdateJamData(intSelectedTexture);
end;

procedure TFormMain.tex_YChange(Sender: TObject);
begin
  if UpdatingFromCode then
    Exit;
  UpdateJamData(intSelectedTexture);
end;

procedure TFormMain.timer_JamRedrawPalsTimer(Sender: TObject);
begin
  timer_JamRedrawPals.Enabled := False;
  JamReGen;
end;

procedure TFormMain.timer_redrawTreeTimer(Sender: TObject);
begin
  timer_redrawTree.Enabled := False;
  TreeReDraw;
end;

procedure TFormMain.toolBar_PalNextClick(Sender: TObject);
var
  X: integer;
begin

  X := intPaletteID;

  if intPaletteID = 3 then
  begin
    PalChange(0);
  end
  else
  begin
    PalChange(intPaletteID + 1);
  end;
end;

procedure TFormMain.toolBar_PalPrevClick(Sender: TObject);
var
  X: integer;
begin

  if intPaletteID = 0 then
  begin
    PalChange(3);
  end
  else
  begin
    PalChange(intPaletteID - 1);
  end;

end;

procedure TFormMain.PaintBoxPalettePaint(Sender: TObject);
var
  Count, I: integer;
  PalBytes: TBytes;
  C: TColor;
  CellLeft, CellRight: integer;
  Scale: Double;
begin
  if not Assigned(FJamFile) then
    Exit;

  if intSelectedTexture > -1 then
  begin
    Count := FJamFile.Entries[intSelectedTexture].Info.PaletteSizeDiv4;
    if Count = 0 then
      Exit;

    PalBytes := FJamFile.Entries[intSelectedTexture].Palettes[intPaletteID];

    Scale := PaintBoxPalette.Width / Count;

    for I := 0 to Count - 1 do
    begin
      C := RGBFromTRGB(GPXPal[PalBytes[I]]);
      localGpxPal[I] := GPXPal[PalBytes[I]];

      // Use rounded boundaries
      CellLeft := Round(I * Scale);
      CellRight := Round((I + 1) * Scale);

      PaintBoxPalette.Canvas.Brush.Color := C;
      PaintBoxPalette.Canvas.FillRect(Rect(CellLeft, 0, CellRight,
        PaintBoxPalette.height));
    end;
  end;
end;

procedure TFormMain.mainMenuImportTextureClick(Sender: TObject);
begin
  ImportTexture(Sender);
end;

procedure TFormMain.ImportTexture(Sender: TObject);
begin
  if importDialog.Execute then
  begin
    if (importDialog.filename <> '') and FileExists(importDialog.filename) then
     if boolHWJAM then
      FHWJamFile.ImportTexture(intSelectedTexture, importDialog.filename)
     else
      FJamFile.ImportTexture(intSelectedTexture, importDialog.filename);
  end
  else
    Exit;

  RefreshCanvas;

  if not boolHWJam then
  RefreshPalette;

  DrawTexture;

end;

procedure TFormMain.ImageCanvasMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
// var
// DX, DY: Integer;
// NewRect: TRect;
// EInfo: TJamEntryInfo;
begin
  // inherited;
  // if FDragMode = dmNone then Exit;
  //
  // DX := X - FStartPt.X;
  // DY := Y - FStartPt.Y;
  // NewRect := FOrigRect;
  //
  // case FDragMode of
  // dmMove:
  // OffsetRect(NewRect, DX, DY);
  //
  // dmResizeTL:
  // begin
  // NewRect.Left := FOrigRect.Left + DX;
  // NewRect.Top  := FOrigRect.Top  + DY;
  // end;
  // dmResizeTR:
  // begin
  // NewRect.Right := FOrigRect.Right + DX;
  // NewRect.Top   := FOrigRect.Top   + DY;
  // end;
  // dmResizeBL:
  // begin
  // NewRect.Left   := FOrigRect.Left + DX;
  // NewRect.Bottom := FOrigRect.Bottom + DY;
  // end;
  // dmResizeBR:
  // begin
  // NewRect.Right  := FOrigRect.Right  + DX;
  // NewRect.Bottom := FOrigRect.Bottom + DY;
  // end;
  // end;
  //
  // // constrain minimum size, if you like
  // if NewRect.Width < 1 then NewRect.Right := NewRect.Left + 1;
  // if NewRect.Height < 1 then NewRect.Bottom := NewRect.Top + 1;
  //
  // // write back into the entry
  // EInfo := FJamFile.Entries[intSelectedTexture].Info;
  //
  // EInfo.X      := NewRect.Left;
  // EInfo.Y      := NewRect.Top;
  // EInfo.Width  := NewRect.Width;
  // EInfo.Height := NewRect.Height;
  //
  // FJamFile.Entries[intSelectedTexture].Info :=  EInfo;

  // redraw immediately
  // refreshCanvas;
end;

procedure TFormMain.AddNewTexture(Sender: TObject);

begin
  if importDialog.Execute then
  begin
    if (importDialog.filename <> '') and FileExists(importDialog.filename) then
    begin
      if boolHWJAM then
      begin
        FHWJamFile.AddTexture(importDialog.filename);
        intSelectedTexture := FHWJamFile.FEntries.Count - 1;

        JamReGen;

        UpdatingFromCode := True;

        RefreshCanvas;
        DrawTexture;
        DrawTree;
        SelectTreeTex;

        UpdatingFromCode := False;
      end
    else
    begin
      FJamFile.AddTexture(importDialog.filename);
      JamReGen;

      intSelectedTexture := FJamFile.FEntries.Count - 1;

      UpdatingFromCode := True;

//      RefreshCanvas;
//      RefreshPalette;
//      DrawTexture;
      DrawTree;
      SelectTreeTex;

      UpdatingFromCode := False;
    end;
  end
  else
    Exit;
  end;
end;

procedure TFormMain.mainMenuAddTextureClick(Sender: TObject);
begin
  AddNewTexture(Sender);
end;

procedure TFormMain.numBox_BlurAmountChange(Sender: TObject);
begin
  // PaletteReGen;
end;

procedure TFormMain.numBox_SimpThreshChange(Sender: TObject);
begin
  // PaletteReGen;
end;

procedure TFormMain.ScrollBox1Click(Sender: TObject);
begin
  if boolJamLoaded then
  begin
    UpdatingFromCode := True;
    DeSelectTexture();
    SelectTreeTex();
    UpdatingFromCode := False;
  end;
end;

procedure TFormMain.SelectTexture(jamID: integer);
var
  I: integer;
begin

  if jamID = -1 then
    Exit;

  boolTexSelected := True;

  intSelectedTexture := jamID;

  UpdatingFromCode := True;

  if boolHWJAM then
  begin

    RefreshCanvas;
    DrawTexture;

    tex_ID.Enabled := True;
    tex_X.Enabled := True;
    tex_Y.Enabled := True;
    tex_width.Enabled := True;
    tex_height.Enabled := True;

    tex_ID.Value := FHWJamFile.FEntries[jamID].FInfo.jamID;
    tex_X.Value := FHWJamFile.FEntries[jamID].FInfo.X;
    tex_Y.Value := FHWJamFile.FEntries[jamID].FInfo.Y;
    tex_width.Value := FHWJamFile.FEntries[jamID].FInfo.Width;
    tex_height.Value := FHWJamFile.FEntries[jamID].FInfo.height;

    tex_flags.Enabled := True;
    for I := 0 to 15 do
      tex_flags.Checked[I] :=
        UnPackFlag(FHWJamFile.FEntries[jamID].FInfo.jamFlags, I);

  end

  else
  begin
    RefreshPalette;
    DrawTexture;
    RefreshCanvas;

    tex_ID.Enabled := True;
    tex_X.Enabled := True;
    tex_Y.Enabled := True;
    tex_width.Enabled := True;
    tex_height.Enabled := True;

    btnPal0.Enabled := True;
    btnPal1.Enabled := True;
    btnPal2.Enabled := True;
    btnPal3.Enabled := True;

    btnGenPal.Enabled := True;
    btnRemovePal.Enabled := True;

    tex_ID.Value := FJamFile.FEntries[jamID].FInfo.jamID;
    tex_X.Value := FJamFile.FEntries[jamID].FInfo.X;
    tex_Y.Value := FJamFile.FEntries[jamID].FInfo.Y;
    tex_width.Value := FJamFile.FEntries[jamID].FInfo.Width;
    tex_height.Value := FJamFile.FEntries[jamID].FInfo.height;

    chkBoxTrans.Checked := DetectTransCol(FJamFile.FEntries[jamID].FTexture);

    tex_flags.Enabled := True;

    for I := 0 to 15 do
      tex_flags.Checked[I] :=
        UnPackFlag(FJamFile.FEntries[jamID].FInfo.jamFlags, I);

  end;

  toolBar_DeleteTexture.Enabled := True;
  toolBar_ImportTexture.Enabled := True;
  toolBar_ExportTexture.Enabled := True;

  mainMenuDeleteTexture.Enabled := boolTexSelected;
  mainMenuImportTexture.Enabled := boolTexSelected;
  mainMenuExportTexture.Enabled := boolTexSelected;
  mainMenuCut.Enabled := boolTexSelected;
  mainMenuCopy.Enabled := boolTexSelected;
  mainMenuModifyPalette.Enabled := boolTexSelected;
  mainMenuTextureProperties.Enabled := boolTexSelected;

  mainMenuPaste.Enabled := boolJamLoaded;

  mainMenuAddTexture.Enabled := boolJamLoaded;

  mainMenuImportCanvas.Enabled := boolJamLoaded;
  mainMenuExportCanvas.Enabled := boolJamLoaded;
  mainMenuSave.Enabled := boolJamLoaded;
  mainMenuSaveAs.Enabled := boolJamLoaded;
  mainMenuClose.Enabled := boolJamLoaded;

  if SelectFromCanvas = True then
    SelectTreeTex;

  UpdatingFromCode := False;

end;

procedure TFormMain.SelectTreeTex();
var
  N: TTreeNode;
  JNode: TJamTreeNode;
begin
  if (intSelectedTexture < 0) or (JamTree.Items.Count = 0) then
    Exit;

  JamTree.Items.BeginUpdate;
  try
    JamTree.Selected := nil;

    // Collapse all texture nodes first (optional)
    var
    Root := JamTree.Items[0];
    N := Root.getFirstChild;
    while Assigned(N) do
    begin
      N.Collapse(False);
      N := N.getNextSibling;
    end;

    // Walk all tree nodes to find a matching jamID node
    N := JamTree.Items.GetFirstNode;
    while Assigned(N) do
    begin
      if Assigned(N.data) then
      begin
        JNode := TJamTreeNode(N.data);
        if (JNode.nodeType = jamID) and (JNode.jamID = intSelectedTexture) then
        begin
          JamTree.Selected := N;
          N.Expand(False);
          N.MakeVisible;
          Break;
        end;
      end;
      N := N.GetNext;
    end;

  finally
    JamTree.Items.EndUpdate;
  end;
end;

procedure TFormMain.popUpDeleteTextureClick(Sender: TObject);
var
  I: integer;
begin
  I := intSelectedTexture;

  DeSelectTexture();

  FJamFile.DeleteTexture(I);

  RefreshPalette;
  RefreshCanvas;
  DrawTree();
end;

procedure TFormMain.DeSelectTexture();
var
  I: integer;
begin

  intSelectedTexture := -1;
  RefreshCanvas;

  toolBar_DeleteTexture.Enabled := False;
  toolBar_ImportTexture.Enabled := False;
  toolBar_ExportTexture.Enabled := False;

  ImageEntry.Picture := nil;
  panel_PalPreview.Visible := False;

  UpdatingFromCode := True;

  tex_ID.Enabled := False;
  tex_X.Enabled := False;
  tex_Y.Enabled := False;
  tex_width.Enabled := False;
  tex_height.Enabled := False;

  btnPal0.Enabled := False;
  btnPal1.Enabled := False;
  btnPal2.Enabled := False;
  btnPal3.Enabled := False;

  btnGenPal.Enabled := False;
  btnRemovePal.Enabled := False;

  tex_ID.Value := 0;
  tex_X.Value := 0;
  tex_Y.Value := 0;
  tex_width.Value := 0;
  tex_height.Value := 0;

  for I := 0 to 15 do
    tex_flags.Checked[I] := False;

  tex_flags.Enabled := False;

  UpdatingFromCode := False;
  boolTexSelected := False;

  mainMenuDeleteTexture.Enabled := boolTexSelected;
  mainMenuImportTexture.Enabled := boolTexSelected;
  mainMenuExportTexture.Enabled := boolTexSelected;
  mainMenuCut.Enabled := boolTexSelected;
  mainMenuCopy.Enabled := boolTexSelected;
  mainMenuModifyPalette.Enabled := boolTexSelected;
  mainMenuTextureProperties.Enabled := boolTexSelected;

  mainMenuPaste.Enabled := boolJamLoaded;

  mainMenuAddTexture.Enabled := boolJamLoaded;

  mainMenuImportCanvas.Enabled := boolJamLoaded;
  mainMenuExportCanvas.Enabled := boolJamLoaded;
  mainMenuSave.Enabled := boolJamLoaded;
  mainMenuSaveAs.Enabled := boolJamLoaded;
  mainMenuClose.Enabled := boolJamLoaded;

end;

procedure TFormMain.UpdateJamData(jamID: integer);
var
  I: integer;
begin
  if (jamID < 0) or (boolHWJAM and (jamID >= FHWJamFile.Entries.Count)) or
    (not boolHWJAM and (jamID >= FJamFile.Entries.Count)) then
    Exit;

  if boolHWJAM then
  begin
    with FHWJamFile.FEntries[jamID] do
    begin
      timer_JamRedrawPals.Interval := 90;
      tempDimensions.X := FInfo.X;
      tempDimensions.Y := FInfo.Y;
      tempDimensions.Width := FInfo.Width;
      tempDimensions.height := FInfo.height;

      FInfo.jamID := tex_ID.Value;
      FInfo.X := tex_X.Value;
      FInfo.Y := tex_Y.Value;
      FInfo.Width := tex_width.Value;
      FInfo.height := tex_height.Value;

      FInfo.jamFlags := 0;
      for I := 0 to 15 do
        if tex_flags.Checked[I] then
          FInfo.jamFlags := PackFlag(FInfo.jamFlags, I);
    end;
  end
  else
  begin
    with FJamFile.FEntries[jamID] do
    begin
      timer_JamRedrawPals.Interval := 250;
      tempDimensions.X := FInfo.X;
      tempDimensions.Y := FInfo.Y;

      FInfo.jamID := tex_ID.Value;
      FInfo.X := tex_X.Value;
      FInfo.Y := tex_Y.Value;

      FJamFile.UpdateTextureSize(jamID, tex_height.Value, tex_width.Value);

      FInfo.jamFlags := 0;
      for I := 0 to 15 do
        if tex_flags.Checked[I] then
          FInfo.jamFlags := PackFlag(FInfo.jamFlags, I);
    end;
  end;

  timer_JamRedrawPals.Enabled := False;
  timer_redrawTree.Enabled := False;

  timer_redrawTree.Enabled := True;

  timer_JamRedrawPals.Enabled := True;

  RefreshCanvas;
end;

procedure TFormMain.ClearPaletteImg();
begin
  panel_PalPreview.Visible := False;
end;

end.
