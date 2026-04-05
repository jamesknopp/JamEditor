unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Math, Registry, IOUtils,
  clipbrd,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.NumberBox, Vcl.ExtDlgs,
  Vcl.Menus, Vcl.ImgList, Vcl.ImageCollection,
  Vcl.VirtualImageList, System.Generics.Collections,

  JamGeneral, JamSW, JamHW, JamPalette, JamAnalysis, GeneralHelpers,
  newJamDlg,
  Vcl.Samples.Spin, Vcl.CheckLst, Vcl.ToolWin, Vcl.BaseImageCollection,
  System.ImageList, jampalettedetector, jambatch, options;

type

  TJamTreeNodeID = (jamDimensions, jamPosX, jamposY, jamFlags, jamColour,
    jamUnk, jamID, canvasDimensions, rootNode, jamwidth, jamheight, jamData,
    jamPalInfo);

  TJamTreeNode = class
    data: integer;
    jamID: integer;
    editNode: boolean;
    nodeType: TJamTreeNodeID;
  end;

type
  TResizeMode = (rmNone, rmMove, rmTopLeft, rmTop, rmTopRight, rmRight,
    rmBottomRight, rmBottom, rmBottomLeft, rmLeft);

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
    toolBar_GP3PAL: TToolButton;
    ToolButton11: TToolButton;
    toolBar_PalPrev: TToolButton;
    toolBar_Pal1: TToolButton;
    toolBar_Pal2: TToolButton;
    toolBar_Pal3: TToolButton;
    toolBar_Pal4: TToolButton;
    toolBar_PalNext: TToolButton;
    toolBarImageList: TVirtualImageList;
    ImageCollection1: TImageCollection;
    leftPanel: TPanel;
    timer_JamRedrawPals: TTimer;
    timer_redrawTree: TTimer;
    N9: TMenuItem;
    popUpAddTexture: TMenuItem;
    N10: TMenuItem;
    popUpDeleteTexture: TMenuItem;
    N11: TMenuItem;
    mainMenuDeleteTexture: TMenuItem;
    mnuRecentFiles: TMenuItem;
    ools1: TMenuItem;
    BatchConvert1: TMenuItem;
    View1: TMenuItem;
    View2: TMenuItem;
    N12: TMenuItem;
    ResetZoom1: TMenuItem;
    N13: TMenuItem;
    menuDrawOutlines: TMenuItem;
    treeimagecollection: TImageCollection;
    treeimagelist: TVirtualImageList;
    toolbar_drawOutlines: TToolButton;
    ToolButton5: TToolButton;
    toolbarZoomIN: TToolButton;
    ToolButton6: TToolButton;
    toolbarZoomOUT: TToolButton;
    toolbarZoomReset: TToolButton;
    ToolButton3: TToolButton;
    ToolButton8: TToolButton;
    N14: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N15: TMenuItem;
    PreviewMask1: TMenuItem;
    N16: TMenuItem;
    ConverttoGP2JAM: TMenuItem;
    ConverttoGP3SWJAM: TMenuItem;
    ConverttoGP3HWJAM: TMenuItem;
    panel_TexProperties_Generic: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    tex_height: TSpinEdit;
    tex_ID: TSpinEdit;
    tex_width: TSpinEdit;
    tex_X: TSpinEdit;
    tex_Y: TSpinEdit;
    panel_flags: TPanel;
    tex_flags: TCheckListBox;
    panel_TexPreview: TPanel;
    ImageEntry: TImage;
    panel_PalPreview: TPanel;
    PaintBoxPalette: TPaintBox;
    panel_PaletteEdit: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    comboSimpMethod: TComboBox;
    chkBoxSimpPal: TCheckBox;
    chkBoxTrans: TCheckBox;
    numBox_BlurAmount: TNumberBox;
    numBox_SimpThresh: TNumberBox;
    btnRemovePal: TButton;
    btnPal0: TButton;
    btnPal1: TButton;
    btnPal2: TButton;
    btnPal3: TButton;
    btnRemoveAllPals: TButton;
    btnGenPal: TButton;
    btnRegenAllPals: TButton;
    panel_RCR: TPanel;
    Label11: TLabel;
    rcrOdd: TButton;
    rcrEven: TButton;
    rcrReset: TButton;
    panel_TexScaling: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    scaleFlags: TCheckListBox;
    texScaleX: TSpinEdit;
    texScaleY: TSpinEdit;
    texScale: TSpinEdit;
    jam_Canvas: TPanel;
    Label4: TLabel;
    canvasHeight: TSpinEdit;
    Button5: TButton;
    CategoryPanelGroup1: TCategoryPanelGroup;
    panel_TexturePreview: TCategoryPanel;
    Panel_TexProperties: TCategoryPanel;
    panel_TexFlags: TCategoryPanel;
    panel_ScaleParameters: TCategoryPanel;
    panel_PalEdit: TCategoryPanel;
    panel_JAMCanvas: TCategoryPanel;
    panel_RCRControls: TCategoryPanel;
    N17: TMenuItem;
    JamAnalysis1: TMenuItem;
    SaveDecryptedJAM: TMenuItem;
    N18: TMenuItem;
    autoPackTexs: TMenuItem;
    undoTimer: TTimer;
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

    procedure btnSaveJamClick(Sender: TObject);
    procedure ImportTexture(Sender: TObject);
    procedure btnExportTextureClick(Sender: TObject);
    procedure toolBar_PalPrevClick(Sender: TObject);
    procedure toolBar_PalNextClick(Sender: TObject);
    procedure numBox_SimpThreshChange(Sender: TObject);
    procedure numBox_BlurAmountChange(Sender: TObject);
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
    procedure timer_redrawTreeTimer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure popUpDeleteTextureClick(Sender: TObject);

    procedure mainMenuExportCanvasClick(Sender: TObject);
    procedure canvasPopupMenuPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BatchConvert1Click(Sender: TObject);
    procedure View2Click(Sender: TObject);
    procedure ResetZoom1Click(Sender: TObject);
    procedure btnRemoveAllPalsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure btnRegenAllPalsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure toolbar_drawOutlinesClick(Sender: TObject);
    procedure toolBar_GP3PALMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure toolBar_GP2PALMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure toolbarZoomINClick(Sender: TObject);
    procedure toolbarZoomResetClick(Sender: TObject);
    procedure toolbarZoomOUTClick(Sender: TObject);
    procedure menuDrawOutlinesClick(Sender: TObject);
    procedure ShowHintInStatusBar(Sender: TObject);
    // procedure ImageCanvasMouseMove(Sender: TObject; Shift: TShiftState; X,
    // Y: Integer);
    procedure ImageCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure mainMenuSaveAsClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tex_XKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tex_XKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mainMenuCopyClick(Sender: TObject);
    procedure mainMenuPasteClick(Sender: TObject);
    procedure MainMenu1Change(Sender: TObject; Source: TMenuItem;
      Rebuild: boolean);
    procedure JamTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure tex_IDKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tex_YKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tex_widthKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tex_heightKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tex_IDKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tex_YKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tex_widthKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tex_heightKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mainMenuCutClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure mainMenuDeleteTextureClick(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure PreviewMask1Click(Sender: TObject);
    procedure ConverttoGP2JAMClick(Sender: TObject);
    procedure ConverttoGP3SWJAMClick(Sender: TObject);
    procedure ConverttoGP3HWJAMClick(Sender: TObject);
    procedure scaleFlagsClickCheck(Sender: TObject);
    procedure texScaleXChange(Sender: TObject);
    procedure texScaleYChange(Sender: TObject);
    procedure texScaleChange(Sender: TObject);
    procedure texScaleKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure texScaleXKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure texScaleYKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure texScaleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure texScaleXKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure texScaleYKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Panel_TexPropertiesCollapse(Sender: TObject);
    procedure Panel_TexPropertiesExpand(Sender: TObject);
    procedure panel_TexFlagsCollapse(Sender: TObject);
    procedure panel_JAMCanvasCollapse(Sender: TObject);
    procedure panel_JAMCanvasExpand(Sender: TObject);
    procedure panel_PalEditCollapse(Sender: TObject);
    procedure panel_PalEditExpand(Sender: TObject);
    procedure panel_RCRControlsCollapse(Sender: TObject);
    procedure panel_RCRControlsExpand(Sender: TObject);
    procedure panel_ScaleParametersCollapse(Sender: TObject);
    procedure panel_ScaleParametersExpand(Sender: TObject);
    procedure panel_TexFlagsExpand(Sender: TObject);
    procedure panel_TexturePreviewExpand(Sender: TObject);
    procedure panel_TexturePreviewCollapse(Sender: TObject);
    procedure JamAnalysis1Click(Sender: TObject);
    procedure SaveDecryptedJAMClick(Sender: TObject);
    procedure mainMenuImportCanvasClick(Sender: TObject);
    procedure autoPackTexsClick(Sender: TObject);
    procedure undoTimerTimer(Sender: TObject);
    procedure tex_XMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tex_XMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  public
    FJamFile: TJamFile;
    FHWJamFile: THWJamFile;
    UpdatingFromCode: boolean;
    SelectFromCanvas: boolean;

    MRUList: TStringList;

    procedure CopyJAM(Entries: TList<TjamEntry>);
    function PasteJam: TList<TjamEntry>;

    procedure CopyHWJAM(Entries: TList<THWjamEntry>);
    function PasteHWJam: TList<THWjamEntry>;

    procedure Copy();

    procedure DoUndo();
    procedure PushUndoState();

    procedure RefreshCanvas;
    procedure RefreshPalette();
    procedure DrawTexture();
    procedure PalChange(palID: integer);
    procedure JamReGen;
    procedure TreeReDraw;
    procedure DrawTree;
    procedure SelectTexture(id: integer; treeupdate: boolean);
    procedure DeSelectTexture();
    procedure NewJam(filename: string; hwJAM: boolean; height: integer);
    procedure LoadJam(filename: string);
    procedure UpdateJamData(id: integer);
    procedure SelectTreeTex();
    procedure ClearPaletteImg();
    procedure LoadMRU;
    procedure SaveMRU;
    procedure UpdateMRU;
    procedure AddToMRU(const filename: string);
    procedure RecentFileClick(Sender: TObject);
    procedure CheckJamModified();
    procedure UISetup();
    procedure UpdateCaption();
    procedure UpdateUIData(id: integer);
    procedure ConvertJAM(output: TJamType);
    procedure DeleteTexture();
    procedure JamModified(modified: boolean);
    function JamSanityCheck(): boolean;
    function HitTestResizeZone(X, Y, EntryX, EntryY, EntryW, EntryH: integer)
      : TResizeMode;

    procedure genMask();

    procedure JamSanityCheckInform(onSave: boolean);

    function AddJamTreeNode(Tree: TTreeView; Parent: TTreeNode;
      const Caption: string; data: integer; jamID: integer; editNode: boolean;
      nodeType: TJamTreeNodeID): TTreeNode;
  private
    procedure MsgHandler(var Msg: TMsg; var Handled: boolean);
  public

  end;

var

  // change to TQueue
  SWUndoStack, SWRedoStack: TStack<TJamFile>;
  HWUndoStack, HWRedoStack: TStack<THWJamFile>;

  FormMain: TFormMain;

  boolLCtrl: boolean;

  SelectedTreeNodes: TList<TTreeNode>;

  CurrentResizeMode: TResizeMode = rmNone;
  LastMousePos: TPoint;
  userisTyping: boolean = false;

  boolFlagChange: boolean;
  boolXChange: boolean;
  boolYChange: boolean;
  boolWidthChange: boolean;
  boolHeightChange: boolean;
  boolIDChange: boolean;
  boolScaleXChange: boolean;
  boolScaleYChange: boolean;
  boolScaleChange: boolean;
  boolScaleFlagsChange: boolean;

implementation

{$R *.dfm}

uses JamBrowser;

procedure ForceCategoryPanelRedraw(CPG: TCategoryPanelGroup);
var
  i: integer;

  panel: TCategoryPanel;
begin
  CPG.DisableAlign;
  try
    CPG.Realign;
    CPG.Invalidate;
    CPG.Update;
  finally
    CPG.EnableAlign;
  end;

  begin
    for i := 0 to CPG.ControlCount - 1 do
    begin
      if CPG.Controls[i] is TCategoryPanel then
      begin
        panel := TCategoryPanel(CPG.Controls[i]);
        RedrawWindow(panel.Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or
          RDW_FRAME or RDW_ALLCHILDREN);
      end;
    end;

    RedrawWindow(CPG.Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME or
      RDW_ALLCHILDREN);
  end;
end;

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
    i: integer;
    Current: TTreeNode;
  begin
    Result := nil;
    if Path = '' then
      Exit;

    Parts := Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);
    Current := nil;

    for i := 0 to High(Parts) do
    begin
      Result := nil;
      if Current = nil then
        Current := Tree.Items.GetFirstNode
      else
        Current := Current.getFirstChild;

      while Current <> nil do
      begin
        if SameText(Current.Text, Parts[i]) then
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
  i: integer;
  NodePath: string;
begin
  for i := 0 to ExpandedPaths.Count - 1 do
  begin
    NodePath := ExpandedPaths[i];
    Node := FindNodeByPath(NodePath);
    if Assigned(Node) then
      Node.Expand(false);
  end;

  Node := FindNodeByPath(SelectedPath);
  if Assigned(Node) then
    Tree.Selected := Node;
end;

function TFormMain.HitTestResizeZone(X, Y, EntryX, EntryY, EntryW,
  EntryH: integer): TResizeMode;
const
  HandleSize = 6;
var
  RightX, BottomY: integer;
begin
  Result := rmNone;

  RightX := EntryX + EntryW;
  BottomY := EntryY + EntryH;

  // Corners
  if (Abs(X - EntryX) <= HandleSize) and (Abs(Y - EntryY) <= HandleSize) then
    Exit(rmTopLeft);
  if (Abs(X - RightX) <= HandleSize) and (Abs(Y - EntryY) <= HandleSize) then
    Exit(rmTopRight);
  if (Abs(X - RightX) <= HandleSize) and (Abs(Y - BottomY) <= HandleSize) then
    Exit(rmBottomRight);
  if (Abs(X - EntryX) <= HandleSize) and (Abs(Y - BottomY) <= HandleSize) then
    Exit(rmBottomLeft);

  // Edges
  if (X > EntryX + HandleSize) and (X < RightX - HandleSize) then
  begin
    if Abs(Y - EntryY) <= HandleSize then
      Exit(rmTop);
    if Abs(Y - BottomY) <= HandleSize then
      Exit(rmBottom);
  end;

  if (Y > EntryY + HandleSize) and (Y < BottomY - HandleSize) then
  begin
    if Abs(X - EntryX) <= HandleSize then
      Exit(rmLeft);
    if Abs(X - RightX) <= HandleSize then
      Exit(rmRight);
  end;

  // Inside texture area (not edge/corner): move
  if (X >= EntryX) and (X <= RightX) and (Y >= EntryY) and (Y <= BottomY) then
    Exit(rmMove);
end;

procedure TFormMain.LoadMRU();
var
  Reg: TRegistry;
  i: integer;
  s: string;
begin
  MRUList.Clear;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(MRUKeyPath) then
    begin
      for i := 0 to intMaxMRU - 1 do
      begin
        if Reg.ValueExists('File' + IntToStr(i)) then
        begin
          s := Reg.ReadString('File' + IntToStr(i));
          if FileExists(s) then
            MRUList.Add(s);
        end;
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormMain.MainMenu1Change(Sender: TObject; Source: TMenuItem;
  Rebuild: boolean);
begin

  if boolHWJAM then
  begin
    if not Clipboard.HasFormat(ClipboardHWJAM) then
      mainMenuPaste.Enabled := false;
  end
  else
  begin
    if not Clipboard.HasFormat(ClipboardJAM) then
      mainMenuPaste.Enabled := false;
  end;

end;

procedure TFormMain.SaveDecryptedJAMClick(Sender: TObject);
begin

  if JamSanityCheck then
  begin
    JamSanityCheckInform(true);
    Exit;
  end;

  dlgSaveJam.filename := FJamFile.JamFileName + '.jamdec';

  if dlgSaveJam.Execute then
  begin
    FJamFile.SaveToFile(dlgSaveJam.filename, true);

  end
  else
    // user cancelled the Save As dialog
    Exit;

end;

procedure TFormMain.SaveMRU;
var
  Reg: TRegistry;
  i: integer;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(MRUKeyPath, true) then
    begin
      // write up to MaxMRU entries
      for i := 0 to MRUList.Count - 1 do
        Reg.WriteString('File' + IntToStr(i), MRUList[i]);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFormMain.scaleFlagsClickCheck(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode then
    Exit;
  boolScaleFlagsChange := true;
  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.Copy();
var
  i: integer;
  jamList: TList<TjamEntry>;
  jamHWList: TList<THWjamEntry>;

begin
  if boolHWJAM then
  begin
    SelectedTextureList.sort;
    jamHWList := TList<THWjamEntry>.Create;

    for i := 0 to SelectedTextureList.Count - 1 do
    begin
      jamHWList.Add(FHWJamFile.Entries[SelectedTextureList[i]]);
    end;
    CopyHWJAM(jamHWList);

    jamHWList.Clear;

  end
  else
  begin
    SelectedTextureList.sort;
    jamList := TList<TjamEntry>.Create;

    for i := 0 to SelectedTextureList.Count - 1 do
    begin
      jamList.Add(FJamFile.Entries[SelectedTextureList[i]]);
    end;
    CopyJAM(jamList);

    jamList.Clear;
  end;
end;

procedure TFormMain.CopyHWJAM(Entries: TList<THWjamEntry>);
var
  Stream: TMemoryStream;
  Count, i: integer;
  Handle: THandle;
  P: Pointer;
begin
  Stream := TMemoryStream.Create;
  try
    Count := Entries.Count;
    Stream.WriteBuffer(Count, SizeOf(Count));

    for i := 0 to Count - 1 do
      Entries[i].SaveToStream(Stream);

    Stream.Position := 0;
    Handle := GlobalAlloc(GMEM_MOVEABLE, Stream.Size);
    if Handle = 0 then
      Exit;

    P := GlobalLock(Handle);
    try
      Stream.ReadBuffer(P^, Stream.Size);
    finally
      GlobalUnlock(Handle);
    end;

    Clipboard.SetAsHandle(ClipboardHWJAM, Handle);
  finally
    Stream.Free;
  end;
end;

procedure TFormMain.Panel_TexPropertiesCollapse(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.Panel_TexPropertiesExpand(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_TexturePreviewCollapse(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_TexturePreviewExpand(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

function TFormMain.PasteHWJam: TList<THWjamEntry>;
var
  Handle: THandle;
  P: Pointer;
  Stream: TMemoryStream;
  i, Count, X: integer;
  Entry: THWjamEntry;
begin
  Result := TList<THWjamEntry>.Create;

  if not Clipboard.HasFormat(ClipboardHWJAM) then
    Exit;

  Handle := Clipboard.GetAsHandle(ClipboardHWJAM);
  if Handle = 0 then
    Exit;

  P := GlobalLock(Handle);
  if P = nil then
    Exit;

  try
    Stream := TMemoryStream.Create;
    try
      Stream.WriteBuffer(P^, GlobalSize(Handle));
      Stream.Position := 0;

      Stream.ReadBuffer(Count, SizeOf(Count));
      for i := 0 to Count - 1 do
      begin
        Entry := THWjamEntry.Create(Default (THWJamEntryInfo));
        Entry.LoadFromStream(Stream);
        if Entry.FInfo.jamID = FHWJamFile.FEntries[X].FInfo.jamID then
          Entry.FInfo.jamID := FHWJamFile.GetNextJamID(FHWJamFile.FEntries) + i;

        Result.Add(Entry);
      end;
    finally
      Stream.Free;
    end;
  finally
    GlobalUnlock(Handle);
  end;
end;

procedure TFormMain.CopyJAM(Entries: TList<TjamEntry>);
var
  Stream: TMemoryStream;
  Count, i: integer;
  Handle: THandle;
  P: Pointer;
begin
  Stream := TMemoryStream.Create;
  try
    Count := Entries.Count;
    Stream.WriteBuffer(Count, SizeOf(Count));

    for i := 0 to Count - 1 do
      Entries[i].SaveToStream(Stream);

    Stream.Position := 0;
    Handle := GlobalAlloc(GMEM_MOVEABLE, Stream.Size);
    if Handle = 0 then
      Exit;

    P := GlobalLock(Handle);
    try
      Stream.ReadBuffer(P^, Stream.Size);
    finally
      GlobalUnlock(Handle);
    end;

    Clipboard.SetAsHandle(ClipboardJAM, Handle);
  finally
    Stream.Free;
  end;
end;

function TFormMain.PasteJam: TList<TjamEntry>;
var
  Handle: THandle;
  P: Pointer;
  Stream: TMemoryStream;
  i, Count, X: integer;
  Entry: TjamEntry;
begin
  Result := TList<TjamEntry>.Create;

  if not Clipboard.HasFormat(ClipboardJAM) then
    Exit;

  Handle := Clipboard.GetAsHandle(ClipboardJAM);
  if Handle = 0 then
    Exit;

  P := GlobalLock(Handle);
  if P = nil then
    Exit;

  try
    Stream := TMemoryStream.Create;
    try
      Stream.WriteBuffer(P^, GlobalSize(Handle));
      Stream.Position := 0;

      Stream.ReadBuffer(Count, SizeOf(Count));
      for i := 0 to Count - 1 do
      begin
        Entry := TjamEntry.Create(Default (TJamEntryInfo));
        Entry.LoadFromStream(Stream);
        for X := 0 to FJamFile.FEntries.Count - 1 do
          if Entry.FInfo.jamID = FJamFile.FEntries[X].FInfo.jamID then
            Entry.FInfo.jamID := FJamFile.GetNextJamID(FJamFile.FEntries) + i;
        Result.Add(Entry);
      end;
    finally
      Stream.Free;
    end;
  finally
    GlobalUnlock(Handle);
  end;
end;

procedure TFormMain.UpdateMRU();
var
  i: integer;
  itm: TMenuItem;
begin
  mnuRecentFiles.Clear;
  for i := 0 to MRUList.Count - 1 do
  begin
    itm := TMenuItem.Create(mnuRecentFiles);
    itm.Caption := Format('&%d %s', [i + 1, WinShortPath(MRUList[i], 90)]);
    itm.Hint := MRUList[i]; // store full path
    itm.OnClick := RecentFileClick;
    mnuRecentFiles.Add(itm);
  end;
  mnuRecentFiles.Enabled := MRUList.Count > 0;
end;

procedure TFormMain.View2Click(Sender: TObject);
begin
  options.optionsForm.show;
end;

procedure TFormMain.AddToMRU(const filename: string);
var
  idx: integer;
begin
  idx := MRUList.IndexOf(filename);
  if idx <> -1 then
    MRUList.Delete(idx);
  MRUList.Insert(0, filename);
  if MRUList.Count > intMaxMRU then
    MRUList.Delete(MRUList.Count - 1);
  SaveMRU;
  UpdateMRU;
end;

procedure TFormMain.autoPackTexsClick(Sender: TObject);
var
  JamRects: TArray<TJamRect>;
  i: integer;
begin

  if boolUndo then
  PushUndoState;

  if boolHWJAM then
  begin
    FHWJamFile.BuildRect_HW(FHWJamFile, JamRects);
    PackRects(JamRects, 256, FHWJamFile.canvasHeight);

    for i := 0 to high(JamRects) do
    begin
      begin
        FHWJamFile.FEntries[JamRects[i].index].FInfo.X := JamRects[i].X;

        FHWJamFile.FEntries[JamRects[i].index].FInfo.Y := JamRects[i].Y;
      end;

    end;

  end
  else
  begin
    FJamFile.BuildRect_SW(FJamFile, JamRects);
    PackRects(JamRects, 256, FJamFile.canvasHeight);

    for i := 0 to high(JamRects) do
    begin

      begin
        FJamFile.FEntries[JamRects[i].index].FInfo.X := JamRects[i].X;

        FJamFile.FEntries[JamRects[i].index].FInfo.Y := JamRects[i].Y;
      end;

    end;

  end;

  timer_redrawTree.Enabled := true;

  RefreshCanvas;
  JamModified(true);

end;

procedure TFormMain.RecentFileClick(Sender: TObject);
var
  filename: string;
begin
  filename := (Sender as TMenuItem).Hint;
  if FileExists(filename) then
  begin
    LoadJam(filename);
    AddToMRU(filename); // bump it back to the top
  end
  else
    ShowMessage('File not found: ' + filename);
end;

procedure TFormMain.Redo1Click(Sender: TObject);
begin

  if boolHWJAM then
  begin
    if HWRedoStack.Count = 0 then
      Exit;

    HWUndoStack.Push(FHWJamFile.Clone);

    FHWJamFile := HWRedoStack.pop;

    Undo1.Enabled := HWUndoStack.Count > 0;
    Redo1.Enabled := HWRedoStack.Count > 0;

  end
  else
  begin

    if SWRedoStack.Count = 0 then
      Exit;

    SWUndoStack.Push(FJamFile.Clone);

    FJamFile := SWRedoStack.pop;

    Undo1.Enabled := SWUndoStack.Count > 0;
    Redo1.Enabled := SWRedoStack.Count > 0;

  end;

  RefreshCanvas;
  TreeReDraw;
  JamModified(true);
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

  if nodeType = jamPosX then
    Result.ImageIndex := 11;

  if nodeType = jamposY then
    Result.ImageIndex := 12;

  if nodeType = jamheight then
    Result.ImageIndex := 1;

  if nodeType = canvasDimensions then
    Result.ImageIndex := 1;

  if nodeType = jamwidth then
    Result.ImageIndex := 8;

  if nodeType = jamFlags then
    Result.ImageIndex := 14;

  if nodeType = jamData then
    Result.ImageIndex := 9;

  if nodeType = jamPalInfo then
    Result.ImageIndex := 5;

  if nodeType = jamColour then
    Result.ImageIndex := 10;

  if nodeType = jamUnk then
    Result.ImageIndex := 7;

  Result.SelectedIndex := Result.ImageIndex;
  Result.data := jamNode;

end;

procedure TFormMain.DrawTree();
var
  JamRoot, ItemsRoot, Node, ScalingNode, UnTexNode, UnkNode, CanvasHeightNode
    : TTreeNode;
  i, j: integer;
  E: TjamEntry;
  hwEntry: THWjamEntry;
  Tree: TTreeView;
begin
  Tree := JamTree;
  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;

    if boolHWJAM then
    begin
      JamRoot := Tree.Items.Add(nil, FHWJamFile.JamFileName);
      JamRoot.ImageIndex := 2;
      JamRoot.SelectedIndex := 2;

      ItemsRoot := AddJamTreeNode(Tree, JamRoot,
        Format('JAM Items: %d', [FHWJamFile.FHeader.NumItems]),
        FHWJamFile.FHeader.NumItems, -1, false, rootNode);

      ItemsRoot.ImageIndex := 3;
      ItemsRoot.SelectedIndex := 3;

      for i := 0 to FHWJamFile.FEntries.Count - 1 do
      begin
        hwEntry := FHWJamFile.FEntries[i];

        Node := AddJamTreeNode(Tree, ItemsRoot, Format('ID:%d  [%d x %d]',
          [hwEntry.Info.jamID, hwEntry.Info.Width, hwEntry.Info.height]), 0, i,
          false, jamID);

        Node.ImageIndex := 4;
        Node.SelectedIndex := 4;

        AddJamTreeNode(Tree, Node, Format('X:%d', [hwEntry.Info.X]),
          hwEntry.Info.X, i, true, jamPosX);
        AddJamTreeNode(Tree, Node, Format('Y:%d', [hwEntry.Info.Y]),
          hwEntry.Info.Y, i, true, jamposY);
        AddJamTreeNode(Tree, Node, Format('Width:%d', [hwEntry.Info.Width]),
          hwEntry.Info.Width, i, false, jamwidth);
        AddJamTreeNode(Tree, Node, Format('Height:%d', [hwEntry.Info.height]),
          hwEntry.Info.height, i, false, jamheight);
        AddJamTreeNode(Tree, Node, Format('JamFlags:%d', [hwEntry.Info.jamFlags]
          ), hwEntry.Info.jamFlags, i, false, jamFlags);

        ScalingNode := AddJamTreeNode(Tree, Node, 'Scaling Info', 0, i,
          false, jamID);
        ScalingNode.ImageIndex := 6;
        ScalingNode.SelectedIndex := 6;

        AddJamTreeNode(Tree, ScalingNode, Format('Origin X: %d',
          [hwEntry.Info.scaleX]), hwEntry.Info.scaleX, i, false, jamPosX);

        AddJamTreeNode(Tree, ScalingNode, Format('Origin Y: %d',
          [hwEntry.Info.scaleY]), hwEntry.Info.scaleY, i, false, jamposY);

        AddJamTreeNode(Tree, ScalingNode, Format('Scaling Factor: %d',
          [hwEntry.Info.scaleFactor]), (hwEntry.Info.scaleFactor), i, false,
          canvasDimensions);

        AddJamTreeNode(Tree, ScalingNode,
          Format('Flags: %d', [(hwEntry.Info.scaleFlags)]),
          hwEntry.Info.scaleFlags, i, false, jamFlags);

        // AddJamTreeNode(Tree, Node, Format('Word6:%d', [hwEntry.Info.word6]),
        // hwEntry.Info.word6, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word6a:%d', [hwEntry.Info.word6a]),
        // hwEntry.Info.word6a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word6b:%d', [hwEntry.Info.word6b]),
        // hwEntry.Info.word6b, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word7a:%d', [hwEntry.Info.word7a]),
        // hwEntry.Info.word7a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word7b:%d', [hwEntry.Info.word7b]),
        // hwEntry.Info.word7b, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word8a:%d', [hwEntry.Info.word8a]),
        // hwEntry.Info.word8a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word8b:%d', [hwEntry.Info.word8b]),
        // hwEntry.Info.word8b, i, True, jamPosX);
        //
        // AddJamTreeNode(Tree, Node, Format('Word11:%d', [hwEntry.Info.word11]),
        // hwEntry.Info.word11, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word11a:%d', [hwEntry.Info.word11a]),
        // hwEntry.Info.word11a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word11b:%d', [hwEntry.Info.word11b]),
        // hwEntry.Info.word11b, i, True, jamPosX);
        //
        // AddJamTreeNode(Tree, Node, Format('Word12:%d', [hwEntry.Info.word12]),
        // hwEntry.Info.word12, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word12a:%d', [hwEntry.Info.word12a]),
        // hwEntry.Info.word12a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word12b:%d', [hwEntry.Info.word12b]),
        // hwEntry.Info.word12b, i, True, jamPosX);
        //
        // AddJamTreeNode(Tree, Node, Format('Word13:%d', [hwEntry.Info.word13]),
        // hwEntry.Info.word13, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word13a:%d', [hwEntry.Info.word13a]),
        // hwEntry.Info.word13a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word13b:%d', [hwEntry.Info.word13b]),
        // hwEntry.Info.word13b, i, True, jamPosX);
        //
        // AddJamTreeNode(Tree, Node, Format('Word14:%d', [hwEntry.Info.word14]),
        // hwEntry.Info.word14, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word14a:%d', [hwEntry.Info.word14a]),
        // hwEntry.Info.word14a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word14b:%d', [hwEntry.Info.word14b]),
        // hwEntry.Info.word14b, i, True, jamPosX);
        //
        // AddJamTreeNode(Tree, Node, Format('Word15:%d', [hwEntry.Info.word15]),
        // hwEntry.Info.word15, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word15a:%d', [hwEntry.Info.word15a]),
        // hwEntry.Info.word15a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word15b:%d', [hwEntry.Info.word15b]),
        // hwEntry.Info.word15b, i, True, jamPosX);
        //
        // AddJamTreeNode(Tree, Node, Format('Word16:%d', [hwEntry.Info.word16]),
        // hwEntry.Info.word16, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word16a:%d', [hwEntry.Info.word16a]),
        // hwEntry.Info.word16a, i, True, jamPosX);
        // AddJamTreeNode(Tree, Node, Format('Word16b:%d', [hwEntry.Info.word16b]),
        // hwEntry.Info.word16b, i, True, jamPosX);
        //
        // AddJamTreeNode(Tree, Node, Format('numframes:%d',
        // [hwEntry.Info.NumFrames]), hwEntry.Info.NumFrames, i, True, jamPosX);
      end;

      AddJamTreeNode(Tree, JamRoot, Format('Canvas Size: W: %d x H: %d',
        [FHWJamFile.canvasWidth, FHWJamFile.FHeader.JamTotalHeight]),
        FHWJamFile.FHeader.JamTotalHeight, -1, true, canvasDimensions);

    end
    else
    begin
      with FJamFile do
      begin
        JamRoot := Tree.Items.Add(nil, JamFileName);
        JamRoot.ImageIndex := 2;
        JamRoot.SelectedIndex := 2;

        ItemsRoot := AddJamTreeNode(Tree, JamRoot,
          Format('JAM Items: %d', [FHeader.NumItems]), FHeader.NumItems, -1,
          false, rootNode);

        ItemsRoot.ImageIndex := 3;
        ItemsRoot.SelectedIndex := 3;

        for i := 0 to FEntries.Count - 1 do
        begin
          E := FEntries[i];

          Node := AddJamTreeNode(Tree, ItemsRoot, Format('ID:%d  [%d x %d]',
            [E.Info.jamID, E.Info.Width, E.Info.height]), 0, i, false, jamID);

          Node.ImageIndex := 4;
          Node.SelectedIndex := 4;

          AddJamTreeNode(Tree, Node, Format('X:%d', [E.Info.X]), E.Info.X, i,
            true, jamPosX);
          AddJamTreeNode(Tree, Node, Format('Y:%d', [E.Info.Y]), E.Info.Y, i,
            true, jamposY);
          AddJamTreeNode(Tree, Node, Format('Width:%d', [E.Info.Width]),
            E.Info.Width, i, false, jamwidth);
          AddJamTreeNode(Tree, Node, Format('Height:%d', [E.Info.height]),
            E.Info.height, i, false, jamheight);
          AddJamTreeNode(Tree, Node, Format('ImagePTR:%d', [E.Info.ImagePtr]),
            E.Info.jamFlags, i, false, jamData);
          AddJamTreeNode(Tree, Node, Format('JamFlags:%d', [E.Info.jamFlags]),
            E.Info.jamFlags, i, false, jamFlags);
          AddJamTreeNode(Tree, Node, Format('PaletteSize:%d',
            [E.Info.PaletteSizeDiv4]), E.Info.PaletteSizeDiv4, i, false,
            jamPalInfo);

          ScalingNode := AddJamTreeNode(Tree, Node, 'Scaling Info', 0, i,
            false, jamID);
          ScalingNode.ImageIndex := 6;
          ScalingNode.SelectedIndex := 6;

          AddJamTreeNode(Tree, ScalingNode, Format('Origin X: %d',
            [E.Info.scaleX]), E.Info.scaleX, i, false, jamPosX);

          AddJamTreeNode(Tree, ScalingNode, Format('Origin Y: %d',
            [E.Info.scaleY]), E.Info.scaleY, i, false, jamposY);

          AddJamTreeNode(Tree, ScalingNode, Format('Scaling Factor: %d',
            [E.Info.scaleFactor]), (E.Info.scaleFactor), i, false,
            canvasDimensions);

          AddJamTreeNode(Tree, ScalingNode,
            Format('Flags: %d', [(E.Info.scaleFlag)]), E.Info.scaleFlag, i,
            false, jamFlags);


          // AddJamTreeNode(Tree, ScalingNode, Format('Origin X: %d',[FJamFile.GetIDX08_X(E.Info.Idx08)]),
          // FJamFile.GetIDX08_X(E.Info.Idx08), I, false, jamPosX);
          //
          // AddJamTreeNode(Tree, ScalingNode, Format('Origin Y: %d',
          // [FJamFile.GetIDX08_Y(E.Info.Idx08)]),
          // FJamFile.GetIDX08_Y(E.Info.Idx08), I, false, jamposY);
          //
          // AddJamTreeNode(Tree, ScalingNode, Format('Scaling Factor: %d',
          // [FJamFile.GetIDX0aScale(E.Info.Idx0A)]),
          // FJamFile.GetIDX08_Y(E.Info.Idx08), I, false, canvasDimensions);
          //
          // AddJamTreeNode(Tree, ScalingNode,
          // Format('Flags: %d', [FJamFile.GetIDX0aFlags(E.Info.Idx0A)]),
          // FJamFile.GetIDX08_Y(E.Info.Idx08), I, false, jamFlags);

          UnTexNode := AddJamTreeNode(Tree, Node, 'Untextured Colour', 0, i,
            false, jamPalInfo);
          AddJamTreeNode(Tree, UnTexNode, Format('Primary: %d', [E.Info.Idx16]),
            E.Info.Idx16, i, true, jamColour);
          AddJamTreeNode(Tree, UnTexNode, Format('Secondary: %d', [E.Info.Idx17]
            ), E.Info.Idx17, i, true, jamColour);

          UnkNode := AddJamTreeNode(Tree, Node, 'Unknown Data', 0, i,
            false, jamUnk);
          UnkNode.ImageIndex := 7;
          UnkNode.SelectedIndex := 7;
          AddJamTreeNode(Tree, UnkNode, Format('Idx03:%d', [E.Info.Unk]),
            E.Info.Unk, i, false, jamUnk);
          AddJamTreeNode(Tree, UnkNode, Format('Idx0E:%d', [E.Info.Idx0E]),
            E.Info.Idx0E, i, false, jamUnk);
          for j := 0 to 7 do
            AddJamTreeNode(Tree, UnkNode, Format('Idx18_0%d:%d',
              [j, E.Info.Idx18[j]]), E.Info.Idx18[j], i, false, jamUnk);
        end;
        AddJamTreeNode(Tree, JamRoot, Format('Canvas Size: W: %d x H: %d',
          [canvasWidth, FHeader.JamTotalHeight]), FHeader.JamTotalHeight, -1,
          true, canvasDimensions);
      end;
    end;

    Tree.TopItem.Expanded := true;
  finally
    Tree.Items.EndUpdate;

  end;
end;

procedure TFormMain.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure TFormMain.mainMenuExportCanvasClick(Sender: TObject);
var
  tempBMP: TBitmap;
begin
  if boolJamLoaded = true then
  begin

    if exportDialog.Execute then

      if boolHWJAM then
      begin
        tempBMP := FHWJamFile.DrawCanvas(false);
        tempBMP.SaveToFile(exportDialog.filename)

      end
      else
      begin
        tempBMP := FJamFile.DrawJamCanvas(false);
        tempBMP.PixelFormat := pf8bit;
        tempBMP.Palette := creategpxpal;
        tempBMP.SaveToFile(exportDialog.filename);
      end;

  end;

end;

procedure TFormMain.ToolButton1Click(Sender: TObject);
begin
  CheckJamModified();

  if boolJamLoaded then
    DeSelectTexture();

  boolTexSelected := false;

  newJamDlg.newJamDialog.show;

end;

procedure TFormMain.toolbarZoomINClick(Sender: TObject);
begin
  intJamZoom := intJamZoom + 1;
  RefreshCanvas;
end;

procedure TFormMain.toolbarZoomOUTClick(Sender: TObject);
begin
  if intJamZoom <= 0.4 then
    Exit;

  if intJamZoom <= 1 then
    intJamZoom := intJamZoom - 0.2
  else

    intJamZoom := intJamZoom - 1;
  RefreshCanvas;
end;

procedure TFormMain.toolbarZoomResetClick(Sender: TObject);
begin
  intJamZoom := 1;
  RefreshCanvas;
end;

procedure TFormMain.toolbar_drawOutlinesClick(Sender: TObject);
begin
  btnDrawDataClick(Sender);
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

procedure TFormMain.JamAnalysis1Click(Sender: TObject);
begin
  frmJamAnalysis.show;
end;

procedure TFormMain.JamReGen;
var
  tmpCanvas: TBitmap;
  bmpPal: TBitmap;
  i: integer;

begin
  if not boolJamLoaded then
    Exit;

  i := intSelectedTexture;
  if boolHWJAM then
  begin
    tmpCanvas := FHWJamFile.FEntries[i].FOriginalTex;
    tmpCanvas := StretchF(tmpCanvas, FHWJamFile.FEntries[i].Info.Width,
      FHWJamFile.FEntries[i].Info.height);

    FHWJamFile.FEntries[i].FTexture.height := FHWJamFile.FEntries[i]
      .Info.height;
    FHWJamFile.FEntries[i].FTexture.Width := FHWJamFile.FEntries[i].Info.Width;
    FHWJamFile.FEntries[i].FTexture := tmpCanvas;

    DrawTexture;
  end

  else
  begin

    intSimplifyThreshold := Round(numBox_SimpThresh.Value);
    intSimplifyMethod := comboSimpMethod.ItemIndex;
    intBlurThreshold := Round(numBox_BlurAmount.Value);
    boolSimpifyAllPals := chkBoxSimpPal.Checked;
    boolProtectTrans := chkBoxTrans.Checked;

    tmpCanvas := TBitmap.Create;
    try
      tmpCanvas.Assign(FJamFile.FEntries[i].FOriginalTex);

      tmpCanvas := StretchF(tmpCanvas, FJamFile.FEntries[i].FInfo.Width,
        FJamFile.FEntries[i].FInfo.height);

      if generatePal then
      begin
        if Assigned(FJamFile.FEntries[i].FTexture) then
        begin
          freeandnil(FJamFile.FEntries[i].FTexture);
          FJamFile.FEntries[i].FTexture := TBitmap.Create;
        end;

        FJamFile.FEntries[i].FTexture := FJamFile.GenerateGPxBMP(tmpCanvas, i,
          intSimplifyMethod, intSimplifyThreshold, intBlurThreshold,
          boolSimpifyAllPals, boolProtectTrans);
        generatePal := false;

      end
      else
      begin
        if FJamFile.FEntries[i].PaletteSizeDiv4 > 0 then

        else
        begin
          bmpPal := TBitmap.Create;
          bmpPal.Assign(tmpCanvas);
          bmpPal.PixelFormat := pf8bit;
          bmpPal := CreateGPxPalBMP(tmpCanvas);
          bmpPal.Palette := creategpxpal;
          FJamFile.EncodeTexture(i, bmpPal);
          bmpPal.Free;
        end;
      end;

      if FJamFile.FEntries[i].PaletteSizeDiv4 > 0 then
        FJamFile.CachePaletteBMP(i)
      else
        FJamFile.CachePaletteBMP(i);
      RefreshPalette;
      RefreshCanvas;
      DrawTexture;

      chkBoxTrans.Checked := DetectTransCol(FJamFile.FEntries[i].FTexture);

    finally
      tmpCanvas.Free;
      generatePal := false;
    end;
  end;
end;

procedure TFormMain.menuDrawOutlinesClick(Sender: TObject);
begin
  btnDrawDataClick(Sender);
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

  if boolJamLoaded = true then
  begin

    btnArray[0] := toolBar_Pal1;
    btnArray[1] := toolBar_Pal2;
    btnArray[2] := toolBar_Pal3;
    btnArray[3] := toolBar_Pal4;

    intPaletteID := palID;

    for X := 0 to 3 do
      if X = intPaletteID then
        btnArray[X].down := true;

    RefreshPalette();
    RefreshCanvas;
    if intSelectedTexture > -1 then
      DrawTexture
  end;
end;

procedure TFormMain.panel_JAMCanvasCollapse(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_JAMCanvasExpand(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_PalEditCollapse(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_PalEditExpand(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_RCRControlsCollapse(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_RCRControlsExpand(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_ScaleParametersCollapse(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_ScaleParametersExpand(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_TexFlagsCollapse(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.panel_TexFlagsExpand(Sender: TObject);
begin
  ForceCategoryPanelRedraw(CategoryPanelGroup1);
end;

procedure TFormMain.BatchConvert1Click(Sender: TObject);
begin
  jambatch.JamBatchForm.show;
end;

procedure TFormMain.Browser1Click(Sender: TObject);
begin
  JamBrowser.JamBrowserFrm.show;
end;

procedure TFormMain.btnDrawDataClick(Sender: TObject);
begin
  if boolJamLoaded = true then
  begin
    boolDrawOutlines := not boolDrawOutlines;
    toolbar_drawOutlines.down := boolDrawOutlines;
    menuDrawOutlines.Checked := boolDrawOutlines;
    RefreshCanvas;
  end;
end;

procedure TFormMain.btnExportTextureClick(Sender: TObject);
var
  tempBMP: TBitmap;
begin
  if boolJamLoaded = true then
  begin
    exportDialog.InitialDir := strExportPath;
    if exportDialog.Execute then
    begin
      if boolHWJAM then
        FHWJamFile.ExportTexture(intSelectedTexture, exportDialog.filename)
      else
      begin
        tempBMP := FJamFile.FEntries[intSelectedTexture].FCachedTex[0];
        tempBMP.PixelFormat := pf8bit;
        tempBMP.Palette := creategpxpal;
        tempBMP.SaveToFile(exportDialog.filename);
      end;
      strExportPath := ExtractFilePath(exportDialog.filename);
    end;
  end;
end;

procedure TFormMain.btnGP2palClick(Sender: TObject);
var
  i: integer;
begin

  FJamFile.SetGpxPal(true);

  if boolJamLoaded = true then
  begin
    FJamFile.ReCacheTextures;

    RefreshPalette;
    RefreshCanvas;

    if boolTexSelected then
      DrawTexture;

    boolGP2Jam := true;
    boolGP3Jam := false;
    boolHWJAM := false;

    toolBar_GP3PAL.down := false;
  end;

end;

procedure TFormMain.btnGP3palClick(Sender: TObject);
var
  i: integer;
begin

  FJamFile.SetGpxPal(false);

  if boolJamLoaded = true then
  begin
    FJamFile.ReCacheTextures;

    RefreshPalette;
    RefreshCanvas;

    if boolTexSelected then
      DrawTexture;

    boolGP2Jam := false;
    boolGP3Jam := true;
    boolHWJAM := false;

    toolBar_GP2PAL.down := false;

  end;
end;

procedure TFormMain.NewJam(filename: string; hwJAM: boolean; height: integer);
var
  i: integer;
begin

  if Assigned(FHWJamFile) then
    freeandnil(FHWJamFile);

  if Assigned(FJamFile) then
    freeandnil(FJamFile);

  if hwJAM then
  begin
    boolHWJAM := true;
    if panel_PalPreview.Visible then
      ClearPaletteImg;

    FHWJamFile := THWJamFile.Create;

    FHWJamFile.CreateNewHWJam(filename, height);

  end
  else
  begin
    boolHWJAM := false;

    FJamFile := TJamFile.Create;

    FJamFile.CreateNewJAM(filename, height);

    for i := 0 to 255 do
      localGpxPal[i] := GPXPal[i];

    PalChange(0);

  end;

  intSelectedTexture := -1;
  intJamZoom := 1;

  boolJamLoaded := true;

  UISetup();

  DrawTree;

  RefreshCanvas;

  SetStretchBltMode(ImageCanvas.Canvas.Handle, HALFTONE);

  HWUndoStack.Clear;
  SWUndoStack.Clear;

end;

procedure TFormMain.LoadJam(filename: string);
var
  i: integer;
begin
  boolUndo := false;

  undo1.Enabled := false;
  redo1.Enabled := false;

  HWUndoStack.Clear;
  SWUndoStack.Clear;

  if Assigned(FHWJamFile) then
  begin
    DeSelectTexture;
    freeandnil(FHWJamFile);
  end;

  if Assigned(FJamFile) then
  begin
    DeSelectTexture;
    freeandnil(FJamFile);
  end;

  boolJamLoaded := false;

  boolTexSelected := false;

  JamModified(false);

  if isHWJAM(filename) then
  begin
    boolHWJAM := true;

    if not Assigned(FHWJamFile) then
      FHWJamFile := THWJamFile.Create;

    boolJamLoaded := FHWJamFile.LoadFromFile(filename);
    jamtype := jamGP3HW;
  end
  else
  begin
    boolHWJAM := false;

    if not Assigned(FJamFile) then
      FJamFile := TJamFile.Create;

    jamtype := TJamPaletteDetector.Instance.Detect(filename, false);
    case jamtype of
      jamGP2:
        begin
          FJamFile.SetGpxPal(true)
        end;
      jamGP3SW:
        begin
          FJamFile.SetGpxPal(false)
        end;
    end;

    boolJamLoaded := FJamFile.LoadFromFile(filename, false);
  end;

  if boolJamLoaded then
  begin
    HWUndoStack.Clear;
    SWUndoStack.Clear;
    AddToMRU(filename);

    intSelectedTexture := -1;
    intJamZoom := 1;

    UISetup();

    DrawTree;

    RefreshCanvas;

    SetStretchBltMode(ImageCanvas.Canvas.Handle, HALFTONE);
  end;


  boolUndo := true;

end;

procedure TFormMain.UISetup();
var
  i: integer;

begin
  if boolJamLoaded then
  begin
    SaveDecryptedJAM.Visible := false;
    toolbar_drawOutlines.Enabled := true;
    toolBar_Save.Enabled := true;
    toolBar_AddTexture.Enabled := true;
    toolbarZoomIN.Enabled := true;
    toolbarZoomOUT.Enabled := true;
    toolbarZoomReset.Enabled := true;
    mainMenuSave.Enabled := true;
    mainMenuSaveAs.Enabled := true;
    menuDrawOutlines.Enabled := true;
    ResetZoom1.Enabled := true;

    autoPackTexs.Enabled := boolJamIssues;

    if boolHWJAM then
    begin
      if panel_PalPreview.Visible then
        ClearPaletteImg;

      panel_TexturePreview.Visible := true;
      Panel_TexProperties.Visible := true;
      panel_TexFlags.Visible := true;
      panel_TexScaling.Visible := true;
      panel_ScaleParameters.Visible := true;

      panel_PalPreview.Visible := false;
      // panel_PaletteEdit.Visible := false;

      // panel_palEdit.Visible := false;
      panel_RCRControls.Visible := false;

      toolBar_PalPrev.Visible := false;
      toolBar_Pal1.Visible := false;
      toolBar_Pal2.Visible := false;
      toolBar_Pal3.Visible := false;
      toolBar_Pal4.Visible := false;
      toolBar_PalNext.Visible := false;
      toolBar_GP2PAL.Enabled := false;
      toolBar_GP3PAL.Enabled := false;

      btnGenPal.Enabled := false;
      btnPal0.Enabled := false;
      btnPal1.Enabled := false;
      btnPal2.Enabled := false;
      btnPal3.Enabled := false;
      btnRegenAllPals.Enabled := false;
      btnRemoveAllPals.Enabled := false;
      btnRemovePal.Enabled := false;

      ConverttoGP2JAM.Enabled := true;
      ConverttoGP3SWJAM.Enabled := true;
      ConverttoGP3HWJAM.Enabled := false;

      SaveDecryptedJAM.Visible := false;

    end
    else
    begin
      for i := 0 to 255 do
        localGpxPal[i] := GPXPal[i];
      SaveDecryptedJAM.Visible := true;
      PalChange(0);

      panel_TexturePreview.Visible := true;
      Panel_TexProperties.Visible := true;
      panel_TexFlags.Visible := true;
      panel_TexScaling.Visible := true;
      panel_ScaleParameters.Visible := true;

      panel_PaletteEdit.Visible := true;

      panel_PalEdit.Visible := true;
      panel_RCRControls.Visible := false;

      panel_JAMCanvas.Visible := true;

      toolBar_PalPrev.Visible := true;
      toolBar_Pal1.Visible := true;
      toolBar_Pal2.Visible := true;
      toolBar_Pal3.Visible := true;
      toolBar_Pal4.Visible := true;
      toolBar_PalNext.Visible := true;
      toolBar_GP2PAL.Enabled := true;
      toolBar_GP3PAL.Enabled := true;

      btnPal0.Enabled := true;
      btnPal1.Enabled := true;
      btnPal2.Enabled := true;
      btnPal3.Enabled := true;
      btnRegenAllPals.Enabled := true;
      btnRemoveAllPals.Enabled := true;

      SaveDecryptedJAM.Visible := true;

      case jamtype of
        jamGP2:
          begin
            toolBar_GP3PAL.down := false;
            toolBar_GP2PAL.down := true;
            ConverttoGP2JAM.Enabled := false;
            ConverttoGP3SWJAM.Enabled := true;
            ConverttoGP3HWJAM.Enabled := true;
          end;
        jamGP3SW:
          begin
            toolBar_GP3PAL.down := true;
            toolBar_GP2PAL.down := false;
            ConverttoGP2JAM.Enabled := true;
            ConverttoGP3SWJAM.Enabled := false;
            ConverttoGP3HWJAM.Enabled := true;

          end;
      end;

    end;

    if boolrcrJAM or boolJipMode then
    begin

      panel_PaletteEdit.Visible := false;
      panel_PalEdit.Visible := false;

      toolBar_PalPrev.Visible := false;
      toolBar_Pal1.Visible := false;
      toolBar_Pal2.Visible := false;
      toolBar_Pal3.Visible := false;
      toolBar_Pal4.Visible := false;
      toolBar_PalNext.Visible := false;

      btnGenPal.Enabled := false;
      btnPal0.Enabled := false;
      btnPal1.Enabled := false;
      btnPal2.Enabled := false;
      btnPal3.Enabled := false;
      btnRegenAllPals.Enabled := false;
      btnRemoveAllPals.Enabled := false;
      btnRemovePal.Enabled := false;

      ConverttoGP2JAM.Enabled := false;
      ConverttoGP3SWJAM.Enabled := false;
      ConverttoGP3HWJAM.Enabled := false;
    end
    else
      panel_RCR.Visible := false;
    if boolrcrJAM then
    begin
      panel_RCR.Visible := true;
      panel_RCRControls.Visible := true;
    end
    else if boolJipMode then
    begin
      toolBar_GP2PAL.Enabled := false;
      toolBar_GP3PAL.Enabled := false;
    end;
  end;

  UpdateCaption;

end;

procedure TFormMain.Undo1Click(Sender: TObject);
begin
  DoUndo;
end;

procedure TFormMain.undoTimerTimer(Sender: TObject);
begin
  undoTimer.Enabled := false;
  boolUndo := true;
end;

procedure TFormMain.btnLoadJamClick(Sender: TObject);
begin

  CheckJamModified();

  dlgOpenJam.InitialDir := strOpenPath;

  if dlgOpenJam.Execute then
  begin
    if (dlgOpenJam.filename <> '') and FileExists(dlgOpenJam.filename) then
    begin
      // if boolHWJAM = True then
      // begin
      // if Assigned(FHWJamFile) then
      // begin
      // DeSelectTexture;
      // JamTree.Items.Clear;
      // boolJamLoaded := false;
      // end;
      // end;
      // if Assigned(FJamFile) then
      // begin
      // showmessage('jam file deloading');
      // DeSelectTexture;
      // JamTree.Items.Clear;
      // boolJamLoaded := false;
      // end;
      LoadJam(dlgOpenJam.filename);
      strOpenPath := ExtractFilePath(dlgOpenJam.filename);
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

  if JamSanityCheck then
  begin
    JamSanityCheckInform(true);
    Exit;
  end;

  if boolHWJAM then
    FHWJamFile.SaveToFile(FHWJamFile.JamFullPath)
  else
    FJamFile.SaveToFile(FJamFile.JamFullPath, false);

  JamModified(false);
end;

procedure TFormMain.genMask;
var

  bmp, scaledBMP: TBitmap;
  newWidth, newHeight: integer;
begin

  if boolHWJAM = false then
    bmp := CreateTransparencyMatte(FJamFile.DrawFullJam(true));

  if boolHWJAM = true then
    bmp := CreateTransparencyMatte(FHWJamFile.DrawCanvas(true));

  newWidth := Round(bmp.Width * intJamZoom);
  newHeight := Round(bmp.height * intJamZoom);

  try
    scaledBMP := TBitmap.Create;
    scaledBMP.height := newHeight;
    scaledBMP.Width := newWidth;

    SetStretchBltMode(scaledBMP.Canvas.Handle, HALFTONE);

    StretchBlt(scaledBMP.Canvas.Handle, 0, 0, newWidth, newHeight,
      bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.height, SRCCOPY);

    if boolHWJAM = false then
      scaledBMP := FJamFile.DrawOutlines(scaledBMP);

    if boolHWJAM = true then
      scaledBMP := FHWJamFile.DrawOutlines(scaledBMP);

    ImageCanvas.height := newHeight;
    ImageCanvas.Width := newWidth;
    ImageCanvas.Picture.Bitmap := scaledBMP;

  finally
    freeandnil(scaledBMP);
    freeandnil(bmp);
  end;
end;

procedure TFormMain.Button4Click(Sender: TObject);
begin
  FJamFile.ResizeJam(FJamFile.FHeader.JamTotalHeight);
  RefreshPalette;
  RefreshCanvas;
  DrawTexture;
  TreeReDraw;
end;

procedure TFormMain.Button5Click(Sender: TObject);
var
  i: integer;
  newHeight: integer;
begin
  newHeight := 0;

  if boolHWJAM then
  begin
    for i := 0 to FHWJamFile.FEntries.Count - 1 do
      newHeight := Max(newHeight, FHWJamFile.FEntries[i].FInfo.Y +
        FHWJamFile.FEntries[i].FInfo.height);

    FHWJamFile.FHeader.JamTotalHeight := newHeight;

    RefreshCanvas;
    TreeReDraw;
    JamModified(true);
  end
  else
  begin
    for i := 0 to FJamFile.FEntries.Count - 1 do
      newHeight := Max(newHeight, FJamFile.FEntries[i].FInfo.Y +
        FJamFile.FEntries[i].FInfo.height);

    FJamFile.FHeader.JamTotalHeight := newHeight;

    RefreshCanvas;
    TreeReDraw;

    JamModified(true);

  end;

end;

procedure TFormMain.Button6Click(Sender: TObject);
var

  c: TRGB;
  i: integer;

begin

  for i := 0 to 255 do
  begin
    c.r := i;
    c.g := i;
    c.b := i;
    GPXPal[i] := c;
    localGpxPal[i] := c;
  end;

  RefreshCanvas;

end;

procedure TFormMain.ConvertJAM(output: TJamType);
var
  tmpJAM: TJamFile;
  tmpHWJAM: THWJamFile;
  i: integer;
  tempJamLoc: string;
  tmpFilename: string;
begin

  UpdatingFromCode := true;

  tempJamLoc := TPath.GetTempFileName; // create temp file location

  if jamtype = jamGP3HW then // if current JAM is HW JAM
  begin
    tmpFilename := FHWJamFile.JamFileName;
    FHWJamFile.SaveToFile(tempJamLoc); // save it
    tmpHWJAM := THWJamFile.Create;
    tmpHWJAM.LoadFromFile(tempJamLoc); // load it as temp file
  end
  else
  begin
    tmpFilename := FJamFile.JamFileName;
    FJamFile.SaveToFile(tempJamLoc, false);
    // else it's a GP2 or GP3 SW JAM
    tmpJAM := TJamFile.Create;
    tmpJAM.LoadFromFile(tempJamLoc, false);
    // load it as a normal JAM file
  end;

  case output of

    jamGP2:

      begin
        if Assigned(FJamFile) then
        begin
          for i := 0 to FJamFile.FEntries.Count - 1 do
            FJamFile.FEntries[i].Free;

          FJamFile.FEntries.Clear;
          FJamFile.FHeader.NumItems := 0;
        end
        else
        begin
          FJamFile := TJamFile.Create
        end;

        if jamtype = jamGP3HW then
        begin
          FJamFile.ConvertHWJam(FHWJamFile, true);
          FJamFile.SetGpxPal(true);
          FJamFile.JamFileName := tmpFilename
        end
        else
        begin
          FJamFile.ConvertGPxJam(tmpJAM, true);
          FJamFile.JamFileName := tmpFilename;
        end;

        boolGP2Jam := true;
        boolGP3Jam := false;
        boolHWJAM := false;

        toolBar_GP3PAL.down := false;
        toolBar_GP2PAL.down := true;
        jamtype := jamGP2;

      end;

    jamGP3SW:

      begin

        if Assigned(FJamFile) then
        begin
          for i := 0 to FJamFile.FEntries.Count - 1 do
            FJamFile.FEntries[i].Free;

          FJamFile.FEntries.Clear;

          FJamFile.FHeader.NumItems := 0;
        end
        else
        begin
          FJamFile := TJamFile.Create
        end;

        if jamtype = jamGP3HW then
        begin
          FJamFile.ConvertHWJam(FHWJamFile, false);
          FJamFile.JamFileName := tmpFilename;
        end
        else
        begin
          FJamFile.ConvertGPxJam(tmpJAM, false);
          FJamFile.JamFileName := tmpFilename;
        end;
        FJamFile.SetGpxPal(false);

        boolGP2Jam := false;
        boolGP3Jam := true;
        boolHWJAM := false;

        toolBar_GP3PAL.down := true;
        toolBar_GP2PAL.down := false;
        jamtype := jamGP3SW;
      end;

    jamGP3HW:

      begin

        if Assigned(FJamFile) then
        begin
          freeandnil(FJamFile);
        end;

        if Assigned(FHWJamFile) then
        begin
          for i := 0 to FHWJamFile.FEntries.Count - 1 do
            FHWJamFile.FEntries[i].Free;

          FHWJamFile.FEntries.Clear;

          FHWJamFile.FHeader.NumItems := 0;
        end
        else
        begin
          FHWJamFile := THWJamFile.Create;
        end;
        FHWJamFile.ConvertGPxJam(tempJamLoc);
        FHWJamFile.JamFileName := tmpFilename;

        boolGP2Jam := false;
        boolGP3Jam := false;
        boolHWJAM := true;

        jamtype := jamGP3HW;

        toolBar_GP3PAL.down := true;
        toolBar_GP2PAL.down := false;
      end;
  end;

  HWUndoStack.Clear;
  SWUndoStack.Clear;

  UpdatingFromCode := false;

  UISetup;

  DrawTree;
  RefreshCanvas;

  if boolTexSelected then
    DrawTexture;

  if TFile.Exists(tempJamLoc) then
  begin
    try
      TFile.Delete(tempJamLoc);
    except
      on E: EFOpenError do
        // “file in use” — you might need to close whatever app has it open
        raise Exception.CreateFmt('Cannot delete %s: it is still in use.',
          [tempJamLoc]);
      on E: Exception do
        raise;
    end;
  end;

end;

procedure TFormMain.ConverttoGP2JAMClick(Sender: TObject);
begin
  ConvertJAM(jamGP2);
end;

procedure TFormMain.ConverttoGP3HWJAMClick(Sender: TObject);
begin

  ConvertJAM(jamGP3HW);
end;

procedure TFormMain.ConverttoGP3SWJAMClick(Sender: TObject);
begin
  ConvertJAM(jamGP3SW);
end;

procedure TFormMain.btnRegenAllPalsClick(Sender: TObject);
var
  i: integer;
  tmpCanvas: TBitmap;
begin

  intSimplifyThreshold := Round(numBox_SimpThresh.Value);
  intSimplifyMethod := comboSimpMethod.ItemIndex;
  intBlurThreshold := Round(numBox_BlurAmount.Value);
  boolSimpifyAllPals := chkBoxSimpPal.Checked;
  boolProtectTrans := chkBoxTrans.Checked;

  tmpCanvas := TBitmap.Create;

  for i := 0 to FJamFile.FEntries.Count - 1 do
    if Assigned(FJamFile.FEntries[i].FTexture) then
    begin
      tmpCanvas.Assign(FJamFile.FEntries[i].FOriginalTex);
      freeandnil(FJamFile.FEntries[i].FTexture);
      FJamFile.FEntries[i].FTexture := TBitmap.Create;
      FJamFile.FEntries[i].FTexture := FJamFile.GenerateGPxBMP(tmpCanvas, i,
        intSimplifyMethod, intSimplifyThreshold, intBlurThreshold,
        boolSimpifyAllPals, boolProtectTrans);
      FJamFile.CachePaletteBMP(i);
    end;

  RefreshPalette;
  RefreshCanvas;
  DrawTexture;
  TreeReDraw;
end;

procedure TFormMain.rcrOddClick(Sender: TObject);

begin
  boolRCRDrawMode := true;
  RefreshCanvas;
end;

procedure TFormMain.rcrResetClick(Sender: TObject);
begin

  RefreshCanvas;
end;

procedure TFormMain.rcrEvenClick(Sender: TObject);
begin
  boolRCRDrawMode := false;
  RefreshCanvas
end;

procedure TFormMain.btnGenPalClick(Sender: TObject);

begin
  generatePal := true;
  JamReGen;

  RefreshPalette;
  RefreshCanvas;
  DrawTexture;

  TreeReDraw;
end;

procedure TFormMain.btnRemoveAllPalsClick(Sender: TObject);
var
  i: integer;
begin

  for i := 0 to FJamFile.FEntries.Count - 1 do
    FJamFile.ZeroPalette(i);
  TreeReDraw;
end;

procedure TFormMain.btnRemovePalClick(Sender: TObject);
begin
  FJamFile.ZeroPalette(intSelectedTexture);
  TreeReDraw;
end;

procedure TFormMain.canvasPopupMenuPopup(Sender: TObject);
begin

  popUpDeleteTexture.Enabled := boolTexSelected;
  popupImportTexture.Enabled := boolTexSelected;
  popupExportTexture.Enabled := boolTexSelected;
  popUpAddTexture.Enabled := boolJamLoaded;
end;

procedure TFormMain.CheckJamModified();
var
  filename: string;
begin

  if boolJamLoaded then
  begin

    if boolHWJAM then
      filename := FHWJamFile.JamFullPath
    else
      filename := FJamFile.JamFullPath;

    if boolJamModified then
    begin
      // Ask the user if they want to save
      case MessageDlg('Do you want to save the file?', mtConfirmation,
        [mbYes, mbNo], 0) of
        mrYes:
          begin
            // If we have no filename yet, run Save As...
            if filename = '' then
            begin
              dlgSaveJam.filename := FJamFile.JamFileName + '.jam';

              if dlgSaveJam.Execute then
              begin
                if boolHWJAM then
                  FHWJamFile.SaveToFile(dlgSaveJam.filename)
                else
                  FJamFile.SaveToFile(dlgSaveJam.filename, false)
              end
              else
                // user cancelled the Save As dialog
                Exit;
            end
            else
            begin
              if boolHWJAM then
                FHWJamFile.SaveToFile(filename)
              else
                FJamFile.SaveToFile(filename, false);
            end;
          end;

        mrNo:
          // just continue without saving
          Exit;
      end;
    end;
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CheckJamModified;
end;

procedure TFormMain.ShowHintInStatusBar(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := GetLongHint(Application.Hint);
end;

procedure TFormMain.MsgHandler(var Msg: TMsg; var Handled: boolean);

var
  i, idx: integer;
  Entry: TjamEntry;
  EntryHW: THWjamEntry;
  Key: Word;
  movestep: integer;
  NewX, NewY: integer;
  Changed: boolean;
begin
  Changed := false;

  if (Msg.message = WM_KEYUP) then
  begin
    Key := Msg.wParam;
    Handled := false;

    if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
    begin
      boolUndo := true;
    end;

    case Key of
      17:
        begin
          boolLCtrl := false;
          Handled := true;
        end;
    end;

  end;

  if (Msg.message = WM_KEYDOWN) then
  begin
    Key := Msg.wParam;
    Handled := false; // default is: let the message go through

    if Key = 17 then // VK_CONTROL
    begin
      boolLCtrl := true;
      Handled := true;
      Exit;
    end;

    if (SelectedTextureList.Count = 0) or (not boolLCtrl) then
      Exit;

    for idx in SelectedTextureList do
    begin
      if boolHWJAM then
        EntryHW := FHWJamFile.Entries[idx]
      else
        Entry := FJamFile.Entries[idx];

      if GetKeyState(VK_SHIFT) < 0 then
        movestep := 10
      else
        movestep := 1;

      UpdatingFromCode := false;
      Changed := false;

      case Key of
        VK_LEFT:
          begin
            if boolUndo then
            PushUndoState;
            boolUndo := false;
            if boolHWJAM then
            begin
              NewX := Max(0, EntryHW.FInfo.X - movestep);
              if EntryHW.FInfo.X <> NewX then
              begin
                EntryHW.FInfo.X := NewX;
                Changed := true;
              end;
            end
            else
            begin
              NewX := Max(0, Entry.FInfo.X - movestep);
              if Entry.FInfo.X <> NewX then
              begin
                Entry.FInfo.X := NewX;
                Changed := true;
              end;
            end;
          end;

        VK_RIGHT:
          begin
            if boolUndo then
            PushUndoState;
            boolUndo := false;
            if boolHWJAM then
            begin
              NewX := Min(255, EntryHW.FInfo.X + movestep);
              if EntryHW.FInfo.X <> NewX then
              begin
                EntryHW.FInfo.X := NewX;
                Changed := true;
              end;
            end
            else
            begin
              NewX := Min(255, Entry.FInfo.X + movestep);
              if Entry.FInfo.X <> NewX then
              begin
                Entry.FInfo.X := NewX;
                Changed := true;
              end;
            end;
          end;

        VK_UP:
          begin
            if boolUndo then
            PushUndoState;
            boolUndo := false;
            if boolHWJAM then
            begin
              NewY := Max(0, EntryHW.FInfo.Y - movestep);
              if EntryHW.FInfo.Y <> NewY then
              begin
                EntryHW.FInfo.Y := NewY;
                Changed := true;
              end;

            end
            else
            begin
              NewY := Max(0, Entry.FInfo.Y - movestep);
              if Entry.FInfo.Y <> NewY then
              begin
                Entry.FInfo.Y := NewY;
                Changed := true;
              end;
            end;
          end;

        VK_DOWN:
          begin
            if boolUndo then
            PushUndoState;
            boolUndo := false;
            if boolHWJAM then
            begin
              NewY := Min(FHWJamFile.FHeader.JamTotalHeight - 1,
                EntryHW.FInfo.Y + movestep);
              if EntryHW.FInfo.Y <> NewY then
              begin
                EntryHW.FInfo.Y := NewY;
                Changed := true;
              end;

            end
            else
            begin
              NewY := Min(FJamFile.FHeader.JamTotalHeight - 1,
                Entry.FInfo.Y + movestep);
              if Entry.FInfo.Y <> NewY then
              begin
                Entry.FInfo.Y := NewY;
                Changed := true;
              end;
            end;
          end;

      else
        Exit; // unknown key: allow default processing (Handled stays False)
      end;

      if Changed then
      begin
        if SelectedTextureList.Count = 1 then
          UpdateUIData(intSelectedTexture);

        timer_redrawTree.Enabled := false;
        timer_redrawTree.Enabled := true;
        RefreshCanvas;
        JamModified(true);

        Handled := true; // We handled this movement key
      end;
    end;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
  Reg: TRegistry;

begin
  boolLCtrl := false;

  ClipboardJAM := RegisterClipboardFormat(CLIPBOARD_JAM);
  ClipboardHWJAM := RegisterClipboardFormat(CLIPBOARD_HWJAM);

  SWUndoStack := TStack<TJamFile>.Create;
  HWUndoStack := TStack<THWJamFile>.Create;

  SWRedoStack := TStack<TJamFile>.Create;
  HWRedoStack := TStack<THWJamFile>.Create;

  boolUndo := true;

  Application.OnHint := ShowHintInStatusBar;
  SelectedTextureList := TList<integer>.Create;

  SelectedTreeNodes := TList<TTreeNode>.Create;

  Application.OnMessage := self.MsgHandler;

  FJamFile.SetGpxPal(false);

  for i := 0 to 255 do
    localGpxPal[i] := GPXPal[i];

  intPaletteID := 0;

  intJamZoom := 1;

  intSimplifyThreshold := 20;
  intSimplifyMethod := 3;
  intBlurThreshold := 1;
  // intSimplifyDist := 20;
  boolSimpifyAllPals := false;
  boolProtectTrans := false;

  boolFlagChange := false;
  boolXChange := false;
  boolYChange := false;
  boolWidthChange := false;
  boolHeightChange := false;
  boolIDChange := false;

  boolJamIssues := false;

  generatePal := false;

  boolJamLoaded := false;

  boolHWJAM := false;

  UpdatingFromCode := false;

  timer_redrawTree.Enabled := false;

  comboSimpMethod.ItemIndex := intSimplifyMethod;

  numBox_SimpThresh.Value := Round(intSimplifyThreshold);
  numBox_BlurAmount.Value := intBlurThreshold;
  chkBoxSimpPal.Checked := boolSimpifyAllPals;
  chkBoxTrans.Checked := boolProtectTrans;

  Reg := TRegistry.Create(KEY_READ);

  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(baseKeyPath, true) then
    begin
      strOpenPath := Reg.ReadString('OpenPath');
      strSavePath := Reg.ReadString('SavePath');
      strImportPath := Reg.ReadString('ImportPath');
      strExportPath := Reg.ReadString('ExportPath');
      strBrowserPath := Reg.ReadString('BrowserPath');
      strGP2Location := Reg.ReadString('GP2Location');
      strGP3Location := Reg.ReadString('GP3Location');
      strGP32kLocation := Reg.ReadString('GP32kLocation');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  intMaxMRU := 10;
  MRUList := TStringList.Create;

  LoadMRU;
  UpdateMRU;

  with ImageEntry.Picture.Bitmap do
  begin
    Width := 1;
    height := 1;
    Canvas.Brush.Color := panel_TexPreview.Color;
    // or any background color
    Canvas.FillRect(Rect(0, 0, Width, height));
  end;

  PaintBoxPalette.Canvas.Brush.Color := panel_PalPreview.Color;
  // or clBtnFace
  PaintBoxPalette.Canvas.FillRect(PaintBoxPalette.ClientRect);

end;

procedure TFormMain.PreviewMask1Click(Sender: TObject);
begin
  genMask;
end;

procedure TFormMain.PushUndoState();
begin
  if boolHWJAM then
  begin
    HWUndoStack.Push(FHWJamFile.Clone);

    while HWRedoStack.Count > 0 do
      HWRedoStack.pop.Free;

    Undo1.Enabled := HWUndoStack.Count > 0;
    Redo1.Enabled := HWRedoStack.Count > 0;

  end
  else
  begin
    SWUndoStack.Push(FJamFile.Clone);

    while SWRedoStack.Count > 0 do
      SWRedoStack.pop.Free;

    Undo1.Enabled := SWUndoStack.Count > 0;
    Redo1.Enabled := SWRedoStack.Count > 0;

  end;

end;

procedure TFormMain.DoUndo();
begin

  if boolHWJAM then
  begin
    if HWUndoStack.Count = 0 then
    begin
      Undo1.Enabled := false;
      Exit;
    end;

    if HWUndoStack.Count = intMaxUndo then
      HWUndoStack.pop;

    HWRedoStack.Push(FHWJamFile.Clone);

    // Free current file
    FHWJamFile.Free;

    // Restore previous
    FHWJamFile := HWUndoStack.pop;

  end
  else
  begin

    if SWUndoStack.Count = 0 then
    begin
      Undo1.Enabled := false;
      Exit;
    end;

    SWRedoStack.Push(FJamFile.Clone);

    if SWRedoStack.Count = 0 then
      Redo1.Enabled := false
    else
      Redo1.Enabled := true;

    // Free current file
    FJamFile.Free;

    // Restore previous
    FJamFile := SWUndoStack.pop;

  end;

  RefreshCanvas;
  TreeReDraw;
  JamModified(true);

end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  Reg: TRegistry;
  i: integer;
begin
  Reg := TRegistry.Create(KEY_WRITE);

  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(baseKeyPath, true) then
    begin
      Reg.WriteString('OpenPath', strOpenPath);
      Reg.WriteString('SavePath', strSavePath);
      Reg.WriteString('ImportPath', strImportPath);
      Reg.WriteString('ExportPath', strExportPath);
      Reg.WriteString('BrowserPath', strBrowserPath);
      Reg.WriteString('GP2Location', strGP2Location);
      Reg.WriteString('GP3Location', strGP3Location);
      Reg.WriteString('GP32kLocation', strGP32kLocation);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  MRUList.Free;
  SelectedTextureList.Free;

  while SWUndoStack.Count > 0 do
    SWUndoStack.pop.Free;

  while HWUndoStack.Count > 0 do
    HWUndoStack.pop.Free;

  while SWRedoStack.Count > 0 do
    SWRedoStack.pop.Free;

  while HWRedoStack.Count > 0 do
    HWRedoStack.pop.Free;

  SWUndoStack.Free;
  HWUndoStack.Free;

  SWRedoStack.Free;
  HWRedoStack.Free;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: integer;
begin
  // if boolJamLoaded = false then
  // Exit;
  //
  // if SelectedTextureList.Count = 0 then
  // Exit;
  //
  // case Key of
  // VK_DELETE:
  // begin
  // DeleteTexture;
  // Key := 0;
  // end;
  //
  // end;

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

  if boolJamLoaded = false then
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

  if intJamZoom <= 0.4 then
    Exit;

  // Recenter scroll based on relative mouse position
  ScrollBox1.HorzScrollBar.Position := Round((RelX * NewZoom) - CursorPos.X);
  ScrollBox1.VertScrollBar.Position := Round((RelY * NewZoom) - CursorPos.Y);

  RefreshCanvas;
  Handled := true;
end;

//
// procedure TFormMain.ImageCanvasMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// var
// I, localX, localY: Integer;
// EntryRef: TJamEntryRef;
// intCount : integer;
// begin
// if Button <> mbLeft then Exit;
// if not boolJamLoaded then Exit;
//
// localX := Round(X / intJamZoom);
// localY := Round(Y / intJamZoom);
// intSelectedTexture := -1;
//
//
//
// for I := 0 to intCount do
// begin
//
// if boolHWJAM then
// EntryRef := TJamEntryRef.FromHW(FHWJamFile.Entries[i].Info)
// else
// EntryRef := TJamEntryRef.FromSW(FJamFile.Entries[i].Info);
//
// if PtInRect(Rect(EntryRef.X, EntryRef.Y, EntryRef.X + EntryRef.Width, EntryRef.Y + EntryRef.Height), Point(localX, localY)) then
// begin
// intSelectedTexture := I;
// CurrentResizeMode := HitTestResizeZone(localX, localY, EntryRef.X, EntryRef.Y, EntryRef.Width, EntryRef.Height);
// LastMousePos := Point(localX, localY);
// Break;
// end;
// end;
//
//
// if intSelectedTexture = -1 then
// DeSelectTexture
// else
// SelectTexture(intSelectedTexture);
//
// end;

procedure TFormMain.ImageCanvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i, jamX, jamY, jamW, jamH, tempX, tempY: integer;
begin

  if not boolJamLoaded then
    Exit;
  if not(ssShift in Shift) then
  begin
    SelectedTextureList.Clear;
    SelectedTreeNodes.Clear;
  end;

  UpdatingFromCode := true;
  SelectFromCanvas := true;
  intSelectedTexture := -1;

  tempX := X;
  tempY := Y;

  if boolHWJAM then
  begin
    for i := 0 to FHWJamFile.Entries.Count - 1 do
    begin
      with FHWJamFile.Entries[i].Info do
      begin
        jamX := Round(X * intJamZoom);
        jamY := Round(Y * intJamZoom);
        jamW := Round(Width * intJamZoom);
        jamH := Round(height * intJamZoom);
      end;

      if (X >= jamX) and (X < jamX + jamW) and (Y >= jamY) and (Y < jamY + jamH)
      then
      begin
        intSelectedTexture := i;
        if not SelectedTextureList.Contains(i) then
          SelectedTextureList.Add(i);

        Break;
      end;
    end;
  end
  else
  begin
    for i := 0 to FJamFile.Entries.Count - 1 do
    begin
      with FJamFile.Entries[i].Info do
      begin
        jamX := Round(X * intJamZoom);
        jamY := Round(Y * intJamZoom);
        if jamY mod 2 <> 0 then
          jamX := jamX + 256;

        jamW := Round(Width * intJamZoom);
        jamH := Round(height * intJamZoom);
        if boolrcrJAM then
        begin
          jamW := jamW * 2;
          jamH := jamH div 2;
          jamY := jamY div 2;
        end;
      end;

      if (tempX >= jamX) and (tempX < jamX + jamW) and (tempY >= jamY) and
        (tempY < jamY + jamH) then
      begin
        intSelectedTexture := i;
        if not SelectedTextureList.Contains(i) then
          SelectedTextureList.Add(i);
        Break;
      end;
    end;
  end;

  if intSelectedTexture = -1 then
  begin
    DeSelectTexture;
    SelectedTextureList.Clear;
    UpdatingFromCode := false;
    SelectFromCanvas := false;
  end
  else
  begin
    SelectTexture(intSelectedTexture, false);
    if SelectedTextureList.Count > 1 then
    begin
      UpdatingFromCode := true;
      tex_Y.Value := 0;
      tex_X.Value := 0;
      tex_ID.Value := 0;
      tex_width.Value := 0;
      tex_height.Value := 0;
    end;
    UpdatingFromCode := false;
    SelectFromCanvas := false;
  end;

end;

// procedure TFormMain.ImageCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
// var
// dx, dy: Integer;
// Info: TJamEntryRef;
// begin
// if intSelectedTexture = -1 then Exit;
// if ssLeft in Shift then
// begin
// dx := Round(X / intJamZoom) - LastMousePos.X;
// dy := Round(Y / intJamZoom) - LastMousePos.Y;
// end;
//
// if boolHWJAM then
// info := TJamEntryRef.FromHW(FHWJamFile.Entries[intSelectedTexture].Info)
// else
// info := TJamEntryRef.FromSW(FJamFile.Entries[intSelectedTexture].Info);
//
//
// case CurrentResizeMode of
// rmMove:
// begin
// info.SetX(info.X + dx);
// info.SetY(info.y + dy);
// end;
// rmTopLeft:
// begin
// info.SetX(info.X - dx);
// info.SetWidth(dx + info.Width);
// info.sety(info.y - dy);
// info.setheight(info.width + dy);
// end;
// rmTop:
// begin
// info.setY(info.y - dy);
// info.setheight(info.height + dy)
// end;
// rmTopRight:
// begin
// info.SetWidth(info.Width + dx);
// info.SetY(info.y - dy);
// info.SetHeight(info.height + dy);
// end;
// rmRight:
// info.SetWidth(dx + info.Width);
// rmBottomRight:
// begin
// info.SetWidth(dx + info.Width);
// info.setheight(info.width + dy);
// end;
// rmBottom:
// info.setheight(info.width + dx);
// rmBottomLeft:
// begin
// info.setx(info.x - dx);
// info.SetWidth(dx + info.Width);
// info.setheight(info.width + dx);
//
// end;
// rmLeft:
// begin
// info.setx(info.x - dx);
// info.setwidth(info.width + dx);
// end;
// end;
//
// LastMousePos := Point(Round(X / intJamZoom), Round(Y / intJamZoom));
//
// // Ensure values stay positive and within bounds
// Info.SetWidth(Max(1, Info.Width));
// Info.SetHeight(Max(1, Info.Height));
//
/// /    // Write back updated info
/// /    if boolHWJam then
/// /      FHWJamFile.Entries[intSelectedTexture].Info := Info
/// /    else
/// /      FJamFile.Entries[intSelectedTexture].Info := Info;
//
// // Push to UI + Canvas
// //UpdateJamData(intSelectedTexture);
// refreshcanvas;
// end;

procedure TFormMain.ImageCanvasMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
    CurrentResizeMode := rmNone;
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
  if Stage = cdPrePaint then
  begin
    if SelectedTreeNodes.Contains(Node) then
    begin
      Sender.Canvas.Brush.Color := clSkyBlue;
      Sender.Canvas.FillRect(Node.DisplayRect(true));
    end;
  end;

  if Stage <> cdPostPaint then
    Exit;

  if not Assigned(Node) or not Assigned(Node.data) then
    Exit;

  JNode := TJamTreeNode(Node.data);
  if not(JNode.nodeType in [jamColour]) then
    Exit; // draw only on colour nodes

  Canvas := Sender.Canvas;

  // 1. Get the area where the node is drawn
  TextRect := Node.DisplayRect(true);

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

  DefaultDraw := true;
end;

procedure TFormMain.JamTreeChange(Sender: TObject; Node: TTreeNode);
var
  jamNode: TJamTreeNode;
begin

  if UpdatingFromCode = false then
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
          SelectTexture(jamNode.jamID, true);
        end;

      jamPosX:
        begin
          SelectTexture(jamNode.jamID, true);
        end;

      jamposY:
        begin
          SelectTexture(jamNode.jamID, true);
        end;

      jamwidth:
        begin

          SelectTexture(jamNode.jamID, true);
        end;

      jamheight:
        begin

          SelectTexture(jamNode.jamID, true);
        end;
      jamData:
        begin
          SelectTexture(jamNode.jamID, true);
        end;

      jamPalInfo:
        begin

          SelectTexture(jamNode.jamID, true);
        end;

      jamFlags:
        begin

          SelectTexture(jamNode.jamID, true);
        end;

      jamColour:
        begin

          SelectTexture(jamNode.jamID, true);
        end;

      jamUnk:
        begin

          SelectTexture(jamNode.jamID, true);
        end;

      jamID:
        begin
          SelectTexture(jamNode.jamID, true);
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
  JamTree.Invalidate;
end;

procedure TFormMain.JamTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.data) then
  begin
    TObject(Node.data).Free;
    Node.data := nil;
  end;
end;

procedure TFormMain.JamTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  ClickedNode: TTreeNode;
  ClickedData, ExistingData: TJamTreeNode;
  i, j: integer;
begin
  // ClickedNode := JamTree.GetNodeAt(X, Y);
  //
  // if not Assigned(ClickedNode) or not Assigned(ClickedNode.Data) then
  // Exit;
  //
  // ClickedData := TJamTreeNode(ClickedNode.Data);
  //
  //
  // // Check if user is attempting a multi-selection
  // if (ssCtrl in Shift) or (ssShift in Shift) then
  // begin
  // // We only allow multi-selection if ALL are of type jamID
  // if ClickedData.NodeType <> jamID then
  // begin
  // // Block multi-select for this type: clear selection and allow only the clicked one
  // for I := 0 to JamTree.Items.Count - 1 do
  // JamTree.Items[I].Selected := False;
  //
  // ClickedNode.Selected := True;
  // Exit;
  // end;
  //
  // // If we are adding to a multi-select, make sure all selected nodes are jamID
  // for I := 0 to JamTree.Items.Count - 1 do
  // begin
  // if JamTree.Items[I].Selected then
  // begin
  // ExistingData := TJamTreeNode(JamTree.Items[I].Data);
  // if ExistingData.NodeType <> jamID then
  // begin
  // // Invalid multi-select — clear and select only the clicked one
  // for  j:= 0 to JamTree.Items.Count - 1 do
  // JamTree.Items[I].Selected := False;
  // ClickedNode.Selected := True;
  // Exit;
  // end;
  // end;
  // end;
  // end;
end;

procedure TFormMain.RefreshCanvas;
var
  bmp, scaledBMP: TBitmap;
  newWidth, newHeight: integer;
begin

  if boolrcrJAM = false then
  begin
    JamSanityCheck;
    JamSanityCheckInform(false);

    autoPackTexs.Enabled := boolJamIssues;
  end;

  if boolHWJAM = false then
    bmp := FJamFile.DrawFullJam(true);

  if boolHWJAM = true then
    bmp := FHWJamFile.DrawCanvas(true);

  newWidth := Round(bmp.Width * intJamZoom);
  newHeight := Round(bmp.height * intJamZoom);

  try
    scaledBMP := TBitmap.Create;
    scaledBMP.height := newHeight;
    scaledBMP.Width := newWidth;

    SetStretchBltMode(scaledBMP.Canvas.Handle, HALFTONE);

    StretchBlt(scaledBMP.Canvas.Handle, 0, 0, newWidth, newHeight,
      bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.height, SRCCOPY);

    if boolHWJAM = false then
      scaledBMP := FJamFile.DrawOutlines(scaledBMP);

    if boolHWJAM = true then
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
      panel_PalPreview.Visible := false
    else
    begin
      panel_PalPreview.Visible := true;
      PaintBoxPalette.Repaint;
    end;
  PaintBoxPalette.Invalidate;
end;

procedure TFormMain.ResetZoom1Click(Sender: TObject);
begin
  intJamZoom := 1;
  RefreshCanvas;
end;

procedure TFormMain.Splitter2Paint(Sender: TObject);
var
  Width: integer;
  height: integer;
  ratio: Double;
begin

  if boolJamLoaded = false then
    Exit;

  ratio := ImageEntry.Picture.Graphic.height / ImageEntry.Picture.Graphic.Width;

  ImageEntry.height := Round(ImageEntry.Width * ratio);

  if ImageEntry.height > 256 then
    ImageEntry.height := 256;

end;

procedure TFormMain.texScaleChange(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode or userisTyping then
    Exit;
  boolScaleChange := true;
  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.texScaleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    texScaleChange(Sender); // manually trigger change
  end;
end;

procedure TFormMain.texScaleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false
end;

procedure TFormMain.texScaleXChange(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode or userisTyping then
    Exit;
  boolScaleXChange := true;
  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.texScaleXKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    texScaleXChange(Sender); // manually trigger change
  end;

end;

procedure TFormMain.texScaleXKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false
end;

procedure TFormMain.texScaleYChange(Sender: TObject);
begin
  var
    i: integer;
  begin
    if UpdatingFromCode or userisTyping then
      Exit;
    boolScaleYChange := true;
    for i := 0 to SelectedTextureList.Count - 1 do
      UpdateJamData(SelectedTextureList[i]);
  end;

end;

procedure TFormMain.texScaleYKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    texScaleYChange(Sender); // manually trigger change
  end;

end;

procedure TFormMain.texScaleYKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false

end;

procedure TFormMain.tex_flagsClickCheck(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode then
    Exit;
  boolFlagChange := true;
  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.tex_heightChange(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode or userisTyping then
    Exit;

  boolHeightChange := true;

   if boolUndo then
  PushUndoState;

  boolUndo := false;
  UndoTimer.Enabled := false;
  UndoTimer.Enabled := true;

  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.tex_heightKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    tex_heightChange(Sender); // manually trigger change
  end;

end;

procedure TFormMain.tex_heightKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false;
end;

procedure TFormMain.tex_IDChange(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode or userisTyping then
    Exit;

  boolIDChange := true;

  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.tex_IDKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    tex_IDChange(Sender); // manually trigger change
  end;

end;

procedure TFormMain.tex_IDKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false;

end;

procedure TFormMain.tex_widthChange(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode or userisTyping then
    Exit;

  boolWidthChange := true;

   if boolUndo then
  PushUndoState;

  boolUndo := false;
  UndoTimer.Enabled := false;
  UndoTimer.Enabled := true;

  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.tex_widthKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    tex_widthChange(Sender); // manually trigger change
  end;

end;

procedure TFormMain.tex_widthKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false;
end;

procedure TFormMain.tex_XChange(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode or userisTyping then
    Exit;
  boolXChange := true;
  if boolUndo then
  PushUndoState;

  boolUndo := false;
  UndoTimer.Enabled := false;
  UndoTimer.Enabled := true;

  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.tex_XKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    tex_XChange(Sender); // manually trigger change
  end;

end;

procedure TFormMain.tex_XKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false;

end;

procedure TFormMain.tex_XMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if boolUndo then
    PushUndoState;

    boolUndo := false;
end;

procedure TFormMain.tex_XMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    boolUndo := true;
end;

procedure TFormMain.tex_YChange(Sender: TObject);
var
  i: integer;
begin
  if UpdatingFromCode or userisTyping then
    Exit;

  boolYChange := true;

   if boolUndo then
  PushUndoState;

  boolUndo := false;
  UndoTimer.Enabled := false;
  UndoTimer.Enabled := true;

  for i := 0 to SelectedTextureList.Count - 1 do
    UpdateJamData(SelectedTextureList[i]);
end;

procedure TFormMain.tex_YKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  userisTyping := true;
  if Key = VK_RETURN then
  begin
    userisTyping := false;
    tex_YChange(Sender); // manually trigger change
  end;

end;

procedure TFormMain.tex_YKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  // If the user presses a non-editing key, you might reset typing
  if Key = VK_ESCAPE then
    userisTyping := false;
end;

procedure TFormMain.timer_JamRedrawPalsTimer(Sender: TObject);
begin
  timer_JamRedrawPals.Enabled := false;
  JamReGen;
end;

procedure TFormMain.timer_redrawTreeTimer(Sender: TObject);
begin
  timer_redrawTree.Enabled := false;
  UpdatingFromCode := true;
  TreeReDraw;

  UpdatingFromCode := false;
end;

procedure TFormMain.toolBar_GP2PALMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if toolBar_GP2PAL.down = false then
    toolBar_GP2PAL.down := true;
end;

procedure TFormMain.toolBar_GP3PALMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if toolBar_GP3PAL.down = false then
    toolBar_GP3PAL.down := true;
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
  Count, i: integer;
  PalBytes: TBytes;
  c: TColor;
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

    for i := 0 to Count - 1 do
    begin
      c := RGBFromTRGB(GPXPal[PalBytes[i]]);
      localGpxPal[i] := GPXPal[PalBytes[i]];

      // Use rounded boundaries
      CellLeft := Round(i * Scale);
      CellRight := Round((i + 1) * Scale);

      PaintBoxPalette.Canvas.Brush.Color := c;
      PaintBoxPalette.Canvas.FillRect(Rect(CellLeft, 0, CellRight,
        PaintBoxPalette.height));
    end;
  end;
end;

procedure TFormMain.mainMenuImportCanvasClick(Sender: TObject);
begin

  importDialog.InitialDir := strImportPath;
  if importDialog.Execute then
  begin
    if (importDialog.filename <> '') and FileExists(importDialog.filename) then
    begin
      if boolHWJAM then
        FHWJamFile.ImportCanvas(importDialog.filename)
      else
        FJamFile.ImportCanvas(importDialog.filename);
      strImportPath := ExtractFilePath(importDialog.filename);
    end;

  end
  else
    Exit;

  RefreshCanvas;

  if not boolHWJAM then
    RefreshPalette;

  DrawTexture;

end;

procedure TFormMain.mainMenuImportTextureClick(Sender: TObject);
begin
  ImportTexture(Sender);
end;

procedure TFormMain.mainMenuPasteClick(Sender: TObject);
var
  jamList: TList<TjamEntry>;
  jamHWList: TList<THWjamEntry>;
  i: integer;
begin
  if boolHWJAM then
  begin
    if not Clipboard.HasFormat(ClipboardHWJAM) then
      Exit;

    DeSelectTexture;
    SelectedTextureList.Clear;

    jamHWList := PasteHWJam();

    for i := 0 to jamHWList.Count - 1 do
      FHWJamFile.FEntries.Add(jamHWList[i]);

    intSelectedTexture := FHWJamFile.FEntries.Count - 1;
    if not SelectedTextureList.Contains(intSelectedTexture) then
      SelectedTextureList.Add(intSelectedTexture);

    RefreshCanvas;

    UpdatingFromCode := true;

    DrawTree;
    SelectTreeTex;
    JamModified(true);

    UpdatingFromCode := false;

    jamHWList.Clear;
    jamHWList.Free;

  end
  else
  begin

    if not Clipboard.HasFormat(ClipboardJAM) then
      Exit;

    DeSelectTexture;
    SelectedTextureList.Clear;

    jamList := PasteJam();

    for i := 0 to jamList.Count - 1 do
      FJamFile.FEntries.Add(jamList[i]);

    intSelectedTexture := FJamFile.FEntries.Count - 1;
    if not SelectedTextureList.Contains(intSelectedTexture) then
      SelectedTextureList.Add(intSelectedTexture);

    JamReGen;

    UpdatingFromCode := true;

    DrawTree;
    SelectTreeTex;
    JamModified(true);

    UpdatingFromCode := false;

    jamList.Clear;
    jamList.Free;
  end;

end;

procedure TFormMain.mainMenuSaveAsClick(Sender: TObject);
begin

  if JamSanityCheck then
  begin
    JamSanityCheckInform(true);
    Exit;
  end;

  if boolHWJAM then
    dlgSaveJam.filename := FHWJamFile.JamFileName + '.jam'
  else
    dlgSaveJam.filename := FJamFile.JamFileName + '.jam';

  if dlgSaveJam.Execute then
  begin
    if boolHWJAM then
    begin
      FHWJamFile.SaveToFile(dlgSaveJam.filename);
      FHWJamFile.JamFileName :=
        lowercase(ChangeFileExt(ExtractFileName(dlgSaveJam.filename), ''));
      FHWJamFile.JamFullPath := dlgSaveJam.filename;
    end
    else
    begin
      FJamFile.SaveToFile(dlgSaveJam.filename, false);
      FJamFile.JamFileName :=
        lowercase(ChangeFileExt(ExtractFileName(dlgSaveJam.filename), ''));
      FJamFile.JamFullPath := dlgSaveJam.filename;
    end;

    JamModified(false);

  end
  else
    // user cancelled the Save As dialog
    Exit;

end;

procedure TFormMain.ImportTexture(Sender: TObject);
begin
  importDialog.InitialDir := strImportPath;
  if importDialog.Execute then
  begin
    if (importDialog.filename <> '') and FileExists(importDialog.filename) then
    begin
      if boolHWJAM then
        FHWJamFile.ImportTexture(intSelectedTexture, importDialog.filename)
      else
        FJamFile.ImportTexture(intSelectedTexture, importDialog.filename);
      strImportPath := ExtractFilePath(importDialog.filename);
    end;

  end
  else
    Exit;

  RefreshCanvas;

  if not boolHWJAM then
    RefreshPalette;

  DrawTexture;

end;

procedure TFormMain.AddNewTexture(Sender: TObject);

begin
  if importDialog.Execute then
  begin
    if (importDialog.filename <> '') and FileExists(importDialog.filename) then
    begin
      PushUndoState;
      if boolHWJAM then
      begin
        FHWJamFile.AddTexture(importDialog.filename);
        intSelectedTexture := FHWJamFile.FEntries.Count - 1;

        JamReGen;

        UpdatingFromCode := true;

        RefreshCanvas;
        DrawTexture;
        DrawTree;
        SelectTreeTex;

        UpdatingFromCode := false;
        JamModified(true);
      end
      else
      begin
        FJamFile.AddTexture(importDialog.filename);
        JamReGen;

        intSelectedTexture := FJamFile.FEntries.Count - 1;

        UpdatingFromCode := true;

        // RefreshCanvas;
        // RefreshPalette;
        // DrawTexture;
        DrawTree;
        SelectTreeTex;
        JamModified(true);

        UpdatingFromCode := false;
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

// procedure TFormMain.mainMenuCopyClick(Sender: TObject);
// var
// I: integer;
// jamList: TList<TjamEntry>;
// begin
//
// jamList := TList<TjamEntry>.Create;
//
// for I := 0 to SelectedTextureList.Count - 1 do
// begin
// jamList.Add(FJamFile.Entries[SelectedTextureList[I]]);
// end;
//
// CopyJAM(jamList);
//
// jamList.Clear;
// end;

procedure TFormMain.mainMenuCopyClick(Sender: TObject);
begin

  Copy;

end;

procedure TFormMain.mainMenuCutClick(Sender: TObject);
var
  i: integer;
begin

  Copy;

  if boolHWJAM then
  begin
    for i := SelectedTextureList.Count - 1 downto 0 do
    begin
      FHWJamFile.DeleteTexture(SelectedTextureList[i])
    end;
  end
  else
  begin
    for i := SelectedTextureList.Count - 1 downto 0 do
    begin
      FJamFile.DeleteTexture(SelectedTextureList[i])
    end;
  end;

  DeSelectTexture;
  SelectedTextureList.Clear;

  UpdatingFromCode := true;

  DrawTree;
  SelectTreeTex;
  JamModified(true);

  UpdatingFromCode := false;

end;

procedure TFormMain.mainMenuDeleteTextureClick(Sender: TObject);
begin
  DeleteTexture;
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
    UpdatingFromCode := true;
    DeSelectTexture();
    SelectTreeTex();
    UpdatingFromCode := false;
  end;
end;

procedure TFormMain.UpdateUIData(id: integer);
var
  i: integer;
begin
  if boolHWJAM then
  begin

    tex_ID.Enabled := true;
    tex_X.Enabled := true;
    tex_Y.Enabled := true;
    tex_width.Enabled := true;
    tex_height.Enabled := true;

    tex_ID.Value := FHWJamFile.FEntries[id].FInfo.jamID;
    tex_X.Value := FHWJamFile.FEntries[id].FInfo.X;
    tex_Y.Value := FHWJamFile.FEntries[id].FInfo.Y;
    tex_width.Value := FHWJamFile.FEntries[id].FInfo.Width;
    tex_height.Value := FHWJamFile.FEntries[id].FInfo.height;

    tex_flags.Enabled := true;
    for i := 0 to 15 do
      tex_flags.Checked[i] :=
        UnPackFlag(FHWJamFile.FEntries[id].FInfo.jamFlags, i);

  end
  else
  begin
    RefreshPalette;

    tex_ID.Enabled := true;
    tex_X.Enabled := true;
    tex_Y.Enabled := true;
    tex_width.Enabled := true;
    tex_height.Enabled := true;

    btnPal0.Enabled := true;
    btnPal1.Enabled := true;
    btnPal2.Enabled := true;
    btnPal3.Enabled := true;

    btnGenPal.Enabled := true;
    btnRemovePal.Enabled := true;

    tex_ID.Value := FJamFile.FEntries[id].FInfo.jamID;
    tex_X.Value := FJamFile.FEntries[id].FInfo.X;
    tex_Y.Value := FJamFile.FEntries[id].FInfo.Y;
    tex_width.Value := FJamFile.FEntries[id].FInfo.Width;
    tex_height.Value := FJamFile.FEntries[id].FInfo.height;

    chkBoxTrans.Checked := DetectTransCol(FJamFile.FEntries[id].FTexture);

    tex_flags.Enabled := true;

    texScale.Enabled := true;
    texScaleX.Enabled := true;
    texScaleY.Enabled := true;
    scaleFlags.Enabled := true;

    texScale.Value := FJamFile.FEntries[id].FInfo.scaleFactor;
    texScaleX.Value := FJamFile.FEntries[id].FInfo.scaleX;
    texScaleY.Value := FJamFile.FEntries[id].FInfo.scaleY;

    // texScale.Value := FJamFile.GetIDX0aScale(FJamFile.FEntries[id].FInfo.Idx0A);
    // texScaleX.Value := FJamFile.GetIDX08_X(FJamFile.FEntries[id].FInfo.Idx08);
    // texScaleY.Value := FJamFile.GetIDX08_Y(FJamFile.FEntries[id].FInfo.Idx08);
    for i := 0 to 15 do
      tex_flags.Checked[i] :=
        UnPackFlag(FJamFile.FEntries[id].FInfo.jamFlags, i);

    for i := 0 to 7 do
      scaleFlags.Checked[i] :=
        UnPackFlag(FJamFile.FEntries[id].FInfo.scaleFlag, i);

  end;
end;

procedure TFormMain.SelectTexture(id: integer; treeupdate: boolean);
begin

  if id = -1 then
    Exit;

  if treeupdate then
  begin
    SelectedTextureList.Clear;
    SelectedTreeNodes.Clear;
  end;

  if not SelectedTextureList.Contains(id) then
    SelectedTextureList.Add(id);

  boolTexSelected := true;

  intSelectedTexture := id;

  UpdatingFromCode := true;

  UpdateUIData(id);

  DrawTexture;
  RefreshCanvas;

  toolBar_DeleteTexture.Enabled := true;
  toolBar_ImportTexture.Enabled := true;
  toolBar_ExportTexture.Enabled := true;

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

  if SelectFromCanvas = true then
    SelectTreeTex;

  UpdatingFromCode := false;
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
    Root := JamTree.Items[1];
    N := Root.getFirstChild;
    while Assigned(N) do
    begin
      N.Collapse(false);
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
          N.Expand(false);
          N.MakeVisible;
          SelectedTreeNodes.Add(N);
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
begin
  DeleteTexture;

end;

procedure TFormMain.DeleteTexture;
var
  i: integer;
begin

  PushUndoState;

  SelectedTextureList.sort;

  if boolHWJAM then
  begin
    for i := SelectedTextureList.Count - 1 downto 0 do
      FHWJamFile.DeleteTexture(SelectedTextureList[i]);

  end
  else
  begin

    for i := SelectedTextureList.Count - 1 downto 0 do
      FJamFile.DeleteTexture(SelectedTextureList[i]);

    FJamFile.CalculateImagePtrs;

  end;
  DeSelectTexture();

  JamModified(true);

  RefreshPalette;
  RefreshCanvas;
  DrawTree();
end;

procedure TFormMain.DeSelectTexture();
var
  i: integer;

  N: TTreeNode;
  JNode: TJamTreeNode;

begin

  if SelectedTextureList.Count < 0 then
    Exit;

  intSelectedTexture := -1;

  SelectedTextureList.Clear;

  SelectedTreeNodes.Clear;

  RefreshCanvas;

  toolBar_DeleteTexture.Enabled := false;
  toolBar_ImportTexture.Enabled := false;
  toolBar_ExportTexture.Enabled := false;

  ImageEntry.Picture := nil;
  panel_PalPreview.Visible := false;

  UpdatingFromCode := true;

  tex_ID.Enabled := false;
  tex_X.Enabled := false;
  tex_Y.Enabled := false;
  tex_width.Enabled := false;
  tex_height.Enabled := false;

  btnPal0.Enabled := false;
  btnPal1.Enabled := false;
  btnPal2.Enabled := false;
  btnPal3.Enabled := false;

  btnGenPal.Enabled := false;
  btnRemovePal.Enabled := false;

  tex_ID.Value := 0;
  tex_X.Value := 0;
  tex_Y.Value := 0;
  tex_width.Value := 0;
  tex_height.Value := 0;

  for i := 0 to 15 do
    tex_flags.Checked[i] := false;

  texScale.Value := 0;
  texScaleX.Value := 0;
  texScaleY.Value := 0;

  for i := 0 to 7 do
    scaleFlags.Checked[i] := false;

  texScale.Enabled := false;
  texScaleX.Enabled := false;
  texScaleY.Enabled := false;
  scaleFlags.Enabled := false;

  tex_flags.Enabled := false;

  UpdatingFromCode := false;
  boolTexSelected := false;

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

  JamTree.Items.BeginUpdate;
  // JamTree.Selected := nil;

  // Collapse all texture nodes first (optional)
  var
  Root := JamTree.Items[1];
  N := Root.getFirstChild;
  while Assigned(N) do
  begin
    N.Collapse(false);
    N := N.getNextSibling;
  end;

  JamTree.Items.EndUpdate;

end;

procedure TFormMain.UpdateJamData(id: integer);
var
  i: integer;
  changedFlags: array [0 .. 15] of boolean;
  changedScaleFlags: array [0 .. 7] of boolean;
  tempflags: byte;

begin

  if (id < 0) or (boolHWJAM and (id >= FHWJamFile.Entries.Count)) or
    (not boolHWJAM and (id >= FJamFile.Entries.Count)) then
    Exit;

  if boolUndo = true then
  PushUndoState;

  if boolHWJAM then
  begin
    with FHWJamFile.FEntries[id] do
    begin
      timer_JamRedrawPals.Interval := 90;
      tempDimensions.X := FInfo.X;
      tempDimensions.Y := FInfo.Y;
      tempDimensions.Width := FInfo.Width;
      tempDimensions.height := FInfo.height;

      if boolIDChange then
        FInfo.jamID := tex_ID.Value;
      if boolXChange then
        FInfo.X := tex_X.Value;
      if boolYChange then
        FInfo.Y := tex_Y.Value;
      if boolWidthChange then
        FInfo.Width := tex_width.Value;
      if boolHeightChange then
        FInfo.height := tex_height.Value;

      if boolFlagChange then
      begin
        for i := 0 to 15 do
          changedFlags[i] := tex_flags.Checked[i] <>
            UnPackFlag(FInfo.jamFlags, i);

        for i := 0 to 15 do
          if changedFlags[i] then
            if tex_flags.Checked[i] then
              FInfo.jamFlags := FInfo.jamFlags or (1 shl i)
            else
              FInfo.jamFlags := FInfo.jamFlags and not(1 shl i);
      end;

    end;
  end
  else
  begin
    with FJamFile.FEntries[id] do
    begin
      timer_JamRedrawPals.Interval := 250;
      tempDimensions.X := FInfo.X;
      tempDimensions.Y := FInfo.Y;
      tempDimensions.Width := FInfo.Width;
      tempDimensions.height := FInfo.height;

      if boolIDChange then
        FInfo.jamID := tex_ID.Value;

      if boolXChange then

        FInfo.X := tex_X.Value;

      if boolYChange then

        FInfo.Y := tex_Y.Value;

      if boolWidthChange then

        FInfo.Width := tex_width.Value;

      if boolHeightChange then
        FInfo.height := tex_height.Value;

      if boolFlagChange then
      begin
        for i := 0 to 15 do
          changedFlags[i] := tex_flags.Checked[i] <>
            UnPackFlag(FInfo.jamFlags, i);

        for i := 0 to 15 do
          if changedFlags[i] then
            if tex_flags.Checked[i] then
              FInfo.jamFlags := FInfo.jamFlags or (1 shl i)
            else
              FInfo.jamFlags := FInfo.jamFlags and not(1 shl i);
      end;

      if boolScaleXChange then
        FInfo.scaleX := texScaleX.Value;

      if boolScaleYChange then
        FInfo.scaleY := texScaleY.Value;

      if boolScaleChange then
        FInfo.scaleFactor := texScale.Value;
      // if boolScaleXChange then
      // FInfo.Idx08 := FJamFile.SetIDX08(texScaleX.Value, fjamfile.GetIDX08_Y(finfo.Idx08));
      //
      // if boolScaleYChange then
      // FInfo.Idx08 := FJamFile.SetIDX08(fjamfile.GetIDX08_X(finfo.Idx08), texScaleY.Value);
      //
      // if boolScaleChange then
      // FInfo.Idx0A := FJamFIle.SetIDX0a(fjamfile.GetIDX0aFlags(info.idx0a), texScale.Value);

      if boolScaleFlagsChange then
      begin

        for i := 0 to 7 do
        begin
          tempflags := Info.scaleFlag;
          changedScaleFlags[i] := scaleFlags.Checked[i] <>
            UnPackFlag(tempflags, i);
        end;

        for i := 0 to 7 do
          if changedScaleFlags[i] then
            if scaleFlags.Checked[i] then
            begin
              tempflags := Info.scaleFlag;
              tempflags := tempflags or (1 shl i);
              FInfo.scaleFlag := tempflags;
            end
            else
            begin
              tempflags := Info.scaleFlag;
              tempflags := tempflags and not(1 shl i);
              FInfo.scaleFlag := tempflags;
            end;

      end;

    end;
  end;

  timer_JamRedrawPals.Enabled := false;
  timer_redrawTree.Enabled := false;

  timer_redrawTree.Enabled := true;

  timer_JamRedrawPals.Enabled := true;

  RefreshCanvas;
  JamModified(true);

  boolXChange := false;
  boolYChange := false;
  boolWidthChange := false;
  boolHeightChange := false;

end;

procedure TFormMain.ClearPaletteImg();
begin
  panel_PalPreview.Visible := false;
end;

procedure TFormMain.UpdateCaption();
var
  modText: string;
begin
  Caption := Application.Title;

  if boolJamLoaded then
  begin
    if boolJamModified then
      modText := '*'
    else
      modText := '';

    if boolHWJAM then
      Caption := Format('%s%s- %s', [FHWJamFile.JamFullPath, modText,
        Application.Title])
    else
      Caption := Format('%s%s- %s', [FJamFile.JamFullPath, modText,
        Application.Title])
  end;
end;

function TFormMain.JamSanityCheck: boolean;
var
  JamRects: TArray<TJamRect>;
  i: integer;
begin

  if boolHWJAM then
  begin

    FHWJamFile.BuildRect_HW(FHWJamFile, JamRects);
    Result := DetectRectsOverlap(JamRects);
    boolJamIssues := Result;

  end
  else
  begin

    FJamFile.BuildRect_SW(FJamFile, JamRects);

    Result := DetectRectsOverlap(JamRects);
    boolJamIssues := Result;

  end;

end;

procedure TFormMain.JamSanityCheckInform(onSave: boolean);
var
  errorMessage: string;
  X: integer;
begin

  errorMessage := 'Jam Sanity Check: All clear';

  if onSave then
  begin
    errorMessage := 'The following textures are interecting: ' + sLineBreak +
      sLineBreak;

    for X := 0 to IntersectList.Count - 1 do
      errorMessage := errorMessage + IntToStr(IntersectList[X].jamID) + ' ' +
        IntToStr(IntersectList[X].intersectID) + sLineBreak;

    ShowMessage(errorMessage);
  end
  else
  begin
    if boolJamIssues then
    begin
      errorMessage := 'Jam Sanity Check: Textures';
      for X := 0 to IntersectList.Count - 1 do
        errorMessage := Format('%s %d, ',
          [errorMessage, IntersectList[X].jamID]);

      errorMessage := errorMessage + ' have intersections';
    end;
  end;

  StatusBar1.Panels[1].Text := errorMessage;
end;

procedure TFormMain.JamModified(modified: boolean);
begin
  boolJamModified := modified;
  if boolJamLoaded then
    UpdateCaption;
end;

end.
