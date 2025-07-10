unit JamBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Threading, system.IOUtils, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.FileCtrl, System.StrUtils, ShellCtrls,
  VirtualTrees, VirtualTrees.DrawTree, VirtualTrees.Types,
  VirtualTrees.BaseTree, EasyListview, GeneralHelpers, System.ImageList,
  Vcl.ImgList, MPCommonObjects, MPCommonUtilities, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,

  JamGeneral, JamHW, JamSW;

type
  // This data record contains all necessary information about a particular file system object.
  // This can either be a folder (virtual or real) or an image file.
  PShellObjectData = ^TShellObjectData;

  TShellObjectData = record
    FullPath, Display: UnicodeString;
    Attributes: Cardinal;
    OpenIndex, CloseIndex: Integer; // image indices into the system image list
    Image: TBitmap;
    Properties: UnicodeString; // some image properties, preformatted
  end;

type

  TJamBrowseNode = class
    FileName: string;
    Thumb: TBitmap;
    Orig: TBitmap;
    Height,Width,numTexs: integer;
    jamType: string;
    destructor Destroy; override;
  end;

  PJamBrowseNode = ^TJamBrowseNode;

type
  TJamBrowser = class(TForm)
    PanelTop: TPanel;
    SliderThumbSize: TTrackBar;
    PanelFolders: TPanel;
    SplitterLeft: TSplitter;
    Splitter1: TSplitter;
    PanelPreview: TPanel;
    ImagePreview: TImage;
    SplitterFilmstrip: TSplitter;
    directoryTree: TVirtualDrawTree;
    SystemImages: TImageList;
    jamListView: TEasyListview;
    StatusBar1: TStatusBar;
    ProgressBar: TProgressBar;
    lblFilename: TLabel;
    lblJamType: TLabel;
    lblDimensions: TLabel;
    lblTexs: TLabel;
    jamLoading: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SliderThumbSizeChange(Sender: TObject);

    procedure directoryTreeCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure directoryTreeDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure directoryTreeGetNodeWidth(Sender: TBaseVirtualTree;
      HintCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      var NodeWidth: TDimension);
    procedure directoryTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure directoryTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure directoryTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure directoryTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: TImageIndex);
    procedure FormDestroy(Sender: TObject);
    procedure directoryTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure jamListViewItemFreeing(Sender: TCustomEasyListview;
      Item: TEasyItem);
    procedure jamListViewItemImageDrawIsCustom(Sender: TCustomEasyListview;
      Item: TEasyItem; Column: TEasyColumn; var IsCustom: Boolean);
    procedure jamListViewItemImageDraw(Sender: TCustomEasyListview;
      Item: TEasyItem; Column: TEasyColumn; ACanvas: TCanvas;
      const RectArray: TEasyRectArrayObject; AlphaBlender: TEasyAlphaBlender);
    procedure jamListViewItemSelectionChanged(Sender: TCustomEasyListview;
      Item: TEasyItem);
    procedure jamListViewItemDblClick(Sender: TCustomEasyListview;
      Button: TCommonMouseButton; MousePos: TPoint; HitInfo: TEasyHitInfoItem);

  private
    FileList: TStringList;
    ThumbSize: Integer;
    FThumbSize: Integer;
    FExtensionsInitialized: Boolean;
    FExtensionList: TStringList;
    FDriveStrings: string;

    FCancelToken: Integer;
    FPendingThumbnails: Integer;
    FLoadedThumbnails: Integer;

    procedure LoadImagesFromFolderAsync(const Folder: string);
    procedure LoadImagesWorker(const Folder: string);
    procedure RedrawThumbnails;
    procedure ClearPreview;
    function CanDisplay(const Name: String): Boolean;
    function GetDriveString(Index: Integer): string;
    function ReadAttributes(const Name: UnicodeString): Cardinal;
    function GenerateThumbnail(srcBmp: TBitmap; maxSize: Integer): TBitmap;

    procedure QueueThumbnailTask(const FilePath: string);

  public
    gotSelection: Boolean;
  end;

const
  SupportedExtensions: array [0 .. 1] of string = ('.jam','.jip');

var
  JamBrowserFrm: TJamBrowser;

implementation

{$R *.dfm}

uses
  System.Math, ShellAPI, Mask, ShlObj, ActiveX, VirtualTrees.Utils, MainForm;

{ TJamBrowseNode }

destructor TJamBrowseNode.Destroy;
begin
  FreeAndNil(Thumb);
  FreeAndNil(Orig);
  inherited;
end;

{ TJamBrowser }

procedure DrawBitmapInRectCentered(const Canvas: TCanvas;
  const SourceBmp: TBitmap; const TargetRect: TRect);
var
  SrcW, SrcH: Integer;
  RectW, RectH: Integer;
  Scale, ScaleW, ScaleH: Double;
  DrawW, DrawH: Integer;
  OffsetX, OffsetY: Integer;
begin
  SrcW := SourceBmp.Width;
  SrcH := SourceBmp.Height;
  RectW := TargetRect.Right - TargetRect.Left;
  RectH := TargetRect.Bottom - TargetRect.Top;

  if (SrcW <= 0) or (SrcH <= 0) or (RectW <= 0) or (RectH <= 0) then
    Exit;

  // Compute scaling factor to fit inside rect
  ScaleW := RectW / SrcW;
  ScaleH := RectH / SrcH;
  Scale := Min(ScaleW, ScaleH);

  // Compute scaled size
  DrawW := Round(SrcW * Scale);
  DrawH := Round(SrcH * Scale);

  // Compute offsets to center the image
  OffsetX := TargetRect.Left + (RectW - DrawW) div 2;
  OffsetY := TargetRect.Top + (RectH - DrawH) div 2;

  // Draw stretched and centered
  Canvas.StretchDraw(Rect(OffsetX, OffsetY, OffsetX + DrawW, OffsetY + DrawH),
    SourceBmp);
end;

// ----------------- utility functions ----------------------------------------------------------------------------------

function IncludeTrailingBackslash(const S: string): string;

begin
  if not IsPathDelimiter(S, Length(S)) then
    Result := S + '\'
  else
    Result := S;
end;

// ----------------------------------------------------------------------------------------------------------------------

function ExcludeTrailingBackslash(const S: string): string;

begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result) - 1);
end;

// ----------------------------------------------------------------------------------------------------------------------

function HasChildren(const Folder: string): Boolean;

// Determines whether folder contains other file objects.

var
  SR: TSearchRec;

begin
  Result := FindFirst(IncludeTrailingBackslash(Folder) + '*.*',
    faReadOnly or faHidden or faSysFile or faArchive, SR) = 0;
  if Result then
    FindClose(SR);
end;

// ----------------------------------------------------------------------------------------------------------------------

function GetIconIndex(Name: string; Flags: Cardinal): Integer;

// Returns the index of the system icon for the given file object.

var
  SFI: TSHFileInfo;

begin
  if SHGetFileInfo(PChar(Name), 0, SFI, SizeOf(TSHFileInfo), Flags) = 0 then
    Result := -1
  else
    Result := SFI.iIcon;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure GetOpenAndClosedIcons(Name: string; var Open, Closed: Integer);

begin
  Closed := GetIconIndex(Name, SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  Open := GetIconIndex(Name, SHGFI_SYSICONINDEX or SHGFI_SMALLICON or
    SHGFI_OPENICON);
end;




// ----------------------------------------------------------------------------------------------------------------------

function TJamBrowser.CanDisplay(const Name: string): Boolean;

// Determines whether the given file is one we can display in the image tree.

var
  Ext: string;
  I: Integer;

begin
  if not FExtensionsInitialized then
  begin
    FExtensionsInitialized := True;
    FExtensionList := TStringList.Create;
{$IFDEF GraphicEx}
    FileFormatList.GetExtensionList(FExtensionList);
    for I := 0 to FExtensionList.Count - 1 do
      FExtensionList[I] := '.' + FExtensionList[I];
{$ELSE}
    // GraphicEx is not used so add some default extensions
    with FExtensionList do
    begin
      // Add('.bmp');
      // Add('.ico');
      // Add('.jpg');
      // Add('.jpeg');
      // Add('.wmf');
      // Add('.emf');
    end;
{$ENDIF}
    FExtensionList.Sort;
  end;

  Ext := ExtractFileExt(Name);
  Result := FExtensionList.Find(Ext, I);
end;

// ----------------------------------------------------------------------------------------------------------------------

function TJamBrowser.GetDriveString(Index: Integer): string;

// Helper method to extract a sub string (given by Index) from FDriveStrings.

var
  Head, Tail: PChar;

begin
  Head := PChar(FDriveStrings);
  Result := '';
  repeat
    Tail := Head;
    while Tail^ <> #0 do
      Inc(Tail);
    if Index = 0 then
    begin
      SetString(Result, Head, Tail - Head);
      Break;
    end;
    Dec(Index);
    Head := Tail + 1;
  until Head^ = #0;
end;



procedure TJamBrowser.jamListViewItemDblClick(Sender: TCustomEasyListview;
  Button: TCommonMouseButton; MousePos: TPoint; HitInfo: TEasyHitInfoItem);
begin

FormMain.LoadJam(TJamBrowseNode(hitinfo.Item.data).FileName);

end;

procedure TJamBrowser.jamListViewItemFreeing(Sender: TCustomEasyListview;
  Item: TEasyItem);
begin
if Assigned(Item.Data) then
  begin
    TJamBrowseNode(Item.Data).Free;
    Item.Data := nil;
  end;
  ClearPreview;
end;

procedure TJamBrowser.jamListViewItemImageDraw(Sender: TCustomEasyListview;
  Item: TEasyItem; Column: TEasyColumn; ACanvas: TCanvas;
  const RectArray: TEasyRectArrayObject; AlphaBlender: TEasyAlphaBlender);
var
  img: TBitmap;
begin
  img := TJamBrowseNode(Item.data).Thumb;
  AlphaBlender.Blend(Sender, Item, ACanvas, RectArray.IconRect, img);
end;

procedure TJamBrowser.jamListViewItemImageDrawIsCustom
  (Sender: TCustomEasyListview; Item: TEasyItem; Column: TEasyColumn;
  var IsCustom: Boolean);
begin
  IsCustom := True;
end;

procedure TJamBrowser.jamListViewItemSelectionChanged(
  Sender: TCustomEasyListview; Item: TEasyItem);
begin
  ImagePreview.Picture.Bitmap := TJamBrowseNode(Item.data).Orig;
  lblJamType.Visible := true;
  lblFilename.Visible := true;
  lblDimensions.Visible := true;
  lblTexs.visible := true;

  lblJamType.Caption := 'Jam Type: ' + TJamBrowseNode(Item.data).jamType;
  lblFilename.Caption := 'File Name: ' + ExtractFileName(TJamBrowseNode(Item.data).FileName);
  lblDimensions.Caption := 'Dimensions: ' + inttostr(TJamBrowseNode(Item.data).Width) + ' x ' + inttostr(TJamBrowseNode(Item.data).Height);
  lblTexs.Caption := 'Number of Textures: ' + inttostr(TJamBrowseNode(Item.data).numTexs);
end;

// ----------------------------------------------------------------------------------------------------------------------

function TJamBrowser.ReadAttributes(const Name: UnicodeString): Cardinal;

// Determines the attributes of the given shell object (file, folder).

const
  SFGAO_CONTENTSMASK = $F0000000; // This value is wrongly defined in ShlObj.

var
  Desktop: IShellFolder;
  Eaten: Cardinal;
  PIDL: PItemIDList;
  Malloc: IMalloc;

begin
  // Get the root folder of the shell name space.
  SHGetDesktopFolder(Desktop);
  // While parsing the name also the shell object's attributes are determined.
  // These is what we are really interested in.
  Result := SFGAO_DISPLAYATTRMASK or SFGAO_CONTENTSMASK or SFGAO_COMPRESSED;
  Desktop.ParseDisplayName(0, nil, PWideChar(Name), Eaten, PIDL, Result);
  // Don't forget to free the returned PIDL. The shell folder is released automatically.
  SHGetMalloc(Malloc);
  Malloc.Free(PIDL);
end;

function TJamBrowser.GenerateThumbnail(srcBmp: TBitmap;
  maxSize: Integer): TBitmap;
var
  Ratio, ScaleW, ScaleH: Double;
  ThumbW, ThumbH: Integer;
  TempBmp, ResizedBmp: TBitmap;
  OffsetX, OffsetY: Integer;
  PaddingFactor: Double;
begin
  Result := nil;

//  if (srcBmp.Width < 12) or (srcBmp.Height < 12) then
//  begin
//    Result := srcBmp;
//    Exit(Result);
//  end;

  PaddingFactor := 0.95;

  TempBmp := TBitmap.Create;
  try
    TempBmp.Assign(srcBmp);

    if not (srcBmp.Width < 12) or (srcBmp.Height < 12) then
    begin
 {$R-}
    // Scale to fit within padding-adjusted square
    ScaleW := (maxSize * PaddingFactor) / TempBmp.Width;
    ScaleH := (maxSize * PaddingFactor) / TempBmp.Height;
    Ratio := Min(ScaleW, ScaleH);

    ThumbW := Round(TempBmp.Width * Ratio);
    ThumbH := Round(TempBmp.Height * Ratio);

    // Resize with high-quality routine
    ResizedBmp := StretchF(TempBmp, ThumbW, ThumbH);
     {$R+}
     end
     else
     resizedBMP := srcBMP;

    // Create result bitmap with background color from JamListView.Color
    Result := TBitmap.Create;
    Result.PixelFormat := pf24bit;
    Result.SetSize(maxSize, maxSize);
    Result.Canvas.Brush.Color := jamListView.Color;
    Result.Canvas.FillRect(Rect(0, 0, maxSize, maxSize));

    // Center resized thumbnail on canvas
    OffsetX := (maxSize - ThumbW) div 2;
    OffsetY := (maxSize - ThumbH) div 2;

    Result.Canvas.Draw(OffsetX, OffsetY, ResizedBmp);
//    if not (srcBmp.Width < 12) or (srcBmp.Height < 12) then
  ResizedBmp.Free;
  finally
    TempBmp.Free;
  end;
end;

procedure TJamBrowser.FormCreate(Sender: TObject);
var
  SFI: TSHFileInfo;
  I, DriveCount: Integer;
  Len: Integer;
  DriveMap, Mask: Cardinal;

begin

  FileList := TStringList.Create;
  ThumbSize := 128;
  SliderThumbSize.Min := 128;
  SliderThumbSize.Max := 512;
  SliderThumbSize.Position := ThumbSize;

  jamListView.CellSizes.Thumbnail.Width := ThumbSize + 40;
  jamListView.CellSizes.Thumbnail.Height := ThumbSize + 40;

  // Initial layout
  RedrawThumbnails;
  directoryTree.NodeDataSize := SizeOf(TShellObjectData);

  // Fill root level of image tree. Determine which drives are mapped.
  DriveCount := 0;
  DriveMap := GetLogicalDrives;
  Mask := 1;
  for I := 0 to 25 do
  begin
    if (DriveMap and Mask) <> 0 then
      Inc(DriveCount);
    Mask := Mask shl 1;
  end;
  // Determine drive strings which are used in the initialization process.
  Len := GetLogicalDriveStrings(0, nil);
  SetLength(FDriveStrings, Len);
  GetLogicalDriveStrings(Len, PChar(FDriveStrings));
  directoryTree.RootNodeCount := DriveCount;

  SystemImages.Handle := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SystemImages.ShareImages := True;

  FThumbSize := 200;
end;

procedure TJamBrowser.FormDestroy(Sender: TObject);
begin
  jamlistview.items.Clear;
  FreeAndNil(FExtensionList);
end;

procedure TJamBrowser.SliderThumbSizeChange(Sender: TObject);
begin
  ThumbSize := SliderThumbSize.Position;
  jamListView.CellSizes.Thumbnail.SetSize(ThumbSize, ThumbSize + 40);
  RedrawThumbnails;

end;

procedure TJamBrowser.LoadImagesFromFolderAsync(const Folder: string);
begin

  TTask.Run(
    procedure
    begin
      LoadImagesWorker(Folder);
    end);
end;

procedure TJamBrowser.QueueThumbnailTask(const FilePath: string);
var
  JamFile: TJamFile;
  HWJamFile: THWJamFile;
  Thumb: TBitmap;
  JamThumb: TJamBrowseNode;
  Item: TEasyItem;
  jamType: string;
  height,width,numTexs : integer;
begin
  Thumb := nil;
  JamThumb := nil;
  try
    // 1) Load & draw into Thumb, freeing file-object immediately afterward
    if isHWJAM(FilePath) then
    begin
      HWJamFile := THWJamFile.Create;
      try
        HWJamFile.LoadFromFile(FilePath);
        Thumb := HWJamFile.DrawCanvas(false);
        jamType := 'Hardware JAM File';
        height := thumb.height;
        width := thumb.width;
        numTexs := HWjamfile.FEntries.Count;
      finally
        HWJamFile.Free;
      end;
    end
    else
    begin
      JamFile := TJamFile.Create;
      try
        JamFile.LoadFromFile(FilePath, true);
        Thumb := JamFile.DrawFullJam(false);
        jamType := 'Software JAM File';
        if TPath.GetExtension(filepath) = 'jip' then
           jamType := 'Software JIP File';
        height := thumb.height;
        width := thumb.width;
        numTexs := JamFile.FEntries.Count;
      finally
        JamFile.Free;
      end;
    end;

    // 2) Build the browse-node and add it to the listview
    //    If anything goes wrong here, free both Thumb and JamThumb.
    try
      JamThumb := TJamBrowseNode.Create;
      JamThumb.FileName := FilePath;
      JamThumb.Orig     := Thumb;  // store original image
      JamThumb.Thumb    := GenerateThumbnail(Thumb, ThumbSize);
      JamThumb.Height := height;
      JamThumb.Width := Width;
      JamThumb.numTexs := numTexs;
      JamThumb.jamType := JamType;

      Item := jamListView.Items.Add(JamThumb);
      Item.Caption := ExtractFileName(JamThumb.FileName);

      // 3) Update progress
      Inc(FLoadedThumbnails);
       jamloading.caption := 'Loading JAM ' + intToStr(FLoadedThumbnails) + ' of ' + intToStr(FPendingThumbnails);
      ProgressBar.Position := FLoadedThumbnails;

    except
      // cleanup on error
   if Assigned(Thumb) then
      Thumb.Free;
    if Assigned(JamThumb) then
      JamThumb.Free;
      raise;
    end;

  finally
    // nothing else to free here: file objects already freed, thumbs transferred to node
  end;
end;


procedure TJamBrowser.LoadImagesWorker(const Folder: string);
var
  SR: TSearchRec;
  Ext: string;
  I: Integer;
  NewList: TStringList;
  LocalToken: Integer;
begin
  LocalToken := FCancelToken;
  NewList := TStringList.Create;

  if FindFirst(Folder + '\*.*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory = 0) then
      begin
        Ext := LowerCase(ExtractFileExt(SR.Name));
        for I := Low(SupportedExtensions) to High(SupportedExtensions) do
          if Ext = SupportedExtensions[I] then
          begin
          if SR.name = 'shill.jam' then exit;
            NewList.Add(Folder + '\' + SR.Name);
            Break;
          end;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  jamListView.Items.clear;
  jamListView.BeginUpdate;
  TThread.Queue(nil,
    procedure
    var
      I: Integer;
    begin
      FileList.Assign(NewList);

      ProgressBar.Max := FileList.Count;
      ProgressBar.Position := 0;
      ProgressBar.Visible := True;

      jamLoading.Visible := true;


      FPendingThumbnails := FileList.Count;
      FLoadedThumbnails := 0;

      for I := 0 to FileList.Count - 1 do
         QueueThumbnailTask(FileList[I]);

      ProgressBar.Visible := false; // move this inside synchronized block
      jamLoading.Visible := false;
      NewList.Free;
    end);
  jamListView.EndUpdate;

end;


procedure TJamBrowser.RedrawThumbnails;
var
  I: Integer;
  Node: TJamBrowseNode;
  NewThumb: TBitmap;
begin
  for I := 0 to jamListView.Items.Count - 1 do
  begin
    Node := TJamBrowseNode(jamListView.Items[I].Data);
    if not Assigned(Node) then
      Continue;

    NewThumb := GenerateThumbnail(Node.Orig, ThumbSize);
    FreeAndNil(Node.Thumb);
    Node.Thumb := NewThumb;

    jamListView.Items[I].Invalidate(true);  // Redraw only that item
  end;

  // Optional: if using per-item invalidation above, you don't need this
  // jamListView.Invalidate;
end;


procedure TJamBrowser.directoryTreeChange(Sender: TBaseVirtualTree;
Node: PVirtualNode);
VAR
  data: PShellObjectData;
begin
  imagepreview.picture := nil;
  data := Sender.GetNodeData(Node);
  if data <> nil then
    LoadImagesFromFolderAsync(data.FullPath);
end;

procedure TJamBrowser.directoryTreeCompareNodes(Sender: TBaseVirtualTree;
Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
// The node comparison routine is the heart of the tree sort. Here we have to tell the caller which node we consider
// being "larger" or "smaller".

var
  Data1, Data2: PShellObjectData;

begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  // Folder are always before files. Check if *both* are folders or *both* are non-folders, but not different.
  if ((Data1.Attributes xor Data2.Attributes) and SFGAO_FOLDER) <> 0 then
  begin
    // One of both is a folder the other is a file.
    if (Data1.Attributes and SFGAO_FOLDER) <> 0 then
      Result := -1
    else
      Result := 1;
  end
  else
    // Both are of same type (folder or file). Just compare captions.
    // Note that we use ANSI comparison, while the strings are Unicode. Since this will implicitely convert the captions
    // to ANSI for comparation it might happen that the sort order is wrong for names which contain text in a language
    // other than the current OS language. A full blown Unicode comparison is beyond the scope of this demo.
    Result := CompareText(Data1.FullPath, Data2.FullPath);
end;

procedure TJamBrowser.directoryTreeDrawNode(Sender: TBaseVirtualTree;
const PaintInfo: TVTPaintInfo);
// This is the main paint routine for a node in a draw tree. There is nothing special here. Demonstrating the
// specific features of a draw tree (compared to the string tree) is a bit difficult, since the only difference is
// that the draw tree does not handle node content (captions in the case of the string tree).

var
  data: PShellObjectData;
  X: Integer;
  S: UnicodeString;
  R: TRect;

begin
  with Sender as TVirtualDrawTree, PaintInfo do
  begin
    data := Sender.GetNodeData(Node);
    if (Column = FocusedColumn) and (Selected[Node]) then
      Canvas.Font.Color := clHighlightText
    else if (data.Attributes and SFGAO_COMPRESSED) <> 0 then
      Canvas.Font.Color := clBlue
    else
      Canvas.Font.Color := clWindowText;

    SetBKMode(Canvas.Handle, TRANSPARENT);

    R := ContentRect;
    InflateRect(R, -TextMargin, 0);
    Dec(R.Right);
    Dec(R.Bottom);
    S := '';
    case Column of
      0, 2:
        begin
          if Column = 2 then
          begin
            if Assigned(data.Image) then
              S := data.Properties;
          end
          else
            S := data.Display;
          if Length(S) > 0 then
          begin
            with R do
            begin
              if (NodeWidth - 2 * Margin) > (Right - Left) then
                S := ShortenString(Canvas.Handle, S, Right - Left);
            end;
            DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R,
              DT_TOP or DT_LEFT or DT_VCENTER or DT_SINGLELINE);
          end;
        end;
      1:
        begin
          if Assigned(data.Image) then
          begin
            X := ContentRect.Left + (directoryTree.Header.Columns[1].Width -
              data.Image.Width - Margin) div 2;
            BitBlt(Canvas.Handle, X, ContentRect.Top + 2, data.Image.Width,
              data.Image.Height, data.Image.Canvas.Handle, 0, 0, SRCCOPY);
          end;
        end;
    end;
  end;
end;

procedure TJamBrowser.directoryTreeFreeNode(Sender: TBaseVirtualTree;
Node: PVirtualNode);
var
  data: PShellObjectData;

begin
  data := Sender.GetNodeData(Node);
  data.Image.Free;
  Finalize(data^); // Clear string data.
end;

procedure TJamBrowser.directoryTreeGetImageIndex(Sender: TBaseVirtualTree;
Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
var Ghosted: Boolean; var Index: TImageIndex);
var
  data: PShellObjectData;

begin
  if Column = 0 then
  begin
    data := Sender.GetNodeData(Node);
    case Kind of
      ikNormal, ikSelected:
        begin
          if Sender.Expanded[Node] then
            Index := data.OpenIndex
          else
            Index := data.CloseIndex;
        end;
      ikOverlay:
        if (data.Attributes and SFGAO_SHARE) <> 0 then
          Index := 0
        else if (data.Attributes and SFGAO_LINK) <> 0 then
          Index := 1;
    end;
  end;
end;

procedure TJamBrowser.directoryTreeGetNodeWidth(Sender: TBaseVirtualTree;
HintCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
var NodeWidth: TDimension);

// Since the draw tree does not know what is in a cell, we have to return the width of the content (not the entire
// cell width, this could be determined by the column width).

var
  data: PShellObjectData;
  AMargin: Integer;

begin
  with Sender as TVirtualDrawTree do
    AMargin := TextMargin;

  begin
    data := Sender.GetNodeData(Node);
    case Column of
      0:
        begin
          if Sender.NodeParent[Node] = nil then
            NodeWidth := Canvas.TextWidth(data.FullPath) + 2 * AMargin
          else
            NodeWidth := Canvas.TextWidth(ExtractFileName(data.FullPath)) + 2
              * AMargin;
        end;
      1:
        begin
          if Assigned(data.Image) then
            NodeWidth := data.Image.Width;
        end;
      2:
        NodeWidth := Canvas.TextWidth(data.Properties) + 2 * AMargin;
    end;
  end;
end;

procedure TJamBrowser.directoryTreeInitChildren(Sender: TBaseVirtualTree;
Node: PVirtualNode; var ChildCount: Cardinal);
// Called just before a node with children (only folder nodes can have children) is expanded.

var
  data, ChildData: PShellObjectData;
  SR: TSearchRec;
  ChildNode: PVirtualNode;
  NewName: String;

begin
  data := Sender.GetNodeData(Node);
  if FindFirst(IncludeTrailingBackslash(data.FullPath) + '*.*', faAnyFile, SR) = 0
  then
  begin
    Screen.Cursor := crHourGlass;
    Sender.BeginUpdate;
    try
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          NewName := IncludeTrailingBackslash(data.FullPath) + SR.Name;
          if (SR.Attr and faDirectory <> 0) or CanDisplay(NewName) then
          begin
            ChildNode := Sender.AddChild(Node);
            ChildData := Sender.GetNodeData(ChildNode);
            ChildData.FullPath := NewName;
            ChildData.Attributes := ReadAttributes(NewName);
            if (ChildData.Attributes and SFGAO_FOLDER) = 0 then
              ChildData.Properties := Format('%n KB, ', [SR.Size / 1024]);
            GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex,
              ChildData.CloseIndex);

            Sender.ValidateNode(Node, false);
            Inc(ChildCount);
          end;
        end;
      until FindNext(SR) <> 0;

      // finally sort node
      Sender.Sort(Node, 0, TVirtualStringTree(Sender)
        .Header.SortDirection, false);
    finally
      Sender.EndUpdate;
      FindClose(SR);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TJamBrowser.directoryTreeInitNode(Sender: TBaseVirtualTree;
ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data: PShellObjectData;
  Picture: TPicture;

begin
  data := Sender.GetNodeData(Node);
  if ParentNode = nil then
  begin
    // top level node, initialize first enumeration
    data.FullPath := GetDriveString(Node.Index);
    data.Display := data.FullPath;
    GetOpenAndClosedIcons(data.FullPath, data.OpenIndex, data.CloseIndex);
  end
  else
  begin
    data.Display := ExtractFileName(ExcludeTrailingBackslash(data.FullPath));
    if (data.Attributes and SFGAO_FOLDER) = 0 then
    begin
      Picture := TPicture.Create;
      try
        try
          data.Image := TBitmap.Create;
          Picture.LoadFromFile(data.FullPath);
          if not(Picture.Graphic is TBitmap) then
          begin
            // Some extra steps needed to keep non TBitmap descentants alive when
            // scaling. This is needed because when accessing Picture.Bitmap all
            // non-TBitmap content will simply be erased (definitly the wrong
            // action, but we can't do anything to prevent this). Hence we
            // must explicitly draw the graphic to a bitmap.
            with data.Image do
            begin
              Width := Picture.Width;
              Height := Picture.Height;
              Canvas.Draw(0, 0, Picture.Graphic);
            end;
            Picture.Bitmap.Assign(data.Image);
          end;
          // RescaleImage(Picture.Bitmap, Data.Image);

          // Collect some additional image properties.
          data.Properties := data.Properties + Format('%d x %d pixels',
            [Picture.Width, Picture.Height]);
          case Picture.Bitmap.PixelFormat of
            pf1bit:
              data.Properties := data.Properties + ', 2 colors';
            pf4bit:
              data.Properties := data.Properties + ', 16 colors';
            pf8bit:
              data.Properties := data.Properties + ', 256 colors';
            pf15bit:
              data.Properties := data.Properties + ', 32K colors';
            pf16bit:
              data.Properties := data.Properties + ', 64K colors';
            pf24bit:
              data.Properties := data.Properties + ', 16M colors';
            pf32bit:
              data.Properties := data.Properties + ', 16M+ colors';
          end;
          if data.Image.Height + 4 > TVirtualDrawTree(Sender).DefaultNodeHeight
          then
            Sender.NodeHeight[Node] := data.Image.Height + 4;
        except
          data.Image.Free;
          data.Image := nil;
        end;
      finally
        Picture.Free;
      end; // try..finally
    end; // if
  end; // else
  data.Attributes := ReadAttributes(data.FullPath);
  if ((data.Attributes and SFGAO_HASSUBFOLDER) <> 0) or
    (((data.Attributes and SFGAO_FOLDER) <> 0) and HasChildren(data.FullPath))
  then
    Include(InitialStates, ivsHasChildren);

end;

procedure TJamBrowser.ClearPreview();
begin

  imagepreview.picture := nil;

  lblJamType.Visible := false;
  lblFilename.Visible := false;
  lblDimensions.Visible := false;
  lblTexs.visible := false;

end;

end.
