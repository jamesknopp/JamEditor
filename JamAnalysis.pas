unit JamAnalysis;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.StrUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MPCommonObjects, EasyListview,System.Threading, System.IOUtils,
  Vcl.ExtCtrls, JamSW, JamHW, JamPalette, JamGeneral, jamPaletteDetector,
  Vcl.StdCtrls, System.Math;


type

TJamItem = class
    Path: string;
    JamInfo: TJamEntryInfo;
    Thumb: TBitmap;
    JamType: string;
    destructor Destroy; override;
  end;

  PJamItem = ^TJamItem;

type
  TfrmJamAnalysis = class(TForm)
    Panel1: TPanel;
    jamlistview: TEasyListview;
    Panel2: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    texPreview: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure jamlistviewItemImageDraw(Sender: TCustomEasyListview;
      Item: TEasyItem; Column: TEasyColumn; ACanvas: TCanvas;
      const RectArray: TEasyRectArrayObject; AlphaBlender: TEasyAlphaBlender);
//    procedure jamlistviewItemImageDrawIsCustom(Sender: TCustomEasyListview;
//      Item: TEasyItem; Column: TEasyColumn; var IsCustom: Boolean);
    function jamlistviewItemCompare(Sender: TCustomEasyListview;
      Column: TEasyColumn; Group: TEasyGroup; Item1, Item2: TEasyItem;
      var DoDefault: Boolean): Integer;
    procedure jamlistviewItemFreeing(Sender: TCustomEasyListview;
      Item: TEasyItem);
    procedure jamlistviewItemSelectionChanged(Sender: TCustomEasyListview;
      Item: TEasyItem);
    procedure FormDestroy(Sender: TObject);
    procedure jamlistviewItemImageDrawIsCustom(
  Sender: TCustomEasyListview; Item: TEasyItem; Column: TEasyColumn;
  var IsCustom: Boolean);

  private
    { Private declarations }
  public
    FileList: TStringList;
    procedure QueueJamItem(const FilePath: string);
    procedure LoadFromFolder(const Folder: string);
    procedure LoadImagesWorker(const Folder: string);

  end;

var
  frmJamAnalysis: TfrmJamAnalysis;


implementation


{$R *.dfm}

destructor TJamItem.Destroy;
begin
  FreeAndNil(Thumb);
  inherited;
end;



procedure TfrmJamAnalysis.Button1Click(Sender: TObject);
begin
LoadImagesWorker('D:\gp3\Gp3jamsH');
LoadImagesWorker('D:\gp3\Gp3jams');
LoadImagesWorker('D:\gp2\Gamejams');
end;

procedure TfrmJamAnalysis.FormCreate(Sender: TObject);
begin
filelist := TStringlist.Create;
end;

procedure TfrmJamAnalysis.FormDestroy(Sender: TObject);
begin
  jamListView.items.Clear;
  filelist.free;
end;

function TfrmJamAnalysis.jamlistviewItemCompare(Sender: TCustomEasyListview;
  Column: TEasyColumn; Group: TEasyGroup; Item1, Item2: TEasyItem;
  var DoDefault: Boolean): Integer;
var
  val1, val2: Integer;
begin
  Result := 0;
  if Column.Index > 0 then
  begin
    val1 := StrToIntDef(Item1.Captions[Column.Index], 0);
    val2 := StrToIntDef(Item2.Captions[Column.Index], 0);

    Result := CompareValue(val1, val2);

    // Flip the result for descending sort
    if Column.SortDirection = esdDescending then
      Result := -Result;

    DoDefault := False; // prevent default sorting
  end
  else
    DoDefault := True; // fall back to default sort for other columns
end;

procedure TfrmJamAnalysis.jamlistviewItemFreeing(Sender: TCustomEasyListview;
  Item: TEasyItem);
begin
if Assigned(Item.data) then
  begin
    TJamItem(Item.data).Free;
    Item.data := nil;
  end;
end;

procedure TfrmJamAnalysis.jamlistviewItemImageDraw(Sender: TCustomEasyListview;
  Item: TEasyItem; Column: TEasyColumn; ACanvas: TCanvas;
  const RectArray: TEasyRectArrayObject; AlphaBlender: TEasyAlphaBlender);
var
img : TBitmap;
begin
  img := TJamItem(Item.data).Thumb;
  AlphaBlender.Blend(Sender, Item, ACanvas, RectArray.IconRect, img);
end;

procedure TfrmJamAnalysis.jamlistviewItemImageDrawIsCustom(
  Sender: TCustomEasyListview; Item: TEasyItem; Column: TEasyColumn;
  var IsCustom: Boolean);
begin

isCustom := true;

end;

procedure TfrmJamAnalysis.jamlistviewItemSelectionChanged(
  Sender: TCustomEasyListview; Item: TEasyItem);
begin
    texpreview.Picture.Bitmap := TJamItem(Item.data).Thumb;
end;

procedure TfrmJamAnalysis.LoadFromFolder(const Folder: string);
begin

TTask.Run(procedure begin LoadImagesWorker(Folder)end);
end;

procedure TfrmJamAnalysis.QueueJamItem(const FilePath: string);
var
  JamFile: TJamFile;
  HWJamFile: THWJamFile;
  Thumb: TBitmap;
  Node: TJamItem;
  jamType: string;
  Height, Width, numTexs: Integer;
  jamPal: TJamType;
  Item: TEasyItem;
  filename : string;

begin
  Thumb := nil;
  Node := nil;
  try
TThread.Queue(nil,procedure

    var
  i,j,k : integer;

begin

if lowercase(TPath.GetFileName(filepath)) = 'barm.jam'  then exit;
if lowercase(TPath.GetFileName(filepath)) = 'bars.jam' then exit;
if lowercase(TPath.GetFileName(filepath)) = 'hun_s1.jam' then exit;
if lowercase(TPath.GetFileName(filepath)) = 'mhill.jam' then exit;
if lowercase(TPath.GetFileName(filepath)) = 'shill.jam' then exit;

    // Detect and load JAM or JIP
    if isHWJAM(FilePath) then
    begin
      HWJamFile := THWJamFile.Create;
      try
        HWJamFile.LoadFromFile(FilePath);
        jamType := 'Hardware JAM File';

        for j := 0 to HWJamFile.FHeader.NumItems-1 do
          begin
          node := TJamItem.create;
          node.Path := filepath;
          node.JamType := jamtype;
          node.JamInfo.X := HWJamFile.FEntries[j].FInfo.x;
          node.JamInfo.Y := HWJamFile.FEntries[j].FInfo.Y;
          node.jaminfo.Height := HWJamFile.FEntries[j].FInfo.Height;
          node.jaminfo.JamId := HWJamFile.FEntries[j].FInfo.JamID;
          node.JamInfo.JamFlags := HWJamFile.FEntries[j].FInfo.jamflags;

          node.thumb := TBitmap.Create;
          node.Thumb.height := HWJamFile.FEntries[j].finfo.Height;
          node.Thumb.width := HWJamFile.FEntries[j].finfo.width;

          node.thumb.Canvas.draw(0,0,HWJamFile.FEntries[j].FTexture);

          item := jamlistview.items.add(node);

          item.captions[1] := node.Path;
          item.captions[2] := inttostr(node.JamInfo.JamId);
          item.captions[3] := inttostr(node.JamInfo.ImagePtr);
          item.captions[4] := inttostr(node.JamInfo.PaletteSizeDiv4);

          for k := 0 to 15 do
          begin
          item.captions[5+k] := inttostr(FlagToInt(UnPackFlag(node.jaminfo.JamFlags,k)));
          end;

//          item.captions[21] := inttostr(jamfile.GetIDX08_X(node.jaminfo.Idx08));
//          item.captions[22] := inttostr(jamfile.GetIDX08_Y(node.jaminfo.Idx08));
//          item.captions[23] := inttostr(jamfile.GetIDX0aScale(node.jaminfo.Idx0a));
//          for k := 0 to 7 do
//          begin
//          item.captions[24+k] := inttostr(FlagToInt(UnPackFlag(jamfile.GetIDX0aFlags(node.jaminfo.Idx0A),k)));
//          end;
//          item.captions[32] := inttostr(node.JamInfo.Unk);
//          item.captions[33] := inttostr(node.JamInfo.Idx0E);
//
//          for k := 0 to 7 do
//          item.captions[k] := inttostr(node.JamInfo.Idx18[k]);
          end;

      finally
        HWJamFile.Free;
      end;
    end
    else
    begin
      JamFile := TJamFile.Create;
      try
        jamPal := TJamPaletteDetector.Instance.Detect(FilePath, true);
        case jamPal of
          jamGP2:  for i := 0 to 255 do GPXPal[i] := Gp2Pal[i];
          jamGP3SW: for i := 0 to 255 do GPXPal[i] := Gp3Pal[i];
        end;

        JamFile.LoadFromFile(FilePath, true);
        jamType := IfThen(TPath.GetExtension(FilePath) = '.jip', 'Software JIP File', 'Software JAM File');

        for j := 0 to JamFile.FHeader.NumItems-1 do
        begin
          node := TJamItem.create;
          node.Path := filepath;
          node.JamType := jamtype;
          node.JamInfo := jamfile.FEntries[j].FInfo;
          node.Thumb := TBitmap.create;
          node.thumb.width := jamfile.FEntries[j].Info.Width;
          node.thumb.height := jamfile.FEntries[j].Info.height;
          node.Thumb.Canvas.Draw(0,0,jamfile.FEntries[j].FTexture);

          item := jamlistview.items.add(node);

          item.captions[1] := node.Path;
          item.captions[2] := inttostr(node.JamInfo.JamId);
          item.captions[3] := inttostr(node.JamInfo.ImagePtr);
          item.captions[4] := inttostr(node.JamInfo.PaletteSizeDiv4);

          for k := 0 to 15 do
          begin
          item.captions[5+k] := inttostr(FlagToInt(UnPackFlag(node.jaminfo.JamFlags,k)));
          end;

//          item.captions[21] := inttostr(jamfile.GetIDX08_X(node.jaminfo.Idx08));
//          item.captions[22] := inttostr(jamfile.GetIDX08_Y(node.jaminfo.Idx08));
//          item.captions[23] := inttostr(jamfile.GetIDX0aScale(node.jaminfo.Idx0a));

          item.captions[21] := inttostr(node.jaminfo.scaleX);
          item.captions[22] := inttostr(node.jaminfo.scaley);
          item.captions[23] := inttostr(node.jaminfo.scaleFactor);
          for k := 0 to 7 do
          begin
          item.captions[24+k] := inttostr(FlagToInt(UnPackFlag(node.JamInfo.scaleFlag,k)));
//          item.captions[24+k] := inttostr(FlagToInt(UnPackFlag(jamfile.GetIDX0aFlags(node.jaminfo.Idx0A),k)));
          end;
          item.captions[32] := inttostr(node.JamInfo.Unk);
          item.captions[33] := inttostr(node.JamInfo.Idx0E);

          for k := 0 to 7 do
          item.captions[k+34] := inttostr(node.JamInfo.Idx18[k]);
        end;
      finally
        JamFile.Free;
      end;
    end;end);

    Node := nil; // prevent freeing in finally
  finally
    Thumb.Free;
    Node.Free;
  end;
    jamListView.Sort.SortAll();
end;

procedure TfrmJamAnalysis.LoadImagesWorker(const Folder: string);
var
  SR: TSearchRec;
  NewList,CopiedList: TStringList;
  FileExt: string;
  i: Integer;
  files : TArray<string>;
  f : string;
begin

Files := TDirectory.Getfiles(folder, '*.jam',TSearchOption.soAllDirectories);

  NewList := TStringList.Create;
  try
  for f in files do
  begin
  newlist.add(f);
  end;

//    // Off-thread file scanning
//    if FindFirst(Folder + '\*.*', faAnyFile, SR) = 0 then
//    begin
//      repeat
//        if (SR.Attr and faDirectory = 0) then
//        begin
//          FileExt := LowerCase(ExtractFileExt(SR.Name));
//          if (FileExt = '.jam') or (FileExt = '.jip') then
//            if not SameText(SR.Name, 'shill.jam') then
//              NewList.Add(Folder + '\' + SR.Name);
//        end;
//      until FindNext(SR) <> 0;
//      FindClose(SR);
//    end;


    // Backup palette (off-thread is okay)
    for i := 0 to 255 do
      tmpPal[i] := GPXPal[i];

    CopiedList := TStringList.Create;
    CopiedList.Assign(NewList);

TThread.Queue(nil,procedure
      var
        x: Integer;
      begin
        try
//          Self.jamListView.Items.Clear;
          Self.FileList.Assign(copiedlist);



//          Self.ProgressBar.Max := Self.FileList.Count;
//          Self.ProgressBar.Position := 0;
//          Self.ProgressBar.Visible := True;
//
//          Self.jamLoading.Visible := True;
//
//          Self.FPendingThumbnails := Self.FileList.Count;
//          Self.FLoadedThumbnails := 0;

          for x := 0 to Self.FileList.Count - 1 do
          begin
            Self.jamListView.BeginUpdate;
            Self.QueueJamItem(Self.FileList[x]);
            self.jamlistview.Update;
            self.jamlistview.Invalidate;
            Application.ProcessMessages;
            Self.jamListView.EndUpdate;
          end;
        finally

          copiedlist.free;
//          Self.jamLoading.Visible := false;
        end;
      end)

  finally
    NewList.Free;
    // Restore palette (can be done off-thread)
    for i := 0 to 255 do
      GPXPal[i] := tmpPal[i];
  end;
end;




end.
