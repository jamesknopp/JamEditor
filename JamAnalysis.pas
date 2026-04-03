unit JamAnalysis;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.StrUtils,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MPCommonObjects, EasyListview,
  System.Threading, System.IOUtils, System.SyncObjs,
  Vcl.ExtCtrls, JamSW, JamHW, JamPalette, JamGeneral, jamPaletteDetector,
  Vcl.StdCtrls, System.Math, Vcl.ComCtrls;

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
    Splitter1: TSplitter;
    GroupBox1: TGroupBox;
    texPreview: TImage;
    scanGroup: TGroupBox;
    strFolder: TEdit;
    Label1: TLabel;
    Button2: TButton;
    chkSubfolders: TCheckBox;
    btnScanFolder: TButton;
    btnScanGP2: TButton;
    btnScanGP3: TButton;
    scanGP32K: TButton;
    chkSoftware: TCheckBox;
    chkHardware: TCheckBox;
    ProgressBar: TProgressBar;
    jamLoading: TLabel;
    Button1: TButton;
    chkClearList: TCheckBox;
    procedure btnScanFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure jamlistviewItemImageDraw(Sender: TCustomEasyListview;
      Item: TEasyItem; Column: TEasyColumn; ACanvas: TCanvas;
      const RectArray: TEasyRectArrayObject; AlphaBlender: TEasyAlphaBlender);
    // procedure jamlistviewItemImageDrawIsCustom(Sender: TCustomEasyListview;
    // Item: TEasyItem; Column: TEasyColumn; var IsCustom: Boolean);
    function jamlistviewItemCompare(Sender: TCustomEasyListview;
      Column: TEasyColumn; Group: TEasyGroup; Item1, Item2: TEasyItem;
      var DoDefault: Boolean): Integer;
    procedure jamlistviewItemFreeing(Sender: TCustomEasyListview;
      Item: TEasyItem);
    procedure jamlistviewItemSelectionChanged(Sender: TCustomEasyListview;
      Item: TEasyItem);
    procedure FormDestroy(Sender: TObject);
    procedure jamlistviewItemImageDrawIsCustom(Sender: TCustomEasyListview;
      Item: TEasyItem; Column: TEasyColumn; var IsCustom: Boolean);
    procedure btnScanGP2Click(Sender: TObject);
    procedure btnScanGP3Click(Sender: TObject);
    procedure scanGP32KClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure strFolderChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    FLoadTask: ITask;
    // FCTS: ICancellationTokenSource;
    // procedure CancelLoad(const WaitMillis: Integer = 2000);
  public
    FileList: TStringList;
    procedure QueueJamItem(const FilePath: string);
    procedure LoadFromFolder(const Folder: string);
    procedure LoadImagesWorker(const Folder: string; subfolders: Boolean);

  end;

var
  frmJamAnalysis: TfrmJamAnalysis;
  strFolderToScan: string;
  cancelJob: Boolean;

implementation

{$R *.dfm}

destructor TJamItem.Destroy;
begin
  FreeAndNil(Thumb);
  inherited;
end;

procedure TfrmJamAnalysis.btnScanGP3Click(Sender: TObject);
begin
  if (chkSoftware.Checked = false) and (chkHardware.Checked = false) then
  begin
    showMessage
      ('Select whether you want to scan for software and/or hardware JAMs');
    exit;
  end;

  if strGP3Location.Length > 0 then
    LoadImagesWorker(strGP3Location, chkSubfolders.Checked)
  else
    showMessage('GP3 Location not defined - go to options to locate GP3');
end;

procedure TfrmJamAnalysis.Button1Click(Sender: TObject);
begin
  cancelJob := true;
end;

procedure TfrmJamAnalysis.Button2Click(Sender: TObject);
var
  dlg: TFileOpenDialog;
  pickedDir: string;
  missing: TArray<string>;
  i: Integer;
  missingString: string;
begin
  dlg := TFileOpenDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [fdoPickFolders, fdoPathMustExist];
    dlg.Title := 'Select folder to browse';
    if not dlg.Execute then
      exit;

    pickedDir := dlg.FileName; // the folder the user chose

    strFolder.Text := pickedDir;
    strFolderToScan := pickedDir;

  finally
    dlg.Free;
  end;

end;

procedure TfrmJamAnalysis.btnScanFolderClick(Sender: TObject);
begin
  if strFolderToScan.Length > 0 then
    if DirectoryExists(strFolderToScan) then
      LoadImagesWorker(strFolderToScan, chkSubfolders.Checked)
    else
      showMessage('Directory does not exist')
  else
    showMessage('Please select a valid folder to scan');

end;

procedure TfrmJamAnalysis.btnScanGP2Click(Sender: TObject);
begin
  if (chkSoftware.Checked = false) and (chkHardware.Checked = false) then
  begin
    showMessage
      ('Select whether you want to scan for software and/or hardware JAMs');
    exit;
  end;

  if strGP2Location.Length > 0 then
    LoadImagesWorker(strGP2Location, chkSubfolders.Checked)
  else
    showMessage('GP2 Location not defined - go to options to locate GP2');
end;

procedure TfrmJamAnalysis.FormCreate(Sender: TObject);
begin
  FileList := TStringList.Create;
  cancelJob := false;
end;

procedure TfrmJamAnalysis.FormDestroy(Sender: TObject);
begin
  jamlistview.items.Clear;
  FileList.Free;
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

    DoDefault := false; // prevent default sorting
  end
  else
    DoDefault := true; // fall back to default sort for other columns
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
  img: TBitmap;
begin
  img := TJamItem(Item.data).Thumb;
  AlphaBlender.Blend(Sender, Item, ACanvas, RectArray.IconRect, img);
end;

procedure TfrmJamAnalysis.jamlistviewItemImageDrawIsCustom
  (Sender: TCustomEasyListview; Item: TEasyItem; Column: TEasyColumn;
  var IsCustom: Boolean);
begin

  IsCustom := true;

end;

procedure TfrmJamAnalysis.jamlistviewItemSelectionChanged
  (Sender: TCustomEasyListview; Item: TEasyItem);
begin
  texPreview.Picture.Bitmap := TJamItem(Item.data).Thumb;
end;

procedure TfrmJamAnalysis.LoadFromFolder(const Folder: string);
begin

  TTask.Run(
    procedure
    begin
      LoadImagesWorker(Folder, chkSubfolders.Checked)
    end);
end;

procedure TfrmJamAnalysis.QueueJamItem(const FilePath: string);
var
  JamFile: TJamFile;
  HWJamFile: THWJamFile;
  Thumb: TBitmap;
  Node: TJamItem;
  JamType: string;
  Height, Width, numTexs: Integer;
  jamPal: TJamType;
  Item: TEasyItem;
  FileName: string;

begin
  Thumb := nil;
  Node := nil;
  try
    TThread.Queue(nil,
      procedure

      var
        i, j, k: Integer;

      begin

        if lowercase(TPath.GetFileName(FilePath)) = 'barm.jam' then
          exit;
        if lowercase(TPath.GetFileName(FilePath)) = 'bars.jam' then
          exit;
        if lowercase(TPath.GetFileName(FilePath)) = 'hun_s1.jam' then
          exit;
        if lowercase(TPath.GetFileName(FilePath)) = 'mhill.jam' then
          exit;
        if lowercase(TPath.GetFileName(FilePath)) = 'shill.jam' then
          exit;

        // Detect and load JAM or JIP
        if isHWJAM(FilePath) then
        begin
          if chkHardware.Checked then
          begin
            HWJamFile := THWJamFile.Create;
            try
              HWJamFile.LoadFromFile(FilePath);
              JamType := 'Hardware JAM File';

              for j := 0 to HWJamFile.FHeader.NumItems - 1 do
              begin
                Node := TJamItem.Create;
                Node.Path := FilePath;
                Node.JamType := JamType;
                Node.JamInfo.X := HWJamFile.FEntries[j].FInfo.X;
                Node.JamInfo.Y := HWJamFile.FEntries[j].FInfo.Y;
                Node.JamInfo.Height := HWJamFile.FEntries[j].FInfo.Height;
                Node.JamInfo.JamId := HWJamFile.FEntries[j].FInfo.JamId;
                Node.JamInfo.JamFlags := HWJamFile.FEntries[j].FInfo.JamFlags;

                Node.Thumb := TBitmap.Create;
                Node.Thumb.Height := HWJamFile.FEntries[j].FInfo.Height;
                Node.Thumb.Width := HWJamFile.FEntries[j].FInfo.Width;

                Node.Thumb.Canvas.draw(0, 0, HWJamFile.FEntries[j].FTexture);

                Item := jamlistview.items.add(Node);

                Item.Captions[1] := Node.Path;
                Item.Captions[2] := inttostr(Node.JamInfo.JamId);
                Item.Captions[3] := inttostr(Node.JamInfo.ImagePtr);
                Item.Captions[4] := inttostr(Node.JamInfo.PaletteSizeDiv4);

                for k := 0 to 15 do
                begin
                  Item.Captions[5 + k] :=
                    inttostr(FlagToInt(UnPackFlag(Node.JamInfo.JamFlags, k)));
                end;

                Item.Captions[21] := inttostr(Node.JamInfo.scaleX);
                Item.Captions[22] := inttostr(Node.JamInfo.scaley);
                Item.Captions[23] := inttostr(Node.JamInfo.scaleFactor);
                for k := 0 to 7 do
                begin
                  Item.Captions[24 + k] :=
                    inttostr(FlagToInt(UnPackFlag(Node.JamInfo.scaleFlag, k)));
                end;

              end;

            finally
              HWJamFile.Free;
            end;
          end;
        end
        else
        begin
          if chkSoftware.Checked then
          begin
            JamFile := TJamFile.Create;
            try
              jamPal := TJamPaletteDetector.Instance.Detect(FilePath, true);
              case jamPal of
                jamGP2:
                  for i := 0 to 255 do
                    GPXPal[i] := Gp2Pal[i];
                jamGP3SW:
                  for i := 0 to 255 do
                    GPXPal[i] := Gp3Pal[i];
              end;

              JamFile.LoadFromFile(FilePath, true);
              JamType := IfThen(TPath.GetExtension(FilePath) = '.jip',
                'Software JIP File', 'Software JAM File');

              for j := 0 to JamFile.FHeader.NumItems - 1 do
              begin
                Node := TJamItem.Create;
                Node.Path := FilePath;
                Node.JamType := JamType;
                Node.JamInfo := JamFile.FEntries[j].FInfo;
                Node.Thumb := TBitmap.Create;
                Node.Thumb.Width := JamFile.FEntries[j].Info.Width;
                Node.Thumb.Height := JamFile.FEntries[j].Info.Height;
                Node.Thumb.Canvas.draw(0, 0, JamFile.FEntries[j].FTexture);

                Item := jamlistview.items.add(Node);

                Item.Captions[1] := Node.Path;
                Item.Captions[2] := inttostr(Node.JamInfo.JamId);
                Item.Captions[3] := inttostr(JamFile.FHeader.jamtotalheight);
                Item.Captions[4] := inttostr(Node.JamInfo.PaletteSizeDiv4);

                for k := 0 to 15 do
                begin
                  Item.Captions[5 + k] :=
                    inttostr(FlagToInt(UnPackFlag(Node.JamInfo.JamFlags, k)));
                end;

                Item.Captions[21] := inttostr(Node.JamInfo.scaleX);
                Item.Captions[22] := inttostr(Node.JamInfo.scaley);
                Item.Captions[23] := inttostr(Node.JamInfo.scaleFactor);
                for k := 0 to 7 do
                begin
                  Item.Captions[24 + k] :=
                    inttostr(FlagToInt(UnPackFlag(Node.JamInfo.scaleFlag, k)));

                end;
                Item.Captions[32] := inttostr(Node.JamInfo.Unk);
                Item.Captions[33] := inttostr(Node.JamInfo.Idx0E);

                for k := 0 to 7 do
                  Item.Captions[k + 34] := inttostr(Node.JamInfo.Idx18[k]);
              end;
            finally
              JamFile.Free;
            end;
          end;
        end;
      end);

    Node := nil; // prevent freeing in finally
  finally
    Thumb.Free;
    Node.Free;
  end;
  jamlistview.Sort.SortAll();
end;

procedure TfrmJamAnalysis.scanGP32KClick(Sender: TObject);
begin
  if (chkSoftware.Checked = false) and (chkHardware.Checked = false) then
  begin
    showMessage
      ('Select whether you want to scan for software and/or hardware JAMs');
    exit;
  end;

  if strGP32kLocation.Length > 0 then
    LoadImagesWorker(strGP32kLocation, chkSubfolders.Checked)
  else
    showMessage
      ('GP3 2000 Location not defined - go to options to locate GP3 2000');
end;

procedure TfrmJamAnalysis.strFolderChange(Sender: TObject);
begin
  strFolderToScan := strFolder.Text;
end;

procedure TfrmJamAnalysis.LoadImagesWorker(const Folder: string;
subfolders: Boolean);
var
  SR: TSearchRec;
  NewList, CopiedList: TStringList;
  FileExt: string;
  i: Integer;
  files: TArray<string>;
  f: string;
begin
  cancelJob := false;

  if subfolders then
    files := TDirectory.Getfiles(Folder, '*.jam',
      TSearchOption.soAllDirectories)
  else
    files := TDirectory.Getfiles(Folder, '*.jam',
      TSearchOption.soTopDirectoryOnly);

  NewList := TStringList.Create;
  try
    for f in files do
    begin
      NewList.add(f);
    end;

    // Backup palette (off-thread is okay)
    for i := 0 to 255 do
      tmpPal[i] := GPXPal[i];

    CopiedList := TStringList.Create;
    CopiedList.Assign(NewList);

    TThread.Queue(nil,
      procedure
      var
        X: Integer;
      begin
        try
          Self.jamlistview.items.Clear;
          Self.FileList.Assign(CopiedList);
          scanGroup.Enabled := false;

          Self.ProgressBar.Max := Self.FileList.Count;
          Self.ProgressBar.Position := 0;
          Self.ProgressBar.Visible := true;

          Self.jamLoading.Caption := format('Loading JAM %d of %d',
            [Self.ProgressBar.Position, Self.ProgressBar.Max]);

          Self.jamLoading.Visible := true;

          for X := 0 to Self.FileList.Count - 1 do
          begin
            Self.jamlistview.BeginUpdate;
            Self.QueueJamItem(Self.FileList[X]);
            Self.jamlistview.Update;
            Self.jamlistview.Invalidate;
            Self.jamlistview.EndUpdate;
            Self.ProgressBar.Position := Self.ProgressBar.Position + 1;
            Self.jamLoading.Caption := format('Loading JAM %d of %d',
              [Self.ProgressBar.Position, Self.ProgressBar.Max]);
            Application.ProcessMessages;

            if cancelJob then
              break;

          end;
        finally

          CopiedList.Free;
          scanGroup.Enabled := true;
          Self.jamLoading.Visible := false;
          Self.ProgressBar.Visible := false;
        end;
      end)

  finally
    NewList.Clear;
    NewList.Free;
    // Restore palette (can be done off-thread)
    for i := 0 to 255 do
      GPXPal[i] := tmpPal[i];
  end;
end;

end.
