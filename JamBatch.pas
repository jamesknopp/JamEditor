unit JamBatch;

interface

uses
  System.IOUtils, Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Generics.Collections,
  System.Classes, Vcl.Dialogs, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.NumberBox, Vcl.Samples.Spin,
  System.Threading, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin,
  jamGeneral, jamPaletteDetector, jamSW, jamHW, System.types, strutils,
  Vcl.Menus, generalhelpers;

type

  TTextureSimpOptions = (quad, seed, mean, neighbour);

  TJamOptions = record
    Simplify: TTextureSimpOptions;
    Blur: integer;
    simplifyThresh: integer;
    doPals: boolean;
    doSimplePal: boolean;
    doSoftenMatte: boolean;
  end;

  TJamBatchItem = class
    filepath: string;
    filename: string;
    outputpath: string;
    inputType: TJamType;
    outputType: TJamType;
    TextureOptions: TJamOptions;
    processed: boolean;
    constructor Create(const aFile: string);
  end;

type
  TJamBatchForm = class(TForm)
    panel: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    cbSimplify: TComboBox;
    chkSimpPalette: TCheckBox;
    chkDoMatte: TCheckBox;
    seBlur: TNumberBox;
    seThreshold: TNumberBox;
    chkDoPalette: TCheckBox;
    strJamFile: TEdit;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    edtOutputPath: TEdit;
    Label5: TLabel;
    btnBrowseOutput: TButton;
    btnRun: TButton;
    Button4: TButton;
    btnAddFile: TButton;
    btnDel: TButton;
    lvBatch: TListView;
    btnScanFolder: TButton;
    radioGP2: TRadioButton;
    radioGP3: TRadioButton;
    radioGP3HW: TRadioButton;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    jamBatchPopup: TPopupMenu;
    AddFiles01: TMenuItem;
    AddScanFolder1: TMenuItem;
    N1: TMenuItem;
    DeleteItems1: TMenuItem;
    Label6: TLabel;
    edtFilename: TEdit;
    chkScanAllFolders: TCheckBox;
    btnConvertTrack: TButton;
    openTrack: TOpenDialog;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnScanFolderClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure lvBatchSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure cbSimplifyChange(Sender: TObject);
    procedure seThresholdChange(Sender: TObject);
    procedure seBlurChange(Sender: TObject);
    procedure edtOutputPathChange(Sender: TObject);
    procedure rgOutputTypeClick(Sender: TObject);
    procedure chkSimpPaletteClick(Sender: TObject);
    procedure chkDoMatteClick(Sender: TObject);
    procedure chkDoPaletteClick(Sender: TObject);
    function GetJamType(jamType: TJamType): string;
    function GetSimplifyOptions(opts: TTextureSimpOptions): string;
    procedure radioGP2Click(Sender: TObject);
    procedure radioGP3Click(Sender: TObject);
    procedure radioGP3HWClick(Sender: TObject);
    procedure lvBatchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure jamBatchPopupPopup(Sender: TObject);
    procedure lvBatchClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure edtFilenameChange(Sender: TObject);
    procedure DeleteItems1Click(Sender: TObject);
    procedure btnConvertTrackClick(Sender: TObject);

  private
    // O(1) duplicate detection — paired with BatchList. Normalised lower-case
    // filepath → item. Kept in sync in AddBatchItem / btnDelClick / OnClear.
    FPathIndex: TDictionary<string, TJamBatchItem>;

    procedure RefreshListView;
    procedure ConvertJam;
    procedure ConvertSingleItem(Item: TJamBatchItem; DoPalette: Boolean);
    procedure UpdateListViewItem(li: TListItem);
    procedure UpdateItemRow(Item: TJamBatchItem);
    procedure RefreshSelectedItems;
    procedure PopulateDetails(const Items: TArray<TJamBatchItem>);
    procedure ApplyDetailsToItems(const Items: TArray<TJamBatchItem>);
    procedure OnItemProcessed(Item: TJamBatchItem);
    function GetSelectedBatchItems: TArray<TJamBatchItem>;
    function ItemExists(const APath: string): Boolean;
    function AddBatchItem(const FilePath: string): TJamBatchItem;
    function DefaultOutputType(InputType: TJamType): TJamType;
    function DefaultOutputPath(const SourcePath: string;
      OutputType: TJamType): string;
    function NormalisePath(const APath: string): string;
    procedure EnsurePathIndex;
  public
    destructor Destroy; override;
  end;

var
  JamBatchForm: TJamBatchForm;
  BatchList: TObjectList<TJamBatchItem>;

procedure InitBatchList;
procedure FreeBatchList;

implementation

{$R *.dfm}

constructor TJamBatchItem.Create(const aFile: string);
begin
  inherited Create;
  filepath := aFile;
  outputpath := ChangeFileExt(aFile, ''); // or wherever you like
  inputType := jamGP3SW;
  outputType := jamGP3HW;

  with TextureOptions do
  begin
    Simplify := neighbour;
    Blur := intBlurThreshold;
    simplifyThresh := intSimplifyThreshold;
    doSimplePal := False;
    doSoftenMatte := False;
  end;
  processed := False;
end;

procedure InitBatchList;
begin
  BatchList := TObjectList<TJamBatchItem>.Create(True);
end;

procedure FreeBatchList;
begin
  BatchList.Free;
end;

destructor TJamBatchForm.Destroy;
begin
  FreeAndNil(FPathIndex);
  inherited;
end;

function TJamBatchForm.NormalisePath(const APath: string): string;
begin
  Result := ExpandFileName(APath).ToLower;
end;

procedure TJamBatchForm.EnsurePathIndex;
var
  itm: TJamBatchItem;
begin
  if FPathIndex <> nil then Exit;
  FPathIndex := TDictionary<string, TJamBatchItem>.Create;
  // Populate from any items already in BatchList (defensive)
  for itm in BatchList do
    FPathIndex.AddOrSetValue(NormalisePath(itm.filepath), itm);
end;

function TJamBatchForm.ItemExists(const APath: string): Boolean;
begin
  EnsurePathIndex;
  Result := FPathIndex.ContainsKey(NormalisePath(APath));
end;

function TJamBatchForm.DefaultOutputType(InputType: TJamType): TJamType;
begin
  // "Convert to the opposite" default: SW → HW, HW → SW, GP2 → GP3SW
  case InputType of
    jamGP3SW: Result := jamGP3HW;
    jamGP3HW: Result := jamGP3SW;
    jamGP2:   Result := jamGP3SW;
  else
    Result := InputType;
  end;
end;

function TJamBatchForm.DefaultOutputPath(const SourcePath: string;
  OutputType: TJamType): string;
var
  dir: string;
begin
  dir := ExtractFilePath(SourcePath);
  // GP3 SW and HW share the same pair of Gp3Jams / Gp3JamsH folders;
  // flip between them so output lands in the "other" folder by default.
  if OutputType in [jamGP3SW, jamGP3HW] then
    Result := ToggleGP3JamsFolder(dir)
  else
    Result := dir;
end;

function TJamBatchForm.AddBatchItem(const FilePath: string): TJamBatchItem;
begin
  EnsurePathIndex;
  if FPathIndex.ContainsKey(NormalisePath(FilePath)) then
    Exit(nil);

  Result := TJamBatchItem.Create(FilePath);
  Result.inputType := TJamPaletteDetector.Instance.Detect(FilePath, True);
  Result.outputType := DefaultOutputType(Result.inputType);
  Result.filename := ExtractFileName(FilePath);
  Result.outputpath := DefaultOutputPath(FilePath, Result.outputType);

  BatchList.Add(Result);
  FPathIndex.Add(NormalisePath(FilePath), Result);
end;

procedure TJamBatchForm.jamBatchPopupPopup(Sender: TObject);
begin
  if lvBatch.SelCount > 0 then
    DeleteItems1.Enabled := True
  else
    DeleteItems1.Enabled := False;
end;

function TJamBatchForm.GetSelectedBatchItems: TArray<TJamBatchItem>;
var
  i: integer;
  tmp: TList<TJamBatchItem>;
begin
  tmp := TList<TJamBatchItem>.Create;
  try
    for i := 0 to lvBatch.Items.Count - 1 do
      if lvBatch.Items[i].Selected then
        tmp.Add(TJamBatchItem(lvBatch.Items[i].Data));
    Result := tmp.ToArray;
  finally
    tmp.Free;
  end;
end;

procedure TJamBatchForm.btnAddFileClick(Sender: TObject);
var
  dlg: TOpenDialog;
  fn: string;
begin
  dlg := TOpenDialog.Create(nil);
  try
    dlg.Filter := 'JAM files|*.jam;*.jip';
    dlg.Options := dlg.Options + [ofAllowMultiSelect];
    if dlg.Execute then
      for fn in dlg.Files do
        AddBatchItem(fn);
    RefreshListView;
  finally
    dlg.Free;
  end;
end;

procedure TJamBatchForm.btnBrowseOutputClick(Sender: TObject);
var
  dlg: TFileOpenDialog;
begin
  dlg := TFileOpenDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [fdoPickFolders, fdoPathMustExist];
    dlg.Title := 'Select output folder';
    if dlg.Execute then
    begin
      edtOutputPath.Text := dlg.filename;
      edtOutputPath.SetFocus;
      ApplyDetailsToItems(GetSelectedBatchItems);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TJamBatchForm.btnDelClick(Sender: TObject);
var
  i: integer;
  idxs: TList<integer>;
  itm: TJamBatchItem;
begin
  if lvBatch.SelCount = 0 then Exit;
  if MessageDlg('Remove the selected item(s)?', mtConfirmation, [mbYes, mbNo],
    0) <> mrYes then
    Exit;

  EnsurePathIndex;
  idxs := TList<integer>.Create;
  try
    for i := 0 to lvBatch.Items.Count - 1 do
      if lvBatch.Items[i].Selected then
        idxs.Add(i);
    idxs.Sort;
    // remove highest to lowest so preceding indexes stay valid
    for i := idxs.Count - 1 downto 0 do
    begin
      itm := BatchList[idxs[i]];
      FPathIndex.Remove(NormalisePath(itm.filepath));
      BatchList.Delete(idxs[i]); // TObjectList owns → frees item
    end;
    RefreshListView;
  finally
    idxs.Free;
  end;
end;

procedure TJamBatchForm.btnScanFolderClick(Sender: TObject);
var
  dlg: TFileOpenDialog;
  Dir: string;
  Files: TArray<string>;
  f: string;
  opt: TSearchOption;
begin
  dlg := TFileOpenDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [fdoPickFolders, fdoPathMustExist];
    dlg.Title := 'Select folder to scan for JAMs';
    if not dlg.Execute then Exit;

    Dir := dlg.filename;
    if chkScanAllFolders.Checked then
      opt := TSearchOption.soAllDirectories
    else
      opt := TSearchOption.soTopDirectoryOnly;

    Files := TDirectory.GetFiles(Dir, '*.jam', opt);
    for f in Files do
      AddBatchItem(f);

    RefreshListView;
  finally
    dlg.Free;
  end;
end;

procedure TJamBatchForm.btnConvertTrackClick(Sender: TObject);
begin
end;

procedure TJamBatchForm.cbSimplifyChange(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.chkDoMatteClick(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.chkDoPaletteClick(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.chkSimpPaletteClick(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.edtOutputPathChange(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.lvBatchClick(Sender: TObject);
begin
  if lvBatch.SelCount > 0 then
    btnDel.Enabled := True
  else
    btnDel.Enabled := False;
end;

procedure TJamBatchForm.lvBatchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Ctrl+A → select all
  if (Key = Ord('A')) and (Shift = [ssCtrl]) then
  begin
    lvBatch.Items.BeginUpdate;
    try
      for var i := 0 to lvBatch.Items.Count - 1 do
        lvBatch.Items[i].Selected := True;
    finally
      lvBatch.Items.EndUpdate;
    end;
    Key := 0; // swallow
  end
  // Delete → confirm & remove
  else if (Key = VK_DELETE) and (lvBatch.SelCount > 0) then
  begin
    btnDelClick(Sender);
    Key := 0;
  end;
end;

procedure TJamBatchForm.lvBatchSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
var
  sel: TList<TJamBatchItem>;
  i: integer;
begin
  sel := TList<TJamBatchItem>.Create;
  try
    for i := 0 to lvBatch.Items.Count - 1 do
      if lvBatch.Items[i].Selected then
        sel.Add(TJamBatchItem(lvBatch.Items[i].Data));
    PopulateDetails(sel.ToArray);
  finally
    sel.Free;
  end;

  if lvBatch.SelCount > 0 then
    btnDel.Enabled := True
  else
    btnDel.Enabled := False;
end;

procedure TJamBatchForm.ConvertSingleItem(Item: TJamBatchItem;
  DoPalette: Boolean);
// Handles one input/output type pair with the correct load/convert/save
// pattern. Always frees its bitmap objects, even on exception.
// Item.outputpath is the directory; Item.filename is the file name —
// SaveToFile needs the full path, so we combine them here.
var
  JamFile, OldJamFile: TJamFile;
  HWJamFile: THWJamFile;
  outFile: string;
  x: integer;

  procedure ZeroPalettesIfRequested;
  begin
    if not DoPalette then Exit;
    for x := 0 to JamFile.FEntries.Count - 1 do
      JamFile.ZeroPalette(x);
  end;

begin
  // Ensure the destination directory exists
  checkPath(Item.outputpath);

  // Build the full output file path
  outFile := TPath.Combine(Item.outputpath, Item.filename);

  // HW → SW/GP2: load HW, convert, save SW
  if Item.inputType = jamGP3HW then
  begin
    if not (Item.outputType in [jamGP3SW, jamGP2]) then Exit;
    JamFile := TJamFile.Create;
    HWJamFile := THWJamFile.Create;
    try
      HWJamFile.LoadFromFile(Item.filepath);
      JamFile.ConvertHWJam(HWJamFile, Item.outputType = jamGP2);
      JamFile.SaveToFile(outFile, False);
    finally
      JamFile.Free;
      HWJamFile.Free;
    end;
    Exit;
  end;

  // GP2 / GP3SW source — two sub-paths: → SW (with palette option) or → HW
  if Item.inputType in [jamGP2, jamGP3SW] then
  begin
    if Item.outputType = jamGP3HW then
    begin
      HWJamFile := THWJamFile.Create;
      try
        HWJamFile.ConvertGpxJam(Item.filepath);
        HWJamFile.SaveToFile(outFile);
      finally
        HWJamFile.Free;
      end;
      Exit;
    end;

    if Item.outputType in [jamGP2, jamGP3SW] then
    begin
      JamFile := TJamFile.Create;
      OldJamFile := TJamFile.Create;
      try
        OldJamFile.LoadFromFile(Item.filepath, False);
        JamFile.ConvertGpxJam(OldJamFile, Item.outputType = jamGP2);
        ZeroPalettesIfRequested;
        JamFile.SaveToFile(outFile, False);
      finally
        JamFile.Free;
        OldJamFile.Free;
      end;
    end;
  end;
end;

procedure TJamBatchForm.ConvertJam;
// Snapshot BatchList, process each item on a background thread. The
// snapshot is a local var, not a global. Per-item UI updates are marshalled
// to the main thread via TThread.Queue so the batch keeps running and
// doesn't race with VCL.
var
  items: TArray<TJamBatchItem>;
  doPalette: Boolean;
begin
  if BatchList.Count = 0 then Exit;
  items := BatchList.ToArray;
  doPalette := chkDoPalette.Checked;

  TTask.Run(
    procedure
    var
      i: integer;
      it: TJamBatchItem;
    begin
      for i := 0 to High(items) do
      begin
        it := items[i];
        try
          ConvertSingleItem(it, doPalette);
          it.processed := True;
        except
          on E: Exception do
          begin
            // Mark as not-processed; continue with remaining files
            it.processed := False;
            OutputDebugString(PChar(Format(
              'Batch: conversion failed for %s: %s',
              [it.filepath, E.Message])));
          end;
        end;
        // Marshal the UI update to the main thread — non-blocking so the
        // background batch doesn't stall on each Queue call.
        TThread.Queue(nil,
          procedure
          begin
            OnItemProcessed(it);
          end);
      end;
    end);
end;

procedure TJamBatchForm.DeleteItems1Click(Sender: TObject);
begin
  btnDelClick(Sender);
end;

procedure TJamBatchForm.edtFilenameChange(Sender: TObject);
begin

  if lvBatch.SelCount > 1 then
    Exit;

  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.btnRunClick(Sender: TObject);
begin
  // ConvertJam internally launches its own TTask; don't double-wrap.
  ConvertJam;
end;

procedure TJamBatchForm.radioGP2Click(Sender: TObject);
begin
  radioGP3.Checked := False;
  radioGP3HW.Checked := False;
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.radioGP3Click(Sender: TObject);
begin
  radioGP2.Checked := False;
  radioGP3HW.Checked := False;
  ApplyDetailsToItems(GetSelectedBatchItems);

end;

procedure TJamBatchForm.radioGP3HWClick(Sender: TObject);
begin
  radioGP2.Checked := False;
  radioGP3.Checked := False;
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.OnItemProcessed(Item: TJamBatchItem);
begin
  // Just update the one row rather than clearing + rebuilding the entire
  // ListView (which loses sort order, selection, and scroll position).
  UpdateItemRow(Item);
end;

procedure TJamBatchForm.UpdateItemRow(Item: TJamBatchItem);
var
  i: integer;
  li: TListItem;
begin
  for i := 0 to lvBatch.Items.Count - 1 do
  begin
    li := lvBatch.Items[i];
    if TJamBatchItem(li.Data) = Item then
    begin
      UpdateListViewItem(li);
      Exit;
    end;
  end;
end;

function TJamBatchForm.GetJamType(jamType: TJamType): string;
begin
  case jamType of
    jamGP2:   Result := 'GP2 JAM';
    jamGP3SW: Result := 'GP3 Software JAM';
    jamGP3HW: Result := 'GP3 Hardware JAM';
  else
    Result := '';
  end;
end;

function TJamBatchForm.GetSimplifyOptions(opts: TTextureSimpOptions): string;
begin
  case opts of
    quad:      Result := 'Quad Tree Method (Blocky)';
    seed:      Result := 'Seed Threshold';
    mean:      Result := 'Mean Region';
    neighbour: Result := 'Neighbour Threshold (Usually best)';
  else
    Result := '';
  end;
end;

procedure TJamBatchForm.RefreshListView;
var

  itm: TJamBatchItem;
  li: TListItem;
begin
  lvBatch.Items.BeginUpdate;
  try
    lvBatch.Items.Clear;
    for itm in BatchList do
    begin
      li := lvBatch.Items.Add;
      li.Caption := ExtractFileName(itm.filepath);
      li.SubItems.Add(GetJamType(itm.inputType));
      li.SubItems.Add(GetJamType(itm.outputType));
      li.SubItems.Add(TPath.Combine(itm.outputpath, itm.filename));
      li.SubItems.Add(IfThen(itm.processed, 'Done', 'Pending'));
      li.SubItems.Add(GetSimplifyOptions(itm.TextureOptions.Simplify));
      li.SubItems.Add(inttostr(itm.TextureOptions.Blur));
      li.SubItems.Add(inttostr(itm.TextureOptions.simplifyThresh));
      li.SubItems.Add(IfThen(itm.TextureOptions.doSimplePal, 'Yes', 'No'));
      li.SubItems.Add(IfThen(itm.TextureOptions.doSoftenMatte, 'Yes', 'No'));
      li.SubItems.Add(IfThen(itm.TextureOptions.doPals, 'Yes', 'No'));
      li.Data := itm;
    end;
  finally
    lvBatch.Items.EndUpdate;
  end;
end;

procedure TJamBatchForm.UpdateListViewItem(li: TListItem);
var
  itm: TJamBatchItem;
begin
  itm := TJamBatchItem(li.Data);

  // li.Caption is column 0
  li.Caption := ExtractFileName(itm.filepath);

  // SubItems[0] is column 1, SubItems[1] is column 2, etc.
  li.SubItems[0] := GetJamType(itm.inputType);
  li.SubItems[1] := GetJamType(itm.outputType);
  li.SubItems[2] := TPath.Combine(itm.outputpath, itm.filename);
  li.SubItems[3] := IfThen(itm.processed, 'Done', 'Pending');
  li.SubItems[4] := GetSimplifyOptions(itm.TextureOptions.Simplify);
  li.SubItems[5] := inttostr(itm.TextureOptions.Blur);
  li.SubItems[6] := inttostr(itm.TextureOptions.simplifyThresh);
  li.SubItems[7] := IfThen(itm.TextureOptions.doSimplePal, 'Yes', 'No');
  li.SubItems[8] := IfThen(itm.TextureOptions.doSoftenMatte, 'Yes', 'No');
  li.SubItems[9] := IfThen(itm.TextureOptions.doPals, 'Yes', 'No');
end;

procedure TJamBatchForm.RefreshSelectedItems;
var
  i: integer;
begin
  lvBatch.Items.BeginUpdate;
  try
    for i := 0 to lvBatch.Items.Count - 1 do
      if lvBatch.Items[i].Selected then
        UpdateListViewItem(lvBatch.Items[i]);
  finally
    lvBatch.Items.EndUpdate;
  end;
end;

procedure TJamBatchForm.rgOutputTypeClick(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.seBlurChange(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.seThresholdChange(Sender: TObject);
begin
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

// show values (or “mixed” state) for 1 vs. many
procedure TJamBatchForm.PopulateDetails(const Items: TArray<TJamBatchItem>);
var
  single: boolean;
  jamItem: TJamBatchItem;
  x: integer;
begin
  single := Length(Items) = 1;
  if single then
    jamItem := Items[0];

  // OutputPath
  if single then
  begin
    edtOutputPath.Text := jamItem.outputpath;
    strJamFile.Text := jamItem.filepath;
    edtFilename.Text := jamItem.filename;
  end
  else
    edtOutputPath.Text := '';

  // OutputType
  if single then
  begin
    x := Ord(jamItem.outputType);
    if x = 0 then
      radioGP2.Checked := True;
    if x = 1 then
      radioGP3.Checked := True;
    if x = 2 then
      radioGP3HW.Checked := True;
  end;

  // texture options...
  if single then
  begin
    cbSimplify.ItemIndex := Ord(jamItem.TextureOptions.Simplify);
    seBlur.Value := jamItem.TextureOptions.Blur;
    seThreshold.Value := Round(jamItem.TextureOptions.simplifyThresh);
    chkDoPalette.Checked := jamItem.TextureOptions.doPals;
    chkDoMatte.Checked := jamItem.TextureOptions.doSoftenMatte;
    chkSimpPalette.Checked := jamItem.TextureOptions.doSimplePal;

  end
  else
  begin
    cbSimplify.ItemIndex := -1;
    seBlur.Value := 0;
    seThreshold.Value := 0;
    chkDoPalette.Checked := False;
    chkDoMatte.Checked := False;
    chkSimpPalette.Checked := False;
    edtFilename.Text := '';
    edtOutputPath.Text := '';
    strJamFile.Text := '';
  end;
end;

// Write any changed detail back to all selected items. We use "which
// control has focus" as a proxy for "which property the user is editing",
// so only the edited field gets applied across selection.
procedure TJamBatchForm.ApplyDetailsToItems(const Items: TArray<TJamBatchItem>);
var
  jamItem: TJamBatchItem;
begin
  for jamItem in Items do
  begin
    if edtOutputPath.Focused then
      jamItem.outputpath := edtOutputPath.Text;

    if edtFilename.Focused then
      jamItem.filename := edtFilename.Text;

    if radioGP2.Focused and radioGP2.Checked then
      jamItem.outputType := jamGP2;
    if radioGP3.Focused and radioGP3.Checked then
      jamItem.outputType := jamGP3SW;
    if radioGP3HW.Focused and radioGP3HW.Checked then
      jamItem.outputType := jamGP3HW;

    if cbSimplify.Focused then
      jamItem.TextureOptions.Simplify :=
        TTextureSimpOptions(cbSimplify.ItemIndex);
    if seBlur.Focused then
      jamItem.TextureOptions.Blur := Round(seBlur.Value);
    if seThreshold.Focused then
      jamItem.TextureOptions.simplifyThresh := Round(seThreshold.Value);
    if chkDoPalette.Focused then
      jamItem.TextureOptions.doPals := chkDoPalette.Checked;
    if chkDoMatte.Focused then
      jamItem.TextureOptions.doSoftenMatte := chkDoMatte.Checked;
    if chkSimpPalette.Focused then
      jamItem.TextureOptions.doSimplePal := chkSimpPalette.Checked;
  end;
  RefreshSelectedItems;
end;

initialization

InitBatchList;

finalization

FreeBatchList;

end.
