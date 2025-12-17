unit JamBatch;

interface

uses
  System.IOUtils, Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Generics.Collections,
  System.Variants, System.Classes, Vcl.FileCtrl, Vcl.Dialogs, Vcl.Graphics,
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

  private
    procedure RefreshListView;

    procedure ConvertJam();
    procedure UpdateListViewItem(li: TListItem);
    procedure RefreshSelectedItems;
    procedure PopulateDetails(const Items: TArray<TJamBatchItem>);
    procedure ApplyDetailsToItems(const Items: TArray<TJamBatchItem>);
    procedure OnItemProcessed(Item: TJamBatchItem);
    function GetSelectedBatchItems: TArray<TJamBatchItem>;
    function ItemExists(const APath: string): boolean;
  public
    { Public declarations }
  end;

var
  JamBatchForm: TJamBatchForm;
  BatchList: TObjectList<TJamBatchItem>;
  tempDir: string;
  updateDir: boolean;
  tmpArr : TArray<TJamBatchItem>;

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

function TJamBatchForm.ItemExists(const APath: string): boolean;
var
  itm: TJamBatchItem;
  normNew, normExisting: string;
begin

  normNew := ExpandFileName(APath).ToLower;
  for itm in BatchList do
  begin
    normExisting := ExpandFileName(itm.filepath).ToLower;
    if normExisting = normNew then
      Exit(True);
  end;
  Result := False;
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
  fn, dironly: string;
  itm: TJamBatchItem;
begin
  dlg := TOpenDialog.Create(nil);
  try
    dlg.Filter := 'JAM files|*.jam;*.jip';
    dlg.Options := dlg.Options + [ofAllowMultiSelect];
    if dlg.Execute then
      for fn in dlg.Files do
      begin
        if ItemExists(fn) then
          Continue; // skip duplicates
        itm := TJamBatchItem.Create(fn);
        // detect palette

        itm.inputType := TJamPaletteDetector.Instance.Detect(fn, True);
        itm.outputType := itm.inputType;

        itm.filename := ExtractFileName(fn);


        dironly := extractfilepath(fn);

        if (itm.outputType = jamGP3SW) or (itm.outputType = jamGP3HW) then
          itm.outputpath := ToggleGP3JamsFolder(dironly)
          else
          itm.outputpath := dironly;

        BatchList.Add(itm);
      end;
    RefreshListView;
  finally
    dlg.Free;
  end;
end;

procedure TJamBatchForm.btnBrowseOutputClick(Sender: TObject);
var
  dlg: TFileOpenDialog;
  Dir: string;
  Files: TArray<string>;
  f: string;
  itm: TJamBatchItem;
begin
  dlg := TFileOpenDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [fdoPickFolders, fdoPathMustExist];
    dlg.Title := 'Select output folder';
    if dlg.Execute then
    begin
      tempDir := dlg.FileName; // actually the folder
      edtOutputPath.Text := tempDir;
      edtOutputPath.SetFocus;
      updateDir := True;
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
begin
  if lvBatch.SelCount > 0 then
    if MessageDlg('Remove the selected item(s)?', mtConfirmation, [mbYes, mbNo],
      0) = mrYes then
    begin
      idxs := TList<integer>.Create;
      try
        // gather selected indexes
        for i := 0 to lvBatch.Items.Count - 1 do
          if lvBatch.Items[i].Selected then
            idxs.Add(i);
            idxs.Sort;
        // remove highest to lowest
        for i := idxs.Count - 1 downto 0 do
          BatchList.Delete(idxs[i]);
        RefreshListView;
      finally
        idxs.Free;
      end;
    end;
end;

procedure TJamBatchForm.btnScanFolderClick(Sender: TObject);
var
  dlg: TFileOpenDialog;
  Dir: string;
  Files: TArray<string>;
  f: string;
  itm: TJamBatchItem;
begin
  dlg := TFileOpenDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [fdoPickFolders, fdoPathMustExist];
    dlg.Title := 'Select folder to scan for JAMs';
    if dlg.Execute then
    begin
      Dir := dlg.FileName; // actually the folder
      if chkScanAllFolders.Checked then
       Files := TDirectory.GetFiles(Dir, '*.jam',
        TSearchOption.soAllDirectories)
      else
      Files := TDirectory.GetFiles(Dir, '*.jam',
        TSearchOption.soTopDirectoryOnly);
      for f in Files do
      begin
        if ItemExists(f) then
          Continue; // skip duplicates
        itm := TJamBatchItem.Create(f);

        itm.inputType := TJamPaletteDetector.Instance.Detect(f, True);

        if itm.inputType = jamGP3SW then
          itm.outputType := jamGP3HW;

        if itm.inputType = jamGP3HW then
          itm.outputType := jamGP3SW;

        if itm.inputType = jamGP2 then
          itm.outputType := jamGP3SW;

        itm.outputpath := ToggleGP3JamsFolder(f);
        itm.filename := ExtractFileName(f);

        BatchList.Add(itm);
      end;
      RefreshListView;
    end;
  finally
    dlg.Free;
  end;
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



procedure TJamBatchForm.ConvertJAM();
var
  i, j: integer;
begin

  tmpArr := BatchList.ToArray;

//
 TTask.run(procedure
  var
    i,x: integer; ThisItem: TJamBatchItem;
  FJamFile, FOldJamFile: TJamFile;
  FHWJamFile: THWJamFile;
  begin
  for i := 0 to High(tmpArr) do
  begin
    ThisItem := tmpArr[i]; // <-- capture per-iteration

    checkPath(thisitem.outputpath);

    if ThisItem.inputType = jamGP3HW then
    begin
      if ThisItem.outputType = jamGP3SW then
      begin
        FJamFile := TJamFile.Create;
        FHWJamFile := THWJamFile.Create;

        FHWJamFile.LoadFromFile(ThisItem.filepath);

        FJamFile.ConvertHWJam(FHWJamFile, False);

        FJamFile.SaveToFile(ThisItem.outputpath);

        FJamFile.Free;
        FHWJamFile.Free;
      end;

      if ThisItem.outputType = jamGP2 then
      begin
        FJamFile := TJamFile.Create;
        FHWJamFile := THWJamFile.Create;

        FHWJamFile.LoadFromFile(ThisItem.filepath);

        FJamFile.ConvertHWJam(FHWJamFile, True);

        FJamFile.SaveToFile(ThisItem.outputpath);

        FJamFile.Free;
        FHWJamFile.Free;
      end;
    end;

  if ThisItem.inputType = jamGP2 then
    begin
      if ThisItem.outputType = jamGP3SW then
      begin
        FJamFile := TJamFile.Create;
        FOldJamFile := TJamFile.Create;

        FOldJamFile.LoadFromFile(ThisItem.filepath, False);

        FJamFile.ConvertGpxJam(FOldJamFile, False);

        if chkdopalette.checked then
        for x := 0 to FJamfile.FEntries.count-1 do
        fjamfile.ZeroPalette(x);

        FJamFile.SaveToFile(ThisItem.outputpath);

        FJamFile.Free;
        FOldJamFile.Free;
      end;

      if ThisItem.outputType = jamGP2 then
      begin
        FJamFile := TJamFile.Create;
        FOldJamFile := TJamFile.Create;

        FOldJamFile.LoadFromFile(ThisItem.filepath, False);

        FJamFile.ConvertGpxJam(FOldJamFile, True);

        if chkdopalette.checked then
        for x := 0 to FJamfile.FEntries.count-1 do
        fjamfile.ZeroPalette(x);

        FJamFile.SaveToFile(ThisItem.outputpath);

        FJamFile.Free;
        FOldJamFile.Free;
      end;

      if thisItem.outputType = jamGP3HW then
      begin
        FHWJamFIle := THWJamFile.Create;
        FHWJamfile.ConvertGPxJam(thisitem.filepath);
        FHWJamfile.SaveToFile(thisitem.outputpath);
        FHWJamFile.free;
      end;

    end;

     if ThisItem.inputType = jamGP3SW then
    begin
      if ThisItem.outputType = jamGP3SW then
      begin
        FJamFile := TJamFile.Create;
        FOldJamFile := TJamFile.Create;

        FOldJamFile.LoadFromFile(ThisItem.filepath, False);

        FJamFile.ConvertGpxJam(FOldJamFile, False);

        if chkdopalette.checked then
        for x := 0 to FJamfile.FEntries.count-1 do
        fjamfile.ZeroPalette(x);

        FJamFile.SaveToFile(ThisItem.outputpath);

        FJamFile.Free;
        FOldJamFile.Free;
      end;

      if ThisItem.outputType = jamGP2 then
      begin
        FJamFile := TJamFile.Create;
        FOldJamFile := TJamFile.Create;

        FOldJamFile.LoadFromFile(ThisItem.filepath, False);

        FJamFile.ConvertGpxJam(FOldJamFile, True);

        if chkdopalette.checked then
        for x := 0 to FJamfile.FEntries.count-1 do
        fjamfile.ZeroPalette(x);

        FJamFile.SaveToFile(ThisItem.outputpath);

        FJamFile.Free;
        FOldJamFile.Free;
      end;

      if thisItem.outputType = jamGP3HW then
      begin
        FHWJamFIle := THWJamFile.Create;
        FHWJamfile.ConvertGPxJam(thisitem.filepath);
        FHWJamfile.SaveToFile(thisitem.outputpath);
        FHWJamFile.free;
      end;

    end;
    ThisItem.processed := True;
    OnItemProcessed(thisitem);
    end;
     end);



end;

procedure TJamBatchForm.DeleteItems1Click(Sender: TObject);
begin
btnDelClick(sender);
end;

procedure TJamBatchForm.edtFilenameChange(Sender: TObject);
begin

if lvbatch.SelCount > 1 then exit;

  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.btnRunClick(Sender: TObject);
begin
TTask.run(procedure begin convertjam; end);
end;

procedure TJamBatchForm.radioGP2Click(Sender: TObject);
begin
  radioGP3.checked := False;
  radioGP3HW.checked := False;
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.radioGP3Click(Sender: TObject);
begin
  radioGP2.checked := False;
  radioGP3HW.checked := False;
  ApplyDetailsToItems(GetSelectedBatchItems);

end;

procedure TJamBatchForm.radioGP3HWClick(Sender: TObject);
begin
  radioGP2.checked := False;
  radioGP3.checked := False;
  ApplyDetailsToItems(GetSelectedBatchItems);
end;

procedure TJamBatchForm.OnItemProcessed(Item: TJamBatchItem);
begin

   BatchList.Remove(Item);
   RefreshListView;
end;

function TJamBatchForm.GetJamType(jamType: TJamType): string;
begin

  if jamType = jamGP2 then
    Result := 'GP2 JAM';

  if jamType = jamGP3SW then
    Result := 'GP3 Software JAM';

  if jamType = jamGP3HW then
    Result := 'GP3 Hardware JAM';

end;

function TJamBatchForm.GetSimplifyOptions(opts: TTextureSimpOptions): string;
begin

  if opts = quad then
    Result := 'Quad Tree Method (Blocky)';

  if opts = seed then
    Result := 'Seed Threshold';

  if opts = mean then
    Result := 'Mean Region';

  if opts = neighbour then
    Result := 'Neighbour Threshold (Usually best)';

end;

procedure TJamBatchForm.RefreshListView;
var
  i: integer;
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
      li.SubItems.Add(itm.outputpath + itm.filename);
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
  li.SubItems[2] := itm.outputpath + '\' + itm.filename;
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
    edtfilename.text := jamItem.filename;
  end
  else
    edtOutputPath.Text := '';

  // OutputType
  if single then
  begin
    x := Ord(jamItem.outputType);
    if x = 0 then
      radioGP2.checked := True;
    if x = 1 then
      radioGP3.checked := True;
    if x = 2 then
      radioGP3HW.checked := True;
  end;

  // texture options...
  if single then
  begin
    cbSimplify.ItemIndex := Ord(jamItem.TextureOptions.Simplify);
    seBlur.Value := jamItem.TextureOptions.Blur;
    seThreshold.Value := Round(jamItem.TextureOptions.simplifyThresh);
    chkDoPalette.checked := jamItem.TextureOptions.doPals;
    chkDoMatte.checked := jamItem.TextureOptions.doSoftenMatte;
    chkSimpPalette.checked := jamItem.TextureOptions.doSimplePal;
    chkDoPalette.checked := jamItem.TextureOptions.doPals;

  end
  else
  begin
    cbSimplify.ItemIndex := -1;
    seBlur.Value := 0;
    seThreshold.Value := 0;
    chkDoPalette.checked := False;
    chkDoMatte.checked := False;
    chkSimpPalette.checked := False;
      edtfilename.text := '';
      edtOutputPath.text := '';
      strjamfile.text := '';
  end;
end;

// write any changed detail back to *all* selected items
procedure TJamBatchForm.ApplyDetailsToItems(const Items: TArray<TJamBatchItem>);
var
  jamItem: TJamBatchItem;
begin
  // jamGP2, jamGP3SW, jamGP3HW
  for jamItem in Items do
  begin
    if edtOutputPath.Focused then
     if lvbatch.SelCount > 1 then
     jamItem.outputpath := edtOutputPath.Text
     else
      jamItem.outputpath := edtOutputPath.Text;

     if edtfilename.Focused then
     if lvbatch.SelCount > 1 then
     jamItem.filename := edtfilename.Text
     else
      jamItem.filename := edtfilename.Text;

    if radioGP2.Focused then
      if radioGP2.checked then
        jamItem.outputType := jamGP2;

    if radioGP3.Focused then
      if radioGP3.checked then
        jamItem.outputType := jamGP3SW;

    if radioGP3HW.Focused then
      if radioGP3HW.checked then
        jamItem.outputType := jamGP3HW;

    if cbSimplify.Focused then
      jamItem.TextureOptions.Simplify := TTextureSimpOptions(cbSimplify.ItemIndex);
    if seBlur.Focused then
      jamItem.TextureOptions.Blur := Round(seBlur.Value);
    if seThreshold.Focused then
      jamItem.TextureOptions.simplifyThresh := Round(seThreshold.Value);
    if chkDoPalette.Focused then
      jamItem.TextureOptions.doPals := chkDoPalette.checked;
    if chkDoMatte.Focused then
      jamItem.TextureOptions.doSoftenMatte := chkDoMatte.checked;
    if chkSimpPalette.Focused then
      jamItem.TextureOptions.doSimplePal := chkSimpPalette.checked;
  end;
  RefreshSelectedItems;

  updateDir := False;
  tempDir := '';
end;

initialization

InitBatchList;

finalization

FreeBatchList;

end.
