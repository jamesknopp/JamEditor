unit JamHW;

interface

uses
  // System
  System.SysUtils, System.Classes, System.Generics.Collections,
  // Windows/VCL
  Winapi.Windows, Vcl.Graphics,
  // Project
  GeneralHelpers, JamGeneral, JamPalette, jampalettedetector;

type

  THWJamTempDimensions = record
    x: integer;
    y: integer;
    height: integer;
    width: integer;
  end;

  THWJamHeaderArray = array of THWJamEntryInfo;

  THWJamEntry = class
  public
    FInfo: THWJamEntryInfo;
    FTexture: TBitmap;
    FOriginalTex: TBitmap;
    boolImportedBMP: boolean;
    tempDimensions: THWJamTempDimensions;



    procedure LoadFromStream(Stream : TStream);
    procedure SaveToStream(Stream : TStream);

    function Clone: THWJamEntry;

    constructor Create(const Info: THWJamEntryInfo); overload;
    destructor Destroy; override;
    // in the class declaration:
    property Info: THWJamEntryInfo read FInfo write FInfo;
  end;

  THWJamFile = class
  private

  public
    FHeader: THWJamHeader;
    FEntries: TList<THWJamEntry>;

    canvasHeight, canvasWidth: integer;
    CanvasBitmap: TBitmap;
    JamFileName: string;
    JamFullPath: string;

    constructor Create;
    destructor Destroy; override;

    function Clone: THWJamFile;


    function AddTexture(bmp: TBitmap; newinfo: TJamEntryInfo): integer overload;
    procedure AddTexture(textureFilename: string)overload;

    function LoadFromFile(const FileName: string): boolean;
    procedure SaveToFile(const FileName: string);

    procedure CreateNewHWJam(FileName: string; height: integer);

    procedure DeleteTexture(JamId: integer);

    function GetNextJamID(JamEntries: TList<THWJamEntry>): Word;

    procedure ImportTexture(JamId: integer; textureFilename: string);
    procedure ExportTexture(JamId: integer; textureFilename: string);

    procedure PackHWJAMHeader(const Hdr: THWJamEntryInfo; Dest: PWord);
    procedure ParseHWJAMHeader(const Src: PWord; out Hdr: THWJamEntryInfo);

    procedure WriteRLE(const Data: TArray<Word>; Stream: TStream);

    function DrawCanvas(uiUpdate: boolean): TBitmap;
    function DrawSingleTexture(JamId: integer): TBitmap;
    function DrawOutlines(JamCanvas: TBitmap): TBitmap;

    procedure ConvertGPxJam(origJamPath: string);

    property Entries: TList<THWJamEntry> read FEntries write FEntries;
  end;

implementation

uses JamSW;

constructor THWJamEntry.Create(const Info: THWJamEntryInfo);
begin
  FInfo := Info;
  FTexture := TBitmap.Create;
  FOriginalTex := TBitmap.Create;
end;

destructor THWJamEntry.Destroy;
begin

  if assigned(FOriginalTex) then
    freeandnil(FOriginalTex);

  if assigned(FTexture) then
    freeandnil(FTexture);

  inherited;
end;

function THWJamEntry.Clone: THWJamEntry;
begin
  Result := THWJamEntry.Create;

  // Copy simple values
  Result.FInfo := Self.FInfo;
  Result.boolImportedBMP := Self.boolImportedBMP;
  Result.tempDimensions := Self.tempDimensions;

  // Clone FTexture
  if Assigned(Self.FTexture) then
  begin
    Result.FTexture := TBitmap.Create;
    Result.FTexture.Assign(Self.FTexture);
  end;

  // Clone FOriginalTex
  if Assigned(Self.FOriginalTex) then
  begin
    Result.FOriginalTex := TBitmap.Create;
    Result.FOriginalTex.Assign(Self.FOriginalTex);
  end;
end;


constructor THWJamFile.Create;
begin
  FEntries := TList<THWJamEntry>.Create;
end;

destructor THWJamFile.Destroy;
var
  i: integer;
begin
  if assigned(FEntries) then
  begin
    for i := 0 to FEntries.Count - 1 do
      freeandnil(FEntries[i]);
    freeandnil(FEntries);
  end;

  freeandnil(FEntries);
  freeandnil(CanvasBitmap);

  inherited;
end;

procedure THWJamEntry.SaveToStream(Stream: TStream);
var
  I, L: Integer;
begin
  // Save FInfo
  Stream.WriteBuffer(FInfo, SizeOf(FInfo));


  // Save FTexture
  FTexture.SaveToStream(Stream);

  // Save FOriginalTex
  FOriginalTex.SaveToStream(Stream);


  // Save TempDimensions
  Stream.WriteBuffer(TempDimensions, SizeOf(TempDimensions));

  // Save boolImportedBMP
  Stream.WriteBuffer(boolImportedBMP, SizeOf(boolImportedBMP));
end;

procedure THWJamEntry.LoadFromStream(Stream: TStream);
var
  I, L: Integer;
begin
  // Read FInfo
  Stream.ReadBuffer(FInfo, SizeOf(FInfo));


  // Read FTexture
  FreeAndNil(FTexture);
  FTexture := TBitmap.Create;
  FTexture.LoadFromStream(Stream);

  // Read FOriginalTex
  FreeAndNil(FOriginalTex);
  FOriginalTex := TBitmap.Create;
  FOriginalTex.LoadFromStream(Stream);


  // Read TempDimensions
  Stream.ReadBuffer(TempDimensions, SizeOf(TempDimensions));

  // Read boolImportedBMP
  Stream.ReadBuffer(boolImportedBMP, SizeOf(boolImportedBMP));
end;

procedure THWJamFile.DeleteTexture(JamId: integer);
begin
  FEntries.Delete(JamId);
  Dec(FHeader.NumItems);
end;

function THWJamFile.Clone: THWJamFile;
var
  i: Integer;
  Entry: THWJamEntry;
begin
  Result := THWJamFile.Create;

  // Copy basic fields
  Result.FHeader := Self.FHeader;
  Result.canvasWidth := Self.canvasWidth;
  Result.canvasHeight := Self.canvasHeight;
  Result.JamFileName := Self.JamFileName;
  Result.JamFullPath := Self.JamFullPath;

  // Clone canvas bitmap
  if Assigned(Self.CanvasBitmap) then
  begin
    Result.CanvasBitmap := TBitmap.Create;
    Result.CanvasBitmap.Assign(Self.CanvasBitmap);
  end;

  // Deep copy entries
  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := FEntries[i].Clone; // requires THWJamEntry.Clone
    Result.FEntries.Add(Entry);
  end;
end;

procedure THWJamFile.ParseHWJAMHeader(const Src: PWord;
  out Hdr: THWJamEntryInfo);
type
  PWordArray = ^TWordArray;
  TWordArray = array [0 .. MaxInt div SizeOf(Word) - 1] of Word;
var
  A: PWordArray;
  i: integer;
begin
  A := PWordArray(Src);
  // reconstruct PosRaw
  Hdr.PosRaw := integer(A^[0]) or (integer(A^[1]) shl 16);
  // decode X,Y
  Hdr.x := (Hdr.PosRaw and $1FF) div 2;
  Hdr.y := Hdr.PosRaw div 512;
  // direct fields
  Hdr.width := A^[2];
  Hdr.height := A^[3];
  Hdr.JamId := A^[9];
  Hdr.FrameCountExp := A^[6];
  // derived
  Hdr.NumFrames := 1 shl Hdr.FrameCountExp;
  Hdr.jamFlags := A^[10];
  // unpack flags
  for i := 0 to 15 do
    Hdr.FrameFlags[i] := (Hdr.jamFlags and (1 shl i)) <> 0;
end;

function THWJamFile.GetNextJamID(JamEntries: TList<THWJamEntry>): Word;
var
  I: Integer;
  MaxID: Word;
begin
  MaxID := 0;
  for I := 0 to JamEntries.Count - 1 do
  begin
    if JamEntries[I].FInfo.JamID > MaxID then
      MaxID := JamEntries[I].FInfo.JamID;
  end;

  if MaxID < High(Word) then
    Result := MaxID + 1
  else
    raise Exception.Create('Maximum JamID value (65535) exceeded');
end;


procedure THWJamFile.PackHWJAMHeader(const Hdr: THWJamEntryInfo; Dest: PWord);
var
  RawPos: integer;
  A: array [0 .. 15] of Word;
  j: integer;
begin
  // Reconstruct PosRaw from X and Y
  // X = (PosRaw and $1FF) div 2  => PosRaw low 9 bits = X*2
  // Y = PosRaw div 512
  RawPos := (Hdr.y * 512) + (Hdr.x * 2);

  A[0] := Word(RawPos and $FFFF); // low word
  A[1] := Word((RawPos shr 16) and $FFFF); // high word
  A[2] := Hdr.width;
  A[3] := Hdr.height;
  A[4] := 0;
  A[5] := 0;
  A[6] := Hdr.FrameCountExp;
  A[7] := 0;
  A[8] := 0;
  A[9] := Hdr.JamId;
  A[10] := Hdr.jamFlags;
  for j := 11 to 15 do
    A[j] := 0;
  Move(A, Dest^, SizeOf(A));
end;

function THWJamFile.LoadFromFile(const FileName: string): boolean;
type
  PWordArray = ^TWordArray;
  TWordArray = array [0 .. MaxInt div SizeOf(Word) - 1] of Word;
var
  A: PWordArray;
  fs: TFileStream;
  magic: Cardinal;
  i, j, totalWords, dst, row, col: integer;
  ctrl: Byte;
  w: Word;
  raw: TArray<Word>;
  rawSrc: PWord;
  Info: THWJamEntryInfo;
  BlockCount, TrueSize: integer;
  sFilename: string;
  ScanPtr: PRGBTriple;
  c: Word;
begin
  result := false;

  intjamMaxWidth := 256;

  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));

  JamFileName := sFilename;

  // Read & decrypt all file bytes
  if not FileExists(FileName) then
    Exit;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

  try
    fs.ReadBuffer(magic, SizeOf(magic));
    if magic <> JAM_HW_MAGIC then
      Exit;

    fs.ReadBuffer(FHeader.NumItems, SizeOf(Word));
    fs.ReadBuffer(FHeader.JamTotalHeight, SizeOf(Word));

    totalWords := (FHeader.NumItems shl 4) + (FHeader.JamTotalHeight shl 8);
    SetLength(raw, totalWords);
    dst := 0;
    while dst < totalWords do
    begin
      fs.ReadBuffer(ctrl, 1);
      if (ctrl and $80) <> 0 then
      begin
        ctrl := ctrl and $7F;
        fs.ReadBuffer(raw[dst], ctrl * SizeOf(Word));
        Inc(dst, ctrl);
      end
      else if ctrl > 0 then
      begin
        fs.ReadBuffer(w, SizeOf(w));
        for i := 1 to ctrl do
        begin
          raw[dst] := w;
          Inc(dst);
        end;
      end;
    end;

    // there is always one texture - lets ensure that

    if FHeader.NumItems = 0 then
      FHeader.NumItems := 1;

    // Read entries and add them to the list
    for i := 0 to FHeader.NumItems - 1 do
    begin
      FEntries.Add(THWJamEntry.Create(Info));

      with FEntries[i] do
      begin

        A := PWordArray(@raw[i * 16]);

        FInfo.PosRaw := integer(A^[0]) or (integer(A^[1]) shl 16);
        FInfo.x := (FInfo.PosRaw and $1FF) div 2;
        FInfo.y := FInfo.PosRaw div 512;
        FInfo.width := A^[2];
        FInfo.height := A^[3];
        FInfo.JamId := A^[9];
        FInfo.FrameCountExp := A^[6];
        FInfo.NumFrames := 1 shl FInfo.FrameCountExp;
        FInfo.jamFlags := A^[10];

        for j := 0 to 15 do
          FInfo.FrameFlags[j] := (FInfo.jamFlags and (1 shl i)) <> 0;
      end;
    end;

    intJamMaxHeight := FHeader.JamTotalHeight;

    // Write Canvas for the first time

    CanvasBitmap := TBitmap.Create;

    with CanvasBitmap do
    begin
      PixelFormat := pf24bit;
      width := 256;
      height := FHeader.JamTotalHeight;
    end;

    for row := 0 to CanvasBitmap.height - 1 do
    begin
      ScanPtr := CanvasBitmap.ScanLine[row];
      for col := 0 to CanvasBitmap.width - 1 do
      begin
        c := raw[FHeader.NumItems * 16 + row * 256 + col];

        // unpack 5/6/5 → 8/8/8
        ScanPtr^.rgbtRed := Byte((c and $F800) shr 11) * 255 div $1F;
        ScanPtr^.rgbtGreen := Byte((c and $07E0) shr 5) * 255 div $3F;
        ScanPtr^.rgbtBlue := Byte(c and $001F) * 255 div $1F;

        Inc(ScanPtr);
      end;
    end;
    for i := 0 to FHeader.NumItems - 1 do
      with FEntries[i] do
      begin

        FTexture := DrawSingleTexture(i);
        FOriginalTex.Assign(FTexture);
      end
  finally
    fs.free;
  end;

  JamFullPath := FileName;
  result := True;
end;

procedure THWJamFile.CreateNewHWJam(FileName: string; height: integer);
var
  sFilename: string;

begin

  intjamMaxWidth := 256;

  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));
  JamFullPath := '';

  FHeader.NumItems := 0;

  FHeader.JamTotalHeight := height;

  intJamMaxHeight := FHeader.JamTotalHeight;

  // Write Canvas for the first time

  CanvasBitmap := TBitmap.Create;
  with CanvasBitmap do
  begin
    PixelFormat := pf24bit;
    width := 256;
    height := FHeader.JamTotalHeight;
    Canvas.Brush.Color := TCol_TransGP3HW;
    Canvas.FillRect(Rect(0, 0, CanvasBitmap.width, CanvasBitmap.height));
  end;

  JamFileName := FileName;
  boolJamLoaded := True;

end;

procedure THWJamFile.WriteRLE(const Data: TArray<Word>; Stream: TStream);
var
  i, runLen: integer;
  current: Word;
  header: Byte;
begin
  i := 0;
  while i < Length(Data) do
  begin
    current := Data[i];
    // count how many times it repeats (max 127)
    runLen := 1;
    while (i + runLen < Length(Data)) and (Data[i + runLen] = current) and
      (runLen < 127) do
      Inc(runLen);
    if runLen > 1 then
    begin
      // write a repeat block (high bit clear)
      header := Byte(runLen);
      Stream.WriteBuffer(header, 1);
      Stream.WriteBuffer(current, SizeOf(current));
      Inc(i, runLen);
    end
    else
    begin
      // write a literal block (high bit set)
      runLen := 1;
      while (i + runLen < Length(Data)) and (runLen < 127) and
        ((i + runLen + 1 >= Length(Data)) or
        (Data[i + runLen] <> Data[i + runLen + 1])) do
        Inc(runLen);
      header := Byte(runLen or $80);
      Stream.WriteBuffer(header, 1);
      Stream.WriteBuffer(Data[i], runLen * SizeOf(Word));
      Inc(i, runLen);
    end;
  end;
end;

procedure THWJamFile.SaveToFile(const FileName: string);
var
  fs: TFileStream;
  hdrCount, heightVal: Word;
  raw: TArray<Word>;
  totalWords: integer;
  i, row, col, offset: integer;
  v: Cardinal;
  w: Word;
  tmpTex: TBitmap;
begin

  hdrCount := FHeader.NumItems;
  heightVal := FHeader.JamTotalHeight;
  totalWords := (hdrCount shl 4) + (256 * heightVal);
  SetLength(raw, totalWords);

  // 1) pack each header into the first hdrCount*16 words
  for i := 0 to hdrCount - 1 do
    PackHWJAMHeader(FEntries[i].FInfo, @raw[i * 16]);

  // 2 - draw brand new canvas
  CanvasBitmap.Canvas.lock;

  for i := 0 to hdrCount - 1 do
  begin
    tmpTex := stretchF(FEntries[i].FOriginalTex, FEntries[i].FInfo.width,
      FEntries[i].FInfo.height);
    try
      CanvasBitmap.Canvas.Draw(FEntries[i].FInfo.x,
        FEntries[i].FInfo.y, tmpTex);
    finally
      tmpTex.free;
    end;

  end;

  // 3) pack pixel data into raw[hdrCount*16..]
  offset := hdrCount * 16;
  for row := 0 to heightVal - 1 do
    for col := 0 to 256 - 1 do
    begin
      raw[offset] := PackRGB565(CanvasBitmap.Canvas.Pixels[col, row]);
      Inc(offset);
    end;

  CanvasBitmap.Canvas.Unlock;
  // 3) write to disk: magic, counts, then RLE data
  fs := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    v := JAM_HW_MAGIC;
    fs.WriteBuffer(v, SizeOf(v));

    w := hdrCount;
    fs.WriteBuffer(w, SizeOf(w));

    w := heightVal;
    fs.WriteBuffer(w, SizeOf(w));

    WriteRLE(raw, fs);

  finally
    fs.free;
  end;
end;

procedure THWJamFile.ImportTexture(JamId: integer; textureFilename: string);
var
  srcPic: TPicture;
  textureWidth, textureHeight: integer;
  tmpCanvas: TBitmap;
begin

  srcPic := TPicture.Create;
  textureWidth := FEntries[JamId].Info.width;
  textureHeight := FEntries[JamId].Info.height;

  try
    tmpCanvas.Canvas.Lock;
    tmpCanvas := TBitmap.Create;
    tmpCanvas.PixelFormat := srcPic.bitmap.PixelFormat;
    srcPic.LoadFromFile(textureFilename);
    // auto‐detects BMP, JPEG, PNG, etc.

    tmpCanvas.SetSize(textureWidth, textureHeight);

    tmpCanvas.Canvas.stretchdraw(Rect(0, 0, textureWidth, textureHeight),
      stretchF(srcPic.bitmap, textureWidth, textureHeight));

    FEntries[JamId].FOriginalTex.Assign(tmpCanvas);
    FEntries[JamId].FTexture.Assign(tmpCanvas); // Rebuild and store texture

    tmpCanvas.Canvas.UnLock;
  finally
    srcPic.free;
  end;

end;

procedure THWJamFile.ExportTexture(JamId: integer; textureFilename: string);
var
  srcPic: TPicture;
  textureWidth, textureHeight: integer;
  tmpCanvas: TBitmap;
begin

  tmpCanvas := FEntries[JamId].FTexture;
  tmpCanvas.SaveToFile(textureFilename);

end;

function THWJamFile.DrawCanvas(uiUpdate: boolean): TBitmap;
var
  bmpAll, entryBmp, stretchedBmp: TBitmap;
  i: integer;
begin
  // Create the master canvas
  bmpAll := TBitmap.Create;
  try
    bmpAll.Canvas.Lock;
    bmpAll.PixelFormat := pf32bit;
    bmpAll.width := 256;
    bmpAll.height := FHeader.JamTotalHeight;
    bmpAll.Canvas.Brush.Color := TCol_TransGP3HW;
    bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.width, bmpAll.height));

    // Draw each entry
    for i := 0 to FEntries.Count - 1 do
    begin
      if uiUpdate then
      begin
        // Use the cached original texture, scale it, draw, then free
        entryBmp := FEntries[i].FOriginalTex;
        stretchedBmp := stretchF(entryBmp, FEntries[i].Info.width,
          FEntries[i].Info.height);
        try
          bmpAll.Canvas.Draw(FEntries[i].Info.x, FEntries[i].Info.y,
            stretchedBmp);
        finally
          stretchedBmp.free;
        end;
      end
      else
      begin
        // Generate a one‐off snapshot, draw it, then free
        entryBmp := DrawSingleTexture(i);
        try
          bmpAll.Canvas.Draw(FEntries[i].Info.x, FEntries[i].Info.y, entryBmp);
        finally
          entryBmp.free;
        end;
      end;
    end;

    bmpAll.canvas.unlock;
    // Success—transfer ownership of bmpAll to the caller
    result := bmpAll;
  except
    // On any exception, free bmpAll and re‐raise
    bmpAll.free;
    raise;
  end;
end;

function THWJamFile.DrawSingleTexture(JamId: integer): TBitmap;
var
  w, H, x, y: integer;
  dst: TBitmap;
begin
  // 1) Pull out rectangle info
  w := FEntries[JamId].FInfo.width;
  H := FEntries[JamId].FInfo.height;
  x := FEntries[JamId].FInfo.x;
  y := FEntries[JamId].FInfo.y;

  // 2) Sanity‐check
  if (w <= 0) or (H <= 0) then
    raise Exception.CreateFmt('Invalid texture size %dx%d for entry %d',
      [w, H, JamId]);

  // 3) Create and ensure it gets freed on error
  dst := TBitmap.Create;
  try
    dst.canvas.lock;
    dst.PixelFormat := CanvasBitmap.PixelFormat;
    dst.width := w;
    dst.height := H;


    // 4) Copy pixels
    if not BitBlt(dst.Canvas.Handle, 0, 0, w, H, CanvasBitmap.Canvas.Handle, x,
      y, SRCCOPY) then
      raise Exception.Create('BitBlt failed in DrawSingleTexture');

    // 5) Hand off ownership
    dst.canvas.unlock;
    result := dst;
    dst := nil; // prevent the finally block from freeing it
  finally
    dst.free; // frees only if an exception occurred or Dst wasn't handed off
  end;
end;

function THWJamFile.DrawOutlines(JamCanvas: TBitmap): TBitmap;
var
  i: integer;
  tmpBMP: TBitmap;
begin
  if not boolDrawOutlines then
    Exit(JamCanvas);

  // Clone the source
  tmpBMP := TBitmap.Create;
  tmpBMP.Assign(JamCanvas);
  try
    // Draw outlines on the clone
    for i := 0 to FEntries.Count - 1 do
      with FEntries[i].FInfo do
        DrawTextureOutlines(tmpBMP, x, y, width, height, i, JamId);

    result := tmpBMP;
    tmpBMP := nil; // ownership transferred to Result
  finally
    tmpBMP.free; // frees only if an exception occurred
  end;
end;

procedure THWJamFile.AddTexture(textureFilename: string);
var
  srcPic: TPicture;
  Info: THWJamEntryInfo;
  newTex: THWJamEntry;
  x: integer;
  scaledCanvas: TBitmap;
  transbool: boolean;
begin
  srcPic := TPicture.Create;

  scaledCanvas := nil;
  try
    srcPic.LoadFromFile(textureFilename);
    Info.x := 0;
    Info.y := 0;
    Info.width := srcPic.width;
    Info.height := srcPic.height;
    if FEntries.Count > 0 then
      Info.JamId := FEntries[FEntries.Count - 1].Info.JamId + 1
    else
      Info.JamId := 0;

    newTex := THWJamEntry.Create(Info);
    newTex.FOriginalTex := TBitmap.Create;
    newTex.FTexture := TBitmap.Create;

    newTex.FOriginalTex.SetSize(Info.width, Info.height);
    newTex.FTexture.SetSize(Info.width, Info.height);

    transbool := DetectTransCol(srcPic.bitmap);

    srcPic.bitmap := ReplaceTransparentColour(srcPic.bitmap, TCol_TransGP3HW);

    newTex.FOriginalTex.Assign(srcPic.bitmap);
    newTex.FTexture.Assign(srcPic.bitmap);

    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    x := FEntries.Count - 1;
    intSelectedTexture := x;

    FEntries[x].FInfo.jamFlags := 0;

    if not isPowerOfTwo(FEntries[x].FTexture.width) then
      FEntries[x].FInfo.jamFlags := PackFlag(FEntries[x].FInfo.jamFlags, 9);

    if not isPowerOfTwo(FEntries[x].FTexture.height) then
      FEntries[x].FInfo.jamFlags := PackFlag(FEntries[x].FInfo.jamFlags, 8);

    if transbool then
      FEntries[x].FInfo.jamFlags := PackFlag(FEntries[x].FInfo.jamFlags, 3);

  finally
    srcPic.free;

  end;
end;

function THWJamFile.AddTexture(bmp: TBitmap; newinfo: TJamEntryInfo): integer;
var
  Info: THWJamEntryInfo;
  newTex: THWJamEntry;
  x, y: integer;
  tmpCanvas, scaledCanvas: TBitmap;
  transbool: boolean;
begin
  tmpCanvas := TBitmap.Create;
  tmpCanvas.Assign(bmp);
  scaledCanvas := nil;
  try

    Info.x := newinfo.x;
    Info.y := newinfo.y;
    Info.width := newinfo.width;
    Info.height := newinfo.height;
    Info.JamId := newinfo.JamId;
    Info.jamFlags := newinfo.jamFlags;

    newTex := THWJamEntry.Create(Info);
    newTex.FOriginalTex := TBitmap.Create;
    newTex.FTexture := TBitmap.Create;

    tmpCanvas := ReplaceTransparentColour(tmpCanvas, TCol_TransGP3HW);

    tmpCanvas.PixelFormat := pf24bit;

    newTex.FOriginalTex.SetSize(Info.width, Info.height);
    newTex.FTexture.SetSize(Info.width, Info.height);

    newTex.FOriginalTex.Assign(tmpCanvas);
    newTex.FTexture.Assign(tmpCanvas);

    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    x := FEntries.Count - 1;
    intSelectedTexture := x;

    if not isPowerOfTwo(FEntries[x].FTexture.width) then
      FEntries[x].FInfo.jamFlags := PackFlag(FEntries[x].FInfo.jamFlags, 9);

    if not isPowerOfTwo(FEntries[x].FTexture.height) then
      FEntries[x].FInfo.jamFlags := PackFlag(FEntries[x].FInfo.jamFlags, 8);

    if transbool then
      FEntries[x].FInfo.jamFlags := PackFlag(FEntries[x].FInfo.jamFlags, 3);

  finally
    tmpCanvas.free;
    scaledCanvas.free;
  end;
end;

procedure THWJamFile.ConvertGPxJam(origJamPath: string);
var
  origJam: TJamFile;
  jamType: TJamType;

  i: integer;
begin

  origJam := TJamFile.Create;

  jamType := TJamPaletteDetector.Instance.Detect(origJamPath, false);
  case jamType of
    jamGP2:
      begin
        origJam.SetGpxPal(true)
      end;
    jamGP3SW:
      begin
        origJam.SetGpxPal(false)
      end;
  end;

  origJam.LoadFromFile(origJamPath, false);
  intjamMaxWidth := 256;

  FHeader.JamTotalHeight := origJam.FHeader.JamTotalHeight;

  intJamMaxHeight := FHeader.JamTotalHeight;

  CanvasBitmap := TBitmap.Create;

  // Write Canvas for the first time

  CanvasBitmap.Canvas.lock;
  with CanvasBitmap do
  begin
    PixelFormat := pf24bit;
    width := 256;
    height := FHeader.JamTotalHeight;
    Canvas.Brush.Color := TCol_TransGP3HW;
    Canvas.FillRect(Rect(0, 0, CanvasBitmap.width, CanvasBitmap.height));
  end;
  CanvasBitmap.Canvas.Unlock;

  for i := 0 to origJam.FHeader.NumItems - 1 do
    AddTexture(origJam.FEntries[i].FTexture, origJam.FEntries[i].FInfo);

  FHeader.NumItems := origJam.FHeader.NumItems;

  origJam.free;

end;

end.
