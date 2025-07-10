unit JamHW;

interface

uses
  // System
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  System.IOUtils, System.Math,
  // Windows/VCL
  Winapi.Windows, Vcl.Graphics, Vcl.Dialogs, Vcl.ComCtrls,
  // Project
  GeneralHelpers, JamGeneral, JamPalette;

type
  THWJamHeader = packed record
    NumItems: Word;
    JamTotalHeight: Word;
  end;


  THWJamEntryInfo = record
    PosRaw:         Integer;            // raw dword from words[0]+[1]<<16
    X, Y:           Integer;            // decoded Left/Top
    Width, Height:  Word;               // words[2], words[3]
    JamID:          Word;               // words[5]
    FrameCountExp:  Word;               // words[6]
    NumFrames:      Integer;            // = 1 shl FrameCountExp
    jamflags:       Word;               // words[10]
    FrameFlags:     array[0..15] of Boolean; // unpacked bits
  end;

  THWJamTempDimensions = record
  x : integer;
  y: integer;
  height : integer;
  width : integer;
  end;

  THWJamHeaderArray = array of THWJamEntryInfo;

  THWJamEntry = class
  public
    FInfo: THWJamEntryInfo;
    FTexture: TBitmap;
    FOriginalTex: TBitmap;
    boolImportedBMP: boolean;
    tempDimensions : THWJamTempDimensions;
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

    canvasHeight, canvasWidth: Integer;
    CanvasBitmap: TBitmap;
    JamFileName: string;

    constructor Create;
    destructor  Destroy; override;

    function AddTexture(bmp : TBitmap) : integer overload;
    procedure AddTexture(textureFilename : string) overload;

    function LoadFromFile(const FileName: string) : boolean;
    procedure SaveToFile(const FileName: string);

    procedure CreateNewHWJam(filename : string; height : integer);

    procedure ImportTexture(JamId: Integer; textureFilename: string);
    procedure ExportTexture(JamId: Integer; textureFilename: string);

    procedure PackHWJAMHeader(const Hdr: THWJamEntryInfo; Dest: PWord);
    procedure ParseHWJAMHeader(const Src: PWord; out Hdr: THWJamEntryInfo);

    procedure WriteRLE(const Data: TArray<Word>; Stream: TStream);

    function DrawCanvas(uiUpdate : boolean): TBitmap;
    function DrawSingleTexture(JamId: Integer): TBitmap;
    function DrawOutlines(JamCanvas: TBitmap): TBitmap;

    property Entries: TList<THWJamEntry> read FEntries write FEntries;
  end;

implementation

constructor THWJamEntry.Create(const Info: THWJamEntryInfo);
begin
  FInfo := Info;
end;

destructor THWJamEntry.Destroy;
begin
freeandnil(ftexture);
freeandnil(FOriginalTex);

  inherited;
end;

constructor THWJamFile.Create;
begin
  FEntries := TList<THWJamEntry>.Create;
end;

destructor THWJamFile.Destroy;
begin
freeandnil(canvasBitmap);
freeandnil(FEntries);
  inherited;
end;

procedure THWJamFile.ParseHWJAMHeader(const Src: PWord; out Hdr: THWJamEntryInfo);
type
  PWordArray = ^TWordArray;
  TWordArray = array[0..MaxInt div SizeOf(Word)-1] of Word;
var
  A: PWordArray;
  i: Integer;
begin
  A := PWordArray(Src);
  // reconstruct PosRaw
  Hdr.PosRaw        := Integer(A^[0]) or (Integer(A^[1]) shl 16);
  // decode X,Y
  Hdr.X             := (Hdr.PosRaw and $1FF) div 2;
  Hdr.Y             := Hdr.PosRaw div 512;
  // direct fields
  Hdr.Width         := A^[2];
  Hdr.Height        := A^[3];
  Hdr.JamID         := A^[9];
  Hdr.FrameCountExp := A^[6];
  // derived
  Hdr.NumFrames     := 1 shl Hdr.FrameCountExp;
  Hdr.jamFlags      := A^[10];
  // unpack flags
  for i := 0 to 15 do
    Hdr.FrameFlags[i] := (Hdr.jamFlags and (1 shl i)) <> 0;
end;

procedure THWJamFile.PackHWJAMHeader(const Hdr: THWJamEntryInfo; Dest: PWord);
var
  RawPos: Integer;
  A: array[0..15] of Word;
  j: Integer;
begin
  // Reconstruct PosRaw from X and Y
  // X = (PosRaw and $1FF) div 2  => PosRaw low 9 bits = X*2
  // Y = PosRaw div 512
  RawPos := (Hdr.Y * 512) + (Hdr.X * 2);

  A[0]  := Word(RawPos and $FFFF);         // low word
  A[1]  := Word((RawPos shr 16) and $FFFF); // high word
  A[2]  := Hdr.Width;
  A[3]  := Hdr.Height;
  A[4]  := 0;
  A[5]  := 0;
  A[6]  := Hdr.FrameCountExp;
  A[7]  := 0;
  A[8]  := 0;
  A[9]  := Hdr.JamID;
  A[10] := Hdr.jamFlags;
  for j := 11 to 15 do
    A[j] := 0;
  Move(A, Dest^, SizeOf(A));
end;

//
function THWJamFile.LoadFromFile(const FileName: string) : boolean;
type
  PWordArray = ^TWordArray;
  TWordArray = array[0..MaxInt div SizeOf(Word)-1] of Word;
var
  A: PWordArray;
  fs: TFileStream;
  magic: Cardinal;
  i, j, totalWords,dst,row,col: Integer;
  ctrl: Byte;
  w: word;
  raw: TArray<Word>;
  rawSrc: PWord;
  Info: THWJamEntryInfo;
  BlockCount, TrueSize: Integer;
  sFilename: string;
   ScanPtr: PRGBTriple;
  c: Word;
begin
  result := false;

  intjamMaxWidth := 256;

  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));

  // Read & decrypt all file bytes
  if not FileExists(FileName) then Exit;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

  try
    fs.ReadBuffer(magic, SizeOf(magic));
    if magic <> JAM_HW_MAGIC then Exit;

    fs.ReadBuffer(FHeader.NumItems, SizeOf(word));
    fs.ReadBuffer(FHeader.JamTotalHeight, SizeOf(word));

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
          raw[dst] := w; Inc(dst);
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


        A := PWordArray(@raw[i*16]);

        FInfo.PosRaw        := Integer(A^[0]) or (Integer(A^[1]) shl 16);
        FInfo.X             := (FInfo.PosRaw and $1FF) div 2;
        FInfo.Y             := FInfo.PosRaw div 512;
        FInfo.Width         := A^[2];
        FInfo.Height        := A^[3];
        FInfo.JamID         := A^[9];
        FInfo.FrameCountExp := A^[6];
        FInfo.NumFrames     := 1 shl FInfo.FrameCountExp;
        FInfo.jamFlags      := A^[10];

        for j  := 0 to 15 do
        FInfo.FrameFlags[j] := (FInfo.jamFlags and (1 shl i)) <> 0;
      end;
    end;

    intJamMaxHeight := FHeader.JamTotalHeight;

    // Write Canvas for the first time

    CanvasBitmap := TBitmap.Create;
    with CanvasBitmap do
    begin
      PixelFormat := pf24bit;
      Width       := 256;
      Height      := FHeader.JamTotalHeight;
    end;

    for row := 0 to CanvasBitmap.Height - 1 do
  begin
    ScanPtr := CanvasBitmap.ScanLine[row];
    for col := 0 to CanvasBitmap.Width - 1 do
    begin
      c := raw[FHeader.NumItems * 16 + row * 256 + col];

      // unpack 5/6/5 → 8/8/8
      ScanPtr^.rgbtRed   := Byte((c and $F800) shr 11) * 255 div $1F;
      ScanPtr^.rgbtGreen := Byte((c and $07E0) shr  5) * 255 div $3F;
      ScanPtr^.rgbtBlue  := Byte( c and $001F)         * 255 div $1F;

      Inc(ScanPtr);
    end;
  end;
    for i := 0 to FHeader.NumItems-1 do
      with FEntries[i] do
      begin

      FTexture := DrawSingleTexture(i);
      FOriginalTex := FTexture;
      end
  finally
    fs.free;
  end;

  JamFileName := FileName;
  boolJamLoaded := True;
  result := true;
end;

procedure THWJamFile.CreateNewHWJam(filename : string; height : integer);
var
sFilename : string;

begin

  intjamMaxWidth := 256;

  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));

  FHeader.NumItems := 0;

  FHeader.JamTotalHeight := height;

  intJamMaxHeight := FHeader.JamTotalHeight;

    // Write Canvas for the first time

  CanvasBitmap := TBitmap.Create;
    with CanvasBitmap do
    begin
      PixelFormat := pf24bit;
      Width       := 256;
      Height      := FHeader.JamTotalHeight;
      Canvas.Brush.Color := TCol_TransGP3HW;
      Canvas.FillRect(Rect(0, 0, CanvasBitmap.Width, CanvasBitmap.Height));
    end;

  JamFileName := FileName;
  boolJamLoaded := True;

end;

procedure THWJamFile.WriteRLE(const Data: TArray<Word>; Stream: TStream);
var
  i, runLen: Integer;
  current: Word;
  header: Byte;
begin
  i := 0;
  while i < Length(Data) do
  begin
    current := Data[i];
    // count how many times it repeats (max 127)
    runLen := 1;
    while (i + runLen < Length(Data)) and (Data[i+runLen] = current) and (runLen < 127) do
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
      while (i + runLen < Length(Data))
        and (runLen < 127)
        and ((i + runLen + 1 >= Length(Data)) or (Data[i+runLen] <> Data[i+runLen+1])) do
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
  totalWords: Integer;
  i, row, col, offset: Integer;
  v: Cardinal;
  w: Word;
  tmpTex : TBitmap;
begin

  hdrCount  := FHeader.NumItems;
  heightVal := FHeader.JamTotalHeight;
  totalWords := (hdrCount shl 4) + (256 * heightVal);
  SetLength(raw, totalWords);

  // 1) pack each header into the first hdrCount*16 words
  for i := 0 to hdrCount - 1 do
    PackHWJAMHeader(FEntries[i].FInfo, @raw[i*16]);


  // 2 - draw brand new canvas

  for i := 0 to hdrcount-1 do
  begin
  tmpTex := TBitmap.create;
  tmpTex := stretchF(fentries[i].FOriginalTex, fentries[i].FInfo.Width, fentries[i].Finfo.height);
  canvasbitmap.canvas.Draw(fentries[i].FInfo.X,fentries[i].FInfo.Y, tmpTex);
  freeandnil(tmptex)
  end;



  // 3) pack pixel data into raw[hdrCount*16..]
  offset := hdrCount * 16;
  for row := 0 to heightVal - 1 do
    for col := 0 to 256 - 1 do
    begin
      raw[offset] := PackRGB565(CanvasBitmap.Canvas.Pixels[col, row]);
      Inc(offset);
    end;

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
    fs.Free;
  end;
  end;


procedure THWJamFile.ImportTexture(JamId: Integer; textureFilename: string);
var
  srcPic: TPicture;
  textureWidth, textureHeight: Integer;
  tmpCanvas: TBitmap;
begin

  srcPic := TPicture.Create;
  textureWidth := FEntries[JamId].Info.Width;
  textureHeight := FEntries[JamId].Info.Height;

  try

    tmpCanvas := TBitmap.Create;
    tmpCanvas.PixelFormat := srcPic.bitmap.PixelFormat;
    srcPic.LoadFromFile(textureFilename);
    // auto‐detects BMP, JPEG, PNG, etc.

    tmpCanvas.SetSize(textureWidth, textureHeight);

    tmpCanvas.Canvas.stretchdraw(Rect(0, 0, textureWidth, textureHeight),
      StretchF(srcPic.bitmap, textureWidth, textureHeight));

    FEntries[JamId].FOriginalTex := tmpCanvas;
    FEntries[JamId].FTexture := tmpCanvas; // Rebuild and store texture

  finally
    srcPic.Free;
  end;

end;

procedure THWJamFile.ExportTexture(JamId: Integer; textureFilename: string);
var
  srcPic: TPicture;
  textureWidth, textureHeight: Integer;
  tmpCanvas: TBitmap;
begin

try

 tmpCanvas := TBitmap.create;

 tmpCanvas := FEntries[jamID].FTexture;

 tmpCanvas.savetoFile(texturefilename);
finally
 tmpcanvas.free;
end;



end;

function THWJamFile.DrawCanvas(uiUpdate : boolean) : TBitmap;
var
  bmpAll, entryBmp, stretchedBMP: TBitmap;
  i: Integer;
begin
  bmpAll := TBitmap.Create;
  try
    // configure master bitmap
    bmpAll.PixelFormat := pf32bit;
    bmpAll.Width  := 256;
    bmpAll.Height := fHeader.JamTotalHeight;
    bmpAll.Canvas.Brush.Color := TCol_TransGP3HW;
    bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.Width, bmpAll.Height));

    // draw each entry
    for i := 0 to FEntries.Count - 1 do
    begin

      if uiUpdate then
      begin
        entryBMP := FEntries[i].FOriginalTex;
        stretchedBmp := StretchF(entryBmp, FEntries[i].Info.Width, FEntries[i].Info.Height);
        bmpAll.Canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y, stretchedBmp);
      end
      else
      begin
        entryBmp := DrawSingleTexture(i);
        bmpAll.Canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y, entryBMP);
      end;
      end;

    // hand off ownership to caller
    Result := bmpAll;
  except
    bmpAll.Free;  // free on *any* exception
    raise;
  end;
end;


function THWJamFile.DrawSingleTexture(JamId: Integer): TBitmap;
var
  W, H, X, Y: Integer;
  Dst: TBitmap;
begin
  // 1) Pull out the rectangle info

    W := FEntries[JamId].FInfo.Width;
    H := FEntries[JamId].FInfo.Height;
    X := FEntries[JamId].FInfo.X;
    Y := FEntries[JamId].FInfo.Y;


  // 2) Sanity‐check dimensions
  if (W <= 0) or (H <= 0) then
    raise Exception.CreateFmt('Invalid texture size %dx%d for entry %d', [W, H, JamId]);

  // 3) Create & fill in a safe exception block
  Dst := TBitmap.Create;
  try
    Dst.PixelFormat := CanvasBitmap.PixelFormat;
    Dst.Width  := W;
    Dst.Height := H;

    // 4) Copy the pixels
    if not BitBlt(
         Dst.Canvas.Handle, 0, 0, W, H,
         CanvasBitmap.Canvas.Handle, X, Y,
         SRCCOPY
       ) then
      raise Exception.Create('BitBlt failed in DrawSingleTexture');

    // 5) On success, hand ownership to the caller
    Result := Dst;
  except
    Dst.Free;  // avoid leaking if anything here raises
    raise;
  end;
end;


function THWJamFile.DrawOutlines(JamCanvas: TBitmap): TBitmap;
var
  i, textW, textH: Integer;
  idText: string;
  rectX, rectY: Integer;
  X, Y, W, H: Integer;
  textRect: TRect;
  tmpBMP : TBitmap;
begin
  if not boolDrawOutlines then
    exit(JamCanvas);

  tmpBMP := TBitmap.create;
  tmpBMP := jamCanvas;

  for i := 0 to FEntries.Count - 1 do
  begin
   with FEntries[i] do
   DrawTextureOutlines(tmpBmp, info.x, info.y, info.width, info.height, i, info.JamId);
  end;

  Result := tmpBMP;
end;

procedure THWJamFile.AddTexture(textureFilename: string);
var
  srcPic: TPicture;
  Info: THWJamEntryInfo;
  newTex: THWJamEntry;
  x: integer;
  tmpCanvas, scaledCanvas: TBitmap;
begin
  srcPic := TPicture.Create;
  tmpCanvas := TBitmap.Create;
  scaledCanvas := nil;
  try
    srcPic.LoadFromFile(textureFilename);
    Info.X := 0;
    Info.Y := 0;
    Info.Width := srcPic.Width;
    Info.Height := srcPic.Height;
    if FEntries.Count > 0 then
      Info.JamId := FEntries[FEntries.Count - 1].Info.JamId + 1
    else
      Info.JamId := 0;

    newTex := THWJamEntry.Create(Info);
    newTex.FOriginalTex := TBitmap.Create;
    newTex.FTexture := TBitmap.Create;

    newTex.FOriginalTex.SetSize(Info.Width, Info.Height);
    newTex.FTexture.SetSize(Info.Width, Info.Height);

    newTex.FOriginalTex.Assign(srcPic.Bitmap);
    newTex.FTexture.Assign(srcPic.Bitmap);

    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    x := FEntries.Count - 1;
    intSelectedTexture := x;


  finally
    srcPic.Free;
    tmpCanvas.Free;
    scaledCanvas.Free;
  end;
end;

function THWJamFile.AddTexture(bmp: TBitmap) : integer;
var
  Info: THWJamEntryInfo;
  newTex: THWJamEntry;
  x: integer;
  tmpCanvas, scaledCanvas: TBitmap;
begin
  tmpCanvas := bmp;
  scaledCanvas := nil;
  try

    Info.X := 0;
    Info.Y := 0;
    Info.Width := bmp.Width;
    Info.Height := bmp.Height;
    if FEntries.Count > 0 then
      Info.JamId := FEntries[FEntries.Count - 1].Info.JamId + 1
    else
      Info.JamId := 0;

    newTex := THWJamEntry.Create(Info);
    newTex.FOriginalTex := TBitmap.Create;
    newTex.FTexture := TBitmap.Create;

    newTex.FOriginalTex.SetSize(Info.Width, Info.Height);
    newTex.FTexture.SetSize(Info.Width, Info.Height);

    scaledCanvas := StretchF(bmp, Info.Width, Info.Height);

    newTex.FOriginalTex.Assign(scaledCanvas);
    newTex.FTexture.Assign(scaledCanvas);


    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    x := FEntries.Count - 1;
    intSelectedTexture := x;

    FEntries[x].FTexture := scaledCanvas;
    FEntries[x].FOriginalTex := scaledCanvas;

  finally
    tmpCanvas.Free;
    scaledCanvas.Free;
  end;

  result := x;
end;



end.
