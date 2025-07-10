unit JamSW;

interface

uses
  // System
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  System.IOUtils, System.Math, System.StrUtils,
  // Windows/VCL
  Winapi.Windows, Vcl.Graphics, Vcl.Dialogs, Vcl.ComCtrls,
  // Project
  GeneralHelpers, JamGeneral, JamPalette, JamHW, GP2Mask;

type
  TJamHeader = packed record
    NumItems: Word;
    JamTotalHeight: Word;
  end;

  TJamEntryInfo = packed record
    X: Byte;
    Y: Word;
    Unk: Byte; // totally unk?
    Width: Word;
    Height: Word;
    Idx08: Word; // scale origin x & y?
    Idx0A: Word; // scaling flags & factor?
    ImagePtr: Word;
    Idx0E: Word; // totally unk
    PaletteSizeDiv4: Word;
    JamId: Word;
    JamFlags: Word; // flags
    Idx16: Byte; // untex color 1; entry in palette
    Idx17: Byte; // untext color 2; entry in palette
    Idx18: array [0 .. 7] of Byte;
    // 3,4 and 6 seem to be used in car liveries (gp3)... maybe load up all JAM files and create CSVs with all the data to review??
  end;

  TJamTempDimensions = record
    X: integer;
    Y: integer;
    Height: integer;
    Width: integer;
  end;

  TJamEntry = class
  public
    FInfo: TJamEntryInfo;
    FPalettes: TLocalPaletteArray;
    FTexture: TBitmap;
    FOriginalTex: TBitmap;
    FRawTexture: TBytes;
    FCachedTex: array [0 .. 3] of TBitmap;
    TempDimensions: TJamTempDimensions;
    boolImportedBMP: boolean;
    function GetPalette(Index: integer): TBytes;
    function GetPaletteSizeDiv4: Word;
    procedure SetPaletteSizeDiv4(const Value: Word);
    constructor Create(const Info: TJamEntryInfo); overload;
    destructor Destroy; override;
    // in the class declaration:
    property Info: TJamEntryInfo read FInfo write FInfo;
    property Palettes: TLocalPaletteArray read FPalettes write FPalettes;
    property PaletteSizeDiv4: Word read GetPaletteSizeDiv4
      write SetPaletteSizeDiv4;

  end;

  TJamFile = class
  private

  public
    FHeader: TJamHeader;
    FEntries: TList<TJamEntry>;
    FRawData: TBytes;
    LevelIdx: array [0 .. 3] of TBitmap;
    PalPerLevel: array [0 .. 3] of TArray<TColor>;
    canvasHeight, canvasWidth: integer;
    CanvasBitmap: TBitmap;
    JamFileName: string;

    function UnJam(const Data: TBytes): TBytes;
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const FileName: string; preview: boolean): boolean;
    procedure SaveToFile(const FileName: string);

    procedure CreateNewJAM(FileName: string; Height: integer);

    procedure DecryptSWJam(const FileName: string);

    procedure EncodeTexture(JamId: integer; texture: TBitmap);
    procedure EncodeCanvas(JamId: integer);

    function AddTexture(bmp: TBitmap): integer overload;
    procedure DeleteTexture(JamId: integer);

    procedure AddTexture(textureFilename: string)overload;

    procedure ImportTexture(JamId: integer; textureFilename: string);
    procedure ExportTexture(JamId: integer; textureFilename: string);

    procedure ExportCanvas(FileName: string);

    procedure ZeroPalette(JamId: integer);

    function ConvertHWJam(hwJam: THWJamFile): TJamFile;

    function GenerateGPxBMP(bitmap: TBitmap; JamId: integer): TBitmap;

    function DrawSingleTexture(const Raw: TBytes; TotalImageSize: integer;
      JamId: integer; drawFromEntry: boolean): TBitmap;
    function DrawPalTexture(JamId: integer): TBitmap;

    function DrawFullJam(UIUpdate: boolean): TBitmap;
    function DrawFullJIP(const Raw: TBytes): TBitmap;
    function DrawFullRCR(const Raw: TBytes): TBitmap;

    function DrawJamCanvas(UIUpdate: boolean): TBitmap;

    function DrawOutlines(JamCanvas: TBitmap): TBitmap;

    procedure UpdateTextureSize(JamId: integer; Height: integer;
      Width: integer);

    procedure CachePaletteBMP(JamId: integer);

    property Entries: TList<TJamEntry> read FEntries write FEntries;
  end;

implementation

uses mainform;

{ TJamEntry }

constructor TJamEntry.Create(const Info: TJamEntryInfo);
var
  i: integer;
begin
  FInfo := Info;
  FTexture := nil;
  FOriginalTex := nil;
  FRawTexture := nil;
  for i := 0 to 3 do
    FCachedTex[i] := nil;
  intPaletteID := 0;
end;

destructor TJamEntry.Destroy;
var
  i: Integer;
begin
  // Clear palette arrays
  for i := 0 to 3 do
    FPalettes[i] := nil; // clearer and avoids redundant SetLength

  // Free textures safely
  if assigned(FTexture) then
  freeAndNil(FTexture);

  if assigned(FOriginalTex) then
  freeAndNil(FOriginalTex);

  for i := 0 to 3 do
  FCachedTex[i].free;

  // Clear byte array
  FRawTexture := nil;

  inherited;
end;


function TJamEntry.GetPalette(Index: integer): TBytes;
begin
  if (Index < Low(FPalettes)) or (Index > High(FPalettes)) then
    raise ERangeError.CreateFmt('Palette index %d out of range', [Index]);
  Result := FPalettes[Index];
end;

function TJamEntry.GetPaletteSizeDiv4: Word;
begin
  Result := FInfo.PaletteSizeDiv4;
end;

procedure TJamEntry.SetPaletteSizeDiv4(const Value: Word);
begin
  FInfo.PaletteSizeDiv4 := Value;
end;

{ TJamEntry }

{ TJamFile }

constructor TJamFile.Create;
begin
  FEntries := TList<TJamEntry>.Create;
  boolRcrJam := False;
  boolJipMode := False;
  CanvasBitmap := TBitmap.Create;
  canvasHeight := 256;
end;

destructor TJamFile.Destroy;
var
  i: integer;
begin
   if Assigned(FEntries) then
  begin
    for i := 0 to FEntries.Count - 1 do
      freeandnil(FEntries[i]);
    freeandnil(FEntries);
  end;

  if assigned(canvasbitmap) then
  FreeAndNil(CanvasBitmap);

  setLength(FRawData,0);

  FRawData := nil;

  for i := 0 to 3 do
    PalPerLevel[i] := nil;

  for i := 0 to 3 do
  begin
  if assigned(LevelIdx[i]) then
    FreeAndNil(LevelIdx[i]);
  end;

  inherited;
end;

procedure TJamFile.CreateNewJAM(FileName: string; Height: integer);
begin

  FEntries := TList<TJamEntry>.Create;

  boolRcrJam := False;
  boolJipMode := False;
  CanvasBitmap := TBitmap.Create;
  canvasHeight := 256;

  setLength(FRawData, 256 * Height);
  FHeader.NumItems := 0;
  FHeader.JamTotalHeight := Height;

  intJamMaxWidth := 256;
  intJamMaxHeight := Height;

end;

procedure TJamFile.UpdateTextureSize(JamId: integer; Height: integer;
  Width: integer);

begin

  FEntries[JamId].FInfo.Width := Width;
  FEntries[JamId].FInfo.Height := Height;

end;

function TJamFile.UnJam(const Data: TBytes): TBytes;
var
  X: Cardinal;
  n, i: integer;
  pc: PByte;
begin
  Result := Copy(Data);
  X := $B082F164;
  X := X or 1;
  n := Length(Result) div 4;
  pc := @Result[0];
  for i := 0 to n - 1 do
  begin
    PCardinal(pc)^ := PCardinal(pc)^ xor X;
    Inc(pc, 4);
{$Q-}
    X := X + (X * 4);
{$Q+}
  end;
  // remaining bytes
  n := Length(Result) and 3;
  for i := 0 to n - 1 do
  begin
    pc^ := pc^ xor Byte(X and $FF);
    Inc(pc);
    X := X shr 8;
  end;
end;

procedure TJamFile.DecryptSWJam(const FileName: string);
var
  Raw, Buf: TBytes;

begin

  Raw := TFile.ReadAllBytes(FileName);

  Buf := UnJam(Raw);

  Raw := Buf;

  TFile.WriteAllBytes(FileName + 'dec', Raw);

end;

function TJamFile.LoadFromFile(const FileName: string;
  preview: boolean): boolean;
const
  RCRFixupFiles: array [0 .. 9] of string = ('rcr1a', 'rcr2a', 'rcr3a', 'rcr4a',
    'rcr5a', 'rcr1', 'rcr2', 'rcr3', 'rcr4', 'rcr5');
HardcodedDims:
array [0 .. 7] of record Name: string;
Width, Height: integer;
IsRCR:
boolean;
end
= ((Name: 'mhill'; Width: 512; Height: 320; IsRCR: False), (Name: 'shill';
  Width: 64; Height: 16; IsRCR: False), (Name: 'car_srf'; Width: 512;
  Height: 544; IsRCR: True), (Name: 'hlm_srf'; Width: 256; Height: 64;
  IsRCR: True), (Name: 'wh_srf'; Width: 512; Height: 256; IsRCR: True),
  (Name: 'vcp_srf'; Width: 512; Height: 256; IsRCR: True), (Name: 'vcp_srf2';
  Width: 512; Height: 256; IsRCR: True), (Name: 'shill'; Width: 64; Height: 16;
  IsRCR: False));

var
  Raw, Buf: TBytes;
  Ptr, i, BlockCount, TrueSize, palCount: integer;
  Info: TJamEntryInfo;
  sFilename: string;
begin

  Result := False;

  for i := 0 to FEntries.Count - 1 do
    FEntries[i].Free;
  FEntries.Clear;
  // Initial state
  boolRcrJam := False;
  boolJipMode := SameText(TPath.GetExtension(FileName), '.jip');
  intJamMaxWidth := 0;
  intJamMaxHeight := 0;

  sFilename := LowerCase(ChangeFileExt(ExtractFileName(FileName), ''));

  // if sFilename = 'shill' then Exit;
  CheckIfRCR(sFilename);

  // Load + decrypt
  Raw := TFile.ReadAllBytes(FileName);
  Buf := UnJam(Raw);
  Ptr := 0;

  // Header
  Move(Buf[Ptr], FHeader, SizeOf(FHeader));
  Inc(Ptr, SizeOf(FHeader));
  if FHeader.NumItems = 0 then
    FHeader.NumItems := 1;

  // Parse Entries
  if not MatchText(sFilename, ['car_srf', 'hlm_srf', 'vcp_srf', 'vcp_srf2',
    'shill', 'mhill']) then
  begin
    for i := 0 to FHeader.NumItems - 1 do
    begin
      Move(Buf[Ptr], Info, SizeOf(Info));
      Inc(Ptr, SizeOf(Info));
      FEntries.Add(TJamEntry.Create(Info));
    end;

    // RCR fixups
    if boolRcrJam and MatchText(sFilename, RCRFixupFiles) then
    begin
      for i := 0 to FHeader.NumItems - 1 do
      begin
        FEntries[i].FInfo.Width := FEntries[i].FInfo.Width * 2;
        FEntries[i].FInfo.Y := FEntries[i].FInfo.Y div 2;
        // FHeader.JamTotalHeight := FHeader.JamTotalHeight *2;
      end;
    end
    else if boolRcrJam then
    begin
      for i := 0 to FHeader.NumItems - 1 do
      begin
        FEntries[i].FInfo.Height := FEntries[i].FInfo.Height * 2;
        // FEntries[i].FInfo.WIDTH := FEntries[i].FInfo.wIDTH *2;
        FEntries[i].FInfo.Y := FEntries[i].FInfo.Y div 2;
      end;
    end;

    // Parse Palettes and dimensions
    for i := 0 to FHeader.NumItems - 1 do
    begin
      with FEntries[i] do
      begin
        intJamMaxWidth := Max(intJamMaxWidth, FInfo.X + FInfo.Width);
        intJamMaxHeight := Max(intJamMaxHeight, FInfo.Y + FInfo.Height);

        palCount := FInfo.PaletteSizeDiv4;
        if palCount > 0 then
        begin
          if Ptr + palCount * 4 > Length(Buf) then
            raise Exception.CreateFmt
              ('Palette error: entry %d too short.', [i]);

          for var p := 0 to 3 do
          begin
            setLength(FPalettes[p], palCount);
            Move(Buf[Ptr], FPalettes[p][0], palCount);
            Inc(Ptr, palCount);
          end;
        end
        else
          for var p := 0 to 3 do
            FPalettes[p] := nil;
      end;
    end;
  end;

  // Override hardcoded JAM types
  for var H in HardcodedDims do
    if sFilename = H.Name then
    begin
      intJamMaxWidth := H.Width;
      intJamMaxHeight := H.Height;
      FHeader.JamTotalHeight := H.Height;
      boolRcrJam := boolRcrJam or H.IsRCR;
      Break;
    end;

  // Extract raw pixel data
  BlockCount := FHeader.JamTotalHeight;
  TrueSize := BlockCount * 256;
  if Ptr + TrueSize > Length(Buf) then
    raise Exception.CreateFmt
      ('JAM image data too short: need %d bytes at offset %d, have %d',
      [TrueSize, Ptr, Length(Buf)]);

  FRawData := Copy(Buf, Ptr, TrueSize);

  // Generate textures
  if not MatchText(sFilename, ['shill', 'mhill']) then
    for i := 0 to FHeader.NumItems - 1 do
    begin
      with FEntries[i] do
      begin
       if Assigned(FTexture) then
        FTexture.free;

        FTexture := DrawSingleTexture(FRawData, Length(FRawData), i, False);
        if preview = False then
        begin
        if Assigned(FOriginalTex) then
        FreeAndNil(FOriginalTex);
          FOriginalTex := DrawPalTexture(i);
          CachePaletteBMP(i);
        end;

      end;
    end;

  JamFileName := sFilename;
  Result := True;
end;

procedure TJamFile.SaveToFile(const FileName: string);
var
  OutBuf: TBytes;
  Header: TJamHeader;
  Ptr, i, palCount: integer;
  entry: TJamEntry;
begin
  // 1) Prepare header
  Header.NumItems := FHeader.NumItems;
  Header.JamTotalHeight := FHeader.JamTotalHeight;

  // 2) Estimate total size:
  // header + all entry‐infos + all raw textures + all local palettes
  Ptr := SizeOf(Header) + FEntries.Count * SizeOf(TJamEntryInfo);
  for i := 0 to FEntries.Count - 1 do
  begin
    entry := FEntries[i];
    Ptr := Ptr + Length(FRawData);
    // include any local palettes you wrote out in LoadFromFile
    palCount := entry.FInfo.PaletteSizeDiv4;
    // e.g. same count used during Load
    Ptr := Ptr + palCount * 4; // assuming 4 palettes of that length
  end;
  setLength(OutBuf, Ptr);

  // 3) Write header
  Move(Header, OutBuf[0], SizeOf(Header));
  Ptr := SizeOf(Header);

  // 4) Write each TJamEntryInfo
  for i := 0 to FEntries.Count - 1 do
  begin
    entry := FEntries[i];
    Move(entry.FInfo, OutBuf[Ptr], SizeOf(entry.FInfo));
    Inc(Ptr, SizeOf(entry.FInfo));
  end;

  // 5) Write raw texture + palettes for each entry
  for i := 0 to FEntries.Count - 1 do
  begin
    entry := FEntries[i];
    EncodeCanvas(i);

    // 5a) local palettes (up to four of them) — same order you read them in LoadFromFile
    palCount := entry.FInfo.PaletteSizeDiv4;
    if palCount > 0 then
    begin
      Move(entry.FPalettes[0][0], OutBuf[Ptr], palCount);
      Inc(Ptr, palCount);
      Move(entry.FPalettes[1][0], OutBuf[Ptr], palCount);
      Inc(Ptr, palCount);
      Move(entry.FPalettes[2][0], OutBuf[Ptr], palCount);
      Inc(Ptr, palCount);
      Move(entry.FPalettes[3][0], OutBuf[Ptr], palCount);
      Inc(Ptr, palCount);
    end;

    // 5b) raw texture bytes
    // if Length(Entry.FRawTexture) > 0 then
    // begin
    // Move(Entry.FRawTexture[0], OutBuf[Ptr], Length(Entry.FRawTexture));
    // Inc(Ptr, Length(Entry.FRawTexture));
    // end;
  end;

  Move(FRawData[0], OutBuf[Ptr], Length(FRawData));
  //Inc(Ptr, Length(FRawData));
  OutBuf := UnJam(OutBuf);
  // 7) Finally write everything out
  TFile.WriteAllBytes(FileName, OutBuf);
end;

// Takes a texture's raw data and then inserts it into the JAM's main rawData area.

procedure TJamFile.EncodeCanvas(JamId: integer);
var
  entry: TJamEntry;
  Info: TJamEntryInfo;
  W, H: integer;
  X0, Y0: integer;
  TempRaw: TBytes;
  Y, pos: integer;

begin

  entry := FEntries[JamId];
  Info := entry.FInfo;

  W := Info.Width;
  H := Info.Height;
  X0 := Info.X;
  Y0 := Info.Y;

  TempRaw := entry.FRawTexture;

  for Y := 0 to H - 1 do
  begin
    pos := (Y0 + Y) * 256 + X0; // srcStride = 256
    Move(TempRaw[Y * W], FRawData[pos], W);
  end;

end;

// Encodes a bitmap into indicied data (bytes)

procedure TJamFile.EncodeTexture(JamId: integer; texture: TBitmap);
var
  entry: TJamEntry;
  Info: TJamEntryInfo;
  W, H: integer;
  indices, TempRaw: TBytes;
  X, Y, origIdx: integer;
  bestIdx: integer;

begin
  entry := FEntries[JamId];
  Info := entry.FInfo;
  W := Info.Width;
  H := Info.Height;

  // 1) grab the raw 8-bit indices from the bitmap
  indices := BitmapToIndices(texture);

  setLength(TempRaw, W * H);
  // 3) Scan each pixel: find nearest GPxPal index, record in TempRaw & UsedFlags
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
    begin
      origIdx := indices[Y * W + X];
      bestIdx := origIdx;
      TempRaw[Y * W + X] := Byte(bestIdx);
    end;

  // 7) Write TempRaw into the entry’s FRawTexture and the master FRawData

  setLength(entry.FRawTexture, W * H);
  for Y := 0 to H - 1 do
  begin
    Move(TempRaw[Y * W], entry.FRawTexture[Y * W], W);
    // Move(TempRaw[Y * W], FRawData[pos], W);
  end;

  if Assigned(entry.FTexture) then
  entry.ftexture.free;

  entry.FTexture := DrawSingleTexture(entry.FRawTexture,
    Length(entry.FRawTexture), JamId, True);
end;


// draws a single texture - if this is a first draw when the jam is loaded, drawfromentry should be true.
// Otherwise utilise the original texture. This utilises the encoded raw data, be it the rawdata on the texture or the full original data set of the JAM

function TJamFile.DrawSingleTexture(const Raw: TBytes; TotalImageSize: integer;
  JamId: integer; drawFromEntry: boolean): TBitmap;
var
  W, H, X0, Y0: integer;
  SrcStride: integer;
  palCount, i, X, Y, idx, dst: integer;
  LocalPal: array [0 .. 255] of Byte;
  bmp: TBitmap;
  dstID: integer;
begin

  W := FEntries[JamId].FInfo.Width;
  H := FEntries[JamId].FInfo.Height;

  if drawFromEntry then
  begin
    SrcStride := W;
    X0 := 0;
    Y0 := 0;
  end
  else
  begin
    if JamFileName = 'chwheel1' then
      SrcStride := 512
    else
      SrcStride := 256;
    X0 := FEntries[JamId].FInfo.X;
    Y0 := FEntries[JamId].FInfo.Y;
  end;

  // Default = identity mapping
  for i := 0 to 255 do
    LocalPal[i] := i;

  palCount := FEntries[JamId].FInfo.PaletteSizeDiv4;
  if palCount > 256 then
    palCount := 256;

  for i := 0 to palCount - 1 do
    LocalPal[i] := FEntries[JamId].FPalettes[intPaletteID][i];

  bmp := TBitmap.Create;
  try
    bmp.Width := W;
    bmp.Height := H;
    bmp.PixelFormat := pf8bit;
    bmp.Palette := CreateGPxPal;

    setLength(FEntries[JamId].FRawTexture, W * H);

    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        idx := X0 + X + (Y0 + Y) * SrcStride;
        if idx >= Length(Raw) then
          raise Exception.CreateFmt
            ('DrawSingleTexture: Raw index %d out of bounds (%d)',
            [idx, Length(Raw)]);

        dstID := (Y * W) + X;
        FEntries[JamId].FRawTexture[dstID] := Raw[idx];

        dst := LocalPal[Raw[idx]];
        bmp.Canvas.Pixels[X, Y] := RGB(GPxPal[dst].R, GPxPal[dst].G,
          GPxPal[dst].B);
      end;

    Result := bmp;
  except
    bmp.Free;
    raise;
  end;
end;


// caches all the palette bitmaps; in the event there's no palette the texture is applied to all for caches for safety.

procedure TJamFile.CachePaletteBMP(JamId: integer);
var
  tmpPal, X, palCount: integer;
  bmp: TBitmap;
begin
  tmpPal := intPaletteID;
  palCount := FEntries[JamId].FInfo.PaletteSizeDiv4;

  if palCount = 0 then
  begin
    intPaletteID := 0;
    for X := 0 to 3 do
    begin
      bmp := DrawPalTexture(JamId);
      try
        if assigned(FEntries[JamId].FCachedTex[X]) then
        FreeAndNil(FEntries[JamId].FCachedTex[X]);

        FEntries[JamId].FCachedTex[X] := TBitmap.Create;
        FEntries[JamId].FCachedTex[X].Assign(bmp);
      finally
        bmp.Free;
      end;
    end;
  end
  else
  begin
    for X := 0 to 3 do
    begin
      intPaletteID := X;
      bmp := DrawPalTexture(JamId);
      try
        if assigned(FEntries[JamId].FCachedTex[X]) then
        FreeAndNil(FEntries[JamId].FCachedTex[X]);

        FEntries[JamId].FCachedTex[X] := TBitmap.Create;
        FEntries[JamId].FCachedTex[X].Assign(bmp);
      finally
        bmp.Free;
      end;
    end;
  end;

  intPaletteID := tmpPal;
end;


// Draws a texture with the local palette applied

function TJamFile.DrawPalTexture(JamId: integer): TBitmap;
var
  W, H: integer;
  palCount, i, X, Y, idx, dst: integer;
  LocalPal: array [0 .. 255] of Byte;
  jamTex: TBitmap;
begin
  Result := nil;

  if JamId = -1 then
    Exit;

  palCount := FEntries[JamId].FInfo.PaletteSizeDiv4;
  if palCount = 0 then
  begin
    Result := DrawSingleTexture(FEntries[JamId].FRawTexture,
      FHeader.JamTotalHeight, JamId, True);
    Exit;
  end;

  W := FEntries[JamId].FInfo.Width;
  H := FEntries[JamId].FInfo.Height;

  jamTex := TBitmap.Create;
  try
    jamTex.Width := W;
    jamTex.Height := H;
    jamTex.PixelFormat := pf24bit;

    for i := 0 to 255 do
      LocalPal[i] := i;

    if palCount > 256 then
      palCount := 256;

    for i := 0 to palCount - 1 do
      LocalPal[i] := FEntries[JamId].FPalettes[intPaletteID][i];

    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        idx := X + Y * W;
        if idx >= Length(FEntries[JamId].FRawTexture) then
          raise Exception.CreateFmt
            ('DrawPalTexture: Texture index %d out of bounds (%d)',
            [idx, Length(FEntries[JamId].FRawTexture)]);
        dst := LocalPal[FEntries[JamId].FRawTexture[idx]];
        jamTex.Canvas.Pixels[X, Y] := RGB(GPxPal[dst].R, GPxPal[dst].G,
          GPxPal[dst].B);
      end;

    jamTex.Palette := CreateGPxPal;
    Result := jamTex;
  except
    jamTex.Free;
    raise;
  end;
end;

// Draws Jam Canvas from the fully updated whole Jam Raw Data - partly a QC check process.

function TJamFile.DrawJamCanvas(UIUpdate: boolean): TBitmap;
var
  bmpAll, entryBmp: TBitmap;
  i: integer;
begin
  bmpAll := TBitmap.Create;
  try
    bmpAll.PixelFormat := pf8bit;
    bmpAll.Palette := CreateGPxPal;
    bmpAll.Width := 256;
    bmpAll.Height := FHeader.JamTotalHeight;

    bmpAll.Canvas.Brush.Color := RGB(GPxPal[0].R, GPxPal[0].G, GPxPal[0].B);
    bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.Width, bmpAll.Height));

    if boolRcrJam then
    begin
      bmpAll.Width := 512;
      bmpAll.Height := FHeader.JamTotalHeight div 2;
      entryBmp := DrawFullRCR(FRawData);
      try
        bmpAll.Canvas.Draw(0, 0, entryBmp);
      finally
        entryBmp.Free;
      end;
      Result := bmpAll;
      Exit;
    end
    else if boolJipMode then
    begin
      entryBmp := DrawFullJIP(FRawData);
      try
        bmpAll.Height := FHeader.JamTotalHeight;
        intJamMaxHeight := FHeader.JamTotalHeight;
        bmpAll.Canvas.Draw(0, 0, entryBmp);
      finally
        entryBmp.Free;
      end;
      Result := bmpAll;
      Exit;
    end;

    // Generate textures
    if not boolRcrJam then
      for i := 0 to FHeader.NumItems - 1 do
      begin
        with FEntries[i] do
        begin
          bmpAll.Canvas.Draw(Info.X, Info.Y, DrawSingleTexture(FRawData,
            Length(FRawData), i, False));
        end;
      end;
    Result := bmpAll;
  except
    bmpAll.Free;
    raise;
  end;
end;

function TJamFile.DrawFullJam(UIUpdate: boolean): TBitmap;
var
  bmpAll, entryBmp, stretchedBmp: TBitmap;
  i: integer;
begin
  bmpAll := TBitmap.Create;
  try
    bmpAll.PixelFormat := pf8bit;
    bmpAll.Palette := CreateGPxPal;
    bmpAll.Width := 256;
    bmpAll.Height := FHeader.JamTotalHeight;

    bmpAll.Canvas.Brush.Color := RGB(GPxPal[0].R, GPxPal[0].G, GPxPal[0].B);
    bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.Width, bmpAll.Height));

    if boolRcrJam then
    begin
      bmpAll.Width := 512;
      bmpAll.Height := FHeader.JamTotalHeight div 2;
      entryBmp := DrawFullRCR(FRawData);
      try
        bmpAll.Canvas.Draw(0, 0, entryBmp);
      finally
        entryBmp.Free;
      end;
      Result := bmpAll;
      Exit;
    end
    else if boolJipMode then
    begin
      entryBmp := DrawFullJIP(FRawData);
      try
        bmpAll.Height := FHeader.JamTotalHeight;
        intJamMaxHeight := FHeader.JamTotalHeight;
        bmpAll.Canvas.Draw(0, 0, entryBmp);
      finally
        entryBmp.Free;
      end;
      Result := bmpAll;
      Exit;
    end
    else
    begin
      for i := 0 to FEntries.Count - 1 do
      begin
        if UIUpdate then
        begin
          entryBmp := FEntries[i].FCachedTex[intPaletteID];
          if not Assigned(entryBmp) then
            Continue;

          stretchedBmp := resizeTransProtection(entryBmp,
            FEntries[i].Info.Width, FEntries[i].Info.Height,
            RGBFromTRGB(GPxPal[0]));
          try
            bmpAll.Canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y,
              stretchedBmp);
          finally
            FreeAndNil(stretchedBmp);
          end;
        end
        else
        begin
          entryBmp := DrawSingleTexture(FEntries[i].FRawTexture,
            Length(FEntries[i].FRawTexture), i, True);
          try
            bmpAll.Canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y,
              entryBmp);
          finally
            entryBmp.Free;
          end;
        end;
      end;
    end;

    Result := bmpAll;
  except
    bmpAll.Free;
    raise;
  end;
end;


// function TJamFile.DrawFullJam(UIUpdate: Boolean): TBitmap;
// var
// bmpAll, entryBmp, stretchedBmp: TBitmap;
// i: Integer;
// begin
// bmpAll := TBitmap.Create;
// try
// bmpAll.PixelFormat := pf8bit;
// bmpAll.Palette := CreateGPxPal;
// bmpAll.Width := 256;
// bmpAll.Height := FHeader.JamTotalHeight;
//
// bmpAll.Canvas.Brush.Color := RGB(GPxPal[0].R, GPxPal[0].G, GPxPal[0].B);
// bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.Width, bmpAll.Height));
//
// if boolRcrJam then
// begin
// bmpAll.Width := 512;
// bmpAll.Height := FHeader.JamTotalHeight div 2;
// entryBmp := DrawFullRCR(FRawData);
// try
// bmpAll.Canvas.Draw(0, 0, entryBmp);
// finally
// entryBmp.Free;
// end;
// Result := bmpAll;
// Exit;
// end
// else if boolJipMode then
// begin
// entryBmp := DrawFullJIP(FRawData);
// try
// bmpAll.Height := FHeader.JamTotalHeight;
// intJamMaxHeight := FHeader.JamTotalHeight;
// bmpAll.Canvas.Draw(0, 0, entryBmp);
// finally
// entryBmp.Free;
// end;
// Result := bmpAll;
// Exit;
// end
// else
// begin
// for i := 0 to FEntries.Count - 1 do
// begin
// if UIUpdate then
// begin
// entryBmp := FEntries[i].FCachedTex[intPaletteID];
// stretchedBmp :=resizeTransProtection(entryBmp,FEntries[i].Info.Width, FEntries[i].Info.Height, RGBFromTRGB(GpXPal[0]));
// try
// bmpAll.Canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y, stretchedBmp);
// finally
// stretchedBmp.Free;
// end;
// end
// else
// begin
// entryBmp := DrawSingleTexture(FEntries[i].FRawTexture, Length(FEntries[i].FRawTexture), i, True);
// try
// bmpAll.Canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y, entryBmp);
// finally
// entryBmp.Free;
// end;
// end;
// end;
// end;
//
// Result := bmpAll;
// except
// bmpAll.Free;
// raise;
// end;
// end;

function TJamFile.DrawFullJIP(const Raw: TBytes): TBitmap;
var
  i, canvasWidth, canvasHeight: integer;
  CanvasData: TBytes;
  bmp: TBitmap;
begin
  Result := nil;
  canvasWidth := 256;
  canvasHeight := FHeader.JamTotalHeight;

  if Length(Raw) < canvasWidth * canvasHeight then
    raise Exception.Create
      ('DrawFullJIP: Raw data is too short for expected size.');

  CanvasData := Raw;
  bmp := TBitmap.Create;
  try
    bmp.Width := canvasWidth;
    bmp.Height := canvasHeight;
    bmp.PixelFormat := pf8bit;
    bmp.Palette := CreateGPxPal;

    for i := 0 to bmp.Height - 1 do
      Move(CanvasData[i * canvasWidth], bmp.ScanLine[i]^, canvasWidth);

    Result := bmp;
  except
    bmp.Free;
    raise;
  end;
end;

function TJamFile.DrawFullRCR(const Raw: TBytes): TBitmap;
var
  i, canvasWidth, canvasHeight: integer;
  CanvasData: TBytes;
  bmp: TBitmap;
begin
  Result := nil;

  canvasWidth := 512;
  canvasHeight := FHeader.JamTotalHeight div 2;

  if Length(Raw) < canvasWidth * canvasHeight then
    raise Exception.CreateFmt('DrawFullRCR: Raw data too short (%d < %d)',
      [Length(Raw), canvasWidth * canvasHeight]);

  CanvasData := Raw;

  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf8bit;
    bmp.Width := canvasWidth;
    bmp.Height := canvasHeight;
    bmp.Palette := CreateGPxPal;

    for i := 0 to bmp.Height - 1 do
      Move(CanvasData[i * canvasWidth], bmp.ScanLine[i]^, canvasWidth);

    Result := bmp;
  except
    bmp.Free;
    raise;
  end;
end;

function TJamFile.DrawOutlines(JamCanvas: TBitmap): TBitmap;
var
  i: integer;
  tmpBMP: TBitmap;
begin
  if not boolDrawOutlines then
    Exit(JamCanvas);

  tmpBMP := TBitmap.Create;
  tmpBMP := JamCanvas;

  for i := 0 to FEntries.Count - 1 do
  begin
    with FEntries[i] do
      DrawTextureOutlines(tmpBMP, Info.X, Info.Y, Info.Width, Info.Height, i,
        Info.JamId);
  end;

  Result := tmpBMP;
end;

procedure TJamFile.AddTexture(textureFilename: string);
var
  srcPic: TPicture;
  Info: TJamEntryInfo;
  newTex: TJamEntry;
  X: integer;
  tmpCanvas, scaledCanvas: TBitmap;
  transBool: boolean;
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
    Info.PaletteSizeDiv4 := 0;

    newTex := TJamEntry.Create(Info);
    newTex.FOriginalTex := TBitmap.Create;
    newTex.FTexture := TBitmap.Create;

    newTex.FOriginalTex.SetSize(Info.Width, Info.Height);
    newTex.FTexture.SetSize(Info.Width, Info.Height);

    scaledCanvas := resizeTransProtection(srcPic.bitmap, Info.Width,
      Info.Height, RGBFromTRGB(GPxPal[0]));

    newTex.FOriginalTex.Assign(scaledCanvas);
    newTex.FTexture.Assign(scaledCanvas);

    for X := 0 to 3 do
      newTex.FPalettes[X] := nil;

    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    X := FEntries.Count - 1;
    intSelectedTexture := X;

    FEntries[X].FTexture := GenerateGPxBMP(scaledCanvas, X);
    FEntries[X].FOriginalTex := CreateGPxPalBMP(scaledCanvas);

    CachePaletteBMP(X);

    FEntries[X].FInfo.JamFlags := 0;

    transBool := DetectTransCol(FEntries[X].FCachedTex[0]);

    if not isPowerOfTwo(FEntries[X].FTexture.Width) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 9);

    if not isPowerOfTwo(FEntries[X].FTexture.Height) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 8);

    if transBool then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 3);

    mainform.formmain.JamRegen;

  finally
    srcPic.Free;
    tmpCanvas.Free;
    scaledCanvas.Free;
  end;
end;

procedure TJamFile.DeleteTexture(JamId: integer);
begin

  FEntries.Delete(JamId);
  Dec(FHeader.NumItems);

end;

function TJamFile.AddTexture(bmp: TBitmap): integer;
var
  Info: TJamEntryInfo;
  newTex: TJamEntry;
  X: integer;
  tmpCanvas, scaledCanvas: TBitmap;
  transBool: boolean;
begin
  tmpCanvas := TBitmap.Create;
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

    Info.PaletteSizeDiv4 := 0;

    newTex := TJamEntry.Create(Info);
    newTex.FOriginalTex := TBitmap.Create;
    newTex.FTexture := TBitmap.Create;

    newTex.FOriginalTex.SetSize(Info.Width, Info.Height);
    newTex.FTexture.SetSize(Info.Width, Info.Height);

    scaledCanvas := resizeTransProtection(bmp, Info.Width, Info.Height,
      RGBFromTRGB(GPxPal[0]));

    newTex.FOriginalTex.Assign(scaledCanvas);
    newTex.FTexture.Assign(scaledCanvas);

    for X := 0 to 3 do
      newTex.FPalettes[X] := nil;

    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    X := FEntries.Count - 1;
    intSelectedTexture := X;

    FEntries[X].FTexture := GenerateGPxBMP(scaledCanvas, X);
    FEntries[X].FOriginalTex := CreateGPxPalBMP(scaledCanvas);

    CachePaletteBMP(X);

    FEntries[X].FInfo.JamFlags := 0;

    transBool := DetectTransCol(FEntries[X].FCachedTex[0]);

    if not isPowerOfTwo(FEntries[X].FTexture.Width) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 9);

    if not isPowerOfTwo(FEntries[X].FTexture.Height) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 8);

    if transBool then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 3);

    mainform.formmain.JamRegen;

  finally
    tmpCanvas.Free;
    scaledCanvas.Free;
  end;

  Result := X;
end;

procedure TJamFile.ImportTexture(JamId: integer; textureFilename: string);
var
  srcPic: TPicture;
  textureWidth, textureHeight: integer;
  tmpCanvas, scaledCanvas: TBitmap;
begin
  srcPic := TPicture.Create;
  tmpCanvas := TBitmap.Create;
  scaledCanvas := nil;
  textureWidth := FEntries[JamId].Info.Width;
  textureHeight := FEntries[JamId].Info.Height;

  try
    srcPic.LoadFromFile(textureFilename);
    tmpCanvas.PixelFormat := pf24bit;
    tmpCanvas.SetSize(textureWidth, textureHeight);

    scaledCanvas := resizeTransProtection(srcPic.bitmap, textureWidth,
      textureHeight, RGBFromTRGB(GPxPal[0]));
    tmpCanvas.Canvas.StretchDraw(Rect(0, 0, textureWidth, textureHeight),
      scaledCanvas);

    if assigned(FEntries[jamId].ftexture) then
    FEntries[jamId].ftexture.free;
    FEntries[JamId].FTexture := GenerateGPxBMP(tmpCanvas, JamId);

    if assigned(FEntries[jamId].ftexture) then
    FEntries[jamId].FOriginalTex.free;

    FEntries[JamId].FOriginalTex := CreateGPxPalBMP(tmpCanvas);
    CachePaletteBMP(JamId);
  finally
    srcPic.Free;
    tmpCanvas.Free;
    if Assigned(scaledCanvas) then
      scaledCanvas.Free;
  end;
end;

procedure TJamFile.ExportTexture(JamId: integer; textureFilename: string);
var
  exportPic: TPicture;
begin
  exportPic := TPicture.create;
  exportPic.bitmap := FEntries[JamId].FTexture;
  exportPic.SaveToFile(textureFilename);
  freeandnil(exportPic);
end;

procedure TJamFile.ExportCanvas(FileName: string);
var
  exportPic: TPicture;
begin
  exportPic := TPicture.Create;
  exportPic.bitmap := DrawJamCanvas(False);
  exportPic.SaveToFile(FileName);

  freeandnil(exportPic);

end;

procedure TJamFile.ZeroPalette(JamId: integer);
var
  tmpBMP, resizedBMP: TBitmap;
begin
  tmpBMP := TBitmap.Create;

  resizedBMP := resizeTransProtection(FEntries[JamId].FOriginalTex,
    FEntries[JamId].FInfo.Width, FEntries[JamId].FInfo.Height,
    RGBFromTRGB(GPxPal[0]));
  try
    tmpBMP.Assign(resizedBMP);
    tmpBMP.PixelFormat := pf8bit;
    tmpBMP := CreateGPxPalBMP(resizedBMP);
    tmpBMP.Palette := CreateGPxPal;
    EncodeTexture(JamId, tmpBMP);

    FEntries[JamId].PaletteSizeDiv4 := 0;
    for var i := 0 to 3 do
      FEntries[JamId].FPalettes[i] := nil;

    CachePaletteBMP(JamId);
  finally
    tmpBMP.Free;
    resizedBMP.Free;
  end;
end;

//
//function TJamFile.GenerateGPxBMP(bitmap: TBitmap; JamId: integer): TBitmap;
//var
//  SrcRGB: TBitmap;
//  L1Idx, L2Idx, L3Idx, L4Idx: TBitmap;
//  L1RGB, L2RGB, L3RGB, L4RGB: TBitmap;
//  DisplayIdx: TBitmap;
//  i, j: integer;
//  PalSize2: integer;
//  LP2: PLogPal2;
//  hPalTemp: HPALETTE;
//  X, Y, H, W, idx: integer;
//  indices: TBytes;
//  tmpBMP: TBitmap;
//  Mask: TBoolGrid;
//  tempColour: TRGB;
//  jamPalSize: integer;
//  maskBMP: TBitmap;
//  maskBMP2: TBitmap;
//  maskBMP3: TBitmap;
//  maskBMP4: TBitmap;
//begin
//  for i := 0 to 3 do
//  begin
//    LevelIdx[i] := TBitmap.Create;
//    LevelIdx[i].PixelFormat := pf8bit;
//  end;
//
//  SingleIdxMap := TBitmap.Create;
//  SingleIdxMap.PixelFormat := pf8bit;
//
//  for i := 0 to 3 do
//    setLength(PalPerLevel[i], 256);
//
//  SrcRGB := bitmap;
//
//  try
//
//    if SrcRGB.PixelFormat <> pf24bit then
//      SrcRGB.PixelFormat := pf24bit;
//
//    if boolGp2Livery then
//    begin
//      maskBMP := TBitmap.Create;
//      maskBMP.LoadFromResourceName(HInstance, 'gp2livery');
//      maskBMP.PixelFormat := pf24bit;
//
//      BuildGPxMatteMask(maskBMP, [TCol_TransGP2, TCol_TransGP3,
//        TCol_TransGP3HW], Mask);
//    end
//    else
//      BuildGPxMatteMask(SrcRGB, [TCol_TransGP2, TCol_TransGP3,
//        TCol_TransGP3HW], Mask);
//
//    if intSimplifyMethod = 0 then
//      QuadTreeSimplify(SrcRGB, intSimplifyThreshold, SrcRGB);
//
//    if intSimplifyMethod = 1 then
//      SimplifyBySeedThreshold(SrcRGB, intSimplifyThreshold, SrcRGB);
//
//    if intSimplifyMethod = 2 then
//      SimplifyByRegionMeanThreshold(SrcRGB, intSimplifyThreshold, SrcRGB);
//
//    if intSimplifyMethod = 3 then
//      SimplifyByNeighborThreshold(SrcRGB, intSimplifyThreshold, SrcRGB);
//
//    // Level 1 quantize → L1Idx
//    L1Idx := CreateGPxPalBMP(SrcRGB);
//
//    maskBMP := CreateTransparencyMatte(SrcRGB);
//    maskBMP2 := maskBMP;
//    maskBMP3 := maskBMP;
//    maskBMP4 := maskBMP;
//
//    LevelIdx[0].Assign(L1Idx);
//
//    IndexedTo24bit(L1Idx, L1RGB);
//
//    BleedEdges(L1RGB, maskBMP, RGBFromTRGB(GPxPal[0]), 4);
//
//    L1RGB := ApplyMatteToImage(L1RGB, maskBMP, RGBFromTRGB(GPxPal[0]));
//
//    // Blur chain: L1RGB→L2RGB→L3RGB→L4RGB
//    GaussianBlur(L1RGB, Mask, intBlurThreshold, L2RGB);
//    GaussianBlur(L2RGB, Mask, intBlurThreshold, L3RGB);
//    GaussianBlur(L3RGB, Mask, intBlurThreshold, L4RGB);
//
//    GaussianBlur(maskBMP2, Mask, intBlurThreshold, maskBMP2);
//    GaussianBlur(maskBMP3, Mask, intBlurThreshold, maskBMP3);
//    GaussianBlur(maskBMP4, Mask, intBlurThreshold, maskBMP4);
//
//    L2RGB := ApplyMatteToImage(L2RGB, maskBMP2, RGBFromTRGB(GPxPal[0]));
//    L3RGB := ApplyMatteToImage(L3RGB, maskBMP3, RGBFromTRGB(GPxPal[0]));
//    L4RGB := ApplyMatteToImage(L4RGB, maskBMP4, RGBFromTRGB(GPxPal[0]));
//
//    if boolSimpifyAllPals then
//    begin
//
//      SimplifyByNeighborThreshold(L1RGB, intSimplifyThreshold, L1RGB);
//      SimplifyByNeighborThreshold(L2RGB, intSimplifyThreshold, L2RGB);
//      SimplifyByNeighborThreshold(L3RGB, intSimplifyThreshold, L3RGB);
//      SimplifyByNeighborThreshold(L4RGB, intSimplifyThreshold, L4RGB);
//    end;
//
//    // Quantize blurred → L2Idx, L3Idx, L4Idx
//    L2Idx := CreateGPxPalBMP(L2RGB);
//    LevelIdx[1].Assign(L2Idx);
//    L3Idx := CreateGPxPalBMP(L3RGB);
//    LevelIdx[2].Assign(L3Idx);
//    L4Idx := CreateGPxPalBMP(L4RGB);
//    LevelIdx[3].Assign(L4Idx);
//  finally
//    SrcRGB.Free;
//    L1RGB.Free;
//    L2RGB.Free;
//    L3RGB.Free;
//    L4RGB.Free;
//  end;
//
//  // Build single‐index map + custom palettes
//  BuildBmpIdxPal([LevelIdx[0], LevelIdx[1], LevelIdx[2], LevelIdx[3]],
//    SingleIdxMap, PalPerLevel);
//
//  // showMessage('Current Pal: ' + intToStr(FEntries[jamID].PaletteSizeDiv4));
//
//  jamPalSize := FEntries[JamId].PaletteSizeDiv4;
//
//  if FEntries[JamId].PaletteSizeDiv4 < Length(PalPerLevel[0]) then
//  begin
//    FEntries[JamId].PaletteSizeDiv4 := Length(PalPerLevel[0]);
//    setLength(FEntries[JamId].FPalettes[0], Length(PalPerLevel[0]));
//    setLength(FEntries[JamId].FPalettes[1], Length(PalPerLevel[0]));
//    setLength(FEntries[JamId].FPalettes[2], Length(PalPerLevel[0]));
//    setLength(FEntries[JamId].FPalettes[3], Length(PalPerLevel[0]));
//
//    for i := jamPalSize to Length(PalPerLevel[0]) - 1 do
//    begin
//      FEntries[JamId].FPalettes[0][i] := 0;
//      FEntries[JamId].FPalettes[1][i] := 0;
//      FEntries[JamId].FPalettes[2][i] := 0;
//      FEntries[JamId].FPalettes[3][i] := 0;
//    end;
//  end;
//
//  for i := 0 to 3 do
//  begin
//    for j := 0 to FEntries[JamId].PaletteSizeDiv4 - 1 do
//    begin
//      tempColour.R := GetRValue(PalPerLevel[i][j]);
//      tempColour.G := GetGValue(PalPerLevel[i][j]);
//      tempColour.B := GetBValue(PalPerLevel[i][j]);
//      FEntries[JamId].FPalettes[i][j] := Byte(findGPxCol(GPxPal, tempColour));
//    end;
//  end;
//
//  // singleidxmap.Palette := CreatePaletteFromColors; // make sure new bitmap has the original GP3Pal assigned so the image is 'encoded'
//  EncodeTexture(JamId, SingleIdxMap);
//
//  SingleIdxMap.Palette := CreateGPxPal;
//  Result := SingleIdxMap;
//
//  L1Idx.Free;
//  L2Idx.Free;
//  L3Idx.Free;
//  L4Idx.Free;
//  maskBMP.Free;
//
//end;

function TJamFile.GenerateGPxBMP(bitmap: TBitmap; JamId: integer): TBitmap;
var
  SrcRGB: TBitmap;
  L1Idx, L2Idx, L3Idx, L4Idx: TBitmap;
  L1RGB, L2RGB, L3RGB, L4RGB: TBitmap;
  i, j: integer;
  PalSize2: integer;
  LP2: PLogPal2;
  hPalTemp: HPALETTE;
  X, Y, H, W, idx: integer;
  indices: TBytes;
  tmpBMP: TBitmap;
  Mask: TBoolGrid;
  tempColour: TRGB;
  jamPalSize: integer;
  maskBMP: TBitmap;
  maskBMP2: TBitmap;
  maskBMP3: TBitmap;
  maskBMP4: TBitmap;
  singleIDXMap : TBitmap;
begin


  for i := 0 to 3 do
    if assigned (LevelIDX[i]) then
    FreeAndNil(LevelIdx[i]);

  for i := 0 to 3 do
  begin
    LevelIdx[i] := TBitmap.Create;
    LevelIdx[i].PixelFormat := pf8bit;
  end;

  SingleIdxMap := TBitmap.Create;
  SingleIdxMap.PixelFormat := pf8bit;

  for i := 0 to 3 do
    setLength(PalPerLevel[i], 256);

  SrcRGB := bitmap;

  try

    if SrcRGB.PixelFormat <> pf24bit then
      SrcRGB.PixelFormat := pf24bit;

    if boolGp2Livery then
    begin
      maskBMP := TBitmap.Create;
      maskBMP.LoadFromResourceName(HInstance, 'gp2livery');
      maskBMP.PixelFormat := pf24bit;

      BuildGPxMatteMask(maskBMP, [TCol_TransGP2, TCol_TransGP3,
        TCol_TransGP3HW], Mask);
    end
    else
      BuildGPxMatteMask(SrcRGB, [TCol_TransGP2, TCol_TransGP3,
        TCol_TransGP3HW], Mask);

    if intSimplifyMethod = 0 then
      QuadTreeSimplify(SrcRGB, intSimplifyThreshold, SrcRGB);

    if intSimplifyMethod = 1 then
      SimplifyBySeedThreshold(SrcRGB, intSimplifyThreshold, SrcRGB);

    if intSimplifyMethod = 2 then
      SimplifyByRegionMeanThreshold(SrcRGB, intSimplifyThreshold, SrcRGB);

    if intSimplifyMethod = 3 then
      SimplifyByNeighborThreshold(SrcRGB, intSimplifyThreshold, SrcRGB);

    // Level 1 quantize → L1Idx
    L1Idx := CreateGPxPalBMP(SrcRGB);

    maskBMP := CreateTransparencyMatte(SrcRGB);
    maskBMP2 := maskBMP;
    maskBMP3 := maskBMP;
    maskBMP4 := maskBMP;

    LevelIdx[0].Assign(L1Idx);

    IndexedTo24bit(L1Idx, L1RGB);

    BleedEdges(L1RGB, maskBMP, RGBFromTRGB(GPxPal[0]), 4);

    L1RGB := ApplyMatteToImage(L1RGB, maskBMP, RGBFromTRGB(GPxPal[0]));

    // Blur chain: L1RGB→L2RGB→L3RGB→L4RGB
    GaussianBlur(L1RGB, Mask, intBlurThreshold, L2RGB);
    GaussianBlur(L2RGB, Mask, intBlurThreshold, L3RGB);
    GaussianBlur(L3RGB, Mask, intBlurThreshold, L4RGB);

    GaussianBlur(maskBMP2, Mask, intBlurThreshold, maskBMP2);
    GaussianBlur(maskBMP3, Mask, intBlurThreshold, maskBMP3);
    GaussianBlur(maskBMP4, Mask, intBlurThreshold, maskBMP4);

    L2RGB := ApplyMatteToImage(L2RGB, maskBMP2, RGBFromTRGB(GPxPal[0]));
    L3RGB := ApplyMatteToImage(L3RGB, maskBMP3, RGBFromTRGB(GPxPal[0]));
    L4RGB := ApplyMatteToImage(L4RGB, maskBMP4, RGBFromTRGB(GPxPal[0]));

    if boolSimpifyAllPals then
    begin

      SimplifyByNeighborThreshold(L1RGB, intSimplifyThreshold, L1RGB);
      SimplifyByNeighborThreshold(L2RGB, intSimplifyThreshold, L2RGB);
      SimplifyByNeighborThreshold(L3RGB, intSimplifyThreshold, L3RGB);
      SimplifyByNeighborThreshold(L4RGB, intSimplifyThreshold, L4RGB);
    end;

    // Quantize blurred → L2Idx, L3Idx, L4Idx
    L2Idx := CreateGPxPalBMP(L2RGB);
    LevelIdx[1].Assign(L2Idx);
    L3Idx := CreateGPxPalBMP(L3RGB);
    LevelIdx[2].Assign(L3Idx);
    L4Idx := CreateGPxPalBMP(L4RGB);
    LevelIdx[3].Assign(L4Idx);
  finally
    SrcRGB.Free;
    L1RGB.Free;
    L2RGB.Free;
    L3RGB.Free;
    L4RGB.Free;
  end;

  // Build single‐index map + custom palettes
  BuildBmpIdxPal([LevelIdx[0], LevelIdx[1], LevelIdx[2], LevelIdx[3]],
    SingleIdxMap, PalPerLevel);

  // showMessage('Current Pal: ' + intToStr(FEntries[jamID].PaletteSizeDiv4));

  jamPalSize := FEntries[JamId].PaletteSizeDiv4;

  if FEntries[JamId].PaletteSizeDiv4 < Length(PalPerLevel[0]) then
  begin
    FEntries[JamId].PaletteSizeDiv4 := Length(PalPerLevel[0]);
    setLength(FEntries[JamId].FPalettes[0], Length(PalPerLevel[0]));
    setLength(FEntries[JamId].FPalettes[1], Length(PalPerLevel[0]));
    setLength(FEntries[JamId].FPalettes[2], Length(PalPerLevel[0]));
    setLength(FEntries[JamId].FPalettes[3], Length(PalPerLevel[0]));

    for i := jamPalSize to Length(PalPerLevel[0]) - 1 do
    begin
      FEntries[JamId].FPalettes[0][i] := 0;
      FEntries[JamId].FPalettes[1][i] := 0;
      FEntries[JamId].FPalettes[2][i] := 0;
      FEntries[JamId].FPalettes[3][i] := 0;
    end;
  end;

  for i := 0 to 3 do
  begin
    for j := 0 to FEntries[JamId].PaletteSizeDiv4 - 1 do
    begin
      tempColour.R := GetRValue(PalPerLevel[i][j]);
      tempColour.G := GetGValue(PalPerLevel[i][j]);
      tempColour.B := GetBValue(PalPerLevel[i][j]);
      FEntries[JamId].FPalettes[i][j] := Byte(findGPxCol(GPxPal, tempColour));
    end;
  end;

  // singleidxmap.Palette := CreatePaletteFromColors; // make sure new bitmap has the original GP3Pal assigned so the image is 'encoded'
  EncodeTexture(JamId, SingleIdxMap);

  SingleIdxMap.Palette := CreateGPxPal;

  Result := SingleIdxMap;

  L1Idx.Free;
  L2Idx.Free;
  L3Idx.Free;
  L4Idx.Free;
  maskBMP.Free;
  maskBMP2.Free;
  maskBMP3.Free;
  maskBMP4.Free;

end;

function TJamFile.ConvertHWJam(hwJam: THWJamFile): TJamFile;
var
  i: integer;
  newTexture: TJamEntry;
begin

  setLength(FRawData, 256 * hwJam.FHeader.JamTotalHeight);
  FHeader.NumItems := hwJam.FHeader.NumItems;
  FHeader.JamTotalHeight := hwJam.FHeader.JamTotalHeight;

  intJamMaxWidth := 256;
  intJamMaxHeight := hwJam.FHeader.JamTotalHeight;

  for i := 0 to hwJam.FEntries.Count - 1 do
    with FEntries[i] do
    begin
      AddTexture(hwJam.FEntries[i].FTexture);
      FInfo.JamFlags := hwJam.FEntries[i].FInfo.JamFlags;
      FInfo.X := hwJam.FEntries[i].FInfo.X;
      FInfo.Y := hwJam.FEntries[i].FInfo.Y;
      FInfo.Width := hwJam.FEntries[i].FInfo.Width;
      FInfo.Height := hwJam.FEntries[i].FInfo.Height;
    end;

end;

end.
