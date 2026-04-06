unit JamSW;

interface

uses
  // System
  System.SysUtils, System.Types, System.Generics.Collections,
  System.IOUtils, System.Math, System.StrUtils,
  // Windows/VCL
  Winapi.Windows, Vcl.Graphics, Vcl.dialogs,
  // Project
  GeneralHelpers, JamGeneral, JamPalette, JamHW,

  System.Classes; // for TFile.WriteAllBytes

type

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
    rcrA, rcrB: TBitmap;

    TempDimensions: TJamTempDimensions;
    boolImportedBMP: boolean;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    function Clone: TJamEntry;

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
    JamFileName: string;
    JamFullPath: string;
    JamPal: array [0 .. 255] of TRGB;

    originalCanvas: TBitmap;

    function UnJam(const Data: TBytes): TBytes;
    constructor Create;
    destructor Destroy; override;

    function Clone: TJamFile;

    procedure SetGpxPal(boolGP2: boolean);

    function LoadFromFile(const FileName: string; preview: boolean): boolean;
    procedure SaveToFile(const FileName: string; decrypt: boolean);

    procedure CreateNewJAM(FileName: string; Height: integer);

    procedure SaveDecryptedJam(const FileName: string);

    procedure ImportCanvas(FileName: string);

    procedure EncodeTexture(JamId: integer; texture: TBitmap);
    procedure EncodeCanvas();

    function AddTexture(bmp: TBitmap; newinfo: TJamEntryInfo; palGen: boolean)
      : integer overload;
    procedure DeleteTexture(JamId: integer);

    procedure AddTexture(textureFilename: string)overload;

    procedure ImportTexture(JamId: integer; textureFilename: string);
    procedure ExportTexture(JamId: integer; textureFilename: string);

    procedure ExportCanvas(FileName: string);

    procedure ZeroPalette(JamId: integer);

    procedure ConvertGPxJam(origJam: TJamFile; gp2Pal: boolean);
    procedure ConvertHWJam(hwJam: THWJamFile; gp2Pal: boolean);

    function GenerateGPxBMP(bitmap: TBitmap; JamId: integer; method: integer;
      simplifyThreshold: integer; BlurThreshold: integer; allPals: boolean;
      protectMatte: boolean): TBitmap;

    function DrawSingleTexture(const Raw: TBytes; TotalImageSize: integer;
      JamId: integer; drawFromEntry: boolean): TBitmap;
    function DrawPalTexture(JamId: integer): TBitmap;

    function DrawFullJam(UIUpdate: boolean): TBitmap;
    function DrawRawJAM(const Raw: TBytes): TBitmap;
    function DrawFullRCR(const Raw: TBytes; odd: boolean;
      halfHeight: boolean): TBitmap;

    procedure CalculateImagePtrs;

    function RenderJamCanvas(UIUpdate: boolean): TBitmap;

    procedure DrawBaseCanvas(clean: boolean);

    function DrawOutlines(JamCanvas: TBitmap): TBitmap;

    procedure UpdateTextureSize(JamId: integer; Height: integer;
      Width: integer);

    procedure CachePaletteBMP(JamId: integer);

    procedure ReCacheTextures();

    procedure ResizeJam(originalHeight: integer);

    function GetNextJamID(JamEntries: TList<TJamEntry>): Word;

    function GetIDX08_X(idx08: Word): Byte;
    function GetIDX08_Y(idx08: Word): Byte;

    function SetIDX08(X, Y: Byte): Byte;

    function GetIDX0aFlags(idx0a: Word): Byte;
    function GetIDX0aScale(idx0a: Word): Byte;
    function SetIDX0a(flags, scale: Byte): Word;

    procedure BuildRect_SW(Jam: TJamFile; var Rects: TArray<TJamRect>);

    procedure ApplyRects_SW(Jam: TJamFile; const Rects: TArray<TJamRect>);

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
  rcrA := nil;
  rcrB := nil;

  for i := 0 to 3 do
    FCachedTex[i] := nil;
  intPaletteID := 0;
end;

destructor TJamEntry.Destroy;
var
  i: integer;
begin
  // Clear palette arrays
  for i := 0 to 3 do
    FPalettes[i] := nil;

  // Free textures safely
  if assigned(FTexture) then
    freeAndNil(FTexture);

  if assigned(FOriginalTex) then
    freeAndNil(FOriginalTex);

  for i := 0 to 3 do
    FCachedTex[i].free;

  if assigned(rcrA) then
    freeAndNil(rcrA);

  if assigned(rcrB) then
    freeAndNil(rcrA);
  // Clear byte array
  FRawTexture := nil;

  inherited;
end;

function TJamEntry.Clone: TJamEntry;
var
  i: integer;
begin
  Result := TJamEntry.Create;

  // Copy simple fields
  Result.FInfo := Self.FInfo;
  Result.TempDimensions := Self.TempDimensions;
  Result.boolImportedBMP := Self.boolImportedBMP;

  // Copy palettes (array of TBytes)
  for i := 0 to 3 do
    Result.FPalettes[i] := Copy(Self.FPalettes[i]);

  // Copy raw texture
  Result.FRawTexture := Copy(Self.FRawTexture);

  // Clone FTexture
  if assigned(Self.FTexture) then
  begin
    Result.FTexture := TBitmap.Create;
    Result.FTexture.Assign(Self.FTexture);
  end;

  // Clone FOriginalTex
  if assigned(Self.FOriginalTex) then
  begin
    Result.FOriginalTex := TBitmap.Create;
    Result.FOriginalTex.Assign(Self.FOriginalTex);
  end;

  // Clone cached textures
  for i := 0 to 3 do
  begin
    if assigned(Self.FCachedTex[i]) then
    begin
      Result.FCachedTex[i] := TBitmap.Create;
      Result.FCachedTex[i].Assign(Self.FCachedTex[i]);
    end;
  end;
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

procedure TJamEntry.SaveToStream(Stream: TStream);
var
  i, L: integer;
begin
  // Save FInfo
  Stream.WriteBuffer(FInfo, SizeOf(FInfo));

  // Save Palettes
  for i := 0 to 3 do
  begin
    L := Length(FPalettes[i]);
    Stream.WriteBuffer(L, SizeOf(L));
    if L > 0 then
      Stream.WriteBuffer(FPalettes[i][0], L);
  end;

  // Save FTexture
  FTexture.SaveToStream(Stream);

  // Save FOriginalTex
  FOriginalTex.SaveToStream(Stream);

  for i := 0 to 3 do
  begin
    FCachedTex[i].SaveToStream(Stream);
  end;

  // Save RawTexture
  L := Length(FRawTexture);
  Stream.WriteBuffer(L, SizeOf(L));
  if L > 0 then
    Stream.WriteBuffer(FRawTexture[0], L);

  // Save TempDimensions
  Stream.WriteBuffer(TempDimensions, SizeOf(TempDimensions));

  // Save boolImportedBMP
  Stream.WriteBuffer(boolImportedBMP, SizeOf(boolImportedBMP));
end;

procedure TJamEntry.LoadFromStream(Stream: TStream);
var
  i, L: integer;
begin
  // Read FInfo
  Stream.ReadBuffer(FInfo, SizeOf(FInfo));

  // Read Palettes
  for i := 0 to 3 do
  begin
    Stream.ReadBuffer(L, SizeOf(L));
    SetLength(FPalettes[i], L);
    if L > 0 then
      Stream.ReadBuffer(FPalettes[i][0], L);
  end;

  // Read FTexture
  freeAndNil(FTexture);
  FTexture := TBitmap.Create;
  FTexture.LoadFromStream(Stream);

  // Read FOriginalTex
  freeAndNil(FOriginalTex);
  FOriginalTex := TBitmap.Create;
  FOriginalTex.LoadFromStream(Stream);

  for i := 0 to 3 do
  begin
    freeAndNil(FCachedTex[i]);
    FCachedTex[i] := TBitmap.Create;
    FCachedTex[i].LoadFromStream(Stream);
  end;

  // Read RawTexture
  Stream.ReadBuffer(L, SizeOf(L));
  SetLength(FRawTexture, L);
  if L > 0 then
    Stream.ReadBuffer(FRawTexture[0], L);

  // Read TempDimensions
  Stream.ReadBuffer(TempDimensions, SizeOf(TempDimensions));

  // Read boolImportedBMP
  Stream.ReadBuffer(boolImportedBMP, SizeOf(boolImportedBMP));
end;

{ TJamEntry }

{ TJamFile }

function TJamFile.GetNextJamID(JamEntries: TList<TJamEntry>): Word;
var
  i: integer;
  MaxID: Word;
begin
  MaxID := 0;
  for i := 0 to JamEntries.Count - 1 do
  begin
    if JamEntries[i].FInfo.JamId > MaxID then
      MaxID := JamEntries[i].FInfo.JamId;
  end;

  if MaxID < High(Word) then
    Result := MaxID + 1
  else
    raise Exception.Create('Maximum JamID value (65535) exceeded');
end;

constructor TJamFile.Create;
begin
  FEntries := TList<TJamEntry>.Create;
  boolRcrJam := False;

  // CanvasBitmap := TBitmap.Create;
  canvasHeight := 256;
end;

destructor TJamFile.Destroy;
var
  i: integer;
begin
  if assigned(FEntries) then
  begin
    for i := 0 to FEntries.Count - 1 do
      freeAndNil(FEntries[i]);
    freeAndNil(FEntries);
  end;

  // if assigned(CanvasBitmap) then
  // freeAndNil(CanvasBitmap);

  SetLength(FRawData, 0);

  FRawData := nil;

  for i := 0 to 3 do
    PalPerLevel[i] := nil;

  for i := 0 to 3 do
  begin
    if assigned(LevelIdx[i]) then
      freeAndNil(LevelIdx[i]);
  end;

  if assigned(originalCanvas) then
    freeAndNil(originalCanvas);

  inherited;
end;

function TJamFile.Clone: TJamFile;
var
  NewJam: TJamFile;
  i: integer;
  Entry: TJamEntry;
begin
  NewJam := TJamFile.Create;

  // Copy simple fields
  NewJam.FHeader := Self.FHeader;
  NewJam.FRawData := Copy(Self.FRawData);
  NewJam.canvasHeight := Self.canvasHeight;
  NewJam.canvasWidth := Self.canvasWidth;
  NewJam.JamFileName := Self.JamFileName;
  NewJam.JamFullPath := Self.JamFullPath;
  NewJam.JamPal := Self.JamPal;

  NewJam.originalCanvas := TBitmap.create;
  NewJam.originalCanvas.Assign(self.originalCanvas);

  // copy entries
  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := FEntries[i].Clone; // assumes TJamEntry has Clone method
    NewJam.FEntries.Add(Entry);
  end;

  // // Clone CanvasBitmap
  // NewJam.CanvasBitmap := TBitmap.Create;
  // NewJam.CanvasBitmap.Assign(Self.CanvasBitmap);

  // Clone LevelIdx
  for i := 0 to 3 do
  begin
    NewJam.LevelIdx[i] := TBitmap.Create;
    NewJam.LevelIdx[i].Assign(Self.LevelIdx[i]);
  end;

  // Copy PalPerLevel
  for i := 0 to 3 do
    NewJam.PalPerLevel[i] := Copy(Self.PalPerLevel[i]);

  Result := NewJam;
end;

procedure TJamFile.CalculateImagePtrs;
var
  i: integer;
begin

  for i := 0 to FEntries.Count - 1 do
  begin
    if i = 0 then
    begin
      FEntries[i].FInfo.ImagePtr := 0;
    end
    else
      FEntries[i].FInfo.ImagePtr := FEntries[i - 1].FInfo.ImagePtr +
        (FEntries[i].FInfo.PaletteSizeDiv4 * 4)
  end;

end;

procedure TJamFile.CreateNewJAM(FileName: string; Height: integer);

var
  sFilename: string;
begin

  FEntries := TList<TJamEntry>.Create;

  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));

  // CanvasBitmap := TBitmap.Create;
  canvasHeight := Height;

  SetLength(FRawData, 256 * Height);
  FHeader.NumItems := 0;
  FHeader.JamTotalHeight := Height;

  intJamMaxWidth := 256;
  intJamMaxHeight := Height;

  JamFileName := sFilename;
  JamFullPath := sFilename;
end;

procedure TJamFile.SetGpxPal(boolGP2: boolean);
var
  i: integer;
  c: TRGB;
begin
  if boolGP2 then
    for i := 0 to 255 do
      gpxPal[i] := gp2Pal[i]
  else
    for i := 0 to 255 do
      gpxPal[i] := gp3Pal[i];
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

procedure TJamFile.SaveDecryptedJam(const FileName: string);
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

HardcodedDims:
array [0 .. 6] of record Name: string;
Width, Height: integer;
IsRCR:
boolean;
end
= ((Name: 'mhill'; Width: 512; Height: 327; IsRCR: True), (Name: 'shill';
  Width: 148; Height: 31; IsRCR: True), (Name: 'car_srf'; Width: 512;
  Height: 1088; IsRCR: True), (Name: 'hlm_srf'; Width: 256; Height: 64;
  IsRCR: True), (Name: 'wh_srf'; Width: 512; Height: 512; IsRCR: True),
  (Name: 'vcp_srf'; Width: 512; Height: 512; IsRCR: True), (Name: 'vcp_srf2';
  Width: 512; Height: 512; IsRCR: True));

var
  Raw, Buf: TBytes;
  Ptr, i, BlockCount, TrueSize, palCount: integer;
  Info: TJamEntryInfo;
  sFilename: string;
  c: TRGB;
begin

  Result := False;

  for i := 0 to FEntries.Count - 1 do
    FEntries[i].free;
  FEntries.Clear;

  // Initial state
  boolRcrJam := False;
  boolJipMode := SameText(TPath.GetExtension(FileName), '.jip');
  intJamMaxWidth := 0;
  intJamMaxHeight := 0;

  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));
  //
  if sFilename = 'bars' then
    exit;
  // SaveDecryptedJam(FileName);
  //
  if sFilename = 'barm' then
    exit;

  // SaveDecryptedJam(FileName);

  if sFilename = 'shill' then
    exit;
  CheckIfRCR(FileName);

  // Load + decrypt
  Raw := TFile.ReadAllBytes(FileName);
  Buf := UnJam(Raw);
  Ptr := 0;

  // Header
  Move(Buf[Ptr], FHeader, SizeOf(FHeader));
  Inc(Ptr, SizeOf(FHeader));
  if FHeader.NumItems = 0 then
    FHeader.NumItems := 1;

  // Have to manually detect RCR2B for GP3, as there's RCR2B for GP2 which is a 'real RCR JAM' with double width/interlacing.
  // Whereas GP3 RCR2B is technically a 'normal' JAM file.

  if sFilename = 'rcr2b' then
    if FHeader.JamTotalHeight = 354 then
    begin
      boolRcrJam := False;
      ShowMessage('rcr2b GP3 detected'); // DEBUG
    end;

  // Parse Entries
  if not MatchText(sFilename, ['car_srf', 'hlm_srf', 'vcp_srf', 'vcp_srf2',
    'shill', 'mhill', 'bars', 'barm']) then
  begin
    for i := 0 to FHeader.NumItems - 1 do
    begin
      Move(Buf[Ptr], Info, SizeOf(Info));
      Inc(Ptr, SizeOf(Info));
      FEntries.Add(TJamEntry.Create(Info));
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
            SetLength(FPalettes[p], palCount);
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
  //
  // Override hardcoded JAM types
  for var H in HardcodedDims do
    if sFilename = H.Name then
    begin
      intJamMaxWidth := H.Width;
      intJamMaxHeight := H.Height;
      FHeader.JamTotalHeight := H.Height;
      boolRcrJam := H.IsRCR;
      // Break;
    end;

  // Extract raw pixel data
  BlockCount := FHeader.JamTotalHeight;
  TrueSize := BlockCount * 256;
  // if Ptr + TrueSize > Length(Buf) then
  // raise Exception.CreateFmt
  // ('JAM image data too short: need %d bytes at offset %d, have %d',
  // [TrueSize, Ptr, Length(Buf)]);

  FRawData := Copy(Buf, Ptr, TrueSize);

  canvasHeight := FHeader.JamTotalHeight;

  canvasWidth := 256;

  if boolRcrJam then
    canvasWidth := 512;

  // Generate textures
  if not MatchText(sFilename, ['car_srf', 'hlm_srf', 'vcp_srf', 'vcp_srf2',
    'shill', 'mhill', 'bars', 'barm']) then
    for i := 0 to FHeader.NumItems - 1 do
    begin
      with FEntries[i] do
      begin

        FTexture := DrawSingleTexture(FRawData, Length(FRawData), i, False);
        if preview = False then
        begin
          FOriginalTex := DrawPalTexture(i);
          CachePaletteBMP(i);
        end;
      end;

    end;

  if boolJipMode then
  DrawBaseCanvas(false)
  else
  DrawBaseCanvas(true);

  JamFileName := sFilename;
  JamFullPath := FileName;

  Result := True;
end;

procedure TJamFile.SaveToFile(const FileName: string; decrypt: boolean);
var
  ms: TMemoryStream;
  Entry: TJamEntry;
  palBytes, i: integer;
  buffer: TBytes;
begin
  // 0) Make sure your canvas encoding has run
  EncodeCanvas;

  ms := TMemoryStream.Create;
  try
    // 1) Header
    ms.WriteBuffer(FHeader, SizeOf(FHeader));

    // 2) All entry infos
    for Entry in FEntries do
      ms.WriteBuffer(Entry.FInfo, SizeOf(Entry.FInfo));

    // 3) Per entry palettes + textures
    for Entry in FEntries do
    begin
      palBytes := (Entry.FInfo.PaletteSizeDiv4);
      // Write each of the up-to-4 palettes
      for i := 0 to 3 do
      begin
        if palBytes > 0 then
        begin
          // // If that palette exists, write it; otherwise write zeros
          // if Length(entry.FPalettes[i]) >= palBytes then
          ms.WriteBuffer(Entry.FPalettes[i][0], palBytes)
          // else
          // ms.WriteBuffer(Pointer(AllocMem(palBytes))^, palBytes);
        end;
      end;

    end;

    // 4) Global raw data
    if Length(FRawData) > 0 then
      ms.WriteBuffer(FRawData[0], Length(FRawData));

    // 5) Extract to a TBytes to call UnJam
    SetLength(buffer, ms.Size);
    ms.Position := 0;
    ms.ReadBuffer(buffer[0], ms.Size);

    // 6) XOr
    if decrypt = False then
      buffer := UnJam(buffer);

    // 7) Write to disk
    TFile.WriteAllBytes(FileName, buffer);
  finally
    ms.free;
  end;
end;

procedure TJamFile.ImportCanvas(FileName: string);
var
  indices, tempRawCanvas: TBytes;
  X, Y, i, origidx, bestidx: integer;
  bmp: TBitmap;
  boolCorrectSize: boolean;
begin

  boolCorrectSize := False;

  bmp := TBitmap.Create;

  bmp.LoadFromFile(FileName);

  if (bmp.Height = canvasHeight) and (bmp.Width = canvasWidth) then
  begin
    indices := BitmapToIndices(bmp);

    SetLength(tempRawCanvas, canvasWidth * canvasHeight);

    for Y := 0 to canvasHeight - 1 do
      for X := 0 to canvasWidth - 1 do
      begin
        origidx := indices[Y * canvasWidth + X];
        bestidx := origidx;
        tempRawCanvas[Y * canvasWidth + X] := bestidx;
      end;

    FRawData := nil;
    SetLength(FRawData, canvasWidth * canvasHeight);
    for Y := 0 to canvasHeight - 1 do
    begin
      Move(tempRawCanvas[Y * canvasWidth], FRawData[Y * canvasWidth],
        canvasWidth);
    end;

    for i := 0 to FHeader.NumItems - 1 do
    begin

      ZeroPalette(i);
      with FEntries[i] do
      begin
        if assigned(FTexture) then
          FTexture.free;

        FTexture := DrawSingleTexture(FRawData, Length(FRawData), i, False);

        if assigned(FOriginalTex) then
          freeAndNil(FOriginalTex);
        FOriginalTex := DrawPalTexture(i);
        CachePaletteBMP(i);

      end;

    end;

  end
  else
  begin
    ShowMessage
      (Format('Bitmap file size incorrect - it should match the size of the JAM file. Bitmap is %d x %d and JAM is %d x %d',
      [bmp.Width, bmp.Height, canvasWidth, canvasHeight]));
    exit;
  end;

  bmp.free;
end;

procedure TJamFile.EncodeCanvas();
var
  Entry: TJamEntry;
  Info: TJamEntryInfo;
  W, H, canvasHeight, canvasWidth: integer;
  X0, Y0: integer;
  TempRaw, tempRawCanvas: TBytes;
  Y, pos: integer;
  tempBMP: TBitmap;
  i: integer;
  indices: TBytes;
  X, origidx: integer;
  bestidx: integer;

begin

  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := FEntries[i];
    Info := Entry.FInfo;

    W := Info.Width;
    H := Info.Height;
    X0 := Info.X;
    Y0 := Info.Y;

    TempRaw := Entry.FRawTexture;

    for Y := 0 to H - 1 do
    begin
      pos := (Y0 + Y) * 256 + X0; // srcStride = 256
      Move(TempRaw[Y * W], FRawData[pos], W);
    end;
  end;

  tempBMP := DrawRawJAM(FRawData);

  canvasWidth := 256;
  canvasHeight := FHeader.JamTotalHeight;

  // 1) grab the raw 8-bit indices from the bitmap
  indices := BitmapToIndices(tempBMP);

  SetLength(tempRawCanvas, canvasWidth * canvasHeight);

  // 3) Scan each pixel: find nearest GPxPal index, record in TempRaw
  for Y := 0 to canvasHeight - 1 do
    for X := 0 to canvasWidth - 1 do
    begin
      origidx := indices[Y * canvasWidth + X];
      bestidx := origidx;
      tempRawCanvas[Y * canvasWidth + X] := bestidx;
    end;

  // 7) Write TempRaw into the entry’s FRawTexture and the master FRawData

  FRawData := nil;
  SetLength(FRawData, canvasWidth * canvasHeight);
  for Y := 0 to canvasHeight - 1 do
  begin
    Move(tempRawCanvas[Y * canvasWidth], FRawData[Y * canvasWidth],
      canvasWidth);
  end;
end;

// Encodes a bitmap into indicied data (bytes)

procedure TJamFile.EncodeTexture(JamId: integer; texture: TBitmap);
var
  Entry: TJamEntry;
  Info: TJamEntryInfo;
  W, H: integer;
  indices, TempRaw: TBytes;
  X, Y, origidx: integer;
  bestidx: integer;

begin
  Entry := FEntries[JamId];
  Info := Entry.FInfo;
  W := Info.Width;
  H := Info.Height;

  // 1) grab the raw 8-bit indices from the bitmap
  indices := BitmapToIndices(texture);

  SetLength(TempRaw, W * H);
  // 3) Scan each pixel: find nearest GPxPal index, record in TempRaw
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
    begin
      origidx := indices[Y * W + X];
      bestidx := origidx;
      TempRaw[Y * W + X] := Byte(bestidx);
    end;

  // 7) Write TempRaw into the entry’s FRawTexture and the master FRawData

  SetLength(Entry.FRawTexture, W * H);
  for Y := 0 to H - 1 do
  begin
    Move(TempRaw[Y * W], Entry.FRawTexture[Y * W], W);
  end;

  if assigned(Entry.FTexture) then
    Entry.FTexture.free;

  Entry.FTexture := DrawSingleTexture(Entry.FRawTexture,
    Length(Entry.FRawTexture), JamId, True);
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
  tempY, tempX, prevY: integer;
begin

  if boolRcrJam then
    W := FEntries[JamId].FInfo.Width * 2
  else
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
    SrcStride := 256;

    if boolRcrJam then
    begin
      SrcStride := 512;
      X0 := FEntries[JamId].FInfo.X;
      if FEntries[JamId].FInfo.Y mod 2 <> 0 then
        X0 := X0 + 256;

      Y0 := FEntries[JamId].FInfo.Y div 2;
    end
    else
    begin

      X0 := FEntries[JamId].FInfo.X;
      Y0 := FEntries[JamId].FInfo.Y;
    end;

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
    bmp.canvas.lock;
    bmp.Width := W;
    bmp.Height := H;
    bmp.PixelFormat := pf8bit;
    bmp.Palette := CreateGPxPal;

    SetLength(FEntries[JamId].FRawTexture, W * H);

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
        bmp.canvas.Pixels[X, Y] := RGB(gpxPal[dst].r, gpxPal[dst].g,
          gpxPal[dst].b);
      end;
    bmp.canvas.Unlock;

    if boolRcrJam then
      Result := DeinterlaceRCR(bmp, boolRCRDrawMode)
    else
      Result := bmp;
  except
    bmp.free;
    raise;
  end;
end;


// caches all the palette bitmaps; in the event there's no palette the texture is applied to all for caches for safety.

procedure TJamFile.CachePaletteBMP(JamId: integer);
var
  originalPal, palCount, palIndex: integer;
  bmp: TBitmap;
begin
  // Remember the current palette so we can restore it
  originalPal := intPaletteID;
  try
    palCount := FEntries[JamId].FInfo.PaletteSizeDiv4;
    // If there is no local palette, always use palette 0
    if palCount = 0 then
      intPaletteID := 0;

    // Build / refresh all 4 cached textures
    for palIndex := 0 to 3 do
    begin
      // Temporarily select this palette
      intPaletteID := palIndex;

      // Generate a fresh paletted bitmap
      bmp := DrawPalTexture(JamId);
      try
        // Free any existing cache
        freeAndNil(FEntries[JamId].FCachedTex[palIndex]);
        // Create & assign the new one
        FEntries[JamId].FCachedTex[palIndex] := TBitmap.Create;
        FEntries[JamId].FCachedTex[palIndex].Assign(bmp);
      finally
        bmp.free; // cleanup
      end;
    end;
  finally
    // Restore the original palette index
    intPaletteID := originalPal;
  end;
end;


// Draws a texture with the local palette applied

procedure TJamFile.ReCacheTextures();
var
  i: integer;
begin

  for i := 0 to FEntries.Count - 1 do
    CachePaletteBMP(i);
end;

function TJamFile.DrawPalTexture(JamId: integer): TBitmap;
var
  W, H: integer;
  palCount, i, X, Y, idx, dst: integer;
  LocalPal: array [0 .. 255] of Byte;
  jamTex: TBitmap;
begin
  Result := nil;

  if JamId = -1 then
    exit;

  palCount := FEntries[JamId].FInfo.PaletteSizeDiv4;
  if palCount = 0 then
  begin
    Result := DrawSingleTexture(FEntries[JamId].FRawTexture,
      FHeader.JamTotalHeight, JamId, True);
    exit;
  end;

  W := FEntries[JamId].FInfo.Width;
  H := FEntries[JamId].FInfo.Height;

  jamTex := TBitmap.Create;
  try
    jamTex.canvas.lock;
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
        jamTex.canvas.Pixels[X, Y] := RGB(gpxPal[dst].r, gpxPal[dst].g,
          gpxPal[dst].b);
      end;

    jamTex.Palette := CreateGPxPal;
    jamTex.canvas.Unlock;
    Result := jamTex;
  except
    jamTex.free;
    raise;
  end;
end;

// Draws Jam Canvas from the fully updated whole Jam Raw Data - partly a QC check process.
function TJamFile.RenderJamCanvas(UIUpdate: boolean): TBitmap;
var
  JamBMP, entryBmp, tempBMP: TBitmap;
  i: integer;
begin
  JamBMP := TBitmap.Create;
  try
    JamBMP.canvas.lock;
    JamBMP.PixelFormat := pf8bit;
    JamBMP.Palette := CreateGPxPal;
    JamBMP.SetSize(256, FHeader.JamTotalHeight);

    JamBMP.canvas.Brush.Color := RGB(gpxPal[0].r, gpxPal[0].g, gpxPal[0].b);
    JamBMP.canvas.FillRect(Rect(0, 0, JamBMP.Width, JamBMP.Height));

    if boolRcrJam then
    begin
      JamBMP.SetSize(512, FHeader.JamTotalHeight);
      entryBmp := DrawFullRCR(FRawData, boolRCRDrawMode, True);
      try
        JamBMP.canvas.Draw(0, 0, entryBmp);
      finally
        entryBmp.free;
      end;
      JamBMP.canvas.Unlock;
      Result := JamBMP;
      exit;
    end
    else if boolJipMode then
    begin
      entryBmp := DrawRawJAM(FRawData);
      try
        JamBMP.SetSize(256, FHeader.JamTotalHeight);

        intJamMaxHeight := FHeader.JamTotalHeight;
        JamBMP.canvas.Draw(0, 0, entryBmp);
      finally
        entryBmp.free;
      end;
      JamBMP.canvas.Unlock;
      Result := JamBMP;
      exit;
    end;

    // Standard JAM
    for i := 0 to FHeader.NumItems - 1 do
    begin
      tempBMP := DrawSingleTexture(FRawData, Length(FRawData), i, False);
      try
        JamBMP.canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y, tempBMP);
      finally
        JamBMP.canvas.Unlock;
        tempBMP.free;
      end;
    end;

    Result := JamBMP;
  except
    JamBMP.free;
    raise;
  end;
end;

function TJamFile.DrawFullJam(UIUpdate: boolean): TBitmap;
var
  JamBMP, entryBmp, stretchedBmp: TBitmap;
  i: integer;
begin
  JamBMP := TBitmap.Create;
  JamBMP.canvas.lock;
  try
    JamBMP.PixelFormat := pf8bit;
    JamBMP.Palette := CreateGPxPal;
    JamBMP.Width := 256;
    JamBMP.Height := FHeader.JamTotalHeight;
    JamBMP.Canvas.Draw(0,0,originalCanvas);

   if boolRcrJam then
    begin
      JamBMP.Width := 512;
      JamBMP.Height := FHeader.JamTotalHeight div 2;
      entryBmp := DrawFullRCR(FRawData, boolRCRDrawMode, True);
      try
        JamBMP.canvas.Draw(0, 0, entryBmp);
      finally
        JamBMP.canvas.Unlock;
        entryBmp.free;
      end;
      Result := JamBMP;
      exit;
    end

    else

    begin

      for i := 0 to FEntries.Count - 1 do
      begin
        if UIUpdate then
        begin
          entryBmp := FEntries[i].FCachedTex[intPaletteID];
          if not assigned(entryBmp) then
            continue;
          stretchedBmp := resizeTransProtection(entryBmp,
            FEntries[i].Info.Width, FEntries[i].Info.Height,
            RGBFromTRGB(gpxPal[0]));
          try
            JamBMP.canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y,
              stretchedBmp);
          finally
            JamBMP.canvas.Unlock;
            freeAndNil(stretchedBmp);
          end;
        end
        else
        begin
          entryBmp := DrawSingleTexture(FEntries[i].FRawTexture,
            Length(FEntries[i].FRawTexture), i, True);
          try
            JamBMP.canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y,
              entryBmp);
          finally
            JamBMP.canvas.Unlock;
            entryBmp.free;
          end;
        end;
      end;
    end;

    Result := JamBMP;
  except
    JamBMP.free;
    raise;
  end;
end;

function TJamFile.DrawRawJAM(const Raw: TBytes): TBitmap;
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
    bmp.free;
    raise;
  end;
end;

function TJamFile.DrawFullRCR(const Raw: TBytes; odd: boolean;
  halfHeight: boolean): TBitmap;
var
  i, canvasWidth, canvasHeight: integer;
  tempRCR: TBitmap;
begin
  canvasWidth := 512;
  if halfHeight then
    canvasHeight := FHeader.JamTotalHeight div 2
  else
    canvasHeight := FHeader.JamTotalHeight;

  if Length(Raw) < canvasWidth * canvasHeight then
    raise Exception.CreateFmt('DrawFullRCR: Raw data too short (%d < %d)',
      [Length(Raw), canvasWidth * canvasHeight]);

  tempRCR := TBitmap.Create;
  try
    tempRCR.PixelFormat := pf8bit;
    tempRCR.Palette := CreateGPxPal;
    tempRCR.SetSize(canvasWidth, canvasHeight);

    for i := 0 to canvasHeight - 1 do
      Move(Raw[i * canvasWidth], tempRCR.ScanLine[i]^, canvasWidth);

    Result := DeinterlaceRCR(tempRCR, odd);

  except
    Result.free;
    raise;
  end;
end;

function TJamFile.DrawOutlines(JamCanvas: TBitmap): TBitmap;
var
  i: integer;
  tmpBMP: TBitmap;
  tempX, prevY, tempY: integer;
begin
  if not boolDrawOutlines then
    exit(JamCanvas);

  // Clone
  tmpBMP := TBitmap.Create;
  tmpBMP.Assign(JamCanvas);
  try
    // Draw on the clone
    for i := 0 to FEntries.Count - 1 do
      with FEntries[i].Info do
      begin
        tempX := X;
        tempY := Y;

        if boolRcrJam then
        begin
          if Y mod 2 <> 0 then
          begin
            // prevY := FEntries[i - 1].FInfo.Y;
            tempY := tempY - 1;
            tempX := tempX + 256;
          end
          else
          begin
            tempY := Y;
            tempX := X;
          end;

          tempY := tempY div 2;
          DrawTextureOutlines(tmpBMP, tempX, tempY, Width * 2, Height, i, JamId)
        end
        else
          DrawTextureOutlines(tmpBMP, X, Y, Width, Height, i, JamId);
      end;
    // Hand ownership to the caller
    Result := tmpBMP;
    tmpBMP := nil;
  finally
    tmpBMP.free; // frees only if an exception occurred
  end;
end;

procedure TJamFile.AddTexture(textureFilename: string);
var
  srcPic: TPicture;
  Info: TJamEntryInfo;
  newTex: TJamEntry;
  X, i: integer;
  tmpCanvas, scaledCanvas, tmpBMP: TBitmap;
  transBool: boolean;
begin
  srcPic := TPicture.Create;
  tmpCanvas := TBitmap.Create;
  tmpBMP := TBitmap.Create;
  scaledCanvas := nil;
  try
    srcPic.LoadFromFile(textureFilename);
    Info.X := 0;
    Info.Y := 0;
    Info.Width := srcPic.Width;
    Info.Height := srcPic.Height;

    Info.scaleX := 0;
    Info.scaleY := 0;
    Info.scaleFlag := 0;
    Info.scaleFactor := 0;

    Info.Idx16 := 0;
    Info.Idx17 := 0;
    Info.Unk := 0;
    Info.idx0e := 0;

    for i := 0 to 7 do
      Info.Idx18[i] := 0;

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
      Info.Height, RGBFromTRGB(gpxPal[0]));

    newTex.FOriginalTex.Assign(scaledCanvas);
    newTex.FTexture.Assign(scaledCanvas);

    for X := 0 to 3 do
      newTex.FPalettes[X] := nil;

    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    X := FEntries.Count - 1;
    intSelectedTexture := X;

    if boolJipMode = False then
      FEntries[X].FTexture := GenerateGPxBMP(scaledCanvas, X, intSimplifyMethod,
        intSimplifyThreshold, intBlurThreshold, boolSimpifyAllPals,
        boolProtectTrans)
    else
    begin
      tmpBMP.Assign(newTex.FTexture);
      tmpBMP.PixelFormat := pf8bit;
      tmpBMP := CreateGPxPalBMP(newTex.FTexture);
      tmpBMP.Palette := CreateGPxPal;
      EncodeTexture(X, tmpBMP);
    end;
    CachePaletteBMP(X);

    FEntries[X].FInfo.JamFlags := 0;

    transBool := DetectTransCol(FEntries[X].FCachedTex[0]);

    if not isPowerOfTwo(FEntries[X].FTexture.Width) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 9);

    if not isPowerOfTwo(FEntries[X].FTexture.Height) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 8);

    if transBool then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 3);

  finally
    srcPic.free;
    tmpCanvas.free;
    scaledCanvas.free;
    if boolJipMode then
      tmpBMP.free;
  end;
end;

procedure TJamFile.DeleteTexture(JamId: integer);
begin
  FEntries.Delete(JamId);
  Dec(FHeader.NumItems);
end;

function TJamFile.AddTexture(bmp: TBitmap; newinfo: TJamEntryInfo;
  palGen: boolean): integer;
var
  Info: TJamEntryInfo;
  newTex: TJamEntry;
  X, Y: integer;
  tmpCanvas, scaledCanvas: TBitmap;
  transBool: boolean;
  tmpBMP: TBitmap;
begin
  tmpCanvas := TBitmap.Create;
  tmpBMP := TBitmap.Create;
  tmpCanvas.Assign(bmp);
  scaledCanvas := nil;
  try

    Info.X := newinfo.X;
    Info.Y := newinfo.Y;
    Info.Width := newinfo.Width;
    Info.Height := newinfo.Height;
    Info.JamId := newinfo.JamId;
    Info.JamFlags := newinfo.JamFlags;
    Info.Unk := newinfo.Unk;
    Info.scaleX := newinfo.scaleX;
    Info.scaleY := newinfo.scaleY;
    Info.scaleFlag := newinfo.scaleFlag;
    Info.scaleFactor := newinfo.scaleFactor;
    Info.Idx16 := newinfo.Idx16;
    Info.Idx17 := newinfo.Idx17;

    for Y := 0 to 7 do
      Info.Idx18[Y] := newinfo.Idx18[Y];

    Info.PaletteSizeDiv4 := 0;

    newTex := TJamEntry.Create(Info);
    newTex.FOriginalTex := TBitmap.Create;
    newTex.FTexture := TBitmap.Create;

    newTex.FOriginalTex.SetSize(Info.Width, Info.Height);
    newTex.FTexture.SetSize(Info.Width, Info.Height);

    tmpCanvas := ReplaceTransparentColour(tmpCanvas, RGBFromTRGB(gpxPal[0]));

    scaledCanvas := resizeTransProtection(tmpCanvas, Info.Width, Info.Height,
      RGBFromTRGB(gpxPal[0]));

    newTex.FOriginalTex.Assign(scaledCanvas);
    newTex.FTexture.Assign(scaledCanvas);

    for X := 0 to 3 do
      newTex.FPalettes[X] := nil;

    FEntries.Add(newTex);
    Inc(FHeader.NumItems);

    X := FEntries.Count - 1;
    intSelectedTexture := X;

    if palGen then
      FEntries[X].FTexture := GenerateGPxBMP(scaledCanvas, X, intSimplifyMethod,
        intSimplifyThreshold, intBlurThreshold, boolSimpifyAllPals,
        boolProtectTrans)
    else
    begin
      tmpBMP.Assign(newTex.FTexture);
      tmpBMP.PixelFormat := pf8bit;
      tmpBMP := CreateGPxPalBMP(newTex.FTexture);
      tmpBMP.Palette := CreateGPxPal;
      EncodeTexture(X, tmpBMP);
    end;

    FEntries[X].FOriginalTex := CreateGPxPalBMP(scaledCanvas);

    CachePaletteBMP(X);

    if not isPowerOfTwo(FEntries[X].FTexture.Width) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 9);

    if not isPowerOfTwo(FEntries[X].FTexture.Height) then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 8);

    if transBool then
      FEntries[X].FInfo.JamFlags := PackFlag(FEntries[X].FInfo.JamFlags, 3);

  finally
    tmpCanvas.free;
    tmpBMP.free;
    scaledCanvas.free;
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
  tmpCanvas.canvas.lock;
  scaledCanvas := nil;
  textureWidth := FEntries[JamId].Info.Width;
  textureHeight := FEntries[JamId].Info.Height;

  try
    srcPic.LoadFromFile(textureFilename);
    tmpCanvas.PixelFormat := pf24bit;
    tmpCanvas.SetSize(textureWidth, textureHeight);

    scaledCanvas := resizeTransProtection(srcPic.bitmap, textureWidth,
      textureHeight, RGBFromTRGB(gpxPal[0]));
    tmpCanvas.canvas.StretchDraw(Rect(0, 0, textureWidth, textureHeight),
      scaledCanvas);

    tmpCanvas.canvas.Unlock;

    if assigned(FEntries[JamId].FTexture) then
      FEntries[JamId].FTexture.free;
    FEntries[JamId].FTexture := GenerateGPxBMP(tmpCanvas, JamId,
      intSimplifyMethod, intSimplifyThreshold, intBlurThreshold,
      boolSimpifyAllPals, boolProtectTrans);

    if assigned(FEntries[JamId].FTexture) then
      FEntries[JamId].FOriginalTex.free;

    FEntries[JamId].FOriginalTex := CreateGPxPalBMP(tmpCanvas);
    CachePaletteBMP(JamId);
  finally
    srcPic.free;
    tmpCanvas.free;
    if assigned(scaledCanvas) then
      scaledCanvas.free;
  end;
end;

procedure TJamFile.ExportTexture(JamId: integer; textureFilename: string);
var
  exportPic: TPicture;
begin
  exportPic := TPicture.Create;
  exportPic.bitmap := FEntries[JamId].FTexture;
  exportPic.SaveToFile(textureFilename);
  freeAndNil(exportPic);
end;

procedure TJamFile.ExportCanvas(FileName: string);
var
  exportPic: TPicture;
begin
  exportPic := TPicture.Create;
  exportPic.bitmap := RenderJamCanvas(False);
  exportPic.SaveToFile(FileName);

  freeAndNil(exportPic);

end;

procedure TJamFile.ZeroPalette(JamId: integer);
var
  tmpBMP, resizedBMP: TBitmap;
begin
  tmpBMP := TBitmap.Create;

  resizedBMP := resizeTransProtection(FEntries[JamId].FOriginalTex,
    FEntries[JamId].FInfo.Width, FEntries[JamId].FInfo.Height,
    RGBFromTRGB(gpxPal[0]));
  try
    tmpBMP.Assign(resizedBMP);
    tmpBMP.PixelFormat := pf8bit;
    tmpBMP := CreateGPxPalBMP(resizedBMP);
    tmpBMP.Palette := CreateGPxPal;
    EncodeTexture(JamId, tmpBMP);

    FEntries[JamId].FInfo.ImagePtr := 0;

    FEntries[JamId].PaletteSizeDiv4 := 0;
    for var i := 0 to 3 do
      FEntries[JamId].FPalettes[i] := nil;

    CachePaletteBMP(JamId);
  finally
    tmpBMP.free;
    resizedBMP.free;
  end;
end;

function TJamFile.GenerateGPxBMP(bitmap: TBitmap; JamId: integer;
  method: integer; simplifyThreshold: integer; BlurThreshold: integer;
  allPals: boolean; protectMatte: boolean): TBitmap;
var
  SrcRGB: TBitmap;
  L1Idx, L2Idx, L3Idx, L4Idx: TBitmap;
  L1RGB, L2RGB, L3RGB, L4RGB: TBitmap;
  i, j: integer;
  PalSize2: integer;
  LP2: PLogPal2;
  hPalTemp: HPALETTE;
  X, Y, H, W, k, idx: integer;
  indices: TBytes;
  tmpBMP: TBitmap;
  Mask: TBoolGrid;
  tempColour: TRGB;
  jamPalSize: integer;
  maskBMP: TBitmap;
  maskBMP2: TBitmap;
  maskBMP3: TBitmap;
  maskBMP4: TBitmap;
  singleIDXMap: TBitmap;

  LastColor: TColor;
  RepeatCount, TrimSize: integer;
  CanTrim: boolean;
  PaletteLen, Level: integer;
  TrimFrom: array [0 .. 3] of integer;
  CurrentColor, CompareColor: TColor;
  RepeatStart: integer;
begin

  for i := 0 to 3 do
    if assigned(LevelIdx[i]) then
      freeAndNil(LevelIdx[i]);

  for i := 0 to 3 do
  begin
    LevelIdx[i] := TBitmap.Create;
    LevelIdx[i].PixelFormat := pf8bit;
  end;

  singleIDXMap := TBitmap.Create;
  singleIDXMap.PixelFormat := pf8bit;

  for i := 0 to 3 do
    SetLength(PalPerLevel[i], 256);

  SrcRGB := bitmap;

  try

    if SrcRGB.PixelFormat <> pf24bit then
      SrcRGB.PixelFormat := pf24bit;

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

    maskBMP := CreateTransparencyMatte(SrcRGB);
    maskBMP2 := maskBMP;
    maskBMP3 := maskBMP;
    maskBMP4 := maskBMP;

    L1Idx := CreateGPxPalBMP(SrcRGB, maskBMP);

    LevelIdx[0].Assign(L1Idx);

    IndexedTo24bit(L1Idx, L1RGB);

    BleedEdges(L1RGB, maskBMP, RGBFromTRGB(gpxPal[0]), 4);

    L1RGB := ApplyMatteToImage(L1RGB, maskBMP, RGBFromTRGB(gpxPal[0]));

    // Blur chain: L1RGB→L2RGB→L3RGB→L4RGB
    GaussianBlur(L1RGB, Mask, intBlurThreshold, L2RGB);
    GaussianBlur(L2RGB, Mask, intBlurThreshold, L3RGB);
    GaussianBlur(L3RGB, Mask, intBlurThreshold, L4RGB);

    GaussianBlur(maskBMP2, Mask, intBlurThreshold, maskBMP2);
    GaussianBlur(maskBMP3, Mask, intBlurThreshold, maskBMP3);
    GaussianBlur(maskBMP4, Mask, intBlurThreshold, maskBMP4);

    L2RGB := ApplyMatteToImage(L2RGB, maskBMP2, RGBFromTRGB(gpxPal[0]));
    L3RGB := ApplyMatteToImage(L3RGB, maskBMP3, RGBFromTRGB(gpxPal[0]));
    L4RGB := ApplyMatteToImage(L4RGB, maskBMP4, RGBFromTRGB(gpxPal[0]));

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
    SrcRGB.free;
    L1RGB.free;
    L2RGB.free;
    L3RGB.free;
    L4RGB.free;
  end;

  // Build single‐index map + custom palettes
  BuildBmpIdxPal([LevelIdx[0], LevelIdx[1], LevelIdx[2], LevelIdx[3]],
    singleIDXMap, PalPerLevel);

  /// ///////////////////
  // Check for trailing repeated colors across all palettes
  PaletteLen := Length(PalPerLevel[0]); // assume all same to start

  // for Level := 0 to 3 do
  // begin
  // TrimFrom[Level] := PaletteLen; // default: no trimming
  //
  // // Scan backwards from the end
  // for i := PaletteLen - 2 downto 0 do
  // begin
  // CurrentColor := PalPerLevel[Level][i + 1];
  // RepeatCount := 1;
  //
  // // Count how many times this same color repeats backwards
  // for j := i downto 0 do
  // begin
  // CompareColor := PalPerLevel[Level][j];
  // if CompareColor = CurrentColor then
  // Inc(RepeatCount)
  // else
  // Break;
  // end;
  //
  // if RepeatCount >= 4 then
  // begin
  // // Found a run of 4+ repeated colors: trim from here forward
  // RepeatStart := i + 2 - RepeatCount;
  // TrimFrom[Level] := RepeatStart + 1; // keep 1 instance
  // Break;
  // end;
  // end;
  // end;
  //
  // // Final length is min of all 4
  // PaletteLen := Min(Min(TrimFrom[0], TrimFrom[1]), Min(TrimFrom[2], TrimFrom[3]));
  //
  // // Trim all palettes to unified length
  for Level := 0 to 3 do
    SetLength(PalPerLevel[Level], PaletteLen);

  /// ///////////////////

  jamPalSize := FEntries[JamId].PaletteSizeDiv4;

  // if FEntries[JamId].PaletteSizeDiv4 < Length(PalPerLevel[0]) then
  // begin
  FEntries[JamId].PaletteSizeDiv4 := Length(PalPerLevel[0]);
  SetLength(FEntries[JamId].FPalettes[0], Length(PalPerLevel[0]));
  SetLength(FEntries[JamId].FPalettes[1], Length(PalPerLevel[0]));
  SetLength(FEntries[JamId].FPalettes[2], Length(PalPerLevel[0]));
  SetLength(FEntries[JamId].FPalettes[3], Length(PalPerLevel[0]));

  for i := jamPalSize to Length(PalPerLevel[0]) - 1 do
  begin
    FEntries[JamId].FPalettes[0][i] := 0;
    FEntries[JamId].FPalettes[1][i] := 0;
    FEntries[JamId].FPalettes[2][i] := 0;
    FEntries[JamId].FPalettes[3][i] := 0;
  end;
  // end;

  for i := 0 to 3 do
  begin
    for j := 0 to FEntries[JamId].PaletteSizeDiv4 - 1 do
    begin
      tempColour.r := GetRValue(PalPerLevel[i][j]);
      tempColour.g := GetGValue(PalPerLevel[i][j]);
      tempColour.b := GetBValue(PalPerLevel[i][j]);
      FEntries[JamId].FPalettes[i][j] := Byte(findGPxCol(gpxPal, tempColour));
    end;
  end;

  EncodeTexture(JamId, singleIDXMap);

  singleIDXMap.Palette := CreateGPxPal;

  Result := singleIDXMap;

  for i := 0 to FEntries.Count - 1 do

    if i = 0 then
      FEntries[i].FInfo.ImagePtr := 0
    else
    begin
      FEntries[i].FInfo.ImagePtr := FEntries[i - 1].FInfo.ImagePtr +
        (FEntries[i].Info.PaletteSizeDiv4 * 4);
    end;

  L1Idx.free;
  L2Idx.free;
  L3Idx.free;
  L4Idx.free;
  maskBMP.free;
  maskBMP2.free;
  maskBMP3.free;
  maskBMP4.free;

end;

procedure TJamFile.ConvertHWJam(hwJam: THWJamFile; gp2Pal: boolean);
var
  i, j: integer;
  newTexture: TJamEntry;
  newinfo: TJamEntryInfo;
begin

  if gp2Pal then
    SetGpxPal(True)
  else
    SetGpxPal(False);

  canvasHeight := hwJam.FHeader.JamTotalHeight;

  // CanvasBitmap.Height := canvasHeight;
  // CanvasBitmap.Width := 256;

  FHeader.JamTotalHeight := hwJam.FHeader.JamTotalHeight;

  SetLength(FRawData, 256 * hwJam.FHeader.JamTotalHeight);

  intJamMaxWidth := 256;
  intJamMaxHeight := hwJam.FHeader.JamTotalHeight;

  for i := 0 to hwJam.FEntries.Count - 1 do
  begin
    newinfo.X := hwJam.FEntries[i].FInfo.X;
    newinfo.Y := hwJam.FEntries[i].FInfo.Y;
    newinfo.Width := hwJam.FEntries[i].FInfo.Width;
    newinfo.Height := hwJam.FEntries[i].FInfo.Height;
    newinfo.JamId := hwJam.FEntries[i].FInfo.JamId;
    newinfo.JamFlags := hwJam.FEntries[i].FInfo.JamFlags;

    newinfo.scaleX := hwJam.FEntries[i].FInfo.scaleX;
    newinfo.scaleY := hwJam.FEntries[i].FInfo.scaleY;
    newinfo.scaleFlag := hwJam.FEntries[i].FInfo.scaleFlags;
    newinfo.scaleFactor := hwJam.FEntries[i].FInfo.scaleFactor;

    newinfo.Unk := 0;

    // newinfo.Idx08 := 0;
    // newinfo.Idx0A := 0;

    newinfo.idx0e := 0;

    newinfo.Idx16 := 0;
    newinfo.Idx17 := 0;

    for j := 0 to 7 do

      newinfo.Idx18[j] := 0;

    // detect transparency and if so do not create palettes

    AddTexture(hwJam.FEntries[i].FTexture, newinfo, True);

    for j := 0 to FEntries.Count - 1 do

      with FEntries[j] do
        FInfo.JamFlags := hwJam.FEntries[j].FInfo.JamFlags;
  end;

  FHeader.NumItems := hwJam.FHeader.NumItems;

end;

procedure TJamFile.ConvertGPxJam(origJam: TJamFile; gp2Pal: boolean);
var
  i, j: integer;
  newTexture: TJamEntry;
  buildPal: boolean;
begin

  if gp2Pal then
    SetGpxPal(True)
  else
    SetGpxPal(False);

  buildPal := False;
  canvasHeight := origJam.FHeader.JamTotalHeight;
  canvasWidth := 256;

  // CanvasBitmap.Height := canvasHeight;

  FHeader.JamTotalHeight := origJam.FHeader.JamTotalHeight;
  SetLength(FRawData, 256 * origJam.FHeader.JamTotalHeight);

  intJamMaxWidth := 256;
  intJamMaxHeight := origJam.FHeader.JamTotalHeight;

  for i := 0 to origJam.FHeader.NumItems - 1 do
  begin
    if origJam.FEntries[i].PaletteSizeDiv4 > 1 then
      buildPal := True;
    AddTexture(origJam.FEntries[i].FTexture, origJam.FEntries[i].FInfo,
      buildPal);
    buildPal := False;
  end;

  for j := 0 to origJam.FHeader.NumItems - 1 do
    with FEntries[j] do
      FInfo.JamFlags := origJam.FEntries[j].FInfo.JamFlags;

  FHeader.NumItems := origJam.FHeader.NumItems;

end;

procedure TJamFile.ResizeJam(originalHeight: integer);
var
  i, j: integer;
  newTexture: TJamEntry;
  buildPal: boolean;
  ratio: double;
  tmpCanvas: TBitmap;
  bmpPal: TBitmap;
begin

  ratio := 384 / originalHeight;

  canvasHeight := round(originalHeight * ratio);

  // CanvasBitmap.Height := canvasHeight;

  intJamMaxHeight := canvasHeight;

  tmpCanvas := TBitmap.Create;

  for i := 0 to FHeader.NumItems - 1 do
  begin
    with FEntries[i] do
    begin
      if PaletteSizeDiv4 > 1 then
        buildPal := True;
      FInfo.X := round(Info.X * ratio);
      FInfo.Y := round(Info.Y * ratio);
      FInfo.Height := round(Info.Height * ratio);
      FInfo.Width := round(Info.Width * ratio);

      try
        tmpCanvas.Assign(FOriginalTex);
        tmpCanvas := StretchF(tmpCanvas, FInfo.Width, FInfo.Height);

        if buildPal then
          FTexture := GenerateGPxBMP(tmpCanvas, i, intSimplifyMethod,
            intSimplifyThreshold, intBlurThreshold, boolSimpifyAllPals,
            boolProtectTrans)
        else
        begin
          bmpPal := TBitmap.Create;
          bmpPal.Assign(tmpCanvas);
          bmpPal.PixelFormat := pf8bit;
          bmpPal := CreateGPxPalBMP(tmpCanvas);
          bmpPal.Palette := CreateGPxPal;
          EncodeTexture(i, bmpPal);
          bmpPal.free;
        end;
        CachePaletteBMP(i);
      finally

      end;
      buildPal := False;
    end;
  end;
  tmpCanvas.free;
  FHeader.JamTotalHeight := canvasHeight;
  SetLength(FRawData, 256 * canvasHeight);

end;

function TJamFile.GetIDX08_X(idx08: Word): Byte;
begin
  // result :=
  Result := idx08 and $00FF; // lower byte
end;

function TJamFile.GetIDX08_Y(idx08: Word): Byte;
begin
  Result := (idx08 shr 8) and $00FF; // upper byte
end;

function TJamFile.SetIDX08(X, Y: Byte): Byte;
begin
  Result := (Word(Y) shl 8) or X;
end;

function TJamFile.GetIDX0aFlags(idx0a: Word): Byte;
begin
  Result := idx0a and $00FF; // Lower byte = flags
end;

function TJamFile.GetIDX0aScale(idx0a: Word): Byte;
begin
  Result := (idx0a shr 8) and $00FF; // Upper byte = scale
end;

function TJamFile.SetIDX0a(flags, scale: Byte): Word;
begin
  Result := (Word(scale) shl 8) or flags;
end;

procedure TJamFile.BuildRect_SW(Jam: TJamFile; var Rects: TArray<TJamRect>);
var
  i: integer;
begin
  SetLength(Rects, Jam.FHeader.NumItems);

  for i := 0 to Jam.FHeader.NumItems - 1 do
  begin
    Rects[i].X := Jam.FEntries[i].FInfo.X;
    Rects[i].Y := Jam.FEntries[i].FInfo.Y;
    Rects[i].Width := Jam.FEntries[i].FInfo.Width;
    Rects[i].Height := Jam.FEntries[i].FInfo.Height;
    Rects[i].JamId := Jam.FEntries[i].FInfo.JamId;
    Rects[i].Index := i;
  end;
end;

procedure TJamFile.ApplyRects_SW(Jam: TJamFile; const Rects: TArray<TJamRect>);
var
  i: integer;
begin
  for i := 0 to High(Rects) do
  begin
    Jam.FEntries[Rects[i].Index].FInfo.X := Rects[i].X;
    Jam.FEntries[Rects[i].Index].FInfo.Y := Rects[i].Y;
  end;
end;

procedure TJamFile.DrawBaseCanvas(clean: boolean);
var
  JamBMP: TBitmap;
  i: integer;
begin

     JamBMP := TBitmap.Create;
      JamBMP.PixelFormat := pf8bit;
      JamBMP.Palette := CreateGPxPal;
      JamBMP.Width := 256;
      JamBMP.Height := FHeader.JamTotalHeight;

  try
    if clean then
    begin
       JamBMP.Canvas.Lock;
      try
        JamBMP.Canvas.Brush.Color := RGB(gpxPal[0].r, gpxPal[0].g, gpxPal[0].b);
        JamBMP.Canvas.FillRect(Rect(0, 0, JamBMP.Width, JamBMP.Height));
      finally
        JamBMP.Canvas.Unlock;
      end;
    end
    else
    begin
      JamBMP := DrawRawJAM(FRawData); // assume this returns a NEW bitmap

      JamBMP.Height := FHeader.JamTotalHeight;
      intJamMaxHeight := FHeader.JamTotalHeight;

      JamBMP.Canvas.Lock;
      try
        for i := 0 to FEntries.Count - 1 do
        begin
            JamBMP.Canvas.Brush.Color := RGB(gpxPal[0].r, gpxPal[0].g, gpxPal[0].b);
          JamBMP.Canvas.FillRect(Rect(
            FEntries[i].FInfo.X,
            FEntries[i].FInfo.Y,
            FEntries[i].FInfo.X + FEntries[i].FInfo.Width,
            FEntries[i].FInfo.Y + FEntries[i].FInfo.Height
          ));
        end;
      finally
        JamBMP.Canvas.Unlock;
      end;
    end;

    originalCanvas := TBitmap.create;
    originalCanvas.Width := JamBMP.Width;
    originalCanvas.Height := JamBMP.Height;
    originalCanvas.PixelFormat := JamBMP.PixelFormat;
    originalCanvas.Palette := CreateGPxPal;

    originalCanvas.Canvas.Draw(0,0,JamBmp);

  finally
    JamBMP.Free;
  end;
end;


end.
