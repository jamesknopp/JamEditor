unit JamSW;

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

  TJamEntry = class
  public
    FInfo: TJamEntryInfo;
    FPalettes: TLocalPaletteArray;
    FTexture: TBitmap;
    FOriginalTex: TBitmap;
    FRawTexture: TBytes;
    boolImportedBMP: boolean;
    function GetPalette(Index: Integer): TBytes;
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
    SingleIdxMap: TBitmap;
    canvasHeight, canvasWidth: Integer;
    CanvasBitmap: TBitmap;
    JamFileName: string;

    function UnJam(const Data: TBytes): TBytes;
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure DecryptSWJam(const FileName: string);
//    procedure PopulateTree(Tree: TTreeView);

    procedure EncodeTexture(JamId: Integer; texture: TBitmap);

    procedure AddTexture(textureFilename: string);

    procedure ImportTexture(JamId: Integer; textureFilename: string);
    procedure ExportTexture(JamId: Integer; textureFilename: string);

    procedure ZeroPalette(JamId: Integer);

    function GenerateGPxBMP(bitmap: TBitmap; JamId: Integer): TBitmap;


    function DrawSingleTexture(const Raw: TBytes; TotalImageSize: Integer;
      JamId: Integer; drawFromEntry : boolean): TBitmap;
    function DrawPalTexture(JamId: Integer): TBitmap;

    function DrawFullJam(): TBitmap;
    function DrawFullJIP(const Raw: TBytes): TBitmap;
    function DrawFullRCR(const Raw: TBytes): TBitmap;

    function DrawOutlines(JamCanvas: TBitmap): TBitmap;


    property Entries: TList<TJamEntry> read FEntries write FEntries;
  end;

implementation
{ TJamEntry }

constructor TJamEntry.Create(const Info: TJamEntryInfo);
begin
  FInfo := Info;
  intPaletteID := 0;
end;

destructor TJamEntry.Destroy;
begin
   FTexture.Free;
   FOriginalTex.free;
   FRawTexture := nil;
  inherited;
end;

function TJamEntry.GetPalette(Index: Integer): TBytes;
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
begin
  CanvasBitmap.free;
  SingleIdxMap.free;
  FEntries.Free;
  FRawData := nil;
  inherited;
end;

function TJamFile.UnJam(const Data: TBytes): TBytes;
var
  X: Cardinal;
  n, i: Integer;
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

procedure TJamFile.LoadFromFile(const FileName: string);
var
  Raw, Buf: TBytes;
  Ptr: Integer;
  i: Integer;
  Info: TJamEntryInfo;
  BlockCount, TrueSize: Integer;
  sFilename: string;
  palCount : integer;
  rawPos : integer;
begin
  boolRcrJam := False;
  boolJipMode := False;
  intjamMaxWidth := 0;
  intJamMaxHeight := 0;
  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));

  if sFilename = 'shill' then exit;


  if TPath.GetExtension(FileName) = '.jip' then
    boolJipMode := True;

  CheckIfRCR(sFilename); // check if RCR JAM

  // Read & decrypt all file bytes
  Raw := TFile.ReadAllBytes(FileName);
  Buf := UnJam(Raw);
  Ptr := 0;

  // DecryptSWJam(filename);  Output decrypted JAM

  // Read header
  Move(Buf[Ptr], FHeader, SizeOf(FHeader));
  Inc(Ptr, SizeOf(FHeader));

  // there is always one texture - lets ensure that

  if FHeader.NumItems = 0 then
    FHeader.NumItems := 1;

  // handle odd jam files
  if not((sFilename = 'car_srf') or (sFilename = 'hlm_srf') or
    (sFilename = 'vcp_srf') or (sFilename = 'vcp_srf2') or (sFilename = 'shill') or (sFilename = 'mhill')) then
  begin
    // Read entries and add them to the list
    for i := 0 to FHeader.NumItems - 1 do
    begin
      Move(Buf[Ptr], Info, SizeOf(Info));
      Inc(Ptr, SizeOf(Info));
      FEntries.Add(TJamEntry.Create(Info));
    end;

    // if RCR then we apply special info
    if boolRcrJam then
    begin
    if ((sFilename = 'rcr1a') or (sFilename = 'rcr2a' ) or (sFilename = 'rcr3a' ) or (sFilename = 'rcr4a' ) or (sFilename = 'rcr5a') or (sFilename = 'rcr5') or (sFilename = 'rcr4') or (sFilename = 'rcr3') or (sFilename = 'rcr2') or (sFilename = 'rcr1')) then
    begin
      for i := 0 to FHeader.NumItems - 1 do
      begin
        FEntries[i].FInfo.Height := FEntries[i].FInfo.Height;
         FEntries[i].FInfo.width := FEntries[i].FInfo.width * 2;
//         FEntries[i].FInfo.X := FEntries[i].FInfo.X;
         FEntries[i].FInfo.Y := FEntries[i].FInfo.Y div 2;
      end;
    end
    else
     begin
      for i := 0 to FHeader.NumItems - 1 do
      begin
        FEntries[i].FInfo.Height := FEntries[i].FInfo.Height div 2;
       //  FEntries[i].FInfo.width := FEntries[i].FInfo.width * 2;
         FEntries[i].FInfo.X := FEntries[i].FInfo.X;
         FEntries[i].FInfo.Y := FEntries[i].FInfo.Y div 2;
      end;
    end
    end;

    // otherwise carry on and parse through all the JAM texture entries
    for i := 0 to FHeader.NumItems - 1 do
    begin
      with FEntries[i] do
      begin
        intjamMaxWidth := System.Math.Max(intjamMaxWidth, Info.X + Info.Width);
        // calculating a maximum width, just in case... all JAMs other than RCR/specials are 256 pixel width
        intJamMaxHeight := System.Math.Max(intJamMaxHeight,
          Info.Y + Info.Height);
        // calculate maximum height... JAM files have a maximum height of 768 pixels

        // if we have have an odd calculation, we default always to 256 pixels wide...
        if intjamMaxWidth = 0 then
          intjamMaxWidth := 256;


        PalCount := FInfo.PaletteSizeDiv4;

        // if the file actually has a local palette, read it
        if PalCount > 0 then
        begin
          // make sure there really are PalCount bytes * 4 blocks left in Buf
          if Ptr + PalCount * 4 > Length(Buf) then
            raise Exception.CreateFmt
              ('Bad palette data for entry %d: need %d bytes at %d, have %d',
              [i, PalCount * 4, Ptr, Length(Buf)]);

          // read each of the 4 palette strips
          SetLength(FPalettes[0], PalCount);
          Move(Buf[Ptr], FPalettes[0][0], PalCount);
          Inc(Ptr, PalCount);

          SetLength(FPalettes[1], PalCount);
          Move(Buf[Ptr], FPalettes[1][0], PalCount);
          Inc(Ptr, PalCount);

          SetLength(FPalettes[2], PalCount);
          Move(Buf[Ptr], FPalettes[2][0], PalCount);
          Inc(Ptr, PalCount);

          SetLength(FPalettes[3], PalCount);
          Move(Buf[Ptr], FPalettes[3][0], PalCount);
          Inc(Ptr, PalCount);
        end
        else
        begin
          // no local palette: clear them so AdjustJAMImage will fall back to Gp3Pal
          FPalettes[0] := nil;
          FPalettes[1] := nil;
          FPalettes[2] := nil;
          FPalettes[3] := nil;
        end;

      end;
    end;
  end;

 if boolRcrJam then
  begin
    intjamMaxWidth := 512;
    intJamMaxHeight := FHeader.JamTotalHeight div 2;
  end;

  if sFilename = 'mhill' then
  begin
    intjamMaxWidth := 512;
    intJamMaxHeight := 164;
    FHeader.JamTotalHeight := intJamMaxHeight;
  end;

  if sFilename = 'shill' then
  begin
    intjamMaxWidth := 64;
    intJamMaxHeight := 16;
    FHeader.JamTotalHeight := intJamMaxHeight;
  end;


  if sFilename = 'car_srf' then
  begin
    intjamMaxWidth := 512;
    intJamMaxHeight := 544;
    FHeader.JamTotalHeight := intJamMaxHeight;
    boolRcrJam := True;
  end;

  if sFilename = 'hlm_srf' then
  begin
    intjamMaxWidth := 256;
    intJamMaxHeight := 64;
    FHeader.JamTotalHeight := intJamMaxHeight;
    boolRcrJam := True;
  end;

  if sFilename = 'wh_srf' then
  begin
    intjamMaxWidth := 512;
    intJamMaxHeight := 256;
    FHeader.JamTotalHeight := intJamMaxHeight;
    boolRcrJam := True;
  end;

  if sFilename = 'vcp_srf' then
  begin
    intjamMaxWidth := 512;
    intJamMaxHeight := 256;
    FHeader.JamTotalHeight := intJamMaxHeight;
    boolRcrJam := True;
  end;

  if sFilename = 'vcp_srf2' then
  begin
    intjamMaxWidth := 512;
    intJamMaxHeight := 256;
    FHeader.JamTotalHeight := intJamMaxHeight;
    boolRcrJam := True;
  end;

  // Now pull out exactly (image_total_size * 256) bytes of raw pixel data
  BlockCount := FHeader.JamTotalHeight; // from file, in 256-byte units
  TrueSize := BlockCount * 256; // actual bytes
  if Ptr + TrueSize > Length(Buf) then
    raise Exception.CreateFmt
      ('JAM image data too short: need %d bytes at offset %d, file has %d',
      [TrueSize, Ptr, Length(Buf)]);

  FRawData := Copy(Buf, Ptr, Length(Buf) - Ptr);

  if not boolRcrJam then
    for i := 0 to FHeader.NumItems - 1 do
    begin
      with FEntries[i] do
      begin
        FTexture := DrawSingleTexture(FRawData, Length(FRawData), i,false);
        FOriginalTex := FTexture;
      end;
      // build a single JAM texture, whilst handling palette information
    end;

  JamFileName := FileName;

end;

procedure TJamFile.SaveToFile(const FileName: string);
var
  OutBuf: TBytes;
  Header: TJamHeader;
  Ptr, i, PalCount: Integer;
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
    PalCount := entry.FInfo.PaletteSizeDiv4;
    // e.g. same count used during Load
    Ptr := Ptr + PalCount * 4; // assuming 4 palettes of that length
  end;
  SetLength(OutBuf, Ptr);

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

    // 5a) local palettes (up to four of them) — same order you read them in LoadFromFile
    PalCount := entry.FInfo.PaletteSizeDiv4;
    if PalCount > 0 then
    begin
      Move(entry.FPalettes[0][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
      Move(entry.FPalettes[1][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
      Move(entry.FPalettes[2][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
      Move(entry.FPalettes[3][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
    end;

    // 5b) raw texture bytes
    // if Length(Entry.FRawTexture) > 0 then
    // begin
    // Move(Entry.FRawTexture[0], OutBuf[Ptr], Length(Entry.FRawTexture));
    // Inc(Ptr, Length(Entry.FRawTexture));
    // end;
  end;

  Move(FRawData[0], OutBuf[Ptr], Length(FRawData));
  Inc(Ptr, Length(FRawData));
  OutBuf := UnJam(OutBuf);
  // 7) Finally write everything out
  TFile.WriteAllBytes(FileName, OutBuf);
end;


procedure TJamFile.EncodeTexture(JamId: Integer; texture: TBitmap);
var
  entry: TJamEntry;
  Info: TJamEntryInfo;
  W, H: Integer;
  X0, Y0: Integer;
  indices, TempRaw: TBytes;
  X, Y, pos, origIdx: Integer;
  bestIdx: Integer;

begin
  entry := FEntries[JamId];
  Info := entry.FInfo;
  W := Info.Width;
  H := Info.Height;
  X0 := Info.X;
  Y0 := Info.Y;

  // 1) grab the raw 8-bit indices from the bitmap
  indices := BitmapToIndices(texture);

  SetLength(TempRaw, W * H);
  // 3) Scan each pixel: find nearest GPxPal index, record in TempRaw & UsedFlags
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
    begin
      origIdx := indices[Y * W + X];
      bestIdx := origIdx;
      TempRaw[Y * W + X] := Byte(bestIdx);
    end;

  // 7) Write TempRaw into the entry’s FRawTexture and the master FRawData
  // SetLength(FEntries[jamID].FRawTexture, W * H);
  SetLength(entry.FRawTexture, W * H);
  for Y := 0 to H - 1 do
  begin
    pos := (Y0 + Y) * 256 + X0; // srcStride = 256
    Move(TempRaw[Y * W], entry.FRawTexture[Y * W], W);
    Move(TempRaw[Y * W], FRawData[pos], W);
  end;

  entry.FTexture := DrawSingleTexture(entry.FRawTexture, Length(entry.FRawTexture), JamId, true);

end;   

function TJamFile.DrawSingleTexture(const Raw: TBytes; TotalImageSize: Integer;
  JamId: Integer; drawFromEntry : boolean): TBitmap;
var
  W, H, X0, Y0: Integer;
  SrcStride: Integer;
  PalCount, i, X, Y, idx, dst: Integer;
  LocalPal: array [0 .. 255] of Byte;
  Bmp: TBitmap;
  dstID: Integer;
  rcrMult : integer;
begin

  W := FEntries[JamId].FInfo.Width;
  H := FEntries[JamId].FInfo.Height;

  if drawFromEntry = true then
  begin
  SrcStride := W;
  x0 := 0;
  y0 := 0;
  end
  else
  begin
  SrcStride := 256;
  X0 := FEntries[JamId].FInfo.X;
  Y0 := FEntries[JamId].FInfo.Y;
  end;

  // 3) Build fixed‐size local palette (always 256 entries)
  // Default = identity mapping
  for i := 0 to 255 do
    LocalPal[i] := Byte(i);

  // Overwrite from the file’s palette (up to 256 entries)
  PalCount := FEntries[JamId].FInfo.PaletteSizeDiv4;

  if PalCount > 256 then
    PalCount := 256;
  // if PalCount > 0 then
  for i := 0 to PalCount - 1 do
    LocalPal[i] := FEntries[JamId].FPalettes[intPaletteID][i];

  // 4) Create an 8-bit bitmap & GDI palette
  Bmp := TBitmap.Create;
  try

    Bmp.Width := W;
    Bmp.Height := H;

    SetLength(FEntries[JamId].FRawTexture, W * H);

    // 5) Copy pixels, using SrcStride for raw indexing
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        idx := X0 + X + (Y0 + Y) * SrcStride;
        dstID := (Y * W) + X;
        FEntries[JamId].FRawTexture[dstID] := Raw[idx];
        dst := LocalPal[Raw[idx]]; // 0..255 guaranteed
        Bmp.Canvas.Pixels[X, Y] := RGB(GPxPal[dst].R, GPxPal[dst].G,
          GPxPal[dst].B);
      end;
    Result := Bmp;
  except
    Bmp.Free;
    raise;
  end;

end;

function TJamFile.DrawPalTexture(JamId: Integer): TBitmap;
var
  W, H: Integer;
  PalCount, i, X, Y, idx,dst: Integer;
  LocalPal: array [0 .. 255] of Byte;
  jamTex: TBitmap;

begin

  if JamId = -1 then
    Exit;

  PalCount := FEntries[JamId].FInfo.PaletteSizeDiv4;

  if PalCount = 0 then
  begin
    Result := DrawSingleTexture(FEntries[jamid].FRawTexture, FHeader.JamTotalHeight, JamId,true);
    Exit;
  end;

  W := FEntries[JamId].FInfo.Width;
  H := FEntries[JamId].FInfo.Height;
  jamTex := TBitmap.Create;
  jamTex.PixelFormat := pf8bit;
  jamTex.Width := W;
  jamTex.Height := H;
  jamTex.Palette := CreateLocalPalette(LocalGpxPal);

  // 3) Build fixed‐size local palette (always 256 entries)
  // Default = identity mapping
  for i := 0 to 255 do
    LocalPal[i] := Byte(i);

  // Overwrite from the file’s palette (up to 256 entries)
  PalCount := FEntries[JamId].FInfo.PaletteSizeDiv4;

  if PalCount > 256 then
    PalCount := 256;

  for i := 0 to PalCount - 1 do
    LocalPal[i] := FEntries[JamId].FPalettes[intPaletteID][i];

  try
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        idx := X + Y * jamTex.Width;
        dst := LocalPal[FEntries[JamId].FRawTexture[idx]];
        // 0..255 guaranteed
        jamTex.Canvas.Pixels[X, Y] := RGB(GPxPal[dst].R, GPxPal[dst].G,
          GPxPal[dst].B);
      end;
    Result := jamTex;
  except
    raise;

  end;      

  end;  

function TJamFile.DrawFullJam(): TBitmap;
var
  bmpAll, entryBmp: TBitmap;
  i: Integer;
begin

  bmpAll := TBitmap.Create;
  bmpAll.PixelFormat := pf32bit;
  bmpAll.Width := 256;
  bmpAll.Height := FHeader.JamTotalHeight;

  // pick default JAM transparent colour
  bmpAll.Canvas.Brush.color := RGB(GPxPal[0].R, GPxPal[0].G, GPxPal[0].B);

  // fill the entire bitmap
  bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.Width, bmpAll.Height));

  // special draw routine for RCR JAMs
  if boolRcrJam = True then
  begin
    bmpAll.Width := 512;
    bmpAll.Height := FHeader.JamTotalHeight div 2;
    entryBmp := DrawFullRCR(FRawData);
    bmpAll.Canvas.Draw(0, 0, entryBmp);
    Result := bmpAll;
    Exit;
  end

  else

    // special draw routine for JIPs
    if boolJipMode = True then
    begin
      entryBmp := DrawFullJIP(FRawData);
      bmpAll.Height := FHeader.JamTotalHeight;
      intJamMaxHeight := FHeader.JamTotalHeight;
      bmpAll.Canvas.Draw(0, 0, entryBmp);
      Result := bmpAll;
      Exit
    end

    else

    // normal standard JAM files
    begin
      // 3) draw each entry into it
      for i := 0 to FEntries.Count - 1 do
      begin
        entryBmp := DrawSingleTexture(FEntries[i].FRawTexture, Length(FEntries[i].FRawTexture), i, true);
        // build a single JAM texture, whilst handling palette information
        try
          bmpAll.Canvas.Draw(FEntries[i].Info.X, FEntries[i].Info.Y, entryBmp);
          // draw said texture in its rightful position
        finally
          entryBmp.Free;
        end;
      end;
    end;

  Result := bmpAll;

end;

function TJamFile.DrawFullJIP(const Raw: TBytes): TBitmap;
var
  i: Integer;
  CanvasData: TBytes;
begin

  CanvasData := Raw;

  canvasWidth := 256;
  canvasHeight := FHeader.JamTotalHeight;

  CanvasBitmap := TBitmap.Create;
  CanvasBitmap.Width := canvasWidth;
  CanvasBitmap.Height := canvasHeight;
  CanvasBitmap.PixelFormat := pf8bit;
  CanvasBitmap.Palette := CreatePaletteFromColors;

  for i := 0 to CanvasBitmap.Height - 1 do
    Move(CanvasData[i * canvasWidth], CanvasBitmap.ScanLine[i]^, canvasWidth);

  Result := CanvasBitmap;
end;

function TJamFile.DrawFullRCR(const Raw: TBytes): TBitmap;
var
  i: Integer;
  CanvasData: TBytes;
begin

  CanvasData := Raw;

  canvasWidth := 512;
  canvasHeight := FHeader.JamTotalHeight  div 2;

  CanvasBitmap := TBitmap.Create;
  CanvasBitmap.PixelFormat := pf8bit;
  CanvasBitmap.Width := canvaswidth;
  CanvasBitmap.Height := canvasHeight;
  CanvasBitmap.Palette := CreatePaletteFromColors;

  for i := 0 to CanvasBitmap.Height - 1 do
    Move(CanvasData[i * canvasWidth], CanvasBitmap.ScanLine[i]^, canvasWidth);

  Result := CanvasBitmap;
end;

function TJamFile.DrawOutlines(JamCanvas: TBitmap): TBitmap;
var
  i, textW, textH: Integer;
  idText: string;
  rectX, rectY: Integer;
  x,y,w,h: integer;
  textRect: TRect;
begin
  if not boolDrawOutlines then
    Exit(JamCanvas);

  JamCanvas.Canvas.Pen.Style := psSolid;
  JamCanvas.Canvas.Pen.Width := 1;
  JamCanvas.Canvas.Brush.Style := bsClear;

  for i := 0 to FEntries.Count - 1 do
  begin
  x := Round(FEntries[i].FInfo.X * intJamZoom);
  y := Round(FEntries[i].FInfo.Y * intJamZoom);
  w := Round(FEntries[i].FInfo.Width * intJamZoom);
  h := Round(FEntries[i].FInfo.Height * intJamZoom);


    // Set pen color for outlines
    if i = intSelectedTexture then
    JamCanvas.Canvas.Pen.Color := clHighlight
    else
    JamCanvas.Canvas.Pen.Color := clInactiveBorder;


      JamCanvas.Canvas.Rectangle(x, y, x + w, y + h);


    JamCanvas.Canvas.font.IsScreenFont := true;
    JamCanvas.Canvas.font.Size := min(12,max(5,round(5*intJamZoom)));
    JamCanvas.Canvas.font.Quality := fqClearTypeNatural;
    JamCanvas.Canvas.font.name := 'Segoe UI';

    // Draw texture ID at top-left
    idText := 'Jam ID: ' + FEntries[i].FInfo.JamId.ToString;

    // Measure text dimensions
    textW := JamCanvas.Canvas.TextWidth(idText) + 10; // Padding
    textH := JamCanvas.Canvas.TextHeight(idText) + 2;

    rectX := x;
    rectY := y;
    textRect := Rect(rectX, rectY, rectX + textW, rectY + textH);

    // Draw filled rectangle with black border
    JamCanvas.Canvas.Brush.Style := bsSolid;

    if i = intSelectedTexture then
    JamCanvas.Canvas.Brush.Color := clHighlight
    else
    JamCanvas.Canvas.Brush.Color := clInactiveBorder;

    JamCanvas.Canvas.Pen.Color := clBlack;
    JamCanvas.Canvas.Rectangle(textRect);

    // Draw text in black
    JamCanvas.Canvas.Font.Color := clBlack;
    JamCanvas.Canvas.Brush.Style := bsClear; // Transparent for text


    JamCanvas.Canvas.TextOut(rectX + 6, rectY + 1, idText);

  end;

  Result := JamCanvas;
end;


//procedure TJamFile.PopulateTree(Tree: TTreeView);
//var
//  JamRoot, ItemsRoot, Node, ScalingNode, UnTexNode, UnkNode, CanvasHeightNode: TTreeNode;
//  i: Integer;
//  E: TJamEntry;
//begin
//  Tree.Items.BeginUpdate;
//  Tree.Items.Clear;
//  JamRoot := Tree.Items.Add(nil, jamfilename);
//  ItemsRoot := Tree.Items.AddChild(JamRoot, Format('JAM Items: %d', [FHeader.NumItems]));
//
//  for i := 0 to FEntries.Count - 1 do
//  begin
//    E := FEntries[i];
//
//    Node := Tree.Items.AddChild(ItemsRoot, Format('ID:%d  [%d x %d]',
//      [E.Info.JamId, E.Info.Width, E.Info.Height]));
//    Tree.Items.AddChild(Node, Format('X:%d', [E.Info.X]));
//    Tree.Items.AddChild(Node, Format('Y:%d', [E.Info.Y]));
//    Tree.Items.AddChild(Node, Format('Width:%d', [E.Info.Width]));
//    Tree.Items.AddChild(Node, Format('Height:%d', [E.Info.Height]));
//    Tree.Items.AddChild(Node, Format('JamFlags:%d', [E.Info.JamFlags]));
//    Tree.Items.AddChild(Node, Format('PaletteSize:%d',
//      [E.Info.PaletteSizeDiv4]));
//
//   ScalingNode := Tree.Items.AddChild(Node, 'Scaling Info');
//   Tree.Items.AddChild(ScalingNode, Format('Idx08:%d', [E.Info.Idx08]));
//    Tree.Items.AddChild(ScalingNode, Format('Idx0As:%d', [E.Info.Idx0A]));
////    Tree.Items.AddChild(Node, Format('ImagePtr:%d', [E.Info.ImagePtr]));
//
//    UnTexNode := Tree.Items.AddChild(Node, 'Untextured Colour');
//    Tree.Items.AddChild(UnTexNode, Format('Primary: %d', [E.Info.Idx16]));
//    Tree.Items.AddChild(UnTexNode, Format('Secondary: %d', [E.Info.Idx17]));
//    UnkNode := Tree.Items.AddChild(Node, 'Unknown Data');
//    Tree.Items.AddChild(UnkNode, Format('UNK:%d', [E.Info.Unk]));
//    Tree.Items.AddChild(UnkNode, Format('Idx0Es:%d', [E.Info.Idx0E]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_00:%d', [E.Info.Idx18[0]]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_01:%d', [E.Info.Idx18[1]]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_02:%d', [E.Info.Idx18[2]]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_03:%d', [E.Info.Idx18[3]]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_04:%d', [E.Info.Idx18[4]]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_05:%d', [E.Info.Idx18[5]]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_06:%d', [E.Info.Idx18[6]]));
//    Tree.Items.AddChild(UnkNode, Format('Idx18_07:%d', [E.Info.Idx18[7]]));
//
//    // add more properties as needed
//  end;
//  CanvasHeightNode := Tree.Items.AddChild(JamRoot, Format('Canvas Size: W: %d x H: %d',
//    [intjamMaxWidth, intJamMaxHeight]));
//
//  Tree.TopItem.Expanded := True;
//  Tree.Items.EndUpdate;
//
//end;

procedure TJamFile.AddTexture(textureFilename: string);
var
  srcPic: TPicture;
  textureWidth, textureHeight: Integer;
  tmpCanvas: TBitmap;
  info: TJamEntryInfo;
  newTex: TJamEntry;
begin

  srcPic := TPicture.Create;

  try

    tmpCanvas := TBitmap.Create;
    srcPic.LoadFromFile(textureFilename);
    tmpCanvas.Height := srcpic.Height;
    tmpCanvas.width := srcpic.width;
    tmpCanvas.PixelFormat := srcPic.bitmap.PixelFormat;
    tmpCanvas := srcpic.Bitmap;
    // auto‐detects BMP, JPEG, PNG, etc.

    info.X := 0;
    info.Y := 0;
    info.Width := tmpCanvas.Width;
    info.Height := tmpCanvas.Height;
    info.JamId := FEntries[FEntries.count-1].Info.JamId + 1;

    info.PaletteSizeDiv4 := 0;

    newTex := TJamEntry.Create(info);
    newTex.FOriginalTex := tmpCanvas;
    newTex.FPalettes[0] := nil;
    newTex.FPalettes[1] := nil;
    newTex.FPalettes[2] := nil;
    newTex.FPalettes[3] := nil;

    FEntries.Add(newTex);


    FHeader.NumItems := FHeader.NumItems +1;
    // Store original imported texture so we have quick access to manipulate the palette later on
    tmpCanvas := GenerateGPxBMP(tmpCanvas, FEntries.count-1);

    FEntries[FEntries.count-1].FTexture := tmpCanvas; // Rebuild and store texture



  finally
    srcPic.Free;
    newTex.free;
  end;

end;

procedure TJamFile.ImportTexture(JamId: Integer; textureFilename: string);
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
    srcPic.LoadFromFile(textureFilename);
    tmpCanvas.PixelFormat := srcPic.bitmap.PixelFormat;
    // auto‐detects BMP, JPEG, PNG, etc.

    tmpCanvas.SetSize(textureWidth, textureHeight);

    tmpCanvas.Canvas.stretchdraw(Rect(0, 0, textureWidth, textureHeight),
      StretchF(srcPic.bitmap, textureWidth, textureHeight));

    FEntries[JamId].FOriginalTex := tmpCanvas;
    // Store original imported texture so we have quick access to manipulate the palette later on
    tmpCanvas := GenerateGPxBMP(tmpCanvas, JamId);

    FEntries[JamId].FTexture := tmpCanvas; // Rebuild and store texture

  finally
    srcPic.Free;
  end;

end;

procedure TJamFile.ExportTexture(JamId: Integer; textureFilename: string);
var
  srcPic: TPicture;
  textureWidth, textureHeight: Integer;
  tmpCanvas: TBitmap;
begin

// todo
end;




procedure TJamFile.ZeroPalette(JamId: Integer);
var
  tmpBMP: TBitmap;
  Width, Height: Integer;
 

begin
  Width := FEntries[JamId].FInfo.Width;
  Height := FEntries[JamId].FInfo.Height;
  tmpBMP := TBitmap.Create;
  tmpBMP.PixelFormat := pf8bit;
  tmpBMP.Palette := CreatePaletteFromColors;
  tmpBMP.Width := Width;
  tmpBMP.Height := Height;

  tmpBMP.Canvas.Draw(0, 0, FEntries[JamId].FOriginalTex);

  EncodeTexture(JamId, tmpBMP);

  FEntries[JamId].PaletteSizeDiv4 := 0;
  FEntries[JamId].FPalettes[0] := nil;
  FEntries[JamId].FPalettes[1] := nil;
  FEntries[JamId].FPalettes[2] := nil;
  FEntries[JamId].FPalettes[3] := nil;

  tmpBMP.Free;

end;

function TJamFile.GenerateGPxBMP(bitmap: TBitmap; JamId: Integer): TBitmap;
var
  SrcRGB: TBitmap;
  L1Idx, L2Idx, L3Idx, L4Idx: TBitmap;
  L1RGB, L2RGB, L3RGB, L4RGB: TBitmap;
  DisplayIdx: TBitmap;
  i, j: Integer;
  PalSize2: Integer;
  LP2: PLogPal2;
  hPalTemp: HPALETTE;
  X, Y, H, W, idx: Integer;
  indices: TBytes;
  tmpBMP: TBitmap;
  Mask: TBoolGrid;
  tempColour: TRGB;
  jamPalSize: Integer;
begin

  for i := 0 to 3 do
  begin
    LevelIdx[i] := TBitmap.Create;
    LevelIdx[i].PixelFormat := pf8bit;
  end;

  SingleIdxMap := TBitmap.Create;
  SingleIdxMap.PixelFormat := pf8bit;

  for i := 0 to 3 do
    SetLength(PalPerLevel[i], 256);

  SrcRGB := bitmap;

  try

    if SrcRGB.PixelFormat <> pf24bit then
      SrcRGB.PixelFormat := pf24bit;

    // if boolProtectTrans then
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
    QuantizeToPalette24bit(SrcRGB, L1Idx);

    LevelIdx[0].Assign(L1Idx);
    IndexedTo24bit(L1Idx, L1RGB);

    // Blur chain: L1RGB→L2RGB→L3RGB→L4RGB
    GaussianBlur(L1RGB, Mask, intBlurThreshold, L2RGB);
    GaussianBlur(L2RGB, Mask, intBlurThreshold, L3RGB);
    GaussianBlur(L3RGB, Mask, intBlurThreshold, L4RGB);

    if boolSimpifyAllPals then
    begin

      SimplifyByNeighborThreshold(L1RGB, intSimplifyThreshold, L1RGB);
      SimplifyByNeighborThreshold(L2RGB, intSimplifyThreshold, L2RGB);
      SimplifyByNeighborThreshold(L3RGB, intSimplifyThreshold, L3RGB);
      SimplifyByNeighborThreshold(L4RGB, intSimplifyThreshold, L4RGB);
    end;

    // Quantize blurred → L2Idx, L3Idx, L4Idx
    QuantizeToPalette24bit(L2RGB, L2Idx);
    LevelIdx[1].Assign(L2Idx);
    QuantizeToPalette24bit(L3RGB, L3Idx);
    LevelIdx[2].Assign(L3Idx);
    QuantizeToPalette24bit(L4RGB, L4Idx);
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

  Result := SingleIdxMap;

  L1Idx.Free;
  L2Idx.Free;
  L3Idx.Free;
  L4Idx.Free;

end;





end.
