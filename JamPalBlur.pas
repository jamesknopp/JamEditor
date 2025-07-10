program PalettedBlurMipmaps;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes, Graphics, Math, Generics.Collections;

type
  // A 4-tuple of palette-indices, one per blur level, for a single pixel
  TIndex4 = array[0..3] of Byte;

//----------------------------------------------------------------
// Reads a JASC-PAL (text) file like GP3.pal and returns a dynamic
// array[0..255] of TColor (RGB).  Format:
//   JASC-PAL
//   0100
//   256
//   R G B    <– 256 lines of ints in [0..255]
// Returns true if successful, false otherwise.
//----------------------------------------------------------------
function LoadJascPal(const FileName: string; out Pal: TArray<TColor>): Boolean;
var
  SL: TStringList;
  i, Num, R, G, B: Integer;
  Parts: TArray<string>;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    if (SL.Count < 3) or (Trim(SL[0]) <> 'JASC-PAL') or (Trim(SL[1]) <> '0100') then
      Exit;
    Num := StrToIntDef(Trim(SL[2]), -1);
    if Num <> 256 then
      Exit;
    SetLength(Pal, 256);
    for i := 0 to 255 do
    begin
      // Each line: "R G B"
      Parts := SL[i + 3].Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
      if Length(Parts) <> 3 then Exit;
      R := StrToIntDef(Parts[0], -1);
      G := StrToIntDef(Parts[1], -1);
      B := StrToIntDef(Parts[2], -1);
      if (R < 0) or (R > 255) or (G < 0) or (G > 255) or (B < 0) or (B > 255) then
        Exit;
      // In TColor, format is $00BBGGRR
      Pal[i] := RGB(R, G, B);
    end;
    Result := True;
  finally
    SL.Free;
  end;
end;

//----------------------------------------------------------------
// Given a 24-bit (pf24bit) TBitmap and a 256-entry palette array Pal256,
// produce an 8-bit (pf8bit) indexed TBitmap whose Palette entries =
//? each palette entry is one of Pal256[],
// ? and each pixel is assigned the nearest-color (Euclidean distance).
// We return, in OutIndexed, a pf8bit TBitmap of the same dimensions.
//----------------------------------------------------------------
procedure QuantizeToPalette24bit(Src: TBitmap; const Pal256: TArray<TColor>; out OutIndexed: TBitmap);
var
  x, y, i, BestIndex: Integer;
  PixelColor: TColor;
  R1, G1, B1: Byte;
  PR, PG, PB: Byte;
  BestDist, Dist: Int64;
  Dst: TBitmap;
  RowSrc: PRGBTriple;
  RowDst: PByteArray;
  PalR, PalG, PalB: array[0..255] of Byte;
begin
  Assert(Src.PixelFormat = pf24bit);
  // Precompute the RGB components of each palette entry:
  for i := 0 to 255 do
  begin
    PalR[i] := GetRValue(Pal256[i]);
    PalG[i] := GetGValue(Pal256[i]);
    PalB[i] := GetBValue(Pal256[i]);
  end;

  Dst := TBitmap.Create;
  try
    Dst.PixelFormat := pf8bit;
    Dst.Width := Src.Width;
    Dst.Height := Src.Height;
    // Build a Windows palette structure for Dst so we can fill Dst.Palette[]:
    // We'll create a logical palette of 256 entries with EXACTLY Pal256[].
    // Each palette entry needs a PCINDEXED palette entry:
    var PalEntries: array[0..255] of TMaxLogPalette;
    // Actually, we'll build a LOGPALETTE:
    type
      TPaletteEntry = record
        peRed, peGreen, peBlue, peFlags: Byte;
      end;
    type
      PLogPalette = ^TLogPalette;
      TLogPalette = record
        palVersion: Word; {must be 0x300}
        palNumEntries: Word; {number of entries}
        palPalEntry: array[0..255] of TPaletteEntry;
      end;
    var
      LP: PLogPalette;
      hPal: HPALETTE;
      PalSize: Integer;
    begin
      PalSize := SizeOf(Word) * 2 + SizeOf(TPaletteEntry) * 256;
      GetMem(LP, PalSize);
      try
        LP^.palVersion := $0300;
        LP^.palNumEntries := 256;
        for i := 0 to 255 do
        begin
          LP^.palPalEntry[i].peRed   := PalR[i];
          LP^.palPalEntry[i].peGreen := PalG[i];
          LP^.palPalEntry[i].peBlue  := PalB[i];
          LP^.palPalEntry[i].peFlags := 0;
        end;
        hPal := CreatePalette(LP^);
        Dst.Palette := hPal;
      finally
        FreeMem(LP, PalSize);
      end;
    end;

    // Now loop through each pixel, find nearest palette entry:
    for y := 0 to Src.Height - 1 do
    begin
      RowSrc := Src.Scanline[y];
      RowDst := Dst.Scanline[y];
      for x := 0 to Src.Width - 1 do
      begin
        // Extract Src pixel's RGB:
        R1 := RowSrc^.rgbtRed;
        G1 := RowSrc^.rgbtGreen;
        B1 := RowSrc^.rgbtBlue;
        BestDist := MaxInt64;
        BestIndex := 0;
        // Brute-force search nearest palette color:
        for i := 0 to 255 do
        begin
          PR := PalR[i];
          PG := PalG[i];
          PB := PalB[i];
          Dist := Int64(R1 - PR) * (R1 - PR)
                + Int64(G1 - PG) * (G1 - PG)
                + Int64(B1 - PB) * (B1 - PB);
          if Dist < BestDist then
          begin
            BestDist := Dist;
            BestIndex := i;
          end;
        end;
        RowDst^[x] := Byte(BestIndex);
        Inc(RowSrc);
      end;
    end;

    OutIndexed := Dst; // return ownership to caller
  except
    Dst.Free;
    raise;
  end;
end; { QuantizeToPalette24bit }

//----------------------------------------------------------------
// Given an 8-bit indexed TBitmap (Dst), whose Palette[] is already set
// to a known 256-entry palette, produce a 24-bit (pf24bit) copy (RGB)
// that we can pass to the blur routine.  Copy each pixel’s palette-index
// ? lookup RGB ? store into a new pf24bit TBitmap.
//----------------------------------------------------------------
procedure IndexedTo24bit(const SrcIndexed: TBitmap; var Out24: TBitmap);
var
  x, y: Integer;
  RowDst: PRGBTripleArray;
  RowSrc: PByteArray;
  PalEntries: array[0..255] of TPaletteEntry;
  ALogPal: TMaxLogPalette;
  hPal: HPALETTE;
begin
  Assert(SrcIndexed.PixelFormat = pf8bit);

  // Extract the RGB entries from SrcIndexed.Palette:
  hPal := SrcIndexed.Palette;
  if hPal = 0 then
    raise Exception.Create('IndexedTo24bit: Source has no palette assigned.');
  // GetPaletteEntries retrieves up to N entries:
  GetPaletteEntries(hPal, 0, 256, PalEntries[0]);

  Out24 := TBitmap.Create;
  try
    Out24.PixelFormat := pf24bit;
    Out24.Width := SrcIndexed.Width;
    Out24.Height := SrcIndexed.Height;

    // For each pixel, get its index, then its palette RGB:
    for y := 0 to SrcIndexed.Height - 1 do
    begin
      RowSrc := SrcIndexed.Scanline[y];
      RowDst := Out24.Scanline[y];
      for x := 0 to SrcIndexed.Width - 1 do
      begin
        var Idx := RowSrc^[x];
        RowDst^[x].rgbtRed   := PalEntries[Idx].peRed;
        RowDst^[x].rgbtGreen := PalEntries[Idx].peGreen;
        RowDst^[x].rgbtBlue  := PalEntries[Idx].peBlue;
      end;
    end;

  except
    Out24.Free;
    raise;
  end;
end; { IndexedTo24bit }

//----------------------------------------------------------------
// Apply a separable Gaussian blur of integer radius (sigma = radius/2).
// We build a 1D kernel of length (2*Radius + 1) with weights from
// a discrete Gaussian.  Then we convolve rows and columns separately.
// This is O(N*M*Radius), but for small images and small radius (~2-5), it's okay.
// Src must be pf24bit; Out must be created with pf24bit of same dims.
//----------------------------------------------------------------
procedure GaussianBlur24(const Src: TBitmap; Radius: Integer; out OutImg: TBitmap);
var
  W, H, i, x, y: Integer;
  KernelSize: Integer;
  Kernel: TArray<Double>;
  SumKernel: Double;
  Half: Integer;
  Temp: TBitmap;
  RowSrc, RowTemp: PRGBTripleArray;
  ColSrc, ColTemp: PRGBTriple;
  TmpRow: PRGBTripleArray;
  TmpCol: PRGBTripleArray;
begin
  Assert(Src.PixelFormat = pf24bit);
  W := Src.Width;
  H := Src.Height;
  KernelSize := 2 * Radius + 1;
  SetLength(Kernel, KernelSize);

  // Build 1D Gaussian kernel (sigma ~ Radius/2)
  SumKernel := 0;
  for i := -Radius to Radius do
  begin
    Kernel[i + Radius] := Exp(- (i*i) / (2.0 * (Radius/2)*(Radius/2)));
    SumKernel := SumKernel + Kernel[i + Radius];
  end;
  // Normalize
  for i := 0 to KernelSize - 1 do
    Kernel[i] := Kernel[i] / SumKernel;

  // 1) Horizontal pass: Src ? Temp
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Width  := W;
    Temp.Height := H;

    // For each row
    for y := 0 to H - 1 do
    begin
      RowSrc := Src.Scanline[y];
      RowTemp := Temp.Scanline[y];
      for x := 0 to W - 1 do
      begin
        var AccR := 0.0;
        var AccG := 0.0;
        var AccB := 0.0;
        for i := -Radius to Radius do
        begin
          var X2 := x + i;
          if X2 < 0 then X2 := 0 else if X2 >= W then X2 := W - 1;
          AccR := AccR + RowSrc[X2].rgbtRed   * Kernel[i + Radius];
          AccG := AccG + RowSrc[X2].rgbtGreen * Kernel[i + Radius];
          AccB := AccB + RowSrc[X2].rgbtBlue  * Kernel[i + Radius];
        end;
        RowTemp[x].rgbtRed   := Round(AccR);
        RowTemp[x].rgbtGreen := Round(AccG);
        RowTemp[x].rgbtBlue  := Round(AccB);
      end;
    end;

    // 2) Vertical pass: Temp ? OutImg
    OutImg := TBitmap.Create;
    OutImg.PixelFormat := pf24bit;
    OutImg.Width  := W;
    OutImg.Height := H;

    // For each column, we need to read Temp.Scanline[y][x] for y=0..H-1
    var TempLines: array of PRGBTripleArray;
    SetLength(TempLines, H);
    for y := 0 to H - 1 do
      TempLines[y] := Temp.Scanline[y];

    for y := 0 to H - 1 do
    begin
      for x := 0 to W - 1 do
      begin
        var AccR := 0.0;
        var AccG := 0.0;
        var AccB := 0.0;
        for i := -Radius to Radius do
        begin
          var Y2 := y + i;
          if Y2 < 0 then Y2 := 0 else if Y2 >= H then Y2 := H - 1;
          AccR := AccR + TempLines[Y2][x].rgbtRed   * Kernel[i + Radius];
          AccG := AccG + TempLines[Y2][x].rgbtGreen * Kernel[i + Radius];
          AccB := AccB + TempLines[Y2][x].rgbtBlue  * Kernel[i + Radius];
        end;
        var OutRow := OutImg.Scanline[y];
        OutRow[x].rgbtRed   := Round(AccR);
        OutRow[x].rgbtGreen := Round(AccG);
        OutRow[x].rgbtBlue  := Round(AccB);
      end;
    end;

  finally
    Temp.Free;
  end;
end; { GaussianBlur24 }

//----------------------------------------------------------------
// Build the single-index map + four per-level 256-entry palettes.
// Inputs: an array LevelsIdx[0..3] of TBitmap (each pf8bit,
//         already quantized individually into GP3Pal).
//         GP3Pal: TColor[0..255].
// Outputs:
//   SingleIdx: pf8bit TBitmap where each pixel’s value 0..255 selects
//              one “base 4-tuple.”
//   PalPerLevel: array[0..3] of TArray<TColor> (each length = 256)
//                so that PalPerLevel[L][j] = the GP3Pal color that the
//                j-th base tuple uses at level L.
//----------------------------------------------------------------
procedure BuildSingleIndexMapAndPalettes(const LevelsIdx: array of TBitmap; const GP3Pal: TArray<TColor>; out SingleIdx: TBitmap; out PalPerLevel: array of TArray<TColor>);
type
  TTupleKey = string;
var
  W, H, x, y, L: Integer;
  CountMap: TDictionary<TTupleKey, Integer>; // freq of each 4-tuple
  TupleToColorIndices: TDictionary<TTupleKey, TIndex4>;
  KeysList: TArray<TTupleKey>;
  PairList: TArray<TPair<TTupleKey, Integer>>;
  ICount: Integer;
  // After sorting:
  BaseTuples: TArray<TTupleKey>;       // length <= 256
  TupleToBaseIndex: TDictionary<TTupleKey, Byte>; // map any tuple ? baseIdx
  Key: TTupleKey;
  Idx4, BestIdx4: TIndex4;
  Dist, BestDist: Int64;
  BasePalColors: array[0..255, 0..3] of TColor;
    // BasePalColors[j, L] = GP3Pal[BaseTuples[j][L]]
  // Temp for scanning:
  RowIdx: array[0..3] of PByteArray;
  PalEntries: array[0..255] of TPaletteEntry;
  hPal: HPALETTE;
  i, j: Integer;
begin
  Assert(Length(LevelsIdx) = 4, 'Must supply exactly 4 levels.');

  W := LevelsIdx[0].Width;
  H := LevelsIdx[0].Height;
  for L := 0 to 3 do
    if (LevelsIdx[L].Width <> W) or (LevelsIdx[L].Height <> H) or (LevelsIdx[L].PixelFormat <> pf8bit) then
      raise Exception.CreateFmt('Level %d must be pf8bit and same dimensions.', [L + 1]);

  // 1) Count every pixel’s 4-tuple of palette-indices
  CountMap := TDictionary<TTupleKey, Integer>.Create;
  TupleToColorIndices := TDictionary<TTupleKey, TIndex4>.Create;
  try
    for y := 0 to H - 1 do
    begin
      for L := 0 to 3 do
        RowIdx[L] := LevelsIdx[L].Scanline[y];
      for x := 0 to W - 1 do
      begin
        // Build key = 4 bytes: e.g. Chr(I0) + Chr(I1) + Chr(I2) + Chr(I3)
        Key := '';
        for L := 0 to 3 do
        begin
          Idx4[L] := RowIdx[L]^[x];
          Key := Key + Char(Idx4[L]);
        end;
        if not CountMap.TryGetValue(Key, ICount) then
        begin
          CountMap.Add(Key, 1);
          TupleToColorIndices.Add(Key, Idx4);
        end
        else
          CountMap.Items[Key] := ICount + 1;
      end;
    end;

    // 2) Sort all tuples by frequency descending
    // Build an array of (Key, Count)
    ICount := 0;
    SetLength(PairList, CountMap.Count);
    for Key in CountMap.Keys do
    begin
      PairList[ICount] := TPair<TTupleKey, Integer>.Create(Key, CountMap[Key]);
      Inc(ICount);
    end;
    // Sort by Count descending
    TArray.Sort<TPair<TTupleKey, Integer>>(PairList,
      TComparer<TPair<TTupleKey, Integer>>.Construct(
        function(const A, B: TPair<TTupleKey, Integer>): Integer
        begin
          Result := B.Value - A.Value;
        end));

    // 3) Take top 256 keys as BaseTuples (or fewer if <256 distinct)
    if Length(PairList) < 256 then
      SetLength(BaseTuples, Length(PairList))
    else
      SetLength(BaseTuples, 256);
    for i := 0 to High(BaseTuples) do
      BaseTuples[i] := PairList[i].Key;

    // 4) Precompute BasePalColors[j,L] = GP3Pal[ BaseTuples[j][L] ]
    for j := 0 to High(BaseTuples) do
    begin
      Idx4 := TupleToColorIndices[BaseTuples[j]];
      for L := 0 to 3 do
        BasePalColors[j, L] := GP3Pal[Idx4[L]];
    end;

    // 5) Build TupleToBaseIndex: for each Key in CountMap.Keys,
    //    if Key is among BaseTuples, map Key?its index j.
    //    Otherwise, find the BaseTuple that minimizes:
    //      sum_{L=0..3} [ distance( GP3Pal[Idx4[L]], BasePalColors[j,L] ) ].
    TupleToBaseIndex := TDictionary<TTupleKey, Byte>.Create;
    try
      // First assign base keys directly
      for j := 0 to High(BaseTuples) do
        TupleToBaseIndex.Add(BaseTuples[j], Byte(j));

      // Now handle the rest:
      for Key in CountMap.Keys do
      begin
        if TupleToBaseIndex.ContainsKey(Key) then
          Continue; // already base
        Idx4 := TupleToColorIndices[Key];
        BestDist := MaxInt64;
        BestIdx4 := Idx4;  // (not used here)
        var BestBaseIdx: Integer := 0;
        // Compute nearest base?tuple in 4-level RGB space
        for j := 0 to High(BaseTuples) do
        begin
          var DAcc: Int64 := 0;
          for L := 0 to 3 do
          begin
            var C1 := GP3Pal[Idx4[L]];
            var C2 := BasePalColors[j, L];
            var dR := GetRValue(C1) - GetRValue(C2);
            var dG := GetGValue(C1) - GetGValue(C2);
            var dB := GetBValue(C1) - GetBValue(C2);
            DAcc := DAcc + Int64(dR)*dR + Int64(dG)*dG + Int64(dB)*dB;
          end;
          if DAcc < BestDist then
          begin
            BestDist    := DAcc;
            BestBaseIdx := j;
          end;
        end;
        TupleToBaseIndex.Add(Key, Byte(BestBaseIdx));
      end;

      // 6) Build SingleIdx map:
      SingleIdx := TBitmap.Create;
      SingleIdx.PixelFormat := pf8bit;
      SingleIdx.Width := W;
      SingleIdx.Height := H;
      // We will assign a temporary dummy palette; we'll overwrite later:
      begin
        // Build a 256-color black palette so we can assign the pixel data:
        type
          TPaletteEntry2 = record
            peRed, peGreen, peBlue, peFlags: Byte;
          end;
        type
          PLogPalette2 = ^TLogPalette2;
          TLogPalette2 = record
            palVersion: Word;
            palNumEntries: Word;
            palPalEntry: array[0..255] of TPaletteEntry2;
          end;
        var
          LP2: PLogPalette2;
          PalSz: Integer;
          hPal2: HPALETTE;
        begin
          PalSz := SizeOf(Word)*2 + SizeOf(TPaletteEntry2)*256;
          GetMem(LP2, PalSz);
          try
            LP2^.palVersion := $0300;
            LP2^.palNumEntries := 256;
            for i := 0 to 255 do
            begin
              LP2^.palPalEntry[i].peRed   := 0;
              LP2^.palPalEntry[i].peGreen := 0;
              LP2^.palPalEntry[i].peBlue  := 0;
              LP2^.palPalEntry[i].peFlags := 0;
            end;
            hPal2 := CreatePalette(LP2^);
            SingleIdx.Palette := hPal2;
          finally
            FreeMem(LP2, PalSz);
          end;
        end;
      end;

      // Fill SingleIdx pixel data:
      for y := 0 to H - 1 do
      begin
        for L := 0 to 3 do
          RowIdx[L] := LevelsIdx[L].Scanline[y];
        var RowSingle := SingleIdx.Scanline[y];
        for x := 0 to W - 1 do
        begin
          for L := 0 to 3 do
            Idx4[L] := RowIdx[L]^[x];
          // Build the Key
          Key := '';
          for L := 0 to 3 do
            Key := Key + Char(Idx4[L]);
          // Look up base index
          RowSingle^[x] := TupleToBaseIndex[Key];
        end;
      end;

      // 7) Build the four 256-entry PalPerLevel[L][j]
      for L := 0 to 3 do
      begin
        SetLength(PalPerLevel[L], 256);
        for j := 0 to 255 do
        begin
          // BaseTuples[j][L] is the original GP3Pal index at level L:
          var Idx := TupleToColorIndices[BaseTuples[j]][L];
          PalPerLevel[L][j] := GP3Pal[Idx];
        end;
      end;

    finally
      TupleToBaseIndex.Free;
    end;

  finally
    TupleToColorIndices.Free;
    CountMap.Free;
  end;
end; { BuildSingleIndexMapAndPalettes }

//----------------------------------------------------------------
// Save an 8-bit indexed TBitmap to disk as a BMP (Windows .bmp).
// We assume Bmp.PixelFormat = pf8bit and its Palette[] is set to
// exactly 256 colors.  We simply call Bmp.SaveToFile with the proper
// PixelFormat, and Delphi will write a correct palette?indexed BMP.
//----------------------------------------------------------------
procedure SaveIndexedBitmap(const Bmp: TBitmap; const FileName: string);
begin
  Assert(Bmp.PixelFormat = pf8bit);
  // Ensure type of bitmap is handled as Windows DIB section:
  Bmp.SaveToFile(FileName);
end;

//----------------------------------------------------------------
// Save a 256-entry palette (array of 256 TColor) as a small 16×16
// 8-bit bitmap so you can inspect or use it at load time.  We'll
// create a 16×16 pf8bit TBitmap, assign its Palette[] = Pal256,
// and fill each pixel with indices 0..255.
//----------------------------------------------------------------
procedure SavePaletteSwatch(const Pal256: TArray<TColor>; const FileName: string);
var
  Bmp: TBitmap;
  x, y, idx: Integer;
  LogPal: PLogPalette;
  PalSz: Integer;
  hPal: HPALETTE;
  Row: PByteArray;
begin
  Assert(Length(Pal256) = 256);
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf8bit;
    Bmp.Width := 16;
    Bmp.Height := 16;
    // Build and assign Windows palette:
    type
      TPaletteEntry2 = record
        peRed, peGreen, peBlue, peFlags: Byte;
      end;
      PLogPalette2 = ^TLogPalette2;
      TLogPalette2 = record
        palVersion: Word;
        palNumEntries: Word;
        palPalEntry: array[0..255] of TPaletteEntry2;
      end;
    PalSz := SizeOf(Word) * 2 + SizeOf(TPaletteEntry2) * 256;
    GetMem(LogPal, PalSz);
    try
      LogPal^.palVersion := $0300;
      LogPal^.palNumEntries := 256;
      for idx := 0 to 255 do
      begin
        LogPal^.palPalEntry[idx].peRed   := GetRValue(Pal256[idx]);
        LogPal^.palPalEntry[idx].peGreen := GetGValue(Pal256[idx]);
        LogPal^.palPalEntry[idx].peBlue  := GetBValue(Pal256[idx]);
        LogPal^.palPalEntry[idx].peFlags := 0;
      end;
      hPal := CreatePalette(LogPal^);
      Bmp.Palette := hPal;
    finally
      FreeMem(LogPal, PalSz);
    end;

    // Fill the 16×16 with indices 0..255
    for y := 0 to 15 do
    begin
      Row := Bmp.Scanline[y];
      for x := 0 to 15 do
      begin
        idx := y * 16 + x;
        Row^[x] := Byte(idx);
      end;
    end;

    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end; { SavePaletteSwatch }


var
  GP3PalColors: TArray<TColor>;
  Success: Boolean;
  SrcRGB: TBitmap;
  Level1Idx, Level2Idx, Level3Idx, Level4Idx: TBitmap;
  L1RGB, L2RGB, L3RGB, L4RGB: TBitmap;
  Palettes: array[0..3] of TArray<TColor>;
  SingleIdxMap: TBitmap;
begin
  try
    // 1) Load GP3.PAL
    Success := LoadJascPal('GP3.pal', GP3PalColors);
    if not Success then
      raise Exception.Create('Failed to load GP3.pal');

    // 2) Load source bitmap (24-bit RGB)
    SrcRGB := TBitmap.Create;
    try
      SrcRGB.LoadFromFile('886_256x43.bmp');
      if SrcRGB.PixelFormat <> pf24bit then
        SrcRGB.PixelFormat := pf24bit;

      // 3) Quantize to GP3 ? Level1Idx (pf8bit)
      QuantizeToPalette24bit(SrcRGB, GP3PalColors, Level1Idx);
      Level1Idx.SaveToFile('Level1_Quantized.bmp');

      // 4) Convert back ? L1RGB (pf24bit) and Gaussian blur 3 times
      IndexedTo24bit(Level1Idx, L1RGB);
      GaussianBlur24(L1RGB, 2, L2RGB);  // Blur radius = 2
      GaussianBlur24(L2RGB, 2, L3RGB);
      GaussianBlur24(L3RGB, 2, L4RGB);

      // 5) Quantize each blurred back to GP3 ? Level2Idx, Level3Idx, Level4Idx
      QuantizeToPalette24bit(L2RGB, GP3PalColors, Level2Idx);
      Level2Idx.SaveToFile('Level2_Quantized.bmp');
      QuantizeToPalette24bit(L3RGB, GP3PalColors, Level3Idx);
      Level3Idx.SaveToFile('Level3_Quantized.bmp');
      QuantizeToPalette24bit(L4RGB, GP3PalColors, Level4Idx);
      Level4Idx.SaveToFile('Level4_Quantized.bmp');

    finally
      SrcRGB.Free;
      L1RGB.Free;
      L2RGB.Free;
      L3RGB.Free;
      L4RGB.Free;
    end;

    // 6) Build single-index + four per-level palettes
    BuildSingleIndexMapAndPalettes([Level1Idx, Level2Idx, Level3Idx, Level4Idx], GP3PalColors, SingleIdxMap, Palettes);

    // 7) Save SingleIndexMap (pf8bit)
    SaveIndexedBitmap(SingleIdxMap, 'SingleIndexMap.bmp');

    // 8) Save each of the 4 per-level palettes as 16×16 swatches
    SavePaletteSwatch(Palettes[0], 'Palette_Level1.bmp');
    SavePaletteSwatch(Palettes[1], 'Palette_Level2.bmp');
    SavePaletteSwatch(Palettes[2], 'Palette_Level3.bmp');
    SavePaletteSwatch(Palettes[3], 'Palette_Level4.bmp');

    // 9) (Optionally, also re-save the raw per-level indexed images for inspection)
    Level1Idx.SaveToFile('Level1Idx_Raw.bmp');
    Level2Idx.SaveToFile('Level2Idx_Raw.bmp');
    Level3Idx.SaveToFile('Level3Idx_Raw.bmp');
    Level4Idx.SaveToFile('Level4Idx_Raw.bmp');

    Writeln('All done! Generated:');
    Writeln('  SingleIndexMap.bmp');
    Writeln('  Palette_Level1.bmp');
    Writeln('  Palette_Level2.bmp');
    Writeln('  Palette_Level3.bmp');
    Writeln('  Palette_Level4.bmp');
    Writeln('  (Plus the raw quantized levels if you need them.)');
  except
    on E: Exception do
      Writeln('Error: ', E.ClassName, ': ', E.Message);
  end;
end.
