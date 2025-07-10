unit JamFile;

interface

uses
  // System
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  System.IOUtils, System.Math,
  // Windows/VCL
  Winapi.Windows, Vcl.Graphics, Vcl.Dialogs, Vcl.ComCtrls,
  // Project
  GeneralHelpers, JamGeneral, JamPalette;

const
  JAM_HW_MAGIC     = $0098967F; //Magic number to identify it's a HW JAM
  
  TCol_TransGP2 = $007FAB97; // RGB(151,171,127)
  TCol_TransGP3 = $0067673F; // RGB(63,103,103)
  TCol_TransGP3HW = $00F8FC00; // RGB(0,252,248)

  TransparentColors: array [0 .. 2] of TColor = (TCol_TransGP2, TCol_TransGP3,
    TCol_TransGP3HW);

  dx: array [0 .. 3] of Integer = (1, -1, 0, 0);
  dy: array [0 .. 3] of Integer = (0, 0, 1, -1);

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
    Idx08: Word; // scale origin x
    Idx0A: Word; // scaling information?
    ImagePtr: Word;
    Idx0E: Word; // totally unk
    PaletteSizeDiv4: Word;
    JamId: Word;
    Idx14: Word; // flags
    Idx16: Byte; // untex color 1; entry in palette
    Idx17: Byte; // untext color 2; entry in palette
    Idx18: array [0 .. 7] of Byte;
    // 3,4 and 6 seem to be used in car liveries (gp3)... maybe load up all JAM files and create CSVs with all the data to review??
  end;

  TJamHWEntryInfo = record
    PosRaw:         Integer;            // raw dword from words[0]+[1]<<16
    X, Y:           Integer;            // decoded Left/Top
    Width, Height:  Word;               // words[2], words[3]
    JamID:          Word;               // words[5]
    FrameCountExp:  Word;               // words[6]
    NumFrames:      Integer;            // = 1 shl FrameCountExp
    FlagsRaw:       Word;               // words[10]
    FrameFlags:     array[0..15] of Boolean; // unpacked bits
  end;

  TJamHWHeaderArray = array of TJamHWEntryInfo;

  THWJamEntry = class
  public
    FInfo: TJamHWEntryInfo;
    FTexture: TBitmap;
    FOriginalTex: TBitmap;
    boolImportedBMP: boolean;    
    constructor Create(const Info: TJamHWEntryInfo); overload;
    destructor Destroy; override;
    // in the class declaration:
    property Info: TJamHWEntryInfo read FInfo write FInfo;   
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
    FHWEntries: TJamHWHeaderArray;
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

    function isHWJAM(const FileName: string): boolean;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure LoadSWJam(const FileName: string);
    procedure LoadHWJam(const FileName: string);

    procedure SaveSWJam(const FileName: string);
    procedure SaveHWJam(const FileName: string);
    
    procedure DecryptSWJam(const FileName: string);
    procedure PopulateTree(Tree: TTreeView);
    
    procedure EncodeTexture(JamId: Integer; texture: TBitmap);
    
    procedure ImportTexture(JamId: Integer; textureFilename: string);
    procedure ExportTexture(JamId: Integer; textureFilename: string);

    procedure PackHWJAMHeader(const Hdr: TJamHWEntryInfo; Dest: PWord);
    procedure ParseHWJAMHeader(const Src: PWord; out Hdr: TJamHWEntryInfo);

    function PackRGB565(Color: TColor): Word;

    procedure WriteRLE(const Data: TArray<Word>; Stream: TStream);

    procedure ZeroPalette(JamId: Integer);

    function GenerateGPxBMP(bitmap: TBitmap; JamId: Integer): TBitmap;

    function DrawCanvas(): TBitmap;
    function DrawSingleTexture(const Raw: TBytes; TotalImageSize: Integer;
      JamId: Integer): TBitmap;
    function DrawJIPImage(const Raw: TBytes): TBitmap;
    function DrawRCRImage(const Raw: TBytes): TBitmap;
    function DrawOutlines(JamCanvas: TBitmap): TBitmap;
    function DrawPalTexture(JamId: Integer): TBitmap;

    property Entries: TList<TJamEntry> read FEntries write FEntries;
    property HWEntries: TJamHWHeaderArray read FHWEntries write FHWEntries;
  end;

implementation

function CreateTransparencyMatte(const Bmp: TBitmap): TBitmap;
const
  TransparentColors: array [0 .. 2] of TColor = (TCol_TransGP2,
    // : TColor = $007FAB97; // RGB(151,171,127)
    TCol_TransGP3, // : TColor = $0067673F; // RGB(63,103,103)
    TCol_TransGP3HW // TColor = $00F8FC00; // RGB(0,252,248);
    );
var
  X, Y: Integer;
  srcLine: PRGBTriple;
  dstLine: PRGBTriple;
  Matte: TBitmap;
  c: TColor;
begin
  Matte := TBitmap.Create;
  Matte.PixelFormat := pf24bit;
  Matte.SetSize(Bmp.Width, Bmp.Height);

  Bmp.PixelFormat := pf24bit;

  for Y := 0 to Bmp.Height - 1 do
  begin
    srcLine := Bmp.ScanLine[Y];
    dstLine := Matte.ScanLine[Y];
    for X := 0 to Bmp.Width - 1 do
    begin
      c := RGB(srcLine^.rgbtRed, srcLine^.rgbtGreen, srcLine^.rgbtBlue);
      if (c = TransparentColors[0]) or (c = TransparentColors[1]) or
        (c = TransparentColors[2]) then
      begin
        dstLine^.rgbtRed := 255;
        dstLine^.rgbtGreen := 255;
        dstLine^.rgbtBlue := 255;
      end
      else
      begin
        dstLine^.rgbtRed := 0;
        dstLine^.rgbtGreen := 0;
        dstLine^.rgbtBlue := 0;
      end;
      Inc(srcLine);
      Inc(dstLine);
    end;
  end;

  Result := Matte;
end;

function BitmapContainsTransparentColor(Bmp: TBitmap;
  out FoundColor: TColor): boolean;
const
  TransparentColors: array [0 .. 2] of TColor = (TCol_TransGP2,
    // : TColor = $007FAB97; // RGB(151,171,127)
    TCol_TransGP3, // : TColor = $0067673F; // RGB(63,103,103)
    TCol_TransGP3HW // TColor = $00F8FC00; // RGB(0,252,248);
    );
var
  X, Y, i: Integer;
  px: PRGBTriple;
begin
  Result := False;
  Bmp.PixelFormat := pf24bit;
  for Y := 0 to Bmp.Height - 1 do
  begin
    px := Bmp.ScanLine[Y];
    for X := 0 to Bmp.Width - 1 do
    begin
      for i := 0 to High(TransparentColors) do
        if TransparentColors[i] = RGB(px^.rgbtRed, px^.rgbtGreen, px^.rgbtBlue)
        then
        begin
          Result := True;
          FoundColor := TransparentColors[i];
          Exit;
        end;
      Inc(px);
    end;
  end;
end;

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

{ THWJamEntry }
{ TJamEntry }

constructor THWJamEntry.Create(const Info: TJamHWEntryInfo);
begin
  FInfo := Info;
end;

destructor THWJamEntry.Destroy;
begin
   FTexture.Free;
   FOriginalTex.free;   
  inherited;
end;

{ TJamFile }

constructor TJamFile.Create;
begin
  FEntries := TList<TJamEntry>.Create;
  boolRcrJam := False;
  boolJipMode := False;
end;

destructor TJamFile.Destroy;
begin
  FEntries.Free;
  inherited;
end;

function TJamFile.isHWJAM(const FileName: string) : boolean;
var
  fs: TFileStream;
  magic: Cardinal;
  hdrCount, heightVal: Word;
  raw: TArray<Word>;
  totalWords, dst: Integer;
  ctrl: Byte;
  w: Word;
  i, row, col: Integer;
begin
  if not FileExists(FileName) then Exit;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fs.ReadBuffer(magic, SizeOf(magic));
    if magic <> JAM_HW_MAGIC then result := false
    else
    result := true
  finally
  fs.free;
  end; 
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

function TJamFile.PackRGB565(Color: TColor): Word;
begin
  Result := (GetBValue(Color) shr 3)
          or ((GetGValue(Color) shr 2) shl 5)
          or ((GetRValue(Color) shr 3) shl 11);
end;

procedure TJamFile.ParseHWJAMHeader(const Src: PWord; out Hdr: TJamHWEntryInfo);
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
  Hdr.FlagsRaw      := A^[10];
  // unpack flags
  for i := 0 to 15 do
    Hdr.FrameFlags[i] := (Hdr.FlagsRaw and (1 shl i)) <> 0;
end;

procedure TJamFile.PackHWJAMHeader(const Hdr: TJamHWEntryInfo; Dest: PWord);
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
  A[10] := Hdr.FlagsRaw;
  for j := 11 to 15 do
    A[j] := 0;
  Move(A, Dest^, SizeOf(A));
end;

procedure TJamFile.WriteRLE(const Data: TArray<Word>; Stream: TStream);
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

procedure TJamFile.LoadFromFile(const FileName: string);
var
  Raw, Buf: TBytes;
  Ptr: Integer;
  i: Integer;
  Info: TJamEntryInfo;
  BlockCount, TrueSize: Integer;
  sFilename: string;
begin
  boolRcrJam := False;
  boolJipMode := False;
  intjamMaxWidth := 0;
  intJamMaxHeight := 0;
  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));

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
    (sFilename = 'vcp_srf') or (sFilename = 'vcp_srf2')) then
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
      for i := 0 to FHeader.NumItems - 1 do
      begin
        // FEntries[i].FInfo.Height := FEntries[i].FInfo.Height div 2;
        // FEntries[i].FInfo.width := FEntries[i].FInfo.width * 2;
        // FEntries[i].FInfo.X := FEntries[i].FInfo.X*2;
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

        var
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
        FTexture := DrawSingleTexture(FRawData, Length(FRawData), i);
        FOriginalTex := FTexture;
      end;
      // build a single JAM texture, whilst handling palette information
    end;

  JamFileName := FileName;
  boolJamLoaded := True;
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

procedure TJamFile.LoadSWJam(const FileName: string);
begin

end;

procedure TJamFile.LoadHWJAM(const FileName: string);
var
  fs: TFileStream;
  magic: Cardinal;
  hdrCount, heightVal: Word;
  raw: TArray<Word>;
  totalWords, dst: Integer;
  ctrl: Byte;
  w: Word;
  i, row, col: Integer;
begin
  if not FileExists(FileName) then Exit;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fs.ReadBuffer(magic, SizeOf(magic));
    if magic <> JAM_HW_MAGIC then Exit;
    fs.ReadBuffer(hdrCount, SizeOf(hdrCount));
    fs.ReadBuffer(heightVal, SizeOf(heightVal));
    SetLength(FHWEntries, hdrCount);
    totalWords := (hdrCount shl 4) + (heightVal shl 8);
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
    for i := 0 to hdrCount-1 do
      ParseHWJAMHeader(@raw[i*16], FHWEntries[i]);
    FTexture.Free;
    FBitmap := TBitmap.Create;
    with FBitmap do
    begin
      PixelFormat := pf24bit;
      Width       := BITMAP_WIDTH;
      Height      := heightVal;
    end;
    row := 0; col := 0;
    for i := hdrCount * 16 to hdrCount * 16 + 256 * heightVal - 1 do
    begin
      FBitmap.Canvas.Pixels[col, row] :=
        RGB(
          Byte((raw[i] shr 11) shl 3),
          Byte(((raw[i] and $7E0) shr 5) shl 2),
          Byte((raw[i] and $1F) shl 3)
        );
      Inc(col);
      if col = 256 then
      begin
        col := 0;
        Inc(row);
      end;
    end;
  finally
    fs.Free;
  end;
end;

procedure TJamFile.SaveSWJam(const FileName: string);
begin

end;

procedure TJamFile.SaveHWJam(const FileName: string);
var
  fs: TFileStream;
  hdrCount, heightVal: Word;
  raw: TArray<Word>;
  totalWords: Integer;
  i, row, col, offset: Integer;
  v: Cardinal;
  w: Word;
begin
  Result := False;
  hdrCount  := Length(FHeaders);
  heightVal := FBitmap.Height;
  totalWords := (hdrCount shl 4) + (256 * heightVal);
  SetLength(raw, totalWords);

  // 1) pack each header into the first hdrCount*16 words
  for i := 0 to hdrCount - 1 do
    PackHWJAMHeader(FHeaders[i], @raw[i*16]);

  // 2) pack pixel data into raw[hdrCount*16..]
  offset := hdrCount * 16;
  for row := 0 to heightVal - 1 do
    for col := 0 to BITMAP_WIDTH - 1 do
    begin
      raw[offset] := PackRGB565(FBitmap.Canvas.Pixels[col, row]);
      Inc(offset);
    end;

  // 3) write to disk: magic, counts, then RLE data
  fs := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
  v := JAM_MAGIC;
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

  entry.FTexture := DrawSingleTexture(FRawData, Length(FRawData), JamId);

end;   

function TJamFile.DrawSingleTexture(const Raw: TBytes; TotalImageSize: Integer;
  JamId: Integer): TBitmap;
var
  W, H, X0, Y0: Integer;
  SrcStride: Integer;
  PalCount, i, X, Y, idx, dst: Integer;
  LocalPal: array [0 .. 255] of Byte;
  Bmp: TBitmap;
  dstID: Integer;
begin
  W := FEntries[JamId].FInfo.Width;
  H := FEntries[JamId].FInfo.Height;
  SrcStride := 256;
  X0 := FEntries[JamId].FInfo.X;
  Y0 := FEntries[JamId].FInfo.Y;

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
    Result := DrawSingleTexture(FRawData, FHeader.JamTotalHeight, JamId);
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

function TJamFile.DrawCanvas(): TBitmap;
var
  bmpAll, entryBmp: TBitmap;
  i: Integer;
begin

  bmpAll := TBitmap.Create;
  bmpAll.PixelFormat := pf32bit;
  bmpAll.Width := intjamMaxWidth;
  bmpAll.Height := intJamMaxHeight;

  // pick default JAM transparent colour
  bmpAll.Canvas.Brush.color := RGB(GPxPal[0].R, GPxPal[0].G, GPxPal[0].B);

  // fill the entire bitmap
  bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.Width, bmpAll.Height));

  // special draw routine for RCR JAMs
  if boolRcrJam = True then
  begin
    entryBmp := DrawRCRImage(FRawData);
    bmpAll.Canvas.Draw(0, 0, entryBmp);
    Result := bmpAll;
    Exit;
  end

  else

    // special draw routine for JIPs
    if boolJipMode = True then
    begin
      entryBmp := DrawJIPImage(FRawData);
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
        entryBmp := DrawSingleTexture(FRawData, Length(FRawData), i);
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

function TJamFile.DrawJIPImage(const Raw: TBytes): TBitmap;
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

function TJamFile.DrawRCRImage(const Raw: TBytes): TBitmap;
var
  i: Integer;
  CanvasData: TBytes;
begin

  CanvasData := FRawData;

  canvasWidth := intjamMaxWidth;
  canvasHeight := intJamMaxHeight;

  CanvasBitmap := TBitmap.Create;
  CanvasBitmap.PixelFormat := pf8bit;
  CanvasBitmap.Width := canvasWidth;
  CanvasBitmap.Height := canvasHeight;
  CanvasBitmap.Palette := CreatePaletteFromColors;

  for i := 0 to CanvasBitmap.Height - 1 do
    Move(CanvasData[i * canvasWidth], CanvasBitmap.ScanLine[i]^, canvasWidth);

  Result := CanvasBitmap;
end;

function TJamFile.DrawOutlines(JamCanvas: TBitmap): TBitmap;
var
  centerX, centerY: Integer;
  i: Integer;

begin

  if boolDrawOutlines then
  begin
    JamCanvas.Canvas.pen.Style := psSolid;
    JamCanvas.Canvas.pen.Width := 3 * intJamZoom;

    for i := 0 to FEntries.Count - 1 do
    begin
      if i = intSelectedTexture then
        JamCanvas.Canvas.pen.color := RGB(255, 0, 0)
      else
        JamCanvas.Canvas.pen.color := RGB(255, 255, 255);

      JamCanvas.Canvas.Brush.Style := bsClear;

      JamCanvas.Canvas.Rectangle(FEntries[i].FInfo.X, FEntries[i].FInfo.Y,
        FEntries[i].FInfo.X + FEntries[i].FInfo.Width,
        FEntries[i].FInfo.Y + FEntries[i].FInfo.Height);

      centerX := round(FEntries[i].FInfo.X + (FEntries[i].FInfo.Width / 2));
      centerY := round(FEntries[i].FInfo.Y + (FEntries[i].FInfo.Height / 2));

      JamCanvas.Canvas.font.color := JamCanvas.Canvas.pen.color;

      JamCanvas.Canvas.TextOut(round(centerX), round(centerY),
        FEntries[i].FInfo.JamId.ToString);

      Result := JamCanvas;
    end
  end
  else
    Result := JamCanvas;

end;

procedure TJamFile.PopulateTree(Tree: TTreeView);
var
  Root, Node: TTreeNode;
  i: Integer;
  E: TJamEntry;
begin
  Tree.Items.BeginUpdate;
  Tree.Items.Clear;
  Root := Tree.Items.Add(nil, Format('JAM Items: %d', [FHeader.NumItems]));

  for i := 0 to FEntries.Count - 1 do
  begin
    E := FEntries[i];
    Node := Tree.Items.AddChild(Root, Format('ID:%d  [%d x %d]',
      [E.Info.JamId, E.Info.Width, E.Info.Height]));
    Tree.Items.AddChild(Node, Format('X:%d', [E.Info.X]));
    Tree.Items.AddChild(Node, Format('Y:%d', [E.Info.Y]));
    Tree.Items.AddChild(Node, Format('PaletteSize:%d',
      [E.Info.PaletteSizeDiv4]));
    Tree.Items.AddChild(Node, Format('UNK:%d', [E.Info.Unk]));
    Tree.Items.AddChild(Node, Format('Idx08:%d', [E.Info.Idx08]));
    Tree.Items.AddChild(Node, Format('Idx0As:%d', [E.Info.Idx0A]));
    Tree.Items.AddChild(Node, Format('ImagePtr:%d', [E.Info.ImagePtr]));
    Tree.Items.AddChild(Node, Format('Idx0Es:%d', [E.Info.Idx0E]));
    Tree.Items.AddChild(Node, Format('Idx14:%d', [E.Info.Idx14]));
    Tree.Items.AddChild(Node, Format('Idx16:%d', [E.Info.Idx16]));
    Tree.Items.AddChild(Node, Format('Idx17:%d', [E.Info.Idx17]));
    Tree.Items.AddChild(Node, Format('Idx18_00:%d', [E.Info.Idx18[0]]));
    Tree.Items.AddChild(Node, Format('Idx18_01:%d', [E.Info.Idx18[1]]));
    Tree.Items.AddChild(Node, Format('Idx18_02:%d', [E.Info.Idx18[2]]));
    Tree.Items.AddChild(Node, Format('Idx18_03:%d', [E.Info.Idx18[3]]));
    Tree.Items.AddChild(Node, Format('Idx18_04:%d', [E.Info.Idx18[4]]));
    Tree.Items.AddChild(Node, Format('Idx18_05:%d', [E.Info.Idx18[5]]));
    Tree.Items.AddChild(Node, Format('Idx18_06:%d', [E.Info.Idx18[6]]));
    Tree.Items.AddChild(Node, Format('Idx18_07:%d', [E.Info.Idx18[7]]));

    // add more properties as needed
  end;
  Tree.Items.AddChild(Root, Format('Canvas Size: W: %d x H: %d',
    [intjamMaxWidth, intJamMaxHeight]));

  Tree.TopItem.Expanded := True;
  Tree.Items.EndUpdate;

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
    tmpCanvas.PixelFormat := srcPic.bitmap.PixelFormat;
    srcPic.LoadFromFile(textureFilename);
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
