unit JamGeneral;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.IOUtils, System.Math,
  Winapi.Windows, Vcl.Graphics, System.Generics.Collections;

const
  JAM_HW_MAGIC = $0098967F; // Magic number to identify it's a HW JAM

  CLIPBOARD_JAM = 'JamTexClipboard';
  CLIPBOARD_HWJAM = 'JamHWTexClipboard';

  TCol_TransGP2 = $007FAB97; // RGB(151,171,127)
  TCol_TransGP3 = $0067673F; // RGB(63,103,103)
  TCol_TransGP3HW = $00FFFF00; // RGB(0,255,255)

  TransparentColors: array [0 .. 2] of TColor = (TCol_TransGP2, TCol_TransGP3,
    TCol_TransGP3HW);

  baseKeyPath = '\Software\JKVFX\JamEditor\';
  MRUKeyPath = baseKeyPath + 'RecentFiles';

  dx: array [0 .. 3] of Integer = (1, -1, 0, 0);
  dy: array [0 .. 3] of Integer = (0, 0, 1, -1);

type
  THWJamHeader = packed record
    NumItems: Word;
    JamTotalHeight: Word;
  end;

  THWJamEntryInfo = record
    PosRaw: Integer; // raw dword from words[0]+[1]<<16
    X, Y: Integer; // decoded Left/Top
    Width, Height: Word; // words[2], words[3]
    scaleX, scaleY, scaleFactor, scaleFlags: byte; // words 4  & 5
    word6: Word; // words[6]
    word7, word8: Word;
    JamID: Word; // words[9]
    jamflags: Word; // words[10]
    word11, word12, word13, word14, word15, word16: Word;

    FrameFlags: array [0 .. 15] of Boolean; // unpacked bits

    word6a, word6b, word7a, word7b, word8a, word8b, word11a, word11b, word12a,
      word12b, word13a, word13b, word14a, word14b, word15a, word15b, word16a,
      word16b: byte;
    NumFrames: Integer; // = 1 shl FrameCountExp
  end;

  THWRawJamEntryInfo = packed record
    PosRawX: Word; // 0
    PosRawY: Word; // 1
    Width: Word; // 2
    Height: Word; // 3
    scaleX: byte; // 4
    scaleY: byte; // 4
    scaleFlag: byte; // 5
    scaleFactor: byte; // 5
    ImagePtr: Word; // 6
    Idx0E: Word; // 7 totally unk
    PaletteSizeDiv4: Word; // 8
    JamID: Word; // 9
    jamflags: Word; // 10 flags
    Idx16: byte; // 11 untex color 1; entry in palette
    Idx17: byte; // 11 untext color 2; entry in palette
    Idx18: array [0 .. 7] of byte; // 12-16
    // 3,4 and 6 seem to be used in car liveries (gp3)... maybe load up all JAM files and create CSVs with all the data to review??
  end;

  TJamHeader = packed record
    NumItems: Word;
    JamTotalHeight: Word;
  end;

  TJamEntryInfo = packed record
    X: byte;
    Y: Word;
    Unk: byte; // totally unk?
    Width: Word;
    Height: Word;
    scaleX: byte;
    scaleY: byte;
    scaleFlag: byte;
    scaleFactor: byte;
    // Idx08: Word; // scale origin x & y?
    // Idx0A: Word; // scaling flags & factor?
    ImagePtr: Word;
    Idx0E: Word; // totally unk
    PaletteSizeDiv4: Word;
    JamID: Word;
    jamflags: Word; // flags
    Idx16: byte; // untex color 1; entry in palette
    Idx17: byte; // untext color 2; entry in palette
    Idx18: array [0 .. 7] of byte;
    // 3,4 and 6 seem to be used in car liveries (gp3)... maybe load up all JAM files and create CSVs with all the data to review??
  end;

type
  TJamType = (jamGP2, jamGP3SW, jamGP3HW);

var
  ClipboardJAM: uint;
  ClipboardhwJAM: uint;

  SelectedTextureList: TList<Integer>;
  rcrJAMList: array [0 .. 18] of string = (
    'rcr1a',

    'rcr2a',
    'rcr2b',

    'rcr3a',

    'rcr4a',

    'rcr5a',

    'chwheel1',
    'rcr1',
    'rcr2',
    'rcr3',
    'rcr4',
    'rcr5',
    'mhill',
    'SHILL',
    'car_srf',
    'hlm_srf',
    'ccp_srf',
    'ccp_srf2',
    'wh_srf'
  );

  // rcrJAMList: array [0 .. 22] of string = (
  // 'rcr1a',
  // 'rcr1b',
  // 'rcr2a',
  // 'rcr2b',
  // 'rcr3a',
  // 'rcr3b',
  // 'rcr4a',
  // 'rcr4b',
  // 'rcr5a',
  // 'rcr5b',
  // 'chwheel1',
  // 'rcr1',
  // 'rcr2',
  // 'rcr3',
  // 'rcr4',
  // 'rcr5',
  // 'mhill',
  // 'SHILL',
  // 'Car_srf',
  // 'Hlm_srf',
  // 'Vcp_srf',
  // 'vcp_srf2',
  // 'wh_srf'
  // );

  intJamMaxWidth: Integer;
  intJamMaxHeight: Integer;

  boolRcrJam: Boolean;
  booljipMode: Boolean;

  boolGP2Livery: Boolean;

  boolGP2Jam: Boolean;
  boolGP3Jam: Boolean;
  boolHWJAM: Boolean;

  boolBrowsePal: Boolean;

  booljamLoaded: Boolean;

  generatePal: Boolean;

  boolJamModified: Boolean;

  intJamZoom: double;

  booldrawOutlines: Boolean;

  intSelectedTexture: Integer;
  boolTexSelected: Boolean;

  intSimplifyThreshold: Integer;
  intSimplifyDist: Integer;
  intBlurThreshold: Integer;

  boolSimpifyAllPals: Boolean;
  boolProtectTrans: Boolean;

  intSimplifyMethod: Integer;

  intPaletteID: Integer;

  boolRCRDrawMode: Boolean;

  strImportPath: string;
  strExportPath: string;
  strOpenPath: string;
  strSavePath: string;

  strGP2Location: string;
  strGP3Location: string;
  strGP32kLocation: string;

  strBrowserPath: string;

  intMaxMRU: Integer;

  intMaxUndo: Integer;

  jamType: TJamType;

function CheckIfRCR(const S: string): Integer;
function CreateTransparencyMatte(const Bmp: TBitmap): TBitmap;
function DetectTransCol(Bmp: TBitmap): Boolean;
function ReplaceTransparentColour(const Bmp: TBitmap;
  const ReplacementColor: TColor): TBitmap;

function DrawTextureOutlines(jamCanvas: TBitmap; X: Integer; Y: Integer;
  Width: Integer; Height: Integer; i: Integer; JamID: Integer): TBitmap;

function UnPackRGB565(raw: Word): TColor;
function PackRGB565(Color: TColor): Word;

function isHWJAM(const Filename: string): Boolean;

function UnPackFlag(data: Word; flagNum: Integer): Boolean overload;

function FlagToInt(bool: Boolean): Integer;

function PackFlag(data: Word; flagNum: Integer): Word;

function ToggleGP3JamsFolder(const APath: string): string;

function ExtractColumns8Bit(const Source: TBitmap; ReadOdd: Boolean): TBitmap;

implementation

function DrawTextureOutlines(jamCanvas: TBitmap; X: Integer; Y: Integer;
  Width: Integer; Height: Integer; i: Integer; JamID: Integer): TBitmap;
var
  textW, textH: Integer;
  idText: string;
  rectX, rectY: Integer;
  textRect: TRect;
  w, h, j: Integer;
begin
  if not booldrawOutlines then
    exit(jamCanvas);

  jamCanvas.Canvas.lock;

  jamCanvas.Canvas.Pen.Style := psSolid;
  jamCanvas.Canvas.Pen.Width := 1;
  jamCanvas.Canvas.Brush.Style := bsClear;

  w := round(Width * intJamZoom);
  h := round(Height * intJamZoom);
  Y := round(Y * intJamZoom);
  X := round(X * intJamZoom);

  jamCanvas.Canvas.Pen.Color := clInactiveBorder;
  // Set pen color for outlines
  for j in SelectedTextureList do
  begin
    if j = i then
      jamCanvas.Canvas.Pen.Color := clHighlight;
  end;

  jamCanvas.Canvas.Rectangle(X, Y, X + w, Y + h);

  jamCanvas.Canvas.font.IsScreenFont := True;
  jamCanvas.Canvas.font.Size := min(12, Max(5, round(5 * intJamZoom)));
  jamCanvas.Canvas.font.Quality := fqClearTypeNatural;
  jamCanvas.Canvas.font.name := 'Segoe UI';

  // Draw texture ID at top-left
  idText := 'Jam ID: ' + JamID.ToString;

  // Measure text dimensions
  textW := jamCanvas.Canvas.TextWidth(idText) + 10; // Padding
  textH := jamCanvas.Canvas.TextHeight(idText) + 2;

  rectX := X;
  rectY := Y;
  textRect := Rect(rectX, rectY, rectX + textW, rectY + textH);

  // Draw filled rectangle with black border
  jamCanvas.Canvas.Brush.Style := bsSolid;
  jamCanvas.Canvas.Brush.Color := clInactiveBorder;

  for j in SelectedTextureList do
  begin
    if j = i then

      jamCanvas.Canvas.Brush.Color := clHighlight
  end;

  jamCanvas.Canvas.Pen.Color := clBlack;
  jamCanvas.Canvas.Rectangle(textRect);

  // Draw text in black
  jamCanvas.Canvas.font.Color := clBlack;
  jamCanvas.Canvas.Brush.Style := bsClear; // Transparent for text

  jamCanvas.Canvas.TextOut(rectX + 6, rectY + 1, idText);

  jamCanvas.Canvas.Unlock;

  Result := jamCanvas;

end;

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

function DetectTransCol(Bmp: TBitmap): Boolean;
const
  TransparentColors: array [0 .. 2] of TColor = (TCol_TransGP2, TCol_TransGP3,
    TCol_TransGP3HW);
var
  X, Y, i: Integer;
  px: PRGBTriple;
  MatchCounts: array [0 .. 2] of Integer;
  c: TColor;
begin
  Result := False;

  FillChar(MatchCounts, SizeOf(MatchCounts), 0);

  Bmp.PixelFormat := pf24bit;

  for Y := 0 to Bmp.Height - 1 do
  begin
    px := Bmp.ScanLine[Y];
    for X := 0 to Bmp.Width - 1 do
    begin
      c := RGB(px^.rgbtRed, px^.rgbtGreen, px^.rgbtBlue);

      for i := 0 to High(TransparentColors) do
      begin
        if c = TransparentColors[i] then
        begin
          Inc(MatchCounts[i]);
          if MatchCounts[i] = 4 then
          begin
            Result := True;
            exit;
          end;
          Break;
        end;
      end;
      Inc(px);
    end;
  end;
end;

function ReplaceTransparentColour(const Bmp: TBitmap;
  const ReplacementColor: TColor): TBitmap;
const
  TransparentColors: array [0 .. 2] of TColor = (TCol_TransGP2, TCol_TransGP3,
    TCol_TransGP3HW);
var
  X, Y, i: Integer;
  pxSrc, pxDst: PRGBTriple;
  c: TColor;
  R, G, B: byte;
  Found: Boolean;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.SetSize(Bmp.Width, Bmp.Height);

  Bmp.PixelFormat := pf24bit;

  R := GetRValue(ReplacementColor);
  G := GetGValue(ReplacementColor);
  B := GetBValue(ReplacementColor);

  for Y := 0 to Bmp.Height - 1 do
  begin
    pxSrc := Bmp.ScanLine[Y];
    pxDst := Result.ScanLine[Y];

    for X := 0 to Bmp.Width - 1 do
    begin
      c := RGB(pxSrc^.rgbtRed, pxSrc^.rgbtGreen, pxSrc^.rgbtBlue);

      for i := 0 to High(TransparentColors) do
      begin
        if c = TransparentColors[i] then
        begin

          // Skip replacement if color is the same as ReplacementColor
          if TransparentColors[i] = ReplacementColor then
            pxDst^ := pxSrc^ // leave pixel unchanged
          else
          begin
            pxDst^.rgbtRed := R;
            pxDst^.rgbtGreen := G;
            pxDst^.rgbtBlue := B;
          end;

          Break; // exit inner loop
        end;

        // No match after last color, copy original pixel
        if i = High(TransparentColors) then
          pxDst^ := pxSrc^;
      end;
      Inc(pxSrc);
      Inc(pxDst);
    end;
  end;
end;

function CheckIfRCR(const S: string): Integer;
var
  i: Integer;
  Filename: string;
begin
  Filename := lowerCase(S);
  Result := 0;
  boolRcrJam := False;
  for i := Low(rcrJAMList) to High(rcrJAMList) do
  begin
    // ShowMessage('Input: ' + filename + ' listed item: ' + rcrJamList[i]);
    if lowerCase(Filename) = lowerCase(rcrJAMList[i]) then
    begin
      Result := 1;
      boolRcrJam := True;
      exit;
    end;
  end;
end;

function PackRGB565(Color: TColor): Word;
begin
  Result := (GetBValue(Color) shr 3) or ((GetGValue(Color) shr 2) shl 5) or
    ((GetRValue(Color) shr 3) shl 11);
end;

function UnPackRGB565(raw: Word): TColor;
begin

  Result := RGB(byte((raw shr 11) shl 3), byte(((raw and $7E0) shr 5) shl 2),
    byte((raw and $1F) shl 3));

end;

function isHWJAM(const Filename: string): Boolean;
var
  fs: TFileStream;
  magic: Cardinal;
begin
  Result := False; // default assumption

  if not TFile.Exists(Filename) then
    exit;

  try
    fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      if fs.Size >= SizeOf(magic) then
      begin
        fs.ReadBuffer(magic, SizeOf(magic));
        Result := (magic = JAM_HW_MAGIC);
      end;
    finally
      fs.Free;
    end;
  except
    // Optional: swallow exceptions, or log
    Result := False;
  end;
end;

function UnPackFlag(data: Word; flagNum: Integer): Boolean;
begin
  Result := (data and (1 shl flagNum)) <> 0;
end;

function FlagToInt(bool: Boolean): Integer;
begin
  if bool then
    Result := 1
  else
    Result := 0;
end;

function PackFlag(data: Word; flagNum: Integer): Word;
begin
  Result := data or (1 shl flagNum);
end;

function ToggleGP3JamsFolder(const APath: string): string;
const
  Old1 = 'GP3JAMSH';
  Old2 = 'GP3JAMS';
begin
  // first look for the longer “H” variant
  if ContainsText(APath, Old1) then
    Result := StringReplace(APath, Old1, Old2, [rfReplaceAll, rfIgnoreCase])
    // else, if it has the non‑H variant, add the H
  else if ContainsText(APath, Old2) then
    Result := StringReplace(APath, Old2, Old1, [rfReplaceAll, rfIgnoreCase])
  else
    Result := APath;
end;

function ExtractColumns8Bit(const Source: TBitmap; ReadOdd: Boolean): TBitmap;
var
  StartX, NewWidth, Y, SrcX, DstX: Integer;
  srcLine, dstLine: PByteArray;
begin
  if not Assigned(Source) then
    raise Exception.Create('ExtractColumns8Bit: Source bitmap is nil');

  // Ensure source is 8‑bit
  if Source.PixelFormat <> pf8bit then
    Source.PixelFormat := pf8bit;

  // Decide start column (0 for even, 1 for odd)
  if ReadOdd then
    StartX := 1
  else
    StartX := 0;

  // Compute how many columns we'll pull out:
  // even (StartX=0): (W+1) div 2
  // odd  (StartX=1): W div 2
  // if ReadOdd then
  // NewWidth := Source.Width div 2
  // else
  // NewWidth := (Source.Width + 1) div 2;

  // Create result bitmap
  Result := TBitmap.Create;
  Result.PixelFormat := pf8bit;
  Result.Width := Source.Width;
  Result.Height := Source.Height;

  // Copy palette so the byte values map the same colours
  Result.Palette := CopyPalette(Source.Palette);

  // Copy each scan‑line, picking every 2nd byte
  for Y := 0 to Source.Height - 1 do
  begin
    srcLine := Source.ScanLine[Y];
    dstLine := Result.ScanLine[Y];
    DstX := 0;
    SrcX := StartX;
    while SrcX < Source.Width do
    begin
      dstLine[DstX] := srcLine[SrcX];
      dstLine[DstX + 1] := srcLine[SrcX];
      Inc(DstX, 2);
      Inc(SrcX, 2);
    end;
  end;
end;

end.
