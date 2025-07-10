unit JamGeneral;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  System.IOUtils, System.Math,

  Winapi.Windows, Vcl.Graphics;

const
  JAM_HW_MAGIC     = $0098967F; //Magic number to identify it's a HW JAM

  TCol_TransGP2 = $007FAB97; // RGB(151,171,127)
  TCol_TransGP3 = $0067673F; // RGB(63,103,103)
  TCol_TransGP3HW = $00F8FC00; // RGB(0,252,248)

  TransparentColors: array [0 .. 2] of TColor = (TCol_TransGP2, TCol_TransGP3,
    TCol_TransGP3HW);

  dx: array [0 .. 3] of Integer = (1, -1, 0, 0);
  dy: array [0 .. 3] of Integer = (0, 0, 1, -1);


var
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
    'Car_srf',
    'Hlm_srf',
    'Vcp_srf',
    'vcp_srf2',
    'wh_srf'
  );

//    rcrJAMList: array [0 .. 22] of string = (
//    'rcr1a',
//    'rcr1b',
//    'rcr2a',
//    'rcr2b',
//    'rcr3a',
//    'rcr3b',
//    'rcr4a',
//    'rcr4b',
//    'rcr5a',
//    'rcr5b',
//    'chwheel1',
//    'rcr1',
//    'rcr2',
//    'rcr3',
//    'rcr4',
//    'rcr5',
//    'mhill',
//    'SHILL',
//    'Car_srf',
//    'Hlm_srf',
//    'Vcp_srf',
//    'vcp_srf2',
//    'wh_srf'
//  );

  intJamMaxWidth: integer;
  intJamMaxHeight: integer;

  boolRcrJam: boolean;
  booljipMode: boolean;

  boolGP2Livery : boolean;

  boolGP2Jam : boolean;
  boolGP3Jam : boolean;
  boolHWJAM: boolean;

  booljamLoaded: boolean;

  generatePal : boolean;

  boolmodified: boolean;

  intJamZoom: double;

  booldrawOutlines: boolean;

  intSelectedTexture: integer;
  boolTexSelected : boolean;

  intSimplifyThreshold: integer;
  intSimplifyDist: integer;
  intBlurThreshold: integer;

  boolSimpifyAllPals: boolean;
  boolProtectTrans: boolean;

  intSimplifyMethod: integer;

  intPaletteID: integer;


function CheckIfRCR(const S: string): integer;
function CreateTransparencyMatte(const Bmp: TBitmap): TBitmap;
function DetectTransCol(Bmp: TBitmap): boolean;
function ReplaceTransparentColour(const Bmp: TBitmap; const ReplacementColor: TColor): TBitmap;

function DrawTextureOutlines(jamCanvas : TBitmap; x : integer; y : integer; width : integer; height : integer; i : integer; jamID : integer) : TBitmap;

function UnPackRGB565(raw : Word): TColor;
function PackRGB565(Color: TColor): Word;

function isHWJAM(const Filename : string) : boolean;

function UnPackFlag(data: word; flagNum: integer): boolean;
function PackFlag(data: word; flagNum: integer): word;

implementation

function DrawTextureOutlines(jamCanvas : TBitmap; x : integer; y : integer; width : integer; height : integer; i : integer; jamID : integer) : TBitmap;
var
  textW, textH: Integer;
  idText: string;
  rectX, rectY: Integer;
  textRect: TRect;
  w, h : integer;
begin
  if not boolDrawOutlines then
    exit(JamCanvas);

  JamCanvas.Canvas.Pen.Style := psSolid;
  JamCanvas.Canvas.Pen.Width := 1;
  JamCanvas.Canvas.Brush.Style := bsClear;

  w := round(width * intJamZoom);
  h := round(height * intJamZoom);
  y := round(y * intJamZoom);
  x := round(x * intJamZoom);


    // Set pen color for outlines
    if i = intSelectedTexture then
      JamCanvas.Canvas.Pen.color := clHighlight
    else
      JamCanvas.Canvas.Pen.color := clInactiveBorder;

    JamCanvas.Canvas.Rectangle(X, Y, X + W, Y + H);

    JamCanvas.Canvas.font.IsScreenFont := True;
    JamCanvas.Canvas.font.Size := min(12, Max(5, Round(5 * intJamZoom)));
    JamCanvas.Canvas.font.Quality := fqClearTypeNatural;
    JamCanvas.Canvas.font.name := 'Segoe UI';

    // Draw texture ID at top-left
    idText := 'Jam ID: ' + jamID.ToString;

    // Measure text dimensions
    textW := JamCanvas.Canvas.TextWidth(idText) + 10; // Padding
    textH := JamCanvas.Canvas.TextHeight(idText) + 2;

    rectX := X;
    rectY := Y;
    textRect := Rect(rectX, rectY, rectX + textW, rectY + textH);

    // Draw filled rectangle with black border
    JamCanvas.Canvas.Brush.Style := bsSolid;

    if i = intSelectedTexture then
      JamCanvas.Canvas.Brush.color := clHighlight
    else
      JamCanvas.Canvas.Brush.color := clInactiveBorder;

    JamCanvas.Canvas.Pen.color := clBlack;
    JamCanvas.Canvas.Rectangle(textRect);

    // Draw text in black
    JamCanvas.Canvas.font.color := clBlack;
    JamCanvas.Canvas.Brush.Style := bsClear; // Transparent for text

    JamCanvas.Canvas.TextOut(rectX + 6, rectY + 1, idText);

  Result := JamCanvas;

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
  TransparentColors: array [0 .. 2] of TColor = (
    TCol_TransGP2, TCol_TransGP3, TCol_TransGP3HW
  );
var
  X, Y, i: Integer;
  px: PRGBTriple;
  MatchCounts: array[0 .. 2] of Integer;
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
            Exit;
          end;
          Break;
        end;
      end;
      Inc(px);
    end;
  end;
end;

function ReplaceTransparentColour(const Bmp: TBitmap; const ReplacementColor: TColor): TBitmap;
const
  TransparentColors: array [0..2] of TColor = (
    TCol_TransGP2, TCol_TransGP3, TCol_TransGP3HW
  );
var
  X, Y, i: Integer;
  pxSrc, pxDst: PRGBTriple;
  c: TColor;
  R, G, B: Byte;
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
            pxDst^ := pxSrc^  // leave pixel unchanged
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


function CheckIfRCR(const S: string): integer;
var
  I: integer;
  filename: string;
begin
  filename := lowerCase(S);
  result := 0;
  boolRcrJam := false;
  for I := Low(rcrJAMList) to High(rcrJAMList) do
  begin
    // ShowMessage('Input: ' + filename + ' listed item: ' + rcrJamList[i]);
    if lowerCase(filename) = rcrJAMList[I] then
    begin
      result := 1;
      boolRcrJam := true;
      exit;
    end;
  end;
end;

function PackRGB565(Color: TColor): Word;
begin
  Result := (GetBValue(Color) shr 3)
          or ((GetGValue(Color) shr 2) shl 5)
          or ((GetRValue(Color) shr 3) shl 11);
end;

function UnPackRGB565(raw : Word): TColor;
begin

result := RGB(Byte((raw shr 11) shl 3),
              Byte(((raw and $7E0) shr 5) shl 2),
              Byte((raw and $1F) shl 3)
              );

end;

function isHWJAM(const FileName: string): Boolean;
var
  fs: TFileStream;
  magic: Cardinal;
begin
  Result := False; // default assumption

  if not TFile.Exists(FileName) then
    Exit;

  try
    fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
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


function UnPackFlag(data: word; flagNum: integer): boolean;
begin
 result := data and (1 shl flagNum) <> 0;
end;

function PackFlag(data: word; flagNum: integer): word;
begin
 result := data or (1 shl flagNum);
end;

end.
