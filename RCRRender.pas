unit RCRRender;

// Renders RCR UV-mapped sprites by sampling texture maps.
//
// Two render paths are provided:
//
// 1. RenderRCRSprite  (tyres / simple single-texture sprites)
//    Sprite B plane = U (texture X), A plane = V (texture Y).
//    Each pixel samples directly from TextureMap[U, V].
//    Transparency: pixel is background only when BOTH U=0 AND V=0.
//
// 2. RenderRCRCarSprite  (GP3 car sprites - masked multi-texture)
//    Uses a segmentation mask (rcr1b) to route each pixel to the
//    correct texture and ferrari.bmp section:
//
//      Mask 0        -> background (GP3 transparent colour)
//      Mask 1        -> contact shadow (leave as background)
//      Mask 2, 6     -> chassis.bmp,   direct UV
//      Mask 3        -> ferrari.bmp section 1 (rows 128-255), U mirrored (255-U)
//      Mask 4        -> ferrari.bmp section 1 (rows 128-255), direct UV
//      Mask 5, 7     -> ferrari.bmp section 2 (rows 256-383), direct UV
//      Mask 8-11     -> tyre texture (e.g. whbridg0), direct UV
//
//    ferrari.bmp layout (256 x 384):
//      Section 0:  rows   0-127  left side  (not used by rcr1)
//      Section 1:  rows 128-255  top/nose
//      Section 2:  rows 256-383  right side
//
// All input sprite/mask bitmaps must be pf8bit indexed.
// Texture bitmaps may be any format; they are converted to pf24bit internally.
// Output is always pf24bit.  Caller is responsible for freeing the returned bitmap.

interface

uses
  Vcl.Graphics, System.Math, Winapi.Windows, JamGeneral;

// Simple single-texture RCR render (tyres, etc.)
// SpriteB    : B plane of the RCR sprite (U coordinates), pf8bit
// SpriteA    : A plane of the RCR sprite (V coordinates), pf8bit
// AtlasB     : B plane of the atlas (unused in direct path, reserved)
// AtlasA     : A plane of the atlas (unused in direct path, reserved)
// TextureMap : source texture (e.g. whbridg0), any pixel format
// GameMode   : selects background fill colour
// TransparentIndex : palette index treated as transparent (default 0)
function RenderRCRSprite(SpriteB, SpriteA,
                         AtlasB,  AtlasA,
                         TextureMap: TBitmap;
                         GameMode: TJamType = jamGP3SW;
                         TransparentIndex: Byte = 0): TBitmap;

// Masked multi-texture RCR render for GP3 car sprites.
// SpriteB    : B plane of rcr1a (U coordinates), pf8bit
// SpriteA    : A plane of rcr1a (V coordinates), pf8bit
// MaskBmp    : rcr1b segmentation mask, pf8bit
// FerrariTex : livery texture (ferrari.bmp), 256x384
// ChassisTex : chassis/suspension texture (chassis.bmp)
// TyreTex    : tyre texture (whbridg0.bmp)
// GameMode   : selects background fill colour
function RenderRCRCarSprite(SpriteB, SpriteA,
                             MaskBmp: TBitmap;
                             FerrariTex, ChassisTex, TyreTex: TBitmap;
                             GameMode: TJamType = jamGP3SW): TBitmap;

implementation

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

function BmpTo24(Src: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Assign(Src);
  Result.PixelFormat := pf24bit;
end;

function BgFromMode(GameMode: TJamType): TColor;
begin
  case GameMode of
    jamGP2:   Result := TCol_TransGP2;
    jamGP3HW: Result := TCol_TransGP3HW;
  else
    Result := TCol_TransGP3;  // jamGP3SW default
  end;
end;

// ---------------------------------------------------------------------------
//  RenderRCRSprite  (tyres / single-texture)
// ---------------------------------------------------------------------------

function RenderRCRSprite(SpriteB, SpriteA,
                         AtlasB,  AtlasA,
                         TextureMap: TBitmap;
                         GameMode: TJamType = jamGP3SW;
                         TransparentIndex: Byte = 0): TBitmap;
var
  x, y:   Integer;
  uIdx,
  vIdx:   Byte;
  pU, pV: PByte;
  pOut:   PRGBTriple;
  pTex:   PRGBTriple;
  texW,
  texH:   Integer;
  tx, ty: Integer;
  TexRGB: TBitmap;
begin
  Assert(SpriteB.PixelFormat = pf8bit, 'SpriteB must be pf8bit');
  Assert(SpriteA.PixelFormat = pf8bit, 'SpriteA must be pf8bit');
  Assert(SpriteB.Width  = SpriteA.Width,  'Sprite A/B width mismatch');
  Assert(SpriteB.Height = SpriteA.Height, 'Sprite A/B height mismatch');

  TexRGB := BmpTo24(TextureMap);
  try
    texW := TexRGB.Width;
    texH := TexRGB.Height;

    Result := TBitmap.Create;
    try
      Result.Width       := SpriteB.Width;
      Result.Height      := SpriteB.Height;
      Result.PixelFormat := pf24bit;
      Result.Canvas.Brush.Color := BgFromMode(GameMode);
      Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

      for y := 0 to SpriteB.Height - 1 do
      begin
        pU   := SpriteB.ScanLine[y];
        pV   := SpriteA.ScanLine[y];
        pOut := Result.ScanLine[y];

        for x := 0 to SpriteB.Width - 1 do
        begin
          uIdx := pU^;  Inc(pU);
          vIdx := pV^;  Inc(pV);

          if not ((uIdx = TransparentIndex) and (vIdx = TransparentIndex)) then
          begin
            tx   := Min(Integer(uIdx), texW - 1);
            ty   := Min(Integer(vIdx), texH - 1);
            pTex := PRGBTriple(PByte(TexRGB.ScanLine[ty]) + tx * SizeOf(TRGBTriple));
            pOut^ := pTex^;
          end;

          Inc(pOut);
        end;
      end;

    except
      Result.Free;
      raise;
    end;
  finally
    TexRGB.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  RenderRCRCarSprite  (GP3 car - masked multi-texture)
// ---------------------------------------------------------------------------

function RenderRCRCarSprite(SpriteB, SpriteA,
                             MaskBmp: TBitmap;
                             FerrariTex, ChassisTex, TyreTex: TBitmap;
                             GameMode: TJamType = jamGP3SW): TBitmap;
var
  x, y:      Integer;
  uIdx,
  vIdx,
  maskIdx:   Byte;
  pU, pV,
  pMask:     PByte;
  pOut:      PRGBTriple;
  pTex:      PRGBTriple;
  fx, fy:    Integer;
  cx, cy:    Integer;
  tx, ty:    Integer;
  FerRGB,
  ChasRGB,
  TyrRGB:    TBitmap;
  ferW, ferH: Integer;
  chasW, chasH: Integer;
  tyrW, tyrH: Integer;
begin
  Assert(SpriteB.PixelFormat = pf8bit,  'SpriteB must be pf8bit');
  Assert(SpriteA.PixelFormat = pf8bit,  'SpriteA must be pf8bit');
  Assert(MaskBmp.PixelFormat = pf8bit,  'MaskBmp must be pf8bit');
  Assert(SpriteB.Width  = SpriteA.Width,  'Sprite A/B width mismatch');
  Assert(SpriteB.Height = SpriteA.Height, 'Sprite A/B height mismatch');
  Assert(SpriteB.Width  = MaskBmp.Width,  'Sprite/mask width mismatch');
  Assert(SpriteB.Height = MaskBmp.Height, 'Sprite/mask height mismatch');

  // Convert all textures to 24bpp for direct scanline RGB access
  FerRGB  := BmpTo24(FerrariTex);
  ChasRGB := BmpTo24(ChassisTex);
  TyrRGB  := BmpTo24(TyreTex);
  try
    ferW  := FerRGB.Width;   ferH  := FerRGB.Height;
    chasW := ChasRGB.Width;  chasH := ChasRGB.Height;
    tyrW  := TyrRGB.Width;   tyrH  := TyrRGB.Height;

    Result := TBitmap.Create;
    try
      Result.Width       := SpriteB.Width;
      Result.Height      := SpriteB.Height;
      Result.PixelFormat := pf24bit;
      Result.Canvas.Brush.Color := BgFromMode(GameMode);
      Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

      for y := 0 to SpriteB.Height - 1 do
      begin
        pU    := SpriteB.ScanLine[y];
        pV    := SpriteA.ScanLine[y];
        pMask := MaskBmp.ScanLine[y];
        pOut  := Result.ScanLine[y];

        for x := 0 to SpriteB.Width - 1 do
        begin
          uIdx    := pU^;    Inc(pU);
          vIdx    := pV^;    Inc(pV);
          maskIdx := pMask^; Inc(pMask);

          case maskIdx of

            // 0: background - already filled, skip
            0: { /* nothing */ };

            // 1: contact shadow - leave as background
            1: { /* nothing */ };

            // 2, 6: chassis/suspension - direct UV into chassis.bmp
            2, 6:
            begin
              cx   := Min(Integer(uIdx), chasW - 1);
              cy   := Min(Integer(vIdx), chasH - 1);
              pTex := PRGBTriple(PByte(ChasRGB.ScanLine[cy]) + cx * SizeOf(TRGBTriple));
              pOut^ := pTex^;
            end;

            // 3: top/nose texture, section 1 (rows 128-255), U mirrored
            3:
            begin
              fx   := Min(255 - Integer(uIdx), ferW - 1);
              fy   := Min(128 + Integer(vIdx), ferH - 1);
              pTex := PRGBTriple(PByte(FerRGB.ScanLine[fy]) + fx * SizeOf(TRGBTriple));
              pOut^ := pTex^;
            end;

            // 4: top/nose texture, section 1 (rows 128-255), direct UV
            4:
            begin
              fx   := Min(Integer(uIdx), ferW - 1);
              fy   := Min(128 + Integer(vIdx), ferH - 1);
              pTex := PRGBTriple(PByte(FerRGB.ScanLine[fy]) + fx * SizeOf(TRGBTriple));
              pOut^ := pTex^;
            end;

            // 5, 7: right side / front face, section 2 (rows 256-383), direct UV
            5, 7:
            begin
              fx   := Min(Integer(uIdx), ferW - 1);
              fy   := Min(256 + Integer(vIdx), ferH - 1);
              pTex := PRGBTriple(PByte(FerRGB.ScanLine[fy]) + fx * SizeOf(TRGBTriple));
              pOut^ := pTex^;
            end;

            // 8-11: tyres - direct UV into tyre texture (e.g. whbridg0)
            8, 9, 10, 11:
            begin
              tx   := Min(Integer(uIdx), tyrW - 1);
              ty   := Min(Integer(vIdx), tyrH - 1);
              pTex := PRGBTriple(PByte(TyrRGB.ScanLine[ty]) + tx * SizeOf(TRGBTriple));
              pOut^ := pTex^;
            end;

          end; // case

          Inc(pOut);
        end; // x
      end; // y

    except
      Result.Free;
      raise;
    end;
  finally
    FerRGB.Free;
    ChasRGB.Free;
    TyrRGB.Free;
  end;
end;

end.
