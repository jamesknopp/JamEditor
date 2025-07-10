unit resampler;

uses
  Vcl.Graphics, Windows, Math, System.SysUtils, System.Classes,
  System.Threading;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..65535] of TRGBTriple;

const
  TCol_TransGP2   = $007FAB97; // RGB(151,171,127)
  TCol_TransGP3   = $0067673F; // RGB(63,103,103)
  TCol_TransGP3HW = $00F8FC00; // RGB(0,252,248)

function Clamp(Value, MinVal, MaxVal: Integer): Integer; inline;
begin
  if Value < MinVal then Result := MinVal
  else if Value > MaxVal then Result := MaxVal;
end;

function IsTransparentColor(const C: TRGBTriple): Boolean;
begin
  Result :=
    (RGB(C.rgbtRed, C.rgbtGreen, C.rgbtBlue) = TCol_TransGP2) or
    (RGB(C.rgbtRed, C.rgbtGreen, C.rgbtBlue) = TCol_TransGP3) or
    (RGB(C.rgbtRed, C.rgbtGreen, C.rgbtBlue) = TCol_TransGP3HW);
end;

function CubicHermite(t: Single): Single;
begin
  if t < 0 then t := -t;
  if t <= 1 then
    Result := (1.5 * t - 2.5) * t * t + 1
  else if t < 2 then
    Result := ((-0.5 * t + 2.5) * t - 4) * t + 2
  else
    Result := 0;
end;

function Sinc(x: Double): Double;
begin
  if x = 0 then Result := 1
  else Result := Sin(Pi * x) / (Pi * x);
end;

function Lanczos(x: Double; a: Integer): Double;
begin
  if Abs(x) < a then
    Result := Sinc(x) * Sinc(x / a)
  else
    Result := 0;
end;

// --------------------------------------------------
// Bilinear Resample
// --------------------------------------------------
function ResizeBitmapBilinear(const Src: TBitmap; NewWidth, NewHeight: Integer): TBitmap;
var
  SrcW, SrcH: Integer;
  Dst: TBitmap;
begin
  SrcW := Src.Width;
  SrcH := Src.Height;

  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.SetSize(NewWidth, NewHeight);

  TParallel.For(0, NewHeight - 1,
    procedure(y: Integer)
    var
      SrcScan: PRGBTripleArray;
      DstScan: PRGBTripleArray;
      x: Integer;
      SrcX, SrcY: Single;
      x1, y1, x2, y2: Integer;
      fx, fy: Single;
      w1, w2, w3, w4: Single;
      c1, c2, c3, c4: TRGBTriple;
      r, g, b: Integer;
    begin
      SrcY := (y + 0.5) * SrcH / NewHeight - 0.5;
      y1 := Trunc(SrcY);
      fy := SrcY - y1;
      y2 := Min(y1 + 1, SrcH - 1);
      y1 := Max(y1, 0);

      DstScan := Dst.ScanLine[y];
      for x := 0 to NewWidth - 1 do
      begin
        SrcX := (x + 0.5) * SrcW / NewWidth - 0.5;
        x1 := Trunc(SrcX);
        fx := SrcX - x1;
        x2 := Min(x1 + 1, SrcW - 1);
        x1 := Max(x1, 0);

        SrcScan := Src.ScanLine[y1];
        c1 := SrcScan^[x1];
        c2 := SrcScan^[x2];

        SrcScan := Src.ScanLine[y2];
        c3 := SrcScan^[x1];
        c4 := SrcScan^[x2];

        if IsTransparentColor(c1) or IsTransparentColor(c2) or
           IsTransparentColor(c3) or IsTransparentColor(c4) then
        begin
          if not IsTransparentColor(c1) then DstScan^[x] := c1
          else if not IsTransparentColor(c2) then DstScan^[x] := c2
          else if not IsTransparentColor(c3) then DstScan^[x] := c3
          else if not IsTransparentColor(c4) then DstScan^[x] := c4
          else DstScan^[x] := c1;
          Continue;
        end;

        w1 := (1 - fx) * (1 - fy);
        w2 := fx * (1 - fy);
        w3 := (1 - fx) * fy;
        w4 := fx * fy;

        r := Round(c1.rgbtRed * w1 + c2.rgbtRed * w2 + c3.rgbtRed * w3 + c4.rgbtRed * w4);
        g := Round(c1.rgbtGreen * w1 + c2.rgbtGreen * w2 + c3.rgbtGreen * w3 + c4.rgbtGreen * w4);
        b := Round(c1.rgbtBlue * w1 + c2.rgbtBlue * w2 + c3.rgbtBlue * w3 + c4.rgbtBlue * w4);

        DstScan^[x].rgbtRed   := Byte(Clamp(r, 0, 255));
        DstScan^[x].rgbtGreen := Byte(Clamp(g, 0, 255));
        DstScan^[x].rgbtBlue  := Byte(Clamp(b, 0, 255));
      end;
    end
  );

  Result := Dst;
end;

// --------------------------------------------------
// Bicubic Resample
// --------------------------------------------------
function ResizeBitmapBicubic(const Src: TBitmap; NewWidth, NewHeight: Integer): TBitmap;
var
  SrcW, SrcH: Integer;
  Dst: TBitmap;
begin
  SrcW := Src.Width;
  SrcH := Src.Height;

  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.SetSize(NewWidth, NewHeight);

  TParallel.For(0, NewHeight - 1,
    procedure(y: Integer)
    var
      DstScan: PRGBTripleArray;
      x, i, j, sx, sy: Integer;
      fx, fy, dx, dy: Single;
      Weight, rSum, gSum, bSum, totalWeight: Single;
      SrcScan: PRGBTripleArray;
      Pixel: TRGBTriple;
      HasTransparent: Boolean;
    begin
      fy := (y + 0.5) * SrcH / NewHeight - 0.5;
      sy := Floor(fy);
      dy := fy - sy;

      DstScan := Dst.ScanLine[y];
      for x := 0 to NewWidth - 1 do
      begin
        fx := (x + 0.5) * SrcW / NewWidth - 0.5;
        sx := Floor(fx);
        dx := fx - sx;

        rSum := 0; gSum := 0; bSum := 0; totalWeight := 0;
        HasTransparent := False;

        for j := -1 to 2 do
          for i := -1 to 2 do
          begin
            SrcScan := Src.ScanLine[Clamp(sy + j, 0, SrcH - 1)];
            Pixel := SrcScan^[Clamp(sx + i, 0, SrcW - 1)];

            if IsTransparentColor(Pixel) then
              HasTransparent := True;

            Weight := CubicHermite(i - dx) * CubicHermite(j - dy);

            rSum := rSum + Pixel.rgbtRed * Weight;
            gSum := gSum + Pixel.rgbtGreen * Weight;
            bSum := bSum + Pixel.rgbtBlue * Weight;
            totalWeight := totalWeight + Weight;
          end;

        if HasTransparent then
        begin
          SrcScan := Src.ScanLine[Clamp(sy, 0, SrcH - 1)];
          DstScan^[x] := SrcScan^[Clamp(sx, 0, SrcW - 1)];
        end
        else if totalWeight <> 0 then
        begin
          DstScan^[x].rgbtRed   := Clamp(Round(rSum / totalWeight), 0, 255);
          DstScan^[x].rgbtGreen := Clamp(Round(gSum / totalWeight), 0, 255);
          DstScan^[x].rgbtBlue  := Clamp(Round(bSum / totalWeight), 0, 255);
        end;
      end;
    end
  );

  Result := Dst;
end;

// --------------------------------------------------
// Lanczos Resample (Lanczos3)
// --------------------------------------------------
function ResizeBitmapLanczos(const Src: TBitmap; NewWidth, NewHeight: Integer): TBitmap;
const
  LanczosA = 3;
var
  SrcW, SrcH: Integer;
  Dst: TBitmap;
begin
  SrcW := Src.Width;
  SrcH := Src.Height;

  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.SetSize(NewWidth, NewHeight);

  TParallel.For(0, NewHeight - 1,
    procedure(y: Integer)
    var
      x, i, j, ix, iy: Integer;
      SrcX, SrcY: Double;
      fx, fy: Double;
      RSum, GSum, BSum, WeightSum, W: Double;
      SrcScan: PRGBTripleArray;
      DstScan: PRGBTripleArray;
      Pixel: TRGBTriple;
      HasTransparent: Boolean;
    begin
      SrcY := (y + 0.5) * SrcH / NewHeight - 0.5;
      fy := SrcY;
      DstScan := Dst.ScanLine[y];

      for x := 0 to NewWidth - 1 do
      begin
        SrcX := (x + 0.5) * SrcW / NewWidth - 0.5;
        fx := SrcX;

        RSum := 0; GSum := 0; BSum := 0; WeightSum := 0;
        HasTransparent := False;

        for j := -LanczosA + 1 to LanczosA do
        begin
          iy := Clamp(Round(fy) + j, 0, SrcH - 1);
          SrcScan := Src.ScanLine[iy];

          for i := -LanczosA + 1 to LanczosA do
          begin
            ix := Clamp(Round(fx) + i, 0, SrcW - 1);
            Pixel := SrcScan^[ix];

            if IsTransparentColor(Pixel) then
              HasTransparent := True;

            W := Lanczos(fx - ix, LanczosA) * Lanczos(fy - iy, LanczosA);

            RSum := RSum + Pixel.rgbtRed * W;
            GSum := GSum + Pixel.rgbtGreen * W;
            BSum := BSum + Pixel.rgbtBlue * W;
            WeightSum := WeightSum + W;
          end;
        end;

        if HasTransparent then
        begin
          iy := Clamp(Round(fy), 0, SrcH - 1);
          ix := Clamp(Round(fx), 0, SrcW - 1);
          SrcScan := Src.ScanLine[iy];
          DstScan^[x] := SrcScan^[ix];
        end
        else if WeightSum <> 0 then
        begin
          DstScan^[x].rgbtRed   := Clamp(Round(RSum / WeightSum), 0, 255);
          DstScan^[x].rgbtGreen := Clamp(Round(GSum / WeightSum), 0, 255);
          DstScan^[x].rgbtBlue  := Clamp(Round(BSum / WeightSum), 0, 255);
        end;
      end;
    end
  );

  Result := Dst;
end;

