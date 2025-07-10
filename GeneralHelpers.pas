unit GeneralHelpers;

interface

uses
  Winapi.Windows, System.Threading, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics;

function ClampInt(const Value, MinValue, MaxValue: Integer): Integer;

function ColorDist2(const A, B: TRGBTriple): Double;

function StretchF(BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;

function BoolToInt(B: Boolean): Integer;

function IsPowerOfTwo(Value: Integer): Boolean;

implementation

function IsPowerOfTwo(Value: Integer): Boolean;
begin
  Result := (Value > 0) and ((Value and (Value - 1)) = 0);
end;

function ClampInt(const Value, MinValue, MaxValue: Integer): Integer;
begin
  if Value < MinValue then
    Result := MinValue
  else if Value > MaxValue then
    Result := MaxValue
  else
    Result := Value;
end;

function ColorDist2(const A, B: TRGBTriple): Double;
begin
  Result := Sqr(A.rgbtRed - B.rgbtRed) + Sqr(A.rgbtGreen - B.rgbtGreen) +
    Sqr(A.rgbtBlue - B.rgbtBlue);
end;


function StretchF(BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;
var
rect : Trect;
begin
  if (BMP.Width < 12) or (BMP.Height < 12) then
  begin
    rect := TRect.Create(0,0,outwidth,outheight);
    bmp.canvas.StretchDraw(rect,bmp);
    result := BMP;
  end
  else
  begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf24bit;
    Result.SetSize(OutWidth, OutHeight);
    SetStretchBltMode(Result.Canvas.Handle, HALFTONE);
    SetBrushOrgEx(Result.Canvas.Handle, 0, 0, nil);
    StretchBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height,
      BMP.Canvas.Handle, 0, 0, BMP.Width, BMP.Height, SRCCOPY);
  except
    FreeAndNil(Result);

    raise;
  end;
  end;
end;

function BoolToInt(B: Boolean): Integer;
begin
  if B then
    Result := 1
  else
    Result := 0;
end;


end.
