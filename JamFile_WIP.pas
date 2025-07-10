unit JamFile;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections, Vcl.ComCtrls, System.IOUtils, Winapi.Windows, Vcl.Graphics, Vcl.Dialogs, Math, JamGeneral;

const
  dx: array[0..3] of Integer = (1, -1, 0,  0);
  dy: array[0..3] of Integer = (0, 0, 1,  -1);


var
    PaletteID: integer;
    HWJAM: boolean;

type
  TJamHeader = packed record
    NumItems: Word;
    JamTotalHeight: Word;
  end;

  TJamEntryInfo = packed record
    X: Byte;
    Y: Word;
    Unk: Byte;   //totally unk?
    Width: Word;
    Height: Word;
    Idx08: Word; //scale origin x
    Idx0A: Word; //scaling information?
    ImagePtr: Word;
    Idx0E: Word; // totally unk
    PaletteSizeDiv4: Word;
    JamId: Word;
    Idx14: Word; //flags
    Idx16: Byte; //untex color 1; entry in palette
    Idx17: Byte; //untext color 2; entry in palette
    Idx18: array[0..7] of Byte; // 3,4 and 6 seem to be used in car liveries (gp3)... maybe load up all JAM files and create CSVs with all the data to review??
  end;

  TJamEntry = class
  public
    FInfo: TJamEntryInfo;
    FPalettes: TLocalPaletteArray;
    FTexture: TBitmap;
    FRawTexture: TBytes;
    function GetPalette(Index: Integer): TBytes;
    function  GetPaletteSizeDiv4: Word;
    procedure SetPaletteSizeDiv4(const Value: Word);
    constructor Create(const Info: TJamEntryInfo); overload;     // in the class declaration:
    property Info: TJamEntryInfo read FInfo write FInfo;
    property Palettes: TLocalPaletteArray read FPalettes write FPalettes;
    property PaletteSizeDiv4: Word read  GetPaletteSizeDiv4  write SetPaletteSizeDiv4;

  end;

  TJamFile = class
  private

  public
   FHeader: TJamHeader;
    FEntries: TList<TJamEntry>;
    FRawData: TBytes;
    canvasHeight, canvasWidth: integer;
    CanvasBitmap : TBitmap;
    function UnJam(const Data: TBytes): TBytes;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure DecryptFile(const Filename: string);
    procedure PopulateTree(Tree: TTreeView);
    procedure EncodeTexture(jamID: Integer; texture : TBitmap);
    procedure ImportTexture(JamID : integer; textureFilename : string);
    procedure GenerateMipMap(bmp : TBitmap; jamID : Integer);
    function DrawCanvas(): TBitmap;
    function DrawSingleTexture(const Raw: TBytes; TotalImageSize: Integer; jamID:integer): TBitmap;
    function DrawJIPImage( const Raw: TBytes): TBitmap;
    function DrawRCRImage(const Raw: TBytes): TBitmap;
    function DrawOutlines(JamCanvas : TBitmap) : TBitmap;
    function DrawPalTexture(JamID : integer) : TBitmap;
    property Entries: TList<TJamEntry> read FEntries write FEntries;
  end;

implementation


{ TJamEntry }

constructor TJamEntry.Create(const Info: TJamEntryInfo);
var
i : integer;
begin
  FInfo := Info;
  PaletteID := 0;
end;

function TJamEntry.GetPalette(Index: Integer): TBytes;
begin
  if (Index < Low(FPalettes)) or (Index > High(FPalettes)) then
    raise ERangeError.CreateFmt('Palette index %d out of range',[Index]);
  Result := FPalettes[Index];
end;

function TJamEntry.GetPaletteSizeDiv4: Word;
begin
  Result := FInfo.PaletteSizeDiv4;
end;

procedure TJamEntry.SetPaletteSizeDiv4(const Value: Word);
begin
  FInfo.PaletteSizeDiv4 := Value;
  // (if there’s any side‐effect, you can do it here)
end;

{ TJamFile }




constructor TJamFile.Create;
begin
  FEntries := TList<TJamEntry>.Create;
  rcrJam := false;
  jipMode := false;
end;

destructor TJamFile.Destroy;
begin
  FEntries.Free;
  inherited;
end;

function TJamFile.UnJam(const Data: TBytes): TBytes;
var
  x: Cardinal; n,i: Integer; pc: PByte;
begin
  Result := Copy(Data);
  x := $B082F164;
  x := x or 1;
  n := Length(Result) div 4;
  pc := @Result[0];
  for i:=0 to n-1 do
  begin
    PCardinal(pc)^ := PCardinal(pc)^ xor x;
    Inc(pc,4);
    {$Q-}
    x := x + (x*4);
    {$Q+}
  end;
  // remaining bytes
  n := Length(Result) and 3;
  for i:=0 to n-1 do
  begin
    pc^ := pc^ xor Byte(x and $FF);
    Inc(pc);
    x := x shr 8;
  end;
end;

procedure TJamFile.DecryptFile(const Filename: string);
var
  Raw, Buf: TBytes;
  fs: TFileStream;
begin

  Raw := TFile.ReadAllBytes(FileName);

  Buf := UnJam(Raw);

  raw := buf;

  TFile.WriteAllBytes(filename +'dec', raw);

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
  rcrJam := false;
  jipMode := false;
  jamMaxWidth := 0;
  jamMaxHeight := 0;
  sFilename := lowercase(ChangeFileExt(ExtractFileName(FileName), ''));


  if TPath.GetExtension(FileName) = '.jip' then
    jipMode := true;

  CheckIfRCR(sFilename); // check if RCR JAM

  // Read & decrypt all file bytes
  Raw := TFile.ReadAllBytes(FileName);
  Buf := UnJam(Raw);
  Ptr := 0;

//  DecryptFile(filename);  Output decrypted JAM

  // Read header
  Move(Buf[Ptr], FHeader, SizeOf(FHeader));
  Inc(Ptr, SizeOf(FHeader));

// there is always one texture - lets ensure that

  if FHeader.NumItems = 0 then
       FHeader.NumItems := 1;

// handle odd jam files
    if not ((sFilename = 'car_srf') or (sFilename = 'hlm_srf') or (sFilename = 'vcp_srf') or (sFilename = 'vcp_srf2')) then
      begin
      // Read entries and add them to the list
        for i := 0 to FHeader.NumItems - 1 do
          begin
            Move(Buf[Ptr], Info, SizeOf(Info));
            Inc(Ptr, SizeOf(Info));
            FEntries.Add(TJamEntry.Create(Info));
          end;


      // if RCR then we apply special info
        if rcrJam then
          for i := 0 to FHeader.NumItems - 1 do
            begin
      //       FEntries[i].FInfo.Height := FEntries[i].FInfo.Height div 2;
             //FEntries[i].FInfo.width := FEntries[i].FInfo.width * 2;
           //  FEntries[i].FInfo.X := FEntries[i].FInfo.X*2;
            end;

      // otherwise carry on and parse through all the JAM texture entries
      for i := 0 to FHeader.NumItems - 1 do
        begin
          with FEntries[i] do
            begin
              jamMaxWidth := System.Math.Max(jamMaxWidth, info.X + info.Width);   //calculating a maximum width, just in case... all JAMs other than RCR/specials are 256 pixel width
              jamMaxHeight := System.Math.Max(jamMaxHeight, info.Y + info.Height); // calculate maximum height... JAM files have a maximum height of 768 pixels

              // if we have have an odd calculation, we default always to 256 pixels wide...
              if jamMaxWidth = 0 then
                jamMaxWidth := 256;

              var PalCount := FInfo.PaletteSizeDiv4;

               // if the file actually has a local palette, read it
              if PalCount > 0 then
                begin
                    // make sure there really are PalCount bytes * 4 blocks left in Buf
                    if Ptr + PalCount * 4 > Length(Buf) then
                      raise Exception.CreateFmt('Bad palette data for entry %d: need %d bytes at %d, have %d', [i, PalCount * 4, Ptr, Length(Buf)]);

                    // read each of the 4 palette strips
                    SetLength(FPalettes[0], PalCount);
                    Move(Buf[Ptr],     FPalettes[0][0], PalCount);
                    Inc(Ptr, PalCount);

                    SetLength(FPalettes[1], PalCount);
                    Move(Buf[Ptr],     FPalettes[1][0], PalCount);
                    Inc(Ptr, PalCount);

                    SetLength(FPalettes[2], PalCount);
                    Move(Buf[Ptr],     FPalettes[2][0], PalCount);
                    Inc(Ptr, PalCount);

                    SetLength(FPalettes[3], PalCount);
                    Move(Buf[Ptr],     FPalettes[3][0], PalCount);
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


    if rcrjam then
      begin
        jamMaxWidth := 512;
        jamMaxHeight := fheader.JamTotalHeight div 2;
      end;

      if sFilename = 'mhill' then
        begin
         jamMaxWidth := 512;
         jamMaxHeight := 164;
         fheader.JamTotalHeight := jamMaxHeight;
        end;

    if sFilename = 'car_srf' then
        begin
         jamMaxWidth := 512;
         jamMaxHeight := 544;
         fheader.JamTotalHeight := jamMaxHeight;
         rcrjam := true;
        end;

    if sFilename = 'hlm_srf' then
        begin
         jamMaxWidth := 256;
         jamMaxHeight := 64;
         fheader.JamTotalHeight := jamMaxHeight;
         rcrjam := true;
        end;

    if sFilename = 'wh_srf' then
        begin
         jamMaxWidth := 512;
         jamMaxHeight := 256;
         fheader.JamTotalHeight := jamMaxHeight;
         rcrjam := true;
        end;

    if sFilename = 'vcp_srf' then
        begin
         jamMaxWidth := 512;
         jamMaxHeight := 256;
         fheader.JamTotalHeight := jamMaxHeight;
         rcrjam := true;
        end;

    if sFilename = 'vcp_srf2' then
        begin
         jamMaxWidth := 512;
         jamMaxHeight := 256;
         fheader.JamTotalHeight := jamMaxHeight;
         rcrjam := true;
        end;

  // Now pull out exactly (image_total_size * 256) bytes of raw pixel data
  BlockCount := FHeader.JamTotalHeight;   // from file, in 256-byte units
  TrueSize   := BlockCount * 256;         // actual bytes
  if Ptr + TrueSize > Length(Buf) then
    raise Exception.CreateFmt('JAM image data too short: need %d bytes at offset %d, file has %d',[TrueSize, Ptr, Length(Buf)]);

 // SetLength(FRawData, TrueSize);
//  Move(Buf[Ptr], FRawData[0], Length(FRawdata));
 FRawData := Copy(Buf, Ptr, Length(Buf) - Ptr);

 if not rcrJam then
  for i := 0 to FHeader.NumItems - 1 do
    begin
      with FEntries[i] do
     //  FTexture := DrawSingleTexture(FRawData,Length(FRawData),i)
     end; // build a single JAM texture, whilst handling palette information
end;

function TJamFile.DrawPalTexture(JamID : integer) : TBitmap;
var
  W, H, X0, Y0: Integer;
  SrcStride: Integer;
  PalCount, i, x, y, idx, dst: Integer;
  LocalPal: array[0..255] of Byte;
  jamTex : TBitmap;

begin

if jamID = -1 then
  exit;

W := FEntries[JamID].FInfo.Width;
H := FEntries[JamID].FInfo.Height;
JamTex := TBitmap.Create;
JamTex.PixelFormat := pf8bit;
JamTex.Width  := W;
JamTex.Height := H;
JamTex.Palette := CreateLocalPalette(LocalGpxPal);


  // 3) Build fixed‐size local palette (always 256 entries)
  //    Default = identity mapping
  for i := 0 to 255 do
    LocalPal[i] := Byte(i);

  //    Overwrite from the file’s palette (up to 256 entries)
  PalCount := FEntries[JamID].FInfo.PaletteSizeDiv4;

  if PalCount > 256 then
    PalCount := 256;
  for i := 0 to PalCount - 1 do
    LocalPal[i] := FEntries[JamID].FPalettes[PaletteID][i];

try
    for y := 0 to H -1 do
      for x := 0 to W -1 do
      begin
        idx := x + y*jamtex.Width;
        dst := LocalPal[FEntries[JamID].FRawTexture[idx]];  // 0..255 guaranteed
        JamTex.Canvas.Pixels[x,y] :=  RGB(GPxPal[dst].R, GPxPal[dst].G, GPxPal[dst].B);
      end;
    Result := JamTex;
except
raise;

end;

end;

procedure TJamFile.EncodeTexture(jamID: Integer; texture: TBitmap);
var
  entry   : TJamEntry;
  info    : TJamEntryInfo;
  W, H    : Integer;
  palSize : Integer;
  indices, TempRaw: TBytes;
  LocalPal: array[0..255] of Byte; // maps globalIdx -> localIdx or $FF
  localCount: Integer;
  prevOrigIdx, origIdx: Byte;
  x,x0, y,y0, i, j, pos: Integer;
  prevIdx, nextIdx: Byte;
  colPrev, colNext, avgRGB: TRGB;
  LocalPalInt, LocalPalEdge: array[0..255] of Byte;
  px, nx, ny : Byte;
  isEdge: Boolean;
  rawIDX : byte;
  dstPos, srcStride : integer;

begin
  entry   := FEntries[jamID];
  info    := entry.FInfo;
  W       := info.Width;    H := info.Height;
  X0      := info.X;         Y0 := info.Y;
  palSize := entry.PaletteSizeDiv4;


  // 1) grab the raw 8-bit indices from the bitmap
  indices := BitmapToIndices(texture);

  // 2) init mapping and palette0
  for i := 0 to 255 do
    LocalPal[i] := $FF;
  SetLength(entry.FPalettes[0], palSize);

  localCount := 0;
  prevOrigIdx := $FF;

   // 1) initialize maps to “unassigned”
  for origIdx := 0 to 255 do
  begin
    LocalPalInt[origIdx] := $FF;
    LocalPalEdge[origIdx] := $FF;
  end;


 // 3) scan row-major, building two-tone palette
SetLength(TempRaw, W*H);
//
for y := 0 to H-1 do
  for x := 0 to W-1 do
  begin
    origIdx := indices[y * W + x];

    // 3a) detect edge
    isEdge := False;
    for i := 0 to High(DX) do
      if InRange(x + DX[i], 0, W-1) and InRange(y + DY[i], 0, H-1) and (indices[(y+DY[i])*W + (x+DX[i])] <> origIdx) then
      begin
        isEdge := True;
        Break;
      end;

    if isEdge then
    begin
      // — edge variant —
      if LocalPalEdge[origIdx] = $FF then
      begin
        if localCount >= palSize then
          raise Exception.CreateFmt(
            'Too many edge variants: %d / %d',
            [localCount+1, palSize]
          );
        LocalPalEdge[origIdx] := Byte(localCount);
        entry.FPalettes[0][localCount] := origIdx;
        Inc(localCount);
      end;
      TempRaw[y * W + x] := LocalPalEdge[origIdx];
      Continue;
    end;

    // — interior variant —
    // if we’ve already assigned this origIdx, just map & skip
    if LocalPalInt[origIdx] <> $FF then
    begin
      TempRaw[y * W + x] := LocalPalInt[origIdx];
      Continue;
    end;

    // otherwise it’s brand-new — add it
    if localCount >= palSize then

      raise Exception.CreateFmt(
        'Too many interior variants: %d / %d',
        [localCount+1, palSize]
      );

    LocalPalInt[origIdx] := Byte(localCount);
    entry.FPalettes[0][localCount] := origIdx;
    Inc(localCount);

    // now map
    TempRaw[y * W + x] := LocalPalInt[origIdx];
  end;



  // 4) if you have spare slots, pad with your first entry
  for i := localCount to palSize-1 do
    entry.FPalettes[0][i] := entry.FPalettes[0][0];

  // 5) copy TempRaw into your FRawTexture / FRawData just like before
  SetLength(entry.FRawTexture, W * H);
  for y := 0 to H - 1 do
  begin
    pos := (Y0 + y)*256 + X0;  // srcStride = 256
    Move(TempRaw[y * W], entry.FRawTexture[y * W], W);
    Move(TempRaw[y * W], FRawData[pos], W);
  end;


  entry.FTexture := DrawSingleTexture(FRawData, Length(FRawData), jamID);
  generateMipmap(texture, jamid);
end;


procedure TJamFile.SaveToFile(const FileName: string);
var
  OutBuf: TBytes;
  Header: TJamHeader;
  Ptr, i, PalCount: Integer;
  Entry: TJamEntry;
begin
  // 1) Prepare header
  Header.NumItems := fheader.NumItems;
  Header.JamTotalHeight := fheader.JamTotalHeight;

  // 2) Estimate total size:
  //    header + all entry‐infos + all raw textures + all local palettes
  Ptr := SizeOf(Header) + FEntries.Count * SizeOf(TJamEntryInfo);
  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := FEntries[i];
    Ptr := Ptr + Length(FRawdata);
    // include any local palettes you wrote out in LoadFromFile
    PalCount := Entry.Finfo.PaletteSizeDiv4;  // e.g. same count used during Load
    Ptr := Ptr + PalCount * 4;               // assuming 4 palettes of that length
  end;
  SetLength(OutBuf, Ptr);

  // 3) Write header
  Move(Header, OutBuf[0], SizeOf(Header));
  Ptr := SizeOf(Header);

  // 4) Write each TJamEntryInfo
  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := FEntries[i];
    Move(Entry.FInfo, OutBuf[Ptr], SizeOf(Entry.FInfo));
    Inc(Ptr, SizeOf(Entry.FInfo));
  end;

  // 5) Write raw texture + palettes for each entry
  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := FEntries[i];

    // 5a) local palettes (up to four of them) — same order you read them in LoadFromFile
    PalCount := Entry.Finfo.PaletteSizeDiv4;
    if PalCount > 0 then
    begin
      Move(Entry.FPalettes[0][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
      Move(Entry.FPalettes[1][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
      Move(Entry.FPalettes[2][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
      Move(Entry.FPalettes[3][0], OutBuf[Ptr], PalCount);
      Inc(Ptr, PalCount);
    end;

    // 5b) raw texture bytes
  //  if Length(Entry.FRawTexture) > 0 then
 //   begin
 //     Move(Entry.FRawTexture[0], OutBuf[Ptr], Length(Entry.FRawTexture));
  //    Inc(Ptr, Length(Entry.FRawTexture));
//    end;
  end;

  Move(FRawdata[0], OutBuf[Ptr], Length(FRawdata));
  Inc(Ptr, Length(FRawdata));
  OutBuf := UnJam(OutBuf);
  // 7) Finally write everything out
  TFile.WriteAllBytes(FileName, OutBuf);
end;

function TJamFile.DrawSingleTexture(const Raw: TBytes; TotalImageSize: Integer; jamID:integer): TBitmap;
var
  W, H, X0, Y0: Integer;
  SrcStride: Integer;
  PalCount, i, x, y, idx, dst: Integer;
  LocalPal: array[0..255] of Byte;
  Bmp: TBitmap;
  dstID : integer;
begin
      W := FEntries[jamID].FInfo.Width;
      H := FEntries[jamID].FInfo.Height;
      SrcStride := 256;
      X0 := FEntries[jamID].FInfo.X;
      Y0 := FEntries[jamID].FInfo.Y;

  // 3) Build fixed‐size local palette (always 256 entries)
  //    Default = identity mapping
  for i := 0 to 255 do
    LocalPal[i] := Byte(i);

  //    Overwrite from the file’s palette (up to 256 entries)
  PalCount := FEntries[jamID].FInfo.PaletteSizeDiv4;

  if PalCount > 256 then
    PalCount := 256;
  for i := 0 to PalCount - 1 do
    LocalPal[i] := FEntries[jamID].FPalettes[PaletteID][i];

  // 4) Create an 8-bit bitmap & GDI palette
  Bmp := TBitmap.Create;
  try
  //  Bmp.PixelFormat := pf8bit;
    Bmp.Width  := W;
    Bmp.Height := H;
//    bmp.Palette := CreateLocalPalette(LocalGpxPal);

    setLength(FEntries[jamid].FRawTexture, W*H);

    // 5) Copy pixels, using SrcStride for raw indexing
    for y := 0 to H - 1 do
      for x := 0 to W - 1 do
      begin
        idx := X0 + x + (Y0 + y)*SrcStride;
        dstID := (y * W) + x;
        FEntries[jamID].FRawTexture[dstID] := raw[idx];
        dst := LocalPal[Raw[idx]];  // 0..255 guaranteed
        Bmp.Canvas.Pixels[x,y] :=  RGB(GPxPal[dst].R, GPxPal[dst].G, GPxPal[dst].B);
      end;
    Result := Bmp;
  except
    Bmp.Free;
    raise;
  end;


end;

function TJamFile.DrawJIPImage(const Raw: TBytes): TBitmap;
var
  i: Integer;
  CanvasData: TBytes;
  PalSize: Integer;
begin

  CanvasData := Raw;

  CanvasWidth := 256;
  CanvasHeight := FHeader.JamTotalHeight;

  CanvasBitmap := TBitmap.Create;
  CanvasBitmap.Width := CanvasWidth;
  CanvasBitmap.Height := CanvasHeight;
  CanvasBitmap.PixelFormat := pf8bit;
  CanvasBitmap.Palette := CreatePaletteFromColors;

  for I := 0 to CanvasBitmap.Height - 1 do
    Move(CanvasData[I * CanvasWidth], CanvasBitmap.ScanLine[I]^, CanvasWidth);

  result := CanvasBitmap;
end;

function TJamFile.DrawRCRImage(const Raw: TBytes): TBitmap;
var
  i: Integer;
  CanvasData: TBytes;
  PalSize: Integer;
begin

  CanvasData := FRawData;

  CanvasWidth := jamMaxWidth;
  CanvasHeight := jamMaxHeight;

  CanvasBitmap := TBitmap.Create;
  CanvasBitmap.PixelFormat := pf8bit;
  CanvasBitmap.Width := CanvasWidth;
  CanvasBitmap.Height := CanvasHeight;
  CanvasBitmap.Palette := CreatePaletteFromColors;

  for I := 0 to CanvasBitmap.Height - 1 do
    Move(CanvasData[I * CanvasWidth], CanvasBitmap.ScanLine[I]^, CanvasWidth);

  result := CanvasBitmap;
end;

function TJamFile.DrawOutlines(JamCanvas : TBitmap) : TBitmap;
var
centerX, centerY : integer;
i : integer;
begin

   if b_drawOutlines then
   begin
    jamcanvas.Canvas.pen.Style := psSolid;
    jamcanvas.Canvas.pen.width := 3 * i_jamZoom;

    for i := 0 to FEntries.Count-1 do
     begin
      if i = selectedTexture then
        jamcanvas.Canvas.pen.color := rgb(255,0,0)
      else
      jamcanvas.Canvas.pen.color := rgb(255,255,255);

      jamcanvas.Canvas.Brush.Style := bsClear;

      jamcanvas.Canvas.Rectangle(FEntries[i].FInfo.X,FEntries[i].FInfo.Y,FEntries[i].FInfo.X+FEntries[i].FInfo.Width,FEntries[i].FInfo.Y+FEntries[i].FInfo.Height);

      centerX := round(FEntries[i].FInfo.X+(FEntries[i].FInfo.Width/2));
      centerY := round(FEntries[i].FInfo.Y+(FEntries[i].FInfo.Height/2));

      jamcanvas.canvas.font.color :=  jamcanvas.Canvas.pen.color;

      jamcanvas.canvas.TextOut(Round(centerX), Round(centerY), FEntries[i].FInfo.JamId.ToString);

   result := jamCanvas;
     end
   end
   else
   result:= jamCanvas;


end;

procedure TJamFile.PopulateTree(Tree: TTreeView);
var
  Root, Node: TTreeNode; i,j: Integer; E: TJamEntry;
begin
  Tree.Items.Clear;
  Root := Tree.Items.Add(nil, Format('JAM Items: %d', [FHeader.NumItems]));

  for i:=0 to FEntries.Count-1 do
  begin
    E := FEntries[i];
    Node := Tree.Items.AddChild(Root, Format('ID:%d  [%d x %d]', [E.Info.JamId, E.Info.Width, E.Info.Height]));
    Tree.Items.AddChild(Node, Format('X:%d', [E.Info.X]));
    Tree.Items.AddChild(Node, Format('Y:%d', [E.Info.Y]));
    Tree.Items.AddChild(Node, Format('PaletteSize:%d', [E.Info.PaletteSizeDiv4]));
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
  Tree.Items.AddChild(Root, Format('Canvas Size: W: %d x H: %d',[jamMaxWidth, jamMaxHeight]));

  tree.TopItem.Expanded := true;

end;


function TJamFile.DrawCanvas(): TBitmap;
var
  bmpAll, entryBmp: TBitmap;
  i : Integer;
begin

  bmpAll := TBitmap.Create;
  bmpAll.PixelFormat := pf32bit;
  bmpAll.Width  := jamMaxWidth;
  bmpAll.Height := jamMaxHeight;

  // pick default JAM transparent colour
    bmpAll.Canvas.Brush.Color := rgb (GPXPal[0].R,GPXPal[0].G, GPXPal[0].B);

  // fill the entire bitmap
    bmpAll.Canvas.FillRect(Rect(0, 0, bmpAll.Width, bmpAll.Height));

  // special draw routine for RCR JAMs
  if rcrjam = true then
  begin
    entryBMP := DrawRCRImage(FRawData);
    bmpAll.Canvas.Draw(0,0, entryBMP);
    Result := bmpAll;
    exit;
  end

  else

  // special draw routine for JIPs
  if jipMode = true then
  begin
    entryBMP := DrawJIPImage(FRawData);
    bmpAll.Height := FHeader.JamTotalHeight;
    jamMaxHeight := FHeader.JamTotalHeight;
    bmpAll.Canvas.Draw(0,0, entryBMP);
    Result := bmpAll;
    exit
    end

  else

// normal standard JAM files
  begin
      // 3) draw each entry into it
      for i := 0 to FEntries.Count - 1 do
        begin
          entryBmp := DrawSingleTexture(FRawData,Length(FRawData),i); // build a single JAM texture, whilst handling palette information
          try
               bmpAll.Canvas.Draw(FEntries[i].Info.X,FEntries[i].Info.Y,entryBmp); //draw said texture in its rightful position
          finally
                entryBmp.Free;
        end;
      end;
  end;
      Result := bmpAll;                     // 4) hand it back to the caller (caller owns the bitmap)

end;

procedure TJamFile.ImportTexture(JamID : integer; textureFilename : string);
var
srcPic : TPicture;
srcBmp : TBitmap;
textureWidth, textureHeight, srcWidth, srcHeight, y, x : integer;
tmpPalCanvas : TBitmap;
tmpCanvas: TBitmap;
pixelCol : TColor;
tmpRGB : TRGB;
tmpBMP : TBitmap;
begin
   SrcPic := TPicture.Create;
  textureWidth := FEntries[jamID].info.width;
  textureHeight := FEntries[jamID].info.height;
////  tmpCanvas := DrawCanvas;
//  //tmpCanvas.Canvas.draw(FEntries[JamID].Info.X,FEntries[JamID].Info.Y,Result);
  try
      tmpBMP := TBitmap.Create;

       SrcPic.LoadFromFile(textureFilename);          // auto‐detects BMP, JPEG, PNG, etc.
       tmpBMP.height := srcpic.height;
       tmpBMP.width := srcpic.width;

       tmpBMP := srcpic.bitmap;

      tmpCanvas := TBitmap.Create;
      tmpCanvas.PixelFormat := pf8bit;
      tmpCanvas.Palette := CreatePaletteFromColors;

      tmpPalCanvas := TBitmap.Create;
      tmpPalCanvas.PixelFormat := pf8bit;
      tmpPalCanvas.height := SrcPic.height;
      tmpPalCanvas.width := SrcPic.width;
      tmpPalCanvas.Palette := CreatePaletteFromColors;

      srcHeight := srcpic.Height;
      srcWidth := srcpic.Width;

      tmpCanvas.Width := textureWidth;
      tmpCanvas.Height := textureHeight;

        for y := 0 to srcHeight - 1 do
          for x := 0 to srcWidth - 1 do
          begin
            pixelCol := tmpBMP.Canvas.Pixels[x, y];
            tmpRGB.R := getRValue(pixelCol);
            tmpRGB.G := getGValue(pixelCol);
            tmpRGB.B := getBValue(pixelCol);
            tmpPalCanvas.Canvas.Pixels[x, y] := NearestPaletteEntryRGB(tmpRGB);
          end;



//      if ((srcHeight = textureHeight) and (srcWidth = textureWidth)) then
//              tmpCanvas.Assign(tmpPalCanvas)
//    else
       tmpCanvas.Canvas.StretchDraw(rect(0,0,textureWidth,textureHeight),tmpPalCanvas);

       FEntries[jamid].FTexture := tmpCanvas;
       EncodeTexture(jamID, tmpCanvas);
//    except
//      SrcBmp.Free;
//      raise;
//    end;
  finally
    SrcPic.Free;
  end;

end;

procedure TJamFile.GenerateMipMap(bmp : TBitmap; jamID : Integer);
var
  x, y,y0,x0, level, pxIdx: Integer;
  Width, Height: Integer;
  // buffers
  pixR, pixG, pixB: TArray<Byte>;
  pixY, pixCb, pixCr: TArray<Byte>;
  indices: TArray<Byte>;
  Yblur: TArray<Byte>;
  levelBMP: TBitmap;
  palEntry: TRGB;
  newPalEntry : Byte;
  tmpPal2: TRGB;
  palettes : TLocalPaletteArray;
  masterpalette : TBytes;
begin

  try

    Width  := bmp.Width;
    Height := bmp.Height;
    // allocate buffers
    SetLength(pixR, Width*Height);
    SetLength(pixG, Width*Height);
    SetLength(pixB, Width*Height);
    SetLength(indices, Width*Height);
    SetLength(pixY, Width*Height);
    SetLength(pixCb,Width*Height);
    SetLength(pixCr,Width*Height);

    // 2) Extract RGB & compute YCbCr
    for y := 0 to Height-1 do
      for x := 0 to Width-1 do
      begin
      pxIdx := y*Width + x;
       palEntry.r := getRValue(bmp.Canvas.Pixels[x,y]);
       palEntry.g := getGValue(bmp.Canvas.Pixels[x,y]);
       palEntry.b := getBValue(bmp.Canvas.Pixels[x,y]);

        RGBToYCbCr(palEntry.R, palEntry.G, palEntry.B, pixY[pxIdx], pixCb[pxIdx], pixCr[pxIdx]);
        indices := BitmapToIndices(bmp);
//        indices[pxIdx] := nearestpaletteentry(palEntry);
      end;


    palettes := FEntries[jamid].FPalettes;

    // 4) For levels 1..HighLevel: bilateral blur on Y, rebuild palette
    for level := 1 to 3 do
    begin
      BilateralFilterY(pixY, Width, Height,
        2 + 3*(level-1), // sigma_spatial grows with level
        20.0,            // sigma_range fixed
        Yblur);

      // rebuild palette entries
      // start from master
     palettes[level] := MasterPalette;
      // temporary accumulators
      var sumR, sumG, sumB, count: array[0..255] of Double;
      for pxIdx := 0 to Width*Height-1 do
//      begin
//        pxIdx; // use to avoid warning
//        level; // likewise
//      end;
      FillChar(sumR, SizeOf(sumR), 0);
      FillChar(sumG, SizeOf(sumG), 0);
      FillChar(sumB, SizeOf(sumB), 0);
      FillChar(count, SizeOf(count), 0);

      for pxIdx := 0 to Width*Height-1 do
      begin
        var idx := indices[pxIdx];
        var yv  := Yblur[pxIdx];
        // reconstruct blurred RGB via YCbCr
        var r_,g_,b_: Byte;
        YCbCrToRGB(yv, pixCb[pxIdx], pixCr[pxIdx], r_, g_, b_);
        sumR[idx]   := sumR[idx]   + r_;
        sumG[idx]   := sumG[idx]   + g_;
        sumB[idx]   := sumB[idx]   + b_;
        count[idx]  := count[idx]  + 1;
      end;

      // average and snap back to MasterPalette
      for var i := 0 to 255 do
        if count[i] > 0 then
        begin
          tmpPal2.R := Round(sumR[i]/count[i]);
          tmpPal2.G := Round(sumG[i]/count[i]);
          tmpPal2.B := Round(sumB[i]/count[i]);
          newPalEntry := NearestPaletteEntry(tmpPal2);
          // store the master palette entry
          FEntries[jamid].FPalettes[level][i] := newPalEntry;
        end;

    end;

  finally
    bmp.Free;
  end;
end;

end.
