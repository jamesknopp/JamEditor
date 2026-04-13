unit RCRPreview;

// RCR Car Preview Form
//
// Renders GP3/GP3 2000 car sprites by combining:
//   - RCR sprite JAM  (rcr1a-rcr5a)  from Gp3Jams\Main\
//   - Companion mask  (rcr1b-rcr5b)  from Gp3Jams\Main\ (auto-loaded)
//   - Livery JAM      (e.g. ferrar98) from Gp3Jams\liveries\
//   - Chassis JAM     (chassis3)     from Gp3Jams\Main\
//   - Tyre texture    (wh*.bmp)      from Gp3Jams\liveries\
//
// GP3 liveries  : arrows98 benett98 ferrar98 jordan98 mclare98 minardi98
//                 prost98 sauber98 stewar98 tyrrel98 willia98
// GP3 2000 liveries: 1mclar00 2mclar00 arrows00 bar00 benett00 ferrar00
//                    jaguar00 jordan00 minardi00 prost00 sauber00 willia00

interface

uses
  Winapi.Windows,
  System.SysUtils, System.IOUtils, System.Types, System.StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  JamGeneral, JamSW, RCRRender, Vcl.Graphics;

type
  TGameChoice = (gcGP3, gcGP3_2000);

  TRCRPreviewForm = class(TForm)
    pnlControls: TPanel;
    lblGame: TLabel;
    cboGame: TComboBox;
    lblAngle: TLabel;
    cboAngle: TComboBox;
    lblLivery: TLabel;
    cboLivery: TComboBox;
    btnRender: TButton;
    lblStatus: TLabel;
    ScrollBox1: TScrollBox;
    imgPreview: TImage;
    procedure FormShow(Sender: TObject);
    procedure cboGameChange(Sender: TObject);
    procedure btnRenderClick(Sender: TObject);
  private
    function  GetGameRoot: string;
    function  GetMainJamDir: string;  // Gp3Jams\Main\  — RCR JAMs + chassis3.jam
    function  GetLiveryDir: string;   // Gp3Jams\liveries\ — livery JAMs + tyre BMPs
    function  GetLiveryList: TArray<string>;
    function  LoadJamCanvas(const Path: string): TBitmap;
    procedure PopulateGameList;
    procedure PopulateAngles;
    procedure PopulateLiveries;
    procedure SetStatus(const Msg: string);
    procedure AutoDetectGame;
  public
    // Set before showing to pre-select the angle matching the open JAM.
    PreselectedAngle: string;
  end;

var
  RCRPreviewForm: TRCRPreviewForm;

implementation

{$R *.dfm}

const
  // Hardcoded livery JAM names per game version
  GP3LiveryNames: array [0..10] of string = (
    'arrows98', 'benett98', 'ferrar98', 'jordan98', 'mclare98',
    'minardi98', 'prost98',  'sauber98', 'stewar98', 'tyrrel98', 'willia98');

  GP3_2000LiveryNames: array [0..11] of string = (
    '1mclar00', '2mclar00', 'arrows00', 'bar00',    'benett00', 'ferrar00',
    'jaguar00', 'jordan00', 'minardi00','prost00',   'sauber00', 'willia00');

// ---------------------------------------------------------------------------
//  Path helpers
// ---------------------------------------------------------------------------

function TRCRPreviewForm.GetGameRoot: string;
begin
  if cboGame.ItemIndex < 0 then
    Exit('');
  case TGameChoice(cboGame.Items.Objects[cboGame.ItemIndex]) of
    gcGP3:      Result := strGP3Location;
    gcGP3_2000: Result := strGP32kLocation;
  else
    Result := '';
  end;
end;

function TRCRPreviewForm.GetMainJamDir: string;
begin
  // RCR sprite JAMs (rcr?a/b) and chassis3.jam
  Result := IncludeTrailingPathDelimiter(GetGameRoot) + 'Gp3Jams\Main\';
end;

function TRCRPreviewForm.GetLiveryDir: string;
begin
  // SW livery JAMs and tyre BMPs
  Result := IncludeTrailingPathDelimiter(GetGameRoot) + 'Gp3Jams\liveries\';
end;

function TRCRPreviewForm.GetLiveryList: TArray<string>;
begin
  if cboGame.ItemIndex < 0 then
    Exit(nil);
  case TGameChoice(cboGame.Items.Objects[cboGame.ItemIndex]) of
    gcGP3:      Result := TArray<string>(GP3LiveryNames);
    gcGP3_2000: Result := TArray<string>(GP3_2000LiveryNames);
  else
    Result := nil;
  end;
end;

procedure TRCRPreviewForm.SetStatus(const Msg: string);
begin
  lblStatus.Caption := Msg;
  lblStatus.Update;
end;

// ---------------------------------------------------------------------------
//  Load the rendered canvas from a JAM file as a TBitmap.
//  Caller owns the returned bitmap.  Returns nil if load fails.
// ---------------------------------------------------------------------------
function TRCRPreviewForm.LoadJamCanvas(const Path: string): TBitmap;
var
  Jam: TJamFile;
begin
  Result := nil;
  if not FileExists(Path) then
    Exit;

  Jam := TJamFile.Create;
  try
    Jam.SetGpxPal(boolGP2Jam);
    if not Jam.LoadFromFile(Path, False) then
      Exit;
    if not Assigned(Jam.originalCanvas) then
      Exit;

    Result := TBitmap.Create;
    Result.Assign(Jam.originalCanvas);
  finally
    Jam.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Population helpers
// ---------------------------------------------------------------------------

procedure TRCRPreviewForm.PopulateGameList;
const
  GameNames: array [TGameChoice] of string = (
    'Grand Prix 3', 'Grand Prix 3 2000');
var
  gc: TGameChoice;
  root, prevSel: string;
begin
  prevSel := cboGame.Text;
  cboGame.Items.Clear;

  for gc := Low(TGameChoice) to High(TGameChoice) do
  begin
    case gc of
      gcGP3:      root := strGP3Location;
      gcGP3_2000: root := strGP32kLocation;
    end;
    if (root <> '') and DirectoryExists(root) then
      cboGame.Items.AddObject(GameNames[gc], TObject(Ord(gc)));
  end;

  if prevSel <> '' then
    cboGame.ItemIndex := cboGame.Items.IndexOf(prevSel);
  if (cboGame.ItemIndex < 0) and (cboGame.Items.Count > 0) then
    cboGame.ItemIndex := 0;
end;

procedure TRCRPreviewForm.PopulateAngles;
const
  Angles: array [1..5] of string = ('rcr1a','rcr2a','rcr3a','rcr4a','rcr5a');
var
  dir, prevSel: string;
  n: integer;
begin
  prevSel := cboAngle.Text;
  if prevSel = '' then
    prevSel := PreselectedAngle;
  cboAngle.Items.Clear;

  dir := GetMainJamDir;
  if not DirectoryExists(dir) then
  begin
    SetStatus('RCR directory not found: ' + dir);
    Exit;
  end;

  for n := 1 to 5 do
    if FileExists(dir + Angles[n] + '.jam') then
      cboAngle.Items.Add(Angles[n]);

  if cboAngle.Items.Count = 0 then
  begin
    SetStatus('No rcr?a.jam files found in: ' + dir);
    Exit;
  end;

  cboAngle.ItemIndex := cboAngle.Items.IndexOf(LowerCase(prevSel));
  if cboAngle.ItemIndex < 0 then
    cboAngle.ItemIndex := 0;
end;

procedure TRCRPreviewForm.PopulateLiveries;
var
  dir, prevSel, name: string;
  liveries: TArray<string>;
begin
  prevSel := cboLivery.Text;
  cboLivery.Items.Clear;

  dir      := GetLiveryDir;
  liveries := GetLiveryList;

  if liveries = nil then
    Exit;

  if not DirectoryExists(dir) then
  begin
    SetStatus('Livery directory not found: ' + dir);
    Exit;
  end;

  for name in liveries do
    if FileExists(dir + name + '.jam') then
      cboLivery.Items.Add(name);

  if cboLivery.Items.Count = 0 then
  begin
    SetStatus('No livery JAM files found in: ' + dir);
    Exit;
  end;

  cboLivery.ItemIndex := cboLivery.Items.IndexOf(prevSel);
  if cboLivery.ItemIndex < 0 then
    cboLivery.ItemIndex := 0;
end;

procedure TRCRPreviewForm.AutoDetectGame;
var
  i: integer;
  gc: TGameChoice;
  root: string;
begin
  for i := 0 to cboGame.Items.Count - 1 do
  begin
    gc := TGameChoice(cboGame.Items.Objects[i]);
    case gc of
      gcGP3:      root := strGP3Location;
      gcGP3_2000: root := strGP32kLocation;
    else
      Continue;
    end;
    if DirectoryExists(IncludeTrailingPathDelimiter(root) + 'Gp3Jams\Main\') then
    begin
      cboGame.ItemIndex := i;
      Break;
    end;
  end;
end;

// ---------------------------------------------------------------------------
//  Form events
// ---------------------------------------------------------------------------

procedure TRCRPreviewForm.FormShow(Sender: TObject);
begin
  PopulateGameList;
  AutoDetectGame;
  PopulateAngles;
  PopulateLiveries;
  SetStatus('Select options above and click Render.');
end;

procedure TRCRPreviewForm.cboGameChange(Sender: TObject);
begin
  PopulateAngles;
  PopulateLiveries;
end;

// ---------------------------------------------------------------------------
//  Render
// ---------------------------------------------------------------------------

procedure TRCRPreviewForm.btnRenderClick(Sender: TObject);
var
  angleName, tyreName: string;
  rcrPath, liveryJamPath, chassisJamPath, tyrePath: string;
  RcrJam: TJamFile;
  SpriteB, SpriteA: TBitmap;
  LiveryTex, ChassisTex, TyreTex: TBitmap;
  Rendered: TBitmap;
  TyreNames: TArray<string>;
begin
  if cboGame.ItemIndex  < 0 then begin SetStatus('Please select a game version.'); Exit; end;
  if cboAngle.ItemIndex < 0 then begin SetStatus('Please select an RCR angle.');   Exit; end;
  if cboLivery.ItemIndex < 0 then begin SetStatus('Please select a livery.');      Exit; end;

  angleName      := cboAngle.Text;
  rcrPath        := GetMainJamDir + angleName          + '.jam';
  liveryJamPath  := GetLiveryDir  + cboLivery.Text     + '.jam';
  chassisJamPath := GetMainJamDir + 'chassis3'         + '.jam';

  // Find first available tyre texture
  tyrePath  := '';
  TyreNames := TArray<string>.Create(
    'whbridg0.bmp','whgood0.bmp','whmich0.bmp','whpire0.bmp',
    'whstre0.bmp', 'whyoko0.bmp');
  for tyreName in TyreNames do
    if FileExists(GetLiveryDir + tyreName) then
    begin
      tyrePath := GetLiveryDir + tyreName;
      Break;
    end;

  // Validate
  if not FileExists(rcrPath)       then begin SetStatus('RCR JAM not found: '      + rcrPath);        Exit; end;
  if not FileExists(liveryJamPath) then begin SetStatus('Livery JAM not found: '   + liveryJamPath);  Exit; end;
  if not FileExists(chassisJamPath)then begin SetStatus('chassis3.jam not found in: '+ GetMainJamDir);Exit; end;
  if tyrePath = ''                 then begin SetStatus('No tyre BMP (wh*.bmp) in: '+ GetLiveryDir);  Exit; end;

  SetStatus('Loading ' + angleName + '...');
  btnRender.Enabled := False;
  try
    // --- Load RCR sprite JAM (auto-loads companion mask) ---
    RcrJam := TJamFile.Create;
    try
      RcrJam.SetGpxPal(boolGP2Jam);
      if not RcrJam.LoadFromFile(rcrPath, False) then
      begin
        SetStatus('Failed to load RCR JAM: ' + rcrPath);
        Exit;
      end;

      if not Assigned(RcrJam.rcrMask) then
      begin
        SetStatus('Mask not found — expected ' +
          Copy(angleName, 1, Length(angleName) - 1) + 'b.jam alongside ' + angleName + '.jam');
        Exit;
      end;

      // Extract full-canvas UV planes from raw data
      SpriteB := RcrJam.ExtractRCRPlaneRaw(True);   // B plane = U coords
      SpriteA := RcrJam.ExtractRCRPlaneRaw(False);  // A plane = V coords
      try
        // --- Load textures from JAM files ---
        SetStatus('Loading textures...');

        LiveryTex  := LoadJamCanvas(liveryJamPath);
        ChassisTex := LoadJamCanvas(chassisJamPath);

        if not Assigned(LiveryTex) then
        begin
          SetStatus('Failed to load livery JAM: ' + liveryJamPath);
          Exit;
        end;
        if not Assigned(ChassisTex) then
        begin
          SetStatus('Failed to load chassis3.jam: ' + chassisJamPath);
          Exit;
        end;

        TyreTex := TBitmap.Create;
        TyreTex.LoadFromFile(tyrePath);

        try
          SetStatus('Rendering...');
          Rendered := RenderRCRCarSprite(
            SpriteB, SpriteA,
            RcrJam.rcrMask,
            LiveryTex, ChassisTex, TyreTex,
            jamGP3SW);
          try
            imgPreview.Picture.Assign(Rendered);
            imgPreview.AutoSize := True;
            SetStatus(Format('Done — %s with %s  (%d x %d)',
              [angleName, cboLivery.Text, Rendered.Width, Rendered.Height]));
          finally
            Rendered.Free;
          end;
        finally
          LiveryTex.Free;
          ChassisTex.Free;
          TyreTex.Free;
        end;
      finally
        SpriteB.Free;
        SpriteA.Free;
      end;
    finally
      RcrJam.Free;
    end;

  except
    on E: Exception do
      SetStatus('Error: ' + E.Message);
  end;

  btnRender.Enabled := True;
end;

end.
