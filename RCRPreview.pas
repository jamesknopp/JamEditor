unit RCRPreview;

// RCR Car Preview Form
//
// Allows the user to select a game version, RCR sprite angle (rcr1a-rcr5a),
// and livery texture, then renders the full car sprite sheet using
// RenderRCRCarSprite and displays the result.
//
// Texture path resolution (GP3 / GP3 2000):
//   RCR JAMs + livery BMPs + chassis.bmp : <GameRoot>\Gp3Jams\Main\
//   Surface/SRF JAMs                     : <GameRoot>\surfgen\
//
// The companion mask (rcr?b.jam) is loaded automatically alongside the
// selected RCR angle JAM (handled by TJamFile.LoadFromFile).

interface

uses
  Winapi.Windows,
  System.SysUtils, System.IOUtils, System.Types, System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  JamGeneral, JamSW, RCRRender;

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
    function  GetMainJamDir: string;
    procedure PopulateGameList;
    procedure PopulateAngles;
    procedure PopulateLiveries;
    procedure SetStatus(const Msg: string);
    procedure AutoDetectGame;
  public
    // Optional: set before showing to pre-select the angle matching
    // the currently open JAM (e.g. 'rcr1a').
    PreselectedAngle: string;
  end;

var
  RCRPreviewForm: TRCRPreviewForm;

implementation

{$R *.dfm}

// ---------------------------------------------------------------------------
//  Helpers
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
  Result := IncludeTrailingPathDelimiter(GetGameRoot) + 'Gp3Jams\Main\';
end;

procedure TRCRPreviewForm.SetStatus(const Msg: string);
begin
  lblStatus.Caption := Msg;
  lblStatus.Update;
end;

// ---------------------------------------------------------------------------
//  Population helpers
// ---------------------------------------------------------------------------

procedure TRCRPreviewForm.PopulateGameList;
const
  GameNames: array [TGameChoice] of string = (
    'Grand Prix 3',
    'Grand Prix 3 2000'
  );
var
  gc: TGameChoice;
  root: string;
  prevSel: string;
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

  // Restore previous selection if still available
  if prevSel <> '' then
    cboGame.ItemIndex := cboGame.Items.IndexOf(prevSel);
  if (cboGame.ItemIndex < 0) and (cboGame.Items.Count > 0) then
    cboGame.ItemIndex := 0;
end;

procedure TRCRPreviewForm.PopulateAngles;
const
  Angles: array [1..5] of string = ('rcr1a','rcr2a','rcr3a','rcr4a','rcr5a');
var
  dir, path, prevSel: string;
  n: integer;
begin
  prevSel := cboAngle.Text;
  if prevSel = '' then
    prevSel := PreselectedAngle;
  cboAngle.Items.Clear;

  dir := GetMainJamDir;
  if not DirectoryExists(dir) then
  begin
    SetStatus('Game directory not found: ' + dir);
    Exit;
  end;

  for n := 1 to 5 do
  begin
    path := dir + Angles[n] + '.jam';
    if FileExists(path) then
      cboAngle.Items.Add(Angles[n]);
  end;

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
  dir, name, prevSel: string;
  files: TStringDynArray;
  f: string;
begin
  prevSel := cboLivery.Text;
  cboLivery.Items.Clear;

  dir := GetMainJamDir;
  if not DirectoryExists(dir) then
    Exit;

  files := TDirectory.GetFiles(dir, '*.bmp');
  for f in files do
  begin
    name := LowerCase(ExtractFileName(f));
    // Exclude non-livery assets
    if StartsStr('wh',      name) then Continue;  // tyres
    if StartsStr('car_srf', name) then Continue;  // UV atlas
    if StartsStr('hlm_srf', name) then Continue;  // helmet atlas
    if StartsStr('ccp_srf', name) then Continue;  // cockpit atlas
    if StartsStr('wh_srf',  name) then Continue;  // tyre atlas
    if StartsStr('rcr',     name) then Continue;  // rcr extracted frames
    if name = 'chassis.bmp'        then Continue;
    cboLivery.Items.Add(ChangeFileExt(ExtractFileName(f), ''));
  end;

  if cboLivery.Items.Count = 0 then
  begin
    SetStatus('No livery BMP files found in: ' + dir);
    Exit;
  end;

  cboLivery.ItemIndex := cboLivery.Items.IndexOf(prevSel);
  if cboLivery.ItemIndex < 0 then
    cboLivery.ItemIndex := 0;
end;

procedure TRCRPreviewForm.AutoDetectGame;
var
  i: integer;
  obj: TGameChoice;
  root: string;
begin
  // If a JAM was previously loaded, try to match its path against game roots
  if PreselectedAngle = '' then
    Exit;
  for i := 0 to cboGame.Items.Count - 1 do
  begin
    obj := TGameChoice(cboGame.Items.Objects[i]);
    case obj of
      gcGP3:      root := strGP3Location;
      gcGP3_2000: root := strGP32kLocation;
    else
      Continue;
    end;
    // Check if the game dir exists and contains rcr jams
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
  dir, rcrPath, liveryPath, chassisPath, tyrePath: string;
  angleName, tyreName: string;
  RcrJam: TJamFile;
  SpriteB, SpriteA: TBitmap;
  FerrariTex, ChassisTex, TyreTex: TBitmap;
  Rendered: TBitmap;
  TyreNames: TArray<string>;
begin
  if cboGame.ItemIndex < 0 then
  begin
    SetStatus('Please select a game version.');
    Exit;
  end;
  if cboAngle.ItemIndex < 0 then
  begin
    SetStatus('Please select an RCR angle.');
    Exit;
  end;
  if cboLivery.ItemIndex < 0 then
  begin
    SetStatus('Please select a livery.');
    Exit;
  end;

  dir        := GetMainJamDir;
  angleName  := cboAngle.Text;
  rcrPath    := dir + angleName + '.jam';
  liveryPath := dir + cboLivery.Text + '.bmp';
  chassisPath:= dir + 'chassis.bmp';

  // Find first available tyre texture
  tyrePath := '';
  TyreNames := TArray<string>.Create(
    'whbridg0.bmp','whgood0.bmp','whmich0.bmp','whpire0.bmp',
    'whstre0.bmp', 'whyoko0.bmp');
  for tyreName in TyreNames do
    if FileExists(dir + tyreName) then
    begin
      tyrePath := dir + tyreName;
      Break;
    end;

  // Validate
  if not FileExists(rcrPath) then
  begin
    SetStatus('RCR JAM not found: ' + rcrPath);
    Exit;
  end;
  if not FileExists(liveryPath) then
  begin
    SetStatus('Livery not found: ' + liveryPath);
    Exit;
  end;
  if not FileExists(chassisPath) then
  begin
    SetStatus('chassis.bmp not found in: ' + dir);
    Exit;
  end;
  if tyrePath = '' then
  begin
    SetStatus('No tyre texture (wh*.bmp) found in: ' + dir);
    Exit;
  end;

  SetStatus('Loading ' + angleName + '...');
  btnRender.Enabled := False;
  try
    // Load the RCR JAM (auto-loads companion mask rcr?b.jam)
    RcrJam := TJamFile.Create;
    try
      RcrJam.SetGpxPal(boolGP2Jam);
      if not RcrJam.LoadFromFile(rcrPath, False) then
      begin
        SetStatus('Failed to load: ' + rcrPath);
        Exit;
      end;

      if not Assigned(RcrJam.rcrMask) then
      begin
        SetStatus('Mask file not found (expected ' +
          ChangeFileExt(angleName, '') +
          Copy(angleName, Length(angleName), 1).Replace('a', 'b') +
          '.jam alongside the sprite).');
        Exit;
      end;

      // Extract full-canvas raw UV planes
      SpriteB := RcrJam.ExtractRCRPlaneRaw(True);   // B = U (odd bytes)
      SpriteA := RcrJam.ExtractRCRPlaneRaw(False);  // A = V (even bytes)
      try
        // Load textures
        FerrariTex := TBitmap.Create;
        ChassisTex := TBitmap.Create;
        TyreTex    := TBitmap.Create;
        try
          SetStatus('Loading textures...');
          FerrariTex.LoadFromFile(liveryPath);
          ChassisTex.LoadFromFile(chassisPath);
          TyreTex.LoadFromFile(tyrePath);

          SetStatus('Rendering...');
          Rendered := RenderRCRCarSprite(
            SpriteB, SpriteA,
            RcrJam.rcrMask,
            FerrariTex, ChassisTex, TyreTex,
            jamGP3SW);
          try
            imgPreview.Picture.Assign(Rendered);
            imgPreview.AutoSize := True;
            SetStatus(Format('Done — %s with %s  (%dx%d)',
              [angleName, cboLivery.Text,
               Rendered.Width, Rendered.Height]));
          finally
            Rendered.Free;
          end;
        finally
          FerrariTex.Free;
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
