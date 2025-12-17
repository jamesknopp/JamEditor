unit JamPaletteDetector;

interface

uses
  System.SysUtils, System.Generics.Collections, System.IOUtils, System.JSON, Vcl.Dialogs, vcl.forms, system.uitypes,jamgeneral;

type


  TJamPaletteDetector = class
  private
    FMap: TDictionary<string,TJamType>;
    class var FInstance: TJamPaletteDetector;
    constructor Create;
    procedure LoadMap;
    procedure SaveMap;
  public
    class function  Instance: TJamPaletteDetector;
    class procedure Release;
    /// <summary>
    /// 1) Call your HW-header check BEFORE this; if it’s HW, skip this.
    /// 2) If side-car map has it, return that.
    /// 3) *.JIP → GP3.
    /// 4) Folder “GAMEJAMS” → GP2; “GP3JAMS” → GP3.
    /// 5) Else: ask once, store in map.
    /// </summary>
    function  Detect(const APath: string; silent : boolean): TJamType;
    procedure Remember(const APath: string; AType: TJamType);
    procedure ClearEntries;
  end;

implementation

const
  MAPPING_FILE = 'jam_palettes.json';

{ TJamPaletteDetector }

constructor TJamPaletteDetector.Create;
begin
  FMap := TDictionary<string,TJamType>.Create;
  LoadMap;
end;

procedure TJamPaletteDetector.LoadMap;
var
  JSONText: string;
  JSONArray: TJSONArray;
  I: Integer;
  Obj: TJSONObject;
  Pth: string;
  Ty: Integer;
begin
  if not TFile.Exists(MAPPING_FILE) then Exit;
  JSONText := TFile.ReadAllText(MAPPING_FILE);
  JSONArray := TJSONObject.ParseJSONValue(JSONText) as TJSONArray;
  try
    for I := 0 to JSONArray.Count-1 do
    begin
      Obj := JSONArray.Items[I] as TJSONObject;
      Pth := Obj.GetValue<string>('path');
      Ty  := Obj.GetValue<Integer>('type');
      FMap.AddOrSetValue(Pth, TJamType(Ty));
    end;
  finally
    JSONArray.Free;
  end;
end;

procedure TJamPaletteDetector.SaveMap;
var
  JSONArray: TJSONArray;
  Pair: TPair<string,TJamType>;
  Obj: TJSONObject;
begin
  JSONArray := TJSONArray.Create;
  try
    for Pair in FMap do
    begin
      Obj := TJSONObject.Create;
      Obj.AddPair('path', TJSONString.Create(Pair.Key));
      Obj.AddPair('type', TJSONNumber.Create(Ord(Pair.Value)));
      JSONArray.AddElement(Obj);
    end;
    TFile.WriteAllText(MAPPING_FILE, JSONArray.ToString);
  finally
    JSONArray.Free;
  end;
end;

function TJamPaletteDetector.Detect(const APath: string; silent : boolean): TJamType;
var
  Dir: string;
begin

  if FMap.TryGetValue(APath, Result) then
    Exit;


  if SameText(ExtractFileExt(APath), '.JIP') then
  begin
    Result := jamGP3SW;
    Exit;
  end;

  // 3) Folder heuristics
  Dir := ExtractFileDir(APath);
  while Dir <> '' do
  begin
    if SameText(ExtractFileName(Dir), 'GAMEJAMS') then
    begin
      Result := jamGP2;
      Exit;
    end;
    if SameText(ExtractFileName(Dir), 'GP3JAMS') then
    begin
      if isHWJAM(apath) then
      result := jamGP3HW
      else
      Result := jamGP3SW;
      Exit;
    end;
    if SameText(ExtractFileName(Dir), 'GP3JAMSH') then
    begin
      if isHWJAM(apath) then
      result := jamGP3HW
      else
      Result := jamGP3SW;
      Exit;
    end;
    if isHWJAM(apath) then
    begin
      result := jamGP3HW;
      exit;
    end;

    Dir := TDirectory.GetParent(Dir);
  end;

  // 4) Ambiguous: prompt user once      #
  if silent = false then
  begin
  case MessageDlg(
    Format('Is "%s" a GP2 or GP3 palette JAM?', [ExtractFileName(APath)]),
    mtCustom, [mbYes, mbNo], 0, mbOK, ['GP2', 'GP3']) of
  // Yes = GP2, No = GP3
    mrYes: Result := jamGP2;
    mrNo:  Result := jamGP3SW;
  else

  end;

  if Result in [jamGP2, jamGP3SW] then
    Remember(APath, Result);
  end;
end;

procedure TJamPaletteDetector.Remember(const APath: string; AType: TJamType);
begin
  FMap.AddOrSetValue(APath, AType);
  SaveMap;
end;

procedure TJamPaletteDetector.ClearEntries;
begin
  // 1) Clear the in‑memory dictionary
  FMap.Clear;

  // 2) Delete the JSON file so next load is empty
  if TFile.Exists(MAPPING_FILE) then
    TFile.Delete(MAPPING_FILE);
end;

class function TJamPaletteDetector.Instance: TJamPaletteDetector;
begin
  if FInstance = nil then
    FInstance := TJamPaletteDetector.Create;
  Result := FInstance;
end;

class procedure TJamPaletteDetector.Release;
begin
  FreeAndNil(FInstance);
end;

end.

