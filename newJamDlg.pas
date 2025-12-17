unit newJamDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, JamGeneral;

type
  TnewJamDialog = class(TForm)
    strName: TEdit;
    intHeight: TSpinEdit;
    radioJamType: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    btnCreate: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ResetForm;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  newJamDialog: TnewJamDialog;

implementation

uses
System.IOUtils, mainform, jampalette;

{$R *.dfm}


procedure TnewJamDialog.btnCreateClick(Sender: TObject);
var
  FileName: string;
  InvalidChars: TArray<Char>;
  c: Char;
  i : integer;
begin
  // 1) Get & trim
  FileName := Trim(strName.Text);
  if FileName.IsEmpty then
  begin
    ShowMessage('Please enter a file name.');
    Exit;
  end;

  // 2) Check for any invalid file‐name characters
  InvalidChars := TPath.GetInvalidFileNameChars; // ['<', '>', ':', '"', '/', '\', '|', '?', '*', …]
  for c in InvalidChars do
    if FileName.Contains(c) then
    begin
      ShowMessage(Format('The character "%s" is not allowed in file names.', [c]));
      Exit;
    end;

  // 3) Prevent entering an extension
  if FileName.Contains('.') then
  begin
    ShowMessage('Please do not include an extension; it will be added automatically.');
    Exit;
  end;

  // 4) Dispatch based on Jamtype.ItemIndex
  case RadioJamtype.ItemIndex of
    0:
    begin
    for I := 0 to 255 do
    GPXPal[I] := Gp2Pal[I];



    boolGP2JAM := true;
    boolGP3JAM := false;
    boolHWJAM := false;
    boolJipMode := false;
    jamtype := jamGP2;

    formmain.NewJam(filename,false,intheight.value);
    end;
    1:
    begin
      for I := 0 to 255 do
    GPXPal[I] := Gp3Pal[I];

    boolGP3JAM := true;
    jamtype := jamGP3SW;
    boolGP2JAM := false;
    boolHWJAM := false;
    boolJipMode := false;

     formmain.NewJam(filename,false,intheight.value);
    end;

    2:
    begin
    jamtype := jamGP3HW;


    boolHWJAM := true;


    boolGP3JAM := false;

    boolGP2JAM := false;

    boolJipMode := false;
    formmain.NewJam(filename,true,intheight.value);

    end;
    3: begin
      for I := 0 to 255 do
    GPXPal[I] := Gp3Pal[I];

    boolJipMode := true;
    boolGP3JAM := true;


    boolGP2JAM := false;
    boolHWJAM := false;
    boolJipMode := false;

    formmain.NewJam(filename,false,intheight.value);

    end;

  else
    ShowMessage('Please select a JAM type.');
  end;
  newjamdialog.close;

end;

procedure TnewJamDialog.Button2Click(Sender: TObject);
begin
newjamdialog.Close;
end;

procedure TnewJamDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (Key = VK_RETURN) and (ActiveControl = strName) then
  begin
    Key := 0;                // consume it
    btnCreate.Click;
  end;
end;

procedure TnewJamDialog.ResetForm;
begin
  strName.Text      := '';
  intHeight.Value   := 256;
  RadioJamtype.ItemIndex := 0; // e.g. GP2JAM
end;


procedure TnewJamDialog.FormShow(Sender: TObject);
begin
ResetForm;
end;

end.
