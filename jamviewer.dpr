program JamViewer;

{$R *.dres}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  mainform in 'mainform.pas' {FormMain},
  JamGeneral in 'JamGeneral.pas',
  JamPalette in 'JamPalette.pas',
  GeneralHelpers in 'GeneralHelpers.pas',
  JamBrowser in 'JamBrowser.pas' {Form1},
  JamHW in 'JamHW.pas',
  JamSW in 'JamSW.pas',
  GP2Mask in 'GP2Mask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TJamBrowser, JamBrowserFrm);
  Application.Run;
end.
