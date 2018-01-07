program Browser;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  GraphicColor in '..\GraphicColor.pas',
  GraphicCompression in '..\GraphicCompression.pas',
  GraphicEx in '..\GraphicEx.pas',
  GraphicStrings in '..\GraphicStrings.pas',
  JPG in '..\JPG.pas',
  MZLib in '..\MZLib.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

