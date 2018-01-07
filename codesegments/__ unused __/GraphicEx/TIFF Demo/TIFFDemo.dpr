program TIFFDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  GraphicColor in '..\GraphicColor.pas',
  GraphicCompression in '..\GraphicCompression.pas',
  GraphicEx in '..\GraphicEx.pas',
  GraphicStrings in '..\GraphicStrings.pas',
  JPG in '..\JPG.pas',
  MZLib in '..\MZLib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
