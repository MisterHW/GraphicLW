program VOHandler;



uses
  Forms,
  VOHandlerMain in 'VOHandlerMain.pas' {Form1},
  VisualOverlayClass in '..\VisualOverlayClass.pas',
  MMXBlend in '..\MMXBlend.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
