program RawImportDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MultiFunctionalInputDialog in '..\common\MultiFunctionalInputDialog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
