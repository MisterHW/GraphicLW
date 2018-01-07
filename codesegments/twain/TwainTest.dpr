program TwainTest;

uses
  Forms,
  _main in '_main.pas' {ScanForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TScanForm, ScanForm);
  Application.Run;
end.
