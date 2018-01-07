unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MultiFunctionalInputDialog, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  dlg: TMultiFunctionalInputDialog;
  w, h: longint;
begin
  dlg := TMultiFunctionalInputDialog.Create;
  dlg.frm.Caption := 'New Layer';
  dlg.AddInputField('Width' ,@w,'numeric','','','1000');
  dlg.AddInputField('Height',@h,'numeric','','','1000');
  if dlg.Execute then
  begin

  end;
  dlg.Destroy;
end;

end.
