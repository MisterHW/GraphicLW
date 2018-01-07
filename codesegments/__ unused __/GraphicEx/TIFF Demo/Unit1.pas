unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GraphicEX, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  img : TTIFFGraphic;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  img.LoadFromFile('twolayer.tif');
  Canvas.Draw(100,100,img);

  Windows.BitBlt(Canvas.Handle,600,100,512,512,img.MaskHandle,0,0,SRCCOPY); 

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  img := TTIFFGraphic.Create;
end;

end.
