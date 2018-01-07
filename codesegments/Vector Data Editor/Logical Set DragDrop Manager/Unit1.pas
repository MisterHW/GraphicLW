unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, Math, Menus, ActnPopup,
  DragDropButtons;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Image1: TImage;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    PopupActionBar1: TPopupActionBar;
    SelectAll1: TMenuItem;
    ClearAll1: TMenuItem;
    InvertSelection1: TMenuItem;
    procedure DNDMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DNDMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DNDMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    StartObject : TSpeedButton;
    EndObject   : TSpeedButton;
    moving      : Boolean;
    procedure UpdateArrow(a, b: TSpeedButton; target: TImage);
  public
    cmp: TDragDropButtonField;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.UpdateArrow(a, b: TSpeedButton; target: TImage);
var
 recta, rectb: TRect;
 arrowstartx, arrowendx: integer;
begin
  with image1.Picture do
  begin
    Bitmap.Canvas.Brush.Color := Panel1.Color;
    if Bitmap.Height * Bitmap.Width = 0 then
    begin
      Bitmap.Width  := image1.Width;
      Bitmap.Height := image1.Height;
    end;
    Bitmap.Canvas.FillRect(rect(0,0,image1.width, image1.Height));

    if not assigned(a) then exit;
    recta := a.BoundsRect;

    Bitmap.Canvas.Pen.Color := $303030;
    Bitmap.Canvas.Pen.Width := 2;

    Bitmap.Canvas.MoveTo(recta.Left -image1.Left,image1.Height-2);
    Bitmap.Canvas.LineTo(recta.Right-image1.Left,image1.Height-2);

    if assigned(b) and (a<>b) then
    begin
      rectb := b.BoundsRect;

      arrowstartx := (recta.Left + recta.Right) div 2;
      arrowendx   := (rectb.Left + rectb.Right) div 2;

      Bitmap.Canvas.MoveTo( arrowstartx -image1.Left,
                            image1.Height-2);
      Bitmap.Canvas.LineTo( arrowstartx-image1.Left + 8 * sign(arrowendx- arrowstartx),
                            image1.Height-2-8);

      Bitmap.Canvas.LineTo( arrowendx  -image1.Left - 8 * sign(arrowendx- arrowstartx),
                            image1.Height-2-8);

      Bitmap.Canvas.LineTo( arrowendx -image1.Left,
                            image1.Height-2);

      // Bitmap.Canvas.Pen.Color := clblue;

      Bitmap.Canvas.MoveTo(rectb.Left -image1.Left,image1.Height-2);
      Bitmap.Canvas.LineTo(rectb.Right-image1.Left,image1.Height-2);
    end;


  end;
end;

procedure TForm1.DNDMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StartObject := nil;
  EndObject   := nil;

  if not assigned(Sender) then exit;
  if not (Sender is TSpeedButton) then exit;
  X := X + (Sender as TSpeedButton).Left;
  Y := Y + (Sender as TSpeedButton).Top;

  StartObject := Sender as TSpeedButton;
  EndObject   := StartObject;

  Speedbutton5.Hide;
  SpeedButton6.Hide;
  UpdateArrow(StartObject, EndObject,Image1);
  moving := true;
end;

procedure TForm1.DNDMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  pos: TPoint;
begin
  if (not assigned(StartObject)) or (not moving) then exit;

  if not assigned(Sender) then exit;
  if not (Sender is TSpeedButton) then exit;
  X := X + (Sender as TSpeedButton).Left;
  Y := Y + (Sender as TSpeedButton).Top;

  Sender := (Sender as TSpeedButton).Parent.ControlAtPos( Point(X,Y), false, true, true);
  if not (Sender is TSpeedButton) then exit;

  if  EndObject <> Sender then
  begin
    EndObject := Sender as TSpeedButton;
    UpdateArrow(StartObject, EndObject,Image1);
  end;
end;

procedure TForm1.DNDMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  moving := False;
  if not assigned(StartObject) then exit;

  if not assigned(Sender) then exit;
  if not (Sender is TSpeedButton) then exit;
  X := X + (Sender as TSpeedButton).Left;
  Y := Y + (Sender as TSpeedButton).Top;

  Sender := (Sender as TSpeedButton).Parent.ControlAtPos( Point(X,Y), false, true, true);

  if (Sender is TSpeedButton) then
  if  EndObject <> Sender then
  begin
    EndObject := Sender as TSpeedButton;
    UpdateArrow(StartObject, EndObject,Image1);
  end;

  UpdateArrow(StartObject, EndObject,Image1);

{  if EndObject <> StartObject then
    memo1.Lines.Add(StartObject.Caption + ' >> ' + EndObject.Caption)
  else
    memo1.Lines.Add('self : ' + StartObject.Caption);}

  if StartObject = EndObject then
  begin
    UpdateArrow(nil,nil,Image1);
    pt := StartObject.ClientToScreen(Point(0,StartObject.Height));
    Self.PopupActionBar1.Popup(pt.X, pt.Y);
  end
  else
  begin
    SpeedButton5.Left := (EndObject.Left + EndObject.Width div 2) - Speedbutton5.Width+1;
    SpeedButton6.Left := (EndObject.Left + EndObject.Width div 2)+2;
    Speedbutton5.Show;
    SpeedButton6.Show;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  btn : TSpeedButton;
begin
  cmp := TDragDropButtonField.Create(self);
  cmp.Parent := self;
  cmp.Top    := 0;
  cmp.Left   := 250;
  cmp.ActionBar := self.PopupActionBar1;

  btn := cmp.NewButton(0);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  btn.Glyph.Canvas.Draw(0,0,SpeedButton1.Glyph);

  btn := cmp.NewButton(1);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  btn.Glyph.Canvas.Draw(0,0,SpeedButton2.Glyph);

  btn := cmp.NewButton(2);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  btn.Glyph.Canvas.Draw(0,0,SpeedButton3.Glyph);

  btn := cmp.NewButton(3);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  btn.Glyph.Canvas.Draw(0,0,SpeedButton4.Glyph);
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  Speedbutton5.Hide;
  SpeedButton6.Hide;
  UpdateArrow(nil, nil, Image1);
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  Speedbutton5.Hide;
  SpeedButton6.Hide;
  UpdateArrow(nil, nil, Image1);
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  Speedbutton5.Hide;
  SpeedButton6.Hide;
  UpdateArrow(nil, nil,Image1);
end;

end.
