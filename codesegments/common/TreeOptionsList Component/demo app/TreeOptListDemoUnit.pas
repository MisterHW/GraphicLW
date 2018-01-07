unit TreeOptListDemoUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, Math;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    TypeImages: TImageList;
    ImageList1: TImageList;
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeView1Editing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure TreeView1KeyPress(Sender: TObject; var Key: Char);
    procedure TreeView1CustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure FormResize(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    PropertyCount    : integer;
    PropertyIconList : TImageList;
    PropertyIconGap  : integer;
    MouseButtonIsHeld: Boolean;
    PropertyMaskAtMouseDown : integer;
    PropertyStateAtMouseDown: Boolean;

    function GetPropertyAreaWidth: integer;
    property PropertyAreaWidth : integer read GetPropertyAreaWidth;
    procedure PropertyAreaClick(X, Y: integer; Node: TTreeNode);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  ItemArea, PropertyRect, IconRect: TRect;
  PropertyIdx : longint;
  PropertyFlagMask : Cardinal;
  IconOffset, IconPitch: integer;
  imgwidth: integer;
begin
  Sender.Canvas.Font.Style := [];

  // all background except text
  ItemArea := Node.DisplayRect(false);
  Sender.Canvas.Brush.Color := clwhite;
  Sender.Canvas.FillRect(ItemArea);

  ItemArea.Left := ItemArea.Right - GetPropertyAreaWidth;
  CopyRect(PropertyRect, ItemArea);

  Sender.Canvas.Brush.Color := $E0E0E0;
  if Node.StateIndex <> -1 then
    Sender.Canvas.FillRect(ItemArea);

  Sender.Canvas.Font.Color  := clblack;
  Sender.Canvas.Brush.Color := clwhite;
  ItemArea := Node.DisplayRect(true);
  Sender.Canvas.FillRect(ItemArea);

  if not assigned(PropertyIconList)then
       imgwidth := 16
  else
       imgwidth := PropertyIconList.Width;


  if (node.StateIndex > -1) and (assigned(PropertyIconList)) then
  begin

      IconOffset := PropertyRect.Left + PropertyIconGap div 2;
      IconPitch  := imgwidth + PropertyIconGap;

      PropertyFlagmask := 1;
      for PropertyIdx := 0 to min(PropertyCount-1, PropertyIconList.Count-1) do
      begin
          if Node.StateIndex and PropertyFlagmask = PropertyFlagMask then
          begin

             IconRect.Top := (PropertyRect.Top + PropertyRect.Bottom) div 2 - PropertyIconList.Height div 2;
             //IconRect.Bottom := IconRect.Top + PropertyIconList.Height;
             IconRect.Left := IconOffset + IconPitch * PropertyIdx;
             //IconRect.Right := IconRect.Left + PropertyIconList.Width;

             PropertyIconList.Draw(Node.TreeView.Canvas,Iconrect.Left, IconRect.Top,PropertyIdx );
          end
          else
          begin
             IconRect.Top := (PropertyRect.Top + PropertyRect.Bottom) div 2 - 3;
             IconRect.Bottom := IconRect.Top + 6;
             IconRect.Left := IconOffset + IconPitch * PropertyIdx + IconPitch div 2 -6;
             IconRect.Right := IconRect.Left + 6;

             Node.TreeView.Canvas.FillRect(IconRect);
          end; // if
          PropertyFlagmask := PropertyFlagmask shl 1;
      end; // for

  end // if
  else if (node.StateIndex > -1)  then
    begin

      IconOffset := PropertyRect.Left + PropertyIconGap div 2;
      IconPitch  := imgwidth + PropertyIconGap;

      PropertyFlagmask := 1;
      for PropertyIdx := 0 to PropertyCount-1 do
      begin
          if Node.StateIndex and PropertyFlagmask = PropertyFlagMask then
          begin
             IconRect.Top := (PropertyRect.Top + PropertyRect.Bottom) div 2 - 3;
             IconRect.Bottom := IconRect.Top + 6;
             IconRect.Left := IconOffset + IconPitch * PropertyIdx + IconPitch div 2 -6;
             IconRect.Right := IconRect.Left + 6;

             Sender.Canvas.Brush.Color := clblack;
             Node.TreeView.Canvas.FillRect(IconRect);
          end
          else
          begin
             IconRect.Top := (PropertyRect.Top + PropertyRect.Bottom) div 2 - 3;
             IconRect.Bottom := IconRect.Top + 6;
             IconRect.Left := IconOffset + IconPitch * PropertyIdx + IconPitch div 2 -6;
             IconRect.Right := IconRect.Left + 6;

             Sender.Canvas.Brush.Color := clwhite;
             Node.TreeView.Canvas.FillRect(IconRect);
          end; // if
          PropertyFlagmask := PropertyFlagmask shl 1;
      end; // for

    end; // if

  Sender.Canvas.Brush.Color := clwhite;

end;

procedure TForm1.TreeView1Editing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := false;
end;

procedure TForm1.TreeView1KeyPress(Sender: TObject; var Key: Char);
var idx: integer;
begin
  if key = '-' then
  begin
    for idx := 0  to TreeView1.SelectionCount -1 do
      TreeView1.Selections[idx].Collapse(false);
  end;

  if key = '+' then
  begin
    for idx := 0  to TreeView1.SelectionCount -1 do
      TreeView1.Selections[idx].Expand(false);
  end;

end;

procedure TForm1.TreeView1CustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  //Sender.Canvas.FillRect(ARect);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  TreeView1.Repaint;
end;

procedure TForm1.TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rect: TRect;
  node: TTreeNode;
begin
  node := Treeview1.GetNodeAt(X,Y);
  if not assigned(node) then exit;

  MouseButtonIsHeld := true;

  rect := node.DisplayRect(false);
  X := X - (rect.Right - GetPropertyAreaWidth); // shift coordinates to PropertyArea box
  Y := Y - rect.Top;

  if X >= 0 then PropertyAreaClick(X, Y, node);
end;


procedure TForm1.PropertyAreaClick(X, Y: integer; Node: TTreeNode);
var
  IconOffset, IconPitch: integer;
  PropertyRect: TRect;
  PropertyIdx : integer;
  PropertyMask: Cardinal;
  imgwidth    : integer;
begin
  if Node.StateIndex > -1 then
  begin
    PropertyRect      := Node.DisplayRect(false);
    PropertyRect.Left := PropertyRect.Right - GetPropertyAreaWidth;

     if not assigned(PropertyIconList)then
       imgwidth := 16
     else
       imgwidth := PropertyIconList.Width;

    IconOffset := PropertyRect.Left + PropertyIconGap div 2;
    IconPitch  := imgwidth + PropertyIconGap;

    PropertyIdx := (X - PropertyIconGap div 2) div IconPitch;

    if (PropertyIdx >= 0) and (PropertyIdx < 31) then
    begin
      PropertyMask := Cardinal(Trunc(power(2,PropertyIdx)));
      Node.StateIndex := Node.StateIndex XOR PropertyMask; // toggle property

      PropertyMaskAtMouseDown  := PropertyMask;
      PropertyStateAtMouseDown := (Node.StateIndex and PropertyMask) = PropertyMask;
    end; // if PropertyIndex within range

  end; // if Stateindex > -1

end;

function TForm1.GetPropertyAreaWidth: integer;
var
  imgwidth : integer;
begin
  if not assigned(PropertyIconList)then
    imgwidth := 16
  else
    imgwidth := PropertyIconList.Width;

  if PropertyCount > 0 then
    result := PropertyCount * imgwidth + (PropertyCount) * PropertyIconGap
  else
    result := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PropertyCount    := 4;
  PropertyIconGap  := 8;
  PropertyIconList := ImageList1;
end;

procedure TForm1.TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseButtonIsHeld := false;
end;

procedure TForm1.TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  rect: TRect;
  node: TTreeNode;
begin
  if not MouseButtonIsHeld then exit;

  node := Treeview1.GetNodeAt(X,Y);
  if not assigned(node) then exit;

  if Node.StateIndex > -1 then
  begin
    if PropertyStateAtMouseDown = true then
      Node.StateIndex := Node.StateIndex or PropertyMaskAtMouseDown
    else
      Node.StateIndex := Node.StateIndex and (not PropertyMaskAtMouseDown);
  end;
end;

end.
