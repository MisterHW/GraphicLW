unit TreeOptionListCmp;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, Math, ExtCtrls, ComCtrls;


  Type TTreeOptionList = Class(TPanel)
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  private
    MouseButtonIsHeld          : Boolean;
    PropertyMaskAtMouseDown    : integer;
    PropertyStateAtMouseDown   : Boolean;
    ScrollbarVisibleLastTimeChecked: Boolean;
    FlagsChangedByMouseAction  : Boolean;
    MouseOutsidePropertyRegion : Boolean;

    TreeView1: TTreeView;

    procedure TreeViewCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeViewEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure TreeViewCustomDraw(Sender: TCustomTreeView; const ARect: TRect; var DefaultDraw: Boolean);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);

    procedure Resize; override; // TControl.Resize

    function  GetPropertyAreaWidth: integer;
    property  PropertyAreaWidth : integer read GetPropertyAreaWidth;
    procedure PropertyAreaClick(X, Y: integer; Node: TTreeNode);

    function ScrollbarVisibilityHasChanged: Boolean;
  public
    PropertyCount    : integer;
    PropertyIconList : TImageList;
    PropertyIconGap  : integer;

    OnNodeFlagsChanged: TNotifyEvent;
    OnTreeMouseDown: TMouseEvent;
    OnTreeMouseUp  : TMouseEvent;
    OnTreeMouseMove: TMouseMoveEvent;

    property Tree : TTreeView read TreeView1 write TreeView1;
    property FlagsChangedByMouse: Boolean read FlagsChangedByMouseAction;
  end;




implementation





Constructor TTreeOptionList.Create(AOwner: TComponent);
begin
  inherited;

  PropertyCount    := 4;
  PropertyIconGap  := 8;
  PropertyIconList := nil;

  self.BevelInner := bvNone;
  self.BevelOuter := bvNone;
  self.Caption := '';
  self.Width   := 256;
  self.Height  := 256;

  TreeView1 := TTreeView.Create(Application.MainForm);
  with TreeView1 do
  begin
    Parent       := Self;
    Align        := alClient;
    BevelInner   := bvNone;
    BevelKind    := bkFlat;
    BorderStyle  := bsNone;
    MultiSelect  := True;
    OnEditing    := TreeViewEditing;
    OnKeyPress   := TreeViewKeyPress;
    OnMouseDown  := TreeViewMouseDown;
    OnMouseMove  := TreeViewMouseMove;
    OnMouseUp    := TreeViewMouseUp;
    OnExpanded   := TreeViewExpanded;
    OnCollapsed  := TreeViewCollapsed;
    OnCustomDraw     := TreeViewCustomDraw;
    OnCustomDrawItem := TreeViewCustomDrawItem;
    DoubleBuffered   := true;
  end;

  ScrollbarVisibleLastTimeChecked  := false;

end;



Destructor TTreeOptionList.Destroy;
begin

  inherited;
end;



procedure TTreeOptionList.TreeViewCustomDrawItem( Sender: TCustomTreeView;
                                                  Node: TTreeNode;
                                                  State: TCustomDrawState;
                                                  var DefaultDraw: Boolean);
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
             IconRect.Top    := (PropertyRect.Top + PropertyRect.Bottom) div 2 - 3;
             IconRect.Bottom := IconRect.Top + 6;
             IconRect.Left   := IconOffset + IconPitch * PropertyIdx + IconPitch div 2 -6;
             IconRect.Right  := IconRect.Left + 6;

             Sender.Canvas.Brush.Color := clblack;
             Node.TreeView.Canvas.FillRect(IconRect);
          end
          else
          begin
             IconRect.Top    := (PropertyRect.Top + PropertyRect.Bottom) div 2 - 3;
             IconRect.Bottom := IconRect.Top + 6;
             IconRect.Left   := IconOffset + IconPitch * PropertyIdx + IconPitch div 2 -6;
             IconRect.Right  := IconRect.Left + 6;

             Sender.Canvas.Brush.Color := clwhite;
             Node.TreeView.Canvas.FillRect(IconRect);
          end; // if
          PropertyFlagmask := PropertyFlagmask shl 1;
      end; // for

    end; // if

  Sender.Canvas.Brush.Color := clwhite;

end;



procedure TTreeOptionList.TreeViewEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := false;
end;



procedure TTreeOptionList.TreeViewKeyPress(Sender: TObject; var Key: Char);
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



procedure TTreeOptionList.TreeViewCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin

end;



procedure TTreeOptionList.Resize;
begin
  TreeView1.Repaint;
end;



procedure TTreeOptionList.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rect: TRect;
  node: TTreeNode;
  dummy: TPoint;
begin
  dummy := Point(X,Y);
  node := Treeview1.GetNodeAt(X,Y);
  if not assigned(node) then exit;

  MouseButtonIsHeld := true;

  rect := node.DisplayRect(false);
  X := X - (rect.Right - GetPropertyAreaWidth); // shift coordinates to PropertyArea box
  Y := Y -  rect.Top;

  if X >= 0 then
  begin
    PropertyAreaClick(X, Y, node);
    MouseOutsidePropertyRegion := false;
  end
  else
    MouseOutsidePropertyRegion := true;

  if assigned(OnTreeMouseDown) then
    OnTreeMouseDown(Sender, Button, Shift, dummy.X, dummy.Y);
end;



procedure TTreeOptionList.PropertyAreaClick(X, Y: integer; Node: TTreeNode);
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
      Node.StateIndex := Cardinal(Node.StateIndex) XOR PropertyMask; // toggle property
      FlagsChangedByMouseAction := true;
      if Assigned(OnNodeFlagsChanged) then OnNodeFlagsChanged(Node);

      // keep the current action for copy&paste to other elements
      PropertyMaskAtMouseDown  := PropertyMask;
      PropertyStateAtMouseDown := (Node.StateIndex and PropertyMask) = PropertyMask;
    end; // if PropertyIndex within range

  end; // if Stateindex > -1

end;



function TTreeOptionList.GetPropertyAreaWidth: integer;
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



procedure TTreeOptionList.TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseButtonIsHeld := false;

  if assigned(OnTreeMouseUp) then
    OnTreeMouseUp(Sender,Button,Shift,X,Y);

  FlagsChangedByMouseAction := false;
end;



procedure TTreeOptionList.TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  rect: TRect;
  node: TTreeNode;
  OldNodeFlags: longint;
begin
  if (not MouseButtonIsHeld) or MouseOutsidePropertyRegion then
  begin
    if assigned(OnTreeMouseMove) then
      OnTreeMouseMove(Sender,Shift,X,Y);
    exit;
  end;

  node := Treeview1.GetNodeAt(X,Y);
  if not assigned(node) then exit;

  if (Node.StateIndex > -1) then // and (X - (rect.Right - GetPropertyAreaWidth) > 0)
  begin
    OldNodeFlags := Node.StateIndex;

    if PropertyStateAtMouseDown = true then
      Node.StateIndex := Node.StateIndex or PropertyMaskAtMouseDown
    else
      Node.StateIndex := Node.StateIndex and (not PropertyMaskAtMouseDown);

    if Node.StateIndex <> OldNodeFlags then // flags have indeed been changed. Notify.
      if Assigned(OnNodeFlagsChanged) then OnNodeFlagsChanged(Node);
  end;

  if assigned(OnTreeMouseMove) then
    OnTreeMouseMove(Sender,Shift,X,Y);
end;


procedure TTreeOptionList.TreeViewExpanded(Sender: TObject; Node: TTreeNode);
begin
  if ScrollbarVisibilityHasChanged then
    Tree.Repaint;
end;

procedure TTreeOptionList.TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
begin
  if ScrollbarVisibilityHasChanged then
    Tree.Repaint;
end;

function TTreeOptionList.ScrollbarVisibilityHasChanged: Boolean;
var
  ScrollbarVisible : Boolean;
begin
  ScrollbarVisible := (Tree.Width-Tree.ClientWidth-2*Tree.BorderWidth) > 8;
  result := ScrollbarVisible xor ScrollbarVisibleLastTimeChecked;
  ScrollbarVisibleLastTimeChecked := ScrollbarVisible;
end;








end.
