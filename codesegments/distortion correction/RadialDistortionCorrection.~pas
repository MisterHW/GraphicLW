unit RadialDistortionCorrection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Math, Menus, DynamicFunctions,
  GraphicLayerManager, GLMFunctionality, GLWdef, GLWFilters, ExtCtrls,
  Grids, ValEdit, InverseApproximation, treexml, VisualOverlayClass;

type
  TRadialDistortionCorrectionForm = class(TForm)
    GroupBox1: TGroupBox;
    abortbutton: TButton;
    Button1: TButton;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    CheckBox1: TCheckBox;
    showguidesopt: TCheckBox;
    resmode: TTrackBar;
    reslabel: TLabel;
    degs: TEdit;
    TrackBar1: TTrackBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    libmenu: TPopupMenu;
    paramlist: TValueListEditor;
    ftext: TMemo;
    Label2: TLabel;
    Bevel2: TBevel;
    setfunction: TButton;
    updateonmodparams: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    LensTree: TTreeView;
    lensxml: TMemo;
    Label4: TLabel;
    LabelDistortionSetting: TLabel;
    Label3: TLabel;
    LabelNextLensHint: TLabel;
    GroupBox4: TGroupBox;
    FocalLengthEdit: TEdit;
    CropFactorEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    uncropped: TRadioButton;
    inscribed: TRadioButton;
    enclosed: TRadioButton;
    Label7: TLabel;
    Panel1: TPanel;
    finalmode_bilinear: TRadioButton;
    finalmode_nn: TRadioButton;
    Savelibraryexternally1: TMenuItem;
    reloadexternallibrary1: TMenuItem;
    libmembermenu: TPopupMenu;
    edit1: TMenuItem;
    delete1: TMenuItem;
    Add1: TMenuItem;
    Assigntoparameterstab1: TMenuItem;
    Obtainfromparameterstab1: TMenuItem;
    procedure abortbuttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure resmodeChange(Sender: TObject);
    procedure showguidesoptClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1KeyPress(Sender: TObject; var Key: Char);
    procedure OnTimer(var Message:TWMTimer); Message WM_TIMER;
    procedure setfunctionClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure paramlistClick(Sender: TObject);
    procedure ftextClick(Sender: TObject);
    procedure ftextKeyPress(Sender: TObject; var Key: Char);
    procedure Label3Click(Sender: TObject);
    procedure uncroppedClick(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure paramlistKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure paramlistMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure paramlistMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure degsKeyPress(Sender: TObject; var Key: Char);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LensTreeClick(Sender: TObject);
    procedure LensTreeChange(Sender: TObject; Node: TTreeNode);
    procedure LensTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Savelibraryexternally1Click(Sender: TObject);
    procedure reloadexternallibrary1Click(Sender: TObject);
    procedure delete1Click(Sender: TObject);
  private
    VO2: TVisualOverlayProgressBar;
    inverse_function: TInverseLookupFunction;
    forward_function : TVar1SingleObjProc;
    forward_function_obj: TObject;
    designated_lensnode: TTreeNode;
    menu_related_node: TTreeNode;
    PreviewMethod: Integer;
    WorkingLayerIdx: longint;
    tbtimer, mwtimer: integer;
    tmp  : Array[1..10] of single;
    coeff: Array[1..10] of single;
    WheelDelta : shortint;
    ShowWarnings : Boolean;
    LensTreeLoadedFirstTime: Boolean;
    procedure proj(var r: single); stdcall;
    procedure WheelTimerProc(var Message:TWMTimer);
    procedure TrackbarTimerProc(var Message:TWMTimer);
    procedure UpdateProgressBar(Position: integer; Maximum: integer);
    function LoadLensFromLibrary(TN: TTreeNode):Boolean;

    procedure SelectLensFromLibrary(Sender: TObject);
    procedure LibCreateEditDeleteNodeMenu(Sender:TObject);
    procedure LibAssignMenuEvents;
  public
    RemoteGLM:TGraphicLayerManager;
    OnToolAbort : TNotifyEvent;
    OnUpdateCurrentLayer : LongintProc;
    ToolStatus: integer;
    WorkingLayerID: longint;
    MaskLayerID: longint;
    procedure ResetTool;
    procedure PreviewFilterProc(LayerID: integer; target: TBitmap; rct: TRect);
    Procedure LayerChanges(Sender:TObject);
  end;

var
  RadialDistortionCorrectionForm: TRadialDistortionCorrectionForm;

implementation
const
  TrackbarTimer   = 1;
  MouseWheelTimer = 2;

const
  SOURCE_METHOD = 0;
  TARGET_METHOD = 1;

{$R *.dfm}




procedure TRadialDistortionCorrectionForm.OnTimer(var Message:TWMTimer);
begin
  case Message.TimerID of
    TrackbarTimer   : TrackbarTimerProc(Message);
    MouseWheelTimer : WheelTimerProc(Message);
  end;
end;




procedure TRadialDistortionCorrectionForm.ResetTool;
begin
end;



procedure TRadialDistortionCorrectionForm.abortbuttonClick(Sender: TObject);
begin
  if assigned(self.inverse_function) then
  self.inverse_function.Free;
  self.inverse_function := nil;
  RemoteGLM.OnLayerChanges.RemoveEvent(self.LayerChanges); // no clientpaint call here
  RemoteGLM.RemovePreviewHook(self.PreviewFilterProc); // no clientpaint call here

  WorkingLayerIdx := RemoteGLM.LayerByID(WorkingLayerID); // no clientpaint call here
  if WorkingLayerIdx <> -1 then
    RemoteGLM.Layers[WorkingLayerIdx].suppressed := false;
  ToolStatus := 0;

  // RemoteGLM.surpressrepaint;
  if self.Visible then hide;
  //RemoteGLM.surpressrepaint;
  if assigned(OnToolAbort) then OnToolAbort(self); // ClientPaint!
  //RemoteGLM.ClientPaint(self);
end;



procedure TRadialDistortionCorrectionForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  abortbuttonclick(self);
end;



procedure TRadialDistortionCorrectionForm.CheckBox1Click(Sender: TObject);
begin
  //resmode.Enabled := CheckBox1.Checked;
  WorkingLayerIdx := RemoteGLM.LayerByID(WorkingLayerID);
  if WorkingLayerIdx <> -1 then
    RemoteGLM.Layers[WorkingLayerIdx].suppressed := CheckBox1.Checked;

  case CheckBox1.Checked of
    true:RemoteGLM.InsertPreviewHook(self.PreviewFilterProc);
    false:RemoteGLM.RemovePreviewHook(self.PreviewFilterProc);
  end;

  RemoteGLM.ClientPaint(self);
end;



procedure TRadialDistortionCorrectionForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // do not steal the wheel event ;)
  if windows.WindowFromPoint(MousePos) = application.MainForm.Handle then
  begin
    application.MainForm.OnMouseWheel(sender,Shift,WheelDelta,MousePos,Handled);
  end;
    Handled := true; // don't let the trackbar get strange mousewheel events...
end;



procedure TRadialDistortionCorrectionForm.FormCreate(Sender: TObject);
var
  i: integer;
//  xmlfn: string;
//  NodeStruct: PNodeStruct;
begin
  for i := 2 to 10 do coeff[i] := 0;
  coeff[1] := 1;
  self.OnMouseWheel := FormMouseWheel;

//  xmlfn := ExtractFilePath(application.ExeName)+'\'+COMMON_DATA_PATH+'\lensdb.xml';
//  if fileexists(xmlfn) then self.lensxml.Lines.LoadFromFile(xmlfn);
//  treexml.CreateTreeFromXMLStrings(LensTree,lensxml);

// for i := 0 to  LensTree.Items.Count-1 do
// begin
//   NodeStruct := LensTree.Items.Item[i].Data;
//   if not assigned(LensTree.Items.Item[i].Parent) and Assigned(NodeStruct) then
//     NodeStruct^.events[NODESTRUCT_ONCLICK_EVENT] := self.SelectLensFromLibrary;
// end; // for i

  LensTreeLoadedFirstTime := false; // save some start-up time

//  PageControl1.TabIndex := 1;
  designated_lensnode := nil;
  ShowWarnings := true;
end;



procedure TRadialDistortionCorrectionForm.SelectLensFromLibrary(Sender: TObject); // sender is TTreeNode
var
  node : TTreeNode absolute Sender;
  data : PNodeStruct;
  s : string;
begin
  setfunction.Enabled := true;
  data := node.Data;
  if not assigned(data) then exit;
  s := 'click set to select  " ' + PNodeStruct(node.Data)^.nameattr+ ' "';
  LabelNextLensHint.Caption := s;
  designated_lensnode := node;
end;





{procedure f(dummy: Pointer; var s: single); stdcall;
begin
  s := s+ 0.01*s*s + 0.02*s*s*s + 0.1*s*s*s*s;
end; }
procedure TRadialDistortionCorrectionForm.PreviewFilterProc(LayerID: integer; target: TBitmap; rct: TRect);
var
  p,q: TPoint;
  i: integer;
  x, x2: Pointer;

begin
  WorkingLayerIdx := RemoteGLM.LayerByID(WorkingLayerID);
  if WorkingLayerIdx <> -1 then
    if LayerID = WorkingLayerID then
    begin

    x := Addr(TRadialDistortionCorrectionForm.proj); // perform a little hat trick to get around
                                                     // the procedure of object call differences
                                                     // @f, nil,  // <--> procedure f(dummy: Pointer; var s: single); stdcall;
                                                     // Addr(TType.proc),TType(self), // <-->  procedure(var s: single) of object; stdcall
    x2 := Addr(TInverseLookupFunction.inverse);


    GLMFunctionality.RadialDistortionCorrectionFilter(
      target,                                             // target
      RemoteGLM.Layers[WorkingLayerIdx].data,             // source
      x,x2,                                               // procedure to call (of object)
      self, self.inverse_function,
      self.resmode.Position,                              // preview rendering mode
      StrToFloatDef(self.degs.Text,0)*(2*pi/360),         // free rotation
      RemoteGLM.GlobalCoordToScreen(RemoteGLM.Layers[WorkingLayerIdx].Position), // coordinates offset
      RemoteGLM.ZoomFactor);                              // glm screen scaling

    if self.showguidesopt.Checked then
    begin
      p := Point(0,0);
      q := Point(target.width, target.Height);

      target.Canvas.brush.Style := bssolid;

      target.Canvas.brush.Color := $C0C0C0;
      target.Canvas.frameRect(Rect(p.x,p.y,q.X,q.Y));

      target.Canvas.brush.Color := $303030;
      target.Canvas.frameRect(Rect(p.x-1,p.y-1,q.X+1,q.Y+1));

      target.Canvas.pen.Color := $303030;
      for i := - (q.X - p.X) div (32*2) to (q.X - p.X) div (32*2) do
      begin
        target.Canvas.MoveTo(1+p.X + i * 32 + (q.X - p.X) div 2 ,1+ p.Y);
        target.Canvas.LineTo(1+p.X + i * 32 + (q.X - p.X) div 2 ,1+ q.Y);
      end;
      for i := - (q.Y - p.Y) div (32*2) to (q.Y - p.Y) div (32*2) do
      begin
        target.Canvas.MoveTo(1+p.X, 1+ p.Y + i * 32 + (q.Y - p.Y) div 2);
        target.Canvas.LineTo(1+q.X, 1+ p.Y + i * 32 + (q.Y - p.Y) div 2);
      end;

      target.Canvas.pen.Color := $C0C0C0;
      for i := - (q.X - p.X) div (32*2) to (q.X - p.X) div (32*2) do
      begin
        target.Canvas.MoveTo(p.X + i * 32 + (q.X - p.X) div 2 , p.Y);
        target.Canvas.LineTo(p.X + i * 32 + (q.X - p.X) div 2 , q.Y);
      end;
      for i := - (q.Y - p.Y) div (32*2) to (q.Y - p.Y) div (32*2) do
      begin
        target.Canvas.MoveTo(p.X, p.Y + i * 32 + (q.Y - p.Y) div 2);
        target.Canvas.LineTo(q.X, p.Y + i * 32 + (q.Y - p.Y) div 2);
      end;

    end;

    p := RemoteGLM.GlobalCoordToScreen(RemoteGLM.Layers[WorkingLayerIdx].Position);
    q := Point( p.x+round(RemoteGLM.ZoomFactor*RemoteGLM.Layers[WorkingLayerIdx].data.Width ),
                p.y+round(RemoteGLM.ZoomFactor*RemoteGLM.Layers[WorkingLayerIdx].data.Height));
    target.Canvas.Brush.Color := clblue;
    target.Canvas.FrameRect(Rect(p,q));

  end;
end;



procedure TRadialDistortionCorrectionForm.FormShow(Sender: TObject);
var
  i: integer;
  xmlfn: string;
  NodeStruct: PNodeStruct;
begin

  if not LensTreeLoadedFirstTime then
  begin
    xmlfn := ExtractFilePath(application.ExeName)+'\'+COMMON_DATA_PATH+'\lensdb.xml';
    if fileexists(xmlfn) then self.lensxml.Lines.LoadFromFile(xmlfn);
    treexml.CreateTreeFromXMLStrings(LensTree,lensxml);
    LibAssignMenuEvents; // better
    LensTreeLoadedFirstTime := true;
  end;

  RemoteGLM.OnLayerChanges.InsertEvent(self.LayerChanges);
  RemoteGLM.surpressrepaint;
  self.ShowWarnings := false;
  self.setfunction.Click;
  self.ShowWarnings := true;
  if not checkbox1.Checked then RemoteGLM.surpressrepaint;
  CheckBox1Click(self);
end;



procedure TRadialDistortionCorrectionForm.resmodeChange(Sender: TObject);
begin
  case resmode.Position of
   0: reslabel.Caption := 'coarse';
   1: reslabel.Caption := 'medium';
   2: reslabel.Caption := 'fine';
   3: reslabel.Caption := 'exact';
  end;
  RemoteGLM.ClientPaint(self);
end;



procedure TRadialDistortionCorrectionForm.showguidesoptClick(
  Sender: TObject);
begin
  CheckBox1Click(self);
end;



Procedure TRadialDistortionCorrectionForm.LayerChanges(Sender:TObject);
var
  WorkingLayerIdx : integer;
begin
  WorkingLayerIdx := RemoteGLM.LayerByID(self.WorkingLayerID);

  if WorkingLayerIdx  = -1 then
    self.abortbuttonClick(self)
  else
    self.ResetTool;
end;



procedure TRadialDistortionCorrectionForm.ListBox1DblClick(
  Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  libmenu.Popup(p.x,p.y);
end;



procedure TRadialDistortionCorrectionForm.TrackBar1Change(Sender: TObject);
begin
//   KillTimer(self.Handle,TrackbarTimer); tbtimer := 0;
  if tbtimer = 0 then  // makes a better feeling of speed
    tbtimer := SetTimer(self.Handle,TrackbarTimer,50,nil);

  trackbar1.Selstart := min(0, trackbar1.Position);
  trackbar1.SelEnd   := max(0, trackbar1.Position);
  if degs.Focused = false then
  degs.Text := floattostr(trackbar1.Position / 10) ;
end;



procedure TRadialDistortionCorrectionForm.TrackbarTimerProc(var Message:TWMTimer);
begin
  windows.KillTimer(self.Handle,Message.TimerID);
  tbtimer := 0;
  RemoteGLM.ClientPaint(self);
end;



procedure TRadialDistortionCorrectionForm.degsKeyPress(Sender: TObject;
  var Key: Char);
begin
  Trackbar1.Position := strtointdef(degs.Text,0)*10;
end;





procedure TRadialDistortionCorrectionForm.TrackBar1KeyPress(
  Sender: TObject; var Key: Char);
begin
  if key = '0' then trackbar1.Position := 0;
end;



procedure TRadialDistortionCorrectionForm.setfunctionClick(
  Sender: TObject);
var
  i: integer;
  workinglayerindex: longint;
  w, h: integer;
  r_max, r_mono : single;
  f_mono,f_max: single;
  nodefound, lensloaded: Boolean;
begin
  if assigned(self.inverse_function) then
    inverse_function.Destroy;
  inverse_function := nil;

  self.coeff[1] := 1;
  for i := 2 to paramlist.RowCount-paramlist.TopRow do
    self.coeff[i] := 0;

  if pagecontrol1.TabIndex = 1 then // coefficients
  begin
    forward_function     := Addr(TRadialDistortionCorrectionForm.proj);
    forward_function_obj := self;

    self.coeff[1] := max(0.1,min(10,StrToFloatDef(paramlist.Cells[1,1],1)));
    for i := 2 to paramlist.RowCount-paramlist.TopRow do
      self.coeff[i] := max(-10,min(10,StrToFloatDef(paramlist.Cells[1,i],0)));

    workinglayerindex := RemoteGLM.LayerByID(self.WorkingLayerID);
    if workinglayerindex = - 1 then exit;
    w:= RemoteGLM.Layers[WorkingLayerIndex].data.Width;
    h:= RemoteGLM.Layers[WorkingLayerIndex].data.Height;

    r_max := sqrt( 1 + sqr(max(w/h, h/w)));
    r_mono := MonotonieRadius(0,r_max*2.5,forward_function,forward_function_obj);
    f_mono := r_mono;  forward_function(forward_function_obj, f_mono);
    f_max := r_max; forward_function(forward_function_obj, f_max);
    if r_mono > r_max  then PreviewMethod := SOURCE_METHOD
                       else PreviewMethod := TARGET_METHOD;

    if PreviewMethod = SOURCE_METHOD then
      inverse_function := TInverseLookupFunction.Create(
          0,
          f_max*0.1 + f_mono*0.9,
          0.5 * 1 / sqrt(sqr(w)+sqr(h)),
          forward_function,
          forward_function_obj,
          0);

    if PreviewMethod = SOURCE_METHOD
      then Self.LabelDistortionSetting.Caption := 'parameters (src rendering)'
      else Self.LabelDistortionSetting.Caption := 'parameters (target rendering)';
  end;

  if PageControl1.TabIndex = 0 then // library
  begin
    self.LabelNextLensHint.Caption := '';
    nodefound  := false;
    lensloaded := false;

    for i := 0 to LensTree.Items.Count - 1 do
      if LensTree.Items.Item[i] = designated_lensnode then NodeFound := true;

    if not NodeFound then designated_lensnode := nil;

    if NodeFound then lensloaded := self.LoadLensFromLibrary(designated_lensnode);

    if (not NodeFound) or (not LensLoaded) then
    begin
      // setup identity transformation
      self.coeff[1] := 1;
      for i := 2 to 10 do self.coeff[i] := 0;
      forward_function     := Addr(TRadialDistortionCorrectionForm.proj);
      forward_function_obj := self;

      if NodeFound then
      begin
        Application.MessageBox('Failed to load correction scheme.',
          'Error(s) loading lens from library',MB_ICONWARNING or MB_OK);
        designated_lensnode := nil; // don't let it happen again
        NodeFound := False;
      end;
    end;

    workinglayerindex := RemoteGLM.LayerByID(self.WorkingLayerID);
    if workinglayerindex = - 1 then exit;
    w:= RemoteGLM.Layers[WorkingLayerIndex].data.Width;
    h:= RemoteGLM.Layers[WorkingLayerIndex].data.Height;

    r_max := sqrt( 1 + sqr(max(w/h, h/w)));
    r_mono := MonotonieRadius(0,r_max*2.5,forward_function,forward_function_obj);
    f_mono := r_mono;  forward_function(forward_function_obj, f_mono);
    f_max := r_max; forward_function(forward_function_obj, f_max);
    if r_mono > r_max  then PreviewMethod := SOURCE_METHOD
                       else PreviewMethod := TARGET_METHOD;

    if PreviewMethod = SOURCE_METHOD then
      inverse_function := TInverseLookupFunction.Create(
          0,
          f_max*0.1 + f_mono*0.9,
          0.5 * 1 / sqrt(sqr(w)+sqr(h)),
          forward_function,
          forward_function_obj,
          0);
    if NodeFound and LensLoaded then
      self.LabelDistortionSetting.Caption := (PNodeStruct(designated_lensnode.Data)^.nameattr)
    else
      self.LabelDistortionSetting.Caption := ' - no setting loaded - ';
  end;

  Setfunction.Enabled := false;
  remoteGLM.ClientPaint(self);
end;



procedure TRadialDistortionCorrectionForm.PageControl1Change(
  Sender: TObject);
begin
  if pagecontrol1.ActivePageIndex = 2 then
    pagecontrol1.ActivePageIndex := 1;

  if pagecontrol1.ActivePageIndex <>0 then
  Setfunction.Enabled := true;
end;



procedure TRadialDistortionCorrectionForm.paramlistClick(Sender: TObject);
begin
  Setfunction.Enabled := true;
end;



procedure TRadialDistortionCorrectionForm.ftextClick(Sender: TObject);
begin
  windows.SetFocus(ftext.Handle);
end;



procedure TRadialDistortionCorrectionForm.ftextKeyPress(Sender: TObject;
  var Key: Char);
begin
  Setfunction.Enabled := true;
end;



procedure TRadialDistortionCorrectionForm.Label3Click(Sender: TObject);
begin
  Application.MessageBox('use it.', 'Information on distortion correction',MB_OK or MB_ICONASTERISK);
end;



procedure TRadialDistortionCorrectionForm.uncroppedClick(
  Sender: TObject);
begin
  if checkbox1.Checked then RemoteGLM.ClientPaint(self);
end;




procedure TRadialDistortionCorrectionForm.EditClick(Sender: TObject);
begin
  windows.SetFocus((Sender as TWinControl).Handle);
end;



procedure TRadialDistortionCorrectionForm.proj(var r: single); stdcall;
begin
   tmp[2] := r*r;
   tmp[3] := r * tmp[2];
   tmp[4] := tmp[2] * tmp[2];
   tmp[5] := r * tmp[4];
   tmp[6] := tmp[2]* tmp[4];

   r := coeff[1] * r +
        coeff[2] * tmp[2] +
        coeff[3] * tmp[3] +
        coeff[4] * tmp[4] +
        coeff[5] * tmp[5] +
        coeff[6] * tmp[6];
end;



procedure TRadialDistortionCorrectionForm.paramlistKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if updateonmodparams.Checked then self.Setfunction.Click;
end;




////////////////////////

// forward mousewheel events if curser is outside the form
// accumulate wheel deltas and process in timer procedure above
procedure TRadialDistortionCorrectionForm.paramlistMouseWheelDown(
  Sender: TObject; Shift: TShiftState; MousePos: TPoint;
var
  Handled: Boolean);
begin
  if (windows.WindowFromPoint(MousePos)= application.MainForm.Handle) then
  begin
    KillTimer(self.Handle,MouseWheelTimer); mwtimer := 0;
    mwtimer := SetTimer(self.Handle,MouseWheelTimer,10,nil);
    dec(self.WheelDelta);
    Handled := true;
  end;
end;



procedure TRadialDistortionCorrectionForm.paramlistMouseWheelUp(
  Sender: TObject; Shift: TShiftState; MousePos: TPoint;
var
  Handled: Boolean);
begin
  if (windows.WindowFromPoint(MousePos)= application.MainForm.Handle) then
  begin
    KillTimer(self.Handle,MouseWheelTimer); mwtimer := 0;
    mwtimer := SetTimer(self.Handle,MouseWheelTimer,10,nil);
    inc(self.WheelDelta);
    Handled := true;
  end; 
end;



procedure TRadialDistortionCorrectionForm.WheelTimerProc(var Message:TWMTimer);
var
  handled: Boolean;
  MousePos: TPoint;
begin
  windows.KillTimer(self.Handle,Message.TimerID);
  mwtimer := 0;
  windows.GetCursorPos(MousePos);
  application.MainForm.OnMouseWheel(self,[],windows.WHEEL_DELTA*self.WheelDelta,MousePos,Handled);
  self.WheelDelta := 0;
end;

////////////////////////




procedure TRadialDistortionCorrectionForm.Button2Click(Sender: TObject);
begin
  if application.MessageBox('The transformed image can now be zoomed to fit the'#13#10' existing image size to aviod empty spaces. Do you want to do this?','zooming - all coefficients will be altered',mb_YESNO or MB_ICONASTERISK) = IDYES
  then ;
end;



procedure TRadialDistortionCorrectionForm.Button3Click(Sender: TObject);
var
  i: integer;
  factor: single;
begin
  if application.MessageBox('The coefficients will be scaled to get a= 1.0 (useful for panotools correction model)'#13#10' Do you want to do this?','zooming - all coefficients will be altered',mb_YESNO or MB_ICONASTERISK) = IDYES
  then begin
    factor := 1/ max(0.1,min(10,StrToFloatDef(paramlist.Cells[1,1],1)));
    paramlist.Cells[1,1] := '1';
    for i := paramlist.TopRow+1 to paramlist.RowCount-paramlist.TopRow do
      paramlist.Cells[1,i] := FloatToStr(
        power(factor,i)* max(-10,min(10,StrToFloatDef(paramlist.Cells[1,i],0)))
        );
    //applyfunction.Enabled := true;
    self.SetfunctionClick(self);
  end;
end;



procedure TRadialDistortionCorrectionForm.Button1Click(Sender: TObject);
var
  RenderLayeridx, WorkingLayerIdx: integer;
//  RenderLayerID: longint;
  w,h : integer;
  textwidth, textheight: integer;
  rmode: Cardinal;
begin

  // render here

    WorkingLayerIdx := remoteglm.LayerByID(WorkingLayerID);

    w := RemoteGLM.Layers[WorkingLayerIdx].data.width;
    h := RemoteGLM.Layers[WorkingLayerIdx].data.height;

    remoteGLM.surpressrepaint;
    RenderLayeridx := remoteglm.NewLayer(rect(0,0,w,h ),clblack);
    // RenderLayerID  := remoteGLM.layers[RenderLayeridx].LayerID;
    remoteGLM.Layers[RenderLayerIdx].LayerCaption := remoteGLM.Layers[WorkingLayerIdx].LayerCaption + '_dist_corr';

    {with remoteGLM.Layers[RenderLayerIdx].data do
    begin
     Canvas.Font.Size := 36;
     Canvas.Font.Color := cllime;
     textwidth := Canvas.TextWidth(' processing ... ');
     textHeight := Canvas.TextHeight(' processing ... ');
     Canvas.TextOut((w-textwidth) div 2,(h-textheight) div 2,' processing ... ');
    end; }

    self.ToolStatus := 0;
    self.Hide;
    remoteGLM.ClientPaint(self);

    if finalmode_nn.Checked
      then rmode := RADC_RESOLUTION_EXACT
      else rmode := RADC_RESOLUTION_FINAL;


    // add some eye candy
    VO2:= TVisualOverlayProgressBar.Create(ClientRect);
    VO2.Top  := Application.MainForm.ClientHeight- VO2.Height-8;
    VO2.Left := Application.MainForm.ClientWidth - VO2.Width -8;
    VO2.Anchors := [akBottom, akRight];
    RemoteGLM.InsertVisualOverlayObject(VO2);
    VO2.max := 100;
    VO2.Caption := 'correcting distortion ...';
    VO2.Visible := true;

    GLMFunctionality.RadialDistortionCorrectionFilter(
      RemoteGLM.Layers[RenderLayerIdx].data,
      RemoteGLM.Layers[WorkingLayerIdx].data,
      Addr(TRadialDistortionCorrectionForm.proj),nil,
      self,nil,
      rmode,
      StrToFloatDef(self.degs.Text,0)*(2*pi/360),
      Point(0,0),
      1.000,
      self.UpdateProgressBar);

    RemoteGLM.surpressrepaint;
    RemoteGLM.RemoveVisualOverlayObject(VO2);
    VO2.Destroy;

  // done rendering.

  //self.ResetTool;
  // remoteGLM.surpressrepaint;
  //remoteGLM.surpressrepaint;


//  if assigned(OnUpdateCurrentLayer) then OnUpdateCurrentLayer(RenderLayerID);
  //self.Hide;
  remoteGLM.surpressrepaint;
  RemoteGLM.FocusLayer(RenderLayerIdx);

//  remoteGLM.ClientPaint(self);

  self.abortbuttonClick(self); // currentlayerID = workinglayerID wrong...
end;




procedure TRadialDistortionCorrectionForm.LensTreeClick(Sender: TObject);
begin
  Setfunction.Enabled := true;
end;



procedure TRadialDistortionCorrectionForm.LensTreeChange(Sender: TObject;
  Node: TTreeNode);
begin
  if LensTree.Enabled then
  if assigned(Node) then
  if assigned(Node.Data) then
    if assigned(PNodeStruct(Node.Data)^.events[NODESTRUCT_ONCLICK_EVENT]) then
      PNodeStruct(Node.Data)^.events[NODESTRUCT_ONCLICK_EVENT](node);
end;



procedure TRadialDistortionCorrectionForm.UpdateProgressBar(Position: integer; Maximum: integer);
begin
  if assigned(VO2) then VO2.position := 100 * Position / (Maximum+0.00001);
end;



function TRadialDistortionCorrectionForm.LoadLensFromLibrary(TN: TTreeNode): Boolean;
var
  errors: string;
  buf : string;
  focaltype: string;
//  len, crop: single;
  minlength, maxlength, userlength, usercrop: single;
  geometricsets: TTreeNode;
  selectedgeomset, firstgeomset, bestfitgeomset: TTreeNode;
  bestfitweight, newweight: single;
  geomsetcount: integer;
  i: integer;
  setmin, setmax: single;
  coefficientsnode: TTreeNode;
  x: PChar;
const
  FOCAL_TYPE_ZOOM = 'zoom';
  FOCAL_TYPE_FIX = 'fix';
  DEFAULT_FOCALLEN = 50;
  DEFAULT_CROP = 1.0;
  CONVENTION_POLYNOMIAL = 'polynomial';
  CONVENTION_FUNCTION = 'function';

  function PromptErrors : Boolean;
  begin
    result := (length(errors) <> 0);
    if result then
      Application.MessageBox(PChar(errors),'Error(s) loading lens from library',MB_ICONERROR or MB_OK);
  end;

  function IsValidGeometricSet(node: TTreeNode): Boolean;
  var
    dummy: string;
    dummynode: TTreeNode;
  begin
    result := false;

    if not RetrieveValueByPath(node,dummy,'rangestart',nil) then exit; // require rangestart [float]
    if StrToFloatDef(FixDecSep(dummy),-1) = -1 then exit;

    if not RetrieveValueByPath(node,dummy,'rangeend',nil) then exit;   // require rangeend [float]
    if StrToFloatDef(FixDecSep(dummy),-1) = -1 then exit;

    if not RetrieveValueByPath(node,dummy,'convention',nil) then exit;   // require convention [string]
    if (dummy <> CONVENTION_POLYNOMIAL) and (dummy <> CONVENTION_FUNCTION) then exit;
    if (dummy = CONVENTION_POLYNOMIAL) and (not RetrieveNodeByPath(node,dummynode,'coefficients',nil)) then exit;
    if (dummy = CONVENTION_FUNCTION) and (not RetrieveNodeByPath(node,dummynode,'function',nil)) then exit;

    if (dummy = CONVENTION_POLYNOMIAL) then
    begin
      RetrieveNodeByPath(node,dummynode,'coefficients',nil);
      if dummynode.Count = 0 then exit;  // require coefficients
    end;

    if (dummy = CONVENTION_POLYNOMIAL) then
    begin
      RetrieveValueByPath(node,dummy,'function',nil);
      if DeleteChars(dummy,' ;+-^/=()'#13#10 ) = '' then exit; // no function specified
    end;

    result := true;
  end;

  procedure WarningBestSetSelected;
  begin
    buf := '0';
    RetrieveValueByPath(selectedgeomset,buf,'rangestart',nil);
    setmin := StrToFloat(DeleteChars(buf,' '#13#10));
    RetrieveValueByPath(selectedgeomset,buf,'rangeend',nil);
    setmax := StrToFloat(DeleteChars(buf,' '#13#10));

    if self.ShowWarnings then
    if not ((setmin <= userlength) and (userlength <= setmax)) then
      Application.MessageBox(PChar('No geometric correction scheme was found for the given focal length.'#13#10+
          'A set with the highest likelyhood to fit has been selected.'#13#10#13#10+
          'focal length: '+floattostr(userlength)+'mm; set range ['+floattostr(setmin)+' .. '+floattostr(setmax)+'] mm'),
          'Warning: inconsistency found while loading from library',MB_ICONWARNING or MB_OK);
  end;

begin
  result := false;
  if not assigned(TN) then exit;
  errors := '';
  userlength := StrToFloatDef(DeleteChars(self.FocalLengthEdit.Text,' '),DEFAULT_FOCALLEN);
//  usercrop   := StrToFloatDef(FixDecSep(DeleteChars(self.CropFactorEdit.Text,' ')),DEFAULT_CROP);

////////////////////////////////////////////////
// check crop factor, obtain 'crop'

  buf := self.CropFactorEdit.Text;
  if not treexml.RetrieveValueByPath(TN,buf,'focalparams','cropfactor',nil)
    then errors := errors + '! crop factor not specified. Inserting user-defined value (critical)'#13#10
    else if DeleteChars(FixDecSep(buf),' ') <> DeleteChars(FixDecSep(self.CropFactorEdit.Text),' ') then
      errors := errors + '! crop factor for the selected lens does not match (critical)'#13#10;
//  crop := StrToFloatDef(DeleteChars(FixDecSep(buf),' '),DEFAULT_CROP); // just in case we need it again

////////////////////////////////////////////////
//  check focal length, obtain 'focaltype', 'minlength', 'maxlength'

  focaltype := FOCAL_TYPE_FIX;
  if not treexml.RetrieveValueByPath(TN,focaltype,'focalparams','lenstype',nil) then
   errors := errors + '! lenstype not specified. Assuming fix focus type'#13#10;
  focaltype := DeleteChars(lowercase(focaltype),' ');

  buf := self.FocalLengthEdit.Text;
  if not treexml.RetrieveValueByPath(TN,buf,'focalparams','minfocallength',nil) then
   errors := errors + '! minimum focal length not specified. inserting user-defined value'#13#10;
  minlength := StrToFloatDef(DeleteChars(FixDecSep(buf),' '),DEFAULT_FOCALLEN);

  buf := self.FocalLengthEdit.Text;
  if not treexml.RetrieveValueByPath(TN,buf,'focalparams','maxfocallength',nil) then
   errors := errors + '! minimum focal length not specified. inserting user-defined value'#13#10;
  maxlength := StrToFloatDef(DeleteChars(FixDecSep(buf),' '),DEFAULT_FOCALLEN);

  if (focaltype = FOCAL_TYPE_FIX) and (minlength <> maxlength) then
    errors := errors + '! for fix focus lenses, minimum and maximum length may not differ. Using minimum parameter'#13#10;

////////////////////////////////////////////////
// see through the geometric sets, find at least one set,
// if more than one set is available, find best set (nearest or precise)
// obtain 'geometricsets', 'geomsetcount', 'selectedgeomset'

  if errors <> '' then errors := errors +'resuming initialization ...'+#13#10;
  // PromptErrors; errors := ''; // we can still make it from here :-)

  if not treexml.RetrieveNodeByPath(TN,geometricsets,'correction','geometricsets',nil) then
    errors := errors + '! no geometric correction entry found (lens->correction->geometricsets missing)'#13#10;
  if PromptErrors then exit;

  geomsetcount := 0;
  firstgeomset := nil;
  for i := 0 to geometricsets.Count - 1 do
  begin
    if IsValidGeometricSet(geometricsets.Item[i]) then
    begin
      inc(geomsetcount);
      if geomsetcount = 1 then firstgeomset := geometricsets.Item[i];
    end;
  end;

  if geomsetcount = 0 then
    errors := errors + '! no geometric correction sets found '#13#10;
  if PromptErrors then exit;

  if geomsetcount = 1 then
  begin
    selectedgeomset := firstgeomset; // trivial
    WarningBestSetSelected;
  end
  else
    begin // find best match for 'userlength'
    bestfitweight := 10000000; // sqrt(distancefromintervalcenter / (range + 50))
    bestfitgeomset := nil;
    selectedgeomset := nil;

      for i := 0 to geometricsets.Count - 1 do
      begin
        if IsValidGeometricSet(geometricsets.Item[i]) then
        begin
          RetrieveValueByPath(geometricsets.Item[i],buf,'rangestart',nil);
          setmin := StrToFloat(DeleteChars(buf,' '#13#10));
          RetrieveValueByPath(geometricsets.Item[i],buf,'rangeend',nil);
          setmax := StrToFloat(DeleteChars(buf,' '#13#10));

          if (setmin <= userlength) and (userlength <= setmax) then
          begin
            selectedgeomset := geometricsets.Item[i];
            break; // found perfect match
          end;

          // set does not match, keep bestfitgeomset in mind, compare bestfitweight.
          // weight is the distance from the interval center, normalized by the interval width plus offset
          // so wider intervalls are preferred because they are assumed to be more tolerant to range exceeding
          newweight := sqrt(abs((setmin+setmax)/2 - userlength)) / (abs(setmin-setmax)+100);
          if newweight < bestfitweight then
          begin
            bestfitweight := newweight;
            bestfitgeomset := geometricsets.Item[i];
          end; // if newweight < bestfitweight
        end; // if IsValidGeometricset
      end; // for

      if selectedgeomset = nil then
        selectedgeomset := bestfitgeomset; // not-so-trivial :-)
      WarningBestSetSelected;
    end; // if /else geomsetcount <> 1

////////////////////////////////////////////////
//  initialize set

  RetrieveValueByPath(selectedgeomset,buf, 'convention', nil);
  if buf = CONVENTION_POLYNOMIAL then
  begin
    RetrieveNodeByPath(selectedgeomset,coefficientsnode, 'coefficients', nil);
    self.coeff[1] := 1;
    for i := 2 to 10 do self.coeff[i] := 0;
    for i := 1 to 10 do
    begin
      x:= PChar( chr(ord('a')+(i-1)));
      if RetrieveValueByPath(selectedgeomset,buf,'coefficients',@x,nil) // cycle for a, b, c, d, e, ...
        then coeff[i] := StrToFloatDef(FixDecSep(DeleteChars(buf,' '#13#10)), coeff[i]);
    end;

    forward_function     := Addr(TRadialDistortionCorrectionForm.proj);
    forward_function_obj := self;
    result := true;
  end
  else
  if buf = CONVENTION_FUNCTION then
  begin // CONVENTION_FUNCTION
    RetrieveValueByPath(selectedgeomset,buf,'function',nil);

    // MathParser := DynamicFunctions.TMathParser.Create(buf {math function});
    // x86Code := DynamicFunctions.Tx86Code.Create();
    // x86Code.InsertFunctionReference('sin()',Addr(...));
    // x86Code.Translate(MathParser.AssemblerCode);
    // forward_function     := Addr(TRadialDistortionCorrectionForm.proj);
    // forward_function_obj := x86Code;
    // MathParser.Destroy;

    errors := errors + 'Sorry, the math parser and dynamic x86 code generator are not yet implemented.';
    // result := true;
  end
  else
    errors := errors + 'Unknown convention.';


////////////////////////////////////////////////
//  show errors, if there are any

  if PromptErrors then exit;

end;



procedure TRadialDistortionCorrectionForm.LensTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  mousepos: TPoint;
  xmlfn: string;
  libext: Boolean;
  r: TRect;
  NodeAtCursor : TTreeNode;
begin
  //if mbRight = Button then
  begin
    xmlfn := ExtractFilePath(application.ExeName)+'\'+COMMON_DATA_PATH+'\lensdb.xml';
    libext := fileexists(xmlfn);
    Savelibraryexternally1.Enabled := true;
    self.reloadexternallibrary1.Enabled := libext;

    Windows.GetCursorPos(MousePos);
    NodeAtCursor :=lenstree.GetNodeAt(x,y);
    if (NodeAtCursor = nil) and(mbRight = Button) then
      self.libmenu.Popup(mousepos.X, mousepos.Y)
    else
    begin
      r := NodeAtCursor.DisplayRect(true);
      if not IsPointInRect(r,x,y) then
      begin
        if (mbRight = Button) then self.libmenu.Popup(mousepos.X, mousepos.Y);
      end
      else
        if assigned(NodeAtCursor.Data) then
        begin
          if assigned(PNodeStruct(NodeAtCursor.Data)^.events[NODESTRUCT_ONCLICK_EVENT]) then
            PNodeStruct(NodeAtCursor.Data)^.events[NODESTRUCT_ONCLICK_EVENT](NodeAtCursor);

          if Button = mbRight then
          if assigned(PNodeStruct(NodeAtCursor.Data)^.events[NODESTRUCT_ONRIGHTCLICK_EVENT]) then
            PNodeStruct(NodeAtCursor.Data)^.events[NODESTRUCT_ONRIGHTCLICK_EVENT](NodeAtCursor);

          if Button = mbLeft then
          if assigned(PNodeStruct(NodeAtCursor.Data)^.events[NODESTRUCT_ONLEFTCLICK_EVENT]) then
            PNodeStruct(NodeAtCursor.Data)^.events[NODESTRUCT_ONLEFTCLICK_EVENT](NodeAtCursor);
        end;
    end;
  end;
end;



procedure TRadialDistortionCorrectionForm.Savelibraryexternally1Click(
  Sender: TObject);
var
  xmlfn: string;
  xmlpath : string;
  function FindFreeAlternativeFN(path, filen, ext: string): string;
  var i: integer;
  begin
    i := 1;
    result := path + filen + '(' + inttostr(i)+').'+ext;
    while FileExists(result) do
    begin
      inc(i);
      result := path + filen + '(' + inttostr(i)+').'+ext;
    end;
  end;
begin
    xmlpath := ExtractFilePath(application.ExeName)+'\'+COMMON_DATA_PATH;
    xmlfn := xmlpath+'\lensdb.xml';
    if not DirectoryExists(xmlpath) then MkDir(xmlpath);
    if FileExists(xmlfn) then
    begin
      RenameFile(xmlfn, FindFreeAlternativeFN(xmlpath+'\','lensdb','xml'));

      TreeXML.CreateXMLFromTree(lenstree,lensxml);
      self.lensxml.Lines.SaveToFile(xmlfn);
      Application.MessageBox(PChar(
        'An existing library file has been renamed so you can undo changes manually.'#13#10+
        'The current lens database has been saved to : '#13#10#13#10+ xmlfn),
        'library export',mb_IconInformation or MB_OK);
    end
    else
    begin
    TreeXML.CreateXMLFromTree(lenstree,lensxml);
    self.lensxml.Lines.SaveToFile(xmlfn);

    Application.MessageBox(PChar(
        'Exporting the library enables you to modify it - '#13#10+
        'yet it can be deleted in case you would like to restore the internal library.'#13#10+
        'The lens database has been saved to : '#13#10#13#10+ xmlfn),
        'library export',mb_IconInformation or MB_OK);
    end;
end;



procedure TRadialDistortionCorrectionForm.reloadexternallibrary1Click(
  Sender: TObject);
var
  i : integer;
  pns: PNodeStruct;
  xmlfn: string;
  NodeStruct : PNodeStruct;
begin

  if Windows.MessageBox(self.Handle,
        'Loading an external library file will clear all information'#13#10+
        'currently visible in the "use library" tab. You should only accept this'#13#10+
        'if you didn''t modify the library or if you would like to undo changes.',
        'Overwrite Warning : Loss of current database imminent',
        MB_OKCANCEL or MB_ICONWARNING) = IDOK then
  begin

   LensTree.Enabled := false;
   // LensTree.Visible := false; // saves only 50ms but otherwise the user is begin entertained.

  // remove focus
    LabelNextLensHint.Caption :=  'no lens selected ...';
    designated_lensnode := nil;
    self.setfunction.Enabled := false;

  // dispose current library
    for i:= Lenstree.Items.Count-1 downto 0 do
    begin
     pns := Lenstree.Items.Item[i].Data;
     TreeXML.DisposeNodeStruct(pns);
     //Lenstree.Items.Item[i].Destroy;
     end;
     LensTree.Items.Clear;

    // load library text
    xmlfn := ExtractFilePath(application.ExeName)+'\'+COMMON_DATA_PATH+'\lensdb.xml';
    self.lensxml.Lines.LoadFromFile(xmlfn);

    // translate text into tree structure
    treexml.CreateTreeFromXMLStrings(LensTree,lensxml);

    self.LibAssignMenuEvents;

    LensTree.Enabled := true;
    // LensTree.Visible := TRUE;
  end; // if IDOK
end;



procedure TRadialDistortionCorrectionForm.LibAssignMenuEvents;
var
  i : integer;
  pns: PNodeStruct;
  xmlfn: string;
  NodeStruct : PNodeStruct;
begin
    for i := 0 to  LensTree.Items.Count-1 do
     begin
       NodeStruct := LensTree.Items.Item[i].Data;
       if not assigned(LensTree.Items.Item[i].Parent) and Assigned(NodeStruct) then
       NodeStruct^.events[NODESTRUCT_ONLEFTCLICK_EVENT] := self.SelectLensFromLibrary;
       if Assigned(NodeStruct) then
       begin
         if (lowercase(NodeStruct^.localname) = 'entry') or // delete
            (lowercase(NodeStruct^.localname) = 'geometricsets') or // add
            (lowercase(NodeStruct^.localname) = 'geometricset') or // edit, delete
            (lowercase(NodeStruct^.localname) = 'focalparams') or // edit
            (lowercase(NodeStruct^.localname) = 'coefficients') then // assign to parameters tab, get from parameters tab
           NodeStruct^.events[NODESTRUCT_ONRIGHTCLICK_EVENT] := self.LibCreateEditDeleteNodeMenu;
       end;
     end; // for i
end;




procedure TRadialDistortionCorrectionForm.LibCreateEditDeleteNodeMenu(Sender:TObject);
type
  TModifyRight = (mrAdd, mrEdit, mrDelete, mrAssign, mrObtain);
  TModifyRights = Set of TModifyRight;
var
  Node: TTreeNode absolute Sender;
  MousePos: TPoint;
  pns: PNodeStruct;
  NodeRights : TModifyRights;
begin
    if not (Sender is TTreeNode) then exit;
    if not assigned(Node.Data) then exit;
    pns := Node.Data;

    if pns^.localname = 'geometricsets' then NodeRights := [mrAdd];
    if pns^.localname = 'geometricset'  then NodeRights := [mrEdit, mrDelete];
    if pns^.localname = 'focalparams'   then NodeRights := [mrEdit];
    if pns^.localname = 'coefficients'  then NodeRights := [mrAssign, mrObtain];
    if pns^.localname = 'entry'         then NodeRights := [mrDelete];

    Add1.Visible                     := (mrAdd    in NodeRights);
    edit1.Visible                    := (mrEdit   in Noderights);
    delete1.Visible                  := (mrDelete in NodeRights);
    Assigntoparameterstab1.Visible   := (mrAssign in NodeRights);
    Obtainfromparameterstab1.Visible := (mrObtain in NodeRights);

    Windows.GetCursorPos(MousePos);
    menu_related_node := node; // always assign this variable before pop-up of libmembermenu
    self.libmembermenu.Popup(mousepos.X, mousepos.Y);
end;

procedure TRadialDistortionCorrectionForm.delete1Click(Sender: TObject);
begin
  // delete nodestructs
  DeleteChildStructs(menu_related_node);
  _DisposeNodeStruct(PNodeStruct(menu_related_node.Data));
  menu_related_node.Data := nil;
  // delete node recursively(!)
  menu_related_node.DeleteChildren;
  menu_related_node.Delete;
end;

end.
