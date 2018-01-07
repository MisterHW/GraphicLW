unit panotrans;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, math, ComCtrls, GraphicLayerManager,GLWdef, sphericaltransforms,
  VisualOverlayClass, GLMFunctionality, PanotransformClasses;

type
  Tpanotransform = class(TForm)
    GroupBox1: TGroupBox;
    thetatext: TEdit;
    thetatracker: TTrackBar;
    Label1: TLabel;
    phitracker: TTrackBar;
    phitext: TEdit;
    Label2: TLabel;
    psitracker: TTrackBar;
    psitext: TEdit;
    Label3: TLabel;
    GroupBox2: TGroupBox;
    ApplyToolButton: TButton;
    abortbutton: TButton;
    GroupBox3: TGroupBox;
    AlphaHText: TEdit;
    AlphaVText: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    setrangebutton: TButton;
    GroupBox4: TGroupBox;
    pinchtext: TEdit;
    Label6: TLabel;
    pinchtracker: TTrackBar;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox1: TCheckBox;
    procedure thetatrackerChange(Sender: TObject);
    procedure OnTimer(var Message:TWMTimer); Message WM_TIMER;
    procedure abortbuttonClick(Sender: TObject);
    procedure ApplyToolButtonClick(Sender: TObject);
    procedure thetatrackerKeyPress(Sender: TObject; var Key: Char);
    procedure phitrackerKeyPress(Sender: TObject; var Key: Char);
    procedure psitrackerKeyPress(Sender: TObject; var Key: Char);
    procedure thetatextClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AlphaHTextChange(Sender: TObject);
    procedure setrangebuttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure thetatextChange(Sender: TObject);
    procedure phitextChange(Sender: TObject);
    procedure psitextChange(Sender: TObject);
    procedure pinchtrackerKeyPress(Sender: TObject; var Key: Char);
    procedure pinchtextChange(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
  private
    SLE: TSourceLookupEngine;        // source lookup engine
    DLT : TSphericalLookupTransform; // destination lookup transform
    tbtimer : integer;
    isrendering: boolean;
    VO2: TVisualOverlayProgressBar;
    procedure TrackbarTimerProc(var Message:TWMTimer);
    procedure LayerChanges(Sender: TObject);
    procedure ReRenderPreview(Sender:TObject);
    procedure PreviewFilterProc(LayerID: integer; target: TBitmap; rct: TRect);
    procedure UpdateProgressBar(Position: integer; Maximum: integer);
  public
    RemoteGLM:TGraphicLayerManager;
    OnToolAbort : TNotifyEvent;
    OnUpdateCurrentLayer : LongintProc;
    ToolStatus: integer;
    WorkingLayerID: longint;
    MaskLayerID: longint;
    PreviewLayerID: longint;
    RenderLayerID: longint;
    property CannotTerminateMainApp : Boolean read isrendering;
  end;

var
  panotransform: Tpanotransform;

implementation
const
  TrackbarTimer   = 1;

{$R *.dfm}

procedure Tpanotransform.OnTimer(var Message:TWMTimer);
begin
  case Message.TimerID of
    TrackbarTimer   : TrackbarTimerProc(Message);
  end;
end;



procedure Tpanotransform.TrackbarTimerProc(var Message:TWMTimer);
begin
  if isrendering then exit;
  windows.KillTimer(self.Handle,Message.TimerID);
  tbtimer := 0;
  ReRenderPreview(self);
  RemoteGLM.ClientPaint(self); 
end;



procedure Tpanotransform.thetatrackerChange(Sender: TObject);
var
  tracker: TTrackbar absolute sender;
begin
  KillTimer(self.Handle,TrackbarTimer); tbtimer := 0;
  tbtimer := SetTimer(self.Handle,TrackbarTimer,10,nil);


  //tracker := thetatracker;
  if tracker =  thetatracker then
  begin
    tracker.Selstart := min((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    tracker.SelEnd   := max((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    if thetatext.Focused = false then
      thetatext.Text := floattostr(tracker.Position / 10);
    exit;
  end;

  //tracker := phitracker;
  if tracker =  phitracker then
  begin
    tracker.Selstart := min((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    tracker.SelEnd   := max((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    if phitext.Focused = false then
      phitext.Text := floattostr(tracker.Position / 10) ;
    exit;
  end;


  //tracker := psitracker;
  if tracker =  psitracker then
  begin
    tracker.Selstart := min((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    tracker.SelEnd   := max((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    if psitext.Focused = false then
      psitext.Text := floattostr(tracker.Position / 10) ;
    exit;
  end;


  // tracker := pinchtracker;
  if tracker =  pinchtracker then
  begin
    tracker.Selstart := min((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    tracker.SelEnd   := max((tracker.Max + tracker.Min) div 2,
                            tracker.Position);
    if pinchtext.Focused = false then
      pinchtext.Text := floattostr(tracker.Position / 10) ;
    exit;
  end;

end;



procedure Tpanotransform.abortbuttonClick(Sender: TObject);
var
  WorkingLayerIdx : longint;
  PreviewLayerIdx, RenderLayerIdx : longint;
begin
  if self.isrendering then exit;

  RemoteGLM.OnLayerChanges.RemoveEvent(self.LayerChanges);

   PreviewLayerIdx := RemoteGLM.LayerByID(self.PreviewLayerID);
   if PreviewLayerIdx <> -1 then
   begin
     remoteglm.surpressrepaint;
     remoteglm.DeleteLayer(PreviewLayerIdx);
   end;
   // bye bye source lookup engine
   if assigned(SLE) then SLE.Destroy;
   SLE := nil;

   // byebye destination transform, too
   if assigned(DLT) then DLT.Destroy;
   DLT := nil;

   RenderLayerIdx := RemoteGLM.LayerByID(self.RenderLayerID);
   if RenderLayerIdx = -1 then
   begin
     remoteglm.surpressrepaint;
     WorkingLayerIdx := RemoteGLM.LayerByID(WorkingLayerID);
     if WorkingLayerIdx <> -1 then
       remoteglm.FocusLayer(WorkingLayerIdx);
   end
   else
   begin
     remoteglm.surpressrepaint;
     remoteglm.FocusLayer(RenderLayerIdx);
   end;

  if self.Visible then hide;
  if assigned(OnToolAbort) then OnToolAbort(self); // ClientPaint!

  RemoteGLM.ClientPaint(self);
end;



procedure Tpanotransform.ApplyToolButtonClick(Sender: TObject);
var
  renderidx, widx,pidx: integer;
  rct: TRect;
  alpha_h, alpha_v, theta,phi,psi, pinch: single;
  w,h: integer;
const
   INVALID_CONVERSION = -93427313;
begin

  // sanity checks
  if self.isrendering then exit;

  widx := RemoteGLM.LayerByID(self.WorkingLayerID);
  if widx = -1 then exit;

  // range checks
  alpha_h := StrToFloatDef(AlphaHText.Text,INVALID_CONVERSION);
  alpha_v := StrToFloatDef(AlphaVText.Text,INVALID_CONVERSION);
  theta   := StrToFloatDef(thetatext.Text, INVALID_CONVERSION)-90;
  phi     := StrToFloatDef(phitext.Text,   INVALID_CONVERSION);
  psi     := StrToFloatDef(psitext.Text,   INVALID_CONVERSION);
  pinch   := StrToFloatDef(pinchtext.Text, INVALID_CONVERSION);

  if (alpha_h = INVALID_CONVERSION) or
     (alpha_v = INVALID_CONVERSION) or
     (theta = INVALID_CONVERSION-90) or
     (phi = INVALID_CONVERSION) or
     (psi = INVALID_CONVERSION) or
     (pinch = INVALID_CONVERSION) then
    begin
      Application.MessageBox(
        'One or more input fields have illegal float values in them.'#13#10+
        'Please be so kind as to fix that.',
        'Faulty input parameters',MB_ICONWARNING or MB_OK);
      exit;
    end;

  // expand to 360x180, if desired
  if checkbox1.Checked then
  begin
    w := round(RemoteGLM.Layers[widx].data.Width  * (360/alpha_h));
    h := round(RemoteGLM.Layers[widx].data.Height * (180/alpha_v));
  end
  else
  begin
    w := RemoteGLM.Layers[widx].data.Width;
    h := RemoteGLM.Layers[widx].data.Height;
  end;

  rct.TopLeft := RemoteGLM.Layers[widx].Position;
  rct.Right := rct.Left + w;
  rct.Bottom := rct.Top + h;

  // kill preview layer
  pidx := RemoteGLM.LayerByID(self.PreviewLayerID);
  if pidx <> -1 then
  begin
    // dispose preview layer
    remoteglm.surpressrepaint;
    RemoteGLM.DeleteLayer(pidx);
  end;
  PreviewLayerID := 0;

  // prepare rendering target buffer
  RemoteGLM.surpressrepaint;
  renderidx := RemoteGLM.NewLayer(rct,clblack);
  self.RenderLayerID := RemoteGLM.Layers[renderidx].LayerID;
  RemoteGLM.Layers[renderidx].LayerCaption :=
    RemoteGLM.Layers[widx].LayerCaption + '_PanoTr';

  // refresh UI to show empty buffer
  self.Hide;
  remoteGLM.ClientPaint(self);

  // bring up a progressbar
  VO2:= TVisualOverlayProgressBar.Create(ClientRect);
  VO2.Top  := Application.MainForm.ClientHeight- VO2.Height-8;
  VO2.Left := Application.MainForm.ClientWidth - VO2.Width -8;
  VO2.Anchors := [akBottom, akRight];
  RemoteGLM.InsertVisualOverlayObject(VO2);
  VO2.max := 100;
  VO2.Caption := 'transfroming panorama ...';
  VO2.Visible := true;

  // do the transformation
  SphericalToSphericalTransform(  RemoteGLM.Layers[renderidx].data,
                                  RemoteGLM.Layers[widx].data,
                                  alpha_h,alpha_v,
                                  theta, phi, psi,
                                  pinch,
                                  true,
                                  nil,
                                  nil,
                                  self.UpdateProgressBar);

  // update UI
  RemoteGLM.surpressrepaint;
  RemoteGLM.RemoveVisualOverlayObject(VO2);
  VO2.Destroy;

  // clean up
  self.abortbuttonClick(self);
end;



procedure Tpanotransform.thetatrackerKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key = '0' then thetatracker.Position := 900;
end;



procedure Tpanotransform.phitrackerKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key = '0' then phitracker.Position := 0;
end;



procedure Tpanotransform.psitrackerKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key = '0' then psitracker.Position := 0;
end;



procedure Tpanotransform.thetatextClick(Sender: TObject);
begin
  windows.SetFocus((Sender as TWinControl).Handle);
end;



procedure Tpanotransform.FormShow(Sender: TObject);
//var
//  WorkingLayerIdx: longint;
begin
//  WorkingLayerIdx := RemoteGLM.LayerByID(WorkingLayerID);


  RemoteGLM.OnLayerChanges.InsertEvent(self.LayerChanges);
  if StrToFloatDef(FixDecSep(AlphaHText.Text),0) >
     StrToFloatDef(FixDecSep(AlphaVText.Text),0) * 2
    then
       self.AlphaHTextChange(AlphaVText)
    else
       self.AlphaHTextChange(AlphaHText);

  isrendering := false;
  self.ApplyToolButton.Enabled := False;
  self.GroupBox1.Enabled := False;
  self.GroupBox4.Enabled := False;
  self.setrangebutton.Enabled := true;
end;



procedure Tpanotransform.LayerChanges(Sender:TObject);
begin
  if RemoteGLM.LayerByID(self.WorkingLayerID) = -1 then
    self.abortbuttonClick(self);
end;



procedure Tpanotransform.AlphaHTextChange(Sender: TObject);
var
  aspect: single;
//  val: single;
  WorkingLayerIdx: longint;
begin
  setrangebutton.Enabled := true;

  if not (Sender is TEdit) then exit;
  WorkingLayerIdx := RemoteGLM.LayerByID(WorkingLayerID);
  if WorkingLayerIdx = -1 then exit;

  aspect := RemoteGLM.Layers[WorkingLayerIdx].data.width /
            RemoteGLM.Layers[WorkingLayerIdx].data.height;

  if (Sender as TEdit) = AlphaHText then
  begin
    if not AlphaVText.Focused then
      AlphaVText.Text := FloatToStr(StrToFloatDef(AlphaHText.Text,0)/aspect)
  end
  else
  begin
    if not AlphaHText.Focused then
      AlphaHText.Text := FloatToStr(StrToFloatDef(AlphaVText.Text,0)*aspect);
  end;
end;



procedure Tpanotransform.setrangebuttonClick(Sender: TObject);
var
  prevrect: TRect;
  fitfactor, scaling: single;
  widx,pidx, w, h, clw, clh: longint;
  w2, h2: longint;
  alpha_h, alpha_v : single;
  gridsize: integer;
  // bb: word;
  realwidth, realheight : integer;  // the size of the output image
begin
    setrangebutton.Enabled := false;
    // do something
    self.ApplyToolButton.Enabled := true;
    self.GroupBox1.Enabled := true;
    self.GroupBox4.Enabled := true;

    widx := RemoteGLM.LayerByID(self.WorkingLayerID);
    if widx = -1 then exit;



    if RemoteGLM.LayerByID(PreviewLayerID) = -1 then
    begin
      prevrect.Left := RemoteGLM.Layers[widx].Position.X;
      prevrect.Top  := RemoteGLM.Layers[widx].Position.Y;

      w := RemoteGLM.Layers[widx].data.Width;
      h := RemoteGLM.Layers[widx].data.Height;
      realwidth  := w; // changes may be applied below (expand to 360x180)
      realheight := h;

      // generate an up-scaled preview layer that has
      // the width = clientwidth, if downscaling needed
      //
      clw := Application.MainForm.ClientWidth;
      clh := Application.MainForm.ClientHeight;

      alpha_h := StrToFloatDef(AlphaHText.Text,360);
      alpha_v := StrToFloatDef(AlphaVText.Text,180);

      scaling := 1;
      if (w/clw > 1) or (h/clh >1) then
      begin
        // scale to fit width or height,
        // whichever relatively exceeds worse
        //
        scaling := max(w/clw,h/clh);
        fitfactor := 1/scaling;

        w := round (w*fitfactor);
        h := round (h*fitfactor);
      end;

      if checkbox1.Checked then //  expand to 360x180
      begin
        w2 := w; h2 := h;
        w := round(w * (360/alpha_h));
        h := round(h * (180/alpha_v));

        realwidth  := round(realwidth  * (360/alpha_h)); // predict final output size
        realheight := round(realheight * (180/alpha_v));

        prevrect.Left := prevrect.Left - (w-w2) div 2;
        prevrect.Top  := prevrect.Top  - (h-h2) div 2;
        pinchtext.Text := FloatToStr(-(h-h2) div 2); // shift orientate to top
      end;

      prevrect.Right  := prevrect.Left + w;  // add scaled-down width and height to compute preview rect
      prevrect.Bottom := prevrect.Top  + h;

      remoteglm.surpressrepaint;
      pidx := RemoteGLM.NewLayer(prevrect,clsilver);
      self.PreviewLayerID := RemoteGLM.Layers[pidx].LayerID;
      RemoteGLM.Layers[pidx].InternalScaling := scaling;


      if assigned(SLE) then SLE.Destroy;

      // good reference values:
      //  930 px / 360° :  = 12
      // 3200 px / 360° :  = 24
      // dynamic grid size
      gridsize := round((RemoteGLM.Layers[widx].data.Width / alpha_h)*1.8 + 7.3);
      if RemoteGLM.Layers[widx].data.Width < 800 then gridsize := 4;
      // grid size bracketing
      if gridsize < 4 then gridsize := 4;
      if gridsize > 28 then gridsize := 28;


      SLE:= TSourceLookupEngine.Create(
          RemoteGLM.Layers[widx].data.Width,
          RemoteGLM.Layers[widx].data.Height,
          alpha_h,alpha_v,
          RemoteGLM.Layers[widx].data,
          gridsize);

      if assigned(DLT) then DLT.Destroy;

      DLT := TSphericalLookupTransform.Create(
        Point(w,h),
        alpha_h);
    end;

    RemoteGLM.InsertPreviewHook(self.PreviewFilterProc);
    ReRenderPreview(self);

    // resume
    RemoteGLM.ClientPaint(self);

    pinchtracker.Min := - realheight * 10;
    pinchtracker.Max := + realheight * 10; // shifting image over full height of the target picture is allowed
end;



procedure Tpanotransform.ReRenderPreview(Sender:TObject);
var
  PrevIdx, WorkIdx: longint;
  PreviewBMP,WorkingBMP:TBitmap;
  alpha_h, alpha_v, theta,phi,psi, pinch: single;
  // P, Q: PByteArray;
  // x, y: longint;
const
   INVALID_CONVERSION = -93427313;
begin
  // the magic takes place here

  PrevIdx := RemoteGLM.LayerByID(self.PreviewLayerID);
  WorkIdx := RemoteGLM.LayerByID(self.WorkingLayerID);

  WorkingBMP := RemoteGLM.Layers[WorkIdx].data;
  PreviewBMP := RemoteGLM.Layers[PrevIdx].data;

  if (PrevIdx=-1 ) or (WorkIdx = -1) then exit;

  alpha_h := StrToFloatDef(AlphaHText.Text,INVALID_CONVERSION);
  alpha_v := StrToFloatDef(AlphaVText.Text,INVALID_CONVERSION);
  theta   := StrToFloatDef(thetatext.Text,INVALID_CONVERSION)-90;
  phi     := StrToFloatDef(phitext.Text,INVALID_CONVERSION);
  psi     := StrToFloatDef(psitext.Text,INVALID_CONVERSION);
  pinch   := StrToFloatDef(pinchtext.Text,INVALID_CONVERSION);

  if (alpha_h = INVALID_CONVERSION) or
     (alpha_v = INVALID_CONVERSION) or
     (theta = INVALID_CONVERSION-90) or
     (phi = INVALID_CONVERSION) or
     (psi = INVALID_CONVERSION) or
     (pinch = INVALID_CONVERSION) then
    begin
      Application.MessageBox(
        'One or more input fields have illegal float values in them.'#13#10+
        'Please be so kind as to fix that.',
        'Faulty input parameters',MB_ICONWARNING or MB_OK);
      exit;
    end;

  isrendering := true;

  SphericalToSphericalTransform(  PreviewBMP, WorkingBMP,
                                  alpha_h, alpha_v,
                                  theta, phi, psi,
                                  pinch,
                                  false,SLE,DLT,nil );

  RemoteGLM.Layers[PrevIdx].LastModified := GetTickCount;

  isrendering := false;


  if checkbox2.Checked then
    self.RemoteGLM.OnLayerChanges.SharedEvent(RemoteGLM);
end;



procedure Tpanotransform.PreviewFilterProc(LayerID: integer; target: TBitmap; rct: TRect);
var
  idx: integer;
  p,q: TPoint;
  scaling: single;
begin
   if LayerID = self.PreviewLayerID then
   begin
     idx := RemoteGLM.LayerByID(PreviewLayerID);
     p := RemoteGLM.Layers[idx].Position;
     scaling := RemoteGLM.Layers[idx].InternalScaling;
     p.X := round(p.X + RemoteGLM.Layers[idx].data.Width * scaling / 2);  // internal scaling should be supported natively
     p.Y := round(p.Y + RemoteGLM.Layers[idx].data.Height* scaling / 2);
     q := RemoteGLM.GlobalCoordToScreen(p);

     // render crosshairs
     if checkbox3.Checked then
     begin
       target.Canvas.Pen.Color := clwhite;
       target.Canvas.Pen.Width := 1;

       target.Canvas.MoveTo(0,q.y);
       target.Canvas.LineTo(target.Width,q.y);
       target.Canvas.MoveTo(q.x,0);
       target.Canvas.LineTo(q.x, target.Height);

       target.Canvas.Pen.Color := clblack;
       target.Canvas.Pen.Width := 1;

       target.Canvas.MoveTo(0,q.y+1);
       target.Canvas.LineTo(target.Width,q.y+1);
       target.Canvas.MoveTo(q.x+1,0);
       target.Canvas.LineTo(q.x+1, target.Height);
     end;
   end;
end;


procedure Tpanotransform.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not self.isrendering then
    self.abortbuttonClick(self)
  else
    Action := caNone;
end;



procedure Tpanotransform.thetatextChange(Sender: TObject);
begin
  Thetatracker.Position := round(10*StrToFloatDef((Sender as TEdit).Text,90));
end;



procedure Tpanotransform.phitextChange(Sender: TObject);
begin
 Phitracker.Position := round(10*StrToFloatDef((Sender as TEdit).Text,0));
end;



procedure Tpanotransform.psitextChange(Sender: TObject);
begin
 Psitracker.Position := round(10*StrToFloatDef((Sender as TEdit).Text,0));
end;



procedure Tpanotransform.UpdateProgressBar(Position: integer; Maximum: integer);
begin
  if assigned(VO2) then VO2.position := 100 * Position / (Maximum+0.00001);
end;



procedure Tpanotransform.pinchtrackerKeyPress(Sender: TObject; var Key: Char);
begin
  if key = '0' then pinchtracker.Position := 0;
end;



procedure Tpanotransform.pinchtextChange(Sender: TObject);
begin
 pinchtracker.Position := round(10*StrToFloatDef((Sender as TEdit).Text,0));
end;



procedure Tpanotransform.CheckBox3Click(Sender: TObject);
begin
  RemoteGLM.ClientPaint(self);
end;





end.
