unit panorect;

interface
uses
  Windows, GLWDef, GLMFunctionality, GraphicLayerManager,
  VisualOverlayClass,MultiFunctionalInputDialog,
  Forms, Graphics, Controls, Math, Sysutils, Classes,Types,
  sphericaltransforms, PanotransformClasses, trigonometry,
  PanoRectSettings;


type TPanoRectilinearTool = class (TObject)
    constructor Create(Parent: TForm);
    destructor Destory(); 
  private
    Viewport: TBufferedVisualOverlay;
    AbortButton,
    ApplyButton,
    DlgButton :  TVisualOverlayButton;
    SLE: TSourceLookupEngine;

    //_focallen : single;
    params, oldparams, startparams: TViewportParameters;

    bootparams: TViewportParameters;
    bootparamsvalid: Boolean;

    startcv, endcv: TVector3d; // cursor vectors for dragging of the viewport
    startcenter: TVector3d;
    mousestart:TPoint;

    omt, nmt: TVector3d; // old and new in-plane motion tracker pointers to capture infinite circular cursor movements
    angularsteps: double;
    lastangle, angledelta: double;

    forcerepaint: boolean;
    MouseInUse: Boolean;
    CurrentSourceDate: Longint;

    procedure ApplyClick(Sender:TObject);
    procedure AbortClick(Sender:TObject);
    procedure DlgClick(Sender:TObject);
    procedure ToolFinishSequence();
    procedure SetFocallen(new: single);

    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject;
                  Shift: TShiftState; X, Y: Integer);
    procedure MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);

    procedure KeyDown (Sender:TObject; var Key: Word; Shift: TShiftState);
    procedure KeyUp (Sender:TObject; var Key: Word; Shift: TShiftState);

    procedure DialogHideEvent(Sender:TObject);
    procedure DialogShowEvent(Sender:TObject);

    procedure DialogUpdateEvent(Sender: TObject);
  public
    ToolExternalCleanup: TNotifyEvent;
    ToolAbort: TNotifyEvent;
    remoteGLM: TGraphicLayerManager;
    parent: TForm;
    ScreenBuffer: TBitmap;
    WorkingLayerID : longint;
    alpha_h, alpha_v: double;


    property focallen: single read params._focallen write setfocallen;

    procedure Execute(LayerID: Longint);
    function ScreenToSphere(X, Y: integer; var par: TViewportParameters):TVector3d;

    // mouse and keyboard events!
    procedure PaintScreenBuffer(Sender:TObject);
    procedure ProcessLayerChanges(Sender:TObject);
end;

var
  PanoRectTool : TPanoRectilinearTool;

implementation



constructor TPanoRectilinearTool.Create(Parent: TForm);
begin
  inherited Create();

  Viewport    := TBufferedVisualOverlay.Create(Parent.ClientRect);
  AbortButton := TVisualOverlayButton.Create(Parent.ClientRect);
  ApplyButton := TVisualOverlayButton.Create(Parent.ClientRect);
  DlgButton   := TVisualOverlayButton.Create(Parent.ClientRect);

  self.Parent  := Parent;
  forcerepaint := false;
  MouseInUse   := false;

  bootparamsvalid := false;
end;


destructor TPanoRectilinearTool.Destory();
begin
  RemoteGLM.RemoveVisualOverlayObject(Viewport);
  Viewport.Destroy;

  RemoteGLM.RemoveVisualOverlayObject(AbortButton);
  AbortButton.Destroy;

  RemoteGLM.RemoveVisualOverlayObject(ApplyButton);
  ApplyButton.Destroy;

  RemoteGLM.RemoveVisualOverlayObject(DlgButton);
  DlgButton.Destroy;

  inherited;
end;



procedure TPanoRectilinearTool.Execute(LayerID: Longint);
var
  mid: TMultiFunctionalInputDialog;
  var DialogResult : Boolean;
  str1, str2 : string;
  aspect: string;
  alphastr: string;
  aspectRatio : single;
  w, h, s: single;
  // minxy,
  maxxy: single;

  // transformation vars
  gridsize: integer;
  widx: longint;

  PanoramaInfoAvailable: Boolean;
  hfov : double;

  infodata: PQueryIndexList;
  infoEntryID: longint;
  infoEntry: TImageProjectionInfo;
  ShowConsistencyWarning: Boolean;
  hfovAlternatives: string;
  i: integer;
begin
  str1 := 'You need to specify the parameters of the panorama (horizontal / vertial field)'#13#10+
    'and aspect ratio of the projected crop portal.';

  widx := RemoteGLM.LayerByID(WorkingLayerID);
  if widx = -1 then exit;

  CurrentSourceDate := RemoteGLM.Layers[widx].LastMOdified;

  //PanoramaInfoAvailable  := false;
  ShowConsistencyWarning := false;
  hfov := 360; // make the compiler happy.

  //
    // get pano info from meta data, store alpha_h in hfov, set PanoramaInfoAvailable to true
  infodata := RemoteGLM.Layers[widx].meta.QueryEntries(TImageProjectionInfo);
  case length(infodata^.UIDs) of
     0: PanoramaInfoAvailable := false;
     1: begin
          PanoramaInfoAvailable := true;
          infoEntryID := infodata^.UIDs[0];
          infoEntry := TImageProjectionInfo(RemoteGLM.Layers[widx].meta.Find(infoEntryID));
          hfov := infoEntry.alpha_h;
        end;
  else
    begin
      PanoramaInfoAvailable  := true;
      ShowConsistencyWarning := true;
      hfovAlternatives := '';

      for i := 0 to length(infodata^.UIDs)-1 do
      begin
        infoEntryID := infodata^.UIDs[i];
        infoEntry := TImageProjectionInfo(RemoteGLM.Layers[widx].meta.Find(infoEntryID));
        if i = 0 then hfov := infoEntry.alpha_h;
        if i > 0 then hfovAlternatives := hfovAlternatives +'|';
        hfovAlternatives := hfovAlternatives + FloatToStr(infoEntry.alpha_h);
      end;
    end;
  end;
  RemoteGLM.Layers[widx].meta.FreeQuery(infodata^.QueryID);


  mid := TMultiFunctionalInputDialog.Create;
  mid.frm.Caption := 'Panorama parameters (rectilinear portal editor)';

  mid.AddInputField('Information about the settings below ...',nil,'info','','',str1);
  if not bootparamsvalid then bootparams.aspect := '2:1';
  mid.AddInputField( 'aspect ratio',@aspect,'combo',bootparams.aspect,'','3:1|2:1|3:2|4:3|6:5|1:1|5:6|3:4|2:3|1:2|1:3');

  if not ShowConsistencyWarning then
  begin
    if not PanoramaInfoAvailable then hfov := 360;
    mid.AddInputField('horizontal angle of view (degrees)',@alpha_h,'float','0','360',FloatToStr(hfov));
  end
  else
  begin
    str2 := 'More than one set of image parameters was found.'#13#10'these should be merged or removed.';
    mid.AddInputField('Warning: image information is inconsistent.',nil,'info','','',str2);
    mid.AddInputField('horizontal angle of view (degrees)',@alphastr,'combo',FloatToStr(hfov),'',hfovAlternatives);
  end;

  DialogResult := mid.Execute;

  if (not ShowConsistencyWarning) and (aspect = '') then // exit if aspect is invalid
    DialogResult := false;


  if (DialogResult = true) then
  begin
    CopyMemory(@params,@bootparams,sizeOf(TViewportParameters));

    if ShowConsistencyWarning then
      alpha_h := StrToFloatDef(alphastr,360);

    params.aspect := aspect; // remember this value for bootparams
    aspect := ReplaceKeyWord(aspect,':','/');
    aspectRatio := QuotientStringToFloat(aspect);

    if not bootparamsvalid then
      params._focallen := 20;


    maxxy := max(Parent.ClientWidth-128, Parent.ClientHeight -64);

    w := aspectratio * maxxy / (Parent.ClientWidth -128);  // horizontal and vertical ratios
    h := 1           * maxxy / (Parent.ClientHeight-128);
    s := max(w,h);


    // start source lookup engine
    alpha_v := alpha_h * RemoteGLM.Layers[widx].data.height / RemoteGLM.Layers[widx].data.Width;

    if assigned(SLE) then SLE.Destroy;
      // good reference values:
      //  930 px / 360� :  = 12
      // 3200 px / 360� :  = 24
      // dynamic grid size
      //gridsize := round((RemoteGLM.Layers[widx].data.Width / alpha_h)*1.8 + 7.3);
      gridsize := round((RemoteGLM.Layers[widx].data.Width / alpha_h)*1.2 + 6);
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

    // insert viewport
    Viewport.Visible := false;
    RemoteGLM.InsertVisualOverlayObject(Viewport);
    Viewport.Anchors := [ akLeft, akRight, akTop, akBottom ];

    // the parent rect may change from start-up till now,
    // but the overlays aren't inserted so no events are passed
    // we'll catch up with this issue and use InsertVisualOverlayObject
    // immediately afterwards.
    Viewport.UpdateParentAreaChanges(self.parent.ClientRect); // fix margin values
    Viewport.OnPaint      := self.PaintScreenBuffer;

    Viewport.Width  := round(maxxy * aspectratio / s);
    Viewport.Height := round(maxxy / s);
    Viewport.Left   := (Parent.ClientWidth  - Viewport.Width ) div 2;
    Viewport.Top    := (Parent.ClientHeight - Viewport.Height) div 2-16;

//    Viewport.OnPaint      := self.PaintScreenBuffer;
    Viewport.OnMouseWheel := self.MouseWheel;
    Viewport.OnMouseDown  := self.MouseDown;
    Viewport.OnMouseMove  := self.MouseMove;
    Viewport.OnMouseUp    := self.MouseUp;
    Viewport.OnKeyDown    := self.KeyDown;
    Viewport.OnKeyUp      := Self.KeyUp;

    RemoteGLM.surpressrepaint;
    ForceRepaint := true; // don't spare the first rendering action on startup
    Viewport.Visible := true;

    // insert apply button
    ApplyButton.UpdateParentAreaChanges(self.parent.ClientRect); // fix margin values
    ApplyButton.Visible := false;
    ApplyButton.Width := 64;
    ApplyButton.Height := 28;
    ApplyButton.Top := ViewPort.Top + ViewPort.Height;
    ApplyButton.Left := ViewPort.Left + ViewPort.Width-ApplyButton.Width;
    ApplyButton.Caption := 'apply';
    ApplyButton.OnClick := ApplyClick;
    RemoteGLM.InsertVisualOverlayObject(ApplyButton);
    ApplyButton.Anchors := [ akRight, akBottom ];

    RemoteGLM.surpressrepaint;
    ApplyButton.Visible := true;


    // insert abort button
    AbortButton.UpdateParentAreaChanges(self.parent.ClientRect); // fix margin values
    AbortButton.Visible := false;
    AbortButton.Width := 64;
    AbortButton.Height := 28;
    AbortButton.Top := ViewPort.Top + ViewPort.Height;
    AbortButton.Left := ApplyButton.Left -AbortButton.Width;
    AbortButton.Caption := 'abort';
    AbortButton.OnClick := AbortClick;

    RemoteGLM.InsertVisualOverlayObject(AbortButton);
    AbortButton.Anchors := [ akRight, akBottom ];
    AbortButton.Visible := true;


    // insert dialog button
    DlgButton.UpdateParentAreaChanges(self.parent.ClientRect); // fix margin values
    DlgButton.Visible := false;
    DlgButton.Width := 96;
    DlgButton.Height := 28;
    DlgButton.Top := ViewPort.Top + ViewPort.Height;
    DlgButton.Left := Viewport.Left;
    DlgButton.Caption := 'show options';
    DlgButton.OnClick := DlgClick;

    RemoteGLM.InsertVisualOverlayObject(DlgButton);
    DlgButton.Anchors := [ akLeft, akBottom ];
    DlgButton.Visible := true;

    RemoteGLM.OnLayerChanges.InsertEvent(self.ProcessLayerChanges);

    PanoRectSettingsDlg.Parent := self.parent;
    PanoRectSettingsDlg.OnHide := self.DialogHideEvent;
    PanoRectSettingsDlg.OnShow := self.DialogShowEvent;
    PanoRectSettingsDlg.OnParametersUpdated := self.DialogUpdateEvent;

    WIndows.SetParent(PanoRectSettingsDlg.Handle,self.parent.Handle);
    with PanoRectSettingsDlg do
    begin
      Left := DlgButton.Left;
      Top  := DlgButton.Top - Height;
    end;

  end
  else
    if parent.Visible then ToolFinishSequence; // otherwise form1 is already closed and application is terminating

  if assigned (mid) then
    mid.Destroy;

  RemoteGLM.ClientPaint(self); // strange.. sometimes the viewport stays unrendered on startup -> debug
end;



procedure TPanoRectilinearTool.ApplyClick(Sender:TObject);
var
  ipinfo : TImageProjectionInfo;
  EntryID: longint;
  q : PQueryIndexList;
  widx : longint;
begin
  widx := RemoteGLM.LayerByID(WorkingLayerID);

  if widx <> -1 then
  begin
    // save image projection info
    q := RemoteGLM.Layers[widx].meta.QueryEntries(TImageProjectionInfo);

    case length(q^.UIDs) of
      1: begin
           EntryID := q^.UIDs[0];
           ipinfo := TImageProjectionInfo(RemoteGLM.Layers[widx].meta.Find(EntryID)); // retrieve object
           ipinfo.projection := PM_SPHERICAL;  // update parameters
           ipinfo.alpha_h  := alpha_h;
           ipinfo.alpha_v  := alpha_v;
           ipinfo.origin_x := 0;
           ipinfo.origin_y := RemoteGLM.Layers[widx].data.height / 2;
        end;
    else // 0 or more than one: add or append
         begin
           ipinfo := TImageProjectionInfo.Create;
             ipinfo.projection := PM_SPHERICAL;
             ipinfo.alpha_h  := alpha_h;
             ipinfo.alpha_v  := alpha_v;
             ipinfo.origin_x := 0;
             ipinfo.origin_y := RemoteGLM.Layers[widx].data.height / 2;
           RemoteGLM.Layers[widx].meta.Add(ipinfo);
         end;
    end;

    RemoteGLM.Layers[widx].meta.FreeQuery(q^.QueryID);
  end;


  ToolFinishSequence;

  // execute re-integration filters, show progress bar.
end;



procedure TPanoRectilinearTool.AbortClick(Sender:TObject);
begin
  ToolFinishSequence;
end;



procedure TPanoRectilinearTool.ToolFinishSequence();
begin
  CopyMemory(@bootparams,@params,sizeOf(TViewportParameters));
  bootparams.aspect := params.aspect;
  bootparamsvalid := true;

  // write panorama hfov - needs to be implemented

  // do the rest
  RemoteGLM.OnLayerChanges.RemoveEvent(self.ProcessLayerChanges);

  if assigned(SLE) then SLE.Destroy;
  SLE := nil;

  RemoteGLM.surpressrepaint;
  PanoRectSettingsDlg.Hide;

  RemoteGLM.surpressrepaint;
  Viewport.Visible := false;

  RemoteGLM.surpressrepaint;
  RemoteGLM.RemoveVisualOverlayObject(Abortbutton);
  //Abortbutton.Visible := false;

  RemoteGLM.surpressrepaint;
  RemoteGLM.RemoveVisualOverlayObject(ApplyButton);
  //Applybutton.Visible := false;

  RemoteGLM.surpressrepaint;
  RemoteGLM.RemoveVisualOverlayObject(DlgButton);

  ViewPort.SetBounds(Rect(0,0,1,1),self.parent.ClientRect); // reduce buffer
  Viewport.Anchors := [akLeft, akTop]; // do not rescale, keep single-pixel buffer
  Viewport.UpdateParentAreaChanges(self.parent.ClientRect); // update to new buffer size
  Viewport.Translucent := false;
  RemoteGLM.RemoveVisualOverlayObject(Viewport);

  if assigned(ToolExternalCleanup) then ToolExternalCleanup(self);
  remoteGLM.ClientPaint(self);
end;



procedure TPanoRectilinearTool.PaintScreenBuffer(Sender:TObject);
var
  src, dst: TBitmap;
  WorkingLayerIdx: longint;
begin
  // initialisation and checks
  WorkingLayerIdx := RemoteGLM.LayerByID(WorkingLayerID);
  if WorkingLayerIdx = -1 then exit;
  src := RemoteGLM.Layers[WorkingLayerIdx].data;
  dst := self.Viewport.ScreenBuffer;
  if not assigned(dst) then exit;

  // rendering
  // dst.Canvas.TextOut(10,10,'render render ...');

  params.viewportsize := Point(dst.Width, dst.Height);

  if (params._focallen <> oldparams._focallen) or
     (params.phi <> oldparams.phi) or
     (params.theta <> oldparams.theta) or
     (params.psi <> oldparams.psi) or
     (params.viewportsize.x <> oldparams.viewportsize.x) or
     (params.viewportsize.y <> oldparams.viewportsize.y) or
     (params.coarsepreview <>  oldparams.coarsepreview) or
     forcerepaint then
  begin
    try
      SphericalToRectilinearTransform (
        dst,src,alpha_h,alpha_v,
        focallen,
        params.theta, params.phi, params.psi,
        SLE,false,params.coarsepreview );

        PanoRectSettings.PanoRectSettingsDlg.UpdateParameters(params);
        forcerepaint := false;
    except
      dst.Canvas.Brush.Style := bsclear;
      dst.Canvas.Pen.Color := clblack;
      dst.Canvas.TextOut(16,16,'render error.');
    end;
  end;

  CopyMemory(@oldparams,@params,sizeof(TViewportParameters));
end;



procedure TPanoRectilinearTool.ProcessLayerChanges(Sender:TObject);
begin
  // validate source image
  if RemoteGLM.LayerByID(WorkingLayerID) = -1 then
     ToolFinishSequence;

  // check for changes
  if CurrentSourceDate <  RemoteGLM.Layers[RemoteGLM.LayerByID(WorkingLayerID)].LastModified then
    self.PaintScreenBuffer(self);

  // repaint
  forcerepaint := true;
  if not Viewport.Translucent then
      self.Viewport.ParentLocalRepaint(Viewport)
    else
      self.Viewport.ParentRepaint(Viewport);
end;



procedure TPanoRectilinearTool.SetFocallen(new: single);
begin
  if new < 3 then new := 3;     // minimum focal length
  if new > 800 then new := 800; // maximum focal length
  if params._focallen <> new then
  begin
    params._focallen := new;
    if not Viewport.Translucent then
      self.Viewport.ParentLocalRepaint(Viewport)
    else
      self.Viewport.ParentRepaint(Viewport);
  end;
end;



procedure TPanoRectilinearTool.MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
//if Viewport.Translucent then exit;
  params.coarsepreview := true;
  if ssShift in Shift then
    focallen := params._focallen*exp(ln(1.01) * WheelDelta * 0.01)
  else
    focallen := params._focallen*exp(ln(1.05) * WheelDelta * 0.01);

  handled := true;
end;



procedure TPanoRectilinearTool.MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  X := X - viewport.Left; // mouse coordinates are respective to screen buffer!
  Y := Y - viewport.Top;

  MouseInUse := true;
  mousestart := Point(x,y);
  omt.x := x - viewport.Width /2;
  omt.y := y - viewport.Height/2;
  omt.z := 0;

  startparams := params; // oldparams are the params for the last render, not a backup for previous actions!

  endcv := startcv;
  startcv := self.ScreenToSphere(X,Y,params); // start cursor vector
  startcenter := PolarToCartesian(1, (params.theta+90)*pi/180, (params.phi-90)*pi/180); // viewport center
  lastangle :=0;
  angledelta :=0;
  angularsteps :=0;
end;



procedure TPanoRectilinearTool.MouseMove(Sender: TObject;
                  Shift: TShiftState; X, Y: Integer);
var
  normal: TVector3d;
  norm2, angle: double;
  R: TRMatrix;
  newcenter: TVector3d;
  //testcenter: TVector3d;

  cross_z: single;
  // newangle:double;
  f: double;
begin
  X := X - viewport.Left; // mouse coordinates are respective to screen buffer!
  Y := Y - viewport.Top;

  params.coarsepreview := true;

  endcv := self.ScreenToSphere(X,Y,startparams);

  // pan up, down, left or right
  //
  if ssLeft in Shift then
  begin
    // generate rotation matrix
    normal := CrossProduct(startcv, endcv);
    norm2  := sqrt(Scalarproduct(normal, normal));
    if norm2 <> 0 then ScaleVector(normal,1/norm2) else normal.x := 1;
    angle := arccos(  Scalarproduct(startcv, endcv) /
                    sqrt(Scalarproduct(startcv, startcv)*Scalarproduct(endcv, endcv)));
    R := AssociatedRotation3D(normal,-angle);

    // apply rotation, derive new viewport parameters
    newcenter := MatrixTimesVector(R,startcenter);
    params.phi   := 180/pi * PreciseArg(newcenter.x,newcenter.y) + 90;
    params.theta := 180/pi * Arccos(newcenter.z) - 90;

    // update screen
    if not Viewport.Translucent then
        self.Viewport.ParentLocalRepaint(Viewport)
      else
        self.Viewport.ParentRepaint(Viewport);
  end;

  // rotate viewport around the visible center (viewport axis) by changing psi
  //
  if (ssRight in Shift) and not Viewport.Translucent then
  begin
    nmt.x := x - viewport.Width /2;
    nmt.y := y - viewport.Height/2;
    nmt.z := 0;
    cross_z := omt.x*nmt.y - omt.y*nmt.x; // sign determines direction of infinitesimal rotation
    f := sqrt(ScalarProduct(nmt,nmt) * ScalarProduct(omt,omt));
    if f <> 0 then cross_z := cross_z / f;
    omt := nmt;

    if ssShift in Shift then // if CTRL is pressed while rotating the screen with the right mouse button held down, switch to fine tuning
      begin
        params.psi := params.psi +0.1*180/pi * arcsin(cross_z);
      end
    else
      begin
        params.psi := params.psi +1.0*180/pi * arcsin(cross_z);
      end;

    if params.psi < -360 then params.psi := params.psi +360;
    if params.psi >  360 then params.psi := params.psi -360;

    // update screen
    if not Viewport.Translucent then
        self.Viewport.ParentLocalRepaint(Viewport)
      else
        self.Viewport.ParentRepaint(Viewport);
  end;
end;



procedure TPanoRectilinearTool.MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
//  X := X - viewport.Left; // mouse coordinates are respective to screen buffer!
//  Y := Y - viewport.Top;
  PanoRectSettingsDlg.StopTimers(self);

  MouseInUse := false;
  params.coarsepreview := false;
  if not Viewport.Translucent then
      self.Viewport.ParentLocalRepaint(Viewport)
    else
      self.Viewport.ParentRepaint(Viewport);
  startparams := params;
end;



procedure TPanoRectilinearTool.KeyDown (Sender:TObject;
                  var Key: Word; Shift: TShiftState);
var
  handled: Boolean; // dummy
  dorepaint: Boolean;
  m: single;

const
  MOUSEWHEEL_DELTA = 120;
  VK_OEM_PLUS      = $BB;
  VK_OEM_MINUS     = $BD;

  function Modulo(Value: Double; m: Double):Double;
  begin
    if m <> 0 then
      result := value - trunc(value/m)*m
    else
      result := Value;
  end;

begin
  dorepaint := false;

  if ssShift in Shift then m := 0.1 else m := 2;

  if (key = VK_ADD) or (key = VK_OEM_PLUS)  then
    self.MouseWheel(self,Shift,+MOUSEWHEEL_DELTA,remoteGLM.zoompoint,handled);

  if (key = VK_SUBTRACT) or (key = VK_OEM_MINUS) then
    self.MouseWheel(self,Shift,-MOUSEWHEEL_DELTA,remoteGLM.zoompoint,handled);

  // implement WSAD-navigation for sphere just for fun of it :-)
  if (key = $53{S}) then
  begin
    params.theta := params.theta + m*normallen/params._focallen;
    if params.theta > 90 then params.theta := 90;
    DoRepaint := true;
  end;

  if (key = $57{W}) then
  begin
    params.theta := params.theta - m*normallen/params._focallen;
    if params.theta < -90 then params.theta := -90;
    DoRepaint := true;
  end;

  if (key = $41{A}) then
  begin
    params.phi := modulo((params.phi + m*normallen/params._focallen + 360),360);
    DoRepaint := true;
  end;

  if (key = $44{D}) then
  begin
    params.phi := modulo((params.phi - m*normallen/params._focallen + 360),360);
    DoRepaint := true;
  end;

  if DoRepaint then
  begin
    params.coarsepreview := true;
    if not Viewport.Translucent then
      self.Viewport.ParentLocalRepaint(Viewport)
    else
      self.Viewport.ParentRepaint(Viewport);
  end;
end;



procedure TPanoRectilinearTool.KeyUp (Sender:TObject;
                  var Key: Word; Shift: TShiftState);
begin
  if params.coarsepreview and (not MouseInUse) then // don't do this while mouse is active
  begin
    params.coarsepreview := false;

    if not Viewport.Translucent then
        self.Viewport.ParentLocalRepaint(Viewport)
      else
        self.Viewport.ParentRepaint(Viewport);
  end;
end;



function TPanoRectilinearTool.ScreenToSphere(X, Y: integer; var par: TViewportParameters):TVector3d;
var
    rot, A1, A2, A3, Ar: TRMatrix;
    dcx, dcy: single;
    a, b, c, g, h, i, k: single;
    norm: single;
    theta, phi, psi: single;
begin
  theta := pi * par.theta / 180;
  phi   := pi * par.phi   / 180;
  psi   := pi * par.psi   / 180;

    with A3 do
    begin
      m11 :=  cos(phi);  m12 := sin(phi);  m13 := 0;
      m21 :=  -sin(phi); m22 := cos(phi);  m23 := 0;
      m31 :=  0;         m32 := 0;         m33 := 1;
    end;

    with A2 do
    begin
      m11 :=  1; m12 := 0;         m13 := 0;
      m21 :=  0; m22 :=  cos(theta); m23 := sin(theta);
      m31 :=  0; m32 := -sin(theta); m33 := cos(theta);
    end;

    with A1 do
    begin
      m11 :=  cos(psi); m12 := 0; m13 := sin(psi);
      m21 :=  0;        m22 := 1; m23 := 0;
      m31 := -sin(psi); m32 := 0; m33 := cos(psi);
    end;

    Ar  := MatrixMultiply3x3(A2, A1);
    rot := MatrixMultiply3x3(A3, Ar);

  dcx := Viewport.Width  / 2;
  dcy := Viewport.Height / 2;

  k := 1/par._focallen * (normallen/(sqrt(sqr(Viewport.width) + sqr(Viewport.height))));

  g := -(x-dcx)*k;
  i := -(y-dcy)*k;

  norm := 1 / (sqrt(g*g+i*i+1));
  a := g*norm;
  b :=   norm;
  c := i*norm;

  with rot do
  begin
    g := m11 * a + m12 * b + m13 * c;
    h := m21 * a + m22 * b + m23 * c;
    i := m31 * a + m32 * b + m33 * c;
  end;

  result.x := g;
  result.y := -h;
  result.z := i;
end;




procedure TPanoRectilinearTool.DlgClick(Sender:TObject);
begin
    if not PanoRectSettingsDlg.Visible then
    with PanoRectSettingsDlg do
    begin
      Left := DlgButton.Left;
      Top  := DlgButton.Top - Height;
    end;

  PanoRectSettingsDlg.Visible := not PanoRectSettingsDlg.Visible;
end;



procedure TPanoRectilinearTool.DialogHideEvent(Sender:TObject);
begin
  DlgButton.Caption := 'show options';
end;



procedure TPanoRectilinearTool.DialogShowEvent(Sender:TObject);
begin
  DlgButton.Caption := 'hide options';
end;


procedure TPanoRectilinearTool.DialogUpdateEvent(Sender: TObject);
begin
  PanoRectSettingsDlg.GetParameters(params);

  if not Viewport.Translucent then
        self.Viewport.ParentLocalRepaint(Viewport)
      else
        self.Viewport.ParentRepaint(Viewport);
end;


end.
