unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,ShellAPI,
  Dialogs, AppEvnts, GraphicLayerManager,GLMFunctionality, ExtCtrls,math, StdCtrls,
  MultiFunctionalInputDialog, Menus, Unit2, Unit3, unit5, unit6, Clipbrd, GLWDef, GLWFilters,
  XPMan, jpeg, MMXBlend, treexml, LayerExplorer, RadialDistortionCorrection, VisualOverlayClass,
  fmath, sphericaltransforms, panorect, TwainHelper, Twain, NotificationNode, ToolBox,
  ToolBoxDesignSpace, CheckLst, ComCtrls, GLMVectorEditor, GLMFilterFileProcessor;


type
  TGLWMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    tools1: TMenuItem;
    linearregistration1: TMenuItem;
    Layers1: TMenuItem;
    NewLayer1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    OpenDialog1: TOpenDialog;
    NewLayerfromfile1: TMenuItem;
    Deletecurrenlayer1: TMenuItem;
    polartocartesian1: TMenuItem;
    SaveLayertofile1: TMenuItem;
    SaveDialog1: TSaveDialog;
    duplicatelayer1: TMenuItem;
    N2: TMenuItem;
    NewLayerfromclipboad1: TMenuItem;
    Adjustments1: TMenuItem;
    Filters1: TMenuItem;
    Chrominance1: TMenuItem;
    mixchannels1: TMenuItem;
    channellevels1: TMenuItem;
    tint1: TMenuItem;
    brightness1: TMenuItem;
    invert1: TMenuItem;
    threshold1: TMenuItem;
    N3: TMenuItem;
    gobackwardsundo1: TMenuItem;
    goforwardredo1: TMenuItem;
    flattenfilterhistory1: TMenuItem;
    dispose1: TMenuItem;
    manipulation1: TMenuItem;
    externalprocessing1: TMenuItem;
    N4: TMenuItem;
    none1: TMenuItem;
    manageremotecommands1: TMenuItem;
    mapcolors1: TMenuItem;
    pixelbinning1: TMenuItem;
    brightnesscontrast1: TMenuItem;
    XPManifest1: TXPManifest;
    restoreimageundoall1: TMenuItem;
    Timer1: TTimer;
    Image1: TImage;
    Info1: TMenuItem;
    about1: TMenuItem;
    spatialbrightnessbalancer1: TMenuItem;
    Switchtolayer1: TMenuItem;
    Explore1: TMenuItem;
    N5: TMenuItem;
    Changemasklayer1: TMenuItem;
    removemasklayerentry1: TMenuItem;
    selectmasklayer1: TMenuItem;
    switchtomasklayer1: TMenuItem;
    MergeLayers1: TMenuItem;
    radialdistortioncorrection1: TMenuItem;
    N6: TMenuItem;
    debugging1: TMenuItem;
    Action11: TMenuItem;
    Bar1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    sphericalpanoramatransform1: TMenuItem;
    openwith1: TMenuItem;
    closeexternalprocessingqueries1: TMenuItem;
    N8: TMenuItem;
    undousageasmasklayer1: TMenuItem;
    sphericalpanoramaeditor1: TMenuItem;
    rectilinearmanipulationportal1: TMenuItem;
    openinfolder1: TMenuItem;
    rescaling1: TMenuItem;
    imagetransformations1: TMenuItem;
    NewLayerfromTWAINsource1: TMenuItem;
    selectimagesource1: TMenuItem;
    TWAINimportimagewithUI1: TMenuItem;
    TWAINimportimageAuto1: TMenuItem;
    RenamecurrentLayer1: TMenuItem;
    debuginputdialog1: TMenuItem;
    TWAINtransfermode: TMenuItem;
    Native1: TMenuItem;
    Memory1: TMenuItem;
    File2: TMenuItem;
    TWAINSelectDeviceParameters: TMenuItem;
    N10: TMenuItem;
    Preferences1: TMenuItem;
    MiddleButtonMenu: TPopupMenu;
    N11: TMenuItem;
    Setzoommethodtolinear1: TMenuItem;
    Setzoommethodtopresetsteps1: TMenuItem;
    Setzoommethodtopixelwise1: TMenuItem;
    Setzoommethodtonozoom1001: TMenuItem;
    Preferences2: TMenuItem;
    N12: TMenuItem;
    N9: TMenuItem;
    N13: TMenuItem;
    closeallactivelinks1: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    BrowseMainDirectory1: TMenuItem;
    oolbox1: TMenuItem;
    ExportToClipboard1: TMenuItem;
    addvectortestdatatomaincontainer1: TMenuItem;
    batchloadmultithreaded1: TMenuItem;
    reimportfromRAWand1: TMenuItem;
    Setzoommodetocurrentlocked1: TMenuItem;
    BatchProcessAction: TMenuItem;
    Filterfileswithcurrentfilterheap1: TMenuItem;
    savetodifferentformat1: TMenuItem;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RenderOverlay(Sender:TObject);
    procedure Button1Click(Sender: TObject);
    procedure Layer01Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure linearregistration1Click(Sender: TObject);
    procedure NewLayer1Click(Sender: TObject);
    procedure NewLayerfromfile1Click(Sender: TObject);
    Procedure AbortToolLinReg(Sender: TObject);
    Procedure StartRecordPoints(var pts: Array of TPoint);
    procedure Layers1Click(Sender: TObject);
    procedure Deletecurrenlayer1Click(Sender: TObject);
    procedure tools1Click(Sender: TObject);
    procedure polartocartesian1Click(Sender: TObject);
    Procedure AbortToolPolToCart(Sender: TObject);
    Procedure AbortToolPixelBinner(Sender: TObject);
    Procedure AbortToolWithPreviewHook(Sender: TObject);
    procedure StartRecordCircle(var pts: Array of TPoint);
    procedure SaveLayertofile1Click(Sender: TObject);
    procedure duplicatelayer1Click(Sender: TObject);
    procedure NewLayerfromclipboad1Click(Sender: TObject);
    procedure Chrominance1Click(Sender: TObject);
    procedure Filters1Click(Sender: TObject);
    procedure gobackwardsundo1Click(Sender: TObject);
    procedure goforwardredo1Click(Sender: TObject);
    procedure flattenfilterhistory1Click(Sender: TObject);
    procedure dispose1Click(Sender: TObject);
    procedure brightness1Click(Sender: TObject);
    procedure invert1Click(Sender: TObject);
    procedure pixelbinning1Click(Sender: TObject);
    procedure brightnesscontrast1Click(Sender: TObject);
    procedure restoreimageundoall1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure about1Click(Sender: TObject);
    procedure WMDROPFILES (var Msg: TMessage); message WM_DROPFILES;
    procedure FormFileDrop(Sender:TObject);
    procedure spatialbrightnessbalancer1Click(Sender: TObject);
    Procedure AbortStdTool(Sender: TObject);
    procedure manipulation1Click(Sender: TObject);
    procedure Switchtolayer1Click(Sender: TObject);
    procedure Explore1Click(Sender: TObject);
    procedure Changemasklayer1Click(Sender: TObject);
    procedure selectmasklayer1Click(Sender: TObject);
    procedure removemasklayerentry1Click(Sender: TObject);
    procedure switchtomasklayer1Click(Sender: TObject);
    procedure MergeLayers1Click(Sender: TObject);
    procedure radialdistortioncorrection1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Action11Click(Sender: TObject);
    procedure sphericalpanoramatransform1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure openwith1Click(Sender: TObject);
    procedure closeexternalprocessingqueries1Click(Sender: TObject);
    procedure undousageasmasklayer1Click(Sender: TObject);
    procedure openinfolder1Click(Sender: TObject);
    procedure Bar1Click(Sender: TObject);
    procedure rectilinearmanipulationportal1Click(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG;
      var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NewLayerfromTWAINsource1Click(Sender: TObject);
    procedure TWAINimportimagewithUI1Click(Sender: TObject);
    procedure TWAINimportimageAuto1Click(Sender: TObject);
    procedure RenamecurrentLayer1Click(Sender: TObject);
    procedure selectimagesource1Click(Sender: TObject);
    procedure debuginputdialog1Click(Sender: TObject);
    procedure Native1Click(Sender: TObject);
    procedure Memory1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure setZoomMethod(Sender: TObject);
    procedure TWAINSelectDeviceParametersClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure closeallactivelinks1Click(Sender: TObject);
    procedure BrowseMainDirectory1Click(Sender: TObject);
    procedure oolbox1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure ExportToClipboard1Click(Sender: TObject);
    procedure addvectortestdatatomaincontainer1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure batchloadmultithreaded1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure N16bitsinglemonochromeRAW1Click(Sender: TObject);
    procedure Filterfileswithcurrentfilterheap1Click(Sender: TObject);
    procedure savetodifferentformat1Click(Sender: TObject);

  private
    OpenImagesOnStartup: Boolean;
    points: Array of TPoint;
    recordpoints: Boolean;
    CurrentLayerID: longint;
    animationbuf1: TBitmap;
    animationbuf2: TBitmap;
    animationbuf3: TBitmap;
    animationstatus: integer;
    jpg: TJPegImage;
    dropfilename : string;
    BlockMouseDownOnce: Boolean;
    TWAINAcquisitionTargets: Array[0..63] of TTWAINAcquisitionLayerInfo;
    TWAINAcquisitionTargetLength: longint;
    OnAfterTwainAcquisition: TNotificationNode;

    F2DialogRunning: Boolean;

    procedure AppendNewestLayerMenuItem(Sender:TObject=nil);
    procedure LoadFilesOnStartup   (var Message: TMessage); Message WM_LOADIMAGESONSTARTUP;
    procedure UpdateMainFormCaption(var Message: TMessage); Message WM_UPDATE_FORM_CAPTION;
    procedure OnTwainAcquire(Sender: TObject; const Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
    function TWAINAcquisitionTargetIdxByID(ID: DWORD):longint;
    procedure GLMMiddleMouseClick(Sender:TObject; X,Y: integer; Shift: TShiftState);
    procedure FillToolBox(Sender:TObject);
    procedure ActivateComponents(Sender: TObject);

    procedure UpdateMainFormCaptionEvent(Sender: TObject);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    glm: TGraphicLayerManager;
    moving: Boolean;
    modifying: Boolean;
    modifyindex : integer;
    startcursor,currcursor, startpos:TPoint;
    procedure UpdateCurrentLayerID(LayerID: longint);
    procedure UnsetTwainAcquireTargetLayer(SourceIndex: longint);
    function SetTwainAcquireTargetLayer(SourceIndex: longint;
       LayerID: longint): longint;

  end;

var
  GLWMainForm: TGLWMainForm;

const
  animblox = 3*4*2;

implementation

uses
  Unit4, panotrans;

const filters: array[0..7] of string =(
                    'JPeg image (*.jpg)','*.jpg',
                    '24 / 32 bit Bitmap(*.bmp)','*.bmp',
                    'PNG images (*.png)','*.png',
                    'all files (*.*)','*.*');

{$R *.dfm}



procedure TGLWMainForm.AppendNewestLayerMenuItem(Sender:TObject=nil);
var
  item: TMenuItem;
begin
  item := TMenuItem.Create(Self.Layers1);
  item.Caption := 'Layer '+inttostr(length(glm.Layers)-1)+' ['
                  + glm.Layers[(length(glm.Layers)-1)].LayerCaption+']';
  item.Tag := length(glm.Layers)-1;
  item.OnClick := Self.Layer01Click;
  Self.Layers1.Insert(Layers1.Count,item);
  CurrentLayerID := glm.Layers[length(glm.Layers)-1].LayerID;
  Self.Layer01Click(item);
end;



procedure TGLWMainForm.FormCreate(Sender: TObject);
var
  path, fn: string;
  i: integer;
begin
  animationstatus := 0;
  BlockMouseDownOnce := false;
  F2DialogRunning := False;

  self.Color := $707070;

  glm := TGraphicLayerManager.Create(Self);
  Self.OnPaint := glm.ClientPaint;
  glm.OnCustomPostRender := Self.RenderOverlay;
  glm.ZoomFactor := 1;
  glm.OnAddLayer := AppendNewestLayerMenuItem;
  glm.OnCurrentLayerChange := Self.Layer01Click;
  glm.OnMiddleMouseButtonClick := Self.GLMMiddleMouseClick;
  glm.OnDblClick := Switchtolayer1Click;
  glm.OnLayerChanges.InsertEvent(UpdateMainFormCaptionEvent); 

  // open files on startup or via drag&drop
  OpenImagesOnStartup := true; // one-time event
  DragAcceptFiles(self.Handle, true);

  recordpoints := false;

  path := ExtractFilePath(application.ExeName)+COMMON_DATA_PATH; // removed +'\'
  fn := path+'\GLW Readme.txt';

  savedialog1.Filter := filters[0] + '|' + filters[1] + '|' +
                        filters[2] + '|' + filters[3] + '|' +
                        filters[4] + '|' + filters[5] + '|' +
                        filters[6] + '|' + filters[7];
  savedialog1.Options := [ofEnableSizing, ofPathMustExist];
  opendialog1.Options := [ofEnableSizing];

  for i := 0 to length(self.TWAINAcquisitionTargets)-1 do // set to create a new layer on acquisition
  begin
    TWAINAcquisitionTargets[i].ReuseTarget := false;
    TWAINAcquisitionTargets[i].TargetLayerPresetID := -1;
    TWAINAcquisitionTargets[i].SourceIdentifier := 0;
  end;
  TWAINAcquisitionTargetLength := 0; // equivalent to used length in the static array

  self.OnAfterTwainAcquisition := TNotificationNode.Create;
end;





procedure TGLWMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 //
end;




procedure TGLWMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin

  if animationstatus = 1 then inc(animationstatus);

  glm.zoompoint.X := currcursor.X ;
  glm.zoompoint.Y := currcursor.Y ;

  // handle all other compnent events
  glm.OnParentKeyPress(Sender, Key);

  if key =#13 then
  begin
    if recordpoints and (length(points)=4) then
    begin
      recordpoints := false;
      if form2.ToolLinRegStatus = 2 then form2.StoreSourcePoints(points);
      if form2.ToolLinRegStatus = 3 then form2.StoreDestPoints(points);
      glm.ClientPaint(self);
    end;

    if (form3.ToolStatus = 2) and (length(points)=2) then
    begin
      recordpoints := false;
      form3.StoreOrientationPoints(points);
      glm.ClientPaint(self);
    end;
  end;

//  Self.Caption := 'GraphicLayerWindow - '+inttostr(round(100*glm.ZoomFactor))+'%';
  UpdateMainFormCaptionEvent(self);
end;




procedure TGLWMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function NearestPoint:integer; // returns array index, this is the job of a VisualOverlay Object!
  var
    newdist, sqdist,i : integer;
    q: TPoint;
  begin
    sqdist := 1000*1000;
    result := -1;
    for i := 0 to length(points)-1 do
    begin
      q := glm.GlobalCoordToScreen(points[i]);
      newdist := ((x-q.x)*(x-q.x)+(y-q.y)*(y-q.y));
      if newdist < sqdist then
      begin
        result := i;
        sqdist := newdist;
      end;
    end;
  end;

var
  p,q: TPoint;
  nearest: integer;
begin


  if BlockMouseDownOnce then // nasty messaging queue problem workaround
  begin
    BlockMouseDownOnce:= false;
    exit;
  end;

//  windows.Beep(2000,10);

  if animationstatus = 1 then inc(animationstatus);

  if ssLeft in Shift then // modify point or move workspace
  begin
    Nearest := NearestPoint;
    if (nearest = -1) then moving := true;
    if (nearest > -1) then
    begin
      q := glm.GlobalCoordToScreen(points[Nearest]);
      if (sqr(x-q.x)+sqr(y-q.y)) < 32*32 then // radius 32px
      begin
        modifying := true;
        modifyindex := nearest;
      end
      else  moving := true;
    end;
  end;

  startcursor := Point(X, Y);
  p := glm.ScreenToGlobalCoord(startcursor);
  startpos := glm.Origin;

  if (ssRight in Shift) then
  begin
    if recordpoints and (length(points) < 4) then // add point
    begin
      setlength(points,length(points)+1);
       points[length(points)-1] := p;
       glm.ClientPaint(self);
    end;

    if (form3.ToolStatus = 2) and (length(points) < 2)  then
    begin
       setlength(points,length(points)+1);
       points[length(points)-1] := p;
       glm.ClientPaint(self);
    end;
  end;

  if not modifying then // compatibility until stuff is fixed
  // reduce above functionality to forwarding. It's all GLM's business!
  glm.OnParentMouseDown(Sender,Button,Shift,X,Y);
end;




procedure TGLWMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
//  windows.Beep(5000,10);  
  currcursor := Point(X,Y);
  if moving then
  begin
    //p.X := startpos.X+(X-startcursor.X);
    //p.Y := startpos.Y+(Y-startcursor.Y);
    //glm.Origin := p;
  end;

  if modifying then
  begin
    points[modifyindex] := glm.ScreenToGlobalCoord(currcursor);
    glm.ClientPaint(nil);
  end;

  if not modifying then // compatibility until stuff is fixed
  // reduce above functionality to forwarding. It's all GLM's business!
  // if moving then // bridge while functionality is not fully migrated to glm
  glm.OnParentMouseMove(Sender,Shift,X,Y);
end;




procedure TGLWMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   moving := false;
   modifying := false;
   if recordpoints then
   begin
     if (length(points) = 4) and (round(anglesum(points)*180/pi) =720)then
        // 1080: wrong orientation
        //  720: intersection
        //  360: ok
     begin
        // application.MessageBox('angle sum fail',pchar(floattostr(anglesum(points)*180/pi)),0);
        SortToConvexPolygon(points);
     end;
     glm.ClientPaint(self);
   end;

  // reduce above functionality to forwarding. It's all GLM's business!
  glm.OnParentMouseUp(Sender,Button,Shift,X,Y);
end;




procedure TGLWMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
//  hwnd: THandle;
  pt: TSmallPoint;
  lparam: Integer absolute pt;
  dummy: TMessage;
begin
  if animationstatus = 1 then inc(animationstatus);

  if windows.WindowFromPoint(MousePos) <> application.MainForm.Handle then // no wheel events for other forms?
  begin
    //hwnd := windows.WindowFromPoint(MousePos);
    //pt := SmallPoint(MousePos.X, MousePos.Y);
    //SendMessage(hwnd,WM_MOUSEWHEEL,WORD(WheelDelta*WHEEL_DELTA),lparam);

    Handled := true;
    exit;
  end;

  glm.OnParentMouseWheel(Sender,Shift,WheelDelta,MousePos,Handled);
  self.UpdateMainFormCaption(dummy);

  handled := true;
end;


//--------------------------------------------------------------------------
//--------------------------------------------------------------------------


procedure TGLWMainForm.RenderOverlay(Sender:TObject);
var
  buf: TBitmap;
  i,l : integer;
  r : integer;
  p,q: TPoint;
  c: TPoint;
begin
   if not assigned(form3) then exit;
   if not assigned(Sender) then exit;
   
   //if length(glm.Layers) > 0 then
   //if glm.Layers[0].visible = false then exit; // render markers for layer0 only

   // general function: paints dots and lines
   buf := Sender as TBitmap;
 if recordpoints or (assigned(form3) and (form3.ToolStatus =2) )then // Error: read of address 0x0000000 (form3 not assigned)? -> 20110605
 begin
   buf.Canvas.Pen.Color := cllime;
   buf.Canvas.Brush.Color := clgray;

   l := length(points);
   if l < 1 then exit;
      p := glm.GlobalCoordToScreen(points[0]);

   buf.Canvas.MoveTo(p.x,p.y);
   for i := 1 to l do
   begin
      p := glm.GlobalCoordToScreen(points[i mod l]);
      buf.Canvas.LineTo(p.x,p.y);
      buf.Canvas.Ellipse(p.x-4,p.y-4,p.x+4,p.Y+4);
   end;
 end;

//----------------------------FORM2----------------------------------------------


 if assigned(form2) then
 if form2.ToolLinRegStatus = 4 then
 begin
 // source rect
   buf.Canvas.Pen.Color := clred;
   buf.Canvas.Brush.Color := clgray;
   buf.Canvas.Brush.Style := bssolid;

   l := length(form2.sourcepoints);
   if l < 1 then exit;
      p := glm.GlobalCoordToScreen(form2.sourcepoints[0]);

   buf.Canvas.MoveTo(p.x,p.y);
   for i := 1 to l do
   begin
      p := glm.GlobalCoordToScreen(form2.sourcepoints[i mod l]);
      buf.Canvas.LineTo(p.x,p.y);
   end;
 // dest rect
   buf.Canvas.Pen.Color := clblue;

   l := length(form2.targetpoints);
   if l < 1 then exit;
      p := glm.GlobalCoordToScreen(form2.targetpoints[0]);

   buf.Canvas.MoveTo(p.x,p.y);
   for i := 1 to l do
   begin
      p := glm.GlobalCoordToScreen(form2.targetpoints[i mod l]);
      buf.Canvas.LineTo(p.x,p.y);
   end;


 // correspondence lines
   buf.Canvas.Pen.Color := clFuchsia;

   l := length(form2.Sourcepoints);
   if l < 1 then exit;
   l := length(form2.targetpoints);
   if l < 1 then exit;

   for i := 0 to l-1 do
   begin
      p := glm.GlobalCoordToScreen(form2.Sourcepoints[i]);
      q := glm.GlobalCoordToScreen(form2.Targetpoints[i]);
      buf.Canvas.MoveTo(p.x,p.y);
      buf.Canvas.LineTo(q.x,q.y);
   end;

 end;
//----------------------------/FORM2---------------------------------------------



//----------------------------FORM3----------------------------------------------

 if assigned(form3) then
 if (form3.ToolStatus = 2) and (length(points)=2) then
 begin

  buf.Canvas.Pen.Color := cllime;
  buf.Canvas.Brush.Style := bsclear;

  p := glm.GlobalCoordToScreen(points[0]);
  q := glm.GlobalCoordToScreen(points[1]);

  buf.Canvas.MoveTo(p.x,p.y);
  buf.Canvas.LineTo(q.x,q.y);
  r := ceil(sqrt(sqr(p.X-q.X)+sqr(p.Y-q.Y)));
  buf.Canvas.Ellipse(p.X-r,p.Y -r,p.X+r,p.Y +r);
 end;


 if assigned(form3) then
 if (form3.ToolStatus = 4) and(length(points)=2) then
 begin
   buf.Canvas.Pen.Color := clblue;
   buf.Canvas.Brush.Style := bsclear;

  p := glm.GlobalCoordToScreen(form3.orientationpoints[0]);
  q := glm.GlobalCoordToScreen(form3.orientationpoints[1]);

  buf.Canvas.MoveTo(p.x,p.y);
  buf.Canvas.LineTo(q.x,q.y);
  r := ceil(sqrt(sqr(p.X-q.X)+sqr(p.Y-q.Y)));
  buf.Canvas.Ellipse(p.X-r,p.Y -r,p.X+r,p.Y +r);
 end;


//----------------------------/FORM3---------------------------------------------


  c.X := (GLWMainForm.ClientWidth - image1.Picture.Width ) div 2;
  c.Y := (GLWMainForm.ClientHeight- image1.Picture.Height) div 2;
  


if (self.animationstatus >0) and (timer1.Enabled) then
begin

   animationbuf3.Canvas.CopyRect(
      animationbuf1.Canvas.ClipRect,
      glm.imagebuffer.Canvas,
      rect(c,point(c.x+animationbuf1.Width,c.y+animationbuf1.Height)));

   GLWMainForm.Timer1Timer(glm);

end; // hm hm


end;


//--------------------------------------------------------------------------
//--------------------------------------------------------------------------




procedure TGLWMainForm.Button1Click(Sender: TObject);
//var
//  fn: string;
//  path : string;
begin
{  panel1.Hide;
  path := ExtractFilePath(application.ExeName)+COMMON_DATA_PATH; // removed '\'
  fn := path+'\GLW Readme.txt';
  panel1.Hide;
  if checkbox1.Checked then
  begin
     if not DirectoryExists(path) then MkDir(path);
     memo1.Lines.SaveToFile(fn);
  end; }
end;




procedure TGLWMainForm.Layer01Click(Sender: TObject);
var i: integer;
begin

  if length(glm.Layers) = 0 then exit;

  if sender is TMenuItem then
  begin
    for i := 0 to length(glm.Layers) -1 do
      glm.Layers[i].visible :=  (sender as TMenuItem).Tag = i;
    CurrentLayerID := glm.Layers[(sender as TMenuItem).Tag].LayerID;
    glm.OnLayerChanges.SharedEvent(self);
    glm.ClientPaint(self);
  end;

  if sender is TGraphicLayerManager then
  begin
    CurrentLayerID := glm.Layers[(sender as TGraphicLayerManager).CurrentLayer].LayerID;
  end;

end;




procedure TGLWMainForm.Close1Click(Sender: TObject);
begin
  TwainHelper.Terminate;
  application.Terminate;
end;




procedure TGLWMainForm.linearregistration1Click(Sender: TObject);
begin
  if form2.ToolLinRegStatus = 0 then
  begin
    Tools1.Enabled := false;
    form2.WorkingLayerID := CurrentLayerID;
    form2.OnToolAbort := AbortToolLinReg;
    form2.RemoteGLM := glm;
    form2.OnStartRecordPoints := StartRecordPoints;
    form2.Parent := GLWMainForm;
    form2.Left := 0;
    form2.Top := 0;
    form2.ResetTool;
    form2.Show;
  end;
end;




procedure TGLWMainForm.NewLayer1Click(Sender: TObject);
var
  dlg : TMultiFunctionalInputDialog;
  w, h: longint;
begin
  dlg := TMultiFunctionalInputDialog.Create;
  dlg.frm.Caption := 'New Layer';
  dlg.AddInputField('Width' ,@w,'numeric','','','1000');
  dlg.AddInputField('Height',@h,'numeric','','','1000');
  if dlg.Execute then
  begin
    glm.NewLayer(Rect(0,0,w,h),clwhite);
    //AppendNewestLayerMenuItem;
  end;
  dlg.Destroy;
end;




procedure TGLWMainForm.NewLayerfromfile1Click(Sender: TObject);
var
  idx : longint;
  loadbar : TVisualOverlayProgressBar;
  newidx : longint;
begin
  opendialog1.Options := [ofFileMustExist, ofForceShowHidden, ofAllowMultiSelect, ofEnableSizing];
  opendialog1.Filter := 'Standard Formats (*.bmp, *.jpg, *.jpeg, *.png)|*.bmp;*.jpg;*.jpeg;*.png|'+
                        'bitmap images (*.bmp)|*.bmp|'+
                        'JPG images (*.jpg, *.jpeg)|*.jpg;*.jpeg|'+
                        'PNG images (*.png)|*.png|'+
                        'all files (*.*)|*.*';

  if opendialog1.Execute then
  begin

    loadbar := TVisualOverlayProgressBar.Create(GLWMainForm.ClientRect);
    loadbar.Caption := 'loading image(s)  1/'+inttostr(OpenDialog1.Files.Count)+' ...';
    loadbar.min     := 0;
    loadbar.position:= 0;
    loadbar.max     := OpenDialog1.Files.Count;
    loadbar.Width   := 200;
    loadbar.Left    := (-loadbar.Width  + ClientWidth  ) div 2;
    loadbar.Top     := (-loadbar.Height + ClientHeight ) div 2;

    GLM.InsertVisualOverlayObject(loadbar);
    loadbar.Visible := true;
    GLM.ClientPaint(self);


    for idx := 0 to OpenDialog1.Files.Count - 1 do
    begin
      // new
      // GLM.FileImporter.Open(opendialog1.Files.Strings[idx]);

      // legacy, remove when FileImporter is fully implemented
      loadbar.Caption := 'loading image(s)  '+IntToStr(idx+1)+'/'+inttostr(OpenDialog1.Files.Count)+' ...';

      GLM.surpressrepaint;
      newidx := GLM.NewLayerLoadFromFile(0,0,opendialog1.Files.Strings[idx]);
      glm.CurrentLayer := newidx;
      loadbar.position := idx+1;
      UpdateMainFormCaptionEvent(self);
      GLM.ClientPaint(self);
    end;

    GLM.RemoveVisualOverlayObject(loadbar);
    loadbar.Destroy;
    sleep(100);
  end;

  glm.ClientPaint(self);
end;









Procedure TGLWMainForm.AbortToolLinReg(Sender: TObject);
begin
   Tools1.Enabled := true;
   setlength(points,0);
   glm.ClientPaint(self);
end;



Procedure TGLWMainForm.AbortToolWithPreviewHook(Sender: TObject);
begin
   Tools1.Enabled := true;
   glm.RemoveAllPreviewHooks;
   glm.OnLayerChanges.SharedEvent(self);
   glm.ClientPaint(self);
end;

Procedure TGLWMainForm.AbortStdTool(Sender: TObject);
begin
   Tools1.Enabled := true;
   glm.ClientPaint(self);
end;



Procedure TGLWMainForm.AbortToolPolToCart(Sender: TObject);
begin
   Tools1.Enabled := true;
   Deletecurrenlayer1.Enabled := true;
   CurrentLayerID := form3.WorkingLayerID; // inserting and deleting the preview layer
                                           // does mess things up a little.. so let's fix it.
                                           // deleting layers manually should be disabled.
   glm.ClientPaint(self);
end;




Procedure TGLWMainForm.AbortToolPixelBinner(Sender: TObject);
begin
   Tools1.Enabled := true;
   Deletecurrenlayer1.Enabled := true;
   CurrentLayerID := form4.WorkingLayerID;
   glm.ClientPaint(self);
end;




Procedure TGLWMainForm.StartRecordPoints(var pts: Array of TPoint);
var
  i: integer;
begin
   setlength(points,length(pts));
   for i := 0 to length(pts)-1 do
   begin
     points[i] := pts[i];
   end;
   recordpoints := true;
   glm.ClientPaint(self);
end;




procedure TGLWMainForm.Layers1Click(Sender: TObject);
var
  n: integer;
  ActualLayerItems: integer;

begin
  NewLayerfromclipboad1.Enabled := clipboard.HasFormat(CF_PICTURE);

  ActualLayerItems := 0;

  Deletecurrenlayer1.Enabled :=  length(glm.Layers) <> 0;
  SaveLayertofile1.Enabled   :=  length(glm.Layers) <> 0;
  duplicatelayer1.Enabled    :=  length(glm.Layers) <> 0;
  Deletecurrenlayer1.Visible :=  length(glm.Layers) <> 0;
  SaveLayertofile1.Visible   :=  length(glm.Layers) <> 0;
  duplicatelayer1.Visible    :=  length(glm.Layers) <> 0;
  SwitchToLayer1.Visible     :=  length(glm.Layers) <> 0;
  ChangeMaskLayer1.Visible   :=  length(glm.Layers) <> 0;
  MergeLayers1.Visible       :=  length(glm.Layers) <> 0;
  RenamecurrentLayer1.Visible:=  length(glm.Layers) <> 0;
  ExportToClipboard1.Visible :=  length(glm.Layers) <> 0; // decide by index, loop over all Layer1.items where tag = -1 ...

//  if length(glm.Layers) > 0 then
  for n := 0 to Layers1.Count -1 do
  begin
    if Layers1.Items[n].Tag <> -1 then
    begin
      inc(ActualLayerItems);
      if (Layers1.Items[n].Tag >= 0) and (Layers1.Items[n].Tag < Length(GLM.Layers)) then
      begin
        Layers1.items[n].Caption := 'Layer '+inttostr(Layers1.Items[n].Tag)+' ['
                    + glm.Layers[Layers1.Items[n].Tag].LayerCaption+']';
        Layers1.Items[n].Checked := glm.Layers[Layers1.Items[n].Tag].visible;
      end;
    end;
  end;

  for n := 0 to ActualLayerItems - length(glm.Layers)-1 do  // delete dead items
    Layers1.Delete(Layers1.Count-1);
end;



procedure TGLWMainForm.Deletecurrenlayer1Click(Sender: TObject);
begin
  glm.DeleteLayer(glm.LayerByID(CurrentLayerID));
  Layers1.Delete(Layers1.Count-1);
  if length(glm.Layers) <> 0 then
  begin
    glm.Layers[length(glm.Layers)-1].visible := true;
    CurrentLayerID := glm.Layers[length(glm.Layers)-1].LayerID; // maybe go to the subsequent layer, not to the end of all layers, change this
  end;
  glm.OnLayerChanges.SharedEvent(self);
  glm.ClientPaint(self);
end;



procedure TGLWMainForm.tools1Click(Sender: TObject);
var i : integer;
begin
  for i := 0 to tools1.Count -1 do
    if tools1.Items[i].Tag < 1000 then
    tools1.Items[i].enabled := length(glm.Layers) <> 0;
end;



procedure TGLWMainForm.polartocartesian1Click(Sender: TObject);
begin
  if form3.ToolStatus = 0 then
  begin
    Tools1.Enabled := false;
    form3.WorkingLayerID := CurrentLayerID;
    form3.OnToolAbort := AbortToolPolToCart;
    form3.RemoteGLM := glm;
    form3.OnStartRecordCircle := StartRecordCircle;
    form3.Parent := GLWMainForm;
    form3.Left := 0;
    form3.Top := 0;
    form3.ResetTool;
    form3.Show;
    Deletecurrenlayer1.Enabled := false; // messed up by Layers1Click()
    form3.OnUpdateCurrentLayer := UpdateCurrentLayerID;
  end;
end;



procedure TGLWMainForm.StartRecordCircle(var pts: Array of TPoint);
var
  i: integer;
begin
   setlength(points,length(pts));
   for i := 0 to length(pts)-1 do
   begin
     points[i] := pts[i];
   end;
 glm.ClientPaint(self); 
end;



procedure TGLWMainForm.SaveLayertofile1Click(Sender: TObject);
var
  CurrentLayerIndex: integer;
  ext: string;
label
  redo_dialog;
begin
  CurrentLayerIndex := glm.LayerByID(GLWMainForm.CurrentLayerID);

  if CurrentLayerIndex <> -1 then
  begin
    savedialog1.FileName := extractfilename(glm.layers[CurrentLayerIndex].LayerCaption);
redo_dialog:
    if savedialog1.Execute then
    begin
     ext := lowercase(ExtractFileExt(savedialog1.FileName));

     if (ext = '.bmp') or (ext = '.jpg') or (ext ='.jpeg') or (ext = '.png') then
       glm.SaveLayerToFile(CurrentLayerID,savedialog1.FileName,ext)
     else
       begin
         application.MessageBox(
           'Invalid file extension. Please use one of the enlisted file formats.',
           'invalid file extension',mb_OK or MB_ICONEXCLAMATION);
         goto redo_dialog;
       end;

    end;
  end;
end;



procedure TGLWMainForm.UpdateCurrentLayerID(LayerID: longint);
begin
  GLWMainForm.CurrentLayerID := LayerID;
end;



procedure TGLWMainForm.duplicatelayer1Click(Sender: TObject);
var
  srclayer: integer;
  dstlayer: integer;
begin
  srclayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if srclayer <> -1 then
  begin
    glm.surpressrepaint;
    with glm.layers[srclayer] do
    begin
      dstlayer := glm.NewLayer(Rect(
            Position.X,
            Position.Y,
            Position.X + data.Width,
            Position.Y + data.Height),0);
      glm.layers[dstlayer].data.Canvas.Draw(0,0,glm.layers[srclayer].data);
      glm.layers[dstlayer].LayerCaption := glm.layers[srclayer].LayerCaption+'$';
    end;
    glm.OnLayerChanges.SharedEvent(self);
    glm.ClientPaint(self);
  end;
end;



procedure TGLWMainForm.NewLayerfromclipboad1Click(Sender: TObject); // Probleme mit WinXP64 und Vista. Warum?
var
  CurrentLayer: integer;
  hbmp: HBitmap;
  tmp: TBitmap;
  s: string;
begin
  hbmp := Clipboard.GetAsHandle(CF_BITMAP);
  if hbmp = 0 then exit;
  tmp := TBitmap.Create;

  tmp.LoadFromClipboardFormat(CF_BITMAP,hbmp,0);

  if tmp.Width * tmp.Height = 0 then exit;

  tmp.PixelFormat := pf32bit;

  glm.surpressrepaint;
  CurrentLayer := glm.NewLayer(Rect(0,0,tmp.Width,tmp.Height),clblack);

  glm.Layers[CurrentLayer].data.Canvas.Draw(0,0,tmp);
  glm.Layers[CurrentLayer].data.PixelFormat := pf24bit;
  tmp.Free;

  DateTimeToString(s,'hh-mm-ss',gettime);
  glm.Layers[CurrentLayer].LayerCaption := 'clip_'+s;
  glm.OnLayerChanges.SharedEvent(self);
  glm.ClientPaint(self);
end;



procedure TGLWMainForm.Chrominance1Click(Sender: TObject);
var
  CurrentLayer :  integer;
  CurrentFilter:  integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if CurrentLayer = -1 then exit;
  CurrentFilter := glm.InsertFilter(CurrentLayer,FT_CHROMINANCE,nil,0);
  glwfilters.ExecuteFilterAction(glm.layers[CurrentLayer].data,
              glm.layers[CurrentLayer].FilterHeap[CurrentFilter]);
  glm.layers[CurrentLayer].FilterHeapCursor := CurrentFilter;
  glm.OnLayerChanges.SharedEvent(self);
  glm.ClientPaint(self);
end;



procedure TGLWMainForm.Filterfileswithcurrentfilterheap1Click(Sender: TObject);
var
  loadbar : TVisualOverlayProgressBar;
  targetdir: string;
begin
  opendialog1.Options := [ofFileMustExist, ofForceShowHidden, ofAllowMultiSelect, ofEnableSizing];
  opendialog1.Filter := 'Standard Formats (*.bmp, *.jpg, *.jpeg, *.png)|*.bmp;*.jpg;*.jpeg;*.png|'+
                        'bitmap images (*.bmp)|*.bmp|'+
                        'JPG images (*.jpg, *.jpeg)|*.jpg;*.jpeg|'+
                        'PNG images (*.png)|*.png|'+
                        'all files (*.*)|*.*';

  if opendialog1.Execute then
  begin

    loadbar := TVisualOverlayProgressBar.Create(GLWMainForm.ClientRect);
    loadbar.Caption := 'processing image(s)  1/'+inttostr(OpenDialog1.Files.Count)+' ...';
    loadbar.min     := 0;
    loadbar.position:= 0;
    loadbar.max     := OpenDialog1.Files.Count;
    loadbar.Width   := 320;
    loadbar.Left    := (-loadbar.Width  + ClientWidth  ) div 2;
    loadbar.Top     := (-loadbar.Height + ClientHeight ) div 2;

    GLM.InsertVisualOverlayObject(loadbar);
    loadbar.Visible := true;
    GLM.ClientPaint(self);

    targetdir := GLMFilterFileProcessor.BatchFilterFiles(
                OpenDialog1.Files,
                GLM.Layers[GLM.CurrentLayer].LayerID,
                loadbar,
                glm);

    GLM.RemoveVisualOverlayObject(loadbar);
    loadbar.Destroy;
    sleep(200);

    glm.ClientPaint(self);
    ShellExecute(0,'open',PChar(targetdir),nil,nil,SW_SHOWMAXIMIZED);
  end;



end;



procedure TGLWMainForm.Filters1Click(Sender: TObject);
var
  CurrentLayer : integer;
  heap, cur    : integer;
  en           : boolean;
begin

  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  en := CurrentLayer <> -1;

  self.manipulation1.Enabled       := en;
  self.Adjustments1.Enabled        := en;
  self.externalprocessing1.Enabled := en;

  dispose1.Enabled              := false;
  gobackwardsundo1.Enabled      := false;
  goforwardredo1.Enabled        := false;
  flattenfilterhistory1.Enabled := false;
  restoreimageundoall1.Enabled  := false;
  Filterfileswithcurrentfilterheap1.Enabled    := False;

  reimportfromRAWand1.Visible := false;

  if not en then exit;

  reimportfromRAWand1.Visible := (GLM.Layers[CurrentLayer].SourceIsRawFile);

  cur  := glm.Layers[CurrentLayer].FilterHeapCursor;
  heap := length(glm.Layers[CurrentLayer].FilterHeap)-1;

  dispose1.Enabled := (cur < heap);
  gobackwardsundo1.Enabled      := (cur > -1)   and (heap <> -1);
  goforwardredo1.Enabled        := (cur < heap) and (heap <> -1);
  flattenfilterhistory1.Enabled :=                  (heap <> -1);
  restoreimageundoall1.Enabled  := (cur > -1)   and (heap <> -1);
  Filterfileswithcurrentfilterheap1.Enabled    := (cur > -1)   and (heap <> -1);
end;



procedure TGLWMainForm.gobackwardsundo1Click(Sender: TObject);
var
  CurrentLayer :  integer;
  i: integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if CurrentLayer = -1 then exit;
  glm.Layers[CurrentLayer].data.Width := glm.Layers[CurrentLayer].rawdata.Width;
  glm.Layers[CurrentLayer].data.Height:= glm.Layers[CurrentLayer].rawdata.Height;
  glm.Layers[CurrentLayer].data.Canvas.Draw(0,0,glm.Layers[CurrentLayer].rawdata);
  for i := 0 to glm.Layers[CurrentLayer].FilterHeapCursor -1 do
  begin
    glwfilters.ExecuteFilterAction(glm.layers[CurrentLayer].data,
              glm.layers[CurrentLayer].FilterHeap[i]);
  end;
  dec(glm.Layers[CurrentLayer].FilterHeapCursor);
  glm.OnLayerChanges.SharedEvent(self);
  glm.ClientPaint(self);
end;



procedure TGLWMainForm.goforwardredo1Click(Sender: TObject);
var
  CurrentLayer :  integer;
  CurrentFilter: integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  CurrentFilter := glm.layers[CurrentLayer].FilterHeapCursor;
  glwfilters.ExecuteFilterAction(glm.layers[CurrentLayer].data,
              glm.layers[CurrentLayer].FilterHeap[CurrentFilter+1]);
  inc(glm.layers[CurrentLayer].FilterHeapCursor);
  glm.OnLayerChanges.SharedEvent(self);
  glm.ClientPaint(self);
end;



procedure TGLWMainForm.flattenfilterhistory1Click(Sender: TObject);
var
  CurrentLayer :  integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if currentlayer = -1 then exit;
  glm.RemoveAllFilters(CurrentLayer, true);
  glm.OnLayerChanges.SharedEvent(self);
end;



procedure TGLWMainForm.dispose1Click(Sender: TObject);
var
  CurrentLayer :  integer;
  i: integer;
  heap, cur: integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if CurrentLayer = -1 then exit;
  cur := glm.Layers[CurrentLayer].FilterHeapCursor;
  heap := length(glm.Layers[CurrentLayer].FilterHeap)-1;

  for i := heap downto cur+1 do
  begin
    glm.RemoveFilter(CurrentLayer,i);
  end;

end;



procedure TGLWMainForm.batchloadmultithreaded1Click(Sender: TObject);
//var
//  k: longint;
begin
{  for k := 1 to ParamCount do
    GLM.FileImporter.Open(Paramstr(k)); }
end;



procedure TGLWMainForm.brightness1Click(Sender: TObject);
var
  CurrentLayer :  integer;
  CurrentFilter:  integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if CurrentLayer = -1 then exit;
  CurrentFilter := glm.InsertFilter(CurrentLayer,FT_DEPLETE_COLORS,nil,0);
  glwfilters.ExecuteFilterAction(glm.layers[CurrentLayer].data,
              glm.layers[CurrentLayer].FilterHeap[CurrentFilter]);
  glm.layers[CurrentLayer].FilterHeapCursor := CurrentFilter;
  glm.OnLayerChanges.SharedEvent(self);
  glm.ClientPaint(self);
end;



procedure TGLWMainForm.invert1Click(Sender: TObject);
var
  CurrentLayer :  integer;
  CurrentFilter:  integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if CurrentLayer = -1 then exit;
  CurrentFilter := glm.InsertFilter(CurrentLayer,FT_INVERT,nil,0);
  glwfilters.ExecuteFilterAction(glm.layers[CurrentLayer].data,
              glm.layers[CurrentLayer].FilterHeap[CurrentFilter]);
  glm.layers[CurrentLayer].FilterHeapCursor := CurrentFilter;
  glm.ClientPaint(self);
  glm.OnLayerChanges.SharedEvent(self);
end;



procedure TGLWMainForm.pixelbinning1Click(Sender: TObject);
begin
  if form4.ToolStatus = 0 then
  begin
    Tools1.Enabled := false;
    form4.WorkingLayerID := CurrentLayerID;
    form4.OnToolAbort := AbortToolPixelBinner;
    form4.RemoteGLM := glm;
    form4.Parent := GLWMainForm;
    form4.Left := 0;
    form4.Top := 0;
    form4.ResetTool;
    form4.OnUpdateCurrentLayer := UpdateCurrentLayerID;
    Deletecurrenlayer1.Enabled := false;
    form4.Show;
  end;
end;



procedure TGLWMainForm.brightnesscontrast1Click(Sender: TObject);
begin
  if form5.ToolStatus = 0 then
  begin
    Tools1.Enabled := false;
    form5.WorkingLayerID := CurrentLayerID;
    form5.OnToolAbort := AbortToolWithPreviewHook;
    form5.RemoteGLM := glm;
    form5.Parent := GLWMainForm;
    form5.Left := 0;
    form5.Top := 0;
    form5.ResetTool;
    form5.OnUpdateCurrentLayer := UpdateCurrentLayerID;
    Deletecurrenlayer1.Enabled := false;
    form5.Show;
  end;
end;



procedure TGLWMainForm.restoreimageundoall1Click(Sender: TObject);
var
  CurrentLayer :  integer;
begin
  CurrentLayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if CurrentLayer = -1 then exit;
  if assigned(glm.Layers[CurrentLayer].rawdata) then
  begin
    glm.Layers[CurrentLayer].data.Width := glm.Layers[CurrentLayer].rawdata.Width;
    glm.Layers[CurrentLayer].data.Height:= glm.Layers[CurrentLayer].rawdata.Height;
    glm.Layers[CurrentLayer].data.Canvas.Draw(0,0,glm.Layers[CurrentLayer].rawdata);
  end;
  glm.OnLayerChanges.SharedEvent(self);
  glm.ClientPaint(self);
  glm.Layers[CurrentLayer].FilterHeapCursor := -1;
end;



procedure TGLWMainForm.Timer1Timer(Sender: TObject);
var
  c: TPoint;
  i,x,y: integer;
begin
  c.X := (GLWMainForm.ClientWidth- image1.Picture.Width) div 2;
  c.Y := (GLWMainForm.ClientHeight- image1.Picture.Height) div 2;

 if animationstatus >0 then
 begin
   for y := 0 to animationbuf1.Height div animblox-1 do
   for x := 0 to animationbuf1.Width div animblox -1 do
     MMXBlend.blendRect(animationbuf1,
        animationbuf3,
        animationbuf2,
        rect(x*animblox,y*animblox,(x+1)*animblox,(y+1)*animblox),
        min(255,animationstatus+max(0,round(-50 + 200*sin(x/10+y/20+gettickcount/1000)))));

   if sender <> glm then
     GLWMainForm.Canvas.Draw(c.x,c.y,animationbuf1)
   else
     glm.imagebuffer.Canvas.Draw(c.x,c.y,animationbuf1);

   if animationstatus >1 then animationstatus := animationstatus + 15;
   if animationstatus > 550 then
   begin
     timer1.Enabled := false;
     glm.ClientPaint(self);
     animationbuf1.Free;
     animationbuf1 := nil;
     animationbuf2.Free;
     animationbuf2 := nil;
     animationbuf3.Free;
     animationbuf3 := nil;
     jpg.Free;
     jpg := nil;
   end;
 end;

 if animationstatus = 0 then
 begin
   if not assigned(animationbuf1) then
   animationbuf1 := TBitmap.Create;
   animationbuf1.PixelFormat := pf24bit;
   animationbuf1.Canvas.Brush.Color := 0;
   animationbuf1.Width := ceil(image1.Picture.Width/animblox)*animblox;
   animationbuf1.Height := ceil(image1.Picture.Height/animblox)*animblox;

   if not assigned(animationbuf2) then
   animationbuf2 := TBitmap.Create;
   animationbuf2.PixelFormat := pf24bit;
   animationbuf2.Width := animationbuf1.Width;
   animationbuf2.Height := animationbuf1.Height;

   if not assigned(animationbuf3) then
   animationbuf3 := TBitmap.Create;
   animationbuf3.PixelFormat := pf24bit;
   animationbuf3.Width := animationbuf1.Width;
   animationbuf3.Height := animationbuf1.Height;

   animationbuf3.Canvas.CopyRect(
      animationbuf1.Canvas.ClipRect,
      glm.imagebuffer.Canvas,
      rect(c,point(c.x+animationbuf1.Width,c.y+animationbuf1.Height)));

   jpg := TJPegImage.Create;
   jpg.Assign(image1.Picture);
   animationbuf2.Assign(jpg);
   animationstatus := 1;

   for i := 40 downto 1 do
   begin
     for y := 0 to animationbuf1.Height div animblox-1 do
     for x := 0 to animationbuf1.Width div animblox -1 do
     MMXBlend.blendRect(animationbuf1,animationbuf3,animationbuf2,rect(x*animblox,y*animblox,(x+1)*animblox,(y+1)*animblox),min(255,i*10+max(0,round(-50 + 200*sin(x/10+y/20+gettickcount/1000)))));
     GLWMainForm.Canvas.Draw(c.x,c.y,animationbuf1);
     sleep(10);
   end;
 end;

 if animationstatus = -1 then
 begin
   animationstatus := 0;
   timer1.Enabled := false;
 end;
end;



procedure TGLWMainForm.about1Click(Sender: TObject);
begin
  animationstatus := 0;
  timer1.Enabled := true;
end;


procedure TGLWMainForm.WMDROPFILES (var Msg: TMessage);
var
  i, anzahl, size: integer;
  Dateiname: PChar;
  loadbar : TVisualOverlayProgressbar;
begin
  inherited;

  Dateiname := '';
  anzahl := DragQueryFile(Msg.WParam, $FFFFFFFF, Dateiname, 1023);

  loadbar := TVisualOverlayProgressBar.Create(GLWMainForm.ClientRect);
  loadbar.Caption := 'loading image(s)  1/'+inttostr(anzahl)+' ...';
  loadbar.min     := 0;
  loadbar.position:= 0;
  loadbar.max     := anzahl;
  loadbar.Width   := 200;
  loadbar.Left    := (-loadbar.Width  + ClientWidth  ) div 2;
  loadbar.Top     := (-loadbar.Height + ClientHeight ) div 2;

  GLM.InsertVisualOverlayObject(loadbar);
  loadbar.Visible := true;
  GLM.ClientPaint(self);

  for i := 0 to (anzahl - 1) do
  begin
    try
      size := DragQueryFile(Msg.WParam, i , nil, 0)+1;
      Dateiname:= StrAlloc(size);
      DragQueryFile(Msg.WParam,i , Dateiname, size);
      dropfilename := Dateiname;

      loadbar.Caption := 'loading image(s)  '+IntToStr(i+1)+'/'+inttostr(anzahl)+' ...';
      GLM.surpressrepaint;
      FormFileDrop(self);
      loadbar.position := i+1;
      GLM.ClientPaint(self);

      StrDispose(Dateiname);
    except
      // la la la
    end;
  end;
  DragFinish(Msg.WParam);

  GLM.RemoveVisualOverlayObject(loadbar);
  loadbar.Destroy;
  sleep(200);

  glm.ClientPaint(self);
end;



procedure TGLWMainForm.FormFileDrop(Sender:TObject);
begin
  if FileExists(dropfilename) then
    glm.NewLayerLoadFromFile(0,0,dropfilename);
end;




procedure TGLWMainForm.spatialbrightnessbalancer1Click(Sender: TObject);
begin
  if bbalancerform.status = 0 then
  begin
    Tools1.Enabled := false;
    bbalancerform.WorkingLayerID := CurrentLayerID;
    bbalancerform.MaskLayerID := glm.Layers[glm.LayerByID(CurrentLayerID)].MaskLayerID;
    bbalancerform.RemoteGLM   := glm;
    bbalancerform.Parent      := GLWMainForm;
    bbalancerform.OnToolAbort := self.AbortStdTool;
    bbalancerform.Left := 0;
    bbalancerform.Top := 0;
    bbalancerform.InitTool;
    bbalancerform.Show;
  end;
end;



procedure TGLWMainForm.manipulation1Click(Sender: TObject);
begin
  spatialbrightnessbalancer1.Enabled := (bbalancerform.status = 0);
end;



procedure TGLWMainForm.Switchtolayer1Click(Sender: TObject);
var
  i: integer;
begin
  // prepare Layer Selection Dialog

  if length(GLM.Layers) = 0 then exit;
  if GLM.TerminateLayerSelectionDialog(Sender) then exit;

  for i := 0 to length(GLM.Layers)-1 do
    GLM.Layers[i].SelectableInDialog := true;

  SwitchToLayer1.Enabled   := false;
  ChangeMaskLayer1.Enabled := false;

  // execute Layer Selection Dialog
  if GLM.ExecuteLayerSelectionDialog(CurrentLayerID,false,false,self) then
  if GLM.LayerByID(GLM.LayerSelectionID) <> -1 then
  begin
    CurrentLayerID := GLM.LayerSelectionID;
    GLM.FocusLayer(GLM.LayerByID(CurrentLayerID));
  end;

  // clean up
  SwitchToLayer1.Enabled   := true;
  ChangeMaskLayer1.Enabled := true;
  
  windows.PostMessage(application.MainForm.Handle,WM_LBUTTONUP,0,0);
  BlockMouseDownOnce := true;
end;



procedure TGLWMainForm.Explore1Click(Sender: TObject);
begin
    LayerExplorerForm.RemoteGLM := glm;
    LayerExplorerForm.Parent    := GLWMainForm;
    LayerExplorerForm.Left      := GLWMainForm.ClientWidth - LayerExplorerForm.Width;
    LayerExplorerForm.Top       := 0;
    LayerExplorerForm.Show;
end;



procedure TGLWMainForm.Changemasklayer1Click(Sender: TObject);
var
  srclayer: integer;
begin
  srclayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
  if srclayer = -1 then exit;
  removemasklayerentry1.Enabled := glm.Layers[srclayer].MaskLayerID <> -1;
  self.switchtomasklayer1.Enabled := glm.Layers[srclayer].MaskLayerID <> -1;
  undousageasmasklayer1.Enabled := glm.Layers[srclayer].UsedAsMaskLayer;
end;



procedure TGLWMainForm.selectmasklayer1Click(Sender: TObject);
var
  i: integer;
  srclayer: integer;
  CurrentLayerIdx : longint;
begin
//   BlockMouseDownOnce := true;

  CurrentLayerIdx := glm.LayerByID(CurrentLayerID);
  if CurrentLayerIdx = -1 then exit;

  for i := 0 to length(GLM.Layers)-1 do
  begin
    GLM.Layers[i].SelectableInDialog :=
     (GLM.Layers[i].data.Width  = GLM.Layers[CurrentLayerIdx].data.Width  ) and
     (GLM.Layers[i].data.Height = GLM.Layers[CurrentLayerIdx].data.Height );
  end;

  if GLM.ExecuteLayerSelectionDialog(CurrentLayerID,false) then
  begin
    srclayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
    if srclayer <> -1 then
    begin
      glm.Layers[srclayer].MaskLayerID := GLM.LayerSelectionID;
      glm.UpdateMaskLayerUsageInformation;
      glm.OnLayerChanges.SharedEvent(self);
    end;
  end;

end;



procedure TGLWMainForm.removemasklayerentry1Click(Sender: TObject);
var
  srclayer: integer;
begin
    srclayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
    if srclayer <> -1 then
    begin
      glm.Layers[srclayer].MaskLayerID := -1;
      glm.UpdateMaskLayerUsageInformation;
      glm.OnLayerChanges.SharedEvent(self);
    end;
end;



procedure TGLWMainForm.switchtomasklayer1Click(Sender: TObject);
var
  srclayer: integer;
begin
    srclayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
    if srclayer <> -1 then
    begin
      glm.FocusLayer(glm.LayerByID(glm.Layers[srclayer].MaskLayerID));
    end;
end;



procedure TGLWMainForm.MergeLayers1Click(Sender: TObject);
var
  rct: TRect;
  offset: TPoint;
  i: integer;
  newlayer: Longint;
  s: string;
begin
// leave alpha channel away, just merge the images
  glm.UpdateMaskLayerUsageInformation;

  if length(glm.Layers) =0 then exit;

  rct := LayerBoundsRect(glm.Layers[0]);
  for i := 1 to length(glm.Layers)-1 do
    rct := glmfunctionality.FrameRect(rct,LayerBoundsRect(glm.Layers[i]));
  offset := rct.TopLeft;

  glm.surpressrepaint;
  newlayer := glm.NewLayer(rct,clWhite);
  DateTimeToString(s,'hh-mm-ss',gettime);
  glm.Layers[newlayer].LayerCaption := 'merge_'+s;
  // render
    for i := 0 to length(glm.Layers)-2 do
      if not glm.Layers[i].UsedAsMaskLayer then
      begin
        if glm.LayerById(glm.Layers[i].MaskLayerID) <> - 1 then
        begin // render with tranparency
          mmxblend.DrawAlphaRect(
            glm.Layers[newlayer].data,
            rect(rct.Left-offset.X,rct.Top-offset.Y,rct.Right-offset.X, rct.Bottom - offset.Y),
            glm.Layers[i].data,
            glm.Layers[glm.LayerById(glm.Layers[i].MaskLayerID)].data);
        end
        else
        begin // just blit
         glm.Layers[newlayer].data.Canvas.Draw(
          glm.Layers[i].Position.X-offset.X,
          glm.Layers[i].Position.Y-offset.Y,
          glm.Layers[i].data);
        end;
      end;
  // render done
  glm.ClientPaint(self);
  glm.OnLayerChanges.SharedEvent(self);
end;



procedure TGLWMainForm.radialdistortioncorrection1Click(Sender: TObject);
begin
  if RadialDistortionCorrectionForm.toolstatus = 0 then
  begin
    Tools1.Enabled := false;
    Deletecurrenlayer1.Enabled := false;
    with RadialDistortionCorrectionForm do
    begin
      WorkingLayerID := CurrentLayerID;
      MaskLayerID := glm.Layers[glm.LayerByID(CurrentLayerID)].MaskLayerID;
      RemoteGLM := glm;
      Parent := GLWMainForm;
      // OnToolAbort := self.AbortStdTool;
      OnToolAbort := AbortToolWithPreviewHook;
      OnUpdateCurrentLayer := UpdateCurrentLayerID;
      Left := 0;
      Top := 0;
      ResetTool;
      Show;
    end;
  end;
end;



procedure TGLWMainForm.FormResize(Sender: TObject);
begin
  if assigned(glm) then
  begin
    glm.OnParentResize(self);
  end;
end;



procedure TGLWMainForm.Action11Click(Sender: TObject);
var
  vo : TVisualOverlayPanel;
begin
  self.Action11.Enabled := false;
  vo := TVisualOverlayPanel.Create(self.ClientRect);
  vo.Left    := 2;
  vo.Top     := 2;
  vo.Width   := GLWMainForm.ClientWidth -4;
  vo.Height  := GLWMainForm.ClientHeight  -4;
  vo.Anchors := [akTop, akLeft, akRight, akBottom];
  vo.OnClick := self.about1Click;
  glm.InsertVisualOverlayObject(vo);
  vo.Visible := true;
end;





procedure TGLWMainForm.WndProc(var Message: TMessage);
begin
  // remove the background erase message because
  // glm will overwrite the area in the subsequent
  // WM_PAINT step, thus avoid flickering
  if Message.Msg <> WM_ERASEBKGND then
    inherited WndProc(Message);
end;



procedure TGLWMainForm.sphericalpanoramatransform1Click(Sender: TObject);
begin
    // Tools1.Enabled := false;
    Deletecurrenlayer1.Enabled := false;
    with panotransform do
    begin
      WorkingLayerID := CurrentLayerID;
      MaskLayerID := glm.Layers[glm.LayerByID(CurrentLayerID)].MaskLayerID;
      RemoteGLM := glm;
      Parent := GLWMainForm;
      OnToolAbort := AbortToolWithPreviewHook;
      OnUpdateCurrentLayer := UpdateCurrentLayerID;
      Left := 0;
      Top := 0;
      Show;
    end;
end;



procedure TGLWMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Panotransform.CannotTerminateMainApp then CanClose := false;

  if CanClose then
    try    GLWMainForm.Hide;
    except Windows.PostMessage(GLWMainForm.Handle,WM_SHOWWINDOW, 0, SW_HIDE);
    end;
end;

procedure TGLWMainForm.openwith1Click(Sender: TObject);
begin
  glm.CallExternalEditor(CurrentLayerID,'','');
end;



procedure TGLWMainForm.closeexternalprocessingqueries1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to length(GLM.Layers)-1 do
    GLM.Layers[i].SelectableInDialog := assigned(GLM.Layers[i].ExternalEditWatcher);

  if GLM.ExecuteLayerSelectionDialog(CurrentLayerID,true) then
  begin
    for i := 0 to length(GLM.Layers)-1 do
      if GLM.Layers[i].IsSelectedInDialog then
        GLM.ReleaseExternalEditor(GLM.Layers[i].LayerID);
  end;

  // BlockMouseDownOnce := true; no dblclick selection
end;



procedure TGLWMainForm.undousageasmasklayer1Click(Sender: TObject);
var
  srclayer: integer;
  removecnt: integer;
  i: integer;
begin
    srclayer := glm.LayerByID(GLWMainForm.CurrentLayerID);
    removecnt := 0;
    if srclayer <> -1 then
    begin
      for i := 0 to length(glm.Layers)-1 do
      begin
        if i <> srclayer then
          if glm.Layers[i].MaskLayerID = glm.Layers[srcLayer].LayerID then
          begin
             glm.Layers[i].MaskLayerID := -1;
             inc(removecnt);
          end;
      end;
      glm.UpdateMaskLayerUsageInformation;
      glm.OnLayerChanges.SharedEvent(self);
      if removecnt <> 0 then
        MessageBox(0,PChar('The current layer has been used '+inttostr(removecnt)+' time(s)'#13#10+
        'as mask layer. It is a normal layer now.'),
        'dependencies as a mask layer have been cleared.' ,MB_ICONINFORMATION or MB_OK);
    end;
end;


procedure TGLWMainForm.openinfolder1Click(Sender: TObject);
begin
  glm.CallExternalEditor(CurrentLayerID,'$Explorer$','');
end;



procedure TGLWMainForm.Bar1Click(Sender: TObject);
var
  mid: TMultiFunctionalInputDialog;
  i: integer;
var
  str1 : string;
  ar: real;
begin
  str1 := 'You need to specify the parameters of the panorama (horizontal / vertial field)'#13#10+
    'and aspect ratio of the projected crop portal.';
  mid := TMultiFunctionalInputDialog.Create;
  mid.frm.Caption := 'Panorama parameters (rectilinear portal editor)';

  mid.AddInputField('Information about the settings below ...',nil,'info','','',str1);
  mid.AddInputField( 'aspect ratio   100 : ',@i,'trackbar','20','500','100');
  mid.AddInputField( 'aspect ratio',@ar,'combo','2:1','','3:1|2:1|3:2|4:3|6:5|1:1|5:6|3:4|2:3|1:2|1:3');
  mid.AddInputField( 'save to disk ...',@str1,'save');

  mid.Execute;
end;



procedure TGLWMainForm.rectilinearmanipulationportal1Click(Sender: TObject);
begin
  PanoRectTool.RemoteGLM := GLM;
  PanoRectTool.WorkingLayerID := CurrentLayerID;
  PanoRectTool.ToolExternalCleanup := self.AbortStdTool;
//  tools1.Enabled := false;
  PanoRectTool.Execute(CurrentLayerID);
end;



procedure TGLWMainForm.LoadFilesOnStartup(var Message: TMessage);
var
  k : integer;
  loadbar : TVisualOverlayProgressBar;
  //maxw, maxh: integer;
  newidx : integer;
begin
  if not OpenImagesOnStartup then exit;
  OpenImagesOnStartup := false;
  if ParamCount = 0 then exit;

  //////////////
  ///
  //for k := 1 to ParamCount do
  //  GLM.FileImporter.Open(Paramstr(k));
  // all the stuff below will vanish once this works!
  ///
  //////////////


  loadbar := TVisualOverlayProgressBar.Create(GLWMainForm.ClientRect);
  loadbar.Caption := 'loading images ...';
  loadbar.min     := 0;
  loadbar.max     := ParamCount;
  loadbar.Width   := 200;
  loadbar.Left    := (-loadbar.Width + ClientWidth )   div 2;
  loadbar.Top     := (-loadbar.Height + ClientHeight ) div 2;

  GLM.InsertVisualOverlayObject(loadbar);
  loadbar.Visible := true;
  GLM.ClientPaint(self);

  //maxw := 1; maxh := 1;
  newidx := -1;
  for k := 1 to ParamCount do
  begin
    loadbar.Caption := 'loading image(s)  '+IntToStr(k)+'/'+inttostr(ParamCount)+' ...';
    glm.surpressrepaint;
    newidx := glm.NewLayerLoadFromFile(0,0,paramstr(k));
    glm.CurrentLayer := newidx;
    loadbar.position := k;
    UpdateMainFormCaptionEvent(self);
  end;

  if newidx <> -1 then // resize to topmost layer
    begin
      while (GLM.Layers[newidx].data.Width  * GLM.ZoomFactor > ClientWidth ) or
            (GLM.Layers[newidx].data.Height * GLM.ZoomFactor > ClientHeight) do
      begin
        glm.surpressrepaint;
        GLM.ZoomFactor  := GLM.ZoomFactor / 1.1;
      end;
    end;


  if length(glm.layers) = 0 then
      glm.ClientPaint(self)
    else
      sleep(100);

  GLM.RemoveVisualOverlayObject(loadbar);
  loadbar.Destroy;
end;



procedure TGLWMainForm.ActivateComponents(Sender: TObject);
begin
  GLMVectorEditor.VectorEditorDesign.RegisterToGLM(self.glm);
  ToolBox.ToolBoxForm.OnRequestToolBoxFill := FillToolBox;
  ToolBoxForm.OnShowEvent.InsertEvent(
    GLMVectorEditor.VectorEditorDesign.SynchronizeTree); // for some reason, the TTreeView component looks empty altough
                                                         // the nodes are being created, so either it's Windows 7 messing
                                                         // up the refresh or some event messages get lost / window states
                                                         // of controls are invalid (grrrr!)
                                                         // refresh this way. It's not beautiful, but it does the job.


  if self.OpenImagesOnStartup then
    PostMessage(self.Handle,WM_LOADIMAGESONSTARTUP,0,0);

  Application.ProcessMessages;
end;


procedure TGLWMainForm.FillToolBox(Sender:TObject);
  procedure IntegrateGroupBoxes(source: TWinControl; ToolBoxTab: longint);
  var i: integer;
  begin
    for i := 0 to source.ControlCount -1 do // remember controls are being removed
    begin
      if (Source.Controls[0] is TGroupBox) then
        ToolBoxForm.InsetGroupBox((Source.Controls[0] as TGroupBox), ToolBoxTab);
    end;
  end;

begin

// Fill ToolBox
  IntegrateGroupBoxes(VectorEditorDesign, 3);
  VectorEditorDesign.SetHoverEventsToStatusBarUpdate(
                          ToolBoxForm.StatusBarMouseMove);
  VectorEditorDesign.GroupBox2.Align := alClient;

  IntegrateGroupBoxes(ToolBoxDesignFrame.NavigatorBoxes, 15);
end;



procedure TGLWMainForm.ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
var
  i: integer;
begin
  if (msg.wParam = VK_TAB) and (msg.message = WM_KEYDOWN)  then
  begin
      for i := 0 to length(GLM.Layers)-1 do
        GLM.Layers[i].SelectableInDialog := true;

      SwitchToLayer1.Enabled := false;
      ChangeMaskLayer1.Enabled := false;
      if GLM.ExecuteLayerSelectionDialog(CurrentLayerID,false,true) then
      begin
        CurrentLayerID := GLM.LayerSelectionID;
        GLM.FocusLayer(GLM.LayerByID(CurrentLayerID));
      end;
      SwitchToLayer1.Enabled   := true;
      ChangeMaskLayer1.Enabled := true;
  end;
end;



procedure TGLWMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  glm.OnParentKeyDown(Sender, Key, Shift);
end;



procedure TGLWMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  glm.OnParentKeyUp(Sender, Key, Shift);
end;



procedure TGLWMainForm.NewLayerfromTWAINsource1Click(Sender: TObject);
var
  candotwain: Boolean;
begin
  // check availability
  candotwain := TwainHelper.MainTwainSourceAcquired;
  
  TWAINtransfermode.Enabled           := candotwain;
  TWAINimportimagewithUI1.Enabled     := candotwain;
  TWAINimportimageAuto1.Enabled       := candotwain;
  TWAINSelectDeviceParameters.Enabled := candotwain;
end;



procedure TGLWMainForm.TWAINimportimagewithUI1Click(Sender: TObject);
begin
  TwainHelper.ImportWithUI(OnTwainAcquire);
end;



procedure TGLWMainForm.TWAINimportimageAuto1Click(Sender: TObject);
begin
  TwainHelper.StealthImport(OnTwainAcquire);
end;




procedure TGLWMainForm.RenamecurrentLayer1Click(Sender: TObject);
var
  dlg : TMultiFunctionalInputDialog;
  s:string;
  CurrentLayerIndex: longint;
begin
  if F2DialogRunning then exit;
  
  CurrentLayerIndex := glm.LayerByID(GLWMainForm.CurrentLayerID);

  if CurrentLayerIndex <> -1 then
  begin
    dlg := TMultiFunctionalInputDialog.Create;
    dlg.frm.Caption := 'Change Layer name';
    dlg.AddInputField('layer name' ,@s,'string','','',glm.Layers[CurrentLayerIndex].LayerCaption);
    F2DialogRunning := True;
    if dlg.Execute then
    begin
      glm.Layers[CurrentLayerIndex].LayerCaption := s;
      UpdateMainFormCaptionEvent(self);
    end;
    F2DialogRunning := False;
    dlg.Destroy;
  end;

end;



procedure TGLWMainForm.savetodifferentformat1Click(Sender: TObject);
var
  loadbar : TVisualOverlayProgressBar;
  targetdir : string;
begin
  opendialog1.Options := [ofFileMustExist, ofForceShowHidden, ofAllowMultiSelect, ofEnableSizing];
  opendialog1.Filter := 'Standard Formats (*.bmp, *.jpg, *.jpeg, *.png)|*.bmp;*.jpg;*.jpeg;*.png|'+
                        'bitmap images (*.bmp)|*.bmp|'+
                        'JPG images (*.jpg, *.jpeg)|*.jpg;*.jpeg|'+
                        'PNG images (*.png)|*.png|'+
                        'all files (*.*)|*.*';

  if opendialog1.Execute then
  begin

    loadbar := TVisualOverlayProgressBar.Create(GLWMainForm.ClientRect);
    loadbar.Caption := 'processing image(s)  1/'+inttostr(OpenDialog1.Files.Count)+' ...';
    loadbar.min     := 0;
    loadbar.position:= 0;
    loadbar.max     := OpenDialog1.Files.Count;
    loadbar.Width   := 320;
    loadbar.Left    := (-loadbar.Width  + ClientWidth  ) div 2;
    loadbar.Top     := (-loadbar.Height + ClientHeight ) div 2;

    GLM.InsertVisualOverlayObject(loadbar);
    loadbar.Visible := true;
    GLM.ClientPaint(self);

    targetdir := GLMFilterFileProcessor.BatchReSaveFiles(
                OpenDialog1.Files,
                loadbar,
                glm);

    GLM.RemoveVisualOverlayObject(loadbar);
    loadbar.Destroy;
    sleep(200);

    glm.ClientPaint(self);
    ShellExecute(0,'open',PChar(targetdir),nil,nil,SW_SHOWMAXIMIZED);
  end;


end;


procedure TGLWMainForm.selectimagesource1Click(Sender: TObject);
begin
  TwainHelper.SelectMainTwainSource;
  NewLayerfromTWAINsource1.Click; // update menu item "enabled" states
  // try to fill a submenu with available resolutions
  //MainTwainObject.Source[MainTwainSource].GetCapabilityRec(ICAP_XRESOLUTION,Handle,Mode,Container);
end;



function TGLWMainForm.SetTwainAcquireTargetLayer(SourceIndex: longint;
                LayerID: longint): longint; // returns the array index
var
  SourceID: DWORD;
  TargetArrayIdx: longint;
begin
  SourceID       := MainTwainObject.Source[SourceIndex].SourceIdentity^.ID;
  TargetArrayIdx := TWAINAcquisitionTargetIdxByID(SourceID);

  if TargetArrayIdx <> -1 then
  begin
    // update LayerID
    with TWAINAcquisitionTargets[TargetArrayIdx] do
    begin
      TargetLayerPresetID := LayerID;
    end;
  end
  else
  begin
    // add to array
    TargetArrayIdx := TWAINAcquisitionTargetLength;

    if TWAINAcquisitionTargetLength <=  High(TWAINAcquisitionTargets) then
      inc(TWAINAcquisitionTargetLength); // clip to avoid access violations. Who uses more than 64 Twain devices?

    with TWAINAcquisitionTargets[TargetArrayIdx] do
    begin
      SourceIdentifier    := SourceID;
      TargetLayerUsedID   := -1;
    end;
  end;

  result := TargetArrayIdx;
end;



procedure TGLWMainForm.UnsetTwainAcquireTargetLayer(SourceIndex: longint);
var
  SourceID: DWORD;
  TargetArrayIdx: longint;
begin
  SourceID       := MainTwainObject.Source[SourceIndex].SourceIdentity^.ID;
  TargetArrayIdx := TWAINAcquisitionTargetIdxByID(SourceID);

  // delete from array
  if TargetArrayIdx <> -1 then
  begin
    TWAINAcquisitionTargets  [TargetArrayIdx] :=
      TWAINAcquisitionTargets[TWAINAcquisitionTargetLength];
    dec(TWAINAcquisitionTargetLength);
  end;
end;



procedure TGLWMainForm.OnTwainAcquire(Sender: TObject; const Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
var
  newidx  : integer;
  datestr : string;
  devname : string;
  LayerIdx: longint;
  TargetArrayIndex: longint;
  SenderID: DWORD;
begin

  // look up layer for this source by ID
  SenderID := MainTwainObject.Source[index].SourceIdentity^.ID;
  TargetArrayIndex := TWAINAcquisitionTargetIdxByID(SenderID);

  if TargetArrayIndex <> -1 then
  begin
    LayerIdx := glm.LayerByID(TWAINAcquisitionTargets[TargetArrayIndex].TargetLayerPresetID);
    if not TWAINAcquisitionTargets[TargetArrayIndex].ReuseTarget then LayerIdx := -1;
  end
  else
    LayerIdx := -1;

  // make new layer or reuse old layer
  if (LayerIdx = -1) then
  begin
    glm.surpressrepaint;
    newidx := glm.NewLayer(rect(0,0,image.Width,image.Height),clblack);
  end
  else
  begin
    newidx := LayerIdx;
    if glm.Layers[newidx].data.Width  <> image.Width then
       glm.Layers[newidx].data.Width  := image.Width;
    if glm.Layers[newidx].data.Height <> image.Height then
       glm.Layers[newidx].data.Height := image.Height;
  end;

  // if a new layer could be obtained, load image and document acquisition
  if newidx > -1 then
  begin
    // paste image data to layer
    glm.Layers[newidx].data.Canvas.Draw(0,0,image);

    // create new layer caption;
    DateTimeToString(datestr,'hh-mm-ss',GetTime);
    devname:= TwainHelper.MainTwainObject.Source[Index].ProductName;
    if length(devname) > 32 then devname := Copy(devname,1,32)+'...';
    glm.Layers[newidx].LayerCaption := devname + '_' + datestr;

    // update AcquireTarget information
    if TargetArrayIndex = -1 then
      SetTwainAcquireTargetLayer(index, glm.Layers[newidx].LayerID);

    glm.OnLayerChanges.SharedEvent(self);// update layer selection dialog if needed
    glm.ClientPaint(self);
    self.OnAfterTwainAcquisition.SharedEvent(Sender);
  end;

  Cancel := False;
  TwainHelper.FinishAcquisition;
end;



procedure TGLWMainForm.debuginputdialog1Click(Sender: TObject);
var
  mid: TMultiFunctionalInputDialog;
//  i: integer;
var
  str1 : string;
//  ar: real;
  s: string;
begin
  str1 := 'Congratulations! You have found the information field of this dialog.'#13#10+
    'Unfortunately, it does not tell you anything particular at this time ;-)';
  mid := TMultiFunctionalInputDialog.Create;
  mid.frm.Caption := 'tab order test';

  mid.AddInputField('Information about the settings below ...',nil,'info','','',str1);
  mid.AddInputField( 'Text Field 01', nil, 'static', '','',str1);
  mid.AddInputField( 'Text Field 02', @s, 'string');
  mid.AddInputField( 'Text Field 03', @s, 'string');

  mid.Execute;
  mid.Destroy;
end;



procedure TGLWMainForm.N16bitsinglemonochromeRAW1Click(Sender: TObject);
begin
  opendialog1.Options := [ofFileMustExist, ofForceShowHidden, ofAllowMultiSelect];
  opendialog1.Filter := 'all files (*.*)|*.*';

  if opendialog1.Execute then
  begin
    //
  end;

  glm.ClientPaint(self);
end;




procedure TGLWMainForm.Native1Click(Sender: TObject);
var  i : integer;
begin
  for i := 0 to (Sender as TMenuItem).Parent.Count-1 do
  begin
    (Sender as TMenuItem).Parent.Items[i].Checked :=
      ((Sender as TMenuItem).Parent.Items[i] = Sender);
  end;
  TwainHelper.TransferModeChoice := TwainHelper.TTwainTransferMode(ttmNative);
end;



procedure TGLWMainForm.Memory1Click(Sender: TObject);
var  i : integer;
begin
  for i := 0 to (Sender as TMenuItem).Parent.Count-1 do
  begin
    (Sender as TMenuItem).Parent.Items[i].Checked :=
      ((Sender as TMenuItem).Parent.Items[i] = Sender);
  end;
  TwainHelper.TransferModeChoice := TwainHelper.TTwainTransferMode(ttmMemory);
end;



function TGLWMainForm.TWAINAcquisitionTargetIdxByID(ID: DWORD):longint;
var
  i: integer;
begin
  result := -1;

  for i := 0 to High(TWAINAcquisitionTargets) do
  begin
    if TWAINAcquisitionTargets[i].SourceIdentifier = ID then
    begin
      result := i;
      break;
    end;
  end;
end;



procedure TGLWMainForm.Preferences1Click(Sender: TObject);
begin
  Preferences1.Enabled := FALSE;
  GLM.ExecutePresetsDialog(self);
  Preferences1.Enabled := TRUE;
end;



procedure TGLWMainForm.UpdateMainFormCaption(var Message: TMessage);
begin
  UpdateMainFormCaptionEvent(self);
end;



procedure TGLWMainForm.UpdateMainFormCaptionEvent(Sender: TObject);
var
  LayerString : string;
  idx, layeridx : longint;
begin
  layeridx := -1;
  for idx := 0 to length(GLM.Layers) - 1 do
  begin
    if GLM.Layers[idx].visible then
    begin
      if LayerIdx <> -1 then
      begin
        LayerIdx := -1; // more than one image visible
        break;
      end;
      if LayerIdx = -1 then
        LayerIdx := Idx;
    end;
  end;

  if layeridx <> -1 then
    LayerString := '        ['+inttostr(layeridx)+']  '+GLM.Layers[layeridx].LayerCaption

  else
    LayerString := '';

  if GLM.ZoomFactor < 0.5 then
    GLWMainForm.Caption   := 'GraphicLayerWindow - '+
        FloattoStr(round(10000*glm.ZoomFactor)/100) + '%' + LayerString
  else
  GLWMainForm.Caption   := 'GraphicLayerWindow - '+
        IntToStr(  round(  100*glm.ZoomFactor)) + '%' + LayerString;
end;



procedure TGLWMainForm.setZoomMethod(Sender: TObject);
begin
  glm.ZoomMode := TGLMZoomMode((Sender as TMenuItem).Tag);
end;



procedure TGLWMainForm.GLMMiddleMouseClick(Sender:TObject; X,Y: integer;
      Shift: TShiftState);
var
  p: TPoint;
  i: integer;
begin
  GetCursorPos(p);
  for i := 0 to 4 do MiddleButtonMenu.Items[i].Checked :=
      (MiddleButtonMenu.Items[i].Tag = integer(glm.ZoomMode));
  GLWMainForm.MiddleButtonMenu.Popup(p.X, p.Y);
end;



procedure TGLWMainForm.TWAINSelectDeviceParametersClick(Sender: TObject);
begin
  // open device parameters dialog
end;



procedure TGLWMainForm.FormDblClick(Sender: TObject);
begin
  GLM.OnParentDblClick(self);
end;



procedure TGLWMainForm.FormDestroy(Sender: TObject);
begin
  // moved from FormClose to FormDestroy
  //
  OnAfterTwainAcquisition.Destroy;
  OnAfterTwainAcquisition := nil;
  glm.surpressrepaint;
  glm.Destroy;
  glm := nil;
end;

procedure TGLWMainForm.closeallactivelinks1Click(Sender: TObject);
var
  i: integer;
begin
    for i := 0 to length(GLM.Layers)-1 do
        GLM.ReleaseExternalEditor(GLM.Layers[i].LayerID);
end;



procedure TGLWMainForm.BrowseMainDirectory1Click(Sender: TObject);
begin
  ShellExecute(0,'open',PChar(ExtractFilePath(Paramstr(0))),nil,nil,SW_SHOWMAXIMIZED);
end;



procedure TGLWMainForm.oolbox1Click(Sender: TObject);
var
  FormWasVisible: Boolean;
begin
  if ToolBoxForm.Parent <> GLWMainForm then
    ToolBoxForm.Parent := GLWMainForm;
  //if not ToolBoxForm.Visible then
  // generally do this (in case the tool window gets "lost"

  // OS Version < WinXP likes this
  {if ToolBoxForm.Left <>
    (GLWMainForm.ClientWidth - ToolBoxForm.Width) then
  begin
    ToolBoxForm.Top  := -ToolBoxForm.Height-128; // win7 "top=0" error workaround
    ToolBoxForm.Left := GLWMainForm.ClientWidth - ToolBoxForm.Width;
  end; }
  FormWasVisible := ToolBoxForm.Visible;
  //ToolBoxForm.Show;

  // haha! win! works on win7
  Windows.SetWindowPos( ToolBoxForm.Handle,             // form handle
                        HWND_TOP,               // z-Order
                        GLWMainForm.ClientWidth - ToolBoxForm.Width, 0, // left, Top
                        0, 0,                   // Width, Height
                        SWP_NOSIZE); // show, ignore width/height parameters
  ToolBoxForm.Show;

  // Windows 7 likes it this way
  ToolBoxForm.Top  := 0;  // just to be sure
  ToolBoxForm.Left := GLWMainForm.ClientWidth - ToolBoxForm.Width;

  if ToolBoxForm.Height >  ToolBoxForm.Parent.ClientHeight then
     ToolBoxForm.Height := ToolBoxForm.Parent.ClientHeight;

  if FormWasVisible then ToolBoxForm.StatusBarDblClick(self); 
  
end;



procedure TGLWMainForm.FormActivate(Sender: TObject);
begin
  ActivateComponents(self);
end;

procedure TGLWMainForm.FormClick(Sender: TObject);
begin
    // not-so-well integrated forms could
    // have stolen the focus for keyboard events. Get it back!
  Windows.SetForegroundWindow(Application.MainForm.Handle);

  if ToolBoxForm.Visible then   // drop tab focus
  begin
    ToolBoxForm.Enabled := false;
    ToolBoxForm.Enabled := true;
  end;
end;



procedure TGLWMainForm.ExportToClipboard1Click(Sender: TObject);
var
  LayerIdx: integer;
begin
  LayerIdx := glm.LayerByID(CurrentLayerID);
  if LayerIdx = -1 then exit;

  ClipBoard.Clear;
  Clipboard.Assign(GLM.Layers[LayerIdx].data); // drop it like it's hot! Perhaps prompt the user
                                               // when a mask layer is assigned whether he wants
                                               // a) just the current layer
                                               // b) both layers
                                               // c) a 32bit representation with an alpha channel 
end;



procedure TGLWMainForm.addvectortestdatatomaincontainer1Click(Sender: TObject);
var
  p            : TSinglePoint; // dummy variable
  points       : TPointGroup;  // dummy variable
  meta         : TMetaData;    // Global Container Object Reference
  pg           : TPolygonGroup;
  poly         : TSinglePolygon;
begin
  meta := glm.GlobalMetaContent;
  meta.TagName := 'container';

  points := TPointGroup.Create;

  pg := TPolygonGroup.Create;
  poly := TSinglePolygon.Create;
  pg.Add(poly);

  p := TSinglePoint.Create;
  p.Point := Point(0,0);
  Points.Add(p);
  Poly.AddPointReference(p.UID,points.UID);

  p := TSinglePoint.Create;
  p.Point := Point(0,256);
  Points.Add(p);
  Poly.AddPointReference(p.UID,points.UID);

  p := TSinglePoint.Create;
  p.Point := Point(256,256);
  Points.Add(p);
  Poly.AddPointReference(p.UID,points.UID);

  p := TSinglePoint.Create;
  p.Point := Point(256,0);
  Points.Add(p);
  Poly.AddPointReference(p.UID,points.UID);

  meta.Add(Points);
  meta.Add(pg);

  GLM.RecreateMetaContentLists(self);
  GLM.ClientPaint(self);
end;



end.
