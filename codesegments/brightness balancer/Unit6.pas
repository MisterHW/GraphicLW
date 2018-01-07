unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Math,
  Dialogs, ExtCtrls, StdCtrls, GraphicLayerManager, GLMFunctionality, Histogramcmp,
  ComCtrls, GLWFilters, GLWDef;

type
  Tbbalancerform = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Image2: TImage;
    Panel1: TPanel;
    abortbutton: TButton;
    applybutton: TButton;
    GroupBox2: TGroupBox;
    Panel2: TPanel;
    Label2: TLabel;
    suggestionlabel: TLabel;
    GroupBox3: TGroupBox;
    cubesizetracker: TTrackBar;
    Label4: TLabel;
    gclabel: TLabel;
    CheckBox2: TCheckBox;
    brightnesstracker: TTrackBar;
    Label5: TLabel;
    zeropresetlabel: TLabel;
    CheckBox3: TCheckBox;
    Label6: TLabel;
    QuantileCenter: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    QuantileWidth: TEdit;
    Button1: TButton;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    MaskThreshold: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure abortbuttonClick(Sender: TObject);
    procedure applybuttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cubesizetrackerChange(Sender: TObject);
    procedure brightnesstrackerChange(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Label11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MaskThresholdClick(Sender: TObject);
    procedure QuantileCenterClick(Sender: TObject);
    procedure QuantileWidthClick(Sender: TObject);
  private
    ms: TMemoryStream;
    hist: THistogramcmp;
    estimatedsoftlim, estimatedhardlim: integer;
    GraphLayerID: Longint;
    GraphArea: TRect;
  public
    RemoteGLM:TGraphicLayerManager;
    status: integer;
    OnToolAbort : TNotifyEvent;
    WorkingLayerID: longint;
    MaskLayerID: longint;
    MaskLayerIDOfWorkingLayer : longint;
    procedure InitTool(Sender:TObject=nil);
    Procedure LayerChanges(Sender:TObject);
  end;

var
  bbalancerform: Tbbalancerform;

implementation

{$R *.dfm}



procedure Tbbalancerform.FormCreate(Sender: TObject);
begin
 image1.Picture.Bitmap.PixelFormat := pf24bit;
 image2.Picture.Bitmap.PixelFormat := pf24bit;
 image1.Picture.Bitmap.Width  := image1.Width;
 image1.Picture.Bitmap.Height := image1.Height;
 image2.Picture.Bitmap.Width  := image2.Width;
 image2.Picture.Bitmap.Height := image2.Height;
 Status := 0;
 ms := nil;
 hist := nil;
 GraphLayerID := -1;
 GraphArea := rect(0,0,0,0);
end;



procedure Tbbalancerform.InitTool;
var
  WorkingLayerIdx, MaskLayerIdx : integer;
  outrect: TRect;
begin
  self.OnMouseWheel := self.FormMouseWheel;
  WorkingLayerIdx := RemoteGLM.LayerByID(self.WorkingLayerID);

  if self.Visible then
  begin
    if RemoteGLM.LayerByID(self.WorkingLayerID) = -1 then
    begin
      abortbutton.Click;
      exit;
    end;
  end
  else
    MaskLayerIDOfWorkingLayer := RemoteGLM.Layers[WorkingLayerIdx].MaskLayerID;

  image1.Picture.Bitmap.Canvas.Brush.Color := clwhite;
  image1.Picture.Bitmap.Canvas.FillRect(image1.Picture.Bitmap.Canvas.ClipRect);

  // if WorkingLayerIdx <> -1 then
  begin
    outrect := GLMFunctionality.CalculateThumbnailArea(
      rect(0,0,image1.Picture.Bitmap.Width,image1.picture.bitmap.Height),
      2,
      remoteglm.Layers[WorkingLayerIdx].data.Width,
      remoteglm.Layers[WorkingLayerIdx].data.Height);

    GLMFunctionality.StretchCopyRect(
      remoteglm.layers[WorkingLayerIdx].data,
      image1.picture.Bitmap,
      remoteglm.layers[WorkingLayerIdx].data.Canvas.ClipRect,
      outrect);

    image1.Repaint;
    RemoteGLM.OnLayerChanges.InsertEvent(LayerChanges);
  end;

  if assigned(hist) then hist.Destroy; hist := nil;
  if assigned(ms) then ms.Free;
  suggestionlabel.Caption := '';
  image2.Picture.Bitmap.Canvas.Brush.Color := clwhite;
  image2.Picture.Bitmap.Canvas.FillRect(image2.Picture.Bitmap.Canvas.ClipRect);

  if MaskLayerIDOfWorkingLayer <> remoteglm.layers[WorkingLayerIdx].MaskLayerID then
  begin
    if MessageBox(self.Handle,'The mask layer of the current working layer has changed.'#13#10+
         'Do you want the balancer layer to be updated?',
         'Mask Layer changed. Apply to balancer settings?',
         MB_YESNO or MB_ICONQUESTION) = IDYES
      then self.MaskLayerID := remoteglm.layers[WorkingLayerIdx].MaskLayerID;
  end;
  MaskLayerIDOfWorkingLayer := remoteglm.layers[WorkingLayerIdx].MaskLayerID;

  MaskLayerIdx := RemoteGLM.LayerByID(self.MaskLayerID); // do not use layer mask id here..
  if MaskLayerIdx <> -1 then
  begin
    outrect := GLMFunctionality.CalculateThumbnailArea(
      rect(0,0,image2.Picture.Bitmap.Width,image2.picture.bitmap.Height),
      2,
      remoteglm.Layers[MaskLayerIdx].data.Width,
      remoteglm.Layers[MaskLayerIdx].data.Height);

    GLMFunctionality.StretchCopyRect(
      remoteglm.layers[MaskLayerIdx].data,
      image2.picture.Bitmap,
      remoteglm.layers[MaskLayerIdx].data.Canvas.ClipRect,
      outrect);

    image2.Repaint;

    // now create the histogram

    ms := AnalyzeGradientStrength(
        RemoteGLM.Layers[WorkingLayerIdx].data,
        RemoteGLM.Layers[RemoteGLM.LayerByID(MaskLayerID)].data,
        4);
  end
  else
  begin
    ms := AnalyzeGradientStrength(
        RemoteGLM.Layers[WorkingLayerIdx].data,
        nil,
        4);
  end;
  hist := THistogramcmp.Create(panel2,panel2.Width,panel2.Height,ms);
  if hist.QuantileValue[5] / (hist.QuantileValue[3]+0.00001) > 4 then
    suggestionlabel.Caption := 'Warning: high contrast areas could cause artefacts.';
  estimatedsoftlim := min(1000, round(255 / ((4-1) * hist.QuantileValue[5]+0.0001)));
  estimatedhardlim := min(1000, round(255 / ((4-1) * hist.QuantileValue[4]+0.0001)));
  suggestionlabel.Caption := suggestionlabel.Caption + #13#10 +
    'block size < '+inttostr(estimatedsoftlim) + '(soft limit) / '+
                    inttostr(estimatedhardlim) + '(hard limit) ';
  self.cubesizetracker.OnChange(self);

   // flattening without mask layer allowed
  applybutton.Enabled := (WorkingLayerIdx <> -1) ; // and (MaskLayerIdx <> -1);
  if applybutton.Enabled then self.status := 2 else self.status := 1;
end;




procedure Tbbalancerform.Image2Click(Sender: TObject);
var
  i: integer;
  WorkingLayerIdx : integer;
begin
  WorkingLayerIdx := RemoteGLM.LayerByID(self.WorkingLayerID);

  self.Enabled := false;
  for i := 0 to length(RemoteGLM.Layers)-1 do
  begin
    RemoteGLM.Layers[i].SelectableInDialog :=
     (RemoteGLM.Layers[i].data.Width = RemoteGLM.Layers[WorkingLayerIdx].data.Width ) and
     (RemoteGLM.Layers[i].data.Height = RemoteGLM.Layers[WorkingLayerIdx].data.Height );
  end;

  if RemoteGLM.ExecuteLayerSelectionDialog(MaskLayerID) then
  begin
    MaskLayerID := RemoteGLM.LayerSelectionID;
    InitTool;
  end;

  begin
    RemoteGLM.FocusLayer(RemoteGLM.LayerByID(self.WorkingLayerID));
    self.Enabled := true;
  end;
end;




procedure Tbbalancerform.abortbuttonClick(Sender: TObject);
var
  widx: integer;
begin
  self.status := 0;
  self.Hide;
  RemoteGLM.OnLayerChanges.RemoveEvent(self.LayerChanges);
  RemoteGLM.OnLayerChanges.RemoveEvent(self.InitTool);

  RemoteGLM.surpressrepaint;
  RemoteGLM.DeleteLayer(RemoteGLM.LayerByID(GraphLayerID));
  widx := RemoteGLM.LayerByID(self.WorkingLayerID);
  if widx <> -1 then RemoteGLM.FocusLayer(widx);

  if assigned(self.onToolAbort) then onToolAbort(self);
end;




Procedure Tbbalancerform.LayerChanges(Sender:TObject);
var
 i: integer;
  WorkingLayerIdx : integer;
begin
  WorkingLayerIdx := RemoteGLM.LayerByID(self.WorkingLayerID);

  if WorkingLayerIdx  = -1 then
  begin
    self.abortbuttonClick(self);
  end
  else
  begin
    for i := 0 to length(RemoteGLM.Layers)-1 do
      begin
        RemoteGLM.Layers[i].SelectableInDialog :=
        (RemoteGLM.Layers[i].data.Width = RemoteGLM.Layers[WorkingLayerIdx].data.Width ) and
        (RemoteGLM.Layers[i].data.Height = RemoteGLM.Layers[WorkingLayerIdx].data.Height );
      end;
    self.InitTool;
  end;

end;



procedure Tbbalancerform.applybuttonClick(Sender: TObject);
var
  WorkingLayerIdx, MaskLayerIdx : integer;
  currentfilter: integer;
  filtera: PStandardFilterParameters;
  ms : TMemoryStream;
  widx: longint;
begin
  GetMem(filtera,SizeOf(TStandardFilterParameters));
  WorkingLayerIdx := RemoteGLM.LayerByID(self.WorkingLayerID);
  filtera^.i1 := self.cubesizetracker.Position;   // grid constant
  filtera^.i2 := self.brightnesstracker.Position; // intended average brightness
  filtera^.i3 := -1; // out coeff bitmap Layer ID
  filtera^.i4 := -1; // out raw bitmap Layer ID
  filtera^.r1 :=  min(255,max(0,StrTointDef(MaskThreshold.Text,128)));
  filtera^.r2 :=  min(100,max(0,StrTointDef(QuantileCenter.Text,80)));
  filtera^.r3 :=  2* min( 50,max(0,StrTointDef(QuantileWidth.Text,1)));

  // filtera^.bool1 := self.CheckBox1.Checked; // smoothen
  filtera^.bool2 := self.CheckBox2.Checked; // multiscale balancing
  filtera^.bool3 := self.CheckBox3.Checked; // trim to brightness
//  filtera.ExecutionCounter := 0;

  MaskLayerIdx := RemoteGLM.LayerByID(self.MaskLayerID);
  filtera^.bool4 := MaskLayerIdx <> -1;

  ms := TMemoryStream.Create;

  ms.WriteBuffer(filtera,sizeof(TStandardFilterParameters));
  if filtera^.bool4 then RemoteGLM.layers[MaskLayerIdx].data.SaveToStream(ms);

  CurrentFilter := RemoteGLM.InsertFilter(
      WorkingLayerIdx,
      FT_BRIGHTNESS_BALANCER,
      filtera,
      SizeOf(TStandardFilterParameters),
      ms);

  glwfilters.ExecuteFilterAction(
      RemoteGLM.layers[WorkingLayerIdx].data,
      RemoteGLM.layers[WorkingLayerIdx].FilterHeap[CurrentFilter]);

  remoteglm.layers[WorkingLayerIdx].FilterHeapCursor := CurrentFilter;

  // clean up
  RemoteGLM.OnLayerChanges.RemoveEvent(self.LayerChanges);
  RemoteGLM.OnLayerChanges.RemoveEvent(self.InitTool);

  RemoteGLM.surpressrepaint;
  RemoteGLM.DeleteLayer(RemoteGLM.LayerByID(GraphLayerID));
  widx := RemoteGLM.LayerByID(self.WorkingLayerID);
  if widx <> -1 then RemoteGLM.FocusLayer(widx);

  if assigned(self.onToolAbort) then onToolAbort(self);
  self.Hide;
  self.status := 0;
end;



procedure Tbbalancerform.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 self.abortbuttonClick(self);
end;


procedure Tbbalancerform.cubesizetrackerChange(Sender: TObject);
begin
  gclabel.Caption := inttostr(self.cubesizetracker.Position);
  gclabel.Font.Color := clblack;

  if self.cubesizetracker.Position > estimatedhardlim then
    gclabel.Font.Color := clred
  else
  if self.cubesizetracker.Position > estimatedsoftlim then
    gclabel.Font.Color := $007070
  else
    gclabel.Font.Color := clblack;

end;

procedure Tbbalancerform.brightnesstrackerChange(Sender: TObject);
begin
  zeropresetlabel.Caption := inttostr(self.brightnesstracker.Position);
end;

procedure Tbbalancerform.Label6Click(Sender: TObject);
begin
  MessageBox(0,'The histrogram above shows the analysis of localized gradient strengths.'#13#10'Extrapolate a chosen value by the block size to guess the rate of change.'#13#10'Deviations exceeding 255 cannot be controlled.','About the histrogram',MB_OK or MB_ICONASTERISK);
end;




procedure Tbbalancerform.FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // do not steal the wheel event ;)
  application.MainForm.OnMouseWheel(sender,Shift,WheelDelta,MousePos,Handled);
  Handled := true;
end;

procedure Tbbalancerform.Label11Click(Sender: TObject);
begin
  MessageBox(0, 'The center and width values of the characteristic percentile'#13#10+
                'define an interval within an array of pixels which have been'#13#10+
                'sorted by their effective brightness. The mean value of this'#13#10+
                'set of pixels is considered a stable local definition. It is'#13#10+
                'best to place the range within a plateau. The upper'#13#10+
                '5-10% should be rejected. Typical values are 80%, +/-2%.',
                'About Statistical Analysis Settings'
    ,MB_OK or MB_ICONASTERISK);
end;

procedure Tbbalancerform.Button1Click(Sender: TObject);
var
  midx, widx, gidx : longint;
  MaskBitmap: TBitmap;
  st: string;
const
  s = 'generating graph...';


{procedure RotatedTextOut(DC: HDC; Font: HFont; alpha: integer; px, py: integer; s: string);
var
  FontRec: TLogFont;
  new, tmp: HFont;
begin
  ZeroMemory(@FontRec,SizeOf(TLogFont));
  GetObject(Font,SizeOf(TLogFont),Addr(FontRec));
  FontRec.lfOrientation := alpha * 10;
  FontRec.lfEscapement  := alpha * 10;
  new := CreateFontIndirect(FontRec);
  tmp := SelectObject(DC, new);
  TextOut(DC,px, py, PChar(s),length(s));
  SelectObject(DC, tmp);
  DeleteObject(new);
end; }



begin
  widx := RemoteGLM.LayerByID(WorkingLayerID);

  if RemoteGLM.LayerByID(self.GraphLayerID) = - 1 then
  begin
    RemoteGLM.surpressrepaint;
    gidx := RemoteGLM.NewLayer(Rect(0,0,900,900),clwhite);
    GraphLayerID := RemoteGLM.Layers[gidx].LayerID;
    RemoteGLM.Layers[gidx].LayerCaption := 'Brightness Balancer Graph';
    with RemoteGLM.Layers[gidx].data do
    begin
       Canvas.Brush.Color := clblack;
       Canvas.FrameRect(Rect(0,0,width,height));
       GraphArea := Rect(32,32,900-32,32+767);
       Canvas.FrameRect(GraphArea);
       Canvas.Brush.Color := clwhite;
       Canvas.TextOut(
        (GraphArea.Left+GraphArea.Right-Canvas.TextWidth(s) ) div 2,
        (GraphArea.Top+GraphArea.Bottom-Canvas.TextHeight(s)) div 2,
        s);
    end;
    RemoteGLM.ClientPaint(self);
  end
  else
  begin
    gidx := RemoteGLM.LayerByID(GraphLayerID);
    RemoteGLM.FocusLayer(gidx);
  end;



  MaskBitmap := nil;
  midx := RemoteGLM.LayerByID(RemoteGLM.Layers[widx].MaskLayerID);
  if midx <> -1 then MaskBitmap := RemoteGLM.Layers[midx].data;

  GLWFilters.BrightnessBalancerPercentileGraph(
      RemoteGLM.Layers[widx].data,
      RemoteGLM.Layers[gidx].data,
      MaskBitmap,
      GraphArea,
      self.cubesizetracker.Position,
      min(255,max(0,StrTointDef(MaskThreshold.Text,128))),
      min(100,max(0,StrTointDef(QuantileCenter.Text,80))),
      2* min( 50,max(0,StrTointDef(QuantileWidth.Text,1))),
      brightnesstracker.Position,
      Checkbox2.Checked,
      false);

    with RemoteGLM.Layers[gidx].data do
    begin
      st := 'Brightness Percentile Graph : "'+ RemoteGLM.Layers[widx].LayerCaption+'"';
      Canvas.TextOut(
        (GraphArea.Left+GraphArea.Right-Canvas.TextWidth(st) ) div 2,
        8,
        st);

      st := 'X axis: sorted brightness entries (scaled) - Y axis: brightness (arb. units)';
      Canvas.TextOut(
        (GraphArea.Left+GraphArea.Right-Canvas.TextWidth(st) ) div 2,
        GraphArea.Bottom+8,
        st);

      st := 'block size = '+inttostr(self.cubesizetracker.Position);
      Canvas.TextOut(
        GraphArea.Left+4 ,
        GraphArea.Bottom+16,
        st);

      st := 'targeted brightness = '+inttostr(brightnesstracker.Position);
      Canvas.TextOut(
        GraphArea.Left+4 ,
        GraphArea.Bottom+38,
        st);

      st := 'nominal brightness quantile = '+QuantileCenter.Text+' +/- '+QuantileWidth.Text+ '%';
      Canvas.TextOut(
        GraphArea.Left+4 ,
        GraphArea.Bottom+60,
        st);


    end;

  RemoteGLM.ClientPaint(self);
end;

procedure Tbbalancerform.MaskThresholdClick(Sender: TObject);
begin
  (Sender as TWinControl).SetFocus;
end;

procedure Tbbalancerform.QuantileCenterClick(Sender: TObject);
begin
(Sender as TWinControl).SetFocus;
end;

procedure Tbbalancerform.QuantileWidthClick(Sender: TObject);
begin
(Sender as TWinControl).SetFocus;
end;

end.
