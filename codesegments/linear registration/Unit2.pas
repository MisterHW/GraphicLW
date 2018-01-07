unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Types,
  Dialogs, StdCtrls, GraphicLayerManager, GLMFunctionality, Math, GLWdef, bilinearregistration,
  ShellAPI;


type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox3: TGroupBox;
    CropToDestOuter: TCheckBox;
    SaveResult: TCheckBox;
    Button9: TButton;
    Button10: TButton;
    GroupBox4: TGroupBox;
    infobox: TMemo;
    GB05: TGroupBox;
    Label3: TLabel;
    bordertoptext: TEdit;
    borderrighttext: TEdit;
    borderlefttext: TEdit;
    borderbottomtext: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure bordertoptextClick(Sender: TObject);
    procedure Label4DblClick(Sender: TObject);
  private
    sourcepointsstored,
    destpointsstored: Boolean;
  public
    RemoteGLM:TGraphicLayerManager;
    OnToolAbort : TNotifyEvent;
    OnStartRecordPoints : TPointArrayParamFunc;
    ToolLinRegStatus: integer;
    sourcepoints: Array of TPoint;
    targetpoints: Array of TPoint;
    WorkingLayerID: longint;
    procedure StoreSourcePoints(var sp: Array of TPoint);
    procedure StoreDestPoints(var sp  : Array of TPoint);
    procedure ResetTool;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

// modes
// 0 : tool disabled
// 1 : ready for action
// 2 : record source points
// 3 : record target points
// 4 : all points collected, verify correspondent pairs




procedure TForm2.FormCreate(Sender: TObject);
begin
  ToolLinRegStatus := 0;
end;





procedure TForm2.Button2Click(Sender: TObject);
begin
  ToolLinRegStatus := 0;
  hide;
  if assigned(OnToolAbort) then OnToolAbort(self);
end;





procedure TForm2.Button3Click(Sender: TObject);
begin
  self.ToolLinRegStatus := 2;
  self.OnStartRecordPoints(sourcepoints); // must be assigned
  enabled := false;

  infobox.Lines.Clear;
  infobox.Lines.Add('right click to set points, left+drag to modify.'#13#10'(4 points needed)');
  infobox.Lines.Add('afterwards, press enter.');
end;





procedure TForm2.Button4Click(Sender: TObject);
begin
  self.ToolLinRegStatus := 3;
  self.OnStartRecordPoints(targetpoints); // must be assigned
  enabled := false;

  infobox.Lines.Clear;
  infobox.Lines.Add('right click to set points, left+drag to modify.'#13#10'(4 points needed)');
  infobox.Lines.Add('afterwards, press enter.');
end;





procedure TForm2.StoreSourcePoints(var sp: Array of TPoint);
var i: integer;
begin
  self.ToolLinRegStatus := 1;
   setlength(sourcepoints,length(sp));
   for i := 0 to length(sp)-1 do
   begin
     sourcepoints[i] := sp[i];
   end;
  label1.Caption := 'ready';

  sourcepointsstored := true;
  if destpointsstored and sourcepointsstored then
  begin
    button1.Enabled := true;
    self.ToolLinRegStatus := 4;
    infobox.clear;
    infobox.Lines.Add('All points set.');
  end
  else
  begin
    infobox.Lines.Clear;
    infobox.Lines.Add('soure points set, destination points missing, please record them.');
  end;
  enabled := true;
end;





procedure TForm2.StoreDestPoints(var sp: Array of TPoint);
var i: integer;
begin
  self.ToolLinRegStatus := 1;
   setlength(Targetpoints,length(sp));
   for i := 0 to length(sp)-1 do
   begin
     targetpoints[i] := sp[i];
   end;
  label2.Caption := 'ready';
  destpointsstored := true;
  if destpointsstored and sourcepointsstored then
  begin
    button1.Enabled := true;
    self.ToolLinRegStatus := 4;
    infobox.clear;
    infobox.Lines.Add('All points set.');
  end
  else
  begin
    infobox.Lines.Clear;
    infobox.Lines.Add('destination points set, source points missing, please record them.');
  end;
  enabled := true;
end;






procedure TForm2.ResetTool;
begin
  button1.Enabled := false;
  //button5.Enabled := false;
  //button6.Enabled := false;
  //button7.Enabled := false;
  label1.Caption := 'clear';
  label2.Caption := 'clear';
  sourcepointsstored := false;
  destpointsstored := false;
end;





procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  button2.Click;
end;





procedure TForm2.Button1Click(Sender: TObject);
var
  sourceidx, destidx ,i : integer;
  srcp, dstp: Array[0..3] of TPoint;
  croprect: TRect;
  innerrect: TRect;
  imode: TInterpolationMethod;
begin

  if RadioButton2.Checked then
          imode := imBilinear
        else
          imode := imNearestNeighbour;

  sourceidx := remoteglm.LayerByID(self.WorkingLayerID);
  if sourceidx <> -1 then
  begin
    // shift global coordinates to pixel coords
    for i := 0 to length(SourcePoints)-1 do  // length(SourcePoints) = length(TargetPoints)  !
    begin
      srcp[i].X := SourcePoints[i].X - remoteglm.Layers[sourceidx].Position.X;
      srcp[i].Y := SourcePoints[i].Y - remoteglm.Layers[sourceidx].Position.Y;
      dstp[i].X := TargetPoints[i].X - remoteglm.Layers[sourceidx].Position.X;
      dstp[i].Y := TargetPoints[i].Y - remoteglm.Layers[sourceidx].Position.Y;
    end;


    if self.CropToDestOuter.Checked then
    begin  // crop
      // cropping calculations
      innerrect.Left  := min(min(min(dstp[0].X,dstp[1].X),dstp[2].X),dstp[3].X);
      innerrect.Right := max(max(max(dstp[0].X,dstp[1].X),dstp[2].X),dstp[3].X);
      innerrect.Top   := min(min(min(dstp[0].Y,dstp[1].Y),dstp[2].Y),dstp[3].Y);
      innerrect.Bottom:= max(max(max(dstp[0].Y,dstp[1].Y),dstp[2].Y),dstp[3].Y);

      croprect.Left  := innerrect.Left   - StrToIntDef(borderlefttext.Text,0);
      croprect.Right := innerrect.Right  + StrToIntDef(borderrighttext.Text,0);
      croprect.Top   := innerrect.Top    - StrToIntDef(borderToptext.Text,0);
      croprect.Bottom:= innerrect.Bottom + StrToIntDef(borderBottomtext.Text,0);

      for i := 0 to length(SourcePoints)-1 do  // shift to crop rect
      begin
        dstp[i].X := dstp[i].X - croprect.Left;
        dstp[i].Y := dstp[i].Y - croprect.Top;
      end;

      destidx := RemoteGLM.NewLayer(croprect, clwhite);
      RemoteGLM.Layers[destidx].LayerCaption := remoteglm.Layers[sourceidx].LayerCaption+'_lr';



      GLMFunctionality.TransformImage( remoteglm.Layers[sourceidx].Data,
                                     remoteglm.Layers[destidx].Data,
                                     srcp,
                                     dstp, imode);
    end // crop
    else
    begin // nocrop
      destidx := RemoteGLM.NewLayer(Rect(
                    remoteglm.Layers[sourceidx].Position.X,
                    remoteglm.Layers[sourceidx].Position.Y,
                    remoteglm.Layers[sourceidx].Position.X+ remoteglm.Layers[sourceidx].data.Width,
                    remoteglm.Layers[sourceidx].Position.Y+ remoteglm.Layers[sourceidx].data.Height),
                    clwhite);

      RemoteGLM.Layers[destidx].LayerCaption := remoteglm.Layers[sourceidx].LayerCaption+'_lr';
      

      GLMFunctionality.TransformImage( remoteglm.Layers[sourceidx].Data,
                                     remoteglm.Layers[destidx].Data,
                                     srcp,
                                     dstp, imode);

    end; // nocrop
  end
  else
  begin
    Application.MessageBox('Source Layer is gone. Cannot perform registration.','Evident Failure',mb_OK);
    exit;
  end;
  self.ResetTool;
  button2.Click;

  if self.SaveResult.Checked then
  begin
    if not DirectoryExists('registeredimages\') then mkdir('registeredimages\');
    RemoteGLM.Layers[destidx].data.SaveToFile('registeredimages\'+DeleteChars(RemoteGLM.Layers[sourceidx].LayerCaption, '|\/{}!.,;:"§$%&=?ß´`~')+'_linreg.bmp');
  end;
end;




{procedure TForm2.Button9Click(Sender: TObject);
var
  buf: Array of TPoint;
  n,i: integer;
  function PointOfMinimumDistanceTo(srcpoint: TPoint): integer;
  var
    newdist, dist: single;
    k: integer;
  begin
    dist := sqrt(sqr(buf[0].x- srcpoint.x)+sqr(buf[0].y- srcpoint.y));
    result := 0;
    for k := 1 to n-1 do
    begin
      newdist := sqrt(sqr(buf[k].x- srcpoint.x)+sqr(buf[k].y- srcpoint.y));
      if newdist < dist then
      begin
        dist := newdist;
        result := k;
      end;
    end;
  end;
begin
  n := min(length(TargetPoints),length(SourcePoints));
  if n <> 4 then exit;

  setlength(buf,length(TargetPoints));

  for i := 0 to n - 1 do
    buf[i] := TargetPoints[i];

  for i := 0 to n - 1 do
  begin
    TargetPoints[i] := buf[PointOfMinimumDistanceTo(SourcePoints[i])];
  end;

  self.RemoteGLM.ClientPaint(self);

  setlength(buf,0);
end; }



procedure TForm2.Button9Click(Sender: TObject);
var
  n,i,s,d: Integer;
  src_minidx,dst_minidx: integer;
  mindist, newmindist: single;
  cmsrc, cmdst, dstbuf: array[0..3] of TPoint; // center of mass coordinates
  srcmap, dstmap: array[0..3] of Boolean; // true = point unassigned , false = point pair assigned

  function distance(a,b:TPoint):single;
  begin
    result := sqrt(sqr(a.x-b.x)+sqr(a.y-b.y));
  end;

begin
  n := min(length(TargetPoints),length(SourcePoints));  // 4 corners only
  if n <> 4 then exit;

  for i := 0 to n-1 do              // duplicate data and reset map
  begin
    cmsrc[i]  := SourcePoints[i];
    cmdst[i]  := TargetPoints[i];
    dstbuf[i] := TargetPoints[i];
    srcmap[i] := true;
    dstmap[i] := true;
  end;

  TransformToCenterOfMassCoords(cmsrc); // compare polygons where CMs are merged
  TransformToCenterOfMassCoords(cmdst);

  // for each pair do:
  while true do
  begin

  src_minidx := -1; dst_minidx := -1; // nothing selected

  mindist := 10E30;
  for s := 0 to n-1 do
    for d := 0 to n-1 do
      if srcmap[s] and dstmap[d] then
      begin
        newmindist := distance(cmsrc[s],cmdst[d]);
        if newmindist < mindist then
        begin
          src_minidx := s; dst_minidx := d; // select pair
          mindist := newmindist;
        end;
      end;

  // now we have a free pair of points with minimum distance
  // assign pair and remove from stack

  if (src_minidx <> -1) and (dst_minidx <> -1) then
  begin
    TargetPoints[src_minidx] := dstbuf[dst_minidx];
    srcmap[src_minidx] := false;
    dstmap[dst_minidx] := false;
  end
  else
    break; // all points assigned. exit loop

  end; // while true

  self.RemoteGLM.ClientPaint(self);
end;




procedure TForm2.Button10Click(Sender: TObject);
var
  envrect:TRect; // envelop rectangle
begin
      if length(sourcePoints) <> 4 then
      begin
        infobox.Lines.Clear;
        infobox.Lines.Add('Source points not set.');
        exit;
      end;

      envrect.Left  := min(min(min(SourcePoints[0].X,SourcePoints[1].X),
                       SourcePoints[2].X),SourcePoints[3].X);
      envrect.Right := max(max(max(SourcePoints[0].X,SourcePoints[1].X),
                       SourcePoints[2].X),SourcePoints[3].X);
      envrect.Top   := min(min(min(SourcePoints[0].Y,SourcePoints[1].Y),
                       SourcePoints[2].Y),SourcePoints[3].Y);
      envrect.Bottom:= max(max(max(SourcePoints[0].Y,SourcePoints[1].Y),
                       SourcePoints[2].Y),SourcePoints[3].Y);

      setlength(TargetPoints,4);
      TargetPoints[0] := envrect.TopLeft;
      TargetPoints[1] := Point(envrect.Right, envrect.Top);
      TargetPoints[2] := envrect.BottomRight;
      TargetPoints[3] := Point(envrect.left, envrect.Bottom);

      self.StoreDestPoints(TargetPoints); // the circle closes

      self.Button9Click(self);

      if sourcepointsstored then
      begin
        infobox.clear;
        infobox.Lines.Add('All points set.');
      end
      else
      begin
        infobox.Lines.Clear;
        infobox.Lines.Add('destination points set, source points missing, please record them.');
      end;
end;





procedure TForm2.bordertoptextClick(Sender: TObject);
begin
 (Sender as TWinControl).SetFocus;
end;

procedure TForm2.Label4DblClick(Sender: TObject);
begin
  ShellExecute(0,'open',PChar(ExtractFilePath(Paramstr(0))+'registeredimages\'),nil,nil,SW_SHOWMAXIMIZED);
end;

end.
