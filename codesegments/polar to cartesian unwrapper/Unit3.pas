unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Types,
  Dialogs, StdCtrls, GraphicLayerManager, GLMFunctionality, Math, GLWdef;


type
  TForm3 = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    RemoteGLM:TGraphicLayerManager;
    OnToolAbort : TNotifyEvent;
    OnStartRecordCircle : TPointArrayParamFunc;
    OnUpdateCurrentLayer : LongintProc;
    ToolStatus: integer;
    orientationpoints: Array of TPoint;
    WorkingLayerID: longint;
    PreviewLayerID: longint;
    procedure StoreOrientationPoints(var sp: Array of TPoint);
    procedure ResetTool;
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}


// modes
// 0 : tool disabled
// 1 : ready for action
// 2 : record orientation points

// 4 : ready to finish

procedure TForm3.ResetTool;
begin
  button2.Enabled := false;
  button4.Enabled := false;
  enabled:= true;
end;

procedure TForm3.StoreOrientationPoints(var sp: Array of TPoint);
var i: integer;
begin
  self.ToolStatus := 4;
   setlength(orientationpoints,length(sp));
   for i := 0 to length(sp)-1 do
   begin
     orientationpoints[i] := sp[i];
   end;

  button2.Enabled := true;
  button4.Enabled := true;

  enabled := true;
end;

procedure TForm3.Button3Click(Sender: TObject);
var
  PreviewLayeridx: integer;
begin
  PreviewLayeridx := remoteglm.LayerByID(PreviewLayerID);
  remoteGLM.DeleteLayer(PreviewLayeridx);
  ToolStatus := 0;
  hide;
  if assigned(OnToolAbort) then OnToolAbort(self);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  self.ToolStatus :=0;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  button3.Click;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  remoteGLM.DeleteLayer(remoteGLM.LayerByID(PreviewLayerID));
  remoteGLM.ClientPaint(self);

  self.ToolStatus := 2;
  self.OnStartRecordCircle(orientationpoints); // must be assigned
  enabled := false;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  PreviewLayeridx, WorkingLayerIdx: integer;
  w,h : integer;
  i: integer;
begin
  WorkingLayerIdx := remoteglm.LayerByID(WorkingLayerID);
  PreviewLayeridx := remoteglm.LayerByID(PreviewLayerID);

  if WorkingLayeridx = -1 then
  begin
    Application.MessageBox('Source Layer is gone. Cannot perform image transformation.','Evident Failure',mb_OK);
    exit;
  end;

  h := ceil(sqrt(sqr(orientationpoints[0].X-orientationpoints[1].X) +
                 sqr(orientationpoints[0].Y-orientationpoints[1].Y)));
  w := ceil(sqrt(sqr(orientationpoints[0].X-orientationpoints[1].X) +
                 sqr(orientationpoints[0].Y-orientationpoints[1].Y)) * 2 * pi) div 5;

  remoteGLM.surpressrepaint;
  
  if PreviewLayeridx <> -1 then
    remoteGLM.DeleteLayer(PreviewLayeridx);

  WorkingLayerIdx := remoteglm.LayerByID(WorkingLayerID);

    PreviewLayeridx := remoteglm.NewLayer(rect(
    remoteGLM.Layers[WorkingLayerIdx].Position.X+ remoteGLM.Layers[WorkingLayerIdx].data.Width+ 128,
    remoteGLM.Layers[WorkingLayerIdx].Position.Y,
    remoteGLM.Layers[WorkingLayerIdx].Position.X+ remoteGLM.Layers[WorkingLayerIdx].data.Width+ 128 + w,
    remoteGLM.Layers[WorkingLayerIdx].Position.Y+h )
    ,clblack);
    self.PreviewLayerID := remoteGLM.layers[PreviewLayeridx].LayerID;

  remoteGLM.Layers[WorkingLayerIdx].visible := true;
    with remoteGLM.Layers[PreviewLayerIdx].data do
    begin
     Canvas.Font.Size := 36;
     Canvas.Font.Color := cllime;
     Canvas.TextOut(0,0,' processing ... ');
    end;
  remoteGLM.ClientPaint(self);

  for i := 0 to length(orientationpoints)-1 do
      orientationpoints[i] := Point(
        orientationpoints[i].X - remoteGLM.Layers[WorkingLayerIdx].Position.X,
        orientationpoints[i].Y - remoteGLM.Layers[WorkingLayerIdx].Position.Y );

  PolToCartesianTrans(remoteGLM.Layers[WorkingLayerIdx].data,
    remoteGLM.Layers[PreviewLayerIdx].data,
    self.orientationpoints, // needs to be shifted by origin of image!!! fix this
    true);

  for i := 0 to length(orientationpoints)-1 do
      orientationpoints[i] := Point(
        orientationpoints[i].X + remoteGLM.Layers[WorkingLayerIdx].Position.X,
        orientationpoints[i].Y + remoteGLM.Layers[WorkingLayerIdx].Position.Y );

  remoteGLM.Layers[PreviewLayerIdx].LayerCaption := 'Polar to Cartesian transform preview';
  remoteGLM.ClientPaint(self);


end;

procedure TForm3.Button4Click(Sender: TObject);
var
  RenderLayeridx, PreviewLayeridx, WorkingLayerIdx: integer;
  RenderLayerID: longint;
  w,h : integer;
  i: integer;
begin
  // WorkingLayerIdx := remoteglm.LayerByID(WorkingLayerID);
  PreviewLayeridx := remoteglm.LayerByID(PreviewLayerID);

  remoteGLM.surpressrepaint;
  if PreviewLayeridx <> -1 then
      remoteGLM.DeleteLayer(PreviewLayeridx); // now every layer index must be recalculated

  // render here

    WorkingLayerIdx := remoteglm.LayerByID(WorkingLayerID);

    h := ceil(sqrt(sqr(orientationpoints[0].X-orientationpoints[1].X) +
                 sqr(orientationpoints[0].Y-orientationpoints[1].Y)));
    w := ceil(sqrt(sqr(orientationpoints[0].X-orientationpoints[1].X) +
                 sqr(orientationpoints[0].Y-orientationpoints[1].Y)) * 2 * pi);

    RenderLayeridx := remoteglm.NewLayer(rect(
    0,
    0,
    w,
    h )
    ,clblack);
    RenderLayerID := remoteGLM.layers[RenderLayeridx].LayerID;
    remoteGLM.Layers[RenderLayerIdx].LayerCaption := remoteGLM.Layers[WorkingLayerIdx].LayerCaption + ' _ poltrans';

    with remoteGLM.Layers[RenderLayerIdx].data do
    begin
     Canvas.Font.Size := 36;
     Canvas.Font.Color := cllime;
     Canvas.TextOut(50,50,' processing ... ');
    end;

    self.ToolStatus := 0;
    remoteGLM.ClientPaint(self);

    for i := 0 to length(orientationpoints)-1 do
      orientationpoints[i] := Point(
        orientationpoints[i].X - remoteGLM.Layers[WorkingLayerIdx].Position.X,
        orientationpoints[i].Y - remoteGLM.Layers[WorkingLayerIdx].Position.Y);

    PolToCartesianTrans(remoteGLM.Layers[WorkingLayerIdx].data,
    remoteGLM.Layers[RenderLayerIdx].data,
    orientationpoints,
    false);


    for i := 0 to length(orientationpoints)-1 do
      orientationpoints[i] := Point(
        orientationpoints[i].X + remoteGLM.Layers[WorkingLayerIdx].Position.X,
        orientationpoints[i].Y + remoteGLM.Layers[WorkingLayerIdx].Position.Y);

  // done rendering.
  self.ResetTool;
  button3.Click; // currentlayerID = workinglayerID wrong...
  if assigned(OnUpdateCurrentLayer) then OnUpdateCurrentLayer(RenderLayerID);

  remoteGLM.ClientPaint(self);

end;

end.
