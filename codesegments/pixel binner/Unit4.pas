unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Types,
  Dialogs, StdCtrls, GraphicLayerManager, GLMFunctionality, Math, GLWdef, ComCtrls;


type
  TForm4 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    orgwlabel: TLabel;
    newwlabel: TLabel;
    orghlabel: TLabel;
    newhlabel: TLabel;
    GroupBox2: TGroupBox;
    TrackBar1: TTrackBar;
    amplabel: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    BinWidth: TEdit;
    BinHeight: TEdit;
    UpDown1: TUpDown;
    UpDown3: TUpDown;
    Label5: TLabel;
    Label6: TLabel;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure binwidthChange(Sender: TObject);
    procedure binheightChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure OnTimer(var Message:TWMTimer); Message WM_TIMER;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
  private
    group: TPoint;
    timer: integer;
  public
    RemoteGLM:TGraphicLayerManager;
    OnToolAbort : TNotifyEvent;
    OnUpdateCurrentLayer : LongintProc;
    ToolStatus: integer;
    WorkingLayerID: longint;
    PreviewLayerID: longint;
    procedure ResetTool;
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}


// modes
// 0 : tool disabled
// 1 : ready for action
// 4 : ready to finish

procedure TForm4.ResetTool;
begin
  group := Point(2,2);
  Trackbar1.Position := 0;
  enabled:= true;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  self.ToolStatus :=0;
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  KillTimer(self.Handle,timer); timer := 0;
end;




procedure TForm4.Button1Click(Sender: TObject);
var
  PreviewLayeridx: integer;
begin
  Button3.Caption := 'preview';

  KillTimer(self.Handle,timer); timer := 0;
  //groupbox2.Enabled := false;
  groupbox1.Enabled := true;
  //button2.Enabled := false;

  PreviewLayeridx := remoteglm.LayerByID(PreviewLayerID);
  remoteGLM.DeleteLayer(PreviewLayeridx);

  ToolStatus := 0;
  hide;
  if assigned(OnToolAbort) then OnToolAbort(self);
end;


procedure TForm4.Button2Click(Sender: TObject);
var
  sourceidx, destidx : integer;
  targetrect:TRect;
begin
  KillTimer(self.Handle,timer); timer := 0;


  sourceidx := remoteglm.LayerByID(self.WorkingLayerID);
  if sourceidx <> -1 then
  begin
    self.Hide;
    application.ProcessMessages;

    targetrect := rect(RemoteGLM.Layers[sourceidx].Position.X,
                       RemoteGLM.Layers[sourceidx].Position.Y,
                       RemoteGLM.Layers[sourceidx].Position.X
                          + ceil(RemoteGLM.Layers[sourceidx].data.Width / group.X),
                       RemoteGLM.Layers[sourceidx].Position.Y
                          + ceil(RemoteGLM.Layers[sourceidx].data.Height / group.Y) );
    destidx := RemoteGLM.NewLayer(targetrect, clblack);
    RemoteGLM.Layers[destidx].LayerCaption := RemoteGLM.Layers[sourceidx].LayerCaption+'_binned';

    remoteGLM.Layers[destidx].visible := true;
    with remoteGLM.Layers[destidx].data do
    begin
     Canvas.Font.Size := 36;
     Canvas.Font.Color := cllime;
     Canvas.TextOut(0,0,' processing ... ');
    end;
    RemoteGLM.ClientPaint(self);
    // do transformation

  GLMFunctionality.BinPixels(remoteGLM.Layers[sourceidx].data,
                             remoteGLM.Layers[destidx].data,
                             group.X, group.Y,
                             TrackBar1.Position/100);



    // done
  end
  else
  begin
    Application.MessageBox('Source Layer is gone. Cannot perform operation.','Evident Failure',mb_OK);
    exit;
  end;
  self.ResetTool;
  button1.Click; // perform abort actions, too
  if assigned(OnUpdateCurrentLayer) then
    OnUpdateCurrentLayer(remoteGLM.Layers[destidx].LayerID);
end;




procedure TForm4.Button3Click(Sender: TObject);
var
  PreviewLayeridx, WorkingLayerIdx: integer;
  w,h : integer;
begin

  if ToolStatus <> 4 then
  begin
    Button3.Caption := 'reset';
  end
  else
  begin
    Button3.Caption := 'preview';

    KillTimer(self.Handle,timer); timer := 0;
    groupbox1.Enabled := true;

    remoteGLM.DeleteLayer(remoteglm.LayerByID(PreviewLayerID));

    ToolStatus := 0;

    remoteGLM.ClientPaint(self);

    exit;
  end;
  


  groupbox1.Enabled := false;
  groupbox2.Enabled := true;
  toolstatus := 4;
  button2.Enabled := true;
  
  WorkingLayerIdx := remoteglm.LayerByID(WorkingLayerID);
  PreviewLayeridx := remoteglm.LayerByID(PreviewLayerID);

  if WorkingLayeridx = -1 then
  begin
    Application.MessageBox('Source Layer is gone. Cannot perform image transformation.','Evident Failure',mb_OK);
    exit;
  end;

  remoteGLM.surpressrepaint;

  if PreviewLayeridx <> -1 then
    remoteGLM.DeleteLayer(PreviewLayeridx);

  w := ceil(RemoteGLM.Layers[WorkingLayerIdx].data.Width  / group.X);
  h := ceil(RemoteGLM.Layers[WorkingLayerIdx].data.Height / group.Y);

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

  PostMessage(Application.MainForm.Handle, WM_UPDATE_FORM_CAPTION, 0, 0);


  // perform action
  GLMFunctionality.BinPixels(remoteGLM.Layers[WorkingLayerIdx].data,
                             remoteGLM.Layers[PreviewLayerIdx].data,
                             group.X, group.Y,
                             0); // needs to be zero

  remoteGLM.Layers[PreviewLayerIdx].rawdata := TBitmap.Create;
  remoteGLM.Layers[PreviewLayerIdx].rawdata.Width       := remoteGLM.Layers[PreviewLayerIdx].data.width;
  remoteGLM.Layers[PreviewLayerIdx].rawdata.Height      := remoteGLM.Layers[PreviewLayerIdx].data.Height;
  remoteGLM.Layers[PreviewLayerIdx].rawdata.PixelFormat := remoteGLM.Layers[PreviewLayerIdx].data.PixelFormat;
  remoteGLM.Layers[PreviewLayerIdx].rawdata.Canvas.Draw(0,0,remoteGLM.Layers[PreviewLayerIdx].data);
  // done


  remoteGLM.Layers[PreviewLayerIdx].LayerCaption := 'pixel binning transform preview';
  //remoteGLM.ClientPaint(self);
  timer := SetTimer(self.Handle,timer,0,nil); // will refresh in due time :-)
end;


procedure TForm4.FormShow(Sender: TObject);
var
  sourceidx: integer;
begin
  sourceidx := remoteglm.LayerByID(self.WorkingLayerID);
  if sourceidx <> -1 then
  begin
    self.orgwlabel.Caption := inttostr(RemoteGLM.Layers[sourceidx].data.Width);
    self.orghlabel.Caption := inttostr(RemoteGLM.Layers[sourceidx].data.Height);
    binwidthChange(self);
    binheightChange(self);
  end;
end;

procedure TForm4.binwidthChange(Sender: TObject);
begin
 group.X := StrToIntDef(binwidth.Text,1);
 self.newwlabel.Caption := inttostr(ceil(strtointdef(orgwlabel.Caption,0) / group.X));
 self.newhlabel.Caption := inttostr(ceil(strtointdef(orghlabel.Caption,0) / group.Y));
end;

procedure TForm4.binheightChange(Sender: TObject);
begin
  group.Y := StrToIntDef(binHeight.Text,1);
  self.newwlabel.Caption := inttostr(ceil(strtointdef(orgwlabel.Caption,0) / group.X));
  self.newhlabel.Caption := inttostr(ceil(strtointdef(orghlabel.Caption,0) / group.Y));
end;

procedure TForm4.TrackBar1Change(Sender: TObject);
begin
  amplabel.Caption := inttostr(round(100+trackbar1.Position*(group.X*group.Y -1))) +' %';
  KillTimer(self.Handle,timer); timer := 0;
  timer := SetTimer(self.Handle,timer,50,nil);
end;


procedure TForm4.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
 BinWidthChange(Self);
end;

procedure TForm4.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
 BinHeightChange(Self);
end;

procedure TForm4.OnTimer(var Message:TWMTimer);
var
  WorkingLayerIdx,PreviewLayeridx: integer;
  contrast: single;
begin
  windows.KillTimer(self.Handle,Message.TimerID);
  timer := 0;

  WorkingLayerIdx := remoteglm.LayerByID(WorkingLayerID);
  PreviewLayeridx := remoteglm.LayerByID(PreviewLayerID);


  if WorkingLayeridx = -1 then
  begin
    Application.MessageBox('Source Layer is gone. Cannot perform image transformation.','Evident Failure',mb_OK);
    exit;
  end;

  if PreviewLayeridx = -1 then exit;

  contrast := 1 + (group.X*group.Y-1)*TrackBar1.Position/100;
  GLMFunctionality.BrightnessAndContrast(remoteGLM.Layers[PreviewLayerIdx].rawdata,
                             remoteGLM.Layers[PreviewLayerIdx].data,
                             contrast,
                             -(128 - 128 * contrast)/ contrast);



  RemoteGLM.ClientPaint(self);

end;

procedure TForm4.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // do not steal the wheel event ;)
  application.MainForm.OnMouseWheel(sender,Shift,WheelDelta,MousePos,Handled);
  Handled := true;
end;

end.
