unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  GraphicLayerManager, GLMFunctionality, GLWdef, GLWFilters, ExtCtrls;

type
  TForm5 = class(TForm)
    GroupBox1: TGroupBox;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure TrackBar2KeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnTimer(var Message:TWMTimer); Message WM_TIMER;
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  private
    timer: integer;
  public
    RemoteGLM:TGraphicLayerManager;
    OnToolAbort : TNotifyEvent;
    OnUpdateCurrentLayer : LongintProc;
    ToolStatus: integer;
    WorkingLayerID: longint;
    procedure ResetTool;
    procedure PreviewFilterProc(LayerID: integer; target: TBitmap; rct: TRect);
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.TrackBar2KeyPress(Sender: TObject; var Key: Char);
begin
 if key = '0' then Trackbar2.Position := 0;
end;

procedure TForm5.CheckBox1Click(Sender: TObject);
begin
  case CheckBox1.Checked of
    true:RemoteGLM.InsertPreviewHook(self.PreviewFilterProc);
    false:RemoteGLM.RemovePreviewHook(self.PreviewFilterProc);
  end;
  RemoteGLM.ClientPaint(self);
end;

procedure TForm5.ResetTool();
begin
  //
end;

procedure TForm5.PreviewFilterProc(LayerID: integer; target: TBitmap; rct: TRect);
begin
  if LayerID = WorkingLayerID then 
  GLMFunctionality.BrightnessAndContrast(
    target,
    rct,
    exp(3*Trackbar1.Position/Trackbar1.Max),
    TrackBar2.Position);
  // render here
end;

procedure TForm5.Button2Click(Sender: TObject);
begin
  KillTimer(self.Handle,timer); timer := 0;
  form5.Hide;
  if assigned(self.OnToolAbort) then self.OnToolAbort(self);
end;

procedure TForm5.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // do not steal the wheel event ;)
  application.MainForm.OnMouseWheel(sender,Shift,WheelDelta,MousePos,Handled);
  Handled := true;
end;

procedure TForm5.Button1Click(Sender: TObject);
var
  filterdata : PStandardFilterParameters;
  CurrentLayer :  integer;
  CurrentFilter:  integer;
begin
  KillTimer(self.Handle,timer); timer := 0;

  getmem(filterdata,sizeof(TStandardFilterParameters));

  // configure filter
  filterdata^.s1 := exp(3*Trackbar1.Position/Trackbar1.Max); // contrast
  filterdata^.i1 := TrackBar2.Position; // brightness


  CurrentLayer := RemoteGLM.LayerByID(WorkingLayerID);
  if CurrentLayer = -1 then exit;

  CurrentFilter := RemoteGLM.InsertFilter(
    CurrentLayer,
    FT_BRIGHTNESS_AND_CONTRAST,
    filterdata,
    sizeof(TStandardFilterParameters));
  glwfilters.ExecuteFilterAction(remoteglm.layers[CurrentLayer].data,
              remoteglm.layers[CurrentLayer].FilterHeap[CurrentFilter]);
  remoteGLM.layers[CurrentLayer].FilterHeapCursor := CurrentFilter;

  form5.Hide;
  if assigned(self.OnToolAbort) then self.OnToolAbort(self);
end;



procedure TForm5.TrackBar1Change(Sender: TObject);
begin
  KillTimer(self.Handle,timer); timer := 0;
  timer := SetTimer(self.Handle,timer,50,nil);
  label3.Caption := inttostr(round(100*exp(3*Trackbar1.Position/Trackbar1.Max)))+'%';
end;

procedure TForm5.TrackBar2Change(Sender: TObject);
begin
  KillTimer(self.Handle,timer); timer := 0;
  timer := SetTimer(self.Handle,timer,50,nil);
  label4.Caption := inttostr(round(50*(1+TrackBar2.Position/trackbar2.Max)))+'%';
end;

procedure TForm5.OnTimer(var Message:TWMTimer);
//var
  //WorkingLayerIdx : integer;
  //PreviewLayeridx: integer;
 // contrast: single;
begin
  windows.KillTimer(self.Handle,Message.TimerID);
  timer := 0;
  RemoteGLM.ClientPaint(self);
end;


procedure TForm5.FormShow(Sender: TObject);
begin
  checkbox1.OnClick(self);
  button1.SetFocus;
end;

procedure TForm5.Panel1Click(Sender: TObject);
begin
  trackbar1.Position := 0;
end;

procedure TForm5.Panel2Click(Sender: TObject);
begin
  trackbar2.Position := 0;
end;

end.
