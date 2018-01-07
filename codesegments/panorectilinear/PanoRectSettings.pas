unit PanoRectSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Math, PanoTransformClasses;

type
  TPanoRectSettingsDlg = class(TForm)
    GroupBox1: TGroupBox;
    ftracker: TTrackBar;
    ftext: TEdit;
    thetatext: TEdit;
    thetatracker: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    phitext: TEdit;
    phitracker: TTrackBar;
    Label4: TLabel;
    psitext: TEdit;
    psitracker: TTrackBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GroupBox1Click(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure TrackerKeyPress(Sender: TObject; var Key: Char);

    procedure OnTimer(var Message:TWMTimer); Message WM_TIMER;
    procedure TrackbarTimerProc(var Message:TWMTimer);
    procedure TrackerChange(Sender: TObject);
    procedure TextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    tbtimer: longint;
    frtimer: longint;
    isrendering: boolean;
    SendUpdates: Boolean;
    coarse : Boolean;
  public
    OnParametersUpdated : TNotifyEvent;

    procedure UpdateParameters(var par: TViewportParameters);
    procedure GetParameters(var par: TViewportParameters);
    procedure StopTimers(Sender: TObject);
  end;

var
  PanoRectSettingsDlg: TPanoRectSettingsDlg;

const
  TrackbarTimer   = 1;
  FineRenderTimer = 2;

implementation

{$R *.dfm}

procedure TPanoRectSettingsDlg.OnTimer(var Message:TWMTimer);
begin
  case Message.TimerID of
    TrackbarTimer   : TrackbarTimerProc(Message);
    FineRenderTimer : begin
                        Coarse := false;
                        windows.KillTimer(self.Handle,Message.TimerID);
                        if assigned( OnParametersUpdated ) then OnParametersUpdated(self);
                      end;
  end;
end;



procedure TPanoRectSettingsDlg.TrackbarTimerProc(var Message:TWMTimer);
begin
  if isrendering then exit;
  windows.KillTimer(self.Handle,Message.TimerID);
  tbtimer := 0;
  // ReRenderPreview(self);
  // RemoteGLM.ClientPaint(self);
  if assigned( OnParametersUpdated ) then OnParametersUpdated(self);
end;



procedure TPanoRectSettingsDlg.TrackerChange(Sender: TObject);
var
  tracker: TTrackbar absolute sender;
  text: TEdit;
begin
  KillTimer(self.Handle,TrackbarTimer); tbtimer := 0;
  KillTimer(self.Handle,FineRenderTimer); frtimer := 0;
  tbtimer := SetTimer(self.Handle,TrackbarTimer,25,nil);
  frtimer := SetTimer(self.Handle,FineRenderTimer,200,nil);
  coarse := true;

  if tracker = ftracker     then text := ftext     else
  if tracker = thetatracker then text := thetatext else
  if tracker = phitracker   then text := phitext   else
  if tracker = psitracker   then text := psitext   else exit;

  if tracker <> ftracker then
  begin
    tracker.Selstart := min((tracker.Max + tracker.Min) div 2,
                          tracker.Position);
    tracker.SelEnd   := max((tracker.Max + tracker.Min) div 2,
                          tracker.Position);
  end;

  text.Text := floattostr(tracker.Position / 100);

  if assigned(OnParametersUpdated) and SendUpdates
    then OnParametersUpdated(self);
end;



procedure TPanoRectSettingsDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;



procedure TPanoRectSettingsDlg.GroupBox1Click(Sender: TObject);
begin
  self.SetFocus;
end;



procedure TPanoRectSettingsDlg.EditClick(Sender: TObject);
begin
  if Sender is TEdit then (Sender as TEdit).SetFocus;
end;



procedure TPanoRectSettingsDlg.TrackerKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key = '0' then
    if (Sender is TTrackBar) then
      (Sender as TTrackBar).Position := (Sender as TTrackBar).Tag; // presets are stored in "tag"
end;




procedure TPanoRectSettingsDlg.TextChange(Sender: TObject);
var
  text : TEdit absolute Sender;
  tracker: TTrackbar;
begin
  if text = ftext     then tracker := ftracker     else
  if text = thetatext then tracker := thetatracker else
  if text = phitext   then tracker := phitracker   else
  if text = psitext   then tracker := psitracker   else exit;

  tracker.Position := round(100*StrToFloatDef((Sender as TEdit).Text,90));
end;


procedure TPanoRectSettingsDlg.UpdateParameters(var par: TViewportParameters);
var
  dummy: single;
begin
   SendUpdates := false;

   ftracker.Position := round(par._focallen *100);

   thetatracker.Position := -round(par.theta *100);

   if par.phi >180 then dummy := par.phi-360 else dummy := par.phi;
   phitracker.Position := round(dummy *100);

   if par.psi > 180 then dummy := par.psi-360 else
   if par.psi <-180 then dummy := par.psi+360 else dummy := par.psi;
   psitracker.Position := round(dummy *100);

   SendUpdates := true;
end;



procedure TPanoRectSettingsDlg.FormCreate(Sender: TObject);
begin
  SendUpdates := true;
end;


procedure TPanoRectSettingsDlg.GetParameters(var par: TViewportParameters);
begin
  par.coarsepreview := coarse;
  par._focallen := ftracker.Position / 100;
  par.theta := - thetatracker.Position / 100;
  par.phi := phitracker.Position / 100;
  par.psi := psitracker.Position / 100;
end;

procedure TPanoRectSettingsDlg.StopTimers(Sender: TObject);
begin
  KillTimer(self.Handle,TrackbarTimer)  ; tbtimer := 0;
  KillTimer(self.Handle,FineRenderTimer); frtimer := 0;
end;

end.
