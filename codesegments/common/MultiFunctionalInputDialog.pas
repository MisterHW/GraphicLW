unit MultiFunctionalInputDialog;

interface



  uses
    Windows, Messages, SysUtils,
    Classes, Forms, Graphics,
    Extctrls, Controls, ComCtrls, Stdctrls,
    Spin, Dialogs;




  type TInputField= Record
    cap: TLabel;
    valuefield: TComponent;
    miscfields: array of TObject; // optional
    inputtype, min, max, preset: string;
    height: integer;
    data: pointer;
    HasChangedBool: Pointer;
  end;


  type TMulipleOut = array[0..15] of Pointer;


  type TMultiFunctionalInputDialog=class(TObject)
     private
        InputFields: Array of TInputField;
        isdone, success: boolean;

        function  TopOffest: integer;
        procedure OnInfoClick(sender:TObject);
        procedure BuildOutputData(Sender:TObject);

        procedure OnAbortUp  (Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure OnProceedUp(Sender: TObject; var Key: Word; Shift: TShiftState);

        procedure OnAbort(Sender:TObject);
        procedure DisposeComponents;
        function  GetDialogWidth: integer;
        procedure SetDialogWidth(val: integer);
        procedure OnSaveClick(Sender:TObject);
        procedure OnTrackbarChange(Sender:TObject);

        procedure OnInputFieldKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

        procedure OnInfoPanelKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

        procedure AbortButtonMouseUp(Sender: TObject; Button: TMouseButton;
                      Shift: TShiftState; X, Y: Integer);
        procedure ProceedButtonMouseUp(Sender: TObject; Button: TMouseButton;
                      Shift: TShiftState; X, Y: Integer);

     public
        frm: TForm;
        okbutton: TButton;
        abortbutton: TButton;

        property Width: integer read GetDialogWidth write SetDialogWidth;
        constructor Create;
        destructor  Destroy; override;

        function    Execute: Boolean;
        procedure   AddInputField( caption: string;
                                   value: Pointer;
                                   InputType: string;
                                   Min: string ='';
                                   max: string='';
                                   preset: string='';
                                   haschanged: Pointer = nil;
                                   DefaultWidth : integer = 128);
  end;




implementation





////////////////////////////////////////////////////////////////////////////////////////
//                some basic routines                                                 //
////////////////////////////////////////////////////////////////////////////////////////




procedure SplitAndInsert(itemstr: string; separator: char; list: TStrings);
begin
    if itemstr='' then exit;
    while pos(separator, itemstr)>0 do
    begin
      list.Add(copy(itemstr,1,pos(separator, itemstr)-1));
      itemstr := copy(itemstr,pos(separator, itemstr)+1,length(itemstr)-pos(separator, itemstr));
    end;
    list.Add(itemstr);
end;



procedure CenterToParentForm(child, parent:THandle); stdcall;
  var
    r,c: TRect;
    w,h: integer;
    screenorg: TPoint;
begin
    windows.GetClientRect(parent,r);
    windows.GetWindowRect(child,c);
    screenorg := Point(0,0);
    if Windows.GetParent(child) = parent then
      Windows.ClientToScreen(parent,screenorg);
    w := c.Right -c.Left;
    h := c.Bottom-c.Top;
    windows.SetWindowPos( child,
                          0,
                          (r.Right -w) div 2 + screenorg.X,
                          (r.Bottom-h) div 2 + screenorg.Y,
                          w,
                          h,
                          0);
end;



function QuotientstringToFloat(s: string): Extended;
  var
    tracked: array of string;
    buf: string;
    i,a,b,c: integer;

    procedure addtotracked;
    begin
      setlength(tracked,length(tracked)+1);
      tracked[high(tracked)] := buf;
      buf := '';
    end;
begin
    result :=StrToFloatDef(s,-1);
    if result <> -1 then exit;

    buf := '';
    s := s+ ' ';
    for i := 1 to length(s) do
    begin
      if (s[i] >='0') and (s[i] <= '9')
        then buf := buf + s[i]
        else
          begin
            if buf <>'' then addtotracked;
          end;
    end;

    Case length(tracked) of
      1: result := StrToIntDef(tracked[0],-1);
      2: begin
           b := StrToIntDef(tracked[0],-1);
           c := StrToIntDef(tracked[1],-1);
           if c = 0 then result := -1  // divide by zero
                    else result := b/c;
         end;
      3: begin
           a := StrToIntDef(tracked[0], 0);
           b := StrToIntDef(tracked[1],-1);
           c := StrToIntDef(tracked[2],-1);
           if c = 0 then result := -1  // divide by zero
                    else result := a + b/c;
         end;
      else result := -1;
    end;
end;



function IsKeyMsg(var Msg: TMsg): Boolean; // taken from Application.IsKeyMsg() as used in Application.ProcessMessages
var
  Wnd: HWND;
begin
  Result := False;

  if (Msg.Message >= WM_KEYFIRST) and
     (Msg.Message <= WM_KEYLAST) then // message is a key message
  begin
      Wnd := GetCapture;              // get window that currently owns the mouse capture
      if Wnd = 0 then                 // ok, it was worth the try
      begin

        if (Application.MainForm <> nil) and
           (Msg.HWnd = Application.MainForm.ClientHandle) then // ClientHandle vs. Handle ?!
        begin // let the main form handle the message
          Wnd := Application.MainForm.Handle; // pass message to main form
        end
        else  // find the parent that has a child with the handle "Msg.HWnd)", then let the parent handle the message
        begin
          Wnd := Msg.HWnd;
          while (FindControl(Wnd) = nil) and (Wnd <> 0) // go upward in the hierarchy until the parent
            do Wnd := GetParent(Wnd);                   // is found. If no parent is found (GetParent reaches INVALID_HANDLE),
                                                        // just use the handle as is
          if Wnd = 0 then Wnd := Msg.HWnd;
        end;

        // usually, messages are passed directly to e.g. the input field that receives the key events
        // but there can be priviledged events, such as VK_TAB keydown that demand processing by the
        // parent of the control.
        if SendMessage(Wnd, CN_BASE + Msg.Message, Msg.WParam, Msg.LParam) <> 0 then Result := True;

      end
      else // pass to window that has capture assuming it is compatible
      begin

        if (LongWord(GetWindowLong(Wnd, GWL_HINSTANCE)) = HInstance) then
          if SendMessage(Wnd, CN_BASE + Msg.Message, Msg.WParam, Msg.LParam) <> 0 then Result := True;

      end;
  end;

end;



// select first TWinControl in Controls[] list (tab order assumed to be order of creation)
procedure FocusFirstControl(frm : TForm; ExcludeButtons : Boolean = false; ExcludePanels: Boolean = false);
var
  i: integer;
begin
  if not assigned(frm) then exit;

  for i := 0 to frm.ControlCount do
  begin
    if frm.Controls[i] is TWinControl and
      (not (frm.Controls[i] is TButton) or not ExcludeButtons) and
      (not (frm.Controls[i] is TPanel)  or not ExcludePanels) then
    begin
      (frm.Controls[i] as TWinControl).SetFocus;
      break;
    end;
  end;

end;


// select the next wincontrol. one could use the tablist from the form, however,
// this procedure selects according to order of creation.
procedure FocusNextControl(form: TForm; CurrentControl: TWinControl = nil; ClassStrict: Boolean = false);
  var
    i: integer;
    idx,new: integer;
begin
  idx := -1;
  if not assigned(form) then exit;
  if not assigned(CurrentControl) then CurrentControl := form.ActiveControl;
  if not assigned(CurrentControl) then exit;

  // retrieve index of wincontrol
  for i := 0 to form.ControlCount-1 do
  begin
    if form.Controls[i] = CurrentControl then
    begin
      idx := i;
      break;
    end;
  end;

  // if found, focus next wincontrol
  if idx <> -1 then
  begin
    for i := 1 to form.ControlCount-1 do
    begin
      new := (idx + i) mod form.ControlCount;
      if (form.Controls[new].ClassType = CurrentControl.ClassType) or not ClassStrict then // constrain to same type (e.g. cycle only TEdit
      if (form.Controls[new] is TWinControl) then
      begin
        (form.Controls[new] as TWinControl).SetFocus;
        break;
      end;
    end;
  end;

end;





////////////////////////////////////////////////////////////////////////////////////////
//             TPanelEx,   adding KeyUp and KeyDown events to TPanel                  //
////////////////////////////////////////////////////////////////////////////////////////


// need key events for the info panel, extend TPanel class
// and add highlight to visualize tab stop
type TPanelEx = Class(TPanel)
  constructor Create(Sender:TComponent); override;

private
  fOnKeyDown: TKeyEvent;
  fOnKeyUp: TKeyEvent;
  fOnKeyPress: TKeyPressEvent;
  fOnEnterPanel: TNotifyEvent;
  fOnExitPanel: TNotifyEvent;
  fHighlightColor: TColor;
  fBlurColor: TColor;

  procedure HandleKeyDownMessage(var Message: TWMKey); message WM_KEYDOWN;
  procedure HandleKeyUpMessage(var Message: TWMKey)  ; message WM_KEYUP;
  
  procedure SetHighlightColor(cl: TColor);
  procedure SetBlurColor(cl: TColor);

public
  property OnKeyDown  : TKeyEvent read fOnKeyDown write fOnKeyDown;
  property OnKeyUp    : TKeyEvent read fOnKeyUp write fOnKeyUp;
  property OnKeyPress : TKeyPressEvent read fOnKeyPress write fOnKeyPress;

  property HighlightColor: TColor  read fHighLightColor write SetHighlightColor;
  property BlurColor: TColor read fBlurColor write SetBlurColor;

  procedure PanelExitUpdate(Sender: TObject);
  procedure PanelEnterUpdate(Sender: TObject);
end;


// just set default colors, initialize
constructor TPanelEx.Create(Sender:TComponent);
begin
  inherited Create(Sender);
  fBlurColor := $E0E0E0;
  fHighlightColor := clwhite;
  Color := fBlurColor;
end;



// set color to highlight color
procedure TPanelEx.PanelEnterUpdate(Sender: TObject);
begin
  if not (sender is TPanel) then exit;
  (sender as TPanel).Color := fHighlightColor;
end;



// set color to standard color
procedure TPanelEx.PanelExitUpdate(Sender: TObject);
begin
  if not (sender is TPanel) then exit;
  (sender as TPanel).Color := fBlurColor;
end;



procedure TPanelEx.HandleKeyDownMessage(var Message: TWMKey);
begin
  if assigned(fOnKeyDown) then
  begin
    fOnKeyDown( self, Message.CharCode, KeyDataToShiftState(Message.KeyData) );
    Message.Result := 0;
  end
  else
    inherited;
end;



procedure TPanelEx.HandleKeyUpMessage(var Message: TWMKey);
begin
  if assigned(fOnKeyUp) then
  begin
    fOnKeyUp( self, Message.CharCode, KeyDataToShiftState(Message.KeyData) );
    Message.Result := 0;
  end
  else
    inherited;
end;



procedure TPanelEx.SetHighlightColor(cl: TColor);
begin
  fHighLightColor := cl;
  if self.Focused then self.Color := cl;
end;



procedure TPanelEx.SetBlurColor(cl: TColor);
begin
  fBlurColor := cl;
  if not self.Focused then self.Color := cl;
end;








////////////////////////////////////////////////////////////////////////////////////////
//                TMultiFunctionalInputDialog Class Implementation                    //
////////////////////////////////////////////////////////////////////////////////////////


  constructor TMultiFunctionalInputDialog.Create;
  begin
    // basic dialog gui implementation
    isdone:= false;
    frm := TFOrm.Create(nil);
    frm.BorderStyle := bssingle;
    frm.BorderIcons := [biSystemMenu];
      windows.SetParent(frm.Handle,Application.MainForm.Handle);
    frm.Width := 400;
    frm.FormStyle := fsStayOnTop;

    okbutton := TButton.Create(frm);
    okbutton.Parent := frm;
    okbutton.Left := frm.ClientWidth -4-okbutton.Width;
    okbutton.Caption := 'proceed';
    okbutton.OnKeyUp   := OnProceedUp;
    okbutton.OnMouseUp := ProceedButtonMouseUp;

    abortbutton := TButton.Create(frm);
    abortbutton.Parent := frm;
    abortbutton.Left := frm.ClientWidth -4-okbutton.Width-abortbutton.Width-16;
    abortbutton.Caption := 'abort';
    abortbutton.OnKeyUp := OnProceedUp;            // OnClick Event exchanged with key and mouse UP events
    abortbutton.OnMouseUp := AbortButtonMouseUp;   // using the DOWN event would leave the subsequent UP message unhandled
    //abortbutton.Default := true;                 // don't. Messes things up with tab order.
  end;



  destructor TMultiFunctionalInputDialog.Destroy;
  begin
    inherited;
  end;



  function TMultiFunctionalInputDialog.Execute: Boolean;
  var
    msg:tagMSG;
    ret: integer;
  begin
    result := false;
    success := false;

    if length(inputfields)=0 then
    begin
      Application.MessageBox(
        PChar('Cannot build an empty dialog.'#13#10'This may be due to missing implementation.'),
        PChar('MultifunctionalInputDialog execution error'),
        MB_ICONERROR or MB_OK);
      exit;
    end;

    frm.ClientHeight := self.TopOffest + InputFields[length(inputfields)-1].height+48;
    CenterToParentForm(frm.Handle,Application.MainForm.Handle);
    okbutton.Top := frm.ClientHeight -4-okbutton.Height;
    abortbutton.Top := okbutton.Top;

    frm.Show;
    CenterToParentForm(frm.Handle,Application.MainForm.Handle);
    FocusFirstControl(frm, true, true);

    repeat
      ret := integer(GetMessage(msg,0,0,0));

      if (ret <> 0) then
      begin
        if not IsKeyMsg(msg) then
        begin
        // translate message, only the usual...
        TranslateMessage(msg);

        // add ESC key support to abort dialog, do not use WM_KEYDOWN!
        if (msg.message = WM_KEYUP)  and
           (msg.wParam  = VK_ESCAPE) then
           begin
             self.isdone := true;
           end
           else
           begin
             // process other messages
             DispatchMessage(msg);
           end;
        end;
      end
      else
      begin
        result := false;
        application.Terminate;
        exit;
      end;

      if not IsWindowVisible(frm.Handle) then break;
    until isdone;

    self.DisposeComponents;
    result := success;
  end;



  function TMultiFunctionalInputDialog.TopOffest: integer;
  var i: integer;
  begin
    result := 0;
    for i := 0 to length(InputFields)-2 do
      result := result + InputFields[i].height;
  end;



  procedure TMultiFunctionalInputDialog.AddInputField(caption: string;
      value: Pointer; InputType: string; Min: string =''; max: string='';
      preset: string=''; HasChanged: Pointer = nil; DefaultWidth : integer = 128);
  var
    cid: integer;
    pancp: TPanelEx;
  const
    xalign = 128;

    procedure MakeField;
    begin
      setlength(Inputfields,length(Inputfields)+1);
      cid := length(InputFields)-1;

      InputFields[cid].height    := 32;
      InputFields[cid].preset    := preset;
      InputFields[cid].min       := min;
      InputFields[cid].max       := max;
      InputFields[cid].inputtype := inputtype;
      InputFields[cid].data      := value;
      InputFields[cid].HasChangedBool := HasChanged;

      if assigned(HasChanged) then Boolean(HasChanged^) := FALSE;

      if InputType ='numeric'  then InputFields[cid].height := 28;
      if InputType ='float'    then InputFields[cid].height := 28;
      if InputType ='quotient' then InputFields[cid].height := 28;
      if InputType ='string'   then InputFields[cid].height := 28;
      if InputType ='static'   then InputFields[cid].height := 28;
      if InputType ='info'     then InputFields[cid].height := 32;
      if InputType ='combo'    then InputFields[cid].height := 28;
      if InputType ='save'     then InputFields[cid].height := 28;
      if InputType ='trackbar' then InputFields[cid].height := 38;

      InputFields[cid].cap := TLabel.Create(frm);
      InputFields[cid].cap.Parent := frm;
      InputFields[cid].cap.Caption := caption;
      InputFields[cid].cap.Top := TopOffest +
          (InputFields[cid].Height - InputFields[cid].cap.Canvas.TextHeight('W')) div 2; // center
      InputFields[cid].cap.Left := 4;

      if (InputType ='numeric') or
         (InputType ='float') or
         (InputType ='string' ) then
      begin
        InputFields[cid].valuefield := TEdit.Create(frm);
        with (InputFields[cid].valuefield as TEdit)do
        begin
          parent := frm;
          width:= DefaultWidth;
          text := preset;
          left := frm.ClientWidth -4 - width;
          top := TopOffest + 2;
          OnKeyUp := OnInputFieldKeyUp;
        end;
      end;

      if InputType ='quotient' then
      begin
        InputFields[cid].valuefield := TEdit.Create(frm);
        with (InputFields[cid].valuefield as TEdit)do
        begin
          parent := frm;
          width:= DefaultWidth;
          text := preset;
          left := frm.ClientWidth -4- width;
          top := TopOffest + 2;
          OnKeyUp := OnInputFieldKeyUp;
        end;
      end;

      if InputType ='static' then
      begin
        InputFields[cid].valuefield := TEdit.Create(frm);
        with (InputFields[cid].valuefield as TEdit)do
        begin
          parent := frm;
          width:= frm.ClientWidth-8 - xalign;
          text := preset;
          left := xalign;
          readonly := true;
          borderstyle := bsnone;
          color := frm.Color;
          top := TopOffest + 2;
          OnKeyUp := OnInputFieldKeyUp;
        end;
      end;

      if InputType ='info' then
      begin
        InputFields[cid].data := TPanelEx.Create(frm);
        pancp := InputFields[cid].data;
        pancp.Width := frm.ClientWidth+2;
        pancp.Left := -1;
        pancp.Top := TopOffest;
        pancp.Color := $E0E0E0;
        pancp.Bevelinner := bvnone;
        pancp.Height := InputFields[cid].height;
        pancp.Parent := frm;
        pancp.TabStop := true;
        pancp.Tag := cid;

        pancp.OnEnter := pancp.PanelEnterUpdate; // default handlers for background color change
        pancp.OnExit  := pancp.PanelExitUpdate;
        pancp.OnKeyDown := OnInfoPanelKeyDown;

        InputFields[cid].cap.Top := 8;
        InputFields[cid].cap.Font.Style := [fsbold];
        inputfields[cid].cap.Parent := pancp;
        InputFields[cid].valuefield := TLabel.Create(pancp);

        with (InputFields[cid].valuefield as TLabel)do
        begin
          parent := pancp;
          hint := preset;
          caption := '[ ? ]';
          Font.Style := [fsbold];
          left := frm.ClientWidth -4- width;
          top := 6;
          Onclick := self.OnInfoClick;
          if preset='' then visible := false;
        end;

      end;

      if InputType ='combo' then
      begin
        InputFields[cid].valuefield := TCombobox.Create(frm);
        with (InputFields[cid].valuefield as TCombobox) do
        begin
          parent := frm;
          width:= DefaultWidth;
          SplitAndInsert(preset,'|',items);
          text := min;
          left := frm.ClientWidth -4 -width;
          top := TopOffest+2;
          OnKeyUp := OnInputFieldKeyUp;
        end;
      end;

      if InputType ='indexcombo' then
      begin
        InputFields[cid].valuefield := TCombobox.Create(frm);
        with (InputFields[cid].valuefield as TCombobox) do
        begin
          parent := frm;
          width:= DefaultWidth;
          SplitAndInsert(preset,'|',items);
          text := Items.Strings[StrToIntDef(InputFields[cid].min,0)];
          ItemIndex := StrToIntDef(InputFields[cid].min,0);
          Style := csDropDownList;
          left := frm.ClientWidth -4 -width;
          top := TopOffest+2;
          OnKeyUp := OnInputFieldKeyUp;
        end;
      end;

      if InputType ='spin' then
      begin
        InputFields[cid].valuefield := TSpinEdit.Create(frm);
        with (InputFields[cid].valuefield as TSpinEdit) do
        begin
          parent := frm;
          width:= DefaultWidth;
          left := frm.ClientWidth -4 -width;
          top := TopOffest +2;
          value := StrToIntDef(preset,0);
          MaxValue := StrToIntDef(max,0);
          MinValue := StrToIntDef(min,0);
        end;
      end;

      if InputType ='save' then
      begin
        setlength(InputFields[cid].miscfields,2);

        InputFields[cid].miscfields[0] := TEdit.Create(frm);
        with (InputFields[cid].miscfields[0] as TEdit) do
        begin
          parent := frm;
          width:= frm.ClientWidth-8 - xalign -20;
          text := preset;
          left := xalign;
          top := TopOffest +2;
          OnKeyUp := OnInputFieldKeyUp;
        end;

        InputFields[cid].miscfields[1] := Tbutton.Create(frm);
        with (InputFields[cid].miscfields[1] as TButton) do
        begin
          parent := frm;
          width:= 22;
          height:=21;
          caption := '...';
          left := frm.ClientWidth -4 -width;
          top := TopOffest +2;
          tag := integer(Addr(InputFields[cid]));
          onclick := self.OnSaveClick;
        end;
      end;

      if InputType ='trackbar' then
      begin
        setlength(InputFields[cid].miscfields,2);

        InputFields[cid].miscfields[0] := TEdit.Create(frm);
        with (InputFields[cid].miscfields[0] as TEdit) do
        begin
          parent := frm;
          width:= 48;
          text := preset;
          left := xalign;
          borderstyle := bsnone;
          bevelkind := bkflat;
          top := TopOffest +4 + 3;
          OnKeyUp := OnInputFieldKeyUp;
        end;

        InputFields[cid].miscfields[1] := TTrackbar.Create(frm);
        (InputFields[cid].miscfields[1] as TTrackbar).max := StrToIntDef(max,0);
        (InputFields[cid].miscfields[1] as TTrackbar).min := StrToIntDef(min,0);
        with (InputFields[cid].miscfields[1] as TTrackbar) do
        begin
          parent := frm;
          width:= frm.ClientWidth - xalign - 48;
          left := xalign+48;
          top := TopOffest +4;
          position := StrToIntDef(preset,0);
          tag := (InputFields[cid].miscfields[0] as TEdit).Handle;
          //  well, apperently an address pointer is no longer valid on the basis
          // of a changed memory view, so we have to use the window handle of the edit ...
          OnChange := self.OnTrackbarChange;
        end;
      end;
   end;

  begin
    MakeField;
  end;



  procedure TMultiFunctionalInputDialog.OnInfoClick(sender:TObject);
  begin
    self.frm.Hide;
    Application.MessageBox(
        PChar((sender as TLabel).Hint),
        PChar(frm.caption),
        MB_ICONINFORMATION or MB_OK);
    self.frm.Show;
    frm.SetFocus;
  end;



  procedure TMultiFunctionalInputDialog.BuildOutputData(Sender:TObject);
  var
    pinteger: ^integer;
    pstring : ^string;
    pdouble : ^double;
    i: integer;
  begin
    // build output
    for i := 0 to length(InputFields)-1 do
    begin
      if (InputFields[i].inputtype = 'string') or
         (InputFields[i].inputtype = 'quotient') then
      begin
        pstring  := InputFields[i].data;
        pstring^ := (InputFields[i].valuefield as TEdit).Text;
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := InputFields[i].preset <> pstring^;
      end;

      if InputFields[i].inputtype = 'combo' then
      begin
        pstring  := InputFields[i].data;
        pstring^ := (InputFields[i].valuefield as TCombobox).Text;
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := InputFields[i].min <> pstring^;
      end;

      if InputFields[i].inputtype = 'indexcombo' then
      begin
        pinteger  := InputFields[i].data;
        pinteger^ := (InputFields[i].valuefield as TCombobox).ItemIndex;
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := strtointdef(InputFields[i].min,0) <> pinteger^;
      end;

      if InputFields[i].inputtype = 'numeric' then
      begin
        pinteger  := InputFields[i].data;
        pinteger^ := strtointdef((InputFields[i].valuefield as TEdit).Text,StrToIntDef(InputFields[i].preset,0));
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := strtointdef(InputFields[i].preset,0) <> pinteger^;
      end;

      if InputFields[i].inputtype = 'float' then
      begin
        pdouble  := InputFields[i].data;
        pdouble^ := strtoFloatdef((InputFields[i].valuefield as TEdit).Text,StrToFloatDef(InputFields[i].preset,0));
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := strtoFloatdef(InputFields[i].preset,0) <> pdouble^;
      end;

      if InputFields[i].inputtype = 'spin' then
      begin
        pinteger  := InputFields[i].data;
        pinteger^ := (InputFields[i].valuefield as TSpinEdit).Value;
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := strtointdef(InputFields[i].preset,0) <> pinteger^;
      end;

      if InputFields[i].inputtype = 'save' then
      begin
        pstring  := InputFields[i].data;
        pstring^ := (InputFields[i].miscfields[0] as TEdit).Text;
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := InputFields[i].preset <> pstring^;
      end;

      if InputFields[i].inputtype = 'trackbar' then
      begin
        pinteger  := InputFields[i].data;
        pinteger^ := StrToIntDef((InputFields[i].miscfields[0] as TEdit).Text,-1);
        if assigned(InputFields[i].HasChangedBool) then
            Boolean(InputFields[i].HasChangedBool^) := strtointdef(InputFields[i].preset,0) <> pinteger^;
      end;

    end;  // for

    self.success := true;
    self.isdone  := true;
  end;



  procedure TMultiFunctionalInputDialog.OnAbort(Sender:TObject);
  begin
    self.isdone := true;
  end;



  procedure TMultiFunctionalInputDialog.OnSaveClick(Sender:TObject);
  var
    sd: TSavedialog;
    params: ^TInputField;
    ed: TEdit;
  begin
    params := Pointer((Sender as TButton).Tag);
    if not assigned(params) then exit;
    sd := TSavedialog.Create(frm);
    sd.Filter := params^.min;
    sd.DefaultExt := params^.max;
    ed := params^.miscfields[0] as TEdit;
    sd.InitialDir := ExtractFilePath(ed.Text);
    sd.FileName := ExtractFileName(ed.text);
    if sd.Execute then ed.Text := sd.FileName;
    sd.Free;
  end;



  procedure TMultiFunctionalInputDialog.DisposeComponents;
  var i,k: integer;
  begin
     frm.Hide;
     for i := 0 to length(InputFields)-1 do
     begin
       InputFields[i].cap.Free;
       if assigned(InputFields[i].valuefield) then InputFields[i].valuefield.Free;
       for k := 0 to length(InputFields[i].miscfields)-1 do
         InputFields[i].miscfields[k].Free;
     end;
     setlength(InputFields,0);
     okbutton.Free;
     abortbutton.Free; 
     frm.Free;
  end;



  function  TMultiFunctionalInputDialog.GetDialogWidth: integer;
  begin
    result := self.frm.Width;
  end;



  procedure TMultiFunctionalInputDialog.SetDialogWidth(val: integer);
  begin
    self.frm.Width := val;
    okbutton.Left    := frm.ClientWidth -4 -okbutton.Width;
    abortbutton.Left := frm.ClientWidth -4 -okbutton.Width - abortbutton.Width -16;
  end;


  // update associated number field as referenced by the handle in the tag property
  procedure TMultiFunctionalInputDialog.OnTrackbarChange(Sender:TObject);
  var
    tracker: TTrackbar absolute Sender;
    hwnd: THandle;
  begin
    if not (Sender is TTrackbar) then exit;
    hwnd := tracker.Tag; // as set, see creation code
    Windows.SetWindowText(hwnd, PChar(IntToStr(tracker.Position)));
  end;


  // pass focus to next WinControl
  procedure TMultiFunctionalInputDialog.OnInputFieldKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if (ord(key) and $FF = VK_RETURN) and // next control by pressing ENTER instead of TAB for edit fields
       (Sender is TWinControl) and
       (Sender = frm.ActiveControl) then
    begin
      FocusNextControl(frm,(Sender as TWinControl));
    end;
  end;


  // wrapper for former abortbutton.OnClick event
  procedure TMultiFunctionalInputDialog.OnAbortUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if key and $FF =  VK_RETURN then self.OnAbort(Sender);
  end;


  // wrapper for former proceedbutton.OnClick event
  procedure TMultiFunctionalInputDialog.OnProceedUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if key and $FF =  VK_RETURN  then BuildOutputData(sender);
  end;


  // wrapper for former abortbutton.OnClick event
  procedure TMultiFunctionalInputDialog.AbortButtonMouseUp(Sender: TObject; Button: TMouseButton;
                      Shift: TShiftState; X, Y: Integer);
  begin
    OnAbort(Sender);
  end;


  // wrapper for former proceedbutton.OnClick event
  procedure TMultiFunctionalInputDialog.ProceedButtonMouseUp(Sender: TObject; Button: TMouseButton;
                      Shift: TShiftState; X, Y: Integer);
  begin
    BuildOutputData(Sender);
  end;



  procedure TMultiFunctionalInputDialog.OnInfoPanelKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  var
    pan: TPanel;
  begin
    if key and $FF =  VK_RETURN  then
    begin
      if not (Sender is TPanel) then exit;
      pan := (Sender as TPanel);
      OnInfoClick((InputFields[pan.Tag].valuefield as TLabel));
    end;
  end;






end.
