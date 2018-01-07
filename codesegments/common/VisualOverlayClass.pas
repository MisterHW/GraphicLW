unit VisualOverlayClass;

interface
uses
  Windows, Controls, Types, Classes, Graphics, Math, Forms,
  MMXBLend;

// TVisualOverlay is a virtual superclass from which selection masks, markers,
// and interactive GUI components can be derived. Basic keyboard and mouse events
// are passed by the Graphic Layer Manager as well as custom rendering calls that
// demand the object to be drawn.
//
// if the object is active, it can enquire redrawing from the parent object (GLM, TVisualOverlayPanelEx)
//
// visual overlay objects are drawn top-most, render overlays like post-rendering
// hook procedures are subject to the z order of the layers. Apart from that, their
// major purpose is user interaction.

type TVisualOverlay = class(TObject)
    Constructor Create();

  protected
     // every Viual Object needs at least some origin when it comes to drawing itself
     ScreenPosition: TPoint;

     // for correct rendering, there is also a scaling factor needed
     ScreenScaling: Single;

     // the screen position can be, but is not necessarily calculated
     // by the owner object. The GLM generates screen coordinates from
     // absolute coordinates, that are of main interest for selection masks.
     // additional objects don't have absolute positions, only a screen position
     AbsolutePosition: TPoint;

     // property moved from overlaypanel to overlay. Reason: local repaint
     // capabilities are major functionality.
     _boundsrect: TRect;

     // this object has a UID
     _UID: longint;


     // visibility property
     _visible: Boolean;

     // processing flags
     _HandlesWheelEvents: Boolean;
     _HandlesMouseEvents: Boolean;
     _HandlesKeyEvents  : Boolean;

     procedure SetVisibility(IsVisible: Boolean);

  public
    // to enable hand-over of VisualOverlay objects, a record must be kept of whom the
    // Object belongs to, e.g. a handler object that must release control over this instance.
    // Owner may be TVisualOverlayHandler. Variable is set and changed by insertion/removal routines.
    Owner: TObject;

    // OnPaint-Event is called when a visual overlay repaints itself. In general, the OnPaint-
    // event is triggered after drawing is done but in some cases it can be called during rendering
    // e.g. for buffered overlays that depend on image content that is drawn in between steps.
    OnPaint: TNotifyEvent;

    // ParentRepaint needs to be called if the user interaction has changed the appearance of
    // the visual overlay object, e.g. a drag motion has shifted the position of the object
    ParentRepaint : TNotifyEvent;

    // if only content within the Object rendering area has changed and no transparency
    // is used, a call for simple Refreshing of the object apperance can be performed
    // which is much faster
    ParentLocalRepaint : TNotifyEvent;

    // apart from CursorIsInside, let's introduce a flag that controls wheel event handling
    // if true, mousewheel events will be sent to an instance of this class
    // add some more flags in case they are needed. These are set on creation
    property HandlesWheelEvents: Boolean read _HandlesWheelEvents;
    property HandlesMouseEvents: Boolean read _HandlesMouseEvents;
    property HandlesKeyEvents  : Boolean read _HandlesKeyEvents;

    // UID is read-only
    property UID : longint read _UID;

    // BoundsRect specifies the dimension of the overlay or the
    // minimum redraw araa, if the latter larger
    property BoundsRect : TRect read _boundsrect;

    // visibility requires redrawing on change
    property Visible : Boolean read _visible write SetVisibility;
    procedure Show;
    procedure Hide;

    // The owner object demands the Overlay Object to be drawn whenever something has changed
    // or the parent canvas needs repainting
    procedure Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single); virtual; abstract;

    // decide if on mousedown the mouse event can be
    // passed to the viual object or needs to be forwarded to the main handler.
    // The result depends upon the modifyability (if) and the polygonal
    // shape (how/when) that is defined by the derived Class, by the screen
    // position (translation) and scaling (ScreenScaling)
    //
    // if Shift is ssRight, the result can always be true
    function CursorIsInside(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean; virtual;

    // user interface events will be received if CursorIsInside returns true
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseMove(Sender: TObject;
                  Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); virtual; abstract;

    // the owner can send enter and leave events
    Procedure MouseEnter(Sender: TObject); virtual; abstract;
    Procedure MouseLeave(Sender: TObject); virtual; abstract;

    // click events
    Procedure Click(Sender: TObject); virtual; abstract;
    Procedure DblClick(Sender: TObject); virtual; abstract;
    Procedure KeyPress(Sender: TObject; var Key: Char); virtual; abstract;
    Procedure KeyDown(Sender: TObject;  var Key: Word; Shift: TShiftState); virtual; abstract;
    Procedure KeyUp(Sender: TObject;    var Key: Word; Shift: TShiftState); virtual; abstract;

    // mouse wheel events need to be implemented. If the cursor is within the
    // object area (Enter: set, Leave: clear flag) wheel events can be processed
    // exclusively, passing a "handled" variable value.
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual; abstract;

    // for anchored objects recalculation of boundary rectangle and margins
    // is forced when the screen area changes. Overlay objects do NOT reply
    // with a ParentRepaint call, because the area change is immediately followed
    // by repainting the entire area, in the cause of this which the overlay objects
    // are asked to paint themselves at the right point of time
    procedure UpdateParentAreaChanges(clientrect: TRect); virtual;
end;




type TCursorInsideStateRequest = function(
          Button: TMouseButton;
          Shift: TShiftState;
          MousePos: TPoint): Boolean of object;

type TOverlayPaintEvent = procedure(
          Destination: TBitmap;
          AbsoluteOrigin: TPoint;
          CurrentScaling: single) of Object;

    // TVisualOverlaySurface offers standard panel behaviour but no rendering is performed
    // effectively making it an event capturing surface with custom draw. For more, see
    // TVisualOverlayPanel.
type TVisualOverlaySurface= Class(TVisualOverlay)
    Constructor Create(ClientRect: TRect);
  protected
    _caption : string;
    _margin : TRect;
    _color: TColor;
    _bordercolor : TColor;
    function GetLeft: integer;virtual;
    function getTop : integer; virtual;
    function GetWidth : integer; virtual;
    function getHeight : integer; virtual;
    procedure SetLeft(int : integer); virtual;
    procedure SetTop(int: integer); virtual;
    procedure SetWidth(int: integer); virtual;
    procedure SetHeight(int: integer); virtual;
    procedure SetCaption(new: string); virtual;
    procedure SetColor(new:TColor); virtual;
    procedure SetBorderColor(new:TColor);  virtual;
  public
    OnClick: TNotifyEvent;
    OnDblClick: TNotifyEvent;
    OnPaint: TNotifyEvent;
    OnPaintEx: TOverlayPaintEvent;
    OnMouseDown: TMouseEvent;
    OnMouseMove: TMouseMoveEvent;
    OnMouseUp: TMouseEvent;
    OnMouseWheel: TMouseWheelEvent;
    OnKeyDown: TKeyEvent;
    OnKeyPress: TKeyPressEvent;
    OnKeyUp: TKeyEvent;
    OnCursorInsideStateRequest : TCursorInsideStateRequest;

    Anchors: TAnchors;

    property BoundsRect : TRect read _boundsrect;
    property Margin : TRect   read _margin;
    Property Top    : integer read GetTop write SetTop;
    property Left   : integer read getLeft write SetLeft;
    property Width  : integer read GetWidth write SetWidth;
    property Height : integer read Getheight write SetHeight;
    property Color  : TColor  read _color write SetColor;
    property BorderColor  : TColor  read _bordercolor write SetBorderColor;
    property Caption : string read _caption write SetCaption;

    procedure Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single); override;
    function CursorIsInside(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean; override;

    procedure SetBounds(bounds:TRect; parentarea: TRect); virtual;
    procedure UpdateParentAreaChanges(clientrect: TRect); override;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject;
                  Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); override;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
    Procedure MouseEnter(Sender: TObject); override;
    Procedure MouseLeave(Sender: TObject); override;

    Procedure Click(Sender:TObject); override;
    Procedure DblClick(Sender: TObject); override;
    Procedure KeyPress(Sender: TObject; var Key: Char); override;
    Procedure KeyDown(Sender: TObject;  var Key: Word; Shift: TShiftState); override;
    Procedure KeyUp(Sender: TObject;    var Key: Word; Shift: TShiftState); override;
end;




// this is a screen static overlay panel object. If a scaling rectangle is
// needed, implement TVisualOverlayRect. Scaling or non-scaling behaviour is none
// of the superclasses business
type TVisualOverlayPanel = Class(TVisualOverlaySurface)
public
  function CursorIsInside(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean; override;

  procedure Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single); override;
end;




type TVisualOverlayProgressBar = class(TVisualOverlayPanel)
    Constructor Create(ClientRect: TRect);
  private
    _position: double;
    _min,_max : double;
    procedure SetMin(new: double);
    procedure SetMax(new: double);
    procedure SetPosition(new: double);
  public
    OnPaint: TNotifyEvent;


    property min : double read _min write SetMin;
    property max : double read _max write SetMax;
    property position : double read _position write SetPosition;
    
    procedure Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single); override;
end;




type TBufferedVisualOverlay = class(TVisualOverlayPanel)
    Constructor Create(ClientRect: TRect);
    Destructor Destroy(); override;

  private
    Buffer, bgbuffer: TBitmap;
    _translucent: Boolean;
    alpha : Byte;

    procedure SetTranslucency(new: Boolean); virtual;

  protected
    procedure SetWidth(int: integer); override;
    procedure SetHeight(int: integer); override;
  public
    UseBackgroundBuffering: Boolean;

    property Translucent: Boolean read _translucent write SetTranslucency;
    property ScreenBuffer: TBitmap read Buffer;
    procedure Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single); override;
    procedure UpdateParentAreaChanges(clientrect: TRect); override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); override;
    function CursorIsInside(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean; override;
end;




type TVisualOverlayButton = class(TVisualOverlayPanel)
    Constructor Create(ClientRect: TRect);
  public
    procedure Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single); override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); override;
end;




Type TVisualOverlayHandler = class(TObject)
    constructor Create(Target: TBitmap);
    destructor Destroy; override;
  private
    VisualOverlays : Array Of TVisualOverlay;
    EventLinkedVisualUID: longint;
    DownEventShiftState: TShiftState;
    _zoom: Single;
    zoompoint: TPoint;
    _surpressrepaint: Boolean;
    _ReferenceRect: TRect;


  public
    // ClientPaint: TNotifyEvent;
    Buffer : TBitmap;
    Origin : TPoint;
    Parent : TObject;
    ParentVisible : Boolean;

    BeforeClientPaint: TNotifyEvent;
    AfterClientPaint : TNotifyEvent;
    BeforeClientPaintLocally: TNotifyEvent;
    AfterClientPaintLocally : TNotifyEvent;

    property ReferenceRect : TRect read _ReferenceRect;
    procedure SetReferenceRect(new: TRect; Refresh: Boolean = false);
    property Zoom: Single read _zoom write _zoom;


    procedure ClientPaint(Sender: TObject);
    procedure OnParentResize(Sender:TObject);
    procedure surpressrepaint;

    function CanHandleMouseEvent(pos: TPoint;Shift: TShiftState;
                 Button:TMouseButton = mbLeft): Boolean;

    procedure InsertVisualOverlayObject(newobj: TVisualOverlay);
    function  RemoveVisualOverlayObject(obj: TVisualOverlay):TVisualOverlay; overload;
    function  RemoveVisualOverlayObject(UID: longint):TVisualOverlay; overload;
    function  GetVisualOverlayObjectByID(UID: longint): TVisualOverlay;
    function  GetVisualOverlayUIDByPos(pos: TPoint;Shift: TShiftState;
                 Button:TMouseButton = mbLeft): longint;
    procedure UpdateVisualOverlays(NewScreenArea: TRect);
    procedure ClientPaintLocally(Sender:TObject; OldRect: PRect = nil); overload;// will need different event management
    procedure ClientPaintLocally(Sender:TObject); overload;

    procedure PaintOverlays(Sender: TObject);
    procedure ParentMouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
    procedure ParentMouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer);
    procedure ParentMouseUp(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
    procedure ParentMouseWheel(Sender: TObject; Shift: TShiftState;
                  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ParentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ParentKeyPress(Sender:TObject; var Key: Char);
    procedure ParentKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ParentResize(Sender:TObject);
end;



type TVisualOverlayPanelEx = Class(TVisualOverlayPanel)
    Constructor Create(ClientRect: TRect; Parent: TObject = nil; Screen:TBitmap = nil);
    Destructor Destroy(); override;
  private
    VOHandler: TVisualOverlayHandler;

  protected
    procedure SetLeft(int : integer);  override;
    procedure SetTop(int: integer);    override;
    procedure SetWidth(int: integer);  override;
    procedure SetHeight(int: integer); override;


    procedure SetVisibility(IsVisible: Boolean);
  public
    Property Top    : integer read GetTop    write SetTop;  // reintroduce
    property Left   : integer read getLeft   write SetLeft; // reintroduce
    property Width  : integer read GetWidth  write SetWidth;
    property Height : integer read Getheight write SetHeight;


    procedure InsertComponent(vo:TVisualOverlayPanel);
    procedure RemoveComponent(vo:TVisualOverlayPanel);

    procedure Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single); override;

    procedure SetBounds(bounds:TRect; parentarea: TRect); override;
    procedure UpdateParentAreaChanges(clientrect: TRect); override;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject;
                  Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer); override;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
end;



implementation
// global variables in the namespace of this unit
var
  UID_NEW : longint;
  CSEC    : _RTL_CRITICAL_SECTION;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Class TVisualOverlay



Constructor TVisualOverlay.Create();
begin
  inherited;
  try
    EnterCriticalSection(CSEC);
    self._UID := UID_NEW;
    inc(UID_NEW); // prepare next UID for uniqueness at runtime
    LeaveCriticalSection(CSEC);
  except
    self._UID := -GetTickCount;
  end;
  
  self.ParentRepaint      := nil;
  self.ParentLocalRepaint := nil;
  self._visible := false;

  self._HandlesWheelEvents := false;
  self._HandlesMouseEvents := false;
  self._HandlesKeyEvents   := false;
end;



procedure TVisualOverlay.SetVisibility(IsVisible: Boolean);
begin
  if not assigned(self) then exit;

  if IsVisible <> _visible then
  begin
    self._visible := IsVisible;

    if IsVisible = true then
    begin
      if assigned(self.ParentLocalRepaint)
        then self.ParentLocalRepaint(self)
        else if assigned(self.ParentRepaint) then self.ParentRepaint(self);
    end
    else
      if assigned(self.ParentRepaint) then self.ParentRepaint(self);
  end;
end;



procedure TVisualOverlay. Show;
begin
  self.Visible := true;
end;


procedure TVisualOverlay.Hide;
begin
  self.Visible := false;
end;



procedure TVisualOverlay.UpdateParentAreaChanges(clientrect: TRect);
begin
  // this method is mandatory, it will be called for every object
  // even if derived classes don't implement functionality
end;



function TVisualOverlay.CursorIsInside(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  // events are not processed by the superclass
  result := false;
end;




////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Class TVisualOverlaySurface


Constructor TVisualOverlaySurface.Create(ClientRect: TRect);
begin
  inherited Create;
  self.Anchors := [akLeft, akTop];
  self.OnClick := nil;
  self._color := clWhite;
  self.BorderColor := clSilver;
  self.SetBounds(Rect( ClientRect.Left,ClientRect.Top,
                       ClientRect.Left+320,ClientRect.Top+160),
                 ClientRect);

  self._HandlesWheelEvents := true;
  self._HandlesMouseEvents := true;
  self._HandlesKeyEvents   := false;
end;



procedure TVisualOverlaySurface.UpdateParentAreaChanges(clientrect: TRect);
var
  w, h: integer;
begin
  w := self._boundsrect.Right - self._boundsrect.Left;
  h := self._boundsrect.Bottom - self._boundsrect.Top;

  // anchoring case handling

  if (akRight in Anchors) and not (akLeft in Anchors) then
  begin
    _boundsrect.Right := clientrect.Right - _margin.Right;
    _boundsrect.Left := _boundsrect.Right -w;
  end;

  if not (akRight in Anchors) and (akLeft in Anchors) then
  begin
    _boundsrect.Left := clientrect.Left + _margin.Left;
    _boundsrect.Right := _boundsrect.Left +w;
  end;

  if (akRight in Anchors) and (akLeft in Anchors) then
  begin
    _boundsrect.Left := clientrect.Left + _margin.Left;
    _boundsrect.Right := clientrect.Right - _margin.Right;
  end;


  if (akBottom in Anchors) and not (akTop in Anchors) then
  begin
    _boundsrect.Bottom := clientrect.Bottom - _margin.Bottom;
    _boundsrect.Top := _boundsrect.Bottom -h;
  end;

  if not (akBottom in Anchors) and (akTop in Anchors) then
  begin
    _boundsrect.Top := clientrect.Top + _margin.Top;
    _boundsrect.Bottom := _boundsrect.Top +h;
  end;

   if (akBottom in Anchors) and (akTop in Anchors) then
  begin
    _boundsrect.Top := clientrect.Top + _margin.Top;
    _boundsrect.Bottom := clientrect.Bottom - _margin.Bottom;
  end;

// calculate new margins

  self._margin.Left   := _boundsrect.Left  - clientrect.Left;
  self._margin.Top    := _boundsrect.Top   - clientrect.Top;
  self._margin.Right  := Clientrect.Right - _boundsrect.Right;
  self._margin.Bottom := Clientrect.Bottom - _boundsrect.Bottom;
end;


procedure TVisualOverlaySurface.SetBounds(bounds:TRect; parentarea: TRect);
begin
  //self._boundsrect    := bounds;
  self._boundsrect.Top    := bounds.Top    + parentarea.Top;
  self._boundsrect.Left   := bounds.Left   + parentarea.Left;
  self._boundsrect.Bottom := bounds.Bottom + parentarea.Top;
  self._boundsrect.Right  := bounds.Right  + parentarea.Left;

  self._margin.Left   := self._boundsrect.Left  - parentarea.Left;
  self._margin.Top    := self._boundsrect.Top   - parentarea.Top;
  self._margin.Right  := parentarea.Right  - parentarea.Left - self.Width  + self._margin.Left;
  self._margin.Bottom := parentarea.Bottom - parentarea.Top  - self.Height + self._margin.top;
end;



function TVisualOverlaySurface.GetLeft   : integer; begin result := self._boundsrect.Left; end;
function TVisualOverlaySurface.GetTop    : integer; begin result := self._boundsrect.Top; end;
function TVisualOverlaySurface.GetWidth  : integer; begin result := self._boundsrect.Right  - self._boundsrect.Left; end;
function TVisualOverlaySurface.GetHeight : integer; begin result := self._boundsrect.bottom - self._boundsrect.Top; end;



procedure TVisualOverlaySurface.MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  if assigned(OnClick) then OnClick(self);
  if assigned(OnMouseDown) then OnMouseDown(Sender,Button,Shift,X,Y);
end;



procedure TVisualOverlaySurface.MouseMove(Sender: TObject;
                  Shift: TShiftState; X, Y: Integer);
begin
  if assigned(OnMouseMove) then OnMouseMove(Sender,Shift,X,Y);
end;



procedure TVisualOverlaySurface.MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  if assigned(OnMouseUp) then OnMouseUp(Sender,Button, Shift,X,Y);
end;



procedure TVisualOverlaySurface.Paint(Destination: TBitmap;
                AbsoluteOrigin: TPoint; CurrentScaling: single);
begin
  if not self._visible then exit;
  if not assigned(Destination) then exit;

  // call events if assigned
  if assigned(OnPaintEx) then OnPaintEx(Destination, AbsoluteOrigin, CurrentScaling);
  if assigned(OnPaint)   then OnPaint(self);
end;



function TVisualOverlaySurface.CursorIsInside(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  result := false;
  if Assigned(OnCursorInsideStateRequest) then
    Result := OnCursorInsideStateRequest(Button,Shift,MousePos);
end;



procedure TVisualOverlaySurface.SetLeft(int : integer);
var
  shift: integer;
begin
  shift := int - self._boundsrect.Left;
  _boundsrect.Left  := _boundsrect.Left + shift;
  _boundsrect.Right := _boundsrect.Right + shift;
  _margin.Left      := _margin.Left +shift;
  _margin.Right     := _margin.Right - shift;

  if self.Visible then
    if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlaySurface.SetTop(int: integer);
var
  shift: integer;
begin
  shift := int - self._boundsrect.Top;
  _boundsrect.Top    := _boundsrect.Top + shift;
  _boundsrect.Bottom := _boundsrect.Bottom + shift;
  _margin.Top        := _margin.Top +shift;
  _margin.Bottom     := _margin.Bottom - shift;

  if self.Visible then
    if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlaySurface.SetWidth(int: integer);
var
  CanPerformLocalRendering: boolean;
  w: integer;
begin
   CanPerformLocalRendering :=
     ((_boundsrect.Right - _boundsrect.Left) >= int) and
     (assigned(ParentLocalRepaint));

  w := _boundsrect.Right - _boundsrect.Left;
  _boundsrect.Right := _boundsrect.Left + int;
  _margin.Right := _margin.Right - (int-w);

  if self.Visible then
  if CanPerformLocalRendering then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlaySurface.SetHeight(int: integer);
var
  CanPerformLocalRendering: boolean;
  h: integer;
begin
   CanPerformLocalRendering :=
     ((_boundsrect.Bottom - _boundsrect.Top) >= int) and
     (assigned(ParentLocalRepaint));

  h := _boundsrect.Bottom - _boundsrect.Top;
  _boundsrect.Bottom := _boundsrect.Top + int;
  _margin.Bottom := _margin.Bottom - (int - h);

  if self.Visible then
  if CanPerformLocalRendering then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlaySurface.SetCaption(new: string);
begin
  _caption := new;

  if self.Visible then
   if assigned(self.ParentLocalRepaint) then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlaySurface.SetColor(new:TColor);
begin
  _color := new;
  if self.Visible then
   if assigned(self.ParentLocalRepaint) then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlaySurface.SetBorderColor(new:TColor);
begin
  _bordercolor := new;
  if self.Visible then
   if assigned(self.ParentLocalRepaint) then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlaySurface.MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if assigned(OnMouseWheel) then OnMouseWheel(Sender,Shift,WheelDelta,MousePos,Handled);
end;



Procedure TVisualOverlaySurface.MouseEnter(Sender: TObject);
begin
  // implement on demand
end;



Procedure TVisualOverlaySurface.MouseLeave(Sender: TObject);
begin
  // implement on demand
end;



Procedure TVisualOverlaySurface.DblClick(Sender: TObject);
begin
  if assigned(OnDblClick) then OnDblClick(Sender);
end;



Procedure TVisualOverlaySurface.Click(Sender: TObject);
begin
  if assigned(OnClick) then OnClick(Sender);
end;



Procedure TVisualOverlaySurface.KeyPress(Sender: TObject; var Key: Char);
begin
  if assigned(OnKeyPress) then OnKeyPress(Sender,Key);
end;



Procedure TVisualOverlaySurface.KeyDown(Sender: TObject;
    var Key: Word; Shift: TShiftState);
begin
  if assigned(OnKeyDown) then OnKeyDown(Sender,Key, Shift);
end;



Procedure TVisualOverlaySurface.KeyUp(Sender: TObject;
    var Key: Word; Shift: TShiftState);
begin
  if assigned(OnKeyUp) then OnKeyUp(Sender,Key, Shift);
end;



////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Class TVisualOverlayPanel



procedure TVisualOverlayPanel.Paint(Destination: TBitmap;
                AbsoluteOrigin: TPoint; CurrentScaling: single);
begin
  if not self._visible then exit;
  if not assigned(Destination) then exit;

  // draw
  Destination.Canvas.Brush.Style     := bsSolid;
  Destination.Canvas.Brush.Color     := _color;
  Destination.Canvas.FillRect(self._boundsrect);
  Destination.Canvas.Brush.Color     := _bordercolor;
  Destination.Canvas.FrameRect(self._boundsRect);

  // call event if assigned
  if assigned(OnPaint) then OnPaint(self);
end;



function TVisualOverlayPanel.CursorIsInside(Button: TMouseButton;
    Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  inherited CursorIsInside(Button,Shift,MousePos);

  if not assigned(OnCursorInsideStateRequest) then
    result := (MousePos.X >= _boundsrect.Left)   and
              (MousePos.X <= _boundsrect.Right)  and
              (MousePos.Y >= _boundsrect.Top)    and
              (MousePos.Y <= _boundsrect.Bottom) and
              self.Visible;
end;



////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Class TVisualOverlayProgressBar


Constructor TVisualOverlayProgressBar.Create(ClientRect: TRect);
begin
  inherited Create(ClientRect);
  Width     := 320;
  Height    :=  32;
  _min      :=   0;
  _max      := 100;
  _position :=   0;
  self.UpdateParentAreaChanges(ClientRect);
  _caption  := 'in progress ...';
end;



procedure TVisualOverlayProgressBar.Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single);
var
  pbar: TRect;
  a, i: integer; // general purpose variables for minor calculations
  p1 : TPoint;   // general purpose

  function TransformLocal(p: TPoint):TPoint; overload;
  begin
    result.X := p.X + _boundsrect.Left;
    result.Y := p.Y + _boundsrect.Top;
  end;

  function TransformLocal(r: TRect):TRect; overload;
  begin
    result.TopLeft := TransformLocal(r.TopLeft);
    result.BottomRight := TransformLocal(r.BottomRight);
  end;

  function maxval(a,b: integer):integer;
  begin
    result := a;
    if a<b then result := b;
  end;

  function minval(a,b: integer):integer;
  begin
    result := a;
    if a>b then result := b;
  end;

  function rgb(r, g, b: integer): Cardinal;
  begin
    result := maxval(0,minval(255,r))  or
              maxval(0,minval(255,g)) shl 8 or
              maxval(0,minval(255,b)) shl 16;
  end;
  const barcolor = clBlack;
begin
  inherited Paint(Destination,AbsoluteOrigin,CurrentScaling);
  if abs(_max - _min) = 0 then _max := _min + 1;
  

  // output caption
  Destination.Canvas.Font.Size  := 9;
  a := Destination.Canvas.TextHeight('pI');
  p1 := TransformLocal(Point(4,2));
  Destination.Canvas.Font.Color := clblack;
  Destination.Canvas.Brush.Style := bsClear;
  Destination.Canvas.TextOut(p1.X, p1.Y, _caption);

  // calculate progressbar area
  pbar.Left := p1.X;     // aligned with caption
  pbar.Top  := p1.Y + a + 2; // 2 pixels below caption.bottom
  pbar.Right := pbar.Left + self.width -4 - 4; // width minus some space on the right and left
  pbar.Bottom := pbar.Top + self.Height - 2 -a -2 -2; // top-font: -2, font-height: a; spacer=-2, bottom margin -2

  // draw progress bar
  Destination.Canvas.Brush.Color := clblack;
  destination.Canvas.Brush.Style := bsSolid;
  Destination.Canvas.FrameRect(pbar);

  // draw some gradient
  pbar.Right := round(pbar.Left + (pbar.Right - pbar.Left)*(_position/abs(_max-_min)));
  Destination.Canvas.Brush.Color := clsilver;
  destination.Canvas.Brush.Style := bsSolid;
  Destination.Canvas.fillRect(pbar);

//  if Destination.PixelFormat = pf24bit then
//  begin
    for i := pbar.left+1 to pbar.right-1 do // hope this works under win64 -.-
    begin
      Destination.Canvas.pen.Color := rgb(
        (pbar.Right-pbar.Left)-(i-pbar.Left)+120,
        (pbar.Right-pbar.Left)-(i-pbar.Left)+100,
        (pbar.Right-pbar.Left)-(i-pbar.Left)+60);
      Destination.Canvas.MoveTo(i,pbar.Top+1);
      Destination.Canvas.LineTo(i,pbar.Bottom-1);
    end;
//  end;

  // draw another frame rect
  Destination.Canvas.Brush.Color := clsilver;
  destination.Canvas.Brush.Style := bsSolid;
  Destination.Canvas.FrameRect(pbar);
end;



procedure TVisualOverlayProgressBar.SetMin(new: double);
begin
  _min := new;
  if _min > _max then _min := _max;

  if self.Visible then
  if assigned(self.ParentLocalRepaint) then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlayProgressBar.SetMax(new: double);
begin
  _max := new;
  if _max < _min then _max := _min;

  if self.Visible then
  if assigned(self.ParentLocalRepaint) then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlayProgressBar.SetPosition(new: double);
begin
  _position := new;
  if _position < _min then _position := min;
  if _position > _max then _position := max;

  if self.Visible then
  if assigned(self.ParentLocalRepaint) then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Constructor TBufferedVisualOverlay.Create(ClientRect: TRect);
begin
  inherited;
  buffer := TBitmap.Create;
  buffer.PixelFormat := pf24bit;
  buffer.Canvas.Pen.Color := self.Color;
  buffer.Canvas.Brush.Color := self.Color;
  buffer.Width := self.Width;
  buffer.Height := self.Height;
  self._translucent := false;
  alpha := $80;
  UseBackgroundBuffering := false;
  bgbuffer := nil;
end;



Destructor TBufferedVisualOverlay.Destroy();
begin
  buffer.Destroy;
  if assigned(bgbuffer) then bgbuffer.Destroy;
  inherited;
end;



procedure TBufferedVisualOverlay.SetWidth(int: integer);
begin
  buffer.Width := int;
  inherited SetWidth(int);
end;



procedure TBufferedVisualOverlay.SetHeight(int: integer);
begin
  buffer.Height := int;
  inherited SetHeight(int);
end;



procedure TBufferedVisualOverlay.Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single);
begin
  // call event if assigned. This event should fill the ScreenBuffer bitmap
  // which will be drawn now.
  if assigned(OnPaint) then OnPaint(self);

  if not self._translucent then
  begin
    Windows.BitBlt(
      Destination.Canvas.Handle,
      self.Left,
      self.Top,
      buffer.Width,
      buffer.Height,
      buffer.canvas.handle,
      0,
      0,
      SRCCOPY);
  end
  else
  begin
    MMXBlend.BlendRect(
      Destination,Destination,buffer,
      Rect(Left,Top,Left + Width,Top + Height -1 ),
      alpha,
      true);

    Destination.Canvas.Brush.Color := clBlack;
    Destination.Canvas.FrameRect(self.BoundsRect);
  end;
end;



procedure TBufferedVisualOverlay.UpdateParentAreaChanges(clientrect: TRect);
begin
  inherited UpdateParentAreaChanges(ClientRect);
  if (self.width >= 0) then
    buffer.Width := self.Width;
  if (self.Height >=0) then
    buffer.Height := self.Height;
end;



procedure TBufferedVisualOverlay.MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Sender, Button, Shift, X, Y);
  if ssCtrl in Shift then
  begin
    // toggle transparency mode
    self.Translucent := not self.Translucent;
  end;
end;



function TBufferedVisualOverlay.CursorIsInside(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean;

begin
  result := inherited CursorIsInside(Button, Shift, MousePos);
  result := result and not (self.Translucent and not(ssCtrl in Shift));
end;



procedure TBufferedVisualOverlay.SetTranslucency(new: Boolean);
var
  change: boolean;
begin
  change := _translucent xor new;
  _translucent := new;
  if change then
  begin
    if not translucent then
    begin
      if assigned(self.ParentLocalRepaint)
        then
        begin
          ParentLocalRepaint(self)
        end
      else
        begin
          if assigned(self.ParentRepaint)
            then ParentRepaint(self);
        end
    end
    else
      if assigned(self.ParentRepaint)
            then ParentRepaint(self);
  end;
end;







////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//  TVisualOverlayButton

procedure TVisualOverlayButton.Paint(Destination: TBitmap;
                    AbsoluteOrigin: TPoint; CurrentScaling: single);
var
  cw, ch: integer;
begin
   Inherited;
   cw := (self.Width  - Destination.Canvas.TextWidth (caption)) div 2;
   ch := (self.Height - Destination.Canvas.TextHeight(caption)) div 2;
   Destination.Canvas.Brush.Style := bsClear;
   Destination.Canvas.TextOut(self.Left+cw, self.Top + ch, Caption);
   Destination.Canvas.Brush.Style := bsSolid;
end;



Constructor TVisualOverlayButton.Create(ClientRect: TRect);
begin
  Inherited;
  self._boundsrect := rect(
      ClientRect.Left,
      ClientRect.Top,
      ClientRect.Left+ 96,
      ClientRect.Top + 32);

  self._margin.Left   := _boundsrect.Left  - clientrect.Left;
  self._margin.Top    := _boundsrect.Top   - clientrect.Top;
  self._margin.Right  := Clientrect.Right  - _boundsrect.Right;
  self._margin.Bottom := Clientrect.Bottom - _boundsrect.Bottom;

  self._HandlesKeyEvents := true;
end;



procedure TVisualOverlayButton.MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  self._BorderColor := clblack;
  self.Color        := $C0C0C0;
end;



procedure TVisualOverlayButton.MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  self._Color  := clwhite;
  BorderColor := clsilver;
  if assigned(OnClick) then OnClick(self);
end;






////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Class TVisualOverlayHandler


// this stuff is just copy&paste, no claims regarding functionality accepted ;-)
//   ps. made it work (2011-04-01)
// this will need a lot of fixing once the handler become of any practical importance
//  pps. after several sessions the handler seems to work (2011-05-21)


constructor TVisualOverlayHandler.Create(Target:TBitmap);
begin
      inherited Create;

      EventLinkedVisualUID := -1;
      ParentVisible := true;
      self.Buffer := Target;
      // if assigned(Target) then
      //   self._ReferenceRect := rect(0,0,target.Width,target.Height);
end;



destructor TVisualOverlayHandler.Destroy;
    var
      i: integer;
begin
      for i := 0 to length(VisualOverlays)-1 do
        self.VisualOverlays[i].Destroy;
      setlength(VisualOverlays,0);

      inherited;
end;



procedure TVisualOverlayHandler.InsertVisualOverlayObject(newobj: TVisualOverlay);
const OBJECT_NOT_FOUND = nil;
begin
  if self.GetVisualOverlayObjectByID(newobj.UID) = OBJECT_NOT_FOUND then
  begin
    if (newobj.Owner is TVisualOverlayHandler) then
       (newobj.Owner as TVisualOverlayHandler).RemoveVisualOverlayObject(newobj);

    if (newobj is TVisualOverlayPanel) then
       (newobj as TVisualOverlayPanel).UpdateParentAreaChanges(self.ReferenceRect);

    newobj.Owner := self; // pwned
    setlength(self.VisualOverlays, length( self.VisualOverlays ) + 1);
    newobj.ParentRepaint      := self.ClientPaint;
    newobj.ParentLocalRepaint := self.ClientPaintLocally;
    self.VisualOverlays[length( self.VisualOverlays )-1] := newobj;
  end;
end;



function TVisualOverlayHandler.RemoveVisualOverlayObject(obj: TVisualOverlay): TVisualOverlay;
var
  i: integer;
begin
  result := nil;
  for i := 0 to length(self.VisualOverlays)-1 do
    if self.VisualOverlays[i] = obj then
    begin
      result := obj;
      self.VisualOverlays[i] := self.VisualOverlays[length(self.VisualOverlays)-1];
      setlength( self.VisualOverlays, length(self.VisualOverlays)-1);
      obj.Owner := nil;
      obj.ParentRepaint := nil;
      obj.OnPaint := nil;
      break;
    end;
end;



function TVisualOverlayHandler.RemoveVisualOverlayObject(UID: longint) : TVisualOverlay;
var
  i: integer;
begin
  result := nil;
  for i := 0 to length(self.VisualOverlays)-1 do
    if self.VisualOverlays[i].UID = UID then
    begin
      result := VisualOverlays[i];
      self.VisualOverlays[i] := self.VisualOverlays[length(self.VisualOverlays)-1];
      setlength( self.VisualOverlays, length(self.VisualOverlays)-1);
      result.Owner := nil;
      break;
    end;
end;



function TVisualOverlayHandler.GetVisualOverlayObjectByID(UID: longint): TVisualOverlay;
var
  i: integer;
begin
  result := nil;
  for i := 0 to length(self.VisualOverlays)-1 do
    if self.VisualOverlays[i].UID = UID then
    begin
      result := self.VisualOverlays[i];
      break;
    end;
end;



function TVisualOverlayHandler.GetVisualOverlayUIDByPos(pos: TPoint;Shift: TShiftState;
  Button:TMouseButton): longint;
var i: longint;
begin
  result := -1;
  for i := 0 to length(self.VisualOverlays)-1 do
    if VisualOverlays[i].CursorIsInside(Button,Shift,pos) then
    begin
      result := VisualOverlays[i].UID;
      break;
    end;
end;



procedure TVisualOverlayHandler.UpdateVisualOverlays(NewScreenArea: TRect);
var
  i : integer;
begin
  for i := 0 to length(self.VisualOverlays)-1 do
    self.VisualOverlays[i].UpdateParentAreaChanges(_ReferenceRect);
end;



procedure TVisualOverlayHandler.PaintOverlays(Sender: TObject);
var
  i : integer;
begin
      for i := 0 to Length(self.VisualOverlays)-1 do
        if self.VisualOverlays[i].Visible then
          self.VisualOverlays[i].Paint(buffer,self.Origin,self._zoom);
end;



procedure TVisualOverlayHandler.ClientPaintLocally(Sender:TObject);
begin
  ClientPaintLocally(Sender,nil);
end;


procedure TVisualOverlayHandler.ClientPaintLocally(Sender:TObject; OldRect: PRect = nil);
var
  vo : TVisualOverlayPanel absolute Sender;
  i: integer;
  box: TRect;

  function EnclosureRect(r1, r2: TRect): TRect;
  begin
    with Result do
    begin
      left   := min (r1.Left  , r2.Left  );
      right  := max (r1.Right , r2.Right );
      top    := min (r1.Top   , r2.Top   );
      bottom := max (r1.Bottom, r2.Bottom);
    end;
  end;

begin
  if not assigned(parent) then exit;
  if not assigned(buffer) then exit;
  if not assigned(sender) then exit;

  if assigned(BeforeClientPaintLocally) then
    BeforeClientPaintLocally(self);

  if (Sender is TVisualOverlayPanel) then
  begin

    // handler is the master handler of a form
    //
    if parent is TForm then
    begin

      // have background erased
      if assigned(self.BeforeClientPaint) then Self.BeforeClientPaint(self);

      // simple mode - redraw all to avoid difficult overlapping/transparency checks
      //
      for i := 0 to Length(self.VisualOverlays)-1 do
      begin
        if self.VisualOverlays[i].Visible then
           self.VisualOverlays[i].Paint(buffer,
                                        self.Origin,
                                        self._zoom);
        // if (self.VisualOverlays[i] is TVisualOverlayPanel) then
      end;

      // add a more sophisticated, segmented redraw instead of this "blind" approach later
      // maybe add some list of rects to
      // if assigned(AfterClientPaint) then AfterClientPaint(self);
      // or just hand over the local redraw authority to the sub-vo (no geometry info transfer)


      if assigned(OldRect) then
        box := EnclosureRect(OldRect^, vo.BoundsRect)
      else
        CopyRect(box, vo.BoundsRect);

      BitBlt(
        (parent as TForm).Canvas.Handle,
        box.Left,
        box.Top,
        box.Right - box.Left,
        box.Bottom - box.Top,
        buffer.Canvas.Handle,
        box.Left,
        box.Top,
        SRCCOPY);


      if assigned(AfterClientPaintLocally) then
        AfterClientPaintLocally(self);
    end; // if

    // handler is embedded, forward command
    //
    if parent is TVisualOverlay then
    with (parent as TVisualOverlay) do
    begin
         if assigned(ParentLocalRepaint) and not assigned(OldRect) // check whether glitches within OldRect would be missed during local repaint without additional information
           then ParentLocalRepaint(sender)                         //
           else if assigned(ParentRepaint) then ParentRepaint(sender);
    end; // with

  end; // if

end;



procedure TVisualOverlayHandler.ParentMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
//  ForwardToOverlay: Boolean;
  i: integer;

begin
//  ForwardToOverlay := false;
  self.EventLinkedVisualUID := -1;
  DownEventShiftState := Shift; // shift state is lost once MouseUp occurs!

  for i := length(self.VisualOverlays)-1 downto 0 do // z-order as order of creation
  begin
    if VisualOverlays[i].CursorIsInside(Button,Shift,Point(X,Y)) then
    begin
      EventLinkedVisualUID := VisualOverlays[i].UID;
      //ForwardToOverlay := true;
      VisualOverlays[i].MouseDown(Sender,Button,Shift,X,Y);
      break;
    end;
  end;
end;



procedure TVisualOverlayHandler.ParentMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
var
    vo: TVisualOverlay;
//    p: TPoint;
const OBJECT_NOT_FOUND = nil;
begin
    if Shift = [] then EventLinkedVisualUID := -1; // focus is lost

    if EventLinkedVisualUID <> -1 then
    begin
      vo := self.GetVisualOverlayObjectByID(EventLinkedVisualUID);
      if vo = OBJECT_NOT_FOUND then exit;
      vo.MouseMove(Sender, Shift,X,Y);
    end;
end;



procedure TVisualOverlayHandler.ParentMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    vo: TVisualOverlay;
begin
  if assigned(self) then
    if EventLinkedVisualUID <> -1 then
    begin
      vo := self.GetVisualOverlayObjectByID(EventLinkedVisualUID);
      if not assigned(vo) then exit;
      vo.MouseUp(Sender,Button,Shift,X,Y);
      EventLinkedVisualUID := -1; // drop linking
    end;
end;



procedure TVisualOverlayHandler.ParentMouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  UID : longint;
  VO  : TVisualOverlay;
begin
  if parent is TForm then
    MousePos := Point(
          MousePos.X - (parent as TForm).ClientOrigin.X,
          MousePos.Y - (parent as TForm).ClientOrigin.Y)
  else if parent is TVisualOverlay then
    MousePos := Point(
          MousePos.X - (parent as TVisualOverlay).AbsolutePosition.X,
          MousePos.Y - (parent as TVisualOverlay).AbsolutePosition.Y);

  UID := self.GetVisualOverlayUIDByPos(MousePos,Shift);
  VO  := self.GetVisualOverlayObjectByID(UID);
  if assigned(VO) then
    if not (VO.HandlesWheelEvents) then UID := -1;

  if (UID <> -1) then
  begin
      VO.MouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
  end;
end;



procedure TVisualOverlayHandler.ParentKeyPress(Sender:TObject;var Key: Char);
var
  voID: longint;
  vo: TVisualOverlay;
begin
  voID := GetVisualOverlayUIDByPos(zoompoint,[]);
  if voID <> -1 then
  begin
    vo := self.GetVisualOverlayObjectByID(voID);
    vo.KeyPress(Sender, Key);
  end;
end;



procedure TVisualOverlayHandler.ParentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  voID: longint;
  vo: TVisualOverlay;
begin
  voID := GetVisualOverlayUIDByPos(zoompoint,[]);
  if voID <> -1 then
  begin
    vo := self.GetVisualOverlayObjectByID(voID);
    vo.KeyDown(Sender, Key, Shift);
  end;
end;



procedure TVisualOverlayHandler.ParentKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  voID: longint;
  vo: TVisualOverlay;
begin
  voID := GetVisualOverlayUIDByPos(zoompoint,[]);
  if voID <> -1 then
  begin
    vo := self.GetVisualOverlayObjectByID(voID);
    vo.KeyUp(Sender, Key, Shift);
  end;
end;



procedure TVisualOverlayHandler.ParentResize(Sender:TObject);
begin
  if assigned(Sender) then
  begin
      if Sender is TForm then // not good!
        _ReferenceRect := (Sender as TForm).ClientRect;
      if Sender is TVisualOverlayPanel then
        _ReferenceRect := (Sender as TVisualOverlayPanel).BoundsRect;

      UpdateVisualOverlays(_ReferenceRect);
  end;
end;



procedure TVisualOverlayHandler.ClientPaint(Sender: TObject);
    var
      i: integer;
begin
  // check if skip flag is set
  //
  if self._surpressrepaint then
  begin
     _surpressrepaint:= false;
     exit; // maybe we change this to : just skip the calls and execute the events?
  end;

  if not assigned(self.Buffer) then exit;
  if not self.ParentVisible then exit;



  if assigned(BeforeClientPaint) then BeforeClientPaint(self);

  // call overlay object rendering procedures
  //
  for i := 0 to Length(self.VisualOverlays)-1 do
    if self.VisualOverlays[i].Visible then
       self.VisualOverlays[i].Paint(buffer,
                                    self.Origin,
                                    self._zoom);

  if assigned(AfterClientPaint) then AfterClientPaint(self);
end;



procedure TVisualOverlayHandler.OnParentResize(Sender:TObject);
begin
  if assigned(Sender) and assigned(parent) then
  begin
    if parent is TForm then
    begin
      UpdateVisualOverlays((parent as TForm).ClientRect);
      ClientPaint(parent);
    end;
    if parent is TVisualOverlayPanel then
    begin
      UpdateVisualOverlays((parent as TVisualOverlayPanel).BoundsRect);
      ClientPaint(parent);
    end;
  end;
end;



procedure TVisualOverlayHandler.SetReferenceRect(new: TRect; Refresh: Boolean= false);
begin
  if (new.Left <> _ReferenceRect.Left) or
     (new.Top  <> _ReferenceRect.Top) or
     (new.Right <> ReferenceRect.Right) or
     (new.Bottom <> ReferenceRect.Bottom) then
  begin
    _ReferenceRect := new;

    if assigned(parent) then
    begin
      if parent is TForm then
      begin
        UpdateVisualOverlays((parent as TForm).ClientRect);
        ClientPaint(parent);
      end;
      if parent is TVisualOverlayPanel then
      begin
        UpdateVisualOverlays((parent as TVisualOverlayPanel).BoundsRect);
        if Refresh then ClientPaint(parent);
      end;
    end;
  end;
end;



procedure TVisualOverlayHandler.surpressrepaint;
begin
  if assigned(self) then
  self._surpressrepaint := true;
end;



function TVisualOverlayHandler.CanHandleMouseEvent(pos: TPoint;Shift: TShiftState;
                 Button:TMouseButton = mbLeft): Boolean;
begin
  if  self.EventLinkedVisualUID = -1 then
    result := (-1 <> self.GetVisualOverlayUIDByPos(pos,shift,button))
  else
    result := true; // mousemove and mouseup are dependent on whether mousedown
                    // has been taken responsibility for.
end;








////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Class TVisualOverlayPanelEx

// Extended panel class that comprises a handler for sub-objects



Constructor TVisualOverlayPanelEx.Create(ClientRect: TRect; Parent: TObject; Screen:TBitmap);
begin
  inherited Create(ClientRect);

  self.VOHandler := TVisualOverlayHandler.Create(screen);
  self.VOHandler.Buffer := Screen;
  self.VOHandler.SetReferenceRect(ClientRect);
  self.VOHandler.Parent := self;
end;



Destructor TVisualOverlayPanelEx.Destroy();
begin
  VOHandler.Destroy;
  inherited;
end;



procedure TVisualOverlayPanelEx.InsertComponent(vo:TVisualOverlayPanel);
begin
  self.VOHandler.InsertVisualOverlayObject(vo);
end;



procedure TVisualOverlayPanelEx.RemoveComponent(vo:TVisualOverlayPanel);
begin
  self.VOHandler.surpressrepaint;
  vo.UpdateParentAreaChanges(rect(0,0,1024,1024));
  self.VOHandler.RemoveVisualOverlayObject(vo);
end;



procedure TVisualOverlayPanelEx.SetLeft(int : integer);
var
  shift: integer;
begin
  shift := int - self._boundsrect.Left;
  _boundsrect.Left  := _boundsrect.Left  + shift;
  _boundsrect.Right := _boundsrect.Right + shift;
  _margin.Left      := _margin.Left  + shift;
  _margin.Right     := _margin.Right - shift;

  VOHandler.SetReferenceRect(BoundsRect);

  if self.Visible then
    if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlayPanelEx.SetTop(int: integer);
var
  shift: integer;
begin
  shift := int - self._boundsrect.Top;
  _boundsrect.Top    := _boundsrect.Top    + shift;
  _boundsrect.Bottom := _boundsrect.Bottom + shift;
  _margin.Top        := _margin.Top    + shift;
  _margin.Bottom     := _margin.Bottom - shift;

  VOHandler.SetReferenceRect(BoundsRect);

  if self.Visible then
    if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlayPanelEx.Paint(Destination: TBitmap;
                AbsoluteOrigin: TPoint; CurrentScaling: single);
begin
  inherited;
  if self.Visible then VOHandler.ClientPaint(self);
end;



procedure TVisualOverlayPanelEx.MouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  if VOHandler.CanHandleMouseEvent(Point(X,Y),shift,button) then
    VOHandler.ParentMouseDown(Sender,Button,Shift,X,Y)
  else
    inherited;
end;



procedure TVisualOverlayPanelEx.MouseMove(Sender: TObject;
                  Shift: TShiftState; X, Y: Integer);
begin
  if VOHandler.CanHandleMouseEvent(Point(X,Y),shift) then
    VOHandler.ParentMouseMove(Sender,Shift,X,Y)
  else
    inherited;
end;



procedure TVisualOverlayPanelEx.MouseUp  (Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
begin
  if VOHandler.CanHandleMouseEvent(Point(X,Y),shift, Button) then
    VOHandler.ParentMouseUp(Sender,Button, Shift,X,Y)
  else
    inherited;
end;



procedure TVisualOverlayPanelEx.MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if VOHandler.CanHandleMouseEvent(MousePos,shift) then
    VOHandler.ParentMouseWheel(Sender,Shift,WheelDelta,MousePos,Handled)
  else
    inherited;
end;



procedure TVisualOverlayPanelEx.SetBounds(bounds:TRect; parentarea: TRect);
begin
  if assigned(VOHandler) then VOHandler.SetReferenceRect(bounds);
  inherited;
end;



procedure TVisualOverlayPanelEx.UpdateParentAreaChanges(clientrect: TRect);
begin
  inherited;
  if assigned(VOHandler) then VOHandler.SetReferenceRect(BoundsRect);
end;



procedure TVisualOverlayPanelEx.SetVisibility(IsVisible: Boolean);
begin
  if _visible <> IsVisible then
  begin
    inherited;
    VOHandler.ParentVisible := self.Visible;
  end;
end;



procedure TVisualOverlayPanelEx.SetWidth(int: integer);
var
  CanPerformLocalRendering: boolean;
  w: integer;
begin
   CanPerformLocalRendering :=
     ((_boundsrect.Right - _boundsrect.Left) >= int) and
     (assigned(ParentLocalRepaint));

  w := _boundsrect.Right - _boundsrect.Left;
  _boundsrect.Right := _boundsrect.Left + int;
  _margin.Right := _margin.Right - (int-w);

  if assigned(VOHandler) then VOHandler.SetReferenceRect(BoundsRect);

  if self.Visible then
  if CanPerformLocalRendering then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



procedure TVisualOverlayPanelEx.SetHeight(int: integer);
var
  CanPerformLocalRendering: boolean;
  h: integer;
begin
   CanPerformLocalRendering :=
     ((_boundsrect.Bottom - _boundsrect.Top) >= int) and
     (assigned(ParentLocalRepaint));

  h := _boundsrect.Bottom - _boundsrect.Top;
  _boundsrect.Bottom := _boundsrect.Top + int;
  _margin.Bottom := _margin.Bottom - (int - h);

  if assigned(VOHandler) then VOHandler.SetReferenceRect(BoundsRect);

  if self.Visible then
  if CanPerformLocalRendering then ParentLocalRepaint(self)
    else if assigned(self.ParentRepaint) then ParentRepaint(self);
end;



////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////




initialization
  UID_NEW := 1000000;
  InitializeCriticalSection(CSEC);
finalization
  DeleteCriticalSection(CSEC);
end.