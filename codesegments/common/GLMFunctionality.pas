unit GLMFunctionality;

interface

uses
  Windows, Graphics, Types,Classes, math, fmath, Forms, Sysutils,
  BilinearRegistration, draw2d, DynamicFunctions, MathUtils;


const
  RADC_RESOLUTION_COARSE = 0; // radial distortion correction modes
  RADC_RESOLUTION_NORMAL = 1;
  RADC_RESOLUTION_FINE   = 2;
  RADC_RESOLUTION_EXACT  = 3;
  RADC_RESOLUTION_FINAL  = 4;

// single-type Rect types

type TFltPoint = packed record
  X: Single;
  Y: Single;
end;


type TFltRect = packed record
  case Integer of
    0: (Left, Top, Right, Bottom: Single);
    1: (TopLeft, BottomRight: TFltPoint);
end;


type TRectSectionsMap = packed record
  Rects : Array[0..2] of Array[0..2] of TRect;
  Fills : Array[0..2] of Array[0..2] of Byte;
end;

// double-type Rect types

type TFloatPoint = record
  X , Y : Double;
end;

type TFloatRect = packed record
  case Integer of
    0: (Left, Top, Right, Bottom: Double);
    1: (TopLeft, BottomRight: TFloatPoint);
end;



type TUpdateProgressCallback =
  procedure(Position: integer; Maximum: integer) of object;



// calculations
function  Arg(vx, vy: Single):Single;
function  PreciseArg(vx, vy: Double):Double;
function  AngleSum(var p: Array of TPoint): single;
procedure SortToConvexPolygon(var points: Array of TPoint);
procedure TransformToCenterOfMassCoords(var points: Array of TPoint);
function  FindMaximumSquareTileSize(NumberOfTiles: Cardinal; width, height: Cardinal): Cardinal;
function  CalculateThumbnailArea(target: TRect; Margin: integer; srcwidth, srcheight: Cardinal):TRect;
function  IsPointInRect(var r: TRect;x, y: integer): Boolean; // standard test for rects with left >= right, botom >= top with full border test
function  IsPointInRectEx(var rect : TRect; pt: TPoint) : Boolean; overload;// leave out bottom and right border, allow inverted rects
function  IsPointInRectEx(var rect : TFloatRect; pt: TFloatPoint) : Boolean; overload;


// image algorithms
procedure TransformImage(sourcebuffer, outputbuffer:TBitmap;
            var Points: Array of TPoint; var templatepoints: Array of TPoint;
            imode: TInterpolationMethod);
procedure PolToCartesianTrans(sourcebuffer, outputbuffer:TBitmap;
            var Points: Array of TPoint; PreviewRender: boolean= false);
procedure BinPixels(var src, dst: TBitmap;bin_x, bin_y: integer; multiplier: single);
procedure BrightnessAndContrast(src, dst : TBitmap; contrast: single; brightness: single); overload;
procedure BrightnessAndContrast(src : TBitmap; area: TRect; contrast: single; brightness: single); overload;
function AnalyzeGradientStrength(src, mask: TBitmap; BlockSize: integer): TMemoryStream;
procedure RadialDistortionCorrectionFilter(
  dst, src : TBitmap;
  DistortionFunction, InverseDF: TVar1SingleObjProc; DFObj, IDFObj: TObject;
  ResolutionMode: Cardinal; rotation: single; target_org: TPoint; scaling: single = 1;
  CB: TUpdateProgressCallback = nil); overload;
procedure DrawChessboardOverlay(buf: TBitmap; rct: TRect);


// glm core functions
function constrain(min, max, value: longint): longint; overload;
function constrain(min, max, value: Single): Single; overload;
function IntersectionRect(inner, outer: TRect): TRect; overload;
function IntersectionRect(inner, outer: TFltRect): TFltRect; overload; // single-type

function RectToFltRect(r: TRect): TFltRect; // single-type
function FltRectToRect(r: TFltRect): TRect; // single-type
function FloatPoint(Point: TPoint): TFloatPoint; overload; // double-type
function FloatPoint(X, Y: Double): TFloatPoint; overload;  // double-type
function Point(Point: TFloatPoint): TPoint; overload;      // double-type
function Point(X,Y: Double): TPoint; overload;             // double-type

function FrameRect(r1, r2: TRect): TRect;
procedure GenerateSectionsMap(var map: TRectSectionsMap; A, B: TRect);

function EmptyIntersection (Intersection: TRect): Boolean;
procedure StretchCopyRect(src, dest: TBitmap; srect, destrect: TRect); overload;
procedure StretchCopyRect(src, dest: TBitmap; srect, destrect: TFltRect); overload;

// other routines
function DeleteChars(str: string; chars: string):string;
function ReplaceKeyWord(str: string; keyword: string; replacement: string):string;
function FixDecSep(floatstr: string): string;
function GetTimeStamp: string;
function QuotientstringToFloat(s: string): Extended;
procedure SplitAndInsert(itemstr: string; separator: char; list: TStrings);



implementation

var
  DecimalSep : Char;


function FloatPoint(X, Y: Double): TFloatPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function FloatPoint(Point: TPoint): TFloatPoint;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

function Point(Point: TFloatPoint): TPoint;
begin
  Result.X := Floor(Point.X);
  Result.Y := Floor(Point.Y);
end;

function Point(X,Y: Double): TPoint; 
begin
  Result.X := Floor(X);
  Result.Y := Floor(Y);
end;

function IsPointInRectEx( var rect : TRect; pt: TPoint) : Boolean; overload; // leave out bottom and right border, allow inverted rects
begin
    result := ( pt.X >= min(rect.Left, rect.Right) ) and
              ( pt.X <  max(rect.Right, rect.Left) ) and
              ( pt.Y >= min(rect.Top, rect.Bottom) ) and
              ( pt.Y <  max(rect.Bottom, rect.Top) );
end;

function IsPointInRectEx( var rect : TFloatRect; pt: TFloatPoint) : Boolean; overload; // leave out bottom and right border, allow inverted rects
begin
    result := ( pt.X >= min(rect.Left, rect.Right) ) and
              ( pt.X <  max(rect.Right, rect.Left) ) and
              ( pt.Y >= min(rect.Top, rect.Bottom) ) and
              ( pt.Y <  max(rect.Bottom, rect.Top) );
end;


// generates the field information for delta drawing of filled
// polygons. Example:
//
//  _______                  __|____|___         00:  A  01: A    02:  #
// |A  ____|___            _|00|_01_|02_|_       10:  A  11: A&B  12:  B
// |  |B       |            |  |    |   |        20:  #  21: B    22:  B
// |  |        |     -->    |10| 11 |12 |
// |__|        |           _|__|____|___|_       where A : erase rendering / restore background
//    |        |            |20| 21 |22 |        where B : render pattern
//    |________|            |__|____|___|        where A&B or # : do nothing
//                             |    |
//
// encoding in FILLS[x,y] =
// $0: #  -  $1: A  -  $2: B  -  $3: A&B
//
procedure GenerateSectionsMap(var map: TRectSectionsMap; A, B: TRect);
var
  xlist, ylist: array [0..3] of single;
  x,y: integer;
begin
  xlist[0] := A.Left; xlist[1] := A.Right;
  xlist[2] := B.Left; xlist[3] := B.Right;

  ylist[0] := A.Top;  ylist[1] := A.Bottom;
  ylist[2] := B.Top;  ylist[3] := B.Bottom;

  SortSingleData(@xlist[0], 4 * SizeOf(Single));
  SortSingleData(@ylist[0], 4 * SizeOf(Single));

  for y := 0 to 2 do
  for x := 0 to 2 do
  begin
    map.Rects[x,y].TopLeft     := Point(xlist[x  ], ylist[y  ]);
    map.Rects[x,y].BottomRight := Point(xlist[x+1], ylist[y+1]);

    map.Fills[x,y]   := $00;

    if IsPointInRectEx(A, Point(trunc(xlist[x]+xlist[x+1]) div 2, trunc(ylist[y]+ylist[y+1]) div 2)) then
      map.Fills[x,y] := $01;

    if IsPointInRectEx(B, Point(trunc(xlist[x]+xlist[x+1]) div 2, trunc(ylist[y]+ylist[y+1]) div 2)) then
      map.Fills[x,y] := map.Fills[x,y] or $02;
  end;
end;


  
function FixDecSep(floatstr: string): string; // 1,234 <--> 1.234 as needed
var
  othersep: char;
begin
  result := floatstr;
  if DecimalSep = ','
    then othersep := '.'
    else othersep := ',';
  if Pos(floatstr,DecimalSep) < 1 then
    result := ReplaceKeyWord(floatstr,othersep,DecimalSep);
end;



function GetDecimalSep(default: Char): Char;
var
  returnsize : integer;
  buf: PChar;
begin
  returnsize := 1;
  buf := StrAlloc(1);
  if 0 <> Windows.GetLocaleInfo(LOCALE_SYSTEM_DEFAULT,LOCALE_SDECIMAL,buf, returnsize) then
    result := default
  else
    result := buf[0];
end;



function DeleteChars(str: string; chars: string):string;
var
 i, k: cardinal;
 charlen: cardinal;
 found: Boolean;
 cursor : Cardinal;
begin
  setlength(result,length(str));
  cursor := 0;
  charlen := length(chars);
  for i := 1 to length(str) do
  begin
    found := false;
    for k := 1 to charlen do
    begin
      if str[i] = chars[k] then // check against all chars to be excluded
      begin
        found := true;
        break;
      end;
    end;
      if not found then // current char valid, copy
      begin
        inc(cursor);
        result[cursor] := str[i];
      end;
  end;
  setlength(result, cursor);
end;



function ReplaceKeyWord(str: string; keyword: string; replacement: string):string;
var i,keyidx: integer;
var dropbuf,outp: string;
begin
  result := '';
  outp := '';
  keyidx := 1;

  for i := 1 to length(str) do
  begin

    if str[i] = keyword[keyidx] then
    begin
      if keyidx = length(keyword) then
        begin
           keyidx := 1;
           dropbuf := '';
           outp := outp + replacement;
        end
      else
        begin
          dropbuf := dropbuf + str[i];
          inc(keyidx);
        end;
    end
    else
    begin
      outp := outp + dropbuf + str[i];
      dropbuf := '';
      keyidx := 1;
    end;

  end;

  result := outp;
end;



function  FindMaximumSquareTileSize(NumberOfTiles: Cardinal; width, height: Cardinal): Cardinal;
begin
  result := min(width,height);
  while (width div result)*(height div result) < NumberOfTiles do dec(result);
end;



function CalculateThumbnailArea(target: TRect; Margin: integer; srcwidth, srcheight: Cardinal):TRect;
var
  c1, c2: single;
  w, h: integer;
begin
    result.BottomRight := Point(srcWidth, srcHeight);
    result.TopLeft := Point(0,0);
    w := target.Right - target.Left;
    h := target.Bottom - target.Top;
    c1 := result.right/ (w-2*Margin);
    c2 := result.Bottom / (h-2*Margin);
    if (c1 > 1) or (c2 > 1) then
    begin
      c1 := max(c1, c2);
      result.right := floor(result.right / c1);
      result.bottom := floor(result.bottom / c1);
    end;
    result.Left := target.Left  + (w - result.right ) div 2;
    result.Top := target.Top   + (h - result.bottom ) div 2;
    result.Bottom := result.Bottom + result.Top;
    result.Right := result.Right + result.Left;
end;



function constrain(min, max, value: longint): longint; overload;
begin
   result := value;
   if value < min then result := min;
   if value > max then result := max;
end;



function constrain(min, max, value: Single): Single; overload;
begin
   result := value;
   if value < min then result := min;
   if value > max then result := max;
end;



function IntersectionRect(inner, outer: TRect): TRect; overload;
begin;
  result.Left   := Constrain(outer.left,outer.Right, inner.Left);
  result.right  := Constrain(outer.left,outer.Right, inner.right);
  result.Top    := Constrain(outer.Top,outer.Bottom, inner.Top);
  result.Bottom := Constrain(outer.Top,outer.Bottom, inner.Bottom);
end;



function IntersectionRect(inner, outer: TFltRect): TFltRect; overload;
begin;
  result.Left   := Constrain(outer.left,outer.Right, inner.Left);
  result.right  := Constrain(outer.left,outer.Right, inner.right);
  result.Top    := Constrain(outer.Top,outer.Bottom, inner.Top);
  result.Bottom := Constrain(outer.Top,outer.Bottom, inner.Bottom);
end;



function RectToFltRect(r: TRect): TFltRect;
begin
  with result do
  begin
     Top    := r.Top;
     Left   := r.Left;
     Bottom := r.Bottom;
     Right  := r.Right;
  end;
end;



function FltRectToRect(r: TFltRect): TRect;
begin
  with result do
  begin
     Top    := trunc(r.Top);
     Left   := trunc(r.Left);
     Bottom := trunc(r.Bottom);
     Right  := trunc(r.Right);
  end;
end;



function FrameRect(r1, r2: TRect): TRect;
begin
  result.Left   := min(r1.Left, r2.Left);
  result.Top    := min(r1.Top, r2.Top);
  result.Bottom := max(r1.Bottom,r2.Bottom);
  result.Right  := max(r1.Right, r2.Right);
end;




function EmptyIntersection (Intersection: TRect): Boolean;
begin
  result := ((Intersection.Left = Intersection.Right)
          or (Intersection.Top  = Intersection.Bottom));
end;



    procedure StretchCopyRect(src, dest: TBitmap; srect, destrect: TFltRect); overload;
    var
      xratio, yratio, xq, yq: Single;
      org: TFltPoint;
      x,xxx, y,yyy: integer;
      p, q : PByteArray;
      idy, iddelta: Cardinal; // integer-dividable y
      _xxx, _idy: Cardinal;
      isrect, idestrect: TRect;
    begin
      if EmptyIntersection(FltRectToRect(srect)) then exit;

      xratio := (destrect.Right - destrect.Left)/(srect.Right - srect.Left);
      yratio := (destrect.Bottom - destrect.Top)/(srect.Bottom - srect.Top);
      org.X  := srect.Left; // srect.topleft maps to dest(0,0)
      org.Y  := srect.Top;

      isrect    := FltRectToRect(srect);
      idestrect := FltRectToRect(destrect);


      if (xratio > 1 ) or (yratio > 1) or  // extended nonius correction
         (GetStretchBltMode(dest.Canvas.Handle) = STRETCH_HALFTONE) then // do this if halftone stretch blitting is desired.
      begin
      // round source rectangle to full pixels, min/max checks are done by StretchBlt, add if custom drawing is used
      isrect.Left   := floor(srect.Left);
      isrect.Top    := floor(srect.Top);
      isrect.Right  := ceil(srect.Right+0.00001);
      isrect.Bottom := ceil(srect.Bottom+0.00001);

      // recalculate the dest coordinates (usually outside dest borders!)
      idestrect.Left   := floor( destrect.Left  + (isrect.Left   - org.X)*xratio );
      idestrect.Top    := floor( destrect.Top   + (isrect.Top    - org.Y)*yratio );
      idestrect.Right  := floor( destrect.Left  + (isrect.Right  - org.X)*xratio);
      idestrect.Bottom := floor( destrect.Top   + (isrect.Bottom - org.Y)*yratio);

      // draw
      StretchBlt(dest.Canvas.Handle,
                              idestrect.Left,
                              idestrect.Top,
                              idestrect.Right - idestrect.Left,
                              idestrect.Bottom - idestrect.Top,
                  src.Canvas.Handle,
                              isrect.Left,
                              isrect.Top,
                              isrect.Right - isrect.Left,
                              isrect.Bottom - isrect.Top,
                  SRCCOPY);
                  
      end
      else
        begin
          idestrect := IntersectionRect(idestrect,Rect(0,0,dest.Width,dest.Height));
          isrect    := IntersectionRect(isrect,   Rect(0,0,src.Width,src.Height));

          if ((idestrect.Right -1 - idestrect.Left) = 0) or
             ((idestrect.Bottom-1 - idestrect.Top) = 0) then exit;

          xq :=    (isrect.Right  -1 - isrect.Left)/(idestrect.Right  -1 - idestrect.Left);
          yq :=    (isrect.Bottom -1 - isrect.Top) /(idestrect.Bottom -1 - idestrect.Top);

          _xxx := idestrect.Left * 3;
          _idy := isrect.Left  * $10000;
          iddelta := trunc (xq * $10000);



          for y := idestrect.Top to idestrect.Bottom-1 do
          begin
            P := dest.ScanLine[y];
            Q := src.ScanLine[min( src.Height-1 ,
                                   trunc((y - destrect.Top) * yq + srect.top) )];

            xxx := _xxx; // reset loop variables
            idy := _idy;
            for x := idestrect.Left to idestrect.Right-1 do
            begin
              // update source address
              yyy := (idy shr 16)*3; // downscaling and rounding in one step, nice and fast

              // transfer
              P[xxx  ] := Q[yyy  ];
              P[xxx+1] := Q[yyy+1];
              P[xxx+2] := Q[yyy+2];

              // incement addresses
              xxx := xxx + 3;
              idy := idy + iddelta;
            end;
          end;

        end;

    end;



    procedure StretchCopyRect(src, dest: TBitmap; srect, destrect: TRect); overload; // pf24bit only
    var
      xratio, yratio, xq, yq: double;
      x,xxx, y,yyy: integer;
      p, q : PByteArray;
      idy, iddelta: Cardinal; // integer-dividable y
      _xxx, _idy: Cardinal;
    begin
      if EmptyIntersection(srect) then exit;

      xratio :=   (destrect.Right - destrect.Left)/(srect.Right - srect.Left);
      yratio :=   (destrect.Bottom - destrect.Top)/(srect.Bottom - srect.Top);

      if (xratio > 1 ) or (yratio > 1) or  // extended nonius correction
         (GetStretchBltMode(dest.Canvas.Handle) = STRETCH_HALFTONE) then // do this if halftone stretch blitting is desired.
      begin

       StretchBlt(dest.Canvas.Handle,destrect.Left,
                              destrect.Top,
                              destrect.Right - destrect.Left,
                              destrect.Bottom - destrect.Top,
                  src.Canvas.Handle, srect.Left,
                              srect.Top,
                              srect.Right - srect.Left,
                              srect.Bottom - srect.Top,
                  SRCCOPY);
      end
      else     // StretchDraw is a wrapper for StretchBlt which doesn't
               // work correctly when downscaling so nearest neighbour is used instead.
        begin
          destrect := intersectionRect(destrect,Rect(0,0,dest.Width,dest.Height));
          srect := intersectionRect(srect,Rect(0,0,src.Width,src.Height));

          if ((destrect.Right -1 - destrect.Left) = 0) or
             ((destrect.Bottom-1 - destrect.Top) = 0) then exit;

          xq :=    (srect.Right -1 - srect.Left)/(destrect.Right-1 - destrect.Left);
          yq :=    (srect.Bottom-1 - srect.Top)/(destrect.Bottom-1 - destrect.Top);


          _xxx := destrect.Left * 3;
          _idy := (srect.Left) shl 16;
          iddelta := round (xq* $10000);

          for y := destrect.Top to destrect.Bottom-1 do
          begin
            P := dest.ScanLine[y];
            Q := src.ScanLine[floor((y - destrect.Top)*yq + srect.top)];

            xxx := _xxx; // reset loop variables
            idy := _idy;
            for x := destrect.Left to destrect.Right-1 do
            begin
              // update source address
              yyy := (idy shr 16)*3; // downscaling and rounding in one step, nice and fast

              // transfer
              P[xxx  ] := Q[yyy];
              P[xxx+1] := Q[yyy+1];
              P[xxx+2] := Q[yyy+2];

              // incement addresses
              xxx := xxx+3;
              idy := idy + iddelta;
            end;
          end;

        end;

    end;


var
  v: single;
  //asin, acos,
  hyp,prec_v: double;
  //arg_twopi : DWORD;


function PreciseArg(vx, vy: Double):Double;
begin
   result := 0;
   prec_v := vx*vx + vy*vy;
   if (prec_v = 0) then exit;
   hyp := sqrt(prec_v);
   result := arccos(vx/hyp);
   if vy < 0 then result := 2*pi - result;
end;

function Arg(vx, vy: Single):Single;
begin
   v := vx*vx + vy*vy;
   if (vy > 0)
     then result := arccos_s6(vx/sqrt(v))
     else
       if (vy < 0)
         then result := 2*pi-arccos_s6(vx/sqrt(v))   // 2pi-periodische Funktionen interessieren
         else result := 0;                           //     die 2pi nicht -> ggf. sparen!
end;


{function Arg(vx, vy: Single):Single;
begin
   result := 0;
   v := vx*vx + vy*vy;
   if (v = 0) then exit;
   hyp := sqrt(v);
   result := arccos_s6(vx/hyp);
   asm
     mov EAX, [vy]
     mov ECX, [ESP]
     and EAX, $80000000
     xor ECX, EAX         // change sign of result if vy negative
     shr EAX, 31          // get sign bit of 32bit IEEE float (MSB)
     mov EDX, $40C90FDB   // Hex(Single(2 * pi))* (0|1)
     mul EAX, EDX
     mov [arg_twopi], EAX
     mov [ESP], ECX
     fld [arg_twopi]
     fld [ESP]
     fadd
     fstp [ESP]
   end;
end;}



function anglesum(var p: Array of TPoint): single;
var i,n: integer;
var a1, a2: single;
begin
  result := 0;
  n := length(p);
  for i := 0 to n -1 do
  begin
    a1 := Arg( (p[(i+0) mod n].x - p[(i+1) mod n].x) ,
               (p[(i+0) mod n].y - p[(i+1) mod n].y) );
    a2 := Arg( (p[(i+2) mod n].x - p[(i+1) mod n].x) ,
               (p[(i+2) mod n].y - p[(i+1) mod n].y) );

    if (a1-a2) > 0 then result := result + (a1-a2)
                   else result := result +2*pi + (a1-a2);
  end;
end;




procedure SortToConvexPolygon(var points: Array of TPoint);
  var
    mx, my: single; // center of mass
    i,j,k,n : integer;
    angles: Array of Single;
    sbuf: single;
    pbuf: TPoint;
begin;
    n := length(Points);

    if n = 0 then exit;

    // Center of Mass
    mx := 0;
    my := 0;
    for i := 0 to n -1 do
    begin
      mx := mx + points[i].X;
      my := my + points[i].Y;
    end;
    mx := mx / n;
    my := my / n;

    // find angles
    setlength(angles, length(points));
    for i := 0 to n -1 do
    begin
      angles[i] := Arg( points[i].X-mx , -(points[i].Y-my) );
    end;

    // sort by angles
    for j := 0 to n-2 do
    begin
      for k := j+1 to n-1 do
      begin
        if angles[j] < angles[k] then
        begin
          sbuf      := angles[j];
          angles[j] := angles[k];
          angles[k] := sbuf;
          pbuf      := points[j];
          points[j] := points[k];
          points[k] := pbuf;
         //  if j = focusindex then focusindex := k;
        end;
      end;
    end;

    // clean up
    setlength(angles,0);

end;




procedure TransformToCenterOfMassCoords(var points: Array of TPoint);
var
  cm: TPoint;
var n,i: integer;
begin
   n := length(points);
   if n = 0 then exit;

   cm := Point(0,0);
   for i := 0 to n-1 do
   begin
     cm.X := cm.X + Points[i].X;
     cm.Y := cm.Y + Points[i].Y;
   end;

   cm.X := round(cm.X/n);
   cm.Y := round(cm.Y/n);

   for i := 0 to n-1 do
   begin
     Points[i].X := Points[i].X - cm.X;
     Points[i].Y := Points[i].Y - cm.Y;
   end;
end;





procedure TransformImage(sourcebuffer, outputbuffer:TBitmap;
            var Points: Array of TPoint; var templatepoints: Array of TPoint;
            imode: TInterpolationMethod);
var
   t : T4Templates;
   alphax,alphay : T4Vector;
   // templates : array of TTemplate;
   i : integer;
   // outbounds:TRect;
begin
  // main processing

   for i := 1 to 4 do
   begin
     t[i].p1.x := Points[i-1].x;
     t[i].p2.x := TemplatePoints[i-1].x;
     t[i].p1.y := Points[i-1].y;
     t[i].p2.y := TemplatePoints[i-1].y;
   end;

   bilinearRegistration.G1D2interpolation(t,alphax,alphay);
   bilinearRegistration.Registration(sourcebuffer, outputbuffer,alphax,alphay,rmNoChange, imode);
end;




procedure PolToCartesianTrans(sourcebuffer, outputbuffer:TBitmap;
            var Points: Array of TPoint; PreviewRender: boolean= false);

var
 x,y : integer;
 x3: integer;
 sx, sy: integer;
 phi, phi0: double;
 Q: PByteArray;
//  t: longint;

 var
  x1, x2: integer;
  y1, y2, y1a: integer;
  P1, P2: PByteArray;
  c: cardinal;
  fx, fy, ifx, ify: single;
  r,g,b: byte;
  px, py: single;

 function Argument(vx, vy: double):double;
 var r, //asin,
  acos: double;
 begin
  result := 0;
   r := sqrt(vx*vx+vy*vy);
   vx := vx / r;
   // vy := vy / r;
   // asin := arcsin(vy);
   acos := arccos_s6(vx);
   if ( vx >= 0 ) and ( vy >= 0 ) then result :=acos;
   if ( vx < 0 ) and ( vy >= 0 ) then result :=acos;
   if ( vx < 0 ) and ( vy < 0 ) then result :=2 * pi - acos;
   if ( vx >= 0 ) and ( vy < 0 ) then result :=2 * pi - acos;
 end;




begin

// t := gettickcount;
  y1a := -1; P1 := nil; p2 := nil;

  phi0 := Argument((Points[1].X-Points[0].X),-(Points[1].Y-Points[0].Y)); // aus Points[0..1] berechnet -.-
  if PreviewRender then
  for y :=  outputbuffer.Height-1 downto 0 do
  begin
  q := outputbuffer.ScanLine[y];
    for x := 0 to outputbuffer.Width-1 do
    begin
      phi := (2 * pi * x) / outputbuffer.Width - phi0 ;
      sx := Points[0].X + round((outputbuffer.Height-1-y) * cos(phi));
      sy := Points[0].Y + round((outputbuffer.Height-1-y) * sin(phi));
      c := sourcebuffer.Canvas.Pixels[sx,sy];
      x3 := x*3;
      Q[x3]   := c shr 16 and $FF;
      Q[x3+1] := c shr  8 and $FF;
      Q[x3+2] := c        and $FF;
    end;
  end;

  if not PreviewRender then

    for y :=  outputbuffer.Height-1 downto 0 do
    begin

    q := outputbuffer.ScanLine[y];

    for x := 0 to outputbuffer.Width-1 do
    begin
    phi := (2 * pi * x) / outputbuffer.Width - phi0 ;

 // procedure InterPixel(px, py: single); inline;
 px := Points[0].X + ((outputbuffer.Height-1-y) * cos(phi)) ;
 py := Points[0].Y + ((outputbuffer.Height-1-y) * sin(phi)) ;
 begin
   x1 := floor(px)*3 ; x2 := x1 +3;
   y1 := floor(py); y2:= y1 + 1;

   if (x1 > (sourcebuffer.Width-1)*3)or (y2 > sourcebuffer.Height) or
      (x1 < 0) or (y1 < 0) then
   begin
     r := 0; g := 0; b := 0;
   end
   else
   begin

     y2 := min(y2,sourcebuffer.Height-1);

    if (y1a <> y1) then
     begin
      P1 := sourcebuffer.ScanLine[y1];
      P2 := sourcebuffer.ScanLine[y2];
    end;
    y1a := y1;

    fx := px - floor(px); ifx := 1-fx;
    fy := py - floor(py); ify := 1-fy;

    r := floor( (( P1[x1  ]*(ifx)+P1[x2  ]*(fx) )) * (ify) + (( P2[x1  ]*(ifx)+P2[x2  ]*(fx) )) * (fy));
    g := floor( (( P1[x1+1]*(ifx)+P1[x2+1]*(fx) )) * (ify) + (( P2[x1+1]*(ifx)+P2[x2+1]*(fx) )) * (fy));
    b := floor( (( P1[x1+2]*(ifx)+P1[x2+2]*(fx) )) * (ify) + (( P2[x1+2]*(ifx)+P2[x2+2]*(fx) )) * (fy));
  end;
 end;


      x3 := x*3;
      Q[x3]   := r;
      Q[x3+1] := g;
      Q[x3+2] := b;
    end;
  end;

// application.MessageBox(pchar(inttostr(gettickcount-t)),nil);

end;





procedure BinPixels(var src, dst: TBitmap;bin_x, bin_y: integer; multiplier: single);
var
  buf: Array of Cardinal;
  P: PByteArray;
  var x,xxx,bbb, y: integer;
  var tmp, f0: cardinal;
begin
  if not assigned(src) or not assigned(dst) then exit;
  setlength(buf,(src.Width div bin_x)*3*SizeOf(Cardinal));
  Windows.ZeroMemory(@buf[0],ceil(src.Width / bin_x)*3*SizeOf(Cardinal));

  f0 := round((1 + (bin_x*bin_y-1)*multiplier)/(bin_x*bin_y) *4096); // post-divide int product 2^12

  for y := 0 to src.Height-1 do
  begin
    P := src.ScanLine[y];
    bbb := 0;
    for x := 0 to src.Width -1 do
    begin
       xxx := x * 3;
       buf[bbb  ] := buf[bbb  ] + P[xxx  ];
       buf[bbb+1] := buf[bbb+1] + P[xxx+1];
       buf[bbb+2] := buf[bbb+2] + P[xxx+2];
       if ((x mod bin_x) = bin_x-1) then bbb := bbb + 3;
    end;

    if ((y mod bin_y) = bin_y -1 ) or (y = src.Height-1)  then
    begin
      if (y = src.Height -1) then
      begin
        if (src.Height mod bin_y) <> 0 then
          f0 := round((1 + (bin_x*(src.Height mod bin_y)-1)*multiplier)/(bin_x*(src.Height mod bin_y)) *4096);
      end;

      P := dst.ScanLine[y div bin_y];

      for x := 0 to ((src.Width) div bin_x)-1  do
      begin
        tmp := (buf[3*x  ] * f0) shr 12;
        if tmp > 255 then tmp := 255;
        P[3*x  ] := tmp;

        tmp := (buf[3*x+1] * f0) shr 12;
        if tmp > 255 then tmp := 255;
        P[3*x+1] := tmp;

        tmp := (buf[3*x+2] * f0) shr 12;
        if tmp > 255 then tmp := 255;
        P[3*x+2] := tmp;
      end;

      if (src.Width) mod bin_x <> 0 then
      begin
        x := (src.Width) div bin_x;

        tmp := buf[3*x  ] * round(f0 * bin_x/ ((src.Width) mod bin_x)) shr 12;
        if tmp > 255 then tmp := 255;
        P[3*x  ] := tmp;

        tmp := buf[3*x+1] * round(f0 * bin_x/ ((src.Width) mod bin_x)) shr 12;
        if tmp > 255 then tmp := 255;
        P[3*x+1] := tmp;

        tmp := buf[3*x+2] * round(f0 * bin_x/ ((src.Width) mod bin_x)) shr 12;
        if tmp > 255 then tmp := 255;
        P[3*x+2] := tmp;
      end;

      Windows.ZeroMemory(@buf[0],ceil(src.Width / bin_x)*3*SizeOf(Cardinal));
    end; //}
  end;

  setlength(buf,0);
end;



procedure BrightnessAndContrast(src : TBitmap; area: TRect; contrast: single; brightness: single); overload;
var
  P: PByteArray;
  x, y: integer;
  var tmp: integer;
begin
  for y := area.top to area.Bottom-1 do
  begin
    P:= src.ScanLine[Y];
    for x := area.Left*3 to area.Right*3-1 do
    begin
      tmp := round((P[x]-128+ brightness)*contrast+128);
      if tmp > 255 then tmp := 255 else if tmp < 0 then tmp := 0;
      P[x] := tmp;
    end;
  end;
end;



procedure BrightnessAndContrast(src, dst : TBitmap; contrast: single; brightness: single);
var
  P, Q: PByteArray;
  x, y, w, h: integer;
  var tmp: integer;
begin
  w := min(src.Width,dst.Width);
  h := min(src.Height,dst.Height);
  for y := 0 to h-1 do
  begin
    P:= src.ScanLine[Y];
    Q:= dst.ScanLine[Y];

    for x := 0 to w*3-1 do
    begin
      tmp := round((P[x]-128+ brightness)*contrast+128);
      if tmp > 255 then tmp := 255 else if tmp < 0 then tmp := 0;
      Q[x] := tmp;
    end;
  end;
end;



function AnalyzeGradientStrength(src, mask: TBitmap; BlockSize: integer): TMemoryStream;
var
  x, y,u,v: longint;
  io: single;
  P,M: PByteArray;

  subblocksize: integer;
  sx, sy: single;  // Steigungen
  nx, ny: longint; // Normalisierungsfaktor f�r Steigungen
  av: Array[0..1]of Array[0..1] of integer;
  cnt: Array[0..1]of Array[0..1] of integer; // wenn man hier Cardinal verwendet und single := cardinal1 - cardinal2 macht, wobei cardinal2 > cardinal1, wird noch in Ganzzahlarithmetik gerechnet und single = overflow

  MaskShowsAll: Boolean;
begin
  result := TMemoryStream.Create;
  subblocksize := ceil (BlockSize/2);
  io := 0; result.Write(io,4);

  MaskShowsAll := (mask = nil);


  for y := 0 to (src.Height-1) div BlockSize -1 do
  begin
    for x := 0 to (src.Width-1) div BlockSize -1 do
    begin

    //////////////////////
    //   _______
    //  | a | b |   Summen �ber Pixel in a, b, c, d mit Pixelz�hler
    //  |---|---|   a: 00, b: 01, c: 10, d: 11
    //  | c | d |
    //  '---^---'
    av[0,0]  := 0; av[0,1]  := 0;
    cnt[0,0] := 0; cnt[0,1] := 0;

    av[1,0]  := 0; av[1,1]  := 0;
    cnt[1,0] := 0; cnt[1,1] := 0;

   if not MaskShowsAll then
   begin

    for v := BlockSize*y to BlockSize*y + subblocksize-1 do
    begin
      P := src.ScanLine[v]; M := mask.ScanLine[v];
      for u := BlockSize*x*3 to (BlockSize*x + subblocksize)*3-1 do
      begin
        if M[u] > 127 then
        begin
          cnt[0,0] := cnt[0,0] + 1;
          av[0,0]  := av[0,0]  + P[u];
        end;
      end;
      for u := (BlockSize*x + blocksize - subblocksize)*3 to (BlockSize*x + blocksize)*3 -1 do
      begin
        if M[u] > 127 then
        begin
          cnt[0,1] := cnt[0,1] + 1;
          av[0,1]  := av[0,1]  + P[u];
        end;
      end;
    end;

    for v := BlockSize*y + blocksize - subblocksize to BlockSize*y + blocksize -1  do
    begin
      P := src.ScanLine[v]; M := mask.ScanLine[v];
      for u := BlockSize*x*3 to (BlockSize*x + subblocksize)*3-1 do
      begin
        if M[u] > 127 then
        begin
          cnt[1,0] := cnt[1,0] + 1;
          av[1,0]  := av[1,0]  + P[u];
        end;
      end;
      for u := (BlockSize*x + blocksize - subblocksize)*3 to (BlockSize*x + blocksize)*3 -1 do
      begin
        if M[u] > 127 then
        begin
          cnt[1,1] := cnt[1,1] + 1;
          av[1,1]  := av[1,1]  + P[u];
        end;
      end;
    end;

   end
   else
   begin

    cnt[0,0] := subblocksize * subblocksize * 3;
    cnt[0,1] := cnt[0,0];
    cnt[1,0] := cnt[0,0];
    cnt[1,1] := cnt[0,0];

    for v := BlockSize*y to BlockSize*y + subblocksize-1 do
    begin
      P := src.ScanLine[v];
      for u := BlockSize*x*3 to (BlockSize*x + subblocksize)*3-1 do
          av[0,0]  := av[0,0]  + P[u];
      for u := (BlockSize*x + blocksize - subblocksize)*3 to (BlockSize*x + blocksize)*3 -1 do
          av[0,1]  := av[0,1]  + P[u];
    end;

    for v := BlockSize*y + blocksize - subblocksize to BlockSize*y + blocksize -1  do
    begin
      P := src.ScanLine[v];
      for u := BlockSize*x*3 to (BlockSize*x + subblocksize)*3-1 do
          av[1,0]  := av[1,0]  + P[u];
      for u := (BlockSize*x + blocksize - subblocksize)*3 to (BlockSize*x + blocksize)*3 -1 do
          av[1,1]  := av[1,1]  + P[u];
    end;

   end;


    if cnt[0,0] <> 0 then av[0,0] := av[0,0] div cnt[0,0];
    if cnt[0,1] <> 0 then av[0,1] := av[0,1] div cnt[0,1];
    if cnt[1,0] <> 0 then av[1,0] := av[1,0] div cnt[1,0];
    if cnt[1,1] <> 0 then av[1,1] := av[1,1] div cnt[1,1];


    sx := 0; nx := 0;
    sy := 0; ny := 0;

    if (cnt[1,0] <> 0) and (cnt[0,0] <> 0 ) then
    begin
      sx := sx + 1.0*(av[1,0] - av[0,0]);
      nx := nx + 1;
    end;

    if (cnt[1,1] <> 0) and (cnt[0,1] <> 0 ) then
    begin
      sx := sx + 1.0*(av[1,1] - av[0,1]);
      nx := nx + 1;
    end;

    if (cnt[0,1] <> 0) and (cnt[0,0] <> 0 ) then
    begin
      sy := sy + 1.0*(av[0,1] - av[0,0]);
      ny := ny + 1;
    end;

    if (cnt[1,1] <> 0) and (cnt[0,1] <> 0 ) then
    begin
      sy := sy + 1.0*(av[1,1] - av[0,1]);
      ny := ny + 1;
    end;

    if (cnt[1,1] <> 0) and (cnt[0,0] <> 0 ) then
    begin
      sx := sx + 0.5* (av[1,1] - av[0,0]);
      sy := sy + 0.5* (av[1,1] - av[0,0]);
      nx := nx + 1; ny := ny + 1;
    end;

    if (cnt[0,1] <> 0) and (cnt[1,0] <> 0 ) then
    begin
      sx := sx - 0.5* (av[0,1] - av[1,0]);
      sy := sy + 0.5* (av[0,1] - av[1,0]);
      nx := nx + 1; ny := ny + 1;
    end;

    if nx <> 0 then
      sx := sx / (nx*subblocksize);
    if ny <> 0 then
      sy := sy / (ny*subblocksize);


    //////////////////////
    if (nx <> 0) or (ny <> 0) then
      begin
        io := sqrt(sx*sx + sy*sy);
        // MessageBox(0,PChar(inttostr(av[0,0]) +','+inttostr(av[0,1]) +','+inttostr(av[1,0]) +','+inttostr(av[1,1]) +' : '+floattostr(sx)),'',mb_OK);
        result.Write(io,4);
      end;
    end;
  end;


end;




///////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure RadialDistortionCorrectionFilter(
  dst, src : TBitmap;
  DistortionFunction, InverseDF: TVar1SingleObjProc; DFObj, IDFObj: TObject;
  ResolutionMode: Cardinal; rotation: single; target_org: TPoint; scaling: single = 1;
  CB: TUpdateProgressCallback = nil);
var
  textheight, textwidth: integer;
  gx, gy: integer;
  gridconstx, gridconsty: single;
  gridpointcnt: TPoint;
  s, d: TPolygon;

  px, py: single; // temp coord variables
  cx, cy: single; // center coords
  smallside : single;
  rezip_smallside: single;

  CurrentTransF:TVar1SingleObjProc;
  CurrentTransObj: TObject;

  drawrect: TRect;
  x, xxx, sxxx, y,y_, sw, sh: integer;
  P, Q, Q2: PBytearray;
  QBase, QLineDelta: Cardinal; // may be negative so high(Cardinal)-value may occur. Don't change to larger datatype!
  rezip_scaling: single;
  ipx, ipy: integer; // integer value of px, py

  psource, ptarget: ^Cardinal;
  time: longint;

  m11, m12,
  m21, m22 : single; // rotation matrix

  r,rezip_r,r0,scalef, phi, acos: single;
  px_: single;

  procedure radialtransform; // (var px : single; var py: single);
  begin
     px := px - cx;  // shift coordinates
     py := py - cy;

     r := sqrt(px*px + py*py);
     r0 := r;
     if r0 = 0 then r0 := 1;
     r := r * rezip_smallside;
     CurrentTransF(CurrentTransObj, r);
     r := r * smallside;
     scalef := r/r0;

     // lineare und Kongruenzabbildung statt trigonometrischer sin/cos-Berechnung
     px := (px)*scalef; // radiale Transformation    (kommutierend)
     py := (py)*scalef;

     px_:= m11*px + m12*py;  // azimuthale Transformation (kommutierend)
     py := m21*px + m22*py;
     px := px_;

     px := cx + px; // unshift coordinates
     py := cy + py;
  end;

  procedure SetRotationMatrix(angle:single);
  begin
    m11 := cos(angle);
    m22 := m11;
    m12 := -   sin(angle);
    m21 := -m12;
  end;

  var
    ipixel: Cardinal;
    itype: Cardinal;
    buf11, buf12, buf21, buf22: Cardinal;
    xfactor, yfactor,x_1factor, y_1factor: byte;
  const
    EXCEEDS_WIDTH  = $01;
    EXCEEDS_HEIGHT = $02;
  procedure ReadSourceInterPixel;
  begin
    if ipx >= drawrect.Right-1 then itype := EXCEEDS_WIDTH else itype := 0;
    if ipy >= drawrect.Bottom-1 then itype := itype or EXCEEDS_HEIGHT;
    case itype of
      0: begin
        xxx := ipx*3;
        xfactor := round(255*(px - ipx)) and $FF;
        x_1factor := 255-xfactor;
        yfactor := round(255*(py - ipy)) and $FF;
        y_1factor := 255-yfactor;
        ipixel := 0;
        ipixel :=   ( ((Q [xxx  ]*x_1factor) and $FF00 +
                       (Q [xxx+3]*  xfactor) and $FF00 ) shr 8 * y_1factor
                     +((Q2[xxx  ]*x_1factor) and $FF00 +
                       (Q2[xxx+3]*  xfactor) and $FF00 ) shr 8 * yfactor) shr 8 or

                    ( ((Q [xxx+1]*x_1factor) and $FF00 +
                       (Q [xxx+4]*  xfactor) and $FF00 ) shr 8 * y_1factor
                     +((Q2[xxx+1]*x_1factor) and $FF00 +
                       (Q2[xxx+4]*  xfactor) and $FF00 ) shr 8 * yfactor) and $FF00 or

                    ( ((Q [xxx+2]*x_1factor) and $FF00 +
                       (Q [xxx+5]*  xfactor) and $FF00 ) shr 8 * y_1factor
                     +((Q2[xxx+2]*x_1factor) and $FF00 +
                       (Q2[xxx+5]*  xfactor) and $FF00 ) shr 8 * yfactor) shl 8 and $FF0000;
      end;
      EXCEEDS_WIDTH : begin
        xxx := ipx*3;
        yfactor := trunc(255*(py - ipy)) and $FF;
        y_1factor := 255-yfactor;
        ipixel := 0;
        ipixel :=   ( Q [xxx  ] * y_1factor  shr 8 + Q2[xxx  ] * yfactor shr 8 )                    or
                    ( Q [xxx+1] * y_1factor  shr 8 + Q2[xxx+1] * yfactor shr 8 ) shl  8 and   $FF00 or
                    ( Q [xxx+2] * y_1factor  shr 8 + Q2[xxx+2] * yfactor shr 8 ) shl 16 and $FF0000 ;
      end;
      EXCEEDS_HEIGHT : begin
      ipixel := cllime;
        xxx := ipx*3;
        xfactor := trunc(255*(px - ipx)) and $FF;
        x_1factor := 255-xfactor;
        ipixel := 0;
        ipixel :=   ( Q [xxx  ] * x_1factor shr 8 + Q[xxx+3] * xfactor shr 8 )                    or
                    ( Q [xxx+1] * x_1factor shr 8 + Q[xxx+4] * xfactor shr 8 ) shl  8 and   $FF00 or
                    ( Q [xxx+2] * x_1factor shr 8 + Q[xxx+5] * xfactor shr 8 ) shl 16 and $FF0000 ;
      end;
      EXCEEDS_WIDTH or EXCEEDS_HEIGHT: begin
        Q := src.ScanLine[ipy];
        psource :=@Q[ipx*3];
        ipixel := psource^ and $00FFFFFF;
      end;
    end;
  end;

  function IsVisibleTriangle(var canv: TRect; var poly: TPolygon): Boolean;
  var
    bin_x, bin_y : integer; // -1 | 0 | +1 for x, vertical alike#
    bin_x2, bin_y2 : integer;
    // delta: integer;
  begin
    result := false;

    if poly.a.x <= canv.Right then bin_x := -1 else bin_x :=0;
    if poly.a.x >= canv.Left  then bin_x := bin_x + 1;
    if poly.a.y <= canv.Bottom then bin_y := -2 else bin_y :=0;
    if poly.a.y >= canv.Top    then bin_y := bin_y + 2;

    {-4 	-3 	-2  bin_x + bin_y
     -1 	 0 	 1
      2 	 3 	 4 }

    if poly.b.x <= canv.Right then bin_x2 := -1 else bin_x2 :=0;
    if poly.b.x >= canv.Left  then bin_x2 := bin_x2 + 1;
    if poly.b.y <= canv.Bottom then bin_y2 := -2 else bin_y2 :=0;
    if poly.b.y >= canv.Top    then bin_y2 := bin_y2 + 2;


    if    (bin_x *bin_y = 0) or (bin_x2 * bin_y2 = 0) // one point is in the center
       or (bin_x + bin_y = -(bin_x2 + bin_y2)) // diagonal position (connection intersects)
       then
    begin
      result := true;
      exit;
    end;
  end;

begin

  case resolutionmode of
    RADC_RESOLUTION_COARSE: gridpointcnt := Point(ceil(src.Width /128),ceil(src.Height /128));
    RADC_RESOLUTION_NORMAL: gridpointcnt := Point(ceil(src.Width / 64),ceil(src.Height / 64));
    RADC_RESOLUTION_FINE  : gridpointcnt := Point(ceil(src.Width / 32),ceil(src.Height / 32));
  else
    gridpointcnt := Point(2,2);
  end;

  gridconstx :=  (src.Width-2)/gridpointcnt.X;
  gridconsty := (src.height-2)/gridpointcnt.Y;

  smallside := min(src.Width, src.Height);
  rezip_smallside := 1 /smallside;
  cx := src.Width / 2; cy := src.Height / 2;

///////////////////////////////////////////////////////////////////////////////////////////////////////////
 if (resolutionmode <> RADC_RESOLUTION_EXACT) and (assigned(IDFObj)) then // RADC_RESOLUTION_COARSE ... FINE
///////////////////////////////////////////////////////////////////////////////////////////////////////////
 begin
   SetRotationMatrix(rotation);

   CurrentTransF  := InverseDF;
   CurrentTransObj:= IDFObj;
   if (@CurrentTransObj = nil) or (Addr(CurrentTransF) = nil) then exit;

   drawrect := rect(0,0,dst.Width, dst.Height);

   for gy := 0 to gridpointcnt.Y -1 do
   begin
     for gx := 0 to gridpointcnt.X -1 do
     begin
       s.a := Point2d(round(gx * gridconstx),round(gy * gridconsty));
       s.b := Point2d(round(gx * gridconstx),round((gy+1) * gridconsty));
       s.c := Point2d(round((gx+1) * gridconstx),round(gy * gridconsty));

       px := gx * gridconstx; py := gy * gridconsty; radialtransform;
       d.a := Point2d(round(scaling*px + target_org.x),round(scaling*py)+ target_org.y);
       px := gx * gridconstx; py := (gy+1) * gridconsty; radialtransform;
       d.b := Point2d(round(scaling*px + target_org.x),round(scaling*py)+ target_org.y);
       px := (gx+1) * gridconstx; py := gy * gridconsty; radialtransform;
       d.c := Point2d(round(scaling*px + target_org.x),round(scaling*py)+ target_org.y);

       if IsVisibleTriangle(drawrect,d)
          //IsPointInRect(drawrect,d.a.x,d.a.y) or
          //IsPointInRect(drawrect,d.b.x,d.b.y) or
          //IsPointInRect(drawrect,d.c.x,d.c.y) // or
          //IsPointInRect(drawrect,(d.a.x +d.b.x+d.c.x) div 3,(d.a.y+d.b.y+d.c.y) div 3)
          then
       draw2d.drawpoly_texturemapped(dst,src,d,s);

       s.a := Point2d(round((gx+1) * gridconstx),round((gy+1) * gridconsty));
       px := (gx+1) * gridconstx; py := (gy+1) * gridconsty; radialtransform;
       d.a := Point2d(round(scaling*px + target_org.x),round(scaling*py)+ target_org.y);

       if IsVisibleTriangle(drawrect,d)
          //IsPointInRect(drawrect,d.a.x,d.a.y) or
          //IsPointInRect(drawrect,d.b.x,d.b.y) or
          //IsPointInRect(drawrect,d.c.x,d.c.y) //or
          // IsPointInRect(drawrect,(d.a.x +d.b.x+d.c.x) div 3,(d.a.y+d.b.y+d.c.y) div 3)
          then
       draw2d.drawpoly_texturemapped(dst,src,d,s);
    end;
  end;

 end
 else // RADC_RESOLUTION_EXACT or _FINAL
///////////////////////////////////////////////////////////////////////////////////////////////////////////
 if assigned(DFObj) and (resolutionmode = RADC_RESOLUTION_EXACT) then // // RADC_RESOLUTION_EXACT
///////////////////////////////////////////////////////////////////////////////////////////////////////////
 begin
   CurrentTransF  := DistortionFunction;
   CurrentTransObj:= DFObj;

   SetRotationMatrix(-rotation);

   drawrect.TopLeft := target_org;
   drawrect.BottomRight := Point(round(target_org.X + src.Width*scaling),round( target_org.Y + src.Height*scaling));
   drawrect := IntersectionRect(drawrect,rect(0,0,dst.Width, dst.Height));

   rezip_scaling := 1/scaling;
   sw := src.Width; sh := src.Height;

   Q := src.ScanLine[0];
   Q2 := src.ScanLine[1];
   y_ := 0;
   QBase := Cardinal(@Q[0]);
   QLineDelta := Cardinal(@Q[0])-Cardinal(@Q2[0]); // invert negative linedelta :)
   for y := drawrect.Top to drawrect.Bottom-1 do
   begin
     if assigned(CB) and (y mod 10 = 0) then CB(y-max(0,drawrect.Top), drawrect.Bottom-1 - max(0,drawrect.Top));
 
     P := dst.ScanLine[y];
     ptarget := @P[drawrect.Left*3];
     for x := drawrect.Left to drawrect.Right -2 do
     begin
       // entskalierte Koordianten berechnen
       px := (x - target_org.x)*rezip_scaling;
       py := (y - target_org.Y)*rezip_scaling;

       // virtuelle dest-koordinaten radialtransformieren
       radialtransform;
       ipx := round(px-0.499999);
       ipy := round(py-0.499999);

       if (ipy >= 0) and (ipy < sh) and (ipx >= 0) and(ipx < sw) then
       begin
         //if ipx >= sw then break; // end of line reached - good criterion ?
         if ipy <> y_ then
         begin
           y_ := ipy;
           Q := Pointer(QBase - QLineDelta * Cardinal(y_));
         end;
         psource := @Q[ipx*3];
         ptarget^ := psource^;
       end; // if
       ptarget := Pointer(Cardinal(ptarget)+3);
     end; // for x
     begin // last column
         P := PByteArray(ptarget); // has offset (drawrect.right-1)*3
         x := drawrect.Right-1;
         px := (x - target_org.x)*rezip_scaling;
         py := (y - target_org.Y)*rezip_scaling;
         radialtransform;
         ipx := round(px-0.499999);
         ipy := round(py-0.499999);

         if (ipy >= 0) and (ipy < sh) and (ipx >= 0) and(ipx < sw) then
         begin
           Q := Pointer(QBase - QLineDelta * Cardinal(ipy));
           P[0] := Q[ipx*3  ];
           P[1] := Q[ipx*3+1];
           P[2] := Q[ipx*3+2];
         end; // if
     end; // last column
   end; // for y
 end // if assigned(DFObj)
 else
///////////////////////////////////////////////////////////////////////////////////////////////////////////
 if assigned(DFObj) and (resolutionmode = RADC_RESOLUTION_FINAL) then // RADC_RESOLUTION_FINAL final rendering with anti-aliasing :>
///////////////////////////////////////////////////////////////////////////////////////////////////////////
  begin
   CurrentTransF  := DistortionFunction;
   CurrentTransObj:= DFObj;

   SetRotationMatrix(-rotation);

   drawrect := rect(0,0,dst.Width, dst.Height);

   //time := GetTickCount;

   sw := src.Width; sh := src.Height;

   Q := src.ScanLine[0];
   Q2 := src.ScanLine[1];
   y_ := 0;
   QBase := Cardinal(@Q[0]);
   QLineDelta := longint(@Q[0])-longint(@Q2[0]); // invert negative linedelta :)
   
   for y := max(0,drawrect.Top) to drawrect.Bottom-1 do
   begin
     if (y mod 20 = 0) then
       if assigned(CB) then CB(y-max(0,drawrect.Top), drawrect.Bottom-1 - max(0,drawrect.Top));
     P := dst.ScanLine[y];
     ptarget := @P[drawrect.Left*3];
     for x := drawrect.Left to drawrect.Right -2 do
     begin
       px := x;
       py := y;

       // virtuelle dest-koordinaten radialtransformieren
       radialtransform;
       ipx := round(px-0.499999);  // much faster than floor() , how about trunc?
       ipy := round(py-0.499999);

       if (ipy >= 0) and (ipy < sh) and (ipx >= 0) and(ipx < sw) then
       begin
         // if px >= sw then break; // end of line reached - good criterion ?

         // scanline y_ wenn n�tig
         if ipy <> y_ then
         begin
           y_ := ipy;
           Q := Pointer(QBase - QLineDelta * Cardinal(y_));
           Q2 := Pointer(Cardinal(Addr(Q[0])) - QLineDelta);
         end;

         ReadSourceInterPixel;
         ptarget^ :=  ipixel;  // (ptarget^ and $FF000000) or ...
       end;
       ptarget := Pointer(Cardinal(ptarget)+3);
     end; // for x

       begin // last pixel in line
         x := drawrect.Right -1;
         P := PByteArray(ptarget); // has offset (drawrect.right-1)*3
         px  := x;                    py := y;
         radialtransform;
         ipx := round(px-0.499999);  ipy := round(py-0.499999);

         if (ipy >= 0) and (ipy < sh) and (ipx >= 0) and(ipx < sw) then
         begin
           if ipy <> y_ then
           begin
             y_ := ipy;
             Q := Pointer(QBase - QLineDelta * Cardinal(y_));
             Q2 := Pointer(Cardinal(Addr(Q[0])) - QLineDelta);
           end;
           ReadSourceInterPixel;
           P[0] := ipixel and $FF;
           P[1] := ipixel and $FF00 shr 8;
           P[2] := ipixel and $FF0000 shr 16;
        end;
       end;
   end; // for y

   //application.MessageBox(PChar(IntToStr(getTickCount-time)),'',mb_OK);
  end
///////////////////////////////////////////////////////////////////////////////////////////////////////////
 else // Render-mode not supported with the given function
///////////////////////////////////////////////////////////////////////////////////////////////////////////
 begin
   drawrect.TopLeft := target_org;
   drawrect.BottomRight := Point(round(target_org.X + src.Width*scaling),round( target_org.Y + src.Height*scaling));
   drawrect := IntersectionRect(drawrect,rect(0,0,dst.Width, dst.Height));

   dst.Canvas.brush.Style := bsclear;
   textwidth  := dst.Canvas.TextWidth('RENDER MODE NOT SUPPORTED');
   textHeight := dst.Canvas.TextHeight('RENDER MODE NOT SUPPORTED');
   dst.Canvas.TextOut(
        drawrect.Left + (drawrect.Right-drawrect.Left-textwidth ) div 2,
        drawrect.Top  + (drawrect.Bottom-drawrect.Top-textheight) div 2,
        'RENDER MODE NOT SUPPORTED');
 end;

end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////




procedure DrawChessboardOverlay(buf: TBitmap; rct: TRect);
var
  x, y: Integer;
  P: PByteArray;
  val: word;
  bvals : array[0..1] of byte absolute val;
begin
  for y := rct.Top to rct.Bottom -1 do
  begin
    P := buf.ScanLine[y];
    for x := rct.Left to rct.Right -1 do
    begin
      val := 0;
      bvals[0] := P[x*3];
      val := val + ((x and $8) xor (y and $8)) shl 2;
      bvals[0] := bvals[0] or (bvals[1] *$FF); // clip to $FF
      P[x*3] := bvals[0];

      val := 0;
      bvals[0] := P[x*3+1];
      val := val + ((x and $8) xor (y and $8)) shl 2;
      bvals[0] := bvals[0] or (bvals[1] *$FF); // clip to $FF
      P[x*3+1] := bvals[0];

      val := 0;
      bvals[0] := P[x*3+2];
      val := val + ((x and $8) xor (y and $8)) shl 2;
      bvals[0] := bvals[0] or (bvals[1] *$FF); // clip to $FF
      P[x*3+2] := bvals[0];
    end;
  end;
end;



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



function IsPointInRect(var r: TRect;x, y: integer): Boolean;
begin
  result := (x>= r.Left) and (x<=r.Right) and (y>=r.Top) and (y<=r.Bottom);
end;



function GetTimeStamp: string;
var
  h,m,s,ms: word;
begin
    DecodeTime(time,h,m,s,ms);
    result := Format(' [%2d:%2d:%2d-%4d] ',[h,m,s,ms]);
end;



initialization
  DecimalSep := getDecimalSep('.');
  
end.
