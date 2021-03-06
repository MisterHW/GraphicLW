//////////////////////////////////////////////////////////////////
//                                                              //
//  funktionale Erweiterung auf Basis von TCanvas               //
//  Polygone zeichnen (wireframe, gef�llt oder texturiert)      //
//                                                              //
//  H.W. 2004-2009                                              //
//                                                              //
//////////////////////////////////////////////////////////////////

unit draw2d;


interface

uses
  windows,
  SysUtils,Graphics,math;



    Type TPoint2d = record
      x,y: integer;
    end;
    type TPolygon = record
      a,b,c: TPoint2d;
    end;


    function  CreateBuffer(w,h: integer;filename:string = ''): TBitmap; // 32bit buffers
    procedure ClearBuffer(buffer: TBitmap);                             // what might this procedure do?
    procedure drawpoly_wireframe(buffer:TBitmap; p: TPolygon; color: TColor);
    procedure drawpoly_flatfill(buffer:TBitmap; p: TPolygon; color: TColor; renderfx: boolean = false);
    procedure drawpoly_texturemapped(var buffer:TBitmap;var texture: TBitmap;var p,t: TPolygon);

    procedure smoothbuffer(var buffer: TBitmap; const cycles: byte = 1);

    function Point2d(x,y: integer):TPoint2d;
implementation

    var
      xmn, xmx: integer;              // scanline start-u. Endp.
      ymm: Array[0..1] of integer;    // start- und Endpunkte
      tax,tay,tbx,tby: integer;       // texur punkt(a,b) (x,y) m�ssen projeziert werden
      p1,p2,t1,t2,pp1,pp2,tp1,tp2: TPoint2d;
      tv:real;
      dxt,dyt,dxg,dyg: integer;       // delta x teil und delta x ganz
      surfacewidth: integer;
      destsize: TPoint2d;




function Point2d(x,y: integer):TPoint2d;
begin
  result.x := x;
  result.y := y;
end;



procedure smoothbuffer(var buffer: TBitmap; const cycles: byte = 1);
var
  i: integer;
  x,y: integer;
  L0: PBytearray;
  L1: PBytearray;
begin
  for i := 1 to cycles do
  begin
    for y :=1 to buffer.Height-2 do
    begin
    L0 := buffer.ScanLine[y];
    L1 := buffer.ScanLine[y+1];;
      for x :=1 to buffer.width-2 do
      begin
        L0[x*4]   := (L0[x*4  +4]+L0[x*4  ]+L1[x*4  ]+L1[x*4  +4]) div 4;
        L0[x*4+1] := (L0[x*4+1+4]+L0[x*4+1]+L1[x*4+1]+L1[x*4+1+4]) div 4;
        L0[x*4+2] := (L0[x*4+2+4]+L0[x*4+2]+L1[x*4+2]+L1[x*4+2+4]) div 4;
      end;
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

function CreateBuffer(w,h: integer;filename:string = ''): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Canvas.Brush.Color := clblack;
  if filename <> '' then
  if fileexists(filename) then
    Result.LoadFromFile(filename);
  Result.PixelFormat := pf32bit;
  Result.Width := w;
  Result.Height :=h;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

procedure ClearBuffer(buffer: TBitmap);
begin
  buffer.Canvas.Brush.Color := clblack;
  buffer.Canvas.Pen.Color := clblack;
  buffer.Canvas.Rectangle(0,0,buffer.Width,buffer.Height);
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

procedure drawpoly_wireframe(buffer:TBitmap; p: TPolygon; color: TColor);
begin
  buffer.canvas.Pen.Color := color;
  buffer.Canvas.MoveTo(p.a.x,p.a.y);
  buffer.Canvas.lineTo(p.b.x,p.b.y);
  buffer.Canvas.lineTo(p.c.x,p.c.y);
  buffer.Canvas.lineTo(p.a.x,p.a.y);
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

procedure drawpoly_FillLine(start: Pointer; from_x,to_x: integer);
var
  P: PByteArray absolute start;
  i: integer;
begin
  for i :=xmn to xmx do
  begin
    P[i*4  ] :=round(50*random + 200*(i-xmn)/(xmx-xmn+0.001));
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

procedure drawpoly_flatfill(buffer:TBitmap; p: TPolygon; color: TColor; renderfx: boolean = false);
var
  i: integer;
  v: integer;
begin

  buffer.canvas.Pen.Color := color;

  ymm[0] := min(min(p.a.y,p.b.y),p.c.y);
  ymm[0] := max(ymm[0],0);
  ymm[1] := max(max(p.a.y,p.b.y),p.c.y);
  ymm[1] := min(ymm[1],480);

  for i := ymm[0] to ymm[1]-1 do
  begin
  if (i>0) and (i< buffer.height) then
  begin
  xmn := buffer.Width;
  xmx :=0;
    // anfang und Ende der Scanline ermitteln
    // AB
      if (i >= min(p.a.y,p.b.y)) and (i <= max(p.a.y,p.b.y)) and ((p.b.y-p.a.y) <>0) then
      begin
        v := round( p.a.x + ((i-p.a.y)*(p.b.x-p.a.x)) div (p.b.y-p.a.y));
        xmn := min(v,xmn);
        xmx := max(v,xmx);
      end;
    // BC
      if (i >= min(p.b.y,p.c.y)) and (i <= max(p.b.y,p.c.y)) and ((p.c.y-p.b.y) <>0) then
      begin
        v := round( p.b.x + ((i-p.b.y)*(p.c.x-p.b.x)) div (p.c.y-p.b.y));
        xmn := min(v,xmn);
        xmx := max(v,xmx);
      end;
    // CA
      if (i >= min(p.c.y,p.a.y)) and (i <= max(p.c.y,p.a.y)) and ((p.a.y-p.c.y) <>0)  then
      begin
        v := round( p.c.x + ((i-p.c.y)*(p.a.x-p.c.x)) div (p.a.y-p.c.y));
        xmn := min(v,xmn);
        xmx := max(v,xmx);
      end;
    // linie malen
    xmn := max(0,xmn);
    xmx := min(buffer.Width,xmx);

  if renderfx then
    drawpoly_FillLine(buffer.ScanLine[i],xmn,xmx)
  else
  begin
    buffer.Canvas.MoveTo(xmn,i);
    buffer.Canvas.LineTo(xmx,i);
  end;

  end;
  end;

end;

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

var
  drawpoly_SrcScanline0: longint;
  drawpoly_SrcLinedelta: longint;

procedure drawpoly_TextureLine32(dststart:Pointer;src: TBitmap);
var
  P:Cardinal absolute dststart;
var
  T:Cardinal;
  i: integer;
  x,y: integer;
  addr: Cardinal;
  linedelta: Cardinal;
begin
  T := drawpoly_SrcScanline0;
  linedelta := drawpoly_SrcLinedelta;

  if xmn >= 0 then P := P + Cardinal(xmn) * 4;

  if xmn < destsize.x then
  if xmn <> xmx then
  //for i := xmn to min(xmx,destsize.x-1) do
  for i := max(0,xmn) to min(xmx,destsize.x-1) do
  begin
    if xmn <> xmx then
    begin
      x := tax + ((tbx-tax)*(i-xmn)) div (xmx-xmn);
      y := tay + ((tby-tay)*(i-xmn)) div (xmx-xmn);

     addr := T+x*4+y*linedelta;
     //if (i >= 0) then
     asm
       mov EDX, addr;
       mov EAX, [edx];
       mov EDX, P;
       mov [EDX], EAX;
     end;

     inc(P,4);
    end;
  end;

end;




procedure drawpoly_TextureLine24(dststart:Pointer;src: TBitmap);
var
  P:Cardinal absolute dststart;
var
  T:Cardinal;
  i: integer;
  x,y: integer;
  addr: Cardinal;
  linedelta: longint;
  //lld: longint;
  line_start, line_end : integer;
  signx, signy: integer;
  x0, y0, idiv_x, idiv_y, idiv_xdelta, idiv_ydelta: integer;
begin
  T := drawpoly_SrcScanline0;
  linedelta := drawpoly_SrcLinedelta;

  //P := P + max(0,xmn) * 3;
  if xmn >= 0 then P := P + Cardinal(xmn) * 3;

  line_start := max(0,xmn);
  line_end   := min(xmx,destsize.x-2);


  if xmn < xmx then
  begin
    x0 := tax + ((tbx-tax)*(line_start-xmn)) div (xmx-xmn);
    y0 := tay + ((tby-tay)*(line_start-xmn)) div (xmx-xmn);
    idiv_x := 0;
    idiv_y := 0;
    idiv_xdelta :=  round(((tbx-tax) * $10000) / (xmx-xmn));
    idiv_ydelta :=  round(((tby-tay) * $10000) / (xmx-xmn));
    signx := sign(idiv_xdelta);
    signy := sign(idiv_ydelta);
    if idiv_xdelta < 0 then idiv_xdelta := -idiv_xdelta;
    if idiv_ydelta < 0 then idiv_ydelta := -idiv_ydelta;

   for i := line_start to line_end do
   begin
       //x := tax + ((tbx-tax)*(i-xmn)) div (xmx-xmn);
       //y := tay + ((tby-tay)*(i-xmn)) div (xmx-xmn);
       x := x0  + signx * (idiv_x shr 16);
       y := y0  + signy * (idiv_y shr 16);

       if y >=0 then
         addr := Cardinal(T+x*3+y*linedelta);
         
       asm
          mov EDX, addr;
          mov EAX, [edx];
          mov EDX, P;
          mov [EDX], EAX;
       end;
       inc(P,3);

       idiv_x := idiv_x + idiv_xdelta;
       idiv_y := idiv_y + idiv_ydelta;
    end;
  end;

end;

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

procedure drawpoly_texturemapped(var buffer:TBitmap;var texture: TBitmap;var p,t: TPolygon);
const
  side_ab=0;
  side_bc=1;
  side_ca=2;

var
  i: integer;
  v: integer;
  l,r:byte;
  addr: Cardinal;
  linedelta: Cardinal;
  pf: Cardinal;

  function equate_changed(var dst:integer; src:integer):boolean;
  begin
    Result := dst<> src;
    dst := src;
  end;

  function equatep2d(var a,b: TPoint2d):boolean;
  begin
    result := (a.x = b.x) and (a.y = b.y);
  end;

begin
  drawpoly_SrcScanline0 := Cardinal(texture.ScanLine[0]);
  drawpoly_SrcLinedelta := Cardinal(Longint(texture.ScanLine[1])-drawpoly_SrcScanline0);

  if equatep2d(p.a,p.b) or equatep2d(p.b,p.c) or equatep2d(p.c,p.a) then exit; // wenn nur eine gerade aufgespannt wird Abbruch

  if (buffer.PixelFormat = pf24bit) and (texture.PixelFormat = pf24bit) then
    pf := 24
  else if (buffer.PixelFormat = pf32bit) and (texture.PixelFormat = pf32bit) then
    pf := 32
  else
    exit;

  surfacewidth := buffer.Width-1;

  ymm[0] := min(min(p.a.y,p.b.y),p.c.y);
  ymm[0] := max(ymm[0],0);
  ymm[1] := max(max(max(p.a.y,p.b.y),p.c.y),0);
  ymm[1] := min(ymm[1],buffer.Height-1);

  addr := Cardinal(buffer.scanline[0]);
  linedelta := Cardinal(buffer.scanline[1]) - addr;

  for i := max(0,ymm[0]) to min(ymm[1],buffer.height-1) do
  begin
    xmn := high(integer); //buffer.Width-1;
    xmx := low(integer); //0;

    // anfang und Ende der Scanline ermitteln
    // AB
      if (i >= min(p.a.y,p.b.y)) and (i <= max(p.a.y,p.b.y)) and ((p.b.y <> p.a.y)) then        // wenn y im  der y-berich der seite liegt und die seite nicht horiz. ist
      begin
        v := p.a.x + (((i-p.a.y)*(p.b.x-p.a.x)) div (p.b.y-p.a.y));                             // x-wert f�r y an der Seite interpolieren
        if equate_changed(xmn , min(v,xmn)) then l := side_ab;                                  // wenn der wert auf der seite kleiner ist als der alte minimalwert, dann diese seite nehmen
        if equate_changed(xmx , max(v,xmx)) then r := side_ab;
      end;
    // BC
      if (i >= min(p.b.y,p.c.y)) and (i <= max(p.b.y,p.c.y)) and ((p.c.y <> p.b.y)) then
      begin
        v := p.b.x + (((i-p.b.y)*(p.c.x-p.b.x)) div (p.c.y-p.b.y));
        if equate_changed(xmn , min(v,xmn)) then l := side_bc;
        if equate_changed(xmx , max(v,xmx)) then r := side_bc;
      end;
    // CA
      if (i >= min(p.c.y,p.a.y)) and (i <= max(p.c.y,p.a.y)) and ((p.a.y <> p.c.y))  then
      begin
        v := p.c.x + (((i-p.c.y)*(p.a.x-p.c.x)) div (p.a.y-p.c.y));
        if equate_changed(xmn , min(v,xmn)) then l := side_ca;
        if equate_changed(xmx , max(v,xmx)) then r := side_ca;
      end;

    // projektion
        pp1.x:=xmn; pp1.y:=i;                       // Anfgs.Punkt auf zielseite des Bilds
        pp2.x:=xmx; pp2.y:=i;                       // EndPunkt auf zielseite des Bilds

    case l of
    side_ab:
      begin
        p1 := p.a; p2 :=P.b;                        // Zielseite
        t1 := t.a; t2 :=t.b;                        // quellseite
      end;
    side_bc:
      begin
        p1 := p.b; p2 :=P.c;                        // Zielseite
        t1 := t.b; t2 :=t.c;                        // quellseite
      end;
    side_ca:
      begin
        p1 := p.c; p2 :=p.a;                        // Zielseite
        t1 := t.c; t2 :=t.a;                        // quellseite
      end;
    end;

    dxt:= pp1.x-p1.x;   // l�nge des bildseitenabschnitts
    dyt:= pp1.y-p1.y;
    dxg:= p2.x-p1.x;   // l�nge der Bildseite gesamt
    dyg:= p2.y-p1.y;
    tv := sqrt((sqr(dxt)+sqr(dyt))/(sqr(dxg)+sqr(dyg))); // teilverh�ltnis der tats�chlichen l�ngen ( optimiert mit einem sqrt)

                                                        // den Punkt auf der Seite des TExtur-polygons interpolieren
      tax := t1.x +round((t2.x-t1.x)*tv);               //+ ((t2.x-t1.x)*(pp1.x-p1.x)) div (p2.x - p1.x) //tdx*pdx1/pdx       // seitenl�ngeX ader Texturseite * (geteilte Seite des Bilds x)/(l�nge der bildseite X)
      tay := t1.y +round((t2.y-t1.y)*tv);               //((t2.y-t1.y)*(pp1.y-p1.y)) div (p2.y - p1.y);//tdy*pdy1/pdy

    case r of
    side_ab:
      begin
        p1 := p.a; p2 :=P.b;                        // Zielseite
        t1 := t.a; t2 :=t.b;                        // quellseite
      end;
    side_bc:
      begin
        p1 := p.b; p2 :=P.c;                        // Zielseite
        t1 := t.b; t2 :=t.c;                        // quellseite
      end;
    side_ca:
      begin
        p1 := p.c; p2 :=P.a;                        // Zielseite
        t1 := t.c; t2 :=t.a;                        // quellseite
      end;
    end;

    dxt:= pp2.x-p1.x;   // l�nge des bildseitenabschnitts
    dyt:= pp2.y-p1.y;
    dxg:= p2.x-p1.x;   // l�nge der Bildseite gesamt
    dyg:= p2.y-p1.y;

    tv := sqrt((sqr(dxt)+sqr(dyt))/(sqr(dxg)+sqr(dyg))); // teilverh�ltnis der tats�chlichen l�ngen ( optimiert mit einem sqrt)
    
      tbx := t1.x +round((t2.x-t1.x)*tv);               //+ ((t2.x-t1.x)*(pp1.x-p1.x)) div (p2.x - p1.x) //tdx*pdx1/pdx       // seitenl�ngeX ader Texturseite * (geteilte Seite des Bilds x)/(l�nge der bildseite X)
      tby := t1.y +round((t2.y-t1.y)*tv);               //((t2.y-t1.y)*(pp1.y-p1.y)) div (p2.y - p1.y);//tdy*pdy1/pdy

    // linie malen
      destsize.x := buffer.Width; destsize.y := buffer.Height;
      if pf = 32 then
        drawpoly_TextureLine32(Pointer(addr+linedelta*i),texture)
      else
        drawpoly_TextureLine24(Pointer(addr+linedelta*i),texture);
  end;
end;


/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////

end.
