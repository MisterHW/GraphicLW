unit sphericaltransforms;

// todo
// - keine Optimierungen mehr  [OK]
// - final render mit bilinearer, gewarpter Interpolation [OK]
// - edit this crop in ps - feature (post-editing : unique) [OK]
// - Appliation terminate problem : kein Abbruch des laufenden Algos implementiert! [pending]

interface
uses
  forms, windows,
  Graphics, Sysutils, math,
  GLMFunctionality, fmath, PanotransformClasses,
  trigonometry;

const
  normallen : single = 43.2666153; // normal focal length for 36x24mm


procedure SphericalToSphericalTransform(
              target: TBitmap; source: TBitmap; // image buffers
              alpha_h, alpha_v,       // source FOV angle
              theta,phi,psi: single; // coordinate system transformation angles
              polarpinch: single;
              finalrender: boolean = false; // n.n. -  bilinear
              SLE: TSourceLookupEngine = nil;
              DLT: TSphericalLookupTransform = nil;
              updatebar: TUpdateProgressCallback = nil);

procedure SphericalToRectilinearTransform(
              target: TBitmap; source: TBitmap; // image buffers
              alpha_h, alpha_v,       // source FOV angle
              diagonalfov,            // dest FOV angle (full diagonal)
              theta,phi,psi: single; // coordinate system transformation angles
              sle: TSourceLookupEngine;
              finalrender: boolean = false; // n.n. -  bilinear
              fastrendering: boolean = true;
              updatebar: TUpdateProgressCallback = nil);


function MatrixMultiply3x3(var A, B: TRMatrix):TRmatrix;


implementation





  function cpu_id: dword; Assembler;
  asm
        PUSH EAX
        PUSH EBX
		    PUSH EDX
        mov eax,1             ; //Feature Flag abfragen
        CPUID                 ; //CPUID-Befehl ausführen
        mov @Result, edx      ;
		    POP EDX
        POP EBX
        POP EAX
  end;

  function isMMX: Boolean;
  begin
    isMMX := cpu_id AND $00800000 <> 0;
  end;

  function isFPU: Boolean;
  begin
    result := (cpu_id and 1) <> 0;
  end;

  function isSSE: Boolean;
  begin
    result := 0 <> (cpu_id and (1 shl 25));
  end;

  function isSSE2: Boolean;
  begin
    result := 0 <> (cpu_id and (1 shl 26));
  end;

  function isSSE3: Boolean;
  var
    cpsse3: dword;
  begin
    asm
      push eax
      push ebx
      push ecx
      push edx
		  mov eax, 01h       //01h is the parameter for the CPUID command below
		  cpuid
		  // mov cpeinfo, edx   //Get the info stored by CPUID
		  mov cpsse3, ecx    //info about SSE3 is stored in ECX
      pop edx
      pop ecx
      pop ebx
      pop eax
    end;
    result := 0 <> (cpsse3 and 1);
  end;







function MatrixMultiply3x3(var A, B: TRMatrix):TRmatrix;
begin
  result.m11 := A.m11 * B.m11 + A.m12 * B.m21 + A.m13 * B.m31;
  result.m12 := A.m11 * B.m12 + A.m12 * B.m22 + A.m13 * B.m32;
  result.m13 := A.m11 * B.m13 + A.m12 * B.m23 + A.m13 * B.m33;

  result.m21 := A.m21 * B.m11 + A.m22 * B.m21 + A.m23 * B.m31;
  result.m22 := A.m21 * B.m12 + A.m22 * B.m22 + A.m23 * B.m32;
  result.m23 := A.m21 * B.m13 + A.m22 * B.m23 + A.m23 * B.m33;

  result.m31 := A.m31 * B.m11 + A.m32 * B.m21 + A.m33 * B.m31;
  result.m32 := A.m31 * B.m12 + A.m32 * B.m22 + A.m33 * B.m32;
  result.m33 := A.m31 * B.m13 + A.m32 * B.m23 + A.m33 * B.m33;
end;



procedure SphericalToSphericalTransform(
                target: TBitmap; source: TBitmap;
                alpha_h, alpha_v,theta,phi,psi: single;
                polarpinch: single;
                finalrender: boolean = false;
                SLE: TSourceLookupEngine = nil;
                DLT: TSphericalLookupTransform = nil;
                updatebar: TUpdateProgressCallback = nil);
var
  ssc, tsc, issc, itsc: single; // source scaling in pixels/deg, target scaling in pixels/deg
  x, y, u,v, cw, ch,scw, sch: integer; // target x, y, center width, center height
  _theta, _phi: single;
  a, b, c,  d, e, f,  g, h, i: single;

  linedelta, base: Cardinal;
  _y : integer;
  reader: ^Cardinal;
  buf: Cardinal;
  rgb: Array[0..3] of byte absolute buf;
  P, Q, Q2: PByteArray;
  QBase, QLineDelta: Cardinal; // may be negative so high(Cardinal)-value may occur. Don't change to larger datatype!

  rot, A1, A2, A3, Ar: TRMatrix;
  sourcewidth, sourceheight, xxx, xxxr: integer;
  cos__theta, sin__theta,
  sin__phi, cos__phi,
  rad_theta, rad_phi: double;
  tick: longint;

  wrap: Boolean;

  var
    ipx, ipy, y_: integer;
    _w, _h : integer;
    ipixel: Cardinal absolute rgb;
    itype: Cardinal;
    buf11, buf12, buf21, buf22: Cardinal;
    xfactor, yfactor,x_1factor, y_1factor: word;
    px, py: single;
    src: TBitmap absolute source;
    psource, ptarget: ^Cardinal;

    tmp: single;


const
  pi2 = pi/2;
  pi4 = pi/4;
  pi34 = 3*pi/4;
  pi180 = pi/180;
  pipi = 2*pi;
  ipi180 = 180/pi; // inverse : 180/pi

begin
    wrap  := (alpha_h = 360) and (alpha_v = 180) and (round(10 * alpha_h * source.Width / source.Height) = 1800);
    // wrap := false;
    alpha_h :=  pi180 * alpha_h;
    theta   := -pi180 * theta;
    phi     :=  pi180 * phi;
    psi     := -pi180 * psi;

    cw := target.Width  div 2;
    ch := target.Height div 2;

    scw := source.Width  div 2;
    sch := source.Height div 2;

    // modification: reduced widths by 1 to alleviate black line bug
    ssc := alpha_h / (source.Width-1); // besides, use real angles source scaling coefficient
    tsc := alpha_h / (target.Width-1); // same angle, possibly different resolution
    issc := (source.Width-1) / alpha_h;
    // itsc := 1/tsc;

    // source addressing
    base         := Cardinal(source.Scanline[0]);
    linedelta    := Cardinal(source.ScanLine[1]) - base; // negative number as Cardinal -> large complement
    sourcewidth  := source.Width;
    sourceheight := source.Height;

    // set up rotation matrix
    //
    with A3 do
    begin
      m11 :=  1; m12 := 0;         m13 := 0;
      m21 :=  0; m22 :=  cos(phi); m23 := sin(phi);
      m31 :=  0; m32 := -sin(phi); m33 := cos(phi);
    end;

    with A2 do
    begin
      m11 :=  cos(theta); m12 := 0; m13 := sin(theta);
      m21 :=  0;          m22 := 1; m23 := 0;
      m31 := -sin(theta); m32 := 0; m33 := cos(theta);
    end;

    with A1 do
    begin
      m11 :=  1; m12 := 0;         m13 := 0;
      m21 :=  0; m22 :=  cos(psi); m23 := sin(psi);
      m31 :=  0; m32 := -sin(psi); m33 := cos(psi);
    end;

    Ar  := MatrixMultiply3x3(A2, A1);
    rot := MatrixMultiply3x3(A3, Ar);

  //windows.beep(1000,1);
  if assigned(SLE) then
    SLE.polarpinch := polarpinch;

  if not finalrender then
  begin
    tick := GetTickCount;

    for y := 0 to (target.Height-1) do
    begin
      if y mod 128 = 0 then
      begin
        Application.ProcessMessages;
      end;

      P := target.ScanLine[y];
      for x := 0 to (target.Width-1) do
      begin
        DLT.Lookup(x, y, a, b, c);

       with rot do
        begin
          h :=   m11 * a + m12 * b + m13 * c;
          g := -(m21 * a + m22 * b + m23 * c);
          i :=   m31 * a + m32 * b + m33 * c;
        end;

        SLE.ISLookup(u,v,g,h,i);

        if (u >= 0) and (u < SourceWidth) and
           (v >= 0) and (v < SourceHeight) then
        begin
          Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u));
          buf := Reader^;
        end
        else
          buf := 0;

        xxx := 3*x;
        P[xxx]   := rgb[0];
        P[xxx+1] := rgb[1];
        P[xxx+2] := rgb[2];

      end;// for x
    end; // for y

  // application.MainForm.caption := IntToStr(GetTickCount - Tick);

  end // if not finalrender
  else
  begin // FINAL RENDER!

    Q  := src.ScanLine[0];
    Q2 := src.ScanLine[1];
    y_ := 0;
    QBase      := Cardinal(@Q[0]);
    QLineDelta := Cardinal(@Q[0])-Cardinal(@Q2[0]); // invert negative linedelta :)

    _w := source.Width;
    _h := source.Height;

    for y := 0 to target.Height-1 do
    begin
      if assigned(updatebar) and (y mod 16 = 0)
        then updatebar(y, target.Height-1);

      P := target.ScanLine[y];
      for x := 0 to target.Width-1 do
      begin
        // inverse transform to source coordinates
        _theta := pi2 + tsc * (y-ch);
        _phi   :=       tsc * (x-cw);

        if _phi >  pi then _phi := _phi -pipi;
        if _phi < -pi then _phi := _phi +pipi; 

        sin__theta := sin(_theta);

        a := sin__theta* cos(_phi);
        b := sin__theta* sin(_phi);
        c := cos(_theta);

        with rot do
        begin
          g := m11 * a + m12 * b + m13 * c;
          h := m21 * a + m22 * b + m23 * c;
          i := m31 * a + m32 * b + m33 * c;
        end;

        _phi   := PreciseArg(g,h);
        _theta := arccos(i);

        if _phi >  pi then _phi := _phi -pipi;
        if _phi < -pi then _phi := _phi +pipi;


        //px := trunc(scw + issc*       _phi);
        //py := trunc(sch + issc*(_theta-pi2));

        {if (u >= 0) and (u < SourceWidth) and
           (v >= 0) and (v < SourceHeight) then
        begin
          Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u));
          buf := Reader^;
        end
          else buf := 0; //}



        // load aa pixel into buffer
        px := scw + issc*       _phi;
        py := sch + issc*(_theta-pi2) - polarpinch;
        ipx := trunc(px);
        ipy := trunc(py);

        if (wrap or ((ipx>=0) and (ipx <_w) and (ipy >=0) and (ipy < _h))) then
        begin

          if ipy <> y_ then
          begin
            y_ := ipy;
            Q  := Pointer( base + (Cardinal(ipy)  ) mod Cardinal(_h) * linedelta  );
            Q2 := Pointer( base + (Cardinal(ipy)+1) mod Cardinal(_h) * linedelta  );
            if (ipy +1 = _h) then
            begin
              if not wrap
                then Q2 := Q
                else Q2 := Pointer( base );
            end;
          end;

          //ReadSourceInterPixel;
          xxx  := ipx*3;
          xxxr :=(ipx+1) mod (_w) * 3;
          if not wrap then
            if (ipx +1 = _w) then xxxr := xxx;

          xfactor := floor(256*(px - ipx));
          x_1factor := 256-xfactor;
          yfactor := floor(256*(py - ipy));
          y_1factor := 256-yfactor;


          ipixel := ( ((Q [xxx   ]*x_1factor) +
                       (Q [xxxr  ]*  xfactor) ) shr 8 * y_1factor
                     +((Q2[xxx   ]*x_1factor) +
                       (Q2[xxxr  ]*  xfactor) ) shr 8 * yfactor) shr 8 and $FF or

                    ( ((Q [xxx +1]*x_1factor) +
                       (Q [xxxr+1]*  xfactor) ) shr 8 * y_1factor
                     +((Q2[xxx +1]*x_1factor) +
                       (Q2[xxxr+1]*  xfactor) ) shr 8 * yfactor) and $FF00 or

                    ( ((Q [xxx +2]*x_1factor) +
                       (Q [xxxr+2]*  xfactor) ) shr 8 * y_1factor
                     +((Q2[xxx +2]*x_1factor) +
                       (Q2[xxxr+2]*  xfactor) ) shr 8 * yfactor) shl 8 and $FF0000;


          xxx := 3*x;
          P[xxx]   := rgb[0]; P[xxx+1] := rgb[1]; P[xxx+2] := rgb[2];

        end; // if [ranges]

      end;// for x
    end; // for y
  end; // final render

end;









procedure SphericalToRectilinearTransform(
              target: TBitmap; source: TBitmap; // image buffers
              alpha_h, alpha_v,       // source FOV angle
              diagonalfov,            // dest FOV angle (full diagonal)
              theta,phi,psi: single; // coordinate system transformation angles
              sle: TSourceLookupEngine;
              finalrender: boolean = false; // n.n. -  bilinear
              fastrendering: boolean = true;
              updatebar: TUpdateProgressCallback = nil);

var
  tick: longint;
  rot, A1, A2, A3, Ar: TRMatrix;

  procedure PreviewTransformSimple;
  var
    p: ^DWORD;
    dstrmax, dstip: single;
    dst : TBitmap absolute target;
    src : TBitmap absolute source;
    dcx, dcy, scx, scy: integer;
    x, y, xxx: integer;
     Q: PByteArray; //P: source, Q: dest
    r, dsttheta, dstphi: single;

    reader: ^Cardinal;
    buf: Cardinal;
    rgb: Array[0..3] of byte absolute buf;
    u,v: integer;
    a, b, c, g, h, i, k: single;
    SourceWidth, SourceHeight, DestLineBytes: integer;
    base, linedelta: Cardinal;
    norm: double;
    parity:integer;

    XWithinRange, YWithinRange: Boolean;
  begin

    // render parameters
    //
    dstrmax := sqrt(sqr(dst.width) + sqr(dst.height)) * 0.5; // half diagonal pixel size
    if tan(diagonalfov) = 0 then exit;

    DestLineBytes := dst.Width*3;
    dcx := dst.Width  div 2;
    dcy := dst.Height div 2;

    scx := src.Width  div 2;
    scy := src.Height div 2;

    // source addressing
    base         := Cardinal(src.Scanline[0]);
    linedelta    := Cardinal(src.ScanLine[1]) - base;
    SourceWidth := src.Width;
    SourceHeight := src.Height;

    k := 1/diagonalfov * (normallen/(2*dstrmax));

    if fastrendering then
    begin
    parity := dst.height mod 2;

    for y := dst.Height div 2 downto 1-parity  do
    begin
      Q := dst.ScanLine[y*2+parity-1];
      for x := 0 to dst.Width div 2 do
      begin
        g := -(x*2-dcx)*k;
        i := -((y*2+parity-1)-dcy)*k;

        norm := 1 / (sqrt(g*g+i*i+1));
        a := g*norm;
        b :=   norm;
        c := i*norm;

        with rot do
        begin
          g := m11 * a + m12 * b + m13 * c;
          h := m21 * a + m22 * b + m23 * c;
          i := m31 * a + m32 * b + m33 * c;
        end;

        SLE.ISLookup(u,v,g,h,i);

        XWithinRange := (u >= 0) and (u < SourceWidth);
        YWithinRange := (v >  0) and (v < SourceHeight);

        if XWithinRange and YWithinRange then
        begin
            Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u));
            buf := Reader^;
        end
        else
          if XWithinRange and (v = 0) then
          begin
             if u < SourceWidth -1 then
             begin
               Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u));
               buf := Reader^;
             end
             else
             begin
               Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u) -1);  // realign block
               buf := (Reader^) shr 8; // remove displacement again
             end;
          end
          else
            buf := 0;

        xxx := x*6;
        // copy 3 pixels at a time and duplicate
        p  := @Q[xxx];
        p^ := buf;
        p  := Pointer(Cardinal(P)+3); // paint next pixel, too
        p^ := buf;
      end;
    end;

    for y := 0 to dst.Height div 2-1  do
    begin
      CopyMemory(dst.ScanLine[y*2+parity],dst.ScanLine[y*2+1+parity],DestLineBytes);
    end;


    end  // fast rendering
    else
    begin
      for y := 0 to dst.Height-1  do
      begin
        Q := dst.ScanLine[y];
        for x := 0 to dst.Width-1 do
        begin
          g := -(x-dcx)*k;
          i := -(y-dcy)*k;

          norm := 1 / (sqrt(g*g+i*i+1));
          a := g*norm;
          b :=   norm;
          c := i*norm;

          with rot do
          begin
            g := m11 * a + m12 * b + m13 * c;
            h := m21 * a + m22 * b + m23 * c;
            i := m31 * a + m32 * b + m33 * c;
          end;

          SLE.ISLookup(u,v,g,h,i);

        XWithinRange := (u >= 0) and (u < SourceWidth);
        YWithinRange := (v >  0) and (v < SourceHeight);

        if XWithinRange and YWithinRange then
        begin
            Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u));
            buf := Reader^;
        end
        else
          if XWithinRange and (v = 0) then
          begin
             if u < SourceWidth -1 then
             begin
               Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u));
               buf := Reader^;
             end
             else
             begin
               Reader := Pointer(base + Cardinal(v) * linedelta + 3*Cardinal(u) -1);
               buf := (Reader^) shr 8;
             end;
          end
          else
            buf := 0;

          xxx := x*3;
          Q[xxx]   := rgb[0];
          Q[xxx+1] := rgb[1];
          Q[xxx+2] := rgb[2];
       end;
     end;

    end; // end normal rendering

  end;

  procedure PreviewTransformMMXSSE;
  begin
    PreviewTransformSimple;
  end;

  procedure FinalTransformSimple;
  begin
  end;

  procedure FinalTransformMMXSSE;
  begin
  end;


begin
  // change all parameters from degrees to radians
  theta := pi * theta / 180;
  phi := pi * phi / 180;
  psi := pi * psi / 180;

  alpha_v := pi * alpha_v / 180;
  alpha_h := pi * alpha_h / 180;

  tick := gettickcount;

  if not assigned(SLE) then exit;

    // set up rotation matrix
    //
    with A3 do
    begin
      m11 :=  cos(phi);  m12 := sin(phi);  m13 := 0;
      m21 :=  -sin(phi); m22 := cos(phi);  m23 := 0;
      m31 :=  0;         m32 := 0;         m33 := 1;
    end;

    with A2 do
    begin
      m11 :=  1; m12 := 0;         m13 := 0;
      m21 :=  0; m22 :=  cos(theta); m23 := sin(theta);
      m31 :=  0; m32 := -sin(theta); m33 := cos(theta);
    end;

    with A1 do
    begin
      m11 :=  cos(psi); m12 := 0; m13 := sin(psi);
      m21 :=  0;        m22 := 1; m23 := 0;
      m31 := -sin(psi); m32 := 0; m33 := cos(psi);
    end;

    Ar  := MatrixMultiply3x3(A2, A1);
    rot := MatrixMultiply3x3(A3, Ar);

  // select appropriate rendering mode
  if finalrender then
    begin
      if isMMX and isSSE then
          FinalTransformMMXSSE
        else
          FinalTransformSimple;
    end
  else
    begin                      
      if isMMX and isSSE then
          PreviewTransformMMXSSE
        else
          PreviewTransformSimple;
    end;

  tick := gettickcount - tick;
  // target.Canvas.TextOut(10,10,inttostr(tick)) ;

end;


end.





