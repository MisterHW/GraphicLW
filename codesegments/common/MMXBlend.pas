/////////////////////////////////////////////////////////////
//                                                         //
//     Alpha blending powered by MMX commands              //
//     programmed by Helge Wurst 2003..                    //
//                                                         //
/////////////////////////////////////////////////////////////

// * 2003    : first implementation (blend two bitmaps to a target buffer using a global transparency value)
// * 2009-06 : some register corruption problems fixed
//           : added DrawAlphaRect (render a bitmap onto another bitmap with transparency mask)
// * 2009-12 : changed the blend() routine input parameter scaling of XOFFSET from pixels to subpixels (no byte-alignment errors under windows 7)
//           : fixed modulo problem in Blend (dropped subpixels)
// * 2011-01 : included correction for $FF / $100 complementary coefficients by adding to word factors
//           : test (A*coeff + B*($FF-coeff + 2) ) div 2 for numerical stability, revise code if needed - test looks ok!

// ---------------------------------------------------------
// todo:
//
// - include non-MMX blend-rect routine for machines that do not feature MMX
// - include range checks into blend-rect routine
// -



unit MMXBlend;

interface

uses
  Windows, SysUtils, Graphics, Math;

   function isMMX:Boolean; // keep this even after BlendRect has non-MMX backup routine

   procedure BlendRect(
                       Destination: TBitmap;  // DestRect = BackgrndRect
                       Background: TBitmap;
                       Foreground: TBitmap;   // foregroundrect = 0,0,width,height or BackgroundRect.TopLeft, width+..., height+...
                       DestRect: TRect;
                       BlendFactor: Byte;
                       SetForegroundOrigin: Boolean = false);

  procedure DrawAlphaRect(
                       Destination: TBitmap; DestRect: TRect; // ((x0, y0)(x0+w, y0+h))
                       Graphic    : TBitmap;  // ((0,0)(w,h))
                       BlendMask  : TBitmap); // ((0,0)(w,h))


implementation

var
  Z, Q, V, A: PBytearray;
  exitZ, exitA, exitV, exitQ : PByteArray;
  rel: Integer;

const
  idstr : string = #13#10#13#10#13#10'This Unit was programmed by Helge Wurst, 2003-2011. You may not modify, disassemble or publish this code without written permission.'#13#10#13#10#13#10;



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



  procedure blend(XOFFSET: Integer; XPixels: Integer); //assembler;
  var
    i: Integer;
    f: integer;
  asm
        mov EAX, XOFFSET;   // XOFFSET in pixels * pixelformat / 8bit , fixed pixel misalignment under Windows 7

   mov EDX , XPixels;  // Anzahl der 4er Blöcke
   mov i, EDX;


        mov EDX, Z;      // Startaddresse
        add EDX, EAX;

        mov ECX, Q;      // Hintergrundzeile
        add ECX, EAX;

        mov  EAX, $0;
        movd MM2, EAX; // MM2 ist ????0000

        mov EAX, rel;  // Multiplikationsschlüssel
        sal EAX,8;
        add EAX, rel;
        sal EAX,8;
        add EAX, rel;
        sal EAX,8;
        add EAX, rel;
        movd MM7, EAX;       // 00 00 00 00 80 80 80 80                    // für Multiplikation Pixel*faktor
        punpcklbw MM7, MM2;  // 00 80 00 80 00 80 00 80

        not EAX; // warning, this is ones's complement!
        movd MM6, EAX;                                                     // für Multiplikation Pixel*(256-faktor)
        punpcklbw MM6, MM2;  // 00 FF-80 00 FF-80 00 FF-80 00 FF-80

        // added hotfix for non-identity problem with multiplication factors
        mov EAX, $02020202;  // add 2, fixes the $FF / $100 problem
        movd MM0, EAX;
        punpcklbw MM0, MM2;
        paddw MM6, MM0;

        mov  EAX, V;     // Vordergrund

  @PixelLoop:


        movd MM0, [ECX];     // Werte A Laden     0000AAAA
        punpcklbw MM0, MM2;  // 0A0A0A0A
        pmullw  MM0, MM7;    // 0A0A0A0A *  00 80 00 80 00 80 00 80 = A' .. A' .. A' .. A' ..   (.. ist der uninteressante Rest)
        psrlw MM0, 8;        // schiebe 8 bit nach Rechts             0  A' .. A' .. A' .. A'

        movd MM1, [EAX];     // Wert B 0000BBBB Vordergrund ebenso blenden
        punpcklbw MM1, MM2;
        pmullw  MM1, MM6;
        psrlw MM1, 8;

        paddw MM0, MM1;

        packuswb MM0, MM2;   // 0000A'A'A'A'
        movd [EDX], MM0;     // Ausgabe der unteren 4 Byte;

        add EDX, 4; // nächster Block
        add ECX, 4;
        add EAX, 4;
  dec i;
  cmp i, $0;
  jne @PixelLoop;

  emms; // MMX zurücksetzen

  mov exitV, EAX;
  mov exitQ, ECX;
  mov exitZ, EDX;

  end;



  function MovePointer( p: Pointer; padd: longword):Pointer;
  asm
    PUSH EAX
    MOV EAX, p;
    ADD EAX, padd;
    MOV @Result, EAX;
    POP EAX
  end;



  procedure BlendRect(Destination: TBitmap; Background: TBitmap; Foreground: TBitmap; DestRect: TRect; BlendFactor: Byte; SetForegroundOrigin: Boolean = false); // DestRect = BackgrndRect foregroundrect = 0,0,width,height
  var i,y: integer;
  begin
   if (isMMX) and (background.PixelFormat = foreground.pixelformat) and (foreground.PixelFormat = destination.PixelFormat) then
     begin
        if Foreground.PixelFormat = pf32bit then
        begin
          for y := 0 to DestRect.Bottom- DestRect.top-1  do
          begin
            Z := Destination.ScanLine[y+DestRect.top];
            Q := Background.ScanLine [y+DestRect.top];
              if not SetForegroundOrigin
                then V := MovePointer(Foreground.ScanLine[y+abs(DestRect.top)],DestRect.Left*4)
                else V := Foreground.ScanLine[y];
            rel := BlendFactor;
            blend( DestRect.Left,(Destrect.Right - DestRect.Left));
          end;
       end
       else
         if Foreground.PixelFormat = pf24bit then
         begin
            for y := 0 to DestRect.Bottom- DestRect.top-1  do
            begin
              Z := Destination.ScanLine[y+DestRect.top];
              Q := Background.ScanLine [y+DestRect.top];
              if not SetForegroundOrigin
                then V := MovePointer(Foreground.ScanLine[y+abs(DestRect.top)],DestRect.Left*3)
                else V := Foreground.ScanLine[y];
              rel := BlendFactor;
              blend( DestRect.Left*3,(Destrect.Right - DestRect.Left) *3 shr 2);
              for i := 0 to (Destrect.Right - DestRect.Left) *3 mod 4 -1 do // do the odd subpixels
                exitZ[i] := (exitV[i]*BlendFactor + exitQ[i]*($100-BlendFactor)) shr 8; // changed from $ff to &100
            end;
         end;
     end
   else
     begin
       if not isMMX then
       begin
         windows.MessageBox(0,'MMX assembler commands not supported.', 'Error', MB_OK);
       end
       else
         windows.MessageBox(0,'Pixel formats differ.', 'Error', MB_OK);
     end;
  end;





procedure maskblend(XOFFSET: Integer; XPixels: Integer);
var
  i: Integer;
asm
push EBX
        // -- rearranged code, no need to stuff this in the pixel loop
        push EBX
        mov EBX, $0;
        movd MM2, EBX;       // MM2 : XX XX XX XX 00 00 00 00

        mov EBX, $02020202;
        movd MM3, EBX;
        punpcklbw MM3, MM2;  // MM3 : 00 01 00 01 00 01 00 01 // should be 00 02 ..., please re-evaluate for divide-by-2 glitches when changing this value!
        pop EBX
        // --

        mov EAX, XOFFSET;    // XOFF
        imul EAX, 3          // 24bit pixel format : multiply by 3

        mov EDX , XPixels;   // Anzahl der 4er Blöcke
        mov i, EDX;

        mov EDX, Z;          // Basisbildadresse -> EDX
        add EDX, EAX;

        mov ECX, A;          // Maskenadresse -> ECX

        mov  EAX, V;         // Overlayadresse -> EAX

  @PixelLoop:

   // prepare multiplication coefficients
        push EBX

        mov EBX, [ECX];      // load mask bytes from memory
        movd MM7, EBX;       // MM7 : XX XX XX XX AA BB CC DD
                             // für Multiplikation Pixel*faktor
        punpcklbw MM7, MM2;  // MM7 : 00 AA 00 BB 00 CC 00 DD
        not EBX;             // 1s complement
        movd MM6, EBX;       // MM6 : XX XX XX XX (FF-AA) (FF-BB) (FF-CC) (FF-DD)
                             // für Multiplikation Pixel*(256-faktor)
        punpcklbw MM6, MM2;  // MM6 : 00 (FF-AA) 00 (FF-BB) 00 (FF-CC) 00 (FF-DD)
        paddw MM6, MM3;      // compensate for $FF vs $100 error

        pop EBX

   // multiply
        movd MM0, [EAX];     // Werte A laden  0000AAAA
        punpcklbw MM0, MM2;  // 0A0A0A0A
        pmullw  MM0, MM7;    // 0A0A0A0A *  00 80 00 80 00 80 00 80 = A' .. A' .. A' .. A' ..   (.. ist der uninteressante Rest)
        psrlw MM0, 8;        // schiebe 8 bit nach Rechts             0  A' .. A' .. A' .. A'

        movd MM1, [EDX];     // Werte B laden  0000BBBB
        punpcklbw MM1, MM2;
        pmullw  MM1, MM6;
        psrlw MM1, 8;

    // merge
        paddw MM0, MM1;
        packuswb MM0, MM2;   // 0000A'A'A'A' (= "divide by 256")
        movd [EDX], MM0;     // Ausgabe der unteren 4 Byte;

        add EDX, 4;          // nächster Block
        add ECX, 4;
        add EAX, 4;
  dec i;
  cmp i, $0;
  jne @PixelLoop;

  emms; // MMX zurücksetzen
pop EBX
end;






 // all images need to be  pf24bit
  procedure DrawAlphaRect(Destination: TBitmap; DestRect: TRect; // ((x0, y0)(x0+w, y0+h))
                          Graphic    : TBitmap;  // ((0,0)(w,h))
                          BlendMask  : TBitmap); // ((0,0)(w,h))
  var
    x, y, w: integer;
    dZ, dA, dV : Cardinal;
    c: integer;
    z_base, va_base : integer;
    // tick, erg: longint;
    graphorg: TPoint; // graphic origin;
  begin

   if (Destination.PixelFormat <> pf24bit) or
      (Graphic.PixelFormat     <> pf24bit) or
      (BlendMask.PixelFormat   <> pf24bit) then
   begin
     MessageBox(0,'Internal Error: input graphic format violation '#13#10+
      'One or more bitmaps are not 24 bit type.',
      'Error at MMXBlend.DrawAlphaRect',MB_ICONERROR);
     exit;
   end;

   if (not assigned(Destination)) or
      (not assigned(Graphic))     or
      (not assigned(BlendMask))   then
   begin
     MessageBox(0,'Internal Error: input graphic violation '#13#10+
      'One or more bitmaps are not assigned (NULL).',
      'Error at MMXBlend.DrawAlphaRect',MB_ICONERROR);
     exit;
   end;

   graphorg.X := -min(0,destrect.Left)*3;
   graphorg.Y := -min(0,destrect.Top);

   // clip to available mask and graphic area
   destrect.Right  := destrect.Left + min(min(Graphic.Width , BlendMask.Width ),destrect.Right- destrect.Left);
   destrect.Bottom := destrect.Top  + min(min(Graphic.Height, BlendMask.Height),destrect.Bottom- destrect.top);

   // clip to avaiable target image area
   destrect.Left  := max(0,min(Destination.Width , destrect.Left  ));
   destrect.Top   := max(0,min(Destination.Height, destrect.Top   ));
   destrect.Right := max(0,min(Destination.Width , destrect.Right ));
   destrect.bottom:= max(0,min(Destination.Height, destrect.Bottom)); // }

   // clip to area to be rendered



   if isMMX then // MMX optimized code is 5.03x faster than byte-wise processing on an AMD 2500+
     begin
        Z := Destination.ScanLine[DestRect.top];
        A := Pointer(Cardinal( BlendMask.ScanLine[graphorg.Y]) + graphorg.X);
        V := Pointer(Cardinal( Graphic.ScanLine[graphorg.Y]  ) + graphorg.X);

        if  DestRect.Bottom <> DestRect.top then
        begin
          dZ := Cardinal(Destination.ScanLine[1]) -  Cardinal(Destination.ScanLine[0]);
          dA := Cardinal(  BlendMask.ScanLine[1]) -  Cardinal(  BlendMask.ScanLine[0]);
          dV := Cardinal(    Graphic.ScanLine[1]) -  Cardinal(    Graphic.ScanLine[0]);
        end;

        z_base  := DestRect.Left*3+(((destrect.Right-DestRect.Left)* 3) div 4) * 4;                  // calculate x offset for Z
        va_base := (((destrect.Right-destrect.Left)* 3) div 4) * 4;// calculate x offset for A, V
        for y := 0 to (DestRect.Bottom-DestRect.top)-1  do
        begin
          // blend line
          maskblend(DestRect.Left,(DestRect.Right- DestRect.Left)*3 shr 2);

          // process missing pixel color components
          for c := 0 to ((destrect.Right-destrect.left)*3) mod 4 -1  do
          begin
             Z[z_base+c] :=  (
                Z[ z_base+c] * (256 - A[va_base+c]) +
                V[va_base+c] * (      A[va_base+c]) ) shr 8;
          end;

          Z := Pointer(Cardinal(Z) + dZ);
          A := Pointer(Cardinal(A) + dA);
          V := Pointer(Cardinal(V) + dV);
        end;
     end
   else
     begin // compatibility mode
        Z := Destination.ScanLine[DestRect.top];
        A := Pointer(Cardinal( BlendMask.ScanLine[graphorg.Y]) + graphorg.X);
        V := Pointer(Cardinal( Graphic.ScanLine[graphorg.Y]  ) + graphorg.X);

        if  DestRect.Bottom <> DestRect.top then
        begin
          dZ := Cardinal(Destination.ScanLine[1]) -  Cardinal(Destination.ScanLine[0]);
          dA := Cardinal(  BlendMask.ScanLine[1]) -  Cardinal(  BlendMask.ScanLine[0]);
          dV := Cardinal(    Graphic.ScanLine[1]) -  Cardinal(    Graphic.ScanLine[0]);
        end;

        for y := 0 to (DestRect.Bottom-DestRect.top)-1  do
        begin
          // blend line
          for x := 0 to (DestRect.right-DestRect.left)*3-1 do
          begin
            Z[DestRect.left*3 + x] := (
               Z[DestRect.left*3 + x] * (256 - A[x]) +    // changed from 255 to 256 for obvious reasons
               V[x]                   * (      A[x]) ) shr 8;
          end;

          Z := Pointer(Cardinal(Z) + dZ);
          A := Pointer(Cardinal(A) + dA);
          V := Pointer(Cardinal(V) + dV);
        end;
     end;

  end;








end.
