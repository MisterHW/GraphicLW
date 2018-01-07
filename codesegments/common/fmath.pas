unit fmath;

interface

function arccos_s6(x: single):single;
function sin_6(x: single):single;
function RndSingleParity(s: single): Cardinal;

implementation

var
  xs : single;

const
  a = +1.493415037;
  b = -0.159437656;
  c = +0.48560268e-1;
  d = -0.199230397e-1;
  e = +0.94214809e-2;
  f = -0.48413448e-2;
  g = +0.2627085e-2;

function arccos_s6(x: single):single; // error < 4 ppm within full range [-1..1]
begin
  if x > 0 then
    begin
        if x > 1 then x := 1;
        xs :=   x - 0.42;
        result :=   sqrt(1-x) * ((((((g*xs+f)*xs+e)*xs+d)*xs+c)*xs+b)*xs + a);
    end
  else
    begin
        if x < -1 then x := -1;
        xs := - x - 0.42;
        result := - sqrt(1+x) * ((((((g*xs+f)*xs+e)*xs+d)*xs+c)*xs+b)*xs + a) + pi;
    end;
end;



var
  RNDSP_EXP : Cardinal;
  RNDSP_LOOP: integer;
function RndSingleParity(s: single): Cardinal; // zero: even, non-zero: odd
var
  c : Cardinal absolute s;
begin
    c := ((c + (1 shl 23)) and $3FFFFFFF) shl 1;
    RNDSP_EXP := c shr 24;
    c := (c  shl 7) ; //or (1 shl 31);
    if RNDSP_EXP < 25
      then for RNDSP_LOOP := RNDSP_EXP downto 1 do c := c shl 1
      else c := 0;
    result := c;
end;



var
  fcw16  : Word;
  fbuf32 : single;
  ibuf32 : cardinal absolute fbuf32;
const
  sin_6_ipi: single  = 1/pi;
  sin_6_a:single = 1;// sin(0.5*Pi);
  sin_6_c:single = -4.93429; // fitted values
  sin_6_e:single =  4.04506;
  sin_6_g:single = -1.23285;
  sin_6_p:single = 0.5; // argument offset for the polynomial
  sin_6_shift:single = 1E2; // must be even
function sin_6(x: single):single;
begin
  asm


    // set FRNDINT mode to truncate
		fstcw fcw16;
		mov ax, fcw16 ;
		and ax, $f0ff ;      // Clears bits 8-11.
		or ax, $0c00 ;       // Rounding control=%11, Precision = %00.
		mov fcw16 , ax;
		fldcw fcw16;

    // perform
    fld [sin_6_p]               // keep 0.5 for later
    fld x;                      // |x|||
    fld [sin_6_ipi];            // |ipi|x|
    fmul;                       // |ipi*x||

    fld [sin_6_shift]
    fadd                        // add 10^2 to avoid the crack at the zero point. don't forget this anomaly,
                                //  limit input range to <100 pi , this shall do for most applications within -4pi..4pi
    fld st(0);                  // |ipi*x|ipi*x|
    frndint                     // |int(ipi*x)|ipi*x|
    fst fbuf32                  // store for subsequent parity analysis
    fsub                        // |ipi*x- int(ipi*x)||  = |modulo(ipi*x)||
    fabs                        // |abs(modulo(ipi*x)|| 0..1
    fsub                        // |abs(modulo(ipi*x)-0.5|| = |x||||||
    fld st(0)
    fmul                        // |x^2|||

    fld st(0)                   // |x^2|x^2||
    fmul st(1), st(0)           // |x^2|x^4||
    fld st(0)                   // |x^2|x^2|x^4||
    fmul st(0), st(2)           // |x^6|x^2|x^4||

    fld [sin_6_g]               // |g|x^6|x^2|x^4||
    fmulp st(1), st(0)          // |g*x^6|x^2|x^4||
    fld [sin_6_e]               // |e|g*x^6|x^2|x^4||
    fmulp st(3), st(0)          // |g*x^6|x^2|e*x^4||
    fld [sin_6_c]               // |c|g*x^6|x^2|e*x^4||
    fmulp st(2), st(0)          // |g*x^6|c*x^2|e*x^4||
    fld [sin_6_a]               // |a|g*x^6|c*x^2|e*x^4||
    fadd                        // |a + g*x^6|c*x^2|e*x^4||
    fadd                        // |a + g*x^6 + c*x^2|e*x^4||
    fadd                        // |a + g*x^6 + c*x^2 + e*x^4||

    fstp  [ESP]                 // return st(0) = a + c*x^2 + e*x^4 + g*x^6


  end;

  // find int-part parity and correct result sign (32bit IEEE float only)
  //
  ibuf32 := ((ibuf32 + (1 shl 23)) and $3FFFFFFF) shl 1;
  RNDSP_EXP := ibuf32 shr 24;
  ibuf32 := (ibuf32  shl 7); //or (1 shl 31);

  if RNDSP_EXP < 25
    then for RNDSP_LOOP := RNDSP_EXP downto 1 do ibuf32 := ibuf32 shl 1
    else ibuf32 := 0;

  // now ibuf is 0 for even integer-parts of the normalized argument
  // zero for positive half-waves
  //
  if LongBool(ibuf32) then result := -result;
end;




end.
