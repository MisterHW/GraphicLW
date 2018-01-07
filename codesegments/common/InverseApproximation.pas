unit InverseApproximation;

interface

type TVar1SingleObjProc = procedure(sender: TObject; var s : single ); stdcall;

type TSingleArray = array of Single;
     PSingleArray = ^TSingleArray;

type TInverseLookupFunction = class
    constructor Create(func_lower_lim, func_upper_lim: single;
        resolution: single; func: TVar1SingleObjProc; sender: TObject; init_arg: single);
    destructor Destroy;  override;

  private
    table: Array of Single;
    Ia, Ib : single;
    ref_factor: single;
    ref_offset: single;
    pre_idx: single;
    idx_intpart: longint;
    idx_oddpart: single;
    size: longint;
  public
    procedure inverse(var s: single); stdcall;
end;

function MonotonieRadius(lower, upper: single; f: TVar1SingleObjProc; sender: TObject = nil): single;



implementation
uses
  windows, math;



  constructor TInverseLookupFunction.Create(func_lower_lim, func_upper_lim: single;
        resolution: single; func: TVar1SingleObjProc; sender: TObject; init_arg: single);
    var
      i: integer;
      f_soll,r_test, r_test1, r_test2, dev: single;
      iterationcnt: integer;
    const
      epsilon   = 1/1000000;
      tolerance = 0.0005;
    function ext_f(s: single): single;
      begin
        func(sender,s);
        result := s;
      end;
    begin
      inherited Create();
      Ia := min(func_lower_lim, func_upper_lim);
      Ib := max(func_lower_lim, func_upper_lim);
      ref_offset := func_lower_lim;
      ref_factor := 1/resolution;
      size := ceil((Ib-Ia)/resolution);
      setlength(table,size);
      //table := system.GetMemory(size*sizeof(Single));
      // System.GetMem(table,size*sizeof(Single)); // system.GetMemory(size*sizeof(single));

      r_test := init_arg; // Startargument
      for i := 0 to size-1 do
      begin
        f_soll := (i / ref_factor) + ref_offset;
        iterationcnt := 0;
        while abs(ext_f(r_test) - f_soll) > tolerance do
        begin
          dev := (ext_f(r_test + epsilon) - ext_f(r_test))/epsilon;
          if dev = 0 then
          begin
            // asm nop; end;
            break; // error, non-monotonous area detected
          end;
          r_test1 := r_test - (ext_f(r_test) - f_soll)/dev;

          dev := (ext_f(r_test1 + epsilon) - ext_f(r_test1))/epsilon;
          if dev = 0 then exit; // error, non-monotonous area detected
          r_test2 := r_test1 - (ext_f(r_test1) - f_soll)/dev;

          if sign (r_test - r_test1) <> sign (r_test1 - r_test2) then
            r_test := (r_test + r_test1)/2
          else
            r_test := r_test2;

          inc(iterationcnt);
          if iterationcnt > 20 then
          begin
            //Windows.MessageBox(0,'','',mb_ok);
            //asm nop; end;
            break;
          end;

        end; // while
        table[i] := r_test;
      end;  // for i
    end;


    destructor TInverseLookupFunction.Destroy;
    begin
      // system.FreeMemory(table);
      //setlength(table,0);
      inherited Destroy();
    end;

    procedure TInverseLookupFunction.inverse(var s: single); stdcall;
    begin
        pre_idx := (s-ref_offset)*ref_factor;
        idx_intpart := round(pre_idx-0.49999999);
        if idx_intpart < 0 then idx_intpart := 0;
        if idx_intpart >= size then idx_intpart := size-1;
        idx_oddpart := pre_idx - idx_intpart;
        s := table[idx_intpart]*(1-idx_oddpart) + table[idx_intpart+1]*idx_oddpart;
    end;



function MonotonieRadius(lower, upper: single; f: TVar1SingleObjProc; sender: TObject): single;
const
  epsilon = 1/100000;
  delta   = 1/ 10000;
var
  testsign : integer;
  i: longint;
  k: single;
  function ext_f(s: single): single;
  begin
    f(sender,s);
    result := s;
  end;
begin
  result := upper + delta;
  testsign := sign(ext_f(2* epsilon)-ext_f(epsilon));
  for i := 0 to ceil((upper-lower)/delta) do
  begin
    k := i * delta;
    if testsign <> sign(ext_f(k+epsilon)-ext_f( k)) then
    begin
      result := k-epsilon;
      break;
    end;
  end;
end;


end.
