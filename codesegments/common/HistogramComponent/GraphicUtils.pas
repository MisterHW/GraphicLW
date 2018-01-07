unit GraphicUtils;
/////////////////////////////////////////////////////////////////////////////////
//
//  Misc. rendering procedures
//

interface
  uses
    Graphics;

  function InterpolateColor(c1,c2: TColor; factor: Single): TColor;
  procedure RenderGradient(Can: TCanvas; x,y,w,h: integer; c1, c2: TColor);
  procedure RenderCenterGradient(Can: TCanvas; x,y,w,h: integer; c1, c2: TColor);
implementation

  function InterpolateColor(c1,c2: TColor; factor: Single): TColor; // factor: 0..1
  var
    complementfactor: single;
    col1: Array[0..3] of byte absolute c1;
    col2: Array[0..3] of byte absolute c2;
    res : Array[0..3] of byte absolute result;
  begin
    complementfactor := 1- factor;
    res[0] := round(col1[0]* complementfactor + col2[0]*factor);
    res[1] := round(col1[1]* complementfactor + col2[1]*factor);
    res[2] := round(col1[2]* complementfactor + col2[2]*factor);
    res[3] :=0;
  end;

  procedure RenderGradient(Can: TCanvas; x,y,w,h: integer; c1, c2: TColor);
  var i: integer;
  begin
    for i := 0 to h do
    begin
      can.Pen.Color := InterpolateColor(c1,c2,i/h);
      Can.MoveTo(x  ,y+i);
      Can.LineTo(x+w,y+i);
    end;
  end;

  procedure RenderCenterGradient(Can: TCanvas; x,y,w,h: integer; c1, c2: TColor);
  var i: integer;
  begin
    for i := 0 to h do
    begin
      can.Pen.Color := InterpolateColor(c1,c2,sin(pi*1/h));
      Can.MoveTo(x  ,y+i);
      Can.LineTo(x+w,y+i);
    end; 
  end;

end.
