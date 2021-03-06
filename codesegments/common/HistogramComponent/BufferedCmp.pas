unit BufferedCmp;

interface
  uses
    WIndows, Messages,Sysutils, Classes, Controls, ExtCtrls, GraphicUtils, Graphics ;

  Type TBufferedCmp = Class(TPaintbox)
    Constructor Create(Sender:TWinControl; w,h: integer); reintroduce;
    Destructor Destroy; override;
  public
    Buf:TBitmap;
    Canv: TCanvas;
    procedure Repaint(Sender:TObject); reintroduce;
  end;
implementation

    Constructor TBufferedCmp.Create(Sender:TWinControl; w,h: integer);
    begin
      inherited Create(Sender);
      self.Width := w;
      self.Height := h;
      Buf := TBitmap.Create;
      Buf.Width := w;
      Buf.Height := h;
      canv := buf.Canvas;
      self.OnPaint := Repaint;
    end;

    Destructor TBufferedCmp.Destroy;
    begin
      Buf.Destroy;
      inherited;
    end;

    procedure TBufferedCmp.Repaint(Sender:TObject);
    begin
      self.Canvas.Draw(0,0,buf);
    end;
end.
