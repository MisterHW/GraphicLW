unit ToolBoxDesignSpace;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, GraphicLayerManager, StdCtrls;

type
  TToolBoxDesignFrame = class(TForm)
    PageControl1: TPageControl;
    NavigatorBoxes: TTabSheet;
    NavigatorGroupBox: TGroupBox;
  private
    { Private declarations }
  public
    GLM: TGraphicLayerManager;
  end;

var
  ToolBoxDesignFrame: TToolBoxDesignFrame;

implementation

{$R *.dfm}

end.
