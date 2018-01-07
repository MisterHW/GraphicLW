unit LayerExplorer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  GLWDef, GraphicLayerManager;

type
  TLayerExplorerForm = class(TForm)
  private
    { Private declarations }
  public
    RemoteGLM: TGraphicLayerManager;
  end;

var
  LayerExplorerForm: TLayerExplorerForm;

implementation

{$R *.dfm}

end.
