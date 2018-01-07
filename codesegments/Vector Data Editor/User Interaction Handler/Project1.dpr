program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UIntHandler in 'UIntHandler.pas',
  RList in '..\..\common\ID Resolver List\RList.pas',
  pngextra in '..\..\common\PNGImage\pngextra.pas',
  pngimage in '..\..\common\PNGImage\pngimage.pas',
  pnglang in '..\..\common\PNGImage\pnglang.pas',
  zlibpas in '..\..\common\PNGImage\zlibpas.pas',
  draw2d in '..\..\common\draw2d.pas',
  DynamicFunctions in '..\..\common\DynamicFunctions.pas',
  FileSystemWatch in '..\..\common\FileSystemWatch.pas',
  fmath in '..\..\common\fmath.pas',
  GLMFunctionality in '..\..\common\GLMFunctionality.pas',
  GLWdef in '..\..\common\GLWdef.pas',
  GLWFilters in '..\..\common\GLWFilters.pas',
  InverseApproximation in '..\..\common\InverseApproximation.pas',
  MMXBlend in '..\..\common\MMXBlend.pas',
  MultiFunctionalInputDialog in '..\..\common\MultiFunctionalInputDialog.pas',
  NotificationNode in '..\..\common\NotificationNode.pas',
  OpenSystem in '..\..\common\OpenSystem.pas',
  treexml in '..\..\common\treexml.pas',
  VisualOverlayClass in '..\..\common\VisualOverlayClass.pas',
  GraphicLayerManager in '..\..\main form\GraphicLayerManager.pas',
  LayerExplorer in '..\..\main form\LayerExplorer.pas' {LayerExplorerForm},
  LayerSelectDlg in '..\..\main form\LayerSelectDlg.pas',
  ap in '..\..\linear registration\algorithm\ap.pas',
  bilinearRegistration in '..\..\linear registration\algorithm\bilinearRegistration.pas',
  inv in '..\..\linear registration\algorithm\inv.pas',
  lu in '..\..\linear registration\algorithm\lu.pas',
  trinverse in '..\..\linear registration\algorithm\trinverse.pas',
  BufferedCmp in '..\..\common\HistogramComponent\BufferedCmp.pas',
  GraphicUtils in '..\..\common\HistogramComponent\GraphicUtils.pas',
  HistogramCmp in '..\..\common\HistogramComponent\HistogramCmp.pas',
  MathUtils in '..\..\common\HistogramComponent\MathUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TLayerExplorerForm, LayerExplorerForm);
  Application.Run;
end.
