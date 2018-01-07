program GraphicLW;

uses
  Forms,
  Unit5 in 'codesegments\brightness and contrast\Unit5.pas' {Form5},
  Unit6 in 'codesegments\brightness balancer\Unit6.pas' {bbalancerform},
  draw2d in 'codesegments\common\draw2d.pas',
  DynamicFunctions in 'codesegments\common\DynamicFunctions.pas',
  GLMFunctionality in 'codesegments\common\GLMFunctionality.pas',
  GLWdef in 'codesegments\common\GLWdef.pas',
  GLWFilters in 'codesegments\common\GLWFilters.pas',
  InverseApproximation in 'codesegments\common\InverseApproximation.pas',
  MMXBlend in 'codesegments\common\MMXBlend.pas',
  NotificationNode in 'codesegments\common\NotificationNode.pas',
  MultiFunctionalInputDialog in 'codesegments\common\MultiFunctionalInputDialog.pas',
  treexml in 'codesegments\common\treexml.pas',
  VisualOverlayClass in 'codesegments\common\VisualOverlayClass.pas',
  BufferedCmp in 'codesegments\common\HistogramComponent\BufferedCmp.pas',
  GraphicUtils in 'codesegments\common\HistogramComponent\GraphicUtils.pas',
  HistogramCmp in 'codesegments\common\HistogramComponent\HistogramCmp.pas',
  MathUtils in 'codesegments\common\HistogramComponent\MathUtils.pas',
  pngextra in 'codesegments\common\PNGImage\pngextra.pas',
  pngimage in 'codesegments\common\PNGImage\pngimage.pas',
  pnglang in 'codesegments\common\PNGImage\pnglang.pas',
  zlibpas in 'codesegments\common\PNGImage\zlibpas.pas',
  RadialDistortionCorrection in 'codesegments\distortion correction\RadialDistortionCorrection.pas' {RadialDistortionCorrectionForm},
  Unit2 in 'codesegments\linear registration\Unit2.pas' {Form2},
  ap in 'codesegments\linear registration\algorithm\ap.pas',
  bilinearRegistration in 'codesegments\linear registration\algorithm\bilinearRegistration.pas',
  inv in 'codesegments\linear registration\algorithm\inv.pas',
  lu in 'codesegments\linear registration\algorithm\lu.pas',
  trinverse in 'codesegments\linear registration\algorithm\trinverse.pas',
  GraphicLayerManager in 'codesegments\main form\GraphicLayerManager.pas',
  LayerExplorer in 'codesegments\main form\LayerExplorer.pas' {LayerExplorerForm},
  LayerSelectDlg in 'codesegments\main form\LayerSelectDlg.pas' {LayerSelectDlg},
  Unit1 in 'codesegments\main form\Unit1.pas' {GLWMainForm},
  Unit4 in 'codesegments\pixel binner\Unit4.pas' {Form4},
  Unit3 in 'codesegments\polar to cartesian unwrapper\Unit3.pas' {Form3},
  panotrans in 'codesegments\panotransform\panotrans.pas' {panotransform},
  sphericaltransforms in 'codesegments\panotransform\sphericaltransforms.pas',
  fmath in 'codesegments\common\fmath.pas',
  FileSystemWatch in 'codesegments\common\FileSystemWatch.pas',
  OpenSystem in 'codesegments\common\OpenSystem.pas',
  panorect in 'codesegments\panorectilinear\panorect.pas',
  PanotransformClasses in 'codesegments\panorectilinear\PanotransformClasses.pas',
  trigonometry in 'codesegments\panorectilinear\trigonometry.pas',
  DelphiTwain in 'codesegments\twain\DelphiTwain.pas',
  DelphiTwainUtils in 'codesegments\twain\DelphiTwainUtils.pas',
  Twain in 'codesegments\twain\Twain.pas',
  TwainHelper in 'codesegments\twain\TwainHelper.pas',
  PanoRectSettings in 'codesegments\panorectilinear\PanoRectSettings.pas' {PanoRectSettingsDlg},
  ToolBox in 'codesegments\main form\ToolBox.pas' {ToolBoxForm},
  ToolBoxDesignSpace in 'codesegments\main form\ToolBoxDesignSpace.pas' {ToolBoxDesignFrame},
  GLMVectorEditor in 'codesegments\Vector Data Editor\GLMVectorEditor.pas' {VectorEditorDesign},
  TreeOptionListCmp in 'codesegments\common\TreeOptionsList Component\TreeOptionListCmp.pas',
  RList in 'codesegments\common\ID Resolver List\RList.pas',
  DragDropButtons in 'codesegments\Vector Data Editor\Logical Set DragDrop Manager\DragDropButtons.pas',
  UIntHandler in 'codesegments\Vector Data Editor\User Interaction Handler\UIntHandler.pas',
  VectorObjectTrigonometry in 'codesegments\Vector Data Editor\VectorObjectTrigonometry.pas',
  GLMFilterFileProcessor in 'codesegments\main form\GLMFilterFileProcessor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLWMainForm, GLWMainForm);
  Application.CreateForm(TPanoRectSettingsDlg, PanoRectSettingsDlg);
  Application.CreateForm(TToolBoxForm, ToolBoxForm);
  Application.CreateForm(TToolBoxDesignFrame, ToolBoxDesignFrame);
  Application.CreateForm(TVectorEditorDesign, VectorEditorDesign);
  PanoRectTool := TPanoRectilinearTool.Create(GLWMainForm);
  Application.CreateForm(Tpanotransform, panotransform);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(Tbbalancerform, bbalancerform);
  Application.CreateForm(TRadialDistortionCorrectionForm, RadialDistortionCorrectionForm);
  Application.CreateForm(TLayerExplorerForm, LayerExplorerForm);
  Application.Run;
end.
