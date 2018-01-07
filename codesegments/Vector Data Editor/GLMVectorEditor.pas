unit GLMVectorEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XMLDoc, XMLIntf, ComCtrls,
  GraphicLayerManager, ExtCtrls, Buttons,  ImgList, UIntHandler,
  TreeOptionListCmp,  VisualOverlayClass, treexml, GLWDef, RList, Menus, ToolWin,
  MultiFunctionalInputDialog, DragDropButtons, ActnPopup, GLMFunctionality,
  VectorObjectTrigonometry;

type TObjectSetLogicOperation = (loSetPlus, loSetMinus, loSetAll, loUnsetAll, loInvert);

type
  TVectorEditorDesign = class(TForm)
    GroupBox2   : TGroupBox;
    GroupBox1   : TGroupBox;
    Panel1: TPanel;
    LoadVectorObjectsButton: TSpeedButton;
    SaveVectorObjectsButton: TSpeedButton;
    SaveDialog1 : TSaveDialog;
    OpenDialog1 : TOpenDialog;
    ImageList1  : TImageList;
    TypeImages  : TImageList;
    DeleteVectorObjectsButton: TSpeedButton;
    TreeListPopup: TPopupMenu;
    MenuItemNewPolygon: TMenuItem;
    MenuItemNewPolygonGroup: TMenuItem;
    MenuItemNewPoint: TMenuItem;
    MenuItemNewPointGroup: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemPaste: TMenuItem;
    N1: TMenuItem;
    MenuItemFlush: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemLoadContainer: TMenuItem;
    MenuItemSaveContainer: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    SelectDeselectInvertMenu: TPopupActionBar;
    SelectAll1: TMenuItem;
    DeselectAll1: TMenuItem;
    Invert1: TMenuItem;
    Label1: TLabel;
    Shape1: TShape;
    N2: TMenuItem;
    ApplyDeletion1: TMenuItem;
    ImageList2: TImageList;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    PopupMenu1: TPopupMenu;
    ToolButton8: TToolButton;
    MoveallPointsinproximityofcursor1: TMenuItem;
    ShowObjectHints1: TMenuItem;
    PolygonAssemblyautoselectnextpolygonappend1: TMenuItem;
    PlacePointswithsubpixelprecision1: TMenuItem;
    ToolButton9AllowPopupBox: TToolButton;
    PopupPanel: TPanel;
    ToolBar2: TToolBar;
    DubButton2: TToolButton;
    DubButton1: TToolButton;
    DubButton3: TToolButton;
    ToolBar3: TToolBar;
    DubButton4: TToolButton;
    DubButton5: TToolButton;
    DubButton6: TToolButton;
    HideVectorObjectsofhiddenLayers1: TMenuItem;
    MenuItemMoveSelectionHere: TMenuItem;
    MenuItemInsertDuplicate: TMenuItem;

    procedure SaveVectorObjectsButtonClick(Sender: TObject);
    procedure LoadVectorObjectsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DeleteVectorObjectsButtonClick(Sender: TObject);
    procedure MenuItemNewPolygonGroupClick(Sender: TObject);
    procedure MenuItemNewPointClick(Sender: TObject);
    procedure MenuItemNewPointGroupClick(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemFlushClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemNewPolygonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemLoadContainerClick(Sender: TObject);
    procedure MenuItemSaveContainerClick(Sender: TObject);
    procedure SelectDeselectInvertMenuPopup(Sender: TObject);
    procedure ApplyDeletion1Click(Sender: TObject);
    procedure Invert1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure DeselectAll1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure OverriderightclickandshowQuickBar1Click(Sender: TObject);
    procedure MoveallPointsinproximityofcursor1Click(Sender: TObject);
    procedure ShowObjectHints1Click(Sender: TObject);
    procedure PolygonAssemblyautoselectnextpolygonappend1Click(Sender: TObject);
    procedure PlacePointswithsubpixelprecision1Click(Sender: TObject);
    procedure DubButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DubButton6MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HideVectorObjectsofhiddenLayers1Click(Sender: TObject);
    procedure MenuItemMoveSelectionHereClick(Sender: TObject);
    procedure MenuItemInsertDuplicateClick(Sender: TObject);
  private
    RemoteGLM     : TGraphicLayerManager;
    TreeList      : TTreeOptionList;
    VectorSurface : TVisualOverlaySurface;

    CutPasteContainer : TMetaData;
    MoveContainer     : TMetaData;

    DDButtonField : TDragDropButtonField;
    CurrentHandler: TUserInteractionHandler;

    CreatePointHandler     : TCreatePointHandler;
    MoveObjectsHandler     : TMoveObjectsHandler;
    AssemblePolygonHandler : TAssemblePolygonHandler;
    DeleteObjectsHandler   : TDeleteObjectsHandler;
    MarqueeHandler         : TMarqueeHandler;

    PopupDlg: TForm;

    procedure UpdateAfterLayerChanges(Sender: TObject);
    procedure ResyncAfterVectorDataChanges(Sender: TObject);
    function CursorOverModifyableObject(Button: TMouseButton;
                  Shift: TShiftState; MousePos: TPoint): Boolean;
    procedure PaintSurface(Destination: TBitmap;
                AbsoluteOrigin: TPoint; CurrentScaling: single);
    function CreateInstanceByClassName(ClassName: String; node: IXMLNode): TObject;

    procedure TreeListMouseDown(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeListMouseUp(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeListMouseMove(Sender: TObject;
                Shift: TShiftState; X, Y: Integer);

    procedure VectorSurfaceMouseDown(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure VectorSurfaceMouseUp(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure VectorSurfaceMouseMove(Sender: TObject;
                Shift: TShiftState; X, Y: Integer);

    procedure NodeFlagsChanged(Sender:TObject);

    procedure LoadContainerObjectFromFile(con: TMetaData);
    procedure SaveContainerObjectToFile  (con: TMetaData);

    procedure SetOperationPlus(Source, Dest: longint);
    procedure SetOperationMinus(Source, Dest: longint);

    procedure PerformSetOperation(Source, Dest: longint; Operation: TObjectSetLogicOperation);

    function SurfaceHandlesEvents(Button: TMouseButton;
                Shift: TShiftState; MousePos: TPoint): Boolean;

    procedure HandlerDisconnects(Sender: TObject);

    procedure ExecuteToolBoxPopup(Position : TPoint);
    procedure SyncButtonsToPopup;
  public
    procedure RegisterToGLM(LayerManager: TGraphicLayerManager);
    procedure SetHoverEventsToStatusBarUpdate(Event: TMouseMoveEvent);
    procedure SynchronizeTree(Sender: TObject);
  end;

var
  VectorEditorDesign: TVectorEditorDesign;



implementation

{$R *.dfm}



procedure TVectorEditorDesign.ApplyDeletion1Click(Sender: TObject);
var
  idx: Integer;
begin
  if assigned(RemoteGLM) then
  begin
    // delete flagged
    RemoteGLM.GlobalMetaContent.DeleteFlaggedObjects;
    for idx := 0 to Length(RemoteGLM.Layers) - 1 do
      RemoteGLM.Layers[idx].meta.DeleteFlaggedObjects;

    RemoteGLM.RecreateMetaContentLists(self);
    Self.SynchronizeTree(self);
    RemoteGLM.ClientPaint(self);  
  end;
end;



function TVectorEditorDesign.CreateInstanceByClassName(ClassName: String; node: IXMLNode): TObject;
begin
  if      ClassName= XMLTAG_SINGLEPOINT        then result := TSinglePoint.Create
  else if ClassName= XMLTAG_SINGLEPOLYGON      then result := TSinglePolygon.Create
  else if ClassName= XMLTAG_POINTGROUP         then result := TPointGroup.Create
  else if ClassName= XMLTAG_POLYGONGROUP       then result := TPolygonGroup.Create
  else result := nil;

  if assigned(result) then
    if (result is TXMLExportableObject) then
       (result as TXMLExportableObject).LoadFromXMLStructure(node);
end;



procedure TVectorEditorDesign.UpdateAfterLayerChanges(Sender: TObject);
begin
  //
end;



procedure TVectorEditorDesign.ResyncAfterVectorDataChanges(Sender: TObject);
begin
  SynchronizeTree(self);
end;



procedure TVectorEditorDesign.RegisterToGLM(LayerManager: TGraphicLayerManager);
begin
  RemoteGLM := LayerManager;

  if not assigned(RemoteGLM) then
  begin
    Application.MessageBox('Error : TGraphicLayerManager not assigned.',
                           'TVectorEditorDesign.RegisterToGLM',
                           MB_ICONERROR or MB_OK);
    exit;
  end;

  RemoteGLM.OnMetaContentRefresh.InsertEvent(ResyncAfterVectorDataChanges);
  RemoteGLM.OnLayerChanges.InsertEvent(self.UpdateAfterLayerChanges);
  RemoteGLM.InsertVisualOverlayObject(VectorSurface);

  RemoteGLM.OnMetaContentChange.InsertEvent(CreatePointHandler.VectorObjectsChanged);
  RemoteGLM.OnMetaContentChange.InsertEvent(MoveObjectsHandler.VectorObjectsChanged);
  RemoteGLM.OnMetaContentChange.InsertEvent(AssemblePolygonHandler.VectorObjectsChanged);
  RemoteGLM.OnMetaContentChange.InsertEvent(DeleteObjectsHandler.VectorObjectsChanged);
  RemoteGLM.OnMetaContentChange.InsertEvent(MarqueeHandler.VectorObjectsChanged);

  RemoteGLM.IncludeAllLayersInMetaContentLists := not HideVectorObjectsofhiddenLayers1.Checked;
end;



procedure TVectorEditorDesign.SaveVectorObjectsButtonClick(
  Sender: TObject);
begin
  SaveContainerObjectToFile(RemoteGLM.GlobalMetaContent);
end;



procedure TVectorEditorDesign.SaveContainerObjectToFile(con: TMetaData);
begin
  SaveDialog1.DefaultExt := '.xml';
  SaveDialog1.Filter     := '(*.xml)  Extensible Markup Language File|*.xml|'+
                            '(*.txt)  Textfile|*.txt|'+
                            '(*.*)  All Files|*.*';
  SaveDialog1.Options    := [ofForceShowHidden, ofPathMustExist];

  Application.ProcessMessages; // avoid blanking of the button glyph

  if SaveDialog1.Execute then
     con.SaveToFile(SaveDialog1.FileName);

end;



procedure TVectorEditorDesign.DeleteVectorObjectsButtonClick(Sender: TObject);
begin
  if assigned(RemoteGLM) then
  begin
    RemoteGLM.GlobalMetaContent.DestroyContent(self);

    RemoteGLM.RecreateMetaContentLists(self);
    SynchronizeTree(self);
    RemoteGLM.ClientPaint(self);
  end;
end;



procedure TVectorEditorDesign.LoadVectorObjectsButtonClick(
  Sender: TObject);
begin
  LoadContainerObjectFromFile(RemoteGLM.GlobalMetaContent);
end;



procedure TVectorEditorDesign.LoadContainerObjectFromFile(con: TMetaData);
var
  xmld      : TXMLDocument;
  mainnodes : longint;
  meta      : TMetaData;
  inode     : IXMLNode;
  L1Point, LPoints, L1Poly, LPolys: TResolverList;
  itemidx, subidx : longint;
  OnePoly   : TSinglePolygon;
  ListIdx: longint;

begin
  OpenDialog1.DefaultExt := '.xml';
  OpenDialog1.Filter     := '(*.xml)  Extensible Markup Language File|*.xml|'+
                            '(*.txt)  Textfile|*.txt|'+
                            '(*.*)  All Files|*.*';
  OpenDialog1.Options    := [ofForceShowHidden, ofFileMustExist];

  Application.ProcessMessages; // avoid blanking of the button glyph

  if OpenDialog1.Execute then
  begin
    xmld := TXMLDocument.Create(Self);
    xmld.LoadFromFile(OpenDialog1.FileName);
    xmld.Active := true;

    meta := nil;

    for mainnodes := xmld.ChildNodes.Count - 1 downto 0  do
    begin

      if (xmld.ChildNodes[mainnodes].NodeName = XMLTAG_CONTAINER) then
      begin
        if not assigned(meta) then
        begin
           // create one global dummy container
           meta := TMetaData.Create;
           meta.OnRequestNewInstance := Self.CreateInstanceByClassName;


           // load container, drop xml node
           inode := xmld.ChildNodes[mainnodes];
           meta.LoadFromXMLStructure(inode);
           xmld.DOMDocument.RemoveChild(xmld.ChildNodes[mainnodes].DOMNode);

           // translate IDs to UIDs of current session
           // IDs from import are stored in "IDAsLoaded", all references
           // made to any object have to be resolved now

           L1Point := TResolverList.Create;
           LPoints := TResolverList.Create;
           L1Poly  := TResolverList.Create;
           LPolys  := TResolverList.Create;

           // create a lot of look-up lists
           RecreateMetaListByClass( meta , L1Point , TSinglePoint   , true , true );
           RecreateMetaListByClass( meta , LPoints , TPointGroup    , true , true );
           RecreateMetaListByClass( meta , L1Poly  , TSinglePolygon , true , true );
           RecreateMetaListByClass( meta , LPolys  , TPolygonGroup  , true , true );

           // resolve references within single polygon objects
           for itemidx := 0 to L1Poly.Count - 1 do // for each polygon do
           begin
             OnePoly := (L1Poly.Items[itemidx].Obj as TSinglePolygon);
             for subidx := 0 to OnePoly.Count -1 do // for each point reference do
             begin
               // resolve GroupID pointgroup reference
               ListIdx := LPoints.IndexByID(OnePoly.Points[subidx].GroupUID);
               if ListIdx <> INDEX_NOT_RESOLVED then
                 OnePoly.Points[subidx].GroupUID :=
                                (LPoints.Items[ListIdx].Obj as TPointGroup).UID
               else
               begin
                 OnePoly.Points[subidx].GroupUID := -1; // ID could not be resolved
                 MessagebOx(0,'GroupID could not be resolved.','XML Parsing Error',0);
               end;

               ListIdx := L1Point.IndexByID(OnePoly.Points[subidx].PointUID);
               if ListIdx <> INDEX_NOT_RESOLVED then
                 OnePoly.Points[subidx].PointUID :=
                                (L1Point.Items[ListIdx].Obj as TSinglePoint).UID
               else
               begin
                 OnePoly.Points[subidx].PointUID := -1; // ID could not be resolved
                 MessagebOx(0,'PointID could not be resolved.','XML Parsing Error',0);
               end;

             end;
           end;

           // resolve other objects with references
           // ...

           // done, free lists

           L1Point.Destroy;
           LPoints.Destroy;
           L1Poly.Destroy;
           LPolys.Destroy;

           // append to main container
           con.Assimilate(meta);
           con.Text := meta.Text;

          break;
        end; // meta not assigned before
      end; // if is MetaData

    end; // for

    xmld.Active := false; // this destroys the xml document object?
    if assigned(meta) then meta.Destroy;

    if assigned(RemoteGLM) then
    begin
      RemoteGLM.RecreateMetaContentLists(self); // re-creates lists, triggers OnMetaContentChange / OnMetaContentRefresh Event
      Self.SynchronizeTree(self);
      RemoteGLM.ClientPaint(self);
    end;

  end; // if Execute OpenDialog

end;



procedure TVectorEditorDesign.FormCreate(Sender: TObject);
var
  btn : TSpeedButton;
begin
  CutPasteContainer := TMetaData.Create;

  TreeList := TTreeOptionList.Create(Self.GroupBox2);

  PopupDlg := nil;

  with TreeList do
  begin
      Parent            := GroupBox2;
      Align             := alClient;
      PropertyIconList  := ImageList1;
      Tree.Images       := TypeImages;
      OnNodeFlagsChanged:= NodeFlagsChanged;
      OnTreeMouseDown   := TreeListMouseDown;
      OnTreeMouseUp     := TreeListMouseUp;
      OnTreeMouseMove   := TreeListMouseMove;
  end;

  VectorSurface := TVisualOverlaySurface.Create(Application.MainForm.ClientRect);
  with VectorSurface do
  begin
      Anchors     := [akLeft, akRight, akTop, akBottom];
      OnCursorInsideStateRequest := CursorOverModifyableObject;
      OnPaintEx   := PaintSurface;
      Visible     := true;
      OnMouseDown := VectorSurfaceMouseDown;
      OnMouseMove := VectorSurfaceMouseMove;
      OnMouseUp   := VectorSurfaceMouseUp;
      OnCursorInsideStateRequest := SurfaceHandlesEvents;
  end;

  // assign Popup Menu Item Flags
  //
  MenuItemNewPolygon.Tag       := moNewPoly;
  MenuItemNewPolygonGroup.Tag  := moNewPolyGroup;
  MenuItemNewPoint.Tag         := moNewPoint;
  MenuItemNewPointGroup.Tag    := moNewPointGroup;
  MenuItemCut.Tag              := moCut;
  MenuItemPaste.Tag            := moPaste;
  MenuItemFlush.Tag            := moFlush;
  MenuItemDelete.Tag           := moDelete;
  MenuItemEdit.Tag             := moEdit;
  MenuItemLoadContainer.Tag    := moLoadContainer;
  MenuItemSaveContainer.Tag    := moSaveContainer;
  MenuItemInsertDuplicate.Tag  := moInsertMove;
  MenuItemMoveSelectionHere.Tag := moInsertDuplicate;


  DDButtonField := TDragDropButtonField.Create(Panel1);
  DDButtonField.Parent    := Panel1;
  DDButtonField.ActionBar := SelectDeselectInvertMenu;

  btn := DDButtonField.NewButton(OBJECT_FLAG_VISIBLE);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  ImageList1.Draw(btn.Glyph.Canvas, 0, 0, 0);

  btn := DDButtonField.NewButton(OBJECT_FLAG_LOCKED);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  ImageList1.Draw(btn.Glyph.Canvas,0,0, 1);

  btn := DDButtonField.NewButton(OBJECT_FLAG_TOBEDELETED);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  ImageList1.Draw(btn.Glyph.Canvas,0,0, 2);

  btn := DDButtonField.NewButton(OBJECT_FLAG_SELECTED);
  btn.Glyph.Width  := 16;
  btn.Glyph.Height := 16;
  ImageList1.Draw(btn.Glyph.Canvas,0,0, 3);

  DDButtonField.Left   := Panel1.Width - DDButtonField.Width ;
  DDButtonField.Top       := 0;
  DDButtonField.Anchors   := [akTop, akRight];

  DDButtonField.PlusEvent  := SetOperationPlus;
  DDButtonField.MinusEvent := SetOperationMinus;


  // Handlers
  MoveContainer := TMetaData.Create;

  CreatePointHandler := TCreatePointHandler.Create;
  CreatePointHandler.OnDisconnect := HandlerDisconnects;

  MoveObjectsHandler := TMoveObjectsHandler.Create;
  MoveObjectsHandler.OnDisconnect := HandlerDisconnects;
  MoveObjectsHandler.OnRightclickOneObject := MenuItemEditClick;

  AssemblePolygonHandler := TAssemblePolygonHandler.Create;
  AssemblePolygonHandler.OnDisconnect := HandlerDisconnects;

  DeleteObjectsHandler := TDeleteObjectsHandler.Create;
  DeleteObjectsHandler.OnDisconnect := HandlerDisconnects;

  MarqueeHandler := TMarqueeHandler.Create;
  MarqueeHandler.OnDisconnect := HandlerDisconnects;
end;



procedure TVectorEditorDesign.FormDestroy(Sender: TObject);
begin
  CreatePointHandler.Destroy;
  MoveObjectsHandler.Destroy;
  AssemblePolygonHandler.Destroy;
  DeleteObjectsHandler.Destroy;
  MarqueeHandler.Destroy;

  CutPasteContainer.Destroy;
  DDButtonField.Destroy;

  if assigned(PopupDlg) then
    PopupDlg.Destroy;
end;



function TVectorEditorDesign.CursorOverModifyableObject(
                Button: TMouseButton;
                Shift: TShiftState;
                MousePos: TPoint): Boolean;
begin
  result := false;
end;



procedure TVectorEditorDesign.PaintSurface(
                Destination    : TBitmap;
                AbsoluteOrigin : TPoint;
                CurrentScaling : single);
var
  groupidx, polyidx, itemidx : integer;
  coords: TPoint;
  firstpt, pt, nextpt: TSinglePoint;
  g: TVectorObjectGroup;
  groupflags: longint;
  pointlistidx, nextidx: longint;
  startcoords, endcoords : TPoint;
  ptradius: integer;
begin
  if not assigned(RemoteGLM) then exit;

  // render polygons, lines and points according to their visibility / lock status
  //

  Destination.Canvas.Pen.Color   := clWhite;
  Destination.Canvas.Pen.Style   := psDash;
  Destination.Canvas.Pen.Width   := 1;

  // render polygons (wireframe for now)
  //
  for groupidx := 0 to RemoteGLM.PolygonGroupList.Count - 1 do
  begin
    g := RemoteGLM.PolygonGroupList.Items[groupidx].obj as TVectorObjectGroup;
    groupflags := g.Flags;
    if groupflags < 0 then groupflags := 0;

   for polyidx := 0 to g.Count - 1 do
   begin

      with (g.Items[polyidx] as TSinglePolygon) do
      begin

        if (Count > 1) and TestObjectFlag(Flags and GroupFlags, OBJECT_FLAG_VISIBLE) then
        begin

            pointlistidx := RemoteGLM.PointList.IndexByID(Points[0].PointUID);

            if pointlistidx <> INDEX_NOT_RESOLVED then
              pt := RemoteGLM.PointList.Items[pointlistidx].obj as TSinglePoint
            else // if not INDEX_NOT_RESOLVED
              pt := nil;

            firstpt := pt;

            if TestObjectFlag(Flags or GroupFlags, OBJECT_FLAG_SELECTED) then
            begin
               Destination.Canvas.Pen.Width   := 3;
               Destination.Canvas.Pen.Color := clLime;
            end
            else
            begin
              Destination.Canvas.Pen.Width   := 1;
              Destination.Canvas.Pen.Color := clWhite;
            end;

            for itemidx := 1 to Count - 1 do
            begin
              nextidx := RemoteGLM.PointList.IndexByID(Points[itemidx].PointUID);

              if nextidx <> INDEX_NOT_RESOLVED then
                nextpt := RemoteGLM.PointList.Items[nextidx].obj as TSinglePoint
              else
                nextpt := nil;

              if assigned(pt) and assigned(nextpt) then
              begin
                StartCoords := Point(RemoteGLM.GlobalCoordToScreen(FloatPoint(pt.X, pt.Y)));
                EndCoords   := Point(RemoteGLM.GlobalCoordToScreen(FloatPoint(nextpt.X, nextpt.Y)));
                Destination.Canvas.MoveTo(StartCoords.X , StartCoords.Y);
                Destination.Canvas.LineTo(EndCoords.X   , EndCoords.Y);
              end;

              pt := nextpt;
            end; // for points in polygon

            if assigned(pt) and assigned(firstpt) then
            begin
                StartCoords := Point(RemoteGLM.GlobalCoordToScreen(FloatPoint(pt.X, pt.Y)));
                EndCoords   := Point(RemoteGLM.GlobalCoordToScreen(FloatPoint(firstpt.X, firstpt.Y)));
                Destination.Canvas.MoveTo(StartCoords.X , StartCoords.Y);
                Destination.Canvas.LineTo(EndCoords.X   , EndCoords.Y);
            end;

        end; // if Count > 1
      end; // with

    end; // for polygons

  end; // for groups


  // render Points
  //
  Destination.Canvas.Pen.Style := psSolid;
  Destination.Canvas.Font.Size := 7;

  for groupidx := 0 to RemoteGLM.PointGroupList.Count - 1 do
  begin
    g := RemoteGLM.PointGroupList.Items[groupidx].obj as TVectorObjectGroup;
    groupflags := g.Flags;
    if groupflags < 0 then groupflags := 0;

    if TestObjectFlag(g.Flags, OBJECT_FLAG_VISIBLE) then
    begin

      for itemidx := 0 to g.Count - 1 do
      begin
        pt     := g.Items[itemidx] as TSinglePoint;

        if TestObjectFlag(pt.Flags,OBJECT_FLAG_VISIBLE) then
        begin

          coords := Point(RemoteGLM.GlobalCoordToScreen(FloatPoint(pt.X, pt.Y)));

          if TestObjectFlag(pt.Flags or groupflags, OBJECT_FLAG_SELECTED) then
          begin
            Destination.Canvas.Pen.Width   := 2;
            Destination.Canvas.Pen.Color   := clLime;
            ptradius := 5;
          end
          else
          begin
            Destination.Canvas.Pen.Width   := 1;
            Destination.Canvas.Pen.Color   := clWhite;
            ptradius := 4;
          end;

          Destination.Canvas.Brush.Color := clblack;
          Destination.Canvas.Ellipse( Coords.X-ptradius, Coords.Y-ptradius,
                                      Coords.X+ptradius+1, Coords.Y+ptradius+1 );

          Destination.Canvas.Pen.Width   := 1;

          Destination.Canvas.MoveTo( Coords.X-ptradius-3 , Coords.Y );
          Destination.Canvas.LineTo( Coords.X+ptradius+4 , Coords.Y );
          Destination.Canvas.MoveTo( Coords.X , Coords.Y-ptradius-3 );
          Destination.Canvas.LineTo( Coords.X , Coords.Y+ptradius+4 );

          if ShowObjectHints1.Checked then
          begin
            Destination.Canvas.Font.Color  := clblack;
            if TestObjectFlag(pt.Flags or groupflags, OBJECT_FLAG_SELECTED) then
              Destination.Canvas.Brush.Color := cllime
            else
              Destination.Canvas.Brush.Color := clsilver;
              
            Destination.Canvas.TextOut( Coords.X,
                                      Coords.Y-8-Destination.Canvas.TextHeight('W'),
                                      '( ' + FloatToStr(round(pt.X  *100)/100)+' | '+
                                             FloatToStr(round(pt.Y * 100)/100)+' )');
          end;
        end; // if point visible
      end; // for

    end; // if group visible
  end; // for

  Destination.Canvas.Pen.Width := 1;
  Destination.Canvas.Pen.Color := clWhite;

  if assigned(CurrentHandler) then
    if CurrentHandler.RequiresHandlerDraw then
      CurrentHandler.HandlerPaintEvent(Destination, AbsoluteOrigin,CurrentScaling);
end;



procedure TVectorEditorDesign.SelectDeselectInvertMenuPopup(Sender: TObject);
begin
  ApplyDeletion1.Visible := (self.DDButtonField.Source = OBJECT_FLAG_TOBEDELETED);

end;



procedure TVectorEditorDesign.SetHoverEventsToStatusBarUpdate(Event: TMouseMoveEvent);
begin
  // assign show-hint-on-hover event

  self.SaveVectorObjectsButton.OnMouseMove   := Event;
  self.LoadVectorObjectsButton.OnMouseMove   := Event;
  self.DeleteVectorObjectsButton.OnMouseMove := Event;

  Self.ToolButton1.OnMouseMove := Event;
  Self.ToolButton2.OnMouseMove := Event;
  Self.ToolButton3.OnMouseMove := Event;
  Self.ToolButton4.OnMouseMove := Event;
  Self.ToolButton5.OnMouseMove := Event;
  Self.ToolButton6.OnMouseMove := Event;
  Self.ToolButton7.OnMouseMove := Event;
  Self.ToolButton9AllowPopupBox.OnMouseMove := Event;
end;



procedure TVectorEditorDesign.SynchronizeTree(Sender: TObject);
var
  LView: TResolverList;
  idx : longint;
  listidx: longint;
  tn: TTreeNode;
  md: TMetaData;
  ContainerLayerIdx : longint;
  IdxStr : string;

  procedure InitializeNodeStates(Node: TTreeNode; recurse: Boolean = true);
  var idx: integer;
  begin
    Node.ImageIndex := -1; // default to "no icon"
    if TObject(Node.Data) is TMetaData      then Node.ImageIndex := 0;
    if TObject(Node.Data) is TSinglePoint   then Node.ImageIndex := 1;
    if TObject(Node.Data) is TPointGroup    then Node.ImageIndex := 2;
    if TObject(Node.Data) is TPolygonGroup  then Node.ImageIndex := 3;
    if TObject(Node.Data) is TSinglePolygon then Node.ImageIndex := 4;
    Node.SelectedIndex := Node.ImageIndex;

    //if (Node.ImageIndex <> -1) and (Node.StateIndex  = -1) then // node type is known
    //  Node.StateIndex    := OBJECT_FLAG_VISIBLE or OBJECT_FLAG_LOCKED;

    for idx := 0 to Node.Count-1 do InitializeNodeStates(Node.Item[idx],recurse);
  end;

begin
  if not assigned(RemoteGLM) then exit;

  // list all containers
  LView := TResolverList.Create;
  LView.Append(          RemoteGLM.GlobalMetaContent,
                Cardinal(RemoteGLM.GlobalMetaContent));

  for idx := 0 to Length(RemoteGLM.Layers) - 1 do
  begin
    LView.Append(          RemoteGLM.Layers[idx].meta,
                  Cardinal(RemoteGLM.Layers[idx].meta));
  end;
  LView.SortByID;

  // drop existing containers from LView list, delete vanished containers from treeview

  for idx := TreeList.Tree.Items.Count-1 downto 0 do
  begin
    if not assigned(TreeList.Tree.Items.Item[idx].Parent) then
    begin
      ListIdx := LView.IndexByID(Cardinal(TreeList.Tree.Items[idx].Data)); // changed from items.item[idx] to items[idx], any improvements?
      if ListIdx = INDEX_NOT_RESOLVED then
        TreeList.Tree.Items[idx].Delete// .Delete(TreeList.Tree.Items.Item[idx])
      else
        LView.Items[ListIdx].Delete := TRUE; // mark for deletion
    end;
  end;

  LView.ApplyDeletion;


  // add missing items (the ones that remain after deletion) to treeview
  for idx := 0 to LView.Count - 1 do
  begin
    tn := TreeList.Tree.Items.Add( nil, (LView.Items[idx].Obj as TMetaData).Text);

    tn.Data := LView.Items[idx].Obj;

    if LView.Items[idx].Obj = RemoteGLM.GlobalMetaContent  then
      tn.Text := tn.Text + ' (main)';

    InitializeNodeStates(tn, false);
  end;

  LView.Destroy;

  for idx := TreeList.Tree.Items.Count - 1 downto 0 do
  begin
    //if not assigned(TreeList.Tree.Items[idx].Parent) then // just to be sure...
    //begin
      if Assigned(TreeList.Tree.Items[idx].Data) then // object is assigned
      begin
        if (TObject(TreeList.Tree.Items[idx].Data) is TXMLExportableObject) then
        begin
          (TObject(TreeList.Tree.Items[idx].Data) as TXMLExportableObject).SynchronizeToNode(TreeList.Tree.Items[idx]);
        end; // if is TXMLExportableObject
    // end; // if assigned()
    end; // if no parent

    InitializeNodeStates(TreeList.Tree.Items[idx], false);

    if Assigned(TreeList.Tree.Items[idx].Data) then
    begin
      if TObject(TreeList.Tree.Items[idx].Data) is TMetaData then
      begin
        md := TObject(TreeList.Tree.Items[idx].Data) as TMetaData;

        ContainerLayerIdx := RemoteGLM.LayerByID(md.LayerID);
        
        if ContainerLayerIdx <> -1 then
           IdxStr := IntToStr(ContainerLayerIdx) + '.'
        else
           IdxStr := '';

        if md.Text = '' then
          TreeList.Tree.Items[idx].Text := IdxStr+ md.TagName + md.Suffix
        else
          TreeList.Tree.Items[idx].Text := IdxStr+ md.Text + md.Suffix;
      end;
    end;

  end; // for

  TreeList.Tree.Repaint; // fix rendering errors...
end;



procedure TVectorEditorDesign.ToolButton1Click(Sender: TObject);
var
  vg: TVectorObjectGroup;
  MBResult : longint;
begin
  if (Sender is TToolButton) then
    if not (Sender as TToolButton).Down then
    begin
      CurrentHandler := nil;
      exit;
    end;

  CurrentHandler := CreatePointHandler;
  CreatePointHandler.MovePointsInProximity := MoveallPointsinproximityofcursor1.Checked;

  vg := nil;

  if TreeList.Tree.SelectionCount > 0 then
    if assigned(TreeList.Tree.Selected.Data) then
      if TObject(TreeList.Tree.Selected.Data) is TPointGroup then
        vg := TObject(TreeList.Tree.Selected.Data) as TPointGroup;

  // add a PointGroup Object if the user forgot to create or select one
  if not assigned(vg) then
  begin
    MBResult := Application.MessageBox(
                           'No PointGroup object is currently selected.'#13#10#13#10+
                           'Confirm this dialog to initialize a New Group or'#13#10+
                           'Decline it, then Select an Existing Group by clicking it.'#13#10#13#10+
                           'Create a New PointGroup Object?','PointGroup needed. Create New?',
                           MB_YESNO or MB_ICONINFORMATION);

    if MBResult = IDYES then
    begin
      vg := TPointGroup.Create;
      RemoteGLM.GlobalMetaContent.Add(vg);
      RemoteGLM.RecreateMetaContentLists(self);
    end;

  end;

  CreatePointHandler.Connect( RemoteGLM,
                              VectorSurface,
                              vg,
                              nil,
                              nil,
                              RemoteGLM.ClientPaint);

  CreatePointHandler.AllowSubpixelCoordinates :=
        PlacePointswithsubpixelprecision1.Checked;
end;



procedure TVectorEditorDesign.ToolButton3Click(Sender: TObject);
begin

  if (Sender is TToolButton) then
    if not (Sender as TToolButton).Down then
    begin
      CurrentHandler := nil;
      exit;
    end;

  CurrentHandler := MoveObjectsHandler;

  CurrentHandler.Connect( RemoteGLM,
                              VectorSurface,
                              nil,
                              nil,
                              MoveContainer,
                              RemoteGLM.ClientPaint);

  CurrentHandler.AllowSubpixelCoordinates :=
        PlacePointswithsubpixelprecision1.Checked;
end;



procedure TVectorEditorDesign.ToolButton2Click(Sender: TObject);
var
  OnePoly: TSinglePolygon;
  DummyGroup: TPolygonGroup;
  MBResult : longint;
begin

  if (Sender is TToolButton) then
    if not (Sender as TToolButton).Down then
    begin
      CurrentHandler := nil;
      exit;
    end;

  if RemoteGLM.PointList.Count = 0 then
  begin
    ToolButton2.Down := false;
    Application.MessageBox('No Points available.'#13#10#13#10+
                           'You need Points within a PointGroup to construct a Polygon.',
                           'Points needed. Not ready to create Polygons.', MB_OK);
    exit;
  end;

  CurrentHandler := AssemblePolygonHandler;

  OnePoly := nil;
  DummyGroup := nil;

  if TreeList.Tree.SelectionCount > 0 then
  begin
    if assigned(TreeList.Tree.Selected.Data) then
    begin
      if TObject(TreeList.Tree.Selected.Data) is TSinglePolygon then
      begin
        OnePoly := TObject(TreeList.Tree.Selected.Data) as TSinglePolygon;

        if assigned(TreeList.Tree.Selected.Parent) then
          if assigned(TreeList.Tree.Selected.Parent.Data) then
            if (TObject(TreeList.Tree.Selected.Parent.Data) is TPolygonGroup) then
              DummyGroup := TPolygonGroup(TreeList.Tree.Selected.Parent.Data);
      end;

      // permit addition to existing polygongroups
      if (TObject(TreeList.Tree.Selected.Data) is TPolygonGroup) then
            DummyGroup := TPolygonGroup(TreeList.Tree.Selected.Data);
    end;
  end;

  // add a PointGroup Object if the user forgot to create or select one
  if not assigned(OnePoly) then
  begin
    MBResult := 0;

    if not assigned(OnePoly) and not assigned(DummyGroup) then
    MBResult := Application.MessageBox(
                           'No Polygon object is currently selected.'#13#10#13#10+
                           'Confirm this dialog to initialize a New PolygonGroup and Polygon or'#13#10+
                           'Decline it, then Select an Existing Polygon by clicking it.'#13#10#13#10+
                           'Create New Group + Polygon Objects?','Polygon and Group needed. Create New?',
                           MB_YESNO or MB_ICONINFORMATION);


    if not assigned(OnePoly) and assigned(DummyGroup) then
    MBResult := Application.MessageBox(
                           'No Polygon object is currently selected.'#13#10#13#10+
                           'Confirm this dialog to initialize and Polygon only or'#13#10+
                           'Decline it, then Select an Existing Polygon by clicking it.'#13#10#13#10+
                           'Create one New Polygon Object?','Polygon needed. Create New?',
                           MB_YESNO or MB_ICONINFORMATION);

    if MBResult = IDYES then
    begin
      if not assigned(DummyGroup) then
        DummyGroup := TPolygonGroup.Create;
      RemoteGLM.GlobalMetaContent.Add(DummyGroup);

      OnePoly := TSinglePolygon.Create;
      DummyGroup.Add(OnePoly);

      RemoteGLM.RecreateMetaContentLists(self);
    end;

  end;

  AssemblePolygonHandler.Connect( RemoteGLM,
                              VectorSurface,
                              DummyGroup,
                              OnePoly,
                              nil,
                              RemoteGLM.ClientPaint);
                              
  AssemblePolygonHandler.AutoSelectNext :=
        PolygonAssemblyautoselectnextpolygonappend1.Checked;

  AssemblePolygonHandler.AllowSubpixelCoordinates :=
        PlacePointswithsubpixelprecision1.Checked;
end;



procedure TVectorEditorDesign.ToolButton4Click(Sender: TObject);
begin

  if (Sender is TToolButton) then
    if not (Sender as TToolButton).Down then
    begin
      CurrentHandler := nil;
      exit;
    end;

  CurrentHandler := DeleteObjectsHandler;

  CurrentHandler.Connect( RemoteGLM,
                          VectorSurface,
                          nil,
                          nil,
                          MoveContainer,
                          RemoteGLM.ClientPaint);

  CurrentHandler.AllowSubpixelCoordinates :=
        PlacePointswithsubpixelprecision1.Checked;
end;



procedure TVectorEditorDesign.ToolButton5Click(Sender: TObject);
begin

  if (Sender is TToolButton) then
    if not (Sender as TToolButton).Down then
    begin
      CurrentHandler := nil;
      exit;
    end;

  CurrentHandler := MarqueeHandler;

  CurrentHandler.Connect( RemoteGLM,
                          VectorSurface,
                          nil,
                          nil,
                          MoveContainer,
                          RemoteGLM.ClientPaint);

  CurrentHandler.AllowSubpixelCoordinates :=
        PlacePointswithsubpixelprecision1.Checked;
end;



procedure TVectorEditorDesign.ToolButton6Click(Sender: TObject);
begin
  ToolButton6.Down := false;
  CurrentHandler := nil;
end;



procedure TVectorEditorDesign.ToolButton7Click(Sender: TObject);
var
  p: TPoint;
begin
  p.X := Toolbutton7.BoundsRect.Left;
  p.Y := Toolbutton7.BoundsRect.Bottom;
  p := ToolBar1.ClientToScreen(p);
  self.PopupMenu1.Popup(p.x, p.y);
end;



procedure TVectorEditorDesign.VectorSurfaceMouseDown(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SelectedPoints: TResolverList;
  Idx: longint;
  query : PQueryIndexList;
  OnePoly: TSinglePolygon;
  OneGroup: TPolygonGroup;
  GroupIdx : longint;
  var DummyContainer : TMetaData;
begin

  if not assigned(RemoteGLM) then EXIT;

  if assigned(PopupDlg) then
    if PopupDlg.Visible then EXIT;


  if assigned(CurrentHandler) then
  begin


    if CurrentHandler is TMoveObjectsHandler then
    begin
      // grab selected objects or look at what can be
      // grabbed within proximity of the cursor
      //
      SelectedPoints := TResolverList.Create;
      RemoteGLM.EnumAllSelectedVectorObjects(SelectedPoints);

      // copy these objects
      SetLength(MoveContainer.Entries, SelectedPoints.Count);
      for idx := 0 to SelectedPoints.Count - 1 do
      begin
        MoveContainer.Entries[idx].UID := SelectedPoints.Items[idx].ID; // this is the local ID for ID in queries but we might as well use the object UID here...
        MoveContainer.Entries[idx].obj := SelectedPoints.Items[idx].Obj;
      end;

      // IF NO OBJECTS ARE SELECTED ...
      // get some in proximity of cursor
      //
      if SelectedPoints.Count = 0 then
        FindObjectsInProximityOfCursor(RemoteGLM, Point(X,Y), MoveContainer);

      SelectedPoints.Destroy;
    end; // if CurrentHandler is TMoveObjectsHandler;


    if (CurrentHandler is TAssemblePolygonHandler) then // right-click to change current polygon
    begin

      if (ssRight in Shift) then
      begin
        OnePoly  := nil;
        OneGroup := nil;
        MoveContainer.Clear;

        FindObjectsInProximityOfCursor(RemoteGLM, Point(X,Y), MoveContainer);

        query := MoveContainer.QueryEntries(TSinglePolygon);

        if length(query^.UIDs) > 1 then
        begin
          DummyContainer := TMetaData.Create;
          for idx := 0 to length(query^.UIDs) - 1 do
             DummyContainer.Add(query^.UnsafeObj[idx]);
          OnePoly := HaveUserSelectOneVectorObject(DummyContainer) as TSinglePolygon;
          DummyContainer.Destroy;
        end
        else if length(query^.UIDs) = 1 then
        begin
          OnePoly := query^.UnsafeObj[0] as TSinglePolygon;
        end;

        if assigned(OnePoly) then
        begin
            GroupIdx := RemoteGLM.PolygonGroupList.IndexByID(
                         RemoteGLM.ResolveVectorObjectGroupID(OnePoly));
            if GroupIdx <> INDEX_NOT_RESOLVED then
              OneGroup := RemoteGLM.PolygonGroupList.Items[GroupIdx].obj as TPolygonGroup;

          (CurrentHandler as TAssemblePolygonHandler).Connect(
                      RemoteGLM,
                      VectorSurface,
                      OneGroup,
                      OnePoly,
                      nil,
                      RemoteGLM.ClientPaint);

          OnePoly.ChangeFlags(OBJECT_FLAG_SELECTED, opXOR);
          RemoteGLM.ClientPaint(self);
          sleep(100);
          OnePoly.ChangeFlags(OBJECT_FLAG_SELECTED, opXOR);
          RemoteGLM.ClientPaint(self);
        end;

        MoveContainer.FreeQuery(query^.QueryID);
      end;
    end;  // if CurrentHandler is TAssemblePolygonHandler


    if CurrentHandler is TDeleteObjectsHandler then
    begin
      MoveContainer.Clear;
      FindObjectsInProximityOfCursor(RemoteGLM, Point(X,Y), MoveContainer);
    end;

    if CurrentHandler is TMarqueeHandler then
    begin
      MoveContainer.Clear;
      FindObjectsInProximityOfCursor(RemoteGLM, Point(X,Y), MoveContainer);
    end;


    CurrentHandler.MouseDown(Sender, Button, Shift, X, Y);
  end; // if assigned CurrentHandler

end;



procedure TVectorEditorDesign.VectorSurfaceMouseUp(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned(CurrentHandler) then
    CurrentHandler.MouseUp(Sender, Button, Shift, X, Y);
end;



procedure TVectorEditorDesign.VectorSurfaceMouseMove(Sender: TObject;
                Shift: TShiftState; X, Y: Integer);
begin
  if assigned(PopupDlg) then
    if PopupDlg.Visible then PopupDlg.Hide;
    

  if assigned(CurrentHandler) then
    CurrentHandler.MouseMove(Sender, Shift, X, Y);
end;



function TVectorEditorDesign.SurfaceHandlesEvents(Button: TMouseButton;
                Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  result := false;
  if assigned(CurrentHandler) and (GetAsyncKeyState(VK_CONTROL) = 0) then
    result := CurrentHandler.SurfaceHandlesEvents(Button,Shift,MousePos);

  if (GetAsyncKeyState(VK_CONTROL) <> 0) and (ssRight in Shift) and
      ToolButton9AllowPopupBox.Down then
  begin
    ExecuteToolBoxPopup(MousePos);
    result := true;
  end;

end;



procedure TVectorEditorDesign.HandlerDisconnects(Sender: TObject);
begin
  if CurrentHandler = Sender then CurrentHandler := nil;
  
  if Sender = CreatePointHandler     then ToolButton1.Down := false;
  if Sender = AssemblePolygonHandler then ToolButton2.Down := false;
  if Sender = MoveObjectsHandler     then ToolButton3.Down := false;

  SyncButtonsToPopup;
end;



procedure TVectorEditorDesign.HideVectorObjectsofhiddenLayers1Click(
  Sender: TObject);
begin
  HideVectorObjectsofhiddenLayers1.Checked := not
    HideVectorObjectsofhiddenLayers1.Checked;

  RemoteGLM.IncludeAllLayersInMetaContentLists := not HideVectorObjectsofhiddenLayers1.Checked;
  RemoteGLM.RecreateMetaContentLists(Self);

  RemoteGLM.ClientPaint(self);
end;

procedure TVectorEditorDesign.TreeListMouseDown(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vg: TVectorObjectGroup;
  obj: TVectorObject;
begin
  if ssRight in Shift then
    TreeViewRightClick(TreeList.Tree, TreeListPopup, X, Y);

  if assigned(CurrentHandler) then
  begin
    vg  := nil;
    obj := nil;


    if TreeList.Tree.SelectionCount > 0 then
      if assigned(TreeList.Tree.Selected.Data) then
      begin
        // retrieve new object and group
        if (TObject(TreeList.Tree.Selected.Data) is TVectorObject) and
            not (TObject(TreeList.Tree.Selected.Data) is TVectorObjectGroup) then
        begin
          obj := TObject(TreeList.Tree.Selected.Data) as TVectorObject;

        if assigned(TreeList.Tree.Selected.Parent) then
          if assigned(TreeList.Tree.Selected.Parent.Data) then
            if (TObject(TreeList.Tree.Selected.Parent.Data) is TPolygonGroup) then
              vg := TVectorObjectGroup(TreeList.Tree.Selected.Parent.Data);
        end;

        // permit addition to existing group
        if (TObject(TreeList.Tree.Selected.Data) is TVectorObjectGroup) then
              vg := TVectorObjectGroup(TreeList.Tree.Selected.Data);
    end;

    if assigned(vg) then
      if (CurrentHandler is TCreatePointHandler) and
         (vg is TPointGroup) then
           CurrentHandler.Connect( RemoteGLM,    // reconnect with new group
                                VectorSurface,
                                vg,
                                obj,
                                nil,
                                RemoteGLM.ClientPaint);

    if (CurrentHandler is TAssemblePolygonHandler) and
        assigned(vg) then
    begin
      if (vg is TPolygonGroup) and
         ((not assigned(obj)) or ((obj is TSinglePolygon))) then
           CurrentHandler.Connect( RemoteGLM,    // reconnect with new group
                                VectorSurface,
                                vg,
                                obj,
                                nil,
                                RemoteGLM.ClientPaint);
    end;
  end;

end;



procedure TVectorEditorDesign.TreeListMouseUp(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TreeList.FlagsChangedByMouse then
    RemoteGLM.ClientPaint(self);
end;



procedure TVectorEditorDesign.TreeListMouseMove(Sender: TObject;
                Shift: TShiftState; X, Y: Integer);
begin
  //
end;



procedure TVectorEditorDesign.NodeFlagsChanged(Sender:TObject);
begin
  if not (Sender is TTreeNode) then exit;

  if TObject((Sender as TTreeNode).Data) is TVectorObject then
    (TObject((Sender as TTreeNode).Data) as TVectorObject).Flags :=
          (Sender as TTreeNode).StateIndex;
end;



procedure TVectorEditorDesign.OverriderightclickandshowQuickBar1Click(
  Sender: TObject);
begin
end;



//////////////////////////////////////////////////////////////////////////
// PopupMenu Item Procedures

procedure TVectorEditorDesign.MenuItemNewPointGroupClick(Sender: TObject);
var
  obj: TObject;
  dummy: TPointGroup;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //
  if not (obj is TMetaData) then exit;
  dummy := TPointGroup.Create;
  (obj as TMetaData).Add(dummy);

  RemoteGLM.RecreateMetaContentLists(self);
  self.SynchronizeTree(self);
  TreeList.Tree.Selected.Expand(false);
end;



procedure TVectorEditorDesign.MenuItemNewPolygonGroupClick(Sender: TObject);
var
  obj: TObject;
  dummy: TPolygonGroup;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //
  if not (obj is TMetaData) then exit;
  dummy := TPolygonGroup.Create;
  (obj as TMetaData).Add(dummy);

  RemoteGLM.RecreateMetaContentLists(self);
  self.SynchronizeTree(self);
  TreeList.Tree.Selected.Expand(false); 
end;



procedure TVectorEditorDesign.MenuItemFlushClick(Sender: TObject);
var obj: TObject;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //
  if (not (obj is TMetaData)) and
     (not (obj is TVectorObjectGroup)) and
     (not (obj is TSinglePolygon)) then exit;

  if (obj is TMetaData) then
     (obj as TMetaData).DestroyContent(self);

  if (obj is TVectorObjectGroup) then
     (obj as TVectorObjectGroup).Clear;

  if (obj is TSinglePolygon) then
     (obj as TSinglePolygon).Clear;

  RemoteGLM.RecreateMetaContentLists(self);
  SynchronizeTree(self);
  RemoteGLM.ClientPaint(self);
end;



procedure TVectorEditorDesign.MenuItemInsertDuplicateClick(Sender: TObject);
begin
  //
end;

procedure TVectorEditorDesign.MenuItemNewPolygonClick(Sender: TObject);
var
  obj: TObject;
  dummy: TSinglePolygon;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //
  if not (obj is TVectorObjectGroup) then exit;
  dummy := TSinglePolygon.Create;
  (obj as TVectorObjectGroup).Add(dummy);

  // start a dialog here if user is to be prompted for presets
  //

  // finish
  //
  RemoteGLM.RecreateMetaContentLists(self);
  self.SynchronizeTree(self);
  TreeList.Tree.Selected.Expand(false);
end;



procedure TVectorEditorDesign.MenuItemNewPointClick(Sender: TObject);
var
  obj: TObject;
  dummy: TSinglePoint;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //
  if not (obj is TVectorObjectGroup) then exit;
  dummy := TSinglePoint.Create;
  (obj as TVectorObjectGroup).Add(dummy);

  // start a dialog here if user is to be prompted for presets
  //

  // finish
  //
  RemoteGLM.RecreateMetaContentLists(self);
  self.SynchronizeTree(self);
  TreeList.Tree.Selected.Expand(false);
  RemoteGLM.ClientPaint(self);
end;



procedure TVectorEditorDesign.MenuItemDeleteClick(Sender: TObject);
var
  obj, parentobj: TObject;
  parentnode: TTreeNode;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // retrieve parent object by tree structure
  // (legitimate because tree is in sync)
  parentnode := TreeList.Tree.Selected.Parent;

  if not assigned(parentnode) then exit;
  if not assigned(parentnode.Data) then exit;
  parentobj := TObject(parentnode.Data);

  // perform operation
  //

  if ParentObj is TMetaData then
    (ParentObj as TMetaData).Remove(obj);

  if (obj is TVectorObject) and
     (ParentObj is TVectorObjectGroup) then
     (ParentObj as TVectorObjectGroup).Remove((obj as TVectorObject));

  if (ParentObj is TMetaData) or
     (ParentObj is TVectorObjectGroup) then
  begin
    TreeList.Tree.Selected.Data := nil;
    //TreeList.Tree.Selected.Delete;
    RemoteGLM.RecreateMetaContentLists(self); 
    SynchronizeTree(self);
    RemoteGLM.ClientPaint(self);
  end;

end;



procedure TVectorEditorDesign.MenuItemSaveContainerClick(Sender: TObject);
var obj: TObject;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //
  if not (obj is TMetaData) then exit;

  SaveContainerObjectToFile(obj as TMetaData);
end;



procedure TVectorEditorDesign.MoveallPointsinproximityofcursor1Click(
  Sender: TObject);
begin
  MoveallPointsinproximityofcursor1.Checked := not MoveallPointsinproximityofcursor1.Checked;
  CreatePointHandler.MovePointsInProximity := MoveallPointsinproximityofcursor1.Checked;
end;



procedure TVectorEditorDesign.MenuItemLoadContainerClick(Sender: TObject);
var obj: TObject;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //
  if not (obj is TMetaData) then exit;

  LoadContainerObjectFromFile(obj as TMetaData);
end;



procedure TVectorEditorDesign.MenuItemMoveSelectionHereClick(Sender: TObject);
begin
  //
end;

procedure TVectorEditorDesign.MenuItemEditClick(Sender: TObject); // OnRightclickOneObject + TreeList Click
var
  obj: TObject;
  dlg: TMultifunctionalInputDialog;
  vec: TVectorObject;
  md: TMetaData;
  infotext: String;
  s  : Array[0..1] of string;
  val: array[0..3] of Double;
  DoRepaint: Boolean;
  q : PQueryIndexList;
  ObjIdx : longint;
begin
  obj := nil;

  // implement compatibility for callers that pass a TMetaData container with one
  // or more objects as sender. Pick Point or Polygon, if only one member exists.
  //
  if assigned(Sender) then
  begin
    if (Sender is TMetaData) then
    begin
      md := Sender as TMetaData;
      if (md.Count = 1) and assigned(md.Entries[0].obj) then // one point can be edited
      begin
          if (md.Entries[0].obj is TSinglePoint) or
             (md.Entries[0].obj is TSinglePolygon) then
            obj := md.Entries[0].obj as TVectorObject;
      end
      else
      begin
          if md.Count > 1 then // separate polygon objects from points, ...
          begin
            q := md.QueryEntries(TSinglePolygon);
            if length(q^.UIDs) = 1 then // one polygon can be edited...
            begin
               ObjIdx := RemoteGLM.PolygonList.IndexByID(q^.UIDs[0]);
               if ObjIdx <> INDEX_NOT_RESOLVED then
                 Obj    := RemoteGLM.PolygonList.Items[ObjIdx].Obj as TVectorObject;
            end
            else
            begin
              // several objects exist, ask user which one he wants to edit
              Obj := HaveUserSelectOneVectorObject(md);
            end;
            md.FreeQuery(q^.QueryID);
          end;
      end;

      if not assigned(obj) then
        exit;

    end;
  end;

  // standard call as popup menu procedure for TreeList
  //
  if not assigned(obj) then
  begin
    // recover object linked to node
    //
    if not assigned(TreeList.Tree.Selected) then exit;
    if not assigned(TreeList.Tree.Selected.Data) then exit;
    obj  := TObject(TreeList.Tree.Selected.Data);
  end;


  if not (obj is TVectorObject) then
  begin
    if not (obj is TMetaData) then
      exit
    else
      begin
        md := (obj as TMetaData);

        dlg := TMultiFunctionalInputDialog.Create;
        dlg.frm.Caption := 'Edit Vector Object';
        infotext := 'This Object is a container. It holds different types of objects'#13#10+
                    'of which some can be exported and re-imported, including '#13#10+
                    'non-vector object type instances';

        dlg.AddInputField( 'Information about this dialog ...',nil,'info','','',infotext);


        dlg.AddInputField( 'Object Type :' , nil, 'static','','',md.TagName);
        dlg.AddInputField( 'Object Count :', nil, 'static','','',IntToStr(md.Count));
        dlg.AddInputField( 'Description (text) :', @s[0], 'string','','',md.Text);

        if dlg.Execute then
        begin
          md.Text := s[0];
          Self.SynchronizeTree(self);
        end;

        dlg.Destroy;
        exit;
      end;
  end
  else
  begin
    vec := obj as TVectorObject;

    infotext := 'Some attributes of a vector object can be edited,'#13#10+
              'e.g. the text, tag and individual parameters such as'#13#10+
              'coordinates. A few others however cannot be changed (type,'#13#10+
              'unique identifier). IDs are unique during the whole session.'#13#10+
              'In exported xml files, the ID attribute and references to'#13#10+
              'IDs reflect the connections among objects. For known objects,'#13#10+
              'these IDs are consistently replaced by new ones on import.'#13#10#13#10+
              'Please note that unknown object nodes within the'#13#10+
              'file are not being updated and may have ID attributes that'#13#10+
              'collide with vector object IDs when a container is re-exported.';

    dlg := TMultiFunctionalInputDialog.Create;
    dlg.frm.Caption := 'Edit Vector Object';
    dlg.AddInputField( 'Information about this dialog ...',nil,'info','','',infotext);

    dlg.AddInputField( 'Object Type :', nil  , 'static','','',vec.TagName);
    dlg.AddInputField( 'Object ID :'  , nil  , 'static','','',IntToStr(vec.UID));
    dlg.AddInputField( 'Tag :'        , @s[0], 'string','','',vec.Tag);
    dlg.AddInputField( 'Description (text) :', @s[1], 'string','','',vec.Text);

    if (obj is TSinglePoint) then
    begin
      dlg.AddInputField( 'X = ', @val[0], 'float','','',FloatToStr((vec as TSinglePoint).X) );
      dlg.AddInputField( 'Y = ', @val[1], 'float','','',FloatToStr((vec as TSinglePoint).Y) );
    end;

    DoRepaint := false;

    if dlg.Execute then
    begin
      if vec.Tag <> '' then StrDispose(vec.Tag);
      vec.Tag  := SetToPChar(s[0]);

      if vec.Text <> '' then StrDispose(vec.Text);
      vec.Text := SetToPChar(s[1]);

      if (obj is TSinglePoint) then
      begin
        DoRepaint := ( (vec as TSinglePoint).X <> val[0] ) or
                     ( (vec as TSinglePoint).Y <> val[1] );
        (vec as TSinglePoint).X := val[0];
        (vec as TSinglePoint).Y := val[1];
      end;


      Self.SynchronizeTree(self);
    end;

    dlg.Destroy;
    if DoRepaint then RemoteGLM.ClientPaint(self);
    
  end;

end;



procedure TVectorEditorDesign.MenuItemCutClick(Sender: TObject);
var obj: TObject;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //

  // move selected object(s) to temporary container
end;



procedure TVectorEditorDesign.MenuItemPasteClick(Sender: TObject);
var obj: TObject;
begin
  // recover object linked to node
  //
  if not assigned(TreeList.Tree.Selected) then exit;
  if not assigned(TreeList.Tree.Selected.Data) then exit;
  obj  := TObject(TreeList.Tree.Selected.Data);

  // perform operation
  //

  // insert objects from temporary container into obj as TMetadata or obj as TVectorObjectGroup
  // if types match
end;

//////////////////////////////////////////////////////////////////////////



procedure TVectorEditorDesign.PerformSetOperation(Source, Dest: longint; Operation: TObjectSetLogicOperation);

  procedure ChangeVectorObject(v: TVectorObject);
  begin
    case Operation of
      loSetPlus:
         begin
           if  (v.Flags > 0)  and TestObjectFlag(v.Flags, Source ) then // has flag
           begin
              v.ChangeFlags( Dest , opOR );
           end;
         end;
      loSetMinus:
          begin
            if  (v.Flags > 0)  and TestObjectFlag(v.Flags, Source ) then // has flag
            begin
              v.ChangeFlags( not Cardinal(Dest), opAND );
            end;
          end;
      loSetAll   : v.ChangeFlags(     Cardinal(Source) , opOR  );
      loUnsetAll : v.ChangeFlags( not Cardinal(Source) , opAND );
      loInvert   : v.ChangeFlags(     Cardinal(Source) , opXOR );
    end;
  end;

  procedure ChangeContainer(c: TMetaData);
  var
    idx, subidx: longint;
  begin
    for idx := 0 to c.Count - 1 do
    begin

      if c.Entries[idx].obj is TVectorObject then
        ChangeVectorObject(c.Entries[idx].obj as TVectorObject);

      if c.Entries[idx].obj is TVectorObjectGroup then
        for subidx := 0 to (c.Entries[idx].obj as TVectorObjectGroup).Count - 1 do
          ChangeVectorObject((c.Entries[idx].obj as TVectorObjectGroup).Items[subidx]);
    end;
  end;

var
  idx: longint;
begin
  if not assigned(RemoteGLM) then exit;

  ChangeContainer(RemoteGLM.GlobalMetaContent);
  for idx := 0 to Length(RemoteGLM.Layers) - 1 do
    ChangeContainer(RemoteGLM.Layers[idx].meta);

  SynchronizeTree(self);
  RemoteGLM.ClientPaint(self);
end;



procedure TVectorEditorDesign.PlacePointswithsubpixelprecision1Click(
  Sender: TObject);
begin
  PlacePointswithsubpixelprecision1.Checked :=
    not PlacePointswithsubpixelprecision1.Checked;

  if assigned(CurrentHandler) then
  begin
    CurrentHandler.AllowSubpixelCoordinates :=
                    PlacePointswithsubpixelprecision1.Checked;
  end;
end;



procedure TVectorEditorDesign.PolygonAssemblyautoselectnextpolygonappend1Click(
  Sender: TObject);
begin
  PolygonAssemblyautoselectnextpolygonappend1.Checked :=
    not PolygonAssemblyautoselectnextpolygonappend1.Checked;

  if assigned(CurrentHandler) then
  begin
    if CurrentHandler is TAssemblePolygonHandler then
    begin
      (CurrentHandler as TAssemblePolygonHandler).AutoSelectNext :=
                       PolygonAssemblyautoselectnextpolygonappend1.Checked;
    end;
  end;
end;



procedure TVectorEditorDesign.SetOperationPlus(Source, Dest: longint);
begin
  PerformSetOperation(Source,Dest,loSetPlus);
end;



procedure TVectorEditorDesign.ShowObjectHints1Click(Sender: TObject);
begin
  ShowObjectHints1.Checked := not ShowObjectHints1.Checked;
  if RemoteGLM.PointList.Count <> 0 then
    RemoteGLM.ClientPaint(self); 
end;



procedure TVectorEditorDesign.SetOperationMinus(Source, Dest: longint);
begin
  PerformSetOperation(Source,Dest,loSetMinus);
end;

procedure TVectorEditorDesign.Invert1Click(Sender: TObject);
begin
  PerformSetOperation(DDButtonField.Source,DDButtonField.Dest,loInvert);
end;

procedure TVectorEditorDesign.SelectAll1Click(Sender: TObject);
begin
  PerformSetOperation(DDButtonField.Source,DDButtonField.Dest,loSetAll);
end;

procedure TVectorEditorDesign.DeselectAll1Click(Sender: TObject);
begin
  PerformSetOperation(DDButtonField.Source,DDButtonField.Dest,loUnsetAll);
end;


procedure TVectorEditorDesign.DubButton6MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PopupDlg.Hide;
  ToolButton6.Down := true;
  ToolButton6Click(Self);
end;

procedure TVectorEditorDesign.DubButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Ref: TToolButton;
begin
  Ref := nil;

  if Sender = DubButton1 then Ref := ToolButton1;
  if Sender = DubButton2 then Ref := ToolButton2;
  if Sender = DubButton3 then Ref := ToolButton3;
  if Sender = DubButton4 then Ref := ToolButton4;
  if Sender = DubButton5 then Ref := ToolButton5;

  if not assigned(Ref) then exit;

  PopupDlg.Hide;

  Ref.Down := true;
  Ref.Click;


end;



procedure TVectorEditorDesign.ExecuteToolBoxPopup(Position : TPoint);
begin
  if not assigned(PopupDlg) then
  begin
    PopupDlg := TForm.Create(Application.MainForm);
    PopupDlg.Parent := Application.MainForm;

    PopupDlg.FormStyle    := fsNormal;
    PopupDlg.BorderStyle  := bsnone;
    PopupDlg.ClientWidth  := PopupPanel.Width;
    PopupDlg.ClientHeight := PopupPanel.Height;

    PopupPanel.Parent.RemoveControl(PopupPanel);
    PopupDlg.InsertControl(PopupPanel);
    PopupPanel.Parent := PopupDlg;
    PopupPanel.left   := 0;
    PopupPanel.top    := 0;
  end;

  Windows.SetWindowPos( PopupDlg.Handle,            // form handle
                        HWND_TOP,                   // Z-Order
                        Position.X-2, Position.Y-2, // Top, left
                        0, 0,                       // Width, Height
                        SWP_NOSIZE);                // ignore width/height parameters

  SyncButtonsToPopup;

  PopupDlg.Show;
end;



procedure TVectorEditorDesign.SyncButtonsToPopup;
begin
  DubButton1.Down := ToolButton1.Down;
  DubButton2.Down := ToolButton2.Down;
  DubButton3.Down := ToolButton3.Down;
  DubButton4.Down := ToolButton4.Down;
  DubButton5.Down := ToolButton5.Down;
end;



end.
