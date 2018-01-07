unit GraphicLayerManager;

interface
uses
   Windows, Messages, Sysutils,Classes, Controls, Types, Graphics,
   JPeg, PNGImage, Forms, Math, ShellAPI, OpenSystem,

   GLWDef, LayerSelectDlg, NotificationNode, VisualOverlayClass,
   FileSystemWatch, MultiFunctionalInputDialog, RList, GLMFunctionality;
   

  Type TGLMZoomMode     = ( zmExponential,
                            zmPixelwise,
                            zmLinear,
                            zmPresetSteps,
                            zmFixTo100Percent,
                            zmFixCurrent);

  Type TGLMHalftoneMode = ( htmBlocks,
                            htmHalftone);


  Type TGraphicLayerManager = class(TObject)
    constructor Create(Target: TForm);
    destructor Destroy; override;

  private
    parent: TForm;
    buffer: TBitmap;
    frmDockMode: TDockMode;
    frm: TLayerSelectDlg;  // only one dialog window at a time. It's
                           // better for transparence because they all look the same!
    DialogStarter:TObject; // create privilege for starter to terminate dialog while it is running

    _zoom: Single;
    _ZoomMode : TGLMZoomMode;
    _HalftoneMode: TGLMHalftoneMode;
    _pos: TPoint;
    _LayerSelectionID: integer;
    _surpressrepaint: Boolean;

    VisualOverlays : Array Of TVisualOverlay;
    EventLinkedVisualUID: longint;
    DownEventShiftState: TShiftState;

    CanMoveOrigin : Boolean; // origin can be moved if the mouse actions are not forwarded
    StartPosition : TPoint;
    OldOrigin : TPoint;

    PreviewHooks: Array of TGLMPreviewHookCallback;
    TerminatedInMessageLoop: Boolean;
    procedure SetZoom(newzoom: Single);
    procedure SetOrigin(newpos: TPoint);
    procedure CallPreviewHooks(LayerID: integer; buf: TBitmap; area: TRect);
    procedure TempFileChangeNotify(Sender:TObject);

    procedure ZoomByDelta(ticks: longint);
    procedure SetZoomMode(newmode:TGLMZoomMode);
    procedure SetHalftoneMode(newmode:TGLMHalftoneMode);

    // procedure LoadingStatusProgressbarUpdate(Sender: TObject; var Struct: TProgressbarStruct);

  public
    // FileImporter           : TGLMImporter;
    // Loadbar                : TVisualOverlayProgressbar;

    GlobalMetaContent      : TMetaData;

    PointList, PolygonList,
    PointGroupList,
    PolygonGroupList       : TResolverList; // global object lists

    OnMetaContentChange    : TNotificationNode; // notification for creation / destruction AND modification of vector contents
    OnMetaContentRefresh   : TNotificationNode; // notification for non-invasive changes (modifications) of vector content parameters
                                                // OnMetaContentChange.InsertEvent(OnMetaContentRefresh): change->refresh
    zoompoint: TPoint;
    Layers: Array of TSingleLayer;
    OnCustomPostRender : TNotifyEvent; // self param = buffer (workaround)
    OnAddLayer: TNotifyEvent;
    OnCurrentLayerChange: TNotifyEvent;
    CurrentLayer: longint;   // only used for OnCurrentLayerChange event
    OnLayerChanges: TNotificationNode;
    OnMiddleMouseButtonClick : TMouseClickEvent;
    OnDblClick: TNotifyEvent;

    UnsafeAfterClientPaintEvent : TNotificationNode;
    IncludeAllLayersInMetaContentLists : Boolean;

    property  ZoomFactor : Single        read _zoom     write SetZoom;
    property  Origin : TPoint            read _pos      write SetOrigin;
    property  imagebuffer : TBitmap      read buffer;
    property  LayerSelectionID : integer read _LayerSelectionID;
    property  ZoomMode: TGLMZoomMode     read _ZoomMode write SetZoomMode;

    procedure ClientPaint(Sender: TObject);
    procedure surpressrepaint;
    
    function  ScreenToGlobalCoord(Cursor: TPoint):TPoint; overload;
    function  ScreenToGlobalCoord(Cursor: TFloatPoint) : TFloatPoint; overload;
    function  GlobalCoordToScreen(Cursor: TPoint):TPoint; overload;
    function  GlobalCoordToScreen(Cursor: TFloatPoint) : TFloatPoint; overload;

    function  LayerByID(LayerID: longint):integer;
    function  NewLayerLoadFromFile(left, top: Longint; filename: String): integer;
    function  SaveLayerToFile(LayerID: longint; filename: string; Extension: string): BOOLEAN;
    function  NewLayer(bounds: TRect; Color: TColor): integer;
    procedure DeleteLayer(idx: integer);
    procedure FocusLayer(LayerIndex: integer);

    function  InsertFilter(Layer: integer;
                _FilterID: Cardinal;_FilterData:Pointer; _DataSize: int64;
                StreamObject: TMemoryStream = nil): integer;
    procedure RemoveFilter(Layer: integer; index: integer);
    procedure RemoveAllFilters(Layer:integer; ApplyChanges: Boolean = false);

    function  InsertPreviewHook(CallBack: TGLMPreviewHookCallback): boolean;
    function  RemovePreviewHook(CallBack: TGLMPreviewHookCallback): boolean;
    procedure RemoveAllPreviewHooks;

    function ExecuteLayerSelectionDialog(PreselectedLayerID: integer = -1;
                AllowMultiSelection: Boolean = false; TabKeyExit: Boolean = false;
                Sender:TObject = nil) : Boolean;
    procedure UpdateLayerSelectionDialog(Sender:TObject);
    function  TerminateLayerSelectionDialog(Sender:TObject = nil): Boolean;


    procedure UpdateMaskLayerUsageInformation;

    procedure InsertVisualOverlayObject(newobj: TVisualOverlay);
    function  RemoveVisualOverlayObject(obj: TVisualOverlay):TVisualOverlay; overload;
    function  RemoveVisualOverlayObject(UID: longint):TVisualOverlay; overload;
    function  GetVisualOverlayObjectByID(UID: longint): TVisualOverlay;
    function  GetVisualOverlayUIDByPos(pos: TPoint;Shift: TShiftState;
                 Button:TMouseButton = mbLeft): longint;
    procedure UpdateVisualOverlays(NewScreenArea: TRect);
    procedure ClientPaintLocally(Sender:TObject); // used for some classes like VisualOverlay

    procedure OnParentMouseDown(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
    procedure OnParentMouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer);
    procedure OnParentMouseUp(Sender: TObject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
    procedure OnParentMouseWheel(Sender: TObject; Shift: TShiftState;
                  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnParentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnParentKeyPress(Sender:TObject; var Key: Char);
    procedure OnParentKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnParentDblClick(Sender: TObject);

    procedure CallExternalEditor(LayerID: Longint; filename: string; paramstring: string);
    procedure ReleaseExternalEditor(LayerID: Longint);

    procedure ResizeBuffer(BoundsRect: TRect);
    procedure OnParentResize(Sender:TObject);

    procedure ExecutePresetsDialog(Sender:TObject);

    procedure RecreateMetaContentLists(Sender: TObject); overload; // local version

    procedure EnumAllSelectedVectorObjects(var SelectedPoints : TResolverList);
    procedure DereferencePolygonPoints(poly: TSinglePolygon; var List : TResolverList);
    function  GetGroupFlags(polygon: TSinglePolygon) : longint;

    procedure DeleteListedVectorObjects(list: TResolverList);
    function  ResolvePointGroupID(singlepoint : TSinglePoint):int64;
    function  ResolveVectorObjectGroupID(OneObject : TVectorObject):int64;

    procedure UnsafeBlitToScreen(rect: TRect; mode: Cardinal);
  end;




implementation







const
  ZoomPresetArray : Array[0..70] of single = (
    2,
    2.25,
    2.5,
    3,
    3.5,
    4,
    4.5,
    5,
    6,
    7,
    7.5,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    16,
    19,
    20,
    22,
    25,
    27,
    29,
    33,
    36,
    39,
    42,
    46,
    50,
    55,
    60,
    66,
    75,
    83,
    90,
    100,
    110,
    120,
    133,
    150,
    165,
    180,
    200,
    220,
    240,
    260,
    280,
    300,
    333,
    366,
    400,
    450,
    500,
    550,
    600,
    666,
    720,
    800,
    900,
    1000,
    1100,
    1200,
    1333,
    1500,
    1600,
    1800,
    2000,
    2500,
    3200);



constructor TGraphicLayerManager.Create(Target: TForm);
    var
      mid: integer;
begin
      SetLength(Layers, 0);
      parent := Target;
      _zoom := 1;
      _pos := Point(0,0);
      _surpressrepaint := false;
      OnCustomPostRender := nil;

      buffer        := TBitmap.Create;
      buffer.Canvas.Brush.Color := clgray;
      buffer.PixelFormat := pf24bit;
      mid           := Screen.MonitorFromWindow(Target.Handle).MonitorNum;
      buffer.Width  := Screen.Monitors[mid].Width;
      buffer.Height := Screen.Monitors[mid].Height;
      setlength(PreviewHooks,0);

      OnLayerChanges := TNotificationNode.Create;
      TerminatedInMessageLoop := false;

      EventLinkedVisualUID := -1;

      frmDockMode := dm_bottom; // dm_none; // or read from config file
      frm := nil;

      _ZoomMode := zmPresetSteps; //zmExponential;

      GlobalMetaContent   := TMetaData.Create;
      GlobalMetaContent.Suffix := ' (main)';
      PointList           := TResolverList.Create;
      PolygonList         := TResolverList.Create;
      PointGroupList      := TResolverList.Create;
      PolygonGroupList    := TResolverList.Create;
      OnMetaContentChange := TNotificationNode.Create;

      OnMetaContentRefresh:= TNotificationNode.Create;
      OnMetaContentChange.InsertEvent(OnMetaContentRefresh.SharedEvent);

      UnsafeAfterClientPaintEvent := TNotificationNode.Create;

      // FileImporter := TGLMImporter.Create(LoadingStatusProgressbarUpdate);
end;




destructor TGraphicLayerManager.Destroy;
    var
      i: integer;
begin
      for i := 0 to length(Layers)-1 do
        if assigned(Layers[i].data) then
        begin
          Layers[i].meta.Destroy;
          Layers[i].data.Destroy;
        end;
      setlengtH(Layers,0);

      for i := 0 to length(VisualOverlays)-1 do
        self.VisualOverlays[i].Destroy;
        
      setlength(VisualOverlays,0);

      buffer.Destroy;
      buffer := nil;
      OnLayerChanges.Destroy;
      OnLayerChanges := nil;

      UnsafeAfterClientPaintEvent.Destroy;

      OnMetaContentRefresh.Destroy;
      OnMetaContentRefresh := nil;
      OnMetaContentChange.Destroy;
      OnMetaContentChange := nil;

      PointList.Destroy;
      PointList := nil;
      PolygonList.Destroy;
      PolygonList := nil;
      PointGroupList.Destroy;
      PointGroupList := nil;
      PolygonGroupList.Destroy;
      PolygonGroupList := nil;

      try
        GlobalMetaContent.Destroy;
        GlobalMetaContent := nil;
      except
      end;

      {try
        FileImporter.Destroy;
      except
      end; }

      inherited;
end;




procedure TGraphicLayerManager.SetZoom(newzoom: Single);
begin
      newzoom := constrain( ZoomPresetArray[0]/100,
                            ZoomPresetArray[High(ZoomPresetArray)]/100,
                            newzoom); // zoom range = 2% - 1600%

      self._pos.X := round( (self._pos.X - self.zoompoint.X) * (newzoom / _zoom) + self.zoompoint.X);
      self._pos.Y := round( (self._pos.Y - self.zoompoint.Y) * (newzoom / _zoom) + self.zoompoint.Y);
      self._zoom := newzoom;
      if assigned(self) then
        self.ClientPaint(nil);
end;



procedure TGraphicLayerManager.ClientPaint(Sender: TObject);
    var
      LayerDataRect, SourceDataRect, Intersection, ClientRect: TFltRect;
      i: integer;
      zsf : single;
begin
      // check if skip flag is set
      if self._surpressrepaint then
      begin
         _surpressrepaint:= false;
         exit;
      end;

      if not assigned(buffer) then exit;
      if buffer.width * buffer.height = 0 then exit;
      if not assigned(parent) then exit;

      // reset buffer parameters
      buffer.Canvas.Brush.Style := bssolid;
      buffer.Canvas.Brush.Color := parent.Color;
      buffer.Canvas.FillRect(Parent.ClientRect);

      // set buffer StretchBlt mode
      if self._HalftoneMode = htmHalftone then
      begin
        // CreateHalftonePalette(buffer.Canvas.Handle);
        SetStretchBltMode(buffer.canvas.Handle, STRETCH_HALFTONE);
        SetBrushOrgEx(buffer.Canvas.Handle,0,0,nil); // avoid brush misalignment
      end
      else
      begin
        SetStretchBltMode(buffer.canvas.Handle, STRETCH_DELETESCANS);
        SetBrushOrgEx(buffer.Canvas.Handle,0,0,nil); // avoid brush misalignment
      end;

      // draw layers
      for i := 0 to Length(Layers)-1 do
      begin
        zsf := _zoom * Layers[i].InternalScaling;  // zoom scale factor
        LayerDataRect.Left   := round(  Layers[i].Position.X                        * zsf + _pos.X);
        LayerDataRect.Top    := round(  Layers[i].Position.Y                        * zsf + _pos.Y);
        LayerDataRect.Right  := round( (Layers[i].Position.X+Layers[i].data.Width ) * zsf + _pos.X );
        LayerDataRect.Bottom := round( (Layers[i].Position.Y+Layers[i].data.Height) * zsf + _pos.Y );

        ClientRect   := RectToFltRect(Parent.ClientRect);
        Intersection := IntersectionRect(LayerDataRect,ClientRect);

        if not EmptyIntersection(FltRectToRect(Intersection)) then
        begin
           SourceDataRect.Left   := ( (Intersection.Left   - LayerDataRect.Left) /zsf );
           SourceDataRect.Top    := ( (Intersection.Top    - LayerDataRect.Top)  /zsf );
           SourceDataRect.Right  := ( (Intersection.Right  - LayerDataRect.Left )/zsf );
           SourceDataRect.Bottom := ( (Intersection.Bottom - LayerDataRect.Top)  /zsf );

           if Layers[i].visible then
           begin
             if not Layers[i].suppressed then
               StretchCopyRect(Layers[i].data,buffer, SourceDataRect, Intersection); // improved method
             self.CallPreviewHooks(Layers[i].LayerID,buffer,FltRectToRect(Intersection));
           end;
        end;
      end;

      // call overlay object rendering procedures
      for i := 0 to Length(self.VisualOverlays)-1 do
        if self.VisualOverlays[i].Visible then
          self.VisualOverlays[i].Paint(buffer,self.Origin,self._zoom);

      // perform post-rendering, if assigned
      if assigned(OnCustomPostRender) then OnCustomPostRender(self.buffer);

      // blit buffer
      // it will be necessary to implement local updating that differs from ClientPaintLocally by
      // executing the rendering code above, followed by a cropped BitBlt to increase performance !!!
      // restricting the rendering of overlays and layers would cause too much trouble to be
      // a serious goal for performance tweaking. Get back to that when multiple alphablended layers
      // have to be redrawn over and over again... but not today.
      //
      Windows.BitBlt(parent.Canvas.Handle,
          0,0,buffer.Width,buffer.Height,
          buffer.Canvas.Handle,0,0,
          SRCCOPY);

      UnsafeAfterClientPaintEvent.SharedEvent(Parent);
end;




function TGraphicLayerManager.NewLayerLoadFromFile(left, top: Longint; filename: String): integer;
    var
      tempdata: TBitmap;
      jpg : TJPegImage;
      png: TPNGObject;
      Stream:TStream;
      ext: string;
      midx, MID: longint;

      procedure ExtractAlpha(dst24, src32: TBitmap);
        var i, k : longint;
            P, Q : PByteArray;
       begin
         for k := 0 to min(dst24.Height, src32.Height)-1 do
         begin
           Q := dst24.ScanLine[k];
           P := src32.ScanLine[k];
          for i :=  0 to min(dst24.Width, src32.Width)-1 do
          begin
            Q[i*3  ] := P[i*4+3];
            Q[i*3+1] := P[i*4+3];
            Q[i*3+2] := P[i*4+3];
          end;
        end;
      end;
begin
      tempdata := nil;
      result := -1;
      MID := -1;

      if not fileexists(filename) then exit;

      ext := lowercase(ExtractFileExt(filename));

      if  ext = '.png' then
      begin
        tempdata := TBitmap.Create;
        tempdata.PixelFormat := pf24bit;
        png := TPNGObject.Create;
        png.LoadFromFile(filename);
        tempdata.PixelFormat := pf24bit;
        tempdata.Width := png.Width;
        tempdata.Height := png.Height;
        tempdata.Canvas.Draw(0,0,png);
        png.Destroy;
      end;

      if (ext = '.jpg') or
         (ext = '.jpeg') then
      begin
        tempdata := TBitmap.Create;
        tempdata.PixelFormat := pf24bit;
        jpg := TJPegImage.Create;
        jpg.LoadFromFile(filename);
        tempdata.PixelFormat := pf24bit;
        tempdata.Width := jpg.Width;
        tempdata.Height := jpg.Height;
        tempdata.Canvas.Draw(0,0,jpg);
        jpg.Destroy;
      end;

      if ext = '.bmp' then
      begin
        Stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
        try
          tempdata := TBitmap.Create;
          tempdata.LoadFromStream(Stream);
          if (tempdata.PixelFormat <> pf24bit) and
             (tempdata.PixelFormat <> pf32bit) then
                 tempdata.PixelFormat := pf24bit;
          if tempdata.PixelFormat = pf32bit then
          begin
            midx := Self.NewLayer(Rect(0,0,tempData.Width,TempData.Height),clWhite);
            MID  := Layers[midx].LayerID;
            Layers[midx].LayerCaption := ExtractFileName(filename)+'_mask';
            Layers[midx].UsedAsMaskLayer := True;

            ExtractAlpha(Layers[midx].data, tempdata);

            tempdata.PixelFormat := pf24bit; // reduce
          end;


          { it was necessary to patch up the Delphi7 VCL implementation
            as shown below:

            Graphics.pas :: procedure TBitmap.ReadDIB(...);
            [...]
            // (h.w.2011-01-14): I noticed that the BitsMem size returned by
            // the system routine CreateDIBSection(...) is the exact
            // amount of memory needed for the image so any accidental padding
            // after the image data block will cause an unhandled stream read
            // error because ImageSize is too large.
            // for uncompressed images, it is easy to recalculate the size
            with BitmapInfo^.bmiHeader do
            if (biCompression = BI_RGB) then
            begin
              ImageSize := ((( biWidth * biBitCount + 7 ) div 8 + 3 ) div 4 ) * 4
                             * biHeight;
            end;
            Stream.ReadBuffer(BitsMem^, ImageSize);
            [...]

            }
        except
          if assigned(tempdata) then
            tempdata.Destroy;
          tempdata := nil;
        end;
        Stream.Free;
      end;

      if tempdata = nil then exit;

      setlength(self.Layers,length(self.Layers)+1);
      ZeroMemory(@Layers[length(Layers)-1], SizeOf(TSingleLayer));
      result := length(self.Layers)-1;
      with Layers[length(Layers)-1] do
      begin
          data := tempdata;
          Position.X := left;
          Position.Y := Top;
          InternalScaling := 1;
          Transparency := 1;
          LayerCaption := ExtractFileName(filename);
          visible := true;
          LayerID := GetNewUID; // lol, "LayerID := GetTickCount" causes awesome bugs in the layer selection dialog and access violations when deleting layers when images are cached and loading is faster than 1ms!!! Stupid, huh?
          // mask := nil;
          MaskLayerID := MID;
          FilterHeapCursor := -1;
          setlength(FilterHeap,0);
          SelectableInDialog := true;
          IsSelectedInDialog := false;
          suppressed := false;
          externaleditwatcher := nil;
          LastModified := GetTickCount;
          meta := TMetaData.Create;
          meta.LayerID:= LayerID;

          SourceIsRawFile := False;
          SourceFileName := filename;
          SourceFileType := ftStandardFormat;
      end;

      RecreateMetaContentLists(self);
      If Assigned(OnAddLayer) then OnAddLayer(self);
      OnLayerChanges.SharedEvent(self); 
end;



function TGraphicLayerManager.NewLayer(bounds: TRect; Color:TColor): integer; // returns the new layer index
    var
      tempdata: TBitmap;
begin
        tempdata                    := TBitmap.Create;
        tempdata.PixelFormat        := pf24bit;
        tempdata.Canvas.Brush.Color := Color;
        tempdata.Width  := bounds.Right  - bounds.Left;
        tempdata.Height := bounds.Bottom - bounds.Top;

      setlength(self.Layers,length(self.Layers)+1);
      ZeroMemory(@Layers[length(Layers)-1], SizeOf(TSingleLayer));

      result := length(self.Layers)-1;
      with Layers[length(Layers)-1] do
      begin
          data                 := tempdata;
          Position.X           := bounds.Left;
          Position.Y           := bounds.top;
          InternalScaling      := 1;
          Transparency         := 1;
          visible              := true;
          LayerID              := GetNewUID;
          LayerCaption         := 'Layer'+IntToStr(LayerID);
          // mask := nil;
          MaskLayerID          := -1;
          FilterHeapCursor     := -1;
          setlength(FilterHeap,0);
          SelectableInDialog   := true;
          IsSelectedInDialog   := false;
          suppressed           := false;
          ExternalEditWatcher  := nil;
          meta := TMetaData.Create;
          meta.LayerID:= LayerID;

          SourceIsRawFile      := False;
          SourceFileName       := '';
          SourceFileType       := ftStandardFormat;
      end;

      RecreateMetaContentLists(self);
      If Assigned(OnAddLayer) then OnAddLayer(self);
      OnLayerChanges.SharedEvent(self); 
end;




procedure TGraphicLayerManager.SetOrigin(newpos: TPoint);
begin
      _pos := newpos;
      self.ClientPaint(nil);
end;




procedure TGraphicLayerManager.surpressrepaint;
begin
      self._surpressrepaint := true;
end;




function TGraphicLayerManager.ScreenToGlobalCoord(Cursor: TPoint):TPoint;
begin
      result.X := round((Cursor.X - _pos.X)/_zoom);
      result.Y := round((Cursor.Y - _pos.Y)/_zoom);
end;




function TGraphicLayerManager.GlobalCoordToScreen(Cursor: TPoint):TPoint;
begin
      Result.X := _pos.X + round( (Cursor.X) * _zoom );
      Result.Y := _pos.Y + round( (Cursor.Y) * _zoom );
end;


function TGraphicLayerManager.ScreenToGlobalCoord(Cursor : TFloatPoint) : TFloatPoint;
begin
      result.X := ((Cursor.X - _pos.X)/_zoom);
      result.Y := ((Cursor.Y - _pos.Y)/_zoom);
end;



function TGraphicLayerManager.GlobalCoordToScreen(Cursor: TFloatPoint) : TFloatPoint;
begin
      Result.X := _pos.X + ( (Cursor.X) * _zoom );
      Result.Y := _pos.Y + ( (Cursor.Y) * _zoom );
end;



function TGraphicLayerManager.LayerByID(LayerID: longint):integer;
    var i: integer;
begin
      result := -1;
      for i := 0 to length(self.Layers)-1 do
      begin
        if Layers[i].LayerID  = LayerID then
        begin
          result := i;
          break;
        end;
      end;
end;




procedure TGraphicLayerManager.DeleteLayer(idx: integer);
var
  k: integer;
begin
  if (idx < 0 ) or (idx > length(Layers)-1) then exit;

  ReleaseExternalEditor(Layers[idx].LayerID);

  if assigned(Layers[idx].data)    then Layers[idx].data.Destroy;
  if assigned(Layers[idx].rawdata) then Layers[idx].rawdata.Destroy;

  Layers[idx].LayerCaption := ' - deleted - ';

  Layers[idx].meta.Destroy;
  Layers[idx].meta := nil;

  for k := idx to length(Layers)-2 do
  begin
    Layers[k] := Layers[k+1];
  end;

  setlength(Layers,length(Layers)-1);

  RecreateMetaContentLists(self);
end;




function TGraphicLayerManager.InsertFilter(Layer: integer;
                      _FilterID: Cardinal;_FilterData:Pointer; _DataSize: int64;
                      StreamObject: TMemoryStream = nil): integer;
    var
      i: integer;
begin
      result := -1;
      if (Layer < 0 ) or (Layer > length(Layers)-1) then exit;
      // if no filter has been applied yet, create raw image snapshot
      if Layers[Layer].FilterHeapCursor < 0 then
      begin
        Layers[Layer].rawdata := TBitmap.Create;
        Layers[Layer].rawdata.Width := Layers[Layer].data.Width;
        Layers[Layer].rawdata.Height := Layers[Layer].data.Height;
        Layers[Layer].rawdata.PixelFormat := Layers[Layer].data.PixelFormat;
        Layers[Layer].rawdata.Canvas.Draw(0,0,Layers[Layer].data);
      end;

      // shift all filters after cursor upwards by one
      setlength(Layers[Layer].FilterHeap,length(Layers[Layer].FilterHeap)+1);
      // Layers[Layer].FilterHeap[length(Layers[Layer].FilterHeap)-1] := AllocMem(SizeOf(TFilterInstruction));
      if (Layers[Layer].FilterHeapCursor > -1) then
        for  i := length(Layers[Layer].FilterHeap) -2 downto Layers[Layer].FilterHeapCursor + 1 do
        begin
          Layers[Layer].FilterHeap[i+1].FilterID := Layers[Layer].FilterHeap[i].FilterID;
          Layers[Layer].FilterHeap[i+1].FilterData := Layers[Layer].FilterHeap[i].FilterData;
          Layers[Layer].FilterHeap[i+1].FilterDataSize := Layers[Layer].FilterHeap[i].FilterDataSize;
        end;

      // insert new filter after cursor
      with Layers[Layer].FilterHeap[Layers[Layer].FilterHeapCursor + 1] do
      begin
        FilterID := _FilterID;
        FilterData := _FilterData;
        FilterDataSize := _DataSize;
        StreamObj := StreamObject;
      end;
      result := Layers[Layer].FilterHeapCursor + 1;
end;




procedure TGraphicLayerManager.RemoveFilter(Layer: integer; index: integer);
    var
      i: integer;
begin
      if (Layer < 0 ) or (Layer > length(Layers)-1) then exit;
      if (index < 0) or (index > length(Layers[layer].FilterHeap)-1) then exit;

      if assigned(Layers[Layer].FilterHeap[index].StreamObj)
          then begin Layers[Layer].FilterHeap[index].StreamObj.Free; end;
      if Layers[Layer].FilterHeap[index].FilterDataSize <> 0
        then FreeMem(Layers[Layer].FilterHeap[index].FilterData, Layers[Layer].FilterHeap[index].FilterDataSize);

      if index < length(Layers[Layer].FilterHeap) -1 then
      begin
        for  i := index to length(Layers[Layer].FilterHeap) -1   do
        begin
          Layers[Layer].FilterHeap[i].FilterID       := Layers[Layer].FilterHeap[i+1].FilterID;
          Layers[Layer].FilterHeap[i].FilterData     := Layers[Layer].FilterHeap[i+1].FilterData;
          Layers[Layer].FilterHeap[i].FilterDataSize := Layers[Layer].FilterHeap[i+1].FilterDataSize;
          Layers[Layer].FilterHeap[i].StreamObj      := Layers[Layer].FilterHeap[i+1].StreamObj;
        end;
      end;
      setlength(Layers[Layer].FilterHeap,length(Layers[Layer].FilterHeap)-1);
end;




procedure TGraphicLayerManager.RemoveAllFilters(Layer:integer; ApplyChanges: Boolean = false);
    var
      i: Integer;
begin
      if (Layer < 0 ) or (Layer > length(Layers)-1) then exit;

      if (not ApplyChanges) and assigned(Layers[layer].rawdata) then
         Layers[layer].data.Canvas.Draw(0,0,Layers[layer].rawdata);

      if assigned(Layers[layer].rawdata) then Layers[layer].rawdata.Free;

      for i := 0 to length(Layers[layer].FilterHeap)-1 do
      begin
        if Layers[layer].FilterHeap[i].FilterDataSize <> 0 then
          FreeMem(Layers[layer].FilterHeap[i].FilterData,Layers[layer].FilterHeap[i].FilterDataSize);

        if assigned(Layers[Layer].FilterHeap[i].StreamObj) then
        begin
          Layers[Layer].FilterHeap[i].StreamObj.Free; Layers[Layer].FilterHeap[i].StreamObj:=nil ;
        end;
      end;

      setlength(Layers[layer].FilterHeap,0);
      Layers[layer].FilterHeapCursor := -1;
end;




procedure TGraphicLayerManager.CallPreviewHooks(LayerID: integer; buf: TBitmap; area: TRect);
    var
      i: integer;
begin
      for i := 0 to length(PreviewHooks)-1 do
      begin
        try
          if assigned(PreviewHooks[i]) then PreviewHooks[i](LayerID,buf,area);
        except
        else
        end;
      end;
end;



function TGraphicLayerManager.InsertPreviewHook(CallBack: TGLMPreviewHookCallback): boolean;
begin
      result := false;
      RemovePreviewHook(CallBack); // do not insert hook twice
      if assigned(CallBack) then
      begin
        setlength(PreviewHooks,length(PreviewHooks)+1);
        PreviewHooks[length(PreviewHooks)-1] := Callback;
        result := true;
      end;
end;



function TGraphicLayerManager.RemovePreviewHook(CallBack: TGLMPreviewHookCallback): boolean;
    var
      i,j: integer;
begin
      result := false;
      for i := 0 to length(PreviewHooks)-1 do
      begin
        if @PreviewHooks[i] = @CallBack then
        begin
          for j := i to length(PreviewHooks)-2 do
            PreviewHooks[j] := PreviewHooks[j+1];
          setlength(PreviewHooks,length(PreviewHooks)-1);
          result := true;
        end;
      end;
end;



procedure TGraphicLayerManager.RemoveAllPreviewHooks;
begin
  setlength(PreviewHooks,0);
end;




function TGraphicLayerManager.ExecuteLayerSelectionDialog(
      PreselectedLayerID: integer = -1;
      AllowMultiSelection: Boolean = false;
      TabKeyExit: Boolean = false;
      Sender:TObject = nil) : Boolean;
  var
    msg:tagMSG;
    ret, i: integer;
    key: integer;
    ckey: char;
    wkey: word;
    shift: TShiftState;
begin
    result := false;
    if length(Layers)=0 then exit;

    if assigned(frm) then
    begin
      MessageBox(frm.Handle,'Layer dialog is alreay in use.'#13#10+
            'No new dialog was started to avoid confusion.',
            'Dialog arleady in use.',
            mb_OK or MB_ICONHAND );
      result := FALSE;
      exit;
    end;

    DialogStarter := Sender;

    // create form
    OnLayerChanges.InsertEvent(UpdateLayerSelectionDialog);

    frm := TLayerSelectDlg.Create(nil, frmDockMode);
    frm.Parent := parent;
    frm.Panel1.Visible := not TabKeyExit;


    frm.SetBounds(self);
    frm.RemoteGLM   := self;
    frm.multiselect := AllowMultiSelection;

    if self.LayerByID(PreselectedLayerID) <> -1 then
    begin
      frm.DeselectAll;
      frm.SelectLayer(PreselectedLayerID);
    end;

    self.TerminatedInMessageLoop := false;

    frm.Top := - frm.Height - 128; // win7 "Top=0 on Show()" error workaround
    frm.DlgShow(Sender);
    frm.SetBounds(self);           // win7 "Top=0 on Show()" error workaround


    if TabKeyExit then
    begin
      if GetAsyncKeyState(VK_SHIFT) <> 0 then
        wkey := VK_LEFT
      else
        wkey := VK_RIGHT;

      frm.OnKeyDown(self,wkey,[]);
    end;

    Application.ProcessMessages;

    OnLayerChanges.InsertEvent(frm.LayersChangedNotification);

    repeat
      ret := integer(GetMessage(msg,0,0,0));

      if ret <> 0 then
      begin
        key  := Cardinal(msg.wParam); // (word) key := msg.wparam ; Fehler bei Bereichsprüfung unter Windows 7 ?! Warum?!
        wkey := Cardinal(key)  and $FFFF;
        ckey := char(wkey and $FF);
        shift := [];

        // take over some message types...
        if (msg.message = WM_KEYDOWN) then
          begin
            frm.FormKeyDown(nil,wkey,shift);
            msg.message := 0;
          end
        else if (msg.message = WM_KEYUP) then
          begin
            if (ckey = #9) and TabKeyExit then
            begin
              PostMessage(frm.okbutton.Handle,WM_LBUTTONDOWN,0,0);
              PostMessage(frm.okbutton.Handle,WM_LBUTTONUP,0,0);
            end
            else
            begin
              frm.FormKeyPress(nil,ckey);
              frm.FormKeyUp(nil,wkey,shift);
            end;
            msg.message := 0;
          end
        else
        begin
          TranslateMessage(msg);
          DispatchMessage(msg);
        end;

      end
      else
      begin
        TerminatedInMessageLoop := true;
        application.Terminate;
        break;
      end;

      if not IsWindowVisible(frm.Handle) then break;

    until frm.isdone;


    if not TerminatedInMessageLoop then
    begin
      result := frm.success;
      try
        OnLayerChanges.RemoveEvent(UpdateLayerSelectionDialog);
        OnLayerChanges.RemoveEvent(frm.LayersChangedNotification);
        frmDockMode := frm.LastDockMode;
      except
      end;

      if (frm.success) and (not frm.multiselect) then
        for i := 0 to length(Layers) -1 do
          if Layers[i].IsSelectedInDialog then
          begin
            _LayerSelectionID := Layers[i].LayerID;
            break;
          end;
      frm.Destroy;
      frm := nil;
      while not windows.PeekMessage(msg,0,0,0,PM_REMOVE) do; // clear message queue
    end;

end;



procedure TGraphicLayerManager.FocusLayer(LayerIndex: longint);
  var
    i: integer;
begin
    if TerminatedInMessageLoop then exit;
    for i := 0 to length(Layers) -1 do
      Layers[i].visible := LayerIndex = i;
    CurrentLayer := LayerIndex;

    // zzzz
    RecreateMetaContentLists(self); 
    
    if assigned(OnCurrentLayerChange) then OnCurrentLayerChange(self);
    OnLayerChanges.SharedEvent(self);
    ClientPaint(self);
end;



procedure TGraphicLayerManager.UpdateLayerSelectionDialog(Sender:TObject);
begin
  if assigned(frm) then
    frm.LayersChangedNotification(Sender);
end;



procedure TGraphicLayerManager.UpdateMaskLayerUsageInformation;
  var
    i: integer;
    mIdx: integer;
begin
    for i := 0 to length(Layers)-1 do
      Layers[i].UsedAsMaskLayer := false;
    for i := 0 to length(Layers)-1 do
    begin
      mIdx := LayerByID(Layers[i].MaskLayerID);
      if mIdx <> -1 then Layers[mIdx].UsedAsMaskLayer := true;
    end;
end;



function TGraphicLayerManager.SaveLayerToFile(LayerID: longint;
      filename: string; Extension: string): BOOLEAN;
  var
    png: TPNGObject;
    jpg: TJPegImage;
    Layeridx, MaskIdx: longint;
    msgresult: longint;

  function removechar(s: string; chr: Char): string;
    var i: integer;
    begin
      for i := 1 to length(s) do
        if s[i] <> chr then
          result := result + s[i];
    end;


  function SaveTransparentBMP(LIdx, MIdx: longint; fn: string; force32bit: Boolean = false): Boolean;
  var
    pi    : TPNGObject;
    dummy : TBitmap;
    P, Q: PByteArray;

    procedure DrawAlpha(dst: TBitmap; x, y: longint; src: TBitmap; Scaling: single = 1.0);
    var i, k : longint;
        P, Q : PByteArray;
    begin
      // add relative scaling later if needed. This is not implemented here yet!
     if scaling <> 1.0 then Application.MessageBox('Mismatched relative scaling not supported.','TGraphicLayerManager.SaveLayerToFile.DrawAlpha',MB_ICONERROR);

     for k := min(y, dst.Height) to min(dst.Height, y + src.Height)-1 do
     begin
       Q := dst.ScanLine[k];
       P := src.ScanLine[k+y];
       for i :=  min(x, dst.Width) to min(dst.Width, x + src.Width)-1 do
       begin
         Q[i*4+3] := (P[(i+x)*3]+P[(i+x)*3+1]+P[(i+x)*3+2]) div 3;
       end;
     end;
    end;

  begin
    result := False;
    if ((MIdx < 0) and not force32bit) or (LIdx < 0) then exit;

    pi    := nil;
    dummy := nil;

    if (not force32bit) and (MIdx < 0) then
      msgresult := Application.MessageBox(  'Mask Layer present. Abort for 24bit.'#13#10+
                                            'Save Bitmap as 32bit bitmap?','Save as 32bit Bitmap?', MB_YESNO);

    if (msgresult = ID_YES)  or Force32bit then
    try
      dummy := TBitmap.Create;
      dummy.PixelFormat := pf32bit;
      dummy.TransparentMode := tmAuto;
      dummy.Transparent := True;
      dummy.Width  := Layers[LIdx].data.Width;
      dummy.Height := Layers[LIdx].data.Height;

      dummy.Canvas.Draw(0,0,Layers[LIdx].data);
      if (Midx >-1) then
        DrawAlpha(  dummy,
                    Layers[MIdx].Position.X - round(Layers[LIdx].Position.X / Layers[MIdx].InternalScaling),
                    Layers[MIdx].Position.Y - round(Layers[LIdx].Position.Y / Layers[MIdx].InternalScaling),
                    Layers[MIdx].data,
                    Layers[MIdx].InternalScaling / Layers[LIdx].InternalScaling ); // relative scaling

      dummy.SaveToFile(FileName);

      result := true;
    except
      Application.MessageBox('Save as transparent bitmap failed. Possible cause: out of memory.', 'TGraphicLayerManager.SaveLayerToFile.SaveTransparentBMP', MB_ICONERROR);
    end;

    if assigned(pi)    then pi.Destroy;
    if assigned(dummy) then Dummy.Destroy;
  end;

begin
    result := false;

    LayerIdx := LayerByID(LayerID);
    if LayerIdx = -1 then exit;
    MaskIdx := LayerByID(Layers[LayerIdx].MaskLayerID);

    extension := lowercase(removechar(extension,'*'));
    if ExtractFileExt(Filename) ='' then Filename := filename + Extension;

    if extension = '.bmp' then
    begin
     if not SaveTransparentBMP(LayerIdx, MaskIdx, FileName) then
      begin
        try
          layers[LayerIdx].data.SaveToFile(FileName);
        finally
          result := true;
        end;
      end;
    end;

    if extension = '.b32' then
    begin
      FileName := ChangeFileExt(FileName, '.bmp');
      result := SaveTransparentBMP(LayerIdx, MaskIdx, FileName, true);
    end;

    if (extension = '.jpg') or (extension = '.jpeg') then
    begin
      jpg := TJPegImage.Create;
      jpg.Assign(layers[LayerIdx].data);
      jpg.CompressionQuality := 100;
      jpg.Smoothing := true;
      jpg.Performance := jpBestQuality;
      jpg.Compress;
      jpg.SaveToFile(FileName);
      jpg.Free;
      result := true;
    end;

    if extension = '.png' then
    begin
//     if not SaveTransparentPNG(LayerIdx, MaskIdx, FileName) then
      begin
        try
           png := TPNGObject.Create;
           png.Assign(self.layers[LayerIdx].data);
           png.SaveToFile(Filename);
           png.Free;
        finally
          result := true;
        end;
      end;
    end;

end;

//

procedure TGraphicLayerManager.InsertVisualOverlayObject(newobj: TVisualOverlay);
const OBJECT_NOT_FOUND = nil;
begin
  if self.GetVisualOverlayObjectByID(newobj.UID) = OBJECT_NOT_FOUND then
  begin
    setlength(self.VisualOverlays, length( self.VisualOverlays ) + 1);
    // link events
    newobj.ParentRepaint      := self.ClientPaint;
    newobj.ParentLocalRepaint := self.ClientPaintLocally;
    self.VisualOverlays[length( self.VisualOverlays )-1] := newobj; // pass object reference
  end;
end;



function TGraphicLayerManager.RemoveVisualOverlayObject(obj: TVisualOverlay): TVisualOverlay;
var
  i: integer;
begin
  result := nil;
  for i := 0 to length(self.VisualOverlays)-1 do
    if self.VisualOverlays[i] = obj then
    begin
      result := obj;
      self.VisualOverlays[i] := self.VisualOverlays[length(self.VisualOverlays)-1];
      setlength( self.VisualOverlays, length(self.VisualOverlays)-1);
      break;
    end;
end;



function TGraphicLayerManager.RemoveVisualOverlayObject(UID: longint) : TVisualOverlay;
var
  i: integer;
begin
  result := nil;
  for i := 0 to length(self.VisualOverlays)-1 do
    if self.VisualOverlays[i].UID = UID then
    begin
      result := VisualOverlays[i];
      self.VisualOverlays[i] := self.VisualOverlays[length(self.VisualOverlays)-1];
      setlength( self.VisualOverlays, length(self.VisualOverlays)-1);
      break;
    end;
end;



function TGraphicLayerManager.GetVisualOverlayObjectByID(UID: longint): TVisualOverlay;
var
  i: integer;
begin
  result := nil;
  for i := 0 to length(self.VisualOverlays)-1 do
    if self.VisualOverlays[i].UID = UID then
    begin
      result := self.VisualOverlays[i];
      break;
    end;
end;



function TGraphicLayerManager.GetVisualOverlayUIDByPos(pos: TPoint;Shift: TShiftState;
  Button:TMouseButton): longint;
var i: longint;
begin
  result := -1;
  for i := 0 to length(self.VisualOverlays)-1 do
    if VisualOverlays[i].CursorIsInside(Button,Shift,pos) then
    begin
      result := VisualOverlays[i].UID;
      break;
    end;
end;



procedure TGraphicLayerManager.UpdateVisualOverlays(NewScreenArea: TRect);
var
  i : integer;
begin
  for i := 0 to length(self.VisualOverlays)-1 do
    self.VisualOverlays[i].UpdateParentAreaChanges(NewScreenArea);
end;



procedure TGraphicLayerManager.ClientPaintLocally(Sender:TObject);
var
  vo : TVisualOverlayPanel absolute Sender;
  i: integer;
begin
  if (Sender is TVisualOverlayPanel) then
  begin
    for i := 0 to length(self.Layers)-1 do
    begin
      vo.Paint(buffer,self.Origin,self._zoom);
    end;

    BitBlt(
      parent.Canvas.Handle,
      vo.BoundsRect.Left,
      vo.BoundsRect.Top,
      vo.Width,
      vo.Height,
      buffer.Canvas.Handle,
      vo.BoundsRect.Left,
      vo.BoundsRect.Top,
      SRCCOPY);
  end;
end;



procedure TGraphicLayerManager.OnParentMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ForwardToOverlay: Boolean;
  i: integer;

begin
  ForwardToOverlay := false;
  self.EventLinkedVisualUID := -1;
  DownEventShiftState := Shift; // shift state is lost once MouseUp occurs!

  for i := length(self.VisualOverlays)-1 downto 0 do // z-order as order of creation
  begin
    if VisualOverlays[i].CursorIsInside(Button,Shift,Point(X,Y)) then
    begin
      EventLinkedVisualUID := VisualOverlays[i].UID;
      ForwardToOverlay := true;
      VisualOverlays[i].MouseDown(Sender,Button,Shift,X,Y);
      break;
    end;
  end;

  // other options that can inhibit Origin movement are appended here
  CanMoveOrigin := (not ForwardToOverlay) and (true);

  if CanMoveOrigin then
  begin
    StartPosition     := Point(X,Y);
    OldOrigin         := self.Origin;
  end;
end;



procedure TGraphicLayerManager.OnParentMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
var
    vo: TVisualOverlay;
    p: TPoint;
const OBJECT_NOT_FOUND = nil;
begin

  // move origin or forward to visual overlay handlers
  if CanMoveOrigin and (ssLeft in Shift) then
  begin
    p.X := OldOrigin.X+(X-StartPosition.X);
    p.Y := OldOrigin.Y+(Y-StartPosition.Y);
    self.Origin := p; // set and repaint
  end
  else
  begin
    if EventLinkedVisualUID <> -1 then
    begin
      vo := self.GetVisualOverlayObjectByID(EventLinkedVisualUID);
      if vo = OBJECT_NOT_FOUND then exit;
      vo.MouseMove(Sender, Shift,X,Y);
    end;
  end;
  
end;



procedure TGraphicLayerManager.OnParentMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    vo: TVisualOverlay;
begin
  if assigned(self) then
    if EventLinkedVisualUID <> -1 then
    begin
      self.CanMoveOrigin := false;
      vo := self.GetVisualOverlayObjectByID(EventLinkedVisualUID);
      if not assigned(vo) then exit;
      vo.MouseUp(Sender,Button,Shift,X,Y);
      EventLinkedVisualUID := -1; // drop linking
    end
    else
    begin
      self.CanMoveOrigin := false;
      if ssMiddle in DownEventShiftState then
      begin
        if assigned(self.OnMiddleMouseButtonClick) then
          OnMiddleMouseButtonClick(parent,x,y,DownEventShiftState);
      end;
    end;

end;



procedure TGraphicLayerManager.OnParentMouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  UID : longint;
  VO : TVisualOverlay;
begin
  MousePos := Point(
          MousePos.X - self.parent.ClientOrigin.X,
          MousePos.Y - self.parent.ClientOrigin.Y);

  UID := self.GetVisualOverlayUIDByPos(MousePos,Shift);
  VO  := self.GetVisualOverlayObjectByID(UID);
  if assigned(VO) then
    if not (VO.HandlesWheelEvents) then UID := -1;

  if (UID <> -1) then
    begin
      VO.MouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
    end
  else
    begin
      ZoomPoint   := MousePos;
      ZoomByDelta(WheelDelta);
    end;
end;



procedure TGraphicLayerManager.CallExternalEditor(LayerID: Longint; filename: string; paramstring: string);
var
  layeridx: longint;
  path,fn : string;
begin
  layeridx := self.LayerByID(LayerID);
  if layeridx = -1 then exit;

if not ((filename = '$Explorer$') and assigned(self.Layers[layeridx].ExternalEditWatcher)) then
begin

  if assigned(self.Layers[layeridx].ExternalEditWatcher) then
    if MessageBox(0,'The selected Layer maintains an external processing link.'#13#10+
          'Discard link and start new editor?',
          'Layer is already being edited. Restart?',
          MB_YESNO or MB_ICONWARNING) = IDYES
      then
        self.Layers[layeridx].ExternalEditWatcher.Destroy
      else
        exit;

  // create temp folder and save layer
  path := ExtractFilePath(application.ExeName)+COMMON_DATA_PATH;
  if not directoryExists(path) then mkdir(path);
  path := path + '\temp';
  if not directoryExists(path) then mkdir(path);
  path := path + '\'+inttostr(layeridx);
  if not directoryExists(path) then mkdir(path);
  self.SaveLayerToFile(LayerID,path+'\modify','.bmp');
  fn := path+'\modify.bmp';

  // call external editor
  if filename = '' then
    begin
      // open as...
      if not ShellExecuteOpenAs(fn) then exit;
    end
  else
    if filename = '$Explorer$' then
      begin
        // browse...
        ShellExecute(0,'open',PChar(path),nil,nil,SW_SHOWNORMAL);
      end
    else
      begin
        // run editor ...
        if not ShellExecuteOpen(fn,filename) then exit;
      end;

  // create DirectoryWatcher and link event
  self.Layers[layeridx].ExternalEditWatcher :=  TDirectoryWatch.create(
      path,
      FILE_NOTIFY_CHANGE_LAST_WRITE,
      TempFileChangeNotify);
  self.Layers[layeridx].ExternalEditWatcher.longintdata := LayerID;
  self.Layers[layeridx].ExternalEditWatcher.str := fn;
  self.Layers[layeridx].ExternalEditWatcher.Resume;
end
else // re-open folder
begin
  path := ExtractFilePath(application.ExeName)+COMMON_DATA_PATH;
  path := path + '\temp';
  path := path + '\'+inttostr(layeridx);
  ShellExecute(0,'open',PChar(path),nil,nil,SW_SHOWNORMAL);
end;


end;



procedure TGraphicLayerManager.ReleaseExternalEditor(LayerID: Longint);
var
   idx: integer;
   fn: string;
begin
  idx := LayerByID(LayerID);
  if idx = -1 then exit;

  if assigned(Layers[idx].ExternalEditWatcher) then
  begin
    fn := Layers[idx].ExternalEditWatcher.str;
    Layers[idx].ExternalEditWatcher.Destroy;
    Layers[idx].ExternalEditWatcher := nil;
    try
      if FileExists(fn) then
      begin
        DeleteFile(fn);
        rmdir(ExtractFilePath(fn));
      end;
    except
      Application.MessageBox(
          'Cannot remove temporary file(s).',
          'Temporary files cannot be deleted',
          MB_OK);
    end; // try
  end; // if
end;



procedure TGraphicLayerManager.TempFileChangeNotify(Sender:TObject);
var
  layeridx: longint;
  wat: TDirectoryWatch absolute Sender;
begin
  Sleep(200);
  // ask user whether to decline or apply changes or
  // insert changes as a new layer
  if not (Sender is TDirectoryWatch) then exit;
  layeridx := self.LayerByID(wat.longintdata);
  if layeridx = -1 then
    self.NewLayerLoadFromFile(0,0,wat.str)
  else
  begin
    // insert refresher filter here .. allow undo/redo :-)
    // wait for file to be closed and accessible
    // not implemented! leads to access violations! 
    Layers[layeridx].data.LoadFromFile(wat.str);
  end;
  self.OnLayerChanges.SharedEvent(Sender);
  self.ClientPaint(self);
end;



procedure TGraphicLayerManager.ResizeBuffer(BoundsRect: TRect);
var
  new_width, new_height: integer;
begin
  new_width  := abs(BoundsRect.Right - BoundsRect.Left);
  new_height := abs(BoundsRect.Bottom - BoundsRect.Top);
  if new_width  > buffer.Width  then buffer.Width  := new_width  + 128;  // add some excess buffer space to boost performance
  if new_height > buffer.Height then buffer.Height := new_height + 128;
end;



procedure TGraphicLayerManager.OnParentResize(Sender:TObject);
begin
  if assigned(self.frm) then frm.SetBounds(self);
  self.CanMoveOrigin := false; // ResizeBuffer is mandatory when form is resized,
                               // so discard any mouse events that are caused by
                               // double-clicking NC area
  if assigned(Sender) then
  begin
    ResizeBuffer(parent.ClientRect);
    UpdateVisualOverlays(parent.ClientRect);
    ClientPaint(parent);
  end;
end;



procedure TGraphicLayerManager.OnParentKeyPress(Sender:TObject;var Key: Char);
var
  voID: longint;
  vo: TVisualOverlay;
begin

  // global keys
  if key = #27 then
  begin
    self.surpressrepaint;
    self.ZoomFactor := 0.5;
    self.Origin := Point(0,0);
  end;

  // shared
  voID := GetVisualOverlayUIDByPos(zoompoint,[]);
  if voID = -1 then
  begin
    if ZoomMode <> zmFixTo100Percent then
    begin
      if key = #49 then self.ZoomFactor := 0.25;
      if key = #50 then self.ZoomFactor := 0.5;
      if key = #51 then self.ZoomFactor := 1;
      if key = #52 then self.ZoomFactor := 2;
      if key = #53 then self.ZoomFactor := 4;
    end;

    if key = '+' then self.ZoomByDelta(+200); // self.ZoomFactor := self.ZoomFactor*1.1;
    if key = '-' then self.ZoomByDelta(-200); // self.ZoomFactor := self.ZoomFactor/1.1;
  end
  else
  begin
    vo := self.GetVisualOverlayObjectByID(voID);
    vo.KeyPress(Sender, Key);
  end;
end;



procedure TGraphicLayerManager.OnParentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  voID: longint;
  vo: TVisualOverlay;
begin
  voID := GetVisualOverlayUIDByPos(zoompoint,[]);
  if voID <> -1 then
  begin
    vo := self.GetVisualOverlayObjectByID(voID);
    vo.KeyDown(Sender, Key, Shift);
  end;
end;



procedure TGraphicLayerManager.OnParentKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  voID: longint;
  vo: TVisualOverlay;
begin
  voID := GetVisualOverlayUIDByPos(zoompoint,[]);
  if voID <> -1 then
  begin
    vo := self.GetVisualOverlayObjectByID(voID);
    vo.KeyUp(Sender, Key, Shift);
  end;
end;



function TGraphicLayerManager.TerminateLayerSelectionDialog(Sender:TObject = nil): Boolean; // returns true if dialog had to be terminated
begin
  if assigned(frm) then
  begin
    if Sender = DialogStarter then // privileged action
    begin
      result := frm.Visible;
      frm.isdone := true;
    end
    else
      result := false;
  end
  else
  begin
    result := false;
  end;
end;



procedure TGraphicLayerManager.SetZoomMode(newmode:TGLMZoomMode);
begin
  self._ZoomMode := newmode;

  case newmode of
    zmFixTo100Percent:
      begin
        self._zoom := 1;
        self._pos := Point(0,0);
        self.ClientPaint(nil);
        PostMessage(parent.Handle,WM_USER+$486,0,0); // send WM_UPDATE_FORM_CAPTION
      end;
  end;
end;



procedure TGraphicLayerManager.SetHalftoneMode(newmode:TGLMHalftoneMode);
begin
  if self._HalftoneMode <> newmode then
  begin
    self._HalftoneMode := newmode;
    self.ClientPaint(self);
  end;
end;



procedure TGraphicLayerManager.ZoomByDelta(ticks: longint);
var
  w, idx: integer;
//  ref, diff: single;
begin
  case _ZoomMode of
    zmExponential:
      begin
         ZoomFactor  := ZoomFactor * exp(ln(1.05) * ticks * 0.01);
      end;
    zmPixelwise:
      begin
         w := self.parent.ClientWidth;
         ZoomFactor  := ZoomFactor * exp(ln(1+1/w) * ticks * 0.01);
      end;
    zmLinear:
      begin
         ZoomFactor  := round(ZoomFactor/0.05)*0.05 + round(ticks * 0.01)*0.05;
      end;
    zmPresetSteps:
      begin
        idx := 0;
        for w := 0 to high(ZoomPresetArray) do  // find nearest list element
        begin
          if abs(ZoomPresetArray[w] - Zoomfactor*100) < abs(ZoomPresetArray[idx] - Zoomfactor*100) then
            idx := w;
        end;
        // increment or decrement index to change zoom
        idx := constrain(0,high(ZoomPresetArray),idx+ticks div 100);

        ZoomFactor := ZoomPresetArray[idx]/100;
      end;
    zmFixTo100Percent:
      begin
        // see SetZoomMode ...
      end;
  end;
end;



procedure TGraphicLayerManager.ExecutePresetsDialog(Sender:TObject);
var
  dlg: TMultiFunctionalInputDialog;
  FieldHasChanged: Array[0..15] of Boolean;
var
  iZoomMode: integer;
  iHalftoneMode: integer;
  b: Byte;
  str1: string;
begin
  str1 := 'These are the preferences for GraphicLW that will persist during'#13#10+
          'the ongoing session. You can abort the dialog if you don''t want'#13#10+
          'to apply the changes you made.'#13#10;

  dlg := TMultiFunctionalInputDialog.Create;
  dlg.frm.Caption := 'User Interface Preferences';

  dlg.AddInputField(  'Information about the settings below ...',
                      nil,
                      'info',
                      '',
                      '',
                      str1);

  dlg.AddInputField(  'mouse/keyboard zoom mode',
                      @iZoomMode,
                      'indexcombo',
                      IntToStr(integer(_zoommode)),
                      '',
                      'exponential|pixelwise|linear|preset steps|no zoom (100% view)|lock current zoom',
                      @FieldHasChanged[0]);


  dlg.AddInputField(  'screen rendering interpolation',
                      @iHalftoneMode,
                      'indexcombo',
                      IntToStr(integer(_HalftoneMode)),
                      '',
                      'n.n. rendering|halftone',
                      @FieldHasChanged[1]);


  if dlg.Execute then
  begin
    // DO NOT USE THE OBJECT VARIABLE _zoommode
    // because TGLMZoomMode is of type BYTE. Thanks, Delphi...
    b := iZoomMode;
    if FieldHasChanged[0] then self.SetZoomMode(TGLMZoomMode(b));
    b := iHalftoneMode;
    if FieldHasChanged[1] then self.SetHalftoneMode(TGLMHalftoneMode(b));
  end;

  dlg.Destroy;
end;



procedure TGraphicLayerManager.OnParentDblClick(Sender: TObject);
var
  voID: longint;
  vo: TVisualOverlay;
  cur, cl, pt: TPoint;
begin
  cl := parent.ClientOrigin;
  GetCursorPos(cur);
  pt.X := cur.X - cl.X;
  pt.Y := cur.Y - cl.Y;

  voID := GetVisualOverlayUIDByPos(pt,[]);
  if voID <> -1 then
  begin
    vo := self.GetVisualOverlayObjectByID(voID);
    if not vo.HandlesKeyEvents then
    begin
      if assigned(OnDblClick) then OnDblClick(Sender);
    end;
  end
  else if assigned(OnDblClick) then OnDblClick(Sender);
end;




procedure TGraphicLayerManager.RecreateMetaContentLists(Sender: TObject);
var
  l: longint;
begin
  if (not assigned(PointList)) or
     (not assigned(PolygonList)) or
     (not assigned(PolygonGroupList)) or
     (not assigned(PointGroupList)) or
     (not assigned(GlobalMetaContent)) then
 begin
   Application.MessageBox('Error : One or more list objects are not assigned.',
                          'TGraphicLayerManager.RecreateMetaContentLists',
                          MB_ICONERROR or MB_OK);
 end;

  // recreate lists
  try
   PointList.Clear;
   PolygonList.Clear;
   PointGroupList.Clear;
   PolygonGroupList.Clear;

   RecreateMetaListByClass( GlobalMetaContent , PointList        , TSinglePoint  , false,false);
   RecreateMetaListByClass( GlobalMetaContent , PolygonList      , TSinglePolygon, false,false);
   RecreateMetaListByClass( GlobalMetaContent , PointGroupList   , TPointGroup   , false,false);
   RecreateMetaListByClass( GlobalMetaContent , PolygonGroupList , TPolygonGroup , false,false);

    // zzzz
   for l := 0 to Length(Layers) - 1 do
   if assigned(Layers[l].meta) and (Layers[l].visible or IncludeAllLayersInMetaContentLists) then
   begin
     RecreateMetaListByClass( Layers[l].meta , PointList        , TSinglePoint   , false,false);
     RecreateMetaListByClass( Layers[l].meta , PolygonList      , TSinglePolygon , false,false);
     RecreateMetaListByClass( Layers[l].meta , PointGroupList   , TPointGroup    , false,false);
     RecreateMetaListByClass( Layers[l].meta , PolygonGroupList , TPolygonGroup  , false,false);
   end;

   PointList.SortByID;
   PolygonList.SortByID;
   PointGroupList.SortByID;
   PolygonGroupList.SortByID;

  except
    Application.MessageBox('RecreateMetaLists failed.','TGraphicLayerManager.RecreateMetaContentLists',0);
  end;
  // finally
  if assigned(OnMetaContentChange) then
    OnMetaContentChange.SharedEvent(Self)
  else
    Application.MessageBox('OnMetaContentChange is unassigned.','TGraphicLayerManager.RecreateMetaContentLists',0);
end;


{
procedure TGraphicLayerManager.LoadingStatusProgressbarUpdate(Sender: TObject; var Struct: TProgressbarStruct);
begin
  if not assigned(parent) then exit;

  if Struct.Initialize then
  begin
    if not assigned(loadbar) then
      loadbar := TVisualOverlayProgressBar.Create(parent.ClientRect);
    loadbar.Caption := 'loading images ...';
    loadbar.min := Struct.Min;
    loadbar.max := Struct.Max;
    loadbar.position := Struct.Pos;
    loadbar.Width := 200;

    loadbar.Left := (- loadbar.Width  + parent.ClientWidth  ) div 2;
    loadbar.Top  := (- loadbar.Height + parent.ClientHeight ) div 2;

    InsertVisualOverlayObject(loadbar);
    ClientPaint(self);
    loadbar.Visible := true;

    exit;
  end;

  if Struct.Terminate and assigned(loadbar) then
  begin
    Sleep(200);

    RemoveVisualOverlayObject(loadbar);
    loadbar.Destroy;
    loadbar := nil;

    exit;
  end;

  if assigned(loadbar) then
  begin
    surpressrepaint;
    loadbar.Caption := Struct.Caption;
    surpressrepaint;
    loadbar.Min      := Struct.Min;
    surpressrepaint;
    loadbar.Max      := Struct.Max;
    loadbar.position := min(Struct.Pos, Struct.Max);
  end;


end;
}




function TGraphicLayerManager.GetGroupFlags(polygon: TSinglePolygon) : longint;
  var
    I : integer;
    OneGroup: TPolygonGroup;
begin
    Result := 0;

    for I := 0 to Self.PolygonGroupList.Count - 1 do
    begin
      OneGroup := (Self.PolygonGroupList.Items[I].Obj as TPolygonGroup);
      if OneGroup.IndexByID(Polygon.UID) <> -1 then
      begin
        Result := OneGroup.Flags;
        break;
      end;
    end;
end;



procedure TGraphicLayerManager.DereferencePolygonPoints(poly: TSinglePolygon; var List : TResolverList);
  var
    idx: longint;
    objectindex : longint;
begin
    for idx := 0 to poly.Count - 1 do
    begin
      objectindex := Self.PointList.IndexByID(poly.points[idx].PointUID);
      if objectindex <> INDEX_NOT_RESOLVED then
       List.Append(Self.PointList.Items[objectIndex].Obj,
                   poly.points[idx].PointUID)
      else
       List.Append(nil, -1); // invalid point, polygon is broken here.
    end;
end;



procedure TGraphicLayerManager.EnumAllSelectedVectorObjects(var SelectedPoints : TResolverList);
var
  OnePoint : TSinglePoint;
  OnePoly  : TSinglePolygon;
  OnePG    : TPointGroup;
  PolyGroupFlags, Idx, SubIdx, ObjectIndex: longint;
  GroupFlags : longint;
begin
  if not assigned(SelectedPoints) then exit;


      for idx := 0 to Self.PointList.Count - 1 do // ok this is a bit redundant but there may be points outside pointgroup objects
      begin
        try
          if assigned(Self.PointList.Items[idx].Obj) then
          begin
            OnePoint :=  (Self.PointList.Items[idx].Obj as TSinglePoint);
            if Self.PointList.Items[idx].Obj is TSinglePoint then
              if TestObjectFlag( OnePoint.Flags, OBJECT_FLAG_SELECTED) then
                SelectedPoints.Append(OnePoint,OnePoint.UID);
          end;
        except
        end;
      end;

      for idx := 0 to Self.PolygonList.Count - 1 do
      begin
        PolyGroupFlags := GetGroupFlags(Self.PolygonList.Items[idx].Obj as TSinglePOlygon);

        try
          if assigned(Self.PolygonList.Items[idx].Obj) then
            if (Self.PolygonList.Items[idx].Obj is TSinglePOlygon) then
            begin
              OnePoly := (Self.PolygonList.Items[idx].Obj as TSinglePolygon);

              GroupFlags := OnePoly.Flags;
              if GroupFlags < 0 then GroupFlags := 0;

              for Subidx := 0 to OnePoly.Count - 1 do
              begin
                ObjectIndex := Self.PointList.IndexByID(OnePoly.Points[SubIdx].PointUID);

                if ObjectIndex <> INDEX_NOT_RESOLVED then
                begin
                  OnePoint := Self.PointList.Items[ObjectIndex].Obj as TSinglePoint;
                  if TestObjectFlag( OnePoint.Flags or
                                     GroupFlags or
                                     PolyGroupFlags,
                                     OBJECT_FLAG_SELECTED ) then
                    SelectedPoints.Append(OnePoint, OnePoint.UID);
                end; // index resolved
              end; // for
            end; // if is TSinglePolygon
        except
        end;

      end; // for polygons in list

      for idx := 0 to Self.PointGroupList.Count - 1 do
      begin

        try
          if assigned(Self.PointGroupList.Items[idx].Obj) then
            if (Self.PointGroupList.Items[idx].Obj is TPointGroup) then
            begin
              OnePG := (Self.PointGroupList.Items[idx].Obj as TPointGroup);
              GroupFlags := OnePG.Flags;
              if GroupFlags < 0 then GroupFlags := 0;

              for Subidx := 0 to OnePG.Count - 1 do
              begin
                  OnePoint := OnePG.Points[SubIdx];
                  if TestObjectFlag(OnePoint.Flags or GroupFlags, OBJECT_FLAG_SELECTED) then
                    SelectedPoints.Append(OnePoint, OnePoint.UID);
              end; // for
            end; // if is TPointGroup
        except
        end;

      end; // for PointGroups }


      // sort Points by ID
      SelectedPoints.SortByID;

      // kick duplicates
      for idx := 0 to SelectedPoints.Count - 2 do
        if SelectedPoints.Items[idx    ].ID =
           SelectedPoints.Items[idx + 1].ID then
             SelectedPoints.Items[idx].Delete := True;
      SelectedPoints.ApplyDeletion;
end;



procedure TGraphicLayerManager.DeleteListedVectorObjects(list: TResolverList);

  procedure DeleteListedObjectsWithinGroup(list: TResolverList; g : TVectorObjectGroup);
  var
    I, ItemIdx: Integer;
    groupflags : longint;
  begin
    groupflags := g.Flags;
    if groupflags < 0 then groupflags := 0;
    
    for I := g.Count - 1 downto 0 do
    begin
      if List.IndexByID(g.Items[I].UID) <> INDEX_NOT_RESOLVED then
      begin
        if not TestObjectFlag(groupflags or g.Items[I].Flags, OBJECT_FLAG_LOCKED) then
          g.Remove(g.Items[I]);
      end
      else
      begin
        for ItemIdx := 0 to g.Count - 1 do
          if assigned(g.Items[ItemIdx]) then
            if g.Items[ItemIdx] is TVectorObjectGroup then
              DeleteListedObjectsWithinGroup(List, g.Items[ItemIdx] as TVectorObjectGroup);
      end;
    end;
  end;


  procedure DeleteListedObjectsWithinContainer(list: TResolverList; Container: TMetaData);
  var
    I: Integer;
    vo : TVectorObject;
  begin
    for I := Container.Count - 1 downto 0 do
    begin
      if assigned(Container.Entries[i].obj) then
        if Container.Entries[i].obj is TVectorObject then
        begin
          vo := Container.Entries[i].obj as TVectorObject;

          if List.IndexByID(vo.UID) <> INDEX_NOT_RESOLVED then
          begin
            if not TestObjectFlag(vo.Flags, OBJECT_FLAG_LOCKED) then
              Container.Remove(vo);
          end
          else
          begin
            if vo is TVectorObjectGroup then
              DeleteListedObjectsWithinGroup(List, vo as TVectorObjectGroup);
          end;

        end;
    end;
  end;

var
  idx : longint;
begin
  Assert(assigned(GlobalMetaContent));

  DeleteListedObjectsWithinContainer(list,GlobalMetaContent);
  for idx := 0 to Length(Layers) - 1 do
  begin
    Assert(assigned(Layers[idx].meta));
    DeleteListedObjectsWithinContainer(list,Layers[idx].meta);
  end;

end;



procedure TGraphicLayerManager.UnsafeBlitToScreen(rect: TRect; mode: Cardinal);
begin
  Windows.BitBlt( Parent.Canvas.Handle,
                  rect.Left, rect.Top,
                  rect.Right - rect.Left, rect.Bottom -rect.Top,
                  buffer.Canvas.Handle,
                  rect.Left, rect.Top, mode);
end;


function TGraphicLayerManager.ResolvePointGroupID(
              singlepoint : TSinglePoint):int64;
var
  PGIdx: longint;
  pg : TPointGroup;
  Itemidx : longint;
begin
    result := -1;
    for PGIdx := 0 to PointGroupList.Count - 1 do
    begin
      if PointGroupList.Items[PGIdx].Obj is TPointGroup then
      begin
        pg := PointGroupList.Items[PGIdx].Obj as TPointGroup;
        for ItemIdx := 0 to pg.Count - 1 do
          if pg.Items[ItemIdx].UID = singlepoint.UID then
          begin
            result := pg.UID;
            exit;
          end;
      end;
    end;
end;


function TGraphicLayerManager.ResolveVectorObjectGroupID(OneObject : TVectorObject):int64;
var
  PGIdx: longint;
  pg : TVectorObjectGroup;
  GroupList : TResolverList;
  Itemidx : longint;
  GroupClassType: TClass;
begin
  result := -1;

  if not assigned(OneObject) then exit;

  GroupList := nil;

  if OneObject is TSinglePoint then
  begin
      GroupList := PointGroupList;
      GroupClassType := TPointGroup;
  end;

  if OneObject is TSinglePolygon then
  begin
      GroupList := PolygonGroupList;
      GroupClassType := TPolygonGroup;
  end;

  if not assigned(GroupList) then exit;

  for PGIdx := 0 to GroupList.Count - 1 do
    begin
      if GroupList.Items[PGIdx].Obj is GroupClassType then
      begin
        pg := GroupList.Items[PGIdx].Obj as TVectorObjectGroup;
        for ItemIdx := 0 to pg.Count - 1 do
          if pg.Items[ItemIdx].UID = OneObject.UID then
          begin
            result := pg.UID;
            exit;
          end;
      end;
  end;
end;



end.
