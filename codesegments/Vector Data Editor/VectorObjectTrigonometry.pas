unit VectorObjectTrigonometry;


interface
uses
  Types, GLWDef, GraphicLayerManager, SysUtils,
  GLMFunctionality, RList, MultifunctionalInputDialog;


function PointInProximity(screenpoint, testpoint: TPoint;
                radius: single = 6): Boolean;

function PointNearLine(a, b: TFloatPoint;
                TestPoint: TFloatPoint; MaxNDist: single) : Boolean;
function PolygonInProximity(GLM: TGraphicLayerManager;
                polypoints : TResolverList; testpoint: TPoint): Boolean;

procedure FindObjectsInProximityOfCursor(GLM: TGraphicLayerManager;
                screencur: TPoint; var container : TMetaData);

procedure FindObjectsInsideRectangle(GLM: TGraphicLayerManager;
                GlobalRect: TFloatRect; var container : TMetaData);

function HaveUserSelectOneVectorObject( cont : TMetaData ) : TVectorObject;




implementation




  function PointInProximity(screenpoint, testpoint: TPoint; radius: single = 6): Boolean;
  begin
    result:= ( abs(TestPoint.X-screenpoint.X)+
               abs(TestPoint.Y-screenpoint.Y)  ) < radius * 1.41; {ceil(radius*1.41..}
  end;

  function PointNearLine(a, b: TFloatPoint; TestPoint: TFloatPoint; MaxNDist: single) : Boolean;
  var
    d  : double; // distance AB
    nl : double; // length of projection in normal direction to AB
    pl : double; // length of projection parallel to direction AB
    dir : TFloatPOint absolute b;
  begin
    result := false;

    d := sqrt( sqr(b.X - a.X) + sqr(b.Y - a.Y));

    if d < MaxNDist * 0.1 then
    begin
      result := PointInProximity(Point(a),Point(TestPoint), MaxNDist);
      exit;
    end;

    // make relative coordinates
    //
    TestPoint.X := TestPoint.X - a.X;
    TestPoint.Y := TestPoint.Y - a.Y;
    dir.X := (b.X - a.X) / d; // now b becomes the direction vector
    dir.Y := (b.Y - a.Y) / d;
    // a.X := 0; a.Y := 0; // use implicitly by not using a

    pl := TestPoint.X * dir.X + TestPoint.Y * dir.Y; // scalar product with direction vector

    if (pl < MaxNDist) or (pl > d - MaxNDist) then exit; // normal base point is outside line segment
                                                         // spare the points at the edges
    nl := sqrt(sqr(TestPoint.X - dir.X * pl) +
               sqr(TestPoint.Y - dir.Y * pl));


    result := nl <= MaxNDist;
  end;

  function PolygonInProximity(GLM: TGraphicLayerManager;
              polypoints : TResolverList; testpoint: TPoint): Boolean;
  var
    idx, cnt : longint;
    p1, p2: TSinglePoint;
    ScreenStartPoint, ScreenEndPoint : TFloatPoint;
    ScreenCursorPoint : TFloatPoint;
  begin
    result := false;

    if not assigned(polypoints) then exit;
    if not polypoints.Count > 2 then exit;

    cnt := polypoints.Count;
    for idx := 0 to cnt - 1 do
    begin
      p1 := polypoints.Items[ idx           ].obj as TSinglePoint;
      p2 := polypoints.Items[(idx+1) mod cnt].obj as TSinglePoint;

      if assigned(p1) and assigned(p2) then
      begin
        ScreenStartPoint  := GLM.GlobalCoordToScreen(FloatPoint(p1.X, p1.Y));
        ScreenEndPoint    := GLM.GlobalCoordToScreen(FloatPoint(p2.X, p2.Y));
        ScreenCursorPoint := FloatPoint(TestPoint);

        if PointNearLine( ScreenStartPoint , ScreenEndPoint , ScreenCursorPoint, 4) then
        begin
          result := true;
          break;
        end;
      end;
    end;

  end;

  procedure FindObjectsInProximityOfCursor(GLM: TGraphicLayerManager;
                 screencur: TPoint; var container : TMetaData);
  var
    OnePoint : TSinglePoint;
    OnePoly  : TSinglePolygon;
    idx, subidx : longint;
    ptlist   : TResolverList;
  begin
    // check for points
    //
    for idx := 0 to GLM.PointList.Count - 1 do
    begin
      OnePoint := GLM.PointList.Items[idx].Obj as TSinglePoint;
      if PointInProximity( screencur ,
                           Point(GLM.GlobalCoordToScreen  (
                             FloatPoint(OnePoint.X,OnePoint.Y))) ) then
      begin
        container.Add(OnePoint);
      end;
    end;

    // check for polygon boundaries
    //
    ptlist := TResolverList.Create;
    for idx := 0 to GLM.PolygonList.Count - 1 do
    begin
      OnePoly := GLM.PolygonList.Items[idx].Obj as TSinglePolygon;
      GLM.DereferencePolygonPoints(OnePoly, ptlist);
      if PolygonInProximity(GLM, ptlist , ScreenCur ) then
      begin
        Container.Add(OnePoly); // experimental, add both points and polygons to a container
        for subidx := 0 to ptlist.Count - 1 do
          Container.Add(ptList.Items[subidx].Obj);
      end;
      ptlist.Clear;
    end;
    ptlist.Destroy;
  end;




procedure FindObjectsInsideRectangle(GLM: TGraphicLayerManager;
                GlobalRect: TFloatRect; var container : TMetaData);
  var
    OnePoint : TSinglePoint;
    OnePoly  : TSinglePolygon;
    idx, RefIdx : longint;
    ptlist   : TResolverList;
    AllPointsIn : Boolean;
  begin
    if not assigned(GLM)  or not assigned(container) then exit;

    ptlist := TResolverList.Create;

    // check for points
    //
    for idx := 0 to GLM.PointList.Count - 1 do
    begin
      OnePoint := GLM.PointList.Items[idx].Obj as TSinglePoint;
      if IsPointInRectEx( GlobalRect,
                          FloatPoint( OnePoint.X , OnePoint.Y ) ) then
      begin
        container.Add(OnePoint);
        ptlist.Append(OnePoint, OnePoint.UID);
      end;
    end;

    ptList.SortByID;

    // check for polygons that are completely inside the selection
    //
    for idx := 0 to GLM.PolygonList.Count - 1 do
    begin
      OnePoly := GLM.PolygonList.Items[idx].Obj as TSinglePolygon;

      AllPointsIn := True;

      for RefIdx := 0 to OnePoly.Count - 1 do
        AllPointsIn := AllPointsIn and
                      ((ptlist.IndexByID(OnePoly.Points[RefIdx].PointUID) <> INDEX_NOT_RESOLVED));

      if AllPointsIn then
        Container.Add(OnePoly);

    end;
    ptlist.Destroy;
end;



function HaveUserSelectOneVectorObject( cont : TMetaData ) : TVectorObject;
var
  mid: TMultifunctionalInputDialog;
  I, ItemIdx : longint;
  itemsstr : string;
  vo : TVectorObject;
  infotext: string;
begin
  result := nil;
  mid := TMultifunctionalInputDialog.Create;

  itemsstr := '';
  for I := 0 to cont.Count - 1 do
  begin
      if i <> 0 then itemsstr := itemsstr + '|';
      if not (cont.Entries[i].obj is TVectorObject) then
        itemsstr := Itemsstr + ' - '
      else
      begin
        vo := (cont.Entries[i].obj as TVectorObject);
        if (vo is TSinglePoint) and (vo.Text = '') then
          itemsstr := Itemsstr + '[' +
                      vo.TagName +'] (' +
                      FloatToStr(Round(100 * (vo as TSinglePoint).X) / 100) + ' : ' +
                      FloatToStr(Round(100 * (vo as TSinglePoint).Y) / 100) + ')  ID '+
                      IntToStr(vo.UID)
        else
          itemsstr := Itemsstr + '[' +
                      vo.TagName +'] ' +
                      TruncateText(vo.Text, 12) + '  ID '+
                      IntToStr(vo.UID);

      end;
  end;


  infotext := 'Multiple Selections exist at the screen point '#13#10+
              'you have clicked. Pick one and proceed or abort this action.';

  mid.frm.Caption := 'Edit Vector Object (multiple options)';
  mid.AddInputField( 'Information about this dialog ...',nil,'info','','',infotext);
  mid.AddInputField( 'Pick from current selection :' ,@ItemIdx, 'indexcombo','','',itemsstr,nil, 200);

  ItemIdx := -1;

  if mid.Execute then
  begin
      if ItemIdx <> -1 then
        if cont.Entries[itemidx].obj is TVectorObject then
          result := cont.Entries[itemidx].obj as TVectorObject;
  end;

  mid.Destroy;
end;



end.
