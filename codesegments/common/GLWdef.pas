unit GLWdef;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Types,
  Dialogs, StdCtrls, FileSystemWatch, Math, XMLDoc, XMLIntf, RList, ComCtrls,
  Menus;


const
  WM_LOADIMAGESONSTARTUP = WM_USER + $0386;
  WM_UPDATE_FORM_CAPTION = WM_USER + $0486;


type TXMLExportableObject = class
    Payload: WideString;
    procedure SynchronizeToNode(Node: TTreeNode);       virtual; abstract;
    procedure SaveToXMLStructure  (var xml : IXMLNode); virtual; abstract;
    procedure LoadFromXMLStructure(var xml : IXMLNode); virtual; abstract;
end;



type TMetaEntry = record
  UID: longint; // Array Identifier, independent from any other IDs the object might have
                // primary use is the indexing for query results
  obj: TObject;
end;



type TQueryIndexList = record
  QueryID: longint;
  UIDs     : array of longint;
  UnsafeObj: array of TObject;
end; PQueryIndexList = ^TQueryIndexList;


type TCreateInstanceByClassNameProc = function(ClassName: String; structure: IXMLNode): TObject of object;



type TMetaData = class(TXMLExportableObject)
  constructor Create;
  destructor Destroy; override;
private
  _Text: string;
  Queries: Array of TQueryIndexList; // index arrays
  function GetCount: longint;
public
  TagName: string;
  LayerID: longint;
  Entries: Array of TMetaEntry;      // want immediate access, moved from private to public
  OnRequestNewInstance : TCreateInstanceByClassNameProc;
  Suffix : string;

  property Text: string read _Text write _Text;
  property Count: longint read GetCount;

  function  Add(new: TObject):int64;
  function  Find(UID: int64): TObject;
  function  QueryEntries(by_this_class: TClass): PQueryIndexList;
  function  FindQuery(QUID: int64): PQueryIndexList;
  procedure FreeQuery(QUID: int64);
  procedure Remove(entry: int64);   overload;
  procedure Remove(entry: TObject); overload;
  procedure DestroyContent(Sender:TObject);
  procedure Clear;

  procedure SaveToFile(filename: string);
  procedure SaveToXMLStructure(  var xml : IXMLNode)  ; overload; override;
  procedure SaveToXMLStructure( IXMLDoc: IXMLDocument); overload; 
  procedure LoadFromXMLStructure(var xml : IXMLNode);             override;

  procedure Assimilate( Container: TMetaData );
  procedure SynchronizeToNode(tn: TTreeNode); override;
  procedure DeleteFlaggedObjects;
end;



///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////


const
  OBJECT_FLAG_VISIBLE       = $00000001;
  OBJECT_FLAG_LOCKED        = $00000002;
  OBJECT_FLAG_TOBEDELETED   = $00000004;
  OBJECT_FLAG_SELECTED      = $00000008;


type
  TFlagChangeOperation = (opAND, opOR, opXOR);

type // one type block for cross-linked objects

  TVectorObject = class;

  TObjectArray  = Array of TVectorObject;

  TVectorObject = class(TXMLExportableObject)
    constructor Create; virtual;
    destructor Destroy; override;

  private
    //TextHasChanged: Boolean;
    //procedure SetText(newtext: PChar);
    function GetTagName :string; virtual; abstract;
  protected
    _ParentLayerID: longint;
    _UID: int64;
    _IDAsLoaded: int64;
  public
    Tag        : PChar;      // some variables to make the object manageable
    Text       : PChar;
    // property Text: string read _Text write SetText
    Identifier : Cardinal;
    Flags      : longint;

    // property Visible: Boolean read GetFlagVisible write SetFlagVisible;
    // property Locked : Boolean read GetFlagLocked write SetFlagLocked;

    property IDAsLoaded: int64 read _IDAsLoaded;

    property ParentLayerID: longint read _ParentLayerID write _ParentLayerID;
    property UID: int64 read _UID;
    property TagName: string read GetTagName;
    procedure LoadFromXMLStructure(var xml : IXMLNode); override;
    procedure SynchronizeToNode(tn: TTreeNode); override;

    procedure ChangeFlags(Flags: Cardinal; FlagOp: TFlagChangeOperation);
  end;

  TVectorObjectGroup = class(TVectorObject)
    constructor Create; override;
    destructor Destroy; override;
  private
    function MakeChildInstance(ClassName: string):TVectorObject; virtual; abstract;
  protected
    ObjectArray: TObjectArray;
    function GetSingleObject(Index: longint) : TVectorObject;
    function GetCount : longint;
  public
    property Items[Index: longint]:TVectorObject read GetSingleObject;
    property Count: Longint read GetCount;

    function Add(new: TVectorObject):int64;
    function  Find(UID: int64): TVectorObject;
    procedure Remove(entry: int64; CanDestroy: Boolean = true);         overload;
    procedure Remove(entry: TVectorObject; CanDestroy: Boolean = true); overload;

    procedure SaveToXMLStructure(  var xml : IXMLNode); override;
    procedure LoadFromXMLStructure(var xml : IXMLNode); override;
    procedure SynchronizeToNode(tn: TTreeNode); override;

    procedure DeleteFlaggedObjects;
    procedure Clear;
  end;

//end of type block


type TSinglePoint = class(TVectorObject)
private
  function GetTagName :string; override;
  function ConvertToInts: TPoint;
  procedure ConvertToFloats(pt: TPoint);
public
  X, Y: double;

  property Point: TPoint read ConvertToInts write ConvertToFloats;
  
  procedure SaveToXMLStructure(  var xml : IXMLNode); override;
  procedure LoadFromXMLStructure(var xml : IXMLNode); override;
end;


type TPointGroup = class(TVectorObjectGroup)
  constructor Create; override;
  destructor Destroy; override;
private
  function MakeChildInstance(ClassName: string):TVectorObject; override;
  function GetTagName :string; override;
  function  GetSinglePoint(Index: longint): TSinglePoint;
  procedure SetSinglePoint(Index: longint; const Value: TSinglePoint);
public
  property Points[Index: Longint]: TSinglePoint read GetSinglePoint write SetSinglePoint;
end;



type TSinglePointReference = record
  GroupUID : int64;
  PointUID : int64;
end;


type TPointReferenceArray = Array of TSinglePointReference;


type TSinglePolygon = class(TVectorObject)
  constructor Create; override;
  destructor Destroy; override;
private
  function GetTagName :string; override;
  function GetCount: longint;
public
  Points: TPointReferenceArray;

  property Count: longint read GetCount;

  procedure AddPointReference(PointUID, GroupUID: int64);
  function InsertPointReference(PointUID, GroupUID: int64; At: longint): longint;
  function ReferenceIdxByID(PointID: int64):longint;
  procedure MovePointReference(FromIdx, ToIdx: longint);
  procedure Clear;

  procedure SaveToXMLStructure(  var xml : IXMLNode); override;
  procedure LoadFromXMLStructure(var xml : IXMLNode); override;
end;


type TPolygonGroup = class(TVectorObjectGroup)
  constructor Create; override;
  destructor Destroy; override;
private
  function MakeChildInstance(ClassName: string):TVectorObject; override;
  function GetTagName :string; override;
  function  GetSinglePolygon(Index: longint): TSinglePolygon;
  procedure SetSinglePolygon(Index: longint; const Value: TSinglePolygon);
public
  property Polygons[Index: Longint]: TSinglePolygon read GetSinglePolygon write SetSinglePolygon;
  function IndexByID(PolygonID: int64): longint;
end;



///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////


  // this structure belongs to an unfinished approach to realize "streaming layers" that are overwritten periodically by a streaming source.
  // Unfortunately, TWAIN sucks at delivering more than 2-3 fps, so this was never realized.
type TTWAINAcquisitionLayerInfo = record
    SourceIdentifier    : DWORD;     // TW_Identity ID is TW_UINT32
    TargetLayerPresetID : longint;   // set to -1 if not used
    TargetLayerUsedID   : longint;   // will be filled in on OnTwainAcquire
    ReuseTarget         : Boolean;   // for image stream-like behaviour, reuse a Layer and trigger OnLayerChanges afterwards
end;


type LongintProc = Procedure(Param: Longint) of object;

type TPointArrayParamFunc = procedure(var pts: Array of TPoint) of object;

type TGLMPreviewHookCallback = procedure(LayerID: integer; target: TBitmap; rct: TRect) of object;

type TMouseClickEvent = procedure(Sender:TObject; X,Y: integer; Shift: TShiftState) of object;



type TProjectionMode = (PM_DEFAULT_IMAGE, PM_SPHERICAL, PM_CYLINDRICAL, PM_RECTILINEAR, PM_OTHER);



Type TImageProjectionInfo = class
  alpha_h, alpha_v: double;
  origin_x, origin_y : double;
  projection: TProjectionMode;
end;



Type TFilterInstruction = Record
    FilterID: Cardinal;
    FilterData: Pointer;
    FilterDataSize: Int64;
    StreamObj: TMemoryStream;
end; PFilterInstruction = ^TFilterInstruction;



type TStandardFilterParameters = packed record
  bool1, bool2, bool3, bool4: Boolean;
  r1, r2, r3, r4: byte;
  g1, g2, g3, g4: byte;
  b1, b2, b3, b4: byte;
  c1, c2, c3, c4: Cardinal;
  i1, i2, i3, i4: integer;
  s1, s2 : single;
  d1, d2 : double;
end; PStandardFilterParameters = ^TStandardFilterParameters;


type TSourceFileType = (ftStandardFormat, ftRAW16bitSingle);


Type TSingleLayer = Record
    InternalScaling: Single;   // ignore
    Position: TPoint;          // absolute position in pixels
    Transparency: Single;      // 0..1
    data: TBitmap;             // pf24bit;
    rawdata: TBitmap;
    MaskLayerID: integer;         // mask as existing layer
    UsedAsMaskLayer: Boolean;
    LayerCaption: String[255];
    visible: Boolean;
    LayerID: longint;
    FilterHeap: Array Of TFilterInstruction;
    FilterHeapCursor: integer;
    SelectableInDialog: Boolean;
    IsSelectedInDialog: Boolean;
    suppressed : Boolean; // suppress rendering of the layer, leave the job to a preview hook
    ExternalEditWatcher: TDirectoryWatch;
    LastModified: Longint; // last tickcount date of change
    meta: TMetaData;

    SourceIsRawFile: Boolean;
    SourceFileName : String[255];
    SourceFileType : TSourceFileType;
end;



type TRGBColor = packed record
  case Integer of
    1 : (col: Cardinal);
    2 : (chans: Array[0..3] of byte);
end;



const
  FT_CHROMINANCE = 0;
  FT_DEPLETE_COLORS = 1;
  FT_INVERT = 2;
  FT_BRIGHTNESS_AND_CONTRAST = 3;
  FT_BRIGHTNESS_BALANCER = 4;
  {...}

  COMMON_DATA_PATH = 'GraphicLW Files';

  NODESTRUCT_ONCLICK_EVENT      = 0; // event array indices
  NODESTRUCT_ONLEFTCLICK_EVENT  = 1;
  NODESTRUCT_ONRIGHTCLICK_EVENT = 2;

  XMLATTR_ID            = 'ID';
  XMLATTR_TAG           = 'tag';
  XMLATTR_TEXT          = 'text';
  XMLATTR_LAYER         = 'layerID';

  XMLTAG_CONTAINER      = 'container';
  XMLTAG_POINTGROUPS    = 'pointGroups';
  XMLTAG_POINTGROUP     = 'pointGroup';
  XMLTAG_POLYGONGROUP   = 'polygons';
  XMLTAG_SINGLEPOLYGON  = 'polygon';
  XMLTAG_SINGLEPOINT    = 'point';
  XMLTAG_POINTREF       = 'pointReference';
  XMLTAG_POINT_X        = 'x';
  XMLTAG_POINT_Y        = 'y';
  XMLTAG_GROUPREFERENCE = 'pointGroupID';
  XMLTAG_POINTREFERENCE = 'pointID';



const // constants for customized menu item visibility,
      // see AdjustMenuItemPermissions, TreeViewRightClick

  // tag MenuOption Flags, one for each menu item
  moCut             = $00000001;
  moDelete          = $00000002;
  moNewPolyGroup    = $00000004;
  moNewPointGroup   = $00000008;
  moNewPoly         = $00000010;
  moNewPoint        = $00000020;
  moEdit            = $00000040;
  moPaste           = $00000080;
  moFlush           = $00000100;
  moLoadContainer   = $00000200;
  moSaveContainer   = $00000400;
  moInsertMove      = $00000800;
  moInsertDuplicate = $00001000;

  // pre-defined sets of flags suitable for each class
  moTMetaData     = moNewPolyGroup or
                    moNewPointGroup or
                    moEdit or
                    moFlush or
                    moLoadContainer or
                    moSaveContainer ;//or
                    //moInsertMove or
                    //moInsertDuplicate;

  moTPointGroup   = moNewPoint or
                    moEdit or
                    moFlush or
                    moDelete;

  moTPolyGroup    = moNewPoly or
                    moEdit or
                    moFlush or
                    moDelete;

  moTSinglePoly   = moEdit or
                    moFlush or
                    moDelete;

  moTSinglePoint  = moEdit or
                    moDelete;


procedure AdjustMenuItemPermissions( menu            : TPopupMenu;     // popup menu object
                                     obj             : TObject;        // TObject(TreeNode.Data)
                                     AdditionalFlags : Cardinal = 0 ); // e.g. cut, paste if available

procedure TreeViewRightClick( tree  : TTreeView;
                              menu  : TPopupMenu;
                              X , Y : integer );




function LayerBoundsRect(var Layer: TSingleLayer): TRect;

function AsRGB(color: TColor; DC: HDC = 0): TRGBColor;

function GetNewUID: int64;

procedure RecreateMetaLists(
                                    Container  : TMetaData;
                                    PointRL    : TResolverList;
                                    PolygonRL  : TResolverList;
                                    PntGroupRL : TResolverList;
                                    PlyGroupRL : TResolverList );

procedure RecreateMetaListByClass(
                                    Container: TMetaData;
                                    List  : TResolverList;
                                    TReferenceClass : TClass;
                                    ByLoadedID: Boolean = false;
                                    ClearAndSort: Boolean = true);


function TestObjectFlag(ObjectFlags, TestFlag: longint): Boolean;

function SetToPChar(const str: string): PAnsiChar;

procedure ConvertContainerBeforeImport(containernode: iXMLNode);
procedure ConvertContainerForExport(containernode: iXMLNode);

procedure StringToXMLNodes(parent: iXMLNode; str: String);

function TruncateText(str: String; len : integer = 10): string;




implementation

var
  UID_NEW : int64;
  CSEC    : _RTL_CRITICAL_SECTION;




function TruncateText(str: String; len : integer = 10): string;
begin
  if length(str) > 10 then
    result := Copy(str , 1                             , len div 2-1 ) +
              '..' +
              Copy(str , length(str)-(len-2-len div 2) , len-1-len div 2)
  else result := str;
end;


function GetNewUID: int64;
begin
  try
    EnterCriticalSection(CSEC);
    Result := UID_NEW;
    inc(UID_NEW);
    LeaveCriticalSection(CSEC);
  except
    Result := - GetTickCount;
  end;
end;



function AsRGB(color: TColor; DC: HDC = 0): TRGBColor;
var
  rgbc: TRGBColor absolute color;
  entry: PALETTEENTRY;
const
  cpSystemPalette  = $00;
  cpActingPalette  = $01;
  cpLogicalPalette = $02;
  cpGenericPalette = $08;
  cpNoColor        = $1F;
  cpDefaultColor   = $20;
  cpSystemColor    = $FF;
begin
  case rgbc.chans[3] of
    cpGenericPalette, cpLogicalPalette:
      begin
        result.col := (rgbc.col and $00FFFFFF);
      end;

    cpNoColor, cpDefaultColor :
      begin
        result.col := rgbc.col;
      end;

    cpActingPalette :
      begin
        if (GetDeviceCaps(DC,RASTERCAPS) and RC_PALETTE) = RC_PALETTE then
        begin
          if 1 = GetPaletteEntries( GetCurrentObject(DC, OBJ_PAL),
                                    rgbc.chans[0], 1, entry) then
          begin
            result.col      := $00000000;
            result.chans[0] := entry.peRed;
            result.chans[1] := entry.peGreen;
            result.chans[2] := entry.peBlue;
          end
          else
            result.col := PaletteIndex(rgbc.chans[0]);
        end
        else
          result.col := PaletteIndex(rgbc.chans[0]);
      end;

    cpSystemPalette :
      begin
        result.col := PaletteIndex(rgbc.chans[0]);
      end;

    cpSystemColor :
      begin
        result.col := GetSysColor(rgbc.chans[0]);
      end;

  end;
end;



function LayerBoundsRect(var Layer: TSingleLayer): TRect;
begin
  result.TopLeft := Layer.Position;
  result.Right := result.Left + ceil(Layer.data.Width  * Layer.InternalScaling);
  result.Bottom := result.Top + ceil(Layer.data.Height * Layer.InternalScaling);
end;



function TestObjectFlag(ObjectFlags, TestFlag: longint): Boolean;
begin
  result := false;
  if ObjectFlags < 0 then exit;

  result := (ObjectFlags and TestFlag) = TestFlag;
end;


function SetToPChar(const str: string): PAnsiChar;
begin
    if length(str) > 0  then
    begin
      result := StrAlloc(length(str)+1);
      ZeroMemory(result,length(str)+1); // because StrCopy sucks (causes access violations when the #0 at the end of the string is missing)
      CopyMemory(result, @str[1],length(str));
    end
    else
      result := nil;
end;



///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////




constructor TMetaData.Create;
begin
  Setlength(self.Entries, 0);
  Setlength(self.Queries, 0);
  TagName := XMLTAG_CONTAINER;
  LayerID := -1;
end;


destructor TMetaData.Destroy;
var quidx: longint;
begin
  Setlength(self.Entries, 0);
  for quidx := 0 to  Length(Queries) - 1 do
  begin
    SetLength(Queries[quidx].UIDs, 0);
    SetLength(Queries[quidx].UnsafeObj, 0);
  end;
  setlength(self.Queries, 0);
  
  inherited;
end;


procedure TMetaData.DeleteFlaggedObjects;
var
  idx : longint;
  cursor: longint;
begin

  cursor := -1;
  for idx := 0 to Count-1 do
  begin
    if Entries[idx].obj is TVectorObject then
    begin
      if not TestObjectFlag( (Entries[idx].obj as TVectorObject).Flags, OBJECT_FLAG_TOBEDELETED) then
      begin
        inc(cursor);
        if (Entries[idx].obj is TVectorObjectGroup) then
           (Entries[idx].obj as TVectorObjectGroup).DeleteFlaggedObjects;

        if (cursor < idx) and (cursor >=0) then
          Entries[cursor] := Entries[idx];
      end // if not delete
      else
      begin
        Entries[idx].obj.Destroy;
        Entries[idx].obj := nil;
      end;
    end; // if is TVectorObject

  end;

  SetLength(Entries, cursor+1);
end;


procedure TMetaData.SynchronizeToNode(tn: TTreeNode);
var
  LView, LTree: TResolverList;
  idx : longint;
  listidx: longint;
  node: TTreeNode;
  tv: TTreeView;
  NodeName: String;
begin

  // list all subobjects
  LView := TResolverList.Create;

  for idx := 0 to Self.Count - 1 do
  begin
    LView.Append(        Self.Entries[idx].obj,
                Cardinal(Self.Entries[idx].obj));
  end;
  LView.SortByID;

  // drop existing containers from LView list, delete vanished containers from treeview
  for idx := tn.Count-1 downto 0 do
  begin
    ListIdx := LView.IndexByID(Cardinal(tn.Item[idx].Data));
    if ListIdx = INDEX_NOT_RESOLVED then
      tn.Item[idx].Delete
    else
      LView.Items[ListIdx].Delete := TRUE; // mark for deletion
  end;

  LView.ApplyDeletion;


  // add missing items (the ones that remain after deletion) to nodelist
  for idx := 0 to LView.Count - 1 do
  begin
    tv := tn.TreeView as TTreeView;

    if LView.Items[idx].Obj is TVectorObject then
      NodeName := (LView.Items[idx].Obj as TVectorObject).TagName
    else if LView.Items[idx].Obj is TComponent then
      NodeName := (LView.Items[idx].Obj as TComponent).Name
    else
      NodeName := LView.Items[idx].Obj.ClassName;

    node      := tv.Items.AddChild(tn,NodeName);
    node.Data := LView.Items[idx].Obj;
   end;


   for idx := 0 to tn.Count - 1 do
   begin
    // MessageBox(0,PChar('TVectorObject '+self.TagName+' synchronizing item '+inttostr(idx)+' : '+(TObject(tn.item[idx].Data) as TVectorObject).GetTagName),'SynchronizeTree',MB_OK);

     if assigned(tn.item[idx].Data) then
       if TObject(tn.item[idx].Data) is TXMLExportableObject then
         (TObject(tn.item[idx].Data) as TXMLExportableObject).SynchronizeToNode(tn.Item[idx]);
   end;
     // MessageBox(0,PChar('TVectorObject '+self.TagName+' is done. '),'SynchronizeTree',MB_OK);
end;



function TMetaData.Add(new: TObject):int64;
var
  i: integer;
  CurrUID: int64;
begin

  if not assigned(new) then
  begin
    exit;
    Result := -1;
  end;

  for i := 0 to length(self.Entries)-1 do
  begin
    if Entries[i].obj = new then
    begin
      result := Entries[i].UID;
      exit;
    end;
  end;

  if new is TVectorObject then
    CurrUID := (new as TVectorObject).UID // use object UID
  else
    CurrUID := GetNewUID;
    
  setlength(Entries,length(Entries)+1);
  Entries[high(Entries)].UID := CurrUID;
  Entries[high(Entries)].obj := new;
  result := CurrUID;
end;



function TMetaData.GetCount: longint;
begin
  Result := Length(Entries);
end;



function  TMetaData.Find(UID: int64): TObject;
var i: integer;
begin
  result := nil;
  for i := 0 to length(self.Entries)-1 do
  begin
    if Entries[i].UID = UID then
    begin
      result := Entries[i].obj;
      break;
    end;
  end;
end;



procedure TMetaData.Remove(entry: int64);
var i, idx: integer;
begin
  idx := -1;
  for i := 0 to length(self.Entries)-1 do
  begin
    if Entries[i].UID = entry then
    begin
      idx := i;
      break;
    end;
  end;

  if idx <> -1 then
  begin
    try
      if assigned(Entries[idx].obj) then Entries[idx].obj.Destroy;
    except
    end;
    Entries[idx] := Entries[high(Entries)];
    setlength(Entries, length(entries)-1);
  end;
end;



procedure TMetaData.Remove(entry: TObject);
var i, idx: integer;
begin
  idx := -1;
  for i := 0 to length(self.Entries)-1 do
  begin
    if Entries[i].obj = entry then
    begin
      idx := i;
      break;
    end;
  end;

  if idx <> -1 then
  begin
    try
      if assigned(Entries[idx].obj) then Entries[idx].obj.Destroy;
    except
    end;
    Entries[idx] := Entries[high(Entries)];
    setlength(Entries, length(entries)-1);
  end;
end;



procedure TMetaData.SaveToXMLStructure(IXMLDoc: IXMLDocument);
var
  idx: longint;
  sheet : IXMLNode;
begin
  sheet := IXMLDoc.AddChild(self.TagName);

  for idx := 0 to High(Entries) do
    if (Entries[idx].obj is TXMLExportableObject) then
       (Entries[idx].obj as TXMLExportableObject).SaveToXMLStructure(sheet);

  StringToXMLNodes(sheet, PayLoad);

  ConvertContainerForExport(sheet);
end;



procedure TMetaData.SaveToFile(filename: string);
var
  xmldoc: IXMLDocument;
begin
  xmldoc := newXMLDocument;
  xmldoc.Active := true;

  self.SaveToXMLStructure(xmldoc);

  xmldoc.SaveToFile(filename);
  xmldoc.Active := False;
end;



procedure TMetaData.SaveToXMLStructure(  var xml : IXMLNode);
var
  idx: longint;
  node : IXMLNode;
begin
  node := xml.AddChild(self.TagName);

  for idx := 0 to High(Entries) do
    if (Entries[idx].obj is TXMLExportableObject) then
       (Entries[idx].obj as TXMLExportableObject).SaveToXMLStructure(node);

  // add the payload (convert it somehow!!!)

  ConvertContainerForExport(node);
end;



procedure TMetaData.LoadFromXMLStructure(var xml : IXMLNode);
var
  SubNodeIdx : longint;
  cls: TObject;
begin
  self.Payload := ''; // flush payload
  self._Text   := '';

  ConvertContainerBeforeImport(xml); // explode "pointgroups"

  if (xml.HasAttribute(XMLATTR_TEXT)) then
  begin
    if not VarIsNull(xml.Attributes[XMLATTR_TEXT]) then
    begin
      case VarType(xml.Attributes[XMLATTR_TEXT]) of
        varStrArg : self._Text  := SetToPChar(String(  xml.Attributes[XMLATTR_TEXT]));
        varOleStr : self._Text  := SetToPChar(VarToStr(xml.Attributes[XMLATTR_TEXT]));
      end;
    end;
  end;

  // create data structure
  for SubNodeIdx := xml.ChildNodes.Count-1 downto 0 do
  begin
    if assigned(OnRequestNewInstance) then
    begin
      cls := OnRequestNewInstance(
          xml.ChildNodes[SubNodeIdx].NodeName,
          xml.ChildNodes[SubNodeIdx]);

      if assigned(cls) then
      begin
        self.Add(cls);
        xml.DOMNode.RemoveChild(xml.ChildNodes[SubNodeIdx].DOMNode);
      end
      else
        self.Payload := self.Payload + xml.ChildNodes[SubNodeIdx].XML;

    end
    else
    begin // store rest as XML Payload...
      self.Payload := self.Payload + xml.ChildNodes[SubNodeIdx].XML;
    end;
  end; // for

end;



function  TMetaData.QueryEntries(by_this_class: TClass): PQueryIndexList;
var
  i,
  qidx,
  cnt: integer;
  CurrQUID: int64;
begin
  setlength(Queries,length(Queries)+1);
  qidx := high(Queries);
  CurrQUID := GetNewUID;
  Queries[qidx].QueryID :=  CurrQUID;

  setlength(Queries[qidx].UIDs,0);
  setlength(Queries[qidx].UnsafeObj,0);

  cnt := 0;
  for i := 0 to length(Entries)-1 do
  begin
    if Entries[i].obj.ClassType = by_this_class then
    begin
      inc(cnt);
      setlength(Queries[qidx].UIDs, cnt);
      setlength(Queries[qidx].UnsafeObj, cnt);

      Queries[qidx].UIDs     [cnt-1] := Entries[i].UID;
      Queries[qidx].UnsafeObj[cnt-1] := Entries[i].Obj;
    end;
  end;

  result := @Queries[qidx];
end;



function  TMetaData.FindQuery(QUID: int64): PQueryIndexList;
var i: integer;
begin
  result := nil;
  for i := 0 to length(self.Queries)-1 do
  begin
    if Queries[i].QueryID = QUID then
    begin
      result := @Queries[i];
      break;
    end;
  end;
end;



procedure TMetaData.FreeQuery(QUID: int64);
var i, idx: integer;
begin
  idx := -1;
  for i := 0 to length(Queries)-1 do
  begin
    if Queries[i].QueryID = QUID then
    begin
      idx := i;
      break;
    end;
  end;

  if idx <> -1 then
  begin
    setlength(Queries[idx].UIDs,0);
    setlength(Queries[idx].UnsafeObj,0);

    Queries[idx] := Queries[high(Queries)];
    setlength(Queries,length(Queries)-1);
  end;
end;



procedure TMetaData.Assimilate( Container: TMetaData );
var
   childidx: longint;
begin
  for childidx := 0 to Container.Count - 1 do
    Self.Add(Container.Entries[childidx].obj);

  Self.Payload := Self.Payload + Container.Payload;
end;



procedure TMetaData.DestroyContent(Sender:TObject);
var i: integer;
begin
  for i := 0 to Length(Entries) - 1 do
    if assigned(Entries[i].obj) then Entries[i].obj.Destroy;

  SetLength(Entries,0);

  self.Payload := '';
end;


procedure TMetaData.Clear;
begin
  SetLength(Entries,0);
  self.Payload := '';
end;




///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////




constructor TVectorObject.Create;
begin
  inherited;
  _ParentLayerID := -1;
  _UID := GetNewUID;
  _IDAsLoaded := -1;
  Tag := nil;
  Text := nil;
  Identifier := 0;
  Payload := '';
  Flags := OBJECT_FLAG_VISIBLE;
end;



destructor TVectorObject.Destroy;
begin
  StrDispose(Text);
  StrDispose(Tag);
  payload := ''; // assume payload belongs to VectorObject: dispose of it
  inherited;
end;



procedure TVectorObject.LoadFromXMLStructure(var xml : IXMLNode);
begin
  // OLE Strings, ain't they lovely? -.-

  if xml.HasAttribute(XMLATTR_TAG) then
   if not VarIsNull(xml.HasAttribute(XMLATTR_TAG)) then
    case VarType(xml.Attributes[XMLATTR_TAG]) of
        varStrArg : self.Tag  := SetToPChar(String(  xml.Attributes[XMLATTR_TAG]));
        varOleStr : self.Tag  := SetToPChar(VarToStr(xml.Attributes[XMLATTR_TAG]));
    else
      MessageBox(0,'Invalid Attribute Value for "tag"','XML Parsing Error',0);
    end;

  if xml.HasAttribute(XMLATTR_TEXT) then
   if not VarIsNull(xml.HasAttribute(XMLATTR_TEXT)) then
    case VarType(xml.Attributes[XMLATTR_TEXT]) of
        varStrArg : self.Text  := SetToPChar(String(  xml.Attributes[XMLATTR_TEXT]));
        varOleStr : self.Text  := SetToPChar(VarToStr(xml.Attributes[XMLATTR_TEXT]));
    else
        MessageBox(0,'Invalid Attribute Value for "text"','XML Parsing Error',0);
    end;

  if xml.HasAttribute(XMLATTR_ID) then // don't forget to update references where pointid = _IDAsLoaded
   if not VarIsNull(xml.HasAttribute(XMLATTR_ID)) then
    case VarType(xml.Attributes[XMLATTR_ID]) of
        varStrArg : self._IDAsLoaded := StrToIntDef(String(  xml.Attributes[XMLATTR_ID]),-1);
        varOleStr : self._IDAsLoaded := StrToIntDef(VarToStr(xml.Attributes[XMLATTR_ID]),-1);
    else
        MessageBox(0,'Invalid Attribute Value for "id"','XML Parsing Error',0);
        self._IDAsLoaded := -1;
    end;
end;




procedure TVectorObject.SynchronizeToNode(tn: TTreeNode);
begin
  if self.Text <> '' then
    tn.Text := TruncateText(self.Text)
  else
    tn.Text := self.GetTagName;

  tn.StateIndex := self.Flags;
end;


procedure TVectorObject.ChangeFlags(Flags: Cardinal; FlagOp: TFlagChangeOperation);
begin
  //
  if (self.Flags >= 0) then
  case FlagOp of
    opAND : self.Flags := Cardinal(self.Flags) and Flags;
    opOR  : self.Flags := Cardinal(self.Flags) or  Flags;
    opXOR : self.Flags := Cardinal(self.Flags) xor Flags;
  end;
end;

///////////////////////////////////////////////////////////////////



procedure TVectorObjectGroup.SynchronizeToNode(tn: TTreeNode);
var
  LView, LTree: TResolverList;
  idx : longint;
  listidx: longint;
  node: TTreeNode;
  tv: TTreeView;
  NodeName: String;
begin
  inherited; // from TVectorObject

  // list all subobjects
  LView := TResolverList.Create;

  for idx := 0 to Self.Count - 1 do
  begin
    LView.Append( self.ObjectArray[idx],
                  Cardinal(self.ObjectArray[idx]));
  end;
  LView.SortByID;

  // drop existing members from LView list, delete vanished members from treeview
  for idx := tn.Count-1 downto 0 do
  begin
    ListIdx := LView.IndexByID(Cardinal(tn.Item[idx].Data));
    if ListIdx = INDEX_NOT_RESOLVED then
      tn.Item[idx].Delete
    else
      LView.Items[ListIdx].Delete := TRUE; // mark for deletion
  end;

  LView.ApplyDeletion;

  // add missing items (the ones that remain after deletion) to nodelist
  for idx := 0 to LView.Count - 1 do
  begin
    tv := tn.TreeView as TTreeView;

    if LView.Items[idx].Obj is TVectorObject then
      NodeName := (LView.Items[idx].Obj as TVectorObject).TagName
    else if LView.Items[idx].Obj is TComponent then
      NodeName := (LView.Items[idx].Obj as TComponent).Name
    else
      NodeName := LView.Items[idx].Obj.ClassName;

    node := tv.Items.AddChild(tn,NodeName);
    node.Data := LView.Items[idx].Obj;
   end;

   {if LView.Count <> 0 then
     tn.Expand(false);}
   

   LView.Destroy;
   LView := nil;

   for idx := 0 to tn.Count - 1 do
   begin
     if assigned(tn.item[idx].Data) then
       if TObject(tn.item[idx].Data) is TXMLExportableObject then
         (TObject(tn.item[idx].Data) as TXMLExportableObject).SynchronizeToNode(tn.Item[idx]);
   end;   
end;


procedure TVectorObjectGroup.DeleteFlaggedObjects;
var
  idx    : longint;
  cursor : longint;
begin
  cursor := -1;

  for idx := 0 to Count-1 do
  begin
    if ObjectArray[idx] is TVectorObject then
    begin
      if not TestObjectFlag( (ObjectArray[idx] as TVectorObject).Flags, OBJECT_FLAG_TOBEDELETED) then
      begin
        inc(cursor);

        if (ObjectArray[idx] is TVectorObjectGroup) then
           (ObjectArray[idx] as TVectorObjectGroup).DeleteFlaggedObjects;

        if (cursor < idx) then
          ObjectArray[cursor] := ObjectArray[idx];
      end // if not delete
      else
      begin
        ObjectArray[idx].Destroy;
        ObjectArray[idx] := nil;
      end;
    end; // if is TVectorObject

  end;

  SetLength( ObjectArray, cursor + 1 );
end;



procedure TVectorObjectGroup.Clear;
var
  idx    : longint;
begin
  for idx := 0 to Count-1 do
    if assigned(Items[idx]) then Items[idx].Destroy;

  setlength(ObjectArray, 0);
end;

constructor TVectorObjectGroup.Create;
begin
  inherited;
  SetLength(ObjectArray, 0);
end;

destructor TVectorObjectGroup.Destroy;
begin
  Self.Clear;
  inherited;
end;


function TVectorObjectGroup.GetSingleObject(Index: longint) : TVectorObject;
begin
  Result := ObjectArray[Index];
end;



function TVectorObjectGroup.GetCount : longint;
begin
  Result := Length(ObjectArray);
end;



function TVectorObjectGroup.Add(new: TVectorObject):int64;
var
  i: integer;
begin
  for i := 0 to length(ObjectArray)-1 do
  begin
    if ObjectArray[i] = new then
    begin
      result := ObjectArray[i].UID;
      exit;
    end;
  end;

  setlength(ObjectArray,length(ObjectArray)+1);
  ObjectArray[high(ObjectArray)] := new;
  result := new.UID;
end;



function  TVectorObjectGroup.Find(UID: int64): TVectorObject;
var i: integer;
begin
  result := nil;
  for i := 0 to length(ObjectArray)-1 do
  begin
    if ObjectArray[i].UID = UID then
    begin
      result := ObjectArray[i];
      break;
    end;
  end;
end;



procedure TVectorObjectGroup.Remove(entry: int64; CanDestroy: Boolean = true);
var i, idx: integer;
begin
  idx := -1;
  for i := 0 to length(ObjectArray)-1 do
  begin
    if ObjectArray[i].UID = entry then
    begin
      idx := i;
      break;
    end;
  end;

  if idx <> -1 then
  begin
    try
      if assigned(ObjectArray[idx]) and CanDestroy then
      begin
        ObjectArray[idx].Destroy;
        ObjectArray[idx] := nil;
      end;
    except
    end;

    if Length(ObjectArray) > 1 then
      for i := idx+1 to Length(ObjectArray)-1 do
        ObjectArray[i-1] := ObjectArray[i];

    Setlength(ObjectArray, Length(ObjectArray)-1);
  end;
end;



procedure TVectorObjectGroup.Remove(entry: TVectorObject; CanDestroy: Boolean = true);
var i, idx: integer;
begin
  idx := -1;
  if Length(ObjectArray) = 0 then exit;



  for i := 0 to length(ObjectArray)-1 do
  begin
    if ObjectArray[i] = entry then
    begin
      idx := i;
      break;
    end;
  end;

  if idx <> -1 then
  begin
    try
      if assigned(ObjectArray[idx]) and CanDestroy then
      begin
        ObjectArray[idx].Destroy;
        ObjectArray[idx] := nil;
      end;
    except
    end;

    if Length(ObjectArray) > 1 then
      for i := idx+1 to Length(ObjectArray)-1 do
        ObjectArray[i-1] := ObjectArray[i];

    Setlength(ObjectArray, Length(ObjectArray)-1);
  end;
end;



procedure TVectorObjectGroup.SaveToXMLStructure(var xml : IXMLNode);
var
  node: IXMLNode;
  idx : longint;
begin
  node := xml.AddChild(self.TagName);
  node.Attributes[XMLATTR_ID  ] := IntToStr(self._UID);
  node.Attributes[XMLATTR_TAG ] := String(self.Tag);
  node.Attributes[XMLATTR_TEXT] := String(self.Text);

  for idx := 0 to Length(ObjectArray)-1 do
    ObjectArray[idx].SaveToXMLStructure(node);

  StringToXMLNodes(node,PayLoad);
end;



procedure TVectorObjectGroup.LoadFromXMLStructure(var xml : IXMLNode);
var
  subnodeidx: integer;
  newinst: TVectorObject;
  xmlnode: IXMLNode;
begin
  inherited;

  for subnodeidx := 0 to xml.ChildNodes.Count - 1 do
  begin
    xmlnode := xml.ChildNodes[subnodeidx];
    newinst := MakeChildInstance(xmlnode.NodeName);
    if assigned(newinst) then
    begin
      self.Add(newinst);
      newinst.LoadFromXMLStructure(xmlnode);
    end
    else
      self.Payload := self.Payload + xmlnode.xml;
  end;
end;




///////////////////////////////////////////////////////////////////




function TSinglePoint.ConvertToInts: TPoint;
begin
  Result.X := floor(X);
  Result.Y := floor(Y);
end;



procedure TSinglePoint.ConvertToFloats(pt: TPoint);
begin
  X := pt.X;
  Y := pt.Y;
end;



function TSinglePoint.GetTagName :string;
begin
  result := XMLTAG_SINGLEPOINT;
end;



procedure TSinglePoint.SaveToXMLStructure(var xml : IXMLNode);
var
  node: IXMLNode;
begin
  node := xml.AddChild(self.TagName);
  node.Attributes[XMLATTR_ID  ] := IntToStr(self._UID);
  node.Attributes[XMLATTR_TAG ] := String(self.Tag);
  node.Attributes[XMLATTR_TEXT] := String(self.Text);

  node.AddChild(XMLTAG_POINT_X).Text := FloatToStr(X);
  node.AddChild(XMLTAG_POINT_Y).Text := FloatToStr(Y);

  StringToXMLNodes(node,PayLoad);
end;



procedure TSinglePoint.LoadFromXMLStructure(var xml : IXMLNode);
var
  i: integer;
begin
  inherited;

  for i := 0 to xml.ChildNodes.Count - 1 do
  begin
    if xml.ChildNodes[i].NodeName = XMLTAG_POINT_X then
      self.X := StrToFloatDef(xml.ChildNodes[i].Text, 0);
    if xml.ChildNodes[i].NodeName = XMLTAG_POINT_Y then
      self.Y := StrToFloatDef(xml.ChildNodes[i].Text, 0);
  end;
end;



///////////////////////////////////////////////////////////////////



constructor TPointGroup.Create;
begin
  inherited;
  setlength(self.ObjectArray, 0);
  Flags := OBJECT_FLAG_VISIBLE;
end;



destructor TPointGroup.Destroy;
var i: integer;
begin
  for i := 0 to length(ObjectArray) -1 do
    if assigned(Points[i]) then ObjectArray[i].Destroy;
  setlength(ObjectArray, 0);
  inherited;
end;



function TPointGroup.MakeChildInstance(ClassName: string):TVectorObject;
begin
  result := TSinglePoint.Create;
end;



function TPointGroup.GetTagName :string;
begin
  result := XMLTAG_POINTGROUP;
end;



function  TPointGroup.GetSinglePoint(Index: longint): TSinglePoint;
begin
  if (Index >= 0) and (Index < Length(ObjectArray)) then
    result := (ObjectArray[Index] as TSinglePoint)
  else
    result := nil;
end;



procedure TPointGroup.SetSinglePoint(Index: longint; const Value: TSinglePoint);
begin
  if (Index >= 0) and (Index < Length(ObjectArray)) then
    ObjectArray[Index] := Value;
end;



///////////////////////////////////////////////////////////////////



constructor TSinglePolygon.Create;
begin
  inherited;
  setlength(self.Points, 0);
  Flags := OBJECT_FLAG_VISIBLE;
end;



destructor TSinglePolygon.Destroy;
begin
  setlength(self.Points, 0); // dispose references
  inherited;
end;



function TSinglePolygon.GetTagName :string;
begin
  result := XMLTAG_SINGLEPOLYGON;
end;



function TSinglePolygon.GetCount: longint;
begin
  result := Length(Points);
end;



procedure TSinglePolygon.MovePointReference(FromIdx, ToIdx: longint);
var
  cnt, Idx : longint;
  dummy: TSinglePointReference;
begin
  cnt := self.Count;
  if (cnt = 0) or (FromIdx = ToIdx) then exit;

  if (FromIdx < 0)     or (ToIdx < 0)      or
     (FromIdx > cnt-1) or (ToIdx > cnt -1) then
    exit;

  if FromIdx < ToIdx then
  begin
    dummy := Points[FromIdx];
    for Idx := FromIdx to ToIdx - 1 do
      Points[idx] := Points[idx + 1];
    Points[ToIdx] := dummy;
  end
  else // FromIdx > ToIdx
  begin
    dummy := Points[FromIdx];
    for Idx := FromIdx downto ToIdx + 1 do
      Points[idx] := Points[idx - 1];
    Points[ToIdx] := dummy;
  end;
end;



procedure TSinglePolygon.AddPointReference(PointUID, GroupUID: int64);
begin
  setlength(Points, Length(Points)+1);
  Points[High(Points)].GroupUID := GroupUID;
  Points[High(Points)].PointUID := PointUID;
end;

function TSinglePolygon.ReferenceIdxByID(PointID: int64):longint;
var idx: longint;
begin
  result := -1;
  for idx := 0 to Count - 1 do
    if self.Points[idx].PointUID = PointID then
    begin
      result := idx;
      break;
    end;
end;



function TSinglePolygon.InsertPointReference(PointUID, GroupUID: int64; At: longint): longint;
var
  OldLen : longint;
  Idx : longint;
begin
  Result := -1;
  if (PointUID = -1) or (GroupUID = -1) then exit;

  OldLen := Count;

  if (At <= -1) or (At > self.Count-1) then At := OldLen;

  SetLength(Points, OldLen+1);

  for Idx := OldLen downto At+1 do
    Points[idx] := Points[idx-1];

  Points[At].GroupUID := GroupUID;
  Points[At].PointUID := PointUID;

  Result := At; // return number of array element or array index + 1 repectively.
end;



procedure TSinglePolygon.Clear;
begin
  setlength(Points,0);
end;



procedure TSinglePolygon.SaveToXMLStructure(var xml : IXMLNode);
var
  node: IXMLNode;
  subnode : IXMLNode;
  idx: longint;
begin
  node := xml.AddChild(self.TagName);
  node.Attributes[XMLATTR_ID  ] := IntToStr(self._UID);
  node.Attributes[XMLATTR_TAG ] := String(self.Tag);
  node.Attributes[XMLATTR_TEXT] := String(self.Text);

  for idx := 0 to Length(Points)-1 do
  begin
    subnode := node.AddChild(XMLTAG_POINTREF);
    subnode.AddChild(XMLTAG_GROUPREFERENCE).Text := IntToStr(Points[idx].GroupUID);
    subnode.AddChild(XMLTAG_POINTREFERENCE).Text := IntToStr(Points[idx].PointUID);
  end;

  StringToXMLNodes(node,PayLoad);
end;



procedure TSinglePolygon.LoadFromXMLStructure(var xml : IXMLNode);
var
  i, subnodeidx: integer;
  pid, gid: int64;
begin
  inherited;

  for i := 0 to xml.ChildNodes.Count - 1 do
  begin
    if xml.ChildNodes[i].NodeName = XMLTAG_POINTREF then
    begin
      gid := -1; pid := -1;
      for subnodeidx := 0 to xml.ChildNodes[i].ChildNodes.Count -1 do
      begin
        if xml.ChildNodes[i].ChildNodes[subnodeidx].NodeName = XMLTAG_GROUPREFERENCE then
              gid :=  StrToIntDef(xml.ChildNodes[i].ChildNodes[subnodeidx].Text,-1);
        if xml.ChildNodes[i].ChildNodes[subnodeidx].NodeName = XMLTAG_POINTREFERENCE then
              pid :=  StrToIntDef(xml.ChildNodes[i].ChildNodes[subnodeidx].Text,-1);
      end;

      if (gid <> -1 ) and (pid <> -1) then
        self.AddPointReference(pid,gid)
      else
        Windows.MessageBox(0,
                    'Warning : invalid point reference.'#13#10,
                    'Data Import Problem',MB_OK);
    end
    else
      self.Payload := self.Payload + xml.ChildNodes[i].XML;
  end;
end;



constructor TPolygonGroup.Create;
begin
  inherited;
  setlength(self.ObjectArray, 0);
  self.Flags := OBJECT_FLAG_VISIBLE;
end;



destructor TPolygonGroup.Destroy;
var i: integer;
begin
  for i := 0 to length(ObjectArray) -1 do
    if assigned(ObjectArray[i]) then ObjectArray[i].Destroy;
  setlength(ObjectArray, 0);
  inherited;
end;



function TPolygonGroup.MakeChildInstance(ClassName: string):TVectorObject;
begin
  result := TSinglePolygon.Create;
end;



function TPolygonGroup.GetTagName :string;
begin
  result := XMLTAG_POLYGONGROUP;
end;



function  TPolygonGroup.GetSinglePolygon(Index: longint): TSinglePolygon;
begin
  if (Index >= 0) and (Index < Length(ObjectArray)) then
    result := (ObjectArray[Index] as TSinglePolygon)
  else
    result := nil;
end;



procedure TPolygonGroup.SetSinglePolygon(Index: longint; const Value: TSinglePolygon);
begin
  if (Index >= 0) and (Index < Length(ObjectArray)) then
    ObjectArray[Index] := Value;
end;



function TPolygonGroup.IndexByID(PolygonID: int64): longint;
var i: integer;
begin
  result := -1;

  for i := 0 to length(ObjectArray) -1 do
    if assigned(ObjectArray[i]) then
      if ObjectArray[i].UID = PolygonID then
      begin
        result := i;
        break;
      end;

end;


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////


procedure RecreateMetaLists(
                                    Container  : TMetaData;
                                    PointRL    : TResolverList;
                                    PolygonRL  : TResolverList;
                                    PntGroupRL : TResolverList;
                                    PlyGroupRL : TResolverList );
var
  idx      : longint;
  childidx : longint;
begin

  if (not assigned(Container))  or
     (not assigned(PointRL))    or
     (not assigned(PolygonRL))  or
     (not assigned(PntGroupRL)) or
     (not assigned(PlyGroupRL)) then
  begin
    Application.MessageBox( 'Error: one or more objects are not assigned',
                            'GLWDef.RecreateMetaLists' , 0 );
  end;

  try

  PointRL.Clear;
  PolygonRL.Clear;
  PntGroupRL.Clear;
  PlyGroupRL.Clear;

  for idx := 0  to Length(Container.Entries)-1 do
  begin
    with Container.Entries[idx] do
    begin
      if obj is TSinglePoint   then   PointRL.Append( obj, (obj as TVectorObject).UID );

      if obj is TSinglePolygon then PolygonRL.Append( obj, (obj as TVectorObject).UID );

      if obj is TPolygonGroup  then
      with (obj as TPolygonGroup) do
      begin
        PlyGroupRL.Append( obj, (obj as TVectorObject).UID );
        for childidx := 0 to Count - 1 do
          PolygonRL.Append(Items[childidx], Items[childidx].UID);
      end;

      if obj is TPointGroup  then
      with (obj as TPointGroup) do
      begin
        PntGroupRL.Append( obj, (obj as TVectorObject).UID );
        for childidx := 0 to Count - 1 do
          PointRL.Append(Items[childidx], Items[childidx].UID);
      end;

    end; // for
  end;

  PointRL.SortByID;
  PolygonRL.SortByID;
  PlyGroupRL.SortByID;
  PntGroupRL.SortByID;

  except
    Application.MessageBox('Error during execution of RecreateMetaLists','GLWDef.RecreateMetaLists',0);
  end;
end;



procedure RecreateMetaListByClass(
                                    Container    : TMetaData;
                                    List         : TResolverList;
                                    TReferenceClass : TClass;
                                    ByLoadedID   : Boolean = false;
                                    ClearAndSort : Boolean = true );
var
  idx      : longint;
//  childidx : longint;
  IDForList: int64;

  procedure ScanThroughObjectGroup(
                g: TVectorObjectGroup;
                RefClass: TClass;
                l: TResolverList);
  var
    childidx: longint;
  begin
    for childidx := 0 to g.Count - 1 do
    try
      if g.Items[childidx] is RefClass then
      begin
        case ByLoadedID of
         true  : l.Append(g.Items[childidx], g.Items[childidx].IDAsLoaded);
         false : l.Append(g.Items[childidx], g.Items[childidx].UID);
        end;
      end
      else if g.Items[childidx] is TVectorObjectGroup then
        ScanThroughObjectGroup(g,RefClass,l);
      except
        // lalala
      end;
  end;

begin
  if (not assigned(List)) or
     (not assigned(Container)) then exit;
  

  if ClearAndSort then List.Clear;

  for idx := 0  to Container.Count - 1 do
  begin
    with Container.Entries[idx] do
    begin

      if obj is TReferenceClass then
      begin
        try

           if obj is TVectorObject then
           begin
             case ByLoadedID of
               true : IDForList := (obj as TVectorObject).IDAsLoaded;
               false: IDForList := (obj as TVectorObject).UID;
             end;
           end
           else
             IDForList := GetNewUID;

           List.Append(obj, IDForList);
        except
          Application.MessageBox(
            PChar('Error : Container.Entries['+IntToStr(idx)+'] reference invalid.'),
            'RecreateMetaListByClass Error',MB_ICONERROR or MB_OK);
        end;
      end;

      if obj is TVectorObjectGroup then
        ScanThroughObjectGroup((obj as TVectorObjectGroup),TReferenceClass,List);

    end;
  end;

  if ClearAndSort then List.SortByID;
end;




////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
///  select a set of Menu Item Options for each Class of
///  objects attached to tree nodes, then Popup the menu
///  with only the appropriate items visible:


{const
  // tag MenuOption Flags, one for each menu item
  moCut           = $00000001;
  moDelete        = $00000002;
  moNewPolyGroup  = $00000004;
  moNewPointGroup = $00000008;
  moNewPoly       = $00000010;
  moNewPoint      = $00000020;
  moEdit          = $00000040;
  moPaste         = $00000080;
  moFlush         = $00000100;
  moLoadContainer = $00000200;
  moSaveContainer = $00000400;

  // pre-defined sets of flags suitable for each class
  moTMetaData     = moNewPolyGroup or moNewPointGroup or moEdit or moFlush or moLoadContainer or moSaveContainer;
  moTPointGroup   = moNewPoint or moEdit or moFlush or moDelete;
  moTPolyGroup    = moNewPoly or moEdit or moFlush or moDelete;
  moTSinglePoly   = moEdit or moDelete;
  moTSinglePoint  = moEdit or moDelete; }


procedure AdjustMenuItemPermissions(menu: TPopupMenu;          // popup menu object
                                    obj: TObject;                   // TObject(TreeNode.Data)
                                    AdditionalFlags: Cardinal = 0); // e.g. cut, paste if available
var
    moPermissions : Cardinal;
    itemidx       : Longint;
begin
  moPermissions := AdditionalFlags;

  // retrieve permission set for the respective class
  //
  with obj do
  begin
    if ClassType = TMetaData      then moPermissions := moTMetaData;
    if ClassType = TPolygonGroup  then moPermissions := moTPolyGroup;
    if ClassType = TSinglePolygon then moPermissions := moTSinglePoly;
    if ClassType = TPointGroup    then moPermissions := moTPointGroup;
    if ClassType = TSinglePoint   then moPermissions := moTSinglePoint;
  end;

  // set visibility of menu items according to permissions
  //
  for itemidx := 0 to menu.Items.Count - 1 do
    menu.Items[itemidx].Visible :=
      (Cardinal(menu.Items[itemidx].Tag) and moPermissions) <> 0;
end;


procedure TreeViewRightClick(tree: TTreeView; menu:TPopupMenu; X,Y: integer);
var
  p: TPoint;
begin
  Windows.GetCursorPos(p);

  // add "select node" on right-click action
  //
  if tree.GetNodeAt(X,Y) <> nil then tree.Selected := tree.GetNodeAt(X,Y);

  // if a node is selected and its data is assigned,
  // show menu according to classtype of TObject(Data)
  //
  if assigned(tree.Selected) then if assigned(tree.Selected.Data) then
  begin
    AdjustMenuItemPermissions(menu, tree.Selected.Data);
    if tree.SelectionCount <> 0 then menu.Popup(p.X, p.Y);
  end;

end;

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////



// explode all PointGroups into PointGroup-Objects
procedure ConvertContainerBeforeImport(containernode: iXMLNode);
var
  idx, subidx : longint;
begin
  for idx := containernode.ChildNodes.Count-1 downto 0 do
  begin
    if containernode.ChildNodes[idx].NodeName = XMLTAG_POINTGROUPS then
    begin
      for subidx := containernode.ChildNodes[idx].ChildNodes.Count -1 downto 0 do
        containernode.ChildNodes.Insert(  -1, containernode.ChildNodes[idx].ChildNodes[subidx] );
      Containernode.ChildNodes.Delete(idx);
    end;
  end;
end;



// pack all PointGroup-Objects into one PointGroups-object
procedure ConvertContainerForExport(containernode: iXMLNode);
var
  pgs : iXMLNode;
  idx : longint;
begin
  pgs := containernode.AddChild( XMLTAG_POINTGROUPS );
  for idx := containernode.ChildNodes.Count-1 downto 0 do
  begin
    if containernode.ChildNodes[idx].NodeName = XMLTAG_POINTGROUP then
      pgs.ChildNodes.Insert(-1,containernode.ChildNodes[idx]);
  end;
end;



procedure StringToXMLNodes(parent: iXMLNode; str: String);
  var
    dummydoc    : TXMLDocument;
    dummysource : TStringStream;
    buffer      : String;
    primary     : iXMLNode;
    OneNode     : iXMLNode;
    idx         : integer;
begin
    if str = '' then exit;

    // wrap content into dummy document
    buffer := '<?xml version="1.0"?>'#13#10'<sheet>'+str+'</sheet>';
    dummysource := TStringStream.Create(buffer);
    dummysource.Seek(0,soFromBeginning);

    // use import functionality from this instance
    dummydoc := TXMLDocument.Create(Application);
    dummydoc.LoadFromStream(dummysource); // feed the instance
    dummydoc.Active := True;              // and activate it

    primary := dummydoc.Node.ChildNodes[1];    // just [1]: <sheet>...</sheet>

    if assigned(primary) then
    begin
      // clone nodes, insert
      for idx := 0 to primary.ChildNodes.Count -1 do
      begin
        OneNode := primary.ChildNodes[idx].CloneNode(true);
        parent.ChildNodes.Insert(-1,OneNode);
      end;
    end;

    // clean up
    dummydoc.Active := False;
    dummydoc.Destroy; // not sure if this is valid (what does Active=False do?)
    dummysource.Destroy;
end;



initialization
  UID_NEW := 1;
  InitializeCriticalSection(CSEC);
finalization
  DeleteCriticalSection(CSEC);
end.
