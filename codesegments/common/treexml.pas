unit treexml;

interface
uses
  Windows, XMLDoc, XMLIntf, ComCtrls, Classes, StdCtrls, SysUtils;


type TNodeStruct = packed record
  hasnameattr: Boolean;
  nameattr : PChar;
  localname: PChar;
  hastext  : Boolean;
  text     : PChar;
  ref      : Array[0..9] of Pointer;
  events   : Array[0..9] of TNotifyEvent;
end;
  PNodeStruct = ^TNodeStruct;

type  VRetrieveValueByPath = function(TN: TTreeNode; var return: string;
          const PathStringArray: PChar): Boolean; cdecl varargs;
      VRetrieveNodeByPath = function(TN: TTreeNode; var ReturnNode: TTreeNode;
          const PathStringArray: PChar): Boolean; cdecl varargs;
var
  RetrieveValueByPath : VRetrieveValueByPath;
  RetrieveNodeByPath  : VRetrieveNodeByPath;


Procedure SetString(var PCharStr: PChar; src: string);
procedure DisposeNodeStruct(var PNS: PNodeStruct);
procedure _DisposeNodeStruct(PNS: Pointer);
procedure DeleteChildStructs(Parent: TTreeNode);

procedure GrowTreeFromNode(xmlsrc: IXMLNodeList;treeview: TTreeView;parentnode: TTreeNode);
procedure CreateTreeFromXMLStrings(treeview: TTreeView; xml: TMemo);
procedure CreateXMLFromTree(treeview: TTreeView; xml: TMemo);

implementation



procedure DisposeNodeStruct(var PNS: PNodeStruct);
begin
  if not assigned(PNS) then exit;
  StrDispose(PNS^.nameattr);  PNS^.nameattr  := nil;
  StrDispose(PNS^.localname); PNS^.localname := nil;
  StrDispose(PNS^.text);      PNS^.text      := nil;
  FreeMem(PNS,sizeof(TNodeStruct));
end;

procedure _DisposeNodeStruct(PNS: Pointer);
var
  PPNS: PNodeStruct absolute PNS;
begin
  DisposeNodeStruct(PPNS);
  // cannot set PNS = nil here, this is the caller's job
end;



Procedure SetString(var PCharStr: PChar; src: string);
begin
  if assigned(PCharStr) then StrDispose(PCharStr);
  PCharStr := StrAlloc(length(src)+1);
  StrPLCopy(PCharStr, src, length(src));
end;



procedure GrowTreeFromNode(xmlsrc: IXMLNodeList;treeview: TTreeView;parentnode: TTreeNode);
var
  i: integer;
  tn: TTreeNode;

  procedure FillDataStruct(var node: TTreeNode;xmlnode: IXMLNode);
  var st: PNodeStruct;
  begin
    node.Data := GetMemory(sizeof(TNodeStruct));
    ZeroMemory(node.Data,sizeof(TNodeStruct));
    st := node.Data;

    st^.hasnameattr := xmlnode.HasAttribute('name');
    if st^.hasnameattr
      then SetString(st^.nameattr, xmlnode.Attributes['name'])
      else st^.nameattr := nil;

    SetString(st^.localname, xmlnode.LocalName);

    st^.hastext := xmlnode.IsTextElement;
    if st^.hastext
      then SetString(st^.Text, xmlnode.Text)
      else st^.Text := nil;
  end;
begin
  if parentnode = nil then
  begin
    for i := 0 to xmlsrc.Count -1 do
     if xmlsrc.Nodes[i].LocalName <> '' then
     begin
      if xmlsrc.Nodes[i].HasAttribute('name')
        then tn := treeview.Items.Add(nil,xmlsrc.Nodes[i].Attributes['name'])
        else tn := treeview.Items.Add(nil,xmlsrc.Nodes[i].LocalName);
      FillDataStruct(tn, xmlsrc.Nodes[i]);
      if xmlsrc.Nodes[i].HasChildNodes
        then GrowTreeFromNode(xmlsrc.Nodes[i].ChildNodes,treeview,tn);
     end;
  end
  else
  begin
    for i := 0 to xmlsrc.Count -1 do
    begin
      if xmlsrc.Nodes[i].IsTextElement then
      begin
        tn := treeview.Items.AddChild(parentnode,xmlsrc.Nodes[i].LocalName+'='+xmlsrc.Nodes[i].Text);
        FillDataStruct(tn, xmlsrc.Nodes[i]);
      end
      else
      begin
        if xmlsrc.Nodes[i].HasAttribute('name')
          then tn := treeview.Items.AddChild(parentnode,xmlsrc.Nodes[i].Attributes['name'])
          else tn := treeview.Items.AddChild(parentnode,xmlsrc.Nodes[i].LocalName);
        FillDataStruct(tn, xmlsrc.Nodes[i]);

        if xmlsrc.Nodes[i].HasChildNodes
          then GrowTreeFromNode(xmlsrc.Nodes[i].ChildNodes,treeview,tn);
      end;
    end;
  end;
end;



procedure CreateTreeFromXMLStrings(treeview: TTreeView; xml: TMemo);
var
  s: TMemoryStream;
  XMLDoc : IXMLDocument;
begin
  s := TMemoryStream.Create;
  xml.Lines.SaveToStream(s);
  XMLDoc := newXMLDocument;
  XMLDoc.LoadFromStream(s);
  XMLDoc.Active := true;

  treeview.Items.Clear;
  GrowTreeFromNode(XMLDoc.ChildNodes[0].ChildNodes,TreeView,nil);
  XMLDoc.Active := false;
  s.Free;
end;



// if you find a way to forward a parameter list with a-priori unknown size,
// please simplify this procedure by using _RetrieveNodeByPath
//
function _RetrieveValueByPath(TN: TTreeNode; var return: string;
          const PathStrings: PChar):Boolean; cdecl;
var
  patharray : Array[0..15] of PChar absolute PathStrings; // 16 levels should be enough, really
  i,k: integer;
  foundsubnode: Boolean;
  localname, pathname: string;
begin
  result := false;
  if @return = nil then exit;
  if TN = nil then exit;

  for i := 0 to 15 do
  begin
    // MessageBox(0,PChar(),nil,0);
    if patharray[i] = nil then // seek zero-termination
      break
    else
    begin
      foundsubnode := false;
      for k := 0 to TN.Count -1 do
      begin
        if TN.Item[k].Data <> nil then
        begin
          localname := PNodeStruct(TN.Item[k].Data)^.localname; lowercase(localname);
          pathname  := PChar(patharray[i]); pathname := lowercase(pathname);
          if localname = pathname then
          begin
            TN := TN.Item[k];
            foundsubnode := true;
            break;
          end; // if
        end; // if
      end;// for

      if not foundsubnode then
        exit; // the tree path ends prematurely
    end;
  end; // for

  if TN.Data <> nil then
  begin
    return := PNodeStruct(TN.Data)^.Text;
    result := true; // if we got this far, it worked.
  end;
end;



function _RetrieveNodeByPath(TN: TTreeNode; var returnnode: TTreeNode;
          const PathStrings: PChar):Boolean; cdecl;
var
  patharray : Array[0..255] of PChar absolute PathStrings; // 256 levels should be enough, really
  i,k: integer;
  foundsubnode: Boolean;
begin
  result := false;
  if @returnnode = nil then exit; // variable needs to be assigned
  if TN = nil then exit;
  ReturnNode := nil;

  for i := 0 to 255 do
    if patharray[i] = nil then // seek zero-termination
      break
    else
    begin
      foundsubnode := false;
      for k := 0 to TN.Count -1 do
        if TN.Item[k].Data <> nil then
          if lowercase(PNodeStruct(TN.Item[k].Data)^.localname) = lowercase(patharray[i]) then
          begin
            TN := TN.Item[k];
            foundsubnode := true;
            break;
          end;
      if not foundsubnode then exit; // the tree path ends prematurely
    end;

  if TN.Data <> nil then
  begin
    returnnode := TN;
    result := true; // if we got this far, it worked.
  end;
end;


procedure CreateXMLFromTree(treeview: TTreeView; xml: TMemo);
var
  i: integer;

 function TagStr(tn: TTreeNode): string;
 begin
   result := 'node';
   if assigned(tn) then
     if assigned(tn.Data) then
     begin
       if assigned(PNodeStruct(tn.Data)^.localname) then
         result := PNodeStruct(tn.Data)^.localname;
     end;
 end;

 function TagAttrStr(tn: TTreeNode): string;
 begin
   result := '';
   if assigned(tn) then
     if assigned(tn.Data) then
     begin
       if assigned(PNodeStruct(tn.Data)^.nameattr) then
         if PNodeStruct(tn.Data)^.hasnameattr then
           result := ' name="'+PNodeStruct(tn.Data)^.nameattr+'" ';
     end;
 end;

  function GetTimeStamp: string; // be independent, include the function here
  var
    h,m,s,ms: word;
  begin
    DecodeTime(time,h,m,s,ms);
    result := ' '+DateTimeToStr(Date()) + Format(' [%2d:%2d:%2d-%4d] ',[h,m,s,ms]);
  end;

  procedure bla(s: string; tabs: string = ''); begin xml.Lines.Add(tabs + '<'+s+'>'); end;

  procedure TranslateSubNodes(tn: TTreeNode; tabs: string);
  var
    i: integer;
    hastext: boolean;
  begin
    tabs := tabs + #9; // go one level deeper
    if not tn.HasChildren then exit;

    for i := 0 to tn.count -1 do
    begin
      hastext := false;
      if assigned(tn.Item[i].Data) then
        if assigned(PNodeStruct(tn.Item[i].Data)^.text) then
          hastext := PNodeStruct(tn.Item[i].Data)^.hastext;
      if not hastext then
      begin
        bla(TagStr(tn.Item[i])+TagAttrStr(tn.Item[i]),tabs);
        translatesubnodes(tn.Item[i],tabs);
        bla('/'+TagStr(tn.Item[i]),tabs);
      end
      else
        bla(  TagStr(tn.Item[i])+TagAttrStr(tn.Item[i])+
              '>'+PNodeStruct(tn.Item[i].Data)^.text+'</' +
              TagStr(tn.Item[i])  ,tabs);
    end;
  end;

begin

  xml.Lines.clear;
  bla('xmlsheet');
  xml.Lines.Add('');
  xml.Lines.Add('<!-- xml lens correction database - exported by GraphicLW v1 '+GetTimeStamp+'-->'#13#10);

  for i := 0 to treeview.Items.Count-1 do
    if treeview.Items[i].Parent = nil then // for all top-most nodes do
    begin
      bla(TagStr(treeview.Items[i])+ TagAttrStr(treeview.Items[i]),#9);
      TranslateSubNodes(treeview.Items[i],#9);
      bla('/'+TagStr(treeview.Items[i]) ,#9);
      xml.Lines.Add(#13#10);
    end;
  bla('/xmlsheet');
end;



procedure DeleteChildStructs(Parent: TTreeNode);
var
  i : integer;
begin
  for i := 0 to Parent.Count-1 do
  begin
    _DisposeNodeStruct(Parent.Item[i].Data);
    Parent.Item[i].Data := nil;
    DeleteChildStructs(Parent.Item[i]);
  end;
end;




initialization
  RetrieveValueByPath := Addr(_RetrieveValueByPath);
  RetrieveNodeByPath  := Addr(_RetrieveNodeByPath);
end.
