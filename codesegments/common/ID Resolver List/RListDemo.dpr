program RListDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  RList in 'RList.pas';

function GetTickCount():Cardinal; stdcall; external 'kernel32.dll';

var
  list: TResolverList;
  i: integer;
  stamp, timediff: longint;
  thisID: longint;
begin
  list := TResolverList.Create;

  randomize;


  List.Clear;

  for i  := 0 to 20 do
    List.Append(Pointer(i),round(1000 * random));

  for i  := 0 to 20 do
    writeln(List.Items[i].ID);

  readln;

  List.SortByID;

  for i  := 0 to 20 do
    writeln(List.Items[i].ID);

  readln;


  writeln('speed test (ID) - 1E+05 elements');

  List.Clear;

  for i  := 0 to 100000 do
    List.Append(Pointer(i),i);


  stamp := GetTickCount;
  for i  := 1 to 200000 do
  begin
    thisID :=  round(random*100000);
    List.IndexByID(thisID);
  end;
  timediff := longint(GetTickCount)-stamp;
  writeln('Lookup by ID: ',timediff,' ms');
  writeln('speed test (non-indexed) - 1E+05 elements');


  stamp := GetTickCount;
  for i  := 1 to 200000 do
  begin
    thisID :=  round(random*100000);
    List.IndexByObj(Pointer(thisID));
  end;
  timediff := longint(GetTickCount)-stamp;
  writeln('Lookup (non-indexed): ',timediff,' ms');
  readln;


  list.Destroy;
end.
