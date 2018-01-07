unit NotificationNode;

interface

uses
  Classes;

  Type TNotificationNode = Class
    constructor Create;
    destructor Destroy; override;
  private
    heap: Array Of TNotifyEvent;
  public
    procedure InsertEvent(Event:TNotifyEvent);
    procedure RemoveEvent(Event:TNotifyEvent);
  published
    procedure SharedEvent(Sender: TObject);
  end;

implementation



constructor TNotificationNode.Create;
begin
  inherited;
  setlength(heap,0);
end;



destructor TNotificationNode.Destroy;
begin
  setlength(heap,0);
  inherited;
end;



procedure TNotificationNode.InsertEvent(Event:TNotifyEvent);
begin
  if not assigned(Event) then exit;
  RemoveEvent(Event);
  setlength(heap,length(heap)+1);
  heap[length(heap)-1] := Event;
end;



procedure TNotificationNode.RemoveEvent(Event:TNotifyEvent);
var
  i, j: integer;
begin
  for i := 0 to length(heap)-1 do
  if @Event = @heap[i] then
    begin
      for j := i to length(heap)-2 do heap[j] := heap[j+1];
      setlength(heap,length(heap)-1);
      break;
    end;
end;



procedure TNotificationNode.SharedEvent(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to length(heap)-1 do
  begin
    heap[i](Sender);
  end;
end;


end.
