unit FileSystemWatch;

interface
uses
  Windows,Classes;

{NotifyFilter:
  FILE_NOTIFY_CHANGE_FILE_NAME
  FILE_NOTIFY_CHANGE_DIR_NAME
  FILE_NOTIFY_CHANGE_ATTRIBUTES
  FILE_NOTIFY_CHANGE_SIZE
  FILE_NOTIFY_CHANGE_LAST_WRITE
  FILE_NOTIFY_CHANGE_SECURITY

example:
  TDirectoryWatch.Create(
    pathname,
    FILE_NOTIFY_CHANGE_LAST_WRITE,
    OnFileChange);
}

// File system watch object that sends a notification event if a filtered event
// has occured such as "LAST_WRITE" of a file in the specified directory has
// changed. In this case, the application can respond by updating its representation
// of the file or let the user decide whether to apply or decline changes.
//
Type TDirectoryWatch = class(TThread)
    constructor create(path: string; NotifyFilter: Cardinal;
        Callback: TNotifyEvent = nil);
    destructor Destroy; override;
  protected
    procedure execute; override;
  private
    path: string;
    filter: Cardinal;
    Callback: TNotifyEvent;
    EventObjs: Array[0..2] of THandle;
  public
    data: array[0..255] of byte;
    longintdata: longint;
    ptr: Pointer;
    str: string;
    procedure Release;
end;


implementation

const
  KILL_EVENT   = 0; // array indices and constants at the same time
  CHANGE_EVENT = 1;
  SLEEP_EVENT  = 2;
  CALL_RATE_MILLISECS = 500; // change notification dead-time (ms)
  RETRY_TIMEOUT = 200; // ms
  MAXIMUM_RETRIES = 5;

procedure TDirectoryWatch.Execute;
var
  waitresult: Cardinal;
  last_call: longint;
  hFile: THandle;
  RetryCount: integer;
begin
  last_call := 0;
  RetryCount := 0;

  // register for this file change notification
  EventObjs[CHANGE_EVENT] := Windows.FindFirstChangeNotificationA(
        PChar(self.path),
        FALSE,
        self.filter);

  if EventObjs[CHANGE_EVENT] <> INVALID_HANDLE_VALUE then // registration successful
  begin
    while true do // loop
    begin
      waitresult := WaitForMultipleObjects( 2, @EventObjs[0], FALSE, infinite);
      case waitresult of
        WAIT_OBJECT_0 + CHANGE_EVENT :
          begin

            if assigned(self.Callback) and
               (Gettickcount- last_call > CALL_RATE_MILLISECS) then
            begin

              repeat // try to access file to validate its accessability and to flush buffers
                hFile := CreateFile( PChar(self.path),
                          GENERIC_READ,
                          FILE_SHARE_READ,
                          nil,
                          OPEN_EXISTING,
                          FILE_ATTRIBUTE_NORMAL or FILE_FLAG_NO_BUFFERING,
                          0);

                if hFile = INVALID_HANDLE_VALUE then
                  WaitForMultipleObjects(2, @EventObjs[1],FALSE,RETRY_TIMEOUT) // SLEEP_EVENT and KILL_EVENT!
                else
                  FlushFileBuffers(hFile);

                inc(RetryCount);
              until (hFile <> INVALID_HANDLE_VALUE) or (RetryCount >= MAXIMUM_RETRIES);

              if hFile <> INVALID_HANDLE_VALUE then CloseHandle(hFile);

              Callback(self);
              last_call := Gettickcount;
            end;

            if not FindNextChangeNotification(EventObjs[CHANGE_EVENT]) // request that we be notified on the next event, too
              then break;

          end;
        WAIT_TIMEOUT:;
        else // implies (WAIT_OBJECT_0 + KILL_EVENT)
          break;
      end; // case
    end; // while

    FindCloseChangeNotification(EventObjs[CHANGE_EVENT]);
  end;

  CloseHandle(EventObjs[KILL_EVENT]);
  self.Terminate;
end;



constructor TDirectoryWatch.Create(path: string; NotifyFilter: Cardinal;
        Callback: TNotifyEvent = nil);
begin
  inherited Create(true);
  self.path     := path;
  self.filter   := NotifyFilter;
  self.Callback := Callback;
  EventObjs[KILL_EVENT]  := CreateEvent(nil,false,false,nil);
  EventObjs[SLEEP_EVENT] := CreateEvent(nil,false,false,nil);
end;



procedure TDirectoryWatch.Release;
begin
  SetEvent(EventObjs[KILL_EVENT]);
  // might be called during own callback event, so don't
  // create a deadlock by self.WaitFor
end;



destructor TDirectoryWatch.Destroy;
var i: integer;
begin
  self.Release;

  if not self.Suspended then self.WaitFor;
  {for i := 0 to length(EventObjs)-1 do
    try
      CloseHandle(EventObjs[i]);
    except
    end;  }
  inherited Destroy;
end;



end.
