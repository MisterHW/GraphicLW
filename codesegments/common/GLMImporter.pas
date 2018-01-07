unit GLMImporter;

{

  maybe pass a reserved location with the filename so that
  all images are sorted in correct order.

}

interface
uses
  Windows, Classes, SysUtils, Graphics;


type TProgressbarStruct = record
  Initialize : Boolean;
  Terminate  : Boolean;
  Min        : Longint;
  Pos        : Longint;
  Max        : Longint;
  Caption    : String;
  AdditionalText : String;
end;

Type TUpdateImporterProgressBar =
  procedure (Sender: TObject; var Struct: TProgressbarStruct) of object;


Type
     
TImageImportThread = class(TThread)
  constructor Create(CreateSuspended: Boolean); virtual;
  destructor Destroy; override;
private
  fn: string;
  procedure Execute; override;
public
  procedure AssignTask(Filename: String);
end;


TGLMImporter = class(TThread)
  Constructor Create(Callback: TUpdateImporterProgressBar); virtual;
  destructor Destroy; override;
private
  UpdateProc : TUpdateImporterProgressBar;
  pbs        : TProgressbarStruct;
  ThisRunAcceptedJobs : longint;
  ThisRunFinishedJobs : single;

  Threads    : Array of TImageImportThread;
  Filenames  : TStringList;
  Handles    : Array of THandle;
  procedure Execute; override;
  function PopFilename(var fn: String): Boolean;
public
  procedure Open(filename: String);
end;




implementation
var
 SystemInfo : TSystemInfo;
 InsertSec  : _RTL_CRITICAL_SECTION;
 PushPopSec : _RTL_CRITICAL_SECTION;


 
//////////////////////////////////////////////////////////////////////////
//     TImageImportThread
//////////////////////////////////////////////////////////////////////////



constructor TImageImportThread.Create;
begin
  inherited;
end;



destructor TImageImportThread.Destroy;
begin
  inherited;
end;



procedure TImageImportThread.Execute;
begin
  while not Self.Terminated do
  begin
    // execute action here
    //

    // go to sleep
    //
    Self.Suspend;
  end;
end;



procedure TImageImportThread.AssignTask(Filename: String);
begin
  if not Suspended then
  begin
    Windows.MessageBox(  0,
                         'Error in TImageImportThread.AssignTask : '#13#10+
                         'Job assigned while Worker Thread in action.',
                         'Invalid Job Assignment',
                         MB_ICONERROR or MB_OK );
    exit;
  end;

  fn := Filename;
  self.Resume;
end;



//////////////////////////////////////////////////////////////////////////
//     TGLMImporter
//////////////////////////////////////////////////////////////////////////



procedure TGLMImporter.Execute;
var
  idx: longint;
  NewFn: string;

  function GetAvailableThreadIdx : longint;
  var i : longint;
  begin
    result := -1;
    for i := 0 to Length(Threads) - 1 do
    begin
      if Threads[i].Suspended then
      begin
        result := i;
        break;
      end;
    end;
  end;

begin

  while not Terminated do
  begin
    if (ThisRunFinishedJobs = 0) and (ThisRunAcceptedJobs > 0) then
    begin
      pbs.Initialize := true;
      pbs.Terminate := false;
      pbs.Min := 0;
      pbs.Pos := 0;
      pbs.Max := ThisRunAcceptedJobs;
      pbs.Caption := 'loading image(s)  '+IntToStr(pbs.Pos)+'/'+inttostr(pbs.Max)+' ...';
      pbs.AdditionalText := '';
      if assigned(UpdateProc) then
        UpdateProc(self,pbs);
    end;

    while PopFilename(NewFn) do
    begin
      // get a Thread
      //
      idx := GetAvailableThreadIdx;
      if idx = -1 then
      begin
        WaitForMultipleObjects( Length(Threads), @Handles[0], false, 120000);
        idx := GetAvailableThreadIdx;
      end;

      // assign a task to it
      //
      Threads[idx].AssignTask(NewFn);
    end;

    Self.Suspend;
  end;

end;



constructor TGLMImporter.Create(Callback: TUpdateImporterProgressBar);
var i : integer;
begin
  inherited Create(true);

  UpdateProc := Callback;

  Filenames := TStringList.Create;

  SetLength(Threads, SystemInfo.dwNumberOfProcessors);
  SetLength(Handles, Length(Threads));
  for i := 0 to Length(Threads) - 1 do
  begin
     Threads[i] := TImageImportThread.Create(true);
     Handles[i] := Threads[i].Handle;
  end;

  ZeroMemory(@pbs,SizeOf(TProgressbarStruct));
  ThisRunAcceptedJobs := 0;
  ThisRunFinishedJobs := 0;

  Self.Resume;
end;



destructor TGLMImporter.Destroy;
var i : integer;
begin
  Self.Terminate;
  if Self.Suspended then Self.Resume;
  Self.WaitFor;

  for i := 0 to Length(Threads) - 1 do
  begin
     Threads[i].Terminate;
     if Threads[i].Suspended then
       Threads[i].Resume;
     Threads[i].WaitFor;
     Threads[i].Destroy;
  end;

  SetLength(Threads, 0);
  SetLength(Handles, 0);

  FileNames.Destroy;

  inherited;
end;



function TGLMImporter.PopFilename(var fn: String): Boolean;
begin
  result := Filenames.Count > 0;
  if not result then exit;

  EnterCriticalSection(PushPopSec);
  fn := Filenames.Strings[0];
  Filenames.Delete(0);
  LeaveCriticalSection(PushPopSec);
end;



procedure TGLMImporter.Open(filename: String);
begin
  if FileExists(Filename) then
  begin
    EnterCriticalSection(PushPopSec);
    Filenames.Add(filename);
    inc(ThisRunAcceptedJobs);
    LeaveCriticalSection(PushPopSec);

    if Self.Suspended then
      Self.Resume;
  end;
end;



//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////



initialization
  ZeroMemory(@SystemInfo, SizeOf(SystemInfo));
  Windows.GetSystemInfo(SystemInfo);
  InitializeCriticalSection(InsertSec);
  InitializeCriticalSection(PushPopSec);

finalization
  DeleteCriticalSection(InsertSec);
  DeleteCriticalSection(PushPopSec);


end.
