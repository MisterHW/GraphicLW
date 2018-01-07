unit OpenSystem;

interface

function GetParamStr(P: PChar; var Param: string): PChar;
function SetFileExt(s: string; NewExt: string): string;
function WinExecAndWait32(FileName:String; CommandLineStr: string;
            Visibility : integer; CreateConsole: Boolean = false):integer;
function ShellExecuteOpenAs(const fn: string): Boolean;
function ShellExecuteOpen(const fn: string; const app: string): Boolean;

const
  {$EXTERNALSYM SEE_MASK_NOASYNC}
  SEE_MASK_NOASYNC = $00000100;

implementation

uses
  windows, sysutils, ShellAPI, forms;


{$IFDEF MSWINDOWS}

function GetParamStr(P: PChar; var Param: string): PChar; // Delphi library function
var
  i, Len: Integer;
  Start, S, Q: PChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      P := CharNext(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNext(P);
        Inc(Len, Q - P);
        P := Q;
      end;
      if P[0] <> #0 then
        P := CharNext(P);
    end
    else
    begin
      Q := CharNext(P);
      Inc(Len, Q - P);
      P := Q;
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNext(P);
        while P < Q do
        begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then P := CharNext(P);
    end
    else
    begin
      Q := CharNext(P);
      while P < Q do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result := P;
end;
{$ENDIF}


function SetFileExt(s: string; NewExt: string): string;
var
  FileNameLen: integer;
begin
  s := ExtractFileName(s);
  result := ExtractFilePath(s);
  FileNameLen := length(s) - length(ExtractFileExt(s)) + 1;
  if Pos('.',s) > 1 then dec(FileNameLen);
  result := result + Copy(s,1,FileNameLen) + '.' + NewExt;
end;


function WinExecAndWait32(FileName:String; CommandLineStr: string;
            Visibility : integer; CreateConsole: Boolean = false):integer;
var
  zAppName:array[0..1023] of char;
  zCommandLine:array[0..1023] of char;
  zCurDir:array[0..512] of char;
  WorkDir:String;
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
  CreationFlags: Cardinal;
    function GetExitCodeProcess_(hProcess: THandle;
                               var lpExitCode: integer): Longbool;
    var
      ExitCode: Cardinal absolute lpExitCode;
    begin
      result := GetExitCodeProcess(hProcess,ExitCode);
    end;
begin

  StrPCopy(zAppName,FileName);
  StrPCopy(zCommandLine,CommandLineStr);
  GetDir(0,WorkDir);
  StrPCopy(zCurDir,WorkDir);
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  CreationFlags := NORMAL_PRIORITY_CLASS;
  if CreateConsole then CreationFlags := CreationFlags or CREATE_NEW_CONSOLE;
  if not CreateProcess(
    zAppName,                      { application name }
    zCommandLine,                  { pointer to command line string }
    nil,                           { pointer to process security attributes }
    nil,                           { pointer to thread security attributes }
    false,                         { handle inheritance flag }
    CreationFlags,                 { creation flags }
    nil,                           { pointer to new environment block }
    nil,                           { pointer to current directory name }
    StartupInfo,                   { pointer to STARTUPINFO }
    ProcessInfo) then Result := -1 { pointer to PROCESS_INF }
  else begin
    WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
    GetExitCodeProcess_(ProcessInfo.hProcess,Result);
  end;
end;


function FindWindowOfProcess(ProcessID: THandle): THandle;
var
  hIt: THandle;
  pid: DWord;
  pcreatorid: DWord;
begin
  result := INVALID_HANDLE_VALUE;
  hIt := Windows.FindWindow(nil,nil);

  while hIt <> 0 do
  begin
    if GetParent(hIt) = 0 then // top-level window
    begin
      pcreatorid := GetWindowThreadProcessId(hIt,pid); // test pid
      if ProcessID = pid then
      begin
        result := hIt;
        break;
      end;
    end;
  end;
end;


function ShellExecuteOpenAs(const fn: string): Boolean;
var
  SEInfo : TShellExecuteInfo;
begin
  System.FillChar(SEInfo,SizeOf(TShellExecuteInfo),#0);
  with SEInfo do begin
    cbSize := SizeOf(TShellExecuteInfo);
    fMask := SEE_MASK_DOENVSUBST or // replace environment variables
             // SEE_MASK_NOASYNC or // use if in a separate thread that manages a 'finsihed' event, pointless!
             SEE_MASK_FLAG_NO_UI;   // don't show errors
    lpFile := 'rundll32';
    lpParameters := PChar('shell32.dll,OpenAs_RunDLL '+fn);
    lpVerb := nil;
    nShow := SW_SHOWNORMAL;
  end;
  Result := ShellExecuteEx(@SEInfo);
end;



function ShellExecuteOpen(const fn: string; const app: string): Boolean;
var
  SEInfo : TShellExecuteInfo;
begin
  System.FillChar(SEInfo,SizeOf(TShellExecuteInfo),#0);

  with SEInfo do begin
    cbSize := SizeOf(TShellExecuteInfo);
    fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI;
    lpFile := PChar(app);
    lpParameters := PChar('"'+fn+'"');
    lpVerb := 'open';
    nShow := SW_SHOWNORMAL;
  end;

  Result := ShellExecuteEx(@SEInfo);
end;





end.
 