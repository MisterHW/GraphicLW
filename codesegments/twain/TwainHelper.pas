unit TwainHelper;

interface
uses
  Windows, Forms, Types, DelphiTwain, TWAIN,
  SysUtils;

type
  {redeclare Transfer mode for twain, typecast later}
  TTwainTransferMode = (ttmFile, ttmNative, ttmMemory);

var
  MainTwainSource: integer;
  MainTwainSourceAcquired: Boolean;
  MainTwainObject: TDelphiTwain;
  TransferModeChoice: TTwainTransferMode;

procedure SelectMainTwainSource;
procedure StealthImport(CallbackEvent: TOnTwainAcquire);
procedure ImportWithUI(CallbackEvent: TOnTwainAcquire);
procedure FinishAcquisition;
procedure Terminate;



implementation



procedure SelectMainTwainSource;
var
  UserSelectedSource: integer;
begin
  if not assigned(MainTwainObject) then
  begin
    MainTwainObject := TDelphiTwain.Create(Application.MainForm);
  end;

  if MainTwainObject.LoadLibrary then
  begin
    MainTwainObject.SourceManagerLoaded := TRUE; // execute dialog
    UserSelectedSource := MainTwainObject.SelectSource;

    if UserSelectedSource <> -1 then
    begin
      MainTwainSourceAcquired := true;
      MainTwainSource := UserSelectedSource;
    end;

    Application.MainForm.SetFocus;
  end
  else
  begin
    Application.MessageBox( 'Twain is not installed.','TWAIN source selection failed', MB_ICONWARNING or MB_OK);
  end;
end;



procedure Terminate;
var
  i: integer;
begin
  try
    if assigned(MainTwainObject) then
    begin
      for i := 0 to MainTwainObject.SourceCount -1 do
      begin
        MainTwainObject.Source[i].Enabled := FALSE;
        MainTwainObject.Source[i].UnloadSource;
      end;
      MainTwainObject.UnloadSourceManager(true);
      MainTwainObject.Destroy;
    end;
  except // catch any access violation that might have
         // occured because it's time to terminate anyway.
  else
  end;
end;



procedure ImportWithUI(CallbackEvent: TOnTwainAcquire);
begin
  // check source selection
  if not MainTwainSourceAcquired then SelectMainTwainSource;
  // try again
  if not MainTwainSourceAcquired then exit;

  if assigned(CallbackEvent) then
  begin
    MainTwainObject.OnTwainAcquire := CallbackEvent;
  end;

  with MainTwainObject.Source[MainTwainSource] do
  begin
    if Loaded then UnloadSource;
    Loaded  := TRUE;
    ShowUI  := TRUE;
    TransferMode := DelphiTwain.TTwainTransferMode(TransferModeChoice);
    Enabled := TRUE;
  end;

  Application.ProcessMessages;
end;



procedure StealthImport(CallbackEvent: TOnTwainAcquire);
begin
  // check source selection
  if not MainTwainSourceAcquired then SelectMainTwainSource;
  // try again
  if not MainTwainSourceAcquired then exit;

  if assigned(CallbackEvent) then
  begin
    MainTwainObject.OnTwainAcquire := CallbackEvent;
  end;

  with MainTwainObject.Source[MainTwainSource] do
  begin
    if Loaded then UnloadSource;
    Loaded  := TRUE;
    TransferMode := DelphiTwain.TTwainTransferMode(TransferModeChoice);
    ShowUI  := FALSE; {don't show UI}
    Enabled := TRUE;
  end;
end;


procedure FinishAcquisition;
begin
  if not MainTwainSourceAcquired then exit;
  if MainTwainSource <> -1 then
  begin
    // MainTwainObject.Source[MainTwainSource].DisableSource; 
    // do not disable, this is being done after acquisition

    // do not unload here, TWAIN sample application will hang up. For compatibility
    // with devices that need reloading such as the Creative LiveCam Vista,
    // unload will be performed during import.
    // for streaming, forget about the unloading and enjoy the illusion of
    // good performance.
  end;
end;



initialization
  MainTwainObject := nil;
  MainTwainSource := -1;
  MainTwainSourceAcquired := False;
  TransferModeChoice := ttmNative;
end.
