unit GLMFilterFileProcessor;

interface
uses
   Windows, Classes, SysUtils, Forms, Math,Dialogs, ShellAPI,
   VisualOverlayClass, GraphicLayerManager, GLWFilters, GLWDef;


function BatchFilterFiles(
              filenames: TStrings;
              ReferenceLayerID : longint;
              progressbar: TVisualOverlayProgressBar;
              RemoteGLM : TGraphicLayerManager): string;

function BatchReSaveFiles( filenames: TStrings;
              progressbar : TVisualOverlayProgressBar;
              RemoteGLM : TGraphicLayerManager ):string;


implementation




function MakeUnusedFilename(fn: string):string;
  var
    cnt : int64;
    ext : string;
begin
    cnt := 1;
    result := fn;
    if not fileexists(fn) then exit;

    ext := ExtractFileExt(fn);
    repeat
      result := ChangeFileExt(fn, '('+IntToStr(cnt)+')'+ext);
      inc(cnt);
    until not fileexists(result);
end;



function BatchFilterFiles( filenames: TStrings;
              ReferenceLayerID : longint;
              progressbar : TVisualOverlayProgressBar;
              RemoteGLM : TGraphicLayerManager ):string;
var
  FileIdx, FilterIdx : longint;
  RefIdx, PLIdx, MIdx : longint; // reference layer index, processing layer index
  CurrentLayerID, ProcessingLayerID : longint;
  outfn, outpath, outext: string;
  save : TSaveDialog;
  safetytag: string;
  OutExtAsSource : Boolean;

begin

  if not assigned(filenames) then exit;
  if filenames.Count = 0     then exit;
  if not assigned(RemoteGLM) then exit;
  RefIdx := RemoteGLM.LayerByID(ReferenceLayerID); // assign
  if RefIdx < 0 then exit;
  if RemoteGLM.Layers[RefIdx].FilterHeapCursor < 0 then exit;

  CurrentLayerID := -1;
  if RemoteGLM.CurrentLayer >=0 then
    CurrentLayerID := RemoteGLM.Layers[RemoteGLM.CurrentLayer].LayerID;


  save := TSaveDialog.Create(Application.MainForm);

  save.Options := [ofPathMustExist, ofEnableSizing];
  save.Filter := 'same format as source|*|'+
                 '24 bit bitmap (*.bmp)|*.bmp|'+
                 '32 bit bitmap (*.bmp)|*.bmp|'+
                 'JPG image (*.jpg)|*.jpg|'+
                 'PNG image (*.png)|*.png|';

  save.FileName := ExtractFilePath(filenames[0])+'... filename will be ignored ...';
  save.DefaultExt := '';

  if not save.Execute then
  begin
    Application.MessageBox('Save action could not be completed.','GLMFilterFileProcessor.BatchFilterFiles',MB_ICONERROR);
    save.destroy;
    exit; // abort batch
  end;

  outpath := ExtractFilePath(save.FileName);
  result := outpath;
  if outpath = ExtractFilePath(FileNames[0]) then
    safetytag := '_filtered'
  else
    safetytag := '';

  case save.FilterIndex of
    1 : outext := '';
    2 : outext := '.bmp';
    3 : outext := '.b32';
    4 : outext := '.jpg';
    5 : outext := '.png';
  else
    outext := '';
  end;

  OutExtAsSource := (outext= '');

  save.Destroy;


  if assigned(progressbar) then
  begin
    progressbar.max := filenames.Count;
    progressbar.position := 0;
  end;

  for FileIdx := 0 to filenames.Count - 1 do
  begin
    if assigned(progressbar) then
    begin
      progressbar.position := FileIdx;
      progressbar.Caption := 'processing image(s)  '+inttostr(FileIdx+1)+'/'+inttostr(filenames.Count)+' ...';
      RemoteGLM.ClientPaintLocally(progressbar);
    end;

    // processing starts here
    //

    PLIdx := RemoteGLM.NewLayerLoadFromFile(0,0,filenames.Strings[FileIdx]); // assign
    if PLIdx > -1 then // loading successful
    begin
      ProcessingLayerID := RemoteGLM.Layers[PLIdx].LayerID; // assign

      RemoteGLM.ClientPaint(nil);
      Application.ProcessMessages;

      RefIdx := RemoteGLM.LayerByID(ReferenceLayerID); // ensure integrity
      for FilterIdx := 0 to min( RemoteGLM.Layers[RefIdx].FilterHeapCursor,
                                 length(RemoteGLM.Layers[RefIdx].FilterHeap )) do
      begin
        PLIdx := RemoteGLM.LayerByID(ProcessingLayerID); // ensure integrity
        ExecuteFilterAction( RemoteGLM.Layers[PLIdx].data,
                             RemoteGLM.Layers[RefIdx].Filterheap[FilterIdx] );
        RemoteGLM.ClientPaint(nil);
        RefIdx := RemoteGLM.LayerByID(ReferenceLayerID); // ensure integrity
      end;

      if OutExtAsSource then
        outext := ExtractFileExt(filenames.Strings[FileIdx]);

      outfn := outpath +
               ChangeFileExt( ExtractfileName(filenames.Strings[FileIdx]) ,
                              safetytag + outext );

      outfn := MakeUnusedFilename(outfn);
      RemoteGLM.SaveLayerToFile(ProcessingLayerID, outfn, outext);

      PLIdx := RemoteGLM.LayerByID(ProcessingLayerID); // ensure integrity
      MIdx  := RemoteGLM.LayerByID(RemoteGLM.Layers[PLIdx].MaskLayerID); // assign
      RemoteGLM.DeleteLayer(PLIdx); // will normally be deleted
      RemoteGLM.DeleteLayer(MIdx);  // will be deleted if MIdx > -1
    end;

    //
    // processing done
  end;

  if assigned(progressbar) then
  begin
    progressbar.position := FileIdx;
    progressbar.Caption := 'processing image(s)  '+inttostr(filenames.Count)+'/'+inttostr(filenames.Count)+' ...';
    RemoteGLM.ClientPaintLocally(progressbar);
    sleep(200);
  end;

  if RemoteGLM.LayerByID(CurrentLayerID) >= 0 then
    RemoteGLM.FocusLayer(RemoteGLM.LayerByID(CurrentLayerID))
  else
    if length(RemoteGLM.Layers) > 0 then
      RemoteGLM.FocusLayer( length(RemoteGLM.Layers) - 1);


end;



function BatchReSaveFiles( filenames: TStrings;
              progressbar : TVisualOverlayProgressBar;
              RemoteGLM : TGraphicLayerManager ):string;
var
  FileIdx, FilterIdx : longint;
  PLIdx, MIdx : longint; // reference layer index, processing layer index
  CurrentLayerID, ProcessingLayerID : longint;
  outfn, outpath, outext: string;
  OutExtAsSource : Boolean;
  save : TSaveDialog;
  safetytag: string;
begin

  if not assigned(filenames) then exit;
  if filenames.Count = 0     then exit;
  if not assigned(RemoteGLM) then exit;

  CurrentLayerID := -1;
  if (RemoteGLM.CurrentLayer >=0) and
     (Length(RemoteGLM.Layers)>0) then
         CurrentLayerID := RemoteGLM.Layers[RemoteGLM.CurrentLayer].LayerID;


  save := TSaveDialog.Create(Application.MainForm);

  save.Options := [ofPathMustExist, ofEnableSizing];
  save.Filter := 'same format as source|*|'+
                 '24 bit bitmap (*.bmp)|*.bmp|'+
                 '32 bit bitmap (*.bmp)|*.bmp|'+
                 'JPG image (*.jpg)|*.jpg|'+
                 'PNG image (*.png)|*.png|';

  save.FileName := ExtractFilePath(filenames[0])+'... filename will be ignored ...';
  save.DefaultExt := '';

  if not save.Execute then
  begin
    save.destroy;
    exit; // abort batch
  end;

  outpath := ExtractFilePath(save.FileName);
  result := outpath;

  if outpath = ExtractFilePath(FileNames[0]) then
    safetytag := '(1)'
  else
    safetytag := '';

  case save.FilterIndex of
    1 : outext := '';
    2 : outext := '.bmp';
    3 : outext := '.b32';
    4 : outext := '.jpg';
    5 : outext := '.png';
  else
    outext := '';
  end;

  OutExtAsSource := (outext = '');

  save.Destroy;

  
  if assigned(progressbar) then
  begin
    progressbar.max := filenames.Count;
    progressbar.position := 0;
  end;

  for FileIdx := 0 to filenames.Count - 1 do
  begin

    if assigned(progressbar) then
    begin
      progressbar.position := FileIdx;
      progressbar.Caption := 'processing image(s)  '+inttostr(FileIdx+1)+'/'+inttostr(filenames.Count)+' ...';
    end;

    // processing starts here
    //
    remoteGLM.surpressrepaint;
    PLIdx := RemoteGLM.NewLayerLoadFromFile(0,0,filenames.Strings[FileIdx]); // assign

    if PLIdx > -1 then // loading successful
    begin
      ProcessingLayerID := RemoteGLM.Layers[PLIdx].LayerID; // assign

      Application.ProcessMessages;
      RemoteGLM.ClientPaint(Application.Mainform);


      if OutExtAsSource then
        outext := ExtractFileExt(filenames.Strings[FileIdx]);

      outfn := outpath +
               ChangeFileExt( ExtractfileName(filenames.Strings[FileIdx]) ,
                              safetytag + outext );

      outfn := MakeUnusedFilename(outfn);

      //Application.MessageBox(PChar(outfn),'save file to:');
      RemoteGLM.SaveLayerToFile(ProcessingLayerID, outfn, outext);

      PLIdx := RemoteGLM.LayerByID(ProcessingLayerID); // ensure integrity
      MIdx  := RemoteGLM.LayerByID(RemoteGLM.Layers[PLIdx].MaskLayerID); // assign
      RemoteGLM.DeleteLayer(PLIdx); // will normally be deleted
      RemoteGLM.DeleteLayer(MIdx);  // will be deleted if MIdx > -1
    end;

    //
    // processing done
  end;

  if assigned(progressbar) then
  begin
    progressbar.position := FileIdx;
    progressbar.Caption := 'processing image(s)  '+inttostr(filenames.Count)+'/'+inttostr(filenames.Count)+' ...';
    RemoteGLM.ClientPaintLocally(progressbar);
    sleep(200);
  end;

  if RemoteGLM.LayerByID(CurrentLayerID) >= 0 then
    RemoteGLM.FocusLayer(RemoteGLM.LayerByID(CurrentLayerID))
  else
    if length(RemoteGLM.Layers) > 0 then
      RemoteGLM.FocusLayer( length(RemoteGLM.Layers) - 1);

end;




end.
