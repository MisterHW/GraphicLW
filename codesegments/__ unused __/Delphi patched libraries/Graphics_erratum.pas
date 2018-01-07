unit Graphics;




procedure TBitmap.ReadDIB(Stream: TStream; ImageSize: LongWord; bmf: PBitmapFileHeader);
const
  DIBPalSizes: array [Boolean] of Byte = (sizeof(TRGBQuad), sizeof(TRGBTriple));
var
  DC, MemDC: HDC;
  BitsMem: Pointer;
  OS2Header: TBitmapCoreHeader;
  BitmapInfo: PBitmapInfo;
  ColorTable: Pointer;
  HeaderSize: Integer;
  OS2Format: Boolean;
  BMHandle, OldBMP: HBITMAP;
  DIB: TDIBSection;
  Pal, OldPal: HPalette;
  RLEStream: TStream;
  vbmf: TBitmapFileHeader;
{$IFDEF LINUX}
  I: Integer;
{$ENDIF}
begin
  Pal := 0;
  BMHandle := 0;
  RLEStream := nil;
  Stream.Read(HeaderSize, sizeof(HeaderSize));
  OS2Format := HeaderSize = sizeof(OS2Header);
  if OS2Format then HeaderSize := sizeof(TBitmapInfoHeader);
  GetMem(BitmapInfo, HeaderSize + 12 + 256 * sizeof(TRGBQuad));
  with BitmapInfo^ do
  try
    try
      if OS2Format then  // convert OS2 DIB to Win DIB
      begin
        Stream.Read(Pointer(Longint(@OS2Header) + sizeof(HeaderSize))^,
          sizeof(OS2Header) - sizeof(HeaderSize));
        FillChar(bmiHeader, sizeof(bmiHeader), 0);
        with bmiHeader, OS2Header do
        begin
          biWidth := bcWidth;
          biHeight := bcHeight;
          biPlanes := bcPlanes;
          biBitCount := bcBitCount;
        end;
        Dec(ImageSize, sizeof(OS2Header));
      end
      else
      begin // support bitmap headers larger than TBitmapInfoHeader
        Stream.Read(Pointer(Longint(BitmapInfo) + sizeof(HeaderSize))^,
          HeaderSize - sizeof(HeaderSize));
        Dec(ImageSize, HeaderSize);

        if (bmiHeader.biCompression <> BI_BITFIELDS) and
          (bmiHeader.biCompression <> BI_RGB) then
        begin // Preserve funky non-DIB data (like RLE) until modified
          RLEStream := TMemoryStream.Create;
          // source stream could be unidirectional.  don't reverse seek
          if bmf = nil then
          begin
            FillChar(vbmf, sizeof(vbmf), 0);
            vbmf.bfType := $4D42;
            vbmf.bfSize := ImageSize + Cardinal(HeaderSize);
            bmf := @vbmf;
          end;
          RLEStream.Write(bmf^, sizeof(bmf^));
          RLEStream.Write(HeaderSize, sizeof(HeaderSize));
          RLEStream.Write(Pointer(Longint(BitmapInfo) + sizeof(HeaderSize))^,
            HeaderSize - sizeof(HeaderSize));
          RLEStream.CopyFrom(Stream, ImageSize);
          { Cast ImageSize (long word) to integer to avoid integer overflow when negating. }
          RLEStream.Seek(-Integer(ImageSize), soFromEnd);
          Stream := RLEStream;  // the rest of the proc reads from RLEStream
        end;
      end;

      with bmiHeader do
      begin
        biSize := HeaderSize;
        ColorTable := Pointer(Longint(BitmapInfo) + HeaderSize);

        { check number of planes. DIBs must be 1 color plane (packed pixels) }
        if biPlanes <> 1 then InvalidBitmap;

        // 3 DWORD color element bit masks (ie 888 or 565) can precede colors
        // TBitmapInfoHeader sucessors include these masks in the headersize
        if (HeaderSize = sizeof(TBitmapInfoHeader)) and
          ((biBitCount = 16) or (biBitCount = 32)) and
          (biCompression = BI_BITFIELDS) then
        begin
          Stream.ReadBuffer(ColorTable^, 3 * sizeof(DWORD));
          Inc(Longint(ColorTable), 3 * sizeof(DWORD));
          Dec(ImageSize, 3 * sizeof(DWORD));
        end;

        // Read the color palette
        if biClrUsed = 0 then
          biClrUsed := GetDInColors(biBitCount);
        Stream.ReadBuffer(ColorTable^, biClrUsed * DIBPalSizes[OS2Format]);
        Dec(ImageSize, biClrUsed * DIBPalSizes[OS2Format]);

        // biSizeImage can be zero. If zero, compute the size.
        if biSizeImage = 0 then            // top-down DIBs have negative height
          biSizeImage := BytesPerScanLine(biWidth, biBitCount, 32) * Abs(biHeight);

        if biSizeImage < ImageSize then ImageSize := biSizeImage;
      end;

      { convert OS2 color table to DIB color table }
      if OS2Format then RGBTripleToQuad(ColorTable^);

      DC := GDICheck(GetDC(0));
      try
        if ((bmiHeader.biCompression <> BI_RGB) and
          (bmiHeader.biCompression <> BI_BITFIELDS)) or DDBsOnly then
        begin
          MemDC := 0;
          GetMem(BitsMem, ImageSize);
          try
            Stream.ReadBuffer(BitsMem^, ImageSize);
            MemDC := GDICheck(CreateCompatibleDC(DC));
            OldBMP := SelectObject(MemDC, CreateCompatibleBitmap(DC, 1, 1));
            OldPal := 0;
            if bmiHeader.biClrUsed > 0 then
            begin
              Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);
              OldPal := SelectPalette(MemDC, Pal, False);
              RealizePalette(MemDC);
            end;

            try
              BMHandle := CreateDIBitmap(MemDC, BitmapInfo^.bmiHeader, CBM_INIT, BitsMem,
                BitmapInfo^, DIB_RGB_COLORS);
              if (BMHandle = 0) then
                if GetLastError = 0 then InvalidBitmap else RaiseLastOSError;
            finally
              if OldPal <> 0 then
                SelectPalette(MemDC, OldPal, True);
              DeleteObject(SelectObject(MemDC, OldBMP));
            end;
          finally
            if MemDC <> 0 then DeleteDC(MemDC);
            FreeMem(BitsMem);
          end;
        end
        else
        begin
          BMHandle := CreateDIBSection(DC, BitmapInfo^, DIB_RGB_COLORS, BitsMem, 0, 0);
          if (BMHandle = 0) or (BitsMem = nil) then
            if GetLastError = 0 then InvalidBitmap else RaiseLastOSError;

          try
{$IFDEF LINUX}
            // I need to pre-touch the memory in 4096 byte increments to ensure
            // the read will succeed. WINE marks this memory as not present to
            // catch when we make changes to it. If we read directly into it
            // Linux will (correctly) terminate the read with a failure since an
            // exception occured during the read. We need to make sure these
            // exceptions are triggered in user space instead of kernel.
            for I := 1 to (ImageSize + 4095) div 4096 do
              PByteArray(BitsMem)^[(I - 1) * 4096] := 0;
{$ENDIF}

            // (h.w.2011-01-14): I noticed that the BitsMem size returned by
            // the system routine CreateDIBSection(...) is the exact
            // amount of memory needed for the image so any accidental padding
            // after the image data block will cause an unhandled stream read
            // error because ImageSize is too large.
            // for uncompressed images, it is easy to recalculate the size
            with BitmapInfo^.bmiHeader do
            if (biCompression = BI_RGB) then
            begin
              ImageSize := ((( biWidth * biBitCount + 7 ) div 8 + 3 ) div 4 ) * 4
                             * biHeight;
            end;
            Stream.ReadBuffer(BitsMem^, ImageSize);
          except
            DeleteObject(BMHandle);
            raise;
          end;
        end;
      finally
        ReleaseDC(0, DC);
      end;
      // Hi-color DIBs don't preserve color table, so create palette now
      if (bmiHeader.biBitCount > 8) and (bmiHeader.biClrUsed > 0) and (Pal = 0)then
        Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);

      FillChar(DIB, sizeof(DIB), 0);
      GetObject(BMHandle, Sizeof(DIB), @DIB);
      // GetObject / CreateDIBSection don't preserve these info values
      DIB.dsBmih.biXPelsPerMeter := bmiHeader.biXPelsPerMeter;
      DIB.dsBmih.biYPelsPerMeter := bmiHeader.biYPelsPerMeter;
      DIB.dsBmih.biClrUsed := bmiHeader.biClrUsed;
      DIB.dsBmih.biClrImportant := bmiHeader.biClrImportant;
    except
      RLEStream.Free;
      raise;
    end;
  finally
    FreeMem(BitmapInfo);
  end;
  NewImage(BMHandle, Pal, DIB, OS2Format, RLEStream);
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TBitmap.ReadStream(Stream: TStream; Size: Longint);
var
  Bmf: TBitmapFileHeader;
  DIB: TDIBSection;
begin
  FreeContext;
  if Size = 0 then
  begin
    FillChar(DIB, sizeof(DIB), 0);
    NewImage(0, 0, DIB, False);
  end
  else
  begin
    Stream.ReadBuffer(Bmf, sizeof(Bmf));
    if Bmf.bfType <> $4D42 then InvalidBitmap; // accept only "BM" windows bitmaps
    ReadDIB(Stream, Size - sizeof(Bmf), @Bmf);
  end;
end;

