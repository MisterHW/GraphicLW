Unit _main;

Interface

Uses
	SysUtils, Classes, Controls, Graphics, Forms, Dialogs, StdCtrls, Contnrs,
	FileCtrl, DelphiTwain;

Type
	TScanForm	= Class(TForm)
							CBGUI		: TCheckBox;
							BtnScan	: TButton;
							Procedure FormCreate(Sender: TObject);
							Procedure FormDestroy(Sender: TObject);
							Procedure BtnScanClick(Sender: TObject);
							Procedure TwainTwainAcquire(Sender: TObject;
								Const Index: Integer; Image: TBitmap; Var Cancel: Boolean);
							Procedure TwainAcquireCancel(Sender: TObject;
								Const Index: Integer);
							Procedure TwainSourceDisable(Sender: TObject;
								Const Index: Integer);
					  Private
							Twain		: TDelphiTwain;
							FScanner	: Integer;
							FDestDir	: String;
							FBilder	: TObjectList;
					  End;

Var
	ScanForm		: TScanForm;

Implementation

{$R *.dfm}

Procedure TScanForm.FormCreate(Sender: TObject);
Begin
	FDestDir:='';
	FBilder:=TObjectList.Create;						// Speicher für Bilder anlegen
	FBilder.OwnsObjects:=True;							// Speicher gehört der Variable

  Twain := TDelphiTwain.Create(Self);
	Twain.LoadLibrary;
	Twain.LoadSourceManager;
	Twain.TransferMode:=ttmNative;
	// <- ttmMemory ist extrem buggy und funktionierte bei mir nie!
  Twain.OnTwainAcquire  := TwainTwainAcquire;
  Twain.OnSourceDisable := TwainSourceDisable;
  Twain.OnAcquireCancel := TwainAcquireCancel;


	Case Twain.SourceCount Of
		0	:	Begin
					ShowMessage('Es ist kein Scanner angeschlossen.');
					Halt;
				End;
		1	:	FScanner:=0;
	Else		FScanner:=Twain.SelectSource;
	End;
End;

Procedure TScanForm.FormDestroy(Sender: TObject);
Begin
	Twain.UnloadSourceManager(True);					// Gerätemanager entladen
	Twain.UnloadLibrary;									// TWAIN entladen
	FBilder.Free;											// Bilderliste freigeben
End;

Procedure TScanForm.BtnScanClick(Sender: TObject);
Begin
	If SelectDirectory('Wählen Sie das Ziel-Verzeichnis für die Bilder aus:',
			FDestDir, FDestDir
			//,[sdNewFolder, sdShowEdit, sdShowShares, sdValidateDir], nil
      ) Then
	Try
		FDestDir:=IncludeTrailingPathDelimiter(FDestDir);

		Twain.Source[FScanner].ShowUI:=CBGUI.Checked;
		Twain.Source[FScanner].LoadSource;
		(* -> Platz für weitere Einstellungen
		Twain.Source[FScanner].SetIPixelType();
		Twain.Source[FScanner].SetIBitDepth();
		etc. pp. *)
		Twain.Source[FScanner].Enabled:=True;
	Except
		ShowMessage('Der Scanner konnte nicht konfiguriert/gestartet werden.');
	End;
End;

// Es wird ein Bild von der TWAIN-Software geliefert ...
Procedure TScanForm.TwainTwainAcquire(Sender: TObject; Const Index: Integer;
	Image: TBitmap; Var Cancel: Boolean);
Begin
	FBilder.Add(TBitmap.Create);						// Liste um ein Bild erweitern
	With TBitmap(FBilder.Last) Do						// mit diesem Bild arbeiten
	Begin
		Assign(Image);										// Bilddaten übernehmen
		(* ggf. bereits hier Änderungen vornehmen z.B.
		Case ScanJob.ABit Of
			1	:	PixelFormat:=pf1bit;					// Schwarzweiß machen
			4	:	PixelFormat:=pf4bit;					// 16 Graustufen
			8	:	PixelFormat:=pf8bit;					// 256 Graustufen
			15	:	PixelFormat:=pf15bit;				// 32K Farben
			16	:	PixelFormat:=pf16bit;				// 65K Farben
			24	:	PixelFormat:=pf24bit;				// 16.7 Mio Farbe
			32	:	PixelFormat:=pf32bit;				// Mit Alphakanal
		End;*)
	End;
End;

// Manche Scanner-Software entlädt den Treiber nicht von allein
// -> wir machen das daher zur Sicherheit von Hand und rufen das Speichern auf
Procedure TScanForm.TwainAcquireCancel(Sender: TObject; Const Index: Integer);
Begin
	Twain.Source[FScanner].UnloadSource;			// nötig: TWAIN entladen
	TwainSourceDisable(nil, FScanner);				// Speichern von Hand aufrufen
End;

// Speichern der Bilder
// Scannen beendet -> Twain löst dieses Ereignis NORMALER WEISE von allein aus
Procedure TScanForm.TwainSourceDisable(Sender: TObject; Const Index: Integer);
Var
	I				: Integer;
Begin
	If FBilder.Count = 0 Then Exit;					// keine Bilder zum Speichern da

	I:=1;														// mit dem 1. Bild beginnen
	While FBilder.Count > 0 Do
	Try
		(* ggf. auch erst hier Änderungen vornehmen ...
			oder z.B. in JPEG oder PNG umwandeln *)

		TBitmap(FBilder.Items[0]).SaveToFile(		// Bild speichern
			FDestDir + 'Bild_' + FormatFloat('####', I) + '.bmp');
    Application.MessageBox(PChar(FDestDir + 'Bild_' + FormatFloat('####', I) + '.bmp'),'saving image to...',0);
		FBilder.Delete(0);								// Bild löschen
		Inc(I);												// Bildzähler erhöhen
		Application.ProcessMessages;					// auch ohne Thread am Leben halten
	Except
	End;
End;

End.
