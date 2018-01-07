  panel1 := TVisualOverlayPanelEx.Create(form1.ClientRect, form1, buffer);
  panel1.Caption := '';
  panel1.Color := clwhite;
  panel1.BorderColor := clgray;
  panel1.SetBounds(rect(200,4,320+200,200+4),form1.ClientRect);
  voh.InsertVisualOverlayObject(panel1);
  panel1.Visible := true;

  ncpanel := TVisualOverlayPanel.Create(form1.ClientRect);
  ncpanel.SetBounds(rect(0,0,panel1.Width,20),form1.ClientRect);
  ncpanel.BorderColor := panel1.BorderColor;
  ncpanel.Color := $D0D0D0;
  panel1.InsertComponent(ncpanel);
  ncpanel.Visible := true;

  nccancel := TVisualOverlayButton.Create(form1.ClientRect);
  nccancel.Caption := 'X';
  nccancel.SetBounds(rect(2,2,20,18),form1.ClientRect);
  nccancel.Left := ncpanel.Width - nccancel.Width -2;
  panel1.InsertComponent(nccancel);
  nccancel.Visible := true;


  button1 := TVisualOverlayButton.Create(form1.ClientRect);
  button1.Caption := 'do nothing';
  button1.Top  := 36;
  button1.Left := 16;
  button1.Width := 110;
  panel1.InsertComponent(button1);
  button1.Visible := true;


  button2 := TVisualOverlayButton.Create(form1.ClientRect);
  button2.Caption := 'move window';
  button2.Top  := 76;
  button2.Left := 16;
  button2.Width := 110;
  panel1.InsertComponent(button2);
  button2.Visible := true;


  button3 := TVisualOverlayButton.Create(form1.ClientRect);
  button3.Caption := 'show margin';
  button3.Top  := 116;
  button3.Left := 16;
  button3.Width := 110;
  panel1.InsertComponent(button3);
  button3.Visible := true;


  deeplayer := TVisualOverlayPanelEx.Create(panel1.BoundsRect, panel1, buffer);
  deeplayer.Caption := '';
  deeplayer.Color := clwhite;
  deeplayer.BorderColor := clgray;
  deeplayer.SetBounds(rect(134,36,176+134,154+36),panel1.BoundsRect);
  panel1.InsertComponent(deeplayer);
  deeplayer.Visible := true;

  deepbutton := TVisualOverlayButton.Create(form1.ClientRect);
  deepbutton.Caption := 'child button';
  deepbutton.SetBounds(rect(24,16,110,24+16),form1.ClientRect);

  voh.InsertVisualOverlayObject(deepbutton);
  deepbutton.Visible := true;


  dummy := TVisualOverlayPanel.Create(form1.ClientRect);
  dummy.SetBounds(rect(400,400,700,600),form1.ClientRect);
  dummy.BorderColor := panel1.BorderColor;
  dummy.Color := $F0F0F0;
  voh.InsertVisualOverlayObject(dummy);
  dummy.Visible := true;
