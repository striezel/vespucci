{$mode objfpc}
program vespucci;

uses Gui, GLUT;

var TheGUI: TGui;

procedure DrawWrapper; cdecl;
begin
  TheGUI.Draw;
end;

procedure KeyWrapper(Key: Byte; x, y: Longint); cdecl;
begin
  WriteLn('Key: ', Key);
  TheGUI.KeyFunc(Key, x,y, False);
end;

procedure SpecialWrapper(Key: Longint; x, y: Longint); cdecl;
begin
  WriteLn('Special key: ', Key);
  TheGUI.KeyFunc(Key, x, y, True);
end;

procedure MouseWrapper(button, state, x, y: Longint); cdecl;
begin
  case button of
    GLUT_LEFT_BUTTON: Write('LeftMouse');
    GLUT_RIGHT_BUTTON: Write('RightMouse');
    GLUT_MIDDLE_BUTTON: Write('MiddleMouse');
  else Write('UnknownMouse(', button, ')');
  end;//case
  if state=GLUT_UP then Write(' up') else Write(' down');
  WriteLn(' x: ',x, '; y: ', y);
end;//proc

procedure ResizeWrapper(Width, Height: Longint); cdecl;
begin
  TheGUI.Resize(Width, Height);
end;//proc

procedure IdleWrapper; cdecl;
begin
  TheGUI.Draw;
end;

begin
  WriteLn(Gui.cWindowCaption);
  WriteLn;
  WriteLn('glutInit...');
  glutInit(@argc, argv);
  WriteLn('glutInitDisplayMode...');
  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);

  WriteLn('glutInitWindowPosition...');
  glutInitWindowPosition(0,0);
  WriteLn('glutInitWindowSize...');
  glutInitWindowSize(32*x_Fields+BarWidth, 32*y_Fields+16+16);

  WriteLn('glutCreateWindow...');
  if (glutCreateWindow(cWindowCaption)<=0) then
  begin
    WriteLn('ERROR: Could not create window.');
    Exit;
  end;
  WriteLn('Creating GUI...');
  TheGUI:= TGui.Create;
  //functions
  WriteLn('glutFuncs (callback)...');
  glutDisplayFunc(@DrawWrapper);
  glutReshapeFunc(@ResizeWrapper);
  glutKeyboardFunc(@KeyWrapper);
  glutSpecialFunc(@SpecialWrapper);
  glutMouseFunc(@MouseWrapper);
  glutIdleFunc(@IdleWrapper);

  WriteLn('Starting GUI...');
  TheGui.Start;//starts GUI ang GLUT's main loop
end.