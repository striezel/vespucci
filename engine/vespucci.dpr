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
  TheGUI.KeyFunc(Key, x,y);
end;

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
  WriteLn('Creating GUI...');
  TheGUI:= TGui.Create;
  WriteLn('glutInit...');
  glutInit(@argc, argv);
  WriteLn('glutInitDisplayMode...');
  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);
  
  WriteLn('glutInitWindowPosition');
  glutInitWindowPosition(0,0);
  WriteLn('glutInitWindowSize');
  glutInitWindowSize(32*x_Fields+BarWidth, 32*y_Fields+16+16);

  WriteLn('glutCreateWindow');
  if (glutCreateWindow(cWindowCaption)<=0) then
  begin
    WriteLn('ERROR: Could not create window.');
    Exit;
  end;
  //functions
  WriteLn('glutFuncs (callback)');
  glutDisplayFunc(@DrawWrapper);
  glutReshapeFunc(@ResizeWrapper);
  glutKeyboardFunc(@KeyWrapper);
  glutIdleFunc(@IdleWrapper);
  
  WriteLn('Starting GUI...');
  TheGui.Start;//starts GUI ang GLUT's main loop
end.