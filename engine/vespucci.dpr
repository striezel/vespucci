{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010  Thoronador

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ***************************************************************************
}

{$mode objfpc}
program vespucci;

uses Gui, GLUT;

var TheGUI: TGui;
    Frames: LongWord;
    ShowFrames, ShowKey, ShowMouse: Boolean;

procedure DrawWrapper; cdecl;
begin
  TheGUI.Draw;
  Frames:= Frames+1;
end;

procedure KeyWrapper(Key: Byte; x, y: LongInt); cdecl;
begin
  if ShowKey then WriteLn('Key: ', Key);
  TheGUI.KeyFunc(Key, {x,y,} False);
end;

procedure SpecialWrapper(Key: Longint; x, y: LongInt); cdecl;
begin
  if ShowKey then WriteLn('Special key: ', Key);
  TheGUI.KeyFunc(Key, {x, y,} True);
end;

procedure MouseWrapper(button, state, x, y: LongInt); cdecl;
begin
  if ShowMouse then
  begin
    case button of
      GLUT_LEFT_BUTTON: Write('LeftMouse');
      GLUT_RIGHT_BUTTON: Write('RightMouse');
      GLUT_MIDDLE_BUTTON: Write('MiddleMouse');
    else Write('UnknownMouse(', button, ')');
    end;//case
    if state=GLUT_UP then Write(' up') else Write(' down');
    WriteLn(' x: ',x, '; y: ', y);
  end;//if
  TheGui.MouseFunc(button, state, x, y);
end;//proc

procedure MouseMoveWrapper(x,y: LongInt); cdecl;
begin
  if ShowMouse then WriteLn('Mouse moved. x: ',x,'; y: ', y);
  TheGui.MouseMoveFunc(x,y);
end;//proc

procedure ResizeWrapper(Width, Height: LongInt); cdecl;
begin
  TheGUI.Resize(Width, Height);
end;//proc

procedure IdleWrapper; cdecl;
begin
  TheGUI.Draw;
  Frames:= Frames+1;
end;

procedure TimerWrapper(value: LongInt); cdecl;
begin
  glutTimerFunc(1000, @TimerWrapper, 0);
  WriteLn('Frames: ', Frames);
  Frames:= 0;
end;//proc

procedure ShowHelp;
begin
  WriteLn('Vespucci: help on command line parameters');
  WriteLn();
  WriteLn('--frames: displays framerate (FPS) every second (disabled by default)');
  WriteLn('--no-frames: explicitly tells vespucci NOT to display FPS');
  WriteLn('--mouse: displays debug info on mouse movement and clicks (disabled by default)');
  WriteLn('--keys: displays debug info on pressed keys (disabled by default)');
  WriteLn('--no-keys: explicitly tells vespucci NOT to display key info');
  WriteLn('--help : displays this help');
end;//proc

begin
  Frames:= 0;
  ShowFrames:= False;
  ShowMouse:= False;
  ShowKey:= False;
  WriteLn(Gui.cWindowCaption);
  WriteLn;
  //check for parameters
  for Frames:= 1 to ParamCount do
    if ParamStr(Frames)='--help' then
    begin
      ShowHelp;
      Exit;
    end
    else if ParamStr(Frames)='--no-frames' then ShowFrames:= False
    else if ParamStr(Frames)='--frames' then ShowFrames:= True
    else if ParamStr(Frames)='--mouse' then ShowMouse:= True
    else if ParamStr(Frames)='--no-keys' then ShowKey:= False
    else if ParamStr(Frames)='--keys' then ShowKey:= True
    else begin
      WriteLn('Unrecognised parameter: "',ParamStr(Frames),'"');
      WriteLn('Use "--help" to show possible command line parameters.');
      Exit;
    end;//else
  Frames:= 0;

  WriteLn('glutInit...');
  glutInit(@argc, argv);
  WriteLn('glutInitDisplayMode...');
  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);

  WriteLn('glutInitWindowPosition...');
  glutInitWindowPosition(0,0);
  WriteLn('glutInitWindowSize...');
  glutInitWindowSize(cWindowWidth, cWindowHeight);

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
  glutMotionFunc(@MouseMoveWrapper);
  glutPassiveMotionFunc(@MouseMoveWrapper);
  glutIdleFunc(@IdleWrapper);

  if (ShowFrames) then glutTimerFunc(1000, @TimerWrapper, 0);

  WriteLn('Starting GUI...');
  TheGui.Start;//starts GUI and GLUT's main loop
end.