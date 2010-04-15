#include <iostream>
#include <string>
#include <GL/glut.h>
#include "GUI.h"
#include "PascalTypes.h"
#include "ParamStr.h"

TGui* TheGUI;
int Frames;
bool ShowFrames, ShowKey, ShowMouse;

void DrawWrapper()
{
  TheGUI->Draw();
  Frames = Frames+1;
}

void KeyWrapper(Byte Key, LongInt x, LongInt y)
{
  if (ShowKey) std::cout <<"Key: "<<(int)Key<<"\n";
  TheGUI->KeyFunc(Key, x,y, false);
}

void SpecialWrapper(LongInt Key, LongInt x, LongInt y)
{
  if (ShowKey) std::cout<<"Special key: "<<Key<<"\n";
  TheGUI->KeyFunc(Key, x, y, true);
}

void MouseWrapper(int button, int state, int x, int y)
{
  if (ShowMouse)
  {
    switch (button)
    {
      case GLUT_LEFT_BUTTON: std::cout <<"LeftMouse"; break;
      case GLUT_RIGHT_BUTTON: std::cout <<"RightMouse"; break;
      case GLUT_MIDDLE_BUTTON: std::cout <<"MiddleMouse"; break;
      default: std::cout <<"UnknownMouse("<<button<<")"; break;
    }//swi
    if (state==GLUT_UP) std::cout << " up"; else std::cout <<" down";
    std::cout << " x: "<<x<<"; y: "<<y<<"\n";
  }//if
  TheGUI->MouseFunc(button, state, x, y);
}//proc

void MouseMoveWrapper(int x, int y)
{
  if (ShowMouse) std::cout <<"Mouse moved. x: "<<x<<"; y: "<<y<<"\n";
  TheGUI->MouseMoveFunc(x,y);
}//proc

void ResizeWrapper(int Width, int Height)
{
  TheGUI->Resize(Width, Height);
}//proc

void IdleWrapper()
{
  TheGUI->Draw();
  Frames = Frames+1;
}

void TimerWrapper(int value)
{
  glutTimerFunc(1000, &TimerWrapper, 0);
  std::cout<<"Frames: "<<Frames<<"\n";
  Frames = 0;
}//proc

void ShowHelp()
{
  std::cout<<"Vespucci: help on command line parameters\n";
  std::cout<<"\n";
  std::cout<<"--frames: displays framerate (FPS) every second (disabled by default)\n";
  std::cout<<"--no-frames: explicitly tells vespucci NOT to display FPS\n";
  std::cout<<"--mouse: displays debug info on mouse movement and clicks (disabled by default)\n";
  std::cout<<"--keys: displays debug info on pressed keys (disabled by default)\n";
  std::cout<<"--no-keys: explicitly tells vespucci NOT to display key info\n";
  std::cout<<"--help : displays this help\n";
}//proc

int main(int argc, char **argv)
{
  std::cout << "Hello world!" << std::endl;
  Frames = 0;
  ShowFrames = false;
  ShowMouse = false;
  ShowKey = false;
  std::cout <<cWindowCaption<<"\n";
  std::cout << "\n";
  std::string Param;
  //check for parameters
  for (Frames= 1; Frames<argc; ++Frames)
  {
    Param = std::string(argv[Frames]);
    if (Param=="--help")
    {
      ShowHelp();
      return 0;
    }
    else if (Param=="--no-frames") ShowFrames = false;
    else if (Param=="--frames") ShowFrames = true;
    else if (Param=="--mouse") ShowMouse = true;
    else if (Param=="--no-keys") ShowKey = false;
    else if (Param=="--keys") ShowKey = true;
    else
    {
      std::cout << "Unrecognised parameter: \""<<Param<<"\"\n";
      std::cout << "Use \"--help\" to show possible command line parameters.\n";
      return 0;
    }//else
  }//for

  //push values fpr ParamStr()
  for (Frames=0; Frames<argc; ++Frames)
  {
    ParamBuffer::GetSingleton().push(std::string(argv[Frames]));
  }

  Frames = 0;

  std::cout << "glutInit...\n";
  glutInit(&argc, argv);
  std::cout << "glutInitDisplayMode...\n";
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);

  std::cout << "glutInitWindowPosition...\n";
  glutInitWindowPosition(0,0);
  std::cout << "glutInitWindowSize...\n";
  glutInitWindowSize(cWindowWidth, cWindowHeight);

  std::cout << "glutCreateWindow...\n";
  if (glutCreateWindow(cWindowCaption.c_str())<=0)
  {
    std::cout << "ERROR: Could not create window.\n";
    return 0;
  }
  std::cout << "Creating GUI...\n";
  TheGUI = new TGui;
  //functions
  std::cout << "glutFuncs (callback)...\n";
  glutDisplayFunc(&DrawWrapper);
  glutReshapeFunc(&ResizeWrapper);
  glutKeyboardFunc(&KeyWrapper);
  glutSpecialFunc(&SpecialWrapper);
  glutMouseFunc(&MouseWrapper);
  glutMotionFunc(&MouseMoveWrapper);
  glutPassiveMotionFunc(&MouseMoveWrapper);
  glutIdleFunc(&IdleWrapper);

  if (ShowFrames) glutTimerFunc(1000, &TimerWrapper, 0);

  std::cout << "Starting GUI...\n";
  TheGUI->Start();//starts GUI and GLUT's main loop
  return 0;
}
