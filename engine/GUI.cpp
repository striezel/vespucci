#include "GUI.h"
#include <cctype>
#include <iostream>
#include "PascalFunctions.h"


int Ord(const TRiverType rt_val)
{
  return static_cast<int> (rt_val);
}

TRiverType Low(const TRiverType rt_val)
{
  return rtOne;
}

TRiverType High(const TRiverType rt_val)
{
  return rtThree;
}

std::string cTribeTexNames(const LongInt NationCount)
{
  switch (NationCount)
  {
    case cNationArawak: return "tents.bmp";
    case cNationAztec: return "aztec.bmp";
    case cNationInca: return "inca.bmp";
    case cNationTupi: return "tents.bmp";
    case cNationCherokee: return "tents.bmp";
    case cNationIroquois: return "tents.bmp";
    case cNationSioux: return "tents.bmp";
    case cNationApache: return "tents.bmp";
    default: throw 42; break;
  }
}

// **** TGui functions ****

TGui::TGui()
{
  #ifdef DEBUG_CODE
    std::cout<< "Entered TGui.Create\n";
  #endif
  //inherited Create;
  mouse.x = 0;
  mouse.y = 0;
  mouse.down = false;
  mouse.down_x = -1;
  mouse.down_y = -1;
  OffsetX = 0; OffsetY = 0;
  Wooden_Mode = true;
  MiniMapOffset_Y = 0;
  TData::GetSingleton().SetPlayerNation(cNationEngland);
  TUnit* Ship = TData::GetSingleton().NewUnit(utCaravel, TData::GetSingleton().PlayerNation(), 36, 13);
  std::cout <<"First caravel created.\n";
  TUnit* passenger = TData::GetSingleton().NewUnit(utColonist, TData::GetSingleton().PlayerNation(), 36, 13);
  passenger->GiveTools(100);
  Ship->LoadUnit(passenger);
  passenger = TData::GetSingleton().NewUnit(utRegular, TData::GetSingleton().PlayerNation(), 36, 13);
  Ship->LoadUnit(passenger);

  menu_cat = mcNone;
  selected_menu_option = 1;
  report = rtNone;
  cur_colony = NULL;
  ColonyBuildingPage = false;
  europe = NULL;
  focused = NULL;
  //message
  msg.txt = "";
  //SetLength(msg.options, 0);
  msg.options.clear();
  msg.selected_option =0;
  msg.inputCaption = "";
  msg.inputText = "";
  msg_queue.first = NULL;
  msg_queue.last = NULL;

  //set texture names to "empty" and then load them
  glEnable(GL_TEXTURE_2D);
  //terrain textures
  TArraySq32RGB tempTex;
  LongInt i;
  std::string err_str;
  for (i= Ord(Low(ttArctic)); i<=Ord(High(ttArctic)); ++i)
  {
    m_TerrainTexNames[TTerrainType(i)] = 0;
    if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+terrain_img_path+cTerrainTexNames[TTerrainType(i)], tempTex, err_str))
    {
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      glGenTextures(1, &m_TerrainTexNames[TTerrainType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[TTerrainType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, &(tempTex[0].r));
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    }//if
  }//for

  //river textures
  TArraySq32RGBA AlphaTex;
  for (i= Ord(Low(rtOne)); i<=Ord(High(rtOne)); ++i)
  {
    m_RiverTexNames[TRiverType(i)] = 0;
    if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+terrain_img_path+cRiverTexNames[TRiverType(i)], tempTex, err_str))
    {
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, &m_RiverTexNames[TRiverType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_RiverTexNames[TRiverType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    }//if
  }//for

  //good textures
  for (i= Ord(Low(gtFood)); i<=Ord(High(gtFood)); ++i)
  {
    m_GoodTexNames[TGoodType(i)] = 0;
    if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+good_img_path+cGoodTexNames[TGoodType(i)], tempTex, err_str))
    {
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, &m_GoodTexNames[TGoodType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[TGoodType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    }//if
  }//for
  //unit textures
  for (i= Ord(Low(utFarmer)); i<=Ord(High(utFarmer)); ++i)
  {
    m_UnitTexNames[TUnitType(i)] = 0;
    if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+unit_img_path+cUnitTexNames[TUnitType(i)], tempTex, err_str))
    {
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, &m_UnitTexNames[TUnitType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[TUnitType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    }//if
  }//for
  //unit state textures
  for (i= Ord(Low(usNormal)); i<=Ord(High(usNormal)); ++i)
  {
    m_StateTexNames[TUnitState(i)] = 0;
    if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+state_img_path+cStateTexNames[TUnitState(i)], tempTex, err_str))
    {
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, &m_StateTexNames[TUnitState(i)]);
      glBindTexture(GL_TEXTURE_2D, m_StateTexNames[TUnitState(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    }//if
  }//for

  //colony textures
  m_ColonyTexNames[0] = 0;
  if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+colony_img_path+cColonyTexNames[0], tempTex, err_str))
  {
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, &m_ColonyTexNames[0]);
    glBindTexture(GL_TEXTURE_2D, m_ColonyTexNames[0]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  }//if

  //building textures
  LongInt j;
  TArray128x64RGB BuildTex;
  TArray128x64RGBA AlphaBuildTex;
  for (i= Ord(Low(btNone)); i<=Ord(High(btNone)); ++i)
  {
    for (j= 1; j<=3; ++j)
    {
      m_BuildingTexNames[TBuildingType(i)][j] = 0;
      if (cBuildingTexNames[TBuildingType(i)][j]!="")
      {
        if (ReadBitmapToArr128x64RGB(TData::GetSingleton().GetPathBase()+building_img_path+cBuildingTexNames[TBuildingType(i)][j], BuildTex, err_str))
        {
          //change order of color components from blue, green, red (as in file) to
          //  red, green, blue (as needed for GL)
          SwapRGB_To_BGR(BuildTex);
          GetAlphaByColor(BuildTex, AlphaBuildTex);
          glGenTextures(1, &m_BuildingTexNames[TBuildingType(i)][j]);
          glBindTexture(GL_TEXTURE_2D, m_BuildingTexNames[TBuildingType(i)][j]);
          glTexImage2D(GL_TEXTURE_2D, 0, 4, 128, 64, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaBuildTex[0].r));
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        }//if
        else std::cout << "Error while reading bitmap \""<<TData::GetSingleton().GetPathBase()+building_img_path+cBuildingTexNames[TBuildingType(i)][j]
                     <<"\": "<<err_str<<"\n";
      }//if
    }//for
  }//for


  //tribe textures
  for (i= cMinIndian; i<=cMaxIndian; ++i)
    m_TribeTexNames[i] = 0;
  if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+tribe_img_path+cTribeTexNames(cNationCherokee), tempTex, err_str))
  {
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, &m_TribeTexNames[cNationCherokee]);
    glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[cNationCherokee]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    for (i= cMinIndian; i<=cMaxIndian; ++i)
      m_TribeTexNames[i] = m_TribeTexNames[cNationCherokee];
  }//if
  if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+tribe_img_path+cTribeTexNames(cNationAztec), tempTex, err_str))
  {
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, &m_TribeTexNames[cNationAztec]);
    glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[cNationAztec]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  }
  if (ReadBitmapToArr32RGB(TData::GetSingleton().GetPathBase()+tribe_img_path+cTribeTexNames(cNationInca), tempTex, err_str))
  {
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, &m_TribeTexNames[cNationInca]);
    glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[cNationInca]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  }

  //error texture
  m_ErrorTexName = 0;
  GetAlphaByColor(cErrorTex, AlphaTex);
  glGenTextures(1, &m_ErrorTexName);
  glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
  glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, &(AlphaTex[0].r));
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  //welcome message (German), originally for test reasons only
  ShowMessageSimple(
          "Willkommen bei Vespucci!                                    "
         +cSpace60
         +"Hinweise zur Steuerung:                                     "
         +"  Pfeiltasten bewegen eine Einheit/die Karte in die angege- "
         +"  bene Richtung. Zusaetzlich koennen auch die Ziffern 1,3,7 "
         +"  und 9 auf dem Nummernblock benutzt werden, um eine Einheit"
         +"  diagonal zu bewegen.                                      "
         +"  Leertaste beendet die aktuelle Runde, mit ESC wird das    "
         +"  Spiel beendet. Die Leertaste oder Enter kann auch genutzt "
         +"  werden, um diese Meldung verschwinden zu lassen.          "
         +cSpace60
         +"  Viele Sachen sind noch nicht implementiert, Vespucci be-  "
         +"  findet sich gerade am Anfang der Entwicklungsphase.");
  Wooden_Mode = false;
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.Create\n";
  #endif
}//constructor

TGui::~TGui()
{
  #ifdef DEBUG_CODE
    std::cout <<"Entered TGui.Destroy\n";
  #endif
  GLUTCallbacksToNil();
  //delete dat;
  //dat = NULL;
  //free textures
  LongInt i, j;
  for (i= Ord(Low(ttArctic)); i<=Ord(High(ttArctic)); ++i)
    if (m_TerrainTexNames[TTerrainType(i)]!= 0)
      glDeleteTextures(1, &m_TerrainTexNames[TTerrainType(i)]);
  for (i = Ord(Low(gtFood)); i<=Ord(High(gtFood)); ++i)
    if (m_GoodTexNames[TGoodType(i)]!= 0) glDeleteTextures(1, &m_GoodTexNames[TGoodType(i)]);
  for (i= Ord(Low(utFarmer)); i<=Ord(High(utFarmer)); ++i)
    if (m_UnitTexNames[TUnitType(i)]!= 0) glDeleteTextures(1, &m_UnitTexNames[TUnitType(i)]);
  for (i= Ord(Low(usNormal)); i<=Ord(High(usNormal)); ++i)
    if (m_StateTexNames[TUnitState(i)]!= 0) glDeleteTextures(1, &m_StateTexNames[TUnitState(i)]);
  if (m_ColonyTexNames[0]!= 0) glDeleteTextures(1, &m_ColonyTexNames[0]);
  for (i= Ord(Low(btNone)); i<= Ord(High(btNone)); ++i)
    for (j= 1; j<=3; ++j)
      if (m_BuildingTexNames[TBuildingType(i)][j]!= 0)
        glDeleteTextures(1, &m_BuildingTexNames[TBuildingType(i)][j]);
  for (i= cMinIndian; i<=cMaxIndian; ++i)
    if (m_TribeTexNames[i]!=0)
      glDeleteTextures(1, &m_TribeTexNames[i]);
  glDeleteTextures(1, &m_ErrorTexName);
  //inherited Destroy;
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.Destroy\n";
  #endif
}//destructor

void TGui::GLUTCallbacksToNil()
{
  //glutDisplayFunc(nil);
  glutReshapeFunc(NULL);
  glutKeyboardFunc(NULL);
  glutSpecialFunc(NULL);
  glutMouseFunc(NULL);
  glutMotionFunc(NULL);
  glutPassiveMotionFunc(NULL);
  glutIdleFunc(NULL);
}//proc

void TGui::KeyFunc(Byte Key, LongInt x, LongInt y, bool Special)
//var tempUnit: TUnit;
{
  #ifdef DEBUG_CODE
    std::cout<<"Entered TGui.KeyFunc\n";
  #endif
  //react on message
  if (msg.txt!="")
  {
    if (msg.inputCaption!="")
    {
      //process input
      switch (Key)
      {
        case KEY_BACKSPACE:
        case KEY_DELETE:
             msg.inputText = msg.inputText.substr(0, msg.inputText.length()-1);
             break;
        case KEY_RETURN:
        case KEY_ESCAPE:
             GetNextMessage();
             glutPostRedisplay();
             break;
        default:
          if ((Key>=KEY_SPACE) and !Special)
            msg.inputText = msg.inputText+static_cast<char>(Key);
          break;//case-default
      }//swi
      return; //better be safe than sorry ;)
    }//if input message
    switch (Key)
    {
      case KEY_RETURN:
      case KEY_ESCAPE:
      case KEY_SPACE:
           GetNextMessage();
           glutPostRedisplay();
           break;
    }//swi
    //we even got options here
    if (msg.options.size()>0)
    {
      switch (Key)
      {
        case GLUT_KEY_UP:
        case KEY_NUMPAD8:
             msg.selected_option = msg.selected_option -1;
             if (msg.selected_option<0) msg.selected_option = msg.options.size()-1;
             break;//case UP
        case GLUT_KEY_DOWN:
        case KEY_NUMPAD2:
             msg.selected_option = msg.selected_option +1;
             if (msg.selected_option>=msg.options.size()) msg.selected_option = 0;
             break;//case DOWN
      }//swi
    }//if options

    return;//to prevent other things, keys can do to your units. We have
           // a message window, so display it, until space is hit.
  }//if

  //keys if menu is active
  if (InMenu())
  {
    switch (Key)
    {
      case GLUT_KEY_UP:
      case KEY_NUMPAD8:
           selected_menu_option = selected_menu_option-1;
           if (selected_menu_option<1) selected_menu_option = TLanguage::GetSingleton().GetOptionCount(menu_cat);
           break;//case UP, 8
      case GLUT_KEY_DOWN:
      case KEY_NUMPAD2:
           selected_menu_option = selected_menu_option+1;
           if (selected_menu_option>TLanguage::GetSingleton().GetOptionCount(menu_cat)) selected_menu_option = 1;
           break;//case DOWN, 2
      case GLUT_KEY_LEFT:
      case KEY_NUMPAD4:
           if (menu_cat!=mcGame) menu_cat = Pred(menu_cat);
           else menu_cat = mcTrade;
           break;//left
      case GLUT_KEY_RIGHT:
      case KEY_NUMPAD6:
           if (menu_cat!=mcTrade) menu_cat = Succ(menu_cat);
           else menu_cat = mcGame;
           break;//right
      case KEY_ESCAPE:
           menu_cat = mcNone;
           break;
      case KEY_RETURN:
      case KEY_SPACE:
           HandleMenuSelection(menu_cat, selected_menu_option);
           menu_cat = mcNone;
           selected_menu_option =1;
           break;//Enter, Leertaste
    }//swi
    return; //better here ;)
  }//if InMenu

  TCallbackRec temp_cb;
  //"general" keys
  if (Key==KEY_ESCAPE)
  {
    if (InColony()) cur_colony = NULL;
    else if (InEurope()) europe = NULL;
    else if (InReport()) report = rtNone;
    else
    {
      temp_cb._type = CBT_EXIT;
      temp_cb.cbExit = CBF_Exit;
      ShowMessageOptions("Vespucci beenden?", ToStringArr("Nein", "Ja"), temp_cb);
    }//else
  }//if KEY_ESCAPE

  if (InReport())
  {
    switch (Key)
    {
      case KEY_RETURN:
      case KEY_SPACE:
           report = rtNone;
           break;
    }//swi
    return;
  }//if InReport

  if (InColony())
  {
    if (!Special and (toupper(char(Key))=='T')) ColonyBuildingPage = not ColonyBuildingPage;
  }//if InColony


  if (Wooden_Mode or InEurope() or InColony() or InReport()) return; //rest is only for america view

  if (not Special)
  {
    switch (toupper(char(Key)))
    {
      case 'B': //build colony
           if (focused!=NULL)
           {
             TMap& temp_Map = TMap::GetSingleton();
             if (focused->IsShip() or temp_Map.tiles[focused->GetPosX()][focused->GetPosY()]->IsWater())
               ShowMessageSimple(TLanguage::GetSingleton().GetBuildColony(2));
             else
             {
               if (temp_Map.tiles[focused->GetPosX()][focused->GetPosY()]->GetType()==ttMountains)
                 ShowMessageSimple(TLanguage::GetSingleton().GetBuildColony(4));
               else
               {
                 if (TData::GetSingleton().FreeForSettlement(focused->GetPosX(), focused->GetPosY()))
                 {
                   temp_cb.inputText = "";
                   temp_cb._type = CBT_BUILD_COLONY;
                   temp_cb.BuildColony = new TBuildColonyData;
                   temp_cb.BuildColony->x = focused->GetPosX();
                   temp_cb.BuildColony->y = focused->GetPosY();
                   temp_cb.BuildColony->num_nation = TData::GetSingleton().PlayerNation();
                   temp_cb.BuildColony->founder = focused;
                   temp_cb.BuildColony->AData = TData::GetSingleton();
                   ShowMessageInput(TLanguage::GetSingleton().GetBuildColony(0), TLanguage::GetSingleton().GetBuildColony(1),
                       TLanguage::GetSingleton().GetColonyNames(focused->GetNation(),
                       TData::GetSingleton().GetColonyList(focused->GetNation()).size()), temp_cb);
                   focused = NULL;
                 }
                 else
                   ShowMessageSimple(TLanguage::GetSingleton().GetBuildColony(3));
               }//else
             }//if
           }//if
           break; //end of 'B'
      case 'F': //fortify
           if (focused!=NULL)
           {
             if (focused->GetState()==usFortified) focused->SetState(usNormal);
             else focused->SetState(usFortified);
           }//if, fortify
           break;
      case 'S': break;;//sentry
      case 'C':
           if (focused!=NULL) CenterOn(focused->GetPosX(), focused->GetPosY());
             else CenterOn(25, 35);//just for testing, yet
           break;
    }//swi
  }//if not Special

  if (focused==NULL)
  {
    //no unit focused
    //move map
    switch (Key)
    {
      case GLUT_KEY_LEFT:
      case KEY_NUMPAD4:
           if (OffsetX>0) OffsetX = OffsetX-1; /*Move map left*/
           break;
      case GLUT_KEY_RIGHT:
      case KEY_NUMPAD6:
           if (OffsetX<cMap_X-x_Fields) OffsetX = OffsetX+1; /*Move map right*/
           break;
      case GLUT_KEY_DOWN:
      case KEY_NUMPAD2:
           if (OffsetY<cMap_Y-y_Fields) OffsetY = OffsetY+1; /*Move map down*/
           if (MiniMapOffset_Y<cMap_Y-MiniMap_y_Fields) MiniMapOffset_Y = MiniMapOffset_Y+1;
           break;//KEY_DOWN
      case GLUT_KEY_UP:
      case KEY_NUMPAD8:
           if (OffsetY>0) OffsetY = OffsetY-1; /*Move map up*/
           if (MiniMapOffset_Y>0) MiniMapOffset_Y = MiniMapOffset_Y-1;
           break;//KEY_UP
      case KEY_SPACE: //try to get next unit
           focused = TData::GetSingleton().GetFirstLazyUnit(TData::GetSingleton().PlayerNation());
           if (focused!=NULL) CenterOn(focused->GetPosX(), focused->GetPosY());
           else
           {
             //no units left, start new round
             TData::GetSingleton().AdvanceYear();
             TData::GetSingleton().NewRound(TData::GetSingleton().PlayerNation());
             focused = TData::GetSingleton().GetFirstLazyUnit(TData::GetSingleton().PlayerNation());
           }//else
           break;//case SPACE
    }//swi
  }//if
  else
  {
    //we have a focused unit, so move it
    TDirection direc = dirNone;
    switch (Key)
    {
      case KEY_NUMPAD1: direc =dirSW; break;
      case GLUT_KEY_DOWN:
      case KEY_NUMPAD2: direc = dirS; break; /*Move down*/
      case KEY_NUMPAD3: direc = dirSE; break;
      case GLUT_KEY_LEFT:
      case KEY_NUMPAD4: direc = dirW; break; /*Move left*/
      case GLUT_KEY_RIGHT:
      case KEY_NUMPAD6: direc = dirE; break; /*Move right*/
      case KEY_NUMPAD7: direc = dirNW; break;
      case GLUT_KEY_UP:
      case KEY_NUMPAD8: direc = dirN; break; /*Move unit up*/
      case KEY_NUMPAD9: direc = dirNE; break;
      case KEY_SPACE:
           focused->MovesLeft = 0;
           TUnit* tempUnit = TData::GetSingleton().GetFirstLazyUnit(TData::GetSingleton().PlayerNation());
                   if (tempUnit!=NULL)
                   {
                     focused = tempUnit;
                     CenterOn(focused->GetPosX(), focused->GetPosY());
                   }//if
                   else
                   {
                     //no units left, start new round
                     TData::GetSingleton().AdvanceYear();
                     TData::GetSingleton().NewRound(TData::GetSingleton().PlayerNation());
                     focused = TData::GetSingleton().GetFirstLazyUnit(TData::GetSingleton().PlayerNation());
                   }//else
           break;//KEY_SPACE
    }//swi
    //should move now
    if (direc!=dirNone)
    {
      Byte temp_x = focused->GetPosX();
      Byte temp_y = focused->GetPosY();
      ApplyDir(&temp_x, &temp_y, direc);
      TMap& temp_Map = TMap::GetSingleton();
      if (focused->IsShip())
      {
        if (not temp_Map.tiles[temp_x][temp_y]->IsWater())
        {
          TColony* temp_col = TData::GetSingleton().GetColonyInXY(temp_x, temp_y);
          if (temp_col!=NULL)
          {
            if ((temp_col->GetNation()==focused->GetNation()) and (focused->MovesLeft>0))
            {
              //ship enters colony
              focused->WarpToXY(temp_x, temp_y, temp_Map);
              focused->DropAllPassengers();
              focused->MovesLeft = focused->MovesLeft-1;
            }
            else focused->Move(direc, temp_Map);
          }//if temp_col<>nil
          else if (focused->EmbarkedPassengers()>0)
          {
            //check for landfall
            temp_cb._type = CBT_LANDFALL;
            temp_cb.Landfall = new TLandfallData;
            temp_cb.Landfall->cbLandfall = CBF_Landfall;
            temp_cb.Landfall->Ship = focused;
            TUnit* tempUnit = focused->GetFirstEmbarkedPassenger();
            if (tempUnit!=NULL) temp_cb.Landfall->UType = tempUnit->GetType();
            else temp_cb.Landfall->UType = utGalleon;
            temp_cb.Landfall->x = temp_x;
            temp_cb.Landfall->y = temp_y;
            temp_cb.Landfall->AMap = temp_Map;
            ShowMessageOptions(TLanguage::GetSingleton().GetLandfall(0), ToStringArr(TLanguage::GetSingleton().GetLandfall(1), TLanguage::GetSingleton().GetLandfall(2)), temp_cb);
          } //if passengers>0
          else focused->Move(direc, temp_Map);
        }//if not water
        else focused->Move(direc, temp_Map);
      }//if IsShip
      else focused->Move(direc, temp_Map);
    }//if
    //check if unit moved out of sight, and center on it, if neccessary
    if (focused!=NULL)
    {
      if ((focused->GetPosX()<=OffsetX) or (focused->GetPosY()<=OffsetY) or
          (focused->GetPosX()>=OffsetX+x_Fields-1) or (focused->GetPosY()>=OffsetY+y_Fields-1))
        CenterOn(focused->GetPosX(), focused->GetPosY());
    }//if
  }//else
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.KeyFunc\n";
  #endif
}//proc

void TGui::MouseFunc(const LongInt button, const LongInt state, const LongInt x, const LongInt y)
/*var pos_x, pos_y: Integer;
    temp_cat: TMenuCategory;
    tempUArr: TUnitArr;
    tempAmount, tempWord: Word;
    str_arr: TShortStrArr;
    bx, by: Single;
    bType, bType2: TBuildingType;*/
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.MouseFunc\n";
  #endif
  if (msg.txt!="") return;

  //general stuff
  if ((button==GLUT_LEFT) and (state==GLUT_UP)) mouse.down = false;
  else if ((button==GLUT_LEFT) and (state==GLUT_DOWN))
  {
    mouse.down = true;
    mouse.down_x = x;
    mouse.down_y = y;
  }//if down

  TCallbackRec temp_cbr;
  ShortInt sx, sy, sx_d, sy_d;
  float bx, by;
  TGoodType tempGood;

  //handling colony view's mouse events
  // ---- general ones, i.e. the ones that work on both pages
  if (InColony())
  {
    //check for pressing the red "E" in colony view
    if ((button==GLUT_LEFT) and (state==GLUT_UP))
    {
      //check for pressing the red "E" in colony view
      if ((x>608) and (y>cWindowHeight-50))
      {
        //WriteLn('Debug: x: ',x, '; y: ', y, #13#10, 'out of colony now');
        cur_colony = NULL;
        glutPostRedisplay();
        return;
      }//if
      //check for colony bar click (i.e. renaming colony)
      else if ((mouse.y<=16) and (mouse.down_y<=16))
      {
        temp_cbr._type = CBT_RENAME_COLONY;
        temp_cbr.RenameColony = new TRenameColonyData;
        temp_cbr.RenameColony->AColony = cur_colony;
        ShowMessageInput(TLanguage::GetSingleton().GetColonyString(csRenameQuestion),
          TLanguage::GetSingleton().GetColonyString(csRenameLabel), cur_colony->GetName(), temp_cbr);
        return;
      }//if title bar clicked

      //check for switcher button (unit view/ building view)
      else if ((GetSwitcherButtonAtMouse(mouse.x, mouse.y)!=-1) and
         (GetSwitcherButtonAtMouse(mouse.x, mouse.y)==GetSwitcherButtonAtMouse(mouse.down_x, mouse.down_y)))
      {
        //button was pressed
        ColonyBuildingPage = (GetSwitcherButtonAtMouse(mouse.x, mouse.y)==1);
        return;
      }//if

      // -- units in fields
      GetColonyFieldAtMouse(sx, sy);
      GetColonyFieldAtMouse(sx_d, sy_d, mouse.down_x, mouse.down_y);
      //moving unit in field
      if ((sx!=-2) and (sx_d!=-2) and ((sx!=sx_d) or (sy!=sy_d)) and (cur_colony->GetUnitInField(sx_d, sy_d)!=NULL))
      {
        //check whether field is center or not
        if ((sx!=0) or (sy!=0))
        {
          TUnit* tempUnit = cur_colony->GetUnitInField(sx_d, sy_d);
          tempGood = cur_colony->GetUnitInFieldGood(sx_d, sy_d);
          cur_colony->SetUnitInField(sx_d, sy_d, NULL);
          cur_colony->SetUnitInField(sx, sy, tempUnit, tempGood);
        }//if
      }//if
      //change job of unit
      else if ((sx!=-2) and (sx_d!=-2) and (cur_colony->GetUnitInField(sx, sy)!=NULL))
      {
        temp_cbr._type = CBT_JOB_CHANGE;
        temp_cbr.JobChange = new TJobChangeData;
        temp_cbr.JobChange->x_shift = sx;
        temp_cbr.JobChange->y_shift = sy;
        temp_cbr.JobChange->AColony = cur_colony;
        ShowMessageOptions("Choose profession for unit "+TLanguage::GetSingleton().GetUnitName(cur_colony->GetUnitInField(sx, sy)->GetType())+":",
                           TData::GetSingleton().GetJobList(sx, sy, cur_colony->GetUnitInField(sx, sy)->GetType(), cur_colony), temp_cbr);
      }//else if

    }//if Left Mouse Button up
  }//if InColony (general)

  TStringArr str_arr;
  LongInt pos_x, pos_y;
  TUnitArr tempUArr;
  Word tempAmount;

  // ---- events on unit page, e.g. dragging goods/ units
  if (InColony() and not ColonyBuildingPage)
  {
    if ((button==GLUT_LEFT) and (state==GLUT_UP))
    {
      GetColonyFieldAtMouse(sx, sy);
      GetColonyFieldAtMouse(sx_d, sy_d, mouse.down_x, mouse.down_y);

      //*** check for good transfer ***
      //from ship to port of colony
      sx = GetCargoBoxAtMouse(mouse.down_x, mouse.down_y);
      tempGood = GetGoodAtMouse();
      if ((sx!=-1) and (tempGood!=gtCross))
      {
        tempUArr = TData::GetSingleton().GetAllShipsInXY(cur_colony->GetPosX(), cur_colony->GetPosY());
        if (tempUArr.size()>0) //at least one ship present?
          if (tempUArr[0]->GetCargoAmountBySlot(sx)>0)
          {
            //we have a cargo transfer to the port
            tempAmount = tempUArr[0]->UnloadGood(tempUArr[0]->GetCargoGoodBySlot(sx), tempUArr[0]->GetCargoAmountBySlot(sx));
            cur_colony->AddToStore(tempUArr[0]->GetCargoGoodBySlot(sx), tempAmount);
          }//if
      }//if
      //from port to ship
      sx = GetCargoBoxAtMouse();
      tempGood = GetGoodAtMouse(mouse.down_x, mouse.down_y);
      if ((sx!=-1) and (tempGood!=gtCross))
      {
        tempUArr = TData::GetSingleton().GetAllShipsInXY(cur_colony->GetPosX(), cur_colony->GetPosY());
        if (tempUArr.size()>0)
        {
          if (cur_colony->GetStore(tempGood)>=100) tempAmount = 100;
          else tempAmount = cur_colony->GetStore(tempGood);
          if (tempUArr[0]->LoadGood(tempGood, tempAmount))
          {
            cur_colony->RemoveFromStore(tempGood, tempAmount);
          }//if
          else ShowMessageSimple(TLanguage::GetSingleton().GetTransfer(tsOutOfSpace));
        }//if
      }//if

      //check for moving unit from "outside" to fields
      GetColonyFieldAtMouse(sx, sy, mouse.x, mouse.y);
      pos_x = GetColonyUnitAtMouse(mouse.down_x, mouse.down_y);
      if ((pos_x!=-1) and (sx!=-2) and ((sx!=0) or (sy!=0)))
      {
        tempUArr = TData::GetSingleton().GetAllUnitsInColony(cur_colony);
        if (tempUArr.size()>pos_x)
        {
          cur_colony->SetUnitInField(sx, sy, tempUArr[pos_x]);
          return;
        }//if
      }//if

      //check for moving unit from fields to "outside"
      GetColonyFieldAtMouse(sx, sy, mouse.down_x, mouse.down_y);
      pos_x = GetColonyUnitAtMouse(mouse.x, mouse.y);
      if ((pos_x!=-1) and (sx!=-2) and ((sx!=0) or (sy!=0)))
      {
        if (cur_colony->GetUnitInField(sx, sy)!=NULL)
        {
          if (cur_colony->GetInhabitants()>1) cur_colony->SetUnitInField(sx, sy, NULL);
          else
          {
            //ask whether they want to abandon colony
            temp_cbr._type = CBT_ABANDON_COLONY;
            temp_cbr.AbandonColony = new TAbandonColonyData;
            temp_cbr.AbandonColony->AColony = cur_colony;
            temp_cbr.AbandonColony->AData = TData::GetSingleton();
            ShowMessageOptions(TLanguage::GetSingleton().GetColonyString(csAbandonQuestion),
              ToStringArr(TLanguage::GetSingleton().GetColonyString(csAbandonNo),
                TLanguage::GetSingleton().GetColonyString(csAbandonYes)), temp_cbr);
          }//else
        }//if unit<>nil
      }//if

      //check for unit management "outside" of colony
      pos_x = GetColonyUnitAtMouse(mouse.x, mouse.y);
      pos_y = GetColonyUnitAtMouse(mouse.down_x, mouse.down_y);
      if ((pos_x!=-1) and (pos_x==pos_y))
      {
        tempUArr = TData::GetSingleton().GetAllUnitsInColony(cur_colony);
        if (tempUArr.size()>pos_x)
        {
          temp_cbr._type = CBT_COLONY_UNIT;
          temp_cbr.ColonyUnit = new TColonyUnitData;
          temp_cbr.ColonyUnit->AUnit = tempUArr[pos_x];
          //SetLength(str_arr, 3);
          str_arr = TStringArr(3, "");
          if ((tempUArr[pos_x]->GetState()==usFortified) or (tempUArr[pos_x]->GetState()==usWaitingForShip))
            str_arr[0] = TLanguage::GetSingleton().GetColonyUnit(cusCancelOrders);
          else str_arr[0] = TLanguage::GetSingleton().GetColonyUnit(cusOnBoard);
          if (tempUArr[pos_x]->GetState()==usFortified)
            str_arr[1] = TLanguage::GetSingleton().GetColonyUnit(cusOnBoard);
          else str_arr[1] = TLanguage::GetSingleton().GetColonyUnit(cusFortify);
          str_arr[2] = TLanguage::GetSingleton().GetOthers(osNoChanges);
          ShowMessageOptions(TLanguage::GetSingleton().GetColonyUnit(cusOptions)+" "+TLanguage::GetSingleton().GetUnitName(tempUArr[pos_x]->GetType())+":",
                             str_arr, temp_cbr);
        }//if
      }//if

    }//else if button=LEFT and state=UP
    #ifdef DEBUG_CODE
    std::cout <<"Exiting TGui.MouseFunc\n";
    #endif
    return;
  }//if colony (units' page)

  TBuildingType bType;
  TUnit* tempUnit;
  // ---- events on building page, i.e. moving units from and to buildings
  if (InColony() and ColonyBuildingPage)
  {
    if ((button==GLUT_LEFT) and (state==GLUT_UP))
    {
      //dragging unit from field to building
      GetColonyFieldAtMouse(sx, sy, mouse.down_x, mouse.down_y);
      bType = GetBuildingAtMouse(x,y);
      if (((bType >=btArmory) and (bType<=btBlacksmith)) and (sx!=-2))
      {
        tempUnit = cur_colony->GetUnitInField(sx, sy);
        if (tempUnit!=NULL)
        {
          //colonist dragged from fields to building
          sx_d = cur_colony->GetFirstFreeBuildingSlot(bType);
          if (sx_d!=-1)
          {
            cur_colony->SetUnitInField(sx, sy, NULL);
            cur_colony->SetUnitInBuilding(bType, sx_d, tempUnit);
          }//if
          else ShowMessageSimple(TLanguage::GetSingleton().GetBuildingString(bsMaxThree));
        }//if unit<>nil
      }//if
      //dragging unit from building to field
      GetColonyFieldAtMouse(sx, sy, x, y);
      bType = GetBuildingAtMouse(mouse.down_x,mouse.down_y);
      if ((bType!=btNone) and (sx!=-2))
      {
        GetBuildingPosition(bType, bx, by);
        pos_x = static_cast<LongInt>((mouse.down_x-bx*FieldWidth)/FieldWidth);
        tempUnit = cur_colony->GetUnitInBuilding(bType, pos_x);
        if ((tempUnit!=NULL) and ((sx!=0) or (sy!=0)))
        {
          cur_colony->SetUnitInBuilding(bType, pos_x, NULL);
          cur_colony->SetUnitInField(sx, sy, tempUnit);
        }//if (unit<>nil)
      }//if (building and field valid)

      //check for dragging unit from one building to another building
      bType = GetBuildingAtMouse(mouse.down_x,mouse.down_y);
      TBuildingType bType2 = GetBuildingAtMouse(x,y);
      if (((bType>=btArmory) and (bType<=btBlacksmith)) and ((bType2>=btArmory) and (bType2<=btBlacksmith)) and (bType!=bType2))
      {
        //get slot at first building
        GetBuildingPosition(bType, bx, by);
        sx = static_cast<LongInt>(mouse.down_x - bx*FieldWidth) / FieldWidth;
        //get free slot #
        sy = cur_colony->GetFirstFreeBuildingSlot(bType2);
        //get the unit
        tempUnit = cur_colony->GetUnitInBuilding(bType, sx);
        if (((sx>=0) and (sx<=2)) and (tempUnit!=NULL))
        {
          if (sy!=-1)
          {
            cur_colony->SetUnitInBuilding(bType, sx, NULL);
            cur_colony->RealignUnitsInBuilding(bType);
            cur_colony->SetUnitInBuilding(bType2, sy, tempUnit);
          }//if
          else ShowMessageSimple(TLanguage::GetSingleton().GetBuildingString(bsMaxThree));
        }//if
      }//if

      //check for clicking construction bar
      if (IsMouseInConstructionBar(x,y) and IsMouseInConstructionBar(mouse.down_x,mouse.down_y))
      {
        temp_cbr._type = CBT_CONSTRUCTION;
        temp_cbr.Construction = new TConstructionData;
        temp_cbr.Construction->AColony = cur_colony;
        //SetLength(str_arr, 1);
        str_arr.clear();
        str_arr.push_back(TLanguage::GetSingleton().GetOthers(osNothing));
        for (pos_x= Ord(Succ(btNone)); pos_x<=Ord(High(btNone)); ++pos_x)
        {
          pos_y = cur_colony->GetBuildingLevel(TBuildingType(pos_x));
          if (pos_y<GetMaxBuildingLevel(TBuildingType(pos_x)))
          {
            //SetLength(str_arr, length(str_arr)+1);
            Word tempWord;
            GetBuildingCost(TBuildingType(pos_x), pos_y+1, &tempAmount, &tempWord);
            str_arr.push_back(StretchTo59(
                 TLanguage::GetSingleton().GetBuildingName(TBuildingType(pos_x), pos_y+1),
                 IntToStr(tempAmount)+" "+TLanguage::GetSingleton().GetGoodName(gtHammer)+", "
                 +IntToStr(tempWord)+" "+TLanguage::GetSingleton().GetGoodName(gtTool)
               ));
          }//if
        }//for
        ShowMessageOptions(TLanguage::GetSingleton().GetBuildingString(bsSelectNext), str_arr, temp_cbr);
      }//construction bar
    }//if (left button up)
    #ifdef DEBUG_CODE
    std::cout << "Exiting TGui.MouseFunc\n";
    #endif
    return;
  }//if at colony's building page

  if (InColony()) return;

  //handle European view's mouse events
  if (europe!=NULL)
  {
    //check for pressing the red "E" in European view
    if ((button==GLUT_LEFT) and (state==GLUT_UP) and (x>608) and (y>cWindowHeight-50))
    {
      europe = NULL;
      glutPostRedisplay();
    }//if
    //check for good transfer to port or from port to ship
    else if ((button==GLUT_LEFT) and (state==GLUT_UP))
    {
      //from ship to port
      sx = GetCargoBoxAtMouse(mouse.down_x, mouse.down_y);
      tempGood = GetGoodAtMouse();
      //WriteLn('Debug: Cargo to port: sx: ', sx, '; Good: ', dat.GetLang.GetGoodName(tempGood));
      if ((sx!=-1) and (tempGood!=gtCross))
      {
        std::cout << "Trying to clear cargo...\n";
        tempUArr = TData::GetSingleton().GetAllShipsInEurope(europe->GetCount());
        if (tempUArr.size()>0) //at least one ship present?
          if (tempUArr[0]->GetCargoAmountBySlot(sx)>0)
          {
            //we have a cargo transfer to the port
            if (europe->IsBoycotted(tempUArr[0]->GetCargoGoodBySlot(sx)))
              ShowMessageSimple(TLanguage::GetSingleton().GetTransfer(tsBoycotted));
            else
            {
              //start the transfer, finally
              tempGood = tempUArr[0]->GetCargoGoodBySlot(sx);
              tempAmount = tempUArr[0]->UnloadGood(tempUArr[0]->GetCargoGoodBySlot(sx), tempUArr[0]->GetCargoAmountBySlot(sx));
              //print message about earnings
              ShowMessageSimple(
              StretchTo60(TLanguage::GetSingleton().GetGoodName(tempGood)+": "+IntToStr(tempAmount)+"x"
                 +IntToStr(europe->GetPrice(tempGood, true))+" "+TLanguage::GetSingleton().GetOthers(osGold),
                IntToStr(europe->GetPrice(tempGood, true)*tempAmount)+" "+TLanguage::GetSingleton().GetOthers(osGold))
              +StretchTo60("-"+IntToStr(europe->GetTaxRate())+"% "+TLanguage::GetSingleton().GetOthers(osTax),
                 IntToStr((europe->GetPrice(tempGood, true)*tempAmount*europe->GetTaxRate()) / 100)+" "+TLanguage::GetSingleton().GetOthers(osGold))
              +StretchTo60(TLanguage::GetSingleton().GetOthers(osEarnings)+":", IntToStr(europe->GetPrice(tempGood, true)*tempAmount-((europe->GetPrice(tempGood, true)*tempAmount*europe->GetTaxRate()) / 100))+" "+TLanguage::GetSingleton().GetOthers(osGold)));
              //actually sell it
              europe->SellGood(tempUArr[0]->GetCargoGoodBySlot(sx), tempAmount);
            }//else
          }//if
      }//if
      //from port to ship
      sx = GetCargoBoxAtMouse();
      tempGood = GetGoodAtMouse(mouse.down_x, mouse.down_y);
      // WriteLn('Debug: Cargo to ship: sx: ', sx, '; Good: ', dat.GetLang.GetGoodName(tempGood));
      if ((sx!=-1) and (tempGood!=gtCross))
      {
        std::cout << "Trying to load cargo...\n";
        tempUArr = TData::GetSingleton().GetAllShipsInEurope(europe->GetCount());
        if (tempUArr.size()>0) //at least one ship present?
        {
          //cargo transfer from port to ship
          if (europe->IsBoycotted(tempGood))
            ShowMessageSimple(TLanguage::GetSingleton().GetTransfer(tsBoycotted));
          else if (europe->GetPrice(tempGood, false)*100>europe->GetGold())
            ShowMessageSimple(TLanguage::GetSingleton().GetTransfer(tsOutOfGold));
          else
          {
            //start the transfer
            if (tempUArr[0]->LoadGood(tempGood, 100))
            {
              europe->BuyGood(tempGood, 100);
              //should show message about costs to player
              ShowMessageSimple(
              StretchTo60(TLanguage::GetSingleton().GetGoodName(tempGood)+": 100x", IntToStr(europe->GetPrice(tempGood, false))+" "+TLanguage::GetSingleton().GetOthers(osGold))
              + StretchTo60(TLanguage::GetSingleton().GetOthers(osCost)+":", IntToStr(europe->GetPrice(tempGood, false)*100)+" "+TLanguage::GetSingleton().GetOthers(osGold))
              );
            }
            else ShowMessageSimple(TLanguage::GetSingleton().GetTransfer(tsOutOfSpace));
          }//else
        }//if
      }//if


      //check for moving ship from "to new world box" to "expected soon box"
      pos_x = GetToNewWorldAtMouse(mouse.down_x, mouse.down_y);
      //WriteLn('Debug: New World at mouse: pos_x: ', pos_x);
      if ((pos_x!=-1) and IsMouseInExpectedSoon())
      {
        std::cout<< "Trying to send back to europe...\n";
        tempUArr = TData::GetSingleton().GetAllShipsGoingToNewWorld(europe->GetCount());
        if (pos_x<tempUArr.size()) tempUArr[pos_x]->CallBackToEurope();
        return;
      }//if

      //check for moving ship from "expected soon box" to "to new world box"
      pos_x = GetExpectedSoonAtMouse(mouse.down_x, mouse.down_y);
      //WriteLn('Debug: Expected Soon at mouse: pos_x: ', pos_x);
      if ((pos_x!=-1) and IsMouseInToNewWorld())
      {
        std::cout << "Trying to send back to new world...\n";
        tempUArr = TData::GetSingleton().GetAllShipsGoingToEurope(europe->GetCount());
        if (pos_x<tempUArr.size()) tempUArr[pos_x]->CallBackToNewWorld();
        return;
      }//if

      //check for moving ship from port to "new world box"
      pos_x = GetShipAtMouse(mouse.down_x, mouse.down_y);
      //WriteLn('Debug: Ship at mouse: pos_x: ', pos_x);
      if ((pos_x!=-1) and IsMouseInToNewWorld())
      {
        std::cout << "Trying to send a ship to new world...\n";
        tempUArr = TData::GetSingleton().GetAllShipsInEurope(europe->GetCount());
        if (pos_x<tempUArr.size())
        {
          //load all possible units we can load, before we go off to New World
          tempUnit = tempUArr[pos_x];
          tempUArr = TData::GetSingleton().GetAllNonShipsInEurope(europe->GetCount());
          for (pos_y= 0; pos_y<tempUArr.size(); ++pos_y)
            if ((tempUArr[pos_y]->GetState()==usWaitingForShip) and (tempUnit->FreeCapacity()>0))
              tempUnit->LoadUnit(tempUArr[pos_y]);
          tempUnit->SendToNewWorld();
          return;
        }//if
      }//if

      //check for clicked unit in port
      pos_x = GetUnitAtMouse(mouse.x, mouse.y);
      pos_y = GetUnitAtMouse(mouse.down_x, mouse.down_y);
      if ((pos_x!=-1) and (pos_x==pos_y))
      {
        tempUArr = TData::GetSingleton().GetAllNonShipsInEurope(europe->GetCount());
        if (pos_x<tempUArr.size())
        {
          //SetLength(str_arr, 5);
          str_arr = TStringArr(5, "");

          if (tempUArr[pos_x]->GetState()==usWaitingForShip) str_arr[0] = TLanguage::GetSingleton().GetEuroPort(epsNotOnShip);
          else str_arr[0] = TLanguage::GetSingleton().GetEuroPort(epsGoOnShip);
          if (tempUArr[pos_x]->HasMuskets())
            str_arr[1] = TLanguage::GetSingleton().GetEuroPort(epsDisarm)+" ("+TLanguage::GetSingleton().GetOthers(osSaving)+" "+IntToStr(europe->GetPrice(gtMusket, true)*50)+" "+TLanguage::GetSingleton().GetOthers(osGold)+")";
          else
            str_arr[1] = TLanguage::GetSingleton().GetEuroPort(epsArm)+" ("+TLanguage::GetSingleton().GetOthers(osCost)+" "+IntToStr(europe->GetPrice(gtMusket, false)*50)+" "+TLanguage::GetSingleton().GetOthers(osGold)+")";
          if (tempUArr[pos_x]->HasHorses())
            str_arr[2] = TLanguage::GetSingleton().GetEuroPort(epsNoHorses)+" ("+TLanguage::GetSingleton().GetOthers(osSaving)+" "+IntToStr(europe->GetPrice(gtHorses, true)*50)+" "+TLanguage::GetSingleton().GetOthers(osGold)+")";
          else str_arr[2] = TLanguage::GetSingleton().GetEuroPort(epsGiveHorses)+" ("+TLanguage::GetSingleton().GetOthers(osCost)
                 +" "+IntToStr(europe->GetPrice(gtHorses, false)*50)+" "+TLanguage::GetSingleton().GetOthers(osGold)+")";
          if (tempUArr[pos_x]->GetToolAmount()>0) str_arr[3] = TLanguage::GetSingleton().GetEuroPort(epsNoTools)
               +" ("+TLanguage::GetSingleton().GetOthers(osSaving)+" "+IntToStr(europe->GetPrice(gtTool, true)*tempUArr[pos_x]->GetToolAmount())+" "+TLanguage::GetSingleton().GetOthers(osGold)+")";
          else str_arr[3] = TLanguage::GetSingleton().GetEuroPort(epsGiveTools)+" ("+TLanguage::GetSingleton().GetOthers(osCost)
                 +" "+IntToStr(europe->GetPrice(gtTool, false)*(100-tempUArr[pos_x]->GetToolAmount()))+" "+TLanguage::GetSingleton().GetOthers(osGold)+")";
          str_arr[4] = TLanguage::GetSingleton().GetOthers(osNoChanges);

          temp_cbr.option =0;
          temp_cbr._type = CBT_EURO_PORT_UNIT;
          temp_cbr.inputText = "";
          temp_cbr.EuroPort = new TEuroPortUnitData;
          temp_cbr.EuroPort->AUnit = tempUArr[pos_x];
          temp_cbr.EuroPort->EuroNat = europe;
          ShowMessageOptions(TLanguage::GetSingleton().GetEuroPort(epsManageHeading), str_arr, temp_cbr);
          return;
        }//if pos_x<>High(array)
      }//if pos_x<>-1

      //check for button "Buy Ship"
      switch (GetButtonAtMouse(mouse.x, mouse.y))
      {
        case 1:
             //SetLength(str_arr, Ord(utFrigate)-Ord(utCaravel)+2);
             str_arr = TStringArr(Ord(utFrigate)-Ord(utCaravel)+2, "");
             str_arr[0] = TLanguage::GetSingleton().GetOthers(osNothing);
             for (pos_x = Ord(utCaravel); pos_x<=Ord(utFrigate); ++pos_x)
               str_arr[1+pos_x-Ord(utCaravel)] = StretchTo59(TLanguage::GetSingleton().GetUnitName(TUnitType(pos_x))+":",TLanguage::GetSingleton().GetOthers(osCost)
                      +" "+IntToStr(cShipPrices(TUnitType(pos_x)))+" "+TLanguage::GetSingleton().GetOthers(osGold));
             temp_cbr.option =0;
             temp_cbr._type = CBT_EURO_PORT_BUY;
             temp_cbr.inputText = "";
             temp_cbr.EuroBuy = new TEuroBuyData;
             temp_cbr.EuroBuy->EuroNat = europe;
             temp_cbr.EuroBuy->AData = TData::GetSingleton();
             ShowMessageOptions(TLanguage::GetSingleton().GetEuroPort(epsBuyHeading), str_arr, temp_cbr);
             break;//case ButtonAtMouse=1 ("Buy Ship")
        case 2:
             //SetLength(str_arr, 1);
             str_arr = TStringArr(1, "");
             str_arr[0] = TLanguage::GetSingleton().GetOthers(osNothing);
             for (pos_x= Ord(utFarmer); pos_x<=Ord(utRegular); ++pos_x)
               if (cUnitPrices(TUnitType(pos_x))>0)
               {
                   str_arr.push_back( StretchTo59(TLanguage::GetSingleton().GetUnitName(TUnitType(pos_x))+":",TLanguage::GetSingleton().GetOthers(osCost)
                                   +" "+IntToStr(cUnitPrices(TUnitType(pos_x)))+" "+TLanguage::GetSingleton().GetOthers(osGold)));
               }//if
             temp_cbr.option = 0;
             temp_cbr._type = CBT_EURO_PORT_TRAIN;
             temp_cbr.inputText = "";
             temp_cbr.EuroTrain = new TEuroTrainData;
             temp_cbr.EuroTrain->EuroNat = europe;
             temp_cbr.EuroTrain->AData = TData::GetSingleton();
             ShowMessageOptions(TLanguage::GetSingleton().GetEuroPort(epsTrainHeading),
                                str_arr, temp_cbr);
             break;//case ButtonAtMouse=2 ("Train units")
      }//swi


    }//else if (button=LEFT) and (state=UP)
    #ifdef DEBUG_CODE
    std::cout <<"Exiting TGui.MouseFunc\n";
    #endif
    return;
  }//europe

  if (InReport())
  {
    if ((button==GLUT_LEFT) and (state==GLUT_UP)) report = rtNone;
    #ifdef DEBUG_CODE
    std::cout<<"Exiting TGui.MouseFunc\n";
    #endif
    return;
  }//report

  TMenuCategory temp_cat;
  //handle map view's mouse events here
  if ((button==GLUT_LEFT) and (state==GLUT_UP))
  {
    if (InMenu())
    {
      GetMenuSelectionAtMouse(temp_cat, pos_x);
      //WriteLn('Debug: GUI got selection: cat.: ', Ord(temp_cat), '; sel.: ', pos_x);//for debug
      if ((pos_x==0) and (temp_cat==menu_cat)) menu_cat = mcNone;
      else if (pos_x==0)
      {
        menu_cat = temp_cat;
        selected_menu_option = 1;
      }//if
      else if ((pos_x!=-1) and (temp_cat==menu_cat))
      {
        HandleMenuSelection(menu_cat, pos_x);
        menu_cat = mcNone;
        selected_menu_option = 1;
      }//if
      else if (pos_x==-1)
      {
        menu_cat = mcNone;
        selected_menu_option = 1;
      }//if
      return;
    }//if InMenu

    if (Wooden_Mode) pos_x = -1;
    else
    {
      //in america view
      GetSquareAtMouse(pos_x, pos_y);
      //WriteLn('GUI got square: x: ', pos_x, '; y: ', pos_y);//for debug
    }//else
    if (pos_x!=-1)
    {
      //check for colony first
      cur_colony = TData::GetSingleton().GetColonyInXY(pos_x, pos_y);
      //if not player's colony, set back to nil
      if (cur_colony!=NULL)
      {
        if (cur_colony->GetNation()!=TData::GetSingleton().PlayerNation())
        {
          cur_colony = NULL;
          std::cout<< "Debug: x: "<<x<<"; y: "<<y<<"\nout of colony now";
        }
      }//if colony<>nil
      else
      {
        /*If we don't have a colony there, there might be a unit?*/
        focused = TData::GetSingleton().GetFirstUnitInXY(pos_x, pos_y);
      }//else
      CenterOn(pos_x, pos_y);
      glutPostRedisplay();
    }//if pos_x<>-1
    else
    {
      menu_cat = GetMenuCategoryAtMouse();
      #ifdef DEBUG_CODE
      std::cout <<"GUI got category: "<<Ord(menu_cat))<<"\n";
      #endif
    }//else
  }//if
  #ifdef DEBUG_CODE
    std::cout <<"Leaving TGui.MouseFunc\n";
  #endif
}//proc

void TGui::MouseMoveFunc(const LongInt x, const LongInt y)
{
  mouse.x = x;
  mouse.y = y;
}//func

void TGui::Resize(LongInt Width, LongInt Height)
{
  #ifdef DEBUG_CODE
    std::cout <<"Entered TGui.Resize\n";
  #endif
  if ((Width!=cWindowWidth) or (Height!=cWindowHeight))
    glutReshapeWindow(cWindowWidth, cWindowHeight);
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.Resize\n";
  #endif
}//proc

void TGui::Start()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.Start\n";
  #endif
  std::cout << "glEnable-like stuff\n";
   // Enable backface culling
  glEnable(GL_CULL_FACE);
  // Set up depth buffer
  //glEnable(GL_DEPTH_TEST);
  //glDepthFunc(GL_LESS);
  glAlphaFunc(GL_GREATER, 0.2);
  //Starting
  std::cout << "glutMainLoop\n";
  glutMainLoop();
}//proc Start

void TGui::Draw()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.Draw\n";
  #endif
  glLoadIdentity();
  glViewport(0,0, cWindowWidth, cWindowHeight);
  glOrtho(0.0, 20.0, -0.5, 12.5, -1.0, 1.0);

  glClearColor(0.83, 0.66, 0.39,0.0);//set "wooden" color as clear color...
                                     //saves us from drawing wooden bar
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  if (InColony())
  {
    DrawColonyView();
  }//if
  else if (InEurope())
  {
    DrawEuropeanView();
  }//if
  else if (InWoodenMode())
  {
    //draw border
    glBegin(GL_QUADS);
      glColor3f(0.0, 0.0, 0.0);
      glVertex2f(0.0, y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields+BorderWidth);
      glVertex2f(0.0, y_Fields+BorderWidth);
    glEnd();
    DrawMenuBar();
    DrawMenu();
  }//if
  else if (InReport())
  {
    DrawReport();
  }//if
  else
  {
    //draw the normal america view with map and stuff

    //draw borders
    glBegin(GL_QUADS);
      glColor3ubv(&BorderColour[0]);
      //vertical border between map/ sidebar
      glVertex2f(x_Fields, 0.0);
      glVertex2f(x_Fields+BorderWidth, 0.0);
      glVertex2f(x_Fields+BorderWidth, y_Fields);
      glVertex2f(x_Fields, y_Fields);

      //horizontal border between map & (later to come) menu
      glVertex2f(0.0, y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields+BorderWidth);
      glVertex2f(0.0, y_Fields+BorderWidth);

      //horizontal bar between minimap & rest of bar
      glVertex2f(x_Fields, y_Fields - 2*BorderWidth -2*PixelWidth*MiniMap_y_Fields);
      glVertex2f(x_Fields, y_Fields - 3*BorderWidth -2*PixelWidth*MiniMap_y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth,
                 y_Fields - 3*BorderWidth -2*PixelWidth*MiniMap_y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth,
                 y_Fields - 2*BorderWidth -2*PixelWidth*MiniMap_y_Fields);
    glEnd();//borders

    //draw the real map
    TMap& tempMap = TMap::GetSingleton();
    LongInt i, j;
    for (i= OffsetX; i<=OffsetX +x_Fields-1; ++i)
      for (j= OffsetY; j<=OffsetY +y_Fields-1; ++j)
      {
        if (m_TerrainTexNames[tempMap.tiles[i][j]->m_Type]==0)
        {
          glBegin(GL_QUADS);
            switch (tempMap.tiles[i][j]->m_Type)
            {
              case ttArctic: glColor3f(1.0, 1.0, 1.0); break;//white
              case ttSea: glColor3f(0.0, 0.0, 1.0); break; //blue
              case ttOpenSea: glColor3f(0.3, 0.3, 1.0); break;//lighter blue
              case ttHills:
              case ttMountains: glColor3f(0.5, 0.0, 0.0); break;
              default:
              glColor3f(0.3, 1.0, 0.3); break;//some greenish stuff
            }//swi
            glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
            glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
            glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
            glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
          glEnd();
        }//if-then
        else
        {
          glEnable(GL_TEXTURE_2D);
          glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[tempMap.tiles[i][j]->m_Type]);
          glBegin(GL_QUADS);
            glColor3f(1.0, 1.0, 1.0);
            glTexCoord2f(0.0, 1.0);
            glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
            glTexCoord2f(0.0, 0.0);
            glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
            glTexCoord2f(1.0, 0.0);
            glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
            glTexCoord2f(1.0, 1.0);
            glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
          glEnd();
          glDisable(GL_TEXTURE_2D);
        }//else branch

        //check for river and draw, if present
          //still to do here
        if (tempMap.GetRiverType(i,j)!=cMapRiverNone)
        {
          //determine texture name
          GLuint tex;
          switch (tempMap.GetRiverType(i,j))
          {
            case cMapRiverNorth:
            case cMapRiverEast:
            case cMapRiverSouth:
            case cMapRiverWest:
                 tex = m_RiverTexNames[rtOne];
                 break;
            case cMapRiverNS:
            case cMapRiverEW:
                 tex = m_RiverTexNames[rtTwo_Straight];
                 break;
            case cMapRiverNE:
            case cMapRiverSE:
            case cMapRiverSW:
            case cMapRiverNW:
                 tex = m_RiverTexNames[rtTwo_Bend];
                 break;
            case cMapRiverNotN:
            case cMapRiverNotE:
            case cMapRiverNotS:
            case cMapRiverNotW:
                 tex = m_RiverTexNames[rtThree];
                 break;
            default:
                 tex = 0;
                 break;
          }//swi
          if (tex!=0)
          {
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_ALPHA_TEST);
            glBindTexture(GL_TEXTURE_2D, tex);
            glBegin(GL_QUADS);
              glColor3f(1.0, 1.0, 1.0);
              switch (tempMap.GetRiverType(i,j))
              {
                case cMapRiverNorth:
                case cMapRiverNS:
                case cMapRiverNE:
                case cMapRiverNotW:
                case cMapRiverAll:
                     //no rotation needed
                     glTexCoord2f(0.0, 1.0);
                     glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                     glTexCoord2f(0.0, 0.0);
                     glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(1.0, 0.0);
                     glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(1.0, 1.0);
                     glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                     break;//no rotation
                case cMapRiverWest:
                case cMapRiverEW:
                case cMapRiverNW:
                case cMapRiverNotS:
                     //rotation: 90° (positive direction)
                     glTexCoord2f(1.0, 1.0);
                     glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                     glTexCoord2f(0.0, 1.0);
                     glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(0.0, 0.0);
                     glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(1.0, 0.0);
                     glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                     break; //rot.: 90°
                case cMapRiverSouth:
                case cMapRiverSW:
                case cMapRiverNotE:
                     //rotation: 180° (positive direction)
                     glTexCoord2f(1.0, 0.0);
                     glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                     glTexCoord2f(1.0, 1.0);
                     glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(0.0, 1.0);
                     glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(0.0, 0.0);
                     glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                     break; //rot.: 180°
                case cMapRiverEast:
                case cMapRiverSE:
                case cMapRiverNotN:
                     //rotation: 270° (positive direction)
                     glTexCoord2f(0.0, 0.0);
                     glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                     glTexCoord2f(1.0, 0.0);
                     glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(1.0, 1.0);
                     glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                     glTexCoord2f(0.0, 1.0);
                     glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                     break; //rot.: 270°
              }//swi
            glEnd();
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          }//if river texture present
        }//if river present

        //check for unit and draw unit icon, if present
        TUnit* tempUnit = TData::GetSingleton().GetFirstUnitInXY(i,j);
        if (tempUnit!=NULL)
        {
          if (m_UnitTexNames[tempUnit->GetType()]!=0)
          {
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_ALPHA_TEST);
            DrawUnitIcon(tempUnit, i-OffsetX, -j-1+y_Fields+OffsetY, false, true);
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          }//if
        }//if

        //check for colony and draw icon, if present
        TColony* tempColony = TData::GetSingleton().GetColonyInXY(i,j);
        if (tempColony!=NULL)
        {
          if (m_ColonyTexNames[0]!=0)
          {
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_ALPHA_TEST);
            glBindTexture(GL_TEXTURE_2D, m_ColonyTexNames[0]);
            glBegin(GL_QUADS);
              glColor3f(1.0, 1.0, 1.0);
              glTexCoord2f(0.0, 1.0);
              glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
              glTexCoord2f(0.0, 0.0);
              glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
              glTexCoord2f(1.0, 0.0);
              glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
              glTexCoord2f(1.0, 1.0);
              glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
            glEnd();
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          }//if
        }//if
        else
        {
          TTribe* tempTribe = TData::GetSingleton().GetTribeInXY(i,j);
          if (tempTribe!=NULL)
          {
            if ((tempTribe->GetNation()>=cMinIndian) and (tempTribe->GetNation()<=cMaxIndian))
            {
              if (m_TribeTexNames[tempTribe->GetNation()]!=0)
              {
                glEnable(GL_TEXTURE_2D);
                glEnable(GL_ALPHA_TEST);
                glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[tempTribe->GetNation()]);
                glBegin(GL_QUADS);
                  glColor3f(1.0, 1.0, 1.0);
                  glTexCoord2f(0.0, 1.0);
                  glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                  glTexCoord2f(0.0, 0.0);
                  glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                  glTexCoord2f(1.0, 0.0);
                  glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                  glTexCoord2f(1.0, 1.0);
                  glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                glEnd();
                glDisable(GL_ALPHA_TEST);
                glDisable(GL_TEXTURE_2D);
              }//if
            }//if
          }//if
        }//else
      }//for
    //end of map

    //draw the MiniMap

    //draw border (as a rectangle larger than minimap)
    glBegin(GL_QUADS);
      glColor3ub(157, 86, 20);
      glVertex2f(x_Fields+ 22*PixelWidth, y_Fields);
      glVertex2f(x_Fields+ 22*PixelWidth,
                 y_Fields-2*BorderWidth - MiniMap_y_Fields*2*PixelWidth);
      glVertex2f(x_Fields+ (BarWidth-22)*PixelWidth,
                 y_Fields-2*BorderWidth - MiniMap_y_Fields*2*PixelWidth);
      glVertex2f(x_Fields+ (BarWidth-22)*PixelWidth, y_Fields);
    glEnd();

    //draw the actual minimap
    glBegin(GL_QUADS);
      for (i =0; i<=MiniMap_x_Fields-1; ++i)
        for (j= MiniMapOffset_Y; j<=MiniMapOffset_Y +MiniMap_y_Fields-1; ++j)
        {
          glColor3ubv(&(cMapColour[tempMap.tiles[i][j]->m_Type][0]));
          glVertex3f(x_Fields + (24+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MiniMapOffset_Y)*2*PixelWidth, 0.1);
          glVertex3f(x_Fields + (24+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MiniMapOffset_Y+1)*2*PixelWidth, 0.1);
          glVertex3f(x_Fields + (26+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MiniMapOffset_Y+1)*2*PixelWidth, 0.1);
          glVertex3f(x_Fields + (26+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MiniMapOffset_Y)*2*PixelWidth, 0.1);
        }//for
    glEnd();//MiniMap
    DrawMenuBar();
    //display side bar information
    // - season and year
    WriteText(TLanguage::GetSingleton().GetSeason(TData::GetSingleton().IsAutumn())+" "+IntToStr(TData::GetSingleton().GetYear()),
              x_Fields + 4*PixelWidth,
              y_Fields - 3*BorderWidth -2*PixelWidth*MiniMap_y_Fields- 16*PixelWidth);
    // - info about focused unit
    if (focused!=NULL)
    {
      if (m_UnitTexNames[focused->GetType()]!=0)
      {
        //draw unit icon
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[focused->GetType()]);
        glBegin(GL_QUADS);
          glColor3f(1.0, 1.0, 1.0);
          glTexCoord2f(0.0, 0.0);
          glVertex2f(x_Fields + 4*PixelWidth, 7.0);
          glTexCoord2f(1.0, 0.0);
          glVertex2f(x_Fields + 36*PixelWidth, 7.0);
          glTexCoord2f(1.0, 1.0);
          glVertex2f(x_Fields + 36*PixelWidth, 8.0);
          glTexCoord2f(0.0, 1.0);
          glVertex2f(x_Fields + 4*PixelWidth, 8.0);
        glEnd();
        glDisable(GL_TEXTURE_2D);
      }//if Icon present
      // -- moves of unit
      glColor3ubv(&cMenuTextColour[0]);
      WriteText(TLanguage::GetSingleton().GetOthers(osMoves)+": "+IntToStr(focused->MovesLeft),
                x_Fields +40*PixelWidth, 7.5);
      // -- location of unit
      WriteText(TLanguage::GetSingleton().GetOthers(osLocation)+": "+IntToStr(focused->GetPosX())+","+IntToStr(focused->GetPosY()),
                x_Fields +40*PixelWidth, 7.0);
      // -- type of unit
      WriteText(TLanguage::GetSingleton().GetUnitName(focused->GetType()),
                x_Fields +4*PixelWidth, 6.5);
      // -- terrain of unit's location
      WriteText(TLanguage::GetSingleton().GetTerrainName(tempMap.tiles[focused->GetPosX()][focused->GetPosY()]->GetType()),
                x_Fields +4*PixelWidth, 6.0);
      // -- number of tools (if present)
      if (focused->GetToolAmount()>0)
        WriteText(IntToStr(focused->GetToolAmount())+" "+TLanguage::GetSingleton().GetGoodName(gtTool),
                  x_Fields +4*PixelWidth, 5.5);

    }//if Focused unit present

    //draw menu, if present
    DrawMenu();

  }//if America view

  //show the text messages, if present
  DrawMessage();
  glutSwapBuffers();
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.Draw\n";
  #endif
}//TGui.Draw

void TGui::DrawColonyView()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.DrawColonyView\n";
  #endif
  //draw border
  glBegin(GL_QUADS);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(0.0, y_Fields);
    glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields);
    glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields+BorderWidth);
    glVertex2f(0.0, y_Fields+BorderWidth);
  glEnd();
  //border around field map
  glLineWidth(2.0);
  glBegin(GL_LINE_STRIP);
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields);
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.0);
    glVertex2f(cWindowWidth*PixelWidth, y_Fields-5.0);
  glEnd();
  //draw fields
  TMap& local_Map = TMap::GetSingleton();
  ShortInt i, j;
  for (i = -1; i<=1; ++i)
    for (j= -1; j<=1; ++j)
    {
      //draw terrain
      if (m_TerrainTexNames[local_Map.tiles[i+cur_colony->GetPosX()][j+cur_colony->GetPosY()]->m_Type]==0)
      {
        #ifdef DEBUG_CODE
          std::cout <<"TGui.DrawColonyView: Trying to draw flat terrain in "<<i<<","<<j<<"\n";
        #endif
        glBegin(GL_QUADS);
        switch (local_Map.tiles[i+cur_colony->GetPosX()][j+cur_colony->GetPosY()]->m_Type)
        {
          case ttArctic: glColor3f(1.0, 1.0, 1.0); break;//white
          case ttSea: glColor3f(0.0, 0.0, 1.0); break;//blue
          case ttOpenSea: glColor3f(0.3, 0.3, 1.0); break;//lighter blue
          case ttHills:
          case ttMountains: glColor3f(0.5, 0.0, 0.0); break;
          default: glColor3f(0.3, 1.0, 0.3); break;//some greenish stuff
        }//swi
          glVertex2f(i+x_Fields+2.0, y_Fields-3.0-j);//lower left corner
          glVertex2f(i+x_Fields+3.0, y_Fields-3.0-j);
          glVertex2f(i+x_Fields+3.0, y_Fields-2.0-j);
          glVertex2f(i+x_Fields+2.0, y_Fields-2.0-j);
        glEnd();
      }//if-then
      else
      {
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[local_Map.tiles[i+cur_colony->GetPosX()][j+cur_colony->GetPosY()]->m_Type]);
        glBegin(GL_QUADS);
          glColor3f(1.0, 1.0, 1.0);
          glTexCoord2f(0.0, 0.0);
          glVertex2f(i+x_Fields+2.0, y_Fields-3.0-j);//lower left corner
          glTexCoord2f(1.0, 0.0);
          glVertex2f(i+x_Fields+3.0, y_Fields-3.0-j);
          glTexCoord2f(1.0, 1.0);
          glVertex2f(i+x_Fields+3.0, y_Fields-2.0-j);
          glTexCoord2f(0.0, 1.0);
          glVertex2f(i+x_Fields+2.0, y_Fields-2.0-j);
        glEnd();
        glDisable(GL_TEXTURE_2D);
      }//else
      //draw units working there
      if (cur_colony->GetUnitInField(i,j)!=NULL)
      {
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        DrawUnitIcon(cur_colony->GetUnitInField(i,j), i+x_Fields+2.0, y_Fields-3.0-j, true, false);
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
      }//if
    }//for
  //show text for field
  GetColonyFieldAtMouse(i,j);
  if ((i!=-2) and (cur_colony->GetUnitInField(i,j)!=NULL))
  {
    std::string tempStr = IntToStr(local_Map.tiles[i+cur_colony->GetPosX()][j+cur_colony->GetPosY()]->GetGoodProduction(
                   cur_colony->GetUnitInFieldGood(i,j), (cur_colony->GetUnitInField(i,j)->GetType()==GetUnitForGood(cur_colony->GetUnitInFieldGood(i,j)))
                   and (cur_colony->GetUnitInField(i,j)->GetType()!=utCriminal)))
               +" "+TLanguage::GetSingleton().GetGoodName(cur_colony->GetUnitInFieldGood(i,j));
    LongInt str_width = tempStr.length()*8;
    glColor3ubv(&cMenuTextColour[0]);
    WriteText(tempStr, x_Fields+2.5-(str_width*PixelWidth*0.5), y_Fields - 0.75);
  }//if

  DrawColonyTitleBar();
  DrawGoodsBar();

  //draw page switchers
  // -- highlighted section
  glColor3f(0.83*0.8, 0.66*0.8, 0.39*0.8);
  if (ColonyBuildingPage)
  {
    glBegin(GL_QUADS);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-6.0);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-6.0);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.5);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.5);
    glEnd();
  }
  else
  {
    glBegin(GL_QUADS);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.5);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.5);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.0);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.0);
    glEnd();
  }//else

  // -- borders
  glLineWidth(2.0);
  glBegin(GL_LINE_STRIP);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.0);
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-6.0);
    glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-6.0);
    glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.0);
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.0);
    //separating line
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.5);
    glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.5);
  glEnd();

  // -- captions
  glColor3ubv(&cMenuTextColour[0]);
  WriteText("Buildings", cWindowWidth*PixelWidth-5.0+4*PixelWidth, y_Fields-6.0+4*PixelWidth);
  WriteText("Units", cWindowWidth*PixelWidth-5.0+4*PixelWidth, y_Fields-5.5+4*PixelWidth);

  std::string tempStr;
  LongInt str_width;
  if (ColonyBuildingPage)
  {
    DrawColonyBuildings();
    //bar for current construction in progress
    // --- background
    glBegin(GL_QUADS);
      glColor3f(0.5, 0.5, 1.0);
      glVertex2f(3.0, 1.25);
      glVertex2f(12.0, 1.25);
      glVertex2f(12.0, 1.75);
      glVertex2f(3.0, 1.75);
    glEnd();
    // --- border
    glLineWidth(2.0);
    glBegin(GL_LINE_LOOP);
      glColor3f(0.25, 0.25, 1.0);
      glVertex2f(3.0, 1.25);
      glVertex2f(12.0, 1.25);
      glVertex2f(12.0, 1.75);
      glVertex2f(3.0, 1.75);
    glEnd();
    // --- text
    TBuildingType bt = cur_colony->GetCurrentConstruction();
    tempStr = TLanguage::GetSingleton().GetBuildingString(bsUnderConstruction)+": "+
              TLanguage::GetSingleton().GetBuildingName(bt, cur_colony->GetBuildingLevel(bt)+1);
    glColor3f(1.0, 1.0, 1.0);
    //WriteText(tempStr , 3.25, 1.375);
    WriteText(tempStr , 7.5-tempStr.length()*PixelWidth*4, 1.375);

    //text, if mouse hovers over buildings
    bt = GetBuildingAtMouse(mouse.x, mouse.y);
    if (bt!=btNone)
    {
      if (cur_colony->GetBuildingLevel(bt)>0)
      {
        tempStr = TLanguage::GetSingleton().GetBuildingName(bt, cur_colony->GetBuildingLevel(bt));
        str_width = 8*tempStr.length();
        //Umrandung zeichnen
        glBegin(GL_QUADS);
          glColor3f(0.0, 0.0, 0.0);
          glVertex2f((mouse.x-2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth-0.5);
          glVertex2f((mouse.x+str_width+2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth-0.5);
          glVertex2f((mouse.x+str_width+2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth);
          glVertex2f((mouse.x-2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth);
        glEnd();
        //Text ausgeben
        glColor3f(1.0, 1.0, 1.0);
        WriteText(tempStr, mouse.x*PixelWidth, (cWindowHeight+3-mouse.y)*PixelWidth-0.5);
      }//level>0
    }//if bt<>btNone
  }//if ColonyBuildingPage
  else
  {
    TUnitArr u_arr;
    u_arr.clear();
    DrawShipsInPort(u_arr);

    //draw units in colony
    u_arr = TData::GetSingleton().GetAllUnitsInColony(cur_colony);
    if (u_arr.size()>0)
    {
      glEnable(GL_ALPHA_TEST);
      glEnable(GL_TEXTURE_2D);
      for (i= 0; i<=Min(23, u_arr.size()-1); ++i)
      {
        DrawUnitIcon(u_arr[i], 14.0 + (i % 6),(cGoodBarHeight+1)*PixelWidth+(i / 6), true, true);
      }
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_ALPHA_TEST);
    }//if u_arr länger als 0

    //check for movable unit in field and draw it
    if (mouse.down)
    {
      GetColonyFieldAtMouse(i,j, mouse.down_x, mouse.down_y);
      if ((i!=-2) and (cur_colony->GetUnitInField(i,j)!=NULL))
      {
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        if (m_UnitTexNames[cur_colony->GetUnitInField(i,j)->GetType()]!=0)
          glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[cur_colony->GetUnitInField(i,j)->GetType()]);
        else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
        i = mouse.x / FieldWidth;
        j = ((mouse.y-16) % FieldWidth)-FieldWidth;
        glBegin(GL_QUADS);
          glColor3f(1.0, 1.0, 1.0);
          glTexCoord2f(0.0, 0.0);
          glVertex2f((mouse.x-i)*PixelWidth, (cWindowHeight-mouse.y+j)*PixelWidth-0.5);//lower left corner
          glTexCoord2f(1.0, 0.0);
          glVertex2f((mouse.x-i)*PixelWidth+1.0, (cWindowHeight-mouse.y+j)*PixelWidth-0.5);
          glTexCoord2f(1.0, 1.0);
          glVertex2f((mouse.x-i)*PixelWidth+1.0, (cWindowHeight-mouse.y+j)*PixelWidth+0.5);
          glTexCoord2f(0.0, 1.0);
          glVertex2f((mouse.x-i)*PixelWidth, (cWindowHeight-mouse.y+j)*PixelWidth+0.5);
        glEnd();
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
      }//if (i<>-2) and ...
      else
      {
        //check for unit moved from "outside" of colony
        str_width = GetColonyUnitAtMouse(mouse.down_x, mouse.down_y);
        if (str_width!=-1)
        {
          u_arr = TData::GetSingleton().GetAllUnitsInColony(cur_colony);
          if (u_arr.size()-1>=str_width)
          {
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_ALPHA_TEST);
            DrawUnitIcon(u_arr[str_width], 14.0+ (str_width % 6)+(mouse.x-mouse.down_x)*PixelWidth,
                            (cGoodBarHeight+1+mouse.down_y-mouse.y)*PixelWidth+(str_width / 6), true, false);
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          }//if
        }//if
        else DrawGoodDraggedFromBar();
      }//else
    }//if mouse.down

  }//else, i.e. not ColonyBuildingPage

  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.DrawColonyView\n";
  #endif
}//proc DrawColonyView

void TGui::GetBuildingPosition(const TBuildingType bt, float& x, float& y) const
{
/*Colony layout:
 (length in Field units (=currently 32px on screen)

  /--0.5--+----3.0------+--0.5--+----3.0-------+--0.5--+-------3.0-----+--0.5--+----3.0---+--0.5--\
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |      free     | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btCarpenter |       | btBlackSmith |       | btChurch      |       | btPress  |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btFurTrader |       | btDistiller  |       | btWeaver      |       | btArmory |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btSchool    |       | btStable     |       | btTobacconist |       |  btDock  |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btWarehouse |       |  btTownHall  |       | nothing(free) |       |  btFort  |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  \-------+-------------+-------+--------------+-------+---------------+-------+----------+-------/
*/
  //assign x
  switch (bt)
  {
    case btCarpenter:
    case btFurTrader:
    case btSchool:
    case btWarehouse: x = 0.5; break;
    case btBlacksmith:
    case btDistiller:
    case btStable:
    case btTownHall: x = 4.0; break;
    case btChurch:
    case btWeaver:
    case btTobacconist: x = 7.5; break;
    case btPress:
    case btArmory:
    case btDock:
    case btFort: x = 11.0; break;
    default: x = 0.0; break;
  }//swi

  //assign y
  switch (bt)
  {
    case btCarpenter:
    case btBlacksmith:
    case btChurch:
    case btPress: y = y_Fields -2.5; break;
    case btFurTrader:
    case btDistiller:
    case btWeaver:
    case btArmory: y = y_Fields -5.0; break;
    case btSchool:
    case btStable:
    case btTobacconist:
    case btDock: y = y_Fields -7.5; break;
    case btWarehouse:
    case btTownHall:
    case btFort: y = y_Fields - 10.0; break;
    default: y = 0.0; break;
  }
}//procedure GetBuildingPosition

void TGui::DrawColonyBuildings()
{
  //buildings
  LongInt i;
  Byte level;
  for (i=Ord(Low(btNone)); i<=Ord(High(btNone)); ++i)
  {
    level = cur_colony->GetBuildingLevel(TBuildingType(i));
    //check for valid level
    if ((level>=1) and (level<=3))
    {
      float x, y;
      GetBuildingPosition(TBuildingType(i), x,y);
      //check, whether texture is loaded
      if (m_BuildingTexNames[TBuildingType(i)][level]!=0)
      {
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        glBindTexture(GL_TEXTURE_2D, m_BuildingTexNames[TBuildingType(i)][level]);
        glBegin(GL_QUADS);
          glColor3f(1.0, 1.0, 1.0);
          glTexCoord2f(0.0, 0.0);
          glVertex2f(x, y);
          glTexCoord2f(0.75, 0.0);
          glVertex2f(x+3.0, y);
          glTexCoord2f(0.75, 1.0);
          glVertex2f(x+3.0, y+2.0);
          glTexCoord2f(0.0, 1.0);
          glVertex2f(x, y+2.0);
        glEnd();
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
      }//building texture present
      //sum up production
      Byte production = 0;
      //draw units in buildings
      for (level= 0; level<=2; ++level)
        if (cur_colony->GetUnitInBuilding(TBuildingType(i), level)!=NULL)
        {
          glEnable(GL_TEXTURE_2D);
          glEnable(GL_ALPHA_TEST);
          DrawUnitIcon(cur_colony->GetUnitInBuilding(TBuildingType(i), level), x+level, y, true, false);
          glDisable(GL_ALPHA_TEST);
          glDisable(GL_TEXTURE_2D);
          production = production + cur_colony->GetProduction(TBuildingType(i), cur_colony->GetUnitInBuilding(TBuildingType(i), level)->GetType());
        }//if unit present in building
      //draw production amount
      if (production>0)
      {
        //draw good icon
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        if (m_GoodTexNames[GetProducedGood(TBuildingType(i))]!=0)
          glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[GetProducedGood(TBuildingType(i))]);
        else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
        glBegin(GL_QUADS);
          glColor3f(1.0, 1.0, 1.0);
          glTexCoord2f(0.0, 0.0);
          glVertex2f(x-0.25, y+1.5);
          glTexCoord2f(1.0, 0.0);
          glVertex2f(x+0.75, y+1.5);
          glTexCoord2f(1.0, 1.0);
          glVertex2f(x+0.75, y+2.5);
          glTexCoord2f(0.0, 1.0);
          glVertex2f(x-0.25, y+2.5);
        glEnd();
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
        //write amount of produced good
        glColor3ubv(&cMenuTextColour[0]);
        WriteText(IntToStr(production), x+1.0, y+2.0);
      }//if production>0
    }//if 1<=Level<=3
  }//for
}//proc DrawColonyBuildings

void TGui::DrawEuropeanView()
{
  //border
  glLineWidth(2.0);
  glBegin(GL_LINES);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(0.0, y_Fields);
    glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields);
  glEnd();
  DrawGoodsBar();
  DrawEuropeTitleBar();

  TUnitArr Ship, Colonists, Expected, NewWorld;
  if (europe!=NULL)
    TData::GetSingleton().GetEuropeanQuartett(europe->GetCount(), Ship, Colonists, Expected, NewWorld);
  else
    TData::GetSingleton().GetEuropeanQuartett(cNationEngland, Ship, Colonists, Expected, NewWorld);

  DrawShipsInPort(Ship);
  DrawPeopleInEurope(Colonists);
  DrawExpectedSoon(Expected);
  DrawShipsToNewWorld(NewWorld);

  DrawEuropeButtons();

  DrawGoodDraggedFromBar();
}//proc

void TGui::DrawGoodDraggedFromBar()
{
  //draw dragged items
  if (mouse.down)
  {
    TGoodType tempGood = GetGoodAtMouse(mouse.down_x, mouse.down_y);
    if (tempGood!=gtCross)
    {
      glColor3f(1.0, 1.0, 1.0);
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_ALPHA_TEST);
      if (m_GoodTexNames[tempGood]!=0) glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[tempGood]);
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
      glBegin(GL_QUADS);
        glTexCoord2f(0.0, 0.0);
        glVertex2f((Ord(tempGood)*38+4+mouse.x-mouse.down_x)*PixelWidth,
                   -0.5+(17+mouse.down_y-mouse.y)*PixelWidth);
        glTexCoord2f(1.0, 0.0);
        glVertex2f((Ord(tempGood)*38+4+mouse.x-mouse.down_x)*PixelWidth+1.0,
                   -0.5+(17+mouse.down_y-mouse.y)*PixelWidth);
        glTexCoord2f(1.0, 1.0);
        glVertex2f((Ord(tempGood)*38+4+mouse.x-mouse.down_x)*PixelWidth+1.0,
                   -0.5+(17+mouse.down_y-mouse.y)*PixelWidth+1.0);
        glTexCoord2f(0.0, 1.0);
        glVertex2f((Ord(tempGood)*38+4+mouse.x-mouse.down_x)*PixelWidth,
                   -0.5+(17+mouse.down_y-mouse.y)*PixelWidth+1.0);
      glEnd();
      glDisable(GL_ALPHA_TEST);
      glDisable(GL_TEXTURE_2D);
    }//if
  }//if
}//proc

void TGui::DrawEuropeButtons()
{
  glBegin(GL_QUADS);
    glColor3f(0.5, 0.5, 1.0);
    //"Buy ships" button
    glVertex2f(9.0, cGoodBarHeight*PixelWidth);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+1.0);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+1.0);
    //"Train units" button
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+1.5);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+1.5);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+2.5);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+2.5);
  glEnd();
  glLineWidth(2.0);
  glBegin(GL_LINE_LOOP);
    //"Buy Ships" button
    glColor3f(0.75, 0.75, 1.0);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth);
    glColor3f(0.25, 0.25, 1.0);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+1.0);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+1.0);
  glEnd();
  glBegin(GL_LINE_LOOP);
    //"Train units" button
    glColor3f(0.75, 0.75, 1.0);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+1.5);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+1.5);
    glColor3f(0.25, 0.25, 1.0);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+2.5);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+2.5);
  glEnd();
  glColor3f(0.2, 0.2, 0.2);
  WriteText("Buy Ship", 9.5, cGoodBarHeight*PixelWidth+0.25);
  WriteText("Train units", 9.125, cGoodBarHeight*PixelWidth+1.75);
}//proc

void TGui::DrawShipsInPort(const TUnitArr& predefShips)
{
  if ((cur_colony==NULL) and (europe==NULL)) return;
  glBegin(GL_QUADS);
    //front of boxes
    glColor3f(cWoodenColour[0]*0.8, cWoodenColour[1]*0.8, cWoodenColour[2]*0.8);
    glVertex2f(1.0, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    glVertex2f(1.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    //"upper face" (isometric)
    glColor3f(cWoodenColour[0]*0.7, cWoodenColour[1]*0.7, cWoodenColour[2]*0.7);
    glVertex2f(1.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    glVertex2f(7.5, (cGoodBarHeight+1)*PixelWidth+1.5);
    glVertex2f(1.5, (cGoodBarHeight+1)*PixelWidth+1.5);
    //"right face" (isometric)
    glColor3f(cWoodenColour[0]*0.6, cWoodenColour[1]*0.6, cWoodenColour[2]*0.6);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(7.5, (cGoodBarHeight+1)*PixelWidth+0.5);
    glVertex2f(7.5, (cGoodBarHeight+1)*PixelWidth+1.5);
  glEnd();
  //separating lines
  glBegin(GL_LINE_STRIP);
    glColor3f(cWoodenColour[0]*0.5, cWoodenColour[1]*0.5, cWoodenColour[2]*0.5);
    glVertex2f(1.0, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    glVertex2f(1.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    ShortInt i;
    for (i=0; i<=5; ++i)
    {
      glVertex2f(1.0+i, (cGoodBarHeight+1)*PixelWidth);
      glVertex2f(2.0+i, (cGoodBarHeight+1)*PixelWidth);
      glVertex2f(1.0+i, (cGoodBarHeight+1)*PixelWidth+1.0);
      glVertex2f(2.0+i, (cGoodBarHeight+1)*PixelWidth+1.0);
    }//for
  glEnd();
  //draw all present ships
  TUnitArr ShipArr;
  if (predefShips.size()>0) ShipArr = predefShips;
  else
  {
    if (cur_colony!=NULL)
      ShipArr = TData::GetSingleton().GetAllShipsInXY(cur_colony->GetPosX(), cur_colony->GetPosY());
    else ShipArr = TData::GetSingleton().GetAllShipsInEurope(TData::GetSingleton().PlayerNation());
  }//else
  if (ShipArr.size()>0)
  {
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    //draw list of ships in port
    for (i= 0; i<ShipArr.size(); ++i)
    {
      if (m_UnitTexNames[ShipArr[i]->GetType()]!=0) glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[ShipArr[i]->GetType()]);
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
      glBegin(GL_QUADS);
        glTexCoord2f(0.0, 0.0);
        glVertex2f(1.0+(i % 6), (cGoodBarHeight+1)*PixelWidth+1.0 +(i / 6));
        glTexCoord2f(1.0, 0.0);
        glVertex2f(2.0+(i % 6), (cGoodBarHeight+1)*PixelWidth+1.0 +(i / 6));
        glTexCoord2f(1.0, 1.0);
        glVertex2f(2.0+(i % 6), (cGoodBarHeight+1)*PixelWidth+2.0 +(i / 6));
        glTexCoord2f(0.0, 1.0);
        glVertex2f(1.0+(i % 6), (cGoodBarHeight+1)*PixelWidth+2.0 +(i / 6));
      glEnd();
    }//for
    //draw icons of goods in first ship
    for (i= 0; i<=5; ++i)
    {
      if (ShipArr[0]->GetCargoAmountBySlot(i)>0)
      {
        if (m_GoodTexNames[ShipArr[0]->GetCargoGoodBySlot(i)]!=0)
          glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[ShipArr[0]->GetCargoGoodBySlot(i)]);
        else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
        glBegin(GL_QUADS);
          glTexCoord2f(0.0, 0.0);
          glVertex2f(1.0+i, (cGoodBarHeight+1)*PixelWidth);
          glTexCoord2f(1.0, 0.0);
          glVertex2f(2.0+i, (cGoodBarHeight+1)*PixelWidth);
          glTexCoord2f(1.0, 1.0);
          glVertex2f(2.0+i, (cGoodBarHeight+1)*PixelWidth+1.0);
          glTexCoord2f(0.0, 1.0);
          glVertex2f(1.0+i, (cGoodBarHeight+1)*PixelWidth+1.0);
        glEnd();
      }//if
    }//for
    glDisable(GL_TEXTURE_2D);

    //draw dragged good
    if (mouse.down)
    {
      i = GetCargoBoxAtMouse(mouse.down_x, mouse.down_y);
      if (i!=-1)
      {
        if (ShipArr[0]->GetCargoAmountBySlot(i)>0)
        {
          glColor3f(1.0, 1.0, 1.0);
          glEnable(GL_TEXTURE_2D);
          if (m_GoodTexNames[ShipArr[0]->GetCargoGoodBySlot(i)]!=0) glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[ShipArr[0]->GetCargoGoodBySlot(i)]);
          else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
          glBegin(GL_QUADS);
            glTexCoord2f(0.0, 0.0);
            glVertex2f(1.0+i+(mouse.x-mouse.down_x)*PixelWidth,
                       (cGoodBarHeight+1+mouse.down_y-mouse.y)*PixelWidth);
            glTexCoord2f(1.0, 0.0);
            glVertex2f(1.0+i+(mouse.x-mouse.down_x)*PixelWidth+1.0,
                       (cGoodBarHeight+1+mouse.down_y-mouse.y)*PixelWidth);
            glTexCoord2f(1.0, 1.0);
            glVertex2f(1.0+i+(mouse.x-mouse.down_x)*PixelWidth+1.0,
                       (cGoodBarHeight+1+mouse.down_y-mouse.y)*PixelWidth+1.0);
            glTexCoord2f(0.0, 1.0);
            glVertex2f(1.0+i+(mouse.x-mouse.down_x)*PixelWidth,
                       (cGoodBarHeight+1+mouse.down_y-mouse.y)*PixelWidth+1.0);
          glEnd();
          glDisable(GL_TEXTURE_2D);
        }//if
      }//if i<>-1
    }//if mouse.down
  }//if length>0
}//proc DrawShipsInPort

void TGui::DrawPeopleInEurope(const TUnitArr& People)
{
  if (europe==NULL) return;
  glBegin(GL_QUADS);
    //Anlegesteg
    glColor3f(cWoodenColour[0]*0.8, cWoodenColour[1]*0.8, cWoodenColour[2]*0.8);
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(cWindowWidth*PixelWidth, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(cWindowWidth*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.5);
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.5);
    //"Begrenzung" (linksseitiger Abschluss)
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.75);
    glVertex2f((cWindowWidth-6.5*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.75);
    glVertex2f((cWindowWidth-6.5*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth-0.25);
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth-0.25);
  glEnd();
  //Umrandung (border)
  glLineWidth(2.0);
  glBegin(GL_LINE_LOOP);
    //Anlegesteg
    glColor3f(cWoodenColour[0]*0.6, cWoodenColour[1]*0.6, cWoodenColour[2]*0.6);
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(cWindowWidth*PixelWidth, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(cWindowWidth*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.5);
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.5);
    //"Begrenzung" (linksseitiger Abschluss)
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.75);
    glVertex2f((cWindowWidth-6.5*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth+0.75);
    glVertex2f((cWindowWidth-6.5*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth-0.25);
    glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth, (cGoodBarHeight+1)*PixelWidth-0.25);
  glEnd();

  TUnitArr PeopleArr;
  if (People.size()>0) PeopleArr = People;
  else PeopleArr = TData::GetSingleton().GetAllNonShipsInEurope(europe->GetCount());

  if (PeopleArr.size()>0)
  {
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    //draw list of ships in port
    ShortInt i;
    for (i = 0; i<PeopleArr.size(); ++i)
    {
      if (m_UnitTexNames[PeopleArr[i]->GetType()]!=0) glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[PeopleArr[i]->GetType()]);
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
      glBegin(GL_QUADS);
        glColor3f(1.0, 1.0, 1.0);
        glTexCoord2f(0.0, 0.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+(i % 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i / 6));
        glTexCoord2f(1.0, 0.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+1.0+(i % 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i / 6));
        glTexCoord2f(1.0, 1.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+1.0+(i % 6), (cGoodBarHeight+1)*PixelWidth+1.5 +(i / 6));
        glTexCoord2f(0.0, 1.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+(i % 6), (cGoodBarHeight+1)*PixelWidth+1.5 +(i / 6));
      glEnd();
      DrawStateIcon(PeopleArr[i]->GetState(), (cWindowWidth-6*FieldWidth)*PixelWidth+(i % 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i / 6));
    }//for
    glDisable(GL_TEXTURE_2D);
  }//if
}//proc

void TGui::DrawExpectedSoon(const TUnitArr& ExpSoon)
{
  //border
  glLineWidth(2.0);
  glBegin(GL_LINE_LOOP);
    glColor3fv(&cBlueBorderColour[0]);
    glVertex2f(1.0, y_Fields - 1.0);
    glVertex2f(1.0, y_Fields - 3.0);
    glVertex2f(1.0 +cShipsInExpectedSoon+1, y_Fields - 3.0);
    glVertex2f(1.0 +cShipsInExpectedSoon+1, y_Fields - 1.0);
  glEnd();
  WriteText("Expected soon", 1.5, y_Fields -0.75);
  if (ExpSoon.size()>0)
  {
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    LongInt i;
    for (i= 0; i<=Min(ExpSoon.size()-1,cShipsInExpectedSoon-1); ++i)
    {
      if (m_UnitTexNames[ExpSoon[i]->GetType()]!=0) glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[ExpSoon[i]->GetType()]);
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
      glBegin(GL_QUADS);
        glTexCoord2f(0.0, 0.0);
        glVertex2f(1.5+i, y_Fields - 2.5);
        glTexCoord2f(1.0, 0.0);
        glVertex2f(2.5+i, y_Fields - 2.5);
        glTexCoord2f(1.0, 1.0);
        glVertex2f(2.5+i, y_Fields - 1.5);
        glTexCoord2f(0.0, 1.0);
        glVertex2f(1.5+i, y_Fields - 1.5);
      glEnd();
    }//for
    glDisable(GL_TEXTURE_2D);
  }//if
}//proc

void TGui::DrawShipsToNewWorld(const TUnitArr& ToNewWorld)
{
  //border
  glLineWidth(2.0);
  glBegin(GL_LINE_LOOP);
    glColor3fv(&cBlueBorderColour[0]);
    glVertex2f(3.0+cShipsInExpectedSoon, y_Fields - 1.0);
    glVertex2f(3.0+cShipsInExpectedSoon, y_Fields - 3.0);
    glVertex2f(4.0+cShipsInExpectedSoon+cShipsInToNewWorld, y_Fields - 3.0);
    glVertex2f(4.0+cShipsInExpectedSoon+cShipsInToNewWorld, y_Fields - 1.0);
  glEnd();
  WriteText("Ziel: Neue Welt", 3.5+cShipsInExpectedSoon, y_Fields -0.75);
  if (ToNewWorld.size()>0)
  {
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    LongInt i;
    for (i= 0; i<=Min(ToNewWorld.size()-1,cShipsInToNewWorld-1); ++i)
    {
      if (m_UnitTexNames[ToNewWorld[i]->GetType()]!=0) glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[ToNewWorld[i]->GetType()]);
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
      glBegin(GL_QUADS);
        glTexCoord2f(0.0, 0.0);
        glVertex2f(3.5+cShipsInExpectedSoon+i, y_Fields - 2.5);
        glTexCoord2f(1.0, 0.0);
        glVertex2f(4.5+cShipsInExpectedSoon+i, y_Fields - 2.5);
        glTexCoord2f(1.0, 1.0);
        glVertex2f(4.5+cShipsInExpectedSoon+i, y_Fields - 1.5);
        glTexCoord2f(0.0, 1.0);
        glVertex2f(3.5+cShipsInExpectedSoon+i, y_Fields - 1.5);
      glEnd();
    }//for
    glDisable(GL_TEXTURE_2D);
  }//if
}//proc

void TGui::DrawReport()
{
  //only economy and fleet implemented yet
  LongInt i, j;
  TColonyArr col_arr;
  TUnitArr u_arr;
  LongInt freight_offset;

  switch (report)
  {
    case rtEconomy:
         col_arr = TData::GetSingleton().GetColonyList(TData::GetSingleton().PlayerNation());

         //draw good icons
         glEnable(GL_TEXTURE_2D);
         glEnable(GL_ALPHA_TEST);
         glColor3f(1.0, 1.0, 1.0);
         for (i= Ord(gtFood); i<=Ord(gtMusket); ++i)
         {
           if (m_GoodTexNames[TGoodType(i)]!=0) glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[TGoodType(i)]);
           else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
           glBegin(GL_QUADS);
             glTexCoord2f(0.0, 0.0);
             glVertex2f(4+i-Ord(gtFood), y_Fields-2);
             glTexCoord2f(1.0, 0.0);
             glVertex2f(4+i-Ord(gtFood)+1, y_Fields-2);
             glTexCoord2f(1.0, 1.0);
             glVertex2f(4+i-Ord(gtFood)+1, y_Fields-1);
             glTexCoord2f(0.0, 1.0);
             glVertex2f(4+i-Ord(gtFood), y_Fields-1);
           glEnd();
         }//for
         glDisable(GL_ALPHA_TEST);
         glDisable(GL_TEXTURE_2D);

         //seperating lines
         glColor3f(0.0, 0.0, 0.0);
         glLineWidth(2.0);
         glBegin(GL_LINES);
         for (i= y_Fields-2; i>=1; --i)
         {
           glVertex2f(0.0, i);
           glVertex2f(cWindowWidth*PixelWidth, i);
           glVertex2f(0.0, i-0.5);
           glVertex2f(cWindowWidth*PixelWidth, i-0.5);
         }//for
         for (i= Ord(gtFood); i<=Ord(gtMusket); ++i)
         {
           glVertex2f(4+i-Ord(gtFood), y_Fields-2);
           glVertex2f(4+i-Ord(gtFood), 0.0);
         }//for
         glEnd();

         //print storage amounts
         if (col_arr.size()>0)
         {
           glColor3ubv(&cMenuTextColour[0]);
           for (i= 0; i<col_arr.size(); ++i)
           {
             WriteText(col_arr[i]->GetName(), PixelWidth, y_Fields-2.5+4*PixelWidth-i*0.5);
             for (j= Ord(gtFood); j<=Ord(gtMusket); ++j)
               WriteText(IntToStr(col_arr[i]->GetStore(TGoodType(j))), 4+2*PixelWidth+j-Ord(gtFood), y_Fields-2.5+4*PixelWidth-i*0.5);
           }//for
         }//if
         else
         {
           glColor3f(0.0, 0.0, 0.0);
           i = TextWidthTimesRoman24("You have no colonies yet.");
           j = (cWindowWidth-i) / 2;
           glBegin(GL_QUADS);
             glVertex2f(j*PixelWidth-1.0, y_Fields-4.75);
             glVertex2f((i+j)*PixelWidth+1.0, y_Fields-4.75);
             glVertex2f((i+j)*PixelWidth+1.0, y_Fields-3.75);
             glVertex2f(j*PixelWidth-1.0, y_Fields-3.75);
           glEnd();
           glColor3ub(255, 0, 0);
           WriteTimesRoman24("You have no colonies yet.", j*PixelWidth, y_Fields-4.5);
         }//else
         break;//case rtEconomy
    case rtFleet:
         u_arr = TData::GetSingleton().GetAllShips(TData::GetSingleton().PlayerNation());

         //headings
         glColor3ubv(&cMenuTextColour[0]);
         WriteText(TLanguage::GetSingleton().GetOthers(osShip), 0.5, y_Fields-0.75);
         WriteText(TLanguage::GetSingleton().GetOthers(osFreight), 5.5, y_Fields-0.75);
         WriteText(TLanguage::GetSingleton().GetOthers(osLocation), 11.5, y_Fields-0.75);
         WriteText(TLanguage::GetSingleton().GetOthers(osDestination), 15.5, y_Fields-0.75);
         //line grid
         glColor3f(0.0, 0.0, 0.0);
         glLineWidth(2.0);
         glBegin(GL_LINES);
           for (i = y_Fields-1; i>= 1; --i)
           {
             glVertex2f(0.0, i);
             glVertex2f(cWindowWidth*PixelWidth, i);
           }//for
           glVertex2f(5.0, y_Fields-1);
           glVertex2f(5.0, 1);
           glVertex2f(11.0, y_Fields-1);
           glVertex2f(11.0, 1);
           glVertex2f(15.0, y_Fields-1);
           glVertex2f(15.0, 1);
         glEnd();

         //display ships
         if (u_arr.size()>0)
         {
           for (i= 0; i<=Min(u_arr.size()-1,cFleetReportUnitsPerPage-1); ++i)
           {
             //unit and name
             glEnable(GL_ALPHA_TEST);
             glEnable(GL_TEXTURE_2D);
             DrawUnitIcon(u_arr[i], 0.0, y_Fields-2-i, true, true);
             glDisable(GL_TEXTURE_2D);
             glDisable(GL_ALPHA_TEST);
             glColor3ubv(&cMenuTextColour[0]);
             WriteText(TLanguage::GetSingleton().GetUnitName(u_arr[i]->GetType()), 1.125, y_Fields-2-i+3*PixelWidth);

             freight_offset = 5;
             //draw goods in ship
             for (j = 0; j<u_arr[i]->FreightCapacity(); ++j)
             {
               if (u_arr[i]->GetCargoAmountBySlot(j)>0)
               {
                 glEnable(GL_ALPHA_TEST);
                 glEnable(GL_TEXTURE_2D);
                 if (m_GoodTexNames[u_arr[i]->GetCargoGoodBySlot(j)]!=0)
                   glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[u_arr[i]->GetCargoGoodBySlot(j)]);
                 else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
                 glBegin(GL_QUADS);
                   glColor3f(1.0, 1.0, 1.0);
                   glTexCoord2f(0.0, 0.0);
                   glVertex2f(freight_offset, y_Fields-2-i);
                   glTexCoord2f(1.0, 0.0);
                   glVertex2f(freight_offset+1.0, y_Fields-2-i);
                   glTexCoord2f(1.0, 1.0);
                   glVertex2f(freight_offset+1.0, y_Fields-1-i);
                   glTexCoord2f(0.0, 1.0);
                   glVertex2f(freight_offset, y_Fields-1-i);
                 glEnd();
                 glDisable(GL_TEXTURE_2D);
                 glDisable(GL_ALPHA_TEST);
                 freight_offset = freight_offset+1;
               }//if
             }//for j
             //draw units in ship
             for (j= 0; j<u_arr[i]->FreightCapacity(); ++j)
             {
               if (u_arr[i]->GetPassengerBySlot(j)!=NULL)
               {
                 glEnable(GL_ALPHA_TEST);
                 glEnable(GL_TEXTURE_2D);
                 DrawUnitIcon(u_arr[i]->GetPassengerBySlot(j), freight_offset, y_Fields-2-i, true, true);
                 glDisable(GL_TEXTURE_2D);
                 glDisable(GL_ALPHA_TEST);
                 freight_offset = freight_offset+1;
               }//if
             }//for j

             //write location
             glColor3ubv(&cMenuTextColour[0]);
             switch (u_arr[i]->GetLocation())
             {
               case ulEurope:
                    WriteText(TLanguage::GetSingleton().GetPortName(u_arr[i]->GetNation()), 11.125, y_Fields-2-i+3*PixelWidth);
                    break;
               case ulGoToEurope:
               case ulGoToNewWorld:
                    WriteText(TLanguage::GetSingleton().GetOthers(osHighSea), 11.125, y_Fields-2-i+3*PixelWidth);
                    break;
               default:
                    WriteText("("+IntToStr(u_arr[i]->GetPosX())+","+IntToStr(u_arr[i]->GetPosY())+")", 11.125, y_Fields-2-i+3*PixelWidth);
                    break;
             }//switch location

             //write destination
             switch (u_arr[i]->GetLocation())
             {
               case ulEurope: break; //write nothing
               case ulGoToEurope:
                    WriteText(TLanguage::GetSingleton().GetPortName(u_arr[i]->GetNation()), 15.125, y_Fields-2-i+3*PixelWidth);
                    break;
               case ulGoToNewWorld:
                    WriteText(TLanguage::GetSingleton().GetOthers(osNewWorld)+" ("+IntToStr(u_arr[i]->GetPosX())+","
                              +IntToStr(u_arr[i]->GetPosY())+")", 15.125, y_Fields-2-i+3*PixelWidth);
                    break;
               case ulAmerica:
                    if (u_arr[i]->GetTask()!=NULL)
                      if (u_arr[i]->GetTask()->GetType()==ttGoTo)
                        WriteText("("+IntToStr((static_cast<TGoToTask*> (u_arr[i]->GetTask()))->DestinationX())+","
                                  +IntToStr((static_cast<TGoToTask*>(u_arr[i]->GetTask()))->DestinationY())+")",
                                  11.125, y_Fields-2-i+3*PixelWidth);
             }//switch location for destination

           }//for i
         }//if
         break;//rtFleet
    case rtColony:
         glColor3ubv(&cMenuTextColour[0]);
         WriteText(TLanguage::GetSingleton().GetMenuOption(mcReports, 2), 3.0, y_Fields-0.25);
         WriteText("Name", 0.5, y_Fields-0.75);
         WriteText("Einheiten", 6.5, y_Fields-0.75);
         col_arr = TData::GetSingleton().GetColonyList(TData::GetSingleton().PlayerNation());
         if (col_arr.size()>0)
         {
           for (i= 0; i<=Min(col_arr.size()-1, y_Fields); ++i)
           {
             if (m_ColonyTexNames[0]!=0)
             {
               glEnable(GL_TEXTURE_2D);
               glEnable(GL_ALPHA_TEST);
               glBindTexture(GL_TEXTURE_2D, m_ColonyTexNames[0]);
               glBegin(GL_QUADS);
                 glColor3f(1.0, 1.0, 1.0);
                 glTexCoord2f(0.0, 0.0);
                 glVertex2f(0.0, y_Fields-2-i);
                 glTexCoord2f(1.0, 0.0);
                 glVertex2f(1.0, y_Fields-2-i);
                 glTexCoord2f(1.0, 1.0);
                 glVertex2f(1.0, y_Fields-1-i);
                 glTexCoord2f(0.0, 1.0);
                 glVertex2f(0.0, y_Fields-1-i);
               glEnd();
               glDisable(GL_ALPHA_TEST);
               glDisable(GL_TEXTURE_2D);
             }//if ColonyTex present
             glColor3ubv(&cMenuTextColour[0]);
             WriteText(IntToStr(col_arr[i]->GetInhabitants()), 1.25, y_Fields-1.75-i);
             WriteText(col_arr[i]->GetName(), 2.25, y_Fields-1.75-i);
             u_arr = TData::GetSingleton().GetAllUnitsInColony(col_arr[i]);
             glEnable(GL_TEXTURE_2D);
             glEnable(GL_ALPHA_TEST);
             for (j= 0; j<= Min(20, u_arr.size()-1); ++j)
             {
               DrawUnitIcon(u_arr[j], 7.0+j, y_Fields-2-i, true, true);
             }//for
             glDisable(GL_ALPHA_TEST);
             glDisable(GL_TEXTURE_2D);
           }//for
         }//if
         else
         {
           glColor3f(0.0, 0.0, 0.0);
           i = TextWidthTimesRoman24("You have no colonies yet.");
           j = (cWindowWidth-i) / 2;
           glBegin(GL_QUADS);
             glVertex2f(j*PixelWidth-1.0, y_Fields-4.75);
             glVertex2f((i+j)*PixelWidth+1.0, y_Fields-4.75);
             glVertex2f((i+j)*PixelWidth+1.0, y_Fields-3.75);
             glVertex2f(j*PixelWidth-1.0, y_Fields-3.75);
           glEnd();
           glColor3ub(255, 0, 0);
           WriteTimesRoman24("You have no colonies yet.", j*PixelWidth, y_Fields-4.5);
         }//else
         break;//rtColony
  }//swi
}//proc

void TGui::DrawMenu()
{
  if (menu_cat!=mcNone)
  {
    LongInt max_len = TLanguage::GetSingleton().GetMaxLen(menu_cat);
    LongInt count = TLanguage::GetSingleton().GetOptionCount(menu_cat);
    GLfloat offset = GetMenuStartX(menu_cat);
    //draw box
    glBegin(GL_QUADS);
      glColor3fv(&cWoodenColour[0]);
      glVertex2f(offset, y_Fields-count*0.5);//lower left
      glVertex2f(offset+ max_len*8*PixelWidth + 1.0, y_Fields-count*0.5);
      glVertex2f(offset+ max_len*8*PixelWidth + 1.0, y_Fields);
      glVertex2f(offset, y_Fields);
    //draw highlighted option box
      glColor3f(0.83*0.8, 0.66*0.8, 0.39*0.8);
      glVertex2f(offset+ 0.25, y_Fields-selected_menu_option*0.5);
      glVertex2f(offset+ max_len*8*PixelWidth + 0.75, y_Fields-selected_menu_option*0.5);
      glVertex2f(offset+ max_len*8*PixelWidth + 0.75, y_Fields+0.5-selected_menu_option*0.5);
      glVertex2f(offset+ 0.25, y_Fields+0.5-selected_menu_option*0.5);
    glEnd();
    //draw border lines
    glLineWidth(2.0);
    glBegin(GL_LINE_LOOP);
      glColor3f(0.0, 0.0, 0.0);
      glVertex2f(offset, y_Fields-count*0.5);//lower left
      glVertex2f(offset+ max_len*8*PixelWidth + 1.0, y_Fields-count*0.5);
      glVertex2f(offset+ max_len*8*PixelWidth + 1.0, y_Fields);
      glVertex2f(offset, y_Fields);
    glEnd();
    //now put the text
    glColor3ubv(&cMenuTextColour[0]);
    LongInt i;
    for (i= 1; i<=count; ++i)
      WriteText(TLanguage::GetSingleton().GetMenuOption(menu_cat, i), 0.5+offset, 3*PixelWidth+ y_Fields-i*0.5);
  }//if
}//proc DrawMenu

void TGui::DrawUnitIcon(const TUnit* the_Unit, const GLfloat left, const GLfloat bottom,
            const bool UseErrorIfTexNotPresent, const bool ShowState)
{
  if (the_Unit!=NULL)
  {
    if ((m_UnitTexNames[the_Unit->GetType()]==0) and not UseErrorIfTexNotPresent) return;
    else if (m_UnitTexNames[the_Unit->GetType()]==0) glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
    else glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[the_Unit->GetType()]);
    //the unit itself
    glBegin(GL_QUADS);
      glColor3f(1.0, 1.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex2f(left, bottom);
      glTexCoord2f(1.0, 0.0);
      glVertex2f(left+1.0, bottom);
      glTexCoord2f(1.0, 1.0);
      glVertex2f(left+1.0, bottom+1.0);
      glTexCoord2f(0.0, 1.0);
      glVertex2f(left, bottom+1.0);
    glEnd();
    if (ShowState and (m_StateTexNames[the_Unit->GetState()]!=0))
    {
      //the state icon
      glBindTexture(GL_TEXTURE_2D, m_StateTexNames[the_Unit->GetState()]);
      glBegin(GL_QUADS);
        if ((the_Unit->GetNation()>=cMin_Nations) and (the_Unit->GetNation()<=cMaxIndian))
          glColor3ubv(&cNationColours[the_Unit->GetNation()][0]);
        glTexCoord2f(0.0, 0.0);
        glVertex2f(left, bottom);
        glTexCoord2f(1.0, 0.0);
        glVertex2f(left+1.0, bottom);
        glTexCoord2f(1.0, 1.0);
        glVertex2f(left+1.0, bottom+1.0);
        glTexCoord2f(0.0, 1.0);
        glVertex2f(left, bottom+1.0);
      glEnd();
    }//if icon
  }//if
}//proc

void TGui::DrawStateIcon(const TUnitState state, const GLfloat left, const GLfloat bottom)
{
  if (m_StateTexNames[state]!=0)
  {
    glBindTexture(GL_TEXTURE_2D, m_StateTexNames[state]);
    glBegin(GL_QUADS);
      glColor3f(1.0, 0.0, 0.0);
      glTexCoord2f(0.0, 0.0);
      glVertex2f(left, bottom);
      glTexCoord2f(1.0, 0.0);
      glVertex2f(left+1.0, bottom);
      glTexCoord2f(1.0, 1.0);
      glVertex2f(left+1.0, bottom+1.0);
      glTexCoord2f(0.0, 1.0);
      glVertex2f(left, bottom+1.0);
    glEnd();
  }//if
}//proc DrawStateIcon

void TGui::DrawMessage()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.DrawMessage\n";
  #endif
  //show message, where neccessary
  if (msg.txt!="")
  {
    LongInt msg_lines, i, msg_opts;
    if (msg.options.size()==0)
    {
      if (msg.inputCaption=="")
      {
        /*we got a simple message, no options, no input :) */
        //get required number of lines
        msg_lines = (msg.txt.length()+59) / 60;
        //draw box
        glBegin(GL_QUADS);
          glColor3f(0.83, 0.66, 0.39);
          glVertex2f(2.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 6.5 +0.25*msg_lines);
          glVertex2f(2.0, 6.5 +0.25*msg_lines);
        glEnd();
        //draw box border
        glLineWidth(2.0);
        glBegin(GL_LINE_LOOP);
          glColor3f(0.0, 0.0, 0.0);//black
          glVertex2f(2.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 6.5 +0.25*msg_lines);
          glVertex2f(2.0, 6.5 +0.25*msg_lines);
        glEnd();
        //write lines
        glColor3ubv(&cMenuTextColour[0]);
        for (i= 1; i<=msg_lines; ++i)
          WriteText(msg.txt.substr((i-1)*60, 60), 2.5, 6.0+0.25*msg_lines-i*0.5);
      }//if
      else
      {
        /*we got an input message window here*/
        //get required number of lines
        msg_lines = (msg.txt.length()+59) / 60;
        //draw box
        glBegin(GL_QUADS);
          glColor3f(0.83, 0.66, 0.39);
          //we have one more line, due to input...
          glVertex2f(2.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 6.75 +0.25*msg_lines);
          glVertex2f(2.0, 6.75 +0.25*msg_lines);
        glEnd();
        //draw box border
        glLineWidth(2.0);
        glBegin(GL_LINE_LOOP);
          glColor3f(0.0, 0.0, 0.0);//black
          glVertex2f(2.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 6.75 +0.25*msg_lines);
          glVertex2f(2.0, 6.75 +0.25*msg_lines);
        glEnd();
        //write lines of message
        glColor3ubv(&cMenuTextColour[0]);
        for (i= 1; i<=msg_lines; ++i)
          WriteText(msg.txt.substr((i-1)*60, 60), 2.5, 6.25+0.25*msg_lines-i*0.5);
        //write caption
        WriteText(msg.inputCaption, 2.5, 5.75 -msg_lines*0.25);
        //write input text
        WriteText(msg.inputText, 3.0+ 0.25*msg.inputCaption.length(), 5.75 -msg_lines*0.25);
        //draw border of "text input box"
        glBegin(GL_LINE_LOOP);
          glVertex2f(2.75+ 0.25*msg.inputCaption.length(), 5.5 -msg_lines*0.25);
          glVertex2f(17.75, 5.5 -msg_lines*0.25);
          glVertex2f(17.75, 6.25 -msg_lines*0.25);
          glVertex2f(2.75+ 0.25*msg.inputCaption.length(), 6.25 -msg_lines*0.25);
        glEnd();
      }//else
    }
    else
    {
      //we got options
      //get required number of lines
      msg_lines = (msg.txt.length()+59) / 60;
      msg_opts = msg.options.size();
      //draw box
      glBegin(GL_QUADS);
        glColor3f(0.83, 0.66, 0.39);
        glVertex2f(2.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 6.5 +0.25*(msg_lines+msg_opts));
        glVertex2f(2.0, 6.5 +0.25*(msg_lines+msg_opts));
      glEnd();
      //draw box border
      glLineWidth(2.0);
      glBegin(GL_LINE_LOOP);
        glColor3f(0.0, 0.0, 0.0);//black
        glVertex2f(2.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 6.5 +0.25*(msg_lines+msg_opts));
        glVertex2f(2.0, 6.5 +0.25*(msg_lines+msg_opts));
      glEnd();
      //write text lines
      glColor3ubv(&cMenuTextColour[0]);
      for (i= 1; i<=msg_lines; ++i)
        WriteText(msg.txt.substr((i-1)*60, 60), 2.5, 6.0+0.25*(msg_lines+msg_opts)-i*0.5);
      //draw highlighted background for current option
      glBegin(GL_QUADS);
        glColor3f(0.83*0.8, 0.66*0.8, 0.39*0.8);
        glVertex2f(2.5, 5.4+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
        glVertex2f(17.5, 5.4+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
        glVertex2f(17.5, 5.9+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
        glVertex2f(2.5, 5.9+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
      glEnd();
      //write options
      glColor3ubv(&cMenuTextColour[0]);
      for (i= 1; i<=msg_opts; ++i)
        WriteText(" "+msg.options[i-1], 2.5, 6.0+0.25*(msg_lines+msg_opts)-(i+msg_lines)*0.5);
    }//if
  }//if
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.MouseFunc\n";
  #endif
} //TGui.DrawMessage

void TGui::CenterOn(const LongInt x, const LongInt y)
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.CenterOn("<<x<<","<<y<<")\n";
  #endif
  OffsetX = x-7;
  if (OffsetX<0) OffsetX = 0;
  else if (OffsetX>cMap_X-x_Fields) OffsetX = cMap_X-x_Fields;
  OffsetY = y-6;
  if (OffsetY<0) OffsetY = 0;
  else if (OffsetY>cMap_Y-y_Fields) OffsetY = cMap_Y-y_Fields;
  //Move Minimap accordingly
  MiniMapOffset_Y = y -(MiniMap_y_Fields / 2);
  if (MiniMapOffset_Y<0) MiniMapOffset_Y =0;
  else if (MiniMapOffset_Y>cMap_Y-MiniMap_y_Fields)
    MiniMapOffset_Y = cMap_Y-MiniMap_y_Fields;
}//proc

void TGui::WriteText(const std::string& msg_txt, const float x, const float y)
{
  #ifdef DEBUG_CODE
    std::cout<< "Entered TGui.WriteText\n";
  #endif
  glRasterPos3f(x, y, 0.2);
  LongInt i;
  for (i=0; i<msg_txt.length(); ++i)
    if (Ord_char(msg_txt[i]) >=32)/* and (Ord(msg_txt[i]) <=127)*/
    {
      glutBitmapCharacter(GLUT_BITMAP_8_BY_13, Ord_char(msg_txt[i]));
    }
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.WriteText\n";
  #endif
}//proc

void TGui::WriteHelvetica12(const std::string& msg_txt, const float x, const float y)
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.WriteHelvetica12\n";
  #endif
  glRasterPos3f(x, y, 0.2);
  LongInt i;
  for (i= 0; i<msg_txt.length(); ++i)
    if (Ord_char(msg_txt[i]) >=32)/* and (Ord(msg_txt[i]) <=127)*/
    {
      glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, Ord_char(msg_txt[i]));
    }
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.WriteHelvetica12\n";
  #endif
}//proc

void TGui::WriteTimesRoman24(const std::string& msg_txt, const float x, const float y)
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.WriteTimes24\n";
  #endif
  glRasterPos3f(x, y, 0.2);
  LongInt i;
  for (i= 0; i<msg_txt.length(); ++i)
    if (Ord_char(msg_txt[i]) >=32)/* and (Ord(msg_txt[i]) <=127)*/
    {
      glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, Ord_char(msg_txt[i]));
    }
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.WriteTimes24\n";
  #endif
}//proc

LongInt TGui::TextWidthTimesRoman24(const std::string& msg_txt)
{
  LongInt Result = 0;
  LongInt i;
  for (i= 0; i<msg_txt.length(); ++i)
    Result = Result+glutBitmapWidth(GLUT_BITMAP_TIMES_ROMAN_24, Ord_char(msg_txt[i]));
  return Result;
}//func

void TGui::DrawMenuBar()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.DrawMenuBar\n";
  #endif
  glColor3ubv(&cMenuTextColour[0]);
  std::string s = TLanguage::GetSingleton().GetMenuLabel(mcGame);
  LongInt i;
  for (i= Ord(Succ(mcGame)); i<=Ord(High(mcGame)); ++i)
    s = s+"  "+TLanguage::GetSingleton().GetMenuLabel(TMenuCategory(i));
  WriteText(s, 0.1, 12.0+5.0*PixelWidth);
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.DrawMenuBar\n";
  #endif
}//proc

void TGui::DrawGoodsBar()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.DrawGoodsBar\n";
  #endif
  std::string price_str;
  LongInt str_width;
  //background
  glBegin(GL_QUADS);
    glColor3ub(76, 100, 172);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, cGoodBarHeight*PixelWidth-0.5);
    glVertex2f(0.0, cGoodBarHeight*PixelWidth-0.5);
  glEnd();
  glLineWidth(2.0);
  //border box
  glBegin(GL_LINE_LOOP);
    glColor3ub(192, 216, 240);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, cGoodBarHeight*PixelWidth -0.5);
    glVertex2f(0.0, cGoodBarHeight*PixelWidth-0.5);
  glEnd();
  //the vertical lines
  LongInt i;
  glBegin(GL_LINES);
    for (i= 1; i<=15; ++i)
    {
      glVertex2f(i*38*PixelWidth, -0.5);
      glVertex2f(i*38*PixelWidth, cGoodBarHeight*PixelWidth -0.5);
    }//for
  glEnd();
  //draw the good icons, if present
  glColor3f(1.0, 1.0, 1.0);
  for (i= Ord(gtFood); i<=Ord(gtMusket); ++i)
  {
    if (m_GoodTexNames[TGoodType(i)]!=0)
    {
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_ALPHA_TEST);
      glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[TGoodType(i)]);
      glBegin(GL_QUADS);
        glTexCoord2f(0.0, 0.0);
        glVertex2f((i*38+4)*PixelWidth, -0.5+17*PixelWidth);
        glTexCoord2f(1.0, 0.0);
        glVertex2f((i*38+36)*PixelWidth, -0.5+17*PixelWidth);
        glTexCoord2f(1.0, 1.0);
        glVertex2f((i*38+36)*PixelWidth, -0.5+49*PixelWidth);
        glTexCoord2f(0.0, 1.0);
        glVertex2f((i*38+4)*PixelWidth, -0.5+49*PixelWidth);
      glEnd();
      glDisable(GL_TEXTURE_2D);
    }//if
  }//for
  //Draw the read E for exit
  glColor3f(1.0, 0.0, 0.0);
  glRasterPos2f((38*16.0+5)*PixelWidth, 0.0);
  glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, Ord_char('E'));
  //colony
  if (cur_colony!=NULL)
  {
    glColor3ub(0,0,0);
    for (i= Ord(gtFood); i<=Ord(gtMusket); ++i)
    {
      WriteText(IntToStr(cur_colony->GetStore(TGoodType(i))), (5+i*38)*PixelWidth, 4*PixelWidth -0.5);
    }//for
  }//if
  //european port view
  else if (europe!=NULL)
  {
    glColor3ub(0,0,0);
    for (i= Ord(gtFood); i<= Ord(gtMusket); ++i)
    {
      price_str = IntToStr(europe->GetPrice(TGoodType(i), true))+"/"
                 +IntToStr(europe->GetPrice(TGoodType(i), false));
      str_width = 0;
      LongInt j;
      for (j = 0; j< price_str.length(); ++j)
        str_width = str_width + glutBitmapWidth(GLUT_BITMAP_HELVETICA_12, Ord_char(price_str[j]));
      WriteHelvetica12(price_str, (2+ ((36-str_width) / 2) +i*38)*PixelWidth, 4*PixelWidth -0.5);
    }//for
  }//else if
  if (GetGoodAtMouse()!=gtCross)
  {
    price_str = TLanguage::GetSingleton().GetGoodName(GetGoodAtMouse());
    str_width = 8*price_str.length();
    //use "i" as temporary var to store the pixel count where the text begins
    if (str_width+mouse.x<cWindowWidth) i = mouse.x;
    else i = cWindowWidth-str_width;
    glBegin(GL_QUADS);
      glColor3ub(0,0,0);
      glVertex2f((i-2)*PixelWidth, 51*PixelWidth -0.5);
      glVertex2f((i+2+str_width)*PixelWidth, 51*PixelWidth -0.5);
      glVertex2f((i+2+str_width)*PixelWidth, 66*PixelWidth -0.5);
      glVertex2f((i-2)*PixelWidth, 66*PixelWidth -0.5);
    glEnd();
    glColor3ub(255, 255, 255);
    WriteText(price_str, i*PixelWidth, (cGoodBarHeight+1)*PixelWidth -0.5);
  }//func
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.DrawGoodsBar\n";
  #endif
}//proc

void TGui::DrawColonyTitleBar()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.DrawColonyTitleBar\n";
  #endif
  if (cur_colony!=NULL)
  {
    std::string s;
    s = cur_colony->GetName() +".  "+TLanguage::GetSingleton().GetSeason(TData::GetSingleton().IsAutumn())
       +", "+IntToStr(TData::GetSingleton().GetYear())+". "+TLanguage::GetSingleton().GetOthers(osGold)+": ";
    TNation* temp_nat = TData::GetSingleton().GetNation(cur_colony->GetNation());
    if (temp_nat!=NULL)
    {
      if (temp_nat->IsEuropean()) s = s+IntToStr((static_cast<TEuropeanNation*>(temp_nat))->GetGold())+"°";
      else s = s+" -1°";
    }//if
    else s = s+" -1°";
    glColor3ubv(&cMenuTextColour[0]);
    WriteText(s, ((cWindowWidth-8*s.length()) / 2)*PixelWidth, 12.0+5.0*PixelWidth);
  }//if
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.DrawColonyTitleBar\n";
  #endif
}//proc

void TGui::DrawEuropeTitleBar()
{
  if (europe!=NULL)
  {
    std::string s;
    s = TLanguage::GetSingleton().GetPortName(europe->GetCount())+", "
       +TLanguage::GetSingleton().GetNationName(europe->GetCount())+". "
       +TLanguage::GetSingleton().GetSeason(TData::GetSingleton().IsAutumn())+" "+IntToStr(TData::GetSingleton().GetYear())
       +". "+TLanguage::GetSingleton().GetOthers(osTax)+": "+IntToStr(europe->GetTaxRate())
       +". "+TLanguage::GetSingleton().GetOthers(osGold)+": "+IntToStr(europe->GetGold())+"°";
    glColor3ubv(&cMenuTextColour[0]);
    WriteText(s, ((cWindowWidth-8*s.length()) / 2)*PixelWidth, 12.0+5.0*PixelWidth);
  }//if
}//proc

bool TGui::InMenu() const
{
  return (menu_cat!=mcNone);
}//func

bool TGui::InColony() const
{
  return (cur_colony!=NULL);
}//func

bool TGui::InEurope() const
{
  return (europe!=NULL);
}//func

bool TGui::InReport() const
{
  return (report!=rtNone);
}//func

bool TGui::InWoodenMode() const
{
  return Wooden_Mode;
}//func

TUnit* TGui::GetFocusedUnit() const
{
  return focused;
}//func

void TGui::GetSquareAtMouse(LongInt& sq_x, LongInt& sq_y) const
{
  sq_x = mouse.x / 32;
  if (mouse.y>16)
    sq_y = (mouse.y-16) / 32;
  else sq_y = -1;
  if ((sq_x>=0) and (sq_x<x_Fields) and (sq_y>=0) and (sq_y<y_Fields))
  {
    //all OK so far, add offset to get absolute values
    sq_x = sq_x+OffsetX;
    sq_y = sq_y+OffsetY;
  }
  else
  {
    //values out of range
    sq_x = -1;
    sq_y = -1;
  }//else
}//func

TGoodType TGui::GetGoodAtMouse(const LongInt m_x, const LongInt m_y) const
{
  if ((m_x==-1) or (m_y==-1))
  {
    if ((mouse.x<0) or (mouse.x>607) or (mouse.y<cWindowHeight-50) or (mouse.y>cWindowHeight-16))
      return gtCross;
    else return TGoodType(Ord(gtFood)+(mouse.x / 38));
  }
  else
  {
    if ((m_x<0) or (m_x>607) or (m_y<cWindowHeight-50) or (m_y>cWindowHeight-16))
      return gtCross;
    else return TGoodType(Ord(gtFood)+(m_x / 38));
  }//else
}//func

void TGui::EnqueueNewMessage(const std::string& msg_txt, const TStringArr& opts, const std::string& inCaption, const std::string& inText, TCallbackRec cbRec)
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.EnqueueNewMessage\n";
  #endif
  TQueueElem* temp;
  temp = new TQueueElem;
  temp->txt = msg_txt;
  //SetLength(temp^.options, length(opts));
  temp->options = TStringArr(opts.size(), "");
  LongInt i;
  for (i= 0; i<opts.size(); ++i)
    temp->options[i] = Trim(opts[i]).substr(0,59);
  //maximum caption is half the line long (i.e. 30 characters)
  temp->inputCaption = Trim(inCaption).substr(0, 30);
  temp->inputText = Trim(inText);
  temp->cbRec = cbRec;
  temp->next = NULL;
  if (msg_queue.first==NULL)
  {
    msg_queue.first = temp;
    msg_queue.last = temp;
  }//if
  else
  {
    msg_queue.last->next = temp;
    msg_queue.last = temp;
  }//else
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.EnqueueNewMessage\n";
  #endif
}//proc

void TGui::ShowMessageSimple(const std::string& msg_txt)
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.ShowMessageSimple\n";
  #endif
  if (msg.txt=="")
  {
    msg.txt = Trim(msg_txt);
    //SetLength(msg.options, 0);
    msg.options.clear();
    msg.inputCaption = "";
    msg.inputText = "";
    msg.cbRec = cEmptyCallback();
  }
  else
  {
    //enqueue new message
    TStringArr null_opts;
    //SetLength(null_opts, 0);
    null_opts.clear();
    EnqueueNewMessage(msg_txt, null_opts, "", "", cEmptyCallback());
  }//else
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.ShowMessageSimple\n";
  #endif
}//proc

void TGui::ShowMessageOptions(const std::string& msg_txt, const TStringArr& opts, TCallbackRec cbRec)
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.ShowMessageOptions\n";
  #endif
  if (msg.txt=="")
  {
    msg.txt = Trim(msg_txt)+cSpace60;
    //SetLength(msg.options, length(opts));
    msg.options = TStringArr(opts.size(), "");
    LongInt i;
    for (i= 0; i<opts.size(); ++i)
      msg.options[i] = Trim(opts[i]).substr(0,59);
    msg.inputCaption = "";
    msg.inputText = "";
    msg.selected_option = 0;
    msg.cbRec = cbRec;
  }
  else
  {
    //enqueue new message
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, opts, "", "", cbRec);
  }//else
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.ShowMessageOptions\n";
  #endif
}//proc

void TGui::ShowMessageInput(const std::string& msg_txt, const std::string& inCaption, const std::string& inDefault, TCallbackRec cbRec)
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.ShowMessageInput\n";
  #endif
  if (msg.txt=="")
  {
    msg.txt = Trim(msg_txt)+cSpace60;
    //SetLength(msg.options, 0);
    msg.options.clear();
    //input caption maximum is half the line (i.e. 30 characters)
    msg.inputCaption = Trim(inCaption).substr(0, 30);
    msg.inputText = Trim(inDefault);
    msg.selected_option = 0;
    msg.cbRec = cbRec;
  }
  else
  {
    //enqueue new message
    TStringArr null_opts;
    //SetLength(null_opts, 0);
    null_opts.clear();
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, null_opts, inCaption, inDefault, cbRec);
  }//else
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.ShowMessageInput\n";
  #endif
}//func

void TGui::GetNextMessage()
{
  #ifdef DEBUG_CODE
    std::cout << "Entered TGui.GetNextMessage\n";
  #endif
  //save last selection before anything else
  if ((msg.options.size()>1) or (msg.inputCaption!=""))
  {
    //set last selected option
    msg.cbRec.option = msg.selected_option;
    msg.cbRec.inputText = msg.inputText;
    //check whether we need callbacks
    if ((((msg.cbRec._type==CBT_LOAD_GAME) or (msg.cbRec._type==CBT_SAVE_GAME)) and (msg.selected_option==0))
       or ((msg.cbRec._type==CBT_LOAD_GAME) and (TData::GetSingleton().GetSaveInfo(msg.selected_option)=="("+TLanguage::GetSingleton().GetOthers(osEmpty)+")")))
    {
      //skip callbacks
    }
    else
    {
      //handle callbacks
      bool local_bool = HandleCallback(msg.cbRec);

      switch (msg.cbRec._type)
      {
        case CBT_LOAD_GAME:
             if (not local_bool)
             {
               ShowMessageSimple(TLanguage::GetSingleton().GetSaveLoad(slsLoadError));
               Wooden_Mode = true;
             }//if
             else
             {
               Wooden_Mode = false;
               ShowMessageSimple(TLanguage::GetSingleton().GetSaveLoad(slsLoadSuccess));
             }//else
             break;// CBT_LOAD_GAME
        case CBT_SAVE_GAME:
             if (local_bool) ShowMessageSimple(TLanguage::GetSingleton().GetSaveLoad(slsSaveSuccess));
             else ShowMessageSimple(TLanguage::GetSingleton().GetSaveLoad(slsSaveError));
             break;// CBT_SAVE_GAME
        case CBT_ABANDON_COLONY:
             if (local_bool) cur_colony = NULL;
             break;
      }//swi
    }//else
  }//if
  //now the main work
  if (msg_queue.first!=NULL)
  {
    msg.txt = msg_queue.first->txt;
    //SetLength(msg.options, length(msg_queue.first^.options));
    msg.options = TStringArr(msg_queue.first->options.size(), "");
    LongInt i;
    for (i=0; i<msg_queue.first->options.size(); ++i)
      msg.options[i] = msg_queue.first->options[i];
    msg.inputCaption = msg_queue.first->inputCaption;
    msg.inputText = msg_queue.first->inputText;
    msg.cbRec = msg_queue.first->cbRec;
    //move first pointer to new first element
    TQueueElem* temp = msg_queue.first;
    msg_queue.first = msg_queue.first->next;
    if (msg_queue.first==NULL) msg_queue.last = NULL;
    //shorten queue (and thus free former first element)
    delete temp;
    temp = NULL;
    msg.selected_option =0;
  }//if-then-branch
  else
  {
    //no new messages in queue; clear msg.
    msg.txt = "";
    //SetLength(msg.options, 0);
    msg.options.clear();
    msg.selected_option =0;
    msg.inputCaption = "";
    msg.inputText = "";
    msg.cbRec = cEmptyCallback();
  }//else branch
  #ifdef DEBUG_CODE
    std::cout << "Leaving TGui.GetNextMessage\n";
  #endif
}//proc

void TGui::HandleMenuSelection(const TMenuCategory categ, const LongInt selected)
{
  TStringArr str_arr;
  TCallbackRec temp_cb;
  TTask* tempTask;

  switch (categ)
  {
    case mcGame:
         switch (selected)
         {
           case 1: //save
                if (InWoodenMode())
                  ShowMessageSimple(TLanguage::GetSingleton().GetSaveLoad(slsNoGameLoaded));
                else
                {
                  temp_cb._type = CBT_SAVE_GAME;
                  temp_cb.SaveGame = new TSaveGameData;
                  temp_cb.SaveGame->AData = TData::GetSingleton();
                  str_arr = TData::GetSingleton().GetSaveSlots();
                  ShowMessageOptions(TLanguage::GetSingleton().GetSaveLoad(slsSaveChoose),
                                     ToStringArr(TLanguage::GetSingleton().GetOthers(osNothing), str_arr),
                                     temp_cb);
                }//else
                break;//save
           case 2: //load
                temp_cb._type = CBT_LOAD_GAME;
                temp_cb.LoadGame = new TLoadGameData;
                temp_cb.LoadGame->AData = TData::GetSingleton();
                str_arr = TData::GetSingleton().GetSaveSlots();
                ShowMessageOptions(TLanguage::GetSingleton().GetSaveLoad(slsLoadChoose),
                                   ToStringArr(TLanguage::GetSingleton().GetOthers(osNothing), str_arr),
                                   temp_cb);
                break;;//load
           case 3://quit?
                temp_cb._type = CBT_EXIT;
                temp_cb.cbExit = CBF_Exit;
                ShowMessageOptions("Vespucci beenden?", ToStringArr("Nein", "Ja"), temp_cb);
                break;//3 of mcGame
         }//swi
         break;//mcGame
    case mcView:
         switch (selected)
         {
           case 1:
                europe = static_cast<TEuropeanNation*> (TData::GetSingleton().GetNation(TData::GetSingleton().PlayerNation())); //europe
                break;
           case 2:
                if (focused!=NULL) CenterOn(focused->GetPosX(), focused->GetPosY()); //center view
                break;
         }//swi
         break;//mcView
    case mcOrders:
         switch (selected)
         {
           case 1: //fortify
                if (focused!=NULL)
                {
                  if (focused->GetState()==usFortified) focused->SetState(usNormal);
                  else focused->SetState(usFortified);
                }//if
                break;
           case 2: //goto
                if (focused!=NULL)
                {
                  if (focused->IsShip())
                  {
                    TColonyArr col_arr = TData::GetSingleton().GetColonyList(focused->GetNation());
                    //SetLength(str_arr, 1);
                    str_arr = TStringArr(1, "");
                    str_arr[0] = TLanguage::GetSingleton().GetOthers(osNoChanges);
                    LongInt i;
                    for (i = 0; i<col_arr.size(); ++i)
                      if (col_arr[i]->AdjacentWater(TMap::GetSingleton()))
                      {
                        str_arr.push_back(col_arr[i]->GetName());
                      }//if
                    temp_cb._type = CBT_GOTO_SHIP;
                    temp_cb.option = 0;
                    temp_cb.GotoShip = new TGotoShipData;
                    temp_cb.GotoShip->Ship = focused;
                    temp_cb.GotoShip->AData = TData::GetSingleton();
                    ShowMessageOptions("Choose a destination location:", str_arr, temp_cb);
                  } //if
                }//if
                break;// mcOrders,2, goto
           case 3: //clear forest
                if (focused!=NULL)
                {
                  if (not TMap::GetSingleton().tiles[focused->GetPosX()][focused->GetPosY()]->HasForest())
                    ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psIsCleared));
                  else if (focused->IsShip() or (focused->GetType()==utRegular or focused->GetType()==utDragoon
                           or focused->GetType()==utScout or focused->GetType()== utConvoy))
                    ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psWrongUnit));
                  else if (focused->GetToolAmount()<20) ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psNoTools));
                  else
                  {
                    //do the real work now :)
                    tempTask = new TClearTask(focused, focused->GetPosX(), focused->GetPosY());
                    focused->SetTask(tempTask);
                  }//else
                };//if
                break;//3 of orders (clear forest)
           case 4: //plough fields
                if (focused!=NULL)
                {
                  if (TMap::GetSingleton().tiles[focused->GetPosX()][focused->GetPosY()]->IsPloughed())
                    ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psIsPloughed));
                  else if (TMap::GetSingleton().tiles[focused->GetPosX()][focused->GetPosY()]->HasForest())
                    ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psNeedsClearing));
                  else if (focused->IsShip() or (focused->GetType()==utRegular or focused->GetType()==utDragoon
                          or focused->GetType()==utScout or focused->GetType()==utConvoy))
                    ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psWrongUnit));
                  else if (focused->GetToolAmount()<20) ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psNoTools));
                  else
                  {
                    //do the real work now :)
                    tempTask = new TPloughTask(focused, focused->GetPosX(), focused->GetPosY());
                    focused->SetTask(tempTask);
                  }//else
                }//if
                break;//4 of mcOrders (plough fields)
           case 5: //construct road
                if (focused!=NULL)
                {
                  if (TMap::GetSingleton().tiles[focused->GetPosX()][focused->GetPosY()]->HasRoad())
                    ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psHasRoad));
                  else if (focused->IsShip() or (focused->GetType()==utRegular or focused->GetType()== utDragoon
                           or focused->GetType()==utScout or focused->GetType()==utConvoy))
                    ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psWrongUnit));
                  else if (focused->GetToolAmount()<20) ShowMessageSimple(TLanguage::GetSingleton().GetPioneer(psNoTools));
                  else
                  {
                    //do the real work now :)
                    tempTask = new TRoadTask(focused, focused->GetPosX(), focused->GetPosY());
                    focused->SetTask(tempTask);
                  }//else
                }//if
                break;//5 of mcOrders (create road)
           case 6: //no orders
                if (focused!=NULL)
                {
                  focused->MovesLeft = 0;
                  TUnit* tempUnit = TData::GetSingleton().GetFirstLazyUnit(TData::GetSingleton().PlayerNation());
                  if (tempUnit!=NULL)
                  {
                    focused = tempUnit;
                    CenterOn(focused->GetPosX(), focused->GetPosY());
                  }//if
                  else
                  {
                    //no units left, start new round
                    TData::GetSingleton().AdvanceYear();
                    TData::GetSingleton().NewRound(TData::GetSingleton().PlayerNation());
                    focused = TData::GetSingleton().GetFirstLazyUnit(TData::GetSingleton().PlayerNation());
                  }//else
                }//if
                break; //6 of mcOrders
         };//swi
         break;//mcOrders
    case mcReports:
         switch (selected)
         {
           case 1: report = rtEconomy; break;
           case 2: report = rtColony; break;
           case 3: report = rtFleet; break;
         };//swi
         break;//mcReports
  }//swi
}//proc

GLfloat TGui::GetMenuStartX(const TMenuCategory categ) const
{
  //also see TGui.DrawMenuBar for further info on how these values are calculated
  if ((categ==mcNone) or (categ==mcGame)) return 0.0;
  else
  {
    std::string temp_str = "";
    LongInt i;
    for (i= Ord(mcGame); i<=Ord(Pred(categ)); ++i)
      temp_str = temp_str+TLanguage::GetSingleton().GetMenuLabel(TMenuCategory(i))+"  ";
    return temp_str.length()*8*PixelWidth;
  }//else
}//func

TMenuCategory TGui::GetMenuCategoryAtMouse() const
{
  if (mouse.y>16) return mcNone;
  else
  {
    std::string temp_str = "";
    TMenuCategory Result = mcGame;
    LongInt i;
    for (i = Ord(mcGame); i<=Ord(Pred(High(mcGame))); ++i)
    {
      temp_str = temp_str+TLanguage::GetSingleton().GetMenuLabel(TMenuCategory(i))+"  ";
      if (mouse.x>temp_str.length()*8) Result = TMenuCategory(i+1);
    }//for
    temp_str = temp_str+TLanguage::GetSingleton().GetMenuLabel(High(mcGame));
    if (mouse.x>temp_str.length()*8) Result = mcNone;
    return Result;
  }//else
}//func

void TGui::GetMenuSelectionAtMouse(TMenuCategory& cat, LongInt& sel_option) const
{
  if (mouse.y<16)
  {
    cat = GetMenuCategoryAtMouse();
    sel_option =0;
  }//if
  else
  {
    //get selected option
    sel_option = mouse.y / 16;
    if ((mouse.x>=GetMenuStartX(menu_cat)*FieldWidth) and
       (mouse.x<=GetMenuStartX(menu_cat)*FieldWidth+TLanguage::GetSingleton().GetMaxLen(menu_cat)*8+FieldWidth)
       and (sel_option<=TLanguage::GetSingleton().GetOptionCount(menu_cat)))
      cat = menu_cat;
    else
    {
      cat = mcNone;
      sel_option = -1;
    }//else
  }//else
}//proc

void TGui::GetColonyFieldAtMouse(ShortInt& x_shift, ShortInt& y_shift, const LongInt m_x, const LongInt m_y) const
{
  if ((m_x==-1) or (m_y==-1))
  {
    x_shift = (mouse.x / FieldWidth)-x_Fields-2;
    y_shift = (mouse.y -16) / FieldWidth -2;
  }//if
  else
  {
    x_shift = (m_x / FieldWidth)-x_Fields-2;
    y_shift = (m_y -16) / FieldWidth -2;
  }//else
  if ((x_shift<-1) or (x_shift>1) or (y_shift<-1) or (y_shift>1))
  {
    x_shift = -2;
    y_shift = -2;
  }//if
}//proc

ShortInt TGui::GetCargoBoxAtMouse(const LongInt m_x, const LongInt m_y) const
{
  if ((m_x==-1) or (m_y==-1))
  {
    if ((mouse.x<FieldWidth) or (mouse.x>=7*FieldWidth) or (mouse.y>cWindowHeight-cGoodBarHeight-16)
         or (mouse.y<cWindowHeight-cGoodBarHeight-16-FieldWidth))
      return -1;
    else return (mouse.x-FieldWidth) / FieldWidth;
  }//if
  else
  {
    if ((m_x<FieldWidth) or (m_x>=7*FieldWidth) or (m_y>cWindowHeight-cGoodBarHeight-16)
         or (m_y<cWindowHeight-cGoodBarHeight-16-FieldWidth))
      return -1;
    else return (m_x-FieldWidth) / FieldWidth;
  }//else
}//func

bool TGui::IsMouseInExpectedSoon(const LongInt m_x, const LongInt m_y) const
{
  if ((m_x==-1) or (m_y==-1))
  {
    return ((mouse.x>=FieldWidth) and (mouse.x<=(2+cShipsInExpectedSoon)*FieldWidth)
             and (mouse.y>=16+FieldWidth) and (mouse.y<=16+3*FieldWidth));
  }//if
  else return ((m_x>=FieldWidth) and (m_x<=(2+cShipsInExpectedSoon)*FieldWidth)
             and (m_y>=16+FieldWidth) and (m_y<=16+3*FieldWidth));
}//func

ShortInt TGui::GetExpectedSoonAtMouse(const LongInt m_x, const LongInt m_y) const
{
  ShortInt Result;
  if ((m_x==-1) or (m_y==-1))
    Result = (mouse.x- ((3*FieldWidth)/ 2)) / FieldWidth;
  else Result = (m_x- ((3*FieldWidth)/ 2)) / FieldWidth;
  if (not(((Result>=0) and (Result<=cShipsInExpectedSoon)) and IsMouseInExpectedSoon(m_x, m_y))) Result = -1;
  return Result;
}//func

bool TGui::IsMouseInToNewWorld(const LongInt m_x, const LongInt m_y) const
{
  if ((m_x==-1) or (m_y==-1))
  {
    return ((mouse.x>=(3+cShipsInExpectedSoon)*FieldWidth) and (mouse.x<=(4+cShipsInExpectedSoon+cShipsInToNewWorld)*FieldWidth)
             and (mouse.y>=16+FieldWidth) and (mouse.y<=16+3*FieldWidth));
  }//if
  else return ((m_x>=(3+cShipsInExpectedSoon)*FieldWidth) and (m_x<=(4+cShipsInExpectedSoon+cShipsInToNewWorld)*FieldWidth)
             and (m_y>=16+FieldWidth) and (m_y<=16+3*FieldWidth));
}//func

ShortInt TGui::GetToNewWorldAtMouse(const LongInt m_x, const LongInt m_y) const
{
  ShortInt Result;
  if ((m_x==-1) or (m_y==-1))
    Result = (mouse.x- ((3+cShipsInExpectedSoon)*FieldWidth+ FieldWidth / 2)) / FieldWidth;
  else Result = (m_x- ((3+cShipsInExpectedSoon)*FieldWidth+ FieldWidth / 2)) / FieldWidth;
  if (not(((Result>=0) and (Result<=cShipsInToNewWorld)) and IsMouseInToNewWorld(m_x, m_y))) Result = -1;
  return Result;
}//func

LongInt TGui::GetShipAtMouse(const LongInt m_x, const LongInt m_y) const
{
 // glVertex2f(1.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+1.0 +(i div 6));
 if ((m_x<=FieldWidth) or (m_x>=7*FieldWidth) or (m_y>=cWindowHeight-cGoodBarHeight-FieldWidth-17)) return -1;
 else
 {
   LongInt Result = (m_x-FieldWidth) / FieldWidth;
   Result = Result+6*((cWindowHeight-(cGoodBarHeight+FieldWidth+17)-m_y) / FieldWidth);
   return Result;
 }//else
}//func

LongInt TGui::GetUnitAtMouse(const LongInt m_x, const LongInt m_y) const
{
  //glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+(i mod 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i div 6));
  if ((m_x<=cWindowWidth-6*FieldWidth) or (m_x>=cWindowWidth) or
      (m_y>=cWindowHeight-cGoodBarHeight-1-FieldWidth) or (m_y<=16))
    return -1;
  else
  {
    LongInt Result = (m_x-(cWindowWidth-6*FieldWidth)) / FieldWidth;
    Result = Result+ 6*(((cWindowHeight-cGoodBarHeight-1-FieldWidth)-m_y) / FieldWidth);
    return Result;
  }//else
}//func

LongInt TGui::GetButtonAtMouse(const LongInt m_x, const LongInt m_y) const
{
  if ((m_x>=9*FieldWidth) and (m_x<=12*FieldWidth) and (m_y<=cWindowHeight-cGoodBarHeight-(FieldWidth / 2)))
  {
      if (m_y>=cWindowHeight-cGoodBarHeight-FieldWidth-(FieldWidth / 2)) return 1;
      else if ((m_y>=cWindowHeight-cGoodBarHeight-3*FieldWidth) and (m_y<=cWindowHeight-cGoodBarHeight-2*FieldWidth)) return 2;
      else return -1;
  }//if
  else return -1;
}//func

LongInt TGui::GetColonyUnitAtMouse(const LongInt m_x, const LongInt m_y) const
{
  /// 14.0 + (i mod 6),(cGoodBarHeight+1)*PixelWidth+(i div 6)
  if ((m_x>=cWindowWidth) or (m_x<=14*FieldWidth) or (m_y>=cWindowHeight-(cGoodBarHeight+17))) return -1;
  LongInt Result;
  Result = (m_x-14*FieldWidth) / FieldWidth;//x-part
  Result = Result +6* (((cWindowHeight-(cGoodBarHeight+17))-m_y) / FieldWidth);
  if (Result>=24) return -1;
  return Result;
}//func

//returns: -1 of none, 0 if upper, 1 if lower
LongInt TGui::GetSwitcherButtonAtMouse(const LongInt m_x, const LongInt m_y) const
{
  if ((m_x>=cWindowWidth-2*FieldWidth) or (m_x<=cWindowWidth-5*FieldWidth)
     or (m_y<=16+5*FieldWidth) or (m_y>=16+6*FieldWidth)) return -1;
  else return ((m_y-16-5*FieldWidth) / 16);
}//func

TBuildingType TGui::GetBuildingAtMouse(const LongInt mx, const LongInt my) const
{
/*Colony layout:
 (length in Field units (=currently 32px on screen)

  /--0.5--+----3.0------+--0.5--+----3.0-------+--0.5--+-------3.0-----+--0.5--+----3.0---+--0.5--\
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |      free     | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btCarpenter |       | btBlackSmith |       | btChurch      |       | btPress  |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btFurTrader |       | btDistiller  |       | btWeaver      |       | btArmory |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btSchool    |       | btStable     |       | btTobacconist |       |  btDock  |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |   image     |       |   image      |       |   image       |       |   image  |       |
 2.0 free |    of       | free  |    of        | free  |    of         | free  |    of    | free  |
  |       | btWarehouse |       |  btTownHall  |       | nothing(free) |       |  btFort  |       |
  +-------+-------------+-------+--------------+-------+---------------+-------+----------+-------+
  |       |             |       |              |       |               |       |          |       |
 0.5 free | free        | free  |  free        | free  |     free      | free  |  free    | free  |
  |       |             |       |              |       |               |       |          |       |
  \-------+-------------+-------+--------------+-------+---------------+-------+----------+-------/
*/

  if ((mx>=16) and (mx<=16+3*FieldWidth)) /*first column*/
  {
    if ((my>=32) and (my<=32+2*FieldWidth)) return btCarpenter; /*first row*/
    if ((my>=3*FieldWidth+16) and (my<=5*FieldWidth+16)) return btFurTrader/*second row*/;
    if ((my>=6*FieldWidth) and (my<=8*FieldWidth)) return btSchool/*third row*/;
    if ((my>=8*FieldWidth+16) and (my<=10*FieldWidth+16)) return btWarehouse/*fourth row*/;
    return btNone;
  }//if
  if ((mx>=4*FieldWidth) and (mx<=7*FieldWidth)) /*second column*/
  {
    if ((my>=32) and (my<=32+2*FieldWidth)) return btBlacksmith/*first row*/;
    if ((my>=3*FieldWidth+16) and (my<=5*FieldWidth+16)) return btDistiller/*second row*/;
    if ((my>=6*FieldWidth) and (my<=8*FieldWidth)) return btStable/*third row*/;
    if ((my>=8*FieldWidth+16) and (my<=10*FieldWidth+16)) return btTownHall/*fourth row*/;
    return btNone;
  }//if
  if ((mx>=7*FieldWidth+16) and (mx<=10*FieldWidth+16)) /*third column*/
  {
    if ((my>=32) and (my<=32+2*FieldWidth)) return btChurch/*first row*/;
    if ((my>=3*FieldWidth+16) and (my<=5*FieldWidth+16)) return btWeaver/*second row*/;
    if ((my>=6*FieldWidth) and (my<=8*FieldWidth)) return btTobacconist/*third row*/;
    //free /*fourth row*/
    return btNone;
  }//if
  if ((mx>=11*FieldWidth) and (mx<=14*FieldWidth)) /*fourth column*/
  {
    if ((my>=32) and (my<=32+2*FieldWidth)) return btPress/*first row*/;
    if ((my>=3*FieldWidth+16) and (my<=5*FieldWidth+16)) return btArmory/*second row*/;
    if ((my>=6*FieldWidth) and (my<=8*FieldWidth)) return btDock/*third row*/;
    if ((my>=8*FieldWidth+16) and (my<=10*FieldWidth+16)) return btFort/*fourth row*/;
    return btNone;
  }//case
  return btNone;
}//func

bool TGui::IsMouseInConstructionBar(const LongInt mx, const LongInt my) const
{
  return ((mx>=3*FieldWidth) and (mx<=12*FieldWidth) and (y_Fields*FieldWidth+16-my>= 1.25*FieldWidth)
           and (y_Fields*FieldWidth+16-my<= 1.75*FieldWidth));
}//func
