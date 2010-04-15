#ifndef GUI_H
#define GUI_H

#include "Map.h"
#include "Data.h"
#include <string>
#include <GL/gl.h>
#include <GL/glut.h>
#include "Terrain.h"
#include "Language.h"
#include "Colony.h"
#include "Tribe.h"
#include "Nations.h"
#include "Goods.h"
#include "Units.h"
#include "BitmapReader.h"
#include "Callbacks.h"
#include "Helper.h"
#include "ErrorTexture.h"
#include "PascalTypes.h"

enum TRiverType {rtOne, rtTwo_Bend, rtTwo_Straight, rtThree};

int Ord(const TRiverType rt_val);
TRiverType Low(const TRiverType rt_val);
TRiverType High(const TRiverType rt_val);

const LongInt x_Fields = 15;
const LongInt y_Fields = 12;
const LongInt FieldWidth = 32; //width of field in pixels
const LongInt BarWidth = 160; //bar width in px
const float PixelWidth = 0.03125; // =1/32, i.e. 1px

const LongInt cGoodBarHeight = 52;

const LongInt MiniMap_x_Fields = 56;
const LongInt MiniMap_y_Fields = 39;

const LongInt cWindowWidth = 32*x_Fields+BarWidth;
const LongInt cWindowHeight = 32*y_Fields+16+16;

const LongInt cShipsInExpectedSoon = 5;
const LongInt cShipsInToNewWorld = 5;

const LongInt cFleetReportUnitsPerPage = y_Fields-2;

const float BorderWidth = 0.0625; // =1/16, i.e. 2px
const Byte BorderColour[3] ={0,0,0}; //black

const Byte cMapColour[ttMountains+1][3] ={
       {224, 224, 224}, //ttArctic
       {32, 44, 136}, //ttSea
       {80, 80, 255}, //ttOpenSea (orig.: same as Sea)
       {132, 112, 80}, //ttPlains
       {28, 108, 16}, //ttGrassland
       {136, 140, 60}, //ttPrairie
       {116, 164, 76}, //ttSavannah
       {52, 72, 156}, //ttMarsh
       {116, 164, 76}, //ttSwamp (same as ttSavannah)
       {204, 176, 140}, //ttDesert
       {184, 184, 64}, //ttTundra
       {184, 184, 64}, //ttBoreal (same as ttTundra)
       {52, 72, 156}, //ttWetland (same as ttMarsh)
       {201, 175, 138}, //ttScrubForest
       {136, 140, 60}, //ttBroadleaf (same as ttPrairie)
       {130, 112, 79}, //ttMixedForest (nearly same as ttPlains)
       {28, 107, 18}, //ttConiferForest
       {117, 164, 77}, //ttRainForest (nearly same as ttSwamp)
       {116, 164, 76}, //ttTropicalForest (same as ttSavannah)
       {184, 160, 124}, //ttHills
       {216, 204, 172}  //ttMountains
      };

const std::string cTerrainTexNames[ttMountains+1] ={
       "arctic.bmp", //ttArctic
       "sea.bmp", //ttSea
       "opensea.bmp", //ttOpenSea
       "plains.bmp", //ttPlains
       "green.bmp", //ttGrassland
       "prairie.bmp", //ttPrairie
       "savannah.bmp", //ttSavannah
       "marsh.bmp", //ttMarsh
       "swamp.bmp", //ttSwamp
       "desert.bmp", //ttDesert
       "tundra.bmp", //ttTundra
       "boreal.bmp", //ttBoreal
       "wetland.bmp", //ttWetland
       "scrub.bmp", //ttScrubForest
       "broadleaf.bmp", //ttBroadleaf
       "mixed.bmp", //ttMixedForest
       "conifer.bmp", //ttConiferForest
       "rain.bmp", //ttRainForest
       "tropical.bmp", //ttTropicalForest
       "hills.bmp", //ttHills
       "mountain.bmp"  //ttMountains
     };

const std::string cRiverTexNames[rtThree+1] ={
       "river_n.bmp", //one (spring)
       "river_ne.bmp", //2, bend
       "river_ns.bmp", //2, straight
       "river_nes.bmp" //3
     };

const std::string cGoodTexNames[gtCross+1] ={
       "food.bmp", //gtFood
       "sugar.bmp", //gtSugar
       "tobacco.bmp", //gtTobacco
       "cotton.bmp", //gtCotton
       "fur.bmp", //gtFur
       "wood.bmp", //gtWood
       "ore.bmp", //gtOre
       "silver.bmp", //gtSilver
       "horses.bmp", //gtHorses
       "rum.bmp", //gtRum
       "cigar.bmp", //gtCigar
       "cloth.bmp", //gtCloth
       "coat.bmp", //gtCoat
       "tradegoods.bmp", //gtTradegoods
       "tool.bmp", //gtTool
       "musket.bmp", //gtMusket
       "hammer.bmp", //gtHammer
       "bell.bmp", //gtLibertyBell
       "cross.bmp"//gtCross
    };

const std::string cUnitTexNames[utBraveOnHorse+1] ={
       "criminal.bmp", //utCriminal
       "servant.bmp", //utServant
       "colonist.bmp", //utColonist
       "farmer.bmp", //utFarmer
       "fisher.bmp", //utFisher
       "furhunter.bmp", //utFurHunter
       "silverminer.bmp", //utSilverMiner
       "woodcutter.bmp", //utWoodcutter
       "oreminer.bmp", //utOreMiner
       "sugarplanter.bmp", //utSugarplanter
       "cottonplanter.bmp", //utCottonplanter
       "tobaccoplanter.bmp", //utTobaccoplanter
       "preacher.bmp", //utPreacher
       "statesman.bmp", //utStatesman
       "carpenter.bmp", //utCarpenter
       "distiller.bmp", //utDistiller
       "weaver.bmp", //utWeaver
       "tobacconist.bmp", //utTobacconist
       "furtrader.bmp", //utFurTrader
       "smith.bmp", //utSmith
       "weaponsmith.bmp", //utWeaponSmith
       "scout.bmp", //utScout
       "pioneer.bmp", //utPioneer
       "missionary.bmp", //utMissionary
       "regular.bmp", //utRegular
       "dragoon.bmp", //utDragoon
       "artillery.bmp", //utArtillery
       "convoy.bmp", //utConvoy
       "caravel.bmp", //utCaravel
       "tradingship.bmp", //utTradingShip
       "galleon.bmp", //utGalleon
       "privateer.bmp", //utPrivateer
       "frigate.bmp", //utFrigate
       "manowar.bmp", //utMan_o_War
       "brave.bmp", //utBrave
       "brave_horse.bmp"//utBraveOnHorse
    };

const std::string cStateTexNames[usCreateRoad+1] ={
       "normal.bmp", //usNormal
       "fortified.bmp", //usFortified
       "waitship.bmp", //usWaiting forShip
       "goto.bmp", //usGoTo
       "plough.bmp", //usPloughing
       "road.bmp" //usCreateRoad
    };

const std::string cColonyTexNames[1] ={
       "colony.bmp" //normal colony
    };

const std::string cBuildingTexNames[btBlacksmith+1][4] ={
      {"", "", "", ""}, //btNone, 0
      {"", "stockade.bmp", "fort.bmp", "fortress.bmp"}, //btFort, 3
      {"", "docks.bmp", "trydock.bmp", "shipyard.bmp"}, //btDock, 3
      {"", "storage.bmp", "storage2.bmp", ""}, //btWareHouse, 2
      {"", "stable.bmp", "", ""}, //btStable, 1
      {"", "customhouse.bmp", "", ""}, //btCustomHouse, 1
      {"", "", "", ""}, //btPress, 2
      {"", "school.bmp", "college.bmp", "university.bmp"}, //btSchool, 3
      {"", "", "", ""}, //btArmory, 3
      {"", "townhall.bmp", "", ""}, //btTownhall, 1
      {"", "", "", ""}, //btWeaver, 3
      {"", "tobacconist.bmp", "", ""}, //btTobacconist, 3
      {"", "", "", ""}, //btDistiller, 3
      {"", "", "", ""}, //btFurTrader, 3
      {"", "carpenter.bmp", "sawmill.bmp", ""}, //btCarpenter, 2
      {"", "church.bmp", "cathedral.bmp", ""}, //btChurch, 2
      {"", "blacksmith.bmp", "", ""}  //btBlackSmith, 3
    };

std::string cTribeTexNames(const LongInt NationCount);

const std::string cWindowCaption = "Vespucci v0.01.r107";

const Byte cMenuTextColour[3] = {20, 108, 16};
const Byte cMenuHighColour[3] = {255, 20, 20};
const GLfloat cWoodenColour[3] = {0.83, 0.66, 0.39};
const GLfloat cBlueBorderColour[3] = {0.25, 0.35, 0.64};

//Keys
const LongInt KEY_BACKSPACE = 8;
const LongInt KEY_RETURN = 13; //sure?
const LongInt KEY_ESCAPE = 27;
const LongInt KEY_SPACE = 32; //sure?
const LongInt KEY_DELETE = 127;

//maybe starts with 97, maybe with 49, try it
const LongInt KEY_NUMPAD1 = 49;
const LongInt KEY_NUMPAD2 = 50;
const LongInt KEY_NUMPAD3 = 51;
const LongInt KEY_NUMPAD4 = 52;
//const LongInt KEY_NUMPAD5 = 53;
const LongInt KEY_NUMPAD6 = 54;
const LongInt KEY_NUMPAD7 = 55;
const LongInt KEY_NUMPAD8 = 56;
const LongInt KEY_NUMPAD9 = 57;

//{PlaceDollarSignHereDEFINE DEBUG_CODE 1}


struct TQueueElem
{
  std::string txt;
  TStringArr options;
  std::string inputCaption, inputText;
  TCallbackRec cbRec;
  TQueueElem* next;
};//rec

class TGui
{
  private:
    struct {
      LongInt x,y;
      bool down;
      LongInt down_x, down_y;
    } mouse;//rec
    TMenuCategory menu_cat;
    LongInt selected_menu_option;
    LongInt OffsetX, OffsetY;
    LongInt MiniMapOffset_Y;
    bool Wooden_Mode;
    TColony* cur_colony;
    bool ColonyBuildingPage;
    TEuropeanNation* europe;
    TUnit* focused;
    //TData* dat;
    TReportType report;
    //text messages
    struct
    {
      std::string txt;
      TStringArr options;
      LongInt selected_option;
      std::string inputCaption, inputText;
      TCallbackRec cbRec;
    } msg;//rec
    struct
    {
      TQueueElem* first;
      TQueueElem* last;
    } msg_queue;//rec
    //terrain texture "names" (as in OpenGL names)
    GLuint m_TerrainTexNames[ttMountains+1];
    //river texture "names" (as in OpenGL names)
    GLuint m_RiverTexNames[rtThree+1];
    //good texture "names" (as in OpenGL names)
    GLuint m_GoodTexNames[gtCross+1];
    //unit texture "names" (as in OpenGL names)
    GLuint m_UnitTexNames[utBraveOnHorse+1];
    //state icon "names" (as in OpenGL names)
    GLuint m_StateTexNames[usCreateRoad+1];
    //colony texture "names" ( " " " " )
    GLuint m_ColonyTexNames[1];
    //colony building texture "names" ( " " " " )
    GLuint m_BuildingTexNames[btBlacksmith+1][4];
    //Tribe texture "names" ( " " " " )
    GLuint m_TribeTexNames[cMaxIndian+1];
    //Error Texture (yellow sign with black "!" on it)
    GLuint m_ErrorTexName;

    void DrawMenuBar();
    void DrawGoodsBar();
    void DrawColonyTitleBar();
    void DrawEuropeTitleBar();
    void DrawMessage();
    void DrawColonyView();
    void GetBuildingPosition(const TBuildingType bt, float& x, float& y) const;
    void DrawColonyBuildings();
    void DrawEuropeanView();
    void DrawGoodDraggedFromBar();
    void DrawEuropeButtons();
    void DrawShipsInPort(const TUnitArr& predefShips);
    void DrawPeopleInEurope(const TUnitArr& People);
    void DrawExpectedSoon(const TUnitArr& ExpSoon);
    void DrawShipsToNewWorld(const TUnitArr& ToNewWorld);
    void DrawReport();
    void DrawMenu();
    void DrawUnitIcon(const TUnit* the_Unit, const GLfloat left, const GLfloat bottom,
                  const bool UseErrorIfTexNotPresent=false, const bool ShowState=false);
    void DrawStateIcon(const TUnitState state, const GLfloat left, const GLfloat bottom);
    void GetSquareAtMouse(LongInt& sq_x, LongInt& sq_y) const;
    TGoodType GetGoodAtMouse(const LongInt m_x=-1, const LongInt m_y=-1) const;
    TMenuCategory GetMenuCategoryAtMouse() const;
    void GetMenuSelectionAtMouse(TMenuCategory& cat, LongInt& sel_option) const;
    void GetColonyFieldAtMouse(ShortInt& x_shift, ShortInt& y_shift, const LongInt m_x=-1, const LongInt m_y=-1) const;
    ShortInt GetCargoBoxAtMouse(const LongInt m_x=-1, const LongInt m_y=-1) const;
    bool IsMouseInExpectedSoon(const LongInt m_x=-1, const LongInt m_y=-1) const;
    bool IsMouseInToNewWorld(const LongInt m_x=-1, const LongInt m_y=-1) const;
    ShortInt GetExpectedSoonAtMouse(const LongInt m_x=-1, const LongInt m_y=-1) const;
    ShortInt GetToNewWorldAtMouse(const LongInt m_x=-1, const LongInt m_y=-1) const;
    LongInt GetShipAtMouse(const LongInt m_x, const LongInt m_y) const;
    LongInt GetUnitAtMouse(const LongInt m_x, const LongInt m_y) const;
    LongInt GetButtonAtMouse(const LongInt m_x, const LongInt m_y) const;
    LongInt GetColonyUnitAtMouse(const LongInt m_x, const LongInt m_y) const;
    LongInt GetSwitcherButtonAtMouse(const LongInt m_x, const LongInt m_y) const;
    TBuildingType GetBuildingAtMouse(const LongInt mx, const LongInt my) const;
    bool IsMouseInConstructionBar(const LongInt mx, const LongInt my) const;
    void EnqueueNewMessage(const std::string& msg_txt, const TStringArr& opts, const std::string& inCaption, const std::string& inText, TCallbackRec cbRec);
    void GetNextMessage();//de-facto dequeue
    void HandleMenuSelection(const TMenuCategory categ, const LongInt selected);
    GLfloat GetMenuStartX(const TMenuCategory categ) const;
    void GLUTCallbacksToNil();
  public:
    TGui();
    virtual ~TGui();
    void KeyFunc(Byte Key, LongInt x, LongInt y, bool Special=false);
    void MouseFunc(const LongInt button, const LongInt state, const LongInt x, const LongInt y);
    void MouseMoveFunc(const LongInt x, const LongInt y);
    void Resize(const LongInt Width, const LongInt Height);
    void Start();
    void Draw();
    void CenterOn(const LongInt x, const LongInt y);
    void WriteText(const std::string& msg_txt, const float x, const float y);
    void WriteHelvetica12(const std::string& msg_txt, const float x, const float y);
    void WriteTimesRoman24(const std::string& msg_txt, const float x, const float y);

    LongInt TextWidthTimesRoman24(const std::string& msg_txt);

    void ShowMessageSimple(const std::string& msg_txt);
    void ShowMessageOptions(const std::string& msg_txt, const TStringArr& opts, TCallbackRec cbRec);
    void ShowMessageInput(const std::string& msg_txt, const std::string& inCaption, const std::string& inDefault, TCallbackRec cbRec);

    bool InMenu() const;
    bool InColony() const;
    bool InEurope() const;
    bool InReport() const;
    bool InWoodenMode() const;
    TUnit* GetFocusedUnit() const;
};//class TGui

#endif
