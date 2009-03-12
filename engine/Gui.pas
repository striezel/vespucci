unit Gui;

interface

uses
  Map, Data, GL, GLUT, Terrain, Language, Colony, Tribe, Nation, Goods,
  Units, SysUtils, BitmapReader, Callbacks, Helper, ErrorTexture;

const
  x_Fields = 15;
  y_Fields = 12;
  FieldWidth = 32; //width of field in pixels
  BarWidth = 160; //bar width in px
  PixelWidth = 0.03125; // =1/32, i.e. 1px

  cGoodBarHeight = 52;

  Minimap_x_Fields = 56;
  Minimap_y_Fields = 39;

  cWindowWidth = 32*x_Fields+BarWidth;
  cWindowHeight = 32*y_Fields+16+16;

  cShipsInExpectedSoon = 5;
  cShipsInToNewWorld = 5;

  cFleetReportUnitsPerPage = y_Fields-2;

  BorderWidth: Single = 0.0625; // =1/16, i.e. 2px
  BorderColour: array[0..2] of Byte = (0,0,0); //black

  cMapColour: array [TTerrainType] of array [0..2] of Byte=(
       (224, 224, 224), //ttArctic
       (32, 44, 136), //ttSea
       (80, 80, 255), //ttOpenSea (orig.: same as Sea)
       (132, 112, 80), //ttPlains
       (28, 108, 16), //ttGrassland
       (136, 140, 60), //ttPrairie
       (116, 164, 76), //ttSavannah
       (52, 72, 156), //ttMarsh
       (116, 164, 76), //ttSwamp (same as ttSavannah)
       (204, 176, 140), //ttDesert
       (184, 184, 64), //ttTundra
       (184, 184, 64), //ttBoreal (same as ttTundra)
       (52, 72, 156), //ttWetland (same as ttMarsh)
       (201, 175, 138), //ttScrubForest
       (136, 140, 60), //ttBroadleaf (same as ttPrairie)
       (130, 112, 79), //ttMixedForest (nearly same as ttPlains)
       (28, 107, 18), //ttConiferForest
       (117, 164, 77), //ttRainForest (nearly same as ttSwamp)
       (116, 164, 76), //ttTropicalForest (same as ttSavannah)
       (184, 160, 124), //ttHills
       (216, 204, 172)  //ttMountains
      );

  cTerrainTexNames: array [TTerrainType] of string =(
       'arctic.bmp', //ttArctic
       'sea.bmp', //ttSea
       'opensea.bmp', //ttOpenSea
       'plains.bmp', //ttPlains
       'green.bmp', //ttGrassland
       'prairie.bmp', //ttPrairie
       'savannah.bmp', //ttSavannah
       'marsh.bmp', //ttMarsh
       'swamp.bmp', //ttSwamp
       'desert.bmp', //ttDesert
       'tundra.bmp', //ttTundra
       'boreal.bmp', //ttBoreal
       'wetland.bmp', //ttWetland
       'scrub.bmp', //ttScrubForest
       'broadleaf.bmp', //ttBroadleaf
       'mixed.bmp', //ttMixedForest
       'conifer.bmp', //ttConiferForest
       'rain.bmp', //ttRainForest
       'tropical.bmp', //ttTropicalForest
       'hills.bmp', //ttHills
       'mountain.bmp'  //ttMountains
     );

  cGoodTexNames: array [TGoodType] of string =(
       'food.bmp', //gtFood
       'sugar.bmp', //gtSugar
       'tobacco.bmp', //gtTobacco
       'cotton.bmp', //gtCotton
       'fur.bmp', //gtFur
       'wood.bmp', //gtWood
       'ore.bmp', //gtOre
       'silver.bmp', //gtSilver
       'horses.bmp', //gtHorses
       'rum.bmp', //gtRum
       'cigar.bmp', //gtCigar
       'cloth.bmp', //gtCloth
       'coat.bmp', //gtCoat
       'tradegoods.bmp', //gtTradegoods
       'tool.bmp', //gtTool
       'musket.bmp', //gtMusket
       'hammer.bmp', //gtHammer
       'bell.bmp', //gtLibertyBell
       'cross.bmp'//gtCross
    );

  cUnitTexNames: array [TUnitType] of string =(
       'criminal.bmp', //utCriminal
       'servant.bmp', //utServant
       'colonist.bmp', //utColonist
       'farmer.bmp', //utFarmer
       'fisher.bmp', //utFisher
       'furhunter.bmp', //utFurHunter
       'silverminer.bmp', //utSilverMiner
       'woodcutter.bmp', //utWoodcutter
       'oreminer.bmp', //utOreMiner
       'sugarplanter.bmp', //utSugarplanter
       'cottonplanter.bmp', //utCottonplanter
       'tobaccoplanter.bmp', //utTobaccoplanter
       'preacher.bmp', //utPreacher
       'statesman.bmp', //utStatesman
       'carpenter.bmp', //utCarpenter
       'distiller.bmp', //utDistiller
       'weaver.bmp', //utWeaver
       'tobacconist.bmp', //utTobacconist
       'furtrader.bmp', //utFurTrader
       'smith.bmp', //utSmith
       'weaponsmith.bmp', //utWeaponSmith
       'scout.bmp', //utScout
       'pioneer.bmp', //utPioneer
       'missionary.bmp', //utMissionary
       'regular.bmp', //utRegular
       'dragoon.bmp', //utDragoon
       'artillery.bmp', //utArtillery
       'convoy.bmp', //utConvoy
       'caravel.bmp', //utCaravel
       'tradingship.bmp', //utTradingShip
       'galleon.bmp', //utGalleon
       'privateer.bmp', //utPrivateer
       'frigate.bmp', //utFrigate
       'manowar.bmp', //utMan_o_War
       'brave.bmp', //utBrave
       'brave_horse.bmp'//utBraveOnHorse
    );

  cStateTexNames: array[TUnitState] of string =(
       'normal.bmp', //usNormal
       'fortified.bmp', //usFortified
       'waitship.bmp', //usWaiting forShip
       'goto.bmp', //usGoTo
       'plough.bmp', //usPloughing
       'road.bmp' //usCreateRoad
    );

  cColonyTexNames: array [0..0] of string =(
       'colony.bmp' //normal colony
    );

  cBuildingTexNames: array [TBuildingType] of array [1..3] of string =(
      ('stockade.bmp', 'fort.bmp', 'fortress.bmp'), //btFort, 3
      ('docks.bmp', 'trydock.bmp', 'shipyard.bmp'), //btDock, 3
      ('storage.bmp', 'storage2.bmp', ''), //btWareHouse, 2
      ('stable.bmp', '', ''), //btStable, 1
      ('customhouse.bmp', '', ''), //btCustomHouse, 1
      ('', '', ''), //btPress
      ('school.bmp', 'college.bmp', 'university.bmp'), //btSchool, 3
      ('', '', ''), //btArmory
      ('townhall.bmp', '', ''), //btTownhall, 1
      ('', '', ''), //btWeaver
      ('tobacconist.bmp', '', ''), //btTobacconist
      ('', '', ''), //btDistiller
      ('', '', ''), //btFurTrader
      ('carpenter.bmp', 'sawmill.bmp', ''), //btCarpenter, 2
      ('church.bmp', 'cathedral.bmp', ''), //btChurch, 2
      ('blacksmith.bmp', '', '')  //btBlackSmith, 3
    );

  cTribeTexNames: array [cMinIndian..cMaxIndian] of string =(
       'tents.bmp', //cNationArawak
       'aztec.bmp', //cNationAztec
       'inca.bmp', //cNationInca
       'tents.bmp', //cNationTupi
       'tents.bmp', //cNationCherokee
       'tents.bmp', //cNationIroquois
       'tents.bmp', //cNationSioux
       'tents.bmp' //cNationApache
    );

  cWindowCaption = 'Vespucci v0.01.r095';

  cMenuTextColour : array [0..2] of Byte = (20, 108, 16);
  cMenuHighColour : array [0..2] of Byte = (255, 20, 20);
  cWoodenColour: array [0..2] of GLfloat = (0.83, 0.66, 0.39);
  cBlueBorderColour: array[0..2] of GLfloat = (0.25, 0.35, 0.64);

  //Keys
  KEY_BACKSPACE = 8;
  KEY_RETURN = 13; //sure?
  KEY_ESCAPE = 27;
  KEY_SPACE = 32; //sure?
  KEY_DELETE = 127;

  //maybe starts with 97, maybe with 49, try it
  KEY_NUMPAD1 = 49;
  KEY_NUMPAD2 = 50;
  KEY_NUMPAD3 = 51;
  KEY_NUMPAD4 = 52;
  //KEY_NUMPAD5 = 53;
  KEY_NUMPAD6 = 54;
  KEY_NUMPAD7 = 55;
  KEY_NUMPAD8 = 56;
  KEY_NUMPAD9 = 57;

{PlaceDollarSignHereDEFINE DEBUG_CODE 1}

type
  PQueueElem = ^TQueueElem;
  TQueueElem = record
                 txt: AnsiString;
                 options:TShortStrArr;
                 inputCaption, inputText: ShortString;
                 cbRec: TCallbackRec;
                 next: PQueueElem;
               end;//rec
  TGui = class
    private
      mouse: record
               x,y: LongInt;
               down: Boolean;
               down_x, down_y: LongInt;
             end;//rec
      menu_cat: TMenuCategory;
      selected_menu_option: Integer;
      OffsetX, OffsetY: Integer;
      MiniMapOffset_Y: Integer;
      Wooden_Mode: Boolean;
      cur_colony: TColony;
      ColonyBuildingPage: Boolean;
      europe: TEuropeanNation;
      focused: TUnit;
      dat: TData;
      report: TReportType;
      //text messages
      msg: record
             txt: AnsiString;
             options: TShortStrArr;
             selected_option: Integer;
             inputCaption, inputText: ShortString;
             cbRec: TCallbackRec;
           end;//rec
      msg_queue: record
                   first: PQueueElem;
                   last: PQueueElem;
                 end;//rec
      //terrain texture "names" (as in OpenGL names)
      m_TerrainTexNames: array [TTerrainType] of GLuint;
      //good texture "names" (as in OpenGL names)
      m_GoodTexNames: array [TGoodType] of GLuint;
      //unit texture "names" (as in OpenGL names)
      m_UnitTexNames: array [TUnitType] of GLuint;
      //state icon "names" (as in OpenGL names)
      m_StateTexNames: array[TUnitState] of GLuint;
      //colony texture "names" ( " " " " )
      m_ColonyTexNames: array [0..0] of GLuint;
      //colony building texture "names" ( " " " " )
      m_BuildingTexNames: array [TBuildingType] of array [1..3] of GLuint;
      //Tribe texture "names" ( " " " " )
      m_TribeTexNames: array [cMinIndian..cMaxIndian] of GLuint;
      //Error Texture (yellow sign with black "!" on it)
      m_ErrorTexName: GLuint;

      procedure InitGLUT;
      procedure DrawMenuBar;
      procedure DrawGoodsBar;
      procedure DrawColonyTitleBar;
      procedure DrawEuropeTitleBar;
      procedure DrawMessage;
      procedure DrawColonyView;
      procedure GetBuildingPosition(const bt: TBuildingType; var x,y: Single);
      procedure DrawColonyBuildings;
      procedure DrawEuropeanView;
      procedure DrawGoodDraggedFromBar;
      procedure DrawEuropeButtons;
      procedure DrawShipsInPort(const predefShips: TUnitArr);
      procedure DrawPeopleInEurope(const People: TUnitArr);
      procedure DrawExpectedSoon(const ExpSoon: TUnitArr);
      procedure DrawShipsToNewWorld(const ToNewWorld: TUnitArr);
      procedure DrawReport;
      procedure DrawMenu;
      procedure DrawUnitIcon(const the_Unit: TUnit; const left, bottom: GLfloat;
                  const UseErrorIfTexNotPresent: Boolean = False; const ShowState: Boolean = False);
      procedure DrawStateIcon(const state: TUnitState; const left, bottom: GLfloat);
      procedure GetSquareAtMouse(var sq_x, sq_y: Integer);
      function  GetGoodAtMouse(const m_x: LongInt=-1; const m_y: LongInt=-1): TGoodType;
      function  GetMenuCategoryAtMouse: TMenuCategory;
      procedure GetMenuSelectionAtMouse(var cat: TMenuCategory; var sel_option: Integer);
      procedure GetColonyFieldAtMouse(var x_shift, y_shift: ShortInt; const m_x: LongInt=-1; m_y: LongInt=-1);
      function  GetCargoBoxAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;
      function  IsMouseInExpectedSoon(const m_x: LongInt=-1; m_y: LongInt=-1): Boolean;
      function  IsMouseInToNewWorld(const m_x: LongInt=-1; m_y: LongInt=-1): Boolean;
      function  GetExpectedSoonAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;
      function  GetToNewWorldAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;
      function  GetShipAtMouse(const m_x, m_y: LongInt): Integer;
      function  GetUnitAtMouse(const m_x, m_y: LongInt): Integer;
      function  GetButtonAtMouse(const m_x, m_y: LongInt): Integer;
      function  GetColonyUnitAtMouse(const m_x, m_y: LongInt): Integer;
      function  GetSwitcherButtonAtMouse(const m_x, m_y: LongInt): LongInt;
      procedure EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const inCaption, inText: ShortString; cbRec: TCallbackRec);
      procedure GetNextMessage;//de-facto dequeue
      procedure HandleMenuSelection(const categ: TMenuCategory; const selected: Integer);
      function  GetMenuStartX(const categ: TMenuCategory): GLfloat;
    public
      constructor Create;
      destructor Destroy;
      procedure KeyFunc(Key: Byte; x, y: LongInt; Special: Boolean = False);
      procedure MouseFunc(const button, state, x,y: LongInt);
      procedure MouseMoveFunc(const x,y: LongInt);
      procedure Resize(Width, Height: LongInt);
      procedure Start;
      procedure Draw;
      procedure CenterOn(const x, y: Integer);
      procedure WriteText(const msg_txt: string; const x, y: Single);
      procedure WriteHelvetica12(const msg_txt: string; const x, y: Single);
      procedure WriteTimesRoman24(const msg_txt: string; const x, y: Single);

      function TextWidthTimesRoman24(const msg_txt: string): LongInt;

      procedure ShowMessageSimple(const msg_txt: AnsiString);
      procedure ShowMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; cbRec: TCallbackRec);
      procedure ShowMessageInput(const msg_txt: AnsiString; const inCaption: ShortString; const inDefault: ShortString; cbRec: TCallbackRec);

      function InMenu: Boolean;
      function InColony: Boolean;
      function InEurope: Boolean;
      function InReport: Boolean;
      function InWoodenMode: Boolean;
      function GetFocusedUnit: TUnit;
  end;//class TGui

implementation

// **** TGui functions ****

constructor TGui.Create;
var i, j: Integer;
    //general texture format
    tempTex: TArraySq32RGB;
    AlphaTex: TArraySq32RGBA;
    //building texture format
    BuildTex: TArray128x64RGB;
    AlphaBuildTex: TArray128x64RGBA;
    err_str: string;
    Ship, passenger: TUnit;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.Create');
  {$ENDIF}
  inherited Create;
  mouse.x:= 0;
  mouse.y:= 0;
  mouse.down:= False;
  mouse.down_x:= -1;
  mouse.down_y:= -1;
  OffsetX:= 0; OffsetY:= 0;
  Wooden_Mode:= True;
  MiniMapOffset_Y:= 0;
  dat:= TData.Create(cNationEngland);
  Ship:= dat.NewUnit(utCaravel, dat.PlayerNation, 36, 13);
  WriteLn('First caravel created.');
  passenger:= dat.NewUnit(utColonist, dat.PlayerNation, 36, 13);
  passenger.GiveTools(100);
  Ship.LoadUnit(passenger);
  passenger:= dat.NewUnit(utRegular, dat.PlayerNation, 36, 13);
  Ship.LoadUnit(passenger);

  menu_cat:= mcNone;
  selected_menu_option:= 1;
  report:= rtNone;
  cur_colony:= nil;
  ColonyBuildingPage:= False;
  europe:= nil;
  focused:= nil;
  //message
  msg.txt:= '';
  SetLength(msg.options, 0);
  msg.selected_option:=0;
  msg.inputCaption:= '';
  msg.inputText:= '';
  msg_queue.first:= nil;
  msg_queue.last:= nil;

  //set texture names to "empty" and then load them
  glEnable(GL_TEXTURE_2D);
  //terrain textures
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
  begin
    m_TerrainTexNames[TTerrainType(i)]:= 0;
    if ReadBitmapToArr32RGB(dat.GetPathBase+terrain_img_path+cTerrainTexNames[TTerrainType(i)], tempTex, err_str) then
    begin
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      glGenTextures(1, @m_TerrainTexNames[TTerrainType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[TTerrainType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, @tempTex[0].r);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;//if
  end;//for
  //good textures
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
  begin
    m_GoodTexNames[TGoodType(i)]:= 0;
    if ReadBitmapToArr32RGB(dat.GetPathBase+good_img_path+cGoodTexNames[TGoodType(i)], tempTex, err_str) then
    begin
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, @m_GoodTexNames[TGoodType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[TGoodType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;//if
  end;//for
  //unit textures
  for i:= Ord(Low(TUnitType)) to Ord(High(TUnitType)) do
  begin
    m_UnitTexNames[TUnitType(i)]:= 0;
    if ReadBitmapToArr32RGB(dat.GetPathBase+unit_img_path+cUnitTexNames[TUnitType(i)], tempTex, err_str) then
    begin
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, @m_UnitTexNames[TUnitType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[TUnitType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;//if
  end;//for
  //unit state textures
  for i:= Ord(Low(TUnitState)) to Ord(High(TUnitState)) do
  begin
    m_StateTexNames[TUnitState(i)]:= 0;
    if ReadBitmapToArr32RGB(dat.GetPathBase+state_img_path+cStateTexNames[TUnitState(i)], tempTex, err_str) then
    begin
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, @m_StateTexNames[TUnitState(i)]);
      glBindTexture(GL_TEXTURE_2D, m_StateTexNames[TUnitState(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;//if
  end;//for

  //colony textures
  m_ColonyTexNames[0]:= 0;
  if ReadBitmapToArr32RGB(dat.GetPathBase+colony_img_path+cColonyTexNames[0], tempTex, err_str) then
  begin
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, @m_ColonyTexNames[0]);
    glBindTexture(GL_TEXTURE_2D, m_ColonyTexNames[0]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;//if

  //building textures
  for i:= Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
  begin
    for j:= 1 to 3 do
    begin
      m_BuildingTexNames[TBuildingType(i), j]:= 0;
      if (cBuildingTexNames[TBuildingType(i), j]<>'') then
      begin
        if ReadBitmapToArr128x64RGB(dat.GetPathBase+building_img_path+cBuildingTexNames[TBuildingType(i), j], BuildTex, err_str) then
        begin
          //change order of color components from blue, green, red (as in file) to
          //  red, green, blue (as needed for GL)
          SwapRGB_To_BGR(BuildTex);
          GetAlphaByColor(BuildTex, AlphaBuildTex);
          glGenTextures(1, @m_BuildingTexNames[TBuildingType(i), j]);
          glBindTexture(GL_TEXTURE_2D, m_BuildingTexNames[TBuildingType(i), j]);
          glTexImage2D(GL_TEXTURE_2D, 0, 4, 128, 64, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaBuildTex[0].r);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        end//if
        else WriteLn('Error while reading bitmap "',dat.GetPathBase+building_img_path+cBuildingTexNames[TBuildingType(i), j],
                     '": ', err_str);
      end;//if
    end;//for
  end;//for


  //tribe textures
  for i:= cMinIndian to cMaxIndian do
    m_TribeTexNames[i]:= 0;
  if ReadBitmapToArr32RGB(dat.GetPathBase+tribe_img_path+cTribeTexNames[cNationCherokee], tempTex, err_str) then
  begin
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, @m_TribeTexNames[cNationCherokee]);
    glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[cNationCherokee]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    for i:= cMinIndian to cMaxIndian do
      m_TribeTexNames[i]:= m_TribeTexNames[cNationCherokee];
  end;//if
  if ReadBitmapToArr32RGB(dat.GetPathBase+tribe_img_path+cTribeTexNames[cNationAztec], tempTex, err_str) then
  begin
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, @m_TribeTexNames[cNationAztec]);
    glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[cNationAztec]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;
  if ReadBitmapToArr32RGB(dat.GetPathBase+tribe_img_path+cTribeTexNames[cNationInca], tempTex, err_str) then
  begin
    //change order of color components from blue, green, red (as in file) to
    //  red, green, blue (as needed for GL)
    SwapRGB_To_BGR(tempTex);
    GetAlphaByColor(tempTex, AlphaTex);
    glGenTextures(1, @m_TribeTexNames[cNationInca]);
    glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[cNationInca]);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;

  //error texture
  m_ErrorTexName:= 0;
  GetAlphaByColor(cErrorTex, AlphaTex);
  glGenTextures(1, @m_ErrorTexName);
  glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
  glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  //welcome message (German), originally for test reasons only
  ShowMessageSimple(
          'Willkommen bei Vespucci!                                    '
         +cSpace60
         +'Hinweise zur Steuerung:                                     '
         +'  Pfeiltasten bewegen eine Einheit/die Karte in die angege- '
         +'  bene Richtung. Zusaetzlich koennen auch die Ziffern 1,3,7 '
         +'  und 9 auf dem Nummernblock benutzt werden, um eine Einheit'
         +'  diagonal zu bewegen.                                      '
         +'  Leertaste beendet die aktuelle Runde, mit ESC wird das    '
         +'  Spiel beendet. Die Leertaste oder Enter kann auch genutzt '
         +'  werden, um diese Meldung verschwinden zu lassen.          '
         +cSpace60
         +'  Viele Sachen sind noch nicht implementiert, Vespucci be-  '
         +'  findet sich gerade am Anfang der Entwicklungsphase.');
  Wooden_Mode:= False;
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.Create');
  {$ENDIF}
end;//constructor

destructor TGui.Destroy;
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.Destroy');
  {$ENDIF}
  dat.Destroy;
  //free textures
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
    if m_TerrainTexNames[TTerrainType(i)]<> 0 then glDeleteTextures(1, @m_TerrainTexNames[TTerrainType(i)]);
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    if m_GoodTexNames[TGoodType(i)]<> 0 then glDeleteTextures(1, @m_GoodTexNames[TGoodType(i)]);
  for i:= Ord(Low(TUnitType)) to Ord(High(TUnitType)) do
    if m_UnitTexNames[TUnitType(i)]<> 0 then glDeleteTextures(1, @m_UnitTexNames[TUnitType(i)]);
  inherited Destroy;
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.Destroy');
  {$ENDIF}
end;//destructor

procedure TGui.KeyFunc(Key: Byte; x, y: LongInt; Special: Boolean = False);
var tempUnit: TUnit;
    temp_cb: TCallbackRec;
    temp_x, temp_y: Byte;
    temp_Map: TMap;
    direc: TDirection;
    temp_col: TColony;
    tempTask: TTask;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.KeyFunc');
  {$ENDIF}
  //react on message
  if msg.txt<>'' then
  begin
    if msg.inputCaption<>'' then
    begin
      //process input
      case Key of
        KEY_BACKSPACE, KEY_DELETE: msg.inputText:= copy(msg.inputText, 1, length(msg.inputText)-1);
        KEY_RETURN, KEY_ESCAPE: begin
                                  GetNextMessage;
                                  glutPostRedisplay;
                                end;
        else begin
          if ((Key>=KEY_SPACE) and not Special) then msg.inputText:= msg.inputText+Chr(Key);
        end;//case-else
      end;//case
      Exit; //better be safe than sorry ;)
    end;//if input message
    case Key of
      KEY_RETURN, KEY_ESCAPE, KEY_SPACE: begin
                                           GetNextMessage;
                                           glutPostRedisplay;
                                         end;//case
    end;//case
    //we even got options here
    if length(msg.options)>0 then
    begin
      case Key of
        GLUT_KEY_UP, KEY_NUMPAD8: begin
                                    msg.selected_option:= msg.selected_option -1;
                                    if msg.selected_option<0 then msg.selected_option:= High(msg.options);
                                  end;//case UP
        GLUT_KEY_DOWN, KEY_NUMPAD2: begin
                                    msg.selected_option:= msg.selected_option +1;
                                    if msg.selected_option>High(msg.options) then msg.selected_option:= 0;
                                  end;//case DOWN
      end;//case
    end;//if options

    Exit;//to prevent other things, keys can do to your units. We have
         // a message window, so display it, until space is hit.
  end;//if

  //keys if menu is active
  if InMenu then
  begin
    case Key of
      GLUT_KEY_UP, KEY_NUMPAD8: begin
                                  selected_menu_option:= selected_menu_option-1;
                                  if selected_menu_option<1 then selected_menu_option:= dat.GetLang.GetOptionCount(menu_cat);
                                end;//case UP, 8
      GLUT_KEY_DOWN, KEY_NUMPAD2: begin
                                  selected_menu_option:= selected_menu_option+1;
                                  if selected_menu_option>dat.GetLang.GetOptionCount(menu_cat) then selected_menu_option:= 1;
                                end;//case DOWN, 2
      GLUT_KEY_LEFT, KEY_NUMPAD4: begin
                                    if menu_cat<>mcGame then menu_cat:= Pred(menu_cat) else menu_cat:= mcTrade;
                                  end;//left
      GLUT_KEY_RIGHT, KEY_NUMPAD6: begin
                                     if menu_cat<>mcTrade then menu_cat:= Succ(menu_cat) else menu_cat:= mcGame;
                                   end;//right
      KEY_ESCAPE: menu_cat:= mcNone;
      KEY_RETURN, KEY_SPACE: begin
                               HandleMenuSelection(menu_cat, selected_menu_option);
                               menu_cat:= mcNone;
                               selected_menu_option:=1;
                             end;//Enter, Leertaste
    end;//case
    Exit; //better here ;)
  end;//if InMenu

  //"general" keys
  if Key=KEY_ESCAPE then
  begin
    if InColony then cur_colony:= nil
    else if InEurope then europe:= nil
    else if InReport then report:= rtNone
    else begin
      temp_cb._type:= CBT_EXIT;
      temp_cb.cbExit:= @CBF_Exit;
      ShowMessageOptions('Vespucci beenden?', ToShortStrArr('Nein', 'Ja'), temp_cb);
    end;//else
  end;//if KEY_ESCAPE

  if InReport then
  begin
    case KEY of
      KEY_RETURN, KEY_SPACE: report:= rtNone;
    end;//case
    Exit;
  end;//if InReport

  if InColony then
  begin
    if not Special and (UpCase(char(Key))='T') then ColonyBuildingPage:= not ColonyBuildingPage;
  end;//if InColony


  if Wooden_Mode or InEurope or InColony or InReport then Exit; //rest is only for america view

  if not Special then
  begin
    case UpCase(char(Key)) of
      'B': //build colony
           if focused<>nil then
           begin
             temp_Map:= dat.GetMap;
             if (focused.IsShip or temp_Map.tiles[focused.GetPosX, focused.GetPosY].IsWater) then
               ShowMessageSimple(dat.GetLang.GetBuildColony(2))
             else begin
               if temp_Map.tiles[focused.GetPosX, focused.GetPosY].GetType=ttMountains then
                 ShowMessageSimple(dat.GetLang.GetBuildColony(4))
               else begin
                 if dat.FreeForSettlement(focused.GetPosX, focused.GetPosY) then
                 begin
                   temp_cb.inputText:= '';
                   temp_cb._type:= CBT_BUILD_COLONY;
                   temp_cb.BuildColony.x:= focused.GetPosX;
                   temp_cb.BuildColony.y:= focused.GetPosY;
                   temp_cb.BuildColony.num_nation:= dat.PlayerNation;
                   temp_cb.BuildColony.founder:= focused;
                   temp_cb.BuildColony.AData:= dat;
                   ShowMessageInput(dat.GetLang.GetBuildColony(0), dat.GetLang.GetBuildColony(1),
                       dat.GetLang.GetColonyNames(focused.GetNation,
                       length(dat.GetColonyList(focused.GetNation))), temp_cb);
                   focused:= nil;
                 end
                 else
                   ShowMessageSimple(dat.GetLang.GetBuildColony(3));
               end;//else
             end;//if
           end;//if
           //end of 'B'
      'F': //fortify
           if focused<>nil then
           begin
             if focused.GetState=usFortified then focused.SetState(usNormal)
             else focused.SetState(usFortified);
           end;//if, fortify
      'S': ;//sentry
      'C': begin
             if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY)
             else CenterOn(25, 35);//just for testing, yet
           end;
    end;//case
  end;//if not Special

  if (focused=nil) then
  begin
    //no unit focused
    //move map
    case Key of
      GLUT_KEY_LEFT, KEY_NUMPAD4: if (OffsetX>0) then OffsetX:= OffsetX-1;{Move map left}
      GLUT_KEY_RIGHT, KEY_NUMPAD6: if (OffsetX<cMap_X-x_Fields) then OffsetX:= OffsetX+1;{Move map right}
      GLUT_KEY_DOWN, KEY_NUMPAD2: begin
                                    if (OffsetY<cMap_Y-y_Fields) then OffsetY:= OffsetY+1; {Move map down}
                                    if MiniMapOffset_Y<cMap_Y-Minimap_y_Fields then MiniMapOffset_Y:= MiniMapOffset_Y+1;
                                  end;//KEY_DOWN
      GLUT_KEY_UP, KEY_NUMPAD8: begin
                                  if (OffsetY>0) then OffsetY:= OffsetY-1; {Move map up}
                                  if MiniMapOffset_Y>0 then MiniMapOffset_Y:= MiniMapOffset_Y-1;
                                end;//KEY_UP
      KEY_SPACE: //try to get next unit
                 begin
                   focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                   if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY)
                   else begin
                     //no units left, start new round
                     dat.AdvanceYear;
                     dat.NewRound(dat.PlayerNation);
                     focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                   end;//else
                 end;//case SPACE
    end;//case
  end//if
  else begin
    //we have a focused unit, so move it
    direc:= dirNone;
    case Key of
      KEY_NUMPAD1: direc:=dirSW;
      GLUT_KEY_DOWN, KEY_NUMPAD2: direc:= dirS; {Move down}
      KEY_NUMPAD3: direc:= dirSE;
      GLUT_KEY_LEFT, KEY_NUMPAD4: direc:= dirW; {Move left}
      GLUT_KEY_RIGHT, KEY_NUMPAD6: direc:= dirE; {Move right}
      KEY_NUMPAD7: direc:= dirNW;
      GLUT_KEY_UP, KEY_NUMPAD8: direc:= dirN; {Move unit up}
      KEY_NUMPAD9: direc:= dirNE;
      KEY_SPACE: begin
                   focused.MovesLeft:= 0;
                   tempUnit:= dat.GetFirstLazyUnit(dat.PlayerNation);
                   if tempUnit<>nil then
                   begin
                     focused:= tempUnit;
                     CenterOn(focused.GetPosX, focused.GetPosY);
                   end//if
                   else begin
                     //no units left, start new round
                     dat.AdvanceYear;
                     dat.NewRound(dat.PlayerNation);
                     focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                   end;//else
                 end;//KEY_SPACE
    end;//case
    //should move now
    if direc<>dirNone then
    begin
      temp_x:= focused.GetPosX;
      temp_y:= focused.GetPosY;
      ApplyDir(temp_x, temp_y, direc);
      temp_Map:= dat.GetMap;
      if (focused.IsShip) then
      begin
        if (not temp_Map.tiles[temp_x, temp_y].IsWater) then
        begin
          temp_col:= dat.GetColonyInXY(temp_x, temp_y);
          if (temp_col<>nil) then
          begin
            if (temp_col.GetNation=focused.GetNation) and (focused.MovesLeft>0) then
            begin
              //ship enters colony
              focused.WarpToXY(temp_x, temp_y, temp_Map);
              focused.DropAllPassengers;
              focused.MovesLeft:= focused.MovesLeft-1;
            end
            else focused.Move(direc, temp_Map);
          end//if temp_col<>nil
          else if (focused.EmbarkedPassengers>0) then
          begin
            //check for landfall
            temp_cb._type:= CBT_LANDFALL;
            temp_cb.Landfall.cbLandfall:= @CBF_Landfall;
            temp_cb.Landfall.Ship:= focused;
            tempUnit:= focused.GetFirstEmbarkedPassenger;
            if tempUnit<>nil then temp_cb.Landfall.UType:= tempUnit.GetType
            else temp_cb.Landfall.UType:= utGalleon;
            temp_cb.Landfall.x:= temp_x;
            temp_cb.Landfall.y:= temp_y;
            temp_cb.Landfall.AMap:= temp_Map;
            ShowMessageOptions(dat.GetLang.GetLandfall(0), ToShortStrArr(dat.GetLang.GetLandfall(1), dat.GetLang.GetLandfall(2)), temp_cb);
          end //if passengers>0
          else focused.Move(direc, temp_Map);
        end//if not water
        else focused.Move(direc, temp_Map);
      end//if IsShip
      else focused.Move(direc, temp_Map);
    end;//if
    //check if unit moved out of sight, and center on it, if neccessary
    if focused<>nil then
    begin
      if ((focused.GetPosX<=OffsetX) or (focused.GetPosY<=OffsetY) or
          (focused.GetPosX>=OffsetX+x_Fields-1) or (focused.GetPosY>=OffsetY+y_Fields-1)) then
        CenterOn(focused.GetPosX, focused.GetPosY);
    end;//if
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.KeyFunc');
  {$ENDIF}
end;//proc

procedure TGui.MouseFunc(const button, state, x,y: LongInt);
var pos_x, pos_y: Integer;
    temp_cat: TMenuCategory;
    sx, sy, sx_d, sy_d: ShortInt;
    temp_cbr: TCallbackRec;
    tempUnit: TUnit;
    tempGood: TGoodType;
    tempUArr: TUnitArr;
    tempAmount: Byte;
    str_arr: TShortStrArr;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.MouseFunc');
  {$ENDIF}
  if msg.txt<>'' then Exit;

  //general stuff
  if ((button=GLUT_LEFT) and (state=GLUT_UP)) then mouse.down:= False
  else if ((button=GLUT_LEFT) and (state=GLUT_DOWN)) then
  begin
    mouse.down:= True;
    mouse.down_x:= x;
    mouse.down_y:= y;
  end;//if down

  //handling colony view's mouse events
  if (cur_colony<>nil) then
  begin
    //check for pressing the red "E" in colony view
    if ((button=GLUT_LEFT) and (state=GLUT_UP) and (x>608) and (y>cWindowHeight-50)) then
    begin
      cur_colony:= nil;
      glutPostRedisplay;
    end//if
    else if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
    begin
      //check for colony bar click (i.e. renaming colony)
      if ((mouse.y<=16) and (mouse.down_y<=16)) then
      begin
        temp_cbr._type:= CBT_RENAME_COLONY;
        temp_cbr.RenameColony.AColony:= cur_colony;
        with dat.GetLang do
          ShowMessageInput(GetColonyString(csRenameQuestion), GetColonyString(csRenameLabel), cur_colony.GetName, temp_cbr);
        Exit;
      end;//if

      GetColonyFieldAtMouse(sx, sy);
      GetColonyFieldAtMouse(sx_d, sy_d, mouse.down_x, mouse.down_y);
      //moving unit in field
      if ((sx<>-2) and (sx_d<>-2) and ((sx<>sx_d) or (sy<>sy_d)) and (cur_colony.GetUnitInField(sx_d, sy_d)<>nil)) then
      begin
        //check whether field is center or not
        if ((sx<>0) or (sy<>0)) then
        begin
          tempUnit:= cur_colony.GetUnitInField(sx_d, sy_d);
          tempGood:= cur_colony.GetUnitInFieldGood(sx_d, sy_d);
          cur_colony.SetUnitInField(sx_d, sy_d, nil);
          cur_colony.SetUnitInField(sx, sy, tempUnit, tempGood);
        end;//if
      end//if
      else if ((sx<>-2) and (cur_colony.GetUnitInField(sx, sy)<>nil)) then
      begin
        temp_cbr._type:= CBT_JOB_CHANGE;
        temp_cbr.JobChange.x_shift:= sx;
        temp_cbr.JobChange.y_shift:= sy;
        temp_cbr.JobChange.AColony:= cur_colony;
        ShowMessageOptions('Choose profession for unit '+dat.GetLang.GetUnitName(cur_colony.GetUnitInField(sx, sy).GetType)+':',
                           dat.GetJobList(sx, sy, cur_colony.GetUnitInField(sx, sy).GetType, cur_colony), temp_cbr);
      end;//else if

      //*** check for good transfer ***
      //from ship to port of colony
      sx:= GetCargoBoxAtMouse(mouse.down_x, mouse.down_y);
      tempGood:= GetGoodAtMouse;
      if ((sx<>-1) and (tempGood<>gtCross)) then
      begin
        tempUArr:= dat.GetAllShipsInXY(cur_colony.GetPosX, cur_colony.GetPosY);
        if length(tempUArr)>0 then //at least one ship present?
          if tempUArr[0].GetCargoAmountBySlot(sx)>0 then
          begin
            //we have a cargo transfer to the port
            tempAmount:= tempUArr[0].UnloadGood(tempUArr[0].GetCargoGoodBySlot(sx), tempUArr[0].GetCargoAmountBySlot(sx));
            cur_colony.AddToStore(tempUArr[0].GetCargoGoodBySlot(sx), tempAmount);
          end;//if
      end;//if
      //from port to ship
      sx:= GetCargoBoxAtMouse;
      tempGood:= GetGoodAtMouse(mouse.down_x, mouse.down_y);
      if ((sx<>-1) and (tempGood<>gtCross)) then
      begin
        tempUArr:= dat.GetAllShipsInXY(cur_colony.GetPosX, cur_colony.GetPosY);
        if length(tempUArr)>0 then
        begin
          if cur_colony.GetStore(tempGood)>=100 then tempAmount:= 100
          else tempAmount:= cur_colony.GetStore(tempGood);
          if tempUArr[0].LoadGood(tempGood, tempAmount) then
          begin
            cur_colony.RemoveFromStore(tempGood, tempAmount);
          end//if
          else ShowMessageSimple(dat.GetLang.GetTransfer(tsOutOfSpace));
        end;//if
      end;//if

      //check for moving unit from "outside" to fields
      GetColonyFieldAtMouse(sx, sy, mouse.x, mouse.y);
      pos_x:= GetColonyUnitAtMouse(mouse.down_x, mouse.down_y);
      if ((pos_x<>-1) and (sx<>-2) and ((sx<>0) or (sy<>0))) then
      begin
        tempUArr:= dat.GetAllUnitsInColony(cur_colony);
        if High(tempUArr)>=pos_x then
        begin
          cur_colony.SetUnitInField(sx, sy, tempUArr[pos_x]);
          Exit;
        end;//if
      end;//if

      //check for moving unit from fields to "outside"
      GetColonyFieldAtMouse(sx, sy, mouse.down_x, mouse.down_y);
      pos_x:= GetColonyUnitAtMouse(mouse.x, mouse.y);
      if ((pos_x<>-1) and (sx<>-2) and ((sx<>0) or (sy<>0))) then
      begin
        if cur_colony.GetUnitInField(sx, sy)<>nil then
        begin
          if cur_colony.GetInhabitants>1 then cur_colony.SetUnitInField(sx, sy, nil)
          else begin
            //ask whether they want to abandon colony
            temp_cbr._type:= CBT_ABANDON_COLONY;
            temp_cbr.AbandonColony.AColony:= cur_colony;
            temp_cbr.AbandonColony.AData:= dat;
            with dat.GetLang do
              ShowMessageOptions(GetColonyString(csAbandonQuestion), ToShortStrArr(GetColonyString(csAbandonNo), GetColonyString(csAbandonYes)), temp_cbr);
          end;//else
        end;//if unit<>nil
      end;//if

      //check for unit management "outside" of colony
      pos_x:= GetColonyUnitAtMouse(mouse.x, mouse.y);
      pos_y:= GetColonyUnitAtMouse(mouse.down_x, mouse.down_y);
      if ((pos_x<>-1) and (pos_x=pos_y)) then
      begin
        tempUArr:= dat.GetAllUnitsInColony(cur_colony);
        if High(tempUArr)>=pos_x then
        begin
          temp_cbr._type:= CBT_COLONY_UNIT;
          temp_cbr.ColonyUnit.AUnit:= tempUArr[pos_x];
          SetLength(str_arr, 3);
          if (tempUArr[pos_x].GetState in [usFortified, usWaitingforShip]) then
            str_arr[0]:= dat.GetLang.GetColonyUnit(cusCancelOrders)
          else str_arr[0]:= dat.GetLang.GetColonyUnit(cusOnBoard);
          if (tempUArr[pos_x].GetState=usFortified) then
            str_arr[1]:= dat.GetLang.GetColonyUnit(cusOnBoard)
          else str_arr[1]:= dat.GetLang.GetColonyUnit(cusFortify);
          str_arr[2]:= dat.GetLang.GetOthers(osNoChanges);
          ShowMessageOptions(dat.GetLang.GetColonyUnit(cusOptions)+' '+dat.GetLang.GetUnitName(tempUArr[pos_x].GetType)+':',
                             str_arr, temp_cbr);
        end;//if
      end;//if

      //check for switcher button (unit view/ building view)
      if ((GetSwitcherButtonAtMouse(mouse.x, mouse.y)<>-1) and
         (GetSwitcherButtonAtMouse(mouse.x, mouse.y)=GetSwitcherButtonAtMouse(mouse.down_x, mouse.down_y))) then
      begin
        //button was pressed
        ColonyBuildingPage:= GetSwitcherButtonAtMouse(mouse.x, mouse.y)=1;
      end;//if

    end;//else if button=LEFT and state=UP
    {$IFDEF DEBUG_CODE}
    WriteLn('Exiting TGui.MouseFunc');
    {$ENDIF}
    Exit;
  end;//if colony

  //handle European view's mouse events
  if (europe<>nil) then
  begin
    //check for pressing the red "E" in European view
    if ((button=GLUT_LEFT) and (state=GLUT_UP) and (x>608) and (y>cWindowHeight-50)) then
    begin
      europe:= nil;
      glutPostRedisplay;
    end//if
    //check for good transfer to port or from port to ship
    else if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
    begin
      WriteLn('This is European left button GLUT_UP...');
      //from ship to port
      sx:= GetCargoBoxAtMouse(mouse.down_x, mouse.down_y);
      tempGood:= GetGoodAtMouse;
      WriteLn('Cargo to port: sx: ', sx, '; Good: ', dat.GetLang.GetGoodName(tempGood));
      if ((sx<>-1) and (tempGood<>gtCross)) then
      begin
        WriteLn('Trying to clear cargo...');
        tempUArr:= dat.GetAllShipsInEurope(europe.GetCount);
        if length(tempUArr)>0 then //at least one ship present?
          if tempUArr[0].GetCargoAmountBySlot(sx)>0 then
          begin
            //we have a cargo transfer to the port
            if europe.IsBoycotted(tempUArr[0].GetCargoGoodBySlot(sx)) then
              ShowMessageSimple(dat.GetLang.GetTransfer(tsBoycotted))
            else begin
              //start the transfer, finally
              tempGood:= tempUArr[0].GetCargoGoodBySlot(sx);
              tempAmount:= tempUArr[0].UnloadGood(tempUArr[0].GetCargoGoodBySlot(sx), tempUArr[0].GetCargoAmountBySlot(sx));
              //print message about earnings
              ShowMessageSimple(
              StretchTo60(dat.GetLang.GetGoodName(tempGood)+': '+IntToStr(tempAmount)+'x'
                 +IntToStr(europe.GetPrice(tempGood, True))+' '+dat.GetLang.GetOthers(osGold),
                IntToStr(europe.GetPrice(tempGood, True)*tempAmount)+' '+dat.GetLang.GetOthers(osGold))
              +StretchTo60('-'+IntToStr(europe.GetTaxRate)+'% '+dat.GetLang.GetOthers(osTax),
                 IntToStr((europe.GetPrice(tempGood, True)*tempAmount*europe.GetTaxRate) div 100)+' '+dat.GetLang.GetOthers(osGold))
              +StretchTo60(dat.GetLang.GetOthers(osEarnings)+':', IntToStr(europe.GetPrice(tempGood, True)*tempAmount-((europe.GetPrice(tempGood, True)*tempAmount*europe.GetTaxRate) div 100))+' '+dat.GetLang.GetOthers(osGold)));
              //actually sell it
              europe.SellGood(tempUArr[0].GetCargoGoodBySlot(sx), tempAmount);
            end;//else
          end;//if
      end;//if
      //from port to ship
      sx:= GetCargoBoxAtMouse;
      tempGood:= GetGoodAtMouse(mouse.down_x, mouse.down_y);
      WriteLn('Cargo to ship: sx: ', sx, '; Good: ', dat.GetLang.GetGoodName(tempGood));
      if ((sx<>-1) and (tempGood<>gtCross)) then
      begin
        WriteLn('Trying to load cargo...');
        tempUArr:= dat.GetAllShipsInEurope(europe.GetCount);
        if length(tempUArr)>0 then //at least one ship present?
        begin
          //cargo transfer from port to ship
          if europe.IsBoycotted(tempGood) then
            ShowMessageSimple(dat.GetLang.GetTransfer(tsBoycotted))
          else if europe.GetPrice(tempGood, False)*100>europe.GetGold then
            ShowMessageSimple(dat.GetLang.GetTransfer(tsOutOfGold))
          else begin
            //start the transfer
            if tempUArr[0].LoadGood(tempGood, 100) then
            begin
              europe.BuyGood(tempGood, 100);
              //should show message about costs to player
              ShowMessageSimple(
              StretchTo60(dat.GetLang.GetGoodName(tempGood)+': 100x', IntToStr(europe.GetPrice(tempGood, false))+' '+dat.GetLang.GetOthers(osGold))
              + StretchTo60(dat.GetLang.GetOthers(osCost)+':', IntToStr(europe.GetPrice(tempGood, false)*100)+' '+dat.GetLang.GetOthers(osGold))
              );
            end
            else ShowMessageSimple(dat.GetLang.GetTransfer(tsOutOfSpace));
          end;//else
        end;//if
      end;//if


      //check for moving ship from "to new world box" to "expected soon box"
      pos_x:= GetToNewWorldAtMouse(mouse.down_x, mouse.down_y);
      WriteLn('New World at mouse: pos_x: ', pos_x);
      if (pos_x<>-1) and IsMouseInExpectedSoon then
      begin
        WriteLn('Trying to send back to europe...');
        tempUArr:= dat.GetAllShipsGoingToNewWorld(europe.GetCount);
        if pos_x<=High(tempUArr) then tempUArr[pos_x].CallBackToEurope;
        Exit;
      end;//if

      //check for moving ship from "expected soon box" to "to new world box"
      pos_x:= GetExpectedSoonAtMouse(mouse.down_x, mouse.down_y);
      WriteLn('Expected Soon at mouse: pos_x: ', pos_x);
      if (pos_x<>-1) and IsMouseInToNewWorld then
      begin
        WriteLn('Trying to send back to new world...');
        tempUArr:= dat.GetAllShipsGoingToEurope(europe.GetCount);
        if pos_x<=High(tempUArr) then tempUArr[pos_x].CallBackToNewWorld;
        Exit;
      end;//if

      //check for moving ship from port to "new world box"
      pos_x:= GetShipAtMouse(mouse.down_x, mouse.down_y);
      WriteLn('Ship at mouse: pos_x: ', pos_x);
      if (pos_x<>-1) and IsMouseInToNewWorld then
      begin
        WriteLn('Trying to send a ship to new world...');
        tempUArr:= dat.GetAllShipsInEurope(europe.GetCount);
        if pos_x<=High(tempUArr) then
        begin
          //load all possible units we can load, before we go off to New World
          tempUnit:= tempUArr[pos_x];
          tempUArr:= dat.GetAllNonShipsInEurope(europe.GetCount);
          for pos_y:= 0 to High(tempUArr) do
            if ((tempUArr[pos_y].GetState=usWaitingForShip) and (tempUnit.FreeCapacity>0)) then
              tempUnit.LoadUnit(tempUArr[pos_y]);
          tempUnit.SendToNewWorld;
          Exit;
        end;//if
      end;//if

      //check for clicked unit in port
      pos_x:= GetUnitAtMouse(mouse.x, mouse.y);
      pos_y:= GetUnitAtMouse(mouse.down_x, mouse.down_y);
      if ((pos_x<>-1) and (pos_x=pos_y)) then
      begin
        tempUArr:= dat.GetAllNonShipsInEurope(europe.GetCount);
        if pos_x<=High(tempUArr) then
        begin
          SetLength(str_arr, 5);
          with dat.GetLang do
          begin
            if tempUArr[pos_x].GetState=usWaitingForShip then str_arr[0]:= GetEuroPort(epsNotOnShip)
            else str_arr[0]:= GetEuroPort(epsGoOnShip);
            if tempUArr[pos_x].HasMuskets then
              str_arr[1]:= GetEuroPort(epsDisarm)+' ('+GetOthers(osSaving)+' '+IntToStr(europe.GetPrice(gtMusket, True)*50)+' '+GetOthers(osGold)+')'
            else
              str_arr[1]:= GetEuroPort(epsArm)+' ('+GetOthers(osCost)+' '+IntToStr(europe.GetPrice(gtMusket, False)*50)+' '+GetOthers(osGold)+')';
            if tempUArr[pos_x].HasHorses then
              str_arr[2]:= GetEuroPort(epsNoHorses)+' ('+GetOthers(osSaving)+' '+IntToStr(europe.GetPrice(gtHorses, True)*50)+' '+GetOthers(osGold)+')'
            else str_arr[2]:= GetEuroPort(epsGiveHorses)+' ('+GetOthers(osCost)
                   +' '+IntToStr(europe.GetPrice(gtHorses, False)*50)+' '+GetOthers(osGold)+')';
            if tempUArr[pos_x].GetToolAmount>0 then str_arr[3]:= GetEuroPort(epsNoTools)
                 +' ('+GetOthers(osSaving)+' '+IntToStr(europe.GetPrice(gtTool, True)*tempUArr[pos_x].GetToolAmount)+' '+GetOthers(osGold)+')'
            else str_arr[3]:= GetEuroPort(epsGiveTools)+' ('+GetOthers(osCost)
                   +' '+IntToStr(europe.GetPrice(gtTool, False)*(100-tempUArr[pos_x].GetToolAmount))+' '+GetOthers(osGold)+')';
            str_arr[4]:= GetOthers(osNoChanges);
          end;//with
          temp_cbr.option:=0;
          temp_cbr._type:= CBT_EURO_PORT_UNIT;
          temp_cbr.inputText:= '';
          temp_cbr.EuroPort.AUnit:= tempUArr[pos_x];
          temp_cbr.EuroPort.EuroNat:= europe;
          ShowMessageOptions(dat.GetLang.GetEuroPort(epsManageHeading), str_arr, temp_cbr);
          Exit;
        end;//if pos_x<>High(array)
      end;//if pos_x<>-1

      //check for button "Buy Ship"
      case GetButtonAtMouse(mouse.x, mouse.y) of
        1: begin
             SetLength(str_arr, Ord(utFrigate)-Ord(utCaravel)+2);
             str_arr[0]:= dat.GetLang.GetOthers(osNothing);
             for pos_x:= Ord(utCaravel) to Ord(utFrigate) do
               with dat.GetLang do
                 str_arr[1+pos_x-Ord(utCaravel)]:= StretchTo59(GetUnitName(TUnitType(pos_x))+':',GetOthers(osCost)
                      +' '+IntToStr(cShipPrices[TUnitType(pos_x)])+' '+GetOthers(osGold));
             temp_cbr.option:=0;
             temp_cbr._type:= CBT_EURO_PORT_BUY;
             temp_cbr.inputText:= '';
             temp_cbr.EuroBuy.EuroNat:= europe;
             temp_cbr.EuroBuy.AData:= dat;
             ShowMessageOptions(dat.GetLang.GetEuroPort(epsBuyHeading), str_arr, temp_cbr);
           end;//case ButtonAtMouse=1 ("Buy Ship")
        2: begin
             SetLength(str_arr, 1);
             str_arr[0]:= dat.GetLang.GetOthers(osNothing);
             for pos_x:= Ord(utFarmer) to Ord(utRegular) do
               if cUnitPrices[TUnitType(pos_x)]>0 then
               begin
                 SetLength(str_arr, length(str_arr)+1);
                 with dat.GetLang do
                   str_arr[High(str_arr)]:= StretchTo59(GetUnitName(TUnitType(pos_x))+':',GetOthers(osCost)
                                   +' '+IntToStr(cUnitPrices[TUnitType(pos_x)])+' '+GetOthers(osGold));
               end;//if
             temp_cbr.option:= 0;
             temp_cbr._type:= CBT_EURO_PORT_TRAIN;
             temp_cbr.inputText:= '';
             temp_cbr.EuroTrain.EuroNat:= europe;
             temp_cbr.EuroTrain.AData:= dat;
             ShowMessageOptions(dat.GetLang.GetEuroPort(epsTrainHeading),
                               str_arr, temp_cbr);
           end;//case ButtonAtMouse=2 ("Train units")
      end;//case


    end;//else if (button=LEFT) and (state=UP)
    {$IFDEF DEBUG_CODE}
    WriteLn('Exiting TGui.MouseFunc');
    {$ENDIF}
    Exit;
  end;//europe

  if InReport then
  begin
    if ((button=GLUT_LEFT) and (state=GLUT_UP)) then report:= rtNone;
    {$IFDEF DEBUG_CODE}
    WriteLn('Exiting TGui.MouseFunc');
    {$ENDIF}
    Exit;
  end;//report

  //handle map view's mouse events here
  if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
  begin
    if InMenu then
    begin
      GetMenuSelectionAtMouse(temp_cat, pos_x);
      WriteLn('GUI got selection: cat.: ', Ord(temp_cat), '; sel.: ', pos_x);//for debug
      if (pos_x=0) and (temp_cat=menu_cat) then menu_cat:= mcNone
      else if (pos_x=0) then
      begin
        menu_cat:= temp_cat;
        selected_menu_option:= 1;
      end//if
      else if (pos_x<>-1) and (temp_cat=menu_cat) then
      begin
        HandleMenuSelection(menu_cat, pos_x);
        menu_cat:= mcNone;
        selected_menu_option:= 1;
      end//if
      else if (pos_x=-1) then
      begin
        menu_cat:= mcNone;
        selected_menu_option:= 1;
      end;//if
      Exit;
    end;//if InMenu

    if Wooden_Mode then pos_x:= -1
    else begin
      //in america view
      GetSquareAtMouse(pos_x, pos_y);
      WriteLn('GUI got square: x: ', pos_x, '; y: ', pos_y);//for debug
    end;//else
    if (pos_x<>-1) then
    begin
      //check for colony first
      cur_colony:= dat.GetColonyInXY(pos_x, pos_y);
      //if not player's colony, set back to nil
      if cur_colony<>nil then
      begin
        if cur_colony.GetNation<>dat.PlayerNation then cur_colony:= nil;
      end//if colony<>nil
      else begin
        {If we don't have a colony there, there might be a unit?}
        focused:= dat.GetFirstUnitInXY(pos_x, pos_y);
      end;//else
      CenterOn(pos_x, pos_y);
      glutPostRedisplay;
    end//if pos_x<>-1
    else begin
      menu_cat:= GetMenuCategoryAtMouse;
      {$IFDEF DEBUG_CODE}
      WriteLn('GUI got category: ', Ord(menu_cat));
      {$ENDIF}
    end;//else
  end;//if
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.MouseFunc');
  {$ENDIF}
end;//proc

procedure TGui.MouseMoveFunc(const x,y: LongInt);
begin
  mouse.x:= x;
  mouse.y:= y;
end;//func

procedure TGui.Resize(Width, Height: LongInt);
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.Resize');
  {$ENDIF}
  if ((Width<>cWindowWidth) or (Height<>cWindowHeight)) then
    glutReshapeWindow(cWindowWidth, cWindowHeight);
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.Resize');
  {$ENDIF}
end;//proc

procedure TGui.InitGLUT;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.InitGLUT');
  {$ENDIF}
  WriteLn('glEnable-like stuff');
   // Enable backface culling
  glEnable(GL_CULL_FACE);
  // Set up depth buffer
  //glEnable(GL_DEPTH_TEST);
  //glDepthFunc(GL_LESS);
  glAlphaFunc(GL_GREATER, 0.2);
  //Starting
  WriteLn('glutMainLoop');
  glutMainLoop;
end;//proc

procedure TGui.Start;
begin
  InitGLUT;
end;//proc Start

procedure TGui.Draw;
var i, j: Integer;
    tempUnit: TUnit;
    tempColony: TColony;
    tempTribe: TTribe;
    tempMap: TMap;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.Draw');
  {$ENDIF}
  glLoadIdentity;
  glViewport(0,0, cWindowWidth, cWindowHeight);
  glOrtho(0.0, 20.0, -0.5, 12.5, -1.0, 1.0);

  glClearColor(0.83, 0.66, 0.39,0.0);//set "wooden" color as clear color...
                                     //saves us from drawing wooden bar
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if InColony then
  begin
    DrawColonyView;
  end//if
  else if InEurope then
  begin
    DrawEuropeanView;
  end//if
  else if InWoodenMode then
  begin
    //draw border
    glBegin(GL_QUADS);
      glColor3f(0.0, 0.0, 0.0);
      glVertex2f(0.0, y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields);
      glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields+BorderWidth);
      glVertex2f(0.0, y_Fields+BorderWidth);
    glEnd;
    DrawMenuBar;
    DrawMenu;
  end//if
  else if InReport then
  begin
    DrawReport;
  end//if
  else begin
    //draw the normal america view with map and stuff

    //draw borders
    glBegin(GL_QUADS);
      glColor3ubv(@BorderColour);
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
    glEnd;//borders

    //draw the real map
    tempMap:= dat.GetMap;
    for i:= OffsetX to OffSetX +x_Fields-1 do
      for j:= OffSetY to OffsetY +y_Fields-1 do
      begin
        if m_TerrainTexNames[tempMap.tiles[i,j].m_Type]=0 then
        begin
          glBegin(GL_QUADS);
            case tempMap.tiles[i,j].m_Type of
              ttArctic: glColor3f(1.0, 1.0, 1.0);//white
              ttSea: glColor3f(0.0, 0.0, 1.0);//blue
              ttOpenSea: glColor3f(0.3, 0.3, 1.0);//lighter blue
              ttHills, ttMountains: glColor3f(0.5, 0.0, 0.0);
            else
              glColor3f(0.3, 1.0, 0.3);//some greenish stuff
            end;//case
            glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
            glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
            glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
            glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
          glEnd;
        end//if-then
        else begin
          glEnable(GL_TEXTURE_2D);
          glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[tempMap.tiles[i,j].m_Type]);
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
          glEnd;
          glDisable(GL_TEXTURE_2D);
        end;//else branch

        //check for unit and draw unit icon, if present
        tempUnit:= dat.GetFirstUnitInXY(i,j);
        if (tempUnit<>nil) then
        begin
          if (m_UnitTexNames[tempUnit.GetType]<>0) then
          begin
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_ALPHA_TEST);
            DrawUnitIcon(tempUnit, i-OffsetX, -j-1+y_Fields+OffsetY, False, True);
            {glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[tempUnit.GetType]);
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
            glEnd;}
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          end;//if
        end;//if

        //check for colony and draw icon, if present
        tempColony:= dat.GetColonyInXY(i,j);
        if tempColony<>nil then
        begin
          if (m_ColonyTexNames[0]<>0) then
          begin
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
            glEnd;
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          end;//if
        end//if
        else begin
          tempTribe:= dat.GetTribeInXY(i,j);
          if tempTribe<>nil then
          begin
            if tempTribe.GetNation in [cMinIndian..cMaxIndian] then
            begin
              if (m_TribeTexNames[tempTribe.GetNation]<>0) then
              begin
                glEnable(GL_TEXTURE_2D);
                glEnable(GL_ALPHA_TEST);
                glBindTexture(GL_TEXTURE_2D, m_TribeTexNames[tempTribe.GetNation]);
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
                glEnd;
                glDisable(GL_ALPHA_TEST);
                glDisable(GL_TEXTURE_2D);
              end;//if
            end;//if
          end;//if
        end;//else
      end;//for
    //end of map

    //draw the MiniMap

    //draw border (as a rectangle larger than minimap)
    glBegin(GL_QUADS);
      glColor3ub(157, 86, 20);
      glVertex2f(x_Fields+ 22*PixelWidth, y_Fields);
      glVertex2f(x_Fields+ 22*PixelWidth,
                 y_Fields-2*BorderWidth - Minimap_y_Fields*2*PixelWidth);
      glVertex2f(x_Fields+ (BarWidth-22)*PixelWidth,
                 y_Fields-2*BorderWidth - Minimap_y_Fields*2*PixelWidth);
      glVertex2f(x_Fields+ (BarWidth-22)*PixelWidth, y_Fields);
    glEnd;

    //draw the actual minimap
    glBegin(GL_QUADS);
      for i:=0 to MiniMap_x_Fields-1 do
        for j:= MiniMapOffset_Y to MiniMapOffset_Y +MiniMap_y_Fields-1 do
        begin
          glColor3ubv(@cMapColour[tempMap.tiles[i,j].m_Type,0]);
          glVertex3f(x_Fields + (24+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MinimapOffset_Y)*2*PixelWidth, 0.1);
          glVertex3f(x_Fields + (24+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MinimapOffset_Y+1)*2*PixelWidth, 0.1);
          glVertex3f(x_Fields + (26+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MinimapOffset_Y+1)*2*PixelWidth, 0.1);
          glVertex3f(x_Fields + (26+2*i)*PixelWidth,
                     y_Fields-BorderWidth - (j-MinimapOffset_Y)*2*PixelWidth, 0.1);
        end;//for
    glEnd;//MiniMap
    DrawMenuBar;
    //display side bar information
    // - season and year
    WriteText(dat.GetLang.GetSeason(dat.IsAutumn)+' '+IntToStr(dat.GetYear),
              x_Fields + 4*PixelWidth,
              y_Fields - 3*BorderWidth -2*PixelWidth*MiniMap_y_Fields- 16*PixelWidth);
    // - info about focused unit
    if focused<>nil then
    begin
      if m_UnitTexNames[focused.GetType]<>0 then
      begin
        //draw unit icon
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[focused.GetType]);
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
        glEnd;
        glDisable(GL_TEXTURE_2D);
      end;//if Icon present
      // -- moves of unit
      glColor3ubv(@cMenuTextColour[0]);
      WriteText(dat.GetLang.GetOthers(osMoves)+': '+IntToStr(focused.MovesLeft),
                x_Fields +40*PixelWidth, 7.5);
      // -- location of unit
      WriteText(dat.GetLang.GetOthers(osLocation)+': '+IntToStr(focused.GetPosX)+','+IntToStr(focused.GetPosY),
                x_Fields +40*PixelWidth, 7.0);
      // -- type of unit
      WriteText(dat.GetLang.GetUnitName(focused.GetType),
                x_Fields +4*PixelWidth, 6.5);
      // -- terrain of unit's location
      WriteText(dat.GetLang.GetTerrainName(tempMap.tiles[focused.GetPosX,focused.GetPosY].GetType),
                x_Fields +4*PixelWidth, 6.0);

    end;//if Focused unit present

    //draw menu, if present
    DrawMenu;

  end;//if America view

  //show the text messages, if present
  DrawMessage;
  glutSwapBuffers();
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.Draw');
  {$ENDIF}
end;//TGui.Draw

procedure TGui.DrawColonyView;
var i,j: ShortInt;
    local_Map: TMap;
    tempStr: string;
    str_width: Integer;
    u_arr: TUnitArr;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.DrawColonyView');
  {$ENDIF}
  //draw border
  glBegin(GL_QUADS);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(0.0, y_Fields);
    glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields);
    glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields+BorderWidth);
    glVertex2f(0.0, y_Fields+BorderWidth);
  glEnd;
  //border around field map
  glLineWidth(2.0);
  glBegin(GL_LINE_STRIP);
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields);
    glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.0);
    glVertex2f(cWindowWidth*PixelWidth, y_Fields-5.0);
  glEnd;
  //draw fields
  local_Map:= dat.GetMap;
  for i:= -1 to 1 do
    for j:= -1 to 1 do
    begin
      //draw terrain
      if m_TerrainTexNames[local_Map.tiles[i+cur_colony.GetPosX,j+cur_colony.GetPosY].m_Type]=0 then
      begin
        {$IFDEF DEBUG_CODE}
          WriteLn('TGui.DrawColonyView: Trying to draw flat terrain in ',i,',',j);
        {$ENDIF}
        glBegin(GL_QUADS);
        case local_Map.tiles[i+cur_colony.GetPosX,j+cur_colony.GetPosY].m_Type of
          ttArctic: glColor3f(1.0, 1.0, 1.0);//white
          ttSea: glColor3f(0.0, 0.0, 1.0);//blue
          ttOpenSea: glColor3f(0.3, 0.3, 1.0);//lighter blue
          ttHills, ttMountains: glColor3f(0.5, 0.0, 0.0);
        else
          glColor3f(0.3, 1.0, 0.3);//some greenish stuff
        end;//case
          glVertex2f(i+x_Fields+2.0, y_Fields-3.0-j);//lower left corner
          glVertex2f(i+x_Fields+3.0, y_Fields-3.0-j);
          glVertex2f(i+x_Fields+3.0, y_Fields-2.0-j);
          glVertex2f(i+x_Fields+2.0, y_Fields-2.0-j);
        glEnd;
      end//if-then
      else begin
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[local_Map.tiles[i+cur_colony.GetPosX,j+cur_colony.GetPosY].m_Type]);
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
        glEnd;
        glDisable(GL_TEXTURE_2D);
      end;//else
      //draw units working there
      if cur_colony.GetUnitInField(i,j)<>nil then
      begin
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        DrawUnitIcon(cur_colony.GetUnitInField(i,j), i+x_Fields+2.0, y_Fields-3.0-j, True, False);
        {if m_UnitTexNames[cur_colony.GetUnitInField(i,j).GetType]<>0 then
          glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[cur_colony.GetUnitInField(i,j).GetType])
        else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
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
        glEnd;}
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
      end;//if
    end;//for
  //show text for field
  GetColonyFieldAtMouse(i,j);
  if ((i<>-2) and (cur_colony.GetUnitInField(i,j)<>nil)) then
  begin
    tempStr:= IntToStr(local_Map.tiles[i+cur_colony.GetPosX,j+cur_colony.GetPosY].GetGoodProduction(
                   cur_colony.GetUnitInFieldGood(i,j), (cur_colony.GetUnitInField(i,j).GetType=GetUnitForGood(cur_colony.GetUnitInFieldGood(i,j)))
                   and (cur_colony.GetUnitInField(i,j).GetType<>utCriminal)))
               +' '+dat.GetLang.GetGoodName(cur_colony.GetUnitInFieldGood(i,j));
    str_width:= length(tempStr)*8;
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(tempStr, x_Fields+2.5-(str_width*PixelWidth*0.5), y_Fields - 0.75);
  end;//if

  DrawColonyTitleBar;
  DrawGoodsBar;

  //draw page switchers
  // -- highlighted section
  glColor3f(0.83*0.8, 0.66*0.8, 0.39*0.8);
  if ColonyBuildingPage then
  begin
    glBegin(GL_QUADS);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-6.0);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-6.0);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.5);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.5);
    glEnd;
  end
  else begin
    glBegin(GL_QUADS);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.5);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.5);
      glVertex2f(cWindowWidth*PixelWidth-2.0, y_Fields-5.0);
      glVertex2f(cWindowWidth*PixelWidth-5.0, y_Fields-5.0);
    glEnd;
  end;//else

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
  glEnd;

  // -- captions
  glColor3ubv(@cMenuTextColour[0]);
  WriteText('Buildings', cWindowWidth*PixelWidth-5.0+4*PixelWidth, y_Fields-6.0+4*PixelWidth);
  WriteText('Units', cWindowWidth*PixelWidth-5.0+4*PixelWidth, y_Fields-5.5+4*PixelWidth);

  if ColonyBuildingPage then
  begin
    DrawColonyBuildings;
  end
  else begin

    DrawShipsInPort(nil);

    //draw units in colony
    u_arr:= dat.GetAllUnitsInColony(cur_colony);
    if length(u_arr)>0 then
    begin
      glEnable(GL_ALPHA_TEST);
      glEnable(GL_TEXTURE_2D);
      for i:= 0 to Min(23, High(u_arr)) do
      begin
        DrawUnitIcon(u_arr[i], 14.0 + (i mod 6),(cGoodBarHeight+1)*PixelWidth+(i div 6), True, True);
      end;
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_ALPHA_TEST);
    end;//if u_arr länger als 0

    //check for movable unit in field and draw it
    if (mouse.down) then
    begin
      GetColonyFieldAtMouse(i,j, mouse.down_x, mouse.down_y);
      if ((i<>-2) and (cur_colony.GetUnitInField(i,j)<>nil)) then
      begin
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        if m_UnitTexNames[cur_colony.GetUnitInField(i,j).GetType]<>0 then
          glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[cur_colony.GetUnitInField(i,j).GetType])
        else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
        i:= mouse.x mod FieldWidth;
        j:= ((mouse.y-16) mod FieldWidth)-FieldWidth;
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
        glEnd;
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
      end//if (i<>-2) and ...
      else begin
        //check for unit moved from "outside" of colony
        str_width:= GetColonyUnitAtMouse(mouse.down_x, mouse.down_y);
        if str_width<>-1 then
        begin
          u_arr:= dat.GetAllUnitsInColony(cur_colony);
          if High(u_arr)>=str_width then
          begin
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_ALPHA_TEST);
            DrawUnitIcon(u_arr[str_width], 14.0+ (str_width mod 6)+(mouse.x-mouse.down_x)*PixelWidth,
                            (cGoodBarHeight+1+mouse.down_y-mouse.y)*PixelWidth+(str_width div 6), True, False);
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          end;//if
        end//if
        else DrawGoodDraggedFromBar;
      end;//else
    end;//if mouse.down

  end;//else, i.e. not ColonyBuildingPage

  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.DrawColonyView');
  {$ENDIF}
end;//proc DrawColonyView

procedure TGui.GetBuildingPosition(const bt: TBuildingType; var x,y: Single);
begin
{Colony layout:
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
}
  //assign x
  case bt of
    btCarpenter, btFurTrader, btSchool, btWarehouse: x:= 0.5;
    btBlackSmith, btDistiller, btStable, btTownHall: x:= 4.0;
    btChurch, btWeaver, btTobacconist: x:= 7.5;
    btPress, btArmory, btDock, btFort: x:= 11.0;
  else
    x:= 0.0;
  end;//case

  //assign y
  case bt of
    btCarpenter, btBlackSmith, btChurch, btPress: y:= y_Fields -2.5;
    btFurTrader, btDistiller, btWeaver, btArmory: y:= y_Fields -5.0;
    btSchool, btStable, btTobacconist, btDock: y:= y_Fields -7.5;
    btWarehouse, btTownHall, btFort: y:= y_Fields - 10.0;
  else
    y:= 0.0;
  end;
end;//procedure GetBuildingPosition

procedure TGui.DrawColonyBuildings;
var x, y: Single;
    i: Integer;
    level: Byte;
begin
  for i:=Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
  begin
    level:= cur_colony.GetBuildingLevel(TBuildingType(i));
    //check for valid level
    if (level in [1..3]) then
    begin
      //check, whether texture is loaded
      if m_BuildingTexNames[TBuildingType(i), level]<>0 then
      begin
        GetBuildingPosition(TBuildingType(i), x,y);
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        glBindTexture(GL_TEXTURE_2D, m_BuildingTexNames[TBuildingType(i), level]);
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
        glEnd;
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
      end;//texture present
    end;//if 1<=Level<=3
  end;//for
end;//proc DrawColonyBuildings

procedure TGui.DrawEuropeanView;
var Ship, Colonists, Expected, NewWorld: TUnitArr;
begin
  //border
  glLineWidth(2.0);
  glBegin(GL_LINES);
    glColor3f(0.0, 0.0, 0.0);
    glVertex2f(0.0, y_Fields);
    glVertex2f(x_Fields+ BarWidth*PixelWidth, y_Fields);
  glEnd;
  DrawGoodsBar;
  DrawEuropeTitleBar;

  if europe<>nil then
    dat.GetEuropeanQuartett(europe.GetCount, Ship, Colonists, Expected, NewWorld)
  else
    dat.GetEuropeanQuartett(cNationEngland, Ship, Colonists, Expected, NewWorld);

  DrawShipsInPort(Ship);
  DrawPeopleInEurope(Colonists);
  DrawExpectedSoon(Expected);
  DrawShipsToNewWorld(NewWorld);

  DrawEuropeButtons;

  DrawGoodDraggedFromBar;
end;//proc

procedure TGui.DrawGoodDraggedFromBar;
var tempGood: TGoodType;
begin
  //draw dragged items
  if mouse.down then
  begin
    tempGood:= GetGoodAtMouse(mouse.down_x, mouse.down_y);
    if tempGood<>gtCross then
    begin
      glColor3f(1.0, 1.0, 1.0);
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_ALPHA_TEST);
      if m_GoodTexNames[tempGood]<>0 then glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[tempGood])
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
      glEnd;
      glDisable(GL_ALPHA_TEST);
      glDisable(GL_TEXTURE_2D);
    end;//if
  end;//if
end;//proc

procedure TGui.DrawEuropeButtons;
begin
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
  glEnd;
  glLineWidth(2.0);
  glBegin(GL_LINE_LOOP);
    //"Buy Ships" button
    glColor3f(0.75, 0.75, 1.0);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth);
    glColor3f(0.25, 0.25, 1.0);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+1.0);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+1.0);
  glEnd;
  glBegin(GL_LINE_LOOP);
    //"Train units" button
    glColor3f(0.75, 0.75, 1.0);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+1.5);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+1.5);
    glColor3f(0.25, 0.25, 1.0);
    glVertex2f(12.0, cGoodBarHeight*PixelWidth+2.5);
    glVertex2f(9.0, cGoodBarHeight*PixelWidth+2.5);
  glEnd;
  glColor3f(0.2, 0.2, 0.2);
  WriteText('Buy Ship', 9.5, cGoodBarHeight*PixelWidth+0.25);
  WriteText('Train units', 9.125, cGoodBarHeight*PixelWidth+1.75);
end;//proc

procedure TGui.DrawShipsInPort(const predefShips: TUnitArr);
var i: ShortInt;
    ShipArr: TUnitArr;
begin
  if ((cur_colony=nil) and (europe=nil)) then Exit;
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
  glEnd;
  //separating lines
  glBegin(GL_LINE_STRIP);
    glColor3f(cWoodenColour[0]*0.5, cWoodenColour[1]*0.5, cWoodenColour[2]*0.5);
    glVertex2f(1.0, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth);
    glVertex2f(7.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    glVertex2f(1.0, (cGoodBarHeight+1)*PixelWidth+1.0);
    for i:=0 to 5 do
    begin
      glVertex2f(1.0+i, (cGoodBarHeight+1)*PixelWidth);
      glVertex2f(2.0+i, (cGoodBarHeight+1)*PixelWidth);
      glVertex2f(1.0+i, (cGoodBarHeight+1)*PixelWidth+1.0);
      glVertex2f(2.0+i, (cGoodBarHeight+1)*PixelWidth+1.0);
    end;//for
  glEnd;
  //draw all present ships
  if (predefShips<>nil) then ShipArr:= predefShips
  else begin
    if cur_colony<>nil then
      ShipArr:= dat.GetAllShipsInXY(cur_colony.GetPosX, cur_colony.GetPosY)
    else ShipArr:= dat.GetAllShipsInEurope(dat.PlayerNation);
  end;//else
  if length(ShipArr)>0 then
  begin
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    //draw list of ships in port
    for i:= 0 to High(ShipArr) do
    begin
      if m_UnitTexNames[ShipArr[i].GetType]<>0 then glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[ShipArr[i].GetType])
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
      glBegin(GL_QUADS);
        glTexCoord2f(0.0, 0.0);
        glVertex2f(1.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+1.0 +(i div 6));
        glTexCoord2f(1.0, 0.0);
        glVertex2f(2.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+1.0 +(i div 6));
        glTexCoord2f(1.0, 1.0);
        glVertex2f(2.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+2.0 +(i div 6));
        glTexCoord2f(0.0, 1.0);
        glVertex2f(1.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+2.0 +(i div 6));
      glEnd;
    end;//for
    //draw icons of goods in first ship
    for i:= 0 to 5 do
    begin
      if ShipArr[0].GetCargoAmountBySlot(i)>0 then
      begin
        if m_GoodTexNames[ShipArr[0].GetCargoGoodBySlot(i)]<>0 then
          glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[ShipArr[0].GetCargoGoodBySlot(i)])
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
        glEnd;
      end;//if
    end;//for
    glDisable(GL_TEXTURE_2D);

    //draw dragged good
    if mouse.down then
    begin
      i:= GetCargoBoxAtMouse(mouse.down_x, mouse.down_y);
      if i<>-1 then
      begin
        if ShipArr[0].GetCargoAmountBySlot(i)>0 then
        begin
          glColor3f(1.0, 1.0, 1.0);
          glEnable(GL_TEXTURE_2D);
          if m_GoodTexNames[ShipArr[0].GetCargoGoodBySlot(i)]<>0 then glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[ShipArr[0].GetCargoGoodBySlot(i)])
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
          glEnd;
          glDisable(GL_TEXTURE_2D);
        end;//if
      end//if i<>-1
    end;//if mouse.down
  end;//if length>0
end;//proc DrawShipsInPort

procedure TGui.DrawPeopleInEurope(const People: TUnitArr);
var i: ShortInt;
    PeopleArr: TUnitArr;
begin
  if (europe=nil) then Exit;
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
  glEnd;
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
  glEnd;

  if (People<>nil) then PeopleArr:= People
  else PeopleArr:= dat.GetAllNonShipsInEurope(europe.GetCount);

  if length(PeopleArr)>0 then
  begin
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    //draw list of ships in port
    for i:= 0 to High(PeopleArr) do
    begin
      if m_UnitTexNames[PeopleArr[i].GetType]<>0 then glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[PeopleArr[i].GetType])
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
      glBegin(GL_QUADS);
        glColor3f(1.0, 1.0, 1.0);
        glTexCoord2f(0.0, 0.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+(i mod 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i div 6));
        glTexCoord2f(1.0, 0.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+1.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i div 6));
        glTexCoord2f(1.0, 1.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+1.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+1.5 +(i div 6));
        glTexCoord2f(0.0, 1.0);
        glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+(i mod 6), (cGoodBarHeight+1)*PixelWidth+1.5 +(i div 6));
      glEnd;
      DrawStateIcon(PeopleArr[i].GetState, (cWindowWidth-6*FieldWidth)*PixelWidth+(i mod 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i div 6));
    end;//for
    glDisable(GL_TEXTURE_2D);
  end;//if
end;//proc

procedure TGui.DrawExpectedSoon(const ExpSoon: TUnitArr);
var i: Integer;
begin
  //border
  glLineWidth(2.0);
  glBegin(GL_LINE_LOOP);
    glColor3fv(@cBlueBorderColour[0]);
    glVertex2f(1.0, y_Fields - 1.0);
    glVertex2f(1.0, y_Fields - 3.0);
    glVertex2f(1.0 +cShipsInExpectedSoon+1, y_Fields - 3.0);
    glVertex2f(1.0 +cShipsInExpectedSoon+1, y_Fields - 1.0);
  glEnd;
  WriteText('Expected soon', 1.5, y_Fields -0.75);
  if ExpSoon<>nil then
  begin
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    for i:= 0 to Min(High(ExpSoon),cShipsInExpectedSoon-1) do
    begin
      if m_UnitTexNames[ExpSoon[i].GetType]<>0 then glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[ExpSoon[i].GetType])
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
      glEnd;
    end;//for
    glDisable(GL_TEXTURE_2D);
  end;//if
end;//proc

procedure TGui.DrawShipsToNewWorld(const ToNewWorld: TUnitArr);
var i: Integer;
begin
  //border
  glLineWidth(2.0);
  glBegin(GL_LINE_LOOP);
    glColor3fv(@cBlueBorderColour[0]);
    glVertex2f(3.0+cShipsInExpectedSoon, y_Fields - 1.0);
    glVertex2f(3.0+cShipsInExpectedSoon, y_Fields - 3.0);
    glVertex2f(4.0+cShipsInExpectedSoon+cShipsInToNewWorld, y_Fields - 3.0);
    glVertex2f(4.0+cShipsInExpectedSoon+cShipsInToNewWorld, y_Fields - 1.0);
  glEnd;
  WriteText('Ziel: Neue Welt', 3.5+cShipsInExpectedSoon, y_Fields -0.75);
  if ToNewWorld<>nil then
  begin
    glColor3f(1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);
    for i:= 0 to Min(High(ToNewWorld),cShipsInToNewWorld-1) do
    begin
      if m_UnitTexNames[ToNewWorld[i].GetType]<>0 then glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[ToNewWorld[i].GetType])
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
      glEnd;
    end;//for
    glDisable(GL_TEXTURE_2D);
  end;//if
end;//proc

procedure TGui.DrawReport;
var i, j, freight_offset: Integer;
    col_arr: TColonyArr;
    u_arr: TUnitArr;
begin
  //only economy and fleet implemented yet
  case Report of
    rtEconomy: begin
                 col_arr:= dat.GetColonyList(dat.PlayerNation);

                 //draw good icons
                 glEnable(GL_TEXTURE_2D);
                 glEnable(GL_ALPHA_TEST);
                 glColor3f(1.0, 1.0, 1.0);
                 for i:= Ord(gtFood) to Ord(gtMusket) do
                 begin
                   if m_GoodTexNames[TGoodType(i)]<>0 then glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[TGoodType(i)])
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
                   glEnd;
                 end;//for
                 glDisable(GL_ALPHA_TEST);
                 glDisable(GL_TEXTURE_2D);

                 //seperating lines
                 glColor3f(0.0, 0.0, 0.0);
                 glLineWidth(2.0);
                 glBegin(GL_LINES);
                   for i:= y_Fields-2 downto 1 do
                   begin
                     glVertex2f(0.0, i);
                     glVertex2f(cWindowWidth*PixelWidth, i);
                     glVertex2f(0.0, i-0.5);
                     glVertex2f(cWindowWidth*PixelWidth, i-0.5);
                   end;//for
                   for i:= Ord(gtFood) to Ord(gtMusket) do
                   begin
                     glVertex2f(4+i-Ord(gtFood), y_Fields-2);
                     glVertex2f(4+i-Ord(gtFood), 0.0);
                   end;//for
                 glEnd;

                 //print storage amounts
                 if length(col_arr)>0 then
                 begin
                   glColor3ubv(@cMenuTextColour[0]);
                   for i:= 0 to High(col_arr) do
                   begin
                     WriteText(col_arr[i].GetName, PixelWidth, y_Fields-2.5+4*PixelWidth-i*0.5);
                     for j:= Ord(gtFood) to Ord(gtMusket) do
                       WriteText(IntToStr(col_arr[i].GetStore(TGoodType(j))), 4+2*PixelWidth+j-Ord(gtFood), y_Fields-2.5+4*PixelWidth-i*0.5)
                   end;//for
                 end//if
                 else begin
                   glColor3f(0.0, 0.0, 0.0);
                   i:= TextWidthTimesRoman24('You have no colonies yet.');
                   j:= (cWindowWidth-i) div 2;
                   glBegin(GL_QUADS);
                     glVertex2f(j*PixelWidth-1.0, y_Fields-4.75);
                     glVertex2f((i+j)*PixelWidth+1.0, y_Fields-4.75);
                     glVertex2f((i+j)*PixelWidth+1.0, y_Fields-3.75);
                     glVertex2f(j*PixelWidth-1.0, y_Fields-3.75);
                   glEnd;
                   glColor3ub(255, 0, 0);
                   WriteTimesRoman24('You have no colonies yet.', j*PixelWidth, y_Fields-4.5);
                 end;//else
               end;//case rtEconomy
    rtFleet: begin
               u_arr:= dat.GetAllShips(dat.PlayerNation);

               //headings
               glColor3ubv(@cMenuTextColour[0]);
               WriteText(dat.GetLang.GetOthers(osShip), 0.5, y_Fields-0.75);
               WriteText(dat.GetLang.GetOthers(osFreight), 5.5, y_Fields-0.75);
               WriteText(dat.GetLang.GetOthers(osLocation), 11.5, y_Fields-0.75);
               WriteText(dat.GetLang.GetOthers(osDestination), 15.5, y_Fields-0.75);
               //line grid
               glColor3f(0.0, 0.0, 0.0);
               glLineWidth(2.0);
               glBegin(GL_LINES);
                 for i:= y_Fields-1 downto 1 do
                 begin
                   glVertex2f(0.0, i);
                   glVertex2f(cWindowWidth*PixelWidth, i);
                 end;//for
                 glVertex2f(5.0, y_Fields-1);
                 glVertex2f(5.0, 1);
                 glVertex2f(11.0, y_Fields-1);
                 glVertex2f(11.0, 1);
                 glVertex2f(15.0, y_Fields-1);
                 glVertex2f(15.0, 1);
               glEnd;

               //display ships
               if length(u_arr)>0 then
               begin
                 for i:= 0 to Min(High(u_arr),cFleetReportUnitsPerPage-1) do
                 begin
                   //unit and name
                   glEnable(GL_ALPHA_TEST);
                   glEnable(GL_TEXTURE_2D);
                   DrawUnitIcon(u_arr[i], 0.0, y_Fields-2-i, True, True);
                   glDisable(GL_TEXTURE_2D);
                   glDisable(GL_ALPHA_TEST);
                   glColor3ubv(@cMenuTextColour[0]);
                   WriteText(dat.GetLang.GetUnitName(u_arr[i].GetType), 1.125, y_Fields-2-i+3*PixelWidth);

                   freight_offset:= 5;
                   //draw goods in ship
                   for j:= 0 to u_arr[i].FreightCapacity-1 do
                   begin
                     if u_arr[i].GetCargoAmountBySlot(j)>0 then
                     begin
                       glEnable(GL_ALPHA_TEST);
                       glEnable(GL_TEXTURE_2D);
                       if m_GoodTexNames[u_arr[i].GetCargoGoodBySlot(j)]<>0 then
                         glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[u_arr[i].GetCargoGoodBySlot(j)])
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
                       glEnd;
                       glDisable(GL_TEXTURE_2D);
                       glDisable(GL_ALPHA_TEST);
                       freight_offset:= freight_offset+1;
                     end;//if
                   end;//for j
                   //draw units in ship
                   for j:= 0 to u_arr[i].FreightCapacity-1 do
                   begin
                     if u_arr[i].GetPassengerBySlot(j)<>nil then
                     begin
                       glEnable(GL_ALPHA_TEST);
                       glEnable(GL_TEXTURE_2D);
                       DrawUnitIcon(u_arr[i].GetPassengerBySlot(j), freight_offset, y_Fields-2-i, True, True);
                       glDisable(GL_TEXTURE_2D);
                       glDisable(GL_ALPHA_TEST);
                       freight_offset:= freight_offset+1;
                     end;//if
                   end;//for j

                   //write location
                   glColor3ubv(@cMenuTextColour[0]);
                   case u_arr[i].GetLocation of
                     ulEurope: WriteText(dat.GetLang.GetPortName(u_arr[i].GetNation), 11.125, y_Fields-2-i+3*PixelWidth);
                     ulGoToEurope, ulGoToNewWorld: WriteText(dat.GetLang.GetOthers(osHighSea), 11.125, y_Fields-2-i+3*PixelWidth);
                   else WriteText('('+IntToStr(u_arr[i].GetPosX)+','+IntToStr(u_arr[i].GetPosY)+')', 11.125, y_Fields-2-i+3*PixelWidth);
                   end;//case location

                   //write destination
                   case u_arr[i].GetLocation of
                     ulEurope: ; //write nothing
                     ulGoToEurope: WriteText(dat.GetLang.GetPortName(u_arr[i].GetNation), 15.125, y_Fields-2-i+3*PixelWidth);
                     ulGoToNewWorld: WriteText(dat.GetLang.GetOthers(osNewWorld)+' ('+IntToStr(u_arr[i].GetPosX)+','
                                        +IntToStr(u_arr[i].GetPosY)+')', 15.125, y_Fields-2-i+3*PixelWidth);
                     ulAmerica: if u_arr[i].GetTask<>nil then
                                  if (u_arr[i].GetTask is TGoToTask) then
                                    WriteText('('+IntToStr((u_arr[i].GetTask as TGoToTask).DestinationX)+','
                                                +IntToStr((u_arr[i].GetTask as TGoToTask).DestinationY)+')',
                                                11.125, y_Fields-2-i+3*PixelWidth);
                   end;//case location for destination

                 end;//for i
               end;//if
             end;//rtFleet
    rtColony: begin
                glColor3ubv(@CMenuTextColour[0]);
                WriteText(dat.GetLang.GetMenuOption(mcReports, 2), 3.0, y_Fields-0.25);
                WriteText('Name', 0.5, y_Fields-0.75);
                WriteText('Einheiten', 6.5, y_Fields-0.75);
                col_arr:= dat.GetColonyList(dat.PlayerNation);
                if length(col_arr)>0 then
                begin
                  for i:= 0 to Min(High(col_arr), y_Fields) do
                  begin
                    if (m_ColonyTexNames[0]<>0) then
                    begin
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
                      glEnd;
                      glDisable(GL_ALPHA_TEST);
                      glDisable(GL_TEXTURE_2D);
                    end;//if ColonyTex present
                    glColor3ubv(@CMenuTextColour[0]);
                    WriteText(IntToStr(col_arr[i].GetInhabitants), 1.25, y_Fields-1.75-i);
                    WriteText(col_arr[i].GetName, 2.25, y_Fields-1.75-i);
                    u_arr:= dat.GetAllUnitsInColony(col_arr[i]);
                    glEnable(GL_TEXTURE_2D);
                    glEnable(GL_ALPHA_TEST);
                    for j:= 0 to min(20, High(u_arr)) do
                    begin
                      DrawUnitIcon(u_arr[j], 7.0+j, y_Fields-2-i, True, True);
                    end;//for
                    glDisable(GL_ALPHA_TEST);
                    glDisable(GL_TEXTURE_2D);
                  end;//for
                end//if
                else begin
                  glColor3f(0.0, 0.0, 0.0);
                  i:= TextWidthTimesRoman24('You have no colonies yet.');
                  j:= (cWindowWidth-i) div 2;
                  glBegin(GL_QUADS);
                    glVertex2f(j*PixelWidth-1.0, y_Fields-4.75);
                    glVertex2f((i+j)*PixelWidth+1.0, y_Fields-4.75);
                    glVertex2f((i+j)*PixelWidth+1.0, y_Fields-3.75);
                    glVertex2f(j*PixelWidth-1.0, y_Fields-3.75);
                  glEnd;
                  glColor3ub(255, 0, 0);
                  WriteTimesRoman24('You have no colonies yet.', j*PixelWidth, y_Fields-4.5);
                end;//else
              end;//rtColony
  end;//case
end;//proc

procedure TGui.DrawMenu;
var count, i, max_len: Integer;
    offset: GLfloat;
begin
  if menu_cat<>mcNone then
  begin
    max_len:= dat.GetLang.GetMaxLen(menu_cat);
    count:= dat.GetLang.GetOptionCount(menu_cat);
    offset:= GetMenuStartX(menu_cat);
    //draw box
    glBegin(GL_QUADS);
      glColor3fv(@cWoodenColour[0]);
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
    glEnd;
    //draw border lines
    glLineWidth(2.0);
    glBegin(GL_LINE_LOOP);
      glColor3f(0.0, 0.0, 0.0);
      glVertex2f(offset, y_Fields-count*0.5);//lower left
      glVertex2f(offset+ max_len*8*PixelWidth + 1.0, y_Fields-count*0.5);
      glVertex2f(offset+ max_len*8*PixelWidth + 1.0, y_Fields);
      glVertex2f(offset, y_Fields);
    glEnd;
    //now put the text
    glColor3ubv(@cMenuTextColour[0]);
    for i:= 1 to count do
      WriteText(dat.GetLang.GetMenuOption(menu_cat, i), 0.5+offset, 3*PixelWidth+ y_Fields-i*0.5);
  end;//if
end;//proc DrawMenu

procedure TGui.DrawUnitIcon(const the_Unit: TUnit; const left, bottom: GLfloat;
            const UseErrorIfTexNotPresent: Boolean = False; const ShowState: Boolean = False);
begin
  if the_Unit<>nil then
  begin
    if (m_UnitTexNames[the_Unit.GetType]=0) and not UseErrorIfTexNotPresent then Exit
    else if m_UnitTexNames[the_Unit.GetType]=0 then glBindTexture(GL_TEXTURE_2D, m_ErrorTexName)
    else glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[the_Unit.GetType]);
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
    glEnd;
    if ShowState and (m_StateTexNames[the_Unit.GetState]<>0) then
    begin
      //the state icon
      glBindTexture(GL_TEXTURE_2D, m_StateTexNames[the_Unit.GetState]);
      glBegin(GL_QUADS);
        if (the_Unit.GetNation in [cMin_Nations..cMaxIndian]) then
          glColor3ubv(@cNationColours[the_Unit.GetNation,0]);
        glTexCoord2f(0.0, 0.0);
        glVertex2f(left, bottom);
        glTexCoord2f(1.0, 0.0);
        glVertex2f(left+1.0, bottom);
        glTexCoord2f(1.0, 1.0);
        glVertex2f(left+1.0, bottom+1.0);
        glTexCoord2f(0.0, 1.0);
        glVertex2f(left, bottom+1.0);
      glEnd;
    end;//if icon
  end;//if
end;//proc

procedure TGui.DrawStateIcon(const state: TUnitState; const left, bottom: GLfloat);
begin
  if m_StateTexNames[state]<>0 then
  begin
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
    glEnd;
  end;//if
end;//proc DrawStateIcon

procedure TGui.DrawMessage;
var i, msg_lines, msg_opts: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.DrawMessage');
  {$ENDIF}
  //show message, where neccessary
  if msg.txt<>'' then
  begin
    if length(msg.options)=0 then
    begin
      if msg.inputCaption='' then
      begin
        {we got a simple message, no options, no input :) }
        //get required number of lines
        msg_lines:= (length(msg.txt)+59) div 60;
        //draw box
        glBegin(GL_QUADS);
          glColor3f(0.83, 0.66, 0.39);
          glVertex2f(2.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 6.5 +0.25*msg_lines);
          glVertex2f(2.0, 6.5 +0.25*msg_lines);
        glEnd;
        //draw box border
        glLineWidth(2.0);
        glBegin(GL_LINE_LOOP);
          glColor3f(0.0, 0.0, 0.0);//black
          glVertex2f(2.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 6.5 +0.25*msg_lines);
          glVertex2f(2.0, 6.5 +0.25*msg_lines);
        glEnd;
        //write lines
        glColor3ubv(@cMenuTextColour[0]);
        for i:= 1 to msg_lines do
          WriteText(copy(msg.txt,1+(i-1)*60, 60), 2.5, 6.0+0.25*msg_lines-i*0.5);
      end//if
      else begin
        {we got an input message window here}
        //get required number of lines
        msg_lines:= (length(msg.txt)+59) div 60;
        //draw box
        glBegin(GL_QUADS);
          glColor3f(0.83, 0.66, 0.39);
          //we have one more line, due to input...
          glVertex2f(2.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 6.75 +0.25*msg_lines);
          glVertex2f(2.0, 6.75 +0.25*msg_lines);
        glEnd;
        //draw box border
        glLineWidth(2.0);
        glBegin(GL_LINE_LOOP);
          glColor3f(0.0, 0.0, 0.0);//black
          glVertex2f(2.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 6.75 +0.25*msg_lines);
          glVertex2f(2.0, 6.75 +0.25*msg_lines);
        glEnd;
        //write lines of message
        glColor3ubv(@cMenuTextColour[0]);
        for i:= 1 to msg_lines do
          WriteText(copy(msg.txt,1+(i-1)*60, 60), 2.5, 6.25+0.25*msg_lines-i*0.5);
        //write caption
        WriteText(msg.inputCaption, 2.5, 5.75 -msg_lines*0.25);
        //write input text
        WriteText(msg.inputText, 3.0+ 0.25*length(msg.inputCaption), 5.75 -msg_lines*0.25);
        //draw border of "text input box"
        glBegin(GL_LINE_LOOP);
          glVertex2f(2.75+ 0.25*length(msg.inputCaption), 5.5 -msg_lines*0.25);
          glVertex2f(17.75, 5.5 -msg_lines*0.25);
          glVertex2f(17.75, 6.25 -msg_lines*0.25);
          glVertex2f(2.75+ 0.25*length(msg.inputCaption), 6.25 -msg_lines*0.25);
        glEnd;
      end;//else
    end
    else begin
      //we got options
      //get required number of lines
      msg_lines:= (length(msg.txt)+59) div 60;
      msg_opts:= length(msg.options);
      //draw box
      glBegin(GL_QUADS);
        glColor3f(0.83, 0.66, 0.39);
        glVertex2f(2.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 6.5 +0.25*(msg_lines+msg_opts));
        glVertex2f(2.0, 6.5 +0.25*(msg_lines+msg_opts));
      glEnd;
      //draw box border
      glLineWidth(2.0);
      glBegin(GL_LINE_LOOP);
        glColor3f(0.0, 0.0, 0.0);//black
        glVertex2f(2.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 6.5 +0.25*(msg_lines+msg_opts));
        glVertex2f(2.0, 6.5 +0.25*(msg_lines+msg_opts));
      glEnd;
      //write text lines
      glColor3ubv(@cMenuTextColour[0]);
      for i:= 1 to msg_lines do
        WriteText(copy(msg.txt,1+(i-1)*60, 60), 2.5, 6.0+0.25*(msg_lines+msg_opts)-i*0.5);
      //draw highlighted background for current option
      glBegin(GL_QUADS);
        glColor3f(0.83*0.8, 0.66*0.8, 0.39*0.8);
        glVertex2f(2.5, 5.4+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
        glVertex2f(17.5, 5.4+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
        glVertex2f(17.5, 5.9+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
        glVertex2f(2.5, 5.9+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
      glEnd;
      //write options
      glColor3ubv(@cMenuTextColour[0]);
      for i:= 1 to msg_opts do
        WriteText(' '+msg.options[i-1], 2.5, 6.0+0.25*(msg_lines+msg_opts)-(i+msg_lines)*0.5);
    end;//if
  end;//if
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.MouseFunc');
  {$ENDIF}
end; //TGui.DrawMessage

procedure TGui.CenterOn(const x, y: Integer);
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.CenterOn(',x,',',y,')');
  {$ENDIF}
  OffSetX:= x-7;
  if OffSetX<0 then OffsetX:= 0
  else if (OffSetX>cMap_X-x_Fields) then OffsetX:= cMap_X-x_Fields;
  OffSetY:= y-6;
  if OffSetY<0 then OffsetY:= 0
  else if (OffSetY>cMap_Y-y_Fields) then OffsetY:= cMap_Y-y_Fields;
  //Move Minimap accordingly
  MiniMapOffset_Y:= y -(Minimap_y_Fields div 2);
  if MiniMapOffset_Y<0 then MiniMapOffset_Y:=0
  else if (MiniMapOffset_Y>cMap_Y-Minimap_y_fields) then
    MiniMapOffset_Y:= cMap_Y-Minimap_y_fields;
end;//proc

procedure TGui.WriteText(const msg_txt: string; const x, y: Single);
//const cFontType = GLUT_BITMAP_8_BY_13;
{maybe we should try GLUT_BITMAP_9_BY_15 instead.
   other alternatives:
   GLUT_BITMAP_HELVETICA_10, GLUT_BITMAP_HELVETICA_12, GLUT_BITMAP_HELVETICA_18
   GLUT_BITMAP_TIMES_ROMAN_10 }
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.WriteText');
  {$ENDIF}
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_8_BY_13, Ord(msg_txt[i]));
    end;
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.WriteText');
  {$ENDIF}
end;//proc

procedure TGui.WriteHelvetica12(const msg_txt: string; const x, y: Single);
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.WriteHelvetica12');
  {$ENDIF}
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, Ord(msg_txt[i]));
    end;
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.WriteHelvetica12');
  {$ENDIF}
end;//proc

procedure TGui.WriteTimesRoman24(const msg_txt: string; const x, y: Single);
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.WriteTimes24');
  {$ENDIF}
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, Ord(msg_txt[i]));
    end;
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.WriteTimes24');
  {$ENDIF}
end;//proc

function TGui.TextWidthTimesRoman24(const msg_txt: string): LongInt;
var i: LongInt;
begin
  Result:= 0;
  for i:= 1 to length(msg_txt) do
    Result:= Result+glutBitmapWidth(GLUT_BITMAP_TIMES_ROMAN_24, Ord(msg_txt[i]));
end;//func

procedure TGui.DrawMenuBar;
var s: string;
    i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.DrawMenuBar');
  {$ENDIF}
  glColor3ubv(@cMenuTextColour[0]);
  s:= dat.GetLang.GetMenuLabel(mcGame);
  for i:= Ord(Succ(mcGame)) to Ord(High(TMenuCategory)) do
    s:= s+'  '+dat.GetLang.GetMenuLabel(TMenuCategory(i));
  WriteText(s, 0.1, 12.0+5.0*PixelWidth);
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.DrawMenuBar');
  {$ENDIF}
end;//proc

procedure TGui.DrawGoodsBar;
var i, j, str_width: Integer;
    price_str: string;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.DrawGoodsBar');
  {$ENDIF}
  //background
  glBegin(GL_QUADS);
    glColor3ub(76, 100, 172);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, cGoodBarHeight*PixelWidth-0.5);
    glVertex2f(0.0, cGoodBarHeight*PixelWidth-0.5);
  glEnd;
  glLineWidth(2.0);
  //border box
  glBegin(GL_LINE_LOOP);
    glColor3ub(192, 216, 240);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, cGoodBarHeight*PixelWidth -0.5);
    glVertex2f(0.0, cGoodBarHeight*PixelWidth-0.5);
  glEnd;
  //the vertical lines
  glBegin(GL_LINES);
    for i:= 1 to 15 do
    begin
      glVertex2f(i*38*PixelWidth, -0.5);
      glVertex2f(i*38*PixelWidth, cGoodBarHeight*PixelWidth -0.5);
    end;//for
  glEnd;
  //draw the good icons, if present
  glColor3f(1.0, 1.0, 1.0);
  for i:= Ord(gtFood) to Ord(gtMusket) do
  begin
    if m_GoodTexNames[TGoodType(i)]<>0 then
    begin
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
      glEnd;
      glDisable(GL_TEXTURE_2D);
    end;//if
  end;//for
  //Draw the read E for exit
  glColor3f(1.0, 0.0, 0.0);
  glRasterPos2f((38*16.0+5)*PixelWidth, 0.0);
  glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, Ord('E'));
  //colony
  if cur_colony<>nil then
  begin
    glColor3ub(0,0,0);
    for i:= Ord(gtFood) to Ord(gtMusket) do
    begin
      WriteText(IntToStr(cur_colony.GetStore(TGoodType(i))), (5+i*38)*PixelWidth, 4*PixelWidth -0.5);
    end;//for
  end//if
  //european port view
  else if europe<>nil then
  begin
    glColor3ub(0,0,0);
    for i:= Ord(gtFood) to Ord(gtMusket) do
    begin
      price_str:= IntToStr(europe.GetPrice(TGoodType(i), True))+'/'
                 +IntToStr(europe.GetPrice(TGoodType(i), False));
      str_width:= 0;
      for j:= 1 to length(price_str) do
        str_width:= str_width + glutBitmapWidth(GLUT_BITMAP_HELVETICA_12, Ord(price_str[j]));
      WriteHelvetica12(price_str, (2+ ((36-str_width) div 2) +i*38)*PixelWidth, 4*PixelWidth -0.5);
    end;//for
  end;//else if
  if GetGoodAtMouse<>gtCross then
  begin
    price_str:= dat.GetLang.GetGoodName(GetGoodAtMouse);
    str_width:= 8*length(price_str);
    //use "i" as temporary var to store the pixel count where the text begins
    if (str_width+mouse.x<cWindowWidth) then i:= mouse.x
    else i:= cWindowWidth-str_width;
    glBegin(GL_QUADS);
      glColor3ub(0,0,0);
      glVertex2f((i-2)*PixelWidth, 51*PixelWidth -0.5);
      glVertex2f((i+2+str_width)*PixelWidth, 51*PixelWidth -0.5);
      glVertex2f((i+2+str_width)*PixelWidth, 66*PixelWidth -0.5);
      glVertex2f((i-2)*PixelWidth, 66*PixelWidth -0.5);
    glEnd;
    glColor3ub(255, 255, 255);
    WriteText(price_str, i*PixelWidth, (cGoodBarHeight+1)*PixelWidth -0.5)
  end;//func
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.DrawGoodsBar');
  {$ENDIF}
end;//proc

procedure TGui.DrawColonyTitleBar;
var s: string;
    temp_nat: TNation;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.DrawColonyTitleBar');
  {$ENDIF}
  if cur_colony<>nil then
  begin
    with dat.GetLang do
      s:= cur_colony.GetName +'.  '+GetSeason(dat.IsAutumn)+', '+IntToStr(dat.GetYear)+'. '+GetOthers(osGold)+': ';
    temp_nat:= dat.GetNation(cur_colony.GetNation);
    if temp_nat<>nil then
    begin
      if temp_nat.IsEuropean then s:= s+IntToStr(TEuropeanNation(temp_nat).GetGold)+'°'
      else s:= s+' -1°';
    end//if
    else s:= s+' -1°';
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(s, ((cWindowWidth-8*length(s)) div 2)*PixelWidth, 12.0+5.0*PixelWidth);
  end;//if
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.DrawColonyTitleBar');
  {$ENDIF}
end;//proc

procedure TGui.DrawEuropeTitleBar;
var s: string;
begin
  if europe<>nil then
  begin
    with dat.GetLang do
      s:= GetPortName(europe.GetCount)+', '+GetNationName(europe.GetCount)+'. '+GetSeason(dat.IsAutumn)+' '+IntToStr(dat.GetYear)
        +'. '+GetOthers(osTax)+': '+IntToStr(europe.GetTaxRate)+'. '+GetOthers(osGold)+': '+IntToStr(europe.GetGold)+'°';
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(s, ((cWindowWidth-8*length(s)) div 2)*PixelWidth, 12.0+5.0*PixelWidth);
  end;//if
end;//proc

function TGui.InMenu: Boolean;
begin
  Result:= menu_cat<>mcNone;
end;//func

function TGui.InColony: Boolean;
begin
  Result:= cur_colony<>nil;
end;//func

function TGui.InEurope: Boolean;
begin
  Result:= europe<>nil;
end;//func

function TGui.InReport: Boolean;
begin
  Result:= (report<>rtNone);
end;//func

function TGui.InWoodenMode: Boolean;
begin
  Result:= Wooden_Mode;
end;//func

function TGui.GetFocusedUnit: TUnit;
begin
  Result:= focused;
end;//func

procedure TGui.GetSquareAtMouse(var sq_x, sq_y: Integer);
begin
  sq_x:= mouse.x div 32;
  if mouse.y>16 then
    sq_y:= (mouse.y-16) div 32
  else sq_y:= -1;
  if ((sq_x>=0) and (sq_x<x_Fields) and (sq_y>=0) and (sq_y<y_Fields)) then
  begin
    //all OK so far, add offset to get absolute values
    sq_x:= sq_x+OffsetX;
    sq_y:= sq_y+OffsetY;
  end
  else begin
    //values out of range
    sq_x:= -1;
    sq_y:= -1;
  end;//else
end;//func

function TGui.GetGoodAtMouse(const m_x: LongInt=-1; const m_y: LongInt=-1): TGoodType;
begin
  if ((m_x=-1) or (m_y=-1)) then
  begin
    if ((mouse.x<0) or (mouse.x>607) or (mouse.y<cWindowHeight-50) or (mouse.y>cWindowHeight-16)) then
      Result:= gtCross
    else Result:= TGoodType(Ord(gtFood)+(mouse.x div 38));
  end
  else begin
    if ((m_x<0) or (m_x>607) or (m_y<cWindowHeight-50) or (m_y>cWindowHeight-16)) then
      Result:= gtCross
    else Result:= TGoodType(Ord(gtFood)+(m_x div 38));
  end;//else
end;//func

procedure TGui.EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const inCaption, inText: ShortString; cbRec: TCallbackRec);
var temp: PQueueElem;
    i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.EnqueueNewMessage');
  {$ENDIF}
  New(temp);
  temp^.txt:= msg_txt;
  SetLength(temp^.options, length(opts));
  for i:= 0 to High(opts) do temp^.options[i]:= copy(Trim(opts[i]),1,59);
  //maximum caption is half the line long (i.e. 30 characters)
  temp^.inputCaption:= copy(Trim(inCaption),1, 30);
  temp^.inputText:= Trim(inText);
  temp^.cbRec:= cbRec;
  temp^.next:= nil;
  if msg_queue.first=nil then
  begin
    msg_queue.first:= temp;
    msg_queue.last:= temp;
  end//if
  else begin
    msg_queue.last^.next:= temp;
    msg_queue.last:= temp;
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.EnqueueNewMessage');
  {$ENDIF}
end;//proc

procedure TGui.ShowMessageSimple(const msg_txt: AnsiString);
var null_opts: TShortStrArr;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.ShowMessageSimple');
  {$ENDIF}
  if msg.txt='' then
  begin
    msg.txt:= Trim(msg_txt);
    SetLength(msg.options, 0);
    msg.inputCaption:= '';
    msg.inputText:= '';
    msg.cbRec:= cEmptyCallback;
  end
  else begin
    //enqueue new message
    SetLength(null_opts, 0);
    EnqueueNewMessage(msg_txt, null_opts, '', '', cEmptyCallback);
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.ShowMessageSimple');
  {$ENDIF}
end;//proc

procedure TGui.ShowMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; cbRec: TCallbackRec);
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.ShowMessageOptions');
  {$ENDIF}
  if msg.txt='' then
  begin
    msg.txt:= Trim(msg_txt)+cSpace60;
    SetLength(msg.options, length(opts));
    for i:= 0 to High(opts) do
      msg.options[i]:= copy(Trim(opts[i]),1,59);
    msg.inputCaption:= '';
    msg.inputText:= '';
    msg.selected_option:= 0;
    msg.cbRec:= cbRec;
  end
  else begin
    //enqueue new message
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, opts, '', '', cbRec);
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.ShowMessageOptions');
  {$ENDIF}
end;//proc

procedure TGui.ShowMessageInput(const msg_txt: AnsiString; const inCaption: ShortString; const inDefault: ShortString; cbRec: TCallbackRec);
var null_opts: TShortStrArr;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.ShowMessageInput');
  {$ENDIF}
  if msg.txt='' then
  begin
    msg.txt:= Trim(msg_txt)+cSpace60;
    SetLength(msg.options, 0);
    //input caption maximum is half the line (i.e. 30 characters)
    msg.inputCaption:= copy(Trim(inCaption),1, 30);
    msg.inputText:= Trim(inDefault);
    msg.selected_option:= 0;
    msg.cbRec:= cbRec;
  end//if
  else begin
    //enqueue new message
    SetLength(null_opts, 0);
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, null_opts, inCaption, inDefault, cbRec);
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.ShowMessageInput');
  {$ENDIF}
end;//func

procedure TGui.GetNextMessage;
var i: Integer;
    temp: PQueueElem;
    local_bool: Boolean;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TGui.GetNextMessage');
  {$ENDIF}
  //save last selection before anything else
  if ((length(msg.options)>1) or (msg.inputCaption<>'')) then
  begin
    //set last selected option
    msg.cbRec.option:= msg.selected_option;
    msg.cbRec.inputText:= msg.inputText;
    //check whether we need callbacks
    if (((msg.cbRec._type in [CBT_LOAD_GAME, CBT_SAVE_GAME]) and (msg.selected_option=0))
       or ((msg.cbRec._type=CBT_LOAD_GAME) and (dat.GetSaveInfo(msg.selected_option)='('+dat.GetLang.GetOthers(osEmpty)+')'))) then
    begin
      //skip callbacks
    end
    else begin
      //handle callbacks
      local_bool:= HandleCallback(msg.cbRec);

      case msg.cbRec._type of
        CBT_LOAD_GAME: begin
                         if not local_bool then
                         begin
                           ShowMessageSimple(dat.GetLang.GetSaveLoad(slsLoadError));
                           Wooden_Mode:= True;
                         end//if
                         else begin
                           Wooden_Mode:= False;
                           ShowMessageSimple(dat.GetLang.GetSaveLoad(slsLoadSuccess));
                         end;//else
                       end;// CBT_LOAD_GAME
        CBT_SAVE_GAME: begin
                         if local_bool then ShowMessageSimple(dat.GetLang.GetSaveLoad(slsSaveSuccess))
                         else ShowMessageSimple(dat.GetLang.GetSaveLoad(slsSaveError));
                       end;// CBT_SAVE_GAME
        CBT_ABANDON_COLONY: if local_bool then cur_colony:= nil;
      end;//case
    end;//else
  end;//if
  //now the main work
  if msg_queue.first<>nil then
  begin
    msg.txt:= msg_queue.first^.txt;
    SetLength(msg.options, length(msg_queue.first^.options));
    for i:=0 to High(msg_queue.first^.options) do
      msg.options[i]:= msg_queue.first^.options[i];
    msg.inputCaption:= msg_queue.first^.inputCaption;
    msg.inputText:= msg_queue.first^.inputText;
    msg.cbRec:= msg_queue.first^.cbRec;
    //move first pointer to new first element
    temp:= msg_queue.first;
    msg_queue.first:= msg_queue.first^.next;
    if msg_queue.first=nil then msg_queue.last:= nil;
    //shorten queue (and thus free former first element)
    Dispose(temp);
    msg.selected_option:=0;
  end//if-then-branch
  else begin
    //no new messages in queue; clear msg.
    msg.txt:= '';
    SetLength(msg.options, 0);
    msg.selected_option:=0;
    msg.inputCaption:= '';
    msg.inputText:= '';
    msg.cbRec:= cEmptyCallback;
  end;//else branch
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TGui.GetNextMessage');
  {$ENDIF}
end;//proc

procedure TGui.HandleMenuSelection(const categ: TMenuCategory; const selected: Integer);
var temp_cb: TCallbackRec;
    tempUnit: TUnit;
    str_arr: TShortStrArr;
    col_arr: TColonyArr;
    i: Integer;
    tempTask: TTask;
begin
  case categ of
    mcGame: begin
              case selected of
                1: begin //save
                     temp_cb._type:= CBT_SAVE_GAME;
                     temp_cb.SaveGame.AData:= dat;
                     str_arr:= dat.GetSaveSlots;
                     ShowMessageOptions(dat.GetLang.GetSaveLoad(slsSaveChoose),
                                        ToShortStrArr(dat.GetLang.GetOthers(osNothing), str_arr),
                                        temp_cb);
                   end;//save
                2: begin //load
                     temp_cb._type:= CBT_LOAD_GAME;
                     temp_cb.LoadGame.AData:= dat;
                     str_arr:= dat.GetSaveSlots;
                     ShowMessageOptions(dat.GetLang.GetSaveLoad(slsLoadChoose),
                                        ToShortStrArr(dat.GetLang.GetOthers(osNothing), str_arr),
                                        temp_cb);
                   end;//load
                3: begin//quit?
                     temp_cb._type:= CBT_EXIT;
                     temp_cb.cbExit:= @CBF_Exit;
                     ShowMessageOptions('Vespucci beenden?', ToShortStrArr('Nein', 'Ja'), temp_cb);
                   end;//3 of mcGame
              end;//case
            end;//mcGame
    mcView: begin
              case selected of
                1: europe:= TEuropeanNation(dat.GetNation(dat.PlayerNation)); //europe
                2: if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY); //center view
              end;//case
            end;//mcView
    mcOrders: begin
                case selected of
                  1: //fortify
                     if focused<>nil then
                       if focused.GetState=usFortified then focused.SetState(usNormal)
                       else focused.SetState(usFortified);
                  2: //goto
                     if focused<>nil then
                     begin
                       if focused.IsShip then
                       begin
                         col_arr:= dat.GetColonyList(focused.GetNation);
                         SetLength(str_arr, 1);
                         str_arr[0]:= dat.GetLang.GetOthers(osNoChanges);
                         for i:= 0 to High(col_arr) do
                           if col_arr[i].AdjacentWater(dat.GetMap) then
                           begin
                             SetLength(str_arr, length(str_arr)+1);
                             str_arr[High(str_arr)]:= col_arr[i].GetName;
                           end;//if
                         temp_cb._type:= CBT_GOTO_SHIP;
                         temp_cb.option:= 0;
                         temp_cb.GotoShip.Ship:= focused;
                         temp_cb.GotoShip.AData:= dat;
                         ShowMessageOptions('Choose a destination location:', str_arr, temp_cb);
                       end;//if
                     end;//if; mcOrders,2, goto
                  3: //clear forest
                     if focused<>nil then
                     begin
                       if not dat.GetMap.tiles[focused.GetPosX, focused.GetPosY].HasForest then
                         ShowMessageSimple(dat.GetLang.GetPioneer(psIsCleared))
                       else if focused.IsShip or (focused.GetType in [utRegular, utDragoon, utScout, utConvoy]) then
                         ShowMessageSimple(dat.GetLang.GetPioneer(psWrongUnit))
                       else if focused.GetToolAmount<20 then ShowMessageSimple(dat.GetLang.GetPioneer(psNoTools))
                       else begin
                         //do the real work now :)
                         tempTask:= TClearTask.Create(focused, focused.GetPosX, focused.GetPosY, dat.GetMap);
                         focused.SetTask(tempTask);
                       end;//else
                     end;//if //3 of orders (clear forest)
                  4: //plough fields
                     if focused<>nil then
                     begin
                       if dat.GetMap.tiles[focused.GetPosX, focused.GetPosY].IsPloughed then
                         ShowMessageSimple(dat.GetLang.GetPioneer(psIsPloughed))
                       else if dat.GetMap.tiles[focused.GetPosX, focused.GetPosY].HasForest then
                         ShowMessageSimple(dat.GetLang.GetPioneer(psNeedsClearing))
                       else if focused.IsShip or (focused.GetType in [utRegular, utDragoon, utScout, utConvoy]) then
                         ShowMessageSimple(dat.GetLang.GetPioneer(psWrongUnit))
                       else if focused.GetToolAmount<20 then ShowMessageSimple(dat.GetLang.GetPioneer(psNoTools))
                       else begin
                         //do the real work now :)
                         tempTask:= TPloughTask.Create(focused, focused.GetPosX, focused.GetPosY, dat.GetMap);
                         focused.SetTask(tempTask);
                       end;//else
                     end;//if //4 of mcOrders (plough fields)
                  5: //construct road
                     if focused<>nil then
                     begin
                       if dat.GetMap.tiles[focused.GetPosX,focused.GetPosY].HasRoad then
                         ShowMessageSimple(dat.GetLang.GetPioneer(psHasRoad))
                       else if focused.IsShip or (focused.GetType in [utRegular, utDragoon, utScout, utConvoy]) then
                         ShowMessageSimple(dat.GetLang.GetPioneer(psWrongUnit))
                       else if focused.GetToolAmount<20 then ShowMessageSimple(dat.GetLang.GetPioneer(psNoTools))
                       else begin
                         //do the real work now :)
                         tempTask:= TRoadTask.Create(focused, focused.GetPosX, focused.GetPosY, dat.GetMap);
                         focused.SetTask(tempTask);
                       end;//else
                     end;//if //5 of mcOrders (create road)
                  6: //no orders
                     if focused<>nil then
                     begin
                       focused.MovesLeft:= 0;
                       tempUnit:= dat.GetFirstLazyUnit(dat.PlayerNation);
                       if tempUnit<>nil then
                       begin
                         focused:= tempUnit;
                         CenterOn(focused.GetPosX, focused.GetPosY);
                       end//if
                       else begin
                         //no units left, start new round
                         dat.AdvanceYear;
                         dat.NewRound(dat.PlayerNation);
                         focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                       end;//else
                     end; //6 of mcOrders
                end;//case
              end;//mcOrders
    mcReports: begin
                 case selected of
                   1: report:= rtEconomy;
                   2: report:= rtColony;
                   3: report:= rtFleet;
                 end;//case
               end;//mcReports
  end;//case
end;//proc

function TGui.GetMenuStartX(const categ: TMenuCategory): GLfloat;
var temp_str: string;
    i: Integer;
begin
  //also see TGui.DrawMenuBar for further info on how these values are calculated
  if (categ in [mcNone, mcGame]) then Result:= 0.0
  else begin
    temp_str:= '';
    for i:= Ord(mcGame) to Ord(Pred(categ)) do
      temp_str:= temp_str+dat.GetLang.GetMenuLabel(TMenuCategory(i))+'  ';
    Result:= length(temp_str)*8*PixelWidth;
  end;//else
end;//func

function TGui.GetMenuCategoryAtMouse: TMenuCategory;
var temp_str: string;
    i: Integer;
begin
  if (mouse.y>16) then Result:= mcNone
  else begin
    temp_str:= '';
    Result:= mcGame;
    for i:= Ord(mcGame) to Ord(Pred(High(TMenuCategory))) do
    begin
      temp_str:= temp_str+dat.GetLang.GetMenuLabel(TMenuCategory(i))+'  ';
      if mouse.x>length(temp_str)*8 then Result:= TMenuCategory(i+1);
    end;//func
    temp_str:= temp_str+dat.GetLang.GetMenuLabel(High(TMenuCategory));
    if mouse.x>length(temp_str)*8 then Result:= mcNone;
  end;//else
end;//func

procedure TGui.GetMenuSelectionAtMouse(var cat: TMenuCategory; var sel_option: Integer);
begin
  if mouse.y<16 then
  begin
    cat:= GetMenuCategoryAtMouse;
    sel_option:=0;
  end//if
  else begin
    //get selected option
    sel_option:= mouse.y div 16;
    if ((mouse.x>=GetMenuStartX(menu_cat)*FieldWidth) and
       (mouse.x<=GetMenuStartX(menu_cat)*FieldWidth+dat.GetLang.GetMaxLen(menu_cat)*8+FieldWidth)
       and (sel_option<=dat.GetLang.GetOptionCount(menu_cat))) then
      cat:= menu_cat
    else begin
      cat:= mcNone;
      sel_option:= -1;
    end;//else
  end;//else
end;//proc

procedure TGui.GetColonyFieldAtMouse(var x_shift, y_shift: ShortInt; const m_x: LongInt=-1; m_y: LongInt=-1);
begin
  if ((m_x=-1) or (m_y=-1)) then
  begin
    x_shift:= (mouse.x div FieldWidth)-x_Fields-2;
    y_shift:= (mouse.y -16) div FieldWidth -2;
  end//if
  else begin
    x_shift:= (m_x div FieldWidth)-x_Fields-2;
    y_shift:= (m_y -16) div FieldWidth -2;
  end;//else
  if ((x_shift<-1) or (x_shift>1) or (y_shift<-1) or (y_shift>1)) then
  begin
    x_shift:= -2;
    y_shift:= -2;
  end;//if
end;//proc

function TGui.GetCargoBoxAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;
begin
  if ((m_x=-1) or (m_y=-1)) then
  begin
    if ((mouse.x<FieldWidth) or (mouse.x>=7*FieldWidth) or (mouse.y>cWindowHeight-cGoodBarHeight-16)
         or (mouse.y<cWindowHeight-cGoodBarHeight-16-FieldWidth)) then
      Result:= -1
    else Result:= (mouse.x-FieldWidth) div FieldWidth;
  end//if
  else begin
    if ((m_x<FieldWidth) or (m_x>=7*FieldWidth) or (m_y>cWindowHeight-cGoodBarHeight-16)
         or (m_y<cWindowHeight-cGoodBarHeight-16-FieldWidth)) then
      Result:= -1
    else Result:= (m_x-FieldWidth) div FieldWidth;
  end;//else
end;//func

function TGui.IsMouseInExpectedSoon(const m_x: LongInt=-1; m_y: LongInt=-1): Boolean;
begin
  if ((m_x=-1) or (m_y=-1)) then
  begin
    Result:= (mouse.x>=FieldWidth) and (mouse.x<=(2+cShipsInExpectedSoon)*FieldWidth)
             and (mouse.y>=16+FieldWidth) and (mouse.y<=16+3*FieldWidth);
  end//if
  else Result:= (m_x>=FieldWidth) and (m_x<=(2+cShipsInExpectedSoon)*FieldWidth)
             and (m_y>=16+FieldWidth) and (m_y<=16+3*FieldWidth);
end;//func

function TGui.GetExpectedSoonAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;
begin
  if ((m_x=-1) or (m_y=-1)) then
    Result:= (mouse.x- ((3*FieldWidth)div 2)) div FieldWidth
  else Result:= (m_x- ((3*FieldWidth)div 2)) div FieldWidth;
  if not((Result in [0..cShipsInExpectedSoon]) and IsMouseInExpectedSoon(m_x, m_y)) then Result:= -1;
end;//func

function TGui.IsMouseInToNewWorld(const m_x: LongInt=-1; m_y: LongInt=-1): Boolean;
begin
  if ((m_x=-1) or (m_y=-1)) then
  begin
    Result:= (mouse.x>=(3+cShipsInExpectedSoon)*FieldWidth) and (mouse.x<=(4+cShipsInExpectedSoon+cShipsinToNewWorld)*FieldWidth)
             and (mouse.y>=16+FieldWidth) and (mouse.y<=16+3*FieldWidth);
  end//if
  else Result:= (m_x>=(3+cShipsInExpectedSoon)*FieldWidth) and (m_x<=(4+cShipsInExpectedSoon+cShipsinToNewWorld)*FieldWidth)
             and (m_y>=16+FieldWidth) and (m_y<=16+3*FieldWidth);
end;//func

function TGui.GetToNewWorldAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;
begin
  if ((m_x=-1) or (m_y=-1)) then
    Result:= (mouse.x- ((3+cShipsInExpectedSoon)*FieldWidth+ FieldWidth div 2)) div FieldWidth
  else Result:= (m_x- ((3+cShipsInExpectedSoon)*FieldWidth+ FieldWidth div 2)) div FieldWidth;
  if not((Result in [0..cShipsInToNewWorld]) and IsMouseInToNewWorld(m_x, m_y)) then Result:= -1;
end;//func

function TGui.GetShipAtMouse(const m_x, m_y: LongInt): Integer;
begin
 // glVertex2f(1.0+(i mod 6), (cGoodBarHeight+1)*PixelWidth+1.0 +(i div 6));
 if ((m_x<=FieldWidth) or (m_x>=7*FieldWidth) or (m_y>=cWindowHeight-cGoodBarHeight-FieldWidth-17)) then Result:= -1
 else begin
   Result:= (m_x-FieldWidth) div FieldWidth;
   Result:= Result+6*((cWindowHeight-(cGoodBarHeight+FieldWidth+17)-m_y)div FieldWidth);
 end;//else
end;//func

function TGui.GetUnitAtMouse(const m_x, m_y: LongInt): Integer;
begin
  //glVertex2f((cWindowWidth-6*FieldWidth)*PixelWidth+(i mod 6), (cGoodBarHeight+1)*PixelWidth+0.5 +(i div 6));
  if ((m_x<=cWindowWidth-6*FieldWidth) or (m_x>=cWindowWidth) or
      (m_y>=cWindowHeight-cGoodBarHeight-1-FieldWidth) or (m_y<=16)) then
    Result:= -1
  else begin
    Result:= (m_x-(cWindowWidth-6*FieldWidth)) div FieldWidth;
    Result:= Result+ 6*(((cWindowHeight-cGoodBarHeight-1-FieldWidth)-m_y) div FieldWidth);
  end;//else
end;//func

function TGui.GetButtonAtMouse(const m_x, m_y: LongInt): Integer;
begin
  if ((m_x>=9*FieldWidth) and (m_x<=12*FieldWidth) and (m_y<=cWindowHeight-cGoodBarHeight-(FieldWidth div 2))) then
  begin
      if (m_y>=cWindowHeight-cGoodBarHeight-FieldWidth-(FieldWidth div 2)) then Result:= 1
      else if ((m_y>=cWindowHeight-cGoodBarHeight-3*FieldWidth) and (m_y<=cWindowHeight-cGoodBarHeight-2*FieldWidth)) then Result:= 2
      else Result:= -1;
  end//if
  else Result:= -1;
end;//func

function TGui.GetColonyUnitAtMouse(const m_x, m_y: LongInt): Integer;
begin
  /// 14.0 + (i mod 6),(cGoodBarHeight+1)*PixelWidth+(i div 6)
  if (m_x>=cWindowWidth) or (m_x<=14*FieldWidth) or (m_y>=cWindowHeight-(cGoodBarHeight+17)) then Result:= -1
  else begin
    Result:= (m_x-14*FieldWidth) div FieldWidth;//x-part
    Result:= Result +6* (((cWindowHeight-(cGoodBarHeight+17))-m_y) div FieldWidth);
    if Result>=24 then Result:= -1;
  end;//else
end;//func

//returns: -1 of none, 0 if upper, 1 if lower
function TGui.GetSwitcherButtonAtMouse(const m_x, m_y: LongInt): LongInt;
begin
  if (m_x>=cWindowWidth-2*FieldWidth) or (m_x<=cWindowWidth-5*FieldWidth)
     or (m_y<=16+5*FieldWidth) or (m_y>=16+6*FieldWidth) then Result:= -1
  else Result:= (m_y-16-5*FieldWidth) div 16;
end;//func

end.