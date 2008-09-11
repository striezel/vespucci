unit Gui;

interface

uses
  Map, Data, GL, GLU, GLUT, Terrain, Language, Colony, Nation, Goods, Units,
  SysUtils, BitmapReader
  {$IFDEF Win32}, Windows
  {$ELSE}{, keysym, X, Xlib, libc} //linux stuff here
  {$ENDIF};

const
  x_Fields = 15;
  y_Fields = 12;
  FieldWidth = 32;
  BarWidth = 160; //bar width in px

  PixelWidth = 0.03125; // =1/32, i.e. 1px

  Minimap_x_Fields = 56;
  Minimap_y_Fields = 39;

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

  cWindowCaption = 'Vespucci v0.01';
  cSpace60 = '                                                            ';

  cMenuTextColour : array [0..2] of Byte = (20, 108, 16);
  cMenuHighColour : array [0..2] of Byte = (255, 20, 20);

  //Keys
  KEY_RETURN = 13; //sure?
  KEY_ESCAPE = 27;
  KEY_SPACE = 32; //sure?

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

type
  TShortStrArr = array of ShortString;
  PQueueElem = ^TQueueElem;
  TQueueElem = record
                 txt: AnsiString;
                 options:TShortStrArr;
                 msg_ID: LongWord;
                 next: PQueueElem;
               end;//rec
  TGui = class
    private
      WindowHeight, WindowWidth: Integer;
      menu_cat: TMenuCategory;
      cur_colony: PColony;
      europe: PEuropeanNation;
      focused: TUnit;
      lang: TLanguage;
      dat: TData;
      //text messages
      msg: record
             txt: AnsiString;
             options: TShortStrArr;
             selected_option: Integer;
             msg_ID: LongWord;
           end;//rec
      {msg_queue: array of record
                            txt: AnsiString;
                            options:TShortStrArr;
                            msg_ID: LongWord;
                          end;//rec}
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

      m_last_ID: LongWord;
      m_last_processed_ID: LongWord;
      m_last_selected_option: Integer;

      procedure InitGLUT;
      procedure DrawMenuBar;
      procedure DrawGoodsBar;
      procedure DrawColonyTitleBar;
      procedure GetSquareAtMouse(const mouse_x, mouse_y: Longint; var sq_x, sq_y: Integer);
      procedure EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const msgID: LongWord=0);
      procedure GetNextMessage;//de-facto dequeue

      procedure ProcessEvents;
    public
      m_Map: TMap;
      OffsetX, OffsetY: Integer;
      MiniMapOffset_Y: Integer;
      constructor Create;
      destructor Destroy;
      procedure KeyFunc(Key: Byte; x, y: Longint; Special: Boolean = False);
      procedure MouseFunc(const button, state, x,y: Longint);
      procedure Resize(Width, Height: Longint);
      procedure Start;
      procedure Draw;
      procedure CenterOn(const x, y: Integer);
      procedure WriteText(const msg_txt: string; const x, y: Single);
      procedure WriteHelvetica12(const msg_txt: string; const x, y: Single);

      procedure ShowMessageSimple(const msg_txt: AnsiString);
      procedure ShowMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; const msgID: LongWord=0);

      function InMenu: Boolean;
      function InColony: Boolean;
      function InEurope: Boolean;
      function GetFocusedUnit: TUnit;

      function GetUniqueID: LongWord;
      function GetLastProcessedMessageID: LongWord;
      function GetLastSelectedOption: Integer;
      function GetSelectedMenuOption(const msg_txt: AnsiString; const opts: TShortStrArr): Integer;
  end;//class TGui
  PGui = ^TGui;

  function IntToStr(const i: Integer): string;
  function ToShortStrArr(const s1, s2: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3, s4: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3, s4, s5: ShortString): TShortStrArr; overload;

var
  ptrGUI: PGui;

implementation

//helper functions
function IntToStr(const i: Integer): string;
begin
  Str(i, Result);
end;//func

function ToShortStrArr(const s1, s2: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 2);
  Result[0]:= s1;
  Result[1]:= s2;
end;//func

function ToShortStrArr(const s1, s2, s3: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 3);
  Result[0]:= s1;
  Result[1]:= s2;
  Result[2]:= s3;
end;//func

function ToShortStrArr(const s1, s2, s3, s4: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 4);
  Result[0]:= s1;
  Result[1]:= s2;
  Result[2]:= s3;
  Result[3]:= s4;
end;//func

function ToShortStrArr(const s1, s2, s3, s4, s5: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 5);
  Result[0]:= s1;
  Result[1]:= s2;
  Result[2]:= s3;
  Result[3]:= s4;
  Result[4]:= s5;
end;//func

// **** TGui functions ****

constructor TGui.Create;
var i: Integer;
    tempTex: TArraySq32RGB;
    AlphaTex: TArraySq32RGBA;
    err_str: string;
begin
  inherited Create;
  OffsetX:= 0; OffsetY:= 0;
  MiniMapOffset_Y:= 0;
  m_Map:= TMap.Create;
  m_last_ID:= 0;
  m_last_processed_ID:=0;
  m_last_selected_option:= -1;
  if FileExists(america_map_path) then
  begin
    if m_Map.LoadFromFile(america_map_path) then
      WriteLn('Map "'+america_map_path+'" successfully loaded.')
    else begin
      WriteLn('Couldn''t load map file "'+america_map_path+'" properly. Using generation routine instead.');
      m_Map.Generate(0.7);
    end;
  end
  else begin
    WriteLn('Couldn''t find map file "'+america_map_path+'". Using generation routine instead.');
    m_Map.Generate(0.7);
  end;
  m_Map.GenerateSpecials;
  menu_cat:= mcNone;
  cur_colony:= nil;
  europe:= nil;
  focused:= nil;
  //message
  msg.txt:= '';
  SetLength(msg.options, 0);
  msg.selected_option:=0;
  //SetLength(msg_queue, 0);
  msg_queue.first:= nil;
  msg_queue.last:= nil;
  //language
  lang:= TLanguage.Create;
  ptrGui:= @self;
  dat:= TData.Create(lang);
  dat.NewUnit(utCaravel, cNationEngland, 36, 13);
  WriteLn('First caravel created.');
  //set texture names to "empty" and then load them
  glEnable(GL_TEXTURE_2D);
  //terrain textures
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
  begin
    m_TerrainTexNames[TTerrainType(i)]:= 0;
    if ReadBitmapToArr32RGB(terrain_img_path+cTerrainTexNames[TTerrainType(i)], tempTex, err_str) then
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
    if ReadBitmapToArr32RGB(good_img_path+cGoodTexNames[TGoodType(i)], tempTex, err_str) then
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
    if ReadBitmapToArr32RGB(unit_img_path+cUnitTexNames[TUnitType(i)], tempTex, err_str) then
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
end;//constructor

destructor TGui.Destroy;
var i: Integer;
begin
  m_Map.Destroy;
  lang.Destroy;
  dat.Destroy;
  //free textures
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
    if m_TerrainTexNames[TTerrainType(i)]<> 0 then glDeleteTextures(1, @m_TerrainTexNames[TTerrainType(i)]);
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    if m_GoodTexNames[TGoodType(i)]<> 0 then glDeleteTextures(1, @m_GoodTexNames[TGoodType(i)]);
  for i:= Ord(Low(TUnitType)) to Ord(High(TUnitType)) do
    if m_UnitTexNames[TUnitType(i)]<> 0 then glDeleteTextures(1, @m_UnitTexNames[TUnitType(i)]);
  inherited Destroy;
end;//destructor

procedure TGui.KeyFunc(Key: Byte; x, y: LongInt; Special: Boolean = False);
var tempUnit: TUnit;
begin
  //react on message
  if msg.txt<>'' then
  begin
    case Key of
      KEY_RETURN, KEY_ESCAPE, KEY_SPACE: begin
                                           GetNextMessage;
                                           glutPostRedisplay;
                                         end;//case KEY_SPACE
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

  //"general" keys
  if Key=KEY_ESCAPE then
    if GetSelectedMenuOption('Vespucci beenden?', ToShortStrArr('Nein', 'Ja')) = 1 then halt; {exit}
  case UpCase(char(Key)) of
    'F': ;//fortify
    'S': ;//sentry
    ' ': ;//space skips unit
    'C': begin
           if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY)
           else CenterOn(25, 35);//just for testing, yet
         end;
    //T is for testing only
    'T': begin
           ShowMessageSimple('Dies ist ein Test.');
           ShowMessageSimple('Nummer zwei.');
           ShowMessageOptions('Nummer drei.', ToShortStrArr('1', '2', '3'));
           ShowMessageSimple('Nummer zum vierten Male. :o');
           ShowMessageSimple('Nummer fünef.');
           ShowMessageSimple('Nummer Sechs.');
           ShowMessageOptions('Sieben mal sieben ist...', ToShortStrArr('7', '49', 'vierzig und neun', 'nicht definiert'));
         end;
  end;//case

  if (focused=nil) then
  begin
    //no unit focused
    //move map
    case Key of
      GLUT_KEY_LEFT, KEY_NUMPAD4: if (OffsetX>0) then OffsetX:= OffsetX-1;{Move map left}
      GLUT_KEY_RIGHT, KEY_NUMPAD6: if (OffsetX<cMap_X-x_Fields) then OffsetX:= OffsetX+1;{Move map right}
      GLUT_KEY_DOWN, KEY_NUMPAD2: if (OffsetY<cMap_y-y_Fields) then OffsetY:= OffsetY+1; {Move map down}
      GLUT_KEY_UP, KEY_NUMPAD8: if (OffsetY>0) then OffsetY:= OffsetY-1; {Move map up}
      KEY_SPACE: //try to get next unit
                 begin
                   focused:= dat.GetFirstLazyUnit(dat.player_nation);
                   if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY);
                 end;//case SPACE
    end;//case
  end//if
  else begin
    //we have a focused unit, so move it
    case Key of
      KEY_NUMPAD1: focused.Move(dirSW, m_Map);
      GLUT_KEY_DOWN, KEY_NUMPAD2: focused.Move(dirS, m_Map); {Move down}
      KEY_NUMPAD3: focused.Move(dirSE, m_Map);
      GLUT_KEY_LEFT, KEY_NUMPAD4: focused.Move(dirW, m_Map); {Move left}
      GLUT_KEY_RIGHT, KEY_NUMPAD6: focused.Move(dirE, m_Map); {Move right}
      KEY_NUMPAD7: focused.Move(dirNW, m_Map);
      GLUT_KEY_UP, KEY_NUMPAD8: focused.Move(dirN, m_Map); {Move unit up}
      KEY_NUMPAD9: focused.Move(dirNE, m_Map);
      KEY_SPACE: if focused.MovesLeft>0 then
                 begin
                   focused.MovesLeft:= 0;
                   tempUnit:= dat.GetFirstLazyUnit(dat.player_nation);
                   if tempUnit<>nil then
                   begin
                     focused:= tempUnit;
                     CenterOn(focused.GetPosX, focused.GetPosY);
                   end;
                 end//if
                 else begin
                   //no moves left, start new round
                   dat.AdvanceYear;
                   dat.NewRound(dat.player_nation);
                   focused:= dat.GetFirstLazyUnit(dat.player_nation);
                 end;
    end;//case
    //check if unit moved out of sight, and center on it, if neccessary
    if focused<>nil then
    begin
      if ((focused.GetPosX<=OffsetX) or (focused.GetPosY<=OffsetY) or
          (focused.GetPosX>=OffsetX+x_Fields-1) or (focused.GetPosY>=OffsetY+y_Fields-1)) then
        CenterOn(focused.GetPosX, focused.GetPosY);
    end;//if
  end;//else
end;//proc

procedure TGui.MouseFunc(const button, state, x,y: Longint);
var pos_x, pos_y: Integer;
begin
  //handle mouse events here
  if ((button=GLUT_LEFT) and (state=GLUT_UP) and (europe=nil) and (cur_colony=nil)) then
  begin
    GetSquareAtMouse(x,y, pos_x, pos_y);
    WriteLn('GUI got square: x: ', pos_x, '; y: ', pos_y);//for debug
    if (pos_x<>-1) then
    begin
      focused:= dat.GetFirstUnitInXY(pos_x, pos_y);
      CenterOn(pos_x, pos_y);
      glutPostRedisplay;
    end;//if
  end;//if
end;//proc

procedure TGui.Resize(Width, Height: Longint);
begin
  WindowWidth:= Width;
  WindowHeight:= Height;
end;//proc

procedure TGui.InitGLUT;
begin
// DoImplementThisStuff;
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
    msg_lines, msg_opts: Integer;
begin
  glLoadIdentity;
  glViewport(0,0, 32*x_Fields+BarWidth, 32*y_Fields+16+16);
  glOrtho(0.0, 20.0, -0.5, 12.5, -1.0, 1.0);

  glClearColor(0.83, 0.66, 0.39,0.0);//set "wooden" color as clear color...
                                     //saves us from drawing wooden bar
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

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
  for i:= OffsetX to OffSetX +x_Fields-1 do
    for j:= OffSetY to OffsetY +y_Fields-1 do
    begin
      if m_TerrainTexNames[m_Map.tiles[i,j].m_Type]=0 then
      begin
        glBegin(GL_QUADS);
          case m_Map.tiles[i,j].m_Type of
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
        glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[m_Map.tiles[i,j].m_Type]);
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
          glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[tempUnit.GetType]);
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
        end;//if
      end;//if
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
        glColor3ubv(@cMapColour[m_Map.tiles[i,j].m_Type,0]);
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
  glColor3ubv(@cMenuTextColour[0]);
  DrawMenuBar;
  //display side bar information
  // - season and year
  WriteText(lang.GetSeason(dat.IsAutumn)+' '+IntToStr(dat.GetYear),
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
    WriteText(lang.GetMoves+': '+IntToStr(focused.MovesLeft),
              x_Fields +40*PixelWidth, 7.5);
    // -- location of unit
    WriteText(lang.GetLocation+': '+IntToStr(focused.GetPosX)+','+IntToStr(focused.GetPosY),
              x_Fields +40*PixelWidth, 7.0);
    // -- type of unit
    WriteText(lang.GetUnitName(focused.GetType),
              x_Fields +4*PixelWidth, 6.5);
    // -- terrain of unit's location
    WriteText(lang.GetTerrainName(m_Map.tiles[focused.GetPosX,focused.GetPosY].GetType),
              x_Fields +4*PixelWidth, 6.0);

  end;//if Focused unit present

  //show message, where neccessary
  if msg.txt<>'' then
  begin
    if length(msg.options)=0 then
    begin
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

  glutSwapBuffers();
end; //TGui.Draw

procedure TGui.CenterOn(const x, y: Integer);
begin
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
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_8_BY_13, Ord(msg_txt[i]));
    end;
end;//proc

procedure TGui.WriteHelvetica12(const msg_txt: string; const x, y: Single);
var i: Integer;
begin
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, Ord(msg_txt[i]));
    end;
end;//proc

procedure TGui.DrawMenuBar;
var s: string;
    i: Integer;
begin
  s:= lang.GetMenuLabel(mcGame);
  for i:= Ord(Succ(mcGame)) to Ord(High(TMenuCategory)) do
    s:= s+'  '+lang.GetMenuLabel(TMenuCategory(i));
  WriteText(s, 0.1, 12.0+5.0*PixelWidth);
end;//proc

procedure TGui.DrawGoodsBar;
var i: Integer;
begin
  //background
  glBegin(GL_QUADS);
    glColor3ub(76, 100, 172);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, 40*PixelWidth-0.5);
    glVertex2f(0.0, 40*PixelWidth-0.5);
  glEnd;
  glLineWidth(2.0);
  //border box
  glBegin(GL_LINE_LOOP);
    glColor3ub(192, 216, 240);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, 40*PixelWidth -0.5);
    glVertex2f(0.0, 40*PixelWidth-0.5);
  glEnd;
  //the vertical lines
  glBegin(GL_LINES);
    for i:= 1 to 15 do
    begin
      glVertex2f(i*38*PixelWidth, -0.5);
      glVertex2f(i*38*PixelWidth, 40*PixelWidth -0.5);
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
        glVertex2f((i*38+7)*PixelWidth, -0.5+14*PixelWidth);
        glTexCoord2f(1.0, 0.0);
        glVertex2f((i*38+31)*PixelWidth, -0.5+14*PixelWidth);
        glTexCoord2f(1.0, 1.0);
        glVertex2f((i*38+31)*PixelWidth, -0.5+38*PixelWidth);
        glTexCoord2f(0.0, 1.0);
        glVertex2f((i*38+7)*PixelWidth, -0.5+38*PixelWidth);
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
      WriteText(IntToStr(cur_colony^.GetStore(TGoodType(i))), (5+i*38)*PixelWidth, 4*PixelWidth -0.5);
    end;//for
  end//if
  //european port view
  else if europe<>nil then
  begin
    glColor3ub(0,0,0);
    for i:= Ord(gtFood) to Ord(gtMusket) do
    begin
      WriteHelvetica12(IntToStr(europe^.GetPrice(TGoodType(i), True))+'/'
               +IntToStr(europe^.GetPrice(TGoodType(i), False)),
               (2+i*38)*PixelWidth, 4*PixelWidth -0.5);
    end;//for
  end//else if
end;//proc

procedure TGui.DrawColonyTitleBar;
var s: string;
begin
  if cur_colony<>nil then
  begin
    //year and season still need to be adjusted dynamically
    s:= cur_colony^.GetName +'.  '+lang.GetSeason(dat.IsAutumn)+', '+IntToStr(dat.GetYear)+'. Gold: ';
    if cur_colony^.GetNation^.IsEuropean then s:= s+IntToStr(TEuropeanNation(cur_colony^.GetNation^).GetGold)+'°'
    else s:= s+' -1°';
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(s, 0.1, 12.0+5.0*PixelWidth);
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

function TGui.GetFocusedUnit: TUnit;
begin
  Result:= focused;
end;//func

procedure TGui.GetSquareAtMouse(const mouse_x, mouse_y: Longint; var sq_x, sq_y: Integer);
begin
  sq_x:= mouse_x div 32;
  if mouse_y>16 then
    sq_y:= (mouse_y-16) div 32
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

procedure TGui.EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const msgID: LongWord=0);
var temp: PQueueElem;
    i: Integer;
begin
  New(temp);
  temp^.txt:= msg_txt;
  SetLength(temp^.options, length(opts));
  for i:= 0 to High(opts) do temp^.options[i]:= copy(Trim(opts[i]),1,59);
  temp^.msg_ID:= msgID;
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
end;//proc

procedure TGui.ShowMessageSimple(const msg_txt: AnsiString);
var null_opts: TShortStrArr;
begin
  if msg.txt='' then
  begin
    msg.txt:= Trim(msg_txt);
    SetLength(msg.options, 0);
    msg.msg_ID:= 0;
  end
  else begin
    //enqueue new message
    SetLength(null_opts, 0);
    EnqueueNewMessage(msg_txt, null_opts, 0);
  end;//else
end;//proc

procedure TGui.ShowMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; const msgID: LongWord=0);
var i: Integer;
    tempCardinal: LongWord;
begin
  if msg.txt='' then
  begin
    msg.txt:= Trim(msg_txt)+cSpace60;
    SetLength(msg.options, length(opts));
    for i:= 0 to High(opts) do
      msg.options[i]:= copy(Trim(opts[i]),1,59);
    msg.selected_option:= 0;
    if msgID<>0 then msg.msg_ID:= msgID
    else msg.msg_ID:= GetUniqueID;
  end
  else begin
    //enqueue new message
    if msgID<>0 then tempCardinal:= msgID else tempCardinal:= GetUniqueID;
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, opts, tempCardinal);
  end;//else
end;//proc

procedure TGui.GetNextMessage;
var i: Integer;
    temp: PQueueElem;
begin
  //save last ID and selection before anything else
  if ((msg.msg_ID<>0) and (length(msg.options)>1)) then
  begin
    m_last_processed_ID:= msg.msg_ID;
    m_last_selected_option:= msg.selected_option;
  end;
  //now the main work
  if msg_queue.first<>nil then
  begin
    msg.txt:= msg_queue.first^.txt;
    SetLength(msg.options, length(msg_queue.first^.options));
    for i:=0 to High(msg_queue.first^.options) do
      msg.options[i]:= msg_queue.first^.options[i];
    msg.msg_ID:= msg_queue.first^.msg_ID;
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
    msg.msg_ID:= 0;
  end;//else branch
end;//proc

function TGui.GetUniqueID: LongWord;
begin
  m_last_ID:= m_last_ID+1;
  Result:= m_last_ID;
end;//func

function TGui.GetLastProcessedMessageID: LongWord;
begin
  Result:= m_last_processed_ID;
end;//func

function TGui.GetLastSelectedOption: Integer;
begin
  Result:= m_last_selected_option;
end;//func

procedure TGui.ProcessEvents;
{$IFDEF Win32}
var aMsg: TMsg;
{$ELSE}
{var dpy: PDisplay;
    event: TXEvent;
    spec_key: Integer;
    key_sym: TKeySym;}
{$ENDIF}
begin
{$IFDEF Win32}
  //windows stuff here
  while (PeekMessage(aMsg, 0, 0, 0, PM_NOREMOVE)) do
  begin
    if (not GetMessage(aMsg, 0, 0, 0)) then break;
    TranslateMessage(aMsg);
    DispatchMessage(aMsg);
    Draw;
  end;//while
{$ELSE}
  //linux stuff here
  {dpy:= XOpenDisplay(nil);
  if dpy<>nil then
  begin
    WriteLn('Display opened');
    while (XPending(dpy)>0) do
    begin
      WriteLn('Pending events: ', XPending(dpy));
      XNextEvent(dpy, @event);
      case event._type of
        ButtonPress: MouseFunc(event.xbutton.button-1, GLUT_DOWN, event.xbutton.x, event.xbutton.y);
        ButtonRelease: MouseFunc(event.xbutton.button-1, GLUT_UP, event.xbutton.x, event.xbutton.y);
        KeyPress: begin
                    key_sym:= XLookupKeySym(@(event.xkey), 0);
                    spec_key:= -1;
                    case key_sym of
                      XK_F1: spec_key:= GLUT_KEY_F1;
                      XK_F2: spec_key:= GLUT_KEY_F2;
                      XK_F3: spec_key:= GLUT_KEY_F3;
                      XK_F4: spec_key:= GLUT_KEY_F4;
                      XK_F5: spec_key:= GLUT_KEY_F5;
                      XK_F6: spec_key:= GLUT_KEY_F6;
                      XK_F7: spec_key:= GLUT_KEY_F7;
                      XK_F8: spec_key:= GLUT_KEY_F8;
                      XK_F9: spec_key:= GLUT_KEY_F9;
                      XK_F10: spec_key:= GLUT_KEY_F10;
                      XK_F11: spec_key:= GLUT_KEY_F11;
                      XK_F12: spec_key:= GLUT_KEY_F12;
                      XK_Left: spec_key:= GLUT_KEY_LEFT;
                      XK_Right: spec_key:= GLUT_KEY_RIGHT;
                      XK_Up: spec_key:= GLUT_KEY_UP;
                      XK_Down: spec_key:= GLUT_KEY_DOWN;
                      XK_KP_Prior, XK_Prior: spec_key:= GLUT_KEY_PAGE_UP;
                      XK_KP_Next, XK_Next: spec_key:= GLUT_KEY_PAGE_DOWN;
                      XK_KP_Home, XK_Home: spec_key:= GLUT_KEY_HOME;
                      XK_KP_End, XK_End: spec_key:= GLUT_KEY_END;
                      XK_KP_Insert, XK_Insert: spec_key:= GLUT_KEY_INSERT;
                    end;//case
                    if spec_key<>-1 then KeyFunc(spec_key, event.xkey.x, event.xkey.y, True)
                    else KeyFunc(event.xkey.keycode, event.xkey.x, event.xkey.y, false);
                  end;//case: KeyPress
      end;//case
      Draw;
    end;//while
    XCloseDisplay(dpy);
    WriteLn('Display closed.');
  end;//if
  //usleep(200*1000);}
{$ENDIF}
end;//func

function TGui.GetSelectedMenuOption(const msg_txt: AnsiString; const opts: TShortStrArr): Integer;
var tempID: LongWord;
begin
  tempID:= GetUniqueID;
  Result:= -1;
  ShowMessageOptions(msg_txt, opts, tempID);
  repeat
    ProcessEvents;
  until GetLastProcessedMessageID=tempID;
  Result:= GetLastSelectedOption;
end;//func

end.