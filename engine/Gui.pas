unit Gui;

interface

uses
  Map, Data, GL, GLU, GLUT, Terrain, Language, Colony, Nation, Goods, Units,
  SysUtils, BitmapReader, Callbacks;

const
  x_Fields = 15;
  y_Fields = 12;
  FieldWidth = 32;
  BarWidth = 160; //bar width in px

  PixelWidth = 0.03125; // =1/32, i.e. 1px

  Minimap_x_Fields = 56;
  Minimap_y_Fields = 39;

  cWindowWidth = 32*x_Fields+BarWidth;
  cWindowHeight = 32*y_Fields+16+16;

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
  cColonyTexNames: array [0..0] of string =(
       'colony.bmp' //normal colony
    );

  cWindowCaption = 'Vespucci v0.01';
  cSpace60 = '                                                            ';

  cMenuTextColour : array [0..2] of Byte = (20, 108, 16);
  cMenuHighColour : array [0..2] of Byte = (255, 20, 20);

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

type
  TShortStrArr = array of ShortString;
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
      mouse_x, mouse_y: Integer;
      menu_cat: TMenuCategory;
      cur_colony: TColony;
      europe: PEuropeanNation;
      focused: TUnit;
      lang: TLanguage;
      dat: TData;
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
      //colony texture "names" ( " " " " )
      m_ColonyTexNames: array [0..0] of GLuint;

      procedure InitGLUT;
      procedure DrawMenuBar;
      procedure DrawGoodsBar;
      procedure DrawColonyTitleBar;
      procedure DrawMessage;
      procedure DrawColonyView;
      procedure GetSquareAtMouse(var sq_x, sq_y: Integer);
      function  GetGoodAtMouse: TGoodType;
      procedure EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const inCaption, inText: ShortString; cbRec: TCallbackRec);
      procedure GetNextMessage;//de-facto dequeue
    public
      m_Map: TMap;
      OffsetX, OffsetY: Integer;
      MiniMapOffset_Y: Integer;
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

      procedure ShowMessageSimple(const msg_txt: AnsiString);
      procedure ShowMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; cbRec: TCallbackRec);
      procedure ShowMessageInput(const msg_txt: AnsiString; const inCaption: ShortString; const inDefault: ShortString; cbRec: TCallbackRec);

      function InMenu: Boolean;
      function InColony: Boolean;
      function InEurope: Boolean;
      function GetFocusedUnit: TUnit;
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
    Ship, passenger: TUnit;
begin
  inherited Create;
  mouse_x:= 0;
  mouse_y:= 0;
  OffsetX:= 0; OffsetY:= 0;
  MiniMapOffset_Y:= 0;
  m_Map:= TMap.Create;
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
  msg.inputCaption:= '';
  msg.inputText:= '';
  msg_queue.first:= nil;
  msg_queue.last:= nil;
  //language
  lang:= TLanguage.Create;
  ptrGui:= @self;
  dat:= TData.Create(lang);
  Ship:= dat.NewUnit(utCaravel, cNationEngland, 36, 13);
  WriteLn('First caravel created.');
  passenger:= dat.NewUnit(utColonist, cNationEngland, 36, 13);
  passenger.GiveTools(100);
  Ship.LoadUnit(passenger);
  {//wait until we have texture for regulars
  passenger:= dat.NewUnit(utRegular, cNationEngland, 36, 13);
  Ship.LoadUnit(passenger);}
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
  //colony textures
  m_ColonyTexNames[0]:= 0;
  if ReadBitmapToArr32RGB(colony_img_path+cColonyTexNames[0], tempTex, err_str) then
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
    temp_cb: TCallbackRec;
    temp_x, temp_y: Byte;
    direc: TDirection;
begin
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

  //"general" keys
  if Key=KEY_ESCAPE then
  begin
    if InColony then cur_colony:= nil
    else if InEurope then europe:= nil
    else begin
      temp_cb._type:= CBT_EXIT;
      temp_cb.cbExit:= @CBF_Exit;
      ShowMessageOptions('Vespucci beenden?', ToShortStrArr('Nein', 'Ja'), temp_cb);
    end;//else
  end;//if KEY_ESCAPE
  case UpCase(char(Key)) of
    'B': //build colony
         if focused<>nil then
         begin
           if (focused.IsShip or m_Map.tiles[focused.GetPosX, focused.GetPosY].IsWater) then
             ShowMessageSimple(lang.GetBuildColony(2))
           else begin
             if m_Map.tiles[focused.GetPosX, focused.GetPosY].GetType=ttMountains then
               ShowMessageSimple(lang.GetBuildColony(4))
             else begin
               if dat.FreeForSettlement(focused.GetPosX, focused.GetPosY) then
               begin
                 temp_cb.inputText:= '';
                 temp_cb._type:= CBT_BUILD_COLONY;
                 temp_cb.BuildColony.x:= focused.GetPosX;
                 temp_cb.BuildColony.y:= focused.GetPosY;
                 temp_cb.BuildColony.num_nation:= dat.player_nation;
                 temp_cb.BuildColony.founder:= focused;
                 temp_cb.BuildColony.AMap:= m_Map;
                 temp_cb.BuildColony.AData:= dat;
                 ShowMessageInput(lang.GetBuildColony(0), lang.GetBuildColony(1), 'Plymouth', temp_cb);
                 focused:= nil;
               end
               else
                 ShowMessageSimple(lang.GetBuildColony(3));
             end;//else
           end;//if
         end;//if
         //end of 'B'
    'F': ;//fortify
    'S': ;//sentry
    ' ': ;//space skips unit
    'C': begin
           if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY)
           else CenterOn(25, 35);//just for testing, yet
         end;
    //T is for testing only
    'T': begin
           ShowMessageSimple('Dies ist ein Test./ This is a test.');
           ShowMessageSimple('Nummer zwei.');
           ShowMessageOptions('Nummer drei.', ToShortStrArr('1', '2', '3'), cEmptyCallback);
           ShowMessageSimple('Nummer zum vierten Male. :o');
           ShowMessageInput('Number five. Please insert a text here, for testing.', 'Your text:', '(leer)', cEmptyCallback);
           ShowMessageSimple('Nummer Sechs.');
           ShowMessageOptions('Sieben mal sieben ist...', ToShortStrArr('7', '49', 'vierzig und neun', 'nicht definiert'), cEmptyCallback);
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
    //should move now
    if direc<>dirNone then
    begin
      temp_x:= focused.GetPosX;
      temp_y:= focused.GetPosY;
      ApplyDir(temp_x, temp_y, direc);
      if (focused.IsShip and not m_Map.tiles[temp_x, temp_y].IsWater and (focused.EmbarkedPassengers>0)) then
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
        temp_cb.Landfall.AMap:= m_Map;
        ShowMessageOptions(lang.GetLandfall(0), ToShortStrArr(lang.GetLandfall(1), lang.GetLandfall(2)), temp_cb);
      end
      else focused.Move(direc, m_Map);
    end;//if
    //check if unit moved out of sight, and center on it, if neccessary
    if focused<>nil then
    begin
      if ((focused.GetPosX<=OffsetX) or (focused.GetPosY<=OffsetY) or
          (focused.GetPosX>=OffsetX+x_Fields-1) or (focused.GetPosY>=OffsetY+y_Fields-1)) then
        CenterOn(focused.GetPosX, focused.GetPosY);
    end;//if
  end;//else
end;//proc

procedure TGui.MouseFunc(const button, state, x,y: LongInt);
var pos_x, pos_y: Integer;
begin
  if msg.txt<>'' then Exit;
  if (cur_colony<>nil) then
  begin
    //check for pressing the red "E" in colony view
    if ((button=GLUT_LEFT) and (state=GLUT_UP) and (x>608) and (y>cWindowHeight-50)) then
    begin
      cur_colony:= nil;
      glutPostRedisplay;
    end;//if
    Exit;
  end;
  //handle mouse events here
  if ((button=GLUT_LEFT) and (state=GLUT_UP) and (europe=nil) and (cur_colony=nil)) then
  begin
    GetSquareAtMouse(pos_x, pos_y);
    WriteLn('GUI got square: x: ', pos_x, '; y: ', pos_y);//for debug
    if (pos_x<>-1) then
    begin
      focused:= dat.GetFirstUnitInXY(pos_x, pos_y);
      CenterOn(pos_x, pos_y);
      {If we don't have a unit there, there might be a colony?}
      if focused=nil then cur_colony:= dat.GetColonyInXY(pos_x, pos_y);
      //if not player's colony, set back to nil
      if cur_colony<>nil then
        if cur_colony.GetNation<>nil then
          if cur_colony.GetNation.GetCount<>dat.player_nation then cur_colony:= nil;
      glutPostRedisplay;
    end;//if
  end;//if
end;//proc

procedure TGui.MouseMoveFunc(const x,y: LongInt);
begin
  mouse_x:= x;
  mouse_y:= y;
end;//func

procedure TGui.Resize(Width, Height: LongInt);
begin
  if ((Width<>cWindowWidth) or (Height<>cWindowHeight)) then
    glutReshapeWindow(cWindowWidth, cWindowHeight);
end;//proc

procedure TGui.InitGLUT;
begin
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
begin
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

  end;//if America view

  //show the text messages, if present
  DrawMessage;

  glutSwapBuffers();
end;//TGui.Draw

procedure TGui.DrawColonyView;
var i,j: Integer;
begin
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
  for i:= -1 to 1 do
    for j:= -1 to 1 do
    begin
      //draw terrain
      if m_TerrainTexNames[m_Map.tiles[i+cur_colony.GetPosX,j+cur_colony.GetPosY].m_Type]=0 then
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
          glVertex2f(i+x_Fields+2.0, y_Fields-3.0-j);//lower left corner
          glVertex2f(i+x_Fields+3.0, y_Fields-3.0-j);
          glVertex2f(i+x_Fields+3.0, y_Fields-2.0-j);
          glVertex2f(i+x_Fields+2.0, y_Fields-2.0-j);
        glEnd;
      end//if-then
      else begin
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, m_TerrainTexNames[m_Map.tiles[i+cur_colony.GetPosX,j+cur_colony.GetPosY].m_Type]);
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
        if m_UnitTexNames[cur_colony.GetUnitInField(i,j).GetType]<>0 then
        begin
          glEnable(GL_TEXTURE_2D);
          glEnable(GL_ALPHA_TEST);
          glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[cur_colony.GetUnitInField(i,j).GetType]);
          glBegin(GL_TEXTURE_2D);
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
          glDisable(GL_ALPHA_TEST);
          glDisable(GL_TEXTURE_2D);
        end;//if
    end;//for
  DrawColonyTitleBar;
  DrawGoodsBar;
end;//proc DrawColonyView

procedure TGui.DrawMessage;
var i, msg_lines, msg_opts: Integer;
begin
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
end; //TGui.DrawMessage

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
var i, j, str_width: Integer;
    price_str: string;
begin
  //background
  glBegin(GL_QUADS);
    glColor3ub(76, 100, 172);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, 52*PixelWidth-0.5);
    glVertex2f(0.0, 52*PixelWidth-0.5);
  glEnd;
  glLineWidth(2.0);
  //border box
  glBegin(GL_LINE_LOOP);
    glColor3ub(192, 216, 240);
    glVertex2f(0.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, -0.5);
    glVertex2f(38*PixelWidth*16.0, 52*PixelWidth -0.5);
    glVertex2f(0.0, 52*PixelWidth-0.5);
  glEnd;
  //the vertical lines
  glBegin(GL_LINES);
    for i:= 1 to 15 do
    begin
      glVertex2f(i*38*PixelWidth, -0.5);
      glVertex2f(i*38*PixelWidth, 52*PixelWidth -0.5);
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
      price_str:= IntToStr(europe^.GetPrice(TGoodType(i), True))+'/'
                 +IntToStr(europe^.GetPrice(TGoodType(i), False));
      str_width:= 0;
      for j:= 1 to length(price_str) do
        str_width:= str_width + glutBitmapWidth(GLUT_BITMAP_HELVETICA_12, Ord(price_str[j]));
      WriteHelvetica12(price_str, (2+ ((36-str_width) div 2) +i*38)*PixelWidth, 4*PixelWidth -0.5);
    end;//for
  end;//else if
  if GetGoodAtMouse<>gtCross then
  begin
    price_str:= lang.GetGoodName(GetGoodAtMouse);
    str_width:= 8*length(price_str);
    //use "i" as temporary var to store the pixel count where the text begins
    if (str_width+mouse_x<cWindowWidth) then i:= mouse_x
    else i:= cWindowWidth-str_width;
    glBegin(GL_QUADS);
      glColor3ub(0,0,0);
      glVertex2f((i-2)*PixelWidth, 51*PixelWidth -0.5);
      glVertex2f((i+2+str_width)*PixelWidth, 51*PixelWidth -0.5);
      glVertex2f((i+2+str_width)*PixelWidth, 66*PixelWidth -0.5);
      glVertex2f((i-2)*PixelWidth, 66*PixelWidth -0.5);
    glEnd;
    glColor3ub(255, 255, 255);
    WriteText(price_str, i*PixelWidth, 53*PixelWidth -0.5)
  end;//func
end;//proc

procedure TGui.DrawColonyTitleBar;
var s: string;
begin
  if cur_colony<>nil then
  begin
    s:= cur_colony.GetName +'.  '+lang.GetSeason(dat.IsAutumn)+', '+IntToStr(dat.GetYear)+'. Gold: ';
    if cur_colony.GetNation.IsEuropean then s:= s+IntToStr(TEuropeanNation(cur_colony.GetNation).GetGold)+'°'
    else s:= s+' -1°';
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

function TGui.GetFocusedUnit: TUnit;
begin
  Result:= focused;
end;//func

procedure TGui.GetSquareAtMouse(var sq_x, sq_y: Integer);
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

function TGui.GetGoodAtMouse: TGoodType;
begin
  if ((mouse_x<0) or (mouse_x>607) or (mouse_y<cWindowHeight-50) or (mouse_y>cWindowHeight-16)) then
    Result:= gtCross
  else
    Result:= TGoodType(Ord(gtFood)+(mouse_x div 38));
end;//func

procedure TGui.EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const inCaption, inText: ShortString; cbRec: TCallbackRec);
var temp: PQueueElem;
    i: Integer;
begin
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
end;//proc

procedure TGui.ShowMessageSimple(const msg_txt: AnsiString);
var null_opts: TShortStrArr;
begin
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
end;//proc

procedure TGui.ShowMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; cbRec: TCallbackRec);
var i: Integer;
begin
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
end;//proc

procedure TGui.ShowMessageInput(const msg_txt: AnsiString; const inCaption: ShortString; const inDefault: ShortString; cbRec: TCallbackRec);
var null_opts: TShortStrArr;
begin
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
end;//func

procedure TGui.GetNextMessage;
var i: Integer;
    temp: PQueueElem;
begin
  //save last selection before anything else
  if ((length(msg.options)>1) or (msg.inputCaption<>'')) then
  begin
    //set last selected option
    msg.cbRec.option:= msg.selected_option;
    msg.cbRec.inputText:= msg.inputText;
    //handle callbacks
    HandleCallback(msg.cbRec);
  end;
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
end;//proc

end.