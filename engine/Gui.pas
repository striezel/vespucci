{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011, 2012, 2015, 2024  Dirk Stolle

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

unit Gui;

interface

uses
  Map, Data, GL, GLUT, Terrain, Language, Colony, Tribe, Nation, Goods,
  Units, SysUtils, BitmapReader, Helper, ErrorTexture, FoundingFathers,
  EuropeanNation, MessageSystem, BasicCallback, ExitCallback, LandfallCallback,
  SaveLoadCallbacks, JobChangeCallback, EuroPortCallbacks, BuildColonyCallback,
  AbandonColonyCallback, ColonyCallbacks, GotoShipCallback,
  FoundingSelectCallback, NewGameCallbacks;

type
  TRiverType = (rtOne, rtTwo_Bend, rtTwo_Straight, rtThree, rtFour);

const
  x_Fields = 15; // fields on new world map - horizontal direction
  y_Fields = 12; //fields on new world map - vertical direction
  FieldWidth = 32; //width of field in pixels
  BarWidth = 160; //bar width in px
  PixelWidth = 0.03125; // =1/32, i.e. 1px

  cGoodBarHeight = 52; //height of bar for goods (Europe; colony)

  Minimap_x_Fields = 56; //fields on mini map - horizontal direction
  Minimap_y_Fields = 39; //fields on mini map - vertical direction

  cWindowWidth = 32*x_Fields+BarWidth; //width of main window
  cWindowHeight = 32*y_Fields+16+16; //height of main window

  cShipsInExpectedSoon = 5;
  cShipsInToNewWorld = 5;

  cFleetReportUnitsPerPage = y_Fields-2;

  BorderWidth: Single = 0.0625; // =1/16, i.e. 2px
  BorderColour: array[0..2] of Byte = (0,0,0); //black

  { constant array that holds the colours of terrain fields for minimap or for
    terrain types that have no texture (yet)
  }
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

  { texture names (as in path) for terrain types }
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

  { texture names (as in path) for river on map }
  cRiverTexNames: array [TRiverType] of string =(
       'river_n.bmp', //one (spring)
       'river_ne.bmp', //2, bend
       'river_ns.bmp', //2, straight
       'river_nes.bmp', //3
       'river_all.bmp'
     );

  { texture names (as in path) for goods (icons) }
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

  { texture names (as in path) for unit icons }
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

  { texture names (as in path) for unit states }
  cStateTexNames: array[TUnitState] of string =(
       'normal.bmp', //usNormal
       'fortified.bmp', //usFortified
       'waitship.bmp', //usWaiting forShip
       'goto.bmp', //usGoTo
       'plough.bmp', //usPloughing
       'road.bmp' //usCreateRoad
    );

  { texture names (as in path) for colony

    remarks:
        This still has to be extended for colonies which are forts or a fortress.
  }
  cColonyTexNames: array [0..0] of string =(
       'colony.bmp' //normal colony
    );

  { texture names (as in path) for buildings }
  cBuildingTexNames: array [TBuildingType] of array [1..3] of string =(
      ('', '', ''), //btNone, 0
      ('stockade.bmp', 'fort.bmp', 'fortress.bmp'), //btFort, 3
      ('docks.bmp', 'trydock.bmp', 'shipyard.bmp'), //btDock, 3
      ('storage.bmp', 'storage2.bmp', ''), //btWareHouse, 2
      ('stable.bmp', '', ''), //btStable, 1
      ('customhouse.bmp', '', ''), //btCustomHouse, 1
      ('', '', ''), //btPress, 2
      ('school.bmp', 'college.bmp', 'university.bmp'), //btSchool, 3
      ('', '', ''), //btArmory, 3
      ('townhall.bmp', '', ''), //btTownhall, 1
      ('', '', ''), //btWeaver, 3
      ('tobacconist.bmp', '', ''), //btTobacconist, 3
      ('', '', ''), //btDistiller, 3
      ('', '', ''), //btFurTrader, 3
      ('carpenter.bmp', 'sawmill.bmp', ''), //btCarpenter, 2
      ('church.bmp', 'cathedral.bmp', ''), //btChurch, 2
      ('blacksmith.bmp', '', '')  //btBlackSmith, 3
    );

  { texture names (as in path) for tribes }
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

  { caption of game window }
  cWindowCaption = 'Vespucci v0.01.r217';

  { text colour (greenish) }
  cMenuTextColour : array [0..2] of Byte = (20, 108, 16);
  { colour for highlighted menu items }
  cMenuHighColour : array [0..2] of Byte = (255, 20, 20);
  { background colour ("wooden" colour) }
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
  { pointer type for elements of the message queue }
  //PQueueElem = ^TQueueElem;
  { record that hold an element of the message queue }
  {TQueueElem = record
                 txt: AnsiString;
                 options:TShortStrArr;
                 inputCaption, inputText: ShortString;
                 cbRec: TBasicCallback;
                 next: PQueueElem;
               end;//rec}

  { ********
    **** TGui class
    ****
    **** purpose: implements the graphical user interface of the game, i.e.
    ****          handles user input and graphical output
    ****
    **** TODO:
    **** =====
    ****
    **** - When new round is started, units of other nations should be updated,
    ****   too, and not only the player's units.
    **** - A lot more.
    *******
  }
  TGui = class
    private
      mouse: record
               x,y: LongInt;
               down: Boolean;
               down_x, down_y: LongInt;
             end;//rec
      { the currently selected menu category - if any }
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
      report_pages: Integer;
      //terrain texture "names" (as in OpenGL names)
      m_TerrainTexNames: array [TTerrainType] of GLuint;
      //river texture "names" (as in OpenGL names)
      m_RiverTexNames: array [TRiverType] of GLuint;
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

      { draws the menu bar (New World view) }
      procedure DrawMenuBar;

      { draws the good bar (in both colonies and European view) }
      procedure DrawGoodsBar;

      { draws the hover text on the good bar (in both colonies and European view) }
      procedure DrawGoodsBarHoverText;

      { draws the title bar for colonies }
      procedure DrawColonyTitleBar;

      { draws the title bar for European view }
      procedure DrawEuropeTitleBar;

      { draws the message window (if a message is present) }
      procedure DrawMessage;

      { draws the colony view }
      procedure DrawColonyView;

      { retrieves the position of a building in GL coordinates

        parameters:
            bt  - the type of the building whose position is needed
            x,y - var parameters that will contain the x- and y-coordinates of
                  the building's position

        remarks:
            The position is the lower left corner of the building texture.
      }
      procedure GetBuildingPosition(const bt: TBuildingType; var x,y: Single);

      { draws the buildings in a colony }
      procedure DrawColonyBuildings;

      { draws the European view (port of original nation) }
      procedure DrawEuropeanView;

      { draws the icon of the good that is dragged from the good bar to a ship
        or vice versa
      }
      procedure DrawGoodDraggedFromBar;

      { draws the buttons in European view, i.e. buttons for buying ships and
        training units at the academy
      }
      procedure DrawEuropeButtons;

      { draws all ships that are in the port

        parameters:
            predefShips - the prefetched list of units (i.e. ships) in the port
                          This can also be nil. In that case the function will
                          retrieve the ships for the colony or European port,
                          respectively, itself.

        remarks:
            This procedure will do nothing, of the GUI is not in colony view or
            European view.
      }
      procedure DrawShipsInPort(const predefShips: TUnitArr);

      { draws all units in European view

        parameters:
            People - predefined list of units in Europe. If this is nil, then
                     the procedure will retrieve a list of units of the current
                     nation itself.
      }
      procedure DrawPeopleInEurope(const People: TUnitArr);

      { draws all ships that are expected to arrive in Europe soon

        parameters:
            ExpSoon - the list of ships - must NOT be nil
      }
      procedure DrawExpectedSoon(const ExpSoon: TUnitArr);

      { draws all ships that are sailing to the new world

        parameters:
            ToNewWorld - list of ships that are sailing to the new world

        remarks:
            If ToNewWorld is nil, then no ships will be drawn.
      }
      procedure DrawShipsToNewWorld(const ToNewWorld: TUnitArr);

      { draws the current report (if any) }
      procedure DrawReport;

      {draws the report about continental congress }
      procedure DrawCongressReport;

      { draws the colony report }
      procedure DrawColonyReport;

      { draws the foreign affairs report }
      procedure DrawForeignAffairsReport;

      { draws the job report }
      procedure DrawJobReport;

      { draws the report about Indian nations }
      procedure DrawIndianReport;

      { draws the score report }
      procedure DrawScoreReport;

      { draws the menu (if active) }
      procedure DrawMenu;

      { draws a unit icon a the given position

        parameters:
            the_Unit - the unit whose icon has to be drawn (must not be nil, or
                       nothing will be drawn by this procedure)
            left     - the x-coordinate of the lower left corner of the icon
            bottom   - the y-coordinate of the lower left corner of the icon
            UseErrorIfTexNotPresent - if set to true, the error texture will be
                                      used as the unit's icon, if no icon
                                      texture for that unit is present
            ShowState - if true, the state of the unit will also be drawn
      }
      procedure DrawUnitIcon(const the_Unit: TUnit; const left, bottom: GLfloat;
                  const UseErrorIfTexNotPresent: Boolean = False; const ShowState: Boolean = False);

      { draws the state icon for a unit

        parameters:
            state  - the state of the unit
            left   - the x-coordinate of the lower left corner of the icon
            bottom - the y-coordinate of the lower left corner of the icon
      }
      procedure DrawStateIcon(const state: TUnitState; const left, bottom: GLfloat);

      { returns the coordinates of the map square at the current mouse position

        parameters:
           sq_x - will hold the x-coordinate of the map square, or -1 if the
                  mouse position is not within a valid map square
           sq_y - will hold the y-coordinate of the map square, or -1 if the
                  mouse position is not within a valid map square

        remarks:
            If the mouse position does not point to a valid map square, both
            sq_x and sq_y will be set to -1. Therefore, it's sufficient to
            check only one of these values for -1 in order to detect an invalid
            mouse position.
      }
      procedure GetSquareAtMouse(var sq_x, sq_y: Integer);

      { returns the current good at the mouse position

        parameters:
            m_x, m_y - the position of the mouse in pixel/ window coordinates.
                       If both m_x and m_y are set to -1, the current mouse
                       position will be used.

        remarks:
            If the given mouse position does not point to a good at the good
            bar, then the return value will be gtCross. (Crosses are not shown
            at the good bar, so this can be distinguished from goods shown at
            the bar.)
      }
      function  GetGoodAtMouse(const m_x: LongInt=-1; const m_y: LongInt=-1): TGoodType;

      { returns the menu category at the current mouse position

        remarks:
            If there is no menu category at the mouse position, mcNone will be
            returned.
      }
      function  GetMenuCategoryAtMouse: TMenuCategory;

      { returns the menu selection, i.e. menu category and index of the selected
        option, at the current mouse position

        parameters:
            cat - used to return the selected category
            sel_option - used to return the selected option

        remarks:
            Valid options are zero (i.e. the menu bar itself) or greater than
            zero (i.e. an option was selected). If a value of -1 is returned,
            the mouse position cannot be a valid menu selection.
            To indicate an invalid position for a menu selection, the returned
            category will be mcNone.
      }
      procedure GetMenuSelectionAtMouse(var cat: TMenuCategory; var sel_option: Integer);

      { returns the offset of the colony field at the specified mouse position

        parameters:
            x_shift  - is used to return the x-offset of the colony field
            y_shift  - is used to return the y-offset of the colony field
            m_x, m_y - mouse position; if both values are set to -1, the current
                       mouse position is used

        remarks:
            If the given mouse position does not point to a valid field, both
            x_shift and y_shift will be set to -2. (Valid values of x_shift and
            y_shift are in [-1;1].)
      }
      procedure GetColonyFieldAtMouse(var x_shift, y_shift: ShortInt; const m_x: LongInt=-1; m_y: LongInt=-1);

      { returns the index of the cargo box at the specified mouse position

        parameters:
            m_x, m_y - mouse position; if both are set to -1, the current mouse
                       position is used in the calculation.

        remarks:
            Valid return values are greater than or equal to zero. A return
            value of -1 indicates that the mouse position does not point to a
            valid cargo box.
      }
      function  GetCargoBoxAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;

      { returns true, if the given mouse position points to a location within
        the "Expected Soon" box in European view

        parameters:
            m_x, m_y - mouse position; if both are set to -1, the current mouse
                       position is used in the calculation.
      }
      function  IsMouseInExpectedSoon(const m_x: LongInt=-1; m_y: LongInt=-1): Boolean;

      { returns true, if the given mouse position points to a location within
        the "Going To New World" box in European view

        parameters:
            m_x, m_y - mouse position; if both are set to -1, the current mouse
                       position is used in the calculation.
      }
      function  IsMouseInToNewWorld(const m_x: LongInt=-1; m_y: LongInt=-1): Boolean;

      { returns the index within the "Expected Soon" box in European view that
        the given mouse position points to

        parameters:
            m_x, m_y - mouse position; if both are set to -1, the current mouse
                       position is used in the calculation.

        remarks:
            Valid return values are within the range [0;cShipsInExpectedSoon].
            A value of -1 indicates that the mouse position does not point to a
            valid location.
      }
      function  GetExpectedSoonAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;

      { returns the index within the "Going to new world" box in European view
        that the given mouse position points to

        parameters:
            m_x, m_y - mouse position; if both are set to -1, the current mouse
                       position is used in the calculation.

        remarks:
            Valid return values are within the range [0;cShipsInToNewWorld].
            A value of -1 indicates that the mouse position does not point to a
            valid location.
      }
      function  GetToNewWorldAtMouse(const m_x: LongInt=-1; m_y: LongInt=-1): ShortInt;

      { returns the index of the ship at the given mouse position

        parameters:
            m_x, m_y - the position of the mouse

        remarks:
            A return value of -1 is used to indicate an invalid mouse position.
            Valid return values are equal to or greater than zero.
      }
      function  GetShipAtMouse(const m_x, m_y: LongInt): Integer;

      { returns the index of the unit (not ship) at the given mouse position

        parameters:
            m_x, m_y - the position of the mouse

        remarks:
            A return value of -1 is used to indicate an invalid mouse position.
            Valid return values are equal to or greater than zero.
      }
      function  GetUnitAtMouse(const m_x, m_y: LongInt): Integer;

      { returns the index of the button (European view) at the given mouse
        position

        parameters:
            m_x, m_y - the position of the mouse

        remarks:
            A return value of -1 is used to indicate an invalid mouse position.
            Valid return values are 1 or 2.
      }
      function  GetButtonAtMouse(const m_x, m_y: LongInt): Integer;

      { returns the index of the colony unit at the given mouse position

        parameters:
            m_x, m_y - the position of the mouse

        remarks:
            A return value of -1 is used to indicate an invalid mouse position.
            Valid return values are equal to or greater than zero.
      }
      function  GetColonyUnitAtMouse(const m_x, m_y: LongInt): Integer;

      { returns the index of the switcher button at the given mouse position

        parameters:
            m_x, m_y - the position of the mouse

        remarks:
            A return value of -1 is used to indicate an invalid mouse position.
            Valid return values are zero (upper button) and one (lower button).
      }
      function  GetSwitcherButtonAtMouse(const m_x, m_y: LongInt): LongInt;

      { returns the type of building at the given mouse position

        parameters:
            m_x, m_y - the position of the mouse

        remarks:
            A return value of btNone is used to indicate an invalid mouse
            position.
      }
      function  GetBuildingAtMouse(const mx, my: LongInt): TBuildingType;

      { returns true, if the given mouse position points to a location within
        the construction bar in Colony view

        parameters:
            m_x, m_y - mouse position
      }
      function  IsMouseInConstructionBar(const mx, my: LongInt): Boolean;

      { returns true, if the given mouse position points to a location within
        the message box

        parameters:
            m_x, m_y - mouse position
      }
      function  IsMouseInMessageBox(const mx, my: LongInt): Boolean;

      { returns the message option at the given mouse position

        parameters:
            m_x, m_y - the position of the mouse

        remarks:
            A return value of -1 is used to indicate an invalid mouse position.
      }
      function GetMessageOptionAtMouse(const mx, my: LongInt): Integer;

      { deletes the current message and replaces it with the next message in
        the message queue
      }
      procedure GetNextMessage;//de-facto dequeue

      { handles the given menu selection

        parameters:
            categ    - the menu category
            selected - index of the selected option
      }
      procedure HandleMenuSelection(const categ: TMenuCategory; const selected: Integer);

      { gets the horizontal offset where the given menu category starts

        parameters:
            categ - the menu category
      }
      function  GetMenuStartX(const categ: TMenuCategory): GLfloat;

      { clears all GLUT callbacks, i.e. sets them to nil }
      procedure GLUTCallbacksToNil;

      { shows a new message that let's the player choose a new founding father, if applicable }
      procedure CheckFoundingFatherMessage;
    public
      { constructor }
      constructor Create;

      { destructor }
      destructor Destroy; override;

      { procedure to inject key events to the GUI

        parameters:
            Key     - the key that was pressed
            x,y     - position (mouse)
            special - true to indicate that a special key was pressed

        remarks:
            The first three parameters are equal to those from the corresponding
            key callback of GLUT.
      }
      procedure KeyFunc(Key: Byte; {x, y: LongInt;} Special: Boolean = False);

      { procedure to inject mouse click events to the GUI

        parameters:
            button - the mouse button that was pressed or released
            state  - state of the mouse button (up/down)
            x,y    - mouse position

        remarks:
            The parameters are equal to those from the corresponding mouse
            callback of GLUT.
      }
      procedure MouseFunc(const button, state, x,y: LongInt);

      { procedure to inject mouse movement events to the GUI

        parameters:
            x,y    - mouse position

        remarks:
            The parameters are equal to those from the corresponding mouse
            callback of GLUT.
      }
      procedure MouseMoveFunc(const x,y: LongInt);

      { procedure that should be called when the window is resized

        parameters:
            width, height - new width and height of the window
      }
      procedure Resize(Width, Height: LongInt);

      { starts the GUI and GLUT's main loop }
      procedure Start;

      { draws a new frame }
      procedure Draw;

      { centers the new world map on a certain map square

        parameters:
            x,y - coordinates of the map square that shall be in the center
      }
      procedure CenterOn(const x, y: Integer);

      { writes a text in the game's standard font at the given position

        parameters:
            msg_txt - the text that has to be written
            x,y     - GL-coordinates of the lower left corner of the first letter
      }
      procedure WriteText(const msg_txt: string; const x, y: Single);

      { writes a text in the Helvetica font at the given position

        parameters:
            msg_txt - the text that has to be written
            x,y     - GL-coordinates of the lower left corner of the first letter
      }
      procedure WriteHelvetica12(const msg_txt: string; const x, y: Single);

      { writes a text in the Times Roman font at the given position

        parameters:
            msg_txt - the text that has to be written
            x,y     - GL-coordinates of the lower left corner of the first letter
      }
      procedure WriteTimesRoman24(const msg_txt: string; const x, y: Single);

      { returns the width in pixels a text string would have if it was written
        onto the screen with the Times Roman 24 pt font

        parameters:
            msg_txt - the text
      }
      function TextWidthTimesRoman24(const msg_txt: string): LongInt;

      { returns true, if the menu is active }
      function InMenu: Boolean;

      { returns true, if the GUI is in colony view }
      function InColony: Boolean;

      { returns true, if the GUI is in European view }
      function InEurope: Boolean;

      { returns true, if the GUI shows a report }
      function InReport: Boolean;

      { returns true, if the GUI is in "wooden mode" (i.e. has no valid data yet) }
      function InWoodenMode: Boolean;

      { returns the currently focused unit, or nil if no unit is in focus }
      function GetFocusedUnit: TUnit;
  end;//class TGui

  { returns the vertical mouse position translated to OpenGL coordinates }
  function MouseYToGLCoord(const my: LongInt): Single;

implementation

uses
  DebugWriter;

function MouseYToGLCoord(const my: LongInt): Single;
begin

  // vertical mouse position translated to OpenGL coordinates
  Result:= -13.0/cWindowHeight*my+12.5;
end; //func

// **** TGui functions ****

constructor TGui.Create;
var i, j: Integer;
    bits: Byte;
    //general texture format
    tempTex: TArraySq32RGB;
    AlphaTex: TArraySq32RGBA;
    //building texture format
    BuildTex: TArray128x64RGB;
    AlphaBuildTex: TArray128x64RGBA;
    err_str: string;
    Ship: TUnit;
begin
  WriteDebugLn('Entered TGui.Create');
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

  Randomize;
  bits:= 0;
  for j:= cMinEuropean to cMaxEuropean do
  begin
    repeat
      i:= Random(4);
    until ((1 shl i) and bits)=0;
    (dat.GetNation(j) as TEuropeanNation).SetSpawnpoint(cSpawnpointsAmerica[i].x, cSpawnpointsAmerica[i].y);
    dat.SpawnEuropeanNation(j, cSpawnpointsAmerica[i].x, cSpawnpointsAmerica[i].y);
    bits:= bits or (1 shl i);
  end;//for
  WriteDebugLn('First ships created.');

  menu_cat:= mcNone;
  selected_menu_option:= 1;
  report:= rtNone;
  report_pages:= 0;
  cur_colony:= nil;
  ColonyBuildingPage:= False;
  europe:= nil;
  focused:= nil;

  //center on caravel
  Ship:= dat.GetFirstLazyUnit(dat.PlayerNation);
  if (Ship<>nil) then
  begin
    CenterOn(Ship.GetPosX, Ship.GetPosY);
    focused:= Ship;
  end;

  //set texture names to "empty" and then load them
  glEnable(GL_TEXTURE_2D);
  //terrain textures
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
  begin
    m_TerrainTexNames[TTerrainType(i)]:= 0;
    err_str:= '(undefined)';
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

  //river textures
  for i:= Ord(Low(TRiverType)) to Ord(High(TRiverType)) do
  begin
    m_RiverTexNames[TRiverType(i)]:= 0;
    if ReadBitmapToArr32RGB(dat.GetPathBase+terrain_img_path+cRiverTexNames[TRiverType(i)], tempTex, err_str) then
    begin
      //change order of color components from blue, green, red (as in file) to
      //  red, green, blue (as needed for GL)
      SwapRGB_To_BGR(tempTex);
      GetAlphaByColor(tempTex, AlphaTex);
      glGenTextures(1, @m_RiverTexNames[TRiverType(i)]);
      glBindTexture(GL_TEXTURE_2D, m_RiverTexNames[TRiverType(i)]);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, @AlphaTex[0].r);
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
  msg.AddMessageSimple(
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
  WriteDebugLn('Leaving TGui.Create');
end;//constructor

destructor TGui.Destroy;
var i, j: Integer;
begin
  WriteDebugLn('Entered TGui.Destroy');
  GLUTCallbacksToNil;
  dat.Destroy;
  //free textures
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
    if m_TerrainTexNames[TTerrainType(i)]<> 0 then
      glDeleteTextures(1, @m_TerrainTexNames[TTerrainType(i)]);
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    if m_GoodTexNames[TGoodType(i)]<> 0 then glDeleteTextures(1, @m_GoodTexNames[TGoodType(i)]);
  for i:= Ord(Low(TUnitType)) to Ord(High(TUnitType)) do
    if m_UnitTexNames[TUnitType(i)]<> 0 then glDeleteTextures(1, @m_UnitTexNames[TUnitType(i)]);
  for i:= Ord(Low(TUnitState)) to Ord(High(TUnitState)) do
    if m_StateTexNames[TUnitState(i)]<> 0 then glDeleteTextures(1, @m_StateTexNames[TUnitState(i)]);
  if m_ColonyTexNames[0]<> 0 then glDeleteTextures(1, @m_ColonyTexNames[0]);
  for i:= Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
    for j:= 1 to 3 do
      if m_BuildingTexNames[TBuildingType(i), j]<> 0 then
        glDeleteTextures(1, @m_BuildingTexNames[TBuildingType(i), j]);
  for i:= cMinIndian to cMaxIndian do
    if (m_TribeTexNames[i])<>0 then
      glDeleteTextures(1, @m_TribeTexNames[i]);
  glDeleteTextures(1, @m_ErrorTexName);
  inherited Destroy;
  WriteDebugLn('Leaving TGui.Destroy');
end;//destructor

procedure TGui.GLUTCallbacksToNil;
begin
  //glutDisplayFunc(nil);
  glutReshapeFunc(nil);
  glutKeyboardFunc(nil);
  glutSpecialFunc(nil);
  glutMouseFunc(nil);
  glutMotionFunc(nil);
  glutPassiveMotionFunc(nil);
  glutIdleFunc(nil);
end;//proc

procedure TGui.KeyFunc(Key: Byte; {x, y: LongInt;} Special: Boolean = False);
var tempUnit: TUnit;
    temp_cb: TBasicCallback;
    temp_x, temp_y: Byte;
    temp_Map: TMap;
    direc: TDirection;
    temp_col: TColony;
begin
  WriteDebugLn('Entered TGui.KeyFunc');
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
    else if InReport then
    begin
      report_pages:= report_pages -1;
      if (report_pages<=0) then report:= rtNone;
    end
    else begin
      temp_cb:= TExitCallback.Create;
      msg.AddMessageOptions('Vespucci beenden?', ToShortStrArr('Nein', 'Ja'), temp_cb);
    end;//else
  end;//if KEY_ESCAPE

  if InReport then
  begin
    case KEY of
      KEY_RETURN, KEY_SPACE: begin
                               report_pages:= report_pages -1;
                               if (report_pages<=0) then report:= rtNone;
                             end;//return, space
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
             if focused.GetNation=dat.PlayerNation then
             begin
               temp_Map:= dat.GetMap;
               if (focused.IsShip or temp_Map.tiles[focused.GetPosX, focused.GetPosY].IsWater) then
                 msg.AddMessageSimple(dat.GetLang.GetBuildColony(2))
               else begin
                 if temp_Map.tiles[focused.GetPosX, focused.GetPosY].GetType=ttMountains then
                   msg.AddMessageSimple(dat.GetLang.GetBuildColony(4))
                 else begin
                   if dat.FreeForSettlement(focused.GetPosX, focused.GetPosY) then
                   begin
                     temp_cb:= TBuildColonyCallback.Create(focused.GetPosX, focused.GetPosY,
                                focused, dat);
                     temp_cb.inputText:= '';
                     msg.AddMessageInput(dat.GetLang.GetBuildColony(0), dat.GetLang.GetBuildColony(1),
                         dat.GetLang.GetColonyNames(focused.GetNation,
                         length(dat.GetColonyList(focused.GetNation))), temp_cb);
                     focused:= nil;
                   end
                   else
                     msg.AddMessageSimple(dat.GetLang.GetBuildColony(3));
                 end;//else (i.e. not mountains)
               end;//else (i.e. not ship or water)
             end;//if player's nation
           end;//if focused<>nil
           //end of 'B'
      'F': //fortify
           if focused<>nil then
           begin
             if focused.GetNation=dat.PlayerNation then
             begin
               if focused.GetState=usFortified then focused.SetState(usNormal)
               else focused.SetState(usFortified);
             end;//if player's nation
           end;//if, fortify
      'S': ;//sentry
	       //TODO
      'C': begin
             //Centering the map on a unit does not change the unit's state, so
             //  we don't need to check whether it belongs to the player.
             if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY);
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
                     dat.ProcessNationsAfterPlayer;
                     dat.ProcessNationsBeforePlayer;
                     dat.NewRound(dat.PlayerNation);
                     focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                     CheckFoundingFatherMessage;
                   end;//else
                 end;//case SPACE
    end;//case
  end//if
  else begin
    //we have a focused unit, so move it
    if focused.GetNation=dat.PlayerNation then
    begin
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
                       dat.ProcessNationsAfterPlayer;
                       dat.ProcessNationsBeforePlayer;
                       dat.NewRound(dat.PlayerNation);
                       focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                       CheckFoundingFatherMessage;
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
              else focused.Move(direc, temp_Map, dat);
            end//if temp_col<>nil
            else if (focused.EmbarkedPassengers>0) then
            begin
              //check for landfall
              tempUnit:= focused.GetFirstEmbarkedPassenger;
              if tempUnit<>nil then
                temp_cb:= TLandfallCallback.Create(focused, tempUnit.GetType, temp_x, temp_y, temp_Map)
              else
                temp_cb:= TLandfallCallback.Create(focused, utGalleon, temp_x, temp_y, temp_Map);
              msg.AddMessageOptions(dat.GetLang.GetLandfall(0), ToShortStrArr(dat.GetLang.GetLandfall(1), dat.GetLang.GetLandfall(2)), temp_cb);
            end //if passengers>0
            else focused.Move(direc, temp_Map, dat);
          end//if not water
          else focused.Move(direc, temp_Map, dat);
        end//if IsShip
        else focused.Move(direc, temp_Map, dat);
      end;//if direc<>dirNone
      //check if unit moved out of sight, and center on it, if neccessary
      if focused<>nil then
      begin
        if ((focused.GetPosX<=OffsetX) or (focused.GetPosY<=OffsetY) or
            (focused.GetPosX>=OffsetX+x_Fields-1) or (focused.GetPosY>=OffsetY+y_Fields-1)) then
          CenterOn(focused.GetPosX, focused.GetPosY);
      end;//if
    end;//if player's unit
  end;//else
  WriteDebugLn('Leaving TGui.KeyFunc');
end;//proc

procedure TGui.MouseFunc(const button, state, x,y: LongInt);
var pos_x, pos_y: Integer;
    temp_cat: TMenuCategory;
    sx, sy, sx_d, sy_d: ShortInt;
    temp_cbr: TBasicCallback;
    tempUnit: TUnit;
    tempGood: TGoodType;
    tempUArr: TUnitArr;
    tempAmount, tempWord: Word;
    str_arr: TShortStrArr;
    bx, by: Single;
    bType, bType2: TBuildingType;
begin
  WriteDebugLn('Entered TGui.MouseFunc');

  //general stuff
  if ((button=GLUT_LEFT) and (state=GLUT_UP)) then mouse.down:= False
  else if ((button=GLUT_LEFT) and (state=GLUT_DOWN)) then
  begin
    mouse.down:= True;
    mouse.down_x:= x;
    mouse.down_y:= y;
  end;//if down

  //handling mouse events for message box
  // -- remarks: mouse events do nothing in message boxes with input line
  if (msg.txt<>'') then
  begin
    //events are triggered on mouse up of left mouse button
    if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
    begin
      if (IsMouseInMessageBox(x, y) and
         IsMouseInMessageBox(mouse.down_x, mouse.down_y)) then
      begin
        if (msg.inputCaption='') then
        begin
          if (length(msg.options)=0) then
          begin
            //next message, box is dismissed
            GetNextMessage;
            glutPostRedisplay;
          end//iff
          else begin
            //we got options, so check for clicking one of the options
            pos_x:= GetMessageOptionAtMouse(mouse.x, mouse.y);
            if (pos_x<>-1) then
            begin
              //set selection and then dismiss box
              msg.selected_option:= pos_x;
              GetNextMessage;
              glutPostRedisplay;
            end;//if
          end; //else
        end;//if no input caption
      end;//if in message box
    end;//if left and up
    Exit;
  end;//if message present

  //handling colony view's mouse events
  // ---- general ones, i.e. the ones that work on both pages
  if (InColony) then
  begin
    //check for pressing the red "E" in colony view
    if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
    begin
      //check for pressing the red "E" in colony view
      if ((x>608) and (y>cWindowHeight-50)) then
      begin
        cur_colony:= nil;
        glutPostRedisplay;
        Exit;
      end//if
      //check for colony bar click (i.e. renaming colony)
      else if ((mouse.y<=16) and (mouse.down_y<=16)) then
      begin
        temp_cbr:= TRenameColonyCallback.Create(cur_colony);
        with dat.GetLang do
          msg.AddMessageInput(GetColonyString(csRenameQuestion), GetColonyString(csRenameLabel), cur_colony.GetName, temp_cbr);
        Exit;
      end//if title bar clicked

      //check for switcher button (unit view/ building view)
      else if ((GetSwitcherButtonAtMouse(mouse.x, mouse.y)<>-1) and
         (GetSwitcherButtonAtMouse(mouse.x, mouse.y)=GetSwitcherButtonAtMouse(mouse.down_x, mouse.down_y))) then
      begin
        //button was pressed
        ColonyBuildingPage:= GetSwitcherButtonAtMouse(mouse.x, mouse.y)=1;
        Exit;
      end;//if

      // -- units in fields
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
      //change job of unit
      else if ((sx<>-2) and (sx_d<>-2) and (cur_colony.GetUnitInField(sx, sy)<>nil)) then
      begin
        temp_cbr:= TJobChangeCallback.Create(sx, sy, cur_colony);
        msg.AddMessageOptions('Choose profession for unit '+dat.GetLang.GetUnitName(cur_colony.GetUnitInField(sx, sy).GetType)+':',
                           dat.GetJobList(sx, sy, cur_colony.GetUnitInField(sx, sy).GetType, cur_colony), temp_cbr);
      end;//else if

    end;//if Left Mouse Button up
  end;//if InColony (general)

  // ---- events on unit page, e.g. dragging goods/ units
  if (InColony and not ColonyBuildingPage) then
  begin
    if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
    begin
      GetColonyFieldAtMouse(sx, sy);
      GetColonyFieldAtMouse(sx_d, sy_d, mouse.down_x, mouse.down_y);

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
          else msg.AddMessageSimple(dat.GetLang.GetTransfer(tsOutOfSpace));
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
            temp_cbr:= TAbandonColonyCallback.Create(cur_colony, dat);
            with dat.GetLang do
              msg.AddMessageOptions(GetColonyString(csAbandonQuestion), ToShortStrArr(GetColonyString(csAbandonNo), GetColonyString(csAbandonYes)), temp_cbr);
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
          temp_cbr:= TColonyUnitCallback.Create(tempUArr[pos_x]);
          SetLength(str_arr, 3);
          if (tempUArr[pos_x].GetState in [usFortified, usWaitingforShip]) then
            str_arr[0]:= dat.GetLang.GetColonyUnit(cusCancelOrders)
          else str_arr[0]:= dat.GetLang.GetColonyUnit(cusOnBoard);
          if (tempUArr[pos_x].GetState=usFortified) then
            str_arr[1]:= dat.GetLang.GetColonyUnit(cusOnBoard)
          else str_arr[1]:= dat.GetLang.GetColonyUnit(cusFortify);
          str_arr[2]:= dat.GetLang.GetOthers(osNoChanges);
          msg.AddMessageOptions(dat.GetLang.GetColonyUnit(cusOptions)+' '+dat.GetLang.GetUnitName(tempUArr[pos_x].GetType)+':',
                             str_arr, temp_cbr);
        end;//if
      end;//if

    end;//else if button=LEFT and state=UP
    WriteDebugLn('Exiting TGui.MouseFunc');
    Exit;
  end;//if colony (units' page)

  // ---- events on building page, i.e. moving units from and to buildings
  if (InColony and ColonyBuildingPage) then
  begin
    if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
    begin
      //dragging unit from field to building
      GetColonyFieldAtMouse(sx, sy, mouse.down_x, mouse.down_y);
      bType:= GetBuildingAtMouse(x,y);
      if ((bType in [btArmory..btBlacksmith]) and (cur_colony.GetBuildingLevel(bType)>0) and (sx<>-2)) then
      begin
        tempUnit:= cur_colony.GetUnitInField(sx, sy);
        if (tempUnit<>nil) then
        begin
          //colonist dragged from fields to building
          sx_d:= cur_colony.GetFirstFreeBuildingSlot(bType);
          if (sx_d<>-1) then
          begin
            cur_colony.SetUnitInField(sx, sy, nil);
            cur_colony.SetUnitInBuilding(bType, sx_d, tempUnit);
          end//if
          else msg.AddMessageSimple(dat.GetLang.GetBuildingString(bsMaxThree));
        end;//if unit<>nil
      end;//if
      //dragging unit from building to field
      GetColonyFieldAtMouse(sx, sy, x, y);
      bType:= GetBuildingAtMouse(mouse.down_x,mouse.down_y);
      if ((bType<>btNone) and (sx<>-2)) then
      begin
        GetBuildingPosition(bType, bx, by);
        pos_x:= Trunc((mouse.down_x-bx*FieldWidth)/FieldWidth);
        tempUnit:= cur_colony.GetUnitInBuilding(bType, pos_x);
        if ((tempUnit<>nil) and ((sx<>0) or (sy<>0))) then
        begin
          cur_colony.SetUnitInBuilding(bType, pos_x, nil);
          cur_colony.SetUnitInField(sx, sy, tempUnit);
        end;//if (unit<>nil)
      end;//if (building and field valid)

      //check for dragging unit from one building to another building
      bType:= GetBuildingAtMouse(mouse.down_x,mouse.down_y);
      bType2:= GetBuildingAtMouse(x,y);
      if ((bType in [btArmory..btBlacksmith]) and (bType2 in [btArmory..btBlacksmith])
          and (bType<>bType2) and (cur_colony.GetBuildingLevel(bType2)>0)) then
      begin
        //get slot at first building
        GetBuildingPosition(bType, bx, by);
        sx:= Trunc(mouse.down_x - bx*FieldWidth) div FieldWidth;
        //get free slot #
        sy:= cur_colony.GetFirstFreeBuildingSlot(bType2);
        //get the unit
        tempUnit:= cur_colony.GetUnitInBuilding(bType, sx);
        if ((sx in [0..2]) and (tempUnit<>nil)) then
        begin
          if (sy<>-1) then
          begin
            cur_colony.SetUnitInBuilding(bType, sx, nil);
            cur_colony.RealignUnitsInBuilding(bType);
            cur_colony.SetUnitInBuilding(bType2, sy, tempUnit);
          end//if
          else msg.AddMessageSimple(dat.GetLang.GetBuildingString(bsMaxThree));
        end;//if
      end;//if

      //check for clicking construction bar
      if (IsMouseInConstructionBar(x,y) and IsMouseInConstructionBar(mouse.down_x,mouse.down_y)) then
      begin
        temp_cbr:= TConstructionCallback.Create(cur_colony);
        SetLength(str_arr, 1);
        str_arr[0]:= dat.GetLang.GetOthers(osNothing);
        for pos_x:= Ord(Succ(btNone)) to Ord(High(TBuildingType)) do
        begin
          pos_y:= cur_colony.GetBuildingLevel(TBuildingType(pos_x));
          if (pos_y<GetMaxBuildingLevel(TBuildingType(pos_x))) then
          begin
            SetLength(str_arr, length(str_arr)+1);
            GetBuildingCost(TBuildingType(pos_x), pos_y+1, tempAmount, tempWord);
            str_arr[High(str_arr)]:= StretchTo59(
                 dat.GetLang.GetBuildingName(TBuildingType(pos_x), pos_y+1),
                 IntToStr(tempAmount)+' '+dat.GetLang.GetGoodName(gtHammer)+', '
                 +IntToStr(tempWord)+' '+dat.GetLang.GetGoodName(gtTool)
               );
          end;//if
        end;//for
        msg.AddMessageOptions(dat.GetLang.GetBuildingString(bsSelectNext), str_arr, temp_cbr);
      end;//construction bar
    end;//if (left button up)
    WriteDebugLn('Exiting TGui.MouseFunc');
    Exit;
  end;//if at colony's building page

  If InColony then Exit;

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
      //from ship to port
      sx:= GetCargoBoxAtMouse(mouse.down_x, mouse.down_y);
      tempGood:= GetGoodAtMouse;
      if ((sx<>-1) and (tempGood<>gtCross)) then
      begin
        {$IFDEF DEBUG_CODE}
        WriteDebugLn('Trying to clear cargo...');
        {$ENDIF}
        tempUArr:= dat.GetAllShipsInEurope(europe.GetCount);
        if length(tempUArr)>0 then //at least one ship present?
          if tempUArr[0].GetCargoAmountBySlot(sx)>0 then
          begin
            //we have a cargo transfer to the port
            if europe.IsBoycotted(tempUArr[0].GetCargoGoodBySlot(sx)) then
              msg.AddMessageSimple(dat.GetLang.GetTransfer(tsBoycotted))
            else begin
              //start the transfer, finally
              tempGood:= tempUArr[0].GetCargoGoodBySlot(sx);
              tempAmount:= tempUArr[0].UnloadGood(tempUArr[0].GetCargoGoodBySlot(sx), tempUArr[0].GetCargoAmountBySlot(sx));
              //print message about earnings
              msg.AddMessageSimple(
              StretchTo60(dat.GetLang.GetGoodName(tempGood)+': '+IntToStr(tempAmount)+'x'
                 +IntToStr(europe.GetPrice(tempGood, True))+' '+dat.GetLang.GetOthers(osGold),
                IntToStr(europe.GetPrice(tempGood, True)*tempAmount)+' '+dat.GetLang.GetOthers(osGold))
              +StretchTo60('-'+IntToStr(europe.GetTaxRate)+'% '+dat.GetLang.GetOthers(osTax),
                 IntToStr((europe.GetPrice(tempGood, True)*tempAmount*europe.GetTaxRate) div 100)+' '+dat.GetLang.GetOthers(osGold))
              +StretchTo60(dat.GetLang.GetOthers(osEarnings)+':', IntToStr(europe.GetPrice(tempGood, True)*tempAmount
                            -((europe.GetPrice(tempGood, True)*tempAmount*europe.GetTaxRate) div 100))+' '+dat.GetLang.GetOthers(osGold)));
              //actually sell it
              europe.SellGood(tempUArr[0].GetCargoGoodBySlot(sx), tempAmount);
            end;//else
          end;//if
      end;//if
      //from port to ship
      sx:= GetCargoBoxAtMouse;
      tempGood:= GetGoodAtMouse(mouse.down_x, mouse.down_y);
      if ((sx<>-1) and (tempGood<>gtCross)) then
      begin
        {$IFDEF DEBUG_CODE}
        WriteDebugLn('Trying to load cargo...');
        {$ENDIF}
        tempUArr:= dat.GetAllShipsInEurope(europe.GetCount);
        if length(tempUArr)>0 then //at least one ship present?
        begin
          //cargo transfer from port to ship
          if europe.IsBoycotted(tempGood) then
            msg.AddMessageSimple(dat.GetLang.GetTransfer(tsBoycotted))
          else if europe.GetPrice(tempGood, False)*100>europe.GetGold then
            msg.AddMessageSimple(dat.GetLang.GetTransfer(tsOutOfGold))
          else begin
            //start the transfer
            if tempUArr[0].LoadGood(tempGood, 100) then
            begin
              europe.BuyGood(tempGood, 100);
              //should show message about costs to player
              msg.AddMessageSimple(
              StretchTo60(dat.GetLang.GetGoodName(tempGood)+': 100x', IntToStr(europe.GetPrice(tempGood, false))+' '+dat.GetLang.GetOthers(osGold))
              + StretchTo60(dat.GetLang.GetOthers(osCost)+':', IntToStr(europe.GetPrice(tempGood, false)*100)+' '+dat.GetLang.GetOthers(osGold))
              );
            end
            else msg.AddMessageSimple(dat.GetLang.GetTransfer(tsOutOfSpace));
          end;//else
        end;//if
      end;//if


      //check for moving ship from "to new world box" to "expected soon box"
      pos_x:= GetToNewWorldAtMouse(mouse.down_x, mouse.down_y);
      if (pos_x<>-1) and IsMouseInExpectedSoon then
      begin
        {$IFDEF DEBUG_CODE}
        WriteDebugLn('Trying to send back to europe...');
        {$ENDIF}
        tempUArr:= dat.GetAllShipsGoingToNewWorld(europe.GetCount);
        if pos_x<=High(tempUArr) then tempUArr[pos_x].CallBackToEurope;
        Exit;
      end;//if

      //check for moving ship from "expected soon box" to "to new world box"
      pos_x:= GetExpectedSoonAtMouse(mouse.down_x, mouse.down_y);
      if (pos_x<>-1) and IsMouseInToNewWorld then
      begin
        WriteDebugLn('Trying to send back to new world...');
        tempUArr:= dat.GetAllShipsGoingToEurope(europe.GetCount);
        if pos_x<=High(tempUArr) then tempUArr[pos_x].CallBackToNewWorld;
        Exit;
      end;//if

      //check for moving ship from port to "new world box"
      pos_x:= GetShipAtMouse(mouse.down_x, mouse.down_y);
      if (pos_x<>-1) and IsMouseInToNewWorld then
      begin
        WriteDebugLn('Trying to send a ship to new world...');
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
          temp_cbr:= TEuroPortUnitCallback.Create(tempUArr[pos_x], europe);
          temp_cbr.option:=0;
          temp_cbr.inputText:= '';
          msg.AddMessageOptions(dat.GetLang.GetEuroPort(epsManageHeading), str_arr, temp_cbr);
          Exit;
        end;//if pos_x<>High(array)
      end;//if pos_x<>-1

      //check for button "Buy Ship"
      case GetButtonAtMouse(mouse.x, mouse.y) of
        1: begin
             SetLength(str_arr, Ord(utFrigate)-Ord(utCaravel)+3);
             //first option is always "nothing"
             str_arr[0]:= dat.GetLang.GetOthers(osNothing);
             //second option is artillery
             with dat.GetLang do
               str_arr[1]:= StretchTo59(GetUnitName(utArtillery),GetOthers(osCost)
                           +'  500 '+GetOthers(osGold));
             //set option lines to ship names
             for pos_x:= Ord(utCaravel) to Ord(utFrigate) do
               with dat.GetLang do
                 str_arr[2+pos_x-Ord(utCaravel)]:= StretchTo59(GetUnitName(TUnitType(pos_x)),GetOthers(osCost)
                      +' '+IntToStr(cShipPrices[TUnitType(pos_x)])+' '+GetOthers(osGold));

             temp_cbr:= TEuroPortBuyCallback.Create(dat, europe);
             temp_cbr.option:=0;
             temp_cbr.inputText:= '';
             msg.AddMessageOptions(dat.GetLang.GetEuroPort(epsBuyHeading), str_arr, temp_cbr);
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
             temp_cbr:= TEuroPortTrainCallback.Create(dat, europe);
             temp_cbr.option:= 0;
             temp_cbr.inputText:= '';
             msg.AddMessageOptions(dat.GetLang.GetEuroPort(epsTrainHeading),
                               str_arr, temp_cbr);
           end;//case ButtonAtMouse=2 ("Train units")
      end;//case


    end;//else if (button=LEFT) and (state=UP)
    {$IFDEF DEBUG_CODE}
    WriteDebugLn('Exiting TGui.MouseFunc');
    {$ENDIF}
    Exit;
  end;//europe

  if InReport then
  begin
    if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
    begin
      report_pages:= report_pages-1;
      if report_pages<=0 then report:= rtNone;
    end;//if
    {$IFDEF DEBUG_CODE}
    WriteDebugLn('Exiting TGui.MouseFunc');
    {$ENDIF}
    Exit;
  end;//report

  //handle map view's mouse events here
  if ((button=GLUT_LEFT) and (state=GLUT_UP)) then
  begin
    if InMenu then
    begin
      GetMenuSelectionAtMouse(temp_cat, pos_x);
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
    end;//else
    if (pos_x<>-1) then
    begin
      //check for colony first
      cur_colony:= dat.GetColonyInXY(pos_x, pos_y);
      //if not player's colony, set back to nil
      if cur_colony<>nil then
      begin
        if cur_colony.GetNation<>dat.PlayerNation then
        begin
          cur_colony:= nil;
          {$IFDEF DEBUG_CODE}
          WriteLn('Debug: x: ',x, '; y: ', y, #13#10, 'out of colony now');
          {$ENDIF}
        end;
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
  WriteDebugLn('Leaving TGui.MouseFunc');
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
  WriteDebugLn('Entered TGui.Resize');
  {$ENDIF}
  if ((Width<>cWindowWidth) or (Height<>cWindowHeight)) then
    glutReshapeWindow(cWindowWidth, cWindowHeight);
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.Resize');
  {$ENDIF}
end;//proc

procedure TGui.Start;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.Start');
  WriteDebugLn('glEnable-like stuff');
  {$ENDIF}
  // Enable backface culling
  glEnable(GL_CULL_FACE);
  // Set up depth buffer
  //glEnable(GL_DEPTH_TEST);
  //glDepthFunc(GL_LESS);
  glAlphaFunc(GL_GREATER, 0.2);
  //Starting
  WriteDebugLn('glutMainLoop');
  glutMainLoop;
end;//proc Start

procedure TGui.Draw;
var i, j: Integer;
    tempUnit: TUnit;
    tempColony: TColony;
    tempTribe: TTribe;
    tempMap: TMap;
    tex: GLuint;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.Draw');
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

        //check for river and draw, if present
          //still to do here
        if (tempMap.GetRiverType(i,j)<>cMapRiverNone) then
        begin
          //determine texture name
          case tempMap.GetRiverType(i,j) of
            cMapRiverNorth, cMapRiverEast, cMapRiverSouth, cMapRiverWest:
                     tex:= m_RiverTexNames[rtOne];
            cMapRiverNS, cMapRiverEW: tex:= m_RiverTexNames[rtTwo_Straight];
            cMapRiverNE, cMapRiverSE, cMapRiverSW, cMapRiverNW:
                     tex:= m_RiverTexNames[rtTwo_Bend];
            cMapRiverNotN, cMapRiverNotE, cMapRiverNotS, cMapRiverNotW:
                     tex:= m_RiverTexNames[rtThree];
            cMapRiverAll: tex:= m_RiverTexNames[rtFour];
          else tex:= 0;
          end;//case
          if (tex<>0) then
          begin
            glEnable(GL_TEXTURE_2D);
            glEnable(GL_ALPHA_TEST);
            glBindTexture(GL_TEXTURE_2D, tex);
            glBegin(GL_QUADS);
              glColor3f(1.0, 1.0, 1.0);
              case tempMap.GetRiverType(i,j) of
                cMapRiverNorth, cMapRiverNS, cMapRiverNE, cMapRiverNotW, cMapRiverAll:
                  begin //no rotation needed
                    glTexCoord2f(0.0, 1.0);
                    glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                    glTexCoord2f(0.0, 0.0);
                    glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(1.0, 0.0);
                    glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(1.0, 1.0);
                    glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                  end;//no rotation
                cMapRiverWest, cMapRiverEW, cMapRiverNW, cMapRiverNotS:
                  begin //rotation: 90° (positive direction)
                    glTexCoord2f(1.0, 1.0);
                    glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                    glTexCoord2f(0.0, 1.0);
                    glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(0.0, 0.0);
                    glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(1.0, 0.0);
                    glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                  end; //rot.: 90°
                cMapRiverSouth, cMapRiverSW, cMapRiverNotE:
                  begin //rotation: 180° (positive direction)
                    glTexCoord2f(1.0, 0.0);
                    glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                    glTexCoord2f(1.0, 1.0);
                    glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(0.0, 1.0);
                    glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(0.0, 0.0);
                    glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                  end; //rot.: 180°
                cMapRiverEast, cMapRiverSE, cMapRiverNotN:
                  begin //rotation: 270° (positive direction)
                    glTexCoord2f(0.0, 0.0);
                    glVertex2f(i-OffsetX, -j+y_Fields+OffsetY);//j: f(j)=-j+y_Fields+OffsetY
                    glTexCoord2f(1.0, 0.0);
                    glVertex2f(i-OffsetX, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(1.0, 1.0);
                    glVertex2f(i-OffsetX+1, -j-1+y_Fields+OffsetY);//j+1
                    glTexCoord2f(0.0, 1.0);
                    glVertex2f(i-OffsetX+1, -j+y_Fields+OffsetY);//j
                  end; //rot.: 270°
              end;//case
            glEnd;
            glDisable(GL_ALPHA_TEST);
            glDisable(GL_TEXTURE_2D);
          end;//if river texture present
        end;//if river present

        //check for unit and draw unit icon, if present
        tempUnit:= dat.GetFirstUnitInXY(i,j);
        if (tempUnit<>nil) then
        begin
          glEnable(GL_TEXTURE_2D);
          glEnable(GL_ALPHA_TEST);
          DrawUnitIcon(tempUnit, i-OffsetX, -j-1+y_Fields+OffsetY, true, True);
          glDisable(GL_ALPHA_TEST);
          glDisable(GL_TEXTURE_2D);
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
      //draw unit icon
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_ALPHA_TEST);
      if m_UnitTexNames[focused.GetType]<>0 then
        glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[focused.GetType])
      else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
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
      // -- number of tools (if present)
      if (focused.GetToolAmount>0) then
        WriteText(IntToStr(focused.GetToolAmount)+' '+dat.GetLang.GetGoodName(gtTool),
                  x_Fields +4*PixelWidth, 5.5);
    end;//if Focused unit present

    //draw menu, if present
    DrawMenu;

  end;//if America view

  //show the text messages, if present
  DrawMessage;
  glutSwapBuffers();
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.Draw');
  {$ENDIF}
end;//TGui.Draw

procedure TGui.DrawColonyView;
var i,j: ShortInt;
    local_Map: TMap;
    tempStr: string;
    str_width: Integer;
    u_arr: TUnitArr;
    bt: TBuildingType;
    h, t: Word;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.DrawColonyView');
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
  // draw fields around colony
  local_Map:= dat.GetMap;
  for i:= -1 to 1 do
    for j:= -1 to 1 do
    begin
      //draw terrain
      if m_TerrainTexNames[local_Map.tiles[i+cur_colony.GetPosX,j+cur_colony.GetPosY].m_Type]=0 then
      begin
        {$IFDEF DEBUG_CODE}
        WriteDebugLn('TGui.DrawColonyView: Trying to draw flat terrain in ',i,',',j);
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
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
      end;//if
    end;//for
  // draw colony icon on center field
  if m_ColonyTexNames[0] <> 0 then
  begin
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_ALPHA_TEST);
    glBindTexture(GL_TEXTURE_2D, m_ColonyTexNames[0]);
    glBegin(GL_QUADS);
      glColor3f(1.0, 1.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex2f(x_Fields + 2.0, y_Fields - 3.0); //lower left corner
      glTexCoord2f(1.0, 0.0);
      glVertex2f(x_Fields + 3.0, y_Fields - 3.0);
      glTexCoord2f(1.0, 1.0);
      glVertex2f(x_Fields + 3.0, y_Fields - 2.0);
      glTexCoord2f(0.0, 1.0);
      glVertex2f(x_Fields + 2.0, y_Fields - 2.0);
    glEnd;
    glDisable(GL_ALPHA_TEST);
    glDisable(GL_TEXTURE_2D);
  end;
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
    //bar for current construction in progress
    // --- background
    glBegin(GL_QUADS);
      glColor3f(0.5, 0.5, 1.0);
      glVertex2f(3.0, 1.25);
      glVertex2f(12.0, 1.25);
      glVertex2f(12.0, 1.75);
      glVertex2f(3.0, 1.75);
    glEnd;
    // --- border
    glLineWidth(2.0);
    glBegin(GL_LINE_LOOP);
      glColor3f(0.25, 0.25, 1.0);
      glVertex2f(3.0, 1.25);
      glVertex2f(12.0, 1.25);
      glVertex2f(12.0, 1.75);
      glVertex2f(3.0, 1.75);
    glEnd;
    // --- text
    bt:= cur_colony.GetCurrentConstruction;
    tempStr:= dat.GetLang.GetBuildingString(bsUnderConstruction)+': '+
              dat.GetLang.GetBuildingName(bt, cur_colony.GetBuildingLevel(bt)+1);
    glColor3f(1.0, 1.0, 1.0);
    WriteText(tempStr , 7.5-length(tempStr)*PixelWidth*4, 1.375);
    // ---- show progress (hammers and tools)
    if (bt<>btNone) then
    begin
      glColor3ubv(@cMenuTextColour[0]);
      WriteText(dat.GetLang.GetOthers(osProgress)+ ':',
               cWindowWidth*PixelWidth-5.0, y_Fields-6.5);
      GetBuildingCost(bt, cur_colony.GetBuildingLevel(bt)+1, h, t);
      //hammers
      WriteText(dat.GetLang.GetGoodName(gtHammer)+': ',
                cWindowWidth*PixelWidth-4.75, y_Fields-7.0);
      if (h>0) then
        WriteText(IntToStr(cur_colony.GetStore(gtHammer))+'/'+IntToStr(h)+' ('
                  +IntToStr((cur_colony.GetStore(gtHammer)*100) div h) +'%)',
                  cWindowWidth*PixelWidth-4.5, y_Fields-7.5)
      else
        WriteText(IntToStr(cur_colony.GetStore(gtHammer))+'/'+IntToStr(h)
                  +' (100%)', cWindowWidth*PixelWidth-4.5, y_Fields-7.5);
      //tools
      WriteText(dat.GetLang.GetGoodName(gtTool)+': ',
                cWindowWidth*PixelWidth-4.75, y_Fields-8.0);
      if (t>0) then
        WriteText(IntToStr(cur_colony.GetStore(gtTool))+'/'+IntToStr(t)+' ('
                  +IntToStr((cur_colony.GetStore(gtTool)*100) div t) +'%)',
                  cWindowWidth*PixelWidth-4.5, y_Fields-8.5)
      else
        WriteText(IntToStr(cur_colony.GetStore(gtTool))+'/'+IntToStr(t)
                  +' (100%)', cWindowWidth*PixelWidth-4.5, y_Fields-8.5);
    end;//if bt not equal btNone
    //text, if mouse hovers over buildings
    bt:= GetBuildingAtMouse(mouse.x, mouse.y);
    if (bt<>btNone) then
    begin
      if (cur_colony.GetBuildingLevel(bt)>0) then begin
        tempStr:= dat.GetLang.GetBuildingName(bt, cur_colony.GetBuildingLevel(bt));
        str_width:= 8*length(tempStr);
        //Umrandung zeichnen
        glBegin(GL_QUADS);
          glColor3f(0.0, 0.0, 0.0);
          glVertex2f((mouse.x-2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth-0.5);
          glVertex2f((mouse.x+str_width+2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth-0.5);
          glVertex2f((mouse.x+str_width+2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth);
          glVertex2f((mouse.x-2)*PixelWidth, (cWindowHeight-mouse.y)*PixelWidth);
        glEnd;
        //Text ausgeben
        glColor3f(1.0, 1.0, 1.0);
        WriteText(tempStr, mouse.x*PixelWidth, (cWindowHeight+3-mouse.y)*PixelWidth-0.5);
      end;//level>0
    end;//if bt<>btNone
  end//if ColonyBuildingPage
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

  DrawGoodsBarHoverText;

  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.DrawColonyView');
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
    level, production: Byte;
    ENat: TEuropeanNation;
begin
  //buildings
  for i:=Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
  begin
    level:= cur_colony.GetBuildingLevel(TBuildingType(i));
    //check for valid level
    if (level in [1..3]) then
    begin
      GetBuildingPosition(TBuildingType(i), x,y);
      //check, whether texture is loaded
      if m_BuildingTexNames[TBuildingType(i), level]<>0 then
      begin
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
      end;//building texture present
      //draw units in buildings
      for level:= 0 to 2 do
        if (cur_colony.GetUnitInBuilding(TBuildingType(i), level)<>nil) then
        begin
          glEnable(GL_TEXTURE_2D);
          glEnable(GL_ALPHA_TEST);
          DrawUnitIcon(cur_colony.GetUnitInBuilding(TBuildingType(i), level), x+level, y, True, False);
          glDisable(GL_ALPHA_TEST);
          glDisable(GL_TEXTURE_2D);
        end;//if unit present in building
      //draw production amount
      ENat:= dat.GetNation(cur_colony.GetNation) as TEuropeanNation;
      production:= cur_colony.GetTotalProduction(TBuildingType(i),
                                                 ENat.HasFoundingFather(ffJefferson),
                                                 ENat.HasFoundingFather(ffPenn));
      if (production>0) then
      begin
        //draw good icon
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        if (m_GoodTexNames[GetProducedGood(TBuildingType(i))]<>0) then
          glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[GetProducedGood(TBuildingType(i))])
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
        glEnd;
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
        //write amount of produced good
        glColor3ubv(@cMenuTextColour[0]);
        WriteText(IntToStr(production), x+1.0, y+2.0);
      end;//if production>0
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
  DrawGoodsBarHoverText;
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
          if m_GoodTexNames[ShipArr[0].GetCargoGoodBySlot(i)]<>0 then
            glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[ShipArr[0].GetCargoGoodBySlot(i)])
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
      if m_UnitTexNames[PeopleArr[i].GetType]<>0 then
        glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[PeopleArr[i].GetType])
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
  //only economy, fleet, colony (partially), foreign affairs and score (part.) implemented yet
  case Report of
    rtCongress: DrawCongressReport;
    rtJob: DrawJobReport;
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
                     begin
                       //calculate offset based on length of string
                       freight_offset:= IntegerLength(col_arr[i].GetStore(TGoodType(j)))*4;
                       WriteText(IntToStr(col_arr[i].GetStore(TGoodType(j))),
                                 4+2*PixelWidth+0.5+j-Ord(gtFood)-freight_offset*PixelWidth,
                                 y_Fields-2.5+4*PixelWidth-i*0.5);
                     end;//for
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
    rtColony: DrawColonyReport;
    rtForeign: DrawForeignAffairsReport;
    rtIndian: DrawIndianReport;
    rtScore: DrawScoreReport;
  end;//case
end;//proc

procedure TGui.DrawCongressReport;
var i, max_bells: Integer;
    EuroNat: TEuropeanNation;
    offset: Single;
begin
  //headline
  glColor3ubv(@cMenuTextColour[0]);
  i:= (cWindowWidth-length(dat.GetLang.GetReportString(rlsCongress))*8) div 2;
  WriteText(dat.GetLang.GetReportString(rlsCongress), i*PixelWidth,  y_Fields-0.25);
  //next meeting
  EuroNat:= dat.GetNation(dat.PlayerNation) as TEuropeanNation;
  WriteText(dat.GetLang.GetReportString(rlsNextCongress)+': '
            +dat.GetLang.GetFoundingFatherName(EuroNat.GetNextFoundingFather),
            0.5, y_Fields-1.0);
  //bells
  glEnable(GL_ALPHA_TEST);
  glEnable(GL_TEXTURE_2D);
  if m_GoodTexNames[gtLibertyBell]<>0 then
    glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[gtLibertyBell])
  else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
  glBegin(GL_QUADS);
    glColor3f(1.0, 1.0, 1.0);
    max_bells:= GetRequiredLibertyBells(EuroNat.GetPresentFoundingFathers+1);
    for i:= 0 to EuroNat.GetLibertyBells-1 do
    begin
      offset:= 2.0+ (i/(max_bells-1))*15.0;
      glTexCoord2f(0.0, 0.0);
      glVertex2f(offset, y_Fields-2.5);
      glTexCoord2f(1.0, 0.0);
      glVertex2f(offset+1.0, y_Fields-2.5);
      glTexCoord2f(1.0, 1.0);
      glVertex2f(offset+1.0, y_Fields-1.5);
      glTexCoord2f(0.0, 1.0);
      glVertex2f(offset, y_Fields-1.5);
    end;//for
  glEnd;
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_ALPHA_TEST);
  glColor3ubv(@cMenuTextColour[0]);
  // --- number of bells
  WriteText(IntToStr(EuroNat.GetLibertyBells), 1.0, y_Fields-2.0);
  //attitudes
  { attitide not implemented yet, so display static values }
  WriteText(dat.GetLang.GetReportString(rlsRebelAttitude)+': 0%    '
            +dat.GetLang.GetReportString(rlsLoyalAttitude)+': 100%',
            0.5, y_Fields-3.5);
  //king's forces
  WriteText(dat.GetLang.GetReportString(rlsExpeditionForces)+':',
            0.5, y_Fields-4.5);
  WriteText('not implemented yet', 1.5, y_Fields-5.0);
  //founding fathers
  WriteText(dat.GetLang.GetReportString(rlsFoundingFathers)+':',
            0.5, y_Fields-6.0);
  max_bells:= 0; //works as sort of counter/ offset
  for i:= Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
    if EuroNat.HasFoundingFather(TFoundingFathers(i)) then
    begin
      WriteText(dat.GetLang.GetFoundingFatherName(TFoundingFathers(i)),
                1.0+(max_bells mod 4)*4.5,
                y_Fields-6.5-(max_bells div 4)*0.5);
      max_bells:= max_bells+1;
    end;//if
end;//proc

procedure TGui.DrawColonyReport;
var col_arr: TColonyArr;
    u_arr:   TUnitArr;
    i, j: Integer;
    tempUnit: TUnit;
begin
  if (report_pages=2) then
  begin
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(dat.GetLang.GetMenuOption(mcReports, 4), 3.0, y_Fields-0.25);
    WriteText(dat.GetLang.GetOthers(osName), 0.5, y_Fields-0.75);
    WriteText(dat.GetLang.GetReportString(rlsMilitaryGarrisons), 6.5, y_Fields-0.75);
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
        glColor3ubv(@cMenuTextColour[0]);
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
  end //report_pages=2
  else begin //report_page <> 2
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(dat.GetLang.GetMenuOption(mcReports, 4), 3.0, y_Fields-0.25);
    WriteText(dat.GetLang.GetOthers(osName), 0.5, y_Fields-0.75);
    WriteText(dat.GetLang.GetReportString(rlsSonsOfLiberty), 6.5, y_Fields-0.75);
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
        glColor3ubv(@cMenuTextColour[0]);
        WriteText(IntToStr(col_arr[i].GetInhabitants), 1.25, y_Fields-1.75-i);
        WriteText(col_arr[i].GetName, 2.25, y_Fields-1.75-i);

        //level of press (if present)
        if col_arr[i].GetBuildingLevel(btPress)>0 then
          WriteText(dat.GetLang.GetBuildingName(btPress, col_arr[i].GetBuildingLevel(btPress)),
                    8.0, y_Fields-1.75-i);

        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        //production of liberty bells
        // -- icon
        if m_GoodTexNames[TGoodType(i)]<>0 then
           glBindTexture(GL_TEXTURE_2D, m_GoodTexNames[gtLibertyBell])
        else glBindTexture(GL_TEXTURE_2D, m_ErrorTexName);
        glBegin(GL_QUADS);
          glColor3f(1.0, 1.0, 1.0);
          glTexCoord2f(0.0, 0.0);
          glVertex2f(12.0, y_Fields-2-i);
          glTexCoord2f(1.0, 0.0);
          glVertex2f(13.0, y_Fields-2-i);
          glTexCoord2f(1.0, 1.0);
          glVertex2f(13.0, y_Fields-1-i);
          glTexCoord2f(0.0, 1.0);
          glVertex2f(12.0, y_Fields-1-i);
        glEnd;
        glDisable(GL_ALPHA_TEST);
        glDisable(GL_TEXTURE_2D);
        // -- number of bells produced
        glColor3ubv(@cMenuTextColour[0]);
        WriteText(IntToStr(col_arr[i].GetTotalProduction(btTownHall,
          (dat.GetNation(col_arr[i].GetNation) as TEuropeanNation).HasFoundingFather(ffJefferson),
           false {assume Penn is not present, because he isn't needed for town halls})),
           14.0, y_Fields-1.75-i);
        // -- people in building
        glColor3f(1.0, 1.0, 1.0);
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_ALPHA_TEST);
        for j:= 0 to 2 do
        begin
          tempUnit:= col_arr[i].GetUnitInBuilding(btTownHall, j);
          if tempUnit<>nil then
          DrawUnitIcon(tempUnit, 16.0+j, y_Fields-2-i, True, false);
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
  end;// else (report_page<>2)
end;//proc ColonyReport

procedure TGui.DrawForeignAffairsReport;
var i, j, count: Integer;
    EuroNat: TEuropeanNation;
    f_rec: TForeignRecord;
    offset: GLfloat;
begin
  i:= (cWindowWidth-length(dat.GetLang.GetReportString(rlsForeignAffairs))*8) div 2;
  glColor3ubv(@cMenuTextColour[0]);
  WriteText(dat.GetLang.GetReportString(rlsForeignAffairs), i*PixelWidth, y_Fields);

  for i:= cMinEuropean to cMaxEuropean do
  begin
    glColor3ubv(@cMenuTextColour[0]);
    glLineWidth(2.0);
    glBegin(GL_LINES);
      glVertex2f(0.0, y_Fields-0.5 -(i-cMinEuropean)*3.0);
      glVertex2f(20.0, y_Fields-0.5 -(i-cMinEuropean)*3.0);
    glEnd;
    EuroNat:= dat.GetNation(i) as TEuropeanNation;
    //leader's & nation's name
    WriteText(EuroNat.GetLeaderName()+', '+dat.GetLang.GetNationName(i)+':',
              0.5, y_Fields-1.0 -(i-cMinEuropean)*3.0);
    f_rec:= dat.GetForeignReport(i);
    if ((dat.GetNation(dat.PlayerNation) as TEuropeanNation).HasFoundingFather(ffDeWitt)) then
    begin
      //colony-related data
      WriteText(dat.GetLang.GetReportString(rlsColonies)+': '+IntToStr(f_rec.Colonies),
                0.5, y_Fields-1.5 -(i-cMinEuropean)*3.0);
      WriteText(dat.GetLang.GetReportString(rlsAverageColony)+': '+IntToStr(f_rec.Average),
                7.0, y_Fields-1.5 -(i-cMinEuropean)*3.0);
      WriteText(dat.GetLang.GetReportString(rlsPopulation)+': '+IntToStr(f_rec.Population),
                14.0, y_Fields-1.5 -(i-cMinEuropean)*3.0);
      //military/ naval/ merchants
      WriteText(dat.GetLang.GetReportString(rlsMilitaryPower)+': '+IntToStr(f_rec.Military),
                0.5, y_Fields-2.0 -(i-cMinEuropean)*3.0);
      WriteText(dat.GetLang.GetReportString(rlsNavalPower)+': '+IntToStr(f_rec.Naval),
                7.0, y_Fields-2.0 -(i-cMinEuropean)*3.0);
      WriteText(dat.GetLang.GetReportString(rlsMerchantMarine)+': '+IntToStr(f_rec.Merchant),
                14.0, y_Fields-2.0 -(i-cMinEuropean)*3.0);
    end;//if De Witt is present
    //diplomatic status
    count:= 0;
    for j:= cMinEuropean to cMaxEuropean do
    begin
      if f_rec.Diplomatic[j]<>dsUndefined then
      begin
        glColor3ubv(@cMenuTextColour[0]);
        WriteText(dat.GetLang.GetNationName(j)+': ', 0.5+6.5*count,
                  y_Fields-2.5 -(i-cMinEuropean)*3.0);
        offset:= (length(dat.GetLang.GetNationName(j))+2)*8*PixelWidth;
        case f_rec.Diplomatic[j] of
          dsWar: begin
                   glColor3f(1.0, 0.0, 0.0);
                   WriteText(dat.GetLang.GetReportString(rlsWar),
                     0.5+6.5*count+offset, y_Fields-2.5 -(i-cMinEuropean)*3.0);
                 end;//war
          dsPeace: begin
                     glColor3f(1.0, 1.0, 1.0);
                     WriteText(dat.GetLang.GetReportString(rlsPeace),
                       0.5+6.5*count+offset, y_Fields-2.5 -(i-cMinEuropean)*3.0);
                   end;//peace
        end;//case
        count:= count +1;
      end;//if
    end;//for
    //rebels / tories
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(dat.GetLang.GetReportString(rlsRebels)+': '+IntToStr(f_rec.Rebels),
              0.5, y_Fields-3.0 -(i-cMinEuropean)*3.0);
    WriteText(dat.GetLang.GetReportString(rlsLoyalists)+': '+IntToStr(f_rec.Loyals),
              7.0, y_Fields-3.0 -(i-cMinEuropean)*3.0);
  end;//for
end;//proc Foreign affairs report

procedure TGui.DrawJobReport;
var ut: TUnitType;
    i: Integer;
    w_arr: TWorkArray;
begin
  i:= (cWindowWidth-length(dat.GetLang.GetReportString(rlsJobReport))*8) div 2;
  glColor3ubv(@cMenuTextColour[0]);
  WriteText(dat.GetLang.GetReportString(rlsJobReport), i*PixelWidth, y_Fields);
  w_arr:= dat.GetWorkArray(dat.PlayerNation);
  i:= 0;
  ut:= utCriminal;
  while ut<=utDragoon do
  begin
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_ALPHA_TEST);
    if (m_UnitTexNames[ut]=0) then glBindTexture(GL_TEXTURE_2D, m_ErrorTexName)
    else glBindTexture(GL_TEXTURE_2D, m_UnitTexNames[ut]);
    //the unit icon
    glBegin(GL_QUADS);
      glColor3f(1.0, 1.0, 1.0);
      glTexCoord2f(0.0, 0.0);
      glVertex2f(0.5+(i mod 3)*6.5 , y_Fields-2-(i div 3));
      glTexCoord2f(1.0, 0.0);
      glVertex2f(1.5+(i mod 3)*6.5, y_Fields-2-(i div 3));
      glTexCoord2f(1.0, 1.0);
      glVertex2f(1.5+(i mod 3)*6.5, y_Fields-1-(i div 3));
      glTexCoord2f(0.0, 1.0);
      glVertex2f(0.5+(i mod 3)*6.5, y_Fields-1-(i div 3));
    glEnd;
    glDisable(GL_ALPHA_TEST);
    glDisable(GL_TEXTURE_2D);
    //unit name and number of units
    glColor3ubv(@cMenuTextColour[0]);
    WriteText(dat.GetLang.GetUnitName(ut), 2.0+(i mod 3)*6.5 , y_Fields-1.5-(i div 3));
    WriteText(IntToStr(w_arr[ut]), 3.5+(i mod 3)*6.5 , y_Fields-2.0-(i div 3));
    i:= i+1;
    ut:= Succ(ut);
  end;//while
end;//proc

procedure TGui.DrawIndianReport;
var i, j: Integer;
    indian_arr: TIndianReportArray;
    rep_offset: Single;
begin
  i:= (cWindowWidth-length(dat.GetLang.GetReportString(rlsIndianReport))*8) div 2;
  glColor3ubv(@cMenuTextColour[0]);
  WriteText(dat.GetLang.GetReportString(rlsIndianReport), i*PixelWidth, y_Fields-0.25);
  //list all available indian nations
  indian_arr:= dat.GetIndianReport(dat.PlayerNation);
  rep_offset:= y_Fields-0.75;
  for i:= cMinIndian to cMaxIndian do
  begin
    if (indian_arr[i].settlements>=0) and indian_arr[i].spawned then
    begin
      //set text colour to menu text colour
      glColor3ubv(@cMenuTextColour[0]);
      WriteText(dat.GetLang.GetNationName(i)+': ', 2.0, rep_offset);
      //write nation's level
      j:= cWindowWidth-length(dat.GetLang.GetTechLevelString(indian_arr[i].tech_level, tlsLevelName))*8 - 16;
      WriteText(dat.GetLang.GetTechLevelString(indian_arr[i].tech_level, tlsLevelName), j*PixelWidth, rep_offset);
      //write number of settlements
      if (indian_arr[i].settlements>1) then
        WriteText(IntToStr(indian_arr[i].settlements)+' '+dat.GetLang.GetTechLevelString(indian_arr[i].tech_level, tlsMultipleSettlementName),
                  2.5, rep_offset-0.5)
      else if (indian_arr[i].settlements=1) then
        WriteText(dat.GetLang.GetTechLevelString(indian_arr[i].tech_level, tlsOneSettlementName),
                  2.5, rep_offset-0.5)
      else WriteText(dat.GetLang.GetReportString(rlsIndianExterminated), 2.5, rep_offset-0.5);
      //write attitude
      if (indian_arr[i].settlements>0) then
        WriteText(dat.GetLang.GetAttitudeString(indian_arr[i].attitude), (cWindowWidth div 2)*PixelWidth, rep_offset-0.5);
      rep_offset:= rep_offset - 1.5;
    end;//if zero or more settlements and spawned
  end;//for
  //Did we have some data to show?
  if rep_offset=y_Fields-0.75 then
  begin
    //set text colour to menu text colour
    glColor3ubv(@cMenuTextColour[0]);
    i:= (cWindowWidth-length(dat.GetLang.GetReportString(rlsIndianNoData))*8) div 2;
    //show player that there is no data for him/her yet
    WriteText(dat.GetLang.GetReportString(rlsIndianNoData), i*PixelWidth, rep_offset -2.0);
  end;//if
end;//proc

procedure TGui.DrawScoreReport;
var u_arr: TUnitArr;
    i, j: Integer;
    str1: string;
    score: TScoreRecord;
begin
  u_arr:= dat.GetAllNonCargoUnits(dat.PlayerNation);
  i:= (cWindowWidth-length(dat.GetLang.GetReportString(rlsColonizationScore))*8) div 2;
  glColor3ubv(@cMenuTextColour[0]);
  WriteText(dat.GetLang.GetReportString(rlsColonizationScore), i*PixelWidth, y_Fields-0.25);
  //compose string of leader name, nation, season, year
  str1:= (dat.GetNation(dat.PlayerNation) as TEuropeanNation).GetLeaderName
         +' ('+dat.GetLang.GetNationName(dat.PlayerNation)+'), '
         +dat.GetLang.GetSeason(dat.IsAutumn)+' '+IntToStr(dat.GetYear);
  i:= (cWindowWidth-length(str1)*8) div 2;
  WriteText(str1, i*PixelWidth, y_Fields-0.75);
  score:= dat.GetScore(dat.PlayerNation, u_arr);
  WriteText(dat.GetLang.GetReportString(rlsCitizens)+': +'+IntToStr(score.Citizens),
            0.5, y_Fields-1.5);
  //draw citizens
  j:= High(u_arr);
  if (j>x_Fields*2) then j:= x_Fields*2;
  glColor3f(1.0, 1.0, 1.0);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_ALPHA_TEST);
  for i:=0 to j do
    DrawUnitIcon(u_arr[i], 0.5+i*0.5, y_Fields-3.0, true);
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_TEXTURE_2D);
  //continental congress
  glColor3ubv(@cMenuTextColour[0]);
  WriteText(dat.GetLang.GetReportString(rlsContinentalCongress)
            +': +'+IntToStr(score.Congress), 0.5, y_Fields-4.0);
  //show list of congress members
  j:= 0; //counts how many of them we already have been written
  for i:= Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
  begin
    if ((dat.GetNation(dat.PlayerNation) as TEuropeanNation).HasFoundingFather(TFoundingFathers(i))) then
    begin
      WriteText(dat.GetLang.GetFoundingFatherName(TFoundingFathers(i)),
                1.0+(j mod 3)*7.0,
                y_Fields-4.5-(j div 3)*0.5);
      j:= j+1;
    end;//if
  end;//for

  //gold
  WriteText(dat.GetLang.GetOthers(osGold)+' ('+IntToStr(
            (dat.GetNation(dat.PlayerNation) as TEuropeanNation).GetGold)
            +'°): +'+IntToStr(score.Gold), 0.5, y_Fields-8.0);
  //villages burned
  WriteText(IntToStr((dat.GetNation(dat.PlayerNation) as TEuropeanNation).GetVillagesBurned)
            +' '+dat.GetLang.GetReportString(rlsVillagesBurned)+': '
            +IntToStr(score.Villages), 0.5, y_Fields-8.5);
  //total score
  WriteText(dat.GetLang.GetReportString(rlsTotalScore)+': '+IntToStr(score.Total),
            0.5, y_Fields-9.0);
   //"progress bar" - assume that 5000 is the maximum
   glBegin(GL_QUADS);
     glColor3f(0.0, 0.0, 0.0);
     glVertex2f(2.0-2*PixelWidth, y_Fields-11.0-2*PixelWidth);
     glVertex2f(18.0+2*PixelWidth, y_Fields-11.0-2*PixelWidth);
     glVertex2f(18.0+2*PixelWidth, y_Fields-10.0+2*PixelWidth);
     glVertex2f(2.0-2*PixelWidth, y_Fields-10.0+2*PixelWidth);
     glColor3ubv(@cMenuTextColour[0]);
     glVertex2f(2.0, y_Fields-11.0);

     glVertex2f(2.0+16.0/5000.0*score.Total, y_Fields-11.0);
     glVertex2f(2.0+16.0/5000.0*score.Total, y_Fields-10.0);
     glVertex2f(2.0, y_Fields-10.0);
  glEnd;
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
        if (the_Unit.GetNation in [cMinNations..cMaxIndian]) then
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
  WriteDebugLn('Entered TGui.DrawMessage');
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
  WriteDebugLn('Leaving TGui.DrawMessage');
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
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.WriteText');
  {$ENDIF}
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_8_BY_13, Ord(msg_txt[i]));
    end;
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.WriteText');
  {$ENDIF}
end;//proc

procedure TGui.WriteHelvetica12(const msg_txt: string; const x, y: Single);
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.WriteHelvetica12');
  {$ENDIF}
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, Ord(msg_txt[i]));
    end;
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.WriteHelvetica12');
  {$ENDIF}
end;//proc

procedure TGui.WriteTimesRoman24(const msg_txt: string; const x, y: Single);
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.WriteTimes24');
  {$ENDIF}
  glRasterPos3f(x, y, 0.2);
  for i:= 1 to length(msg_txt) do
    if (Ord(msg_txt[i]) >=32){ and (Ord(msg_txt[i]) <=127)} then
    begin
      glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, Ord(msg_txt[i]));
    end;
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.WriteTimes24');
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
  WriteDebugLn('Entered TGui.DrawMenuBar');
  {$ENDIF}
  glColor3ubv(@cMenuTextColour[0]);
  s:= dat.GetLang.GetMenuLabel(mcGame);
  for i:= Ord(Succ(mcGame)) to Ord(High(TMenuCategory)) do
    s:= s+'  '+dat.GetLang.GetMenuLabel(TMenuCategory(i));
  WriteText(s, 0.1, 12.0+5.0*PixelWidth);
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.DrawMenuBar');
  {$ENDIF}
end;//proc

procedure TGui.DrawGoodsBar;
var i, j, str_width: Integer;
    price_str: string;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.DrawGoodsBar');
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
    for i:= Ord(gtFood) to Ord(gtMusket) do
    begin
      if ((TGoodType(i)=gtFood) or (cur_colony.GetStore(TGoodType(i)) <= (cur_colony.GetBuildingLevel(btWarehouse)+1)*100)) then
        glColor3ub(0,0,0) //black numbers
      else glColor3ub(255, 0, 0); //red numbers for goods that are more than the colony can store
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
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.DrawGoodsBar');
  {$ENDIF}
end;//proc

procedure TGui.DrawGoodsBarHoverText;
var current_good: TGoodType;
    hover_text: String;
    i, str_width: Integer;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.DrawGoodsBarHoverText');
  {$ENDIF}
  current_good := GetGoodAtMouse;
  if current_good<>gtCross then
  begin
    hover_text:= dat.GetLang.GetGoodName(current_good);
    str_width:= 8*length(hover_text);
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
    WriteText(hover_text, i*PixelWidth, (cGoodBarHeight+1)*PixelWidth -0.5)
  end;//func
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.DrawGoodsBarHoverText');
  {$ENDIF}
end;

procedure TGui.DrawColonyTitleBar;
var s: string;
    temp_nat: TNation;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.DrawColonyTitleBar');
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
  WriteDebugLn('Leaving TGui.DrawColonyTitleBar');
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

procedure TGui.GetNextMessage;
var local_bool: Boolean;
begin
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Entered TGui.GetNextMessage');
  {$ENDIF}
  //save last selection before anything else
  if ((length(msg.options)>=1) or (msg.inputCaption<>'')) then
  begin
    //set last selected option
    msg.cbRec.option:= msg.selected_option;
    msg.cbRec.inputText:= msg.inputText;
    //check whether we need callbacks
    if (((msg.cbRec.GetType in [CBT_LOAD_GAME, CBT_SAVE_GAME]) and (msg.selected_option=0))
       or ((msg.cbRec.GetType=CBT_LOAD_GAME) and (dat.GetSaveInfo(msg.selected_option)='('+dat.GetLang.GetOthers(osEmpty)+')'))) then
    begin
      //skip callbacks
      {$IFDEF DEBUG_CODE}
      WriteDebugLn('DBG: TGui.GetNextMessage: skipping callbacks');
      {$ENDIF}
    end
    else begin
      //handle callbacks
      {$IFDEF DEBUG_CODE}
      WriteDebugLn('DBG: TGui.GetNextMessage: handling callback');
      WriteDebugLn('DBG: TGui.GetNextMessage: callback type is ', msg.cbRec.GetType);
      {$ENDIF}
      if (msg.cbRec<>nil) then
        local_bool:= msg.cbRec.Handle
      else local_bool:= true;
      {$IFDEF DEBUG_CODE}
      WriteDebugLn('DBG: TGui.GetNextMessage: local_bool is ', local_bool);
      {$ENDIF}

      case msg.cbRec.GetType of
        CBT_LOAD_GAME: begin
                         if not local_bool then
                         begin
                           msg.AddMessageSimple(dat.GetLang.GetSaveLoad(slsLoadError));
                           focused:= nil;
                           Wooden_Mode:= True;
                         end//if
                         else begin
                           Wooden_Mode:= False;
                           msg.AddMessageSimple(dat.GetLang.GetSaveLoad(slsLoadSuccess));
                           focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                           if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY);
                         end;//else
                       end;// CBT_LOAD_GAME
        CBT_SAVE_GAME: begin
                         if local_bool then msg.AddMessageSimple(dat.GetLang.GetSaveLoad(slsSaveSuccess))
                         else msg.AddMessageSimple(dat.GetLang.GetSaveLoad(slsSaveError));
                       end;// CBT_SAVE_GAME
        CBT_ABANDON_COLONY: if local_bool then cur_colony:= nil;
        CBT_PLAYER_NAME_SELECTION: if local_bool then Wooden_Mode:= False;
      end;//case
    end;//else
  end;//if
  //now the main work
  msg.DequeueMessage;
  {$IFDEF DEBUG_CODE}
  WriteDebugLn('Leaving TGui.GetNextMessage');
  {$ENDIF}
end;//proc

procedure TGui.HandleMenuSelection(const categ: TMenuCategory; const selected: Integer);
var temp_cb: TBasicCallback;
    tempUnit: TUnit;
    str_arr: TShortStrArr;
    col_arr: TColonyArr;
    i: Integer;
    tempTask: TTask;
begin
  case categ of
    mcGame: begin
              case selected of
                1: begin //new game
                     //not completely implemented yet
                     //clear referenced data
                     focused:= nil;
                     europe:= nil;
                     cur_colony:= nil;
                     Wooden_Mode:= true;
                     temp_cb:= TNewGameCallback.Create(dat);
                     SetLength(str_arr, 2);
                     str_arr[0]:= dat.GetLang.GetNewGameString(ngsNewWorld);
                     str_arr[1]:= dat.GetLang.GetNewGameString(ngsAmerica);
                     msg.AddMessageOptions(dat.GetLang.GetNewGameString(ngsNewGame),
                                           str_arr, temp_cb);
                   end;//new game
                2: begin //save
                     if InWoodenMode then
                       msg.AddMessageSimple(dat.GetLang.GetSaveLoad(slsNoGameLoaded))
                     else begin
                       temp_cb:= TSaveCallback.Create(dat);
                       str_arr:= dat.GetSaveSlots;
                       msg.AddMessageOptions(dat.GetLang.GetSaveLoad(slsSaveChoose),
                                          ToShortStrArr(dat.GetLang.GetOthers(osNothing), str_arr),
                                          temp_cb);
                     end;//else
                   end;//save
                3: begin //load
                     temp_cb:= TLoadCallback.Create(dat);
                     str_arr:= dat.GetSaveSlots;
                     msg.AddMessageOptions(dat.GetLang.GetSaveLoad(slsLoadChoose),
                                        ToShortStrArr(dat.GetLang.GetOthers(osNothing), str_arr),
                                        temp_cb);
                   end;//load
                4: begin//quit?
                     temp_cb:= TExitCallback.Create;
                     msg.AddMessageOptions(dat.GetLang.GetOthers(osQuitGame),
                           ToShortStrArr(dat.GetLang.GetOthers(osNo), dat.GetLang.GetOthers(osYes)),
                           temp_cb);
                   end;//3 of mcGame
              end;//case
            end;//mcGame
    mcView: begin
              if (not InWoodenMode) then
              begin
                case selected of
                  1: europe:= TEuropeanNation(dat.GetNation(dat.PlayerNation)); //europe
                  2: if focused<>nil then CenterOn(focused.GetPosX, focused.GetPosY); //center view
                end;//case
              end;//if not in "wooden" mode
            end;//mcView
    mcOrders: begin
                if (not InWoodenMode) then
                begin
                  case selected of
                    1: //fortify
                       if focused<>nil then
                         if focused.GetNation=dat.PlayerNation then
                           if focused.GetState=usFortified then focused.SetState(usNormal)
                           else focused.SetState(usFortified);
                    2: //goto
                       if focused<>nil then
                       begin
                         if focused.GetNation=dat.PlayerNation then
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
                             temp_cb:= TGotoShipCallback.Create(focused, dat);
                             temp_cb.option:= 0;
                             ///TODO: adjust language in message string
                             msg.AddMessageOptions('Choose a destination location:', str_arr, temp_cb);
                           end;//if
                         end;//if player's nation
                       end;//if; mcOrders,2, goto
                    3: //clear forest
                       if focused<>nil then
                       begin
                         if focused.GetNation=dat.PlayerNation then
                         begin
                           if not dat.GetMap.tiles[focused.GetPosX, focused.GetPosY].HasForest then
                             msg.AddMessageSimple(dat.GetLang.GetPioneer(psIsCleared))
                           else if focused.IsShip or (focused.GetType in [utRegular, utDragoon, utScout, utConvoy]) then
                             msg.AddMessageSimple(dat.GetLang.GetPioneer(psWrongUnit))
                           else if focused.GetToolAmount<20 then msg.AddMessageSimple(dat.GetLang.GetPioneer(psNoTools))
                           else if focused.GetTask<>nil then msg.AddMessageSimple(dat.GetLang.GetPioneer(psBusy))
                           else begin
                             //do the real work now :)
                             tempTask:= TClearTask.Create(focused, focused.GetPosX, focused.GetPosY, dat.GetMap);
                             focused.SetTask(tempTask);
                           end;//else
                         end;//if player's nation
                       end;//if //3 of orders (clear forest)
                    4: //plough fields
                       if focused<>nil then
                       begin
                         if focused.GetNation=dat.PlayerNation then
                         begin
                           if dat.GetMap.tiles[focused.GetPosX, focused.GetPosY].IsPloughed then
                             msg.AddMessageSimple(dat.GetLang.GetPioneer(psIsPloughed))
                           else if dat.GetMap.tiles[focused.GetPosX, focused.GetPosY].HasForest then
                             msg.AddMessageSimple(dat.GetLang.GetPioneer(psNeedsClearing))
                           else if focused.IsShip or (focused.GetType in [utRegular, utDragoon, utScout, utConvoy]) then
                             msg.AddMessageSimple(dat.GetLang.GetPioneer(psWrongUnit))
                           else if focused.GetToolAmount<20 then msg.AddMessageSimple(dat.GetLang.GetPioneer(psNoTools))
                           else if focused.GetTask<>nil then msg.AddMessageSimple(dat.GetLang.GetPioneer(psBusy))
                           else begin
                             //do the real work now :)
                             tempTask:= TPloughTask.Create(focused, focused.GetPosX, focused.GetPosY, dat.GetMap);
                             focused.SetTask(tempTask);
                           end;//else
                         end;//if player's nation
                       end;//if //4 of mcOrders (plough fields)
                    5: //construct road
                       if focused<>nil then
                       begin
                         if focused.GetNation=dat.PlayerNation then
                         begin
                           if dat.GetMap.tiles[focused.GetPosX,focused.GetPosY].HasRoad then
                             msg.AddMessageSimple(dat.GetLang.GetPioneer(psHasRoad))
                           else if focused.IsShip or (focused.GetType in [utRegular, utDragoon, utScout, utConvoy]) then
                             msg.AddMessageSimple(dat.GetLang.GetPioneer(psWrongUnit))
                           else if focused.GetToolAmount<20 then msg.AddMessageSimple(dat.GetLang.GetPioneer(psNoTools))
                           else if focused.GetTask<>nil then msg.AddMessageSimple(dat.GetLang.GetPioneer(psBusy))
                           else begin
                             //do the real work now :)
                             tempTask:= TRoadTask.Create(focused, focused.GetPosX, focused.GetPosY, dat.GetMap);
                             focused.SetTask(tempTask);
                           end;//else
                         end;//if player's nation
                       end;//if //5 of mcOrders (create road)
                    6: //no orders
                       if focused<>nil then
                       begin
                         if focused.GetNation=dat.PlayerNation then
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
                             dat.ProcessNationsAfterPlayer;
                             dat.ProcessNationsBeforePlayer;
                             dat.NewRound(dat.PlayerNation);
                             focused:= dat.GetFirstLazyUnit(dat.PlayerNation);
                             CheckFoundingFatherMessage;
                           end;//else
                         end;//if player's nation
                       end; //6 of mcOrders (no orders)
                  end;//case
                end;//if not in "wooden" mode
              end;//mcOrders
    mcReports: begin
                 if (not InWoodenMode) then
                 begin
                   case selected of
                     1: begin
                          report:= rtCongress;
                          report_pages:= 1;
                        end;
                     2: begin
                          report:= rtJob;
                          report_pages:= 1;
                        end;
                     3: begin
                          report:= rtEconomy;
                          report_pages:= 1;
                        end;
                     4: begin
                          report_pages:= 2;
                          report:= rtColony;
                        end;
                     5: begin
                          report:= rtFleet;
                          report_pages:= 1;
                        end;
                     6: begin
                          report:= rtForeign;
                          report_pages:= 1;
                        end;
                     7: begin
                          report:= rtIndian;
                          report_pages:= 1;
                        end;
                     8: begin
                          report:= rtScore;
                          report_pages:= 1;
                        end;
                   end;//case
                 end;//if
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
    if (cat<>mcNone) then sel_option:=0
    else sel_option:= -1;
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
    Result:= (mouse.x>=(3+cShipsInExpectedSoon)*FieldWidth) and (mouse.x<=(4+cShipsInExpectedSoon+cShipsInToNewWorld)*FieldWidth)
             and (mouse.y>=16+FieldWidth) and (mouse.y<=16+3*FieldWidth);
  end//if
  else Result:= (m_x>=(3+cShipsInExpectedSoon)*FieldWidth) and (m_x<=(4+cShipsInExpectedSoon+cShipsInToNewWorld)*FieldWidth)
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

function TGui.GetBuildingAtMouse(const mx, my: LongInt): TBuildingType;
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

  case mx of
    16..16+3*FieldWidth: {first column}
                         case my of
                           32..32+2*FieldWidth: Result:= btCarpenter{first row};
                           3*FieldWidth+16..5*FieldWidth+16: Result:= btFurTrader{second row};
                           6*FieldWidth..8*FieldWidth: Result:= btSchool{third row};
                           8*FieldWidth+16..10*FieldWidth+16: Result:= btWarehouse{fourth row};
                           else Result:= btNone;
                         end;//case
    4*FieldWidth..7*FieldWidth: {second column}
                                case my of
                                  32..32+2*FieldWidth: Result:= btBlacksmith{first row};
                                  3*FieldWidth+16..5*FieldWidth+16: Result:= btDistiller{second row};
                                  6*FieldWidth..8*FieldWidth: Result:= btStable{third row};
                                  8*FieldWidth+16..10*FieldWidth+16: Result:= btTownhall{fourth row};
                                  else Result:= btNone;
                                end;//case
    7*FieldWidth+16..10*FieldWidth+16: {third column}
                                       case my of
                                         32..32+2*FieldWidth: Result:= btChurch{first row};
                                         3*FieldWidth+16..5*FieldWidth+16: Result:= btWeaver{second row};
                                         6*FieldWidth..8*FieldWidth: Result:= btTobacconist{third row};
                                         //free {fourth row}
                                         else Result:= btNone;
                                       end;//case
    11*FieldWidth..14*FieldWidth: {fourth column}
                                  case my of
                                    32..32+2*FieldWidth: Result:= btPress{first row};
                                    3*FieldWidth+16..5*FieldWidth+16: Result:= btArmory{second row};
                                    6*FieldWidth..8*FieldWidth: Result:= btDock{third row};
                                    8*FieldWidth+16..10*FieldWidth+16: Result:= btFort{fourth row};
                                    else Result:= btNone;
                                  end;//case
  else Result:= btNone;
  end;//case mx
end;//func

function TGui.IsMouseInConstructionBar(const mx, my: LongInt): Boolean;
begin
  Result:= (mx>=3*FieldWidth) and (mx<=12*FieldWidth) and (y_Fields*FieldWidth+16-my>= 1.25*FieldWidth)
           and (y_Fields*FieldWidth+16-my<= 1.75*FieldWidth);
end;//func

function TGui.IsMouseInMessageBox(const mx, my: LongInt): Boolean;
var msg_lines, msg_opts: Integer;
    ypsilon: Single;
begin
  Result:= False;
  if (msg.txt<>'') then
  begin
    //get required number of lines
    msg_lines:= (length(msg.txt)+59) div 60;
    // -- ypsilon:= mouse position translated to OpenGL coordinates
    ypsilon:= MouseYToGLCoord(my);

    if length(msg.options)=0 then
    begin
      if msg.inputCaption='' then
      begin
        {we got a simple message, no options, no input :) }
        {//draw box border
        glBegin(GL_LINE_LOOP);
          glColor3f(0.0, 0.0, 0.0);//black
          glVertex2f(2.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 5.5 -0.25*msg_lines);
          glVertex2f(18.0, 6.5 +0.25*msg_lines);
          glVertex2f(2.0, 6.5 +0.25*msg_lines);
        glEnd;}
        Result:= ((mx>2*FieldWidth) and (mx<18*FieldWidth) and
           (ypsilon>= 5.5-0.25*msg_lines) and (ypsilon<= 6.5+0.25*msg_lines));
      end//if
      else begin
        {we got an input message window here}
        {//draw box border
        glLineWidth(2.0);
        glBegin(GL_LINE_LOOP);
          glColor3f(0.0, 0.0, 0.0);//black
          glVertex2f(2.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 5.25 -0.25*msg_lines);
          glVertex2f(18.0, 6.75 +0.25*msg_lines);
          glVertex2f(2.0, 6.75 +0.25*msg_lines);
        glEnd;}
        Result:= ((mx>2*FieldWidth) and (mx<18*FieldWidth) and
           (y_Fields*FieldWidth+16-my>= (5.25-0.25*msg_lines)*FieldWidth)
           and (y_Fields*FieldWidth+16-my<= (6.75+0.25*msg_lines)*FieldWidth));
      end;//else
    end
    else begin
      //we got options
      msg_opts:= length(msg.options);
      {//draw box border
      glLineWidth(2.0);
      glBegin(GL_LINE_LOOP);
        glColor3f(0.0, 0.0, 0.0);//black
        glVertex2f(2.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 5.5 -0.25*(msg_lines+msg_opts));
        glVertex2f(18.0, 6.5 +0.25*(msg_lines+msg_opts));
        glVertex2f(2.0, 6.5 +0.25*(msg_lines+msg_opts));
      glEnd;}

      Result:= ((mx>2*FieldWidth) and (mx<18*FieldWidth) and
           (ypsilon>=(5.5 -0.25*(msg_lines+msg_opts)))
           and (ypsilon<= (6.5 +0.25*(msg_lines+msg_opts))));
    end;//if
  end;//if msg.txt<>''
end;//func

function TGui.GetMessageOptionAtMouse(const mx, my: LongInt): Integer;
var msg_lines, msg_opts: Integer;
    {ypsilon,} temp: Single;
begin
  Result:= -1;
  if ((msg.txt<>'') and (length(msg.options)>0)) then
  begin
    {//draw highlighted background for current option
    glBegin(GL_QUADS);
      glColor3f(0.83*0.8, 0.66*0.8, 0.39*0.8);
      glVertex2f(2.5, 5.4+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
      glVertex2f(17.5, 5.4+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
      glVertex2f(17.5, 5.9+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
      glVertex2f(2.5, 5.9+0.25*(msg_lines+msg_opts)-(msg.selected_option+msg_lines)*0.5);
    glEnd;}
    if ((mx>2.5*FieldWidth) and (mx<17.5*FieldWidth)) then
    begin
      msg_lines:= (length(msg.txt)+59) div 60;
      msg_opts:= length(msg.options);
      //calculate selected option
      // -- ypsilon:= mouse position translated to OpenGL coordinates
      //ypsilon:= MouseYToGLCoord(my);
      temp:= (5.9+0.25*(msg_lines+msg_opts)-msg_lines*0.5)-MouseYToGLCoord(my);
      if (temp<0.0) then Result:= -1
      else Result:= Trunc(temp/0.5);
      if ((Result<0) or (Result>msg_opts)) then Result:= -1;
    end;//if
  end;//if
end;//func

procedure TGui.CheckFoundingFatherMessage;
var temp_cb: TFoundingSelectCallback;
    selection: TFoundingFatherArray;
    EuroNat: TEuropeanNation;
    str_arr: TShortStrArr;
    i: Integer;
begin
  EuroNat:= (dat.GetNation(dat.PlayerNation) as TEuropeanNation);
  if EuroNat=nil then Exit;
  if (EuroNat.GetNextFoundingFather=ffNone) and (EuroNat.GetPresentFoundingFathers<25) then
  begin
    if length(dat.GetColonyList(dat.PlayerNation))>0 then
    begin
      selection:= EuroNat.GetFoundingFatherSelection;
      temp_cb:= TFoundingSelectCallback.Create(EuroNat, selection);
      temp_cb.option:= 0;
      //strings und so
      SetLength(str_arr, 0);
      for i:= 0 to 4 do
      begin
        if selection[i]<>ffNone then
        begin
          SetLength(str_arr, length(str_arr)+1);
          str_arr[High(str_arr)]:= dat.GetLang.GetFoundingFatherName(selection[i])
              +' ('+dat.GetLang.GetFoundingFatherTypeName(GetFoundingFatherType(selection[i]))+')';
        end;//if
      end;//for
      //TODO: localize message string
      msg.AddMessageOptions('Which founding father shall join the continental congress   next?',
                          str_arr, temp_cb);
    end;//if has colonies
  end;//if
end;//proc

end.
